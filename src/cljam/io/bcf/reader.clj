(ns cljam.io.bcf.reader
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [clojure.tools.logging :as logging]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.vcf.reader :as vcf-reader]
            [cljam.io.vcf.util :as vcf-util]
            [cljam.util :as util])
  (:import [java.io Closeable IOException]
           [java.net URL]
           [java.nio ByteBuffer]
           [bgzf4j BGZFInputStream]))

(declare read-variants meta-info)

(deftype BCFReader [^URL url meta-info header ^BGZFInputStream reader ^long start-pos]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (protocols/read this {}))
  (read [this option] (read-variants this option))
  (indexed? [_] false)
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this {:keys [chr start end]} option]
    (logging/warn "May cause degradation of performance.")
    (filter
     (fn [v] (and (if chr (= (:chr v) chr) true)
                  (if start (<= start (:pos v)) true)
                  (if end (<= (+ (:pos v) (count (:ref v))) end) true)))
     (read-variants this option))))

;; need dynamic extension for namespace issue.
(extend-type BCFReader
  protocols/IVariantReader
  (meta-info [this] (meta-info this))
  (header [this] (.header this))
  (read-variants
    ([this] (protocols/read-variants this {}))
    ([this option] (read-variants this option))))

(defn- parse-meta-and-header
  "Parses meta-info and header of BCF files and returns them as a map.
  lines must be a sequence of strings."
  [lines]
  (let [{metas "##"
         headers "#"} (group-by #(re-find #"^\#+" %) lines)]
    {:meta (transduce
            (map vcf-reader/parse-meta-info-line)
            (completing
             (fn [meta-info [k v]]
               (if (#{:contig :info :filter :format :alt :sample :pedigree} k)
                 (update meta-info k (fnil conj []) v)
                 (assoc meta-info k v))))
            {}
            metas)
     :header (first (map vcf-reader/parse-header-line headers))}))

(defn ^BCFReader reader
  "Returns an open cljam.bcf.reader.BCFReader of f. Should be used inside with-open to
  ensure the Reader is properly closed.
   Throws IOException if failed to parse BCF file format."
  [f]
  (let [rdr (BGZFInputStream. (cio/file f))
        magic (lsb/read-bytes rdr 5)]
    (if (= (seq magic) (map byte "BCF\2\2"))
      (let [hlen (lsb/read-int rdr)
            header-buf (lsb/read-bytes rdr hlen)]
        (if (= (aget ^bytes header-buf (dec hlen)) 0) ;; NULL-terminated
          (let [{:keys [header meta]} (->> (String. ^bytes header-buf 0 (int (dec hlen)))
                                           cstr/split-lines
                                           parse-meta-and-header)]
            (BCFReader. (util/as-url f) meta header rdr (.getFilePointer rdr)))
          (do
            (.close rdr)
            (throw (IOException. (str "Invalid file format. BCF header must be NULL-terminated."))))))
      (do
        (.close rdr)
        (throw (IOException. (str "Invalid file format. BCF v2.2 magic not found in " f)))))))

(defn- read-typed-atomic-value
  "Reads an atomic value, which is typed as either
  integer(8,16,32 bit) or float or character."
  [r ^long type-id]
  (case type-id
    1 (let [i (lsb/read-byte r)]
        (case (bit-and 0xFF i) 0x80 nil 0x81 :eov i))
    2 (let [i (lsb/read-short r)]
        (case (bit-and 0xFFFF i) 0x8000 nil 0x8001 :eov i))
    3 (let [i (lsb/read-int r)]
        (case (bit-and 0xFFFFFFFF i) 0x80000000 nil 0x80000001 :eov i))
    5 (let [i (lsb/read-float r)]
        (case (Float/floatToRawIntBits i) 0x7F800001 nil 0x7F800002 :eov i))
    7 (lsb/read-byte r)))

(defn- read-typed-value
  "Reads typed value from BCF file. n-sample is a number of values repeated
  after type specifier byte."
  ([rdr]
   (first (read-typed-value rdr 1)))
  ([rdr n-sample]
   (let [type-byte (lsb/read-byte rdr)
         len (unsigned-bit-shift-right (bit-and 0xF0 type-byte) 4)
         total-len (if (= len 15) (read-typed-value rdr) len)
         type-id (bit-and 0x0F type-byte)]
     (if (= type-id 0)
       (repeat n-sample :exists)
       (let [results (->> #(read-typed-atomic-value rdr type-id)
                          (repeatedly (* n-sample total-len))
                          (partition total-len)
                          doall)]
         (if (= type-id 7)
           (map #(String. (byte-array %)) results)
           (map (fn [xs] (take-while #(not= % :eov) xs)) results)))))))

(defn- read-typed-kv
  "Reads a key-value pair."
  ([rdr]
   (let [[k [v]] (read-typed-kv rdr 1)]
     [k v]))
  ([rdr n-sample]
   [(first (read-typed-value rdr)) (read-typed-value rdr n-sample)]))

(defn- read-data-line-buffer
  "Reads a single record of variant and store to the ByteBuffer objects."
  [rdr]
  (let [l-shared (lsb/read-uint rdr)
        l-indv (lsb/read-uint rdr)
        shared-bb (ByteBuffer/allocate l-shared)
        indv-bb (ByteBuffer/allocate l-indv)]
    (lsb/read-bytes rdr (.array shared-bb) 0 l-shared)
    (lsb/read-bytes rdr (.array indv-bb) 0 l-indv)
    {:l-shared l-shared
     :l-indv l-indv
     :shared shared-bb
     :individual indv-bb}))

(defn- parse-data-line-shallow
  "Parses only chromosome, position and ref-length. Can be used for position-based querying."
  [contigs {:keys [^ByteBuffer shared] :as m}]
  (let [chrom-id (lsb/read-int shared)
        pos (inc (lsb/read-int shared))
        rlen (lsb/read-int shared)]
    (.position shared 0)
    (assoc m :chr (:id (contigs chrom-id)) :pos pos :rlen rlen)))

(defn- parse-data-line-deep
  "Parses full data of a variant. Returns a map containing indices for meta-info."
  [{:keys [^ByteBuffer shared ^ByteBuffer individual]}]
  (let [chrom-id (lsb/read-int shared)
        pos (inc (lsb/read-int shared))
        rlen (lsb/read-int shared)
        qual (lsb/read-float shared)
        n-allele-info (lsb/read-int shared)
        n-allele (unsigned-bit-shift-right n-allele-info 16)
        n-info (bit-and n-allele-info 0xFFFF)
        n-fmt-sample (lsb/read-uint shared)
        n-fmt (bit-and 0xFF (unsigned-bit-shift-right n-fmt-sample 24))
        n-sample (bit-and n-fmt-sample 0xFFFFFF)
        id (let [i (read-typed-value shared)] (if (sequential? i) (first i) i))
        refseq (read-typed-value shared)
        altseq (doall (repeatedly (dec n-allele) #(read-typed-value shared)))
        flter (read-typed-value shared)
        info (doall (repeatedly n-info #(read-typed-kv shared)))
        genotype (doall (repeatedly n-fmt #(read-typed-kv individual n-sample)))]
    {:chr chrom-id
     :pos pos
     :ref-length rlen
     :qual qual
     :id id
     :ref refseq
     :alt (if (empty? altseq) nil altseq)
     :filter flter
     :info info
     :genotype genotype
     :n-sample n-sample}))

(defn- bcf-map->parsed-variant
  "Converts a BCF-style variant map to parsed variant using meta-info."
  [contigs filters formats info [fmt-kw & indiv-kws] variant]
  (let [gts (:genotype variant)
        indiv (map
               (fn [i] (into
                        {}
                        (keep
                         (fn [[k vs]]
                           (let [tag (formats k)
                                 v (nth vs i)]
                             (when-not (or (nil? v) (= [nil] v))
                               [(:kw tag)
                                (cond
                                  (= (:kw tag) :GT) (vcf-util/ints->genotype v)
                                  (and (= (:number tag) 1) (sequential? v)) (first v)
                                  :else v)])))) gts))
               (range (:n-sample variant)))]
    (-> (dissoc variant :genotype)
        (dissoc :ref-length)
        (dissoc :n-sample)
        (update :chr (comp :id contigs))
        (update :filter #(map (comp :kw filters) %))
        (update :info #(into {} (map (fn [[k v]] [(:kw (info k))
                                                  (if (and (= (:number (info k)) 1) (sequential? v))
                                                    (first v)
                                                    v)])) %))
        (assoc fmt-kw (map (comp :kw formats first) gts))
        (merge (zipmap indiv-kws indiv)))))

(defn- read-data-lines
  "Reads data from BCF file and returns them as a lazy-sequence of maps."
  [^BGZFInputStream rdr read-fn]
  (when (pos? (.available rdr))
    (let [data (read-fn rdr)]
      (cons data
            (lazy-seq (read-data-lines rdr read-fn))))))

(defn- meta->map
  "Creates a map for searching meta-info with indices."
  [meta]
  (into {} (map (fn [m] [(Integer/parseInt (:idx m)) (assoc m :kw (keyword (:id m)))])) meta))

(defn meta-info
  "Returns meta-information of the BCF from rdr as a map."
  [^BCFReader rdr]
  (-> (.meta-info rdr)
      (update :contig (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :filter (fn [xs] (keep (fn [m] (when-not (= (:id m) "PASS") (dissoc m :idx))) xs)))
      (update :info (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :format (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))))

(defn read-variants
  "Returns data lines of the BCF from rdr as a lazy sequence of maps.
   rdr must implement cljam.bcf.BCFReader.
   Can take a option :depth to specify parsing level. Default is :deep.

     :deep    Fully parsed variant map. FORMAT, FILTER, INFO and samples columns are parsed.
     :vcf     VCF-style map. FORMAT, FILTER, INFO and samples columns are strings.
     :bcf     BCF-style map. CHROM, FILTER, INFO and :genotype contains indices to meta-info.
     :shallow Only CHROM, POS and ref-length are parsed.
     :raw     Raw map of ByteBufers."
  ([rdr]
   (read-variants rdr {}))
  ([^BCFReader rdr {:keys [depth] :or {depth :deep}}]
   (.seek ^BGZFInputStream (.reader rdr) ^long (.start-pos rdr))
   (let [contigs (meta->map (:contig (.meta-info rdr)))
         filters (assoc (meta->map (:filter (.meta-info rdr))) 0 {:id "PASS" :kw :PASS})
         formats (meta->map (:format (.meta-info rdr)))
         info (meta->map (:info (.meta-info rdr)))
         kws (mapv keyword (drop 8 (.header rdr)))
         parse-fn (case depth
                    :deep (comp (partial bcf-map->parsed-variant contigs filters formats info kws)
                                parse-data-line-deep)
                    :vcf (comp (vcf-util/variant-vals-stringifier (.meta-info rdr) (.header rdr))
                               (partial bcf-map->parsed-variant contigs filters formats info kws)
                               parse-data-line-deep)
                    :bcf parse-data-line-deep
                    :shallow (partial parse-data-line-shallow contigs)
                    :raw identity)]
     (read-data-lines (.reader rdr)
                      (fn [rdr] (parse-fn (read-data-line-buffer rdr)))))))
