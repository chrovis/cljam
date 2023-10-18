(ns cljam.io.bcf.reader
  (:require [clojure.string :as cstr]
            [clojure.tools.logging :as logging]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.vcf.reader :as vcf-reader]
            [cljam.io.vcf.util :as vcf-util]
            [cljam.util :as util]
            [cljam.io.util.bin :as util-bin]
            [cljam.io.csi :as csi])
  (:import [java.io Closeable IOException FileNotFoundException]
           [java.net URL]
           [java.nio Buffer ByteBuffer]
           [bgzf4j BGZFInputStream])
  (:refer-clojure :exclude [read]))

(declare read-variants meta-info read-variants-randomly read-file-offsets)

(deftype BCFReader
         [^URL url meta-info header ^BGZFInputStream reader
          ^long start-pos index-delay]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (protocols/read this {}))
  (read [this option] (read-variants this option))
  (indexed? [_]
    (try
      @index-delay
      true
      (catch FileNotFoundException _
        false)))
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this {:keys [chr ^long start ^long end]} option]
    (logging/warn "May cause degradation of performance.")
    (filter
     (fn [{^long v-pos :pos
           v-chr :chr
           v-ref :ref}]
       (and (if chr (= v-chr chr) true)
            (if start (<= start v-pos) true)
            (if end (<= (+ v-pos (count v-ref)) end) true)))
     (read-variants this option))))

;; need dynamic extension for namespace issue.
(extend-type BCFReader
  protocols/IVariantReader
  (meta-info [this] (meta-info this))
  (header [this] (.header this))
  (read-variants
    ([this] (protocols/read-variants this {}))
    ([this option] (read-variants this option)))
  (read-variants-randomly [this region-option deep-option]
    (read-variants-randomly this region-option deep-option))
  (read-file-offsets [this]
    (read-file-offsets this)))

(defn- parse-meta-and-header
  "Parses meta-info and header of BCF files and returns them as a map.
  lines must be a sequence of strings."
  [lines]
  (let [{metas "##"
         headers "#"} (group-by #(re-find #"^\#+" %) lines)]
    {:meta (transduce
            (map vcf-reader/parse-meta-info-line)
            (completing
             (fn [meta-info' [k v]]
               (if (#{:contig :info :filter :format :alt :sample :pedigree} k)
                 (update meta-info' k (fnil conj []) v)
                 (assoc meta-info' k v))))
            {}
            metas)
     :header (first (map vcf-reader/parse-header-line headers))}))

(defn reader
  "Returns an open cljam.bcf.reader.BCFReader of f. Should be used inside
  with-open to ensure the Reader is properly closed.
  Throws IOException if failed to parse BCF file format."
  ^BCFReader
  [f]
  (let [rdr (bgzf/bgzf-input-stream f)
        magic (lsb/read-bytes rdr 5)]
    (if (= (seq magic) (map byte "BCF\2\2"))
      (let [hlen (int (lsb/read-int rdr))
            header-buf (lsb/read-bytes rdr hlen)]
        (if (zero? (aget ^bytes header-buf (dec hlen))) ;; NULL-terminated
          (let [{:keys [header] meta-info :meta} (->> (String. ^bytes header-buf
                                                               0
                                                               (int (dec hlen)))
                                                      cstr/split-lines
                                                      parse-meta-and-header)]
            (BCFReader. (util/as-url f) meta-info header rdr (.getFilePointer rdr)
                        (delay (csi/read-index (str f ".csi")))))
          (do
            (.close rdr)
            (throw
             (IOException.
              "Invalid file format. BCF header must be NULL-terminated.")))))
      (do
        (.close rdr)
        (throw
         (IOException.
          (str "Invalid file format. BCF v2.2 magic not found in " f)))))))

(defn- read-typed-atomic-value
  "Reads an atomic value, which is typed as either
  integer(8,16,32 bit) or float or character."
  [r ^long type-id]
  (case type-id
    1 (let [i (byte (lsb/read-byte r))]
        (case (bit-and 0xFF i) 0x80 nil 0x81 :eov i))
    2 (let [i (short (lsb/read-short r))]
        (case (bit-and 0xFFFF i) 0x8000 nil 0x8001 :eov i))
    3 (let [i (int (lsb/read-int r))]
        (case (bit-and 0xFFFFFFFF i) 0x80000000 nil 0x80000001 :eov i))
    5 (let [i (int (lsb/read-int r))]
        (case (bit-and 0xFFFFFFFF i) 0x7F800001 nil 0x7F800002
              :eov (Float/intBitsToFloat i)))
    7 (lsb/read-byte r)))

(defn- bytes->strs
  [ba]
  (map (fn [^String s]
         (let [z (.indexOf s (int 0))
               s' (cond-> s (not (neg? z)) (subs 0 z))]
           (when-not (= s' ".") (not-empty s'))))
       (cstr/split (String. (byte-array ba)) #",")))

(defn- read-typed-value
  "Reads typed value from BCF file. n-sample is a number of values repeated
  after type specifier byte."
  ([rdr]
   (first (read-typed-value rdr 1)))
  ([rdr ^long n-sample]
   (let [type-byte (int (lsb/read-byte rdr))
         len (unsigned-bit-shift-right (bit-and 0xF0 type-byte) 4)
         total-len (if (= len 15) (long (first (read-typed-value rdr))) len)
         type-id (bit-and 0x0F type-byte)]
     (case type-id
       0 (repeat n-sample nil)
       7 (doall (repeatedly n-sample
                            #(bytes->strs (lsb/read-bytes rdr total-len))))
       (->> #(read-typed-atomic-value rdr type-id)
            (repeatedly (* n-sample total-len))
            (partition total-len)
            (map (fn [xs] (take-while #(not= % :eov) xs)))
            doall)))))

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
        pos (inc (int (lsb/read-int shared)))
        rlen (lsb/read-int shared)]
    (.position ^Buffer shared 0)
    (assoc m :chr (:id (contigs chrom-id)) :pos pos :rlen rlen)))

(defn- parse-data-line-deep
  "Parses full data of a variant. Returns a map containing indices for meta-info."
  [{:keys [^ByteBuffer shared ^ByteBuffer individual]}]
  (let [chrom-id (lsb/read-int shared)
        pos (inc (int (lsb/read-int shared)))
        rlen (lsb/read-int shared)
        qual (lsb/read-int shared)
        n-allele-info (int (lsb/read-int shared))
        n-allele (unsigned-bit-shift-right n-allele-info 16)
        n-info (bit-and n-allele-info 0xFFFF)
        n-fmt-sample (long (lsb/read-uint shared))
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
     :qual (when-not (= qual 0x7F800001) (Float/intBitsToFloat qual))
     :id id
     :ref (first refseq)
     :alt (seq (map first altseq))
     :filter flter
     :info info
     :genotype genotype
     :n-sample n-sample}))

(defn- fixup-val [{kw :kw t :type n :number} v]
  (cond->> v
    (= kw :GT) vcf-util/ints->genotype
    (= t "Flag") ((constantly :exists))
    (= t "Character") (map first)
    (and (= n 1) (not= kw :GT)) first))

(defn- bcf-map->parsed-variant
  "Converts a BCF-style variant map to parsed variant using meta-info."
  [contigs filters formats info
   [fmt-kw & indiv-kws] {:keys [genotype n-sample] :as variant}]
  (let [->gt-kv (fn [i [k vs]]
                  (let [{:keys [kw] :as f} (formats k)
                        v (nth vs i nil)]
                    [kw (when-not (or (nil? v) (= [nil] v))
                          (fixup-val f v))]))
        ->info-kv (fn [[k v]]
                    (let [{:keys [kw] :as i} (info k)]
                      [kw (fixup-val i v)]))
        indiv (map (fn [i] (not-empty (into {} (map #(->gt-kv i %)) genotype)))
                   (range n-sample))
        v (-> (dissoc variant :genotype :ref-length :n-sample)
              (update :chr (comp :id contigs))
              (update :filter #(not-empty (map (comp :kw filters) %)))
              (update :info #(not-empty (into {} (map ->info-kv) %))))]
    (cond-> v
      fmt-kw (assoc fmt-kw (not-empty (map (comp :kw formats first) genotype)))
      indiv-kws (merge (zipmap indiv-kws indiv)))))

(defn- read-data-lines
  "Reads data from BCF file and returns them as a lazy-sequence of maps."
  [^BGZFInputStream rdr read-fn]
  (when (pos? (.available rdr))
    (let [data (read-fn rdr)]
      (cons data (lazy-seq (read-data-lines rdr read-fn))))))

(defn- meta->map
  "Creates a map for searching meta-info with indices."
  [meta-info]
  (into {} (map (fn [m] [(Integer/parseInt (:idx m)) (assoc m :kw (keyword (:id m)))])) meta-info))

(defn meta-info
  "Returns meta-information of the BCF from rdr as a map."
  [^BCFReader rdr]
  (-> (.meta-info rdr)
      (update :contig (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :filter (fn [xs] (keep (fn [m] (when-not (= (:id m) "PASS") (dissoc m :idx))) xs)))
      (update :info (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :format (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))))

(defn- make-parse-fn [^BCFReader rdr info depth]
  (let [contigs (meta->map (:contig (.meta-info rdr)))
        filters (assoc (meta->map (:filter (.meta-info rdr)))
                       0
                       {:id "PASS" :kw :PASS})
        formats (meta->map (:format (.meta-info rdr)))
        kws (mapv keyword (drop 8 (.header rdr)))]
    (case depth
      :deep (comp (partial bcf-map->parsed-variant
                           contigs filters formats info kws)
                  parse-data-line-deep)
      :vcf (comp (vcf-util/variant-vals-stringifier
                  (.meta-info rdr)
                  (.header rdr))
                 (partial bcf-map->parsed-variant
                          contigs filters formats info kws)
                 parse-data-line-deep)
      :bcf parse-data-line-deep
      :shallow (partial parse-data-line-shallow contigs)
      :raw identity)))

(defn read-variants
  "Returns data lines of the BCF from rdr as a lazy sequence of maps.
   rdr must implement cljam.bcf.BCFReader.
   Can take an option :depth to specify parsing level. Default is :deep.

     :deep    Fully parsed variant map. FORMAT, FILTER, INFO and samples columns are parsed.
     :vcf     VCF-style map. FORMAT, FILTER, INFO and samples columns are strings.
     :bcf     BCF-style map. CHROM, FILTER, INFO and :genotype contains indices to meta-info.
     :shallow Only CHROM, POS and ref-length are parsed.
     :raw     Raw map of ByteBufers."
  ([rdr]
   (read-variants rdr {}))
  ([^BCFReader rdr {:keys [depth] :or {depth :deep}}]
   (.seek ^BGZFInputStream (.reader rdr) ^long (.start-pos rdr))
   (let [info (meta->map (:info (.meta-info rdr)))
         parse-fn  (make-parse-fn rdr info depth)]
     (read-data-lines (.reader rdr)
                      (fn [rdr] (parse-fn (read-data-line-buffer rdr)))))))

(defn- make-lazy-variants [f s]
  (when-first [fs s]
    (lazy-cat
     (f fs)
     (make-lazy-variants f (rest s)))))

(defn read-variants-randomly
  "Reads variants of the BCF file randomly using csi file.
   Returns them as a lazy sequence."
  [^BCFReader rdr
   {:keys [chr ^long start ^long end] :or {start 1 end 4294967296}}
   {:keys [depth] :or {depth :deep}}]
  (let [info (meta->map (:info (.meta-info rdr)))
        parse-fn  (make-parse-fn rdr info depth)
        input-stream ^BGZFInputStream (.reader rdr)
        chr-names (->> (.meta-info rdr) :contig (mapv :id))
        ref-idx (.indexOf ^clojure.lang.PersistentVector chr-names chr)
        csi-data @(.index-delay rdr)
        spans (when-not (neg? ref-idx)
                (util-bin/get-spans csi-data ref-idx start end))]
    (make-lazy-variants
     (fn [[chunk-beg ^long chunk-end]]
       (.seek input-stream chunk-beg)
       (->> #(when (< (.getFilePointer input-stream) chunk-end)
               (-> input-stream
                   read-data-line-buffer
                   parse-fn))
            repeatedly
            (take-while identity)
            (filter
             (fn [{chr' :chr :keys [^long pos info] ref' :ref}]
               (and (= chr' chr)
                    (<= pos end)
                    (<= start
                        (long (get info :END (dec (+ pos (count ref')))))))))))
     spans)))

(defn read-file-offsets
  "Reads file offsets and a genomic position of variants from BCF and returns
  them as a lazy sequence. Each element is a map containing :chr, :chr-index,
  :beg, :end, :file-beg, :file-end."
  [^BCFReader rdr]
  (let [^BGZFInputStream input-stream (.reader rdr)
        contigs (->> (:contig (.meta-info rdr))
                     (map-indexed (fn [index contig] [(:id contig) index]))
                     (into {}))
        parse-fn (make-parse-fn rdr (meta->map (:info (.meta-info rdr))) :shallow)]
    (letfn [(step [beg-pointer]
              (when (pos? (.available input-stream))
                (when-let [line (read-data-line-buffer input-stream)]
                  (let [end-pointer (.getFilePointer input-stream)
                        {:keys [chr ^long pos ^long rlen]} (parse-fn line)]
                    (cons {:file-beg beg-pointer, :file-end end-pointer
                           :chr-index (contigs chr), :beg pos, :chr chr,
                           :end (dec (+ pos rlen))}
                          (lazy-seq (step end-pointer)))))))]
      (step (.getFilePointer input-stream)))))
