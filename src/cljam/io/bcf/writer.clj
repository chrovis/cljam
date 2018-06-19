(ns cljam.io.bcf.writer
  (:require [clojure.string :as cstr]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.vcf.writer :as vw]
            [cljam.io.vcf.util :as vcf-util]
            [cljam.util :as util])
  (:import [java.io Closeable IOException DataOutputStream]
           [java.net URL]
           [java.nio ByteBuffer ByteOrder]
           [bgzf4j BGZFOutputStream]))

(declare write-variants)

(deftype BCFWriter [^URL url meta-info header ^DataOutputStream writer]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-url [this] (.url this))
  protocols/IVariantWriter
  (write-variants [this variants]
    (write-variants this variants)))

(def ^:private ^:const bcf-meta-keys
  [:fileformat :file-date :source :reference :contig :phasing :info :filter :format :alt :sample :pedigree])
(def ^:private ^:const meta-info-prefix "##")
(def ^:private ^:const header-prefix "#")

(defn- stringify-meta
  "Converts meta-info rows to a sequence of strings."
  [meta-info]
  (->>
   (for [k bcf-meta-keys :let [v (meta-info k)] :when v]
     (if (sequential? v)
       (for [x v]
         (str meta-info-prefix (vw/stringify-key k) "=<" (vw/stringify-structured-line k x) ">"))
       [(str meta-info-prefix (vw/stringify-key k) "=" v)]))
   (apply concat)))

(defn- write-file-header
  "Writes BCF file header, meta-info and header row to writer."
  [^BCFWriter w]
  (let [wtr ^DataOutputStream (.writer w)
        hdr-ba (-> \newline
                   (cstr/join
                    (concat
                     (stringify-meta (.meta-info w))
                     [(vw/stringify-header (.header w))]))
                   (str (char 0)) ;; NULL-terminated
                   .getBytes)
        hlen (alength hdr-ba)]
    (lsb/write-bytes wtr (byte-array (map byte "BCF\2\2")))
    (lsb/write-int wtr hlen)
    (lsb/write-bytes wtr hdr-ba)))

(defn ^BCFWriter writer
  "Returns an open cljam.bcf.BCFWriter of f.
   Meta-information lines and a header line will be written in this function.
   Should be used inside with-open to ensure the Writer is properly closed. e.g.

     (with-open [wtr (writer \"out.bcf\"
                             {:file-date \"20090805\", :source \"myImpu...\" ...}
                             [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
       (WRITING-BCF))"
  [^String f meta-info header]
  (let [bos (BGZFOutputStream. f)
        dos (DataOutputStream. bos)
        indexed-meta (-> meta-info
                         (update :contig (fn [xs] (map-indexed (fn [i m] (assoc m :idx (str i))) xs)))
                         (update :filter (fn [xs]
                                           (let [{pass true
                                                  others false} (group-by #(= "PASS" (:id %)) (:filter meta-info))]
                                             (concat (when pass (assoc (first pass) :idx "0"))
                                                     (map-indexed (fn [i m] (assoc m :idx (str (inc i)))) others)))))
                         (update :info (fn [xs] (map-indexed (fn [i m] (assoc m :idx (str i))) xs)))
                         (update :format (fn [xs] (map-indexed (fn [i m] (assoc m :idx (str i))) xs))))
        w (BCFWriter. (util/as-url f) indexed-meta header dos)]
    (write-file-header w)
    w))

(defn- value-type
  "Returns an integer indicating type of input value."
  [v]
  (cond
    (nil? v) 1
    (keyword? v) 1
    (float? v) 5
    (char? v) 7
    (<= Byte/MIN_VALUE v Byte/MAX_VALUE) 1
    (<= Short/MIN_VALUE v Short/MAX_VALUE) 2
    (<= Integer/MIN_VALUE v Integer/MAX_VALUE) 3))

(def ^:private ^:const int8-special-map
  {nil 0x80 :eov 0x81 :exists 1})
(def ^:private ^:const int16-special-map
  {nil 0x8000 :eov 0x8001 :exists 1})
(def ^:private ^:const int32-special-map
  {nil 0x80000000 :eov 0x80000001 :exists 1})
(def ^:private ^:const float32-special-map
  {nil (Float/intBitsToFloat 0x7F800001) :eov (Float/intBitsToFloat 0x7F800002) :exists 1})

(defn- encode-typed-value
  "Encodes given value and returns as a byte-array.
   Allowed input types are integer, float, character, and homogeneous sequence of them. String is also supported."
  ([v]
   (if (= v :exists)
     (byte-array [0x00])
     (encode-typed-value [v] 1)))
  ([[v :as vs] n-sample]
   (let [total-len (apply max 0 (map #(if (or (sequential? %) (string? %)) (count %) 1) vs))
         type-id (long (cond (string? v) 7 (sequential? v) (apply max 1 (map value-type v)) :else (value-type v)))
         type-byte (unchecked-byte (bit-or (bit-shift-left (min 15 total-len) 4) type-id))
         bb (ByteBuffer/allocate (+ (* n-sample total-len (case type-id 1 1 2 2 3 4 5 4 7 1))
                                    (if (<= 15 total-len) (case (int (value-type total-len)) 1 3 2 4 3 6) 1)))]
     (.order bb ByteOrder/LITTLE_ENDIAN)
     (.put bb type-byte)
     (when (<= 15 total-len)
       (.put bb ^bytes (encode-typed-value total-len)))
     (doseq [x vs
             b (if (or (sequential? x) (string? x)) x [x])]
       (case type-id
         1 (.put bb (unchecked-byte (get int8-special-map b b)))
         2 (.putShort bb (unchecked-short (get int16-special-map b b)))
         3 (.putInt bb (unchecked-int (get int32-special-map b b)))
         5 (.putFloat bb (float (get float32-special-map b b)))
         7 (.put bb (byte b))))
     (.array bb))))

(defn- encode-typed-kvs
  "Encodes key-value pairs and returns as a byte-array."
  [kvs]
  (let [bas (->> kvs
                 (mapcat
                  (fn [[k v]]
                    [(encode-typed-value k)
                     (encode-typed-value v)])))
        bb (ByteBuffer/allocate (reduce + (map alength bas)))]
    (doseq [ba bas]
      (.put bb ^bytes ba))
    (.array bb)))

(defn- encode-variant-shared
  "Encodes shared part of a variant and returns as a byte buffer."
  [v]
  (let [chrom-id (:chr v)
        pos (dec (:pos v))
        rlen (:ref-length v)
        qual (if-let [qual-val (:qual v)] qual-val (byte-array [(float32-special-map nil)]))
        n-allele (inc (count (:alt v)))
        n-info (count (:info v))
        n-allele-info (bit-or (bit-shift-left n-allele 16) n-info)
        n-fmt (count (:format v))
        n-sample (:n-sample v)
        n-fmt-sample (bit-or (bit-shift-left n-fmt 24) n-sample)
        id (if-let [id-str (:id v)] (encode-typed-value id-str) (byte-array [0x07]))
        refseq ^bytes (encode-typed-value (:ref v))
        altseq (byte-array (mapcat encode-typed-value (:alt v)))
        flter ^bytes (encode-typed-value (:filter v))
        info ^bytes (encode-typed-kvs (:info v))
        l-shared (+ 4 4 4 4 4 4
                    (alength ^bytes id)
                    (alength refseq)
                    (alength altseq)
                    (alength flter)
                    (alength info))]
    (doto (ByteBuffer/allocate l-shared)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.putInt chrom-id)
      (.putInt pos)
      (.putInt rlen)
      (.putFloat qual)
      (.putInt n-allele-info)
      (.putInt n-fmt-sample)
      (.put ^bytes id)
      (.put refseq)
      (.put altseq)
      (.put flter)
      (.put info))))

(defn- encode-variant-indv
  "Encodes individual part of a variant and returns as a byte buffer"
  [v]
  (let [n-sample (:n-sample v)
        bas (->> (:genotype v)
                 (mapcat
                  (fn [[k values]]
                    (let [vs (map (fn [i] (nth values i nil)) (range n-sample))
                          new-vs (if (some sequential? vs)
                                   (let [max-len (apply max (map count vs))]
                                     (map (fn [i] (or i (concat (repeat (dec max-len) nil) [:eov]))) vs))
                                   vs)]
                      [(encode-typed-value k)
                       (encode-typed-value new-vs n-sample)]))))
        bb (ByteBuffer/allocate (reduce + (map alength bas)))]
    (doseq [ba bas]
      (.put bb ^bytes ba))
    bb))

(defn- write-variant
  "Encodes a BCF-style variant map and write it to writer."
  [w v]
  (let [shared-ba (.array ^ByteBuffer (encode-variant-shared v))
        indv-ba (.array ^ByteBuffer (encode-variant-indv v))]
    (lsb/write-uint w (alength shared-ba))
    (lsb/write-uint w (alength indv-ba))
    (lsb/write-bytes w shared-ba)
    (lsb/write-bytes w indv-ba)))

(defn- update-if-contained
  "Like update, but only affects if k is contained in m."
  [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn- parsed-variant->bcf-map
  "Converts a parsed variant map to BCF-style map."
  [[fmt-kw & indiv-kws :as kws] contigs filters formats info variant]
  (let [fmt (variant fmt-kw)
        indivs (map (fn [kw] (update-if-contained (variant kw) :GT vcf-util/genotype->ints)) indiv-kws)
        genotype (into {} (map (fn [k] [(:idx (formats k)) (map (fn [indiv] (get indiv k)) indivs)])) fmt)]
    (-> (apply dissoc variant kws)
        (assoc :n-sample (count indivs))
        (assoc :ref-length (count (:ref variant)))
        (update :chr (comp :idx contigs))
        (update :filter (fn [f] (map (comp :idx filters) f)))
        (update :info (fn [i] (into {} (map (fn [[k v]] [(:idx (info k)) v])) i)))
        (assoc :format (map (comp :idx formats) fmt))
        (assoc :genotype genotype))))

(defn- meta->map
  "Creates a map for searching meta-info with (f id)."
  [meta f]
  (into {} (map (fn [m] [(f (:id m)) (update m :idx #(Integer/parseInt %))])) meta))

(defn write-variants
  "Writes data lines on writer, returning nil. variants must be a sequence of parsed or VCF-style maps. e.g.

    (write-variants [{:chr \"19\", :pos 111, :id nil, :ref \"A\",
                      :alt [\"C\"], :qual 9.6, :filter [:PASS], :info {:DP 4},
                      :FORMAT [:GT :HQ] ...} ...])"
  [^BCFWriter w variants]
  (let [kws (mapv keyword (drop 8 (.header w)))
        contigs (meta->map (:contig (.meta-info w)) identity)
        filters (assoc (meta->map (:filter (.meta-info w)) keyword) :PASS {:idx 0})
        formats (meta->map (:format (.meta-info w)) keyword)
        info (meta->map (:info (.meta-info w)) keyword)
        parse-variant (vcf-util/variant-parser (.meta-info w) (.header w))]
    (doseq [v variants]
      (->> (if (some string? ((apply juxt :filter :info kws) v)) (parse-variant v) v)
           (parsed-variant->bcf-map kws contigs filters formats info)
           (write-variant (.writer w))))))
