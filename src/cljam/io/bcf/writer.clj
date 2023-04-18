(ns cljam.io.bcf.writer
  (:require [clojure.string :as cstr]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.vcf.writer :as vw]
            [cljam.io.vcf.util :as vcf-util]
            [cljam.util :as util])
  (:import [java.io Closeable DataOutputStream]
           [java.net URL]
           [java.nio ByteBuffer ByteOrder]))

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

(def ^:private ^:const default-fileformat "VCFv4.3")
(def ^:private ^:const bcf-meta-keys
  [:fileformat :file-date :source :reference :contig :phasing :info :filter
   :format :alt :sample :pedigree])
(def ^:private ^:const meta-info-prefix "##")
(def ^:private ^:const type-kws
  {"String" :str, "Character" :char,
   "Integer" :int, "Float" :float, "Flag" :flag})
(def ^:private ^:const default-pass-filter
  {:id "PASS", :description "All filters passed"})

(defn- stringify-meta
  "Converts meta-info rows to a sequence of strings."
  [meta-info]
  (apply
   concat
   (for [k bcf-meta-keys :let [v (meta-info k)] :when v]
     (if (sequential? v)
       (for [x v]
         (str meta-info-prefix (vw/stringify-key k) "=<" (vw/stringify-structured-line k x) ">"))
       [(str meta-info-prefix (vw/stringify-key k) "=" v)]))))

(defn- write-file-header
  "Writes BCF file header, meta-info and header row to writer."
  [^BCFWriter w]
  (let [wtr ^DataOutputStream (.writer w)
        hdr-ba (-> \newline
                   (cstr/join
                    (concat
                     (stringify-meta (.meta-info w))
                     [(vw/stringify-header (.header w))]))
                   (str \newline) ;; newline at the end of the header
                   (str (char 0)) ;; NULL-terminated
                   .getBytes)
        hlen (alength hdr-ba)]
    (lsb/write-bytes wtr (byte-array (map byte "BCF\2\2")))
    (lsb/write-int wtr hlen)
    (lsb/write-bytes wtr hdr-ba)))

(defn- index-meta
  [meta-info]
  (let [m (update meta-info :filter
                  (fn [xs]
                    (let [{[p] true, f false} (group-by #(= "PASS" (:id %)) xs)]
                      (cons (or p default-pass-filter) f))))
        fif (->> [:filter :info :format]
                 (mapcat #(map vector (repeat %) (% m)))
                 (map-indexed (fn [i [k v]] [k (assoc v :idx (str i))]))
                 (reduce (fn [r [k v]] (update r k (fnil conj []) v)) {}))]
    (-> meta-info
        (update :contig #(map-indexed (fn [i c] (assoc c :idx (str i))) %))
        (merge fif))))

(defn writer
  "Returns an open cljam.bcf.BCFWriter of f.
   Meta-information lines and a header line will be written in this function.
   Should be used inside with-open to ensure the Writer is properly closed. e.g.

     (with-open [wtr (writer \"out.bcf\"
                             {:file-date \"20090805\", :source \"myImpu...\" ...}
                             [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
       (WRITING-BCF))"
  ^BCFWriter
  [f meta-info header]
  (let [bos (bgzf/bgzf-output-stream f)
        dos (DataOutputStream. bos)
        indexed-meta (->> meta-info
                          (merge {:fileformat default-fileformat})
                          index-meta)]
    (doto (BCFWriter. (util/as-url f) indexed-meta header dos)
      (write-file-header))))

(defn- value-type
  "Returns an integer indicating type of input value."
  ^long [v]
  (cond
    (nil? v) 1
    (keyword? v) 1
    (float? v) 5
    (char? v) 7
    ;; 0x80-0x87, 0x8000-0x8007 and 0x80000000-0x80000007
    ;; are reserved for future use in the BCF2 spec.
    (<= (+ Byte/MIN_VALUE 8) v Byte/MAX_VALUE) 1
    (<= (+ Short/MIN_VALUE 8) v Short/MAX_VALUE) 2
    (<= (+ Integer/MIN_VALUE 8) v Integer/MAX_VALUE) 3))

(def ^:private ^:const int8-special-map
  {nil 0x80 :eov 0x81 :exists 1})
(def ^:private ^:const int16-special-map
  {nil 0x8000 :eov 0x8001 :exists 1})
(def ^:private ^:const int32-special-map
  {nil 0x80000000 :eov 0x80000001 :exists 1})
(def ^:private ^:const float32-special-map
  {nil 0x7F800001 :eov 0x7F800002 :exists 1})

(defn- encode-typed-value
  (^bytes [element-type v]
   (if (= v :exists)
     (byte-array [0x00])
     (encode-typed-value element-type [v] 1)))
  (^bytes [element-type vs ^long n-sample]
   (let [str? (or (= element-type :str) (= element-type :char))
         vs (map (fn [v]
                   (let [v (if (sequential? v) v [v])]
                     (if str? (cstr/join \, (map #(or % ".") v)) v))) vs)
         max-len (long (apply max 0 (map count vs)))
         vs (map
             (fn [v]
               (let [l (max 1 (count v))]
                 (if (< l max-len)
                   (if str?
                     (apply str v (repeat (- max-len l) (char 0)))
                     (concat
                      (or (seq v) [nil])
                      (repeat (- max-len l) :eov)))
                   v))) vs)
         type-id (case element-type
                   (:str :char) 7
                   :float 5
                   (long (apply max 1 (mapcat (partial map value-type) vs))))
         type-byte (bit-or (bit-shift-left (min 15 max-len) 4) type-id)
         len-bytes (when (<= 15 max-len)
                     (encode-typed-value :int max-len))
         n-bytes (+ (* n-sample max-len (case type-id 1 1 2 2 3 4 5 4 7 1))
                    1 (if len-bytes (alength ^bytes len-bytes) 0))
         bb (ByteBuffer/allocate n-bytes)]
     (.order bb ByteOrder/LITTLE_ENDIAN)
     (.put bb (unchecked-byte type-byte))
     (when len-bytes
       (.put bb ^bytes len-bytes))
     (doseq [v vs
             b v]
       (case type-id
         1 (.put bb (unchecked-byte (get int8-special-map b b)))
         2 (.putShort bb (unchecked-short (get int16-special-map b b)))
         3 (.putInt bb (unchecked-int (get int32-special-map b b)))
         5 (.putInt bb (unchecked-int (or (get float32-special-map b)
                                          (Float/floatToRawIntBits b))))
         7 (.put bb (byte (get {nil 0 :eov 0} b b)))))
     (.array bb))))

(defn- concat-bytes
  ^bytes [xs]
  (let [l (long (transduce (map alength) + xs))
        bb (ByteBuffer/allocate l)]
    (doseq [x xs]
      (.put bb ^bytes x))
    (.array bb)))

(defn- encode-variant-shared
  "Encodes shared part of a variant and returns as a byte buffer."
  ^ByteBuffer [{:keys [chr pos id ref-length alt qual info n-sample]
                ref-bases :ref filters :filter formats :format}]
  (let [chrom-id (unchecked-int chr)
        pos (unchecked-int (dec ^long pos))
        rlen (unchecked-int ref-length)
        qual (unchecked-int (if qual
                              (Float/floatToRawIntBits qual)
                              (float32-special-map nil)))
        n-allele (inc (count alt))
        n-info (count info)
        n-allele-info (bit-or (bit-shift-left n-allele 16) n-info)
        n-fmt (count formats)
        n-fmt-sample (bit-or (bit-shift-left n-fmt 24) ^long n-sample)
        id (if id (encode-typed-value :str id) (byte-array [0x07]))
        refseq ^bytes (encode-typed-value :str ref-bases)
        altseq (concat-bytes (map (partial encode-typed-value :str) alt))
        filters (if-let [f (seq filters)]
                  (encode-typed-value :int f)
                  (byte-array [0x00]))
        info (if (pos? n-info)
               (->> info
                    (mapcat
                     (fn [[k t v]]
                       [(encode-typed-value :int k)
                        (encode-typed-value t v)]))
                    concat-bytes)
               (byte-array 0))
        l-shared (+ 24
                    (alength id) (alength refseq) (alength altseq)
                    (alength filters) (alength info))]
    (doto (ByteBuffer/allocate l-shared)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.putInt chrom-id)
      (.putInt pos)
      (.putInt rlen)
      (.putInt qual)
      (.putInt n-allele-info)
      (.putInt n-fmt-sample)
      (.put id)
      (.put refseq)
      (.put altseq)
      (.put filters)
      (.put info))))

(defn- encode-variant-indv
  [{:keys [^long n-sample genotype]}]
  (->> genotype
       (mapcat
        (fn [[k t vs]]
          [(encode-typed-value :int k)
           (encode-typed-value t vs n-sample)]))
       concat-bytes
       (ByteBuffer/wrap)))

(defn- write-variant
  "Encodes a BCF-style variant map and write it to writer."
  [w v]
  (let [shared-ba (.array ^ByteBuffer (encode-variant-shared v))
        indv-ba (.array ^ByteBuffer (encode-variant-indv v))]
    (lsb/write-uint w (alength shared-ba))
    (lsb/write-uint w (alength indv-ba))
    (lsb/write-bytes w shared-ba)
    (lsb/write-bytes w indv-ba)))

(defn- parsed-variant->bcf-map
  "Converts a parsed variant map to BCF-style map."
  [[fmt-kw & indiv-kws :as kws] contigs filters formats info variant]
  (let [fmts (keep (fn [f] (when-let [m (formats f)] [f m])) (variant fmt-kw))
        genotype (map
                  (fn [[k {:keys [idx type-kw]}]]
                    (->> indiv-kws
                         (map #(cond-> (get-in variant [% k] nil)
                                 (= k :GT) vcf-util/genotype->ints))
                         (vector idx type-kw)))
                  fmts)]
    (-> (apply dissoc variant kws)
        (assoc :n-sample (count indiv-kws)
               :ref-length (if-let [e (get-in variant [:info :END])]
                             (inc (- e (:pos variant)))
                             (count (:ref variant)))
               :format (map (comp :idx second) fmts)
               :genotype genotype)
        (update :chr (comp :idx contigs))
        (update :filter (fn [f] (map (comp :idx filters) f)))
        (update :info (fn [i]
                        (keep (fn [[k v]]
                                (when-let [{:keys [idx type-kw]} (info k)]
                                  [idx type-kw v])) i))))))

(defn- meta->map
  "Creates a map for searching meta-info with (f id)."
  [meta f]
  (into {} (map
            (fn [{:keys [id] t :type :as m}]
              [(f id) (cond-> (update m :idx #(Integer/parseInt %))
                        t (assoc :type-kw (type-kws t)))])) meta))

(defn write-variants
  "Writes data lines on writer. Returns nil. `variants` must be a sequence of
  parsed or VCF-style maps.
  e.g.

    (write-variants [{:chr \"19\", :pos 111, :id nil, :ref \"A\",
                      :alt [\"C\"], :qual 9.6, :filter [:PASS], :info {:DP 4},
                      :FORMAT [:GT :HQ] ...} ...])"
  [^BCFWriter w variants]
  (let [kws (mapv keyword (drop 8 (.header w)))
        contigs (meta->map (:contig (.meta-info w)) identity)
        filters (assoc (meta->map (:filter (.meta-info w)) keyword)
                       :PASS {:idx 0})
        formats (-> (.meta-info w)
                    :format
                    (meta->map keyword)
                    (assoc-in [:GT :type-kw] :int)
                    (assoc-in [:GT :number] nil))
        info (meta->map (:info (.meta-info w)) keyword)
        parse-variant (vcf-util/variant-parser (.meta-info w) (.header w))]
    (doseq [v variants]
      (->> (if (some string? ((apply juxt :filter :info kws) v))
             (parse-variant v)
             v)
           (parsed-variant->bcf-map kws contigs filters formats info)
           (write-variant (.writer w))))))
