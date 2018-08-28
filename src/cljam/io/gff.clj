(ns cljam.io.gff
  (:require [clojure.string :as cstr]
            [clojure.java.io :as cio]
            [proton.core :as p]
            [cljam.util :as util])
  (:import [java.io Closeable BufferedReader BufferedWriter]
           java.nio.CharBuffer))

;; Encoder / Decoder
;; -----------------

(defn- escape-in-column? [^long i]
  (or (<= i 0x1F)
      (= i 0x25)
      (= i 0x7F)))

(defn- escape-in-attr? [^long i]
  (or (<= i 0x1F)
      (= i 0x25)
      (= i 0x26)
      (= i 0x2C)
      (= i 0x3B)
      (= i 0x3D)
      (= i 0x7F)))

(defn- escape-in-target? [^long i]
  (or (<= i 0x1F)
      (= i 0x20)
      (= i 0x25)
      (= i 0x26)
      (= i 0x2C)
      (= i 0x3B)
      (= i 0x3D)
      (= i 0x7F)))

(defn- ^String encode [pred ^String s]
  (let [cb (CharBuffer/wrap s)
        sb (StringBuilder. (.length s))]
    (while (.hasRemaining cb)
      (let [c (.get cb)
            i (int c)]
        (if (pred i)
          (let [upper (bit-and 0xf (unsigned-bit-shift-right i 4))
                lower (bit-and 0xf i)]
            (.append sb \%)
            (.append sb (unchecked-char
                         (if (<= upper 9) (+ 48 upper) (+ 55 upper))))
            (.append sb (unchecked-char
                         (if (<= lower 9) (+ 48 lower) (+ 55 lower)))))
          (.append sb c))))
    (str sb)))

(defn- ^String encode-in-attr [s]
  (encode escape-in-attr? s))

(defn- ^String decode [pred ^String s]
  (when s
    (let [cb (CharBuffer/wrap s)
          sb (StringBuilder. (.length s))]
      (while (.hasRemaining cb)
        (let [c (.get cb)]
          (if (= \% c)
            (let [upper (.get cb)
                  lower (.get cb)
                  i (bit-or
                     (bit-shift-left
                      (unchecked-int (Character/digit upper 16)) 4)
                     (unchecked-int (Character/digit lower 16)))]
              (if (pred i)
                (.append sb (unchecked-char i))
                (throw
                 (ex-info
                  "Found an invalid character encoding while decoding GFF3 file"
                  {:input s, :invalid-string (str c upper lower)}))))
            (.append sb c))))
      (str sb))))

(defn- ^String decode-in-attr [s]
  (decode escape-in-attr? s))

(defn- ^String encode-multiple [xs]
  (cstr/join \, (map encode-in-attr xs)))

(defn- decode-multiple [s]
  (map (partial decode escape-in-attr?) (cstr/split s #",")))

(def ^:const ^:private
  target-regexp
  #"(\S+) ([1-9]\d*) ([1-9]\d*)(?: ([+-]))?")

(defn- ^String encode-target [{:keys [chr start end strand]}]
  (cstr/join \space (cond-> [(encode escape-in-target? chr) start end]
                      strand (conj (case strand :forward \+ :reverse \-)))))

(defn- decode-target [s]
  (when-let [[_ target-id start end strand] (re-matches target-regexp s)]
    (cond-> {:chr (decode escape-in-target? target-id)
             :start (p/as-long start)
             :end (p/as-long end)}
      strand (assoc :strand (case (first strand) \+ :forward \- :reverse)))))

(defn- ^String encode-gap [xs]
  (cstr/join \space (map (fn [[op len]] (str op len)) xs)))

(defn- decode-gap [s]
  (->> s
       (re-seq #"(?:^|\s)([MIDFR])([1-9]\d*)")
       (map
        (fn [[_ [op] len]]
          [op (p/as-long len)]))))

(defn- ^String encode-db [xs]
  (->> xs
       (map
        (fn [{:keys [db-tag id]}]
          (str
           (encode escape-in-attr? db-tag)
           \:
           (encode escape-in-attr? id))))
       (cstr/join \,)))

(defn- decode-db [s]
  (->> (cstr/split s #",")
       (map (fn [x]
              (let [[db-tag id] (cstr/split x #":" 2)]
                {:db-tag (decode escape-in-attr? db-tag),
                 :id (decode escape-in-attr? id)})))))

(defn- dot->nil [^String s]
  (when-not (and s
                 (zero? (dec (.length s)))
                 (= \. (.charAt s 0)))
    s))

(def ^:const ^:private
  predefined-tags
  ;; `:index` is not defined in the spec, can be ignored
  {"ID" {:index 0, :key :id,
         :encoder encode-in-attr, :decoder decode-in-attr},
   "Name" {:index 2, :key :name,
           :encoder encode-in-attr, :decoder decode-in-attr},
   "Alias" {:index 3, :key :alias,
            :encoder encode-multiple, :decoder decode-multiple},
   "Parent" {:index 1, :key :parent,
             :encoder encode-multiple, :decoder decode-multiple},
   "Target" {:index 4, :key :target,
             :encoder encode-target, :decoder decode-target},
   "Gap" {:index 5, :key :gap,
          :encoder encode-gap, :decoder decode-gap},
   "Derives_from" {:index 6, :key :derives-from,
                   :encoder encode-in-attr, :decoder decode-in-attr},
   "Note" {:index 7, :key :note,
           :encoder encode-multiple, :decoder decode-multiple},
   "Dbxref" {:index 8, :key :db-xref,
             :encoder encode-db, :decoder decode-db},
   "Ontology_term" {:index 9, :key :ontology-term,
                    :encoder encode-db, :decoder decode-db},
   "Is_circular" {:index 10, :key :circular?,
                  :encoder str, :decoder #(Boolean/parseBoolean %)}})

;; Reader
;; ------

(deftype GFFReader [reader version]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(def ^:const ^:private
  version-regexp
  #"##gff-version ([1-9]\d*)(?:\.([1-9]\d*)(?:\.([1-9]\d*))?)?")

(defn version
  "Returns a file format version of the given `reader`."
  [^GFFReader reader]
  (.version reader))

(defn ^GFFReader reader
  "Returns an open `cljam.io.gff.GFFReader` instance of `f`. Should be used
  inside `with-open` to ensure the reader is properly closed."
  [f]
  (let [r ^BufferedReader (cio/reader (util/compressor-input-stream f))]
    (try
      (let [version-line (.readLine r)
            [version-directive & xs] (re-matches version-regexp version-line)
            {:keys [version] :as v} (-> [:version
                                         :major-revision
                                         :minor-revision]
                                        (zipmap (map p/as-long xs)))]
        (when-not version-directive
          (throw
           (ex-info
            "GFF3 must start with the `##gff-version 3.#.#` directive"
            {:url (try (util/as-url f) (catch Exception _ nil)),
             :version-directive version-line})))
        (when-not (= version 3)
          (throw
           (ex-info
            "Only GFF version 3 is supported"
            (assoc v :url (try (util/as-url f) (catch Exception _ nil))))))
        (GFFReader. r v))
      (catch Exception e
        (.close r)
        (throw e)))))

(defn- parse-attr [s]
  (let [[raw-tag value] (cstr/split s #"=" 2)
        tag' (decode-in-attr raw-tag)
        {:keys [key decoder]
         :or {key tag'
              decoder decode-multiple}} (predefined-tags tag')]
    [key (decoder value)]))

(defn- parse-attrs
  [s]
  (into {} (map parse-attr) (some-> s (cstr/split #";"))))

(defn- parse-gff-line
  [s]
  (let [[seq-id src typ start end
         score strand phase attrs] (cstr/split s #"\t" 9)]
    {:chr (->> seq-id dot->nil (decode escape-in-column?))
     :source (->> src dot->nil (decode escape-in-column?))
     :type (->> typ dot->nil (decode escape-in-column?))
     :start (p/as-long start)
     :end (p/as-long end)
     :score (p/as-double score)
     ;; +: forward, -: reverse, ?: unknown, nil: not-stranded
     :strand (some-> strand dot->nil first (case \+ :forward \- :reverse \? :unknown))
     :phase (some-> phase dot->nil first (case \0 0 \1 1 \2 2))
     :attributes (-> attrs dot->nil parse-attrs)}))

(defn read-features
  "Reads features of the GFF file, returning them as a lazy sequence. `reader`
  must be an instance of `cljam.io.gff.GFFReader`."
  [^GFFReader reader]
  (->> reader
       .reader
       line-seq
       (sequence
        (comp
         ;; TODO: handle FASTA sequences
         (take-while #(not (or (cstr/starts-with? % "##FASTA")
                               (cstr/starts-with? % ">"))))
         (comp
          ;; TODO: handle other directives
          (remove #(cstr/starts-with? % "#"))
          ;; TODO: construct tree structures
          (map parse-gff-line))))))

;; Writer
;; ------

(deftype GFFWriter [writer version]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))))

(defn ^GFFWriter writer
  "Returns an open `cljam.io.gff.GFFWriter` instance of `f`. Should be used
  inside `with-open` to ensure the writer is properly closed. Can take an
  optional argument `options`, a map containing `:version`, `:major-revision`,
  `:minor-revision` and `:encoding`. Currently supporting only `:version` 3.
  To compress outputs, set `:encoding` to `:gzip` or `:bzip2`."
  ([f]
   (writer f {}))
  ([f options]
   (let [{:keys [encoding version] :as opts} (merge {:version 3} options)
         url (try (util/as-url f) (catch Exception _ nil))]
     (when-not (= 3 version)
       (throw (ex-info "Only GFF3 is supported" (assoc opts :url url))))
     (-> (cond
           encoding (util/compressor-output-stream f encoding)
           url (util/compressor-output-stream f)
           :else f)
         cio/writer
         (GFFWriter. opts)))))

(def ^:const ^:private
  inv-predefined-tags
  (->> predefined-tags
       (map (fn [[key-str {:keys [key] :as x}]]
              [key (assoc x :key-str key-str)]))
       (into {})))

(def ^:const ^:private
  predefined-keys
  (map (comp :key val) (sort-by (comp :index val) predefined-tags)))

(defn- write-attrs!
  [^BufferedWriter w attrs]
  (let [first? (volatile! true)]
    (doseq [key (concat predefined-keys
                        (apply disj (set (keys attrs)) predefined-keys))
            :let [value (get attrs key)]
            :when value
            :let [{:keys [^String key-str encoder]
                   :or {key-str key
                        encoder encode-multiple}} (inv-predefined-tags key)]]
      (if @first?
        (vreset! first? false)
        (.append w \;))
      (.write w key-str)
      (.append w \=)
      (.write w ^String (encoder value)))))

(defn- write-feature!
  [^BufferedWriter w {:keys [chr ^String source ^String type ^long start
                             ^long end score ^Character strand phase
                             attributes]}]
  (.write w (encode escape-in-column? chr))
  (.append w \tab)
  (.write w (or (some->> source (encode escape-in-column?)) "."))
  (.append w \tab)
  (.write w (or (some->> type (encode escape-in-column?)) "."))
  (.append w \tab)
  (.write w (String/valueOf start))
  (.append w \tab)
  (.write w (String/valueOf end))
  (.append w \tab)
  (.write w (or (some->> score String/valueOf cstr/lower-case) "."))
  (.append w \tab)
  (.append w (case strand :forward \+ :reverse \- :unknown \? nil \.))
  (.append w \tab)
  (.append w (if (nil? phase) \. (case (byte phase) 0 \0 1 \1 2 \2)))
  (.append w \tab)
  (if (seq attributes)
    (write-attrs! w attributes)
    (.append w \.)))

(defn write-features
  "Writes `features` to the GFF file. `writer` must be an instance of
  `cljam.io.gff.GFFWriter`. `features` must be a sequence of feature maps."
  [^GFFWriter writer features]
  (let [w ^BufferedWriter (.writer writer)
        {:keys [^long version major-revision minor-revision]} (.version writer)]
    (.write w "##gff-version ")
    (.write w (String/valueOf version))
    (when major-revision
      (.append w \.)
      (.write w (String/valueOf major-revision))
      (when minor-revision
        (.append w \.)
        (.write w (String/valueOf minor-revision))))
    (doseq [f features]
      (.newLine w)
      (write-feature! w f))))
