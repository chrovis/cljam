(ns cljam.io.bed
  "Functions to read and write the BED (Browser Extensible Data) format. See
  http://genome.ucsc.edu/FAQ/FAQformat#format1 for the detail BED specifications."
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [proton.core :refer [as-int as-long]]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]
            [cljam.util.chromosome :as chr]
            [cljam.util.region :as region]
            [clojure.tools.logging :as logging])
  (:import [java.net URL]
           [java.io BufferedReader BufferedWriter Closeable]))

(declare read-fields write-fields)

(defrecord BEDReader [^BufferedReader reader ^URL url]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (protocols/read this {}))
  (read [this option] (read-fields this))
  (indexed? [_] false)
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this {:keys [chr start end]} option]
    (logging/warn "May cause degradation of performance.")
    (filter (fn [m] (and (or (not chr) (= (:chr m) chr))
                         (or (not start) (<= start (:start m)))
                         (or (not end) (<= (:end m) end))))
            (read-fields this))))

(defrecord BEDWriter [^BufferedWriter writer ^URL url]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-url [this] (.url this)))

(defn ^BEDReader reader
  "Returns an open cljam.io.bed.BEDReader of f. Should be used inside with-open
  to ensure the reader is properly closed."
  [f]
  (let [abs (.getAbsolutePath (cio/file f))]
    (BEDReader. (cio/reader (util/compressor-input-stream abs)) (util/as-url abs))))

(defn ^BEDWriter writer
  "Returns an open cljam.io.bed.BEDWriter of f. Should be used inside with-open
  to ensure the writer is properly closed."
  [f]
  (let [abs (.getAbsolutePath (cio/file f))]
  (BEDWriter. (cio/writer (util/compressor-output-stream abs)) (util/as-url abs))))

(def ^:const bed-columns
  [:chr :start :end :name :score :strand :thick-start :thick-end :item-rgb :block-count :block-sizes :block-starts])

(defn- str->long-list
  "Convert string of comma-separated long values into list of longs.
  Comma at the end of input string will be ignored.
  Returns nil if input is nil."
  [^String s]
  (when-not (nil? s)
    (map as-long (cstr/split s #","))))

(defn- long-list->str
  "Inverse function of str->long-list."
  [xs]
  (when (seq xs)
    (cstr/join "," xs)))

(defn- update-some
  "Same as update if map 'm' contains key 'k'. Otherwise returns the original map 'm'."
  [m k f & args]
  (if (get m k)
    (apply update m k f args)
    m))

(defn- deserialize-bed
  "Parse BED fields string and returns a map.
  Based on information at https://genome.ucsc.edu/FAQ/FAQformat#format1."
  [^String s]
  {:post [;; First 3 fields are required.
          (:chr %) (:start %) (:end %)
          ;; The chromEnd base is not included in the display of the feature.
          (< (:start %) (:end %))
          ;; Lower-numbered fields must be populated if higher-numbered fields are used.
          (every? true? (drop-while false? (map nil? ((apply juxt bed-columns) %))))
          ;; A score between 0 and 1000.
          (if-let [s (:score %)] (<= 0 s 1000) true)
          ;; The number of items in this list should correspond to blockCount.
          (if-let [xs (:block-sizes %)] (= (count xs) (:block-count %)) true)
          ;; The number of items in this list should correspond to blockCount.
          (if-let [xs (:block-starts %)] (= (count xs) (:block-count %)) true)
          ;; The first blockStart value must be 0.
          (if-let [[f] (:block-starts %)] (= 0 f) true)
          ;; The final blockStart position plus the final blockSize value must equal chromEnd.
          (if-let [xs (:block-starts %)] (= (+ (last xs) (last (:block-sizes %))) (- (:end %) (:start %))) true)
          ;; Blocks may not overlap.
          (if-let [xs (:block-starts %)] (apply <= (mapcat (fn [a b] [a (+ a b)]) xs (:block-sizes %))) true)]}
  (reduce
   (fn deserialize-bed-reduce-fn [m [k f]] (update-some m k f))
   (zipmap bed-columns (cstr/split s #"\s+"))
   {:start as-long
    :end as-long
    :score as-long
    :strand #(case % "." :no-strand "+" :plus "-" :minus)
    :thick-start as-long
    :thick-end as-long
    :block-count as-long
    :block-sizes str->long-list
    :block-starts str->long-list}))

(defn- serialize-bed
  "Serialize bed fields into string."
  [m]
  {:pre [;; First 3 fields are required.
         (:chr m) (:start m) (:end m)
         ;; The chromEnd base is not included in the display of the feature.
         (< (:start m) (:end m))
         ;; Lower-numbered fields must be populated if higher-numbered fields are used.
         (every? true? (drop-while false? (map nil? ((apply juxt bed-columns) m))))
         ;; A score between 0 and 1000.
         (if-let [s (:score m)] (<= 0 s 1000) true)
         ;; The number of items in this list should correspond to blockCount.
         (if-let [xs (:block-sizes m)] (= (count xs) (:block-count m)) true)
         ;; The number of items in this list should correspond to blockCount.
         (if-let [xs (:block-starts m)] (= (count xs) (:block-count m)) true)
         ;; The first blockStart value must be 0.
         (if-let [[f] (:block-starts m)] (= 0 f) true)
         ;; The final blockStart position plus the final blockSize value must equal chromEnd.
         (if-let [xs (:block-starts m)] (= (+ (last xs) (last (:block-sizes m))) (- (:end m) (:start m))) true)
         ;; Blocks may not overlap.
         (if-let [xs (:block-starts m)] (apply <= (mapcat (fn [a b] [a (+ a b)]) xs (:block-sizes m))) true)]}
  (->> (-> m
           (update-some :strand #(case % :plus "+" :minus "-" :no-strand "."))
           (update-some :block-sizes long-list->str)
           (update-some :block-starts long-list->str))
       ((apply juxt bed-columns))
       (take-while identity)
       (cstr/join \tab)))

(defn- header-or-comment?
  "Checks if given string is neither a header nor a comment line."
  [^String s]
  (or (empty? s)
      (.startsWith s "browser")
      (.startsWith s "track")
      (.startsWith s "#")))

(defn- normalize
  "Normalize BED fields.
  BED fields are stored in format: 0-origin and inclusive-start / exclusive-end.
  This function converts the coordinate into cljam style: 1-origin and inclusive-start / inclusive-end."
  [m]
  (-> m
      (update :chr chr/normalize-chromosome-key)
      (update :start inc)
      (update-some :thick-start inc)))

(defn- denormalize
  "De-normalize BED fields.
  This is an inverse function of normalize."
  [m]
  (-> m
      (update :start dec)
      (update-some :thick-start dec)))

(defn read-raw-fields
  "Returns a lazy sequence of unnormalized BED fields."
  [^BEDReader rdr]
  (sequence
   (comp (remove header-or-comment?)
         (map deserialize-bed))
   (line-seq (.reader rdr))))

(defn read-fields
  "Returns a lazy sequence of normalized BED fields."
  [^BEDReader rdr]
  (sequence
   (comp (remove header-or-comment?)
         (map deserialize-bed)
         (map normalize))
   (line-seq (.reader rdr))))

(defn sort-fields
  "Sort BED fields based on :chr, :start and :end.
  :chr with common names come first, in order of (chr1, chr2, ..., chrX, chrY, chrM).
  Other chromosomes follow after in lexicographic order."
  [xs]
  (sort-by
   (fn [m]
     [(chr/chromosome-order-key (:chr m))
      (:start m)
      (:end m)])
   xs))

(defn merge-fields
  "Sort and merge overlapped regions.
  Currently, this function affects only :end and :name fields."
  [xs]
  (region/merge-regions-with
   (fn [x {:keys [name end]}]
     (-> x
         (update :end max end)
         (update-some :name str "+" name)))
   0
   (sort-fields xs)))

(defn- fields-<=
  "Compare two BED fields on the basis of :chr and :end fields."
  [x y]
  (<= (compare [(chr/chromosome-order-key (:chr x)) (:end x)]
               [(chr/chromosome-order-key (:chr y)) (:end y)])
      0))

(defn intersect-fields
  "Returns a lazy sequence that is the intersection of the two BED sequences.

  The input sequences will first be sorted with sort-fields, which may cause
  an extensive memory use for ones with a large number of elements.
  Note also that this function assumes the input sequences contain only valid
  regions, and thus :start <= :end holds for each region. Make sure yourself
  the input sequences meet the condition, or the function may return a wrong
  result."
  [xs ys]
  (letfn [(intersect [xs ys]
            (lazy-seq
             (if-let [x (first xs)]
               (if-let [y (first ys)]
                 (cond->> (if (fields-<= x y)
                            (intersect (next xs) ys)
                            (intersect xs (next ys)))
                   (region/overlapped-regions? x y)
                   (cons (-> x
                             (update :start max (:start y))
                             (update :end min (:end y)))))
                 [])
               [])))]
    (intersect (sort-fields xs) (merge-fields ys))))

(defn subtract-fields
  "Returns a lazy sequence that is the result of subtracting the BED fields
  in the sequence ys from the sequence xs.

  The input sequences will first be sorted with sort-fields, which may cause
  an extensive memory use for ones with a large number of elements.
  Note also that this function assumes the input sequences contain only valid
  regions, and thus :start <= :end holds for each region. Make sure yourself
  the input sequences meet the condition, or the function may return a wrong
  result."
  [xs ys]
  (letfn [(subtract [xs ys]
            (lazy-seq
             (if-let [x (first xs)]
               (if-let [y (first ys)]
                 (let [[r1 r2] (region/subtract-region x y)]
                   (if r2
                     (cons r1 (subtract (cons r2 (next xs)) (next ys)))
                     (if r1
                       (if (fields-<= r1 y)
                         (cons r1 (subtract (next xs) ys))
                         (subtract (cons r1 (next xs)) (next ys)))
                       (subtract (next xs) ys))))
                 xs)
               [])))]
    (subtract (sort-fields xs) (merge-fields ys))))

(defn complement-fields
  "Takes a sequence of maps containing :name and :len keys (representing
  chromosome's name and length, resp.) and a sequence of BED fields,
  and returns a lazy sequence that is the complement of the BED sequence.

  The input sequence will first be sorted with sort-fields, which may cause
  an extensive memory use for ones with a large number of elements.
  Note also that this function assumes the BED sequence contains only valid
  regions, and thus :start <= :end holds for each region. Make sure yourself
  the BED sequence meets the condition, or the function may return a wrong
  result."
  [refs xs]
  (let [chr->len (into {} (map (juxt :name :len)) refs)
        chrs (sort-by chr/chromosome-order-key (map :name refs))]
    (when-first [{:keys [chr]} (filter #(not (chr->len (:chr %))) xs)]
      (let [msg (str "Length of chromosome " chr " not specified")]
        (throw (IllegalArgumentException. msg))))
    (letfn [(complement [xs chrs pos]
              (lazy-seq
               (when-let [chr (first chrs)]
                 (let [len (get chr->len chr)
                       x (first xs)]
                   (if (and x (= (:chr x) chr))
                     (cond->> (complement (next xs) chrs (inc (:end x)))
                       (< pos (:start x))
                       (cons {:chr chr :start pos :end (dec (:start x))}))
                     (cond->> (complement xs (next chrs) 1)
                       (< pos len)
                       (cons {:chr chr :start pos :end len})))))))]
      (complement (merge-fields xs) chrs 1))))

(defn write-raw-fields
  "Write sequence of BED fields to writer without converting :start and :thick-start values."
  [^BEDWriter wtr xs]
  (let [w ^BufferedWriter (.writer wtr)]
    (->> xs
         (map serialize-bed)
         (cstr/join \newline)
         (.write w))))

(defn write-fields
  "Write sequence of BED fields to writer."
  [^BEDWriter wtr xs]
  (let [w ^BufferedWriter (.writer wtr)]
    (->> xs
         (map (comp serialize-bed denormalize))
         (cstr/join \newline)
         (.write w))))
