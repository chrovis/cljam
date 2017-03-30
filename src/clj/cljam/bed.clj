(ns cljam.bed
  (:require [clojure.string :as cstr]
            [cljam.util :as util]
            [cljam.util.chromosome :as chr-util])
  (:import [java.io BufferedReader BufferedWriter]))

(def ^:const bed-columns
  [:chr :start :end :name :score :strand :thick-start :thick-end :item-rgb :block-count :block-sizes :block-starts])

(defn- str->long-list
  "Convert string of comma-separated long values into list of longs.
  Comma at the end of input string will be ignored.
  Returns nil if input is nil."
  [^String s]
  (when-not (nil? s)
    (map util/str->long (cstr/split s #","))))

(defn- long-list->str
  "Inverse function of str->long-list."
  [xs]
  (when-not (nil? xs)
    (apply str (interpose "," xs))))

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
  {:post [(and (:chr %) (:start %) (:end %))
          ;; First 3 fields are required.
          (< (:start %) (:end %))
          ;; The chromEnd base is not included in the display of the feature.
          (every? true? (drop-while false? (map nil? ((apply juxt bed-columns) %))))
          ;; Lower-numbered fields must be populated if higher-numbered fields are used.
          (if-let [s (:score %)] (<= 0 s 1000) true)
          ;; A score between 0 and 1000.
          (if-let [xs (:block-sizes %)] (= (count xs) (:block-count %)) true)
          ;; The number of items in this list should correspond to blockCount.
          (if-let [xs (:block-starts %)] (= (count xs) (:block-count %)) true)
          ;; The number of items in this list should correspond to blockCount.
          (if-let [[f] (:block-starts %)] (= 0 f) true)
          ;; The first blockStart value must be 0.
          (if-let [xs (:block-starts %)] (= (+ (last xs) (last (:block-sizes %))) (- (:end %) (:start %))) true)
          ;; The final blockStart position plus the final blockSize value must equal chromEnd.
          (if-let [xs (:block-starts %)] (apply <= (mapcat (fn [a b] [a (+ a b)]) xs (:block-sizes %))) true)
          ;; Blocks may not overlap.
          ]}
  (reduce
   (fn deserialize-bed-reduce-fn [m [k f]] (update-some m k f))
   (zipmap bed-columns (cstr/split s #"\s+"))
   {:start util/str->long
    :end util/str->long
    :score util/str->long
    :strand #(case % "." :no-strand "+" :plus "-" :minus)
    :thick-start util/str->long
    :thick-end util/str->long
    :block-count util/str->long
    :block-sizes str->long-list
    :block-starts str->long-list}))

(defn- serialize-bed
  "Serialize bed fields into string."
  [m]
  (->> (-> m
           (update-some :strand #(case % :plus "+" :minus "-" :no-strand "."))
           (update-some :block-sizes long-list->str)
           (update-some :block-starts long-list->str))
       ((apply juxt bed-columns))
       (take-while identity)
       (interpose " ")
       (apply str)))

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
  This function converts the coordinate into cljam style: 1-origin and inclusice-start / inclusive-end."
  [m]
  (-> m
      (update :chr chr-util/normalize-chromosome-key)
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
  [^BufferedReader rdr]
  (sequence
   (comp (remove header-or-comment?)
         (map deserialize-bed))
   (line-seq rdr)))

(defn read-fields
  "Returns a lazy sequence of normalized BED fields."
  [^BufferedReader rdr]
  (sequence
   (comp (remove header-or-comment?)
         (map deserialize-bed)
         (map normalize))
   (line-seq rdr)))

(defn sort-fields
  "Sort BED fields based on :chr, :start and :end.
  :chr with common names come first, in order of (chr1, chr2, ..., chrX, chrY, chrM).
  Other chromosomes follow after in lexicographic order."
  [xs]
  (sort-by
   (fn [m]
     [(or (util/str->int (last (re-find #"(chr)?(\d+)" (:chr m)))) Integer/MAX_VALUE)
      (or ({"X" 23 "Y" 24 "M" 25} (last (re-find #"(chr)?([X|Y|M])" (:chr m)))) Integer/MAX_VALUE)
      (:chr m)
      (:start m)
      (:end m)])
   xs))

(defn merge-fields
  "Sort and merge overlapped regions.
  Currently, this function affects only :end and :name fields."
  [xs]
  (reduce
   (fn [r m]
     (let [l (last r)]
       (if (and l (= (:chr l) (:chr m)) (<= (:start m) (:end l)) (<= (:start l) (:end m)))
         (update r (dec (count r)) (fn [n m] (-> n (assoc :end (:end m)) (update-some :name str "+" (:name m)))) m)
         (conj r m))))
   []
   (sort-fields xs)))

(defn write-raw-fields
  "Write sequence of BED fields to writer without converting :start and :thick-start values."
  [^BufferedWriter wtr xs]
  (->> xs
       (map serialize-bed)
       (interpose "\n")
       ^String (apply str)
       (.write wtr)))

(defn write-fields
  "Write sequence of BED fields to writer."
  [^BufferedWriter wtr xs]
  (->> xs
       (map (comp serialize-bed denormalize))
       (interpose "\n")
       ^String (apply str)
       (.write wtr)))
