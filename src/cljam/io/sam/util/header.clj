(ns cljam.io.sam.util.header
  "Utility functions for SAM header."
  (:refer-clojure :exclude [sorted?])
  (:require [clojure.string :as cstr]
            [cljam.io.sam.common :as sam-common])
  (:import clojure.lang.IEditableCollection))

;;; parse

(defn- parse-header-keyvalues
  "e.g. \"LN:45 SN:ref\" -> {:LN 45, :SN \"ref\"}"
  [keyvalues]
  (into
   {}
   (map (fn [kv]
          (let [[k v] (cstr/split kv #":" 2)]
            [(keyword k)
             (case k
               "LN" (Integer/parseInt v)
               "PI" (Integer/parseInt v)
               v)])))
   keyvalues))

(defn parse-header-line
  "e.g. \"@SQ	SN:ref	LN:45\" => [:SQ {:SN \"ref\" :LN 45}]"
  [line]
  (let [[typ & kvs] (cstr/split line #"\t")]
    [(keyword (subs typ 1)) (parse-header-keyvalues kvs)]))

(defn- finalize-rf
  "Wrap a reducing function with finalizing function."
  [rf f]
  (fn finalize-rf-inner
    ([] (rf))
    ([x] (f (rf x)))
    ([x y] (rf x y))))

(defn- into-rf
  "Returns a reducing function which returns a new coll with elements conjoined."
  [to]
  (if (instance? IEditableCollection to)
    (fn into-rf-editable
      ([] (transient to))
      ([x] (with-meta (persistent! x) (meta to)))
      ([r x] (conj! r x)))
    (fn into-rf-non-editable
      ([] to)
      ([x] x)
      ([r x] (conj r x)))))

(defn- group-by-rf
  "Returns a reducing function acts like `group-by`.
  Second argument `rf` is a reducing function applied to each group. Default is (into-rf [])."
  ([keyfn] (group-by-rf keyfn (into-rf [])))
  ([keyfn rf]
   (fn group-by-rf-inner
     ([] (transient {}))
     ([x] (into {} (map (fn group-by-rf-finalize [[k v]] [k (rf v)])) (persistent! x)))
     ([x y] (let [k (keyfn y)]
              (if-let [old (get x k)]
                (assoc! x k (rf old y))
                (assoc! x k (rf (rf) y))))))))

(def into-header
  "A reducing function which builds a map from parsed headers."
  (-> (group-by-rf first ((map second) (into-rf [])))
      (finalize-rf (fn [m] (if (some? (:HD m)) (update m :HD first) m)))))

(defn parse-header
  "Parse a header string, returning a map of the header."
  [s]
  (->> (cstr/split-lines s)
       (transduce
        (map parse-header-line)
        into-header)))

;;; stringify

(defn- stringify-header-keyvalues [kv-map]
  (cstr/join \tab
             (map (fn [kv]
                    (let [[k v] (seq kv)]
                      (str (name k) \: v)))
                  kv-map)))

(defn stringify-header
  "Converts parsed header lines to a string."
  [hdr]
  (cstr/join \newline
             (map (fn [h]
                    (let [[typ kvs] h]
                      (if (= typ :HD)
                        (str "@HD" \tab (stringify-header-keyvalues kvs))
                        (cstr/join \newline
                                   (map #(str \@ (name typ) \tab (stringify-header-keyvalues %)) kvs)))))
                  (seq hdr))))

;;; @HD

(defn update-version
  "Overwrites format version in SAM header."
  [header]
  (assoc-in header [:HD :VN] sam-common/sam-version))

(def ^:const order-unknown
  "Unknown sorting order of alignments."
  :unknown)
(def ^:const order-unsorted
  "Unsorted of alignments."
  :unsorted)
(def ^:const order-coordinate
  "Coordinate sorting order of alignments."
  :coordinate)
(def ^:const order-queryname
  "queryname sorting order of alignments."
  :queryname)

(defn sorted-by
  "Replaces the sorting order field in SAM header."
  [order header]
  (assoc-in header [:HD :SO] (name order)))

(defn sort-order
  "Returns sorting order of the sam as Keyword. Returning order is one of the
  following: :queryname, :coordinate, :unsorted, :unknown."
  [header]
  (or (keyword (:SO (:HD header))) order-unknown))

(defn sorted?
  "Returns true if the sam is sorted, false if not. It is detected by
  `@HD SO:***` tag in the header."
  [header]
  (let [so (:SO (:HD header))]
    (or (= so (name order-queryname))
        (= so (name order-coordinate)))))
