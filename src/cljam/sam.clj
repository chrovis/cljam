(ns cljam.sam
  (:require [clojure.string :as str]))

(defrecord SamHeader [])

(defrecord SamAlignment
    [qname flag rname pos mapq cigar rnext pnext tlen seq qual options])

(defrecord Sam
    [header alignments])

;;; parse

(defn- parse-header-keyvalues [keyvalues]
  (map (fn [kv]
         (let [[k v] (str/split kv #":")]
           {(keyword k) v}))
       keyvalues))

(defn parse-header [line]
  "Parse a line, returning a SamHeader record."
  (let [[type & keyvalues] (str/split line #"\t")]
    (assoc (SamHeader.)
      (keyword (subs type 1))
      (vec (parse-header-keyvalues keyvalues)))))

(defn- parse-optional-fields [options]
  (map (fn [op]
         (let [[tag type value] (str/split op #":")]
           {(keyword tag) {:type type :value value}}))
       options))

(defn parse-alignment [line]
  "Parse a line, returning a SamAlignment record."
  (let [fields (str/split line #"\t")]
    (SamAlignment. (first fields)
                   (Integer/parseInt (nth fields 1))
                   (nth fields 2)
                   (Integer/parseInt (nth fields 3))
                   (Integer/parseInt (nth fields 4))
                   (nth fields 5)
                   (nth fields 6)
                   (Integer/parseInt (nth fields 7))
                   (Integer/parseInt (nth fields 8))
                   (nth fields 9)
                   (nth fields 10)
                   (vec (parse-optional-fields (drop 11 fields))))))

;;; stringify

(defn- stringify-header-keyvalues [kv-vec]
  (->> (map (fn [kv]
              (let [[k v] (first (seq kv))]
                (str (name k) \: v)))
            kv-vec)
       (str/join \tab)))

(defn- stringify-optional-fields [options]
  (->> (map (fn [op]
              (let [[tag entity] (first (seq op))]
                (str (name tag) \: (:type entity) \: (:value entity))))
            options)
       (str/join \tab)))

(defmulti stringify class)

(defmethod stringify SamHeader
  [sh]
  (let [[type keyvalues] (first (seq sh))]
    (str \@ (name type) \tab (stringify-header-keyvalues keyvalues))))

(defmethod stringify SamAlignment
  [sa]
  (-> (str/join \tab [(:qname sa)
                      (:flag  sa)
                      (:rname sa)
                      (:pos   sa)
                      (:mapq  sa)
                      (:cigar sa)
                      (:rnext sa)
                      (:pnext sa)
                      (:tlen  sa)
                      (:seq   sa)
                      (:suql  sa)
                      (stringify-optional-fields (:options sa))])
      str/trim))
