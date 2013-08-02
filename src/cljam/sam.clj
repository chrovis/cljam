(ns cljam.sam
  (:require [clojure.string :as str :refer [split join trim upper-case]]))

;;; Define records

(defrecord SamHeader [])

(defrecord SamAlignment
    [qname flag rname pos mapq cigar rnext pnext tlen seq qual options])

(defrecord Sam
    [header alignments])

;;; Parse

(defn- parse-header-keyvalues [keyvalues]
  (apply merge (map (fn [kv]
                      (let [[k v] (split kv #":")]
                        {(keyword k) v}))
                    keyvalues)))

(defn parse-header [line]
  "Parse a line, returning a SamHeader record."
  (let [[type & keyvalues] (split line #"\t")]
    (assoc (SamHeader.)
      (keyword (subs type 1))
      (parse-header-keyvalues keyvalues))))

(defn- parse-optional-fields [options]
  (map (fn [op]
         (let [[tag type value] (split op #":")]
           {(keyword tag) {:type type :value value}}))
       options))

(defn- parse-seq-text [s]
  (upper-case s))

(defn parse-alignment [line]
  "Parse a line, returning a SamAlignment record."
  (let [fields (split line #"\t")]
    (SamAlignment. (first fields)
                   (Integer/parseInt (nth fields 1))
                   (nth fields 2)
                   (Integer/parseInt (nth fields 3))
                   (Integer/parseInt (nth fields 4))
                   (nth fields 5)
                   (nth fields 6)
                   (Integer/parseInt (nth fields 7))
                   (Integer/parseInt (nth fields 8))
                   (parse-seq-text (nth fields 9))
                   (nth fields 10)
                   (vec (parse-optional-fields (drop 11 fields))))))

;;; Stringify

(defn- stringify-header-keyvalues [kv-map]
  (join \tab
        (map (fn [kv]
               (let [[k v] (seq kv)]
                 (str (name k) \: v)))
             kv-map)))

(defn- stringify-optional-fields [options]
  (join \tab
        (map (fn [op]
               (let [[tag entity] (first (seq op))]
                 (str (name tag) \: (:type entity) \: (:value entity))))
             options)))

(defmulti stringify class)

(defmethod stringify SamHeader [sh]
  (let [[type keyvalues] (first (seq sh))]
    (str \@ (name type) \tab (stringify-header-keyvalues keyvalues))))

(defmethod stringify SamAlignment [sa]
  (trim
   (join \tab
         [(:qname sa)
          (:flag  sa)
          (:rname sa)
          (:pos   sa)
          (:mapq  sa)
          (:cigar sa)
          (:rnext sa)
          (:pnext sa)
          (:tlen  sa)
          (:seq   sa)
          (:qual  sa)
          (stringify-optional-fields (:options sa))])))

;;; Utilities

(defn hd-header [sam]
  (some #(when-not (nil? (:HD %)) %) (:header sam)))

(defn make-refs [sam]
  (for [h (filter :SQ (:header sam))]
    {:name (:SN (:SQ h)), :len (:LN (:SQ h))}))
