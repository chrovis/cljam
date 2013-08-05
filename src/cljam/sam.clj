(ns cljam.sam
  (:require [clojure.string :as str :refer [split join trim upper-case]]
            [cljam.util :refer [ra-line-seq]])
  (:import java.io.RandomAccessFile))

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

;;; Reference functions

(defn make-refs [sam]
  "Return a reference sequence from the sam."
  (for [h (filter :SQ (:header sam))]
    {:name (:SN (:SQ h)), :len (:LN (:SQ h))}))

(defn ref-id [refs name]
  "Returns reference ID from the reference sequence and the specified reference
  name. If not found, return nil."
  (some #(when (= name (:name (second %))) (first %))
        (map-indexed vector refs)))

(defn ref-name [refs id]
  "Returns a reference name from the reference ID. Returns nil if id is not
  mapped."
  (if (<= 0 id (dec (count refs)))
    (:name (nth refs id))))

;;; Utilities

(defn hd-header [sam]
  (some #(when-not (nil? (:HD %)) %) (:header sam)))

;;; I/O

(deftype ^:private SamReader [header reader]
  java.io.Closeable
  (close [this] (.. this reader close)))

(defn- read-header* [rdr]
  (when-let [line (.readLine rdr)]
    (if (= (first line) \@)
      (cons (parse-header line) (read-header* rdr)))))

(defn reader [f]
  (let [header (with-open [r (clojure.java.io/reader f)]
                 (read-header* r))]
    (->SamReader header (clojure.java.io/reader f))))

(defn read-header
  [^SamReader rdr]
  (.header rdr))

(defn read-alignments
  [^SamReader rdr]
  (map parse-alignment (filter #(not= (first %) \@) (ra-line-seq (.reader rdr)))))
