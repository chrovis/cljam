(ns cljam.sam
  (:refer-clojure :exclude [slurp spit])
  (:require [clojure.string :as str :refer [split join trim upper-case]])
  (:import [java.io BufferedReader BufferedWriter]))

(def version "SAM format version" "1.4")

;;; Parse

(defn- parse-header-keyvalues
  "e.g. \"LN:45 SN:ref\" -> {:LN 45, :SN \"ref\"}"
  [keyvalues]
  (apply merge
         (map (fn [kv]
                (let [[k v] (split kv #":")]
                  {(keyword k) (case k
                                 "LN" (Integer/parseInt v)
                                 "PI" (Integer/parseInt v)
                                 v)}))
              keyvalues)))

(defn- parse-header-line [line]
  (let [[typ & kvs] (split line #"\t")]
    {(keyword (subs typ 1)) (if (= typ "@HD")
                              (parse-header-keyvalues kvs)
                              (vector (parse-header-keyvalues kvs)))}))

(defn- parse-header* [col]
  (when (seq col)
    (merge-with #(vec (concat %1 %2)) (parse-header-line (first col)) (parse-header* (rest col)))))

(defn parse-header
  "Parse a header string, returning a map of the header."
  [s]
  (parse-header* (split s #"\n")))

(defn- parse-optional-fields [options]
  (map (fn [op]
         (let [[tag type value] (split op #":")]
           {(keyword tag) {:type type :value value}}))
       options))

(defn- parse-seq-text [s]
  (upper-case s))

(defn parse-alignment
  "Parse an alignment line, returning a map of the alignment."
  [line]
  (let [fields (split line #"\t")]
    {:qname   (first fields)
     :flag    (Integer/parseInt (nth fields 1))
     :rname   (nth fields 2)
     :pos     (Integer/parseInt (nth fields 3))
     :mapq    (Integer/parseInt (nth fields 4))
     :cigar   (nth fields 5)
     :rnext   (nth fields 6)
     :pnext   (Integer/parseInt (nth fields 7))
     :tlen    (Integer/parseInt (nth fields 8))
     :seq     (parse-seq-text (nth fields 9))
     :qual    (nth fields 10)
     :options (vec (parse-optional-fields (drop 11 fields)))}))

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

(defn stringify-header [hdr]
  (join \newline
        (map (fn [h]
               (let [[typ kvs] h]
                 (if (= typ :HD)
                   (str "@HD" \tab (stringify-header-keyvalues kvs))
                   (join \newline
                         (map #(str \@ (name typ) \tab (stringify-header-keyvalues %)) kvs)))))
             (seq hdr))))

(defn stringify-alignment [sa]
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

(defn make-refs [hdr]
  "Return a reference sequence from the sam."
  (for [sq (:SQ hdr)]
    {:name (:SN sq), :len (:LN sq)}))

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

;;; I/O

(deftype SamReader [header reader]
  java.io.Closeable
  (close [this] (.close ^BufferedReader (.reader this))))

(defn- read-header* [^BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (if (= (first line) \@)
      (merge-with #(vec (concat %1 %2)) (parse-header-line line) (read-header* rdr)))))

(defn reader [f]
  (let [header (with-open [r (clojure.java.io/reader f)]
                 (read-header* r))]
    (->SamReader header (clojure.java.io/reader f))))

(defn read-header
  [rdr]
  (.header rdr))

(defn read-alignments
  [rdr]
  (when-let [line (.readLine ^BufferedReader (.reader rdr))]
    (if-not (= (first line) \@)
      (cons (parse-alignment line) (lazy-seq (read-alignments rdr)))
      (lazy-seq (read-alignments rdr)))))

(defn writer [f]
  (clojure.java.io/writer f))

(defn write-header [^BufferedWriter wrtr hdr]
  (.write wrtr ^String (stringify-header hdr))
  (.newLine wrtr))

(defn write-alignments [^BufferedWriter wrtr alns]
  (doseq [a alns]
    (.write wrtr ^String (stringify-alignment a))
    (.newLine wrtr)))

(defn slurp
  "Opens a reader on sam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (with-open [r (reader f)]
    {:header (read-header r)
     :alignments (vec (read-alignments r))}))

(defn spit
  "Opposite of slurp-sam. Opens sam-file with writer, writes sam headers and
  alignments, then closes the sam-file."
  [f sam]
  (with-open [w (writer f)]
    (write-header w (:header sam))
    (write-alignments w (:alignments sam))))
