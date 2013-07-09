(ns cljam.io
  (:require [clojure.string :as str]
            [clojure.java.io :only (file reader) :as io])
  (:import (java.io DataOutputStream FileOutputStream)
           (java.util.zip Deflater)))

(defn- parse-header-keyvalues [keyvalues]
  (map (fn [kv]
         (let [[k v] (str/split kv #":")]
           {(keyword k) v}))
       keyvalues))

(defn- parse-header-line [line]
  (let [[type & keyvalues] (str/split line #"\t")]
    {(keyword (subs type 1))
     (vec (parse-header-keyvalues keyvalues))}))

(defn- parse-optional-fields [options]
  (map (fn [op]
         (let [[tag type value] (str/split op #":")]
           {(keyword tag) {:type type :value value}}))
       options))

(defn- parse-line [line]
  (let [fields (str/split line #"\t")]
    {:QNAME   (first fields)
     :FLAG    (Integer/parseInt (nth fields 1))
     :RNAME   (nth fields 2)
     :POS     (Integer/parseInt (nth fields 3))
     :MAPQ    (Integer/parseInt (nth fields 4))
     :CIGAR   (nth fields 5)
     :RNEXT   (nth fields 6)
     :PNEXT   (Integer/parseInt (nth fields 7))
     :TLEN    (Integer/parseInt (nth fields 8))
     :SEQ     (nth fields 9)
     :QUAL    (nth fields 10)
     :options (vec (parse-optional-fields (drop 11 fields)))}))

(defn- stringify-header-keyvalues [kv-vec]
  (->> (map (fn [kv]
              (let [[k v] (first (seq kv))]
                (str (name k) \: v)))
            kv-vec)
       (str/join \tab)))

(defn- stringify-header-map [amap]
  (let [[type keyvalues] (first (seq amap))]
    (str \@ (name type) \tab (stringify-header-keyvalues keyvalues))))

(defn- stringify-optional-fields [options]
  (->> (map (fn [op]
              (let [[tag entity] (first (seq op))]
                (str (name tag) \: (:type entity) \: (:value entity))))
            options)
       (str/join \tab)))

(defn- stringify-alignment-map [amap]
  (-> (str/join \tab [(:QNAME amap)
                      (:FLAG  amap)
                      (:RNAME amap)
                      (:POS   amap)
                      (:MAPQ  amap)
                      (:CIGAR amap)
                      (:RNEXT amap)
                      (:PNEXT amap)
                      (:TLEN  amap)
                      (:SEQ   amap)
                      (:QUAL  amap)
                      (stringify-optional-fields (:options amap))])
      str/trim))

(defn slurp-sam
  "Opens a reader on sam-file and reads all its headers and alignments, returning a map about sam records."
  [sam-file]
  (with-open [r (io/reader sam-file)]
    (loop [sam {:header [], :alignments []}, line (.readLine r)]
      (if (nil? line)
        sam
        (recur
         (if (= (first line) \@)
           (assoc sam :header (conj (:header sam) (parse-header-line line)))
           (assoc sam :alignments (conj (:alignments sam) (parse-line line))))
         (.readLine r))))))

(defn spit-sam
  "Opposite of slurp-sam. Opens sam-file with writer, writes sam headers and alignments, then closes sam-file."
  [sam-file sam]
  (with-open [w (io/writer sam-file)]
    (doseq [header-map (:header sam)]
      (.write w (stringify-header-map header-map))
      (.newLine w))
    (doseq [alignment-map (:alignments sam)]
      (.write w (stringify-alignment-map alignment-map))
      (.newLine w))
    nil))

(defn deflate [byte-data]
  (let [ret (bytes nil)
        defl (Deflater. 5 true)]
    (.setInput defl byte-data)
    (.finish defl)
    (.deflate defl ret)
    ret))

(defn spit-bam
  [bam-file sam]
  (with-open [w (DataOutputStream. (FileOutputStream. bam-file))]
    (.write w (.getBytes "BAM\1"))
    (doseq [header-map (:header sam)]
      (.write w (.getBytes (stringify-header-map header-map))))
    (doseq [alignment-map (:alignments sam)]
      (.write w (.getBytes (stringify-alignment-map alignment-map))))
    nil))
