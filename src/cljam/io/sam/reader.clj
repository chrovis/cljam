(ns cljam.io.sam.reader
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.sam.util :as sam-util]
            [cljam.io :as io])
  (:import [java.io BufferedReader Closeable]))

(declare read-alignments* read-blocks* read-alignments-in-region*)

;;; reader

(deftype SAMReader [f header reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  io/IReader
  (reader-path [this]
    (.f this))
  (read [this]
    (io/read this {}))
  (read [this option]
    (read-alignments* this))
  io/IRegionReader
  (read-in-region [this region]
    (io/read-in-region this region {}))
  (read-in-region [this region option]
    (read-alignments-in-region* this region option))
  io/IAlignmentReader
  (read-header [this]
    (.header this))
  (read-refs [this]
    (vec (sam-util/make-refs (.header this))))
  (read-alignments [this]
    (io/read-alignments this {} {}))
  (read-alignments [this region]
    (io/read-alignments this region {}))
  (read-alignments [this {:keys [chr start end] :as region} option]
    (if (or chr start end)
      (read-alignments-in-region* this region option)
      (read-alignments* this)))
  (read-blocks [this]
    (io/read-blocks this {}))
  (read-blocks [this region]
    (io/read-blocks this region {}))
  (read-blocks [this region option]
    (read-blocks* this option)))

(defn- read-alignments*
  [^SAMReader sam-reader]
  (when-let [line (.readLine ^BufferedReader (.reader sam-reader))]
    (if-not (= (first line) \@)
      (cons (sam-util/parse-alignment line) (lazy-seq (read-alignments* sam-reader)))
      (lazy-seq (read-alignments* sam-reader)))))

(defn- read-alignments-in-region*
  [^SAMReader sam-reader {:keys [chr start end]} option]
  (logging/warn "May cause degradation of performance.")
  (filter
   (fn [a] (and (if chr (= (:rname a) chr) true)
                (if start (<= start (sam-util/get-end a)) true)
                (if end (<= (:pos a) end) true)))
   (read-alignments* sam-reader)))

(defn- parse-coordinate
  [^String line]
  (let [t0 (.indexOf line (int \tab) 0)
        t1 (.indexOf line (int \tab) (unchecked-inc t0))
        t2 (.indexOf line (int \tab) (unchecked-inc t1))
        t3 (.indexOf line (int \tab) (unchecked-inc t2))
        flag (Integer/parseInt (.substring line (unchecked-inc t0) t1))
        rname (.substring line (unchecked-inc t1) t2)
        pos (Integer/parseInt (.substring line (unchecked-inc t2) t3))]
    {:data line
     :rname rname
     :pos pos
     :flag flag}))

(defn- parse-qname
  [^String line]
  (let [t0 (.indexOf line (int \tab) 0)
        t1 (.indexOf line (int \tab) (unchecked-inc t0))
        qname (.substring line 0 t0)
        flag (Integer/parseInt (.substring line (unchecked-inc t0) t1))]
    {:data line
     :qname qname
     :flag flag}))

(defn- read-blocks**
  [parse-fn ^BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (if-not (= (first line) \@)
      (cons (parse-fn line) (lazy-seq (read-blocks** parse-fn rdr)))
      (lazy-seq (read-blocks** parse-fn rdr)))))

(defn- read-blocks*
  [^SAMReader sam-reader {:keys [mode] :or {mode :normal}}]
  (let [parse-fn (case mode
                   :normal (fn [line] {:data line})
                   :coordinate parse-coordinate
                   :queryname parse-qname)]
    (read-blocks** parse-fn (.reader sam-reader))))

(defn- read-header* [^BufferedReader rdr]
  (->> (line-seq rdr)
       (transduce
        (comp
         (take-while (fn [line] (= (first line) \@)))
         (map sam-util/parse-header-line))
        sam-util/into-header)))

(defn reader [f]
  (let [header (with-open [r (cio/reader f)]
                 (read-header* r))]
    (->SAMReader (.getAbsolutePath (cio/file f))
                 header (cio/reader f))))
