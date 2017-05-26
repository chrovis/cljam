(ns cljam.sam.reader
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.util.sam-util :as sam-util]
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
    (io/read-alignments this {} {}))
  (read-alignments [this {:keys [chr start end] :as region} option]
    (if (or chr start end)
      (read-alignments-in-region* this region option)
      (read-alignments* this)))
  (read-blocks [this]
    (io/read-blocks this {}))
  (read-blocks [this option]
    (read-blocks* this)))

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

(defn- read-blocks*
  [^SAMReader sam-reader]
  (when-let [line (.readLine ^BufferedReader (.reader sam-reader))]
    (if-not (= (first line) \@)
      (cons {:line line} (lazy-seq (read-blocks* sam-reader)))
      (lazy-seq (read-blocks* sam-reader)))))

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
