(ns cljam.io.sam.reader
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sam.util.header :as header]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util])
  (:import [java.io BufferedReader Closeable]
           [cljam.io.protocols SAMCoordinateBlock SAMQuerynameBlock]))

(declare read-alignments* read-blocks* read-alignments-in-region*)

;;; reader

(deftype SAMReader [url header reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this]
    (.url this))
  (read [this]
    (protocols/read this {}))
  (read [this region]
    (protocols/read-alignments this region))
  (indexed? [_] false)
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this region option]
    (read-alignments-in-region* this region option))
  protocols/IAlignmentReader
  (read-header [this]
    (.header this))
  (read-refs [this]
    (vec (refs/make-refs (.header this))))
  (read-alignments [this]
    (protocols/read-alignments this {}))
  (read-alignments [this {:keys [chr start end] :as region}]
    (if (or chr start end)
      (read-alignments-in-region* this region)
      (read-alignments* this)))
  (read-blocks [this]
    (protocols/read-blocks this {}))
  (read-blocks [this region]
    (protocols/read-blocks this region {}))
  (read-blocks [this region option]
    (read-blocks* this option)))

(defn- read-alignments*
  [^SAMReader sam-reader]
  (eduction
   (comp
    (drop-while (fn [[f]] (= f \@)))
    (map sam-util/parse-alignment))
   (line-seq (.reader sam-reader))))

(defn- read-alignments-in-region*
  [^SAMReader sam-reader {:keys [chr start end]}]
  (logging/warn "May cause degradation of performance.")
  (eduction
   (filter
    (fn [a] (and (if chr (= (:rname a) chr) true)
                 (if start (<= start (sam-util/get-end a)) true)
                 (if end (<= (:pos a) end) true))))
   (read-alignments* sam-reader)))

(defn- parse-coordinate
  [rname->ref-id ^String line]
  (let [t0 (.indexOf line (int \tab) 0)
        t1 (.indexOf line (int \tab) (unchecked-inc t0))
        t2 (.indexOf line (int \tab) (unchecked-inc t1))
        t3 (.indexOf line (int \tab) (unchecked-inc t2))
        flag (Integer/parseInt (.substring line (unchecked-inc t0) t1))
        rname (.substring line (unchecked-inc t1) t2)
        pos (Integer/parseInt (.substring line (unchecked-inc t2) t3))]
    (SAMCoordinateBlock. line (rname->ref-id rname 0) pos flag)))

(defn- parse-qname
  [^String line]
  (let [t0 (.indexOf line (int \tab) 0)
        t1 (.indexOf line (int \tab) (unchecked-inc t0))
        qname (.substring line 0 t0)
        flag (Integer/parseInt (.substring line (unchecked-inc t0) t1))]
    (SAMQuerynameBlock. line qname flag)))

(defn- read-blocks*
  [^SAMReader sam-reader {:keys [mode] :or {mode :normal}}]
  (let [parse-fn (if (fn? mode)
                   mode
                   (case mode
                     :normal (fn [line] {:data line})
                     :coordinate (->> (.header sam-reader)
                                      :SQ
                                      (into {"*" -1} (map-indexed (fn [i {:keys [SN]}] [SN i])))
                                      (partial parse-coordinate))
                     :queryname parse-qname))]
    (eduction
     (comp
      (drop-while (fn [[f]] (= f \@)))
      (map parse-fn))
     (line-seq (.reader sam-reader)))))

(defn- read-header* [^BufferedReader rdr]
  (->> (line-seq rdr)
       (transduce
        (comp
         (take-while (fn [line] (= (first line) \@)))
         (map header/parse-header-line))
        header/into-header)))

(defn reader [f]
  (let [header (with-open [r (cio/reader f)]
                 (read-header* r))]
    (->SAMReader (util/as-url f)
                 header (cio/reader f))))
