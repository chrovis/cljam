(ns cljam.bam
  (:refer-clojure :exclude [slurp spit])
  (:require [cljam.io :as io]
            (cljam.bam [reader :as bam-reader]
                       [writer :as bam-writer])))

(defn reader
  [f & option]
  (bam-reader/reader f option))

(defn writer
  [f]
  (bam-writer/writer f))

(defn ^:deprecated
  slurp
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f & options]
  (let [{:keys [chr start end] :or {chr nil
                                    start 0
                                    end -1}} options]
    (with-open [r (bam-reader/reader f {})]
      {:header (io/read-header r)
       :alignments (io/read-alignments r
                                       (if (nil? chr)
                                         {}
                                         {:chr chr :start start :end end}))})))

(defn ^:deprecated
  spit
  "Opposite of slurp-bam. Opens bam-file with writer, writes sam headers and
  alignments, then closes the bam-file."
  [f sam]
  (with-open [w (bam-writer/writer f)]
    (io/write-header w (:header sam))
    (io/write-refs w (:header sam))
    (io/write-alignments w (:alignments sam) (:header sam))))
