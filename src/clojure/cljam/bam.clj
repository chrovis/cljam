(ns cljam.bam
  (:refer-clojure :exclude [slurp spit])
  (:require [cljam.sam :as sam]
            [cljam.protocol :refer [read-header read-refs]]
            (cljam.bam [reader :as bam-reader]
                       [writer :as bam-writer])))

(defn reader
  [f]
  (bam-reader/reader f))

(defn writer
  [f]
  (bam-writer/writer f))

(defn slurp
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f & options]
  (let [{:keys [chr start end] :or {chr nil
                                    start 0
                                    end -1}} options]
    (with-open [r (bam-reader/reader f)]
      (let [h (read-header r)]
        {:header h
         :alignments (if (nil? chr)
                       nil
                       (vec (bam-reader/read-alignments r chr start end)))}))))

(defn spit
  "Opposite of slurp-bam. Opens bam-file with writer, writes sam headers and
  alignments, then closes the bam-file."
  [f sam]
  (with-open [w (bam-writer/writer f)]
    (let [refs (sam/make-refs (:header sam))]
      (bam-writer/write-header w (:header sam))
      (bam-writer/write-refs w refs)
      (bam-writer/write-alignments w (:alignments sam) refs))))
