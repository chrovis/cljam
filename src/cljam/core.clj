(ns cljam.core
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]))

(defn reader [f & {:keys [ignore-index] :or {ignore-index true}}]
  (condp re-find f
    #"\.sam$" (sam/reader f)
    #"\.bam$" (bam/reader f :ignore-index ignore-index)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn writer [f]
  (condp re-find f
    #"\.sam$" (sam/writer f)
    #"\.bam$" (bam/writer f)
    (throw (IllegalArgumentException. "Invalid file type"))))
