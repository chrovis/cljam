(ns cljam.pileup.pileup
  "Provides simple pileup functions."
  (:require [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads]]
            [cljam.bam :as bam]
            [cljam.util.sam-util :as sam-util]
            [cljam.io :as io]
            [cljam.pileup.common :as common]))

(defn- count-for-positions
  "Piles the alignments up and counts them in the positions, returning it as a
  seq."
  [alns beg end]
  (let [pile (long-array (inc (- end beg)))]
    (doseq [aln alns]
      (let [left (max (:pos aln) beg)
            right (min (sam-util/get-end aln) end)
            left-index (- left beg)]
        (dotimes [i (inc (- right left))]
          (aset-long pile (+ i left-index) (inc (aget pile (+ i left-index)))))))
    (seq pile)))

(defn- regions
  [start end step]
  (->> [(inc end)]
       (concat (range start (inc end) step))
       (partition 2 1)
       (map (fn [[s e]] [s (dec e)]))))

(defn- pileup*
  "Internal pileup function."
  [rdr rname rlength start end step]
  (let [n-threads (get-exec-n-threads)
        read-fn (fn [r start end]
                  (io/read-alignments r {:chr rname :start start :end end :deep :shallow}))
        count-fn (fn [xs]
                   (if (= n-threads 1)
                     (map (fn [[start end]]
                            (with-open [r (bam/clone-reader rdr)]
                              (count-for-positions (read-fn r start end) start end))) xs)
                     (cp/pmap (dec n-threads)
                              (fn [[start end]]
                                (with-open [r (bam/clone-reader rdr)]
                                  (count-for-positions (read-fn r start end) start end))) xs)))]
    (->> (regions start end step)
         count-fn
         (apply concat))))

(defn first-pos
  "Return a position of first alignment in left-right, or nil."
  [bam-reader rname left right]
  (-> (io/read-alignments bam-reader {:chr rname
                                      :start left
                                      :end right
                                      :depth :first-only})
      first
      :pos))

(defn pileup
  "Piles alignments up, returning the pileup as a lazy seq. Requires a
  `cljam.bam.reader.BAMReader` instance and rname. If start and end are not
  supplied, piles whole range up."
  ([bam-reader rname]
     (pileup bam-reader rname -1 -1))
  ([bam-reader rname start end & {:keys [step] :or {step common/step}}]
     (try
       (if-let [r (sam-util/ref-by-name (io/read-refs bam-reader) rname)]
         (pileup*
          bam-reader
          rname (:len r)
          (if (neg? start) 1 start)
          (if (neg? end) (:len r) end)
          step))
       (catch bgzf4j.BGZFException _
         (throw (RuntimeException. "Invalid file format"))))))
