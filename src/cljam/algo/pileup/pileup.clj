(ns cljam.algo.pileup.pileup
  "Provides simple pileup functions."
  (:require [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads]]
            [cljam.util :as util]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util :as sam-util]
            [cljam.algo.pileup.common :as common]))

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

(defn- pileup*
  "Internal pileup function."
  [rdr rname rlength start end step]
  (let [n-threads (get-exec-n-threads)
        read-fn (fn [r start end]
                  (sam/read-alignments r {:chr rname :start start :end end :depth :shallow}))
        count-fn (fn [xs]
                   (if (= n-threads 1)
                     (map (fn [[start end]]
                            (count-for-positions (read-fn rdr start end) start end)) xs)
                     (cp/pmap (dec n-threads)
                              (fn [[start end]]
                                (with-open [r (sam/clone-bam-reader rdr)]
                                  (count-for-positions (read-fn r start end) start end))) xs)))]
    (->> (util/divide-region start end step)
         count-fn
         (apply concat))))

(defn first-pos
  "Return a position of first alignment in left-right, or nil."
  [bam-reader region]
  (-> bam-reader
      (sam/read-alignments (assoc region :depth :first-only))
      first
      :pos))

(defn pileup
  "Piles alignments up, returning the pileup as a lazy seq. Requires a
  `cljam.bam.reader.BAMReader` instance and region. If start and end are not
  supplied, piles whole range up."
  [bam-reader {:keys [chr start end] :or {start -1 end -1}} & {:keys [step] :or {step common/step}}]
  (try
    (if-let [r (sam-util/ref-by-name (sam/read-refs bam-reader) chr)]
      (pileup*
       bam-reader
       chr (:len r)
       (if (neg? start) 1 start)
       (if (neg? end) (:len r) end)
       step))
    (catch bgzf4j.BGZFException _
      (throw (RuntimeException. "Invalid file format")))))
