(ns cljam.pileup.pileup
  "Provides simple pileup functions."
  (:require [cljam.util.sam-util :as sam-util]
            [cljam.io :as io]
            [cljam.pileup.common :refer [window-width step center]]
            cljam.bam.reader)
  (:import cljam.bam.reader.BAMReader))

(defn- update-pile
  "Updates the pile vector from the alignment, returning the updated pile
  vector. pile must be a vector."
  [aln rname pile positions]
  (if (= rname (:rname aln))
    (let [win-beg (first positions)
          win-end (last positions)
          left (max (:pos aln) win-beg)
          right (min (sam-util/get-end aln) win-end)]
      (loop [i left, pile* pile]
        (if (<= i right)
          (recur (inc i) (update-in pile* [(- i win-beg)] inc))
          pile*)))
    (vec (repeat (count pile) 0))))

(defn- count-for-positions
  "Piles the alignments up and counts them in the positions, returning it as a
  vector."
  [alns rname positions]
  (loop [[aln & rest] alns
         pile (vec (repeat (count positions) 0))]
    (if aln
      (recur rest (update-pile aln rname pile positions))
      pile)))

(defn rpositions
  "Returns a lazy seq of nums from start (inclusive) to end (inclusive)."
  ([start end]
     (rpositions start end start))
  ([start end n]
     (if (>= end n)
       (cons n (lazy-seq (rpositions start end (inc n)))))))

(defn- read-alignments
  "Reads alignments which have the rname and are included in a range defined by
  the pos and window size, returning the alignments as a lazy seq. Reading
  depth is shallow."
  [rdr rname rlength pos]
  (let [left (max (- pos window-width) 0)
        right (min rlength (+ pos window-width))]
    (io/read-alignments rdr {:chr rname
                             :start left
                             :end right
                             :depth :shallow})))

(defn- pileup*
  "Internal pileup function."
  [rdr rname rlength start end]
  (let [read-alignments-memo (memoize read-alignments)]
    (->> (rpositions start end)
         (partition-all step)
         (map (fn [positions]
                (let [pos (if (= (count positions) step)
                            (nth positions center)
                            (nth positions (quot (count positions) 2)))
                      alns (read-alignments-memo rdr rname rlength pos)]
                  (count-for-positions alns rname positions))))
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
  ([bam-reader rname start end]
     (try
       (if-let [r (sam-util/ref-by-name (io/read-refs bam-reader) rname)]
         (pileup* bam-reader
                  rname (:len r)
                  (if (neg? start) 0 start)
                  (if (neg? end) (:len r) end)))
       (catch bgzf4j.BGZFException _
         (throw (RuntimeException. "Invalid file format"))))))
