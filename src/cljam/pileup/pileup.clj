(ns cljam.pileup.pileup
  "Provides simple pileup functions."
  (:require [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads]]
            [cljam.util.sam-util :as sam-util]
            [cljam.io :as io]
            [cljam.pileup.common :refer [window-width step center]]
            cljam.bam.reader)
  (:import cljam.bam.reader.BAMReader))

(defn- update-pile
  "Updates the pile vector from the alignment, returning the updated pile
  vector. positions and pile must be vectors."
  [aln rname pile positions]
  (if (= rname (:rname aln))
    (let [win-beg (first positions)
          win-end (peek positions)
          left (max (:pos aln) win-beg)
          right (min (sam-util/get-end aln) win-end)]
      (loop [i left, pile* pile]
        (if (<= i right)
          (recur (inc i) (update pile* (- i win-beg) inc))
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

(defn grouped-rpositions
  "Returns a lazy seq of grouped positions. Equivalent of
  (partition-all n (rpositions start end)). This implementation is faster."
  [start end n]
  (lazy-seq
   (letfn [(rpositions* [s e]
             (loop [i s xs (transient [])]
               (if (<= i e)
                 (recur (inc i) (conj! xs i))
                 (persistent! xs))))]
     (when (<= start end)
       (cons (rpositions* start (min (+ start n -1) end))
             (grouped-rpositions (+ start n) end n))))))

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
  (let [n-threads (get-exec-n-threads)
        count-fn (fn [xs]
                   (if (= n-threads 1)
                     (map (fn [[positions alns]]
                            (count-for-positions alns rname positions)) xs)
                     (cp/pmap (dec n-threads)
                              (fn [[positions alns]]
                                (count-for-positions alns rname positions)) xs)))]
    (->> (grouped-rpositions start end step)
         (map vec)
         (map (fn [positions]
                (let [pos (if (= (count positions) step)
                            (nth positions center)
                            (nth positions (quot (count positions) 2)))
                      alns (doall (read-alignments rdr rname rlength pos))]
                  [positions alns])))
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
  ([bam-reader rname start end]
     (try
       (if-let [r (sam-util/ref-by-name (io/read-refs bam-reader) rname)]
         (pileup* bam-reader
                  rname (:len r)
                  (if (neg? start) 1 start)
                  (if (neg? end) (:len r) end)))
       (catch bgzf4j.BGZFException _
         (throw (RuntimeException. "Invalid file format"))))))
