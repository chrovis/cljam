(ns cljam.pileup.pileup
  (:require [cljam.util.sam-util :as sam-util]
            [cljam.io :as io]
            [cljam.pileup.common :refer [window-width step center]]
            [cljam.bam.reader]))

(defn- update-pile
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
  [alns rname positions]
  (loop [[aln & rest] alns
         pile (vec (repeat (count positions) 0))]
    (if aln
      (recur rest (update-pile aln rname pile positions))
      pile)))

(defn rpositions
  ([^Long start ^Long end]
     (rpositions start end start))
  ([^Long start ^Long end ^Long n]
     (if (>= end n)
       (cons n
             (lazy-seq (rpositions start end (inc n))))
       nil)))

(defn- read-alignments
  [rdr ^String rname ^Long rlength ^Long pos]
  (let [^Long left (let [^Long val (- pos window-width)]
                     (if (< val 0)
                       0
                       val))
        ^Long right (let [^Long val (+ pos window-width)]
                      (if (< rlength val)
                        rlength
                        val))]
    (io/read-alignments rdr {:chr rname
                             :start left
                             :end right
                             :depth :shallow})))

(defn- search-ref
  [refs rname]
  (first
   (filter (fn [r] (= (:name r) rname))
           refs)))

(defn- pileup*
  [^cljam.bam.reader.BAMReader rdr ^String rname ^Long rlength ^Long start ^Long end]
  (let [read-alignments-memo (memoize read-alignments)]
    (flatten
     (let [parts (partition-all step (rpositions start end))]
       (map (fn [positions]
              ;; (println (first positions))
              (let [^Long pos (if (= (count positions) step)
                                (nth positions center)
                                (nth positions (quot (count positions) 2)))
                    ^clojure.lang.LazySeq alns (read-alignments-memo rdr rname rlength pos)]
                (count-for-positions alns rname positions)))
            parts)))))

(defn first-pos
  "Return a position of first alignment in left-right, or nil."
  [rdr ^String rname ^Long left ^Long right]
  (:pos (first (io/read-alignments rdr {:chr rname
                                        :start left
                                        :end right
                                        :depth :first-only}))))

(defn pileup
  ([^cljam.bam.reader.BAMReader rdr ^String rname]
     (pileup rdr rname -1 -1))
  ([^cljam.bam.reader.BAMReader rdr ^String rname ^Long start* ^Long end*]
     (try
       (let [r (search-ref (.refs rdr) rname)]
         (if (nil? r)
           nil
           (pileup* rdr
                    rname (:len r)
                    (if (neg? start*) 0 start*)
                    (if (neg? end*) (:len r) end*))))
       (catch bgzf4j.BGZFException e (throw (RuntimeException. "Invalid file format"))))))
