(ns cljam.pileup.pileup
  (:require [cljam.cigar :as cgr]
            [cljam.io :as io]
            [cljam.pileup.common :refer [window-width step center]]
            [cljam.bam.reader]))

(defn- count-for-alignment
  [^clojure.lang.PersistentHashMap aln
   ^String rname
   ^clojure.lang.LazySeq positions]
  (if (= rname (:rname aln))
    (let [^Long left (:pos aln)
          ^Long right (dec (+ left (cgr/count-ref (:cigar aln))))]
      (map (fn [p]
             (if (<= left p right) 1 0)) positions))
    (repeat (count positions) 0)))

(defn- count-for-positions
  "Returns a histogram value of the specified position."
  [^clojure.lang.LazySeq alns
   ^String rname positions]
  (if (pos? (count alns))
    (apply map + (map #(count-for-alignment % rname positions) alns))
    (repeat (count positions) 0)))

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
                                        :depth :shallow}))))

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
