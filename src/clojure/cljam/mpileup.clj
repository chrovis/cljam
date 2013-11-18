(ns cljam.mpileup
  (:require [clojure.string :as str]
            [cljam.cigar :as cgr]
            [cljam.io :as io]
            [cljam.util :refer [ubyte]]
            [cljam.util.sam-util :refer [phred->fastq fastq->phred]]))

;; TODO: estiamte from actual data
(def ^:private window-width 1250)
(def ^:private step 2000)
(def ^:private center (int (/ step 2)))

(defn- wrap
  [rname positions counts seq qual]
  (map (fn [{:keys [pos count qual seq]}]
         {:rname rname
          :pos pos
          :ref \N ; FIXME
          :count count
          :seq seq
          :qual qual})
       (map merge
            (map (partial hash-map :pos) positions)
            (map (partial hash-map :count) counts)
            (map (partial hash-map :qual) qual)
            (map (partial hash-map :seq) seq))))

(defn- pickup-qual [aln pos]
  (if (= (:qual aln) "*")
    \~
    (nth (:qual aln) pos)))

(comment

 (with-open [r (cljam.bam/reader "test/resources/test.sorted.bam")]
   (mpileup r "ref"))

 )

(defn- count-for-alignment
  [^clojure.lang.PersistentHashMap aln
   ^String rname
   ^clojure.lang.LazySeq positions]
  (if (= rname (:rname aln))
    (let [^Long left (:pos aln)
          ^Long right (dec (+ left (cgr/count-ref (:cigar aln))))
          subs-seq (cgr/substantial-seq (:cigar aln) (:seq aln))]
      (map (fn [p]
             (if (and (>= p left) (<= p right))
               {:count 1
                :seq (nth subs-seq (- p left))
                :qual (pickup-qual aln (- p left))
                :head (= p left)
                :tail (= p right)}
               {:count 0, :seq nil, :qual nil}))
           positions))
    (take (count positions) (repeat {:count 0, :seq nil, :qual nil}))))

(defn- count-for-positions
  [alns rname positions]
  (if (pos? (count alns))
    (let [cfas (map #(count-for-alignment % rname positions) alns)]
      (wrap rname positions
            (apply map (fn [& args]
                         (reduce + (map :count args))) cfas)
            (apply map (fn [& args]
                         (str/join
                          (map (fn [a]
                                 (str (if (:head a) (str "^" (:qual a)))
                                      (:seq a)
                                      (if (:tail a) "$")))
                               args)))
                   cfas)
            (apply map (fn [& args]
                         (str/join
                          (map :qual args)))
                   cfas)))
    (wrap rname positions
          (take (count positions) (repeat 0))
          nil nil)))

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
                             :depth :deep})))

(defn- search-ref
  [refs rname]
  (first
   (filter (fn [r] (= (:name r) rname))
           refs)))

(defn- mpileup*
  ([rdr ^String rname ^Long rlength ^Long start ^Long end]
     (flatten
      (let [parts (partition-all step (rpositions start end))]
        (map (fn [positions]
               (let [^Long pos (if (= (count positions) step)
                                 (nth positions center)
                                 (nth positions (quot (count positions) 2)))
                     ^clojure.lang.LazySeq alns (read-alignments rdr rname rlength pos)]
                 (count-for-positions alns rname positions)))
             parts)))))

(defn mpileup
  ([rdr ^String rname]
     (mpileup rdr rname -1 -1))
  ([rdr ^String rname ^Long start* ^Long end*]
     (let [r (search-ref (.refs rdr) rname)]
       (if (nil? r)
         nil
         (mpileup* rdr
                  rname (:len r)
                  (if (neg? start*) 0 start*)
                  (if (neg? end*) (:len r) end*))))))
