(ns cljam.pileup
  (:require (cljam [cigar :as cgr]
                   [bam :as bam])))

(def ^:private window-width 500) ;; TODO: estiamte from actual data
(def ^:private step 100) ;; TODO: estiamte from actual data
(def ^:private center 50) ;; TODO: estiamte from actual data

(defn- count-for-pos
  "Returns a histogram value of the specified position."
  [alns rname pos]
  (loop [alns2 alns
         val 0]
    (let [[aln & rst] alns2]
      (if (nil? aln)
        val
        (if (and (= rname (:rname aln))
                 (>= pos (:pos aln))
                 (<= pos (+ (:pos aln) (cgr/count-ref (:cigar aln)))))
          (recur rst (inc val))
          (recur rst val))))))

(defn- rpositions
  ([len] (rpositions 0 len))
  ([n len] (if (>= len n)
             (cons n
                   (lazy-seq (rpositions (inc n) len)))
             nil)))

(defn- read-alignments
  [rdr rname rlength pos]
  (let [left (let [val (- pos window-width)]
               (if (< val 0)
                 0
                 val))
        right (let [val (+ pos window-width)]
               (if (< rlength val)
                 rlength
                 val))]
    (bam/read-alignments rdr rname left right)))

(defn- search-ref
  [refs rname]
  (first
   (filter (fn [r] (= (:name r) rname))
           refs)))

(defn- pileup*
  ([rdr rname rlength]
     (flatten
      (map (fn [positions]
             (let [pos (nth positions center)
                   alns (read-alignments rdr rname rlength pos)]
               (map
                (fn [p]
                  {:rname rname
                   :pos p
                   :n (count-for-pos alns rname p)})
                positions)))
           (partition step (rpositions rlength))))))

;;; OPTIMIZE: This is implemented by pure Clojure, but it is too slow...
(defn pileup
  ([rdr]
     ;; TODO
     )
  ([rdr ^String rname]
     (let [r (search-ref (.refs rdr) rname)]
       (if (nil? r)
         nil
         (pileup* rdr rname (:len r))))))
