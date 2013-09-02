(ns cljam.pileup
  (:require [cljam [cigar :as cgr]]))

(defn- calc-pos
  "Returns a histogram value of the specified position."
  [alns rname pos]
  (loop [alns2 alns
         val 0]
    (let [[aln & rst] alns2]
      (if (or (nil? aln) (not= rname (:rname aln)) (< pos (:pos aln)))
        val
        (if (< pos (+ (:pos aln) (cgr/count-ref (:cigar aln))))
          (recur rst (inc val))
          (recur rst val))))))

(defn- pileup*
  ([alns]
     (pileup* alns (:rname (first alns)) (:pos (first alns))))
  ([alns rname]
     (let [alns (filter #(= (:rname %) rname) alns)]
      (pileup* alns rname (:pos (first alns)))))
  ([alns rname pos]
     (when (seq alns)
       (let [val (calc-pos alns rname pos)
             [aln & rst] alns]
         (if (zero? val)
           (if-not (or (nil? rst) (= rname (:rname (first rst))))
             (pileup* rst (:rname (first rst)) (:pos (first rst)))
             (if (< pos (:pos aln))
               (pileup* alns rname (:pos aln))
               (pileup* rst rname (inc pos))))
           (lazy-seq
            (cons
             {:rname rname, :pos pos, :n val}
             (if-not (or (nil? rst) (= rname (:rname (first rst))))
               (pileup* rst (:rname (first rst)) (:pos (first rst)))
               (if (< pos (+ (:pos aln) (cgr/count-ref (:cigar aln))))
                 (pileup* alns rname (inc pos))
                 (pileup* rst rname (inc pos)))))))))))

;;; OPTIMIZE: This is implemented by pure Clojure, but it is too slow...
(defn pileup
  ([sam]
     (pileup* (:alignments sam)))
  ([sam rname]
     (pileup* (:alignments sam) rname)))
