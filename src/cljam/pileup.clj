(ns cljam.pileup
  (:require [clojure.string :refer [join]]
            (cljam [sorter :as sorter])))

(defn substantial-seq [aln]
  (let [{seq :seq, cigar :cigar} aln]
    (join
     (loop [cursor  0
            matches (re-seq #"([0-9]*)([MIDNSHP=X])" cigar)
            ret     []]
       (if (first matches)
         (let [n  (Integer/parseInt (second (first matches)))
               op (last (first matches))]
           (condp #(not (nil? (%1 %2))) op
             #{"M" "=" "X"}
             (recur (+ cursor n)
                    (rest matches)
                    (conj ret (subs seq cursor (+ cursor n))))

             #{"D"}
             (recur cursor
                    (rest matches)
                    (conj ret (join (repeat n "*"))))

             #{"N"}
             (recur cursor
                    (rest matches)
                    (conj ret (join (repeat n ">"))))

             (recur cursor (rest matches) ret)))
         ret)))))

(defn- calc-pos #^Long
  [alns #^String rname #^Long pos]
  (loop [alns2 alns
         val 0]
    (let [[aln & rst] alns2]
      (if (or (nil? aln) (not= rname (:rname aln)) (< pos (:pos aln)))
        val
        (if (< pos (+ (:pos aln) (count (substantial-seq aln))))
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
               (if (< pos (+ (:pos aln) (count (substantial-seq aln))))
                 (pileup* alns rname (inc pos))
                 (pileup* rst rname (inc pos)))))))))))

;;; OPTIMIZE: This is implemented by pure Clojure, but it is too slow...
(defn pileup
  ([sam]
     (pileup* (:alignments sam)))
  ([sam rname]
     (pileup* (:alignments sam) rname)))
