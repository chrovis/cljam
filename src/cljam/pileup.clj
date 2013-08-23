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

(defn- calc-pos
  [alns rname pos]
  (loop [alns2 alns
         val 0]
    (let [[aln & rst] alns2]
      (if (or (nil? aln) (not= rname (:rname aln)) (< pos (:pos aln)))
        {:n val, :rname rname, :pos pos}
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
       (let [ans (calc-pos alns rname pos)]
         (if (zero? (:n ans))
           (if (< pos (+ (:pos (first alns)) (count (substantial-seq (first alns)))))
             (pileup* alns rname (inc pos))
             (if (= rname (:rname (first (rest alns))))
               (pileup* (rest alns) rname (inc pos))
               (pileup* (rest alns) (:rname (first (rest alns))) (:pos (first (rest alns))))))
           (lazy-seq
            (cons
             ans
             (if (< pos (+ (:pos (first alns)) (count (substantial-seq (first alns)))))
               (pileup* alns rname (inc pos))
               (if (= rname (:rname (first (rest alns))))
                 (pileup* (rest alns) rname (inc pos))
                 (pileup* (rest alns) (:rname (first (rest alns))) (:pos (first (rest alns)))))))))))))

;;; OPTIMIZE: This is implemented by pure Clojure, but it is too slow...
(defn pileup
  ([sam]
     (pileup* (:alignments sam)))
  ([sam rname]
     (pileup* (:alignments sam) rname)))
