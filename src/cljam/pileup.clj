(ns cljam.pileup
  (:require [clojure.string :refer [join]]
            (cljam [sorter :as sorter])))

(defn substantial-seq [sa]
  (let [{seq :seq, cigar :cigar} sa]
    (join
     (loop [cursor  0
            matches (re-seq #"([0-9]*)([MIDNSHP=X])" cigar)
            ret     []]
       (if (first matches)
         (let [n  (Integer/parseInt (second (first matches)))
               op (last (first matches))]
           (condp #(not (nil? (%1 %2))) op
             #{"M"  "=" "X"}
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

;;; FIXME: incorrect algorithm
(defn pileup
  [sam]
  (if-not (sorter/sorted? sam)
    (throw (Exception. "Not sorted"))
    (loop [ret []
           tmp {}
           sas (:alignments sam)]
      (if (seq sas)
        (let [sa (first sas)
              [fin res] (if (= (:rname (second (first tmp))) (:rname sa))
                          (split-with #(< (first %) (:pos sa)) (sort tmp))
                          [(sort tmp) []])]
          (recur (concat ret (map #(assoc (second %) :pos (first %)) fin))
                 (loop [tmp2 (zipmap (map first res) (map second res))
                        pos  (:pos sa)]
                   (if (< pos (+ (:pos sa) (count (substantial-seq sa))))
                     (recur (assoc tmp2 pos
                                   (if-let [coll (tmp2 pos)]
                                     (update-in coll [:n] inc)
                                     {:rname (:rname sa), :n 1}))
                            (inc pos))
                     tmp2))
                 (rest sas)))
        (concat ret (map #(assoc (second %) :pos (first %)) (sort tmp)))))))


;; 7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
;; T  T  A  G  A  T  A  A  A  G  A  G  G  A  T  A  *  C  T  G
;;       A  A  A  A  G  A  T  A  A  *  G  *  G  G  A  T  A  A  A
;;       A  G  C  T  A  A
;;                            A  T  A  G  C  T  C  T  C  A  G  C
;;                                                                   T  A  G  G  C
