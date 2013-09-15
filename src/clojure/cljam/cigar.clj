(ns cljam.cigar
  (:require [clojure.string :refer [join]]))

(defn parse
  "Parses CIGAR text, returns seq of lengths and operations."
  [s]
  (for [[_ n op] (re-seq #"([0-9]*)([MIDNSHP=X])" s)]
    [(Integer/parseInt n) (first op)]))

(defn count-op
  "Returns length of CIGAR operations."
  [s]
  (count (parse s)))

(defn count-ref
  "Returns length of reference bases."
  [s]
  (->> (parse s)
       (filter (comp #{\M \D \N \= \X} last))
       (map first)
       (reduce +)))

(defn substantial-seq [cigar seq]
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
       ret))))
