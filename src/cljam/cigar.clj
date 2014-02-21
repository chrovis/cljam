(ns cljam.cigar
  (:require [clojure.string :as str]))

(defn parse
  "Parses CIGAR text, returns seq of lengths and operations."
  [^String s]
  (for [[_ n op] (re-seq #"([0-9]*)([MIDNSHP=X])" s)]
    [(Integer/parseInt n) (first op)]))

(defn count-op
  "Returns length of CIGAR operations."
  [^String s]
  (count (parse s)))

(defn- count-ref*
  "Returns length of reference bases."
  [^String s]
  (->> (parse s)
       (filter (comp #{\M \D \N \= \X} last))
       (map first)
       (reduce +)))

(def count-ref
  (memoize count-ref*))

(defn substantial-seq [cigar seq]
  (str/join
   (loop [matches (parse cigar)
          cursor  0
          ret     []]
     (if-let [[n op]  (first matches)]
       (let [[cursor* ret*] (condp #(not (nil? (%1 %2))) op
                              #{\M \= \X} [(+ cursor n)
                                           (conj ret (subs seq cursor (+ cursor n)))]
                              #{\D}       [cursor
                                           (conj ret (str/join (repeat n "*")))]
                              #{\N}       [cursor
                                           (conj ret (str/join (repeat n ">")))]
                              #{\S \I}    [(+ cursor n)
                                           ret]
                              [cursor ret])]
         (recur (rest matches) cursor* ret*))
       ret))))
