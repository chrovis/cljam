(ns cljam.cigar
  "Parser of CIGAR strings.")

(defn parse
  "Parses CIGAR string, returning a sequence of lengths and operations."
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
       (filter (comp #{\M \D \N \= \X} peek))
       (map first)
       (reduce +)))

(def count-ref
  (memoize count-ref*))
