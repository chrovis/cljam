(ns cljam.binary-cigar-codec
  (:import (cljam.lib.cigar Cigar CigarElement CigarOperator)))

(defn binary-cigar->cigar-element [cigarette]
  (let [binary-op (bit-and cigarette 0xf)
        length (bit-shift-right cigarette 4)]
    (CigarElement. length (CigarOperator/binaryToEnum binary-op))))


(defn decode [binary-cigar]
  (let [ret (Cigar.)]
    (while (.hasRemaining binary-cigar)
      (.add ret (binary-cigar->cigar-element (.getInt binary-cigar))))
    ret))
