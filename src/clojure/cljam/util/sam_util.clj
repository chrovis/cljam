(ns cljam.util.sam-util
  (:require [cljam.cigar :refer [count-ref]]
            [cljam.util :refer [reg->bin]]))

(defn- get-end [aln]
  (dec
   (+ (:pos aln)
      (count-ref (:cigar aln)))))

(defn compute-bin
  "Returns indexing bin based on alignment start and end."
  [aln]
  (let [beg (dec (:pos aln))
        tmp-end (get-end aln)
        end (if (<= tmp-end 0) (inc beg) tmp-end)]
   (reg->bin beg end)))
