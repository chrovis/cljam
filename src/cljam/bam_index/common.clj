(ns cljam.bam-index.common)

(def ^String bai-magic "BAI\1")

(def ^:const max-bins 37450)

(def level-starts [0 1 9 73 585 4681])

(def max-lidx-size (- (inc max-bins) (last level-starts)))
