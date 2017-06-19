(ns cljam.io.bam-index.common)

(def bai-magic "BAI\1")

(def ^:const max-bins 37450)

(def level-starts [0 1 9 73 585 4681])

(def ^:const linear-index-shift 14)
