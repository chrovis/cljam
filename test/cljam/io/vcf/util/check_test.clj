(ns cljam.io.vcf.util.check-test
  (:require [clojure.test :refer [deftest are]]
            [cljam.io.protocols :as protocols]
            [cljam.io.vcf.util.check :as check]))

(defn- seq-reader [s]
  (reify protocols/ISequenceReader
    (read-sequence [_ {:keys [^long start ^long end]} _]
      (subs s (dec start) end))))

(deftest same-ref?
  (are [?seq ?variant ?expected]
       (= ?expected (check/same-ref? (seq-reader ?seq) ?variant))
    "A" {:pos 1, :ref "A"} true
    "ATT" {:pos 2, :ref "tt"} true
    "TTT" {:pos 3, :ref "A"} false
    "ATGC" {:pos 4, :ref "N"} false))
