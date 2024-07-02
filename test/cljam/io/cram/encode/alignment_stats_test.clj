(ns cljam.io.cram.encode.alignment-stats-test
  (:require [cljam.io.cram.encode.alignment-stats :as stats]
            [clojure.test :refer [deftest is testing]]))

(deftest alignment-stats-builder-test
  (testing "all the reads are mapped to the same reference"
    (let [builder (stats/make-alignment-stats-builder)]
      (stats/update! builder 0 1 10 10 1)
      (stats/update! builder 0 5 14 10 1)
      (stats/update! builder 0 21 24 5 1)
      (is (= {:ri 0, :start 1, :end 24, :nbases 25, :nrecords 3}
             (into {} (stats/build builder))))))
  (testing "some of the reads are mapped to a different reference"
    (let [builder (stats/make-alignment-stats-builder)]
      (stats/update! builder 0 1 10 10 1)
      (stats/update! builder 1 1 7 7 1)
      (stats/update! builder 0 11 20 10 1)
      (is (= {:ri -2, :start 0, :end 0, :nbases 27 :nrecords 3}
             (into {} (stats/build builder))))))
  (testing "all the reads are unmapped"
    (let [builder (stats/make-alignment-stats-builder)]
      (stats/update! builder -1 0 0 10 1)
      (stats/update! builder -1 0 0 7 1)
      (stats/update! builder -1 11 20 10 1)
      (is (= {:ri -1, :start 0, :end 0, :nbases 27 :nrecords 3}
             (into {} (stats/build builder)))))))

(deftest merge-stats-test
  (testing "all the alignments stats are for the same reference"
    (is (= {:ri 1, :start 1, :end 150, :nbases 25000, :nrecords 50}
           (into {}
                 (stats/merge-stats [{:ri 1, :start 1, :end 100, :nbases 10000, :nrecords 20}
                                     {:ri 1, :start 50, :end 150, :nbases 15000, :nrecords 30}])))))
  (testing "some alignments stats are for a different reference"
    (is (= {:ri -2, :start 0, :end 0, :nbases 25000, :nrecords 50}
           (into {}
                 (stats/merge-stats [{:ri 0, :start 1, :end 100, :nbases 10000, :nrecords 20}
                                     {:ri 1, :start 50, :end 150, :nbases 15000, :nrecords 30}]))))
    (is (= {:ri -2, :start 0, :end 0, :nbases 25000, :nrecords 50}
           (into {}
                 (stats/merge-stats [{:ri -2, :start 0, :end 0, :nbases 10000, :nrecords 20}
                                     {:ri 1, :start 50, :end 150, :nbases 15000, :nrecords 30}]))))
    (is (= {:ri -2, :start 0, :end 0, :nbases 25000, :nrecords 50}
           (into {}
                 (stats/merge-stats [{:ri -2, :start 0, :end 0, :nbases 10000, :nrecords 20}
                                     {:ri -1, :start 0, :end 0, :nbases 15000, :nrecords 30}])))))
  (testing "all the alignments stats are for unmapped slices"
    (is (= {:ri -1, :start 0, :end 0, :nbases 25000, :nrecords 50}
           (into {}
                 (stats/merge-stats [{:ri -1, :start 0, :end 0, :nbases 10000, :nrecords 20}
                                     {:ri -1, :start 0, :end 0, :nbases 15000, :nrecords 30}]))))))
