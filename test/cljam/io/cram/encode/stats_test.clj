(ns cljam.io.cram.encode.stats-test
  (:require [cljam.io.cram.encode.stats :as stats]
            [clojure.test :refer [deftest is testing]]))

(deftest stats-recorder-test
  (testing "all the reads are mapped to the same reference"
    (let [stats (stats/make-stats-recorder 2)]
      (stats/update! stats 0 1 10 10 1)
      (stats/update! stats 0 5 14 10 1)
      (stats/update! stats 0 21 24 5 1)
      (is (= {:ri 0, :start 1, :end 24, :nbases 25, :nrecords 3}
             (into {} (stats/stats stats))))))
  (testing "some of the reads are mapped to a different reference"
    (let [stats (stats/make-stats-recorder 2)]
      (stats/update! stats 0 1 10 10 1)
      (stats/update! stats 1 1 7 7 1)
      (stats/update! stats 0 11 20 10 1)
      (is (= {:ri -2, :start 0, :end 0, :nbases 27 :nrecords 3}
             (into {} (stats/stats stats))))))
  (testing "all the reads are unmapped"
    (let [stats (stats/make-stats-recorder 2)]
      (stats/update! stats -1 0 0 10 1)
      (stats/update! stats -1 0 0 7 1)
      (stats/update! stats -1 11 20 10 1)
      (is (= {:ri -1, :start 0, :end 0, :nbases 27 :nrecords 3}
             (into {} (stats/stats stats)))))))
