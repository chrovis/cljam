(ns cljam.util.intervals-test
  (:require [clojure.test :refer [deftest is]]
            [cljam.util.intervals :as intervals]))

(def ^:private test-input1
  [{:chr "chr1" :start 10 :end 100} {:chr "chr2" :start 10 :end 100}
   {:chr "chr2" :start 150 :end 200} {:chr "chr2" :start 150 :end 250}
   {:chr "chr2" :start 180 :end 300} {:chr "chr2" :start 181 :end 200}
   {:chr "chr2" :start 182 :end 200} {:chr "chr2" :start 182 :end 201}
   {:chr "chr2" :start 200 :end 400}])

(deftest sorted-map-intervals-test
  (let [smi (intervals/make-sorted-map-intervals test-input1)]
    (is (= (intervals/find-overlap-intervals smi "chr1" 1 10)
           [{:chr "chr1" :start 10 :end 100}]))
    (is (= (intervals/find-overlap-intervals smi "chr1" 1 9) []))
    (is (= (intervals/find-overlap-intervals smi "chr1" 100 101)
           [{:chr "chr1" :start 10 :end 100}]))
    (is (= (intervals/find-overlap-intervals smi "chr1" 101 102) []))
    (is (= (intervals/find-overlap-intervals smi "chr1" 11 99)
           [{:chr "chr1" :start 10 :end 100}]))
    (is (= (intervals/find-overlap-intervals smi "chr2" 10 100)
           [{:chr "chr2" :start 10 :end 100}]))
    (is (= (intervals/find-overlap-intervals smi "chr2" 110 111) []))
    (is (= (intervals/find-overlap-intervals smi "chr2" 90 160)
           [{:chr "chr2" :start 10 :end 100} {:chr "chr2" :start 150 :end 200}
            {:chr "chr2" :start 150 :end 250}]))
    (is (= (intervals/find-overlap-intervals smi "chr2" 150 181)
           [{:chr "chr2" :start 150 :end 200} {:chr "chr2" :start 150 :end 250}
            {:chr "chr2" :start 180 :end 300} {:chr "chr2" :start 181 :end 200}]))
    (is (= (intervals/find-overlap-intervals smi "chr2" 201 240)
           [{:chr "chr2" :start 150 :end 250} {:chr "chr2" :start 180 :end 300}
            {:chr "chr2" :start 182 :end 201} {:chr "chr2" :start 200 :end 400}]))))
