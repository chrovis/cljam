(ns cljam.util.intervals-test
  (:require [clojure.test :refer [deftest are testing]]
            [clojure.template :refer [do-template]]
            [cljam.util.intervals :as intervals]))

(def ^:private test-input1
  [{:chr "chr1" :start 10 :end 100} {:chr "chr2" :start 10 :end 100}
   {:chr "chr2" :start 150 :end 200} {:chr "chr2" :start 150 :end 250}
   {:chr "chr2" :start 180 :end 300} {:chr "chr2" :start 181 :end 200}
   {:chr "chr2" :start 182 :end 200} {:chr "chr2" :start 182 :end 201}
   {:chr "chr2" :start 200 :end 400}])

(deftest sorted-map-intervals-test
  (do-template [index-type]
               (testing (str index-type)
                 (let [index
                       (intervals/index-intervals test-input1
                                                  {:structure index-type})]
                   (are [chr start end result]
                        (= (set (intervals/find-overlap-intervals index chr
                                                                  start end))
                           result)
                     "chr1" 1 10 #{{:chr "chr1" :start 10 :end 100}},
                     "chr1" 1 9 #{},
                     "chr1" 100 101 #{{:chr "chr1" :start 10 :end 100}},
                     "chr1" 101 102 #{},
                     "chr1" 11 99 #{{:chr "chr1" :start 10 :end 100}},
                     "chr2" 10 100 #{{:chr "chr2" :start 10 :end 100}},
                     "chr2" 110 111 #{}
                     "chr2" 90 160 #{{:chr "chr2" :start 10 :end 100}
                                     {:chr "chr2" :start 150 :end 200}
                                     {:chr "chr2" :start 150 :end 250}},
                     "chr2" 150 181 #{{:chr "chr2" :start 150 :end 200}
                                      {:chr "chr2" :start 150 :end 250}
                                      {:chr "chr2" :start 180 :end 300}
                                      {:chr "chr2" :start 181 :end 200}},
                     "chr2" 201 240 #{{:chr "chr2" :start 150 :end 250}
                                      {:chr "chr2" :start 180 :end 300}
                                      {:chr "chr2" :start 182 :end 201}
                                      {:chr "chr2" :start 200 :end 400}})))
               :sorted-map :nclist))
