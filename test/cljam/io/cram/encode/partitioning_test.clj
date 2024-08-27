(ns cljam.io.cram.encode.partitioning-test
  (:require [cljam.io.cram.encode.partitioning :as partition]
            [clojure.test :refer [are deftest testing]]))

(defn- partition-alignments [header options alns]
  (let [acc (volatile! [])]
    (partition/with-each-container header options alns
      (fn [counter container-records]
        (vswap! acc conj [counter (mapv vec container-records)])))
    @acc))

(deftest with-each-container-test
  (let [options {:slices-per-container 2
                 :records-per-slice 3
                 :min-single-ref-slice-size 2}]
    (testing "sorted by coord"
      (let [header {:HD {:SO "coordinate"}}]
        (are [input expected]
             (= expected
                (partition-alignments header options input))
          []
          []

          [{:qname "q1", :rname "chr1"}]
          [[0 [[{:qname "q1", :rname "chr1"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "chr1"}
           {:qname "q4", :rname "chr1"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}
                {:qname "q3", :rname "chr1"}]
               [{:qname "q4", :rname "chr1"}]]]]

          (map (fn [i] {:qname (str \q (inc (long i))), :rname "chr1"}) (range 6))
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}
                {:qname "q3", :rname "chr1"}]
               [{:qname "q4", :rname "chr1"}
                {:qname "q5", :rname "chr1"}
                {:qname "q6", :rname "chr1"}]]]]

          (map (fn [i] {:qname (str \q (inc (long i))), :rname "chr1"}) (range 7))
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}
                {:qname "q3", :rname "chr1"}]
               [{:qname "q4", :rname "chr1"}
                {:qname "q5", :rname "chr1"}
                {:qname "q6", :rname "chr1"}]]]
           [6 [[{:qname "q7", :rname "chr1"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr2"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr2"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "chr2"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}]]]
           [2 [[{:qname "q3", :rname "chr2"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "chr1"}
           {:qname "q4", :rname "chr2"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}
                {:qname "q3", :rname "chr1"}]]]
           [3 [[{:qname "q4", :rname "chr2"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "chr2"}
           {:qname "q4", :rname "chr2"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}]]]
           [2 [[{:qname "q3", :rname "chr2"}
                {:qname "q4", :rname "chr2"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr2"}
           {:qname "q3", :rname "chr2"}
           {:qname "q4", :rname "chr2"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr2"}]]]
           [2 [[{:qname "q3", :rname "chr2"}
                {:qname "q4", :rname "chr2"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "chr1"}
           {:qname "q4", :rname "chr1"}
           {:qname "q5", :rname "chr2"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}
                {:qname "q3", :rname "chr1"}]
               [{:qname "q4", :rname "chr1"}]]]
           [4 [[{:qname "q5", :rname "chr2"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr2"}
           {:qname "q3", :rname "chr2"}
           {:qname "q4", :rname "chr3"}
           {:qname "q5", :rname "chr4"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr2"}]]]
           [2 [[{:qname "q3", :rname "chr2"}
                {:qname "q4", :rname "chr3"}]]]
           [4 [[{:qname "q5", :rname "chr4"}]]]]

          [{:qname "q1", :rname "*"}]
          [[0 [[{:qname "q1", :rname "*"}]]]]

          [{:qname "q1", :rname "*"}
           {:qname "q2", :rname "*"}]
          [[0 [[{:qname "q1", :rname "*"}
                {:qname "q2", :rname "*"}]]]]

          [{:qname "q1", :rname "*"}
           {:qname "q2", :rname "*"}
           {:qname "q3", :rname "*"}
           {:qname "q4", :rname "*"}]
          [[0 [[{:qname "q1", :rname "*"}
                {:qname "q2", :rname "*"}
                {:qname "q3", :rname "*"}]
               [{:qname "q4", :rname "*"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "*"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "*"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "*"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}]]]
           [2 [[{:qname "q3", :rname "*"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "chr1"}
           {:qname "q3", :rname "*"}
           {:qname "q4", :rname "*"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "chr1"}]]]
           [2 [[{:qname "q3", :rname "*"}
                {:qname "q4", :rname "*"}]]]]

          [{:qname "q1", :rname "chr1"}
           {:qname "q2", :rname "*"}
           {:qname "q3", :rname "*"}
           {:qname "q4", :rname "*"}]
          [[0 [[{:qname "q1", :rname "chr1"}
                {:qname "q2", :rname "*"}]]]
           [2 [[{:qname "q3", :rname "*"}
                {:qname "q4", :rname "*"}]]]])
        (are [input] (thrown? Exception (partition-alignments header options input))
          [{:qname "q1", :rname "*"}
           {:qname "q2", :rname "chr1"}]

          [{:qname "q1", :rname "*"}
           {:qname "q2", :rname "*"}
           {:qname "q3", :rname "chr1"}])))
    (testing "unsorted"
      (are [input expected]
           (= expected (partition-alignments {:HD {:SO "unsorted"}} options input))
        [{:qname "q1", :rname "chr1"}
         {:qname "q2", :rname "chr2"}
         {:qname "q3", :rname "chr1"}]
        [[0 [[{:qname "q1", :rname "chr1"}
              {:qname "q2", :rname "chr2"}
              {:qname "q3", :rname "chr1"}]]]]

        [{:qname "q1", :rname "chr1"}
         {:qname "q2", :rname "chr2"}
         {:qname "q3", :rname "chr1"}
         {:qname "q4", :rname "chr2"}]
        [[0 [[{:qname "q1", :rname "chr1"}
              {:qname "q2", :rname "chr2"}
              {:qname "q3", :rname "chr1"}]]]
         [3 [[{:qname "q4", :rname "chr2"}]]]]

        [{:qname "q1", :rname "*"}
         {:qname "q2", :rname "chr1"}]
        [[0 [[{:qname "q1", :rname "*"}
              {:qname "q2", :rname "chr1"}]]]]

        [{:qname "q1", :rname "*"}
         {:qname "q2", :rname "*"}
         {:qname "q3", :rname "chr1"}]
        [[0 [[{:qname "q1", :rname "*"}
              {:qname "q2", :rname "*"}
              {:qname "q3", :rname "chr1"}]]]]))))
