(ns cljam.util.t-whole-genome
  (:require [clojure.test :refer :all]
            [cljam.util.whole-genome :as wg]))

(def simple-refs [{:name "1", :len 100} {:name "2", :len 200} {:name "3", :len 300}])
(def refs
  [{:name "chr1", :len 248956422}  {:name "chr2", :len 242193529}  {:name "chr3", :len 198295559}
   {:name "chr4", :len 190214555}  {:name "chr5", :len 181538259}  {:name "chr6", :len 170805979}
   {:name "chr7", :len 159345973}  {:name "chr8", :len 145138636}  {:name "chr9", :len 138394717}
   {:name "chr10", :len 133797422} {:name "chr11", :len 135086622} {:name "chr12", :len 133275309}
   {:name "chr13", :len 114364328} {:name "chr14", :len 107043718} {:name "chr15", :len 101991189}
   {:name "chr16", :len 90338345}  {:name "chr17", :len 83257441}  {:name "chr18", :len 80373285}
   {:name "chr19", :len 58617616}  {:name "chr20", :len 64444167}  {:name "chr21", :len 46709983}
   {:name "chr22", :len 50818468}  {:name "chrX", :len 156040895}  {:name "chrY", :len 57227415}
   {:name "chrM", :len 16569}])

(deftest chr->wg
  (let [idx (wg/create-chr-to-whole-genome-index simple-refs)]
    (testing "chr -> whole-genome index"
      (is (= idx {"1" {:offset 0, :len 100}, "2" {:offset 100, :len 200}, "3" {:offset 300, :len 300}})))

    (testing "chr -> whole-genome (simple)"
      (are [?chr ?pos ?wg-pos]
          (= (wg/to-whole-genome-coord idx ?chr ?pos) ?wg-pos)
        "0" 1 nil
        "1" 0 nil
        "1" 1 1
        "1" 100 100
        "2" 0 nil
        "2" 1 101
        "2" 200 300
        "2" 201 nil
        "3" 1 301
        "3" 300 600
        "3" 301 nil)))

  (let [idx (wg/create-chr-to-whole-genome-index refs)]
    (testing "chr -> whole-genome"
      (are [?chr ?pos ?wg-pos]
          (= (wg/to-whole-genome-coord idx ?chr ?pos) ?wg-pos)
        "chr1" 1 1
        "chr1" 248956422 248956422
        "chr2" 1 (+ 248956422 1)
        "chr2" 242193529 (+ 248956422 242193529)
        "chr3" 1 (+ 248956422 242193529 1)))))

(deftest wg->chr
  (let [idx (wg/create-whole-genome-to-chr-index simple-refs)]
    (testing "whole-genome -> chr index"
      (is (= idx {0 {:name "1", :len 100}, 100 {:name "2", :len 200}, 300 {:name "3", :len 300}})))

    (testing "whole-genome -> chr"
      (are [?wg-pos ?chr-and-pos]
          (= (wg/to-chr-and-pos idx ?wg-pos) ?chr-and-pos)
          0 nil
          1 ["1" 1]
          2 ["1" 2]
          99 ["1" 99]
          100 ["1" 100]
          101 ["2" 1]
          300 ["2" 200]
          301 ["3" 1]
          600 ["3" 300]
          601 nil))

    (testing "whole-genome [start, end] -> regions"
      (are [?wg-start ?wg-end ?regions]
          (= (wg/to-regions idx ?wg-start ?wg-end) ?regions)
        0 0 nil
        0 1 [{:chr "1", :start 1, :end 1}]
        1 0 nil
        1 1 [{:chr "1", :start 1, :end 1}]
        1 2 [{:chr "1", :start 1, :end 2}]
        2 1 nil
        50 100 [{:chr "1", :start 50, :end 100}]
        50 150 [{:chr "1", :start 50, :end 100} {:chr "2", :start 1, :end 50}]
        50 350 [{:chr "1", :start 50, :end 100} {:chr "2", :start 1, :end 200} {:chr "3", :start 1, :end 50}]
        150 550 [{:chr "2", :start 50, :end 200} {:chr "3", :start 1, :end 250}]
        301 600 [{:chr "3", :start 1, :end 300}]
        301 601 [{:chr "3", :start 1, :end 300}]
        600 601 [{:chr "3", :start 300, :end 300}]
        601 601 nil))))
