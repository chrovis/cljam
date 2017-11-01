(ns cljam.io.fasta.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.fasta.core :as fa-core]))

(def illegal-fasta-file test-tabix-file)

(def temp-test-fa-file (str temp-dir "/test.fa"))
(def temp-medium-fa-file (str temp-dir "/medium.fa"))

(deftest read-headers-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read-headers rdr) test-fa-header))))

(deftest read-indices-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read-indices rdr)
           [{:name "ref" :len 45 :offset 5 :line-blen 45 :line-len 46}
            {:name "ref2" :len 40 :offset 57 :line-blen 40 :line-len 41}]))))

(deftest read-sequences-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read-sequences rdr) test-fa-sequences))))

(deftest read-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read rdr) test-fa-data))
    (is (= (fa-core/read rdr) [{:len 0}]))
    (is (not-throw? (fa-core/reset rdr)))
    (is (= (fa-core/read rdr) test-fa-data)))
  (is (thrown? Exception
               (with-open [rdr (fa-core/reader test-tabix-file)]
                 (fa-core/read rdr)))))

(deftest sequential-read-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/sequential-read rdr)
           (map #(update % :sequence cstr/upper-case) test-fa-sequences))))
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/sequential-read rdr {:mask? false})
           (map #(update % :sequence cstr/upper-case) test-fa-sequences))))
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/sequential-read rdr {:mask? true})
           test-fa-sequences)))
  (with-open [rdr (fa-core/reader test-fa-bz2-file)]
    (is (= (fa-core/sequential-read rdr)
           (map #(update % :sequence cstr/upper-case) test-fa-sequences))))
  (with-open [rdr (fa-core/reader test-fa-bz2-file)]
    (is (= (fa-core/sequential-read rdr {:mask? false})
           (map #(update % :sequence cstr/upper-case) test-fa-sequences))))
  (with-open [rdr (fa-core/reader test-fa-bz2-file)]
    (is (= (fa-core/sequential-read rdr {:mask? true})
           test-fa-sequences)))
  (with-open [rdr (fa-core/reader medium-fa-file)]
    (let [fa (fa-core/sequential-read rdr)]
      (is (= (map :name fa) '("chr1" "chr2" "chr3" "chr4" "chr5" "chr6" "chr7" "chr8" "chr9")))
      (is (= (map (comp count :sequence) fa) (repeat 9 50000)))))
  (with-open [rdr (fa-core/reader medium-fa-gz-file)]
    (let [fa (fa-core/sequential-read rdr)]
      (is (= (map :name fa) '("chr1" "chr2" "chr3" "chr4" "chr5" "chr6" "chr7" "chr8" "chr9")))
      (is (= (map (comp count :sequence) fa) (repeat 9 50000))))))
