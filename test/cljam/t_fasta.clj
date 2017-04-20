(ns cljam.t-fasta
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.fasta :as fasta]
            [clojure.string :as str]))

(deftest read-fasta-file
  (is (thrown? java.io.IOException
               (with-open [rdr (fasta/reader test-tabix-file)]
                 (fasta/read rdr))))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read rdr) test-fa-data))
    (is (= (fasta/read rdr) [{:len 0}]))
    (is (not-throw? (fasta/reset rdr)))
    (is (= (fasta/read rdr) test-fa-data)))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-indices rdr)
           [{:name "ref" :len 45 :offset 5 :line-blen 45 :line-len 46}
            {:name "ref2" :len 40 :offset 57 :line-blen 40 :line-len 41}])))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-headers rdr) test-fa-header)))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequences rdr) test-fa-sequences)))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {}) test-fa-sequences)))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref" :start 5 :end 10}) "TGTTAG")))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref2" :start 1 :end 16})
           "AGGTTTTATAAAACAA")))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref2" :start 0 :end 45})
           "NAGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCGNNNNN")))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref"})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT")))
  (with-open [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref2"})
           "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))))

(deftest sequential-reading-of-fasta-file
  (is (thrown? Exception (fasta/sequential-read test-tabix-file)))
  (is (= (fasta/sequential-read test-fa-file)
         (map #(update % :sequence str/upper-case) test-fa-sequences)))
  (is (= (fasta/sequential-read test-fa-bz2-file)
         (map #(update % :sequence str/upper-case) test-fa-sequences)))
  (let [fa (fasta/sequential-read medium-fa-file)]
    (is (= (map :name fa) '("chr1")))
    (is (= (map (comp count :sequence) fa) '(100000))))
  (let [fa (fasta/sequential-read medium-fa-gz-file)]
    (is (= (map :name fa) '("chr1")))
    (is (= (map (comp count :sequence) fa) '(100000)))))
