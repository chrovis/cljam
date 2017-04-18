(ns cljam.t-fasta
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.fasta :as fasta]
            [clojure.string :as str]))

(defn- slurp= [fa1 fa2]
  (when (= (count fa1) (count fa2))
    (loop [fa1 fa1
           fa2 fa2]
      (let [ff1 (first fa1)
            ff2 (first fa2)]
        (if (nil? ff1)
          true
          (when (and (= (:rname ff1) (:rname ff2))
                     (= (:offset ff1) (:offset ff2))
                     (= (:seq ff1) (:seq ff2))
                     (= (or (:blen ff1) (:line-blen ff1))
                        (or (:blen ff2) (:line-blen ff2))))
            (recur (rest fa1) (rest fa2))))))))

(deftest slurp-fasta
  (is (slurp= (fasta/slurp test-fa-file) test-fa)))

(deftest read-fasta-file
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-headers rdr)) test-fa-header))
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequences rdr)) test-fa-sequences))
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref" :start 5 :end 10})) "TGTTAG"))
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref2" :start 1 :end 16}))
        "AGGTTTTATAAAACAA"))
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref2" :start 0 :end 45}))
        "NAGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCGNNNNN"))
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref"}))
        "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
  (let [rdr (fasta/reader test-fa-file)]
    (is (= (fasta/read-sequence rdr {:chr "ref2"}))
        "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG")))

(deftest sequential-reading-of-fasta-file
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
