(ns cljam.io.t-fasta
  (:require [clojure.test :refer :all]
            [clojure.string :as cstr]
            [cljam.t-common :refer :all]
            [cljam.io.fasta :as fasta]))

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
         (map #(update % :sequence cstr/upper-case) test-fa-sequences)))
  (is (= (fasta/sequential-read test-fa-bz2-file)
         (map #(update % :sequence cstr/upper-case) test-fa-sequences)))
  (let [fa (fasta/sequential-read medium-fa-file)]
    (is (= (map :name fa) '("chr1")))
    (is (= (map (comp count :sequence) fa) '(100000))))
  (let [fa (fasta/sequential-read medium-fa-gz-file)]
    (is (= (map :name fa) '("chr1")))
    (is (= (map (comp count :sequence) fa) '(100000)))))

(deftest write-fasta-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [f (str temp-dir "/test.fa")]
      (with-open [r (fasta/reader test-fa-file)
                  w (fasta/writer f)]
        (fasta/write-sequences w (fasta/read r)))
      (is (= (fasta/sequential-read f)
             (fasta/sequential-read test-fa-file)))

      (is (same-file? f test-fa-file))
      (is (same-file? (str f ".fai") test-fai-file))

      (with-open [r1 (fasta/reader f :ignore-index false)
                  r2 (fasta/reader test-fa-file :ignore-index false)]
        (is (= (map (juxt :rname :seq) (fasta/read r1))
               (map (juxt :rname :seq) (fasta/read r2))))))

    (let [f (str temp-dir "/test2.fa")]
      (with-open [r (fasta/reader test-fa-file)
                  w (fasta/writer f)]
        (fasta/write-sequences w (map (fn [s] (update s :seq (fn [x] (map cstr/upper-case x)))) (fasta/read r))))
      (with-open [r1 (fasta/reader f :ignore-index false)
                  r2 (fasta/reader test-fa-file :ignore-index false)]
        (is (= (map (juxt :rname (comp cstr/upper-case :seq)) (fasta/read r1))
               (map (juxt :rname (comp cstr/upper-case :seq)) (fasta/read r2))))))

    (let [f (str temp-dir "/medium.fa")]
      (with-open [r (fasta/reader medium-fa-file)
                  w (fasta/writer f {:cols 60})]
        (fasta/write-sequences w (fasta/read r)))
      (is (= (fasta/sequential-read f)
             (fasta/sequential-read medium-fa-file)))

      (is (same-file? f medium-fa-file))
      (is (same-file? (str f ".fai") medium-fai-file))

      (with-open [r1 (fasta/reader f :ignore-index false)
                  r2 (fasta/reader medium-fa-file :ignore-index false)]
        (is (= (map (juxt :rname :seq) (fasta/read r1))
               (map (juxt :rname :seq) (fasta/read r2))))))

    (let [f (str temp-dir "/test.fa.gz")]
      (with-open [r (fasta/reader test-fa-file)
                  w (fasta/writer f)]
        (fasta/write-sequences w (fasta/read r)))
      (is (= (fasta/sequential-read f)
             (fasta/sequential-read test-fa-file))))

    (let [f (str temp-dir "/test.fa.bz2")]
      (with-open [r (fasta/reader test-fa-file)
                  w (fasta/writer f)]
        (fasta/write-sequences w (fasta/read r)))
      (is (= (fasta/sequential-read f)
             (fasta/sequential-read test-fa-file))))

    (let [f (str temp-dir "/test3.fa")]
      (with-open [w (fasta/writer f)]
        (fasta/write-sequences w (fasta/sequential-read test-fa-file)))
      (is (= (fasta/sequential-read f)
             (fasta/sequential-read test-fa-file)))
      (with-open [r1 (fasta/reader f :ignore-index false)
                  r2 (fasta/reader test-fa-file :ignore-index false)]
        (is (= (map (juxt :rname (comp cstr/upper-case :seq)) (fasta/read r1))
               (map (juxt :rname (comp cstr/upper-case :seq)) (fasta/read r2))))))

    (let [f (str temp-dir "/test4.fa")]
      (with-open [r (fasta/reader test-fa-file)
                  w (fasta/writer f {:create-index? false})]
        (fasta/write-sequences w (fasta/read r)))

      (is (thrown? java.io.FileNotFoundException
                   (with-open [r (fasta/reader f :ignore-index false)]))))))
