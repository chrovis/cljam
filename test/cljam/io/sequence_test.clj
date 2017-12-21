(ns cljam.io.sequence-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.fasta.core :as fa-core]
            [cljam.io.sequence :as cseq]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]))

(def temp-test-fa-file (str temp-dir "/test.fa"))
(def temp-medium-fa-file (str temp-dir "/medium.fa"))

(deftest reader-test
  (testing "fasta"
    (with-open [rdr (cseq/reader test-fa-file)
                cloned (cseq/reader rdr)]
      (is (instance? cljam.io.fasta.reader.FASTAReader rdr))
      (is (instance? cljam.io.fasta.reader.FASTAReader cloned))))
  (testing "twobit"
    (with-open [rdr (cseq/reader test-twobit-file)
                cloned (cseq/reader rdr)]
      (is (instance? cljam.io.twobit.reader.TwoBitReader rdr))
      (is (instance? cljam.io.twobit.reader.TwoBitReader cloned))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (cseq/reader f))
      "test-resources/fasta/not-found.fa"
      test-fai-file
      (Object.))))

(deftest read-indices-test
  (testing "fasta"
    (with-open [rdr (cseq/reader test-fa-file)]
      (is (= (cseq/read-indices rdr)
             [{:name "ref", :len 45, :offset 5, :line-blen 45, :line-len 46}
              {:name "ref2", :len 40, :offset 57, :line-blen 40, :line-len 41}]))))
  (testing "twobit"
    (with-open [rdr (cseq/reader test-twobit-file)]
      (is (= (cseq/read-indices rdr)
             [{:name "ref", :len 45, :offset 33, :ambs [], :masks [], :header-offset 16}
              {:name "ref2", :len 40, :offset 61, :ambs [], :masks [[1 40]], :header-offset 24}])))))

(deftest read-sequence-test
  (with-open [fa-rdr (cseq/reader test-fa-file)
              tb-rdr (cseq/reader test-twobit-file)]
    (are [?reg ?opts ?expect]
        (= (cseq/read-sequence fa-rdr ?reg ?opts)
           (cseq/read-sequence tb-rdr ?reg ?opts)
           ?expect)
      {} {} nil
      {:chr "badref"} {} nil
      {:chr "ref" :start -1 :end 0} {} nil
      {:chr "ref" :start 0 :end 0} {} nil
      {:chr "ref" :start -1 :end 1} {} "A"
      {:chr "ref" :start 0 :end 1} {} "A"
      {:chr "ref" :start 1 :end 2} {} "AG"
      {:chr "ref" :start 44 :end 45} {} "AT"
      {:chr "ref" :start 45 :end 45} {} "T"
      {:chr "ref" :start 45 :end 46} {} "T"
      {:chr "ref" :start 46 :end 46} {} nil
      {:chr "ref" :start 46 :end 47} {} nil
      {:chr "ref" :start 5 :end 10} {:mask? false} "TGTTAG"
      {:chr "ref" :start 5 :end 10} {:mask? true} "TGTTAG"
      {:chr "ref2" :start 1 :end 16} {:mask? false} "AGGTTTTATAAAACAA"
      {:chr "ref2" :start 1 :end 16} {:mask? true} "aggttttataaaacaa"
      {:chr "ref2" :start 1 :end 45} {:mask? false} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
      {:chr "ref2" :start 10} {:mask? false} "AAAACAATTAAGTCTACAGAGCAACTACGCG"
      {:chr "ref2" :end 10} {:mask? false} "AGGTTTTATA"
      {:chr "ref"} {:mask? false} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
      {:chr "ref"} {:mask? true} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
      {:chr "ref2"} {:mask? false} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
      {:chr "ref2"} {:mask? true} "aggttttataaaacaattaagtctacagagcaactacgcg"))
  (with-open [fa-rdr (cseq/reader test-fa-file)
              tb-rdr (cseq/reader test-twobit-file)]
    (are [?reg ?expect]
        (= (cseq/read-sequence fa-rdr ?reg)
           (cseq/read-sequence tb-rdr ?reg)
           ?expect)
      {:chr "ref" :start 5 :end 10} "TGTTAG"
      {:chr "ref2" :start 1 :end 16} "AGGTTTTATAAAACAA"
      {:chr "ref2" :start 0 :end 45} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
      {:chr "ref"} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
      {:chr "ref2"} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
  (with-open [fa-rdr (cseq/reader test-fa-file)
              tb-rdr (cseq/reader test-twobit-file)]
    (is (= (protocols/read-in-region fa-rdr {:chr "ref2" :start 1 :end 16})
           (protocols/read-in-region tb-rdr {:chr "ref2" :start 1 :end 16})
           "AGGTTTTATAAAACAA"))
    (is (= (protocols/read-in-region fa-rdr {:chr "ref2" :start 1 :end 16} {:mask? false})
           (protocols/read-in-region tb-rdr {:chr "ref2" :start 1 :end 16} {:mask? false})
           "AGGTTTTATAAAACAA"))
    (is (= (protocols/read-in-region fa-rdr {:chr "ref2" :start 1 :end 16} {:mask? true})
           (protocols/read-in-region tb-rdr {:chr "ref2" :start 1 :end 16} {:mask? true})
           "aggttttataaaacaa"))))

(deftest read-sequence-medium-fasta-test
  (let [expect (str "tgaatcaCATCAATTAAGAACTTTCTTCACCACCCCTTCGCTGTCATC"
                    "CTTTTCTCTCCACTATTCACCCAACATCATCCGGGACCAGAACTAATGTC"
                    "AGCAAAGC")
        u-expect (cstr/upper-case expect)]
    (with-open [rdr (cseq/fasta-reader medium-fa-file)]
      (is (= (cseq/read-sequence rdr {:chr "chr3" :start 2053 :end 2158}) u-expect))
      (is (= (cseq/read-sequence rdr {:chr "chr3" :start 2053 :end 2158} {:mask? false}) u-expect))
      (is (= (cseq/read-sequence rdr {:chr "chr3" :start 2053 :end 2158} {:mask? true}) expect)))))

(deftest read-sequence-twobit-test
  (testing "reference test"
    (with-open [r (cseq/twobit-reader test-twobit-file)
                c (cseq/reader r)]
      (is (= (for [i (range 1 45) j (range i 46)]
               (cseq/read-sequence r {:chr "ref" :start i :end j}))
             (for [i (range 1 45) j (range i 46)]
               (subs "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT" (dec i) j))))
      (is (= (protocols/read-in-region r {:chr "ref2" :start 1 :end 40})
             "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
      (is (= (protocols/read-in-region r {:chr "ref2" :start 1 :end 40} {:mask? false})
             "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
      (is (= (protocols/read-in-region r {:chr "ref2" :start 1 :end 40} {:mask? true})
             "aggttttataaaacaattaagtctacagagcaactacgcg"))))
  (testing "reference test with N"
    (with-open [r (cseq/twobit-reader test-twobit-n-file)
                c (cseq/reader r)]
      (are [?region ?opt ?result] (= (cseq/read-sequence r ?region ?opt)
                                     (cseq/read-sequence c ?region ?opt)
                                     ?result)
        {} {} nil
        {:chr "badref"} {} nil
        {:chr "ref"} {} "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
        {:chr "ref2"} {} "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
        {:chr "ref2"} {:mask? true} "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
        {:chr "ref" :start 10} {} "GATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
        {:chr "ref" :end 10} {} "NNNNNGTTAG"
        {:chr "ref" :start -3 :end 0} {} nil
        {:chr "ref" :start -3 :end 1} {} "N"
        {:chr "ref" :start 46 :end 50} {} nil
        {:chr "ref" :start 45 :end 50} {} "T"
        {:chr "ref" :start 1 :end 4} {} "NNNN"
        {:chr "ref" :start 0 :end 4} {} "NNNN"
        {:chr "ref" :start 41 :end 50} {} "NCCAT"
        {:chr "ref" :start 1 :end 45} {} "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
        {:chr "ref2" :start 1 :end 40} {} "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
        {:chr "ref2" :start 1 :end 40} {:mask? true} "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
        {:chr "chr1" :start 1 :end 40} {} nil)
      (is (= (for [i (range 1 45) j (range i 46)]
               (cseq/read-sequence r {:chr "ref" :start i :end j}))
             (for [i (range 1 45) j (range i 46)]
               (subs "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT" (dec i) j))))))
  (testing "big endian"
    (with-open [r (cseq/twobit-reader test-twobit-be-file)]
      (is (= (cseq/read-sequence r {:chr "ref"})
             "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT")))
    (with-open [r (cseq/twobit-reader test-twobit-be-n-file)]
      (is (= (cseq/read-sequence r {:chr "ref"})
             "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT")))))

(deftest indexed?-test
  (testing "fasta"
    (are [f] (with-open [rdr (cseq/reader f)]
               (true? (cseq/indexed? rdr)))
      test-fa-file
      medium-fa-file)
    (with-before-after {:before (do (prepare-cache!)
                                    (cio/copy (cio/file test-fa-file) (cio/file temp-test-fa-file))
                                    (cio/copy (cio/file medium-fa-file) (cio/file temp-medium-fa-file)))
                        :after (clean-cache!)}
      (are [f] (with-open [rdr (cseq/reader f)]
                 (false? (cseq/indexed? rdr)))
        temp-test-fa-file
        temp-medium-fa-file)))
  (testing "twobit"
    (with-open [rdr (cseq/reader test-twobit-file)]
      (true? (cseq/indexed? rdr)))))

(deftest writer-test
  (testing "fasta"
    (with-open [wtr (cseq/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fa")))]
      (is (instance? cljam.io.fasta.writer.FASTAWriter wtr))))
  (testing "twobit"
    (with-open [wtr (cseq/writer (.getAbsolutePath (cio/file util/temp-dir "temp.2bit")))]
      (is (instance? cljam.io.twobit.writer.TwoBitWriter wtr))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (cseq/writer (.getAbsolutePath (cio/file util/temp-dir f))))
      "temp.fsta"
      "temp.fa.fai")))

(deftest write-sequences-fasta-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [f (str temp-dir "/test.fa")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (fa-core/read r)))
      (is (same-sequence-contents? f test-fa-file))
      (is (same-file? f test-fa-file))
      (is (same-file? (str f ".fai") test-fai-file))

      (with-open [r1 (cseq/fasta-reader f)
                  r2 (cseq/fasta-reader test-fa-file)]
        (is (= (map (juxt :rname :seq) (fa-core/read r1))
               (map (juxt :rname :seq) (fa-core/read r2))))))

    (let [f (str temp-dir "/test2.fa")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (map (fn [s] (update s :seq (fn [x] (map cstr/upper-case x)))) (fa-core/read r))))
      (with-open [r1 (cseq/fasta-reader f)
                  r2 (cseq/fasta-reader test-fa-file)]
        (is (= (map (juxt :rname (comp cstr/upper-case :seq)) (fa-core/read r1))
               (map (juxt :rname (comp cstr/upper-case :seq)) (fa-core/read r2))))))

    (let [f (str temp-dir "/medium.fa")]
      (with-open [r (cseq/fasta-reader medium-fa-file)
                  w (cseq/fasta-writer f {:cols 50})]
        (cseq/write-sequences w (fa-core/read r)))
      (is (same-sequence-contents? f medium-fa-file))
      (is (same-file? f medium-fa-file))
      (is (same-file? (str f ".fai") medium-fai-file))

      (with-open [r1 (cseq/fasta-reader f)
                  r2 (cseq/fasta-reader medium-fa-file)]
        (is (= (map (juxt :rname :seq) (fa-core/read r1))
               (map (juxt :rname :seq) (fa-core/read r2))))))

    (let [f (str temp-dir "/test.fa.gz")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (fa-core/read r)))
      (is (same-sequence-contents? f test-fa-file)))

    (let [f (str temp-dir "/test.fa.bz2")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (fa-core/read r)))
      (is (same-sequence-contents? f test-fa-file)))

    (let [f (str temp-dir "/test3.fa")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (fa-core/sequential-read r)))
      (is (same-sequence-contents? f test-fa-file))
      (with-open [r1 (cseq/fasta-reader f)
                  r2 (cseq/fasta-reader test-fa-file)]
        (is (= (map (juxt :rname (comp cstr/upper-case :seq)) (fa-core/read r1))
               (map (juxt :rname (comp cstr/upper-case :seq)) (fa-core/read r2))))))

    (let [f (str temp-dir "/test4.fa")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f {:create-index? false})]
        (cseq/write-sequences w (fa-core/read r)))
      (is (thrown? java.io.FileNotFoundException
                   (with-open [r (cseq/fasta-reader f)]
                     (cseq/read-sequence r {:chr "ref"})))))))

(deftest write-sequences-twobit-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [f (str temp-dir "/test.2bit")]
      (with-open [r (cseq/twobit-reader test-twobit-file)
                  w (cseq/twobit-writer f)]
        (cseq/write-sequences w (cseq/read-all-sequences r {:mask? true})))
      (is (same-file? f test-twobit-file))
      (with-open [r1 (cseq/twobit-reader f)
                  r2 (cseq/twobit-reader test-twobit-file)]
        (is (= (cseq/read-all-sequences r1 {:mask? true}) (cseq/read-all-sequences r2 {:mask? true})))))

    (let [f (str temp-dir "/test-n.2bit")]
      (with-open [r (cseq/twobit-reader test-twobit-n-file)
                  w (cseq/twobit-writer f)]
        (cseq/write-sequences w (cseq/read-all-sequences r {:mask? true})))
      (is (same-file? f test-twobit-n-file)))

    (let [f (str temp-dir "/test-2.2bit")
          s [{:name "SEQ1" :sequence "AAAA"}
             {:name "SEQ2" :sequence "AAAAG"}
             {:name "SEQ3" :sequence "AAAAGC"}
             {:name "SEQ4" :sequence "AAAAGCT"}
             {:name "SEQ5" :sequence "AAAAGCTA"}]]
      (with-open [w (cseq/writer f)] (cseq/write-sequences w s))
      (with-open [r (cseq/reader f)] (is (= (cseq/read-all-sequences r) s))))))
