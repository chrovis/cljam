(ns cljam.io.sequence-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.fasta.core :as fa-core]
            [cljam.io.sequence :as cseq]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]))

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

(deftest read-sequence-fasta-test
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref" :start 5 :end 10}) "TGTTAG"))
    (is (= (cseq/read-sequence rdr {:chr "ref" :start 5 :end 10} {:mask? false})
           "TGTTAG"))
    (is (= (cseq/read-sequence rdr {:chr "ref" :start 5 :end 10} {:mask? true})
           "TGTTAG")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 1 :end 16})
           "AGGTTTTATAAAACAA"))
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 1 :end 16} {:mask? false})
           "AGGTTTTATAAAACAA"))
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 1 :end 16} {:mask? true})
           "aggttttataaaacaa")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 0 :end 45})
           "NAGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCGNNNNN"))
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 0 :end 45} {:mask? false})
           "NAGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCGNNNNN"))
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 0 :end 45} {:mask? true})
           "NaggttttataaaacaattaagtctacagagcaactacgcgNNNNN")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref"})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
    (is (= (cseq/read-sequence rdr {:chr "ref"} {:mask? false})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
    (is (= (cseq/read-sequence rdr {:chr "ref"} {:mask? true})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref2"})
           "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
    (is (= (cseq/read-sequence rdr {:chr "ref2"} {:mask? false})
           "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
    (is (= (cseq/read-sequence rdr {:chr "ref2"} {:mask? true})
           "aggttttataaaacaattaagtctacagagcaactacgcg")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (protocols/read-in-region rdr {:chr "ref2" :start 1 :end 16})
           "AGGTTTTATAAAACAA"))
    (is (= (protocols/read-in-region rdr {:chr "ref2" :start 1 :end 16} {:mask? false})
           "AGGTTTTATAAAACAA"))
    (is (= (protocols/read-in-region rdr {:chr "ref2" :start 1 :end 16} {:mask? true})
           "aggttttataaaacaa"))))

(deftest read-sequence-twobit-test
  (testing "reference test"
    (with-open [r (cseq/twobit-reader test-twobit-file)
                c (cseq/reader r)]
      (are [?region ?opt ?result] (= (cseq/read-sequence r ?region ?opt)
                                     (cseq/read-sequence c ?region ?opt)
                                     ?result)
        {:chr "ref"} {} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
        {:chr "ref2"} {} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
        {:chr "ref2"} {:mask? true} "aggttttataaaacaattaagtctacagagcaactacgcg"
        {:chr "ref" :start 1 :end 4} {} "AGCA"
        {:chr "ref" :start 0 :end 4} {} "NAGCA"
        {:chr "ref" :start 41 :end 50} {} "GCCATNNNNN"
        {:chr "ref" :start 1 :end 45} {} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
        {:chr "ref2" :start 1 :end 40} {} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
        {:chr "ref2" :start 1 :end 40} {:mask? true} "aggttttataaaacaattaagtctacagagcaactacgcg"
        {:chr "chr1" :start 1 :end 40} {} nil)
      (is (= (for [i (range 1 45) j (range i 46)]
               (cseq/read-sequence r {:chr "ref" :start i :end j}))
             (for [i (range 1 45) j (range i 46)]
               (subs "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT" (dec i) j))))
      (is (= (protocols/read-in-region r {:chr "ref2" :start 1 :end 40}) "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
      (is (= (protocols/read-in-region r {:chr "ref2" :start 1 :end 40} {:mask? false}) "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
      (is (= (protocols/read-in-region r {:chr "ref2" :start 1 :end 40} {:mask? true}) "aggttttataaaacaattaagtctacagagcaactacgcg"))))
  (testing "reference test with N"
    (with-open [r (cseq/twobit-reader test-twobit-n-file)
                c (cseq/reader r)]
      (are [?region ?opt ?result] (= (cseq/read-sequence r ?region ?opt)
                                     (cseq/read-sequence c ?region ?opt)
                                     ?result)
        {:chr "ref"} {} "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
        {:chr "ref2"} {} "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
        {:chr "ref2"} {:mask? true} "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
        {:chr "ref" :start 1 :end 4} {} "NNNN"
        {:chr "ref" :start 0 :end 4} {} "NNNNN"
        {:chr "ref" :start 41 :end 50} {} "NCCATNNNNN"
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
      (is (= (fa-core/sequential-read f)
             (fa-core/sequential-read test-fa-file)))

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
                  w (cseq/fasta-writer f {:cols 60})]
        (cseq/write-sequences w (fa-core/read r)))
      (is (= (fa-core/sequential-read f)
             (fa-core/sequential-read medium-fa-file)))

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
      (is (= (fa-core/sequential-read f)
             (fa-core/sequential-read test-fa-file))))

    (let [f (str temp-dir "/test.fa.bz2")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (fa-core/read r)))
      (is (= (fa-core/sequential-read f)
             (fa-core/sequential-read test-fa-file))))

    (let [f (str temp-dir "/test3.fa")]
      (with-open [w (cseq/fasta-writer f)]
        (cseq/write-sequences w (fa-core/sequential-read test-fa-file)))
      (is (= (fa-core/sequential-read f)
             (fa-core/sequential-read test-fa-file)))
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
      (is (same-file? f test-twobit-n-file)))))
