(ns cljam.io.t-sequence
  (:require [clojure.test :refer :all]
            [clojure.string :as cstr]
            [cljam.t-common :refer :all]
            [cljam.io.fasta.core :as fa-core]
            [cljam.io.sequence :as cseq]))

(deftest read-sequence-fasta-test
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref" :start 5 :end 10}) "TGTTAG")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 1 :end 16})
           "AGGTTTTATAAAACAA")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref2" :start 0 :end 45})
           "NAGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCGNNNNN")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref"})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT")))
  (with-open [rdr (cseq/fasta-reader test-fa-file)]
    (is (= (cseq/read-sequence rdr {:chr "ref2"})
           "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))))

(deftest read-sequence-twobit-test
  (testing "reference test"
    (with-open [r (cseq/twobit-reader test-twobit-file)]
      (are [?region ?opt ?result] (= (cseq/read-sequence r ?region ?opt) ?result)
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
               (subs "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT" (dec i) j))))))
  (testing "reference test with N"
    (with-open [r (cseq/twobit-reader test-twobit-n-file)]
      (are [?region ?opt ?result] (= (cseq/read-sequence r ?region ?opt) ?result)
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

      (with-open [r1 (cseq/fasta-reader f :ignore-index false)
                  r2 (cseq/fasta-reader test-fa-file :ignore-index false)]
        (is (= (map (juxt :rname :seq) (fa-core/read r1))
               (map (juxt :rname :seq) (fa-core/read r2))))))

    (let [f (str temp-dir "/test2.fa")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f)]
        (cseq/write-sequences w (map (fn [s] (update s :seq (fn [x] (map cstr/upper-case x)))) (fa-core/read r))))
      (with-open [r1 (cseq/fasta-reader f :ignore-index false)
                  r2 (cseq/fasta-reader test-fa-file :ignore-index false)]
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

      (with-open [r1 (cseq/fasta-reader f :ignore-index false)
                  r2 (cseq/fasta-reader medium-fa-file :ignore-index false)]
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
      (with-open [r1 (cseq/fasta-reader f :ignore-index false)
                  r2 (cseq/fasta-reader test-fa-file :ignore-index false)]
        (is (= (map (juxt :rname (comp cstr/upper-case :seq)) (fa-core/read r1))
               (map (juxt :rname (comp cstr/upper-case :seq)) (fa-core/read r2))))))

    (let [f (str temp-dir "/test4.fa")]
      (with-open [r (cseq/fasta-reader test-fa-file)
                  w (cseq/fasta-writer f {:create-index? false})]
        (cseq/write-sequences w (fa-core/read r)))

      (is (thrown? java.io.FileNotFoundException
                   (with-open [r (cseq/fasta-reader f :ignore-index false)] nil))))))

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
