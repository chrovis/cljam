(ns cljam.t-io
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.io :as io]
            [cljam.io.core :as io-core]
            [cljam.t-common :as common]))

(deftest readers
  (are [?file]
      (with-open [r (io-core/reader ?file)]
        (= (io/reader-path r) (.getAbsolutePath (cio/file ?file))))
    common/test-sam-file
    common/small-bam-file
    common/test-fa-file
    common/test-twobit-file
    common/test-vcf-v4_3-file
    common/test-bcf-v4_3-file
    common/test-fq-file
    common/test-bed-file1))

(deftest writers
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (are [?ext]
        (let [f (.getPath (cio/file common/temp-dir (str "test." ?ext)))]
          (with-open [r (io-core/writer f)]
            (= (io/writer-path r) (.getAbsolutePath (cio/file f)))))
      "sam"
      "bam"
      "fasta"
      "2bit"
      "fastq"
      "bed")

    (are [?ext]
        (let [f (.getPath (cio/file common/temp-dir (str "test." ?ext)))]
          (with-open [r (io-core/writer f {} [])]
            (= (io/writer-path r) (.getAbsolutePath (cio/file f)))))
      "vcf"
      "bcf")))

(deftest bam-io
  (is (= (with-open [r (io-core/reader common/medium-bam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read r)))
         [546609 547480 662438 714873 907683 1026525 1058055 1102230 1105259 1120976]))

  (is (= (with-open [r (io-core/reader common/medium-bam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read r {:depth :deep})))
         [546609 547480 662438 714873 907683 1026525 1058055 1102230 1105259 1120976]))

  (is (= (with-open [r (io-core/reader common/medium-bam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {})))
         [546609 547480 662438 714873 907683 1026525 1058055 1102230 1105259 1120976]))

  (is (= (with-open [r (io-core/reader common/medium-bam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {:chr "chr2"})))
         [67302 388030 393457 552907 650982 777266 845153 875507 905898 906758]))

  (is (= (with-open [r (io-core/reader common/medium-bam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {:chr "chr2" :start 70000})))
         [388030 393457 552907 650982 777266 845153 875507 905898 906758 1232621]))

  (is (= (with-open [r (io-core/reader common/medium-bam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {:chr "chr2" :start 70000 :end 500000})))
         [388030 393457])))

(deftest sam-io
  (is (= (with-open [r (io-core/reader common/test-sam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read r)))
         [29 7 9 9 6 16 37 1 2 10]))

  (is (= (with-open [r (io-core/reader common/test-sam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read r {})))
         [29 7 9 9 6 16 37 1 2 10]))

  (is (= (with-open [r (io-core/reader common/test-sam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {})))
         [29 7 9 9 6 16 37 1 2 10]))

  (is (= (with-open [r (io-core/reader common/test-sam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {:chr "ref2"})))
         [6 1 2 10 14 12]))

  (is (= (with-open [r (io-core/reader common/test-sam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {:chr "ref2" :start 10})))
         [6 1 2 10 14 12]))

  (is (= (with-open [r (io-core/reader common/test-sam-file)]
           (into [] (comp (map :pos) (take 10)) (io/read-in-region r {:chr "ref2" :start 10 :end 11})))
         [6 1 2 10])))

(deftest fasta-io
  (is
   (= (with-open [r (io-core/reader common/test-fa-file)]
        (doall (io/read r)))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"}]))

  (is
   (= (with-open [r (io-core/reader common/test-fa-file)]
        (doall (io/read r {})))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"}]))

  (is
   (= (with-open [r (io-core/reader common/test-fa-file)]
        (doall (io/read-all-sequences r)))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"}]))

  (is
   (= (with-open [r (io-core/reader common/test-fa-file)]
        (doall (io/read-all-sequences r {})))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"}]))

  (is
   (= (with-open [r (io-core/reader common/test-fa-file)]
        (io/read-in-region r {:chr "ref" :start 1 :end 10}))
      "AGCATGTTAG"))

  (is
   (= (with-open [r (io-core/reader common/test-fa-file)]
        (io/read-sequence r {:chr "ref" :start 1 :end 10}))
      "AGCATGTTAG")))

(deftest twobit-io
  (is
   (= (with-open [r (io-core/reader common/test-twobit-file)]
        (doall (io/read r)))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"}]))

  (is
   (= (with-open [r (io-core/reader common/test-twobit-file)]
        (doall (io/read r {:mask? true})))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "aggttttataaaacaattaagtctacagagcaactacgcg"}]))
  (is
   (= (with-open [r (io-core/reader common/test-twobit-file)]
        (doall (io/read-all-sequences r)))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"}]))

  (is
   (= (with-open [r (io-core/reader common/test-twobit-file)]
        (doall (io/read-all-sequences r {:mask? true})))
      [{:name "ref", :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
       {:name "ref2", :sequence "aggttttataaaacaattaagtctacagagcaactacgcg"}]))

  (is
   (= (with-open [r (io-core/reader common/test-twobit-file)]
        (io/read-in-region r {:chr "ref" :start 1 :end 10}))
      "AGCATGTTAG"))

  (is
   (= (with-open [r (io-core/reader common/test-twobit-file)]
        (io/read-sequence r {:chr "ref" :start 1 :end 10}))
      "AGCATGTTAG")))

(deftest fastq-io
  (is
   (= (with-open [r (io-core/reader common/test-fq-file)]
        (->> (io/read r) (map #(into {} %)) doall))
      common/test-fq-sequences))

  (is
   (= (with-open [r (io-core/reader common/test-fq-file)]
        (->> (io/read r {:decode-quality nil})
             (map #(into {} %))
             doall))
      common/test-fq-sequences-raw)))

(deftest vcf-io
  (is
   (= (with-open [r (io-core/reader common/test-vcf-v4_3-file)]
        (doall (io/read r)))
      common/test-vcf-v4_3-variants-deep))

  (is
   (= (with-open [r (io-core/reader common/test-vcf-v4_3-file)]
        (doall (io/read-in-region r {:chr "20"})))
      common/test-vcf-v4_3-variants-deep))

  (is
   (= (with-open [r (io-core/reader common/test-vcf-v4_3-file)]
        (doall (io/read-in-region r {:chr "20" :start 1 :end 20000})))
      (take 2 common/test-vcf-v4_3-variants-deep))))

(deftest bcf-io
  (is
   (= (with-open [r (io-core/reader common/test-bcf-v4_3-file)]
        (doall (io/read r)))
      common/test-vcf-v4_3-variants-deep))

  (is
   (= (with-open [r (io-core/reader common/test-bcf-v4_3-file)]
        (doall (io/read-in-region r {:chr "20"})))
      common/test-vcf-v4_3-variants-deep))

  (is
   (= (with-open [r (io-core/reader common/test-bcf-v4_3-file)]
        (doall (io/read-in-region r {:chr "20" :start 1 :end 20000})))
      (take 2 common/test-vcf-v4_3-variants-deep))))

(deftest bed-io
  (is
   (= (with-open [r (io-core/reader common/test-bed-file1)]
        (doall (io/read r)))
      [{:chr "chr22" :start 1001 :end 5000 :name "cloneA" :score 960 :strand :plus :thick-start 1001
        :thick-end 5000 :item-rgb "0" :block-count 2 :block-sizes [567 488] :block-starts [0 3512]}
       {:chr "chr22" :start 2001 :end 6000 :name "cloneB" :score 900 :strand :minus :thick-start 2001
        :thick-end 6000 :item-rgb "0" :block-count 2 :block-sizes [433 399] :block-starts [0 3601]}])))
