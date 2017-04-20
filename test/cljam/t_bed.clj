(ns cljam.t-bed
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [cljam.bed :as bed]
            [cljam.fasta :as fa]
            [cljam.io :as cio]
            [cljam.bam :as bam]
            [cljam.util.sam-util :as sam-util])
  (:import [java.io BufferedReader InputStreamReader ByteArrayInputStream
            ByteArrayOutputStream OutputStreamWriter BufferedWriter]))

(defn- str->bed [^String s]
  (with-open [bais (ByteArrayInputStream. (.getBytes s))
              isr (InputStreamReader. bais)
              br (BufferedReader. isr)]
    (doall (bed/read-fields br))))

(defn- bed->str [xs]
  (with-open [bao (ByteArrayOutputStream.)
              osw (OutputStreamWriter. bao)
              bw (BufferedWriter. osw)]
    (bed/write-fields bw xs)
    (.flush bw)
    (.toString bao)))

(defn- raw-str->bed [^String s]
  (with-open [bais (ByteArrayInputStream. (.getBytes s))
              isr (InputStreamReader. bais)
              br (BufferedReader. isr)]
    (doall (bed/read-raw-fields br))))

(defn- bed->raw-str [xs]
  (with-open [bao (ByteArrayOutputStream.)
              osw (OutputStreamWriter. bao)
              bw (BufferedWriter. osw)]
    (bed/write-raw-fields bw xs)
    (.flush bw)
    (.toString bao)))

(deftest bed-file-reader-1
  (is (= (raw-str->bed "1 0 100 N 0 + 0 0 255,0,0 2 10,90 0,10")
         [{:chr "1" :start 0 :end 100 :name "N" :score 0 :strand :plus :thick-start 0 :thick-end 0
           :item-rgb "255,0,0" :block-count 2 :block-sizes [10 90] :block-starts [0 10]}]))

  (is (= (str->bed "1 0 100 N 0 + 0 0 255,0,0 2 10,90 0,10")
         [{:chr "chr1" :start 1 :end 100 :name "N" :score 0 :strand :plus :thick-start 1 :thick-end 0
           :item-rgb "255,0,0" :block-count 2 :block-sizes [10 90] :block-starts [0 10]}]))

 (with-open [r (io/reader "test-resources/test1.bed")]
   (is (= (bed/read-fields r)
          [{:chr "chr22" :start 1001 :end 5000 :name "cloneA" :score 960 :strand :plus :thick-start 1001
            :thick-end 5000 :item-rgb "0" :block-count 2 :block-sizes [567 488] :block-starts [0 3512]}
           {:chr "chr22" :start 2001 :end 6000 :name "cloneB" :score 900 :strand :minus :thick-start 2001
            :thick-end 6000 :item-rgb "0" :block-count 2 :block-sizes [433 399] :block-starts [0 3601]}])))

 (with-open [r (io/reader "test-resources/test2.bed")]
   (is (= (bed/read-fields r)
          [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :plus :thick-start 127471197
            :thick-end 127472363 :item-rgb "255,0,0"}
           {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :plus :thick-start 127472364
            :thick-end 127473530 :item-rgb "255,0,0"}
           {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :plus :thick-start 127473531
            :thick-end 127474697 :item-rgb "255,0,0"}
           {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :plus :thick-start 127474698
            :thick-end 127475864 :item-rgb "255,0,0"}
           {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :minus :thick-start 127475865
            :thick-end 127477031 :item-rgb "0,0,255"}
           {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :minus :thick-start 127477032
            :thick-end 127478198 :item-rgb "0,0,255"}
           {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :minus :thick-start 127478199
            :thick-end 127479365 :item-rgb "0,0,255"}
           {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :plus :thick-start 127479366
            :thick-end 127480532 :item-rgb "255,0,0"}
           {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :minus :thick-start 127480533
            :thick-end 127481699 :item-rgb "0,0,255"}])))

  (with-open [r (io/reader "test-resources/test3.bed")]
    (is (= (bed/read-fields r)
           [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :plus}
            {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :plus}
            {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :plus}
            {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :plus}
            {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :minus}
            {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :minus}
            {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :minus}
            {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :plus}
            {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :minus}]))))

(deftest bed-file-reader-2
  (are [?bed-str ?expected] (= (str->bed ?bed-str) ?expected)
    ""                []
    "#"               []
    "track name=foo"  []
    "browser"         []
    "1 0 1"           [{:chr "chr1" :start 1 :end 1}]
    "1 0 1\n"         [{:chr "chr1" :start 1 :end 1}]
    "1\t0\t1"         [{:chr "chr1" :start 1 :end 1}]
    "1\t0\t1\n"       [{:chr "chr1" :start 1 :end 1}]
    "chr1 0 1"        [{:chr "chr1" :start 1 :end 1}]
    "1 0 1\n1 1 2"    [{:chr "chr1" :start 1 :end 1} {:chr "chr1" :start 2 :end 2}]
    "1 0 1 Name"      [{:chr "chr1" :start 1 :end 1 :name "Name"}]
    "1 0 1 N 0"       [{:chr "chr1" :start 1 :end 1 :name "N" :score 0}]
    "1 0 1 N 0 +"     [{:chr "chr1" :start 1 :end 1 :name "N" :score 0 :strand :plus}]
    "1 0 1 N 0 + 0"   [{:chr "chr1" :start 1 :end 1 :name "N" :score 0 :strand :plus :thick-start 1}]
    "1 0 1 N 0 + 0 1" [{:chr "chr1" :start 1 :end 1 :name "N" :score 0 :strand :plus :thick-start 1 :thick-end 1}]))

(deftest bed-reader-assertions
  (are [?bed-str] (thrown? java.lang.AssertionError (str->bed ?bed-str))
    "a"
    "\na"
    "trac"
    "1 0"
    "1 0 0"
    "1,0,1"
    "1 O 1"
    "chr 1 0 1"))

(deftest bed-sort
  (is (= (bed/sort-fields []) []))
  (is (= (bed/sort-fields [{:chr "chr1" :start 1 :end 20} {:chr "chrY" :start 2 :end 4} {:chr "chr10" :start 1 :end 100}
                           {:chr "chr1" :start 10 :end 20} {:chr "chr2" :start 20 :end 40} {:chr "chr1" :start 1 :end 3}])
         [{:chr "chr1" :start 1 :end 3} {:chr "chr1" :start 1 :end 20} {:chr "chr1" :start 10 :end 20}
          {:chr "chr2" :start 20 :end 40} {:chr "chr10" :start 1 :end 100} {:chr "chrY" :start 2 :end 4}]))
  (is (= (bed/sort-fields [{:chr "chr10" :start 1 :end 2} {:chr "chr1_KI270892v1_alt" :start 1 :end 2} {:chr "chr1" :start 2 :end 3}
                           {:chr "chr1" :start 1 :end 2} {:chr "chr10_KI270825v1_alt" :start 1 :end 2}
                           {:chr "chr1_KI270714v1_random" :start 1 :end 2} {:chr "chr2" :start 1 :end 2} {:chr "chrM" :start 1 :end 2}
                           {:chr "chr1_GL383518v1_alt" :start 1 :end 2} {:chr "another" :start 1 :end 2} {:chr "chrX" :start 1 :end 2}])
         [{:chr "chr1" :start 1 :end 2} {:chr "chr1" :start 2 :end 3} {:chr "chr1_GL383518v1_alt" :start 1 :end 2}
          {:chr "chr1_KI270714v1_random" :start 1 :end 2} {:chr "chr1_KI270892v1_alt" :start 1 :end 2}
          {:chr "chr2" :start 1 :end 2} {:chr "chr10" :start 1 :end 2} {:chr "chr10_KI270825v1_alt" :start 1 :end 2}
          {:chr "chrX" :start 1 :end 2} {:chr "chrM" :start 1 :end 2} {:chr "another" :start 1 :end 2}])))

(deftest bed-merge
  (is (= (bed/merge-fields []) []))
  (is (= (bed/merge-fields [{:chr "chr2" :start 1 :end 10} {:chr "chr1" :start 1 :end 10} {:chr "chr1" :start 4 :end 13}
                            {:chr "chr1" :start 13 :end 15} {:chr "chr1" :start 16 :end 20}])
         [{:chr "chr1" :start 1 :end 15} {:chr "chr1" :start 16 :end 20} {:chr "chr2" :start 1 :end 10}]))
  (is (= (bed/merge-fields [{:chr "chr1" :start 1 :end 10 :name "chr1:1-10"} {:chr "chr1" :start 4 :end 13 :name "chr1:4-13"}])
         [{:chr "chr1" :start 1 :end 13 :name "chr1:1-10+chr1:4-13"}])))

(deftest bed-reader-and-bam-reader
  (with-open [bam (bam/reader "test-resources/test.sorted.bam")]
    (letfn [(ref-pos-end [m] {:rname (:rname m) :pos (:pos m) :end (sam-util/get-end m)})
            (read-region [s] (->> (str->bed s) (mapcat #(cio/read-alignments bam %)) (map ref-pos-end)))]
      (are [?region-str ?result] (= (read-region ?region-str) ?result)
        "ref 0 6" []
        "ref 6 7" [{:rname "ref" :pos 7 :end 22}]
        "ref 7 8" [{:rname "ref" :pos 7 :end 22}]
        "ref 8 9" [{:rname "ref" :pos 7 :end 22}
                   {:rname "ref" :pos 9 :end 18}
                   {:rname "ref" :pos 9 :end 14}]
        "ref 21 22" [{:rname "ref" :pos 7 :end 22}
                     {:rname "ref" :pos 16 :end 40}]
        "ref 22 23" [{:rname "ref" :pos 16 :end 40}]
        "ref 0 45" [{:rname "ref" :pos 7  :end 22}
                    {:rname "ref" :pos 9  :end 18}
                    {:rname "ref" :pos 9  :end 14}
                    {:rname "ref" :pos 16 :end 40}
                    {:rname "ref" :pos 29 :end 33}
                    {:rname "ref" :pos 37 :end 45}]))))

(deftest bed-reader-and-fasta-reader
  (with-open [fa (fa/reader "test-resources/medium.fa")]
    (comment "chr1" "TAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCC...")
    (letfn [(read-region [s] (->> (str->bed s) (map #(fa/read-sequence fa %))))]
      (are [?region-str ?result] (= (read-region ?region-str) ?result)
        "1 0 1" ["T"]
        "1 0 10" ["TAACCCTAAC"]
        "1 0 10\n1 10 20" ["TAACCCTAAC" "CCTAACCCTA"]))))

(deftest bed-writer
  (is (= (bed->raw-str (raw-str->bed "1 0 10")) "1 0 10"))
  (is (= (bed->str (str->bed "1 0 1")) "chr1 0 1"))
  (is (= (bed->str (str->bed "1 0 10")) "chr1 0 10"))
  (is (= (bed->str (str->bed "1 0 1\n1 1 2")) "chr1 0 1\nchr1 1 2"))
  (is (= (with-open [r (io/reader "test-resources/test1.bed")] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (io/reader "test-resources/test1.bed")] (doall (bed/read-fields r)))))
  (is (= (with-open [r (io/reader "test-resources/test2.bed")] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (io/reader "test-resources/test2.bed")] (doall (bed/read-fields r)))))
  (is (= (with-open [r (io/reader "test-resources/test3.bed")] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (io/reader "test-resources/test3.bed")] (doall (bed/read-fields r))))))
