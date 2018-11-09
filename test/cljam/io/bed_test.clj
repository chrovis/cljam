(ns cljam.io.bed-test
  (:require [clojure.java.io :as cio]
            [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.bed :as bed]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.sequence :as cseq])
  (:import [java.io BufferedReader InputStreamReader ByteArrayInputStream
            ByteArrayOutputStream OutputStreamWriter BufferedWriter]
           [cljam.io.bed BEDReader BEDWriter]))

(defn- str->bed [^String s]
  (with-open [bais (ByteArrayInputStream. (.getBytes s))
              isr (InputStreamReader. bais)
              br (bed/BEDReader. (BufferedReader. isr) nil)]
    (doall (bed/read-fields br))))

(defn- bed->str [xs]
  (with-open [bao (ByteArrayOutputStream.)
              osw (OutputStreamWriter. bao)
              bw (bed/BEDWriter. (BufferedWriter. osw) nil)]
    (bed/write-fields bw xs)
    (.flush ^BufferedWriter (.writer bw))
    (.toString bao)))

(deftest bed-file-reader-1
  (is (= (str->bed "1 0 100 N 0 + 0 0 255,0,0 2 10,90 0,10")
         [{:chr "1" :start 1 :end 100 :name "N" :score 0 :strand :forward :thick-start 1 :thick-end 0
           :item-rgb "255,0,0" :block-count 2 :block-sizes [10 90] :block-starts [0 10]}]))

  (with-open [r (bed/reader test-bed-file1)]
    (is (= (bed/read-fields r)
           [{:chr "chr22" :start 1001 :end 5000 :name "cloneA" :score 960 :strand :forward :thick-start 1001
             :thick-end 5000 :item-rgb "0" :block-count 2 :block-sizes [567 488] :block-starts [0 3512]}
            {:chr "chr22" :start 2001 :end 6000 :name "cloneB" :score 900 :strand :reverse :thick-start 2001
             :thick-end 6000 :item-rgb "0" :block-count 2 :block-sizes [433 399] :block-starts [0 3601]}])))

  (with-open [r (bed/reader test-bed-file1-gz)]
    (is (= (bed/read-fields r)
           [{:chr "chr22" :start 1001 :end 5000 :name "cloneA" :score 960 :strand :forward :thick-start 1001
             :thick-end 5000 :item-rgb "0" :block-count 2 :block-sizes [567 488] :block-starts [0 3512]}
            {:chr "chr22" :start 2001 :end 6000 :name "cloneB" :score 900 :strand :reverse :thick-start 2001
             :thick-end 6000 :item-rgb "0" :block-count 2 :block-sizes [433 399] :block-starts [0 3601]}])))

  (with-open [r (bed/reader test-bed-file2)]
    (is (= (bed/read-fields r)
           [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :forward :thick-start 127471197
             :thick-end 127472363 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :forward :thick-start 127472364
             :thick-end 127473530 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :forward :thick-start 127473531
             :thick-end 127474697 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :forward :thick-start 127474698
             :thick-end 127475864 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :reverse :thick-start 127475865
             :thick-end 127477031 :item-rgb "0,0,255"}
            {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :reverse :thick-start 127477032
             :thick-end 127478198 :item-rgb "0,0,255"}
            {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :reverse :thick-start 127478199
             :thick-end 127479365 :item-rgb "0,0,255"}
            {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :forward :thick-start 127479366
             :thick-end 127480532 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :reverse :thick-start 127480533
             :thick-end 127481699 :item-rgb "0,0,255"}])))

  (with-open [r (bed/reader test-bed-file2-bz2)]
    (is (= (bed/read-fields r)
           [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :forward :thick-start 127471197
             :thick-end 127472363 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :forward :thick-start 127472364
             :thick-end 127473530 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :forward :thick-start 127473531
             :thick-end 127474697 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :forward :thick-start 127474698
             :thick-end 127475864 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :reverse :thick-start 127475865
             :thick-end 127477031 :item-rgb "0,0,255"}
            {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :reverse :thick-start 127477032
             :thick-end 127478198 :item-rgb "0,0,255"}
            {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :reverse :thick-start 127478199
             :thick-end 127479365 :item-rgb "0,0,255"}
            {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :forward :thick-start 127479366
             :thick-end 127480532 :item-rgb "255,0,0"}
            {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :reverse :thick-start 127480533
             :thick-end 127481699 :item-rgb "0,0,255"}])))

  (with-open [r (bed/reader test-bed-file3)]
    (is (= (bed/read-fields r)
           [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :forward}
            {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :forward}
            {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :forward}
            {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :forward}
            {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :reverse}
            {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :reverse}
            {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :reverse}
            {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :forward}
            {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :reverse}]))))

(deftest bed-file-reader-2
  (are [?bed-str ?expected] (= (str->bed ?bed-str) ?expected)
    ""                []
    "#"               []
    "track name=foo"  []
    "browser"         []
    "1 0 1"           [{:chr "1" :start 1 :end 1}]
    "1 0 1\n"         [{:chr "1" :start 1 :end 1}]
    "1\t0\t1"         [{:chr "1" :start 1 :end 1}]
    "1\t0\t1\n"       [{:chr "1" :start 1 :end 1}]
    "chr1 0 1"        [{:chr "chr1" :start 1 :end 1}]
    "1 0 1\n1 1 2"    [{:chr "1" :start 1 :end 1} {:chr "1" :start 2 :end 2}]
    "1 0 1 Name"      [{:chr "1" :start 1 :end 1 :name "Name"}]
    "1 0 1 N 0"       [{:chr "1" :start 1 :end 1 :name "N" :score 0}]
    "1 0 1 N 0 +"     [{:chr "1" :start 1 :end 1 :name "N" :score 0 :strand :forward}]
    "1 0 1 N 0 + 0"   [{:chr "1" :start 1 :end 1 :name "N" :score 0 :strand :forward :thick-start 1}]
    "1 0 1 N 0 + 0 1" [{:chr "1" :start 1 :end 1 :name "N" :score 0 :strand :forward :thick-start 1 :thick-end 1}]))

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
         [{:chr "chr1" :start 1 :end 20} {:chr "chr2" :start 1 :end 10}]))
  (is (= (bed/merge-fields [{:chr "chr2" :start 1 :end 10} {:chr "chr1" :start 1 :end 10} {:chr "chr1" :start 4 :end 13}
                            {:chr "chr1" :start 14 :end 15} {:chr "chr1" :start 16 :end 20}])
         [{:chr "chr1" :start 1 :end 20} {:chr "chr2" :start 1 :end 10}]))
  (is (= (bed/merge-fields [{:chr "chr2" :start 1 :end 10} {:chr "chr1" :start 1 :end 10} {:chr "chr1" :start 4 :end 13}
                            {:chr "chr1" :start 15 :end 15} {:chr "chr1" :start 16 :end 20}])
         [{:chr "chr1" :start 1 :end 13} {:chr "chr1" :start 15 :end 20} {:chr "chr2" :start 1 :end 10}]))
  (is (= (bed/merge-fields [{:chr "chr1" :start 1 :end 10 :name "chr1:1-10"} {:chr "chr1" :start 4 :end 13 :name "chr1:4-13"}])
         [{:chr "chr1" :start 1 :end 13 :name "chr1:1-10+chr1:4-13"}])))

(deftest bed-intersect
  (are [?xs ?ys ?result] (= ?result (bed/intersect-fields ?xs ?ys))
    []
    []
    []

    [{:chr "chr1" :start 10 :end 40}]
    []
    []

    []
    [{:chr "chr1" :start 10 :end 40}]
    []

    [{:chr "chr1" :start 10 :end 40}]
    [{:chr "chr1" :start 50 :end 60}]
    []

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80}]
    [{:chr "chr1" :start 5 :end 20}]
    [{:chr "chr1" :start 10 :end 20}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80}]
    [{:chr "chr1" :start 20 :end 30}]
    [{:chr "chr1" :start 20 :end 30}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80}]
    [{:chr "chr1" :start 30 :end 60} {:chr "chr1" :start 70 :end 90}]
    [{:chr "chr1" :start 30 :end 40} {:chr "chr1" :start 50 :end 60} {:chr "chr1" :start 70 :end 80}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80} {:chr "chr2" :start 10 :end 40}]
    [{:chr "chr1" :start 10 :end 20} {:chr "chr2" :start 30 :end 40}]
    [{:chr "chr1" :start 10 :end 20} {:chr "chr2" :start 30 :end 40}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 10 :end 50}]
    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 30 :end 50}]
    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 10 :end 50}]))

(deftest bed-subtract
  (are [?xs ?ys ?result] (= ?result (bed/subtract-fields ?xs ?ys))
    []
    []
    []

    [{:chr "chr1" :start 10 :end 40}]
    []
    [{:chr "chr1" :start 10 :end 40}]

    []
    [{:chr "chr1" :start 10 :end 40}]
    []

    [{:chr "chr1" :start 10 :end 40}]
    [{:chr "chr1" :start 50 :end 60}]
    [{:chr "chr1" :start 10 :end 40}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80}]
    [{:chr "chr1" :start 5 :end 20}]
    [{:chr "chr1" :start 21 :end 40} {:chr "chr1" :start 50 :end 80}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80}]
    [{:chr "chr1" :start 20 :end 30}]
    [{:chr "chr1" :start 10 :end 19} {:chr "chr1" :start 31 :end 40} {:chr "chr1" :start 50 :end 80}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80}]
    [{:chr "chr1" :start 30 :end 60} {:chr "chr1" :start 70 :end 90}]
    [{:chr "chr1" :start 10 :end 29} {:chr "chr1" :start 61 :end 69}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 50 :end 80} {:chr "chr2" :start 10 :end 40}]
    [{:chr "chr1" :start 10 :end 20} {:chr "chr2" :start 5 :end 30}]
    [{:chr "chr1" :start 21 :end 40} {:chr "chr1" :start 50 :end 80} {:chr "chr2" :start 31 :end 40}]

    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 10 :end 50}]
    [{:chr "chr1" :start 10 :end 40} {:chr "chr1" :start 30 :end 50}]
    []))

(deftest bed-complement
  (are [?xs ?result]
      (= ?result
         (bed/complement-fields [{:name "chr1" :len 1000}
                                 {:name "chr2" :len 800}]
                                ?xs))
    []
    [{:chr "chr1" :start 1 :end 1000} {:chr "chr2" :start 1 :end 800}]

    [{:chr "chr1" :start 1 :end 300}]
    [{:chr "chr1" :start 301 :end 1000} {:chr "chr2" :start 1 :end 800}]

    [{:chr "chr1" :start 1 :end 300} {:chr "chr1" :start 900 :end 1000} {:chr "chr2" :start 1 :end 300}]
    [{:chr "chr1" :start 301 :end 899} {:chr "chr2" :start 301 :end 800}]

    [{:chr "chr1" :start 1 :end 300} {:chr "chr1" :start 200 :end 500}]
    [{:chr "chr1" :start 501 :end 1000} {:chr "chr2" :start 1 :end 800}])

  (is (thrown? IllegalArgumentException
               (bed/complement-fields [{:name "chr1" :len 1000}]
                                      [{:chr "chr2" :start 1 :end 100}]))))

(deftest bed-reader-and-bam-reader
  (with-open [bam (sam/bam-reader test-sorted-bam-file)]
    (letfn [(ref-pos-end [m] {:rname (:rname m) :pos (:pos m) :end (sam-util/get-end m)})
            (read-region [s] (->> (str->bed s) (mapcat #(sam/read-alignments bam %)) (map ref-pos-end)))]
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
  (with-open [fa (cseq/fasta-reader medium-fa-file)]
    (comment "chr1" "TAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCCTAACCC...")
    (letfn [(read-region [s] (->> (str->bed s) (map #(cseq/read-sequence fa %))))]
      (are [?region-str ?result] (= (read-region ?region-str) ?result)
        "chr1 0 1" ["T"]
        "chr1 0 10" ["TAACCCTAAC"]
        "chr1 0 10\nchr1 10 20" ["TAACCCTAAC" "CCTAACCCTA"]))))

(deftest bed-writer
  (is (= (bed->str (str->bed "1 0 1")) "1\t0\t1"))
  (is (= (bed->str (str->bed "1 0 10")) "1\t0\t10"))
  (is (= (bed->str (str->bed "1 0 1\n1 1 2")) "1\t0\t1\n1\t1\t2"))
  (is (= (with-open [r (bed/reader test-bed-file1)] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (bed/reader test-bed-file1)] (doall (bed/read-fields r)))))
  (is (= (with-open [r (bed/reader test-bed-file1-gz)] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (bed/reader test-bed-file1-gz)] (doall (bed/read-fields r)))))
  (is (= (with-open [r (bed/reader test-bed-file2)] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (bed/reader test-bed-file2)] (doall (bed/read-fields r)))))
  (is (= (with-open [r (bed/reader test-bed-file2-bz2)] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (bed/reader test-bed-file2-bz2)] (doall (bed/read-fields r)))))
  (is (= (with-open [r (bed/reader test-bed-file3)] (str->bed (bed->str (bed/read-fields r))))
         (with-open [r (bed/reader test-bed-file3)] (doall (bed/read-fields r)))))

  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (str temp-dir "/test1.bed")
          temp-file-gz (str temp-dir "/test1.bed.gz")
          temp-file-bz2 (str temp-dir "/test1.bed.bz2")
          xs (with-open [r (bed/reader test-bed-file1)] (doall (bed/read-fields r)))]
      (with-open [wtr1 (bed/writer temp-file)
                  wtr2 (bed/writer temp-file-gz)
                  wtr3 (bed/writer temp-file-bz2)]
        (bed/write-fields wtr1 xs)
        (bed/write-fields wtr2 xs)
        (bed/write-fields wtr3 xs))
      (with-open [rdr1 (bed/reader temp-file)
                  rdr2 (bed/reader temp-file-gz)
                  rdr3 (bed/reader temp-file-bz2)]
        (is (= xs
               (bed/read-fields rdr1)))
        (is (= xs
               (bed/read-fields rdr2)))
        (is (= xs
               (bed/read-fields rdr3)))))))

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [x] (with-open [rdr (bed/reader x)]
                 (= (bed/read-fields rdr)
                    [{:chr "chr22" :start 1001 :end 5000 :name "cloneA" :score 960 :strand :forward :thick-start 1001
                      :thick-end 5000 :item-rgb "0" :block-count 2 :block-sizes [567 488] :block-starts [0 3512]}
                     {:chr "chr22" :start 2001 :end 6000 :name "cloneB" :score 900 :strand :reverse :thick-start 2001
                      :thick-end 6000 :item-rgb "0" :block-count 2 :block-sizes [433 399] :block-starts [0 3601]}]))
        test-bed-file1
        (cio/file test-bed-file1)
        (cio/as-url (cio/file test-bed-file1))
        (cio/as-url (str (:uri server) "/bed/test1.bed")))))

  (testing "writer"
    (let [temp-file (str temp-dir "/test1.bed")]
      (are [x] (with-before-after {:before (prepare-cache!)
                                   :after (clean-cache!)}
                 (with-open [rdr (bed/reader test-bed-file1)
                             wtr (bed/writer temp-file)]
                   (not-throw? (bed/write-fields wtr (bed/read-fields rdr)))))
        temp-file
        (cio/file temp-file)
        (cio/as-url (cio/file temp-file))))))
