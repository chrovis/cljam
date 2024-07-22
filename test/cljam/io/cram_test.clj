(ns cljam.io.cram-test
  (:require [cljam.io.cram :as cram]
            [cljam.io.sam :as sam]
            [cljam.test-common :as common :refer [clean-cache! deftest-remote
                                                  prepare-cache!
                                                  prepare-cavia!
                                                  with-before-after]]
            [clojure.java.io :as io]
            [clojure.test :refer [are deftest is testing]]))

(def ^:private temp-cram-file (io/file common/temp-dir "test.cram"))

(defn- fixup-bam-aln [aln]
  (-> (into {} aln)
      (update :cigar #(if (= % "") "*" %))
      (update :options #(sort-by (comp name key first) %))))

(deftest reader-test
  (with-open [bam-rdr (sam/reader common/test-bam-file)
              cram-rdr (cram/reader common/test-cram-file
                                    {:reference common/test-fa-file})
              cram-rdr' (cram/reader cram-rdr)]
    (is (not (cram/indexed? cram-rdr)))
    (is (= (sam/read-header bam-rdr)
           (dissoc (cram/read-header cram-rdr) :HD)
           (dissoc (cram/read-header cram-rdr') :HD)))
    (is (= (sam/read-refs bam-rdr)
           (cram/read-refs cram-rdr)
           (cram/read-refs cram-rdr')))
    (is (= (map fixup-bam-aln (sam/read-alignments bam-rdr))
           (cram/read-alignments cram-rdr)
           (cram/read-alignments cram-rdr')))
    (are [?region ?count] (= ?count
                             (count (cram/read-alignments cram-rdr ?region))
                             (count (cram/read-alignments cram-rdr' ?region)))
      {:chr "ref"} 6
      {:chr "ref2", :start 35, :end 35} 2)))

(deftest-remote reader-with-multiple-containers-test
  (with-before-after {:before (prepare-cavia!)}
    (testing "read all the alignments"
      (with-open [bam-rdr (sam/reader common/medium-bam-file)
                  cram-rdr (cram/reader common/medium-cram-file
                                        {:reference common/hg19-twobit-file})]
        (is (= (sam/read-header bam-rdr)
               (dissoc (cram/read-header cram-rdr) :HD)))
        (is (= (sam/read-refs bam-rdr)
               (cram/read-refs cram-rdr)))
        (is (= (map fixup-bam-aln (sam/read-alignments bam-rdr))
               (cram/read-alignments cram-rdr)))))
    (testing "read alignments in specified regions (with and without index file)"
      (with-open [cram-rdr (cram/reader common/medium-cram-file
                                        {:reference common/hg19-twobit-file})
                  cram-rdr' (cram/reader common/medium-without-index-cram-file
                                         {:reference common/hg19-twobit-file})]
        (is (cram/indexed? cram-rdr))
        (is (not (cram/indexed? cram-rdr')))
        (are [?region ?count] (= ?count
                                 (count (cram/read-alignments cram-rdr ?region))
                                 (count (cram/read-alignments cram-rdr' ?region)))
          {:chr "chr1"} 615
          {:chr "*"} 4348
          {:chr "chr1", :start 546610, :end 546610} 1
          ;; region crosses over slice boundary
          {:chr "chr1", :start 205500000, :end 209000000} 4
          ;; chr starts and ends in the middle of slice
          {:chr "chr14", :end 21234329} 10
          {:chr "chr14", :start 105661859} 10
          ;; region crosses over container boundary
          {:chr "chr19", :start 54000000, :end 55000000} 12)))))

(deftest writer-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [r (cram/reader common/test-cram-file
                               {:reference common/test-fa-file})
                w (cram/writer temp-cram-file
                               {:reference common/test-fa-file})]
      (cram/write-header w (cram/read-header r))
      (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
    (with-open [r (cram/reader common/test-cram-file
                               {:reference common/test-fa-file})
                r' (cram/reader temp-cram-file
                                {:reference common/test-fa-file})]
      (is (= (cram/read-header r)
             (cram/read-header r')))
      (is (= (cram/read-alignments r)
             (cram/read-alignments r'))))))

(deftest-remote writer-with-multiple-containers-test
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!))
                      :after (clean-cache!)}
    (with-open [r (cram/reader common/medium-cram-file
                               {:reference common/hg19-twobit-file})
                w (cram/writer temp-cram-file
                               {:reference common/hg19-twobit-file})]
      (cram/write-header w (cram/read-header r))
      (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
    (with-open [r (cram/reader common/medium-cram-file
                               {:reference common/hg19-twobit-file})
                r' (cram/reader temp-cram-file
                                {:reference common/hg19-twobit-file})]
      (is (= (cram/read-header r)
             (cram/read-header r')))
      (is (= (cram/read-alignments r)
             (cram/read-alignments r'))))))
