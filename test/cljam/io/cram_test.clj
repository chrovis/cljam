(ns cljam.io.cram-test
  (:require [cljam.io.cram :as cram]
            [cljam.io.sam :as sam]
            [cljam.test-common :as common :refer [deftest-remote
                                                  prepare-cavia!
                                                  with-before-after]]
            [clojure.test :refer [are deftest is testing]]))

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
           (cram/read-alignments cram-rdr')))))

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
    (testing "read alignments in specified regions"
      (with-open [cram-rdr (cram/reader common/medium-cram-file
                                        {:reference common/hg19-twobit-file})]
        (is (cram/indexed? cram-rdr))
        (are [?region ?count] (= ?count (count (cram/read-alignments cram-rdr ?region)))
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
