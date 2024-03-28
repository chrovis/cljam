(ns cljam.io.cram-test
  (:require [cljam.io.cram :as cram]
            [cljam.io.sam :as sam]
            [cljam.test-common :as common :refer [deftest-remote
                                                  prepare-cavia!
                                                  with-before-after]]
            [clojure.test :refer [deftest is]]))

(defn- fixup-bam-aln [aln]
  (-> aln
      (dissoc :end)
      (update :cigar #(if (= % "") "*" %))
      (update :options #(sort-by (comp name key first) %))))

(deftest reader-test
  (with-open [bam-rdr (sam/reader common/test-bam-file)
              cram-rdr (cram/reader common/test-cram-file
                                    {:reference common/test-fa-file})
              cram-rdr' (cram/reader cram-rdr)]
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
    (with-open [bam-rdr (sam/reader common/medium-bam-file)
                cram-rdr (cram/reader common/medium-cram-file
                                      {:reference common/hg19-twobit-file})]
      (is (= (sam/read-header bam-rdr)
             (dissoc (cram/read-header cram-rdr) :HD)))
      (is (= (sam/read-refs bam-rdr)
             (cram/read-refs cram-rdr)))
      (is (= (map fixup-bam-aln (sam/read-alignments bam-rdr))
             (cram/read-alignments cram-rdr))))))
