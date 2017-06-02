(ns cljam.t-bam
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :refer [copy file delete-file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-index.core :as bai-core]
            [cljam.sorter :as sorter])
  (:import [java.io IOException]))

(def temp-file (str temp-dir "/test.bam"))
(def temp-file-sorted (str temp-dir "/test.sorted.bam"))
(def not-found-file (str temp-dir "/not-found.bam"))
(def invalid-file-1 test-fa-file)
(def invalid-file-2 test-tabix-file)

(deftest slurp-bam
  (is (= (slurp-bam-for-test test-bam-file) test-sam)))

(deftest-slow slurp-bam-medium-file
  (is (not-throw? (slurp-bam-for-test medium-bam-file))))

;; NB: Cannot slurp large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy slurp-bam-large-file
;;   (with-before-after {:before (prepare-cavia!)}
;;     (is (not-throw? (slurp-bam-for-test large-bam-file)))))

(deftest spit-bam
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-file test-sam)))
    (is (= (slurp-bam-for-test temp-file) test-sam))))

(deftest-slow spit-bam-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy spit-bam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-bam-for-test temp-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(defn- shallow= [alns1 alns2]
  (= (map #(select-keys % [:rname :pos]) alns1)
     (map #(select-keys % [:rname :pos]) alns2)))

(defn- pointer= [alns1 alns2]
  (= (map #(select-keys % [:rname :pos :flag]) alns1)
     (map #(select-keys % [:rname :pos :flag]) alns2)))

(deftest bamreader
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-bam-for-test temp-file test-sam))
                      :after (clean-cache!)}
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (= (io/read-alignments rdr) (:alignments test-sam))))
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (io/read-alignments rdr {:chr "ref2"}))))
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (= (io/read-alignments rdr {:depth :deep}) (:alignments test-sam))))
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (shallow= (io/read-alignments rdr {:depth :shallow})
                    (:alignments test-sam))))
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (pointer= (io/read-alignments rdr {:depth :pointer})
                    (:alignments test-sam))))
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (= (data->clj (io/read-blocks rdr)) test-sam-data)))
    (with-open [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (data->clj (io/read-blocks rdr {:chr "ref2"})))))))

(deftest bamreader-with-index
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (io/read-alignments rdr {:chr "ref2"})
             (drop 6 (:alignments test-sam-sorted-by-pos)))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (io/read-alignments rdr {:chr "ref2" :start 21})
             (drop 7 (:alignments test-sam-sorted-by-pos)))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (io/read-alignments rdr {:chr "ref2" :end 9})
             (take 3 (drop 6 (:alignments test-sam-sorted-by-pos))))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (io/read-alignments rdr {:chr "ref2" :start 10 :end 12})
             (take 5 (drop 6 (:alignments test-sam-sorted-by-pos))))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (data->clj (io/read-blocks rdr))
             test-sorted-bam-data)))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (io/read-blocks rdr {:chr "ref2"})))
             (drop 6 test-sorted-bam-data))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (io/read-blocks rdr {:chr "ref2" :start 2})))
             (drop 7 test-sorted-bam-data))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (io/read-blocks rdr {:chr "ref2" :end 2})))
             (take 2 (drop 6 test-sorted-bam-data)))))
    (with-open [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (io/read-blocks rdr {:chr "ref2" :start 4 :end 12})))
             (take 3 (drop 8 test-sorted-bam-data)))))))

(deftest bamreader-invalid-files
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? Exception (bam/reader invalid-file-1 :ignore-index true)))
    (is (thrown? IOException (bam/reader invalid-file-2 :ignore-index true)))
    (is (thrown? IOException (bam/reader not-found-file :ignore-index true)))))

(deftest-slow bamreader-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [rdr (bam/reader medium-bam-file :ignore-index true)]
      (let [header (io/read-header rdr)
            refs (io/read-refs rdr)
            alns (io/read-alignments rdr)]
        (is (= refs medium-sam-refs))
        (with-open [w (bam/writer temp-file)]
          (is (not-throw? (io/write-header w header)))
          (is (not-throw? (io/write-refs w refs)))
          (is (not-throw? (io/write-alignments w alns header)))
          (same-file? medium-bam-file temp-file))))))

(deftest-remote bamreader-large-file
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-cavia!))
                      :after (clean-cache!)}
    (with-open [rdr (bam/reader large-bam-file :ignore-index true)]
      (is (= (io/read-refs rdr) large-sam-refs))
      (is (not-throw? (io/read-alignments rdr))))))
