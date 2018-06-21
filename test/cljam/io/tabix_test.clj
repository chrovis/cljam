(ns cljam.io.tabix-test
  (:require [clojure.java.io :as cio]
            [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.tabix :as tbi]))

(deftest about-read-index-with-error
  (is (thrown? java.io.IOException (tbi/read-index small-bam-file))))

(deftest about-read-index-returns-a-map
  (is (map? (tbi/read-index test-tabix-file))))

(deftest about-read-index-check-the-returning-maps-structure
  (is (just-map? {:n-seq number?
                  :preset number?
                  :sc number?
                  :bc number?
                  :ec number?
                  :meta number?
                  :skip number?
                  :seq vector?
                  :bin-index vector?
                  :linear-index vector?}
                 (tbi/read-index test-tabix-file))))

(deftest-remote large-file
  (with-before-after {:before (prepare-cavia!)}
    (is (not-throw? (tbi/read-index test-large-tabix-file)))))

(deftest source-type-test
  (with-open [server (http-server)]
    (are [x] (map? (tbi/read-index x))
      test-tabix-file
      (cio/file test-tabix-file)
      (cio/as-url (cio/file test-tabix-file))
      (cio/as-url (str (:uri server) "/tabix/test.gtf.gz.tbi")))))
