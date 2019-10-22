(ns cljam.io.tabix-test
  (:require [clojure.java.io :as cio]
            [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.tabix :as tbi])
  (:import
   [cljam.io.tabix Tabix]
   [cljam.io.util.chunk Chunk]))

(deftest about-read-index-with-error
  (is (thrown? java.io.IOException (tbi/read-index small-bam-file))))

(deftest about-read-index-returns-tabix-object
  (is (instance? Tabix (tbi/read-index test-tabix-file))))

(deftest about-read-index-check-the-returning-object
  (let [tabix-data ^Tabix (tbi/read-index test-tabix-file)]
    (is (number? (.n-ref tabix-data)))
    (is (number? (.preset tabix-data)))
    (is (number? (.sc tabix-data)))
    (is (number? (.bc tabix-data)))
    (is (number? (.ec tabix-data)))
    (is (number? (.meta tabix-data)))
    (is (number? (.skip tabix-data)))
    (is (vector? (.seq tabix-data)))
    (is (instance?  Chunk (get (get (get (.bidx tabix-data) 0) 4687) 0)))
    (is (vector?  (get (.lidx tabix-data) 0)))))

(deftest-remote large-file
  (with-before-after {:before (prepare-cavia!)}
    (is (not-throw? (tbi/read-index test-large-tabix-file)))))

(deftest source-type-test
  (with-open [server (http-server)]
    (are [x] (instance? Tabix (tbi/read-index x))
      test-tabix-file
      (cio/file test-tabix-file)
      (cio/as-url (cio/file test-tabix-file))
      (cio/as-url (str (:uri server) "/tabix/test.gtf.gz.tbi")))))
