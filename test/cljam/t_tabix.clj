(ns cljam.t-tabix
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.tabix :as tbi]))

(deftest about-read-index-done-without-errors
  (is (not-throw? (tbi/read-index test-tabix-file))))

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

(deftest-slow-heavy large-file
  (with-before-after {:before (prepare-cavia!)}
    (is (not-throw? (tbi/read-index test-large-tabix-file)))))
