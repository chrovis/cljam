(ns cljam.t-vcf
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.vcf :as vcf]))

(def ^:private temp-file (str temp-dir "/test.vcf"))

(defn- slurp-vcf-for-test
  [f]
  (with-open [rdr (vcf/reader f)]
    {:meta-info (vcf/meta-info rdr)
     :header (vcf/header rdr)
     :variants (doall (vcf/read-variants rdr))}))

(defn- spit-vcf-for-test
  [f meta-info header variants]
  (with-open [wtr (vcf/writer f meta-info header)]
    (vcf/write-variants wtr variants)))

(deftest meta-info-returns-meta-information-of-the-vcf-v4_0-as-a-map
  (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
    (is (= (vcf/meta-info rdr) test-vcf-v4_0-meta-info))))

(deftest meta-info-returns-meta-information-of-the-vcf-v4_3-as-a-map
  (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
    (is (= (vcf/meta-info rdr) test-vcf-v4_3-meta-info))))

(deftest header-returns-header-line-of-the-vcf-v4_0-as-a-vector
  (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
    (is (= (vcf/header rdr) test-vcf-v4_0-header))))

(deftest header-returns-header-line-of-the-vcf-v4_3-as-a-vector
  (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
    (is (= (vcf/header rdr) test-vcf-v4_3-header))))

(deftest read-variants-returns-data-lines-of-the-vcf-v4_0-as-a-lazy-sequence
  (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
    (is (= (vcf/read-variants rdr) test-vcf-v4_0-variants))))

(deftest read-variants-returns-data-lines-of-the-vcf-v4_3-as-a-lazy-sequence
  (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
    (is (= (vcf/read-variants rdr) test-vcf-v4_3-variants))))

(deftest about-writing-vcf-v4_0
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-vcf-for-test temp-file test-vcf-v4_0-meta-info test-vcf-v4_0-header test-vcf-v4_0-variants)))
    (is (= (slurp-vcf-for-test temp-file)
           {:meta-info test-vcf-v4_0-meta-info
            :header test-vcf-v4_0-header
            :variants test-vcf-v4_0-variants}))))

(deftest about-writing-vcf-v4_3
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-vcf-for-test temp-file test-vcf-v4_3-meta-info test-vcf-v4_3-header test-vcf-v4_3-variants)))
    (is (= (slurp-vcf-for-test temp-file)
           {:meta-info test-vcf-v4_3-meta-info
            :header test-vcf-v4_3-header
            :variants test-vcf-v4_3-variants}))))
