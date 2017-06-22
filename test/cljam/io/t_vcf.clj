(ns cljam.io.t-vcf
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.t-common :refer :all]
            [cljam.io.vcf :as vcf]
            [cljam.util :as util])
  (:import bgzf4j.BGZFException))

(def ^:private temp-file (str temp-dir "/test.vcf"))

(defn- slurp-vcf-for-test
  [f & [depth]]
  (with-open [rdr (vcf/reader f)]
    {:meta-info (vcf/meta-info rdr)
     :header (vcf/header rdr)
     :variants (doall (vcf/read-variants rdr {:depth (or depth :deep)}))}))

(defn- spit-vcf-for-test
  [f meta-info header variants]
  (with-open [wtr (vcf/writer f meta-info header)]
    (vcf/write-variants wtr variants)))

(deftest bcf-reader-test
  (is (thrown? BGZFException
               (with-open [r (vcf/bcf-reader test-vcf-v4_3-file)] nil)))
  (is (thrown? java.io.IOException
               (with-open [r (vcf/bcf-reader test-bam-file)] nil)))
  (is (thrown? java.io.IOException
               (with-open [r (vcf/bcf-reader test-bcf-invalid-file)] nil))))

(deftest reader-test
  (testing "vcf"
    (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
      (is (instance? cljam.io.vcf.reader.VCFReader rdr))))
  (testing "bcf"
    (with-open [rdr (vcf/reader test-bcf-v4_3-file)]
      (is (instance? cljam.io.bcf.reader.BCFReader rdr))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (vcf/reader f))
      "test-resources/vcf/not-found.vcf"
      "test-resources/bcf/not-found.bcf")))

(deftest meta-info-vcf-test
  (testing "VCF v4.0"
    (with-open [rdr (vcf/vcf-reader test-vcf-v4_0-file)]
      (is (= (vcf/meta-info rdr) test-vcf-v4_0-meta-info))))
  (testing "VCF v4.3"
    (with-open [rdr (vcf/vcf-reader test-vcf-v4_3-file)]
      (is (= (vcf/meta-info rdr) test-vcf-v4_3-meta-info)))))

(deftest meta-info-bcf-test
  (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)]
    (is (= (vcf/meta-info r) test-vcf-v4_3-meta-info))))

(deftest header-vcf-test
  (testing "VCF v4.0"
    (with-open [rdr (vcf/vcf-reader test-vcf-v4_0-file)]
      (is (= (vcf/header rdr) test-vcf-v4_0-header))))
  (testing "VCF v4.3"
    (with-open [rdr (vcf/vcf-reader test-vcf-v4_3-file)]
      (is (= (vcf/header rdr) test-vcf-v4_3-header)))))

(deftest header-bcf-test
  (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)]
    (is (= (vcf/header r) test-vcf-v4_3-header))))

(deftest read-variants-vcf-test
  (testing "VCF v4.0"
    (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
      (is (= (vcf/read-variants rdr) test-vcf-v4_0-variants-deep))))
  (testing "VCF v4.3"
    (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
      (is (= (vcf/read-variants rdr) test-vcf-v4_3-variants-deep)))))

(deftest read-variants-bcf-test
  (testing "default"
    (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)]
      (doseq [[v1 v2] (map vector (vcf/read-variants r) test-vcf-v4_3-variants-deep)]
        (is (= v1 v2)))))
  (testing ":depth :vcf"
    (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)]
      (is (=  (doall (vcf/read-variants r {:depth :vcf})) test-vcf-v4_3-variants))))
  (testing ":depth :deep"
    (with-open [r  (vcf/bcf-reader test-bcf-v4_3-file)]
      (doseq [[v1 v2] (map vector (vcf/read-variants r {:depth :deep}) test-vcf-v4_3-variants-deep)]
        (is (= v1 v2)))))
  (testing ":depth :shallow"
    (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)]
      (doseq [[v1 v2] (map vector
                           (map (juxt :chr :pos :rlen) (vcf/read-variants r {:depth :shallow}))
                           (map (fn [v] [(:chr v) (:pos v) (count (:ref v))]) test-vcf-v4_3-variants))]
        (is (= v1 v2)))))
  (testing ":depth :bcf"
    (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)]
      (doseq [[v1 v2] (map vector
                           (map (juxt :chr :pos :id :ref :alt) (vcf/read-variants r {:depth :bcf}))
                           (map (fn [v] [0 (:pos v) (:id v) (:ref v) (:alt v)]) test-vcf-v4_3-variants-deep))]
        (is (= v1 v2))))))

(deftest writer-test
  (testing "vcf"
    (with-open [wtr (vcf/writer (.getAbsolutePath (cio/file util/temp-dir "temp.vcf")) {} [])]
      (is (instance? cljam.io.vcf.writer.VCFWriter wtr))))
  (testing "bcf"
    (with-open [wtr (vcf/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bcf")) {} [])]
      (is (instance? cljam.io.bcf.writer.BCFWriter wtr))))
  (testing "throws Exception"
    (is (thrown? Exception
                 (vcf/writer (.getAbsolutePath (cio/file util/temp-dir "temp.vccf")) {} [])))))

(deftest about-writing-vcf-v4_0-deep
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; In (nth test-vcf-v4_0-variants-deep 10),
    ;; "./.:.:." => "./."
    ;; "0|2:3:." => "0|2:3"
    ;; trailing fields can be dropped. Result differs but it's OK.
    (is (not-throw? (spit-vcf-for-test temp-file
                                       test-vcf-v4_0-meta-info
                                       test-vcf-v4_0-header
                                       (take 10 test-vcf-v4_0-variants-deep))))
    (is (= (slurp-vcf-for-test temp-file :deep)
           {:meta-info test-vcf-v4_0-meta-info
            :header test-vcf-v4_0-header
            :variants (take 10 test-vcf-v4_0-variants-deep)}))))

(deftest about-writing-vcf-v4_0-vcf
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-vcf-for-test temp-file
                                       test-vcf-v4_0-meta-info
                                       test-vcf-v4_0-header
                                       test-vcf-v4_0-variants)))
    (is (= (slurp-vcf-for-test temp-file :vcf)
           {:meta-info test-vcf-v4_0-meta-info
            :header test-vcf-v4_0-header
            :variants test-vcf-v4_0-variants}))))

(deftest about-writing-vcf-v4_3-deep
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-vcf-for-test temp-file test-vcf-v4_3-meta-info test-vcf-v4_3-header test-vcf-v4_3-variants-deep)))
    (is (= (slurp-vcf-for-test temp-file :deep)
           {:meta-info test-vcf-v4_3-meta-info
            :header test-vcf-v4_3-header
            :variants test-vcf-v4_3-variants-deep}))))

(deftest about-writing-vcf-v4_3-vcf
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-vcf-for-test temp-file test-vcf-v4_3-meta-info test-vcf-v4_3-header test-vcf-v4_3-variants-deep)))
    (is (= (slurp-vcf-for-test temp-file :vcf)
           {:meta-info test-vcf-v4_3-meta-info
            :header test-vcf-v4_3-header
            :variants test-vcf-v4_3-variants}))))

(deftest about-read-write-bcf
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.bcf"))]
      (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)
                  w (vcf/bcf-writer temp-file (vcf/meta-info r) (vcf/header r))]
        (vcf/write-variants w (vcf/read-variants r)))
      (with-open [r1 (vcf/bcf-reader test-bcf-v4_3-file)
                  r2 (vcf/bcf-reader temp-file)]
        (is (= (vcf/header r2) (vcf/header r1)))
        (is (= (vcf/meta-info r2) (vcf/meta-info r1)))
        (doseq [[x2 x1] (map vector (vcf/read-variants r2) (vcf/read-variants r1))]
          (is (= x2 x1)))))))

(deftest vcf->bcf-conversion-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.bcf"))]
      (with-open [r (vcf/reader test-vcf-v4_3-file)
                  w (vcf/bcf-writer temp-file (vcf/meta-info r) (vcf/header r))]
        (vcf/write-variants w (vcf/read-variants r)))
      (with-open [r1 (vcf/reader test-vcf-v4_3-file)
                  r2 (vcf/bcf-reader temp-file)]
        (is (= (vcf/header r2) (vcf/header r1)))
        (is (= (vcf/meta-info r2) (vcf/meta-info r1)))
        (doseq [[x2 x1] (map vector (vcf/read-variants r2) (vcf/read-variants r1))]
          (is (= x2 x1)))))))

(deftest bcf->vcf-conversion-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.vcf"))]
      (with-open [r (vcf/bcf-reader test-bcf-v4_3-file)
                  w (vcf/writer temp-file (vcf/meta-info r) (vcf/header r))]
        (vcf/write-variants w (vcf/read-variants r)))
      (with-open [r1 (vcf/reader test-vcf-v4_3-file)
                  r2 (vcf/reader temp-file)]
        (is (= (vcf/header r2) (vcf/header r1)))
        (is (= (vcf/meta-info r2) (vcf/meta-info r1)))
        (doseq [[x2 x1] (map vector (vcf/read-variants r2) (vcf/read-variants r1))]
          (is (= x2 x1)))))))
