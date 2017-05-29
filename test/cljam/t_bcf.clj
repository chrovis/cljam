(ns cljam.t-bcf
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :as cio]
            [cljam.bcf :as bcf]
            [cljam.vcf :as vcf])
  (:import [java.io IOException]
           [bgzf4j BGZFException]))

(deftest about-reading-bcf-header
  (is
   (= (with-open [r (bcf/reader test-bcf-v4_3-file)]
        (bcf/header r))
      test-vcf-v4_3-header)))

(deftest about-reader
  (is
   (thrown? BGZFException
            (with-open [r (bcf/reader test-vcf-v4_3-file)])))
  (is
   (thrown? IOException
            (with-open [r (bcf/reader test-bam-file)])))
  (is
   (thrown? IOException
            (with-open [r (bcf/reader test-bcf-invalid-file)]))))

(deftest about-reading-bcf-meta
  (is
   (= (with-open [r (bcf/reader test-bcf-v4_3-file)]
        (bcf/meta-info r))
      test-vcf-v4_3-meta-info)))

(deftest about-reading-bcf-variants
  (with-open [r (bcf/reader test-bcf-v4_3-file)]
    (doseq [[v1 v2] (map vector (bcf/read-variants r) test-vcf-v4_3-variants-deep)]
      (is
       (= v1 v2)))))

(deftest about-reading-bcf-variants-vcf
  (is
   (= (with-open [r (bcf/reader test-bcf-v4_3-file)]
        (doall (bcf/read-variants r {:depth :vcf})))
      test-vcf-v4_3-variants)))

(deftest about-reading-bcf-variants-deep
  (with-open [r  (bcf/reader test-bcf-v4_3-file)]
    (doseq [[v1 v2] (map vector (bcf/read-variants r {:depth :deep}) test-vcf-v4_3-variants-deep)]
      (is
       (= v1 v2)))))

(deftest about-reading-bcf-variants-shallow
  (with-open [r (bcf/reader test-bcf-v4_3-file)]
    (doseq [[v1 v2] (map vector
                         (map (juxt :chr :pos :rlen) (bcf/read-variants r {:depth :shallow}))
                         (map (fn [v] [(:chr v) (:pos v) (count (:ref v))]) test-vcf-v4_3-variants))]
      (is
       (= v1 v2)))))

(deftest about-reading-bcf-variants-bcf
  (with-open [r (bcf/reader test-bcf-v4_3-file)]
    (doseq [[v1 v2] (map vector
                         (map (juxt :chr :pos :id :ref :alt) (bcf/read-variants r {:depth :bcf}))
                         (map (fn [v] [0 (:pos v) (:id v) (:ref v) (:alt v)]) test-vcf-v4_3-variants-deep))]
      (is
       (= v1 v2)))))

(deftest about-read-write-bcf
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.bcf"))]
      (with-open [r (bcf/reader test-bcf-v4_3-file)
                  w (bcf/writer temp-file (bcf/meta-info r) (bcf/header r))]
        (bcf/write-variants w (bcf/read-variants r)))
      (with-open [r1 (bcf/reader test-bcf-v4_3-file)
                  r2 (bcf/reader temp-file)]
        (is
         (= (bcf/header r2) (bcf/header r1)))
        (is
         (= (bcf/meta-info r2) (bcf/meta-info r1)))
        (doseq [[x2 x1] (map vector (bcf/read-variants r2) (bcf/read-variants r1))]
          (is
           (= x2 x1)))))))

(deftest about-vcf->bcf-conversion
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.bcf"))]
      (with-open [r (vcf/reader test-vcf-v4_3-file)
                  w (bcf/writer temp-file (vcf/meta-info r) (vcf/header r))]
        (bcf/write-variants w (vcf/read-variants r)))
      (with-open [r1 (vcf/reader test-vcf-v4_3-file)
                  r2 (bcf/reader temp-file)]
        (is
         (= (bcf/header r2) (vcf/header r1)))
        (is
         (= (bcf/meta-info r2) (vcf/meta-info r1)))
        (doseq [[x2 x1] (map vector (bcf/read-variants r2) (vcf/read-variants r1))]
          (is
           (= x2 x1)))))))

(deftest about-bcf->vcf-conversion
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.vcf"))]
      (with-open [r (bcf/reader test-bcf-v4_3-file)
                  w (vcf/writer temp-file (bcf/meta-info r) (bcf/header r))]
        (vcf/write-variants w (bcf/read-variants r)))
      (with-open [r1 (vcf/reader test-vcf-v4_3-file)
                  r2 (vcf/reader temp-file)]
        (is
         (= (vcf/header r2) (vcf/header r1)))
        (is
         (= (vcf/meta-info r2) (vcf/meta-info r1)))
        (doseq [[x2 x1] (map vector (vcf/read-variants r2) (vcf/read-variants r1))]
          (is
           (= x2 x1)))))))
