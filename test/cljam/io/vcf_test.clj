(ns cljam.io.vcf-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.test-common :refer :all]
            [cljam.io.vcf :as vcf])
  (:import bgzf4j.BGZFException))

(def ^:private temp-file (str temp-dir "/test.vcf"))
(def ^:private temp-bcf-file (str temp-dir "/test.bcf"))

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
      (is (instance? cljam.io.vcf.reader.VCFReader rdr)))
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (let [tmp (cio/file temp-dir "temp-vcf-file-without-suffix")]
        (cio/copy (cio/file test-vcf-v4_3-file) tmp)
        (with-open [rdr (vcf/reader tmp)]
          (is (instance? cljam.io.vcf.reader.VCFReader rdr))))))
  (testing "bcf"
    (with-open [rdr (vcf/reader test-bcf-v4_3-file)]
      (is (instance? cljam.io.bcf.reader.BCFReader rdr)))
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (let [tmp (cio/file temp-dir "temp-bcf-file-without-suffix")]
        (cio/copy (cio/file test-bcf-v4_3-file) tmp)
        (with-open [rdr (vcf/reader tmp)]
          (is (instance? cljam.io.bcf.reader.BCFReader rdr))))))
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
      (is (= (vcf/read-variants rdr) test-vcf-v4_3-variants-deep))))
  (testing "no samples"
    (with-open [rdr (vcf/reader test-vcf-no-samples-file)]
      (is (= (vcf/read-variants rdr) test-vcf-no-samples-variants-deep)))))

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
        (is (= v1 v2)))))
  (testing "no samples"
    (with-open [rdr (vcf/reader test-bcf-no-samples-file)]
      (is (= (vcf/read-variants rdr) test-vcf-no-samples-variants-deep)))))

(deftest read-variants-complex-test
  (with-open [v (vcf/reader test-vcf-complex-file)    ;; uncompressed VCF
              z (vcf/reader test-vcf-complex-gz-file) ;; bgzipped VCF
              b (vcf/reader test-bcf-complex-file)]   ;; bgzipped BCF
    (is (= (vcf/read-variants v)
           (vcf/read-variants z)
           (vcf/read-variants b)))))

(deftest-remote bin-index-is-done-without-errors-with-a-large-file
  (with-before-after {:before (prepare-cavia!)}
    (testing "vcf"
      (are [index]
           (with-open [v (vcf/vcf-reader test-large-vcf-file)]
             (is (not-throw?
                  (vcf/read-variants-randomly v index {}))))
        {:chr "chr1"
         :start 20
         :end 1000000}
        {:chr "chr1"
         :start 2000}
        {:chr "chr1"}))
    (testing "bcf"
      (are [index]
           (with-open [v (vcf/vcf-reader test-large-bcf-file)]
             (is (not-throw?
                  (vcf/read-variants-randomly v index {}))))
        {:chr "chr1"
         :start 20
         :end 1000000}
        {:chr "chr1"
         :start 2000}
        {:chr "chr1"}))))

(deftest read-randomly-variants-complex-test
  (doseq [index [{:chr "1"}
                 {:chr "2" :start 30000}
                 {:chr "2" :start 40000}
                 {:chr "2" :end 40000}
                 {:chr "2" :start 30000 :end 40000}]]
    (with-open [vcf-file (vcf/vcf-reader test-vcf-complex-file)
                vcf-gz-file (vcf/vcf-reader test-vcf-complex-gz-file)
                bcf-file (vcf/bcf-reader test-bcf-complex-file)]
      (is (= (vcf/read-variants-randomly vcf-gz-file index {})
             (vcf/read-variants-randomly bcf-file index {})
             (let [{:keys [chr start end] :or {start 1 end 4294967296}} index]
               (filter
                (fn [{chr' :chr :keys [pos ref info]}]
                  (and (= chr chr')
                       (<= start (get info :END (dec (+ pos (count ref)))))
                       (>= end pos)))
                (vcf/read-variants
                 vcf-file))))))))

(deftest writer-test
  (testing "vcf"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [wtr (vcf/writer (.getAbsolutePath (cio/file temp-dir "temp.vcf")) {} [])]
        (is (instance? cljam.io.vcf.writer.VCFWriter wtr)))))
  (testing "bcf"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [wtr (vcf/writer (.getAbsolutePath (cio/file temp-dir "temp.bcf")) {} [])]
        (is (instance? cljam.io.bcf.writer.BCFWriter wtr))))
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [wtr (vcf/writer (.getAbsolutePath (cio/file temp-dir "temp.bcf"))
                                  {:filter [{:id "PASS", :description "All filters passed"}]} [])]
        (is (instance? cljam.io.bcf.writer.BCFWriter wtr)))))
  (testing "throws Exception"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (is (thrown? Exception
                   (vcf/writer (.getAbsolutePath (cio/file temp-dir "temp.vccf")) {} []))))))

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
    (testing "v4.3"
      (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3.bcf"))]
        (with-open [r (vcf/reader test-vcf-v4_3-file)
                    w (vcf/bcf-writer temp-file (vcf/meta-info r) (vcf/header r))]
          (vcf/write-variants w (vcf/read-variants r)))
        (with-open [r1 (vcf/reader test-vcf-v4_3-file)
                    r2 (vcf/bcf-reader temp-file)]
          (is (= (vcf/header r2) (vcf/header r1)))
          (is (= (vcf/meta-info r2) (vcf/meta-info r1)))
          (doseq [[x2 x1] (map vector (vcf/read-variants r2) (vcf/read-variants r1))]
            (is (= x2 x1))))))
    (testing "no samples"
      (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_no_samples.bcf"))]
        (with-open [v (vcf/reader test-vcf-no-samples-file)]
          (let [xs (vcf/read-variants v)
                m (vcf/meta-info v)
                h (vcf/header v)]
            (with-open [b (vcf/writer temp-file m h)]
              (vcf/write-variants b xs))
            (with-open [b (vcf/reader temp-file)]
              (is (= xs (vcf/read-variants b))))
            (with-open [b1 (vcf/reader test-bcf-no-samples-file)
                        b2 (vcf/reader temp-file)]
              (is (= (vcf/read-variants b1 {:depth :bcf})
                     (vcf/read-variants b2 {:depth :bcf}))))))))
    (testing "v4.3 complex"
      (let [temp-file (.getAbsolutePath (cio/file temp-dir "test_v4_3_complex.bcf"))]
        (with-open [v (vcf/reader test-vcf-complex-file)]
          (let [xs (vcf/read-variants v)
                m (vcf/meta-info v)
                h (vcf/header v)]
            (with-open [b (vcf/writer temp-file m h)]
              (vcf/write-variants b xs))
            (with-open [b (vcf/reader temp-file)]
              (is (= xs (vcf/read-variants b))))))))
    (testing "v4.3 complex bgzip"
      (let [temp-file (.getAbsolutePath
                       (cio/file temp-dir "test_v4_3_complex_bgzip.bcf"))]
        (with-open [v (vcf/reader test-vcf-complex-gz-file)]
          (let [xs (vcf/read-variants v)
                m (vcf/meta-info v)
                h (vcf/header v)]
            (with-open [b (vcf/writer temp-file m h)]
              (vcf/write-variants b xs))
            (with-open [b (vcf/reader temp-file)]
              (is (= xs (vcf/read-variants b))))))))))

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

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [x] (with-open [rdr (vcf/reader x)]
                 (and (= (vcf/meta-info rdr) test-vcf-v4_3-meta-info)
                      (= (vcf/header rdr) test-vcf-v4_3-header)
                      (= (vcf/read-variants rdr) test-vcf-v4_3-variants-deep)))
        test-vcf-v4_3-file
        (cio/file test-vcf-v4_3-file)
        (cio/as-url (cio/file test-vcf-v4_3-file))
        (cio/as-url (str (:uri server) "/vcf/test-v4_3.vcf"))

        test-bcf-v4_3-file
        (cio/file test-bcf-v4_3-file)
        (cio/as-url (cio/file test-bcf-v4_3-file))
        (cio/as-url (str (:uri server) "/bcf/test-v4_3.bcf")))))
  (testing "writer"
    (are [x] (with-before-after {:before (prepare-cache!)
                                 :after (clean-cache!)}
               (not-throw? (spit-vcf-for-test x
                                              test-vcf-v4_3-meta-info
                                              test-vcf-v4_3-header
                                              test-vcf-v4_3-variants-deep)))
      temp-file
      (cio/file temp-file)
      (cio/as-url (cio/file temp-file))

      temp-bcf-file
      (cio/file temp-bcf-file)
      (cio/as-url (cio/file temp-bcf-file)))))

(deftest read-variants-randomly-test
  (testing "vcf"
    (are [chr start end]
         (let [vcf1* (vcf/vcf-reader test-vcf-various-bins-gz-file)
               vcf2* (vcf/vcf-reader test-vcf-various-bins-gz-file)]
           (=
            (vcf/read-variants-randomly vcf1* {:chr chr :start start :end end}
                                        {})
            (filter
             (fn [{chr' :chr :keys [pos ref info]}]
               (and (= chr chr')
                    (<= start (get info :END (dec (+ pos (count ref)))))
                    (>= end pos)))
             (vcf/read-variants vcf2*))))
      "chr1" 1 16384
      "chr1" 1 49153
      "chr1" 1 30000
      "chr1" 49153 147457
      "chr1" 32769 147457
      "chr1" 49153 1064952
      "chr1" 1048577 414826496
      "chr1" 1048577 414826497
      "chr1" 32769 414859265
      "chr1" 414859265 536608769))
  (testing "bcf"
    (are [chr start end]
         (let [bcf1* (vcf/bcf-reader test-bcf-various-bins-file)
               bcf2* (vcf/bcf-reader test-bcf-various-bins-file)
               vcf (vcf/vcf-reader test-vcf-various-bins-gz-file)]
           (=
            (vcf/read-variants-randomly bcf2* {:chr chr :start start :end end} {})
            (vcf/read-variants-randomly vcf {:chr chr :start start :end end} {})
            (filter
             (fn [{chr' :chr :keys [pos ref info]}]
               (and (= chr chr')
                    (<= start (get info :END (dec (+ pos (count ref)))))
                    (>= end pos)))
             (vcf/read-variants bcf1*))))
      "chr1" 1 16384
      "chr1" 1 49153
      "chr1" 1 30000
      "chr1" 49153 147457
      "chr1" 32769 147457
      "chr1" 49153 1064952
      "chr1" 1048577 414826496
      "chr1" 1048577 414826497
      "chr1" 32769 414859265
      "chr1" 414859265 536608769)))
