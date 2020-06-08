(ns cljam.io.csi-test
  (:require [clojure.java.io :as cio]
            [clojure.test :refer [deftest is are testing]]
            [cljam.test-common :refer
             [deftest-remote
              with-before-after
              prepare-cache!
              prepare-cavia!
              clean-cache!
              unrecord
              http-server
              temp-dir
              small-bam-file
              test-csi-file
              test-large-vcf-csi-file]]
            [cljam.io.csi :as csi])
  (:import [cljam.io.csi CSI]))

(def ^:private aux-data
  (->> (str "02000000010000000200000000000000"
            "23000000000000000a00000063687231"
            "006368723300")
       (partition 2)
       (map #(unchecked-byte (Short/parseShort (apply str %) 16)))
       (byte-array)))

(def ^:private ^:const aux-map
  {:format 2, :col-seq 1, :col-beg 2, :col-end 0,
   :meta-char \#, :skip 0, :chrs ["chr1" "chr3"]})

(deftest tabix-aux-test
  (is (= aux-map (@#'csi/parse-tabix-aux aux-data)))
  (is (= (vec aux-data) (vec (@#'csi/create-tabix-aux aux-map)))))

(deftest about-read-index-with-error
  (is (thrown? java.io.IOException (csi/read-index small-bam-file))))

(deftest about-read-index-returns-csi-object
  (is (instance? CSI (csi/read-index test-csi-file))))

(deftest read-index-test
  (let [csi-data (csi/read-index test-csi-file)]
    (is (= 4 (.n-ref csi-data)))
    (is (= 14 (.min-shift csi-data)))
    (is (= 6 (.depth csi-data)))
    (is (= [{37449 [{:beg 3904, :end 3973}]}
            {37451 [{:beg 3973, :end 4031}]}
            {44450 [{:beg 4031, :end 4783}]}
            {37449 [{:beg 4783, :end 104333312}]}]
           (unrecord (.bidx csi-data))))
    (is (= [{1 3904} {32769 3973} {114704385 4031} {1 4783}]
           (.loffset csi-data)))))

(deftest concat-offsets-test
  (is (= [{:file-beg 1, :file-end 20}
          {:file-beg 21, :file-end 30}
          {:file-beg 32, :file-end 40}
          {:file-beg 50, :file-end 60}]
         (#'csi/concatenate-offsets
          [{:file-beg 1, :file-end 10}
           {:file-beg 10, :file-end 20}
           {:file-beg 21, :file-end 30}
           {:file-beg 32, :file-end 40}
           {:file-beg 50, :file-end 60}]))))

(deftest compress-bidx-test
  (is (= {585 [{:file-beg 20, :file-end 30}
               {:file-beg 10, :file-end 20}
               {:file-beg 4294967296, :file-end 4294967400}],
          4682 [{:file-beg 30, :file-end 4294967296}]}
         (#'csi/compress-bidx
          5
          {585 [{:file-beg 20, :file-end 30}],
           4681 [{:file-beg 10, :file-end 20}],
           4682 [{:file-beg 30, :file-end 4294967296}],
           4683 [{:file-beg 4294967296, :file-end 4294967400}]}))))

(deftest calc-bidx-test
  (is (= {585 [{:beg 10, :end 30} {:beg 4294967296, :end 4294967400}],
          4682 [{:beg 30, :end 4294967296}]}
         (unrecord
          (#'csi/calc-bidx
           [{:file-beg 10, :file-end 20,
             :beg 1, :end 1}
            {:file-beg 20, :file-end 30,
             :beg 16384, :end 16385}
            {:file-beg 30, :file-end 4294967296,
             :beg 16385, :end 16385}
            {:file-beg 4294967296, :file-end 4294967400,
             :beg 32769, :end 32769}]
           14 5)))))

(deftest offsets->index-test
  (let [data [{:chr-index 0, :chr "chr1", :beg 1, :end 1,
               :file-beg 10, :file-end 20}
              {:chr-index 0, :chr "chr1", :beg 16384, :end 16385,
               :file-beg 20, :file-end 30}
              {:chr-index 0, :chr "chr1", :beg 16385, :end 16385,
               :file-beg 30, :file-end 4294967296}
              {:chr-index 0, :chr "chr1", :beg 32769, :end 32769,
               :file-beg 4294967296, :file-end 4294967400}
              {:chr-index 2, :chr "chr3", :beg 1, :end 1,
               :file-beg 4294967400, :file-end 4294967800}]
        vcf (csi/offsets->index data 14 5 {:variant-file-type :vcf, :names []})
        vcf' (csi/offsets->index data 14 5 {:variant-file-type :vcf,
                                            :names ["chr1" "chr2" "chr3"]})
        bcf (csi/offsets->index data 14 5 {:variant-file-type :bcf,
                                           :names ["chr1" "chr2" "chr3"]})]
    (testing "n-ref"
      (is (= 2 (.n-ref vcf) (.n-ref vcf')))
      (is (= 3 (.n-ref bcf))))
    (testing "binning index"
      (is (= [{585 [{:beg 10, :end 30} {:beg 4294967296, :end 4294967400}]
               4682 [{:beg 30, :end 4294967296}]}
              {4681 [{:beg 4294967400, :end 4294967800}]}]
             (unrecord (.bidx vcf))
             (unrecord (.bidx vcf'))))
      (is (= [{585 [{:beg 10, :end 30} {:beg 4294967296, :end 4294967400}],
               4682 [{:beg 30, :end 4294967296}]}
              nil
              {4681 [{:beg 4294967400, :end 4294967800}]}]
             (unrecord (.bidx bcf)))))
    (testing "linear index"
      (is (= [{1 10, 16385 20} {1 4294967400}]
             (.loffset vcf)
             (.loffset vcf')))
      (is (= [{1 10, 16385 20} nil {1 4294967400}]
             (.loffset bcf))))
    (testing "aux"
      (is (= {:format 2, :col-seq 1, :col-beg 2, :col-end 0,
              :meta-char \#, :skip 0, :chrs ["chr1" "chr3"]}
             (.aux vcf)
             (.aux vcf')))
      (is (nil? (.aux bcf))))))

(deftest source-type-test
  (with-open [server (http-server)]
    (are [x] (instance? CSI (csi/read-index x))
      test-csi-file
      (cio/file test-csi-file)
      (cio/as-url (cio/file test-csi-file))
      (cio/as-url (str (:uri server) "/csi/test.csi")))))

(deftest-remote large-read-write-test
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!)),
                      :after (clean-cache!)}
    (let [temp-csi-file (cio/file temp-dir "test_temp.csi")
          r (csi/read-index test-large-vcf-csi-file)
          _ (csi/write-index temp-csi-file r)
          w (csi/read-index temp-csi-file)]
      (is (= (.n-ref r) (.n-ref w)))
      (is (= (.min-shift r) (.min-shift w)))
      (is (= (.depth r) (.depth w)))
      (is (= (.bidx r) (.bidx w)))
      (is (= (.loffset r) (.loffset w)))
      (is (= (.aux r) (.aux w))))))
