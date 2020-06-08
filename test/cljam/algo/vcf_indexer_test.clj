(ns cljam.algo.vcf-indexer-test
  "Tests for cljam.algo.bam-indexer."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as cio]
            [cljam.test-common :refer
             [deftest-remote
              temp-dir
              with-before-after
              unrecord
              prepare-cache!
              prepare-cavia!
              clean-cache!
              test-large-vcf-file
              test-large-vcf-csi-file
              test-vcf-complex-gz-file
              test-vcf-complex-csi-file
              test-vcf-changed-chr-order-file
              test-vcf-changed-chr-order-field-less-file
              test-vcf-chr-skipped-file
              test-vcf-chr-skipped-csi-file
              test-vcf-various-bins-gz-file
              test-vcf-various-bins-csi-file
              test-bcf-complex-file
              test-bcf-complex-csi-file
              test-bcf-various-bins-file
              test-bcf-various-bins-csi-file
              test-bcf-changed-chr-order-file
              test-bcf-changed-chr-order-csi-file
              test-large-bcf-file
              test-large-bcf-csi-file]]
            [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf]
            [cljam.algo.vcf-indexer :as vcf-indexer]))

(def tmp-csi-file (str temp-dir "/tmp.csi"))
(def tmp-vcf-file (str temp-dir "/tmp.vcf.gz"))
(def tmp-vcf-csi-file (str temp-dir "/tmp.vcf.gz.csi"))

(defn- check-loffset [file-offsets file-offset-index beg file-offset]
  ;; If "beg" which is the key of the loffset does not exist
  ;; between the beg-end of the variant data,
  ;; the selected data must be closest to "beg"
  ;; among data with pos greater than "beg".
  (let [index (get file-offset-index file-offset)
        current-offset (nth file-offsets index)
        previous-offset (nth file-offsets (dec index))]
    (and (= (:chr current-offset) (:chr previous-offset))
         (< (:end previous-offset) beg (:beg current-offset)))))

(deftest about-vcf-indexer
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (testing "vcf"
      (vcf-indexer/create-index
       test-vcf-complex-gz-file tmp-csi-file {:shift 14 :depth 6})
      (let [computed (csi/read-index tmp-csi-file)
            csi (csi/read-index test-vcf-complex-csi-file)]
        (is (= 4 (.n-ref csi) (.n-ref computed)))
        (is (= 14 (.min-shift csi) (.min-shift computed)))
        (is (= 6 (.depth csi) (.depth computed)))
        (is (= [{1 3904},
                {32769 3973},
                {114704385 4031},
                {1 4783}]
               (.loffset csi)
               (.loffset computed)))
        (is (= [{37449 [{:beg 3904, :end 3973}]},
                {37451 [{:beg 3973, :end 4031}]},
                {44450 [{:beg 4031, :end 4783}]},
                {37449 [{:beg 4783, :end 106168320}]}]
               ;; HACK: Overwriting the last pointer due to the implementation
               ;; differences of htslib and bgzf4j
               (-> (unrecord (.bidx csi))
                   (assoc-in [3 37449 0 :end] 106168320))
               (unrecord (.bidx computed)))))

      (vcf-indexer/create-index
       test-vcf-complex-gz-file tmp-csi-file {})
      (let [computed (csi/read-index tmp-csi-file)]
        (is (= 4 (.n-ref computed)))
        (is (= 14 (.min-shift computed)))
        (is (= 5 (.depth computed)))
        (is (= [{1 3904},
                {32769 3973},
                {114704385 4031},
                {1 4783}]
               (.loffset computed)))
        (is (= [{4681 [{:beg 3904, :end 3973}]},
                {4683 [{:beg 3973, :end 4031}]},
                {11682 [{:beg 4031, :end 4783}]},
                {4681 [{:beg 4783, :end 106168320}]}]
               (unrecord (.bidx computed)))))

      (vcf-indexer/create-index
       test-vcf-complex-gz-file tmp-csi-file {:shift 18 :depth 5})
      (let [computed (csi/read-index tmp-csi-file)]
        (is (= (.n-ref computed) 4))
        (is (= (.min-shift computed) 18))
        (is (= (.depth computed) 5))
        (is (= (.loffset computed)
               [{1 3904},
                {1 3973},
                {114556929 4031},
                {1 4783}]))
        (is (= [{4681 [{:beg 3904, :end 3973}]},
                {4681 [{:beg 3973, :end 4031}]},
                {5118 [{:beg 4031, :end 4783}]},
                {4681 [{:beg 4783, :end 106168320}]}]
               (unrecord (.bidx computed))))))

    (testing "bcf"
      (vcf-indexer/create-index
       test-bcf-complex-file tmp-csi-file {:shift 14 :depth 5})
      (let [computed (csi/read-index tmp-csi-file)
            csi (csi/read-index test-bcf-complex-csi-file)]
        (is (= (.n-ref csi) (.n-ref computed)))
        (is (= (.depth csi) (.depth computed)))
        (is (= (.aux csi) (.aux computed)))
        (is (= (.loffset csi) (.loffset computed)))
        (is (= [{4681 [{:beg 4302, :end 4386}]}
                {4683 [{:beg 4386, :end 4452}]}
                {11682 [{:beg 4452, :end 5476}]}
                {4681 [{:beg 5476, :end 129236992}]}]
               (unrecord (.bidx csi))
               (unrecord (.bidx computed))))))))

(deftest about-vcf-changed-chr-order
  (testing "vcf"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (do (vcf-indexer/create-index test-vcf-changed-chr-order-file
                                    tmp-csi-file {:shift 18 :depth 5})
          (let [csi (csi/read-index tmp-csi-file)]
            (is (= ["19" "20" "X"] (:chrs (.aux csi))))
            (is (= [{4681 [{:beg 2096, :end 2205}]}
                    {4681 [{:beg 1647, :end 1811}],
                     4685 [{:beg 1811, :end 2096}]}
                    {4681 [{:beg 2205, :end 70975488}]}]
                   (unrecord (.bidx csi))))
            (is (= (.loffset csi)
                   [{1 2096},
                    {1 1647, 1048577 1811},
                    {1 2205}]))))))
  (testing "bcf"
    (with-open [r (vcf/reader test-bcf-changed-chr-order-file)]
      (let [file-offsets (vcf/read-file-offsets r)
            computed (csi/offsets->index
                      file-offsets 14 6 {:variant-file-type :bcf})
            csi (csi/read-index test-bcf-changed-chr-order-csi-file)
            file-offset-index (->> file-offsets
                                   (map-indexed #(vector (:file-beg %2) %1))
                                   (into {}))]
        (is (= (.n-ref csi) (.n-ref computed)))
        (is (= (.min-shift csi) (.min-shift computed)))
        (is (= (.depth csi) (.depth computed)))
        (is (= (vec (.aux csi)) (vec (.aux computed))))
        (is (= [{37449 [{:beg 2275, :end 2387}]}
                {37449 [{:beg 1862, :end 1940}],
                 37450 [{:beg 1940, :end 2009}],
                 37516 [{:beg 2009, :end 2089}],
                 37524 [{:beg 2089, :end 2275}]}
                {37449 [{:beg 2387, :end 79953920}]}]
               (unrecord (.bidx csi))
               (unrecord (.bidx computed))))
        (doseq [[l-csi l-comp] (map vector (.loffset csi) (.loffset computed))]
          (doseq [[beg offset] l-comp]
            (is (or (= (get l-csi beg) offset)
                    (check-loffset
                     file-offsets file-offset-index beg (get l-comp beg))))))))))

(deftest about-vcf-changed-chr-order-field-less
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (vcf-indexer/create-index test-vcf-changed-chr-order-field-less-file
                              tmp-csi-file {:shift 18 :depth 5})
    (let [csi (csi/read-index tmp-csi-file)]
      (is (= ["20" "19" "X"] (:chrs (.aux csi))))
      (is (= [{4681 [{:beg 1597, :end 1761}],
               4685 [{:beg 1761, :end 2046}]}
              {4681 [{:beg 2046, :end 2155}]}
              {4681 [{:beg 2155, :end 70057984}]}]
             (unrecord (.bidx csi))))
      (is (= [{1 1597, 1048577 1761} {1 2046} {1 2155}]
             (.loffset csi))))))

(deftest about-vcf-skipped-chr-order
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (vcf-indexer/create-index
     test-vcf-chr-skipped-file tmp-csi-file {:shift 14 :depth 6})
    (let [computed (csi/read-index tmp-csi-file)
          csi (csi/read-index test-vcf-chr-skipped-csi-file)]
      (is (= 3 #_(.n-ref csi) (.n-ref computed)))
      (is (= 14 (.min-shift csi) (.min-shift computed)))
      (is (= 6 (.depth csi) (.depth computed)))
      (is (= ["chr1" "" "chr3"]
             #_(:chrs (.aux csi))
             (:chrs (.aux computed))))
      (is (= [{37449 [{:beg 117, :end 136}]}
              {}
              {37449 [{:beg 136, :end 10682368}]}]
             ;; HACK: Overwriting the last pointer due to the implementation
             ;; differences of htslib and bgzf4j
             #_(-> (unrecord (.bidx csi))
                 (assoc-in [1 37449 0 :end] 10682368))
             (unrecord (.bidx computed))))
      (is (= [{1 117} {} {1 136}]
             #_(.loffset csi)
             (.loffset computed))))))

(deftest about-reading-vcf-skipped-chr-order
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (cio/copy (cio/file test-vcf-chr-skipped-file)
              (cio/file tmp-vcf-file))
    (vcf-indexer/create-index  test-vcf-chr-skipped-file
                               tmp-vcf-csi-file {:shift 14 :depth 6})

    (is (= (vcf/read-variants-randomly (vcf/reader tmp-vcf-file)
                                       {:chr "chr1"} {})
           '({:alt ["T"], :ref "A", :pos 1, :filter nil,
              :id nil, :info nil, :qual nil, :chr "chr1"})))
    (is (= (vcf/read-variants-randomly (vcf/reader tmp-vcf-file)
                                       {:chr "chr3"} {})
           '({:alt ["C"], :ref "G", :pos 1, :filter nil,
              :id nil, :info nil, :qual nil, :chr "chr3"})))))

(deftest csi-comparison-test
  (testing "vcf"
    (with-open [r (vcf/reader test-vcf-various-bins-gz-file)]
      (let [file-offsets (vcf/read-file-offsets r)
            computed (csi/offsets->index
                      file-offsets 14 6 {:variant-file-type :vcf})
            csi (csi/read-index test-vcf-various-bins-csi-file)
            file-offset-index (->> file-offsets
                                   (map-indexed #(vector (:file-beg %2) %1))
                                   (into {}))]
        (is (= (.n-ref csi) (.n-ref computed)))
        (is (= (.min-shift csi) (.min-shift computed)))
        (is (= (.depth csi) (.depth computed)))
        (is (= (.aux csi) (.aux computed)))
        ;; Exclude tail data
        ;; due to differences in implementation of HTSlib and bgzf4j.
        (is (= (update (.bidx csi) 0 dissoc 75 299594)
               (update (.bidx computed) 0 dissoc 75 299594)))

        (doseq [[l-csi l-comp] (map vector (.loffset csi) (.loffset computed))]
          (doseq [[beg offset] l-csi]
            (is (or (= offset (get l-comp beg))
                    (check-loffset
                     file-offsets file-offset-index beg (get l-comp beg)))))))))
  (testing "bcf"
    (with-open [r (vcf/reader test-bcf-various-bins-file)]
      (let [file-offsets (vcf/read-file-offsets r)
            computed (csi/offsets->index
                      file-offsets 14 6 {:variant-file-type :bcf})
            csi (csi/read-index test-bcf-various-bins-csi-file)
            file-offset-index (->> file-offsets
                                   (map-indexed #(vector (:file-beg %2) %1))
                                   (into {}))]
        (is (= (.n-ref csi) (.n-ref computed)))
        (is (= (.min-shift csi) (.min-shift computed)))
        (is (= (.depth csi) (.depth computed)))
        (is (= (.aux csi) (.aux computed)))
        (is (= (.bidx csi)
               (.bidx computed)))
        (doseq [[l-csi l-comp] (map vector (.loffset csi) (.loffset computed))]
          (doseq [[beg offset] l-csi]
            (is (or (= offset (get l-comp beg))
                    (check-loffset
                     file-offsets file-offset-index beg (get l-comp beg))))))))))

(deftest-remote large-csi-comparison-test
  (testing "vcf"
    (with-before-after {:before (prepare-cavia!)}
      (with-open [r (vcf/reader test-large-vcf-file)]
        (let [file-offsets (vcf/read-file-offsets r)
              csi (csi/read-index test-large-vcf-csi-file)
              computed (csi/offsets->index
                        file-offsets 14 6 {:variant-file-type :vcf})
              file-offset-index (->> file-offsets
                                     (map-indexed #(vector (:file-beg %2) %1))
                                     (into {}))]
          (is (= (.n-ref csi) (.n-ref computed)))
          (is (= (.min-shift csi) (.min-shift computed)))
          (is (= (.depth csi) (.depth computed)))
          (is (= (.aux csi) (.aux computed)))
          (is (= (update (.bidx csi) 24 dissoc 37450)
                 (update (.bidx computed) 24 dissoc 37450)))
          (doseq [[l-csi l-comp] (map vector (.loffset csi) (.loffset computed))]
            (doseq [[beg offset] l-csi]
              (is (or (= offset (get l-comp beg))
                      (check-loffset
                       file-offsets file-offset-index beg (get l-comp beg))))))))))
  (testing "bcf"
    (with-before-after {:before (prepare-cavia!)}
      (with-open [r (vcf/reader test-large-bcf-file)]
        (let [file-offsets (vcf/read-file-offsets r)
              csi (csi/read-index test-large-bcf-csi-file)
              computed (csi/offsets->index
                        file-offsets 14 5 {:variant-file-type :bcf})
              file-offset-index (->> file-offsets
                                     (map-indexed #(vector (:file-beg %2) %1))
                                     (into {}))]
          (is (= (.n-ref csi) (.n-ref computed)))
          (is (= (.min-shift csi) (.min-shift computed)))
          (is (= (.depth csi) (.depth computed)))
          (is (= (vec (.aux csi)) (vec (.aux computed))))
          (is (= (update (.bidx csi) 24 dissoc 4681)
                 (update (.bidx computed) 24 dissoc 4681)))
          (doseq [[l-csi l-comp] (map vector (.loffset csi) (.loffset computed))]
            (doseq [[beg offset] l-csi]
              (is (or (= offset (get l-comp beg))
                      (check-loffset
                       file-offsets file-offset-index beg (get l-comp beg)))))))))))
