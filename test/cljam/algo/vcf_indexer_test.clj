(ns cljam.algo.vcf-indexer-test
  "Tests for cljam.algo.bam-indexer."
  (:require [clojure.test :refer :all]
            [clojure.string :as cstr]
            [clojure.java.io :as cio]
            [cljam.test-common :refer :all]
            [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf]
            [cljam.algo.vcf-indexer :as vcf-indexer])
  (:import cljam.io.csi.CSI
           cljam.io.vcf.reader.VCFReader))

(def tmp-csi-file (str temp-dir "/tmp.csi"))
(def tmp-vcf-file (str temp-dir "/tmp.vcf.gz"))
(def tmp-vcf-csi-file (str temp-dir "/tmp.vcf.gz.csi"))

(defn- chunks->maps [bidx]
  (->> bidx
       (map (fn [[chr bin-chunks]]
              [chr
               (->> bin-chunks
                    (map (fn [[bin chunks]]
                           [bin (mapv #(into {} %) chunks)]))
                    (into {}))]))
       (into {})))

(deftest about-vcf-indexer
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw?
         (vcf-indexer/create-index test-vcf-complex-gz-file
                                   tmp-csi-file {:shift 14 :depth 6})))

    (do (vcf-indexer/create-index test-vcf-complex-gz-file
                                  tmp-csi-file {:shift 14 :depth 6})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) 4))
          (is (= (.min-shift csi) 14))
          (is (= (.depth csi) 6))
          (is (= (.loffset csi)
                 {0 {1 3904},
                  1 {32769 3973},
                  2 {114704385 4031},
                  3 {1 4783}}))
          (is (= (chunks->maps (.bidx csi))
                 {0 {37449 [{:beg 3904, :end 3973}]},
                  1 {37451 [{:beg 3973, :end 4031}]},
                  2 {44450 [{:beg 4031, :end 4783}]},
                  3 {37449 [{:beg 4783, :end 106168320}]}}))))

    (do (vcf-indexer/create-index test-vcf-complex-gz-file
                                  tmp-csi-file {})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) 4))
          (is (= (.min-shift csi) 14))
          (is (= (.depth csi) 5))
          (is (= (.loffset csi)
                 {0 {1 3904},
                  1 {32769 3973},
                  2 {114704385 4031},
                  3 {1 4783}}))
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 3904, :end 3973}]},
                  1 {4683 [{:beg 3973, :end 4031}]},
                  2 {11682 [{:beg 4031, :end 4783}]},
                  3 {4681 [{:beg 4783, :end 106168320}]}}))))

    (do (vcf-indexer/create-index test-vcf-complex-gz-file
                                  tmp-csi-file {:shift 18 :depth 5})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) 4))
          (is (= (.min-shift csi) 18))
          (is (= (.depth csi) 5))
          (is (= (.loffset csi)
                 {0 {1 3904},
                  1 {1 3973},
                  2 {114556929 4031},
                  3 {1 4783}}))
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 3904, :end 3973}]},
                  1 {4681 [{:beg 3973, :end 4031}]},
                  2 {5118 [{:beg 4031, :end 4783}]},
                  3 {4681 [{:beg 4783, :end 106168320}]}}))))))

(deftest about-vcf-changed-chr-order
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (do (vcf-indexer/create-index test-vcf-changed-chr-order-file
                                  tmp-csi-file {:shift 18 :depth 5})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (cstr/split (String. (byte-array (drop 28 (.aux csi))))
                             #"\00")
                 ["19" "20" "X"]))

          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 2096, :end 2205}]},
                  1 {4681 [{:beg 1647, :end 1811}],
                     4685 [{:beg 1811, :end 2096}]},
                  2 {4681 [{:beg 2205, :end 70975488}]}}))

          (is (= (.loffset csi)
                 {0 {1 2096},
                  1 {1 1647, 1048577 1811},
                  2 {1 2205}}))))))

(deftest about-vcf-changed-chr-order-field-less
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (do (vcf-indexer/create-index test-vcf-changed-chr-order-field-less-file
                                  tmp-csi-file {:shift 18 :depth 5})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (cstr/split (String. (byte-array (drop 28 (.aux csi))))
                             #"\00")
                 ["20" "19" "X"]))
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 1597, :end 1761}],
                     4685 [{:beg 1761, :end 2046}]},
                  1 {4681 [{:beg 2046, :end 2155}]},
                  2 {4681 [{:beg 2155, :end 70057984}]}}))
          (is (= (.loffset csi)
                 {0 {1 1597, 1048577 1761},
                  1 {1 2046},
                  2 {1 2155}}))))))

(deftest about-vcf-skipped-chr-order
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (vcf-indexer/create-index  test-vcf-chr-skipped-file
                               tmp-csi-file {:shift 14 :depth 6})
    (let [csi ^CSI (csi/read-index tmp-csi-file)]

      (is (= (.n-ref csi) 3))
      (is (= (.min-shift csi) 14))
      (is (= (.depth csi) 6))
      (is (= (cstr/split (String. (byte-array (drop 28 (.aux csi))))
                         #"\00")
             ["chr1" "chr2" "chr3"]))
      (is (= (chunks->maps (.bidx csi))
             {0 {37449 [{:beg 117, :end 136}]},
              1 {}
              2 {37449 [{:beg 136, :end 10682368}]}}))
      (is (= (.loffset csi) {0 {1 117}, 1 {} ,2 {1 136}})))))

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

(defn- check-loffset [file-offsets file-offset-index beg file-offset]
   ;;If "beg" which is the key of the loffset does not exist
   ;;between the beg-end of the variant data,
   ;;the selected data must be closest to "beg"
   ;;among data with pos greater than "beg".
  (let [index (get file-offset-index file-offset)
        current-offset (nth file-offsets index)
        previous-offset (nth file-offsets (dec index))]
    (and (= (:chr current-offset) (:chr previous-offset))
         (< (:end previous-offset) beg (:beg current-offset)))))

(deftest csi-comparison-test
  (with-open [r (vcf/reader test-vcf-various-bins-gz-file)]
    (let [file-offsets (vcf/read-file-offsets r)
          computed
          (csi/offsets->index file-offsets 14 6
                              {:variant-file-type :vcf
                               :names (mapv :id
                                            (:contig (.meta-info ^VCFReader
                                                      r)))})
          csi ^CSI (csi/read-index test-vcf-various-bins-csi-file)
          file-offset-index (->> file-offsets
                                 (map-indexed #(vector (:file-beg %2)
                                                       %1))
                                 (into {}))]
      (is (= (.n-ref csi) (.n-ref ^CSI computed)))
      (is (= (.min-shift csi) (.min-shift ^CSI computed)))
      (is (= (.depth csi) (.depth ^CSI computed)))
      (is (= (vec (.aux csi)) (vec (.aux ^CSI computed))))
      ;;Exclude tail data
      ;;due to differences in implementation of HTSlib and bgzf4j.
      (is (= (update (.bidx csi) 0 dissoc 75)
             (update (.bidx ^CSI computed) 0 dissoc 75)))

      (doseq [chr (keys (.loffset csi))]
        (doseq [beg (keys (get (.loffset csi) chr))]
          (is (or (= (get-in (.loffset csi) [chr beg])
                     (get-in (.loffset ^CSI computed) [chr beg]))
                  (check-loffset file-offsets
                                 file-offset-index
                                 beg (get-in (.loffset ^CSI computed)
                                             [chr beg])))))))))

(deftest-remote large-csi-comparison-test
  (with-before-after {:before (prepare-cavia!)}
    (with-open [r (vcf/reader test-large-vcf-file)]
      (let [file-offsets (vcf/read-file-offsets r)
            csi ^CSI (csi/read-index test-large-vcf-csi-file)
            computed
            (csi/offsets->index file-offsets 14 6
                                {:variant-file-type :vcf
                                 :names (mapv :id
                                              (:contig (.meta-info ^VCFReader
                                                        r)))})
            file-offset-index (->> file-offsets
                                   (map-indexed #(vector (:file-beg %2)
                                                         %1))
                                   (into {}))]
        (is (= (.n-ref csi) (.n-ref ^CSI computed)))
        (is (= (.min-shift csi) (.min-shift ^CSI computed)))
        (is (= (.depth csi) (.depth ^CSI computed)))
        (is (= (vec (.aux csi)) (vec (.aux ^CSI computed))))
        (is (= (update (.bidx csi) 24 dissoc 37450)
               (update (.bidx ^CSI computed) 24 dissoc 37450)))
        (doseq [chr (keys (.loffset csi))]
          (doseq [beg (keys (get (.loffset csi) chr))]
            (is (or (= (get-in (.loffset csi) [chr beg])
                       (get-in (.loffset ^CSI computed) [chr beg]))
                    (check-loffset file-offsets
                                   file-offset-index
                                   beg (get-in (.loffset ^CSI computed)
                                               [chr beg]))))))))))
