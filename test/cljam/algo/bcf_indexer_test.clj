(ns cljam.algo.bcf-indexer-test
  "Tests for cljam.algo.bam-indexer."
  (:require [clojure.test :refer [deftest is]]
            [cljam.test-common :refer
             [deftest-remote
              temp-dir
              with-before-after
              prepare-cache!
              prepare-cavia!
              clean-cache!
              not-throw?
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
            [cljam.algo.bcf-indexer :as bcf-indexer])
  (:import cljam.io.csi.CSI
           cljam.io.bcf.reader.BCFReader))

(def tmp-csi-file (str temp-dir "/tmp.csi"))

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

(deftest about-vcf-indexer
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw?
         (bcf-indexer/create-index test-bcf-complex-file
                                   tmp-csi-file {:shift 14 :depth 5})))

    (do (bcf-indexer/create-index test-bcf-complex-file
                                  tmp-csi-file {:shift 14 :depth 5})
        (let [computed ^CSI (csi/read-index tmp-csi-file)
              csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) (.n-ref computed)))
          (is (= (.depth csi) (.depth computed)))
          (is (= (.aux csi) (.aux computed)))
          (is (= (.loffset csi) (.loffset computed)))
          (is (= (.bidx csi) (.bidx computed)))))))

(deftest csi-comparison-test
  (with-open [r (vcf/reader test-bcf-various-bins-file)]
    (let [file-offsets (vcf/read-file-offsets r)
          computed
          (csi/offsets->index file-offsets 14 6
                              {:variant-file-type :bcf
                               :names (mapv :id
                                            (:contig (.meta-info ^BCFReader
                                                      r)))})
          csi ^CSI (csi/read-index test-bcf-various-bins-csi-file)
          file-offset-index (->> file-offsets
                                 (map-indexed #(vector (:file-beg %2)
                                                       %1))
                                 (into {}))]
      (is (= (.n-ref csi) (.n-ref ^CSI computed)))
      (is (= (.min-shift csi) (.min-shift ^CSI computed)))
      (is (= (.depth csi) (.depth ^CSI computed)))
      (is (= (vec (.aux csi)) (vec (.aux ^CSI computed))))
      (is (= (.bidx csi)
             (.bidx ^CSI computed)))
      (doseq [chr (keys (.loffset csi))]
        (doseq [beg (keys (get (.loffset csi) chr))]
          (is (or (= (get-in (.loffset csi) [chr beg])
                     (get-in (.loffset ^CSI computed) [chr beg]))
                  (check-loffset file-offsets
                                 file-offset-index
                                 beg (get-in (.loffset ^CSI computed)
                                             [chr beg])))))))))

(deftest changed-chr-csi-comparison-test
  (with-open [r (vcf/reader test-bcf-changed-chr-order-file)]
    (let [file-offsets (vcf/read-file-offsets r)
          computed
          (csi/offsets->index file-offsets 14 6
                              {:variant-file-type :bcf
                               :names (mapv :id
                                            (:contig (.meta-info ^BCFReader
                                                      r)))})
          csi ^CSI (csi/read-index  test-bcf-changed-chr-order-csi-file)
          file-offset-index (->> file-offsets
                                 (map-indexed #(vector (:file-beg %2)
                                                       %1))
                                 (into {}))]
      (is (= (.n-ref csi) (.n-ref ^CSI computed)))
      (is (= (.min-shift csi) (.min-shift ^CSI computed)))
      (is (= (.depth csi) (.depth ^CSI computed)))
      (is (= (vec (.aux csi)) (vec (.aux ^CSI computed))))
      (is (= (.bidx csi) (.bidx ^CSI computed)))
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
    (with-open [r (vcf/reader test-large-bcf-file)]
      (let [file-offsets (vcf/read-file-offsets r)
            csi ^CSI (csi/read-index test-large-bcf-csi-file)
            computed
            (csi/offsets->index file-offsets 14 5
                                {:variant-file-type :bcf
                                 :names (mapv :id
                                              (:contig (.meta-info ^BCFReader
                                                        r)))})
            file-offset-index (->> file-offsets
                                   (map-indexed #(vector (:file-beg %2)
                                                         %1))
                                   (into {}))]
        (is (= (.n-ref csi) (.n-ref ^CSI computed)))
        (is (= (.min-shift csi) (.min-shift ^CSI computed)))
        (is (= (.depth csi) (.depth ^CSI computed)))
        (is (= (vec (.aux csi)) (vec (.aux ^CSI computed))))
        (is (= (update (.bidx csi) 24 dissoc 4681)
               (update (.bidx ^CSI computed) 24 dissoc 4681)))
        (doseq [chr (keys (.loffset csi))]
          (doseq [beg (keys (get (.loffset csi) chr))]
            (is (or (= (get-in (.loffset csi) [chr beg])
                       (get-in (.loffset ^CSI computed) [chr beg]))
                    (check-loffset file-offsets
                                   file-offset-index
                                   beg (get-in (.loffset ^CSI computed)
                                               [chr beg]))))))))))
