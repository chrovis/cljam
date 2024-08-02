(ns cljam.io.crai-test
  (:require [cljam.io.crai :as crai]
            [cljam.test-common :as common]
            [cljam.util :as util]
            [clojure.java.io :as io]
            [clojure.test :refer [are deftest is]]))

(def ^:private test-refs
  (->> (concat (range 1 23) ["X" "Y"])
       (mapv #(array-map :name (str "chr" %)))))

(deftest read-index-test
  (let [idx (crai/read-index common/medium-crai-file test-refs)]
    (are [?chr ?start ?end ?expected]
         (= ?expected (crai/find-overlapping-entries idx ?chr ?start ?end))
      "chr1" 1 Long/MAX_VALUE
      [{:chr "chr1"
        :start 546609
        :end (+ 546609 205262429)
        :container-offset 324
        :slice-offset 563
        :size 22007}
       {:chr "chr1"
        :start 206547069
        :end (+ 206547069 42644506)
        :container-offset 324
        :slice-offset 22570
        :size 7349}]

      "chr1" 550000 600000
      [{:chr "chr1"
        :start 546609
        :end (+ 546609 205262429)
        :container-offset 324
        :slice-offset 563
        :size 22007}]

      "chr1" 210000000 240000000
      [{:chr "chr1"
        :start 206547069
        :end (+ 206547069 42644506)
        :container-offset 324
        :slice-offset 22570
        :size 7349}]

      "chr1" 200000000 210000000
      [{:chr "chr1"
        :start 546609
        :end (+ 546609 205262429)
        :container-offset 324
        :slice-offset 563
        :size 22007}
       {:chr "chr1"
        :start 206547069
        :end (+ 206547069 42644506)
        :container-offset 324
        :slice-offset 22570
        :size 7349}]

      "*" 0 0
      [{:chr "*"
        :start 0
        :end 0
        :container-offset 354657
        :slice-offset 563
        :size 23119}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 171
        :size 23494}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 23665
        :size 23213}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 46878
        :size 23051}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 69929
        :size 23563}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 93492
        :size 24231}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 117723
        :size 24078}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 141801
        :size 23871}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 165672
        :size 24365}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 378365
        :slice-offset 190037
        :size 12326}])))

(deftest write-index-entries-test
  (let [entries [{:ref-seq-id 0, :start 546609, :span 205262429,
                  :container-offset 324, :slice-offset 563, :size 22007}
                 {:ref-seq-id 0, :start 206547069, :span 42644506,
                  :container-offset 324, :slice-offset 22570, :size 7349}
                 {:ref-seq-id 1, :start 67302, :span 231638920,
                  :container-offset 30272, :slice-offset 563, :size 21618}
                 {:ref-seq-id -1, :start 0, :span 0,
                  :container-offset 354657, :slice-offset 563, :size 23119}
                 {:ref-seq-id -1, :start 0, :span 0,
                  :container-offset 378365, :slice-offset 171, :size 23494}
                 {:ref-seq-id -1, :start 0, :span 0,
                  :container-offset 378365, :slice-offset 23665, :size 23213}]
        f (io/file common/temp-dir "test.cram.crai")]
    (common/with-before-after {:before (common/prepare-cache!)
                               :after (common/clean-cache!)}
      (with-open [w (crai/writer f)]
        (crai/write-index-entries w entries))
      (with-open [r (io/reader (util/compressor-input-stream f))]
        (is (= entries (#'crai/read-index-entries r)))))))
