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
        :container-offset 327
        :slice-offset 563
        :size 21475}
       {:chr "chr1"
        :start 206547069
        :end (+ 206547069 42644506)
        :container-offset 327
        :slice-offset 22038
        :size 7234}]

      "chr1" 550000 600000
      [{:chr "chr1"
        :start 546609
        :end (+ 546609 205262429)
        :container-offset 327
        :slice-offset 563
        :size 21475}]

      "chr1" 210000000 240000000
      [{:chr "chr1"
        :start 206547069
        :end (+ 206547069 42644506)
        :container-offset 327
        :slice-offset 22038
        :size 7234}]

      "chr1" 200000000 210000000
      [{:chr "chr1"
        :start 546609
        :end (+ 546609 205262429)
        :container-offset 327
        :slice-offset 563
        :size 21475}
       {:chr "chr1"
        :start 206547069
        :end (+ 206547069 42644506)
        :container-offset 327
        :slice-offset 22038
        :size 7234}]

      "*" 0 0
      [{:chr "*"
        :start 0
        :end 0
        :container-offset 368626
        :slice-offset 541
        :size 15676}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 171
        :size 23422}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 23593
        :size 23258}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 46851
        :size 23200}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 70051
        :size 23643}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 93694
        :size 24213}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 117907
        :size 24066}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 141973
        :size 23886}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 165859
        :size 24339}
       {:chr "*"
        :start 0
        :end 0
        :container-offset 384869
        :slice-offset 190198
        :size 11639}])))

(deftest write-index-entries-test
  (let [entries [{:ref-seq-id 0, :start 546609, :span 205262429,
                  :container-offset 327, :slice-offset 563, :size 21475}
                 {:ref-seq-id 0, :start 206547069, :span 42644506,
                  :container-offset 327, :slice-offset 22038, :size 7234}
                 {:ref-seq-id 1, :start 67302, :span 231638920,
                  :container-offset 29628, :slice-offset 563, :size 21098}
                 {:ref-seq-id -1, :start 0, :span 0,
                  :container-offset 368626, :slice-offset 541, :size 15676}
                 {:ref-seq-id -1, :start 0, :span 0,
                  :container-offset 384869, :slice-offset 171, :size 23422}
                 {:ref-seq-id -1, :start 0, :span 0,
                  :container-offset 384869, :slice-offset 23593, :size 23258}]
        f (io/file common/temp-dir "test.cram.crai")]
    (common/with-before-after {:before (common/prepare-cache!)
                               :after (common/clean-cache!)}
      (with-open [w (crai/writer f)]
        (crai/write-index-entries w entries))
      (with-open [r (io/reader (util/compressor-input-stream f))]
        (is (= entries (#'crai/read-index-entries r)))))))
