(ns cljam.io.cram.writer-test
  (:require [cljam.io.cram.encode.record :as record]
            [cljam.io.cram.writer :as writer]
            [clojure.test :refer [deftest is testing]]))

(deftest container-index-entries-test
  (testing "no multiple reference slices"
    (is (= [{:ref-seq-id 0, :start 123, :span 456
             :container-offset 100, :slice-offset 200, :size 1000}
            {:ref-seq-id 0, :start 987, :span 654
             :container-offset 100, :slice-offset 1200, :size 900}
            {:ref-seq-id 1, :start 1234, :span 5678
             :container-offset 100, :slice-offset 2100, :size 1100}]
           (#'writer/container-index-entries
            100
            {:landmarks [200 1200 2100]}
            [{:header {:ref-seq-id 0, :start 123, :span 456}
              :size 1000}
             {:header {:ref-seq-id 0, :start 987, :span 654}
              :size 900}
             {:header {:ref-seq-id 1, :start 1234, :span 5678}
              :size 1100}]
            [[{::record/ref-index 0, :pos 123 ::record/end 273}
              {::record/ref-index 0, :pos 428 ::record/end 578}]
             [{::record/ref-index 0, :pos 987 ::record/end 1137}
              {::record/ref-index 0, :pos 1490 ::record/end 1640}]
             [{::record/ref-index 1, :pos 1234 ::record/end 1384}
              {::record/ref-index 1, :pos 6761 ::record/end 6911}]])))
    (is (= [{:ref-seq-id -1, :start 0, :span 0
             :container-offset 100, :slice-offset 200, :size 1000}
            {:ref-seq-id -1, :start 0, :span 0
             :container-offset 100, :slice-offset 1200, :size 900}]
           (#'writer/container-index-entries
            100
            {:landmarks [200 1200]}
            [{:header {:ref-seq-id -1, :start 0, :span 0}
              :size 1000}
             {:header {:ref-seq-id -1, :start 0, :span 0}
              :size 900}]
            [[{::record/ref-index -1, :pos 0 ::record/end 0}
              {::record/ref-index -1, :pos 0 ::record/end 0}]
             [{::record/ref-index -1, :pos 0 ::record/end 0}
              {::record/ref-index -1, :pos 0 ::record/end 0}]]))))
  (testing "some multiple reference slices"
    (is (= [{:ref-seq-id 0, :start 123, :span 456
             :container-offset 100, :slice-offset 200, :size 1000}
            {:ref-seq-id 0, :start 987, :span 654
             :container-offset 100, :slice-offset 1200, :size 900}
            {:ref-seq-id 1, :start 1234, :span 5678
             :container-offset 100, :slice-offset 1200, :size 900}
            ;; Both of :start/:span should be zero for unmapped slices in respect of
            ;; the CRAM specification, but it's guaranteed by the CRAI writer,
            ;; so it doesn't matter what values these keys actually take here
            {:ref-seq-id -1, :start 0, :span 1
             :container-offset 100, :slice-offset 1200, :size 900}]
           (#'writer/container-index-entries
            100
            {:landmarks [200 1200]}
            [{:header {:ref-seq-id 0, :start 123, :span 456}
              :size 1000}
             {:header {:ref-seq-id -2, :start 0, :span 0}
              :size 900}]
            [[{::record/ref-index 0, :pos 123 ::record/end 273}
              {::record/ref-index 0, :pos 428 ::record/end 578}]
             [{::record/ref-index 0, :pos 987 ::record/end 1138}
              {::record/ref-index 0, :pos 1490 ::record/end 1640}
              {::record/ref-index 1, :pos 1234 ::record/end 1384}
              {::record/ref-index 1, :pos 6761 ::record/end 6911}
              {::record/ref-index -1, :pos 0 ::record/end 0}
              {::record/ref-index -1, :pos 0 ::record/end 0}]])))))
