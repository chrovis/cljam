(ns cljam.io.vcf.writer-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.vcf.writer :as vcf-writer]))

(deftest stringify-meta-info-pedigree
  (is (= (#'vcf-writer/stringify-structured-line :pedigree
                                                 {:name-0 "G0-ID"
                                                  :name-1 "G1-ID"})
         "Name_0=G0-ID,Name_1=G1-ID"))
  (is (= (#'vcf-writer/stringify-structured-line :pedigree
                                                 {:name-1 "G1-ID"
                                                  :name-0 "G0-ID"})
         "Name_0=G0-ID,Name_1=G1-ID")))

(deftest stringify-data-line-alt
  (is (= (#'vcf-writer/stringify-data-line-alt ["C"]) "C"))
  (is (= (#'vcf-writer/stringify-data-line-alt ["C" "G"]) "C,G"))
  (is (= (#'vcf-writer/stringify-data-line-alt nil) nil)))

(deftest stringify-data-line-qual
  (is (= (#'vcf-writer/stringify-data-line-qual 10.0) "10"))
  (is (= (#'vcf-writer/stringify-data-line-qual 9.6) "9.6"))
  (is (= (#'vcf-writer/stringify-data-line-qual nil) nil)))

(deftest stringify-meta-info-sample
  (is (= (#'vcf-writer/stringify-structured-line :sample
                                                 {:id "Blood"
                                                  :genomes "Germline"
                                                  :mixture "1."
                                                  :description "test"
                                                  :note "extra note"})
         "ID=Blood,genomes=\"Germline\",mixture=\"1.\",description=\"test\",note=\"extra note\"")))
