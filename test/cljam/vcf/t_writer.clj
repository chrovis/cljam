(ns cljam.vcf.t-writer
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.vcf.writer :refer :all]))

(deftest stringify-meta-info-pedigree
  (is (= (#'cljam.vcf.writer/stringify-meta-info-pedigree {:name-0 "G0-ID"
                                                           :name-1 "G1-ID"})
         "Name_0=G0-ID,Name_1=G1-ID"))
  (is (= (#'cljam.vcf.writer/stringify-meta-info-pedigree {:name-1 "G1-ID"
                                                           :name-0 "G0-ID"})
         "Name_0=G0-ID,Name_1=G1-ID")))

(deftest stringify-data-line-alt
  (is (= (#'cljam.vcf.writer/stringify-data-line-alt ["C"]) "C"))
  (is (= (#'cljam.vcf.writer/stringify-data-line-alt ["C" "G"]) "C,G"))
  (is (= (#'cljam.vcf.writer/stringify-data-line-alt nil) nil)))

(deftest stringify-data-line-qual
  (is (= (#'cljam.vcf.writer/stringify-data-line-qual 10.0) "10"))
  (is (= (#'cljam.vcf.writer/stringify-data-line-qual 9.6) "9.6"))
  (is (= (#'cljam.vcf.writer/stringify-data-line-qual nil) nil)))
