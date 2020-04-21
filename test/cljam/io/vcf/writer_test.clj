(ns cljam.io.vcf.writer-test
  (:require [clojure.test :refer [deftest is are]]
            [cljam.io.vcf.writer :as vcf-writer])
  (:import [java.io StringWriter BufferedWriter]
           [cljam.io.vcf.writer VCFWriter]))

(deftest stringify-meta-info-pedigree
  (is (= (#'vcf-writer/stringify-structured-line :pedigree
                                                 {:id "SampleID"
                                                  :name-0 "G0-ID"
                                                  :name-1 "G1-ID"})
         "ID=SampleID,Name_0=G0-ID,Name_1=G1-ID")))

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
         "ID=Blood,Genomes=Germline,Mixture=1.,Description=\"test\",Note=\"extra note\"")))

(deftest empty-info-format
  (let [meta-info {:info [{:id "XA", :type "String", :number 1}],
                   :format [{:id "XB", :type "String", :number 1}]}
        header ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO"
                "FORMAT" "SAMPLE01" "SAMPLE02"]]
    (are [?variant ?str]
         (= ?str
            (with-open [sw (StringWriter.)
                        bw (BufferedWriter. sw)
                        w (VCFWriter. nil bw meta-info header)]
              (vcf-writer/write-variants w [?variant])
              (.flush bw)
              (str sw)))
      {:chr "1", :pos 1, :ref "N"}
      "1\t1\t.\tN\t.\t.\t.\t.\t.\t.\t.\n"

      {:chr "2", :pos 2, :ref "N", :info {}}
      "2\t2\t.\tN\t.\t.\t.\t.\t.\t.\t.\n"

      {:chr "3", :pos 3, :ref "N", :info {}, :FORMAT []}
      "3\t3\t.\tN\t.\t.\t.\t.\t.\t.\t.\n"

      {:chr "4", :pos 4, :ref "N", :info {}, :FORMAT [], :SAMPLE01 {}}
      "4\t4\t.\tN\t.\t.\t.\t.\t.\t.\t.\n"

      {:chr "5", :pos 5, :ref "N", :info {:XX "5"}}
      "5\t5\t.\tN\t.\t.\t.\t.\t.\t.\t.\n"

      {:chr "6", :pos 6, :ref "N", :info {:XA "6"}, :FORMAT [:XB], :SAMPLE01 {}}
      "6\t6\t.\tN\t.\t.\t.\tXA=6\tXB\t.\t.\n")))
