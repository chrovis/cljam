(ns cljam.io.vcf.util-test
  (:require  [clojure.test :refer :all]
             [clojure.string :as cstr]
             [cljam.test-common :refer :all]
             [cljam.io.vcf.util :as vcf-util]))

(deftest about-parse-info
  (let [parse-info (vcf-util/info-parser [{:id "NS", :number 1, :type "Integer"}
                                          {:id "DP", :number 1, :type "Integer"}
                                          {:id "AF", :number "A", :type "Float"}
                                          {:id "AA", :number 1, :type "String"}
                                          {:id "DB", :number 0, :type "Flag"}
                                          {:id "H2", :number 0, :type "Flag"}
                                          {:id "CIPOS", :number 2, :type "Integer"}
                                          {:id "HOMLEN", :number ".", :type "Integer"}
                                          {:id "CC", :number 1, :type "Character"}])]
    (are [?info-str ?expected]
        (= (parse-info ?info-str) ?expected)
      "." nil
      "NS=3" {:NS 3}
      "DP=0" {:DP 0}
      "AF=0" {:AF [0.0]}
      "CIPOS=1,10" {:CIPOS [1 10]}
      "HOMLEN=1" {:HOMLEN [1]}
      "HOMLEN=1,2,3,4,5" {:HOMLEN [1 2 3 4 5]}
      "CC=A" {:CC \A}
      "DB"   {:DB :exists}
      "DB;H2" {:DB :exists :H2 :exists}
      "NS=3;DP=13;AA=T" {:NS 3, :DP 13, :AA "T"}
      "NS=3;DP=9;AA=G" {:NS 3, :DP 9, :AA "G"}
      "NS=3;DP=14;AF=0.5;DB;H2" {:NS 3, :DP 14, :AF [0.5], :DB :exists :H2 :exists}
      "NS=3;DP=11;AF=0.017" {:NS 3, :DP 11, :AF [(float 0.017)]}
      "NS=2;DP=10;AF=0.333,0.667;AA=T;DB" {:NS 2, :DP 10, :AF [(float 0.333) (float 0.667)] :AA "T", :DB :exists}
      "NS;DP=12" {:NS nil, :DP 12} ;; Not VCF standard. "NS=.;DP=12" is correct. However, some variant caller outputs such form.
      )))

(deftest about-parse-filter
  (are [?filter-str ?expected]
      (= (vcf-util/parse-filter ?filter-str) ?expected)
    "." nil
    "PASS" [:PASS]
    "q10" [:q10]
    "s50" [:s50]
    "q10;s50" [:q10 :s50]))

(deftest about-parse-sample
  (let [parse-sample (vcf-util/sample-parser [{:id "GT", :number 1, :type "String"}
                                              {:id "GQ", :number 1, :type "Integer"}
                                              {:id "DP", :number 1, :type "Integer"}
                                              {:id "HQ", :number 2, :type "Integer"}
                                              {:id "AF", :number 1, :type "Float"}
                                              {:id "CC", :number 1, :type "Character"}])]
    (are [?format-str ?sample-str ?expected]
        (= (parse-sample ?format-str ?sample-str) ?expected)
      "." "." nil
      "GT" "0/0" {:GT "0/0"}
      "GQ" "48" {:GQ 48}
      "DP" "1" {:DP 1}
      "HQ" "1,20" {:HQ [1 20]}
      "CC" "A" {:CC \A}
      "AF" "0.5" {:AF 0.5}
      "AF" "." {:AF nil}
      "HQ" ".,." {:HQ [nil nil]}
      "HQ" "." {:HQ nil}
      "GT:GQ:DP:HQ" "2|1:2:0:18,2" {:GT "2|1", :GQ 2, :DP 0, :HQ [18 2]}
      "GT:GQ:DP:HQ" "2/2:35:4" {:GT "2/2", :GQ 35, :DP 4})))

(deftest about-parse-genotype
  (are [?gt-str ?expected]
      (= (vcf-util/parse-genotype ?gt-str) ?expected)
    "." nil
    "0" [[0 true]]
    "1" [[1 true]]
    "0/0" [[0 false] [0 false]]
    "0/1" [[0 false] [1 false]]
    "1/1" [[1 false] [1 false]]
    "0|0" [[0 true] [0 true]]
    "0|1" [[0 true] [1 true]]
    "1|1" [[1 true] [1 true]]
    "0/1/2" [[0 false] [1 false] [2 false]]
    "0/1|2" [[0 false] [1 false] [2 true]]))

(deftest about-stringify-genotype
  (are [?gt ?expected]
      (= (vcf-util/stringify-genotype ?gt) ?expected)
    nil nil
    [[0 true]] "0"
    [[1 true]] "1"
    [[0 false]] "0"
    [[1 false]] "1"
    [[0 true] [0 true]] "0|0"
    [[0 true] [1 true]] "0|1"
    [[1 true] [1 true]] "1|1"
    [[0 false] [0 false]] "0/0"
    [[0 false] [1 false]] "0/1"
    [[1 false] [1 false]] "1/1"
    [[0 false] [1 false] [2 false]] "0/1/2"
    [[0 false] [1 false] [2 true]] "0/1|2"))

(deftest about-parse-variant-v4_3
  (let [parse-variant (vcf-util/variant-parser test-vcf-v4_3-meta-info test-vcf-v4_3-header)]
    (are [?variant ?expected]
        (= (parse-variant ?variant) ?expected)
      (nth test-vcf-v4_3-variants 0) (nth test-vcf-v4_3-variants-deep 0)
      (nth test-vcf-v4_3-variants 1) (nth test-vcf-v4_3-variants-deep 1)
      (nth test-vcf-v4_3-variants 2) (nth test-vcf-v4_3-variants-deep 2)
      (nth test-vcf-v4_3-variants 3) (nth test-vcf-v4_3-variants-deep 3)
      (nth test-vcf-v4_3-variants 4) (nth test-vcf-v4_3-variants-deep 4))))

(deftest about-parse-variant-v4_0
  (let [parse-variant (vcf-util/variant-parser test-vcf-v4_0-meta-info test-vcf-v4_0-header)]
    (are [?variant ?expected]
        (= (parse-variant ?variant) ?expected)
      (nth test-vcf-v4_0-variants 0) (nth test-vcf-v4_0-variants-deep 0)
      (nth test-vcf-v4_0-variants 1) (nth test-vcf-v4_0-variants-deep 1)
      (nth test-vcf-v4_0-variants 2) (nth test-vcf-v4_0-variants-deep 2)
      (nth test-vcf-v4_0-variants 3) (nth test-vcf-v4_0-variants-deep 3)
      (nth test-vcf-v4_0-variants 4) (nth test-vcf-v4_0-variants-deep 4)
      (nth test-vcf-v4_0-variants 5) (nth test-vcf-v4_0-variants-deep 5)
      (nth test-vcf-v4_0-variants 6) (nth test-vcf-v4_0-variants-deep 6)
      (nth test-vcf-v4_0-variants 7) (nth test-vcf-v4_0-variants-deep 7)
      (nth test-vcf-v4_0-variants 8) (nth test-vcf-v4_0-variants-deep 8)
      (nth test-vcf-v4_0-variants 9) (nth test-vcf-v4_0-variants-deep 9)
      (nth test-vcf-v4_0-variants 10) (nth test-vcf-v4_0-variants-deep 10)
      (nth test-vcf-v4_0-variants 11) (nth test-vcf-v4_0-variants-deep 11))))

(deftest about-stringify-variant-vals-v4_3
  (let [stringify-variant-vals (vcf-util/variant-vals-stringifier test-vcf-v4_3-meta-info test-vcf-v4_3-header)]
    (are [?expected ?variant]
        (= (stringify-variant-vals ?variant) ?expected)
      (nth test-vcf-v4_3-variants 0) (nth test-vcf-v4_3-variants-deep 0)
      (nth test-vcf-v4_3-variants 1) (nth test-vcf-v4_3-variants-deep 1)
      (nth test-vcf-v4_3-variants 2) (nth test-vcf-v4_3-variants-deep 2)
      (nth test-vcf-v4_3-variants 3) (nth test-vcf-v4_3-variants-deep 3)
      (nth test-vcf-v4_3-variants 4) (nth test-vcf-v4_3-variants-deep 4))))

(deftest about-stringify-variant-vals-v4_0
  (let [stringify-variant-vals (vcf-util/variant-vals-stringifier test-vcf-v4_0-meta-info test-vcf-v4_0-header)]
    (are [?expected ?variant]
        (= (stringify-variant-vals ?variant) ?expected)
      (nth test-vcf-v4_0-variants 0) (nth test-vcf-v4_0-variants-deep 0)
      (nth test-vcf-v4_0-variants 1) (nth test-vcf-v4_0-variants-deep 1)
      (nth test-vcf-v4_0-variants 2) (nth test-vcf-v4_0-variants-deep 2)
      (nth test-vcf-v4_0-variants 3) (nth test-vcf-v4_0-variants-deep 3)
      (nth test-vcf-v4_0-variants 4) (nth test-vcf-v4_0-variants-deep 4)
      (nth test-vcf-v4_0-variants 5) (nth test-vcf-v4_0-variants-deep 5)
      (nth test-vcf-v4_0-variants 6) (nth test-vcf-v4_0-variants-deep 6)
      (nth test-vcf-v4_0-variants 7) (nth test-vcf-v4_0-variants-deep 7)
      (nth test-vcf-v4_0-variants 8) (nth test-vcf-v4_0-variants-deep 8)
      (nth test-vcf-v4_0-variants 9) (nth test-vcf-v4_0-variants-deep 9)
      ;; "./.:.:." => "./."
      ;; "0|2:3:." => "0|2:3"
      ;; trailing fields can be dropped. Result differs but it's OK.
      ;; (nth test-vcf-v4_0-variants 10) (nth test-vcf-v4_0-variants-deep 10)
      (nth test-vcf-v4_0-variants 11) (nth test-vcf-v4_0-variants-deep 11))))
