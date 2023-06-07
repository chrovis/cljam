(ns cljam.io.vcf.util.validator-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [cljam.io.vcf.util.validator :as validator]))

(def base-header
  ["CHROM" "POS" "ID" "REF" "ALT"
   "QUAL" "FILTER" "INFO" "FORMAT"])

(deftest check-each-samples-test
  (testing "type check"
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:DP 100} :ref "A"}
          [:normal]
          {:DP {:type "Integer" :number 1}})))

    (is (= (get-in (#'validator/check-each-samples
                    {:normal {:DP "str100"} :ref "A"}
                    [:normal]
                    {:DP {:type "Integer" :number 1}})
                   [:normal :DP])
           [(str "Not match type declaration. "
                 "Requires Integer, but got [\"str100\"].")]))
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:GT "1|0"} :ref "A"}
          [:normal]
          {:GT {:type "String" :number 1}})))
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:A [1 2]} :ref "A"}
          [:normal]
          {:A {:type "Integer" :number 2}}))))

  (testing "number check"
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:A [1 2]} :alt ["A" "T"] :ref "A"}
          [:normal]
          {:A {:type "Integer" :number 2}})))
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:A [1 2]} :alt ["A" "T"] :ref "A"}
          [:normal]
          {:A {:type "Integer" :number "A"}})))
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:A [1 2 3]} :alt ["A" "T"] :ref "A"}
          [:normal]
          {:A {:type "Integer" :number "R"}})))
    (is (nil?
         (#'validator/check-each-samples
          {:normal {:A [1 2 3 4 5 6]} :alt ["A" "T"] :ref "A"}
          [:normal]
          {:A {:type "Integer" :number "G"}})))
    (is (= (get-in
            (#'validator/check-each-samples
             {:normal {:A [1]} :ref "A"}
             [:normal]
             {:A {:type "Integer" :number 2}})
            [:normal :A])
           ["Invalid number of elements. Requires 2, but got 1."]))
    (is (= (get-in
            (#'validator/check-each-samples
             {:normal {:A [1]} :alt ["A" "T"] :ref "A"}
             [:normal]
             {:A {:type "Integer" :number "A"}})
            [:normal :A])
           ["Invalid number of elements. Requires A, but got 1."])))
  (testing "not contain meta"
    (is (= (get-in
            (#'validator/check-each-samples
             {:normal {:A [1] :b [100]} :ref "A"}
             [:normal]
             {:A {:type "Integer" :number 1}})
            [:normal :b])
           ["Key :b not in meta."]))))

(deftest make-validator-test
  (is (= (:alt ((validator/make-validator
                 {:contig [{:id "chr10"}]} base-header {:filetype :vcf})
                {:chr "chr10" :pos 10 :ref "A" :alt "AT"}))
         ["Must be a sequence."])))

(deftest validate-variant-test
  (testing "contig check"
    (is (nil?
         (validator/validate-variant
          (validator/make-validator
           {:contig [{:id "chr10"}]}
           base-header
           {})
          {:chr "chr10" :pos 10 :ref "A" :alt ["AT"]})))
    (is (= (:chr
            (validator/validate-variant
             (validator/make-validator
              {:contig [{:id "c hr10"}]}
              base-header
              {})
             {:chr "c hr10" :pos 10 :ref "A" :alt ["AT"]}))
           ["Must not contain whitespaces commas or angle brackets, but got [\\space]."]))
    (is (= (:chr
            (validator/validate-variant
             (validator/make-validator {} base-header {:filetype :bcf})
             {:chr "chr10" :pos 10 :ref "A" :alt ["AT"]}))
           ["CHROM chr10 must be declared in a ##contig field in the header."])))
  (testing "ref check"
    (is (= (:ref
            (validator/validate-variant
             (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
             {:chr "chr10" :pos 10 :ref "AP"}))
           ["Must consist of ACGTNacgtn."]))
    (is (= (:ref
            (validator/validate-variant
             (validator/make-validator
              {:contig [{:id "chr10"}]}
              base-header
              {})
             {:chr "chr10" :pos 10
              :ref "" :alt ["GT"]}))
           ["Must consist of ACGTNacgtn."])))
  (testing "alt check"
    (is (=
         (:alt
          (validator/validate-variant
           (validator/make-validator
            {:contig [{:id "chr10"}]}
            base-header
            {})
           {:chr "chr10" :pos 10 :ref "A" :alt "AT"}))
         ["Must be a sequence."]))
    (is (= (:alt
            (validator/validate-variant
             (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
             {:chr "chr10" :pos 10 :ref "A" :alt ""}))
           ["Must be a sequence."]))
    (is (= (:alt (validator/validate-variant
                  (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
                  {:chr "chr10" :pos 10 :ref "A" :alt ["AP"]}))
           ["Contains bad allele at index 0."]))
    (is (= (:alt (validator/validate-variant
                  (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
                  {:chr "chr10" :pos 10 :ref "A" :alt ["" 1]}))
           [(str "An allele cannot be empty but got an empty allele at index 0"
                 "., "
                 "An allele must be string but got other type at index 1.")]))
    (is (nil?
         (validator/validate-variant
          (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
          {:chr "chr10" :pos 10 :alt ["A" "<DEL>" nil] :ref "G"})))
    (is (nil?
         (validator/validate-variant
          (validator/make-validator
           {:contig [{:id "chr10"}]}
           base-header
           {})
          {:chr "chr10" :pos 10 :alt ["G]17:198982]" "]13:123456]T"]
           :ref "G"}))))
  (testing "qual check"
    (is (nil?
         (validator/validate-variant
          (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
          {:chr "chr10" :pos 10
           :qual 0.1
           :ref "G" :alt ["GT"]}))))
  (testing "vcf filter check"
    (is (nil?
         (validator/validate-variant
          (validator/make-validator
           {:contig [{:id "chr10"}]}
           base-header {})
          {:chr "chr10" :pos 10
           :filter [:PASS]
           :ref "G" :alt ["GT"]}))))
  (is (nil?
       (validator/validate-variant
        (validator/make-validator

         {:contig [{:id "chr10"}]}
         base-header
         {})
        {:chr "chr10" :pos 10
         :filter [:q10 :s5]
         :ref "G" :alt ["GT"]})))
  (testing "bcf filter check"
    (is (= (:filter
            (validator/validate-variant
             (validator/make-validator
              {:contig [{:id "chr10"}]}
              base-header
              {:filetype :bcf})
             {:chr "chr10" :pos 10
              :filter [:q10]
              :ref "G" :alt ["GT"]}))
           ["Filter identifiers [:q10] must in meta-info."]))
    (is (nil?
         (validator/validate-variant
          (validator/make-validator

           {:contig [{:id "chr10"}] :filter [{:id "q10"}]}
           base-header
           {:filetype :bcf})
          {:chr "chr10" :pos 10
           :filter [:q10]
           :ref "G" :alt ["GT"]})))
    (is (nil?
         (validator/validate-variant
          (validator/make-validator
           {:contig [{:id "chr10"}] :filter [{:id "q10"}]}
           base-header
           {:filetype :bcf})
          {:chr "chr10" :pos 10 :filter [:PASS] :ref "G" :alt ["GT"]}))))
  (testing "info check"
    (is (= (validator/validate-variant
            (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
            {:chr "chr10" :pos 10 :info {:GT 100} :ref "A" :alt ["AT"]})
           {:GT
            ["Key :GT is not contained in ##info fields in the header."]})))
  (testing "format check"
    (is (nil?
         (validator/validate-variant
          (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
          {:chr "chr10" :pos 10 :FORMAT ["A"] :ref "G" :alt ["GT"]})))))

(deftest validate-variants-test
  (is (nil?
       (validator/validate-variants
        (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
        [{:chr "chr10" :pos 10 :ref "A" :alt ["AT"]}])))
  (is (= (validator/validate-variants
          (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
          [{:chr "chr10" :pos 10 :ref "A" :alt ["AT"]}
           {:chr "chr10" :pos 10 :ref "A" :alt "A"}])
         [{:alt ["Must be a sequence."]}])))

(deftest check-variant-test
  (is (thrown? clojure.lang.ExceptionInfo
               (validator/check-variant
                (validator/make-validator {} base-header {})
                {:chr "chr10" :pos 100})))
  (let [variant {:chr "chr10" :pos 10 :ref "A" :alt ["AT"]}]
    (is (= (validator/check-variant
            (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
            variant)
           variant))))

(deftest check-variants-test
  (testing "bcf"
    (is (thrown? clojure.lang.ExceptionInfo
                 (doall (validator/check-variants
                         (validator/make-validator {} base-header {:filetype :bcf})
                         [{:chr "chr10" :pos 100}])))))
  (testing "vcf"
    (is (thrown? clojure.lang.ExceptionInfo
                 (doall (validator/check-variants
                         (validator/make-validator {} base-header {})
                         [{:chr "<chr10>" :pos 100}]))))
    (let [org [{:chr "chr10" :pos 10 :ref "A" :alt ["AT"]}]]
      (is (= (validator/check-variants
              (validator/make-validator {:contig [{:id "chr10"}]} base-header {})
              org) org)))))
