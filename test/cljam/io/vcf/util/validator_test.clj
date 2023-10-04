(ns cljam.io.vcf.util.validator-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [cljam.io.vcf.util.validator :as v]))

(deftest merge-validation-results-test
  (are [res1 res2 expected] (= expected (#'v/merge-validation-results res1 res2))
    nil nil nil
    nil (#'v/error [:chr] "bad contig") {:errors {[:chr] ["bad contig"]}}
    (#'v/error [:chr] "bad contig") nil {:errors {[:chr] ["bad contig"]}}

    (#'v/error [:chr] "bad contig")
    (#'v/error [:pos] "bad pos")
    {:errors
     {[:chr] ["bad contig"]
      [:pos] ["bad pos"]}}

    (#'v/error [:chr] "bad contig")
    (#'v/error [:chr] "not defined")
    {:errors {[:chr] ["bad contig" "not defined"]}}))

(deftest prepend-keys-test
  (are [res keys expected] (= expected (#'v/prepend-keys res keys))
    nil [:foo] nil
    (#'v/error [:chr] "bad contig") [:foo] {:errors {[:foo :chr] ["bad contig"]}}))

(deftest prep-contig-defs-test
  (is (= {"1" {:id "1" :length 248956422}
          "2" {:id "2" :length 242193529}}
         (#'v/prep-contig-defs nil
                               [{:id "1" :length 248956422}
                                {:id "2" :length 242193529}]))))

(deftest validate-chrom-test
  (testing "file-type :vcf"
    (let [validator (v/map->VCFValidator {})
          prepped (#'v/prep-contig-defs validator
                                        [{:id "1" :length 248956422}
                                         {:id "2" :length 242193529}
                                         {:id "HLA-A*01:01:01:01" :length 3503}])
          validator (assoc validator :defs {:contig prepped})]
      (are [variant expected]
           (= expected (#'v/validate-chrom validator variant))
        {:chr "1"} nil
        {:chr "HLA-A*01:01:01:01"} nil

        {:chr 1}
        {:errors
         {[:chr]
          [(str ":chr must be a non-empty string that consists of characters "
                "other than whitespace, commas or angle brackets, but got 1")]}}

        {:CHR "1"}
        {:errors
         {[:chr]
          [(str ":chr must be a non-empty string that consists of characters "
                "other than whitespace, commas or angle brackets, but got nil")]}}

        {:chr "chr 1"}
        {:errors
         {[:chr]
          [(str ":chr must be a non-empty string that consists of characters "
                "other than whitespace, commas or angle brackets, but got "
                "\"chr 1\"")]}}

        {:chr "X"}
        {:warnings {[:chr] ["Contig X not defined in meta info"]}})))
  (testing "file-type :bcf"
    (let [validator (v/map->VCFValidator {})
          prepped (#'v/prep-contig-defs validator
                                        [{:id "1" :length 248956422}
                                         {:id "2" :length 242193529}])
          validator (assoc validator :defs {:contig prepped})]
      (is (= {:warnings {[:chr] ["Contig X not defined in meta info"]}}
             (#'v/validate-chrom validator {:chr "X"}))))))

(deftest validate-pos-test
  (are [variant expected] (= expected (#'v/validate-pos nil variant))
    {:pos 1} nil

    {:pos "1"}
    {:errors {[:pos] [":pos must be integer, but got \"1\""]}}

    {:POS 1}
    {:errors {[:pos] [":pos must be integer, but got nil"]}}))

(deftest validate-ref-test
  (are [variant expected] (= expected (#'v/validate-ref nil variant))
    {:ref "A"} nil
    {:ref "ctg"} nil

    {:ref \A}
    {:errors
     {[:ref]
      [(str ":ref must be a non-empty string consisting of A/C/G/T/N "
            "(case insensitive), but got \\A")]}}

    {:ref "B"}
    {:errors
     {[:ref]
      [(str ":ref must be a non-empty string consisting of A/C/G/T/N "
            "(case insensitive), but got \"B\"")]}}

    {:REF "A"}
    {:errors
     {[:ref]
      [(str ":ref must be a non-empty string consisting of A/C/G/T/N "
            "(case insensitive), but got nil")]}}))

(deftest validate-alt-test
  (are [variant expected] (= expected (#'v/validate-alt nil variant))
    {:ref "A" :alt ["G"]} nil
    {:ref "A" :alt ["G" "<DEL>"]} nil
    {:ref "G" :alt ["G]17:198982]" "]13:123456]T"]} nil

    {:ref "A" :alt "G"}
    {:errors
     {[:alt] [":alt must be a sequence of strings or nil, but got \"G\""]}}

    {:ref "A" :alt ["G" \C]}
    {:errors
     {[:alt 1] [(str "Every allele in :alt must be a non-empty string or nil, "
                     "but got \\C")]}}

    {:ref "A" :alt ["G" \C ""]}
    {:errors
     {[:alt 1] [(str "Every allele in :alt must be a non-empty string or nil, "
                     "but got \\C")]
      [:alt 2] [(str "Every allele in :alt must be a non-empty string or nil, "
                     "but got \"\"")]}}))

(deftest validate-qual-test
  (are [variant expected] (= expected (#'v/validate-qual nil variant))
    {:qual nil} nil
    {:qual 60} nil

    {:qual "60"}
    {:errors {[:qual] [":qual must be a number or nil, but got \"60\""]}}))

(deftest prep-filter-defs-test
  (is (= {:q10 {:id "q10"}
          :s50 {:id "s50"}}
         (#'v/prep-filter-defs nil [{:id "q10"} {:id "s50"}]))))

(deftest validate-filter-test
  (testing "file-type :vcf"
    (let [validator (v/map->VCFValidator {})
          prepped (#'v/prep-filter-defs validator [{:id "q10"} {:id "s50"}])
          validator (assoc validator :defs {:filter prepped})]
      (are [variant expected] (= expected (#'v/validate-filter validator variant))
        {:filter nil} nil
        {:filter [:q10 :s50]} nil

        {:filter :q10}
        {:errors
         {[:filter] [":filter must be a sequence of keywords or nil, but got :q10"]}}

        {:filter [:q10 "s50"]}
        {:errors
         {[:filter 1]
          [(str "Every filter in :filter must be a keyword consisting "
                "of characters other than whitespace or semicolons, "
                "but got \"s50\"")]}}

        {:filter ["q10" :unknown]}
        {:warnings
         {[:filter 1] ["Filter :unknown not defined in meta info"]}
         :errors
         {[:filter 0]
          [(str "Every filter in :filter must be a keyword consisting "
                "of characters other than whitespace or semicolons, "
                "but got \"q10\"")]}})))
  (testing "file-type :bcf"
    (let [validator (v/map->VCFValidator {:file-type :bcf})
          prepped (#'v/prep-filter-defs validator [{:id "q10"} {:id "s50"}])
          validator (assoc validator :defs {:filter prepped})]
      (is (= {:errors
              {[:filter 0]
               [(str "Every filter in :filter must be a keyword consisting "
                     "of characters other than whitespace or semicolons, "
                     "but got \"q10\"")]
               [:filter 1] ["Filter :unknown not defined in meta info"]}}
             (#'v/validate-filter validator {:filter ["q10" :unknown]}))))))

(deftest make-field-type-validator
  (are [field-def field-val expected]
       (= expected
          (let [f (#'v/make-field-type-validator field-def)]
            (f field-val)))
    {:id "AC" :type "Integer" :number "A"} [50] nil
    {:id "AC" :type "Integer" :number "A"} ["50"]
    {:errors
     {[:AC 0] ["Wrong type of value: :AC expects Integer, but got \"50\""]}}

    {:id "AC" :type "Integer" :number "A"} [(bit-shift-left 1 31)]
    {:errors
     {[:AC 0] [(str "Wrong type of value: :AC expects Integer, but got "
                    (bit-shift-left 1 31))]}}

    {:id "BQ" :type "Float" :number 1} [30.0] nil
    {:id "BQ" :type "Float" :number 1} ["30.0"]
    {:errors
     {[:BQ 0] ["Wrong type of value: :BQ expects Float, but got \"30.0\""]}}

    {:id "TYPE" :type "Character" :number 1} [\A] nil
    {:id "TYPE" :type "Character" :number 1} [(int \A)]
    {:errors
     {[:TYPE 0] ["Wrong type of value: :TYPE expects Character, but got 65"]}}

    {:id "CIGAR" :type "String" :number "A"} ["150M"] nil

    {:id "CIGAR" :type "String" :number "A"} [150M]
    {:errors
     {[:CIGAR 0] ["Wrong type of value: :CIGAR expects String, but got 150M"]}}

    {:id "VALIDATED" :type "Flag" :number 0} [true] nil))

(deftest make-field-number-validator
  (are [field-def num-alts field-val expected]
       (= expected
          (let [f (#'v/make-field-number-validator 2 field-def)]
            (f num-alts field-val)))
    {:id "BQ" :type "Float" :number 1} 1 [30.0] nil
    {:id "BQ" :type "Float" :number 1} 1 [30.0 40.0]
    {:errors
     {[:BQ] ["Wrong number of values: :BQ expects 1 value(s), but got 2 value(s)"]}}

    {:id "HQ" :type "Integer" :number 2} 1 [30 40] nil
    {:id "HQ" :type "Integer" :number 2} 1 [30]
    {:errors
     {[:HQ] ["Wrong number of values: :HQ expects 2 value(s), but got 1 value(s)"]}}

    {:id "AC" :type "Integer" :number "A"} 1 [50] nil
    {:id "AC" :type "Integer" :number "A"} 1 [50 60]
    {:errors
     {[:AC] ["Wrong number of values: :AC expects 1 value(s), but got 2 value(s)"]}}

    {:id "AD" :type "Integer" :number "R"} 1 [50 60] nil
    {:id "AD" :type "Integer" :number "R"} 1 [50]
    {:errors
     {[:AD] ["Wrong number of values: :AD expects 2 value(s), but got 1 value(s)"]}}

    {:id "GP" :type "Float" :number "G"} 1 [0.5 0.3 0.2] nil
    {:id "GP" :type "Float" :number "G"} 1 [0.5 0.3]
    {:errors
     {[:GP] ["Wrong number of values: :GP expects 3 value(s), but got 2 value(s)"]}}))

(deftest make-info-field-validator-test
  (are [variant expected] (= expected
                             (let [f (#'v/make-info-field-validator
                                      2
                                      {:id "AC" :type "Integer" :number "A"})]
                               (f :AC variant)))
    {:alt ["A"] :info nil} nil
    {:alt ["A"] :info {}} nil
    {:alt ["A"] :info {:AC [10]}} nil
    {:alt ["A" "C"] :info {:AC [10 20]}} nil

    {:alt ["A"] :info {:AC ["10"]}}
    {:errors
     {[:info :AC 0] ["Wrong type of value: :AC expects Integer, but got \"10\""]}}

    {:alt ["A"] :info {:AC [20 10]}}
    {:errors
     {[:info :AC] ["Wrong number of values: :AC expects 1 value(s), but got 2 value(s)"]}}))

(deftest prep-info-field-defs-test
  (is (= {:DP {:id "DP" :type "Integer" :number 1 :fn true}
          :AF {:id "AF" :type "Float" :number "A" :fn true}}
         (into {}
               (map (fn [[id field-def]] [id (update field-def :fn fn?)]))
               (#'v/prep-info-field-defs {:ploidy 2}
                                         [{:id "DP" :type "Integer" :number 1}
                                          {:id "AF" :type "Float" :number "A"}])))))

(deftest validate-info-fields-test
  (let [validator (v/map->VCFValidator {:ploidy 2})
        prepped (#'v/prep-info-field-defs validator
                                          [{:id "DP" :type "Integer" :number 1}
                                           {:id "AF" :type "Float" :number "A"}])
        validator (assoc validator :defs {:info prepped})]
    (are [variant expected] (= expected (#'v/validate-info-fields validator variant))
      {:alt ["A"] :info {:DP 10 :AF [0.5]}} nil
      {:alt ["A"] :info {:DP 10}} nil
      {:alt ["A"] :info {:AF [0.5]}} nil

      {:alt ["A"] :info {:DP [10 "20"] :AF ["0.5"]}}
      {:errors
       {[:info :DP] ["Wrong number of values: :DP expects 1 value(s), but got 2 value(s)"]
        [:info :DP 1] ["Wrong type of value: :DP expects Integer, but got \"20\""]
        [:info :AF 0] ["Wrong type of value: :AF expects Float, but got \"0.5\""]}}

      {:alt ["A"] :info {:AD [10]}}
      {:errors
       {[:info :AD] ["Info key :AD not defined in meta info"]}})))

(deftest make-gt-field-validator-test
  (are [variant expected] (= expected
                             (let [f (#'v/make-gt-field-validator
                                      2
                                      {:id "AD" :type "Integer" :number "R"})]
                               (f :sample01 :AD variant)))
    {:alt ["A"] :sample01 nil} nil
    {:alt ["A"] :sample01 {}} nil
    {:alt ["A"] :sample01 {:AD [30 10]}} nil
    {:alt ["A" "C"] :sample01 {:AD [30 20 10]}} nil

    {:alt ["A"] :sample01 {:AD [30 "10"]}}
    {:errors
     {[:sample01 :AD 1] ["Wrong type of value: :AD expects Integer, but got \"10\""]}}

    {:alt ["A"] :sample01 {:AD [30 20 10]}}
    {:errors
     {[:sample01 :AD] ["Wrong number of values: :AD expects 2 value(s), but got 3 value(s)"]}}))

(deftest prep-format-defs-test
  (is (= {:AD {:id "AD" :type "Integer" :number "R" :fn true}
          :GP {:id "GP" :type "Float" :number "G" :fn true}
          :GT {:id "GT" :type "String" :number 1 :fn true}}
         (into {}
               (map (fn [[id field-def]] [id (update field-def :fn fn?)]))
               (#'v/prep-format-defs {:ploidy 2}
                                     [{:id "AD" :type "Integer" :number "R"}
                                      {:id "GP" :type "Float" :number "G"}
                                      {:id "GT" :type "String" :number 1}])))))

(deftest validate-format-test
  (testing "file-type :vcf"
    (let [validator (v/map->VCFValidator {:ploidy 2})
          prepped (#'v/prep-format-defs validator
                                        [{:id "AD" :type "Integer" :number "R"}
                                         {:id "GP" :type "Float" :number "G"}
                                         {:id "GT" :type "String" :number 1}])
          validator (assoc validator :defs {:format prepped})]
      (are [variant expected] (= expected (#'v/validate-format validator variant))
        {} nil
        {:FORMAT []} nil
        {:FORMAT [:GT :AD :GP]} nil

        {:FORMAT :AD}
        {:errors
         {[:FORMAT] [":FORMAT must be a sequence of keywords or nil, but got :AD"]}}

        {:FORMAT [:AD]}
        {:errors
         {[:FORMAT 0] ["First genotype key must be :GT, but got :AD"]}}

        {:FORMAT [:GT :AD "DP"]}
        {:errors
         {[:FORMAT 2] ["Every genotype key in :FORMAT must be keyword, but got \"DP\""]}}

        {:FORMAT [:GT :AD :UNKNOWN]}
        {:warnings
         {[:FORMAT 2] ["Genotype key :UNKNOWN not defined in meta info"]}})))
  (testing "file-type :bcf"
    (let [validator (v/map->VCFValidator {:ploidy 2 :file-type :bcf})
          prepped (#'v/prep-format-defs validator
                                        [{:id "AD" :type "Integer" :number "R"}
                                         {:id "GP" :type "Float" :number "G"}
                                         {:id "GT" :type "String" :number 1}])
          validator (assoc validator :defs {:format prepped})]
      (is (= {:errors
              {[:FORMAT 2] ["Genotype key :UNKNOWN not defined in meta info"]}}
             (#'v/validate-format validator {:FORMAT [:GT :AD :UNKNOWN]}))))))

(deftest validate-samples-test
  (let [validator (v/map->VCFValidator {:ploidy 2
                                        :samples [:sample01 :sample02]})
        prepped (#'v/prep-format-defs validator
                                      [{:id "AD" :type "Integer" :number "R"}
                                       {:id "GP" :type "Float" :number "G"}
                                       {:id "GT" :type "String" :number 1}])
        validator (assoc validator :defs {:format prepped})]
    (are [variant expected] (= expected (#'v/validate-samples validator variant))
      {:alt ["A"] :FORMAT [:GT :AD :GP]} nil

      {:alt ["A"]
       :FORMAT [:GT :AD :GP]
       :sample01 {:GT "0/1" :AD [30 10] :GP [0.5 0.3 0.2]}
       :sample02 {:GT "0/1" :AD [20 20] :GP [0.2 0.6 0.2]}}
      nil

      {:alt ["A"]
       :FORMAT [:GT :AD :GP]
       :sample01 {:GT "0/0" :AD [30] :GP [0.3 0.2 0.1]}
       :sample02 {:GT "0/1" :AD [20 20] :GP [0.2 0.6 "0.2"]}}
      {:errors
       {[:sample01 :AD] ["Wrong number of values: :AD expects 2 value(s), but got 1 value(s)"]
        [:sample02 :GP 2] ["Wrong type of value: :GP expects Float, but got \"0.2\""]}})))

(def mandatory-header-columns
  ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO"])

(def test-validator
  (v/make-validator {:contig [{:id "1" :length 248956422}
                              {:id "2" :length 242193529}]
                     :filter [{:id "q10"} {:id "s50"}]
                     :info [{:id "DP" :type "Integer" :number 1}
                            {:id "AF" :type "Float" :number "A"}]
                     :format [{:id "AD" :type "Integer" :number "R"}
                              {:id "GP" :type "Float" :number "G"}
                              {:id "GT" :type "String" :number 1}]}
                    (conj mandatory-header-columns
                          :FORMAT :sample01 :sample02)))

(def test-variant
  {:chr "1" :pos 12345 :filter :PASS :ref "A" :alt ["G"] :qual 60
   :info {:DP 50 :AF [0.5]} :FORMAT [:GT :AD :GP]
   :sample01 {:GT "0/1" :AD [30 20] :GP [0.5 0.3 0.2]}})

(deftest validate-data-record-test
  (are [variant expected] (= expected
                             (-> (#'v/validate-data-record test-validator variant)
                                 (get :errors)
                                 keys
                                 set))
    test-variant #{}

    (update test-variant :chr #(Long/parseLong %))
    #{[:chr]}

    (update test-variant :pos str)
    #{[:pos]}

    (update test-variant :filter name)
    #{[:filter]}

    (update test-variant :ref first)
    #{[:ref]}

    (update test-variant :alt first)
    #{[:alt]}

    (update test-variant :qual str)
    #{[:qual]}

    (update-in test-variant [:info :AF] rest)
    #{[:info :AF]}

    (update test-variant :FORMAT first)
    #{[:FORMAT]}

    (update-in test-variant [:sample01 :AD] rest)
    #{[:sample01 :AD]})

  (is (= {:errors {[] ["Variant must be a map, but got \"foo\""]}}
         (#'v/validate-data-record test-validator "foo"))))

(deftest validate-variant-test
  (is (nil? (v/validate-variant test-validator test-variant)))
  (let [broken-variant (update test-variant :pos str)
        res (v/validate-variant test-validator broken-variant)]
    (is (= #{[:pos]} (set (keys (:errors res)))))
    (is (= broken-variant (:variant res)))))

(deftest validate-variants
  (let [broken-variant (update test-variant :pos (comp str inc))
        variants [test-variant broken-variant]
        res (v/validate-variants test-validator variants)]
    (is (= 1 (count res)))
    (is (= broken-variant (:variant (first res))))
    (is (= #{[:pos]} (set (keys (:errors (first res))))))))

(deftest check-variant-test
  (is (= test-variant (v/check-variant test-validator test-variant)))
  (let [broken-variant (update test-variant :pos str)
        ex (try
             (v/check-variant test-validator broken-variant)
             (catch Exception e e))
        res (ex-data ex)]
    (is (= broken-variant (:variant res)))
    (is (= #{[:pos]} (set (keys (:errors res)))))))

(deftest check-variants-test
  (let [variants [test-variant (update test-variant :pos inc)]]
    (is (= variants (doall (v/check-variants test-validator variants)))))
  (let [broken-variant (update test-variant :pos (comp str inc))
        variants [test-variant broken-variant]
        ex (try
             (doall (v/check-variants test-validator variants))
             (catch Exception e e))
        res (ex-data ex)]
    (is (= broken-variant (:variant res)))
    (is (= #{[:pos]} (set (keys (:errors res)))))))
