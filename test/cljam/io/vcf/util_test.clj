(ns cljam.io.vcf.util-test
  (:require [clojure.test :refer :all]
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
    "./." [[nil false] [nil false]]
    "0|0" [[0 true] [0 true]]
    "0|1" [[0 true] [1 true]]
    "1|1" [[1 true] [1 true]]
    ".|." [[nil true] [nil true]]
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
    [[nil true] [nil true]] ".|."
    [[0 false] [0 false]] "0/0"
    [[0 false] [1 false]] "0/1"
    [[1 false] [1 false]] "1/1"
    [[nil false] [nil false]] "./."
    [[0 false] [1 false] [2 false]] "0/1/2"
    [[0 false] [1 false] [2 true]] "0/1|2"))

(deftest genotype-seq
  (are [?ploidy ?n-alt-alleles ?expected]
      (= ?expected (vcf-util/genotype-seq ?ploidy ?n-alt-alleles))
    1 1 [[0] [1]]
    1 2 [[0] [1] [2]]
    2 1 [[0 0] [0 1] [1 1]]
    2 2 [[0 0] [0 1] [1 1] [0 2] [1 2] [2 2]]
    2 3 [[0 0] [0 1] [1 1] [0 2] [1 2] [2 2] [0 3] [1 3] [2 3] [3 3]]
    3 1 [[0 0 0] [0 0 1] [0 1 1] [1 1 1]]
    3 2 [[0 0 0] [0 0 1] [0 1 1] [1 1 1] [0 0 2]
         [0 1 2] [1 1 2] [0 2 2] [1 2 2] [2 2 2]]))

(deftest genotype-index
  (are [?genotype ?expected]
      (= ?expected (vcf-util/genotype-index ?genotype))
    [0] 0
    [1] 1
    [0 0] 0
    [0 1] 1
    [1 1] 2
    [0 2] 3
    [1 2] 4
    [2 2] 5
    [3 3] 9
    [0 0 0] 0
    [1 1 2] 6
    [1 2 2] 8))

(deftest about-genotypes
  (are [?ploidy ?n-alt-alleles]
      (let [x (vcf-util/genotype-seq ?ploidy ?n-alt-alleles)]
        (= (range (count x)) (map vcf-util/genotype-index x)))
    1 0
    1 1
    1 2
    1 3
    2 0
    2 1
    2 2
    2 3
    2 4
    3 0
    3 1
    3 2
    3 3
    3 4
    4 0
    4 1
    4 2
    4 3
    4 4))

(deftest biallelic-genotype
  (are [?genotype ?target-allele ?expected]
      (= ?expected (vcf-util/biallelic-genotype ?genotype ?target-allele))
    "0" 1 "0"
    "1" 1 "1"
    "2" 1 "0"
    "0/0" 1 "0/0"
    "0/1" 1 "0/1"
    "0/2" 1 "0/0"
    "1/2" 1 "1/0"
    "2|2" 1 "0|0"
    "0/1" 2 "0/0"
    "0|2" 2 "0|1"
    "1/2" 2 "0/1"
    "0/1/2" 2 "0/0/1"
    "2|3|4" 3 "0|1|0"
    ;; a call cannot be made
    nil 0 nil
    "./." 1 "./."
    ;; hemizygous
    "./1" 1 "./1"
    "./2" 2 "./1"
    "./1" 2 "./0"))

(deftest biallelic-coll
  (are [?ploidy ?n-alt-alleles ?target-allele ?coll ?expected]
      (= ?expected
         (vcf-util/biallelic-coll ?ploidy ?n-alt-alleles ?target-allele ?coll))
    2 1 1 [10 20 30] [10 20 30]
    2 2 1 [10 20 30 40 50 60] [10 20 30]
    2 2 2 [10 20 30 40 50 60] [10 40 60]
    3 2 2 [1 2 3 4 5 6 7 8 9 10] [1 5 8 10]))

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

(deftest parse-breakend
  (are [?alt ?expected]
      (= ?expected (vcf-util/parse-breakend ?alt))

    "]13:123456]T" {:chr "13", :pos 123456, :strand :forward,
                    :join :before, :bases "T"}
    "]13:123456]AGTNNNNNCAT" {:chr "13", :pos 123456, :strand :forward,
                              :join :before, :bases "AGTNNNNNCAT"}
    "AGTNNNNNCA[2:321682[" {:chr "2", :pos 321682, :strand :forward,
                            :join :after, :bases "AGTNNNNNCA"}
    "C[<ctg1>:1[" {:chr "<ctg1>", :pos 1, :strand :forward,
                   :join :after, :bases "C"}
    "[13:123457[C" {:chr "13", :pos 123457, :strand :reverse,
                    :join :before, :bases "C"}
    "G]17:198982]" {:chr "17", :pos 198982, :strand :reverse,
                    :join :after, :bases "G"}
    ".[13:123457[" {:chr "13", :pos 123457, :strand :forward,
                    :join :after, :bases "."}
    "]2:321681]A" {:chr "2", :pos 321681, :strand :forward,
                   :join :before, :bases "A"}
    "C[1:1[" {:chr "1", :pos 1, :strand :forward,
              :join :after, :bases "C"}
    "]1:0]A" {:chr "1", :pos 0, :strand :forward,
              :join :before, :bases "A"}
    "C[<ct[g1:123>:1[" {:chr "<ct[g1:123>", :pos 1, :strand :forward,
                        :join :after, :bases "C"}

    ".A" {:join :before, :bases "A"}
    ".TGCA" {:join :before, :bases "TGCA"}
    "G." {:join :after, :bases "G"}
    "TCC." {:join :after, :bases "TCC"}

    "G" nil
    "ATT" nil
    "[1:12T" nil
    "[1:2[T[" nil
    "[1:[T" nil
    "[:1[T" nil
    "[:[T" nil
    "[1:1e[T" nil
    "[1:2]T[" nil
    "]1:1]" nil
    "]1:1[T" nil
    "]1:1T" nil
    "]12]T" nil ;; possibly not malformed
    "N[<[>[>[" nil ;; possibly not malformed
    "." nil
    "*" nil
    ".G." nil
    "..G" nil
    "<>" nil
    "C<ctg1>" nil ;; INS
    "<ctg1>C" nil ;; INS
    "<DUP>" nil
    "<INV>" nil))

(deftest stringify-breakend
  (are [?expected ?bnd]
      (= ?expected (vcf-util/stringify-breakend ?bnd))
    "]13:123456]T" {:chr "13", :pos 123456, :strand :forward,
                    :join :before, :bases "T"}
    "]13:123456]AGTNNNNNCAT" {:chr "13", :pos 123456, :strand :forward,
                              :join :before, :bases "AGTNNNNNCAT"}
    "AGTNNNNNCA[2:321682[" {:chr "2", :pos 321682, :strand :forward,
                            :join :after, :bases "AGTNNNNNCA"}
    "C[<ctg1>:1[" {:chr "<ctg1>", :pos 1, :strand :forward,
                   :join :after, :bases "C"}
    "[13:123457[C" {:chr "13", :pos 123457, :strand :reverse,
                    :join :before, :bases "C"}
    "G]17:198982]" {:chr "17", :pos 198982, :strand :reverse,
                    :join :after, :bases "G"}
    ".[13:123457[" {:chr "13", :pos 123457, :strand :forward,
                    :join :after, :bases "."}
    "]2:321681]A" {:chr "2", :pos 321681, :strand :forward,
                   :join :before, :bases "A"}
    "C[1:1[" {:chr "1", :pos 1, :strand :forward,
              :join :after, :bases "C"}
    "]1:0]A" {:chr "1", :pos 0, :strand :forward,
              :join :before, :bases "A"}
    "C[<ct[g1:123>:1[" {:chr "<ct[g1:123>", :pos 1, :strand :forward,
                        :join :after, :bases "C"}

    ".A" {:join :before, :bases "A"}
    ".TGCA" {:join :before, :bases "TGCA"}
    "G." {:join :after, :bases "G"}
    "TCC." {:join :after, :bases "TCC"}

    nil nil
    nil {}
    nil {:bases "A"}

    ".A" {:bases "A", :join :before, :chr "1", :pos 1}
    ".A" {:bases "A", :join :before, :chr "1", :strand :forward}
    ".A" {:bases "A", :join :before, :pos 1, :strand :forward}))

(deftest inspect-allele
  (are [?ref ?alt ?expected]
      (= ?expected (vcf-util/inspect-allele ?ref ?alt))

    "A" ""    {:type :no-call} ;; malformed
    "A" "."   {:type :no-call}
    "A" nil   {:type :no-call}

    "A" "*"   {:type :spanning-deletion}

    "A" "X"   {:type :unspecified}
    "A" "<*>" {:type :unspecified}
    "A" "<X>" {:type :unspecified}

    "A" "A"   {:type :ref}
    "A" "a"   {:type :ref}
    "AA" "AA" {:type :ref}
    "AA" "Aa" {:type :ref}

    "A" "<DUP>"     {:type :id, :id "DUP"}
    "A" "<INV>"     {:type :id, :id "INV"}
    "A" "<NON_REF>" {:type :id, :id "NON_REF"} ;; not :unspecified

    "A"  "T"      {:type :snv, :ref \A, :alt \T, :offset 0}
    "TC" "TG"     {:type :snv, :ref \C, :alt \G, :offset 1}
    "TC" "GC"     {:type :snv, :ref \T, :alt \G, :offset 0}
    "TCA" "TAA"   {:type :snv, :ref \C, :alt \A, :offset 1}
    "TCAG" "tcAC" {:type :snv, :ref \G, :alt \C, :offset 3}
    "TGTAT" "TGcAT" {:type :snv, :ref \T, :alt \c, :offset 2}

    "TG"   "Gc"   {:type :mnv, :ref "TG",  :alt "Gc",  :offset 0}
    "TCG"  "tGA"  {:type :mnv, :ref "CG",  :alt "GA",  :offset 1}
    "TGCA" "TCCC" {:type :mnv, :ref "GCA", :alt "CCC", :offset 1} ;; two snvs?
    "TGGTA" "TCcAA" {:type :mnv, :ref "GGT", :alt "CcA", :offset 1}

    "TC"  "T"     {:type :deletion, :n-bases 1, :offset 0, :deleted "C"}
    "TTC" "TC"    {:type :deletion, :n-bases 1, :offset 0, :deleted "T"}
    "GTC" "G"     {:type :deletion, :n-bases 2, :offset 0, :deleted "TC"}
    "TCG" "TG"    {:type :deletion, :n-bases 1, :offset 0, :deleted "C"}
    "TGCA" "TGC"  {:type :deletion, :n-bases 1, :offset 2, :deleted "A"}
    ;; ambiguous
    "TCGCG" "TCG" {:type :deletion, :n-bases 2, :offset 2, :deleted "CG"}
    ;; ambiguous
    "TAAACCCTAAA" "TAA" {:type :deletion, :n-bases 8,
                         :offset 2, :deleted "ACCCTAAA"}

    "C"   "CTAG"  {:type :insertion, :n-bases 3, :offset 0, :inserted "TAG"}
    "TC"  "TTC"   {:type :insertion, :n-bases 1, :offset 0, :inserted "T"}
    "GTC" "GTCT"  {:type :insertion, :n-bases 1, :offset 2, :inserted "T"}
    "TCG" "TCAG"  {:type :insertion, :n-bases 1, :offset 1, :inserted "A"}
    ;; ambiguous
    "GCG" "GCGCG" {:type :insertion, :n-bases 2, :offset 2, :inserted "CG"}
    ;; ambiguous
    "ATTTTTTTTTTTTT" "ATTTTTTTTTTTTTTT" {:type :insertion, :n-bases 2,
                                         :offset 13, :inserted "TT"}

    "T" "<ctg1>C" {:type :complete-insertion, :join :before,
                   :base \C, :id "ctg1"}
    "T" "c<ctg1>" {:type :complete-insertion, :join :after,
                   :base \c, :id "ctg1"}

    "G" "G."    {:type :breakend, :join :after, :bases "G"}
    "A" ".A"    {:type :breakend, :join :before, :bases "A"}
    "T" "TCC."  {:type :breakend, :join :after, :bases "TCC"}
    "A" ".TGCA" {:type :breakend, :join :before, :bases "TGCA"}
    "T" "]13:123456]T" {:type :breakend, :join :before,
                        :strand :forward, :chr "13", :pos 123456, :bases "T"}

    "T" "<ctg1>CT" {:type :other} ;; malformed
    "T" "<CT"      {:type :other} ;; malformed
    "T" "C,A"      {:type :other}
    "." "A"        {:type :other} ;; malformed
    "" "A"         {:type :other} ;; malformed
    nil "A"        {:type :other} ;; malformed
    nil nil        {:type :other} ;; malformed
    "R" "A"        {:type :other} ;; malformed

    "TAC" "GC" {:type :complex}
    "TG" "TAC" {:type :complex}
    "TCA" "GGGG" {:type :complex}
    "AAAA" "CAC" {:type :complex}))
