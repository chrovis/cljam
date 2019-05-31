(ns cljam.io.vcf.util.normalize-test
  (:require
   [clojure.test :refer :all]
   [cljam.test-common :refer :all]
   [cljam.io.vcf.util.normalize :as norm]
   [cljam.io.protocols :as protocols]
   [cljam.io.sequence :as io-seq]
   [cljam.util.chromosome :as chr]))

(deftest make-splitter
  (let [meta-info {:info [{:id "XF" :number 0}
                          {:id "DP" :number 1}
                          {:id "XT" :number 2}
                          {:id "AC" :number "A"}
                          {:id "XR" :number "R"}
                          {:id "XG" :number "G"}]
                   :format [{:id "XF" :number 0}
                            {:id "DP" :number 1}
                            {:id "XT" :number 2}
                            {:id "GT" :number 1}
                            {:id "AC" :number "A"}
                            {:id "XR" :number "R"}
                            {:id "XG" :number "G"}]}
        header ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO"
                "FORMAT" "SAMPLE01" "SAMPLE02"]]
    (are [?variant ?expected]
         (= ?expected ((norm/make-splitter meta-info header) ?variant))

      {:chr "1", :pos 1, :ref "T", :alt ["A"],
       :id nil, :qual 100.0, :filter [:PASS],
       :info {:XF :exists, :DP 10, :XT [1 2],
              :AC [2], :XR [100.0 200.0], :XG [0 1 2]}
       :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
       :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20],
                  :GT "0/1", :AC [40], :XR [2 3], :XG [10 11 12]},
       :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40],
                  :GT "0/0/1", :AC [80], :XR [1 7], :XG [0 1 2]}}

      [{:chr "1", :pos 1, :ref "T", :alt ["A"],
        :id nil, :qual 100.0, :filter [:PASS],
        :info {:XF :exists, :DP 10, :XT [1 2],
               :AC [2], :XR [100.0 200.0], :XG [0 1 2]}
        :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20],
                   :GT "0/1", :AC [40], :XR [2 3], :XG [10 11 12]},
        :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40],
                   :GT "0/0/1", :AC [80], :XR [1 7], :XG [0 1 2]}}]

      ;; -----

      {:chr "2", :pos 20, :ref "T", :alt ["A", "C"],
       :id nil, :qual 100.0, :filter [:PASS],
       :info {:XF :exists, :DP 10, :XT [1 2],
              :AC [2 4], :XR [100.0 200.0 300.0], :XG [0 1 2 3 4 5]}
       :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
       :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/2",
                  :AC [40 50], :XR [2 3 4], :XG [10 11 12 13 14 15]},
       :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/1/2",
                  :AC [80 90], :XR [1 7 9], :XG [0 1 2 3 4 5 6 7 8 9]}}

      [{:chr "2", :pos 20, :ref "T", :alt ["A"],
        :id nil, :qual 100.0, :filter [:PASS],
        :info {:XF :exists, :DP 10, :XT [1 2],
               :AC [2], :XR [100.0 200.0], :XG [0 1 2]}
        :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/0",
                   :AC [40], :XR [2 3], :XG [10 11 12]},
        :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/1/0",
                   :AC [80], :XR [1 7], :XG [0 1 2 3]}}
       {:chr "2", :pos 20, :ref "T", :alt ["C"],
        :id nil, :qual 100.0, :filter [:PASS],
        :info {:XF :exists, :DP 10, :XT [1 2],
               :AC [4], :XR [100.0 300.0], :XG [0 3 5]}
        :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/1",
                   :AC [50], :XR [2 4], :XG [10 13 15]},
        :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/0/1",
                   :AC [90], :XR [1 9], :XG [0 4 7 9]}}]

      ;; -----

      {:chr "3", :pos 300, :ref "T", :alt ["A", "C"],
       :id nil, :qual 100.0, :filter [:PASS],
       :info {}
       :FORMAT [:XF :DP :XT:AC :XR :XG],
       :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20],
                  :AC [40 50], :XR [2 3 4], :XG [10 11 12 13 14 15]},
       :SAMPLE02 {:XF :exists, :DP 20, :XT [10 20],
                  :AC [40 50], :XR [2 3 4], :XG [10 11 12 13 14 15]}}
      [{:chr "3", :pos 300, :ref "T", :alt ["A"],
        :id nil, :qual 100.0, :filter [:PASS],
        :info {}
        :FORMAT [:XF :DP :XT:AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20],
                   :AC [40], :XR [2 3], :XG [10 11 12]},
        :SAMPLE02 {:XF :exists, :DP 20, :XT [10 20],
                   :AC [40], :XR [2 3], :XG [10 11 12]}}
       {:chr "3", :pos 300, :ref "T", :alt ["C"],
        :id nil, :qual 100.0, :filter [:PASS],
        :info {}
        :FORMAT [:XF :DP :XT:AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20],
                   :AC [50], :XR [2 4], :XG [10 13 15]},
        :SAMPLE02 {:XF :exists, :DP 20, :XT [10 20],
                   :AC [50], :XR [2 4], :XG [10 13 15]}}])))

(defn- seq-reader [s]
  (reify protocols/ISequenceReader
    (read-sequence [_ {:keys [^long start ^long end]} _]
      (subs s (dec start) end))))

(deftest same-ref?
  (are [?seq ?variant ?expected]
       (= ?expected (norm/same-ref? (seq-reader ?seq) ?variant))
    "A" {:pos 1, :ref "A"} true
    "ATT" {:pos 2, :ref "tt"} true
    "TTT" {:pos 3, :ref "A"} false
    "ATGC" {:pos 4, :ref "N"} false))

(deftest trim-right
  (are [?seq ?pos ?ref ?alts ?expected]
       (= ?expected
          (@#'norm/trim-right
           (seq-reader ?seq)
           {:pos ?pos, :ref ?ref :alt ?alts} 3))
    "AATGACCGACCGACCTTGA"
    11 "CGA" ["C"]
    {:pos 11, :ref "CGA", :alt ["C"]}

    "AATGACCGACCGACCTTGA"
    11 "CGA" ["CA"]
    {:pos 11, :ref "CG", :alt ["C"]}

    "AATGACCGACCGACCTTGA"
    11 "CGA" ["CA" "CGAA"]
    {:pos 11, :ref "CG", :alt ["C" "CGA"]}

    "AATGACCGACCGACCTTGA"
    11 "CGAC" ["TCGAC" "ACGAC" "GCGAC"]
    {:pos 8, :ref "GAC", :alt ["GACT" "GACA" "GACG"]}

    "ATGCACTCCGTTGCATCCCCCTG"
    17 "C" ["CC"]
    {:pos 14, :ref "CAT", :alt ["CATC"]}

    "ATGCACTCCGTTGCATCCCCCTG"
    18 "C" ["CC"]
    {:pos 15, :ref "AT", :alt ["ATC"]}

    "ATGCACTCCGTTGCATCCCCCTG"
    19 "C" ["CC"]
    {:pos 16, :ref "T", :alt ["TC"]}

    "ATGCACTCCGTTGCATCCCCCTG"
    20 "C" ["CC"]
    {:pos 14, :ref "CAT", :alt ["CATC"]}

    "ATGCACTCCGTTGCATCCCCCTG"
    21 "C" ["CC"]
    {:pos 15, :ref "AT", :alt ["ATC"]}

    "ATGATGATGATG"
    11 "TG" ["TATG" "TATGATG"]
    {:pos 8, :ref "TG", :alt ["TGAT" "TGATATG"]}

    "ACCCCCCCCC"
    9 "CC" ["C"]
    {:pos 1, :ref "AC", :alt ["A"]}

    "ACCCCCCCCC"
    8 "CCC" ["C" "CC" "CCCC"]
    {:pos 1, :ref "ACC", :alt ["A" "AC" "ACCC"]}

    "TACCCCCCCCC"
    9 "CCC" ["C" "CC" "CCCC"]
    {:pos 1, :ref "TACC", :alt ["TA" "TAC" "TACCC"]}

    "GTACCCCCCCCC"
    10 "CCC" ["C" "CC" "CCCC"]
    {:pos 1, :ref "GTACC", :alt ["GTA" "GTAC" "GTACCC"]}

    "CGTACCCCCCCCC"
    11 "CCC" ["C" "CC" "CCCC"]
    {:pos 2, :ref "GTACC", :alt ["GTA" "GTAC" "GTACCC"]}

    "TCGTACCCCCCCCC"
    12 "CCC" ["C" "CC" "CCCC"]
    {:pos 3, :ref "GTACC", :alt ["GTA" "GTAC" "GTACCC"]}

    "CCCCC"
    5 "C" ["CC"]
    {:pos 1, :ref "C", :alt ["CC"]}))

(deftest realign
  (testing "not affected"
    (are [?seq ?variant ?expected]
         (= ?expected (norm/realign (seq-reader ?seq) ?variant))
      "G"
      {:pos 1, :ref "G", :alt ["T"]}
      {:pos 1, :ref "G", :alt ["T"]}

      "TGGG"
      {:pos 1, :ref "TGGG", :alt ["TAC" "TG" "TGGGG", "AC"], :info {:END 4}}
      {:pos 1, :ref "TGGG", :alt ["TAC" "TG" "TGGGG", "AC"], :info {:END 4}}

      ""
      {:pos 10, :ref "A", :alt nil}
      {:pos 10, :ref "A", :alt nil}

      "NNNNNNAC"
      {:pos 7, :ref "A", :alt ["a", "AT"], :info {:END 7}}
      {:pos 7, :ref "A", :alt ["a", "AT"], :info {:END 7}}

      "CAT"
      {:pos 1, :ref "C", :alt ["CC"]}
      {:pos 1, :ref "C", :alt ["CC"]}))

  (testing "realigned"
    (are [?seq ?variant ?expected]
         (= ?expected (norm/realign (seq-reader ?seq) ?variant))
      ""
      {:pos 10, :ref "AA", :alt nil, :info {:END 11}}
      {:pos 10, :ref "A", :alt nil, :info {:END 10}}

      "AT"
      {:pos 1, :ref "AT", :alt nil, :info {:END 2}}
      {:pos 1, :ref "A", :alt nil, :info {:END 1}}

      "ATGC"
      {:pos 2, :ref "TGC", :alt nil, :info {:END 4}}
      {:pos 2, :ref "T", :alt nil, :info {:END 2}}

      "AT"
      {:pos 1, :ref "AT", :alt ["TT"], :info {:END 2}}
      {:pos 1, :ref "A", :alt ["T"], :info {:END 1}}

      "AT"
      {:pos 1, :ref "AT", :alt ["AA"], :info {:END 2}}
      {:pos 2, :ref "T", :alt ["A"], :info {:END 2}}

      "ATTTTTTTTTTTTT"
      {:pos 1, :ref "ATTTTTTTTTTTTT", :alt ["ATTTTTTTTTTTTTTT"], :info {:END 15}}
      {:pos 1, :ref "A", :alt ["ATT"], :info {:END 1}}

      "GATG"
      {:pos 1, :ref "GATG", :alt ["GACT"]}
      {:pos 3, :ref "TG", :alt ["CT"]}

      "TAAACCCTAAA"
      {:pos 1, :ref "TAAACCCTAAA", :alt ["TAA" "TAACCCTAAA"]}
      {:pos 1, :ref "TAAACCCTA", :alt ["T" "TAACCCTA"]}

      "CACA"
      {:pos 1, :ref "CACA", :alt ["CAC"]}
      {:pos 3, :ref "CA", :alt ["C"]}

      "CAT"
      {:pos 1, :ref "CAT", :alt ["AT"], :info {:END 3}}
      {:pos 1, :ref "CA", :alt ["A"], :info {:END 2}}

      "CAC"
      {:pos 1, :ref "CAC", :alt ["CC", "CTC"]}
      {:pos 1, :ref "CA", :alt ["C", "CT"]}

      "GTTTTTTTTTTTTT"
      {:pos 1, :ref "GTTTTTTTTTTTTT",
       :alt ["GTTTTTTTTTTTTTTC", "GTTTTTTTTTTTTTT"]}
      {:pos 14, :ref "T", :alt ["TTC", "TT"]}

      "ACTCTGGGGGGGGGGGGGGGGGGGGAGGGGA"
      {:pos 22, :ref "G", :alt ["GAG"]}
      {:pos 21, :ref "G", :alt ["GGA"]}

      "ACTCTGGGGGGGGGGGGGGGGGGGGAGGGGA"
      {:pos 22, :ref "G", :alt ["GAG", "GCG"]}
      {:pos 21, :ref "G", :alt ["GGA", "GGC"]}))

  (testing "skipped"
    (are [?seq ?variant ?expected]
         (= ?expected (norm/realign (seq-reader ?seq) ?variant))
      ""
      {}
      {}

      "G" ;; ref
      {:pos 1, :ref "G", :alt ["G"], :info {:END 1}}
      {:pos 1, :ref "G", :alt ["G"], :info {:END 1}}

      "ATAAAAAAAAA" ;; ref
      {:pos 10, :ref "AA", :alt ["AA"], :info {:END 11}}
      {:pos 10, :ref "AA", :alt ["AA"], :info {:END 11}}

      "G" ;; spanning deletion
      {:pos 1, :ref "G", :alt ["G" "*"], :info {:END 2}}
      {:pos 1, :ref "G", :alt ["G" "*"], :info {:END 2}}

      "C" ;; INS
      {:pos 1, :ref "C", :alt ["C<ctg1>"]}
      {:pos 1, :ref "C", :alt ["C<ctg1>"]}

      "NC" ;; BND
      {:pos 2, :ref "C", :alt ["CTT[1:123["]}
      {:pos 2, :ref "C", :alt ["CTT[1:123["]}

      "NNC" ;; Symbolic
      {:pos 3, :ref "C", :alt ["<DUP>"]}
      {:pos 3, :ref "C", :alt ["<DUP>"]})))

(deftest realign-real-seq
  (are [?variant ?realigned]
       (= ?realigned
          (with-open [r (io-seq/reader medium-twobit-file)]
            (norm/realign r ?variant)))
    {:chr "chr1", :pos 30658, :ref "T", :alt ["TT"]}
    {:chr "chr1", :pos 30639, :ref "C", :alt ["CT"]}

    {:chr "chr1", :pos 30640, :ref "T", :alt ["TCCT"]}
    {:chr "chr1", :pos 30636, :ref "T", :alt ["TTCC"]}))

(deftest normalize
  (is (= [{:chr "1", :pos 2, :id nil, :ref "A", :alt ["AT"],
           :qual nil, :filter "PASS", :info {:AC [1]},
           :format [:GT], :SAMPLE01 {:GT "0/1"} :SAMPLE02 {:GT "0/0"}}
          {:chr "1", :pos 15, :id nil, :ref "T", :alt ["TTC"],
           :qual nil, :filter "PASS", :info {:AC [2]},
           :format [:GT], :SAMPLE01 {:GT "1/0"} :SAMPLE02 {:GT "0/1"}}]
         (sort-by
          (juxt (comp chr/chromosome-order-key :chr) :pos)
          (norm/normalize
           (seq-reader "TATTTTTTTTTTTTTCTTTTT")
           {:filter [{:id "PASS"}],
            :info [{:id "AC", :type "Integer", :number "A"}],
            :format [{:id "GT", :type "String", :number 1}]}
           ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO"
            "FORMAT" "SAMPLE01" "SAMPLE02"]
           [{:chr "1", :pos 15, :id nil, :ref "TC", :alt ["TTCC" "TTC"],
             :qual nil, :filter "PASS", :info {:AC [2 1]},
             :format [:GT], :SAMPLE01 {:GT "1/2"} :SAMPLE02 {:GT "0/1"}}])))))
