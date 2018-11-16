(ns cljam.io.vcf.util.normalize-test
  (:require [clojure.test :refer :all]
            [cljam.io.vcf.util.normalize :as norm]
            [cljam.io.protocols :as protocols]))

(deftest splitter
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
        header ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO" "FORMAT" "SAMPLE01" "SAMPLE02"]]
    (are [?variant ?expected]
        (= ?expected ((norm/splitter meta-info header) ?variant))

      {:chr "1", :pos 1, :ref "T", :alt ["A"], :id nil, :qual 100.0, :filter [:PASS],
       :info {:XF :exists, :DP 10, :XT [1 2], :AC [2], :XR [100.0 200.0], :XG [0 1 2]}
       :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
       :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/1", :AC [40], :XR [2 3], :XG [10 11 12]},
       :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/0/1", :AC [80], :XR [1 7], :XG [0 1 2]}}

      [{:chr "1", :pos 1, :ref "T", :alt ["A"], :id nil, :qual 100.0, :filter [:PASS],
        :info {:XF :exists, :DP 10, :XT [1 2], :AC [2], :XR [100.0 200.0], :XG [0 1 2]}
        :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/1", :AC [40], :XR [2 3], :XG [10 11 12]},
        :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/0/1", :AC [80], :XR [1 7], :XG [0 1 2]}}]

      ;; -----

      {:chr "2", :pos 20, :ref "T", :alt ["A", "C"], :id nil, :qual 100.0, :filter [:PASS],
       :info {:XF :exists, :DP 10, :XT [1 2], :AC [2 4], :XR [100.0 200.0 300.0], :XG [0 1 2 3 4 5]}
       :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
       :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/2", :AC [40 50], :XR [2 3 4], :XG [10 11 12 13 14 15]},
       :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/1/2", :AC [80 90], :XR [1 7 9], :XG [0 1 2 3 4 5 6 7 8 9]}}

      [{:chr "2", :pos 20, :ref "T", :alt ["A"], :id nil, :qual 100.0, :filter [:PASS],
        :info {:XF :exists, :DP 10, :XT [1 2], :AC [2], :XR [100.0 200.0], :XG [0 1 2]}
        :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/0", :AC [40], :XR [2 3], :XG [10 11 12]},
        :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/1/0", :AC [80], :XR [1 7], :XG [0 1 2 3]}}
       {:chr "2", :pos 20, :ref "T", :alt ["C"], :id nil, :qual 100.0, :filter [:PASS],
        :info {:XF :exists, :DP 10, :XT [1 2], :AC [4], :XR [100.0 300.0], :XG [0 3 5]}
        :FORMAT [:XF :DP :XT :GT :AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :GT "0/1", :AC [50], :XR [2 4], :XG [10 13 15]},
        :SAMPLE02 {:XF :exists, :DP 300, :XT [30 40], :GT "0/0/1", :AC [90], :XR [1 9], :XG [0 4 7 9]}}]

      ;; -----

      {:chr "3", :pos 300, :ref "T", :alt ["A", "C"], :id nil, :qual 100.0, :filter [:PASS],
       :info {}
       :FORMAT [:XF :DP :XT:AC :XR :XG],
       :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :AC [40 50], :XR [2 3 4], :XG [10 11 12 13 14 15]},
       :SAMPLE02 {:XF :exists, :DP 20, :XT [10 20], :AC [40 50], :XR [2 3 4], :XG [10 11 12 13 14 15]}}
      [{:chr "3", :pos 300, :ref "T", :alt ["A"], :id nil, :qual 100.0, :filter [:PASS],
        :info {}
        :FORMAT [:XF :DP :XT:AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :AC [40], :XR [2 3], :XG [10 11 12]},
        :SAMPLE02 {:XF :exists, :DP 20, :XT [10 20], :AC [40], :XR [2 3], :XG [10 11 12]}}
       {:chr "3", :pos 300, :ref "T", :alt ["C"], :id nil, :qual 100.0, :filter [:PASS],
        :info {}
        :FORMAT [:XF :DP :XT:AC :XR :XG],
        :SAMPLE01 {:XF :exists, :DP 20, :XT [10 20], :AC [50], :XR [2 4], :XG [10 13 15]},
        :SAMPLE02 {:XF :exists, :DP 20, :XT [10 20], :AC [50], :XR [2 4], :XG [10 13 15]}}])))

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

(deftest realign
  (testing "not affected"
    (are [?seq ?variant ?expected]
        (= ?expected (norm/realign (seq-reader ?seq) ?variant))
      "G" ;; ref
      {:pos 1, :ref "G", :alt ["G"]}
      {:pos 1, :ref "G", :alt ["G"]}

      "G"
      {:pos 1, :ref "G", :alt ["T"]}
      {:pos 1, :ref "G", :alt ["T"]}

      "G" ;; deleted
      {:pos 1, :ref "G", :alt ["*"]}
      {:pos 1, :ref "G", :alt ["*"]}

      "TGGG"
      {:pos 1, :ref "TGGG", :alt ["TAC" "TG" "TGGGG", "AC"]}
      {:pos 1, :ref "TGGG", :alt ["TAC" "TG" "TGGGG", "AC"]}

      ""
      {}
      {}

      ""
      {:pos 1, :ref "A", :alt nil}
      {:pos 1, :ref "A", :alt nil}

      "NNNNNNAC"
      {:pos 7, :ref "A", :alt ["a", "AT"]}
      {:pos 7, :ref "A", :alt ["a", "AT"]}

      "CAT"
      {:pos 1, :ref "C", :alt ["CC"]}
      {:pos 1, :ref "C", :alt ["CC"]}))

  (testing "realigned"
    (are [?seq ?variant ?expected]
        (= ?expected (norm/realign (seq-reader ?seq) ?variant))
      "AT"
      {:pos 1, :ref "AT", :alt nil}
      {:pos 1, :ref "A", :alt nil}

      "AT"
      {:pos 1, :ref "AT", :alt ["TT"]}
      {:pos 1, :ref "A", :alt ["T"]}

      "AT"
      {:pos 1, :ref "AT", :alt ["AA"]}
      {:pos 2, :ref "T", :alt ["A"]}

      "ATTTTTTTTTTTTT"
      {:pos 1, :ref "ATTTTTTTTTTTTT", :alt ["ATTTTTTTTTTTTTTT"]}
      {:pos 1, :ref "A", :alt ["ATT"]}

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
      {:pos 1, :ref "CAT", :alt ["AT"]}
      {:pos 1, :ref "CA", :alt ["A"]}

      "CAC"
      {:pos 1, :ref "CAC", :alt ["CC", "CTC"]}
      {:pos 1, :ref "CA", :alt ["C", "CT"]}

      "GTTTTTTTTTTTTT"
      {:pos 1, :ref "GTTTTTTTTTTTTT", :alt ["GTTTTTTTTTTTTTTC", "GTTTTTTTTTTTTTT"]}
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
      "C" ;; INS
      {:pos 1, :ref "C", :alt ["C<ctg1>"]}
      {:pos 1, :ref "C", :alt ["C<ctg1>"]}

      "NC" ;; BND
      {:pos 2, :ref "C", :alt ["CTT[1:123["]}
      {:pos 2, :ref "C", :alt ["CTT[1:123["]}

      "NNC" ;; Symbolic
      {:pos 3, :ref "C", :alt ["<DUP>"]}
      {:pos 3, :ref "C", :alt ["<DUP>"]})))
