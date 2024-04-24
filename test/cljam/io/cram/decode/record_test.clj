(ns cljam.io.cram.decode.record-test
  (:require [cljam.io.cram.decode.record :as record]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.sequence :as cseq]
            [cljam.test-common :as common]
            [clojure.test :refer [are deftest is testing]]))

(defn- build-stub-decoders
  ([data] (build-stub-decoders data (fn [_ v] v)))
  ([data wrap-fn]
   (into {}
         (map (fn [[k xs]]
                (let [xs' (volatile! xs)
                      f (fn []
                          (when (seq @xs')
                            (let [v (first @xs')]
                              (vswap! xs' next)
                              (or (some->> v (wrap-fn k))
                                  ;; nil in test data is only used for readability
                                  ;; and has no meaning, so just ignore it
                                  (recur)))))]
                  [k f])))
         data)))

(defn- build-stub-tag-decoders [data]
  (into {}
        (map (fn [[k v]]
               [k (build-stub-decoders v #(array-map :type (str %1) :value %2))]))
        data))

(deftest ref-name-test
  (let [cram-header {:SQ
                     [{:SN "chr1"}
                      {:SN "chr2"}
                      {:SN "chr3"}]}]
    (is (= "chr2" (#'record/ref-name cram-header 1)))
    (is (= "*" (#'record/ref-name cram-header -1)))))

(deftest read-group-id-test
  (let [cram-header {:RG
                     [{:ID "rg001"}
                      {:ID "rg002"}
                      {:ID "rg003"}]}]
    (is (= "rg002" (#'record/read-group-id cram-header 1)))
    (is (nil? (#'record/read-group-id cram-header -1)))))

(def ^:private test-seq-resolver
  (let [seqs (with-open [r (cseq/reader common/test-fa-file)]
               (into {} (map (juxt :name :sequence)) (cseq/read-all-sequences r)))]
    (reify resolver/ISeqResolver
      (resolve-sequence [_ {:keys [chr start end]}]
        (let [s (get seqs chr)]
          (subs s (dec (long start)) end))))))

(deftest record-end-test
  (are [?record ?features ?expected]
       (= ?expected (#'record/record-end ?record ?features))
    {:pos 2, ::record/len 5}
    []
    6

    {:pos 2, ::record/len 5}
    [{:code :subst, :pos 3, :subst 2}]
    6

    {:pos 2, ::record/len 5}
    [{:code :subst, :pos 2, :subst 3}
     {:code :subst, :pos 3, :subst 2}]
    6

    {:pos 2, ::record/len 5}
    [{:code :insert-base, :pos 3, :base (int \A)}]
    5

    {:pos 2, ::record/len 5}
    [{:code :insert-base, :pos 3, :base (int \A)}
     {:code :subst, :pos 4, :subst 1}]
    5

    {:pos 2, ::record/len 5}
    [{:code :insertion, :pos 3, :bases (.getBytes "CA")}]
    4

    {:pos 2, ::record/len 5}
    [{:code :subst, :pos 2, :subst 3}
     {:code :insertion, :pos 3, :bases (.getBytes "CA")}]
    4

    {:pos 2, ::record/len 5}
    [{:code :deletion, :pos 3, :len 2}]
    8

    {:pos 2, ::record/len 5}
    [{:code :deletion, :pos 3, :len 2}
     {:code :subst, :pos 3, :subst 0}]
    8

    {:pos 2, ::record/len 5}
    [{:code :softclip, :pos 4, :bases (.getBytes "TT")}]
    4

    {:pos 2, ::record/len 5}
    [{:code :softclip, :pos 1, :bases (.getBytes "TT")}]
    4))

(deftest record-seq-test
  (let [compression-header {:preservation-map
                            {:SM
                             {\A {0 \C, 1 \G, 2 \T, 3 \N}
                              \C {0 \G, 1 \T, 2 \N, 3 \A}
                              \G {0 \T, 1 \N, 2 \A, 3 \C}
                              \T {0 \N, 1 \A, 2 \C, 3 \G}
                              \N {0 \A, 1 \C, 2 \G, 3 \T}}}}]
    (are [?record ?features ?expected]
         (= ?expected (#'record/record-seq test-seq-resolver compression-header ?record ?features))
      {:rname "ref", :pos 2, :end 6, ::record/len 5}
      []
      "GCATG"

      {:rname "ref", :pos 2, :end 6, ::record/len 5}
      [{:code :subst, :pos 3, :subst 2}]
      "GCTTG"

      {:rname "ref", :pos 2, :end 6, ::record/len 5}
      [{:code :subst, :pos 2, :subst 3}
       {:code :subst, :pos 3, :subst 2}]
      "GATTG"

      {:rname "ref", :pos 2, :end 5, ::record/len 5}
      [{:code :insert-base, :pos 3, :base (int \A)}]
      "GCAAT"

      {:rname "ref", :pos 2, :end 5, ::record/len 5}
      [{:code :insert-base, :pos 3, :base (int \A)}
       {:code :subst, :pos 4, :subst 1}]
      "GCAGT"

      {:rname "ref", :pos 2, :end 4, ::record/len 5}
      [{:code :insertion, :pos 3, :bases (.getBytes "CA")}]
      "GCCAA"

      {:rname "ref", :pos 2, :end 4, ::record/len 5}
      [{:code :subst, :pos 2, :subst 3}
       {:code :insertion, :pos 3, :bases (.getBytes "CA")}]
      "GACAA"

      {:rname "ref", :pos 2, :end 8, ::record/len 5}
      [{:code :deletion, :pos 3, :len 2}]
      "GCGTT"

      {:rname "ref", :pos 2, :end 8, ::record/len 5}
      [{:code :deletion, :pos 3, :len 2}
       {:code :subst, :pos 3, :subst 0}]
      "GCTTT"

      {:rname "ref", :pos 2, :end 4, ::record/len 5}
      [{:code :softclip, :pos 4, :bases (.getBytes "TT")}]
      "GCATT"

      {:rname "ref", :pos 2, :end 4, ::record/len 5}
      [{:code :softclip, :pos 1, :bases (.getBytes "TT")}]
      "TTGCA")))

(deftest record-qual-test
  (are [?record ?features ?qual ?expected]
       (= ?expected
          (#'record/record-qual ?record ?features (build-stub-decoders {:QS ?qual})))
    {::record/flag 0x01, ::record/len 5}
    []
    (->> ["ABCDE"]
         (mapcat #(.getBytes ^String %))
         (map #(- (long %) 33)))
    "ABCDE"

    {::record/flag 0x02, ::record/len 5}
    (->> [{:code :read-base, :pos 1, :qual \A}
          {:code :read-base, :pos 2, :qual \B}
          {:code :read-base, :pos 3, :qual \C}
          {:code :read-base, :pos 4, :qual \D}
          {:code :read-base, :pos 5, :qual \E}]
         (map (fn [f] (update f :qual #(- (int %) 33)))))
    []
    "ABCDE"

    {::record/flag 0x04, ::record/len 5}
    (->> [{:code :score, :pos 1, :qual \A}
          {:code :score, :pos 2, :qual \B}
          {:code :score, :pos 3, :qual \C}
          {:code :score, :pos 4, :qual \D}
          {:code :score, :pos 5, :qual \E}]
         (map (fn [f] (update f :qual #(- (int %) 33)))))
    []
    "ABCDE"

    {::record/flag 0x08, ::record/len 5}
    (->> [{:code :scores, :pos 1, :scores "ABC"}
          {:code :scores, :pos 4, :scores "DE"}]
         (map (fn [f]
                (update f :scores
                        (fn [qs]
                          (->> (.getBytes qs)
                               (map #(- (long %) 33))
                               byte-array))))))
    []
    "ABCDE"))

(deftest record-cigar-test
  (are [?features ?expected]
       (= ?expected
          (->> (#'record/record-cigar {::record/len 5} ?features)
               (map (fn [[n op]] (str n op)))
               (apply str)))
    []
    "5M"

    [{:code :subst, :pos 3, :subst 2}]
    "5M"

    [{:code :subst, :pos 2, :subst 3}
     {:code :subst, :pos 3, :subst 2}]
    "5M"

    [{:code :insert-base, :pos 3, :base (int \A)}]
    "2M1I2M"

    [{:code :insert-base, :pos 3, :base (int \A)}
     {:code :subst, :pos 4, :subst 1}]
    "2M1I2M"

    [{:code :insertion, :pos 3, :bases (.getBytes "CA")}]
    "2M2I1M"

    [{:code :subst, :pos 2, :subst 3}
     {:code :insertion, :pos 3, :bases (.getBytes "CA")}]
    "2M2I1M"

    [{:code :deletion, :pos 3, :len 2}]
    "2M2D3M"

    [{:code :deletion, :pos 3, :len 2}
     {:code :subst, :pos 3, :subst 0}]
    "2M2D3M"

    [{:code :softclip, :pos 4, :bases (.getBytes "TT")}]
    "3M2S"

    [{:code :softclip, :pos 1, :bases (.getBytes "TT")}]
    "2S3M"))

(deftest build-read-features-decoder-test
  (are [?decoders ?expected]
       (let [fs-decoder (#'record/build-read-features-decoder
                         (build-stub-decoders ?decoders))]
         (= ?expected (fs-decoder)))
    {:FN [0]}
    []

    {:FN [2]
     :FC [(int \B) (int \B)]
     :FP [5 10]
     :BA [(int \A) (int \T)]
     :QS [33 0]}
    [{:code :read-base, :pos 5, :base (int \A), :qual 33}
     {:code :read-base, :pos 15, :base (int \T), :qual 0}]

    {:FN [2]
     :FC [(int \X) (int \X)]
     :FP [5 10]
     :BS [0 3]}
    [{:code :subst, :pos 5, :subst 0}
     {:code :subst, :pos 15, :subst 3}]

    {:FN [2]
     :FC [(int \I) (int \I)]
     :FP [5 10]
     :IN ["CA" "TT"]}
    [{:code :insertion, :pos 5, :bases "CA"}
     {:code :insertion, :pos 15, :bases "TT"}]

    {:FN [2]
     :FC [(int \S) (int \S)]
     :FP [1 10]
     :SC ["CA" "TT"]}
    [{:code :softclip, :pos 1, :bases "CA"}
     {:code :softclip, :pos 11, :bases "TT"}]

    {:FN [2]
     :FC [(int \H) (int \H)]
     :FP [1 10]
     :HC [2 5]}
    [{:code :hardclip, :pos 1, :len 2}
     {:code :hardclip, :pos 11, :len 5}]

    {:FN [2]
     :FC [(int \P) (int \P)]
     :FP [1 10]
     :PD [2 5]}
    [{:code :padding, :pos 1, :len 2}
     {:code :padding, :pos 11, :len 5}]

    {:FN [2]
     :FC [(int \D) (int \D)]
     :FP [5 10]
     :DL [2 5]}
    [{:code :deletion, :pos 5, :len 2}
     {:code :deletion, :pos 15, :len 5}]

    {:FN [2]
     :FC [(int \N) (int \N)]
     :FP [5 10]
     :RS [2 5]}
    [{:code :ref-skip, :pos 5, :len 2}
     {:code :ref-skip, :pos 15, :len 5}]

    {:FN [2]
     :FC [(int \i) (int \i)]
     :FP [5 10]
     :BA [(int \A) (int \T)]}
    [{:code :insert-base, :pos 5, :base (int \A)}
     {:code :insert-base, :pos 15, :base (int \T)}]

    {:FN [2]
     :FC [(int \b) (int \b)]
     :FP [5 10]
     :BB ["CAT" "TAG"]}
    [{:code :bases, :pos 5, :bases "CAT"}
     {:code :bases, :pos 15, :bases "TAG"}]

    {:FN [2]
     :FC [(int \q) (int \q)]
     :FP [5 10]
     :QQ ["??@" "###"]}
    [{:code :scores, :pos 5, :scores "??@"}
     {:code :scores, :pos 15, :scores "###"}]

    {:FN [2]
     :FC [(int \Q) (int \Q)]
     :FP [5 10]
     :QS [33 0]}
    [{:code :score, :pos 5, :score 33}
     {:code :score, :pos 15, :score 0}]))

(deftest decode-slice-records-test
  (testing "mapped reads"
    (let [cram-header {:SQ
                       [{:SN "ref"}
                        {:SN "ref2"}]
                       :RG
                       [{:ID "rg001"}
                        {:ID "rg002"}]}
          compression-header {:preservation-map
                              {:RN true
                               :AP true
                               :RR true
                               :SM {\A {0 \C, 1 \G, 2 \T, 3 \N}
                                    \C {0 \A, 1 \G, 2 \T, 3 \N}
                                    \G {0 \A, 1 \C, 2 \T, 3 \N}
                                    \T {0 \A, 1 \C, 2 \G, 3 \N}
                                    \N {0 \A, 1 \C, 2 \G, 3 \T}}
                               :TD [[]
                                    [{:tag :MD, :type \Z}
                                     {:tag :NM, :type \c}]]}}
          slice-header {:ref-seq-id 0
                        :start 1
                        :records 5}
          ds-decoders (build-stub-decoders
                       {:BF [67 67 145 147 65]
                        :CF [3 5 3 1 3]
                        :RI []
                        :RL [5 5 5 5 5]
                        :AP [0 4 5 5 5]
                        :RG [0 0 1 1 -1]
                        :RN (->> ["q001" "q002" "q003" "q004" "q005"]
                                 (map #(.getBytes ^String %)))
                        :MF [1   nil 1   nil 2]
                        :NF [nil 1   nil nil nil]
                        :NS [0   nil 1   nil -1]
                        :NP [151 nil 100 nil 0]
                        :TS [150 nil 0   nil 0]
                        :TL [1 1 1 1 0]
                        :FN [1 1 0 2 0]
                        :FC [(int \X)
                             (int \S)
                             nil
                             (int \I) (int \D)
                             nil]
                        :FP [3
                             1
                             nil
                             2 2
                             nil]
                        :BS [0 nil nil nil nil]
                        :IN (->> [nil nil nil "A" nil]
                                 (keep #(some-> ^String % (.getBytes))))
                        :SC (->> [nil "CC" nil nil nil]
                                 (keep #(some-> ^String % (.getBytes))))
                        :DL [nil nil nil 1 nil]
                        :QS (->> ["HFHHH" "##AAC" "CCCFF" "EBBFF" "AEEEE"]
                                 (mapcat #(.getBytes ^String %))
                                 (map #(- (long %) 33)))
                        :MQ [0 15 60 15 0]})
          tag-decoders (build-stub-tag-decoders
                        {:MD {\Z ["2C2" "3" "5" "3^T2" nil]}
                         :NM {\c [1 0 0 2 nil]}})]
      (is (= [{:qname "q001", :flag 99, :rname "ref", :pos 1, :end 5, :mapq 0,
               :cigar "5M", :rnext "=", :pnext 151, :tlen 150, :seq "AGAAT", :qual "HFHHH"
               :options [{:RG {:type "Z", :value "rg001"}}
                         {:MD {:type "Z", :value "2C2"}}
                         {:NM {:type "c", :value 1}}]}
              {:qname "q002", :flag 99, :rname "ref", :pos 5, :end 7, :mapq 15,
               :cigar "2S3M", :rnext "=", :pnext 15, :tlen 15, :seq "CCTGT", :qual "##AAC"
               :options [{:RG {:type "Z", :value "rg001"}}
                         {:MD {:type "Z", :value "3"}}
                         {:NM {:type "c", :value 0}}]}
              {:qname "q003", :flag 177, :rname "ref", :pos 10, :end 14, :mapq 60,
               :cigar "5M", :rnext "ref2", :pnext 100, :tlen 0, :seq "GATAA", :qual "CCCFF"
               :options [{:RG {:type "Z", :value "rg002"}}
                         {:MD {:type "Z", :value "5"}}
                         {:NM {:type "c", :value 0}}]}
              {:qname "q004", :flag 147, :rname "ref", :pos 15, :end 19, :mapq 15,
               :cigar "1M1I1M1D2M", :rnext "=", :pnext 5, :tlen -15, :seq "GAAAG", :qual "EBBFF"
               :options [{:RG {:type "Z", :value "rg002"}}
                         {:MD {:type "Z", :value "3^T2"}}
                         {:NM {:type "c",  :value 2}}]}
              {:qname "q005", :flag 73, :rname "ref", :pos 20, :end 24, :mapq 0,
               :cigar "5M", :rnext "*", :pnext 0, :tlen 0, :seq "CTGTG", :qual "AEEEE"
               :options []}]
             (record/decode-slice-records test-seq-resolver
                                          cram-header
                                          compression-header
                                          slice-header
                                          ds-decoders
                                          tag-decoders)))))
  (testing "unmapped reads"
    (let [cram-header {:SQ
                       [{:SN "ref"}
                        {:SN "ref2"}]}
          compression-header {:preservation-map
                              {:RN true
                               :AP true
                               :RR true
                               :SM {\A {0 \C, 1 \G, 2 \T, 3 \N}
                                    \C {0 \A, 1 \G, 2 \T, 3 \N}
                                    \G {0 \A, 1 \C, 2 \T, 3 \N}
                                    \T {0 \A, 1 \C, 2 \G, 3 \N}
                                    \N {0 \A, 1 \C, 2 \G, 3 \T}}
                               :TD [[]]}}
          slice-header {:ref-seq-id -1
                        :start 0
                        :records 5}
          ds-decoders (build-stub-decoders
                       {:BF [77 141 77 141 77]
                        :CF [3 3 3 3 3]
                        :RL [5 5 5 5 5]
                        :AP [0 0 0 0 0]
                        :RG [-1 -1 -1 -1 -1]
                        :RN (->> ["q001" "q001" "q002" "q002" "q003"]
                                 (map #(.getBytes ^String %)))
                        :MF [2 2 2 2 2]
                        :NS [-1 -1 -1 -1 -1]
                        :NP [0 0 0 0 0]
                        :TS [0 0 0 0 0]
                        :TL [0 0 0 0 0]
                        :BA (->> ["AATCC" "ATTGT" "TGGTA" "TCTTG" "GCACA"]
                                 (mapcat #(.getBytes ^String %)))
                        :QS (->> ["CCFFF" "BDFAD" "ADDHF" "DDDFD" "BCCFD"]
                                 (mapcat #(.getBytes ^String %))
                                 (map #(- (long %) 33)))})
          tag-decoders (build-stub-tag-decoders {})]
      (is (= [{:qname "q001", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
               :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "AATCC", :qual "CCFFF"
               :options []}
              {:qname "q001", :flag 141, :rname "*", :pos 0, :end 0, :mapq 0,
               :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "ATTGT", :qual "BDFAD"
               :options []}
              {:qname "q002", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
               :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "TGGTA", :qual "ADDHF"
               :options []}
              {:qname "q002", :flag 141, :rname "*", :pos 0, :end 0, :mapq 0,
               :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "TCTTG", :qual "DDDFD"
               :options []}
              {:qname "q003", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
               :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "GCACA", :qual "BCCFD"
               :options []}]
             (record/decode-slice-records test-seq-resolver
                                          cram-header
                                          compression-header
                                          slice-header
                                          ds-decoders
                                          tag-decoders))))))
