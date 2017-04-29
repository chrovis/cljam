(ns cljam.t-common
  (:require [digest]
            [clojure.java.io :refer [file]]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cavia.core :as cavia :refer [defprofile with-profile]]))

(defn- _in-cloverage? []
  (try
    (eval '(var cloverage.coverage/*covered*))
    true
    (catch Throwable _ nil)))

(def ^:private in-cloverage? (memoize _in-cloverage?))

(defn- expand-deftest [sym args body]
  (if (in-cloverage?)
    nil
    `(clojure.test/deftest ~sym ~args ~@body)))

(defmacro deftest-slow [sym args & body]
  (expand-deftest (with-meta sym {:slow true}) args body))

(defmacro deftest-heavy [sym args & body]
  (expand-deftest (with-meta sym {:heavy true}) args body))

(defmacro deftest-slow-heavy [sym args & body]
  (expand-deftest (with-meta sym {:slow true :heavy true}) args body))

(defprofile mycavia
  {:resources [{:id "large.bam"
                :url "https://test.chrov.is/data/GSM721144_H3K36me3.nodup.bam"
                :sha1 "ad282c3779120057abc274ad8fad1910a4ad867b"}
               {:id "large.bai"
                :url "https://test.chrov.is/data/GSM721144_H3K36me3.nodup.bam.bai"
                :sha1 "afe9ffea88433f35f9395360201583e52c3b3cd9"}
               {:id "large.bed.gz"
                :url "https://test.chrov.is/data/test3_summits.bed.gz"
                :sha1 "dcc3ba10c8432be3094cbf5d6fb1b577317e3429"}
               {:id "large.tbi"
                :url "https://test.chrov.is/data/test3_summits.bed.gz.tbi"
                :sha1 "1aff56f9961c0b93c6de3a190f02d3264c27a9c7"}]})

(defn prepare-cavia! []
  (with-profile mycavia
    (cavia/without-print (cavia/get!))
    (cavia/verify)))

(defn clean-cavia! []
  (with-profile mycavia
    (cavia/clean!)))

(def not-throw? (constantly true))

(defmacro with-before-after [params & body]
  (assert (map? params))
  (let [before-expr (:before params)
        after-expr (:after params)]
    `(do
       ~before-expr
       (try
         ~@body
         (finally ~after-expr)))))

(defn just-map? [checker-map target-map]
  (when (= (set (keys checker-map)) (set (keys target-map)))
    (every? (fn [[k pred]]
              (pred (get target-map k)))
            checker-map)))

;;; slurp (for test)
(defn slurp-sam-for-test [f]
  (with-open [r (sam/reader f)]
    {:header (io/read-header r)
     :alignments (doall (io/read-alignments r {}))}))

(defn slurp-bam-for-test [f]
  (with-open [r (bam/reader f :ignore-index true)]
    {:header (io/read-header r)
     :alignments (doall (io/read-alignments r {}))}))

;; spit (for test)
(defn spit-sam-for-test [f sam]
  (with-open [w (sam/writer f)]
    (io/write-header w (:header sam))
    (io/write-alignments w (:alignments sam) nil)))

(defn spit-bam-for-test [f sam]
  (with-open [w (bam/writer f)]
    (io/write-header w (:header sam))
    (io/write-refs w (:header sam))
    (io/write-alignments w (:alignments sam) (:header sam))))

;; Test resources
;; --------------

;; ### SAM files

(def test-sam-file "test-resources/test.sam")

;; ### BAM files

(def test-bam-file "test-resources/test.bam")
(def test-sorted-bam-file "test-resources/test.sorted.bam")
(def small-bam-file "test-resources/small.bam")
(def medium-bam-file "test-resources/medium.bam")
(def large-bam-file (cavia/resource mycavia "large.bam"))

(def dedupe-before-bam-file "test-resources/dedupe_before.bam")
(def dedupe-after-bam-file "test-resources/dedupe_after.bam")

(def normalize-before-bam-file "test-resources/normalize_before.bam")
(def normalize-after-bam-file "test-resources/normalize_after.bam")

;; ### BAM index files

(def test-bai-file "test-resources/test.sorted.bam.bai")
(def test-large-bai-file (cavia/resource mycavia "large.bai"))

;; ### FASTA files

(def test-fa-file  "test-resources/test.fa")
(def test-fa-bz2-file  "test-resources/test.fa.bz2")
(def test-fa-dict-file "test-resources/test-fa.dict")
(def medium-fa-file "test-resources/medium.fa")
(def medium-fa-gz-file "test-resources/medium.fa.gz")

;; ### FASTA index files

(def test-fai-file "test-resources/test.fa.fai")
(def medium-fai-file "test-resources/medium.fa.fai")

;; ### FASTQ files

(def test-fq-file "test-resources/test.fq")
(def test-fq-gz-file "test-resources/test.fq.gz")
(def test-fq-bz2-file "test-resources/test.fq.bz2")

;; ### TABIX files

(def test-tabix-file "test-resources/test.gtf.gz.tbi")
(def test-large-tabix-file (cavia/resource mycavia "large.tbi"))

;; ### VCF files

(def test-vcf-v4_0-file "test-resources/vcf/test-v4_0.vcf")
(def test-vcf-v4_3-file "test-resources/vcf/test-v4_3.vcf")

;; ### pileup files

(def test-pileup-file "test-resources/pileup/test.pileup")
(def test-pileup-dir "test-resources/pileup/")

(def test-sam
  {:header {:SQ [{:SN "ref", :LN 45} {:SN "ref2", :LN 40}]}
   :alignments
   [{:qname "r003", :flag 16 , :rname "ref" , :pos 29, :mapq 30, :cigar "6H5M"              , :rnext "*", :pnext 0 , :tlen  0  , :seq "TAGGC"                     , :qual "*"                         , :options []}
    {:qname "r001", :flag 163, :rname "ref" , :pos 7 , :mapq 30, :cigar "8M4I4M1D3M"        , :rnext "=", :pnext 37, :tlen  39 , :seq "TTAGATAAAGAGGATACTG"       , :qual "*"                         , :options [{:XX {:type "B", :value "S,12561,2,20,112"}}]}
    {:qname "r002", :flag 0  , :rname "ref" , :pos 9 , :mapq 30, :cigar "1S2I6M1P1I1P1I4M2I", :rnext "*", :pnext 0 , :tlen  0  , :seq "AAAAGATAAGGGATAAA"         , :qual "*"                         , :options []}
    {:qname "r003", :flag 0  , :rname "ref" , :pos 9 , :mapq 30, :cigar "5H6M"              , :rnext "*", :pnext 0 , :tlen  0  , :seq "AGCTAA"                    , :qual "*"                         , :options []}
    {:qname "x3"  , :flag 0  , :rname "ref2", :pos 6 , :mapq 30, :cigar "9M4I13M"           , :rnext "*", :pnext 0 , :tlen  0  , :seq "TTATAAAACAAATAATTAAGTCTACA", :qual "??????????????????????????", :options []}
    {:qname "r004", :flag 0  , :rname "ref" , :pos 16, :mapq 30, :cigar "6M14N1I5M"         , :rnext "*", :pnext 0 , :tlen  0  , :seq "ATAGCTCTCAGC"              , :qual "*"                         , :options []}
    {:qname "r001", :flag 83 , :rname "ref" , :pos 37, :mapq 30, :cigar "9M"                , :rnext "=", :pnext 7 , :tlen  -39, :seq "CAGCGCCAT"                 , :qual "*"                         , :options []}
    {:qname "x1"  , :flag 0  , :rname "ref2", :pos 1 , :mapq 30, :cigar "20M"               , :rnext "*", :pnext 0 , :tlen  0  , :seq "AGGTTTTATAAAACAAATAA"      , :qual "????????????????????"      , :options []}
    {:qname "x2"  , :flag 0  , :rname "ref2", :pos 2 , :mapq 30, :cigar "21M"               , :rnext "*", :pnext 0 , :tlen  0  , :seq "GGTTTTATAAAACAAATAATT"     , :qual "?????????????????????"     , :options []}
    {:qname "x4"  , :flag 0  , :rname "ref2", :pos 10, :mapq 30, :cigar "25M"               , :rnext "*", :pnext 0 , :tlen  0  , :seq "CAAATAATTAAGTCTACAGAGCAAC" , :qual "?????????????????????????" , :options []}
    {:qname "x6"  , :flag 0  , :rname "ref2", :pos 14, :mapq 30, :cigar "23M"               , :rnext "*", :pnext 0 , :tlen  0  , :seq "TAATTAAGTCTACAGAGCAACTA"   , :qual "???????????????????????"   , :options []}
    {:qname "x5"  , :flag 0  , :rname "ref2", :pos 12, :mapq 30, :cigar "24M"               , :rnext "*", :pnext 0 , :tlen  0  , :seq "AATAATTAAGTCTACAGAGCAACT"  , :qual "????????????????????????"  , :options []}]})

(def test-sam-blocks
  [{:line "r003\t16\tref\t29\t30\t6H5M\t*\t0\t0\tTAGGC\t*"}
   {:line "r001\t163\tref\t7\t30\t8M4I4M1D3M\t=\t37\t39\tTTAGATAAAGAGGATACTG\t*\tXX:B:S,12561,2,20,112"}
   {:line "r002\t0\tref\t9\t30\t1S2I6M1P1I1P1I4M2I\t*\t0\t0\tAAAAGATAAGGGATAAA\t*"}
   {:line "r003\t0\tref\t9\t30\t5H6M\t*\t0\t0\tAGCTAA\t*"}
   {:line "x3\t0\tref2\t6\t30\t9M4I13M\t*\t0\t0\tTTATAAAACAAATAATTAAGTCTACA\t??????????????????????????"}
   {:line "r004\t0\tref\t16\t30\t6M14N1I5M\t*\t0\t0\tATAGCTCTCAGC\t*"}
   {:line "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*"}
   {:line "x1\t0\tref2\t1\t30\t20M\t*\t0\t0\tAGGTTTTATAAAACAAATAA\t????????????????????"}
   {:line "x2\t0\tref2\t2\t30\t21M\t*\t0\t0\tGGTTTTATAAAACAAATAATT\t?????????????????????"}
   {:line "x4\t0\tref2\t10\t30\t25M\t*\t0\t0\tCAAATAATTAAGTCTACAGAGCAAC\t?????????????????????????"}
   {:line "x6\t0\tref2\t14\t30\t23M\t*\t0\t0\tTAATTAAGTCTACAGAGCAACTA\t???????????????????????"}
   {:line "x5\t0\tref2\t12\t30\t24M\t*\t0\t0\tAATAATTAAGTCTACAGAGCAACT\t????????????????????????"}])

(defn get-shuffled-test-sam
  []
  (assoc test-sam :alignments (shuffle (:alignments test-sam))))

(def test-sam-only-header
  (assoc test-sam :alignments nil))

(def test-sam-sorted-by-pos
  {:header {:HD {:VN "1.4", :SO "coordinate"}
            :SQ [{:SN "ref", :LN 45} {:SN "ref2", :LN 40}]}
   :alignments
   [{:qname "r001", :flag 163, :rname "ref" , :pos 7 , :mapq 30, :cigar "8M4I4M1D3M"        , :rnext "=", :pnext 37, :tlen 39 , :seq "TTAGATAAAGAGGATACTG"       , :qual "*"                         , :options [{:XX {:type "B", :value "S,12561,2,20,112"}}]}
    {:qname "r002", :flag 0  , :rname "ref" , :pos 9 , :mapq 30, :cigar "1S2I6M1P1I1P1I4M2I", :rnext "*", :pnext 0 , :tlen 0  , :seq "AAAAGATAAGGGATAAA"         , :qual "*"                         , :options []}
    {:qname "r003", :flag 0  , :rname "ref" , :pos 9 , :mapq 30, :cigar "5H6M"              , :rnext "*", :pnext 0 , :tlen 0  , :seq "AGCTAA"                    , :qual "*"                         , :options []}
    {:qname "r004", :flag 0  , :rname "ref" , :pos 16, :mapq 30, :cigar "6M14N1I5M"         , :rnext "*", :pnext 0 , :tlen 0  , :seq "ATAGCTCTCAGC"              , :qual "*"                         , :options []}
    {:qname "r003", :flag 16 , :rname "ref" , :pos 29, :mapq 30, :cigar "6H5M"              , :rnext "*", :pnext 0 , :tlen 0  , :seq "TAGGC"                     , :qual "*"                         , :options []}
    {:qname "r001", :flag 83 , :rname "ref" , :pos 37, :mapq 30, :cigar "9M"                , :rnext "=", :pnext 7 , :tlen -39, :seq "CAGCGCCAT"                 , :qual "*"                         , :options []}
    {:qname "x1"  , :flag 0  , :rname "ref2", :pos 1 , :mapq 30, :cigar "20M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "AGGTTTTATAAAACAAATAA"      , :qual "????????????????????"      , :options []}
    {:qname "x2"  , :flag 0  , :rname "ref2", :pos 2 , :mapq 30, :cigar "21M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "GGTTTTATAAAACAAATAATT"     , :qual "?????????????????????"     , :options []}
    {:qname "x3"  , :flag 0  , :rname "ref2", :pos 6 , :mapq 30, :cigar "9M4I13M"           , :rnext "*", :pnext 0 , :tlen 0  , :seq "TTATAAAACAAATAATTAAGTCTACA", :qual "??????????????????????????", :options []}
    {:qname "x4"  , :flag 0  , :rname "ref2", :pos 10, :mapq 30, :cigar "25M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "CAAATAATTAAGTCTACAGAGCAAC" , :qual "?????????????????????????" , :options []}
    {:qname "x5"  , :flag 0  , :rname "ref2", :pos 12, :mapq 30, :cigar "24M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "AATAATTAAGTCTACAGAGCAACT"  , :qual "????????????????????????"  , :options []}
    {:qname "x6"  , :flag 0  , :rname "ref2", :pos 14, :mapq 30, :cigar "23M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "TAATTAAGTCTACAGAGCAACTA"   , :qual "???????????????????????"   , :options []}]})

(def test-sam-sorted-by-qname
  {:header {:HD {:VN "1.4", :SO "queryname"}
            :SQ [{:SN "ref", :LN 45} {:SN "ref2", :LN 40}]}
   :alignments
   [{:qname "r001", :flag 83 , :rname "ref" , :pos 37, :mapq 30, :cigar "9M"                , :rnext "=", :pnext 7 , :tlen -39, :seq "CAGCGCCAT"                 , :qual "*"                         , :options []}
    {:qname "r001", :flag 163, :rname "ref" , :pos 7 , :mapq 30, :cigar "8M4I4M1D3M"        , :rnext "=", :pnext 37, :tlen 39 , :seq "TTAGATAAAGAGGATACTG"       , :qual "*"                         , :options [{:XX {:type "B", :value "S,12561,2,20,112"}}]}
    {:qname "r002", :flag 0  , :rname "ref" , :pos 9 , :mapq 30, :cigar "1S2I6M1P1I1P1I4M2I", :rnext "*", :pnext 0 , :tlen 0  , :seq "AAAAGATAAGGGATAAA"         , :qual "*"                         , :options []}
    {:qname "r003", :flag 16 , :rname "ref" , :pos 29, :mapq 30, :cigar "6H5M"              , :rnext "*", :pnext 0 , :tlen 0  , :seq "TAGGC"                     , :qual "*"                         , :options []}
    {:qname "r003", :flag 0  , :rname "ref" , :pos 9 , :mapq 30, :cigar "5H6M"              , :rnext "*", :pnext 0 , :tlen 0  , :seq "AGCTAA"                    , :qual "*"                         , :options []}
    {:qname "r004", :flag 0  , :rname "ref" , :pos 16, :mapq 30, :cigar "6M14N1I5M"         , :rnext "*", :pnext 0 , :tlen 0  , :seq "ATAGCTCTCAGC"              , :qual "*"                         , :options []}
    {:qname "x1"  , :flag 0  , :rname "ref2", :pos 1 , :mapq 30, :cigar "20M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "AGGTTTTATAAAACAAATAA"      , :qual "????????????????????"      , :options []}
    {:qname "x2"  , :flag 0  , :rname "ref2", :pos 2 , :mapq 30, :cigar "21M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "GGTTTTATAAAACAAATAATT"     , :qual "?????????????????????"     , :options []}
    {:qname "x3"  , :flag 0  , :rname "ref2", :pos 6 , :mapq 30, :cigar "9M4I13M"           , :rnext "*", :pnext 0 , :tlen 0  , :seq "TTATAAAACAAATAATTAAGTCTACA", :qual "??????????????????????????", :options []}
    {:qname "x4"  , :flag 0  , :rname "ref2", :pos 10, :mapq 30, :cigar "25M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "CAAATAATTAAGTCTACAGAGCAAC" , :qual "?????????????????????????" , :options []}
    {:qname "x5"  , :flag 0  , :rname "ref2", :pos 12, :mapq 30, :cigar "24M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "AATAATTAAGTCTACAGAGCAACT"  , :qual "????????????????????????"  , :options []}
    {:qname "x6"  , :flag 0  , :rname "ref2", :pos 14, :mapq 30, :cigar "23M"               , :rnext "*", :pnext 0 , :tlen 0  , :seq "TAATTAAGTCTACAGAGCAACTA"   , :qual "???????????????????????"   , :options []}]})

(def test-sam-refs [{:name "ref", :len 45} {:name "ref2", :len 40}])
(def medium-sam-refs [{:name "chr1",  :len 249250621}
                      {:name "chr2",  :len 243199373}
                      {:name "chr3",  :len 198022430}
                      {:name "chr4",  :len 191154276}
                      {:name "chr5",  :len 180915260}
                      {:name "chr6",  :len 171115067}
                      {:name "chr7",  :len 159138663}
                      {:name "chr8",  :len 146364022}
                      {:name "chr9",  :len 141213431}
                      {:name "chr10", :len 135534747}
                      {:name "chr11", :len 135006516}
                      {:name "chr12", :len 133851895}
                      {:name "chr13", :len 115169878}
                      {:name "chr14", :len 107349540}
                      {:name "chr15", :len 102531392}
                      {:name "chr16", :len 90354753}
                      {:name "chr17", :len 81195210}
                      {:name "chr18", :len 78077248}
                      {:name "chr19", :len 59128983}
                      {:name "chr20", :len 63025520}
                      {:name "chr21", :len 48129895}
                      {:name "chr22", :len 51304566}
                      {:name "chrX",  :len 155270560}
                      {:name "chrY",  :len 59373566}])

(def large-sam-refs [{:len 247249719 :name "chr1"}
                     {:len 135374737 :name "chr10"}
                     {:len 134452384 :name "chr11"}
                     {:len 132349534 :name "chr12"}
                     {:len 114142980 :name "chr13"}
                     {:len 106368585 :name "chr14"}
                     {:len 100338915 :name "chr15"}
                     {:len 88827254  :name "chr16"}
                     {:len 78774742  :name "chr17"}
                     {:len 76117153  :name "chr18"}
                     {:len 63811651  :name "chr19"}
                     {:len 242951149 :name "chr2"}
                     {:len 62435964  :name "chr20"}
                     {:len 46944323  :name "chr21"}
                     {:len 49691432  :name "chr22"}
                     {:len 199501827 :name "chr3"}
                     {:len 191273063 :name "chr4"}
                     {:len 180857866 :name "chr5"}
                     {:len 170899992 :name "chr6"}
                     {:len 158821424 :name "chr7"}
                     {:len 146274826 :name "chr8"}
                     {:len 140273252 :name "chr9"}
                     {:len 48502     :name "chrL"}
                     {:len 16571     :name "chrM"}
                     {:len 154913754 :name "chrX"}
                     {:len 57772954  :name "chrY"}])

(defn data->clj [data]
  (map #(update % :data vec) data))

(def test-sam-data
  [{:data [0 0 0 0 28 0 0 0 5 30 73 18 2 0 16 0 5 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 101 0 0 0 80 0 0 0 -127 68 32 -1 -1 -1 -1 -1], :size 53}
   {:data [0 0 0 0 6 0 0 0 5 30 73 18 5 0 -93 0 19 0 0 0 0 0 0 0 36 0 0 0 39 0 0 0 114 48 48 49 0 -128 0 0 0 65 0 0 0 64 0 0 0 18 0 0 0 48 0 0 0 -120 20 24 17 20 20 65 -127 40 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 88 88 66 83 4 0 0 0 17 49 2 0 20 0 112 0], :size 102}
   {:data [0 0 0 0 8 0 0 0 5 30 73 18 9 0 0 0 17 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 50 0 20 0 0 0 33 0 0 0 96 0 0 0 22 0 0 0 17 0 0 0 22 0 0 0 17 0 0 0 64 0 0 0 33 0 0 0 17 17 65 -127 20 68 24 17 16 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1], :size 99}
   {:data [0 0 0 0 8 0 0 0 5 30 73 18 2 0 0 0 6 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 85 0 0 0 96 0 0 0 20 40 17 -1 -1 -1 -1 -1 -1], :size 54}
   {:data [1 0 0 0 5 0 0 0 3 30 73 18 3 0 0 0 26 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 51 0 -112 0 0 0 65 0 0 0 -48 0 0 0 -120 24 17 17 33 17 -127 24 -127 20 -126 -127 33 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 86}
   {:data [0 0 0 0 15 0 0 0 5 30 73 18 4 0 0 0 12 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 52 0 96 0 0 0 -29 0 0 0 17 0 0 0 80 0 0 0 24 20 40 40 33 66 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1], :size 71}
   {:data [0 0 0 0 36 0 0 0 5 30 73 18 1 0 83 0 9 0 0 0 0 0 0 0 6 0 0 0 -39 -1 -1 -1 114 48 48 49 0 -112 0 0 0 33 66 66 33 -128 -1 -1 -1 -1 -1 -1 -1 -1 -1], :size 55}
   {:data [1 0 0 0 0 0 0 0 3 30 73 18 1 0 0 0 20 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 49 0 64 1 0 0 20 72 -120 -127 -127 17 18 17 24 17 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 69}
   {:data [1 0 0 0 1 0 0 0 3 30 73 18 1 0 0 0 21 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 50 0 80 1 0 0 68 -120 -120 24 17 17 33 17 -127 24 -128 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 71}
   {:data [1 0 0 0 9 0 0 0 3 30 73 18 1 0 0 0 25 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 52 0 -112 1 0 0 33 17 -127 24 -127 20 -126 -127 33 65 66 17 32 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 77}
   {:data [1 0 0 0 13 0 0 0 3 30 73 18 1 0 0 0 23 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 54 0 112 1 0 0 -127 24 -127 20 -126 -127 33 65 66 17 40 16 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 74}
   {:data [1 0 0 0 11 0 0 0 3 30 73 18 1 0 0 0 24 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 53 0 -128 1 0 0 17 -127 24 -127 20 -126 -127 33 65 66 17 40 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 75}])

(def test-sorted-bam-data
  [{:data [0 0 0 0 6 0 0 0 5 30 73 18 5 0 -93 0 19 0 0 0 0 0 0 0 36 0 0 0 39 0 0 0 114 48 48 49 0 -128 0 0 0 65 0 0 0 64 0 0 0 18 0 0 0 48 0 0 0 -120 20 24 17 20 20 65 -127 40 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 88 88 66 83 4 0 0 0 17 49 2 0 20 0 112 0], :size 102}
   {:data [0 0 0 0 8 0 0 0 5 30 73 18 9 0 0 0 17 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 50 0 20 0 0 0 33 0 0 0 96 0 0 0 22 0 0 0 17 0 0 0 22 0 0 0 17 0 0 0 64 0 0 0 33 0 0 0 17 17 65 -127 20 68 24 17 16 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1], :size 99}
   {:data [0 0 0 0 8 0 0 0 5 30 73 18 2 0 0 0 6 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 85 0 0 0 96 0 0 0 20 40 17 -1 -1 -1 -1 -1 -1], :size 54}
   {:data [0 0 0 0 15 0 0 0 5 30 73 18 4 0 0 0 12 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 52 0 96 0 0 0 -29 0 0 0 17 0 0 0 80 0 0 0 24 20 40 40 33 66 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1], :size 71}
   {:data [0 0 0 0 28 0 0 0 5 30 73 18 2 0 16 0 5 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 101 0 0 0 80 0 0 0 -127 68 32 -1 -1 -1 -1 -1], :size 53}
   {:data [0 0 0 0 36 0 0 0 5 30 73 18 1 0 83 0 9 0 0 0 0 0 0 0 6 0 0 0 -39 -1 -1 -1 114 48 48 49 0 -112 0 0 0 33 66 66 33 -128 -1 -1 -1 -1 -1 -1 -1 -1 -1], :size 55}
   {:data [1 0 0 0 0 0 0 0 3 30 73 18 1 0 0 0 20 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 49 0 64 1 0 0 20 72 -120 -127 -127 17 18 17 24 17 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 69}
   {:data [1 0 0 0 1 0 0 0 3 30 73 18 1 0 0 0 21 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 50 0 80 1 0 0 68 -120 -120 24 17 17 33 17 -127 24 -128 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 71}
   {:data [1 0 0 0 5 0 0 0 3 30 73 18 3 0 0 0 26 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 51 0 -112 0 0 0 65 0 0 0 -48 0 0 0 -120 24 17 17 33 17 -127 24 -127 20 -126 -127 33 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 86}
   {:data [1 0 0 0 9 0 0 0 3 30 73 18 1 0 0 0 25 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 52 0 -112 1 0 0 33 17 -127 24 -127 20 -126 -127 33 65 66 17 32 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 77}
   {:data [1 0 0 0 11 0 0 0 3 30 73 18 1 0 0 0 24 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 53 0 -128 1 0 0 17 -127 24 -127 20 -126 -127 33 65 66 17 40 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 75}
   {:data [1 0 0 0 13 0 0 0 3 30 73 18 1 0 0 0 23 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 120 54 0 112 1 0 0 -127 24 -127 20 -126 -127 33 65 66 17 40 16 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30], :size 74}])

(def test-sorted-bam-levels
  [{:type "i", :value 0}
   {:type "i", :value 1}
   {:type "i", :value 2}
   {:type "i", :value 2}
   {:type "i", :value 0}
   {:type "i", :value 0}
   {:type "i", :value 0}
   {:type "i", :value 1}
   {:type "i", :value 2}
   {:type "i", :value 3}
   {:type "i", :value 4}
   {:type "i", :value 5}])

(def test-fa
  [{:rname "ref",  :offset 5,  :seq "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT", :blen 45}
   {:rname "ref2", :offset 57, :seq "aggttttataaaacaattaagtctacagagcaactacgcg",      :blen 40}])

(def sq-incomplete-alignments
  [{:SN "re",   :LN 45} ; not exists in alignments on test-sam
   {:SN "ref",  :LN 45}
   {:SN "ref1", :LN 40} ; not exists in alignments on test-sam
   {:SN "ref2", :LN 40}
   {:SN "ref3", :LN 45} ; not exists in alignments on test-sam
   ])
(def test-sam-incomplete-alignments
  {:header (assoc (:header test-sam) :SQ sq-incomplete-alignments)
   :alignments (:alignments test-sam)})
(def test-sam-incomplete-alignments-sorted-by-pos
  {:header (assoc (:header test-sam-sorted-by-pos) :SQ sq-incomplete-alignments)
   :alignments (:alignments test-sam-sorted-by-pos)})

(def temp-dir (.getPath (file (System/getProperty "java.io.tmpdir") "cljam-test")))

(defn prepare-cache!
  []
  (.mkdir (file temp-dir)))

(defn clean-cache!
  []
  (let [dir (file temp-dir)]
    (when (.exists dir)
      (doseq [f (seq (.list dir))]
        (.delete (file (str temp-dir "/" f))))
      (.delete dir))))


(defn- uniq [coll]
  (reduce
    (fn [r one]
      (if (= (first r) one)
        r
        (conj r one)))
    nil
    coll))

(defn- get-rnames [sam]
  (uniq (map :rname (:alignments sam))))

(defn check-sort-order [target-sam & [contrast-sam]]
  ;; TODO: only coordinate currently. need to test by queryname sort.
  (let [target-rnames (get-rnames target-sam)]
    ;; check rname groups
    (when contrast-sam
      (when-not (= target-rnames (get-rnames contrast-sam))
        (throw (Exception. "not matched by rnames order"))))
    ;; check order
    (dorun
      (map
        (fn [rname]
          (reduce
            (fn [prev one]
              (case (compare (:pos prev) (:pos one))
                -1 true
                1 (throw (Exception. "pos not sorted"))
                (case (compare (:qname prev) (:qname one))
                  -1 true
                  1 (throw (Exception. "qname not sorted"))
                  true))
              one)
            (filter #(= rname (:rname %)) (:alignments target-sam))))
        target-rnames))))

;; Utilities
;; ---------

(defn same-file?
  "Returns true if the two files' MD5 hash are same, false if not."
  [f1 f2]
  (= (digest/sha1 (file f1)) (digest/sha1 (file f2))))

(defn same-bam-file?
  [f1 f2]
  (with-open [r1 (bam/reader f1)
              r2 (bam/reader f2)]
    (and (= (io/read-header r1)
            (io/read-header r2))
         (= (io/read-alignments r1 {})
            (io/read-alignments r2 {})))))

;;;; FASTA

(def test-fa-header [{:desc "", :name "ref", :offset 5} {:desc "", :name "ref2", :offset 57}])

(def test-fa-sequences '({:name "ref",
                          :sequence "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
                         {:name "ref2",
                          :sequence "aggttttataaaacaattaagtctacagagcaactacgcg"}))

(def test-fa-data
  [{:rname "ref" :len 45 :offset 5 :line-len 46 :line-blen 45
    :seq "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
   {:rname "ref2" :len 40 :offset 57 :line-len 41 :line-blen 40
    :seq "aggttttataaaacaattaagtctacagagcaactacgcg"}])

;;;; FASTQ

(def test-fq-sequences
  `({:name "SEQ_ID_1"
     :sequence "GGGGGGGGGG"
     :quality (0 1 2 3 4 5 6 7 8 9)}
    {:name "SEQ_ID_2"
     :sequence "ATGCATGCATGCATGCATGCATGCATGCATGCATGCATGC"
     :quality ~(range 40)}
    {:name "SEQ_ID_3"
     :sequence "AAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGC"
     :quality ~(range 94)}))

(def test-fq-sequences-raw
  `({:name "SEQ_ID_1"
     :sequence "GGGGGGGGGG"
     :quality "!\"#$%&'()*"}
    {:name "SEQ_ID_2"
     :sequence "ATGCATGCATGCATGCATGCATGCATGCATGCATGCATGC"
     :quality "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGH"}
    {:name "SEQ_ID_3"
     :sequence "AAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGCCCAAATTTGGGC"
     :quality "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"}))

;; ### VCF

(def test-vcf-v4_0-meta-info
  {:fileformat "VCFv4.0"
   :file-date "20090805"
   :source "myImputationProgramV3.1"
   :reference "1000GenomesPilot-NCBI36"
   :phasing "partial"
   :info [{:id "NS", :number 1, :type "Integer", :description "Number of Samples With Data"}
          {:id "AN", :number 1, :type "Integer", :description "Total number of alleles in called genotypes"}
          {:id "AC", :number nil, :type "Integer", :description "Allele count in genotypes, for each ALT allele, in the same order as listed"}
          {:id "DP", :number 1, :type "Integer", :description "Total Depth"}
          {:id "AF", :number nil, :type "Float", :description "Allele Frequency"}
          {:id "AA", :number 1, :type "String", :description "Ancestral Allele"}
          {:id "DB", :number 0, :type "Flag", :description "dbSNP membership, build 129"}
          {:id "H2", :number 0, :type "Flag", :description "HapMap2 membership"}]
   :filter [{:id "q10", :description "Quality below 10"}
            {:id "s50", :description "Less than 50% of samples have data"}]
   :format [{:id "GT", :number 1, :type "String", :description "Genotype"}
            {:id "GQ", :number 1, :type "Integer", :description "Genotype Quality"}
            {:id "DP", :number 1, :type "Integer", :description "Read Depth"}
            {:id "HQ", :number 2, :type "Integer", :description "Haplotype Quality"}]
   :alt [{:id "DEL:ME:ALU", :description "Deletion of ALU element"}
         {:id "CNV", :description "Copy number variable region"}]})

(def test-vcf-v4_0-header
  ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO" "FORMAT" "NA00001"
   "NA00002" "NA00003"])

(def test-vcf-v4_0-variants
  '({:chrom "19", :pos 111, :id nil, :ref "A", :alt ["C"], :qual 9.6, :filter nil, :info nil,
     :FORMAT "GT:HQ", :NA00001 "0|0:10,10", :NA00002 "0|0:10,10", :NA00003 "0/1:3,3"}
    {:chrom "19", :pos 112, :id nil, :ref "A", :alt ["G"], :qual 10.0, :filter nil, :info nil,
     :FORMAT "GT:HQ", :NA00001 "0|0:10,10", :NA00002 "0|0:10,10", :NA00003 "0/1:3,3"}
    {:chrom "20", :pos 14370, :id "rs6054257", :ref "G", :alt ["A"], :qual 29.0, :filter "PASS", :info nil,
     :FORMAT "GT:GQ:DP:HQ", :NA00001 "0|0:48:1:51,51", :NA00002 "1|0:48:8:51,51", :NA00003 "1/1:43:5:.,."}
    {:chrom "20", :pos 17330, :id nil, :ref "T", :alt ["A"], :qual 3.0, :filter "q10", :info nil,
     :FORMAT "GT:GQ:DP:HQ", :NA00001 "0|0:49:3:58,50", :NA00002 "0|1:3:5:65,3", :NA00003 "0/0:41:3:.,."}
    {:chrom "20", :pos 1110696, :id "rs6040355", :ref "A", :alt ["G" "T"], :qual 67.0, :filter "PASS", :info nil,
     :FORMAT "GT:GQ:DP:HQ",:NA00001 "1|2:21:6:23,27", :NA00002 "2|1:2:0:18,2", :NA00003 "2/2:35:4:.,."}
    {:chrom "20", :pos 1230237, :id nil, :ref "T", :alt nil, :qual 47.0, :filter "PASS", :info nil,
     :FORMAT "GT:GQ:DP:HQ", :NA00001 "0|0:54:.:56,60", :NA00002 "0|0:48:4:51,51", :NA00003 "0/0:61:2:.,."}
    {:chrom "20", :pos 1234567, :id "microsat1", :ref "G", :alt ["GA" "GAC"], :qual 50.0, :filter "PASS", :info nil,
     :FORMAT "GT:GQ:DP", :NA00001 "0/1:.:4", :NA00002 "0/2:17:2", :NA00003 "1/1:40:3"}
    {:chrom "20", :pos 1235237, :id nil, :ref "T", :alt nil, :qual nil, :filter nil, :info nil,
     :FORMAT "GT", :NA00001 "0/0", :NA00002 "0|0", :NA00003 "./."}
    {:chrom "X", :pos 9, :id nil, :ref "A", :alt ["T"], :qual 12.1, :filter nil, :info nil,
     :FORMAT "GT", :NA00001 "0", :NA00002 "0/1", :NA00003 "1/0"}
    {:chrom "X", :pos 10, :id "rsTest", :ref "AC", :alt ["A" "ATG"], :qual 10.0, :filter "PASS", :info nil,
     :FORMAT "GT", :NA00001 "0", :NA00002 "0/1", :NA00003 "0|2"}
    {:chrom "X", :pos 11, :id "rsTest2", :ref "T", :alt ["A" "<DEL:ME:ALU>"], :qual 10.0, :filter "q10;s50", :info nil,
     :FORMAT "GT:DP:GQ", :NA00001 ".:3:10", :NA00002 "./.:.:.", :NA00003 "0|2:3:."}
    {:chrom "X", :pos 12, :id nil, :ref "T", :alt ["A"], :qual 13.0, :filter nil, :info nil,
     :FORMAT "GT", :NA00001 "0", :NA00002 "1/0", :NA00003 "1/1"}))

(def test-vcf-v4_3-meta-info
  {:fileformat "VCFv4.3"
   :file-date "20090805"
   :source "myImputationProgramV3.1"
   :reference "file:///seq/references/1000GenomesPilot-NCBI36.fasta"
   :contig [{:id "20", :length 62435964, :assembly "B36",
             :md-5 "f126cdf8a6e0c7f379d618ff66beb2da", :species "Homo sapiens", :taxonomy "x"}]
   :phasing "partial"
   :info [{:id "NS", :number 1, :type "Integer", :description "Number of Samples With Data"}
          {:id "DP", :number 1, :type "Integer", :description "Total Depth"}
          {:id "AF", :number "A", :type "Float", :description "Allele Frequency"}
          {:id "AA", :number 1, :type "String", :description "Ancestral Allele"}
          {:id "DB", :number 0, :type "Flag", :description "dbSNP membership, build 129"}
          {:id "H2", :number 0, :type "Flag", :description "HapMap2 membership"}]
   :filter [{:id "q10", :description "Quality below 10"}
            {:id "s50", :description "Less than 50% of samples have data"}]
   :format [{:id "GT", :number 1, :type "String", :description "Genotype"}
            {:id "GQ", :number 1, :type "Integer", :description "Genotype Quality"}
            {:id "DP", :number 1, :type "Integer", :description "Read Depth"}
            {:id "HQ", :number 2, :type "Integer", :description "Haplotype Quality"}]})

(def test-vcf-v4_3-header
  ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO" "FORMAT" "NA00001"
   "NA00002" "NA00003"])

(def test-vcf-v4_3-variants
  '({:chrom "20", :pos 14370, :id "rs6054257", :ref "G", :alt ["A"], :qual 29.0, :filter "PASS", :info "NS=3;DP=14;AF=0.5;DB;H2",
     :FORMAT "GT:GQ:DP:HQ", :NA00001 "0|0:48:1:51,51", :NA00002 "1|0:48:8:51,51", :NA00003 "1/1:43:5:.,."}
    {:chrom "20", :pos 17330, :id nil, :ref "T", :alt ["A"], :qual 3.0, :filter "q10", :info "NS=3;DP=11;AF=0.017",
     :FORMAT "GT:GQ:DP:HQ", :NA00001 "0|0:49:3:58,50", :NA00002 "0|1:3:5:65,3", :NA00003 "0/0:41:3"}
    {:chrom "20", :pos 1110696, :id "rs6040355", :ref "A", :alt ["G" "T"], :qual 67.0, :filter "PASS", :info "NS=2;DP=10;AF=0.333,0.667;AA=T;DB",
     :FORMAT "GT:GQ:DP:HQ",:NA00001 "1|2:21:6:23,27", :NA00002 "2|1:2:0:18,2", :NA00003 "2/2:35:4"}
    {:chrom "20", :pos 1230237, :id nil, :ref "T", :alt nil, :qual 47.0, :filter "PASS", :info "NS=3;DP=13;AA=T",
     :FORMAT "GT:GQ:DP:HQ", :NA00001 "0|0:54:7:56,60", :NA00002 "0|0:48:4:51,51", :NA00003 "0/0:61:2"}
    {:chrom "20", :pos 1234567, :id "microsat1", :ref "GTC", :alt ["G" "GTCT"], :qual 50.0, :filter "PASS", :info "NS=3;DP=9;AA=G",
     :FORMAT "GT:GQ:DP", :NA00001 "0/1:35:4", :NA00002 "0/2:17:2", :NA00003 "1/1:40:3"}))
