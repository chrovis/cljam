(ns cljam.t-common
  (:use [clojure.java.io :only [file]])
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cavia.core :as cavia :refer [defprofile with-profile]]
            ))

(defprofile mycavia
  {:resources [{:id "large.bam"
                :url "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/data/HG04238/alignment/HG04238.unmapped.ILLUMINA.bwa.ITU.low_coverage.20130415.bam"
                :sha1 "f62c94eb80aa68f5c8d36e6147e66aefd879ae5d"
                :auth {:user "anonymous", :password "test%40example.com"}}
               {:id "large.tbi"
                :url "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/ALL.chr1.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz.tbi"
                :sha1 "ebc756953ba502e6aba8f9b673c683318f6c63ee"
                :auth {:user "anonymous"}}]})

(defn prepare-cavia! []
  (with-profile mycavia
    (cavia/without-print (cavia/get!))
    (cavia/verify)))

(defn clean-cavia! []
  (with-profile mycavia
    (cavia/clean!)))


;;; slurp (for test)
(defn slurp-sam-for-test [f]
  (with-open [r (sam/reader f)]
    {:header (io/read-header r)
     :alignments (doall (io/read-alignments r {}))}))

(defn slurp-bam-for-test [f]
  (with-open [r (bam/reader f)]
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

(def test-sam-file "test/resources/test.sam")
(def test-bam-file "test/resources/test.bam")
(def test-sorted-bam-file "test/resources/test.sorted.bam")
(def test-fa-file  "test/resources/test.fa")
(def test-fai-file "test/resources/test.fa.fai")
(def test-tabix-file "test/resources/test.gtf.gz.tbi")
(def test-large-tabix-file (cavia/resource mycavia "large.tbi"))

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
(def large-sam-refs [{:name "1",          :len 249250621}
                     {:name "2",          :len 243199373}
                     {:name "3",          :len 198022430}
                     {:name "4",          :len 191154276}
                     {:name "5",          :len 180915260}
                     {:name "6",          :len 171115067}
                     {:name "7",          :len 159138663}
                     {:name "8",          :len 146364022}
                     {:name "9",          :len 141213431}
                     {:name "10",         :len 135534747}
                     {:name "11",         :len 135006516}
                     {:name "12",         :len 133851895}
                     {:name "13",         :len 115169878}
                     {:name "14",         :len 107349540}
                     {:name "15",         :len 102531392}
                     {:name "16",         :len 90354753}
                     {:name "17",         :len 81195210}
                     {:name "18",         :len 78077248}
                     {:name "19",         :len 59128983}
                     {:name "20",         :len 63025520}
                     {:name "21",         :len 48129895}
                     {:name "22",         :len 51304566}
                     {:name "X",          :len 155270560}
                     {:name "Y",          :len 59373566}
                     {:name "MT",         :len 16569}
                     {:name "GL000207.1", :len 4262}
                     {:name "GL000226.1", :len 15008}
                     {:name "GL000229.1", :len 19913}
                     {:name "GL000231.1", :len 27386}
                     {:name "GL000210.1", :len 27682}
                     {:name "GL000239.1", :len 33824}
                     {:name "GL000235.1", :len 34474}
                     {:name "GL000201.1", :len 36148}
                     {:name "GL000247.1", :len 36422}
                     {:name "GL000245.1", :len 36651}
                     {:name "GL000197.1", :len 37175}
                     {:name "GL000203.1", :len 37498}
                     {:name "GL000246.1", :len 38154}
                     {:name "GL000249.1", :len 38502}
                     {:name "GL000196.1", :len 38914}
                     {:name "GL000248.1", :len 39786}
                     {:name "GL000244.1", :len 39929}
                     {:name "GL000238.1", :len 39939}
                     {:name "GL000202.1", :len 40103}
                     {:name "GL000234.1", :len 40531}
                     {:name "GL000232.1", :len 40652}
                     {:name "GL000206.1", :len 41001}
                     {:name "GL000240.1", :len 41933}
                     {:name "GL000236.1", :len 41934}
                     {:name "GL000241.1", :len 42152}
                     {:name "GL000243.1", :len 43341}
                     {:name "GL000242.1", :len 43523}
                     {:name "GL000230.1", :len 43691}
                     {:name "GL000237.1", :len 45867}
                     {:name "GL000233.1", :len 45941}
                     {:name "GL000204.1", :len 81310}
                     {:name "GL000198.1", :len 90085}
                     {:name "GL000208.1", :len 92689}
                     {:name "GL000191.1", :len 106433}
                     {:name "GL000227.1", :len 128374}
                     {:name "GL000228.1", :len 129120}
                     {:name "GL000214.1", :len 137718}
                     {:name "GL000221.1", :len 155397}
                     {:name "GL000209.1", :len 159169}
                     {:name "GL000218.1", :len 161147}
                     {:name "GL000220.1", :len 161802}
                     {:name "GL000213.1", :len 164239}
                     {:name "GL000211.1", :len 166566}
                     {:name "GL000199.1", :len 169874}
                     {:name "GL000217.1", :len 172149}
                     {:name "GL000216.1", :len 172294}
                     {:name "GL000215.1", :len 172545}
                     {:name "GL000205.1", :len 174588}
                     {:name "GL000219.1", :len 179198}
                     {:name "GL000224.1", :len 179693}
                     {:name "GL000223.1", :len 180455}
                     {:name "GL000195.1", :len 182896}
                     {:name "GL000212.1", :len 186858}
                     {:name "GL000222.1", :len 186861}
                     {:name "GL000200.1", :len 187035}
                     {:name "GL000193.1", :len 189789}
                     {:name "GL000194.1", :len 191469}
                     {:name "GL000225.1", :len 211173}
                     {:name "GL000192.1", :len 547496}
                     {:name "NC_007605",  :len 171823}
                     {:name "hs37d5",     :len 35477943}
                     ])

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

(def medium-bam-file "test/resources/medium.bam")
;;; How to generate "medium.bam":
;;; $ wget https://share.xcoo.jp/works/.../B6_all_bwa.sorted.bam
;;; $ samtools view -h B6_all_bwa.sorted.bam > src.sam
;;; $ grep '^@' src.sam > r.head
;;; $ grep -v '^@' src.sam > r.body
;;; $ cat r.body | perl -ne 'print $_ if rand() < 0.001' > r.body2
;;; $ cat r.head r.body2 > result.sam
;;; $ samtools view -S -b result.sam > test/resources/medium.bam

(def large-bam-file (cavia/resource mycavia "large.bam"))
