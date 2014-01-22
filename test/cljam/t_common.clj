(ns cljam.t-common
  (:use [clojure.java.io :only [file]])
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cavy.core :as cavy :refer [defcavy]]
            ))

(defcavy mycavy
  {:resources [{:id "large.bam"
                :url "ftp://ftp.broadinstitute.org/old/NA12878.BI.illumina.dup.sort.bam"
                :sha1 "523780c7f39dfe97939124e92c8d34d0759c841f"
                ;:sha1 "e3d87d947e8b5cc0b88fef042b2e5438ab9120b8"
                :auth {:user "gsapubftp-anonymous", :password "cljam-test"}
                }
               ]})

(defn prepare-cavy! [] (cavy/without-print (cavy/get!)) (cavy/verify))
(defn clean-cavy! [] (cavy/clean!))


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
(def large-sam-refs [{:name "1",  :len 247249719}
                     {:name "2",  :len 242951149}
                     {:name "3",  :len 199501827}
                     {:name "4",  :len 191273063}
                     {:name "5",  :len 180857866}
                     {:name "6",  :len 170899992}
                     {:name "7",  :len 158821424}
                     {:name "8",  :len 146274826}
                     {:name "9",  :len 140273252}
                     {:name "10", :len 135374737}
                     {:name "11", :len 134452384}
                     {:name "12", :len 132349534}
                     {:name "13", :len 114142980}
                     {:name "14", :len 106368585}
                     {:name "15", :len 100338915}
                     {:name "16", :len 88827254}
                     {:name "17", :len 78774742}
                     {:name "18", :len 76117153}
                     {:name "19", :len 63811651}
                     {:name "20", :len 62435964}
                     {:name "21", :len 46944323}
                     {:name "22", :len 49691432}
                     {:name "X",  :len 154913754}
                     {:name "Y",  :len 57772954}
                     {:name "MT", :len 16571}])

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

(def large-bam-file (cavy/resource "large.bam"))

