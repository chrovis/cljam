(ns cljam.t-common
  (:use [clojure.java.io :only [file]])
  (:require cljam.sam)
  (:import [cljam.sam Sam SamHeader SamAlignment]))

(def test-sam-file "test/resources/test.sam")

(def test-bam-file "test/resources/test.bam")

(def test-fa-file "test/resources/test.fa")

(def test-sam
  (Sam. [(assoc (SamHeader.) :SQ {:SN "ref",  :LN "45"})
         (assoc (SamHeader.) :SQ {:SN "ref2", :LN "40"})]
        [(SamAlignment. "r003" 16  "ref"  29 30 "6H5M"               "*" 0  0   "TAGGC"                      "*"                          [])
         (SamAlignment. "r001" 163 "ref"  7  30 "8M4I4M1D3M"         "=" 37 39  "TTAGATAAAGAGGATACTG"        "*"                          [{:XX {:type "B", :value "S,12561,2,20,112"}}])
         (SamAlignment. "r002" 0   "ref"  9  30 "1S2I6M1P1I1P1I4M2I" "*" 0  0   "AAAAGATAAGGGATAAA"          "*"                          [])
         (SamAlignment. "r003" 0   "ref"  9  30 "5H6M"               "*" 0  0   "AGCTAA"                     "*"                          [])
         (SamAlignment. "x3"   0   "ref2" 6  30 "9M4I13M"            "*" 0  0   "ttataaaacAAATaattaagtctaca" "??????????????????????????" [])
         (SamAlignment. "r004" 0   "ref"  16 30 "6M14N1I5M"          "*" 0  0   "ATAGCTCTCAGC"               "*"                          [])
         (SamAlignment. "r001" 83  "ref"  37 30 "9M"                 "=" 7  -39 "CAGCGCCAT"                  "*"                          [])
         (SamAlignment. "x1"   0   "ref2" 1  30 "20M"                "*" 0  0   "aggttttataaaacaaataa"       "????????????????????"       [])
         (SamAlignment. "x2"   0   "ref2" 2  30 "21M"                "*" 0  0   "ggttttataaaacaaataatt"      "?????????????????????"      [])
         (SamAlignment. "x4"   0   "ref2" 10 30 "25M"                "*" 0  0   "CaaaTaattaagtctacagagcaac"  "?????????????????????????"  [])
         (SamAlignment. "x6"   0   "ref2" 14 30 "23M"                "*" 0  0   "Taattaagtctacagagcaacta"    "???????????????????????"    [])
         (SamAlignment. "x5"   0   "ref2" 12 30 "24M"                "*" 0  0   "aaTaattaagtctacagagcaact"   "????????????????????????"   [])]))

(def test-sam-sorted-by-pos
  (Sam. [(assoc (SamHeader.) :HD {:VN "1.4",  :SO "coordinate"})
         (assoc (SamHeader.) :SQ {:SN "ref",  :LN "45"})
         (assoc (SamHeader.) :SQ {:SN "ref2", :LN "40"})]
        [(SamAlignment. "r001" 163 "ref"  7  30 "8M4I4M1D3M"         "=" 37 39  "TTAGATAAAGAGGATACTG"        "*"                          [{:XX {:type "B", :value "S,12561,2,20,112"}}])
         (SamAlignment. "r002" 0   "ref"  9  30 "1S2I6M1P1I1P1I4M2I" "*" 0  0   "AAAAGATAAGGGATAAA"          "*"                          [])
         (SamAlignment. "r003" 0   "ref"  9  30 "5H6M"               "*" 0  0   "AGCTAA"                     "*"                          [])
         (SamAlignment. "r004" 0   "ref"  16 30 "6M14N1I5M"          "*" 0  0   "ATAGCTCTCAGC"               "*"                          [])
         (SamAlignment. "r003" 16  "ref"  29 30 "6H5M"               "*" 0  0   "TAGGC"                      "*"                          [])
         (SamAlignment. "r001" 83  "ref"  37 30 "9M"                 "=" 7  -39 "CAGCGCCAT"                  "*"                          [])
         (SamAlignment. "x1"   0   "ref2" 1  30 "20M"                "*" 0  0   "aggttttataaaacaaataa"       "????????????????????"       [])
         (SamAlignment. "x2"   0   "ref2" 2  30 "21M"                "*" 0  0   "ggttttataaaacaaataatt"      "?????????????????????"      [])
         (SamAlignment. "x3"   0   "ref2" 6  30 "9M4I13M"            "*" 0  0   "ttataaaacAAATaattaagtctaca" "??????????????????????????" [])
         (SamAlignment. "x4"   0   "ref2" 10 30 "25M"                "*" 0  0   "CaaaTaattaagtctacagagcaac"  "?????????????????????????"  [])
         (SamAlignment. "x5"   0   "ref2" 12 30 "24M"                "*" 0  0   "aaTaattaagtctacagagcaact"   "????????????????????????"   [])
         (SamAlignment. "x6"   0   "ref2" 14 30 "23M"                "*" 0  0   "Taattaagtctacagagcaacta"    "???????????????????????"    [])]))

(def test-sam-sorted-by-qname
  (Sam. [(assoc (SamHeader.) :HD {:VN "1.4",  :SO "queryname"})
         (assoc (SamHeader.) :SQ {:SN "ref",  :LN "45"})
         (assoc (SamHeader.) :SQ {:SN "ref2", :LN "40"})]
        [(SamAlignment. "r001" 83  "ref"  37 30 "9M"                 "=" 7  -39 "CAGCGCCAT"                  "*"                          [])
         (SamAlignment. "r001" 163 "ref"  7  30 "8M4I4M1D3M"         "=" 37 39  "TTAGATAAAGAGGATACTG"        "*"                          [{:XX {:type "B", :value "S,12561,2,20,112"}}])
         (SamAlignment. "r002" 0   "ref"  9  30 "1S2I6M1P1I1P1I4M2I" "*" 0  0   "AAAAGATAAGGGATAAA"          "*"                          [])
         (SamAlignment. "r003" 16  "ref"  29 30 "6H5M"               "*" 0  0   "TAGGC"                      "*"                          [])
         (SamAlignment. "r003" 0   "ref"  9  30 "5H6M"               "*" 0  0   "AGCTAA"                     "*"                          [])
         (SamAlignment. "r004" 0   "ref"  16 30 "6M14N1I5M"          "*" 0  0   "ATAGCTCTCAGC"               "*"                          [])
         (SamAlignment. "x1"   0   "ref2" 1  30 "20M"                "*" 0  0   "aggttttataaaacaaataa"       "????????????????????"       [])
         (SamAlignment. "x2"   0   "ref2" 2  30 "21M"                "*" 0  0   "ggttttataaaacaaataatt"      "?????????????????????"      [])
         (SamAlignment. "x3"   0   "ref2" 6  30 "9M4I13M"            "*" 0  0   "ttataaaacAAATaattaagtctaca" "??????????????????????????" [])
         (SamAlignment. "x4"   0   "ref2" 10 30 "25M"                "*" 0  0   "CaaaTaattaagtctacagagcaac"  "?????????????????????????"  [])
         (SamAlignment. "x5"   0   "ref2" 12 30 "24M"                "*" 0  0   "aaTaattaagtctacagagcaact"   "????????????????????????"   [])
         (SamAlignment. "x6"   0   "ref2" 14 30 "23M"                "*" 0  0   "Taattaagtctacagagcaacta"    "???????????????????????"    [])]))

(def test-fa
  [{:ref "ref",  :offset 5,  :seq "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"}
   {:ref "ref2", :offset 57, :seq "aggttttataaaacaattaagtctacagagcaactacgcg"}])

(def temp-dir (str (System/getProperty "java.io.tmpdir") "cljam-test"))

(defn mk-temp-dir! []
  (.mkdir (file temp-dir)))

(defn rm-temp-dir! []
  (.delete (file temp-dir)))
