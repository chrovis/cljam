(ns cljam.t-bed
  (:use [midje.sweet])
  (:require [clojure.java.io :as io]
            [cljam.bed :as bed])
  (:import [java.io BufferedReader InputStreamReader ByteArrayInputStream]))

(defn- str->bed [^String s]
  (with-open [bais (ByteArrayInputStream. (.getBytes s))
              isr (InputStreamReader. bais)
              br (BufferedReader. isr)]
    (doall (bed/read-fields br))))

(facts "bed file reader"
 (str->bed "1 0 100 N 0 + 0 0 255,0,0 2 10,90 0,10")
 => [{:chr "chr1" :start 1 :end 100 :name "N" :score 0 :strand :plus :thick-start 1 :thick-end 0
      :item-rgb "255,0,0" :block-count 2 :block-sizes [10 90] :block-starts [0 10]}]

 (with-open [r (io/reader "test-resources/test1.bed")]
   (bed/read-fields r) =>
   [{:chr "chr22" :start 1001 :end 5000 :name "cloneA" :score 960 :strand :plus :thick-start 1001
     :thick-end 5000 :item-rgb "0" :block-count 2 :block-sizes [567 488] :block-starts [0 3512]}
    {:chr "chr22" :start 2001 :end 6000 :name "cloneB" :score 900 :strand :minus :thick-start 2001
     :thick-end 6000 :item-rgb "0" :block-count 2 :block-sizes [433 399] :block-starts [0 3601]}])

 (with-open [r (io/reader "test-resources/test2.bed")]
   (bed/read-fields r) =>
   [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :plus :thick-start 127471197
     :thick-end 127472363 :item-rgb "255,0,0"}
    {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :plus :thick-start 127472364
     :thick-end 127473530 :item-rgb "255,0,0"}
    {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :plus :thick-start 127473531
     :thick-end 127474697 :item-rgb "255,0,0"}
    {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :plus :thick-start 127474698
     :thick-end 127475864 :item-rgb "255,0,0"}
    {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :minus :thick-start 127475865
     :thick-end 127477031 :item-rgb "0,0,255"}
    {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :minus :thick-start 127477032
     :thick-end 127478198 :item-rgb "0,0,255"}
    {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :minus :thick-start 127478199
     :thick-end 127479365 :item-rgb "0,0,255"}
    {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :plus :thick-start 127479366
     :thick-end 127480532 :item-rgb "255,0,0"}
    {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :minus :thick-start 127480533
     :thick-end 127481699 :item-rgb "0,0,255"}])

  (with-open [r (io/reader "test-resources/test3.bed")]
   (bed/read-fields r) =>
   [{:chr "chr7" :start 127471197 :end 127472363 :name "Pos1" :score 0 :strand :plus}
    {:chr "chr7" :start 127472364 :end 127473530 :name "Pos2" :score 0 :strand :plus}
    {:chr "chr7" :start 127473531 :end 127474697 :name "Pos3" :score 0 :strand :plus}
    {:chr "chr7" :start 127474698 :end 127475864 :name "Pos4" :score 0 :strand :plus}
    {:chr "chr7" :start 127475865 :end 127477031 :name "Neg1" :score 0 :strand :minus}
    {:chr "chr7" :start 127477032 :end 127478198 :name "Neg2" :score 0 :strand :minus}
    {:chr "chr7" :start 127478199 :end 127479365 :name "Neg3" :score 0 :strand :minus}
    {:chr "chr7" :start 127479366 :end 127480532 :name "Pos5" :score 0 :strand :plus}
    {:chr "chr7" :start 127480533 :end 127481699 :name "Neg4" :score 0 :strand :minus}]))

(tabular
 (fact "bed file reader"
  (str->bed ?bed-str) => ?expected)
 ?bed-str          ?expected
 ""                []
 "#"               []
 "track name=foo"  []
 "browser"         []
 "1 0 1"           [{:chr "chr1" :start 1 :end 1}]
 "1 0 1\n"         [{:chr "chr1" :start 1 :end 1}]
 "1\t0\t1"         [{:chr "chr1" :start 1 :end 1}]
 "1\t0\t1\n"       [{:chr "chr1" :start 1 :end 1}]
 "chr1 0 1"        [{:chr "chr1" :start 1 :end 1}]
 "1 0 1\n1 1 2"    [{:chr "chr1" :start 1 :end 1} {:chr "chr1" :start 2 :end 2}]
 "1 0 1 Name"      [{:chr "chr1" :start 1 :end 1 :name "Name"}]
 "1 0 1 N 0"       [{:chr "chr1" :start 1 :end 1 :name "N" :score 0}]
 "1 0 1 N 0 +"     [{:chr "chr1" :start 1 :end 1 :name "N" :score 0 :strand :plus}]
 "1 0 1 N 0 + 0"   [{:chr "chr1" :start 1 :end 1 :name "N" :score 0 :strand :plus :thick-start 1}]
 "1 0 1 N 0 + 0 1" [{:chr "chr1" :start 1 :end 1 :name "N" :score 0 :strand :plus :thick-start 1 :thick-end 1}])

(tabular
 (fact "bed reader assertions"
  (str->bed ?bed-str) => (throws java.lang.AssertionError))
 ?bed-str
 "a"
 "\na"
 "trac"
 "1 0"
 "1 0 0"
 "1,0,1"
 "1 O 1"
 "chr 1 0 1")

(facts
 "bed sort"
 (bed/sort-fields []) => []
 (bed/sort-fields
  [{:chr "chr1" :start 1 :end 20} {:chr "chrY" :start 2 :end 4} {:chr "chr10" :start 1 :end 100}
   {:chr "chr1" :start 10 :end 20} {:chr "chr2" :start 20 :end 40} {:chr "chr1" :start 1 :end 3}])
 =>[{:chr "chr1" :start 1 :end 3} {:chr "chr1" :start 1 :end 20} {:chr "chr1" :start 10 :end 20}
    {:chr "chr2" :start 20 :end 40} {:chr "chr10" :start 1 :end 100} {:chr "chrY" :start 2 :end 4}]
  (bed/sort-fields
   [{:chr "chr10" :start 1 :end 2} {:chr "chr1_KI270892v1_alt" :start 1 :end 2} {:chr "chr1" :start 2 :end 3}
    {:chr "chr1" :start 1 :end 2} {:chr "chr10_KI270825v1_alt" :start 1 :end 2}
    {:chr "chr1_KI270714v1_random" :start 1 :end 2} {:chr "chr2" :start 1 :end 2} {:chr "chrM" :start 1 :end 2}
    {:chr "chr1_GL383518v1_alt" :start 1 :end 2} {:chr "another" :start 1 :end 2} {:chr "chrX" :start 1 :end 2}])
  => [{:chr "chr1" :start 1 :end 2} {:chr "chr1" :start 2 :end 3} {:chr "chr1_GL383518v1_alt" :start 1 :end 2}
      {:chr "chr1_KI270714v1_random" :start 1 :end 2} {:chr "chr1_KI270892v1_alt" :start 1 :end 2}
      {:chr "chr2" :start 1 :end 2} {:chr "chr10" :start 1 :end 2} {:chr "chr10_KI270825v1_alt" :start 1 :end 2}
      {:chr "chrX" :start 1 :end 2} {:chr "chrM" :start 1 :end 2} {:chr "another" :start 1 :end 2}])

(facts
 "bed merge"
 (bed/merge-fields []) => []
 (bed/merge-fields
  [{:chr "chr2" :start 1 :end 10} {:chr "chr1" :start 1 :end 10} {:chr "chr1" :start 4 :end 13}
   {:chr "chr1" :start 13 :end 15} {:chr "chr1" :start 16 :end 20}])
 => [{:chr "chr1" :start 1 :end 15} {:chr "chr1" :start 16 :end 20} {:chr "chr2" :start 1 :end 10}]
 (bed/merge-fields
  [{:chr "chr1" :start 1 :end 10 :name "chr1:1-10"} {:chr "chr1" :start 4 :end 13 :name "chr1:4-13"}])
 => [{:chr "chr1" :start 1 :end 13 :name "chr1:1-10+chr1:4-13"}])
