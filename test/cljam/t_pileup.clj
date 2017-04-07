(ns cljam.t-pileup
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.bam :as bam]
            [cljam.fasta :as fa]
            [cljam.pileup :as plp]
            [cljam.pileup.mpileup :as mplp]))

(def test-bam-pileup-ref '(0 0 0 0 0 0 1 1 3 3 3 3 3 3 2 3 3 3 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 2 2 2 1 1 1 1 1))
(def test-bam-pileup-ref2 '(1 2 2 2 2 3 3 3 3 4 4 5 5 6 6 6 6 6 6 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 2 1 0 0 0 0))
(def test-bam-mpileup-seq-ref
  '(() () () () () () ("T") ("T") ("A" "A" "A") ("G" "G" "G") ("A" "A" "C") ("T" "T" "T") ("A" "A" "A")
    ("A+4AGAG" "A+2GG" "A") ("G" "G") ("A" "A" "A") ("T" "T" "T") ("A-1N" "A+2AA" "A") ("*" "G") ("C" "C")
    ("T" "T") ("G" ">") (">") (">") (">") (">") (">") (">") (">" "T") (">" "A") (">" "G") (">" "G") (">" "C")
    (">") (">+1C") ;; adjacent indel >+1T
    ("T") ("C" "C") ("A" "A") ("G" "G") ("C" "C") ("G") ("C") ("C") ("A") ("T")))
(def test-bam-mpileup-seq-ref2
  '(("A") ("G" "G") ("G" "G") ("T" "T") ("T" "T") ("T" "T" "T") ("T" "T" "T") ("A" "A" "A") ("T" "T" "T")
    ("A" "A" "A" "C") ("A" "A" "A" "A") ("A" "A" "A" "A" "A") ("A" "A" "A" "A" "A") ("C" "C" "C+4AAAT" "T" "T" "T")
    ("A" "A" "A" "A" "A" "A") ("A" "A" "A" "A" "A" "A") ("A" "A" "T" "T" "T" "T") ("T" "T" "T" "T" "T" "T")
    ("A" "A" "A" "A" "A" "A") ("A" "A" "A" "A" "A" "A") ("T" "G" "G" "G" "G") ("T" "T" "T" "T" "T")
    ("C" "C" "C" "C") ("T" "T" "T" "T") ("A" "A" "A" "A") ("C" "C" "C" "C") ("A" "A" "A" "A") ("G" "G" "G")
    ("A" "A" "A") ("G" "G" "G") ("C" "C" "C") ("A" "A" "A") ("A" "A" "A") ("C" "C" "C") ("T" "T") ("A") () () () ()))
(def test-bam-mpileup-seq-ref-with-ref
  '(() () () () () () (".") (".") ("." "." ".") ("." "." ".") ("." "." "C") ("." "." ".") ("." "." ".")
    (".+4AGAG" ".+2GG" ".") ("." ".") ("." "." ".") ("." "." ".") (".-1G" ".+2AA" ".") ("*" ".") ("." ".")
    ("." ".") ("." ">") (">") (">") (">") (">") (">") (">") (">" ".") (">" ".") (">" ".") (">" ".") (">" ".")
    (">") (">+1C") ;; adjacent indel >+1T
    (".") ("." ".") ("." ".") ("." ".") ("." ".") (".") (".") (".") (".") (".")))
(def test-bam-mpileup-qual-ref
  '([] [] [] [] [] [] [\~] [\~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~]
    [\~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~] [\~] [\~] [\~]
    [\~] [\~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~] [\~] [\~] [\~ \~] [\~ \~] [\~ \~] [\~ \~]
    [\~] [\~] [\~] [\~] [\~]))

(deftest pileup-returns-LazySeq
  (is (= (type (plp/pileup (bam/reader test-sorted-bam-file) "ref" nil))
         clojure.lang.LazySeq))
  (is (= (type (plp/pileup (bam/reader test-sorted-bam-file) "ref2" nil))
         clojure.lang.LazySeq)))

(deftest pileup
  (is (= (plp/pileup (bam/reader test-sorted-bam-file) "ref" nil)
         test-bam-pileup-ref))
  (is (= (plp/pileup (bam/reader test-sorted-bam-file) "ref2" nil)
         test-bam-pileup-ref2)))

(deftest first-pos
  (is (= (plp/first-pos (bam/reader test-sorted-bam-file) "ref" 0 64) 7))
  (is (= (plp/first-pos (bam/reader test-sorted-bam-file) "ref2" 0 64) 1)))

(deftest pileup-seq
  ;; ----------
  ;; 1234567890...
  (is (= (map (fn [xs] (map #(dissoc % :end) xs))
              (mplp/pileup-seq 1 2 [{:pos 1 :cigar "10M"}]))
         [[{:pos 1 :cigar "10M"}] [{:pos 1 :cigar "10M"}]]))

  ;;    ----------
  ;;   ----------
  ;;  ----------
  ;; ----------
  ;; 1234567890123...
  (is (= (map count (mplp/pileup-seq 1 20 (map #(hash-map :pos (inc %) :cigar "10M") (range))))
         [1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map count (mplp/pileup-seq 101 120 (map #(hash-map :pos (inc %) :cigar "10M") (range))))
         [10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map :pos (last (mplp/pileup-seq 1 1000000 (map #(hash-map :pos (inc %) :cigar "10M") (range)))))
         [999991 999992 999993 999994 999995 999996 999997 999998 999999 1000000]))

  ;;     -----
  ;;    ----
  ;;   ---
  ;;  --
  ;; -
  ;; 1234567890...
  (is (= (map count (mplp/pileup-seq 1 10 (map #(hash-map :pos (inc %) :cigar (str (inc %) "M")) (range))))
         [1 1 2 2 3 3 4 4 5 5]))

  ;;       --------
  ;;      ----------
  ;;     --
  ;;    ----
  ;;   ------
  ;;  --------
  ;; ----------
  ;; 1234567890...
  (is (= (map count (mplp/pileup-seq 1 10 (map #(hash-map :pos (inc %) :cigar (str (- 10 (* (mod % 5) 2)) "M")) (range))))
         [1 2 3 4 5 6 6 6 6 6])))

(deftest mpileup
  (with-open [br (bam/reader test-sorted-bam-file)
              fr (fa/reader test-fa-file)]
    (let [mplp-ref (doall (plp/mpileup br "ref"))
          mplp-ref2 (doall (plp/mpileup br "ref2"))]
      (is (= (map :rname mplp-ref) (repeat 45 "ref")))
      (is (= (map :ref mplp-ref) (repeat 45 \N)))
      (is (= (map :count mplp-ref) test-bam-pileup-ref))
      (is (= (map :pos mplp-ref) (range 1 46)))
      (is (= (map :seq mplp-ref) test-bam-mpileup-seq-ref))
      (is (= (map :qual mplp-ref) test-bam-mpileup-qual-ref))
      (is (= (map :count mplp-ref2) test-bam-pileup-ref2))
      (is (= (map :seq mplp-ref2) test-bam-mpileup-seq-ref2)))

    (let [mplp-ref (doall (plp/mpileup fr br "ref"))]
      (is (= (map :rname mplp-ref) (repeat 45 "ref")))
      ;;                                  123456789012345678901234567890123456789012345
      (is (= (apply str (map :ref mplp-ref))
             "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
      (is (= (map :count mplp-ref) test-bam-pileup-ref))
      (is (= (map :pos mplp-ref) (range 1 46)))
      (is (= (map :seq mplp-ref) test-bam-mpileup-seq-ref-with-ref))
      (is (= (map :qual mplp-ref) test-bam-mpileup-qual-ref)))))

(deftest mpileup-region
  (with-open [br (bam/reader test-sorted-bam-file)
              fr (fa/reader test-fa-file)]
    ;; 1234567890123456789012345678901234567890
    ;; aggttttataaaacaattaagtctacagagcaactacgcg
    (let [mplp-ref1 (doall (plp/mpileup fr br "ref" 1 40))
          mplp-ref2 (doall (plp/mpileup fr br "ref2" 1 40))]
      (is (= (apply str (map :ref mplp-ref1))
             "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGC"))
      (is (= (map :count mplp-ref1) (take 40 test-bam-pileup-ref)))
      (is (= (map :pos mplp-ref1) (range 1 41)))
      (is (= (map :seq mplp-ref1) (take 40 test-bam-mpileup-seq-ref-with-ref)))
      (is (= (apply str (map :ref mplp-ref2))
             "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG")))))
