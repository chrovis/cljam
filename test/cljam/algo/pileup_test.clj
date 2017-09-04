(ns cljam.algo.pileup-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.sam :as sam]
            [cljam.io.sequence :as cseq]
            [cljam.algo.pileup :as plp]
            [cljam.algo.pileup.mpileup :as mplp]))

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

(deftest about-pileup-seq

  ;; ----------
  ;; 1234567890...
  (is (= (map (fn [xs] (map #(dissoc % :end) xs))
              (mplp/pileup-seq 1 2 [{:pos 1 :cigar "10M" :end 10}]))
         [[{:pos 1 :cigar "10M"}] [{:pos 1 :cigar "10M"}]]))

  ;;    ----------
  ;;   ----------
  ;;  ----------
  ;; ----------
  ;; 1234567890123...
  (is (= (map count
              (mplp/pileup-seq 1 20 (map #(hash-map :pos (inc %) :cigar "10M" :end (+ % 10)) (range))))
         [1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map count
              (mplp/pileup-seq 101 120 (map #(hash-map :pos (inc %) :cigar "10M" :end (+ % 10)) (range))))
         [10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map :pos
              (last (mplp/pileup-seq 1 100000 (map #(hash-map :pos (inc %) :cigar "10M" :end (+ % 10)) (range)))))
         [99991 99992 99993 99994 99995 99996 99997 99998 99999 100000]))

  ;;     -----
  ;;    ----
  ;;   ---
  ;;  --
  ;; -
  ;; 1234567890...
  (is (= (map count
              (mplp/pileup-seq 1 10 (map #(hash-map :pos (inc %) :cigar (str (inc %) "M") :end (+ % (inc %))) (range))))
         [1 1 2 2 3 3 4 4 5 5]))

  ;;       --------
  ;;      ----------
  ;;     --
  ;;    ----
  ;;   ------
  ;;  --------
  ;; ----------
  ;; 1234567890...
  (is (= (map count
              (mplp/pileup-seq 1 10 (map #(hash-map :pos (inc %) :cigar (str (- 10 (* (mod % 5) 2)) "M") :end (+ % (- 10 (* (mod % 5) 2)))) (range))))
         [1 2 3 4 5 6 6 6 6 6])))

(deftest about-mpileup
  (with-open [br (sam/bam-reader test-sorted-bam-file)
              fr (cseq/fasta-reader test-fa-file)]
    (let [mplp-ref (doall (plp/mpileup br {:chr "ref"}))
          mplp-ref2 (doall (plp/mpileup br {:chr "ref2"}))]
      (is (= (map :rname mplp-ref) (repeat 45 "ref")))
      (is (= (map :ref mplp-ref) (repeat 45 \N)))
      (is (= (map :count mplp-ref) test-bam-pileup-ref))
      (is (= (map :pos mplp-ref) (range 1 46)))
      (is (= (map :seq mplp-ref) test-bam-mpileup-seq-ref))
      (is (= (map :qual mplp-ref) test-bam-mpileup-qual-ref))
      (is (= (map :count mplp-ref2) test-bam-pileup-ref2))
      (is (= (map :seq mplp-ref2) test-bam-mpileup-seq-ref2)))

    (let [mplp-ref (doall (plp/mpileup fr br {:chr "ref"}))]
      (is (= (map :rname mplp-ref) (repeat 45 "ref")))
      ;;                                  123456789012345678901234567890123456789012345
      (is (= (apply str (map :ref mplp-ref))
             "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
      (is (= (map :count mplp-ref) test-bam-pileup-ref))
      (is (= (map :pos mplp-ref) (range 1 46)))
      (is (= (map :seq mplp-ref) test-bam-mpileup-seq-ref-with-ref))
      (is (= (map :qual mplp-ref) test-bam-mpileup-qual-ref)))))

(deftest mpileup-region
  (with-open [br (sam/bam-reader test-sorted-bam-file)
              fr (cseq/fasta-reader test-fa-file)]
    ;; 1234567890123456789012345678901234567890
    ;; aggttttataaaacaattaagtctacagagcaactacgcg
    (let [mplp-ref1 (doall (plp/mpileup fr br {:chr "ref" :start 1 :end 40}))
          mplp-ref2 (doall (plp/mpileup fr br {:chr "ref2" :start 1 :end 40}))]
      (is (= (apply str (map :ref mplp-ref1))
             "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGC"))
      (is (= (map :count mplp-ref1) (take 40 test-bam-pileup-ref)))
      (is (= (map :pos mplp-ref1) (range 1 41)))
      (is (= (map :seq mplp-ref1) (take 40 test-bam-mpileup-seq-ref-with-ref)))
      (is (= (apply str (map :ref mplp-ref2))
             "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG")))))

(def reads-for-pileup
  [{:qname "R001" :flag 99 :rname "seq1" :pos 3 :end 10 :seq "AATTGGCCAA" :qual "AABBCCDDEE" :cigar "2S8M"
    :rnext "=" :pnext 1 :tlen 8 :mapq 60}
   {:qname "R001" :flag 147 :rname "seq1" :pos 3 :end 10 :seq "TTGGCCAATT" :qual "AABBCCDDEE" :cigar "8M2S"
    :rnext "=" :pnext 3 :tlen -8 :mapq 20}])

(deftest overlap-correction
  (let [plps (->> reads-for-pileup
                  (filter mplp/basic-mpileup-pred)
                  (mplp/pileup* "AATTGGCCAATTGGCC" {:chr "seq1" :start 1 :end 10})
                  (map (comp mplp/transpose-pile mplp/correct-overlapped-reads first)))]
    (is (= (map :ref plps) [\A \A \T \T \G \G \C \C \A \A]))
    (is (= (map :count plps) [0 0 2 2 2 2 2 2 2 2]))
    (is (= (map :seq plps) [[] [] [\. \.] [\. \.] [\. \.] [\. \.] [\. \.] [\. \.] [\. \.] [\. \.]]))
    (is (= (map :qual plps) [[] [] [\b \!] [\b \!] [\d \!] [\d \!] [\f \!] [\f \!] [\h \!] [\h \!]]))))

(deftest overlap-qual-correction
  (let [aplp (mplp/correct-overlapped-reads
              {:pile
               [{:seq \A :qual (- (int \I) 33) :read {:qname "R001" :flag 99}}
                {:seq \T :qual (- (int \A) 33) :read {:qname "R001" :flag 147}}]})]
    (is (= (map :seq (:pile aplp)) [\A \T]))
    (is (= (map :qual (:pile aplp)) [32 0])))
  (let [aplp (mplp/correct-overlapped-reads
              {:pile
               [{:seq \T :qual (- (int \A) 33) :read {:qname "R001" :flag 147}}
                {:seq \A :qual (- (int \I) 33) :read {:qname "R001" :flag 99}}]})]
    (is (= (map :seq (:pile aplp)) [\T \A]))
    (is (= (map :qual (:pile aplp)) [0 32]))))

(deftest filter-by-base-quality
  (let [plps (->> reads-for-pileup
                  (filter mplp/basic-mpileup-pred)
                  (mplp/pileup* "AATTGGCCAATTGGCC" {:chr "seq1" :start 1 :end 10})
                  (sequence
                   (comp
                    (map first)
                    (map mplp/correct-overlapped-reads)
                    (map (mplp/filter-by-base-quality 1))
                    (map mplp/transpose-pile))))]
    (is (= (map :ref plps) [\A \A \T \T \G \G \C \C \A \A]))
    (is (= (map :count plps) [0 0 1 1 1 1 1 1 1 1]))
    (is (= (map :seq plps) [[] [] [\.] [\.] [\.] [\.] [\.] [\.] [\.] [\.]]))
    (is (= (map :qual plps) [[] [] [\b] [\b] [\d] [\d] [\f] [\f] [\h] [\h]]))))

(deftest filter-by-map-quality
  (let [plps (->> reads-for-pileup
                  (sequence
                   (comp
                    (filter mplp/basic-mpileup-pred)
                    (filter (fn [a] (<= 30 (:mapq a))))))
                  (mplp/pileup* "AATTGGCCAATTGGCC" {:chr "seq1" :start 1 :end 10})
                  (sequence
                   (comp
                    (map first)
                    (map mplp/correct-overlapped-reads)
                    (map mplp/transpose-pile))))]
    (is (= (map :ref plps) [\A \A \T \T \G \G \C \C \A \A]))
    (is (= (map :count plps) [0 0 1 1 1 1 1 1 1 1]))
    (is (= (map :seq plps) [[] [] [\.] [\.] [\.] [\.] [\.] [\.] [\.] [\.]]))
    (is (= (map :qual plps) [[] [] [\B] [\B] [\C] [\C] [\D] [\D] [\E] [\E]]))))

(deftest about-create-mpileup
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [out-file (str temp-dir "/test.pileup")]
      (is (not-throw? (plp/create-mpileup test-sorted-bam-file out-file)))
      (is (same-file? out-file test-pileup-file)))))
