(ns cljam.algo.pileup-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.protocols :as p]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sequence :as cseq]
            [cljam.algo.pileup :as plp]
            [cljam.algo.pileup.mpileup :as mplp]
            [clojure.string :as cstr]))

(def test-bam-pileup-ref [0 0 0 0 0 0 1 1 3 3 3 3 3 3 2 3 3 3 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 2 2 2 1 1 1 1 1])
(def test-bam-pileup-ref2 [1 2 2 2 2 3 3 3 3 4 4 5 5 6 6 6 6 6 6 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 2 1 0 0 0 0])
(def test-bam-mpileup-seq-ref
  '(() () () () () () ("T") ("T") ("A" "A" "A") ("G" "G" "G") ("A" "A" "C") ("T" "T" "T") ("A" "A" "A")
    ("A+4AGAG" "A+2GG" "A") ("G" "G") ("A" "A" "A") ("T" "T" "T") ("A-1N" "A+2AA" "A") ("*" "G") ("C" "C")
    ("T" "T") ("G" ">") (">") (">") (">") (">") (">") (">") (">" "T") (">" "A") (">" "G") (">" "G") (">" "C")
    (">") (">+1C") ;; adjacent indel >+1T
    ("T") ("C" "C") ("A" "A") ("G" "G") ("C" "C") ("G") ("C") ("C") ("A") ("T")))
(def test-bam-mpileup-seq-ref-freq
  [{} {} {} {} {} {} {\T 1} {\T 1} {\A 3} {\G 3} {\A 2 \C 1} {\T 3} {\A 3}
   {\A 3} {\G 2} {\A 3} {\T 3} {\A 3} {\G 1 \* 1} {\C 2}
   {\T 2} {\G 1 \> 1} {\> 1} {\> 1} {\> 1} {\> 1} {\> 1} {\> 1} {\T 1 \> 1} {\A 1 \> 1} {\G 1 \> 1} {\G 1 \> 1} {\C 1 \> 1}
   {\> 1} {\> 1}
   {\T 1} {\C 2} {\A 2} {\G 2} {\C 2} {\G 1} {\C 1} {\C 1} {\A 1} {\T 1}])
(def test-bam-mpileup-seq-ref-ins
  [{} {} {} {} {} {} {} {} {} {} {} {} {}
   {"AGAG" 1 "GG" 1} {} {} {} {"AA" 1} {} {}
   {} {} {} {} {} {} {} {} {} {} {} {} {}
   {} {"C" 1}
   {} {} {} {} {} {} {} {} {} {}])
(def test-bam-mpileup-seq-ref-del
  [{} {} {} {} {} {} {} {} {} {} {} {} {}
   {} {} {} {} {1 1} {} {}
   {} {} {} {} {} {} {} {} {} {} {} {} {}
   {} {}
   {} {} {} {} {} {} {} {} {} {}])
(def test-bam-mpileup-seq-ref2
  '(("A") ("G" "G") ("G" "G") ("T" "T") ("T" "T") ("T" "T" "T") ("T" "T" "T") ("A" "A" "A") ("T" "T" "T")
    ("A" "A" "A" "C") ("A" "A" "A" "A") ("A" "A" "A" "A" "A") ("A" "A" "A" "A" "A") ("C" "C" "C+4AAAT" "T" "T" "T")
    ("A" "A" "A" "A" "A" "A") ("A" "A" "A" "A" "A" "A") ("A" "A" "T" "T" "T" "T") ("T" "T" "T" "T" "T" "T")
    ("A" "A" "A" "A" "A" "A") ("A" "A" "A" "A" "A" "A") ("T" "G" "G" "G" "G") ("T" "T" "T" "T" "T")
    ("C" "C" "C" "C") ("T" "T" "T" "T") ("A" "A" "A" "A") ("C" "C" "C" "C") ("A" "A" "A" "A") ("G" "G" "G")
    ("A" "A" "A") ("G" "G" "G") ("C" "C" "C") ("A" "A" "A") ("A" "A" "A") ("C" "C" "C") ("T" "T") ("A") () () () ()))
(def test-bam-mpileup-seq-ref2-freq
  [{\A 1} {\G 2} {\G 2} {\T 2} {\T 2} {\T 3} {\T 3} {\A 3} {\T 3}
   {\A 3 \C 1} {\A 4} {\A 5} {\A 5} {\C 3 \T 3}
   {\A 6} {\A 6} {\A 2 \T 4} {\T 6}
   {\A 6} {\A 6} {\T 1 \G 4} {\T 5}
   {\C 4} {\T 4} {\A 4} {\C 4} {\A 4} {\G 3}
   {\A 3} {\G 3} {\C 3} {\A 3} {\A 3} {\C 3} {\T 2} {\A 1} {} {} {} {}])
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

(defn- ->aln [m]
  (-> {:qname "DUMMY", :flag 0, :rname "DUMMY", :pos -1, :end -1, :rnext "DUMMY", :pnext -1, :tlen -1, :mapq -1, :cigar "10M", :seq "AAAAAAAAAA", :qual "IIIIIIIIII"}
      (merge m)
      (update :pos int)
      (update :end int)
      (update :flag int)
      (update :tlen int)
      (update :pnext int)
      (update :mapq int)
      p/map->SAMAlignment))

(defn- ->pile [[pos pile]]
  [pos (map (juxt :pos :end) pile)])

(deftest about-pileup-seq

  ;; ----------
  ;; 1234567890...
  (is (= (map ->pile (mplp/pileup-seq 1 2 (mapv ->aln [{:pos 1 :cigar "10M" :end 10}])))
         [[1 [[1 10]]]
          [2 [[1 10]]]]))

  ;;    ----------
  ;;   ----------
  ;;  ----------
  ;; ----------
  ;; 1234567890123...
  (is (= (map (comp count second)
              (mplp/pileup-seq 1 20 (map #(->aln (hash-map :pos (inc %) :cigar "10M" :end (+ % 10))) (range))))
         [1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map (comp count second)
              (mplp/pileup-seq 101 120 (map #(->aln (hash-map :pos (inc %) :cigar "10M" :end (+ % 10))) (range))))
         [10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map :pos
              (second (last (mplp/pileup-seq 1 100000 (map #(->aln (hash-map :pos (inc %) :cigar "10M" :end (+ % 10))) (range))))))
         [99991 99992 99993 99994 99995 99996 99997 99998 99999 100000]))

  ;;     -----
  ;;    ----
  ;;   ---
  ;;  --
  ;; -
  ;; 1234567890...
  (is (= (map (comp count second)
              (mplp/pileup-seq 1 10 (map #(->aln (hash-map :pos (inc %) :cigar (str (inc %) "M") :end (+ % (inc %)))) (range))))
         [1 1 2 2 3 3 4 4 5 5]))

  ;;       --------
  ;;      ----------
  ;;     --
  ;;    ----
  ;;   ------
  ;;  --------
  ;; ----------
  ;; 1234567890...
  (is (= (map (comp count second)
              (mplp/pileup-seq 1 10 (map #(->aln (hash-map :pos (inc %) :cigar (str (- 10 (* (mod % 5) 2)) "M") :end (+ % (- 10 (* (mod % 5) 2))))) (range))))
         [1 2 3 4 5 6 6 6 6 6])))

(deftest pileup-seq
  (testing "empty"
    (is (nil? (seq (mplp/pileup-seq 1 10 []))))
    (is (nil? (seq (mplp/pileup-seq 2 10 (map ->aln [{:pos 1 :end 1}])))))
    (is (nil? (seq (mplp/pileup-seq 1 10 (map ->aln [{:pos 11 :end 11}]))))))
  (testing "dense"
    (are [?in ?out]
        (= ?out (mapv ->pile (mplp/pileup-seq 1 10 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[1 1]] [[1 [[1 1]]]]
      [[3 5]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]]]
      [[3 5] [4 4]] [[3 [[3 5]]] [4 [[3 5] [4 4]]] [5 [[3 5]]]]
      [[3 5] [4 6]] [[3 [[3 5]]] [4 [[3 5] [4 6]]] [5 [[3 5] [4 6]]] [6 [[4 6]]]]))
  (testing "sparse"
    (are [?in ?out]
        (= ?out (mapv ->pile (mplp/pileup-seq 1 10 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[3 5] [14 16]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]]]
      [[3 5] [6 7]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]] [6 [[6 7]]] [7 [[6 7]]]]
      [[3 5] [4 5] [6 7]] [[3 [[3 5]]] [4 [[3 5] [4 5]]] [5 [[3 5] [4 5]]] [6 [[6 7]]] [7 [[6 7]]]]
      [[3 5] [7 8] [10 11]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]] [7 [[7 8]]] [8 [[7 8]]] [10 [[10 11]]]])))

(deftest align-pileup-seqs
  (testing "empty"
    (is (empty? (mplp/align-pileup-seqs)))
    (is (empty? (mplp/align-pileup-seqs [])))
    (is (empty? (mplp/align-pileup-seqs [] []))))
  (testing "single"
    (are [?in ?out]
        (= ?out (mplp/align-pileup-seqs ?in))
      [{:pos 3 :pile [{:pos 3 :end 5}]}]
      [[3 [{:pos 3 :pile [{:pos 3 :end 5}]}]]]

      [{:pos 3 :pile [[{:pos 3 :end 5}]]}
       {:pos 4 :pile [[{:pos 3 :end 5}]]}
       {:pos 5 :pile [[{:pos 3 :end 5}]]}]
      [[3 [{:pos 3 :pile [[{:pos 3 :end 5}]]}]]
       [4 [{:pos 4 :pile [[{:pos 3 :end 5}]]}]]
       [5 [{:pos 5 :pile [[{:pos 3 :end 5}]]}]]]

      [{:pos 3 :pile [{:pos 3 :end 5}]}
       {:pos 4 :pile [{:pos 3 :end 5} {:pos 4 :end 4}]}
       {:pos 5 :pile [{:pos 3 :end 5}]}
       {:pos 8 :pile [{:pos 8 :end 8}]}]
      [[3 [{:pos 3 :pile [{:pos 3 :end 5}]}]]
       [4 [{:pos 4 :pile [{:pos 3 :end 5} {:pos 4, :end 4}]}]]
       [5 [{:pos 5 :pile [{:pos 3 :end 5}]}]]
       [8 [{:pos 8 :pile [{:pos 8 :end 8}]}]]]))
  (testing "multiple"
    (is (= [[3 [{:pos 3, :pile [{:pos 3, :end 5}]} nil]]
            [4 [{:pos 4, :pile [{:pos 3, :end 5}]} {:pos 4, :pile [{:pos 4, :end 6}]}]]
            [5 [{:pos 5, :pile [{:pos 3, :end 5}]} {:pos 5, :pile [{:pos 4, :end 6}]}]]
            [6 [nil {:pos 6, :pile [{:pos 4, :end 6}]}]]]
           (mplp/align-pileup-seqs [{:pos 3 :pile [{:pos 3 :end 5}]}
                                    {:pos 4 :pile [{:pos 3 :end 5}]}
                                    {:pos 5 :pile [{:pos 3 :end 5}]}]
                                   [{:pos 4 :pile [{:pos 4 :end 6}]}
                                    {:pos 5 :pile [{:pos 4 :end 6}]}
                                    {:pos 6 :pile [{:pos 4 :end 6}]}])))
    (is (= [[3 [{:pos 3, :pile [{:pos 3, :end 5}]} nil]]
            [4 [{:pos 4, :pile [{:pos 3, :end 5}]} nil]]
            [5 [{:pos 5, :pile [{:pos 3, :end 5}]} nil]]
            [8 [nil {:pos 8, :pile [{:pos 8, :end 9}]}]]
            [9 [nil {:pos 9, :pile [{:pos 8, :end 9}]}]]]
           (mplp/align-pileup-seqs [{:pos 3 :pile [{:pos 3 :end 5}]}
                                    {:pos 4 :pile [{:pos 3 :end 5}]}
                                    {:pos 5 :pile [{:pos 3 :end 5}]}]
                                   [{:pos 8 :pile [{:pos 8 :end 9}]}
                                    {:pos 9 :pile [{:pos 8 :end 9}]}])))
    (is (= [[3 [{:pos 3, :pile [{:pos 3, :end 4}]} nil {:pos 3, :pile [{:pos 3, :end 3}]} nil]]
            [4 [{:pos 4, :pile [{:pos 3, :end 4}]} {:pos 4, :pile [{:pos 4, :end 4}]} nil nil]]]
           (mplp/align-pileup-seqs [{:pos 3 :pile [{:pos 3 :end 4}]}
                                    {:pos 4 :pile [{:pos 3 :end 4}]}]
                                   [{:pos 4 :pile [{:pos 4 :end 4}]}]
                                   [{:pos 3 :pile [{:pos 3 :end 3}]}]
                                   [])))))

(deftest about-mpileup
  (testing "dense"
    (with-open [br (sam/bam-reader test-sorted-bam-file)
                fr (cseq/fasta-reader test-fa-file)]
      (let [mplp-ref (doall (plp/mpileup br {:chr "ref"}))
            mplp-ref2 (doall (plp/mpileup br {:chr "ref2"}))]
        (is (= (repeat 39 "ref")
               (map :rname mplp-ref)))
        (is (= (filter pos? test-bam-pileup-ref)
               (map (comp count :pile) mplp-ref)))
        (is (= (->> test-bam-pileup-ref
                    (map vector (range))
                    (filter (comp pos? second))
                    (map (comp inc first)))
               (map :pos mplp-ref)))
        (is (= (remove empty? test-bam-mpileup-seq-ref-freq)
               (map #(frequencies (map (comp first :seq) (:pile %))) mplp-ref)))
        (is (= (keep #(seq (map qual/fastq-char->phred-byte %)) test-bam-mpileup-qual-ref)
               (map #(map :qual (:pile %)) mplp-ref)))
        (is (= (filter pos? test-bam-pileup-ref2)
               (map (comp count :pile) mplp-ref2)))
        (is (= (remove empty? test-bam-mpileup-seq-ref2-freq)
               (map #(frequencies (map (comp first :seq) (:pile %))) mplp-ref2)))))))

(deftest mpileup-region
  (with-open [br (sam/bam-reader test-sorted-bam-file)]
    ;; 1234567890123456789012345678901234567890
    ;; aggttttataaaacaattaagtctacagagcaactacgcg
    (let [mplp-ref1 (doall (plp/mpileup br {:chr "ref" :start 1 :end 40}))
          mplp-ref2 (doall (plp/mpileup br {:chr "ref2" :start 1 :end 40}))]
      (is (= (filter pos? (take 40 test-bam-pileup-ref)) (map (comp count :pile) mplp-ref1)))
      (is (= (->> test-bam-pileup-ref
                  (take 40)
                  (map vector (range))
                  (filter (comp pos? second))
                  (map (comp inc first)))
             (map :pos mplp-ref1)))
      (is (= (filter seq (take 40 test-bam-mpileup-seq-ref-freq))
             (map #(frequencies (map (comp first :seq) (:pile %))) mplp-ref1))))))

(def ^:private reads-for-pileup
  (mapv
   p/map->SAMAlignment
   [{:qname "R001" :flag (int 99) :rname "seq1" :pos (int 3) :end (int 10) :seq "AATTGGCCAA" :qual "AABBCCDDEE" :cigar "2S8M"
     :rnext "=" :pnext (int 3) :tlen (int 8) :mapq (int 60)}
    {:qname "R001" :flag (int 147) :rname "seq1" :pos (int 3) :end (int 10) :seq "TTGGCCAATT" :qual "AABBCCDDEE" :cigar "8M2S"
     :rnext "=" :pnext (int 3) :tlen (int -8) :mapq (int 20)}]))

(defn- pileup* [{:keys [chr start end] :as region} xs]
  (->> xs
       (sequence
        (comp
         (filter (mplp/basic-mpileup-pred 13))
         (map mplp/index-cigar)))
       (mplp/pileup-seq start end)
       (map (partial mplp/gen-pile chr))))

(deftest overlap-correction
  (let [plps (->> reads-for-pileup
                  (pileup* {:chr "seq1" :start 1 :end 10})
                  (map mplp/correct-overlapped-reads))]
    (is (= (filter pos? [0 0 2 2 2 2 2 2 2 2])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T \T] [\T \T] [\G \G] [\G \G] [\C \C] [\C \C] [\A \A] [\A \A]])
           (map #(map (comp first :seq) (:pile %)) plps)))
    (is (= (filter seq [[] [] [65 0] [65 0] [67 0] [67 0] [69 0] [69 0] [71 0] [71 0]])
           (map #(map :qual (:pile %)) plps)))))

(deftest overlap-qual-correction
  (let [aplp (mplp/correct-overlapped-reads
              {:pile
               (map #(update % :read ->aln)
                    [{:seq [\A] :qual (- (int \I) 33) :read {:qname "R001" :flag 99}}
                     {:seq [\T] :qual (- (int \A) 33) :read {:qname "R001" :flag 147}}])})]
    (is (= (map (comp first :seq) (:pile aplp)) [\A \T]))
    (is (= (map :qual (:pile aplp)) [32 0])))
  (let [aplp (mplp/correct-overlapped-reads
              {:pile
               (map #(update % :read ->aln)
                    [{:seq [\T] :qual (- (int \A) 33) :read {:qname "R001" :flag 147}}
                     {:seq [\A] :qual (- (int \I) 33) :read {:qname "R001" :flag 99}}])})]
    (is (= (map (comp first :seq) (:pile aplp)) [\T \A]))
    (is (= (map :qual (:pile aplp)) [0 32]))))

(deftest filter-by-base-quality
  (let [plps (->> reads-for-pileup
                  (pileup* {:chr "seq1" :start 1 :end 10})
                  (sequence
                   (comp
                    (map mplp/correct-overlapped-reads)
                    (map (mplp/filter-by-base-quality 1)))))]
    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\A] [\A]])
           (map #(map (comp first :seq) (:pile %)) plps)))
    (is (= (filter seq [[] [] [65] [65] [67] [67] [69] [69] [71] [71]])
           (map #(map :qual (:pile %)) plps)))))

(deftest filter-by-map-quality
  (let [plps (->> reads-for-pileup
                  (filter (fn [a] (<= 30 (:mapq a))))
                  (pileup* {:chr "seq1" :start 1 :end 10})
                  (sequence
                   (comp
                    (map mplp/correct-overlapped-reads))))]
    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\A] [\A]])
           (map #(map (comp first :seq) (:pile %)) plps)))
    (is (= (filter seq [[] [] [33] [33] [34] [34] [35] [35] [36] [36]])
           (map #(map :qual (:pile %)) plps)))))

(deftest stringify-mpileup-read
  (testing "without-ref"
    (are [?in ?out]
        (= ?out (mplp/stringify-mpileup-read nil "chr1" 10 nil ?in))
      {:seq [\A] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "A"
      {:seq [\N] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "N"
      {:seq [\A] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "a"
      {:seq [\N] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "n"
      {:seq [\A "A"] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "A+1A"
      {:seq [\A 2] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "A-2NN"
      {:seq [\A "AT"] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "a+2at"
      {:seq [\A 3] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "a-3nnn"
      {:seq [\A] :read {:flag 0 :mapq 60 :pos 10 :end 15}} "^]A"
      {:seq [\A "AT"] :read {:flag 16 :mapq 40 :pos 10 :end 15}} "^Ia+2at"
      {:seq [\A] :read {:flag 0 :mapq 40 :pos 5 :end 10}} "A$"
      {:seq [\A 4] :read {:flag 16 :mapq 40 :pos 5 :end 10}} "a-4nnnn$"))
  (testing "with-ref"
    (let [r (reify p/ISequenceReader
              (p/read-sequence [this {:keys [start end]}]
                (subs "ATGCATGCATGCATGC" (dec start) end)))]
      (are [?in ?out]
          (= ?out (mplp/stringify-mpileup-read r "chr1" 10 \T ?in))
        {:seq [\A] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "A"
        {:seq [\T] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "."
        {:seq [\T] :read {:flag 16 :mapq 60 :pos 5 :end 15}} ","
        {:seq [\N] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "N"
        {:seq [\A] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "a"
        {:seq [\N] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "n"
        {:seq [\A "A"] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "A+1A"
        {:seq [\A 2] :read {:flag 0 :mapq 60 :pos 5 :end 15}} "A-2GC"
        {:seq [\A "AT"] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "a+2at"
        {:seq [\A 3] :read {:flag 16 :mapq 60 :pos 5 :end 15}} "a-3gca"
        {:seq [\A] :read {:flag 0 :mapq 60 :pos 10 :end 15}} "^]A"
        {:seq [\A "AT"] :read {:flag 16 :mapq 40 :pos 10 :end 15}} "^Ia+2at"
        {:seq [\A] :read {:flag 0 :mapq 40 :pos 5 :end 10}} "A$"
        {:seq [\A 4] :read {:flag 16 :mapq 40 :pos 5 :end 10}} "a-4gcat$"))))

(deftest stringify-mpileup-line
  (testing "without-ref"
    (are [?in ?out]
        (= ?out (mplp/stringify-mpileup-line nil ?in))
      {:rname "chr1" :pos 10 :pile []} "chr1\t10\tN\t0\t\t")))

(deftest about-create-mpileup
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [out-file (str temp-dir "/test.pileup")]
      (is (not-throw? (plp/create-mpileup test-sorted-bam-file out-file)))
      (doseq [[r1 r2] (->> [test-pileup-file out-file]
                           (map (comp cstr/split-lines slurp))
                           (apply map vector))
              :when (not (cstr/starts-with? r1 "ref\t35\t"))] ;; adjacent indel
        (is (= r1 r2))))))
