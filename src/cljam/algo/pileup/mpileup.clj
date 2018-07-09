(ns cljam.algo.pileup.mpileup
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [clojure.tools.logging :as logging]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.sam.util.flag :as flag]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sequence :as cseq])
  (:import [cljam.io.protocols SAMAlignment]
           [java.io BufferedWriter]))

(defn- cover-locus? [^long pos ^SAMAlignment aln]
  (<= (.pos aln) pos (.end aln))) ;; TODO: end?

(defn pileup-seq
  "Returns a lazy sequence that each element contains [position (reads piled-up at the locus)]."
  [^long start ^long end alns]
  (let [step (fn step [^long pos prev-buf rest-alns]
               (lazy-seq
                (when (< (dec pos) end)
                  (let [[i rests] (split-with (partial cover-locus? pos) rest-alns)
                        p (some-> rests ^SAMAlignment first .pos)
                        [b :as buf] (into (filterv #(<= pos (.end ^SAMAlignment %)) prev-buf) i)
                        next-pos (if (and p (not b)) p (inc pos))]
                    (if b
                      (cons [pos buf] (step next-pos buf rests))
                      (when (and p (<= p end))
                        (step next-pos buf rests)))))))
        [x :as xs] (drop-while #(< (.end ^SAMAlignment %) start) alns)]
    (when x
      (step (max start (.pos ^SAMAlignment x)) [] xs))))

(defn- quals-at-ref
  [idx ^String qual]
  (let [empty-qual? (and (= (.length qual) 1)
                         (= (.charAt qual 0) \*))]
    (if empty-qual?
      (vec (repeat (count idx) 93)) ;; \~
      (mapv (fn [[op x xs]]
              (if (number? x) (qual/fastq-char->phred-byte (.charAt qual x)) 93)) idx))))

(defn- seqs-at-ref
  [idx ^String s]
  (mapv (fn [[op x xs]]
          (let [c (if (number? x) (.charAt s x) x)]
            (case op
              :m [c]
              :d [c xs]
              :i [c (subs s (first xs) (last xs))]))) idx))

(defn index-cigar [^SAMAlignment aln]
  (let [idx (cigar/to-index (.cigar aln))]
    (assoc aln
           :seqs-at-ref (seqs-at-ref idx (:seq aln))
           :quals-at-ref (quals-at-ref idx (.qual aln)))))

(defn basic-mpileup-pred
  "Basic predicate function for filtering alignments for mpileup."
  [^long min-mapq]
  (fn [^SAMAlignment aln]
    (let [flag (.flag aln)]
      (and flag
           (zero? (bit-and (flag/encoded #{:unmapped :filtered-out :duplicated :supplementary :secondary}) flag))
           (or (not (flag/multiple? flag)) (flag/properly-aligned? flag))
           (not-empty (.cigar aln))
           (<= min-mapq (.mapq aln))))))

(defn gen-pile [chr [ref-pos alns]]
  {:rname chr
   :pos ref-pos
   :pile (mapv (fn seq-and-qual [^SAMAlignment aln]
                 (let [rel-pos (- ref-pos (.pos aln))]
                   {:qual ((:quals-at-ref aln) rel-pos)
                    :seq ((:seqs-at-ref aln) rel-pos)
                    :read aln})) alns)})

(defn- correct-qual
  "Correct quality of two overlapped mate reads by setting zero quality for one of the base."
  [{[s1] :seq q1 :qual :as r1} {[s2] :seq q2 :qual :as r2}]
  (if (= s1 s2)
    [(assoc r1 :qual (min 200 (+ q1 q2))) (assoc r2 :qual 0)]
    (if (<= q2 q1)
      [(assoc r1 :qual (int (* 0.8 q1))) (assoc r2 :qual 0)]
      [(assoc r1 :qual 0) (assoc r2 :qual (int (* 0.8 q2)))])))

(defn correct-overlapped-reads
  "Correct quality of two overlapped mate reads in piled-up reads."
  [{:keys [pile] :as p}]
  (if (<= (count pile) 1)
    p
    (->> pile
         (group-by (fn [x] (.qname ^SAMAlignment (:read x))))
         (into [] (mapcat
                   (fn [[qname xs]]
                     (if (<= (count xs) 1) xs (apply correct-qual xs)))))
         (assoc p :pile))))

(defn filter-by-base-quality
  "Returns a predicate for filtering piled-up reads by base quality at its position."
  [min-base-quality]
  (fn [p] (update p :pile (fn [pp] (filterv #(<= min-base-quality (:qual %)) pp)))))

(defn pileup
  ([sam-reader region]
   (pileup sam-reader region {}))
  ([sam-reader
    {:keys [chr start end] :or {start 1 end Integer/MAX_VALUE}}
    {:keys [min-base-quality min-map-quality] :or {min-base-quality 13 min-map-quality 0}}]
  (when-let [len (:len (refs/ref-by-name (sam/read-refs sam-reader) chr))]
    (let [s (max 1 start)
          e (min len end)]
      (->> {:chr chr :start s :end e}
           (sam/read-alignments sam-reader)
           (sequence
            (comp
             (filter (basic-mpileup-pred min-map-quality))
             (map index-cigar)))
           (pileup-seq s e)
           (sequence
            (comp (map (partial gen-pile chr))
                  (map correct-overlapped-reads)
                  (map (filter-by-base-quality min-base-quality))
                  (filter (comp seq :pile)))))))))

(defn align-pileup-seqs
  "Align multiple pileup seqs."
  [& xs]
  (if (<= (count xs) 1)
    (map (fn [{:keys [pos] :as m}] [pos [m]]) (first xs))
    (letfn [(step [xs]
              (lazy-seq
               (when (some seq xs)
                 (let [min-pos (apply min (keep (comp :pos first) xs))]
                   (cons [min-pos (mapv (fn [[{:keys [pos] :as m}]] (when (= pos min-pos) m)) xs)]
                         (step (mapv (fn [[{:keys [pos]} :as ys]] (if (= pos min-pos) (next ys) ys)) xs)))))))]
      (step xs))))

;; Writing
;; -------

(defn ^String stringify-mpileup-read
  [ref-reader rname pos ref {:keys [seq read]}]
  (let [forward? (not (flag/reversed? (:flag read)))
        case-fn (if forward? cstr/upper-case cstr/lower-case)
        sb (StringBuilder.)
        base (first seq)]
    (when (= (:pos read) pos)
      (.append sb \^)
      (.append sb (qual/phred-byte->fastq-char (:mapq read))))
    (if (= base ref)
      (.append sb (if forward? \. \,))
      (.append sb (case-fn base)))
    (when-let [x (second seq)]
      (if (number? x)
        (do (.append sb \-)
            (.append sb x)
            (.append sb (case-fn
                         (if ref-reader
                           (cseq/read-sequence ref-reader {:chr rname :start (inc pos) :end (+ pos x)})
                           (apply str (repeat x \N))))))
        (do (.append sb \+)
            (.append sb (count x))
            (.append sb (case-fn x)))))
    (when (= (:end read) pos)
      (.append sb \$))
    (str sb)))

(defn ^String stringify-mpileup-line
  [ref-reader {:keys [rname pos pile]}]
  (let [ref-base (some-> ref-reader
                         (cseq/read-sequence {:chr rname :start pos :end pos} {:mask? true}))
        ref-char (some-> ref-base cstr/upper-case first)
        bases (cstr/join (map (partial stringify-mpileup-read ref-reader rname pos ref-char) pile))
        quals (cstr/join (map (comp qual/phred-byte->fastq-char :qual) pile))]
    (cstr/join \tab [rname pos (or ref-base "N") (count pile) bases quals])))

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  [sam-reader ref-reader ^BufferedWriter out-writer region]
  (doseq [elm (pileup sam-reader region)]
    (.write out-writer (stringify-mpileup-line ref-reader elm))
    (.newLine out-writer)))
