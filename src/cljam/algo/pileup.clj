(ns cljam.algo.pileup
  "Functions to calculate pileup from the BAM."
  (:require [cljam.io.sam :as sam]
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.sam.util.flag :as flag]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.pileup :as plpio])
  (:import [cljam.io.protocols SAMAlignment]
           [cljam.io.pileup PileupBase LocusPile]))

(defn- cover-locus? [^long pos ^SAMAlignment aln]
  (<= (.pos aln) pos))

(defn pileup-seq
  "Returns a lazy sequence that each element contains [position (alignments
  piled-up at the locus)]."
  [^long start ^long end alns]
  (letfn [(step [^long pos prev-buf rest-alns]
            (lazy-seq
             (when (< (dec pos) end)
               (let [[i rests] (split-with (partial cover-locus? pos) rest-alns)
                     p (some-> rests ^SAMAlignment first .pos)
                     [b :as buf] (into (filterv #(<= pos (.end ^SAMAlignment %))
                                                prev-buf) i)
                     next-pos (if (and p (not b)) p (inc pos))]
                 (if b
                   (cons [pos buf] (step next-pos buf rests))
                   (when (and p (<= p end))
                     (step next-pos buf rests)))))))]
    (let [[x :as xs] (drop-while #(< (.end ^SAMAlignment %) start) alns)]
      (when x
        (step (max start (.pos ^SAMAlignment x)) [] xs)))))

(defn- quals-at-ref
  [idx ^String qual]
  (let [empty-qual? (and (= (.length qual) 1)
                         (= (.charAt qual 0) \*))]
    (if empty-qual?
      (vec (repeat (count idx) 93)) ;; \~
      (mapv (fn [[_ x]]
              (if (number? x)
                (qual/fastq-char->phred-byte (.charAt qual x))
                93))
            idx))))

(defn- seqs-at-ref
  [idx ^String s]
  (mapv (fn [[op x xs]]
          (let [c (if (number? x) (.charAt s x) x)]
            (case op
              :m [c]
              :d [c xs]
              :i [c (subs s (first xs) (last xs))]))) idx))

(defn index-cigar
  "Align bases and base quality scores with the reference coordinate."
  [^SAMAlignment aln]
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
           (zero? (bit-and (flag/encoded #{:unmapped
                                           :filtered-out
                                           :duplicated
                                           :supplementary
                                           :secondary}) flag))
           (or (not (flag/multiple? flag)) (flag/properly-aligned? flag))
           (not-empty (.cigar aln))
           (<= min-mapq (.mapq aln))))))

(defn resolve-base
  "Find a piled-up base and an indel from an alignment."
  [^long ref-pos ^SAMAlignment aln]
  (let [relative-pos (- ref-pos (.pos aln))
        qual ((:quals-at-ref aln) relative-pos)
        [base indel] ((:seqs-at-ref aln) relative-pos)]
    (-> (PileupBase.
         (zero? relative-pos)
         (when (zero? relative-pos) (.mapq aln))
         base
         qual
         (flag/reversed? (.flag aln))
         (= ref-pos (.end aln))
         (when-not (number? indel) indel)
         (when (number? indel) indel)
         (.qname aln))
        (assoc :alignment aln))))

(defn- resolve-bases
  [[ref-pos alns]]
  [ref-pos (mapv (partial resolve-base ref-pos) alns)])

(defn ->locus-pile
  "Convert a pile into `cljam.io.pileup.LocusPile`."
  [chr [pos pile]]
  (let [c (count pile)]
    (when (pos? c)
      (LocusPile. chr pos \N c pile))))

(defn- correct-qual
  "Correct quality of two overlapped mate reads by setting zero quality for one
  of the base."
  [^PileupBase r1 ^PileupBase r2]
  (let [b1 (.base r1)
        q1 (.qual r1)
        b2 (.base r2)
        q2 (.qual r2)]
    (if (= b1 b2)
      [(assoc r1 :qual (min 200 (+ q1 q2))) (assoc r2 :qual 0)]
      (if (<= q2 q1)
        [(assoc r1 :qual (int (* 0.8 q1))) (assoc r2 :qual 0)]
        [(assoc r1 :qual 0) (assoc r2 :qual (int (* 0.8 q2)))]))))

(defn correct-overlapped-bases
  "Find out overlapped bases and tweak their base quality scores."
  [xs]
  (if (<= (count xs) 1)
    xs
    (->> xs
         (group-by (fn [x] (.qname ^PileupBase x)))
         (into [] (mapcat
                   (fn [[_ xs]]
                     (if (<= (count xs) 1) xs (apply correct-qual xs))))))))

(defn- correct-overlaps
  [pile]
  (update pile 1 correct-overlapped-bases))

(defn filter-by-base-quality
  "Returns a predicate for filtering piled-up reads by base quality at its
  position."
  [min-base-quality]
  (fn [p]
    (->> #(<= min-base-quality (.qual ^PileupBase %))
         (partial filterv)
         (update p 1))))

(defn pileup
  "Piles up alignments in given region and returns a lazy sequence of
  `cljam.io.pileup.LocusPile`s.

  The following options are available:
  - `min-base-quality` Minimum quality of called bases [13]
  - `min-map-quality` Minimum quality of alignments [0]
  - `ignore-overlaps?` Disable detecting overlapped bases of PE reads [false]"
  ([sam-reader region]
   (pileup sam-reader region {}))
  ([sam-reader
    {:keys [chr start end] :or {start 1 end Integer/MAX_VALUE}}
    {:keys [min-base-quality min-map-quality ignore-overlaps?]
     :or {min-base-quality 13 min-map-quality 0 ignore-overlaps? false}}]
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
             (comp (map resolve-bases)
                   (if ignore-overlaps?
                     identity
                     (map correct-overlaps))
                   (if (pos? min-base-quality)
                     (map (filter-by-base-quality min-base-quality))
                     identity)
                   (keep (partial ->locus-pile chr)))))))))

(defn align-pileup-seqs
  "Align multiple piled-up seqs."
  [& xs]
  (if (<= (count xs) 1)
    (map (fn [{:keys [pos] :as m}] [pos [m]]) (first xs))
    (letfn [(step [xs]
              (lazy-seq
               (when (some seq xs)
                 (let [min-pos (apply min (keep (comp :pos first) xs))]
                   (cons [min-pos (mapv
                                   (fn [[{:keys [pos] :as m}]]
                                     (when (= pos min-pos) m)) xs)]
                         (step (mapv
                                (fn [[{:keys [pos]} :as ys]]
                                  (if (= pos min-pos) (next ys) ys)) xs)))))))]
      (step xs))))

(defn mpileup
  "Pile up alignments from multiple sources.

  The following `options` are available:
  - `min-base-quality` Minimum quality of called bases [13]
  - `min-map-quality` Minimum quality of alignments [0]
  - `ignore-overlaps?` Disable detecting overlapped bases of PE reads [false]"
  [region options & sam-readers]
  (apply align-pileup-seqs (map #(pileup % region options) sam-readers)))

(defn create-mpileup
  "Creates a mpileup file from the BAM file.

  The following `options` are available:
  - `min-base-quality` Minimum quality of called bases [13]
  - `min-map-quality` Minimum quality of alignments [0]
  - `ignore-overlaps?` Disable detecting overlapped bases of PE reads [false]"
  ([in-sam out-mplp]
   (create-mpileup in-sam nil out-mplp))
  ([in-sam in-ref out-mplp]
   (create-mpileup in-sam in-ref out-mplp nil))
  ([in-sam in-ref out-mplp region]
   (create-mpileup in-sam in-ref out-mplp region nil))
  ([in-sam in-ref out-mplp region options]
   (with-open [s (sam/reader in-sam)
               w (plpio/writer out-mplp in-ref)]
     (let [regs (if region
                  [region]
                  (map
                   (fn [{:keys [name len]}]
                     {:chr name :start 1 :end len})
                   (sam/read-refs s)))]
       (doseq [reg regs]
         (plpio/write-piles w (pileup s reg options)))))))
