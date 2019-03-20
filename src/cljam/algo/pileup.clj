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

(def ^:const base-chunk-size 5000)

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
         (.mapq aln)
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
  (when (seq pile)
    (LocusPile. chr pos pile)))

(defn filter-by-base-quality
  "Returns a predicate for filtering piled-up reads by base quality at its
  position."
  [min-base-quality]
  (fn [p]
    (->> #(<= min-base-quality (.qual ^PileupBase %))
         (partial filterv)
         (update p 1))))

(defn- merge-corrected-quals
  "Merge corrected quals with the uncorrected part."
  [^SAMAlignment aln ^long correct-start corrected-quals]
  (let [quals (:quals-at-ref aln)
        start (.pos aln)
        len   (count corrected-quals)]
    (if (<= start correct-start)
      (-> quals
          (subvec 0 (- correct-start start))
          (into corrected-quals)
          (into (subvec quals (+ (- correct-start start) len))))
      (into corrected-quals (subvec quals len)))))

(defn- correct-pair-quals
  "Correct quals of a pair. Returns a map with corrected quals."
  [^SAMAlignment a1 ^SAMAlignment a2]
  (if-not (and (pos? (.pnext a1))
               (<= (max (.pos a1) (.pos a2)) (.end a1)))
    nil
    (let [tlen1 (.tlen a1)
          tlen2 (.tlen a2)
          quals1 (:quals-at-ref a1)
          quals2 (:quals-at-ref a2)
          seqs1  (:seqs-at-ref a1)
          seqs2  (:seqs-at-ref a2)
          correct-start (max (.pos a1) (.pos a2))
          new-quals (for [pos (range correct-start
                                     (inc (min (.end a1) (.end a2))))]
                      (let [relative-pos1 (- pos (.pos a1))
                            relative-pos2 (- pos (.pos a2))
                            q1 (quals1 relative-pos1)
                            q2 (quals2 relative-pos2)
                            [b1 _] (seqs1 relative-pos1)
                            [b2 _] (seqs2 relative-pos2)]
                         (if (= b1 b2)
                           [(min 200 (+ q1 q2)) 0]
                           (if (<= q2 q1)
                             [(int (* 0.8 q1)) 0]
                             [0 (int (* 0.8 q2))]))))
          [new1 new2] (apply map vector new-quals)]
      [(merge-corrected-quals a1 correct-start new1)
       (merge-corrected-quals a2 correct-start new2)])))

(defn- make-corrected-quals-map
  "Make a map which has corrected quals of all overlapping pairs."
  [alns]
  (->> alns
       (group-by (fn [x] (.qname ^SAMAlignment x)))
       (into {} (map
                 (fn [[qname xs]]
                   (if (<= (count xs) 1)
                     {}
                     {(keyword qname) (apply correct-pair-quals xs)}))))))

(defn- correct-quals-at-ref
  "Returns an alignment with corrected quals by looking up quals
  in the corrected quals map."
  [corrected-map ^SAMAlignment aln]
  (if-not (= (.rnext aln) "=")
    aln
    (if-let [quals-pair ((keyword (.qname aln)) corrected-map)]
      (let [new-quals (if (flag/r1? (.flag aln))
                        (first quals-pair)
                        (second quals-pair))]
        (assoc aln :quals-at-ref new-quals))
      aln)))

(defn- get-max-pnext
  "Get the maximum pnext of alignments."
  [alns]
  (->> alns
       (map (fn [^SAMAlignment aln]
              (if (= (.rnext aln) "=")
                (.pnext aln)
                0)))
       (apply max)))

(defn- read-chunk
  "Read a chunk which is specified by the region."
  [sam-reader min-map-quality region]
  (->> region
       (sam/read-alignments sam-reader)
       (sequence
         (comp
           (filter (basic-mpileup-pred min-map-quality))
           (map index-cigar)))))

(defn- get-chunk
  "Get a chunk in which all pairs exist.
  Returns the end position of the chunk and alignments in the chunk.
  It concatenates chunks when a pair overlap across multiple chunks."
  [sam-reader min-map-quality last-end {:keys [chr start end] :as region}]
  (loop [s    start
         e    end
         alns (read-chunk sam-reader min-map-quality region)]
    (if (or (empty? alns)                ;; empty chunk
            (= e last-end)               ;; this is the last chunk
            (<= (get-max-pnext alns) e)) ;; all pairs are in this chunk
      [e alns]
      (let [next-start (inc e)
            next-end   (min last-end (+ e (- e s) 1))]
        (recur
          next-start
          next-end
          (->> (read-chunk sam-reader
                           min-map-quality
                           {:chr   chr
                            :start next-start
                            :end   next-end})
               (concat alns)
               distinct))))))

(defn- pileup-chunk
  "Piles up alignments in a chunk position-by-position.
  Returns a lazy sequence of `cljam.io.pileup.LocusPile`s."
  [^long start ^long end ignore-overlaps? alns]
  (->> alns
       (sequence
         (comp
           (drop-while #(< (.end ^SAMAlignment %) start))
           (if ignore-overlaps?
             identity
             (keep (partial correct-quals-at-ref
                            (make-corrected-quals-map alns))))))
       (pileup-seq start end)))

(defn- pileup-chunks
  "Piles up alignments in each chunk."
  [sam-reader {:keys [chr start end]} chunk-size
   min-base-quality min-map-quality ignore-overlaps?]
  (loop [chunk-start start
         total-piles []]
    (if-not (<= chunk-start end)
      total-piles
      (let [tmp-chunk-end (min (+ chunk-start chunk-size) end)
            [chunk-end alns] (get-chunk sam-reader
                                        min-map-quality
                                        end
                                        {:chr chr
                                         :start chunk-start
                                         :end tmp-chunk-end})
            piles (->> alns
                       (pileup-chunk chunk-start chunk-end ignore-overlaps?)
                       (sequence
                         (comp
                           (map resolve-bases)
                           (if (pos? min-base-quality)
                             (map (filter-by-base-quality min-base-quality))
                             identity)
                           (keep (partial ->locus-pile chr)))))]
        (recur (inc chunk-end)
               (concat total-piles piles))))))

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
           e (min len end)
           region {:chr chr :start s :end e}]
       (pileup-chunks sam-reader
                      region
                      base-chunk-size
                      min-base-quality
                      min-map-quality
                      ignore-overlaps?)))))

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
