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

(defn resolve-base [^long ref-pos ^SAMAlignment aln]
  (let [relative-pos (- ref-pos (.pos aln))
        qual ((:quals-at-ref aln) relative-pos)
        [base indel] ((:seqs-at-ref aln) relative-pos)]
    {:start? (zero? relative-pos)
     :mapq (when (zero? relative-pos) (.mapq aln))
     :base base
     :qual qual
     :reverse? (flag/reversed? (.flag aln))
     :end? (= ref-pos (.end aln))
     :insertion (when-not (number? indel) indel)
     :deletion (when (number? indel) indel)
     :alignment aln}))

(defn resolve-bases [[ref-pos alns]]
  [ref-pos (mapv (partial resolve-base ref-pos) alns)])

(defn- correct-qual
  "Correct quality of two overlapped mate reads by setting zero quality for one of the base."
  [{s1 :base q1 :qual :as r1} {s2 :base q2 :qual :as r2}]
  (if (= s1 s2)
    [(assoc r1 :qual (min 200 (+ q1 q2))) (assoc r2 :qual 0)]
    (if (<= q2 q1)
      [(assoc r1 :qual (int (* 0.8 q1))) (assoc r2 :qual 0)]
      [(assoc r1 :qual 0) (assoc r2 :qual (int (* 0.8 q2)))])))

(defn correct-overlapped-reads
  [xs]
  (if (<= (count xs) 1)
    xs
    (->> xs
         (group-by (fn [x] (.qname ^SAMAlignment (:alignment x))))
         (into [] (mapcat
                   (fn [[_ xs]]
                     (if (<= (count xs) 1) xs (apply correct-qual xs))))))))

(defn correct-overlaps
  [pile]
  (update pile 1 correct-overlapped-reads))

(defn filter-by-base-quality
  "Returns a predicate for filtering piled-up reads by base quality at its position."
  [min-base-quality]
  (fn [p] (update p 1 (partial filterv #(<= min-base-quality (:qual %))))))

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
             (comp (map resolve-bases)
                   (map correct-overlaps)
                   (map (filter-by-base-quality min-base-quality))
                   (keep (fn [[pos pile]]
                           (when (seq pile)
                             {:rname chr
                              :pos pos
                              :count (count pile)
                              :pile pile}))))))))))

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
