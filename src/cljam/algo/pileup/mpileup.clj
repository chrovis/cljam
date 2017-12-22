(ns cljam.algo.pileup.mpileup
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [clojure.tools.logging :as logging]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.sam.util.flag :as flag]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sequence :as cseq]))

(defn to-mpileup
  "Stringify mpileup sequence."
  [x]
  (if (vector? x)
    (let [[y op xs] x] (apply str y op (count xs) xs))
    (str x)))

(defn substitute-seq
  "Substitute sequence with mpileup index."
  [[^Character r & refs] ^String s [op x xs]]
  (letfn [(get-char [i] (if (number? i) (let [c (.charAt s i)] (if (= c r) \. c)) i))]
    (case op
      :m (get-char x)
      :d [(get-char x) \- (take (count xs) refs)]
      :i [(get-char x) \+ (map #(.charAt s %) xs)]
      x)))

(defn substitute-qual
  "Substitute base quality with mpileup index."
  [^String q [op x]]
  (if (and (number? x) (not= q "*")) (.charAt q x) \~))

(defn pileup-seq
  "Returns a lazy sequence that each element contains reads piled-up at the locus."
  [^long start ^long end reads]
  (letfn [(cover-locus? [pos aln]
            (<= (:pos aln) pos (:end aln)))
          (step [pos buf alns]
            (lazy-seq
             (when (< pos end)
               (let [[i o] (split-with (partial cover-locus? (inc pos)) alns)
                     b (doall (concat (filter #(<= (inc pos) (:end %)) buf) i))]
                 (cons b (step (inc pos) b o))))))]
    (->> reads
         (sequence
          (comp
           (remove #(empty? (:cigar %)))
           (drop-while #(< (:end %) start))))
         (step (dec start) []))))

(defrecord MPileupElement [^String rname ^long pos ^Character ref pile])

(defn gen-mpileup
  "Compute mpileup info from piled-up reads and reference."
  [^String rname ^long locus [^Character ref-base :as refs] reads]
  (->> reads
       (map (fn gen-mpileup-seq [{:keys [^long pos seq qual cig-index] :as aln}]
              {:qual (- (int (substitute-qual qual (nth cig-index (- locus pos)))) 33)
               :seq (substitute-seq refs seq (nth cig-index (- locus pos)))
               :read aln}))
       (MPileupElement. rname locus ref-base)))

(defn pileup*
  "Internal mpileup function independent from I/O.
   Can take multiple alignments seqs."
  [refseq {:keys [chr start end]} & aln-seqs]
  (->> aln-seqs
       (map
        (fn [alns]
          (->> alns
               (map (fn [a] (assoc a :cig-index (cigar/to-index (:cigar a)))))
               (pileup-seq start end))))
       (apply map
              (fn [index refs & plps]
                (map (fn [plp] (gen-mpileup chr index refs plp)) plps))
              (range start (inc end))
              (partition 100 1 (concat refseq (repeat \N))))))

(defn basic-mpileup-pred
  "Basic predicate function for filtering alignments for mpileup."
  [{:keys [flag]}]
  (and flag
       (zero? (bit-and (flag/encoded #{:unmapped :filtered-out :duplicated :supplementary :secondary}) flag))
       (or (not (flag/multiple? flag)) (flag/properly-aligned? flag))))

(defn- correct-qual
  "Correct quality of two overlapped mate reads by setting zero quality for one of the base."
  [{s1 :seq q1 :qual :as r1} {s2 :seq q2 :qual :as r2}]
  (if (= s1 s2)
    [(assoc r1 :qual (min 200 (+ q1 q2))) (assoc r2 :qual 0)]
    (if (<= q2 q1)
      [(assoc r1 :qual (int (* 0.8 q1))) (assoc r2 :qual 0)]
      [(assoc r1 :qual 0) (assoc r2 :qual (int (* 0.8 q2)))])))

(defn correct-overlapped-reads
  "Correct quality of two overlapped mate reads in piled-up reads."
  [pile]
  (if (<= (count (:pile pile)) 1)
    pile
    (->> (:pile pile)
         (group-by (comp :qname :read))
         (mapcat
          (fn [[qname xs]]
            (if (<= (count xs) 1) xs (apply correct-qual xs))))
         (assoc pile :pile))))

(defn filter-by-base-quality
  "Returns a predicate for filtering piled-up reads by base quality at its position."
  [min-base-quality]
  (fn [p] (update p :pile (fn [pp] (filter #(<= min-base-quality (:qual %)) pp)))))

(defn transpose-pile
  "Converts a pile {:pile [{:seq SEQ, :qual QUAL, :read READ}]}
  into {:seq [], :qual [], :reads []}."
  [p]
  (if (zero? (count (:pile p)))
    (assoc p :seq [] :qual [] :reads [] :count 0)
    (->> (:pile p)
         (map (juxt :seq #(char (+ (:qual %) 33)) :read))
         (apply map vector)
         (zipmap [:seq :qual :reads])
         (merge p {:count (count (:pile p))}))))

(defn pileup
  "Returns a lazy sequence of MPileupElement calculated from FASTA and BAM."
  ([bam-reader region]
   (pileup nil bam-reader region))
  ([ref-reader bam-reader {:keys [chr start end] :or {start -1 end -1}}]
   (try
     (if-let [r (refs/ref-by-name (sam/read-refs bam-reader) chr)]
       (let [s (if (neg? start) 1 start)
             e (if (neg? end) (:len r) end)
             refseq (if ref-reader
                      (cseq/read-sequence ref-reader {:chr chr :start s :end e})
                      (repeat \N))]
         (->> (sam/read-alignments bam-reader {:chr chr :start s :end e})
              (sequence
               (comp
                (filter basic-mpileup-pred)
                (filter (fn [a] (<= 0 (:mapq a))))))
              (pileup* refseq {:chr chr :start s :end e})
              (sequence
               (comp (map first)
                     (map correct-overlapped-reads)
                     (map (filter-by-base-quality 13))
                     (map transpose-pile)
                     (map (fn [p] (update p :seq (fn [s] (map to-mpileup s))))))))))
     (catch bgzf4j.BGZFException _
       (throw (RuntimeException. "Invalid file format"))))))

;; Writing
;; -------

(defn- write-line!
  [^java.io.BufferedWriter w line]
  (.write w (cstr/join \tab [(:rname line)
                             (:pos line)
                             (:ref line)
                             (:count line)
                             (cstr/join (:seq line))
                             (cstr/join (:qual line))]))
  (.newLine w))

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  [f fa-rdr bam-reader]
  (try
    (with-open [w (cio/writer f)]
      (doseq [rname (map :name (sam/read-refs bam-reader))]
        (doseq [line (pileup fa-rdr bam-reader {:chr rname})]
          (when-not (zero? (:count line))
            (write-line! w line)))))
    (catch Exception e (do
                         (cio/delete-file f)
                         (logging/error "Failed to create mpileup")
                         (throw e)))))
