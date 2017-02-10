(ns cljam.pileup.mpileup
  (:require [clojure.string :as cstr]
            [clojure.java.io :refer [writer]]
            [clojure.tools.logging :as logging]
            [me.raynes.fs :as fs]
            [cljam.util.sam-util :as sam-util]
            [cljam.sequence :as cseq]
            [cljam.io :as io]
            [cljam.fasta :as fa]
            [cljam.cigar :as cig]
            [cljam.pileup.common :refer [window-width step center]]
            [cljam.pileup.pileup :refer [rpositions]]))

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
           (remove #(and (empty? (:cigar %)) (nil? (:cigar-bytes (:meta %)))))
           (map #(assoc % :end (sam-util/get-end %)))
           (drop-while #(< (:end %) start))))
         (step (dec start) []))))

(defrecord MPileupElement [^String rname ^long pos ^Character ref ^long count seq qual reads])

(defn gen-mpileup
  "Compute mpileup info from piled-up reads and reference."
  [^String rname ^long locus [^Character ref-base :as refs] reads]
  (let [seqs (map (fn gen-mpileup-seq [{:keys [^long pos ^String seq cig-index]}]
                   (substitute-seq refs seq (nth cig-index (- locus pos)))) reads)
        qual (map (fn gen-mpileup-qual [{:keys [^long pos ^String qual cig-index]}]
                    (substitute-qual qual (nth cig-index (- locus pos)))) reads)]
    (MPileupElement. rname locus ref-base (count reads) seqs qual reads)))

(defn pileup*
  "Internal mpileup function independent from I/O.
   Can take multiple alignments seqs."
  [refseq rname start end & aln-seqs]
  (->> aln-seqs
       (map
        (fn [alns]
          (->> alns
               (sequence (map (fn [a] (assoc a :cig-index (cig/to-index (:cigar a))))))
               (pileup-seq start end))))
       (apply map
              (fn [index refs & plps]
                (map (fn [plp] (gen-mpileup rname index refs plp)) plps))
              (range start (inc end))
              (partition 10 1 (concat refseq (repeat \N))))))

(defn pileup
  "Returns a lazy sequence of MPileupElement calculated from FASTA and BAM."
  ([bam-reader rname]
   (pileup nil bam-reader rname -1 -1))
  ([fa-reader bam-reader rname]
   (pileup fa-reader bam-reader rname -1 -1))
  ([fa-reader bam-reader rname start end]
   (try
     (if-let [r (sam-util/ref-by-name (io/read-refs bam-reader) rname)]
       (let [s (if (neg? start) 1 start)
             e (if (neg? end) (:len r) end)
             refseq (if fa-reader
                      (fa/read-sequence fa-reader {:chr rname :start s :end e})
                      (repeat \N))
             alns (io/read-alignments bam-reader {:chr rname :start s :end e :depth :deep})]
         (map (fn [p] (update (first p) :seq (fn [s] (map to-mpileup s)))) (pileup* refseq rname s e alns))))
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
    (with-open [w (writer f)]
      (doseq [rname (map :name (io/read-refs bam-reader))]
        (doseq [line (pileup fa-rdr bam-reader rname)]
          (when-not (zero? (:count line))
            (write-line! w line)))))
    (catch Exception e (do
                         (fs/delete f)
                         (logging/error "Failed to create mpileup")
                         (throw e)))))
