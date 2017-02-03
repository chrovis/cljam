(ns cljam.pileup.mpileup
  (:require [clojure.string :as cstr]
            [clojure.java.io :refer [writer]]
            [clojure.tools.logging :as logging]
            [me.raynes.fs :as fs]
            [cljam.util.sam-util :as sam-util]
            [cljam.sequence :as cseq]
            [cljam.io :as io]
            [cljam.fasta :as fa]
            [cljam.pileup.common :refer [window-width step center]]
            [cljam.pileup.pileup :refer [rpositions]]))

(defn- append-seq
  [op target current]
  (case op
    \M (apply conj current (map str (:seq target)))
    \I (if (seq current)
         (update-in current [(dec (count current))]
                    vector
                    (str "+" (:n target) (apply str (:seq target))))
         current)
    \D (apply conj current (map str (:seq target)))
    \N (apply conj current (map str (:seq target)))
    current))

(defn encode-seq [seq*]
  (loop [[f & r] (filter #(nil? (#{\P} (:op %))) seq*)
         ret     []
         op      nil
         tmp     {:n 0, :op nil, :seq nil}]
    (if (nil? f)
      (append-seq op tmp ret)
      (if (nil? op)
        (recur r ret (:op f) f)
        (if (= (:op f) op)
          (recur r ret (:op f) (-> tmp
                                   (update-in [:n] + (:n f))
                                   (assoc :op (:op f))
                                   (update-in [:seq] (partial apply conj) (:seq f))))
          (let [new-ret (append-seq op tmp ret)]
            (recur r new-ret (:op f) f)))))))

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

(defn- gen-mpileup
  "Compute mpileup info from piled-up reads and reference."
  [^String rname ^long locus ^Character reference reads]
  (let [seq (map (fn [{:keys [^long pos ^String seq ^String cigar encoded-seq] :as aln}]
                   (let [s (nth (or encoded-seq (encode-seq (cseq/parse seq cigar))) (- locus pos))]
                     (cond
                       (vector? s) (second s)
                       (= reference (first s)) "."
                       :else s))) reads)
        qual (map (fn [{:keys [^long pos ^String qual] :as aln}]
                    (if (= qual "*") \~ (nth qual (- locus pos) \~))) reads)]
    (MPileupElement. rname locus reference (count reads) seq qual reads)))

(defn pileup*
  "Internal pileup function independent from I/O."
  [refseq alns rname start end]
  (->> alns
       (sequence (map (fn [a] (assoc a :encoded-seq (encode-seq (cseq/parse (:seq a) (:cigar a)))))))
       (pileup-seq start end)
       (map
        (partial gen-mpileup rname)
        (range start (inc end))
        refseq)))

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
         (pileup* refseq alns rname s e)))
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
