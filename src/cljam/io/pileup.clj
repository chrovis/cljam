(ns cljam.io.pileup
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sequence :as cseq]
            [proton.core :as p])
  (:import [java.io Closeable BufferedWriter])
  (:refer-clojure :exclude [ref]))

;; Records
;; -------

(defrecord PileupBase [^boolean start?
                       mapq ;; byte?
                       ^char base
                       ^short qual
                       ^boolean reverse?
                       ^boolean end?
                       insertion ;; String?
                       deletion ;; int?
                       qname ;; String?
                       alignment]) ;; SAMAlignment?

(defrecord LocusPile [rname
                      ^int pos
                      pile])

;; Reader
;; ------

(deftype PileupReader [reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(defn reader
  "Returns an open instance of cljam.io.pileup.PileupReader of f. Should be used
  inside with-open to ensure the reader is properly closed."
  ^PileupReader
  [f]
  (PileupReader. (cio/reader f)))

(def ^:private
  upper-table
  (let [ba (byte-array 128)]
    (doseq [c "ATGCN"]
      (aset-byte ba (byte (int c)) (byte (int c)))
      (aset-byte ba
                 (+ (byte (int c))
                    (- (byte (int \a))
                       (byte (int \A))))
                 (byte (int c))))
    (doseq [[from to] [[\, -1] [\. -1] [\< \>] [\> \>] [\* \*]]]
      (aset-byte ba (byte (int from)) (byte (int to))))
    ba))

(defn- parse-bases-col
  [ref-base ^String column]
  ;; TODO: Too slow and unreadable
  (let [len (.length column)]
    (loop [i 0
           results (transient [])]
      (if (< i len)
        (let [c (.charAt column i)
              start? (= \^ c)
              mapq (when start? (qual/fastq-char->phred-byte (.charAt column (inc i))))
              base (if mapq (.charAt column (+ i 2)) c)
              base-int (int base)
              reverse? (or (= base-int 60)
                           (= base-int 44)
                           (<= 97 base-int))
              upper-base-int (aget ^bytes upper-table base-int)
              upper-base (if (neg? upper-base-int) ref-base (char upper-base-int))]
          (when (zero? upper-base-int)
            (throw (ex-info (format "Invalid character %s in %s" base column) {:column column :base base})))
          (if (= len (+ i (if mapq 3 1)))
            (persistent! (conj! results (PileupBase. start? mapq upper-base -1 reverse? false nil nil nil nil)))
            (let [x (.charAt column (+ i (if mapq 3 1)))
                  ins? (= x \+)
                  del? (= x \-)
                  [indel-num-chars indel-num] (when (or ins? del?)
                                                (loop [j (+ i (if mapq 4 2))
                                                       k 0
                                                       results 0]
                                                  (let [c (.charAt column j)]
                                                    (if (Character/isDigit c)
                                                      (recur (inc j) (inc k) (+ (* 10 results) (- (int c) 48)))
                                                      [k results]))))
                  indel-seq (when indel-num
                              (let [s (+ i (if mapq 4 2) (long indel-num-chars))]
                                (cstr/upper-case (.substring column s (+ s (long indel-num))))))
                  end? (boolean (or (= \$ x)
                                    (and indel-num
                                         (not= len (+ i (if mapq 4 2) (long indel-num-chars) (long indel-num)))
                                         (= \$ (.charAt column (+ i (if mapq 4 2) (long indel-num-chars) (long indel-num)))))))
                  next-pos (long (+ i 1 (if mapq 2 0)
                                    (if indel-num (+ 1 (long indel-num-chars) (long indel-num)) 0)
                                    (if end? 1 0)))]
              (recur next-pos
                     (conj! results (PileupBase. start? mapq upper-base -1 reverse? end? (when ins? indel-seq) (when del? indel-num) nil nil))))))
        (persistent! results)))))

(defn- parse-pileup-line
  [line]
  (let [[rname pos [ref-base] cnt bases' quals & _] (cstr/split line #"\t")
        ;; TODO: handle extra columns like mapq, qname...
        upper-ref-base (Character/toUpperCase ^char ref-base)
        pile (mapv (fn [b q] (assoc b :qual q))
                   (some->> bases' (parse-bases-col upper-ref-base))
                   (some-> quals qual/fastq->phred))]
    (-> (LocusPile. rname (p/as-long pos) pile)
        (assoc :count (p/as-int cnt) :ref ref-base))))

(defn read-piles
  "Reads piled-up bases of the pileup file, returning them as a lazy sequence of
  cljam.io.pileup.LocusPile."
  [^PileupReader pileup-reader]
  (->> pileup-reader
       .reader
       line-seq
       (map parse-pileup-line)))

;; Writer
;; ------

(defn- write-mpileup-alignment!
  [^BufferedWriter w ref-reader rname ref-pos ref'
   {:keys [base reverse? mapq start? end? insertion deletion]}]
  (let [case-base-fn (if-not reverse?
                       identity
                       (fn [c]
                         (if (= c \>)
                           \<
                           (Character/toLowerCase ^char c))))
        case-fn (if-not reverse? identity cstr/lower-case)]
    (when start?
      (.append w \^)
      (.append w (qual/phred-byte->fastq-char mapq)))
    (if (= base ref') ;; match / deleted / skipped
      (.append w (if reverse? \, \.))
      (.append w (unchecked-char (case-base-fn base))))
    (when deletion
      (.append w \-)
      (.append w (String/valueOf (int deletion)))
      (.append w ^String (case-fn
                          (if ref-reader
                            (->> {:chr rname
                                  :start (inc (long ref-pos))
                                  :end (+ (long ref-pos) (int deletion))}
                                 (cseq/read-sequence ref-reader))
                            (apply str (repeat deletion \N))))))
    (when insertion
      (.append w \+)
      (.append w (String/valueOf (count insertion)))
      (.append w ^String (case-fn insertion)))
    (when end?
      (.append w \$))))

(defn- write-mpileup-line!
  [^BufferedWriter w ref-reader {:keys [rname pos pile]}]
  (let [ref-base (some-> ref-reader
                         (cseq/read-sequence
                          {:chr rname :start pos :end pos}
                          {:mask? true}))
        ref-char (some-> ref-base cstr/upper-case first)]
    (.write w ^String rname)
    (.append w \tab)
    (.write w (String/valueOf (long pos)))
    (.append w \tab)
    (.write w (or ^String ref-base "N"))
    (.append w \tab)
    (.write w (String/valueOf (count pile)))
    (.append w \tab)
    (doseq [p pile]
      (write-mpileup-alignment! w ref-reader rname pos ref-char p))
    (.append w \tab)
    (doseq [p pile]
      (.append w (qual/phred-byte->fastq-char (:qual p))))))

(deftype PileupWriter [writer ref-reader]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))
    (some-> ^Closeable (.ref-reader this) .close)))

(defn writer
  "Returns an open instance of cljam.io.pileup.PileupWriter of f. Should be used
  inside with-open to ensure the writer is properly closed. A reference sequence
  is used when the second arg `reference-path` is supplied."
  (^PileupWriter [f]
   (writer f nil))
  (^PileupWriter [f reference-path]
   (PileupWriter. (cio/writer f) (some-> reference-path cseq/reader))))

(defn write-piles
  "Writes piled-up bases to a file. `pileup-writer` must be an instance of
  cljam.io.pileup.PileupWriter."
  [^PileupWriter pileup-writer piles]
  (let [^BufferedWriter wtr (.writer pileup-writer)
        ref-reader (.ref-reader pileup-writer)]
    (doseq [pile piles]
      (write-mpileup-line! wtr ref-reader pile)
      (.newLine wtr))
    (.flush wtr)))
