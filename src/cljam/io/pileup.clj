(ns cljam.io.pileup
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.io.sam.util.flag :as flag]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sequence :as cseq]
            [proton.core :as p])
  (:import [java.io Closeable BufferedWriter]))

;; Reader
;; ------

(deftype PileupReader [reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(defn ^PileupReader reader
  [in]
  (PileupReader. (cio/reader in)))

(defrecord PileupBase [^boolean start?
                       mapq
                       ^char base
                       ^byte qual
                       ^boolean reverse?
                       ^boolean end?
                       insertion
                       deletion])

(def ^:private
  upper-table
  (let [ba (byte-array 128)]
    (doseq [c "ATGCN"]
      (aset-byte ba (byte c) (byte c))
      (aset-byte ba (+ (byte c) (- (byte \a) (byte \A))) (byte c)))
    (doseq [[from to] [[\, -1] [\. -1] [\< \>] [\> \>] [\* \*]]]
      (aset-byte ba (byte from) (byte to)))
    ba))

(defn parse-bases-col
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
            (persistent! (conj! results (PileupBase. start? mapq upper-base -1 reverse? false nil nil)))
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
                              (let [s (+ i (if mapq 4 2) indel-num-chars)]
                                (cstr/upper-case (.substring column s (+ s indel-num)))))
                  end? (boolean (or (= \$ x)
                                    (and indel-num
                                         (not= len (+ i (if mapq 4 2) indel-num-chars indel-num))
                                         (= \$ (.charAt column (+ i (if mapq 4 2) indel-num-chars indel-num))))))
                  next-pos (long (+ i 1 (if mapq 2 0)
                                    (if indel-num (+ 1 indel-num-chars indel-num) 0)
                                    (if end? 1 0)))]
              (recur next-pos
                     (conj! results (PileupBase. start? mapq upper-base -1 reverse? end? (when ins? indel-seq) (when del? indel-num)))))))
        (persistent! results)))))

(defn parse-pileup-line
  [line]
  (let [[rname pos [ref-base] cnt bases quals & _] (cstr/split line #"\t") ;; TODO: extra columns
        upper-ref-base (Character/toUpperCase ^char ref-base)]
    {:rname rname
     :pos (p/as-int pos)
     :ref ref-base
     :count (p/as-long cnt)
     :pile (mapv (fn [b q] (assoc b :qual q)) (some->> bases (parse-bases-col upper-ref-base)) (some-> quals qual/fastq->phred))}))

(defn read-piles
  [^PileupReader reader]
  (->> reader
       .reader
       line-seq
       (map parse-pileup-line)))


;; Writer
;; ------

(defn ^String stringify-mpileup-alignment
  ([ref-reader rname ref-pos ref pileup-base]
   (stringify-mpileup-alignment (StringBuilder.) ref-reader rname ref-pos ref pileup-base))
  ([^StringBuilder sb ref-reader rname ref-pos ref {:keys [base qual reverse? mapq start? end? insertion deletion]}]
   (let [case-base-fn (if-not reverse?
                        identity
                        (fn [c]
                          (if (= c \>)
                            \<
                            (Character/toLowerCase ^char c))))
         case-fn (if-not reverse? identity cstr/lower-case)]
     (.setLength sb 0)
     (when start?
       (.append sb \^)
       (.append sb (qual/phred-byte->fastq-char mapq)))
     (if (= base ref) ;; match / deleted / skipped
       (.append sb (if reverse? \, \.))
       (.append sb (case-base-fn base)))
     (when deletion
       (do (.append sb \-)
           (.append sb deletion)
           (.append sb (case-fn
                        (if ref-reader
                          (->> {:chr rname
                                :start (inc ref-pos)
                                :end (+ ref-pos deletion)}
                               (cseq/read-sequence ref-reader))
                          (apply str (repeat deletion \N)))))))
     (when insertion
       (do (.append sb \+)
           (.append sb (count insertion))
           (.append sb (case-fn insertion))))
     (when end?
       (.append sb \$))
     (str sb))))

(defn ^String stringify-mpileup-line
  ([ref-reader pile]
   (stringify-mpileup-line (StringBuilder.) ref-reader pile))
  ([sb ref-reader {:keys [rname pos pile]}]
   (let [ref-base (some-> ref-reader
                          (cseq/read-sequence
                           {:chr rname :start pos :end pos}
                           {:mask? true}))
         ref-char (some-> ref-base cstr/upper-case first)
         base-fn (partial stringify-mpileup-alignment sb ref-reader rname pos ref-char)
         bases (cstr/join (map base-fn pile))
         quals (cstr/join (map (comp qual/phred-byte->fastq-char :qual) pile))]
     (cstr/join \tab [rname pos (or ref-base "N") (count pile) bases quals]))))

(deftype PileupWriter [writer ref-reader]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))
    (some-> ^Closeable (.ref-reader this) .close)))

(defn ^PileupWriter writer
  ([out]
   (writer out nil))
  ([out reference]
   (PileupWriter. (cio/writer out) (some-> reference cseq/reader))))

(defn write-piles
  [^PileupWriter pileup-writer piles]
  (let [^BufferedWriter writer (.writer pileup-writer)
        ref-reader (.ref-reader pileup-writer)]
    (doseq [pile piles]
      (.write writer (stringify-mpileup-line ref-reader pile))
      (.newLine writer))
    (.flush writer)))
