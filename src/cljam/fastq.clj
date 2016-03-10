(ns cljam.fastq
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.io.Writer
           [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.util.zip GZIPInputStream GZIPOutputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream BZip2CompressorOutputStream]))

(deftype FASTQReader [reader f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

(deftype FASTQWriter [writer f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

(defn ^FASTQReader reader [^String f]
  (let [file (io/file f)
        path (.getAbsolutePath file)
        basename (.getName file)]
    (-> (condp re-find basename
          #"\.(gz|GZ|gzip|GZIP)$"
          (-> (io/input-stream path) GZIPInputStream. InputStreamReader. BufferedReader.)
          #"\.(bz2|BZ2|bzip2|BZIP2)$"
          (-> (io/input-stream path) BZip2CompressorInputStream. InputStreamReader. BufferedReader.)
          (io/reader path))
        (FASTQReader. path))))

(defn ^FASTQWriter writer [^String f]
  (let [file (io/file f)
        path (.getAbsolutePath file)
        basename (.getName file)]
    (-> (condp re-find basename
          #"\.(gz|GZ|gzip|GZIP)$"
          (-> (io/output-stream path) GZIPOutputStream. OutputStreamWriter. BufferedWriter.)
          #"\.(bz2|BZ2|bzip2|BZIP2)$"
          (-> (io/output-stream path) BZip2CompressorOutputStream. OutputStreamWriter. BufferedWriter.)
          (io/writer path :encoding "UTF-8"))
        (FASTQWriter. path))))

(defn- decode-fastq
  [[name-line seq-line plus-line qual-line]
   & {:keys [decode-quality] :or {decode-quality :phred33}}]
  {:pre [(not-empty name-line)
         (not-empty seq-line)
         (not-empty plus-line)
         (not-empty qual-line)
         (= (first name-line) \@)
         (= (first plus-line) \+)
         (not-empty (rest name-line))
         (or (empty? (rest plus-line))
             (= (rest plus-line) (rest name-line)))
         (= (count seq-line) (count qual-line))]
   :post [(every? (fn [q] (case decode-quality
                            :phred33 (<= 0 q 93)
                            :phred64 (<= 0 q 62)
                            true))
                  (:quality %))]}
  {:name (subs name-line 1)
   :sequence seq-line
   :quality (case decode-quality
              :phred33 (map #(- (int %) 33) qual-line)
              :phred64 (map #(- (int %) 64) qual-line)
              qual-line)})

(defn read-sequence
  [^FASTQReader rdr & opts]
  (->> (.reader rdr)
       line-seq
       (map string/trim)
       (partition-all 4)
       (map #(apply decode-fastq % opts))))

(defn- ^String encode-fastq
  [{:keys [name sequence quality]}
   & {:keys [encode-quality] :or {encode-quality :phred33}}]
  {:pre [(not-empty name)
         (not-empty sequence)
         (not-empty quality)
         (= (count sequence) (count quality))
         (every? #(case encode-quality
                    :phred33 (<= 0 % 93)
                    :phred64 (<= 0 % 62)
                    true) quality)]}
  (-> [(str "@" name)
       sequence
       "+"
       (apply str (map #(case encode-quality
                          :phred33 (char (+ % 33))
                          :phred64 (char (+ % 64))
                          %)
                       quality))]
      (interleave (repeat \newline))
      (as-> x (apply str x))))

(defn write-sequence
  [^FASTQWriter wtr sequence & opts]
  (let [w ^java.io.Writer (.writer wtr)]
    (doseq [s sequence]
      (.write w ^String (apply encode-fastq s opts)))))
