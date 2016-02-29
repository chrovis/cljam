(ns cljam.fastq
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.io.Writer))

(deftype FASTQReader [reader f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

(deftype FASTQWriter [writer f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

(defn ^FASTQReader reader [^String f]
  (let [p (.getAbsolutePath (io/file f))]
    (FASTQReader. (io/reader p) p)))

(defn ^FASTQWriter writer [^String f]
  (let [p (.getAbsolutePath (io/file f))]
    (FASTQWriter. (io/writer p :encoding "UTF-8") p)))

(defn- decode-fastq
  [[name-line seq-line plus-line qual-line]
   & {:keys [decode-quality] :or {decode-quality true}}]
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
   :post [(or (not decode-quality) (every? (fn [q] (<= 0 q 93)) (:quality %)))]}
  {:name (subs name-line 1)
   :sequence seq-line
   :quality (if decode-quality (map #(- (int %) 33) qual-line) qual-line)})

(defn read-sequence
  [^FASTQReader rdr & opts]
  (->> (.reader rdr)
       line-seq
       (map string/trim)
       (partition-all 4)
       (map #(apply decode-fastq % opts))))

(defn- ^String encode-fastq
  [{:keys [name sequence quality]}]
  {:pre [(not-empty name)
         (not-empty sequence)
         (not-empty quality)
         (= (count sequence) (count quality))
         (every? (fn [q] (<= 0 q 93)) quality)]}
  (-> [(str "@" name) sequence "+" (apply str (map #(char (+ % 33)) quality))]
      (interleave (repeat \newline))
      (as-> x (apply str x))))

(defn write-sequence
  [^FASTQWriter wtr sequence]
  (let [w ^java.io.Writer (.writer wtr)]
    (doseq [s sequence]
      (.write w (encode-fastq s)))))
