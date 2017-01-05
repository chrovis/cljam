(ns cljam.fasta.reader
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.string :as cstr]
            [cljam.util :refer [graph?]]
            [cljam.util.fasta :refer [header-line? parse-header-line]]
            [cljam.fasta-index.core :as fasta-index])
  (:import [java.io RandomAccessFile InputStream]))

;; FASTAReader
;; -----------

(deftype FASTAReader [reader f index]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

;; Reading
;; -------

(defn- read* [line ^RandomAccessFile rdr]
  (loop [line line
         ret {}]
    (if-not (nil? line)
      (if (= (first line) \>)
        (if (seq ret)
          (cons (assoc ret :len (count (filter (partial not= \space) (:seq ret))))
                (lazy-seq (read* line rdr)))
          (let [ref (subs line 1)
                offset (.getFilePointer rdr)]
            (recur (.readLine rdr) (assoc ret :rname ref :offset offset))))
        (let [ret' (if (:line-len ret)
                     (update-in ret [:seq] str line)
                     (assoc ret
                       :seq line
                       :line-len (inc (count line))
                       :line-blen (count (filter graph? line))))]
          (recur (.readLine rdr) ret')))
      (cons (assoc ret :len (count (filter (partial not= \space) (:seq ret))))
            nil))))

(defn load-headers
  [^RandomAccessFile rdr]
  (.seek rdr 0)
  (loop [line (.readLine rdr), headers []]
    (if line
      (if (header-line? line)
        (let [offset (.getFilePointer rdr)]
          (recur (.readLine rdr) (conj headers (merge (parse-header-line line)
                                                      {:offset offset}))))
        (recur (.readLine rdr) headers))
      headers)))

(defn- read-sequence*
  [^FASTAReader rdr name]
  (let [reader ^RandomAccessFile (.reader rdr)
        line (.readLine reader)]
    (if line
      (if-not (header-line? line)
        {:name name, :sequence line}
        (:name (parse-header-line line))))))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [^FASTAReader rdr]
  (.seek ^RandomAccessFile (.reader rdr) 0)
  (let [read-fn (fn read-fn* [^FASTAReader rdr name]
                  (let [s (read-sequence* rdr name)]
                    (cond
                     (string? s) (read-fn* rdr s)
                     (map? s) (cons s (lazy-seq (read-fn* rdr name))))))]
    (read-fn rdr nil)))

(def ^:private newline-character (char 10))
(def ^:private return-character (char 13))

(defn- newline*?
  [c]
  (or (= c newline-character)
      (= c return-character)))

(def newline? (memoize newline*?))

(defn- read-sequence-with-offset
  [^FASTAReader rdr offset-start offset-end]
  (let [len (- offset-end offset-start)
        ba (byte-array len)
        r ^RandomAccessFile (.reader rdr)]
    (doto r
      (.seek offset-start)
      (.read ba 0 len))
    (-> (String. ba)
        (cstr/replace #"\n" "")
        cstr/upper-case)))

(defn read-whole-sequence
  [^FASTAReader rdr name]
  (when-let [fai (.index rdr)]
    (let [header (fasta-index/get-header fai name)
          [offset-start offset-end] (fasta-index/get-span fai name 0 (:len header))]
      (read-sequence-with-offset rdr offset-start offset-end))))

(defn read-sequence
  [^FASTAReader rdr name start end]
  (when-let [fai (.index rdr)]
    (let [header (fasta-index/get-header fai name)]
      (when-let [[offset-start offset-end] (fasta-index/get-span fai name (dec start) end)]
        (->> (concat (repeat (max 0 (- 1 start)) \N)
                       (read-sequence-with-offset rdr offset-start offset-end)
                       (repeat (max 0 (- end (:len header))) \N))
             (apply str))))))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r ^RandomAccessFile (.reader rdr)]
    (read* (.readLine r) r)))

(defn reset
  [^FASTAReader rdr]
  (let [r ^RandomAccessFile (.reader rdr)]
    (.seek r 0)))

(definline create-ba [^java.nio.ByteBuffer buffer]
  `(when (pos? (.position ~buffer))
       (let [ba# (byte-array (.position ~buffer))]
         (.clear ~buffer)
         (.get ~buffer ba#)
         (.clear ~buffer)
         ba#)))

(defn- sequqntial-read*
  "Core function to read FASTA sequentially.
   Function f is called every time a single sequence finishes reading.
   When finished reading entire file, f is called with nil."
  [^InputStream stream page-size seq-buf-size ^bytes byte-map f]
  (let [buf (byte-array page-size)
        n-buf (java.nio.ByteBuffer/allocate 1024)
        s-buf (java.nio.ByteBuffer/allocate seq-buf-size)]
    (loop [rname* nil]
      (let [bytes (.read stream buf)]
        (if (pos? bytes)
          (recur
           (loop [i 0 name-line? false rname rname*]
             (if (< i bytes)
               (let [b (aget buf i)]
                 (if (= b 62) ; \>
                   (do (when-let [s (create-ba s-buf)] (f {:name rname :sequence s}))
                       (recur (inc i) true rname))
                   (if (= b 10) ; \newline
                     (if name-line?
                       (recur (inc i) false (create-ba n-buf))
                       (recur (inc i) name-line? rname))
                     (do (if name-line?
                           (.put n-buf b)
                           (.put s-buf (aget byte-map b)))
                         (recur (inc i) name-line? rname)))))
               rname)))
          (f {:name rname* :sequence (create-ba s-buf)}))))
    (f nil)))

(defn sequential-read-byte-array
  "Returns list of maps containing sequence as byte-array.
   Bases ACGTN are encoded as 1~5"
  [^InputStream stream page-size seq-buf-size]
  (let [s (atom [])
        byte-map (byte-array (range 128))]
    (doseq [[i v] [[\a 1] [\A 1] [\c 2] [\C 2] [\g 3] [\G 3] [\t 4] [\T 4] [\n 5] [\N 5]]]
      (aset-byte byte-map (byte i) (byte v)))
    (sequqntial-read* stream page-size seq-buf-size byte-map #(when % (swap! s conj %)))
    @s))

(defn sequential-read-string
  "Returns list of maps containing sequence as upper-case string."
  [^InputStream stream page-size seq-buf-size]
  (let [s (atom [])
        byte-map (byte-array (range 128))
        handler (fn [{:keys [name sequence]}]
                  (when (and name sequence)
                    (swap! s conj {:name (String. ^bytes name) :sequence (String. ^bytes sequence)})))]
    (doseq [[i v] [[\a \A] [\c \C] [\g \G] [\t \T] [\n \N]]]
      (aset-byte byte-map (byte i) (byte v)))
    (sequqntial-read* stream page-size seq-buf-size byte-map handler)
    @s))
