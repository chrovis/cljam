(ns cljam.io.fasta.reader
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as cstr]
            [cljam.util :refer [graph?]]
            [cljam.io.fasta.util :refer [header-line? parse-header-line]]
            [cljam.io.fasta-index.core :as fasta-index])
  (:import [java.io RandomAccessFile InputStream]
           [java.nio ByteBuffer]))

;; FASTAReader
;; -----------

(deftype FASTAReader [reader stream f index-delay]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))
    (.close ^java.io.Closeable (.stream this))))

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

(defn- read-sequence-with-offset
  [^FASTAReader rdr offset-start offset-end {:keys [mask?]}]
  (let [len (- offset-end offset-start)
        ba (byte-array len)
        r ^RandomAccessFile (.reader rdr)]
    (doto r
      (.seek offset-start)
      (.read ba 0 len))
    (-> (String. ba)
        (cstr/replace #"\n" "")
        ((if mask? identity cstr/upper-case)))))

(defn read-whole-sequence
  [^FASTAReader rdr name opts]
  (let [fai @(.index-delay rdr)
        header (fasta-index/get-header fai name)
        [offset-start offset-end] (fasta-index/get-span fai name 0 (:len header))]
    (read-sequence-with-offset rdr offset-start offset-end opts)))

(defn read-sequence
  [^FASTAReader rdr name start end opts]
  (let [fai @(.index-delay rdr)
        header (fasta-index/get-header fai name)]
    (when-let [[offset-start offset-end] (fasta-index/get-span fai name (dec start) end)]
      (->> (concat (repeat (max 0 (- 1 start)) \N)
                   (read-sequence-with-offset rdr offset-start offset-end opts)
                   (repeat (max 0 (- end (:len header))) \N))
           (apply str)))))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r ^RandomAccessFile (.reader rdr)]
    (read* (.readLine r) r)))

(defn reset
  [^FASTAReader rdr]
  (let [r ^RandomAccessFile (.reader rdr)]
    (.seek r 0)))

(definline create-ba [^ByteBuffer buffer]
  `(when (pos? (.position ~buffer))
       (let [ba# (byte-array (.position ~buffer))]
         (.clear ~buffer)
         (.get ~buffer ba#)
         (.clear ~buffer)
         ba#)))

(def ^:private ^:const gt-byte (byte \>))
(def ^:private ^:const newline-byte (byte \newline))

(defn- read-buffer!
  [^bytes buf size ^bytes byte-map ^ByteBuffer name-buf ^ByteBuffer seq-buf ^ByteBuffer rest-buf]
  (loop [i 0, name-line? false]
    (if (< i size)
      (let [b (aget buf i)]
        (cond
          (= b gt-byte) (if (pos? (.position seq-buf))
                          (do (.put rest-buf buf i (- size i))
                              true)
                          (recur (inc i) true))
          (= b newline-byte) (if name-line?
                               (recur (inc i) false)
                               (recur (inc i) name-line?))
          :else (do (if name-line?
                      (.put name-buf b)
                      (.put seq-buf (aget byte-map b)))
                    (recur (inc i) name-line?))))
      false)))

(defn- sequential-read1!
  [^InputStream stream buffers byte-map loaded-bytes]
  (let [{:keys [buf ^ByteBuffer name-buf ^ByteBuffer seq-buf ^ByteBuffer rest-buf]} buffers
        read-preload? (atom (some? (seq loaded-bytes)))]
    (loop [new-ref? false]
      (if-not new-ref?
        (if @read-preload?
          (let [new-ref*? (read-buffer! loaded-bytes (count loaded-bytes) byte-map name-buf seq-buf rest-buf)]
            (reset! read-preload? false)
            (recur new-ref*?))
          (let [n (.read stream buf)]
            (if (pos? n)
              (recur (read-buffer! buf n byte-map name-buf seq-buf rest-buf))
              {:name (create-ba name-buf) :sequence (create-ba seq-buf) :rest-bytes (create-ba rest-buf) :eof? true})))
        {:name (create-ba name-buf) :sequence (create-ba seq-buf) :rest-bytes (create-ba rest-buf) :eof? false}))))

(defn- sequential-read!
  [stream buffers byte-map loaded-bytes eof?]
  (when (or (not eof?) (seq loaded-bytes))
    (lazy-seq
     (let [m (sequential-read1! stream buffers byte-map loaded-bytes)]
       (cons (select-keys m [:name :sequence])
             (sequential-read! stream buffers byte-map (:rest-bytes m) (:eof? m)))))))

(defn- sequential-read
  [stream page-size seq-buf-size byte-map]
  (let [buf (byte-array page-size)
        name-buf (ByteBuffer/allocate 1024)
        seq-buf (ByteBuffer/allocate seq-buf-size)
        rest-buf (ByteBuffer/allocate page-size)]
    (sequential-read! stream
                      {:buf buf :name-buf name-buf :seq-buf seq-buf :rest-buf rest-buf}
                      byte-map (byte-array 0) false)))

(defn sequential-read-byte-array
  "Returns list of maps containing sequence as byte-array. Bases ACGTN are
  encoded as 1-5."
  [stream page-size seq-buf-size]
  (let [byte-map (byte-array (range 128))]
    (doseq [[i v] [[\a 1] [\A 1] [\c 2] [\C 2] [\g 3] [\G 3] [\t 4] [\T 4] [\n 5] [\N 5]]]
      (aset-byte byte-map (byte i) (byte v)))
    (sequential-read stream page-size seq-buf-size byte-map)))

(defn sequential-read-string
  "Returns list of maps containing sequence as upper-case string."
  [stream page-size seq-buf-size {:keys [mask?]}]
  (let [byte-map (byte-array (range 128))]
    (when-not mask?
      (doseq [[i v] [[\a \A] [\c \C] [\g \G] [\t \T] [\n \N]]]
        (aset-byte byte-map (byte i) (byte v))))
    (map (fn [{:keys [^bytes name ^bytes sequence]}]
           {:name (String. name) :sequence (String. sequence)})
         (sequential-read stream page-size seq-buf-size byte-map))))
