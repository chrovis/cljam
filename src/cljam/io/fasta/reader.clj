(ns cljam.io.fasta.reader
  (:refer-clojure :exclude [read read-line])
  (:require [cljam.util :refer [graph?]]
            [cljam.io.fasta.util :refer [header-line? parse-header-line]]
            [cljam.io.fasta-index.core :as fasta-index]
            [cljam.io.util.bgzf.gzi :as gzi])
  (:import [java.io Closeable RandomAccessFile InputStream EOFException]
           [java.nio Buffer ByteBuffer CharBuffer]
           [java.nio.channels FileChannel$MapMode]
           [bgzf4j BGZFInputStream]))

;; FASTAReader
;; -----------

(deftype FASTAReader [reader stream url index-delay]
  Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))
    (.close ^java.io.Closeable (.stream this))))

(defprotocol RandomAccessor
  (seek [this pos])
  (read-line [this])
  (read-buffer [this start end])
  (get-file-pointer [this]))

(extend-type RandomAccessFile
  RandomAccessor
  (seek [this pos]
    (.seek this pos))
  (read-line [this]
    (.readLine this))
  (read-buffer [this start end]
    (.. this
        getChannel
        (map FileChannel$MapMode/READ_ONLY start (- end start))))
  (get-file-pointer [this]
    (.getFilePointer this)))

(deftype IndexedBGZFInputStream [^BGZFInputStream is idx]
  Closeable
  (close [_]
    (.close is))
  RandomAccessor
  (seek [_ pos]
    (.seek is pos))
  (read-line [_]
    (.readLine is))
  (read-buffer [_ start end]
    (.seek is (gzi/uncomp->comp @idx start))
    (let [buf (byte-array (- end start))]
      (if (neg? (.read is buf))
        (throw (EOFException.))
        (ByteBuffer/wrap buf))))
  (get-file-pointer [_]
    (gzi/comp->uncomp @idx (.getFilePointer is))))

;; Reading
;; -------

(defn- read* [line rdr]
  (loop [line line
         ret {}]
    (if-not (nil? line)
      (if (= (first line) \>)
        (if (seq ret)
          (cons (assoc ret :len (count (filter (partial not= \space) (:seq ret))))
                (lazy-seq (read* line rdr)))
          (let [ref (subs line 1)
                offset (get-file-pointer rdr)]
            (recur (read-line rdr) (assoc ret :rname ref :offset offset))))
        (if (:rname ret)
          (let [ret' (if (:line-len ret)
                       (update-in ret [:seq] str line)
                       (assoc ret
                              :seq line
                              :line-len (inc (count line))
                              :line-blen (count (filter graph? line))))]
            (recur (read-line rdr) ret'))
          (throw (ex-info "Missing sequence name" {:line line}))))
      (cons (assoc ret :len (count (filter (partial not= \space) (:seq ret))))
            nil))))

(defn load-headers
  [rdr]
  (seek rdr 0)
  (loop [line (read-line rdr), headers []]
    (if line
      (if (header-line? line)
        (let [offset (get-file-pointer rdr)]
          (recur (read-line rdr) (conj headers (merge (parse-header-line line)
                                                      {:offset offset}))))
        (recur (read-line rdr) headers))
      headers)))

(defn- read-sequence*
  [^FASTAReader rdr name]
  (when-let [line (read-line (.reader rdr))]
    (if-not (header-line? line)
      (if name
        {:name name, :sequence line}
        (throw (ex-info "Missing sequence name" {})))
      (:name (parse-header-line line)))))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [^FASTAReader rdr]
  (seek (.reader rdr) 0)
  (letfn [(read-fn [rdr name]
            (let [s (read-sequence* rdr name)]
              (cond
                (string? s) (read-fn rdr s)
                (map? s) (cons s (lazy-seq (read-fn rdr name))))))]
    (read-fn rdr nil)))

(defn read-sequence
  [^FASTAReader rdr name start end {:keys [mask?]}]
  (let [fai @(.index-delay rdr)]
    (when-let [len (:len (fasta-index/get-header fai name))]
      (let [start' (max 1 (or start 1))
            end' (min len (or end len))]
        (when (<= start' end')
          (let [buf (CharBuffer/allocate (inc (- end' start')))]
            (when-let [[s e] (fasta-index/get-span fai name (dec start') end')]
              (let [bb ^ByteBuffer (read-buffer (.reader rdr) s e)]
                (if mask?
                  (while (.hasRemaining bb)
                    (let [c (unchecked-char (.get bb))]
                      (when-not (or (= \newline c) (= \return c))
                        (.put buf c))))
                  (while (.hasRemaining bb)
                    (let [c (unchecked-long (.get bb))]
                      (when-not (or (= 10 c) (= 13 c))
                        ;; toUpperCase works only for ASCII chars.
                        (.put buf (unchecked-char (bit-and c 0x5f))))))))
              (.flip ^Buffer buf)
              (.toString buf))))))))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r (.reader rdr)]
    (read* (read-line r) r)))

(defn reset
  [^FASTAReader rdr]
  (seek (.reader rdr) 0))

(definline create-ba [^ByteBuffer buffer]
  `(when (pos? (.position ~buffer))
     (let [ba# (byte-array (.position ~buffer))]
       (.clear ~(with-meta buffer {:tag `Buffer}))
       (.get ~buffer ba#)
       (.clear ~(with-meta buffer {:tag `Buffer}))
       ba#)))

(def ^:private ^:const gt-byte (byte \>))
(def ^:private ^:const newline-byte (byte \newline))

(defn- read-buffer!
  [^bytes buf ^long size buffers ^bytes byte-map]
  (let [{:keys [^ByteBuffer name-buf ^ByteBuffer seq-buf ^ByteBuffer rest-buf]} buffers]
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
        false))))

(defn- sequential-read1!
  [^InputStream stream buf buffers byte-map loaded-bytes]
  (let [{:keys [^ByteBuffer name-buf ^ByteBuffer seq-buf ^ByteBuffer rest-buf]} buffers
        read-preload? (atom (some? (seq loaded-bytes)))]
    (loop [new-ref? false]
      (if-not new-ref?
        (if @read-preload?
          (let [new-ref*? (read-buffer! loaded-bytes (count loaded-bytes) buffers byte-map)]
            (reset! read-preload? false)
            (recur new-ref*?))
          (let [n (.read stream buf)]
            (if (pos? n)
              (recur (read-buffer! buf n buffers byte-map))
              {:name (create-ba name-buf) :sequence (create-ba seq-buf) :rest-bytes (create-ba rest-buf) :eof? true})))
        {:name (create-ba name-buf) :sequence (create-ba seq-buf) :rest-bytes (create-ba rest-buf) :eof? false}))))

(defn- sequential-read!
  [stream buf buffers byte-map loaded-bytes eof?]
  (when (or (not eof?) (seq loaded-bytes))
    (lazy-seq
     (let [m (sequential-read1! stream buf buffers byte-map loaded-bytes)]
       (cons (select-keys m [:name :sequence])
             (sequential-read! stream buf buffers byte-map (:rest-bytes m) (:eof? m)))))))

(defn- sequential-read
  [stream page-size seq-buf-size byte-map]
  (let [buf (byte-array page-size)
        name-buf (ByteBuffer/allocate 1024)
        seq-buf (ByteBuffer/allocate seq-buf-size)
        rest-buf (ByteBuffer/allocate page-size)]
    (sequential-read! stream buf
                      {:name-buf name-buf :seq-buf seq-buf :rest-buf rest-buf}
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
