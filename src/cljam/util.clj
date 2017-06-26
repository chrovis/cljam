(ns cljam.util
  "General utilities."
  (:require [clojure.java.io :refer [file] :as cio]
            [clojure.string :as cstr])
  (:import [org.apache.commons.compress.compressors
            CompressorStreamFactory CompressorException]))

;; Disk cache
;; ----------

(def temp-dir (let [system-tmp-dir-path (or (System/getenv "TMPDIR")
                                            (System/getProperty "java.io.tmpdir"))
                    dir-path (.getPath (file system-tmp-dir-path "cljam"))]
                (.mkdirs (file dir-path))
                dir-path))

;; byte array
;; ----------

(defn ubyte
  "Casts to byte avoiding an error about out of range for byte."
  [n]
  {:pre [(<= 0 n 255)]}
  (byte (if (< n 0x80) n (- n 0x100))))

;; string utils
;; ------------

(defn ^"[B" string->bytes [^String s]
  (let [buf (byte-array (count s))]
    (.getBytes s 0 (alength buf) buf 0)
    buf))

(defn ^String bytes->string [^bytes b]
  (String. b 0 (alength b)))

(defn graph?
  "Returns true if c is a visible character, false if not."
  [c]
  (<= 0x20 (byte c) 0x7E))

(defn space?
  "Returns true if c is a character that creates \"white space\" in displayed
  text."
  [c]
  (not (nil? (#{\space \tab \newline \formfeed \return (char 0x0b)} c))))

;; map utils
;; ---------

(defmacro swap
  [m k f]
  `(assoc ~m ~k (~f (get ~m ~k))))


;; file utils
;; ---------

(defn basename
  [path]
  (let [filename (.getName (file path))]
    (first (cstr/split filename #"\.(?=[^\.]+$)"))))

(def ^:private compressor-map
  {:gzip CompressorStreamFactory/GZIP
   :bzip2 CompressorStreamFactory/BZIP2})

(defn ^java.io.InputStream compressor-input-stream
  "Returns an compressor input stream from f, autodetecting the compressor type
  from the first few bytes of f. Returns java.io.BufferedInputStream if the
  compressor type is not known. Should be used inside with-open to ensure the
  InputStream is properly closed."
  [f]
  (let [is (cio/input-stream f)]
    (try
      (-> (CompressorStreamFactory. true)
          (.createCompressorInputStream is))
      (catch CompressorException _
        is))))

(defn ^java.io.OutputStream compressor-output-stream
  "Returns an compressor output stream from f and a compressor type k. k must be
  selected from :gzip or :bzip2. Autodetects the compressor type from the
  extension of f if k is not passed. Returns java.io.BufferedOutputStream if the
  compressor type is not known. Should be used inside with-open to ensure the
  OutputStream is properly closed."
  ([f]
   (compressor-output-stream f (condp re-find (.getName (file f))
                                 #"(?i)\.(gz|gzip)$" :gzip
                                 #"(?i)\.(bz2|bzip2)$" :bzip2
                                 nil)))
  ([f k]
   (let [os (cio/output-stream f)]
     (if-let [s (get compressor-map k)]
       (-> (CompressorStreamFactory.)
           (.createCompressorOutputStream s os))
       os))))

;; region utils
;; ---------

(defn divide-region
  "Divides a region [start end] into several chunks with maximum length 'step'.
  Returns a lazy sequence of vector."
  [start end step]
  (->> [(inc end)]
       (concat (range start (inc end) step))
       (partition 2 1)
       (map (fn [[s e]] [s (dec e)]))))

(defn divide-refs
  "Divides refs into several chunks with maximum length 'step'.
  Returns a lazy sequence of map containing {:chr :start :end}."
  [refs step]
  (mapcat
   (fn [{:keys [name len]}]
     (map (fn [[s e]] {:chr name :start s :end e})
          (divide-region 1 len step)))
   refs))
