(ns cljam.util
  "General utilities."
  (:require [clojure.java.io :refer [file] :as cio])
  (:import [java.net MalformedURLException URL]
           [java.nio.file Files FileVisitor FileVisitResult]
           [java.nio.file.attribute FileAttribute]
           [org.apache.commons.compress.compressors
            CompressorStreamFactory CompressorException]))

;; Disk cache
;; ----------

(defn create-temp-dir
  "Returns a created temporary directory with the given `prefix`."
  [prefix]
  (->> 0
       (make-array FileAttribute)
       (Files/createTempDirectory prefix)
       .toFile))

(defn delete-temp-dir!
  "Takes the temporary directory created by `create-temp-dir` and deletes the
  `dir` and the files in `dir`."
  [^java.io.File dir]
  (Files/walkFileTree
   (.toPath dir)
   (reify FileVisitor
     (visitFile [this# file# attrs#]
       (Files/deleteIfExists file#)
       FileVisitResult/CONTINUE)
     (visitFileFailed [this# file# exc#]
       FileVisitResult/CONTINUE)
     (preVisitDirectory [this# dir# attrs#]
       FileVisitResult/CONTINUE)
     (postVisitDirectory [this# dir# exc#]
       (Files/deleteIfExists dir#)
       FileVisitResult/CONTINUE))))

(defmacro with-temp-dir
  "bindings => [dir prefix ...]
  Creates a temporary directory with the given `prefix` and binds the given
  `dir` to it. Finally, deletes `dir` and the files in `dir`."
  [bindings & body]
  (assert (vector? bindings) "bindings must be a vector")
  (assert (even? (count bindings)) "bindings must have an even number of forms")
  (cond
    (zero? (count bindings)) `(do ~@body)
    (symbol? (bindings 0)) (let [[dir prefix] bindings]
                             `(let [~dir (create-temp-dir ~prefix)]
                                (try
                                  (with-temp-dir ~(subvec bindings 2) ~@body)
                                  (finally
                                    (delete-temp-dir! ~dir)))))
    :else (throw (IllegalArgumentException. "binding must be a symbol"))))

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
  (.getBytes s))

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


;; file utils
;; ---------

(defn ^URL as-url
  [x]
  (try
    (cio/as-url x)
    (catch MalformedURLException _
      (cio/as-url (cio/file x)))))

(defn basename
  [x]
  (when-let [url (as-url x)]
    (second (re-find #"([^/]+)\.(?=[^\./]+$)" (.getPath url)))))

(def ^:private compressor-map
  {:gzip CompressorStreamFactory/GZIP
   :bzip2 CompressorStreamFactory/BZIP2})

(defn ^java.io.InputStream compressor-input-stream
  "Returns a compressor input stream from f, autodetecting the compressor type
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
  "Returns a compressor output stream from f and a compressor type k. k must be
  selected from :gzip or :bzip2. Autodetects the compressor type from the
  extension of f if k is not passed. Returns java.io.BufferedOutputStream if the
  compressor type is not known. Should be used inside with-open to ensure the
  OutputStream is properly closed."
  ([f]
   (compressor-output-stream f (condp re-find (.getPath (as-url f))
                                 #"(?i)\.(gz|gzip)$" :gzip
                                 #"(?i)\.(bz2|bzip2)$" :bzip2
                                 nil)))
  ([f k]
   (let [os (cio/output-stream f)]
     (if-let [s (get compressor-map k)]
       (-> (CompressorStreamFactory.)
           (.createCompressorOutputStream s os))
       os))))
