(ns cljam.util
  "General utilities."
  (:require [clojure.java.io :refer [file]]))

;; CPU info
;; --------

(def num-cores
  "The number of processors available to the Java virtual machine."
  (.availableProcessors (Runtime/getRuntime)))

;; Disk cache
;; ----------

(def temp-dir (let [dir-path (.getPath (file (System/getProperty "java.io.tmpdir") "cljam"))]
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

(defn string->bytes [^String s]
  (let [buf (byte-array (count s))]
    (.getBytes s 0 (count buf) buf 0)
    buf))

(defn ^String bytes->string [^bytes b]
  (String. b 0 (count b)))

(defn from-hex-digit [^Character c]
  (let [d (Character/digit c 16)]
    (when (= d -1)
      (throw (NumberFormatException. (str "Invalid hex digit: " c))))
    d))

(defn hex-string->bytes [s]
  {:pre [(even? (count s))]}
  (byte-array
   (map #(byte (bit-or (bit-shift-left (from-hex-digit (nth s (* % 2))) 4)
                       from-hex-digit (nth s (inc (* % 2)))))
        (range (count s)))))

(defn str->long [s]
  (if-not (nil? s)
    (try
      (let [[n _ _] (re-matches #"(|-|\+)(\d+)" s)]
        (Long. n))
      (catch Exception e
        nil))
    nil))

(defn str->int [s]
  (if-not (nil? s)
    (try
      (let [[n _ _] (re-matches #"(|-|\+)(\d+)" s)]
        (Integer. ^String n))
      (catch NumberFormatException e
        (str->long s))
      (catch Exception e
        nil))
    nil))

(defn graph?
  "Returns true if c is a visible character, false if not."
  [c]
  (<= 0x20 (byte c) 0x7E))

(defn space?
  "Returns true if c is a character that creates \"white space\" in displayed
  text."
  [c]
  (not (nil? (#{\space \tab \newline \formfeed \return (char 0x0b)} c))))

;; seq utils
;; ---------

(defn gen-vec
  ([n]
     (gen-vec n nil))
  ([n ini]
     (vec (repeat n ini))))

;; map utils
;; ---------

(defmacro swap
  [m k f]
  `(assoc ~m ~k (~f (get ~m ~k))))
