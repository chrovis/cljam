(ns cljam.io.util.bgzf
  (:refer-clojure :exclude [compare])
  (:require [clojure.java.io :as cio])
  (:import [java.io File]
           [java.net MalformedURLException URI URL]
           [bgzf4j BGZFInputStream BGZFOutputStream]))

(defprotocol BGZFIOFactory
  (make-bgzf-input-stream [x])
  (make-bgzf-output-stream [x]))

(extend-protocol BGZFIOFactory
  File
  (make-bgzf-input-stream [^File x]
    (BGZFInputStream. x))
  (make-bgzf-output-stream [^File x]
    (BGZFOutputStream. x))

  URL
  (make-bgzf-input-stream [^URL x]
    (if (= (.getProtocol x) "file")
      (make-bgzf-input-stream (cio/as-file x))
      (BGZFInputStream. x)))
  (make-bgzf-output-stream [^URL x]
    (if (= (.getProtocol x) "file")
      (make-bgzf-output-stream (cio/as-file x))
      (throw (IllegalArgumentException. (str "Can not write to non-file URL <" x ">")))))

  URI
  (make-bgzf-input-stream [^URI x]
    (make-bgzf-input-stream (.toURL x)))
  (make-bgzf-output-stream [^URI x]
    (make-bgzf-output-stream (.toURL x)))

  String
  (make-bgzf-input-stream [x]
    (try
      (make-bgzf-input-stream (URL. x))
      (catch MalformedURLException _
        (make-bgzf-input-stream (cio/as-file x)))))
  (make-bgzf-output-stream [x]
    (try
      (make-bgzf-output-stream (URL. x))
      (catch MalformedURLException _
        (make-bgzf-output-stream (cio/as-file x))))))

(defn ^BGZFInputStream bgzf-input-stream [x]
  (make-bgzf-input-stream x))

(defn ^BGZFOutputStream bgzf-output-stream [x]
  (make-bgzf-output-stream x))

(def ^:private ^:const shift-amount 16)

(def ^:private ^:const address-mask 0xFFFFFFFFFFFF)

(def ^:private ^:const offset-mask 0xFFFF)

(defn compare
  "Negative if fp1 is earlier in file than fp2, positive if it is later, 0 if equal."
  [^long fp1 ^long fp2]
  (cond
   (= fp1 fp2)                 0
   ;; When treating as unsigned, negative number is > positive.
   (and (< fp1 0) (>= fp2 0))  1
   (and (>= fp1 0) (< fp2 0)) -1
   ;; Either both negative or both non-negative, so regular comparison works.
   (< fp1 fp2)                -1
   :else                       1))

(defn get-block-address
  "File offset of start of BGZF block for this file pointer."
  [^long fp]
  (bit-and (bit-shift-right fp shift-amount) address-mask))

(defn get-block-offset
  "Offset into uncompressed block for this virtual file pointer."
  [^long fp]
  (bit-and fp offset-mask))

(defn same-or-adjacent-blocks?
  "Returns true if fp2 points to somewhere in the same BGZF block, or the one
  immediately following fp1's BGZF block."
  [^long fp1 ^long fp2]
  (let [block1 (long (get-block-address fp1))
        block2 (long (get-block-address fp2))]
    (or (= block1 block2) (= (inc block1) block2))))

(defn- bgzip-header?
  [^bytes b]
  (and (<= 16 (alength b))
       (= (unchecked-byte 0x1f) (aget b 0))
       (= (unchecked-byte 0x8b) (aget b 1))
       (bit-test (aget b 3) 2) ;; FEXTRA
       (= (byte \B) (aget b 12)) ;; SI1
       (= (byte \C) (aget b 13)) ;; SI2
       (= 2 (aget b 14)) ;; LEN
       (zero? (aget b 15))))

(defn bgzip?
  "Checks if a given file is bgzipped or not."
  [f]
  (let [buf (byte-array 16)]
    (with-open [r (cio/input-stream f)]
      (loop [off 0
             len (alength buf)]
        (let [n (.read r buf off len)]
          (cond
            (neg? n) false
            (< n len) (recur (+ off n) (- len n))
            :else (bgzip-header? buf)))))))
