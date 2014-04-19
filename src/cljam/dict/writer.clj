(ns cljam.dict.writer
  (:require [pandect.core :refer [md5]]
            [cljam.common :refer [version]]
            [cljam.util :refer [string->bytes]])
  (:import java.io.BufferedWriter))

;; DICTWriter
;; ----------

(deftype DICTWriter [writer f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

;; Making dict
;; -----------

(def ^:private upper-case-offset
  (- (byte \A) (byte \a)))

(defn- upper-case [b]
  (if (or (< b (byte \a)) (> b (byte \z)))
    b
    (byte (+ b upper-case-offset))))

(defn- make-hash [seq]
  (let [bases ^bytes (string->bytes seq)]
    (loop [i 0]
      (when (< i (count bases))
        (aset bases i ^byte (upper-case (nth bases i)))
        (recur (inc i))))
    (md5 bases)))

;; Writing
;; -------

(defn- write-header!
  [^BufferedWriter wtr]
  (.write wtr (str "@HD\tVN:" version "\tSO:unsorted"))
  (.newLine wtr))

(defn- write-sequence!
  [^BufferedWriter wtr ref seq ur]
  (let [blen (count (filter (partial not= \space) seq))
        m5 (make-hash seq)]
    (.write wtr (str "@SQ\tSN:" ref "\tLN:" blen "\tUR:" ur "\tM5:" m5)))
  (.newLine wtr))

(defn- write-dict*!
  [^BufferedWriter wtr reads ur]
  (write-header! wtr)
  (doseq [sq reads]
    (write-sequence! wtr (:rname sq) (:seq sq) ur)))

(defn write-dict!
  [^DICTWriter wtr reads ur]
  (write-dict*! (.writer wtr) reads ur))
