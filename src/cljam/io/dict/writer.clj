(ns cljam.io.dict.writer
  "Makes a sequence dictionary from FASTA data, and writes it to a file."
  (:require [digest]
            [cljam.io.sam.common :refer [sam-version]]
            [cljam.util :refer [string->bytes graph?]])
  (:import java.io.BufferedWriter))

;; DICTWriter
;; ----------

(deftype DICTWriter [^java.io.BufferedWriter writer url]
  java.io.Closeable
  (close [this]
    (.close writer)))

;; Making dict
;; -----------

(def ^:const ^:private upper-case-offset
  "Equals to `(- (byte \\A) (byte \\a))`."
  -32)

(defn- upper-case [b]
  (if (or (< b (byte \a)) (> b (byte \z)))
    b
    (byte (+ b upper-case-offset))))

(defn- make-hash
  "Normalizes the sequence string, calculates its MD5 hash, and returns it."
  [sequence]
  (let [bases ^bytes (string->bytes sequence)]
    (loop [i 0]
      (when (< i (count bases))
        (aset bases i ^byte (upper-case (nth bases i)))
        (recur (inc i))))
    (digest/md5 bases)))

(defn- init-dict-status
  []
  {:sequence "", :len 0})

(defn- update-dict-status
  [dict-status sequence]
  {:sequence (str (:sequence dict-status) sequence)
   :len (+ (:len dict-status) (count (filter graph? sequence)))})

(defn make-dict
  "Calculates sequence dictionary from the headers and sequences, returning it
  as a map."
  [headers sequences ur]
  (loop [[seq* & rest] sequences
         name (:name seq*)
         dict-status (init-dict-status)
         dicts {}]
    (if seq*
      (let [name' (:name seq*)
            new? (not= name' name)
            dict-status' (update-dict-status
                          (if new? (init-dict-status) dict-status) (:sequence seq*))
            dicts' (if new?
                     (assoc dicts name {:blen (:len dict-status)
                                        :ur ur
                                        :m5 (make-hash (:sequence dict-status))})
                     dicts)]
        (recur rest name' dict-status' dicts'))
      (assoc dicts name {:blen (:len dict-status)
                         :ur ur
                         :m5 (make-hash (:sequence dict-status))}))))

;; Writing
;; -------

(defn- write-header!
  [^BufferedWriter wtr]
  (.write wtr (str "@HD\tVN:" sam-version "\tSO:unsorted"))
  (.newLine wtr))

(defn- write-sequence!
  [^BufferedWriter wtr name blen ur m5]
  (.write wtr (str "@SQ\tSN:" name "\tLN:" blen "\tM5:" m5 "\tUR:" ur))
  (.newLine wtr))

(defn- write-dict*!
  [wtr headers sequences ur]
  (let [dicts (make-dict headers sequences ur)]
   (write-header! wtr)
   (doseq [header headers]
     (let [dict (get dicts (:name header))]
      (write-sequence! wtr
                       (:name header)
                       (:blen dict)
                       (:ur dict)
                       (:m5 dict))))))

(defn write-dict!
  [^DICTWriter wtr headers sequences ur]
  (write-dict*! (.writer wtr) headers sequences ur))
