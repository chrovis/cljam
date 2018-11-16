(ns cljam.util.sequence
  (:require [clojure.string :as cstr])
  (:import [java.nio CharBuffer]))

(def ^:private revcomp-table
  (let [ba (byte-array 128)]
    (dotimes [i 128] (aset-byte ba i (byte \N)))
    (doseq [s ["AT" "GC" "NN"]
            [a b] [(cstr/upper-case s) (cstr/lower-case s)]]
      (aset-byte ba (int a) (byte b))
      (aset-byte ba (int b) (byte a)))
    ba))

(defn revcomp
  "Returns a reverse-complement sequence of given sequence. All characters other
  than ATGCNatgcn will be replaced with N. Masks will be kept."
  [^String s]
  (let [l (.length s)
        cb (CharBuffer/allocate l)]
    (dotimes [i l]
      (->> (unchecked-dec (- l i))
           (.charAt s)
           unchecked-int
           (aget ^bytes revcomp-table)
           unchecked-char
           (.put cb)))
    (.flip cb)
    (.toString cb)))

(defn atgcn?
  "Checks if a given string contains only [ATGCNatgcn]."
  [^String s]
  (let [l (.length s)]
    (loop [i 0]
      (if (< i l)
        (let [c' (unchecked-long (unchecked-int (.charAt s (unchecked-int i))))
              c (if (<= 97 c') (- c' 32) c')]
          (if (or (= c 65) (= c 67) (= c 71) (= c 78) (= c 84))
            (recur (unchecked-inc i))
            false))
        true))))

(defn ^String ->atgcn
  "Replaces all characters other than [ATGCatgc] with N."
  [^String s]
  (when s
    (let [l (.length s)
          cb (CharBuffer/allocate l)]
      (dotimes [i l]
        (let [c'' (.charAt s i)
              c' (unchecked-long (unchecked-int c''))
              c (if (<= 97 c') (- c' 32) c')]
          (if (or (= c 65) (= c 67) (= c 71) (= c 78) (= c 84))
            (.put cb c'')
            (.put cb \N))))
      (.rewind cb)
      (.toString cb))))
