(ns cljam.io.sam.util.cigar
  "Parser of CIGAR strings."
  (:require [clojure.core.memoize :as memoize]
            [proton.core :as proton])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn parse
  "Parses CIGAR string, returning a sequence of lengths and operations."
  [^String s]
  (for [[_ n op] (re-seq #"([0-9]*)([MIDNSHP=X])" s)]
    [(Integer/parseInt n) (first op)]))

(defn simplify
  "Merge contiguous same operations of parsed CIGAR."
  [cigs]
  (loop [[[^long l op :as x] & xs] cigs result (transient [])]
    (if (and l op)
      (let [[^long nl nop] (first xs)]
        (if (= op nop)
          (recur (cons [(+ l nl) op] (next xs)) result)
          (recur xs (conj! result x))))
      (persistent! result))))

(defn- concat! [v coll]
  (reduce conj! v coll))

(defn- update-last! [coll f]
  (let [c (dec (count coll))]
    (if (neg? c)
      coll
      (let [[op x] (get coll c)]
        (if (= :m op)
          (assoc! coll c (f x))
          coll)))))

(defn to-index*
  "Convert CIGAR string to sequence of indices."
  [^String s]
  (let [cigs (simplify (remove (comp #{\P \H} second) (parse s)))]
    (loop [[[^long l op] & xs] cigs r 0 s 0 idx (transient [])]
      (if (and l op)
        (condp get op
          #{\M \= \X} (recur xs (+ l r) (+ l s) (concat! idx (map (fn [x] [:m x]) (range s (+ l s)))))
          #{\D} (recur xs (+ r l) s (concat! (update-last! idx (fn [x] [:d x l])) (repeat l [:m \*])))
          #{\N} (recur xs (+ r l) s (concat! idx (repeat l [:m \>])))
          #{\S} (recur xs r (+ s l) idx)
          #{\I} (recur xs r (+ s l) (update-last! idx (fn [x] [:i x [s (+ l s)]]))))
        (persistent! idx)))))

(def to-index
  (memoize/lu to-index* :lu/threshold
              (or (proton/as-int (System/getProperty "cljam.sam.cigar.cache-size"))
                  1024)))

(defn count-op
  "Returns length of CIGAR operations."
  ^long [^String s]
  (count (parse s)))

(defn- count-ref-str*
  [^String s]
  (->> (parse s)
       (filter (comp #{\M \D \N \= \X} peek))
       (map first)
       (reduce +)))

(def ^:private count-ref-str
  (memoize count-ref-str*))

(defn count-ref-bytes
  "Count covering length in reference from encoded CIGAR byte-array."
  ^long
  [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (loop [ref-length 0]
      (if (.hasRemaining buf)
        (let [b (Integer/toUnsignedLong (.getInt buf))
              op (bit-and b 0xF)
              n (unsigned-bit-shift-right b 4)]
          (recur (+ ref-length (case op 0 n 2 n 3 n 7 n 8 n 0))))
        ref-length))))

(defn decode-cigar-and-ref-length
  "Decode CIGAR string and length of alignment in reference.
  Returns a vector of [cigar, ref-length]."
  [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)
        sb (StringBuilder.)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (loop [ref-length 0]
      (if (.hasRemaining buf)
        (let [b (Integer/toUnsignedLong (.getInt buf))
              op (bit-and b 0xF)
              n  (unsigned-bit-shift-right b 4)]
          (doto sb
            (.append n)
            (.append (case op 0 \M 1 \I 2 \D 3 \N 4 \S 5 \H 6 \P 7 \= 8 \X)))
          (recur (+ ref-length (case op 0 n 2 n 3 n 7 n 8 n 0))))
        [(.toString sb) ref-length]))))

(defn placeholder?
  "Returns a boolean indicating whether a CIGAR is in `kSmN` format."
  [^bytes cigar-bytes]
  (and (= 8 (alength cigar-bytes))
       (= 4 (bit-and 0xF (aget cigar-bytes 0)))   ;; S
       (= 3 (bit-and 0xF (aget cigar-bytes 4))))) ;; N

(defn encode-cigar
  "Encodes CIGAR string into a sequence of longs."
  [cigar]
  (mapv (fn [[^long n c]]
          (bit-or (bit-shift-left n 4)
                  (case c \M 0 \I 1 \D 2 \N 3 \S 4 \H 5 \P 6 \= 7 \X 8)))
        (parse cigar)))

(defmulti count-ref
  "Returns length of reference bases."
  class)

(defmethod count-ref String
  [s]
  (count-ref-str s))

(defmethod count-ref (Class/forName "[B")
  [b]
  (count-ref-bytes b))

(defn ->placeholder
  "Creates an encoded placeholder from a given CIGAR string.
  The placeholder is in the format of kSmN where k is the read length and m is
  the reference length. Returns a vector of ints."
  [cigar-str]
  (transduce
   identity
   (fn
     ([[^long r ^long q]]
      [(bit-or (bit-shift-left q 4) 4)
       (bit-or (bit-shift-left r 4) 3)])
     ([[^long r ^long q] [n op]]
      [(+ r (case op (\M \D \N \= \X) (long n) 0))
       (+ q (case op (\M \I \S \= \X) (long n) 0))]))
   [0 0]
   (parse cigar-str)))
