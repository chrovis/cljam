(ns cljam.cigar
  "Parser of CIGAR strings."
  (:import [java.nio ByteBuffer ByteOrder]))

(defn parse
  "Parses CIGAR string, returning a sequence of lengths and operations."
  [^String s]
  (for [[_ n op] (re-seq #"([0-9]*)([MIDNSHP=X])" s)]
    [(Integer/parseInt n) (first op)]))

(defn count-op
  "Returns length of CIGAR operations."
  [^String s]
  (count (parse s)))

(defn- count-ref-str*
  [^String s]
  (->> (parse s)
       (filter (comp #{\M \D \N \= \X} peek))
       (map first)
       (reduce +)))

(def ^:private count-ref-str
  (memoize count-ref-str*))

(def ^:private bases-op-bytes
  #{(byte 0)
    (byte 2)
    (byte 3)
    (byte 7)
    (byte 8)})

(defn- count-ref-bytes
  [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (loop [len 0]
      (if (.hasRemaining buf)
        (let [b (.getInt buf)
              op (bit-and b 0xF)
              n (bit-shift-right b 4)]
          (if (bases-op-bytes op)
            (recur (+ len n))
            (recur len)))
        len))))

(defmulti count-ref
  "Returns length of reference bases."
  class)

(defmethod count-ref String
  [s]
  (count-ref-str s))

(defmethod count-ref (Class/forName "[B")
  [b]
  (count-ref-bytes b))
