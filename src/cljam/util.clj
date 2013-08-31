(ns cljam.util
  (:require [clojure.string :refer [join]]))

(defn ubyte
  "Casts to byte avoiding an error about out of range for byte."
  [n]
  {:pre [(<= 0 n 255)]}
  (byte (if (< n 0x80) n (- n 0x100))))

(defn ra-line-seq
  [rdr]
  (when-let [line (.readLine rdr)]
    (cons line (lazy-seq (ra-line-seq rdr)))))

;;; string utils

(def ^:private upper-case-offset (- (byte \A) (byte \a)))

(defn upper-case
  "Converts a lower case letter to upper case."
  [b]
  (if (or (< b \a) (> b \z))
    b
    (byte (+ b upper-case-offset))))

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

;;;

(defn reg->bin
  "Calculates bin given an alignment covering [beg,end) (zero-based, half-close-half-open),
  the same as reg2bin on samtools."
  [beg end]
  (let [end (dec end)]
    (cond
     (= (bit-shift-right beg 14) (bit-shift-right end 14))
     (+ (/ (dec (bit-shift-left 1 15)) 7) (bit-shift-right beg 14))

     (= (bit-shift-right beg 17) (bit-shift-right end 17))
     (+ (/ (dec (bit-shift-left 1 12)) 7) (bit-shift-right beg 17))

     (= (bit-shift-right beg 20) (bit-shift-right end 20))
     (+ (/ (dec (bit-shift-left 1 9)) 7) (bit-shift-right beg 20))

     (= (bit-shift-right beg 23) (bit-shift-right end 23))
     (+ (/ (dec (bit-shift-left 1 6)) 7) (bit-shift-right beg 23))

     (= (bit-shift-right beg 26) (bit-shift-right end 26))
     (+ (/ (dec (bit-shift-left 1 3)) 7) (bit-shift-right beg 26))

     :else 0)))

(def compressed-bases-low
  "Representation of bases for when in low-order nybble."
  {:eq (ubyte 0x0)
   :a  (ubyte 0x1)
   :c  (ubyte 0x2)
   :g  (ubyte 0x4)
   :t  (ubyte 0x8)
   :n  (ubyte 0xf)
   :m  (ubyte 0x3)
   :r  (ubyte 0x5)
   :s  (ubyte 0x6)
   :v  (ubyte 0x7)
   :w  (ubyte 0x9)
   :y  (ubyte 0xa)
   :h  (ubyte 0xb)
   :k  (ubyte 0xc)
   :d  (ubyte 0xd)
   :b  (ubyte 0xe)})

(def compressed-bases-high
  "Representation of bases for when in high-order nybble."
  {:eq (ubyte 0x0)
   :a  (ubyte 0x10)
   :c  (ubyte 0x20)
   :g  (ubyte 0x40)
   :t  (ubyte 0x80)
   :n  (ubyte 0xf0)
   :m  (ubyte 0x30)
   :r  (ubyte 0x50)
   :s  (ubyte 0x60)
   :v  (ubyte 0x70)
   :w  (ubyte 0x90)
   :y  (ubyte 0xa0)
   :h  (ubyte 0xb0)
   :k  (ubyte 0xc0)
   :d  (ubyte 0xd0)
   :b  (ubyte 0xe0)})

(defn char->compressed-base-low
  "Convert from a char to BAM nybble representation of a base in low-order nybble."
  [base]
  (condp #(some #{%2} %1) base
    [\=]       (:eq compressed-bases-low)
    [\a \A]    (:a  compressed-bases-low)
    [\c \C]    (:c  compressed-bases-low)
    [\g \G]    (:g  compressed-bases-low)
    [\t \T]    (:t  compressed-bases-low)
    [\n \N \.] (:n  compressed-bases-low)
    [\m \M]    (:m  compressed-bases-low)
    [\r \R]    (:r  compressed-bases-low)
    [\s \S]    (:s  compressed-bases-low)
    [\v \V]    (:v  compressed-bases-low)
    [\w \W]    (:w  compressed-bases-low)
    [\y \Y]    (:y  compressed-bases-low)
    [\h \H]    (:h  compressed-bases-low)
    [\k \K]    (:k  compressed-bases-low)
    [\d \D]    (:d  compressed-bases-low)
    [\b \B]    (:b  compressed-bases-low)
    (throw (IllegalArgumentException. (str "Bad type: " base)))))

(defn char->compressed-base-high
  "Convert from a char to BAM nybble representation of a base in high-order nybble."
  [base]
  (condp #(some #{%2} %1) base
    [\=]       (:eq compressed-bases-high)
    [\a \A]    (:a  compressed-bases-high)
    [\c \C]    (:c  compressed-bases-high)
    [\g \G]    (:g  compressed-bases-high)
    [\t \T]    (:t  compressed-bases-high)
    [\n \N \.] (:n  compressed-bases-high)
    [\m \M]    (:m  compressed-bases-high)
    [\r \R]    (:r  compressed-bases-high)
    [\s \S]    (:s  compressed-bases-high)
    [\v \V]    (:v  compressed-bases-high)
    [\w \W]    (:w  compressed-bases-high)
    [\y \Y]    (:y  compressed-bases-high)
    [\h \H]    (:h  compressed-bases-high)
    [\k \K]    (:k  compressed-bases-high)
    [\d \D]    (:d  compressed-bases-high)
    [\b \B]    (:b  compressed-bases-high)
    (throw (IllegalArgumentException. (str "Bad type: " base)))))

(defn compressed-base->char-low
  "Convert from BAM nybble representation of a base in low-order nybble to a char."
  [base]
  (condp = (ubyte (bit-and base 0xf))
    (:eq compressed-bases-low) \=
    (:a  compressed-bases-low) \A
    (:c  compressed-bases-low) \C
    (:g  compressed-bases-low) \G
    (:t  compressed-bases-low) \T
    (:n  compressed-bases-low) \N
    (:m  compressed-bases-low) \M
    (:r  compressed-bases-low) \R
    (:s  compressed-bases-low) \S
    (:v  compressed-bases-low) \V
    (:w  compressed-bases-low) \W
    (:y  compressed-bases-low) \Y
    (:h  compressed-bases-low) \H
    (:k  compressed-bases-low) \K
    (:d  compressed-bases-low) \D
    (:b  compressed-bases-low) \B
    (throw (IllegalArgumentException. (str "Bad type: " base)))))

(defn compressed-base->char-high
  "Convert from BAM nybble representation of a base in high-order nybble to a char."
  [base]
  (condp = (ubyte (bit-and base 0xf0))
    (:eq compressed-bases-high) \=
    (:a  compressed-bases-high) \A
    (:c  compressed-bases-high) \C
    (:g  compressed-bases-high) \G
    (:t  compressed-bases-high) \T
    (:n  compressed-bases-high) \N
    (:m  compressed-bases-high) \M
    (:r  compressed-bases-high) \R
    (:s  compressed-bases-high) \S
    (:v  compressed-bases-high) \V
    (:w  compressed-bases-high) \W
    (:y  compressed-bases-high) \Y
    (:h  compressed-bases-high) \H
    (:k  compressed-bases-high) \K
    (:d  compressed-bases-high) \D
    (:b  compressed-bases-high) \B
    (throw (IllegalArgumentException. (str "Bad type: " base)))))

(defn bytes->compressed-bases [read-bases]
  (let [rlen (count read-bases)
        bases (for [i (range 1 rlen) :when (odd? i)]
                (byte (bit-or (char->compressed-base-high (char (nth read-bases (dec i))))
                              (char->compressed-base-low (char (nth read-bases i))))))]
   (if (odd? rlen)
     (byte-array (conj (vec bases) (char->compressed-base-high (char (nth read-bases (dec rlen))))))
     (byte-array bases))))

(defn compressed-bases->chars [length compressed-bases compressed-offset]
  (let [bases (apply concat
                     (for [i (range 1 length) :when (odd? i)]
                       (let [cidx (+ (/ i 2) compressed-offset)]
                         [(compressed-base->char-high (nth compressed-bases cidx))
                          (compressed-base->char-low  (nth compressed-bases cidx))])))]
    (if (odd? length)
      (conj (vec bases) (compressed-base->char-high (nth compressed-bases (+ (/ length 2) compressed-offset))))
      bases)))

(defn normalize-bases [^bytes bases]
  (map-indexed (fn [idx _]
                 (aset bases idx ^byte (upper-case (nth bases idx)))
                 (if (= (nth bases idx) \.)
                   (aset bases idx (byte \N))))
               bases)
  bases)

;;; fastq and phred

(defmulti fastq->phred class)

(defmethod fastq->phred String
  [^String fastq]
  (let [length (count fastq)]
    (byte-array (for [i (range length)]
                  (fastq->phred (.charAt fastq i))))))

(defmethod fastq->phred Character
  [ch]
  {:pre [(<= 33 (int ch) 126)]}
  (byte (- (int ch) 33)))

(defmulti phred->fastq class)

(defmethod phred->fastq (class (byte-array nil))
  [b]
  (when-not (nil? b)
    (join (map #(phred->fastq (int (bit-and % 0xff))) b))))

(def max-phred-score 93)

(defmethod phred->fastq Integer
  [n]
  {:pre [(<= 0 n max-phred-score)]}
  (char (+ n 33)))
