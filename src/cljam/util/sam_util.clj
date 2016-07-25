(ns cljam.util.sam-util
  "Utilities related to SAM/BAM format."
  (:require [clojure.string :as cstr]
            [cljam.cigar :refer [count-ref]]
            [cljam.util :refer [ubyte str->int str->float]]))

;;; parse

(defn- parse-header-keyvalues
  "e.g. \"LN:45 SN:ref\" -> {:LN 45, :SN \"ref\"}"
  [keyvalues]
  (apply merge
         (map (fn [kv]
                (let [[k v] (cstr/split kv #":")]
                  {(keyword k) (case k
                                 "LN" (Integer/parseInt v)
                                 "PI" (Integer/parseInt v)
                                 v)}))
              keyvalues)))

(defn parse-header-line [line]
  (let [[typ & kvs] (cstr/split line #"\t")]
    {(keyword (subs typ 1)) (if (= typ "@HD")
                              (parse-header-keyvalues kvs)
                              (vector (parse-header-keyvalues kvs)))}))

(defn- parse-header* [col]
  (when (seq col)
    (merge-with #(vec (concat %1 %2)) (parse-header-line (first col)) (parse-header* (rest col)))))

(defn parse-header
  "Parse a header string, returning a map of the header."
  [s]
  (parse-header* (cstr/split s #"\n")))

(defn- parse-tag-single [val-type val]
  (case val-type
    \Z val
    \A (first val)
    \I (str->int val)
    \i (str->int val)
    \s (str->int val)
    \S (str->int val)
    \c (str->int val)
    \C (str->int val)
    \f (str->float val)
    \H nil ;;FIXME
    (throw (Exception. "Unrecognized tag type"))))

(defn- parse-optional-fields [options]
  (map (fn [op]
         (let [[tag val-type-str val] (cstr/split op #":")
               val-type (first val-type-str)]
           {(keyword tag) {:type val-type-str
                           :value (if (= val-type \B)
                                    val
                                    (parse-tag-single val-type val))}}))
       options))

(defn- parse-seq-text [s]
  (cstr/upper-case s))

(defn parse-alignment
  "Parse an alignment line, returning a map of the alignment."
  [line]
  (let [fields (cstr/split line #"\t")]
    {:qname   (first fields)
     :flag    (Integer/parseInt (nth fields 1))
     :rname   (nth fields 2)
     :pos     (Integer/parseInt (nth fields 3))
     :mapq    (Integer/parseInt (nth fields 4))
     :cigar   (nth fields 5)
     :rnext   (nth fields 6)
     :pnext   (Integer/parseInt (nth fields 7))
     :tlen    (Integer/parseInt (nth fields 8))
     :seq     (parse-seq-text (nth fields 9))
     :qual    (nth fields 10)
     :options (vec (parse-optional-fields (drop 11 fields)))}))

;;; stringify

(defn- stringify-header-keyvalues [kv-map]
  (cstr/join \tab
             (map (fn [kv]
                    (let [[k v] (seq kv)]
                      (str (name k) \: v)))
                  kv-map)))

(defn- stringify-optional-fields [options]
  (cstr/join \tab
             (map (fn [op]
                    (let [[tag entity] (first (seq op))]
                      (str (name tag) \: (:type entity) \: (:value entity))))
                  options)))

(defn stringify-header [hdr]
  (cstr/join \newline
             (map (fn [h]
                    (let [[typ kvs] h]
                      (if (= typ :HD)
                        (str "@HD" \tab (stringify-header-keyvalues kvs))
                        (cstr/join \newline
                                   (map #(str \@ (name typ) \tab (stringify-header-keyvalues %)) kvs)))))
                  (seq hdr))))

(defn stringify-alignment [sa]
  (cstr/trim
   (cstr/join \tab
              [(:qname sa)
               (:flag  sa)
               (:rname sa)
               (:pos   sa)
               (:mapq  sa)
               (:cigar sa)
               (:rnext sa)
               (:pnext sa)
               (:tlen  sa)
               (:seq   sa)
               (:qual  sa)
               (stringify-optional-fields (:options sa))])))

;;; indexing bin

(defn reg->bin
  "Calculates bin given an alignment covering [beg,end) (zero-based, half-close-half-open),
  the same as reg2bin on samtools."
  [^long beg ^long end]
  (let [end (dec end)]
    (cond
     (= (bit-shift-right beg 14) (bit-shift-right end 14))
     (+ 4681 (bit-shift-right beg 14))

     (= (bit-shift-right beg 17) (bit-shift-right end 17))
     (+ 585 (bit-shift-right beg 17))

     (= (bit-shift-right beg 20) (bit-shift-right end 20))
     (+ 73 (bit-shift-right beg 20))

     (= (bit-shift-right beg 23) (bit-shift-right end 23))
     (+ 9 (bit-shift-right beg 23))

     (= (bit-shift-right beg 26) (bit-shift-right end 26))
     (+ 1 (bit-shift-right beg 26))

     :else 0)))

(defn get-end
  [aln]
  (dec
   (+ (:pos aln)
      (count-ref (or (:cigar-bytes (:meta aln))
                     (:cigar aln))))))

(defn compute-bin
  "Returns indexing bin based on alignment start and end."
  [aln]
  (let [beg (dec (:pos aln))
        tmp-end (get-end aln)
        end (if (<= tmp-end 0) (inc beg) tmp-end)]
   (reg->bin beg end)))

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

(defn- unfold-ksv-map [ksv-map]
  (into {} (mapcat (fn [[ks v]]
                     (map (fn [k] [k v]) ks))
                   ksv-map)))

(def ^:private _char->compressed-base-low
  (unfold-ksv-map {[\=]       (:eq compressed-bases-low)
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
                   [\b \B]    (:b  compressed-bases-low)}))

(defn char->compressed-base-low
  "Convert from a char to BAM nybble representation of a base in low-order nybble."
  [base]
  (or
   (_char->compressed-base-low base)
   (throw (IllegalArgumentException. (str "Bad type: " base)))))

(def ^:private _char->compressed-base-high
  (unfold-ksv-map {[\=]       (:eq compressed-bases-high)
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
                   [\b \B]    (:b  compressed-bases-high)}))

(defn char->compressed-base-high
  "Convert from a char to BAM nybble representation of a base in high-order nybble."
  [base]
  (or
   (_char->compressed-base-high base)
   (throw (IllegalArgumentException. (str "Bad type: " base)))))

(def ^:private _compressed-base->char-low
  {(:eq compressed-bases-low) \=
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
   (:b  compressed-bases-low) \B})

(defn compressed-base->char-low
  "Convert from BAM nybble representation of a base in low-order nybble to a char."
  [base]
  (or
   (_compressed-base->char-low (ubyte (bit-and base 0xf)))
   (throw (IllegalArgumentException. (str "Bad type: " base)))))

(def ^:private _compressed-base->char-high
  {(:eq compressed-bases-high) \=
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
   (:b  compressed-bases-high) \B})

(defn compressed-base->char-high
  "Convert from BAM nybble representation of a base in high-order nybble to a char."
  [base]
  (or
   (_compressed-base->char-high (ubyte (bit-and base 0xf0)))
   (throw (IllegalArgumentException. (str "Bad type: " base)))))

(defn bytes->compressed-bases [^bytes read-bases]
  (let [rlen (alength read-bases)
        result-len (quot (inc rlen) 2)
        result (byte-array result-len)]
    (loop [i 0]
      (if (<= result-len i)
        result
        (let [h-idx (* 2 i)
              h (char->compressed-base-high (char (aget read-bases h-idx)))
              l-idx (inc h-idx)
              l (if (<= rlen l-idx)
                  0
                  (char->compressed-base-low (char (aget read-bases l-idx))))]
          (aset-byte result i (byte (bit-or h l)))
          (recur (inc i)))))))

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
                 (aset bases idx ^byte (cstr/upper-case (nth bases idx)))
                 (if (= (nth bases idx) \.)
                   (aset bases idx (byte \N))))
               bases)
  bases)

;;; fastq and phred

(defn fastq-char->phred-byte [ch]
  [ch]
  {:pre [(<= 33 (int ch) 126)]}
  (byte (- (int ch) 33)))

(defn fastq->phred [^String fastq]
  (let [length (count fastq)
        result (byte-array length)]
    (dotimes [i length]
      (aset-byte result i (fastq-char->phred-byte (.charAt fastq i))))
    result))

(defmulti phred->fastq class)

(defmethod phred->fastq (class (byte-array nil))
  [b]
  (when-not (nil? b)
    (cstr/join (map #(phred->fastq (int (bit-and % 0xff))) b))))

(def ^:const max-phred-score 93)

(defmethod phred->fastq Integer
  [n]
  {:pre [(<= 0 n max-phred-score)]}
  (char (+ n 33)))

;;; Reference functions

(defn make-refs
  "Return a reference sequence from the sam header."
  [hdr]
  (for [sq (:SQ hdr)]
    {:name (:SN sq), :len (:LN sq)}))

(defn- ref-id*
  [refs name]
  (some #(when (= name (:name (second %))) (first %))
        (map-indexed vector refs)))

(def ref-id
  "Returns reference ID from the reference sequence and the specified reference
  name. If not found, returns nil."
  (memoize ref-id*))

(defn ref-name
  "Returns a reference name from the reference ID. Returns nil if id is not
  mapped."
  [refs id]
  (if (<= 0 id (dec (count refs)))
    (:name (nth refs id))))

(defn ref-by-name
  "Returns the first reference which has the specified name."
  [refs name]
  (some #(if (= (:name %) name) %) refs))
