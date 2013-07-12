(ns cljam.util
  (:import net.sf.samtools.util.StringUtil))

(defn ubyte
  "Casts to byte avoiding an error about out of range for byte."
  [n]
  {:pre [(>= n 0) (<= n 255)]}
  (byte (if (< n 0x80) n (- n 0x100))))

(defn string-to-bytes [s]
  (let [buf (byte-array (count s))]
    (.getBytes s 0 (count buf) buf 0)
    buf))

(defn reg-to-bin
  "Calculates bin given an alignment covering [beg,end) (zero-based, half-close-half-open),
the same as reg2bin on samtools."
  [beg end]
  (let [end (dec end)]
    (cond
     (= (bit-shift-right beg 14) (bit-shift-right end 14))
     (+ (/ (- (bit-shift-left 1 15) 1) 7) (bit-shift-right beg 14))

     (= (bit-shift-right beg 17) (bit-shift-right end 17))
     (+ (/ (- (bit-shift-left 1 12) 1) 7) (bit-shift-right beg 17))

     (= (bit-shift-right beg 20) (bit-shift-right end 20))
     (+ (/ (- (bit-shift-left 1 9) 1) 7) (bit-shift-right beg 20))

     (= (bit-shift-right beg 23) (bit-shift-right end 23))
     (+ (/ (- (bit-shift-left 1 6) 1) 7) (bit-shift-right beg 23))

     (= (bit-shift-right beg 26) (bit-shift-right end 26))
     (+ (/ (- (bit-shift-left 1 3) 1) 7) (bit-shift-right beg 26))

     :else 0)))

(defn char-to-compressed-base-low [base]
  (condp (fn [case-vec ch]
           (some #(= ch %) case-vec)) base
    [\=]       (ubyte 0x0)
    [\a \A]    (ubyte 0x1)
    [\c \C]    (ubyte 0x2)
    [\g \G]    (ubyte 0x4)
    [\t \T]    (ubyte 0x8)
    [\n \N \.] (ubyte 0xf)
    ;; IUPAC ambigui-castt0xy codes
    [\m \M]    (ubyte 0x3)
    [\r \R]    (ubyte 0x5)
    [\s \S]    (ubyte 0x6)
    [\v \V]    (ubyte 0x7)
    [\w \W]    (ubyte 0x9)
    [\y \Y]    (ubyte 0xa)
    [\h \H]    (ubyte 0xb)
    [\k \K]    (ubyte 0xc)
    [\d \D]    (ubyte 0xd)
    [\b \B]    (ubyte 0xe)))

(defn char-to-compressed-base-high [base]
  (condp (fn [case-vec ch]
           (some #(= ch %) case-vec)) base
    [\=]       (ubyte 0x0)
    [\a \A]    (ubyte 0x10)
    [\c \C]    (ubyte 0x20)
    [\g \G]    (ubyte 0x40)
    [\t \T]    (ubyte 0x80)
    [\n \N \.] (ubyte 0xf0)
    ;; IUPAC ambigui-castty codes
    [\m \M]    (ubyte 0x30)
    [\r \R]    (ubyte 0x50)
    [\s \S]    (ubyte 0x60)
    [\v \V]    (ubyte 0x70)
    [\w \W]    (ubyte 0x90)
    [\y \Y]    (ubyte 0xa0)
    [\h \H]    (ubyte 0xb0)
    [\k \K]    (ubyte 0xc0)
    [\d \D]    (ubyte 0xd0)
    [\b \B]    (ubyte 0xe0)))

(defn normalize-bases [bases]
  (map-indexed (fn [idx _]
                 (aset bases idx (StringUtil/toUpperCase (nth bases idx)))
                 (if (= (nth bases idx) \.)
                   (aset bases idx \N)))
               bases)
  bases)

(defn bytes-to-compressed-bases [read-bases]
  (let [rlen (count read-bases)
        bases (for [i (range 1 rlen) :when (odd? i)]
              (byte (bit-or (char-to-compressed-base-high (char (nth read-bases (dec i))))
                            (char-to-compressed-base-low (char (nth read-bases i))))))]
   (if (odd? rlen)
     (byte-array (conj (vec bases) (char-to-compressed-base-high (char (nth read-bases (dec rlen))))))
     (byte-array bases))))

(defmulti fastq-to-phred class)

(defmethod fastq-to-phred String
  [fastq]
  (let [length (count fastq)]
    (-> (for [i (range length)]
          (fastq-to-phred (.charAt fastq i)))
        (byte-array))))

(defmethod fastq-to-phred Character
  [ch]
  {:pre [(>= (int ch) 33) (<= (int ch) 126)]}
  (byte (- (int ch) 33)))
