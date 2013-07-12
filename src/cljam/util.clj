(ns cljam.util
  (:import net.sf.samtools.util.StringUtil))

(defn byte-cast [n]
  (byte (if (< n 0x80) n (- n 0x100))))

(defn reg2bin [beg end]
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

(defn char-to-compressed-base-low [base]
  (condp (fn [case-vec ch]
           (some #(= ch %) case-vec)) base
    [\=]       (byte 0)
    [\a \A]    (byte 1)
    [\c \C]    (byte 2)
    [\g \G]    (byte 4)
    [\t \T]    (byte 8)
    [\n \N \.] (byte 15)
    ;; IUPAC ambiguity codes
    [\m \M]    (byte 3)
    [\r \R]    (byte 5)
    [\s \S]    (byte 6)
    [\v \V]    (byte 7)
    [\w \W]    (byte 9)
    [\y \Y]    (byte 10)
    [\h \H]    (byte 11)
    [\k \K]    (byte 12)
    [\d \D]    (byte 13)
    [\b \B]    (byte 14)))

(defn char-to-compressed-base-high [base]
  (condp (fn [case-vec ch]
           (some #(= ch %) case-vec)) base
    [\=]       (byte-cast 0x0)
    [\a \A]    (byte-cast 0x10)
    [\c \C]    (byte-cast 0x20)
    [\g \G]    (byte-cast 0x40)
    [\t \T]    (byte-cast 0x80)
    [\n \N \.] (byte-cast 0xf0)
    ;; IUPAC ambigui-castty codes
    [\m \M]    (byte-cast 0x30)
    [\r \R]    (byte-cast 0x50)
    [\s \S]    (byte-cast 0x60)
    [\v \V]    (byte-cast 0x70)
    [\w \W]    (byte-cast 0x90)
    [\y \Y]    (byte-cast 0xa0)
    [\h \H]    (byte-cast 0xb0)
    [\k \K]    (byte-cast 0xc0)
    [\d \D]    (byte-cast 0xd0)
    [\b \B]    (byte-cast 0xe0)))

(defn string-to-bytes [s]
  (let [buf (byte-array (count s))]
    (.getBytes s 0 (count buf) buf 0)
    buf))

(defn normalize-bases [bases]
  (map-indexed (fn [idx _]
                 (aset bases idx (StringUtil/toUpperCase (nth bases idx)))
                 (if (= (nth bases idx) \.)
                   (aset bases idx \N)))
               bases)
  bases)

(defn bytes-to-compressed-bases [read-bases]
  (let [rlen (count read-bases)
        aaa (for [i (range 1 rlen) :when (odd? i)]
              (byte (bit-or (char-to-compressed-base-high (char (nth read-bases (dec i))))
                            (char-to-compressed-base-low (char (nth read-bases i))))))]
   (if (odd? rlen)
     (byte-array (conj (vec aaa) (char-to-compressed-base-high (char (nth read-bases (dec rlen))))))
     (byte-array aaa))))
