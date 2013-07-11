(ns cljam.util
  (:import net.sf.samtools.util.StringUtil))

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
  (let [length (count fastq)
        scores (byte-array length)]
    (map-indexed (fn [idx _]
                   (aset scores idx (fastq-to-phred (.charAt fastq idx))))
                 (range length))
    scores))

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
    [\g \G]    (byte 3)
    [\t \T]    (byte 4)
    [\n \N \.] (byte 5)
    ;; IUPAC ambiguity codes
    [\m \M]    (byte 6)
    [\r \R]    (byte 7)
    [\s \S]    (byte 8)
    [\v \V]    (byte 9)
    [\w \W]    (byte 10)
    [\y \Y]    (byte 11)
    [\h \H]    (byte 12)
    [\k \K]    (byte 13)
    [\d \D]    (byte 14)
    [\b \B]    (byte 15)))

(defn char-to-compressed-base-high [base]
  (condp (fn [case-vec ch]
           (some #(= ch %) case-vec)) base
    [\=]       (byte (bit-shift-left (byte 0) 4))
    [\a \A]    (byte (bit-shift-left (byte 1) 4))
    [\c \C]    (byte (bit-shift-left (byte 2) 4))
    [\g \G]    (byte (bit-shift-left (byte 3) 4))
    [\t \T]    (byte (bit-shift-left (byte 4) 4))
    [\n \N \.] (byte (bit-shift-left (byte 5) 4))
    ;; IUPAC ambiguity codes
    [\m \M]    (byte (bit-shift-left (byte 6) 4))
    [\r \R]    (byte (bit-shift-left (byte 7) 4))
    [\s \S]    (byte (bit-shift-left (byte 8) 4))
    [\v \V]    (byte (bit-shift-left (byte 9) 4))
    [\w \W]    (byte (bit-shift-left (byte 10) 4))
    [\y \Y]    (byte (bit-shift-left (byte 11) 4))
    [\h \H]    (byte (bit-shift-left (byte 12) 4))
    [\k \K]    (byte (bit-shift-left (byte 13) 4))
    [\d \D]    (byte (bit-shift-left (byte 14) 4))
    [\b \B]    (byte (bit-shift-left (byte 15) 4))))

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
        len (/ (inc rlen) 2)
        cbases (byte-array len)]
   (map-indexed (fn [idx _]
                  (aset cbases (/ idx 2)
                        (byte (bit-or (char-to-compressed-base-high (nth read-bases (dec idx)))
                                      (char-to-compressed-base-low (nth read-bases idx))))))
                (filter odd? (range 1 (count read-bases))))
   (if (odd? rlen)
     (aset cbases (/ rlen 2) (char-to-compressed-base-high (char (nth read-bases (dec rlen))))))
   cbases))
