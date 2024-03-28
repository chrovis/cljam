(ns cljam.io.bam.decoder
  "Decoder of BAM alignment blocks."
  (:require [clojure.string :as cstr]
            [proton.core :as proton]
            [cljam.util :as util]
            cljam.io.protocols
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sam.util.sequence :as sam-seq]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.bam.common :as common]
            [cljam.io.util.byte-buffer :as bb])
  (:import [java.util Arrays]
           [java.nio Buffer ByteBuffer ByteOrder CharBuffer]
           [cljam.io.protocols SAMAlignment SAMRegionBlock SAMCoordinateBlock SAMQuerynameBlock]))

(definline validate-tag-type
  "Changes (u)int8 or (u)int16 tag type to int32 tag type."
  [t]
  `(case (long ~t)
     ~(long \I) \i
     ~(long \s) \i
     ~(long \S) \i
     ~(long \c) \i
     ~(long \C) \i
     (char ~t)))

(definline parse-tag-single
  "Parses a tag according to `tag-type`."
  [tag-type ^ByteBuffer bb]
  `(case (long ~tag-type)
     ~(long \Z) (bb/read-null-terminated-string ~bb)
     ~(long \A) (char (.get ~bb))
     ~(long \I) (bit-and (.getInt ~bb) 0xffffffff)
     ~(long \i) (.getInt ~bb)
     ~(long \s) (int (.getShort ~bb))
     ~(long \S) (bit-and (.getShort ~bb) 0xffff)
     ~(long \c) (int (.get ~bb))
     ~(long \C) (bit-and (int (.get ~bb)) 0xff)
     ~(long \f) (.getFloat ~bb)
     ~(long \H) (proton/hex->bytes (bb/read-null-terminated-string ~bb))
     (throw (Exception. "Unrecognized tag type"))))

(defn- parse-tag-array [^ByteBuffer bb]
  (let [typ (char (.get bb))
        len (.getInt bb)]
    (->> (for [_ (range len)]
           (case typ
             \c (int (.get bb))
             \C (bit-and (int (.get bb)) 0xff)
             \s (int (.getShort bb))
             \S (bit-and (.getShort bb) 0xffff)
             \i (.getInt bb)
             \I (bit-and (.getInt bb) 0xffffffff)
             \f (.getFloat bb)
             (throw (Exception. (str "Unrecognized tag array type: " typ)))))
         (cons typ)
         (cstr/join \,))))

(defn- parse-option [^ByteBuffer bb]
  (lazy-seq
   (when (.hasRemaining bb)
     (cons
      (let [cb (as-> (CharBuffer/allocate 2) cb
                 (.put cb (unchecked-char (.get bb)))
                 (.put cb (unchecked-char (.get bb)))
                 (.flip ^Buffer cb))
            typ (.get bb)]
        {(keyword (.toString cb))
         {:type  (str (validate-tag-type typ))
          :value (if (= typ 66) ;; (byte \B)
                   (parse-tag-array bb)
                   (parse-tag-single typ bb))}})
      (parse-option bb)))))

(defn decode-options
  "Decodes bytes of bam options."
  [rest']
  (let [bb (ByteBuffer/wrap rest')]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (parse-option bb)))

(defn options-size
  "Returns the number of bytes of the trailing optional fields."
  [^long block-size ^long l-read-name ^long n-cigar-op ^long l-seq]
  (- block-size
     common/fixed-block-size
     l-read-name
     (* n-cigar-op 4)
     (quot (inc l-seq) 2)
     l-seq))

(defn decode-qual
  "Decodes qual from bytes.
   Returns \"*\" if input is filled with 0xff, otherwise returns qual."
  [^bytes b]
  (if (Arrays/equals b (byte-array (alength b) (util/ubyte 0xff)))
    "*"
    (qual/phred-bytes->fastq b)))

(defn decode-seq
  "Decodes seq from bytes.
   Returns \"*\" if seq is empty, otherwise returns sequence."
  [seq-bytes ^long length]
  (if (zero? length)
    "*"
    (sam-seq/compressed-bases->str length seq-bytes 0)))

(defn decode-next-ref-id
  "Returns \"=\" if ref and next is same reference, otherwise returns next-ref-name."
  [refs ^long ref-id ^long next-ref-id]
  (if (= next-ref-id -1)
    "*"
    (if (= ref-id next-ref-id)
      "="
      (refs/ref-name refs next-ref-id))))

(defrecord BAMRawBlock [data ^long pointer-beg ^long pointer-end])

(defn- B-I-type-cigar-str->cigar-str
  [B-I-type-cigar]
  (let [xs (cstr/split (subs B-I-type-cigar 2) #",")
        bb (doto (ByteBuffer/allocate (* 4 (count xs)))
             (.order ByteOrder/LITTLE_ENDIAN))]
    (doseq [x xs] (.putInt bb (Long/parseLong x)))
    (first (cigar/decode-cigar-and-ref-length (.array bb)))))

(defn decode-alignment
  "Decodes BAM block and creates SAMAlignment instance which is compatible with SAM.
  When called with start and end, this function may return nil if any base of the block
  is not included in the range."
  ([refs block]
   (let [buffer      (bb/make-lsb-byte-buffer (:data block))
         ref-id      (.getInt buffer)
         rname       (or (refs/ref-name refs ref-id) "*")
         pos         (inc (.getInt buffer))
         l-read-name (short (bb/read-ubyte buffer))
         mapq        (short (bb/read-ubyte buffer))
         _           (bb/skip buffer 2) ; bin
         n-cigar-op  (int (bb/read-ushort buffer))
         flag        (int (bb/read-ushort buffer))
         l-seq       (.getInt buffer)
         next-ref-id (.getInt buffer)
         rnext       (decode-next-ref-id refs ref-id next-ref-id)
         pnext       (inc (.getInt buffer))
         tlen        (.getInt buffer)
         qname       (bb/read-string buffer (dec l-read-name))
         _           (bb/skip buffer 1)
         cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
         [cigar len] (cigar/decode-cigar-and-ref-length cigar-bytes)
         ref-end     (if (zero? (long len)) pos (dec (+ pos (long len))))
         seq'        (decode-seq (bb/read-bytes buffer (quot (inc l-seq) 2)) l-seq)
         qual        (decode-qual (bb/read-bytes buffer l-seq))
         rest'        (bb/read-bytes buffer (options-size (alength ^bytes (:data block)) l-read-name n-cigar-op l-seq))
         options     (decode-options rest')
         [cigar* options*]
         (if-let [cg (and (cigar/placeholder? cigar-bytes)
                          (:value (some :CG options)))]
           [(B-I-type-cigar-str->cigar-str cg) (remove :CG options)]
           [cigar options])]
     (SAMAlignment. qname (int flag) rname (int pos) ref-end (int mapq)
                    cigar* rnext (int pnext) (int tlen) seq' qual options*)))
  ([refs block ^long start ^long end]
   (let [buffer          (bb/make-lsb-byte-buffer (:data block))
         ref-id          (.getInt buffer)
         pos             (inc (.getInt buffer))]
     (when (<= pos end)
       (let [l-read-name (short (bb/read-ubyte buffer))
             mapq        (short (bb/read-ubyte buffer))
             _           (bb/skip buffer 2) ; bin
             n-cigar-op  (int (bb/read-ushort buffer))
             flag        (int (bb/read-ushort buffer))
             l-seq       (.getInt buffer)
             next-ref-id (.getInt buffer)
             rnext       (decode-next-ref-id refs ref-id next-ref-id)
             pnext       (inc (.getInt buffer))
             tlen        (.getInt buffer)
             qname       (bb/read-string buffer (dec l-read-name))
             _           (bb/skip buffer 1)
             cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
             [cigar len] (cigar/decode-cigar-and-ref-length cigar-bytes)
             ref-end     (int (if (zero? (long len))
                                pos
                                (dec (+ pos (long len)))))]
         (when (<= start ref-end)
           (let [seq'    (decode-seq (bb/read-bytes buffer (quot (inc l-seq) 2)) l-seq)
                 qual    (decode-qual (bb/read-bytes buffer l-seq))
                 rest'    (bb/read-bytes buffer (options-size (alength ^bytes (:data block)) l-read-name n-cigar-op l-seq))
                 rname   (or (refs/ref-name refs ref-id) "*")
                 options (decode-options rest')
                 [cigar* options*]
                 (if-let [cg (and (cigar/placeholder? cigar-bytes)
                                  (:value (some :CG options)))]
                   [(B-I-type-cigar-str->cigar-str cg) (remove :CG options)]
                   [cigar options])]
             (SAMAlignment. qname (int flag) rname (int pos) ref-end (int mapq)
                            cigar* rnext (int pnext) (int tlen) seq' qual options*))))))))

(defn decode-region-block
  "Decodes BAM block and returns a SAMRegionBlock instance containing covering range of the alignment."
  ([^BAMRawBlock block]
   (let [buffer          (bb/make-lsb-byte-buffer (.data block))
         ref-id          (.getInt buffer)
         pos             (inc (.getInt buffer))
         l-read-name     (short (bb/read-ubyte buffer))
         _               (bb/skip buffer 3) ;; MAPQ, bin
         n-cigar-op      (int (bb/read-ushort buffer))
         _               (bb/skip buffer (+ 18 l-read-name)) ;; flag, l_seq, rnext, pnext, tlen, qname
         cigar-bytes     (bb/read-bytes buffer (* n-cigar-op 4))
         ref-length      (cigar/count-ref-bytes cigar-bytes)
         ref-end         (int (if (zero? ref-length) pos (dec (+ pos ref-length))))]
     (SAMRegionBlock. (.data block) ref-id pos ref-end)))
  ([^BAMRawBlock block ^long start ^long end]
   (let [buffer          (bb/make-lsb-byte-buffer (.data block))
         ref-id          (.getInt buffer)
         pos             (inc (.getInt buffer))]
     (when (<= pos end)
       (let [l-read-name (short (bb/read-ubyte buffer))
             _           (bb/skip buffer 3) ;; MAPQ, bin
             n-cigar-op  (int (bb/read-ushort buffer))
             _           (bb/skip buffer (+ 18 l-read-name)) ;; flag, l_seq, rnext, pnext, tlen, qname
             cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
             ref-length  (cigar/count-ref-bytes cigar-bytes)
             ref-end     (if (zero? ref-length) pos (dec (+ pos ref-length)))]
         (when (<= start ref-end)
           (SAMRegionBlock. (.data block) ref-id pos ref-end)))))))

(defn decode-coordinate-block
  "Decodes BAM block and returns a SAMCoordinateBlock instance containing ref-id, pos and flag."
  ([^BAMRawBlock block]
   (let [buffer     (bb/make-lsb-byte-buffer (.data block))
         ref-id     (.getInt buffer)
         pos        (inc (.getInt buffer))
         _          (bb/skip buffer 6) ;; l_read_name, MAPQ, bin, n_cigar_op
         flag       (int (bb/read-ushort buffer))] ;; l_seq, rnext, pnext, tlen, qname
     (SAMCoordinateBlock. (.data block) (int ref-id) (int pos) (int flag))))
  ([^BAMRawBlock block ^long start ^long end]
   (let [buffer          (bb/make-lsb-byte-buffer (.data block))
         ref-id          (.getInt buffer)
         pos             (inc (.getInt buffer))]
     (when (<= pos end)
       (let [l-read-name (short (bb/read-ubyte buffer))
             _           (bb/skip buffer 3) ;; MAPQ, bin
             n-cigar-op  (int (bb/read-ushort buffer))
             flag        (int (bb/read-ushort buffer))
             _           (bb/skip buffer (+ 16 l-read-name)) ;; l_seq, rnext, pnext, tlen, qname
             cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
             ref-length  (cigar/count-ref-bytes cigar-bytes)
             ref-end     (if (zero? ref-length) pos (dec (+ pos ref-length)))]
         (when (<= start ref-end)
           (SAMCoordinateBlock. (.data block) (int ref-id) (int pos) (int flag))))))))

(defn decode-queryname-block
  "Decodes BAM block and returns a SAMQuerynameBlock instance containing qname and flag."
  ([^BAMRawBlock block]
   (let [buffer      (bb/make-lsb-byte-buffer (.data block))
         _           (bb/skip buffer 8) ;; ref-id, pos
         l-read-name (short (bb/read-ubyte buffer))
         _           (bb/skip buffer 5) ;; MAPQ, bin, n_cigar_op
         flag        (int (bb/read-ushort buffer))
         _           (bb/skip buffer 16) ;; l_seq, rnext, pnext, tlen
         qname       (bb/read-string buffer l-read-name)]
     (SAMQuerynameBlock. (.data block) qname (int flag))))
  ([^BAMRawBlock block ^long start ^long end]
   (let [buffer          (bb/make-lsb-byte-buffer (.data block))
         _               (bb/skip buffer 4) ;; ref-id
         pos             (inc (.getInt buffer))]
     (when (<= pos end)
       (let [l-read-name (short (bb/read-ubyte buffer))
             _           (bb/skip buffer 3) ;; MAPQ, bin
             n-cigar-op  (int (bb/read-ushort buffer))
             flag        (int (bb/read-ushort buffer))
             _           (bb/skip buffer 16) ;; l_seq, rnext, pnext, tlen
             qname       (bb/read-string buffer l-read-name)
             cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
             ref-length  (cigar/count-ref-bytes cigar-bytes)
             ref-end     (if (zero? ref-length) pos (dec (+ pos ref-length)))]
         (when (<= start ref-end)
           (SAMQuerynameBlock. (.data block) qname (int flag))))))))

(defrecord BAMPointerBlock [data ^int ref-id ^int pos ^int end ^int flag ^long pointer-beg ^long pointer-end])
(defn decode-pointer-block
  "Decodes BAM block and returns a BAMPointerBlock instance containing region, flag and block pointers."
  ([^BAMRawBlock block]
   (let [buffer      (bb/make-lsb-byte-buffer (.data block))
         ref-id      (.getInt buffer)
         pos         (inc (.getInt buffer))
         l-read-name (short (bb/read-ubyte buffer))
         _           (bb/skip buffer 3) ;; MAPQ, bin
         n-cigar-op  (int (bb/read-ushort buffer))
         flag        (int (bb/read-ushort buffer))
         _           (bb/skip buffer (+ 16 l-read-name)) ;; l_seq, rnext, pnext, tlen, qname
         cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
         ref-length  (cigar/count-ref-bytes cigar-bytes)
         ref-end     (if (zero? ref-length) pos (dec (+ pos ref-length)))]
     (BAMPointerBlock. (.data block) ref-id pos ref-end (int flag) (.pointer-beg block) (.pointer-end block))))
  ([^BAMRawBlock block ^long start ^long end]
   (let [buffer          (bb/make-lsb-byte-buffer (.data block))
         ref-id          (.getInt buffer)
         pos             (inc (.getInt buffer))]
     (when (<= pos end)
       (let [l-read-name (short (bb/read-ubyte buffer))
             _           (bb/skip buffer 3) ;; MAPQ, bin
             n-cigar-op  (int (bb/read-ushort buffer))
             flag        (int (bb/read-ushort buffer))
             _           (bb/skip buffer (+ 16 l-read-name)) ;; l_seq, rnext, pnext, tlen, qname
             cigar-bytes (bb/read-bytes buffer (* n-cigar-op 4))
             ref-length  (cigar/count-ref-bytes cigar-bytes)
             ref-end     (if (zero? ref-length) pos (dec (+ pos ref-length)))]
         (when (<= start ref-end)
           (BAMPointerBlock. (.data block) ref-id pos ref-end (int flag) (.pointer-beg block) (.pointer-end block))))))))

(defn raw-block
  "Checks the range of BAM block and returns the given block if any base is included."
  ([b] b)
  ([b ^long s ^long e]
   (when (decode-region-block b s e)
     b)))
