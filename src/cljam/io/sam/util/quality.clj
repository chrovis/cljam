(ns cljam.io.sam.util.quality
  "Utility functions for phred quality strings."
  (:import [java.nio Buffer ByteBuffer CharBuffer]))

(definline fastq-char->phred-byte
  "Converts fastq-char to phred-byte."
  [ch]
  `(byte (- (int ~ch) 33)))

(definline phred-byte->fastq-char
  "Converts phred-byte to fastq-char."
  [b]
  `(unchecked-char (unchecked-add (Math/min 93 (Math/max (long ~b) 0)) 33)))

(defn fastq->phred
  "Converts fastq quality string to phred bytes."
  ^bytes [^String fastq]
  (let [b (.getBytes fastq)
        in-bb (ByteBuffer/wrap b)
        out-bb (ByteBuffer/allocate (alength b))]
    (while (.hasRemaining in-bb)
      (.put out-bb (fastq-char->phred-byte (.get in-bb))))
    (.array out-bb)))

(defmulti phred->fastq
  "Converts phred bytes or a byte to fastq quality string or char."
  class)

(defn phred-bytes->fastq
  "Converts phred bytes to fastq quality string."
  [^bytes b]
  (when-not (nil? b)
    (let [bb (ByteBuffer/wrap b)
          cb (CharBuffer/allocate (alength b))]
      (loop []
        (when (.hasRemaining bb)
          (.put cb (phred-byte->fastq-char (.get bb)))
          (recur)))
      (.flip ^Buffer cb)
      (.toString cb))))

(defmethod phred->fastq (class (byte-array nil))
  [^bytes b]
  (phred-bytes->fastq b))

(def ^:private ^:const max-phred-score 93)

(defmethod phred->fastq Integer
  [n]
  {:pre [(<= 0 n max-phred-score)]}
  (phred-byte->fastq-char n))
