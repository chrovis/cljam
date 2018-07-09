(ns cljam.io.sam.util.quality
  "Utility functions for phred quality strings."
  (:import [java.nio ByteBuffer CharBuffer]))

(definline fastq-char->phred-byte [ch]
  `(byte (- (int ~ch) 33)))

(definline phred-byte->fastq-char [b]
  `(unchecked-char (unchecked-add ~b 33)))

(defn fastq->phred ^bytes [^String fastq]
  (let [b (.getBytes fastq)
        in-bb (ByteBuffer/wrap b)
        out-bb (ByteBuffer/allocate (alength b))]
    (while (.hasRemaining in-bb)
      (.put out-bb (fastq-char->phred-byte (.get in-bb))))
    (.array out-bb)))

(defmulti phred->fastq class)

(defn phred-bytes->fastq
  [^bytes b]
  (when-not (nil? b)
    (let [bb (ByteBuffer/wrap b)
          cb (CharBuffer/allocate (alength b))]
      (loop []
        (when (.hasRemaining bb)
          (.put cb (phred-byte->fastq-char (.get bb)))
          (recur)))
      (.flip cb)
      (.toString cb))))

(defmethod phred->fastq (class (byte-array nil))
  [^bytes b]
  (phred-bytes->fastq b))

(def ^:const max-phred-score 93)

(defmethod phred->fastq Integer
  [n]
  {:pre [(<= 0 n max-phred-score)]}
  (phred-byte->fastq-char n))
