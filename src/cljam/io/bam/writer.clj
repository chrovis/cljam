(ns cljam.io.bam.writer
  "Writer of BAM file format."
  (:require [cljam.io.protocols :as protocols]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.bam.common :as common]
            [cljam.io.bam.encoder :as encoder])
  (:import [java.io Closeable]))

(declare write-header* write-refs* write-alignments* write-blocks*)

;;
;; BAMWriter
;;

(deftype BAMWriter [f writer]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-path [this]
    (.f this))
  protocols/IAlignmentWriter
  (write-header [this header]
    (write-header* this header))
  (write-refs [this header]
    (write-refs* this header))
  (write-alignments [this alignments header]
    (write-alignments* this alignments header))
  (write-blocks [this blocks]
    (write-blocks* this blocks)))

;;
;; write
;;

(defn write-header* [^BAMWriter wtr header]
  (let [w (.writer wtr)
        header-string (str (sam-util/stringify-header header) \newline)]
    (lsb/write-bytes w (.getBytes ^String common/bam-magic)) ; magic
    (lsb/write-int w (count header-string))
    (lsb/write-string w header-string)))

(defn write-refs* [^BAMWriter wtr header]
  (let [w (.writer wtr)
        refs (sam-util/make-refs header)]
    (lsb/write-int w (count refs))
    (doseq [ref refs]
      (lsb/write-int w (inc (count (:name ref))))
      (lsb/write-string w (:name ref))
      (lsb/write-bytes w (byte-array 1 (byte 0)))
      (lsb/write-int w (:len ref)))))

(defn write-alignments* [^BAMWriter wtr alns header]
  (let [w (.writer wtr)
        refs (sam-util/make-refs header)]
    (doseq [a alns]
      (lsb/write-int w (encoder/get-block-size a))
      (encoder/encode-alignment w refs a))))

(defn write-blocks* [^BAMWriter wtr blocks]
  (let [w (.writer wtr)]
    (doseq [b blocks]
      (lsb/write-int w (alength ^bytes (:data b)))
      (lsb/write-bytes w (:data b)))))

(defn write-blocks-rf
  "Returns a reducing function which writes BAM blocks to the given writer."
  [^BAMWriter writer header]
  (let [w (.writer writer)]
    (fn write-blocks-rf-rf
      ([]
       (write-header* writer header)
       (write-refs* writer header))
      ([_])
      ([_ input]
       (lsb/write-int w (alength ^bytes (:data input)))
       (lsb/write-bytes w (:data input))))))

(defn write-alignments-rf
  "Returns a reducing function which writes alignments to the given BAM writer."
  [writer header]
  (let [refs (sam-util/make-refs header)]
    ((map (partial encoder/encode-alignment refs))
     (write-blocks-rf writer header))))

(defn write-blocks-xf
  "Returns a transducer which writes BAM blocks to the given writer as
  side-effects. Note that this function immediately writes header and reference
  info when invoked to prevent them being written multiple times."
  [^BAMWriter writer header]
  (let [w (.writer writer)]
    (write-header* writer header)
    (write-refs* writer header)
    (fn write-blocks-xf-xf [rf]
      (fn write-blocks-xf-xf-rf
        ([]
         (rf))
        ([result]
         (rf result))
        ([result input]
         (lsb/write-int w (alength ^bytes (:data input)))
         (lsb/write-bytes w (:data input))
         (rf result input))))))

(defn write-alignments-xf
  "Returns a stateful transducer which writes alignments to the given BAM
  writer as side-effects. Note that this function immediately writes
  header and reference info when invoked to prevent them being written multiple
  times."
  [writer header]
  (let [refs (sam-util/make-refs header)]
    (comp
     (map (partial encoder/encode-alignment refs))
     (write-blocks-xf writer header))))
