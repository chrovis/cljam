(ns cljam.io.bam.writer
  "Writer of BAM file format."
  (:require [cljam.io.protocols :as protocols]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sam.util.header :as header]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.bam.common :as common]
            [cljam.io.bam.encoder :as encoder])
  (:import [java.io Closeable]))

(declare write-header* write-refs* write-alignments* write-blocks*)

;;
;; BAMWriter
;;

(deftype BAMWriter [url writer]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-url [this]
    (.url this))
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
        header-string (str (header/stringify-header header) \newline)]
    (lsb/write-bytes w (.getBytes ^String common/bam-magic)) ; magic
    (lsb/write-int w (count header-string))
    (lsb/write-string w header-string)))

(defn write-refs* [^BAMWriter wtr header]
  (let [w (.writer wtr)
        refs (refs/make-refs header)]
    (lsb/write-int w (count refs))
    (doseq [ref refs]
      (lsb/write-int w (inc (count (:name ref))))
      (lsb/write-string w (:name ref))
      (lsb/write-bytes w (byte-array 1 (byte 0)))
      (lsb/write-int w (:len ref)))))

(defn write-alignments* [^BAMWriter wtr alns header]
  (let [w (.writer wtr)
        refs (refs/make-refs header)]
    (doseq [a alns]
      (lsb/write-int w (encoder/get-block-size a))
      (encoder/encode-alignment w a refs))))

(defn write-blocks* [^BAMWriter wtr blocks]
  (let [w (.writer wtr)]
    (doseq [b blocks]
      (lsb/write-int w (alength ^bytes (:data b)))
      (lsb/write-bytes w (:data b)))))
