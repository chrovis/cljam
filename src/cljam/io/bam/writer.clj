(ns cljam.io.bam.writer
  "Writer of BAM file format."
  (:require [clojure.java.io :as cio]
            [cljam.io.protocols :as protocols]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sam.util.header :as header]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.bam.common :as common]
            [cljam.io.bam.encoder :as encoder]
            [cljam.io.bam.decoder :as bam-decoder]
            [cljam.io.bam-index.writer :as bai-writer])
  (:import [java.io ByteArrayOutputStream Closeable DataOutputStream]
           [cljam.io.bam.decoder BAMRawBlock]
           [bgzf4j BGZFOutputStream]))

(declare write-header* write-refs* write-alignments* write-blocks*)

(defn- sort-by-pos?
  "Returns true if the bam is sorted by coordinate, false if not.
   It is detected by`@HD SO:coordinate` tag in the header."
  [header]
  (let [so (:SO (:HD header))]
    (= so (name :coordinate))))

;;
;; BAMWriter
;;

(deftype BAMWriter [url writer data-writer refs index]
  Closeable
  (close [this]
    (.close ^Closeable (.data-writer this))
    (when-let [idx @index]
      (let [last-pointer (.getFilePointer ^BGZFOutputStream writer)
            n-refs (count @refs)]
        (with-open [w (->> (str url ".bai")
                           cio/output-stream
                           DataOutputStream.)]
          (->> (bai-writer/update-last-pointer idx last-pointer)
               (bai-writer/finalize-index n-refs)
               (bai-writer/write-index*! w n-refs))))))
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
  (swap! (.index wtr) #(and (sort-by-pos? header) %))
  (let [w (.data-writer wtr)
        header-string (str (header/stringify-header header) \newline)]
    (lsb/write-bytes w (.getBytes ^String common/bam-magic)) ; magic
    (lsb/write-int w (count header-string))
    (lsb/write-string w header-string)))

(defn write-refs* [^BAMWriter wtr header]
  (let [w (.data-writer wtr)
        refs (refs/make-refs header)]
    (when @(.index wtr)
      (reset! (.refs wtr) refs))
    (lsb/write-int w (count refs))
    (doseq [ref refs]
      (lsb/write-int w (inc (count (:name ref))))
      (lsb/write-string w (:name ref))
      (lsb/write-bytes w (byte-array 1 (byte 0)))
      (lsb/write-int w (:len ref)))))

(defn write-alignments* [^BAMWriter wtr alns header]
  (let [dw (.data-writer wtr)
        w ^BGZFOutputStream (.writer wtr)
        refs (refs/make-refs header)]
    (with-open [baos (ByteArrayOutputStream. 4096)
                dos (DataOutputStream. baos)]
      (let [pointer-block (map
                           (fn [a]
                             (let [pointer-beg (.getFilePointer w)]
                               (.reset baos)
                               (encoder/encode-alignment dos a refs)
                               (lsb/write-int dw (.size baos))
                               (.writeTo baos dw)
                               (when @(.index wtr)
                                 (bam-decoder/decode-pointer-block
                                  (BAMRawBlock. (.toByteArray baos)
                                                pointer-beg
                                                (.getFilePointer w))))))
                           alns)]
        (if @(.index wtr)
          (reset! (.index wtr)
                  (->> pointer-block
                       bai-writer/make-index*
                       (bai-writer/merge-index @(.index wtr))))
          (dorun pointer-block))
        nil))))

(defn write-blocks* [^BAMWriter wtr blocks]
  (let [dw (.data-writer wtr)
        w ^BGZFOutputStream (.writer wtr)
        pointer-block (map
                       (fn [{:keys [data]}]
                         (let [pointer-beg (.getFilePointer w)]
                           (lsb/write-int dw (alength ^bytes data))
                           (lsb/write-bytes dw data)
                           (when @(.index wtr)
                             (bam-decoder/decode-pointer-block
                              (BAMRawBlock. data
                                            pointer-beg
                                            (.getFilePointer w))))))
                       blocks)]
    (if @(.index wtr)
      (reset! (.index wtr)
              (->> pointer-block
                   bai-writer/make-index*
                   (bai-writer/merge-index @(.index wtr))))
      (dorun pointer-block))
    nil))
