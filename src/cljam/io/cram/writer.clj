(ns cljam.io.cram.writer
  (:require [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.record :as record]
            [cljam.io.cram.encode.structure :as struct]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.protocols :as protocols])
  (:import [java.io Closeable OutputStream]
           [java.security MessageDigest]))

(declare write-header write-alignments)

(deftype CRAMWriter [url stream header seq-resolver]
  Closeable
  (close [_]
    (struct/encode-eof-container stream)
    (.close ^OutputStream stream))
  protocols/IWriter
  (writer-url [_] url)
  protocols/IAlignmentWriter
  (write-header [this header]
    (write-header this header))
  (write-refs [_ _])
  (write-alignments [this alignments header]
    (write-alignments this alignments header))
  #_(write-blocks [this blocks]))

(defn write-file-definition
  "Writes the CRAM file definition."
  [^CRAMWriter wtr file-id]
  (struct/encode-file-definition (.-stream wtr) {:major 3 :minor 1} file-id))

(defn write-header
  "TODO"
  [^CRAMWriter wtr header]
  (struct/encode-cram-header-container (.-stream wtr) header))

(defn- reference-md5 [^CRAMWriter wtr {:keys [^long ref-seq-id ^long start ^long span]}]
  (if (neg? ref-seq-id)
    (byte-array 16)
    (let [chr (:SN (nth (:SQ (.-header wtr)) ref-seq-id))
          end (dec (+ start span))
          ref-bases (resolver/resolve-sequence (.-seq-resolver wtr) chr start end)
          md5 (MessageDigest/getInstance "md5")]
      (.digest md5 ref-bases))))

(defn write-alignments
  "TODO"
  [^CRAMWriter wtr alns header]
  (let [^OutputStream out (.-stream wtr)
        ds-encodings ds/default-data-series-encodings
        ds-encoders (ds/build-data-series-encoders ds-encodings)
        stats (record/encode-slice-records header ds-encoders {} alns)
        blocks (->> (vals ds-encoders)
                    (mapcat #(%))
                    (keep (fn [{:keys [content-id ^bytes data] :as block}]
                            (when (pos? (alength data))
                              (update block :data
                                      #(struct/generate-block :raw 4 content-id %)))))
                    (sort-by :content-id)
                    (cons {:content-id 0
                           :data (struct/generate-block :raw 5 0 (byte-array 0))}))
        ref-md5 (reference-md5 wtr stats)
        slice-header (struct/generate-slice-header-block
                      (assoc stats
                             :counter 0
                             :embedded-reference -1
                             :reference-md5 ref-md5)
                      blocks)
        compression-header (struct/generate-compression-header-block
                            {:RN true, :AP false, :RR true}
                            {\A {\T 0, \G 1, \C 2, \N 3}
                             \T {\A 0, \G 1, \C 2, \N 3}
                             \G {\A 0, \T 1, \C 2, \N 3}
                             \C {\A 0, \T 1, \G 2, \N 3}
                             \N {\A 0, \T 1, \G 2, \C 3}}
                            [[]] ds-encodings {})
        container-len (->> blocks
                           (map #(alength ^bytes (:data %)))
                           (apply +
                                  (alength compression-header)
                                  (alength slice-header)))]
    (struct/encode-container-header out
                                    (assoc stats
                                           :length container-len
                                           :counter 0
                                           :blocks (+ 2 (count blocks))
                                           :landmarks [(alength compression-header)]))
    (.write out compression-header)
    (.write out slice-header)
    (run! (fn [{:keys [data]}] (.write out ^bytes data)) blocks)))
