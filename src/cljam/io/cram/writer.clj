(ns cljam.io.cram.writer
  (:require [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.record :as record]
            [cljam.io.cram.encode.stats :as stats]
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

(defn- reference-md5 [^CRAMWriter wtr {:keys [^long ri ^long start ^long end]}]
  (if (neg? ri)
    (byte-array 16)
    (let [chr (:SN (nth (:SQ (.-header wtr)) ri))
          ref-bases (resolver/resolve-sequence (.-seq-resolver wtr) chr start end)
          md5 (MessageDigest/getInstance "md5")]
      (.digest md5 ref-bases))))

(defn- partition-alignments [slices-per-container records-per-slice alns]
  (->> alns
       (partition-all records-per-slice)
       (partition-all slices-per-container)))

(defn- stats->header-base [{:keys [ri ^long start ^long end nbases nrecords]}]
  {:ref-seq-id ri
   :start start
   :span (if (zero? start) 0 (inc (- end start)))
   :bases nbases
   :records nrecords})

(defn- generate-slice [^CRAMWriter wtr counter alns]
  (let [ds-encoders (ds/build-data-series-encoders ds/default-data-series-encodings)
        stats (record/encode-slice-records (.-header wtr) ds-encoders {} alns)
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
        header-block (struct/generate-slice-header-block
                      (assoc (stats->header-base stats)
                             :counter counter
                             :embedded-reference -1
                             :reference-md5 ref-md5)
                      blocks)
        block-data (mapv :data blocks)]
    {:header-block header-block
     :data-blocks block-data
     :stats stats
     :counter (+ (long counter) (long (:nrecords stats)))
     :size (->> block-data
                (map #(alength ^bytes %))
                (apply + (alength header-block)))}))

(defn- generate-slices [wtr counter slices]
  (loop [[slice & more] slices, counter counter, acc []]
    (if slice
      (let [slice' (generate-slice wtr counter slice)]
        (recur more (:counter slice') (conj acc slice')))
      acc)))

(defn- write-container [^CRAMWriter wtr counter slices]
  (let [^OutputStream out (.-stream wtr)
        ds-encodings ds/default-data-series-encodings
        slices' (generate-slices wtr counter slices)
        compression-header-block (struct/generate-compression-header-block
                                  {:RN true, :AP false, :RR true}
                                  {\A {\T 0, \G 1, \C 2, \N 3}
                                   \T {\A 0, \G 1, \C 2, \N 3}
                                   \G {\A 0, \T 1, \C 2, \N 3}
                                   \C {\A 0, \T 1, \G 2, \N 3}
                                   \N {\A 0, \T 1, \G 2, \C 3}}
                                  [[]] ds-encodings {})
        stats (->> slices'
                   (map :stats)
                   (stats/merge-stats (count (:SQ (.-header wtr)))))
        container-len (->> slices'
                           (map (fn [{:keys [^bytes header-block data-blocks]}]
                                  (apply + (alength header-block)
                                         (map #(alength ^bytes %) data-blocks))))
                           (apply + (alength compression-header-block)))
        landmarks (->> slices'
                       (reductions #(+ (long %1) (long (:size %2)))
                                   (alength compression-header-block))
                       butlast)
        container-header (assoc (stats->header-base stats)
                                :counter counter
                                :length container-len
                                :blocks (->> slices'
                                             (map (comp inc count :data-blocks))
                                             (apply + 1))
                                :landmarks landmarks)
        counter' (:counter (peek slices'))]
    (struct/encode-container-header out container-header)
    (.write out compression-header-block)
    (run! (fn [{:keys [^bytes header-block data-blocks]}]
            (.write out header-block)
            (run! #(.write out ^bytes %) data-blocks))
          slices')
    counter'))

(defn write-alignments
  "TODO"
  [wtr alns _]
  (reduce (partial write-container wtr) 0 (partition-alignments 1 10000 alns)))
