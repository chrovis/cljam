(ns cljam.io.cram.writer
  (:require [cljam.io.crai :as crai]
            [cljam.io.cram.encode.alignment-stats :as stats]
            [cljam.io.cram.encode.context :as context]
            [cljam.io.cram.encode.partitioning :as partition]
            [cljam.io.cram.encode.record :as record]
            [cljam.io.cram.encode.structure :as struct]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.protocols :as protocols]
            [cljam.io.sam.util.header :as sam.header])
  (:import [java.io Closeable DataOutputStream OutputStream]
           [java.security MessageDigest]))

(declare write-header write-alignments)

(deftype CRAMWriter [url stream seq-resolver index-writer options]
  Closeable
  (close [_]
    (struct/encode-eof-container stream)
    (.close ^OutputStream stream)
    (when seq-resolver
      (.close ^Closeable seq-resolver))
    (when index-writer
      (.close ^Closeable index-writer)))
  protocols/IWriter
  (writer-url [_] url)
  protocols/IAlignmentWriter
  (write-header [this header]
    (write-header this header))
  (write-refs [_ _])
  (write-alignments [this alignments header]
    (write-alignments this alignments header)
    nil)
  #_(write-blocks [this blocks]))

(defn write-file-definition
  "Writes the CRAM file definition."
  [^CRAMWriter wtr file-id]
  (struct/encode-file-definition (.-stream wtr) {:major 3 :minor 1} file-id))

(defn write-header
  "Writes the CRAM header."
  [^CRAMWriter wtr header]
  (when (and (.-index-writer wtr)
             (not (:skip-sort-order-check? (.-options wtr)))
             (not= (sam.header/sort-order header) sam.header/order-coordinate))
    (throw
     (ex-info "Cannot create CRAM index file for CRAM file not declared as sorted by coordinate"
              {:sort-order (sam.header/sort-order header)})))
  (struct/encode-cram-header-container (.-stream wtr) header))

(defn- preprocess-records
  [cram-header seq-resolver options ^objects container-records]
  (let [container-ctx (context/make-container-context cram-header seq-resolver)
        {:keys [ds-compressor-overrides tag-compressor-overrides]} options
        stats (mapv (partial record/preprocess-slice-records container-ctx)
                    container-records)]
    (context/finalize-container-context container-ctx
                                        stats
                                        ds-compressor-overrides
                                        tag-compressor-overrides)))

(defn- generate-blocks [slice-ctx]
  (->> (context/encoding-results slice-ctx)
       (keep (fn [{:keys [^long raw-size] :as block}]
               (when (pos? raw-size)
                 (update block :data
                         (fn [^bytes data]
                           (struct/generate-block (:compressor block) 4
                                                  (:content-id block) raw-size
                                                  data))))))
       ;; sort + dedupe by :content-id
       (into (sorted-map) (map (juxt :content-id identity)))
       vals
       (cons {:content-id 0
              :data (struct/generate-block :raw 5 0 0 (byte-array 0))})))

(defn- reference-md5
  [{:keys [seq-resolver cram-header]} {:keys [^long ri ^long start ^long end]}]
  (if (neg? ri)
    (byte-array 16)
    (let [chr (:SN (nth (:SQ cram-header) ri))
          ref-bases (resolver/resolve-sequence seq-resolver chr start end)
          md5 (MessageDigest/getInstance "md5")]
      (.digest md5 ref-bases))))

(defn- stats->header-base [{:keys [ri ^long start ^long end nbases nrecords]}]
  {:ref-seq-id ri
   :start start
   :span (if (zero? start) 0 (inc (- end start)))
   :bases nbases
   :records nrecords})

(defn- generate-slice [slice-ctx counter slice-records]
  (record/encode-slice-records slice-ctx slice-records)
  (let [stats (:alignment-stats slice-ctx)
        blocks (generate-blocks slice-ctx)
        ref-md5 (reference-md5 slice-ctx stats)
        header (assoc (stats->header-base stats)
                      :counter counter
                      :embedded-reference -1
                      :reference-md5 ref-md5)
        header-block (struct/generate-slice-header-block header blocks)
        block-data (mapv :data blocks)]
    {:header header
     :header-block header-block
     :data-blocks block-data
     :counter (+ (long counter) (long (:nrecords stats)))
     :size (->> block-data
                (map #(alength ^bytes %))
                (apply + (alength header-block)))}))

(defn- generate-slices [container-ctx counter container-records]
  (loop [i 0, [slice-records & more] container-records, counter counter, acc []]
    (if slice-records
      (let [slice-ctx (context/make-slice-context container-ctx i)
            slice (generate-slice slice-ctx counter slice-records)]
        (recur (inc i) more (:counter slice) (conj acc slice)))
      acc)))

(defn- generate-compression-header-block
  ^bytes [{:keys [preservation-map subst-mat tag-dict ds-encodings tag-encodings]}]
  (struct/generate-compression-header-block preservation-map subst-mat tag-dict
                                            ds-encodings tag-encodings))

(defn- generate-container-header [container-ctx ^bytes compression-header-block slices]
  (let [stats (stats/merge-stats (:alignment-stats container-ctx))
        container-len (->> slices
                           (map (fn [{:keys [^bytes header-block data-blocks]}]
                                  (apply + (alength header-block)
                                         (map #(alength ^bytes %) data-blocks))))
                           (apply + (alength compression-header-block)))
        landmarks (->> slices
                       (reductions #(+ (long %1) (long (:size %2)))
                                   (alength compression-header-block))
                       butlast)]
    (assoc (stats->header-base stats)
           :length container-len
           :blocks (->> slices
                        (map (comp inc count :data-blocks))
                        (apply + 1))
           :landmarks landmarks)))

(defn- slice-index-entries [slice-header slice-records]
  (if (= (:ref-seq-id slice-header) -2)
    (let [spans-builder (stats/make-alignment-spans-builder)]
      (run! (fn [record]
              (stats/update-span! spans-builder (::record/ref-index record)
                                  (:pos record) (::record/end record)))
            slice-records)
      (mapv (fn [[ri {:keys [start span]}]]
              {:ref-seq-id ri, :start start, :span span})
            (stats/build-spans spans-builder)))
    [(select-keys slice-header [:ref-seq-id :start :span])]))

(defn- container-index-entries
  [container-offset container-header slices container-records]
  (for [[offset {:keys [header size]} records] (map vector
                                                    (:landmarks container-header)
                                                    slices
                                                    container-records)
        entry (slice-index-entries header records)]
    (assoc entry
           :container-offset container-offset
           :slice-offset offset
           :size size)))

(defn- write-index-entries
  [index-writer container-offset container-header slices container-records]
  (let [entries (container-index-entries container-offset container-header
                                         slices container-records)]
    (crai/write-index-entries index-writer entries)))

(defn- write-container [^CRAMWriter wtr cram-header counter container-records]
  (let [container-ctx (preprocess-records cram-header (.-seq-resolver wtr)
                                          (.-options wtr) container-records)
        slices (generate-slices container-ctx counter container-records)
        compression-header-block (generate-compression-header-block container-ctx)
        container-header (generate-container-header container-ctx
                                                    compression-header-block
                                                    slices)
        ^DataOutputStream out (.-stream wtr)
        container-offset (.size out)]
    (struct/encode-container-header out (assoc container-header :counter counter))
    (.write out compression-header-block)
    (run! (fn [{:keys [^bytes header-block data-blocks]}]
            (.write out header-block)
            (run! #(.write out ^bytes %) data-blocks))
          slices)
    (when-let [index-writer (.-index-writer wtr)]
      (write-index-entries index-writer container-offset container-header
                           slices container-records))))

(defn write-alignments
  "Writes all the given alignments, which is a sequence of alignment maps."
  [^CRAMWriter wtr alns header]
  (partition/with-each-container header (.-options wtr) alns
    (fn [counter container-records]
      (write-container wtr header counter container-records))))
