(ns cljam.io.cram.writer
  (:require [cljam.io.crai :as crai]
            [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.alignment-stats :as stats]
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
  [seq-resolver rname->idx subst-mat ^objects container-records]
  (dotimes [i (alength container-records)]
    (let [slice-records (aget container-records i)]
      (record/preprocess-slice-records seq-resolver rname->idx subst-mat slice-records))))

(defn- build-tag-dictionary [^objects container-records]
  (let [tags->index (volatile! {})]
    (dotimes [i (alength container-records)]
      (let [^objects slice-records (aget container-records i)]
        (dotimes [j (alength slice-records)]
          (let [record (aget slice-records j)
                tags (into []
                           (keep (fn [opt]
                                   (let [[tag m] (first opt)]
                                     (when-not (= tag :RG)
                                       [tag (first (:type m))]))))
                           (:options record))
                idx (or (get @tags->index tags)
                        (let [idx (count @tags->index)]
                          (vswap! tags->index assoc tags idx)
                          idx))]
            (aset slice-records j
                  (assoc record ::record/tags-index idx))))))
    (->> (sort-by val @tags->index)
         (mapv (fn [[tags _]]
                 (mapv (fn [[tag tag-type]]
                         {:tag tag :type tag-type})
                       tags))))))

(defn- build-tag-encoding [item]
  (letfn [(tag-id [item]
            (let [tag' (name (:tag item))]
              (bit-or (bit-shift-left (int (nth tag' 0)) 16)
                      (bit-shift-left (int (nth tag' 1)) 8)
                      (int (:type item)))))
          (tag-encoding-for-fixed-size [item size]
            {:codec :byte-array-len
             :len-encoding {:codec :huffman, :alphabet [size], :bit-len [0]}
             :val-encoding {:codec :external, :content-id (tag-id item)}})]
    (case (:type item)
      (\A \c \C) (tag-encoding-for-fixed-size item 1)
      (\s \S) (tag-encoding-for-fixed-size item 2)
      (\i \I \f) (tag-encoding-for-fixed-size item 4)
      (let [content-id (tag-id item)]
        {:codec :byte-array-len
         :len-encoding {:codec :external, :content-id content-id}
         :val-encoding {:codec :external, :content-id content-id}}))))

(defn- build-tag-encodings [tag-dict]
  (reduce
   (fn [m entry]
     (reduce
      (fn [m {tag-type :type :as item}]
        (update-in m [(:tag item) tag-type] #(or % (build-tag-encoding item))))
      m entry))
   {} tag-dict))

(defn- reference-md5 [seq-resolver cram-header {:keys [^long ri ^long start ^long end]}]
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

(defn- generate-slice
  [seq-resolver cram-header rname->idx counter tag-dict tag-encodings slice-records]
  (let [ds-encoders (ds/build-data-series-encoders ds/default-data-series-encodings)
        tag-encoders (ds/build-tag-encoders tag-encodings)
        stats (record/encode-slice-records cram-header rname->idx tag-dict
                                           ds-encoders tag-encoders slice-records)
        ds-results (mapcat #(%) (vals ds-encoders))
        tag-results (for [[_tag v] tag-encoders
                          [_type encoder] v
                          res (encoder)]
                      res)
        blocks (->> (concat ds-results tag-results)
                    (keep (fn [{:keys [content-id ^bytes data] :as block}]
                            (when (pos? (alength data))
                              (update block :data
                                      #(struct/generate-block :raw 4 content-id %)))))
                    ;; sort + dedupe by :content-id
                    (into (sorted-map) (map (juxt :content-id identity)))
                    vals
                    (cons {:content-id 0
                           :data (struct/generate-block :raw 5 0 (byte-array 0))}))
        ref-md5 (reference-md5 seq-resolver cram-header stats)
        header (assoc (stats->header-base stats)
                      :counter counter
                      :embedded-reference -1
                      :reference-md5 ref-md5)
        header-block (struct/generate-slice-header-block header blocks)
        block-data (mapv :data blocks)]
    {:header header
     :stats stats
     :header-block header-block
     :data-blocks block-data
     :counter (+ (long counter) (long (:nrecords stats)))
     :size (->> block-data
                (map #(alength ^bytes %))
                (apply + (alength header-block)))}))

(defn- generate-slices
  [seq-resolver cram-header rname->idx counter tag-dict tag-encodings container-records]
  (loop [[slice-records & more] container-records, counter counter, acc []]
    (if slice-records
      (let [slice (generate-slice seq-resolver cram-header rname->idx counter
                                  tag-dict tag-encodings slice-records)]
        (recur more (:counter slice) (conj acc slice)))
      acc)))

(defn- generate-container-header [^bytes compression-header-block slices]
  (let [stats (stats/merge-stats (map :stats slices))
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
  (let [^DataOutputStream out (.-stream wtr)
        ds-encodings ds/default-data-series-encodings
        seq-resolver (.-seq-resolver wtr)
        rname->idx (into {}
                         (map-indexed (fn [i {:keys [SN]}] [SN i]))
                         (:SQ cram-header))
        subst-mat {\A {\T 0, \G 1, \C 2, \N 3}
                   \T {\A 0, \G 1, \C 2, \N 3}
                   \G {\A 0, \T 1, \C 2, \N 3}
                   \C {\A 0, \T 1, \G 2, \N 3}
                   \N {\A 0, \T 1, \G 2, \C 3}}
        _ (preprocess-records seq-resolver rname->idx subst-mat container-records)
        tag-dict (build-tag-dictionary container-records)
        tag-encodings (build-tag-encodings tag-dict)
        slices (generate-slices seq-resolver cram-header rname->idx
                                counter tag-dict tag-encodings container-records)
        compression-header-block (struct/generate-compression-header-block
                                  {:RN true, :AP false, :RR true}
                                  subst-mat tag-dict ds-encodings tag-encodings)
        container-header (generate-container-header compression-header-block slices)
        container-offset (.size out)
        counter' (:counter (peek slices))]
    (struct/encode-container-header out (assoc container-header :counter counter))
    (.write out compression-header-block)
    (run! (fn [{:keys [^bytes header-block data-blocks]}]
            (.write out header-block)
            (run! #(.write out ^bytes %) data-blocks))
          slices)
    (when-let [index-writer (.-index-writer wtr)]
      (write-index-entries index-writer container-offset container-header
                           slices container-records))
    counter'))

(defn- partition-alignments [slices-per-container records-per-slice alns]
  (->> alns
       (partition-all records-per-slice)
       (map object-array)
       (partition-all slices-per-container)
       (map object-array)))

(defn write-alignments
  "Writes all the given alignments, which is a sequence of alignment maps."
  [wtr alns header]
  (reduce (partial write-container wtr header) 0 (partition-alignments 1 10000 alns)))
