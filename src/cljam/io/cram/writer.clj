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

(defn- stats->header-base [{:keys [ri ^long start ^long end nbases nrecords]}]
  {:ref-seq-id ri
   :start start
   :span (if (zero? start) 0 (inc (- end start)))
   :bases nbases
   :records nrecords})

(defn- generate-slice
  [^CRAMWriter wtr counter tag-dict subst-mat tag-encodings slice-records]
  (let [ds-encoders (ds/build-data-series-encoders ds/default-data-series-encodings)
        tag-encoders (ds/build-tag-encoders tag-encodings)
        stats (record/encode-slice-records (.-seq-resolver wtr) (.-header wtr)
                                           tag-dict subst-mat
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

(defn- generate-slices
  [wtr counter tag-dict subst-mat tag-encodings container-records]
  (loop [[slice-records & more] container-records, counter counter, acc []]
    (if slice-records
      (let [slice (generate-slice wtr counter tag-dict subst-mat tag-encodings slice-records)]
        (recur more (:counter slice) (conj acc slice)))
      acc)))

(defn- generate-container-header [^CRAMWriter wtr ^bytes compression-header-block slices]
  (let [stats (->> slices
                   (map :stats)
                   (stats/merge-stats (count (:SQ (.-header wtr)))))
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

(defn- write-container [^CRAMWriter wtr counter container-records]
  (let [^OutputStream out (.-stream wtr)
        ds-encodings ds/default-data-series-encodings
        subst-mat {\A {\T 0, \G 1, \C 2, \N 3}
                   \T {\A 0, \G 1, \C 2, \N 3}
                   \G {\A 0, \T 1, \C 2, \N 3}
                   \C {\A 0, \T 1, \G 2, \N 3}
                   \N {\A 0, \T 1, \G 2, \C 3}}
        tag-dict (build-tag-dictionary container-records)
        tag-encodings (build-tag-encodings tag-dict)
        slices (generate-slices wtr counter tag-dict subst-mat tag-encodings container-records)
        compression-header-block (struct/generate-compression-header-block
                                  {:RN true, :AP false, :RR true}
                                  subst-mat tag-dict ds-encodings tag-encodings)
        container-header (generate-container-header wtr compression-header-block slices)
        counter' (:counter (peek slices))]
    (struct/encode-container-header out (assoc container-header :counter counter))
    (.write out compression-header-block)
    (run! (fn [{:keys [^bytes header-block data-blocks]}]
            (.write out header-block)
            (run! #(.write out ^bytes %) data-blocks))
          slices)
    counter'))

(defn- partition-alignments [slices-per-container records-per-slice alns]
  (->> alns
       (partition-all records-per-slice)
       (map object-array)
       (partition-all slices-per-container)
       (map object-array)))

(defn write-alignments
  "TODO"
  [wtr alns _]
  (reduce (partial write-container wtr) 0 (partition-alignments 1 10000 alns)))
