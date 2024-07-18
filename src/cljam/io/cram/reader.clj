(ns cljam.io.cram.reader
  (:require [cljam.io.cram.bit-stream :as bs]
            [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.decode.record :as record]
            [cljam.io.cram.decode.structure :as struct]
            [cljam.io.cram.encode.alignment-stats :as stats]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.util.intervals :as intervals])
  (:import [java.io Closeable]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [java.util Arrays]))

(declare read-alignments read-alignments-in-region)

(deftype CRAMReader [url channel buffer header refs offset index seq-resolver qname-generator]
  Closeable
  (close [_]
    (when seq-resolver
      (.close ^Closeable seq-resolver))
    (.close ^FileChannel channel))
  protocols/IReader
  (reader-url [_] url)
  (read [this]
    (protocols/read-alignments this))
  (read [this region]
    (protocols/read-alignments this region))
  (indexed? [_]
    (boolean @index))
  protocols/IAlignmentReader
  (read-header [_] @header)
  (read-refs [_] @refs)
  (read-alignments [this]
    (read-alignments this))
  (read-alignments [this region]
    (read-alignments-in-region this region))
  #_(read-blocks [_])
  #_(read-blocks [_ region])
  #_(read-blocks [_ region option])
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this region _]
    (protocols/read-alignments this region)))

(defn- read-to-buffer
  ([rdr] (read-to-buffer rdr nil))
  ([^CRAMReader rdr limit]
   (let [^FileChannel ch (.-channel rdr)
         ^Buffer bb (.-buffer rdr)]
     (.clear bb)
     (.limit bb (or limit (.capacity bb)))
     (while (and (.hasRemaining bb)
                 (< (.position ch) (.size ch)))
       (.read ch ^ByteBuffer bb))
     (.flip bb))))

(defn read-file-definition
  "Reads the CRAM file definition."
  [^CRAMReader rdr]
  (read-to-buffer rdr 26)
  (struct/decode-file-definition (.-buffer rdr)))

(defn- seq-resolver-for-slice [^CRAMReader rdr slice-header blocks]
  (let [embedded-ref-content-id (long (:embedded-reference slice-header))]
    (if (>= embedded-ref-content-id 0)
      (let [ref-bases-block (->> blocks
                                 (filter #(= (:content-id %) embedded-ref-content-id))
                                 first)
            offset (long (:start slice-header))
            ^bytes bs (bb/read-bytes (:data ref-bases-block)
                                     (:raw-size ref-bases-block))]
        (reify resolver/ISeqResolver
          ;; According to the CRAM specification v3.1 §8.5, a slice with an embedded
          ;; reference must not be a multiple reference slice, so the temporary
          ;; sequence resolver here can safely ignore the passed chr
          (resolve-sequence [_ _chr start end]
            (Arrays/copyOfRange bs
                                (- (long start) offset)
                                (inc (- (long end) offset))))))
      (.-seq-resolver rdr))))

(defn- read-slice-records
  ([rdr bb compression-header slice-header]
   (read-slice-records rdr nil bb compression-header slice-header))
  ([^CRAMReader rdr seq-resolver bb compression-header slice-header]
   (let [blocks (into [] (map (fn [_] (struct/decode-block bb)))
                      (range (:blocks slice-header)))
         core-block (first (filter #(zero? (long (:content-id %))) blocks))
         bs-decoder (when core-block
                      (bs/make-bit-stream-decoder (:data core-block)))
         ds-decoders (ds/build-data-series-decoders compression-header bs-decoder blocks)
         tag-decoders (ds/build-tag-decoders compression-header bs-decoder blocks)]
     (record/decode-slice-records (or seq-resolver
                                      (seq-resolver-for-slice rdr slice-header blocks))
                                  (.-qname-generator rdr)
                                  @(.-header rdr)
                                  compression-header
                                  slice-header
                                  ds-decoders
                                  tag-decoders))))

(defn- with-each-slice-header [^ByteBuffer bb f slice-offsets]
  (let [container-header-end (.position bb)
        compression-header (struct/decode-compression-header-block bb)]
    (mapcat (fn [^long offset]
              (.position ^Buffer bb (+ container-header-end offset))
              (f compression-header (struct/decode-slice-header-block bb)))
            slice-offsets)))

(defn- read-container-records
  ([rdr bb container-header]
   (read-container-records rdr bb container-header nil))
  ([^CRAMReader rdr bb container-header idx-entries]
   (->> (if (seq idx-entries)
          (map :slice-offset idx-entries)
          (:landmarks container-header))
        (with-each-slice-header bb
          (fn [compression-header slice-header]
            (read-slice-records rdr bb compression-header slice-header))))))

(defn- with-next-container-header [^CRAMReader rdr f]
  (let [^FileChannel ch (.-channel rdr)
        pos (.position ch)
        _ (read-to-buffer rdr)
        ^ByteBuffer bb (.-buffer rdr)
        container-header (struct/decode-container-header bb)
        container-start (+ pos (.position bb))
        container-size (long (:length container-header))
        ret (f container-header container-start)]
    (.position ch (+ container-start container-size))
    ret))

(defn- read-container-with [^CRAMReader rdr f]
  (letfn [(f' [container-header container-start]
            (let [container-size (long (:length container-header))
                  ^FileChannel ch (.-channel rdr)
                  bb (-> ch
                         (.map FileChannel$MapMode/READ_ONLY container-start container-size)
                         (.order ByteOrder/LITTLE_ENDIAN))]
              (f container-header bb)))]
    (with-next-container-header rdr f')))

(defn skip-container
  "Skips the next container."
  [rdr]
  (with-next-container-header rdr (constantly nil)))

(defn read-header
  "Reads the CRAM header from the CRAM header block."
  [^CRAMReader rdr]
  (read-container-with rdr (fn [_ bb] (struct/decode-cram-header-block bb))))

(defn read-alignments
  "Reads all the alignments from the CRAM file and returns them as a lazy
  sequence of alignment maps."
  [^CRAMReader rdr]
  (let [^FileChannel ch (.-channel rdr)]
    (letfn [(read1 [container-header bb]
              (when-not (struct/eof-container? container-header)
                (read-container-records rdr bb container-header)))
            (step []
              (when (< (.position ch) (.size ch))
                (when-let [alns (read-container-with rdr read1)]
                  (concat alns (lazy-seq (step))))))]
      (step))))

(defn- filter-overlapping-records [chr ^long start ^long end alns]
  (filter #(and (= (:rname %) chr)
                (<= (long (:pos %)) end)
                (<= start (long (:end %))))
          alns))

(defn- read-alignments-in-region-with-index [^CRAMReader rdr chr ^long start ^long end]
  (let [^FileChannel ch (.-channel rdr)
        idx @(.-index rdr)
        offset->entries (->> (intervals/find-overlap-intervals idx chr start end)
                             (group-by :container-offset)
                             (into (sorted-map)))]
    (letfn [(read-fn [entries]
              (fn [container-header bb]
                (read-container-records rdr bb container-header entries)))
            (step [[[^long offset entries] & more]]
              (when offset
                (.position ch offset)
                (concat (read-container-with rdr (read-fn entries))
                        (lazy-seq (step more)))))]
      (filter-overlapping-records chr start end (step offset->entries)))))

(defn- overlaps? [container-or-slice-header refs chr start end]
  (let [seq-id (long (:ref-seq-id container-or-slice-header))]
    (case seq-id
      -1 (= chr "*")
      ;; ref-seq-id = -2 means that the container or slice contains multiple
      ;; references, and the decoder can't tell in advance what reference
      ;; it actually has in it
      -2 true
      (let [chr' (get (nth refs seq-id) :name)
            start' (long (:start container-or-slice-header))
            end' (+ start' (long (:span container-or-slice-header)))]
        (and (= chr' chr)
             (<= start' (long end))
             (<= (long start) end'))))))

(defn- read-alignments-in-region-without-index [^CRAMReader rdr chr ^long start ^long end]
  (let [^FileChannel ch (.-channel rdr)
        refs @(.-refs rdr)]
    (letfn [(read1 [container-header bb]
              (when-not (struct/eof-container? container-header)
                (if (overlaps? container-header refs chr start end)
                  (with-each-slice-header bb
                    (fn [compression-header slice-header]
                      (when (overlaps? slice-header refs chr start end)
                        (read-slice-records rdr bb compression-header slice-header)))
                    (:landmarks container-header))
                  ::skipped)))
            (step []
              (when (< (.position ch) (.size ch))
                (when-let [alns (read-container-with rdr read1)]
                  (if (identical? alns ::skipped)
                    (recur)
                    (concat alns (lazy-seq (step)))))))]
      (.position ch (long @(.-offset rdr)))
      (filter-overlapping-records chr start end (step)))))

(defn- read-alignments-in-region
  [^CRAMReader rdr {:keys [chr ^long start ^long end] :or {start 0 end Long/MAX_VALUE}}]
  (if @(.-index rdr)
    (read-alignments-in-region-with-index rdr chr start end)
    (read-alignments-in-region-without-index rdr chr start end)))

(defn- slice-index-entries
  [^CRAMReader rdr seq-resolver rname->idx bb compression-header slice-header]
  (if (= (:ref-seq-id slice-header) -2)
    (let [slice-records (read-slice-records rdr seq-resolver bb compression-header slice-header)
          spans-builder (stats/make-alignment-spans-builder)]
      (run! (fn [{:keys [rname] :as record}]
              (let [ri (if (= rname "*") -1 (get rname->idx rname))]
                (stats/update-span! spans-builder ri (:pos record) (:end record))))
            slice-records)
      (mapv (fn [[ri {:keys [start span]}]]
              {:ref-seq-id ri, :start start, :span span})
            (stats/build-spans spans-builder)))
    [(select-keys slice-header [:ref-seq-id :start :span])]))

(defn- with-each-slice-header&offset [container-header ^ByteBuffer bb f]
  (let [container-header-end (.position bb)
        compression-header (struct/decode-compression-header-block bb)
        slice-offsets (vec (:landmarks container-header))]
    (mapcat (fn [^long slice-offset ^long slice-end]
              (let [slice-start (+ container-header-end slice-offset)
                    _ (.position ^Buffer bb slice-start)
                    slice-header (struct/decode-slice-header-block bb)
                    slice-size (- slice-end slice-offset)]
                (f compression-header slice-header slice-offset slice-size)))
            slice-offsets
            (-> slice-offsets
                (conj (:length container-header))
                rest))))

;; The current implementation of the CRAM index entry generator decodes each
;; entire CRAM record for records in multiple reference slices, which requires
;; the reference file to restore the read sequence of those records although
;; it's unnecessary in theory.
;; The following stub implementation of the seq resolver allows the CRAM index
;; entry generator to decode the slice records without the limitation of requiring
;; the real reference file.
(defn- stub-seq-resolver []
  (reify resolver/ISeqResolver
    (resolve-sequence [_ _])
    (resolve-sequence [_ _ start end]
      (byte-array (inc (- (long end) (long start))) (byte (int \N))))))

(defn generate-index-entries
  "Generates CRAM index entries for the given CRAM file, returning them as a sequence
  of maps which is intended to be consumed with `cljam.io.crai/write-index-entries`."
  [^CRAMReader rdr]
  (let [^FileChannel ch (.-channel rdr)
        seq-resolver (stub-seq-resolver)
        rname->idx (into {}
                         (map-indexed (fn [i {:keys [SN]}] [SN i]))
                         (:SQ @(.-header rdr)))]
    (letfn [(read-fn [container-offset]
              (fn [container-header bb]
                (when-not (struct/eof-container? container-header)
                  (with-each-slice-header&offset container-header bb
                    (fn [compression-header slice-header slice-offset slice-size]
                      (->> (slice-index-entries rdr seq-resolver rname->idx bb
                                                compression-header slice-header)
                           (map #(assoc %
                                        :container-offset container-offset
                                        :slice-offset slice-offset
                                        :size slice-size))))))))
            (step []
              (let [container-offset (.position ch)]
                (when (< container-offset (.size ch))
                  (when-let [entries (read-container-with rdr (read-fn container-offset))]
                    (concat entries (lazy-seq (step)))))))]
      (.position ch (long @(.-offset rdr)))
      (step))))
