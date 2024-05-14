(ns cljam.io.cram.reader
  (:require [cljam.io.cram.bit-stream :as bs]
            [cljam.io.cram.decode.data-series :as ds]
            [cljam.io.cram.decode.record :as record]
            [cljam.io.cram.decode.structure :as struct]
            [cljam.io.protocols :as protocols]
            [cljam.util.intervals :as intervals])
  (:import [java.io Closeable]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]))

(declare read-alignments read-alignments-in-region)

(deftype CRAMReader [url channel buffer header refs index seq-resolver]
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

(defn- read-slice-records [^CRAMReader rdr bb compression-header slice-header]
  (let [blocks (into [] (map (fn [_] (struct/decode-block bb)))
                     (range (:blocks slice-header)))
        core-block (first (filter #(zero? (long (:content-id %))) blocks))
        bs-decoder (when core-block
                     (bs/make-bit-stream-decoder (:data core-block)))
        ds-decoders (ds/build-data-series-decoders compression-header bs-decoder blocks)
        tag-decoders (ds/build-tag-decoders compression-header bs-decoder blocks)]
    (record/decode-slice-records (.-seq-resolver rdr)
                                 @(.-header rdr)
                                 compression-header
                                 slice-header
                                 ds-decoders
                                 tag-decoders)))

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
      (filter-overlapping-records chr start end (step)))))

(defn- read-alignments-in-region
  [^CRAMReader rdr {:keys [chr ^long start ^long end] :or {start 0 end Long/MAX_VALUE}}]
  (if @(.-index rdr)
    (read-alignments-in-region-with-index rdr chr start end)
    (read-alignments-in-region-without-index rdr chr start end)))
