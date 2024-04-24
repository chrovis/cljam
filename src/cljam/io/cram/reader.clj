(ns cljam.io.cram.reader
  (:require [cljam.io.cram.decode.data-series :as ds]
            [cljam.io.cram.decode.record :as record]
            [cljam.io.cram.decode.structure :as struct]
            [cljam.io.protocols :as protocols]
            [cljam.util.intervals :as intervals])
  (:import [java.io Closeable FileNotFoundException]
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
    (try
      @index
      true
      (catch FileNotFoundException _
        false)))
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

(defn- read-slice-records [^CRAMReader rdr bb compression-header]
  (let [slice-header (struct/decode-slice-header-block bb)
        blocks (into [] (map (fn [_] (struct/decode-block bb)))
                     (range (:blocks slice-header)))
        ds-decoders (ds/build-data-series-decoders compression-header blocks)
        tag-decoders (ds/build-tag-decoders compression-header blocks)]
    (record/decode-slice-records (.-seq-resolver rdr)
                                 @(.-header rdr)
                                 compression-header
                                 slice-header
                                 ds-decoders
                                 tag-decoders)))

(defn- read-container-records
  ([rdr bb container-header]
   (read-container-records rdr bb container-header nil))
  ([^CRAMReader rdr ^ByteBuffer bb container-header idx-entries]
   (let [container-header-end (.position bb)
         compression-header (struct/decode-compression-header-block bb)]
     (->> (if (seq idx-entries)
            (map :slice-offset idx-entries)
            (:landmarks container-header))
          (mapcat
           (fn [^long landmark]
             (.position ^Buffer bb (+ container-header-end landmark))
             (read-slice-records rdr bb compression-header)))))))

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

(defn- read-alignments-in-region
  [^CRAMReader rdr {:keys [chr start end] :or {start 0 end Long/MAX_VALUE}}]
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
      (filter #(and (= (:rname %) chr)
                    (<= (long (:pos %)) (long end))
                    (<= (long start) (long (:end %))))
              (step offset->entries)))))
