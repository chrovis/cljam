(ns cljam.io.cram.reader
  (:require [cljam.io.cram.decode.data-series :as ds]
            [cljam.io.cram.decode.record :as record]
            [cljam.io.cram.decode.structure :as struct]
            [cljam.io.protocols :as protocols])
  (:import [java.io Closeable]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]))

(declare read-alignments)

(deftype CRAMReader [url channel buffer header refs seq-resolver]
  Closeable
  (close [_]
    (when seq-resolver
      (.close ^Closeable seq-resolver))
    (.close ^FileChannel channel))
  protocols/IReader
  (reader-url [_] url)
  (read [this]
    (protocols/read-alignments this))
  #_(read [_ option])
  (indexed? [_] false)
  protocols/IAlignmentReader
  (read-header [_] @header)
  (read-refs [_] @refs)
  (read-alignments [this]
    (read-alignments this))
  #_(read-alignments [_ region])
  #_(read-blocks [_])
  #_(read-blocks [_ region])
  #_(read-blocks [_ region option])
  #_protocols/IRegionReader
  #_(read-in-region [_ region])
  #_(read-in-region [_ region option]))

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

(defn- read-container-records [^CRAMReader rdr ^ByteBuffer bb container-header]
  (let [container-header-end (.position bb)
        compression-header (struct/decode-compression-header-block bb)]
    (->> (:landmarks container-header)
         (mapcat
          (fn [^long landmark]
            (.position ^Buffer bb (+ container-header-end landmark))
            (read-slice-records rdr bb compression-header))))))

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
