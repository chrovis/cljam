(ns cljam.io.cram.encode.compressor
  (:import [java.io ByteArrayOutputStream OutputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorOutputStream]
           [org.apache.commons.compress.compressors.gzip GzipCompressorOutputStream]
           [org.apache.commons.compress.compressors.xz XZCompressorOutputStream]
           [org.apache.commons.io.output CountingOutputStream]))

(defprotocol ICompressor
  (compressor-output-stream [this])
  (->compressed-result [this]))

(deftype RawCompressor [^ByteArrayOutputStream out]
  ICompressor
  (compressor-output-stream [_] out)
  (->compressed-result [_]
    {:compressor :raw, :data (.toByteArray out)}))

(deftype GzipCompressor
         [^GzipCompressorOutputStream out ^ByteArrayOutputStream baos]
  ICompressor
  (compressor-output-stream [_] out)
  (->compressed-result [_]
    (.finish out)
    {:compressor :gzip, :data (.toByteArray baos)}))

(deftype BZip2Compressor
         [^BZip2CompressorOutputStream out ^ByteArrayOutputStream baos]
  ICompressor
  (compressor-output-stream [_] out)
  (->compressed-result [_]
    (.finish out)
    {:compressor :bzip, :data (.toByteArray baos)}))

(deftype LZMACompressor
         [^XZCompressorOutputStream out ^ByteArrayOutputStream baos]
  ICompressor
  (compressor-output-stream [_] out)
  (->compressed-result [_]
    (.finish out)
    {:compressor :lzma, :data (.toByteArray baos)}))

(defn- compress-with [f ^bytes uncompressed]
  (let [out (ByteArrayOutputStream.)]
    (with-open [^OutputStream os (f out)]
      (.write os uncompressed))
    (.toByteArray out)))

(deftype SelectiveCompressor [^ByteArrayOutputStream out alternatives]
  ICompressor
  (compressor-output-stream [_] out)
  (->compressed-result [_]
    (let [uncompressed (.toByteArray out)
          [k v] (->> alternatives
                     (into {}
                           (map (fn [method]
                                  [method
                                   (case method
                                     :raw uncompressed
                                     :gzip (compress-with #(GzipCompressorOutputStream. %)
                                                          uncompressed)
                                     :bzip (compress-with #(BZip2CompressorOutputStream. %)
                                                          uncompressed)
                                     :lzma (compress-with #(XZCompressorOutputStream. %)
                                                          uncompressed)
                                     (throw
                                      (ex-info (str "compression method " method
                                                    " not supported")
                                               {:method method})))])))
                     (apply min-key #(alength ^bytes (val %))))]
      {:compressor k, :data v})))

(defn compressor
  "Returns a corresponding compressor implementation depending on the specified
  compression method(s)."
  [method-or-methods]
  (let [baos (ByteArrayOutputStream.)
        compr (case method-or-methods
                :raw (->RawCompressor baos)
                :gzip (->GzipCompressor (GzipCompressorOutputStream. baos) baos)
                :bzip (->BZip2Compressor (BZip2CompressorOutputStream. baos) baos)
                :lzma (->LZMACompressor (XZCompressorOutputStream. baos) baos)
                :best (->SelectiveCompressor baos #{:raw :gzip :bzip :lzma})
                (if (set? method-or-methods)
                  (->SelectiveCompressor baos method-or-methods)
                  (throw
                   (ex-info (str "compression method " (pr-str method-or-methods)
                                 " not supported")
                            {:method method-or-methods}))))
        out (CountingOutputStream. (compressor-output-stream compr))]
    (reify ICompressor
      (compressor-output-stream [_] out)
      (->compressed-result [_]
        (let [raw-size (.getByteCount out)]
          (if (zero? raw-size)
            {:compressor :raw, :data nil, :raw-size 0}
            (assoc (->compressed-result compr) :raw-size raw-size)))))))
