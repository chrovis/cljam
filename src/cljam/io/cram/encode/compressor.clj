(ns cljam.io.cram.encode.compressor
  (:import [java.io ByteArrayOutputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorOutputStream]
           [org.apache.commons.compress.compressors.gzip GzipCompressorOutputStream]
           [org.apache.commons.compress.compressors.lzma LZMACompressorOutputStream]
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
         [^LZMACompressorOutputStream out ^ByteArrayOutputStream baos]
  ICompressor
  (compressor-output-stream [_] out)
  (->compressed-result [_]
    (.finish out)
    {:compressor :lzma, :data (.toByteArray baos)}))

(defn compressor
  "TODO"
  [method]
  (let [baos (ByteArrayOutputStream.)
        compr (case method
                :raw (->RawCompressor baos)
                :gzip (->GzipCompressor (GzipCompressorOutputStream. baos) baos)
                :bzip (->BZip2Compressor (BZip2CompressorOutputStream. baos) baos)
                :lzma (->LZMACompressor (LZMACompressorOutputStream. baos) baos)
                (throw
                 (ex-info (str "compression method " method " not supported")
                          {:method method})))
        out (CountingOutputStream. (compressor-output-stream compr))]
    (reify ICompressor
      (compressor-output-stream [_] out)
      (->compressed-result [_]
        (let [raw-size (.getByteCount out)]
          (if (zero? raw-size)
            {:compressor :raw, :data nil, :raw-size 0}
            (assoc (->compressed-result compr) :raw-size raw-size)))))))
