(ns cljam.io.cram.decode.structure
  (:require [cljam.io.cram.codecs.rans4x8 :as rans]
            [cljam.io.cram.itf8 :as itf8]
            [cljam.io.sam.util.header :as sam.header]
            [cljam.io.util.byte-buffer :as bb])
  (:import [java.io ByteArrayInputStream IOException]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.util Arrays]
           [org.apache.commons.compress.compressors CompressorStreamFactory]))

(def ^:private ^:const cram-magic "CRAM")

(defn- decode-itf8-array [bb]
  (let [n (itf8/decode-itf8 bb)]
    (loop [i n, acc (transient [])]
      (if (zero? i)
        (persistent! acc)
        (recur (dec i) (conj! acc (itf8/decode-itf8 bb)))))))

(defn- decode-encoding [^ByteBuffer bb]
  (let [codec-id (itf8/decode-itf8 bb)
        _n (itf8/decode-itf8 bb)]
    (case codec-id
      0 {:codec :null}
      1 (let [content-id (itf8/decode-itf8 bb)]
          {:codec :external
           :content-id content-id})
      3 (let [alphabet (decode-itf8-array bb)
              bit-len (decode-itf8-array bb)]
          {:codec :huffman, :alphabet alphabet, :bit-len bit-len})
      4 (let [len-encoding (decode-encoding bb)
              val-encoding (decode-encoding bb)]
          {:codec :byte-array-len, :len-encoding len-encoding, :val-encoding val-encoding})
      5 (let [stop-byte (.get bb)
              external-id (itf8/decode-itf8 bb)]
          {:codec :byte-array-stop, :stop-byte stop-byte, :external-id external-id})
      6 (let [offset (itf8/decode-itf8 bb)
              length (itf8/decode-itf8 bb)]
          {:codec :beta, :offset offset, :length length})
      7 (let [offset (itf8/decode-itf8 bb)
              k (itf8/decode-itf8 bb)]
          {:codec :subexp, :offset offset, :k k})
      9 (let [offset (itf8/decode-itf8 bb)]
          {:codec :gamma, :offset offset})
      (throw (ex-info (str "codec " codec-id " not supported") {})))))

(defn decode-file-definition
  "Decodes the CRAM file definition from the given byte buffer."
  [bb]
  (when-not (Arrays/equals ^bytes (bb/read-bytes bb 4) (.getBytes cram-magic))
    (throw (IOException. "Invalid CRAM file")))
  (let [major (bb/read-ubyte bb)
        minor (bb/read-ubyte bb)
        file-id (String. ^bytes (bb/read-bytes bb 20))]
    {:version {:major major :minor minor}, :id file-id}))

(defn decode-container-header
  "Decodes a container header from the given byte buffer."
  [^ByteBuffer bb]
  (let [len (.getInt bb)
        ref-seq-id (itf8/decode-itf8 bb)
        start-pos (itf8/decode-itf8 bb)
        span (itf8/decode-itf8 bb)
        n-records (itf8/decode-itf8 bb)
        counter (itf8/decode-ltf8 bb)
        n-bases (itf8/decode-ltf8 bb)
        n-blocks (itf8/decode-itf8 bb)
        landmarks (decode-itf8-array bb)
        crc (bb/read-bytes bb 4)]
    {:length len
     :ref ref-seq-id
     :start start-pos
     :span span
     :records n-records
     :counter counter
     :bases n-bases
     :blocks n-blocks
     :landmarks landmarks
     :crc crc}))

(defn eof-container?
  "Returns true iff the given container header represents an EOF container."
  [container-header]
  (and (= (:length container-header) 15)
       (= (:ref container-header) -1)
       (= (:start container-header) 4542278)
       (= (:span container-header) 0)
       (= (:records container-header) 0)
       (= (:counter container-header) 0)
       (= (:bases container-header) 0)
       (= (:blocks container-header) 1)
       (= (:landmarks container-header) [])))

(defn- split-buffer ^ByteBuffer [^ByteBuffer bb size]
  (let [^Buffer bb' (.order (.slice bb) ByteOrder/LITTLE_ENDIAN)]
    (bb/skip bb size)
    (.limit bb' size)))

(def ^:private decode-block-data
  (let [factory (CompressorStreamFactory.)]
    (fn [^ByteBuffer bb ^long method ^long size ^long raw-size]
      (if (zero? size)
        (bb/allocate-lsb-byte-buffer 0)
        (case method
          0 (split-buffer bb size)
          4 (->> (split-buffer bb size)
                 rans/decode
                 bb/make-lsb-byte-buffer)
          (let [compressed (bb/read-bytes bb size)
                bais (ByteArrayInputStream. compressed)
                uncompressed (byte-array raw-size)
                bb' (bb/make-lsb-byte-buffer uncompressed)
                compressor (case method
                             1 CompressorStreamFactory/GZIP
                             2 CompressorStreamFactory/BZIP2
                             3 CompressorStreamFactory/LZMA
                             (throw
                              (ex-info (str "compression method " method
                                            " not supported")
                                       {:method method})))]
            (with-open [is (.createCompressorInputStream factory compressor bais)]
              (.read is uncompressed)
              bb')))))))

(defn decode-block
  "Decodes a block from the given byte buffer."
  [bb]
  (let [method (bb/read-ubyte bb)
        content-type-id (bb/read-ubyte bb)
        content-id (itf8/decode-itf8 bb)
        size (itf8/decode-itf8 bb)
        raw-size (itf8/decode-itf8 bb)
        data (decode-block-data bb method size raw-size)
        crc (bb/read-bytes bb 4)]
    {:method method
     :content-type content-type-id
     :content-id content-id
     :size size
     :raw-size raw-size
     :data data
     :crc crc}))

(defn decode-cram-header-block
  "Decodes a CRAM header block from the given byte buffer."
  [bb]
  (let [{bb' :data} (decode-block bb)
        size (bb/read-uint bb')]
    (sam.header/parse-header (String. ^bytes (bb/read-bytes bb' size)))))

(defn- decode-substitution-matrix [bb]
  (let [bs (bb/read-bytes bb 5)
        all-bases [\A \C \G \T \N]]
    (into {}
          (map (fn [r ^long b]
                 [r (zipmap [(bit-and (bit-shift-right b 6) 0x3)
                             (bit-and (bit-shift-right b 4) 0x3)
                             (bit-and (bit-shift-right b 2) 0x3)
                             (bit-and b 0x3)]
                            (remove #{r} all-bases))])
               all-bases bs))))

(defn- decode-tag-dictionary [^ByteBuffer bb]
  (let [n (itf8/decode-itf8 bb)
        bb' (split-buffer bb n)
        decode-tags (fn [bb]
                      (loop [acc (transient [])]
                        (let [c1 (long (bb/read-ubyte bb))]
                          (if (zero? c1)
                            (persistent! acc)
                            (let [c2 (bb/read-ubyte bb)
                                  t (bb/read-ubyte bb)
                                  tag (keyword (str (char c1) (char c2)))]
                              (recur (conj! acc {:tag tag, :type (char t)})))))))]
    (loop [acc (transient [])]
      (if (.hasRemaining bb')
        (recur (conj! acc (decode-tags bb')))
        (persistent! acc)))))

(defn- decode-preservation-map [^ByteBuffer bb]
  (let [_size (itf8/decode-itf8 bb)
        n (itf8/decode-itf8 bb)]
    (loop [i n, acc (transient {:RN true, :AP true, :RR true})]
      (if (zero? i)
        (persistent! acc)
        (let [k (keyword (String. ^bytes (bb/read-bytes bb 2)))
              v (case k
                  (:RN :AP :RR) (pos? (.get bb))
                  :SM (decode-substitution-matrix bb)
                  :TD (decode-tag-dictionary bb))]
          (recur (dec i) (assoc! acc k v)))))))

(defn- decode-data-series-encodings [bb]
  (let [_size (itf8/decode-itf8 bb)
        n (itf8/decode-itf8 bb)]
    (loop [n n, acc (transient {})]
      (if (zero? n)
        (persistent! acc)
        (let [k (keyword (String. ^bytes (bb/read-bytes bb 2)))
              v (decode-encoding bb)]
          (recur (dec n) (assoc! acc k v)))))))

(defn- decode-tag-encoding-map [bb]
  (let [_size (itf8/decode-itf8 bb)
        n (itf8/decode-itf8 bb)]
    (loop [i n, acc {}]
      (if (zero? i)
        acc
        (let [k (itf8/decode-itf8 bb)
              c1 (char (bit-and (bit-shift-right k 16) 0xff))
              c2 (char (bit-and (bit-shift-right k 8) 0xff))
              t (char (bit-and k 0xff))
              v (decode-encoding bb)
              tag (keyword (str c1 c2))]
          (recur (dec i) (assoc-in acc [tag t] v)))))))

(defn decode-compression-header-block
  "Decodes a compression header block from the given byte buffer."
  [bb]
  (let [{bb' :data} (decode-block bb)
        preservation-map (decode-preservation-map bb')
        data-series-encodings (decode-data-series-encodings bb')
        tag-encoding-map (decode-tag-encoding-map bb')]
    {:preservation-map preservation-map
     :data-series data-series-encodings
     :tags tag-encoding-map}))

(defn decode-slice-header-block
  "Decodes a slice header block from the given byte buffer."
  [bb]
  (let [{bb' :data} (decode-block bb)
        ref-seq-id (itf8/decode-itf8 bb')
        start (itf8/decode-itf8 bb')
        span (itf8/decode-itf8 bb')
        n-records (itf8/decode-itf8 bb')
        counter (itf8/decode-ltf8 bb')
        n-blocks (itf8/decode-itf8 bb')
        content-ids (decode-itf8-array bb')
        embedded-reference (itf8/decode-itf8 bb')
        reference-md5 (bb/read-bytes bb' 16)
        tags []]
    {:ref-seq-id ref-seq-id
     :start start
     :span span
     :records n-records
     :counter counter
     :blocks n-blocks
     :content-ids content-ids
     :embedded-reference embedded-reference
     :reference-md5 reference-md5
     :tags tags}))
