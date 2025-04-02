(ns cljam.io.cram.encode.structure
  (:require [cljam.io.cram.itf8 :as itf8]
            [cljam.io.sam.util.header :as sam.header]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.io.util.lsb.io-stream :as lsb])
  (:import [java.io ByteArrayOutputStream OutputStream]
           [java.util.zip CRC32 CheckedOutputStream]))

(defn- encode-itf8-array [out vs]
  (itf8/encode-itf8 out (count vs))
  (run! #(itf8/encode-itf8 out %) vs))

(defn- with-out-byte-array ^bytes [f]
  (let [out (ByteArrayOutputStream.)]
    (f out)
    (.toByteArray out)))

(defn- with-size-prefixed-out [out f]
  (let [bs (with-out-byte-array f)]
    (itf8/encode-itf8 out (alength bs))
    (lsb/write-bytes out bs)))

(defn- encode-encoding [^OutputStream out encoding]
  (case (:codec encoding)
    :null
    (do (itf8/encode-itf8 out 0)
        (itf8/encode-itf8 out 0))

    :external
    (do (itf8/encode-itf8 out 1)
        (with-size-prefixed-out out
          (fn [out']
            (itf8/encode-itf8 out' (:content-id encoding)))))

    :huffman
    (let [{:keys [alphabet bit-len]} encoding]
      (itf8/encode-itf8 out 3)
      (with-size-prefixed-out out
        (fn [out']
          (encode-itf8-array out' alphabet)
          (encode-itf8-array out' bit-len))))

    :byte-array-len
    (let [{:keys [len-encoding val-encoding]} encoding]
      (itf8/encode-itf8 out 4)
      (with-size-prefixed-out out
        (fn [out']
          (encode-encoding out' len-encoding)
          (encode-encoding out' val-encoding))))

    :byte-array-stop
    (let [{:keys [stop-byte content-id]} encoding]
      (itf8/encode-itf8 out 5)
      (with-size-prefixed-out out
        (fn [^OutputStream out']
          (.write out' (byte stop-byte))
          (itf8/encode-itf8 out' content-id))))

    (throw (ex-info (str "codec " (:codec encoding) " not supported")
                    {:encoding encoding}))))

(defn encode-file-definition
  "Encodes the CRAM file definition to the given OutputStream."
  [^OutputStream out version file-id]
  (lsb/write-bytes out (.getBytes "CRAM"))
  (.write out (byte (:major version)))
  (.write out (byte (:minor version)))
  (let [bb (bb/allocate-lsb-byte-buffer 20)]
    (bb/write-string bb file-id)
    (.write out (.array bb))))

(defn- with-crc-suffixed ^bytes [out f]
  (let [crc (CRC32.)
        out' (CheckedOutputStream. out crc)
        bb (bb/allocate-lsb-byte-buffer 4)]
    (f out')
    (.putInt bb (unchecked-int (.getValue crc)))
    (lsb/write-bytes out (.array bb))))

(defn encode-container-header
  "Encodes a container header to the given OutputStream."
  [out container-header]
  (with-crc-suffixed out
    (fn [out']
      (lsb/write-int out' (:length container-header))
      (itf8/encode-itf8 out' (:ref-seq-id container-header))
      (itf8/encode-itf8 out' (:start container-header))
      (itf8/encode-itf8 out' (:span container-header))
      (itf8/encode-itf8 out' (:records container-header))
      (itf8/encode-ltf8 out' (:counter container-header))
      (itf8/encode-itf8 out' (:bases container-header))
      (itf8/encode-itf8 out' (:blocks container-header))
      (encode-itf8-array out' (:landmarks container-header)))))

(defn encode-block
  "Encodes a block to the given OutputStream."
  [^OutputStream out method content-type content-id raw-size ^bytes block-data]
  (let [method' (case method :raw 0 :gzip 1 :bzip 2 :lzma 3 (:r4x8-o0 :r4x8-o1) 4)]
    (with-crc-suffixed out
      (fn [out']
        (lsb/write-ubyte out' method')
        (lsb/write-ubyte out' content-type)
        (itf8/encode-itf8 out' content-id)
        (itf8/encode-itf8 out' (alength block-data))
        (itf8/encode-itf8 out' raw-size)
        (lsb/write-bytes out' block-data)))))

(defn generate-block
  "Encodes a block and returns the encoded result as a byte array."
  ^bytes [method content-type content-id raw-size block-data]
  (with-out-byte-array
    (fn [out]
      (encode-block out method content-type content-id raw-size block-data))))

(defn- encode-cram-header-block [out header]
  (let [^String s (sam.header/stringify-header header)
        bs (.getBytes s)
        size (alength bs)
        bb (doto (bb/allocate-lsb-byte-buffer (+ 4 size))
             (.putInt size)
             (.put bs))
        block (.array bb)]
    (encode-block out :raw 0 0 (alength block) block)))

(defn encode-cram-header-container
  "Encodes a CRAM header container to the given OutputStream."
  [out header]
  (let [bs (with-out-byte-array #(encode-cram-header-block % header))
        container-header {:length (alength bs)
                          :ref-seq-id 0
                          :start 0
                          :span 0
                          :records 0
                          :counter 0
                          :bases 0
                          :blocks 1
                          :landmarks [0]}]
    (encode-container-header out container-header)
    (lsb/write-bytes out bs)))

(defn- encode-substitution-matrix [out m]
  (let [all-bases [\A \C \G \T \N]
        ret (byte-array 5)]
    (dotimes [i 5]
      (let [r (nth all-bases i)
            codes (get m r)]
        (aset ret i
              (unchecked-byte
               (loop [j 0, k 0, acc 0]
                 (if (< j 5)
                   (if (= i j)
                     (recur (inc j) k acc)
                     (let [a (nth all-bases j)
                           code (long (get codes a))]
                       (recur (inc j) (inc k)
                              (bit-or acc (bit-shift-left code (* (- 3 k) 2))))))
                   acc))))))
    (lsb/write-bytes out ret)))

(defn- encode-tag-dictionary [out dict]
  (with-size-prefixed-out out
    (fn [out']
      (run! (fn [entry]
              (run! (fn [{:keys [tag] :as item}]
                      (let [tag' (name tag)]
                        (lsb/write-ubyte out' (int (nth tag' 0)))
                        (lsb/write-ubyte out' (int (nth tag' 1)))
                        (lsb/write-ubyte out' (int (:type item)))))
                    entry)
              (lsb/write-ubyte out' 0))
            dict))))

(defn- encode-preservation-map [out preservation-map subst-mat tag-dict]
  (with-size-prefixed-out out
    (fn [^OutputStream out']
      (itf8/encode-itf8 out' (+ (count preservation-map) 2))
      (run! (fn [[k v]]
              (lsb/write-bytes out' (.getBytes (name k)))
              (.write out' (byte (if v 1 0))))
            preservation-map)
      (lsb/write-bytes out' (.getBytes "SM"))
      (encode-substitution-matrix out' subst-mat)
      (lsb/write-bytes out' (.getBytes "TD"))
      (encode-tag-dictionary out' tag-dict))))

(defn- encode-data-series-encodings [out ds-encodings]
  (with-size-prefixed-out out
    (fn [out']
      (itf8/encode-itf8 out' (count ds-encodings))
      (run! (fn [[ds encoding]]
              (lsb/write-string out' (name ds))
              (encode-encoding out' encoding))
            ds-encodings))))

(defn- encode-tag-encoding-map [out tag-encodings]
  (with-size-prefixed-out out
    (fn [out']
      (itf8/encode-itf8 out' (count tag-encodings))
      (run! (fn [[tag v]]
              (run! (fn [[t encoding]]
                      (let [tag' (name tag)
                            k (bit-or (bit-shift-left (int (nth tag' 0)) 16)
                                      (bit-shift-left (int (nth tag' 1)) 8)
                                      (int t))]
                        (itf8/encode-itf8 out' k)
                        (encode-encoding out' encoding)))
                    v))
            tag-encodings))))

(defn encode-compression-header-block
  "Encodes a compression header block to the given OutputStream."
  [out preservation-map subst-mat tag-dict ds-encodings tag-encodings]
  ;; ensure that ds-encodings does not contain the encoding for the :embedded-ref
  ;; pseudo data series
  (let [ds-encodings' (dissoc ds-encodings :embedded-ref)
        bs (with-out-byte-array
             (fn [out']
               (encode-preservation-map out' preservation-map subst-mat tag-dict)
               (encode-data-series-encodings out' ds-encodings')
               (encode-tag-encoding-map out' tag-encodings)))]
    (encode-block out :raw 1 0 (alength bs) bs)))

(defn generate-compression-header-block
  "Encodes a compression header block and returns the encoded result as a byte array."
  ^bytes [preservation-map subst-mat tag-dict ds-encodings tag-encodings]
  (with-out-byte-array
    (fn [out]
      (encode-compression-header-block
       out preservation-map subst-mat tag-dict ds-encodings tag-encodings))))

(defn encode-slice-header-block
  "Encodes a slice header block to the given OutputStream."
  [out slice-header blocks]
  (let [bs (with-out-byte-array
             (fn [out']
               (itf8/encode-itf8 out' (:ref-seq-id slice-header))
               (itf8/encode-itf8 out' (:start slice-header))
               (itf8/encode-itf8 out' (:span slice-header))
               (itf8/encode-itf8 out' (:records slice-header))
               (itf8/encode-ltf8 out' (:counter slice-header))
               (itf8/encode-itf8 out' (count blocks))
               (encode-itf8-array out' (map :content-id blocks))
               (itf8/encode-itf8 out' (:embedded-reference slice-header))
               (lsb/write-bytes out' (:reference-md5 slice-header))))]
    (encode-block out :raw 2 0 (alength bs) bs)))

(defn generate-slice-header-block
  "Encodes a slice header block and returns the encoded result as a byte array."
  ^bytes [slice-header blocks]
  (with-out-byte-array
    (fn [out]
      (encode-slice-header-block out slice-header blocks))))

(defn encode-eof-container
  "Encodes an EOF container to the given OutputStream."
  [out]
  (encode-container-header out {:length 15
                                :ref-seq-id -1
                                :start 4542278
                                :span 0
                                :records 0
                                :counter 0
                                :bases 0
                                :blocks 1
                                :landmarks []})
  (lsb/write-bytes out (byte-array [0 1 0 6 6]))
  (lsb/write-bytes out (byte-array [1 0 1 0 1 0 0xee 0x63 0x01 0x4b])))
