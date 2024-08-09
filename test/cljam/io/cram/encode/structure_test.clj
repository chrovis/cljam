(ns cljam.io.cram.encode.structure-test
  (:require [cljam.io.cram.decode.structure :as decode.struct]
            [cljam.io.cram.encode.structure :as struct]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [java.util Arrays]
           [java.util.zip CRC32]
           [org.apache.commons.compress.compressors.gzip GzipCompressorOutputStream]))

(defn- with-encoding-result [encode-fn & args]
  (let [cont (last args)
        out (ByteArrayOutputStream.)
        _ (apply encode-fn out (butlast args))
        bs (.toByteArray out)]
    (cont (bb/make-lsb-byte-buffer bs) bs)))

(defn- calculate-crc ^bytes [^bytes bs ^long start ^long end]
  (let [crc (CRC32.)]
    (.update crc bs start end)
    (-> (bb/allocate-lsb-byte-buffer 4)
        (.putInt (.getValue crc))
        .array)))

(deftest encode-file-definition-test
  (let [version {:major 3, :minor 0}
        file-id "test.cram"]
    (with-encoding-result struct/encode-file-definition version file-id
      (fn [bb _]
        (is (= {:version version, :id file-id}
               (decode.struct/decode-file-definition bb)))))))

(deftest encode-container-header-test
  (let [header {:length 362828
                :ref-seq-id 1
                :start 100
                :span 500
                :records 100
                :counter 0
                :bases 15000
                :blocks 40
                :landmarks [207]}]
    (with-encoding-result struct/encode-container-header header
      (fn [bb ^bytes encoded]
        (let [decoded (decode.struct/decode-container-header bb)
              crc (calculate-crc encoded 0 (- (alength encoded) 4))]
          (is (= header (dissoc decoded :crc)))
          (is (Arrays/equals crc ^bytes (:crc decoded))))))))

(deftest encode-block-test
  (testing "compress with :raw compressor"
    (let [content (.getBytes "This is the block content.")
          raw-size (alength content)
          {:keys [method content-type content-id] :as block} {:method :raw
                                                              :content-type 4
                                                              :content-id 42}]
      (with-encoding-result struct/encode-block method content-type content-id raw-size content
        (fn [bb ^bytes encoded]
          (let [decoded (decode.struct/decode-block bb)
                crc (calculate-crc encoded 0 (- (alength encoded) 4))]
            (is (= (assoc block :method 0 :size raw-size :raw-size raw-size)
                   (dissoc decoded :crc :data)))
            (is (= (seq content)
                   (seq (bb/read-bytes (:data decoded) (:raw-size decoded)))))
            (is (Arrays/equals crc ^bytes (:crc decoded))))))))
  (testing "compress with :gzip compressor"
    (let [content (.getBytes "This is the block content.")
          compressed (with-open [baos (ByteArrayOutputStream.)
                                 out (GzipCompressorOutputStream. baos)]
                       (.write out content)
                       (.finish out)
                       (.toByteArray baos))
          raw-size (alength content)
          {:keys [method content-type content-id] :as block} {:method :gzip
                                                              :content-type 4
                                                              :content-id 42}]
      (with-encoding-result struct/encode-block method content-type content-id raw-size compressed
        (fn [bb ^bytes encoded]
          (let [decoded (decode.struct/decode-block bb)
                crc (calculate-crc encoded 0 (- (alength encoded) 4))]
            (is (= (assoc block :method 1 :size (alength compressed) :raw-size raw-size)
                   (dissoc decoded :crc :data)))
            (is (= (seq content)
                   (seq (bb/read-bytes (:data decoded) (:raw-size decoded)))))
            (is (Arrays/equals crc ^bytes (:crc decoded)))))))))

(deftest encode-cram-header-container-test
  (let [cram-header {:SQ
                     [{:SN "ref"}
                      {:SN "ref2"}]
                     :RG
                     [{:ID "rg001"}
                      {:ID "rg002"}]}]
    (with-encoding-result struct/encode-cram-header-container cram-header
      (fn [^ByteBuffer bb ^bytes encoded]
        (let [container-header (decode.struct/decode-container-header bb)
              container-header-size (.position bb)
              crc (calculate-crc encoded 0 (- container-header-size 4))
              decoded (decode.struct/decode-cram-header-block bb)]
          (is (= {:length (- (alength encoded) container-header-size)
                  :ref-seq-id 0, :start 0, :span 0, :records 0
                  :counter 0, :bases 0, :blocks 1, :landmarks [0]}
                 (dissoc container-header :crc)))
          (is (Arrays/equals crc ^bytes (:crc container-header)))
          (is (= cram-header decoded)))))))

(deftest encode-compression-header-block-test
  (let [preservation-map {:RN true, :AP false, :RR true}
        subst-mat {\A {\T 0, \G 1, \C 2, \N 3}
                   \T {\A 0, \G 1, \C 2, \N 3}
                   \G {\A 0, \T 1, \C 2, \N 3}
                   \C {\A 0, \T 1, \G 2, \N 3}
                   \N {\A 0, \T 1, \G 2, \C 3}}
        tag-dict [[]
                  [{:tag :MD, :type \Z}]
                  [{:tag :NM, :type \c}]
                  [{:tag :MD, :type \Z}
                   {:tag :NM, :type \c}]]
        ds-encodings {:BF {:content-id  1, :codec :external}
                      :RN {:content-id  2, :codec :byte-array-stop, :stop-byte (int \tab)}
                      :BB {:codec :byte-array-len
                           :len-encoding {:codec :huffman, :alphabet [151], :bit-len [0]}
                           :val-encoding {:codec :external, :content-id 3}}
                      :QQ {:codec :byte-array-len
                           :len-encoding {:codec :external, :content-id 4}
                           :val-encoding {:codec :external, :content-id 5}}}
        tag-encodings {:MD {\Z {:codec :byte-array-len
                                :len-encoding {:codec :external, :content-id 5063770}
                                :val-encoding {:codec :external, :content-id 5063770}}}
                       :NM {\c {:codec :byte-array-len
                                :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
                                :val-encoding {:codec :external, :content-id 5131619}}}}]
    (with-encoding-result
      struct/encode-compression-header-block
      preservation-map subst-mat tag-dict ds-encodings tag-encodings
      (fn [bb _]
        (let [decoded (decode.struct/decode-compression-header-block bb)]
          (is (= {:preservation-map (assoc preservation-map :TD tag-dict)
                  :data-series ds-encodings
                  :tags tag-encodings}
                 (update decoded :preservation-map dissoc :SM)))
          (is (= (into {} (map (fn [[k v]] [k (set/map-invert v)]))
                       subst-mat)
                 (get-in decoded [:preservation-map :SM]))))))))

(deftest encode-slice-header-block-test
  (let [header {:ref-seq-id 1
                :start 100
                :span 500
                :records 1000
                :counter 12345
                :embedded-reference -1
                :reference-md5 (byte-array (range 16))
                :tags []}
        blocks [{:content-id 1}
                {:content-id 2}
                {:content-id 3}]]
    (with-encoding-result struct/encode-slice-header-block header blocks
      (fn [bb _]
        (let [decoded (decode.struct/decode-slice-header-block bb)]
          (is (= (-> header
                     (assoc :blocks (count blocks)
                            :content-ids (mapv :content-id blocks))
                     (dissoc :reference-md5))
                 (dissoc decoded :reference-md5)))
          (is (Arrays/equals ^bytes (:reference-md5 header)
                             ^bytes (:reference-md5 decoded))))))))

(deftest encode-eof-container-test
  (with-encoding-result struct/encode-eof-container
    (fn [bb _]
      (let [container-header (decode.struct/decode-container-header bb)
            compression-header (decode.struct/decode-compression-header-block bb)]
        (is (decode.struct/eof-container? container-header))
        (is (= {:preservation-map {:RN true, :AP true, :RR true}
                :data-series {}
                :tags {}}
               compression-header))))))
