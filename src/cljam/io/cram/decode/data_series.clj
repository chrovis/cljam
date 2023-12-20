(ns cljam.io.cram.decode.data-series
  (:require [cljam.io.cram.itf8 :as itf8]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.string :as str])
  (:import [java.nio Buffer ByteBuffer]))

(defn- data-series-type [ds]
  (case ds
    (:BF :CF :RI :RL :AP :RG :MF :NS :NP :TS :NF :TL :FN :FP :DL :RS :PD :HC :MQ)
    :int

    (:FC :BS :BA :QS)
    :byte

    (:RN :BB :QQ :IN :SC)
    :bytes))

(defn- build-codec-decoder
  [{:keys [codec] :as params} data-type content-id->block-data]
  (case codec
    :external
    (let [^ByteBuffer block (get content-id->block-data (:content-id params))]
      (case data-type
        :byte #(.get block)
        :int #(itf8/decode-itf8 block)))

    :huffman
    (let [{:keys [alphabet bit-len]} params]
      (assert (and (= (count alphabet) 1)
                   (zero? (long (first bit-len))))
              "Huffman coding for more than one word is not supported yet.")
      (constantly (first alphabet)))

    :byte-array-len
    (let [{:keys [len-encoding val-encoding]} params
          len-decoder (build-codec-decoder len-encoding :int content-id->block-data)
          val-decoder (build-codec-decoder val-encoding :byte content-id->block-data)]
      (fn []
        (let [len (len-decoder)
              bb (bb/allocate-lsb-byte-buffer len)]
          (dotimes [_ len]
            (.put bb (byte (val-decoder))))
          (.array bb))))

    :byte-array-stop
    (let [{:keys [stop-byte external-id]} params
          ^ByteBuffer block (get content-id->block-data external-id)]
      (fn []
        (.mark ^Buffer block)
        (let [start (.position block)
              end (long
                   (loop []
                     (if (= (.get block) (byte stop-byte))
                       (.position block)
                       (recur))))
              len (dec (- end start))
              _ (.reset ^Buffer block)
              ret (bb/read-bytes block len)]
          (.get block)
          ret)))))

(defn build-data-series-decoders
  "Builds decoders for data series based on the encodings specified in the given
  compression header and block data.

  `ds-encodings` is a map {<data series name> <encoding>} and the return value is
  a map {<data series name> <decoder>}, where:
    - <data series name>: a keyword representing the data series name
    - <encoding>: a map representing the encoding of the data series
    - <decoder>: a function with no arguments that returns a value decoded from
                 the data series upon each call"
  [{ds-encodings :data-series} blocks]
  (let [content-id->block-data (into {} (map (juxt :content-id :data)) blocks)]
    (reduce-kv (fn [decoders ds params]
                 (let [dt (data-series-type ds)
                       decoder (build-codec-decoder params dt content-id->block-data)]
                   (assoc decoders ds decoder)))
               {} ds-encodings)))

(defn- tag-value-coercer [tag-type]
  (case tag-type
    \A #(char (.get ^ByteBuffer %))
    \c #(.get ^ByteBuffer %)
    \C bb/read-ubyte
    \s #(.getShort ^ByteBuffer %)
    \S bb/read-ushort
    \i #(.getInt ^ByteBuffer %)
    \I bb/read-uint
    \f #(.getFloat ^ByteBuffer %)
    \Z bb/read-null-terminated-string
    \H (fn [^ByteBuffer bb]
         (let [s (.getBytes ^String (bb/read-null-terminated-string bb))
               n (quot (alength s) 2)
               arr (byte-array n)]
           (dotimes [i n]
             (let [b (bit-or (bit-shift-left (Character/digit (aget s (* 2 i)) 16) 4)
                             (Character/digit (aget s (inc (* 2 i))) 16))]
               (aset arr i (byte b))))
           arr))
    \B (fn [^ByteBuffer bb]
         (let [tag-type' (char (.get bb))
               len (.getInt bb)
               coercer (tag-value-coercer tag-type')
               vs (repeatedly len (partial coercer bb))]
           (str/join \, (cons tag-type' vs))))))

(defn- build-tag-decoder [tag-encoding tag-type content-id->block-data]
  (let [decoder (build-codec-decoder tag-encoding :bytes content-id->block-data)
        coercer (tag-value-coercer tag-type)]
    (fn []
      (let [bb (bb/make-lsb-byte-buffer (decoder))]
        (coercer bb)))))

(defn build-tag-decoders
  "Builds decoders for tags based on the encodings specified in the given
  compression header and block data.

  `tags` is a map {<tag name> {<type character> <encoding>}} and the return
  value is a map {<tag name> {<type character> <decoder>}}, where:
    - <tag name>: a keyword representing the tag name
    - <type character>: a character representing a type of the tag
    - <encoding>: a map representing the encoding of the tag and type
    - <decoder>: a function with no arguments that returns a value decoded from
                 the data series for the tag upon each call"
  [{:keys [tags]} blocks]
  (let [content-id->block-data (into {} (map (juxt :content-id :data)) blocks)]
    (reduce-kv
     (fn [decoders tag m]
       (reduce-kv
        (fn [decoders tag-type encoding]
          (let [decoder (build-tag-decoder encoding tag-type content-id->block-data)
                tag-type' (str (if (#{\c \C \s \S \i \I} tag-type) \i tag-type))]
            (assoc-in decoders [tag tag-type]
                      (fn [] {:type tag-type' :value (decoder)}))))
        decoders m))
     {} tags)))
