(ns cljam.io.cram.data-series
  (:require [cljam.io.cram.bit-stream :as bs]
            [cljam.io.cram.itf8 :as itf8]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.io.util.lsb.io-stream :as lsb]
            [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream OutputStream]
           [java.nio Buffer ByteBuffer]))

(defn- data-series-type [ds]
  (case ds
    (:BF :CF :RI :RL :AP :RG :MF :NS :NP :TS :NF :TL :FN :FP :DL :RS :PD :HC :MQ)
    :int

    (:FC :BS :BA :QS)
    :byte

    (:RN :BB :QQ :IN :SC)
    :bytes))

(defn- build-codec-decoder
  [{:keys [codec] :as params} data-type bs-decoder content-id->block-data]
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
          len-decoder (build-codec-decoder len-encoding :int bs-decoder content-id->block-data)
          val-decoder (build-codec-decoder val-encoding :byte bs-decoder content-id->block-data)]
      (fn []
        (let [len (len-decoder)
              bb (bb/allocate-lsb-byte-buffer len)]
          (dotimes [_ len]
            (.put bb (byte (val-decoder))))
          (.array bb))))

    :byte-array-stop
    (let [{:keys [stop-byte content-id]} params
          ^ByteBuffer block (get content-id->block-data content-id)]
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
          ret)))

    :beta
    (let [{:keys [offset length]} params]
      (fn []
        (+ (long (bs/read-bits bs-decoder (long length)))
           (long offset))))))

(defn build-data-series-decoders
  "Builds decoders for data series based on the encodings specified in the given
  compression header and block data.

  `ds-encodings` is a map {<data series name> <encoding>} and the return value is
  a map {<data series name> <decoder>}, where:
    - <data series name>: a keyword representing the data series name
    - <encoding>: a map representing the encoding of the data series
    - <decoder>: a function with no arguments that returns a value decoded from
                 the data series upon each call"
  [{ds-encodings :data-series} bs-decoder blocks]
  (let [content-id->block-data (into {} (map (juxt :content-id :data)) blocks)]
    (reduce-kv (fn [decoders ds params]
                 (let [dt (data-series-type ds)
                       decoder (build-codec-decoder params dt bs-decoder content-id->block-data)]
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

(defn- build-tag-decoder [tag-encoding tag-type bs-decoder content-id->block-data]
  (let [decoder (build-codec-decoder tag-encoding :bytes bs-decoder content-id->block-data)
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
  [{:keys [tags]} bs-decoder blocks]
  (let [content-id->block-data (into {} (map (juxt :content-id :data)) blocks)]
    (reduce-kv
     (fn [decoders tag m]
       (reduce-kv
        (fn [decoders tag-type encoding]
          (let [decoder (build-tag-decoder encoding tag-type bs-decoder content-id->block-data)
                tag-type' (str (if (#{\c \C \s \S \i \I} tag-type) \i tag-type))]
            (assoc-in decoders [tag tag-type]
                      (fn [] {:type tag-type' :value (decoder)}))))
        decoders m))
     {} tags)))

(def ^{:doc "Default encodings for all the data series"}
  default-data-series-encodings
  {:BF {:content-id  1, :codec :external}
   :CF {:content-id  2, :codec :external}
   :RI {:content-id  3, :codec :external}
   :RL {:content-id  4, :codec :external}
   :AP {:content-id  5, :codec :external}
   :RG {:content-id  6, :codec :external}
   :RN {:content-id  7, :codec :byte-array-stop, :stop-byte (int \tab)}
   :MF {:content-id  8, :codec :external}
   :NS {:content-id  9, :codec :external}
   :NP {:content-id 10, :codec :external}
   :TS {:content-id 11, :codec :external}
   :NF {:content-id 12, :codec :external}
   :TL {:content-id 13, :codec :external}
   :FN {:content-id 14, :codec :external}
   :FC {:content-id 15, :codec :external}
   :FP {:content-id 16, :codec :external}
   :DL {:content-id 17, :codec :external}
   :BB {:codec :byte-array-len
        :len-encoding {:codec :external, :content-id 18}
        :val-encoding {:codec :external, :content-id 19}}
   :QQ {:codec :byte-array-len
        :len-encoding {:codec :external, :content-id 20}
        :val-encoding {:codec :external, :content-id 21}}
   :BS {:content-id 22, :codec :external}
   :IN {:codec :byte-array-len
        :len-encoding {:codec :external, :content-id 23}
        :val-encoding {:codec :external, :content-id 24}}
   :RS {:content-id 25, :codec :external}
   :PD {:content-id 26, :codec :external}
   :HC {:content-id 27, :codec :external}
   :SC {:codec :byte-array-len
        :len-encoding {:codec :external, :content-id 28}
        :val-encoding {:codec :external, :content-id 29}}
   :MQ {:content-id 30, :codec :external}
   :BA {:content-id 31, :codec :external}
   :QS {:content-id 32, :codec :external}})

(defn- build-codec-encoder
  [{:keys [codec content-id] :as params} data-type content-id->state]
  (letfn [(out-for-encoder []
            (or (get-in @content-id->state [content-id :out])
                (let [out (ByteArrayOutputStream.)]
                  (vswap! content-id->state assoc-in [content-id :out] out)
                  out)))
          (data-for-encoder []
            (let [state (get @content-id->state content-id)]
              (or (get state :data)
                  (let [^ByteArrayOutputStream out (:out state)
                        data (.toByteArray out)]
                    (vswap! content-id->state assoc-in [content-id :data] data)
                    data))))]
    (case codec
      :external
      (let [^OutputStream out (out-for-encoder)]
        (case data-type
          :byte (fn
                  ([] [{:content-id content-id, :data (data-for-encoder)}])
                  ([v] (.write out (int v))))
          :int (fn
                 ([] [{:content-id content-id, :data (data-for-encoder)}])
                 ([v] (itf8/encode-itf8 out v)))))

      :huffman
      (let [{:keys [alphabet bit-len]} params]
        (assert (and (= (count alphabet) 1)
                     (zero? (long (first bit-len))))
                "Huffman coding for more than one word is not supported yet.")
        (fn
          ([] [])
          ([_])))

      :byte-array-len
      (let [{:keys [len-encoding val-encoding]} params
            len-encoder (build-codec-encoder len-encoding :int content-id->state)
            val-encoder (build-codec-encoder val-encoding :byte content-id->state)]
        (fn
          ([] (into (len-encoder) (val-encoder)))
          ([^bytes bs]
           (let [len (alength bs)]
             (len-encoder len)
             (dotimes [i len]
               (val-encoder (aget bs i)))))))

      :byte-array-stop
      (let [{:keys [stop-byte]} params
            ^OutputStream out (out-for-encoder)]
        (fn
          ([] [{:content-id content-id, :data (data-for-encoder)}])
          ([^bytes bs]
           (.write out bs)
           (.write out (int stop-byte))))))))

(defn build-data-series-encoders
  "Builds encoders for data series based on the given encodings.

  `ds-encodings` is a map {<data series name> <encoding>} and the return value is
  a map {<data series name> <encoder>}, where:
    - <data series name>: a keyword representing the data series name
    - <encoding>: a map representing the encoding of the data series
    - <encoder>: a function that has two arities
                 - arity 1: take a value to encode for the data series
                 - arity 0: finalize and return the encoding result"
  [ds-encoding]
  (let [content-id->state (volatile! {})]
    (reduce-kv (fn [encoders ds params]
                 (let [dt (data-series-type ds)
                       encoder (build-codec-encoder params dt content-id->state)]
                   (assoc encoders ds encoder)))
               {} ds-encoding)))

(def ^:private digit->char
  (let [bs (.getBytes "0123456789ABCDEF")]
    (fn [^long i]
      (aget bs i))))

(defn- tag-value-converter [tag-type]
  (case tag-type
    \A (fn [^OutputStream out c] (.write out (byte (int c))))
    \c (fn [^OutputStream out b] (.write out (byte b)))
    \C lsb/write-ubyte
    \s lsb/write-short
    \S lsb/write-ushort
    \i lsb/write-int
    \I lsb/write-uint
    \f lsb/write-float
    \Z (fn [^OutputStream out ^String s]
         (.write out (.getBytes s))
         (.write out (byte 0)))
    \H (fn [^OutputStream out ^"[B" bs]
         (let [n (alength bs)]
           (dotimes [i n]
             (let [b (aget bs i)]
               (.write out (byte (digit->char (bit-and (bit-shift-right b 4) 0x0f))))
               (.write out (byte (digit->char (bit-and b 0x0f))))))
           (.write out (byte 0))))
    \B (fn [^OutputStream out s]
         (let [[t & vs] (str/split s #",")
               t' (first t)
               n (count vs)
               conv (tag-value-converter t')
               vs' (mapv (if (= t' \f)
                           #(Float/parseFloat %)
                           #(Long/parseLong %))
                         vs)]
           (.write out (byte (int t')))
           (lsb/write-int out n)
           (dotimes [i n]
             (conv out (nth vs' i)))))))

(defn- build-tag-encoder [tag-encoding tag-type content-id->state]
  (let [encoder (build-codec-encoder tag-encoding :bytes content-id->state)
        converter (tag-value-converter tag-type)]
    (fn
      ([] (encoder))
      ([v]
       (let [out (ByteArrayOutputStream.)]
         (converter out v)
         (encoder (.toByteArray out)))))))

(defn build-tag-encoders
  "Builds encoders for tags based on the given encodings.

  `tags` is a map {<tag name> {<type character> <encoding>}} and the return
  value is a map {<tag name> {<type character> <encoder>}}, where:
    - <tag name>: a keyword representing the tag name
    - <type character>: a character representing a type of the tag
    - <encoding>: a map representing the encoding of the tag and type
    - <encoder>: a function that has two arities
                 - arity 1: take a value to encode for the tag
                 - arity 0: finalize and return the encoding result"
  [tag-encodings]
  (let [content-id->state (volatile! {})]
    (reduce-kv
     (fn [encoders tag m]
       (reduce-kv
        (fn [encoders tag-type encoding]
          (assoc-in encoders [tag tag-type]
                    (build-tag-encoder encoding tag-type content-id->state)))
        encoders m))
     {} tag-encodings)))
