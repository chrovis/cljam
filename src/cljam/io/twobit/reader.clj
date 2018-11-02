(ns cljam.io.twobit.reader
  (:require [clojure.java.io :as cio]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.lsb :as lsb]
            [cljam.util :as util])
  (:import [java.io Closeable DataInput RandomAccessFile]
           [java.net URL]
           [java.nio CharBuffer]))

(defrecord TwoBitReader [^RandomAccessFile reader ^URL url endian file-index seq-index]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(defn- read-file-header!
  [^DataInput rdr]
  (let [sig (.readInt rdr)
        endian (case sig
                 0x1A412743 :big
                 0x4327411A :little
                 :else nil)]
    (when endian
      (let [read-fn (case endian :big #(.readInt ^DataInput %) :little lsb/read-int)
            ver (read-fn rdr)
            nseq (read-fn rdr)
            zero (read-fn rdr)]
        (when (and (zero? ver) (zero? zero))
          [endian nseq])))))

(defn- read-index!
  [^DataInput rdr endian]
  (let [name-size (case endian
                    :little (lsb/read-ubyte rdr)
                    :big (.readUnsignedByte rdr))
        name-str (case endian
                   :little (lsb/read-string rdr name-size)
                   :big (let [ba (byte-array name-size)]
                          (.readFully rdr ba)
                          (String. ba)))
        offset (case endian
                 :little (lsb/read-int rdr)
                 :big (.readInt rdr))]
    {:name name-str
     :offset offset}))

(defn- read-sequence-header!
  [^DataInput rdr endian]
  (let [read-fn (case endian :little lsb/read-int :big #(.readInt ^DataInput %))
        dna-size (read-fn rdr)
        n-block-count (read-fn rdr)
        n-block-starts (doall (repeatedly n-block-count #(inc (read-fn rdr))))
        n-block-sizes (doall (repeatedly n-block-count #(read-fn rdr)))
        mask-block-count (read-fn rdr)
        mask-block-starts (doall (repeatedly mask-block-count #(inc (read-fn rdr))))
        mask-block-sizes (doall (repeatedly mask-block-count #(read-fn rdr)))
        zero (read-fn rdr)]
    {:len dna-size
     :ambs (mapv vector n-block-starts n-block-sizes)
     :masks (mapv vector mask-block-starts mask-block-sizes)
     :header-offset (+ 16 (* n-block-count 4 2) (* mask-block-count 4 2))}))

(def ^:const twobit-to-str
  (let [table "TCAG"]
    (mapv
     (fn [j] (let [i (byte (- j 128))
                   n4 (bit-and i 2r11)
                   n3 (bit-and (unsigned-bit-shift-right i 2) 2r11)
                   n2 (bit-and (unsigned-bit-shift-right i 4) 2r11)
                   n1 (bit-and (unsigned-bit-shift-right i 6) 2r11)]
               (str (.charAt table n1) (.charAt table n2) (.charAt table n3) (.charAt table n4))))
     (range 256))))

(defn replace-ambs!
  "Replace regions of charbuffer with Ns."
  [^CharBuffer cb ambs ^long start ^long end]
  (doseq [[^long n-start ^long n-size] ambs]
    (when-not (or (< end n-start) (< (+ n-start n-size -1) start))
      (.position cb (max 0 (- n-start start)))
      (dotimes [_ (- (min end (+ n-start n-size -1)) (max start n-start) -1)]
        (.put cb \N)))))

(defn mask!
  "Mask regions of given charbuffer."
  [^CharBuffer cb masks ^long start ^long end]
  (doseq [[^long m-start ^long m-size] masks]
    (when-not (or (< end m-start) (< (+ m-start m-size -1) start))
      (.position cb (max 0 (- m-start start)))
      (.mark cb)
      (let [ca (char-array (- (min end (+ m-start m-size -1)) (max start m-start) -1))]
        (.get cb ca)
        (.reset cb)
        (dotimes [i (alength ca)]
          (.put cb (char (+ (int (aget ca i)) 32)))))))) ;; lower case character

(defn ^String read-sequence
  "Reads sequence at the given region from reader.
   Pass {:mask? true} to enable masking of sequence."
  ([rdr region]
   (read-sequence rdr region {}))
  ([^TwoBitReader rdr {:keys [chr start end]} {:keys [mask?] :or {mask? false}}]
   (when-let [[n {:keys [offset]}]
              (first (filter (fn [[i {:keys [name]}]] (= name chr)) (map vector (range) (.file-index rdr))))]
     (let [{:keys [len ambs masks header-offset]} @(nth (.seq-index rdr) n) ;; Potential seek & read.
           start' (max 1 (or start 1))
           end' (min len (or end len))]
       (when (<= start' end')
         (let [start-offset (quot (dec start') 4)
               end-offset (quot (dec end') 4)
               ba (byte-array (- end-offset start-offset -1))
               cb (CharBuffer/allocate (inc (- end' start')))]
           (.seek ^RandomAccessFile (.reader rdr) (+ offset header-offset start-offset))
           (.readFully ^RandomAccessFile (.reader rdr) ba)
           (dotimes [out-pos (inc (- end' start'))]
             (let [ref-pos (+ out-pos start')
                   ba-pos (- (quot (dec ref-pos) 4) start-offset)
                   bit-pos (mod (dec ref-pos) 4)]
               (when (<= 1 ref-pos len)
                 (.put cb (.charAt ^String (twobit-to-str (+ (aget ba ba-pos) 128)) bit-pos)))))
           (replace-ambs! cb ambs start' end')
           (when mask? (mask! cb masks start' end'))
           (.rewind cb)
           (.toString cb)))))))

(defn- read-all-sequences*
  [rdr chrs option]
  (when (seq chrs)
    (let [[{:keys [name]} & nxt] chrs]
      (lazy-seq
       (cons {:name name
              :sequence (read-sequence rdr {:chr name} option)}
             (read-all-sequences* rdr nxt option))))))

(defn read-all-sequences
  "Reads all sequences in file."
  ([rdr]
   (read-all-sequences rdr {}))
  ([^TwoBitReader rdr option]
   (read-all-sequences* rdr (.file-index rdr) option)))

(defn read-indices
  "Reads metadata of indexed sequences."
  [^TwoBitReader rdr]
  (mapv
   (fn [fi si] (merge fi @si))
   (.file-index rdr)
   (.seq-index rdr)))

(extend-type TwoBitReader
  protocols/IReader
  (reader-url [this] (.url this))
  (read
    ([this] (protocols/read this {}))
    ([this option] (protocols/read-all-sequences this option)))
  (indexed? [_] true)
  protocols/ISequenceReader
  (read-indices
    [this] (read-indices this))
  (read-all-sequences
    ([this] (protocols/read-all-sequences this {}))
    ([this option]
     (read-all-sequences this option)))
  (read-sequence
    (^String [this region]
     (protocols/read-sequence this region {}))
    (^String [this region option]
     (read-sequence this region option)))
  protocols/IRegionReader
  (read-in-region
    (^String [this region]
     (protocols/read-in-region this region {}))
    (^String [this {:keys [chr start end] :as region} option]
     (read-sequence this region option))))

(defn ^TwoBitReader reader
  "Returns .2bit file reader of f."
  [^String f]
  (let [abs-f (.getAbsolutePath (cio/file f))
        rdr (RandomAccessFile. abs-f "r")
        [endian nseq] (read-file-header! rdr)]
    (when (and endian nseq)
      (let [indices (vec (repeatedly nseq #(read-index! rdr endian)))
            seq-indices (mapv (fn [{:keys [offset] :as m}]
                                (delay
                                 (with-open [raf (RandomAccessFile. abs-f "r")]
                                   (.seek raf offset)
                                   (read-sequence-header! raf endian))))
                              indices)]
        (TwoBitReader. rdr (util/as-url abs-f) endian indices seq-indices)))))

(defn ^TwoBitReader clone-reader
  "Clones .2bit reader sharing persistent objects."
  [^TwoBitReader rdr]
  (let [url (.url rdr)
        raf (RandomAccessFile. (cio/as-file url) "r")]
    (TwoBitReader. raf url (.endian rdr) (.file-index rdr) (.seq-index rdr))))
