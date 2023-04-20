(ns cljam.io.twobit.reader
  (:require [cljam.io.protocols :as protocols]
            [cljam.util :as util])
  (:import [java.io Closeable]
           [java.util TreeMap HashMap]
           [java.nio Buffer CharBuffer ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [java.nio.file Paths OpenOption StandardOpenOption]))

(deftype TwoBitReader [buf url index]
  Closeable
  (close [_]))

(defrecord ChromHeader [ambs masks ^long header-offset])

(defrecord Chrom [name ^int len ^int offset ^int index header])

(defn- read-header-block! ^TreeMap [^ByteBuffer buf]
  (let [n-blocks (.getInt buf)
        starts (doto (.slice buf) (.order (.order buf)))
        _ (.position ^Buffer buf (+ (.position buf) (* Integer/BYTES n-blocks)))
        m (TreeMap.)]
    (dotimes [_ n-blocks]
      (.put m (unchecked-inc-int (.getInt starts)) (.getInt buf)))
    (assert (= (.size m) n-blocks))
    m))

(defn- read-sequence-header! [buf]
  (let [amb-blocks (read-header-block! buf)
        mask-blocks (read-header-block! buf)]
    (ChromHeader.
     amb-blocks
     mask-blocks
     (* Integer/BYTES
        (+ 4 (* (.size amb-blocks) 2) (* (.size mask-blocks) 2))))))

(defn- read-file-index! [^ByteBuffer buf ^long n-seqs]
  (let [m (HashMap.)
        ba (byte-array 255)]
    (dotimes [i n-seqs]
      (let [chr-len (Byte/toUnsignedInt (.get buf))
            _ (.get buf ba 0 chr-len)
            chr (String. ba 0 chr-len)
            offset (.getInt buf)
            _ (.mark ^Buffer buf)
            _ (.position ^Buffer buf offset)
            len (.getInt buf)
            _ (.reset ^Buffer buf)
            header (delay
                    (let [buf' (.duplicate buf)]
                      (.order buf' (.order buf))
                      (.position ^Buffer buf' (+ offset Integer/BYTES))
                      (read-sequence-header! buf')))]
        (.put m chr (Chrom. chr len offset i header))))
    m))

(def ^:private ^{:tag (Class/forName "[[C")} twobit-to-str
  (let [table "TCAG"]
    (->> 256
         range
         (map
          (fn [^long j] (let [i (byte (- j 128))
                              n4 (bit-and i 2r11)
                              n3 (bit-and (unsigned-bit-shift-right i 2) 2r11)
                              n2 (bit-and (unsigned-bit-shift-right i 4) 2r11)
                              n1 (bit-and (unsigned-bit-shift-right i 6) 2r11)]
                          (char-array [(.charAt table n1)
                                       (.charAt table n2)
                                       (.charAt table n3)
                                       (.charAt table n4)]))))
         (into-array (Class/forName "[C")))))

(defn replace-ambs!
  "Replace regions of charbuffer with Ns."
  [^CharBuffer cb ^TreeMap ambs ^long start ^long end]
  (let [floor (or (.floorKey ambs (int start)) (int 1))]
    (doseq [[^long n-start ^long n-size] (.subMap ambs floor (int (inc end)))]
      (when-not (or (< end n-start) (< (+ n-start n-size -1) start))
        (.position ^Buffer cb (max 0 (- n-start start)))
        (dotimes [_ (- (min end (+ n-start n-size -1)) (max start n-start) -1)]
          (.put cb \N))))))

(defn mask!
  "Mask regions of given charbuffer."
  [^CharBuffer cb ^TreeMap masks ^long start ^long end]
  (let [floor (or (.floorKey masks (int start)) (int 1))]
    (doseq [[^long m-start ^long m-size] (.subMap masks floor (int (inc end)))]
      (when-not (or (< end m-start) (< (+ m-start m-size -1) start))
        (.position ^Buffer cb (max 0 (- m-start start)))
        (.mark ^Buffer cb)
        (let [ca (char-array
                  (- (min end (+ m-start m-size -1))
                     (max start m-start) -1))]
          (.get cb ca)
          (.reset ^Buffer cb)
          (dotimes [i (alength ca)]
            ;; to lower case character
            (.put cb (unchecked-char
                      (unchecked-add-int
                       (unchecked-int (aget ca i)) 32)))))))))

(defn read-sequence
  "Reads sequence at the given region from reader.
   Pass {:mask? true} to enable masking of sequence."
  (^String [rdr region]
   (read-sequence rdr region {}))
  (^String [^TwoBitReader rdr
            {:keys [chr ^long start ^long end]}
            {:keys [mask?] :or {mask? false}}]
   (when-let [^Chrom c (get (.index rdr) chr)]
     (let [start' (long (max 1 (or start 1)))
           end' (min (.len c) (long (or end (.len c))))]
       (when (<= start' end')
         ;; Potential seek & read.
         (let [^ChromHeader h @(.header c)
               start-offset (quot (dec start') 4)
               end-offset (quot (dec end') 4)
               buf ^ByteBuffer (.buf rdr)
               cb (CharBuffer/allocate (* 4 (inc (- end-offset start-offset))))]
           (.position ^Buffer buf (+ (.offset c) (.header-offset h) start-offset))
           (while (.hasRemaining cb)
             (->> (unchecked-add-int 128 (.get buf))
                  ^chars (aget twobit-to-str)
                  (.put cb)))
           (let [cb' (as-> cb cb
                       (.position ^Buffer cb (rem (dec start') 4))
                       (.slice ^CharBuffer cb)
                       (.limit ^Buffer cb (int (inc (- end' start')))))]
             (replace-ambs! cb' (.ambs h) start' end')
             (when mask? (mask! cb' (.masks h) start' end'))
             (.rewind cb')
             (.toString cb'))))))))

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
   (read-all-sequences* rdr (sort-by :index (vals (.index rdr))) option)))

(defn read-seq-summaries
  "Reads summaries of sequences in this 2bit file."
  [^TwoBitReader rdr]
  (mapv
   (fn [^Chrom c]
     {:name (.name c), :len (.len c)})
   (sort-by :index (vals (.index rdr)))))

(defn read-indices
  "Reads metadata of indexed sequences. Forces loading all indices."
  [^TwoBitReader rdr]
  (mapv
   (fn [{:keys [name len offset header]}]
     (let [{:keys [ambs header-offset masks]} @header]
       {:name name, :len len, :offset offset,
        :ambs (into {} ambs), :masks (into {} masks)
        :header-offset header-offset}))
   (sort-by :index (vals (.index rdr)))))

(extend-type TwoBitReader
  protocols/IReader
  (reader-url [this] (.url this))
  (read
    ([this] (protocols/read this {}))
    ([this option] (protocols/read-all-sequences this option)))
  (indexed? [_] true)
  protocols/ISequenceReader
  (read-seq-summaries
    [this] (read-seq-summaries this))
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
    (^String [this region option]
     (read-sequence this region option))))

(defn reader ^TwoBitReader
  [f]
  (let [url (util/as-url f)]
    (with-open [ch (-> url
                       .toURI
                       Paths/get
                       (FileChannel/open
                        (into-array OpenOption [StandardOpenOption/READ])))]
      (let [buf (.map ch FileChannel$MapMode/READ_ONLY 0 (.size ch))
            _ (.order buf (case (.getInt buf)
                            0x1A412743 ByteOrder/BIG_ENDIAN
                            0x4327411A ByteOrder/LITTLE_ENDIAN))
            version (.getInt buf)
            n-seqs (.getInt buf)
            zero (.getInt buf)]
        (when-not (zero? version)
          (throw (ex-info "Version number must be zero."
                          {:input f, :url url, :version version})))
        (when-not (zero? zero)
          (throw (ex-info "sequenceCount must be followed by zero."
                          {:input f, :url url, :zero zero})))
        (TwoBitReader. buf url (read-file-index! buf n-seqs))))))

(defn clone-reader
  "Clones .2bit reader sharing persistent objects."
  ^TwoBitReader
  [^TwoBitReader rdr]
  (let [buf (doto (.duplicate ^ByteBuffer (.buf rdr))
              (.order (.order ^ByteBuffer (.buf rdr))))]
    (TwoBitReader. buf (.url rdr) (.index rdr))))
