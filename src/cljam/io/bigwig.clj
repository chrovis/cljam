(ns cljam.io.bigwig
  "Functions to read the bigWig format. See https://genome.ucsc.edu/goldenpath/help/bigWig.html
  and https://github.com/ucscGenomeBrowser/kent for the detail bigWig
  specifications."
  (:require [clojure.java.io :as cio]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.lsb :as lsb]
            [cljam.util :as util])
  (:import [java.net URL]
           [java.io Closeable IOException RandomAccessFile]
           [java.util.zip InflaterInputStream]))

(def ^:private bigwig-magic 0x888ffc26)

(def ^:private bpt-magic 0x78CA8C91)

(def ^:private cir-tree-magic 0x2468ACE0)

(declare read-tracks read-all-headers read-bbi-chrom-info read-cir-tree)

(defrecord FixedWidthHeader [magic version zoom-levels chromosome-tree-offset
                             full-data-offset full-index-offset
                             total-summary-offset uncompress-buf-size
                             extension-offset])

(defrecord ZoomHeader [reduction-level data-offset index-offset])

(defrecord TotalSummary [bases-covered min-val max-val sum-data sum-squared])

(defrecord ExtendedHeader [extension-size extra-index-count
                           extra-index-list-offset])

(defrecord BptHeader [block-size key-size val-size item-count root-offset])

(defrecord BbiChromInfo [name id size])

(defrecord CirTree [block-size item-count start-chrom-ix start-base
                    end-chrom-ix end-base file-size items-per-slot
                    root-offset])

; Currently, max number of zoom levels (i.e. max number of zoom headers) is 10,
; the numberais small. Therefore, we choose a record containing vector of zoom
; header instead of a flat record.
; Cf. https://github.com/ucscGenomeBrowser/kent/blob/3a0198acd1f859a603f5aad90188bee2d82efe0c/src/inc/bbiFile.h#L384
(defrecord BigWigHeaders [^FixedWidthHeader fixed-width-header
                          zoom-headers
                          ^TotalSummary total-summary
                          ^ExtendedHeader extended-header
                          ^BptHeader bpt-header
                          bbi-chrom-info
                          ^CirTree cir-tree])

(defrecord BIGWIGReader [^RandomAccessFile reader ^URL url ^BigWigHeaders headers]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (read-tracks this))
  (read [this option] (read-tracks this))
  (indexed? [_] false))

(defn ^BIGWIGReader reader
  "Returns an open cljam.io.bigwig.BIGWIGReader of f. Should be used inside with-open
  to ensure the reader is properly closed."
  [f]
  (let [f (.getAbsolutePath (cio/file f))
        reader (RandomAccessFile. f "r")
        headers (read-all-headers reader)]
    (BIGWIGReader. reader (util/as-url f) headers)))

(defn- check-bigwig-magic
  "Checks if the magic is right for bigWig format. Otherwise, throws IOException."
  [uint]
  (when-not (= uint bigwig-magic)
    (throw (IOException. "Invalid bigWig magic"))))

(defn- check-version
  "Ranged from [1,4]. Throws IOException if the version is out of range."
  [ushort]
  (when-not (and (<= 1 ushort) (<= ushort 4))
    (throw (IOException. "Invalid bigWig version"))))

(defn- check-field-count
  "For bigWig 0. Throws IOException if the fieldCount is invalid."
  [ushort]
  (when-not (zero? ushort)
    (throw (IOException. "Invalid bigWig fieldCount"))))

(defn- check-defined-field-count
  "For bigWig 0. Throws IOException if the definedFieldCount is invalid."
  [ushort]
  (when-not (zero? ushort)
    (throw (IOException. "Invalid bigWig definedFieldCount"))))

(defn- check-auto-sql-offset
  "For bigWig 0. Throws IOException if the autoSqlOffset is invalid."
  [^long long]
  (when-not (zero? long)
    (throw (IOException. "Invalid bigWig autoSqlOffset"))))

(defn- read-fixed-width-header
  "Returns a FixedWidthHeader."
  [^RandomAccessFile r]
  (let [magic (lsb/read-uint r)
        version (lsb/read-ushort r)
        zoom-levels (lsb/read-ushort r)
        chromosome-tree-offset (lsb/read-long r)
        full-data-offset (lsb/read-long r)
        full-index-offset (lsb/read-long r)
        field-count (lsb/read-ushort r)
        defined-field-count (lsb/read-ushort r)
        auto-sql-offset (lsb/read-long r)
        total-summary-offset (lsb/read-long r)
        uncompress-buf-size (lsb/read-uint r)
        extension-offset (lsb/read-long r)]
    (check-bigwig-magic magic)
    (check-version version)
    (check-field-count field-count)
    (check-defined-field-count defined-field-count)
    (check-auto-sql-offset auto-sql-offset)
    (FixedWidthHeader.
     magic version zoom-levels chromosome-tree-offset full-data-offset
     full-index-offset total-summary-offset uncompress-buf-size
     extension-offset)))

(defn- read-zoom-headers
  "Returns a vector of ZoomHeader from reader."
  [^RandomAccessFile r {:keys [zoom-levels]}]
  (letfn [(read-zoom-header [n acc]
            (if (zero? n)
              acc
              (let [reduction-level (lsb/read-uint r)
                    _reserved (lsb/read-uint r)
                    data-offset (lsb/read-long r)
                    index-offset (lsb/read-long r)]
                (recur (dec n)
                       (conj acc
                             (ZoomHeader.
                              reduction-level data-offset index-offset))))))]
    (read-zoom-header zoom-levels [])))

(defn- read-total-summary
  "Returns a totalSummay. If it isn't present, returns nil."
  [^RandomAccessFile r {:keys [total-summary-offset]}]
  (when-not (zero? total-summary-offset)
    (.seek r total-summary-offset)
    (let [bases-covered (lsb/read-long r)
          min-val (lsb/read-double r)
          max-val (lsb/read-double r)
          sum-data (lsb/read-double r)
          sum-squared (lsb/read-double r)]
      (TotalSummary.
       bases-covered min-val max-val sum-data sum-squared))))

(defn- read-extended-header
  "Returns an extendedHeader. It it isn't present, returns nil."
  [^RandomAccessFile r {:keys [extension-offset]}]
  (when-not (zero? extension-offset)
    (.seek r extension-offset)
    (let [extension-size (lsb/read-ushort r)
          extra-index-count (lsb/read-ushort r)
          extra-index-list-offset (lsb/read-long r)]
      (ExtendedHeader.
       extension-size extra-index-count extra-index-list-offset))))

(defn- check-bpt-magic
  "Checks if the magic is right for bpt format. Otherwise, throws IOException."
  [uint]
  (when-not (= uint bpt-magic)
    (throw (IOException. "Invalid bpt magic"))))

(defn- read-bpt-header
  "Returns B+ tree data."
  [^RandomAccessFile r {:keys [chromosome-tree-offset]}]
  (.seek r chromosome-tree-offset)
  (let [magic (lsb/read-uint r)
        block-size (lsb/read-uint r)
        key-size (lsb/read-uint r)
        val-size (lsb/read-uint r)
        item-count (lsb/read-long r)]
    (check-bpt-magic magic)
    (lsb/skip r 8)
    (let [root-offset (.getFilePointer r)]
      (BptHeader.
       block-size key-size val-size item-count root-offset))))

(defn- read-all-headers
  "Returns the all headers of bigWig format."
  [^RandomAccessFile r]
  (.seek r 0)
  (let [fixed-width-header (read-fixed-width-header r)
        zoom-headers (read-zoom-headers r fixed-width-header)
        total-summary (read-total-summary r fixed-width-header)
        extended-header (read-extended-header r fixed-width-header)
        bpt-header (read-bpt-header r fixed-width-header)
        bbi-chrom-info (read-bbi-chrom-info r bpt-header)
        cir-tree (read-cir-tree r fixed-width-header)]
    (BigWigHeaders.
     fixed-width-header zoom-headers total-summary extended-header bpt-header
     bbi-chrom-info cir-tree)))

(defn- read-c-style-string
  "Reads `length` bytes and returns a string. This function is useful for
  handling c-style null-terminated string."
  [^RandomAccessFile r ^long length]
  (->> (lsb/read-bytes r length)
       (reduce
        (fn [cs c]
          (if (zero? c)
            (reduced cs)
            (conj cs c)))
        [])
       (map char)
       (apply str)))

(defn- read-bbi-chrom-info-leaves
  "Returns the BbiChromInfo data of leaves."
  [^RandomAccessFile r key-size child-count]
  (repeatedly child-count
              (fn []
                (let [name (read-c-style-string r key-size)
                      id (lsb/read-uint r)
                      size (lsb/read-uint r)]
                  (BbiChromInfo.
                   name id size)))))

(defn- read-bbi-chrom-info-file-offsets
  "Skips offsets and returns the file offsets of children."
  [^RandomAccessFile r key-size child-count]
  (repeatedly child-count
              (fn []
                (lsb/skip r key-size)
                (lsb/read-long r))))

(defn- read-bbi-chrom-info
  "Returns a sequence of BbiChromInfo data."
  [^RandomAccessFile r {:keys [key-size root-offset]}]
  (letfn [(traverse [block-start]
            (.seek r block-start)
            (let [leaf? (-> r lsb/read-ubyte zero? not)
                  _reversed (lsb/read-ubyte r)
                  child-count (lsb/read-ushort r)]
              (if leaf?
                (doall (read-bbi-chrom-info-leaves r key-size child-count))
                (let [file-offsets (read-bbi-chrom-info-file-offsets r
                                                                     key-size
                                                                     child-count)]
                  (map traverse file-offsets)))))]
    (vec (traverse root-offset))))

(defn- check-cir-tree-magic
  "Checks if the magic is right for chromosome id r-tree index format.
  Otherwise, throws IOException."
  [uint]
  (when-not (= uint cir-tree-magic)
    (throw (IOException. "Invalid cir-tree magic"))))

(defn- read-cir-tree
  "Returns a CirTree data."
  [^RandomAccessFile r {:keys [full-index-offset]}]
  (.seek r full-index-offset)
  (let [magic (lsb/read-uint r)
        block-size (lsb/read-uint r)
        item-count (lsb/read-long r)
        start-chrom-ix (lsb/read-uint r)
        start-base (lsb/read-uint r)
        end-chrom-ix (lsb/read-uint r)
        end-base (lsb/read-uint r)
        file-size (lsb/read-long r)
        items-per-slot (lsb/read-uint r)]
    (check-cir-tree-magic magic)
    (lsb/skip r 4)
    (let [root-offset (.getFilePointer r)]
      (CirTree.
       block-size item-count start-chrom-ix start-base end-chrom-ix
       end-base file-size items-per-slot root-offset))))

(defn- cir-tree-overlaps?
  "Returns true if the given blocks are overlapped."
  [id start end start-chrom-ix start-base end-chrom-ix end-base]
  (letfn [(cmp [a-hi a-lo b-hi b-lo]
            (cond
              (< a-hi b-hi) 1
              (> a-hi b-hi) -1
              (< a-lo b-lo) 1
              (> a-lo b-lo) -1
              :else 0))]
    (and (pos? (cmp id start end-chrom-ix end-base))
         (neg? (cmp id end start-chrom-ix start-base)))))

(defn- cir-tree-leaves->blocks
  "Convert CirTree leaves into blocks that contain a flat map including offset and size."
  [^RandomAccessFile r id start end child-count]
  (->> (repeatedly child-count
                   (fn []
                     (let [start-chrom-ix (lsb/read-uint r)
                           start-base (lsb/read-uint r)
                           end-chrom-ix (lsb/read-uint r)
                           end-base (lsb/read-uint r)
                           offset (lsb/read-long r)
                           size (lsb/read-long r)]
                       (if (cir-tree-overlaps? id start end start-chrom-ix
                                               start-base end-chrom-ix end-base)
                         {:offset offset, :size size}))))
       (remove nil?)))

(defn- fetch-overlapping-blocks
  "Returns a sequence that contains overlapping blocks describing CirTree leaves."
  [^RandomAccessFile r id start end {:keys [root-offset]}]
  (letfn [(make-blocks [index-file-offset]
            (.seek r index-file-offset)
            (let [leaf? (-> r lsb/read-ubyte zero? not)
                  _reserved (lsb/read-ubyte r)
                  child-count (lsb/read-ushort r)]
              (if leaf?
                (doall (cir-tree-leaves->blocks r id start end child-count))
                (->> (repeatedly child-count
                                 (fn []
                                   (let [start-chrom-ix (lsb/read-uint r)
                                         start-base (lsb/read-uint r)
                                         end-chrom-ix (lsb/read-uint r)
                                         end-base (lsb/read-uint r)
                                         offset (lsb/read-uint r)]
                                     {:start-chrom-ix start-chrom-ix
                                      :start-base start-base
                                      :end-chrom-ix end-chrom-ix
                                      :end-base end-base
                                      :offset offset})))
                     doall
                     (sequence
                      (comp
                       (filter (fn [{:keys [start-chrom-ix start-base
                                            end-chrom-ix end-base]}]
                                 (cir-tree-overlaps? id start end
                                                     start-chrom-ix start-base
                                                     end-chrom-ix end-base)))
                       (map (fn [{:keys [offset]}] (recur offset)))))))))]
    (make-blocks root-offset)))

(defn- fetch-overlapping-blocks-group
  "Returns a sequence of blocks that describe overlapping chrom range."
  [^RandomAccessFile r ^BbiChromInfo chrom-info ^CirTree cir-tree]
  (fetch-overlapping-blocks r (:id chrom-info) 0 (:size chrom-info) cir-tree))

(defn- find-gap
  "Returns a map containing `before` and `after` blocks that have gaps between
  them."
  [block]
  (reduce
   (fn [m x]
     (if (not= (:offset x)
               (+ (-> m :before :offset) (-> m :before :size)))
       (reduced (assoc m :after x))
       (assoc m :before x)))
   {:before (first block)}
   (rest block)))

(defn- range-intersection
  "Returns a range intersection of two ranges that include `start` and `end`."
  [a b]
  (- (min (:end a) (:end b))
     (max (:start a) (:start b))))

(defn- ->bedgraph
  "Converts bigWig tracks into BedGraph format (0-based, half-open)."
  [is track-start track-end item-count chrom rng]
  (->> (repeatedly item-count
                   (fn []
                     (let [start (lsb/read-uint is)
                           end (lsb/read-uint is)
                           value (lsb/read-float is)]
                       (when (pos? (range-intersection rng {:start start :end end}))
                         {:track {:line nil :chr chrom :start track-start
                                  :end track-end}
                          :chr chrom :start start :end end :value value}))))
       (remove nil?)
       doall))

(defn- ->variable-step
  "Converts bigWig tracks into variableStep tracks of wig format
  (1-start, fully-closed)."
  [is item-span item-count chrom rng]
  (->> (repeatedly item-count
                   (fn []
                     (let [start (lsb/read-uint is)
                           value (lsb/read-float is)]
                       (when (pos? (range-intersection rng {:start start
                                                            :end (+ start item-span)}))
                         {:track {:line nil :format :variable-step :chr chrom
                                  :step nil :span item-span}
                          :chr chrom :start (inc start)
                          :end (+ start item-span) :value value}))))
       (remove nil?)
       doall))

(defn- ->fixed-step
  "Converts bigWig tracks into fixedStep tracks of wig format
  (1-start, fully-closed)."
  [is start item-step item-span item-count chrom rng]
  (reduce
   (fn [acc i]
     (let [value (lsb/read-float is)]
       (if (pos? (range-intersection rng {:start start, :end (+ start (* i item-step))}))
         (conj acc {:track {:line nil :format :fixed-step :chr chrom
                            :step item-step :span item-span}
                    :chr chrom :value value
                    :start (inc (+ start (* i item-step)))
                    :end (+ (+ start (* i item-step)) item-span)})
         acc)))
   []
   (range item-count)))

(defn- read-blocks
  "Reads blocks according to BbiChromInfo data and returns tracks described
  Wig or BedGraph format."
  [^RandomAccessFile r ^BbiChromInfo chrom-info ^URL url
   {:keys [fixed-width-header cir-tree]}]
  (let [blocks (fetch-overlapping-blocks-group r chrom-info cir-tree)
        rng {:start 0, :end (:size chrom-info)}]
    (letfn [(bigwig-> [offset after blocks acc]
              (let [block (first blocks)
                    rst (rest blocks)]
                (if (= block after)
                  [acc blocks]
                  (->> (with-open [is (cio/input-stream url)]
                         (lsb/skip is offset)
                         (with-open [is (InflaterInputStream. is)]
                           (let [chrom-id (lsb/read-uint is)
                                 start (lsb/read-uint is)
                                 end (lsb/read-uint is)
                                 item-step (lsb/read-uint is)
                                 item-span (lsb/read-uint is)
                                 typ (lsb/read-ubyte is)
                                 _reserved (lsb/read-ubyte is)
                                 item-count (lsb/read-ushort is)]
                             (case (int typ)
                               1 (->bedgraph is start end item-count
                                             (:name chrom-info) rng)
                               2 (->variable-step is item-span item-count
                                                  (:name chrom-info) rng)
                               3 (->fixed-step is start item-step item-span
                                               item-count (:name chrom-info)
                                               rng)
                               (throw (IOException.
                                       "Invalid type of bigWig section header."))))))
                       (concat acc)
                       (bigwig-> (+ offset (:size block)) after rst)
                       lazy-seq))))
            (aux [blocks acc]
              (if (empty? blocks)
                acc
                (let [{:keys [_before after]} (find-gap blocks)]
                  (let [[x new-blocks] (bigwig-> (-> blocks first :offset) after
                                                 blocks [])]
                    (lazy-seq
                     (aux new-blocks (concat acc x)))))))]
      (aux blocks []))))

(defn read-tracks
  "Reads a bigWig tracks from reader and returns a sequence of tracks
  representing Wig (fixedStep or variableStep) or BedGraph format."
  [^BIGWIGReader rdr]
  (let [r ^RandomAccessFile (.reader rdr)]
    (mapcat (fn [chrom-info]
              (read-blocks r chrom-info (.url rdr) (.headers rdr)))
            (:bbi-chrom-info (.headers rdr)))))
