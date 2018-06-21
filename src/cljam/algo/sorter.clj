(ns cljam.algo.sorter
  "Sorter of the SAM/BAM format alignments."
  (:refer-clojure :exclude [sorted?])
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [com.climate.claypoole :as cp]
            [cljam.io.protocols :as protocols]
            [cljam.io.sam :as sam]
            [cljam.io.util :as io-util]
            [cljam.util :as util]
            [cljam.io.sam.util.header :as header])
  (:import [java.io Closeable File]
           [java.util PriorityQueue]))

(def ^:const default-chunk-size 1500000)

(defn- refmap
  [refs]
  (into {} (map-indexed (fn [i x] [(:name x) i])) refs))

;; comparators
;; -----------

(defn- coordinate-key
  [rname->id {:keys [rname ref-id ^int pos ^int flag]}]
  (unchecked-add
   Long/MIN_VALUE
   (bit-or
    (bit-shift-left (int (or ref-id (rname->id rname -1))) 32)
    (bit-shift-left pos 1)
    (bit-and 1 (unsigned-bit-shift-right flag 4)))))

(defn- queryname-key
  [{:keys [qname ^int flag]}]
  (str
   qname
   (bit-and 3 (unsigned-bit-shift-right flag 6))))

(defn- sort-by-index
  "Same as sort-by but caching results of keyfn."
  [keyfn coll]
  (sort-by :index (map (fn [x] (assoc x :index (keyfn x))) coll)))

;; split/merge
;; -----------

(defn- same-type?
  "Returns true if given files are the same type."
  [f & files]
  (apply = (io-util/file-type f) (map io-util/file-type files)))

(defn- split**
  "Splits SAM/BAM file into multiple files each containing alignments up to chunk-size.
  name-fn must be a function taking a int value i and returning a path string for i-th output.
  read-fn must produce a sequence of alignments and write-fn must consume the splitted sequence."
  [rdr mode chunk-size name-fn read-fn write-fn]
  (let [hdr (header/sorted-by mode (header/update-version (sam/read-header rdr)))]
    (logging/info "Splitting...")
    (cp/with-shutdown! [p (cp/threadpool (cp/ncpus))]
      (->> (read-fn rdr)
           (sequence
            (comp
             (partition-all (or chunk-size default-chunk-size))
             (map-indexed (fn [i xs] [(name-fn i) xs]))))
           (cp/pmap
            p
            (fn [[f xs]]
              (logging/info (str "Sorting " (count xs) " alignments..."))
              (with-open [wtr (sam/writer f)]
                (sam/write-header wtr hdr)
                (sam/write-refs wtr hdr)
                (write-fn wtr xs hdr))
              f))
           doall))))

(defn- split*
  "Splits SAM/BAM files with appropriate reader/writer functions."
  [rdr chunk-size name-fn mode sort-fn]
  (let [block? (same-type? (protocols/reader-url rdr) (name-fn 0))
        read-fn (if block?
                  (fn [r] (sam/read-blocks r {} {:mode mode}))
                  sam/read-alignments)
        write-fn (if block?
                   (fn [w xs _] (sam/write-blocks w (sort-fn xs)))
                   (fn [w xs hdr] (sam/write-alignments w (sort-fn xs) hdr)))]
    (split** rdr mode chunk-size name-fn read-fn write-fn)))

(defn- head-pq
  "Take a smallest element from sequences in priority queue."
  [^PriorityQueue q]
  (when-not (.isEmpty q)
    (let [x (.poll q)]
      (when-let [nx (next x)]
        (.add q nx))
      (first x))))

(defn merge-sorted-seqs-by
  "Returns a lazy sequence from pre-sorted sequences.
  Each sequences must be sorted by key-fn.
  Returns first sequence if only 1 sequence is given."
  [key-fn seqs]
  (if (= (count seqs) 1)
    (first seqs)
    (let [pq (PriorityQueue.
              (int (count seqs))
              (fn pq-cmp [x y] (compare (key-fn (first x)) (key-fn (first y)))))]
      (doseq [s seqs]
        (.add pq s))
      (take-while some? (repeatedly (fn repeat-head-pq [] (head-pq pq)))))))

(defn- merge**
  "Merges multiple SAM/BAM files into single SAM/BAM file."
  [wtr hdr files key-fn read-fn write-fn]
  (let [rdrs (map sam/reader files)
        alns (map (comp seq read-fn) rdrs)]
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    (write-fn wtr (merge-sorted-seqs-by key-fn alns) hdr)
    (doseq [rdr rdrs]
      (.close ^Closeable rdr))))

(defn- merge*
  "Merges multiple SAM/BAM files with appropriate reader/writer functions."
  [wtr hdr files mode key-fn]
  (let [block? (apply same-type? (protocols/writer-url wtr) files)
        read-fn (if block?
                  (fn [r] (sam/read-blocks r {} {:mode mode}))
                  sam/read-alignments)
        write-fn (if block?
                   (fn [w xs _] (sam/write-blocks w xs))
                   sam/write-alignments)]
    (merge** wtr hdr files key-fn read-fn write-fn)))

;; Sorter
;; ------

(defn- clean-all!
  "Deletes all files in list."
  [files]
  (doseq [f (map cio/file files)
          :when (.exists ^File f)]
    (.delete ^File f)))

(defn- gen-cache-filename
  "Generates i-th cache filename."
  [fmt prefix i]
  (format "%s/%s_%05d.%s" util/temp-dir prefix i fmt))

(defn sort!
  "Sorts alignments of rdr by mode and writes them to wtr.
  :coordinate and :queryname are available for mode."
  [rdr wtr {:keys [mode chunk-size cache-fmt] :or {cache-fmt :bam}}]
  (let [name-fn (->> rdr
                     protocols/reader-url
                     util/basename
                     (partial gen-cache-filename (name cache-fmt)))
        key-fn ({header/order-coordinate (partial coordinate-key (refmap (sam/read-refs rdr)))
                 header/order-queryname queryname-key} mode)
        splitted-files (split* rdr chunk-size name-fn mode #(sort-by-index key-fn %))
        hdr (header/sorted-by mode (header/update-version (sam/read-header rdr)))]
    (logging/info (str "Merging from " (count splitted-files) " files..."))
    (merge* wtr hdr splitted-files mode key-fn)
    (logging/info "Deleting cache files...")
    (clean-all! splitted-files)))

(defn sort-by-pos
  "Sorts alignments by chromosomal position."
  [rdr wtr & [option]]
  (sort! rdr wtr (into (or option {}) {:mode header/order-coordinate})))

(defn sort-by-qname
  "Sort alignments by query name."
  [rdr wtr & [option]]
  (sort! rdr wtr (into (or option {}) {:mode header/order-queryname})))

(defn sorted?
  "Returns true if the sam is sorted, false if not. It is detected by
  `@HD SO:***` tag in the header."
  [rdr]
  (header/sorted? (sam/read-header rdr)))

(defn sort-order
  "Returns sorting order of the sam as Keyword. Returning order is one of the
  following: :queryname, :coordinate, :unsorted, :unknown."
  [rdr]
  (header/sort-order (sam/read-header rdr)))
