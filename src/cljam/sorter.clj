(ns cljam.sorter
  "Sorter of SAM/BAM format alignments."
  (:require [clojure.java.io :refer [file]]
            [clojure.tools.logging :as logging]
            [clojure.string :as cstr]
            (cljam [core :refer [sam-reader? sam-writer? bam-reader? bam-writer?]]
                   ;; [sam :as sam]
                   [bam :as bam]
                   [common :refer [version]]
                   [util :as util]
                   [io :as io])
            [cljam.util.sam-util :as sam-util]))

(def ^:private chunk-size 1500000)

(def order-unknown :unknown)
(def order-coordinate :coordinate)
(def order-queryname :queryname)

(defn- make-ref-map
  [hdr]
  (->> hdr
       sam-util/make-refs
       (map :name)
       (map-indexed (fn [i val] {val i}))
       (into {})))

;; Coordinate sorter
;; -----------------

(defn- replace-header [hdr vn so]
  (assoc hdr :HD {:VN vn, :SO so}))

(defn- compare-key-pos [ref-map b1 b2]
  (if (= (:rname b1) (:rname b2))
    (if (= (:pos b1) (:pos b2))
      (compare (:qname b1) (:qname b2))
      (compare (:pos b1) (:pos b2)))
    (cond
     (= (:rname b1) "*") 1
     (= (:rname b2) "*") -1
     :else (compare (get ref-map (:rname b1))
                    (get ref-map (:rname b2))))))

(defn- sort-alignments-by-pos [rdr]
  (let [ref-map (make-ref-map (io/read-header rdr))]
    (sort (partial compare-key-pos ref-map) (io/read-blocks rdr {:mode :coordinate}))))

;; Queryname sorter
;; ----------------

;; (defn- compkey-qname [hdr aln]
;;   [(.indexOf ^List (map :name (sam-util/make-refs hdr)) (:rname aln))
;;    (:qname aln)
;;    (bit-and (:flag aln) 0xc0)])

;; (defn- sort-alignments-by-qname [rdr]
;;   (->> (sort-by (partial compkey-qname (:header sam)) (:alignments sam))
;;        (assoc sam :alignments)))

;; split/merge
;; -----------

(defn gen-cache-filename
  [prefix i]
  (format "%s/%s_%05d.cache" util/temp-dir prefix i))

(defn gen-sorted-cache-filename
  [prefix i]
  (format "%s/%s_%05d.sorted.cache" util/temp-dir prefix i))

(defn- find-head
  [keyfn l]
  (reduce (fn [e1 e2]
            (let [v (keyfn e1 e2)]
              (cond
                (or (zero? v)
                    (neg? v)) e1
                (pos? v) e2)))
          l))

(defn- head
  [blocks ref-map]
  (->> blocks
       (map-indexed vector)
       (filter (fn [[_ b]] (not (nil? b))))
       (find-head (fn [[idx1 b1] [idx2 b2]]
                    (compare-key-pos ref-map b1 b2)))))

(defn split-sam
  [rdr name-fn]
  (let [hdr (io/read-header rdr)]
    (doall
     (->> (io/read-blocks rdr)
          (partition-all chunk-size)
          (map-indexed vector)
          (map (fn [[i blks]]
                 (let [f (name-fn i)]
                   (with-open [wtr (bam/writer f)]
                     (io/write-header wtr hdr)
                     (io/write-refs wtr hdr)
                     (io/write-blocks wtr blks))
                   f)))))))

(defn merge-sam
  [wtr header name-fn n]
  (let [rdrs (map #(bam/reader (name-fn %) :ignore-index true) (range n))
        ref-map (make-ref-map header)]
    (io/write-header wtr header)
    (io/write-refs wtr header)
    (loop [blocks-list (map #(io/read-blocks % {:mode :coordinate}) rdrs)]
      (let [candidates (map first blocks-list)]
        (when-not (every? nil? candidates)
          (let [[i b] (head candidates ref-map)]
            (io/write-blocks wtr [b])
            (recur (map
                    (fn [[idx blocks]]
                      (if (= idx i)
                        (drop 1 blocks)
                        blocks))
                    (map-indexed vector blocks-list)))))))))

(defn clean-split-merge-cache
  [name-fn sorted-name-fn count]
  (letfn [(clean [path]
            (let [f (file path)]
              (when (.exists f)
                (.delete f))))]
    (doseq [i (range count)]
      (clean (name-fn i))
      (clean (sorted-name-fn i)))))

;; Sorter
;; ------

(defmulti sort-by-pos (fn [rdr wtr]
                        (cond
                          (and (sam-reader? rdr) (sam-writer? wtr)) :sam
                          (and (bam-reader? rdr) (bam-writer? wtr)) :bam
                          :else nil)))

(defmethod sort-by-pos :sam [rdr wtr]
  (throw (ex-info "SAM not supported yet." {:type :sam-not-supported}))
  ;; TODO implement
  )

(defmethod sort-by-pos :bam [rdr wtr]
  (when (and (bam-reader? rdr)
             (bam-writer? wtr))
    (let [basename (util/basename (.getName (file (io/reader-path rdr))))
          cache-name-fn (partial gen-cache-filename basename)
          sorted-cache-name-fn (partial gen-sorted-cache-filename basename)
          splited-files (split-sam rdr cache-name-fn)
          num-splited (count splited-files)
          hdr (replace-header (into (sorted-map) (io/read-header rdr))
                              version
                              (name order-coordinate))]
      (doseq [idxs (partition-all util/num-cores (range num-splited))]
        (doall
         (pmap
          (fn [i]
            (let [r (bam/reader (cache-name-fn i) :ignore-index true)
                  blks (sort-alignments-by-pos r)]
              (with-open [w (bam/writer (sorted-cache-name-fn i))]
                (io/write-header w hdr)
                (io/write-refs w hdr)
                (io/write-blocks w blks))))
          idxs)))
      (merge-sam wtr hdr sorted-cache-name-fn num-splited)
      (clean-split-merge-cache cache-name-fn sorted-cache-name-fn num-splited))))

(defn sort-by-qname [rdr wtr]
  (throw (ex-info "Query name sortor is not available yet." {:type :method-not-available}))
  ;; (let [alns (sort-alignments-by-qname rdr)
  ;;       hdr (replace-header (into (sorted-map) (io/read-header rdr))
  ;;                           version
  ;;                           (name order-queryname))]
  ;;   (io/write-header wtr hdr)
  ;;   (io/write-refs wtr hdr)
  ;;   (io/write-alignments wtr alns hdr))
  )

(defn sorted-by?
  "Returns true if the sam is sorted, false if not. It is detected by
  `@HD SO:***` tag in the header."
  [rdr]
  (let [so (:SO (:HD (io/read-header rdr)))]
    (or (= so (name order-queryname))
        (= so (name order-coordinate)))))

(defn sort-order
  "Returns sorting order of the sam as Keyword. Returning order is one of the
  following: :queryname, :coordinate, :unsorted, :unknown ."
  [rdr]
  (if-let [so (:SO (:HD (io/read-header rdr)))]
    (keyword so)
    order-unknown))
