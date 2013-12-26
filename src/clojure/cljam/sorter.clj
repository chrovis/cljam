(ns cljam.sorter
  (:require [clojure.java.io :refer [file]]
            [clojure.string :as str]
            (cljam [sam :as sam]
                   [bam :as bam]
                   [common :refer [version]]
                   [util :as util]
                   [io :as io])
            [cljam.util.sam-util :as sam-util]))

(def ^:private chunk-size 1500000)

(def order-unknown :unknown)
(def order-coordinate :coordinate)
(def order-queryname :queryname)

;;
;; coordinate sorter
;;

(defn- replace-header [hdr vn so]
  (conj hdr {:HD {:VN vn, :SO so}}))

(defn- compare-key-pos [ref-map b1 b2]
  (if (= (:rname b1) (:rname b2))
    (compare (:pos b1) (:pos b2))
    (cond
     (= (:rname b1) "*") 1
     (= (:rname b2) "*") -1
     :else (compare (get ref-map (:rname b1))
                    (get ref-map (:rname b2))))))

(defn- sort-alignments-by-pos [rdr]
  (let [ref-map (apply merge
                       (map-indexed (fn [i val] {val i})
                                    (map :name (sam-util/make-refs (io/read-header rdr)))))]
    (sort (partial compare-key-pos ref-map) (io/read-coordinate-blocks rdr))))

;;
;; queryname sorter
;;

;; (defn- compkey-qname [hdr aln]
;;   [(.indexOf ^List (map :name (sam-util/make-refs hdr)) (:rname aln))
;;    (:qname aln)
;;    (bit-and (:flag aln) 0xc0)])

;; FIXME
(defn- sort-alignments-by-qname [rdr]
  ;; (->> (sort-by (partial compkey-qname (:header sam)) (:alignments sam))
  ;;      (assoc sam :alignments))
  )

;;
;; split/merge
;;

(defn gen-cache-filename
  [prefix i]
  (format "%s/%s_%05d.cache" util/temp-dir prefix i))

(defn gen-sorted-cache-filename
  [prefix i]
  (format "%s/%s_%05d.sorted.cache" util/temp-dir prefix i))

(defn- find-head
  ([keyfn l]
     (reduce (fn [e1 e2]
               (condp = (compare e1 e2)
                 0 e1
                 1 e2
                 -1 e1))
             l)))

(defn- head
  [blocks ref-map]
  (find-head
         (fn [[idx1 b1] [idx2 b2]]
           (compare-key-pos ref-map b1 b2))
         (filter
          (fn [[_ b]] (not (nil? b)))
          (map-indexed vector blocks))))

(defn split-sam
  [rdr name-fn]
  (doall
   (map
    (fn [[i blks]]
      (let [f (name-fn i)
            hdr (io/read-header rdr)]
        (with-open [wtr (bam/writer f)]
          (io/write-header wtr hdr)
          (io/write-refs wtr hdr)
          (io/write-blocks wtr blks))
        f))
    (map-indexed vector (partition-all chunk-size (io/read-blocks rdr))))))

(defn merge-sam
  [wtr header name-fn n]
  (let [rdrs (map #(bam/reader (name-fn %)) (range n))
        ref-map (apply merge
                       (map-indexed (fn [i val] {val i})
                                    (map :name (sam-util/make-refs header))))]
    (with-open [wtr wtr]
      (io/write-header wtr header)
      (io/write-refs wtr header)
      (loop [blocks-list (map #(io/read-coordinate-blocks %) rdrs)]
        (let [candidates (map first blocks-list)]
          (when-not (every? nil? candidates)
            (let [[i b] (head candidates ref-map)]
              (io/write-coordinate-blocks wtr [b])
              (recur (map
                      (fn [[idx blocks]]
                        (if (= idx i)
                          (drop 1 blocks)
                          blocks))
                      (map-indexed vector blocks-list))))))))))

(defn clean-split-merge-cache
  [name-fn sorted-name-fn count]
  (let [clean (fn [path]
                (let [f (file path)]
                  (when (.exists f)
                    (.delete f))))]
    (doseq [i (range count)]
      (clean (name-fn i))
      (clean (sorted-name-fn i)))))

;;
;; sorter
;;

(defn sort-by-pos [rdr wtr]
  (let [filename (.getName (file (io/reader-path rdr)))
        basename (first (str/split filename #"\.(?=[^\.]+$)"))
        cache-name-fn (partial gen-cache-filename basename)
        sorted-cache-name-fn (partial gen-sorted-cache-filename basename)
        splited-files (split-sam rdr cache-name-fn)
        num-splited (count splited-files)
        hdr (replace-header (io/read-header rdr)
                            version
                            (name order-coordinate))]
    (doall
     (pmap
      (fn [i]
        (let [r (bam/reader (cache-name-fn i))
              blks (sort-alignments-by-pos r)]
          (with-open [w (bam/writer (sorted-cache-name-fn i))]
            (io/write-header w hdr)
            (io/write-refs w hdr)
            (io/write-coordinate-blocks w blks))))
      (range num-splited)))
    (merge-sam wtr hdr sorted-cache-name-fn num-splited)
    (clean-split-merge-cache cache-name-fn sorted-cache-name-fn num-splited)))

(defn sort-by-qname [rdr wtr]
  (let [alns (sort-alignments-by-qname rdr)
        hdr (replace-header (io/read-header rdr)
                            version
                            (name order-queryname))]
    (with-open [wtr wtr]
      (io/write-header wtr hdr)
      (io/write-refs wtr hdr)
      (io/write-alignments wtr alns hdr))))

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
