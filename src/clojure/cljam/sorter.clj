(ns cljam.sorter
  (:refer-clojure :exclude [sort sorted?])
  (:require (cljam [sam :as sam]
                   [bam :as bam]
                   [common :refer [version]]
                   [util :as util]
                   [io :as io]))
  (:import java.util.List))

(def order-unknown "unknown")
(def order-coordinate "coordinate")
(def order-queryname "queryname")

;;
;; split/merge
;;

(def ^:private chunk-size 500000)

(defn split-sam
  [rdr prefix]
  (doall
   (map
    (fn [[i blks]]
      (let [f (format "%s/%s_%05d.cache" util/temp-dir prefix i)
            hdr (io/read-header rdr)]
        (println f)
        (with-open [wtr (bam/writer f)]
          (io/write-header wtr hdr)
          (io/write-refs wtr hdr)
          (io/write-blocks wtr blks))
        f))
    (map-indexed vector (partition-all chunk-size (io/read-blocks rdr))))))

;;
;; sorter
;;

(defn- replace-header [hdr vn so]
  (conj hdr {:HD {:VN vn, :SO so}}))

(defn- compkey-pos [hdr aln]
  [(.indexOf ^List (map :name (util/make-refs hdr)) (:rname aln))
   (:pos aln)])

(defn- sort-alignments-by-pos [rdr]
  (sort-by (partial compkey-pos (io/read-header rdr)) (io/read-alignments rdr {}))
  ;; (->> (sort-by (partial compkey-pos (:header sam)) (:alignments sam))
  ;;      (assoc sam :alignments))
  )

(defn- compkey-qname [hdr aln]
  [(.indexOf ^List (map :name (util/make-refs hdr)) (:rname aln))
   (:qname aln)
   (bit-and (:flag aln) 0xc0)])

(defn- sort-alignments-by-qname [rdr]
  ;; (->> (sort-by (partial compkey-qname (:header sam)) (:alignments sam))
  ;;      (assoc sam :alignments))
  ;; TODO
  )

(defn sort-by-pos [rdr wtr]
  (let [alns (sort-alignments-by-pos rdr)
        hdr (replace-header (io/read-header rdr)
                            version order-coordinate)]
    (with-open [wtr wtr]
      (io/write-header wtr hdr)
      (io/write-refs wtr hdr)
      (io/write-alignments wtr alns hdr))))

(defn sort-by-qname [rdr wtr]
  (let [alns (sort-alignments-by-qname rdr)
        hdr (replace-header (io/read-header rdr)
                            version order-queryname)]
    (with-open [wtr wtr]
      (io/write-header wtr hdr)
      (io/write-refs wtr hdr)
      (io/write-alignments wtr alns hdr))))

(defn sort [rdr wtr]
  (sort-by-pos rdr wtr))

(defn sorted?
  "Returns true if the sam is sorted, false if not. It is detected by
  `@HD SO:***` tag in the header."
  [rdr]
  (let [so (:SO (:HD (io/read-header rdr)))]
    (or (= so order-queryname)
        (= so order-coordinate))))

(defn sort-order
  "Returns sorting order of the sam as String. Returning order is one of the
  following: \"queryname\", \"coordinate\", \"unsorted\", \"unknown\"."
  [rdr]
  (if-let [so (:SO (:HD (io/read-header rdr)))]
    so
    order-unknown))
