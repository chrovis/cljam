(ns cljam.sorter
  (:refer-clojure :exclude [sort sorted?])
  (:require (cljam [common :refer [version]]
                   [util :as util]
                   [io :as io]))
  (:import java.util.List))

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
                            version "coordinate")]
    (with-open [wtr wtr]
      (io/write-header wtr hdr)
      (io/write-refs wtr hdr)
      (io/write-alignments wtr alns hdr))))

(defn sort-by-qname [rdr wtr]
  (sort-alignments-by-qname rdr)
  ;(add-hd version "queryname")
  )

(defn sort [rdr wtr]
  (sort-by-pos rdr wtr))

(defn sorted?
  "Returns true if the sam is sorted, false if not. It is detected by
  `@HD SO:***` tag in the header."
  [sam]
  (let [so (:SO (:HD (:header sam)))]
    (or (= so "queryname")
        (= so "coordinate"))))

(defn sort-order
  "Returns sorting order of the sam as String. Returning order is one of the
  following: \"queryname\", \"coordinate\", \"unsorted\", \"unknown\"."
  [sam]
  (if-let [so (:SO (:HD (:header sam)))]
    so
    "unknown"))
