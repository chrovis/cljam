(ns cljam.sorter
  (:refer-clojure :exclude [sort sorted?])
  (:require (cljam [sam :as sam]
                   [io :as io]))
  (:import java.util.List))

(defn- compkey-pos [hdr aln]
  [(.indexOf ^List (map :name (sam/make-refs hdr)) (:rname aln))
   (:pos aln)])

(defn- sort-alignments-by-pos [sam]
  (->> (sort-by (partial compkey-pos (:header sam)) (:alignments sam))
       (assoc sam :alignments)))

(defn- compkey-qname [hdr aln]
  [(.indexOf ^List (map :name (sam/make-refs hdr)) (:rname aln))
   (:qname aln)
   (bit-and (:flag aln) 0xc0)])

(defn- sort-alignments-by-qname [sam]
  (->> (sort-by (partial compkey-qname (:header sam)) (:alignments sam))
       (assoc sam :alignments)))

(defn- add-hd [sam vn so]
  (update-in sam [:header] conj {:HD {:VN vn, :SO so}}))

(defn sort-by-pos [sam]
  (-> (sort-alignments-by-pos sam)
      (add-hd sam/version "coordinate")))

(defn sort-by-qname [sam]
  (-> (sort-alignments-by-qname sam)
      (add-hd sam/version "queryname")))

(defn sort [sam]
  (sort-by-pos sam))

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
