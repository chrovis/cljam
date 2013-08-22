(ns cljam.sorter
  (:refer-clojure :exclude [sort sorted?])
  (:require [cljam.sam :as sam]))

(defn- rnames [sam]
  (map :SN (:SQ (:header sam))))

(defn- sort-alignments-by-pos [sam]
  (let [get-order #(.indexOf (vec (rnames sam)) %)]
   (->> (sort-by #(vec [(get-order (:rname %)) (:pos %)]) (:alignments sam))
        (assoc sam :alignments))))

;;; FIXME: Invalid sorting algorithm
(defn- sort-alignments-by-qname [sam]
  (let [get-order #(.indexOf (vec (rnames sam)) %)]
   (->> (sort-by #(vec [(get-order (:rname %)) (:qname %)]) (:alignments sam))
        (assoc sam :alignments))))

(defn- add-hd [sam vn so]
  (update-in sam [:header] conj {:HD {:VN vn, :SO so}}))

(defn sort-by-pos [sam]
  (-> (sort-alignments-by-pos sam)
      (add-hd "1.4" "coordinate")))

(defn sort-by-qname [sam]
  (-> (sort-alignments-by-qname sam)
      (add-hd "1.4" "queryname")))

(defn sort [sam]
  (sort-by-pos sam))

(defn sorted? [sam]
  (let [so (:SO (:HD (:header sam)))]
    (or (= so "queryname")
        (= so "coordinate"))))

(defn sort-order [sam]
  (if-let [so (:SO (:HD (:header sam)))]
    so
    "unknown"))
