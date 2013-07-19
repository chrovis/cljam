(ns cljam.sorter
  (:refer-clojure :exclude [sort])
  (:import cljam.sam.SamHeader))

(defn- rnames [sam]
  (map #(:SN (:SQ %)) (seq (:header sam))))

(defn- sort-alignments-by-pos [sam]
  (let [get-order #(.indexOf (vec (rnames sam)) %)]
   (->> (sort-by #(vec [(get-order (:rname %)) (:pos %)]) (:alignments sam))
        (assoc sam :alignments))))

(defn- sort-alignments-by-qname [sam]
  (let [get-order #(.indexOf (vec (rnames sam)) %)]
   (->> (sort-by #(vec [(get-order (:rname %)) (:qname %)]) (:alignments sam))
        (assoc sam :alignments))))

(defn- add-hd [sam vn so]
  (assoc sam :header (cons (assoc (SamHeader.) :HD {:VN vn, :SO so}) (:header sam))))

(defn sort-by-pos [sam]
  (-> (sort-alignments-by-pos sam)
      (add-hd "1.3" "coordinate")))

(defn sort-by-qname [sam]
  (-> (sort-alignments-by-qname sam)
      (add-hd "1.3" "queryname")))
