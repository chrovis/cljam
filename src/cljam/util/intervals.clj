(ns cljam.util.intervals
  "Data structures for interval queries.")

(defprotocol IIntervals
  (find-overlap-intervals [this chr start end]
    "Finds intervals that is overlap [start end]."))

(deftype Sorted-map-intervals [^clojure.lang.Sorted smap]
  IIntervals
  (find-overlap-intervals [this chr start end]
    (mapcat (fn [[_ ls]] (drop-while #(> start (:end %)) ls))
            (subseq (get (.smap this) chr) <= end))))

(defn make-sorted-map-intervals
  "Returns Sorted-map-intervals that has a sorted-map (start is key)
   for eaach chr calculated from intervals."
  [intervals]
  (->> (group-by :chr intervals)
       (into {}
             (map
              (fn [[chr xs]]
                [chr
                 (->> (group-by :start xs)
                      (map (fn [[k vs]] [k (sort-by :end vs)]))
                      (into (sorted-map)))])))
       Sorted-map-intervals.))
