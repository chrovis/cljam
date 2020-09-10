(ns cljam.util.intervals
  "Data structures for interval queries.")

(defprotocol IIntervals
  (find-overlap-intervals* [this start end]))

(deftype SortedMapIntervals [m]
  IIntervals
  (find-overlap-intervals* [this start end]
    (mapcat (fn [[_ x]] (drop-while #(< (:end %) start) x))
            (subseq m <= end))))

(defn- make-sorted-map-intervals
  [intervals]
  (->> (group-by :start intervals)
       (map (fn [[k vs]] [k (sort-by :end vs)]))
       (into (sorted-map))
       (->SortedMapIntervals)))

(defn index-intervals
  "Make indexes for intervals to find overlaps."
  [intervals]
  (->> intervals
       (group-by :chr)
       (into {} (map (fn [[k vs]] [k (make-sorted-map-intervals vs)])))))

(defn find-overlap-intervals
  "Find intervals that are on the given `chr` and overlap the given interval
  [`start` `end`] using indexes created by `index-intervals`."
  [indexed-intervals chr start end]
  (find-overlap-intervals* (get indexed-intervals chr) start end))
