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

(defn- find-nclist-overlap-intervals [^clojure.lang.Sorted nclist start end]
  (when-let [target-intervals
             (and nclist
                  (->> (subseq nclist  >= start)
                       (map second)
                       (take-while #(<= (:start (first %)) end))))]
    (mapcat #(cons (first %)
                   (find-nclist-overlap-intervals (second %)
                                                  start end))
            target-intervals)))

(deftype NclistIntervals [nclist]
  IIntervals
  (find-overlap-intervals* [this start end]
    (find-nclist-overlap-intervals nclist start end)))

(defn- compare-nclist [a b]
  (let [res (Long/signum (- (:start a) (:start b)))]
    (if (zero? res)
      (Long/signum (- (:end b) (:end a)))
      res)))
(declare make-nclist*)

(defn- make-nested-intervals [intervals]
  (when (seq intervals)
    (let [head (first intervals)
          [nested-intervals rests]
          (split-with #(<= (:start  head) (:start %) (:end %) (:end head))
                      (next intervals))]
      (cons [(:end head) [head (make-nclist* nested-intervals)]]
            (lazy-seq (make-nested-intervals rests))))))

(defn- make-nclist* [intervals]
  (let [sorted-intervals (sort compare-nclist intervals)]
    (into (sorted-map) (make-nested-intervals sorted-intervals))))

(defn- make-nclist [intervals]
  (->NclistIntervals (make-nclist* intervals)))

(defn index-intervals
  "Make indexes for intervals to find overlaps."
  [intervals {:keys [structure] :or {structure :sorted-map}}]
  (->> intervals
       (group-by :chr)
       (into {} (map (fn [[k vs]]
                       [k
                        (case structure
                          :sorted-map (make-sorted-map-intervals vs)
                          :nclist (make-nclist vs))])))))

(defn find-overlap-intervals
  "Find intervals that are on the given `chr` and overlap the given interval
  [`start` `end`] using indexes created by `index-intervals`."
  [indexed-intervals chr start end]
  (find-overlap-intervals* (get indexed-intervals chr) start end))
