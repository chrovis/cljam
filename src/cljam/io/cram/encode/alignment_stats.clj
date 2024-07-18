(ns cljam.io.cram.encode.alignment-stats
  (:import [java.util HashMap]))

(defn- update-single-alignment-span! [^longs cur-span ^long pos ^long end]
  (let [cur-pos (aget cur-span 0)]
    (when (pos? pos)
      (when (or (zero? cur-pos) (< pos cur-pos))
        (aset cur-span 0 pos))
      (when (< (aget cur-span 1) end)
        (aset cur-span 1 end)))))

(defprotocol IAlignmentStatsBuilder
  (update! [_ ri pos end nbases nrecords])
  (build [_]))

(defrecord AlignmentStats [^long ri ^long start ^long end ^long nbases ^long nrecords])

(deftype AlignmentStatsBuilder
         [^:unsynchronized-mutable first?
          ^:unsynchronized-mutable ^long ref-index
          ^:unsynchronized-mutable ^long nbases
          ^:unsynchronized-mutable ^long nrecords
          ^longs span]
  IAlignmentStatsBuilder
  (build [_]
    (let [ri (if first? -1 ref-index)]
      (->AlignmentStats ri
                        (if (neg? ri) 0 (aget span 0))
                        (if (neg? ri) 0 (aget span 1))
                        nbases nrecords)))
  (update! [this ri pos end nbases nrecords]
    (let [ri (long ri)]
      (cond first?
            (do (set! ref-index ri)
                (set! first? false))

            (not= ri ref-index)
            (set! ref-index -2))
      (when (>= ref-index 0)
        (update-single-alignment-span! span pos end))
      (set! (.-nbases this) (+ (.-nbases this) (long nbases)))
      (set! (.-nrecords this) (+ (.-nrecords this) (long nrecords))))))

(defn make-alignment-stats-builder
  "Creates a new alignment stats builder."
  []
  (->AlignmentStatsBuilder true 0 0 0 (long-array 2)))

(defn merge-stats
  "Merges multiple alignment stats into one."
  [ss]
  (let [builder (make-alignment-stats-builder)]
    (run! (fn [{:keys [ri start end nbases nrecords]}]
            (update! builder ri start end nbases nrecords))
          ss)
    (build builder)))

(defprotocol IAlignmentSpansBuilder
  (build-spans [_])
  (update-span! [_ ri pos end]))

(defn- ref-index-comparator [^long ri1 ^long ri2]
  (cond (= ri1 ri2) 0
        (= ri1 -1) 1
        (= ri2 -1) -1
        :else (compare ri1 ri2)))

(defn make-alignment-spans-builder
  "Creates a new alignment spans builder."
  []
  (let [ri->span (HashMap.)]
    (reify IAlignmentSpansBuilder
      (build-spans [_]
        (into (sorted-map-by ref-index-comparator)
              (map (fn [[ri ^longs span]]
                     (let [start (aget span 0)
                           end (aget span 1)]
                       [ri {:start start, :span (inc (- end start))}])))
              ri->span))
      (update-span! [_ ri pos end]
        (let [span (or (get ri->span ri)
                       (let [span (long-array 2)]
                         (.put ri->span ri span)
                         span))]
          (update-single-alignment-span! span pos end))))))
