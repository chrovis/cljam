(ns cljam.io.cram.encode.alignment-stats)

(defprotocol IAlignmentStatsBuilder
  (update! [_ ri pos end nbases nrecords])
  (build [_]))

(defrecord AlignmentStats [^long ri ^long start ^long end ^long nbases ^long nrecords])

(deftype AlignmentStatsBuilder
         [^:unsynchronized-mutable first?
          ^:unsynchronized-mutable ^long ref-index
          ^:unsynchronized-mutable ^long start
          ^:unsynchronized-mutable ^long end
          ^:unsynchronized-mutable ^long nbases
          ^:unsynchronized-mutable ^long nrecords]
  IAlignmentStatsBuilder
  (build [_]
    (let [ri (if first? -1 ref-index)]
      (->AlignmentStats ri
                        (if (neg? ri) 0 start)
                        (if (neg? ri) 0 end)
                        nbases nrecords)))
  (update! [this ri pos end nbases nrecords]
    (let [ri (long ri)
          pos (long pos)
          end (long end)]
      (cond first?
            (do (set! ref-index ri)
                (set! first? false))

            (not= ri ref-index)
            (set! ref-index -2))
      (when (and (>= ref-index 0) (pos? pos))
        (when (or (zero? (.-start this)) (< pos (.-start this)))
          (set! (.-start this) pos))
        (when (< (.-end this) end)
          (set! (.-end this) end)))
      (set! (.-nbases this) (+ (.-nbases this) (long nbases)))
      (set! (.-nrecords this) (+ (.-nrecords this) (long nrecords))))))

(defn make-alignment-stats-builder
  "Creates a new alignment stats builder."
  []
  (->AlignmentStatsBuilder true 0 0 0 0 0))

(defn merge-stats
  "Merges multiple alignment stats into one."
  [ss]
  (let [builder (make-alignment-stats-builder)]
    (run! (fn [{:keys [ri start end nbases nrecords]}]
            (update! builder ri start end nbases nrecords))
          ss)
    (build builder)))
