(ns cljam.io.cram.encode.stats)

(defprotocol IStatsRecorder
  (stats [_])
  (update! [_ ri pos end nbases nrecords]))

(defrecord Stats [^long ri ^long start ^long end ^long nbases ^long nrecords])

(deftype StatsRecorder
         [^booleans refs
          ^:unsynchronized-mutable ^long start
          ^:unsynchronized-mutable ^long end
          ^:unsynchronized-mutable ^long nbases
          ^:unsynchronized-mutable ^long nrecords]
  IStatsRecorder
  (stats [_]
    (let [referred (into #{} (keep-indexed (fn [i r] (when r i))) refs)
          ri (cond (> (count referred) 1) -2
                   (contains? referred (dec (alength refs))) -1
                   :else (or (first referred) -1))]
      (->Stats ri
               (if (neg? ri) 0 start)
               (if (neg? ri) 0 end)
               nbases nrecords)))
  (update! [this ri pos end nbases nrecords]
    (let [ri (long ri)
          pos (long pos)
          end (long end)
          idx (if (neg? ri) (dec (alength refs)) ri)]
      (aset refs idx true)
      (when (pos? pos)
        (when (or (zero? (.-start this)) (< pos (.-start this)))
          (set! (.-start this) pos))
        (when (< (.-end this) end)
          (set! (.-end this) end)))
      (set! (.-nbases this) (+ (.-nbases this) (long nbases)))
      (set! (.-nrecords this) (+ (.-nrecords this) (long nrecords))))))

(defn make-stats-recorder
  "Creates a new alignment stats recorder."
  [nrefs]
  (->StatsRecorder (boolean-array (inc nrefs)) 0 0 0 0))

(defn merge-stats
  "Merges multiple alignment stats into one."
  [nrefs ss]
  (let [rec (make-stats-recorder nrefs)]
    (run! (fn [{:keys [ri start end nbases nrecords]}]
            (update! rec ri start end nbases nrecords))
          ss)
    (stats rec)))
