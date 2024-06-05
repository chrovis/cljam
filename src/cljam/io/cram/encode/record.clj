(ns cljam.io.cram.encode.record
  (:require [cljam.io.sam.util.flag :as sam.flag]
            [cljam.io.sam.util.option :as sam.option]))

(defn- make-alignment-stats [^long nrefs]
  (let [refs (boolean-array (inc nrefs))
        stats (long-array 4)]
    (fn
      ([]
       (let [referred (into #{} (keep-indexed (fn [i r] (when r i))) refs)]
         {:ref-seq-id (cond (> (count referred) 1) -2
                            (contains? referred nrefs) -1
                            :else (or (first referred) -1))
          :start (aget stats 0)
          :span (- (aget stats 1) (aget stats 0))
          :bases (aget stats 2)
          :records (aget stats 3)}))
      ([{^long ri ::ref-index ^long end ::end :keys [^long pos] :as record}]
       (let [idx (if (neg? ri) nrefs ri)]
         (aset refs idx true)
         (when (pos? pos)
           (when (< pos (aget stats 0))
             (aset stats 0 pos))
           (when (< (aget stats 1) end)
             (aset stats 1 end)))
         (aset stats 2 (+ (aget stats 2) (long (count (:seq record)))))
         (aset stats 3 (inc (aget stats 3))))))))

(defn- ref-index [rname->idx rname]
  (if  (= rname "*")
    -1
    (get rname->idx rname)))

(defn- build-positional-data-encoder [cram-header {:keys [RI RL AP RG]}]
  (let [rg-id->idx (into {}
                         (map-indexed (fn [i {:keys [ID]}] [ID i]))
                         (:RG cram-header))]
    (fn [record]
      (let [rg (sam.option/value-for-tag :RG record)]
        (RI (::ref-index record))
        (RL (count (:seq record)))
        (AP (:pos record))
        (RG (if rg (get rg-id->idx rg) -1))))))

(defn- build-read-name-encoder [{:keys [RN]}]
  (fn [record]
    (RN (.getBytes ^String (:qname record)))))

(defn- build-mate-read-encoder [rname->idx {:keys [MF NS NP TS]}]
  (fn [{:keys [^long flag rnext] :as record}]
    (let [mate-flag (cond-> 0
                      (pos? (bit-and flag (sam.flag/encoded #{:next-reversed})))
                      (bit-or 0x01)

                      (pos? (bit-and flag (sam.flag/encoded #{:next-unmapped})))
                      (bit-or 0x02))]
      (MF mate-flag)
      (NS (if (= rnext "=")
            (::ref-index record)
            (ref-index rname->idx rnext)))
      (NP (:pnext record))
      (TS (:tlen record)))))

(defn- build-auxiliary-tags-encoder [{:keys [TL]} _tag-encoders]
  (fn [_record]
    (TL 0)))

(defn- build-read-features-encoder [{:keys [FN]}]
  (fn [_record]
    (FN 0)))

(defn- encode-qual [{:keys [qual] :as record} qs-encoder]
  (if (= qual "*")
    (dotimes [_ (count (:seq record))]
      (qs-encoder -1))
    (let [bs (.getBytes ^String qual)]
      (dotimes [i (alength bs)]
        (qs-encoder (bit-and (long (aget bs i)) 0xff))))))

(defn- build-mapped-read-encoder [{:keys [MQ QS] :as encoders}]
  (let [features-encoder (build-read-features-encoder encoders)]
    (fn [record]
      (features-encoder record)
      (MQ (:mapq record))
      (encode-qual record QS))))

(defn- build-unmapped-read-encoder [{:keys [BA QS]}]
  (fn [record]
    (let [bs (.getBytes ^String (:seq record))]
      (dotimes [i (alength bs)]
        (BA (aget bs i)))
      (encode-qual record QS))))

(defn- build-cram-record-encoder
  [cram-header {:keys [BF CF] :as ds-encoders} tag-encoders stats]
  (let [rname->idx (into {}
                         (map-indexed (fn [i {:keys [SN]}] [SN i]))
                         (:SQ cram-header))
        pos-encoder (build-positional-data-encoder cram-header ds-encoders)
        name-encoder (build-read-name-encoder ds-encoders)
        mate-encoder (build-mate-read-encoder rname->idx ds-encoders)
        tags-encoder (build-auxiliary-tags-encoder ds-encoders tag-encoders)
        mapped-encoder (build-mapped-read-encoder ds-encoders)
        unmapped-encoder (build-unmapped-read-encoder ds-encoders)]
    (fn [{:keys [^long pos] :as record}]
      (let [bf (bit-and (long (:flag record))
                        (bit-not (sam.flag/encoded #{:next-reversed :next-unmapped})))
            cf (cond-> 0
                 true (bit-or 0x01)
                 true (bit-or 0x02)
                 false (bit-or 0x04)
                 (= (:seq record) "*") (bit-or 0x08))
            record' (assoc record
                           ::flag cf
                           ::ref-index (ref-index rname->idx (:rname record))
                           ::end (if (pos? pos)
                                   ;; TODO: need to be calculated from read features
                                   (+ pos (count (:seq record)))
                                   0))]
        (stats record')
        (BF bf)
        (CF cf)
        (pos-encoder record')
        (name-encoder record')
        (mate-encoder record')
        (tags-encoder record')
        (if (sam.flag/unmapped? (:flag record'))
          (unmapped-encoder record')
          (mapped-encoder record'))))))

(defn encode-slice-records
  "TODO"
  [cram-header ds-encoders tag-encoders records]
  (let [stats (make-alignment-stats (count (:SQ cram-header)))
        record-encoder (build-cram-record-encoder cram-header ds-encoders tag-encoders stats)]
    (run! record-encoder records)
    (stats)))
