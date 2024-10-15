(ns cljam.io.cram.encode.record
  (:require [cljam.io.cram.encode.alignment-stats :as stats]
            [cljam.io.cram.encode.subst-matrix :as subst-mat]
            [cljam.io.cram.encode.tag-dict :as tag-dict]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.sam.util.cigar :as sam.cigar]
            [cljam.io.sam.util.flag :as sam.flag]
            [cljam.io.sam.util.option :as sam.option])
  (:import [java.util Arrays List]))

(defn- ref-index [rname->idx rname]
  (if  (= rname "*")
    -1
    (get rname->idx rname)))

(defn- build-positional-data-encoder
  [{:keys [cram-header preservation-map alignment-stats]} {:keys [RI RL AP RG]}]
  (let [rg-id->idx (into {}
                         (map-indexed (fn [i {:keys [ID]}] [ID i]))
                         (:RG cram-header))
        AP' (if (:AP preservation-map)
              (let [pos (volatile! (:start alignment-stats))]
                (fn [^long pos']
                  (AP (- pos' (long @pos)))
                  (vreset! pos pos')))
              AP)]
    (fn [record]
      (let [rg (sam.option/value-for-tag :RG record)]
        (RI (::ref-index record))
        (RL (count (:seq record)))
        (AP' (:pos record))
        (RG (if rg (get rg-id->idx rg) -1))))))

(defn- build-read-name-encoder [{:keys [RN]}]
  (fn [record]
    (RN (.getBytes ^String (:qname record)))))

(defn- build-mate-read-encoder [{:keys [rname->idx]} {:keys [MF NS NP TS]}]
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

(defn- build-auxiliary-tags-encoder [{:keys [tag-dict tag-encoders]} {:keys [TL]}]
  (let [tag-encoder (fn [{:keys [tag] :as item}]
                      (let [tag&type [tag (:type item)]
                            encoder (get-in tag-encoders tag&type)]
                        (fn [opts]
                          (encoder (get opts tag&type)))))
        encoders (mapv (fn [entry]
                         (let [encoders (mapv tag-encoder entry)]
                           (fn [opts]
                             (run! #(% opts) encoders))))
                       tag-dict)]
    (fn [record]
      (let [tl (::tags-index record)
            encoder (nth encoders tl)
            opts (into {}
                       (map (fn [opt]
                              (let [[tag m] (first opt)]
                                [[tag (first (:type m))] (:value m)])))
                       (:options record))]
        (TL tl)
        (encoder opts)))))

(defn- build-read-features-encoder [{:keys [FN FP FC BA QS BS IN DL SC HC RS PD]}]
  (fn [record]
    (let [fs (::features record)]
      (FN (count fs))
      (reduce (fn [^long prev-pos {:keys [^long pos] :as f}]
                (FP (- pos prev-pos))
                (case (:code f)
                  :read-base (do (FC (int \B))
                                 (BA (:base f))
                                 (QS (:qual f)))
                  :subst (do (FC (int \X))
                             (BS @(:subst f)))
                  :insertion (do (FC (int \I))
                                 (IN (:bases f)))
                  :deletion (do (FC (int \D))
                                (DL (:len f)))
                  :softclip (do (FC (int \S))
                                (SC (:bases f)))
                  :hardclip (do (FC (int \H))
                                (HC (:len f)))
                  :ref-skip (do (FC (int \N))
                                (RS (:len f)))
                  :padding (do (FC (int \P))
                               (PD (:len f))))
                pos)
              0 fs))))

(defn- encode-qual [{:keys [qual] :as record} qs-encoder]
  (if (= qual "*")
    (dotimes [_ (count (:seq record))]
      (qs-encoder -1))
    (let [bs (.getBytes ^String qual)]
      (dotimes [i (alength bs)]
        (qs-encoder (bit-and (long (- (aget bs i) 33)) 0xff))))))

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

(defn- build-cram-record-encoder [{:keys [ds-encoders] :as slice-ctx}]
  (let [pos-encoder (build-positional-data-encoder slice-ctx ds-encoders)
        name-encoder (build-read-name-encoder ds-encoders)
        mate-encoder (build-mate-read-encoder slice-ctx ds-encoders)
        tags-encoder (build-auxiliary-tags-encoder slice-ctx ds-encoders)
        mapped-encoder (build-mapped-read-encoder ds-encoders)
        unmapped-encoder (build-unmapped-read-encoder ds-encoders)
        {:keys [BF CF]} ds-encoders]
    (fn [record]
      (let [bf (bit-and (long (:flag record))
                        (bit-not (sam.flag/encoded #{:next-reversed :next-unmapped})))]
        (BF bf)
        (CF (::flag record))
        (pos-encoder record)
        (name-encoder record)
        (mate-encoder record)
        (tags-encoder record)
        (if (sam.flag/unmapped? (:flag record))
          (unmapped-encoder record)
          (mapped-encoder record))))))

(defn encode-slice-records
  "Encodes CRAM records in a slice all at once using the given slice context.
  Returns the alignment stats for this slice."
  [slice-ctx records]
  (let [record-encoder (build-cram-record-encoder slice-ctx)]
    (run! record-encoder records)))

(defn- add-mismatches
  [n subst-mat-builder ^bytes ref-bases rpos ^bytes read-bases ^bytes qs spos fs]
  (loop [i (long n), rpos (long rpos), spos (long spos), fs fs]
    (if (zero? i)
      fs
      (let [ref-base (aget ref-bases (dec rpos))
            read-base (aget read-bases spos)]
        (if (= ref-base read-base)
          (recur (dec i) (inc rpos) (inc spos) fs)
          (let [pos (inc spos)
                f (if (or (neg? (.indexOf "ATGCN" ref-base))
                          (neg? (.indexOf "ATGCN" read-base)))
                    {:code :read-base :pos pos
                     :base read-base
                     :qual (if qs (- (aget qs spos) 33) -1)}
                    {:code :subst :pos pos
                     :subst (subst-mat/assign-code! subst-mat-builder ref-base read-base)})]
            (recur (dec i) (inc rpos) (inc spos) (conj! fs f))))))))

(defn- calculate-read-features&end
  [seq-resolver subst-mat-builder {:keys [rname ^long pos qual cigar] :as record}]
  (if (or (zero? pos) (= (:seq record) "*"))
    [[] pos]
    (let [ref-bases ^bytes (resolver/resolve-sequence seq-resolver rname)
          read-bases (.getBytes ^String (:seq record))
          qs (when-not (= qual "*")
               (.getBytes ^String qual))]
      (loop [[[^long n op] & more] (sam.cigar/parse cigar)
             rpos pos
             spos 0
             fs (transient [])]
        (if op
          (let [pos (inc spos)]
            (case op
              (\M \X \=) (recur more (+ rpos n) (+ spos n)
                                (add-mismatches n subst-mat-builder ref-bases rpos
                                                read-bases qs spos fs))
              \I (let [spos' (+ spos n)
                       bs (Arrays/copyOfRange read-bases spos spos')]
                   (recur more rpos spos' (conj! fs {:code :insertion :pos pos :bases bs})))
              \D (recur more (+ rpos n) spos (conj! fs {:code :deletion :pos pos :len n}))
              \N (recur more (+ rpos n) spos (conj! fs {:code :ref-skip :pos pos :len n}))
              \S (let [spos' (+ spos n)
                       bs (Arrays/copyOfRange read-bases spos spos')]
                   (recur more rpos spos' (conj! fs {:code :softclip :pos pos :bases bs})))
              \H (recur more rpos spos (conj! fs {:code :hardclip :pos pos :len n}))
              \P (recur more rpos spos (conj! fs {:code :padding :pos pos :len n}))))
          [(persistent! fs) (dec rpos)])))))

(defn preprocess-slice-records
  "Preprocesses slice records to calculate some record fields prior to record
  encoding that are necessary for the CRAM writer to generate some header
  components."
  [{:keys [rname->idx seq-resolver subst-mat-builder tag-dict-builder]} ^List records]
  (let [stats-builder (stats/make-alignment-stats-builder)]
    (dotimes [i (.size records)]
      (let [record (.get records i)
            ;; these flag bits of CF are hard-coded at the moment:
            ;; - 0x01: quality scores stored as array (true)
            ;; - 0x02: detached (true)
            ;; - 0x04: has mate downstream (false)
            cf (cond-> 0x03
                 (= (:seq record) "*") (bit-or 0x08))
            ri (ref-index rname->idx (:rname record))
            tags-id (tag-dict/assign-tags-id! tag-dict-builder (:options record))
            [fs end] (calculate-read-features&end seq-resolver subst-mat-builder record)
            record' (assoc record
                           ::flag cf ::ref-index ri ::end end
                           ::features fs ::tags-index tags-id)]
        (stats/update! stats-builder ri (:pos record) end (count (:seq record)) 1)
        (.set records i record')))
    (stats/build stats-builder)))
