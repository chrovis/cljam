(ns cljam.io.cram.decode.record
  (:require [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.sam.util.cigar :as sam.cigar]
            [cljam.io.sam.util.flag :as sam.flag]
            [cljam.io.util.byte-buffer :as bb]))

(defn- ref-name [cram-header ^long ref-id]
  (or (when (<= 0 ref-id)
        (get-in cram-header [:SQ ref-id :SN]))
      "*"))

(defn- read-group-id [cram-header ^long rg-index]
  (when (<= 0 rg-index)
    (get-in cram-header [:RG rg-index :ID])))

(defn- build-positional-data-decoder
  [cram-header {:keys [preservation-map]} slice-header {:keys [RI RL AP RG]}]
  (let [AP' (if (:AP preservation-map)
              (let [pos (volatile! nil)]
                (fn []
                  (let [pos' (+ (long (or @pos (:start slice-header)))
                                (long (AP)))]
                    (vreset! pos pos')
                    pos')))
              AP)
        RI' (if (= (:ref-seq-id slice-header) -2)
              RI
              (constantly (:ref-seq-id slice-header)))]
    (fn [record]
      (let [rg (read-group-id cram-header (RG))]
        (assoc record
               :rname (ref-name cram-header (RI'))
               ::len (RL)
               :pos (AP')
               :options (cond-> []
                          rg
                          (conj {:RG {:type "Z" :value rg}})))))))

(defn- build-read-name-decoder [{:keys [preservation-map]} {:keys [RN]}]
  (if (:RN preservation-map)
    #(assoc % :qname (String. ^bytes (RN)))
    (throw (ex-info "Omitted read names are not supported yet." {}))))

(defn- build-mate-read-decoder [cram-header {:keys [MF NS NP TS NF]}]
  (fn [{:keys [rname] :as record}]
    (let [flag (long (::flag record))]
      (if (pos? (bit-and flag 0x02))
        (let [mate-flag (long (MF))
              bam-flag (cond-> (long (:flag record))
                         (pos? (bit-and mate-flag 0x01))
                         (bit-or (sam.flag/encoded #{:next-reversed}))

                         (pos? (bit-and mate-flag 0x02))
                         (bit-or (sam.flag/encoded #{:next-unmapped})))
              rnext (ref-name cram-header (NS))]
          (assoc record
                 :flag bam-flag
                 :rnext (if (and (= rname rnext) (not= rname "*")) "=" rnext)
                 :pnext (NP)
                 :tlen (TS)))
        (cond-> record
          (pos? (bit-and flag 0x04))
          (assoc ::next-fragment (NF)))))))

(defn- build-auxiliary-tags-decoder [{:keys [preservation-map]} {:keys [TL]} tag-decoders]
  (let [tag-decoder (fn [{tag-type :type :keys [tag]}]
                      (let [decoder (get-in tag-decoders [tag tag-type])]
                        (fn []
                          {tag (decoder)})))
        decoders (mapv (fn [tags]
                         (let [decoders (mapv tag-decoder tags)]
                           (fn []
                             (into [] (map #(%)) decoders))))
                       (:TD preservation-map))]
    (fn [record]
      (let [tl (TL)
            decoder (nth decoders tl)]
        (update record :options into (decoder))))))

(defn- record-end [record features]
  (->> features
       ^long (reduce
              (fn [^long ret f]
                (case (:code f)
                  (:insertion :softclip)
                  (- ret (alength ^bytes (:bases f)))

                  (:deletion :ref-skip)
                  (+ ret (long (:len f)))

                  :insert-base
                  (dec ret)

                  ret))
              (::len record))
       dec
       (+ (long (:pos record)))))

(defn- record-seq
  [seq-resolver {:keys [preservation-map]} {:keys [rname pos end] :as record} features]
  (let [region {:chr rname :start pos :end end}
        ref-bases (.getBytes ^String (resolver/resolve-sequence seq-resolver region))
        len (long (::len record))
        bs (byte-array len (byte (int \N)))
        subst (:SM preservation-map)]
    (loop [[f & more :as fs] features, rpos 0, spos 1]
      (if f
        (let [fpos (long (:pos f))
              gap (- fpos spos)]
          (if (pos? gap)
            (do (System/arraycopy ref-bases rpos bs (dec spos) gap)
                (recur fs (+ rpos gap) fpos))
            (case (:code f)
              :subst
              (let [b (char (aget ref-bases rpos))
                    b' (get-in subst [b (:subst f)])]
                (aset bs (dec fpos) (byte (int b')))
                (recur more (inc rpos) (inc fpos)))

              (:insertion :softclip)
              (let [^bytes bs' (:bases f)
                    n (alength bs')]
                (System/arraycopy bs' 0 bs (dec fpos) n)
                (recur more rpos (+ fpos n)))

              (:deletion :ref-skip)
              (recur more (+ rpos (long (:len f))) fpos)

              :insert-base
              (do (aset bs (dec fpos) (byte (:base f)))
                  (recur more rpos (inc fpos)))

              (recur more rpos spos))))
        (let [gap (- len (dec spos))]
          (when (pos? gap)
            (System/arraycopy ref-bases rpos bs (dec spos) gap))
          (run! (fn [f]
                  (case (:code f)
                    :read-base (aset bs (dec (long (:pos f))) (byte (:base f)))
                    :bases (let [^bytes bs' (:bases f)
                                 n (alength bs')]
                             (System/arraycopy bs' 0 bs (dec (long (:pos f))) n))
                    nil))
                features)
          (String. bs))))))

(defn- qual-score ^long [^long qual]
  (+ qual 33))

(defn- decode-qual ^String [^long len qs-decoder]
  (let [bb (bb/allocate-lsb-byte-buffer len)]
    (loop [i len, miss 0]
      (if (zero? i)
        (if (= len miss)
          "*"
          (String. (.array bb)))
        (let [q (long (qs-decoder))]
          (.put bb (byte (qual-score q)))
          (recur (dec i) (cond-> miss (= q -1) inc)))))))

(defn- record-qual [record features {:keys [QS]}]
  (let [flag (long (::flag record))
        len (long (::len record))]
    (if (pos? (bit-and flag 0x01))
      (decode-qual len QS)
      (let [qs (byte-array len)
            missing? (reduce
                      (fn [missing? {:keys [^long pos] :as f}]
                        (case (:code f)
                          (:read-base :score)
                          (do (aset qs (dec pos) (byte (:qual f)))
                              false)

                          :scores
                          (let [^bytes scores (:scores f)]
                            (System/arraycopy ^bytes scores 0 qs (dec pos) (alength scores))
                            false)
                          missing?))
                      true
                      features)]
        (if missing?
          "*"
          (do (dotimes [i len]
                (aset qs i (byte (qual-score (aget qs i)))))
              (String. qs)))))))

(defn- record-cigar [record features]
  (let [read-len (long (::len record))]
    (loop [[f & more :as fs] features
           pos 0
           acc (transient [])]
      (if f
        (let [p (long (:pos f))
              gap (- p (inc pos))]
          (if (pos? gap)
            (recur fs p (conj! acc [gap \M]))
            (if-let [[op ^long len] (case (:code f)
                                      (:read-base :subst) [\M 1]
                                      :insertion [\I (count (:bases f))]
                                      :softclip [\S (count (:bases f))]
                                      :hardclip [\H (:len f)]
                                      :padding [\P (:len f)]
                                      :deletion [\D (:len f)]
                                      :ref-skip [\N (:len f)]
                                      :insert-base [\I 1]
                                      nil)]
              (let [pos' (if (#{\M \I \S} op)
                           (+ p (dec len))
                           (dec p))]
                (recur more pos' (cond-> acc (pos? len) (conj! [len op]))))
              (recur more p acc))))
        (let [acc' (cond-> acc
                     (< pos read-len)
                     (conj! [(- read-len pos) \M]))]
          (some->> (not-empty (persistent! acc')) sam.cigar/simplify))))))

(defn- build-read-features-decoder [{:keys [FN FC FP BA QS BS IN SC HC PD DL RS BB QQ]}]
  (fn []
    (loop [i (long (FN))
           prev-pos 0
           fs (transient [])]
      (if (zero? i)
        (persistent! fs)
        (let [code (char (FC))
              pos (+ prev-pos (long (FP)))
              data (case code
                     \B {:code :read-base :base (BA) :qual (QS)}
                     \X {:code :subst :subst (BS)}
                     \I {:code :insertion :bases (IN)}
                     \S {:code :softclip :bases (SC)}
                     \H {:code :hardclip :len (HC)}
                     \P {:code :padding :len (PD)}
                     \D {:code :deletion :len (DL)}
                     \N {:code :ref-skip :len (RS)}
                     \i {:code :insert-base :base (BA)}
                     \b {:code :bases :bases (BB)}
                     \q {:code :scores :scores (QQ)}
                     \Q {:code :score :score (QS)})]
          (recur (dec i) pos (conj! fs (assoc data :pos pos))))))))

(defn- build-mapped-read-decoder [seq-resolver compression-header {:keys [MQ] :as decoders}]
  (let [features-decoder (build-read-features-decoder decoders)]
    (fn [record]
      (let [fs (features-decoder)
            end (record-end record fs)
            record' (assoc record :end end)]
        (assoc record'
               :mapq (MQ)
               :seq (record-seq seq-resolver compression-header record' fs)
               :qual (record-qual record' fs decoders)
               :cigar (->> (record-cigar record' fs)
                           (map (fn [[n op]] (str n op)))
                           (apply str)))))))

(defn- build-unmapped-read-decoder [{:keys [BA QS]}]
  (fn [record]
    (let [len (long (::len record))
          flag (long (::flag record))
          bb (bb/allocate-lsb-byte-buffer len)
          _ (dotimes [_ len]
              (.put bb (byte (BA))))
          record' (assoc record
                         :seq (String. (.array bb))
                         :mapq 0
                         :cigar "*"
                         :end (:pos record))]
      (if (zero? (bit-and flag 0x01))
        record'
        (assoc record' :qual (decode-qual len QS))))))

(defn build-cram-record-decoder
  "Builds a CRAM record decoder.

  Returns a function with no arguments that returns a map representing a decoded
  CRAM record upon each call."
  [seq-resolver cram-header compression-header slice-header ds-decoders tag-decoders]
  (let [{:keys [BF CF]} ds-decoders
        pos-decoder (build-positional-data-decoder cram-header
                                                   compression-header
                                                   slice-header
                                                   ds-decoders)
        name-decoder (build-read-name-decoder compression-header ds-decoders)
        mate-decoder (build-mate-read-decoder cram-header ds-decoders)
        tags-decoder (build-auxiliary-tags-decoder compression-header ds-decoders tag-decoders)
        mapped-decoder (build-mapped-read-decoder seq-resolver compression-header ds-decoders)
        unmapped-decoder (build-unmapped-read-decoder ds-decoders)]
    (fn []
      (let [bf (BF)
            cf (CF)
            record' (->> {:flag bf ::flag cf}
                         (pos-decoder)
                         (name-decoder)
                         (mate-decoder)
                         (tags-decoder))]
        (if (sam.flag/unmapped? bf)
          (unmapped-decoder record')
          (mapped-decoder record'))))))

(defn- update-next-mate
  [{:keys [^long flag] :as record} {^long mate-flag :flag :as mate}]
  (assoc record
         :flag (cond-> flag
                 (sam.flag/reversed? mate-flag)
                 (bit-or (sam.flag/encoded #{:next-reversed}))

                 (sam.flag/unmapped? mate-flag)
                 (bit-or (sam.flag/encoded #{:next-unmapped})))
         :rnext (if (= (:rname record) (:rname mate))
                  "="
                  (:rname mate))
         :pnext (:pos mate)))

(defn- update-mate-records
  [{^long s1 :pos ^long e1 :end :as r1} {^long s2 :pos ^long e2 :end :as r2}]
  (let [r1' (update-next-mate r1 r2)
        r2' (update-next-mate r2 r1)]
    (if (or (sam.flag/unmapped? (:flag r1))
            (sam.flag/unmapped? (:flag r2))
            (not= (:rname r1') (:rname r2')))
      [(assoc r1' :tlen 0) (assoc r2' :tlen 0)]
      (if (<= s1 s2)
        (let [tlen (inc (- e2 s1))]
          [(assoc r1' :tlen tlen)
           (assoc r2' :tlen (- tlen))])
        (let [tlen (inc (- e1 s2))]
          [(assoc r1' :tlen (- tlen))
           (assoc r2' :tlen tlen)])))))

(defn resolve-mate-records
  "Resolves mate records in the same slice by updating their flags, tlen, etc."
  [^objects records]
  (dotimes [i (alength records)]
    (let [record (aget records i)]
      (when-let [nf (::next-fragment record)]
        (let [j (inc (+ i (long nf)))
              mate (aget records j)
              [record' mate'] (update-mate-records record mate)]
          (aset records i record')
          (aset records j mate'))))))

(defn decode-slice-records
  "Decodes CRAM records in a slice all at once and returns them as a sequence of maps."
  [seq-resolver cram-header compression-header slice-header ds-decoders tag-decoders]
  (let [record-decoder (build-cram-record-decoder seq-resolver
                                                  cram-header
                                                  compression-header
                                                  slice-header
                                                  ds-decoders
                                                  tag-decoders)
        n (:records slice-header)
        records (object-array n)]
    (dotimes [i n]
      (aset records i (record-decoder)))
    (resolve-mate-records records)
    (map #(dissoc % ::flag ::len ::next-fragment) records)))
