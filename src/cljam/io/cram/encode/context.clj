(ns cljam.io.cram.encode.context
  (:require [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.subst-matrix :as subst-mat]
            [cljam.io.cram.encode.tag-dict :as tag-dict]
            [cljam.io.sam.util.header :as sam.header]))

(defn make-container-context
  "Creates a new container context."
  [cram-header seq-resolver options]
  (let [rname->idx (into {}
                         (map-indexed (fn [i {:keys [SN]}] [SN i]))
                         (:SQ cram-header))
        preservation-map (cond-> {:RN true, :AP false, :RR true}
                           (:omit-read-names? options)
                           (assoc :RN false)

                           (= (sam.header/sort-order cram-header)
                              sam.header/order-coordinate)
                           (assoc :AP true))
        subst-mat-builder (subst-mat/make-subst-matrix-builder)
        tag-dict-builder (tag-dict/make-tag-dict-builder)]
    {:cram-header cram-header
     :rname->idx rname->idx
     :preservation-map preservation-map
     :seq-resolver seq-resolver
     :subst-mat-builder subst-mat-builder
     :tag-dict-builder tag-dict-builder
     :options options}))

(defn finalize-container-context
  "Finalizes the builders in the container context and returns a new container
  context containing those builders' results. This operation must be done before
  creating a slice context."
  [container-ctx alignment-stats]
  (let [{:keys [ds-compressor-overrides
                tag-compressor-overrides]} (:options container-ctx)
        ds-encodings (-> ds/default-data-series-encodings
                         (ds/apply-ds-compressor-overrides ds-compressor-overrides))
        subst-mat (subst-mat/build-subst-matrix (:subst-mat-builder container-ctx))
        tag-dict (tag-dict/build-tag-dict (:tag-dict-builder container-ctx))
        tag-encodings (-> (tag-dict/build-tag-encodings tag-dict)
                          (ds/apply-tag-compressor-overrides tag-compressor-overrides))]
    (assoc container-ctx
           :alignment-stats alignment-stats
           :ds-encodings ds-encodings
           :subst-mat subst-mat
           :tag-dict tag-dict
           :tag-encodings tag-encodings)))

(defn make-slice-context
  "Creates a slice context for the ith slice from the given container context.
  Note that the container context must be finalized with `finalize-container-context`."
  [{:keys [alignment-stats ds-encodings tag-encodings] :as container-ctx} i]
  (let [ds-encoders (ds/build-data-series-encoders (dissoc ds-encodings :embedded-ref))
        tag-encoders (ds/build-tag-encoders tag-encodings)]
    (assoc container-ctx
           :alignment-stats (nth alignment-stats i)
           :ds-encoders ds-encoders
           :tag-encoders tag-encoders)))

(defn encoding-results
  "Returns the encoding results from the given slice context."
  [{:keys [ds-encoders tag-encoders]}]
  (let [ds-results (mapcat #(%) (vals ds-encoders))
        tag-results (for [[_tag v] tag-encoders
                          [_type encoder] v
                          res (encoder)]
                      res)]
    (concat ds-results tag-results)))
