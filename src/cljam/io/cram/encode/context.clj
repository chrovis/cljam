(ns cljam.io.cram.encode.context
  (:require [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.tag-dict :as tag-dict]))

(defn make-container-context
  "Creates a new container context."
  [cram-header preservation-map seq-resolver]
  (let [rname->idx (into {}
                         (map-indexed (fn [i {:keys [SN]}] [SN i]))
                         (:SQ cram-header))
        subst-mat {\A {\T 0, \G 1, \C 2, \N 3}
                   \T {\A 0, \G 1, \C 2, \N 3}
                   \G {\A 0, \T 1, \C 2, \N 3}
                   \C {\A 0, \T 1, \G 2, \N 3}
                   \N {\A 0, \T 1, \G 2, \C 3}}
        tag-dict-builder (tag-dict/make-tag-dict-builder)]
    {:cram-header cram-header
     :rname->idx rname->idx
     :preservation-map preservation-map
     :subst-mat subst-mat
     :seq-resolver seq-resolver
     :tag-dict-builder tag-dict-builder}))

(defn finalize-container-context
  "Finalizes the builders in the container context and returns a new container
  context containing those builders' results. This operation must be done before
  creating a slice context."
  [{:keys [tag-dict-builder] :as container-ctx}]
  (let [tag-dict (tag-dict/build-tag-dict tag-dict-builder)
        tag-encodings (tag-dict/build-tag-encodings tag-dict)]
    (assoc container-ctx
           :tag-dict tag-dict
           :ds-encodings ds/default-data-series-encodings
           :tag-encodings tag-encodings)))

(defn make-slice-context
  "Creates a slice context from the given container context. Note that the container
  context must be finalized with `finalize-container-context`."
  [{:keys [ds-encodings tag-encodings] :as container-ctx}]
  (let [ds-encoders (ds/build-data-series-encoders ds-encodings)
        tag-encoders (ds/build-tag-encoders tag-encodings)]
    (assoc container-ctx
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
