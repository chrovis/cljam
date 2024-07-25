(ns cljam.io.cram.encode.tag-dict
  (:import [java.util HashMap]))

(defprotocol ITagDictionaryBuilder
  (assign-tags-id! [this tags])
  (build-tag-dict [this]))

(defn make-tag-dict-builder
  "Creates a new tag dictionary builder."
  []
  (let [tags->id (HashMap.)]
    (reify ITagDictionaryBuilder
      (assign-tags-id! [_ tags]
        (let [tags' (into []
                          (keep (fn [opt]
                                  (let [[tag m] (first opt)]
                                    (when-not (= tag :RG)
                                      [tag (first (:type m))]))))
                          tags)]
          (or (.get tags->id tags')
              (let [id (count tags->id)]
                (.put tags->id tags' id)
                id))))
      (build-tag-dict [_]
        (->> (sort-by val tags->id)
             (mapv (fn [[tags _]]
                     (mapv (fn [[tag tag-type]]
                             {:tag tag :type tag-type})
                           tags))))))))

(defn- tag-id [item]
  (let [tag' (name (:tag item))]
    (bit-or (bit-shift-left (int (nth tag' 0)) 16)
            (bit-shift-left (int (nth tag' 1)) 8)
            (int (:type item)))))

(defn- tag-encoding-for-fixed-size [item size]
  {:codec :byte-array-len
   :len-encoding {:codec :huffman, :alphabet [size], :bit-len [0]}
   :val-encoding {:codec :external, :content-id (tag-id item)}})

(defn- build-tag-encoding [item]
  (case (:type item)
    (\A \c \C) (tag-encoding-for-fixed-size item 1)
    (\s \S) (tag-encoding-for-fixed-size item 2)
    (\i \I \f) (tag-encoding-for-fixed-size item 4)
    (let [content-id (tag-id item)]
      {:codec :byte-array-len
       :len-encoding {:codec :external, :content-id content-id}
       :val-encoding {:codec :external, :content-id content-id}})))

(defn build-tag-encodings
  "Creates a tag encodings map from the given tag dictionary."
  [tag-dict]
  (reduce
   (fn [m entry]
     (reduce
      (fn [m {tag-type :type :as item}]
        (update-in m [(:tag item) tag-type] #(or % (build-tag-encoding item))))
      m entry))
   {} tag-dict))
