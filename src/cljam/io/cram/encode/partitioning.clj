(ns cljam.io.cram.encode.partitioning
  (:require [cljam.io.sam.util.header :as sam.header])
  (:import [java.util ArrayList List]))

(defn- slice-record-collector
  [header {:keys [^long records-per-slice ^long min-single-ref-slice-size]
           :or {records-per-slice 10000, min-single-ref-slice-size 1000}}]
  (let [sort-by-coord? (= (sam.header/sort-order header) sam.header/order-coordinate)
        ;; CRAM files sorted by coord can easily get back from a multi-ref state
        ;; to a single-ref state. To support this, multi-ref slices should be kept
        ;; as small as possible
        multi-upper-bound (if sort-by-coord?
                            min-single-ref-slice-size
                            records-per-slice)]
    (fn [empty-container? alns]
      (let [slice-records (ArrayList.)]
        (loop [ref-state nil, alns alns]
          (if (seq alns)
            (let [[{:keys [rname] :as aln} & more] alns
                  n-records (.size slice-records)]
              (case ref-state
                nil
                (let [ref-state' (if (= rname "*") :unmapped rname)]
                  (.add slice-records aln)
                  (recur ref-state' more))

                :unmapped
                (if (< n-records records-per-slice)
                  (let [ref-state' (cond (= rname "*") ref-state
                                         sort-by-coord? (throw
                                                         (ex-info
                                                          (str "Unmapped records "
                                                               "must be last")
                                                          {}))
                                         :else :multi-ref)]
                    (.add slice-records aln)
                    (recur ref-state' more))
                  [ref-state slice-records alns])

                :multi-ref
                (if (< n-records multi-upper-bound)
                  (do (.add slice-records aln)
                      (recur ref-state more))
                  [ref-state slice-records alns])

                (if (= ref-state rname)
                  (if (< n-records records-per-slice)
                    (do (.add slice-records aln)
                        (recur ref-state more))
                    [ref-state slice-records alns])
                  ;; If the container already contains one or more single-ref
                  ;; slices, instead of creating and adding a new multi-ref slice
                  ;; to that container, add the accumulated single-ref slice
                  ;; to the container and add the current record to the next slice
                  (if (and empty-container? (< n-records min-single-ref-slice-size))
                    (do (.add slice-records aln)
                        (recur :multi-ref more))
                    [ref-state slice-records alns]))))
            [ref-state slice-records alns]))))))

(defn with-each-container
  "Partitions the given alignment records into containers, which are represented
  as a List of Lists of record maps, and calls the function f with them."
  [header options alns f]
  (let [{:keys [^long slices-per-container] :or {slices-per-container 1}} options
        collect-fn (slice-record-collector header options)
        container-records (ArrayList.)]
    (loop [ref-state nil, written 0, ready 0, alns alns]
      (if (seq alns)
        (let [n-slices (.size container-records)
              [ref-state' ^List slice-records alns'] (collect-fn (zero? n-slices) alns)]
          (if (or (= n-slices slices-per-container)
                  (= ref-state' :multi-ref)
                  (and (some? ref-state) (not= ref-state ref-state')))
            (let [written' (+ written ready)]
              (when-not (.isEmpty container-records)
                (f written container-records))
              (.clear container-records)
              (if (= ref-state' :multi-ref)
                (do (f written' (doto (ArrayList.) (.add slice-records)))
                    (recur nil (+ written' (.size slice-records)) 0 alns'))
                (let [ready' (.size slice-records)]
                  (.add container-records slice-records)
                  (recur ref-state' written' ready' alns'))))
            (let [ready' (+ ready (.size slice-records))]
              (.add container-records slice-records)
              (recur ref-state' written ready' alns'))))
        (when-not (.isEmpty container-records)
          (f written container-records))))))
