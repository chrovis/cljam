(ns cljam.level
  "Analyze level of alignments in BAM."
  (:require [clojure.java.io :refer [file]]
            [clojure.string :as cstr]
            [cljam.core :as core]
            [cljam.io :as io]
            [cljam.util :as util]
            [cljam.bam :as bam]))

(defmulti add-level (fn [rdr wtr]
                           (case (str (type rdr))
                             "class cljam.sam.reader.SAMReader" :sam
                             "class cljam.bam.reader.BAMReader" :bam
                             nil)))

(defmethod add-level :sam
  [rdr wtr]
  (throw (ex-info "SAM not supported yet." {:type :sam-not-supported}))
  ;; TODO implement
  )

(defn- collide?
  [[s1 e1] [s2 e2]]
  (cond
    (< e1 s2) false
    (< e2 s1) false
    :else true))

(defn- map-with-state
  [f state coll]
  (lazy-seq (when-let [s (seq coll)]
              (let [[next-state result] (f state (first s))]
                (cons result (map-with-state f next-state (rest s)))))))

(defn- calc-level
  [levels {:keys [pos seq] :as alignment}]
  (let [segment [pos (+ pos (count seq))]
        level (or (some identity (map-indexed #(when-not (collide? segment %2) %1) levels))
                  (count levels))]
    [(assoc levels level segment)
     (update alignment :options conj {:LV {:type "i" :value level}})]))

(defn- clean [path]
  (let [f (file path)]
    (when (.exists f)
      (.delete f))))

(defmethod add-level :bam
  [rdr wtr]
  (let [filename (.getName (file (io/reader-path rdr)))
        basename (first (cstr/split filename #"\.(?=[^\.]+$)"))
        cache-name-fn #(format "%s/%s_%s.bam" util/temp-dir basename %)
        hdr (io/read-header rdr)
        ;; Exit when not sorted.
        _ (when-not (= (-> hdr :HD :SO) "coordinate")
            (throw (ex-info "Source BAM file must be sorted by coordinate."
                            {:type :bam-not-sorted})))
        ;; Split into per-chr BAM files.
        caches (pmap
                #(let [{:keys [SN LN]} %
                       cache-path (cache-name-fn SN)]
                   (with-open [local-rdr (bam/reader (io/reader-path rdr) :ignore-index false)
                               cache-wtr (bam/writer cache-path)]
                     (io/write-header cache-wtr hdr)
                     (io/write-refs cache-wtr hdr)
                     (let [alignments (->> (io/read-alignments local-rdr {:chr SN :start 0 :end LN})
                                           (map-with-state calc-level []))]
                       (io/write-alignments cache-wtr alignments hdr)))
                   cache-path)
                (hdr :SQ))]
    ;; Merge cache files
    (io/write-header wtr hdr)
    (io/write-refs wtr hdr)
    (doseq [cache caches]
      (with-open [cache-rdr (bam/reader cache :ignore-index true)]
        (io/write-blocks wtr (io/read-blocks cache-rdr)))
      (clean cache))))

