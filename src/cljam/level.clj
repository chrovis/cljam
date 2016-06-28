(ns cljam.level
  "Analyze level of alignments in BAM."
  (:require [clojure.java.io :refer [file]]
            [cljam.core :as core]
            [cljam.io :as io]
            [cljam.util :as util]
            [cljam.bam :as bam]
            [cljam.util.sam-util :as sam-util]))

(defmulti add-level
  (fn [rdr wtr]
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
  "Returns true if two given segments are overlapped.
  Segment must be represented as a vector of length two."
  [[s1 e1] [s2 e2]]
  (cond
    (< e1 s2) false
    (< e2 s1) false
    :else true))

(defn- map-with-state
  "Like map, returns a lazy sequence consisting of the results of applying f to each items of coll.
  The difference is that f must take a state and return the updated one."
  [f state coll]
  (lazy-seq (when-let [s (seq coll)]
              (let [[next-state result] (f state (first s))]
                (cons result (map-with-state f next-state (rest s)))))))

(defn- calc-level
  "Calculates updated alignments with level information as an option.
  levels is a vector consisting of rightmost segments in each level.
  alignment is a deeply parsed alignment data with pos and seq.
  This returns updated levels and alignment as a vector."
  [levels {:keys [pos] :as alignment}]
  (let [segment [pos (sam-util/get-end alignment)]
        level (or (some identity (map-indexed #(when-not (collide? segment %2) %1) (vals levels)))
                  (count levels))]
    [(assoc levels level segment)
     (update alignment :options conj {:LV {:type "i" :value (int level)}})]))

(defn- clean!
  "Deletes a cache file at given path."
  [path]
  (let [f (file path)]
    (when (.exists f)
      (.delete f))))

(defn- create-level-cache!
  "Create a temporary BAM file consisting of alignments of a single chromosome."
  [chr length source-path cache-path]
  (with-open [local-rdr (bam/reader source-path :ignore-index true)
              cache-wtr (bam/writer cache-path)]
    (let [hdr (io/read-header local-rdr)
          alignments (->> {:depth :deep}
                          (io/read-alignments local-rdr)
                          (map-with-state calc-level (sorted-map)))]
      (io/write-header cache-wtr hdr)
      (io/write-refs cache-wtr hdr)
      (io/write-alignments cache-wtr alignments hdr))))

(defn- add-level-mt
  "Adds level information to alignments from rdr and write them to wtr.
  Level calculation process is multithreaded."
  [rdr wtr]
  (let [source-path (io/reader-path rdr)
        cache-name-fn #(->> (str (util/basename source-path) "_" % ".cache")
                            (file util/temp-dir)
                            (.getPath))
        level-cache-name-fn #(->> (str (util/basename source-path) "_" % ".level.cache")
                                  (file util/temp-dir)
                                  (.getPath))
        hdr (io/read-header rdr)
        ;; Split into BAM files according to chromosomes.
        caches (doall
                (map (fn [{:keys [SN LN]}]
                       (let [cache-path (cache-name-fn SN)
                             blocks (io/read-blocks rdr {:chr SN :start 0 :end LN})]
                         (with-open [cache-wtr (bam/writer cache-path)]
                           (io/write-header cache-wtr hdr)
                           (io/write-refs cache-wtr hdr)
                           (io/write-blocks cache-wtr blocks))
                         cache-path))
                     (hdr :SQ)))
        leval-caches (doall
                      (pmap (fn [{:keys [SN LN]}]
                              (let [cache-path (cache-name-fn SN)
                                    level-cache-path (level-cache-name-fn SN)]
                                (create-level-cache! SN LN cache-path level-cache-path)
                                (clean! cache-path)
                                level-cache-path))
                            (hdr :SQ)))]
    (doseq [cache caches]
      (clean! cache))
    ;; Merge cache files
    (io/write-header wtr hdr)
    (io/write-refs wtr hdr)
    (doseq [cache leval-caches]
      (with-open [cache-rdr (bam/reader cache :ignore-index true)]
        (io/write-blocks wtr (io/read-blocks cache-rdr)))
      (clean! cache))))

(defn- check-bam-sorted!
  "Checks if the bam file is sorted by coordinate and throws an exception if not."
  [rdr]
  (when-not (-> rdr io/read-header :HD :SO (= "coordinate"))
    (throw (ex-info "Source BAM file must be sorted by coordinate."
                    {:type :bam-not-sorted}))))

(defmethod add-level :bam
  [rdr wtr]
  (check-bam-sorted! rdr)
  (add-level-mt rdr wtr))
