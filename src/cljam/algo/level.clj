(ns cljam.algo.level
  "Analyze level of alignments in BAM."
  (:require [clojure.java.io :refer [file]]
            [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads *n-threads*]]
            [cljam.io.protocols :as protocols]
            [cljam.io.sam :as sam]
            [cljam.io.util :refer [sam-reader? bam-reader?]]
            [cljam.io.sam.util :as sam-util]
            [cljam.util :as util]))

(defmulti add-level
  (fn [rdr wtr]
    (cond
      (sam-reader? rdr) :sam
      (bam-reader? rdr) :bam
      :else nil)))

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
  (with-open [local-rdr (sam/bam-reader source-path :ignore-index true)
              cache-wtr (sam/bam-writer cache-path)]
    (let [hdr (sam/read-header local-rdr)
          alignments (->> {:depth :deep}
                          (sam/read-alignments local-rdr)
                          (map-with-state calc-level (sorted-map)))]
      (sam/write-header cache-wtr hdr)
      (sam/write-refs cache-wtr hdr)
      (sam/write-alignments cache-wtr alignments hdr))))

(defn- add-level-mt
  "Adds level information to alignments from rdr and write them to wtr.
  Level calculation process is multithreaded."
  [rdr wtr]
  (let [source-path (protocols/reader-path rdr)
        cache-name-fn #(->> (str (util/basename source-path) "_" % ".cache")
                            (file util/temp-dir)
                            (.getPath))
        level-cache-name-fn #(->> (str (util/basename source-path) "_" % ".level.cache")
                                  (file util/temp-dir)
                                  (.getPath))
        hdr (sam/read-header rdr)
        ;; Split into BAM files according to chromosomes.
        caches (doall
                (map (fn [{:keys [SN LN]}]
                       (let [cache-path (cache-name-fn SN)
                             blocks (sam/read-blocks rdr {:chr SN :start 0 :end LN})]
                         (with-open [cache-wtr (sam/bam-writer cache-path)]
                           (sam/write-header cache-wtr hdr)
                           (sam/write-refs cache-wtr hdr)
                           (sam/write-blocks cache-wtr blocks))
                         cache-path))
                     (hdr :SQ)))
        leval-caches (cp/with-shutdown! [pool (cp/threadpool (get-exec-n-threads))]
                       (doall
                        (cp/pmap pool
                                 (fn [{:keys [SN LN]}]
                                   (let [cache-path (cache-name-fn SN)
                                         level-cache-path (level-cache-name-fn SN)]
                                     (create-level-cache! SN LN cache-path level-cache-path)
                                     (clean! cache-path)
                                     level-cache-path))
                                 (hdr :SQ))))]
    (doseq [cache caches]
      (clean! cache))
    ;; Merge cache files
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    (doseq [cache leval-caches]
      (with-open [cache-rdr (sam/bam-reader cache :ignore-index true)]
        (sam/write-blocks wtr (sam/read-blocks cache-rdr)))
      (clean! cache))))

(defn- check-bam-sorted!
  "Checks if the bam file is sorted by coordinate and throws an exception if not."
  [rdr]
  (when-not (-> rdr sam/read-header :HD :SO (= "coordinate"))
    (throw (ex-info "Source BAM file must be sorted by coordinate."
                    {:type :bam-not-sorted}))))

(defmethod add-level :bam
  [rdr wtr & {:keys [n-threads]
              :or {n-threads 0}}]
  (check-bam-sorted! rdr)
  (binding [*n-threads* n-threads]
    (add-level-mt rdr wtr)))
