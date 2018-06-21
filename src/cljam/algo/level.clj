(ns cljam.algo.level
  "Level analyzer of alignments in BAM."
  (:require [clojure.java.io :as cio]
            [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads *n-threads*]]
            [cljam.io.protocols :as protocols]
            [cljam.io.sam :as sam]
            [cljam.io.util :as io-util]
            [cljam.util :as util]))

(defmulti add-level
  "Adds level information to alignments from rdr and writes them to wtr. Level
  calculation process is multithreaded."
  {:arglists '([rdr wtr])}
  (fn [rdr wtr]
    (cond
      (and (io-util/sam-reader? rdr) (io-util/sam-writer? wtr)) :sam
      (and (io-util/bam-reader? rdr) (io-util/bam-writer? wtr)) :bam
      :else nil)))

(defmethod add-level :sam
  [_ _]
  (throw (ex-info "SAM not supported yet." {:type :sam-not-supported}))
  ;; TODO implement
  )

(defn- add-level!
  "Append level info to :options of an alignment. Requires volatile vector as a state."
  [state a]
  (let [pos (long (:pos a))
        end (long (:end a))
        s @state
        lv (loop [i 0 x (first s) xs (next s)]
             (if-not x
               (do (vswap! state conj end) i)
               (if (< x pos)
                 (do (vswap! state assoc i end) i)
                 (recur (inc i) (first xs) (next xs)))))]
    (update a :options conj {:LV {:type "i" :value (int lv)}})))

(defn- cache-path
  "Create a path to the cache file."
  [rdr i]
  (->> i
       (format "%s_%06d.bam" (util/basename (protocols/reader-url rdr)))
       (cio/file util/temp-dir)
       (.getCanonicalPath)))

(defn- add-level-mt
  "Adds level information to alignments from rdr and write them to wtr.
  Level calculation process is multithreaded."
  [rdr wtr]
  (let [hdr (sam/read-header rdr)]
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    (cp/with-shutdown! [p (cp/threadpool (get-exec-n-threads))]
      ;; split and compute levels
      (let [xs (cp/pfor p [[i {:keys [name]}] (map vector (range) (sam/read-refs rdr))]
                 (let [cache (cache-path rdr i)]
                   (with-open [r (sam/reader rdr)
                               w (sam/writer cache)]
                     (sam/write-header w hdr)
                     (sam/write-refs w hdr)
                     (->> {:chr name}
                          (sam/read-alignments r)
                          (map (partial add-level! (volatile! [])))
                          (#(sam/write-alignments w % hdr))))
                   cache))]
        ;; merge
        (doseq [cache xs]
          (with-open [r (sam/reader cache)]
            (sam/write-blocks wtr (sam/read-blocks r)))
          (.delete (cio/file cache)))))))

(defmethod add-level :bam
  [rdr wtr & {:keys [n-threads]
              :or {n-threads 0}}]
  (when-not (-> rdr sam/read-header :HD :SO (= "coordinate"))
    (throw (ex-info "Source BAM file must be sorted by coordinate."
                    {:type :bam-not-sorted})))
  (binding [*n-threads* n-threads]
    (add-level-mt rdr wtr)))
