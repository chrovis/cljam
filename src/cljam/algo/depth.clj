(ns cljam.algo.depth
  "Provides algorithms for calculating simple depth of coverage."
  (:require [com.climate.claypoole :as cp]
            [com.climate.claypoole.lazy :as lazy]
            [cljam.common :as common]
            [cljam.util.region :as region]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util :as sam-util])
  (:import [cljam.io.protocols SAMRegionBlock]))

(def ^:const default-step 1000000)

;; lazy
;; ----

(defn- count-for-positions
  "Piles the alignments up and counts them in the positions, returning it as a seq."
  [alns beg end]
  (let [pile (long-array (inc (- end beg)))]
    (doseq [aln alns]
      (let [left (max (:pos aln) beg)
            right (min (:end aln) end)
            left-index (- left beg)]
        (dotimes [i (inc (- right left))]
          (aset-long pile (+ i left-index) (inc (aget pile (+ i left-index)))))))
    (seq pile)))

(defn- lazy-depth*
  "Internal lazy-depth function returning lazy sequence of depth."
  [rdr rname start end step]
  (let [n-threads (common/get-exec-n-threads)
        read-fn (fn [r start end]
                  (sam/read-blocks r {:chr rname :start start :end end} {:mode :region}))
        count-fn (fn [xs]
                   (if (= n-threads 1)
                     (map (fn [[start end]]
                            (count-for-positions (read-fn rdr start end) start end)) xs)
                     (lazy/pmap (dec n-threads)
                                (fn [[start end]]
                                  (with-open [r (sam/clone-bam-reader rdr)]
                                    (count-for-positions (read-fn r start end) start end))) xs)))]
    (->> (region/divide-region start end step)
         count-fn
         (apply concat))))

(defn lazy-depth
  "Calculate depth of coverage lazily. Returns a lazy seq of depth for range [start, end].
  Requires a `cljam.io.bam.reader.BAMReader` instance and region.
  If start and end are not supplied, piles whole range up.
  Note that CIGAR code in alignemnts are ignored and only start/end positions are used."
  [bam-reader {:keys [chr start end] :or {start 1 end Long/MAX_VALUE}}
   & [{:keys [step n-threads] :or {step default-step n-threads 1}}]]
  {:pre [chr start end (pos? start) (pos? end) (<= start end)]}
  (when-let [{:keys [len]} (sam-util/ref-by-name (sam/read-refs bam-reader) chr)]
    (binding [common/*n-threads* n-threads]
      (lazy-depth* bam-reader chr (min len start) (min len end) step))))

;; eager
;; -----

(defn- unchecked-aset-depth-in-region!
  "Piles alignments up and sets depth values to a part of the given int-array."
  [alns beg end offset ^ints pile]
  (let [beg (int beg)
        end (int end)
        offset (int offset)]
    (doseq [^SAMRegionBlock aln alns]
      (let [left (Math/max ^int (.pos aln) beg)
            right (unchecked-inc-int (.end aln))
            left-index (unchecked-add-int (unchecked-subtract-int left beg) offset)
            right-index (unchecked-add-int (unchecked-subtract-int right beg) offset)]
        (aset-int pile left-index (unchecked-inc-int (aget pile left-index)))
        (when (<= right end)
          (aset-int pile right-index (unchecked-dec-int (aget pile right-index))))))
    (dotimes [i (- end beg)]
      (aset-int
       pile
       (unchecked-add-int (unchecked-inc-int i) offset)
       (unchecked-add-int
        (aget pile (unchecked-add-int i offset))
        (aget pile (unchecked-add-int (unchecked-inc-int i) offset)))))))

(defn- aset-depth-in-region!
  "Piles alignments up and sets depth values to a part of the given int-array.
  It's roughly 15-25% slower than unchecked version."
  [alns beg end offset ^ints pile]
  (let [beg (int beg)
        end (int end)
        offset (int offset)]
    (doseq [aln alns]
      (let [left (Math/max ^int (:pos aln) beg)
            right (int ^long (inc (or (:end aln) (sam-util/get-end aln))))
            left-index (+ (- left beg) offset)
            right-index (+ (- right beg) offset)]
        (aset-int pile left-index (inc (aget pile left-index)))
        (when (<= right end)
          (aset-int pile right-index (dec (aget pile right-index))))))
    (dotimes [i (- end beg)]
      (aset-int pile (+ (inc i) offset) (+ (aget pile (+ i offset)) (aget pile (+ (inc i) offset)))))))

(defn ^"[I" depth*
  "Internal depth function which returns an int-array."
  [rdr {:keys [chr start end] :as region}
   & [{:keys [step unchecked? n-threads] :or {step default-step unchecked? false n-threads 1}}]]
  (let [pile (int-array (inc (- end start)))
        f (if unchecked? unchecked-aset-depth-in-region! aset-depth-in-region!)]
    (if (= n-threads 1)
      (f (sam/read-blocks rdr region {:mode :region}) start end 0 pile)
      (cp/pdoseq
       n-threads
       [[s e] (region/divide-region start end step)]
       (with-open [r (sam/clone-bam-reader rdr)]
         (-> (sam/read-blocks r {:chr chr, :start s, :end e} {:mode :region})
             (f s e (- s start) pile)))))
    pile))

(defn depth
  "Calculate depth of coverage eagerly. Returns a seq of depth for range [start, end].
  Requires a `cljam.io.bam.reader.BAMReader` instance and region.
  If start and end are not supplied, piles whole range up.
  Note that CIGAR code in alignemnts are ignored and only start/end positions are used."
  [bam-reader {:keys [chr start end] :or {start 1 end Long/MAX_VALUE}}
   & [{:keys [step unchecked? n-threads] :or {step default-step unchecked? false n-threads 1}}]]
  {:pre [chr start end (pos? start) (pos? end) (<= start end)]}
  (when-let [{:keys [len]} (sam-util/ref-by-name (sam/read-refs bam-reader) chr)]
    (seq
     (depth*
      bam-reader
      {:chr chr, :start (min len start), :end (min len end)}
      {:step step, :unchecked? unchecked?, :n-threads n-threads}))))
