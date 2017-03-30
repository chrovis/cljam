(ns cljam.bam-index.writer
  (:require [clojure.java.io :as io]
            [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads]]
            [cljam.lsb :as lsb]
            [cljam.util.sam-util :as sam-util]
            [cljam.util.bgzf-util :as bgzf-util]
            [cljam.bam.decoder :as bam-decoder]
            [cljam.bam-index.common :refer :all]
            [cljam.bam-index.chunk :as chunk])
  (:import [java.nio ByteBuffer ByteOrder]))

(declare make-index)
(declare make-index-from-blocks)

;; BAIWriter
;; ---------

(deftype BAIWriter [^java.io.DataOutputStream writer refs f]
  java.io.Closeable
  (close [this]
    (.close writer)))

;; Indexing
;; --------

(defn- pos->lidx-offset
  [^long pos]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) linear-index-shift))

;; ### Intermidate data definitions
;;
;; Use record for performance.
;; Record is faster than map for retrieving elements.

(defrecord MetaData [first-offset last-offset aligned-alns unaligned-alns])

(defrecord IndexStatus [^MetaData meta-data bin-index linear-index])

;; ### Initializing

(defn- init-index-status
  "Returns initialized index status. This data structure is intermidate. Must
  be passed `finalize-index` in the final stage."
  []
  (IndexStatus. (MetaData. -1 0 0 0)
                ;; Intermidiate bin-index -> {bin1 chunks1, bin2 chunks2, ...}
                ;; e.g. {4681 [{:beg 97, :end 555} ...], 37450 [...] ...}
                {}
                ;; Intermidate linear-index -> {pos1 value1, pos2 value2, ...}
                ;; e.g. {5415 4474732776, 14827 5955073327, ...}
                {}))

;; ### Updating

(defn- update-meta-data
  [^MetaData meta-data aln]
  (let [first-offset (.first-offset meta-data)
        last-offset (.last-offset meta-data)
        aligned-alns (.aligned-alns meta-data)
        unaligned-alns (.unaligned-alns meta-data)
        {:keys [beg end]} (:chunk (:meta aln))
        aligned? (zero? (bit-and (:flag aln) 4))]
    (MetaData.
     ;; first-offset
     (if (or (< (bgzf-util/compare beg first-offset) 1)
             (= first-offset -1))
       beg first-offset)
     ;; last-offset
     (if (< (bgzf-util/compare last-offset end) 1)
       end last-offset)
     ;; aligned-alns
     (if aligned? (inc aligned-alns) aligned-alns)
     ;; unaligned-alns
     (if-not aligned? (inc unaligned-alns) unaligned-alns))))

(defn- update-bin-index
  [bin-index aln]
  (let [bin (sam-util/compute-bin aln)
        achunk (:chunk (:meta aln))]
    (assoc bin-index bin
           (if-let [chunks (get bin-index bin)]
             (if (bgzf-util/same-or-adjacent-blocks? (:end (peek chunks)) (:beg achunk))
               (let [l (assoc (peek chunks) :end (:end achunk))]
                 (assoc chunks (dec (count chunks)) l))
               (conj chunks achunk))
             (vector achunk)))))

(defn- update-linear-index
  [linear-index aln]
  (let [beg (-> aln :meta :chunk :beg)
        aln-beg (:pos aln)
        aln-end (sam-util/get-end aln)
        win-beg (if (zero? aln-end)
                  (pos->lidx-offset (dec aln-beg))
                  (pos->lidx-offset aln-beg))
        win-end (if (zero? aln-end)
                  win-beg
                  (pos->lidx-offset aln-end))
        min* (fn [x]
               (if x (min x beg) beg))]
    (loop [i win-beg, ret linear-index]
      (if (<= i win-end)
        (recur (inc i) (assoc ret i (min* (get ret i))))
        ret))))

(defn- update-index-status
  [^IndexStatus index-status aln]
  (IndexStatus. (update-meta-data (.meta-data index-status) aln)
                (update-bin-index (.bin-index index-status) aln)
                (update-linear-index (.linear-index index-status) aln)))

;; ### Making index

(defn- make-index*
  "Calculates index from the references and alignments, returning it as a map.
  Returned index is still intermidate. It must be passed to finalize function
  in the final stage."
  [refs alns]
  (loop [[aln & rest] alns
         ref-name (:rname aln)
         idx-status (init-index-status)
         no-coordinate-alns 0
         indices {}]
    (if aln
      (let [ref-name' (:rname aln)
            new-ref? (not= ref-name' ref-name)
            idx-status' (update-index-status
                         (if new-ref? (init-index-status) idx-status) aln)
            no-coordinate-alns' (if (zero? (:pos aln))
                                  (inc no-coordinate-alns)
                                  no-coordinate-alns)
            indices' (if new-ref?
                       (assoc indices ref-name idx-status)
                       indices)]
        (recur rest ref-name' idx-status' no-coordinate-alns' indices'))
      (assoc indices ref-name idx-status
                     :no-coordinate-alns no-coordinate-alns))))

;; Merging indices
;; -------------

(defn- merge-meta-data
  [^MetaData meta1 ^MetaData meta2]
  (MetaData. (let [f1 (.first-offset meta1)
                   f2 (.first-offset meta2)]
               (cond
                (= f1 -1) f2
                (= f2 -1) f1
                :else (min f1 f2)))
             (max (.last-offset meta1) (.last-offset meta2))
             (+ (.aligned-alns meta1) (.aligned-alns meta2))
             (+ (.unaligned-alns meta1) (.unaligned-alns meta2))))

(defn- merge-chunks
  [chunks1 chunks2]
  (loop [[f & r] (sort chunk/compare (concat chunks1 chunks2))
         chunks' []]
    (if f
      (if-let [last-chunk (peek chunks')]
        (if (bgzf-util/same-or-adjacent-blocks? (:end last-chunk) (:beg f))
          (let [l (assoc last-chunk :end (:end f))]
            (recur r (assoc chunks' (dec (count chunks')) l)))
          (recur r (conj chunks' f)))
        (recur r (conj chunks' f)))
      chunks')))

(defn- merge-bin-index
  [bin-map1 bin-map2]
  (merge-with merge-chunks bin-map1 bin-map2))

(defn- merge-linear-index
  [lidx1 lidx2]
  (merge-with min lidx1 lidx2))

(defn- merge-index
  "Merges two intermidate indices, returning the merged intermidate index."
  [idx1 idx2]
  (let [no-coordinate-alns (+ (:no-coordinate-alns idx1) (:no-coordinate-alns idx2))
        idx1 (dissoc idx1 :no-coordinate-alns)
        idx2 (dissoc idx2 :no-coordinate-alns)]
    (-> (merge-with
         (fn [^IndexStatus v1 ^IndexStatus v2]
           (IndexStatus. (merge-meta-data (.meta-data v1) (.meta-data v2))
                         (merge-bin-index (.bin-index v1) (.bin-index v2))
                         (merge-linear-index (.linear-index v1) (.linear-index v2))))
         idx1 idx2)
        (assoc :no-coordinate-alns no-coordinate-alns))))

;; Finalizing index
;; ----------------

(defn- finalize-bin-index
  [bin-index]
  (->> bin-index
       (seq)
       (sort-by first)
       (map (partial zipmap [:bin :chunks]))))

(defn- complement-linear-index
  "Complements a linear index.
  e.g. ([1 10] [3 30]) -> ([0 0] [1 10] [2 10] [3 30])"
  [linear-index]
  (loop [[f & r] (if (zero? (ffirst linear-index))
                   linear-index
                   (conj linear-index [0 0]))
         ret []]
    (if (seq r)
      (recur r (apply conj ret (map #(conj (vector %) (second f)) (range (first f) (ffirst r)))))
      (conj ret f))))

(defn- finalize-linear-index
  [linear-index]
  (->> linear-index
       (seq)
       (sort-by first)
       (complement-linear-index)
       (map second)))

(defn- finalize-index
  "Converts intermidate BAM index data structure into final one. Must be called
  in the final stage."
  [refs index]
  (loop [[f & r] refs
         index index]
    (if f
      (let [rname (:name f)]
        (if (get index rname)
          (recur r (-> index
                       (update-in [rname :bin-index] finalize-bin-index)
                       (update-in [rname :linear-index] finalize-linear-index)))
          (recur r index)))
      index)))

;; Writing index
;; -----------

(defn- write-bin
  [w bin chunks]
  (lsb/write-int w bin)
  ;; chunks
  (lsb/write-int w (count chunks))
  (doseq [{:keys [beg end]} chunks]
    (lsb/write-long w beg)
    (lsb/write-long w end)))

(defn- write-meta-data
  [w meta-data]
  (lsb/write-int w max-bins)
  (lsb/write-int w 2)
  (lsb/write-long w (:first-offset meta-data))
  (lsb/write-long w (:last-offset meta-data))
  (lsb/write-long w (:aligned-alns meta-data))
  (lsb/write-long w (:unaligned-alns meta-data)))

(defn- write-index*!
  [wtr refs alns]
  ;; magic
  (lsb/write-bytes wtr (.getBytes ^String bai-magic))
  ;; n_ref
  (lsb/write-int wtr (count refs))
  (let [indices (make-index-from-blocks refs alns)]
    (doseq [ref refs]
      (let [index (get indices (:name ref))
            n-bin (count (:bin-index index))]
        ;; bins
        (if (zero? n-bin)
          (lsb/write-int wtr 0)
          (do
            ;; # of bins
            (lsb/write-int wtr (inc n-bin))
            (doseq [bin (:bin-index index)]
              (write-bin wtr (:bin bin) (:chunks bin)))
            ;; meta data
            (write-meta-data wtr (:meta-data index))))
        ;; linear index
        (lsb/write-int wtr (count (:linear-index index)))
        (doseq [l (:linear-index index)]
          (lsb/write-long wtr l))))
    ;; no coordinate alignments
    (lsb/write-long wtr (:no-coordinate-alns indices))))

;; Public
;; ------

(def ^:dynamic *alignments-partition-size*
  "The number of alignments that is loaded each indexing process. This has an
  effect on performance of concurrent indexing. The default value is 10,000."
  10000)

(defn make-index
  "Calculates a BAM index from provided references and alignments. Optionally,
   you can do this process concurrently."
  [refs alns]
  (let [n-threads (get-exec-n-threads)
        make-index-fn (fn [refs alns]
                        (if (= n-threads 1)
                          (make-index* refs alns)
                          (cp/with-shutdown! [pool (cp/threadpool n-threads)]
                            (->> (partition-all *alignments-partition-size* alns)
                                 (cp/pmap pool (partial make-index* refs))
                                 (reduce merge-index)))))]
    (->> alns
         (make-index-fn refs)
         (finalize-index refs))))

(defn make-index-from-blocks
  "Calculates a BAM index from provided references and alignment blocks.
  Optionally, you can do this process concurrently."
  [refs blocks]
  (let [n-threads (get-exec-n-threads)
        make-index-fn (fn [refs blocks]
                        (if (= n-threads 1)
                          (->> blocks
                               (map #(bam-decoder/decode-alignment-block % refs :pointer))
                               (make-index* refs))
                          (cp/with-shutdown! [pool (cp/threadpool (dec n-threads))]
                            (->> (partition-all *alignments-partition-size* blocks)
                                 (cp/pmap pool (fn [sub-blocks]
                                                 (->> sub-blocks
                                                      (map #(bam-decoder/pointer-decode-alignment-block % refs))
                                                      (make-index* refs))))
                                 (reduce merge-index)))))]
    (->> blocks
         (make-index-fn refs)
         (finalize-index refs))))

(defn write-index!
  "Calculates a BAM index from alns, writing the index to a file."
  [^BAIWriter wtr alns]
  (write-index*! (.writer wtr) (.refs wtr) alns))
