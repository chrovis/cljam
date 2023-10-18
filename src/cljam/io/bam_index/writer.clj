(ns cljam.io.bam-index.writer
  (:require [com.climate.claypoole :as cp]
            [cljam.common :refer [get-exec-n-threads]]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.util.bin :as util-bin]
            [cljam.io.bam-index.common :refer [linear-index-shift
                                               linear-index-depth
                                               max-bins
                                               bai-magic]]
            [cljam.io.util.chunk :as chunk]
            [cljam.io.bam.decoder :as bam-decoder])
  (:import [java.io DataOutputStream Closeable]
           [cljam.io.bam.decoder BAMPointerBlock]
           [cljam.io.util.chunk Chunk]))

;; BAIWriter
;; ---------

(deftype BAIWriter [^DataOutputStream writer refs url]
  Closeable
  (close [_]
    (.close writer)))

;; ### Intermediate data definitions
;;
;; Use record for performance.
;; Record is faster than map for retrieving elements.

(defrecord MetaData [^long first-offset ^long last-offset ^long aligned-alns ^long unaligned-alns])

(defrecord IndexStatus [^MetaData meta-data bin-index linear-index])

;; ### Initializing

(defn- init-index-status
  "Returns initialized index status. This data structure is intermediate. Must
  be passed `finalize-index` in the final stage."
  []
  (IndexStatus. (MetaData. -1 0 0 0)
                ;; Intermediate bin-index -> {bin1 chunks1, bin2 chunks2, ...}
                ;; e.g. {4681 [{:beg 97, :end 555} ...], 37450 [...] ...}
                {}
                ;; Intermediate linear-index -> {pos1 value1, pos2 value2, ...}
                ;; e.g. {5415 4474732776, 14827 5955073327, ...}
                {}))

;; ### Updating

(defn- update-meta-data
  [^MetaData meta-data ^BAMPointerBlock aln]
  (let [first-offset (.first-offset meta-data)
        last-offset (.last-offset meta-data)
        aligned-alns (.aligned-alns meta-data)
        unaligned-alns (.unaligned-alns meta-data)
        beg (.pointer-beg aln)
        end (.pointer-end aln)
        aligned? (zero? (bit-and (.flag aln) 4))]
    (MetaData.
     ;; first-offset
     (if (or (< (bgzf/compare beg first-offset) 1)
             (= first-offset -1))
       beg first-offset)
     ;; last-offset
     (if (< (bgzf/compare last-offset end) 1)
       end last-offset)
     ;; aligned-alns
     (if aligned? (inc aligned-alns) aligned-alns)
     ;; unaligned-alns
     (if-not aligned? (inc unaligned-alns) unaligned-alns))))

(defn- update-bin-index
  [bin-index ^BAMPointerBlock aln]
  (let [bin (util-bin/reg->bin
             (.pos aln) (.end aln) linear-index-shift linear-index-depth)
        beg (.pointer-beg aln)
        end (.pointer-end aln)]
    (assoc bin-index bin
           (if-let [chunks (get bin-index bin)]
             (let [last-chunk ^Chunk (peek chunks)]
               (if (bgzf/same-or-adjacent-blocks? (.end last-chunk) beg)
                 (conj (pop chunks) (Chunk. (.beg last-chunk) end))
                 (conj chunks (Chunk. beg end))))
             [(Chunk. beg end)]))))

(defn- update-linear-index
  [linear-index ^BAMPointerBlock aln]
  (let [beg (.pointer-beg aln)
        aln-beg (.pos aln)
        aln-end (.end aln)
        win-beg (if (zero? aln-end)
                  (util-bin/pos->lidx-offset (dec aln-beg) linear-index-shift)
                  (util-bin/pos->lidx-offset aln-beg linear-index-shift))
        win-end (if (zero? aln-end)
                  win-beg
                  (util-bin/pos->lidx-offset aln-end linear-index-shift))
        min* (fn ^long [x]
               (if x (min (long x) beg) beg))]
    (loop [i win-beg, ret linear-index]
      (if (<= i win-end)
        (recur (inc i) (assoc ret i (min* (get ret i))))
        ret))))

(defn- update-index-status
  [^IndexStatus index-status aln]
  (IndexStatus. (update-meta-data (.meta-data index-status) aln)
                (update-bin-index (.bin-index index-status) aln)
                (update-linear-index (.linear-index index-status) aln)))

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
  (loop [[^Chunk f & r] (sort chunk/compare (concat chunks1 chunks2))
         chunks' []]
    (if f
      (if-let [last-chunk ^Chunk (peek chunks')]
        (if (bgzf/same-or-adjacent-blocks? (.end last-chunk) (.beg f))
          (let [l (assoc last-chunk :end (.end f))]
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
  (loop [[f & r] (if (zero? (long (ffirst linear-index)))
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

;; Writing index
;; -----------

(defn- write-bin
  [w ^long bin chunks]
  (lsb/write-int w bin)
  ;; chunks
  (lsb/write-int w (count chunks))
  (doseq [^Chunk chunk' chunks]
    (lsb/write-long w (.beg chunk'))
    (lsb/write-long w (.end chunk'))))

(defn- write-meta-data
  [w meta-data]
  (lsb/write-int w max-bins)
  (lsb/write-int w 2)
  (lsb/write-long w (:first-offset meta-data))
  (lsb/write-long w (:last-offset meta-data))
  (lsb/write-long w (:aligned-alns meta-data))
  (lsb/write-long w (:unaligned-alns meta-data)))

;; Public
;; ------

(def ^:dynamic *alignments-partition-size*
  "The number of alignments that is loaded each indexing process. This has an
  effect on performance of concurrent indexing. The default value is 10,000."
  10000)

;; Merging indices
;; -------------

(defn merge-index
  "Merges two intermediate indices, returning the merged intermediate index."
  [idx1 idx2]
  (let [no-coordinate-alns (+ (long (:no-coordinate-alns idx1))
                              (long (:no-coordinate-alns idx2)))
        idx1 (dissoc idx1 :no-coordinate-alns)
        idx2 (dissoc idx2 :no-coordinate-alns)]
    (-> (merge-with
         (fn [^IndexStatus v1 ^IndexStatus v2]
           (IndexStatus. (merge-meta-data (.meta-data v1) (.meta-data v2))
                         (merge-bin-index (.bin-index v1) (.bin-index v2))
                         (merge-linear-index (.linear-index v1) (.linear-index v2))))
         idx1 idx2)
        (assoc :no-coordinate-alns no-coordinate-alns))))

;; Making index
;; -----------

(defn finalize-index
  "Converts intermediate BAM index data structure into final one. Must be called
  in the final stage."
  [^long nrefs index]
  (loop [i 0
         index index]
    (if (< i nrefs)
      (if (get index i)
        (recur (inc i) (-> index
                           (update-in [i :bin-index] finalize-bin-index)
                           (update-in [i :linear-index] finalize-linear-index)))
        (recur (inc i) index))
      index)))

(defn make-index*
  "Calculates index from the references and alignments, returning it as a map.
  Returned index is still intermediate. It must be passed to finalize function
  in the final stage."
  [alns]
  (loop [[^BAMPointerBlock aln & rest'] alns
         rid (.ref-id aln)
         idx-status (init-index-status)
         no-coordinate-alns 0
         indices {}]
    (if aln
      (let [rid' (.ref-id aln)
            new-ref? (not= rid' rid)
            idx-status' (update-index-status
                         (if new-ref? (init-index-status) idx-status) aln)
            no-coordinate-alns' (if (zero? (.pos aln))
                                  (inc no-coordinate-alns)
                                  no-coordinate-alns)
            indices' (if new-ref?
                       (assoc indices rid idx-status)
                       indices)]
        (recur rest' rid' idx-status' no-coordinate-alns' indices'))
      (assoc indices rid idx-status
             :no-coordinate-alns no-coordinate-alns))))

(defn make-index-from-blocks
  "Calculates a BAM index from provided references and alignment blocks.
  Optionally, you can do this process concurrently."
  [^long nrefs blocks]
  (let [n-threads (get-exec-n-threads)
        make-index-fn (fn [blocks]
                        (if (= n-threads 1)
                          (->> blocks
                               (eduction (map bam-decoder/decode-pointer-block))
                               make-index*)
                          (cp/with-shutdown! [pool (cp/threadpool (dec n-threads))]
                            (->> blocks
                                 (eduction (partition-all *alignments-partition-size*))
                                 (cp/upmap pool (fn [sub-blocks]
                                                  (->> sub-blocks
                                                       (eduction (map bam-decoder/decode-pointer-block))
                                                       make-index*)))
                                 (reduce merge-index {:no-coordinate-alns 0})))))]
    (->> blocks
         make-index-fn
         (finalize-index nrefs))))

(defn update-last-pointer
  "Update the last pointer of the index to the given value."
  [index eof-ptr]
  (if (or (= (keys index) [:no-coordinate-alns])
          (pos? (long (get index :no-coordinate-alns 0))))
    index
    (let [last-ref (apply max (keys (dissoc index :no-coordinate-alns)))
          last-key (->> (for [[bin chunks] (get-in index [last-ref :bin-index])
                              [i {:keys [end]}] (map-indexed vector chunks)]
                          [end [last-ref :bin-index bin i :end]])
                        (apply max-key first)
                        last)]
      (-> index
          (assoc-in [last-ref :meta-data :last-offset] eof-ptr)
          (assoc-in last-key eof-ptr)))))

;; Writing index
;; -----------

(defn write-index*!
  "Write the index to a file."
  [wtr ^long nrefs indices]
  ;; magic
  (lsb/write-bytes wtr (.getBytes ^String bai-magic))
  ;; n_ref
  (lsb/write-int wtr nrefs)
  (dotimes [i nrefs]
    (let [index (get indices i)
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
  (lsb/write-long wtr (:no-coordinate-alns indices)))

;; ------

(defn write-index!
  "Calculates a BAM index from alns, writing the index to a file."
  [^BAIWriter wtr alns]
  (let [nrefs (count (.refs wtr))
        indices (make-index-from-blocks nrefs alns)]
    (write-index*! (.writer wtr) nrefs indices)))
