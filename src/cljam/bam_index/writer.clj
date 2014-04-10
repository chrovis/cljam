(ns cljam.bam-index.writer
  (:require [clojure.java.io :as io]
            [cljam.lsb :as lsb]
            [cljam.cigar :as cgr]
            [cljam.util.sam-util :as sam-util]
            [cljam.util.bgzf-util :as bgzf-util]
            [cljam.bam-index.common :refer :all]
            [cljam.bam-index.chunk :as chunk]))

(declare make-index)

;;
;; BAIWriter
;;

(deftype BAIWriter [writer refs f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

;;
;; Indexing
;;

(defn- pos->lidx-offset
  [^long pos]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) linear-index-shift))

(defn- init-meta-data []
  {:first-offset  -1
   :last-offset    0
   :aligned-alns   0
   :unaligned-alns 0})

(defn- update-meta-data
  [meta-data aln]
  (let [{:keys [first-offset last-offset aligned-alns unaligned-alns]} meta-data
        {:keys [beg end]} (:chunk (:meta aln))
        aligned? (zero? (bit-and (:flag aln) 4))]
    (assoc meta-data
      :first-offset   (if (or (< (bgzf-util/compare beg first-offset) 1)
                              (= first-offset -1))
                        beg first-offset)
      :last-offset    (if (< (bgzf-util/compare last-offset end) 1)
                        end last-offset)
      :aligned-alns   (if aligned? (inc aligned-alns) aligned-alns)
      :unaligned-alns (if-not aligned? (inc unaligned-alns) unaligned-alns))))

(defn- init-bin-index
  "bin-index > {bin1 chunks1, bin2 chunks2, ...}
  e.g. {4681 [{:beg 97, :end 555} ...], 37450 [...] ...}"
  [] {})

(defn- update-bin-index
  [bin-index aln]
  (let [bin (sam-util/compute-bin aln)
        achunk (:chunk (:meta aln))]
    (assoc bin-index bin
           (if-let [chunks (get bin-index bin)]
             (if (bgzf-util/same-or-adjacent-blocks? (:end (peek chunks)) (:beg achunk))
               (let [l (assoc (peek chunks) :end (:end achunk))]
                 (conj (pop chunks) l))
               (conj chunks achunk))
             (vector achunk)))))

(defn- finalize-bin-index
  [bin-index]
  (->> bin-index
       (seq)
       (sort-by first)
       (map (partial zipmap [:bin :chunks]))))

(defn- init-linear-index []
  {})

(defn- update-linear-index
  [linear-index aln]
  (let [beg (get-in aln [:meta :chunk :beg])
        aln-beg (:pos aln)
        aln-end (dec (+ aln-beg (cgr/count-ref (:cigar aln))))
        win-beg (if (zero? aln-end)
                  (pos->lidx-offset (dec aln-beg))
                  (pos->lidx-offset aln-beg))
        win-end (if (zero? aln-end)
                  win-beg
                  (pos->lidx-offset aln-end))
        min* (fn [x]
               (if x (min x beg) beg))]
    (loop [i win-beg, lidx* linear-index]
      (if (<= i win-end)
        (recur (inc i) (update-in lidx* [i] min*))
        lidx*))))

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

(defn- init-index-status []
  {:meta-data    (init-meta-data)
   :bin-index    (init-bin-index)
   :linear-index (init-linear-index)})

(defn- update-index-status
  [index-status aln]
  (-> index-status
      (update-in [:meta-data] update-meta-data aln)
      (update-in [:bin-index] update-bin-index aln)
      (update-in [:linear-index] update-linear-index aln)))

(defn- finalize-index
  "Converts intermidate BAM index data structure into final one. This function
  must be called in the final stage."
  [refs index]
  (loop [[f & r] refs
         index index]
    (if f
      (let [rname (:name f)]
        (if (get index rname)
          (recur r (-> index
                       (update-in [rname :bins] finalize-bin-index)
                       (update-in [rname :lidx] finalize-linear-index)))
          (recur r index)))
      index)))

(defn- make-index*
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
                       (assoc indices ref-name {:meta (:meta-data idx-status)
                                                :bins (:bin-index idx-status)
                                                :lidx (:linear-index idx-status)})
                       indices)]
        (recur rest ref-name' idx-status' no-coordinate-alns' indices'))
      (assoc indices ref-name {:meta (:meta-data idx-status)
                               :bins (:bin-index idx-status)
                               :lidx (:linear-index idx-status)}
                     :no-coordinate-alns no-coordinate-alns))))

;;;
;;; Merge indices
;;;

(defn- merge-meta
  [meta1 meta2]
  {:first-offset (let [f1 (:first-offset meta1)
                       f2 (:first-offset meta2)]
                   (cond
                    (= f1 -1) f2
                    (= f2 -1) f1
                    :else (min f1 f2)))
   :last-offset (max (:last-offset meta1) (:last-offset meta2))
   :aligned-alns (+ (:aligned-alns meta1) (:aligned-alns meta2))
   :unaligned-alns (+ (:unaligned-alns meta1) (:unaligned-alns meta2))})

(defn- merge-chunks
  [chunks1 chunks2]
  (loop [[f & r] (sort chunk/compare (concat chunks1 chunks2))
         chunks' []]
    (if f
      (if-let [last-chunk (peek chunks')]
        (if (bgzf-util/same-or-adjacent-blocks? (:end last-chunk) (:beg f))
          (let [l (assoc last-chunk :end (:end f))]
            (recur r (conj (pop chunks') l)))
          (recur r (conj chunks' f)))
        (recur r (conj chunks' f)))
      chunks')))

(defn- merge-bins
  [bin-map1 bin-map2]
  (merge-with merge-chunks bin-map1 bin-map2))

(defn- merge-lidx
  [lidx1 lidx2]
  (merge-with min lidx1 lidx2))

(defn- merge-index
  [idx1 idx2]
  (let [no-coordinate-alns (+ (:no-coordinate-alns idx1) (:no-coordinate-alns idx2))
        idx1 (dissoc idx1 :no-coordinate-alns)
        idx2 (dissoc idx2 :no-coordinate-alns)]
    (assoc (merge-with (fn [v1 v2]
                         {:meta (merge-meta (:meta v1) (:meta v2))
                          :bins (merge-bins (:bins v1) (:bins v2))
                          :lidx (merge-lidx (:lidx v1) (:lidx v2))})
                       idx1 idx2)
      :no-coordinate-alns no-coordinate-alns)))

;;;
;;; Write index
;;;

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
  (let [indices (make-index refs alns :concurrent true)]
    (doseq [ref refs]
      (let [index (get indices (:name ref))
            n-bin (count (:bins index))]
        ;; bins
        (if (zero? n-bin)
          (lsb/write-int wtr 0)
          (do
            ;; # of bins
            (lsb/write-int wtr (inc n-bin))
            (doseq [bin (:bins index)]
              (write-bin wtr (:bin bin) (:chunks bin)))
            ;; meta data
            (write-meta-data wtr (:meta index))))
        ;; linear index
        (lsb/write-int wtr (count (:lidx index)))
        (doseq [l (:lidx index)]
          (lsb/write-long wtr l))))
    ;; no coordinate alignments
    (lsb/write-long wtr (:no-coordinate-alns indices))))

;;;
;;; public
;;;

(def ^:dynamic *alignments-partition-size* 1000)

(defn make-index
  "Calculates a BAM index from provided references and alignments. Optionally,
   you can do this process concurrently."
  [refs alns & {:keys [concurrent] :or {concurrent false}}]
  (let [make-index-fn (fn [refs alns]
                        (if concurrent
                          (->> (partition-all *alignments-partition-size* alns)
                               (pmap (partial make-index* refs))
                               (reduce merge-index))
                          (make-index* refs alns)))]
    (->> alns
         (make-index-fn refs)
         (finalize-index refs))))

(defn write-index!
  "Calculates a BAM index from alns, writing the index to a file."
  [^BAIWriter wtr alns]
  (write-index*! (.writer wtr) (.refs wtr) alns))
