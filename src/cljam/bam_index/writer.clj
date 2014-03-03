(ns cljam.bam-index.writer
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as logging]
            [cljam.lsb :as lsb]
            [cljam.cigar :as cgr]
            [cljam.util :refer [gen-vec]]
            [cljam.util.sam-util :as sam-util]
            [cljam.util.bgzf-util :as bgzf-util]
            [cljam.bam-index.common :refer :all])
  (:import [java.io DataOutputStream FileOutputStream Closeable]))

(defn- find-ref
  [refs name]
  (first (filter #(= (:name %) name) refs)))

(def ^:private ^:const max-level-start
  (last level-starts))

(defn- max-bin-num [^long seq-len]
  (+ max-level-start
     (bit-shift-right seq-len 14)))

(defn- bin-count
  [refs name]
  (if-let [ref (find-ref refs name)]
    (inc (max-bin-num (:len ref)))
    (dec max-bins)))

;;
;; BAIWriter
;;

(defmacro deftypeonce
  [name & body]
  (let [loaded (symbol (str name "-loaded__"))]
   `(do (defonce ~loaded (atom false))
        (when-not @~loaded
          (reset! ~loaded true)
          (deftype ~name ~@body)))))

(deftypeonce BAIWriter [writer refs f]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))))

;;
;; write index
;;

(def ^:const bam-lidx-shift 14)

(defn pos->lidx-offset
  [pos]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) bam-lidx-shift))

(defn- optimize-lidx
  "Optimizes the linear index for safety. This seems unnecessary, but samtools
  does this process."
  [lidx largest-lidx-seen]
  (loop [i 0
         lidx* []
         last-non-zero 0]
    (if (<= i largest-lidx-seen)
      (let [l (nth lidx i)]
        (if (zero? l)
          (recur (inc i) (conj lidx* last-non-zero) last-non-zero)
          (recur (inc i) (conj lidx* l) l)))
      lidx*)))

(defn- write-bin
  [w bin]
  (lsb/write-int w (:idx bin))
  ;; chunks
  (lsb/write-int w (count (:chunks bin)))
  (doseq [{:keys [beg end]} (:chunks bin)]
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

;;
;; index
;;

(defrecord ^:private MetaData
  [first-offset last-offset aligned-alns unaligned-alns])

(defn- meta-data
  ([]
     (MetaData. -1 0 0 0))
  ([first-offset last-offset aligned-alns unaligned-alns]
     (MetaData. first-offset last-offset aligned-alns unaligned-alns)))

(defrecord ^:private LinearIndexStatus
  [lidx largest-lidx-seen])

(defn- linear-index-status
  ([]
     (LinearIndexStatus. (gen-vec max-lidx-size 0) -1))
  ([lidx largest-lidx-seen]
     (LinearIndexStatus. lidx largest-lidx-seen)))

(defrecord ^:private IndexStatus
  [bin-index linear-index-status meta-data])

(defn- index-status
  ([]
     (IndexStatus. {} (linear-index-status) (meta-data)))
  ([bin-index linear-index-status meta-data]
     (IndexStatus. bin-index linear-index-status meta-data)))

(defn- update-meta-data
  [meta-data aln]
  (let [{:keys [first-offset last-offset aligned-alns unaligned-alns]} meta-data
        {:keys [beg end]} (:chunk (:meta aln))
        ;; Update
        first-offset'   (if (or (< (bgzf-util/compare beg first-offset) 1)
                                (= first-offset -1))
                          beg first-offset)
        last-offset'    (if (< (bgzf-util/compare last-offset end) 1)
                          end last-offset)
        aligned-alns'   (if (zero? (bit-and (:flag aln) 4))
                          (inc aligned-alns) aligned-alns)
        unaligned-alns' (if-not (zero? (bit-and (:flag aln) 4))
                          (inc unaligned-alns) unaligned-alns)]
    (->MetaData first-offset' last-offset' aligned-alns' unaligned-alns')))

;;; bin-index > {bin1 chunks1, bin2 chunks2, ...}
;;; e.g. {8  [{:beg 0, :end 10} ...]
;;;       10 [...]}

(defn- update-bin-index
  [bin-index aln]
  (let [{:keys [beg end] :as achunk} (:chunk (:meta aln))
        bin (sam-util/compute-bin aln)]
    (if-let [chunks (get bin-index bin)]
      (if (bgzf-util/same-or-adjacent-blocks? (:end (last chunks)) beg)
        (let [l (assoc (last chunks) :end end)]
          (assoc bin-index bin (conj (pop chunks) l)))
        (assoc bin-index bin (conj chunks achunk)))
      (assoc bin-index bin (vector achunk)))))

(defn- update-linear-index-status
  [linear-index-status aln]
  (let [{:keys [lidx largest-lidx-seen]} linear-index-status
        {:keys [beg]} (:chunk (:meta aln))
        aln-beg (:pos aln)
        aln-end (+ aln-beg (cgr/count-ref (:cigar aln)))
        win-beg (if (zero? aln-end)
                  (pos->lidx-offset (dec aln-beg))
                  (pos->lidx-offset aln-beg))
        win-end (if (zero? aln-end)
                  win-beg
                  (pos->lidx-offset aln-end))]
    (let [lidx' (loop [i win-beg, lidx* lidx]
                  (if (<= i win-end)
                    (let [l (nth lidx i)]
                      (if (or (zero? l) (< beg l))
                        (recur (inc i) (assoc lidx* i beg))
                        (recur (inc i) lidx*)))
                    lidx*))
          largest-lidx-seen' (if (> win-end largest-lidx-seen)
                               win-end largest-lidx-seen)]
      (->LinearIndexStatus lidx' largest-lidx-seen'))))

(defn- update-index-status
  [index-status aln]
  (let [{:keys [bin-index linear-index-status meta-data]} index-status
        ;; Update
        meta-data'           (update-meta-data meta-data aln)
        bin-index'           (update-bin-index bin-index aln)
        linear-index-status' (update-linear-index-status linear-index-status aln)]
    (->IndexStatus bin-index' linear-index-status' meta-data')))

(defn- make-index!
  [refs alns]
  (loop [[aln & rest] alns
         ref-name (:rname aln)
         idx-status (index-status)
         no-coordinate-alns 0
         indices {}]
    (if aln
      (let [ref-name' (:rname aln)
            new-ref? (not= ref-name' ref-name)
            idx-status' (update-index-status
                         (if new-ref? (index-status) idx-status) aln)
            no-coordinate-alns' (if (zero? (:pos aln))
                                  (inc no-coordinate-alns)
                                  no-coordinate-alns)
            indices' (if new-ref?
                       (assoc indices ref-name {:meta (:meta-data idx-status)
                                                :bins (:bin-index idx-status)
                                                :lidx (optimize-lidx (:lidx (:linear-index-status idx-status))
                                                                     (:largest-lidx-seen (:linear-index-status idx-status)))})
                       indices)]
        (recur rest ref-name' idx-status' no-coordinate-alns' indices'))
      (assoc indices ref-name {:meta (:meta-data idx-status)
                               :bins (:bin-index idx-status)
                               :lidx (optimize-lidx (:lidx (:linear-index-status idx-status))
                                                    (:largest-lidx-seen (:linear-index-status idx-status)))}
                     :no-coordinate-alns no-coordinate-alns))))

(defn- make-index
  [refs alns]
  (merge
   (zipmap (map :name refs)
           (map (fn [r] {:meta {:first-offset -1
                                :last-offset 0
                                :aligned-alns 0
                                :unaligned-alns 0}
                         :bins {}
                         :lidx []}) refs))
   (make-index! refs alns)))

(defn- write-index*!
  [wtr refs alns]
  ;; magic
  (lsb/write-bytes wtr (.getBytes bai-magic))
  ;; n_ref
  (lsb/write-int wtr (count refs))
  (let [indices (make-index refs alns)]
    (doseq [ref refs]
      (let [index (get indices (:name ref))]
        ;; bins
        (if (zero? (count (:bins index)))
          (lsb/write-int wtr 0)
          (let [bins (:bins index)]
            ;; # of bins
            (lsb/write-int wtr (inc (count bins)))
            (doseq [bin bins]
              (write-bin wtr {:idx (first bin), :chunks (second bin)}))
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

(defn writer
  [f refs]
  (->BAIWriter (DataOutputStream. (FileOutputStream. (io/file f)))
               refs
               (.getAbsolutePath (io/file f))))

(defn write-index
  [^BAIWriter wtr alns]
  (try
    (write-index*! (.writer wtr) (.refs wtr) alns)
    (catch Exception e (do
                         (let [f (io/file (.f wtr))]
                           (when (.exists f)
                             (.delete f)))
                         (logging/error "Failed to create BAM index")
                         (.printStackTrace e)))))
