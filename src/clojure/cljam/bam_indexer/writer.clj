(ns cljam.bam-indexer.writer
  (:require [clojure.java.io :refer [file]]
            [clojure.tools.logging :as logging]
            [cljam.lsb :as lsb]
            [cljam.cigar :as cgr]
            [cljam.util :refer [gen-vec]]
            [cljam.util.sam-util :as sam-util]
            [cljam.util.bgzf-util :as bgzf-util]
            [cljam.bam-indexer.common :refer [bai-magic]])
  (:import [java.io DataOutputStream FileOutputStream]))

(def ^:private max-bins 37450)
(def ^:private level-starts [0 1 9 73 585 4681])

(defn- max-bin-num [seq-len]
  (+ (nth level-starts  (dec (count level-starts)))
     (bit-shift-right seq-len 14)))

(def max-lidx-size (- (inc max-bins) (last level-starts)))

;;
;; BAIWriterStatus
;;

(defrecord BAIWriterStatus
    [valid refs ref-name max-idx idx bins bins-seen lidx largest-lidx-seen meta])

(defn- current-ref
  [^BAIWriterStatus s]
  (nth (:refs s) @(:idx s)))

(defn- bin-count
  [refs idx]
  (-> (nth refs idx)
      :len
      max-bin-num
      inc))

(defn- has-next?
  [s]
  (> (:max-idx s) @(:idx s)))

(defn- writer-status
  [refs]
  (BAIWriterStatus. (atom true)
                    refs
                    (atom (:name (first refs)))
                    (dec (count refs))
                    (atom 0)
                    (atom (gen-vec (bin-count refs 0)))
                    (atom 0)
                    (atom (gen-vec max-lidx-size 0))
                    (atom -1)
                    (atom {:first-offset -1
                           :last-offset 0
                           :aligned-alns 0
                           :unaligned-alns 0
                           :no-coordinate-alns 0})))
;;
;; BAIWriter
;;

(deftype BAIWriter [writer status f]
  java.io.Closeable
  (close [this]
    (.. this writer close)))

;;
;; write index
;;

(def bam-lidx-shift 14)

(defn pos->lidx-offset
  [pos]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) bam-lidx-shift))

(defn- optimize-lidx
  [lidx largest-lidx-seen]
  (let [new-lidx (atom (gen-vec (inc largest-lidx-seen) 0))
        last-non-zero-offset (atom 0)]
    (loop [i 0]
      (when (<= i largest-lidx-seen)
        (if (zero? (nth @lidx i))
          (swap! lidx assoc i @last-non-zero-offset)
          (reset! last-non-zero-offset (nth @lidx i)))
        (swap! new-lidx assoc i (nth @lidx i))
        (recur (inc i))))
    @new-lidx))

(defn- write-null-content
  [w]
  (lsb/write-long w 0))

(defn- write-bin
  [w bin]
  {:pre (< (:bin-num bin) max-bins)}
  ;; bin
  (lsb/write-int w (:bin-num bin))
  ;; chunks
  (lsb/write-int w (count (:chunks bin)))
  (doseq [c (:chunks bin)]
    (lsb/write-long w (:beg c))
    (lsb/write-long w (:end c))))

(defn- write-meta-data
  [w meta-data]
  (lsb/write-int w max-bins)
  (lsb/write-int w 2)
  (lsb/write-long w (:first-offset meta-data))
  (lsb/write-long w (:last-offset meta-data))
  (lsb/write-long w (:aligned-alns meta-data))
  (lsb/write-long w (:unaligned-alns meta-data)))

; bins bins-seen lidx meta-data
; @bins @bins-seen (optimize-lidx lidx @largest-lidx-seen) @meta-data

(defn- write-ref*
  [^DataOutputStream w bins bins-seen lidx meta-data]
  (let [size (if (seq bins) bins-seen 0)]
    (if (zero? size)
      (write-null-content w)
      (do
        ;; n_bin
        (lsb/write-int w (inc size))
        ;; bins
        (doseq [bin bins]
          (when-not (nil? bin)
           (when-not (= (:bin-num bin) max-bins)
             (write-bin w bin))))
        ;; meta data
        (write-meta-data w meta-data)
        ;; n_intv
        (lsb/write-int w (count lidx))
        ;; intv
        (doseq [l lidx]
          (lsb/write-long w l))))))

(defn- write-ref!
  [^DataOutputStream w ^BAIWriterStatus s]
  (write-ref* w
              @(:bins s)
              @(:bins-seen s)
              (optimize-lidx (:lidx s) @(:largest-lidx-seen s))
              @(:meta s)))

(defn- next-ref!
  [^BAIWriterStatus s]
  (swap! (:idx s) inc)
  (reset! (:ref-name s) (:name (current-ref s)))
  (reset! (:bins s) (gen-vec (bin-count (:refs s) @(:idx s))))
  (reset! (:bins-seen s) 0)
  (reset! (:lidx s) (gen-vec max-lidx-size 0))
  (reset! (:largest-lidx-seen s) -1)
  (swap! (:meta s) assoc :first-offset -1, :last-offset 0, :aligned-alns 0, :unaligned-alns 0))

(defn- write-no-coordinate-alns-count
  [^DataOutputStream w meta-data]
  (lsb/write-long w (:no-coordinate-alns meta-data)))

(defn- write-index*
  [^DataOutputStream w ^BAIWriterStatus s aln]
  (when @(:valid s)
    (when-not (= (:rname aln) @(:ref-name s))
      (if (has-next? s)
        (do
          ;; Write data about current ref
          (write-ref! w s)
          ;; Set next ref
          (next-ref! s))
        (reset! (:valid s) false))))
  (when @(:valid s)
    ;; meta data
    (let [meta (:meta s)]
      (if (zero? (:pos aln))
        (swap! meta update-in [:no-coordinate-alns] inc)
        (do (if-not (zero? (bit-and (:flag aln) 4))
              (swap! meta update-in [:unaligned-aln] inc)
              (swap! meta update-in [:aligned-alns] inc))
            (if (or (< (bgzf-util/compare (:beg (:chunk (:meta aln)))
                                          (:first-offset @meta)) 1)
                    (= (:first-offset @meta) -1))
              (swap! meta assoc :first-offset (:beg (:chunk (:meta aln)))))
            (if (< (bgzf-util/compare (:last-offset @meta)
                                      (:end (:chunk (:meta aln)))) 1)
              (swap! meta assoc :last-offset (:end (:chunk (:meta aln))))))))
    ;; bin, intv
    (let [bins (:bins s)
          bins-seen (:bins-seen s)
          lidx (:lidx s)
          largest-lidx-seen (:largest-lidx-seen s)
          bin-num (sam-util/compute-bin aln)]
      (when (nil? (nth @bins bin-num))
        (swap! bins assoc bin-num {:bin-num bin-num, :chunks nil})
        (swap! bins-seen inc))
      (let [bin (nth @bins bin-num)
            chunk-beg (:beg (:chunk (:meta aln)))
            chunk-end (:end (:chunk (:meta aln)))]
        ;; chunk
        (if (seq (:chunks bin))
          (if (bgzf-util/same-or-adjacent-blocks? (:end (last (:chunks bin)))
                                                  chunk-beg)
            (let [last-idx (dec (count (:chunks bin)))
                  last-chunk (last (:chunks bin))
                  new-chunks (assoc (:chunks bin) last-idx (assoc last-chunk :end chunk-end))
                  new-bin (assoc bin :chunks new-chunks)]
              (swap! bins assoc bin-num new-bin))
            (let [new-bin (update-in bin [:chunks] conj (:chunk (:meta aln)))]
              (swap! bins assoc bin-num new-bin)))
          (let [new-bin (assoc bin :chunks (vector (:chunk (:meta aln))))]
            (swap! bins assoc bin-num new-bin)))
        ;; lenear index
        (let [aln-beg (:pos aln)
              aln-end (+ aln-beg (cgr/count-ref (:cigar aln)))
              window-beg (if (zero? aln-end)
                           (pos->lidx-offset (dec aln-beg))
                           (pos->lidx-offset aln-beg))
              window-end (if (zero? aln-end)
                           window-beg
                           (pos->lidx-offset aln-end))]
          (if (> window-end @largest-lidx-seen)
            (reset! largest-lidx-seen window-end))
          (loop [win window-beg]
            (when (<= win window-end)
              (if (or (zero? (nth @lidx win))
                      (< chunk-beg (nth @lidx win)))
                (swap! lidx assoc win chunk-beg))
              (recur (inc win)))))))))

;;;
;;; public
;;;

(defn writer
  [f refs]
  (->BAIWriter (DataOutputStream. (FileOutputStream. (file f)))
               (writer-status refs)
               f))

(defn write-index
  [^BAIWriter wtr alns]
  (try
    (let [w (.writer wtr)
          s (.status wtr)]
      ;; magic
      (lsb/write-bytes w (.getBytes bai-magic))
      ;; n_ref
      (lsb/write-int w (count (:refs s)))
      (doseq [aln alns] (write-index* w s aln))
      ;; Write data about last ref
      (write-ref! w s)
      (write-no-coordinate-alns-count w @(:meta s)))
    (catch Exception e (do
                         (let [f (file (.f wtr))]
                           (when (.exists f)
                             (.delete f)))
                         (logging/error "Failed to create BAM index")
                         (.printStackTrace e)))))
