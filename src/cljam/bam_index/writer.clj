(ns cljam.bam-index.writer
  (:require [clojure.java.io :refer [file]]
            [clojure.tools.logging :as logging]
            [clojure.pprint :refer [pprint]] ;; TODO for debug only
            [cljam.lsb :as lsb]
            [cljam.cigar :as cgr]
            [cljam.util :refer [gen-vec]]
            [cljam.util.sam-util :as sam-util]
            [cljam.util.bgzf-util :as bgzf-util]
            [cljam.bam-index.common :refer :all])
  (:import [java.io DataOutputStream FileOutputStream Closeable]))

(defn- max-bin-num [seq-len]
  (+ (nth level-starts  (dec (count level-starts)))
     (bit-shift-right seq-len 14)))

(defn- find-ref
  [refs name]
  (first (filter #(= (:name %) name) refs)))

(defn- bin-count
  [refs name]
  (let [ref (find-ref refs name)]
    (if (nil? ref)
      (dec max-bins)
      (-> ref
          :len
          max-bin-num
          inc))))

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

(defn- write-bin
  [w bin]
  (lsb/write-int w (:idx bin))
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

;;
;; index
;;

; OPTIMIZE: make-index! is slow
(defn- make-index!
  [refs alns]
  (let [indices (atom {})
        ref-name (atom nil)
        bins (atom nil)
        bins-seen (atom 0)
        lidx (atom (gen-vec max-lidx-size 0))
        largest-lidx-seen (atom -1)
        meta (atom {:first-offset -1
                    :last-offset 0
                    :aligned-alns 0
                    :unaligned-alns 0})
        no-coordinate-alns (atom 0)]
    ;; meta data
    (doseq [aln alns]
      (when (nil? @ref-name)
        (reset! ref-name (:rname aln))
        (reset! bins (gen-vec (bin-count refs (:rname aln)))))
      (when-not (= (:rname aln) @ref-name)
        (swap! indices assoc @ref-name {:meta @meta
                                        :bins @bins
                                        :lidx (optimize-lidx lidx @largest-lidx-seen)})
        (reset! ref-name (:rname aln))
        (reset! bins (gen-vec (bin-count refs (:rname aln))))
        (reset! bins-seen 0)
        (reset! lidx (gen-vec max-lidx-size 0))
        (reset! largest-lidx-seen -1)
        (reset! meta {:first-offset -1
                      :last-offset 0
                      :aligned-alns 0
                      :unaligned-alns 0}))
      (if (zero? (:pos aln))
        (swap! no-coordinate-alns inc)
        (do (if-not (zero? (bit-and (:flag aln) 4))
              (swap! meta update-in [:unaligned-alns] inc)
              (swap! meta update-in [:aligned-alns] inc))
            (if (or (< (bgzf-util/compare (:beg (:chunk (:meta aln)))
                                          (:first-offset @meta)) 1)
                    (= (:first-offset @meta) -1))
              (swap! meta assoc :first-offset (:beg (:chunk (:meta aln)))))
            (if (< (bgzf-util/compare (:last-offset @meta)
                                      (:end (:chunk (:meta aln)))) 1)
              (swap! meta assoc :last-offset (:end (:chunk (:meta aln)))))))
      ;; bins
      (let [chunk-beg (:beg (:chunk (:meta aln)))
            chunk-end (:end (:chunk (:meta aln)))]
        (let [bin-idx (sam-util/compute-bin aln)
              bin (if (nil? (nth @bins bin-idx))
                    (do
                      (swap! bins assoc bin-idx {:idx bin-idx, :chunks nil})
                      (swap! bins-seen inc)
                      (nth @bins bin-idx))
                    (nth @bins bin-idx))]
          ;; chunk
          (if (seq (:chunks bin))
            (if (bgzf-util/same-or-adjacent-blocks? (:end (last (:chunks bin)))
                                                    chunk-beg)
              (let [last-idx (dec (count (:chunks bin)))
                    last-chunk (last (:chunks bin))
                    new-chunks (assoc (:chunks bin) last-idx (assoc last-chunk :end chunk-end))
                    new-bin (assoc bin :chunks new-chunks)]
                (swap! bins assoc bin-idx new-bin))
              (let [new-bin (update-in bin [:chunks] conj (:chunk (:meta aln)))]
                (swap! bins assoc bin-idx new-bin)))
            (let [new-bin (assoc bin :chunks (vector (:chunk (:meta aln))))]
              (swap! bins assoc bin-idx new-bin))))
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
              (recur (inc win)))))))
    (swap! indices assoc
           @ref-name {:meta @meta
                      :bins @bins
                      :lidx (optimize-lidx lidx @largest-lidx-seen)}
           :no-coordinate-alns @no-coordinate-alns)
    @indices))

(defn- make-index
  [refs alns]
  (merge
   (zipmap (map :name refs)
           (map (fn [r] {:meta {:first-offset -1
                                :last-offset 0
                                :aligned-alns 0
                                :unaligned-alns 0}
                         :bins []
                         :lidx []}) refs))
   (make-index! refs alns)))

(defn- write-index*!
  [wtr refs alns]
  ;; magic
  (lsb/write-bytes wtr (.getBytes bai-magic))
  ;; n_ref
  (lsb/write-int wtr (count refs))
  (let [indices (make-index refs alns)]
;    (println "Made index")              ; TODO: will remove
    (doseq [ref refs]
      (let [index (get indices (:name ref))]
        ;; bins
        (if (zero? (count (:bins index)))
          (lsb/write-int wtr 0)
          (let [bins (filter #(not (nil? %)) (:bins index))]
            ;; # of bins
            (lsb/write-int wtr (inc (count bins)))
            (doseq [bin bins]
              (write-bin wtr bin))
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
  (->BAIWriter (DataOutputStream. (FileOutputStream. (file f)))
               refs
               f))

(defn write-index
  [^BAIWriter wtr alns]
  (try
    (write-index*! (.writer wtr) (.refs wtr) alns)
    (catch Exception e (do
                         (let [f (file (.f wtr))]
                           (when (.exists f)
                             (.delete f)))
                         (logging/error "Failed to create BAM index")
                         (.printStackTrace e)))))
