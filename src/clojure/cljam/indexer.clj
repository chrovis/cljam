(ns cljam.indexer
  (:require [clojure.java.io :refer [file]]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.lsb :as lsb]
            [cljam.cigar :as cgr]
            [cljam.util :refer [reg->bin]]
            [cljam.util.bgzf-util :as bgzf-util])
  (:import [java.io DataOutputStream FileOutputStream]))

(deftype BAIWriter [writer]
  java.io.Closeable
  (close [this]
    (.. this writer close)))

(defn writer [f]
  (->BAIWriter (DataOutputStream. (BGZFOutputStream. (file f)))))

(def bai-magic "BAI\1")

(defn- get-end [aln]
  (dec
   (+ (:pos aln)
      (cgr/count-ref (:cigar aln)))))

(defn- compute-bin [aln]
  (let [beg (dec (:pos aln))
        tmp-end (get-end aln)
        end (if (<= tmp-end 0) (inc beg) tmp-end)]
   (reg->bin beg end)))

;;; bins: [bin, bin, ...]
;;; bin: {:bin ?, :chunks [chunk, chunk, ...]}
;;; chunk: {:beg ?, :end ?}

(def max-bins 37450)

(def ^:private level-starts [0 1 9 73 585 4681])

(def max-lidx-size (- (inc max-bins) (last level-starts)))

(def bam-lidx-shift 14)

(defn- max-bin-num [seq-len]
  (+ (nth level-starts  (dec (count level-starts)))
     (bit-shift-right seq-len 14)))

(defn convert-to-lidx-offset
  [pos]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) bam-lidx-shift))

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

(defn- optimize-lidx
  [lidx largest-lidx-seen]
  (let [new-lidx (atom (vec (repeat (inc largest-lidx-seen) 0)))
        last-non-zero-offset (atom 0)]
    (loop [i 0]
      (when (<= i largest-lidx-seen)
        (if (zero? (nth @lidx i))
          (swap! lidx assoc i @last-non-zero-offset)
          (reset! last-non-zero-offset (nth @lidx i)))
        (swap! new-lidx assoc i (nth @lidx i))
        (recur (inc i))))
    @new-lidx))

(defn- write-ref
  [w bins bins-seen lidx meta-data]
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

(defn- write-no-coordinate-alns-count
  [w meta-data]
  (lsb/write-long w (:no-coordinate-alns meta-data)))

(defn create-index
  [bam out-bai]
  (with-open [r (bam/reader bam)
              w (DataOutputStream. (FileOutputStream. (file out-bai)))]
    (let [refs (io/read-refs r)
          alns (io/read-alignments r {})]
      ;; magic
      (lsb/write-bytes w (.getBytes bai-magic))
      ;; n_ref
      (lsb/write-int w (count refs))
      (let [current-ref-idx (atom 0)
            bins (atom (vec (repeat (inc (max-bin-num (count refs))) nil)))
            bins-seen (atom 0)
            lidx (atom (vec (repeat max-lidx-size 0)))
            largest-lidx-seen (atom -1)
            meta-data (atom {:first-offset -1, :last-offset 0, :aligned-alns 0, :unaligned-alns 0,
                             :no-coordinate-alns 0})]
        (doseq [aln alns]
          (when-not (= (:rname aln) (:name (nth refs @current-ref-idx)))
            ;; Write data about current ref
            (write-ref w
                       @bins @bins-seen (optimize-lidx lidx @largest-lidx-seen) @meta-data)
            ;; Set next ref
            (swap! current-ref-idx inc)
            (reset! bins (vec (repeat (inc (max-bin-num (count refs))) nil)))
            (reset! bins-seen 0)
            (reset! lidx (vec (repeat max-lidx-size 0)))
            (reset! largest-lidx-seen -1)
            (swap! meta-data assoc :first-offset -1, :last-offset 0, :aligned-alns 0, :unaligned-alns 0))
          ;; meta data
          (if (= (:pos aln) 0)
            (swap! meta-data update-in [:no-coordinate-alns] inc)
            (do (if-not (zero? (bit-and (:flag aln) 4))
                  (swap! meta-data update-in [:unaligned-aln] inc)
                  (swap! meta-data update-in [:aligned-alns] inc))
                (if (or (< (bgzf-util/compare (:beg (:chunk (:meta aln)))
                                              (:first-offset @meta-data)) 1)
                        (= (:first-offset @meta-data) -1))
                  (swap! meta-data assoc :first-offset (:beg (:chunk (:meta aln)))))
                (if (< (bgzf-util/compare (:last-offset @meta-data)
                                          (:end (:chunk (:meta aln)))) 1)
                  (swap! meta-data assoc :last-offset (:end (:chunk (:meta aln)))))))
          ;; bin, intv
          (let [bin-num (compute-bin aln)]
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
                                 (convert-to-lidx-offset (dec aln-beg))
                                 (convert-to-lidx-offset aln-beg))
                    window-end (if (zero? aln-end)
                                 window-beg
                                 (convert-to-lidx-offset aln-end))]
                (if (> window-end @largest-lidx-seen)
                  (reset! largest-lidx-seen window-end))
                (loop [win window-beg]
                  (when (<= win window-end)
                    (if (or (zero? (nth @lidx win))
                            (< chunk-beg (nth @lidx win)))
                      (swap! lidx assoc win chunk-beg))
                    (recur (inc win))))))))
        ;; Write data about current ref
        (write-ref w
                   @bins @bins-seen (optimize-lidx lidx @largest-lidx-seen) @meta-data)
        (write-no-coordinate-alns-count w @meta-data)))))
