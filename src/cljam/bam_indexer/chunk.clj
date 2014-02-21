(ns cljam.bam-indexer.chunk
  (:refer-clojure :exclude [compare])
  (:require [cljam.util.bgzf-util :as bgzf-util]))

;;; A chunk expresses a range in a BAM index.
;;; It consists of a start position and a end position.
;;; A chunk is a map like `{:beg 10, :end 20}`.

(defn compare
  "Returns a negative if chunk1 is earlier than chunk2, a positive if it is
  later, 0 if it is equal."
  [chunk1 chunk2]
  (let [ret (Long/signum (- (:beg chunk1) (:beg chunk2)))]
    (if (zero? ret)
      (Long/signum (- (:end chunk1) (:end chunk2)))
      ret)))

(defn overlap?
  "Returns true if the two chunks overlap."
  [chunk1 chunk2]
  (let [comparison (compare chunk1 chunk2)]
    (or (zero? comparison)
        (let [left (if (neg? comparison) chunk1 chunk2)
              right (if (pos? comparison) chunk1 chunk2)
              left-fp (bgzf-util/get-block-address (:end left))
              right-fp (bgzf-util/get-block-address (:beg right))]
          (or (> left-fp right-fp)
              (and (= left-fp right-fp)
                   (let [left-offset (bgzf-util/get-block-offset (:end left))
                         right-offset (bgzf-util/get-block-offset (:beg right))]
                     (> left-offset right-offset))))))))

(defn adjacent?
  "Returns true if the two chunks are adjacent."
  [chunk1 chunk2]
  (or (and (= (bgzf-util/get-block-address (:end chunk1))
              (bgzf-util/get-block-address (:beg chunk2)))
           (= (bgzf-util/get-block-offset (:end chunk1))
              (bgzf-util/get-block-offset (:beg chunk2))))
      (and (= (bgzf-util/get-block-address (:beg chunk1))
              (bgzf-util/get-block-address (:end chunk2)))
           (= (bgzf-util/get-block-offset (:beg chunk1))
              (bgzf-util/get-block-offset (:end chunk2))))))

(defn optimize-chunks
  [chunks min-offset]
  (let [chunks (sort compare chunks)]
    (loop [[f & r] chunks
           last-chunk nil
           ret []]
      (if f
        (cond
         (<= (:end f) min-offset) (recur r last-chunk ret)
         (nil? last-chunk) (recur r f (conj ret f))
         (and (not (overlap? last-chunk f))
              (not (adjacent? last-chunk f))) (recur r f (conj ret f))
         (> (:end f) (:end last-chunk)) (recur r (assoc last-chunk :end (:end f)) ret)
         :else (recur r last-chunk ret))
        ret))))
