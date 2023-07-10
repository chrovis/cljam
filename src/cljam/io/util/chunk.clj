(ns cljam.io.util.chunk
  (:refer-clojure :exclude [compare])
  (:require [cljam.io.util.bgzf :as bgzf]))

;;; A chunk expresses a range in a BAM index.
;;; It consists of a start position and a end position.
;;; A chunk is a map like `{:beg 10, :end 20}`.

(defrecord Chunk [^long beg ^long end])

(defn compare
  "Returns a negative if chunk1 is earlier than chunk2, a positive if it is
  later, 0 if it is equal."
  ^long
  [^Chunk chunk1 ^Chunk chunk2]
  (let [ret (Long/signum (- (.beg chunk1) (.beg chunk2)))]
    (if (zero? ret)
      (Long/signum (- (.end chunk1) (.end chunk2)))
      ret)))

(defn overlap?
  "Returns true if the two chunks overlap."
  [^Chunk chunk1 ^Chunk chunk2]
  (let [comparison (compare chunk1 chunk2)]
    (or (zero? comparison)
        (let [left (if (neg? comparison) chunk1 chunk2)
              right (if (pos? comparison) chunk1 chunk2)
              left-fp (bgzf/get-block-address (.end left))
              right-fp (bgzf/get-block-address (.beg right))]
          (or (> left-fp right-fp)
              (and (= left-fp right-fp)
                   (let [left-offset (bgzf/get-block-offset (.end left))
                         right-offset (bgzf/get-block-offset (.beg right))]
                     (> left-offset right-offset))))))))

(defn adjacent?
  "Returns true if the two chunks are adjacent."
  [^Chunk chunk1 ^Chunk chunk2]
  (or (and (= (bgzf/get-block-address (.end chunk1))
              (bgzf/get-block-address (.beg chunk2)))
           (= (bgzf/get-block-offset (.end chunk1))
              (bgzf/get-block-offset (.beg chunk2))))
      (and (= (bgzf/get-block-address (.beg chunk1))
              (bgzf/get-block-address (.end chunk2)))
           (= (bgzf/get-block-offset (.beg chunk1))
              (bgzf/get-block-offset (.end chunk2))))))

(defn optimize-chunks
  "Sorts `chunks` and corrects ends of each chunks."
  [chunks ^long min-offset]
  (let [chunks (sort compare chunks)]
    (loop [[^Chunk f & r] chunks
           ^Chunk last-chunk nil
           ret (transient [])]
      (if f
        (cond
          (<= (.end f) min-offset) (recur r last-chunk ret)
          (nil? last-chunk) (recur r f (conj! ret f))
          (and (not (overlap? last-chunk f))
               (not (adjacent? last-chunk f))) (recur r f (conj! ret f))
          (> (.end f) (.end last-chunk)) (let [l (assoc last-chunk :end (.end f))] (recur r l (conj! (pop! ret) l)))
          :else (recur r last-chunk ret))
        (persistent! ret)))))
