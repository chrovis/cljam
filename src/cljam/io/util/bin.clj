(ns cljam.io.util.bin
  (:require [cljam.io.util.chunk :as util-chunk]))

(defn max-pos
  "Returns a maximum position of a binning index. The value is identical to the
  width of bin 0."
  ^long [^long min-shift ^long depth]
  (bit-shift-left 1 (+ min-shift (* 3 depth))))

(defn first-bin-of-level
  "Returns a left-most bin number of the given `level`."
  ^long [^long level]
  (quot (bit-shift-left 1 (* 3 level)) 7))

(defn bin-width-of-level
  "Returns a width shared by bins of the same given `level`."
  ^long [^long level ^long min-shift ^long depth]
  (bit-shift-left 1 (+ min-shift (* 3 (- depth level)))))

(defn bin-level
  "Returns a level that the given `bin` belongs to."
  ^long [^long bin]
  (let [x (inc (quot (- 64 (Long/numberOfLeadingZeros bin)) 3))]
    (cond-> x (< bin (first-bin-of-level x)) dec)))

(defn bin-beg
  "Returns a beginning position of the given `bin`. 1-based."
  ^long [^long bin ^long min-shift ^long depth]
  (let [level (bin-level bin)]
    (inc (* (- bin (first-bin-of-level level))
            (bin-width-of-level level min-shift depth)))))

(defn max-bin
  "Returns a maximum bin number of a binning index with the given `depth`."
  ^long [^long depth]
  (dec (first-bin-of-level (inc depth))))

(defn leading-bins-at-level
  "Returns the distance between the bin corresponding to `pos` and the first one
  at the same level."
  ^long [^long pos ^long level ^long min-shift ^long depth]
  (unsigned-bit-shift-right pos (+ min-shift (* (- depth level) 3))))

(defn pos->lidx-offset
  "Returns an offset of a linear index that the given `pos` belongs to."
  [^long pos ^long linear-index-shift]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) linear-index-shift))

(defn reg->bins
  "Returns all overlapping bins for the specified region [`beg`, `end`] as a
  vector."
  [^long beg ^long end ^long min-shift ^long depth]
  (let [max-pos (max-pos min-shift depth)
        beg (dec (Math/min max-pos (Math/max 1 beg)))
        end (dec (Math/min max-pos (Math/max 1 end)))]
    (into [0]
          (mapcat
           (fn [^long d]
             (let [t (long (transduce
                            (map (fn [^long x] (bit-shift-left 1 (* x 3))))
                            + 0 (range (inc d))))
                   s (+ min-shift (* 3 (- depth d 1)))]
               (range (+ t (bit-shift-right beg s))
                      (+ t 1 (bit-shift-right end s))))))
          (range depth))))

(defn reg->bin
  "Calculates the smallest bin containing the given region [`beg`, `end`]."
  ^long [^long beg ^long end ^long min-shift ^long depth]
  (let [max-pos (max-pos min-shift depth)
        beg (dec (Math/min max-pos (Math/max 1 beg)))
        end (dec (Math/min max-pos (Math/max 1 end)))]
    (loop [level depth]
      (if-not (neg? level)
        (let [beg-bins (leading-bins-at-level beg level min-shift depth)]
          (if (= beg-bins (leading-bins-at-level end level min-shift depth))
            (+ (first-bin-of-level level) beg-bins)
            (recur (dec level))))
        0))))

(defprotocol IBinningIndex
  (get-chunks [this ref-idx bins])
  (get-min-offset [this ref-idx beg])
  (get-min-shift [this])
  (get-depth [this])
  (get-chr-names [this]))

(defn get-spans
  "Calculates span information for random access from index data such as tabix."
  [index-data ^long ref-idx ^long beg ^long end]
  (let [bins (reg->bins
              beg end (get-min-shift index-data) (get-depth index-data))
        chunks (get-chunks index-data ref-idx bins)
        min-offset (get-min-offset index-data ref-idx beg)]
    (->> (util-chunk/optimize-chunks chunks min-offset)
         (map vals))))
