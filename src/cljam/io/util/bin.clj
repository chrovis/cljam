(ns cljam.io.util.bin
  (:require [cljam.io.util.chunk :as util-chunk]))

(defn max-pos ^long [^long min-shift ^long depth]
  (bit-shift-left 1 (+ min-shift (* 3 depth))))

(defn- reg->bins*
  "Returns candidate bins for the specified region as a vector."
  [^long beg ^long end ^long min-shift ^long depth]
  (let [max-pos (max-pos min-shift depth)
        beg (if (<= beg 0) 0 (min (dec beg) max-pos))
        end (if (<= end 0) max-pos (min end max-pos))]
    (into [0]
          (mapcat
           (fn [^long d]
             (let [t
                   (apply + (map (fn [^long x] (bit-shift-left 1 (* x 3)))
                                 (range (inc d))))
                   s (+ min-shift (* 3 (- depth d 1)))]
               (range (+ t (bit-shift-right beg s))
                      (+ t 1 (bit-shift-right (dec end) s))))))
          (range depth))))

(def ^:private reg->bins (memoize reg->bins*))

(defn first-bin-of-level ^long [^long level]
  (quot (bit-shift-left 1 (* 3 level)) 7))

(defn bin-width-of-level ^long [^long level ^long min-shift ^long depth]
  (bit-shift-left 1 (+ min-shift (* 3 (- depth level)))))

(defn bin-level ^long [^long bin]
  (let [x (inc (quot (- 64 (Long/numberOfLeadingZeros bin)) 3))]
    (cond-> x (< bin (first-bin-of-level x)) dec)))

(defn bin-beg ^long [^long bin ^long min-shift ^long depth]
  (let [level (bin-level bin)]
    (inc (* (- bin (first-bin-of-level level))
            (bin-width-of-level level min-shift depth)))))

(defn max-bin ^long [^long depth]
  (dec (first-bin-of-level (inc depth))))

(defprotocol IBinningIndex
  (get-chunks [this ref-idx bins])
  (get-min-offset [this ref-idx beg])
  (get-min-shift [this])
  (get-depth [this])
  (get-chr-names  [this]))

(defn pos->lidx-offset
  [^long pos ^long linear-index-shift]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) linear-index-shift))

(defn get-spans
  "Calculates span information for random access from index data such as tabix."
  [index-data ^long ref-idx ^long beg ^long end]
  (let [bins
        (reg->bins beg end (get-min-shift index-data) (get-depth index-data))
        chunks (get-chunks index-data ref-idx bins)
        min-offset (get-min-offset index-data ref-idx beg)]
    (->> (util-chunk/optimize-chunks chunks min-offset)
         (map vals))))

(defn leading-bins-at-level
  "Returns the distance between the binning corresponding to pos and
   the first one at the passed depth"
  ^long [^long pos ^long level ^long min-shift ^long depth]
  (unsigned-bit-shift-right pos (+ min-shift (* (- depth level) 3))))

(defn reg->bin
  "Calculates bin given an alignment covering [beg, end]"
  ^long [^long beg ^long end ^long min-shift ^long depth]
  (let [max-pos (max-pos min-shift depth)
        beg (dec (Math/min max-pos (Math/max 1  beg)))
        end (dec (Math/min max-pos (Math/max 1  end)))]
    (loop [level depth]
      (if-not (neg? level)
        (let [beg-bins (leading-bins-at-level beg level min-shift depth)]
          (if (= beg-bins (leading-bins-at-level end level min-shift depth))
            (+ (first-bin-of-level level) beg-bins)
            (recur (dec level))))
        0))))
