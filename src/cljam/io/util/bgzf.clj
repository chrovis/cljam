(ns cljam.io.util.bgzf
  (:refer-clojure :exclude [compare]))

(def ^:private ^:const shift-amount 16)

(def ^:private ^:const address-mask 0xFFFFFFFFFFFF)

(def ^:private ^:const offset-mask 0xFFFF)

(defn compare
  "Negative if fp1 is earlier in file than fp2, positive if it is later, 0 if equal."
  [^long fp1 ^long fp2]
  (cond
   (= fp1 fp2)                 0
   ;; When treating as unsigned, negative number is > positive.
   (and (< fp1 0) (>= fp2 0))  1
   (and (>= fp1 0) (< fp2 0)) -1
   ;; Either both negative or both non-negative, so regular comparison works.
   (< fp1 fp2)                -1
   :else                       1))

(defn get-block-address
  "File offset of start of BGZF block for this file pointer."
  [^long fp]
  (bit-and (bit-shift-right fp shift-amount) address-mask))

(defn get-block-offset
  "Offset into uncompressed block for this virtual file pointer."
  [^long fp]
  (bit-and fp offset-mask))

(defn same-or-adjacent-blocks?
  "Returns true if fp2 points to somewhere in the same BGZF block, or the one
  immediately following fp1's BGZF block."
  [^long fp1 ^long fp2]
  (let [block1 (long (get-block-address fp1))
        block2 (long (get-block-address fp2))]
    (or (= block1 block2) (= (inc block1) block2))))
