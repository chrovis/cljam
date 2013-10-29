(ns cljam.util.bgzf-util)

(def ^:private shift-amount 16)

(def ^:private address-mask 0xFFFFFFFFFFFF)

(defn compare
  "Negative if fp1 is earlier in file than fp2, positive if it is later, 0 if equal."
  [fp1 fp2]
  (cond
   (= fp1 fp2)                 0
   (and (< fp1 0) (>= fp2 0))  1
   (and (>= fp1 0) (< fp2 0)) -1
   (< fp1 fp2)                -1
   :else                       1))

(defn get-block-address
  "File offset of start of BGZF block for this file pointer."
  [fp]
  (bit-and (bit-shift-right fp shift-amount) address-mask))

(defn same-or-adjacent-blocks?
  "Returns true if fp2 points to somewhere in the same BGZF block, or the one
  immediately following fp1's BGZF block."
  [fp1 fp2]
  (let [block1 (get-block-address fp1)
        block2 (get-block-address fp2)]
    (or (= block1 block2) (= (inc block1) block2))))
