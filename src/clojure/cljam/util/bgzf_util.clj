(ns cljam.util.bgzf-util)

(def ^:private shift-amount 16)

(def ^:private address-mask 0xFFFFFFFFFFFF)

(defn compare
  [fp1 fp2]
  (cond
   (= fp1 fp2)                 0
   (and (< fp1 0) (>= fp2 0))  1
   (and (>= fp1 0) (< fp2 0)) -1
   (< fp1 fp2)                -1
   :else                       1))

(defn get-block-address
  [fp]
  (bit-and (bit-shift-right fp shift-amount) address-mask))

(defn same-or-adjacent-blocks?
  [fp1 fp2]
  (let [block1 (get-block-address fp1)
        block2 (get-block-address fp2)]
    (or (= block1 block2) (= (inc block1) block2))))
