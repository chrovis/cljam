(ns cljam.io.sam.util.sequence-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.string :as cstr]
            [cljam.io.sam.util.sequence :as sam-seq]
            [cljam.util :as util]))

(def nibble-table "=ACMGRSVTWYHKDBN")

(defn encode [^String s]
  (->> (concat s [\=])
       (partition 2)
       (map (fn [[u l]]
              (let [ui (.indexOf ^String nibble-table (cstr/upper-case u))
                    li (.indexOf ^String nibble-table (cstr/upper-case l))
                    uu (if-not (neg? ui) ui 15)
                    ll (if-not (neg? li) li 15)]
                (unchecked-byte (bit-or (bit-shift-left uu 4) (bit-and 0xff ll))))))))

(deftest str->compressed-bases-1
  (are [?str ?expected] (and (= (map #(bit-and 0xff %)
                                     (sam-seq/str->compressed-bases ?str))
                                ?expected)
                             (= (map #(bit-and 0xff %)
                                     (sam-seq/str->compressed-bases ?str))
                                (map #(bit-and 0xff %) (encode ?str)))
                             (= (map #(bit-and 0xff %)
                                     (sam-seq/str->compressed-bases (cstr/lower-case ?str)))
                                ?expected))
    "="    [0]
    "=A"   [1]
    "=C"   [2]
    "=G"   [4]
    "=T"   [8]
    "=."   [15]
    "A"    [16]
    "C"    [32]
    "G"    [64]
    "T"    [128]
    "."    [240]
    "==A"  [0 16]
    "===A" [0 1]))

(deftest str->compressed-bases-2
  (dotimes [_ 100]
    (let [s (apply str (repeatedly (inc (rand-int 100)) #(rand-nth nibble-table)))]
      (is (= (seq (sam-seq/str->compressed-bases s)) (encode s))))))

(deftest compressed-bases->str
  (are [?length ?bases ?offset ?expected] (= (sam-seq/compressed-bases->str ?length (byte-array (mapv util/ubyte ?bases)) ?offset)
                                             ?expected)
    1   [0x00]                                    0 "="
    2   [0x00]                                    0 "=="
    1   [0x10]                                    0 "A"
    2   [0x12]                                    0 "AC"
    4   [0x12 0x8F]                               0 "ACTN"
    1   [0x12 0x8F]                               1 "T"
    2   [0x12 0x8F]                               1 "TN"
    16  [0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF] 0 nibble-table
    14  [0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF] 1 (subs nibble-table 2)
    2   [0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF] 7 "BN"
    512 (range 256)                               0 (->> (for [i nibble-table j nibble-table] [i j])
                                                         (apply concat)
                                                         cstr/join)))

(deftest normalize-bases
  (are [?src ?result] (= (vec (sam-seq/normalize-bases (byte-array (map byte ?src))))
                         (map byte ?result))
    [\a \b \c \. \d \e \n \N] [\A \B \C \N \D \E \N \N]))
