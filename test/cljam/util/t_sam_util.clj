(ns cljam.util.t-sam-util
  "Tests for cljam.util.sam-util."
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.util :as util]
            [cljam.util.sam-util :as sam-util]
            [clojure.string :as cstr]))

(deftest about-parse-header
  (is (= (sam-util/parse-header "@HD	VN:1.3	SO:coordinate\n@SQ	SN:ref	LN:10\n@SQ	SN:ref2	LN:20\n@PG	ID:cljam	PN:cljam	VN:1.0	CL:java -jar cljam.jar")
         {:HD {:VN "1.3" :SO "coordinate"}
          :SQ [{:SN "ref" :LN 10} {:SN "ref2" :LN 20}]
          :PG [{:ID "cljam" :PN "cljam" :VN "1.0" :CL "java -jar cljam.jar"}]})))

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
  (are [?str ?expected] (and
                          (= (map #(bit-and 0xff %)
                                  (sam-util/str->compressed-bases ?str))
                             ?expected)
                          (= (map #(bit-and 0xff %)
                                  (sam-util/str->compressed-bases ?str))
                             (map #(bit-and 0xff %) (encode ?str)))
                          (= (map #(bit-and 0xff %)
                                  (sam-util/str->compressed-bases (cstr/lower-case ?str)))
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
     (is (= (seq (sam-util/str->compressed-bases s)) (encode s))))))

(deftest compressed-bases->str
  (are [?length ?bases ?offset ?expected] (= (sam-util/compressed-bases->str ?length (byte-array (mapv util/ubyte ?bases)) ?offset)
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

;; Reference functions
;; -------------------

(def refs '({:name "ref", :len 45} {:name "ref2", :len 40}))

(deftest make-refs
  (is (= (sam-util/make-refs (:header test-sam)) refs)))

(deftest ref-id
  (are [?name ?expected] (= (sam-util/ref-id refs ?name) ?expected)
    "ref"      0
    "ref2"     1
    "notfound" nil))

(deftest ref-name
  (are [?id ?expected] (= (sam-util/ref-name refs ?id) ?expected)
    0 "ref"
    1 "ref2"
    9 nil))

(deftest ref-by-name
  (are [?name ?expected] (= (sam-util/ref-by-name refs ?name) ?expected)
    "ref"      {:name "ref", :len 45}
    "ref2"     {:name "ref2", :len 40}
    "notfound" nil))

(deftest flags
  (are [?flag ?primary ?set] (and
                               (= (sam-util/decode-flags ?flag) ?set)
                               (= (sam-util/encode-flags ?set) ?flag)
                               (= (sam-util/primary? ?flag) ?primary)
                               (= (sam-util/primary? ?set) ?primary))
    0     true  #{}
    1     true  #{:multiple}
    2     true  #{:properly-aligned}
    3     true  #{:multiple :properly-aligned}
    4     true  #{:unmapped}
    16    true  #{:reversed}
    83    true  #{:multiple :properly-aligned :reversed :first}
    163   true  #{:multiple :properly-aligned :next-reversed :last}
    99    true  #{:multiple :properly-aligned :next-reversed :first}
    147   true  #{:multiple :properly-aligned :reversed :last}
    121   true  #{:multiple :next-unmapped :reversed :next-reversed :first}
    181   true  #{:multiple :unmapped :reversed :next-reversed :last}
    77    true  #{:multiple :unmapped :next-unmapped :first}
    141   true  #{:multiple :unmapped :next-unmapped :last}
    256   false #{:secondary}
    257   false #{:multiple :secondary}
    2048  false #{:supplementary}
    2049  false #{:multiple :supplementary}
    2304  false #{:secondary :supplementary}
    0x900 false #{:secondary :supplementary}))
