(ns cljam.io.sam.util.flag
  "Utility functions for SAM flags.")

(def ^:private ^:const flags
  {:multiple         1 ; template having multiple segments in sequencing
   :properly-aligned 2 ; each segment properly aligned according to the aligner
   :unmapped         4 ; segment unmapped
   :next-unmapped    8 ; next segment in the template unmapped
   :reversed        16 ; SEQ begin reverse complemented
   :next-reversed   32 ; SEQ of the next segment in the template being reverse complemented
   :first           64 ; the first segment in the template
   :last           128 ; the last segment in the template
   :secondary      256 ; secondary alignment
   :filtered-out   512 ; not passing filters, such as platform/vendor quality controls
   :duplicated    1024 ; PCR or optical duplicate
   :supplementary 2048 ; supplementary alignment
   })

(def ^:private ^:const flag-keywords
  (vec (map vector (map key (sort-by val flags)) (range))))

(defn- long-bit-test
  [^long x ^long n]
  (. clojure.lang.Numbers testBit x n))

(defn decode
  "Returns a set of keywords for a given flag integer."
  [^long f]
  (into #{} (for [[k i] flag-keywords :when (long-bit-test f i)] k)))

(defn encode
  "Returns a flag integer encoding set of keywords."
  [flag-set]
  (reduce + (map flags flag-set)))

(defmacro encoded
  "Macro to provide an encoded flag with set of keywords."
  [flag-set-literal]
  (let [f (encode flag-set-literal)]
    f))

(defn primary?
  "Returns true when an alignment with given flag is a primary line."
  [^long f]
  (zero? (bit-and f 0x900)))

(defn multiple?
  "Tests if the template has multiple segments."
  [^long f]
  (long-bit-test f 0))

(defn properly-aligned?
  "Tests if the paired-end segments are properly aligned."
  [^long f]
  (long-bit-test f 1))

(defn unmapped?
  "Tests if the segment is unmapped."
  [^long f]
  (long-bit-test f 2))

(defn both-unmapped?
  "Tests if both the segment and its mate segment are unmapped."
  [^long f]
  (zero? (bit-xor (bit-and f 0xC) 0xC)))

(defn reversed?
  "Tests if the segment is reversed."
  [^long f]
  (long-bit-test f 4))

(defn r1?
  "Tests if the segment is the first in the template."
  [^long f]
  (long-bit-test f 6))

(defn r2?
  "Tests if the segment is the last in the template."
  [^long f]
  (long-bit-test f 7))

(defn r1r2
  "Returns 0 for single-end, 1 for R1 and 2 for R2."
  ^long
  [^long f]
  (-> f
      (bit-and 0xC0) ;; last-bit first-bit
      (unsigned-bit-shift-right 6)))

(defn secondary?
  "Tests if the alignment is secondary."
  [^long f]
  (long-bit-test f 8))
