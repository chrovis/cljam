(ns cljam.io.sam.util.flag
  "Utility functions for SAM flags.")

(def ^:const flags
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

(def ^:const flag-keywords
  (vec (map vector (map key (sort-by val flags)) (range))))

(defn decode
  "Returns a set of keywords for a given flag integer."
  [^long f]
  (into #{} (for [[k i] flag-keywords :when (bit-test f i)] k)))

(defn encode
  "Returns a flag integer encoding set of keywords."
  [flag-set]
  (reduce + (map flags flag-set)))

(defn primary?
  "Returns true when an alignment with given flag is a primary line."
  [f]
  (cond
    (integer? f) (zero? (bit-and f 0x900))
    (set? f) (nil? (or (:secondary f) (:supplementary f)))))
