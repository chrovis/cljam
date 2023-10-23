(ns cljam.io.sam.util.option
  "Utility functions for SAM optional fields."
  (:require [clojure.string :as cstr]
            [proton.core :as p]))

;;; parse

(defn- parse-tag-single [val-type val']
  (case val-type
    \Z val'
    \A (first val')
    \I (p/as-long val')
    \i (p/as-long val')
    \s (p/as-long val')
    \S (p/as-long val')
    \c (p/as-long val')
    \C (p/as-long val')
    \f (p/as-double val')
    \H (p/hex->bytes val')
    (-> "Unrecognized tag type: %s, for value: %s"
        (format val-type val')
        Exception.
        throw)))

(defn parse-optional-field
  "Parses an optional field string."
  [op]
  (let [[tag val-type-str val'] (cstr/split op #":" 3)
        val-type (first val-type-str)]
    {(keyword tag) {:type val-type-str
                    :value (if (= val-type \B)
                             val'
                             (parse-tag-single val-type val'))}}))

;;; stringify

(defn stringify-optional-fields
  "Converts a sequence of optional fields to a string."
  [options]
  (->> options
       (map
        (fn [op]
          (let [[tag {:keys [value] type' :type}] (first (seq op))]
            (cstr/join \: [(name tag) type' value]))))
       (cstr/join \tab)))

;;; accessors

(defn value-for-tag
  "Returns a value of an optional field named `tag` of an alignment."
  [tag aln]
  (:value (some tag (:options aln))))

(def
  ^{:doc "CIGAR string of the mate alignment."
    :arglists '([aln])}
  mate-cigar
  (partial value-for-tag :MC))

(def
  ^{:doc "Score of the alignment."
    :arglists '([aln])}
  score
  (partial value-for-tag :AS))

(def
  ^{:doc "Barcode sequence."
    :arglists '([aln])}
  barcode
  (partial value-for-tag :BC))

(def
  ^{:doc "Edit distance from reference of the alignment."
    :arglists '([aln])}
  edit-distance
  (partial value-for-tag :NM))

(defn parse-mismatching-positions-str
  "Parse mismatching positions in the SAM optional fields. Returns a sequence
  consisting of vectors which are one of [:match matching-length],
  [:mismatch a-reference-base-char] and [:deletion reference-bases-string]."
  [s]
  (when s
    (let [[_ head tail] (re-matches #"(\d+)(.*)" s)]
      (->> (re-seq #"(([A-Z])|\^([A-Z]+))(\d+)" tail)
           (mapcat
            (fn [[_ _ mismatch deletion match]]
              [(if mismatch
                 [:mismatch (first mismatch)]
                 [:deletion deletion])
               [:match (p/as-long match)]]))
           (cons [:match (p/as-long head)])))))

(def
  ^{:doc "Mismatching positions and bases of the alignment."
    :arglists '([aln])}
  mismatching-positions
  (comp parse-mismatching-positions-str (partial value-for-tag :MD)))

(def
  ^{:doc "Ratio of the primary alignment score and the alternative one."
    :arglists '([aln])}
  primary-to-alt-score
  (partial value-for-tag :pa))

(defn parse-supplementary-alignments-str
  "Parse serialized supplementary alignments."
  [s]
  (when s
    (->> (re-seq #"(\S+?),(\d+),([+-]),((?:\d+[MIDSH])+),(\d+),(\d+);" s)
         (map (fn [[_ & rests]]
                (-> [:rname :pos :strand :cigar :mapq :edit-distance]
                    (zipmap rests)
                    (update :pos p/as-long)
                    (update :mapq p/as-long)
                    (update :strand (fn [^String strand]
                                      (case (.charAt strand 0)
                                        \+ :forward
                                        \- :reverse)))
                    (update :edit-distance p/as-long)))))))

(def
  ^{:doc "List of supplementary alignments."
    :arglists '([aln])}
  supplementary-alignments
  (comp parse-supplementary-alignments-str (partial value-for-tag :SA)))

(defn parse-alternative-hits-str
  "Parse serialized supplementary alignments."
  [s]
  (when s
    (->> (re-seq #"(\S+?),([+-]?\d+),((?:\d+[MIDSH])+),(\d+);" s)
         (map (fn [[_ & rests]]
                (-> (zipmap [:rname :pos :cigar :edit-distance] rests)
                    (update :pos p/as-long)
                    (update :edit-distance p/as-long)))))))

(def
  ^{:doc "List of alternative alignments."
    :arglists '([aln])}
  alternative-hits
  (comp parse-alternative-hits-str (partial value-for-tag :XA)))

(def
  ^{:doc "Suboptimal alignment score."
    :arglists '([aln])}
  suboptimal-score
  (partial value-for-tag :XS))

(def
  ^{:doc "Name of read group of the alignment."
    :arglists '([aln])}
  read-group
  (partial value-for-tag :RG))

(def
  ^{:doc "Comment of reference sequence."
    :arglists '([aln])}
  ref-comment
  (partial value-for-tag :XR))
