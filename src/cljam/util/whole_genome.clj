(ns cljam.util.whole-genome
  "Utilities for conversions between chromosomal positions and whole-genome positions.")

(defn create-chr-to-whole-genome-index
  "Creates a map of [chromosome-name offset&length] from refs."
  [refs]
  (->> refs
       (map :len)
       (reductions + 0)
       (map (fn [r offset] [(:name r) {:offset offset, :len (:len r)}]) refs)
       (into {})))

(defn to-whole-genome-coord
  "Transforms a position in a chromosome into a whole-genome position."
  [chr->offset chr ^long pos]
  (when-let [{:keys [^long offset ^long len]} (chr->offset chr)]
    (when (<= 1 pos len)
      (inc (+ offset (dec pos))))))

(defn create-whole-genome-to-chr-index
  "Creates a sorted-map of [offset chromosome-name&length] from refs."
  [refs]
  (into (sorted-map)
        (map vector
         (->> refs
              (map :len)
              (reductions + 0))
         refs)))

(defn to-chr-and-pos
  "Transforms a whole-genome position to a vector of a chromosome name and a position in the chromosome."
  [offset->ref ^long wg-pos]
  (when-let [[^long offset {:keys [name len]}] (first (rsubseq offset->ref <= (dec wg-pos)))]
    (when (<= 1 (- wg-pos offset) len)
      [name (- wg-pos offset)])))

(defn to-regions
  "Transforms a region in whole-genome coordinate into a sequence of chromosomal regions."
  [offset->ref ^long start ^long end]
  (let [wg-len (let [[o r] (first (rsubseq offset->ref > 0))] (+ o (:len r)))]
    (when (and (<= start end) (or (pos? start) (pos? end)) (or (<= start wg-len) (<= end wg-len)))
      (let [start' (min (max 1 start) wg-len)
            end' (min (max 1 end) wg-len)
            [s-offset {s-name :name s-len :len}] (first (rsubseq offset->ref <= (dec start')))
            [e-offset {e-name :name e-len :len}] (first (rsubseq offset->ref <= (dec end')))]
        (if (= s-offset e-offset)
          [{:chr s-name, :start (- start' s-offset), :end (min (- end' s-offset) s-len)}]
          (concat
           [{:chr s-name, :start (- start' s-offset), :end s-len}]
           (map
            (fn [[_ r]] {:chr (:name r), :start 1, :end (:len r)})
            (subseq offset->ref > s-offset < e-offset))
           [{:chr e-name, :start 1, :end (min (- end' e-offset) e-len)}]))))))
