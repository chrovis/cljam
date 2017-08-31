(ns cljam.util.whole-genome
  "Utilities for conversions between chromosomal positions and whole-genome
  positions.")

(defn chr-to-whole-genome-index
  "Creates a map of [chromosome-name offset&length] from refs."
  [refs]
  (->> refs
       (map :len)
       (reductions + 0)
       (map (fn [r offset] [(:name r) {:offset offset, :len (:len r)}]) refs)
       (into {})))

(defn ->whole-genome-coord
  "Transforms a position in a chromosome into a whole-genome position."
  [chr->offset chr ^long pos]
  (when-let [{:keys [^long offset ^long len]} (chr->offset chr)]
    (when (<= 1 pos len)
      (inc (+ offset (dec pos))))))

(defn whole-genome-to-chr-index
  "Creates a sorted-map of [offset chromosome-name&length] from refs."
  [refs]
  (into (sorted-map)
        (map vector
         (->> refs
              (map :len)
              (reductions + 0))
         refs)))

(defn ->chr-and-pos
  "Transforms a whole-genome position to a vector of a chromosome name and a
  position in the chromosome."
  [offset->ref ^long wg-pos]
  (when-let [[^long offset {:keys [name len]}]
             (first (rsubseq offset->ref <= (dec wg-pos)))]
    (when (<= 1 (- wg-pos offset) len)
      [name (- wg-pos offset)])))

(defn ->regions
  "Transforms a region in whole-genome coordinate into a sequence of chromosomal
  regions."
  [offset->ref ^long start ^long end]
  (let [wg-len (let [[o r] (first (rsubseq offset->ref >= 0))] (+ o (:len r)))]
    (when (and (<= start end)
               (or (pos? start) (pos? end))
               (or (<= start wg-len) (<= end wg-len)))
      (let [start' (min (max 1 start) wg-len)
            end' (min (max 1 end) wg-len)
            [s-offset] (first (rsubseq offset->ref <= (dec start')))
            [e-offset] (first (rsubseq offset->ref <= (dec end')))
            regs (mapv
                  (fn [[_ r]] {:chr (:name r), :start 1, :end (:len r)})
                  (subseq offset->ref >= s-offset <= e-offset))]
        (-> regs
            (assoc-in [0 :start] (- start' s-offset))
            (update-in [(dec (count regs)) :end] min (- end' e-offset)))))))
