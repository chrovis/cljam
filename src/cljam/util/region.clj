(ns cljam.util.region
  "Utility functions for manipulating chromosomal regions."
  (:require [clojure.string :as cstr]
            [proton.core :as proton]))

;;; region predicates
;;; ----------

(defn overlapped-regions?
  "Returns true iff two regions are overlapped with each other."
  [x y]
  (and (= (:chr x) (:chr y))
       (<= (:start y) (:end x))
       (<= (:start x) (:end y))))

;;; region conversion
;;; ----------

(defn merge-regions-with
  "Returns a lazy sequence of merged regions. Input regions must be sorted.
  Neighboring regions apart less than or equal to 'max-gap' will be merged
  with 'merge-fn'. Returns a stateful transducer when no input 'regions' is
  provided."
  ([merge-fn]
   (merge-regions-with merge-fn 0))
  ([merge-fn ^long max-gap]
   (fn merge-regions-transducer [rf]
     (let [last-reg (volatile! nil)]
       (fn merge-regions-rf
         ([] (rf))
         ([r] (rf (if-let [l @last-reg] (rf r l) r)))
         ([r x] (if-let [l @last-reg]
                  (if (and (= (:chr l) (:chr x))
                           (<= (- (dec (:start x)) (:end l)) max-gap))
                    (do (vswap! last-reg merge-fn x) r)
                    (do (vreset! last-reg x) (rf r l)))
                  (do (vreset! last-reg x) r)))))))
  ([merge-fn ^long max-gap regs]
   (if-let [f (first regs)]
     (if-let [s (second regs)]
       (if (and (= (:chr f) (:chr s))
                (<= (- (dec (:start s)) (:end f)) max-gap))
         (let [next-regs (cons (merge-fn f s) (nnext regs))]
           (lazy-seq (merge-regions-with merge-fn max-gap next-regs)))
         (cons f (lazy-seq (merge-regions-with merge-fn max-gap (next regs)))))
       [f])
     [])))

(defn- merge-two-regions
  "Default function to merge two regions."
  [x y]
  (update x :end max (:end y)))

(def
  ^{:doc "Same as 'merge-regions-with' except for 'merge-two-regions' is
  partially applied as merge-fn."
    :arglists '([] [max-gap] [max-gap regions])}
  merge-regions
  (partial merge-regions-with merge-two-regions))

(defn subtract-region
  "Subtract a region from another one.
  Returns a vector of regions."
  [lhs-reg rhs-reg]
  (if (= (:chr lhs-reg) (:chr rhs-reg))
    (filterv
     #(<= (:start %) (:end %))
     [(update lhs-reg :end min (dec (:start rhs-reg)))
      (update lhs-reg :start max (inc (:end rhs-reg)))])
    [lhs-reg]))

(defn complement-regions
  "Returns a sequence of regions complement to in-regions.
  in-regions must be sorted.
  Returns a stateful transducer when no regions provided."
  ([base-region]
   (fn [rf]
     (let [last-reg (volatile! base-region)]
       (fn
         ([] (rf))
         ([r] (rf (if-let [l @last-reg] (rf r l) r)))
         ([r x]
          (if-let [l @last-reg]
            (let [[a b] (subtract-region l x)]
              (vreset! last-reg (or b a))
              (if b (rf r a) r))
            r))))))
  ([base-region in-regions]
   (if-let [reg (first in-regions)]
     (if-let [[a b] (seq (subtract-region base-region reg))]
       (if b
         (cons a (lazy-seq (complement-regions b (next in-regions))))
         (lazy-seq (complement-regions a (next in-regions))))
       [])
     [base-region])))

(defn divide-region
  "Divides a region [start end] into several chunks with maximum length 'step'.
  Returns a lazy sequence of vector."
  [start end step]
  (->> [(inc end)]
       (concat (range start (inc end) step))
       (partition 2 1)
       (map (fn [[s e]] [s (dec e)]))))

(defn divide-refs
  "Divides refs into several chunks with maximum length 'step'.
  Returns a lazy sequence of map containing {:chr :start :end}."
  [refs step]
  (mapcat
   (fn [{:keys [name len]}]
     (map (fn [[s e]] {:chr name :start s :end e})
          (divide-region 1 len step)))
   refs))

;;; validation
;;; ----------

(defn valid-rname?
  "Checks if the given rname conforms to the spec of sam."
  [rname]
  (and rname (string? rname) (re-matches #"[!-)+-<>-~][!-~]*" rname)))

(defn valid-region?
  "Checks if the given region map is a valid 1-based closed range."
  [{:keys [chr start end]}]
  (and start end
       (valid-rname? chr)
       (number? start) (pos? start)
       (number? end) (pos? end)
       (<= start end)))

;;; region <=> string
;;; ----------

(defn parse-region
  "Parse a region string into a map."
  [region-str]
  (when region-str
    (let [pattern #"([!-)+-<>-~][!-~]*?)(:([\d,]+)?(-([\d,]+))?)?"
          [_ chr _ start _ end] (re-matches pattern region-str)
          start' (proton/as-long start)
          end' (proton/as-long end)]
      (when chr
        (cond-> {:chr chr}
          start' (assoc :start start')
          end' (assoc :end end'))))))

(defn parse-region-strict
  "Parse a region string into a map strictly."
  [region-str]
  (let [region-map (parse-region region-str)]
    (when (valid-region? region-map) region-map)))

(defn format-region
  "Format a region map into a string."
  [{:keys [chr start end]}]
  (let [result (->> [chr start end]
                    (take-while some?)
                    (interleave [nil \: \-])
                    (apply str))]
    (when-not (cstr/blank? result) result)))

(defn format-region-strict
  "Format a region map into a string strictly."
  [region-map]
  (when (valid-region? region-map) (format-region region-map)))
