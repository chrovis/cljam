(ns cljam.util.region
  "Utility functions for manipulating chromosomal regions."
  (:require [clojure.string :as cstr]
            [proton.core :as proton]))

;;; region predicates
;;; ----------

(defn overlapped-regions?
  "Returns true if two regions are overlapped with each other."
  [{^long x-start :start
    ^long x-end :end
    x-chr :chr}
   {^long y-start :start
    ^long y-end :end
    y-chr :chr}]
  (and (= x-chr y-chr)
       (<= y-start x-end)
       (<= x-start y-end)))

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
         ([r {^long x-start :start :as x}]
          (if-let [{^long l-end :end :as l} @last-reg]
            (if (and (= (:chr l) (:chr x))
                     (<= (- (dec x-start) l-end) max-gap))
              (do (vswap! last-reg merge-fn x) r)
              (do (vreset! last-reg x) (rf r l)))
            (do (vreset! last-reg x) r)))))))
  ([merge-fn ^long max-gap regs]
   (if-let [{^long f-end :end :as f} (first regs)]
     (if-let [{^long s-start :start :as s}
              (second regs)]
       (if (and (= (:chr f) (:chr s))
                (<= (- (dec s-start) f-end) max-gap))
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
  [lhs-reg
   {^long rhs-reg-start :start
    ^long rhs-reg-end :end
    rhs-reg-chr :chr}]
  (if (= (:chr lhs-reg) rhs-reg-chr)
    (filterv
     (fn [{:keys [^long start ^long end]}] (<= start end))
     [(update lhs-reg :end min (dec rhs-reg-start))
      (update lhs-reg :start max (inc rhs-reg-end))])
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
  [^long start ^long end ^long step]
  (->> [(inc end)]
       (concat (range start (inc end) step))
       (partition 2 1)
       (map (fn [[^long s ^long e]] [s (dec e)]))))

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
  [{:keys [chr ^long start ^long end]}]
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
