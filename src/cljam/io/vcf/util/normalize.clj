(ns cljam.io.vcf.util.normalize
  "Functions related to variant normalization"
  (:require [clojure.string :as cstr]
            [cljam.io.vcf.util :as vcf-util]
            [cljam.io.sequence :as io-seq]
            [cljam.util.sequence :as util-seq]))

(defn- biallelic-kv
  [id->number i n-allele ploidy [k v]]
  (let [i (long i)
        n (id->number k)]
    [k (cond
         (= k :GT) (vcf-util/biallelic-genotype v (inc i))
         (= n "A") [(nth v i)]
         (= n "R") [(first v) (nth v (inc i))]
         (= n "G") (vcf-util/biallelic-coll ploidy n-allele (inc i) v)
         :else v)]))

(defn- biallelic-map
  [m id->number ^long i ^long n-allele]
  (let [ploidy (or (some-> m :GT vcf-util/parse-genotype count) 2)]
    (into {} (map (partial biallelic-kv id->number i n-allele ploidy) m))))

(defn splitter
  "Returns a function that splits a multiallelic variant into a sequence of
  biallelic variants."
  [meta-info header]
  (let [f #(->> meta-info % (map (juxt (comp keyword :id) :number)) (into {}))
        info-id->number (f :info)
        format-id->number (f :format)
        samples (map keyword (drop 9 header))]
    (fn [{:keys [alt] :as v}]
      (let [n (count alt)]
        (if (<= n 1)
          [v]
          (->> n
               range
               (map
                (fn [i]
                  (-> v
                      (update :alt nth i)
                      (update :alt vector)
                      (update :info biallelic-map info-id->number i n)
                      (into
                       (map
                        (fn [k]
                          [k (biallelic-map (k v) format-id->number i n)]))
                       samples))))))))))

(defn same-ref?
  "Checks if a REF allele matches the reference sequence."
  [seq-reader {:keys [chr ^long pos ^String ref]}]
  (let [ref-len (.length ref)
        ref-region {:chr chr :start pos :end (dec (+ pos ref-len))}
        ref-seq (-> seq-reader
                    (io-seq/read-sequence ref-region {:mask? false})
                    util-seq/->atgcn)]
    (and ref-seq (.equalsIgnoreCase ^String ref-seq ref))))

(defn- trim-right-m
  "trim-right implementation for general veriants."
  [seq-reader {:keys [chr pos ^String ref alt] :as v} ^long window]
  (let [n (inc (count alt))
        a (->> alt
               (cons ref)
               (map #(StringBuilder. ^String %))
               ^"[Ljava.lang.StringBuilder;" (into-array StringBuilder))
        r ^StringBuilder (aget a 0)]
    (loop [p (long pos)]
      (let [rl (.length r)
            rc (Character/toUpperCase (.charAt r (dec rl)))
            min-len (long (loop [min-len rl
                                 i 1]
                            (if (< i n)
                              (let [sb ^StringBuilder (aget a i)
                                    l (long (.length sb))]
                                (if (= (Character/toUpperCase
                                        (.charAt sb (dec l)))
                                       rc)
                                  (recur (Math/min min-len l) (inc i))
                                  -1))
                              min-len)))]
        (if (and (not (neg? min-len))
                 (or (< 1 min-len) (< 1 p)))
          (do
            (dotimes [i n]
              (let [sb ^StringBuilder (aget a i)]
                (.setLength sb (dec (.length sb)))))
            (if (= min-len 1)
              (let [n-pad (min p window)
                    region {:chr chr, :start (inc (- p n-pad)), :end (dec p)}
                    s (io-seq/read-sequence seq-reader region {:mask? false})]
                (dotimes [i n]
                  (.insert ^StringBuilder (aget a i) 0 s))
                (recur (inc (- p n-pad))))
              (recur p)))
          (assoc v :pos p :ref (.toString r) :alt (map str (rest a))))))))

(defn- trim-right-2
  "trim-right specialized for biallelic variants."
  [rdr {:keys [chr pos ^String ref alt] :as v} ^long window]
  (let [r (StringBuilder. ref)
        a (StringBuilder. ^String (first alt))]
    (loop [p (long pos)]
      (let [rl (.length r)
            rc (.charAt r (unchecked-dec-int rl))
            al (.length a)
            ac (.charAt a (unchecked-dec-int al))
            min-len (unchecked-long (Math/min rl al))]
        (if (and (or (< 1 min-len) (< 1 p))
                 (= (Character/toUpperCase rc) (Character/toUpperCase ac)))
          (do
            (.setLength r (unchecked-dec-int rl))
            (.setLength a (unchecked-dec-int al))
            (if (= min-len 1)
              (let [n-pad (Math/min p window)
                    start (unchecked-inc (unchecked-subtract p n-pad))
                    region {:chr chr, :start start, :end (dec p)}
                    s (io-seq/read-sequence rdr region {:mask? false})]
                (.insert r (unchecked-int 0) s)
                (.insert a (unchecked-int 0) s)
                (recur start))
              (recur p)))
          (assoc v :pos p :ref (.toString r) :alt [(.toString a)]))))))

(defn trim-right
  "Trims all duplicated allele sequences from right. If reached the start,
  reference sequences of length `window` will be read from `seq-reader` and
  prepended to the alleles."
  [seq-reader {:keys [alt] :as v} ^long window]
  (if (<= 2 (count alt))
    (trim-right-m seq-reader v window)
    (trim-right-2 seq-reader v window)))

(defn- trim-left-m
  "trim-left implementation for general variants."
  [alt {:keys [^String ref] :as v}]
  (let [alleles (cons ref alt)
        n-trim-left (->> alleles
                         (map cstr/upper-case)
                         (apply map =)
                         (take-while true?)
                         count)
        min-len (long (apply min (map count alleles)))
        n-trim-left (min (dec min-len) n-trim-left)]
    (if (pos? n-trim-left)
      (-> v
          (update :pos + n-trim-left)
          (update :ref subs n-trim-left)
          (assoc :alt (map #(subs % n-trim-left) alt)))
      v)))

(defn- trim-left-2
  "trim-left specialized for biallelic variants."
  [alt {:keys [^String ref] :as v}]
  (let [rl (.length ref)
        a ^String (first alt)
        al (.length a)
        min-len (unchecked-dec (Math/min rl al))]
    (loop [i 0]
      (if (and (= (Character/toUpperCase (.charAt ref i))
                  (Character/toUpperCase (.charAt a i)))
               (< i min-len))
        (recur (unchecked-inc i))
        (if (pos? i)
          (-> v
              (update :pos + i)
              (assoc :ref (.substring ref i))
              (assoc :alt [(.substring a i)]))
          v)))))

(defn trim-left
  "Trims all duplicated allele sequences from left."
  [{:keys [alt] :as v}]
  (if (<= 2 (count alt))
    (trim-left-m alt v)
    (trim-left-2 alt v)))

(defn realign
  "Left-align and trim REF/ALT alleles."
  ([seq-reader variant]
   (realign seq-reader variant {}))
  ([seq-reader
    {:keys [^String ref alt] :as variant}
    {:keys [window] :or {window 100}}]
   (if (and
        ref (seq alt) (util-seq/atgcn? ref)
        (every?
         (fn [^String a]
           (let [c (.charAt a (unchecked-int 0))]
             (and
              (not= c \<)
              (not= c \*)
              (util-seq/atgcn? a)
              (not (.equalsIgnoreCase ref a)))))
         alt))
     (trim-left (trim-right seq-reader variant window))
     (if (and ref (nil? (seq alt)))
       (update variant :ref subs 0 1)
       variant))))
