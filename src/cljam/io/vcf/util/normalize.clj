(ns cljam.io.vcf.util.normalize
  (:require [clojure.string :as cstr]
            [cljam.io.vcf.util :as vcf-util]
            [cljam.io.sequence :as io-seq]))

(defn- biallelic-map
  [m id->number ^long i ^long n-allele]
  (let [ploidy (or (some-> m :GT vcf-util/parse-genotype count) 2)]
    (into
     {}
     (map
      (fn [[k v]]
        (let [n (id->number k)]
          [k (cond
               (= k :GT) (vcf-util/biallelic-genotype v (inc i))
               (= n "A") [(nth v i)]
               (= n "R") [(first v) (nth v (inc i))]
               (= n "G") (vcf-util/biallelic-coll ploidy n-allele (inc i) v)
               :else v)])))
     m)))

(defn- get-id->number
  [coll]
  (into {} (map (juxt (comp keyword :id) :number)) coll))

(defn make-splitter
  "Returns a function that splits a multiallelic variant into a sequence of
  biallelic variants."
  [{:keys [info]
    format' :format} header]
  (let [info-id->number (get-id->number info)
        format-id->number (get-id->number format')
        samples (map keyword (drop 9 header))]
    (fn split [{:keys [alt] :as v}]
      (let [n (count alt)]
        (if (<= n 1)
          [v]
          (->> alt
               (map-indexed
                (fn sample-ith-alt [i allele]
                  (-> v
                      (assoc :alt [allele])
                      (update :info biallelic-map info-id->number i n)
                      (into
                       (map
                        (fn [s]
                          [s (biallelic-map (v s) format-id->number i n)]))
                       samples))))))))))

(defn- regions-before [chr ^long pos ^long window]
  (->> (range pos 1 (- window))
       (map
        (fn [^long e]
          {:chr chr,
           :start (Math/max 1 (- e window)),
           :end (dec e)}))))

(defn- char-equals-ignore-case?
  ([_]
   true)
  ([x y]
   (= (Character/toUpperCase ^char x)
      (Character/toUpperCase ^char y)))
  ([x y & z]
   (and (char-equals-ignore-case? x y)
        (apply char-equals-ignore-case? y z))))

(defn- trim-right
  "Trims all duplicated allele sequences from right. If reached the start,
  reference sequences of length `window` will be read from `seq-reader` and
  be prepended to the alleles."
  [seq-reader {:keys [chr ^long pos alt]
               ref' :ref
               {:keys [END]} :info :as v} ^long window]
  (let [alleles (cons ref' alt)
        min-end (dec (+ pos (long (apply min (map count alleles)))))
        regs (regions-before chr pos window)
        ref-seqs (sequence ;; unchunk
                  (mapcat
                   (fn [r]
                     (-> seq-reader
                         (io-seq/read-sequence r {:mask? false})
                         cstr/reverse)))
                  regs)
        allele-seqs (map #(concat (cstr/reverse %) ref-seqs) alleles)
        matched-length (->> allele-seqs
                            (apply map char-equals-ignore-case?)
                            (take-while true?)
                            count
                            long
                            (Math/min (dec min-end)))]
    (if (pos? matched-length)
      (let [pos' (-> (some (fn [{:keys [start end]}]
                             (when (<= start (- min-end matched-length) end)
                               start)) regs)
                     (or pos)
                     long)
            [ref'' & alt'] (map
                            #(->> %2
                                  (take (inc (- (dec (+ pos (count %1))) pos')))
                                  (drop matched-length)
                                  reverse
                                  cstr/join)
                            alleles
                            allele-seqs)]
        (cond-> (assoc v :pos pos' :ref ref'' :alt alt')
          (integer? END) (assoc-in [:info :END] (dec (+ pos' (count ref''))))))
      v)))

(defn- trim-left
  "Trims all duplicated allele sequences from left."
  [{:keys [alt] ref' :ref :as v}]
  (let [alleles (cons ref' alt)
        n-trim-left (->> alleles
                         (apply map char-equals-ignore-case?)
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

(defn realign
  "Left-align and trim REF/ALT alleles."
  ([seq-reader variant]
   (realign seq-reader variant {}))
  ([seq-reader
    {:keys [alt]
     ref' :ref
     :as variant}
    {:keys [window] :or {window 100}}]
   (cond
     (some->> (seq alt)
              (every? (comp #{:snv :mnv :insertion :deletion :complex}
                            :type
                            (partial vcf-util/inspect-allele ref'))))
     (trim-left (trim-right seq-reader variant window))

     (and ref' (nil? (seq alt)))
     (cond-> (update variant :ref subs 0 1)
       (integer? (:END (:info variant)))
       (assoc-in [:info :END] (:pos variant)))

     :else variant)))

(defn normalize
  "Splits multiallelic variants and realigns all alleles. Returns a lazy
  sequence of normalized variants. Note that the result may be unsorted due to
  the realignment. Returns a transducer when no variant is provided. The
  transducer does not guarantee thread safety of `ref-reader`."
  ([ref-reader meta-info header]
   (comp
    (mapcat (make-splitter meta-info header))
    (map (partial realign ref-reader))))
  ([ref-reader meta-info header variants]
   (sequence (normalize ref-reader meta-info header) variants)))
