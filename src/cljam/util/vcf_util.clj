(ns cljam.util.vcf-util
  (:require [clojure.string :as cstr]
            [cljam.util :as util]))

(definline dot-or-nil?
  "Checks if given string is equal to \".\" or nil."
  [^String s]
  `(or (nil? ~s)
       (and (= 1 (.length ~s))
            (= \. (.charAt ~s 0)))))

(defn- value-parser
  "Returns a parser function for given type idenfier."
  [type]
  (case type
    "Flag" (constantly :exists)
    "Integer" #(when-not (dot-or-nil? ^String %) (Integer/parseInt %))
    "Float" #(when-not (dot-or-nil? ^String %) (Float/parseFloat %))
    "Character" #(when-not (dot-or-nil? ^String %) (first %))
    "String" #(when-not (dot-or-nil? ^String %) %)))

(defn- meta->parser
  "Creates a key-value paired vector of 'id' and its parser."
  [{:keys [id number type]}]
  (let [p (value-parser type)]
    [id (comp (if (or (= number 1) (= number 0)) first identity)
              (fn [x] (map p (cstr/split (or x "") #","))))]))

(defn info-parser
  "Returns a parser function defined by meta-info.
   info-meta must be a sequence of map containing keys :id, :number and :type.
   The parser takes a string and returns a map."
  [info-meta]
  (let [parser-map (into {} (map meta->parser) info-meta)]
    (fn [^String s]
      (when-not (dot-or-nil? s)
        (->> (cstr/split s #";")
             (into
              {}
              (map (fn [ss]
                     (let [[k vs] (cstr/split ss #"\=")]
                       [(keyword k) ((parser-map k) vs)])))))))))

(defn info-stringifier
  "Returns a stringifier function of INFO field.
   Takes a vector of maps from info rows of meta-info.
   The stringifier takes a map and returns a string."
  [info-meta]
  (let [id-ordered (mapv :id info-meta)
        info-type (into {} (map (juxt :id :type)) info-meta)]
    (fn [info]
      (when (some? info)
        (->> id-ordered
             (keep
              (fn [k]
                (when-let [v (info (keyword k))]
                  (if (or (= v :exists) (= (info-type k) "Flag"))
                    k
                    (str k "=" (if (sequential? v) (cstr/join "," v) v))))))
             (cstr/join ";"))))))

(defn parse-filter
  "Parses FILTER field and returns a sequence of keywords."
  [^String s]
  (when-not (dot-or-nil? s)
    (map keyword (cstr/split s #";"))))

(defn stringify-filter
  "Stringifies FILTER field. Takes a sequence of keywords or strings."
  [fltr]
  (when (and (some? fltr) (some some? fltr))
    (cstr/join ";" (map name fltr))))

(defn parse-format
  "Parses FORMAT field and returns as a sequence of keywords."
  [^String s]
  (when-not (dot-or-nil? s)
    (map keyword (cstr/split s #":"))))

(defn stringify-format
  "Stringifies FORMAT field. Takes a sequence of keywords or strings."
  [formats]
  (when-not (or (nil? formats) (empty? formats))
    (cstr/join ":" (map name formats))))

(defn parse-genotype
  "Parses genotype (GT) format and returns a vector of pairs: [allele, phased].
   Allele 0 indicates REF allele. 1,2,3... for 1st, 2nd, 3rd allele of ALT."
  [^String gt]
  (when-not (dot-or-nil? gt)
    (->> gt
         (re-seq #"([\||/])?(\d+)")
         (map (fn [[_ phase allele]]
                [(Integer/parseInt allele)
                 (if phase
                   (= phase "|")
                   (neg? (.indexOf ^String gt "/")))])))))

(defn stringify-genotype
  "Stringifies genotype map into VCF-style GT string."
  [gt-seq]
  (when-not (or (nil? gt-seq) (empty? gt-seq))
    (apply str (rest (mapcat (fn [[allele phase]] [(if phase "|" "/") allele]) gt-seq)))))

(defn genotype->ints
  "Convert genotype to a sequence of integers."
  [gt]
  (let [parsed-gt (if (string? gt) (parse-genotype gt) gt)]
    (->> (assoc-in (vec parsed-gt) [0 1] false)
         (map (fn [[allele phased]] (bit-or (bit-shift-left (inc allele) 1) (if phased 1 0)))))))

(defn ints->genotype
  "Convert a sequence of integers to genotype string."
  [vs]
  (->> vs
       (mapcat (fn [i] [(if (odd? i) \| \/) (if (zero? (quot i 2)) "." (dec (quot i 2)))]))
       rest
       (apply str)))

(defn sample-parser
  "Returns a parser function defined by meta-formats.
   info must be a sequence of map containing keys :id, :number and :type.
   The parser takes two string (format-line and sample-line) and returns a map."
  [formats-meta]
  (let [parser-map (into {} (map meta->parser) formats-meta)]
    (fn [^String format-line sample-line]
      (when-not (dot-or-nil? format-line)
        (let [ks (cstr/split format-line #":")
              vs (cstr/split sample-line #":")]
          (into
           {}
           (map (fn [[k ^String v]]
                  [(keyword k) (when-not (dot-or-nil? v) ((parser-map k) v))]))
           (map vector ks vs)))))))

(defn stringify-sample
  "Converts sample map into string. formats must be a seqeunce of keys in sample-map."
  [formats sample-map]
  (->> formats
       (map (fn [k] [k (sample-map k)]))
       reverse
       (drop-while (fn [[k v]] (or (nil? v) (= [nil] v))))
       (map (fn [[k v]]
              (cond
                (sequential? v) (cstr/join "," (map (fn [i] (if (nil? i) "." i)) v))
                (nil? v) "."
                :else v)))
       reverse
       (cstr/join ":")))

(defn variant-parser
  "Returns a parser function to parse :filter, :info, :FORMAT and sample columns of VCF.
   Takes meta-info and header of VCF.
   The parser takes a variant map and returns a parsed map."
  [meta-info header]
  (let [[fmt-kw & sample-kws] (mapv keyword (drop 8 header))
        parse-info (info-parser (:info meta-info))
        parse-sample (sample-parser (:format meta-info))]
    (fn [v]
      (-> v
          (update :filter parse-filter)
          (update :info parse-info)
          (update fmt-kw parse-format)
          (merge (into {} (for [k sample-kws] [k (parse-sample (v fmt-kw) (v k))])))))))

(defn variant-vals-stringifier
  "Returns a stringifier function to stringify :filter, :info, :FORMAT and sample columns of VCF.
   Takes meta-info and header of VCF.
   The stringifier takes a parsed variant map and returns a map."
  [meta-info header]
  (let [[fmt-kw & sample-kws] (mapv keyword (drop 8 header))
        stringify-info (info-stringifier (:info meta-info))]
    (fn [v]
      (-> v
          (update :filter stringify-filter)
          (update :info stringify-info)
          (update fmt-kw stringify-format)
          (merge (into {} (for [k sample-kws] [k (stringify-sample (v fmt-kw) (v k))])))))))
