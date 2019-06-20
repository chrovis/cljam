(ns cljam.io.vcf.util
  (:require [clojure.string :as cstr]
            [proton.core :as p]))

(definline dot-or-nil?
  "Checks if given string is equal to \".\" or nil."
  [^String s]
  `(or (nil? ~s)
       (empty? ~s)
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
                     (let [[k vs] (cstr/split ss #"\=" 2)]
                       [(keyword k) ((parser-map k) vs)])))))))))

(defn info-stringifier
  "Returns a stringifier function of INFO field.
   Takes a vector of maps from info rows of meta-info.
   The stringifier takes a map and returns a string."
  [info-meta]
  (let [id-ordered (mapv :id info-meta)
        info-type (into {} (map (juxt :id :type)) info-meta)]
    (fn [info]
      (when info
        (->> id-ordered
             (keep
              (fn [k]
                (when-let [v (info (keyword k))]
                  (if (or (= v :exists) (= (info-type k) "Flag"))
                    k
                    (str k "=" (if (sequential? v) (cstr/join "," v) v))))))
             (cstr/join ";")
             not-empty)))))

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
         (re-seq #"([\||/])?(.|\d+)")
         (map (fn [[_ phase ^String allele]]
                [(when-not (dot-or-nil? allele) (Integer/parseInt allele))
                 (if phase
                   (= phase "|")
                   (neg? (.indexOf ^String gt "/")))])))))

(defn stringify-genotype
  "Stringifies genotype map into VCF-style GT string."
  [gt-seq]
  (when-not (or (nil? gt-seq) (empty? gt-seq))
    (->> gt-seq
         (mapcat
          (fn [[allele phase]]
            [(if phase "|" "/") (or allele \.)]))
         rest
         (apply str))))

(defn genotype-seq
  "Returns a sequence of genotypes represented as sequences of integers."
  ([^long ploidy ^long n-alt-alleles]
   (genotype-seq ploidy n-alt-alleles []))
  ([^long ploidy ^long n-alt-alleles s]
   (mapcat
    (fn [i]
      (if (= 1 ploidy)
        [(cons i s)]
        (genotype-seq (dec ploidy) i (cons i s))))
    (range (inc n-alt-alleles)))))

(defn genotype-index
  "Returns an index for given genotype."
  ^long [genotype]
  {:pre [(seq genotype)
         (every? (complement neg?) genotype)]}
  (letfn [(combination ^long [^long n ^long k]
            (cond
              (< n k) 0
              (= k n) 1
              (or (= k 1) (= k (dec n))) n
              (< (quot n 2) k) (recur n (- n k))
              :else (loop [i (inc (- n k)), j 1, r 1]
                      (if (<= j k)
                        (recur (inc i) (inc j) (quot (* r i) j))
                        r))))]
    (->> genotype
         sort
         (map-indexed (fn [^long m ^long k] (combination (+ k m) (inc m))))
         (apply +))))

(defn biallelic-genotype
  "Converts a multiallelic `genotype` string into a biallelic one. Ignores all
  alleles other than the reference allele and the `target-allele`."
  [genotype ^long target-allele]
  (->> genotype
       parse-genotype
       (map (fn [[allele phased?]]
              [(when allele (if (= target-allele allele) 1 0)) phased?]))
       stringify-genotype))

(defn biallelic-coll
  "Picks up elements in multiallelic `coll` and make a biallelic one. Ignores
  all alleles other than the reference allele and the `target-allele`."
  [^long ploidy ^long n-alt-alleles ^long target-allele coll]
  (keep-indexed
   (fn [i gt]
     (when (every? (fn [^long a] (or (= a 0) (= a target-allele))) gt)
       (nth coll i)))
   (genotype-seq ploidy n-alt-alleles)))

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
              vs (concat
                  (when (not-empty sample-line)
                    (cstr/split sample-line #":"))
                  (repeat nil))]
          (into
           {}
           (map (fn [[k ^String v]]
                  [(keyword k) (when-not (dot-or-nil? v) ((parser-map k) v))]))
           (map vector ks vs)))))))

(defn stringify-sample
  "Converts sample map into string. formats must be a seqeunce of keys in sample-map."
  [formats sample-map]
  (->> formats
       (map (fn [k] [k (get sample-map k)]))
       reverse
       (drop-while (fn [[_ v]] (or (nil? v) (= [nil] v))))
       (map (fn [[_ v]]
              (cond
                (sequential? v) (cstr/join "," (map (fn [i] (if (nil? i) "." i)) v))
                (nil? v) "."
                :else v)))
       reverse
       (cstr/join ":")
       not-empty))

(defn variant-parser
  "Returns a parser function to parse :filter, :info, :FORMAT and sample columns of VCF.
   Takes meta-info and header of VCF.
   The parser takes a variant map and returns a parsed map."
  [meta-info header]
  (let [[fmt-kw & sample-kws] (mapv keyword (drop 8 header))
        parse-info (info-parser (:info meta-info))
        parse-sample (sample-parser (:format meta-info))]
    (fn [v]
      (cond-> v
        true (update :filter parse-filter)
        true (update :info parse-info)
        fmt-kw (update fmt-kw parse-format)
        sample-kws (into (for [k sample-kws] [k (parse-sample (v fmt-kw) (v k))]))))))

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

(def ^:private ^:const long-breakend-regexp
  ;;   pre-seq    [ or ]  chr    pos     [ or ]    post-seq
  #"([ACGTN]*|\.)([\[\]])(.+?)(?::(\d+))([\[\]])([ACGTN]*|\.)")

(def ^:private ^:const short-breakend-regexp
  #"(\.?)([ATGCN]+)(\.?)")

(defn parse-breakend
  "Parses an ALT allele string of SVTYPE=BND.
  Returns a map with mandatory keys:
  - `:bases` bases that replaces the reference place
  - `:join` `:before` or `:after`
  and optional keys for a mate:
  - `:chr` chromosome name of the mate sequence
  - `:pos` genomic position of the mate sequence
  - `:strand` strand of the mate sequence
  Returns `nil` if input `alt` is not a breakend."
  [alt]
  (if-let [[_ pre-seq [left-bracket] chr pos [right-bracket] post-seq]
           (re-matches long-breakend-regexp alt)]
    (let [pre (not-empty pre-seq)
          post (not-empty post-seq)]
      (when (and (= left-bracket right-bracket)
                 (or pre post)
                 (not (and pre post)))
        (when-let [p (p/as-long pos)]
          {:chr chr,
           :pos p,
           :join (if post :before :after),
           :bases (or pre post),
           :strand (if (= left-bracket \])
                     (if post :forward :reverse)
                     (if post :reverse :forward))})))
    (when-let [[_ [pre-dot] bases [post-dot]]
               (re-matches short-breakend-regexp alt)]
      (when (and (or pre-dot post-dot)
                 (not (and pre-dot post-dot)))
        {:bases bases, :join (if pre-dot :before :after)}))))

(defn stringify-breakend
  "Returns a string representation of a breakend. If the input is malformed,
  returns `nil`. See the docstring of `parse-breakend` for the format."
  [{:keys [chr pos strand join] s :bases}]
  (when (and (not-empty s) (#{:before :after} join))
    (if (and chr pos (#{:forward :reverse} strand))
      (let [before? (= join :before)
            bracket (if (= strand :forward)
                      (if before? \] \[)
                      (if before? \[ \]))]
        (str (when-not before? s) bracket chr \: pos bracket (when before? s)))
      (str (when (= :before join) \.) s (when (= :after join) \.)))))

(defn- inspect-nucleotides-allele
  [ref alt]
  (let [ref-length (count ref)
        alt-length (count alt)
        upper-ref (cstr/upper-case ref)
        upper-alt (cstr/upper-case alt)
        left-match (->> (map = upper-ref upper-alt)
                        (take-while true?)
                        count)
        right-match (->> (cstr/reverse upper-alt)
                         (map = (cstr/reverse upper-ref))
                         (take-while true?)
                         count
                         long
                         (Math/min (- (Math/min ref-length alt-length)
                                      left-match)))
        matched-length (+ left-match right-match)]
    (cond
      (= left-match ref-length alt-length)
      {:type :ref}

      (and (= left-match ref-length) (< left-match alt-length))
      {:type :insertion, :offset (dec left-match),
       :n-bases (- alt-length left-match), :inserted (subs alt left-match)}

      (and (< left-match ref-length) (= left-match alt-length))
      {:type :deletion, :offset (dec left-match),
       :n-bases (- ref-length left-match), :deleted (subs ref left-match)}

      (= (inc matched-length) ref-length alt-length)
      {:type :snv, :ref (nth ref left-match),
       :alt (nth alt left-match), :offset left-match}

      (= matched-length alt-length)
      {:type :deletion, :offset (dec left-match),
       :n-bases (- ref-length right-match left-match),
       :deleted (subs ref left-match (- ref-length right-match))}

      (= matched-length ref-length)
      {:type :insertion, :offset (dec left-match),
       :n-bases (- alt-length right-match left-match),
       :inserted (subs alt left-match (- alt-length right-match))}

      (= ref-length alt-length)
      {:type :mnv, :offset left-match,
       :ref (subs ref left-match (- ref-length right-match)),
       :alt (subs alt left-match (- alt-length right-match))}

      :else {:type :complex})))

(defn inspect-allele
  "Inspects an `alt` allele by comparing to a `ref` allele string.
  Returns a map containing `:type` and other detailed information.
  A value of the key `:type` can be one of the followings:
  - `:no-call`            No variant called
  - `:spanning-deletion`  Placeholder to signify an absent sequence
  - `:unspecified`        Unspecified non-ref allele
  - `:ref`                Duplicated allele of REF
  - `:id`                 Symbolic reference
  - `:snv`                Single nucleotide variant
  - `:mnv`                Multiple nucleotide variants
  - `:insertion`          Insertion of a short base sequence
  - `:deletion`           Deletion of a short base sequence
  - `:complete-insertion` Complete insertion of a long sequence
  - `:breakend`           Breakend of a complex rearrangement
  - `:complex`            Complex nucleotide variants other than snv/mnv/indel
  - `:other`              Can't categorize the allele, might be malformed"
  [ref alt]
  (or
   (when (re-matches #"(?i)[ACGTN]+" (or ref ""))
     (condp re-matches (or (not-empty alt) ".")
       #"\." {:type :no-call}
       #"\*" {:type :spanning-deletion}
       #"(X|<\*>|<X>)" {:type :unspecified}
       #"<(.+)>" :>> (fn [[_ id]] {:type :id, :id id})
       #"(?i)([ACGTN])<(.+)>" :>> (fn [[_ [base] id]]
                                    {:type :complete-insertion,
                                     :join :after, :base base, :id id})
       #"(?i)<(.+)>([ACGTN])" :>> (fn [[_ id [base]]]
                                    {:type :complete-insertion,
                                     :join :before, :base base, :id id})
       #"(?i)[ACGTN]+" (inspect-nucleotides-allele ref alt)
       (some-> (parse-breakend alt) (assoc :type :breakend))))
   {:type :other}))
