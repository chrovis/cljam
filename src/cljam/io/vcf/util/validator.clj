(ns cljam.io.vcf.util.validator
  (:require [cljam.io.vcf.util :as vcf.util]))

(defrecord VCFValidator [ploidy file-type defs samples])

(defn- error [path msg & args]
  {:errors {path [(apply format msg args)]}})

(defn- warn [path msg & args]
  {:warnings {path [(apply format msg args)]}})

(defn- merge-validation-results
  ([] nil)
  ([res] res)
  ([res1 res2]
   (letfn [(rec [x y]
             (cond (nil? x) y
                   (nil? y) x
                   (map? x) (merge-with rec x y)
                   :else (into x y)))]
     (rec res1 res2)))
  ([res1 res2 res3 & more]
   (reduce merge-validation-results res1 (list* res2 res3 more))))

(defn- prepend-keys [res keys']
  ;; takes {:errors {[:chr] ["foo"]}}
  ;; returns {:errors {[key ... :chr] ["foo"]}}
  (when-not (nil? res)
    (into {}
          (map (fn [[kind m]]
                 [kind
                  (into {} (map (fn [[path messages]]
                                  [(into keys' path) messages]))
                        m)]))
          res)))

(defn- error-on-bcf [validator path & args]
  (let [f (if (= :bcf (:file-type validator)) error warn)]
    (apply f path args)))

(defn- prep-contig-defs [_validator contigs]
  (into {} (map (juxt :id identity)) contigs))

(def ^:private ^:const contig-pattern
  #"[0-9A-Za-z!#$%&+./:;?@^_|~-][0-9A-Za-z!#$%&*+./:;=?@^_|~-]*")

(defn- validate-chrom [validator {:keys [chr]}]
  (if (and (string? chr) (re-matches contig-pattern chr))
    (when-not (get-in validator [:defs :contig chr])
      (error-on-bcf validator [:chr] "Contig %s not defined in meta info" chr))
    (error [:chr]
           (str ":chr must be a non-empty string that consists of characters "
                "other than whitespace, commas or angle brackets, but got "
                (pr-str chr)))))

(defn- validate-pos [_validator {:keys [pos]}]
  (when-not (integer? pos)
    (error [:pos] (str ":pos must be integer, but got " (pr-str pos)))))

(defn- validate-ref [_validator {r :ref}]
  (when-not (and (string? r) (re-matches #"(?i)[ATGCN]+" r))
    (error [:ref]
           (str ":ref must be a non-empty string consisting of A/C/G/T/N "
                "(case insensitive), but got " (pr-str r)))))

(defn- validate-alt [_validator {:keys [alt] :as variant}]
  (if (or (nil? alt) (sequential? alt))
    (some->> alt
             (keep-indexed
              (fn [i allele]
                (cond
                  (and (not (nil? allele))
                       (or (not (string? allele)) (= allele "")))
                  {[:alt i]
                   [(str "Every allele in :alt must be a non-empty string or nil, "
                         "but got " (pr-str allele))]}

                  (and (string? (:ref variant))
                       (= :other (:type (vcf.util/inspect-allele (:ref variant) allele))))
                  {[:alt i] [(str "Unexpected allele found: " allele)]})))
             seq
             (apply merge)
             (array-map :errors))
    (error [:alt]
           (str ":alt must be a sequence of strings or nil, but got " (pr-str alt)))))

(defn- validate-qual [_validator {:keys [qual]}]
  (when-not (or (nil? qual) (number? qual))
    (error [:qual]
           (str ":qual must be a number or nil, but got " (pr-str qual)))))

(defn- prep-filter-defs [_validator filters]
  (into {} (map (juxt (comp keyword :id) identity)) filters))

(defn- validate-filter [validator {filt :filter}]
  (if (sequential? filt)
    (->> (for [[i filt] (map-indexed vector filt)]
           (if (and (keyword? filt)
                    (re-matches #"[^ \t;]+" (name filt)))
             (when-not (get-in validator [:defs :filter filt])
               (error-on-bcf validator
                             [:filter i]
                             "Filter %s not defined in meta info" filt))
             (error [:filter i]
                    (str "Every filter in :filter must be a keyword consisting "
                         "of characters other than whitespace or semicolons, "
                         "but got " (pr-str filt)))))
         (apply merge-validation-results))
    (when-not (or (nil? filt) (= filt :PASS))
      (error [:filter]
             (str ":filter must be a sequence of keywords or nil, but got "
                  (pr-str filt))))))

(defn- make-field-type-validator [{:keys [id] type' :type}]
  (let [f (case type'
            "Integer" #(and (integer? %)
                            (< (+ Integer/MIN_VALUE 7) (long %))
                            (<= (long %) Integer/MAX_VALUE))
            "Float" number?
            "Character" char?
            "String" string?
            "Flag" (constantly true)
            (constantly false))]
    (fn [field]
      (->> field
           (keep-indexed
            (fn [i v]
              (when-not (f v)
                (let [id' (keyword id)]
                  (error [id' i]
                         "Wrong type of value: %s expects %s, but got %s"
                         id' type' (pr-str v))))))
           (apply merge-validation-results)))))

(defn- make-field-number-validator [ploidy {:keys [id number]}]
  (let [f (when (not= number 0)
            (case number
              "A" identity
              "R" inc
              "G" #(count (vcf.util/genotype-seq ploidy %))
              nil nil
              (constantly number)))]
    (fn [num-alts field]
      (let [expected (when f (f num-alts))
            actual (count field)]
        (when (and expected (not= expected actual))
          (let [id' (keyword id)]
            (error [id']
                   "Wrong number of values: %s expects %s value(s), but got %d value(s)"
                   id' expected actual)))))))

(defn- make-field-validator [ploidy field-def]
  (let [type-validator (make-field-type-validator field-def)
        number-validator (make-field-number-validator ploidy field-def)]
    (fn [num-alts field]
      (merge-validation-results
       (type-validator field)
       (number-validator num-alts field)))))

(defn- make-info-field-validator [ploidy info-field-def]
  (let [field-validator (make-field-validator ploidy info-field-def)]
    (fn [info-key variant]
      (let [info (get variant :info)]
        (when-let [[_ field] (find info info-key)]
          (let [field (if (and (some? field) (not (sequential? field)))
                        [field]
                        field)]
            (-> (field-validator (count (:alt variant)) field)
                (prepend-keys [:info]))))))))

(defn- prep-info-field-defs [{:keys [ploidy] :as _validator} info]
  (into {} (map (fn [info-field-def]
                  (let [f (make-info-field-validator ploidy info-field-def)]
                    [(keyword (:id info-field-def))
                     (assoc info-field-def :fn f)])))
        info))

(defn- validate-info-fields [validator variant]
  (->> (:info variant)
       keys
       (map (fn [info-key]
              (if-let [f (get-in validator [:defs :info info-key :fn])]
                (f info-key variant)
                (error [:info info-key]
                       "Info key %s not defined in meta info" info-key))))
       (apply merge-validation-results)))

(defn- make-gt-field-validator [ploidy gt-field-def]
  (let [field-validator (make-field-validator ploidy gt-field-def)]
    (fn [sample-name gt-key variant]
      (let [sample (get variant sample-name)]
        (when-let [[_ field] (find sample gt-key)]
          (let [field (if (and (some? field) (not (sequential? field)))
                        [field]
                        field)]
            (-> (field-validator (count (:alt variant)) field)
                (prepend-keys [sample-name]))))))))

(defn- prep-format-defs [{:keys [ploidy] :as _validator} fmt]
  (into {} (map (fn [gt-field-def]
                  (let [f (make-gt-field-validator ploidy gt-field-def)]
                    [(keyword (:id gt-field-def))
                     (assoc gt-field-def :fn f)])))
        fmt))

(defn- validate-format [validator {fmt :FORMAT}]
  (if (or (nil? fmt) (sequential? fmt))
    (->> (for [[i key'] (map-indexed vector fmt)]
           (if (keyword? key')
             (merge-validation-results
              (when (not (get-in validator [:defs :format key']))
                (error-on-bcf validator
                              [:FORMAT i]
                              "Genotype key %s not defined in meta info" key'))
              (when (and (= i 0) (not= key' :GT))
                (error [:FORMAT i]
                       (str "First genotype key must be :GT, but got " key'))))
             (error [:FORMAT i]
                    (str "Every genotype key in :FORMAT must be keyword, "
                         "but got " (pr-str key')))))
         (apply merge-validation-results))
    (error [:FORMAT]
           (str ":FORMAT must be a sequence of keywords or nil, but got "
                (pr-str fmt)))))

(defn- validate-samples [validator variant]
  (when (sequential? (:FORMAT variant))
    (let [fs (into {} (keep (fn [gt-key]
                              (when-let [f (get-in validator [:defs :format gt-key :fn])]
                                [gt-key f])))
                   (:FORMAT variant))]
      (->> (for [sample-name (:samples validator)
                 [gt-key f] fs]
             (f sample-name gt-key variant))
           (apply merge-validation-results)))))

(defn- validate-data-record [validator variant]
  (if (map? variant)
    (let [f (juxt validate-chrom
                  validate-pos
                  validate-ref
                  validate-alt
                  validate-qual
                  validate-filter
                  validate-info-fields
                  validate-format
                  validate-samples)]
      (apply merge-validation-results (f validator variant)))
    (error [] (str "Variant must be a map, but got " (pr-str variant)))))

(defn make-validator
  "Creates a vcf validator that is necessary for variant validation.
  Takes the following three arguments:
   - meta-info: VCF's meta info
   - header: VCF's header columns (including mandatory columns)
   - options: Validation options
  The available validation options are:
   - :file-type  Specify the file type (either of :vcf and :bcf).
                 Defaults to :vcf.
   - :ploidy     Specify the ploidy of sample genotypes"
  ([meta-info header] (make-validator meta-info header {}))
  ([{:keys [contig info]
     format' :format
     filter' :filter}
    header
    {:keys [file-type ploidy] :or {file-type :vcf, ploidy 2}}]
   (let [samples (into [] (drop 8) header)
         validator (map->VCFValidator
                    {:ploidy ploidy :file-type file-type :samples samples})]
     (assoc validator :defs
            {:contig (prep-contig-defs validator contig)
             :filter (prep-filter-defs validator filter')
             :info (prep-info-field-defs validator info)
             :format (prep-format-defs validator format')}))))

(defn validate-variant
  "Checks if the given variant data is in the format cljam expects, and returns
  a validation result map pointing out the problematic portion of data that does
  not conform to the format. Otherwise returns nil.
  The validation result map looks like:
   {:errors {[:chr] [\"...\"]
    :warnings {[:pos] [\"...\"]}
    :variant { ... variant data ...}}}
  "
  [validator variant]
  (let [res (validate-data-record validator variant)]
    (when (seq res)
      (assoc res :variant variant))))

(defn validate-variants
  "Applies `validation-variant` to each element of the given sequence and collects
  non-nil validation results into a lazy sequence.
  Returns a transducer if `variants` is not specified."
  ([validator]
   (keep (partial validate-variant validator)))
  ([validator variants]
   (sequence (validate-variants validator) variants)))

(defn- stringify-validation-result-messages [m]
  (with-out-str
    (doseq [[i [path msgs]] (map-indexed vector m)
            :let [path' (str path)
                  indent (apply str (repeat (+ (count path') 4) \space))]]
      (when (not= i 0) (newline))
      (printf " - %s: %s" path (first msgs))
      (doseq [msg (rest msgs)]
        (newline)
        (printf "%s %s" indent msg)))))

(defn check-variant
  "Checks if the given variant data is in the format cljam expects, and throws
  an error if it doesn't conform to the format. Otherwise returns the input variant
  data.
  Also, if any validation warning is found, it will be reported to stderr."
  [validator variant]
  (let [{:keys [warnings errors] v :variant :as res} (validate-variant validator variant)]
    (when warnings
      (binding [*out* *err*]
        (printf "Variant validation warning at %s\n%s"
                (pr-str (cond-> v
                          (map? v)
                          (select-keys [:chr :pos :id :ref :alt])))
                (stringify-validation-result-messages warnings))
        (newline)))
    (when errors
      (let [msg (format "Variant validation error at %s\n%s"
                        (pr-str (cond-> v
                                  (map? v)
                                  (select-keys [:chr :pos :id :ref :alt])))
                        (stringify-validation-result-messages errors))]
        (throw (ex-info msg res))))
    variant))

(defn check-variants
  "Applies `check-variant` to each element of the given sequence.
  Returns a lazy sequence of the same elements of the input if there are no
  invalid variant. The validation is evaluated lazily and throws an exception
  at the first invalid variant.
  Returns a transducer if `variants` is not specified."
  ([validator]
   (map (partial check-variant validator)))
  ([validator variants]
   (sequence (check-variants validator) variants)))
