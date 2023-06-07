(ns cljam.io.vcf.util.validator
  (:require [cljam.io.vcf.util :as vcf-util]
            [clojure.string :as cstr]))

(defn- valid-ref? [s]
  (and (string? s)
       (seq s)
       (every? #{\A \T \G \C \N
                 \a \t \g \c \n} s)))

(defn- valid-qual? [v] (or (nil? v)
                           (float? v)
                           (integer? v)))

(defn- valid-filter? [filt]
  (or (nil? filt)
      (= filt :PASS)
      (and (sequential? filt)
           (every? keyword? filt))))

(defn- validate-alleles [alt vref]
  (when (sequential? alt)
    (keep-indexed
     #(cond
        (and (not (string? %2)) (not (nil? %2)))
        (format "An allele must be string but got other type at index %d."
                %1)

        (= %2 "")
        (format "An allele cannot be empty but got an empty allele at index %d."
                %1)

        (= :other (:type (vcf-util/inspect-allele vref %2)))
        (format "Contains bad allele at index %d." %1))
     alt)))

(defn- check-base-records [vcf-or-bcf variant meta-included-contig?
                           meta-included-filter?  meta-included-format?]
  (reduce-kv
   (fn [m k vs]
     (reduce
      (fn [m [pred msg]]
        (cond-> m (not (pred (k variant))) (update k (fnil conj []) msg)))
      m
      vs))
   nil
   {:chr
    [[(complement (partial some #{\< \> \, \space \tab}))
      (format "Must not contain whitespaces commas or angle brackets, but got %s."
              (str (vec (distinct (keep #{\< \> \, \space \tab} (:chr variant))))))]
     [#(or (= vcf-or-bcf :vcf)
           (meta-included-contig? %))
      (format (str "CHROM %s must be declared in a "
                   "##contig field in the header.")
              (:chr variant))]],
    :pos [[integer? "Position must be Integer."]],
    :ref [[valid-ref? "Must consist of ACGTNacgtn."]]
    :alt [[(every-pred seq sequential?) "Must be a sequence."]
          [#(not (seq (validate-alleles  % (:ref variant))))
           (cstr/join ", " (validate-alleles (:alt variant) (:ref variant)))]]
    :qual [[valid-qual? "Qual must be float."]]
    :filter [[valid-filter? "Invalid filter."]
             [#(or (= vcf-or-bcf :vcf)
                   (every? meta-included-filter? %))
              (and (sequential? (:filter variant))
                   (format "Filter identifiers %s must in meta-info."
                           (vec (remove meta-included-filter?
                                        (:filter variant)))))]]
    :format [[meta-included-format? "Must be contain meta"]]}))

(defn- check-entry-type [entry type-str]
  ((case type-str
     "Integer" integer?
     "Float" #(or (float? %) (integer? %))
     "Character" char?
     "String" string?
     "Flag" (constantly true)
     (constantly false))
   entry))

(defn- check-entry [entry {:keys [type number]} ^long alt-num]
  (let [entries (if (and (= number 1) (not (sequential? entry)))
                  [entry] entry)
        type-check (and entry
                        (not (every? #(check-entry-type
                                       % type) entries)))
        number' (case number
                  "A" alt-num
                  "R" (inc alt-num)
                  "G" (count (vcf-util/genotype-seq 2 alt-num))
                  number)
        number-check (not (or (nil? entry)
                              (= (count entries) number')))]
    (when (or type-check number-check)
      (cond-> nil
        type-check
        (conj
         (format
          "Not match type declaration. Requires %s, but got %s."
          type (str entries)))
        number-check
        (conj
         (format "Invalid number of elements. Requires %s, but got %d."
                 (str number) (count entries)))))))

(defn- check-each-samples [variant samples mformat]
  (reduce
   (fn [res [ks v]] (assoc-in res ks v))
   nil
   (for [sample samples
         [id entry] (get variant sample)
         :let [fmt (get mformat id)
               res (if fmt
                     (check-entry entry fmt
                                  (count (:alt variant)))
                     [(format "Key %s not in meta." id)])]
         :when res]
     [[sample id] res])))

(defn- make-contig-validator [contig]
  (let [contigs (into #{} (map :id) contig)]
    #(or (nil? %) (contigs %))))

(defn- make-header-contained-filter-validator [filter]
  (let [filter-ids (into #{} (map (comp keyword :id)) filter)]
    #(or (nil? %) (= % :PASS) (filter-ids %))))

(defn- make-format-validator [format]
  #(or (nil? %) ((set (map (comp keyword :id) format)) %)))

(defn- make-info-validator [meta-info]
  (let [id->info (into {} (map (juxt (comp keyword :id) identity)) meta-info)]
    (fn [info alt-num]
      (reduce-kv
       (fn [validated id entry]
         (if-let [info (get id->info id)]
           (if-let [res (check-entry entry info alt-num)]
             (assoc validated id res)
             validated)
           (assoc validated
                  id
                  [(format (str "Key %s is not contained in ##info fields in "
                                "the header.") id)])))
       nil
       info))))

(defn make-validator
  "Make vcf validator for writing.
  Returns a map showing what went wrong if you passed the wrong variant to the validator.
  The first argument is vcf meta-info and the second argument is vcf-header.
  The third argument is an option containing :vcf or :bcf filetype"
  [{:keys [contig format info filter]} header {:keys [filetype] :or {filetype :vcf}}]
  (let [meta-included-contig? (make-contig-validator contig)
        meta-included-filter? (make-header-contained-filter-validator filter)
        meta-included? (make-format-validator format)
        info-validator (make-info-validator info)
        samples (drop 8 header)]
    (fn [variant]
      (merge (check-base-records filetype variant meta-included-contig?
                                 meta-included-filter?  meta-included?)
             (info-validator (:info variant) (count (:alt variant)))
             (check-each-samples variant samples format)))))

(defn validate-variant
  "Find bad expression in the variant and returns a map that explains bad
  positions."
  ([validator variant] (validator variant)))

(defn validate-variants
  "Find bad expression in varints and return sequence of the map that
  explains bad positions"
  ([validator variants] (seq (keep validator variants))))

(defn check-variant
  "Checks if there is any invalid variant in the given sequence `variants`.
  Throw an exception if variant has bad expression, othrwise returns a variant
  passed as argument."
  [validator variant]
  (if-let [info (validator variant)]
    (throw (ex-info (str "VCF validatoin failed: " (keys info))
                    (assoc info :variant variant)))
    variant))

(defn check-variants
  "Checks if there is any invalid variant in the given sequence `variants`.
  Returns a lazy sequence of the same elements of the input if there are no
  invalid variant. The validation is evaluated lazily and throws an exception
  at the first invalid variant."
  ([validator variants]
   (map (fn [v]
          (when-let [info (validator v)]
            (throw (ex-info (str "VCF validatoin failed: " (keys info))
                            (assoc info :variant v))))
          v) variants)))
