(ns cljam.io.vcf.util
  (:require [clojure.string :as cstr]))

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
  "Produces a sequence of genotypes represented as sequences of integers."
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
  (letfn [(c [^long n ^long k]
            (cond
              (< n k) 0
              (or (zero? k) (= k n)) 1
              (or (= k 1) (= k (dec n))) n
              (< (quot n 2) k) (recur n (- n k))
              :else (loop [i (inc (- n k)) j 1 r 1]
                      (if (<= j k)
                        (recur (inc i) (inc j) (quot (* r i) j))
                        r))))]
    (->> genotype
         sort
         (map-indexed (fn [^long m ^long k] (c (+ k m) (inc m))))
         (apply +))))

(defn biallelic-genotype
  "Converts a multiallelic `genotype` string into a biallelic one. Ignores all
  alleles other than the reference allele and the `target-allele`."
  [genotype ^long target-allele]
  (->> genotype
       parse-genotype
       (map (fn [[allele phased?]]
              [(if (nil? allele) nil (if (= target-allele allele) 1 0))
               phased?]))
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
       (drop-while (fn [[_ v]] (or (nil? v) (= [nil] v))))
       (map (fn [[_ v]]
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

(defmacro ^:private ->breakend [^String alt p1 p2 t1 t2 strand join]
  `(let [p1# ~p1
         p2# ~p2
         t1# ~t1
         t2# ~t2
         k# (.lastIndexOf ~alt (int 58) p2#)
         k'# (inc k#)] ;; \:
     (when (and (< p1# k#) (< k'# p2#) (< t1# t2#))
       (when-let [pos# (try
                         (Long/parseLong (.substring ~alt k'# p2#))
                         (catch NumberFormatException _# nil))]
         {:chr (.substring ~alt p1# k#),
          :pos pos#,
          :bases (.substring ~alt t1# t2#),
          :strand ~strand,
          :join ~join}))))

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
  [^String alt]
  (let [left-bracket (int 91) ;; \[
        right-bracket (int 93) ;; \]
        l1 (.indexOf alt left-bracket)
        r1 (.indexOf alt right-bracket)]
    (if-not (neg? l1)
      (let [l2 (.indexOf alt left-bracket (inc l1))
            l2' (inc l2)]
        (when-not (or (neg? l2)
                      (not (neg? (.indexOf alt left-bracket l2')))
                      (not (neg? r1)))
          (if (zero? l1)
            (->breakend alt 1 l2 l2' (.length alt) :reverse :before)
            (->breakend alt (inc l1) l2 0 l1 :forward :after))))
      (if-not (neg? r1)
        (let [r2 (.indexOf alt right-bracket (inc r1))
              r2' (inc r2)]
          (when-not (or (neg? r2)
                        (not (neg? (.indexOf alt right-bracket r2'))))
            (if (zero? r1)
              (->breakend alt 1 r2 r2' (.length alt) :forward :before)
              (->breakend alt (inc r1) r2 0 r1 :reverse :after))))
        (let [dot (int 46) ;; \.
              d (.indexOf alt dot)] ;; single breakends
          (when-not (or (neg? d)
                        (= 1 (.length alt))
                        (not (neg? (.indexOf alt dot (inc d)))))
            (if (zero? d)
              {:bases (.substring alt 1), :join :before}
              {:bases (.substring alt 0 d), :join :after})))))))

(defn- parse-alt'
  [^String ref ^String alt]
  (let [r-len (.length ref)
        a-len (.length alt)
        eq (fn [^long r ^long a]
             (= (Character/toUpperCase (.charAt ref r))
                (Character/toUpperCase (.charAt alt a))))
        [^long r ^long a] (loop [r 0 a 0]
                            (if (and (< r r-len) (< a a-len) (eq r a))
                              (recur (inc r) (inc a))
                              [r a]))]
    (if (and (< a a-len) (= r r-len))
      {:type :insertion, :n-bases (- a-len r),
       :offset (dec a), :inserted (.substring alt a)}
      (if (and (= a a-len) (< r r-len))
        {:type :deletion, :n-bases (- a r-len),
         :offset (dec r), :deleted (.substring ref r)}
        (if (= r r-len a a-len)
          {:type :ref}
          (let [[^long re ^long ae] (loop [re (dec r-len)
                                           ae (dec a-len)]
                                      (if (and (< r re) (< a ae) (eq re ae))
                                        (recur (dec re) (dec ae))
                                        [re ae]))]
            (if (= ae a)
              (if (= re r)
                {:type :snp, :ref (.charAt ref a),
                 :alt (.charAt alt a), :offset a}
                (let [n (- r re)]
                  (if (eq re ae)
                    {:type :deletion, :n-bases n,
                     :offset (dec r), :deleted (.substring ref r re)}
                    {:type :other, :n-bases n})))
              (if (= re r)
                (let [n (- ae a)]
                  (if (eq re ae)
                    {:type :insertion, :n-bases n,
                     :offset (dec a), :inserted (.substring alt a ae)}
                    {:type :other, :n-bases n}))
                (let [n (if (< (- ae a) (- re r))
                          (- r re 1)
                          (- ae a -1))]
                  (if (= (- re r) (- ae a))
                    {:type :mnp, :ref (.substring ref r (inc re)),
                     :alt (.substring alt a (inc ae)), :offset a}
                    {:type :other, :n-bases n}))))))))))

(defn parse-alt
  "Parses an alt allele string and returns a map containing its details."
  [^String ref ^String alt]
  (let [r-len (.length ref)
        a-len (.length alt)
        fa (.charAt alt (int 0))]
    (if (= 1 r-len a-len)
      (if (or (= fa \.)
              (= fa \*)
              (= fa \X)
              (= (Character/toUpperCase fa)
                 (Character/toUpperCase (.charAt ref (int 0)))))
        {:type :ref}
        {:type :snp, :ref (.charAt ref (int 0)), :alt fa, :offset 0})
      (if (= fa \<)
        (let [la (.charAt alt (dec a-len))]
          (if (= la \>)
            (if (and (= 3 a-len)
                     (or (= (.charAt alt (int 1)) \*)   ;; <*>
                         (= (.charAt alt (int 1)) \X))) ;; <X>
              {:type :ref}
              {:type :id, :id (.substring alt (int 1) (dec a-len))})
            (if (and (<= 4 a-len) ;; <id>N, SVTYPE=INS
                     (= (.charAt alt (dec (dec a-len))) \>))
              {:type :complete-insertion, :join :before, :base la,
               :id (.substring alt 1 (dec (dec a-len)))}
              {:type :other})))
        (let [la (.charAt alt (dec a-len))]
          (if (and (<= 4 a-len) ;; N<id>, SVTYPE=INS
                   (= la \>)
                   (= \< (.charAt alt (int 1))))
            {:type :complete-insertion, :join :after, :base fa,
             :id (.substring alt 2 (dec a-len))}
            (if-let [bnd (parse-breakend alt)]
              (assoc bnd :type :breakend)
              (parse-alt' ref alt))))))))
