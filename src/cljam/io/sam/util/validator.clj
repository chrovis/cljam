(ns cljam.io.sam.util.validator)

(defn- error [path msg & args]
  {:errors {path [(apply format msg args)]}})

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

(defn- validate-pos* [rname pos refmap]
  (let [max-len (get-in refmap [rname :LN])]
    (cond
      (not (integer? pos)) ["Must be an integer."]
      (not (<= 0 (int pos) Integer/MAX_VALUE))
      ["Must be in the [0, 2147483647]."]

      (and max-len
           (> (int pos) (int max-len)))
      [(format "Must be less than or equal to %d." max-len)])))

(defn- validate-pos [{:keys [refmap]} {:keys [pos rname]}]
  (when-let [err (validate-pos* rname pos refmap)]
    (apply error :pos err)))

(defn- validate-pnext [{:keys [refmap]} {:keys [pnext rname]}]
  (when-let [err (validate-pos* rname pnext refmap)]
    (apply error :pnext err)))

(defn- validate-rname* [rname refmap]
  (cond
    (not (string? rname)) ["Must be a string."]
    (and (not (= rname "*"))
         (not (get refmap rname)))
    [(format (str "Must be declared as the SN value in the SQ line within the"
                  " header. (%s)") rname)]))

(defn- validate-rname [{:keys [refmap]} {:keys [rname]}]
  (when-let [err (validate-rname* rname refmap)]
    (apply error :rname err)))

(defn- validate-rnext [{:keys [refmap]} {:keys [rnext]}]
  (when-let [err (validate-rname* rnext refmap)]
    (error :rnext err)))

(defn- validate-qname [_ {:keys [qname]}]
  (if (not (string? qname))
    (error :qname "Must be a string.")
    (when-let [res
               (cond-> nil
                 (not (<= (count qname) 254))
                 (conj "Must be less than or equal to 254 characters.")

                 (not (re-matches #"^[!-?A-~]+$" qname))
                 (conj "Must not contain illegal characters."))]
      {:errors {:qname res}})))

(defn- validate-mapq [_ {:keys [mapq]}]
  (cond
    (not (integer? mapq)) (error :mapq "Must be an integer.")
    (not (<= 0 (int mapq) 255))
    (error :mapq "Must be in the [0-255].")))

(defn- validate-cigar [_ {:keys [cigar]}]
  (cond
    (not (string? cigar)) (error :cigar "Must be a string.")
    (not (re-matches #"^\*|([0-9]+[MIDNSHPX=])+$" cigar))
    (error :cigar "Invalid format.")))

(defn- validate-tlen [_ {:keys [tlen]}]
  (cond
    (not (integer? tlen)) (error :tlen "Must be integer.")
    (not (<= (- Integer/MAX_VALUE) tlen Integer/MAX_VALUE))
    (error :tlen "Must be in the [-2147483647,2147483647].")))

(defn- validate-qual [_ {:keys [qual]}]
  (cond
    (not (string? qual)) (error :qual "Must be a string.")
    (not (re-matches #"[!-~]+" qual)) (error :qual "Must be composed only of ASCII characters within the valid phred33 range [!-~].")))

(defn- validate-seq [{:keys [file-type]} {seq' :seq}]
  (cond
    (not (string? seq')) (error :seq "Must be a string.")
    (and (= file-type :bam) (not (re-matches #"\*|[=ACMGRSVTWYHKDBN]+" seq'))) (error :seq "Must not contain bad character.")
    (and (= file-type :sam) (not (re-matches #"\*|[A-Za-z=.]+" seq'))) (error :seq "Must not contain bad character.")))

(defn- validate-option [{:keys [value] type' :type}]
  (case type'
    "A" (when-not (and (char? value) (<= (int \!) (int value) (int \~)))
          ["Must be a char [!-~]."])
    "i" (when-not (and (integer? value) (<= -2147483648 value 2147483647))
          ["Must be 32 bit signed integer."])
    "f" (when-not (or (float? value) (integer? value))
          ["Must be a float."])
    "Z" (when-not (and (string? value) (re-matches #"[ !-~]*" value))
          ["Must be a printable string [ !-~]*"])
    "H" (when-not (and (sequential? value)
                       (every? (every-pred integer?  #(<= -255 (int %) 255))
                               value))
          ["Must be a byte array."])
    "B" (when-not (and (string? value)
                       (re-matches #"[cCsSiIf](,[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*"
                                   value))
          ["Must be a string of comma-separated array of numbers."])
    [(format "Type %s is invalid" (str type'))]))

(defn- validate-options [_ {:keys [options]}]
  (map-indexed #(when-let [err (validate-option %2)]
                  (apply error [:options %1] err))
               options))

(defn- validate-data-record [validator alignment]
  (if (map? alignment)
    (let [f (juxt validate-qname
                  validate-rname
                  validate-rnext
                  validate-pos
                  validate-pnext
                  validate-mapq
                  validate-cigar
                  validate-qual
                  validate-tlen
                  validate-seq)]
      (apply merge-validation-results
             (concat (f validator alignment)
                     (validate-options validator alignment))))
    (error [] (str "Alignment must be a map, but got " (pr-str alignment)))))

(defn make-validator
  ([header] (make-validator header {}))
  ([header {:keys [file-type] :or {file-type :sam}}]
   {:file-type file-type
    :refmap (into {} (map (juxt :SN identity) (:SQ header)))}))

(defn validate-alignment
  [validator alignment]
  (let [res (validate-data-record validator alignment)]
    (when (seq res)
      (assoc res :alignment alignment))))

(defn validate-alignments
  ([validator]
   (keep (partial validate-alignment validator)))
  ([validator alignments]
   (sequence (validate-alignments validator) alignments)))

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

(defn check-alignment
  [validator alignment]
  (let [{:keys [warnings errors] v :alignment :as res} (validate-alignment validator alignment)]
    (when warnings
      (binding [*out* *err*]
        (printf "Alignment validation warning at %s\n%s"
                (pr-str (cond-> v
                          (map? v)
                          (select-keys [:qname :rname :rnext :pos :pnext :mapq :cigar :qual :tlen :seq])))
                (stringify-validation-result-messages warnings))
        (newline)))
    (when errors
      (let [msg (format "Alignment validation error at %s\n%s"
                        (pr-str (cond-> v
                                  (map? v)
                                  (select-keys [:qname :rname :rnext :pos :pnext :mapq :cigar :qual :tlen :seq])))
                        (stringify-validation-result-messages errors))]
        (throw (ex-info msg res))))
    alignment))

(defn check-alignments
  ([validator]
   (map (partial check-alignment validator)))
  ([validator alignments]
   (sequence (check-alignments validator) alignments)))
