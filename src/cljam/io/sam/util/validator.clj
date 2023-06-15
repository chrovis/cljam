(ns cljam.io.sam.util.validator)

(defn- validate-rname [rname refmap]
  (cond
    (not (string? rname)) ["Must be string."]
    (and (not (= rname "*"))
         (not (get refmap rname)))
    [(format "Must be not in header.(%s)" rname)]))

(defn- validate-pos [rname pos refmap]
  (let [max-len (get-in refmap [rname :LN])]
    (cond
      (not (integer? pos)) ["Must be integer."]
      (not (<= 0 (int pos) Integer/MAX_VALUE))
      ["Must be in the [0, 2147483647]."]

      (and max-len
           (> (int pos) (int max-len)))
      [(format "Must be less than or equal %d." max-len)])))

(defn- validate-qname [qname]
  (if (not (string? qname))
    ["Must be string."]
    (cond-> nil
      (not (<= (count qname) 254))
      (conj "Must be less than or equal to 254 characters.")

      (not (re-matches #"^[!-?A-~]+$" qname))
      (conj "Must not contain illegal characters."))))

(defn- validate-mapq [mapq]
  (cond
    (not (integer? mapq)) ["Must be integer."]
    (not (<= 0 (int mapq) 255))
    ["Must be in the [0-255]."]))

(defn- validate-cigar [cigar]
  (cond
    (not (string? cigar)) ["Must be string."]
    (not (re-matches #"^\*|([0-9]+[MIDNSHPX=])+$" cigar))
    ["Invalid format."]))

(defn- validate-tlen [tlen]
  (cond
    (not (integer? tlen)) ["Must be integer."]
    (not (<= (- Integer/MAX_VALUE) tlen Integer/MAX_VALUE))
    ["Must be in the [-2147483647,2147483647]."]))

(defn- validate-qual [qual]
  (cond
    (not (string? qual)) ["Must be string."]
    (not (re-matches #"[!-~]+" qual)) ["Must not contain bad character."]))

(defn- validate-seq [seq]
  (cond
    (not (string? seq)) ["Must be string."]
    (not (re-matches #"\*|[A-Za-z=.]+" seq)) ["Must not contain bad character."]))

(defn- validate-option [{:keys [type value]}]
  (case type
    "A" (when-not (and (char? value) (<= (int \!) (int value) (int \~)))
          ["Must be char [!-~]."])
    "i" (when-not (and (integer? value) (<= -32767 value 32767))
          ["Must be 16 bit signed integer."])
    "f" (when-not (or (float? value) (integer? value))
          ["Must be float."])
    "Z" (when-not (and (string? value) (re-matches #"[ !-~]*" value))
          ["Must be printing string [ !-~]*"])
    "H" (when-not (and (sequential? value)
                       (every? (every-pred integer?  #(<= -255 (int %) 255))
                               value))
          ["Must be byte array."])
    "B" (when-not (and (string? value)
                       (re-matches #"[cCsSiIf](,[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*"
                                   value))
          ["Must be Integer or numeric array string."])
    [(format "Type %s is invalid" (str type))]))

(defn- validate-options [options]
  (reduce
   (fn [res option]
     (let [k (first (keys option))]
       (if-let [checked (validate-option (get option k))]
         (assoc res k checked)
         res)))
   nil
   options))

(defn- make-validator* [header]
  (let [refmap (into {} (map (juxt :SN identity) (:SQ header)))]
    (fn [alignment]
      (let [checked-rname (validate-rname (:rname alignment) refmap)
            checked-pos (validate-pos
                         (:rname alignment) (:pos alignment) refmap)
            checked-qname (validate-qname (:qname alignment))
            checked-rnext (validate-rname (:rname alignment) refmap)
            checked-pnext (validate-pos (:rname alignment) (:pnext alignment)
                                        refmap)
            checked-mapq (validate-mapq (:mapq alignment))
            checked-cigar (validate-cigar (:cigar alignment))
            checked-tlen (validate-tlen (:tlen alignment))
            checked-qual (validate-qual (:qual alignment))
            checked-seq (validate-seq (:seq alignment))
            checked-options (validate-options (:options alignment))]
        (merge
         (cond-> nil
           checked-qname (assoc :qname checked-qname)
           checked-rname (assoc :rname checked-rname)
           checked-pos (assoc :pos checked-pos)
           checked-rnext (assoc :rnext checked-rnext)
           checked-mapq (assoc :mapq checked-mapq)
           checked-cigar (assoc :cigar checked-cigar)
           checked-pnext (assoc :pnext checked-pnext)
           checked-tlen (assoc :tlen checked-tlen)
           checked-qual (assoc :qual checked-qual)
           checked-seq (assoc :seq checked-seq))
         checked-options)))))

(defn make-validator
  "Variant and return returns a map that explains bad positions."
  [header]
  (let [validator (make-validator* header)]
    (fn [alignments]
      (map (fn [a]
             (when-let [info (validator a)]
               (throw (ex-info "Invalid alignment."
                               (assoc info :alignment a))))
             a)
           alignments))))

(defn validate-alignments
  "Takes a list of alignments and throws an illegal alignment
   if it contains illegal content.
   If there is no illegal alignment, the input list is returned as-is.
   Validation is delayed."
  [header alignments]
  (let [validator (make-validator* header)]
    (map (fn [a]
           (when-let [info (validator a)]
             (throw (ex-info "Invalid alignment."
                             (assoc info :alignment a))))
           a)
         alignments)))
