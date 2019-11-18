(ns cljam.io.vcf.reader
  "A type of VCF reader and internal functions to read VCF contents. See
  https://samtools.github.io/hts-specs/ for the detail VCF specifications."
  (:require [clojure.string :as cstr]
            [clojure.tools.logging :as logging]
            [cljam.io.protocols :as protocols]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [proton.core :refer [as-long]]
            [cljam.io.util.bin :as util-bin]
            [cljam.io.vcf.util :as vcf-util])
  (:import [java.io Closeable BufferedReader]
           [clojure.lang LazilyPersistentVector]
           bgzf4j.BGZFInputStream))

;; VCFReader
;; ---------

(declare read-variants read-variants-randomly)

(deftype VCFReader [url meta-info header reader index-delay]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (read-variants this))
  (read [this option] (read-variants this option))
  (indexed? [_] false)
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this {:keys [chr start end]} option]
    (logging/warn "May cause degradation of performance.")
    (filter
     (fn [v] (and (if chr (= (:chr v) chr) true)
                  (if start (<= start (:pos v)) true)
                  (if end (<= (+ (:pos v) (count (:ref v))) end) true)))
     (read-variants this option))))

;; need dynamic extension for namespace issue.
(extend-type VCFReader
  protocols/IVariantReader
  (meta-info [this] (.meta-info this))
  (header [this] (.header this))
  (read-variants
    ([this] (protocols/read-variants this {}))
    ([this option] (read-variants this option))))

;; Utilities
;; ---------

(defn- dot->nil
  [^String s]
  ;; Avoid calling equiv on strings.
  (if (and (= 1 (.length s)) (= \. (.charAt s 0))) nil s))

;; Loading meta-information
;; ------------------------

(defn- meta-line?
  [line]
  (= (subs line 0 2) "##"))

(defn- parse-structured-line
  "e.g. (parse-structured-line \"ID=NS,Number=1,Type=Integer,Description=\\\"Number of Samples With Data\\\"\")
        => {:id \"NS\" :number \"1\" :type \"Integer\"
            :description \"Number of Samples With Data\"}"
  [s]
  (letfn [(parse-string-field [s]
            (loop [s s, acc []]
              (if-let [[_ pre c post] (re-matches #"([^\\\"]*?)([\\\"])(.*)" s)]
                (if (= c "\\")
                  ;; expecting one of the special characters occurs immediately after \\
                  (if (#{\\ \"} (first post))
                    (recur (subs post 1) (conj acc pre (first post)))
                    (let [msg (str "Either '\\' or '\"' was expected immediately after '\\', "
                                   "but got '" (first post) "'")]
                      (throw (RuntimeException. msg))))
                  ;; c == \", that is, we are now at the end of the string field
                  [nil (cstr/join (conj acc pre)) (cstr/replace post #"^," "")])
                (throw (RuntimeException. "Unexpected end of string field")))))]
    (loop [s s, m {}]
      (if-let [[_ k more] (some->> s (re-matches #"([^=]+?)=(.*)"))]
        (let [[_ v more] (if (= (first more) \")
                           (parse-string-field (subs more 1))
                           (re-matches #"([^,]*?)(?:,(.*))?" more))]
          (recur more (assoc m (->kebab-case-keyword k) (dot->nil v))))
        m))))

(defn- parse-meta-info-contig
  [m]
  (update m :length as-long))

(defn- parse-meta-info-info
  [m]
  (update m :number (fn [s]
                      (if (#{"A" "R" "G"} s)
                        s
                        (as-long s)))))

(defn parse-meta-info-line
  [line]
  (let [[_ k* v] (re-find #"^##([\w:/\.\?\-]*)=(.*)$" line)
        k (->kebab-case-keyword k*)]
    [k (if-let [[_ s] (re-find #"^<(.+)>$" v)]
         (cond-> (parse-structured-line s)
           (#{:info :format} k) parse-meta-info-info
           (= k :contig) parse-meta-info-contig)
         v)]))

(defn load-meta-info
  [^BufferedReader rdr]
  (loop [line (.readLine rdr), meta-info {}]
    (if (meta-line? line)
      (let [[k v] (parse-meta-info-line line)]
        (recur (.readLine rdr)
               (if (#{:contig :info :filter :format :alt :sample :pedigree} k)
                 (if (get meta-info k)
                   (update meta-info k conj v)
                   (assoc meta-info k [v]))
                 (assoc meta-info k v))))
      meta-info)))

;; Loading header
;; --------------

(defn- header-line?
  [line]
  (not (nil? (re-find #"^#[^#]*$" line))))

(defn parse-header-line
  [line]
  (cstr/split (subs line 1) #"\t"))

(defn load-header
  [^BufferedReader rdr]
  (loop [line (.readLine rdr)]
    (if (header-line? line)
      (parse-header-line line)
      (recur (.readLine rdr)))))

;; Reading data lines
;; ------------------

(defn- parse-data-line
  [line kws]
  ;; When splitting a string with single-character delimiter,
  ;; `java.lang.String#split` is slightly faster than `clojure.string/split`.
  ;; For more details, please refer to https://github.com/chrovis/cljam/pull/29.
  (let [[fields gt-fields] (->> (.split ^String line "\t")
                                LazilyPersistentVector/createOwning
                                (map dot->nil)
                                (split-at 8))]
    (->> gt-fields
         (interleave kws)
         (concat [:chr (first fields)
                  :pos (as-long (nth fields 1))
                  :id (nth fields 2)
                  :ref (nth fields 3)
                  :alt (some-> (nth fields 4) (cstr/split #","))
                  :qual (some-> (nth fields 5) Double/parseDouble)
                  :filter (nth fields 6)
                  :info (nth fields 7)])
         (apply hash-map))))

(defn- read-data-lines
  [rdr header kws]
  (when-let [line (if (instance? BufferedReader rdr)
                    (.readLine ^BufferedReader rdr)
                    (.readLine ^BGZFInputStream rdr))]
    (if-not (or (meta-line? line) (header-line? line))
      (cons (parse-data-line line kws)
            (lazy-seq (read-data-lines rdr header kws)))
      (read-data-lines rdr header kws))))

(defn read-variants
  ([rdr]
   (read-variants rdr {}))
  ([^VCFReader rdr {:keys [depth] :or {depth :deep}}]
   (let [kws (mapv keyword (drop 8 (.header rdr)))
         parse-fn (case depth
                    :deep (vcf-util/variant-parser (.meta-info rdr) (.header rdr))
                    :vcf identity)]
     (map parse-fn (read-data-lines (.reader rdr) (.header rdr) kws)))))

(defn- make-lazy-variants [f s]
  (when-first [fs s]
    (lazy-cat
     (f fs)
     (make-lazy-variants f (rest s)))))

(defn read-variants-randomly
  "Reads variants of the bgzip compressed VCF file randomly using tabix file.
   Returning them as a lazy sequence."
  [^VCFReader rdr
   {:keys [chr start end] :or {start 1 end 4294967296}}
   {:keys [depth] :or {depth :deep}}]
  (let [kws (mapv keyword (drop 8 (.header rdr)))
        tabix-data @(.index-delay rdr)
        chr-names (->> (.meta-info rdr) :contig (mapv :id))
        ref-idx (.indexOf ^clojure.lang.PersistentVector chr-names chr)
        spans (when-not (neg? ref-idx)
                (util-bin/get-spans tabix-data ref-idx start end))
        input-stream ^BGZFInputStream (.reader rdr)
        parse-fn (case depth
                   :deep (vcf-util/variant-parser (.meta-info rdr) (.header rdr))
                   :vcf identity)]
    (make-lazy-variants
     (fn [[chunk-beg ^long chunk-end]]
       (.seek input-stream chunk-beg)
       (->> #(when (< (.getFilePointer input-stream) chunk-end)
               (-> input-stream
                   .readLine
                   (parse-data-line kws)
                   parse-fn))
            repeatedly
            (take-while identity)
            (filter
             (fn [{chr' :chr :keys [pos ref info]}]
               (and (= chr' chr)
                    (<= pos end)
                    (<= start (get info :END (dec (+ pos (count ref))))))))))
     spans)))
