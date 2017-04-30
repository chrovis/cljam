(ns cljam.vcf.reader
  "A type of VCF reader and internal functions to read VCF contents. See
  https://samtools.github.io/hts-specs/ for the detail VCF specifications."
  (:require [clojure.string :as cstr]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [cljam.util :refer [str->long]]
            [cljam.util.vcf-util :as vcf-util])
  (:import [clojure.lang LazilyPersistentVector]))

;; VCFReader
;; ---------

(deftype VCFReader [f meta-info header reader]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

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
  "e.g. (parse-structured-line \"ID=NS,Number=1,Type=Integer,Description=\"Number of Samples With Data\"\")
        => {:id \"NS\" :number \"1\" :type \"Integer\"
            :description \"Number of Samples With Data\"}"
  [s]
  (->> (re-seq #"([\w:/\.\?\-]*)=((?:\"(.*)\")|([\w:/\.\?\-]*))" s)
       (map (fn [[_ k v1 v2 _]]
              {(->kebab-case-keyword k) (dot->nil (or v2 v1))}))
       (apply merge)))

(defn- parse-meta-info-contig
  [m]
  (update m :length str->long))

(defn- parse-meta-info-info
  [m]
  (update m :number (fn [s]
                      (if (#{"A" "R" "G"} s)
                        s
                        (str->long s)))))

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
  [^java.io.BufferedReader rdr]
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
  [^java.io.BufferedReader rdr]
  (loop [line (.readLine rdr)]
    (if (header-line? line)
      (parse-header-line line)
      (recur (.readLine rdr)))))

;; Reading data lines
;; ------------------

(defn- parse-data-line
  [line header kws]
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
                  :pos (str->long (nth fields 1))
                  :id (nth fields 2)
                  :ref (nth fields 3)
                  :alt (some-> (nth fields 4) (cstr/split #","))
                  :qual (some-> (nth fields 5) Double/parseDouble)
                  :filter (nth fields 6)
                  :info (nth fields 7)])
         (apply hash-map))))

(defn- read-data-lines
  [^java.io.BufferedReader rdr header kws]
  (when-let [line (.readLine rdr)]
    (if-not (or (meta-line? line) (header-line? line))
      (cons (parse-data-line line header kws)
            (lazy-seq (read-data-lines rdr header kws)))
      (read-data-lines rdr header kws))))

(defn read-variants
  [^VCFReader rdr & {:keys [depth] :or {depth :deep}}]
  (let [kws (mapv keyword (drop 8 (.header rdr)))
        parse-fn (case depth
                   :deep (vcf-util/variant-parser (.meta-info rdr) (.header rdr))
                   :vcf identity)]
    (map parse-fn (read-data-lines (.reader rdr) (.header rdr) kws))))
