(ns cljam.vcf.reader
  "A type of VCF reader and internal functions to read VCF contents. See
  https://samtools.github.io/hts-specs/ for the detail VCF specifications."
  (:require [clojure.string :as cstr]
            [cljam.util :refer [str->long]]))

;; VCFReader
;; ---------

(deftype VCFReader [f meta-info header reader]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

;; Utilities
;; ---------

(defn- dot->nil
  [s]
  (if (= s ".") nil s))

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
              {(keyword (cstr/lower-case k)) (dot->nil (or v2 v1))}))
       (apply merge)))

(defn- parse-meta-info-info
  [s]
  (update (parse-structured-line s) :number str->long))

(defn- parse-meta-info-line
  [line]
  (let [[_ k* v] (re-find #"^##([\w:/\.\?\-]*)=(.*)$" line)
        k (keyword (cstr/lower-case k*))]
    [k (if-let [[_ s] (re-find #"^<(.+)>$" v)]
         (if (#{:info :format} k)
           (parse-meta-info-info s)
           (parse-structured-line s))
         v)]))

(defn load-meta-info
  [^java.io.BufferedReader rdr]
  (loop [line (.readLine rdr), meta-info {}]
    (if (meta-line? line)
      (let [[k v] (parse-meta-info-line line)]
        (recur (.readLine rdr)
               (if (#{:info :filter :format :alt :sample :pedigree} k)
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

(defn- parse-header-line
  [line]
  (->> (cstr/split (subs line 1) #"\t")
       (mapv cstr/lower-case)))

(defn load-header
  [^java.io.BufferedReader rdr]
  (loop [line (.readLine rdr)]
    (if (header-line? line)
      (parse-header-line line)
      (recur (.readLine rdr)))))

;; Reading data lines
;; ------------------

(defn- parse-data-line
  [line header]
  (let [[fields gt-fields] (->> (cstr/split line #"\t")
                                (map dot->nil)
                                (split-at 8))]
    (merge {:chrom (first fields)
            :pos (str->long (nth fields 1))
            :id (nth fields 2)
            :ref (nth fields 3)
            :alt (nth fields 4)
            :qual (some-> (nth fields 5) Double/parseDouble)
            :filter (nth fields 6)
            :info (nth fields 7)}
           (zipmap (map keyword (drop 8 header)) gt-fields))))

(defn- read-data-lines
  [^java.io.BufferedReader rdr header]
  (when-let [line (.readLine rdr)]
    (if-not (or (meta-line? line) (header-line? line))
      (cons (parse-data-line line header)
            (lazy-seq (read-data-lines rdr header)))
      (read-data-lines rdr header))))

(defn read-variants
  [^VCFReader rdr]
  (read-data-lines (.reader rdr) (.header rdr)))
