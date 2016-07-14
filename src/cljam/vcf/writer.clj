(ns cljam.vcf.writer
  "A type of VCF writer and internal functions to write VCF contents. See
  https://samtools.github.io/hts-specs/ for the detail VCF specifications."
  (:require [clojure.string :as cstr]
            [camel-snake-kebab.core :refer [->camelCaseString]]))

;; VCFWriter
;; ---------

(deftype VCFWriter [f writer header]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

;; Vars and utilities
;; ------------------

(def ^:private default-fileformat "VCFv4.3")

(def ^:private meta-info-prefix "##")
(def ^:private header-prefix "#")

(defn- nil->dot
  [s]
  (if (nil? s) "." s))

(defn- write-line
  [^java.io.BufferedWriter bwtr ^String s]
  (doto bwtr
    (.write s)
    (.newLine)))

;; Writing meta-information
;; ------------------------

(defn- stringify-meta-info-info
  [m]
  (->> (cond-> [(str "ID=" (:id m))
                (str "Number=" (nil->dot (:number m)))
                (str "Type=" (nil->dot (:type m)))
                (str "Description=\"" (:description m) "\"")]
         (:source m) (conj (str "Source=" (:source m)))
         (:version m) (conj (str "Version=" (:version m))))
       (interpose ",")
       (apply str)))

(defn- stringify-meta-info-filter
  [m]
  (->> [(str "ID=" (:id m))
        (str "Description=\"" (:description m) "\"")]
       (interpose ",")
       (apply str)))

(defn- stringify-meta-info-format
  [m]
  (->> [(str "ID=" (:id m))
        (str "Number=" (nil->dot (:number m)))
        (str "Type=" (nil->dot (:type m)))
        (str "Description=\"" (:description m) "\"")]
       (interpose ",")
       (apply str)))

(defn- stringify-meta-info-alt
  [m]
  (->> [(str "ID=" (:id m))
        (str "Description=\"" (:description m) "\"")]
       (interpose ",")
       (apply str)))

(defn- stringify-meta-info-sample
  [m]
  (->> [(str "ID=" (:id m))
        (str "Genomes=" (:genomes m))
        (str "Mixture=" (:mixture m))
        (str "Description=\"" (:description m) "\"")]
       (interpose ",")
       (apply str)))

(defn- stringify-meta-info-pedigree
  [m]
  (->> (range (count m))
       (map (fn [i]
              (str "Name_" i "=" (get m (keyword (str "name-" i))))))
       (interpose ",")
       (apply str)))

(defn- stringify-structured-line
  [k m]
  (let [f (case k
            :info stringify-meta-info-info
            :filter stringify-meta-info-filter
            :format stringify-meta-info-format
            :alt stringify-meta-info-alt
            :sample stringify-meta-info-sample
            :pedigree stringify-meta-info-pedigree)]
    (f m)))

(defn write-meta-info
  [^VCFWriter wtr meta-info]
  (write-line (.writer wtr) (str meta-info-prefix "fileformat="
                                 (:fileformat meta-info default-fileformat)))
  (doseq [[k v] (dissoc meta-info :fileformat :info :filter :format :alt :sample
                        :pedigree)]
    (write-line (.writer wtr) (str meta-info-prefix (->camelCaseString k) "="
                                   v)))
  (doseq [k [:info :filter :format :alt :sample :pedigree]
          v (get meta-info k)]
    (write-line (.writer wtr) (str meta-info-prefix (cstr/upper-case (name k))
                                   "=<" (stringify-structured-line k v) ">"))))

;; Writing header
;; --------------

(defn- ^String stringify-header
  [header]
  (str header-prefix (apply str (interpose "\t" header))))

(defn write-header
  [^VCFWriter wtr header]
  (write-line (.writer wtr) (stringify-header header)))

;; Writing data lines
;; ------------------

(defn- stringify-data-line-alt
  [v]
  (if v (apply str (interpose "," v))))

(defn- stringify-data-line-qual
  [x]
  (if x
    (if (zero? (mod x 1))
      (str (int x))
      (str x))))

(defn- ^String stringify-data-line
  [m header]
  (let [m* (-> m
               (update :alt stringify-data-line-alt)
               (update :qual stringify-data-line-qual))]
    (->> (concat [:chrom :pos :id :ref :alt :qual :filter :info]
                 (map keyword (drop 8 header)))
         (map #(get m* %))
         (map nil->dot)
         (interpose "\t")
         (apply str))))

(defn write-variants
  [^VCFWriter wtr variants]
  (doseq [v variants]
    (write-line (.writer wtr) (stringify-data-line v (.header wtr)))))
