(ns cljam.io.vcf.writer
  "A type of VCF writer and internal functions to write VCF contents. See
  https://samtools.github.io/hts-specs/ for the detail VCF specifications."
  (:require [clojure.string :as cstr]
            [camel-snake-kebab.core :refer [->camelCaseString]]
            [cljam.io.protocols :as protocols]
            [cljam.io.vcf.util :as vcf-util])
  (:import [java.io Closeable BufferedWriter]))

(declare write-variants)

;; VCFWriter
;; ---------

(deftype VCFWriter [url writer meta-info header]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-url [this] (.url this))
  protocols/IVariantWriter
  (write-variants [this variants]
    (write-variants this variants)))

;; Vars and utilities
;; ------------------

(def ^:private default-fileformat "VCFv4.3")

(def ^:private meta-info-prefix "##")
(def ^:private header-prefix "#")

(defn- nil->dot
  [s]
  (if (nil? s) "." s))

(defn- write-line
  [^BufferedWriter bwtr ^String s]
  (doto bwtr
    (.write s)
    (.newLine)))

;; Writing meta-information
;; ------------------------

(defn stringify-key
  [k]
  (if (#{:info :filter :format :alt :sample :pedigree} k)
    (cstr/upper-case (name k))
    (->camelCaseString k)))

(defn- add-extra-fields
  [fields m required-keys]
  (reduce-kv (fn [fields k v]
               (conj fields (str (->camelCaseString k) "=\"" v "\"")))
             fields
             (apply dissoc m required-keys)))

(defn- pack-meta-info
  [fields m required-keys]
  (cstr/join \, (add-extra-fields fields m required-keys)))

(defn- stringify-meta-info-contig
  [m]
  (-> [(str "ID=" (:id m))]
      (cond->
        (:idx m) (conj (str "idx=" (:idx m))))
      (pack-meta-info m [:id])))

(defn- stringify-meta-info-info
  [m]
  (-> [(str "ID=" (:id m))
       (str "Number=" (nil->dot (:number m)))
       (str "Type=" (nil->dot (:type m)))
       (str "Description=\"" (:description m) "\"")]
      (cond->
        (:source m) (conj (str "Source=\"" (:source m) "\""))
        (:version m) (conj (str "Version=\"" (:version m) "\""))
        (:idx m) (conj (str "idx=" (:idx m))))
      (pack-meta-info m [:id :number :type :description :source :version :idx])))

(defn- stringify-meta-info-filter
  [m]
  (-> [(str "ID=" (:id m))
       (str "Description=\"" (:description m) "\"")]
      (cond->
        (:idx m) (conj (str "idx=" (:idx m))))
      (pack-meta-info m [:id :description :idx])))

(defn- stringify-meta-info-format
  [m]
  (-> [(str "ID=" (:id m))
       (str "Number=" (nil->dot (:number m)))
       (str "Type=" (nil->dot (:type m)))
       (str "Description=\"" (:description m) "\"")]
      (cond->
        (:idx m) (conj (str "idx=" (:idx m))))
      (pack-meta-info m [:id :number :type :description :idx])))

(defn- stringify-meta-info-alt
  [m]
  (-> [(str "ID=" (:id m))
       (str "Description=\"" (:description m) "\"")]
      (pack-meta-info m [:id :description])))

(defn- stringify-meta-info-sample
  [m]
  (-> [(str "ID=" (:id m))]
      (pack-meta-info m [:id])))

(defn- stringify-meta-info-pedigree
  [m]
  (-> [(str "ID=" (:id m))]
      (pack-meta-info m [:id])))

(defn stringify-structured-line
  [k m]
  (let [f (case k
            :contig stringify-meta-info-contig
            :info stringify-meta-info-info
            :filter stringify-meta-info-filter
            :format stringify-meta-info-format
            :alt stringify-meta-info-alt
            :sample stringify-meta-info-sample
            :pedigree stringify-meta-info-pedigree)]
    (f m)))

(defn- write-meta-info1
  [^VCFWriter wtr k v]
  (if-not (nil? v)
    (if (sequential? v)
      (doseq [x v]
        (write-line (.writer wtr) (str meta-info-prefix
                                       (stringify-key k)
                                       "=<" (stringify-structured-line k x) ">")))
      (write-line (.writer wtr) (str meta-info-prefix
                                     (stringify-key k)
                                     "=" v)))))

(defn write-meta-info
  [^VCFWriter wtr meta-info]
  (write-meta-info1 wtr :fileformat (:fileformat meta-info default-fileformat))
  (doseq [k [:file-date :source :reference :contig :phasing]]
    (write-meta-info1 wtr k (get meta-info k)))
  (doseq [k [:info :filter :format :alt :sample :pedigree]]
    (write-meta-info1 wtr k (get meta-info k))))

;; Writing header
;; --------------

(defn ^String stringify-header
  [header]
  (str header-prefix (cstr/join \tab header)))

(defn write-header
  [^VCFWriter wtr header]
  (write-line (.writer wtr) (stringify-header header)))

;; Writing data lines
;; ------------------

(defn- stringify-data-line-alt
  [v]
  (if v
    (cstr/join \, v)))

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
    (->> (concat [:chr :pos :id :ref :alt :qual :filter :info]
                 (map keyword (drop 8 header)))
         (map #(get m* %))
         (map nil->dot)
         (cstr/join \tab))))

(defn write-variants
  [^VCFWriter wtr variants]
  (let [stringify-vals (vcf-util/variant-vals-stringifier (.meta-info wtr) (.header wtr))
        header-kws (drop 8 (map keyword (.header wtr)))]
    (doseq [v variants]
      (write-line (.writer wtr)
                  (stringify-data-line
                   (if (some string? ((apply juxt :filter :info header-kws) v))
                     v
                     (stringify-vals v)) (.header wtr))))))
