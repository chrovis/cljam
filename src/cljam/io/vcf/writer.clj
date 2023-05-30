(ns cljam.io.vcf.writer
  "A type of VCF writer and internal functions to write VCF contents. See
  https://samtools.github.io/hts-specs/ for the detail VCF specifications."
  (:require [clojure.string :as cstr]
            [camel-snake-kebab.core :refer [->camelCaseString ->PascalCaseString]]
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

(defn- escape-special-chars [s]
  (-> s (cstr/replace "\\" "\\\\") (cstr/replace "\"" "\\\"")))

(defn- add-extra-fields
  [fields m required-keys]
  (reduce-kv (fn [fields k v]
               (conj fields
                     (str (->PascalCaseString k) "=\""
                          (escape-special-chars v) "\"")))
             fields
             (apply dissoc m required-keys)))

(defn- pack-meta-info
  [fields m required-keys]
  (cstr/join \, (add-extra-fields fields m required-keys)))

(defn- stringify-meta-info-contig
  [m]
  (-> [(str "ID=" (:id m))]
      (cond->
       (:length m) (conj (str "length=" (:length m)))
       (:assembly m) (conj (str "assembly=" (:assembly m)))
       (:md-5 m) (conj (str "md5=" (:md-5 m)))
       (:url m) (conj (str "URL=" (:url m)))
       (:species m) (conj (str "species=\"" (escape-special-chars (:species m)) "\""))
       (:taxonomy m) (conj (str "taxonomy=" (:taxonomy m)))
       (:idx m) (conj (str "IDX=" (:idx m))))
      (pack-meta-info m [:id :length :assembly :md-5 :url :species :taxonomy :idx])))

(defn- stringify-meta-info-info
  [m]
  (-> [(str "ID=" (:id m))
       (str "Number=" (nil->dot (:number m)))
       (str "Type=" (nil->dot (:type m)))
       (str "Description=\"" (escape-special-chars (:description m)) "\"")]
      (cond->
       (:source m) (conj (str "Source=\"" (escape-special-chars (:source m)) "\""))
       (:version m) (conj (str "Version=\"" (escape-special-chars (:version m)) "\""))
       (:idx m) (conj (str "IDX=" (:idx m))))
      (pack-meta-info m [:id :number :type :description :source :version :idx])))

(defn- stringify-meta-info-filter
  [m]
  (-> [(str "ID=" (:id m))
       (str "Description=\"" (escape-special-chars (:description m)) "\"")]
      (cond->
       (:idx m) (conj (str "IDX=" (:idx m))))
      (pack-meta-info m [:id :description :idx])))

(defn- stringify-meta-info-format
  [m]
  (-> [(str "ID=" (:id m))
       (str "Number=" (nil->dot (:number m)))
       (str "Type=" (nil->dot (:type m)))
       (str "Description=\"" (escape-special-chars (:description m)) "\"")]
      (cond->
       (:idx m) (conj (str "IDX=" (:idx m))))
      (pack-meta-info m [:id :number :type :description :idx])))

(defn- stringify-meta-info-alt
  [m]
  (-> [(str "ID=" (:id m))
       (str "Description=\"" (escape-special-chars (:description m)) "\"")]
      (pack-meta-info m [:id :description])))

(defn- stringify-meta-info-sample
  [m]
  (-> [(str "ID=" (:id m))]
      (cond->
       (:genomes m) (conj (str "Genomes=" (:genomes m)))
       (:mixture m) (conj (str "Mixture=" (:mixture m))))
      (conj (str "Description=\"" (escape-special-chars (:description m)) "\""))
      (pack-meta-info m [:id :genomes :mixture :description])))

(defn- stringify-meta-info-pedigree
  [m]
  (->> (reduce-kv (fn [fields k v]
                    (conj fields
                          (if-let [[_ i] (re-matches #"name-(\d+)" (name k))]
                            (str "Name_" i "=" v)
                            (str (->PascalCaseString k) "=" v))))
                  [(str "ID=" (:id m))]
                  (dissoc m :id))
       (cstr/join \,)))

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
  (when-not (nil? v)
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

(defn stringify-header ^String
  [header]
  (str header-prefix (cstr/join \tab header)))

(defn write-header
  [^VCFWriter wtr header]
  (write-line (.writer wtr) (stringify-header header)))

;; Writing data lines
;; ------------------

(defn- stringify-data-line-alt
  [v]
  (when v
    (cstr/join \, v)))

(def ^:private ^:const precise-integer-limit 0x800000)

(defn- stringify-data-line-qual
  [x]
  (when x
    (if (and (zero? (float (mod x 1)))
             (< (float x) precise-integer-limit))
      (str (int x))
      (str x))))

(defn- stringify-data-line ^String
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
