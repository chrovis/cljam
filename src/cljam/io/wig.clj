(ns cljam.io.wig
  "Functions to read and write the WIG (Wiggle) format. See
  https://genome.ucsc.edu/goldenpath/help/wiggle.html for the detail WIG
  specifications."
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [proton.core :refer [as-long as-double]]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]
            [cljam.util.chromosome :as chr])
  (:import [java.net URL]
           [java.io BufferedReader BufferedWriter Closeable]))

(declare read-fields write-fields)

(defrecord WIGReader [^BufferedReader reader ^URL url]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (read-fields this))
  (read [this option] (read-fields this))
  (indexed? [_] false))

(defrecord WIGWriter [^BufferedWriter writer ^URL url]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-url [this] (.url this)))

(defn ^WIGReader reader
  "Returns an open cljam.io.wig.WIGReader of f. Should be used inside with-open
  to ensure the reader is properly closed."
  [f]
  (WIGReader. (cio/reader (util/compressor-input-stream f)) (util/as-url f)))

(defn ^WIGWriter writer
  "Returns an open cljam.io.wig.WIGWriter of f. Should be used inside with-open
  to ensure the writer is properly closed."
  [f]
  (WIGWriter. (cio/writer (util/compressor-output-stream f)) (util/as-url f)))

(defn- header-or-comment?
  "Checks if given string is neither a header nor a comment line."
  [^String s]
  (or (empty? s)
      (.startsWith s "browser")
      (.startsWith s "track")
      (.startsWith s "#")))

(defn- normalize
  "Normalize WIG lines."
  [m]
  (update m :chrom chr/normalize-chromosome-key))

(defn- fields->map
  "Convert vector [\"key1=value1\" \"key2=value2\" ...] to map {:key1 \"value1\" :key2 \"value2\" ...}."
  [fields]
  (->> fields
       (map #(cstr/split % #"="))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn- str->wiggle-track-data
  "Convert string to Wiggle track data values which can be integer or real,
  positive or negative values."
  [^String s]
  (if-let [l (as-long s)]
    l
    (as-double s)))

(defn- deserialize-wigs
  "Parse WIG lines and returns a vector of maps."
  [lines]
  (loop [lines lines
         wigs []
         current-track nil]
    (if (nil? (first lines))
      (conj wigs current-track)
      (let [fields (-> lines first cstr/trim (cstr/split #"\s+"))]
        (case (first fields)
          ; declaration line of variableStep
          "variableStep"
          (let [{:keys [chrom span] :or {span 1}} (->> fields rest fields->map)]
            (recur (rest lines)
                   (cond-> wigs
                     (some? current-track) (conj current-track))
                   {:format :variable-step
                    :chrom chrom
                    :span (as-long span)
                    :values []}))

          ; declaration line of fixedStep
          "fixedStep"
          (let [{:keys [chrom start step span] :or {span 1}} (->> fields rest fields->map)]
            (recur (rest lines)
                   (cond-> wigs
                     (some? current-track) (conj current-track))
                   {:format :fixed-step
                    :chrom chrom
                    :start (as-long start)
                    :step (as-long step)
                    :span (as-long span)
                    :values []}))

          ; data line
          (let [v (case (:format current-track)
                    :variable-step
                    (let [[start value] fields]
                      {:start (as-long start)
                       :value (str->wiggle-track-data value)})

                    :fixed-step
                    (-> fields first str->wiggle-track-data)

                    (throw (IllegalArgumentException. "Invalid wiggle format")))]
            (recur (rest lines)
                   wigs
                   (update current-track :values #(conj % v)))))))))

(defn- serialize-wig
  "Serialize WIG line into vector of strings."
  [wig]
  {:pre [;; There are two options for formatting wiggle data.
         (or (= (:format wig) :fixed-step) (= (:format wig) :variable-step))
         ;; These fields are required if the format is fixedStep.
         (if (= (:format wig) :fixed-step) (and (:chrom wig) (:span wig) (:start wig) (:step wig) (:values wig)) true)
         ;; These fields are required if the format is variableStep.
         (if (= (:format wig) :variable-step) (and (:chrom wig) (:span wig) (:values wig)) true)]}
  (case (:format wig)
    :variable-step
    (let [{:keys [chrom span values]} wig
          declaration-line (->> (cond-> ["variableStep" (str "chrom=" chrom)]
                                  (not= span 1) (conj (str "span=" span)))
                                (cstr/join \space))
          data-lines (->> values
                          (map (fn [{:keys [start value]}]
                                 (cstr/join \space [start value])))
                          (cstr/join \newline))]
      (cstr/join \newline [declaration-line data-lines]))

    :fixed-step
    (let [{:keys [chrom start step span values]} wig
          declaration-line (->> (cond-> ["fixedStep" (str "chrom=" chrom) (str "start=" start) (str "step=" step)]
                                  (not= span 1) (conj (str "span=" span)))
                                (cstr/join \space))
          data-lines (cstr/join \newline values)]
      (cstr/join \newline [declaration-line data-lines]))

    (throw (IllegalArgumentException. "Invalid wiggle format"))))

(defn read-fields
  "Read sequence of WIG fields from reader"
  [^WIGReader rdr]
  (->> rdr
       .reader
       line-seq
       (remove header-or-comment?)
       deserialize-wigs
       (map normalize)))

(defn write-fields
  "Write sequence of WIG fields to writer."
  [^WIGWriter wtr xs]
  (let [w ^BufferedWriter (.writer wtr)]
    (->> xs
         (map serialize-wig)
         (cstr/join \newline)
         (.write w))))
