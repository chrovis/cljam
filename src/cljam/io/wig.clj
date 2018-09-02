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

(def ^:const wig-fields [:track :chr :start :end :value])

(defn- header-or-comment?
  "Checks if given string is neither a header nor a comment line."
  [^String s]
  (or (empty? s)
      (.startsWith s "browser")
      (.startsWith s "#")))

(defn- normalize
  "Normalize WIG lines."
  [m]
  (update m :chr chr/normalize-chromosome-key))

(defn- fields->map
  "Convert vector [\"key1=value1\" \"key2=value2\" ...] to map
  {:key1 \"value1\" :key2 \"value2\" ...}."
  [fields]
  (into {}
        (map (fn [field]
               (let [[k v] (cstr/split field #"=")]
                 [(keyword k) v])))
        fields))

(defn- str->wiggle-track-data
  "Convert string to Wiggle track data values which can be integer or real,
  positive or negative values."
  [^String s]
  (if-let [l (as-long s)]
    l
    (as-double s)))

(defn- deserialize-wigs
  "Parse WIG lines and return a lazy sequence of flat map."
  [lines]
  (letfn [(deserialize [lines pre-start track]
            (lazy-seq
             (when-first [line lines]
               (let [fields (-> line cstr/trim (cstr/split #"\s+"))]
                 (case (first fields)
                   ; track definition line
                   "track"
                   (let [type (->> fields rest fields->map :type)]
                     (if (= type "wiggle_0")
                       (deserialize (rest lines) pre-start {:line line})
                       (throw "The track type with version must be `wiggle_0`")))

                   ; declaration line of variableStep
                   "variableStep"
                   (let [{:keys [chrom span]
                          :or {span 1}} (->> fields rest fields->map)
                         span (as-long span)
                         track (assoc track :format :variable-step
                                            :chr chrom
                                            :span span
                                            :step nil)]
                     (deserialize (rest lines) nil track))

                   ; declaration line of fixedStep
                   "fixedStep"
                   (let [{:keys [chrom start span step]
                          :or {span 1, step 1}} (->> fields rest fields->map)
                         step (as-long step)
                         pre-start (- (as-long start) step)
                         span (as-long span)
                         track (assoc track :format :fixed-step
                                            :chr chrom
                                            :span span
                                            :step step)]
                     (deserialize (rest lines) pre-start track))

                   ; data line
                   (let [m (case (:format track)
                             :variable-step
                             (let [{:keys [chr span]} track
                                   [start value] fields
                                   start (as-long start)
                                   end (dec (+ start span))
                                   value (str->wiggle-track-data value)]
                               {:track track
                                :chr chr
                                :start start
                                :end end
                                :value value})

                             :fixed-step
                             (let [{:keys [chr span step]} track
                                   start (+ pre-start step)
                                   end (dec (+ start span))
                                   value (-> fields first str->wiggle-track-data)]
                               {:track track
                                :chr chr
                                :start start
                                :end end
                                :value value})

                             (throw
                              (IllegalArgumentException.
                               "Invalid wiggle format")))]
                     (cons m (deserialize (rest lines)
                                          (:start m)
                                          track))))))))]
    (deserialize lines nil {:line nil
                            :format nil
                            :chr nil
                            :step nil
                            :span nil})))

(defn- serialize-wigs
  "Serialize a sequence of WIG fields into a lazy sequence of string."
  [wigs]
  {:pre [;; These fields are required.
         (map (fn [wig] (every? some? ((apply juxt wig-fields) wig))) wigs)
         ;; There are two options for formatting wiggle data.
         (map (fn [wig]
                (let [format (-> wig :track :format)]
                  (or (= format :fixed-step)
                      (= format :variable-step))))
              wigs)]}
  (letfn [(serialize [wigs]
            (->> wigs
                 (sequence
                  (comp
                   (partition-by (juxt (comp :format :track)
                                       #(- (:end %) (:start %))))
                   (map
                    (fn [[{{:keys [line format span step]} :track
                           chr :chr
                           start :start
                           end :end} :as xs]]
                      (case format
                        :variable-step
                        (let [declaration-line (->> (cond-> ["variableStep"
                                                             (str "chrom=" chr)]
                                                      (not= span 1)
                                                      (conj (str "span=" span)))
                                                    (cstr/join \space))
                              data-lines (->> xs
                                              (map (fn [{:keys [start value]}]
                                                     (cstr/join \space
                                                                [start value])))
                                              (cstr/join \newline))]
                          (cstr/join \newline
                                     (cond->> [declaration-line data-lines]
                                       line (cons line))))

                        :fixed-step
                        (let [declaration-line (->> (cond-> ["fixedStep"
                                                             (str "chrom=" chr)
                                                             (str "start=" start)]
                                                      (not= step 1)
                                                      (conj (str "step=" step))

                                                      (not= span 1)
                                                      (conj (str "span=" span)))
                                                    (cstr/join \space))
                              data-lines (->> xs
                                              (map (fn [{:keys [value]}] value))
                                              (cstr/join \newline))]
                          (cstr/join \newline
                                     (cond->> [declaration-line data-lines]
                                       line (cons line)))))))))))]
    (serialize wigs)))

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
         serialize-wigs
         (interpose \newline)
         (run! #(.write w (str %))))))
