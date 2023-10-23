(ns cljam.io.fasta.util
  (:require [cljam.util :refer [space?]]))

(defn header-line?
  "Returns `true` iff the line is a header."
  [line]
  (= (first line) \>))

(def ^:private vertial-bar (char 0x7c))

(defn- vertial-bar?
  [c]
  (= c vertial-bar))

(defn- header-blank?
  [c]
  (or (space? c)
      (vertial-bar? c)))

(defn parse-header-line
  "Parses a header line string `line` into a map containing :name and :desc."
  [line]
  (let [line (subs line 1)]
    {:name (->> line
                (take-while #(not (header-blank? %)))
                (apply str))
     :desc (->> line
                (drop-while #(not (header-blank? %)))
                (drop 1)
                (apply str))}))
