(ns cljam.util.chromosome
  (:require [clojure.string :as str]))

(defn normalize-name
  [s]
  (-> s
      (str/replace #"\."  "_")
      (str/replace #","  "_")
      (str/replace #"\""  "")
      (str/replace #"\'"  "")))

(defn trim-chromosome-key
  [s]
  (-> s
      (str/replace #"Chr" "")
      (str/replace #"chr" "")))

(defn- normalize-chromosome-prefix
  [s]
  (let [m (re-matches #"(?i)^chr([0-9]+|X|Y|M|MT)" s)]
    (if (nil? m)
      s
      (let [[_ tail] m]
        (str "chr" (.toUpperCase tail))))))

(defn- prepend-chromosome-prefix
  [s]
  (let [m (re-matches #"(?i)^([0-9]+|X|Y|M|MT)" s)]
    (if (nil? m)
      s
      (let [[_ tail] m]
        (str "chr" (.toUpperCase tail))))))

(defn normalize-chromosome-key
  [s]
  (-> s
      normalize-name
      prepend-chromosome-prefix
      normalize-chromosome-prefix))
