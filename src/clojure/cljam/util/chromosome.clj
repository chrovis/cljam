(ns cljam.util.chromosome
  (:require [clojure.string :as str]))

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
      prepend-chromosome-prefix
      normalize-chromosome-prefix))
