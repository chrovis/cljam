(ns cljam.util.chromosome
  "Utilities for handling chromosome name."
  (:require [clojure.string :as cstr]))

(defn normalize-name
  [s]
  (-> s
      (cstr/replace #"\."  "_")
      (cstr/replace #","  "_")
      (cstr/replace #"\""  "")
      (cstr/replace #"\'"  "")))

(defn trim-chromosome-key
  "Removes chr prefix from chromosome name."
  [s]
  (cstr/replace s #"(?i)^chr" ""))

(defn- normalize-chromosome-prefix
  [s]
  (let [m (re-matches #"(?i)^chr([0-9]{1,2}|X|Y|M|MT)(.*)$" s)]
    (if (nil? m)
      s
      (let [[_ base leftover] m
            base  (if (re-find #"^\d+$" base)
                    (str (Integer. ^String base))
                    base)]
        (str "chr" (cstr/upper-case (str base leftover)))))))

(defn- prepend-chromosome-prefix
  [s]
  (let [m (re-matches #"(?i)^([0-9]{1,2}|X|Y|M|MT)(.*)$" s)]
    (if (nil? m)
      s
      (let [[_ base leftover] m]
        (str "chr" (cstr/upper-case (str base leftover)))))))

(defn normalize-chromosome-key
  "Normalizes chromosome name."
  [s]
  (-> s
      normalize-name
      prepend-chromosome-prefix
      normalize-chromosome-prefix))
