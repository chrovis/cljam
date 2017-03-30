(ns cljam.util.chromosome
  (:require [clojure.string :as cstr]))

(defn normalize-name
  [s]
  (-> s
      (cstr/replace #"\."  "_")
      (cstr/replace #","  "_")
      (cstr/replace #"\""  "")
      (cstr/replace #"\'"  "")))

(defn trim-chromosome-key
  [s]
  (-> s
      (cstr/replace #"Chr" "")
      (cstr/replace #"chr" "")))

(defn- normalize-chromosome-prefix
  [s]
  (let [m (re-matches #"(?i)^chr([0-9]{1,2}|X|Y|M|MT)(.*)$" s)]
    (if (nil? m)
      s
      (let [[_ base leftover] m
            base  (if (re-find #"^\d+$" base)
                    (str (Integer. base))
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
  [s]
  (-> s
      normalize-name
      prepend-chromosome-prefix
      normalize-chromosome-prefix))
