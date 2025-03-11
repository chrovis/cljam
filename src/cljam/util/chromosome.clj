(ns cljam.util.chromosome
  "Utilities for handling chromosome name."
  (:require [clojure.string :as cstr]
            [proton.core :as proton]))

(defn normalize-name
  "Converts characters ',' or '.' in chromosome name to '_'
  and erases ' and \"."
  [s]
  (-> s
      (cstr/replace #"[,\.]" "_")
      (cstr/replace #"[\"\']" "")))

(defn trim-chromosome-key
  "Removes chr prefix from chromosome name."
  [s]
  (cstr/replace s #"(?i)^chr" ""))

(defn- split-version-suffix
  [s]
  (let [[_ base suffix _] (re-matches #"(.+?)((?i)v[0-9](_alt|_random)?)?" s)]
    [base suffix]))

(defn- normalize-chromosome-prefix
  [s]
  (if-let [[_ base leftover] (re-matches #"(?i)chr([0-9]{1,2}|X|Y|M|MT|Un)(.*)" s)]
    (let [base* (condp re-matches base
                  #"\d+" (str (Integer/parseInt base))
                  #"(?i)Un" "Un"
                  (cstr/upper-case base))]
      (str "chr" base* (cstr/upper-case leftover)))
    s))

(defn- prepend-chromosome-prefix
  [s]
  (if (re-matches #"(?i)([0-9]{1,2}|X|Y|M|MT|Un).*" s)
    (str "chr" s)
    s))

(defn normalize-chromosome-key
  "Normalizes chromosome name."
  [s]
  (let [[base version-suffix] (split-version-suffix s)]
    (str (-> base
             normalize-name
             prepend-chromosome-prefix
             normalize-chromosome-prefix)
         (when version-suffix (cstr/lower-case version-suffix)))))

(defn is-primary-chromosome?
  "Returns `true` if `s` is a primary chromosome name (chr1-chr22, chrX,
  chrY, chrM, and chrMT), otherwise returns `false`."
  [s]
  (some? (re-matches #"^chr([0-9]{1,2}|X|Y|M|MT)"
                     (normalize-chromosome-key s))))

(defn chromosome-order-key
  "Converts a chromosome name into a comparable key.
  In autosomes (chr1, chr2, ...), they are ordered by their numerical
  designations, followed by chrX, chrY, chrM, and chrMT.
  If two chromosome name prefixes are identical, they are sorted
  in lexicographical order of the succeeding strings."
  [s]
  (if-let [[_ _ chr suffix] (re-find #"(?i)^(chr)?([1-9][0-9]*|X|Y|MT|M)(\S*)" s)]
    (if-let [num' (proton/as-int chr)]
      [num' suffix]
      [(- Integer/MAX_VALUE (case chr "X" 4 "Y" 3 "M" 2 "MT" 1)) suffix])
    [Integer/MAX_VALUE s]))
