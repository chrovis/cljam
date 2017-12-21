(ns cljam.io.sam.util.option
  "Utility functions for SAM optional fields."
  (:require [clojure.string :as cstr]
            [proton.core :as p]))

;;; parse

(defn- parse-tag-single [val-type val]
  (case val-type
    \Z val
    \A (first val)
    \I (p/as-long val)
    \i (p/as-long val)
    \s (p/as-long val)
    \S (p/as-long val)
    \c (p/as-long val)
    \C (p/as-long val)
    \f (p/as-double val)
    \H (p/hex->bytes val)
    (throw (Exception. (format "Unrecognized tag type: %s, for value: %s" val-type val)))))

(defn parse-optional-field [op]
  (let [[tag val-type-str val] (cstr/split op #":" 3)
        val-type (first val-type-str)]
    {(keyword tag) {:type val-type-str
                    :value (if (= val-type \B)
                             val
                             (parse-tag-single val-type val))}}))

;;; stringify

(defn stringify-optional-fields [options]
  (->> options
       (map
        (fn [op]
          (let [[tag {:keys [type value]}] (first (seq op))]
            (cstr/join \: [(name tag) type value]))))
       (cstr/join \tab)))
