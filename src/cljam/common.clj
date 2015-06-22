(ns cljam.common
  "Common vars.")

(def version "SAM format version" "1.4")

(def ^:dynamic *n-threads*
  "The number of threads."
  1)

(defn get-exec-n-threads
  "Returns the actual number of threads that will be used for executions.
  *n-threads* itself will be used if it is more than zero. Otherwise,
  (processors + 2) will be used."
  []
  (if (pos? *n-threads*)
    *n-threads*
    (+ 2 (.. Runtime getRuntime availableProcessors))))
