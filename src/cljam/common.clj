(ns cljam.common
  "Common vars.")

(def ^:dynamic *n-threads*
  "The number of threads."
  1)

(defn get-exec-n-threads
  "Returns the actual number of threads that will be used for executions.
  *n-threads* itself will be used if it is more than zero. Otherwise,
  (processors + 3) will be used. Note that this return value includes the main
  thread."
  ^long
  []
  (if (pos? (long *n-threads*))
    *n-threads*
    (+ 3 (.. Runtime getRuntime availableProcessors))))
