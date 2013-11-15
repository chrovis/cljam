(ns cljam.t-core
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :as io]
            [cljam.core :as core]))

(defmacro with-out-file
  [f & body]
  `(binding [*out* (clojure.java.io/writer ~f)]
     ~@body))

(def temp-out (str temp-dir "/out"))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about view"
        (with-out-file temp-out (core/view test-sam-file)) => nil
        (with-out-file temp-out (core/view test-bam-file)) => nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sort"
        (with-out-file temp-out (core/sort test-sam-file (str temp-dir "/out.sam"))) => nil
        (with-out-file temp-out (core/sort test-bam-file (str temp-dir "/out.bam"))) => nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about pileup"
        (with-out-file temp-out (core/pileup test-sorted-bam-file)) => nil))
