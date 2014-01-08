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
(def temp-bam (str temp-dir "/out.bam"))
(def temp-sam (str temp-dir "/out.sam"))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about view"
        (with-out-file temp-out (core/view [test-sam-file])) => nil
        (with-out-file temp-out (core/view [test-bam-file])) => nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sort"
        (with-out-file temp-out (core/sort [test-sam-file (str temp-dir "/out.sam")])) => nil
        (with-out-file temp-out (core/sort [test-bam-file (str temp-dir "/out.bam")])) => nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about pileup"
        (with-out-file temp-out (core/pileup [test-sorted-bam-file])) => nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about convert"
        ;; sam => bam
        (core/convert [test-sam-file temp-bam]) => anything
        (= (slurp-sam-for-test test-sam-file)
           (slurp-bam-for-test temp-bam)) => truthy
        (= (slurp-bam-for-test test-bam-file)
           (slurp-bam-for-test temp-bam)) => truthy
        ;; bam => sam
        (core/convert [test-bam-file temp-sam]) => anything
        (= (slurp-bam-for-test test-bam-file)
           (slurp-sam-for-test temp-sam)) => truthy
        (= (slurp-sam-for-test test-sam-file)
           (slurp-sam-for-test temp-sam)) => truthy
        ))
