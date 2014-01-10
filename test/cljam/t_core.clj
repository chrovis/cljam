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
        (with-out-file temp-out (core/view [test-sam-file])) => anything
        (slurp temp-out) => (slurp "test/resources/t_core.view")
        (with-out-file temp-out (core/view [test-bam-file])) => anything
        (slurp temp-out) => (slurp "test/resources/t_core.view")
        ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about convert"
        ;; sam => bam
        (core/convert [test-sam-file temp-bam]) => anything
        (slurp-bam-for-test temp-bam) => (slurp-sam-for-test test-sam-file)
        (slurp-bam-for-test temp-bam) => (slurp-bam-for-test test-bam-file)
        ;; bam => sam
        (core/convert [test-bam-file temp-sam]) => anything
        (slurp-sam-for-test temp-sam) => (slurp-bam-for-test test-bam-file)
        (slurp-sam-for-test temp-sam) => (slurp-sam-for-test test-sam-file)
        ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sort"
        ;; TODO: add test to sort by qname
        (with-out-file temp-out (core/sort [test-sam-file temp-sam])) => nil
        (slurp-sam-for-test temp-sam) => test-sam-sorted-by-pos
        (check-sort-order (slurp-sam-for-test temp-sam) test-sam-sorted-by-pos) => anything
        (with-out-file temp-out (core/sort [test-bam-file temp-bam])) => nil
        (slurp-bam-for-test temp-bam) => test-sam-sorted-by-pos
        (check-sort-order (slurp-bam-for-test temp-bam) test-sam-sorted-by-pos) => anything
        ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about index"
        "TODO" =future=> nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about pileup"
        (with-out-file temp-out (core/pileup [test-sorted-bam-file])) => nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about faidx"
        "TODO" =future=> nil))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about dict"
        "TODO" =future=> nil))
