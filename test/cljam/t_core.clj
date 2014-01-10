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
  (fact "about sort (by pos)"
        ;; see https://gitlab.xcoo.jp/chrovis/cljam/issues/12
        (with-out-file temp-out (core/sort ["-o" "coordinate" test-sam-file temp-sam])) =future=> anything
        (slurp-sam-for-test temp-sam) =future=> test-sam-sorted-by-pos
        (check-sort-order (slurp-sam-for-test temp-sam) test-sam-sorted-by-pos) =future=> anything
        (with-out-file temp-out (core/sort ["-o" "coordinate" test-bam-file temp-bam])) => anything
        (slurp-bam-for-test temp-bam) => test-sam-sorted-by-pos
        (check-sort-order (slurp-bam-for-test temp-bam) test-sam-sorted-by-pos) => anything
        ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sort (by qname)"
        (with-out-file temp-out (core/sort ["-o" "queryname" test-sam-file temp-sam])) =future=> anything
        (slurp-sam-for-test temp-sam) =future=> test-sam-sorted-by-qname
        (with-out-file temp-out (core/sort ["-o" "queryname" test-bam-file temp-bam])) =future=> anything
        (slurp-bam-for-test temp-bam) =future=> test-sam-sorted-by-qname
        ))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (io/copy (io/file test-sorted-bam-file)
                                                 (io/file temp-bam))))
                     (after  :facts (clean-cache!))]
  (fact "about index"
        (with-out-file temp-out (core/index [temp-bam])) => anything
        (.exists (io/file (str temp-bam ".bai"))) => truthy
        ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about pileup"
        (with-out-file temp-out (core/pileup [test-sorted-bam-file])) => anything
        (slurp temp-out) => (slurp "test/resources/t_core.pileup")
        ))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (io/copy (io/file test-fa-file)
                                                 (io/file temp-out))))
                     (after  :facts (clean-cache!))]
  (fact "about faidx"
        (with-out-file temp-out (core/faidx [temp-out])) => anything
        (.exists (io/file (str temp-out ".fai"))) => truthy))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (let [temp-dict (str temp-dir "/out.dict")]
    (fact "about dict"
          (with-out-file temp-out (core/dict [test-fa-file temp-dict])) =future=> anything
          (.exists (io/file temp-dict)) =future=> truthy)))
