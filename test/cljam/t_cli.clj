(ns cljam.t-cli
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :as io]
            [cljam.cli :as cli]))

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
    ;; NB: "view" output format may change in future
    (with-out-file temp-out (cli/view [test-sam-file])) => anything
    (with-out-file temp-out (cli/view [test-bam-file])) => anything
    ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about convert"
    ;; sam => bam
    (cli/convert [test-sam-file temp-bam]) => anything
    (slurp-bam-for-test temp-bam) => (slurp-sam-for-test test-sam-file)
    (slurp-bam-for-test temp-bam) => (slurp-bam-for-test test-bam-file)
    ;; bam => sam
    (cli/convert [test-bam-file temp-sam]) => anything
    (slurp-sam-for-test temp-sam) => (slurp-bam-for-test test-bam-file)
    (slurp-sam-for-test temp-sam) => (slurp-sam-for-test test-sam-file)
    ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sort (by pos)"
    ;; see https://gitlab.xcoo.jp/chrovis/cljam/issues/12
    (with-out-file temp-out (cli/sort ["-o" "coordinate" test-sam-file temp-sam])) =future=> anything
    (slurp-sam-for-test temp-sam) =future=> test-sam-sorted-by-pos
    (check-sort-order (slurp-sam-for-test temp-sam) test-sam-sorted-by-pos) =future=> anything
    (with-out-file temp-out (cli/sort ["-o" "coordinate" test-bam-file temp-bam])) => anything
    (slurp-bam-for-test temp-bam) => test-sam-sorted-by-pos
    (check-sort-order (slurp-bam-for-test temp-bam) test-sam-sorted-by-pos) => anything
    ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sort (by qname)"
    (with-out-file temp-out (cli/sort ["-o" "queryname" test-sam-file temp-sam])) =future=> anything
    (slurp-sam-for-test temp-sam) =future=> test-sam-sorted-by-qname
    (with-out-file temp-out (cli/sort ["-o" "queryname" test-bam-file temp-bam])) =future=> anything
    (slurp-bam-for-test temp-bam) =future=> test-sam-sorted-by-qname
    ))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (io/copy (io/file test-sorted-bam-file)
                                                 (io/file temp-bam))))
                     (after  :facts (clean-cache!))]
  (fact "about index"
    (with-out-file temp-out (cli/index [temp-bam])) => anything
    (.exists (io/file (str temp-bam ".bai"))) => truthy
    ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about pileup"
    ;; NB: "pileup" output format may change in future (maybe)
    (with-out-file temp-out (cli/pileup [test-sorted-bam-file])) => anything
    ;(slurp temp-out) => (slurp "test-resources/t_cli.pileup")
    ))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (io/copy (io/file test-fa-file)
                                                 (io/file temp-out))))
                     (after  :facts (clean-cache!))]
  (fact "about faidx"
    (with-out-file temp-out (cli/faidx [temp-out])) => anything
    (.exists (io/file (str temp-out ".fai"))) => truthy))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (let [temp-dict (str temp-dir "/out.dict")]
    (fact "about dict"
      (with-out-file temp-out (cli/dict [test-fa-file temp-dict])) =future=> anything
      (.exists (io/file temp-dict)) =future=> truthy)))
