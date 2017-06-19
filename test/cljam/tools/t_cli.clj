(ns cljam.tools.t-cli
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :as cio]
            [cljam.tools.cli :as cli]
            [cljam.io :as io]
            [cljam.io.bam :as bam])
  (:import [java.io PrintStream]))

(defmacro with-out-file
  [f & body]
  `(let [os# (cio/output-stream ~f)
         old-ps# System/out
         ps# (PrintStream. os#)]
     (try
       (System/setOut ps#)
       (binding [*out* (cio/writer os#)]
         ~@body)
       (finally
         (.flush ps#)
         (System/setOut old-ps#)))))

(def temp-out (str temp-dir "/out"))
(def temp-bam (str temp-dir "/out.bam"))
(def temp-sam (str temp-dir "/out.sam"))

(deftest about-view
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; NB: "view" output format may change in future
    (is (not-throw? (with-out-file temp-out (cli/view [test-sam-file]))))
    (is (not-throw? (with-out-file temp-out (cli/view ["-f" "sam" test-sam-file]))))
    (is (not-throw? (with-out-file temp-out (cli/view [test-bam-file]))))
    (is (not-throw? (with-out-file temp-out (cli/view ["-f" "bam" test-bam-file]))))
    (is (not-throw? (with-out-file temp-out (cli/view ["--header" test-bam-file]))))))

(deftest about-convert
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; sam => bam
    (is (not-throw? (cli/convert [test-sam-file temp-bam])))
    (is (= (slurp-bam-for-test temp-bam) (slurp-sam-for-test test-sam-file)))
    (is (= (slurp-bam-for-test temp-bam) (slurp-bam-for-test test-bam-file)))
    ;; bam => sam
    (is (not-throw? (cli/convert [test-bam-file temp-sam])))
    (is (= (slurp-sam-for-test temp-sam) (slurp-bam-for-test test-bam-file)))
    (is (= (slurp-sam-for-test temp-sam) (slurp-sam-for-test test-sam-file)))
    ;; error
    (is (thrown? IllegalArgumentException (cli/convert [test-bam-file (str temp-dir "/test.unknown")])))))

(deftest about-normalize
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/normalize [normalize-before-bam-file temp-bam]))))
    (is (same-bam-file? temp-bam normalize-after-bam-file))))

(deftest about-sort-by-pos
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "coordinate" test-sam-file temp-sam]))))
    (is (= (slurp-sam-for-test temp-sam) test-sam-sorted-by-pos))
    (is (not-throw? (check-sort-order (slurp-sam-for-test temp-sam) test-sam-sorted-by-pos)))
    (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "coordinate" "-c" "4" test-bam-file temp-bam]))))
    (is (= (slurp-bam-for-test temp-bam) test-sam-sorted-by-pos))
    (is (not-throw? (check-sort-order (slurp-bam-for-test temp-bam) test-sam-sorted-by-pos)))
    (is (thrown? IllegalArgumentException (with-out-file temp-out (cli/sort ["-o" "coordinate" test-fa-file temp-bam]))))
    (is (thrown? IllegalArgumentException (with-out-file temp-out (cli/sort ["-o" "coordinate" test-bam-file (str temp-dir "/test.unknown")]))))))

(deftest about-sort-by-qname
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "queryname" test-sam-file temp-sam]))))
    (is (= (slurp-sam-for-test temp-sam) test-sam-sorted-by-qname))
    (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "queryname" test-bam-file temp-bam]))))
    (is (= (slurp-bam-for-test temp-bam) test-sam-sorted-by-qname))))

(deftest about-index
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file test-sorted-bam-file)
                                            (cio/file temp-bam)))
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/index [temp-bam]))))
    (is (.exists (cio/file (str temp-bam ".bai"))))
    (is (not-throw? (with-out-file temp-out (cli/index ["-t" "1" temp-bam]))))
    (is (not-throw? (with-out-file temp-out (cli/index ["-t" "4" temp-bam]))))))

(deftest about-pileup
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; NB: "pileup" output format may change in future (maybe)
    (is (not-throw? (with-out-file temp-out (cli/pileup [test-sorted-bam-file]))))
    (is (= (slurp temp-out) (slurp test-pileup-file)))
    (let [bam-file test-sorted-bam-file]
      (doseq [t ["1" "4"]]
        (are [?args ?file] (= (do
                                (with-out-file temp-out (cli/pileup ?args))
                                (slurp temp-out))
                              (slurp (str test-pileup-dir ?file)))
          ["-t" t "-s" bam-file] "s.pileup"
          ["-t" t "-f" test-fa-file bam-file] "f.pileup"
          ["-t" t "-s" "-f" test-fa-file bam-file] "sf.pileup"
          ["-t" t "-r" "ref2" bam-file] "r1.pileup"
          ["-t" t "-r" "ref2" "-s" bam-file] "r1s.pileup"
          ["-t" t "-r" "ref2" "-f" test-fa-file bam-file] "r1f.pileup"
          ["-t" t "-r" "ref2" "-s" "-f" test-fa-file bam-file] "r1sf.pileup"
          ["-t" t "-r" "ref2:10-200" bam-file] "r2.pileup"
          ["-t" t "-r" "ref2:10-200" "-s" bam-file] "r2s.pileup"
          ["-t" t "-r" "ref2:10-200" "-f" test-fa-file bam-file] "r2f.pileup"
          ["-t" t "-r" "ref2:10-200" "-s" "-f" test-fa-file bam-file] "r2sf.pileup")))))

(deftest about-faidx
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file test-fa-file)
                                            (cio/file temp-out)))
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/faidx [temp-out]))))
    (is (.exists (cio/file (str temp-out ".fai"))))))

(deftest about-dict
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-dict (str temp-dir "/out.dict")]
      (is (not-throw? (with-out-file temp-out (cli/dict [test-fa-file temp-dict]))))
      (is (.exists (cio/file temp-dict))))))

(deftest about-level
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? clojure.lang.ExceptionInfo
                 (with-out-file temp-out (cli/level [test-bam-file temp-bam]))))
    (is (not-throw? (with-out-file temp-out (cli/level [test-sorted-bam-file
                                                        temp-bam]))))
    (with-open [rdr (bam/reader temp-bam :ignore-index true)]
      (is (= (map #(first (keep :LV (:options %)))
                  (io/read-alignments rdr))
             test-sorted-bam-levels)))))

(deftest about-run
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/run ["view" test-sam-file]))))))
