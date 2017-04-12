(ns cljam.t-cli
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :as io]
            [cljam.cli :as cli])
  (:import [java.io PrintStream]))

(defmacro with-out-file
  [f & body]
  `(let [os# (clojure.java.io/output-stream ~f)
         old-ps# System/out
         ps# (PrintStream. os#)]
     (try
       (System/setOut ps#)
       (binding [*out* (clojure.java.io/writer os#)]
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
    (is (not-throw? (with-out-file temp-out (cli/normalize [test-bam-file temp-bam]))))
    ;; TODO: examine result
    ))

(deftest about-sort-by-pos
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; see https://gitlab.xcoo.jp/chrovis/cljam/issues/12
    ;; (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "coordinate" test-sam-file temp-sam])))) ; TODO: future
    ;; (is (= (slurp-sam-for-test temp-sam) test-sam-sorted-by-pos)) ; TODO: future
    ;; (is (not-throw? (check-sort-order (slurp-sam-for-test temp-sam)
    ;;                                   test-sam-sorted-by-pos))) ; TODO: future
    (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "coordinate" test-bam-file temp-bam]))))
    (is (= (slurp-bam-for-test temp-bam) test-sam-sorted-by-pos))
    (is (not-throw? (check-sort-order (slurp-bam-for-test temp-bam)
                                      test-sam-sorted-by-pos)))
    (is (thrown? IllegalArgumentException (with-out-file temp-out (cli/sort ["-o" "coordinate" test-fa-file temp-bam]))))
    (is (thrown? IllegalArgumentException (with-out-file temp-out (cli/sort ["-o" "coordinate" test-bam-file (str temp-dir "/test.unknown")]))))))

(deftest about-sort-by-qname
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "queryname" test-sam-file temp-sam])))) ; TODO: future
    ;; (is (= (slurp-sam-for-test temp-sam) test-sam-sorted-by-qname)) ; TODO: future
    ;; (is (not-throw? (with-out-file temp-out (cli/sort ["-o" "queryname" test-bam-file temp-bam])))) ; TODO: future
    ;; (is (= (slurp-bam-for-test temp-bam) test-sam-sorted-by-qname)) ; TODO: future
    ))

(deftest about-index
  (with-before-after {:before (do (prepare-cache!)
                                  (io/copy (io/file test-sorted-bam-file)
                                           (io/file temp-bam)))
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/index [temp-bam]))))
    (is (.exists (io/file (str temp-bam ".bai"))))
    (is (not-throw? (with-out-file temp-out (cli/index ["-t" "1" temp-bam]))))
    (is (not-throw? (with-out-file temp-out (cli/index ["-t" "4" temp-bam]))))))

(deftest about-pileup
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; NB: "pileup" output format may change in future (maybe)
    (is (not-throw? (with-out-file temp-out (cli/pileup [test-sorted-bam-file]))))
    ;; (is (= (slurp temp-out) (slurp "test-resources/t_cli.pileup")))
    (are [?args] (not-throw? (with-out-file temp-out (cli/pileup ?args)))
      ["-t" "1" "-s" test-sorted-bam-file]
      ["-t" "1" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "1" "-s" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "1" "-r" "ref2:10-200" test-sorted-bam-file]
      ["-t" "1" "-r" "ref2:10-200" "-s" test-sorted-bam-file]
      ["-t" "1" "-r" "ref2:10-200" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "1" "-r" "ref2:10-200" "-s" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "1" "-r" "ref2" test-sorted-bam-file]
      ["-t" "1" "-r" "ref2" "-s" test-sorted-bam-file]
      ["-t" "1" "-r" "ref2" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "1" "-r" "ref2" "-s" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "4" "-s" test-sorted-bam-file]
      ["-t" "4" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "4" "-s" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "4" "-r" "ref2:10-200" test-sorted-bam-file]
      ["-t" "4" "-r" "ref2:10-200" "-s" test-sorted-bam-file]
      ["-t" "4" "-r" "ref2:10-200" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "4" "-r" "ref2:10-200" "-s" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "4" "-r" "ref2" test-sorted-bam-file]
      ["-t" "4" "-r" "ref2" "-s" test-sorted-bam-file]
      ["-t" "4" "-r" "ref2" "-f" test-fa-file test-sorted-bam-file]
      ["-t" "4" "-r" "ref2" "-s" "-f" test-fa-file test-sorted-bam-file])))

(deftest about-faidx
  (with-before-after {:before (do (prepare-cache!)
                                  (io/copy (io/file test-fa-file)
                                           (io/file temp-out)))
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/faidx [temp-out]))))
    (is (.exists (io/file (str temp-out ".fai"))))))

(deftest about-dict
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [temp-dict (str temp-dir "/out.dict")]
      (is (not-throw? (with-out-file temp-out (cli/dict [test-fa-file temp-dict]))))
      (is (.exists (io/file temp-dict))))))

(deftest about-level
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? clojure.lang.ExceptionInfo
                 (with-out-file temp-out (cli/level [test-bam-file temp-bam]))))
    (is (not-throw? (with-out-file temp-out (cli/level [test-sorted-bam-file
                                                        temp-bam]))))
    ;; TODO: examine contents of temp-bam
    ))

(deftest about-run
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (with-out-file temp-out (cli/run ["view" test-sam-file]))))))
