(ns cljam.t-cli
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :as io]
            [cljam.cli :as cli]))

(defmacro with-out-file
  [f & body]
  `(binding [*out* (clojure.java.io/writer ~f)]
     ~@body))

(def temp-out (str temp-dir "/out"))
(def temp-bam (str temp-dir "/out.bam"))
(def temp-sam (str temp-dir "/out.sam"))

(deftest about-view
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; NB: "view" output format may change in future
    (is (not-throw? (with-out-file temp-out (cli/view [test-sam-file]))))
    (is (not-throw? (with-out-file temp-out (cli/view [test-bam-file]))))))

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
    (is (= (slurp-sam-for-test temp-sam) (slurp-sam-for-test test-sam-file)))))

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
                                      test-sam-sorted-by-pos)))))

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
    (is (.exists (io/file (str temp-bam ".bai"))))))

(deftest about-pileup
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; NB: "pileup" output format may change in future (maybe)
    (is (not-throw? (with-out-file temp-out (cli/pileup [test-sorted-bam-file]))))
    ;(is (= (slurp temp-out) (slurp "test-resources/t_cli.pileup")))
    ))

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
