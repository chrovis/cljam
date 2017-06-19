(ns cljam.algo.t-dict
  "Tests for cljam.algo.dict."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as cio]
            [cljam.t-common :refer :all]
            [cljam.algo.dict :as dict]))

(defn same-dict-file? [f1 f2]
  (with-open [r1 (cio/reader f1)
              r2 (cio/reader f2)]
    (let [dict1 (line-seq r1)
          dict2 (line-seq r2)
          ;; NB: `UR` is calculated by local file path, ignore it,
          ;;     and ignore `VN` too.
          omit-some (fn [line]
                      (string/join "\t"
                                   (remove #(re-find #"^(UR|VN):" %)
                                           (string/split line #"\t"))))]
      (when (= (count dict1) (count dict2))
        (->> (map #(= (omit-some %1) (omit-some %2)) dict1 dict2)
             (every? true?))))))

;; Resources
;; ---------

(def temp-fa-file (str temp-dir "/test.fa"))
(def out-dict-file (str temp-fa-file ".dict"))

;; Small-size FASTA
;; ---------------------------------

(deftest about-create-dict
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file test-fa-file)
                                            (cio/file temp-fa-file)))
                      :after (clean-cache!)}
    ;; Create dictionary without errors
    (is (not-throw? (dict/create-dict temp-fa-file out-dict-file)))
    ;; Check the file contents
    (is (same-dict-file? out-dict-file test-fa-dict-file))))
