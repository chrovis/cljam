(ns cljam.t-dict
  "Tests for cljam.dict."
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [cljam.t-common :refer :all]
            [cljam.dict :as dict]))

(defn same-dict-file? [f1 f2]
  (with-open [r1 (io/reader f1)
              r2 (io/reader f2)]
    (let [dict1 (line-seq r1)
          dict2 (line-seq r2)
          ;; NB: `UR` is calculated by file path, ignore it
          omit-ur (fn [line]
                    (if-let [[_ r] (re-find #"^(.*)\tUR:" line)]
                      r
                      line))]
      (when (= (count dict1) (count dict2))
        (->> (map #(= (omit-ur %1) (omit-ur %2)) dict1 dict2)
             (filter identity)
             first
             boolean)))))

;; Resources
;; ---------

(def temp-fa-file (str temp-dir "/test.fa"))
(def out-dict-file (str temp-fa-file ".dict"))

;; Small-size FASTA
;; ---------------------------------

(deftest about-create-dict
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy test-fa-file temp-fa-file))
                      :after (clean-cache!)}
    ;; Create dictionary without errors
    (is (not-throw? (dict/create-dict temp-fa-file out-dict-file)))
    ;; Check the file contents
    (is (same-dict-file? out-dict-file test-fa-dict-file))))
