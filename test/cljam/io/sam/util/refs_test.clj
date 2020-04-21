(ns cljam.io.sam.util.refs-test
  (:require [clojure.test :refer [deftest is are]]
            [cljam.test-common :refer [test-sam]]
            [cljam.io.sam.util.refs :as refs]))

;; Reference functions
;; -------------------

(def refs '({:name "ref", :len 45} {:name "ref2", :len 40}))

(deftest make-refs
  (is (= (refs/make-refs (:header test-sam)) refs)))

(deftest ref-id
  (are [?name ?expected] (= (refs/ref-id refs ?name) ?expected)
    "ref"      0
    "ref2"     1
    "notfound" nil))

(deftest ref-name
  (are [?id ?expected] (= (refs/ref-name refs ?id) ?expected)
    0 "ref"
    1 "ref2"
    9 nil))

(deftest ref-by-name
  (are [?name ?expected] (= (refs/ref-by-name refs ?name) ?expected)
    "ref"      {:name "ref", :len 45}
    "ref2"     {:name "ref2", :len 40}
    "notfound" nil))
