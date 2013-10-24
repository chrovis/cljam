(ns cljam.t-sorter
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam]
            [cljam.sorter :as sorter]))

(def tmp-sorted-sam-file (str temp-dir "tmp.sorted.sam"))

(fact "about sorting a sam by chromosomal positions"
      (sorter/sort-by-pos (sam/reader test-sam-file)
                          (sam/writer tmp-sorted-sam-file)) => nil)

;; FIXME: failed
;; (fact "about sorting a sam by read names"
;;   (sorter/sort-by-qname test-sam) => test-sam-sorted-by-qname)

;; TODO: fix
;; (fact "about sorted?"
;;       (sorter/sorted? (sam/reader test-sam-file)) => falsey
;;       (sorter/sorted? test-sam-sorted-by-pos) => truthy
;;       (sorter/sorted? test-sam-sorted-by-qname) => truthy)

;; TODO: fix
;; (fact "about sort-order"
;;   (sorter/sort-order test-sam) => "unknown"
;;   (sorter/sort-order test-sam-sorted-by-pos) => "coordinate"
;;   (sorter/sort-order test-sam-sorted-by-qname) => "queryname")
