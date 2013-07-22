(ns cljam.t-sorter
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sorter :as sorter]))

(fact "about sorting a sam by chromosomal positions"
  (sorter/sort-by-pos test-sam) => test-sam-sorted-by-pos)

(fact "about sorting a sam by read names"
  (sorter/sort-by-qname test-sam) => test-sam-sorted-by-qname)

(fact "about sorted?"
  (sorter/sorted? test-sam) => falsey
  (sorter/sorted? test-sam-sorted-by-pos) => truthy
  (sorter/sorted? test-sam-sorted-by-qname) => truthy)

(fact "about sort-order"
  (sorter/sort-order test-sam) => "unknown"
  (sorter/sort-order test-sam-sorted-by-pos) => "coordinate"
  (sorter/sort-order test-sam-sorted-by-qname) => "queryname")
