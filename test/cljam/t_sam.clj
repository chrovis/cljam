(ns cljam.t-sam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam])
  (:import cljam.sam.SamHeader))

(fact "about hd-header"
  (sam/hd-header test-sam) => nil?
  (sam/hd-header test-sam-sorted-by-pos) => (assoc (SamHeader.) :HD {:VN "1.4", :SO "coordinate"})
  (sam/hd-header test-sam-sorted-by-qname) => (assoc (SamHeader.) :HD {:VN "1.4", :SO "queryname"}))
