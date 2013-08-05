(ns cljam.t-sam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam])
  (:import cljam.sam.SamHeader))

(fact "about hd-header"
  (sam/hd-header test-sam) => nil?
  (sam/hd-header test-sam-sorted-by-pos) => (assoc (SamHeader.) :HD {:VN "1.4", :SO "coordinate"})
  (sam/hd-header test-sam-sorted-by-qname) => (assoc (SamHeader.) :HD {:VN "1.4", :SO "queryname"}))

(def refs '({:name "ref", :len "45"} {:name "ref2", :len "40"}))

(fact "about make-refs"
  (sam/make-refs test-sam) => refs)

(fact "about ref-id"
  (sam/ref-id refs "ref") => 0
  (sam/ref-id refs "ref2") => 1
  (sam/ref-id refs "notfound") => nil?)

(fact "about ref-name"
  (sam/ref-name refs 0) => "ref"
  (sam/ref-name refs 1) => "ref2"
  (sam/ref-name refs 9) => nil?)

(fact "about slurp-sam"
  (sam/slurp test-sam-file) => test-sam)

(with-state-changes [(before :facts (mk-temp-dir!))
                     (after  :facts (rm-temp-dir!))]
  (fact "about spit-sam"
    (let [temp-file (str temp-dir "/test.sam")]
     (sam/spit temp-file test-sam) => nil?
     (sam/slurp temp-file) => test-sam)))
