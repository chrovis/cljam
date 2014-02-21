(ns cljam.sequence
  (:require [cljam.cigar :as cgr]))

(defn- parse-seq [seq* cigar]
  (when (seq cigar)
   (let [[[n op] & rest-cigar] cigar
         [ret rest-seq] (condp #(not (nil? (%1 %2))) op
                          #{\M \= \X} [{:n n, :op op, :seq (vec (take n seq*))} (drop n seq*)]
                          #{\D}       [{:n n, :op op, :seq (vec (repeat n \*))} seq*]
                          #{\N}       [{:n n, :op op, :seq (vec (repeat n \>))} seq*]
                          #{\S \I}    [{:n n, :op op, :seq (vec (take n seq*))} (drop n seq*)]
                          #{\H}       [{:n n, :op op, :seq nil} seq*]
                          #{\P}       [{:n n, :op op, :seq nil} seq*])]
     (cons ret (parse-seq rest-seq rest-cigar)))))

(defn parse [seq* cigar]
  (parse-seq seq* (cgr/parse cigar)))
