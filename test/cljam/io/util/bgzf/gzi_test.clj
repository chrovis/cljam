(ns cljam.io.util.bgzf.gzi-test
  (:require [clojure.test :refer [deftest is are]]
            [cljam.test-common :as common]
            [cljam.io.util.bgzf.gzi :as gzi]))

(def ^:private medium-fa-bgz-gzi
  (sorted-map
   0      0,
   65280  21483,
   130560 43102,
   195840 64763,
   261120 85797,
   326400 106765,
   391680 128189,
   456960 149697))

(deftest read-gzi
  (is (= medium-fa-bgz-gzi (gzi/read-gzi common/medium-fa-bgz-gzi-file))))

(deftest uncomp->comp
  (are [?uncomp ?comp]
       (= ?comp (gzi/uncomp->comp medium-fa-bgz-gzi ?uncomp))
    0 0
    1 1
    65279 65279
    65280 (bit-shift-left 21483 16)
    456961 (inc (bit-shift-left 149697 16))))

(deftest comp->uncomp
  (are [?comp ?uncomp]
       (= ?uncomp (gzi/comp->uncomp medium-fa-bgz-gzi ?comp))
    0 0
    1 1
    21482 21482
    65279 65279
    (bit-shift-left 21483 16) 65280
    (bit-shift-left 149697 16) 456960
    (inc (bit-shift-left 149697 16)) 456961))
