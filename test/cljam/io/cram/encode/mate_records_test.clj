(ns cljam.io.cram.encode.mate-records-test
  (:require [cljam.io.cram.encode.mate-records :as mate]
            [cljam.io.sam.util.flag :as flag]
            [clojure.test :refer [deftest is]]))

(deftest make-mate-resolver-test
  (let [mate-resolver (mate/make-mate-resolver)]
    (is (nil? (mate/resolve-mate! mate-resolver 1
                                  {:qname "q001"
                                   :flag (flag/encoded #{:properly-aligned :multiple
                                                         :first :next-reversed})})))
    (is (nil? (mate/resolve-mate! mate-resolver 2
                                  {:qname "q002"
                                   :flag (flag/encoded #{:multiple :first :next-unmapped})})))
    (is (nil? (mate/resolve-mate! mate-resolver 3
                                  {:qname "q003"
                                   :flag (flag/encoded #{:multiple :first :supplementary})})))
    (is (= 2 (mate/resolve-mate! mate-resolver 5
                                 {:qname "q002"
                                  :flag (flag/encoded #{:multiple :last :unmapped})})))
    (is (= 1 (mate/resolve-mate! mate-resolver 4
                                 {:qname "q001"
                                  :flag (flag/encoded #{:properly-aligned :multiple
                                                        :last :reversed})})))
    (is (nil? (mate/resolve-mate! mate-resolver 6
                                  {:qname "q003"
                                   :flag (flag/encoded #{:multiple :first :reversed})})))))
