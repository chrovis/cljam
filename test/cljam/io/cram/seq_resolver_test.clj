(ns cljam.io.cram.seq-resolver-test
  (:require [cljam.io.cram.seq-resolver :as resolver]
            [cljam.io.cram.seq-resolver.protocol :as proto]
            [cljam.test-common :as common]
            [clojure.test :refer [are deftest is testing]]))

(deftest resolve-sequence-test
  (testing "seq resolver resolves sequence for specified region"
    (let [resolver (resolver/seq-resolver common/test-fa-file)
          resolver' (resolver/clone-seq-resolver resolver)]
      (are [?region ?expected]
           (= ?expected
              (proto/resolve-sequence resolver ?region)
              (proto/resolve-sequence resolver' ?region))
        {:chr "ref" :start 1 :end 5}
        "AGCAT"

        {:chr "unknown" :start 1 :end 5}
        nil)))
  (testing "resolver can be omitted, but in that case, trying to resolve seq will end up with error"
    (is (thrown? Exception (proto/resolve-sequence nil {:chr "ref" :start 1 :end 5})))))
