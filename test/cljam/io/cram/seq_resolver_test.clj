(ns cljam.io.cram.seq-resolver-test
  (:require [cljam.io.cram.seq-resolver :as resolver]
            [cljam.io.cram.seq-resolver.protocol :as proto]
            [cljam.test-common :as common]
            [clojure.test :refer [deftest is testing]]))

(deftest resolve-sequence-test
  (testing "seq resolver resolves sequence for specified region"
    (let [resolver (resolver/seq-resolver common/test-fa-file)
          resolver' (resolver/clone-seq-resolver resolver)]
      (is (= "AGCAT"
             (String. (proto/resolve-sequence resolver "ref" 1 5))
             (String. (proto/resolve-sequence resolver' "ref" 1 5))))
      (is (= nil
             (proto/resolve-sequence resolver "unknown" 1 5)
             (proto/resolve-sequence resolver' "unknown" 1 5)))))
  (testing "resolver can be omitted, but in that case, trying to resolve seq will end up with error"
    (is (thrown? Exception (proto/resolve-sequence nil "ref" 1 5)))))
