(ns cljam.io.cram.seq-resolver-test
  (:require [cljam.io.cram.seq-resolver :as resolver]
            [cljam.io.cram.seq-resolver.protocol :as proto]
            [cljam.io.sequence :as cseq]
            [cljam.test-common :as common]
            [clojure.test :refer [deftest is testing]]))

(deftest resolve-sequence-test
  (with-open [resolver (resolver/seq-resolver common/test-fa-file)
              resolver' (resolver/clone-seq-resolver resolver)]
    (testing "seq resolver resolves sequence for chr"
      (is (= (with-open [fasta-rdr (cseq/reader common/test-fa-file)]
               (cseq/read-sequence fasta-rdr {:chr "ref"}))
             (String. (proto/resolve-sequence resolver "ref"))
             (String. (proto/resolve-sequence resolver' "ref"))))
      (is (= nil
             (proto/resolve-sequence resolver "unknown")
             (proto/resolve-sequence resolver' "unknown"))))
    (testing "seq resolver resolves sequence for specified region"
      (is (= "AGCAT"
             (String. (proto/resolve-sequence resolver "ref" 1 5))
             (String. (proto/resolve-sequence resolver' "ref" 1 5))))
      (is (= nil
             (proto/resolve-sequence resolver "unknown" 1 5)
             (proto/resolve-sequence resolver' "unknown" 1 5)))))
  (testing "resolver can be omitted, but in that case, trying to resolve seq will end up with error"
    (is (thrown? Exception (proto/resolve-sequence nil "ref")))
    (is (thrown? Exception (proto/resolve-sequence nil "ref" 1 5)))))

(deftest cached-seq-resolver-test
  (with-open [resolver (resolver/seq-resolver common/test-fa-file)
              resolver' (resolver/cached-resolver resolver)]
    (testing "cached seq resolver returns equivalent sequences as the original seq resolver does"
      (is (= (String. (proto/resolve-sequence resolver "ref"))
             (String. (proto/resolve-sequence resolver' "ref"))))
      (is (= (String. (proto/resolve-sequence resolver "ref" 5 10))
             (String. (proto/resolve-sequence resolver' "ref" 5 10))))
      (is (= (proto/resolve-sequence resolver "unknown")
             (proto/resolve-sequence resolver' "unknown")))
      (is (= (proto/resolve-sequence resolver "unknown" 5 10)
             (proto/resolve-sequence resolver' "unknown" 5 10))))
    (testing "cached seq resolver returns the identical sequence for the cached chr"
      (is (identical? (proto/resolve-sequence resolver' "ref")
                      (proto/resolve-sequence resolver' "ref"))))))
