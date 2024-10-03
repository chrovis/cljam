(ns cljam.algo.cram-indexer-test
  (:require [cljam.algo.cram-indexer :as indexer]
            [cljam.io.crai :as crai]
            [cljam.io.cram :as cram]
            [cljam.test-common :as common]
            [cljam.util :as util]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn- read-index-entries [f]
  (with-open [r (io/reader (util/compressor-input-stream f))]
    (doall (#'crai/read-index-entries r))))

(deftest create-index-test
  (let [f (io/file common/temp-dir "medium_without_index.cram.crai")]
    (common/with-before-after {:before (common/prepare-cache!)
                               :after (common/clean-cache!)}
      (is (thrown-with-msg? Exception #"Cannot create CRAM index file .*"
                            (indexer/create-index common/medium-without-index-cram-file f)))
      (indexer/create-index common/medium-cram-file f
                            :skip-sort-order-check? true)
      (is (= (read-index-entries common/medium-crai-file)
             (read-index-entries f))))))

(deftest cram-without-alignments-test
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (let [header {:HD {:SO "coordinate"}
                  :SQ [{:SN "chr1", :LN 100}]}
          target (io/file common/temp-dir "no_aln.cram")
          target-crai (io/file common/temp-dir "no_aln.cram.crai")]
      (with-open [w (cram/writer target)]
        (cram/write-header w header)
        (cram/write-refs w header))
      (is (common/not-throw? (indexer/create-index target target-crai))))))
