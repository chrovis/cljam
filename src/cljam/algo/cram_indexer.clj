(ns cljam.algo.cram-indexer
  (:require [cljam.io.crai :as crai]
            [cljam.io.cram.core :as cram.core]
            [cljam.io.cram.reader :as cram.reader]
            [cljam.io.sam.util.header :as sam.header]))

(defn create-index
  "Creates a CRAM index file from the given CRAM file.

  Takes the following options:
    - skip-sort-order-check?: By default, the CRAM indexer checks if the header
        is declared as `SO:coordinate` and raises an error if not.
        If this option is set to true, the CRAM indexer will skip the header check
        and create an index file regardless of the header declaration."
  [in-cram out-crai & {:keys [skip-sort-order-check?]}]
  (with-open [r (cram.core/reader in-cram {})
              w (crai/writer out-crai)]
    (let [header @(.-header r)]
      (when (and (not skip-sort-order-check?)
                 (not= (sam.header/sort-order header) sam.header/order-coordinate))
        (throw
         (ex-info "Cannot create CRAM index file for CRAM file not declared as sorted by coordinate"
                  {:sort-order (sam.header/sort-order header)})))
      (crai/write-index-entries w (cram.reader/generate-index-entries r)))))
