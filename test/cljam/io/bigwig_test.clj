(ns cljam.io.bigwig-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.bigwig :as bigwig]
            [cljam.util :as util])
  (:import [cljam.io.bigwig BIGWIGReader BigWigStructure FixedWidthHeader
            ZoomHeader TotalSummary ExtendedHeader BptHeader Chrom]))

(def ^:private ^BigWigStructure test-bigwig-fixed-fields
  [(BigWigStructure.
    (FixedWidthHeader. 2291137574 4 2 152 200 368 112 32768 0)
    [(ZoomHeader. 132 6588 6629) (ZoomHeader. 528 12833 12874)]
    (TotalSummary. 110 0.5 79.6 4045.0 188969.5)
    nil
    (BptHeader. 1 4 8 1 184))
   [(Chrom. "chr7" 0 158821424)]])

(def ^:private ^BigWigStructure test-bigwig-variable-fields
  [(BigWigStructure.
    (FixedWidthHeader. 2291137574 4 2 152 214 419 112 32768 0)
    [(ZoomHeader. 10428 6631 6706) (ZoomHeader. 41712 12910 12986)]
    (TotalSummary. 1050 -0.39 0.78 3.5 78.515)
    nil
    (BptHeader. 2 5 8 2 184))
   [(Chrom. "chr22" 0 49691432)
    (Chrom. "chrY" 1 57772954)]])

(def ^:private ^BigWigStructure test-bigwig-bedgraph-fields
  [(BigWigStructure.
    (FixedWidthHeader. 2291137574 4 2 152 212 500 112 32768 0)
    [(ZoomHeader. 2462838 6712 6787) (ZoomHeader. 9851352 12991 13067)]
    (TotalSummary. 17002 14.6 25.1 321574.0 6240134.25)
    nil
    (BptHeader. 2 4 8 2 184))
   [(Chrom. "chr5" 0 180857866)
    (Chrom. "chr7" 1 158821424)]])

(defn- same-headers?
  [ah bh]
  (and (= (:fixed-width-header ah) (:fixed-width-header bh))
       (= (:zoom-headers ah) (:zoom-headers bh))
       (= (:auto-sql ah) (:auto-sql bh))
       (= (:extended-header ah) (:extended-header bh))
       (= (:bpt-header ah) (:bpt-header bh))
       (let [eps 1e-4
             x (:total-summary ah)
             y (:total-summary bh)]
         (and (= (:bases-covered x) (:bases-covered y))
              (-> (- (:min-val x) (:min-val y)) Math/abs (< eps))
              (-> (- (:max-val x) (:max-val y)) Math/abs (< eps))
              (-> (- (:sum-data x) (:sum-data y)) Math/abs (< eps))
              (-> (- (:sum-squared x) (:sum-squared y)) Math/abs (< eps))))))

(defn- same-chroms?
  [ac bc]
  (and (= (:name ac) (:name bc))
       (= (:id ac) (:id bc))
       (= (:size ac) (:size bc))))

(defn- same-bigwig-structure?
  "Returns true if the given arguments are same bigWig structure, otherwise
  false."
  [[ah ac] [bh bc]]
  (and (same-headers? ah bh)
       (same-chroms? ac bc)))

(deftest reader
  (testing "make reader instance"
    (with-open [rdr (bigwig/reader test-bigwig-variable-file)]
      (is (instance? cljam.io.bigwig.BIGWIGReader rdr))))

  (testing "throw Exception"
    (is (thrown? Exception (bigwig/reader "test-resources/bigwig/not-found.bigWig"))))

  (testing "read bigWig file"
    (with-open [r (bigwig/reader test-bigwig-fixed-file)]
      (is (same-bigwig-structure? (bigwig/read-structure r)
                                  test-bigwig-fixed-fields)))
    (with-open [r (bigwig/reader test-bigwig-variable-file)]
      (is (same-bigwig-structure? (bigwig/read-structure r)
                                  test-bigwig-variable-fields)))
    (with-open [r (bigwig/reader test-bigwig-bedgraph-file)]
      (is (same-bigwig-structure? (bigwig/read-structure r)
                                  test-bigwig-bedgraph-fields)))))

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [x] (with-open [rdr (bigwig/reader x)]
                 (same-bigwig-structure? (bigwig/read-structure rdr)
                                         test-bigwig-fixed-fields))
        test-bigwig-fixed-file
        (cio/file test-bigwig-fixed-file)
        (cio/as-url (cio/file test-bigwig-fixed-file))))))
