(ns cljam.io.vcf-bench
  (:require [cljam.io.vcf :as vcf]
            [cljam.test-common :as tcommon]
            [cljam.util :as util]
            [clojure.java.io :as cio]
            [libra.bench :refer [are defbench]]
            [libra.criterium :as c]))

(defbench encode-variant-small-bcf-bench
  (are [f]
       (util/with-temp-dir [d "encode-variant-small-bench"]
         (with-open [r (vcf/reader f)
                     w (vcf/writer (cio/file d "out.bcf")
                                   (vcf/meta-info r)
                                   (vcf/header r))]
           (let [vs (vec (vcf/read-variants r))]
             (c/quick-bench
              (vcf/write-variants w vs)))))
    tcommon/test-bcf-complex-file))

(defbench encode-variant-large-bcf-bench
  (tcommon/prepare-cavia!)
  (are [f]
       (util/with-temp-dir [d "encode-variant-large-bench"]
         (with-open [r (vcf/reader f)
                     w (vcf/writer (cio/file d "out.bcf")
                                   (vcf/meta-info r)
                                   (vcf/header r))]
           (let [vs (vec (vcf/read-variants-randomly r {:chr "chr1" :end 30000000} {}))]
             (c/quick-bench
              (vcf/write-variants w vs)))))
    tcommon/test-large-bcf-file))

(defbench decode-variant-small-bcf-bench
  (are [f]
       (c/quick-bench
        (with-open [r (vcf/reader f)]
          (run! (constantly nil) (vcf/read-variants r))))
    tcommon/test-bcf-complex-file))

(defbench decode-variant-large-bcf-bench
  (tcommon/prepare-cavia!)
  (are [f]
       (c/quick-bench
        (with-open [r (vcf/reader f)]
          (run! (constantly nil) (vcf/read-variants-randomly r {:chr "chr1" :end 30000000} {}))))
    tcommon/test-large-bcf-file))
