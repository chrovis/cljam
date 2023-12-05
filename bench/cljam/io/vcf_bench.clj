(ns cljam.io.vcf-bench
  (:require [cljam.io.vcf :as vcf]
            [cljam.test-common :as tcommon]
            [libra.bench :refer [are defbench]]
            [libra.criterium :as c]))

(defbench decode-small-bcf-bench
  (are [f]
       (c/quick-bench
        (with-open [r (vcf/reader f)]
          (run! (constantly nil) (vcf/read-variants r))))
    tcommon/test-bcf-complex-file))

(defbench decode-large-bcf-bench
  (are [f]
       (c/quick-bench
        (with-open [r (vcf/reader f)]
          (run! (constantly nil) (vcf/read-variants-randomly r {:chr "chr1" :end 30000000} {}))))
    tcommon/test-large-bcf-file))
