(ns cljam.io.sam.util.header-test
  (:require [clojure.test :refer :all]
            [cljam.io.sam.util.header :as header]))

(deftest into-rf
  (let [rf-vec (#'header/into-rf [])
        rf-seq (#'header/into-rf nil)]
    (is (= (-> (rf-vec)
               (rf-vec \a)
               (rf-vec \b)
               (rf-vec \c)
               (rf-vec))
           [\a \b \c]))
    (is (= (-> (rf-seq)
               (rf-seq \a)
               (rf-seq \b)
               (rf-seq \c)
               (rf-seq))
           '(\c \b \a)))))

(deftest about-parse-header
  (is (= (header/parse-header "@HD	VN:1.3	SO:coordinate\n@SQ	SN:ref	LN:10\n@SQ	SN:ref2	LN:20\n@PG	ID:cljam	PN:cljam	VN:1.0	CL:java -jar cljam.jar")
         {:HD {:VN "1.3" :SO "coordinate"}
          :SQ [{:SN "ref" :LN 10} {:SN "ref2" :LN 20}]
          :PG [{:ID "cljam" :PN "cljam" :VN "1.0" :CL "java -jar cljam.jar"}]}))
  (is (= (header/parse-header "@HD\tVN:1.4\tSO:coordinate\n@SQ\tSN:FOO:BAR:10-100;BAZ|QUX,QUUX\tLN:1\n@PG\tID:cljam\tPN:cljam\tVN:1.0\tCL:cljam")
         {:HD {:VN "1.4" :SO "coordinate"}
          :SQ [{:SN "FOO:BAR:10-100;BAZ|QUX,QUUX"
                :LN 1}]
          :PG [{:ID "cljam" :PN "cljam" :VN "1.0" :CL "cljam"}]})))
