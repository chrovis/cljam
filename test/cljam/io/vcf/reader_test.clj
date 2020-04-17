(ns cljam.io.vcf.reader-test
  (:require [clojure.test :refer [deftest is]]
            [cljam.io.vcf.reader :as vcf-reader]))

(deftest parse-structured-line-test
  (is (= {:id "ID", :description "\"This\" is a description",
          :note "You can use \" in string fields by escaping it with \\"}
         (#'vcf-reader/parse-structured-line "ID=ID,Description=\"\\\"This\\\" is a description\",Note=\"You can use \\\" in string fields by escaping it with \\\\\"")))
  (is (thrown-with-msg? RuntimeException
                        #"Unexpected end of string field"
                        (#'vcf-reader/parse-structured-line "x=\"unbalanced double quote")))
  (is (thrown-with-msg? RuntimeException
                        #"Either '\\' or '\"' was expected immediately after '\\', but got 'x'"
                        (#'vcf-reader/parse-structured-line "x=\"unknown \\x escape sequence\""))))
