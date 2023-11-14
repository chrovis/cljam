(ns cljam.io.sam.util.validator-test
  (:require [clojure.test :refer [deftest is are testing]]
            [cljam.io.sam.util.validator :as validator]))

(deftest validate-option-test
  (testing "bad type"
    (is (= (#'validator/validate-option {:type "!" :value \!} :sam)
           ["Type ! is invalid"])))
  (testing "type A"
    (is (nil? (#'validator/validate-option {:type "A" :value \!} :sam)))
    (is (= (#'validator/validate-option {:type "A" :value 100} :sam)
           ["Must be a char [!-~]."])))
  (testing "type c bam"
    (is (nil? (#'validator/validate-option {:type "c" :value 10} :bam)))
    (is (= (#'validator/validate-option {:type "c" :value 300} :bam)
           ["Must be 8 bit signed integer."])))
  (testing "type i"
    (is (nil? (#'validator/validate-option {:type "i" :value 10} :sam)))
    (is (= (#'validator/validate-option {:type "i" :value "10"} :sam)
           ["Must be 32 bit signed integer."]))
    (is (= (#'validator/validate-option {:type "i" :value 100000000000} :sam)
           ["Must be 32 bit signed integer."])))
  (testing "type f"
    (is (nil? (#'validator/validate-option {:type "f" :value 10} :sam)))
    (is (nil? (#'validator/validate-option {:type "f" :value 10.1} :sam)))
    (is (= (#'validator/validate-option {:type "f" :value "A"} :sam)
           ["Must be a float."])))
  (testing "type Z"
    (is (nil? (#'validator/validate-option {:type "Z" :value "!@abc"} :sam)))
    (is (= (#'validator/validate-option {:type "Z" :value 10} :sam)
           ["Must be a printable string [ !-~]*"])))
  (testing "type H"
    (is (nil? (#'validator/validate-option {:type "H" :value [1,2]} :sam)))
    (is (= (#'validator/validate-option {:type "H" :value "A"} :sam)
           ["Must be a byte array."])))
  (testing "type B"
    (is (nil? (#'validator/validate-option
               {:type "B" :value "f,-0.3,0.0,0.3"}
               :sam)))
    (is (= (#'validator/validate-option {:type "B" :value "W"} :sam)
           ["Must be a string of comma-separated array of numbers."]))))

(deftest validate-data-record-test
  (let [validator (validator/make-validator {:SQ [{:SN "ref", :LN 45}]})
        valid-align
        {:rname "ref" :pos 10 :qname "a" :mapq 10 :cigar "16M"
         :rnext "*" :tlen 0 :pnext 0 :seq "ATGC" :qual "*"
         :options {}}]
    (are [k v ans]
         (= (get-in (#'validator/validate-data-record validator (assoc valid-align k v))
                    [:errors k])
            ans)
      :qname 100  ["Must be a string."]
      :qname (apply str (repeat 255 \a))
      ["Must be less than or equal to 254 characters."]

      :qname "@@" ["Must not contain illegal characters."]

      :qname (apply str (repeat 255 \@))
      ["Must not contain illegal characters."
       "Must be less than or equal to 254 characters."]

      :rname 10 ["Must be a string."]
      :rname "NOT-FOUND" ["Must be declared as the SN value in the SQ line within the header. (NOT-FOUND)"]
      :pos "ABC" ["Must be an integer."]
      :pos 100000000 ["Must be less than or equal to 45."]
      :pos 46 ["Must be less than or equal to 45."]
      :pos -100 ["Must be in the [0, 2147483647]."]
      :mapq "A" ["Must be an integer."]
      :mapq 300 ["Must be in the [0-255]."]
      :cigar 10 ["Must be a string."]
      :cigar "3Y" ["Invalid format."]
      :rname 10 ["Must be a string."]
      :pnext 100000000 ["Must be less than or equal to 45."]
      :pnext "A" ["Must be an integer."]
      :tlen -9900000000 ["Must be in the [-2147483647,2147483647]."]
      :qual 10 ["Must be a string."]
      :qual "bad qual" ["Must be composed only of ASCII characters within the valid phred33 range [!-~]."]
      :seq 100 ["Must be a string."]
      :seq [\A \B] ["Must be a string."]
      :seq "A!TGC" ["Must not contain bad character."])
    (is (= (get-in (#'validator/validate-data-record
                    validator
                    (assoc valid-align :options [{:type "!" :value \!}]))
                   [:errors [:options 0]])
           ["Type ! is invalid"]))))

(deftest check-alignment-test
  (is (thrown? clojure.lang.ExceptionInfo
               (doall (validator/check-alignments
                       (validator/make-validator {:SQ [{:SN "ref", :LN 45}]})
                       [{:rname "ref" :pos 10000000
                         :qname "a"
                         :mapq 10 :cigar "16M" :rnext "*"
                         :tlen 0 :pnext 0 :seq "ATGC"
                         :qual "*" :options {}}]))))
  (let [input [{:rname "ref" :pos 10 :qname "a" :mapq 10 :cigar "16M"
                :rnext "*" :tlen 0 :pnext 0 :seq "ATGC" :qual "*"
                :options {}}]]
    (is (= (validator/check-alignments (validator/make-validator {:SQ [{:SN "ref", :LN 45}]}) input)
           input))))
