(ns cljam.io.wig-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.test-common :refer :all]
            [cljam.io.wig :as wig]
            [cljam.util :as util])
  (:import [java.io BufferedReader InputStreamReader ByteArrayInputStream
            ByteArrayOutputStream OutputStreamWriter BufferedWriter]
           [cljam.io.wig WIGReader WIGWriter]))

(defn- str->wig
  [^String s]
  (with-open [bais (ByteArrayInputStream. (.getBytes s))
              isr (InputStreamReader. bais)
              br (wig/WIGReader. (BufferedReader. isr) nil)]
    (doall (wig/read-fields br))))

(defn- wig->str
  [xs]
  (with-open [bao (ByteArrayOutputStream.)
              osw (OutputStreamWriter. bao)
              bw (wig/WIGWriter. (BufferedWriter. osw) nil)]
    (wig/write-fields bw xs)
    (.flush ^BufferedWriter (.writer bw))
    (.toString bao)))

(deftest reader
  (testing "read wig"
    (is (= (str->wig "variableStep chrom=chr1 span=10\n1 1\n100 2")
           [{:format :variable-step
             :chrom "chr1"
             :span 10
             :values [{:start 1 :value 1}
                      {:start 100 :value 2}]}]))
    (is (= (str->wig "fixedStep chrom=chr1 start=1 step=20 span=10\n1\n2")
           [{:format :fixed-step
             :chrom "chr1"
             :start 1
             :step 20
             :span 10
             :values [1 2]}])))

  (testing "make reader instance"
    (with-open [rdr (wig/reader test-wig-file1)]
      (is (instance? cljam.io.wig.WIGReader rdr))))

  (testing "throw Exception"
    (is (thrown? Exception (wig/reader "test-resources/wig/not-found.wig"))))

  (testing "read wig with default span value"
    (is (= (str->wig "variableStep chrom=chr1\n1 0\n100 1")
           [{:format :variable-step
             :chrom "chr1"
             :span 1
             :values [{:start 1 :value 0}
                      {:start 100 :value 1}]}]))
    (is (= (str->wig "fixedStep chrom=chr1 start=1 step=20\n1\n2")
           [{:format :fixed-step
             :chrom "chr1"
             :start 1
             :step 20
             :span 1
             :values [1 2]}])))

  (testing "read wig file"
    (with-open [r (wig/reader test-wig-file1)]
      (is (= (wig/read-fields r)
             [{:format :variable-step
               :chrom "chr19"
               :span 150
               :values [{:start 49304701 :value 10.0}
                        {:start 49304901 :value 12.5}
                        {:start 49305401 :value 15.0}
                        {:start 49305601 :value 17.5}
                        {:start 49305901 :value 20.0}
                        {:start 49306081 :value 17.5}
                        {:start 49306301 :value 15.0}
                        {:start 49306691 :value 12.5}
                        {:start 49307871 :value 10.0}]}
              {:format :fixed-step
               :chrom "chr19"
               :start 49307401
               :step 300
               :span 200
               :values [1000 900 800 700 600 500 400 300 200 100]}])))
    (with-open [r (wig/reader test-wig-file2)]
      (is (= (wig/read-fields r)
             [{:format :variable-step
               :chrom "chr19"
               :span 1
               :values [{:start 49304701 :value 10.0}]}
              {:format :fixed-step
               :chrom "chr19"
               :start 49307401
               :step 300
               :span 1
               :values [1000]}])))))

(deftest writer
  (testing "make writer instance"
    (with-open [wtr (wig/writer (.getAbsolutePath (cio/file util/temp-dir "temp.wig")))]
      (is (instance? cljam.io.wig.WIGWriter wtr))))

  (testing "write wig"
    (is (= (-> "variableStep chrom=chr1 span=10\n1 1\n100 2" str->wig wig->str)
           "variableStep chrom=chr1 span=10\n1 1\n100 2"))
    (is (= (-> "fixedStep chrom=chr1 start=1 step=20 span=10\n1\n2" str->wig wig->str)
           "fixedStep chrom=chr1 start=1 step=20 span=10\n1\n2")))

  (testing "write wig with default span value"
    (is (= (-> "variableStep chrom=chr1\n1 1\n100 2" str->wig wig->str)
           "variableStep chrom=chr1\n1 1\n100 2"))
    (is (= (-> "fixedStep chrom=chr1 start=1 step=20\n1\n2" str->wig wig->str)
           "fixedStep chrom=chr1 start=1 step=20\n1\n2"))))

(deftest source-type-test
  (let [temp-file (str temp-dir "test.wig")]
    (are [x] (with-before-after {:before (prepare-cache!)
                                 :after (clean-cache!)}
               (with-open [rdr (wig/reader test-wig-file1)
                           wtr (wig/writer temp-file)]
                 (not-throw? (wig/write-fields wtr (wig/read-fields rdr)))))
         temp-file
         (cio/file temp-file)
         (cio/as-url (cio/file temp-file)))))
