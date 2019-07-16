(ns cljam.io.fastq-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [clojure.java.io :as cio]
            [cljam.io.fastq :as fq]))

(deftest fastq-file-input
  (is (= (with-open [r (fq/reader test-fq-file)]
           (doall (map (partial into {}) (fq/read-sequences r))))
         test-fq-sequences))
  (is (= (with-open [r (fq/reader test-fq-file)]
           (doall (map (partial into {})
                       (fq/read-sequences r {:decode-quality :phred33}))))
         test-fq-sequences))
  (is (= (with-open [r (fq/reader test-fq-file)]
           (doall (map (partial into {})
                       (fq/read-sequences r {:decode-quality nil}))))
         test-fq-sequences-raw))
  (is (thrown? AssertionError
               (with-open [r (fq/reader test-fq-file)]
                 (doall (map (partial into {})
                             (fq/read-sequences r {:decode-quality :phred64}))))))
  (is (= (with-open [r (fq/reader test-fq-gz-file)]
           (doall (map (partial into {}) (fq/read-sequences r))))
         test-fq-sequences))
  (is (= (with-open [r (fq/reader test-fq-bz2-file)]
           (doall (map (partial into {}) (fq/read-sequences r))))
         test-fq-sequences)))

(deftest fastq-file-output
  (with-before-after {:before (do (clean-cache!)
                                  (prepare-cache!))
                      :after (clean-cache!)}
    (is (= (let [path (.getAbsolutePath (cio/file temp-dir "test.fq"))]
             (with-open [w (fq/writer path)]
               (fq/write-sequences w test-fq-sequences))
             (with-open [r (fq/reader path)]
               (doall (map (partial into {}) (fq/read-sequences r)))))
           test-fq-sequences))

    (is (= (let [path (.getAbsolutePath (cio/file temp-dir "test-33.fq"))]
             (with-open [w (fq/writer path)]
               (fq/write-sequences w test-fq-sequences {:encode-quality :phred33}))
             (with-open [r (fq/reader path)]
               (doall (map (partial into {}) (fq/read-sequences r)))))
           test-fq-sequences))

    (is (= (let [path (.getAbsolutePath (cio/file temp-dir "test-raw.fq"))]
             (with-open [w (fq/writer path)]
               (fq/write-sequences w test-fq-sequences-raw {:encode-quality nil}))
             (with-open [r (fq/reader path)]
               (doall (map (partial into {}) (fq/read-sequences r)))))
           test-fq-sequences))

    (is (thrown? AssertionError
                 (let [path (.getAbsolutePath (cio/file temp-dir "test-64.fq"))]
                   (with-open [w (fq/writer path)]
                     (fq/write-sequences w test-fq-sequences {:encode-quality :phred64}))
                   (with-open [r (fq/reader path)]
                     (doall (map (partial into {}) (fq/read-sequences r)))))))

    (is (= (let [path (.getAbsolutePath (cio/file temp-dir "test.fq.gz"))]
             (with-open [w (fq/writer path)]
               (fq/write-sequences w test-fq-sequences))
             (with-open [r (fq/reader path)]
               (doall (map (partial into {}) (fq/read-sequences r)))))
           test-fq-sequences))

    (is (= (let [path (.getAbsolutePath (cio/file temp-dir "test.fq.bz2"))]
             (with-open [w (fq/writer path)]
               (fq/write-sequences w test-fq-sequences))
             (with-open [r (fq/reader path)]
               (doall (map (partial into {}) (fq/read-sequences r)))))
           test-fq-sequences))))

(def sample-1_8 {:instrument "EAS139"
                 :run 136
                 :flowcell "FC706VJ"
                 :lane 2
                 :tile 2104
                 :x 15343
                 :y 197393
                 :pair 1
                 :filtered true
                 :control 18
                 :index "ATCACG"})

(def sample {:instrument "HWUSI-EAS100R"
             :lane 6
             :tile 73
             :x 941
             :y 1973
             :index 0
             :pair 1})

(deftest fastq-read-id-parsing
  (is (= (fq/deserialize-name "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG")
         sample-1_8))

  (is (= (fq/deserialize-casava-1_8-name "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG")
         sample-1_8))

  (is (= (fq/deserialize-name "@EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG")
         sample-1_8))

  (is (= (fq/serialize-name sample-1_8)
         "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG"))

  (is (= (fq/serialize-casava-1_8-name sample-1_8)
         "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG"))

  (is (= (fq/deserialize-name "HWUSI-EAS100R:6:73:941:1973#0/1")
         sample))

  (is (= (fq/deserialize-name "@HWUSI-EAS100R:6:73:941:1973#0/1")
         sample))

  (is (= (fq/serialize-name sample)
         "HWUSI-EAS100R:6:73:941:1973#0/1"))

  (is (= (fq/serialize-casava-name sample)
         "HWUSI-EAS100R:6:73:941:1973#0/1"))

  (is (nil? (fq/deserialize-name "@EAS139:136:FC706VJ:2:2104:15343:197393_1:Y:18:ATCACG")))

  (is (nil? (fq/deserialize-name "@HWUSI-EAS100R:6:73:941:1973#N/1"))))

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [x] (with-open [rdr (fq/reader x)]
                 (= (map (partial into {}) (fq/read-sequences rdr)) test-fq-sequences))
        test-fq-file
        (cio/file test-fq-file)
        (cio/as-url (cio/file test-fq-file))
        (cio/as-url (str (:uri server) "/fastq/test.fq")))))

  (testing "writer"
    (let [temp-file (.getPath (cio/file temp-dir "test.fq"))]
      (are [x] (with-before-after {:before (prepare-cache!)
                                   :after (clean-cache!)}
                 (with-open [wtr (fq/writer x)]
                   (not-throw? (fq/write-sequences wtr test-fq-sequences))))
        temp-file
        (cio/file temp-file)
        (cio/as-url (cio/file temp-file))))))
