(ns cljam.io.sam-test
  (:require [clojure.test :refer [deftest is are testing]]
            [clojure.java.io :as cio]
            [cljam.test-common :refer
             [deftest-slow
              deftest-remote
              with-before-after
              prepare-cache!
              prepare-cavia!
              clean-cache!
              slurp-sam-for-test
              slurp-bam-for-test
              spit-sam-for-test
              spit-bam-for-test
              not-throw?
              same-file?
              data->clj
              http-server
              temp-dir
              test-sam-file
              test-bam-file
              test-sam
              test-sam-data
              test-sam-refs
              test-sam-sorted-by-pos
              test-sam-sorted-by-qname
              test-sorted-bam-file
              test-sorted-bam-data
              small-bam-file
              medium-bam-file
              medium-sam-refs
              large-bam-file
              large-sam-refs
              opts-sam-file
              opts-bam-file
              test-sam-blocks
              test-bai-file
              test-fa-file
              test-tabix-file]]
            [cljam.io.sam :as sam]
            [cljam.io.protocols :as protocols]))

(def temp-sam-file (str temp-dir "/test.sam"))
(def temp-bam-file (str temp-dir "/test.bam"))
(def temp-bam-file-2 (str temp-dir "/test2.bam"))
(def temp-bam-file-sorted (str temp-dir "/test.sorted.bam"))
(def temp-small-bam-file (str temp-dir "/small.bam"))
(def temp-medium-bam-file (str temp-dir "/medium.bam"))
(def not-found-bam-file (str temp-dir "/not-found.bam"))
(def invalid-bam-file-1 test-fa-file)
(def invalid-bam-file-2 test-tabix-file)

(deftest slurp-sam-test
  (is (= (slurp-sam-for-test test-sam-file) test-sam)))

(deftest slurp-bam-test
  (is (= (slurp-bam-for-test test-bam-file) test-sam)))

(deftest-slow slurp-bam-medium-test
  (is (not-throw? (slurp-bam-for-test medium-bam-file))))

(deftest spit-sam-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-sam-for-test temp-sam-file test-sam)))
    (is (= (slurp-sam-for-test temp-sam-file) test-sam))))

(deftest-slow spit-sam-medium-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-sam-for-test temp-sam-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large SAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy about-spit-sam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-sam-for-test temp-sam-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(deftest spit-bam-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-bam-file test-sam)))
    (is (= (slurp-bam-for-test temp-bam-file) test-sam))))

(deftest-slow spit-bam-medium-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-bam-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy spit-bam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-bam-for-test temp-bam-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(defn- shallow= [alns1 alns2]
  (= (map #(select-keys % [:rname :pos]) alns1)
     (map #(select-keys % [:rname :pos]) alns2)))

(defn- pointer= [alns1 alns2]
  (= (map #(select-keys % [:rname :pos :flag]) alns1)
     (map #(select-keys % [:rname :pos :flag]) alns2)))

(deftest sam-reader-test
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-sam-for-test temp-sam-file test-sam))
                      :after (clean-cache!)}
    (with-open [rdr (sam/sam-reader temp-sam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs)))))

(deftest bam-reader-test
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-bam-for-test temp-bam-file test-sam))
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader temp-bam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (= (sam/read-alignments rdr) (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (sam/read-alignments rdr {:chr "ref2"}))))
    (with-open [rdr (sam/bam-reader temp-bam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (= (sam/read-alignments rdr {:depth :deep}) (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (shallow= (sam/read-alignments rdr {:depth :shallow})
                    (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (pointer= (sam/read-alignments rdr {:depth :pointer})
                    (:alignments test-sam))))
    (doseq [mode [:normal :region :coordinate :queryname :pointer]]
      (with-open [rdr (sam/bam-reader temp-bam-file)]
        (is (= (sam/read-refs rdr) test-sam-refs))
        (is (= (data->clj (sam/read-blocks rdr {} {:mode mode})) test-sam-data))))
    (with-open [rdr (sam/bam-reader temp-bam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (data->clj (sam/read-blocks rdr {:chr "ref2"})))))))

(deftest bam-reader-with-index-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (testing "read-alignments"
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (sam/read-alignments rdr {:chr "ref2"})
               (drop 6 (:alignments test-sam-sorted-by-pos)))))
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (sam/read-alignments rdr {:chr "ref2" :start 21})
               (drop 7 (:alignments test-sam-sorted-by-pos)))))
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (sam/read-alignments rdr {:chr "ref2" :end 9})
               (take 3 (drop 6 (:alignments test-sam-sorted-by-pos))))))
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (sam/read-alignments rdr {:chr "ref2" :start 10 :end 12})
               (take 5 (drop 6 (:alignments test-sam-sorted-by-pos)))))))
    (testing "read-blocks"
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (data->clj (sam/read-blocks rdr))
               test-sorted-bam-data)))
      (doseq [mode [:normal :region :coordinate :queryname :pointer]]
        (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
          (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                      (data->clj (sam/read-blocks rdr {:chr "ref2"} {:mode mode})))
                 (drop 6 test-sorted-bam-data))))
        (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
          (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                      (data->clj (sam/read-blocks rdr {:chr "ref2" :start 2} {:mode mode})))
                 (drop 6 test-sorted-bam-data))))
        (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
          (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                      (data->clj (sam/read-blocks rdr {:chr "ref2" :end 2} {:mode mode})))
                 (take 2 (drop 6 test-sorted-bam-data)))))
        (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
          (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                      (data->clj (sam/read-blocks rdr {:chr "ref2" :start 4 :end 12} {:mode mode})))
                 (take 5 (drop 6 test-sorted-bam-data)))))))
    (testing "read-in-region"
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (protocols/read-in-region rdr {:chr "ref2"})
               (drop 6 (:alignments test-sam-sorted-by-pos))))
        (is (= (protocols/read-in-region rdr {:chr "ref2", :start 21})
               (drop 7 (:alignments test-sam-sorted-by-pos))))
        (is (= (protocols/read-in-region rdr {:chr "ref2", :start 10, :end 12})
               (take 5 (drop 6 (:alignments test-sam-sorted-by-pos))))))
      (with-open [rdr (sam/bam-reader test-sorted-bam-file)]
        (is (= (protocols/read-in-region rdr {:chr "ref2"} {})
               (drop 6 (:alignments test-sam-sorted-by-pos))))
        (is (= (protocols/read-in-region rdr {:chr "ref2", :start 21} {})
               (drop 7 (:alignments test-sam-sorted-by-pos))))
        (is (= (protocols/read-in-region rdr {:chr "ref2", :start 10, :end 12} {})
               (take 5 (drop 6 (:alignments test-sam-sorted-by-pos)))))))))

(deftest bam-reader-invalid-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? Exception (sam/bam-reader invalid-bam-file-1)))
    (is (thrown? java.io.IOException (sam/bam-reader invalid-bam-file-2)))
    (is (thrown? java.io.IOException (sam/bam-reader not-found-bam-file)))))

(deftest-slow bam-reader-medium-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader medium-bam-file)]
      (let [header (sam/read-header rdr)
            refs (sam/read-refs rdr)
            alns (sam/read-alignments rdr)]
        (is (= refs medium-sam-refs))
        (with-open [w (sam/bam-writer temp-bam-file)]
          (is (not-throw? (sam/write-header w header)))
          (is (not-throw? (sam/write-refs w refs)))
          (is (not-throw? (sam/write-alignments w alns header)))
          (same-file? medium-bam-file temp-bam-file))))))

(deftest-remote bam-reader-large-test
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-cavia!))
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader large-bam-file)]
      (is (= (sam/read-refs rdr) large-sam-refs))
      (is (not-throw? (sam/read-alignments rdr))))))

(deftest reader-test
  (testing "sam"
    (with-open [rdr (sam/reader test-sam-file)]
      (is (instance? cljam.io.sam.reader.SAMReader rdr)))
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (let [tmp (cio/file temp-dir "temp-sam-file-without-suffix")]
        (cio/copy (cio/file test-sam-file) tmp)
        (with-open [rdr (sam/reader tmp)]
          (is (instance? cljam.io.sam.reader.SAMReader rdr))))))
  (testing "bam"
    (with-open [rdr (sam/reader test-bam-file)]
      (is (instance? cljam.io.bam.reader.BAMReader rdr)))
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (let [tmp (cio/file temp-dir "temp-bam-file-without-suffix")]
        (cio/copy (cio/file test-bam-file) tmp)
        (with-open [rdr (sam/reader tmp)]
          (is (instance? cljam.io.bam.reader.BAMReader rdr))))))
  (testing "clone bam"
    (with-open [rdr (sam/reader test-bam-file)
                crdr (sam/reader rdr)]
      (is (instance? cljam.io.bam.reader.BAMReader crdr))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (sam/reader f))
      "./test-resources/bam/foo.bam"
      test-bai-file
      "./test-resources/bam/foo.baam")))

(deftest indexed?-test
  (testing "sam"
    (with-open [rdr (sam/reader test-sam-file)]
      (true? (sam/indexed? rdr))))
  (testing "bam"
    (are [f] (with-open [rdr (sam/reader f)]
               (true? (sam/indexed? rdr)))
      test-sorted-bam-file
      small-bam-file
      medium-bam-file)
    (with-before-after {:before (do (prepare-cache!)
                                    (cio/copy (cio/file test-sorted-bam-file) (cio/file temp-bam-file-sorted))
                                    (cio/copy (cio/file small-bam-file) (cio/file temp-small-bam-file))
                                    (cio/copy (cio/file medium-bam-file) (cio/file temp-medium-bam-file)))
                        :after (clean-cache!)}
      (are [f] (with-open [rdr (sam/reader f)]
                 (false? (sam/indexed? rdr)))
        temp-bam-file-sorted
        temp-small-bam-file
        temp-medium-bam-file))))

(deftest sam-writer-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [w (sam/sam-writer temp-sam-file)]
      (is (not-throw? (sam/write-header w (:header test-sam))))
      (is (not-throw? (sam/write-alignments w (:alignments test-sam) nil))))
    (with-open [r (sam/sam-reader temp-sam-file)]
      (is (= (sam/read-header r) (:header test-sam)))
      (is (= (sam/read-refs r) test-sam-refs))
      (is (= (sam/read-blocks r) test-sam-blocks)))
    (with-open [w (sam/sam-writer temp-sam-file)]
      (is (not-throw? (sam/write-header w (:header test-sam))))
      (is (not-throw? (sam/write-blocks w test-sam-blocks))))
    (with-open [r (sam/sam-reader temp-sam-file)]
      (is (= (sam/read-header r) (:header test-sam)))
      (is (= (sam/read-refs r) test-sam-refs))
      (is (= (sam/read-alignments r) (:alignments test-sam))))))

(deftest writer-test
  (testing "sam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [wtr (sam/writer (.getAbsolutePath (cio/file temp-dir "temp.sam")))]
        (is (instance? cljam.io.sam.writer.SAMWriter wtr)))))
  (testing "bam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [wtr (sam/writer (.getAbsolutePath (cio/file temp-dir "temp.bam")))]
        (is (instance? cljam.io.bam.writer.BAMWriter wtr)))))
  (testing "throws Exception"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (are [f] (thrown? Exception (sam/writer (.getAbsolutePath (cio/file temp-dir f))))
        "temp.baam"
        "temp.bai"))))

(deftest writer-index-option-test
  (testing "sam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [w (sam/writer temp-sam-file false)]
        (sam/write-header w (:header test-sam-sorted-by-pos))
        (sam/write-refs w (:header test-sam-sorted-by-pos))
        (sam/write-alignments w (:alignments test-sam-sorted-by-pos) (:header test-sam-sorted-by-pos)))
      (is (not (.exists (cio/file (str temp-sam-file ".bai")))))))
  (testing "bam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [w (sam/writer temp-bam-file false)]
        (sam/write-header w (:header test-sam-sorted-by-pos))
        (sam/write-refs w (:header test-sam-sorted-by-pos))
        (sam/write-alignments w (:alignments test-sam-sorted-by-pos) (:header test-sam-sorted-by-pos)))
      (is (not (.exists (cio/file (str temp-bam-file ".bai")))))
      (with-open [w (sam/writer temp-bam-file true)]
        (sam/write-header w (:header test-sam))
        (sam/write-refs w (:header test-sam))
        (sam/write-alignments w (:alignments test-sam) (:header test-sam)))
      (is (not (.exists (cio/file (str temp-bam-file ".bai")))))
      (with-open [w (sam/writer temp-bam-file true)]
        (sam/write-header w (:header test-sam-sorted-by-qname))
        (sam/write-refs w (:header test-sam-sorted-by-qname))
        (sam/write-alignments w (:alignments test-sam-sorted-by-qname) (:header test-sam-sorted-by-qname)))
      (is (not (.exists (cio/file (str temp-bam-file ".bai")))))
      (with-open [r (sam/reader test-sorted-bam-file)
                  w (sam/writer temp-bam-file true)]
        (sam/write-header w (sam/read-header r))
        (sam/write-refs w (sam/read-header r))
        (sam/write-alignments w (sam/read-alignments r) (sam/read-header r)))
      (is (.exists (cio/file (str temp-bam-file ".bai"))))
      (is (same-file? test-bai-file (str temp-bam-file ".bai")))
      (with-open [r (sam/reader test-sorted-bam-file)
                  w (sam/writer temp-bam-file-2 true)]
        (sam/write-header w (sam/read-header r))
        (sam/write-refs w (sam/read-header r))
        (sam/write-blocks w (sam/read-blocks r)))
      (is (.exists (cio/file (str temp-bam-file-2 ".bai"))))
      (is (same-file? test-bai-file (str temp-bam-file-2 ".bai")))))
  (testing "throws Exception"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (is (thrown? Exception (sam/writer temp-sam-file true))))))

(def test-options
  [{:Xa {:type "A", :value \p}}
   {:Xi {:type "i", :value -100}}
   {:Xf {:type "f", :value -1400.0}}
   {:Xz {:type "Z", :value "AATTGGCC"}}
   {:Xh {:type "H", :value (map unchecked-byte [0x1A 0xE3 0x01])}}
   {:Xc {:type "B", :value "c,-128,0,127"}}
   {:XC {:type "B", :value "C,0,127,255"}}
   {:Xs {:type "B", :value "s,-32768,0,32767"}}
   {:XS {:type "B", :value "S,0,32767,65535"}}
   {:Xi {:type "B", :value "i,-2147483648,0,2147483647"}}
   {:XI {:type "B", :value "I,0,2147483647,4294967295"}}
   {:Xf {:type "B", :value "f,-0.3,0.0,0.3"}}])

(defn bytes-to-seq [t]
  (if (= (get-in t [(ffirst t) :type]) "H")
    (update-in t [(ffirst t) :value] seq)
    t))

(deftest options
  (testing "sam"
    (is
     (= (with-open [r (sam/reader opts-sam-file)]
          (->> (sam/read-alignments r) (mapcat :options) (map bytes-to-seq) doall))
        test-options)))
  (testing "bam"
    (is
     (= (with-open [r (sam/reader opts-bam-file)]
          (->> (sam/read-alignments r) (mapcat :options) (map bytes-to-seq) doall))
        test-options))))

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [x] (with-open [rdr (sam/reader x)]
                 (and (= (sam/read-refs rdr) test-sam-refs)
                      (= (sam/read-alignments rdr) (:alignments test-sam))))
        test-sam-file
        (cio/file test-sam-file)
        (cio/as-url (cio/file test-sam-file))
        (cio/as-url (str (:uri server) "/sam/test.sam"))

        test-bam-file
        (cio/file test-bam-file)
        (cio/as-url (cio/file test-bam-file))
        (cio/as-url (str (:uri server) "/bam/test.bam")))))

  (testing "writer"
    (are [x] (with-before-after {:before (prepare-cache!)
                                 :after (clean-cache!)}
               (with-open [wtr (sam/writer x)]
                 (and (not-throw? (sam/write-header wtr (:header test-sam)))
                      (not-throw? (sam/write-alignments wtr (:alignments test-sam) nil)))))
      temp-sam-file
      (cio/file temp-sam-file)
      (cio/as-url (cio/file temp-sam-file))

      temp-bam-file
      (cio/file temp-bam-file)
      (cio/as-url (cio/file temp-bam-file)))))
