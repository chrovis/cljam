(ns cljam.io.sam-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.sam :as sam]
            [cljam.io.bam.writer :as bam-writer]
            [cljam.io.bam-index.core :as bai]
            [cljam.io.bam-index.writer :as bai-writer]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]))

(def temp-sam-file (str temp-dir "/test.sam"))
(def temp-bam-file (str temp-dir "/test.bam"))
(def temp-bam-file-sorted (str temp-dir "/test.sorted.bam"))
(def temp-bai-file-sorted (cstr/replace temp-bam-file-sorted #"(?i)(\.bam)$" "$1.bai"))
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
      (is (instance? cljam.io.sam.reader.SAMReader rdr))))
  (testing "bam"
    (with-open [rdr (sam/reader test-bam-file)]
      (is (instance? cljam.io.bam.reader.BAMReader rdr))))
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
    (with-open [wtr (sam/writer (.getAbsolutePath (cio/file util/temp-dir "temp.sam")))]
      (is (instance? cljam.io.sam.writer.SAMWriter wtr))))
  (testing "bam"
    (with-open [wtr (sam/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bam")))]
      (is (instance? cljam.io.bam.writer.BAMWriter wtr))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (sam/writer (.getAbsolutePath (cio/file util/temp-dir f))))
      "temp.baam"
      "temp.bai")))

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

(deftest transduce-writer-test
  (testing "invalid writers"
    (is (thrown? IllegalArgumentException (sam/write-blocks-rf nil nil)))
    (is (thrown? IllegalArgumentException (sam/write-blocks-xf nil nil)))
    (is (thrown? IllegalArgumentException (sam/write-alignments-rf nil nil)))
    (is (thrown? IllegalArgumentException (sam/write-alignments-xf nil nil))))
  (testing "read/write regression"
    (are [?in ?out]
        (are [?xf ?rf ?rc]
            (with-before-after {:before (prepare-cache!)
                                :after (clean-cache!)}
              (with-open [r (sam/reader ?in)
                          w (sam/writer ?out)]
                (transduce ?xf ?rf ?rc))
              (same-sam-contents? ?in ?out))
          identity (sam/write-blocks-rf w (sam/read-header r)) (sam/read-blocks r)
          (sam/write-blocks-xf w (sam/read-header r)) (fn [& _]) (sam/read-blocks r)
          identity (sam/write-alignments-rf w (sam/read-header r)) (sam/read-alignments r)
          (sam/write-alignments-xf w (sam/read-header r)) (fn [& _]) (sam/read-alignments r))
      test-sorted-bam-file temp-bam-file-sorted
      test-sam-file temp-sam-file))
  (testing "conversion"
    (are [?in ?out ?ref]
        (with-before-after {:before (prepare-cache!)
                            :after (clean-cache!)}
          (with-open [r (sam/reader ?in)
                      w (sam/writer ?out)]
            (transduce identity (sam/write-alignments-rf w (sam/read-header r)) (sam/read-alignments r)))
          (same-sam-contents? ?ref ?out))
      test-sam-file temp-bam-file test-bam-file
      test-bam-file temp-sam-file test-sam-file))
  (testing "reduced"
    (are [?in ?out]
        (are [?xf ?rf ?pred]
            (with-before-after {:before (prepare-cache!)
                                :after (clean-cache!)}
              (with-open [r (sam/reader ?in)
                          w (sam/writer ?out)]
                (transduce
                 ?xf
                 ?rf
                 (sam/read-blocks r)))
              (?pred (same-sam-contents? ?in ?out)))
          (take 11) (sam/write-blocks-rf w (sam/read-header r)) false?
          (take 12) (sam/write-blocks-rf w (sam/read-header r)) true?
          (comp (take 11) (sam/write-blocks-xf w (sam/read-header r))) (fn [& _]) false?
          (comp (take 12) (sam/write-blocks-xf w (sam/read-header r))) (fn [& _]) true?
          (sam/write-blocks-xf w (sam/read-header r)) (fn ([]) ([_]) ([_ x] (reduced x))) false?)
      test-sorted-bam-file temp-bam-file-sorted)))

(deftest transduce-bai-test
  (testing "indexing"
    (are [?xf ?rf]
        (with-before-after {:before (prepare-cache!)
                            :after (clean-cache!)}
          (with-open [r (sam/reader test-sorted-bam-file)
                      w (bai/writer temp-bai-file-sorted (sam/read-refs r))]
            (transduce ?xf ?rf (sam/read-blocks r)))
          (same-file? test-bai-file temp-bai-file-sorted))
      identity (bai-writer/write-index-rf w)
      (bai-writer/write-index-xf w) (fn [& _]))))
