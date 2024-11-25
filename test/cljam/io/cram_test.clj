(ns cljam.io.cram-test
  (:require [cljam.io.cram :as cram]
            [cljam.io.cram.decode.structure :as struct]
            [cljam.io.cram.reader :as reader]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util.flag :as flag]
            [cljam.test-common :as common :refer [deftest-remote deftest-slow]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [are deftest is testing]])
  (:import [cljam.io.cram.reader CRAMReader]
           [java.nio.channels FileChannel]))

(def ^:private temp-cram-file (io/file common/temp-dir "test.cram"))
(def ^:private temp-cram-file-2 (io/file common/temp-dir "test2.cram"))
(def ^:private temp-cram-file-3 (io/file common/temp-dir "test3.cram"))
(def ^:private temp-sorted-cram-file (io/file common/temp-dir "test.sorted.cram"))

(defn- fixup-bam-aln [aln]
  (-> (into {} aln)
      (update :cigar #(if (= % "") "*" %))
      (update :options #(sort-by (comp name key first) %))))

(deftest reader-test
  (with-open [bam-rdr (sam/reader common/test-bam-file)
              cram-rdr (cram/reader common/test-cram-file
                                    {:reference common/test-fa-file})
              cram-rdr' (cram/reader cram-rdr)]
    (is (not (cram/indexed? cram-rdr)))
    (is (= (sam/read-header bam-rdr)
           (dissoc (cram/read-header cram-rdr) :HD)
           (dissoc (cram/read-header cram-rdr') :HD)))
    (is (= (sam/read-refs bam-rdr)
           (cram/read-refs cram-rdr)
           (cram/read-refs cram-rdr')))
    (is (= (map fixup-bam-aln (sam/read-alignments bam-rdr))
           (cram/read-alignments cram-rdr)
           (cram/read-alignments cram-rdr')))
    (are [?region ?count] (= ?count
                             (count (cram/read-alignments cram-rdr ?region))
                             (count (cram/read-alignments cram-rdr' ?region)))
      {:chr "ref"} 6
      {:chr "ref2", :start 35, :end 35} 2)))

(deftest-remote reader-with-multiple-containers-test
  (common/with-before-after {:before (common/prepare-cavia!)}
    (testing "read all the alignments"
      (with-open [bam-rdr (sam/reader common/medium-bam-file)
                  cram-rdr (cram/reader common/medium-cram-file
                                        {:reference common/hg19-twobit-file})]
        (is (= (sam/read-header bam-rdr)
               (dissoc (cram/read-header cram-rdr) :HD)))
        (is (= (sam/read-refs bam-rdr)
               (cram/read-refs cram-rdr)))
        (is (= (map fixup-bam-aln (sam/read-alignments bam-rdr))
               (cram/read-alignments cram-rdr)))))
    (testing "read alignments in specified regions (with and without index file)"
      (with-open [cram-rdr (cram/reader common/medium-cram-file
                                        {:reference common/hg19-twobit-file})
                  cram-rdr' (cram/reader common/medium-without-index-cram-file
                                         {:reference common/hg19-twobit-file})]
        (is (cram/indexed? cram-rdr))
        (is (not (cram/indexed? cram-rdr')))
        (are [?region ?count] (= ?count
                                 (count (cram/read-alignments cram-rdr ?region))
                                 (count (cram/read-alignments cram-rdr' ?region)))
          {:chr "chr1"} 615
          {:chr "*"} 4348
          {:chr "chr1", :start 546610, :end 546610} 1
          ;; region crosses over slice boundary
          {:chr "chr1", :start 205500000, :end 209000000} 4
          ;; chr starts and ends in the middle of slice
          {:chr "chr14", :end 21234329} 10
          {:chr "chr14", :start 105661859} 10
          ;; region crosses over container boundary
          {:chr "chr19", :start 54000000, :end 55000000} 12)))))

(deftest writer-test
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (testing "unsorted"
      (with-open [r (cram/reader common/test-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file
                                 {:reference common/test-fa-file})]
        (cram/write-header w (cram/read-header r))
        (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
      (with-open [r (cram/reader common/test-cram-file
                                 {:reference common/test-fa-file})
                  r' (cram/reader temp-cram-file
                                  {:reference common/test-fa-file})]
        (is (= (cram/read-header r)
               (cram/read-header r')))
        (is (= (cram/read-alignments r)
               (cram/read-alignments r')))))
    (testing "sorted by coordinate"
      (with-open [r (cram/reader common/test-sorted-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-sorted-cram-file
                                 {:reference common/test-fa-file})]
        (cram/write-header w (cram/read-header r))
        (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
      (with-open [r (cram/reader common/test-sorted-cram-file
                                 {:reference common/test-fa-file})
                  r' (cram/reader temp-sorted-cram-file
                                  {:reference common/test-fa-file})]
        (is (= (cram/read-header r)
               (cram/read-header r')))
        (is (= (cram/read-alignments r)
               (cram/read-alignments r')))))))

(deftest-remote writer-with-multiple-containers-test
  (common/with-before-after {:before (do (common/prepare-cavia!)
                                         (common/prepare-cache!))
                             :after (common/clean-cache!)}
    (with-open [r (cram/reader common/medium-cram-file
                               {:reference common/hg19-twobit-file})
                w (cram/writer temp-cram-file
                               {:reference common/hg19-twobit-file})]
      (cram/write-header w (cram/read-header r))
      (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
    (with-open [r (cram/reader common/medium-cram-file
                               {:reference common/hg19-twobit-file})
                r' (cram/reader temp-cram-file
                                {:reference common/hg19-twobit-file})]
      (is (= (cram/read-header r)
             (cram/read-header r')))
      (is (= (cram/read-alignments r)
             (cram/read-alignments r'))))))

(defn- all-compression-headers [^CRAMReader r]
  (let [^FileChannel ch (.-channel r)]
    (letfn [(read1 [container-header bb]
              (when-not (struct/eof-container? container-header)
                (struct/decode-compression-header-block bb)))
            (step []
              (when (< (.position ch) (.size ch))
                (when-let [header (#'reader/read-container-with r read1)]
                  (cons header (lazy-seq (step))))))]
      (step))))

(deftest writer-with-reference-embedding-test
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (testing "Reference embedding will be enabled if `:embed-reference?` is set to true"
      (with-open [r (cram/reader common/test-sorted-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-sorted-cram-file
                                 {:reference common/test-fa-file
                                  :embed-reference? true
                                  :skip-sort-order-check? true
                                  :ds-compressor-overrides {:embedded-ref :raw}})]
        (cram/write-header w (cram/read-header r))
        (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
      (with-open [r (cram/reader common/test-sorted-cram-file
                                 {:reference common/test-fa-file})
                  r' (cram/reader temp-sorted-cram-file)]
        (is (= (cram/read-header r)
               (cram/read-header r')))
        (is (= (cram/read-alignments r)
               (cram/read-alignments r'))))
      (with-open [r (cram/reader temp-sorted-cram-file)]
        (let [headers (all-compression-headers r)]
          (is (seq headers))
          (is (every? #(false? (get-in % [:preservation-map :RR])) headers)))))
    (testing "Error when trying to embed reference sequences for a CRAM file not declared as `SO:coordinate`"
      (with-open [r (cram/reader common/test-sorted-with-unknown-so-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file
                                 {:reference common/test-fa-file
                                  :embed-reference? true
                                  :ds-compressor-overrides {:embedded-ref :raw}})]
        (is (thrown-with-msg? Exception #"Cannot embed reference sequences for CRAM file not declared as sorted by coordinate"
                              (cram/write-header w (cram/read-header r))))))
    (testing "`:skip-sort-order-check?` skips the header check when embedding reference sequences"
      (with-open [r (cram/reader common/test-sorted-with-unknown-so-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file
                                 {:reference common/test-fa-file
                                  :embed-reference? true
                                  :skip-sort-order-check? true
                                  :ds-compressor-overrides {:embedded-ref :raw}})]
        (is (common/not-throw? (cram/write-header w (cram/read-header r))))))))

(deftest writer-index-options-test
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (testing "A CRAM index file won't be created by default"
      (with-open [r (cram/reader common/test-sorted-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file-2
                                 {:reference common/test-fa-file})]
        (cram/write-header w (cram/read-header r))
        (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
      (is (not (.exists (io/file (str temp-cram-file-2 ".crai"))))))
    (testing "A CRAM index file will be created if `:create-index?` is set to true"
      (with-open [r (cram/reader common/test-sorted-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file-2
                                 {:reference common/test-fa-file
                                  :create-index? true})]
        (cram/write-header w (cram/read-header r))
        (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
      (is (.exists (io/file (str temp-cram-file-2 ".crai")))))
    (testing "Error when trying to create an index file for a CRAM file not declared as `SO:coordinate`"
      (with-open [r (cram/reader common/test-sorted-with-unknown-so-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file-3
                                 {:reference common/test-fa-file
                                  :create-index? true})]
        (is (thrown-with-msg? Exception #"Cannot create CRAM index file for CRAM file not declared as sorted by coordinate"
                              (cram/write-header w (cram/read-header r))))))
    (testing "`:skip-sort-order-check?` skips the header check when creating an index file"
      (with-open [r (cram/reader common/test-sorted-with-unknown-so-cram-file
                                 {:reference common/test-fa-file})
                  w (cram/writer temp-cram-file-3
                                 {:reference common/test-fa-file
                                  :create-index? true
                                  :skip-sort-order-check? true})]
        (cram/write-header w (cram/read-header r))
        (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
      (is (.exists (io/file (str temp-cram-file-3 ".crai")))))))

(deftest-slow writer-with-read-name-omission-test
  (common/with-before-after {:before (do (common/prepare-cavia!)
                                         (common/prepare-cache!))
                             :after (common/clean-cache!)}
    (with-open [r (cram/reader common/paired-sorted-cram-file
                               {:reference common/hg38-twobit-file})
                w (cram/writer temp-cram-file
                               {:reference common/hg38-twobit-file
                                :omit-read-names? true
                                :min-single-ref-slice-size 1})]
      (cram/write-header w (cram/read-header r))
      (cram/write-alignments w (cram/read-alignments r) (cram/read-header r)))
    (with-open [r (cram/reader common/paired-sorted-cram-file
                               {:reference common/hg38-twobit-file})
                r' (cram/reader temp-cram-file
                                {:reference common/hg38-twobit-file})]
      (is (= (cram/read-header r)
             (cram/read-header r')))
      (let [alns (vec (cram/read-alignments r))
            alns' (vec (cram/read-alignments r'))
            mates (->> alns
                       (map-indexed #(assoc %2 :idx %1))
                       (group-by :qname)
                       (into {} (map (fn [[qname xs]]
                                       [qname (into #{} (map :idx) xs)]))))
            mates' (->> alns'
                        (map-indexed #(assoc %2 :idx %1))
                        (group-by :qname)
                        (into {} (map (fn [[qname xs]]
                                        [qname (into #{} (map :idx) xs)]))))
            qnames (set (keys mates))
            qnames' (set (keys mates'))]
        (is (= (map #(dissoc % :qname) alns)
               (map #(dissoc % :qname) alns')))
        (is (not= qnames qnames'))
        (is (every? #(= (get mates %) (get mates' %))
                    (set/intersection qnames qnames')))
        (is (every? (fn [qname]
                      (let [indices (get mates' qname)]
                        (and (= (count indices) 2)
                             (every? #(flag/primary? (:flag (nth alns' %)))
                                     indices))))
                    (set/difference qnames' qnames)))
        (is (= (set (vals mates)) (set (vals mates'))))))))
