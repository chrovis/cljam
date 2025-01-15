(ns cljam.io.bam.reader-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [cljam.util :as util]
            [cljam.util.chromosome :as chr]
            [cljam.io.protocols :as prot]
            [cljam.io.bam.core :as bam.core]
            [cljam.io.sam.util.flag :as flag]))

(defn- conj-flag [flag & xs]
  (flag/encode (apply conj (flag/decode flag) xs)))

(defn- unmap [xs]
  (let [[unmapped mapped] (if (< (double (rand)) 0.5)
                            [0 1]
                            [1 0])]
    (-> xs
        (update unmapped assoc
                :rname "*" :pos 0 :end 0
                :mapq 0 :cigar "" :tlen 0)
        (update-in [unmapped :flag] conj-flag :unmapped)
        (update-in [unmapped :rnext] #(if (= "=" %)
                                        (:rname (first xs))
                                        %))
        (update mapped assoc
                :rnext "*" :pnext 0 :tlen 0)
        (update-in [mapped :flag] conj-flag :next-unmapped))))

(defn- generate-paired-bam [n]
  (let [refs (->>
              (range 24)
              (map (fn [i] {:SN (str "chr" (inc (long i))),
                            :LN (+ 100000000 (long (rand-int 50000000)))}))
              (sort-by (comp chr/chromosome-order-key :SN)))]
    (->>
     (range n)
     (map
      (fn [i]
        (let [{:keys [SN LN] :as r} (rand-nth refs)
              pos (int (rand-int (- (long LN) 300)))
              chimeric? (< (double (rand)) 0.3)
              unmapped? (< (double (rand)) 0.2)
              rnext (if chimeric?
                      (rand-nth (filterv (complement #{r}) refs))
                      r)
              pnext (int (if chimeric?
                           (rand-int (- (long (:LN rnext)) 300))
                           (+ pos (int 70) (int (rand-int 200)))))
              qname (str "read" (inc (long i)))]
          (cond-> [(prot/->SAMAlignment
                    qname (flag/encode #{:multiple :first})
                    SN pos (int (+ pos 150)) 60 "151M"
                    (if chimeric? (:SN rnext) "=") pnext 0 "*" "*" [])
                   (prot/->SAMAlignment
                    qname (flag/encode #{:multiple :last})
                    (:SN rnext) pnext (int (+ pnext 150)) 60 "151M"
                    (if chimeric? SN "=") pos 0 "*" "*" [])]
            unmapped? unmap))))
     (apply concat)
     (sort-by (juxt (comp chr/chromosome-order-key :rname) :pos))
     (vector {:HD {:VN "1.4", :SO "coordinate"}, :SQ refs}))))

(defn- generate-test-case [n alignments]
  (let [in (->> alignments
                shuffle
                (take n)
                (group-by :qname)
                vals
                (map first)) ; take either R1 or R2
        mate? (into #{} (map (juxt :qname (comp not flag/r1? :flag)) in))]
    [in (filter (comp mate? (juxt :qname (comp flag/r1? :flag))) alignments)]))

(deftest read-mate-alignments-test
  ;; 1. Generates a sequence of paired alignments with random positions.
  ;; 2. Write the alignments to a BAM file with an index.
  ;; 3. Randomly pick some subsets of the alignments as queries.
  ;; 4. Compute the mate alignments for each query using (1).
  ;; 5. Read the mate alignments from (2) and compare with (4).
  (util/with-temp-dir [d "read-pairs-test"]
    (let [temp-bam (io/file d "tmp.bam")
          n-total-alignments 10000
          n-query-alignments 50
          n-tests 5
          [header alignments] (generate-paired-bam n-total-alignments)]

      ;; write a BAM with a BAI
      (with-open [w (bam.core/writer temp-bam true)]
        (doto w
          (prot/write-header header)
          (prot/write-refs header)
          (prot/write-alignments alignments header)))

      ;; read and compare
      (with-open [r (bam.core/reader temp-bam)]
        (doseq [[i [input expected]] (->> #(generate-test-case
                                            n-query-alignments alignments)
                                          (repeatedly n-tests)
                                          (map vector (range)))]
          (testing (inc (long i))
            (is (= expected
                   (prot/read-mate-alignments r input)))))))))
