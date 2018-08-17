(ns cljam.io.pileup-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.pileup :as plpio]
            [cljam.io.sequence :as cseq]
            [cljam.io.fasta-index.core :as fai]
            [cljam.io.protocols :as p])
  (:import [java.io File StringWriter StringReader]
           [cljam.io.pileup PileupReader PileupWriter]))

;; Reader
;; ------

(deftest parse-bases-col
  (testing "without-ref"
    (are [?in ?out]
        (= (mapv #(into {:alignment nil} %) ?out)
           (mapv #(into {} %) (#'plpio/parse-bases-col nil ?in)))
      "" []
      "A" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "a" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? true, :end? false, :insertion nil, :deletion nil}]
      "^]A" [{:start? true, :mapq 60, :base \A, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "a$" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? true, :end? true, :insertion nil, :deletion nil}]
      "A$C" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion nil}
             {:start? false, :mapq nil, :base \C, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "A$^]C" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion nil}
               {:start? true, :mapq 60, :base \C, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "^BN" [{:start? true, :mapq 33, :base \N, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      ">" [{:start? false, :mapq nil, :base \>, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "*" [{:start? false, :mapq nil, :base \*, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "<" [{:start? false, :mapq nil, :base \>, :qual -1, :reverse? true, :end? false, :insertion nil, :deletion nil}]
      "A+2TG" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? false, :insertion "TG", :deletion nil}]
      "a-12nnnnnnnnnnnn$c$" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? true, :end? true, :insertion nil, :deletion 12}
                             {:start? false, :mapq nil, :base \C, :qual -1, :reverse? true, :end? true, :insertion nil, :deletion nil}]
      "A+14AAAAATTTTTGGGG" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? false, :insertion "AAAAATTTTTGGGG", :deletion nil}]
      "^]A+14AAAAATTTTTGGGG" [{:start? true, :mapq 60, :base \A, :qual -1, :reverse? false, :end? false, :insertion "AAAAATTTTTGGGG", :deletion nil}]
      "A+14AAAAATTTTTGGGG$" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? true, :insertion "AAAAATTTTTGGGG", :deletion nil}]
      "A+14AAAAATTTTTGGGGG" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? false, :insertion "AAAAATTTTTGGGG", :deletion nil}
                             {:start? false, :mapq nil, :base \G, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "A-2NN$" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion 2}]
      "A-2NN$^Ca" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion 2}
                   {:start? true, :mapq 34, :base \A, :qual -1, :reverse? true, :end? false, :insertion nil, :deletion nil}]))
  (testing "with-ref"
    (are [?in ?out]
        (= (mapv #(into {:alignment nil} %) ?out)
           (mapv #(into {} %) (#'plpio/parse-bases-col \T ?in)))
      "" []
      "A" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      "a$" [{:start? false, :mapq nil, :base \A, :qual -1, :reverse? true, :end? true, :insertion nil, :deletion nil}]
      "," [{:start? false, :mapq nil, :base \T, :qual -1, :reverse? true, :end? false, :insertion nil, :deletion nil}]
      ".$" [{:start? false, :mapq nil, :base \T, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion nil}]
      ".$^]," [{:start? false, :mapq nil, :base \T, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion nil}
               {:start? true, :mapq 60, :base \T, :qual -1, :reverse? true, :end? false, :insertion nil, :deletion nil}]
      "^B." [{:start? true, :mapq 33, :base \T, :qual -1, :reverse? false, :end? false, :insertion nil, :deletion nil}]
      ",+2tg" [{:start? false, :mapq nil, :base \T, :qual -1, :reverse? true, :end? false, :insertion "TG", :deletion nil}]
      ".-2AT$" [{:start? false, :mapq nil, :base \T, :qual -1, :reverse? false, :end? true, :insertion nil, :deletion 2}]
      ",-2at$^Ca" [{:start? false, :mapq nil, :base \T, :qual -1, :reverse? true, :end? true, :insertion nil, :deletion 2}
                   {:start? true, :mapq 34, :base \A, :qual -1, :reverse? true, :end? false, :insertion nil, :deletion nil}]))
  (testing "invalid arguments"
    (are [?in]
        (thrown? Exception (#'plpio/parse-bases-col nil ?in))
      "A+"
      "^"
      "^A"
      "^A+1A"
      "+"
      "-"
      "++"
      "--"
      "A$$A"
      "A+2A^]A"
      "A+2A"
      "A+2"
      "A-2N"
      "A-2")))

(deftest parse-pileup-line
  (testing "without-ref"
    (are [?in ?out]
        (= ?out (into {} (update (#'plpio/parse-pileup-line ?in) :pile (fn [xs] (mapv #(into {} %) xs)))))
      "chr1\t10\tN\t0\t\t" {:rname "chr1", :pos 10, :ref \N, :count 0,
                            :pile []}
      "chr1\t10\tN\t1\tA\tI" {:rname "chr1", :pos 10, :ref \N, :count 1,
                              :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? false, :end? false, :insertion nil, :deletion nil, :alignment nil}]}
      "chr1\t10\tN\t2\taA\tIB" {:rname "chr1", :pos 10, :ref \N, :count 2,
                                :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :alignment nil}
                                       {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? false, :insertion nil, :deletion nil, :alignment nil}]}
      "chr1\t10\tN\t2\taA-2NN$\tIB" {:rname "chr1", :pos 10, :ref \N, :count 2,
                                     :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :alignment nil}
                                            {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? true, :insertion nil, :deletion 2, :alignment nil}]}))
  (testing "with-ref"
    (are [?in ?out]
        (= ?out (into {} (update (#'plpio/parse-pileup-line ?in) :pile (fn [xs] (mapv #(into {} %) xs)))))
      "chr1\t10\tA\t0\t\t" {:rname "chr1", :pos 10, :ref \A, :count 0,
                            :pile []}
      "chr1\t10\ta\t1\t.\tI" {:rname "chr1", :pos 10, :ref \a, :count 1,
                              :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? false, :end? false, :insertion nil, :deletion nil, :alignment nil}]}
      "chr1\t10\tA\t2\t,.\tIB" {:rname "chr1", :pos 10, :ref \A, :count 2,
                                :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :alignment nil}
                                       {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? false, :insertion nil, :deletion nil, :alignment nil}]}
      "chr1\t10\tA\t2\t,.-2CA$\tIB" {:rname "chr1", :pos 10, :ref \A, :count 2,
                                     :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :alignment nil}
                                            {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? true, :insertion nil, :deletion 2, :alignment nil}]})))

(deftest reader
  (with-open [r (plpio/reader test-pileup-file)]
    (is (instance? PileupReader r))))

(deftest read-piles
  (with-open [r (plpio/reader test-pileup-file)]
    (is (= {:rname "ref2",
            :pos 36,
            :ref \N,
            :count 1,
            :pile [{:start? false,
                    :mapq nil,
                    :base \A,
                    :qual 30,
                    :reverse? false,
                    :end? true,
                    :insertion nil,
                    :deletion nil
                    :alignment nil}]}
           (->> xs
                (map #(into {} %))
                (fn [xs])
                (update (last (plpio/read-piles r)) :pile)
                (into {}))))))

;; Writer
;; ------

(deftest stringify-mpileup-read
  (testing "without-ref"
    (are [?in ?out]
        (= ?out (#'plpio/stringify-mpileup-alignment nil "chr1" 10 nil ?in))
      {:base \A :reverse? false :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "A"
      {:base \N :reverse? false :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "N"
      {:base \A :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a"
      {:base \N :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "n"
      {:base \* :reverse? false :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "*"
      {:base \* :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "*"
      {:base \> :reverse? false :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} ">"
      {:base \> :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "<"
      {:base \A :reverse? false :insertion "A" :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "A+1A"
      {:base \A :reverse? false :deletion 2 :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "A-2NN"
      {:base \A :reverse? true :insertion "AT" :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a+2at"
      {:base \A :reverse? true :deletion 3 :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a-3nnn"
      {:base \A :reverse? false :start? true :mapq 60 :alignment {:flag 0 :mapq 60 :pos 10 :end 15}} "^]A"
      {:base \A :reverse? true :start? true :mapq 40 :insertion "AT" :alignment {:flag 16 :mapq 40 :pos 10 :end 15}} "^Ia+2at"
      {:base \A :reverse? false :end? true :alignment {:flag 0 :mapq 40 :pos 5 :end 10}} "A$"
      {:base \A :reverse? true :end? true :deletion 4 :alignment {:flag 16 :mapq 40 :pos 5 :end 10}} "a-4nnnn$"))
  (testing "with-ref"
    (let [r (reify p/ISequenceReader
              (p/read-sequence [this {:keys [start end]}]
                (subs "ATGCATGCATGCATGC" (dec start) end)))]
      (are [?in ?out]
          (= ?out (#'plpio/stringify-mpileup-alignment r "chr1" 10 \T ?in))
        {:base \A :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "A"
        {:base \T :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "."
        {:base \T :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} ","
        {:base \N :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "N"
        {:base \A :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a"
        {:base \N :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "n"
        {:base \* :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "*"
        {:base \* :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "*"
        {:base \> :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} ">"
        {:base \> :reverse? true :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "<"
        {:base \A :insertion "A" :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "A+1A"
        {:base \A :deletion 2 :alignment {:flag 0 :mapq 60 :pos 5 :end 15}} "A-2GC"
        {:base \A :reverse? true :insertion "AT" :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a+2at"
        {:base \A :reverse? true :deletion 3 :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a-3gca"
        {:base \A :start? true :mapq 60 :alignment {:flag 0 :mapq 60 :pos 10 :end 15}} "^]A"
        {:base \A :reverse? true :start? true :mapq 40 :insertion "AT" :alignment {:flag 16 :mapq 40 :pos 10 :end 15}} "^Ia+2at"
        {:base \A :end? true :alignment {:flag 0 :mapq 40 :pos 5 :end 10}} "A$"
        {:base \A :reverse? true :end? true :deletion 4 :alignment {:flag 16 :mapq 40 :pos 5 :end 10}} "a-4gcat$"))))

(defn string-sequence-reader [s]
  (reify p/ISequenceReader
    (p/read-sequence [this region]
      (p/read-sequence this region {}))
    (p/read-sequence [this {:keys [start end]} {:keys [mask?]}]
      ((if mask? identity cstr/upper-case)
       (subs s (dec start) end)))))

(deftest stringify-mpileup-line
  (testing "without-ref"
    (are [?in ?out]
        (= ?out (#'plpio/stringify-mpileup-line nil {:rname (first ?in)
                                                   :pos (second ?in)
                                                   :pile (last ?in)}))
      ["chr1" 10 []] "chr1\t10\tN\t0\t\t"
      ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tN\t1\tA\tI"
      ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}
                  {:base \A :qual 33 :reverse? true :start? true :mapq 60 :alignment {:flag 16 :mapq 60 :pos 10}}]] "chr1\t10\tN\t2\tA^]a\tIB"))
  (testing "with-ref"
    (let [r (string-sequence-reader "NNNNNNNNNAtGCATGCAT")]
      (are [?in ?out]
          (= ?out (#'plpio/stringify-mpileup-line r {:rname (first ?in)
                                                   :pos (second ?in)
                                                   :pile (last ?in)}))
        ["chr1" 10 []] "chr1\t10\tA\t0\t\t"
        ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tA\t1\t.\tI"
        ["chr1" 10 [{:base \A :insertion "AA" :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tA\t1\t.+2AA\tI"
        ["chr1" 10 [{:base \A :deletion 2 :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tA\t1\t.-2TG\tI"
        ["chr1" 11 [{:base \T :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t11\tt\t1\t.\tI"
        ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}
                    {:base \A :qual 33 :reverse? true :start? true :mapq 60 :alignment {:flag 16 :mapq 60 :pos 10}}]] "chr1\t10\tA\t2\t.^],\tIB"))))

(deftest writer
  (let [tmp (File/createTempFile "writer-test" ".mpileup")]
    (try
      (with-open [w (plpio/writer tmp)]
        (is (instance? PileupWriter w)))
      (finally
        (when (.isFile (cio/file tmp))
          (cio/delete-file (cio/file tmp)))))))

(deftest write-piles
  (is (= "chr1\t10\tN\t1\tA\tI\n"
         (with-open [sw (StringWriter.)
                     w (plpio/writer sw)]
           (plpio/write-piles w [{:rname "chr1"
                                  :pos 10
                                  :pile [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}]}])
           (.toString sw)))))


;; Read & Write
;; ------------

(deftest regression
  (testing "without-ref"
    (are [?input]
        (= ?input
           (with-open [r (plpio/reader (StringReader. ?input))
                       sw (StringWriter.)
                       w (plpio/writer sw)]
             (plpio/write-piles w (plpio/read-piles r))
             (str sw)))
      "chr1\t10\tN\t1\tA\tI\n"
      "chr1\t10\tN\t4\tAaTt\tIABC\n"
      "chr1\t10\tN\t4\t^]A+3TTTa-2nn$Tt\tIABC\n"))
  (testing "with-ref"
    (are [?input]
        (= ?input
           (let [tmp (File/createTempFile "pileup-regression" ".fa")
                 idx (File/createTempFile "pileup-regression" ".fai")]
             (try
               (with-open [w (cseq/writer (.getCanonicalPath tmp))]
                 (cseq/write-sequences w [{:name "chr1" :sequence "NNNNNNNNNATGC"}]))
               (fai/create-index tmp idx)
               (with-open [r (plpio/reader (StringReader. ?input))
                           sw (StringWriter.)
                           w (plpio/writer sw tmp)]
                 (plpio/write-piles w (plpio/read-piles r))
                 (str sw))
               (finally
                 (when (.isFile (cio/file tmp))
                   (cio/delete-file (cio/file tmp)))))))
      "chr1\t10\tA\t1\t.\tI\n"
      "chr1\t10\tA\t4\t.,Tt\tIABC\n"
      "chr1\t10\tA\t4\t^].+3TTT,-2tg$Tt\tIABC\n")))