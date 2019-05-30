(ns cljam.io.pileup-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [clojure.walk :as walk]
            [cljam.test-common :refer :all]
            [cljam.io.pileup :as plpio]
            [cljam.io.sequence :as cseq]
            [cljam.io.fasta-index.core :as fai]
            [cljam.io.protocols :as p])
  (:import [java.io File StringWriter StringReader]
           [cljam.io.pileup PileupReader PileupWriter]))

(defn- rec->map [coll]
  (walk/postwalk
   (fn [x]
     (if (record? x)
       (into {} x)
       x))
   coll))

(def ^:private ^:const the-last-pile
  ;; ref2	36	N	1	A$	?
  {:rname "ref2",
   :pos 36,
   :ref \N,
   :count 1,
   :pile
   [{:start? false,
     :mapq nil,
     :base \A,
     :qual 30,
     :reverse? false,
     :end? true,
     :insertion nil,
     :deletion nil,
     :qname nil,
     :alignment nil}]})

;; Reader
;; ------

(deftest parse-bases-col
  (testing "without-ref"
    (are [?in ?out]
         (= (mapv #(into {:qname nil :alignment nil} %) ?out)
            (mapv rec->map (#'plpio/parse-bases-col nil ?in)))
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
         (= (mapv #(into {:qname nil :alignment nil} %) ?out)
            (mapv rec->map (#'plpio/parse-bases-col \T ?in)))
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
         (= ?out (rec->map (#'plpio/parse-pileup-line ?in)))
      "chr1\t10\tN\t0\t\t" {:rname "chr1", :pos 10, :ref \N, :count 0,
                            :pile []}
      "chr1\t10\tN\t1\tA\tI" {:rname "chr1", :pos 10, :ref \N, :count 1,
                              :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? false, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}]}
      "chr1\t10\tN\t2\taA\tIB" {:rname "chr1", :pos 10, :ref \N, :count 2,
                                :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}
                                       {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}]}
      "chr1\t10\tN\t2\taA-2NN$\tIB" {:rname "chr1", :pos 10, :ref \N, :count 2,
                                     :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}
                                            {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? true, :insertion nil, :deletion 2, :qname nil, :alignment nil}]}))
  (testing "with-ref"
    (are [?in ?out]
         (= ?out (rec->map (#'plpio/parse-pileup-line ?in)))
      "chr1\t10\tA\t0\t\t" {:rname "chr1", :pos 10, :ref \A, :count 0,
                            :pile []}
      "chr1\t10\ta\t1\t.\tI" {:rname "chr1", :pos 10, :ref \a, :count 1,
                              :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? false, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}]}
      "chr1\t10\tA\t2\t,.\tIB" {:rname "chr1", :pos 10, :ref \A, :count 2,
                                :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}
                                       {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}]}
      "chr1\t10\tA\t2\t,.-2CA$\tIB" {:rname "chr1", :pos 10, :ref \A, :count 2,
                                     :pile [{:start? false, :mapq nil, :base \A, :qual 40, :reverse? true, :end? false, :insertion nil, :deletion nil, :qname nil, :alignment nil}
                                            {:start? false, :mapq nil, :base \A, :qual 33, :reverse? false, :end? true, :insertion nil, :deletion 2, :qname nil, :alignment nil}]})))

(deftest reader
  (with-open [r (plpio/reader test-pileup-file)]
    (is (instance? PileupReader r))))

(deftest read-piles
  (with-open [r (plpio/reader test-pileup-file)]
    (is (= the-last-pile
           (rec->map (last (plpio/read-piles r)))))))

;; Writer
;; ------

(defmacro with-string-writer [symbol & exprs]
  `(with-open [sw# (StringWriter.)
               ~symbol (cio/writer sw#)]
     ~@exprs
     (.flush ~symbol)
     (str sw#)))

(deftest write-mpileup-alignment!
  (testing "without-ref"
    (are [?in ?out]
         (= ?out (with-string-writer w
                   (#'plpio/write-mpileup-alignment! w nil "chr1" 10 nil ?in)))
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
      {:base \A :reverse? true :insertion "AAAATTTTGGGGCCCC" :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a+16aaaattttggggcccc"
      {:base \A :reverse? true :deletion 3 :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a-3nnn"
      {:base \A :reverse? true :deletion 10 :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a-10nnnnnnnnnn"
      {:base \A :reverse? false :start? true :mapq 60 :alignment {:flag 0 :mapq 60 :pos 10 :end 15}} "^]A"
      {:base \A :reverse? false :start? true :mapq 93 :alignment {:flag 0 :mapq 93 :pos 10 :end 15}} "^~A"
      {:base \A :reverse? false :start? true :mapq 94 :alignment {:flag 0 :mapq 94 :pos 10 :end 15}} "^~A"
      {:base \A :reverse? true :start? true :mapq 40 :insertion "AT" :alignment {:flag 16 :mapq 40 :pos 10 :end 15}} "^Ia+2at"
      {:base \A :reverse? false :end? true :alignment {:flag 0 :mapq 40 :pos 5 :end 10}} "A$"
      {:base \A :reverse? true :end? true :deletion 4 :alignment {:flag 16 :mapq 40 :pos 5 :end 10}} "a-4nnnn$"))
  (testing "with-ref"
    (let [r (reify p/ISequenceReader
              (p/read-sequence [this {:keys [start end]}]
                (subs "ATGCATGCATGCATGCATGCATGCATGC" (dec start) end)))]
      (are [?in ?out]
           (= ?out (with-string-writer w
                     (#'plpio/write-mpileup-alignment! w r "chr1" 10 \T ?in)))
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
        {:base \A :reverse? true :insertion "AAAATTTTGGGGCCCC" :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a+16aaaattttggggcccc"
        {:base \A :reverse? true :deletion 3 :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a-3gca"
        {:base \A :reverse? true :deletion 10 :alignment {:flag 16 :mapq 60 :pos 5 :end 15}} "a-10gcatgcatgc"
        {:base \A :start? true :mapq 60 :alignment {:flag 0 :mapq 60 :pos 10 :end 15}} "^]A"
        {:base \A :start? true :mapq 93 :alignment {:flag 0 :mapq 93 :pos 10 :end 15}} "^~A"
        {:base \A :start? true :mapq 94 :alignment {:flag 0 :mapq 94 :pos 10 :end 15}} "^~A"
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

(deftest write-mpileup-line!
  (testing "without-ref"
    (are [?in ?out]
         (= ?out (with-string-writer w
                   (#'plpio/write-mpileup-line! w nil {:rname (first ?in)
                                                       :pos (second ?in)
                                                       :pile (last ?in)})))
      ["chr1" 10 []] "chr1\t10\tN\t0\t\t"
      ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tN\t1\tA\tI"
      ["chr1" 10 [{:base \A :qual 93 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tN\t1\tA\t~"
      ["chr1" 10 [{:base \A :qual 94 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tN\t1\tA\t~"
      ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}
                  {:base \A :qual 33 :reverse? true :start? true :mapq 60 :alignment {:flag 16 :mapq 60 :pos 10}}]] "chr1\t10\tN\t2\tA^]a\tIB"))
  (testing "with-ref"
    (let [r (string-sequence-reader "NNNNNNNNNAtGCATGCAT")]
      (are [?in ?out]
           (= ?out (with-string-writer w
                     (#'plpio/write-mpileup-line! w r {:rname (first ?in)
                                                       :pos (second ?in)
                                                       :pile (last ?in)})))
        ["chr1" 10 []] "chr1\t10\tA\t0\t\t"
        ["chr1" 10 [{:base \A :qual 40 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tA\t1\t.\tI"
        ["chr1" 10 [{:base \A :qual 93 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tA\t1\t.\t~"
        ["chr1" 10 [{:base \A :qual 94 :alignment {:flag 0 :pos 5}}]] "chr1\t10\tA\t1\t.\t~"
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

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [x] (= the-last-pile
                  (with-open [r (plpio/reader x)]
                    (rec->map (last (plpio/read-piles r)))))
        test-pileup-file
        (cio/file test-pileup-file)
        (cio/as-url (cio/file test-pileup-file))
        (cio/as-url (str (:uri server) "/pileup/test.pileup")))))

  (testing "writer"
    (let [tmp-pileup-file (cio/file temp-dir "pileup-source-type-writer.mpileup")]
      (are [x] (with-before-after {:before (prepare-cache!)
                                   :after (clean-cache!)}
                 (with-open [w (plpio/writer x)]
                   (not-throw? (plpio/write-piles w [the-last-pile]))))
        (.getCanonicalPath tmp-pileup-file)
        tmp-pileup-file
        (cio/as-url tmp-pileup-file)))))
