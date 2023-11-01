(ns cljam.util.dataframe-test
  (:require [cljam.util.dataframe :as df]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest make-dataframe-buffer-test
  (testing "exactly the declared size"
    (let [buf (doto (df/make-dataframe-buffer 3
                                              [[:f1 :boolean]
                                               [:f2 :byte]
                                               [:f3 :char]
                                               [:f4 :short]
                                               [:f5 :int]
                                               [:f6 :long]
                                               [:f7 :float]
                                               [:f8 :double]
                                               [:f9 :object]])
                (df/append-row! [false  0 \a  1  2  3  4.0  5.0 "bc"])
                (df/append-row! [true   6 \d  7  8  9 10.0 11.0 "ef"])
                (df/append-row! [false 12 \g 13 14 15 16.0 17.0 "hi"]))
          df (df/->dataframe! buf)]
      (is (= 3 (count df)))
      (is (= [{:f1 false :f2  0 :f3 \a :f4  1 :f5  2 :f6  3 :f7  4.0 :f8  5.0 :f9 "bc"}
              {:f1 true  :f2  6 :f3 \d :f4  7 :f5  8 :f6  9 :f7 10.0 :f8 11.0 :f9 "ef"}
              {:f1 false :f2 12 :f3 \g :f4 13 :f5 14 :f6 15 :f7 16.0 :f8 17.0 :f9 "hi"}]
             (seq df)))))
  (testing "smaller than the declared size"
    (let [buf (doto (df/make-dataframe-buffer 3
                                              [[:f1 :boolean]
                                               [:f2 :byte]
                                               [:f3 :char]
                                               [:f4 :short]
                                               [:f5 :int]
                                               [:f6 :long]
                                               [:f7 :float]
                                               [:f8 :double]
                                               [:f9 :object]])
                (df/append-row! [false 0 \a 1 2 3  4.0  5.0 "bc"])
                (df/append-row! [true  6 \d 7 8 9 10.0 11.0 "ef"]))
          df (df/->dataframe! buf)]
      (is (= 2 (count df)))
      (is (= [{:f1 false :f2  0 :f3 \a :f4  1 :f5  2 :f6  3 :f7  4.0 :f8  5.0 :f9 "bc"}
              {:f1 true  :f2  6 :f3 \d :f4  7 :f5  8 :f6  9 :f7 10.0 :f8 11.0 :f9 "ef"}]
             (seq df)))))
  (testing "smaller than half the declared size"
    (let [buf (doto (df/make-dataframe-buffer 3
                                              [[:f1 :boolean]
                                               [:f2 :byte]
                                               [:f3 :char]
                                               [:f4 :short]
                                               [:f5 :int]
                                               [:f6 :long]
                                               [:f7 :float]
                                               [:f8 :double]
                                               [:f9 :object]])
                (df/append-row! [false 0 \a 1 2 3 4.0 5.0 "bc"]))
          df (df/->dataframe! buf)]
      (is (= 1 (count df)))
      (is (= [{:f1 false :f2 0 :f3 \a :f4 1 :f5 2 :f6 3 :f7 4.0 :f8  5.0 :f9 "bc"}]
             (seq df))))))

(deftest make-dataframe-test
  (let [df (df/make-dataframe
            [[:f1 (boolean-array [false true])]
             [:f2 (byte-array [1 2])]
             [:f3 (char-array [\a \b])]
             [:f4 (short-array [3 4])]
             [:f5 (int-array [5 6])]
             [:f6 (long-array [7 8])]
             [:f7 (float-array [9.0 10.0])]
             [:f8 (double-array [11.0 12.0])]
             [:f9 (object-array ["abc" "def"])]])]
    (is (= [{:f1 false :f2 1 :f3 \a :f4 3 :f5 5 :f6 7 :f7  9.0 :f8 11.0 :f9 "abc"}
            {:f1 true  :f2 2 :f3 \b :f4 4 :f5 6 :f6 8 :f7 10.0 :f8 12.0 :f9 "def"}]
           (seq df)))))

(deftest print-method-test
  (let [df (df/make-dataframe
            [[:chr (object-array ["chr1" "chr2" "chr3"])]
             [:pos (int-array [123 456 789])]
             [:qual (double-array [10.0 20.0 30.0])]])]
    (is (= [{:chr "chr1" :pos 123 :qual 10.0}
            {:chr "chr2" :pos 456 :qual 20.0}
            {:chr "chr3" :pos 789 :qual 30.0}]
           (read-string (pr-str df))))
    (is (= [{:chr "1" :pos 123 :qual 10.0 :filter [:PASS]}
            {:chr "2" :pos 456 :qual 20.0 :filter [:PASS]}
            {:chr "3" :pos 789 :qual 30.0 :filter [:PASS]}]
           (->> df
                (map #(-> %
                          (update :chr str/replace #"^chr" "")
                          (assoc :filter [:PASS])))
                pr-str
                read-string)))))

(deftest dataframe-ops-test
  (let [df (df/make-dataframe
            [[:chr (object-array ["chr1" "chr2" "chr3"])]
             [:pos (int-array [123 456 789])]
             [:qual (double-array [10.0 20.0 30.0])]])]
    (testing "nth"
      (is (= {:chr "chr1" :pos 123 :qual 10.0} (nth df 0)))
      (is (= {:chr "chr2" :pos 456 :qual 20.0} (nth df 1)))
      (is (= {:chr "chr3" :pos 789 :qual 30.0} (nth df 2 ::not-found)))
      (is (= ::not-found (nth df 3 ::not-found)))
      (is (thrown? Throwable (nth df 3))))
    (testing "get"
      (is (= {:chr "chr1" :pos 123 :qual 10.0} (get df 0)))
      (is (= {:chr "chr2" :pos 456 :qual 20.0} (get df 1)))
      (is (= {:chr "chr3" :pos 789 :qual 30.0} (get df 2 ::not-found)))
      (is (= ::not-found (get df 3 ::not-found)))
      (is (= nil (get df 3))))
    (testing "reduce"
      (is (= [{:chr "chr1" :pos 123 :qual 10.0}
              {:chr "chr3" :pos 789 :qual 30.0}]
             (reduce (fn [acc row]
                       (cond-> acc
                         (odd? (:pos row))
                         (conj row)))
                     [] df))))
    (testing "chunk-cons"
      (is (= [{:chr "chr1" :pos 123 :qual 10.0}
              {:chr "chr2" :pos 456 :qual 20.0}
              {:chr "chr3" :pos 789 :qual 30.0}
              {:chr "chr1" :pos 123 :qual 10.0}
              {:chr "chr2" :pos 456 :qual 20.0}
              {:chr "chr3" :pos 789 :qual 30.0}]
             (chunk-cons df (chunk-cons df nil)))))))

(deftest dataframe-seq-ops-test
  (let [s (seq
           (df/make-dataframe
            [[:chr (object-array ["chr1" "chr2" "chr3"])]
             [:pos (int-array [123 456 789])]
             [:qual (double-array [10.0 20.0 30.0])]]))]
    (testing "count"
      (is (= 3 (count s))))
    (testing "conj"
      (is (= [{:chr "chr1" :pos 1 :qual 0.0}
              {:chr "chr1" :pos 123 :qual 10.0}
              {:chr "chr2" :pos 456 :qual 20.0}
              {:chr "chr3" :pos 789 :qual 30.0}]
             (conj s {:chr "chr1" :pos 1 :qual 0.0}))))
    (testing "empty"
      (is (= () (empty s))))
    (testing "equiv"
      ;; to test the equality of dataframe seqs properly, s must come in the first
      ;; argument of `=` here
      (is (= s
             [{:chr "chr1" :pos 123 :qual 10.0}
              {:chr "chr2" :pos 456 :qual 20.0}
              {:chr "chr3" :pos 789 :qual 30.0}]))
      (is (not= s
                [{:chr "chr1" :pos 123 :qual 10.0}
                 {:chr "chr3" :pos 789 :qual 30.0}
                 {:chr "chr2" :pos 456 :qual 20.0}]))
      (is (not= s
                #{{:chr "chr1" :pos 123 :qual 10.0}
                  {:chr "chr2" :pos 456 :qual 20.0}
                  {:chr "chr3" :pos 789 :qual 30.0}})))))

(deftest dataframe-row-ops-test
  (let [row (first
             (df/make-dataframe
              [[:chr (object-array ["chr1"])]
               [:pos (int-array [123])]
               [:qual (double-array [10.0])]]))]
    (testing "count"
      (is (= 3 (count row))))
    (testing "get"
      (is (= "chr1" (get row :chr)))
      (is (= 123 (get row :pos)))
      (is (= 10.0 (get row :qual)))
      (is (= 10.0 (get row :qual ::not-found)))
      (is (= ::not-found (get row :unknown ::not-found)))
      (is (nil? (get row :unknown))))
    (testing "key access"
      (is (= "chr1" (:chr row)))
      (is (= 123 (:pos row)))
      (is (= 10.0 (:qual row)))
      (is (= 10.0 (:qual row ::not-found)))
      (is (= ::not-found (:unknown row ::not-found)))
      (is (nil? (:unknown row))))
    (testing "find"
      (is (= [:chr "chr1"] (find row :chr)))
      (is (= [:pos 123] (find row :pos)))
      (is (= [:qual 10.0] (find row :qual)))
      (is (nil? (find row :unknown))))
    (testing "assoc"
      (is (= {:chr "1" :pos 123 :qual 10.0}
             (assoc row :chr "1")))
      (is (= {:chr "chr1" :pos 123 :qual 10.0 :filter [:PASS]}
             (assoc row :filter [:PASS]))))
    (testing "dissoc"
      (is (= {:chr "chr1" :pos 123} (dissoc row :qual))))
    (testing "empty"
      (is (= {} (empty row))))
    (testing "conj"
      (is (= {:chr "chr1" :pos 123 :qual 10.0 :filter [:PASS]}
             (conj row [:filter [:PASS]]))))))

(deftest extended-dataframe-row-ops-test
  (let [row (-> (df/make-dataframe
                 [[:chr (object-array ["chr1"])]
                  [:pos (int-array [123])]
                  [:qual (double-array [10.0])]])
                first
                (update :chr str/replace #"^chr" "")
                (assoc :filter [:PASS]))]
    (testing "count"
      (is (= 4 (count row))))
    (testing "get"
      (is (= "1" (get row :chr)))
      (is (= 123 (get row :pos)))
      (is (= 10.0 (get row :qual ::not-found)))
      (is (= [:PASS] (get row :filter ::not-found)))
      (is (= ::not-found (get row :unknown ::not-found)))
      (is (nil? (get row :unknown))))
    (testing "key access"
      (is (= "1" (:chr row)))
      (is (= 123 (:pos row)))
      (is (= 10.0 (:qual row)))
      (is (= [:PASS] (:filter row ::not-found)))
      (is (= ::not-found (:unknown row ::not-found)))
      (is (nil? (:unknown row))))
    (testing "find"
      (is (= [:chr "1"] (find row :chr)))
      (is (= [:pos 123] (find row :pos)))
      (is (= [:qual 10.0] (find row :qual)))
      (is (= [:filter [:PASS]] (find row :filter)))
      (is (nil? (find row :unknown))))
    (testing "assoc"
      (is (= {:chr "1" :pos 120 :qual 10.0 :filter [:PASS]}
             (assoc row :pos 120)))
      (is (= {:chr "1" :pos 123 :qual 10.0 :filter [:q10]}
             (assoc row :filter [:q10]))))
    (testing "dissoc"
      (is (= {:pos 123 :qual 10.0 :filter [:PASS]} (dissoc row :chr)))
      (is (= {:chr "1" :qual 10.0 :filter [:PASS]} (dissoc row :pos)))
      (is (= {:chr "1" :pos 123 :qual 10.0} (dissoc row :filter)))
      (is (= {:chr "1" :pos 123 :qual 10.0 :filter [:PASS]} (dissoc row :unknown))))
    (testing "empty"
      (is (= {} (empty row))))
    (testing "conj"
      (is (= {:chr "1" :pos 120 :qual 10.0 :filter [:PASS]}
             (conj row [:pos 120])))
      (is (= {:chr "1" :pos 123 :qual 10.0 :filter [:q10]}
             (conj row [:filter [:q10]]))))
    (testing "equiv"
      ;; to test the equality of extended dataframe rows properly, row must come
      ;; in the first argument of `=` here
      (is (= row {:chr "1" :pos 123 :qual 10.0 :filter [:PASS]}))
      (is (not= row {:chr "1" :pos 123 :qual 10.0}))
      (is (not= row {:chr "1" :pos 123 :qual 10.0 :filter [:q10]}))
      (is (not= row {:chr "1" :pos 0 :qual 10.0 :filter [:PASS]}))
      (is (not= row "foo")))))

(deftest add-columns-test
  (let [df (df/make-dataframe
            [[:chr (object-array ["chr1" "chr2" "chr3"])]
             [:pos (int-array [123 456 789])]
             [:qual (double-array [10.0 20.0 30.0])]])
        refs (char-array [\G \T \A])
        alts (object-array [[\A] [\A] [\G \T]])]
    (is (= [{:chr "chr1" :pos 123 :qual 10.0 :ref \G :alt [\A]}
            {:chr "chr2" :pos 456 :qual 20.0 :ref \T :alt [\A]}
            {:chr "chr3" :pos 789 :qual 30.0 :ref \A :alt [\G \T]}]
           (seq (df/add-columns df [[:ref refs] [:alt alts]]))))))

(deftest map-column-test
  (let [df (df/make-dataframe
            [[:chr (object-array ["chr1" "chr2" "chr3"])]
             [:pos (int-array [123 456 789])]
             [:qual (double-array [10.0 20.0 30.0])]])]
    (is (= [{:chr "1" :pos 123 :qual 10.0}
            {:chr "2" :pos 456 :qual 20.0}
            {:chr "3" :pos 789 :qual 30.0}]
           (seq (df/map-column df :chr #(str/replace % #"^chr" "")))))))

(deftest drop-columns-test
  (let [df (df/make-dataframe
            [[:chr (object-array ["chr1" "chr2" "chr3"])]
             [:pos (int-array [123 456 789])]
             [:qual (double-array [10.0 20.0 30.0])]
             [:filter (object-array [[:PASS] [:PASS] [:PASS]])]])]
    (is (= [{:chr "chr1" :pos 123}
            {:chr "chr2" :pos 456}
            {:chr "chr3" :pos 789}]
           (seq (df/drop-columns df [:qual :filter]))))))
