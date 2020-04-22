(ns cljam.io.bigwig-test
  (:require [clojure.test :refer [deftest is are testing]]
            [clojure.java.io :as cio]
            [cljam.test-common :refer
             [http-server
              test-bigwig-variable-file
              test-bigwig-fixed-file
              test-bigwig-bedgraph-file
              test-bigwig-non-leaf-blocks-file]]
            [cljam.io.bigwig :as bigwig])
  (:import [cljam.io.bigwig BigWigHeaders FixedWidthHeader
            ZoomHeader TotalSummary BptHeader BbiChromInfo
            CirTree]))

(def ^:private ^BigWigHeaders test-bigwig-fixed-fields
  {:headers (BigWigHeaders.
             (FixedWidthHeader. 2291137574 4 2 152 200 368 112 32768 0)
             [(ZoomHeader. 132 6588 6629) (ZoomHeader. 528 12833 12874)]
             (TotalSummary. 110 0.5 79.6 4045.0 188969.5)
             nil
             (BptHeader. 1 4 8 1 184)
             [(BbiChromInfo. "chr7" 0 158821424)]
             (CirTree. 256 3 0 115597760 0 115597870 368 1 416))
   :tracks [{:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597761 :end 115597765 :value 19.0}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597766 :end 115597770 :value 19.1}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597771 :end 115597775 :value 59.2}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597776 :end 115597780 :value 39.3}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597781 :end 115597785 :value 39.4}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597786 :end 115597790 :value 0.5}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597791 :end 115597795 :value 0.6}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597796 :end 115597800 :value 59.7}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597801 :end 115597805 :value 19.8}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597806 :end 115597810 :value 19.9}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597811 :end 115597815 :value 59.0}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597816 :end 115597820 :value 39.0}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597821 :end 115597825 :value 39.1}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597826 :end 115597830 :value 39.2}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597831 :end 115597835 :value 39.3}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597836 :end 115597840 :value 39.4}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597841 :end 115597845 :value 19.5}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597846 :end 115597850 :value 79.6}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597851 :end 115597855 :value 39.7}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597856 :end 115597860 :value 39.8}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597861 :end 115597865 :value 39.9}
            {:track {:line nil :format :fixed-step :chr "chr7" :step 5 :span 5}
             :chr "chr7" :start 115597866 :end 115597870 :value 59.0}]})

(def ^:private ^BigWigHeaders test-bigwig-variable-fields
  {:headers (BigWigHeaders.
             (FixedWidthHeader. 2291137574 4 2 152 214 419 112 32768 0)
             [(ZoomHeader. 10428 6631 6706) (ZoomHeader. 41712 12910 12986)]
             (TotalSummary. 1050 -0.39 0.78 3.5 78.515)
             nil
             (BptHeader. 2 5 8 2 184)
             [(BbiChromInfo. "chr22" 0 49691432)
              (BbiChromInfo. "chrY"  1 57772954)]
             (CirTree. 256 2 0 14430065 1 87032 419 1 467))
   :tracks [{:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14430066 :end 14430115 :value 0.78}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14430166 :end 14430215 :value -0.05}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14430266 :end 14430315 :value -0.16}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14430366 :end 14430415 :value -0.39}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14432944 :end 14432993 :value 0.42}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14433044 :end 14433093 :value 0.03}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14433144 :end 14433193 :value -0.26}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14433244 :end 14433293 :value 0.08}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14433344 :end 14433393 :value -0.19}
            {:track {:line nil :format :variable-step :chr "chr22" :step nil :span 50}
             :chr "chr22" :start 14433444 :end 14433493 :value -0.20}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 85692 :end 85741 :value -0.22}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 85792 :end 85841 :value -0.35}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 85894 :end 85943 :value 0.34}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86283 :end 86332 :value 0.33}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86383 :end 86432 :value -0.15}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86483 :end 86532 :value 0.03}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86583 :end 86632 :value -0.05}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86683 :end 86732 :value -0.04}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86783 :end 86832 :value 0.16}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86883 :end 86932 :value 0.03}
            {:track {:line nil :format :variable-step :chr "chrY" :step nil :span 50}
             :chr "chrY" :start 86983 :end 87032 :value -0.07}]})

(def ^:private ^BigWigHeaders test-bigwig-bedgraph-fields
  {:headers (BigWigHeaders.
             (FixedWidthHeader. 2291137574 4 2 152 212 500 112 32768 0)
             [(ZoomHeader. 2462838 6712 6787) (ZoomHeader. 9851352 12991 13067)]
             (TotalSummary. 17002 14.6 25.1 321574.0 6240134.25)
             nil
             (BptHeader. 2 4 8 2 184)
             [(BbiChromInfo. "chr5" 0 180857866)
              (BbiChromInfo. "chr7" 1 158821424)]
             (CirTree. 256 2 0 131308806 1 117301101 500 1 548))
   ; NB: This format is a temporary structure until we support BedGraph formats.
   :tracks [{:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131308807 :end 131309339 :value 16.5}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131374792 :end 131375675 :value 19.7}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131590994 :end 131591853 :value 23.3}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131620939 :end 131621712 :value 16.5}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131634918 :end 131635788 :value 17.2}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131657130 :end 131658696 :value 17.9}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131733069 :end 131733870 :value 21.7}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131774125 :end 131774775 :value 22.1}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131853742 :end 131854834 :value 18.3}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131860255 :end 131860700 :value 20.6}
            {:track {:line nil :chr "chr5" :start 131308806 :end 131920856}
             :chr "chr5" :start 131920464 :end 131920856 :value 21.9}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 115637607 :end 115638264 :value 22.8}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 115927011 :end 115927588 :value 21.5}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 115951940 :end 115953971 :value 14.6}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 116099061 :end 116099834 :value 20.7}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 116289664 :end 116290156 :value 23.1}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 116381117 :end 116381561 :value 22.9}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 116750078 :end 116752039 :value 15.1}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 116854640 :end 116854902 :value 25.1}
            {:track {:line nil :chr "chr7" :start 115637606 :end 117301101}
             :chr "chr7" :start 117300179 :end 117301101 :value 20.6}]})

(def ^:private ^BigWigHeaders test-non-leaf-blocks-fields
  {:headers (BigWigHeaders.
             (FixedWidthHeader. 2291137574 4 2 152 201 362 112 96 0)
             [(ZoomHeader. 3000 670 713) (ZoomHeader. 12000 845 888)]
             (TotalSummary. 2000 100.0 1000.0 1100000.0 7.7E8)
             nil
             (BptHeader. 1 5 8 1 184)
             [(BbiChromInfo. "chr19" 0 63811651)]
             (CirTree. 3 4 0 49307400 0 49310300 362 1 410)),
   :tracks
   [{:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49307401 :end 49307600 :value 1000.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49307701 :end 49307900 :value 900.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49308001 :end 49308200 :value 800.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49308301 :end 49308500 :value 700.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49308601 :end 49308800 :value 600.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49308901 :end 49309100 :value 500.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49309201 :end 49309400 :value 400.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49309501 :end 49309700 :value 300.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49309801 :end 49310000 :value 200.0}
    {:track {:line nil :format :fixed-step :chr "chr19" :step 300 :span 200}
     :chr "chr19" :start 49310101 :end 49310300 :value 100.0}]})

(def ^:private ^:const eps 1e-4)

(defn- equal-floating-points-num? [^double a ^double b]
  (< (Math/abs (- a b)) eps))

(defn- same-bigwig-headers?
  "Returns true if the given arguments are same bigWig headers, otherwise
  false."
  [ah bh]
  (and (= (:fixed-width-header ah) (:fixed-width-header bh))
       (= (:zoom-headers ah) (:zoom-headers bh))
       (= (:auto-sql ah) (:auto-sql bh))
       (= (:extended-header ah) (:extended-header bh))
       (= (:bpt-header ah) (:bpt-header bh))
       (apply = (map #(map (juxt :name :id :size) (:bbi-chrom-info %)) [ah bh]))
       (= (:cir-tree ah) (:cir-tree bh))
       (let [x (:total-summary ah)
             y (:total-summary bh)]
         (and (= (:bases-covered x) (:bases-covered y))
              (equal-floating-points-num? (:min-val x) (:min-val y))
              (equal-floating-points-num? (:max-val x) (:max-val y))
              (equal-floating-points-num? (:sum-data x) (:sum-data y))
              (equal-floating-points-num? (:sum-squared x) (:sum-squared y))))))

(defn- same-wig?
  "Returns true if the given arguments are same wig formats and same values."
  [as bs]
  (->> (map (fn [a b]
              (and (= (:track a) (:track b))
                   (= (:chr a) (:chr b))
                   (= (:start a) (:start b))
                   (= (:end a) (:end b))
                   (equal-floating-points-num? (:value a) (:value b))))
            as bs)
       (every? true?)))

(defn- same-bedgraph?
  "Returns true if the given arguments are same BedGraph values."
  [as bs]
  (->> (map (fn [a b]
              (and (= (:track a) (:track b))
                   (= (:chr a) (:chr b))
                   (= (:start a) (:start b))
                   (= (:end a) (:end b))
                   (equal-floating-points-num? (:value a) (:value b))))
            as bs)
       (every? true?)))

(deftest reader
  (testing "make reader instance"
    (with-open [rdr (bigwig/reader test-bigwig-variable-file)]
      (is (instance? cljam.io.bigwig.BIGWIGReader rdr))))

  (testing "throw Exception"
    (is (thrown? Exception (bigwig/reader "test-resources/bigwig/not-found.bigWig"))))

  (testing "read bigWig file"
    (with-open [r (bigwig/reader test-bigwig-fixed-file)]
      (is (and (same-bigwig-headers? (.headers r)
                                     (:headers test-bigwig-fixed-fields))
               (same-wig? (bigwig/read-tracks r)
                          (:tracks test-bigwig-fixed-fields)))))
    (with-open [r (bigwig/reader test-bigwig-variable-file)]
      (is (and (same-bigwig-headers? (.headers r)
                                     (:headers test-bigwig-variable-fields))
               (same-wig? (bigwig/read-tracks r)
                          (:tracks test-bigwig-variable-fields)))))
    (with-open [r (bigwig/reader test-bigwig-bedgraph-file)]
      (is (and (same-bigwig-headers? (.headers r)
                                     (:headers test-bigwig-bedgraph-fields))
               (same-bedgraph? (bigwig/read-tracks r)
                               (:tracks test-bigwig-bedgraph-fields)))))
    (with-open [r (bigwig/reader test-bigwig-non-leaf-blocks-file)]
      (is (and (same-bigwig-headers? (.headers r)
                                     (:headers test-non-leaf-blocks-fields))
               (same-wig? (bigwig/read-tracks r)
                          (:tracks test-non-leaf-blocks-fields)))))))

(deftest source-type-test
  (testing "reader"
    (are [x] (with-open [r (bigwig/reader x)]
               (and (same-bigwig-headers? (.headers r)
                                          (:headers test-bigwig-fixed-fields))
                    (same-wig? (bigwig/read-tracks r)
                               (:tracks test-bigwig-fixed-fields))))
      test-bigwig-fixed-file
      (cio/file test-bigwig-fixed-file)
      (cio/as-url (cio/file test-bigwig-fixed-file))))
  (testing "reader (non-file URL is not supported)"
    (with-open [server (http-server)]
      (let [x (cio/as-url (str (:uri server) "/bigwig/test-fixed.bigWig"))]
        (is (thrown? Exception
                     (with-open [r (bigwig/reader x)]
                       (bigwig/read-tracks r))))))))
