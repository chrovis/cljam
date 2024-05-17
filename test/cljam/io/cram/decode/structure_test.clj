(ns cljam.io.cram.decode.structure-test
  (:require [cljam.io.cram.decode.structure :as struct]
            [cljam.io.sam :as sam]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.test-common :as common]
            [clojure.java.io :as cio]
            [clojure.test :refer [deftest is]])
  (:import [java.io FileInputStream]
           [java.nio ByteOrder]
           [java.nio.channels FileChannel$MapMode]))

(defn- bytes->vec [bs]
  (mapv #(bit-and (long %) 0xff) bs))

(defn- decode-container-header [bb]
  (-> (struct/decode-container-header bb)
      (update :landmarks bytes->vec)
      (update :crc bytes->vec)))

(deftest cram-file-structure-decode-test
  (with-open [fis (FileInputStream. (cio/file common/medium-with-standard-tags-cram-file))
              ch (.getChannel fis)]
    (let [bb (-> ch
                 (.map FileChannel$MapMode/READ_ONLY 0 (.size ch))
                 (.order ByteOrder/LITTLE_ENDIAN))]
      (is (= {:version {:major 3 :minor 0}
              :id "file:///tmp/cljam/me"}
             (struct/decode-file-definition bb)))
      (is (= {:length 278
              :ref-seq-id -1
              :start 0
              :span 0
              :records 0
              :counter 0
              :bases 0
              :blocks 1
              :landmarks []
              :crc [240 201 140 91]}
             (decode-container-header bb)))
      (is (= (with-open [r (sam/reader common/medium-bam-file)]
               (merge {:HD {:VN "1.6" :SO "unsorted"}}
                      (sam/read-header r)))
             (struct/decode-cram-header-block bb)))
      (is (= {:length 362828
              :ref-seq-id -2
              :start 0
              :span 0
              :records 10000
              :counter 0
              :bases 760000
              :blocks 31
              :landmarks [207]
              :crc [125 112 223 241]}
             (decode-container-header bb)))
      (is (= {:preservation-map
              {:RN true
               :AP false
               :RR true
               :SM
               {\A {0 \G, 1 \C, 2 \T, 3 \N}
                \C {0 \G, 1 \T, 2 \A, 3 \N}
                \G {0 \A, 1 \C, 2 \T, 3 \N}
                \T {0 \C, 1 \G, 2 \A, 3 \N}
                \N {0 \A, 1 \C, 2 \G, 3 \T}}
               :TD [[]
                    [{:tag :MD, :type \Z} {:tag :NM, :type \c}]]}
              :data-series
              {:BF {:codec :external, :content-id 1}
               :CF {:codec :external, :content-id 2}
               :RI {:codec :external, :content-id 3}
               :RL {:codec :external, :content-id 4}
               :AP {:codec :external, :content-id 5}
               :RG {:codec :external, :content-id 6}
               :RN {:codec :byte-array-stop, :stop-byte 9, :external-id 7}
               :NF {:codec :external, :content-id 8}
               :MF {:codec :external, :content-id 9}
               :NS {:codec :external, :content-id 10}
               :NP {:codec :external, :content-id 11}
               :TS {:codec :external, :content-id 12}
               :TL {:codec :external, :content-id 13}
               :MQ {:codec :external, :content-id 16}
               :FN {:codec :external, :content-id 17}
               :FP {:codec :external, :content-id 18}
               :FC {:codec :external, :content-id 19}
               :BA {:codec :external, :content-id 22}
               :QS {:codec :external, :content-id 23}
               :BS {:codec :external, :content-id 24}
               :IN {:codec :byte-array-stop, :stop-byte 9, :external-id 25}
               :DL {:codec :external, :content-id 26}
               :RS {:codec :external, :content-id 27}
               :SC {:codec :byte-array-stop, :stop-byte 9, :external-id 28}
               :PD {:codec :external, :content-id 29}
               :HC {:codec :external, :content-id 30}}
              :tags
              {:MD
               {\Z {:codec :byte-array-stop, :stop-byte 9, :external-id 5063770}}
               :NM
               {\c
                {:codec :byte-array-len
                 :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
                 :val-encoding {:codec :external, :content-id 5131619}}}}}
             (struct/decode-compression-header-block bb)))
      (is (= {:tags []
              :content-ids
              [1 2 3 4 5 6 7 8 9 10 11 12 13 16 17 18 19
               22 23 24 25 26 27 28 29 30 5063770 5131619]
              :start 0
              :reference-md5 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              :records 10000
              :counter 0
              :blocks 29
              :embedded-reference -1
              :ref-seq-id -2
              :span 0}
             (-> (struct/decode-slice-header-block bb)
                 (update :reference-md5 bytes->vec))))
      (is (= [{:method 0, :content-type 5, :content-id 0, :size 0, :raw-size 0, :crc [47 7 252 241]}
              {:method 4, :content-type 4, :content-id 1, :size 1039, :raw-size 10000, :crc [202 95 54 132]}
              {:method 4, :content-type 4, :content-id 2, :size 36, :raw-size 10000, :crc [87 22 235 242]}
              {:method 4, :content-type 4, :content-id 3, :size 6684, :raw-size 18308, :crc [1 15 226 78]}
              {:method 4, :content-type 4, :content-id 4, :size 36, :raw-size 10000, :crc [166 191 130 64]}
              {:method 4, :content-type 4, :content-id 5, :size 31125, :raw-size 33411, :crc [179 8 47 242]}
              {:method 4, :content-type 4, :content-id 6, :size 36, :raw-size 50000, :crc [249 83 125 26]}
              {:method 1, :content-type 4, :content-id 7, :size 77500, :raw-size 411343, :crc [1 19 161 227]}
              {:method 1, :content-type 4, :content-id 8, :size 20, :raw-size 0, :crc [100 132 79 154]}
              {:method 1, :content-type 4, :content-id 9, :size 45, :raw-size 10000, :crc [90 129 139 84]}
              {:method 4, :content-type 4, :content-id 10, :size 36, :raw-size 50000, :crc [243 129 185 33]}
              {:method 1, :content-type 4, :content-id 11, :size 45, :raw-size 10000, :crc [61 217 74 145]}
              {:method 4, :content-type 4, :content-id 12, :size 31, :raw-size 10000, :crc [46 244 148 239]}
              {:method 1, :content-type 4, :content-id 13, :size 48, :raw-size 10000, :crc [239 168 203 254]}
              {:method 1, :content-type 4, :content-id 16, :size 1977, :raw-size 7923, :crc [248 27 134 197]}
              {:method 1, :content-type 4, :content-id 17, :size 2046, :raw-size 7923, :crc [61 142 4 22]}
              {:method 1, :content-type 4, :content-id 18, :size 3458, :raw-size 4616, :crc [61 182 157 145]}
              {:method 1, :content-type 4, :content-id 19, :size 271, :raw-size 4616, :crc [88 186 74 54]}
              {:method 4, :content-type 4, :content-id 22, :size 37161, :raw-size 157901, :crc [226 220 219 56]}
              {:method 4, :content-type 4, :content-id 23, :size 187340, :raw-size 760000, :crc [227 117 208 184]}
              {:method 1, :content-type 4, :content-id 24, :size 1377, :raw-size 4493, :crc [28 234 48 60]}
              {:method 1, :content-type 4, :content-id 25, :size 20, :raw-size 0, :crc [252 232 78 0]}
              {:method 1, :content-type 4, :content-id 26, :size 47, :raw-size 73, :crc [87 78 224 195]}
              {:method 1, :content-type 4, :content-id 27, :size 20, :raw-size 0, :crc [21 59 216 237]}
              {:method 1, :content-type 4, :content-id 28, :size 23, :raw-size 3, :crc [186 219 248 202]}
              {:method 1, :content-type 4, :content-id 29, :size 20, :raw-size 0, :crc [111 73 18 0]}
              {:method 1, :content-type 4, :content-id 30, :size 20, :raw-size 0, :crc [82 112 247 118]}
              {:method 4, :content-type 4, :content-id 5063770, :size 10053, :raw-size 43243, :crc [246 219 153 220]}
              {:method 4, :content-type 4, :content-id 5131619, :size 1492, :raw-size 7923, :crc [161 9 102 22]}]
             (mapv (fn [_]
                     (-> (struct/decode-block bb)
                         (update :crc bytes->vec)
                         (dissoc :data)))
                   (range 29))))
      (let [container-header (decode-container-header bb)]
        (is (= {:length 98171
                :ref-seq-id -1
                :start 0
                :span 0
                :records 2271
                :counter 10000
                :bases 172596
                :blocks 31
                :landmarks [171]
                :crc [111 230 105 210]}
               container-header))
        (bb/skip bb (:length container-header))
        (is (struct/eof-container? (decode-container-header bb)))))))
