(ns cljam.io.cram.data-series-test
  (:require [cljam.io.bam.decoder :as decoder]
            [cljam.io.cram.bit-stream :as bs]
            [cljam.io.cram.data-series :as ds]
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.io.util.lsb.io-stream :as lsb]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io ByteArrayOutputStream]))

(deftest build-data-series-decoders-test
  (let [encodings {:BA {:codec :external, :content-id 1}
                   :BF {:codec :external, :content-id 2}
                   :MQ {:codec :huffman, :alphabet [60], :bit-len [0]}
                   :RL {:codec :beta, :offset 149, :length 2}
                   :BB {:codec :byte-array-len
                        :len-encoding {:codec :external, :content-id 3}
                        :val-encoding {:codec :external, :content-id 4}}
                   :RN {:codec :byte-array-stop, :stop-byte 0, :content-id 5}}
        blocks [{:content-id 0
                 :data (bb/make-lsb-byte-buffer (byte-array [2r01001110]))}
                {:content-id 1
                 :data (bb/make-lsb-byte-buffer (byte-array (.getBytes "ATGC")))}
                {:content-id 2
                 :data (->> (byte-array [0x80 0xa1 0x80 0x63 0x80 0xa3 0x80 0x63])
                            bb/make-lsb-byte-buffer)}
                {:content-id 3
                 :data (bb/make-lsb-byte-buffer (byte-array [3 5 4 3]))}
                {:content-id 4
                 :data (bb/make-lsb-byte-buffer (.getBytes "CATCGAACAACTACT"))}
                {:content-id 5
                 :data (->> "qname001\000qname002\000qname003\000qname004\000"
                            .getBytes
                            bb/make-lsb-byte-buffer)}]
        bs-decoder (bs/make-bit-stream-decoder
                    (:data (first (filter #(zero? (:content-id %)) blocks))))
        {:keys [BA BF MQ RL BB RN]}
        (ds/build-data-series-decoders {:data-series encodings} bs-decoder blocks)]
    (is (= (map int "ATGC") [(BA) (BA) (BA) (BA)]))
    (is (= [0xa1 0x63 0xa3 0x63] [(BF) (BF) (BF) (BF)]))
    (is (= [60 60 60 60] [(MQ) (MQ) (MQ) (MQ)]))
    (is (= [150 149 152 151] [(RL) (RL) (RL) (RL)]))
    (is (= ["CAT" "CGAAC" "AACT" "ACT"]
           (map #(String. ^bytes %) [(BB) (BB) (BB) (BB)])))
    (is (= ["qname001" "qname002" "qname003" "qname004"]
           (map #(String. ^bytes %) [(RN) (RN) (RN) (RN)]))))
  (testing "block content interleaves when associated with multiple data series"
    (let [encodings {:RI {:codec :beta, :offset 0, :length 1}
                     :RL {:codec :beta, :offset 150, :length 1}
                     :BB {:codec :byte-array-len
                          :len-encoding {:codec :external, :content-id 1}
                          :val-encoding {:codec :external, :content-id 1}}
                     :DL {:codec :external, :content-id 2}
                     :HC {:codec :external, :content-id 2}}
          blocks [{:content-id 0
                   :data (bb/make-lsb-byte-buffer (byte-array [2r11100100]))}
                  {:content-id 1
                   :data (->> (byte-array [3 (int \A) (int \B) (int \C)
                                           2 (int \D) (int \E)
                                           1 (int \F)])
                              bb/make-lsb-byte-buffer)}
                  {:content-id 2
                   :data (bb/make-lsb-byte-buffer (byte-array [1 2 3 4 5 6]))}]
          bs-decoder (bs/make-bit-stream-decoder
                      (:data (first (filter #(zero? (:content-id %)) blocks))))
          {:keys [RI RL BB DL HC]}
          (ds/build-data-series-decoders {:data-series encodings} bs-decoder blocks)]
      (is (= [[1 151]
              [1 150]
              [0 151]]
             [[(RI) (RL)]
              [(RI) (RL)]
              [(RI) (RL)]]))
      (is (= ["ABC" "DE" "F"]
             (map #(String. ^bytes %) [(BB) (BB) (BB)])))
      (is (= [[1 2] [3 4] [5 6]]
             [[(DL) (HC)]
              [(DL) (HC)]
              [(DL) (HC)]])))))

(deftest build-tag-decoders-test
  (testing "single values"
    (testing "bytes"
      (let [encodings {:sb {\c {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [1]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7561827}}}
                       :ub {\C {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [1]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7692867}}}}
            blocks [{:content-id 7561827
                     :data (bb/make-lsb-byte-buffer (byte-array [0xde 0xed 0xbe 0xef]))}
                    {:content-id 7692867
                     :data (bb/make-lsb-byte-buffer (byte-array [0xca 0xfe 0xba 0xbe]))}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            sb (get-in decoders [:sb \c])
            ub (get-in decoders [:ub \C])]
        (is (= [{:type "i" :value (unchecked-byte 0xde)}
                {:type "i" :value (unchecked-byte 0xed)}
                {:type "i" :value (unchecked-byte 0xbe)}
                {:type "i" :value (unchecked-byte 0xef)}]
               [(sb) (sb) (sb) (sb)]))
        (is (= [{:type "i" :value 0xca}
                {:type "i" :value 0xfe}
                {:type "i" :value 0xba}
                {:type "i" :value 0xbe}]
               [(ub) (ub) (ub) (ub)]))))
    (testing "shorts"
      (let [encodings {:ss {\s {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [2]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7566195}}}
                       :us {\S {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [2]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7697235}}}}
            blocks [{:content-id 7566195
                     :data (doto (bb/allocate-lsb-byte-buffer 8)
                             (.putShort 0x0123)
                             (.putShort 0x4567)
                             (.putShort 0x89ab)
                             (.putShort 0xcdef)
                             .flip)}
                    {:content-id 7697235
                     :data (doto (bb/allocate-lsb-byte-buffer 8)
                             (.putShort 0x0123)
                             (.putShort 0x4567)
                             (.putShort 0x89ab)
                             (.putShort 0xcdef)
                             .flip)}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            ss (get-in decoders [:ss \s])
            us (get-in decoders [:us \S])]
        (is (= [{:type "i" :value 0x0123}
                {:type "i" :value 0x4567}
                {:type "i" :value (unchecked-short 0x89ab)}
                {:type "i" :value (unchecked-short 0xcdef)}]
               [(ss) (ss) (ss) (ss)]))
        (is (= [{:type "i" :value 0x0123}
                {:type "i" :value 0x4567}
                {:type "i" :value 0x89ab}
                {:type "i" :value 0xcdef}]
               [(us) (us) (us) (us)]))))
    (testing "ints"
      (let [encodings {:si {\i {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [4]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7563625}}}
                       :ui {\I {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [4]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7694665}}}}
            blocks [{:content-id 7563625
                     :data (doto (bb/allocate-lsb-byte-buffer 16)
                             (.putInt 0)
                             (.putInt 0x01234567)
                             (.putInt 0x89abcdef)
                             (.putInt 0xffffffff)
                             .flip)}
                    {:content-id 7694665
                     :data (doto (bb/allocate-lsb-byte-buffer 16)
                             (.putInt 0)
                             (.putInt 0x01234567)
                             (.putInt 0x89abcdef)
                             (.putInt 0xffffffff)
                             .flip)}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            si (get-in decoders [:si \i])
            ui (get-in decoders [:ui \I])]
        (is (= [{:type "i" :value 0}
                {:type "i" :value 0x01234567}
                {:type "i" :value (unchecked-int 0x89abcdef)}
                {:type "i" :value -1}]
               [(si) (si) (si) (si)]))
        (is (= [{:type "i" :value 0}
                {:type "i" :value 0x01234567}
                {:type "i" :value 0x89abcdef}
                {:type "i" :value 0xffffffff}]
               [(ui) (ui) (ui) (ui)]))))
    (testing "floats"
      (let [encodings {:fl {\f {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [4]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 6712422}}}}
            blocks [{:content-id 6712422
                     :data (doto (bb/allocate-lsb-byte-buffer 16)
                             (.putFloat 1.0)
                             (.putFloat 0.75)
                             (.putFloat -0.5)
                             (.putFloat 0.0)
                             .flip)}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            fl (get-in decoders [:fl \f])]
        (is (= [{:type "f" :value 1.0}
                {:type "f" :value 0.75}
                {:type "f" :value -0.5}
                {:type "f" :value 0.0}]
               [(fl) (fl) (fl) (fl)]))))
    (testing "strings"
      (let [encodings {:MC {\Z {:codec :byte-array-stop
                                :stop-byte 9
                                :content-id 5063514}}
                       :hx {\H {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 6846536}}}}
            blocks [{:content-id 5063514
                     :data (->> (str "151M\000\011"
                                     "20S131M\000\011"
                                     "16S74M1D58M2S\000\011"
                                     "151M\000\011")
                                .getBytes
                                bb/make-lsb-byte-buffer)}
                    {:content-id 6846536
                     :data (->> (str "01234567\000"
                                     "89abcdef\000"
                                     "cafebabe\000"
                                     "deadbeef\000")
                                .getBytes
                                bb/make-lsb-byte-buffer)}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            MC (get-in decoders [:MC \Z])
            hx (get-in decoders [:hx \H])]
        (is (= [{:type "Z" :value "151M"}
                {:type "Z" :value "20S131M"}
                {:type "Z" :value "16S74M1D58M2S"}
                {:type "Z" :value "151M"}]
               [(MC) (MC) (MC) (MC)]))
        (is (= [{:type "H" :value [0x01 0x23 0x45 0x67]}
                {:type "H" :value [0x89 0xab 0xcd 0xef]}
                {:type "H" :value [0xca 0xfe 0xba 0xbe]}
                {:type "H" :value [0xde 0xad 0xbe 0xef]}]
               (map (fn [m] (update m :value (partial map #(bit-and (long %) 0xff))))
                    [(hx) (hx) (hx) (hx)]))))))
  (testing "array values"
    (testing "byte arrays"
      (let [encodings {:sb {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7561794}}}
                       :ub {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7692866}}}}
            blocks [{:content-id 7561794
                     :data (let [bb (bb/allocate-lsb-byte-buffer 36)]
                             (doseq [vs [[0x00 0x01 0x02 0x03]
                                         [0x7c 0x7d 0x7e 0x7f]
                                         [0x80 0x81 0x82 0x83]
                                         [0xfc 0xfd 0xfe 0xff]]]
                               (.put bb (byte (int \c)))
                               (.putInt bb 4)
                               (doseq [v vs] (.put bb (byte v))))
                             (.flip bb))}
                    {:content-id 7692866
                     :data (let [bb (bb/allocate-lsb-byte-buffer 36)]
                             (doseq [vs [[0x00 0x01 0x02 0x03]
                                         [0x7c 0x7d 0x7e 0x7f]
                                         [0x80 0x81 0x82 0x83]
                                         [0xfc 0xfd 0xfe 0xff]]]
                               (.put bb (byte (int \C)))
                               (.putInt bb 4)
                               (doseq [v vs] (.put bb (byte v))))
                             (.flip bb))}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            sb (get-in decoders [:sb \B])
            ub (get-in decoders [:ub \B])]
        (is (= [{:type "B" :value "c,0,1,2,3"}
                {:type "B" :value "c,124,125,126,127"}
                {:type "B" :value "c,-128,-127,-126,-125"}
                {:type "B" :value "c,-4,-3,-2,-1"}]
               [(sb) (sb) (sb) (sb)]))
        (is (= [{:type "B" :value "C,0,1,2,3"}
                {:type "B" :value "C,124,125,126,127"}
                {:type "B" :value "C,128,129,130,131"}
                {:type "B" :value "C,252,253,254,255"}]
               [(ub) (ub) (ub) (ub)]))))
    (testing "short arrays"
      (let [encodings {:ss {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7566146}}}
                       :us {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7697218}}}}
            blocks [{:content-id 7566146
                     :data (let [bb (bb/allocate-lsb-byte-buffer 36)]
                             (doseq [vs [[0x0123 0x4567]
                                         [0x7ffe 0x7fff]
                                         [0x8000 0x8001]
                                         [0xfffe 0xffff]]]
                               (.put bb (byte (int \s)))
                               (.putInt bb 2)
                               (doseq [v vs] (.putShort bb v)))
                             (.flip bb))}
                    {:content-id 7697218
                     :data (let [bb (bb/allocate-lsb-byte-buffer 36)]
                             (doseq [vs [[0x0123 0x4567]
                                         [0x7ffe 0x7fff]
                                         [0x8000 0x8001]
                                         [0xfffe 0xffff]]]
                               (.put bb (byte (int \S)))
                               (.putInt bb 2)
                               (doseq [v vs] (.putShort bb v)))
                             (.flip bb))}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            ss (get-in decoders [:ss \B])
            us (get-in decoders [:us \B])]
        (is (= [{:type "B" :value (str "s," 0x0123 "," 0x4567)}
                {:type "B" :value (str "s," 0x7ffe "," 0x7fff)}
                {:type "B" :value (str "s," (unchecked-short 0x8000)
                                       "," (unchecked-short 0x8001))}
                {:type "B" :value (str "s," (unchecked-short 0xfffe)
                                       "," (unchecked-short 0xffff))}]
               [(ss) (ss) (ss) (ss)]))
        (is (= [{:type "B" :value (str "S," 0x0123 "," 0x4567)}
                {:type "B" :value (str "S," 0x7ffe "," 0x7fff)}
                {:type "B" :value (str "S," 0x8000 "," 0x8001)}
                {:type "B" :value (str "S," 0xfffe "," 0xffff)}]
               [(us) (us) (us) (us)]))))
    (testing "int arrays"
      (let [encodings {:si {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [13]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7563586}}}
                       :ui {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [13]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7694658}}}}
            blocks [{:content-id 7563586
                     :data (let [bb (bb/allocate-lsb-byte-buffer 52)]
                             (doseq [vs [[0x01234567 0x76543210]
                                         [0x7ffffffe 0x7fffffff]
                                         [0x80000000 0x80000001]
                                         [0xfffffffe 0xffffffff]]]
                               (.put bb (byte (int \i)))
                               (.putInt bb 2)
                               (doseq [v vs] (.putInt bb v)))
                             (.flip bb))}
                    {:content-id 7694658
                     :data (let [bb (bb/allocate-lsb-byte-buffer 52)]
                             (doseq [vs [[0x01234567 0x76543210]
                                         [0x7ffffffe 0x7fffffff]
                                         [0x80000000 0x80000001]
                                         [0xfffffffe 0xffffffff]]]
                               (.put bb (byte (int \I)))
                               (.putInt bb 2)
                               (doseq [v vs] (.putInt bb v)))
                             (.flip bb))}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            si (get-in decoders [:si \B])
            ui (get-in decoders [:ui \B])]
        (is (= [{:type "B" :value (str "i," 0x01234567 "," 0x76543210)}
                {:type "B" :value (str "i," 0x7ffffffe "," 0x7fffffff)}
                {:type "B" :value (str "i," (unchecked-int 0x80000000)
                                       "," (unchecked-int 0x80000001))}
                {:type "B" :value (str "i," (unchecked-int 0xfffffffe)
                                       "," (unchecked-int 0xffffffff))}]
               [(si) (si) (si) (si)]))
        (is (= [{:type "B" :value (str "I," 0x01234567 "," 0x76543210)}
                {:type "B" :value (str "I," 0x7ffffffe "," 0x7fffffff)}
                {:type "B" :value (str "I," 0x80000000 "," 0x80000001)}
                {:type "B" :value (str "I," 0xfffffffe "," 0xffffffff)}]
               [(ui) (ui) (ui) (ui)])))
      (let [vs ["42M1D7M1D74M1D28M"
                "44M2D45M1D58M4S"
                "65M1D9M1D75M2S"
                "18S76M1D57M"]
            encodings {:CG {\B {:codec :byte-array-stop
                                :stop-byte -1
                                :content-id 4409154}}}
            blocks [{:content-id 4409154
                     :data (let [encoded (map cigar/encode-cigar vs)
                                 bb (bb/allocate-lsb-byte-buffer (+ (->> (apply concat encoded)
                                                                         count
                                                                         (* 4))
                                                                    (* (count vs)
                                                                       (+ 1 4 1))))]
                             (doseq [e encoded]
                               (.put bb (byte (int \I)))
                               (.putInt bb (count e))
                               (doseq [x e]
                                 (.putInt bb (int x)))
                               (.put bb (byte -1)))
                             (.flip bb)
                             bb)}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            CG (get-in decoders [:CG \B])]
        (is (= (map #(array-map :type "B" :value %) vs)
               (map #(update % :value #'decoder/B-I-type-cigar-str->cigar-str)
                    [(CG) (CG) (CG) (CG)])))))
    (testing "float arrays"
      (let [encodings {:fl {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [13]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 6712386}}}}
            blocks [{:content-id 6712386
                     :data (let [bb (bb/allocate-lsb-byte-buffer 52)]
                             (doseq [vs [[0.0 0.25]
                                         [0.5 1.0]
                                         [0.0 -0.5]
                                         [-0.75 -1.0]]]
                               (.put bb (byte (int \f)))
                               (.putInt bb 2)
                               (doseq [v vs] (.putFloat bb v)))
                             (.flip bb))}]
            decoders (ds/build-tag-decoders {:tags encodings} nil blocks)
            fl (get-in decoders [:fl \B])]
        (is (= [{:type "B" :value "f,0.0,0.25"}
                {:type "B" :value "f,0.5,1.0"}
                {:type "B" :value "f,0.0,-0.5"}
                {:type "B" :value "f,-0.75,-1.0"}]
               [(fl) (fl) (fl) (fl)]))))))

(deftest build-data-series-encoders-test
  (let [encodings {:BF {:codec :external, :content-id 1}
                   :BA {:codec :external, :content-id 2}
                   :BB {:codec :byte-array-len
                        :len-encoding {:codec :external, :content-id 3}
                        :val-encoding {:codec :external, :content-id 4}}
                   :RN {:codec :byte-array-stop, :stop-byte (int \tab), :content-id 5}}
        {:keys [BF BA BB RN]}
        (ds/build-data-series-encoders encodings)]
    (doseq [[bf ba bb rn] (->> [[0xa1 0x63 0xa3 0x63]
                                (map int "ATGC")
                                (map #(.getBytes ^String %)
                                     ["CAT" "CGAAC" "AACT" "ACT"])
                                (map #(.getBytes ^String %)
                                     ["qname001" "qname002" "qname003" "qname004"])]
                               (apply map vector))]
      (BF bf)
      (BA ba)
      (BB bb)
      (RN rn))
    (let [[{:keys [content-id data]}] (BF)]
      (is (= 1 content-id))
      (is (= [0x80 0xa1 0x63 0x80 0xa3 0x63] (map #(bit-and % 0xff) data))))
    (let [[{:keys [content-id data]}] (BA)]
      (is (= 2 content-id))
      (is (= "ATGC" (String. data))))
    (let [[{content-id1 :content-id, data1 :data}
           {content-id2 :content-id, data2 :data}] (BB)]
      (is (= 3 content-id1))
      (is (= [3 5 4 3] (map #(bit-and % 0xff) data1)))
      (is (= 4 content-id2))
      (is (= "CATCGAACAACTACT" (String. data2))))
    (let [[{:keys [content-id data]}] (RN)]
      (is (= 5 content-id))
      (is (= "qname001\tqname002\tqname003\tqname004\t" (String. data))))))

(deftest build-tag-encoders-test
  (testing "single values"
    (testing "bytes"
      (let [encodings {:sb {\c {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [1]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7561827}}}
                       :ub {\C {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [1]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7692867}}}}
            encoders (ds/build-tag-encoders encodings)
            sb (get-in encoders [:sb \c])
            ub (get-in encoders [:ub \C])]
        (doseq [[v1 v2] (->> [[0xde 0xed 0xbe 0xef]
                              [0xca 0xfe 0xba 0xbe]]
                             (apply map vector))]
          (sb (unchecked-byte v1))
          (ub v2))
        (let [[{:keys [content-id data]}] (sb)]
          (is (= 7561827 content-id))
          (is (= [0xde 0xed 0xbe 0xef] (map #(bit-and % 0xff) data))))
        (let [[{:keys [content-id data]}] (ub)]
          (is (= 7692867 content-id))
          (is (= [0xca 0xfe 0xba 0xbe] (map #(bit-and % 0xff) data))))))
    (testing "shorts"
      (let [encodings {:ss {\s {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [2]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7566195}}}
                       :us {\S {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [2]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7697235}}}}
            encoders (ds/build-tag-encoders encodings)
            ss (get-in encoders [:ss \s])
            us (get-in encoders [:us \S])]
        (doseq [v [0x0123 0x4567 0x89ab 0xcdef]]
          (ss (unchecked-short v))
          (us v))
        (let [[{:keys [content-id data]}] (ss)]
          (is (= 7566195 content-id))
          (is (= [0x23 0x01 0x67 0x45 0xab 0x89 0xef 0xcd] (map #(bit-and % 0xff) data))))
        (let [[{:keys [content-id data]}] (us)]
          (is (= 7697235 content-id))
          (is (= [0x23 0x01 0x67 0x45 0xab 0x89 0xef 0xcd] (map #(bit-and % 0xff) data))))))
    (testing "ints"
      (let [encodings {:si {\i {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [4]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7563625}}}
                       :ui {\I {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [4]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7694665}}}}
            encoders (ds/build-tag-encoders encodings)
            si (get-in encoders [:si \i])
            ui (get-in encoders [:ui \I])]
        (doseq [v [0 0x01234567 0x89abcdef 0xffffffff]]
          (si (unchecked-int v))
          (ui v))
        (let [[{:keys [content-id data]}] (si)]
          (is (= 7563625 content-id))
          (is (= [0x00 0x00 0x00 0x00 0x67 0x45 0x23 0x01
                  0xef 0xcd 0xab 0x89 0xff 0xff 0xff 0xff]
                 (map #(bit-and % 0xff) data))))
        (let [[{:keys [content-id data]}] (ui)]
          (is (= 7694665 content-id))
          (is (= [0x00 0x00 0x00 0x00 0x67 0x45 0x23 0x01
                  0xef 0xcd 0xab 0x89 0xff 0xff 0xff 0xff]
                 (map #(bit-and % 0xff) data))))))
    (testing "floats"
      (let [encodings {:fl {\f {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [4]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 6712422}}}}
            encoders (ds/build-tag-encoders encodings)
            fl (get-in encoders [:fl \f])]
        (doseq [v [1.0 0.75 -0.5 0.0]]
          (fl v))
        (let [[{:keys [content-id data]}] (fl)
              bb (doto (bb/allocate-lsb-byte-buffer 16)
                   (.putFloat 1.0)
                   (.putFloat 0.75)
                   (.putFloat -0.5)
                   (.putFloat 0.0))]
          (is (= 6712422 content-id))
          (is (= (seq (.array bb)) (seq data))))))
    (testing "strings"
      (let [encodings {:MC {\Z {:codec :byte-array-stop
                                :stop-byte 9
                                :content-id 5063514}}
                       :hx {\H {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 6846536}}}}
            encoders (ds/build-tag-encoders encodings)
            MC (get-in encoders [:MC \Z])
            hx (get-in encoders [:hx \H])]
        (doseq [[v1 v2] (->> [["151M" "20S131M" "16S74M1D58M2S" "151M"]
                              [[0x01 0x23 0x45 0x67]
                               [0x89 0xab 0xcd 0xef]
                               [0xca 0xfe 0xba 0xbe]
                               [0xde 0xad 0xbe 0xef]]]
                             (apply map vector))]
          (MC v1)
          (hx (byte-array v2)))
        (let [[{:keys [content-id data]}] (MC)]
          (is (= 5063514 content-id))
          (is (= "151M\000\t20S131M\000\t16S74M1D58M2S\000\t151M\000\t"
                 (String. data))))
        (let [[{:keys [content-id data]}] (hx)]
          (is (= 6846536 content-id))
          (is (= "01234567\00089ABCDEF\000CAFEBABE\000DEADBEEF\000"
                 (String. data)))))))
  (testing "array values"
    (testing "byte arrays"
      (let [encodings {:sb {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7561794}}}
                       :ub {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7692866}}}}
            encoders (ds/build-tag-encoders encodings)
            sb (get-in encoders [:sb \B])
            ub (get-in encoders [:ub \B])]
        (doseq [[v1 v2] (->> [["c,0,1,2,3"
                               "c,124,125,126,127"
                               "c,-128,-127,-126,-125"
                               "c,-4,-3,-2,-1"]
                              ["C,0,1,2,3"
                               "C,124,125,126,127"
                               "C,128,129,130,131"
                               "C,252,253,254,255"]]
                             (apply map vector))]
          (sb v1)
          (ub v2))
        (let [[{:keys [content-id data]}] (sb)]
          (is (= 7561794 content-id))
          (is (= [0x63 0x04 0x00 0x00 0x00 0x00 0x01 0x02 0x03
                  0x63 0x04 0x00 0x00 0x00 0x7c 0x7d 0x7e 0x7f
                  0x63 0x04 0x00 0x00 0x00 0x80 0x81 0x82 0x83
                  0x63 0x04 0x00 0x00 0x00 0xfc 0xfd 0xfe 0xff]
                 (map #(bit-and % 0xff) data))))
        (let [[{:keys [content-id data]}] (ub)]
          (is (= 7692866 content-id))
          (is (= [0x43 0x04 0x00 0x00 0x00 0x00 0x01 0x02 0x03
                  0x43 0x04 0x00 0x00 0x00 0x7c 0x7d 0x7e 0x7f
                  0x43 0x04 0x00 0x00 0x00 0x80 0x81 0x82 0x83
                  0x43 0x04 0x00 0x00 0x00 0xfc 0xfd 0xfe 0xff]
                 (map #(bit-and % 0xff) data))))))
    (testing "short arrays"
      (let [encodings {:ss {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7566146}}}
                       :us {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [9]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7697218}}}}
            encoders (ds/build-tag-encoders encodings)
            ss (get-in encoders [:ss \B])
            us (get-in encoders [:us \B])]
        (doseq [[v1 v2] [[0x0123 0x4567]
                         [0x7ffe 0x7fff]
                         [0x8000 0x8001]
                         [0xfffe 0xffff]]]
          (ss (str "s," (unchecked-short v1) "," (unchecked-short v2)))
          (us (str "S," v1 "," v2)))
        (let [[{:keys [content-id data]}] (ss)]
          (is (= 7566146 content-id))
          (is (= [0x73 0x02 0x00 0x00 0x00 0x23 0x01 0x67 0x45
                  0x73 0x02 0x00 0x00 0x00 0xfe 0x7f 0xff 0x7f
                  0x73 0x02 0x00 0x00 0x00 0x00 0x80 0x01 0x80
                  0x73 0x02 0x00 0x00 0x00 0xfe 0xff 0xff 0xff]
                 (map #(bit-and % 0xff) data))))
        (let [[{:keys [content-id data]}] (us)]
          (is (= 7697218 content-id))
          (is (= [0x53 0x02 0x00 0x00 0x00 0x23 0x01 0x67 0x45
                  0x53 0x02 0x00 0x00 0x00 0xfe 0x7f 0xff 0x7f
                  0x53 0x02 0x00 0x00 0x00 0x00 0x80 0x01 0x80
                  0x53 0x02 0x00 0x00 0x00 0xfe 0xff 0xff 0xff]
                 (map #(bit-and % 0xff) data))))))
    (testing "int arrays"
      (let [encodings {:si {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [13]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7563586}}}
                       :ui {\B {:codec :byte-array-len
                                :len-encoding {:codec :huffman
                                               :alphabet [13]
                                               :bit-len [0]}
                                :val-encoding {:codec :external
                                               :content-id 7694658}}}}
            encoders (ds/build-tag-encoders encodings)
            si (get-in encoders [:si \B])
            ui (get-in encoders [:ui \B])]
        (doseq [[v1 v2] [[0x01234567 0x76543210]
                         [0x7ffffffe 0x7fffffff]
                         [0x80000000 0x80000001]
                         [0xfffffffe 0xffffffff]]]
          (si (str "i," (unchecked-int v1) "," (unchecked-int v2)))
          (ui (str "I," v1 "," v2)))
        (let [[{:keys [content-id data]}] (si)]
          (is (= 7563586 content-id))
          (is (= [0x69 0x02 0x00 0x00 0x00 0x67 0x45 0x23 0x01 0x10 0x32 0x54 0x76
                  0x69 0x02 0x00 0x00 0x00 0xfe 0xff 0xff 0x7f 0xff 0xff 0xff 0x7f
                  0x69 0x02 0x00 0x00 0x00 0x00 0x00 0x00 0x80 0x01 0x00 0x00 0x80
                  0x69 0x02 0x00 0x00 0x00 0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff]
                 (map #(bit-and % 0xff) data))))
        (let [[{:keys [content-id data]}] (ui)]
          (is (= 7694658 content-id))
          (is (= [0x49 0x02 0x00 0x00 0x00 0x67 0x45 0x23 0x01 0x10 0x32 0x54 0x76
                  0x49 0x02 0x00 0x00 0x00 0xfe 0xff 0xff 0x7f 0xff 0xff 0xff 0x7f
                  0x49 0x02 0x00 0x00 0x00 0x00 0x00 0x00 0x80 0x01 0x00 0x00 0x80
                  0x49 0x02 0x00 0x00 0x00 0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff]
                 (map #(bit-and % 0xff) data)))))
      (let [vs ["42M1D7M1D74M1D28M"
                "44M2D45M1D58M4S"
                "65M1D9M1D75M2S"
                "18S76M1D57M"]
            encodings {:CG {\B {:codec :byte-array-stop
                                :stop-byte -1
                                :content-id 4409154}}}
            encoders (ds/build-tag-encoders encodings)
            CG (get-in encoders [:CG \B])]
        (doseq [v vs]
          (CG (str/join \, (cons \I (cigar/encode-cigar v)))))
        (let [[{:keys [content-id data]}] (CG)]
          (is (= 4409154 content-id))
          (is (= (mapcat (fn [v]
                           (let [encoded (cigar/encode-cigar v)
                                 bb (bb/allocate-lsb-byte-buffer (+ 6 (* 4 (count encoded))))]
                             (.put bb (byte (int \I)))
                             (.putInt bb (count encoded))
                             (run! #(.putInt bb %) encoded)
                             (.put bb (byte 0xff))
                             (.array bb)))
                         vs)
                 (seq data))))))
    (testing "float arrays"
      (let [encodings {:fl {\B {:codec :byte-array-len
                                :len-encoding {:codec :external
                                               :content-id 6712386}
                                :val-encoding {:codec :external
                                               :content-id 6712386}}}}
            encoders (ds/build-tag-encoders encodings)
            fl (get-in encoders [:fl \B])]
        (doseq [v ["f,0.0,0.25" "f,0.5" "f,0.0,-0.25" "f,-0.5,-0.75,-1.0"]]
          (fl v))
        (let [[{content-id1 :content-id data1 :data}
               {content-id2 :content-id data2 :data}] (fl)]
          (is (= 6712386 content-id1 content-id2))
          (is (identical? data1 data2))
          (is (= (mapcat (fn [vs]
                           (let [out (ByteArrayOutputStream.)]
                             (.write out (+ 5 (* 4 (count vs))))
                             (.write out (int \f))
                             (lsb/write-int out (count vs))
                             (run! #(lsb/write-float out %) vs)
                             (.toByteArray out)))
                         [[0.0 0.25]
                          [0.5]
                          [0.0 -0.25]
                          [-0.5 -0.75 -1.0]])
                 (seq data1))))))))
