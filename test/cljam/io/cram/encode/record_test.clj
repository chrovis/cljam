(ns cljam.io.cram.encode.record-test
  (:require [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.record :as record]
            [clojure.test :refer [deftest is]]))

(deftest encode-slice-records-test
  (let [cram-header {:SQ
                     [{:SN "ref"}
                      {:SN "ref2"}]
                     :RG
                     [{:ID "rg001"}
                      {:ID "rg002"}]}
        ds-encoders (ds/build-data-series-encoders
                     {:BF {:codec :external, :content-id 1}
                      :CF {:codec :external, :content-id 2}
                      :RI {:codec :external, :content-id 3}
                      :RL {:codec :external, :content-id 4}
                      :AP {:codec :external, :content-id 5}
                      :RG {:codec :external, :content-id 6}
                      :RN {:codec :byte-array-len
                           :len-encoding {:codec :external, :content-id 7}
                           :val-encoding {:codec :external, :content-id 8}}
                      :MF {:codec :external, :content-id 9}
                      :NS {:codec :external, :content-id 10}
                      :NP {:codec :external, :content-id 11}
                      :TS {:codec :external, :content-id 12}
                      :TL {:codec :external, :content-id 13}
                      :FN {:codec :external, :content-id 14}
                      :QS {:codec :external, :content-id 15}
                      :MQ {:codec :external, :content-id 16}})
        records [{:qname "q001", :flag 99, :rname "ref", :pos 1, :end 5, :mapq 0,
                  :cigar "5M", :rnext "=", :pnext 151, :tlen 150, :seq "AGAAT", :qual "HFHHH"
                  :options []}
                 {:qname "q002", :flag 99, :rname "ref", :pos 5, :end 7, :mapq 15,
                  :cigar "2S3M", :rnext "=", :pnext 15, :tlen 15, :seq "CCTGT", :qual "##AAC"
                  :options []}
                 {:qname "q003", :flag 177, :rname "ref", :pos 10, :end 14, :mapq 60,
                  :cigar "5M", :rnext "ref2", :pnext 100, :tlen 0, :seq "GATAA", :qual "CCCFF"
                  :options []}
                 {:qname "q004", :flag 147, :rname "ref", :pos 15, :end 19, :mapq 15,
                  :cigar "1M1I1M1D2M", :rnext "=", :pnext 5, :tlen -15, :seq "GAAAG", :qual "EBBFF"
                  :options []}
                 {:qname "q005", :flag 73, :rname "ref", :pos 20, :end 24, :mapq 0,
                  :cigar "5M", :rnext "*", :pnext 0, :tlen 0, :seq "CTGTG", :qual "AEEEE"
                  :options []}]
        _ (record/encode-slice-records cram-header ds-encoders {} records)
        res (into {}
                  (mapcat (fn [encoder]
                            (for [{:keys [content-id data]} (encoder)]
                              [content-id data])))
                  (vals ds-encoders))]
    (is (= [0x43 0x43 0x80 0x91 0x80 0x93 0x41]
           (map #(bit-and % 0xff) (get res 1))))
    (is (= [3 3 3 3 3] (seq (get res 2))))
    (is (= [0 0 0 0 0] (seq (get res 3))))
    (is (= [5 5 5 5 5] (seq (get res 4))))
    (is (= [1 5 10 15 20] (seq (get res 5))))
    (is (= [0xff 0xff 0xff 0xff 0x0f
            0xff 0xff 0xff 0xff 0x0f
            0xff 0xff 0xff 0xff 0x0f
            0xff 0xff 0xff 0xff 0x0f
            0xff 0xff 0xff 0xff 0x0f]
           (map #(bit-and % 0xff) (get res 6))))
    (is (= [4 4 4 4 4] (seq (get res 7))))
    (is (= "q001q002q003q004q005" (String. ^bytes (get res 8))))
    (is (= [1 1 1 0 2] (seq (get res 9))))
    (is (= [0 0 1 0 0xff 0xff 0xff 0xff 0x0f]
           (map #(bit-and % 0xff) (get res 10))))
    (is (= [0x80 0x97 0x0f 0x64 0x05 0x00]
           (map #(bit-and % 0xff) (get res 11))))
    (is (= [0x80 0x96 0x0f 0x00 0xff 0xff 0xff 0xff 0x01 0x00]
           (map #(bit-and % 0xff) (get res 12))))
    (is (= [0 0 0 0 0] (seq (get res 13))))
    (is (= [0 0 0 0 0] (seq (get res 14))))
    (is (= "HFHHH##AACCCCFFEBBFFAEEEE" (String. ^bytes (get res 15))))))
