(ns cljam.io.cram.encode.tag-dict-test
  (:require [cljam.io.cram.encode.tag-dict :as tag-dict]
            [clojure.test :refer [are deftest is]]))

(deftest make-tag-dict-builder-test
  (let [builder (tag-dict/make-tag-dict-builder)]
    (is (= 0 (tag-dict/assign-tags-id! builder [{:MD {:type "Z", :value "2C2"}}])))
    (is (= 1 (tag-dict/assign-tags-id! builder [{:MD {:type "Z", :value "3"}}
                                                {:NM {:type "c", :value 0}}])))
    (is (= 2 (tag-dict/assign-tags-id! builder [])))
    (is (= 3 (tag-dict/assign-tags-id! builder [{:MD {:type "Z", :value "5"}}
                                                {:NM {:type "c", :value 0}}
                                                {:XA {:type "i", :value 42}}])))
    (is (= 1 (tag-dict/assign-tags-id! builder [{:MD {:type "Z", :value "0T5"}}
                                                {:NM {:type "c", :value 1}}])))
    (is (= 4 (tag-dict/assign-tags-id! builder [{:NM {:type "c", :value 2}}
                                                {:MD {:type "Z", :value "3^T2"}}])))
    (is (= 5 (tag-dict/assign-tags-id! builder [{:MD {:type "Z", :value "4"}}
                                                {:NM {:type "C", :value 0}}])))
    ;; RG tag will be ignored in terms of bulding tag dictionary because it's
    ;; encoded as a separate data series
    (is (= 0 (tag-dict/assign-tags-id! builder [{:RG {:type "Z", :value "rg001"}}
                                                {:MD {:type "Z", :value "3"}}])))
    (is (= [[{:tag :MD, :type \Z}]
            [{:tag :MD, :type \Z} {:tag :NM, :type \c}]
            []
            [{:tag :MD, :type \Z} {:tag :NM, :type \c} {:tag :XA, :type \i}]
            [{:tag :NM, :type \c} {:tag :MD, :type \Z}]
            [{:tag :MD, :type \Z} {:tag :NM, :type \C}]]
           (tag-dict/build-tag-dict builder)))))

(defn- tag-id [tag tag-type]
  (#'tag-dict/tag-id {:tag tag, :type tag-type}))

(deftest build-tag-encoding-test
  (are [input expected] (= expected (#'tag-dict/build-tag-encoding input))
    {:tag :Xc, :type \c}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :Xc \c)}}

    {:tag :XC, :type \C}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :XC \C)}}

    {:tag :Xs, :type \s}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [2], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :Xs \s)}}

    {:tag :XS, :type \S}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [2], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :XS \S)}}

    {:tag :Xi, :type \i}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :Xi \i)}}

    {:tag :XI, :type \I}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :XI \I)}}

    {:tag :Xf, :type \f}
    {:codec :byte-array-len
     :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
     :val-encoding {:codec :external, :content-id (tag-id :Xf \f)}}

    {:tag :XZ, :type \Z}
    {:codec :byte-array-len
     :len-encoding {:codec :external, :content-id (tag-id :XZ \Z)}
     :val-encoding {:codec :external, :content-id (tag-id :XZ \Z)}}

    {:tag :XH, :type \H}
    {:codec :byte-array-len
     :len-encoding {:codec :external, :content-id (tag-id :XH \H)}
     :val-encoding {:codec :external, :content-id (tag-id :XH \H)}}

    {:tag :XB, :type \B}
    {:codec :byte-array-len
     :len-encoding {:codec :external, :content-id (tag-id :XB \B)}
     :val-encoding {:codec :external, :content-id (tag-id :XB \B)}}))

(deftest build-tag-encodings-test
  (are [input expected] (= expected (tag-dict/build-tag-encodings input))
    []
    {}

    [[]]
    {}

    [[{:tag :XA, :type \c}]]
    {:XA {\c {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XA \c)}}}}

    [[{:tag :XA, :type \c}]
     []]
    {:XA {\c {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XA \c)}}}}

    [[{:tag :XA, :type \c} {:tag :XB, :type \i}]]
    {:XA {\c {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XA \c)}}}
     :XB {\i {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XB \i)}}}}

    [[{:tag :XA, :type \c}]
     [{:tag :XB, :type \i}]]
    {:XA {\c {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XA \c)}}}
     :XB {\i {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XB \i)}}}}

    [[{:tag :XA, :type \c} {:tag :XB, :type \i}]
     [{:tag :XB, :type \i}]]
    {:XA {\c {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XA \c)}}}
     :XB {\i {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XB \i)}}}}

    [[{:tag :XA, :type \c} {:tag :XB, :type \s}]
     [{:tag :XB, :type \i}]]
    {:XA {\c {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XA \c)}}}
     :XB {\s {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [2], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XB \s)}}
          \i {:codec :byte-array-len
              :len-encoding {:codec :huffman, :alphabet [4], :bit-len [0]}
              :val-encoding {:codec :external, :content-id (tag-id :XB \i)}}}}))
