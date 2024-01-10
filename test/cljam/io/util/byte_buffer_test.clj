(ns cljam.io.util.byte-buffer-test
  (:require [cljam.io.util.byte-buffer :as bb]
            [clojure.test :refer [deftest is testing]]))

(deftest make-lsb-byte-buffer-test
  (let [bb (bb/make-lsb-byte-buffer (byte-array [0x01 0x23 0x45 0x67]))]
    (is (= 0x67452301 (.getInt bb)))))

(deftest make-msb-byte-buffer-test
  (let [bb (bb/make-msb-byte-buffer (byte-array [0x01 0x23 0x45 0x67]))]
    (is (= 0x01234567 (.getInt bb)))))

(deftest allocate-lsb-byte-buffer-test
  (let [bb (doto (bb/allocate-lsb-byte-buffer 8)
             (.putLong 0x789ABCDEF0123456)
             (.flip))]
    (is (= (int 0xF0123456) (.getInt bb)))
    (is (= (int 0x789ABCDE) (.getInt bb)))))

(deftest allocate-msb-byte-buffer-test
  (let [bb (doto (bb/allocate-msb-byte-buffer 8)
             (.putLong 0x789ABCDEF0123456)
             (.flip))]
    (is (= (int 0x789ABCDE) (.getInt bb)))
    (is (= (int 0xF0123456) (.getInt bb)))))

(deftest read-ops-test
  (let [bb (doto (bb/allocate-lsb-byte-buffer 8)
             (.mark)
             (.putLong 0x789ABCDEF0123456))]
    (testing "read-ubyte"
      (.reset bb)
      (is (= 0x56 (bb/read-ubyte bb)))
      (is (= 0x34 (bb/read-ubyte bb)))
      (is (= 0x12 (bb/read-ubyte bb)))
      (is (= 0xF0 (bb/read-ubyte bb)))
      (is (= 0xDE (bb/read-ubyte bb)))
      (is (= 0xBC (bb/read-ubyte bb)))
      (is (= 0x9A (bb/read-ubyte bb)))
      (is (= 0x78 (bb/read-ubyte bb))))

    (testing "read-ushort"
      (.reset bb)
      (is (= 0x3456 (bb/read-ushort bb)))
      (is (= 0xF012 (bb/read-ushort bb)))
      (is (= 0xBCDE (bb/read-ushort bb)))
      (is (= 0x789A (bb/read-ushort bb))))

    (testing "read-unit"
      (.reset bb)
      (is (= 0xF0123456 (bb/read-uint bb)))
      (is (= 0x789ABCDE (bb/read-uint bb))))

    (testing "read-bytes"
      (.reset bb)
      (is (= [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]
             (map #(bit-and % 0xFF) (bb/read-bytes bb 8))))

      (.reset bb)
      (bb/skip bb 1)
      (is (= [0 0 0x34 0x12 0xF0 0]
             (map #(bit-and % 0xFF) (bb/read-bytes bb (byte-array 6) 2 3)))))

    (testing "read-ints"
      (.reset bb)
      (let [arr (bb/read-ints bb 2)]
        (is (instance? (class (int-array 0)) arr))
        (is (= [0xF0123456 0x789ABCDE] (map #(bit-and % 0xFFFFFFFF) arr))))

      (.reset bb)
      (bb/skip bb 1)
      (is (= [0 0 0xDEF01234 0]
             (map #(bit-and % 0xFFFFFFFF) (bb/read-ints bb (int-array 4) 2 1)))))

    (testing "read-string"
      (.reset bb)
      (doseq [c "ABCDEFGH"]
        (.put bb (byte (int c))))
      (.reset bb)
      (is (= "ABCDEFGH" (bb/read-string bb 8))))

    (testing "read-null-terminated-string"
      (.reset bb)
      (doseq [c [\I \J \K \L 0 \M \N \O]]
        (.put bb (byte (int c))))
      (.reset bb)
      (is (= "IJKL" (bb/read-null-terminated-string bb))))))

(deftest write-ops-test
  (let [bb (bb/allocate-msb-byte-buffer 8)]
    (testing "write-ubyte"
      (bb/write-ubyte bb 0x78)
      (bb/write-ubyte bb 0x9A)
      (bb/write-ubyte bb 0xBC)
      (bb/write-ubyte bb 0xDE)
      (bb/write-ubyte bb 0xF0)
      (bb/write-ubyte bb 0x12)
      (bb/write-ubyte bb 0x34)
      (bb/write-ubyte bb 0x56)
      (.flip bb)
      (is (= 0x789ABCDEF0123456 (.getLong bb))))

    (testing "write-ushort"
      (.flip bb)
      (bb/write-ushort bb 0x789A)
      (bb/write-ushort bb 0xBCDE)
      (bb/write-ushort bb 0xF012)
      (bb/write-ushort bb 0x3456)
      (.flip bb)
      (is (= 0x789ABCDEF0123456 (.getLong bb))))

    (testing "write-uint"
      (.flip bb)
      (bb/write-uint bb 0x789ABCDE)
      (bb/write-uint bb 0xF0123456)
      (.flip bb)
      (is (= 0x789ABCDEF0123456 (.getLong bb))))

    (testing "write-string"
      (.flip bb)
      (bb/write-string bb "ABCDEFGH")
      (.flip bb)
      (is (= "ABCDEFGH" (bb/read-string bb 8))))))
