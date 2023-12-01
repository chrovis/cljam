(ns cljam.io.util.lsb.io-stream-test
  (:require [cljam.io.util.byte-buffer :as bb]
            [cljam.io.util.lsb.io-stream :as lsb]
            [cljam.test-common :as common]
            [clojure.java.io :as cio]
            [clojure.test :refer [deftest is]])
  (:import [java.io
            ByteArrayOutputStream
            DataInputStream
            FileInputStream
            RandomAccessFile]))

(deftest about-data-input-stream
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (let [filename (cio/file common/temp-dir "raf.bin")]
      (with-open [raf (RandomAccessFile. filename "rw")]
        (let [bb (bb/allocate-lsb-byte-buffer 24)]
          (.putLong bb 0x789ABCDEF0123456)
          (doseq [c "ABCDEFGH"]
            (.put bb (byte (int c))))
          (doseq [c [\I \J \K \L 0 \M \N \O]]
            (.put bb (byte (int c))))
          (.write raf (.array bb))))
      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= 0x56 (lsb/read-byte dis)))
        (is (= 0x34 (lsb/read-byte dis)))
        (is (= 0x12 (lsb/read-byte dis)))
        (is (= (unchecked-byte 0xF0) (lsb/read-byte dis)))
        (is (= (unchecked-byte 0xDE) (lsb/read-byte dis)))
        (is (= (unchecked-byte 0xBC) (lsb/read-byte dis)))
        (is (= (unchecked-byte 0x9A) (lsb/read-byte dis)))
        (is (= 0x78 (lsb/read-byte dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= 0x56 (lsb/read-ubyte dis)))
        (is (= 0x34 (lsb/read-ubyte dis)))
        (is (= 0x12 (lsb/read-ubyte dis)))
        (is (= 0xF0 (lsb/read-ubyte dis)))
        (is (= 0xDE (lsb/read-ubyte dis)))
        (is (= 0xBC (lsb/read-ubyte dis)))
        (is (= 0x9A (lsb/read-ubyte dis)))
        (is (= 0x78 (lsb/read-ubyte dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= 0x3456 (lsb/read-short dis)))
        (is (= (unchecked-short 0xF012) (lsb/read-short dis)))
        (is (= (unchecked-short 0xBCDE) (lsb/read-short dis)))
        (is (= 0x789A (lsb/read-short dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= 0x3456 (lsb/read-ushort dis)))
        (is (= 0xF012 (lsb/read-ushort dis)))
        (is (= 0xBCDE (lsb/read-ushort dis)))
        (is (= 0x789A (lsb/read-ushort dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (unchecked-int 0xF0123456) (lsb/read-int dis)))
        (is (= 0x789ABCDE (lsb/read-int dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= 0xF0123456 (lsb/read-uint dis)))
        (is (= 0x789ABCDE (lsb/read-uint dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= 0x789ABCDEF0123456 (lsb/read-long dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (Float/intBitsToFloat (unchecked-int 0xF0123456)) (lsb/read-float dis)))
        (is (= (Float/intBitsToFloat 0x789ABCDE) (lsb/read-float dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (Double/longBitsToDouble 0x789ABCDEF0123456) (lsb/read-double dis))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]
               (map #(bit-and % 0xFF) (lsb/read-bytes dis 8)))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= [0 0 0x56 0x34 0x12 0]
               (map #(bit-and % 0xFF) (lsb/read-bytes dis (byte-array 6) 2 3)))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (lsb/skip dis 8)
        (is (= "ABCDEFGH" (lsb/read-string dis 8)))
        (is (= "IJKL" (lsb/read-null-terminated-string dis)))))))

(deftest about-data-output-stream
  (let [ba (-> (bb/allocate-lsb-byte-buffer 8)
               (.putLong 0x789ABCDEF0123456)
               (.array))]
    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-ubyte baos 0x56)
      (lsb/write-ubyte baos 0x34)
      (lsb/write-ubyte baos 0x12)
      (lsb/write-ubyte baos 0xF0)
      (lsb/write-ubyte baos 0xDE)
      (lsb/write-ubyte baos 0xBC)
      (lsb/write-ubyte baos 0x9A)
      (lsb/write-ubyte baos 0x78)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-short baos 0x3456)
      (lsb/write-short baos (unchecked-short 0xF012))
      (lsb/write-short baos (unchecked-short 0xBCDE))
      (lsb/write-short baos 0x789A)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-ushort baos 0x3456)
      (lsb/write-ushort baos 0xF012)
      (lsb/write-ushort baos 0xBCDE)
      (lsb/write-ushort baos 0x789A)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-int baos 0xF0123456)
      (lsb/write-int baos 0x789ABCDE)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-uint baos 0xF0123456)
      (lsb/write-uint baos 0x789ABCDE)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-long baos 0x789ABCDEF0123456)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-float baos (Float/intBitsToFloat (unchecked-int 0xF0123456)))
      (lsb/write-float baos (Float/intBitsToFloat 0x789ABCDE))
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-double baos (Double/longBitsToDouble 0x789ABCDEF0123456))
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))

    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-bytes baos (byte-array [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]))
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret))))))

  (let [s "ABCDEFGH"
        ba (-> (bb/allocate-lsb-byte-buffer 8)
               (.put (.getBytes s))
               (.array))]
    (with-open [baos (ByteArrayOutputStream. 8)]
      (lsb/write-string baos s)
      (let [ret (.toByteArray baos)]
        (is (= (count ba) (count ret)))
        (is (= (seq ba) (seq ret)))))))
