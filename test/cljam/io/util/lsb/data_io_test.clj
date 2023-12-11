(ns cljam.io.util.lsb.data-io-test
  (:require [cljam.io.util.byte-buffer :as bb]
            [cljam.io.util.lsb.data-io :as lsb]
            [cljam.test-common :as common]
            [clojure.java.io :as cio]
            [clojure.test :refer [deftest is]])
  (:import [java.io DataInputStream FileInputStream RandomAccessFile]))

(deftest about-random-access-file
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
      (with-open [raf (RandomAccessFile. filename "r")]
        (.seek raf 0)
        (is (= 0x56 (lsb/read-byte raf)))
        (is (= 0x34 (lsb/read-byte raf)))
        (is (= 0x12 (lsb/read-byte raf)))
        (is (= (unchecked-byte 0xF0) (lsb/read-byte raf)))
        (is (= (unchecked-byte 0xDE) (lsb/read-byte raf)))
        (is (= (unchecked-byte 0xBC) (lsb/read-byte raf)))
        (is (= (unchecked-byte 0x9A) (lsb/read-byte raf)))
        (is (= 0x78 (lsb/read-byte raf)))

        (.seek raf 0)
        (is (= 0x56 (lsb/read-ubyte raf)))
        (is (= 0x34 (lsb/read-ubyte raf)))
        (is (= 0x12 (lsb/read-ubyte raf)))
        (is (= 0xF0 (lsb/read-ubyte raf)))
        (is (= 0xDE (lsb/read-ubyte raf)))
        (is (= 0xBC (lsb/read-ubyte raf)))
        (is (= 0x9A (lsb/read-ubyte raf)))
        (is (= 0x78 (lsb/read-ubyte raf)))

        (.seek raf 0)
        (is (= 0x3456 (lsb/read-short raf)))
        (is (= (unchecked-short 0xF012) (lsb/read-short raf)))
        (is (= (unchecked-short 0xBCDE) (lsb/read-short raf)))
        (is (= 0x789A (lsb/read-short raf)))

        (.seek raf 0)
        (is (= 0x3456 (lsb/read-ushort raf)))
        (is (= 0xF012 (lsb/read-ushort raf)))
        (is (= 0xBCDE (lsb/read-ushort raf)))
        (is (= 0x789A (lsb/read-ushort raf)))

        (.seek raf 0)
        (is (= (unchecked-int 0xF0123456) (lsb/read-int raf)))
        (is (= 0x789ABCDE (lsb/read-int raf)))

        (.seek raf 0)
        (is (= 0xF0123456 (lsb/read-uint raf)))
        (is (= 0x789ABCDE (lsb/read-uint raf)))

        (.seek raf 0)
        (is (= 0x789ABCDEF0123456 (lsb/read-long raf)))

        (.seek raf 0)
        (is (= (Float/intBitsToFloat (unchecked-int 0xF0123456)) (lsb/read-float raf)))
        (is (= (Float/intBitsToFloat 0x789ABCDE) (lsb/read-float raf)))

        (.seek raf 0)
        (is (= (Double/longBitsToDouble 0x789ABCDEF0123456) (lsb/read-double raf)))

        (.seek raf 0)
        (is (= [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]
               (map #(bit-and % 0xFF) (lsb/read-bytes raf 8))))
        (.seek raf 0)
        (is (= [0 0 0x56 0x34 0x12 0]
               (map #(bit-and % 0xFF) (lsb/read-bytes raf (byte-array 6) 2 3))))

        (.seek raf 0)
        (lsb/skip raf 8)
        (is (= "ABCDEFGH" (lsb/read-string raf 8)))
        (is (= "IJKL" (lsb/read-null-terminated-string raf)))))))

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
