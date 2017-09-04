(ns cljam.io.util.lsb-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.test-common :as common]
            [cljam.io.util.lsb :as lsb])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.io RandomAccessFile DataInputStream FileInputStream]
           [bgzf4j BGZFInputStream BGZFOutputStream]))

(deftest about-byte-buffer
  (let [bb (lsb/gen-byte-buffer 8)]
    (.mark bb)
    (.putLong bb 0x789ABCDEF0123456)

    (.reset bb)
    (is (= (lsb/read-byte bb) 0x56))
    (is (= (lsb/read-byte bb) 0x34))
    (is (= (lsb/read-byte bb) 0x12))
    (is (= (lsb/read-byte bb) (unchecked-byte 0xF0)))
    (is (= (lsb/read-byte bb) (unchecked-byte 0xDE)))
    (is (= (lsb/read-byte bb) (unchecked-byte 0xBC)))
    (is (= (lsb/read-byte bb) (unchecked-byte 0x9A)))
    (is (= (lsb/read-byte bb) 0x78))

    (.reset bb)
    (is (= (lsb/read-ubyte bb) 0x56))
    (is (= (lsb/read-ubyte bb) 0x34))
    (is (= (lsb/read-ubyte bb) 0x12))
    (is (= (lsb/read-ubyte bb) 0xF0))
    (is (= (lsb/read-ubyte bb) 0xDE))
    (is (= (lsb/read-ubyte bb) 0xBC))
    (is (= (lsb/read-ubyte bb) 0x9A))
    (is (= (lsb/read-ubyte bb) 0x78))

    (.reset bb)
    (is (= (lsb/read-short bb) 0x3456))
    (is (= (lsb/read-short bb) (unchecked-short 0xF012)))
    (is (= (lsb/read-short bb) (unchecked-short 0xBCDE)))
    (is (= (lsb/read-short bb) 0x789A))

    (.reset bb)
    (is (= (lsb/read-ushort bb) 0x3456))
    (is (= (lsb/read-ushort bb) 0xF012))
    (is (= (lsb/read-ushort bb) 0xBCDE))
    (is (= (lsb/read-ushort bb) 0x789A))

    (.reset bb)
    (is (= (lsb/read-int bb) (unchecked-int 0xF0123456)))
    (is (= (lsb/read-int bb) 0x789ABCDE))

    (.reset bb)
    (is (= (lsb/read-uint bb) 0xF0123456))
    (is (= (lsb/read-uint bb) 0x789ABCDE))

    (.reset bb)
    (is (= (lsb/read-long bb) 0x789ABCDEF0123456))

    (.reset bb)
    (is (= (lsb/read-float bb) (Float/intBitsToFloat (unchecked-int 0xF0123456))))
    (is (= (lsb/read-float bb) (Float/intBitsToFloat 0x789ABCDE)))

    (.reset bb)
    (is (= (lsb/read-double bb) (Double/longBitsToDouble 0x789ABCDEF0123456)))

    (.reset bb)
    (is (= (map #(bit-and % 0xFF) (lsb/read-bytes bb 8))
           [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]))
    (.reset bb)
    (lsb/skip bb 1)
    (is (= (map #(bit-and % 0xFF) (lsb/read-bytes bb (byte-array 6) 2 3))
           [0 0 0x34 0x12 0xF0 0]))

    (.reset bb)
    (doseq [c "ABCDEFGH"]
      (.put bb (byte c)))
    (.reset bb)
    (is (= (lsb/read-string bb 8) "ABCDEFGH"))

    (.reset bb)
    (doseq [c [\I \J \K \L 0 \M \N \O]]
      (.put bb (byte c)))
    (.reset bb)
    (is (= (lsb/read-null-terminated-string bb) "IJKL"))))

(deftest about-random-access-file
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (let [filename (str common/temp-dir "raf.bin")]
      (with-open [raf (RandomAccessFile. filename "rw")]
        (let [bb (lsb/gen-byte-buffer 24)]
          (.putLong bb 0x789ABCDEF0123456)
          (doseq [c "ABCDEFGH"]
            (.put bb (byte c)))
          (doseq [c [\I \J \K \L 0 \M \N \O]]
            (.put bb (byte c)))
          (.write raf (.array bb))))
      (with-open [raf (RandomAccessFile. filename "r")]
        (.seek raf 0)
        (is (= (lsb/read-byte raf) 0x56))
        (is (= (lsb/read-byte raf) 0x34))
        (is (= (lsb/read-byte raf) 0x12))
        (is (= (lsb/read-byte raf) (unchecked-byte 0xF0)))
        (is (= (lsb/read-byte raf) (unchecked-byte 0xDE)))
        (is (= (lsb/read-byte raf) (unchecked-byte 0xBC)))
        (is (= (lsb/read-byte raf) (unchecked-byte 0x9A)))
        (is (= (lsb/read-byte raf) 0x78))

        (.seek raf 0)
        (is (= (lsb/read-ubyte raf) 0x56))
        (is (= (lsb/read-ubyte raf) 0x34))
        (is (= (lsb/read-ubyte raf) 0x12))
        (is (= (lsb/read-ubyte raf) 0xF0))
        (is (= (lsb/read-ubyte raf) 0xDE))
        (is (= (lsb/read-ubyte raf) 0xBC))
        (is (= (lsb/read-ubyte raf) 0x9A))
        (is (= (lsb/read-ubyte raf) 0x78))

        (.seek raf 0)
        (is (= (lsb/read-short raf) 0x3456))
        (is (= (lsb/read-short raf) (unchecked-short 0xF012)))
        (is (= (lsb/read-short raf) (unchecked-short 0xBCDE)))
        (is (= (lsb/read-short raf) 0x789A))

        (.seek raf 0)
        (is (= (lsb/read-ushort raf) 0x3456))
        (is (= (lsb/read-ushort raf) 0xF012))
        (is (= (lsb/read-ushort raf) 0xBCDE))
        (is (= (lsb/read-ushort raf) 0x789A))

        (.seek raf 0)
        (is (= (lsb/read-int raf) (unchecked-int 0xF0123456)))
        (is (= (lsb/read-int raf) 0x789ABCDE))

        (.seek raf 0)
        (is (= (lsb/read-uint raf) 0xF0123456))
        (is (= (lsb/read-uint raf) 0x789ABCDE))

        (.seek raf 0)
        (is (= (lsb/read-long raf) 0x789ABCDEF0123456))

        (.seek raf 0)
        (is (= (lsb/read-float raf) (Float/intBitsToFloat (unchecked-int 0xF0123456))))
        (is (= (lsb/read-float raf) (Float/intBitsToFloat 0x789ABCDE)))

        (.seek raf 0)
        (is (= (lsb/read-double raf) (Double/longBitsToDouble 0x789ABCDEF0123456)))

        (.seek raf 0)
        (is (= (map #(bit-and % 0xFF) (lsb/read-bytes raf 8))
               [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]))
        (.seek raf 0)
        (is (= (map #(bit-and % 0xFF) (lsb/read-bytes raf (byte-array 6) 2 3))
               [0 0 0x56 0x34 0x12 0]))

        (.seek raf 0)
        (lsb/skip raf 8)
        (is (= (lsb/read-string raf 8) "ABCDEFGH"))
        (is (= (lsb/read-null-terminated-string raf) "IJKL"))))))

(deftest about-data-input-stream
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (let [filename (str common/temp-dir "raf.bin")]
      (with-open [raf (RandomAccessFile. filename "rw")]
        (let [bb (lsb/gen-byte-buffer 24)]
          (.putLong bb 0x789ABCDEF0123456)
          (doseq [c "ABCDEFGH"]
            (.put bb (byte c)))
          (doseq [c [\I \J \K \L 0 \M \N \O]]
            (.put bb (byte c)))
          (.write raf (.array bb))))
      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-byte dis) 0x56))
        (is (= (lsb/read-byte dis) 0x34))
        (is (= (lsb/read-byte dis) 0x12))
        (is (= (lsb/read-byte dis) (unchecked-byte 0xF0)))
        (is (= (lsb/read-byte dis) (unchecked-byte 0xDE)))
        (is (= (lsb/read-byte dis) (unchecked-byte 0xBC)))
        (is (= (lsb/read-byte dis) (unchecked-byte 0x9A)))
        (is (= (lsb/read-byte dis) 0x78)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-ubyte dis) 0x56))
        (is (= (lsb/read-ubyte dis) 0x34))
        (is (= (lsb/read-ubyte dis) 0x12))
        (is (= (lsb/read-ubyte dis) 0xF0))
        (is (= (lsb/read-ubyte dis) 0xDE))
        (is (= (lsb/read-ubyte dis) 0xBC))
        (is (= (lsb/read-ubyte dis) 0x9A))
        (is (= (lsb/read-ubyte dis) 0x78)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-short dis) 0x3456))
        (is (= (lsb/read-short dis) (unchecked-short 0xF012)))
        (is (= (lsb/read-short dis) (unchecked-short 0xBCDE)))
        (is (= (lsb/read-short dis) 0x789A)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-ushort dis) 0x3456))
        (is (= (lsb/read-ushort dis) 0xF012))
        (is (= (lsb/read-ushort dis) 0xBCDE))
        (is (= (lsb/read-ushort dis) 0x789A)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-int dis) (unchecked-int 0xF0123456)))
        (is (= (lsb/read-int dis) 0x789ABCDE)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-uint dis) 0xF0123456))
        (is (= (lsb/read-uint dis) 0x789ABCDE)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-long dis) 0x789ABCDEF0123456)))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-float dis) (Float/intBitsToFloat (unchecked-int 0xF0123456))))
        (is (= (lsb/read-float dis) (Float/intBitsToFloat 0x789ABCDE))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (lsb/read-double dis) (Double/longBitsToDouble 0x789ABCDEF0123456))))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (map #(bit-and % 0xFF) (lsb/read-bytes dis 8))
               [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78])))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (is (= (map #(bit-and % 0xFF) (lsb/read-bytes dis (byte-array 6) 2 3))
               [0 0 0x56 0x34 0x12 0])))

      (with-open [fis (FileInputStream. filename)
                  dis (DataInputStream. fis)]
        (lsb/skip dis 8)
        (is (= (lsb/read-string dis 8) "ABCDEFGH"))
        (is (= (lsb/read-null-terminated-string dis) "IJKL"))))))

(deftest about-bgzf-input-stream
  (common/with-before-after {:before (common/prepare-cache!)
                             :after (common/clean-cache!)}
    (let [filename (str common/temp-dir "raf.bin.gz")]
      (with-open [bgzfos (BGZFOutputStream. (cio/file filename))]
        (let [bb (lsb/gen-byte-buffer 24)]
          (.putLong bb 0x789ABCDEF0123456)
          (doseq [c "ABCDEFGH"]
            (.put bb (byte c)))
          (doseq [c [\I \J \K \L 0 \M \N \O]]
            (.put bb (byte c)))
          (.write bgzfos (.array bb))))
      (with-open [bgzfis (BGZFInputStream. (cio/file filename))]
        (.seek bgzfis 0)
        (is (= (lsb/read-byte bgzfis) 0x56))
        (is (= (lsb/read-byte bgzfis) 0x34))
        (is (= (lsb/read-byte bgzfis) 0x12))
        (is (= (lsb/read-byte bgzfis) (unchecked-byte 0xF0)))
        (is (= (lsb/read-byte bgzfis) (unchecked-byte 0xDE)))
        (is (= (lsb/read-byte bgzfis) (unchecked-byte 0xBC)))
        (is (= (lsb/read-byte bgzfis) (unchecked-byte 0x9A)))
        (is (= (lsb/read-byte bgzfis) 0x78))

        (.seek bgzfis 0)
        (is (= (lsb/read-ubyte bgzfis) 0x56))
        (is (= (lsb/read-ubyte bgzfis) 0x34))
        (is (= (lsb/read-ubyte bgzfis) 0x12))
        (is (= (lsb/read-ubyte bgzfis) 0xF0))
        (is (= (lsb/read-ubyte bgzfis) 0xDE))
        (is (= (lsb/read-ubyte bgzfis) 0xBC))
        (is (= (lsb/read-ubyte bgzfis) 0x9A))
        (is (= (lsb/read-ubyte bgzfis) 0x78))

        (.seek bgzfis 0)
        (is (= (lsb/read-short bgzfis) 0x3456))
        (is (= (lsb/read-short bgzfis) (unchecked-short 0xF012)))
        (is (= (lsb/read-short bgzfis) (unchecked-short 0xBCDE)))
        (is (= (lsb/read-short bgzfis) 0x789A))

        (.seek bgzfis 0)
        (is (= (lsb/read-ushort bgzfis) 0x3456))
        (is (= (lsb/read-ushort bgzfis) 0xF012))
        (is (= (lsb/read-ushort bgzfis) 0xBCDE))
        (is (= (lsb/read-ushort bgzfis) 0x789A))

        (.seek bgzfis 0)
        (is (= (lsb/read-int bgzfis) (unchecked-int 0xF0123456)))
        (is (= (lsb/read-int bgzfis) 0x789ABCDE))

        (.seek bgzfis 0)
        (is (= (lsb/read-uint bgzfis) 0xF0123456))
        (is (= (lsb/read-uint bgzfis) 0x789ABCDE))

        (.seek bgzfis 0)
        (is (= (lsb/read-long bgzfis) 0x789ABCDEF0123456))

        (.seek bgzfis 0)
        (is (= (lsb/read-float bgzfis) (Float/intBitsToFloat (unchecked-int 0xF0123456))))
        (is (= (lsb/read-float bgzfis) (Float/intBitsToFloat 0x789ABCDE)))

        (.seek bgzfis 0)
        (is (= (lsb/read-double bgzfis) (Double/longBitsToDouble 0x789ABCDEF0123456)))

        (.seek bgzfis 0)
        (is (= (map #(bit-and % 0xFF) (lsb/read-bytes bgzfis 8))
               [0x56 0x34 0x12 0xF0 0xDE 0xBC 0x9A 0x78]))
        (.seek bgzfis 0)
        (is (= (map #(bit-and % 0xFF) (lsb/read-bytes bgzfis (byte-array 6) 2 3))
               [0 0 0x56 0x34 0x12 0]))

        (.seek bgzfis 0)
        (lsb/skip bgzfis 8)
        (is (= (lsb/read-string bgzfis 8) "ABCDEFGH"))
        (is (= (lsb/read-null-terminated-string bgzfis) "IJKL"))))))
