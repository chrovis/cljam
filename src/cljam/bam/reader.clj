(ns cljam.bam.reader
  (:use [cljam.io])
  (:require [clojure.string :refer [join]]
            [clojure.java.io :refer [file]]
            (cljam [lsb :as lsb]
                   [util :refer [string->bytes ubyte hex-string->bytes]])
            [cljam.util.sam-util :refer [phred->fastq ref-id ref-name
                                         compressed-bases->chars
                                         parse-header get-end]]
            [cljam.bam-index :refer [get-spans]]
            [cljam.bam.common :refer [fixed-block-size]]
            [cljam.bam.decoder :as decoder])
  (:import java.util.Arrays
           [java.io DataInputStream Closeable EOFException]
           [java.nio ByteBuffer ByteOrder]
           [bgzf4j BGZFInputStream]))

;;
;; BAMReader
;;

(deftype BAMReader [f header refs reader data-reader index]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

;;
;; read alignment
;;

(defn- read-alignment [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size: " block-size))))
    (let [buffer ^ByteBuffer (ByteBuffer/allocate block-size)]
      (lsb/read-bytes rdr (.array buffer) 0 block-size)
      (let [ref-id      (lsb/read-int buffer)
            rname       (if (= ref-id -1) "*" (:name (nth refs ref-id)))
            pos         (inc (lsb/read-int buffer))
            l-read-name (int (lsb/read-ubyte buffer))
            mapq        (lsb/read-ubyte buffer)
            bin         (lsb/read-ushort buffer)
            n-cigar-op  (lsb/read-ushort buffer)
            flag        (lsb/read-ushort buffer)
            l-seq       (lsb/read-int buffer)
            rnext       (decoder/decode-next-ref-id refs (lsb/read-int buffer) rname)
            pnext       (inc (lsb/read-int buffer))
            tlen        (lsb/read-int buffer)
            qname       (lsb/read-string buffer (dec l-read-name))
            _           (lsb/skip buffer 1)
            cigar       (decoder/decode-cigar (lsb/read-bytes buffer (* n-cigar-op 4)))
            seq         (decoder/decode-seq (lsb/read-bytes buffer (/ (inc l-seq) 2)) l-seq)
            qual        (decoder/decode-qual (lsb/read-bytes buffer l-seq))
            rest        (lsb/read-bytes buffer (decoder/options-size block-size
                                                                     l-read-name
                                                                     n-cigar-op
                                                                     l-seq))
            options     (decoder/decode-options rest)]
        {:qname qname, :flag flag, :rname rname, :pos pos, :mapq  mapq,
         :cigar cigar, :rnext rnext, :pnext pnext, :tlen tlen, :seq seq,
         :qual qual, :options options}))))

(defn- light-read-alignment [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size: " block-size))))
    (let [buffer ^ByteBuffer (ByteBuffer/allocate block-size)]
      (lsb/read-bytes rdr (.array buffer) 0 block-size)
      (let [ref-id      (lsb/read-int buffer)
            rname       (or (ref-name refs ref-id) "*")
            pos         (inc (lsb/read-int buffer))
            l-read-name (int (lsb/read-ubyte buffer))
            _           (lsb/skip buffer 3)
            n-cigar-op  (lsb/read-ushort buffer)
            _           (lsb/skip buffer (+ 18 l-read-name))
            cigar-bytes (lsb/read-bytes buffer (* n-cigar-op 4))]
        {:rname rname, :pos pos, :meta {:cigar-bytes cigar-bytes}}))))

(defn- pointer-read-alignment [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        pointer-beg (.getFilePointer ^BGZFInputStream (.reader bam-reader))
        block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size: " block-size))))
    (let [buffer ^ByteBuffer (ByteBuffer/allocate block-size)]
      (lsb/read-bytes rdr (.array buffer) 0 block-size)
      (let [ref-id      (lsb/read-int buffer)
            rname       (if (= ref-id -1) "*" (ref-name refs ref-id))
            pos         (inc (lsb/read-int buffer))
            l-read-name (int (lsb/read-ubyte buffer))
            _           (lsb/skip buffer 3)
            n-cigar-op  (lsb/read-ushort buffer)
            flag        (lsb/read-ushort buffer)
            _           (lsb/skip buffer (+ 16 l-read-name))
            cigar       (decoder/decode-cigar (lsb/read-bytes buffer (* n-cigar-op 4)))
            pointer-end (.getFilePointer ^BGZFInputStream (.reader bam-reader))]
        {:flag flag, :rname rname, :pos pos,:cigar cigar,
         :meta {:chunk {:beg pointer-beg, :end pointer-end}}}))))

(defn- read-coordinate-alignment-block [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        ^Integer block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size:" block-size))))
    (let [data (lsb/read-bytes rdr block-size)
          bb (doto (lsb/gen-byte-buffer block-size)
               (.put data)
               (.flip))
          ref-id (.getInt bb)
          pos (inc (.getInt bb))
          bb-get-char (fn [b]
                        (let [c (.get b)
                              cb (lsb/gen-byte-buffer)]
                          (.limit cb (.capacity cb))
                          (.put cb c)
                          (.put cb (byte 0))
                          (.flip cb)
                          (.getShort cb)))
          bb-skip (fn [b l]
                    (.position b (+ l (.position b))))
          bb-get-string (fn [b l]
                          (let [a (byte-array l)]
                            (.get b a 0 l)
                            (String. ^bytes a 0 0 l)))
          l-read-name (int (bb-get-char bb))
          _ (bb-skip bb 23)
          qname (bb-get-string bb (dec l-read-name))]
      {:size block-size
       :data data
       :qname qname
       :rname (if (= ref-id -1) "*" (:name (nth refs ref-id)))
       :pos pos})))

(defn- read-alignment-block [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size:" block-size))))
    {:size block-size
     :data (lsb/read-bytes rdr block-size)}))

(defn- read-pointer-alignment-block
  "Reads an alignment block, returning a map including the data as byte array
  and the file pointers. This function is for indexing a BAM file."
  [^BAMReader bam-reader _]
  (let [rdr (.data-reader bam-reader)
        pointer-beg (.getFilePointer ^BGZFInputStream (.reader bam-reader))
        block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size: " block-size))))
    {:data (lsb/read-bytes rdr block-size)
     :pointer-beg pointer-beg
     :pointer-end (.getFilePointer ^BGZFInputStream (.reader bam-reader))}))

(defn- read-to-finish
  [^BAMReader rdr
   ^Long finish
   ^clojure.lang.IFn read-fn]
  (let [r ^BGZFInputStream (.reader rdr)]
    (when (and (not (zero? (.available r)))
               (> finish (.getFilePointer r)))
      (cons (read-fn rdr (.refs rdr))
            (lazy-seq (read-to-finish rdr finish read-fn))))))

(defn- read-alignments-first-only
  "It should be equivalent to [(first (filter window @candidates))]"
  [^BAMReader rdr spans window read-fn]
  (loop [left-spans spans]
    (when-let [span (first left-spans)]
      (let [[^Long begin ^Long finish] span]
        (.seek ^BGZFInputStream (.reader rdr) begin)
        (or
          (loop [left (read-to-finish rdr finish read-fn)]
            (when-let [one (first left)]
              (if (window one)
                [one]
                (recur (rest left)))))
          (recur (rest left-spans)))))))

(defn read-alignments*
  [^BAMReader rdr
   ^String chr ^Long start ^Long end
   deep-or-shallow]
  (when (nil? (.index rdr))
    (throw (Exception. "BAM index not found")))
  (let [^BAMIndex bai (.index rdr)
        spans (get-spans bai (ref-id (.refs rdr) chr) start end)
        window (fn [^clojure.lang.PersistentHashMap a]
                 (let [^Long left (:pos a)
                       ^Long right (get-end a)]
                   (and (= chr (:rname a))
                        (<= start right)
                        (>= end left))))
        read-fn (case deep-or-shallow
                  :first-only light-read-alignment
                  :shallow light-read-alignment
                  :deep read-alignment
                  :pointer pointer-read-alignment)
        candidates (delay (flatten (map (fn [[^Long begin ^Long finish]]
                                          (.seek ^BGZFInputStream (.reader rdr) begin)
                                          (doall (read-to-finish rdr finish read-fn))) spans)))]
    (if (= deep-or-shallow :first-only)
      (read-alignments-first-only rdr spans window read-fn)
      (filter window @candidates))))

(defn read-alignments-sequentially*
  [^BAMReader rdr deep-or-shallow]
  (let [read-aln-fn (case deep-or-shallow
                      :shallow light-read-alignment
                      :deep read-alignment
                      :pointer pointer-read-alignment)
        read-fn (fn read-fn*
                  [^BAMReader r ^clojure.lang.PersistentVector refs]
                  (if-let [a (try (read-aln-fn r refs)
                                  (catch EOFException e nil))]
                    (cons a (lazy-seq (read-fn* r refs)))))]
    (read-fn rdr (.refs rdr))))

(defn read-blocks-sequentially*
  [^BAMReader rdr mode]
  (let [read-block-fn (case mode
                        :normal read-alignment-block
                        :coordinate read-coordinate-alignment-block
                        :pointer read-pointer-alignment-block)
        read-fn (fn read-fn* [^BAMReader r ^clojure.lang.PersistentVector refs]
                  (if-let [b (try (read-block-fn r refs)
                                  (catch EOFException e nil))]
                    (cons b (lazy-seq (read-fn* r refs)))))]
    (read-fn rdr (.refs rdr))))

(defn load-headers
  [rdr]
  (let [header (parse-header (lsb/read-string rdr (lsb/read-int rdr)))
        n-ref (lsb/read-int rdr)
        refs (loop [i n-ref, ret []]
               (if (zero? i)
                 ret
                 (let [l-name (lsb/read-int rdr)
                       name   (lsb/read-string rdr l-name)
                       l-ref  (lsb/read-int rdr)]
                   (recur (dec i)
                          (conj ret {:name (subs name 0 (dec l-name))
                                     :len  l-ref})))))]
    {:header header
     :refs refs}))
