(ns cljam.io.bam.encoder
  "Encoder of BAM alignment blocks."
  (:require [clojure.string :as cstr]
            [cljam.util :as util]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.sam.util.sequence :as seq]
            [cljam.io.util.lsb.io-stream :as lsb]
            [cljam.io.bam.common :as common]))

(def ^:private ^:const fixed-tag-size 3)
(def ^:private ^:const fixed-binary-array-tag-size 5)

(defn- get-next-ref-id [sa refs]
  (condp = (:rnext sa)
    "*" -1
    "=" (if-let [id (refs/ref-id refs (:rname sa))] id -1)
    (if-let [id (refs/ref-id refs (:rnext sa))] id -1)))

(defn- get-options-size ^long [sam-alignment]
  (->> (map
        (fn [op]
          (let [[_ value] (first (seq op))]
            (+ fixed-tag-size
               (case (first (:type value))
                 \A 1
                 \i 4
                 \f 4
                 \Z (inc (count (:value value)))
                 \B (let [[array-type & array] (cstr/split (:value value) #",")]
                      (+ fixed-binary-array-tag-size
                         (* (count array)
                            (case (first array-type)
                              \c 1
                              \C 1
                              \s 2
                              \S 2
                              \i 4
                              \I 4
                              \f 4
                              0))))))))
        (:options sam-alignment))
       (reduce +)))

(defn- encode-qual [sam-alignment]
  (if (= (:qual sam-alignment) "*")
    (byte-array (.length ^String (:seq sam-alignment)) (util/ubyte 0xff))
    (qual/fastq->phred (:qual sam-alignment))))

(defn- encode-tag-value [writer val-type value]
  (case val-type
    \A (lsb/write-char writer (char value))
    \c (lsb/write-ubyte writer (byte value))
    \C (lsb/write-ubyte writer (int value))
    \s (lsb/write-short writer (short value))
    \S (lsb/write-ushort writer (int value))
    \i (lsb/write-int writer (int value))
    \I (lsb/write-uint writer (long value))
    \f (lsb/write-float writer (float value))
    \Z (do (lsb/write-string writer value)
           (lsb/write-char writer (char 0)))
    ;; \H nil
    \B (let [[array-type & array] (cstr/split value #",")]
         (lsb/write-bytes writer (byte-array 1 (byte (int (first array-type)))))
         (lsb/write-int writer (count array))
         (case (first array-type)
           \c (doseq [v array]
                (lsb/write-ubyte writer (Byte/parseByte v)))
           \C (doseq [v array]
                (lsb/write-ubyte writer (Integer/parseInt v)))
           \s (doseq [v array]
                (lsb/write-short writer (Short/parseShort v)))
           \S (doseq [v array]
                (lsb/write-ushort writer (Integer/parseInt v)))
           \i (doseq [v array]
                (lsb/write-int writer (Integer/parseInt v)))
           \I (doseq [v array]
                (lsb/write-uint writer (Long/parseLong v)))
           \f (doseq [v array]
                (lsb/write-float writer (Float/parseFloat v))))
         writer)))

(defn get-block-size
  "Returns the number of bytes required to encode the given alignment."
  [aln]
  (let [read-length (.length ^String (:seq aln))
        cigar-length (cigar/count-op (:cigar aln))]
    (+ common/fixed-block-size
       (.length ^String (:qname aln))
       1 ;; null
       (* cigar-length 4)
       (quot (inc read-length) 2)
       read-length
       (get-options-size aln))))

(defn- add-cigar-to-options
  [options cigar]
  (cons
   {:CG {:type "B",:value (str "I," (cstr/join "," (cigar/encode-cigar cigar)))}}
   options))

(defn encode-alignment
  "Converts the alignment `aln` into a byte stream in the BAM file format
  and writes it to the given writer `wrtr`.
  `refs` is a sequence of names of reference sequences that can be acquired
  using `cljam.io.protocols/read-refs`."
  [wrtr aln refs]
  (let [aln (update aln :seq #(if (= % "*") "" %))
        cigar-ops-count (cigar/count-op (:cigar aln))
        [encoded-cigar cigar-ops-count opts*]
        (if (> cigar-ops-count 65535)
          [(cigar/->placeholder (:cigar aln))
           2 (add-cigar-to-options (:options aln) (:cigar aln))]
          [(cigar/encode-cigar (:cigar aln)) cigar-ops-count (:options aln)])]
    ;; refID
    (lsb/write-int wrtr (or (refs/ref-id refs (:rname aln)) -1))
    ;; pos
    (lsb/write-int wrtr (dec (long (:pos aln))))
    ;; bin_mq_nl
    (lsb/write-ubyte wrtr (short (inc (.length ^String (:qname aln)))))
    (lsb/write-ubyte wrtr (short (:mapq aln)))
    (lsb/write-ushort wrtr (sam-util/compute-bin aln))
    ;; flag_nc
    (lsb/write-ushort wrtr cigar-ops-count)
    (lsb/write-ushort wrtr (:flag aln))
    ;; l_seq
    (lsb/write-int wrtr (.length ^String (:seq aln)))
    ;; next_refID
    (lsb/write-int wrtr (get-next-ref-id aln refs))
    ;; next_pos
    (lsb/write-int wrtr (dec (long (:pnext aln))))
    ;; tlen
    (lsb/write-int wrtr (:tlen aln))
    ;; read_name
    (lsb/write-string wrtr (:qname aln))
    (lsb/write-bytes wrtr (byte-array 1 (byte 0)))
    ;; cigar
    (doseq [cigar encoded-cigar] (lsb/write-int wrtr cigar))
    ;; seq
    (lsb/write-bytes wrtr (seq/str->compressed-bases (:seq aln)))
    ;; qual
    (lsb/write-bytes wrtr (encode-qual aln))
    ;; options
    (doseq [opt opts*]
      (let [[tag value] (first (seq opt))]
        (lsb/write-short
         wrtr
         (short (bit-or (bit-shift-left (byte (int (second (name tag)))) 8)
                        (byte (int (first (name tag)))))))
        (lsb/write-bytes wrtr (.getBytes ^String (:type value)))
        (encode-tag-value wrtr (first (:type value)) (:value value))))))
