(ns cljam.io.bam.encoder
  "Encoder of BAM alignment blocks."
  (:require [clojure.string :as cstr]
            [cljam.util :as util]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.util.cigar :as cgr]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.bam.common :as common]))

(def ^:private fixed-tag-size 3)
(def ^:private fixed-binary-array-tag-size 5)

(defn- get-next-ref-id [sa refs]
  (condp = (:rnext sa)
    "*" -1
    "=" (if-let [id (sam-util/ref-id refs (:rname sa))] id -1)
    (if-let [id (sam-util/ref-id refs (:rnext sa))] id -1)))

(defn- get-options-size [sam-alignment]
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
    (sam-util/fastq->phred (:qual sam-alignment))))

(defn- encode-tag-value [writer val-type value]
  (case val-type
    \A (lsb/write-char writer (char value))
    \i (lsb/write-int writer (int value))
    \f (lsb/write-float writer (float value))
    \Z (do (lsb/write-string writer value)
           (lsb/write-char writer (char 0)))
    ;; \H nil
    \B (let [[array-type & array] (cstr/split value #",")]
         (case (first array-type)
           \c nil
           \C nil
           \s nil
           \S (do
                (lsb/write-bytes writer (byte-array 1 (byte \S)))
                (lsb/write-int writer (count array))
                (doseq [v array]
                 (lsb/write-short writer (Short/parseShort v))))
           \i nil
           \I nil
           \f nil))))

(defn get-block-size [aln]
  (let [read-length (.length ^String (:seq aln))
        cigar-length ^long (cgr/count-op (:cigar aln))]
    (+ ^long common/fixed-block-size
       (.length ^String (:qname aln))
       1 ;; null
       (* cigar-length 4)
       (quot (inc read-length) 2)
       read-length
       ^long (get-options-size aln))))

(defn encode-alignment [wrtr aln refs]
  ;; refID
  (lsb/write-int wrtr (or (sam-util/ref-id refs (:rname aln)) -1))
  ;; pos
  (lsb/write-int wrtr (dec (:pos aln)))
  ;; bin_mq_nl
  (lsb/write-ubyte wrtr (short (inc (.length ^String (:qname aln)))))
  (lsb/write-ubyte wrtr (short (:mapq aln)))
  (lsb/write-ushort wrtr (sam-util/compute-bin aln))
  ;; flag_nc
  (lsb/write-ushort wrtr (cgr/count-op (:cigar aln)))
  (lsb/write-ushort wrtr (:flag aln))
  ;; l_seq
  (lsb/write-int wrtr (.length ^String (:seq aln)))
  ;; next_refID
  (lsb/write-int wrtr (get-next-ref-id aln refs))
  ;; next_pos
  (lsb/write-int wrtr (dec (:pnext aln)))
  ;; tlen
  (lsb/write-int wrtr (:tlen aln))
  ;; read_name
  (lsb/write-string wrtr (:qname aln))
  (lsb/write-bytes wrtr (byte-array 1 (byte 0)))
  ;; cigar
  (doseq [cigar (cgr/encode-cigar (:cigar aln))]
    (lsb/write-int wrtr cigar))
  ;; seq
  (lsb/write-bytes wrtr (sam-util/str->compressed-bases (:seq aln)))
  ;; qual
  (lsb/write-bytes wrtr (encode-qual aln))
  ;; options
  (doseq [op (:options aln)]
    (let [[tag value] (first (seq op))]
      (lsb/write-short wrtr (short (bit-or (bit-shift-left (byte (second (name tag))) 8)
                                           (byte (first (name tag))))))
      (lsb/write-bytes wrtr (.getBytes ^String (:type value)))
      (encode-tag-value wrtr (first (:type value)) (:value value)))))
