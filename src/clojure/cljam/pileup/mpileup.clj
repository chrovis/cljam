(ns cljam.pileup.mpileup
  (:require [clojure.string :as str]
            [cljam.cigar :as cgr]
            [cljam.sequence :as cseq]
            [cljam.io :as io]
            [cljam.fasta :as fa]
            [cljam.util :refer [ubyte]]
            [cljam.util.sam-util :refer [phred->fastq fastq->phred]]
            [cljam.pileup.common :refer [window-width step center]]
            [cljam.pileup.pileup :refer [rpositions]])
  (:import cljam.fasta.FASTAReader))

(defn- pickup-qual [aln pos]
  (if (= (:qual aln) "*")
    \~
    (nth (:qual aln) pos)))

(defn- encode-seq* [seq*]
  (loop [[f & r] (filter #(nil? (#{\P} (:op %))) seq*)
         ret     []
         op      nil
         tmp     {:n 0, :op nil, :seq nil}]
    (if (nil? f)
      (case op
        \M (apply conj ret (map str (:seq tmp)))
        \I (if (seq ret)
             (update-in ret [(dec (count ret))] str "+" (:n tmp) (apply str (:seq tmp)))
             ret)
        \D (apply conj ret (map str (:seq tmp)))
        \N (apply conj ret (map str (:seq tmp)))
        ret)
      (if (nil? op)
        (recur r ret (:op f) f)
        (if (= (:op f) op)
          (recur r ret (:op f) (-> tmp
                                   (update-in [:n] + (:n f))
                                   (assoc :op (:op f))
                                   (update-in [:seq] (partial apply conj) (:seq f))))
          (let [new-ret (case op
                          \M (apply conj ret (map str (:seq tmp)))
                          \I (if (seq ret)
                               (update-in ret [(dec (count ret))] str "+" (:n tmp) (apply str (:seq tmp)))
                               ret)
                          \D (apply conj ret (map str (:seq tmp)))
                          \N (apply conj ret (map str (:seq tmp)))
                          ret)]
            (recur r new-ret (:op f) f)))))))

(defn- encode-seq
  "Encode sequence strings, returning string sequence for mpileup output.
  e.g. ({:n 2, :op \\M :seq [\\T \\A]} ... {:n 1, :op \\M :seq [\\C]})
       => (\"^?TA\" ... \"C$\")"
  [seq*]
  (let [seq** (encode-seq* seq*)]
    (-> (update-in seq** [(dec (count seq**))] str "$") ; Append "$" to the end
        (update-in [0] #(str "^?" %)))))                ; Insert "^?" before the begin

(defrecord ^:private PileupStatus [count seq qual])

(defn- count-for-alignment
  [aln rname positions]
  (if (= rname (:rname aln))
    (let [left  (:pos aln)
          right (dec (+ left (cgr/count-ref (:cigar aln))))
          seq*  (encode-seq (cseq/parse (:seq aln) (:cigar aln)))]
      (map (fn [p]
             (if (<= left p right)
               (PileupStatus. 1 (nth seq* (- p left)) (pickup-qual aln (- p left)))
               (PileupStatus. 0 nil nil))) positions))
    (repeat (count positions) (PileupStatus. 0 nil nil))))

(defn- pickup-ref
  [ref-line pos]
  (if (nil? ref-line)
    \N
    (let [idx (dec pos)]
      (if (neg? idx)
        \N
        (if-let [ref (nth (:seq ref-line) idx)]
          ref
          \N)))))

(defn- count-for-positions
  [alns ref-line rname positions]
  (if (pos? (count alns))
    (let [cfas (map #(count-for-alignment % rname positions) alns)
          plp1 (apply map (fn [& a]
                            {:rname rname
                             :count (reduce + (map :count a))
                             :seq   (str/join (map :seq a))
                             :qual  (str/join (map :qual a))}) cfas)]
      (map #(assoc %2 :pos %1 :ref (pickup-ref ref-line %2))
           positions plp1))
    (let [plp1 (repeat (count positions) {:rname rname
                                          :count 0
                                          :seq   nil
                                          :qual  nil})]
      (map #(assoc %2 :pos %1 :ref (pickup-ref ref-line %2))
           positions plp1))))

(defn- read-alignments
  [rdr rname rlength pos]
  (let [left (let [val (- pos window-width)]
               (if (< val 0)
                 0
                 val))
        right (let [val (+ pos window-width)]
                (if (< rlength val)
                  rlength
                  val))]
    (io/read-alignments rdr {:chr rname
                             :start left
                             :end right
                             :depth :deep})))

(defn- read-ref-fasta-line
  [fa-rdr rname]
  (if-not (nil? fa-rdr)
   (first
    (filter #(= (:rname %) rname) (fa/read fa-rdr)))))

(defn- search-ref
  [refs rname]
  (first
   (filter (fn [r] (= (:name r) rname))
           refs)))

(defn- pileup*
  ([rdr ^FASTAReader fa-rdr rname rlength start end]
     (flatten
      (let [parts (partition-all step (rpositions start end))]
        (map (fn [positions]
               (let [pos (if (= (count positions) step)
                           (nth positions center)
                           (nth positions (quot (count positions) 2)))
                     alns (read-alignments rdr rname rlength pos)
                     ref-line (read-ref-fasta-line fa-rdr rname)]
                 (count-for-positions alns ref-line rname positions)))
             parts)))))

(defn pileup
  ([rdr rname]
     (pileup rdr rname -1 -1))
  ([rdr rname start* end* & {:keys [ref-fasta] :or {ref-fasta nil}}]
     (let [r (search-ref (.refs rdr) rname)]
       (if (nil? r)
         nil
         (pileup* rdr
                  ref-fasta
                  rname (:len r)
                  (if (neg? start*) 0 start*)
                  (if (neg? end*) (:len r) end*))))))
