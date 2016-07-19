(ns cljam.t-fastq
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :as io]
            [cljam.fastq :as fq]))

(fact
 "FASTQ file input"
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r)))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r :decode-quality :phred33)))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r :decode-quality nil)))) => test-fq-sequences-raw
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r :decode-quality :phred64)))) => throws AssertionError
 (with-open [r (fq/reader test-fq-gz-file)]
   (doall (map (partial into {}) (fq/read-sequence r)))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-bz2-file)]
   (doall (map (partial into {}) (fq/read-sequence r)))) => test-fq-sequences)

(with-state-changes
  [(before
    :facts
    (do (clean-cache!)
        (prepare-cache!)))
   (after
    :facts
    (clean-cache!))]
  (fact
   "FASTQ file output"
   (let [path (.getAbsolutePath (io/file temp-dir "test.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test-33.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences :encode-quality :phred33))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test-raw.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences-raw :encode-quality nil))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test-64.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences :encode-quality :phred64))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => throws AssertionError

   (let [path (.getAbsolutePath (io/file temp-dir "test.fq.gz"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test.fq.bz2"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences))

(def sample-1_8 {:instrument "EAS139"
                 :run 136
                 :flowcell "FC706VJ"
                 :lane 2
                 :tile 2104
                 :x 15343
                 :y 197393
                 :pair 1
                 :filtered true
                 :control 18
                 :index "ATCACG"})

(def sample {:instrument "HWUSI-EAS100R"
             :lane 6
             :tile 73
             :x 941
             :y 1973
             :index 0
             :pair 1})

(fact
 "FASTQ read ID parsing"
 (fq/deserialize-name "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG")
 => sample-1_8

 (fq/deserialize-casava-1_8-name "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG")
 => sample-1_8

 (fq/deserialize-name "@EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG")
 => sample-1_8

 (fq/serialize-name sample-1_8)
 => "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG"

 (fq/serialize-casava-1_8-name sample-1_8)
 => "EAS139:136:FC706VJ:2:2104:15343:197393 1:Y:18:ATCACG"

 (fq/deserialize-name "HWUSI-EAS100R:6:73:941:1973#0/1")
 => sample

 (fq/deserialize-name "@HWUSI-EAS100R:6:73:941:1973#0/1")
 => sample

 (fq/serialize-name sample)
 => "HWUSI-EAS100R:6:73:941:1973#0/1"

 (fq/serialize-casava-name sample)
 => "HWUSI-EAS100R:6:73:941:1973#0/1"

 (fq/deserialize-name "@EAS139:136:FC706VJ:2:2104:15343:197393_1:Y:18:ATCACG")
 => nil

 (fq/deserialize-name "@HWUSI-EAS100R:6:73:941:1973#N/1")
 => nil)
