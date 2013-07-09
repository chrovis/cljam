(ns cljam.bam
  :use [clojure.java.io :only (file reader writer)])

(defrecord Bam1Core
    [tid pos bin qual l_qname flag n-cigar l-qseq mtid mpos isize])

(defrecord Bam1
    [bam1-core
     l-aux data-len m-data
     data])

(defn bam-header-read [bam-reader])

(defn bam-index-core [bam-reader]
  nil)

(defn index [bam-path]
  (let [out-path (str bam-path ".bai")]
   (with-open [r (reader bam-path)]
     (with-open [w (writer out-path)]
       nil)
     nil)))
