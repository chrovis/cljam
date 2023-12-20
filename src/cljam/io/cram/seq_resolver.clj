(ns cljam.io.cram.seq-resolver
  (:require [cljam.io.cram.seq-resolver.protocol :as proto]
            [cljam.io.sequence :as cseq])
  (:import [java.io Closeable]))

(deftype SeqResolver [seq-reader]
  java.io.Closeable
  (close [_]
    (.close ^Closeable seq-reader))
  proto/ISeqResolver
  (resolve-sequence [_ region]
    (cseq/read-sequence seq-reader region)))

(defn seq-resolver
  "Creates e new sequence resolver from the given sequence file."
  [seq-file]
  (->SeqResolver (cseq/reader seq-file)))

(defn clone-seq-resolver
  "Creates a cloned sequence resolver based on the given resolver."
  [^SeqResolver resolver]
  (->SeqResolver (cseq/reader (.-seq-reader resolver))))
