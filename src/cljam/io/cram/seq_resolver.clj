(ns cljam.io.cram.seq-resolver
  (:require [cljam.io.cram.seq-resolver.protocol :as proto]
            [cljam.io.sequence :as cseq]
            [clojure.core.cache.wrapped :as cache])
  (:import [java.io Closeable]
           [java.util Arrays]))

(deftype SeqResolver [seq-reader]
  java.io.Closeable
  (close [_]
    (.close ^Closeable seq-reader))
  proto/ISeqResolver
  (resolve-sequence [this chr]
    (proto/resolve-sequence this chr nil nil))
  (resolve-sequence [_ chr start end]
    (when-let [s (cseq/read-sequence seq-reader {:chr chr :start start :end end})]
      (.getBytes ^String s))))

(defn seq-resolver
  "Creates a new sequence resolver from the given sequence file."
  [seq-file]
  (->SeqResolver (cseq/reader seq-file)))

(defn clone-seq-resolver
  "Creates a cloned sequence resolver based on the given resolver."
  [^SeqResolver resolver]
  (->SeqResolver (cseq/reader (.-seq-reader resolver))))
