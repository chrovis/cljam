(ns cljam.sam.reader
  (:use [cljam.io])
  (:require [clojure.java.io :refer [file]]
            [clojure.tools.logging :as logging]
            [cljam.util.sam-util :refer [make-refs
                                         parse-alignment] :as sam-util])
  (:import [java.io BufferedReader Closeable]))

(declare read-alignments* read-blocks*)

;;; reader

(deftype SAMReader [f header reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  ISAMReader
  (reader-path [this]
    (.f this))
  (read-header [this]
    (.header this))
  (read-refs [this]
    (vec (make-refs (.header this))))
  (read-alignments [this]
    (read-alignments* this))
  (read-alignments [this _]
    (read-alignments* this))
  (read-blocks [this]
    (read-blocks* this))
  (read-blocks [this option]
    (read-blocks* this)))

(defn- read-alignments*
  [^SAMReader sam-reader]
  (when-let [line (.readLine ^BufferedReader (.reader sam-reader))]
    (if-not (= (first line) \@)
      (cons (parse-alignment line) (lazy-seq (read-alignments* sam-reader)))
      (lazy-seq (read-alignments* sam-reader)))))

(defn- read-blocks*
  [^SAMReader sam-reader]
  (when-let [line (.readLine ^BufferedReader (.reader sam-reader))]
    (if-not (= (first line) \@)
      (cons {:line line} (lazy-seq (read-blocks* sam-reader)))
      (lazy-seq (read-blocks* sam-reader)))))

(defn- read-header* [^BufferedReader rdr]
  (->> (line-seq rdr)
       (transduce
        (comp
         (take-while (fn [line] (= (first line) \@)))
         (map sam-util/parse-header-line))
        sam-util/into-header)))

(defn reader [f]
  (let [header (with-open [r (clojure.java.io/reader f)]
                 (read-header* r))]
    (->SAMReader (.getAbsolutePath (file f))
                 header (clojure.java.io/reader f))))
