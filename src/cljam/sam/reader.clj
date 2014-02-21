(ns cljam.sam.reader
  (:use [cljam.io])
  (:require [clojure.java.io :refer [file]]
            [clojure.tools.logging :as logging]
            [cljam.util.sam-util :refer [make-refs
                                         parse-alignment
                                         parse-header-line]])
  (:import [java.io BufferedReader Closeable]))

;;; reader

(deftype SAMReader [f header reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(extend-type SAMReader
  ISAMReader
  (reader-path [this]
    (.f this))
  (read-header [this]
    (.header this))
  (read-refs [this]
    (vec (make-refs (.header this))))
  (read-alignments [this _]
    (when-let [line (.readLine ^BufferedReader (.reader this))]
      (if-not (= (first line) \@)
        (cons (parse-alignment line) (lazy-seq (read-alignments this {})))
        (lazy-seq (read-alignments this {})))))
  (read-blocks [this]
    (logging/info "SAMReader does not support read-blocks"))
  (read-coordinate-blocks [this]
    (logging/info "SAMReader does not support read-coordinate-blocks")))

(defn- read-header* [^BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (if (= (first line) \@)
      (merge-with #(vec (concat %1 %2)) (parse-header-line line) (read-header* rdr)))))

(defn reader [f]
  (let [header (with-open [r (clojure.java.io/reader f)]
                 (read-header* r))]
    (->SAMReader (.getAbsolutePath (file f))
                 header (clojure.java.io/reader f))))
