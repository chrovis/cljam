(ns cljam.sam.writer
  (:use [cljam.io])
  (:require [clojure.java.io :refer [file]]
            [clojure.tools.logging :as logging]
            [cljam.util.sam-util :refer [stringify-header
                                         stringify-alignment]])
  (:import [java.io BufferedWriter Closeable]))

;;; writer

(deftype SAMWriter [f writer]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))))

(defn writer [f]
  (->SAMWriter (.getAbsolutePath (file f))
               (clojure.java.io/writer f)))

(extend-type SAMWriter
  ISAMWriter
  (writer-path [this]
    (.f this))
  (write-header [this header]
    (.write (.writer this) ^String (stringify-header header))
    (.newLine (.writer this)))
  (write-refs [this refs]
    (logging/info "SAMWriter does not support write-refs"))
  (write-alignments [this alignments refs]
    (doseq [a alignments]
      (.write (.writer this) ^String (stringify-alignment a))
      (.newLine (.writer this))))
  (write-blocks [this blocks]
    ;;(logging/info "SAMWriter does not support write-blocks")
    )
  (write-coordinate-blocks [this blocks]
    ;;(logging/info "SAMWriter does not support write-coordinate-blocks")
    ))
