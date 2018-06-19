(ns cljam.io.fasta-index.writer
  "Writing features for a FASTA index file."
  (:require [clojure.string :as cstr]
            [cljam.io.fasta.util :refer [header-line? parse-header-line]])
  (:import [java.io BufferedWriter]))

;;;; FAIWriter

(deftype FAIWriter [writer url]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

;;;; Writing

(defn make-indices
  [^java.io.BufferedReader r]
  (let [indices (atom [])
        current-index (atom nil)]
    (loop [l (.readLine r)
           pos 0]
      (when-not (nil? l)
        (let [llen (count l)]
          (if (header-line? l)
            (let [h (parse-header-line l)
                  i {:name (:name h)
                     :len 0
                     :offset (+ pos (inc llen))
                     :line-blen nil
                     :line-len nil}]
              (when-not (nil? @current-index)
                (swap! indices conj @current-index))
              (reset! current-index i))
            (do (swap! current-index
                       assoc
                       :len (+ (:len @current-index) llen))
                (when (or (nil? (:line-blen @current-index))
                          (nil? (:line-len @current-index)))
                  (swap! current-index
                         assoc
                         :line-blen llen
                         :line-len (inc llen)))))
          (recur (.readLine r)
                 (+ pos (inc llen))))))
    (when-not (nil? @current-index)
      (swap! indices conj @current-index))
    @indices))

(defn- write-index*!
  [^BufferedWriter wtr indices]
  (doseq [i indices]
    (.write wtr (cstr/join "\t"
                           [(:name i)
                            (:len i)
                            (:offset i)
                            (:line-blen i)
                            (:line-len i)]))
    (.newLine wtr)))

(defn write-index!
  [rdr ^FAIWriter wtr]
  (let [indices (make-indices rdr)]
    (write-index*! (.writer wtr) indices)))
