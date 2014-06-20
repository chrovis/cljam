(ns cljam.fasta-index.writer
  "Writing features for a FASTA index file."
  (:require [clojure.string :as str]
            [cljam.util :refer [graph?]])
  (:import [java.io BufferedWriter]))

;; FAIWriter
;; ---------

(deftype FAIWriter [writer f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

;; Indexing
;; --------

(defn- init-index-status
  []
  {:len 0, :line-len -1, :line-blen -1})

(defn- update-index-status
  [index-status sequence]
  (let [sequence (:sequence sequence)]
    (if (or (= (:line-len index-status) -1)
            (= (:line-blen index-status) -1))
      (assoc index-status
        :len (+ (:len index-status) (count (filter graph? sequence)))
        :line-len (inc (count sequence))
        :line-blen (count (filter graph? sequence)))
      (assoc index-status
        :len (+ (:len index-status) (count (filter graph? sequence)))))))

(defn- offset
  [headers name]
  (->> headers
       (filter #(= name (:name %)))
       first
       :offset))

(defn make-index
  [headers sequences]
  (loop [[sequence & rest] sequences
         name (:name sequence)
         idx-status (init-index-status)
         indices {}]
    (if sequence
      (let [name' (:name sequence)
            new-name? (not= name' name)
            idx-status' (update-index-status
                         (if new-name? (init-index-status) idx-status) sequence)
            indices' (if new-name?
                       (assoc indices name (assoc idx-status
                                                  :offset (offset headers name)))
                       indices)]
        (recur rest name' idx-status' indices'))
      (assoc indices name (assoc idx-status
                                 :offset (offset headers name))))))

;; Writing
;; -------

(defn- write-index*!
  [^BufferedWriter wtr headers sequences]
  (let [indices (make-index headers sequences)]
    (doseq [header headers]
      (let [index (get indices (:name header))]
        ;; (println (:name header) index)
        (.write wtr (str/join "\t"
                              [(:name header)
                               (:len index)
                               (:offset index)
                               (:line-blen index)
                               (:line-len index)]))
        (.newLine wtr)))))

(defn write-index!
  [^FAIWriter wtr headers sequences]
  (write-index*! (.writer wtr) headers sequences))
