(ns cljam.tabix
  (:require [clojure.java.io :as io]
            [cljam.lsb :as lsb])
  (:import java.util.Arrays
           [java.io DataInputStream IOException]
           bgzf4j.BGZFInputStream))

(set! *warn-on-reflection* true)

(def tabix-magic "TBI\1")

(def ^:private max-bin 37450)

(def ^:private tad-min-chunk-gap 32768)

(def ^:private tad-lidx-shift 14)

(defn- read-index* [^DataInputStream rdr]
  (when-not (Arrays/equals (lsb/read-bytes rdr 4) (.getBytes tabix-magic))
    (throw (IOException. "Invalid TABIX file"))
    (let [seq    (lsb/read-int rdr)
          preset (lsb/read-int rdr)
          sc     (lsb/read-int rdr)
          bc     (lsb/read-int rdr)
          ec     (lsb/read-int rdr)
          meta   (lsb/read-int rdr)
          skip   (lsb/read-int rdr)
          len    (lsb/read-int rdr)
          buf    (lsb/read-bytes len)
          ; TODO
          ])
    )
  nil)

(deftype TABIXReader [f ^DataInputStream reader]
  java.io.Closeable
  (close [this]
    (.close ^DataInputStream (.reader this))))

(defn read-index
  [^TABIXReader rdr]
  (read-index* (.reader rdr)))

(defn reader [f]
  (->TABIXReader f (DataInputStream. (BGZFInputStream. (io/file f)))))

;; (defn main [f]
;;   (let [tr (TabixReader. f)]
;;     (loop [s (.readLine tr)]
;;       (when s
;;         (println s)
;;         (recur (.readLine tr))))))
