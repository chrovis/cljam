(ns cljam.fasta
  (:refer-clojure :exclude [slurp])
  (:import java.io.RandomAccessFile))

(defn slurp
  "Opens a reader on a fasta-file and reads all its contents, returning a map
  about the data."
  [f]
  (with-open [r (RandomAccessFile. f "r")]
    (loop [fa []
           line (.readLine r)]
      (if (nil? line)
        fa
        (if (= (first line) \>)
          (let [ref    (subs line 1)
                offset (.getFilePointer r)
                seq    (.readLine r)
                blen   (count (filter (partial not= \space) seq))]
            (recur (conj fa {:ref ref, :offset offset, :seq seq, :blen blen})
                   (.readLine r)))
          (recur fa (.readLine r)))))))
