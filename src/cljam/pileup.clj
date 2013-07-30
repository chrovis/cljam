(ns cljam.pileup
  (:require [cljam.sorter :as sorter]))

;;; FIXME: incorrect algorithm
(defn pileup
  [sam]
  (if-not (sorter/sorted? sam)
    (throw (Exception. "Not sorted"))
    (loop [ret []
           tmp {}
           sas (:alignments sam)]
      (if (seq sas)
        (let [sa   (first sas)
              [fin res] (split-with #(> (:pos sa) (first %)) tmp)]
          (recur (concat ret (map #(assoc (second %) :pos (first %)) fin))
                 (loop [tmp2 (zipmap (map first res) (map second res))
                        pos  (:pos sa)]
                   (if (< pos (+ (:pos sa) (count (:seq sa))))
                     (recur (assoc tmp2 pos
                                   (if-let [coll (tmp2 pos)]
                                     (update-in coll [:n] inc)
                                     {:rname (:rname sa), :n 1}))
                            (inc pos))
                     tmp2))
                 (rest sas)))
        ret))))
