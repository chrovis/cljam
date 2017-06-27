(ns cljam.algo.dedupe
  "Functions to remove duplications."
  (:refer-clojure :exclude [dedupe])
  (:require [com.climate.claypoole :as cp]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util :as sam-util]))

(defn- refs->regions [refs]
  (for [{:keys [name len]} refs]
    {:chr name :start 1 :end len}))

(defn- sum-quals [a]
  (when-let [q ^String (:qual a)]
    (when-not (and (= (.length q) 1) (= \* (.charAt q 0)))
      (reduce (fn [r x] (+ r (- (int x) 33))) 0 q))))

(defn- paired? [{:keys [flag]}]
  (pos? (bit-and flag (sam-util/flags :multiple))))

(defn- mapped? [{:keys [flag]}]
  (zero? (bit-and flag (bit-or (sam-util/flags :unmapped) (sam-util/flags :next-unmapped)))))

(defn dedupe-xform
  "Returns a transducer which removes PCR duplications."
  [& {:keys [remove-dups] :or {remove-dups true}}]
  (let [removed (atom #{})
        cp (juxt sum-quals :mapq)]
    (comp
     (partition-by (juxt :rname :pos))
     (mapcat
      (fn [alns]
        (loop [[{:keys [pos tlen qname] :as x} :as xs] alns heads {} tails [] dups []]
          (if x
            (cond
              (not (and (paired? x) (mapped? x) (= (:rnext x) "="))) (recur (next xs) heads (conj tails x) dups)
              (pos? tlen) (let [k [pos tlen]]
                            (if-let [v (get heads k)]
                              (let [[good bad] (if (pos? (compare (cp v) (cp x))) [v x] [x v])]
                                (swap! removed conj (:qname bad))
                                (recur (next xs) (assoc heads k good) tails (conj dups bad)))
                              (recur (next xs) (assoc heads k x) tails dups)))
              (@removed qname) (do (swap! removed disj qname) (recur (next xs) heads tails (conj dups x)))
              :else (recur (next xs) heads (conj tails x) dups))
            (concat
             (vals heads)
             tails
             (when-not remove-dups
               (map (fn [a] (update a :flag #(bit-or % 1024))) dups))))))))))

(defn dedupe
  "Removes PCR duplications from paired-end alignments."
  [in out & {:keys [remove-dups] :or {remove-dups true}}]
  (cp/with-shutdown! [pool (cp/ncpus)]
    (let [[header refs] (with-open [r (sam/bam-reader in)] [(sam/read-header r) (sam/read-refs r)])]
      (with-open [w (sam/bam-writer out)]
        (sam/write-header w header)
        (sam/write-refs w header)
        (sam/write-alignments
         w
         (->> refs
              refs->regions
              (cp/pmap
               pool
               (fn [region]
                 (with-open [r (sam/bam-reader in)]
                   (->> (sam/read-alignments r region)
                        (sequence (dedupe-xform :remove-dups remove-dups))
                        doall))))
              (sequence cat))
         header)))))
