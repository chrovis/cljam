(ns cljam.io.cram.encode.subst-matrix)

(defprotocol ICounter
  (inc! [this]))

(defprotocol IMutableInt
  (set-val! [this v]))

(deftype MutableInt [^:unsynchronized-mutable ^long n]
  ICounter
  (inc! [_]
    (set! n (inc n)))
  IMutableInt
  (set-val! [_ v]
    (set! n (long v)))
  clojure.lang.IDeref
  (deref [_] n))

(defprotocol ISubstMatrixBuilder
  (assign-code! [this ref alt])
  (build-subst-matrix [this]))

(definline ^:private base->index [b]
  `(case (long ~b)
     ~(int \A) 0
     ~(int \C) 1
     ~(int \G) 2
     ~(int \T) 3
     ~(int \N) 4))

(deftype SubstMatrixBuilder [^objects freqs]
  ISubstMatrixBuilder
  (assign-code! [_ r a]
    (let [mut (aget ^objects (aget freqs (base->index r)) (base->index a))]
      (inc! mut)
      mut))
  (build-subst-matrix [_]
    ;; Up to this point, the `MutableInt`s contained in the freqs hold
    ;; the frequency of how many times the corresponding SNV has been observed.
    ;; From this point on, a base substitution code (BS code) is assigned to
    ;; each SNV based on the frequencies, and the value of each `MutableInt` is
    ;; replaced with the BS code of the corresponding SNV.
    (let [all-bases [\A \C \G \T \N]]
      (reduce
       (fn [m r]
         (let [^objects freqs (aget freqs (base->index (int r)))]
           (->> all-bases
                (keep (fn [a]
                        (when (not= r a)
                          [a (aget freqs (base->index (int a)))])))
                (sort-by (comp - deref second))
                (map-indexed vector)
                (reduce (fn [m [i [a mut]]]
                          (set-val! mut i)
                          (assoc-in m [r a] i))
                        m))))
       {} all-bases))))

(defn make-subst-matrix-builder
  "Creates a new substitution matrix builder."
  []
  (->> (fn [] (into-array (repeatedly 5 #(->MutableInt 0))))
       (repeatedly 5)
       into-array
       ->SubstMatrixBuilder))
