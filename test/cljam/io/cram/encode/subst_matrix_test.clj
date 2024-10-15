(ns cljam.io.cram.encode.subst-matrix-test
  (:require [cljam.io.cram.encode.subst-matrix :as subst-mat]
            [clojure.test :refer [deftest is]]))

(deftest make-subst-matrix-builder-test
  (let [builder (subst-mat/make-subst-matrix-builder)
        a->t (subst-mat/assign-code! builder (int \A) (int \T))
        a->c (subst-mat/assign-code! builder (int \A) (int \C))
        a->g (subst-mat/assign-code! builder (int \A) (int \G))
        a->t' (subst-mat/assign-code! builder (int \A) (int \T))
        g->c (subst-mat/assign-code! builder (int \G) (int \C))
        g->t (subst-mat/assign-code! builder (int \G) (int \T))
        t->n (subst-mat/assign-code! builder (int \T) (int \N))
        n->c (subst-mat/assign-code! builder (int \N) (int \C))]
    ;; before build-subst-matrix is called, the returned references retain
    ;; the frequencies of how many times each corresponding SNV was observed
    (is (= 2 @a->t @a->t'))
    (is (= 1 @a->c))
    (is (= 1 @a->g))
    (is (= 1 @g->c))
    (is (= 1 @g->t))
    (is (= 1 @t->n))
    (is (= 1 @n->c))
    ;; build-subst-matrix returns a substitution matrix built based on the frequencies
    (is (= {\A {\C 1, \G 2, \T 0, \N 3}
            \C {\A 0, \G 1, \T 2, \N 3}
            \G {\A 2, \C 0, \T 1, \N 3}
            \T {\A 1, \C 2, \G 3, \N 0}
            \N {\A 1, \C 0, \G 2, \T 3}}
           (subst-mat/build-subst-matrix builder)))
    ;; once build-subst-matrix has been called, the returned references get
    ;; updated to hold the value of the base substitution code assigned to each
    ;; corresponding SNV
    (is (= 0 @a->t @a->t'))
    (is (= 1 @a->c))
    (is (= 2 @a->g))
    (is (= 0 @g->c))
    (is (= 1 @g->t))
    (is (= 0 @t->n))
    (is (= 0 @n->c))))
