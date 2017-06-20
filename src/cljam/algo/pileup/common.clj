(ns cljam.algo.pileup.common)

;; TODO: estiamte from actual data
(def ^:const window-width 5250)
(def ^:const step 10000)
(def ^:const center (int (/ step 2)))
