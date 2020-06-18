(ns cljam.io.bam-index.writer-test
  "Tests for cljam.io.bam-index."
  (:require [clojure.test :refer [deftest is testing]]
            [cljam.io.bam-index.writer :as writer]))

(def update-last-pointer 29229056)
(def input-index-1 {0 {:meta-data {:first-offset 97
                                   :last-offset 555
                                   :aligned-alns 6
                                   :unaligned-alns 0}
                       :bin-index {4681 [{:beg 97, :end 555}]}
                       :linear-index {0 97}}
                    1 {:meta-data {:first-offset 555
                                   :last-offset 1031
                                   :aligned-alns 6
                                   :unaligned-alns 0}
                       :bin-index {4681 [{:beg 555, :end 1031}]}
                       :linear-index {0 555}}
                    :no-coordinate-alns 0})
(def output-index-1 {0 {:meta-data {:first-offset 97
                                    :last-offset 555
                                    :aligned-alns 6
                                    :unaligned-alns 0}
                        :bin-index {4681 [{:beg 97, :end 555}]}
                        :linear-index {0 97}}
                     1 {:meta-data {:first-offset 555
                                    :last-offset 29229056
                                    :aligned-alns 6
                                    :unaligned-alns 0}
                        :bin-index {4681 [{:beg 555, :end 29229056}]}
                        :linear-index {0 555}}
                     :no-coordinate-alns 0})
(def input-index-2 {0 {:meta-data {:first-offset 97
                                   :last-offset 555
                                   :aligned-alns 6
                                   :unaligned-alns 0}
                       :bin-index {4681 [{:beg 97, :end 345}]
                                   4781 [{:beg 345, :end 555}]}
                       :linear-index {0 97}}
                    1 {:meta-data {:first-offset 555
                                   :last-offset 1031
                                   :aligned-alns 6
                                   :unaligned-alns 0}
                       :bin-index {4881 [{:beg 555, :end 800}
                                         {:beg 900, :end 1031}]}
                       :linear-index {0 555}}
                    2 {:meta-data {:first-offset 1031
                                   :last-offset 2031
                                   :aligned-alns 6
                                   :unaligned-alns 0}
                       :bin-index {4981 [{:beg 1031, :end 1131}
                                         {:beg 1231, :end 1331}]
                                   5081 [{:beg 1331, :end 1431}
                                         {:beg 1531, :end 2031}]}
                       :linear-index {0 1031}}
                    :no-coordinate-alns 0})
(def output-index-2 {0 {:meta-data {:first-offset 97
                                    :last-offset 555
                                    :aligned-alns 6
                                    :unaligned-alns 0}
                        :bin-index {4681 [{:beg 97, :end 345}]
                                    4781 [{:beg 345, :end 555}]}
                        :linear-index {0 97}}
                     1 {:meta-data {:first-offset 555
                                    :last-offset 1031
                                    :aligned-alns 6
                                    :unaligned-alns 0}
                        :bin-index {4881 [{:beg 555, :end 800}
                                          {:beg 900, :end 1031}]}
                        :linear-index {0 555}}
                     2 {:meta-data {:first-offset 1031
                                    :last-offset 29229056
                                    :aligned-alns 6
                                    :unaligned-alns 0}
                        :bin-index {4981 [{:beg 1031, :end 1131}
                                          {:beg 1231, :end 1331}]
                                    5081 [{:beg 1331, :end 1431}
                                          {:beg 1531, :end 29229056}]}
                        :linear-index {0 1031}}
                     :no-coordinate-alns 0})
(def input-index-not-update-1 {:no-coordinate-alns 0})
(def input-index-not-update-2 {0 {:meta-data {:first-offset 97
                                              :last-offset 555
                                              :aligned-alns 6
                                              :unaligned-alns 0}
                                  :bin-index {4681 [{:beg 97, :end 555}]}
                                  :linear-index {0 97}}
                               1 {:meta-data {:first-offset 555
                                              :last-offset 1031
                                              :aligned-alns 6
                                              :unaligned-alns 0}
                                  :bin-index {4681 [{:beg 555, :end 1031}]}
                                  :linear-index {0 555}}
                               :no-coordinate-alns 3})

;;; unit test

;;; update-last-pointer

(deftest update-last-pointer-test
  (testing "update"
    (let [output-1 (writer/update-last-pointer input-index-1 update-last-pointer)
          output-2 (writer/update-last-pointer input-index-2 update-last-pointer)]
      (is (= output-1 output-index-1))
      (is (= output-2 output-index-2))))
  (testing "not update"
    (let [output-1 (writer/update-last-pointer input-index-not-update-1 update-last-pointer)
          output-2 (writer/update-last-pointer input-index-not-update-2 update-last-pointer)]
      (is (= output-1 input-index-not-update-1))
      (is (= output-2 input-index-not-update-2)))))
