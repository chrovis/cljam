(ns cljam.t-twobit
  (:require [clojure.test :refer :all]
            [cljam.twobit :as tb]))

(deftest twobit-reference
  (with-open [r (tb/reader "./test-resources/test.2bit")]
    (are [?arg ?result] (= (tb/read-sequence r ?arg) ?result)
      {:chr "ref"} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
      {:chr "ref2"} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
      {:chr "ref2" :mask? true} "aggttttataaaacaattaagtctacagagcaactacgcg"
      {:chr "ref" :start 1 :end 4} "AGCA"
      {:chr "ref" :start 0 :end 4} "NAGCA"
      {:chr "ref" :start 41 :end 50} "GCCATNNNNN"
      {:chr "ref" :start 1 :end 45} "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
      {:chr "ref2" :start 1 :end 40} "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
      {:chr "ref2" :start 1 :end 40 :mask? true} "aggttttataaaacaattaagtctacagagcaactacgcg"
      {:chr "chr1" :start 1 :end 40} nil)
    (is (= (for [i (range 1 45) j (range i 46)]
             (tb/read-sequence r {:chr "ref" :start i :end j}))
           (for [i (range 1 45) j (range i 46)]
             (subs "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT" (dec i) j))))))

(deftest twobit-reference-with-n
  (with-open [r (tb/reader "./test-resources/test-n.2bit")]
    (are [?arg ?result] (= (tb/read-sequence r ?arg) ?result)
      {:chr "ref"} "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
      {:chr "ref2"} "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
      {:chr "ref2" :mask? true} "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
      {:chr "ref" :start 1 :end 4} "NNNN"
      {:chr "ref" :start 0 :end 4} "NNNNN"
      {:chr "ref" :start 41 :end 50} "NCCATNNNNN"
      {:chr "ref" :start 1 :end 45} "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
      {:chr "ref2" :start 1 :end 40} "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
      {:chr "ref2" :start 1 :end 40 :mask? true} "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
      {:chr "chr1" :start 1 :end 40} nil)
    (is (= (for [i (range 1 45) j (range i 46)]
             (tb/read-sequence r {:chr "ref" :start i :end j}))
           (for [i (range 1 45) j (range i 46)]
             (subs "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT" (dec i) j))))))

(deftest twobit-big-endian
  (with-open [r (tb/reader "./test-resources/be-test.2bit")]
    (is (= (tb/read-sequence r {:chr "ref"})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT")))
  (with-open [r (tb/reader "./test-resources/be-test-n.2bit")]
    (is (= (tb/read-sequence r {:chr "ref"})
           "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"))))
