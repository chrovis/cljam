(ns cljam.t-twobit
  (:require [clojure.test :refer :all]
            [cljam.twobit :as tb]))

(deftest twobit-reference
  (with-open [r (tb/reader "./test-resources/test.2bit")]
    (is (= (tb/read-sequence r {:chr "ref"})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
    (is (= (tb/read-sequence r {:chr "ref2"})
           "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
    (is (= (tb/read-sequence r {:chr "ref2" :mask? true})
           "aggttttataaaacaattaagtctacagagcaactacgcg"))
    (is (= (for [i (range 1 45) j (range i 46)]
             (tb/read-sequence r {:chr "ref" :start i :end j}))
           (for [i (range 1 45) j (range i 46)]
             (subs "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT" (dec i) j))))
    (is (= (tb/read-sequence r {:chr "ref" :start 1 :end 4}) "AGCA"))
    (is (= (tb/read-sequence r {:chr "ref" :start 0 :end 4}) "NAGCA"))
    (is (= (tb/read-sequence r {:chr "ref" :start 41 :end 50}) "GCCATNNNNN"))
    (is (= (tb/read-sequence r {:chr "ref" :start 1 :end 45})
           "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"))
    (is (= (tb/read-sequence r {:chr "ref2" :start 1 :end 40})
           "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"))
    (is (= (tb/read-sequence r {:chr "ref2" :start 1 :end 40 :mask? true})
           "aggttttataaaacaattaagtctacagagcaactacgcg"))
    (is (= (tb/read-sequence r {:chr "chr1" :start 1 :end 40}) nil))))

(deftest twobit-reference-with-n
  (with-open [r (tb/reader "./test-resources/test-n.2bit")]
    (is (= (tb/read-sequence r {:chr "ref"})
           "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"))
    (is (= (tb/read-sequence r {:chr "ref2"})
           "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"))
    (is (= (tb/read-sequence r {:chr "ref2" :mask? true})
           "agNNNttataaaacaattaNNNctacagagcaactaNNNN"))
    (is (= (for [i (range 1 45) j (range i 46)]
             (tb/read-sequence r {:chr "ref" :start i :end j}))
           (for [i (range 1 45) j (range i 46)]
             (subs "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT" (dec i) j))))
    (is (= (tb/read-sequence r {:chr "ref" :start 1 :end 4}) "NNNN"))
    (is (= (tb/read-sequence r {:chr "ref" :start 0 :end 4}) "NNNNN"))
    (is (= (tb/read-sequence r {:chr "ref" :start 41 :end 50}) "NCCATNNNNN"))
    (is (= (tb/read-sequence r {:chr "ref" :start 1 :end 45})
           "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"))
    (is (= (tb/read-sequence r {:chr "ref2" :start 1 :end 40})
           "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"))
    (is (= (tb/read-sequence r {:chr "ref2" :start 1 :end 40 :mask? true})
           "agNNNttataaaacaattaNNNctacagagcaactaNNNN"))
    (is (= (tb/read-sequence r {:chr "chr1" :start 1 :end 40}) nil))))
