(ns cljam.t-twobit
  (:use [midje.sweet])
  (:require [cljam.twobit :as tb]))

(fact
 "TwoBit reference"
 (with-open [r (tb/reader "./test-resources/test.2bit")]
   (tb/read-sequence r {:chr "ref"}) => "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
   (tb/read-sequence r {:chr "ref2"}) => "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
   (tb/read-sequence r {:chr "ref2" :mask? true}) => "aggttttataaaacaattaagtctacagagcaactacgcg"
   (for [i (range 1 45) j (range i 46)]
     (tb/read-sequence r {:chr "ref" :start i :end j}))
   => (for [i (range 1 45) j (range i 46)]
        (subs "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT" (dec i) j))
   (tb/read-sequence r {:chr "ref" :start 1 :end 4}) => "AGCA"
   (tb/read-sequence r {:chr "ref" :start 0 :end 4}) => "NAGCA"
   (tb/read-sequence r {:chr "ref" :start 41 :end 50}) => "GCCATNNNNN"
   (tb/read-sequence r {:chr "ref" :start 1 :end 45}) => "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
   (tb/read-sequence r {:chr "ref2" :start 1 :end 40}) => "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG"
   (tb/read-sequence r {:chr "ref2" :start 1 :end 40 :mask? true}) => "aggttttataaaacaattaagtctacagagcaactacgcg"
   (tb/read-sequence r {:chr "chr1" :start 1 :end 40}) => nil))

(fact
 "TwoBit reference with N"
 (with-open [r (tb/reader "./test-resources/test-n.2bit")]
   (tb/read-sequence r {:chr "ref"}) => "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
   (tb/read-sequence r {:chr "ref2"}) => "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
   (tb/read-sequence r {:chr "ref2" :mask? true}) => "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
   (for [i (range 1 45) j (range i 46)]
     (tb/read-sequence r {:chr "ref" :start i :end j}))
   => (for [i (range 1 45) j (range i 46)]
        (subs "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT" (dec i) j))
   (tb/read-sequence r {:chr "ref" :start 1 :end 4}) => "NNNN"
   (tb/read-sequence r {:chr "ref" :start 0 :end 4}) => "NNNNN"
   (tb/read-sequence r {:chr "ref" :start 41 :end 50}) => "NCCATNNNNN"
   (tb/read-sequence r {:chr "ref" :start 1 :end 45}) => "NNNNNGTTAGATAAGATAGCNNTGCTAGTAGGCAGTCNNNNCCAT"
   (tb/read-sequence r {:chr "ref2" :start 1 :end 40}) => "AGNNNTTATAAAACAATTANNNCTACAGAGCAACTANNNN"
   (tb/read-sequence r {:chr "ref2" :start 1 :end 40 :mask? true}) => "agNNNttataaaacaattaNNNctacagagcaactaNNNN"
   (tb/read-sequence r {:chr "chr1" :start 1 :end 40}) => nil))
