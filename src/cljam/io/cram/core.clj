(ns cljam.io.cram.core
  (:require [cljam.io.cram.seq-resolver :as resolver]
            [cljam.io.cram.reader :as reader.core]
            [cljam.io.sam.util.refs :as util.refs]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.util :as util]
            [clojure.java.io :as cio])
  (:import [cljam.io.cram.reader CRAMReader]
           [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]))

(defn reader
  "Creates a new CRAM reader that reads a CRAM file f.

  Takes an option map as the second argument. An option map consists of:
    - reference: a string representing the path to a reference file"
  ^CRAMReader [f {:keys [reference]}]
  (let [file (cio/file f)
        url (util/as-url (.getAbsolutePath file))
        ch (FileChannel/open (.toPath file)
                             (into-array OpenOption [StandardOpenOption/READ]))
        bb (bb/allocate-lsb-byte-buffer 256)
        seq-resolver (some-> reference resolver/seq-resolver)
        header (volatile! nil)
        refs (delay (util.refs/make-refs @header))
        rdr (reader.core/->CRAMReader url ch bb header refs seq-resolver)]
    (reader.core/read-file-definition rdr)
    (vreset! header (reader.core/read-header rdr))
    rdr))

(defn clone-reader
  "Creates a cloned CRAM reader based on the given CRAM reader."
  ^CRAMReader [^CRAMReader rdr]
  (let [url (.-url rdr)
        file (cio/as-file url)
        ch (FileChannel/open (.toPath file)
                             (into-array OpenOption [StandardOpenOption/READ]))
        bb (bb/allocate-lsb-byte-buffer 256)
        seq-resolver (some-> (.-seq-resolver rdr) resolver/clone-seq-resolver)
        rdr' (reader.core/->CRAMReader url ch bb
                                       (delay @(.-header rdr))
                                       (delay @(.-refs rdr))
                                       seq-resolver)]
    (reader.core/read-file-definition rdr')
    (reader.core/skip-container rdr')
    rdr'))
