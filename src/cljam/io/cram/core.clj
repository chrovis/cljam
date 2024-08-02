(ns cljam.io.cram.core
  (:require [cljam.io.crai :as crai]
            [cljam.io.cram.reader :as reader]
            [cljam.io.cram.seq-resolver :as resolver]
            [cljam.io.cram.writer :as writer]
            [cljam.io.sam.util.refs :as util.refs]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.util :as util]
            [clojure.java.io :as cio]
            [clojure.string :as str])
  (:import [cljam.io.cram.reader CRAMReader]
           [cljam.io.cram.writer CRAMWriter]
           [java.io DataOutputStream FileNotFoundException]
           [java.net URL]
           [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]))

(defn- qname-generator [^URL url]
  (let [prefix (-> (.getPath url)
                   (str/replace #"^.*/" "")
                   (str/replace #"[^!-?A-~]+" "_"))
        len (inc (count prefix))]
    (fn [i]
      (let [digits (str (inc (long i)))
            len' (+ len (count digits))]
        (cond-> (str prefix \: digits)
          (> len' 254)
          (subs (- len' 254) len'))))))

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
        qname-gen (qname-generator url)
        header (volatile! nil)
        refs (delay (vec (util.refs/make-refs @header)))
        offset (volatile! nil)
        idx (delay
              (try
                (crai/read-index (str f ".crai") @refs)
                (catch FileNotFoundException _
                  nil)))
        rdr (reader/->CRAMReader url ch bb header refs offset idx seq-resolver qname-gen)]
    (reader/read-file-definition rdr)
    (vreset! header (reader/read-header rdr))
    (vreset! offset (.position ch))
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
        rdr' (reader/->CRAMReader url ch bb
                                  (delay @(.-header rdr))
                                  (delay @(.-refs rdr))
                                  (delay @(.-offset rdr))
                                  (delay @(.-index rdr))
                                  seq-resolver
                                  (.-qname-generator rdr))]
    (reader/read-file-definition rdr')
    (reader/skip-container rdr')
    rdr'))

(defn writer
  "Creates a new CRAM writer that writes to a CRAM file f.

  Takes an option map as the second argument. See the docstring for `cljam.io.cram/writer`
  for more details on the option map."
  ^CRAMWriter [f {:keys [reference create-index?] :as opts}]
  (let [file (cio/file f)
        url (cio/as-url file)
        url' (str url)
        file-id (subs url' 0 (min 20 (count url')))
        out (DataOutputStream. (cio/output-stream file))
        index-writer (when create-index?
                       (crai/writer (util/as-url (str url' ".crai"))))
        seq-resolver (some-> reference resolver/seq-resolver resolver/cached-resolver)
        wtr (writer/->CRAMWriter url out seq-resolver index-writer opts)]
    (writer/write-file-definition wtr file-id)
    wtr))
