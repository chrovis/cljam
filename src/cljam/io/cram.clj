(ns cljam.io.cram
  "Alpha - subject to change. Provides functions for reading from a CRAM file."
  (:require [cljam.io.cram.core :as cram]
            [cljam.io.protocols :as protocols]
            [cljam.io.util :as io-util])
  (:import [cljam.io.cram.reader CRAMReader]))

(defn reader
  "Creates a CRAM reader depending on the argument f: If f is a file or a string
  that representing the path to a CRAM file, returns a new reader that reads
  that CRAM file. If f is a CRAM reader, creates and returns a cloned CRAM reader
  from it.

  The function also takes an optional argument `option`, which is a map that
  consists of:
    - reference: A string representing the path to the reference file, or
                 a sequence reader that reads sequences from the reference file.
                 This may be omitted only when the CRAM file to be read does not
                 require a reference file."
  (^CRAMReader [f] (reader f {}))
  (^CRAMReader [f option]
   (if (io-util/cram-reader? f)
     (cram/clone-reader f)
     (cram/reader f option))))

(defn read-header
  "Returns the header of the CRAM file."
  [rdr]
  (protocols/read-header rdr))

(defn read-refs
  "Returns the references of the CRAM file."
  [rdr]
  (protocols/read-refs rdr))

(defn read-alignments
  "Reads all the alignments from the CRAM file and returns them as a lazy sequence
  of record maps."
  [rdr]
  (protocols/read-alignments rdr))
