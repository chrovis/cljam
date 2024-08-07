(ns cljam.io.cram
  "Alpha - subject to change. Provides functions for reading and writing a CRAM file."
  (:refer-clojure :exclude [indexed?])
  (:require [cljam.io.cram.core :as cram]
            [cljam.io.protocols :as protocols]
            [cljam.io.util :as io-util])
  (:import [cljam.io.cram.reader CRAMReader]
           [cljam.io.cram.writer CRAMWriter]))

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
  ([rdr]
   (protocols/read-alignments rdr))
  ([rdr region]
   (protocols/read-alignments rdr region)))

(defn indexed?
  "Returns true if the reader can be randomly accessed, false if not. Note this
  function immediately realizes a delayed index."
  [rdr]
  (protocols/indexed? rdr))

(defn writer
  "Creates a new CRAM writer that writes to a CRAM file f.

  The function also takes an optional argument `option`, which is a map that
  consists of:
    - reference: A string representing the path to the reference file, or
        a sequence reader that reads sequences from the reference file.
        This may be omitted only when the CRAM file to be read does not require
        a reference file.
    - create-index?: If true, creates a .crai index file in the course of CRAM
        file writing.
    - skip-sort-order-check?: When creating a CRAM index for the CRAM file,
        the CRAM writer, by default, checks if the header is declared as
        `SO:coordinate` and raises an error if not.
        If this option is set to true, the CRAM writer will skip the header check
        and create an index file regardless of the header declaration."
  (^CRAMWriter [f] (writer f {}))
  (^CRAMWriter [f option] (cram/writer f option)))

(defn write-header
  "Writes header to the CRAM file."
  [wtr header]
  (protocols/write-header wtr header))

(defn write-refs
  "Does nothing. This exists only for the sake of compatibility with other
  alignment writers."
  [wtr refs]
  (protocols/write-refs wtr refs))

(defn write-alignments
  "Writes alignments to the CRAM file."
  [wtr alns header]
  (protocols/write-alignments wtr alns header))
