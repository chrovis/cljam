(ns cljam.util-test
  "Tests for cljam.util."
  (:require [clojure.java.io :as cio]
            [clojure.test :refer :all]
            [cljam.util :as util])
  (:import [java.net URL]))

(deftest ubyte
  (are [?n ?expected] (= (util/ubyte ?n) ?expected)
    0    (byte 0)
    127  (byte 127)
    128  (byte -128)
    255  (byte -1))
  (are [?n] (thrown? AssertionError (util/ubyte ?n))
    -1
    256))

(deftest as-url-test
  (are [x] (instance? URL (util/as-url x))
    (URL. "http://example.com/path/to/file.bam")
    (URL. "file:/path/to/file.bam")

    (cio/file "/path/to/file.bam")
    (cio/file "path/to/file.bam")

    "http://example.com/path/to/file.bam"
    "file:/path/to/file.bam"
    "/path/to/file.bam"
    "path/to/file.bam"
    "")
  (is (nil? (util/as-url nil))))

(deftest basename-test
  (are [x e] (= (util/basename x) e)
    (URL. "http://example.com/path/to/file.bam") "file"
    (URL. "file:/path/to/file.bam")              "file"

    (cio/file "/path/to/file.bam") "file"
    (cio/file "path/to/file.bam")  "file"

    "http://example.com/path/to/file.bam" "file"
    "file:/path/to/file.bam"              "file"
    "/path/to/file.bam"                   "file"
    "path/to/file.bam"                    "file"

    (URL. "file:/path/to/file.sorted.bam") "file.sorted"

    ""  nil
    nil nil))
