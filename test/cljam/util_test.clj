(ns cljam.util-test
  "Tests for cljam.util."
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.util :as util])
  (:import [java.io File]
           [java.net URL]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest with-temp-dir-test
  (testing "name with given prefixes"
    (let [x "foo", y "bar"]
      (util/with-temp-dir [d x, e y]
        (let [has-prefix? (fn [name' prefix]
                            (and (> (count name') (count prefix))
                                 (cstr/starts-with? name' prefix)))]
          (is (has-prefix? (.getName ^File d) x))
          (is (has-prefix? (.getName ^File e) y))))))
  (testing "no temp items outside of the macro's scope"
    (let [[d e] (util/with-temp-dir [d "foo", e "bar"] [d e])
          deleted? (fn [dir] (not (.exists (cio/file dir))))]
      (is (deleted? d))
      (is (deleted? e))))
  (testing "users can delete temp directories before entering a finally clause"
    (try
      (util/with-temp-dir [d "foo", e "bar"]
        (cio/delete-file d true)
        (cio/delete-file e true))
      (catch Exception e (is false e))
      (finally (is true))))
  (testing "automatically deletes subdirectories created by users"
    (let [sub-dirs (util/with-temp-dir [d "foo"]
                     (let [sub-dirs [(cio/file d "bar") (cio/file d "qux")]]
                       (run! (fn [^File dir] (.mkdir dir)) sub-dirs)
                       sub-dirs))
          deleted? (fn [dir] (not (.exists (cio/file dir))))]
      (is (every? deleted? sub-dirs))))
  (testing "keeps original nodes if symbolic links are deleted"
    (let [orig (cio/file temp-dir "orig")]
      (with-before-after {:before (prepare-cache!)
                          :after (clean-cache!)}
        (.mkdir orig)
        (util/with-temp-dir [d "foo"]
          (Files/createSymbolicLink
           (.toPath (cio/file d "orig-link"))
           (.toPath orig)
           (make-array FileAttribute 0)))
        (is (.exists orig))))))

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
