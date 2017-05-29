# cljam

A DNA Sequence Alignment/Map (SAM) library for Clojure. [[API Reference]][api-reference] [[Annotated Source]][annotated-source]

[![Clojars Project](https://img.shields.io/clojars/v/cljam.svg)](https://clojars.org/cljam)

[![Build Status](https://travis-ci.org/chrovis/cljam.svg?branch=master)](https://travis-ci.org/chrovis/cljam)

[![codecov](https://codecov.io/gh/chrovis/cljam/branch/master/graph/badge.svg)](https://codecov.io/gh/chrovis/cljam)

[![Dependency Status](https://www.versioneye.com/user/projects/55dc18598d9c4b0021000759/badge.svg?style=flat)](https://www.versioneye.com/user/projects/55dc18598d9c4b0021000759)

## Installation

cljam is available as a Maven artifact from [Clojars][clojars].

To use with Leiningen/Boot, add the following dependency.

```clojure
[cljam "0.3.1"]
```

To use with Maven, add the following dependency.

```xml
<dependency>
  <groupId>cljam</groupId>
  <artifactId>cljam</artifactId>
  <version>0.3.1</version>
</dependency>
```

## Breaking changes in 0.2.0

From 0.2.0 release, ranges are represented as one-based closed intervals. For example,

```clojure
{:chr "chr1", :start 1, :end 3}
```

represents the first three bases of chromosome 1.

## Getting started

To read a SAM/BAM format file,

```clojure
(require '[cljam.core :refer [reader]]
         '[cljam.io :as io])

;; Open a file
(with-open [r (reader "path/to/file.bam")]
  ;; Retrieve header
  (io/read-header r)
  ;; Retrieve alignments
  (doall (take 5 (io/read-alignments r)))
```

To create a sorted file,

```clojure
(require '[cljam.core :refer [reader writer]]
         '[cljam.sorter :as sorter])

(with-open [r (reader "path/to/file.bam")
            w (writer "path/to/sorted.bam")]
  ;; Sort by chromosomal coordinates
  (sorter/sort-by-pos r w))
```

To create a BAM index file,

```clojure
(require '[cljam.bam-indexer :as bai])

;; Create a new BAM index file
(bai/create-index "path/to/sorted.bam" "path/to/sorted.bam.bai")
```

To pileup,

```clojure
(require '[cljam.core :refer [reader]]
         '[cljam.pileup :as plp])

(with-open [r (reader "path/to/sorted.bam" :ignore-index false)]
  ;; Pileup "chr1" alignments
  (take 10 (plp/pileup r "chr1" nil)))
;; => (0 0 1 1 3 3 3 3 2 3)
```

Check https://chrovis.github.io/cljam for more information.

If you are Clojure beginner, read [Getting Started for Clojure Beginners](https://github.com/chrovis/cljam/wiki/Getting-Started-for-Clojure-Beginners).

## Command-line tool

cljam provides a command-line tool to use the features easily.

### Executable installation

Run `lein-bin` plugin and it creates standalone console executable into `target` directory.

```console
$ lein with-profile +1.8 bin
Created /path/to/cljam/target/cljam-0.3.1.jar
Created /path/to/cljam/target/cljam-0.3.1-standalone.jar
Creating standalone executable: /path/to/cljam/target/cljam
```

Copy the executable somewhere in your `$PATH`.

### Usage

All commands are displayed by `cljam -h`, and detailed help for each command are displayed by `cljam [cmd] -h`.

```console
$ cljam view -h
```

For example, to display contents of a SAM file including the header,

```console
$ cljam view --header path/to/file.sam
```

## Development

### Test

To run tests,

- `lein test` for basic tests,
- `lein test :slow` for slow tests with local resources,
- `lein test :remote` for tests with remote resources.

To get coverage

```console
$ lein cloverage
```

And open `target/coverage/index.html`.

### Generating document

cljam uses [Codox](https://github.com/weavejester/codox) for API reference and
[Marginalia](https://github.com/gdeer81/marginalia) for annotated source code.

```console
$ lein docs
```

generates these documents in `target/docs` and `target/literate` directories.

## Citing cljam

T. Takeuchi, A. Yamada, T. Aoki, and K. Nishimura. [cljam: a library for handling DNA sequence alignment/map (SAM) with parallel processing](http://dx.doi.org/10.1186/s13029-016-0058-6). Source Code for Biology and Medicine, Vol. 11, No. 1, pp. 1-4, 2016.

## Contributors

Sorted by first commit.

- Toshiki Takeuchi ([@totakke](https://github.com/totakke))
- Takashi Aoki ([@federkasten](https://github.com/federkasten))
- Atsuo Yamada ([@ayamada](https://github.com/ayamada))
- Jun Imura ([@alumi](https://github.com/alumi))

## License

Copyright 2013-2017 [Xcoo, Inc.][xcoo]

Licensed under the [Apache License, Version 2.0][apache-license-2.0].

[clojars]: https://clojars.org/cljam
[api-reference]: https://chrovis.github.io/cljam/docs
[annotated-source]: https://chrovis.github.io/cljam/literate
[xcoo]: https://xcoo.jp/
[apache-license-2.0]: http://www.apache.org/licenses/LICENSE-2.0.html
