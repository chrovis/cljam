# cljam

A DNA Sequence Alignment/Map (SAM) library for Clojure. [API Reference](http://chrovis.github.io/cljam)

[![Build Status](https://travis-ci.org/chrovis/cljam.svg?branch=master)](https://travis-ci.org/chrovis/cljam)

## Installation

cljam is available as a Maven artifact from [Clojars][clojars].

To use with Leiningen, add the following dependency.

```clojure
[cljam "0.1.2"]
```

To use with Maven, add the following dependency.

```xml
<dependency>
  <groupId>cljam</groupId>
  <artifactId>cljam</artifactId>
  <version>0.1.2</version>
</dependency>
```

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
  (take 5 (io/read-alignments r {})))
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

(with-open [r (reader "path/to/sorted.bam")]
  ;; Pileup "chr1" alignments
  (take 10 (plp/pileup r "chr1")))
```

Check https://chrovis.github.io/cljam for more information.

## Command-line tool

cljam provides a command-line tool to use the features easily.

### Executable installation

Run `lein-bin` plugin and it creates standalone console executable into `target` directory.

```bash
$ lein bin
Created /path/to/cljam/target/cljam-0.1.2.jar
Created /path/to/cljam/target/cljam-0.1.2-standalone.jar
Creating standalone executable: /path/to/cljam/target/cljam
```

Copy the executable somewhere in your `$PATH`.

### Usage

All commands are displayed by `cljam -h`, and detailed help for each command are displayed by `cljam [cmd] -h`.

```bash
$ cljam view -h
```

For example, to display contents of a SAM file including the header,

```bash
$ cljam view --header path/to/file.sam
```

## Development

### Test

To run all basic tests,

```bash
$ lein midje
```

To run heavy tests which uses remote large-size files,

```bash
$ lein midje :filter heavy
```

### Generating document

cljam uses [Marginalia][marginalia] for generating documents.

```bash
$ lein marg -m
```

generates HTML documents in `docs` directory.

## Contributors

Sorted by first commit.

- Toshiki Takeuchi ([@totakke](https://github.com/totakke))
- Takashi Aoki ([@federkasten](https://github.com/federkasten))
- Atsuo Yamada ([@ayamada](https://github.com/ayamada))

## License

Copyright 2013 [Xcoo, Inc.][xcoo]

Licensed under the [Apache License, Version 2.0][apache-license-2.0].

[clojars]: https://clojars.org/cljam
[marginalia]: http://gdeer81.github.io/marginalia/
[xcoo]: http://www.xcoo.jp/
[apache-license-2.0]: http://www.apache.org/licenses/LICENSE-2.0.html
