# cljam

A DNA Sequence Alignment/Map (SAM) library for Clojure.

## Features

Current cljam supports the following features.

SAM/BAM:

| Format | Read | Write | Normalize | Sort | Pielup |
| ------ | ---: | ----: | --------: | ---: | -----: |
| SAM    |  Yes |   Yes |       Yes |  Yes |    Yes |
| BAM    |  Yes |   Yes |       Yes |  Yes |    Yes |

BAM index (.bai):

| Format     | Read | Create |
| ---------- | ---: | -----: |
| BAM index  |  Yes |    Yes |

FASTA:

| Format | Read | Write | Create dict |
| ------ | ---: | ----: | ----------: |
| FASTA  |  Yes |    No |         Yes |

FASTA index (.fai):

| Format      | Read | Create |
| ----------- | ---: | -----: |
| FASTA index |   No |    Yes |

FASTA sequence dictionary (.dict):

| Format                    | Read | Create |
| ------------------------- | ---: | -----: |
| FASTA sequence dictionary |   No |    Yes |

tabix:

| Format | Read | Create |
| ------ | ---: | -----: |
| tabix  |  Yes |     No |

## Command-line tool

cljam provides a command-line tool to use the features easily.

### Installation

Run `lein-bin` plugin and it creates standalone console executable into `target` directory.

    $ lein bin
    > Creating standalone executable: .../target/cljam

Copy the executable somewhere in your `$PATH`.

### Usage

All commands are displayed by `cljam -h`, and detailed help for a command are displayed by `cljam [cmd] -h`.

e.g.

    $ cljam view --header test/resources/test.sam

## Test

To run all basic tests,

    $ lein midje

To run heavy tests which uses remote large-size files,

    $ lein midje :filter heavy

## Benchmark

Use criterium.

e.g.

    cljam.pileup> (use 'criterium.core)
    cljam.pileup> (with-progress-reporting
                   (bench
                    (pileup (cljam.bam/slurp "test/resources/test.sorted.bam"))))

## License

Copyright © 2013 Xcoo, Inc.

Distributed under the Eclipse Public License, the same as Clojure.
