# cljam

A DNA Sequence Alignment/Map (SAM) library for Clojure.

## Install command-line tool

Run `lein-bin` plugin and it creates standalone console executable into `target`.

    $ lein bin
    > Creating standalone executable: ... target/cljam

Copy the executable somewhere in your `$PATH`.

## Usage

All commands are displayed by `cljam -h`, and detailed help for a command are displayed by `cljam [cmd] -h`.

e.g.

    $ cljam view --header test/resources/test.sam

## Test

Run all test.

    $ lein midje

## Benchmark

Use criterium.

e.g.

    cljam.pileup> (use 'criterium.core)
    cljam.pileup> (with-progress-reporting
                   (bench
                    (pileup (cljam.bam/slurp "test/resources/test.sorted.bam"))))

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
