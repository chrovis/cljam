# cljam

A DNA Sequence Alignment/Map (SAM) library for Clojure.

## Usage

You have to install [clj-sub-command][clj-sub-command] to your local repository.

    $ git clone https://github.com/totakke/clj-sub-command.git
    $ cd clj-sub-command
    $ lein install

Use commandline tools on leiningen. For example,

    $ lein run view --header test/resources/test.sam

## Test

Add the following to `.lein/profiles.clj`, and install lein-midje plugin.

    {:user {:plugins [[lein-midje "3.0.1"]]}}

Run all test.

    $ lein midje

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.

[clj-sub-command]: https://github.com/totakke/clj-sub-command
