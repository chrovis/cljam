# Change Log

## [0.2.1] - 2017-04-06

### Added

* 2bit-encoded reference reader. [#31](https://github.com/chrovis/cljam/pull/31)
* Add functions for deduplicating paired-end BAM. [#32](https://github.com/chrovis/cljam/pull/32)
* Add :ignore-index option to cljam.fasta/reader. [#34](https://github.com/chrovis/cljam/pull/34)

### Changed

* Improve performance of FASTA indexing. [#30](https://github.com/chrovis/cljam/pull/30)
* Improve performance of BAM sequence encoding. [#35](https://github.com/chrovis/cljam/pull/35)

### Fixed

* Fix cider-refresh error. [#33](https://github.com/chrovis/cljam/pull/33)
* Fix StackOverflowError in parse-header of SAM/BAM files. [#38](https://github.com/chrovis/cljam/pull/38)

## [0.2.0] - 2017-02-20

### BREAKING

From this release, ranges are represented as one-based closed intervals. For example,

```clojure
{:chr "chr1", :start 1, :end 3}
```

represents the first three bases of chromosome 1.

### Added

* BED file reader/writer. [#20](https://github.com/chrovis/cljam/pull/20)
* Read unplaced reads in BAM. [#24](https://github.com/chrovis/cljam/pull/24)

### Changed

* Improve performance of reading BAM files. [#22](https://github.com/chrovis/cljam/pull/22)
* Replace candidate-message function with one in clj-sub-command.
* Improve performance of reading vcf file. [#29](https://github.com/chrovis/cljam/pull/29)

### Fixed

* Fix a bug in BAM random reader. [#21](https://github.com/chrovis/cljam/pull/21)
* Read multiple contigs in VCF meta info. [#23](https://github.com/chrovis/cljam/issues/23) [#25](https://github.com/chrovis/cljam/pull/25)
* Enable decompressUntilEOF of CompressorStreamFactory. [#26](https://github.com/chrovis/cljam/pull/26)
* Fix bugs in mpileup. [#27](https://github.com/chrovis/cljam/pull/27) [#28](https://github.com/chrovis/cljam/issues/28)

## [0.1.6] - 2017-01-06

* Sequential reading function for FASTA [#16](https://github.com/chrovis/cljam/pull/16)
* Common compressor stream [#17](https://github.com/chrovis/cljam/pull/17)
* Add encode/decode functions for SAM FLAG field [#19](https://github.com/chrovis/cljam/pull/19)
* Fix mpileup bug
* Improve project settings [#18](https://github.com/chrovis/cljam/pull/18)

## [0.1.5] - 2016-07-26

* Add FASTQ I/O
* Add VCF I/O
* Improve level analyzer
* Improve mpileup
* Improve performance of SAM/BAM conversion
* Fix bugs of sorter
* Fix small bugs

## [0.1.4] - 2016-02-02

* Add level analyzer of alignments
* Update dependencies except clojure 1.8.0
* Enable CI with JDK8
* Fix bugs of sorter
* Fix bugs of m-pileup
* Fix small bugs

## [0.1.3] - 2015-08-14

* Improve FASTA reading
* Improve normalization
* Improve pileup performance (also support concurrency)
* Enable `index` command to specify the number of threads
* Display sub-command candidates
* Fix bugs

## [0.1.2] - 2014-05-20

* Improve performance of pileup
* Fix small bugs
* Refactoring codes

## [0.1.1] - 2014-04-25

* Improve performance of BAM indexing
* Fix bugs of FASTA indexing
* Fix bugs of sequence dictionary creation
* Refactoring codes

## 0.1.0 - 2014-04-03

First release

[Unreleased]: https://github.com/chrovis/cljam/compare/0.2.1...HEAD
[0.2.1]: https://github.com/chrovis/cljam/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/chrovis/cljam/compare/0.1.6...0.2.0
[0.1.6]: https://github.com/chrovis/cljam/compare/0.1.5...0.1.6
[0.1.5]: https://github.com/chrovis/cljam/compare/0.1.4...0.1.5
[0.1.4]: https://github.com/chrovis/cljam/compare/0.1.3...0.1.4
[0.1.3]: https://github.com/chrovis/cljam/compare/0.1.2...0.1.3
[0.1.2]: https://github.com/chrovis/cljam/compare/0.1.1...0.1.2
[0.1.1]: https://github.com/chrovis/cljam/compare/0.1.0...0.1.1
