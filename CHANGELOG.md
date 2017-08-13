# Change Log

## [Unreleased]

### Changed

* Refactor cljam.algo.level. [#93](https://github.com/chrovis/cljam/pull/93)
* Refactor BAI I/O. [#96](https://github.com/chrovis/cljam/pull/96)


### Fixed

* Update an example usage of depth in README. [#94](https://github.com/chrovis/cljam/pull/94)
* Use returned values of conj! in reg->bins\*. [#95](https://github.com/chrovis/cljam/pull/95)


## [0.4.1] - 2017-08-01

### Changed

* Improve chromosome normalization. [#89](https://github.com/chrovis/cljam/pull/89)
* Suppress log output in test. [#90](https://github.com/chrovis/cljam/pull/90)
* Extract depth algorithm and improve performance. [#91](https://github.com/chrovis/cljam/pull/91)

### Fixed

* Fix parser of optional fields in SAM/BAM. [#92](https://github.com/chrovis/cljam/pull/92)

## [0.4.0] - 2017-06-28

### BREAKING

Namespaces of most APIs are changed in this release.

* `cljam.io.***` - reader/writer functions of various formats such as SAM, VCF, and FASTA.
* `cljam.algo.***` - algorithms such as sort, indexing, and pileup.
* `cljam.util.***` - utilities such as chromosome name normalization.
* `cljam.tools.***` - tools such as command-line interface.

### Added

* Add protocols for I/O APIs. [#78](https://github.com/chrovis/cljam/pull/78)
* Add FASTAWriter and TwoBitWriter. [#79](https://github.com/chrovis/cljam/pull/79)
* Support SAM normalization. [#86](https://github.com/chrovis/cljam/pull/86)
* Add docstring. [#88](https://github.com/chrovis/cljam/pull/88)

### Changed

* Remove dependency on [raynes/fs](https://github.com/Raynes/fs). [#80](https://github.com/chrovis/cljam/pull/80)
* Improve sorter. [#81](https://github.com/chrovis/cljam/pull/81)
* Namespace refactoring. [#83](https://github.com/chrovis/cljam/pull/83)
* I/O API refactoring. [#84](https://github.com/chrovis/cljam/pull/84) [#85](https://github.com/chrovis/cljam/pull/85)
* Use [proton](https://github.com/xcoo/proton) instead of some utilities. [#87](https://github.com/chrovis/cljam/pull/87)

### Fixed

* Fix test of BCF writer. [#82](https://github.com/chrovis/cljam/pull/82)

## [0.3.1] - 2017-05-29

### Added

* Add BED reader/writer. [#73](https://github.com/chrovis/cljam/pull/73)

### Changed

* Change default start/end of bam reader. [#74](https://github.com/chrovis/cljam/pull/74)
* Extend cljam.core reader/writer functions. [#75](https://github.com/chrovis/cljam/pull/75)
* Setup automatic snapshots deployment. [#76](https://github.com/chrovis/cljam/pull/76)
* Improve performance of deep decoding of BAM files. [#77](https://github.com/chrovis/cljam/pull/77)

## [0.3.0] - 2017-05-08

### BREAKING

From this release, `cljam.vcf/read-variants` parses FORMAT, FILTER, INFO and samples columns of VCF.

```clojure
(require '[cljam.vcf :as vcf])

(with-open [rdr (vcf/reader "test-resources/vcf/test-v4_3.vcf")]
  (first (vcf/read-variants rdr)))
;;=> {:FORMAT (:GT :GQ :DP :HQ),
;;    :NA00001 {:DP 1, :GQ 48, :GT "0|0", :HQ (51 51)},
;;    :NA00002 {:DP 8, :GQ 48, :GT "1|0", :HQ (51 51)},
;;    :NA00003 {:DP 5, :GQ 43, :GT "1/1", :HQ (nil nil)},
;;    :alt ["A"],
;;    :chr "20",
;;    :filter (:PASS),
;;    :id "rs6054257",
;;    :info {:AF (0.5), :DB :exists, :DP 14, :H2 :exists, :NS 3},
;;    :pos 14370,
;;    :qual 29.0,
;;    :ref "G"}
```

Add `{:depth :vcf}` option to the second argument if string is preferred.

### Added

* Add a function to clone bam reader. [#39](https://github.com/chrovis/cljam/pull/39)
* Support coverage analysis with [cloverage](https://github.com/cloverage/cloverage). [#45](https://github.com/chrovis/cljam/pull/45)
	* Integration with [Codecov](https://codecov.io/gh/chrovis/cljam). [#46](https://github.com/chrovis/cljam/pull/46)
	* Reduce worktime of coverage. [#69](https://github.com/chrovis/cljam/pull/69)
* Add many tests. [#60](https://github.com/chrovis/cljam/pull/60)
* Add tests to check generated data correctness. [#61](https://github.com/chrovis/cljam/issues/61)
	* Examine result file of `dedupe` in test. [#63](https://github.com/chrovis/cljam/pull/63)
	* Examine result file of `create-dict` in test. [#64](https://github.com/chrovis/cljam/pull/64)
	* Examine result files of `cljam pileup` in test. [#65](https://github.com/chrovis/cljam/pull/65)
	* Examine result file of `cljam normalize` in test. [#66](https://github.com/chrovis/cljam/pull/66)
	* Examine result file of `create-mpileup`. [#67](https://github.com/chrovis/cljam/pull/67)
	* Examine result file of `cljam level` test. [#70](https://github.com/chrovis/cljam/pull/70)
* Add BCF reader/writer and utility functions for VCF. [#68](https://github.com/chrovis/cljam/pull/68)

### Changed

* Improves performance of pileup. [#41](https://github.com/chrovis/cljam/pull/41)
* Migrate from midje to clojure.test. [#44](https://github.com/chrovis/cljam/pull/44)
* Correct base qualities if reads are overlapped. [#47](https://github.com/chrovis/cljam/pull/47)
* Use [lein-binplus](https://github.com/BrunoBonacci/lein-binplus) instead of [lein-bin](https://github.com/Raynes/lein-bin). [#48](https://github.com/chrovis/cljam/pull/48)
* Add marks to private fn. [#50](https://github.com/chrovis/cljam/pull/50)
* Refine unused code for newline. [#51](https://github.com/chrovis/cljam/pull/51)
* Pileup without options. [#54](https://github.com/chrovis/cljam/pull/54)
* Refine `trim-chromosome-key`. [#55](https://github.com/chrovis/cljam/pull/55)
* Refine `fastq-char->phred-byte` / Add `phred-byte->fastq-char`. [#56](https://github.com/chrovis/cljam/pull/56)
* Separate test resources based on file types. [#71](https://github.com/chrovis/cljam/pull/71)
* Separate :slow and :heavy test-selectors. [#72](https://github.com/chrovis/cljam/pull/72)

### Fixed

* Fix for `create-mpileup`. [#52](https://github.com/chrovis/cljam/pull/52)
* Fix for `cljam level` command. [#57](https://github.com/chrovis/cljam/pull/57)

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

[Unreleased]: https://github.com/chrovis/cljam/compare/0.4.1...HEAD
[0.4.1]: https://github.com/chrovis/cljam/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/chrovis/cljam/compare/0.3.1...0.4.0
[0.3.1]: https://github.com/chrovis/cljam/compare/0.3.0...0.3.1
[0.3.0]: https://github.com/chrovis/cljam/compare/0.2.1...0.3.0
[0.2.1]: https://github.com/chrovis/cljam/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/chrovis/cljam/compare/0.1.6...0.2.0
[0.1.6]: https://github.com/chrovis/cljam/compare/0.1.5...0.1.6
[0.1.5]: https://github.com/chrovis/cljam/compare/0.1.4...0.1.5
[0.1.4]: https://github.com/chrovis/cljam/compare/0.1.3...0.1.4
[0.1.3]: https://github.com/chrovis/cljam/compare/0.1.2...0.1.3
[0.1.2]: https://github.com/chrovis/cljam/compare/0.1.1...0.1.2
[0.1.1]: https://github.com/chrovis/cljam/compare/0.1.0...0.1.1
