# Change Log

## [Unreleased]

### Added

* Add content-based file type detection. [#171](https://github.com/chrovis/cljam/pull/171)
* Add support for random reading of bgzipped FASTA files. [#174](https://github.com/chrovis/cljam/pull/174)

### Fixed

* Fix I/O of variants with empty info/individual fields. [#167](https://github.com/chrovis/cljam/pull/167)
* Fix bugs in BCF I/O. [#168](https://github.com/chrovis/cljam/pull/168)
* Ignore undeclared keys in INFO and FORMAT when writing BCF files. [#169](https://github.com/chrovis/cljam/pull/169)
* Provide a default file format version for BCF. [#170](https://github.com/chrovis/cljam/pull/170)
* Fix nonintentional paths. [#172](https://github.com/chrovis/cljam/pull/172)
* Fix decoding of long CIGAR ops in BAM. [#173](https://github.com/chrovis/cljam/pull/173)

## [0.7.2] - 2019-06-04

### Added

* Add utility functions for manipulating genotypes. [#162](https://github.com/chrovis/cljam/pull/162)
* Add support for breakends in VCF. [#163](https://github.com/chrovis/cljam/pull/163)
* Add a function to inspect an ALT allele of VCF. [#164](https://github.com/chrovis/cljam/pull/164)
* Normalize variants in a VCF file. [#166](https://github.com/chrovis/cljam/pull/166)

### Changed

* Improve pileup by chunking alignments. [#160](https://github.com/chrovis/cljam/pull/160)
* Improve pileup more. [#165](https://github.com/chrovis/cljam/pull/165)

### Fixed

* Skip INFO value parsing if it's empty. [#161](https://github.com/chrovis/cljam/pull/161)

## [0.7.1] - 2019-04-01

### Fixed

* Remove useless nil entries in variants. [#156](https://github.com/chrovis/cljam/pull/156)
* Fix ByteBuffer/CharBuffer methods incompatibility. [#159](https://github.com/chrovis/cljam/pull/159)

## [0.7.0] - 2018-11-21

### BREAKING

* Strand representation is changed to keyword (`:forward`, `:reverse`).
* Chromosome name normalization is decoupled from BED i/o.
* Pileup module is entirely rewritten. See [#140](https://github.com/chrovis/cljam/pull/140) for more information.

### Added

* Support extra fields for VCF/BCF meta info. [#135](https://github.com/chrovis/cljam/pull/135)
* Character escaping support for string fields in VCF meta info. [#138](https://github.com/chrovis/cljam/pull/138)
* GFF3 I/O. [#143](https://github.com/chrovis/cljam/pull/143)
* Add basic reader/writer support for WIG. [#145](https://github.com/chrovis/cljam/pull/145)
* Add a reader for bigWig format. [#149](https://github.com/chrovis/cljam/pull/149)

### Changed

* Rewrite pileup module. [#140](https://github.com/chrovis/cljam/pull/140)
* Use :forward/:reverse/nil as values for the key ':strand'. [#144](https://github.com/chrovis/cljam/pull/144)
* Improve performance of TwoBitReader. [#153](https://github.com/chrovis/cljam/pull/153)
* Remove normalization of chromosome names from BED reader. [#154](https://github.com/chrovis/cljam/pull/154)

### Fixed

* Fix options of cljam.algo.pileup/pileup. [#146](https://github.com/chrovis/cljam/pull/146)
* Fix a bug in `cljam.algo.pileup/pileup`. [#148](https://github.com/chrovis/cljam/pull/148)
* Fix some fields of cljam.io.pileup LocusPile and PileupBase. [#151](https://github.com/chrovis/cljam/pull/151)

## [0.6.0] - 2018-06-22

### BREAKING

* N padding for out-of-range bases is **not** appended. See [#120 comment](https://github.com/chrovis/cljam/pull/120#issuecomment-343369370) and [#121](https://github.com/chrovis/cljam/pull/121) for more information.
* `cljam.io.protocols/{reader,writer}-path` were renamed to `cljam.io.protocols/{reader,writer}-url`. Their return values are `java.net.URL`.

### Added

* Add some extensions for FASTA format. [#124](https://github.com/chrovis/cljam/pull/124)
* Add many util functions for SAM/BAM files & split cljam.io.sam.util. [#128](https://github.com/chrovis/cljam/pull/128)
* Add FASTQ=>FASTA, SAM=>FASTQ conversions. [#129](https://github.com/chrovis/cljam/pull/129)
* Add some BED manipulation APIs. [#131](https://github.com/chrovis/cljam/pull/131)
* Support more source types. [#132](https://github.com/chrovis/cljam/pull/132)

### Changed

* Remove N padding for out-of-range bases in read-sequence. [#121](https://github.com/chrovis/cljam/pull/121)
* Upgrade dependencies. [#134](https://github.com/chrovis/cljam/pull/134)

### Fixed

* Fix some serialization functions. [#122](https://github.com/chrovis/cljam/pull/122)
* Fix parsing SAM header values containing ':'. [#123](https://github.com/chrovis/cljam/pull/123)
* Fix wrong padding of last bases in 2bit writer. [#127](https://github.com/chrovis/cljam/pull/127)
* Fix a bug in merging bed regions. [#130](https://github.com/chrovis/cljam/pull/130)
* Fix exec binary on Java 9+. [#133](https://github.com/chrovis/cljam/pull/133)

## [0.5.1] - 2017-11-10

### Added

* Set up benchmarks. [#108](https://github.com/chrovis/cljam/pull/108)
* Add `read-indices` to `ISequenceReader` protocol. [#109](https://github.com/chrovis/cljam/pull/109)
* Extract region utils to `cljam.util.region` and add some functions. [#110](https://github.com/chrovis/cljam/pull/110)
* Add region option to view command. [#112](https://github.com/chrovis/cljam/pull/112)
* Add FASTA-TwoBit converter. [#117](https://github.com/chrovis/cljam/pull/117)

### Changed

* Make convert parallelism controllable. [#113](https://github.com/chrovis/cljam/pull/113)
* Java 9 compatibility. [#114](https://github.com/chrovis/cljam/pull/114)
* Move coverage phase to build stages. [#116](https://github.com/chrovis/cljam/pull/116)
* Low-memory sequence I/O. [#118](https://github.com/chrovis/cljam/pull/118)
* Improve performance of FASTA random reading. [#120](https://github.com/chrovis/cljam/pull/120)

### Fixed

* Fix StackOverflowError in `cljam.io.protocols/read-in-region`. [#107](https://github.com/chrovis/cljam/pull/107)
* Fix parsing comma-separated number in region. [#111](https://github.com/chrovis/cljam/pull/111)
* Don't refer `clojure.core/indexed?` in `cljam.io`. [#115](https://github.com/chrovis/cljam/pull/115)
* Fix a bug that 2bit reader incorrectly reads small n. [#119](https://github.com/chrovis/cljam/pull/119)

## [0.5.0] - 2017-09-05

### BREAKING

* `:ignore-index` option of `cljam.io.sam/bam-reader` and `cljam.io.sequence/fasta-reader` is removed.
* `:depth` option of `cljam.io.sam/read-alignments` is removed. It returns `SAMAlignment` only.
* SAM/BAM reading functions return `Eduction` instances instead of lazy sequences.

### Added

* Add a logging configuration for cli. [#99](https://github.com/chrovis/cljam/pull/99)
* Add an ordering function for chromosome name. [#101](https://github.com/chrovis/cljam/pull/101)
* Add utilities for whole-genome coordinate. [#103](https://github.com/chrovis/cljam/pull/103)

### Changed

* Refactor cljam.algo.level. [#93](https://github.com/chrovis/cljam/pull/93)
* Refactor BAI I/O. [#96](https://github.com/chrovis/cljam/pull/96)
* Reorganize BAM I/O. [#97](https://github.com/chrovis/cljam/pull/97)
* Tweak the normalization function for chromosome name. [#98](https://github.com/chrovis/cljam/pull/98)
* Make index reading of FASTA reader delayed. [#100](https://github.com/chrovis/cljam/pull/100)
* Improve sequence readers. [#104](https://github.com/chrovis/cljam/pull/104)
* Rename test namespaces to conventional names. [#105](https://github.com/chrovis/cljam/pull/105)
* Upgrade dependencies. [#106](https://github.com/chrovis/cljam/pull/106)

### Fixed

* Update an example usage of depth in README. [#94](https://github.com/chrovis/cljam/pull/94)
* Use returned values of conj! in reg->bins\*. [#95](https://github.com/chrovis/cljam/pull/95)
* Use lazy pmap for lazy-depth. [#102](https://github.com/chrovis/cljam/pull/102)

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

[Unreleased]: https://github.com/chrovis/cljam/compare/0.7.2...HEAD
[0.7.2]: https://github.com/chrovis/cljam/compare/0.7.1...0.7.2
[0.7.1]: https://github.com/chrovis/cljam/compare/0.7.0...0.7.1
[0.7.0]: https://github.com/chrovis/cljam/compare/0.6.0...0.7.0
[0.6.0]: https://github.com/chrovis/cljam/compare/0.5.1...0.6.0
[0.5.1]: https://github.com/chrovis/cljam/compare/0.5.0...0.5.1
[0.5.0]: https://github.com/chrovis/cljam/compare/0.4.1...0.5.0
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
