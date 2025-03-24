(ns cljam.io.gff-test
  (:require [clojure.test :refer [deftest is are testing]]
            [clojure.string :as cstr]
            [clojure.java.io :as cio]
            [cljam.test-common :refer
             [with-before-after
              prepare-cache!
              clean-cache!
              not-throw?
              http-server
              temp-dir
              test-gff3-file]]
            [cljam.io.gff :as gff])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [cljam.io.gff GFFReader GFFWriter]))

(def ^:private ^String
  simple-gff
  (->> ["##gff-version 3"
        "ctg123	.	exon	1300	1500	.	+	.	ID=exon00001"
        "ctg123	.	exon	1050	1500	.	+	.	ID=exon00002"
        "ctg123	.	exon	3000	3902	.	+	.	ID=exon00003"
        "ctg123	.	exon	5000	5500	.	+	.	ID=exon00004"
        "ctg123	.	exon	7000	9000	.	+	.	ID=exon00005"]
       (cstr/join \newline)))

(def ^:private
  simple-edn
  [{:chr "ctg123", :source nil, :type "exon", :start 1300, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00001"}}
   {:chr "ctg123", :source nil, :type "exon", :start 1050, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00002"}}
   {:chr "ctg123", :source nil, :type "exon", :start 3000, :end 3902, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00003"}}
   {:chr "ctg123", :source nil, :type "exon", :start 5000, :end 5500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00004"}}
   {:chr "ctg123", :source nil, :type "exon", :start 7000, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00005"}}])

(def ^:private ^String
  nested-gff-1
  (->> ["##gff-version 3"
        "ctg123	.	mRNA	1300	9000	.	+	.	ID=mrna0001;Name=foobar"
        "ctg123	.	exon	1300	1500	.	+	.	ID=exon00001;Parent=mrna0001"
        "ctg123	.	exon	1050	1500	.	+	.	ID=exon00002;Parent=mrna0001"
        "ctg123	.	exon	3000	3902	.	+	.	ID=exon00003;Parent=mrna0001"
        "ctg123	.	exon	5000	5500	.	+	.	ID=exon00004;Parent=mrna0001"
        "ctg123	.	exon	7000	9000	.	+	.	ID=exon00005;Parent=mrna0001"]
       (cstr/join \newline)))

(def ^:private
  nested-edn-1
  [{:chr "ctg123", :source nil, :type "mRNA", :start 1300, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "mrna0001", :name "foobar"}}
   {:chr "ctg123", :source nil, :type "exon", :start 1300, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00001", :parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 1050, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00002", :parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 3000, :end 3902, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00003", :parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 5000, :end 5500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00004", :parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 7000, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00005", :parent ["mrna0001"]}}])

(def ^:private ^String
  nested-gff-2
  (->> ["##gff-version 3"
        "ctg123	.	operon	1300	15000	.	+	.	ID=operon001;Name=Operon"
        "ctg123	.	mRNA	1300	9000	.	+	.	ID=mrna0001;Parent=operon001;Name=foobar"
        "ctg123	.	exon	1300	1500	.	+	.	Parent=mrna0001"
        "ctg123	.	exon	1050	1500	.	+	.	Parent=mrna0001"
        "ctg123	.	exon	3000	3902	.	+	.	Parent=mrna0001"
        "ctg123	.	exon	5000	5500	.	+	.	Parent=mrna0001"
        "ctg123	.	exon	7000	9000	.	+	.	Parent=mrna0001"
        "ctg123	.	mRNA	10000	15000	.	+	.	ID=mrna0002;Parent=operon001;Name=baz"
        "ctg123	.	exon	10000	12000	.	+	.	Parent=mrna0002"
        "ctg123	.	exon	14000	15000	.	+	.	Parent=mrna0002"]
       (cstr/join \newline)))

(def ^:private
  nested-edn-2
  [{:chr "ctg123", :source nil, :type "operon", :start 1300, :end 15000, :score nil, :strand :forward, :phase nil, :attributes {:id "operon001", :name "Operon"}}
   {:chr "ctg123", :source nil, :type "mRNA", :start 1300, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "mrna0001", :parent ["operon001"], :name "foobar"}}
   {:chr "ctg123", :source nil, :type "exon", :start 1300, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 1050, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 3000, :end 3902, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 5000, :end 5500, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 7000, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0001"]}}
   {:chr "ctg123", :source nil, :type "mRNA", :start 10000, :end 15000, :score nil, :strand :forward, :phase nil, :attributes {:id "mrna0002", :parent ["operon001"], :name "baz"}}
   {:chr "ctg123", :source nil, :type "exon", :start 10000, :end 12000, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0002"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 14000, :end 15000, :score nil, :strand :forward, :phase nil, :attributes {:parent ["mrna0002"]}}])

(def ^:private ^String
  discontinuous-gff
  (->> ["##gff-version 3"
        "ctg123	example	match	26122	26126	.	+	.	ID=match001"
        "ctg123	example	match	26497	26869	.	+	.	ID=match001"
        "ctg123	example	match	27201	27325	.	+	.	ID=match001"
        "ctg123	example	match	27372	27433	.	+	.	ID=match001"
        "ctg123	example	match	27565	27565	.	+	.	ID=match001"]
       (cstr/join \newline)))

(def ^:private
  discontinuous-edn
  [{:chr "ctg123", :source "example", :type "match", :start 26122, :end 26126, :score nil, :strand :forward, :phase nil, :attributes {:id "match001"}}
   {:chr "ctg123", :source "example", :type "match", :start 26497, :end 26869, :score nil, :strand :forward, :phase nil, :attributes {:id "match001"}}
   {:chr "ctg123", :source "example", :type "match", :start 27201, :end 27325, :score nil, :strand :forward, :phase nil, :attributes {:id "match001"}}
   {:chr "ctg123", :source "example", :type "match", :start 27372, :end 27433, :score nil, :strand :forward, :phase nil, :attributes {:id "match001"}}
   {:chr "ctg123", :source "example", :type "match", :start 27565, :end 27565, :score nil, :strand :forward, :phase nil, :attributes {:id "match001"}}])

(def ^:private ^String
  example-gene-gff
  (->> ["##gff-version 3.2.1"
        "##sequence-region ctg123 1 1497228"
        "ctg123	.	gene	1000	9000	.	+	.	ID=gene00001;Name=EDEN"
        "ctg123	.	TF_binding_site	1000	1012	.	+	.	ID=tfbs00001;Parent=gene00001"
        "ctg123	.	mRNA	1050	9000	.	+	.	ID=mRNA00001;Parent=gene00001;Name=EDEN.1"
        "ctg123	.	mRNA	1050	9000	.	+	.	ID=mRNA00002;Parent=gene00001;Name=EDEN.2"
        "ctg123	.	mRNA	1300	9000	.	+	.	ID=mRNA00003;Parent=gene00001;Name=EDEN.3"
        "ctg123	.	exon	1300	1500	.	+	.	ID=exon00001;Parent=mRNA00003"
        "ctg123	.	exon	1050	1500	.	+	.	ID=exon00002;Parent=mRNA00001,mRNA00002"
        "ctg123	.	exon	3000	3902	.	+	.	ID=exon00003;Parent=mRNA00001,mRNA00003"
        "ctg123	.	exon	5000	5500	.	+	.	ID=exon00004;Parent=mRNA00001,mRNA00002,mRNA00003"
        "ctg123	.	exon	7000	9000	.	+	.	ID=exon00005;Parent=mRNA00001,mRNA00002,mRNA00003"
        "ctg123	.	CDS	1201	1500	.	+	0	ID=cds00001;Parent=mRNA00001;Name=edenprotein.1"
        "ctg123	.	CDS	3000	3902	.	+	0	ID=cds00001;Parent=mRNA00001;Name=edenprotein.1"
        "ctg123	.	CDS	5000	5500	.	+	0	ID=cds00001;Parent=mRNA00001;Name=edenprotein.1"
        "ctg123	.	CDS	7000	7600	.	+	0	ID=cds00001;Parent=mRNA00001;Name=edenprotein.1"
        "ctg123	.	CDS	1201	1500	.	+	0	ID=cds00002;Parent=mRNA00002;Name=edenprotein.2"
        "ctg123	.	CDS	5000	5500	.	+	0	ID=cds00002;Parent=mRNA00002;Name=edenprotein.2"
        "ctg123	.	CDS	7000	7600	.	+	0	ID=cds00002;Parent=mRNA00002;Name=edenprotein.2"
        "ctg123	.	CDS	3301	3902	.	+	0	ID=cds00003;Parent=mRNA00003;Name=edenprotein.3"
        "ctg123	.	CDS	5000	5500	.	+	1	ID=cds00003;Parent=mRNA00003;Name=edenprotein.3"
        "ctg123	.	CDS	7000	7600	.	+	1	ID=cds00003;Parent=mRNA00003;Name=edenprotein.3"
        "ctg123	.	CDS	3391	3902	.	+	0	ID=cds00004;Parent=mRNA00003;Name=edenprotein.4"
        "ctg123	.	CDS	5000	5500	.	+	1	ID=cds00004;Parent=mRNA00003;Name=edenprotein.4"
        "ctg123	.	CDS	7000	7600	.	+	1	ID=cds00004;Parent=mRNA00003;Name=edenprotein.4"]
       (cstr/join \newline)))

(def ^:private
  example-gene-edn
  [{:chr "ctg123", :source nil, :type "gene", :start 1000, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "gene00001", :name "EDEN"}}
   {:chr "ctg123", :source nil, :type "TF_binding_site", :start 1000, :end 1012, :score nil, :strand :forward, :phase nil, :attributes {:id "tfbs00001", :parent ["gene00001"]}}
   {:chr "ctg123", :source nil, :type "mRNA", :start 1050, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "mRNA00001", :parent ["gene00001"], :name "EDEN.1"}}
   {:chr "ctg123", :source nil, :type "mRNA", :start 1050, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "mRNA00002", :parent ["gene00001"], :name "EDEN.2"}}
   {:chr "ctg123", :source nil, :type "mRNA", :start 1300, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "mRNA00003", :parent ["gene00001"], :name "EDEN.3"}}
   {:chr "ctg123", :source nil, :type "exon", :start 1300, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00001", :parent ["mRNA00003"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 1050, :end 1500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00002", :parent ["mRNA00001" "mRNA00002"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 3000, :end 3902, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00003", :parent ["mRNA00001" "mRNA00003"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 5000, :end 5500, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00004", :parent ["mRNA00001" "mRNA00002" "mRNA00003"]}}
   {:chr "ctg123", :source nil, :type "exon", :start 7000, :end 9000, :score nil, :strand :forward, :phase nil, :attributes {:id "exon00005", :parent ["mRNA00001" "mRNA00002" "mRNA00003"]}}
   {:chr "ctg123", :source nil, :type "CDS", :start 1201, :end 1500, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00001", :parent ["mRNA00001"], :name "edenprotein.1"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 3000, :end 3902, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00001", :parent ["mRNA00001"], :name "edenprotein.1"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 5000, :end 5500, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00001", :parent ["mRNA00001"], :name "edenprotein.1"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 7000, :end 7600, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00001", :parent ["mRNA00001"], :name "edenprotein.1"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 1201, :end 1500, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00002", :parent ["mRNA00002"], :name "edenprotein.2"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 5000, :end 5500, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00002", :parent ["mRNA00002"], :name "edenprotein.2"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 7000, :end 7600, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00002", :parent ["mRNA00002"], :name "edenprotein.2"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 3301, :end 3902, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00003", :parent ["mRNA00003"], :name "edenprotein.3"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 5000, :end 5500, :score nil, :strand :forward, :phase 1, :attributes {:id "cds00003", :parent ["mRNA00003"], :name "edenprotein.3"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 7000, :end 7600, :score nil, :strand :forward, :phase 1, :attributes {:id "cds00003", :parent ["mRNA00003"], :name "edenprotein.3"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 3391, :end 3902, :score nil, :strand :forward, :phase 0, :attributes {:id "cds00004", :parent ["mRNA00003"], :name "edenprotein.4"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 5000, :end 5500, :score nil, :strand :forward, :phase 1, :attributes {:id "cds00004", :parent ["mRNA00003"], :name "edenprotein.4"}}
   {:chr "ctg123", :source nil, :type "CDS", :start 7000, :end 7600, :score nil, :strand :forward, :phase 1, :attributes {:id "cds00004", :parent ["mRNA00003"], :name "edenprotein.4"}}])

(def ^:private ^String
  circular-gff
  (->> ["##gff-version 3.2.1"
        "# organism Enterobacteria phage f1"
        "# Note Bacteriophage f1, complete genome."
        "J02448	GenBank	region	1	6407	.	+	.	ID=J02448;Name=J02448;Is_circular=true"
        "J02448	GenBank	CDS	6006	7238	.	+	0	ID=geneII;Name=II;Note=protein II"]
       (cstr/join \newline)))

(def ^:private
  circular-edn
  [{:chr "J02448", :source "GenBank", :type "region", :start 1, :end 6407, :score nil, :strand :forward, :phase nil, :attributes {:id "J02448", :name "J02448", :circular? true}}
   {:chr "J02448", :source "GenBank", :type "CDS", :start 6006, :end 7238, :score nil, :strand :forward, :phase 0, :attributes {:id "geneII", :name "II", :note ["protein II"]}}])

(def ^:private ^String
  gap-gff
  (->> ["##gff-version 3.2.1"
        "chr3	.	Match	1	23	.	.	.	ID=Match1;Target=EST23 1 21;Gap=M8 D3 M6 I1 M6"
        "ctg123	.	nucleotide_to_protein	100	129	.	+	.	ID=match008;Target=p101 1 10;Gap=M3 I1 M2 D1 M4"]
       (cstr/join \newline)))

(def ^:private
  gap-edn
  [{:chr "chr3", :source nil, :type "Match", :start 1, :end 23, :score nil, :strand nil, :phase nil,
    :attributes {:id "Match1", :target {:chr "EST23", :start 1, :end 21}, :gap [[\M 8] [\D 3] [\M 6] [\I 1] [\M 6]]}}
   {:chr "ctg123", :source nil, :type "nucleotide_to_protein", :start 100, :end 129, :score nil, :strand :forward, :phase nil,
    :attributes {:id "match008", :target {:chr "p101", :start 1, :end 10}, :gap [[\M 3] [\I 1] [\M 2] [\D 1] [\M 4]]}}])

(def ^:private ^String
  alignment-gff
  (->> ["##gff-version 3.2.1"
        "ctg123	.	cDNA_match	1050	9000	6.2e-45	+	.	ID=match00001;Target=cdna0123 12 2964;Gap=M451 D3499 M501 D1499 M2001"]
       (cstr/join \newline)))

(def ^:private
  alignment-edn
  [{:chr "ctg123", :source nil, :type "cDNA_match", :start 1050, :end 9000, :score 6.2e-45, :strand :forward, :phase nil,
    :attributes {:id "match00001", :target {:chr "cdna0123", :start 12, :end 2964}, :gap [[\M 451] [\D 3499] [\M 501] [\D 1499] [\M 2001]]}}])

(def ^:private ^String
  alignment-multiple-gff
  (->> ["##gff-version 3.2.1"
        "ctg123	.	cDNA_match	1050	1500	5.8e-42	+	.	ID=match00001;Target=cdna0123 12 462"
        "ctg123	.	cDNA_match	5000	5500	8.1e-43	+	.	ID=match00001;Target=cdna0123 463 963"
        "ctg123	.	cDNA_match	7000	9000	1.4e-40	+	.	ID=match00001;Target=cdna0123 964 2964"]
       (cstr/join \newline)))

(def ^:private
  alignment-multiple-edn
  [{:chr "ctg123", :source nil, :type "cDNA_match", :start 1050, :end 1500, :score 5.8e-42, :strand :forward, :phase nil, :attributes {:id "match00001", :target {:chr "cdna0123", :start 12, :end 462}}}
   {:chr "ctg123", :source nil, :type "cDNA_match", :start 5000, :end 5500, :score 8.1e-43, :strand :forward, :phase nil, :attributes {:id "match00001", :target {:chr "cdna0123", :start 463, :end 963}}}
   {:chr "ctg123", :source nil, :type "cDNA_match", :start 7000, :end 9000, :score 1.4e-40, :strand :forward, :phase nil, :attributes {:id "match00001", :target {:chr "cdna0123", :start 964, :end 2964}}}])

(def ^:private ^String
  alignment-reverse-gff
  (->> ["##gff-version 3.2.1"
        "ctg123	.	EST_match	1200	3200	2.2e-30	+	.	ID=match00002;Target=mjm1123.5 5 506;Gap=M301 D1499 M201"
        "ctg123	.	EST_match	7000	9000	7.4e-32	-	.	ID=match00003;Target=mjm1123.3 1 502;Gap=M101 D1499 M401"]
       (cstr/join \newline)))

(def ^:private
  alignment-reverse-edn
  [{:chr "ctg123", :source nil, :type "EST_match", :start 1200, :end 3200, :score 2.2e-30, :strand :forward, :phase nil,
    :attributes {:id "match00002", :target {:chr "mjm1123.5", :start 5, :end 506}, :gap [[\M 301] [\D 1499] [\M 201]]}}
   {:chr "ctg123", :source nil, :type "EST_match", :start 7000, :end 9000, :score 7.4e-32, :strand :reverse, :phase nil,
    :attributes {:id "match00003", :target {:chr "mjm1123.3", :start 1, :end 502}, :gap [[\M 101] [\D 1499] [\M 401]]}}])

(def ^:private ^String
  alignment-group-gff
  (->> ["##gff-version 3.2.1"
        "ctg123	.	cDNA_match	1200	9000	.	.	.	ID=cDNA00001"
        "ctg123	.	match_part	1200	3200	2.2e-30	+	.	ID=match00002;Parent=cDNA00001;Target=mjm1123.5 5 506;Gap=M301 D1499 M201"
        "ctg123	.	match_part	7000	9000	7.4e-32	-	.	ID=match00003;Parent=cDNA00001;Target=mjm1123.3 1 502;Gap=M101 D1499 M401"]
       (cstr/join \newline)))

(def ^:private
  alignment-group-edn
  [{:chr "ctg123", :source nil, :type "cDNA_match", :start 1200, :end 9000, :score nil, :strand nil, :phase nil,
    :attributes {:id "cDNA00001"}}
   {:chr "ctg123", :source nil, :type "match_part", :start 1200, :end 3200, :score 2.2e-30, :strand :forward, :phase nil,
    :attributes {:id "match00002", :parent ["cDNA00001"], :target {:chr "mjm1123.5", :start 5, :end 506}, :gap [[\M 301] [\D 1499] [\M 201]]}}
   {:chr "ctg123", :source nil, :type "match_part", :start 7000, :end 9000, :score 7.4e-32, :strand :reverse, :phase nil,
    :attributes {:id "match00003", :parent ["cDNA00001"], :target {:chr "mjm1123.3", :start 1, :end 502}, :gap [[\M 101] [\D 1499] [\M 401]]}}])

(def ^:private ^String
  encoding-gff
  (->> ["##gff-version 3.2"
        "ch r;1	sour =ce	ty &p,e	1	10	9.0	?	.	."
        "chr%253B1	sour%253Dce	ty%2526p%252Ce	1	10	.	+	.	Target=Foo%20Bar 1 10 +;Dbxref=EMBL:AA816246,NCBI_gi:10727410;Foo=Bar%2C,Baz "
        " !\"#$%25&'%09()*+,-./%0A0123456789:;<=>?@[\\]^_`{|}~	.	type	1	10	.	.	.	ID= !\"#$%25%26'%09()*+%2C-./%0A0123456789:%3B<%3D>?@[\\]^_`{|}~;Target=%20!\"#$%25%26'%09()*+%2C-./%0A0123456789:%3B<%3D>?@[\\]^_`{|}~ 1 10 -"]
       (cstr/join \newline)))

(def ^:private
  encoding-edn
  [{:chr "ch r;1", :source "sour =ce", :type "ty &p,e", :start 1, :end 10, :score 9.0, :strand :unknown, :phase nil, :attributes {}}
   {:chr "chr%3B1", :source "sour%3Dce", :type "ty%26p%2Ce", :start 1, :end 10, :score nil, :strand :forward, :phase nil,
    :attributes {:target {:chr "Foo Bar", :start 1, :end 10, :strand :forward}, :db-xref [{:db-tag "EMBL", :id "AA816246"}, {:db-tag "NCBI_gi", :id "10727410"}], "Foo" ["Bar," "Baz "]}}
   {:chr " !\"#$%&'\t()*+,-./\n0123456789:;<=>?@[\\]^_`{|}~", :source nil, :type "type", :start 1, :end 10, :score nil, :strand nil, :phase nil,
    :attributes {:id " !\"#$%&'\t()*+,-./\n0123456789:;<=>?@[\\]^_`{|}~",
                 :target {:chr " !\"#$%&'\t()*+,-./\n0123456789:;<=>?@[\\]^_`{|}~", :start 1, :end 10, :strand :reverse}}}])

(def ^:private
  example-edn
  [{:chr "ctg123", :source nil, :type "gene", :start 1000, :end 9000, :score nil, :strand :forward, :phase nil,
    :attributes {:id "gene00001", :name "EDEN"}}
   {:chr "ctg123", :source nil, :type "TF_binding_site", :start 1000, :end 1012, :score nil, :strand :forward, :phase nil,
    :attributes {:id "tfbs00001", :parent ["gene00001"]}}
   {:chr "ctg123", :source nil, :type "mRNA", :start 1050, :end 9000, :score nil, :strand :forward, :phase nil,
    :attributes {:id "mRNA00001", :parent ["gene00001"], :name "EDEN.1"}}
   {:chr "ctg123", :source nil, :type "five_prime_UTR", :start 1050, :end 1200, :score nil, :strand :forward, :phase nil,
    :attributes {:parent ["mRNA00001"]}}
   {:chr "ctg123", :source nil, :type "CDS", :start 1201, :end 1500, :score nil, :strand :forward, :phase 0,
    :attributes {:id "cds00001", :parent ["mRNA00001"]}}
   {:chr "ctg123", :source nil, :type "CDS", :start 3000, :end 3902, :score nil, :strand :forward, :phase 0,
    :attributes {:id "cds00001", :parent ["mRNA00001"]}}
   {:chr "ctg123", :source nil, :type "CDS", :start 5000, :end 5500, :score nil, :strand :forward, :phase 0,
    :attributes {:id "cds00001", :parent ["mRNA00001"]}}
   {:chr "ctg123", :source nil, :type "CDS", :start 7000, :end 7600, :score nil, :strand :forward, :phase 0,
    :attributes {:id "cds00001", :parent ["mRNA00001"]}}
   {:chr "ctg123", :source nil, :type "three_prime_UTR", :start 7601, :end 9000, :score nil, :strand :forward, :phase nil,
    :attributes {:parent ["mRNA00001"]}}
   {:chr "ctg123", :source nil, :type "cDNA_match", :start 1050, :end 1500, :score 5.8e-42, :strand :forward, :phase nil,
    :attributes {:id "match00001", :target {:chr "cdna0123", :start 12, :end 462}}}
   {:chr "ctg123", :source nil, :type "cDNA_match", :start 5000, :end 5500, :score 8.1e-43, :strand :forward, :phase nil,
    :attributes {:id "match00001", :target {:chr "cdna0123", :start 463, :end 963}}}
   {:chr "ctg123", :source nil, :type "cDNA_match", :start 7000, :end 9000, :score 1.4e-40, :strand :forward, :phase nil,
    :attributes {:id "match00001", :target {:chr "cdna0123", :start 964, :end 2964}}}])

(deftest reader
  (with-open [bais (ByteArrayInputStream. (.getBytes simple-gff))
              r (gff/reader bais)]
    (is (instance? GFFReader r))
    (is (= {:version 3, :major-revision nil, :minor-revision nil}
           (gff/version r))))
  (with-open [bais (ByteArrayInputStream. (.getBytes alignment-gff))
              r (gff/reader bais)]
    (is (instance? GFFReader r))
    (is (= {:version 3, :major-revision 2, :minor-revision 1}
           (gff/version r))))
  (with-open [bais (ByteArrayInputStream. (.getBytes "##"))]
    (is (thrown? Exception (gff/reader bais))))
  (with-open [bais (ByteArrayInputStream. (.getBytes "##"))]
    (is (= {:url nil, :version-directive "##"}
           (try (gff/reader bais) (catch Exception e (ex-data e))))))
  (with-open [bais (ByteArrayInputStream. (.getBytes "##gff-version 2"))]
    (is (thrown? Exception (gff/reader bais))))
  (with-open [bais (ByteArrayInputStream. (.getBytes "##gff-version 2"))]
    (is (= {:url nil, :version 2, :major-revision nil, :minor-revision nil}
           (try (gff/reader bais) (catch Exception e (ex-data e))))))
  (with-open [bais (ByteArrayInputStream. (.getBytes "##gff-version 3\nctg%41123\t.\t.\t1\t10\t.\t.\t.\t."))
              r (gff/reader bais)]
    (is (thrown-with-msg?
         Exception
         #"Found an invalid character encoding while decoding GFF3 file"
         (gff/read-features r))))
  (with-open [bais (ByteArrayInputStream. (.getBytes "##gff-version 3\nctg%41123\t.\t.\t1\t10\t.\t.\t.\t."))
              r (gff/reader bais)]
    (is (= {:input "ctg%41123", :invalid-string "%41"}
           (try (gff/read-features r) (catch Exception e (ex-data e)))))))

(deftest read-features
  (are [?str ?edn]
       (= ?edn
          (with-open [bais (ByteArrayInputStream. (.getBytes ?str))
                      r (gff/reader bais)]
            (doall (gff/read-features r))))
    simple-gff simple-edn
    nested-gff-1 nested-edn-1
    nested-gff-2 nested-edn-2
    discontinuous-gff discontinuous-edn
    example-gene-gff example-gene-edn
    circular-gff circular-edn
    gap-gff gap-edn
    alignment-gff alignment-edn
    alignment-multiple-gff alignment-multiple-edn
    alignment-reverse-gff alignment-reverse-edn
    alignment-group-gff alignment-group-edn
    encoding-gff encoding-edn))

(deftest read-features-from-file
  (with-open [r (gff/reader test-gff3-file)]
    (is (= example-edn
           (gff/read-features r)))))

(deftest writer
  (with-open [baos (ByteArrayOutputStream.)
              w (gff/writer baos)]
    (is (instance? GFFWriter w)))
  (with-open [baos (ByteArrayOutputStream.)
              w (gff/writer baos {:version 3})]
    (is (instance? GFFWriter w)))
  (with-open [baos (ByteArrayOutputStream.)
              w (gff/writer baos {:version 3, :major-revision 2, :minor-revision 1})]
    (is (instance? GFFWriter w)))
  (with-open [baos (ByteArrayOutputStream.)]
    (is (thrown? Exception (gff/writer baos {:version 2}))))
  (with-open [baos (ByteArrayOutputStream.)]
    (is (= {:url nil, :version 2}
           (try (gff/writer baos {:version 2}) (catch Exception e (ex-data e))))))
  (with-open [baos (ByteArrayOutputStream.)]
    (with-open [w (gff/writer baos {:version 3, :encoding :gzip})]
      (gff/write-features w simple-edn))
    (let [ba (.toByteArray baos)]
      ;; GZIP file header
      (is (= (unchecked-byte 0x1f) (aget ba 0)))
      (is (= (unchecked-byte 0x8b) (aget ba 1))))))

(deftest write-features
  (are [?edn ?str]
      ;; ignore directives and comment lines
       (= (cstr/replace ?str #"(?<=\n)#.*?\n" "")
          (with-open [bais (ByteArrayInputStream. (.getBytes ?str))
                      baos (ByteArrayOutputStream.)]
            (let [v (with-open [r (gff/reader bais)]
                      (gff/version r))]
              (with-open [w (gff/writer baos v)]
                (gff/write-features w ?edn)))
            (str baos)))
    simple-edn simple-gff
    nested-edn-1 nested-gff-1
    nested-edn-2 nested-gff-2
    discontinuous-edn discontinuous-gff
    example-gene-edn example-gene-gff
    circular-edn circular-gff
    gap-edn gap-gff
    alignment-edn alignment-gff
    alignment-multiple-edn alignment-multiple-gff
    alignment-reverse-edn alignment-reverse-gff
    alignment-group-edn alignment-group-gff
    encoding-edn encoding-gff))

(deftest write-features-to-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [f (cio/file temp-dir "gff-write.gff3")]
      (is (not-throw? (with-open [w (gff/writer f)]
                        (gff/write-features w simple-edn))))
      (is (.isFile f)))
    (let [f (cio/file temp-dir "gff-write.gff3.gz")]
      (is (not-throw? (with-open [w (gff/writer f)]
                        (gff/write-features w simple-edn))))
      (is (.isFile f)))
    (let [f (cio/file temp-dir "gff-write.gff3.bz2")]
      (is (not-throw? (with-open [w (gff/writer f)]
                        (gff/write-features w simple-edn))))
      (is (.isFile f)))))

(deftest source-type-test
  (testing "reader"
    (with-open [server (http-server)]
      (are [?x] (= example-edn
                   (with-open [r (gff/reader ?x)]
                     (doall (gff/read-features r))))
        test-gff3-file
        (cio/file test-gff3-file)
        (cio/as-url (cio/file test-gff3-file))
        (cio/as-url (str (:uri server) "/gff3/example.gff3")))))
  (testing "writer"
    (let [tmp-gff3-file (cio/file temp-dir "gff3-source-type-writer.gff3")]
      (are [?x] (with-before-after {:before (prepare-cache!)
                                    :after (clean-cache!)}
                  (with-open [w (gff/writer ?x)]
                    (not-throw? (gff/write-features w example-edn))))
        (.getCanonicalPath tmp-gff3-file)
        tmp-gff3-file
        (cio/as-url tmp-gff3-file)))))
