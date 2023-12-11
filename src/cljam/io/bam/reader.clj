(ns cljam.io.bam.reader
  "Reader of BAM file format."
  (:require [cljam.io.protocols :as protocols]
            [cljam.io.sam.util.refs :as refs]
            [cljam.io.sam.util.header :as header]
            [cljam.io.bam-index.core :as bai]
            [cljam.io.bam.decoder :as decoder]
            [cljam.io.util.lsb.data-io :as lsb])
  (:import [java.io Closeable FileNotFoundException]
           [cljam.io.bam.decoder BAMRawBlock]
           [bgzf4j BGZFInputStream]))

(declare read-blocks-sequentially*
         read-blocks-randomly*)

;; BAMReader
;; ---------

(deftype BAMReader [url header refs reader data-reader index-delay start-pos]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this]
    (.url this))
  (read [this]
    (protocols/read this {}))
  (read [this region]
    (protocols/read-alignments this region))
  (indexed? [this]
    (try
      @(.index-delay this)
      true
      (catch FileNotFoundException _
        false)))
  protocols/IAlignmentReader
  (read-header [this]
    (.header this))
  (read-refs [this]
    (.refs this))
  (read-alignments [this]
    (protocols/read-alignments this {}))
  (read-alignments [this {:keys [chr start end]
                          :or {start 1
                               end Long/MAX_VALUE}}]
    (let [decoder (partial decoder/decode-alignment (.refs this))]
      (if (nil? chr)
        (read-blocks-sequentially* this 64 decoder)
        (read-blocks-randomly* this chr start end 64 decoder))))
  (read-blocks [this]
    (protocols/read-blocks this {} {}))
  (read-blocks [this region]
    (protocols/read-blocks this region {}))
  (read-blocks [this
                {:keys [chr start end]
                 :or {start 1
                      end Long/MAX_VALUE}}
                {:keys [mode chunk-size]
                 :or {mode :normal
                      chunk-size 64}}]
    (let [decoder (if (fn? mode)
                    mode
                    (case mode
                      :normal decoder/raw-block
                      :region decoder/decode-region-block
                      :coordinate decoder/decode-coordinate-block
                      :queryname decoder/decode-queryname-block
                      :pointer decoder/decode-pointer-block))]
      (if (nil? chr)
        (read-blocks-sequentially* this chunk-size decoder)
        (read-blocks-randomly* this chr start end chunk-size decoder))))
  protocols/IRegionReader
  (read-in-region [this region]
    (protocols/read-in-region this region {}))
  (read-in-region [this region _]
    (protocols/read-alignments this region)))

(defn- read-a-block!
  "Reads a single alignment block from a reader."
  ^"[B" [rdr]
  (let [block-size (lsb/read-int rdr)]
    (lsb/read-bytes rdr block-size)))

(defn- read-to-finish
  "Reads alignment blocks until reaches to the finish pointer or EOF."
  ([^BAMReader rdr ^long chunk-size]
   (let [r ^BGZFInputStream (.-reader rdr)
         dr (.-data-reader rdr)]
     (letfn [(step [^long start]
               (lazy-seq
                (let [buf (chunk-buffer chunk-size)]
                  (loop [i chunk-size, start start]
                    (if (> i 0)
                      (if (zero? (.available r))
                        (chunk-cons (chunk buf) nil)
                        (let [data (read-a-block! dr)
                              curr (.getFilePointer r)]
                          (chunk-append buf (BAMRawBlock. data start curr))
                          (recur (dec i) curr)))
                      (chunk-cons (chunk buf) (step start)))))))]
       (step (.getFilePointer r)))))
  ([^BAMReader rdr
    ^long start
    ^long finish
    ^long chunk-size]
   (let [r ^BGZFInputStream (.reader rdr)
         dr (.data-reader rdr)]
     (letfn [(step [^long start]
               (lazy-seq
                (let [buf (chunk-buffer chunk-size)]
                  (loop [i chunk-size, start start]
                    (.seek r start)
                    (if (> i 0)
                      (if (and (< start finish)
                               (> (.available r) 0))
                        (let [data (read-a-block! dr)
                              curr (.getFilePointer r)]
                          (chunk-append buf (BAMRawBlock. data start curr))
                          (recur (dec i) curr))
                        (chunk-cons (chunk buf) nil))
                      (chunk-cons (chunk buf) (step start)))))))]
       (.seek r start)
       (step start)))))

(defn- read-blocks-sequentially*
  "Reads blocks sequentially from current position.
  Returns an eduction of decoded blocks."
  [^BAMReader rdr chunk-size decoder]
  (eduction
   (keep decoder)
   (read-to-finish rdr chunk-size)))

(defn- read-blocks-randomly*
  "Reads blocks crossing the given range using BAM index.
  Returns an eduction of decoded blocks."
  [^BAMReader rdr chr start end chunk-size decoder]
  (let [bai @(.index-delay rdr)]
    (if (= chr "*")
      (do (.seek ^BGZFInputStream (.reader rdr) (ffirst (bai/get-unplaced-spans bai)))
          (read-blocks-sequentially* rdr chunk-size decoder))
      (let [refs (.refs rdr)]
        (->> (bai/get-spans bai (refs/ref-id refs chr) start end)
             (eduction
              (comp
               (mapcat
                (fn [[begin finish]]
                  (read-to-finish rdr begin finish chunk-size)))
               (keep #(decoder % start end)))))))))

(defn load-headers
  "Reads header section of BAM file and returns it as a map."
  [rdr]
  (let [header (header/parse-header (lsb/read-string rdr (lsb/read-int rdr)))
        n-ref (int (lsb/read-int rdr))
        refs (loop [i n-ref, ret []]
               (if (zero? i)
                 ret
                 (let [l-name (int (lsb/read-int rdr))
                       name'  (lsb/read-string rdr l-name)
                       l-ref  (lsb/read-int rdr)]
                   (recur (dec i)
                          (conj ret {:name (subs name' 0 (dec l-name))
                                     :len  l-ref})))))]
    {:header header
     :refs refs}))
