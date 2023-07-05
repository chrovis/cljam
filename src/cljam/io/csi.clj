(ns cljam.io.csi
  "Basic I/O of CSI:Coordinate Sorted Index files."
  (:require [clojure.string :as cstr]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.util.chunk :as chunk]
            [cljam.io.util.bin :as util-bin])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException]
           [java.nio ByteBuffer ByteOrder]))

(def ^:const ^:private csi-magic "CSI\1")

(def ^:private ^:const default-vcf-csi-aux
  {:format 2, :col-seq 1, :col-beg 2, :col-end 0, :meta-char \#, :skip 0})

(def ^:private ^:const min-large-chunk-size 0x10000)

;;; 4byte x 7 (format,seq,beg,end,meta,skip,l_nm)
(def ^:private ^:const tabix-field-size (* 4 7))

(deftype CSI [n-ref min-shift depth bidx loffset aux]
  util-bin/IBinningIndex
  (get-chunks [_ ref-idx bins]
    (vec (mapcat (get bidx ref-idx) bins)))
  (get-min-offset [_ ref-idx beg]
    (if-let [min-offsets (rsubseq (get loffset ref-idx) <= beg)]
      (second (first min-offsets))
      0))
  (get-depth [_]
    depth)
  (get-min-shift [_]
    min-shift)
  (get-chr-names [_]
    (:chrs aux)))

(defn- parse-tabix-aux [^bytes ba]
  (when (<= tabix-field-size (alength ba))
    (let [bb (doto (ByteBuffer/wrap ba)
               (.order ByteOrder/LITTLE_ENDIAN))
          format' (.getInt bb)
          col-seq (.getInt bb)
          col-beg (.getInt bb)
          col-end (.getInt bb)
          meta-char (char (.getInt bb))
          skip (.getInt bb)
          l-nm (.getInt bb)]
      (when-not (= l-nm (.remaining bb))
        (throw (ex-info "l-nm does not match"
                        {:l-nm l-nm, :remaining (.remaining bb)})))
      {:format format', :col-seq col-seq, :col-beg col-beg, :col-end col-end,
       :meta-char meta-char, :skip skip,
       :chrs (cstr/split (String. ba (.position bb) (.remaining bb)) #"\00")})))

(defn- create-tabix-aux [aux]
  (let [contig-bytes (.getBytes (str (cstr/join (char 0) (:chrs aux)) (char 0)))
        bb (doto (ByteBuffer/wrap
                  (byte-array (+ (alength contig-bytes) tabix-field-size)))
             (.order ByteOrder/LITTLE_ENDIAN))]
    (.putInt bb (:format aux))
    (.putInt bb (:col-seq aux))
    (.putInt bb (:col-beg aux))
    (.putInt bb (:col-end aux))
    (.putInt bb (int (:meta-char aux)))
    (.putInt bb (:skip aux))
    (.putInt bb (alength contig-bytes))
    (.put bb contig-bytes)
    (.array bb)))

(defn- read-chunks!
  [rdr]
  (let [n-chunk (lsb/read-int rdr)]
    (->> #(let [beg (lsb/read-long rdr) end (lsb/read-long rdr)]
            (chunk/->Chunk beg end))
         (repeatedly n-chunk)
         vec)))

(defn- read-bin-index
  [rdr]
  (let [n-ref (lsb/read-int rdr)]
    (->> #(let [bin (lsb/read-int rdr)
                loffset (lsb/read-long rdr)
                chunks (read-chunks! rdr)]
            {:bin (long bin), :loffset loffset, :chunks chunks})
         (repeatedly n-ref)
         vec)))

(defn- read-index*
  ^CSI [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes csi-magic))
    (throw (IOException. "Invalid CSI file")))
  (let [min-shift (lsb/read-int rdr)
        depth (lsb/read-int rdr)
        l-aux (lsb/read-int rdr)
        aux (lsb/read-bytes rdr l-aux)
        tabix-aux (try (parse-tabix-aux aux) (catch Throwable _ nil))
        n-ref (lsb/read-int rdr)
        bins (vec (repeatedly n-ref #(read-bin-index rdr)))
        max-bin (util-bin/max-bin depth)
        bidx (mapv #(into {} (map (juxt :bin :chunks)) %) bins)
        loffset (mapv
                 #(into
                   (sorted-map)
                   (keep
                    (fn [{:keys [^long bin loffset]}]
                      (when (<= bin max-bin)
                        [(util-bin/bin-beg bin min-shift depth)
                         loffset]))) %)
                 bins)]
    (->CSI n-ref min-shift depth bidx loffset tabix-aux)))

(defn read-index
  "Reads a CSI file `f` and returns an instance of cljam.io.csi.CSI."
  ^CSI [f]
  (with-open [r (DataInputStream. (bgzf/bgzf-input-stream f))]
    (read-index* r)))

(defn- concatenate-offsets [offsets]
  (reduce
   (fn [res chunk']
     (if (= (:file-end (peek res)) (:file-beg chunk'))
       (assoc-in res [(dec (count res)) :file-end] (:file-end chunk'))
       (conj res chunk')))
   []
   (sort-by :file-beg offsets)))

(defn- small-chunks? [chunks]
  (< (- (bgzf/get-block-address (:file-end (last chunks)))
        (bgzf/get-block-address (:file-beg (first chunks))))
     min-large-chunk-size))

(defn- compress-bidx [^long depth bidx]
  (letfn [(f [^long level bidx]
            (reduce
             (fn [ret [bin offsets]]
               (let [parent-bin (util-bin/parent-bin bin)]
                 (if (and (= (util-bin/bin-level bin) level)
                          (contains? bidx parent-bin)
                          (small-chunks? offsets))
                   (update ret parent-bin concat offsets)
                   (assoc ret bin offsets))))
             {}
             (sort-by key bidx)))]
    (reduce (fn [b level] (f level b)) bidx (range depth 0 -1))))

(defn- calc-bidx [file-offsets ^long shift ^long depth]
  (->> file-offsets
       (group-by #(util-bin/reg->bin (:beg %) (:end %) shift depth))
       (compress-bidx depth)
       (map (fn [[bin offsets]]
              [bin (->> (concatenate-offsets offsets)
                        (mapv #(chunk/->Chunk (:file-beg %) (:file-end %))))]))
       (into (sorted-map))))

(defn- calc-loffsets [begs file-offsets]
  (->> begs
       (map (fn [^long beg]
              [beg (->> file-offsets
                        (drop-while #(< (long (:end %)) beg))
                        (map :file-beg)
                        first)]))
       (into (sorted-map))))

(defn- intmap->vec [xs]
  (reduce
   (fn [r [k v]]
     (if (contains? r k)
       (assoc r k v)
       (conj (into r (repeat (- (long k) (count r)) nil)) v)))
   []
   xs))

(defn offsets->index
  "Calculates loffsets and bidx
   from offsets {:file-beg :file-end :beg :end :chr :chr-index}.
   `variant-file-type` is one of `:vcf` or `:bcf`.
   If `variant-file-type` is `:vcf`, tabix-like aux data will be created."
  ^CSI [offsets shift depth
        {:keys [variant-file-type] :or {variant-file-type :bcf}}]
  (let [pseudo-bin (+ 2 (util-bin/max-bin depth))
        xs (->> offsets
                (partition-by :chr-index)
                (map
                 (fn [[{:keys [chr-index chr]} :as offsets]]
                   (let [b (calc-bidx offsets shift depth)
                         l (calc-loffsets
                            (into #{}
                                  (comp
                                   (filter (fn [^long x]
                                             (<= x (util-bin/max-bin depth))))
                                   (map #(util-bin/bin-beg % shift depth)))
                                  (keys b))
                            offsets)
                         pseudo-chunks [(chunk/->Chunk
                                         (:file-beg (first offsets))
                                         (:file-end (last offsets)))
                                        (chunk/->Chunk (count offsets) 0)]]
                     [chr-index
                      {:bidx (assoc b pseudo-bin pseudo-chunks),
                       :loffset l, :chr chr}])))
                (intmap->vec)
                ((if (= variant-file-type :vcf)
                   (partial filterv identity)
                   identity)))
        aux (when (= variant-file-type :vcf)
              (assoc default-vcf-csi-aux :chrs (mapv :chr xs)))]
    (->CSI (count xs) shift depth (mapv :bidx xs) (mapv :loffset xs) aux)))

(defn write-index
  "Writes CSI file from CSI data."
  [f ^CSI csi]
  (let [max-bin (util-bin/max-bin (.depth csi))]
    (with-open [w (DataOutputStream. (bgzf/bgzf-output-stream f))]
      (lsb/write-bytes w (.getBytes ^String csi-magic))
      (lsb/write-int w (.min-shift csi))
      (lsb/write-int w (.depth csi))
      (let [tabix-aux (some-> (.aux csi) create-tabix-aux)]
        (lsb/write-int w (count tabix-aux))
        (when tabix-aux
          (lsb/write-bytes w tabix-aux)))
      (lsb/write-int w (count (.bidx csi)))
      (doseq [[offsets loffset] (map vector (.bidx csi) (.loffset csi))]
        (lsb/write-int w (count offsets))
        (doseq [[bin chunks] offsets]
          (lsb/write-int w bin)
          (lsb/write-long
           w
           (if (<= (long bin) max-bin)
             (get loffset (util-bin/bin-beg bin (.min-shift csi) (.depth csi)))
             0))
          (lsb/write-int w (count chunks))
          (doseq [chunk' chunks]
            (lsb/write-long w (:beg chunk'))
            (lsb/write-long w (:end chunk'))))))))
