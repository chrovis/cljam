(ns cljam.io.csi
  "Reader of a CSI format file."
  (:require [clojure.string :as cstr]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.util.chunk :as chunk]
            [cljam.io.util.bin :as util-bin])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException]))

(def ^:private default-vcf-csi-aux
  {:format 2 :col-seq 1 :col-beg 2 :col-end 0 :meta-char \# :skip 0})

(def min-large-chunk-size 0x10000)

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
    min-shift))

(def ^:const ^:private csi-magic "CSI\1")

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
            (hash-map
             :bin bin
             :loffset loffset
             :chunks chunks))
         (repeatedly n-ref)
         vec)))

(defn- read-index*
  [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes csi-magic))
    (throw (IOException. "Invalid CSI file")))
  (let [min-shift (lsb/read-int rdr)
        depth (lsb/read-int rdr)
        l-aux (lsb/read-int rdr)
        aux (lsb/read-bytes rdr l-aux)
        n-ref (lsb/read-int rdr)
        bins (vec (repeatedly n-ref #(read-bin-index rdr)))
        max-bin (util-bin/max-bin depth)
        bidx (->> bins
                  (map-indexed (fn [index bin]
                                 [index
                                  (into {} (comp (map (juxt :bin :chunks))
                                                 (filter #(<= (first %)
                                                              max-bin)))
                                        bin)]))
                  (into (sorted-map)))
        loffset (->> bins
                     (map-indexed
                      (fn [index bin]
                        [index
                         (into (sorted-map)
                               (comp (map (juxt :bin :loffset))
                                     (filter #(<= (first %) max-bin))
                                     (map (fn [[bin loffset]]
                                            [(util-bin/bin-beg bin
                                                               min-shift depth)
                                             loffset])))
                               bin)]))
                     (into (sorted-map)))]
    (->CSI n-ref min-shift depth bidx loffset aux)))

(defn read-index
  [f]
  (with-open [r (DataInputStream. (bgzf/bgzf-input-stream f))]
    (read-index* r)))

(defn- concatenate-offsets [offsets]
  (reduce (fn [res chunk]
            (if (= (:file-end (first res)) (:file-beg chunk))
              (cons (assoc (first res) :file-beg (:file-beg (first res))
                           :file-end (:file-end chunk))
                    (next res))
              (cons chunk res)))
          nil
          offsets))

(defn- compress-bidx [depth bidx]
  (let [target-bins (filter #(= (util-bin/bin-level %) depth) (keys bidx))]
    (->> target-bins
         (map (fn [bin]
                (let [offsets (get bidx bin)
                      parent-bin (bit-shift-right (dec bin) 3)]
                  (if (and (< (- (bit-shift-right (:file-end (last offsets))
                                                  16)
                                 (bit-shift-right (:file-beg (first offsets))
                                                  16))
                              min-large-chunk-size)
                           (get bidx parent-bin))
                    [parent-bin offsets]
                    [bin offsets]))))
         (concat (apply dissoc bidx target-bins))
         (group-by first)
         (map (fn [[bin offset-array]]
                [bin (->> (map second offset-array)
                          (apply concat)
                          (sort-by :file-beg))]))
         (into {}))))

(defn- calc-bidx [file-offsets shift depth]
  (->> file-offsets
       (map #(assoc %
                    :bin (util-bin/reg->bin (:beg %)
                                            (:end %) shift depth)))
       (group-by :bin)
       ((apply comp (for [i (range depth)]
                      #(compress-bidx (inc i) %))))
       (map (fn [[bin offsets]]
              [bin (->> (concatenate-offsets offsets)
                        (map #(chunk/->Chunk (:file-beg %) (:file-end %)))
                        reverse)]))
       (into (sorted-map))))

(defn- calc-loffsets [begs file-offsets]
  (->> begs
       (map (fn [beg]
              [beg (->> (drop-while #(< (:end %) beg) file-offsets)
                        (map :file-beg)
                        first)]))
       (into (sorted-map))))

(defn- create-vcf-csi-aux-data [contigs]
  (let [contig-bytes (.getBytes (str (cstr/join (char 0) contigs) (char 0)))]
    (byte-array
     (concat (->> (assoc default-vcf-csi-aux
                         :l-nm (count contig-bytes))
                  ((juxt :format :col-seq :col-beg
                         :col-end :meta-char :skip :l-nm))
                  (map (fn [n]
                         (map #(bit-shift-right (int n) %)
                              (range 0 32 8))))
                  (apply concat))
             contig-bytes))))

(defn offsets->index
  "Calculates loffsets and bidx
   from offsets {:file-beg :file-end :beg :end :chr :chr-index }.
   variant-file-type is :vcf or :bcf.
   If variant-file-type is :vcf, aux data like hts_lib will be created."
  [offsets shift depth
   {:keys [variant-file-type names] :or {variant-file-type :bcf}}]
  (let [chr-offsets (merge (->> (range (count names))
                                (map #(vector % []))
                                (into {}))
                           (group-by :chr-index offsets))
        bidx (->> chr-offsets
                  (map (fn [[chr-index offsets]]
                         [chr-index (calc-bidx offsets shift depth)]))
                  (into (sorted-map)))
        loffsets (->> chr-offsets
                      (map (fn [[chr-index offsets]]
                             [chr-index (calc-loffsets
                                         (set (map #(util-bin/bin-beg % shift
                                                                      depth)
                                                   (keys (get bidx
                                                              chr-index))))
                                         offsets)]))
                      (into (sorted-map)))
        aux (when (= variant-file-type :vcf)
              (create-vcf-csi-aux-data names))]
    (->CSI (count bidx) shift depth bidx loffsets aux)))

(defn write-index
  "Writes CSI file from CSI data."
  [f ^CSI csi]
  (with-open [w (DataOutputStream. (bgzf/bgzf-output-stream f))]
    (lsb/write-bytes w (.getBytes ^String csi-magic))
    (lsb/write-int w (.min-shift csi))
    (lsb/write-int w (.depth csi))
    (let [aux (.aux csi)]
      (lsb/write-int w (count aux))
      (when aux (lsb/write-bytes w aux)))
    (lsb/write-int w (count (.bidx csi)))
    (doseq [[chr-index offsets] (.bidx csi)]
      (lsb/write-int w (count offsets))
      (doseq [[bin chunks] offsets]
        (lsb/write-int w bin)
        (lsb/write-long w (get-in (.loffset csi)
                                  [chr-index
                                   (util-bin/bin-beg bin
                                                     (.min-shift csi)
                                                     (.depth csi))]))
        (lsb/write-int w (count chunks))
        (doseq [chunk chunks]
          (lsb/write-long w (:beg chunk))
          (lsb/write-long w (:end chunk)))))))
