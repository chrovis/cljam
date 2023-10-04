(ns cljam.io.util.dataframe
  (:import [java.util IdentityHashMap]))

(defprotocol IDataFrameBuffer
  (-append-val! [this val]))

(deftype DataFrameBuffer
         [^int m ^int n columns ^objects types ^objects data
          ^:unsynchronized-mutable ^long i ^:unsynchronized-mutable ^long j]
  IDataFrameBuffer
  (-append-val! [_ val]
    (let [t (aget types j)
          arr (aget data j)]
      (case t
        :long (aset ^longs arr i (long val))
        :double (aset ^doubles arr i (double val))
        (aset ^objects arr i val))
      (set! j (inc j))
      (when (>= j n)
        (set! i (inc i))
        (set! j 0))
      val)))

(defn make-dataframe-buffer [m columns]
  (let [n (count columns)
        columns' (IdentityHashMap. n)
        _ (doseq [[i [k _]] (map-indexed vector columns)]
            (.put columns' k i))
        types (into-array Object (map second columns))
        data (->> columns
                  (map (fn [[_ t]]
                         (case t
                           :long (long-array m)
                           :double (double-array m)
                           (object-array m))))
                  (into-array Object))]
    (DataFrameBuffer. m n columns' types data 0 0)))

(defn append-val! [buffer val]
  (-append-val! buffer val))

(defn append-row! [buffer row]
  (run! (partial append-val! buffer) row))

(declare make-dataframe-seq make-dataframe-row dataframe-row-accessors)

(deftype DataFrame
         [^int m, ^int n, ^int offset, columns
          ^objects types, ^objects data, accessors]
  clojure.lang.Counted
  (count [_] (- m offset))
  clojure.lang.Indexed
  (nth [this i]
    (let [i' (- i offset)]
      (if (< i' m)
        (make-dataframe-row this i' @accessors)
        (throw (IndexOutOfBoundsException.)))))
  (nth [this i not-found]
    (let [i' (- i offset)]
      (if (< i' m)
        (make-dataframe-row this i' @accessors)
        not-found)))
  clojure.lang.ILookup
  (valAt [this i]
    (let [i' (- (int i) offset)]
      (when (< i' m)
        (nth this i))))
  (valAt [this i not-found]
    (nth this i not-found))
  clojure.lang.Seqable
  (seq [this]
    (make-dataframe-seq this offset @accessors))
  clojure.lang.IReduceInit
  (reduce [this f init]
    (let [accessors' @accessors]
      (loop [i offset, acc init]
        (if (>= i m)
          acc
          (let [row (make-dataframe-row this i accessors')
                acc' (f acc row)]
            (recur (inc i) acc'))))))
  clojure.lang.IChunk
  (dropFirst [_]
    (DataFrame. m n (inc offset) columns types data accessors)))

(defn ->dataframe! [^DataFrameBuffer buffer]
  (let [accessors (volatile! nil)
        frame (DataFrame. (.-m buffer) (.-n buffer) 0 (.-columns buffer)
                          (.-types buffer) (.-data buffer) accessors)]
    (vreset! accessors (dataframe-row-accessors frame))
    frame))

(defmethod print-method DataFrame [^DataFrame frame ^java.io.Writer w]
  (.write w "[")
  (doseq [[i row] (map-indexed vector frame)]
    (.write w (pr-str row))
    (when-not (= (long i) (dec (.-m frame)))
      (.write w " ")))
  (.write w "]"))

(deftype DataFrameSeq [^DataFrame frame ^int offset accessors]
  clojure.lang.ISeq
  (first [_]
    (when (< offset (.-m frame))
      (make-dataframe-row frame offset accessors)))
  (more [_]
    (if (< (inc offset) (.-m frame))
      (DataFrameSeq. frame (inc offset) accessors)
      ()))
  (next [_]
    (when (< (inc offset) (.-m frame))
      (DataFrameSeq. frame (inc offset) accessors)))
  clojure.lang.Seqable
  (seq [this] this))

(defn- make-dataframe-seq [frame offset accessors]
  (DataFrameSeq. frame offset accessors))

(declare dataframe-row->map)

(deftype DataFrameRow [^DataFrame frame ^int row ^java.util.IdentityHashMap accessors]
  clojure.lang.Counted
  (count [_] (.-n frame))
  clojure.lang.ILookup
  (valAt [this k]
    (when-let [f (.get accessors k)]
      (f this)))
  (valAt [this k not-found]
    (if-let [f (.get accessors k)]
      (f this)
      not-found))
  clojure.lang.Seqable
  (seq [this]
    (seq (dataframe-row->map this)))
  clojure.lang.Associative
  (assoc [this k v]
    (assoc (dataframe-row->map this) k v))
  (containsKey [_ k]
    (contains? accessors k))
  (entryAt [this k]
    (when-let [f (.get accessors k)]
      (clojure.lang.MapEntry/create k (f this))))
  clojure.lang.IPersistentMap
  (without [this k]
    (dissoc (dataframe-row->map this) k))
  clojure.lang.IPersistentCollection
  (empty [_] {})
  (cons [this [k v]]
    (assoc this k v))
  clojure.lang.IKeywordLookup
  (getLookupThunk [_ k]
    (.get accessors k)))

(defn- dataframe-row->map [^DataFrameRow row]
  (let [^DataFrame frame (.-frame row)
        accessors (.-accessors row)]
    (into {} (map #(vector % ((get accessors %) row)))
          (keys (.-columns frame)))))

(defmethod print-method DataFrameRow [row w]
  (print-method (dataframe-row->map row) w))

(defmacro thunk
  {:clj-kondo/lint-as 'clojure.core/fn}
  [[arg] & body]
  `(reify
     clojure.lang.IFn
     (~'invoke [this# arg#]
       (let [~arg arg#]
         ~@body))
     clojure.lang.ILookupThunk
     (~'get [this# arg#]
       (let [~arg arg#]
         ~@body))))

(defn dataframe-row-accessor [^DataFrame frame key]
  (let [^IdentityHashMap columns (.-columns frame)
        col (int (.get columns key))
        t (aget ^objects (.-types frame) col)]
    (case t
      :long (thunk [^DataFrameRow row]
              (aget ^longs (aget ^objects (.-data frame) col) (.-row row)))
      :double (thunk [^DataFrameRow row]
                (aget ^doubles (aget ^objects (.-data frame) col) (.-row row)))
      (thunk [^DataFrameRow row]
        (aget ^objects (aget ^objects (.-data frame) col) (.-row row))))))

(defn dataframe-row-accessors [^DataFrame frame]
  (let [cols (.-columns frame)
        m (java.util.IdentityHashMap. (count cols))]
    (run! #(.put m % (dataframe-row-accessor frame %)) (keys cols))
    m))

(defn make-dataframe-row [frame i accessors]
  (DataFrameRow. frame i accessors))

(comment
  (def buffer
    (doto (make-dataframe-buffer 3 [[:chr :object] [:pos :long] [:qual :double]])
      (append-row! ["chr1" 123 30])
      (append-row! ["chr2" 456 0])
      (append-row! ["chr3" 789 20])))

  (def frame (->dataframe! buffer))
  (get (nth frame 0) :chr)
  (get (nth frame 1) :pos)
  (get (nth frame 2) :qual)
  (select-keys (nth frame 2) [:chr :pos])
  (let [{:keys [chr pos qual]} (dataframe-row-accessors frame)]
    (run! (fn [row]
            (println "chr:" (chr row))
            (println "pos:" (pos row))
            (println "qual:" (qual row)))
          frame))
  (chunk-cons frame nil))
