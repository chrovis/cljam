(ns cljam.io.util.dataframe
  (:import [java.util Arrays IdentityHashMap]))

(defprotocol IDataFrameBuffer
  (-append-val! [this val]))

(def ^:private prim-type->type-key
  {Byte/TYPE :byte
   Character/TYPE :char
   Short/TYPE :short
   Integer/TYPE :int
   Long/TYPE :long
   Float/TYPE :float
   Double/TYPE :double})

(defn- array-type [arr]
  (let [t (class arr)]
    (when (.isArray t)
      (get prim-type->type-key (.getComponentType t) :object))))

(defn- typed-aget [t arr i]
  (case t
    :boolean (aget ^booleans arr i)
    :byte (aget ^bytes arr i)
    :char (aget ^chars arr i)
    :short (aget ^shorts arr i)
    :int (aget ^ints arr i)
    :long (aget ^longs arr i)
    :float (aget ^floats arr i)
    :double (aget ^doubles arr i)
    (aget ^objects arr i)))

(defn- typed-aset [t arr i val]
  (case t
    :boolean (aset ^booleans arr i (boolean val))
    :byte (aset ^bytes arr i (byte val))
    :char (aset ^chars arr i (char val))
    :short (aset ^shorts arr i (short val))
    :int (aset ^ints arr i (int val))
    :long (aset ^longs arr i (long val))
    :float (aset ^floats arr i (float val))
    :double (aset ^doubles arr i (double val))
    (aset ^objects arr i val)))

(defn- make-typed-array [t x]
  (case t
    :boolean (boolean-array x)
    :byte (byte-array x)
    :char (char-array x)
    :short (short-array x)
    :int (int-array x)
    :long (long-array x)
    :float (float-array x)
    :double (double-array x)
    (object-array x)))

(defn- ->array [t coll]
  (if (.isArray (class coll))
    coll
    (make-typed-array t coll)))

(deftype DataFrameBuffer
         [^int m ^int n columns ^objects types ^objects data
          ^:unsynchronized-mutable ^long i ^:unsynchronized-mutable ^long j]
  IDataFrameBuffer
  (-append-val! [_ val]
    (let [t (aget types j)
          arr (aget data j)]
      (typed-aset t arr i val)
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
                  (map (fn [[_ t]] (make-typed-array t m)))
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

(defn- make-dataframe* [m n offset columns types data]
  (let [accessors (volatile! nil)
        frame (DataFrame. (int m) (int n) offset columns types data accessors)]
    (vreset! accessors (dataframe-row-accessors frame))
    frame))

(defn make-dataframe [column-data]
  (let [m (count (second (first column-data)))
        n (count column-data)
        columns (IdentityHashMap. n)
        types (object-array n)
        data (object-array n)]
    (doseq [[^long i [k arr]] (map-indexed vector column-data)]
      (.put columns k i)
      (aset types i (array-type arr))
      (aset data i arr))
    (make-dataframe* m n 0 columns types data)))

(defn ->dataframe! [^DataFrameBuffer buffer]
  (make-dataframe* (.-m buffer) (.-n buffer) 0 (.-columns buffer)
                   (.-types buffer) (.-data buffer)))

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
      :boolean (thunk [^DataFrameRow row]
                      (aget ^booleans (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                            (.-row row)))
      :byte (thunk [^DataFrameRow row]
                   (aget ^bytes (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                         (.-row row)))
      :char (thunk [^DataFrameRow row]
                   (aget ^chars (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                         (.-row row)))
      :int (thunk [^DataFrameRow row]
                  (aget ^ints (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                        (.-row row)))
      :short (thunk [^DataFrameRow row]
                    (aget ^shorts (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                          (.-row row)))
      :long (thunk [^DataFrameRow row]
                   (aget ^longs (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                         (.-row row)))
      :float (thunk [^DataFrameRow row]
                    (aget ^floats (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                          (.-row row)))
      :double (thunk [^DataFrameRow row]
                     (aget ^doubles (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                           (.-row row)))
      (thunk [^DataFrameRow row]
             (aget ^objects (aget ^objects (.-data ^DataFrame (.-frame row)) col)
                   (.-row row))))))

(defn dataframe-row-accessors [^DataFrame frame]
  (let [cols (.-columns frame)
        m (java.util.IdentityHashMap. (count cols))]
    (run! #(.put m % (dataframe-row-accessor frame %)) (keys cols))
    m))

(defn make-dataframe-row [frame i accessors]
  (DataFrameRow. frame i accessors))

(defn column-defs->accessors [defs]
  (let [buf (make-dataframe-buffer 0 defs)
        frame (->dataframe! buf)]
    (dataframe-row-accessors frame)))

(defn add-columns [^DataFrame frame defs new-data]
  (let [n (.-n frame)
        n' (+ n (count defs))
        ^IdentityHashMap columns (.-columns frame)
        ^IdentityHashMap columns' (.clone columns)
        ^objects types (.-types frame)
        ^objects types' (Arrays/copyOf types n')
        ^objects data (.-data frame)
        ^objects data' (Arrays/copyOf data n')]
    (doseq [[^long i [name t] coll] (map vector (range) defs new-data)
            :let [i' (+ n i)]]
      (.put columns' name i')
      (aset types' i' t)
      (aset data' i' (->array t coll)))
    (make-dataframe* (.-m frame) n' (.-offset frame) columns' types' data')))

(defn replace-columns [^DataFrame frame col-names defs new-data]
  (let [n (.-n frame)
        ^IdentityHashMap columns (.-columns frame)
        ^IdentityHashMap columns' (.clone columns)
        ^objects types (.-types frame)
        ^objects types' (Arrays/copyOf types n)
        ^objects data (.-data frame)
        ^objects data' (Arrays/copyOf data n)
        col-indices (map #(.get columns %) col-names)]
    (doseq [[^long i [name t] coll] (map vector col-indices defs new-data)]
      (.put columns' name i)
      (aset types' i t)
      (aset data' i (->array t coll)))
    (make-dataframe* (.-m frame) n (.-offset frame) columns' types' data')))

(defn map-column
  ([frame col-name f]
   (map-column frame col-name nil f))
  ([^DataFrame frame col-name col-type f]
   (let [m (.-m frame)
         col-idx (.get ^IdentityHashMap (.-columns frame) col-name)
         old-col-type (aget ^objects (.-types frame) col-idx)
         old-col (aget ^objects (.-data frame) col-idx)
         col-type' (or col-type old-col-type)
         arr (make-typed-array col-type' m)]
     (dotimes [i m]
       (typed-aset col-type' arr i
                   (f (typed-aget old-col-type old-col i))))
     (replace-columns frame [col-name] [[col-name col-type']] [arr]))))

(defn drop-columns [^DataFrame frame col-names]
  (let [n (.-n frame)
        n' (- n (count col-names))
        ^IdentityHashMap columns (.-columns frame)
        ^IdentityHashMap columns' (IdentityHashMap. n')
        ^objects types (.-types frame)
        ^objects types' (object-array n')
        ^objects data (.-data frame)
        ^objects data' (object-array n')
        col-names' (set col-names)]
    (loop [[[col i] & more] (sort-by val columns)
           i' 0]
      (when col
        (if (contains? col-names' col)
          (recur more i')
          (do
            (.put columns' col i')
            (aset types' i' (aget types i))
            (aset data' i' (aget data i))
            (recur more (inc i'))))))
    (make-dataframe* (.-m frame) n' (.-offset frame) columns' types' data')))

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
