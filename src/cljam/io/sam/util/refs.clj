(ns cljam.io.sam.util.refs
  "Utility functions for SAM references.")
(defn make-refs
  "Return a reference sequence from the sam header."
  [hdr]
  (for [sq (:SQ hdr)]
    {:name (:SN sq), :len (:LN sq)}))

(defn- ref-id*
  [refs name]
  (some #(when (= name (:name (second %))) (first %))
        (map-indexed vector refs)))

(def ref-id
  "Returns reference ID from the reference sequence and the specified reference
  name. If not found, returns nil."
  (memoize ref-id*))

(defn ref-name
  "Returns a reference name from the reference ID. Returns nil if id is not
  mapped."
  [refs ^long id]
  (when (<= 0 id (dec (count refs)))
    (:name (nth refs id))))

(defn ref-by-name
  "Returns the first reference which has the specified name."
  [refs name]
  (some #(if (= (:name %) name) %) refs))
