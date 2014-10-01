(ns cr-map
  (:use
   clojure.test))

;;; Not a slide, just some odds and ends

(def number-desc
  (comparator (fn [n m] (> n m))))

(defn first-key [map] (first (first map)))
(defn first-val [map] (second (first map)))

(def ^:dynamic creator-key "a")
(defmacro as-creator
  [c & body]
  `(binding [creator-key ~c]
     ~@body))

(declare cr-map-id cr-major)


;;; The over the wire update format
;;;
;;; - Identifies the map by name
;;;
;;; - Is marked with the new highest major version number
;;;
;;; - Is tagged with the minor version number, an orderable bit of
;;;   stuff that identifies the writer

(defn cr-update
  [map & kvs]
  {:map (cr-map-id map)
   :val (apply hash-map kvs)
   :maj (inc (cr-major map))
   :min creator-key})

(defn cr-update?
  [obj]
  (and (map? obj) (contains? obj :map)))


;;; The cr-map data structure
;;;
;;; - cr-map is an ordered (descending) map of major versions
;;; - revision is an ordered map of creator tag to update value

(defn cr-map
  [id]
  (with-meta
    (sorted-map-by number-desc)
    {:id id}))

(defn cr-map-id
  [map]
  (get (meta map) :id))

(defn cr-major
  [map]
  (or (first-key map) -1))

(defn cr-get-rev
  [map maj]
  (or (get map maj) (sorted-map)))


;;; Apply an update to a local cr-map
;;;
;;; - A functional update, this returns a new cr-map

(defn cr-apply
  [map update]
  (let [{val :val maj :maj min :min} update]
    (assoc map
      maj
      (assoc (cr-get-rev map maj)
        min val))))


;;; Deletion uses a tombstone
;;;
;;; - Top secret control values are a bad idea

(def tombstone [0xDEADBEEF])
(defn tombstone? [x] (= x tombstone))

(defn not-tombstone
  [x]
  (when-not (tombstone? x)
    x))

(defn cr-delete
  [map & ks]
  (apply cr-update
         map
         (interleave ks (repeat tombstone))))


;;; Get traverses a cr-map
;;;
;;; - Later versions hide values from earlier versions
;;;
;;; - Higher minor versions win in a major version

(declare rev-contains)

(defn cr-get
  [map key]
  (if-let [val (rev-contains (first-val map) key)]
    (not-tombstone (get val key))
    (recur (next map) key)))

(defn rev-contains
  [rev key]
  (when-not (empty? rev)
    (let [val (first-val rev)]
      (if (contains? val key)
        val
        (recur (next rev) key)))))


;;; A couple of helper functions short cut update and apply.

(defn cr-assoc
  [map & kvs]
  (cr-apply map (apply cr-update map kvs)))

(defn cr-dissoc
  [map & ks]
  (cr-apply map (apply cr-delete map ks)))


;;; It's a map!

(def foo (cr-assoc (cr-map "foo") :a 1 :b 2))
(def p1  (cr-update foo :b 1))
(def p2  (as-creator "b" (cr-update foo :b 2)))
(def bar (cr-apply foo p1))
(def p3  (cr-update bar :b 3))
(def p4  (as-creator "b" (cr-update bar :b 4)))
(def p5  (as-creator "c" (cr-update bar :b 5)))

(deftest test-mappiness
  (is (= (cr-get foo :a) 1))
  (is (= (cr-get foo :b) 2))
  (is (= 2  (cr-get (cr-assoc foo :a 2) :a)))
  (is (nil? (cr-get (cr-dissoc foo :a)  :a))))


;;; Idempotent

(deftest test-idempotency
  (is (identical?
       bar
       (cr-apply bar p1)))

  (is (identical?
       bar
       (cr-apply (cr-apply bar p1) p1))))


;;; Associtive

(defn ap [m & ps] (reduce cr-apply m ps))

(deftest test-associtivity
  (is (= (ap (ap bar p3 p4) p5)
         (ap (ap bar p3) p4 p5))))


;;; Commutative

(deftest test-commutivity
  (is (= (ap foo p1 p2)
         (ap foo p2 p1)))

  (is (= (ap foo p1 p5)
         (ap foo p5 p1)))

  (is (= (ap foo p1 p2 p3 p4 p5)
         (ap foo p5 p4 p3 p2 p1)
         (ap foo p3 p5 p4 p1 p2)
         (ap (ap foo p5 p3 p4) p1 p2)
         (ap foo p3 p2 p1 p5 p4 p3 p3 p3))))


;;; Some Final Thoughts
;;;
;;; - CRDTs in general don't preserve causality, just assert a total
;;;   order across writers
;;;
;;; - Compare to git (or cons cells)
;;;
;;; - Tradeoff supports reading the best answer now
