(ns cr-map
  (:use
   clojure.test))


;;; The over the wire update format
;;;
;;; - Identifies the map by name, so the receiver can retrieve the
;;;   cached value from its local store
;;; - Contains a map of changes
;;; - Is marked with the new highest major version number
;;; - Is tagged with the minor version number, an orderable bit of
;;;   stuff that identifies the writer.

(declare cr-map-id cr-major)
(def ^:dynamic creator-key "a")
(defmacro with-c [c & body] `(binding [creator-key ~c] ~@body))

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

(def number-desc (comparator (fn [n m] (> n m))))

(defn cr-map
  [id]
  (-> (sorted-map-by number-desc)
      (with-meta {:id id})))

(defn cr-map-id [map] (:id (meta map)))

(def first-key ffirst)
(def first-val (comp second first))

(defn cr-major
  [map]
  (or (first-key map) -1))

(defn cr-get-rev
  [map maj]
  (or (get map maj) (sorted-map)))


;;; Apply an update to a local cr-map
;;;
;;; - A functional update, this returns a new, larger cr-map

(defn cr-apply
  [map {:keys [val maj min] :as update}]
  (assoc map
    maj
    (assoc (cr-get-rev map maj)
      min val)))


;;; Deletion uses a tombstone
;;;
;;; - Top secret control values are a bad idea
;;; - A separate deletion dictionary in the update is a

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

(deftest test-mappiness
  (def foo
    (-> (cr-map "foo")
        (cr-assoc :a 1 :b 2)))

  (is (= (cr-get foo :a) 1))
  (is (= (cr-get foo :b) 2))
  (is (= 2  (-> (cr-assoc foo :a 2) (cr-get :a))))
  (is (nil? (-> (cr-dissoc foo :a)  (cr-get :a)))))


;;; Idempotent

(deftest test-idempotency
  (def p3 (cr-update foo :b 3))
  (def bar (cr-apply foo p3))

  (is (identical? bar (-> bar (cr-apply p3) (cr-apply p3)))))


;;; Associtive

(defn ap [m & ps] (reduce cr-apply m ps))

(deftest test-associtivity
  (def p4 (cr-update bar :b 4))
  (def p5 (with-c "b" (cr-update bar :b 5)))
  (def p6 (with-c "c" (cr-update bar :c 6)))

  (is (= (ap (ap bar p4 p5) p6)
         (ap (ap bar p4) p5 p6))))


;;; Commutative

(deftest test-commutivity
  (def baz (ap bar p4 p5 p6))

  (def p7 (cr-update baz :b 7))
  (def p8 (with-c "b" (cr-delete baz :b)))

  (is (= (ap baz p7 p8)
         (ap baz p8 p7)))

  (is (= (ap foo p8 p7 p6 p5 p4 p3)
         (ap foo p3 p4 p5 p6 p7 p8)
         (ap foo p3 p5 p4 p6 p8 p7)
         (ap (ap foo p5 p3 p4) p8 p6 p7)
         (ap (ap foo p6 p3 p7) p8 p5 p4 p3 p3 p3))))
