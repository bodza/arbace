(ns arbace.core
    (:refer-clojure :only [*ns* = and apply assoc cond conj cons defmacro #_def defn defprotocol defrecord #_do doseq extend-type first #_if if-not import inc into keys let letfn loop next ns-imports ns-unmap or peek pop #_recur second seq some? str symbol symbol? #_throw update vary-meta vector?])
    (:require [clojure.core.rrb-vector :refer [catvec subvec vec #_vector]] [flatland.ordered.map :refer [#_ordered-map]] [flatland.ordered.set :refer [#_ordered-set]])
)

(defmacro § [& _])
(defmacro ß [& _])

(defmacro defp [p & s] (let [i (symbol (str p "'iface"))] `(do (defprotocol ~p ~@s) (def ~i (:on-interface ~p)) ~p)))
(defmacro defr [r & s] (let [c (symbol (str r "'class"))] `(do (defrecord ~c [])    (extend-type ~c ~r ~@s)     ~c)))
(defmacro defm [r p & s] (let [i (:on r)]                 `(do                      (extend-type ~i ~p ~@s)     ~i)))

(defmacro class-ns [r [& s] & z] `(do (defr ~r ~@s) ~@z))
(defmacro value-ns [_ & z] (cons 'do z))

(doseq [% (keys (ns-imports *ns*))] (ns-unmap *ns* %))

(import
    [java.lang Boolean Error String]
)

(defmacro throw! [^String s] `(throw (Error. ~s)))

(defmacro def-      [x & s] `(def      ~(vary-meta x assoc :private true) ~@s))
(defmacro defn-     [x & s] `(defn     ~(vary-meta x assoc :private true) ~@s))
(defmacro defmacro- [x & s] `(defmacro ~(vary-meta x assoc :private true) ~@s))

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro     when       [? & s] (let [[e & s] (=> s)]               `(if     ~? (do ~@s) ~e)))
    (defmacro     when-not   [? & s] (let [[e & s] (=> s)]               `(if-not ~? (do ~@s) ~e)))
    (defmacro let-when     [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if     ~? (do ~@s) ~e))))
    (defmacro let-when-not [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if-not ~? (do ~@s) ~e))))
)

(letfn [(v' [v] (cond (vector? v) v (symbol? v) [v v] :else [`_# v]))
        (r' [r] (cond (vector? r) `((recur ~@r)) (some? r) `((recur ~r))))
        (=> [s] (if (= '=> (first s)) (next s) (cons nil s)))
        (l' [v ? r s] (let [r (r' r) [e & s] (=> s)] `(loop ~(v' v) (if ~? (do ~@s ~@r) ~e))))]
    (defmacro loop-when [v ? & s] (l' v ? nil s))
    (defmacro loop-when-recur [v ? r & s] (l' v ? r s))
)

(letfn [(r' [r] (cond (vector? r) `(recur ~@r) (some? r) `(recur ~r)))
        (=> [s] (if (= '=> (first s)) (second s)))]
    (defmacro recur-if [? r & s] `(if ~? ~(r' r) ~(=> s)))
)

(defmacro any
    ([f x y] `(~f ~x ~y))
    ([f x y & s] `(let [f# ~f x# ~x] (or (f# x# ~y) (any f# x# ~@s))))
)

(defn index-of [s x]
    (loop-when [i 0 s (seq s)] (some? s) => -1
        (when-not (= (first s) x) => i
            (recur (inc i) (next s))
        )
    )
)

(defn assoc'  [v i x & s] (apply assoc  (vec v) i x s))
(defn conj'   [v   x & s] (apply conj   (vec v)   x s))
(defn into'   [v       s]       (into   (vec v)     s))
(defn peek'   [v]               (peek   (vec v)      ))
(defn pop'    [v]               (pop    (vec v)      ))
(defn update' [v i f & s] (apply update (vec v) i f s))

(defn dissoc' [v i] (let [v (vec v)] (catvec (subvec v 0 i) (subvec v (inc i)))))

(import
    [jdk.vm.ci.hotspot CompilerToVM HotSpotJVMCIRuntime HotSpotVMConfig]
)

(value-ns HotSpot
    (def #_"HotSpotJVMCIRuntime" JVMCI'runtime (HotSpotJVMCIRuntime/runtime))

    (def #_"CompilerToVM"    HotSpot'native (#_"HotSpotJVMCIRuntime" .getCompilerToVM JVMCI'runtime))
    (def #_"HotSpotVMConfig" HotSpot'config (#_"HotSpotJVMCIRuntime" .getConfig       JVMCI'runtime))

    (def #_"boolean" HotSpot'useCompressedOops          (.getFlag HotSpot'config, "UseCompressedOops",          Boolean))
    (def #_"boolean" HotSpot'useCompressedClassPointers (.getFlag HotSpot'config, "UseCompressedClassPointers", Boolean))

    (when-not (and HotSpot'useCompressedOops HotSpot'useCompressedClassPointers)
        (throw! "“Use the Force, Luke!”")
    )
)

(value-ns Arbace)

(defn -main [& args])

(§ soon
(ns rrb-vector
    (:refer-clojure :exclude [->VecSeq subvec vector vector-of vec])
    (:require
        [clojure.core.protocols :refer [IKVReduce]]
    )
    (:import
        [java.util.concurrent.atomic AtomicReference]
        [clojure.core ArrayManager Vec VecNode]
        [clojure.lang
            APersistentVector$SubVector ArityException Associative Box Cons Counted IEditableCollection IFn IHashEq ILookup IMeta IObj IPersistentCollection
            IPersistentMap IPersistentStack IPersistentVector ISeq ITransientAssociative ITransientCollection ITransientVector Indexed MapEntry PersistentList
            PersistentVector PersistentVector$Node RT Reversible Seqable Sequential Util
        ]
    )
)

(set! *unchecked-math* true) ;; :warn-on-boxed

(defprotocol PSliceableVector
    (slicev [v i e])
)

(defprotocol PSpliceableVector
    (splicev [v w])
)

(defprotocol AsRRBT
    (as-rrbt [v])
)

;;; array managers

(defmacro mk-am [t] (#'clojure.core/mk-am &env &form t))

(definline object [x] x)

(def ams (assoc @#'clojure.core/ams :object (mk-am object)))

(def object-am (ams :object))

;;; empty nodes

(def empty-pv-node PersistentVector/EMPTY_NODE)

(def empty-gvec-node clojure.core/EMPTY-NODE)

;;; node managers

(definterface NodeManager
    (node [^AtomicReference edit arr])
    (empty [])
    (array [node])
    (^AtomicReference edit [node])
    (^boolean regular [node])
    (clone [^ArrayManager am ^int shift node])
)

(def object-nm
    (reify NodeManager
        (node [_ edit arr] (PersistentVector$Node. edit arr))
        (empty [_] empty-pv-node)
        (array [_ node] (.-array ^PersistentVector$Node node))
        (edit [_ node] (.-edit ^PersistentVector$Node node))
        (regular [_ node] (not (== (alength ^objects (.-array ^PersistentVector$Node node)) (int 33))))
        (clone [_ am shift node]
            (PersistentVector$Node. (.-edit ^PersistentVector$Node node) (aclone ^objects (.-array ^PersistentVector$Node node)))
        )
    )
)

(def primitive-nm
    (reify NodeManager
        (node [_ edit arr] (VecNode. edit arr))
        (empty [_] empty-gvec-node)
        (array [_ node] (.-arr ^VecNode node))
        (edit [_ node] (.-edit ^VecNode node))
        (regular [_ node] (not (== (alength ^objects (.-arr ^VecNode node)) (int 33))))
        (clone [_ am shift node]
            (if (zero? shift)
                (VecNode. (.-edit ^VecNode node) (.aclone am (.-arr ^VecNode node)))
                (VecNode. (.-edit ^VecNode node) (aclone ^objects (.-arr ^VecNode node)))
            )
        )
    )
)

;;; ranges

(defmacro ranges [nm node]
  `(ints (aget ~(with-meta `(.array ~nm ~node) {:tag 'objects}) 32))
)

(defn last-range [^NodeManager nm node]
    (let [
        b (ranges nm node)
        i (unchecked-dec-int (aget b 32))
    ]
        (aget b i)
    )
)

(defn regular-ranges [shift cnt]
    (let [
        step (<< (int 1) (int shift))
        b' (int-array 33)
    ]
        (loop [i (int 0) r step]
            (if (< r cnt)
                (do
                    (aset b' i r)
                    (recur (unchecked-inc-int i) (unchecked-add-int r step))
                )
                (do
                    (aset b' i (int cnt))
                    (aset b' 32 (unchecked-inc-int i))
                    b'
                )
            )
        )
    )
)

;;; root overflow

(defn overflow? [^NodeManager nm root shift cnt]
    (if (.regular nm root)
        (< (<< (int 1) (int shift)) (>> (unchecked-inc-int (int cnt)) (int 5)))
        (let [
            b (ranges nm root)
            n (aget b 32)
        ]
            (and (== n (int 32))
                (or (== (int shift) (int 5))
                    (recur
                        nm
                        (aget ^objects (.array nm root) (unchecked-dec-int n))
                        (unchecked-subtract-int (int shift) (int 5))
                        (unchecked-add-int (unchecked-subtract-int (aget b 31) (aget b 30)) (int 32))
                    )
                )
            )
        )
    )
)

;;; find 0 / nil

(defn index-of-0 ^long [a]
    (let [a (ints a)]
        (loop-when [l 0 h 31] (< l (unchecked-dec h)) => (cond (zero? (aget a l)) l (zero? (aget a h)) h :else 32)
            (let [mid (unchecked-add l (>> (unchecked-subtract h l) 1))]
                (if (zero? (aget a mid))
                    (recur l mid)
                    (recur (unchecked-inc-int mid) h)
                )
            )
        )
    )
)

(defn index-of-nil ^long [a]
    (loop-when [l 0 h 31] (< l (unchecked-dec h)) => (cond (nil? (aget ^objects a l)) l (nil? (aget ^objects a h)) h :else 32)
        (let [mid (unchecked-add l (>> (unchecked-subtract h l) 1))]
            (if (nil? (aget ^objects a mid))
                (recur l mid)
                (recur (unchecked-inc-int mid) h)
            )
        )
    )
)

;;; children

(defn first-child [^NodeManager nm node]
    (aget ^objects (.array nm node) 0)
)

(defn last-child [^NodeManager nm node]
    (let [a (.array nm node)]
        (aget ^objects a (if (.regular nm node) (dec (index-of-nil a)) (unchecked-dec-int (aget (ranges nm node) 32))))
    )
)

(defn remove-leftmost-child [^NodeManager nm shift parent]
    (let [a (.array nm parent)]
        (when (some? (aget ^objects a 1)) => nil
            (let [
                regular? (.regular nm parent)
                a' (object-array (if regular? 32 33))
                _ (System/arraycopy a 1 a' 0 31)
            ]
                (when-not regular?
                    (let [
                        b (ranges nm parent)
                        n (dec (aget b 32))
                        b' (int-array 33)
                        b0 (aget b 0)
                        _ (dotimes [i n] (aset b' i (- (aget b (inc i)) b0)))
                        _ (aset b' n (int 0))
                        _ (aset b' 32 n)
                    ]
                        (aset ^objects a' 32 b')
                    )
                )
                (.node nm (.edit nm parent) a')
            )
        )
    )
)

(defn replace-leftmost-child [^NodeManager nm shift parent pcnt child delta]
    (if (.regular nm parent)
        (let [
            b' (int-array 33)
            step (<< 1 shift)
            n (& (>> shift (dec pcnt)) 0x1f)
            _ (aset b' 0 (int (- step delta)))
            _ (aset b' n (int (- pcnt delta)))
            _ (dotimes [i n] (aset b' (inc i) (+ (aget b' i) step)))
            _ (aset b' 32 (int (inc n)))
            a' (object-array 33)
            _ (aset ^objects a' 0 child)
            _ (System/arraycopy (.array nm parent) 1 a' 1 n)
            _ (aset ^objects a' 32 b')
        ]
            (.node nm nil a')
        )
        (let [
            b' (int-array 33)
            b (ranges nm parent)
            n (aget b 32)
            _ (dotimes [i n] (aset b' i (- (aget b i) (int delta))))
            _ (aset b' 32 n)
            a' (aclone ^objects (.array nm parent))
            _ (aset ^objects a' 0 child)
            _ (aset ^objects a' 32 b')
        ]
            (.node nm nil a')
        )
    )
)

(defn replace-rightmost-child [^NodeManager nm shift parent child delta]
    (if (.regular nm parent)
        (let [
            a (.array nm parent)
            n (unchecked-dec (index-of-nil a))
        ]
            (if (.regular nm child)
                (let [
                    a' (aclone ^objects a)
                    _ (aset ^objects a' n child)
                ]
                    (.node nm nil a')
                )
                (let [
                    step (<< 1 shift)
                    b' (int-array 33)
                    _ (loop-when-recur [i 0 r step] (<= i n) [(inc i) (+ r step)] (aset b' i r))
                    _ (aset b' n (int (last-range nm child)))
                    _ (aset b' 32 (inc n))
                    a' (object-array 33)
                    _ (System/arraycopy a 0 a' 0 n)
                    _ (aset ^objects a' n child)
                    _ (aset ^objects a' 32 b')
                ]
                    (.node nm nil a')
                )
            )
        )
        (let [
            b (ranges nm parent)
            b' (aclone b)
            n (dec (aget b 32))
            _ (aset b' n (int (+ (aget b n) delta)))
            a' (aclone ^objects (.array nm parent))
            _ (aset ^objects a' n child)
            _ (aset ^objects a' 32 b')
        ]
            (.node nm nil a')
        )
    )
)

;;; fold-tail

(defn new-path [^NodeManager nm ^ArrayManager am shift node]
    (let [
        reg? (== (.alength am (.array nm node)) 32)
        n (if reg? 32 33)
        a (object-array n)
        b
            (when-not reg?
                (doto (int-array 33)
                    (aset 0 (.alength am (.array nm node)))
                    (aset 32 1)
                )
            )
        ret (.node nm nil a)
    ]
        (loop [a a shift shift]
            (if (== shift 5)
                (do
                    (when-not reg?
                        (aset a 32 b)
                    )
                    (aset a 0 node)
                )
                (let [
                    a' (object-array n)
                    e (.node nm nil a')
                ]
                    (aset a 0 e)
                    (when-not reg?
                        (aset a 32 b)
                    )
                    (recur a' (- shift 5))
                )
            )
        )
        ret
    )
)

(defn fold-tail [^NodeManager nm ^ArrayManager am node shift cnt tail]
    (let [
        n (.alength am tail)
        reg? (and (.regular nm node) (== n 32))
        a (.array nm node)
        i (index-of-nil a)
        a' (object-array (if reg? 32 33))
        b (when-not (.regular nm node) (ranges nm node))
        t
            (when-not (== shift 5) => (.node nm nil tail)
                (fold-tail nm am
                    (aget ^objects a (dec i))
                    (- shift 5)
                    (when-not (.regular nm node) => (mod cnt (<< 1 shift))
                        (let [i (unchecked-dec-int (aget b 32))]
                            (if (pos? i)
                                (unchecked-subtract-int (aget b i) (aget b (unchecked-dec-int i)))
                                (aget b 0)
                            )
                        )
                    )
                    tail
                )
            )
        b' (ints (when-not reg? (if b (aclone b) (regular-ranges shift cnt))))
    ]
        (when-not (and (or (nil? t) (== shift 5)) (== i 32))
            (System/arraycopy a 0 a' 0 i)
            (when-not reg?
                (if (or (nil? t) (== shift 5))
                    (do
                        (aset b' i (+ (if (pos? i) (aget b' (dec i)) (int 0)) n))
                        (aset b' 32 (inc i))
                    )
                    (do
                        (when (pos? i)
                            (aset b' (dec i) (+ (aget b' (dec i)) n))
                        )
                        (aset b' 32 i)
                    )
                )
            )
            (when-not reg?
                (aset a' 32 b')
            )
            (if (nil? t)
                (aset a' i (new-path nm am (unchecked-subtract-int shift 5) (.node nm nil tail)))
                (aset a' (if (== shift 5) i (dec i)) t)
            )
            (.node nm nil a')
        )
    )
)

(def- rrbt-concat-threshold 33)
(def- max-extra-search-steps 2)

(defmacro- caching-hash [coll hash-fn hash-key]
  `(let [h# ~hash-key]
        (when (== h# (int -1)) => h#
            (let [h# (~hash-fn ~coll)]
                (set! ~hash-key (int h#))
                h#
            )
        )
    )
)

(defn- hash-gvec-seq [s]
    (loop-when [hash (int 1) s (seq s)] s => hash
        (recur (unchecked-add-int (unchecked-multiply-int 31 hash) (Util/hash (first s))) (next s))
    )
)

(definterface IVecImpl
    (^int tailoff [])
    (arrayFor [^int i])
    (pushTail [^int shift ^int cnt parent tailnode])
    (popTail [^int shift ^int cnt node])
    (newPath [^AtomicReference edit ^int shift node])
    (doAssoc [^int shift node ^int i val])
)

(deftype VecSeq [^ArrayManager am, ^IVecImpl vec, anode, ^int i, ^int offset, ^IPersistentMap _meta, ^:unsynchronized-mutable ^int _hash, ^:unsynchronized-mutable ^int _hasheq]
    clojure.core.protocols/InternalReduce
    (internal-reduce [_ f val]
        (loop-when [result val aidx i off offset] (< aidx (count vec)) => result
            (let [
                node (.arrayFor vec aidx)
                alen (.alength am node)
                result
                    (loop-when [result result node-idx off] (< node-idx alen) => result
                        (let [result (f result (.aget am node node-idx))]
                            (if (reduced? result)
                                result
                                (recur result (inc node-idx))
                            )
                        )
                    )
            ]
                (if (reduced? result)
                    @result
                    (recur result (+ aidx alen) 0)
                )
            )
        )
    )

    Object
    (hashCode [this] (caching-hash this hash-gvec-seq _hash))
    (equals [this that]
        (or (identical? this that)
            (and (sequential? that)
                (loop-when [xs this ys (seq that)] xs => (nil? ys)
                    (and ys (Util/equals (first xs) (first ys))
                        (recur (next xs) (next ys))
                    )
                )
            )
        )
    )

    IHashEq
    (hasheq [this]
        (when (== _hasheq (int -1)) => _hasheq
            (let [hash (hash-ordered-coll this)]
                (set! _hasheq (int hash))
                hash
            )
        )
    )

    IMeta
    (meta [this] _meta)

    IObj
    (withMeta [this m] (VecSeq. am vec anode i offset m _hash _hasheq))

    Counted
    (count [this] (unchecked-subtract-int (unchecked-subtract-int (count vec) i) offset))

    ISeq
    (first [_] (.aget am anode offset))
    (next [this]
        (if (< (inc offset) (.alength am anode))
            (VecSeq. am vec anode i (inc offset) nil -1 -1)
            (let [nexti (+ i (.alength am anode))]
                (when (< nexti (count vec))
                    (VecSeq. am vec (.arrayFor vec nexti) nexti 0 nil -1 -1)
                )
            )
        )
    )
    (more [this] (or (.next this) (PersistentList/EMPTY)))
    (cons [this o] (Cons. o this))
    (equiv [this o]
        (or (identical? this o)
            (and (instance? Sequential o)
                (loop-when [me this you (seq o)] (some? me) => (nil? you)
                    (and (Util/equiv (first me) (first you))
                        (recur (next me) (next you))
                    )
                )
            )
        )
    )
    (empty [_] PersistentList/EMPTY)

    Seqable
    (seq [this] this)
)

(defn slice-right [^NodeManager nm ^ArrayManager am node shift end]
    (let [shift (int shift) end (int end)]
        (if (zero? shift)
            ;; potentially return a short node, although it would be better to make sure a regular
            ;; leaf is always left at the right, with any items over the final 32 moved into tail
            ;; (and then potentially back into the tree should the tail become too long...)
            (let [
                a' (.array am end)
                _ (System/arraycopy (.array nm node) 0 a' 0 end)
            ]
                (.node nm nil a')
            )
            (let [
                regular? (.regular nm node)
                b (when-not regular? (ranges nm node))
                n (& (>> (unchecked-dec-int end) shift) (int 0x1f))
                n
                    (when-not regular? => n
                        (loop-when [i n] (< (aget b i) end) => i
                            (recur (unchecked-inc-int i))
                        )
                    )
                child-end
                    (if regular?
                        (let [ce (unchecked-remainder-int end (<< (int 1) shift))]
                            (if (zero? ce) (<< (int 1) shift) ce)
                        )
                        (when (pos? n) => end
                            (unchecked-subtract-int end (aget b (unchecked-dec-int n)))
                        )
                    )
                a (.array nm node)
                c' (slice-right nm am (aget ^objects a n) (unchecked-subtract-int shift (int 5)) child-end)
                regular-child?
                    (if (== shift (int 5))
                        (== (int 32) (.alength am (.array nm c')))
                        (.regular nm c')
                    )
                a' (object-array (if (and regular? regular-child?) 32 33))
                d'
                    (if regular-child?
                        (let [m (mod child-end (<< 1 shift))]
                            (if (zero? m) (<< 1 shift) m)
                        )
                        (if (== shift (int 5))
                            (.alength am (.array nm c'))
                            (last-range nm c')
                        )
                    )
                _ (System/arraycopy a 0 a' 0 n)
                _ (aset ^objects a' n c')
            ]
                (when-not (and regular? regular-child?)
                    (let [
                        b' (int-array 33)
                        step (<< (int 1) shift)
                    ]
                        (if regular?
                            (dotimes [i n] (aset b' i (unchecked-multiply-int (inc i) step)))
                            (dotimes [i n] (aset b' i (aget b i)))
                        )
                        (aset b' n (unchecked-add-int (if (pos? n) (aget b' (unchecked-dec-int n)) (int 0)) d'))
                        (aset b' 32 (unchecked-inc-int n))
                        (aset a' 32 b')
                    )
                )
                (.node nm nil a')
            )
        )
    )
)

(defn slice-left [^NodeManager nm ^ArrayManager am node shift start end]
    (let [
        shift (int shift)
        start (int start)
        end (int end)
    ]
        (if (zero? shift)
            ;; potentially return a short node
            (let [
                a (.array nm node)
                n' (unchecked-subtract-int (.alength am a) start)
                a' (.array am n')
                _ (System/arraycopy a start a' 0 n')
            ]
                (.node nm nil a')
            )
            (let [
                regular? (.regular nm node)
                a (.array nm node)
                b (when-not regular? (ranges nm node))
                n (& (>> start shift) (int 0x1f))
                n
                    (when-not regular? => n
                        (loop-when [i n] (<= (aget b i) start) => i
                            (recur (unchecked-inc-int i))
                        )
                    )
                n'
                    (when regular? => (aget b 32)
                        (loop [n n]
                            (when-not (or (== n (int 32)) (nil? (aget ^objects a n))) => n
                                (recur (unchecked-inc-int n))
                            )
                        )
                    )
                x (if regular? (unchecked-multiply-int n (<< (int 1) shift)) (aget b (unchecked-dec-int n)))
                child-start (when (pos? n) => start (unchecked-subtract-int start x))
                child-end (int (min (<< (int 1) shift) (when (pos? n) => end (unchecked-subtract-int end x))))
                c' (slice-left nm am (aget ^objects a n) (unchecked-subtract-int shift (int 5)) child-start child-end)
                n' (unchecked-subtract-int n' n)
                n' (if (nil? c') (unchecked-dec-int n') n')
            ]
                (cond
                    (zero? n')
                        nil
                    regular?
                        (let [
                            a' (object-array 33)
                            b' (int-array 33)
                            b0
                                (if (or (nil? c') (== shift (int 5)) (.regular nm c'))
                                    (unchecked-subtract-int (<< (int 1) shift) (& (>> start (unchecked-subtract-int shift (int 5))) (int 0x1f)))
                                    (int (last-range nm c'))
                                )
                            step (<< (int 1) shift)
                        ]
                            (loop-when [i (int 0) r b0] (< i n')
                                (aset b' i r)
                                (recur (unchecked-inc-int i) (unchecked-add-int r step))
                            )
                            (when (< 1 n')
                                (aset b' (dec n') (- end start))
                            )
                            (aset b' 32 n')
                            (System/arraycopy a (if (nil? c') (unchecked-inc-int n) n) a' 0 n')
                            (when (some? c')
                                (aset a' 0 c')
                            )
                            (aset a' 32 b')
                            (.node nm (.edit nm node) a')
                        )
                    :else
                        (let [
                            a' (object-array 33)
                            b' (int-array 33)
                        ]
                            (loop-when [i (int 0) n n] (< i n')
                                (aset b' i (unchecked-subtract-int (aget b n) start))
                                (recur (unchecked-inc-int i) (unchecked-inc-int n))
                            )
                            (aset b' 32 n')
                            (System/arraycopy a (if (nil? c') (unchecked-inc-int n) n) a' 0 n')
                            (when (some? c')
                                (aset a' 0 c')
                            )
                            (aset a' 32 b')
                            (.node nm (.edit nm node) a')
                        )
                )
            )
        )
    )
)

(declare splice-rrbts ->Transient)

(deftype Vector [^NodeManager nm, ^ArrayManager am, ^int cnt, ^int shift, root, tail, ^IPersistentMap _meta, ^:unsynchronized-mutable ^int _hash, ^:unsynchronized-mutable ^int _hasheq]
    Object
    (equals [this that]
        (or (identical? this that)
            (if (instance? IPersistentVector that)
                (and (== cnt (count that))
                    (loop [i (int 0)]
                        (or (== i cnt)
                            (and (.equals (.nth this i) (nth that i))
                                (recur (unchecked-inc-int i))
                            )
                        )
                    )
                )
                (and (instance? Sequential that)
                    (.equals (seq this) (seq that))
                )
            )
        )
    )
    (hashCode [this]
        (when (== _hash (int -1)) => _hash
            (loop [h (int 1) i (int 0)]
                (if (== i cnt)
                    (do
                        (set! _hash (int h))
                        h
                    )
                    (let [val (.nth this i)]
                        (recur (unchecked-add-int (unchecked-multiply-int (int 31) h) (Util/hash val)) (unchecked-inc-int i))
                    )
                )
            )
        )
    )

    IHashEq
    (hasheq [this]
        (when (== _hasheq (int -1)) => _hasheq
            (let [hash (hash-ordered-coll this)]
                (set! _hasheq (int hash))
                hash
            )
        )
    )

    Counted
    (count [_] cnt)

    IMeta
    (meta [_] _meta)

    IObj
    (withMeta [_ m] (Vector. nm am cnt shift root tail m _hash _hasheq))

    Indexed
    (nth [this i]
        (when (and (<= (int 0) i) (< i cnt)) => (throw (IndexOutOfBoundsException.))
            (let [
                tail-off (unchecked-subtract-int cnt (.alength am tail))
            ]
                (when (< i tail-off) => (.aget am tail (unchecked-subtract-int i tail-off))
                    (loop [i i node root shift shift]
                        (if (zero? shift)
                            (let [a (.array nm node)]
                                (.aget am a (& (>> i shift) (int 0x1f)))
                            )
                            (if (.regular nm node)
                                (let [
                                    a (.array nm node)
                                    idx (& (>> i shift) (int 0x1f))
                                ]
                                    (loop [i i node (aget ^objects a idx) shift (unchecked-subtract-int shift (int 5))]
                                        (let [
                                            a (.array nm node)
                                            idx (& (>> i shift) (int 0x1f))
                                        ]
                                            (if (zero? shift)
                                                (.aget am a idx)
                                                (recur i (aget ^objects a idx) (unchecked-subtract-int shift (int 5)))
                                            )
                                        )
                                    )
                                )
                                (let [
                                    a (.array nm node)
                                    b (ranges nm node)
                                    idx
                                        (loop [j (& (>> i shift) (int 0x1f))]
                                            (when-not (< i (aget b j)) => j
                                                (recur (unchecked-inc-int j))
                                            )
                                        )
                                    i
                                        (if (zero? idx)
                                            (int i)
                                            (unchecked-subtract-int (int i) (aget b (unchecked-dec-int idx)))
                                        )
                                ]
                                    (recur i (aget ^objects a idx) (unchecked-subtract-int shift (int 5)))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (nth [this i not-found]
        (when (and (<= (int 0) i) (< i cnt)) => not-found
            (.nth this i)
        )
    )

    IPersistentCollection
    (cons [this val]
        (if (< (.alength am tail) (int 32))
            (let [
                tail-len (.alength am tail)
                t' (.array am (unchecked-inc-int tail-len))
                _ (System/arraycopy tail 0 t' 0 tail-len)
                _ (.aset am t' tail-len val)
            ]
                (Vector. nm am (unchecked-inc-int cnt) shift root t' _meta -1 -1)
            )
            (let [
                tail-node (.node nm (.edit nm root) tail)
                t'
                    (let [a' (.array am 1)]
                        (.aset am a' 0 val)
                        a'
                    )
            ]
                (if (overflow? nm root shift cnt)
                    (if (.regular nm root)
                        (let [
                            a' (object-array 32)
                            new-root (.node nm (.edit nm root) a')
                        ]
                            (doto a'
                                (aset (int 0) root)
                                (aset (int 1) (.newPath this (.edit nm root) shift tail-node))
                            )
                            (Vector. nm am (unchecked-inc-int cnt) (unchecked-add-int shift (int 5)) new-root t' _meta -1 -1)
                        )
                        (let [
                            a' (object-array 33)
                            b' (ints (int-array 33))
                            new-root (.node nm (.edit nm root) a')
                            root-total-range (aget (ranges nm root) (int 31))
                        ]
                            (doto a'
                                (aset (int 0) root)
                                (aset (int 1) (.newPath this (.edit nm root) shift tail-node))
                                (aset (int 32) b')
                            )
                            (doto b'
                                (aset (int 0) root-total-range)
                                (aset (int 1) (unchecked-add-int root-total-range (int 32)))
                                (aset (int 32) (int 2))
                            )
                            (Vector. nm am (unchecked-inc-int cnt) (unchecked-add-int shift (int 5)) new-root t' _meta -1 -1)
                        )
                    )
                    (Vector. nm am (unchecked-inc-int cnt) shift (.pushTail this shift cnt root tail-node) t' _meta -1 -1)
                )
            )
        )
    )
    (empty [_] (Vector. nm am 0 5 (.empty nm) (.array am 0) _meta -1 -1))
    (equiv [this that]
        (if (instance? IPersistentVector that)
            (and (== cnt (count that))
                (loop [i (int 0)]
                    (or (== i cnt)
                        (and (= (.nth this i) (nth that i))
                            (recur (unchecked-inc-int i))
                        )
                    )
                )
            )
            (and (instance? Sequential that)
                (Util/equiv (seq this) (seq that))
            )
        )
    )

    IPersistentStack
    (peek [this]
        (when (pos? cnt)
            (.nth this (unchecked-dec-int cnt))
        )
    )
    (pop [this]
        (cond
            (zero? cnt)
                (throw (IllegalStateException. "Can't pop empty vector"))
            (== 1 cnt)
                (Vector. nm am 0 5 (.empty nm) (.array am 0) _meta -1 -1)
            (< (int 1) (.alength am tail))
                (let [
                    new-tail (.array am (unchecked-dec-int (.alength am tail)))
                    _ (System/arraycopy tail 0 new-tail 0 (.alength am new-tail))
                ]
                    (Vector. nm am (unchecked-dec-int cnt) shift root new-tail _meta -1 -1)
                )
            :else
                (let [
                    new-tail (.arrayFor this (unchecked-subtract-int cnt (int 2)))
                    root-cnt (.tailoff this)
                    new-root (.popTail this shift root-cnt root)
                ]
                    (cond
                        (nil? new-root)
                            (Vector. nm am (unchecked-dec-int cnt) shift (.empty nm) new-tail _meta -1 -1)
                        (and (< (int 5) shift) (nil? (aget ^objects (.array nm new-root) 1)))
                            (Vector. nm am (unchecked-dec-int cnt) (unchecked-subtract-int shift (int 5)) (aget ^objects (.array nm new-root) 0) new-tail _meta -1 -1)
                        :else
                            (Vector. nm am (unchecked-dec-int cnt) shift new-root new-tail _meta -1 -1)
                    )
                )
        )
    )

    IPersistentVector
    (assocN [this i val]
        (cond
            (and (<= (int 0) i) (< i cnt))
                (let [tail-off (.tailoff this)]
                    (when (<= tail-off i) => (Vector. nm am cnt shift (.doAssoc this shift root i val) tail _meta -1 -1)
                        (let [
                            t' (.array am (.alength am tail))
                            idx (unchecked-subtract-int i tail-off)
                            _ (System/arraycopy tail 0 t' 0 (.alength am tail))
                            _ (.aset am t' idx val)
                        ]
                            (Vector. nm am cnt shift root t' _meta -1 -1)
                        )
                    )
                )
            (== i cnt) (.cons this val)
            :else (throw (IndexOutOfBoundsException.))
        )
    )
    (length [this] (.count this))

    Reversible
    (rseq [this]
        (when (pos? cnt) => nil
            (APersistentVector$RSeq. this (unchecked-dec-int cnt))
        )
    )

    Associative
    (assoc [this k v]
        (when (Util/isInteger k) => (throw (IllegalArgumentException. "Key must be integer"))
            (.assocN this k v)
        )
    )
    (containsKey [this k]
        (and (Util/isInteger k) (<= (int 0) (int k)) (< (int k) cnt))
    )
    (entryAt [this k]
        (when (.containsKey this k) => nil
            (MapEntry. k (.nth this (int k)))
        )
    )

    ILookup
    (valAt [this k not-found]
        (when (Util/isInteger k) => not-found
            (let [i (int k)]
                (when (and (<= (int 0) i) (< i cnt)) => not-found
                    (.nth this i)
                )
            )
        )
    )
    (valAt [this k]
        (.valAt this k nil)
    )

    IFn
    (invoke [this k]
        (when (Util/isInteger k) => (throw (IllegalArgumentException. "Key must be integer"))
            (let [i (int k)]
                (when (and (<= (int 0) i) (< i cnt)) => (throw (IndexOutOfBoundsException.))
                    (.nth this i)
                )
            )
        )
    )
    (applyTo [this args]
        (let [n (RT/boundedLength args 1)]
            (case n
                0 (throw (ArityException. n (.. this (getClass) (getSimpleName))))
                1 (.invoke this (first args))
                2 (throw (ArityException. n (.. this (getClass) (getSimpleName))))
            )
        )
    )

    Seqable
    (seq [this]
        (when-not (zero? cnt) => nil
            (VecSeq. am this (.arrayFor this 0) 0 0 nil -1 -1)
        )
    )

    Sequential

    IEditableCollection
    (asTransient [this]
        (->Transient nm am (identical? am object-am) cnt shift (.editableRoot transient-helper nm am root) (.editableTail transient-helper am tail) (.alength am tail))
    )

    IVecImpl
    (tailoff [_] (unchecked-subtract-int cnt (.alength am tail)))
    (arrayFor [this i]
        (when (and (<= (int 0) i) (< i cnt)) => (throw (IndexOutOfBoundsException.))
            (when (< i (.tailoff this)) => tail
                (loop [i (int i) node root shift shift]
                    (if (zero? shift)
                        (.array nm node)
                        (if (.regular nm node)
                            (loop [node (aget ^objects (.array nm node) (& (>> i shift) (int 0x1f))) shift (unchecked-subtract-int shift (int 5))]
                                (if (zero? shift)
                                    (.array nm node)
                                    (recur (aget ^objects (.array nm node) (& (>> i shift) (int 0x1f))) (unchecked-subtract-int shift (int 5)))
                                )
                            )
                            (let [
                                rngs (ranges nm node)
                                j
                                    (loop [j (& (>> i shift) (int 0x1f))]
                                        (when-not (< i (aget rngs j)) => j
                                            (recur (unchecked-inc-int j))
                                        )
                                    )
                                i
                                    (when (pos? j) => i
                                        (unchecked-subtract-int i (aget rngs (unchecked-dec-int j)))
                                    )
                            ]
                                (recur (int i) (aget ^objects (.array nm node) j) (unchecked-subtract-int shift (int 5)))
                            )
                        )
                    )
                )
            )
        )
    )
    (pushTail [this shift cnt node tail-node]
        (if (.regular nm node)
            (let [
                a' (aclone ^objects (.array nm node))
                ret (.node nm (.edit nm node) a')
            ]
                (loop [node ret shift (int shift)]
                    (let [
                        a' (.array nm node)
                        subidx (& (>> (unchecked-dec-int cnt) shift) (int 0x1f))
                    ]
                        (if (== shift (int 5))
                            (aset ^objects a' subidx tail-node)
                            (if-let [child (aget ^objects a' subidx)]
                                (let [
                                    new-carr (aclone ^objects (.array nm child))
                                    new-child (.node nm (.edit nm root) new-carr)
                                ]
                                    (aset ^objects a' subidx new-child)
                                    (recur new-child (unchecked-subtract-int shift (int 5)))
                                )
                                (aset ^objects a' subidx (.newPath this (.edit nm root) (unchecked-subtract-int shift (int 5)) tail-node))
                            )
                        )
                    )
                )
                ret
            )
            (let [
                a' (aclone ^objects (.array nm node))
                b' (ranges nm node)
                n (unchecked-dec-int (aget b' 32))
                ret (.node nm (.edit nm node) a')
                cret
                    (when-not (== shift (int 5)) => nil
                        (let [
                            child (aget ^objects a' n)
                            ccnt
                                (if (pos? n)
                                    (unchecked-subtract-int (aget b' n) (aget b' (unchecked-dec-int n)))
                                    (aget b' 0)
                                )
                        ]
                            (when-not (== ccnt (<< 1 shift))
                                (.pushTail this (unchecked-subtract-int shift (int 5)) (unchecked-inc-int ccnt) (aget ^objects a' n) tail-node)
                            )
                        )
                    )
            ]
                (if cret
                    (let [
                        _ (aset ^objects a' n cret)
                        _ (aset b' n (unchecked-add-int (aget b' n) (int 32)))
                    ]
                        ret
                    )
                    (let [
                        _ (aset ^objects a' (unchecked-inc-int n) (.newPath this (.edit nm root) (unchecked-subtract-int shift (int 5)) tail-node))
                        _ (aset b' (unchecked-inc-int n) (unchecked-add-int (aget b' n) (int 32)))
                        _ (aset b' 32 (unchecked-inc-int (aget b' 32)))
                    ]
                        ret
                    )
                )
            )
        )
    )
    (popTail [this shift cnt node]
        (if (.regular nm node)
            (let [
                n (& (>> (unchecked-dec-int cnt) (int shift)) (int 0x1f))
            ]
                (cond
                    (< (int 5) (int shift))
                        (let [
                            c' (.popTail this (unchecked-subtract-int (int shift) (int 5)) cnt (aget ^objects (.array nm node) n))
                        ]
                            (when-not (and (nil? c') (zero? n)) => nil
                                (let [
                                    a' (aclone ^objects (.array nm node))
                                    _ (aset a' n c')
                                ]
                                    (.node nm (.edit nm root) a')
                                )
                            )
                        )
                    (zero? n)
                        nil
                    :else
                        (let [
                            a' (aclone ^objects (.array nm node))
                            _ (aset a' n nil)
                        ]
                            (.node nm (.edit nm root) a')
                        )
                )
            )
            (let [
                n (int (& (>> (unchecked-dec-int cnt) (int shift)) (int 0x1f)))
                b (ranges nm node)
                n
                    (int
                        (loop [n n]
                            (if (or (zero? (aget b (unchecked-inc-int n))) (== n (int 31)))
                                n
                                (recur (unchecked-inc-int n))
                            )
                        )
                    )
            ]
                (cond
                    (< (int 5) (int shift))
                        (let [
                            b' (aclone b)
                            c (aget ^objects (.array nm node) n)
                            child-cnt
                                (if (zero? n)
                                    (aget b 0)
                                    (unchecked-subtract-int (aget b n) (aget b (unchecked-dec-int n)))
                                )
                            c' (.popTail this (unchecked-subtract-int (int shift) (int 5)) child-cnt c)
                        ]
                            (cond
                                (and (nil? c') (zero? n))
                                    nil
                                (.regular nm c)
                                    (let [
                                        a' (aclone ^objects (.array nm node))
                                    ]
                                        (aset b' n (unchecked-subtract-int (aget b' n) (int 32)))
                                        (aset a' n c')
                                        (aset a' (int 32) b')
                                        (when (nil? c')
                                            (aset b' 32 (unchecked-dec-int (aget b' 32)))
                                        )
                                        (.node nm (.edit nm root) a')
                                    )
                                :else
                                    (let [
                                        diff (unchecked-subtract-int (int (last-range nm c)) (if c' (last-range nm c') 0))
                                        a' (aclone ^objects (.array nm node))
                                    ]
                                        (aset b' n (unchecked-subtract-int (aget b' n) diff))
                                        (aset a' n c')
                                        (aset a' (int 32) b')
                                        (when (nil? c')
                                            (aset b' 32 (unchecked-dec-int (aget b' 32)))
                                        )
                                        (.node nm (.edit nm root) a')
                                    )
                            )
                        )
                    (zero? n)
                        nil
                    :else
                        (let [
                            a' (aclone ^objects (.array nm node))
                            b' (aclone b)
                        ]
                            (aset a' n nil)
                            (aset a' (int 32) b')
                            (aset b' n 0)
                            (aset b' 32 (unchecked-dec-int (aget b' (int 32))))
                            (.node nm (.edit nm root) a')
                        )
                )
            )
        )
    )

    (newPath [this ^AtomicReference edit ^int shift node]
        (if (== (.alength am tail) (int 32))
            (let [shift (int shift)]
                (loop [s (int 0) node node]
                    (when-not (== s shift) => node
                        (let [
                            a' (object-array 32)
                            ret (.node nm edit a')
                        ]
                            (aset a' 0 node)
                            (recur (unchecked-add-int s (int 5)) ret)
                        )
                    )
                )
            )
            (let [shift (int shift)]
                (loop [s (int 0) node node]
                    (when-not (== s shift) => node
                        (let [
                            a' (object-array 33)
                            b' (int-array 33)
                            ret (.node nm edit a')
                        ]
                            (aset a' 0 node)
                            (aset a' 32 b')
                            (aset b' 32 1)
                            (aset b' 0 (.alength am tail))
                            (recur (unchecked-add-int s (int 5)) ret)
                        )
                    )
                )
            )
        )
    )

    (doAssoc [this shift node i val]
        (if (.regular nm node)
            (let [
                node (.clone nm am shift node)
            ]
                (loop [shift (int shift) node node]
                    (when-not (zero? shift) => (.aset am (.array nm node) (& i (int 0x1f)) val)
                        (let [
                            a' (.array nm node)
                            n (& (>> i shift) (int 0x1f))
                            c (.clone nm am shift (aget ^objects a' n))
                        ]
                            (aset ^objects a' n c)
                            (recur (unchecked-subtract-int shift (int 5)) c)
                        )
                    )
                )
                node
            )
            (let [
                a' (aclone ^objects (.array nm node))
                b (ranges nm node)
                n (& (>> i shift) (int 0x1f))
                n
                    (loop [n n]
                        (when-not (< i (aget b n)) => n
                            (recur (unchecked-inc-int n))
                        )
                    )
                i
                    (when-not (zero? n) => i
                        (unchecked-subtract-int i (aget b (unchecked-dec-int n)))
                    )
            ]
                (aset a' n (.doAssoc this (unchecked-subtract-int (int shift) (int 5)) (aget a' n) i val))
                (.node nm (.edit nm node) a')
            )
        )
    )

    IKVReduce
    (kv-reduce [this f init]
        (loop [i (int 0) j (int 0) init init a (.arrayFor this i) n (unchecked-dec-int (.alength am a)) step (unchecked-inc-int n)]
            (let [init (f init (unchecked-add-int i j) (.aget am a j))]
                (if (reduced? init)
                    @init
                    (if (< j n)
                        (recur i (unchecked-inc-int j) init a n step)
                        (let [i (unchecked-add-int i step)]
                            (when (< i cnt) => init
                                (let [
                                    a (.arrayFor this i)
                                    n (.alength am a)
                                ]
                                    (recur i (int 0) init a (unchecked-dec-int n) n)
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    PSliceableVector
    (slicev [this start end]
        (let [
            start (int start)
            end (int end)
            new-cnt (unchecked-subtract-int end start)
        ]
            (cond
                (or (neg? start) (< cnt end)) (throw (IndexOutOfBoundsException.))
                (== start end)                (empty this) ;; NB. preserves metadata
                (< end start)                 (throw (IllegalStateException. "start index greater than end index"))
                :else
                    (let [tail-off (.tailoff this)]
                        (if (<= tail-off start)
                            (let [new-tail (.array am new-cnt)]
                                (System/arraycopy tail (unchecked-subtract-int start tail-off) new-tail 0 new-cnt)
                                (Vector. nm am new-cnt (int 5) (.empty nm) new-tail _meta -1 -1)
                            )
                            (let [
                                tail-cut? (< tail-off end)
                                new-root (if tail-cut? root (slice-right nm am root shift end))
                                new-root (if (zero? start) new-root (slice-left nm am new-root shift start (min end tail-off)))
                                new-tail
                                    (if tail-cut?
                                        (let [
                                            new-len (unchecked-subtract-int end tail-off)
                                            new-tail (.array am new-len)
                                            _ (System/arraycopy tail 0 new-tail 0 new-len)
                                        ]
                                            new-tail
                                        )
                                        (.arrayFor (Vector. nm am new-cnt shift new-root (.array am 0) nil -1 -1) (unchecked-dec-int new-cnt))
                                    )
                                new-root
                                    (when-not tail-cut? => new-root
                                        (.popTail (Vector. nm am new-cnt shift new-root (.array am 0) nil -1 -1) shift new-cnt new-root)
                                    )
                            ]
                                (when (some? new-root) => (Vector. nm am new-cnt 5 (.empty nm) new-tail _meta -1 -1)
                                    (loop-when-recur [r new-root s (int shift)]
                                                     (and (< (int 5) s) (nil? (aget ^objects (.array nm r) 1)))
                                                     [(aget ^objects (.array nm r) 0) (unchecked-subtract-int s (int 5))]
                                                  => (Vector. nm am new-cnt s r new-tail _meta -1 -1)
                                    )
                                )
                            )
                        )
                    )
            )
        )
    )

    PSpliceableVector
    (splicev [this that] (splice-rrbts nm am this (as-rrbt that)))

    AsRRBT
    (as-rrbt [this] this)

    java.lang.Comparable
    (compareTo [this that]
        (when-not (identical? this that) => 0
            (let [
                ^IPersistentVector v (cast IPersistentVector that)
                vcnt (.count v)
            ]
                (cond
                    (< cnt vcnt) -1
                    (> cnt vcnt) 1
                    :else
                        (loop [i (int 0)]
                            (when-not (== i cnt) => 0
                                (let [comp (Util/compare (.nth this i) (.nth v i))]
                                    (when (zero? comp) => comp
                                        (recur (unchecked-inc-int i))
                                    )
                                )
                            )
                        )
                )
            )
        )
    )
)

(extend-protocol AsRRBT
    Vec
    (as-rrbt [^Vec this]
        (Vector. primitive-nm (.-am this) (.-cnt this) (.-shift this) (.-root this) (.-tail this) (.-_meta this) -1 -1)
    )

    PersistentVector
    (as-rrbt [^PersistentVector this]
        (Vector. object-nm object-am (count this) (.-shift this) (.-root this) (.-tail this) (meta this) -1 -1)
    )

    APersistentVector$SubVector
    (as-rrbt [^APersistentVector$SubVector this]
        (slicev (as-rrbt (.-v this)) (.-start this) (.-end this))
    )

    java.util.Map$Entry
    (as-rrbt [^java.util.Map$Entry this]
        (as-rrbt [(.getKey this) (.getValue this)])
    )
)

(defn shift-from-to [^NodeManager nm node from to]
    (cond
        (== from to)
            node
        (.regular nm node)
            (recur nm
                (.node nm (.edit nm node) (doto (object-array 32) (aset 0 node)))
                (unchecked-add-int (int 5) (int from))
                to
            )
        :else
            (recur nm
                (.node nm (.edit nm node) (doto (object-array 33) (aset 0 node) (aset 32 (ints (doto (int-array 33) (aset 0 (int (last-range nm node))) (aset 32 (int 1)))))))
                (unchecked-add-int (int 5) (int from))
                to
            )
    )
)

(defn pair ^"[Ljava.lang.Object;" [x y]
    (doto (object-array 2) (aset 0 x) (aset 1 y))
)

(defn slot-count [^NodeManager nm ^ArrayManager am node shift]
    (let [a (.array nm node)]
        (cond
            (zero? shift) (.alength am a)
            (.regular nm node) (index-of-nil a)
            :else (aget (ranges nm node) 32)
        )
    )
)

(defn subtree-branch-count [^NodeManager nm ^ArrayManager am node shift]
    ;; NB. positive shifts only
    (let [
        a (.array nm node)
    ]
        (if (.regular nm node)
            (loop-when [i 0 sbc 0] (< i 32) => sbc
                (if-let [child (aget ^objects a i)]
                    (recur (inc i) (+ sbc (long (slot-count nm am child (- shift 5)))))
                    sbc
                )
            )
            (let [n (aget (ranges nm node) 32)]
                (loop-when [i 0 sbc 0] (< i n) => sbc
                    (recur (inc i) (+ sbc (long (slot-count nm am (aget ^objects a i) (- shift 5)))))
                )
            )
        )
    )
)

(defn leaf-seq [^NodeManager nm arr]
    (mapcat #(.array nm %) (take (index-of-nil arr) arr))
)

(defn rebalance-leaves [^NodeManager nm ^ArrayManager am n1 cnt1 n2 cnt2 ^Box transferred-leaves]
    (let [
        slc1 (slot-count nm am n1 5)
        slc2 (slot-count nm am n2 5)
        a (+ slc1 slc2)
        sbc1 (subtree-branch-count nm am n1 5)
        sbc2 (subtree-branch-count nm am n2 5)
        p (+ sbc1 sbc2)
        e (- a (inc (quot (dec p) 32)))
    ]
        (cond
            (<= e max-extra-search-steps)
                (pair n1 n2)
            (<= (+ sbc1 sbc2) 1024)
                (let [
                    reg? (zero? (mod p 32))
                    a' (object-array (if reg? 32 33))
                    n' (.node nm nil a')
                ]
                    (loop [i 0 bs (partition-all 32 (concat (leaf-seq nm (.array nm n1)) (leaf-seq nm (.array nm n2))))]
                        (when-first [block bs]
                            (let [a (.array am (count block))]
                                (loop-when-recur [i 0 s (seq block)] s [(inc i) (next s)]
                                    (.aset am a i (first s))
                                )
                                (aset a' i (.node nm nil a))
                                (recur (inc i) (next bs))
                            )
                        )
                    )
                    (when-not reg?
                        (aset a' 32 (regular-ranges 5 p))
                    )
                    (set! (.-val transferred-leaves) sbc2)
                    (pair n' nil)
                )
            :else
                (let [
                    reg? (zero? (mod p 32))
                    a1' (object-array 32)
                    a2' (object-array (if reg? 32 33))
                    n1' (.node nm nil a1')
                    n2' (.node nm nil a2')
                ]
                    (loop [i 0 bs (partition-all 32 (concat (leaf-seq nm (.array nm n1)) (leaf-seq nm (.array nm n2))))]
                        (when-first [block bs]
                            (let [a (.array am (count block))]
                                (loop-when-recur [i 0 s (seq block)] s [(inc i) (next s)]
                                    (.aset am a i (first s))
                                )
                                (if (< i 32)
                                    (aset a1' i (.node nm nil a))
                                    (aset a2' (- i 32) (.node nm nil a))
                                )
                                (recur (inc i) (next bs))
                            )
                        )
                    )
                    (when-not reg?
                        (aset a2' 32 (regular-ranges 5 (- p 1024)))
                    )
                    (set! (.-val transferred-leaves) (- 1024 sbc1))
                    (pair n1' n2')
                )
        )
    )
)

(defn child-seq [^NodeManager nm node shift cnt]
    (let [
        f'cseq
            (fn [c r]
                (let [
                    a (.array nm c)
                    b (if (.regular nm c) (ints (regular-ranges (- shift 5) r)) (ranges nm c))
                    n (if b (aget b 32) (index-of-nil a))
                ]
                    (map list (take n a) (take n (map - b (cons 0 b))))
                )
            )
        a (.array nm node)
        b (if (.regular nm node) (ints (regular-ranges shift cnt)) (ranges nm node))
        n (if b (aget b 32) (index-of-nil a))
    ]
        (mapcat f'cseq (take n a) (take n (map - b (cons 0 b))))
    )
)

(defn rebalance [^NodeManager nm ^ArrayManager am shift n1 cnt1 n2 cnt2 ^Box transferred-leaves]
    (when (some? n2) => (pair n1 nil)
        (let [
            slc1 (slot-count nm am n1 shift) sbc1 (subtree-branch-count nm am n1 shift)
            slc2 (slot-count nm am n2 shift) sbc2 (subtree-branch-count nm am n2 shift)
        ]
            (cond
                (<= (- (+ slc1 slc2) (inc (quot (dec (+ sbc1 sbc2)) 32))) max-extra-search-steps)
                    (pair n1 n2)
                (<= (+ sbc1 sbc2) 1024)
                    (let [
                        a' (object-array 33)
                        b' (int-array 33)
                        n' (.node nm nil a')
                    ]
                        (loop [i 0 bs (partition-all 32 (concat (child-seq nm n1 shift cnt1) (child-seq nm n2 shift cnt2)))]
                            (when-first [block bs]
                                (let [
                                    a (object-array 33)
                                    r (int-array 33)
                                ]
                                    (aset a 32 r)
                                    (aset r 32 (count block))
                                    (loop [i 0 o (int 0) s (seq block)]
                                        (when-first [[gc gcr] s]
                                            (aset ^objects a i gc)
                                            (aset r i (unchecked-add-int o (int gcr)))
                                            (recur (inc i) (unchecked-add-int o (int gcr)) (next s))
                                        )
                                    )
                                    (aset ^objects a' i (.node nm nil a))
                                    (aset b' i (+ (aget r (dec (aget r 32))) (if (pos? i) (aget b' (dec i)) (int 0))))
                                    (aset b' 32 (inc i))
                                    (recur (inc i) (next bs))
                                )
                            )
                        )
                        (aset a' 32 b')
                        (set! (.-val transferred-leaves) cnt2)
                        (pair n' nil)
                    )
                :else
                    (let [
                        a1' (object-array 33) b1' (int-array 33) n1' (.node nm nil a1')
                        a2' (object-array 33) b2' (int-array 33) n2' (.node nm nil a2')
                    ]
                        (loop [i 0 bs (partition-all 32 (concat (child-seq nm n1 shift cnt1) (child-seq nm n2 shift cnt2)))]
                            (when-first [block bs]
                                (let [
                                    a (object-array 33)
                                    r (int-array 33)
                                ]
                                    (aset a 32 r)
                                    (aset r 32 (count block))
                                    (loop [i 0 o (int 0) gcs (seq block)]
                                        (when-first [[gc gcr] gcs]
                                            (aset a i gc)
                                            (aset r i (unchecked-add-int o (int gcr)))
                                            (recur (inc i) (unchecked-add-int o (int gcr)) (next gcs))
                                        )
                                    )
                                    (when (and (< i 32) (< sbc1 (+ (* i 32) (count block))))
                                        (let [
                                            tbs (- (+ (* i 32) (count block)) sbc1)
                                            li (dec (aget r 32))
                                            d (if (< tbs 32) (- (aget r li) (aget r (- li tbs))) (aget r li))
                                        ]
                                            (set! (.-val transferred-leaves) (+ (.-val transferred-leaves) d))
                                        )
                                    )
                                    (let [
                                        a' (if (< i 32) a1' a2')
                                        r' (if (< i 32) b1' b2')
                                        k (mod i 32)
                                    ]
                                        (aset ^objects a' k (.node nm nil a))
                                        (aset r' k (+ (aget r (dec (aget r 32))) (if (pos? k) (aget r' (dec k)) (int 0))))
                                        (aset r' 32 (int (inc k)))
                                    )
                                    (recur (inc i) (next bs))
                                )
                            )
                        )
                        (aset a1' 32 b1')
                        (aset a2' 32 b2')
                        (pair n1' n2')
                    )
            )
        )
    )
)

(defn zippath [^NodeManager nm ^ArrayManager am shift n1 cnt1 n2 cnt2 ^Box transferred-leaves]
    (if (== shift 5)
        (rebalance-leaves nm am n1 cnt1 n2 cnt2 transferred-leaves)
        (let [
            c1 (last-child nm n1)
            c2 (first-child nm n2)
            ccnt1
                (if (.regular nm n1)
                    (let [m (mod cnt1 (<< 1 shift))]
                        (if (zero? m) (<< 1 shift) m)
                    )
                    (let [
                        b (ranges nm n1)
                        n (dec (aget b 32))
                    ]
                        (if (zero? n) (aget b 0) (- (aget b n) (aget b (dec n))))
                    )
                )
            ccnt2
                (if (.regular nm n2)
                    (let [m (mod cnt2 (<< 1 shift))]
                        (if (zero? m) (<< 1 shift) m)
                    )
                    (aget (ranges nm n2) 0)
                )
            next-transferred-leaves (Box. 0)
            [new-c1 new-c2] (zippath nm am (- shift 5) c1 ccnt1 c2 ccnt2 next-transferred-leaves)
            d (.-val next-transferred-leaves)
        ]
            (set! (.-val transferred-leaves) (+ (.-val transferred-leaves) d))
            (rebalance nm am shift
                (if (identical? c1 new-c1) n1 (replace-rightmost-child nm shift n1 new-c1 d))
                (+ cnt1 d)
                (if new-c2 (if (identical? c2 new-c2) n2 (replace-leftmost-child nm shift n2 cnt2 new-c2 d)) (remove-leftmost-child nm shift n2))
                (- cnt2 d)
                transferred-leaves
            )
        )
    )
)

(defn squash-nodes [^NodeManager nm shift n1 cnt1 n2 cnt2]
    (let [
        a1 (.array nm n1) i1 (index-of-nil a1)
        a2 (.array nm n2) i2 (index-of-nil a2)
        slots (concat (take i1 a1) (take i2 a2))
    ]
        (when (<= (count slots) 32) => (pair n1 n2)
            (let [
                rngs1 (take i1 (if (.regular nm n1) (regular-ranges shift cnt1) (ranges nm n1)))
                rngs2 (take i2 (if (.regular nm n2) (regular-ranges shift cnt2) (ranges nm n2)))
                rngs2 (let [r (last rngs1)] (map #(+ % r) rngs2))
                rngs (concat rngs1 rngs2)
                a' (object-array 33)
                b' (int-array 33)
            ]
                (aset a' 32 b')
                (loop-when-recur [i 0 s (seq slots)] s [(inc i) (next s)]
                    (aset a' i (first s))
                )
                (loop-when-recur [i 0 s (seq rngs)] s [(inc i) (next s)] => (aset b' 32 i)
                    (aset b' i (int (first s)))
                )
                (pair (.node nm nil a') nil)
            )
        )
    )
)

(defn splice-rrbts [^NodeManager nm ^ArrayManager am ^Vector v1 ^Vector v2]
    (cond
        (zero? (count v1)) v2
        (< (count v2) rrbt-concat-threshold) (into v1 v2)
        :else
            (let [
                s1 (.-shift v1)
                s2 (.-shift v2)
                r1 (.-root v1)
                o? (overflow? nm r1 s1 (+ (count v1) (- 32 (.alength am (.-tail v1)))))
                r1
                    (when o? => (fold-tail nm am r1 s1 (.tailoff v1) (.-tail v1))
                        (let [
                            tail (.-tail v1)
                            tail-node (.node nm nil tail)
                            reg? (and (.regular nm r1) (== (.alength am tail) 32))
                            arr (object-array (if reg? 32 33))
                        ]
                            (aset arr 0 r1)
                            (aset arr 1 (new-path nm am s1 tail-node))
                            (when-not reg?
                                (let [rngs (int-array 33)]
                                    (aset rngs 32 2)
                                    (aset rngs 0 (- (count v1) (.alength am tail)))
                                    (aset rngs 1 (count v1))
                                    (aset arr 32 rngs)
                                )
                            )
                            (.node nm nil arr)
                        )
                    )
                s1 (if o? (+ s1 5) s1)
                r2 (.-root v2)
                s (max s1 s2)
                r1 (shift-from-to nm r1 s1 s)
                r2 (shift-from-to nm r2 s2 s)
                transferred-leaves (Box. 0)
                [n1 n2] (zippath nm am s r1 (count v1) r2 (- (count v2) (.alength am (.-tail v2))) transferred-leaves)
                d (.-val transferred-leaves)
                ncnt1 (+ (count v1) d)
                ncnt2 (- (count v2) (.alength am (.-tail v2)) d)
                [n1 n2] (if (identical? n2 r2) (squash-nodes nm s n1 ncnt1 n2 ncnt2) (object-array (list n1 n2)))
                ncnt1 (if n2 (int ncnt1) (unchecked-add-int (int ncnt1) (int ncnt2)))
                ncnt2 (if n2 (int ncnt2) (int 0))
            ]
                (if n2
                    (let [
                        arr (object-array 33)
                        new-root (.node nm nil arr)
                    ]
                        (aset arr 0 n1)
                        (aset arr 1 n2)
                        (aset arr 32 (doto (int-array 33) (aset 0 ncnt1) (aset 1 (+ ncnt1 ncnt2)) (aset 32 2)))
                        (Vector. nm am (+ (count v1) (count v2)) (+ s 5) new-root (.-tail v2) nil -1 -1)
                    )
                    (loop [r n1 s (int s)]
                        (if (and (< (int 5) s) (nil? (aget ^objects (.array nm r) 1)))
                            (recur (aget ^objects (.array nm r) 0) (unchecked-subtract-int s (int 5)))
                            (Vector. nm am (+ (count v1) (count v2)) s r (.-tail v2) nil -1 -1)
                        )
                    )
                )
            )
    )
)

(defn array-copy [^ArrayManager am from i to j len]
    (loop-when-recur [i (int i) j (int j) len (int len)] (pos? len) [(unchecked-inc-int i) (unchecked-inc-int j) (unchecked-dec-int len)]
        (.aset am to j (.aget am from i))
    )
)

(deftype Transient [^NodeManager nm, ^ArrayManager am, ^boolean objects?, ^:unsynchronized-mutable ^int cnt, ^:unsynchronized-mutable ^int shift, ^:unsynchronized-mutable root, ^:unsynchronized-mutable tail, ^:unsynchronized-mutable ^int tidx]
    Counted
    (count [this]
        (.ensureEditable transient-helper nm root)
        cnt
    )

    Indexed
    (nth [this i]
        (.ensureEditable transient-helper nm root)
        (when (and (<= (int 0) i) (< i cnt)) => (throw (IndexOutOfBoundsException.))
            (let [
                tail-off (unchecked-subtract-int cnt (.alength am tail))
            ]
                (when (< i tail-off) => (.aget am tail (unchecked-subtract-int i tail-off))
                    (loop [i i node root shift shift]
                        (when-not (zero? shift) => (.aget am (.array nm node) (& (>> i shift) (int 0x1f)))
                            (if (.regular nm node)
                                (let [
                                    a (.array nm node)
                                    idx (& (>> i shift) (int 0x1f))
                                ]
                                    (loop [i i node (aget ^objects a idx) shift (unchecked-subtract-int shift (int 5))]
                                        (let [
                                            a (.array nm node)
                                            idx (& (>> i shift) (int 0x1f))
                                        ]
                                            (if (zero? shift)
                                                (.aget am a idx)
                                                (recur i (aget ^objects a idx) (unchecked-subtract-int shift (int 5)))
                                            )
                                        )
                                    )
                                )
                                (let [
                                    a (.array nm node)
                                    b (ranges nm node)
                                    idx
                                        (loop [j (& (>> i shift) (int 0x1f))]
                                            (when-not (< i (aget b j)) => j
                                                (recur (unchecked-inc-int j))
                                            )
                                        )
                                    i
                                        (when-not (zero? idx) => (int i)
                                            (unchecked-subtract-int (int i) (aget b (unchecked-dec-int idx)))
                                        )
                                ]
                                    (recur i (aget ^objects a idx) (unchecked-subtract-int shift (int 5)))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (nth [this i not-found]
        (.ensureEditable transient-helper nm root)
        (when (and (<= (int 0) i) (< i cnt)) => not-found
            (.nth this i)
        )
    )

    ILookup
    (valAt [this k not-found]
        (.ensureEditable transient-helper nm root)
        (when (Util/isInteger k) => not-found
            (let [i (int k)]
                (when (and (<= (int 0) i) (< i cnt)) => not-found
                    (.nth this i)
                )
            )
        )
    )
    (valAt [this k]
        (.valAt this k nil)
    )

    IFn
    (invoke [this k]
        (.ensureEditable transient-helper nm root)
        (when (Util/isInteger k) => (throw (IllegalArgumentException. "Key must be integer"))
            (let [i (int k)]
                (when (and (<= (int 0) i) (< i cnt)) => (throw (IndexOutOfBoundsException.))
                    (.nth this i)
                )
            )
        )
    )
    (applyTo [this args]
        (.ensureEditable transient-helper nm root)
        (let [n (RT/boundedLength args 1)]
            (case n
                0 (throw (ArityException. n (.. this (getClass) (getSimpleName))))
                1 (.invoke this (first args))
                2 (throw (ArityException. n (.. this (getClass) (getSimpleName))))
            )
        )
    )

    ITransientCollection
    (conj [this val]
        (.ensureEditable transient-helper nm root)
        (if (< tidx 32)
            (do
                (.aset am tail tidx val)
                (set! cnt (unchecked-inc-int cnt))
                (set! tidx (unchecked-inc-int tidx))
                this
            )
            (let [
                tail-node (.node nm (.edit nm root) tail)
                new-tail (.array am 32)
            ]
                (.aset am new-tail 0 val)
                (set! tail new-tail)
                (set! tidx (int 1))
                (if (overflow? nm root shift cnt)
                    (if (.regular nm root)
                        (let [
                            a' (object-array 32)
                        ]
                            (doto a'
                                (aset 0 root)
                                (aset 1 (.newPath transient-helper nm am tail (.edit nm root) shift tail-node))
                            )
                            (set! root (.node nm (.edit nm root) a'))
                            (set! shift (unchecked-add-int shift (int 5)))
                            (set! cnt (unchecked-inc-int cnt))
                            this
                        )
                        (let [
                            a' (object-array 33)
                            b' (int-array 33)
                            new-root (.node nm (.edit nm root) a')
                            root-total-range (aget (ranges nm root) 31)
                        ]
                            (doto a'
                                (aset 0 root)
                                (aset 1 (.newPath transient-helper nm am tail (.edit nm root) shift tail-node))
                                (aset 32 b')
                            )
                            (doto b'
                                (aset 0 root-total-range)
                                (aset 1 (unchecked-add-int root-total-range (int 32)))
                                (aset 32 2)
                            )
                            (set! root new-root)
                            (set! shift (unchecked-add-int shift (int 5)))
                            (set! cnt (unchecked-inc-int cnt))
                            this
                        )
                    )
                    (let [
                        new-root (.pushTail transient-helper nm am shift cnt (.edit nm root) root tail-node)
                    ]
                        (set! root new-root)
                        (set! cnt (unchecked-inc-int cnt))
                        this
                    )
                )
            )
        )
    )
    (persistent [this]
        (.ensureEditable transient-helper nm root)
        (.set (.edit nm root) nil)
        (let [trimmed-tail (.array am tidx)]
            (array-copy am tail 0 trimmed-tail 0 tidx)
            (Vector. nm am cnt shift root trimmed-tail nil -1 -1)
        )
    )

    ITransientVector
    (assocN [this i val]
        (.ensureEditable transient-helper nm root)
        (cond
            (and (<= 0 i) (< i cnt))
                (let [tail-off (unchecked-subtract-int cnt tidx)]
                    (if (<= tail-off i)
                        (.aset am tail (unchecked-subtract-int i tail-off) val)
                        (set! root (.doAssoc transient-helper nm am shift (.edit nm root) root i val))
                    )
                    this
                )
            (== i cnt) (.conj this val)
            :else (throw (IndexOutOfBoundsException.))
        )
    )
    (pop [this]
        (.ensureEditable transient-helper nm root)
        (cond
            (zero? cnt) (throw (IllegalStateException. "Can't pop empty vector"))
            (== 1 cnt)
                (do
                    (set! cnt (int 0))
                    (set! tidx (int 0))
                    (when objects?
                        (.aset am tail 0 nil)
                    )
                    this
                )
            (< 1 tidx)
                (do
                    (set! cnt (unchecked-dec-int cnt))
                    (set! tidx (unchecked-dec-int tidx))
                    (when objects?
                        (.aset am tail tidx nil)
                    )
                    this
                )
            :else
                (let [
                    new-tail-base (.arrayFor this (unchecked-subtract-int cnt (int 2)))
                    new-tail (.aclone am new-tail-base)
                    new-tidx (.alength am new-tail-base)
                    new-root (.popTail transient-helper nm am shift cnt (.edit nm root) root)
                ]
                    (cond
                        (nil? new-root)
                            (do
                                (set! cnt (unchecked-dec-int cnt))
                                (set! root (.ensureEditable transient-helper nm am (.edit nm root) (.empty nm) 5))
                                (set! tail new-tail)
                                (set! tidx new-tidx)
                                this
                            )
                        (and (< 5 shift) (nil? (aget ^objects (.array nm new-root) 1)))
                            (do
                                (set! cnt (unchecked-dec-int cnt))
                                (set! shift (unchecked-subtract-int shift (int 5)))
                                (set! root (aget ^objects (.array nm new-root) 0))
                                (set! tail new-tail)
                                (set! tidx new-tidx)
                                this
                            )
                        :else
                            (do
                                (set! cnt (unchecked-dec-int cnt))
                                (set! root new-root)
                                (set! tail new-tail)
                                (set! tidx new-tidx)
                                this
                            )
                    )
                )
        )
    )

    ITransientAssociative
    (assoc [this k v] (.assocN this k v))

    ;; temporary kludge
    IVecImpl
    (tailoff [_] (unchecked-subtract-int cnt tidx))
    (arrayFor [this i]
        (when (and (<= (int 0) i) (< i cnt)) => (throw (IndexOutOfBoundsException.))
            (when (< i (.tailoff this)) => tail
                (loop [i (int i) node root shift shift]
                    (when-not (zero? shift) => (.array nm node)
                        (if (.regular nm node)
                            (loop [node (aget ^objects (.array nm node) (& (>> i shift) (int 0x1f))) shift (unchecked-subtract-int shift (int 5))]
                                (when-not (zero? shift) => (.array nm node)
                                    (recur (aget ^objects (.array nm node) (& (>> i shift) (int 0x1f))) (unchecked-subtract-int shift (int 5)))
                                )
                            )
                            (let [
                                b (ranges nm node)
                                n
                                    (loop [n (& (>> i shift) (int 0x1f))]
                                        (when-not (< i (aget b n)) => n
                                            (recur (unchecked-inc-int n))
                                        )
                                    )
                                i
                                    (when (pos? n) => i
                                        (unchecked-subtract-int i (aget b (unchecked-dec-int n)))
                                    )
                            ]
                                (recur (int i) (aget ^objects (.array nm node) n) (unchecked-subtract-int shift (int 5)))
                            )
                        )
                    )
                )
            )
        )
    )
)

(definterface ITransientHelper
    (editableRoot [^NodeManager nm, ^ArrayManager am, root])
    (editableTail [^ArrayManager am, tail])
    (ensureEditable [^NodeManager nm, root])
    (ensureEditable [^NodeManager nm, ^ArrayManager am, ^AtomicReference root-edit, current-node, ^int shift])
    (pushTail [^NodeManager nm, ^ArrayManager am, ^int shift, ^int cnt, ^AtomicReference root-edit, current-node, tail-node])
    (popTail [^NodeManager nm, ^ArrayManager am, ^int shift, ^int cnt, ^AtomicReference root-edit, current-node])
    (doAssoc [^NodeManager nm, ^ArrayManager am, ^int shift, ^AtomicReference root-edit, current-node, ^int i, val])
    (newPath [^NodeManager nm, ^ArrayManager am, tail, ^AtomicReference edit, ^int shift, current-node])
)

(def ^ITransientHelper transient-helper
    (reify ITransientHelper
        (editableRoot [this nm am root]
            (.node nm (AtomicReference. (Thread/currentThread)) (clojure.core/aclone ^objects (.array nm root)))
        )
        (editableTail [this am tail]
            (let [a (.array am 32)]
                (System/arraycopy tail 0 a 0 (.alength am tail))
                a
            )
        )
        (ensureEditable [this nm root]
            (let [owner (->> root (.edit nm) (.get))]
                (cond
                    (identical? owner (Thread/currentThread)) nil
                    (not (nil? owner)) (throw (IllegalAccessError. "Transient used by non-owner thread"))
                    :else (throw (IllegalAccessError. "Transient used after persistent! call"))
                )
            )
        )
        (ensureEditable [this nm am root-edit current-node shift]
            (cond
                (identical? root-edit (.edit nm current-node)) current-node
                (zero? shift) (.node nm root-edit (.aclone am (.array nm current-node)))
                :else
                    (let [
                        a' (aclone ^objects (.array nm current-node))
                    ]
                        (when (== 33 (alength ^objects a'))
                            (aset a' 32 (aclone (ints (aget ^objects a' 32))))
                        )
                        (.node nm root-edit a')
                    )
            )
        )
        (pushTail [this nm am shift cnt root-edit current-node tail-node]
            (let [
                ret (.ensureEditable this nm am root-edit current-node shift)
            ]
                (if (.regular nm ret)
                    (do
                        (loop [e ret shift shift]
                            (let [
                                a (.array nm e)
                                n (& (>> (dec cnt) shift) 0x1f)
                            ]
                                (when-not (== shift 5) => (aset ^objects a n tail-node)
                                    (let [child (aget ^objects a n)]
                                        (when (some? child) => (aset ^objects a n (.newPath this nm am (.array nm tail-node) root-edit (unchecked-subtract-int shift 5) tail-node))
                                            (let [
                                                editable-child (.ensureEditable this nm am root-edit child (unchecked-subtract-int shift 5))
                                                _ (aset ^objects a n editable-child)
                                            ]
                                                (recur editable-child (- shift 5))
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        ret
                    )
                    (let [
                        arr (.array nm ret)
                        rngs (ranges nm ret)
                        li (unchecked-dec-int (aget rngs 32))
                        cret
                            (when-not (== shift 5) => nil
                                (let [
                                    child (.ensureEditable this nm am root-edit (aget ^objects arr li) (unchecked-subtract-int shift 5))
                                    ccnt
                                        (when (pos? li) => (aget rngs 0)
                                            (unchecked-subtract-int (aget rngs li) (aget rngs (unchecked-dec-int li)))
                                        )
                                ]
                                    (when-not (== ccnt (<< 1 shift))
                                        (.pushTail this nm am (unchecked-subtract-int shift 5) (unchecked-inc-int ccnt) root-edit child tail-node)
                                    )
                                )
                            )
                    ]
                        (if cret
                            (let [
                                _ (aset ^objects arr li cret)
                                _ (aset rngs li (unchecked-add-int (aget rngs li) 32))
                            ]
                                ret
                            )
                            (let [
                                _ (aset ^objects arr (inc li) (.newPath this nm am (.array nm tail-node) root-edit (unchecked-subtract-int shift 5) tail-node))
                                _ (aset rngs (unchecked-inc-int li) (unchecked-add-int (aget rngs li) 32))
                                _ (aset rngs 32 (unchecked-inc-int (aget rngs 32)))
                            ]
                                ret
                            )
                        )
                    )
                )
            )
        )
        (popTail [this nm am shift cnt root-edit current-node]
            (let [
                ret (.ensureEditable this nm am root-edit current-node shift)
            ]
                (if (.regular nm ret)
                    (let [n (& (>> (unchecked-dec-int cnt) shift) 0x1f)]
                        (cond
                            (< 5 shift)
                                (let [
                                    child (.popTail this nm am (unchecked-subtract-int shift 5) cnt root-edit (aget ^objects (.array nm ret) n))
                                ]
                                    (when-not (and (nil? child) (zero? n)) => nil
                                        (aset ^objects (.array nm ret) n child)
                                        ret
                                    )
                                )
                            (zero? n)
                                nil
                            :else
                                (do
                                    (aset ^objects (.array nm ret) n nil)
                                    ret
                                )
                        )
                    )
                    (let [
                        b (ranges nm ret)
                        n (& (>> (unchecked-dec-int cnt) shift) 0x1f)
                        n
                            (loop [n n]
                                (when-not (or (zero? (aget b (unchecked-inc-int n))) (== n 31)) => n
                                    (recur (unchecked-inc-int n))
                                )
                            )
                    ]
                        (cond
                            (< 5 shift)
                                (let [
                                    c (aget ^objects (.array nm ret) n)
                                    child-cnt
                                        (when-not (zero? n) => (aget b 0)
                                            (unchecked-subtract-int (aget b n) (aget b (unchecked-dec-int n)))
                                        )
                                    c' (.popTail this nm am (unchecked-subtract-int n 5) child-cnt root-edit c)
                                ]
                                    (cond
                                        (and (nil? c') (zero? n))
                                            nil
                                        (.regular nm c)
                                            (do
                                                (aset b n (unchecked-subtract-int (aget b n) 32))
                                                (aset ^objects (.array nm ret) n c')
                                                (when (nil? c')
                                                    (aset b 32 (unchecked-dec-int (aget b 32)))
                                                )
                                                ret
                                            )
                                        :else
                                            (let [
                                                diff (unchecked-subtract-int (last-range nm c) (if c' (last-range nm c') 0))
                                            ]
                                                (aset b n (unchecked-subtract-int (aget b n) diff))
                                                (aset ^objects (.array nm ret) n c')
                                                (when (nil? c')
                                                    (aset b 32 (unchecked-dec-int (aget b 32)))
                                                )
                                                ret
                                            )
                                    )
                                )
                            (zero? n)
                                nil
                            :else
                                (do
                                    (aset ^objects (.array nm ret) n nil)
                                    (aset b n 0)
                                    (aset b 32 (unchecked-dec-int (aget b 32)))
                                    ret
                                )
                        )
                    )
                )
            )
        )
        (doAssoc [this nm am shift root-edit current-node i val]
            (let [ret (.ensureEditable this nm am root-edit current-node shift)]
                (if (.regular nm ret)
                    (loop [shift shift node ret]
                        (when-not (zero? shift) => (.aset am (.array nm node) (& i 0x1f) val)
                            (let [
                                a (.array nm node)
                                n (& (>> i shift) 0x1f)
                                child (.ensureEditable this nm am root-edit (aget ^objects a n) shift)
                            ]
                                (aset ^objects a n child)
                                (recur (unchecked-subtract-int shift 5) child)
                            )
                        )
                    )
                    (let [
                        a (.array nm ret)
                        b (ranges nm ret)
                        n (& (>> i shift) 0x1f)
                        n
                            (loop [n n]
                                (when-not (< i (aget b n)) => n
                                    (recur (unchecked-inc-int n))
                                )
                            )
                        i
                            (when-not (zero? n) => i
                                (unchecked-subtract-int i (aget b (unchecked-dec-int n)))
                            )
                    ]
                        (aset ^objects a n (.doAssoc this nm am (unchecked-subtract-int shift 5) root-edit (aget ^objects a n) i val))
                    )
                )
                ret
            )
        )
        (newPath [this nm am tail edit shift current-node]
            (if (== (.alength am tail) 32)
                (loop [s 0 n current-node]
                    (when-not (== s shift) => n
                        (let [
                            a (object-array 32)
                            ret (.node nm edit a)
                        ]
                            (aset ^objects a 0 n)
                            (recur (unchecked-add s 5) ret)
                        )
                    )
                )
                (loop [s 0 n current-node]
                    (when-not (== s shift) => n
                        (let [
                            a (object-array 33)
                            b (int-array 33)
                            ret (.node nm edit a)
                        ]
                            (aset ^objects a 0 n)
                            (aset ^objects a 32 b)
                            (aset b 32 1)
                            (aset b 0 (.alength am tail))
                            (recur (unchecked-add s 5) ret)
                        )
                    )
                )
            )
        )
    )
)

(extend-protocol PSliceableVector
    Vec                         (slicev [v i e] (slicev (as-rrbt v) i e))
    PersistentVector            (slicev [v i e] (slicev (as-rrbt v) i e))
    APersistentVector$SubVector (slicev [v i e] (slicev (as-rrbt v) i e))
)

(extend-protocol PSpliceableVector
    Vec                         (splicev [v w] (splicev (as-rrbt v) w))
    PersistentVector            (splicev [v w] (splicev (as-rrbt v) w))
    APersistentVector$SubVector (splicev [v w] (splicev (as-rrbt v) w))
)

(defn subvec
    ([v i] (slicev v i (count v)))
    ([v i e] (slicev v i e))
)

(defn catvec
    ([] [])
    ([a]                                      a)
    ([a b]                           (splicev a b))
    ([a b c]                (splicev (splicev a b)          c))
    ([a b c d]              (splicev (splicev a b) (splicev c d)))
    ([a b c d & s] (splicev (splicev (splicev a b) (splicev c d)) (apply catvec s)))
)

(defmacro- gen-vector-method [& s]
    (let [a (with-meta (gensym "a__") {:tag 'objects})]
        `(let [~a (object-array ~(count s))]
            ~@(map-indexed (fn [i x] `(aset ~a ~i ~x)) s)
            (Vector. ^NodeManager object-nm ^ArrayManager object-am ~(count s) 5 empty-pv-node ~a nil ~(if s -1 1) ~(if s -1 (hash [])))
        )
    )
)

(defn vector
    ([]        (gen-vector-method))
    ([a]       (gen-vector-method a))
    ([a b]     (gen-vector-method a b))
    ([a b c]   (gen-vector-method a b c))
    ([a b c d] (gen-vector-method a b c d))
    ([a b c d & s]
        (loop-when [v (transient (vector a b c d)) s s] s => (persistent! v)
            (recur (.conj ^ITransientCollection v (first s)) (next s))
        )
    )
)

(defmacro- gen-vector-of-method [t & s]
    (let [am (gensym "am__") nm (gensym "nm__") a (gensym "a__")]
        `(let [
            ~am ^ArrayManager (ams ~t)
            ~nm ^NodeManager (if (identical? ~t :object) object-nm primitive-nm)
            ~a (.array ~am ~(count s))
        ]
            ~@(map-indexed (fn [i x] `(.aset ~am ~a ~i ~x)) s)
            (Vector. ~nm ~am ~(count s) 5 (if (identical? ~t :object) empty-pv-node empty-gvec-node) ~a nil ~(if s -1 1) ~(if s -1 (hash [])))
        )
    )
)

(defn vector-of
    ([t]         (gen-vector-of-method t))
    ([t a]       (gen-vector-of-method t a))
    ([t a b]     (gen-vector-of-method t a b))
    ([t a b c]   (gen-vector-of-method t a b c))
    ([t a b c d] (gen-vector-of-method t a b c d))
    ([t a b c d & s]
        (loop-when [v (transient (vector-of t a b c d)) s s] s => (persistent! v)
            (recur (.conj ^ITransientCollection v (first s)) (next s))
        )
    )
)

(defn vec [s]
    (if (vector? s) (as-rrbt s) (apply vector s))
)
)
