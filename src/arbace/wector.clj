(ns arbace.wector
    (:refer-clojure :only [defmacro])
)

(defmacro § [& _])
(defmacro ß [& _])

(ns arbace.bore
    (:refer-clojure :only [*ns* apply bit-and bit-or bit-shift-left bit-shift-right cons defmacro defn defprotocol defrecord doseq doto extend-type fn import into-array keys let map merge meta ns-imports ns-resolve ns-unmap object-array rem select-keys some? str symbol symbol? the-ns unsigned-bit-shift-right vary-meta when])
    (:require [flatland.ordered.map :refer [ordered-map]] #_[flatland.ordered.set :refer [ordered-set]])
)

(defmacro refer! [ns s]
    (let [f #(let [v (ns-resolve (the-ns ns) %) n (vary-meta % merge (select-keys (meta v) [:private :macro]))] `(def ~n ~v))]
        (if (symbol? s) (f s) (cons 'do (map f s)))
    )
)

(defmacro import! [& syms-or-seqs] `(do (doseq [n# (keys (ns-imports *ns*))] (ns-unmap *ns* n#)) (import ~@syms-or-seqs)))

(import! [java.lang Class Error #_String System Thread] #_[java.lang.reflect Method] [clojure.lang Namespace Symbol])

(defn #_"void" Bore'import-as [#_"Symbol" sym, #_"String" sig]
    (doto (#_"Class" .getDeclaredMethod Namespace, "referenceClass", (into-array [Symbol Class]))
        (#_"Method" .setAccessible true)
        (#_"Method" .invoke *ns*, (object-array [sym (Class/forName sig)]))
    )
    nil
)

(defmacro import-as [& s] (cons 'do (map (fn [[sym sig]] `(Bore'import-as (quote ~sym), ~sig)) (apply ordered-map s))))

(defn thread [] (Thread/currentThread))

(defmacro defp [p & s] (let [i (symbol (str p "'iface"))] `(do (defprotocol ~p ~@s) (def ~i (:on-interface ~p)) ~p)))
(defmacro defr [r & s] (let [c (symbol (str r "'class"))] `(do (defrecord ~c [] ~r ~@s)                         ~c)))
(defmacro defm [r p & s] (let [i `(:on-interface ~r)]     `(do (extend-type ~i ~p ~@s)                          ~i)))

(defmacro class-ns [r [& s] & z] `(do (defr ~r ~@s) ~@z))
(defmacro value-ns [_ & z] (cons 'do z))

(defmacro arbace-ns  [_ & s] (cons 'do s))
(defmacro cloiure-ns [_ & s] (cons 'do s))

(defmacro throw! [#_"String" s] `(throw (Error. ~s)))

(def % rem)

(def & bit-and)
(def | bit-or)

(def << bit-shift-left)
(def >> bit-shift-right)
(def >>> unsigned-bit-shift-right)

(defn aclone [a] (when (some? a) (clojure.core/aclone a)))
(defn acopy! [a i b j n] (System/arraycopy b, j, a, i, n) a)
(refer! clojure.core [aget alength])
(defn aset! [a i x] (clojure.core/aset a i x) a)
(defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

(ns arbace.wector
    (:refer-clojure :only [* + - -> < <= = and apply assoc atom case compare concat cond condp cons #_count dec declare defn defn- dotimes drop first fn hash-map identical? if-not if-some inc int int-array integer? into last let letfn list loop map mapcat max merge min mod neg? next nil? not object-array or partition-all pos? quot reduced? rem reset! satisfies? second seq sequential? some? str symbol? take update vary-meta vec vector? zero?])
    (:refer arbace.bore :only [& << >>> aclone acopy! aget alength arbace-ns aset! aswap! class-ns defm defp import! thread throw! value-ns])
)

(import!)

(arbace-ns Oops!

(defmacro def-      [x & s] `(def      ~(vary-meta x assoc :private true) ~@s))
(defmacro defmacro- [x & s] `(defmacro ~(vary-meta x assoc :private true) ~@s))

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro     when       [? & s] (let [[e & s] (=> s)]               `(if     ~? (do ~@s) ~e)))
    (defmacro     when-not   [? & s] (let [[e & s] (=> s)]               `(if-not ~? (do ~@s) ~e)))
    (defmacro let-when     [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if     ~? (do ~@s) ~e))))
)

(defmacro- assert-args [& s]
    `(when ~(first s) ~'=> (throw! (str (first ~'&form) " requires " ~(second s)))
        ~(let-when [s (next (next s))] s
            `(assert-args ~@s)
        )
    )
)

(defmacro if-first
    ([bind then] `(if-first ~bind ~then nil))
    ([bind then else & _]
        (assert-args
            (vector? bind) "a vector for its binding"
            (= 2 (clojure.core/count bind)) "exactly 2 forms in binding vector"
            (nil? _) "1 or 2 forms after binding vector"
        )
        `(let-when [s# (seq ~(bind 1))] (some? s#) ~'=> ~else
            (let [~(bind 0) (first s#)]
                ~then
            )
        )
    )
)

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro when-some  [v & s] (let [[e & s] (=> s)] `(if-some  ~(vec v) (do ~@s) ~e)))
    (defmacro when-first [v & s] (let [[e & s] (=> s)] `(if-first ~(vec v) (do ~@s) ~e)))
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
    (defmacro recur-when [? r & s] `(if ~? ~(r' r) ~(=> s)))
)

    (defp IObject
        (#_"boolean" IObject'''equals [#_"IObject" this, #_"Object" that])
        (#_"String" IObject'''toString [#_"IObject" this])
    )

    (defp Seqable
        (#_"ISeq" Seqable'''seq [#_"Seqable" this])
    )

    (defp Counted
        (#_"int" Counted'''count [#_"Counted" this])
    )

    (defp Hashed
        (#_"int" Hashed'''hash [#_"Hashed" this])
    )

    (defp IFn
        (#_"Object" IFn'''invoke
            [#_"IFn" this]
            [#_"IFn" this, a1]
            [#_"IFn" this, a1, a2]
            [#_"IFn" this, a1, a2, a3]
            [#_"IFn" this, a1, a2, a3, a4]
            [#_"IFn" this, a1, a2, a3, a4, a5]
            [#_"IFn" this, a1, a2, a3, a4, a5, a6]
            [#_"IFn" this, a1, a2, a3, a4, a5, a6, a7]
            [#_"IFn" this, a1, a2, a3, a4, a5, a6, a7, a8]
            [#_"IFn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]
            [#_"IFn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"ISeq" args]
        )
        (#_"Object" IFn'''applyTo [#_"IFn" this, #_"ISeq" args])
    )

    (defp IMeta
        (#_"IPersistentMap" IMeta'''meta [#_"IMeta" this])
    )

    (defp IObj
        (#_"IObj" IObj'''withMeta [#_"IObj" this, #_"IPersistentMap" meta])
    )

    (defp Sequential)

    (defp Reversible
        (#_"ISeq" Reversible'''rseq [#_"Reversible" this])
    )

    (defp Indexed
        (#_"Object" Indexed'''nth
            [#_"Indexed" this, #_"int" i]
            [#_"Indexed" this, #_"int" i, #_"Object" not-found]
        )
    )

    (defp ILookup
        (#_"Object" ILookup'''valAt
            [#_"ILookup" this, #_"Object" key]
            [#_"ILookup" this, #_"Object" key, #_"Object" not-found]
        )
    )

    (defp IPersistentCollection
        (#_"IPersistentCollection" IPersistentCollection'''conj [#_"IPersistentCollection" this, #_"Object" o])
        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"IPersistentCollection" this])
    )

    (defp IEditableCollection
        (#_"ITransientCollection" IEditableCollection'''asTransient [#_"IEditableCollection" this])
    )

    (defp Associative
        (#_"Associative" Associative'''assoc [#_"Associative" this, #_"Object" key, #_"Object" val])
        (#_"boolean" Associative'''containsKey [#_"Associative" this, #_"Object" key])
        (#_"IMapEntry" Associative'''entryAt [#_"Associative" this, #_"Object" key])
    )

    (defp IPersistentStack
        (#_"Object" IPersistentStack'''peek [#_"IPersistentStack" this])
        (#_"IPersistentStack" IPersistentStack'''pop [#_"IPersistentStack" this])
    )

    (defp IPersistentVector
        (#_"IPersistentVector" IPersistentVector'''assocN [#_"IPersistentVector" this, #_"int" i, #_"Object" val])
    )

    (defp ITransientCollection
        (#_"ITransientCollection" ITransientCollection'''conj! [#_"ITransientCollection" this, #_"Object" val])
        (#_"IPersistentCollection" ITransientCollection'''persistent! [#_"ITransientCollection" this])
    )

    (defp ITransientAssociative
        (#_"ITransientAssociative" ITransientAssociative'''assoc! [#_"ITransientAssociative" this, #_"Object" key, #_"Object" val])
        (#_"boolean" ITransientAssociative'''containsKey [#_"ITransientAssociative" this, #_"Object" key])
        (#_"IMapEntry" ITransientAssociative'''entryAt [#_"ITransientAssociative" this, #_"Object" key])
    )

    (defp ITransientVector
        (#_"ITransientVector" ITransientVector'''assocN! [#_"ITransientVector" this, #_"int" i, #_"Object" val])
        (#_"ITransientVector" ITransientVector'''pop! [#_"ITransientVector" this])
    )

    (defp IReduce
        (#_"Object" IReduce'''reduce
            [#_"IReduce" this, #_"IFn" f]
            [#_"IReduce" this, #_"IFn" f, #_"Object" r]
        )
    )

    (defp IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"IKVReduce" this, #_"IFn" f, #_"Object" r])
    )

    #_abstract
    (defp AFn) (ß extend-type IFn)

    #_abstract
    (defp APersistentVector)

(defn count'
    ([x] (count' x -1))
    ([x m]
        (condp satisfies? x
            Counted
                (Counted'''count x)
            Seqable
                (loop-when [n 0 s (Seqable'''seq x)] (and s (or (neg? m) (< n m))) => n
                    (when (satisfies? Counted s) => (recur (inc n) (next s))
                        (+ n (Counted'''count s))
                    )
                )
            (when (neg? m) => (throw! (str "count' not supported on " (clojure.core/class x)))
                (clojure.core/count x)
            )
        )
    )
)

(defn count [s] (count' s -1))
)

(declare MapEntry'create Murmur3'mixCollHash RSeq'new RT'printString VSeq'new)

(arbace-ns IPersistentWector
    (defp IPersistentWector
        (#_"IPersistentWector" IPersistentWector'''slicew [#_"IPersistentWector" this, #_"int" start, #_"int" end])
        (#_"IPersistentWector" IPersistentWector'''splicew [#_"IPersistentWector" this, #_"IPersistentWector" that])
    )
)

(defn wector? [x] (satisfies? IPersistentWector x))

(arbace-ns PersistentWector
    (defp WNode)
    (defp TransientWector)
    (defp PersistentWector)
)

(arbace-ns PersistentWector

(def- walue-array object-array)

(class-ns WNode []
    (defn #_"WNode" WNode'new [#_"Thread'" edit, #_"objects" array, #_"ints" index]
        (merge (WNode'class.)
            (hash-map
                #_"Thread'" :edit edit
                #_"objects" :array (or array (object-array 32))
                #_"ints" :index index
            )
        )
    )

    (def #_"WNode" WNode'EMPTY (WNode'new nil, nil, nil))

    (defn #_"void" WNode''assertEditable [#_"WNode" this]
        (let [
            #_"Thread" owner @(or (:edit this) (throw! "transient use of persistent data"))
        ]
            (when-not (identical? (thread) owner)
                (if (some? owner)
                    (throw! "transient used by non-owner thread")
                    (throw! "transient used after persistent! call")
                )
            )
        )
        nil
    )

    (defn #_"boolean" WNode''cow? [#_"WNode" this, #_"Thread'" edit]
        (let [
            #_"Thread'" e (:edit this)
        ]
            (or (nil? e) (nil? @e) (not (or (identical? e edit) (throw! "transient cow!"))))
        )
    )

    (defn #_"WNode" WNode''editableRoot [#_"WNode" this]
        (WNode'new (atom (thread)), (aclone (:array this)), (aclone (:index this)))
    )

    (defn #_"values" WNode'editableTail [#_"values" tail]
        (-> (walue-array 32) (acopy! 0 tail 0 (alength tail)))
    )

    (defn #_"values" WNode''arrayFor
        ([#_"WNode" this, #_"int" i, #_"int" shift, #_"int" cnt] (WNode''arrayFor this, i, shift, cnt, cnt, nil))
        ([#_"WNode" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
            (when (< -1 i cnt) => (throw! "index is out of bounds")
                (when (< i tail-off) => tail
                    (loop-when [i i #_"WNode" node this shift shift] (pos? shift) => (:array node)
                        (let [
                            #_"ints" x (:index node)
                            #_"int" m (& (>>> i shift) 0x1f)
                            [m i]
                                (when (some? x) => [m i]
                                    (let [
                                        m (loop-when-recur m (<= (aget x m) i) (inc m) => m)
                                    ]
                                        [m (if (pos? m) (- i (aget x (dec m))) i)]
                                    )
                                )
                        ]
                            (recur i (aget (:array node) m) (- shift 5))
                        )
                    )
                )
            )
        )
    )

    (defn #_"value" WNode''valueFor [#_"WNode" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"values" tail]
        (let [
            #_"int" tail-off (- cnt (alength tail))
        ]
            (aget (WNode''arrayFor this, i, shift, cnt, tail-off, tail) (if (<= tail-off i) (- i tail-off) (& (>>> i shift) 0x1f)))
        )
    )

    (defn #_"WNode" WNode''newPath [#_"WNode" this, #_"Thread'" edit, #_"int" shift]
        (when (pos? shift) => this
            (WNode'new edit, (-> (object-array 32) (aset! 0 (WNode''newPath this, edit, (- shift 5)))), nil)
        )
    )

    (defn #_"int" WNode'last-range [#_"ints" x]
        (aget x (dec (aget x 32)))
    )

    (defn #_"boolean" WNode''overflow? [#_"WNode" this, #_"int" shift, #_"int" cnt]
        (let [
            #_"ints" x (:index this)
        ]
            (when (some? x) => (< (<< 1 shift) (>>> (inc cnt) 5)) ;; vector: (< (<< 1 shift) (>>> cnt 5))
                (and (= (aget x 32) 32)
                    (or (= shift 5)
                        (recur
                            (aget (:array this) 31)
                            (- shift 5)
                            (+ (- (aget x 31) (aget x 30)) 32)
                        )
                    )
                )
            )
        )
    )

    (defn #_"WNode" WNode''pushTail [#_"WNode" this, #_"Thread'" edit, #_"int" shift, #_"int" cnt, #_"WNode" tail-node]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"objects" a (:array this) #_"ints" x (:index this)
        ]
            (if (some? x)
                (let [
                    #_"int" e (dec (aget x 32))
                    #_"WNode" child
                        (when (< 5 shift)
                            (let [
                                #_"int" n (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0))
                            ]
                                (when (< n (<< 1 shift))
                                    (WNode''pushTail (aget a e), edit, (- shift 5), (inc n), tail-node)
                                )
                            )
                        )
                    a (if cow? (aclone a) a) x (if cow? (aclone x) x)
                    _
                        (if (some? child)
                            (do
                                (aset! a e child)
                                (aswap! x e + 32)
                            )
                            (do
                                (aset! a (inc e) (WNode''newPath tail-node, edit, (- shift 5)))
                                (aset! x (inc e) (+ (aget x e) 32))
                                (aswap! x 32 inc)
                            )
                        )
                ]
                    (if cow? (WNode'new edit, a, x) this)
                )
                (let [
                    #_"int" e (& (>>> (dec cnt) shift) 0x1f)
                    #_"WNode" child
                        (when (< 5 shift) => tail-node
                            (if-some [child (aget a e)]
                                (WNode''pushTail child, edit, (- shift 5), cnt, tail-node)
                                (WNode''newPath tail-node, edit, (- shift 5))
                            )
                        )
                    a (if cow? (aclone a) a)
                    _ (aset! a e child)
                ]
                    (if cow? (WNode'new edit, a, nil) this)
                )
            )
        )
    )

    (defn #_"WNode" WNode''popTail [#_"WNode" this, #_"Thread'" edit, #_"int" shift, #_"int" cnt]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"objects" a (:array this) #_"ints" x (:index this)
            #_"int" e (& (>>> (dec cnt) shift) 0x1f) ;; vector: #_"int" e (& (>>> (- cnt 2) shift) 0x1f): more reasonable!
        ]
            (if (some? x)
                (let [
                    e (loop-when-recur e (and (< e 31) (pos? (aget x (inc e)))) (inc e) => e)
                ]
                    (cond
                        (< 5 shift)
                            (let [
                                #_"WNode" c (aget a e)
                                #_"WNode" c' (WNode''popTail c, edit, (- shift 5), (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0)))
                            ]
                                (when (or (some? c') (pos? e))
                                    (let [
                                        a (if cow? (aclone a) a)
                                        _ (aset! a e c')
                                        #_"int" d
                                            (when (some? (:index c)) => 32
                                                (- (WNode'last-range (:index c)) (if (some? c') (WNode'last-range (:index c')) 0))
                                            )
                                        x (if cow? (aclone x) x)
                                        _ (aswap! x e - d)
                                        _ (when (nil? c') (aswap! x 32 dec))
                                    ]
                                        (if cow? (WNode'new edit, a, x) this)
                                    )
                                )
                            )
                        (pos? e)
                            (let [
                                a (if cow? (aclone a) a)
                                _ (aset! a e nil)
                                x (if cow? (aclone x) x)
                                _ (aset! x e 0)
                                _ (aswap! x 32 dec)
                            ]
                                (if cow? (WNode'new edit, a, x) this)
                            )
                    )
                )
                (cond
                    (< 5 shift)
                        (let [
                            #_"WNode" child (WNode''popTail (aget a e), edit, (- shift 5), cnt)
                        ]
                            (when (or (some? child) (pos? e))
                                (let [
                                    a (if cow? (aclone a) a)
                                    _ (aset! a e child)
                                ]
                                    (if cow? (WNode'new edit, a, nil) this)
                                )
                            )
                        )
                    (pos? e)
                        (let [
                            a (if cow? (aclone a) a)
                            _ (aset! a e nil)
                        ]
                            (if cow? (WNode'new edit, a, nil) this)
                        )
                )
            )
        )
    )

    (defn #_"WNode" WNode''doAssoc [#_"WNode" this, #_"Thread'" edit, #_"int" shift, #_"int" i, #_"value" val]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"objects" a (:array this) #_"ints" x (:index this)
            a (if cow? (aclone a) a)
            #_"int" m (& (>>> i shift) 0x1f)
            _
                (when (pos? shift) => (aset! a m val)
                    (let [
                        [m i]
                            (when (some? x) => [m i]
                                (let [
                                    m (loop-when-recur m (<= (aget x m) i) (inc m) => m)
                                ]
                                    [m (if (pos? m) (- i (aget x (dec m))) i)]
                                )
                            )
                    ]
                        (aswap! a m WNode''doAssoc edit, (- shift 5), i, val)
                    )
                )
        ]
            (if cow? (WNode'new edit, a, x) this)
        )
    )

    (defn- #_"ints" WNode'n-index [#_"int" shift, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
            #_"ints" x (int-array 33)
        ]
            (loop [#_"int" j 0 #_"int" i k]
                (if (< i n)
                    (do
                        (aset! x j i)
                        (recur (inc j) (+ i k))
                    )
                    (-> x
                        (aset! j n)
                        (aset! 32 (inc j))
                    )
                )
            )
        )
    )

    (defn- #_"ints" WNode'm-n-index [#_"int" shift, #_"int" m, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
            #_"ints" x (int-array 33)
        ]
            (loop [#_"int" j 0 #_"int" i k]
                (if (< j m)
                    (do
                        (aset! x j i)
                        (recur (inc j) (+ i k))
                    )
                    (-> x
                        (aset! j n)
                        (aset! 32 (inc j))
                    )
                )
            )
        )
    )

    (defn- #_"int" WNode'index-of-nil [#_"objects" a]
        (loop-when [#_"int" l 0 #_"int" h 31] (< l (dec h)) => (cond (nil? (aget a l)) l (nil? (aget a h)) h :else 32)
            (let [
                #_"int" m (+ l (>>> (- h l) 1))
            ]
                (if (nil? (aget a m))
                    (recur l m)
                    (recur (inc m) h)
                )
            )
        )
    )

    (defn- #_"WNode" WNode''first-child [#_"WNode" this]
        (aget (:array this) 0)
    )

    (defn- #_"WNode" WNode''last-child [#_"WNode" this]
        (let [
            #_"objects" a (:array this) #_"ints" x (:index this)
        ]
            (aget a (dec (if (some? x) (aget x 32) (WNode'index-of-nil a))))
        )
    )

    (defn- #_"WNode" WNode''remove-leftmost-child [#_"WNode" this]
        (let [
            #_"objects" a (:array this)
        ]
            (when (some? (aget a 1))
                (let [
                    #_"objects" a' (-> (object-array 32) (acopy! 0 a 1 31))
                    #_"ints" x (:index this)
                    #_"ints" x'
                        (when (some? x)
                            (let [
                                #_"int" k (aget x 0)
                                #_"int" e (dec (aget x 32))
                                x' (int-array 33)
                                _ (dotimes [#_"int" j e] (aset! x' j (- (aget x (inc j)) k)))
                            ]
                                (-> x' (aset! e 0) (aset! 32 e))
                            )
                        )
                ]
                    (WNode'new nil, a', x')
                )
            )
        )
    )

    (defn- #_"WNode" WNode''replace-leftmost-child [#_"WNode" this, #_"int" shift, #_"int" cnt, #_"WNode" node, #_"int" delta]
        (let [
            #_"objects" a (:array this) #_"ints" x (:index this)
            #_"ints" x' (int-array 33)
            #_"objects" a'
                (if (some? x)
                    (let [
                        #_"int" n (aget x 32)
                        _ (dotimes [#_"int" j n] (aset! x' j (- (aget x j) delta)))
                        _ (aset! x' 32 n)
                    ]
                        (-> (aclone a) (aset! 0 node))
                    )
                    (let [
                        #_"int" k (<< 1 shift)
                        #_"int" n (& (>>> (dec cnt) shift) 0x1f)
                        _ (aset! x' 0 (- k delta))
                        _ (dotimes [#_"int" j n] (aset! x' (inc j) (+ (aget x' j) k)))
                        _ (aset! x' n (- cnt delta))
                        _ (aset! x' 32 (inc n))
                    ]
                        (-> (object-array 32) (aset! 0 node) (acopy! 1 a 1 n))
                    )
                )
        ]
            (WNode'new nil, a', x')
        )
    )

    (defn- #_"WNode" WNode''replace-rightmost-child [#_"WNode" this, #_"int" shift, #_"WNode" node, #_"int" delta]
        (let [
            #_"objects" a (:array this) #_"ints" x (:index this)
        ]
            (if (some? x)
                (let [
                    #_"int" e (dec (aget x 32))
                ]
                    (WNode'new nil, (-> (aclone a) (aset! e node)), (-> (aclone x) (aset! e (+ (aget x e) delta))))
                )
                (let [
                    #_"int" m (dec (WNode'index-of-nil a))
                ]
                    (if (some? (:index node))
                        (WNode'new nil, (-> (object-array 32) (acopy! 0 a 0 m) (aset! m node)), (WNode'm-n-index shift, m, (WNode'last-range (:index node))))
                        (WNode'new nil, (-> (aclone a) (aset! m node)), nil)
                    )
                )
            )
        )
    )

    (defn #_"WNode" WNode''fold-tail [#_"WNode" this, #_"int" shift, #_"int" tail-off, #_"objects" tail]
        (let [
            #_"objects" a (:array this) #_"ints" x (:index this)
            #_"int" m (WNode'index-of-nil a)
            #_"WNode" tail-node
                (when (< 5 shift) => (WNode'new nil, tail, nil)
                    (let [
                        #_"int" n
                            (when (some? x) => (mod tail-off (<< 1 shift))
                                (let [
                                    #_"int" e (dec (aget x 32))
                                ]
                                    (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0))
                                )
                            )
                    ]
                        (WNode''fold-tail (aget a (dec m)), (- shift 5), n, tail)
                    )
                )
        ]
            (when (or (< m 32) (and (some? tail-node) (< 5 shift)))
                (let [
                    #_"int" n (alength tail)
                    #_"ints" x'
                        (when (or (some? x) (< n 32))
                            (let [
                                x' (or (aclone x) (WNode'n-index shift, tail-off))
                            ]
                                (if (and (some? tail-node) (< 5 shift))
                                    (do
                                        (when (pos? m)
                                            (aset! x' (dec m) (+ (aget x' (dec m)) n))
                                        )
                                        (-> x' (aset! 32 m))
                                    )
                                    (do
                                        (aset! x' m (+ (if (pos? m) (aget x' (dec m)) 0) n))
                                        (-> x' (aset! 32 (inc m)))
                                    )
                                )
                            )
                        )
                    #_"objects" a' (-> (object-array 32) (acopy! 0 a 0 m))
                ]
                    (if (some? tail-node)
                        (aset! a' (if (< 5 shift) (dec m) m) tail-node)
                        (aset! a' m (WNode''newPath (WNode'new nil, tail, nil), nil, (- shift 5)))
                    )
                    (WNode'new nil, a', x')
                )
            )
        )
    )

    (def #_"int" WNode'rrbt-concat-threshold 33)
    (def- #_"int" WNode'max-extra-search-steps 2)

    (defn #_"WNode" WNode''slice-right [#_"WNode" this, #_"int" shift, #_"int" end]
        ;; => potentially return a short node, although it would be better to make sure a regular
        ;; leaf is always left at the right, with any items over the final 32 moved into tail
        ;; (and then potentially back into the tree should the tail become too long...)
        (when (pos? shift) => (WNode'new nil, (-> (walue-array end) (acopy! 0 (:array this) 0 end)), nil)
            (let [
                #_"objects" a (:array this) #_"ints" x (:index this)
                #_"int" m (& (>>> (dec end) shift) 0x1f)
                m
                    (when (some? x) => m
                        (loop-when-recur m (< (aget x m) end) (inc m) => m)
                    )
                #_"int" k (<< 1 shift)
                #_"int" child-end
                    (cond
                        (nil? x) (let [#_"int" e (rem end k)] (if (zero? e) k e))
                        (pos? m) (- end (aget x (dec m)))
                        :else    end
                    )
                #_"WNode" child (WNode''slice-right (aget a m), (- shift 5), child-end)
                #_"ints" y (:index child)
                #_"objects" a' (-> (object-array 32) (acopy! 0 a 0 m) (aset! m child))
                #_"ints" x'
                    (when (or (some? x) (some? y))
                        (let [
                            x' (int-array 33)
                            _ (dotimes [#_"int" j m] (aset! x' j (if (some? x) (aget x j) (* (inc j) k))))
                            #_"int" delta
                                (cond
                                    (nil? y)    (let [#_"int" e (mod child-end k) ] (if (zero? e) k e))
                                    (< 5 shift) (WNode'last-range y)
                                    :else       (alength (:array child))
                                )
                            _ (aset! x' m (+ (if (pos? m) (aget x' (dec m)) 0) delta))
                        ]
                            (-> x' (aset! 32 (inc m)))
                        )
                    )
            ]
                (WNode'new nil, a', x')
            )
        )
    )

    (defn #_"WNode" WNode''slice-left [#_"WNode" this, #_"int" shift, #_"int" start, #_"int" end]
        (if (zero? shift)
            ;; potentially return a short node
            (let [
                #_"objects" a (:array this)
                #_"int" n (- (alength a) start)
            ]
                (WNode'new nil, (-> (walue-array n) (acopy! 0 a start n)), nil)
            )
            (let [
                #_"objects" a (:array this) #_"ints" x (:index this)
                #_"int" m (& (>>> start shift) 0x1f)
                m
                    (when (some? x) => m
                        (loop-when-recur m (<= (aget x m) start) (inc m) => m)
                    )
                #_"int" n
                    (when (nil? x) => (aget x 32)
                        (loop-when-recur [n m] (and (< n 32) (some? (aget a n))) [(inc n)] => n)
                    )
                #_"int" k (<< 1 shift)
                #_"WNode" child
                    (let [
                        #_"int" i (if (some? x) (aget x (dec m)) (* m k))
                    ]
                        (WNode''slice-left (aget a m), (- shift 5), (if (pos? m) (- start i) start), (min k (if (pos? m) (- end i) end)))
                    )
                n (- n m)
                n (if (some? child) n (dec n))
            ]
                (when (pos? n)
                    (let [
                        #_"ints" x' (int-array 33)
                        _
                            (if (some? x)
                                (loop-when [#_"int" j 0 #_"int" i m] (< j n)
                                    (aset! x' j (- (aget x i) start))
                                    (recur (inc j) (inc i))
                                )
                                (let [
                                    #_"int" i
                                        (if (and (some? child) (some? (:index child)) (< 5 shift))
                                            (WNode'last-range (:index child))
                                            (- k (& (>>> start (- shift 5)) 0x1f))
                                        )
                                ]
                                    (loop-when [#_"int" j 0 i i] (< j n)
                                        (aset! x' j i)
                                        (recur (inc j) (+ i k))
                                    )
                                    (when (< 1 n)
                                        (aset! x' (dec n) (- end start))
                                    )
                                )
                            )
                        _ (aset! x' 32 n)
                        #_"objects" a'
                            (if (some? child)
                                (-> (object-array 32) (aset! 0 child) (acopy! 1 a (inc m) (dec n)))
                                (-> (object-array 32) (acopy! 0 a (inc m) n))
                            )
                    ]
                        (WNode'new nil, a', x')
                    )
                )
            )
        )
    )

    (defn #_"WNode" WNode''shift-from-to [#_"WNode" this, #_"int" from, #_"int" to]
        (when-not (= from to) => this
            (let [
                #_"ints" x'
                    (when (some? (:index this))
                        (-> (int-array 33) (aset! 0 (WNode'last-range (:index this))) (aset! 32 1))
                    )
            ]
                (recur (WNode'new nil, (-> (object-array 32) (aset! 0 this)), x') (+ 5 from) to)
            )
        )
    )

    (defn- #_"int" WNode''leaf-count [#_"WNode" this, #_"int" shift]
        (let [
            #_"objects" a (:array this) #_"ints" x (:index this)
        ]
            (cond
                (zero? shift) (alength a)
                (some? x)     (aget x 32)
                :else         (WNode'index-of-nil a)
            )
        )
    )

    (defn- #_"int" WNode''tree-count [#_"WNode" this, #_"int" shift]
        ;; NB. positive shifts only
        (let [
            #_"objects" a (:array this) #_"ints" x (:index this)
        ]
            (loop-when-recur [#_"int" i 0 #_"int" n 0]
                             (if (some? x) (< i (aget x 32)) (and (< i 32) (some? (aget a i))))
                             [(inc i) (+ n (WNode''leaf-count (aget a i), (- shift 5)))]
                          => n
            )
        )
    )

    (defn- #_"seq" WNode''leaf-seq [#_"WNode" this]
        (let [
            #_"objects" a (:array this)
        ]
            (mapcat :array (take (WNode'index-of-nil a) a))
        )
    )

    (defn- #_"[WNode WNode int]" WNode'rebalance-leaves [#_"WNode" node1, #_"WNode" node2, #_"int" delta]
        (let [
            #_"int" n1 (WNode''tree-count node1, 5) #_"int" n2 (WNode''tree-count node2, 5) #_"int" n (+ n1 n2)
        ]
            (when (< WNode'max-extra-search-steps (- (+ (WNode''leaf-count node1, 5) (WNode''leaf-count node2, 5)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
                (let [
                    #_"seq" s (map #(WNode'new nil, (walue-array %), nil) (partition-all 32 (concat (WNode''leaf-seq node1) (WNode''leaf-seq node2))))
                ]
                    (if (<= n (* 32 32))
                        (let [
                            #_"ints" x' (when-not (zero? (mod n 32)) (WNode'n-index 5, n))
                        ]
                            [(WNode'new nil, (object-array s), x') nil n2]
                        )
                        (let [
                            #_"ints" x' (when-not (zero? (mod n 32)) (WNode'n-index 5, (- n (* 32 32))))
                        ]
                            [(WNode'new nil, (object-array (take 32 s)), nil) (WNode'new nil, (object-array (drop 32 s)), x') (- (* 32 32) n1)]
                        )
                    )
                )
            )
        )
    )

    (defn- #_"seq" WNode''child-seq [#_"WNode" this, #_"int" shift, #_"int" cnt]
        (let [
            f'cseq
                (fn [#_"WNode" this #_"int" cnt]
                    (let [
                        #_"ints" x (or (:index this) (WNode'n-index (- shift 5), cnt))
                        #_"int" n (aget x 32)
                    ]
                        (take n (map list (:array this) (map - x (cons 0 x))))
                    )
                )
            #_"ints" x (or (:index this) (WNode'n-index shift, cnt))
            #_"int" n (aget x 32)
        ]
            (mapcat f'cseq (take n (:array this)) (take n (map - x (cons 0 x))))
        )
    )

    (defn- #_"[WNode WNode int]" WNode'rebalance [#_"int" shift, #_"WNode" node1, #_"int" cnt1, #_"WNode" node2, #_"int" cnt2, #_"int" delta]
        (when (some? node2) => [node1 nil delta]
            (let [
                #_"int" n1 (WNode''tree-count node1, shift) #_"int" n2 (WNode''tree-count node2, shift) #_"int" n (+ n1 n2)
            ]
                (when (< WNode'max-extra-search-steps (- (+ (WNode''leaf-count node1, shift) (WNode''leaf-count node2, shift)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
                    (let [
                        #_"seq" s (partition-all 32 (concat (WNode''child-seq node1, shift, cnt1) (WNode''child-seq node2, shift, cnt2)))
                    ]
                        (if (<= n (* 32 32))
                            (let [
                                #_"objects" a (object-array 32) #_"ints" x (int-array 33)
                                _
                                    (loop [#_"int" i 0 s s]
                                        (when-first [#_"seq" block s]
                                            (let [
                                                #_"objects" a' (object-array 32) #_"ints" x' (int-array 33)
                                                _
                                                    (loop [#_"int" j 0 #_"int" k 0 #_"seq" z (seq block)]
                                                        (when-first [[#_"WNode" c #_"int" r] z]
                                                            (aset! a' j c)
                                                            (aset! x' j (+ k r))
                                                            (recur (inc j) (+ k r) (next z))
                                                        )
                                                    )
                                                _ (aset! x' 32 (count block))
                                            ]
                                                (aset! a i (WNode'new nil, a', x'))
                                                (aset! x i (+ (WNode'last-range  x') (if (pos? i) (aget x (dec i)) 0)))
                                                (aset! x 32 (inc i))
                                                (recur (inc i) (next s))
                                            )
                                        )
                                    )
                            ]
                                [(WNode'new nil, a, x) nil cnt2]
                            )
                            (let [
                                #_"objects" a1 (object-array 32) #_"ints" x1 (int-array 33)
                                #_"objects" a2 (object-array 32) #_"ints" x2 (int-array 33)
                                delta
                                    (loop [delta delta #_"int" i 0 s s]
                                        (when-first [#_"seq" block s] => delta
                                            (let [
                                                #_"objects" a' (object-array 32) #_"ints" x' (int-array 33)
                                                _
                                                    (loop [#_"int" j 0 #_"int" k 0 #_"seq" z (seq block)]
                                                        (when-first [[#_"WNode" c #_"int" r] z]
                                                            (aset! a' j c)
                                                            (aset! x' j (+ k r))
                                                            (recur (inc j) (+ k r) (next z))
                                                        )
                                                    )
                                                _ (aset! x' 32 (count block))
                                                delta
                                                    (when (and (< i 32) (< n1 (+ (* i 32) (aget x' 32)))) => delta
                                                        (let [
                                                            #_"int" k (- (+ (* i 32) (aget x' 32)) n1)
                                                            #_"int" e (dec (aget x' 32))
                                                        ]
                                                            (+ delta (if (< k 32) (- (aget x' e) (aget x' (- e k))) (aget x' e)))
                                                        )
                                                    )
                                                #_"objects" a (if (< i 32) a1 a2) #_"ints" x (if (< i 32) x1 x2)
                                                #_"int" m (mod i 32)
                                            ]
                                                (aset! a m (WNode'new nil, a', x'))
                                                (aset! x m (+ (WNode'last-range  x') (if (pos? m) (aget x (dec m)) 0)))
                                                (aset! x 32 (inc m))
                                                (recur delta (inc i) (next s))
                                            )
                                        )
                                    )
                            ]
                                [(WNode'new nil, a1, x1) (WNode'new nil, a2, x2) delta]
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"[WNode WNode int]" WNode'zippath [#_"int" shift, #_"WNode" node1, #_"int" cnt1, #_"WNode" node2, #_"int" cnt2, #_"int" delta]
        (if (= shift 5)
            (WNode'rebalance-leaves node1, node2, delta)
            (let [
                #_"WNode" c1 (WNode''last-child node1)
                #_"WNode" c2 (WNode''first-child node2)
                #_"int" k (<< 1 shift)
                #_"int" m1
                    (let [
                        #_"ints" x1 (:index node1)
                    ]
                        (when (some? x1) => (let [#_"int" m (mod cnt1 k)] (if (zero? m) k m))
                            (let [#_"int" e (dec (aget x1 32))]
                                (if (pos? e) (- (aget x1 e) (aget x1 (dec e))) (aget x1 0))
                            )
                        )
                    )
                #_"int" m2
                    (let [
                        #_"ints" x2 (:index node2)
                    ]
                        (when (some? x2) => (let [#_"int" m (mod cnt2 k)] (if (zero? m) k m))
                            (aget x2 0)
                        )
                    )
                [#_"WNode" c1' #_"WNode" c2' #_"int" d'] (WNode'zippath (- shift 5), c1, m1, c2, m2, 0)
            ]
                (WNode'rebalance shift,
                    (if (identical? c1 c1') node1 (WNode''replace-rightmost-child node1, shift, c1', d')),
                    (+ cnt1 d'),
                    (if c2' (if (identical? c2 c2') node2 (WNode''replace-leftmost-child node2, shift, cnt2, c2', d')) (WNode''remove-leftmost-child node2)),
                    (- cnt2 d'),
                    (+ delta d')
                )
            )
        )
    )

    (defn #_"[WNode WNode]" WNode'squash-nodes [#_"int" shift, #_"WNode" node1, #_"int" cnt1, #_"WNode" node2, #_"int" cnt2]
        (let [
            #_"objects" a1 (:array node1) #_"int" n1 (WNode'index-of-nil a1)
            #_"objects" a2 (:array node2) #_"int" n2 (WNode'index-of-nil a2)
            #_"seq" slots (concat (take n1 a1) (take n2 a2))
        ]
            (when (<= (count slots) 32) => [node1 node2]
                (let [
                    #_"seq" s1 (take n1 (or (:index node1) (WNode'n-index shift, cnt1)))
                    #_"seq" s2 (take n2 (or (:index node2) (WNode'n-index shift, cnt2)))
                    #_"seq" index (concat s1 (let [#_"int" d (last s1)] (map #(+ % d) s2)))
                    #_"objects" a' (object-array 32)
                    _
                        (loop-when-recur [#_"int" i 0 s (seq slots)] s [(inc i) (next s)]
                            (aset! a' i (first s))
                        )
                    #_"ints" x' (int-array 33)
                    _
                        (loop-when-recur [#_"int" i 0 s (seq index)] s [(inc i) (next s)] => (aset! x' 32 i)
                            (aset! x' i (first s))
                        )
                ]
                    [(WNode'new nil, a', x') nil]
                )
            )
        )
    )
)

(class-ns TransientWector [AFn]
    (defn #_"TransientWector" TransientWector'new
        ([#_"PersistentWector" w]
            (TransientWector'new (:cnt w), (:shift w), (WNode''editableRoot (:root w)), (WNode'editableTail (:tail w)), (alength (:tail w)))
        )
        ([#_"int" cnt, #_"int" shift, #_"WNode" root, #_"values" tail, #_"int" tlen]
            (merge (TransientWector'class.)
                (hash-map
                    #_"int" :cnt cnt
                    #_"int" :shift shift
                    #_"WNode" :root root
                    #_"values" :tail tail
                    #_"int" :tlen tlen
                )
            )
        )
    )

    (defn- #_"int" TransientWector''tailoff [#_"TransientWector" this]
        (- (:cnt this) (:tlen this))
    )

    (defn- #_"values" TransientWector''arrayFor [#_"TransientWector" this, #_"int" i]
        (WNode''arrayFor (:root this), i, (:shift this), (:cnt this), (TransientWector''tailoff this), (:tail this))
    )

    (defm TransientWector Counted
        (#_"int" Counted'''count [#_"TransientWector" this]
            (WNode''assertEditable (:root this))
            (:cnt this)
        )
    )

    (defm TransientWector Indexed
        (#_"value" Indexed'''nth
            ([#_"TransientWector" this, #_"int" i]
                (WNode''assertEditable (:root this))
                (WNode''valueFor (:root this), i, (:shift this), (:cnt this), (:tail this))
            )
            ([#_"TransientWector" this, #_"int" i, #_"value" not-found]
                (WNode''assertEditable (:root this))
                (when (< -1 i (:cnt this)) => not-found
                    (WNode''valueFor (:root this), i, (:shift this), (:cnt this), (:tail this))
                )
            )
        )
    )

    (defm TransientWector ILookup
        (#_"value" ILookup'''valAt
            ([#_"TransientWector" this, #_"key" key] (ILookup'''valAt this, key, nil))
            ([#_"TransientWector" this, #_"key" key, #_"value" not-found]
                (WNode''assertEditable (:root this))
                (when (integer? key) => not-found
                    (let-when [#_"int" i (int key)] (< -1 i (:cnt this)) => not-found
                        (WNode''valueFor (:root this), i, (:shift this), (:cnt this), (:tail this))
                    )
                )
            )
        )
    )

    (defm TransientWector IFn
        (#_"value" IFn'''invoke [#_"TransientWector" this, #_"key" arg]
            (when (integer? arg) => (throw! "arg must be integer")
                (Indexed'''nth this, (int arg))
            )
        )

        (#_"value" IFn'''applyTo [#_"TransientWector" this, #_"seq" args]
            (case (count' args 1)
                1 (IFn'''invoke this, (first args))
            )
        )
    )

    (defm TransientWector ITransientCollection
        (#_"TransientWector" ITransientCollection'''conj! [#_"TransientWector" this, #_"value" val]
            (WNode''assertEditable (:root this))
            (if (< (:tlen this) 32)
                (let [
                    _ (aset! (:tail this) (:tlen this) val)
                ]
                    (-> this (update :cnt inc) (update :tlen inc))
                )
                (let [
                    #_"WNode" tail-node (WNode'new (:edit (:root this)), (:tail this), nil)
                    this (assoc this :tail (-> (walue-array 32) (aset! 0 val)), :tlen 1)
                ]
                    (if (WNode''overflow? (:root this), (:shift this), (:cnt this))
                        (let [
                            #_"objects" a
                                (-> (object-array 32)
                                    (aset! 0 (:root this))
                                    (aset! 1 (WNode''newPath tail-node, (:edit (:root this)), (:shift this)))
                                )
                            #_"ints" x
                                (when (some? (:index (:root this)))
                                    (let [
                                        #_"int" n (aget (:index (:root this)) 31)
                                    ]
                                        (-> (int-array 33) (aset! 0 n) (aset! 1 (+ n 32)) (aset! 32 2))
                                    )
                                )
                            #_"WNode" root (WNode'new (:edit (:root this)), a, x)
                        ]
                            (-> this (assoc :root root) (update :shift + 5) (update :cnt inc))
                        )
                        (let [
                            #_"WNode" root (WNode''pushTail (:root this), (:edit (:root this)), (:shift this), (:cnt this), tail-node)
                        ]
                            (-> this (assoc :root root) (update :cnt inc))
                        )
                    )
                )
            )
        )
    )

    (defm TransientWector ITransientVector
        (#_"TransientWector" ITransientVector'''assocN! [#_"TransientWector" this, #_"int" i, #_"value" val]
            (WNode''assertEditable (:root this))
            (if (< -1 i (:cnt this))
                (let [
                    #_"int" tail-off (TransientWector''tailoff this)
                ]
                    (if (<= tail-off i)
                        (do
                            (aset! (:tail this) (- i tail-off) val)
                            this
                        )
                        (do
                            (assoc this :root (WNode''doAssoc (:root this), (:edit (:root this)), (:shift this), i, val))
                        )
                    )
                )
                (when (= i (:cnt this)) => (throw! "index is out of bounds")
                    (ITransientCollection'''conj! this, val)
                )
            )
        )
    )

    (defm TransientWector ITransientAssociative
        (#_"TransientWector" ITransientAssociative'''assoc! [#_"TransientWector" this, #_"key" key, #_"value" val]
            (when (integer? key) => (throw! "key must be integer")
                (ITransientVector'''assocN! this, (int key), val)
            )
        )

        (#_"boolean" ITransientAssociative'''containsKey [#_"TransientWector" this, #_"key" key]
            (and (integer? key) (< -1 (int key) (:cnt this)))
        )

        (#_"IMapEntry" ITransientAssociative'''entryAt [#_"TransientWector" this, #_"key" key]
            (when (integer? key)
                (let-when [#_"int" i (int key)] (< -1 i (:cnt this))
                    (MapEntry'create key, (Indexed'''nth this, i))
                )
            )
        )
    )

    (defm TransientWector ITransientVector
        (#_"TransientWector" ITransientVector'''pop! [#_"TransientWector" this]
            (WNode''assertEditable (:root this))
            (cond
                (zero? (:cnt this))
                    (throw! "can't pop the empty vector")
                (= (:cnt this) 1)
                    (let [
                        this (assoc this :cnt 0)
                        this (assoc this :tlen 0)
                        _ (aset! (:tail this) 0 #_"value" nil)
                    ]
                        this
                    )
                (< 1 (:tlen this))
                    (let [
                        this (update this :cnt dec)
                        this (update this :tlen dec)
                        _ (aset! (:tail this) (:tlen this) #_"value" nil)
                    ]
                        this
                    )
                :else
                    (let [
                        #_"values" tail (aclone (TransientWector''arrayFor this, (- (:cnt this) 2)))
                        #_"WNode" root (WNode''popTail (:root this), (:edit (:root this)), (:shift this), (:cnt this))
                        this
                            (cond
                                (nil? root)
                                    (-> this
                                        (assoc :root (WNode'new (:edit (:root this)), nil, nil))
                                    )
                                (and (< 5 (:shift this)) (nil? (aget (:array root) 1)))
                                    (-> this
                                        (update :shift - 5)
                                        (assoc :root (aget (:array root) 0))
                                    )
                                :else
                                    (-> this
                                        (assoc :root root)
                                    )
                            )
                    ]
                        (-> this
                            (update :cnt dec)
                            (assoc :tail tail)
                            (assoc :tlen (alength tail))
                        )
                    )
            )
        )
    )

    (declare PersistentWector'new)

    (defm TransientWector ITransientCollection
        (#_"PersistentWector" ITransientCollection'''persistent! [#_"TransientWector" this]
            (WNode''assertEditable (:root this))
            (reset! (:edit (:root this)) nil)
            (let [
                #_"int" n (:tlen this)
            ]
                (PersistentWector'new (:cnt this), (:shift this), (:root this), (-> (walue-array n) (acopy! 0 (:tail this) 0 n)))
            )
        )
    )
)

(class-ns PersistentWector [APersistentVector]
    (defn #_"PersistentWector" PersistentWector'new
        ([#_"int" cnt, #_"int" shift, #_"WNode" root, #_"values" tail] (PersistentWector'new nil, cnt, shift, root, tail))
        ([#_"IPersistentMap" meta, #_"int" cnt, #_"int" shift, #_"WNode" root, #_"values" tail]
            (merge (PersistentWector'class.)
                (hash-map
                    #_"IPersistentMap" :_meta meta
                    #_"int" :cnt cnt
                    #_"int" :shift shift
                    #_"WNode" :root root
                    #_"values" :tail tail
                )
            )
        )
    )

    (def #_"PersistentWector" PersistentWector'EMPTY (PersistentWector'new 0, 5, WNode'EMPTY, (walue-array 0)))

    (defn #_"PersistentWector" PersistentWector'create [& values]
        (when-some [#_"seq" s (seq values)] => PersistentWector'EMPTY
            (let [
                #_"values" tail (walue-array (take 32 s)) #_"int" n (alength tail)
                #_"PersistentWector" w (PersistentWector'new n, 5, WNode'EMPTY, tail)
            ]
                (when-some [s (seq (drop 32 s))] => w
                    (into w s)
                )
            )
        )
    )

    (defm PersistentWector IMeta
        (#_"IPersistentMap" IMeta'''meta [#_"PersistentWector" this]
            (:_meta this)
        )
    )

    (defm PersistentWector IObj
        (#_"PersistentWector" IObj'''withMeta [#_"PersistentWector" this, #_"IPersistentMap" meta]
            (PersistentWector'new meta, (:cnt this), (:shift this), (:root this), (:tail this))
        )
    )

    (defm PersistentWector IObject
        (#_"boolean" IObject'''equals [#_"PersistentWector" this, #_"Object" that]
            (or (identical? this that)
                (cond
                    (wector? that)
                        (when (= (:cnt this) (:cnt that)) => false
                            (loop-when [#_"int" i 0] (< i (:cnt this)) => true
                                (recur-when (= (Indexed'''nth this, i) (Indexed'''nth that, i)) [(inc i)] => false)
                            )
                        )
                    (sequential? that)
                        (loop-when [#_"int" i 0 #_"seq" s (seq that)] (< i (:cnt this)) => (nil? s)
                            (recur-when (and (some? s) (= (Indexed'''nth this, i) (first s))) [(inc i) (next s)] => false)
                        )
                    :else
                        false
                )
            )
        )

        (#_"String" IObject'''toString [#_"PersistentWector" this]
            (RT'printString this)
        )
    )

    (defm PersistentWector Hashed
        (#_"int" Hashed'''hash [#_"PersistentWector" this]
            (loop-when [#_"int" hash 1 #_"int" i 0] (< i (:cnt this)) => (Murmur3'mixCollHash hash, i)
                (recur (+ (* 31 hash) (Hashed'''hash (Indexed'''nth this, i))) (inc i))
            )
        )
    )

    (defm PersistentWector IEditableCollection
        (#_"TransientWector" IEditableCollection'''asTransient [#_"PersistentWector" this]
            (TransientWector'new this)
        )
    )

    (defm PersistentWector Counted
        (#_"int" Counted'''count [#_"PersistentWector" this]
            (:cnt this)
        )
    )

    (defn- #_"int" PersistentWector''tailoff [#_"PersistentWector" this]
        (- (:cnt this) (alength (:tail this)))
    )

    (defn- #_"values" PersistentWector''arrayFor [#_"PersistentWector" this, #_"int" i]
        (WNode''arrayFor (:root this), i, (:shift this), (:cnt this), (PersistentWector''tailoff this), (:tail this))
    )

    (defm PersistentWector Indexed
        (#_"value" Indexed'''nth
            ([#_"PersistentWector" this, #_"int" i]
                (WNode''valueFor (:root this), i, (:shift this), (:cnt this), (:tail this))
            )
            ([#_"PersistentWector" this, #_"int" i, #_"value" not-found]
                (when (< -1 i (:cnt this)) => not-found
                    (WNode''valueFor (:root this), i, (:shift this), (:cnt this), (:tail this))
                )
            )
        )
    )

    (defm PersistentWector IPersistentCollection
        (#_"PersistentWector" IPersistentCollection'''conj [#_"PersistentWector" this, #_"value" val]
            (let [
                #_"int" tail-len (alength (:tail this))
            ]
                (if (< tail-len 32)
                    (let [
                        #_"values" tail (-> (walue-array (inc tail-len)) (acopy! 0 (:tail this) 0 tail-len) (aset! tail-len val))
                    ]
                        (PersistentWector'new (:_meta this), (inc (:cnt this)), (:shift this), (:root this), tail)
                    )
                    (let [
                        #_"WNode" tail-node (WNode'new (:edit (:root this)), (:tail this), nil)
                        #_"int" shift (:shift this)
                        [#_"WNode" root shift]
                            (if (WNode''overflow? (:root this), shift, (:cnt this))
                                (let [
                                    #_"objects" a
                                        (-> (object-array 32)
                                            (aset! 0 (:root this))
                                            (aset! 1 (WNode''newPath tail-node, (:edit (:root this)), shift))
                                        )
                                    #_"ints" x
                                        (when (some? (:index (:root this)))
                                            (let [
                                                #_"int" n (aget (:index (:root this)) 31)
                                            ]
                                                (-> (int-array 33) (aset! 0 n) (aset! 1 (+ n 32)) (aset! 32 2))
                                            )
                                        )
                                ]
                                    [(WNode'new (:edit (:root this)), a, x) (+ shift 5)]
                                )
                                [(WNode''pushTail (:root this), (:edit (:root this)), shift, (:cnt this), tail-node) shift]
                            )
                    ]
                        (PersistentWector'new (:_meta this), (inc (:cnt this)), shift, root, (walue-array [ val ]))
                    )
                )
            )
        )

        (#_"PersistentWector" IPersistentCollection'''empty [#_"PersistentWector" this]
            (IObj'''withMeta PersistentWector'EMPTY, (:_meta this))
        )
    )

    (defm PersistentWector IPersistentVector
        (#_"PersistentWector" IPersistentVector'''assocN [#_"PersistentWector" this, #_"int" i, #_"value" val]
            (if (< -1 i (:cnt this))
                (let [
                    #_"int" tail-off (PersistentWector''tailoff this)
                ]
                    (if (<= tail-off i)
                        (let [
                            #_"int" n (alength (:tail this))
                            #_"values" tail (-> (walue-array n) (acopy! 0 (:tail this) 0 n) (aset! (- i tail-off) val))
                        ]
                            (PersistentWector'new (:_meta this), (:cnt this), (:shift this), (:root this), tail)
                        )
                        (PersistentWector'new (:_meta this), (:cnt this), (:shift this), (WNode''doAssoc (:root this), (:edit (:root this)), (:shift this), i, val), (:tail this))
                    )
                )
                (when (= i (:cnt this)) => (throw! "index is out of bounds")
                    (IPersistentCollection'''conj this, val)
                )
            )
        )
    )

    (defm PersistentWector IPersistentStack
        (#_"value" IPersistentStack'''peek [#_"PersistentWector" this]
            (when (pos? (:cnt this))
                (Indexed'''nth this, (dec (:cnt this)))
            )
        )

        (#_"PersistentWector" IPersistentStack'''pop [#_"PersistentWector" this]
            (condp = (:cnt this)
                0   (throw! "can't pop the empty vector")
                1   (IObj'''withMeta PersistentWector'EMPTY, (:_meta this))
                (let [
                    #_"int" tail-len (alength (:tail this))
                ]
                    (if (< 1 tail-len)
                        (let [
                            #_"values" tail (-> (walue-array (dec tail-len)) (acopy! 0 (:tail this) 0 (dec tail-len)))
                        ]
                            (PersistentWector'new (:_meta this), (dec (:cnt this)), (:shift this), (:root this), tail)
                        )
                        (let [
                            #_"values" tail (PersistentWector''arrayFor this, (- (:cnt this) 2))
                            #_"int" shift (:shift this)
                            #_"WNode" root (WNode''popTail (:root this), (:edit (:root this)), shift, (PersistentWector''tailoff this)) ;; all the others: (:cnt this)
                            [shift root]
                                (cond
                                    (nil? root)                                     [shift WNode'EMPTY]
                                    (and (< 5 shift) (nil? (aget (:array root) 1))) [(- shift 5) (aget (:array root) 0)]
                                    :else                                           [shift root]
                                )
                        ]
                            (PersistentWector'new (:_meta this), (dec (:cnt this)), shift, root, tail)
                        )
                    )
                )
            )
        )
    )

    (defm PersistentWector IFn
        (#_"value" IFn'''invoke [#_"PersistentWector" this, #_"key" arg]
            (when (integer? arg) => (throw! "arg must be integer")
                (Indexed'''nth this, (int arg))
            )
        )

        (#_"value" IFn'''applyTo [#_"PersistentWector" this, #_"seq" args]
            (case (count' args 1)
                1 (IFn'''invoke this, (first args))
            )
        )
    )

    (defm PersistentWector IReduce
        (#_"value" IReduce'''reduce
            ([#_"PersistentWector" this, #_"IFn" f]
                (when (pos? (:cnt this)) => (f)
                    (loop-when [#_"value" r (aget (PersistentWector''arrayFor this, 0) 0) #_"int" i 0] (< i (:cnt this)) => r
                        (let [#_"values" a (PersistentWector''arrayFor this, i)
                              r (loop-when [r r #_"int" j (if (zero? i) 1 0)] (< j (alength a)) => r
                                    (let [r (f r (aget a j))]
                                        (when-not (reduced? r) => r
                                            (recur r (inc j))
                                        )
                                    )
                                )]
                            (when-not (reduced? r) => @r
                                (recur r (+ i (alength a)))
                            )
                        )
                    )
                )
            )
            ([#_"PersistentWector" this, #_"IFn" f, #_"value" r]
                (loop-when [r r #_"int" i 0] (< i (:cnt this)) => r
                    (let [#_"values" a (PersistentWector''arrayFor this, i)
                          r (loop-when [r r #_"int" j 0] (< j (alength a)) => r
                                (let [r (f r (aget a j))]
                                    (when-not (reduced? r) => r
                                        (recur r (inc j))
                                    )
                                )
                            )]
                        (when-not (reduced? r) => @r
                            (recur r (+ i (alength a)))
                        )
                    )
                )
            )
        )
    )

    (defm PersistentWector IKVReduce
        (#_"value" IKVReduce'''kvreduce [#_"PersistentWector" this, #_"IFn" f, #_"value" r]
            (loop-when [r r #_"int" i 0] (< i (:cnt this)) => r
                (let [
                    #_"values" a (PersistentWector''arrayFor this, i)
                    r
                        (loop-when [r r #_"int" j 0] (< j (alength a)) => r
                            (let [
                                r (f r (+ i j) (aget a j))
                            ]
                                (when-not (reduced? r) => r
                                    (recur r (inc j))
                                )
                            )
                        )
                ]
                    (when-not (reduced? r) => @r
                        (recur r (+ i (alength a)))
                    )
                )
            )
        )
    )

    (defm PersistentWector Associative
        (#_"IPersistentVector" Associative'''assoc [#_"PersistentWector" this, #_"key" key, #_"value" val]
            (when (integer? key) => (throw! "key must be integer")
                (IPersistentVector'''assocN this, (int key), val)
            )
        )

        (#_"boolean" Associative'''containsKey [#_"PersistentWector" this, #_"key" key]
            (and (integer? key) (< -1 (int key) (:cnt this)))
        )

        (#_"IMapEntry" Associative'''entryAt [#_"PersistentWector" this, #_"key" key]
            (when (integer? key)
                (let-when [#_"int" i (int key)] (< -1 i (:cnt this))
                    (MapEntry'create key, (Indexed'''nth this, i))
                )
            )
        )
    )

    (defm PersistentWector ILookup
        (#_"value" ILookup'''valAt
            ([#_"PersistentWector" this, #_"key" key] (ILookup'''valAt this, key, nil))
            ([#_"PersistentWector" this, #_"key" key, #_"value" not-found]
                (when (integer? key) => not-found
                    (let-when [#_"int" i (int key)] (< -1 i (:cnt this)) => not-found
                        (Indexed'''nth this, i)
                    )
                )
            )
        )
    )

    (defm PersistentWector IPersistentWector
        (#_"PersistentWector" IPersistentWector'''slicew [#_"PersistentWector" this, #_"int" start, #_"int" end]
            (cond
                (or (neg? start) (< (:cnt this) end)) (throw! "index is out of bounds")
                (= start end)                         (IPersistentCollection'''empty this) ;; NB. preserves metadata
                (< end start)                         (throw! "start index greater than end index")
                :else
                    (let [
                        #_"int" new-cnt (- end start)
                        #_"int" tail-off (PersistentWector''tailoff this)
                    ]
                        (if (<= tail-off start)
                            (let [
                                #_"values" tail (-> (walue-array new-cnt) (acopy! 0 (:tail this) (- start tail-off) new-cnt))
                            ]
                                (PersistentWector'new (:_meta this), new-cnt, 5, WNode'EMPTY, tail)
                            )
                            (let [
                                #_"boolean" tail-cut? (< tail-off end)
                                #_"WNode" root (:root this)
                                root (if tail-cut? root (WNode''slice-right root, (:shift this), end))
                                root (if (zero? start) root (WNode''slice-left root, (:shift this), start, (min end tail-off)))
                                #_"values" tail
                                    (when tail-cut? => (WNode''arrayFor root, (dec new-cnt), (:shift this), new-cnt)
                                        (let [
                                            #_"int" n (- end tail-off)
                                        ]
                                            (-> (walue-array n) (acopy! 0 (:tail this) 0 n))
                                        )
                                    )
                                root
                                    (when-not tail-cut? => root
                                        (WNode''popTail root, nil, (:shift this), new-cnt)
                                    )
                            ]
                                (when (some? root) => (PersistentWector'new (:_meta this), new-cnt, 5, WNode'EMPTY, tail)
                                    (loop-when-recur [#_"WNode" node root #_"int" shift (:shift this)]
                                                     (and (< 5 shift) (nil? (aget (:array node) 1)))
                                                     [(aget (:array node) 0) (- shift 5)]
                                                  => (PersistentWector'new (:_meta this), new-cnt, shift, node, tail)
                                    )
                                )
                            )
                        )
                    )
            )
        )

        (#_"PersistentWector" IPersistentWector'''splicew [#_"PersistentWector" this, #_"PersistentWector" that]
            (let [
                #_"int" c1 (:cnt this) #_"int" c2 (:cnt that)
            ]
                (cond
                    (zero? c1) that
                    (< c2 WNode'rrbt-concat-threshold) (into this that)
                    :else
                        (let [
                            #_"WNode" r1 (:root this) #_"int" s1 (:shift this) #_"objects" t1 (:tail this)
                            #_"boolean" overflow? (WNode''overflow? r1, s1, (+ c1 (- 32 (alength t1))))
                            r1
                                (when overflow? => (WNode''fold-tail r1, s1, (PersistentWector''tailoff this), t1)
                                    (let [
                                        #_"objects" a'
                                            (-> (object-array 32)
                                                (aset! 0 r1)
                                                (aset! 1 (WNode''newPath (WNode'new nil, t1, nil), nil, s1))
                                            )
                                        #_"ints" x'
                                            (when (or (some? (:index r1)) (< (alength t1) 32))
                                                (-> (int-array 33) (aset! 0 (- c1 (alength t1))) (aset! 1 c1) (aset! 32 2))
                                            )
                                    ]
                                        (WNode'new nil, a', x')
                                    )
                                )
                            s1 (if overflow? (+ s1 5) s1)
                            #_"WNode" r2 (:root that) #_"int" s2 (:shift that) #_"objects" t2 (:tail that)
                            #_"int" shift (max s1 s2)
                            r1 (WNode''shift-from-to r1, s1, shift)
                            r2 (WNode''shift-from-to r2, s2, shift)
                            [#_"WNode" n1 #_"WNode" n2 #_"int" delta] (WNode'zippath shift, r1, c1, r2, (- c2 (alength t2)), 0)
                            #_"int" c1' (+ c1 delta)
                            #_"int" c2' (- c2 (alength t2) delta)
                            [n1 n2] (if (identical? n2 r2) (WNode'squash-nodes shift, n1, c1', n2, c2') [n1 n2])
                        ]
                            (if (some? n2)
                                (let [
                                    #_"objects" a' (-> (object-array 32) (aset! 0 n1) (aset! 1 n2))
                                    #_"ints" x' (-> (int-array 33) (aset! 0 c1') (aset! 1 (+ c1' c2')) (aset! 32 2))
                                ]
                                    (PersistentWector'new nil, (+ c1 c2), (+ shift 5), (WNode'new nil, a', x'), t2)
                                )
                                (loop-when-recur [#_"WNode" node n1 shift shift]
                                                 (and (< 5 shift) (nil? (aget (:array node) 1)))
                                                 [(aget (:array node) 0) (- shift 5)]
                                              => (PersistentWector'new nil, (+ c1 c2), shift, node, t2)
                                )
                            )
                        )
                )
            )
        )
    )

    (defm PersistentWector Sequential)

    (defm PersistentWector Seqable
        (#_"seq" Seqable'''seq [#_"PersistentWector" this]
            (when (pos? (:cnt this))
                (VSeq'new this, 0)
            )
        )
    )

    (defm PersistentWector Reversible
        (#_"seq" Reversible'''rseq [#_"PersistentWector" this]
            (when (pos? (:cnt this))
                (RSeq'new this, (dec (:cnt this)))
            )
        )
    )

    #_foreign
    (§ defm PersistentWector #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"PersistentWector" this, #_"IPersistentVector" that]
            (when-not (identical? this that) => 0
                (let [#_"int" n (:cnt this) #_"int" m (count that)]
                    (cond (< n m) -1 (< m n) 1
                        :else
                            (loop-when [#_"int" i 0] (< i n) => 0
                                (let [#_"int" cmp (compare (Indexed'''nth this, i) (Indexed'''nth that, i))]
                                    (recur-when (zero? cmp) [(inc i)] => cmp)
                                )
                            )
                    )
                )
            )
        )
    )
)
)

(value-ns Wector

(defn subwec
    ([v i]   (IPersistentWector'''slicew v, i, (count v)))
    ([v i e] (IPersistentWector'''slicew v, i, e))
)

(defn catwec
    ([] [])
    ([a]                                                                                                  a)
    ([a b]                                                                   (IPersistentWector'''splicew a, b))
    ([a b c]                                    (IPersistentWector'''splicew (IPersistentWector'''splicew a, b),                              c))
    ([a b c d]                                  (IPersistentWector'''splicew (IPersistentWector'''splicew a, b), (IPersistentWector'''splicew c, d)))
    ([a b c d & s] (IPersistentWector'''splicew (IPersistentWector'''splicew (IPersistentWector'''splicew a, b), (IPersistentWector'''splicew c, d)), (apply catwec s)))
)

(defn wector
    ([]                   PersistentWector'EMPTY)
    ([a]                 (PersistentWector'create a))
    ([a b]               (PersistentWector'create a b))
    ([a b c]             (PersistentWector'create a b c))
    ([a b c d]           (PersistentWector'create a b c d))
    ([a b c d & s] (apply PersistentWector'create a b c d s))
)

(defn wec [s]
    (if (wector? s) s (apply wector s))
)
)

(defn -main [& args])
