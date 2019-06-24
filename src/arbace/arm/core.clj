(ns arbace.arm.core
    (:refer-clojure :only []) (:require [clojure.core :as -])
)

(defmacro about [& s] (cons 'do s))

(defn identical? [a b] (-/identical? a b))

(defn throw! [#_"String" s] (throw (Error. s)))

(let [last-id' (atom 0)] (defn next-id! [] (swap! last-id' inc)))

(defn gensym
    ([] (gensym "G__"))
    ([prefix] (symbol (str prefix (next-id!))))
)

(defn identity [x] x)

(defn nil?  [x] (identical? x nil))
(defn not   [x] (if x false true))
(defn some? [x] (not (nil? x)))

(defmacro if-not
    ([? then] (if-not ? then nil))
    ([? then else] (list 'if ? else then))
)

(defmacro and
    ([] true)
    ([x] x)
    ([x & s] `(let [and# ~x] (if and# (and ~@s) and#)))
)

(defmacro or
    ([] nil)
    ([x] x)
    ([x & s] `(let [or# ~x] (if or# or# (or ~@s))))
)

(defmacro any
    ([f x y] `(~f ~x ~y))
    ([f x y & s] `(let [f# ~f x# ~x] (or (f# x# ~y) (any f# x# ~@s))))
)

(defmacro letfn [fnspecs & body]
    `(letfn* ~(vec (interleave (map first fnspecs) (map (fn [s] (cons `fn s)) fnspecs))) ~@body)
)

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro     when       [? & s] (let [[e & s] (=> s)]               `(if     ~? (do ~@s) ~e)))
    (defmacro     when-not   [? & s] (let [[e & s] (=> s)]               `(if-not ~? (do ~@s) ~e)))
    (defmacro let-when     [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if     ~? (do ~@s) ~e))))
    (defmacro let-when-not [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if-not ~? (do ~@s) ~e))))
)

(defmacro cond [& s]
    (when s
        `(if ~(first s)
            ~(when (next s) => (throw! "cond requires an even number of forms")
                (second s)
            )
            (cond ~@(next (next s)))
        )
    )
)

(defmacro if-some
    ([bind then] `(if-some ~bind ~then nil))
    ([bind then else & _]
        `(let-when [x# ~(bind 1)] (some? x#) ~'=> ~else
            (let [~(bind 0) x#]
                ~then
            )
        )
    )
)

(defmacro cond-some [bind then & else]
    (let [bind (if (vector? bind) bind [`_# bind])]
        `(if-some ~bind ~then ~(when else `(cond-some ~@else)))
    )
)

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro when-some [v & s] (let [[e & s] (=> s)] `(if-some ~(vec v) (do ~@s) ~e)))
)

(defmacro condp [p e & s]
    (letfn [(emit- [p e s]
                (let [n (count s 2)]
                    (cond
                        (= n 0) `(throw! (str "no matching clause: " ~e))
                        (= n 1) (first s)
                        :else `(if (~p ~(first s) ~e) ~(second s) ~(emit- p e (next (next s))))
                    )
                )
            )]
        (emit- p e s)
    )
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

(defmacro -> [x & s]
    (when s => x
        (recur &form &env
            (let-when [f (first s)] (seq? f) => (list f x)
                `(~(first f) ~x ~@(next f))
            )
            (next s)
        )
    )
)

(about #_"defproto"

(defn emit-defproto* [name sigs]
    `(do
        (defonce ~name (hash-map))
        ~@(map (fn [[f & _]] `(defmacro ~f [x# & s#] (list* (list -/find-protocol-method '~name ~(keyword (str f)) x#) x# s#))) sigs)
    )
)
)

(about #_"arbace.arm.Mutable"
    (defonce Mutable (hash-map))
)

(about #_"arbace.arm.Typed"
    (defonce Typed (hash-map))
)

(about #_"defarray"

(defn emit-defarray* [tname cname fields interfaces methods opts]
    (let [a '__array s (mapcat (fn [x y] [(-/name #_keyword y) x]) (-/range) fields)]
        (letfn [(ilookup [[i m]]
                    [
                        (conj i 'clojure.lang.ILookup)
                        (conj m
                            `(valAt [this# k#] (ILookup''valAt this# k# nil))
                            `(valAt [this# k# else#] (if-some [x# (condp = (-/name k#) ~@s nil)] (-/aget (. this# ~a) x#) else#))
                        )
                    ]
                )
                (mutable [[i m]]
                    [
                        (conj i 'arbace.arm.core.Mutable)
                        (conj m
                            `(mutate [this# k# v#] (let [x# (condp = (-/name k#) ~@s)] (-/aset (. this# ~a) x# v#) this#))
                        )
                    ]
                )
                (typed [[i m]]
                    [
                        (conj i 'arbace.arm.core.Typed)
                        (conj m
                            `(type [this#] '~classname)
                        )
                    ]
                )]
            (let [[i m] (-> [interfaces methods] ilookup mutable typed)]
                `(-/eval '~(-/read-string (str (list* 'deftype* (symbol (-/name (-/ns-name -/*ns*)) (-/name tname)) classname (vector a) :implements (vec i) m))))
            )
        )
    )
)
)

(about #_"defassoc"

(defn emit-defassoc* [tname cname interfaces methods opts]
    (let [
        type-hash  (IHashEq''hasheq classname)
    ]
        (let [a '__assoc]
            (letfn [(eqhash [[i m]]
                        [
                            (conj i 'clojure.lang.IHashEq)
                            (conj m
                                `(hasheq [this#] (-/int (bit-xor ~type-hash (.hasheq (. this# ~a)))))
                                `(hashCode [this#] (.hashCode (. this# ~a)))
                                `(equals [this# that#] (and #_(some? that#) (-/instance? ~tname that#) (.equals (. this# ~a) (. that# ~a))))
                            )
                        ]
                    )
                    (ilookup [[i m]]
                        [
                            (conj i 'clojure.lang.ILookup)
                            (conj m
                                `(valAt [this# k#] (.valAt this# k# nil))
                                `(valAt [this# k# else#] (.valAt (. this# ~a) k# else#))
                            )
                        ]
                    )
                    (imap [[i m]]
                        [
                            (conj i 'clojure.lang.IPersistentMap)
                            (conj m
                                `(count [this#] (.count (. this# ~a)))
                                `(empty [this#] (new ~tname (.empty (. this# ~a))))
                                `(cons [this# e#] (new ~tname (.cons (. this# ~a) e#)))
                                `(equiv [this# that#]
                                    (or (identical? this# that#)
                                        (and (identical? (-/class this#) (-/class that#))
                                            (= (. this# ~a) (. that# ~a))
                                        )
                                    )
                                )
                                `(containsKey [this# k#] (.containsKey (. this# ~a) k#))
                                `(entryAt [this# k#] (.entryAt (. this# ~a) k#))
                                `(seq [this#] (.seq (. this# ~a)))
                                `(assoc [this# k# v#] (new ~tname (.assoc (. this# ~a) k# v#)))
                                `(without [this# k#] (new ~tname (.without (. this# ~a) k#)))
                            )
                        ]
                    )
                    (typed [[i m]]
                        [
                            (conj i 'arbace.arm.core.Typed)
                            (conj m
                                `(type [this#] '~classname)
                            )
                        ]
                    )]
                (let [[i m] (-> [interfaces methods] eqhash ilookup imap typed)]
                    `(-/eval '~(-/read-string (str (list* 'deftype* (symbol (-/name (-/ns-name -/*ns*)) (-/name tname)) classname (vector a) :implements (vec i) m))))
                )
            )
        )
    )
)
)

(about #_"arbace.arm.Seqable"
    (defp Seqable
        (#_"seq" Seqable'''seq [#_"Seqable" this])
    )

    (defn seqable? [x] (satisfies? Seqable x))

    (defn #_"seq" seq [x] (when (some? x) (Seqable'''seq x)))

    (defn empty? [x] (not (seq x)))
)

(about #_"arbace.arm.ISeq"
    (defp ISeq
        (#_"any" ISeq'''first [#_"seq" this])
        (#_"seq" ISeq'''next [#_"seq" this])
    )

    (defn seq? [x] (satisfies? ISeq x))

    (defn first [s] (if (seq? s) (ISeq'''first s) (when-some [s (seq s)] (ISeq'''first s))))

    (defn #_"seq" next [s] (if (seq? s) (ISeq'''next s) (when-some [s (seq s)] (ISeq'''next s))))

    (defn second [s] (first (next s)))
    (defn third  [s] (first (next (next s))))
    (defn fourth [s] (first (next (next (next s)))))
)

(about #_"arbace.arm.IObject"
    (defp IObject
        (#_"boolean" IObject'''equals [#_"IObject" this, #_"any" that])
    )
)

(about #_"arbace.arm.Appendable"
    (defp Appendable
        (#_"Appendable" Appendable'''append [#_"Appendable" this, #_"char|String" x])
    )
)

(about #_"arbace.arm.IAppend"
    (defp IAppend
        (#_"Appendable" IAppend'''append [#_"IAppend" this, #_"Appendable" a])
    )
)

(about #_"arbace.arm.Comparable"
    (defp Comparable
        (#_"int" Comparable'''compareTo [#_"Comparable" this, #_"any" that])
    )

    (defn comparable? [x] (satisfies? Comparable x))
)

(about #_"arbace.arm.Comparator"
    (defp Comparator
        (#_"int" Comparator'''compare [#_"Comparator" this, #_"any" x, #_"any" y])
    )

    (defn comparator? [x] (satisfies? Comparator x))
)

(about #_"arbace.arm.Counted"
    (defp Counted
        (#_"int" Counted'''count [#_"Counted" this])
    )

    (defn counted? [x] (satisfies? Counted x))

    (defn count
        ([x] (count x -1))
        ([x m]
            (cond
                (nil? x)
                    0
                (counted? x)
                    (Counted'''count x)
                (seqable? x)
                    (loop-when [n 0 s (seq x)] (and (some? s) (or (neg? m) (< n m))) => n
                        (when (counted? s) => (recur (inc n) (next s))
                            (+ n (Counted'''count s))
                        )
                    )
                :else
                    (throw! (str "count not supported on " x))
            )
        )
    )
)

(about #_"arbace.arm.Hashed"
    (defp Hashed
        (#_"int" Hashed'''hash [#_"Hashed" this])
    )

    (-/extend-protocol Hashed
        java.lang.Object (Hashed'''hash [o] (Object''hashCode o))
        java.lang.String (Hashed'''hash [s] (Murmur3'hashInt (Object''hashCode s)))
        java.lang.Number (Hashed'''hash [n] (Murmur3'hashInt (int n)))
    )

    (defn hashed? [x] (satisfies? Hashed x))

    (defn f'hash [x] (if (some? x) (Hashed'''hash x) 0))

    (defn f'hashcode [x] (if (some? x) (Object''hashCode x) 0))

    (defn hash-combine [seed x]
        (bit-xor seed (+ (f'hashcode x) 0x9e3779b9 (bit-shift-left seed 6) (bit-shift-right seed 2)))
    )
)

(about #_"arbace.arm.IFn"
    (defp IFn
        (#_"any" IFn'''invoke
            [#_"fn" this]
            [#_"fn" this, a1]
            [#_"fn" this, a1, a2]
            [#_"fn" this, a1, a2, a3]
            [#_"fn" this, a1, a2, a3, a4]
            [#_"fn" this, a1, a2, a3, a4, a5]
            [#_"fn" this, a1, a2, a3, a4, a5, a6]
            [#_"fn" this, a1, a2, a3, a4, a5, a6, a7]
            [#_"fn" this, a1, a2, a3, a4, a5, a6, a7, a8]
            [#_"fn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]
            [#_"fn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args]
        )
        (#_"any" IFn'''applyTo [#_"fn" this, #_"seq" args])
    )

    (defn ifn? [x] (satisfies? IFn x))

    (defn spread [s]
        (cond
            (nil? s) nil
            (nil? (next s)) (seq (first s))
            :else (cons (first s) (spread (next s)))
        )
    )

    (defn list*
        ([s] (seq s))
        ([a s] (cons a s))
        ([a b s] (cons a (cons b s)))
        ([a b c s] (cons a (cons b (cons c s))))
        ([a b c d & s] (cons a (cons b (cons c (cons d (spread s))))))
    )

    (defn apply
        ([#_"fn" f s] (IFn'''applyTo f, (seq s)))
        ([#_"fn" f a s] (IFn'''applyTo f, (list* a s)))
        ([#_"fn" f a b s] (IFn'''applyTo f, (list* a b s)))
        ([#_"fn" f a b c s] (IFn'''applyTo f, (list* a b c s)))
        ([#_"fn" f a b c d & s] (IFn'''applyTo f, (cons a (cons b (cons c (cons d (spread s)))))))
    )
)

(about #_"arbace.arm.INamed"
    (defp INamed
        (#_"String" INamed'''getNamespace [#_"INamed" this])
        (#_"String" INamed'''getName [#_"INamed" this])
    )

    (defn named? [x] (satisfies? INamed x))

    (defn #_"String" namespace [#_"INamed" x] (INamed'''getNamespace x))

    (defn #_"String" name [x] (if (string? x) x (INamed'''getName #_"INamed" x)))
)

(about #_"arbace.arm.IDeref"
    (defp IDeref
        (#_"any" IDeref'''deref [#_"IDeref" this])
    )

    (defn deref [#_"IDeref" ref] (IDeref'''deref ref))
)

(about #_"arbace.arm.IAtom"
    (defp IAtom
        (#_"boolean" IAtom'''compareAndSet [#_"IAtom" this, #_"any" o, #_"any" o'])
        (#_"any" IAtom'''swap [#_"IAtom" this, #_"fn" f, #_"seq" args])
        (#_"any" IAtom'''reset [#_"IAtom" this, #_"any" o'])
    )
)

(about #_"arbace.arm.Sequential"
    (defp Sequential)

    (defn sequential? [x] (satisfies? Sequential x))
)

(about #_"arbace.arm.Reversible"
    (defp Reversible
        (#_"seq" Reversible'''rseq [#_"Reversible" this])
    )

    (defn reversible? [x] (satisfies? Reversible x))

    (defn rseq [#_"Reversible" s] (Reversible'''rseq s))
)

(about #_"arbace.arm.Indexed"
    (defp Indexed
        (#_"any" Indexed'''nth
            [#_"Indexed" this, #_"int" i]
            [#_"Indexed" this, #_"int" i, #_"value" not-found]
        )
    )

    (defn indexed? [x] (satisfies? Indexed x))

    (defn nthnext [s n] (loop-when-recur [s (seq s) n n] (and s (pos? n)) [(next s) (dec n)] => s))
)

(about #_"arbace.arm.ILookup"
    (defp ILookup
        (#_"any" ILookup'''valAt
            [#_"ILookup" this, #_"key" key]
            [#_"ILookup" this, #_"key" key, #_"value" not-found]
        )
    )
)

(about #_"arbace.arm.IPersistentCollection"
    (defp IPersistentCollection
        (#_"IPersistentCollection" IPersistentCollection'''conj [#_"IPersistentCollection" this, #_"any" o])
        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"IPersistentCollection" this])
    )

    (defn coll? [x] (satisfies? IPersistentCollection x))

    (defn conj
        ([] (vector))
        ([c] c)
        ([c x] (if (some? c) (IPersistentCollection'''conj c, x) (list x)))
        ([c x & s]
            (let [c (conj c x)]
                (recur-when s [c (first s) (next s)] => c)
            )
        )
    )

    (defn empty [coll]
        (when (coll? coll)
            (IPersistentCollection'''empty #_"IPersistentCollection" coll)
        )
    )

    (defn not-empty [coll] (when (seq coll) coll))
)

(about #_"arbace.arm.IMapEntry"
    (defp IMapEntry
        (#_"any" IMapEntry'''key [#_"IMapEntry" this])
        (#_"any" IMapEntry'''val [#_"IMapEntry" this])
    )

    (defn map-entry? [x] (satisfies? IMapEntry x))

    (defn key [#_"IMapEntry" e] (IMapEntry'''key e))
    (defn val [#_"IMapEntry" e] (IMapEntry'''val e))

    (defn keys [m] (not-empty (map key m)))
    (defn vals [m] (not-empty (map val m)))
)

(about #_"arbace.arm.Associative"
    (defp Associative
        (#_"Associative" Associative'''assoc [#_"Associative" this, #_"key" key, #_"value" val])
        (#_"boolean" Associative'''containsKey [#_"Associative" this, #_"key" key])
        (#_"IMapEntry" Associative'''entryAt [#_"Associative" this, #_"key" key])
    )

    (defn associative? [x] (satisfies? Associative x))

    (defn assoc
        ([#_"Associative" a k v]
            (if (some? a)
                (Associative'''assoc a, k, v)
                (PersistentArrayMap'new (anew [ k, v ]))
            )
        )
        ([a k v & kvs]
            (let-when [a (assoc a k v)] kvs => a
                (when (next kvs) => (throw! "assoc expects even number of arguments after map/vector, found odd number")
                    (recur a (first kvs) (second kvs) (next (next kvs)))
                )
            )
        )
    )

    (defn assoc-in [m [k & ks] v]
        (if ks
            (assoc m k (assoc-in (get m k) ks v))
            (assoc m k v)
        )
    )

    (defn update
        ([m k f] (assoc m k (f (get m k))))
        ([m k f x] (assoc m k (f (get m k) x)))
        ([m k f x y] (assoc m k (f (get m k) x y)))
        ([m k f x y & z] (assoc m k (apply f (get m k) x y z)))
    )

    (defn update-in [m ks f & args]
        (let [[k & ks] ks]
            (if ks
                (assoc m k (apply update-in (get m k) ks f args))
                (assoc m k (apply f (get m k) args))
            )
        )
    )
)

(about #_"arbace.arm.IPersistentMap"
    (defp IPersistentMap
        (#_"IPersistentMap" IPersistentMap'''dissoc [#_"IPersistentMap" this, #_"key" key])
    )

    (defn map? [x] (satisfies? IPersistentMap x))

    (defn dissoc
        ([m] m)
        ([#_"IPersistentMap" m k] (when (some? m) (IPersistentMap'''dissoc m, k)))
        ([m k & ks]
            (when-some [m (dissoc m k)]
                (recur-when ks [m (first ks) (next ks)] => m)
            )
        )
    )
)

(about #_"arbace.arm.IPersistentSet"
    (defp IPersistentSet
        (#_"IPersistentSet" IPersistentSet'''disj [#_"IPersistentSet" this, #_"key" key])
        (#_"boolean" IPersistentSet'''contains? [#_"IPersistentSet" this, #_"key" key])
        (#_"any" IPersistentSet'''get [#_"IPersistentSet" this, #_"key" key])
    )

    (defn set? [x] (satisfies? IPersistentSet x))

    (defn disj
        ([s] s)
        ([#_"IPersistentSet" s k] (when (some? s) (IPersistentSet'''disj s, k)))
        ([s k & ks]
            (when-some [s (disj s k)]
                (recur-when ks [s (first ks) (next ks)] => s)
            )
        )
    )
)

(about #_"arbace.arm.IPersistentStack"
    (defp IPersistentStack
        (#_"any" IPersistentStack'''peek [#_"IPersistentStack" this])
        (#_"IPersistentStack" IPersistentStack'''pop [#_"IPersistentStack" this])
    )

    (defn stack? [x] (satisfies? IPersistentStack x))

    (defn peek [s]
        (when (some? s)
            (IPersistentStack'''peek s)
        )
    )

    (defn pop [s]
        (when (some? s)
            (IPersistentStack'''pop s)
        )
    )
)

(about #_"arbace.arm.IPersistentList"
    (defp IPersistentList)

    (defn list? [x] (satisfies? IPersistentList x))
)

(about #_"arbace.arm.IPersistentVector"
    (defp IPersistentVector
        (#_"IPersistentVector" IPersistentVector'''assocN [#_"IPersistentVector" this, #_"int" i, #_"value" val])
    )

    (defn vector? [x] (satisfies? IPersistentVector x))
)

(about #_"arbace.arm.Atom"
    (defp Atom)
)

(about #_"arbace.arm.AFn"
    #_abstract
    (defp AFn)
)

(about #_"arbace.arm.Symbol"
    (defp Symbol)

    (defn symbol? [x] (satisfies? Symbol x))
)

(about #_"arbace.arm.Keyword"
    (defp Keyword)

    (defn keyword? [x] (satisfies? Keyword x))
)

(about #_"arbace.arm.Fn"
    #_abstract
    (defp Fn)

    (defn fn? [x] (satisfies? Fn x))
)

(about #_"arbace.arm.Closure"
    (defp Closure)
)

(about #_"arbace.arm.ASeq"
    #_abstract
    (defp ASeq)
)

(about #_"arbace.arm.LazySeq"
    (defp LazySeq)
)

(about #_"arbace.arm.APersistentMap"
    #_abstract
    (defp APersistentMap)
)

(about #_"arbace.arm.APersistentSet"
    #_abstract
    (defp APersistentSet)
)

(about #_"arbace.arm.APersistentVector"
    (defp VSeq)
    (defp RSeq)
    #_abstract
    (defp APersistentVector)
)

(about #_"arbace.arm.AMapEntry"
    #_abstract
    (defp AMapEntry)
)

(about #_"arbace.arm.Cons"
    (defp Cons)
)

(about #_"arbace.arm.MapEntry"
    (defp MapEntry)
)

(about #_"arbace.arm.Namespace"
    (defp Namespace)
)

(about #_"arbace.arm.PersistentArrayMap"
    (defp MSeq)
    (defp PersistentArrayMap)
)

(about #_"arbace.arm.PersistentHashMap"
    (defp INode
        (#_"INode" INode'''assoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key])
        (#_"IMapEntry|any" INode'''find
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key]
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" not-found]
        )
        (#_"seq" INode'''nodeSeq [#_"INode" this])
        (#_"INode" INode'''assocT [#_"INode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissocT [#_"INode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf])
    )

    (defp HSeq)
    (defp NSeq)
    (defp ANode)
    (defp BNode)
    (defp CNode)
    (defp PersistentHashMap)
)

(about #_"arbace.arm.PersistentHashSet"
    (defp PersistentHashSet)
)

(about #_"arbace.arm.PersistentList"
    (defp EmptyList)
    (defp PersistentList)
)

(about #_"arbace.arm.PersistentVector"
    (defp VNode)
    (defp PersistentVector)
)

(about #_"arbace.arm.Var"
    (defp Unbound)
    (defp Var)

    (defn var? [v] (satisfies? Var v))
)

(about #_"defarray"
    (defn aget    [a i] (-/aget a i))
    (defn alength [a]   (-/alength a))

    (defn aclone [a]         (when (some? a) (-/aclone a)))
    (defn acopy! [a i b j n] (System'arraycopy b, j, a, i, n) a)
    (defn aset!  [a i x]     (-/aset a i x) a)
    (defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

    (defn anew [size-or-seq]
        (if (number? size-or-seq)
            (-/object-array (int size-or-seq))
            (let [#_"seq" s (seq size-or-seq) #_"int" n (count s)]
                (loop-when-recur [#_"array" a (-/object-array n) #_"int" i 0 s s] (and (< i n) (some? s)) [(aset! a i (first s)) (inc i) (next s)] => a)
            )
        )
    )

    (defn qset!
        ([a k v]    (.mutate a, k, v))
        ([a k v & kvs]
            (let [a (.mutate a, k, v)]
                (recur-when kvs [a (first kvs) (second kvs) (next (next kvs))] => a)
            )
        )
    )

    (defn qswap!
        ([a k f]         (.mutate a, k,       (f (ILookup''valAt a, k))))
        ([a k f x]       (.mutate a, k,       (f (ILookup''valAt a, k) x)))
        ([a k f x y]     (.mutate a, k,       (f (ILookup''valAt a, k) x y)))
        ([a k f x y & z] (.mutate a, k, (apply f (ILookup''valAt a, k) x y z)))
    )
)

(about #_"append, str, pr, prn"
    (defn #_"Appendable" append-chr [#_"Appendable" a, #_"char" x]
        (-> a (Appendable'''append "\\") (Appendable'''append x))
    )

    (def #_"{char String}" char-escape-string
        (hash-map
            \" "\\\""
            \\ "\\\\"
        )
    )

    (defn #_"Appendable" append-str [#_"Appendable" a, #_"String" x]
        (let [
            a (Appendable'''append a, "\"")
            a (-/reduce (fn [a1 a2] (Appendable'''append a1, (-/get char-escape-string a2 a2))) a x)
            a (Appendable'''append a, "\"")
        ]
            a
        )
    )

    (defp SeqForm)
    (defp VecForm)
    (defp MapForm)
    (defp SetForm)

    (defn #_"Appendable" append* [#_"Appendable" a, #_"String" b, #_"fn" f'append, #_"String" c, #_"String" d, #_"Seqable" q]
        (let [a (let-when [a (Appendable'''append a, b) #_"seq" s (seq q)] (some? s) => a
                    (loop [a a s s]
                        (let-when [a (f'append a (first s)) s (next s)] (some? s) => a
                            (recur (Appendable'''append a, c) s)
                        )
                    )
                )]
            (Appendable'''append a, d)
        )
    )

    (defn #_"Appendable" append-seq [#_"Appendable" a, #_"seq" x]    (append* a "(" append " " ")" x))
    (defn #_"Appendable" append-vec [#_"Appendable" a, #_"vector" x] (append* a "[" append " " "]" x))
    (defn #_"Appendable" append-map [#_"Appendable" a, #_"map" x]    (append* a "{" (fn [a e] (-> a (append (key e)) (Appendable'''append " ") (append (val e)))) ", " "}" x))
    (defn #_"Appendable" append-set [#_"Appendable" a, #_"set" x]    (append* a "#{" append " " "}" x))

    (defn #_"Appendable" append [#_"Appendable" a, #_"any" x]
        (condp = x
            nil   (Appendable'''append a, "nil")
            false (Appendable'''append a, "false")
            true  (Appendable'''append a, "true")
            (cond
                (number? x) (Appendable'''append a, (Number''toString x))
                (string? x) (append-str a x)
                :else
                (condp satisfies? x
                    IAppend (IAppend'''append x, a)
                    SeqForm (append-seq a x)
                    VecForm (append-vec a x)
                    MapForm (append-map a x)
                    SetForm (append-set a x)
                    (cond
                        (seq? x)     (append-seq a x)
                        (vector? x)  (append-vec a x)
                        (map? x)     (append-map a x)
                        (set? x)     (append-set a x)
                        (char? x)    (append-chr a x)
                        :else        (Appendable'''append a, (Object''toString x))
                    )
                )
            )
        )
    )

    (defn #_"Appendable" append! [#_"Appendable" a, #_"any" x]
        (if (or (string? x) (char? x)) (Appendable'''append a, x) (append a x))
    )

    (defn #_"String" str
        ([] "")
        ([x] (if (some? x) (-> (StringBuilder'new) (append! x) (StringBuilder''toString)) ""))
        ([x & s]
            ((fn [#_"StringBuilder" sb s] (recur-when s [(append! sb (first s)) (next s)] => (StringBuilder''toString sb)))
                (-> (StringBuilder'new) (append! x)) s
            )
        )
    )

    (defn flush   [] (Flushable''flush   -/*out*)          nil)

    (defn pr
        ([] nil)
        ([x] (append -/*out* x) nil)
        ([x & s]
            (pr x) (space)
            (let-when [[x & s] s] (some? s) => (pr x)
                (recur x s)
            )
        )
    )

    (defn print
        ([] nil)
        ([x] (append! -/*out* x) nil)
        ([x & s]
            (print x) (space)
            (let-when [[x & s] s] (some? s) => (print x)
                (recur x s)
            )
        )
    )

    (defn prn     [& s] (apply pr    s) (newline) (flush) nil)
    (defn println [& s] (apply print s) (newline) (flush) nil)
)

(about #_"arbace.arm.Murmur3"

(about #_"Murmur3"
    (def #_"int" Murmur3'seed 0)
    (def #_"int" Murmur3'C1 0xcc9e2d51)
    (def #_"int" Murmur3'C2 0x1b873593)

    (defn #_"int" Murmur3'mixK1 [#_"int" k1]
        (-> k1 (* Murmur3'C1) (Number'rotateLeft 15) (* Murmur3'C2))
    )

    (defn #_"int" Murmur3'mixH1 [#_"int" h1, #_"int" k1]
        (-> h1 (bit-xor k1) (Number'rotateLeft 13) (* 5) (+ 0xe6546b64))
    )

    (defn #_"int" Murmur3'fmix [#_"int" h1, #_"int" n]
        (let [h1 (bit-xor h1 n)    h1 (bit-xor h1 (unsigned-bit-shift-right h1 16))
              h1 (* (int h1) 0x85ebca6b) h1 (bit-xor h1 (unsigned-bit-shift-right h1 13))
              h1 (* (int h1) 0xc2b2ae35) h1 (bit-xor h1 (unsigned-bit-shift-right h1 16))]
            h1
        )
    )

    (defn #_"int" Murmur3'hashInt [#_"int" input]
        (when-not (zero? input) => 0
            (let [#_"int" k1 (Murmur3'mixK1 input)
                  #_"int" h1 (Murmur3'mixH1 Murmur3'seed, k1)]
                (Murmur3'fmix h1, 4)
            )
        )
    )

    (defn #_"int" Murmur3'hashUnencodedChars [#_"String" s]
        (let [#_"int" h1
                (loop-when [h1 Murmur3'seed #_"int" i 1] (< i (String''length s)) => h1
                    (let [#_"int" k1 (bit-or (int (String''charAt s, (dec i))) (bit-shift-left (int (String''charAt s, i)) 16))]
                        (recur (Murmur3'mixH1 h1, (Murmur3'mixK1 k1)) (+ i 2))
                    )
                )
              h1
                (when (odd? (String''length s)) => h1
                    (let [#_"int" k1 (int (String''charAt s, (dec (String''length s))))]
                        (bit-xor h1 (Murmur3'mixK1 k1))
                    )
                )]
            (Murmur3'fmix h1, (bit-shift-left (String''length s) 1))
        )
    )

    (defn #_"int" Murmur3'mixCollHash [#_"int" hash, #_"int" n]
        (Murmur3'fmix (Murmur3'mixH1 Murmur3'seed, (Murmur3'mixK1 hash)), n)
    )

    (defn #_"int" Murmur3'hashOrdered [#_"Seqable" items]
        (loop-when-recur [#_"int" hash 1 #_"int" n 0 #_"seq" s (seq items)]
                         (some? s)
                         [(+ (* 31 hash) (f'hash (first s))) (inc n) (next s)]
                      => (Murmur3'mixCollHash hash, n)
        )
    )

    (defn #_"int" Murmur3'hashUnordered [#_"Seqable" items]
        (loop-when-recur [#_"int" hash 0 #_"int" n 0 #_"seq" s (seq items)]
                         (some? s)
                         [(+ hash (f'hash (first s))) (inc n) (next s)]
                      => (Murmur3'mixCollHash hash, n)
        )
    )
)
)

(about #_"arbace.arm.Atom"

(about #_"Atom"
    (defq Atom [#_"AtomicReference" data])

    (defn #_"Atom" Atom'new [#_"any" data]
        (new* Atom'class (anew [(AtomicReference'new data)]))
    )

    (defn #_"any" Atom''deref [#_"Atom" this]
        (AtomicReference''get (:data this))
    )

    (defn #_"boolean" Atom''compareAndSet [#_"Atom" this, #_"any" o, #_"any" o']
        (AtomicReference''compareAndSet (:data this), o, o')
    )

    (defn #_"any" Atom''swap [#_"Atom" this, #_"fn" f, #_"seq" args]
        (loop []
            (let [#_"any" o (AtomicReference''get (:data this)) #_"any" o' (apply f o args)]
                (when (AtomicReference''compareAndSet (:data this), o, o') => (recur)
                    o'
                )
            )
        )
    )

    (defn #_"any" Atom''reset [#_"Atom" this, #_"any" o']
        (AtomicReference''set (:data this), o')
        o'
    )

    (defm Atom IDeref
        (IDeref'''deref => Atom''deref)
    )

    (defm Atom IAtom
        (IAtom'''compareAndSet => Atom''compareAndSet)
        (IAtom'''swap => Atom''swap)
        (IAtom'''reset => Atom''reset)
    )
)

(defn atom [x] (Atom'new x))

(defn compare-and-set! [#_"IAtom" a x x'] (IAtom'''compareAndSet a, x, x'))

(defn swap! [#_"IAtom" a f & args] (IAtom'''swap a, f, args))

(defn reset! [#_"IAtom" a x'] (IAtom'''reset a, x'))
)

(about #_"arbace.arm.Reduced"

(defn reduce
    ([f s] (if-some [s (seq s)] (reduce f (first s) (next s)) (f)))
    ([f r s] (if-some [s (seq s)] (recur f (f r (first s)) (next s)) r))
)

(defn into [to from] (reduce conj to from))
)

(about #_"arbace.arm.Util"

(about #_"Util"
    (defn #_"boolean" Util'equiv [#_"any" a, #_"any" b]
        (cond
            (identical? a b)              true
            (nil? a)                      false
            (and (number? a) (number? b)) (-/== a b)
            (coll? a)                     (IObject'''equals a, b)
            (coll? b)                     (IObject'''equals b, a)
            :else                         (IObject'''equals a, b)
        )
    )
)

#_oops!
(defn =
    ([x] true)
    ([x y] (Util'equiv x y))
    ([x y & s] (and (= x y) (recur-when (next s) [y (first s) (next s)] => (= y (first s)))))
)

(defn not=
    ([x] false)
    ([x y] (not (= x y)))
    ([x y & s] (not (apply = x y s)))
)

(about #_"Util"
    (defn #_"int" Util'compare [#_"any" a, #_"any" b]
        (cond
            (= a b)     0
            (nil? a)   -1
            (nil? b)    1
            (number? a) (cond (-/< a (int b)) -1 (-/> a (int b)) 1 :else 0)
            :else       (Comparable''compareTo a, b)
        )
    )
)

(defn compare [x y] (Util'compare x, y))
)

(about #_"arbace.arm.Numbers"

(defn <
    ([x] true)
    ([x y] (-/< (int x) (int y)))
    ([x y & s] (and (< x y) (recur-when (next s) [y (first s) (next s)] => (< y (first s)))))
)

(defn <=
    ([x] true)
    ([x y] (-/<= (int x) (int y)))
    ([x y & s] (and (<= x y) (recur-when (next s) [y (first s) (next s)] => (<= y (first s)))))
)

(defn >
    ([x] true)
    ([x y] (-/> (int x) (int y)))
    ([x y & s] (and (> x y) (recur-when (next s) [y (first s) (next s)] => (> y (first s)))))
)

(defn >=
    ([x] true)
    ([x y] (-/>= (int x) (int y)))
    ([x y & s] (and (>= x y) (recur-when (next s) [y (first s) (next s)] => (>= y (first s)))))
)

(defn max
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & s] (reduce max (max x y) s))
)

(defn min
    ([x] x)
    ([x y] (if (< x y) x y))
    ([x y & s] (reduce min (min x y) s))
)

(defn zero? [n] (-/= (int n) 0))
(defn pos?  [n] (-/> (int n) 0))
(defn neg?  [n] (-/< (int n) 0))

(defn +
    ([] 0)
    ([x] #_"Number" x)
    ([x y] (-/unchecked-add-int (int x) (int y)))
    ([x y & s] (reduce + (+ x y) s))
)

(defn -
    ([x] (-/unchecked-negate-int (int x)))
    ([x y] (-/unchecked-subtract-int (int x) (int y)))
    ([x y & s] (reduce - (- x y) s))
)

(defn abs [a] (if (neg? a) (- a) a))

(defn inc [x] (-/unchecked-inc-int (int x)))
(defn dec [x] (-/unchecked-dec-int (int x)))

(defn *
    ([] 1)
    ([x] #_"Number" x)
    ([x y] (-/unchecked-multiply-int (int x) (int y)))
    ([x y & s] (reduce * (* x y) s))
)

(defn quot [x y]
    (when-not (zero? y) => (throw! "divide by zero")
        (-/unchecked-divide-int (int x) (int y))
    )
)

(defn rem [x y]
    (when-not (zero? y) => (throw! "divide by zero")
        (-/unchecked-remainder-int (int x) (int y))
    )
)

(defn mod [x y]
    (let-when [m (rem x y)] (or (zero? m) (= (pos? x) (pos? y))) => (+ m y)
        m
    )
)

(defn bit-not [x] (-/bit-not (int x)))

(defn bit-and
    ([x y] (-/bit-and (int x) (int y)))
    ([x y & s] (reduce bit-and (bit-and x y) s))
)

(defn bit-or
    ([x y] (-/bit-or (int x) (int y)))
    ([x y & s] (reduce bit-or (bit-or x y) s))
)

(defn bit-xor
    ([x y] (-/bit-xor (int x) (int y)))
    ([x y & s] (reduce bit-xor (bit-xor x y) s))
)

(defn bit-clear [x i] (-/bit-and (int x) (-/bit-not (-/bit-shift-left 1 (int i)))))
(defn bit-set   [x i] (-/bit-or (int x) (-/bit-shift-left 1 (int i))))
(defn bit-flip  [x i] (-/bit-xor (int x) (-/bit-shift-left 1 (int i))))

(defn bit-test  [x i] (-/not= (-/bit-and (int x) (-/bit-shift-left 1 (int i))) 0))

(defn          bit-shift-left  [x n] (-/bit-shift-left           (int x) (int n)))
(defn          bit-shift-right [x n] (-/bit-shift-right          (int x) (int n)))
(defn unsigned-bit-shift-right [x n] (-/unsigned-bit-shift-right (int x) (int n)))

(defn even? [n]
    (when (integer? n) => (throw! (str "argument must be an integer: " n))
        (zero? (bit-and n 1))
    )
)

(defn odd? [n] (not (even? n)))
)

(about #_"arbace.arm.AFn"

(about #_"AFn"
    (defn #_"void" AFn'throwArity [#_"fn" f, #_"int" n]
        (throw! (str "wrong number of args (" (if (neg? n) (str "more than " (dec (- n))) n) ") passed to " f))
    )

    (defn #_"any" AFn'applyTo [#_"fn" f, #_"seq" s]
        (condp = (count s (inc 9))
            0                                           (IFn'''invoke f)
            1 (let [[a1] s]                             (IFn'''invoke f, a1))
            2 (let [[a1 a2] s]                          (IFn'''invoke f, a1, a2))
            3 (let [[a1 a2 a3] s]                       (IFn'''invoke f, a1, a2, a3))
            4 (let [[a1 a2 a3 a4] s]                    (IFn'''invoke f, a1, a2, a3, a4))
            5 (let [[a1 a2 a3 a4 a5] s]                 (IFn'''invoke f, a1, a2, a3, a4, a5))
            6 (let [[a1 a2 a3 a4 a5 a6] s]              (IFn'''invoke f, a1, a2, a3, a4, a5, a6))
            7 (let [[a1 a2 a3 a4 a5 a6 a7] s]           (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7))
            8 (let [[a1 a2 a3 a4 a5 a6 a7 a8] s]        (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7, a8))
            9 (let [[a1 a2 a3 a4 a5 a6 a7 a8 a9] s]     (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7, a8, a9))
              (let [[a1 a2 a3 a4 a5 a6 a7 a8 a9 & s] s] (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7, a8, a9, s))
        )
    )
)
)

(about #_"arbace.arm.Symbol"

(about #_"Symbol"
    (defq Symbol [#_"String" ns, #_"String" name])

    #_inherit
    (defm Symbol AFn)

    (defn #_"Symbol" Symbol'new [#_"String" ns, #_"String" name]
        (new* Symbol'class (anew [ns, name]))
    )

    (defn #_"Symbol" Symbol'intern
        ([#_"String" nsname]
            (let [#_"int" i (String''indexOf nsname, (int (char! "/")))]
                (if (or (= i -1) (= nsname "/"))
                    (Symbol'new nil, nsname)
                    (Symbol'new (String''substring nsname, 0, i), (String''substring nsname, (inc i)))
                )
            )
        )
        ([#_"String" ns, #_"String" name]
            (Symbol'new ns, name)
        )
    )

    (defn #_"boolean" Symbol''equals [#_"Symbol" this, #_"any" that]
        (or (identical? this that)
            (and (symbol? that) (= (:ns this) (:ns that)) (= (:name this) (:name that)))
        )
    )

    (defn #_"Appendable" Symbol''append [#_"Symbol" this, #_"Appendable" a]
        (if (some? (:ns this)) (-> a (Appendable'''append (:ns this)) (Appendable'''append "/") (Appendable'''append (:name this))) (Appendable'''append a, (:name this)))
    )

    (defn #_"int" Symbol''hash [#_"Symbol" this]
        (hash-combine (Murmur3'hashUnencodedChars (:name this)) (:ns this))
    )

    (defn #_"any" Symbol''invoke
        ([#_"Symbol" this, #_"any" obj] (get obj this))
        ([#_"Symbol" this, #_"any" obj, #_"value" not-found] (get obj this not-found))
    )

    (defn #_"int" Symbol''compareTo [#_"Symbol" this, #_"Symbol" that]
        (cond
            (= this that)                              0
            (and (nil? (:ns this)) (some? (:ns that))) -1
            (nil? (:ns this))                          (compare (:name this) (:name that))
            (nil? (:ns that))                          1
            :else
                (let-when [#_"int" cmp (compare (:ns this) (:ns that))] (zero? cmp) => cmp
                    (compare (:name this) (:name that))
                )
        )
    )

    (defm Symbol INamed
        (INamed'''getNamespace => :ns)
        (INamed'''getName => :name)
    )

    (defm Symbol IObject
        (IObject'''equals => Symbol''equals)
    )

    (defm Symbol IAppend
        (IAppend'''append => Symbol''append)
    )

    (defm Symbol Hashed
        (Hashed'''hash => Symbol''hash)
    )

    (defm Symbol IFn
        (IFn'''invoke => Symbol''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm Symbol Comparable
        (Comparable'''compareTo => Symbol''compareTo)
    )
)

(defn symbol
    ([name] (if (symbol? name) name (Symbol'intern name)))
    ([ns name] (Symbol'intern ns, name))
)
)

(about #_"arbace.arm.Keyword"

(about #_"Keyword"
    (defq Keyword [#_"Symbol" sym, #_"int" _hash])

    #_inherit
    (defm Keyword AFn)

    (defn #_"Keyword" Keyword'new [#_"Symbol" sym]
        (new* Keyword'class (anew [sym, (+ (f'hash sym) 0x9e3779b9)]))
    )

    (defn #_"Keyword" Keyword'intern [#_"Symbol" sym]
        (Keyword'new sym)
    )

    (defn #_"String" Keyword''getNamespace [#_"Keyword" this]
        (INamed'''getNamespace (:sym this))
    )

    (defn #_"String" Keyword''getName [#_"Keyword" this]
        (INamed'''getName (:sym this))
    )

    (defn #_"boolean" Keyword''equals [#_"Keyword" this, #_"any" that]
        (or (identical? this that)
            (Symbol''equals (:sym this), (:sym that))
        )
    )

    (defn #_"Appendable" Keyword''append [#_"Keyword" this, #_"Appendable" a]
        (-> a (Appendable'''append ":") (append (:sym this)))
    )

    (defn #_"any" Keyword''invoke
        ([#_"Keyword" this, #_"any" obj] (get obj this))
        ([#_"Keyword" this, #_"any" obj, #_"value" not-found] (get obj this not-found))
    )

    (defn #_"int" Keyword''compareTo [#_"Keyword" this, #_"Keyword" that]
        (compare (:sym this) (:sym that))
    )

    (defm Keyword INamed
        (INamed'''getNamespace => Keyword''getNamespace)
        (INamed'''getName => Keyword''getName)
    )

    (defm Keyword Hashed
        (Hashed'''hash => :_hash)
    )

    (defm Keyword IObject
        (IObject'''equals => Keyword''equals)
    )

    (defm Keyword IAppend
        (IAppend'''append => Keyword''append)
    )

    (defm Keyword IFn
        (IFn'''invoke => Keyword''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm Keyword Comparable
        (Comparable'''compareTo => Keyword''compareTo)
    )
)

(defn keyword
    ([name]
        (cond
            (keyword? name) name
            (symbol? name) (Keyword'intern #_"Symbol" name)
            (string? name) (Keyword'intern (symbol #_"String" name))
        )
    )
    ([ns name] (Keyword'intern (symbol ns name)))
)
)

(about #_"arbace.arm.Fn"

(about #_"Fn"
    (defq Fn [])

    #_inherit
    (defm Fn AFn)

    (defn #_"Fn" Fn'new []
        (new* Fn'class (anew []))
    )

    (defn #_"any" Fn''invoke
        ([#_"Fn" this]                                                   (AFn'throwArity this,   0))
        ([#_"Fn" this, a1]                                               (AFn'throwArity this,   1))
        ([#_"Fn" this, a1, a2]                                           (AFn'throwArity this,   2))
        ([#_"Fn" this, a1, a2, a3]                                       (AFn'throwArity this,   3))
        ([#_"Fn" this, a1, a2, a3, a4]                                   (AFn'throwArity this,   4))
        ([#_"Fn" this, a1, a2, a3, a4, a5]                               (AFn'throwArity this,   5))
        ([#_"Fn" this, a1, a2, a3, a4, a5, a6]                           (AFn'throwArity this,   6))
        ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7]                       (AFn'throwArity this,   7))
        ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7, a8]                   (AFn'throwArity this,   8))
        ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]               (AFn'throwArity this,   9))
        ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args] (AFn'throwArity this, -10))
    )

    (defn #_"int" Fn''compare [#_"Fn" this, #_"any" o1, #_"any" o2]
        (let [#_"any" o (IFn'''invoke this, o1, o2)]
            (if (boolean? o)
                (cond (boolean o) -1 (boolean (IFn'''invoke this, o2, o1)) 1 :else 0)
                (int o)
            )
        )
    )

    (defm Fn IFn
        (IFn'''invoke => Fn''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm Fn Comparator
        (Comparator'''compare => Fn''compare)
    )
)
)

(about #_"arbace.arm.Closure"

(about #_"Closure"
    (defq Closure [#_"FnExpr" fun, #_"map'" _env])

    #_inherit
    (defm Closure Fn AFn)

    (defn #_"Closure" Closure'new [#_"FnExpr" fun, #_"map" env]
        (new* Closure'class (anew [fun, (atom env)]))
    )

    (defn #_"any" Closure''invoke
        ([#_"Closure" this]                                                 (IFn'''applyTo this, nil))
        ([#_"Closure" this, a1]                                             (IFn'''applyTo this, (list a1)))
        ([#_"Closure" this, a1, a2]                                         (IFn'''applyTo this, (list a1 a2)))
        ([#_"Closure" this, a1, a2, a3]                                     (IFn'''applyTo this, (list a1 a2 a3)))
        ([#_"Closure" this, a1, a2, a3, a4]                                 (IFn'''applyTo this, (list a1 a2 a3 a4)))
        ([#_"Closure" this, a1, a2, a3, a4, a5]                             (IFn'''applyTo this, (list a1 a2 a3 a4 a5)))
        ([#_"Closure" this, a1, a2, a3, a4, a5, a6]                         (IFn'''applyTo this, (list a1 a2 a3 a4 a5 a6)))
        ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7]                     (IFn'''applyTo this, (list a1 a2 a3 a4 a5 a6 a7)))
        ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7, a8]                 (IFn'''applyTo this, (list a1 a2 a3 a4 a5 a6 a7 a8)))
        ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]             (IFn'''applyTo this, (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
        ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" a*] (IFn'''applyTo this, (list* a1 a2 a3 a4 a5 a6 a7 a8 a9 a*)))
    )

    (defn #_"any" Closure''applyTo [#_"Closure" this, #_"seq" args]
        (let [
            #_"FnMethod" fm
                (let [#_"int" m (inc Compiler'MAX_POSITIONAL_ARITY) #_"int" n (min (count args m) m)]
                    (or (get (:regulars (:fun this)) n)
                        (let-when [fm (:variadic (:fun this))] (and (some? fm) (<= (dec (- (:arity fm))) n)) => (AFn'throwArity this, (if (< n m) n (- m)))
                            fm
                        )
                    )
                )
            #_"array" vars
                (let [
                    #_"int" m (inc (reduce max (inc -1) (map :idx (vals (deref (:'locals fm))))))
                    #_"int" n (:arity fm) n (if (neg? n) (- n) (inc n))
                ]
                    (loop-when-recur [vars (-> (anew m) (aset! 0 this)) #_"int" i 1 #_"seq" s (seq args)]
                                     (< i n)
                                     [(aset! vars i (first s)) (inc i) (next s)]
                                  => (if (some? s) (aset! vars i s) vars)
                    )
                )
        ]
            (Machine'compute (compile-and-memoize fm), vars)
        )
    )

    (defm Closure IFn
        (IFn'''invoke => Closure''invoke)
        (IFn'''applyTo => Closure''applyTo)
    )

    (defm Closure Comparator
        (Comparator'''compare => Fn''compare)
    )
)
)

(about #_"arbace.arm.ASeq"

(about #_"ASeq"
    (defn #_"boolean" ASeq''equals [#_"ASeq" this, #_"any" that]
        (or (identical? this that)
            (and (sequential? that)
                (loop-when [#_"seq" s (seq this) #_"seq" z (seq that)] (some? s) => (nil? z)
                    (and (some? z) (= (first s) (first z)) (recur (next s) (next z)))
                )
            )
        )
    )
)
)

(about #_"arbace.arm.Cons"

(about #_"Cons"
    (defq Cons [#_"any" car, #_"seq" cdr] SeqForm)

    #_inherit
    (defm Cons ASeq)

    (defn #_"Cons" Cons'new [#_"any" car, #_"seq" cdr]
        (new* Cons'class (anew [car, cdr]))
    )

    (defn #_"seq" Cons''seq [#_"Cons" this]
        this
    )

    (defn #_"seq" Cons''next [#_"Cons" this]
        (seq (:cdr this))
    )

    (defn #_"int" Cons''count [#_"Cons" this]
        (inc (count (:cdr this)))
    )

    (defm Cons Sequential)

    (defm Cons Seqable
        (Seqable'''seq => Cons''seq)
    )

    (defm Cons ISeq
        (ISeq'''first => :car)
        (ISeq'''next => Cons''next)
    )

    (defm Cons Counted
        (Counted'''count => Cons''count)
    )

    (defm Cons Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Cons IObject
        (IObject'''equals => ASeq''equals)
    )
)

(defn cons [x s] (Cons'new x, (seq s)))
)

(about #_"arbace.arm.LazySeq"

(about #_"LazySeq"
    (defq LazySeq [#_"fn'" f, #_"any'" o, #_"seq'" s] SeqForm)

    (defn #_"LazySeq" LazySeq'init [#_"fn" f, #_"seq" s]
        (new* LazySeq'class (anew [(atom f), (atom nil), (atom s)]))
    )

    (defn #_"LazySeq" LazySeq'new [#_"fn" f]
        (LazySeq'init f, nil)
    )

    (defn #_"cons" LazySeq''conj [#_"LazySeq" this, #_"any" o]
        (cons o this)
    )

    (defn #_"IPersistentCollection" LazySeq''empty [#_"LazySeq" this]
        (list)
    )

    (defn #_"seq" LazySeq''seq [#_"LazySeq" this]
        (locking this
            (letfn [(step- [this]
                        (when-some [#_"fn" f (deref (:f this))]
                            (reset! (:f this) nil)
                            (reset! (:o this) (f))
                        )
                        (or (deref (:o this)) (deref (:s this)))
                    )]
                (step- this)
                (when-some [#_"any" o (deref (:o this))]
                    (reset! (:o this) nil)
                    (reset! (:s this) (loop-when-recur o (satisfies? LazySeq o) (step- o) => (seq o)))
                )
                (deref (:s this))
            )
        )
    )

    (defn #_"any" LazySeq''first [#_"LazySeq" this]
        (when-some [#_"seq" s (seq this)]
            (first s)
        )
    )

    (defn #_"seq" LazySeq''next [#_"LazySeq" this]
        (when-some [#_"seq" s (seq this)]
            (next s)
        )
    )

    (defn #_"boolean" LazySeq''equals [#_"LazySeq" this, #_"any" that]
        (if-some [#_"seq" s (seq this)]
            (= s that)
            (and (sequential? that) (nil? (seq that)))
        )
    )

    (defm LazySeq IPersistentCollection
        (IPersistentCollection'''conj => LazySeq''conj)
        (IPersistentCollection'''empty => LazySeq''empty)
    )

    (defm LazySeq Sequential)

    (defm LazySeq Seqable
        (Seqable'''seq => LazySeq''seq)
    )

    (defm LazySeq ISeq
        (ISeq'''first => LazySeq''first)
        (ISeq'''next => LazySeq''next)
    )

    (defm LazySeq IObject
        (IObject'''equals => LazySeq''equals)
    )

    (defm LazySeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )
)

(defmacro lazy-seq [& body] `(LazySeq'new (fn* [] ~@body)))

(defn concat
    ([] (lazy-seq nil))
    ([x] (lazy-seq x))
    ([x y]
        (lazy-seq
            (let-when [s (seq x)] s => y
                (cons (first s) (concat (next s) y))
            )
        )
    )
    ([x y & z]
        (letfn [(cat- [s z]
                    (lazy-seq
                        (let [s (seq s)]
                            (cond
                                s (cons (first s) (cat- (next s) z))
                                z (cat- (first z) (next z))
                            )
                        )
                    )
                )]
            (cat- (concat x y) z)
        )
    )
)

(defn comp
    ([] identity)
    ([f] f)
    ([f g]
        (fn
            ([] (f (g)))
            ([x] (f (g x)))
            ([x y] (f (g x y)))
            ([x y & z] (f (apply g x y z)))
        )
    )
    ([f g & fs] (reduce comp (list* f g fs)))
)

(defn every? [f? s]
    (cond
        (nil? (seq s)) true
        (f? (first s)) (recur f? (next s))
        :else false
    )
)

(defn index-of [s x]
    (loop-when [i 0 s (seq s)] (some? s) => -1
        (when-not (= (first s) x) => i
            (recur (inc i) (next s))
        )
    )
)

(defn some [f? s]
    (when (seq s)
        (or (f? (first s)) (recur f? (next s)))
    )
)

(defn map
    ([f]
        (fn [g]
            (fn
                ([] (g))
                ([x] (g x))
                ([x y] (g x (f y)))
                ([x y & s] (g x (apply f y s)))
            )
        )
    )
    ([f s]
        (lazy-seq
            (when-some [s (seq s)]
                (cons (f (first s)) (map f (next s)))
            )
        )
    )
    ([f s1 s2]
        (lazy-seq
            (let-when [s1 (seq s1) s2 (seq s2)] (and s1 s2)
                (cons (f (first s1) (first s2)) (map f (next s1) (next s2)))
            )
        )
    )
    ([f s1 s2 s3]
        (lazy-seq
            (let-when [s1 (seq s1) s2 (seq s2) s3 (seq s3)] (and s1 s2 s3)
                (cons (f (first s1) (first s2) (first s3)) (map f (next s1) (next s2) (next s3)))
            )
        )
    )
    ([f s1 s2 s3 & z]
        (letfn [(map- [s]
                    (lazy-seq
                        (let-when [s (map seq s)] (every? identity s)
                            (cons (map first s) (map- (map next s)))
                        )
                    )
                )]
            (map (fn [s] (apply f s)) (map- (conj z s3 s2 s1)))
        )
    )
)

(defn mapcat [f & s] (apply concat (apply map f s)))

(defn filter [f? s]
    (lazy-seq
        (when-some [s (seq s)]
            (let-when [x (first s)] (f? x) => (filter f? (next s))
                (cons x (filter f? (next s)))
            )
        )
    )
)

(defn take [n s]
    (lazy-seq
        (when (pos? n)
            (when-some [s (seq s)]
                (cons (first s) (take (dec n) (next s)))
            )
        )
    )
)

(defn drop [n s]
    (letfn [(drop- [n s]
                (let [s (seq s)]
                    (recur-when (and (pos? n) s) [(dec n) (next s)] => s)
                )
            )]
        (lazy-seq (drop- n s))
    )
)

(defn take-nth [n s]
    (lazy-seq
        (when-some [s (seq s)]
            (cons (first s) (take-nth n (drop n s)))
        )
    )
)

(defn interleave
    ([] (list))
    ([c1] (lazy-seq c1))
    ([c1 c2]
        (lazy-seq
            (let-when [s1 (seq c1) s2 (seq c2)] (and s1 s2)
                (cons (first s1) (cons (first s2) (interleave (next s1) (next s2))))
            )
        )
    )
    ([c1 c2 & cs]
        (lazy-seq
            (let-when [ss (map seq (conj cs c2 c1))] (every? identity ss)
                (concat (map first ss) (apply interleave (map next ss)))
            )
        )
    )
)

(defn partition
    ([n s] (partition n n s))
    ([n step s]
        (lazy-seq
            (when-some [s (seq s)]
                (let-when [p (take n s)] (= (count p) n)
                    (cons p (partition n step (nthnext s step)))
                )
            )
        )
    )
    ([n step pad s]
        (lazy-seq
            (when-some [s (seq s)]
                (let-when [p (take n s)] (= (count p) n) => (list (take n (concat p pad)))
                    (cons p (partition n step pad (nthnext s step)))
                )
            )
        )
    )
)
)

(about #_"arbace.arm.APersistentMap"

(about #_"APersistentMap"
    (defn #_"IPersistentCollection" APersistentMap''conj [#_"APersistentMap" this, #_"any" o]
        (condp satisfies? o
            IMapEntry
                (assoc this (key o) (val o))
            IPersistentVector
                (when (= (count o) 2) => (throw! "vector arg to map conj must be a pair")
                    (assoc this (nth o 0) (nth o 1))
                )
            #_else
                (loop-when [this this #_"seq" s (seq o)] (some? s) => this
                    (let [#_"pair" e (first s)]
                        (recur (assoc this (key e) (val e)) (next s))
                    )
                )
        )
    )

    (defn #_"boolean" APersistentMap''equals [#_"APersistentMap" this, #_"any" that]
        (or (identical? this that)
            (and (map? that) (= (count that) (count this))
                (loop-when [#_"seq" s (seq this)] (some? s) => true
                    (let [#_"pair" e (first s) #_"any" k (key e)]
                        (and (contains? that k) (= (val e) (get that k))
                            (recur (next s))
                        )
                    )
                )
            )
        )
    )

    (defn #_"any" APersistentMap''invoke
        ([#_"APersistentMap" this, #_"key" key] (get this key))
        ([#_"APersistentMap" this, #_"key" key, #_"value" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.arm.APersistentSet"

(about #_"APersistentSet"
    (defn #_"boolean" APersistentSet''equals [#_"APersistentSet" this, #_"any" that]
        (or (identical? this that)
            (and (set? that) (= (count this) (count that))
                (loop-when [#_"seq" s (seq that)] (some? s) => true
                    (and (contains? this (first s)) (recur (next s)))
                )
            )
        )
    )

    (defn #_"any" APersistentSet''invoke
        ([#_"APersistentSet" this, #_"key" key] (get this key))
        ([#_"APersistentSet" this, #_"key" key, #_"value" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.arm.APersistentVector"

(about #_"VSeq"
    (defq VSeq [#_"vector" v, #_"int" i] SeqForm)

    #_inherit
    (defm VSeq ASeq)

    (defn #_"VSeq" VSeq'new [#_"vector" v, #_"int" i]
        (new* VSeq'class (anew [v, i]))
    )

    (defn #_"seq" VSeq''seq [#_"VSeq" this]
        this
    )

    (defn #_"any" VSeq''first [#_"VSeq" this]
        (nth (:v this) (:i this))
    )

    (defn #_"seq" VSeq''next [#_"VSeq" this]
        (when (< (inc (:i this)) (count (:v this)))
            (VSeq'new (:v this), (inc (:i this)))
        )
    )

    (defn #_"int" VSeq''count [#_"VSeq" this]
        (- (count (:v this)) (:i this))
    )

    (defm VSeq Sequential)

    (defm VSeq Seqable
        (Seqable'''seq => VSeq''seq)
    )

    (defm VSeq ISeq
        (ISeq'''first => VSeq''first)
        (ISeq'''next => VSeq''next)
    )

    (defm VSeq Counted
        (Counted'''count => VSeq''count)
    )

    (defm VSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm VSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"RSeq"
    (defq RSeq [#_"vector" v, #_"int" i] SeqForm)

    #_inherit
    (defm RSeq ASeq)

    (defn #_"RSeq" RSeq'new [#_"vector" v, #_"int" i]
        (new* RSeq'class (anew [v, i]))
    )

    (defn #_"seq" RSeq''seq [#_"RSeq" this]
        this
    )

    (defn #_"any" RSeq''first [#_"RSeq" this]
        (nth (:v this) (:i this))
    )

    (defn #_"seq" RSeq''next [#_"RSeq" this]
        (when (pos? (:i this))
            (RSeq'new (:v this), (dec (:i this)))
        )
    )

    (defn #_"int" RSeq''count [#_"RSeq" this]
        (inc (:i this))
    )

    (defm RSeq Sequential)

    (defm RSeq Seqable
        (Seqable'''seq => RSeq''seq)
    )

    (defm RSeq ISeq
        (ISeq'''first => RSeq''first)
        (ISeq'''next => RSeq''next)
    )

    (defm RSeq Counted
        (Counted'''count => RSeq''count)
    )

    (defm RSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm RSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)
)

(about #_"arbace.arm.AMapEntry"

(about #_"AMapEntry"
    (defn #_"any" AMapEntry''nth
        ([#_"AMapEntry" this, #_"int" i]
            (condp = i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) (throw! "index is out of bounds"))
        )
        ([#_"AMapEntry" this, #_"int" i, #_"value" not-found]
            (condp = i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) not-found)
        )
    )

    (defn #_"int" AMapEntry''count [#_"AMapEntry" this]
        2
    )

    (defn #_"seq" AMapEntry''seq [#_"AMapEntry" this]
        (VSeq'new this, 0)
    )

    (defn #_"seq" AMapEntry''rseq [#_"AMapEntry" this]
        (RSeq'new this, 1)
    )

    (defn #_"boolean" AMapEntry''equals [#_"AMapEntry" this, #_"any" that]
        (or (identical? this that)
            (cond
                (vector? that)
                    (and (= (count that) 2) (= (nth that 0) (IMapEntry'''key this)) (= (nth that 1) (IMapEntry'''val this)))
                (sequential? that)
                    (loop-when [#_"int" i 0 #_"seq" s (seq that)] (< i 2) => (nil? s)
                        (recur-when (and (some? s) (= (Indexed'''nth this, i) (first s))) [(inc i) (next s)] => false)
                    )
                :else
                    false
            )
        )
    )

    (defn #_"int" AMapEntry''hash [#_"AMapEntry" this]
        (loop-when [#_"int" hash 1 #_"int" i 0] (< i 2) => (Murmur3'mixCollHash hash, i)
            (recur (+ (* 31 hash) (f'hash (Indexed'''nth this, i))) (inc i))
        )
    )

    (defn #_"int" AMapEntry''compareTo [#_"AMapEntry" this, #_"IPersistentVector" that]
        (when-not (identical? this that) => 0
            (let [#_"int" m (count that)]
                (cond (< 2 m) -1 (< m 2) 1
                    :else
                        (loop-when [#_"int" i 0] (< i 2) => 0
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

(about #_"arbace.arm.MapEntry"

(about #_"MapEntry"
    (defq MapEntry [#_"key" k, #_"value" v] VecForm)

    #_inherit
    (defm MapEntry AMapEntry APersistentVector AFn)

    (defn #_"MapEntry" MapEntry'new [#_"key" k, #_"value" v]
        (new* MapEntry'class (anew [k, v]))
    )

    (defm MapEntry IMapEntry
        (IMapEntry'''key => :k)
        (IMapEntry'''val => :v)
    )

    (defm MapEntry Sequential)

    (defm MapEntry Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm MapEntry Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm MapEntry Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm MapEntry Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm MapEntry IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm MapEntry Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm MapEntry Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)
)

(about #_"arbace.arm.PersistentList"

(about #_"EmptyList"
    (defq EmptyList [] SeqForm)

    (defn #_"EmptyList" EmptyList'new []
        (new* EmptyList'class (anew []))
    )

    (def #_"int" EmptyList'HASH (Murmur3'hashOrdered nil))

    (defn #_"int" EmptyList''hash [#_"EmptyList" this]
        EmptyList'HASH
    )

    (defn #_"boolean" EmptyList''equals [#_"EmptyList" this, #_"any" that]
        (and (sequential? that) (nil? (seq that)))
    )

    (defn #_"seq" EmptyList''seq [#_"EmptyList" this]
        nil
    )

    (defn #_"any" EmptyList''first [#_"EmptyList" this]
        nil
    )

    (defn #_"seq" EmptyList''next [#_"EmptyList" this]
        nil
    )

    (defn #_"int" EmptyList''count [#_"EmptyList" this]
        0
    )

    (defn #_"PersistentList" EmptyList''conj [#_"EmptyList" this, #_"any" o]
        (PersistentList'new o, nil, 1)
    )

    (defn #_"EmptyList" EmptyList''empty [#_"EmptyList" this]
        this
    )

    (defn #_"any" EmptyList''peek [#_"EmptyList" this]
        nil
    )

    (defn #_"IPersistentList" EmptyList''pop [#_"EmptyList" this]
        (throw! "can't pop the empty list")
    )

    (defm EmptyList IPersistentList Sequential)

    (defm EmptyList Hashed
        (Hashed'''hash => EmptyList''hash)
    )

    (defm EmptyList IObject
        (IObject'''equals => EmptyList''equals)
    )

    (defm EmptyList Seqable
        (Seqable'''seq => EmptyList''seq)
    )

    (defm EmptyList ISeq
        (ISeq'''first => EmptyList''first)
        (ISeq'''next => EmptyList''next)
    )

    (defm EmptyList Counted
        (Counted'''count => EmptyList''count)
    )

    (defm EmptyList IPersistentCollection
        (IPersistentCollection'''conj => EmptyList''conj)
        (IPersistentCollection'''empty => EmptyList''empty)
    )

    (defm EmptyList IPersistentStack
        (IPersistentStack'''peek => EmptyList''peek)
        (IPersistentStack'''pop => EmptyList''pop)
    )
)

(about #_"PersistentList"
    (defq PersistentList [#_"any" car, #_"IPersistentList" cdr, #_"int" cnt] SeqForm)

    #_inherit
    (defm PersistentList ASeq)

    (defn #_"PersistentList" PersistentList'new
        ([#_"any" car] (PersistentList'new car, nil, 1))
        ([#_"any" car, #_"IPersistentList" cdr, #_"int" cnt]
            (new* PersistentList'class (anew [car, cdr, cnt]))
        )
    )

    (def #_"EmptyList" PersistentList'EMPTY (EmptyList'new))

    (defn #_"PersistentList" PersistentList'create [#_"Reversible" init]
        (into PersistentList'EMPTY (if (satisfies? Reversible init) (rseq init) (reverse init)))
    )

    (defn #_"seq" PersistentList''seq [#_"PersistentList" this]
        this
    )

    (defn #_"PersistentList" PersistentList''conj [#_"PersistentList" this, #_"any" o]
        (PersistentList'new o, this, (inc (:cnt this)))
    )

    (defn #_"PersistentList" PersistentList''empty [#_"PersistentList" this]
        PersistentList'EMPTY
    )

    (defn #_"IPersistentList" PersistentList''pop [#_"PersistentList" this]
        (or (:cdr this) PersistentList'EMPTY)
    )

    (defm PersistentList IPersistentList Sequential)

    (defm PersistentList Seqable
        (Seqable'''seq => PersistentList''seq)
    )

    (defm PersistentList ISeq
        (ISeq'''first => :car)
        (ISeq'''next => :cdr)
    )

    (defm PersistentList Counted
        (Counted'''count => :cnt)
    )

    (defm PersistentList IPersistentCollection
        (IPersistentCollection'''conj => PersistentList''conj)
        (IPersistentCollection'''empty => PersistentList''empty)
    )

    (defm PersistentList IPersistentStack
        (IPersistentStack'''peek => :car)
        (IPersistentStack'''pop => PersistentList''pop)
    )

    (defm PersistentList Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm PersistentList IObject
        (IObject'''equals => ASeq''equals)
    )
)

(defn list
    ([] PersistentList'EMPTY)
    ([& s] (PersistentList'create s))
)

(defn reverse [s] (into (list) s))
)

(about #_"arbace.arm.PersistentArrayMap"

(about #_"MSeq"
    (defq MSeq [#_"array" a, #_"int" i] SeqForm)

    #_inherit
    (defm MSeq ASeq)

    (defn #_"MSeq" MSeq'new [#_"array" a, #_"int" i]
        (new* MSeq'class (anew [a, i]))
    )

    (defn #_"seq" MSeq''seq [#_"MSeq" this]
        this
    )

    (defn #_"pair" MSeq''first [#_"MSeq" this]
        (MapEntry'new (aget (:a this) (:i this)), (aget (:a this) (inc (:i this))))
    )

    (defn #_"seq" MSeq''next [#_"MSeq" this]
        (when (< (+ (:i this) 2) (alength (:a this)))
            (MSeq'new (:a this), (+ (:i this) 2))
        )
    )

    (defn #_"int" MSeq''count [#_"MSeq" this]
        (quot (- (alength (:a this)) (:i this)) 2)
    )

    (defm MSeq Sequential)

    (defm MSeq Seqable
        (Seqable'''seq => MSeq''seq)
    )

    (defm MSeq ISeq
        (ISeq'''first => MSeq''first)
        (ISeq'''next => MSeq''next)
    )

    (defm MSeq Counted
        (Counted'''count => MSeq''count)
    )

    (defm MSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm MSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"PersistentArrayMap"
    (defq PersistentArrayMap [#_"array" array] MapForm)

    #_inherit
    (defm PersistentArrayMap APersistentMap AFn)

    (defn #_"PersistentArrayMap" PersistentArrayMap'new [#_"array" a]
        (new* PersistentArrayMap'class (anew [(or a (anew 0))]))
    )

    (def #_"PersistentArrayMap" PersistentArrayMap'EMPTY (PersistentArrayMap'new nil))

    (defn #_"PersistentArrayMap" PersistentArrayMap''create [#_"PersistentArrayMap" this, #_"array" init]
        (PersistentArrayMap'new init)
    )

    (defn #_"PersistentArrayMap" PersistentArrayMap'createAsIfByAssoc [#_"array" init]
        (when (odd? (alength init))
            (throw! (str "no value supplied for key: " (aget init (dec (alength init)))))
        )
        (let [#_"int" n
                (loop-when [n 0 #_"int" i 0] (< i (alength init)) => n
                    (let [#_"boolean" dup?
                            (loop-when [dup? false #_"int" j 0] (< j i) => dup?
                                (or (= (aget init i) (aget init j))
                                    (recur dup? (+ j 2))
                                )
                            )]
                        (recur (if dup? n (+ n 2)) (+ i 2))
                    )
                )
              init
                (when (< n (alength init)) => init
                    (let [#_"array" nodups (anew n)
                          #_"int" m
                            (loop-when [m 0 #_"int" i 0] (< i (alength init)) => m
                                (let [#_"boolean" dup?
                                        (loop-when [dup? false #_"int" j 0] (< j m) => dup?
                                            (or (= (aget init i) (aget nodups j))
                                                (recur dup? (+ j 2))
                                            )
                                        )
                                      m (when-not dup? => m
                                            (let [#_"int" j
                                                    (loop-when [j (- (alength init) 2)] (<= i j) => j
                                                        (if (= (aget init i) (aget init j))
                                                            j
                                                            (recur (- j 2))
                                                        )
                                                    )]
                                                (aset! nodups m (aget init i))
                                                (aset! nodups (inc m) (aget init (inc j)))
                                                (+ m 2)
                                            )
                                        )]
                                    (recur m (+ i 2))
                                )
                            )]
                        (when (= m n) => (throw! (str "internal error: m=" m))
                            nodups
                        )
                    )
                )]
            (PersistentArrayMap'new init)
        )
    )

    (defn #_"int" PersistentArrayMap''count [#_"PersistentArrayMap" this]
        (quot (alength (:array this)) 2)
    )

    (defn #_"int" PersistentArrayMap'index-of [#_"array" a, #_"key" key]
        (loop-when [#_"int" i 0] (< i (alength a)) => -1
            (if (= (aget a i) key) i (recur (+ i 2)))
        )
    )

    (defn #_"value" PersistentArrayMap''valAt
        ([#_"PersistentArrayMap" this, #_"key" key] (PersistentArrayMap''valAt this, key, nil))
        ([#_"PersistentArrayMap" this, #_"key" key, #_"value" not-found]
            (let [
                #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
            ]
                (if (< -1 i) (aget a (inc i)) not-found)
            )
        )
    )

    (def #_"int" PersistentArrayMap'HASHTABLE_THRESHOLD 16)

    (defn #_"IPersistentMap" PersistentArrayMap''assoc [#_"PersistentArrayMap" this, #_"key" key, #_"value" val]
        (let [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (if (< -1 i)
                (if (= (aget a (inc i)) val)
                    this
                    (PersistentArrayMap''create this, (-> (aclone a) (aset! (inc i) val)))
                )
                (if (< PersistentArrayMap'HASHTABLE_THRESHOLD (alength a))
                    (assoc (PersistentHashMap'create-1a a) key val)
                    (let [
                        #_"int" n (alength a)
                        #_"array" a' (anew (+ n 2))
                        a' (if (pos? n) (acopy! a' 0 a 0 n) a')
                    ]
                        (PersistentArrayMap''create this, (-> a' (aset! n key) (aset! (inc n) val)))
                    )
                )
            )
        )
    )

    (defn #_"boolean" PersistentArrayMap''containsKey [#_"PersistentArrayMap" this, #_"key" key]
        (< -1 (PersistentArrayMap'index-of (:array this), key))
    )

    (defn #_"pair" PersistentArrayMap''entryAt [#_"PersistentArrayMap" this, #_"key" key]
        (let [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (when (< -1 i)
                (MapEntry'new (aget a i), (aget a (inc i)))
            )
        )
    )

    (defn #_"IPersistentMap" PersistentArrayMap''dissoc [#_"PersistentArrayMap" this, #_"key" key]
        (let [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (when (< -1 i) => this
                (let-when [#_"int" n (- (alength a) 2)] (pos? n) => PersistentArrayMap'EMPTY
                    (let [
                        #_"array" a' (-> (anew n) (acopy! 0 a 0 i) (acopy! i a (+ i 2) (- n i)))
                    ]
                        (PersistentArrayMap''create this, a')
                    )
                )
            )
        )
    )

    (defn #_"IPersistentMap" PersistentArrayMap''empty [#_"PersistentArrayMap" this]
        PersistentArrayMap'EMPTY
    )

    (defn #_"seq" PersistentArrayMap''seq [#_"PersistentArrayMap" this]
        (when (pos? (alength (:array this)))
            (MSeq'new (:array this), 0)
        )
    )

    (defm PersistentArrayMap Counted
        (Counted'''count => PersistentArrayMap''count)
    )

    (defm PersistentArrayMap ILookup
        (ILookup'''valAt => PersistentArrayMap''valAt)
    )

    (defm PersistentArrayMap IFn
        (IFn'''invoke => APersistentMap''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm PersistentArrayMap Associative
        (Associative'''assoc => PersistentArrayMap''assoc)
        (Associative'''containsKey => PersistentArrayMap''containsKey)
        (Associative'''entryAt => PersistentArrayMap''entryAt)
    )

    (defm PersistentArrayMap IPersistentMap
        (IPersistentMap'''dissoc => PersistentArrayMap''dissoc)
    )

    (defm PersistentArrayMap IPersistentCollection
        (IPersistentCollection'''conj => APersistentMap''conj)
        (IPersistentCollection'''empty => PersistentArrayMap''empty)
    )

    (defm PersistentArrayMap Seqable
        (Seqable'''seq => PersistentArrayMap''seq)
    )

    (defm PersistentArrayMap IObject
        (IObject'''equals => APersistentMap''equals)
    )

    (defm PersistentArrayMap Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

(defn array-map
    ([] PersistentArrayMap'EMPTY)
    ([& keyvals] (PersistentArrayMap'createAsIfByAssoc (anew keyvals)))
)
)

(about #_"arbace.arm.PersistentHashMap"

(about #_"HSeq"
    (defq HSeq [#_"node[]" nodes, #_"int" i, #_"seq" s] SeqForm)

    #_inherit
    (defm HSeq ASeq)

    (defn #_"HSeq" HSeq'new [#_"node[]" nodes, #_"int" i, #_"seq" s]
        (new* HSeq'class (anew [nodes, i, s]))
    )

    (defn #_"seq" HSeq'create
        ([#_"node[]" nodes] (HSeq'create nodes, 0, nil))
        ([#_"node[]" nodes, #_"int" i, #_"seq" s]
            (when (nil? s) => (HSeq'new nodes, i, s)
                (loop-when i (< i (alength nodes))
                    (when-some [#_"node" node (aget nodes i)] => (recur (inc i))
                        (when-some [s (INode'''nodeSeq node)] => (recur (inc i))
                            (HSeq'new nodes, (inc i), s)
                        )
                    )
                )
            )
        )
    )

    (defn #_"seq" HSeq''seq [#_"HSeq" this]
        this
    )

    (defn #_"pair" HSeq''first [#_"HSeq" this]
        (first (:s this))
    )

    (defn #_"seq" HSeq''next [#_"HSeq" this]
        (HSeq'create (:nodes this), (:i this), (next (:s this)))
    )

    (defm HSeq Sequential)

    (defm HSeq Seqable
        (Seqable'''seq => HSeq''seq)
    )

    (defm HSeq ISeq
        (ISeq'''first => HSeq''first)
        (ISeq'''next => HSeq''next)
    )

    (defm HSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm HSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"NSeq"
    (defq NSeq [#_"array" a, #_"int" i, #_"seq" s] SeqForm)

    #_inherit
    (defm NSeq ASeq)

    (defn #_"NSeq" NSeq'new
        ([#_"array" a, #_"int" i] (NSeq'new a, i, nil))
        ([#_"array" a, #_"int" i, #_"seq" s]
            (new* NSeq'class (anew [a, i, s]))
        )
    )

    (defn #_"seq" NSeq'create
        ([#_"array" a] (NSeq'create a, 0, nil))
        ([#_"array" a, #_"int" i, #_"seq" s]
            (when (nil? s) => (NSeq'new a, i, s)
                (loop-when i (< i (alength a))
                    (when (nil? (aget a i)) => (NSeq'new a, i, nil)
                        (or
                            (when-some [#_"node" node (aget a (inc i))]
                                (when-some [s (INode'''nodeSeq node)]
                                    (NSeq'new a, (+ i 2), s)
                                )
                            )
                            (recur (+ i 2))
                        )
                    )
                )
            )
        )
    )

    (defn #_"seq" NSeq''seq [#_"NSeq" this]
        this
    )

    (defn #_"pair" NSeq''first [#_"NSeq" this]
        (if (some? (:s this))
            (first (:s this))
            (MapEntry'new (aget (:a this) (:i this)), (aget (:a this) (inc (:i this))))
        )
    )

    (defn #_"seq" NSeq''next [#_"NSeq" this]
        (if (some? (:s this))
            (NSeq'create (:a this), (:i this), (next (:s this)))
            (NSeq'create (:a this), (+ (:i this) 2), nil)
        )
    )

    (defm NSeq Sequential)

    (defm NSeq Seqable
        (Seqable'''seq => NSeq''seq)
    )

    (defm NSeq ISeq
        (ISeq'''first => NSeq''first)
        (ISeq'''next => NSeq''next)
    )

    (defm NSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm NSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"PersistentHashMap"
    (defn #_"int" PersistentHashMap'mask [#_"int" hash, #_"int" shift]
        (bit-and (unsigned-bit-shift-right hash shift) 0x1f)
    )

    (defn #_"int" PersistentHashMap'bitpos [#_"int" hash, #_"int" shift]
        (bit-shift-left 1 (PersistentHashMap'mask hash, shift))
    )

    (defn #_"array" PersistentHashMap'cloneAndSet
        ([#_"array" a, #_"int" i, #_"any" x]                          (-> (aclone a) (aset! i x)))
        ([#_"array" a, #_"int" i, #_"any" x, #_"int" j, #_"any" y] (-> (aclone a) (aset! i x) (aset! j y)))
    )

    (defn #_"array" PersistentHashMap'removePair [#_"array" a, #_"int" i]
        (let [#_"int" n (- (alength a) 2) #_"int" m (* 2 i)]
            (-> (anew n) (acopy! 0 a 0 m) (acopy! m a (+ m 2) (- n m)))
        )
    )
)

(about #_"ANode"
    (defq ANode [#_"thread'" edit, #_"int" n, #_"node[]" a])

    (defn #_"ANode" ANode'new [#_"thread'" edit, #_"int" n, #_"node[]" a]
        (new* ANode'class (anew [edit, n, a]))
    )

    (defn #_"ANode" ANode''ensureEditable [#_"ANode" this, #_"thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (ANode'new edit, (:n this), (aclone (:a this)))
        )
    )

    (defn #_"ANode" ANode''editAndSet [#_"ANode" this, #_"thread'" edit, #_"int" i, #_"node" node]
        (let [#_"ANode" e (ANode''ensureEditable this, edit)]
            (aset! (:a e) i node)
            e
        )
    )

    (defn #_"node" ANode''pack [#_"ANode" this, #_"thread'" edit, #_"int" idx]
        (let [#_"array" a' (anew (* 2 (dec (:n this))))
              [#_"int" bitmap #_"int" j]
                (loop-when [bitmap 0 j 1 #_"int" i 0] (< i idx) => [bitmap j]
                    (let [[bitmap j]
                            (when-some [#_"node" ai (aget (:a this) i)] => [bitmap j]
                                (aset! a' j ai)
                                [(bit-or bitmap (bit-shift-left 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )
              bitmap
                (loop-when [bitmap bitmap j j #_"int" i (inc idx)] (< i (alength (:a this))) => bitmap
                    (let [[bitmap j]
                            (when-some [#_"node" ai (aget (:a this) i)] => [bitmap j]
                                (aset! a' j ai)
                                [(bit-or bitmap (bit-shift-left 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )]
            (BNode'new edit, bitmap, a')
        )
    )

    (defn #_"node" ANode''assoc [#_"ANode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)]
            (if (some? ai)
                (let [#_"node" node (INode'''assoc ai, (+ shift 5), hash, key, val, addedLeaf)]
                    (when-not (= node ai) => this
                        (ANode'new nil, (:n this), (PersistentHashMap'cloneAndSet (:a this), i, node))
                    )
                )
                (let [#_"node" node (INode'''assoc BNode'EMPTY, (+ shift 5), hash, key, val, addedLeaf)]
                    (ANode'new nil, (inc (:n this)), (PersistentHashMap'cloneAndSet (:a this), i, node))
                )
            )
        )
    )

    (defn #_"node" ANode''dissoc [#_"ANode" this, #_"int" shift, #_"int" hash, #_"key" key]
        (let-when [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)] (some? ai) => this
            (let-when-not [#_"node" node (INode'''dissoc ai, (+ shift 5), hash, key)] (= node ai) => this
                (cond
                    (some? node)     (ANode'new nil, (:n this), (PersistentHashMap'cloneAndSet (:a this), i, node))
                    (<= (:n this) 8) (ANode''pack this, nil, i)
                    :else            (ANode'new nil, (dec (:n this)), (PersistentHashMap'cloneAndSet (:a this), i, node))
                )
            )
        )
    )

    (defn #_"IMapEntry|value" ANode''find
        ([#_"ANode" this, #_"int" shift, #_"int" hash, #_"key" key]
            (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" node (aget (:a this) i)]
                (when (some? node)
                    (INode'''find node, (+ shift 5), hash, key)
                )
            )
        )
        ([#_"ANode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" not-found]
            (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" node (aget (:a this) i)]
                (when (some? node) => not-found
                    (INode'''find node, (+ shift 5), hash, key, not-found)
                )
            )
        )
    )

    (defn #_"seq" ANode''nodeSeq [#_"ANode" this]
        (HSeq'create (:a this))
    )

    (defn #_"node" ANode''assocT [#_"ANode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)]
            (if (some? ai)
                (let [#_"node" node (INode'''assocT ai, edit, (+ shift 5), hash, key, val, addedLeaf)]
                    (when-not (= node ai) => this
                        (ANode''editAndSet this, edit, i, node)
                    )
                )
                (let [#_"node" node (INode'''assocT BNode'EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf)]
                    (-> (ANode''editAndSet this, edit, i, node) (qswap! :n inc))
                )
            )
        )
    )

    (defn #_"node" ANode''dissocT [#_"ANode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf]
        (let-when [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)] (some? ai) => this
            (let-when-not [#_"node" node (INode'''dissocT ai, edit, (+ shift 5), hash, key, removedLeaf)] (= node ai) => this
                (cond
                    (some? node)     (ANode''editAndSet this, edit, i, node)
                    (<= (:n this) 8) (ANode''pack this, edit, i)
                    :else            (-> (ANode''editAndSet this, edit, i, node) (qswap! :n dec))
                )
            )
        )
    )

    (defm ANode INode
        (INode'''assoc => ANode''assoc)
        (INode'''dissoc => ANode''dissoc)
        (INode'''find => ANode''find)
        (INode'''nodeSeq => ANode''nodeSeq)
        (INode'''assocT => ANode''assocT)
        (INode'''dissocT => ANode''dissocT)
    )
)

(about #_"BNode"
    (defq BNode [#_"thread'" edit, #_"int" bitmap, #_"array" a])

    (defn #_"BNode" BNode'new [#_"thread'" edit, #_"int" bitmap, #_"array" a]
        (new* BNode'class (anew [edit, bitmap, a]))
    )

    (def #_"BNode" BNode'EMPTY (BNode'new nil, 0, (anew 0)))

    (defn #_"int" BNode'index [#_"int" bitmap, #_"int" bit]
        (Number'bitCount (bit-and bitmap (dec bit)))
    )

    (defn #_"node" BNode'create [#_"int" shift, #_"key" key1, #_"value" val1, #_"int" hash2, #_"key" key2, #_"value" val2]
        (let [#_"int" hash1 (f'hash key1)]
            (when-not (= hash1 hash2) => (CNode'new nil, hash1, 2, (anew [ key1, val1, key2, val2 ]))
                (let [#_"boolean'" addedLeaf (atom false) #_"thread'" edit (atom nil)]
                    (-> BNode'EMPTY
                        (INode'''assocT edit, shift, hash1, key1, val1, addedLeaf)
                        (INode'''assocT edit, shift, hash2, key2, val2, addedLeaf)
                    )
                )
            )
        )
    )

    (defn #_"BNode" BNode''ensureEditable [#_"BNode" this, #_"thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (let [#_"int" b (:bitmap this) #_"int" n (Number'bitCount b) #_"int" m (inc n)]
                (BNode'new edit, b, (-> (anew (* 2 m)) (acopy! 0 (:a this) 0 (* 2 n))))
            )
        )
    )

    (defn #_"BNode" BNode''editAndSet
        ([#_"BNode" this, #_"thread'" edit, #_"int" i, #_"any" x]
            (let [#_"BNode" e (BNode''ensureEditable this, edit)]
                (aset! (:a e) i x)
                e
            )
        )
        ([#_"BNode" this, #_"thread'" edit, #_"int" i, #_"any" x, #_"int" j, #_"any" y]
            (let [#_"BNode" e (BNode''ensureEditable this, edit)]
                (aset! (:a e) i x)
                (aset! (:a e) j y)
                e
            )
        )
    )

    (defn #_"BNode" BNode''editAndRemovePair [#_"BNode" this, #_"thread'" edit, #_"int" bit, #_"int" i]
        (when-not (= (:bitmap this) bit)
            (let [
                #_"BNode" e (-> (BNode''ensureEditable this, edit) (qswap! :bitmap bit-xor bit))
                #_"array" a (:a e) #_"int" n (alength a) #_"int" m (* 2 (inc i))
            ]
                (acopy! a (* 2 i) a m (- n m))
                (aset! a (- n 2) nil)
                (aset! a (- n 1) nil)
                e
            )
        )
    )

    (defn #_"node" BNode'createT [#_"thread'" edit, #_"int" shift, #_"key" key1, #_"value" val1, #_"int" hash2, #_"key" key2, #_"value" val2]
        (let [#_"int" hash1 (f'hash key1)]
            (when-not (= hash1 hash2) => (CNode'new nil, hash1, 2, (anew [ key1, val1, key2, val2 ]))
                (let [#_"boolean'" addedLeaf (atom false)]
                    (-> BNode'EMPTY
                        (INode'''assocT edit, shift, hash1, key1, val1, addedLeaf)
                        (INode'''assocT edit, shift, hash2, key2, val2, addedLeaf)
                    )
                )
            )
        )
    )

    (defn #_"node" BNode''assoc [#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" bit (PersistentHashMap'bitpos hash, shift) #_"int" x (BNode'index (:bitmap this), bit)]
            (if-not (zero? (bit-and (:bitmap this) bit))
                (let [
                    #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
                    #_"array" a'
                        (cond
                            (nil? k)
                                (let [#_"node" node (INode'''assoc #_"node" v, (+ shift 5), hash, key, val, addedLeaf)]
                                    (when-not (= node v)
                                        (PersistentHashMap'cloneAndSet (:a this), (inc (* 2 x)), node)
                                    )
                                )
                            (= key k)
                                (when-not (= val v)
                                    (PersistentHashMap'cloneAndSet (:a this), (inc (* 2 x)), val)
                                )
                            :else
                                (let [#_"node" node (BNode'create (+ shift 5), k, v, hash, key, val) _ (reset! addedLeaf true)]
                                    (PersistentHashMap'cloneAndSet (:a this), (* 2 x), nil, (inc (* 2 x)), node)
                                )
                        )
                ]
                    (when (some? a') => this
                        (BNode'new nil, (:bitmap this), a')
                    )
                )
                (let [#_"int" n (Number'bitCount (:bitmap this))]
                    (if (<= 16 n)
                        (let [
                            #_"node[]" nodes (anew #_"node" 32) #_"int" m (PersistentHashMap'mask hash, shift)
                            _ (aset! nodes m (INode'''assoc BNode'EMPTY, (+ shift 5), hash, key, val, addedLeaf))
                            _
                                (loop-when [#_"int" j 0 #_"int" i 0] (< i 32)
                                    (when (odd? (unsigned-bit-shift-right (:bitmap this) i)) => (recur j (inc i))
                                        (let [#_"key|nil" k (aget (:a this) j) #_"value|node" v (aget (:a this) (inc j))]
                                            (if (some? k)
                                                (aset! nodes i (INode'''assoc BNode'EMPTY, (+ shift 5), (f'hash k), k, v, addedLeaf))
                                                (aset! nodes i #_"node" v)
                                            )
                                            (recur (+ j 2) (inc i))
                                        )
                                    )
                                )
                        ]
                            (ANode'new nil, (inc n), nodes)
                        )
                        (let [
                            #_"array" a' (anew (* 2 (inc n)))
                            _ (acopy! a' 0 (:a this) 0 (* 2 x))
                            _ (aset! a' (* 2 x) key)
                            _ (reset! addedLeaf true)
                            _ (aset! a' (inc (* 2 x)) val)
                            _ (acopy! a' (* 2 (inc x)) (:a this) (* 2 x) (* 2 (- n x)))
                        ]
                            (BNode'new nil, (bit-or (:bitmap this) bit), a')
                        )
                    )
                )
            )
        )
    )

    (defn #_"node" BNode''dissoc [#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key]
        (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (bit-and (:bitmap this) bit)) => this
            (let [
                #_"int" x (BNode'index (:bitmap this), bit)
                #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
            ]
                (if (some? k)
                    (when (= key k) => this
                        (BNode'new nil, (bit-xor (:bitmap this) bit), (PersistentHashMap'removePair (:a this), x))
                    )
                    (let [#_"node" node (INode'''dissoc #_"node" v, (+ shift 5), hash, key)]
                        (cond
                            (= node v)
                                this
                            (some? node)
                                (BNode'new nil, (:bitmap this), (PersistentHashMap'cloneAndSet (:a this), (inc (* 2 x)), node))
                            (= (:bitmap this) bit)
                                nil
                            :else
                                (BNode'new nil, (bit-xor (:bitmap this) bit), (PersistentHashMap'removePair (:a this), x))
                        )
                    )
                )
            )
        )
    )

    (defn #_"IMapEntry|value" BNode''find
        ([#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key]
            (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (bit-and (:bitmap this) bit))
                (let [
                    #_"int" x (BNode'index (:bitmap this), bit)
                    #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
                ]
                    (cond
                        (nil? k)  (INode'''find #_"node" v, (+ shift 5), hash, key)
                        (= key k) (MapEntry'new k, v)
                    )
                )
            )
        )
        ([#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" not-found]
            (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (bit-and (:bitmap this) bit)) => not-found
                (let [
                    #_"int" x (BNode'index (:bitmap this), bit)
                    #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
                ]
                    (cond
                        (nil? k)  (INode'''find #_"node" v, (+ shift 5), hash, key, not-found)
                        (= key k) v
                        :else     not-found
                    )
                )
            )
        )
    )

    (defn #_"seq" BNode''nodeSeq [#_"BNode" this]
        (NSeq'create (:a this))
    )

    (defn #_"node" BNode''assocT [#_"BNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" bit (PersistentHashMap'bitpos hash, shift) #_"int" x (BNode'index (:bitmap this), bit)]
            (if-not (zero? (bit-and (:bitmap this) bit))
                (let [
                    #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
                ]
                    (cond
                        (nil? k)
                            (let [#_"node" node (INode'''assocT #_"node" v, edit, (+ shift 5), hash, key, val, addedLeaf)]
                                (when-not (= node v) => this
                                    (BNode''editAndSet this, edit, (inc (* 2 x)), node)
                                )
                            )
                        (= key k)
                            (when-not (= val v) => this
                                (BNode''editAndSet this, edit, (inc (* 2 x)), val)
                            )
                        :else
                            (let [#_"node" node (BNode'createT edit, (+ shift 5), k, v, hash, key, val) _ (reset! addedLeaf true)]
                                (BNode''editAndSet this, edit, (* 2 x), nil, (inc (* 2 x)), node)
                            )
                    )
                )
                (let [#_"int" n (Number'bitCount (:bitmap this))]
                    (cond
                        (< (* n 2) (alength (:a this)))
                            (let [
                                #_"BNode" e (-> (BNode''ensureEditable this, edit) (qswap! :bitmap bit-or bit)) _ (reset! addedLeaf true)
                                _ (acopy! (:a e) (* 2 (inc x)) (:a e) (* 2 x) (* 2 (- n x)))
                                _ (aset! (:a e) (* 2 x) key)
                                _ (aset! (:a e) (inc (* 2 x)) val)
                            ]
                                e
                            )
                        (<= 16 n)
                            (let [
                                #_"node[]" nodes (anew #_"node" 32) #_"int" m (PersistentHashMap'mask hash, shift)
                                _ (aset! nodes m (INode'''assocT BNode'EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf))
                                _
                                    (loop-when [#_"int" j 0 #_"int" i 0] (< i 32)
                                        (when (odd? (unsigned-bit-shift-right (:bitmap this) i)) => (recur j (inc i))
                                            (let [#_"key|nil" k (aget (:a this) j) #_"value|node" v (aget (:a this) (inc j))]
                                                (if (some? k)
                                                    (aset! nodes i (INode'''assocT BNode'EMPTY, edit, (+ shift 5), (f'hash k), k, v, addedLeaf))
                                                    (aset! nodes i #_"node" v)
                                                )
                                                (recur (+ j 2) (inc i))
                                            )
                                        )
                                    )
                            ]
                                (ANode'new edit, (inc n), nodes)
                            )
                        :else
                            (let [
                                #_"array" a' (anew (* 2 (+ n 4)))
                                _ (acopy! a' 0 (:a this) 0 (* 2 x))
                                _ (aset! a' (* 2 x) key)
                                _ (reset! addedLeaf true)
                                _ (aset! a' (inc (* 2 x)) val)
                                _ (acopy! a' (* 2 (inc x)) (:a this) (* 2 x) (* 2 (- n x)))
                            ]
                                (-> (BNode''ensureEditable this, edit)
                                    (qset! :a a')
                                    (qswap! :bitmap bit-or bit)
                                )
                            )
                    )
                )
            )
        )
    )

    (defn #_"node" BNode''dissocT [#_"BNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf]
        (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (bit-and (:bitmap this) bit)) => this
            (let [
                #_"int" x (BNode'index (:bitmap this), bit)
                #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
            ]
                (if (some? k)
                    (when (= key k) => this
                        (reset! removedLeaf true)
                        (BNode''editAndRemovePair this, edit, bit, x)
                    )
                    (let [#_"node" node (INode'''dissocT #_"node" v, edit, (+ shift 5), hash, key, removedLeaf)]
                        (cond
                            (= node v)
                                this
                            (some? node)
                                (BNode''editAndSet this, edit, (inc (* 2 x)), node)
                            (= (:bitmap this) bit)
                                nil
                            :else
                                (BNode''editAndRemovePair this, edit, bit, x)
                        )
                    )
                )
            )
        )
    )

    (defm BNode INode
        (INode'''assoc => BNode''assoc)
        (INode'''dissoc => BNode''dissoc)
        (INode'''find => BNode''find)
        (INode'''nodeSeq => BNode''nodeSeq)
        (INode'''assocT => BNode''assocT)
        (INode'''dissocT => BNode''dissocT)
    )
)

(about #_"CNode"
    (defq CNode [#_"thread'" edit, #_"int" hash, #_"int" n, #_"array" a])

    (defn #_"CNode" CNode'new [#_"thread'" edit, #_"int" hash, #_"int" n, #_"array" a]
        (new* CNode'class (anew [edit, hash, n, a]))
    )

    (defn #_"int" CNode''findIndex [#_"CNode" this, #_"key" key]
        (let [#_"array" a (:a this) #_"int" m (* 2 (:n this))]
            (loop-when [#_"int" i 0] (< i m) => -1
                (if (= (aget a i) key) i (recur (+ i 2)))
            )
        )
    )

    (defn #_"CNode" CNode''ensureEditable
        ([#_"CNode" this, #_"thread'" edit]
            (when-not (identical? (:edit this) edit) => this
                (let [
                    #_"int" n (:n this) #_"int" m (inc n)
                    #_"array" a' (-> (anew (* 2 m)) (acopy! 0 (:a this) 0 (* 2 n)))
                ]
                    (CNode'new edit, (:hash this), n, a')
                )
            )
        )
        ([#_"CNode" this, #_"thread'" edit, #_"int" n, #_"array" a]
            (when-not (identical? (:edit this) edit) => (qset! this :a a, :n n)
                (CNode'new edit, (:hash this), n, a)
            )
        )
    )

    (defn #_"CNode" CNode''editAndSet
        ([#_"CNode" this, #_"thread'" edit, #_"int" i, #_"any" x]
            (let [#_"CNode" e (CNode''ensureEditable this, edit)]
                (aset! (:a e) i x)
                e
            )
        )
        ([#_"CNode" this, #_"thread'" edit, #_"int" i, #_"any" x, #_"int" j, #_"any" y]
            (let [#_"CNode" e (CNode''ensureEditable this, edit)]
                (aset! (:a e) i x)
                (aset! (:a e) j y)
                e
            )
        )
    )

    (defn #_"node" CNode''assoc [#_"CNode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (if (= (:hash this) hash)
            (let [#_"array" a (:a this) #_"int" i (CNode''findIndex this, key) #_"int" n (:n this)]
                (if (< -1 i)
                    (when-not (= (aget a (inc i)) val) => this
                        (CNode'new nil, hash, n, (PersistentHashMap'cloneAndSet a, (inc i), val))
                    )
                    (let [
                        #_"array" a' (-> (anew (* 2 (inc n))) (acopy! 0 a 0 (* 2 n)) (aset! (* 2 n) key) (aset! (inc (* 2 n)) val))
                        _ (reset! addedLeaf true)
                    ]
                        (CNode'new (:edit this), hash, (inc n), a')
                    )
                )
            )
            (let [#_"BNode" node (BNode'new nil, (PersistentHashMap'bitpos (:hash this), shift), (anew [ nil, this ]))]
                (INode'''assoc node, shift, hash, key, val, addedLeaf)
            )
        )
    )

    (defn #_"node" CNode''dissoc [#_"CNode" this, #_"int" shift, #_"int" hash, #_"key" key]
        (let-when [#_"int" i (CNode''findIndex this, key)] (< -1 i) => this
            (let-when [#_"int" n (:n this)] (< 1 n)
                (CNode'new nil, hash, (dec n), (PersistentHashMap'removePair (:a this), (quot i 2)))
            )
        )
    )

    (defn #_"IMapEntry|value" CNode''find
        ([#_"CNode" this, #_"int" shift, #_"int" hash, #_"key" key]
            (let-when [#_"int" i (CNode''findIndex this, key)] (< -1 i)
                (let-when [#_"key" ai (aget (:a this) i)] (= ai key)
                    (MapEntry'new ai, (aget (:a this) (inc i)))
                )
            )
        )
        ([#_"CNode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" not-found]
            (let-when [#_"int" i (CNode''findIndex this, key)] (< -1 i) => not-found
                (when (= (aget (:a this) i) key) => not-found
                    (aget (:a this) (inc i))
                )
            )
        )
    )

    (defn #_"seq" CNode''nodeSeq [#_"CNode" this]
        (NSeq'create (:a this))
    )

    (defn #_"node" CNode''assocT [#_"CNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (if (= (:hash this) hash)
            (let [#_"array" a (:a this) #_"int" i (CNode''findIndex this, key)]
                (if (< -1 i)
                    (when-not (= (aget a (inc i)) val) => this
                        (CNode''editAndSet this, edit, (inc i), val)
                    )
                    (let [#_"int" n (:n this) #_"int" m (alength a)]
                        (if (< (* 2 n) m)
                            (let [_ (reset! addedLeaf true)]
                                (-> (CNode''editAndSet this, edit, (* 2 n), key, (inc (* 2 n)), val)
                                    (qswap! :n inc)
                                )
                            )
                            (let [
                                #_"array" a' (-> (anew (+ m 2)) (acopy! 0 a 0 m) (aset! m key) (aset! (inc m) val))
                                _ (reset! addedLeaf true)
                            ]
                                (CNode''ensureEditable this, edit, (inc n), a')
                            )
                        )
                    )
                )
            )
            (let [#_"BNode" node (BNode'new edit, (PersistentHashMap'bitpos (:hash this), shift), (anew [ nil, this, nil, nil ]))]
                (INode'''assocT node, edit, shift, hash, key, val, addedLeaf)
            )
        )
    )

    (defn #_"node" CNode''dissocT [#_"CNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf]
        (let-when [#_"int" i (CNode''findIndex this, key)] (< -1 i) => this
            (reset! removedLeaf true)
            (let-when [#_"int" n (:n this)] (< 1 n)
                (let [
                    #_"CNode" e (-> (CNode''ensureEditable this, edit) (qswap! :n dec))
                    #_"int" m (* 2 n)
                    _ (aset! (:a e) i (aget (:a e) (- m 2)))
                    _ (aset! (:a e) (inc i) (aget (:a e) (- m 1)))
                    _ (aset! (:a e) (- m 2) nil)
                    _ (aset! (:a e) (- m 1) nil)
                ]
                    e
                )
            )
        )
    )

    (defm CNode INode
        (INode'''assoc => CNode''assoc)
        (INode'''dissoc => CNode''dissoc)
        (INode'''find => CNode''find)
        (INode'''nodeSeq => CNode''nodeSeq)
        (INode'''assocT => CNode''assocT)
        (INode'''dissocT => CNode''dissocT)
    )
)

(about #_"PersistentHashMap"
    (defq PersistentHashMap [#_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value] MapForm)

    #_inherit
    (defm PersistentHashMap APersistentMap AFn)

    (defn #_"PersistentHashMap" PersistentHashMap'new [#_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value]
        (new* PersistentHashMap'class (anew [cnt, root, has-nil?, nil-value]))
    )

    (def #_"PersistentHashMap" PersistentHashMap'EMPTY (PersistentHashMap'new 0, nil, false, nil))

    (defn #_"PersistentHashMap" PersistentHashMap'create-1a [#_"array" init]
        (loop-when-recur [#_"IPersistentMap" m PersistentHashMap'EMPTY #_"int" i 0]
                         (< i (alength init))
                         [(assoc m (aget init i) (aget init (inc i))) (+ i 2)]
                      => m
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'create-1s [#_"Seqable" init]
        (loop-when [#_"IPersistentMap" m PersistentHashMap'EMPTY #_"seq" s (seq init)] (some? s) => m
            (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                (recur (assoc m (first s) (second s)) (next (next s)))
            )
        )
    )

    (defn #_"value" PersistentHashMap''valAt
        ([#_"PersistentHashMap" this, #_"key" key] (PersistentHashMap''valAt this, key, nil))
        ([#_"PersistentHashMap" this, #_"key" key, #_"value" not-found]
            (if (nil? key)
                (when (:has-nil? this) => not-found
                    (:nil-value this)
                )
                (when (some? (:root this)) => not-found
                    (INode'''find (:root this), 0, (f'hash key), key, not-found)
                )
            )
        )
    )

    (def #_"value" PersistentHashMap'NOT_FOUND (anew 0))

    (defn #_"IPersistentMap" PersistentHashMap''assoc [#_"PersistentHashMap" this, #_"key" key, #_"value" val]
        (if (nil? key)
            (when-not (and (:has-nil? this) (= (:nil-value this) val)) => this
                (PersistentHashMap'new (+ (:cnt this) (if (:has-nil? this) 0 1)), (:root this), true, val)
            )
            (let [
                #_"boolean'" addedLeaf (atom false)
                #_"node" root (INode'''assoc (or (:root this) BNode'EMPTY), 0, (f'hash key), key, val, addedLeaf)
            ]
                (when-not (= root (:root this)) => this
                    (PersistentHashMap'new (+ (:cnt this) (if (deref addedLeaf) 1 0)), root, (:has-nil? this), (:nil-value this))
                )
            )
        )
    )

    (defn #_"boolean" PersistentHashMap''containsKey [#_"PersistentHashMap" this, #_"key" key]
        (if (nil? key)
            (:has-nil? this)
            (and (some? (:root this))
                (not (identical? (INode'''find (:root this), 0, (f'hash key), key, PersistentHashMap'NOT_FOUND) PersistentHashMap'NOT_FOUND))
            )
        )
    )

    (defn #_"pair" PersistentHashMap''entryAt [#_"PersistentHashMap" this, #_"key" key]
        (if (nil? key)
            (when (:has-nil? this)
                (MapEntry'new nil, (:nil-value this))
            )
            (when (some? (:root this))
                (INode'''find (:root this), 0, (f'hash key), key)
            )
        )
    )

    (defn #_"IPersistentMap" PersistentHashMap''dissoc [#_"PersistentHashMap" this, #_"key" key]
        (cond
            (nil? key)
                (when (:has-nil? this) => this
                    (PersistentHashMap'new (dec (:cnt this)), (:root this), false, nil)
                )
            (nil? (:root this))
                this
            :else
                (let [#_"node" root (INode'''dissoc (:root this), 0, (f'hash key), key)]
                    (when-not (= root (:root this)) => this
                        (PersistentHashMap'new (dec (:cnt this)), root, (:has-nil? this), (:nil-value this))
                    )
                )
        )
    )

    (defn #_"IPersistentCollection" PersistentHashMap''empty [#_"PersistentHashMap" this]
        PersistentHashMap'EMPTY
    )

    (defn #_"seq" PersistentHashMap''seq [#_"PersistentHashMap" this]
        (let [#_"seq" s (when (some? (:root this)) (INode'''nodeSeq (:root this)))]
            (when (:has-nil? this) => s
                (Cons'new (MapEntry'new nil, (:nil-value this)), s)
            )
        )
    )

    (defm PersistentHashMap Counted
        (Counted'''count => :cnt)
    )

    (defm PersistentHashMap ILookup
        (ILookup'''valAt => PersistentHashMap''valAt)
    )

    (defm PersistentHashMap IFn
        (IFn'''invoke => APersistentMap''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm PersistentHashMap Associative
        (Associative'''assoc => PersistentHashMap''assoc)
        (Associative'''containsKey => PersistentHashMap''containsKey)
        (Associative'''entryAt => PersistentHashMap''entryAt)
    )

    (defm PersistentHashMap IPersistentMap
        (IPersistentMap'''dissoc => PersistentHashMap''dissoc)
    )

    (defm PersistentHashMap IPersistentCollection
        (IPersistentCollection'''conj => APersistentMap''conj)
        (IPersistentCollection'''empty => PersistentHashMap''empty)
    )

    (defm PersistentHashMap Seqable
        (Seqable'''seq => PersistentHashMap''seq)
    )

    (defm PersistentHashMap IObject
        (IObject'''equals => APersistentMap''equals)
    )

    (defm PersistentHashMap Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

(defn hash-map
    ([] PersistentHashMap'EMPTY)
    ([& keyvals] (PersistentHashMap'create-1s keyvals))
)

(defn merge [& maps]
    (when (some identity maps)
        (reduce (fn [a1 a2] (conj (or a1 (hash-map)) a2)) maps)
    )
)
)

(about #_"arbace.arm.PersistentHashSet"

(about #_"PersistentHashSet"
    (defq PersistentHashSet [#_"map" impl] SetForm)

    #_inherit
    (defm PersistentHashSet APersistentSet AFn)

    (defn #_"PersistentHashSet" PersistentHashSet'new [#_"map" impl]
        (new* PersistentHashSet'class (anew [impl]))
    )

    (def #_"PersistentHashSet" PersistentHashSet'EMPTY (PersistentHashSet'new PersistentHashMap'EMPTY))

    (defn #_"PersistentHashSet" PersistentHashSet'create [#_"Seqable" init]
        (into PersistentHashSet'EMPTY init)
    )

    (defn #_"PersistentHashSet" PersistentHashSet'createWithCheck [#_"Seqable" init]
        (loop-when [#_"IPersistentSet" s PersistentHashSet'EMPTY #_"seq" q (seq init) #_"int" n 0] (some? q) => s
            (let [s (conj s (first q))]
                (when (= (count s) (inc n)) => (throw! (str "duplicate key: " (first q)))
                    (recur s (next q) (inc n))
                )
            )
        )
    )

    (defn #_"int" PersistentHashSet''count [#_"PersistentHashSet" this]
        (count (:impl this))
    )

    (defn #_"PersistentHashSet" PersistentHashSet''conj [#_"PersistentHashSet" this, #_"value" val]
        (if (contains? (:impl this) val)
            this
            (PersistentHashSet'new (assoc (:impl this) val val))
        )
    )

    (defn #_"PersistentHashSet" PersistentHashSet''empty [#_"PersistentHashSet" this]
        PersistentHashSet'EMPTY
    )

    (defn #_"IPersistentSet" PersistentHashSet''disj [#_"PersistentHashSet" this, #_"key" key]
        (if (contains? (:impl this) key)
            (PersistentHashSet'new (dissoc (:impl this) key))
            this
        )
    )

    (defn #_"boolean" PersistentHashSet''contains? [#_"PersistentHashSet" this, #_"key" key]
        (contains? (:impl this) key)
    )

    (defn #_"value" PersistentHashSet''get [#_"PersistentHashSet" this, #_"key" key]
        (get (:impl this) key)
    )

    (defn #_"seq" PersistentHashSet''seq [#_"PersistentHashSet" this]
        (keys (:impl this))
    )

    (defm PersistentHashSet Counted
        (Counted'''count => PersistentHashSet''count)
    )

    (defm PersistentHashSet IPersistentCollection
        (IPersistentCollection'''conj => PersistentHashSet''conj)
        (IPersistentCollection'''empty => PersistentHashSet''empty)
    )

    (defm PersistentHashSet IPersistentSet
        (IPersistentSet'''disj => PersistentHashSet''disj)
        (IPersistentSet'''contains? => PersistentHashSet''contains?)
        (IPersistentSet'''get => PersistentHashSet''get)
    )

    (defm PersistentHashSet Seqable
        (Seqable'''seq => PersistentHashSet''seq)
    )

    (defm PersistentHashSet IFn
        (IFn'''invoke => APersistentSet''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm PersistentHashSet IObject
        (IObject'''equals => APersistentSet''equals)
    )

    (defm PersistentHashSet Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

(defn hash-set
    ([] PersistentHashSet'EMPTY)
    ([& keys] (PersistentHashSet'create keys))
)

(defn set [s] (if (set? s) s (into (hash-set) s)))
)

(about #_"arbace.arm.PersistentVector"

(about #_"VNode"
    (defq VNode [#_"array" array, #_"index" index])

    (defn #_"node" VNode'new [#_"array" array, #_"index" index]
        (new* VNode'class (anew [(or array (anew 32)), index]))
    )

    (def #_"node" VNode'EMPTY (VNode'new nil, nil))

    (defn #_"values" VNode''array-for
        ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt] (VNode''array-for this, i, shift, cnt, cnt, nil))
        ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
            (when (< -1 i cnt) => (throw! "index is out of bounds")
                (when (< i tail-off) => tail
                    (loop-when [i i #_"node" node this shift shift] (pos? shift) => (:array node)
                        (let [
                            #_"index" x (:index node)
                            #_"int" m (bit-and (unsigned-bit-shift-right i shift) 0x1f)
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

    (defn #_"value" VNode''value-for [#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
        (when (< -1 i cnt) => (throw! "index is out of bounds")
            (when (< i tail-off) => (aget tail (- i tail-off))
                (loop-when [i i #_"node" node this shift shift] (pos? shift) => (aget (:array node) (bit-and (unsigned-bit-shift-right i shift) 0x1f))
                    (let [
                        #_"index" x (:index node)
                        #_"int" m (bit-and (unsigned-bit-shift-right i shift) 0x1f)
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

    (defn #_"node" VNode''new-path [#_"node" this, #_"int" shift]
        (when (pos? shift) => this
            (VNode'new (-> (anew 32) (aset! 0 (VNode''new-path this, (- shift 5)))), nil)
        )
    )

    (defn #_"int" VNode'last-range [#_"index" x]
        (aget x (dec (aget x 32)))
    )

    (defn #_"boolean" VNode''overflow? [#_"node" this, #_"int" shift, #_"int" cnt]
        (let [
            #_"index" x (:index this)
        ]
            (when (some? x) => (< (bit-shift-left 1 shift) (unsigned-bit-shift-right (inc cnt) 5))
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

    (defn #_"node" VNode''push-tail [#_"node" this, #_"int" shift, #_"int" cnt, #_"node" tail-node]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (if (some? x)
                (let [
                    #_"int" e (dec (aget x 32))
                    #_"node" child
                        (when (< 5 shift)
                            (let [
                                #_"int" n (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0))
                            ]
                                (when (< n (bit-shift-left 1 shift))
                                    (VNode''push-tail (aget a e), (- shift 5), (inc n), tail-node)
                                )
                            )
                        )
                    a (aclone a) x (aclone x)
                    [a x]
                        (if (some? child)
                            [(aset! a e child) (aswap! x e + 32)]
                            (let [
                                a (aset! a (inc e) (VNode''new-path tail-node, (- shift 5)))
                                x (aset! x (inc e) (+ (aget x e) 32))
                            ]
                                [a (aswap! x 32 inc)]
                            )
                        )
                ]
                    (VNode'new a, x)
                )
                (let [
                    #_"int" e (bit-and (unsigned-bit-shift-right (dec cnt) shift) 0x1f)
                    #_"node" child
                        (when (< 5 shift) => tail-node
                            (if-some [child (aget a e)]
                                (VNode''push-tail child, (- shift 5), cnt, tail-node)
                                (VNode''new-path tail-node, (- shift 5))
                            )
                        )
                    a (aclone a)
                    a (aset! a e child)
                ]
                    (VNode'new a, nil)
                )
            )
        )
    )

    (defn #_"node" VNode''pop-tail [#_"node" this, #_"int" shift, #_"int" tail-off]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
            #_"int" e (bit-and (unsigned-bit-shift-right (dec tail-off) shift) 0x1f)
        ]
            (if (some? x)
                (let [
                    e (loop-when-recur e (and (< e 31) (some? (aget x (inc e)))) (inc e) => e)
                ]
                    (cond
                        (< 5 shift)
                            (let [
                                #_"node" child (aget a e)
                                #_"node" child' (VNode''pop-tail child, (- shift 5), (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0)))
                            ]
                                (when (or (some? child') (pos? e))
                                    (let [
                                        a (aclone a)
                                        a (-> a (aset! e child'))
                                        x (aclone x)
                                        x
                                            (if (some? child')
                                                (let [
                                                    #_"int" delta
                                                        (when (some? (:index child)) => 32
                                                            (- (VNode'last-range (:index child)) (VNode'last-range (:index child')))
                                                        )
                                                ]
                                                    (-> x (aswap! e - delta))
                                                )
                                                (-> x (aset! e nil) (aswap! 32 dec))
                                            )
                                    ]
                                        (VNode'new a, x)
                                    )
                                )
                            )
                        (pos? e)
                            (let [
                                a (-> (aclone a) (aset! e nil))
                                x (-> (aclone x) (aset! e nil) (aswap! 32 dec))
                            ]
                                (VNode'new a, x)
                            )
                    )
                )
                (cond
                    (< 5 shift)
                        (let [
                            #_"node" child (VNode''pop-tail (aget a e), (- shift 5), tail-off)
                        ]
                            (when (or (some? child) (pos? e))
                                (let [
                                    a (aclone a)
                                    a (aset! a e child)
                                ]
                                    (VNode'new a, nil)
                                )
                            )
                        )
                    (pos? e)
                        (let [
                            a (aclone a)
                            a (aset! a e nil)
                        ]
                            (VNode'new a, nil)
                        )
                )
            )
        )
    )

    (defn #_"node" VNode''do-assoc [#_"node" this, #_"int" shift, #_"int" i, #_"value" val]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
            a (aclone a)
            #_"int" m (bit-and (unsigned-bit-shift-right i shift) 0x1f)
            a
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
                        (aswap! a m VNode''do-assoc (- shift 5), i, val)
                    )
                )
        ]
            (VNode'new a, x)
        )
    )
)

(about #_"PersistentVector"
    (defq PersistentVector [#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] VecForm)

    #_inherit
    (defm PersistentVector APersistentVector AFn)

    (defn #_"PersistentVector" PersistentVector'new [#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
        (new* PersistentVector'class (anew [cnt, shift, root, tail]))
    )

    (def #_"PersistentVector" PersistentVector'EMPTY (PersistentVector'new 0, 5, VNode'EMPTY, (anew 0)))

    (defn #_"PersistentVector" PersistentVector'create [& values]
        (when-some [#_"seq" s (seq values)] => PersistentVector'EMPTY
            (let [
                #_"values" tail (anew (take 32 s)) #_"int" n (alength tail)
                #_"PersistentVector" w (PersistentVector'new n, 5, VNode'EMPTY, tail)
            ]
                (when-some [s (seq (drop 32 s))] => w
                    (into w s)
                )
            )
        )
    )

    (defn #_"boolean" PersistentVector''equals [#_"PersistentVector" this, #_"any" that]
        (or (identical? this that)
            (cond
                (vector? that)
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

    (defn #_"int" PersistentVector''hash [#_"PersistentVector" this]
        (loop-when [#_"int" hash 1 #_"int" i 0] (< i (:cnt this)) => (Murmur3'mixCollHash hash, i)
            (recur (+ (* 31 hash) (f'hash (Indexed'''nth this, i))) (inc i))
        )
    )

    (defn #_"int" PersistentVector''tail-off [#_"PersistentVector" this]
        (- (:cnt this) (alength (:tail this)))
    )

    (defn #_"values" PersistentVector''array-for [#_"PersistentVector" this, #_"int" i]
        (VNode''array-for (:root this), i, (:shift this), (:cnt this), (PersistentVector''tail-off this), (:tail this))
    )

    (defn #_"value" PersistentVector''value-for [#_"PersistentVector" this, #_"int" i]
        (VNode''value-for (:root this), i, (:shift this), (:cnt this), (PersistentVector''tail-off this), (:tail this))
    )

    (defn #_"value" PersistentVector''nth
        ([#_"PersistentVector" this, #_"int" i]
            (PersistentVector''value-for this, i)
        )
        ([#_"PersistentVector" this, #_"int" i, #_"value" not-found]
            (when (< -1 i (:cnt this)) => not-found
                (PersistentVector''value-for this, i)
            )
        )
    )

    (defn #_"PersistentVector" PersistentVector''conj [#_"PersistentVector" this, #_"value" val]
        (let [
            #_"int" tail-len (alength (:tail this))
        ]
            (if (< tail-len 32)
                (let [
                    #_"values" tail (-> (anew (inc tail-len)) (acopy! 0 (:tail this) 0 tail-len) (aset! tail-len val))
                ]
                    (PersistentVector'new (inc (:cnt this)), (:shift this), (:root this), tail)
                )
                (let [
                    #_"node" tail-node (VNode'new (:tail this), nil)
                    #_"int" shift (:shift this)
                    [#_"node" root shift]
                        (if (VNode''overflow? (:root this), shift, (:cnt this))
                            (let [
                                #_"array" a
                                    (-> (anew 32)
                                        (aset! 0 (:root this))
                                        (aset! 1 (VNode''new-path tail-node, shift))
                                    )
                                #_"index" x
                                    (when (some? (:index (:root this)))
                                        (let [
                                            #_"int" n (aget (:index (:root this)) 31)
                                        ]
                                            (-> (anew 33) (aset! 0 n) (aset! 1 (+ n 32)) (aset! 32 2))
                                        )
                                    )
                            ]
                                [(VNode'new a, x) (+ shift 5)]
                            )
                            [(VNode''push-tail (:root this), shift, (:cnt this), tail-node) shift]
                        )
                ]
                    (PersistentVector'new (inc (:cnt this)), shift, root, (anew [ val ]))
                )
            )
        )
    )

    (defn #_"PersistentVector" PersistentVector''empty [#_"PersistentVector" this]
        PersistentVector'EMPTY
    )

    (defn #_"PersistentVector" PersistentVector''assocN [#_"PersistentVector" this, #_"int" i, #_"value" val]
        (if (< -1 i (:cnt this))
            (let [
                #_"int" tail-off (PersistentVector''tail-off this)
            ]
                (if (<= tail-off i)
                    (let [
                        #_"int" n (alength (:tail this))
                        #_"values" tail (-> (anew n) (acopy! 0 (:tail this) 0 n) (aset! (- i tail-off) val))
                    ]
                        (PersistentVector'new (:cnt this), (:shift this), (:root this), tail)
                    )
                    (PersistentVector'new (:cnt this), (:shift this), (VNode''do-assoc (:root this), (:shift this), i, val), (:tail this))
                )
            )
            (when (= i (:cnt this)) => (throw! "index is out of bounds")
                (IPersistentCollection'''conj this, val)
            )
        )
    )

    (defn #_"value" PersistentVector''peek [#_"PersistentVector" this]
        (when (pos? (:cnt this))
            (Indexed'''nth this, (dec (:cnt this)))
        )
    )

    (defn #_"PersistentVector" PersistentVector''pop [#_"PersistentVector" this]
        (condp = (:cnt this)
            0   (throw! "can't pop the empty vector")
            1   PersistentVector'EMPTY
            (let [
                #_"int" tail-len (alength (:tail this))
            ]
                (if (< 1 tail-len)
                    (let [
                        #_"values" tail (-> (anew (dec tail-len)) (acopy! 0 (:tail this) 0 (dec tail-len)))
                    ]
                        (PersistentVector'new (dec (:cnt this)), (:shift this), (:root this), tail)
                    )
                    (let [
                        #_"values" tail (PersistentVector''array-for this, (- (:cnt this) 2))
                        #_"int" shift (:shift this)
                        #_"node" root (VNode''pop-tail (:root this), shift, (PersistentVector''tail-off this))
                        [shift root]
                            (cond
                                (nil? root)                                     [shift VNode'EMPTY]
                                (and (< 5 shift) (nil? (aget (:array root) 1))) [(- shift 5) (aget (:array root) 0)]
                                :else                                           [shift root]
                            )
                    ]
                        (PersistentVector'new (dec (:cnt this)), shift, root, tail)
                    )
                )
            )
        )
    )

    (defn #_"value" PersistentVector''invoke [#_"PersistentVector" this, #_"key" arg]
        (when (integer? arg) => (throw! "arg must be integer")
            (Indexed'''nth this, (int arg))
        )
    )

    (defn #_"value" PersistentVector''applyTo [#_"PersistentVector" this, #_"seq" args]
        (condp = (count args 1)
            1 (IFn'''invoke this, (first args))
        )
    )

    (defn #_"IPersistentVector" PersistentVector''assoc [#_"PersistentVector" this, #_"key" key, #_"value" val]
        (when (integer? key) => (throw! "key must be integer")
            (IPersistentVector'''assocN this, (int key), val)
        )
    )

    (defn #_"boolean" PersistentVector''containsKey [#_"PersistentVector" this, #_"key" key]
        (and (integer? key) (< -1 (int key) (:cnt this)))
    )

    (defn #_"pair" PersistentVector''entryAt [#_"PersistentVector" this, #_"key" key]
        (when (integer? key)
            (let-when [#_"int" i (int key)] (< -1 i (:cnt this))
                (MapEntry'new key, (Indexed'''nth this, i))
            )
        )
    )

    (defn #_"value" PersistentVector''valAt
        ([#_"PersistentVector" this, #_"key" key] (PersistentVector''valAt this, key, nil))
        ([#_"PersistentVector" this, #_"key" key, #_"value" not-found]
            (when (integer? key) => not-found
                (let-when [#_"int" i (int key)] (< -1 i (:cnt this)) => not-found
                    (PersistentVector''value-for this, i)
                )
            )
        )
    )

    (defn #_"seq" PersistentVector''seq [#_"PersistentVector" this]
        (when (pos? (:cnt this))
            (VSeq'new this, 0)
        )
    )

    (defn #_"seq" PersistentVector''rseq [#_"PersistentVector" this]
        (when (pos? (:cnt this))
            (RSeq'new this, (dec (:cnt this)))
        )
    )

    (defn #_"int" PersistentVector''compareTo [#_"PersistentVector" this, #_"IPersistentVector" that]
        (when-not (identical? this that) => 0
            (let [#_"int" n (:cnt this) #_"int" m (count that)]
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

    (defm PersistentVector IObject
        (IObject'''equals => PersistentVector''equals)
    )

    (defm PersistentVector Hashed
        (Hashed'''hash => PersistentVector''hash)
    )

    (defm PersistentVector Counted
        (Counted'''count => :cnt)
    )

    (defm PersistentVector Indexed
        (Indexed'''nth => PersistentVector''nth)
    )

    (defm PersistentVector IPersistentCollection
        (IPersistentCollection'''conj => PersistentVector''conj)
        (IPersistentCollection'''empty => PersistentVector''empty)
    )

    (defm PersistentVector IPersistentVector
        (IPersistentVector'''assocN => PersistentVector''assocN)
    )

    (defm PersistentVector IPersistentStack
        (IPersistentStack'''peek => PersistentVector''peek)
        (IPersistentStack'''pop => PersistentVector''pop)
    )

    (defm PersistentVector IFn
        (IFn'''invoke => PersistentVector''invoke)
        (IFn'''applyTo => PersistentVector''applyTo)
    )

    (defm PersistentVector Associative
        (Associative'''assoc => PersistentVector''assoc)
        (Associative'''containsKey => PersistentVector''containsKey)
        (Associative'''entryAt => PersistentVector''entryAt)
    )

    (defm PersistentVector ILookup
        (ILookup'''valAt => PersistentVector''valAt)
    )

    (defm PersistentVector Sequential)

    (defm PersistentVector Seqable
        (Seqable'''seq => PersistentVector''seq)
    )

    (defm PersistentVector Reversible
        (Reversible'''rseq => PersistentVector''rseq)
    )

    (defm PersistentVector Comparable
        (Comparable'''compareTo => PersistentVector''compareTo)
    )
)

(defn vector
    ([]                   PersistentVector'EMPTY)
    ([a]                 (PersistentVector'create a))
    ([a b]               (PersistentVector'create a b))
    ([a b c]             (PersistentVector'create a b c))
    ([a b c d]           (PersistentVector'create a b c d))
    ([a b c d & s] (apply PersistentVector'create a b c d s))
)

(defn vec [s]
    (if (vector? s) s (apply vector s))
)
)

(about #_"arbace.arm.RT"

(about #_"RT"
    (defn #_"any" RT'get
        ([#_"any" coll, #_"key" key]
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key)
                (nil? coll)
                    nil
                (set? coll)
                    (IPersistentSet'''get coll, key)
                (and (number? key) (or (string? coll) (array? coll)))
                    (let-when [#_"int" n (int key)] (< -1 n (count coll))
                        (nth coll n)
                    )
            )
        )
        ([#_"any" coll, #_"key" key, #_"value" not-found]
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key, not-found)
                (nil? coll)
                    not-found
                (set? coll)
                    (if (contains? coll key) (IPersistentSet'''get coll, key) not-found)
                (and (number? key) (or (string? coll) (array? coll)))
                    (let [#_"int" n (int key)]
                        (if (< -1 n (count coll)) (nth coll n) not-found)
                    )
                :else
                    not-found
            )
        )
    )

(defn get
    ([coll key          ] (RT'get coll key          ))
    ([coll key not-found] (RT'get coll key not-found))
)

(defn get-in
    ([m ks] (reduce get m ks))
    ([m ks not-found]
        (loop-when [m m o (anew 0) ks (seq ks)] ks => m
            (let-when [m (get m (first ks) o)] (identical? m o) => (recur m o (next ks))
                not-found
            )
        )
    )
)

    (defn #_"any" RT'contains [#_"any" coll, #_"key" key]
        (cond
            (nil? coll)
                false
            (associative? coll)
                (if (Associative'''containsKey coll, key) true false)
            (set? coll)
                (if (IPersistentSet'''contains? coll, key) true false)
            (and (number? key) (or (string? coll) (array? coll)))
                (let [#_"int" n (int key)]
                    (if (< -1 n (count coll)) true false)
                )
            :else
                (throw! (str "contains? not supported on " coll))
        )
    )

(defn contains? [coll key] (RT'contains coll key))

    (defn #_"any" RT'find [#_"any" coll, #_"key" key]
        (cond
            (nil? coll)
                nil
            (associative? coll)
                (Associative'''entryAt coll, key)
            :else
                (throw! (str "find not supported on " coll))
        )
    )

(defn find [m k] (RT'find m k))

    (defn #_"any" RT'nth
        ([#_"any" coll, #_"int" n]
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n)
                (nil? coll)
                    nil
                (string? coll)
                    (Character'valueOf (String''charAt coll, n))
                (array? coll)
                    (Array'get coll, n)
                (map-entry? coll)
                    (let [#_"pair" e coll]
                        (condp = n 0 (key e) 1 (val e) (throw! "index is out of bounds"))
                    )
                (sequential? coll)
                    (loop-when [#_"int" i 0 #_"seq" s (seq coll)] (and (<= i n) (some? s)) => (throw! "index is out of bounds")
                        (recur-when (< i n) [(inc i) (next s)] => (first s))
                    )
                :else
                    (throw! (str "nth not supported on " coll))
            )
        )
        ([#_"any" coll, #_"int" n, #_"value" not-found]
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n, not-found)
                (nil? coll)
                    not-found
                (neg? n)
                    not-found
                (string? coll)
                    (let-when [#_"String" s coll] (< n (String''length s)) => not-found
                        (Character'valueOf (String''charAt s, n))
                    )
                (array? coll)
                    (when (< n (Array'getLength coll)) => not-found
                        (Array'get coll, n)
                    )
                (map-entry? coll)
                    (let [#_"pair" e coll]
                        (condp = n 0 (key e) 1 (val e) not-found)
                    )
                (sequential? coll)
                    (loop-when [#_"int" i 0 #_"seq" s (seq coll)] (and (<= i n) (some? s)) => not-found
                        (recur-when (< i n) [(inc i) (next s)] => (first s))
                    )
                :else
                    (throw! (str "nth not supported on " coll))
            )
        )
    )

(defn nth
    ([s i]           (RT'nth s i          ))
    ([s i not-found] (RT'nth s i not-found))
)

    (defn #_"IPersistentMap" RT'mapUniqueKeys [#_"Seqable" init]
        (cond
            (empty? init)
                PersistentArrayMap'EMPTY
            (<= (count init) PersistentArrayMap'HASHTABLE_THRESHOLD)
                (PersistentArrayMap'new (anew init))
            :else
                (PersistentHashMap'create-1s init)
        )
    )
)
)

(about #_"arbace.arm.Var"

(about #_"Var"
    (defn #_"Appendable" Var'append [#_"Appendable" a, #_"Namespace" ns, #_"Symbol" sym]
        (if (some? ns)
            (-> a (Appendable'''append "#'") (append (:name ns)) (Appendable'''append "/") (append sym))
            (-> a (Appendable'''append "#_var nil #_\"") (append sym) (Appendable'''append "\""))
        )
    )
)

(about #_"Unbound"
    (defq Unbound [#_"Namespace" ns, #_"Symbol" sym])

    #_inherit
    (defm Unbound AFn)

    (defn #_"Unbound" Unbound'new [#_"Namespace" ns, #_"Symbol" sym]
        (new* Unbound'class (anew [ns, sym]))
    )

    (defn #_"Appendable" Unbound''append [#_"Unbound" this, #_"Appendable" a]
        (-> a (Appendable'''append "#_unbound ") (Var'append (:ns this), (:sym this)))
    )

    (defm Unbound IObject
        (IObject'''equals => identical?)
    )

    (defm Unbound IAppend
        (IAppend'''append => Unbound''append)
    )
)

(about #_"Var"
    (defq Var [#_"Namespace" ns, #_"Symbol" sym, #_"any'" root])

    (defn #_"Var" Var'new
        ([#_"Namespace" ns, #_"Symbol" sym] (Var'new ns, sym, (Unbound'new ns, sym)))
        ([#_"Namespace" ns, #_"Symbol" sym, #_"any" root]
            (new* Var'class (anew [ns, sym, (atom root)]))
        )
    )

    (defn #_"Appendable" Var''append [#_"Var" this, #_"Appendable" a]
        (Var'append a, (:ns this), (:sym this))
    )

    (defn #_"boolean" Var''hasRoot [#_"Var" this]
        (not (satisfies? Unbound (deref (:root this))))
    )

    (defn #_"any" Var''get [#_"Var" this]
        (deref (:root this))
    )

(defn var-get [#_"var" x] (Var''get x))

    (defn #_"void" Var''bindRoot [#_"Var" this, #_"any" root]
        (reset! (:root this) root)
        nil
    )

    (defn #_"Var" Var'intern
        ([#_"Namespace" ns, #_"Symbol" sym]
            (Namespace''intern ns, sym)
        )
        ([#_"Namespace" ns, #_"Symbol" sym, #_"any" root]
            (let [#_"Var" v (Namespace''intern ns, sym)]
                (Var''bindRoot v, root)
                v
            )
        )
    )

(defn intern
    ([ns name]      (Var'intern (the-ns ns), name))
    ([ns name root] (Var'intern (the-ns ns), name, root))
)

    (defn #_"any" Var''invoke
        ([#_"Var" this]                                                   (IFn'''invoke (deref this)))
        ([#_"Var" this, a1]                                               (IFn'''invoke (deref this), a1))
        ([#_"Var" this, a1, a2]                                           (IFn'''invoke (deref this), a1, a2))
        ([#_"Var" this, a1, a2, a3]                                       (IFn'''invoke (deref this), a1, a2, a3))
        ([#_"Var" this, a1, a2, a3, a4]                                   (IFn'''invoke (deref this), a1, a2, a3, a4))
        ([#_"Var" this, a1, a2, a3, a4, a5]                               (IFn'''invoke (deref this), a1, a2, a3, a4, a5))
        ([#_"Var" this, a1, a2, a3, a4, a5, a6]                           (IFn'''invoke (deref this), a1, a2, a3, a4, a5, a6))
        ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7]                       (IFn'''invoke (deref this), a1, a2, a3, a4, a5, a6, a7))
        ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8]                   (IFn'''invoke (deref this), a1, a2, a3, a4, a5, a6, a7, a8))
        ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]               (IFn'''invoke (deref this), a1, a2, a3, a4, a5, a6, a7, a8, a9))
        ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args] (IFn'''invoke (deref this), a1, a2, a3, a4, a5, a6, a7, a8, a9, args))
    )

    (defn #_"any" Var''applyTo [#_"Var" this, #_"seq" args]
        (IFn'''applyTo (deref this), args)
    )

    (defm Var IObject
        (IObject'''equals => identical?)
    )

    (defm Var IAppend
        (IAppend'''append => Var''append)
    )

    (defm Var IDeref
        (IDeref'''deref => Var''get)
    )

    (defm Var IFn
        (IFn'''invoke => Var''invoke)
        (IFn'''applyTo => Var''applyTo)
    )
)

(defmacro defonce [name expr]
    `(let-when [v# (def ~name)] (not (Var''hasRoot v#))
        (def ~name ~expr)
    )
)
)

(about #_"arbace.arm.Namespace"

(about #_"Namespace"
    (defq Namespace [#_"Symbol" name, #_"{Symbol Var}'" mappings])

    (def #_"{Symbol Namespace}'" Namespace'namespaces (atom (hash-map)))

    (defn #_"Namespace" Namespace'find [#_"Symbol" name]
        (get (deref Namespace'namespaces) name)
    )

(defn find-ns [sym] (Namespace'find sym))

(defn #_"Namespace" the-ns [x]
    (if (satisfies? Namespace x)
        x
        (or (find-ns x) (throw! (str "no namespace: " x " found")))
    )
)

    (defn #_"Namespace" Namespace'new [#_"Symbol" name]
        (new* Namespace'class (anew [name, (atom (hash-map))]))
    )

    (defn #_"Namespace" Namespace'findOrCreate [#_"Symbol" name]
        (or (Namespace'find name)
            (let [#_"Namespace" ns (Namespace'new name)]
                (swap! Namespace'namespaces assoc name ns)
                ns
            )
        )
    )

(defn create-ns [sym] (Namespace'findOrCreate sym))

    (defn #_"Appendable" Namespace''append [#_"Namespace" this, #_"Appendable" a]
        (Appendable'''append a, (:name (:name this)))
    )

    (defn #_"any" Namespace''getMapping [#_"Namespace" this, #_"Symbol" name]
        (get (deref (:mappings this)) name)
    )

    (defn #_"var" Namespace''intern [#_"Namespace" this, #_"Symbol" sym]
        (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
            (let [#_"any" o
                    (or (get (deref (:mappings this)) sym)
                        (let [#_"var" v (Var'new this, sym)]
                            (swap! (:mappings this) assoc sym v)
                            v
                        )
                    )]
                (when-not (and (var? o) (= (:ns o) this)) => o
                    (let [#_"var" v (Var'new this, sym)]
                        (swap! (:mappings this) assoc sym v)
                        v
                    )
                )
            )
        )
    )

    (defn #_"var" Namespace''findInternedVar [#_"Namespace" this, #_"Symbol" name]
        (let [#_"any" o (get (deref (:mappings this)) name)]
            (when (and (var? o) (= (:ns o) this))
                o
            )
        )
    )

    (defm Namespace IObject
        (IObject'''equals => identical?)
    )

    (defm Namespace IAppend
        (IAppend'''append => Namespace''append)
    )
)
)

(about #_"cloiure.core"

(defn destructure [bindings]
    (letfn [(vec- [v x y]
                (let [v' (gensym "v__") s' (gensym "s__") f' (gensym "f__") amp (some (hash-set '&) x)]
                    (loop-when [v (let [v (conj v v' y)] (if amp (conj v s' `(seq ~v')) v)) n 0 s (seq x) amp? false] s => v
                        (condp = (first s)
                            '&  (recur (destructure- v (second s) s') n (next (next s)) true)
                            :as (destructure- v (second s) v')
                                (when-not amp? => (throw! "unsupported binding form, only :as can follow & parameter")
                                    (recur
                                        (destructure- (if amp (conj v f' `(first ~s') s' `(next ~s')) v)
                                            (first s)
                                            (if amp f' `(nth ~v' ~n nil))
                                        )
                                        (inc n) (next s) amp?
                                    )
                                )
                        )
                    )
                )
            )
            (map- [v x y]
                (let [m' (gensym "m__") as (:as x) or* (:or x)
                      v (conj v m' y m' `(if (seq? ~m') (apply hash-map ~m') ~m')) v (if as (conj v as m') v)
                      s (reduce
                            (fn [m e] (reduce (fn [a1 a2] (assoc a1 a2 ((val e) a2))) (dissoc m (key e)) ((key e) m)))
                            (dissoc x :as :or)
                            (reduce
                                (fn [m k]
                                    (when (keyword? k) => m
                                        (let [ns (namespace k)]
                                            (condp = (name k)
                                                "keys" (assoc m k (fn [z] (keyword (or ns (namespace z)) (name z))))
                                                "syms" (assoc m k (fn [z] (list 'quote (symbol (or ns (namespace z)) (name z)))))
                                                "strs" (assoc m k str)
                                                       m
                                            )
                                        )
                                    )
                                )
                                (hash-map) (keys x)
                            )
                        )]
                    (loop-when [v v s (seq s)] s => v
                        (let [x (key (first s)) k (val (first s))
                              local (if (satisfies? INamed x) (symbol nil (name x)) x)
                              y (if (contains? or* local)
                                    `(get ~m' ~k ~(or* local))
                                    `(get ~m' ~k)
                                )]
                            (recur (if (or (symbol? x) (keyword? x)) (conj v local y) (destructure- v x y)) (next s))
                        )
                    )
                )
            )
            (destructure- [v x y]
                (cond
                    (symbol? x) (conj v x y)
                    (vector? x) (vec- v x y)
                    (map? x)    (map- v x y)
                    :else       (throw! (str "unsupported binding form: " x))
                )
            )]
        (let [pairs (partition 2 bindings)]
            (if (every? symbol? (map first pairs))
                bindings
                (reduce (fn [a1 a2] (destructure- a1 (first a2) (second a2))) (vector) pairs)
            )
        )
    )
)

#_oops!
(defmacro let [bindings & body]
    `(let* ~(destructure bindings) ~@body)
)

(defn maybe-destructured [pars body]
    (if (every? symbol? pars)
        (cons (vec pars) body)
        (loop-when [s (seq pars) pars (vector) lets (vector)] s => `(~pars (let ~lets ~@body))
            (if (symbol? (first s))
                (recur (next s) (conj pars (first s)) lets)
                (let [p' (gensym "p__")]
                    (recur (next s) (conj pars p') (conj lets (first s) p'))
                )
            )
        )
    )
)

#_oops!
(defmacro fn [& s]
    (let [name (when (symbol? (first s)) (first s)) s (if name (next s) s)
          s (if (vector? (first s))
                (list s)
                (if (seq? (first s))
                    s
                    (throw!
                        (if (seq s)
                            (str "parameter declaration " (first s) " should be a vector")
                            (str "parameter declaration missing")
                        )
                    )
                )
            )
          sig-
            (fn* [sig]
                (when (seq? sig) => (throw! (str "invalid signature " sig " should be a list"))
                    (let-when [[pars & body] sig] (vector? pars) => (throw!
                                                                        (if (seq? (first s))
                                                                            (str "parameter declaration " pars " should be a vector")
                                                                            (str "invalid signature " sig " should be a list")
                                                                        )
                                                                    )
                        (maybe-destructured pars (or (and (map? (first body)) (next body)) body))
                    )
                )
            )
          s (map sig- s)]
        (if name (list* 'fn* name s) (cons 'fn* s))
    )
)

#_oops!
(defmacro loop [bindings & body]
    (if (= (destructure bindings) bindings)
        `(loop* ~bindings ~@body)
        (let [s (take-nth 2 bindings) s' (map (fn [z] (if (symbol? z) z (gensym))) s)
              v (reduce
                    (fn [v [x y z]] (if (symbol? x) (conj v z y) (conj v z y x z)))
                    (vector) (map vector s (take-nth 2 (drop 1 bindings)) s')
                )]
            `(let ~v
                (loop* ~(vec (interleave s' s'))
                    (let ~(vec (interleave s s'))
                        ~@body
                    )
                )
            )
        )
    )
)

#_oops!
(defmacro defn [name & s]
    (when (symbol? name) => (throw! "first argument to defn must be a symbol")
        (let [s (if (vector? (first s)) (list s) s)]
            (list 'def name (cons `fn s))
        )
    )
)

(defn memoize [f]
    (let [mem (atom (hash-map))]
        (fn [& args]
            (if-some [e (find (deref mem) args)]
                (val e)
                (let [r (apply f args)]
                    (swap! mem assoc args r)
                    r
                )
            )
        )
    )
)
)

(about #_"arbace.arm.Compiler"
    (defp Expr
        (#_"gen" Expr'''emit [#_"Expr" this, #_"Context" context, #_"map" scope, #_"gen" gen])
    )

    (defp Recur)

    (defp LiteralExpr)
    (defp UnresolvedVarExpr)
    (defp VarExpr)
    (defp BodyExpr)
    (defp IfExpr)
    (defp VectorExpr)
    (defp InvokeExpr)
    (defp LocalBinding)
    (defp LocalBindingExpr)
    (defp FnMethod)
    (defp FnExpr)
    (defp DefExpr)
    (defp LetFnExpr)
    (defp LetExpr)
    (defp RecurExpr)
    (defp ThrowExpr)
)

(about #_"arbace.arm.Machine"

(about #_"Machine"
    (defn #_"any" Machine'compute [#_"code" code, #_"array" vars]
        (loop [#_"stack" s nil #_"int" i 0]
            (let [[x y] (nth code i)]
                (condp = x
                    :anew              (let [[    a & s] s]                             (recur (cons (anew a) s)           (inc i)))
                    :apply             (let [[  b a & s] s]                             (recur (cons (apply a b) s)        (inc i)))
                    :aset              (let [[c b a & s] s] (aset! a b c)               (recur s                           (inc i)))
                    :create            (let [[    a & s] s]                             (recur (cons (Closure'new y, a) s) (inc i)))
                    :dup               (let [[    a]     s]                             (recur (cons a s)                  (inc i)))
                    :get               (let [[    a & s] s]                             (recur (cons (get (deref (:_env a)) y) s) (inc i)))
                    :goto                                                               (recur s                        (deref y))
                    :if-eq?            (let [[  b a & s] s]                             (recur s        (if     (= a b) (deref y) (inc i))))
                    :if-nil?           (let [[    a & s] s]                             (recur s        (if  (nil? a)   (deref y) (inc i))))
                    :invoke-1          (let [[    a & s] s]                             (recur (cons (y a) s)              (inc i)))
                    :invoke-2          (let [[  b a & s] s]                             (recur (cons (y a b) s)            (inc i)))
                    :load                                                               (recur (cons (aget vars y) s)      (inc i))
                    :pop                                                                (recur (next s)                    (inc i))
                    :push                                                               (recur (cons y s)                  (inc i))
                    :put               (let [[  b a & s] s] (swap! (:_env a) assoc y b) (recur s                           (inc i)))
                    :return                                 (first s)
                    :store             (let [[    a & s] s] (aset! vars y a)            (recur s                           (inc i)))
                    :throw                                  (throw (first s))
                )
            )
        )
    )
)
)

(about #_"arbace.arm.Compiler"

(about #_"asm"
    (defn #_"gen" Gen'new [] (vector))

    (defn #_"label" Gen''label [#_"gen" gen] (atom nil))

    (defn Gen''mark
        (#_"label" [#_"gen" gen] (atom (count gen)))
        (#_"gen" [#_"gen" gen, #_"label" label] (reset! label (count gen)) gen)
    )

    (defn #_"gen" Gen''anew          [#_"gen" gen]                          (conj gen [:anew]))
    (defn #_"gen" Gen''apply         [#_"gen" gen]                          (conj gen [:apply]))
    (defn #_"gen" Gen''aset          [#_"gen" gen]                          (conj gen [:aset]))
    (defn #_"gen" Gen''create        [#_"gen" gen, #_"FnExpr" fun]          (conj gen [:create fun]))
    (defn #_"gen" Gen''dup           [#_"gen" gen]                          (conj gen [:dup]))
    (defn #_"gen" Gen''get           [#_"gen" gen, #_"Symbol" name]         (conj gen [:get name]))
    (defn #_"gen" Gen''goto          [#_"gen" gen, #_"label" label]         (conj gen [:goto label]))
    (defn #_"gen" Gen''if-eq?        [#_"gen" gen, #_"label" label]         (conj gen [:if-eq? label]))
    (defn #_"gen" Gen''if-nil?       [#_"gen" gen, #_"label" label]         (conj gen [:if-nil? label]))
    (defn #_"gen" Gen''invoke        [#_"gen" gen, #_"fn" f, #_"int" arity] (conj gen [(keyword (str "invoke-" arity)) f]))
    (defn #_"gen" Gen''load          [#_"gen" gen, #_"int" index]           (conj gen [:load index]))
    (defn #_"gen" Gen''pop           [#_"gen" gen]                          (conj gen [:pop]))
    (defn #_"gen" Gen''push          [#_"gen" gen, #_"value" value]         (conj gen [:push value]))
    (defn #_"gen" Gen''put           [#_"gen" gen, #_"Symbol" name]         (conj gen [:put name]))
    (defn #_"gen" Gen''return        [#_"gen" gen]                          (conj gen [:return]))
    (defn #_"gen" Gen''store         [#_"gen" gen, #_"int" index]           (conj gen [:store index]))
    (defn #_"gen" Gen''throw         [#_"gen" gen]                          (conj gen [:throw]))
)

(def Context'enum-set
    (hash-set
        :Context'STATEMENT
        :Context'EXPRESSION
        :Context'RETURN
    )
)

(about #_"Compiler"
    (def #_"int" Compiler'MAX_POSITIONAL_ARITY #_9 (+ 9 2))

    (defn #_"Namespace" Compiler'namespaceFor
        ([#_"Symbol" sym] (Compiler'namespaceFor *ns*, sym))
        ([#_"Namespace" inns, #_"Symbol" sym]
            (find-ns (symbol (:ns sym)))
        )
    )

    (defn #_"Symbol" Compiler'resolveSymbol [#_"Symbol" sym]
        (cond
            (pos? (String''indexOf (:name sym), (int (char! "."))))
                sym
            (some? (:ns sym))
                (let [#_"Namespace" ns (Compiler'namespaceFor sym)]
                    (if (and (some? ns) (not (and (some? (:name (:name ns))) (= (:name (:name ns)) (:ns sym)))))
                        (symbol (:name (:name ns)) (:name sym))
                        sym
                    )
                )
            :else
                (let [#_"any" o (Namespace''getMapping *ns*, sym)]
                    (cond
                        (nil? o) (symbol (:name (:name *ns*)) (:name sym))
                        (var? o) (symbol (:name (:name (:ns o))) (:name (:sym o)))
                    )
                )
        )
    )

    (defn #_"Var" Compiler'lookupVar [#_"Symbol" sym, #_"boolean" intern?]
        (cond
            (some? (:ns sym))
                (when-some [#_"Namespace" ns (Compiler'namespaceFor sym)]
                    (let [#_"Symbol" name (symbol (:name sym))]
                        (if (and intern? (= ns *ns*))
                            (Namespace''intern ns, name)
                            (Namespace''findInternedVar ns, name)
                        )
                    )
                )
            :else
                (let [#_"any" o (Namespace''getMapping *ns*, sym)]
                    (cond
                        (nil? o)
                            (when intern?
                                (Namespace''intern *ns*, (symbol (:name sym)))
                            )
                        (var? o)
                            o
                        :else
                            (throw! (str "expecting var, but " sym " is mapped to " o))
                    )
                )
        )
    )

    (defn #_"any" Compiler'resolveIn [#_"Namespace" n, #_"Symbol" sym, #_"boolean" allowPrivate]
        (cond
            (some? (:ns sym))
                (when-some [#_"Namespace" ns (Compiler'namespaceFor n, sym)]                     => (throw! (str "no such namespace: " (:ns sym)))
                    (when-some [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))] => (throw! (str "no such var: " sym))
                        v
                    )
                )
            :else
                (or (Namespace''getMapping n, sym) (throw! (str "unable to resolve symbol: " sym " in this context")))
        )
    )

    (defn #_"any" Compiler'resolve
        ([#_"Symbol" sym                          ] (Compiler'resolveIn *ns*, sym, false       ))
        ([#_"Symbol" sym, #_"boolean" allowPrivate] (Compiler'resolveIn *ns*, sym, allowPrivate))
    )

    (defn #_"gen" Compiler'emitArgs [#_"map" scope, #_"gen" gen, #_"indexed" args]
        (let [
            gen (Gen''push gen, (count args))
            gen (Gen''anew gen)
        ]
            (loop-when [gen gen #_"int" i 0] (< i (count args)) => gen
                (let [
                    gen (Gen''dup gen)
                    gen (Gen''push gen, i)
                    gen (Expr'''emit (nth args i), :Context'EXPRESSION, scope, gen)
                    gen (Gen''aset gen)
                ]
                    (recur gen (inc i))
                )
            )
        )
    )

    (defn #_"gen" Compiler'emitLocals [#_"map" scope, #_"gen" gen, #_"map" locals]
        (let [
            gen (Gen''push gen, (bit-shift-left (count locals) 1))
            gen (Gen''anew gen)
        ]
            (loop-when [gen gen #_"int" i 0 #_"seq" s (vals locals)] (some? s) => gen
                (let [
                    #_"LocalBinding" lb (first s)
                    gen (Gen''dup gen)
                    gen (Gen''push gen, i)
                    gen (Gen''push gen, (:sym lb))
                    gen (Gen''aset gen)
                    i (inc i)
                    gen (Gen''dup gen)
                    gen (Gen''push gen, i)
                    gen (FnMethod''emitLocal (get scope :fm), gen, lb)
                    gen (Gen''aset gen)
                    i (inc i)
                ]
                    (recur gen i (next s))
                )
            )
        )
    )
)

(about #_"LiteralExpr"
    (defr LiteralExpr)

    (defn #_"LiteralExpr" LiteralExpr'new [#_"any" value]
        (new* LiteralExpr'class
            (hash-map
                #_"any" :value value
            )
        )
    )

    (def #_"LiteralExpr" LiteralExpr'NIL   (LiteralExpr'new nil))
    (def #_"LiteralExpr" LiteralExpr'TRUE  (LiteralExpr'new true))
    (def #_"LiteralExpr" LiteralExpr'FALSE (LiteralExpr'new false))

    (defn #_"Expr" LiteralExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"int" n (dec (count form))]
            (when (= n 1) => (throw! (str "wrong number of arguments passed to quote: " n))
                (let [#_"any" value (second form)]
                    (condp = value
                        nil                 LiteralExpr'NIL
                        true                LiteralExpr'TRUE
                        false               LiteralExpr'FALSE
                        (cond
                            (string? value) (LiteralExpr'new (String''intern value))
                            :else           (LiteralExpr'new value)
                        )
                    )
                )
            )
        )
    )

    (defn #_"gen" LiteralExpr''emit [#_"LiteralExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (Gen''push gen, (:value this))
        )
    )

    (defm LiteralExpr Expr
        (Expr'''emit => LiteralExpr''emit)
    )
)

(about #_"UnresolvedVarExpr"
    (defr UnresolvedVarExpr)

    (defn #_"UnresolvedVarExpr" UnresolvedVarExpr'new [#_"Symbol" symbol]
        (new* UnresolvedVarExpr'class
            (hash-map
                #_"Symbol" :symbol symbol
            )
        )
    )

    (defn #_"gen" UnresolvedVarExpr''emit [#_"UnresolvedVarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        gen
    )

    (defm UnresolvedVarExpr Expr
        (Expr'''emit => UnresolvedVarExpr''emit)
    )
)

(about #_"VarExpr"
    (defr VarExpr)

    (defn #_"VarExpr" VarExpr'new [#_"Var" var]
        (new* VarExpr'class
            (hash-map
                #_"Var" :var var
            )
        )
    )

    (defn #_"gen" VarExpr''emit [#_"VarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Gen''push gen, (:var this))
            gen (Gen''invoke gen, var-get, 1)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm VarExpr Expr
        (Expr'''emit => VarExpr''emit)
    )
)

(about #_"BodyExpr"
    (defr BodyExpr)

    (defn #_"BodyExpr" BodyExpr'new [#_"vector" exprs]
        (new* BodyExpr'class
            (hash-map
                #_"vector" :exprs exprs
            )
        )
    )

    (defn #_"Expr" BodyExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"seq" s form s (if (= (first s) 'do) (next s) s)
              #_"vector" v
                (loop-when [v (vector) s s] (some? s) => v
                    (let [#_"Context" c (if (or (= context :Context'STATEMENT) (some? (next s))) :Context'STATEMENT context)]
                        (recur (conj v (Compiler'analyze (first s), c, scope)) (next s))
                    )
                )]
            (BodyExpr'new (if (pos? (count v)) v (conj v LiteralExpr'NIL)))
        )
    )

    (defn #_"gen" BodyExpr''emit [#_"BodyExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (loop-when-recur [gen gen #_"seq" s (seq (:exprs this))]
                         (some? (next s))
                         [(Expr'''emit (first s), :Context'STATEMENT, scope, gen) (next s)]
                      => (Expr'''emit (first s), context, scope, gen)
        )
    )

    (defm BodyExpr Expr
        (Expr'''emit => BodyExpr''emit)
    )
)

(about #_"IfExpr"
    (defr IfExpr)

    (defn #_"IfExpr" IfExpr'new [#_"Expr" test, #_"Expr" then, #_"Expr" else]
        (new* IfExpr'class
            (hash-map
                #_"Expr" :test test
                #_"Expr" :then then
                #_"Expr" :else else
            )
        )
    )

    (defn #_"Expr" IfExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (cond
            (< 4 (count form)) (throw! "too many arguments to if")
            (< (count form) 3) (throw! "too few arguments to if")
        )
        (let [#_"Expr" test (Compiler'analyze (second form), scope)
              #_"Expr" then (Compiler'analyze (third form), context, scope)
              #_"Expr" else (Compiler'analyze (fourth form), context, scope)]
            (IfExpr'new test, then, else)
        )
    )

    (defn #_"gen" IfExpr''emit [#_"IfExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            #_"label" l'nil (Gen''label gen) #_"label" l'false (Gen''label gen) #_"label" l'end (Gen''label gen)
            gen (Expr'''emit (:test this), :Context'EXPRESSION, scope, gen)
            gen (Gen''dup gen)
            gen (Gen''if-nil? gen, l'nil)
            gen (Gen''push gen, false)
            gen (Gen''if-eq? gen, l'false)
            gen (Expr'''emit (:then this), context, scope, gen)
            gen (Gen''goto gen, l'end)
            gen (Gen''mark gen, l'nil)
            gen (Gen''pop gen)
            gen (Gen''mark gen, l'false)
            gen (Expr'''emit (:else this), context, scope, gen)
            gen (Gen''mark gen, l'end)
        ]
            gen
        )
    )

    (defm IfExpr Expr
        (Expr'''emit => IfExpr''emit)
    )
)

(about #_"VectorExpr"
    (defr VectorExpr)

    (defn #_"VectorExpr" VectorExpr'new [#_"vector" args]
        (new* VectorExpr'class
            (hash-map
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" VectorExpr'parse [#_"vector" form, #_"map" scope]
        (let [[#_"vector" args #_"boolean" literal?]
                (loop-when [args (vector) literal? true #_"seq" s (seq form)] (some? s) => [args literal?]
                    (let [#_"Expr" e (Compiler'analyze (first s), scope)]
                        (recur (conj args e) (and literal? (satisfies? LiteralExpr e)) (next s))
                    )
                )
        ]
            (when literal? => (VectorExpr'new args)
                (LiteralExpr'new (vec (map :value args)))
            )
        )
    )

    (defn #_"gen" VectorExpr''emit [#_"VectorExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (when (seq (:args this)) => (Gen''push gen, PersistentVector'EMPTY)
                    (let [gen (Compiler'emitArgs scope, gen, (:args this))]
                        (Gen''invoke gen, vec, 1)
                    )
                )
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm VectorExpr Expr
        (Expr'''emit => VectorExpr''emit)
    )
)

(about #_"InvokeExpr"
    (defr InvokeExpr)

    (defn #_"InvokeExpr" InvokeExpr'new [#_"Expr" fexpr, #_"vector" args]
        (new* InvokeExpr'class
            (hash-map
                #_"Expr" :fexpr fexpr
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" InvokeExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"Expr" fexpr (Compiler'analyze (first form), scope)
              #_"vector" args (vec (map (fn [a] (Compiler'analyze a, scope)) (next form)))]
            (InvokeExpr'new fexpr, args)
        )
    )

    (defn #_"gen" InvokeExpr''emit [#_"InvokeExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:fexpr this), :Context'EXPRESSION, scope, gen)
            gen (Compiler'emitArgs scope, gen, (:args this))
            gen (Gen''apply gen)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm InvokeExpr Expr
        (Expr'''emit => InvokeExpr''emit)
    )
)

(about #_"LocalBinding"
    (defr LocalBinding)

    (defn #_"LocalBinding" LocalBinding'new [#_"Symbol" sym, #_"Expr" init, #_"int" idx]
        (new* LocalBinding'class
            (hash-map
                #_"int" :uid (next-id!)
                #_"Symbol" :sym sym
                #_"Expr'" :'init (atom init)
                #_"int" :idx idx
            )
        )
    )
)

(about #_"LocalBindingExpr"
    (defr LocalBindingExpr)

    (defn #_"LocalBindingExpr" LocalBindingExpr'new [#_"LocalBinding" lb]
        (new* LocalBindingExpr'class
            (hash-map
                #_"LocalBinding" :lb lb
            )
        )
    )

    (defn #_"gen" LocalBindingExpr''emit [#_"LocalBindingExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (FnMethod''emitLocal (get scope :fm), gen, (:lb this))
        )
    )

    (defm LocalBindingExpr Expr
        (Expr'''emit => LocalBindingExpr''emit)
    )
)

(about #_"FnMethod"
    (defr FnMethod)

    (defn #_"FnMethod" FnMethod'new [#_"FnExpr" fun, #_"FnMethod" parent]
        (new* FnMethod'class
            (hash-map
                #_"FnExpr" :fun fun
                #_"FnMethod" :parent parent
                #_"{int LocalBinding}'" :'locals (atom (hash-map))
                #_"Number" :arity nil
                #_"Expr" :body nil
            )
        )
    )

    (defn #_"FnMethod" FnMethod'parse [#_"FnExpr" fun, #_"seq" form, #_"map" scope]
        (let [
            scope
                (-> scope
                    (update :fm (fn [parent] (FnMethod'new fun, parent)))
                    (update :'local-env (comp atom deref))
                    (assoc :'local-num (atom 0))
                )
            _
                (when-some [#_"Symbol" f (:fname fun)]
                    (let [#_"LocalBinding" lb (LocalBinding'new f, nil, (deref (get scope :'local-num)))]
                        (swap! (get scope :'local-env) assoc (:sym lb) lb)
                        (swap! (:'locals (get scope :fm)) assoc (:uid lb) lb)
                    )
                )
            [#_"[LocalBinding]" lbs #_"int" arity]
                (loop-when [lbs (vector) arity 0 #_"boolean" variadic? false #_"seq" s (seq (first form))] (some? s) => (if (and variadic? (not (neg? arity))) (throw! "missing variadic parameter") [lbs arity])
                    (let [#_"symbol?" sym (first s)]
                        (when (symbol? sym)        => (throw! "function parameters must be symbols")
                            (when (nil? (:ns sym)) => (throw! (str "can't use qualified name as parameter: " sym))
                                (cond
                                    (= sym '&)
                                        (when-not variadic? => (throw! "overkill variadic parameter list")
                                            (recur lbs arity true (next s))
                                        )
                                    (neg? arity)
                                        (throw! (str "excess variadic parameter: " sym))
                                    ((if variadic? <= <) arity Compiler'MAX_POSITIONAL_ARITY)
                                        (let [
                                            arity (if-not variadic? (inc arity) (- (inc arity)))
                                            #_"LocalBinding" lb (LocalBinding'new sym, nil, (swap! (get scope :'local-num) inc))
                                        ]
                                            (swap! (get scope :'local-env) assoc (:sym lb) lb)
                                            (swap! (:'locals (get scope :fm)) assoc (:uid lb) lb)
                                            (recur (conj lbs lb) arity variadic? (next s))
                                        )
                                    :else
                                        (throw! (str "can't specify more than " Compiler'MAX_POSITIONAL_ARITY " positional parameters"))
                                )
                            )
                        )
                    )
                )
            scope
                (-> scope
                    (assoc :loop-locals lbs)
                    (update :fm assoc :arity arity)
                )
        ]
            (assoc (get scope :fm) :body (BodyExpr'parse (next form), :Context'RETURN, scope))
        )
    )

    (defn #_"gen" FnMethod''emitLocal [#_"FnMethod" this, #_"gen" gen, #_"LocalBinding" lb]
        (if (contains? (deref (:'closes (:fun this))) (:uid lb))
            (let [
                gen (Gen''load gen, 0)
                gen (Gen''get gen, (:sym lb))
            ]
                gen
            )
            (Gen''load gen, (:idx lb))
        )
    )

    (defn #_"gen" FnMethod''compile [#_"FnMethod" this]
        (let [
            #_"map" scope (hash-map :fm this)
            #_"gen" gen (Gen'new)
            scope (assoc scope :loop-label (Gen''mark gen))
            gen (Expr'''emit (:body this), :Context'RETURN, scope, gen)
        ]
            (Gen''return gen)
        )
    )

    (def compile-and-memoize (-/memoize FnMethod''compile))
)

(about #_"FnExpr"
    (defr FnExpr)

    (defn #_"FnExpr" FnExpr'new []
        (new* FnExpr'class
            (hash-map
                #_"Symbol" :fname nil
                #_"{int FnMethod}" :regulars nil
                #_"FnMethod" :variadic nil
                #_"{int LocalBinding}'" :'closes (atom (hash-map))
            )
        )
    )

    (defn #_"Expr" FnExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [
            #_"FnExpr" fun (FnExpr'new)
            [fun form]
                (when (symbol? (second form)) => [fun form]
                    [(assoc fun :fname (second form)) (cons 'fn* (next (next form)))]
                )
            form
                (when (vector? (second form)) => form
                    (list 'fn* (next form))
                )
            [#_"{int FnMethod}" regulars #_"FnMethod" variadic]
                (loop-when [regulars (hash-map) variadic nil #_"seq" s (next form)] (some? s) => [regulars variadic]
                    (let [#_"FnMethod" fm (FnMethod'parse fun, (first s), scope) #_"int" n (:arity fm)]
                        (if (neg? n)
                            (when (nil? variadic) => (throw! "can't have more than 1 variadic overload")
                                (recur regulars fm (next s))
                            )
                            (when (nil? (get regulars n)) => (throw! "can't have 2 overloads with same arity")
                                (recur (assoc regulars n fm) variadic (next s))
                            )
                        )
                    )
                )
        ]
            (when (some? variadic)
                (loop-when-recur [#_"int" n (- (:arity variadic))] (<= n Compiler'MAX_POSITIONAL_ARITY) [(inc n)]
                    (when (some? (get regulars n))
                        (throw! "can't have fixed arity function with more params than variadic function")
                    )
                )
            )
            (assoc fun :regulars regulars, :variadic variadic)
        )
    )

    (defn #_"gen" FnExpr''emit [#_"FnExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (let [
                gen (Compiler'emitLocals scope, gen, (deref (:'closes this)))
                gen (Gen''invoke gen, RT'mapUniqueKeys, 1)
            ]
                (Gen''create gen, this)
            )
        )
    )

    (defm FnExpr Expr
        (Expr'''emit => FnExpr''emit)
    )
)

(about #_"DefExpr"
    (defr DefExpr)

    (defn #_"DefExpr" DefExpr'new [#_"Var" var, #_"Expr" init, #_"boolean" initProvided]
        (new* DefExpr'class
            (hash-map
                #_"Var" :var var
                #_"Expr" :init init
                #_"boolean" :initProvided initProvided
            )
        )
    )

    (defn #_"Expr" DefExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"int" n (count form)]
            (cond
                (< 3 n) (throw! "too many arguments to def")
                (< n 2) (throw! "too few arguments to def")
                :else
                    (let-when [#_"symbol?" s (second form)] (symbol? s)     => (throw! "first argument to def must be a symbol")
                        (when-some [#_"Var" v (Compiler'lookupVar s, true)] => (throw! "can't refer to qualified var that doesn't exist")
                            (let [v (when-not (= (:ns v) *ns*) => v
                                        (when (nil? (:ns s))                => (throw! "can't create defs outside of current ns")
                                            (Namespace''intern *ns*, s)
                                        )
                                    )]
                                (DefExpr'new v, (Compiler'analyze (third form), scope), (= n 3))
                            )
                        )
                    )
            )
        )
    )

    (defn #_"gen" DefExpr''emit [#_"DefExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Gen''push gen, (:var this))
            gen
                (when (:initProvided this) => gen
                    (let [
                        gen (Gen''dup gen)
                        gen (Expr'''emit (:init this), :Context'EXPRESSION, scope, gen)
                        gen (Gen''invoke gen, Var''bindRoot, 2)
                    ]
                        (Gen''pop gen)
                    )
                )
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm DefExpr Expr
        (Expr'''emit => DefExpr''emit)
    )
)

(about #_"LetFnExpr"
    (defr LetFnExpr)

    (defn #_"LetFnExpr" LetFnExpr'new [#_"[LocalBinding]" bindings, #_"Expr" body]
        (new* LetFnExpr'class
            (hash-map
                #_"[LocalBinding]" :bindings bindings
                #_"Expr" :body body
            )
        )
    )

    (defn #_"Expr" LetFnExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"vector?" bindings (second form)]
            (when (vector? bindings)           => (throw! "bad binding form, expected vector")
                (when (even? (count bindings)) => (throw! "bad binding form, expected matched symbol expression pairs")
                    (let [
                        scope (update scope :'local-env (comp atom deref))
                        scope (update scope :'local-num (comp atom deref))
                        #_"[LocalBinding]" lbs
                            (loop-when [lbs (vector) #_"seq" s (seq bindings)] (some? s) => lbs
                                (let [#_"symbol?" sym (first s)]
                                    (when (symbol? sym)        => (throw! (str "bad binding form, expected symbol, got: " sym))
                                        (when (nil? (:ns sym)) => (throw! (str "can't let qualified name: " sym))
                                            (let [
                                                #_"LocalBinding" lb (LocalBinding'new sym, nil, (swap! (get scope :'local-num) inc))
                                            ]
                                                (swap! (get scope :'local-env) assoc (:sym lb) lb)
                                                (swap! (:'locals (get scope :fm)) assoc (:uid lb) lb)
                                                (recur (conj lbs lb) (next (next s)))
                                            )
                                        )
                                    )
                                )
                            )
                        _
                            (loop-when-recur [#_"int" i 0] (< i (count bindings)) [(+ i 2)]
                                (reset! (:'init (nth lbs (quot i 2))) (Compiler'analyze (nth bindings (inc i)), scope))
                            )
                    ]
                        (LetFnExpr'new lbs, (BodyExpr'parse (next (next form)), context, scope))
                    )
                )
            )
        )
    )

    (defn #_"gen" LetFnExpr''emit [#_"LetFnExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (loop-when [gen gen #_"seq" s (seq (:bindings this))] (some? s) => gen
                    (let [
                        #_"LocalBinding" lb (first s)
                        gen (Gen''push gen, nil)
                        gen (Gen''store gen, (:idx lb))
                    ]
                        (recur gen (next s))
                    )
                )
            [#_"{int}" lbset gen]
                (loop-when [lbset (hash-set) gen gen #_"seq" s (seq (:bindings this))] (some? s) => [lbset gen]
                    (let [
                        #_"LocalBinding" lb (first s)
                        gen (Expr'''emit (deref (:'init lb)), :Context'EXPRESSION, scope, gen)
                        gen (Gen''store gen, (:idx lb))
                    ]
                        (recur (conj lbset (:uid lb)) gen (next s))
                    )
                )
            gen
                (loop-when [gen gen #_"seq" s (seq (:bindings this))] (some? s) => gen
                    (let [
                        #_"LocalBinding" lb (first s)
                        gen (Gen''load gen, (:idx lb))
                        gen
                            (loop-when [gen gen #_"seq" s (vals (deref (:'closes (deref (:'init lb)))))] (some? s) => gen
                                (let [
                                    gen
                                        (let-when [#_"LocalBinding" lb (first s)] (contains? lbset (:uid lb)) => gen
                                            (let [
                                                gen (Gen''dup gen)
                                                gen (FnMethod''emitLocal (get scope :fm), gen, lb)
                                                gen (Gen''put gen, (:sym lb))
                                            ]
                                                gen
                                            )
                                        )
                                ]
                                    (recur gen (next s))
                                )
                            )
                        gen (Gen''pop gen)
                    ]
                        (recur gen (next s))
                    )
                )
        ]
            (Expr'''emit (:body this), context, scope, gen)
        )
    )

    (defm LetFnExpr Expr
        (Expr'''emit => LetFnExpr''emit)
    )
)

(about #_"LetExpr"
    (defr LetExpr)

    (defn #_"LetExpr" LetExpr'new [#_"[LocalBinding]" bindings, #_"Expr" body, #_"boolean" loop?]
        (new* LetExpr'class
            (hash-map
                #_"[LocalBinding]" :bindings bindings
                #_"Expr" :body body
                #_"boolean" :loop? loop?
            )
        )
    )

    (defn #_"Expr" LetExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"vector?" bindings (second form)]
            (when (vector? bindings)           => (throw! "bad binding form, expected vector")
                (when (even? (count bindings)) => (throw! "bad binding form, expected matched symbol expression pairs")
                    (let [
                        scope (update scope :'local-env (comp atom deref))
                        scope (update scope :'local-num (comp atom deref))
                        #_"boolean" loop? (= (first form) 'loop*)
                        scope
                            (when loop? => scope
                                (dissoc scope :loop-locals)
                            )
                        #_"[LocalBinding]" lbs
                            (loop-when [lbs (vector) #_"seq" s (seq bindings)] (some? s) => lbs
                                (let [#_"symbol?" sym (first s)]
                                    (when (symbol? sym)        => (throw! (str "bad binding form, expected symbol, got: " sym))
                                        (when (nil? (:ns sym)) => (throw! (str "can't let qualified name: " sym))
                                            (let [
                                                #_"Expr" init (Compiler'analyze (second s), scope)
                                                #_"LocalBinding" lb (LocalBinding'new sym, init, (swap! (get scope :'local-num) inc))
                                            ]
                                                (swap! (get scope :'local-env) assoc (:sym lb) lb)
                                                (swap! (:'locals (get scope :fm)) assoc (:uid lb) lb)
                                                (recur (conj lbs lb) (next (next s)))
                                            )
                                        )
                                    )
                                )
                            )
                        scope
                            (when loop? => scope
                                (assoc scope :loop-locals lbs)
                            )
                        #_"Expr" body (BodyExpr'parse (next (next form)), (if loop? :Context'RETURN context), scope)
                    ]
                        (LetExpr'new lbs, body, loop?)
                    )
                )
            )
        )
    )

    (defn #_"gen" LetExpr''emit [#_"LetExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (loop-when [gen gen #_"seq" s (seq (:bindings this))] (some? s) => gen
                    (let [
                        #_"LocalBinding" lb (first s)
                        gen (Expr'''emit (deref (:'init lb)), :Context'EXPRESSION, scope, gen)
                        gen (Gen''store gen, (:idx lb))
                    ]
                        (recur gen (next s))
                    )
                )
            scope
                (when (:loop? this) => scope
                    (assoc scope :loop-label (Gen''mark gen))
                )
        ]
            (Expr'''emit (:body this), context, scope, gen)
        )
    )

    (defm LetExpr Expr
        (Expr'''emit => LetExpr''emit)
    )
)

(about #_"RecurExpr"
    (defr RecurExpr)

    (defn #_"RecurExpr" RecurExpr'new [#_"vector" loopLocals, #_"vector" args]
        (new* RecurExpr'class
            (hash-map
                #_"vector" :loopLocals loopLocals
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" RecurExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (when (and (= context :Context'RETURN) (some? (get scope :loop-locals))) => (throw! "can only recur from tail position")
            (let [#_"vector" args (vec (map (fn [a] (Compiler'analyze a, scope)) (next form))) #_"int" n (count args) #_"int" m (count (get scope :loop-locals))]
                (when (= n m) => (throw! (str "mismatched argument count to recur, expected: " m " args, got: " n))
                    (RecurExpr'new (get scope :loop-locals), args)
                )
            )
        )
    )

    (defn #_"gen" RecurExpr''emit [#_"RecurExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-some [#_"label" l'loop (get scope :loop-label)] => (throw! "recur misses loop label")
            (let [
                gen
                    (loop-when-recur [gen gen #_"seq" s (seq (:args this))]
                                     (some? s)
                                     [(Expr'''emit (first s), :Context'EXPRESSION, scope, gen) (next s)]
                                  => gen
                    )
                gen
                    (loop-when-recur [gen gen #_"seq" s (rseq (:loopLocals this))]
                                     (some? s)
                                     [(Gen''store gen, (:idx (first s))) (next s)]
                                  => gen
                    )
            ]
                (Gen''goto gen, l'loop)
            )
        )
    )

    (defm RecurExpr Expr
        (Expr'''emit => RecurExpr''emit)
    )
)

(about #_"ThrowExpr"
    (defr ThrowExpr)

    (defn #_"ThrowExpr" ThrowExpr'new [#_"Expr" throwable]
        (new* ThrowExpr'class
            (hash-map
                #_"Expr" :throwable throwable
            )
        )
    )

    (defn #_"Expr" ThrowExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (cond
            (= (count form) 1) (throw! "too few arguments to throw: single Throwable expected")
            (< 2 (count form)) (throw! "too many arguments to throw: single Throwable expected")
            :else              (ThrowExpr'new (Compiler'analyze (second form), scope))
        )
    )

    (defn #_"gen" ThrowExpr''emit [#_"ThrowExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:throwable this), :Context'EXPRESSION, scope, gen)
            gen (Gen''throw gen)
        ]
            gen
        )
    )

    (defm ThrowExpr Expr
        (Expr'''emit => ThrowExpr''emit)
    )
)

(about #_"Compiler"
    (def #_"map" Compiler'specials
        (hash-map
            '&       nil
            'def     DefExpr'parse
            'do      BodyExpr'parse
            'fn*     FnExpr'parse
            'if      IfExpr'parse
            'let*    LetExpr'parse
            'letfn*  LetFnExpr'parse
            'loop*   LetExpr'parse
            'quote   LiteralExpr'parse
            'recur   RecurExpr'parse
            'throw   ThrowExpr'parse
        )
    )

    (defn #_"boolean" Compiler'isSpecial [#_"any" sym]
        (contains? Compiler'specials sym)
    )

    (defn #_"void" Compiler'closeOver [#_"LocalBinding" lb, #_"FnMethod" fm]
        (when (and (some? lb) (some? fm) (not (contains? (deref (:'locals fm)) (:uid lb))))
            (swap! (:'closes (:fun fm)) assoc (:uid lb) lb)
            (Compiler'closeOver lb, (:parent fm))
        )
        nil
    )

    (defn #_"Expr" Compiler'analyzeSymbol [#_"Symbol" sym, #_"map" scope]
        (or
            (when (nil? (:ns sym))
                (when-some [#_"LocalBinding" lb (get (deref (get scope :'local-env)) sym)]
                    (Compiler'closeOver lb, (get scope :fm))
                    (LocalBindingExpr'new lb)
                )
            )
            (let [#_"any" o (Compiler'resolve sym)]
                (cond
                    (var? o)    (VarExpr'new o)
                    (symbol? o) (UnresolvedVarExpr'new o)
                    :else       (throw! (str "unable to resolve symbol: " sym " in this context"))
                )
            )
        )
    )

    (defn #_"Expr" Compiler'analyzeSeq [#_"seq" form, #_"Context" context, #_"map" scope]
        (when-some [#_"any" op (first form)] => (throw! (str "can't call nil, form: " form))
            (let [#_"fn" f'parse (or (get Compiler'specials op) InvokeExpr'parse)]
                (f'parse form, context, scope)
            )
        )
    )

    (defn #_"Expr" Compiler'analyze
        ([#_"edn" form, #_"map" scope] (Compiler'analyze form, :Context'EXPRESSION, scope))
        ([#_"edn" form, #_"Context" context, #_"map" scope]
            (let [form
                    (when (satisfies? LazySeq form) => form
                        (or (seq form) (list))
                    )]
                (condp = form
                    nil                                  LiteralExpr'NIL
                    true                                 LiteralExpr'TRUE
                    false                                LiteralExpr'FALSE
                    (cond
                        (symbol? form)                   (Compiler'analyzeSymbol form, scope)
                        (string? form)                   (LiteralExpr'new (String''intern form))
                        (and (coll? form) (empty? form)) (LiteralExpr'new form)
                        (seq? form)                      (Compiler'analyzeSeq form, context, scope)
                        (vector? form)                   (VectorExpr'parse form, scope)
                        :else                            (LiteralExpr'new form)
                    )
                )
            )
        )
    )

    (defn #_"edn" Compiler'eval
        ([#_"edn" form] (Compiler'eval form, nil))
        ([#_"edn" form, #_"map" scope]
            (IFn'''invoke (Closure'new (Compiler'analyze (list 'fn* [] form), scope), nil))
        )
    )
)
)

(about #_"arbace.arm.LispReader"

(about #_"LispReader"
    (defn #_"Symbol" LispReader'registerGensym [#_"map" scope, #_"Symbol" sym]
        (when (contains? scope :'gensym-env) => (throw! "gensym literal not in syntax-quote")
            (or (get (deref (get scope :'gensym-env)) sym)
                (let [#_"Symbol" gsym (symbol (str (:name sym) "__" (next-id!) "__auto__"))]
                    (swap! (get scope :'gensym-env) assoc sym gsym)
                    gsym
                )
            )
        )
    )

    (defn #_"boolean" LispReader'isMacro [#_"char" ch]
        (contains? LispReader'macros ch)
    )

    (defn #_"boolean" LispReader'isTerminatingMacro [#_"char" ch]
        (and (LispReader'isMacro ch) (not (any = ch (char! "#") (char! "'") (char! "%"))))
    )

    (defn #_"boolean" LispReader'isDigit [#_"char" ch, #_"int" base]
        (not= (Character'digit ch, base) -1)
    )

    (defn #_"boolean" LispReader'isWhitespace [#_"char" ch]
        (or (Character'isWhitespace ch) (= ch (char! ",")))
    )

    (defn #_"Character" LispReader'read1 [#_"Reader" r]
        (let [#_"int" c (Reader''read r)]
            (when-not (= c -1)
                (char c)
            )
        )
    )

    (defn #_"void" LispReader'unread [#_"Reader" r, #_"Character" ch]
        (when (some? ch)
            (Reader''unread r, (int ch))
        )
        nil
    )

    (def #_"Pattern" LispReader'rxInteger #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)")

    (defn #_"any" LispReader'matchNumber [#_"String" s]
        (let [_ (let-when [#_"Matcher" m (Pattern''matcher LispReader'rxInteger, s)] (Matcher''matches m)
                    (when (nil? (Matcher''group m, 2)) => 0
                        (let [[#_"String" n #_"int" radix]
                                (cond-some
                                    [n (Matcher''group m, 3)] [n 10]
                                    [n (Matcher''group m, 4)] [n 16]
                                    [n (Matcher''group m, 5)] [n 8]
                                    [n (Matcher''group m, 7)] [n (Number'parseInt (Matcher''group m, 6))]
                                )]
                            (when (some? n) => :nil
                                (let [#_"BigInteger" bn (BigInteger'new n, radix) bn (if (= (Matcher''group m, 1) "-") (BigInteger''negate bn) bn)]
                                    (when (< (BigInteger''bitLength bn) 64) => bn
                                        (int (BigInteger''longValue bn))
                                    )
                                )
                            )
                        )
                    )
                )]
            (when-not (= _ :nil) _)
        )
    )

    (defn #_"any" LispReader'readNumber [#_"Reader" r, #_"char" ch]
        (let [#_"String" s
                (let [#_"StringBuilder" sb (StringBuilder'new) _ (StringBuilder''append sb, ch)]
                    (loop []
                        (let [ch (LispReader'read1 r)]
                            (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isMacro ch))
                                (do
                                    (LispReader'unread r, ch)
                                    (StringBuilder''toString sb)
                                )
                                (do
                                    (StringBuilder''append sb, ch)
                                    (recur)
                                )
                            )
                        )
                    )
                )]
            (or (LispReader'matchNumber s) (throw! (str "invalid number: " s)))
        )
    )

    (defn #_"String" LispReader'readToken [#_"Reader" r, #_"char" ch]
        (let [#_"StringBuilder" sb (StringBuilder'new) _ (StringBuilder''append sb, ch)]
            (loop []
                (let [ch (LispReader'read1 r)]
                    (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isTerminatingMacro ch))
                        (do
                            (LispReader'unread r, ch)
                            (StringBuilder''toString sb)
                        )
                        (do
                            (StringBuilder''append sb, ch)
                            (recur)
                        )
                    )
                )
            )
        )
    )

    (def #_"Pattern" LispReader'rxSymbol #"[:]?([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)")

    (defn #_"any" LispReader'matchSymbol [#_"String" s]
        (let-when [#_"Matcher" m (Pattern''matcher LispReader'rxSymbol, s)] (Matcher''matches m)
            (let [#_"String" ns (Matcher''group m, 1) #_"String" n (Matcher''group m, 2)]
                (when-not (or (and (some? ns) (String''endsWith ns, ":/")) (String''endsWith n, ":") (not= (String''indexOf s, "::") -1))
                    (let [#_"boolean" kw? (= (String''charAt s, 0) (char! ":")) #_"Symbol" sym (symbol (String''substring s, (if kw? 1 0)))]
                        (if kw? (keyword sym) sym)
                    )
                )
            )
        )
    )

    (defn #_"any" LispReader'interpretToken [#_"String" s]
        (condp = s "nil" nil "true" true "false" false
            (or (LispReader'matchSymbol s) (throw! (str "invalid token: " s)))
        )
    )

    (defn #_"any" LispReader'read
        ([#_"Reader" r, #_"map" scope] (LispReader'read r, scope, true, nil))
        ([#_"Reader" r, #_"map" scope, #_"boolean" eofIsError, #_"any" eofValue] (LispReader'read r, scope, eofIsError, eofValue, nil, nil))
        ([#_"Reader" r, #_"map" scope, #_"boolean" eofIsError, #_"any" eofValue, #_"Character" returnOn, #_"any" returnOnValue]
            (loop []
                (let [#_"char" ch (loop-when-recur [ch (LispReader'read1 r)] (and (some? ch) (LispReader'isWhitespace ch)) [(LispReader'read1 r)] => ch)]
                    (cond
                        (nil? ch)
                            (if eofIsError (throw! "EOF while reading") eofValue)
                        (and (some? returnOn) (= returnOn ch))
                            returnOnValue
                        (LispReader'isDigit ch, 10)
                            (LispReader'readNumber r, ch)
                        :else
                            (let [#_"fn" f'macro (get LispReader'macros ch)]
                                (if (some? f'macro)
                                    (let [#_"any" o (f'macro r scope ch)]
                                        (recur-when (identical? o r) [] => o)
                                    )
                                    (or
                                        (when (any = ch (char! "+") (char! "-"))
                                            (let [#_"char" ch' (LispReader'read1 r) _ (LispReader'unread r, ch')]
                                                (when (and (some? ch') (LispReader'isDigit ch', 10))
                                                    (LispReader'readNumber r, ch)
                                                )
                                            )
                                        )
                                        (LispReader'interpretToken (LispReader'readToken r, ch))
                                    )
                                )
                            )
                    )
                )
            )
        )
    )

    (def #_"any" LispReader'READ_EOF (anew 0))
    (def #_"any" LispReader'READ_FINISHED (anew 0))

    (defn #_"vector" LispReader'readDelimitedForms [#_"Reader" r, #_"map" scope, #_"char" delim]
        (loop [#_"vector" v (vector)]
            (let [#_"any" form (LispReader'read r, scope, false, LispReader'READ_EOF, delim, LispReader'READ_FINISHED)]
                (condp identical? form
                    LispReader'READ_EOF
                        (throw! "EOF while reading")
                    LispReader'READ_FINISHED
                        v
                    (recur (conj v form))
                )
            )
        )
    )
)

(about #_"StringReader"
    (defn #_"char" StringReader'escape [#_"Reader" r]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading string")
            (condp = ch
                (char! "\\") ch
                (char! "\"") ch
                (throw! (str "unsupported escape character: \\" ch))
            )
        )
    )

    (defn #_"any" string-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (let [#_"StringBuilder" sb (StringBuilder'new)]
            (loop []
                (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading string")
                    (when-not (= ch (char! "\""))
                        (StringBuilder''append sb, (if (= ch (char! "\\")) (StringReader'escape r) ch))
                        (recur)
                    )
                )
            )
            (StringBuilder''toString sb)
        )
    )
)

(about #_"DiscardReader"
    (defn #_"any" discard-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (LispReader'read r, scope)
        r
    )
)

(about #_"QuoteReader"
    (defn #_"any" quote-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (list 'quote (LispReader'read r, scope))
    )
)

(about #_"DispatchReader"
    (defn #_"any" dispatch-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (let-when [#_"fn" f'macro (get LispReader'dispatchMacros ch)] (nil? f'macro) => (f'macro r scope ch)
                (LispReader'unread r, ch)
                (throw! (str "no dispatch macro for: " ch))
            )
        )
    )
)

(about #_"SyntaxQuoteReader"
(def unquote)

    (defn #_"boolean" SyntaxQuoteReader'isUnquote [#_"any" form]
        (and (seq? form) (= (first form) `unquote))
    )

(def unquote-splicing)

    (defn #_"boolean" SyntaxQuoteReader'isUnquoteSplicing [#_"any" form]
        (and (seq? form) (= (first form) `unquote-splicing))
    )

    (defn #_"seq" SyntaxQuoteReader'sqExpandList [#_"map" scope, #_"seq" s]
        (loop-when [#_"vector" v (vector) s s] (some? s) => (seq v)
            (let [#_"any" item (first s)
                  v (cond
                        (SyntaxQuoteReader'isUnquote item)         (conj v (list `list (second item)))
                        (SyntaxQuoteReader'isUnquoteSplicing item) (conj v (second item))
                        :else                                      (conj v (list `list (SyntaxQuoteReader'syntaxQuote scope, item)))
                    )]
                (recur v (next s))
            )
        )
    )

    (defn #_"any" SyntaxQuoteReader'syntaxQuote [#_"map" scope, #_"any" form]
        (cond
            (Compiler'isSpecial form)
                (list 'quote form)
            (symbol? form)
                (let [#_"String" ns (:ns form) #_"String" n (:name form)
                        form
                        (cond
                            (and (nil? ns) (String''endsWith n, "#"))
                                (LispReader'registerGensym scope, (symbol (String''substring n, 0, (dec (String''length n)))))
                            (and (nil? ns) (String''endsWith n, "."))
                                (symbol (str (:name (Compiler'resolveSymbol (symbol (String''substring n, 0, (dec (String''length n)))))) "."))
                            (and (nil? ns) (String''startsWith n, "."))
                                form
                            :else
                                (Compiler'resolveSymbol form)
                        )]
                    (list 'quote form)
                )
            (SyntaxQuoteReader'isUnquote form)
                (second form)
            (SyntaxQuoteReader'isUnquoteSplicing form)
                (throw! "splice not in list")
            (coll? form)
                (cond
                    (map? form)
                        (list `apply `hash-map (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList scope, (seq (mapcat identity form))))))
                    (vector? form)
                        (list `apply `vector (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList scope, (seq form)))))
                    (set? form)
                        (list `apply `hash-set (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList scope, (seq form)))))
                    (or (seq? form) (list? form))
                        (when-some [#_"seq" s (seq form)] => (cons `list nil)
                            (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList scope, s)))
                        )
                    :else
                        (throw! "unknown collection type")
                )
            (or (keyword? form) (number? form) (char? form) (string? form))
                form
            :else
                (list 'quote form)
        )
    )

    (defn #_"any" syntax-quote-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (let [scope (assoc scope :'gensym-env (atom (hash-map)))]
            (SyntaxQuoteReader'syntaxQuote scope, (LispReader'read r, scope))
        )
    )
)

(about #_"UnquoteReader"
    (defn #_"any" unquote-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (if (= ch (char! "@"))
                (list `unquote-splicing (LispReader'read r, scope))
                (do
                    (LispReader'unread r, ch)
                    (list `unquote (LispReader'read r, scope))
                )
            )
        )
    )
)

(about #_"ListReader"
    (defn #_"any" list-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (apply list (LispReader'readDelimitedForms r, scope, (char! ")")))
    )
)

(about #_"VectorReader"
    (defn #_"any" vector-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (vec (LispReader'readDelimitedForms r, scope, (char! "]")))
    )
)

(about #_"UnmatchedDelimiterReader"
    (defn #_"any" unmatched-delimiter-reader [#_"Reader" _r, #_"map" scope, #_"char" delim]
        (throw! (str "unmatched delimiter: " delim))
    )
)

(about #_"LispReader"
    (def #_"{char fn}" LispReader'macros
        (hash-map
            (char! "\"")  string-reader
            (char! "'")  quote-reader
            (char! "`")  syntax-quote-reader
            (char! "~")  unquote-reader
            (char! "(")  list-reader,    (char! ")")  unmatched-delimiter-reader
            (char! "[")  vector-reader,  (char! "]")  unmatched-delimiter-reader
            (char! "#")  dispatch-reader
        )
    )

    (def #_"{char fn}" LispReader'dispatchMacros
        (hash-map
            (char! "_")  discard-reader
        )
    )
)
)

(defn read
    ([] (read -/*in*))
    ([s] (read s true nil))
    ([s eof-error? eof-value] (LispReader'read s, nil, (boolean eof-error?), eof-value))
)

(defn eval [form] (Compiler'eval form))
