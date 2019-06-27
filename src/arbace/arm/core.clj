(ns arbace.arm.core
    (:refer-clojure :only []) (:require [clojure.core :as -])
)

(defn identity [x] x)

(defn nil?  [x] (identical? x nil))
(defn not   [x] (if x false true))
(defn some? [x] (not (nil? x)))

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

    (defn first [s]
        (if (seq? s)
            (ISeq'''first s)
            (let* [s (seq s)]
                (when (some? s)
                    (ISeq'''first s)
                )
            )
        )
    )

    (defn #_"seq" next [s]
        (if (seq? s)
            (ISeq'''next s)
            (let* [s (seq s)]
                (when (some? s)
                    (ISeq'''next s)
                )
            )
        )
    )

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
                    (loop* [n 0 s (seq x)]
                        (when (and (some? s) (or (neg? m) (< n m))) => n
                            (when (counted? s) => (recur (inc n) (next s))
                                (+ n (Counted'''count s))
                            )
                        )
                    )
                :else
                    (throw! (str "count not supported on " x))
            )
        )
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
            (let* [c (conj c x)]
                (when s => c
                    (recur c (first s) (next s))
                )
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
            (let* [a (assoc a k v)]
                (when kvs => a
                    (when (next kvs) => (throw! "assoc expects even number of arguments after map/vector, found odd number")
                        (recur a (first kvs) (second kvs) (next (next kvs)))
                    )
                )
            )
        )
    )

    (defn update
        ([m k f] (assoc m k (f (get m k))))
        ([m k f x] (assoc m k (f (get m k) x)))
        ([m k f x y] (assoc m k (f (get m k) x y)))
        ([m k f x y & z] (assoc m k (apply f (get m k) x y z)))
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
            (let* [m (dissoc m k)]
                (when (some? m)
                    (when ks => m
                        (recur m (first ks) (next ks))
                    )
                )
            )
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

(about #_"arbace.arm.Symbol"
    (defp Symbol)

    (defn symbol? [x] (satisfies? Symbol x))
)

(about #_"arbace.arm.Keyword"
    (defp Keyword)

    (defn keyword? [x] (satisfies? Keyword x))
)

(about #_"arbace.arm.Closure"
    (defp Closure)
)

(about #_"arbace.arm.LazySeq"
    (defp LazySeq)
)

(about #_"arbace.arm.APersistentVector"
    (defp VSeq)
    (defp RSeq)
)

(about #_"arbace.arm.Cons"
    (defp Cons)
)

(about #_"arbace.arm.MapEntry"
    (defp MapEntry)
)

(about #_"arbace.arm.PersistentArrayMap"
    (defp MSeq)
    (defp PersistentArrayMap)
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
    (defp Var)

    (defn var? [v] (satisfies? Var v))
)

(about #_"array"
    (defn aget    [a i] (-/aget a i))
    (defn alength [a]   (-/alength a))

    (defn aclone [a]         (when (some? a) (-/aclone a)))
    (defn acopy! [a i b j n] (System'arraycopy b, j, a, i, n) a)
    (defn aset!  [a i x]     (-/aset a i x) a)
    (defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

    (defn anew [size-or-seq]
        (if (number? size-or-seq)
            (-/object-array (int size-or-seq))
            (let* [#_"seq" s (seq size-or-seq) #_"int" n (count s)]
                (loop* [#_"array" a (-/object-array n) #_"int" i 0 s s]
                    (when (and (< i n) (some? s)) => a
                        (recur (aset! a i (first s)) (inc i) (next s))
                    )
                )
            )
        )
    )
)

(about #_"append, str, pr, prn"
    (defn #_"Appendable" append-chr [#_"Appendable" a, #_"char" x]
        (-> a (Appendable'''append "\\") (Appendable'''append x))
    )

    (def #_"{char String}" char-escape-string
        (array-map
            \" "\\\""
            \\ "\\\\"
        )
    )

    (defn #_"Appendable" append-str [#_"Appendable" a, #_"String" x]
        (let* [
            a (Appendable'''append a, "\"")
            a (reduce (fn* [a1 a2] (Appendable'''append a1, (get char-escape-string a2 a2))) a x)
            a (Appendable'''append a, "\"")
        ]
            a
        )
    )

    (defp SeqForm)
    (defp VecForm)
    (defp MapForm)

    (defn #_"Appendable" append* [#_"Appendable" a, #_"String" b, #_"fn" f'append, #_"String" c, #_"String" d, #_"Seqable" q]
        (let* [a (Appendable'''append a, b) #_"seq" s (seq q)
              a (when (some? s) => a
                    (loop* [a a s s]
                        (let* [a (f'append a (first s)) s (next s)]
                            (when (some? s) => a
                                (recur (Appendable'''append a, c) s)
                            )
                        )
                    )
                )]
            (Appendable'''append a, d)
        )
    )

    (defn #_"Appendable" append-seq [#_"Appendable" a, #_"seq" x]    (append* a "(" append " " ")" x))
    (defn #_"Appendable" append-vec [#_"Appendable" a, #_"vector" x] (append* a "[" append " " "]" x))
    (defn #_"Appendable" append-map [#_"Appendable" a, #_"map" x]    (append* a "{" (fn* [a e] (-> a (append (key e)) (Appendable'''append " ") (append (val e)))) ", " "}" x))

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
                    (cond
                        (seq? x)     (append-seq a x)
                        (vector? x)  (append-vec a x)
                        (map? x)     (append-map a x)
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
            ((fn* [#_"StringBuilder" sb s]
                (when s => (StringBuilder''toString sb)
                    (recur (append! sb (first s)) (next s))
                )
             )
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
            (let* [x (first s) s (next s)]
                (when (some? s) => (pr x)
                    (recur x s)
                )
            )
        )
    )

    (defn print
        ([] nil)
        ([x] (append! -/*out* x) nil)
        ([x & s]
            (print x) (space)
            (let* [x (first s) s (next s)]
                (when (some? s) => (print x)
                    (recur x s)
                )
            )
        )
    )

    (defn prn     [& s] (apply pr    s) (newline) (flush) nil)
    (defn println [& s] (apply print s) (newline) (flush) nil)
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
        (loop* []
            (let* [#_"any" o (AtomicReference''get (:data this)) #_"any" o' (apply f o args)]
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
    ([f s]
        (let* [s (seq s)]
            (if (some? s)
                (reduce f (first s) (next s))
                (f)
            )
        )
    )
    ([f r s]
        (let* [s (seq s)]
            (if (some? s)
                (recur f (f r (first s)) (next s))
                r
            )
        )
    )
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

(defn =
    ([x] true)
    ([x y] (Util'equiv x y))
    ([x y & s]
        (and (= x y)
            (when (next s) => (= y (first s))
                (recur y (first s) (next s))
            )
        )
    )
)
)

(about #_"arbace.arm.Numbers"

(defn <
    ([x] true)
    ([x y] (-/< (int x) (int y)))
    ([x y & s]
        (and (< x y)
            (when (next s) => (< y (first s))
                (recur y (first s) (next s))
            )
        )
    )
)

(defn <=
    ([x] true)
    ([x y] (-/<= (int x) (int y)))
    ([x y & s]
        (and (<= x y)
            (when (next s) => (<= y (first s))
                (recur y (first s) (next s))
            )
        )
    )
)

(defn >
    ([x] true)
    ([x y] (-/> (int x) (int y)))
    ([x y & s]
        (and (> x y)
            (when (next s) => (> y (first s))
                (recur y (first s) (next s))
            )
        )
    )
)

(defn >=
    ([x] true)
    ([x y] (-/>= (int x) (int y)))
    ([x y & s]
        (and (>= x y)
            (when (next s) => (>= y (first s))
                (recur y (first s) (next s))
            )
        )
    )
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

(defn inc [x] (-/unchecked-inc-int (int x)))
(defn dec [x] (-/unchecked-dec-int (int x)))

(defn *
    ([] 1)
    ([x] #_"Number" x)
    ([x y] (-/unchecked-multiply-int (int x) (int y)))
    ([x y & s] (reduce * (* x y) s))
)

(defn quot [x y]
    (when (not (zero? y)) => (throw! "divide by zero")
        (-/unchecked-divide-int (int x) (int y))
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
            1 (let* [[a1] s]                             (IFn'''invoke f, a1))
            2 (let* [[a1 a2] s]                          (IFn'''invoke f, a1, a2))
            3 (let* [[a1 a2 a3] s]                       (IFn'''invoke f, a1, a2, a3))
            4 (let* [[a1 a2 a3 a4] s]                    (IFn'''invoke f, a1, a2, a3, a4))
            5 (let* [[a1 a2 a3 a4 a5] s]                 (IFn'''invoke f, a1, a2, a3, a4, a5))
            6 (let* [[a1 a2 a3 a4 a5 a6] s]              (IFn'''invoke f, a1, a2, a3, a4, a5, a6))
            7 (let* [[a1 a2 a3 a4 a5 a6 a7] s]           (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7))
            8 (let* [[a1 a2 a3 a4 a5 a6 a7 a8] s]        (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7, a8))
            9 (let* [[a1 a2 a3 a4 a5 a6 a7 a8 a9] s]     (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7, a8, a9))
              (let* [[a1 a2 a3 a4 a5 a6 a7 a8 a9 & s] s] (IFn'''invoke f, a1, a2, a3, a4, a5, a6, a7, a8, a9, s))
        )
    )
)
)

(about #_"arbace.arm.Symbol"

(about #_"Symbol"
    (defq Symbol [#_"String" name])

    (defn #_"Symbol" Symbol'new [#_"String" name]
        (new* Symbol'class (anew [name]))
    )

    (defn #_"boolean" Symbol''equals [#_"Symbol" this, #_"any" that]
        (or (identical? this that)
            (and (symbol? that) (= (:name this) (:name that)))
        )
    )

    (defn #_"Appendable" Symbol''append [#_"Symbol" this, #_"Appendable" a]
        (Appendable'''append a, (:name this))
    )

    (defn #_"any" Symbol''invoke
        ([#_"Symbol" this, #_"any" obj] (get obj this))
        ([#_"Symbol" this, #_"any" obj, #_"value" not-found] (get obj this not-found))
    )

    (defm Symbol IObject
        (IObject'''equals => Symbol''equals)
    )

    (defm Symbol IAppend
        (IAppend'''append => Symbol''append)
    )

    (defm Symbol IFn
        (IFn'''invoke => Symbol''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )
)

(defn symbol [name] (if (symbol? name) name (Symbol'new name)))
)

(about #_"arbace.arm.Keyword"

(about #_"Keyword"
    (defq Keyword [#_"Symbol" sym])

    (defn #_"Keyword" Keyword'new [#_"Symbol" sym]
        (new* Keyword'class (anew [sym]))
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
)

(defn keyword [name]
    (cond
        (keyword? name) name
        (symbol? name) (Keyword'new #_"Symbol" name)
        (string? name) (Keyword'new (symbol #_"String" name))
    )
)
)

(about #_"arbace.arm.Closure"

(about #_"Closure"
    (defq Closure [#_"FnExpr" fun, #_"map" env])

    (defn #_"Closure" Closure'new [#_"FnExpr" fun, #_"map" env]
        (new* Closure'class (anew [fun, env]))
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
        (let* [
            #_"FnMethod" fm
                (let* [#_"int" m (inc Compiler'MAX_POSITIONAL_ARITY) #_"int" n (min (count args m) m)]
                    (or (get (:regulars (:fun this)) n)
                        (let* [fm (:variadic (:fun this))]
                            (when (and (some? fm) (<= (dec (- (:arity fm))) n)) => (AFn'throwArity this, (if (< n m) n (- m)))
                                fm
                            )
                        )
                    )
                )
            #_"array" vars
                (let* [
                    #_"int" m (inc (reduce max (inc -1) (map :idx (vals (deref (:'locals fm))))))
                    #_"int" n (:arity fm) n (if (neg? n) (- n) (inc n))
                ]
                    (loop* [vars (-> (anew m) (aset! 0 this)) #_"int" i 1 #_"seq" s (seq args)]
                        (when (< i n) => (if (some? s) (aset! vars i s) vars)
                            (recur (aset! vars i (first s)) (inc i) (next s))
                        )
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
)
)

(about #_"arbace.arm.ASeq"

(about #_"ASeq"
    (defn #_"boolean" ASeq''equals [#_"ASeq" this, #_"any" that]
        (or (identical? this that)
            (and (sequential? that)
                (loop* [#_"seq" s (seq this) #_"seq" z (seq that)]
                    (when (some? s) => (nil? z)
                        (and (some? z) (= (first s) (first z)) (recur (next s) (next z)))
                    )
                )
            )
        )
    )
)
)

(about #_"arbace.arm.Cons"

(about #_"Cons"
    (defq Cons [#_"any" car, #_"seq" cdr] SeqForm)

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

    (defm Cons IObject
        (IObject'''equals => ASeq''equals)
    )
)

(defn cons [x s] (Cons'new x, (seq s)))
)

(about #_"arbace.arm.LazySeq"

(about #_"LazySeq"
    (defq LazySeq [#_"fn'" f, #_"any'" o, #_"seq'" s] SeqForm)

    (defn #_"LazySeq" LazySeq'new [#_"fn" f]
        (new* LazySeq'class (anew [(atom f), (atom nil), (atom nil)]))
    )

    (defn #_"cons" LazySeq''conj [#_"LazySeq" this, #_"any" o]
        (cons o this)
    )

    (defn #_"IPersistentCollection" LazySeq''empty [#_"LazySeq" this]
        (list)
    )

    (defn #_"seq" LazySeq''seq [#_"LazySeq" this]
        (locking this
            (let* [step- (fn* [this]
                        (let* [#_"fn" f (deref (:f this))]
                            (when (some? f)
                                (reset! (:f this) nil)
                                (reset! (:o this) (f))
                            )
                        )
                        (or (deref (:o this)) (deref (:s this)))
                    )]
                (step- this)
                (let* [#_"any" o (deref (:o this))]
                    (when (some? o)
                        (reset! (:o this) nil)
                        (reset! (:s this)
                            (loop* (satisfies? LazySeq o)
                                (when o => (seq o)
                                    (recur (step- o))
                                )
                            )
                        )
                    )
                )
                (deref (:s this))
            )
        )
    )

    (defn #_"any" LazySeq''first [#_"LazySeq" this]
        (let* [#_"seq" s (seq this)]
            (when (some? s)
                (first s)
            )
        )
    )

    (defn #_"seq" LazySeq''next [#_"LazySeq" this]
        (let* [#_"seq" s (seq this)]
            (when (some? s)
                (next s)
            )
        )
    )

    (defn #_"boolean" LazySeq''equals [#_"LazySeq" this, #_"any" that]
        (let* [#_"seq" s (seq this)]
            (if (some? s)
                (= s that)
                (and (sequential? that) (nil? (seq that)))
            )
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
)

(defn lazy-seq! [f] (LazySeq'new f))

(defn index-of [s x]
    (loop* [i 0 s (seq s)]
        (when (some? s) => -1
            (when (not (= (first s) x)) => i
                (recur (inc i) (next s))
            )
        )
    )
)

(defn map [f s]
    (lazy-seq!
        (fn* []
            (let* [s (seq s)]
                (when (some? s)
                    (cons (f (first s)) (map f (next s)))
                )
            )
        )
    )
)

(defn take [n s]
    (lazy-seq!
        (fn* []
            (when (pos? n)
                (let* [s (seq s)]
                    (when (some? s)
                        (cons (first s) (take (dec n) (next s)))
                    )
                )
            )
        )
    )
)

(defn drop [n s]
    (lazy-seq!
        (fn* []
            (let* [s (seq s)]
                (when (and (pos? n) s) => s
                    (recur (dec n) (next s))
                )
            )
        )
    )
)
)

(about #_"arbace.arm.APersistentVector"

(about #_"VSeq"
    (defq VSeq [#_"vector" v, #_"int" i] SeqForm)

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

    (defm VSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"RSeq"
    (defq RSeq [#_"vector" v, #_"int" i] SeqForm)

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

    (defm RSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)
)

(about #_"arbace.arm.MapEntry"

(about #_"MapEntry"
    (defq MapEntry [#_"key" k, #_"value" v] VecForm)

    (defn #_"MapEntry" MapEntry'new [#_"key" k, #_"value" v]
        (new* MapEntry'class (anew [k, v]))
    )

    (defn #_"any" MapEntry''nth
        ([#_"MapEntry" this, #_"int" i]
            (condp = i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) (throw! "index is out of bounds"))
        )
        ([#_"MapEntry" this, #_"int" i, #_"value" not-found]
            (condp = i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) not-found)
        )
    )

    (defn #_"int" MapEntry''count [#_"MapEntry" this]
        2
    )

    (defn #_"seq" MapEntry''seq [#_"MapEntry" this]
        (VSeq'new this, 0)
    )

    (defn #_"seq" MapEntry''rseq [#_"MapEntry" this]
        (RSeq'new this, 1)
    )

    (defn #_"boolean" MapEntry''equals [#_"MapEntry" this, #_"any" that]
        (or (identical? this that)
            (cond
                (vector? that)
                    (and (= (count that) 2) (= (nth that 0) (IMapEntry'''key this)) (= (nth that 1) (IMapEntry'''val this)))
                (sequential? that)
                    (loop* [#_"int" i 0 #_"seq" s (seq that)]
                        (when (< i 2) => (nil? s)
                            (when (and (some? s) (= (Indexed'''nth this, i) (first s))) => false
                                (recur (inc i) (next s))
                            )
                        )
                    )
                :else
                    false
            )
        )
    )

    (defm MapEntry IMapEntry
        (IMapEntry'''key => :k)
        (IMapEntry'''val => :v)
    )

    (defm MapEntry Sequential)

    (defm MapEntry Indexed
        (Indexed'''nth => MapEntry''nth)
    )

    (defm MapEntry Counted
        (Counted'''count => MapEntry''count)
    )

    (defm MapEntry Seqable
        (Seqable'''seq => MapEntry''seq)
    )

    (defm MapEntry Reversible
        (Reversible'''rseq => MapEntry''rseq)
    )

    (defm MapEntry IObject
        (IObject'''equals => MapEntry''equals)
    )
)
)

(about #_"arbace.arm.PersistentList"

(about #_"EmptyList"
    (defq EmptyList [] SeqForm)

    (defn #_"EmptyList" EmptyList'new []
        (new* EmptyList'class (anew []))
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

    (defm EmptyList IPersistentList Sequential)

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
)

(about #_"PersistentList"
    (defq PersistentList [#_"any" car, #_"IPersistentList" cdr, #_"int" cnt] SeqForm)

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

    (defm MSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"PersistentArrayMap"
    (defq PersistentArrayMap [#_"array" array] MapForm)

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
        (let* [#_"int" n
                (loop* [n 0 #_"int" i 0]
                    (when (< i (alength init)) => n
                        (let* [#_"boolean" dup?
                                (loop* [dup? false #_"int" j 0]
                                    (when (< j i) => dup?
                                        (or (= (aget init i) (aget init j))
                                            (recur dup? (+ j 2))
                                        )
                                    )
                                )
                        ]
                            (recur (if dup? n (+ n 2)) (+ i 2))
                        )
                    )
                )
              init
                (when (< n (alength init)) => init
                    (let* [#_"array" nodups (anew n)
                          #_"int" m
                            (loop* [m 0 #_"int" i 0]
                                (when (< i (alength init)) => m
                                    (let* [#_"boolean" dup?
                                            (loop* [dup? false #_"int" j 0]
                                                (when (< j m) => dup?
                                                    (or (= (aget init i) (aget nodups j))
                                                        (recur dup? (+ j 2))
                                                    )
                                                )
                                            )
                                        m (when (not dup?) => m
                                                (let* [#_"int" j
                                                        (loop* [j (- (alength init) 2)]
                                                            (when (<= i j) => j
                                                                (if (= (aget init i) (aget init j))
                                                                    j
                                                                    (recur (- j 2))
                                                                )
                                                            )
                                                        )
                                                ]
                                                    (aset! nodups m (aget init i))
                                                    (aset! nodups (inc m) (aget init (inc j)))
                                                    (+ m 2)
                                                )
                                            )]
                                        (recur m (+ i 2))
                                    )
                                )
                            )
                    ]
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
        (loop* [#_"int" i 0]
            (when (< i (alength a)) => -1
                (if (= (aget a i) key) i (recur (+ i 2)))
            )
        )
    )

    (defn #_"value" PersistentArrayMap''valAt
        ([#_"PersistentArrayMap" this, #_"key" key] (PersistentArrayMap''valAt this, key, nil))
        ([#_"PersistentArrayMap" this, #_"key" key, #_"value" not-found]
            (let* [
                #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
            ]
                (if (< -1 i) (aget a (inc i)) not-found)
            )
        )
    )

    (defn #_"IPersistentMap" PersistentArrayMap''assoc [#_"PersistentArrayMap" this, #_"key" key, #_"value" val]
        (let* [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (if (< -1 i)
                (if (= (aget a (inc i)) val)
                    this
                    (PersistentArrayMap''create this, (-> (aclone a) (aset! (inc i) val)))
                )
                (let* [
                    #_"int" n (alength a)
                    #_"array" a' (anew (+ n 2))
                    a' (if (pos? n) (acopy! a' 0 a 0 n) a')
                ]
                    (PersistentArrayMap''create this, (-> a' (aset! n key) (aset! (inc n) val)))
                )
            )
        )
    )

    (defn #_"boolean" PersistentArrayMap''containsKey [#_"PersistentArrayMap" this, #_"key" key]
        (< -1 (PersistentArrayMap'index-of (:array this), key))
    )

    (defn #_"pair" PersistentArrayMap''entryAt [#_"PersistentArrayMap" this, #_"key" key]
        (let* [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (when (< -1 i)
                (MapEntry'new (aget a i), (aget a (inc i)))
            )
        )
    )

    (defn #_"IPersistentMap" PersistentArrayMap''dissoc [#_"PersistentArrayMap" this, #_"key" key]
        (let* [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (when (< -1 i) => this
                (let* [#_"int" n (- (alength a) 2)]
                    (when (pos? n) => PersistentArrayMap'EMPTY
                        (let* [
                            #_"array" a' (-> (anew n) (acopy! 0 a 0 i) (acopy! i a (+ i 2) (- n i)))
                        ]
                            (PersistentArrayMap''create this, a')
                        )
                    )
                )
            )
        )
    )

    (defn #_"IPersistentCollection" PersistentArrayMap''conj [#_"PersistentArrayMap" this, #_"any" o]
        (condp satisfies? o
            IMapEntry
                (assoc this (key o) (val o))
            IPersistentVector
                (when (= (count o) 2) => (throw! "vector arg to map conj must be a pair")
                    (assoc this (nth o 0) (nth o 1))
                )
            #_else
                (loop* [this this #_"seq" s (seq o)]
                    (when (some? s) => this
                        (let* [#_"pair" e (first s)]
                            (recur (assoc this (key e) (val e)) (next s))
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

    (defn #_"boolean" PersistentArrayMap''equals [#_"PersistentArrayMap" this, #_"any" that]
        (or (identical? this that)
            (and (map? that) (= (count that) (count this))
                (loop* [#_"seq" s (seq this)]
                    (when (some? s) => true
                        (let* [#_"pair" e (first s) #_"any" k (key e)]
                            (and (contains? that k) (= (val e) (get that k))
                                (recur (next s))
                            )
                        )
                    )
                )
            )
        )
    )

    (defm PersistentArrayMap Counted
        (Counted'''count => PersistentArrayMap''count)
    )

    (defm PersistentArrayMap ILookup
        (ILookup'''valAt => PersistentArrayMap''valAt)
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
        (IPersistentCollection'''conj => PersistentArrayMap''conj)
        (IPersistentCollection'''empty => PersistentArrayMap''empty)
    )

    (defm PersistentArrayMap Seqable
        (Seqable'''seq => PersistentArrayMap''seq)
    )

    (defm PersistentArrayMap IObject
        (IObject'''equals => PersistentArrayMap''equals)
    )
)

(defn array-map
    ([] PersistentArrayMap'EMPTY)
    ([& keyvals] (PersistentArrayMap'createAsIfByAssoc (anew keyvals)))
)

(defn memoize [f]
    (let* [mem (atom (array-map))]
        (fn* [& args]
            (let* [e (find (deref mem) args)]
                (if (some? e)
                    (val e)
                    (let* [r (apply f args)]
                        (swap! mem assoc args r)
                        r
                    )
                )
            )
        )
    )
)
)

(about #_"arbace.arm.PersistentVector"

(about #_"VNode"
    (defq VNode [#_"array" array, #_"index" index])

    (defn #_"node" VNode'new [#_"array" array, #_"index" index]
        (new* VNode'class (anew [(or array (anew 32)), index]))
    )

    (def #_"node" VNode'EMPTY (VNode'new nil, nil))

    (defn #_"value" VNode''value-for [#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
        (when (< -1 i cnt) => (throw! "index is out of bounds")
            (when (< i tail-off) => (aget tail (- i tail-off))
                (loop* [i i #_"node" node this shift shift]
                    (when (pos? shift) => (aget (:array node) (bit-and (unsigned-bit-shift-right i shift) 0x1f))
                        (let* [
                            #_"index" x (:index node)
                            #_"int" m (bit-and (unsigned-bit-shift-right i shift) 0x1f)
                            [m i]
                                (when (some? x) => [m i]
                                    (let* [m
                                            (loop* (<= (aget x m) i)
                                                (when m => m
                                                    (recur (inc m))
                                                )
                                            )
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

    (defn #_"node" VNode''new-path [#_"node" this, #_"int" shift]
        (when (pos? shift) => this
            (VNode'new (-> (anew 32) (aset! 0 (VNode''new-path this, (- shift 5)))), nil)
        )
    )

    (defn #_"int" VNode'last-range [#_"index" x]
        (aget x (dec (aget x 32)))
    )

    (defn #_"boolean" VNode''overflow? [#_"node" this, #_"int" shift, #_"int" cnt]
        (let* [
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
        (let* [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (if (some? x)
                (let* [
                    #_"int" e (dec (aget x 32))
                    #_"node" child
                        (when (< 5 shift)
                            (let* [
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
                            (let* [
                                a (aset! a (inc e) (VNode''new-path tail-node, (- shift 5)))
                                x (aset! x (inc e) (+ (aget x e) 32))
                            ]
                                [a (aswap! x 32 inc)]
                            )
                        )
                ]
                    (VNode'new a, x)
                )
                (let* [
                    #_"int" e (bit-and (unsigned-bit-shift-right (dec cnt) shift) 0x1f)
                    #_"node" child
                        (when (< 5 shift) => tail-node
                            (let* [child (aget a e)]
                                (if (some? child)
                                    (VNode''push-tail child, (- shift 5), cnt, tail-node)
                                    (VNode''new-path tail-node, (- shift 5))
                                )
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
        (let* [
            #_"array" a (:array this) #_"index" x (:index this)
            #_"int" e (bit-and (unsigned-bit-shift-right (dec tail-off) shift) 0x1f)
        ]
            (if (some? x)
                (let* [e
                        (loop* (and (< e 31) (some? (aget x (inc e))))
                            (when e => e
                                (recur (inc e))
                            )
                        )
                ]
                    (cond
                        (< 5 shift)
                            (let* [
                                #_"node" child (aget a e)
                                #_"node" child' (VNode''pop-tail child, (- shift 5), (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0)))
                            ]
                                (when (or (some? child') (pos? e))
                                    (let* [
                                        a (aclone a)
                                        a (-> a (aset! e child'))
                                        x (aclone x)
                                        x
                                            (if (some? child')
                                                (let* [
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
                            (let* [
                                a (-> (aclone a) (aset! e nil))
                                x (-> (aclone x) (aset! e nil) (aswap! 32 dec))
                            ]
                                (VNode'new a, x)
                            )
                    )
                )
                (cond
                    (< 5 shift)
                        (let* [
                            #_"node" child (VNode''pop-tail (aget a e), (- shift 5), tail-off)
                        ]
                            (when (or (some? child) (pos? e))
                                (let* [
                                    a (aclone a)
                                    a (aset! a e child)
                                ]
                                    (VNode'new a, nil)
                                )
                            )
                        )
                    (pos? e)
                        (let* [
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
        (let* [
            #_"array" a (:array this) #_"index" x (:index this)
            a (aclone a)
            #_"int" m (bit-and (unsigned-bit-shift-right i shift) 0x1f)
            a
                (when (pos? shift) => (aset! a m val)
                    (let* [
                        [m i]
                            (when (some? x) => [m i]
                                (let* [m
                                        (loop* (<= (aget x m) i)
                                            (when m => m
                                                (recur (inc m))
                                            )
                                        )
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

    (defn #_"PersistentVector" PersistentVector'new [#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
        (new* PersistentVector'class (anew [cnt, shift, root, tail]))
    )

    (def #_"PersistentVector" PersistentVector'EMPTY (PersistentVector'new 0, 5, VNode'EMPTY, (anew 0)))

    (defn #_"PersistentVector" PersistentVector'create [& values]
        (let* [#_"seq" s (seq values)]
            (when (some? s) => PersistentVector'EMPTY
                (let* [
                    #_"values" tail (anew (take 32 s)) #_"int" n (alength tail)
                    #_"PersistentVector" w (PersistentVector'new n, 5, VNode'EMPTY, tail)
                ]
                    (let* [s (seq (drop 32 s))]
                        (when (some? s) => w
                            (into w s)
                        )
                    )
                )
            )
        )
    )

    (defn #_"boolean" PersistentVector''equals [#_"PersistentVector" this, #_"any" that]
        (or (identical? this that)
            (cond
                (vector? that)
                    (when (= (:cnt this) (:cnt that)) => false
                        (loop* [#_"int" i 0]
                            (when (< i (:cnt this)) => true
                                (when (= (Indexed'''nth this, i) (Indexed'''nth that, i)) => false
                                    (recur (inc i))
                                )
                            )
                        )
                    )
                (sequential? that)
                    (loop* [#_"int" i 0 #_"seq" s (seq that)]
                        (when (< i (:cnt this)) => (nil? s)
                            (when (and (some? s) (= (Indexed'''nth this, i) (first s))) => false
                                (recur (inc i) (next s))
                            )
                        )
                    )
                :else
                    false
            )
        )
    )

    (defn #_"int" PersistentVector''tail-off [#_"PersistentVector" this]
        (- (:cnt this) (alength (:tail this)))
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
        (let* [
            #_"int" tail-len (alength (:tail this))
        ]
            (if (< tail-len 32)
                (let* [
                    #_"values" tail (-> (anew (inc tail-len)) (acopy! 0 (:tail this) 0 tail-len) (aset! tail-len val))
                ]
                    (PersistentVector'new (inc (:cnt this)), (:shift this), (:root this), tail)
                )
                (let* [
                    #_"node" tail-node (VNode'new (:tail this), nil)
                    #_"int" shift (:shift this)
                    [#_"node" root shift]
                        (if (VNode''overflow? (:root this), shift, (:cnt this))
                            (let* [
                                #_"array" a
                                    (-> (anew 32)
                                        (aset! 0 (:root this))
                                        (aset! 1 (VNode''new-path tail-node, shift))
                                    )
                                #_"index" x
                                    (when (some? (:index (:root this)))
                                        (let* [
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
            (let* [
                #_"int" tail-off (PersistentVector''tail-off this)
            ]
                (if (<= tail-off i)
                    (let* [
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
            (let* [#_"int" i (int key)]
                (when (< -1 i (:cnt this))
                    (MapEntry'new key, (Indexed'''nth this, i))
                )
            )
        )
    )

    (defn #_"value" PersistentVector''valAt
        ([#_"PersistentVector" this, #_"key" key] (PersistentVector''valAt this, key, nil))
        ([#_"PersistentVector" this, #_"key" key, #_"value" not-found]
            (when (integer? key) => not-found
                (let* [#_"int" i (int key)]
                    (when (< -1 i (:cnt this)) => not-found
                        (PersistentVector''value-for this, i)
                    )
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

    (defm PersistentVector IObject
        (IObject'''equals => PersistentVector''equals)
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

(defn get
    ([coll key]
        (when (some? coll)
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key)
                (and (number? key) (or (string? coll) (array? coll)))
                    (let* [#_"int" n (int key)]
                        (when (< -1 n (count coll))
                            (nth coll n)
                        )
                    )
            )
        )
    )
    ([coll key not-found]
        (when (some? coll) => not-found
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key, not-found)
                (and (number? key) (or (string? coll) (array? coll)))
                    (let* [#_"int" n (int key)]
                        (when (< -1 n (count coll)) => not-found
                            (nth coll n)
                        )
                    )
                :else
                    not-found
            )
        )
    )
)

(defn contains? [coll key]
    (when (some? coll) => false
        (cond
            (associative? coll)
                (Associative'''containsKey coll, key)
            (and (number? key) (or (string? coll) (array? coll)))
                (let* [#_"int" n (int key)]
                    (< -1 n (count coll))
                )
            :else
                (throw! (str "contains? not supported on " coll))
        )
    )
)

(defn find [coll key]
    (when (some? coll)
        (cond
            (associative? coll)
                (Associative'''entryAt coll, key)
            :else
                (throw! (str "find not supported on " coll))
        )
    )
)

(defn nth
    ([coll n]
        (when (some? coll)
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n)
                (string? coll)
                    (Character'valueOf (String''charAt coll, n))
                (array? coll)
                    (Array'get coll, n)
                (map-entry? coll)
                    (let* [#_"pair" e coll]
                        (condp = n 0 (key e) 1 (val e) (throw! "index is out of bounds"))
                    )
                (sequential? coll)
                    (loop* [#_"int" i 0 #_"seq" s (seq coll)]
                        (when (and (<= i n) (some? s)) => (throw! "index is out of bounds")
                            (when (< i n) => (first s)
                                (recur (inc i) (next s))
                            )
                        )
                    )
                :else
                    (throw! (str "nth not supported on " coll))
            )
        )
    )
    ([coll n not-found]
        (when (some? coll) => not-found
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n, not-found)
                (neg? n)
                    not-found
                (string? coll)
                    (let* [#_"String" s coll]
                        (when (< n (String''length s)) => not-found
                            (Character'valueOf (String''charAt s, n))
                        )
                    )
                (array? coll)
                    (when (< n (Array'getLength coll)) => not-found
                        (Array'get coll, n)
                    )
                (map-entry? coll)
                    (let* [#_"pair" e coll]
                        (condp = n 0 (key e) 1 (val e) not-found)
                    )
                (sequential? coll)
                    (loop* [#_"int" i 0 #_"seq" s (seq coll)]
                        (when (and (<= i n) (some? s)) => not-found
                            (when (< i n) => (first s)
                                (recur (inc i) (next s))
                            )
                        )
                    )
                :else
                    (throw! (str "nth not supported on " coll))
            )
        )
    )
)

    (defn #_"IPersistentMap" RT'mapUniqueKeys [#_"Seqable" init]
        (if (empty? init) PersistentArrayMap'EMPTY (PersistentArrayMap'new (anew init)))
    )
)

(about #_"arbace.arm.Var"

(about #_"Var"
    (defq Var [#_"Symbol" sym, #_"any'" root])

    (defn #_"Var" Var'new
        ([#_"Symbol" sym] (Var'new sym, nil))
        ([#_"Symbol" sym, #_"any" root]
            (new* Var'class (anew [sym, (atom root)]))
        )
    )

    (defn #_"Appendable" Var''append [#_"Var" this, #_"Appendable" a]
        (-> a (Appendable'''append "#'") (append (:sym this)))
    )

    (defn #_"any" Var''get [#_"Var" this]
        (deref (:root this))
    )

    (defn #_"void" Var''bindRoot [#_"Var" this, #_"any" root]
        (reset! (:root this) root)
        nil
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
)

(about #_"arbace.arm.Namespace"

(about #_"Namespace"
    (def #_"{Symbol Var}'" Namespace'mappings (atom (array-map)))

    (defn #_"Var" Namespace'getMapping [#_"Symbol" name]
        (get (deref Namespace'mappings) name)
    )

    (defn #_"Var" Namespace'intern [#_"Symbol" sym]
        (or (get (deref Namespace'mappings) sym)
            (let* [#_"Var" v (Var'new sym)]
                (swap! Namespace'mappings assoc sym v)
                v
            )
        )
    )
)
)

(about #_"arbace.arm.LispReader"

(about #_"LispReader"
    (defn #_"boolean" LispReader'isMacro [#_"char" ch]
        (contains? LispReader'macros ch)
    )

    (defn #_"boolean" LispReader'isTerminatingMacro [#_"char" ch]
        (and (LispReader'isMacro ch) (not (or (= ch (char! "#")) (= ch (char! "'")) (= ch (char! "%")))))
    )

    (defn #_"boolean" LispReader'isDigit [#_"char" ch]
        (Character'isDigit ch)
    )

    (defn #_"boolean" LispReader'isWhitespace [#_"char" ch]
        (or (Character'isWhitespace ch) (= ch (char! ",")))
    )

    (defn #_"Character" LispReader'read1 [#_"Reader" r]
        (let* [#_"int" c (Reader''read r)]
            (when (not (= c -1))
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

    (def #_"Pattern" LispReader'rxInteger #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+))")

    (defn #_"any" LispReader'matchNumber [#_"String" s]
        (let* [#_"Matcher" m (Pattern''matcher LispReader'rxInteger, s)]
            (when (Matcher''matches m)
                (when (nil? (Matcher''group m, 2)) => 0
                    (let* [[#_"String" n #_"int" radix]
                            (let* [n (Matcher''group m, 3)]
                                (if (some? n)
                                    [n 10]
                                    (let* [n (Matcher''group m, 4)]
                                        (if (some? n)
                                            [n 16]
                                            (let* [n (Matcher''group m, 5)]
                                                (when (some? n)
                                                    [n 8]
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                    ]
                        (when (some? n)
                            (let* [#_"BigInteger" bn (BigInteger'new n, radix)]
                                (BigInteger''intValue (if (= (Matcher''group m, 1) "-") (BigInteger''negate bn) bn))
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"any" LispReader'readNumber [#_"Reader" r, #_"char" ch]
        (let* [#_"String" s
                (let* [#_"StringBuilder" sb (StringBuilder'new) _ (StringBuilder''append sb, ch)]
                    (loop* []
                        (let* [ch (LispReader'read1 r)]
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
        (let* [#_"StringBuilder" sb (StringBuilder'new) _ (StringBuilder''append sb, ch)]
            (loop* []
                (let* [ch (LispReader'read1 r)]
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

    (defn #_"any" LispReader'matchSymbol [#_"String" s]
        (let* [#_"boolean" kw? (String''startsWith s, ":") s (if kw? (String''substring s, 1) s)]
            (when (and (pos? (String''length s)) (not (LispReader'isDigit (String''charAt s, 0))))
                (if kw? (keyword s) (symbol s))
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
            (loop* []
                (let* [#_"char" ch
                        (loop* [ch (LispReader'read1 r)]
                            (when (and (some? ch) (LispReader'isWhitespace ch)) => ch
                                (recur (LispReader'read1 r))
                            )
                        )
                ]
                    (cond
                        (nil? ch)
                            (if eofIsError (throw! "EOF while reading") eofValue)
                        (and (some? returnOn) (= returnOn ch))
                            returnOnValue
                        (LispReader'isDigit ch)
                            (LispReader'readNumber r, ch)
                        :else
                            (let* [#_"fn" f'macro (get LispReader'macros ch)]
                                (if (some? f'macro)
                                    (let* [#_"any" o (f'macro r scope ch)]
                                        (when (identical? o r) => o
                                            (recur)
                                        )
                                    )
                                    (or
                                        (when (or (= ch (char! "+")) (= ch (char! "-")))
                                            (let* [#_"char" ch' (LispReader'read1 r) _ (LispReader'unread r, ch')]
                                                (when (and (some? ch') (LispReader'isDigit ch'))
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
        (loop* [#_"vector" v (vector)]
            (let* [#_"any" form (LispReader'read r, scope, false, LispReader'READ_EOF, delim, LispReader'READ_FINISHED)]
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

    (defn #_"char" StringReader'escape [#_"Reader" r]
        (let* [#_"char" ch (LispReader'read1 r)]
            (when (some? ch) => (throw! "EOF while reading string")
                (condp = ch
                    (char! "\\") ch
                    (char! "\"") ch
                    (throw! (str "unsupported escape character: \\" ch))
                )
            )
        )
    )

    (defn #_"any" string-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (let* [#_"StringBuilder" sb (StringBuilder'new)]
            (loop* []
                (let* [#_"char" ch (LispReader'read1 r)]
                    (when (some? ch) => (throw! "EOF while reading string")
                        (when (not (= ch (char! "\"")))
                            (StringBuilder''append sb, (if (= ch (char! "\\")) (StringReader'escape r) ch))
                            (recur)
                        )
                    )
                )
            )
            (StringBuilder''toString sb)
        )
    )

    (defn #_"any" discard-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (LispReader'read r, scope)
        r
    )

    (defn #_"any" quote-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (list 'quote (LispReader'read r, scope))
    )

    (defn #_"any" dispatch-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (let* [#_"char" ch (LispReader'read1 r)]
            (when (some? ch) => (throw! "EOF while reading character")
                (let* [#_"fn" f'macro (get LispReader'dispatchMacros ch)]
                    (when (nil? f'macro) => (f'macro r scope ch)
                        (LispReader'unread r, ch)
                        (throw! (str "no dispatch macro for: " ch))
                    )
                )
            )
        )
    )

    (defn #_"any" list-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (apply list (LispReader'readDelimitedForms r, scope, (char! ")")))
    )

    (defn #_"any" vector-reader [#_"Reader" r, #_"map" scope, #_"char" _delim]
        (vec (LispReader'readDelimitedForms r, scope, (char! "]")))
    )

    (defn #_"any" unmatched-delimiter-reader [#_"Reader" _r, #_"map" scope, #_"char" delim]
        (throw! (str "unmatched delimiter: " delim))
    )

    (def #_"{char fn}" LispReader'macros
        (array-map
            (char! "\"")  string-reader
            (char! "'")  quote-reader
            (char! "(")  list-reader,    (char! ")")  unmatched-delimiter-reader
            (char! "[")  vector-reader,  (char! "]")  unmatched-delimiter-reader
            (char! "#")  dispatch-reader
        )
    )

    (def #_"{char fn}" LispReader'dispatchMacros
        (array-map
            (char! "_")  discard-reader
        )
    )
)

(defn read
    ([] (read -/*in*))
    ([s] (read s true nil))
    ([s eof-error? eof-value] (LispReader'read s, nil, (boolean eof-error?), eof-value))
)
)

(about #_"arbace.arm.Compiler"
    (defp Expr
        (#_"gen" Expr'''emit [#_"Expr" this, #_"Context" context, #_"map" scope, #_"gen" gen])
    )

    (defp LiteralExpr)
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
    (defp LetExpr)
    (defp RecurExpr)
    (defp ThrowExpr)

(about #_"Compiler"
    (def #_"int" Compiler'MAX_POSITIONAL_ARITY #_9 (+ 9 2))

    (defn #_"Var" Compiler'lookupVar [#_"Symbol" sym]
        (or (Namespace'getMapping sym) (Namespace'intern (symbol (:name sym))))
    )

    (defn #_"Var" Compiler'resolve [#_"Symbol" sym]
        (or (Namespace'getMapping sym) (throw! (str "unable to resolve symbol: " sym " in this context")))
    )

    (defn #_"gen" Compiler'emitArgs [#_"map" scope, #_"gen" gen, #_"indexed" args]
        (let* [
            gen (Gen''push gen, (count args))
            gen (Gen''anew gen)
        ]
            (loop* [gen gen #_"int" i 0]
                (when (< i (count args)) => gen
                    (let* [
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
    )

    (defn #_"gen" Compiler'emitLocals [#_"map" scope, #_"gen" gen, #_"map" locals]
        (let* [
            gen (Gen''push gen, (bit-shift-left (count locals) 1))
            gen (Gen''anew gen)
        ]
            (loop* [gen gen #_"int" i 0 #_"seq" s (vals locals)]
                (when (some? s) => gen
                    (let* [
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
)

(about #_"LiteralExpr"
    (defr LiteralExpr)

    (defn #_"LiteralExpr" LiteralExpr'new [#_"any" value]
        (new* LiteralExpr'class
            (array-map
                #_"any" :value value
            )
        )
    )

    (def #_"LiteralExpr" LiteralExpr'NIL   (LiteralExpr'new nil))
    (def #_"LiteralExpr" LiteralExpr'TRUE  (LiteralExpr'new true))
    (def #_"LiteralExpr" LiteralExpr'FALSE (LiteralExpr'new false))

    (defn #_"Expr" LiteralExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [#_"int" n (dec (count form))]
            (when (= n 1) => (throw! (str "wrong number of arguments passed to quote: " n))
                (let* [#_"any" value (second form)]
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
        (when (not (= context :Context'STATEMENT)) => gen
            (Gen''push gen, (:value this))
        )
    )

    (defm LiteralExpr Expr
        (Expr'''emit => LiteralExpr''emit)
    )
)

(about #_"VarExpr"
    (defr VarExpr)

    (defn #_"VarExpr" VarExpr'new [#_"Var" var]
        (new* VarExpr'class
            (array-map
                #_"Var" :var var
            )
        )
    )

    (defn #_"gen" VarExpr''emit [#_"VarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let* [
            gen (Gen''push gen, (:var this))
            gen (Gen''invoke gen, Var''get, 1)
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
            (array-map
                #_"vector" :exprs exprs
            )
        )
    )

    (defn #_"Expr" BodyExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [#_"seq" s form s (if (= (first s) 'do) (next s) s)
              #_"vector" v
                (loop* [v (vector) s s]
                    (when (some? s) => v
                        (let* [#_"Context" c (if (or (= context :Context'STATEMENT) (some? (next s))) :Context'STATEMENT context)]
                            (recur (conj v (Compiler'analyze (first s), c, scope)) (next s))
                        )
                    )
                )
        ]
            (BodyExpr'new (if (pos? (count v)) v (conj v LiteralExpr'NIL)))
        )
    )

    (defn #_"gen" BodyExpr''emit [#_"BodyExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (loop* [gen gen #_"seq" s (seq (:exprs this))]
            (when (some? (next s)) => (Expr'''emit (first s), context, scope, gen)
                (recur (Expr'''emit (first s), :Context'STATEMENT, scope, gen) (next s))
            )
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
            (array-map
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
        (let* [#_"Expr" test (Compiler'analyze (second form), scope)
              #_"Expr" then (Compiler'analyze (third form), context, scope)
              #_"Expr" else (Compiler'analyze (fourth form), context, scope)]
            (IfExpr'new test, then, else)
        )
    )

    (defn #_"gen" IfExpr''emit [#_"IfExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let* [
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
            (array-map
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" VectorExpr'parse [#_"vector" form, #_"map" scope]
        (let* [[#_"vector" args #_"boolean" literal?]
                (loop* [args (vector) literal? true #_"seq" s (seq form)]
                    (when (some? s) => [args literal?]
                        (let* [#_"Expr" e (Compiler'analyze (first s), scope)]
                            (recur (conj args e) (and literal? (satisfies? LiteralExpr e)) (next s))
                        )
                    )
                )
        ]
            (when literal? => (VectorExpr'new args)
                (LiteralExpr'new (vec (map :value args)))
            )
        )
    )

    (defn #_"gen" VectorExpr''emit [#_"VectorExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let* [
            gen
                (when (seq (:args this)) => (Gen''push gen, PersistentVector'EMPTY)
                    (let* [gen (Compiler'emitArgs scope, gen, (:args this))]
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
            (array-map
                #_"Expr" :fexpr fexpr
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" InvokeExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [#_"Expr" fexpr (Compiler'analyze (first form), scope)
              #_"vector" args (vec (map (fn* [a] (Compiler'analyze a, scope)) (next form)))]
            (InvokeExpr'new fexpr, args)
        )
    )

    (defn #_"gen" InvokeExpr''emit [#_"InvokeExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let* [
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
            (array-map
                #_"int" :uid (next-id!)
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
            (array-map
                #_"LocalBinding" :lb lb
            )
        )
    )

    (defn #_"gen" LocalBindingExpr''emit [#_"LocalBindingExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when (not (= context :Context'STATEMENT)) => gen
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
            (array-map
                #_"FnExpr" :fun fun
                #_"FnMethod" :parent parent
                #_"{int LocalBinding}'" :'locals (atom (array-map))
                #_"Number" :arity nil
                #_"Expr" :body nil
            )
        )
    )

    (defn #_"FnMethod" FnMethod'parse [#_"FnExpr" fun, #_"seq" form, #_"map" scope]
        (let* [
            scope
                (-> scope
                    (update :fm (fn* [x] (FnMethod'new fun, x)))
                    (update :'local-env (fn* ([x] (atom (deref x)))))
                    (assoc :'local-num (atom 0))
                )
            _
                (let* [#_"Symbol" f (:fname fun)]
                    (when (some? f)
                        (let* [#_"LocalBinding" lb (LocalBinding'new f, nil, (deref (get scope :'local-num)))]
                            (swap! (get scope :'local-env) assoc (:sym lb) lb)
                            (swap! (:'locals (get scope :fm)) assoc (:uid lb) lb)
                        )
                    )
                )
            [#_"[LocalBinding]" lbs #_"int" arity]
                (loop* [lbs (vector) arity 0 #_"boolean" variadic? false #_"seq" s (seq (first form))]
                    (when (some? s) => (if (and variadic? (not (neg? arity))) (throw! "missing variadic parameter") [lbs arity])
                        (let* [#_"symbol?" sym (first s)]
                            (when (symbol? sym)        => (throw! "function parameters must be symbols")
                                (cond
                                    (= sym '&)
                                        (when (not variadic?) => (throw! "overkill variadic parameter list")
                                            (recur lbs arity true (next s))
                                        )
                                    (neg? arity)
                                        (throw! (str "excess variadic parameter: " sym))
                                    ((if variadic? <= <) arity Compiler'MAX_POSITIONAL_ARITY)
                                        (let* [
                                            arity (if (not variadic?) (inc arity) (- (inc arity)))
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
            (let* [
                gen (Gen''load gen, 0)
                gen (Gen''get gen, (:sym lb))
            ]
                gen
            )
            (Gen''load gen, (:idx lb))
        )
    )

    (defn #_"gen" FnMethod''compile [#_"FnMethod" this]
        (let* [
            #_"map" scope (array-map :fm this)
            #_"gen" gen (Gen'new)
            scope (assoc scope :loop-label (Gen''mark gen))
            gen (Expr'''emit (:body this), :Context'RETURN, scope, gen)
        ]
            (Gen''return gen)
        )
    )

    (def compile-and-memoize (memoize FnMethod''compile))
)

(about #_"FnExpr"
    (defr FnExpr)

    (defn #_"FnExpr" FnExpr'new []
        (new* FnExpr'class
            (array-map
                #_"Symbol" :fname nil
                #_"{int FnMethod}" :regulars nil
                #_"FnMethod" :variadic nil
                #_"{int LocalBinding}'" :'closes (atom (array-map))
            )
        )
    )

    (defn #_"Expr" FnExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [
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
                (loop* [regulars (array-map) variadic nil #_"seq" s (next form)]
                    (when (some? s) => [regulars variadic]
                        (let* [#_"FnMethod" fm (FnMethod'parse fun, (first s), scope) #_"int" n (:arity fm)]
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
                )
        ]
            (when (some? variadic)
                (loop* [#_"int" n (- (:arity variadic))]
                    (when (<= n Compiler'MAX_POSITIONAL_ARITY)
                        (when (some? (get regulars n))
                            (throw! "can't have fixed arity function with more params than variadic function")
                        )
                        (recur (inc n))
                    )
                )
            )
            (assoc fun :regulars regulars, :variadic variadic)
        )
    )

    (defn #_"gen" FnExpr''emit [#_"FnExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when (not (= context :Context'STATEMENT)) => gen
            (let* [
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
            (array-map
                #_"Var" :var var
                #_"Expr" :init init
                #_"boolean" :initProvided initProvided
            )
        )
    )

    (defn #_"Expr" DefExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [#_"int" n (count form)]
            (cond
                (< 3 n) (throw! "too many arguments to def")
                (< n 2) (throw! "too few arguments to def")
                :else
                    (let* [#_"symbol?" s (second form)]
                        (when (symbol? s) => (throw! "first argument to def must be a symbol")
                            (DefExpr'new (Compiler'lookupVar s), (Compiler'analyze (third form), scope), (= n 3))
                        )
                    )
            )
        )
    )

    (defn #_"gen" DefExpr''emit [#_"DefExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let* [
            gen (Gen''push gen, (:var this))
            gen
                (when (:initProvided this) => gen
                    (let* [
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

(about #_"LetExpr"
    (defr LetExpr)

    (defn #_"LetExpr" LetExpr'new [#_"[LocalBinding]" bindings, #_"Expr" body, #_"boolean" loop?]
        (new* LetExpr'class
            (array-map
                #_"[LocalBinding]" :bindings bindings
                #_"Expr" :body body
                #_"boolean" :loop? loop?
            )
        )
    )

    (defn #_"Expr" LetExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [#_"vector?" bindings (second form)]
            (when (vector? bindings)           => (throw! "bad binding form, expected vector")
                (when (even? (count bindings)) => (throw! "bad binding form, expected matched symbol expression pairs")
                    (let* [
                        scope (update scope :'local-env (fn* ([x] (atom (deref x)))))
                        scope (update scope :'local-num (fn* ([x] (atom (deref x)))))
                        #_"boolean" loop? (= (first form) 'loop*)
                        scope
                            (when loop? => scope
                                (dissoc scope :loop-locals)
                            )
                        #_"[LocalBinding]" lbs
                            (loop* [lbs (vector) #_"seq" s (seq bindings)]
                                (when (some? s) => lbs
                                    (let* [#_"symbol?" sym (first s)]
                                        (when (symbol? sym) => (throw! (str "bad binding form, expected symbol, got: " sym))
                                            (let* [
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
        (let* [
            gen
                (loop* [gen gen #_"seq" s (seq (:bindings this))]
                    (when (some? s) => gen
                        (let* [
                            #_"LocalBinding" lb (first s)
                            gen (Expr'''emit (deref (:'init lb)), :Context'EXPRESSION, scope, gen)
                            gen (Gen''store gen, (:idx lb))
                        ]
                            (recur gen (next s))
                        )
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
            (array-map
                #_"vector" :loopLocals loopLocals
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" RecurExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (when (and (= context :Context'RETURN) (some? (get scope :loop-locals))) => (throw! "can only recur from tail position")
            (let* [#_"vector" args (vec (map (fn* [a] (Compiler'analyze a, scope)) (next form))) #_"int" n (count args) #_"int" m (count (get scope :loop-locals))]
                (when (= n m) => (throw! (str "mismatched argument count to recur, expected: " m " args, got: " n))
                    (RecurExpr'new (get scope :loop-locals), args)
                )
            )
        )
    )

    (defn #_"gen" RecurExpr''emit [#_"RecurExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let* [#_"label" l'loop (get scope :loop-label)]
            (when (some? l'loop) => (throw! "recur misses loop label")
                (let* [
                    gen
                        (loop* [gen gen #_"seq" s (seq (:args this))]
                            (when (some? s) => gen
                                (recur (Expr'''emit (first s), :Context'EXPRESSION, scope, gen) (next s))
                            )
                        )
                    gen
                        (loop* [gen gen #_"seq" s (rseq (:loopLocals this))]
                            (when (some? s) => gen
                                (recur (Gen''store gen, (:idx (first s))) (next s))
                            )
                        )
                ]
                    (Gen''goto gen, l'loop)
                )
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
            (array-map
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
        (let* [
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
        (array-map
            '&     nil
            'def   DefExpr'parse
            'do    BodyExpr'parse
            'fn*   FnExpr'parse
            'if    IfExpr'parse
            'let*  LetExpr'parse
            'loop* LetExpr'parse
            'quote LiteralExpr'parse
            'recur RecurExpr'parse
            'throw ThrowExpr'parse
        )
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
            (let* [#_"LocalBinding" lb (get (deref (get scope :'local-env)) sym)]
                (when (some? lb)
                    (Compiler'closeOver lb, (get scope :fm))
                    (LocalBindingExpr'new lb)
                )
            )
            (let* [#_"Var" v (Compiler'resolve sym)]
                (VarExpr'new v)
            )
        )
    )

    (defn #_"Expr" Compiler'analyzeSeq [#_"seq" form, #_"Context" context, #_"map" scope]
        (let* [#_"any" op (first form)]
            (when (some? op) => (throw! (str "can't call nil, form: " form))
                (let* [#_"fn" f'parse (or (get Compiler'specials op) InvokeExpr'parse)]
                    (f'parse form, context, scope)
                )
            )
        )
    )

    (defn #_"Expr" Compiler'analyze
        ([#_"edn" form, #_"map" scope] (Compiler'analyze form, :Context'EXPRESSION, scope))
        ([#_"edn" form, #_"Context" context, #_"map" scope]
            (let* [form
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

(defn eval [form] (Compiler'eval form))
)

(about #_"arbace.arm.Machine"

(about #_"asm"
    (defn #_"gen" Gen'new [] (vector))

    (defn #_"label" Gen''label [#_"gen" gen] (atom nil))

    (defn Gen''mark
        (#_"label" [#_"gen" gen] (atom (count gen)))
        (#_"gen" [#_"gen" gen, #_"label" label] (reset! label (count gen)) gen)
    )

    (defn #_"gen" Gen''anew    [#_"gen" gen]                          (conj gen [:anew]))
    (defn #_"gen" Gen''apply   [#_"gen" gen]                          (conj gen [:apply]))
    (defn #_"gen" Gen''aset    [#_"gen" gen]                          (conj gen [:aset]))
    (defn #_"gen" Gen''create  [#_"gen" gen, #_"FnExpr" fun]          (conj gen [:create fun]))
    (defn #_"gen" Gen''dup     [#_"gen" gen]                          (conj gen [:dup]))
    (defn #_"gen" Gen''get     [#_"gen" gen, #_"Symbol" name]         (conj gen [:get name]))
    (defn #_"gen" Gen''goto    [#_"gen" gen, #_"label" label]         (conj gen [:goto label]))
    (defn #_"gen" Gen''if-eq?  [#_"gen" gen, #_"label" label]         (conj gen [:if-eq? label]))
    (defn #_"gen" Gen''if-nil? [#_"gen" gen, #_"label" label]         (conj gen [:if-nil? label]))
    (defn #_"gen" Gen''invoke  [#_"gen" gen, #_"fn" f, #_"int" arity] (conj gen [(keyword (str "invoke-" arity)) f]))
    (defn #_"gen" Gen''load    [#_"gen" gen, #_"int" index]           (conj gen [:load index]))
    (defn #_"gen" Gen''pop     [#_"gen" gen]                          (conj gen [:pop]))
    (defn #_"gen" Gen''push    [#_"gen" gen, #_"value" value]         (conj gen [:push value]))
    (defn #_"gen" Gen''return  [#_"gen" gen]                          (conj gen [:return]))
    (defn #_"gen" Gen''store   [#_"gen" gen, #_"int" index]           (conj gen [:store index]))
    (defn #_"gen" Gen''throw   [#_"gen" gen]                          (conj gen [:throw]))
)

(about #_"Machine"
    (defn #_"any" Machine'compute [#_"code" code, #_"array" vars]
        (loop* [#_"stack" s nil #_"int" i 0]
            (let* [[x y] (nth code i)]
                (condp = x
                    :anew     (let* [[    a & s] s]                  (recur (cons (anew a) s)           (inc i)))
                    :apply    (let* [[  b a & s] s]                  (recur (cons (apply a b) s)        (inc i)))
                    :aset     (let* [[c b a & s] s] (aset! a b c)    (recur s                           (inc i)))
                    :create   (let* [[    a & s] s]                  (recur (cons (Closure'new y, a) s) (inc i)))
                    :dup      (let* [[    a]     s]                  (recur (cons a s)                  (inc i)))
                    :get      (let* [[    a & s] s]                  (recur (cons (get (:env a) y) s)   (inc i)))
                    :goto                                           (recur s                        (deref y))
                    :if-eq?   (let* [[  b a & s] s]                  (recur s        (if     (= a b) (deref y) (inc i))))
                    :if-nil?  (let* [[    a & s] s]                  (recur s        (if  (nil? a)   (deref y) (inc i))))
                    :invoke-1 (let* [[    a & s] s]                  (recur (cons (y a) s)              (inc i)))
                    :invoke-2 (let* [[  b a & s] s]                  (recur (cons (y a b) s)            (inc i)))
                    :load                                           (recur (cons (aget vars y) s)      (inc i))
                    :pop                                            (recur (next s)                    (inc i))
                    :push                                           (recur (cons y s)                  (inc i))
                    :return                        (first s)
                    :store    (let* [[    a & s] s] (aset! vars y a) (recur s                           (inc i)))
                    :throw                         (throw (first s))
                )
            )
        )
    )
)
)
