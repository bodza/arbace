(ns arbace.wector
    (:refer-clojure :only [defmacro])
)

(defmacro § [& _])
(defmacro ß [& _])

(ns arbace.bore
    (:refer-clojure :only [*ns* -> = apply case conj cons defmacro defn defn- doseq fn identity keys keyword let letfn map mapcat merge meta select-keys some? str symbol symbol? vary-meta vec vector when with-meta]) (:require [clojure.core :as -])
)

(defmacro import! [& syms-or-seqs] `(do (doseq [n# (keys (-/ns-imports *ns*))] (-/ns-unmap *ns* n#)) (-/import ~@syms-or-seqs)))

(import! [java.lang Error #_String System Thread] [clojure.lang ILookup ITransientAssociative])

(defmacro refer! [ns s]
    (let [f #(let [v (-/ns-resolve (-/the-ns (if (= ns '-) 'clojure.core ns)) %) n (vary-meta % merge (select-keys (meta v) [:private :macro]))] `(def ~n ~v))]
        (if (symbol? s) (f s) (cons 'do (map f s)))
    )
)

(defmacro about [& s] (cons 'do s))

(about #_"Oops!"

(defn- emit-defarray* [tname cname fields interfaces methods opts]
    (let [
        classname  (with-meta (symbol (str (-/namespace-munge *ns*) "." cname)) (meta cname))
        interfaces (vec interfaces)
        fields     (map #(with-meta % nil) fields)
    ]
        (let [a '__data v (-/gensym)]
            (letfn [(ilookup [[i m]]
                        [
                            (conj i 'clojure.lang.ILookup)
                            (conj m
                                `(valAt [this# k#] (.valAt this# k# nil))
                                `(valAt [this# k# else#] (case k# ~@(mapcat (fn [x y] [(keyword y) `(-/aget ~a ~x)]) (-/range) fields)))
                            )
                        ]
                    )
                    (imap [[i m]]
                        [
                            (conj i 'clojure.lang.ITransientAssociative)
                            (conj m
                                `(assoc [this# k# ~v] (case k# ~@(mapcat (fn [x y] [(keyword y) `(-/aset ~a ~x ~v)]) (-/range) fields)) this#)
                            )
                        ]
                    )]
                (let [[i m] (-> [interfaces methods] ilookup imap)]
                    `(deftype* ~(symbol (-/name (-/ns-name *ns*)) (-/name tname))
                        ~classname
                        ~(vector a)
                        :implements ~(vec i)
                        ~@(mapcat identity opts)
                        ~@m
                    )
                )
            )
        )
    )
)
 
(defmacro defarray [name fields & opts+specs]
    (#'-/validate-fields fields name)
    (let [[interfaces methods opts] (#'-/parse-opts+specs opts+specs)]
        `(do
            ~(emit-defarray* name name (vec fields) (vec interfaces) methods opts)
            (-/import ~(symbol (str (-/namespace-munge *ns*) "." name)))
        )
    )
)
)

(defmacro defp [p & s]   (let [i (symbol (str p "'iface"))] `(do (-/defprotocol ~p ~@s) (def ~i (:on-interface ~p)) ~p)))
(defmacro defq [r f & s] (let [c (symbol (str r "'class"))] `(do (defarray ~c ~(vec f) ~r ~@s)                      ~c)))
(defmacro defm [r & s]   (let [i `(:on-interface ~r)]       `(do (-/extend-type ~i ~@s)                             ~i)))

(defmacro throw! [#_"String" s] `(throw (Error. ~s)))

(refer! - [< <= max min neg? pos? zero?])

(def +    -/unchecked-add-int)
(def neg  -/unchecked-negate-int)
(def -    -/unchecked-subtract-int)
(def inc  -/unchecked-inc-int)
(def dec  -/unchecked-dec-int)
(def *    -/unchecked-multiply-int)
(def quot -/unchecked-divide-int)
(def rem  -/unchecked-remainder-int)

(def &   -/bit-and)
(def |   -/bit-or)
(def <<  -/bit-shift-left)
(def >>  -/bit-shift-right)
(def >>> -/unsigned-bit-shift-right)

(refer! - [aget alength])

(defn anew [size-or-seq] (-/object-array size-or-seq))
(defn aclone [a]         (when (some? a) (-/aclone a)))
(defn acopy! [a i b j n] (System/arraycopy b, j, a, i, n) a)
(defn aset!  [a i x]     (-/aset a i x) a)
(defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

(defn thread [] (Thread/currentThread))

(ns arbace.wector
    (:refer-clojure :only [-> = and apply assoc assoc! atom case compare concat cond condp cons declare defn defn- drop first fn get identical? if-not if-some int integer? last let letfn list loop map mapcat next nil? not or reduced? reset! satisfies? second seq sequential? some? symbol? take vary-meta vec vector?]) (:require [clojure.core :as -])
    (:refer arbace.bore :only [& * + - < << <= >>> about aclone acopy! aget alength anew aset! aswap! dec defm defp defq import! inc max min neg? pos? quot rem thread throw! zero?])
)

(import!)

(about #_"Oops!"

(defmacro def-      [x & s] `(def      ~(vary-meta x assoc :private true) ~@s))
(defmacro defmacro- [x & s] `(defmacro ~(vary-meta x assoc :private true) ~@s))

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro     when       [? & s] (let [[e & s] (=> s)]               `(if     ~? (do ~@s) ~e)))
    (defmacro     when-not   [? & s] (let [[e & s] (=> s)]               `(if-not ~? (do ~@s) ~e)))
    (defmacro let-when     [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if     ~? (do ~@s) ~e))))
)

(defmacro- assert-args [& s]
    `(when ~(first s) ~'=> (throw! (-/str (first ~'&form) " requires " ~(second s)))
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
            (= 2 (-/count bind)) "exactly 2 forms in binding vector"
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

    (defn editable? [x] (satisfies? IEditableCollection x))

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

(defn count
    ([x] (count x -1))
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
            (when (neg? m) => (throw! (-/str "count not supported on " (-/class x)))
                (-/count x)
            )
        )
    )
)

(defn conj
    ([] [])
    ([coll] coll)
    ([coll x] (if (some? coll) (IPersistentCollection'''conj coll, x) (list x)))
    ([coll x & s] (recur-when s [(conj coll x) (first s) (next s)] => (conj coll x)))
)

(defn reduce
    ([f s] (if-some [s (seq s)] (reduce f (first s) (next s)) (f)))
    ([f r s] (if-some [s (seq s)] (recur f (f r (first s)) (next s)) r))
)

(defn transient [#_"IEditableCollection" coll] (IEditableCollection'''asTransient coll))
(defn persistent! [#_"ITransientCollection" coll] (ITransientCollection'''persistent! coll))

(defn conj!
    ([] (transient []))
    ([coll] coll)
    ([#_"ITransientCollection" coll x] (ITransientCollection'''conj! coll, x))
)

(defn reduce!
    ([f s] (if-some [s (seq s)] (reduce! f (first s) (next s)) (f)))
    ([f r s] (persistent! (reduce f (transient r) s)))
)

(defn into [to from]
    (if (editable? to)
        (reduce! conj! to from)
        (reduce conj to from)
    )
)

(defn update!
    ([m k f] (assoc! m k (f (get m k))))
    ([m k f x] (assoc! m k (f (get m k) x)))
    ([m k f x y] (assoc! m k (f (get m k) x y)))
    ([m k f x y & z] (assoc! m k (apply f (get m k) x y z)))
)
)

(declare MapEntry'create Murmur3'mixCollHash RSeq'new RT'printString VSeq'new)

(about #_"cloiure.core.IPersistentWector"
    (defp IPersistentWector
        (#_"IPersistentWector" IPersistentWector'''slicew [#_"IPersistentWector" this, #_"int" start, #_"int" end])
        (#_"IPersistentWector" IPersistentWector'''splicew [#_"IPersistentWector" this, #_"IPersistentWector" that])
    )
)

(defn wector? [x] (satisfies? IPersistentWector x))

(about #_"arbace.wector"
    (defp WNode)
    (defp TransientWector)
    (defp PersistentWector)
)

(about #_"arbace.wector"

(about #_"WNode"
    (defq WNode [edit, array, index])

    (defn #_"node" WNode'new [#_"Thread'" edit, #_"array" array, #_"index" index]
        (assoc! (WNode'class. (anew 3))
            #_"Thread'" :edit edit
            #_"array" :array (or array (anew 32))
            #_"index" :index index
        )
    )

    (def #_"node" WNode'EMPTY (WNode'new nil, nil, nil))

    (defn #_"void" WNode''assert-editable [#_"node" this]
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

    (defn #_"boolean" WNode''cow? [#_"node" this, #_"Thread'" edit]
        (let [
            #_"Thread'" e (:edit this)
        ]
            (or (nil? e) (nil? @e) (not (or (identical? e edit) (throw! "transient cow!"))))
        )
    )

    (defn #_"node" WNode''editable-root [#_"node" this]
        (WNode'new (atom (thread)), (aclone (:array this)), (aclone (:index this)))
    )

    (defn #_"values" WNode'editable-tail [#_"values" tail]
        (-> (anew 32) (acopy! 0 tail 0 (alength tail)))
    )

    (defn #_"values" WNode''array-for
        ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt] (WNode''array-for this, i, shift, cnt, cnt, nil))
        ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
            (when (< -1 i cnt) => (throw! "index is out of bounds")
                (when (< i tail-off) => tail
                    (loop-when [i i #_"node" node this shift shift] (pos? shift) => (:array node)
                        (let [
                            #_"index" x (:index node)
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

    (defn #_"value" WNode''value-for [#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"values" tail]
        (let [
            #_"int" tail-off (- cnt (alength tail))
        ]
            (aget (WNode''array-for this, i, shift, cnt, tail-off, tail) (if (<= tail-off i) (- i tail-off) (& (>>> i shift) 0x1f)))
        )
    )

    (defn #_"node" WNode''new-path [#_"node" this, #_"Thread'" edit, #_"int" shift]
        (when (pos? shift) => this
            (WNode'new edit, (-> (anew 32) (aset! 0 (WNode''new-path this, edit, (- shift 5)))), nil)
        )
    )

    (defn #_"int" WNode'last-range [#_"index" x]
        (aget x (dec (aget x 32)))
    )

    (defn #_"boolean" WNode''overflow? [#_"node" this, #_"int" shift, #_"int" cnt]
        (let [
            #_"index" x (:index this)
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

    (defn #_"node" WNode''push-tail [#_"node" this, #_"Thread'" edit, #_"int" shift, #_"int" cnt, #_"node" tail-node]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
        ]
            (if (some? x)
                (let [
                    #_"int" e (dec (aget x 32))
                    #_"node" child
                        (when (< 5 shift)
                            (let [
                                #_"int" n (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0))
                            ]
                                (when (< n (<< 1 shift))
                                    (WNode''push-tail (aget a e), edit, (- shift 5), (inc n), tail-node)
                                )
                            )
                        )
                    a (if cow? (aclone a) a) x (if cow? (aclone x) x)
                    [a x]
                        (if (some? child)
                            [(aset! a e child) (aswap! x e + 32)]
                            (let [
                                a (aset! a (inc e) (WNode''new-path tail-node, edit, (- shift 5)))
                                x (aset! x (inc e) (+ (aget x e) 32))
                            ]
                                [a (aswap! x 32 inc)]
                            )
                        )
                ]
                    (if cow? (WNode'new edit, a, x) this)
                )
                (let [
                    #_"int" e (& (>>> (dec cnt) shift) 0x1f)
                    #_"node" child
                        (when (< 5 shift) => tail-node
                            (if-some [child (aget a e)]
                                (WNode''push-tail child, edit, (- shift 5), cnt, tail-node)
                                (WNode''new-path tail-node, edit, (- shift 5))
                            )
                        )
                    a (if cow? (aclone a) a)
                    a (aset! a e child)
                ]
                    (if cow? (WNode'new edit, a, nil) this)
                )
            )
        )
    )

    (defn #_"node" WNode''pop-tail [#_"node" this, #_"Thread'" edit, #_"int" shift, #_"int" cnt]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
            #_"int" e (& (>>> (dec cnt) shift) 0x1f) ;; vector: #_"int" e (& (>>> (- cnt 2) shift) 0x1f): more reasonable!
        ]
            (if (some? x)
                (let [
                    e (loop-when-recur e (and (< e 31) (some? (aget x (inc e)))) (inc e) => e)
                ]
                    (cond
                        (< 5 shift)
                            (let [
                                #_"node" child (aget a e)
                                #_"node" child' (WNode''pop-tail child, edit, (- shift 5), (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0)))
                            ]
                                (when (or (some? child') (pos? e))
                                    (let [
                                        a (if cow? (aclone a) a)
                                        a (-> a (aset! e child'))
                                        x (if cow? (aclone x) x)
                                        x
                                            (if (some? child')
                                                (let [
                                                    #_"int" delta
                                                        (when (some? (:index child)) => 32
                                                            (- (WNode'last-range (:index child)) (WNode'last-range (:index child')))
                                                        )
                                                ]
                                                    (-> x (aswap! e - delta))
                                                )
                                                (-> x (aset! e nil) (aswap! 32 dec))
                                            )
                                    ]
                                        (if cow? (WNode'new edit, a, x) this)
                                    )
                                )
                            )
                        (pos? e)
                            (let [
                                a (-> (if cow? (aclone a) a) (aset! e nil))
                                x (-> (if cow? (aclone x) x) (aset! e nil) (aswap! 32 dec))
                            ]
                                (if cow? (WNode'new edit, a, x) this)
                            )
                    )
                )
                (cond
                    (< 5 shift)
                        (let [
                            #_"node" child (WNode''pop-tail (aget a e), edit, (- shift 5), cnt)
                        ]
                            (when (or (some? child) (pos? e))
                                (let [
                                    a (if cow? (aclone a) a)
                                    a (aset! a e child)
                                ]
                                    (if cow? (WNode'new edit, a, nil) this)
                                )
                            )
                        )
                    (pos? e)
                        (let [
                            a (if cow? (aclone a) a)
                            a (aset! a e nil)
                        ]
                            (if cow? (WNode'new edit, a, nil) this)
                        )
                )
            )
        )
    )

    (defn #_"node" WNode''do-assoc [#_"node" this, #_"Thread'" edit, #_"int" shift, #_"int" i, #_"value" val]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
            a (if cow? (aclone a) a)
            #_"int" m (& (>>> i shift) 0x1f)
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
                        (aswap! a m WNode''do-assoc edit, (- shift 5), i, val)
                    )
                )
        ]
            (if cow? (WNode'new edit, a, x) this)
        )
    )

    (defn- #_"index" WNode'n-index [#_"int" shift, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
        ]
            (loop-when-recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< i n) [(aset! x j i) (inc j) (+ i k)] => (-> x (aset! j n) (aset! 32 (inc j))))
        )
    )

    (defn- #_"index" WNode'm-n-index [#_"int" shift, #_"int" m, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
        ]
            (loop-when-recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< j m) [(aset! x j i) (inc j) (+ i k)] => (-> x (aset! j n) (aset! 32 (inc j))))
        )
    )

    (defn- #_"int" WNode'index-of-nil [#_"array" a]
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

    (defn- #_"node" WNode''first-child [#_"node" this]
        (aget (:array this) 0)
    )

    (defn- #_"node" WNode''last-child [#_"node" this]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (aget a (dec (if (some? x) (aget x 32) (WNode'index-of-nil a))))
        )
    )

    (defn- #_"node" WNode''remove-leftmost-child [#_"node" this]
        (let [
            #_"array" a (:array this)
        ]
            (when (some? (aget a 1))
                (let [
                    #_"index" x (:index this)
                    #_"index" x'
                        (when (some? x)
                            (let [
                                #_"int" k (aget x 0)
                                #_"int" e (dec (aget x 32))
                            ]
                                (loop-when-recur [x' (anew 33) #_"int" j 0] (< j e) [(aset! x' j (- (aget x (inc j)) k)) (inc j)] => (aset! x' 32 e))
                            )
                        )
                ]
                    (WNode'new nil, (-> (anew 32) (acopy! 0 a 1 31)), x')
                )
            )
        )
    )

    (defn- #_"node" WNode''replace-leftmost-child [#_"node" this, #_"int" shift, #_"int" cnt, #_"node" node, #_"int" delta]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
            [#_"array" a' #_"index" x']
                (if (some? x)
                    (let [
                        #_"int" n (aget x 32)
                        x'
                            (loop-when-recur [x' (anew 33) #_"int" j 0]
                                             (< j n)
                                             [(aset! x' j (- (aget x j) delta)) (inc j)]
                                          => (aset! x' 32 n)
                            )
                    ]
                        [(-> (aclone a) (aset! 0 node)) x']
                    )
                    (let [
                        #_"int" k (<< 1 shift)
                        #_"int" n (& (>>> (dec cnt) shift) 0x1f)
                        x'
                            (loop-when-recur [x' (-> (anew 33) (aset! 0 (- k delta))) #_"int" j 0]
                                             (< j n)
                                             [(aset! x' (inc j) (+ (aget x' j) k)) (inc j)]
                                          => (-> x' (aset! n (- cnt delta)) (aset! 32 (inc n)))
                            )
                    ]
                        [(-> (anew 32) (aset! 0 node) (acopy! 1 a 1 n)) x']
                    )
                )
        ]
            (WNode'new nil, a', x')
        )
    )

    (defn- #_"node" WNode''replace-rightmost-child [#_"node" this, #_"int" shift, #_"node" node, #_"int" delta]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
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
                        (WNode'new nil, (-> (anew 32) (acopy! 0 a 0 m) (aset! m node)), (WNode'm-n-index shift, m, (WNode'last-range (:index node))))
                        (WNode'new nil, (-> (aclone a) (aset! m node)), nil)
                    )
                )
            )
        )
    )

    (defn #_"node" WNode''fold-tail [#_"node" this, #_"int" shift, #_"int" tail-off, #_"values" tail]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
            #_"int" m (WNode'index-of-nil a)
            #_"node" tail-node
                (when (< 5 shift) => (WNode'new nil, tail, nil)
                    (let [
                        #_"int" n
                            (when (some? x) => (rem tail-off (<< 1 shift))
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
                    #_"index" x'
                        (when (or (some? x) (< n 32))
                            (let [
                                x' (or (aclone x) (WNode'n-index shift, tail-off))
                            ]
                                (if (and (some? tail-node) (< 5 shift))
                                    (let [
                                        x' (if (pos? m) (aswap! x' (dec m) + n) x')
                                    ]
                                        (-> x' (aset! 32 m))
                                    )
                                    (let [
                                        x' (aset! x' m (+ (if (pos? m) (aget x' (dec m)) 0) n))
                                    ]
                                        (-> x' (aset! 32 (inc m)))
                                    )
                                )
                            )
                        )
                    #_"array" a' (-> (anew 32) (acopy! 0 a 0 m))
                    a'
                        (if (some? tail-node)
                            (aset! a' (if (< 5 shift) (dec m) m) tail-node)
                            (aset! a' m (WNode''new-path (WNode'new nil, tail, nil), nil, (- shift 5)))
                        )
                ]
                    (WNode'new nil, a', x')
                )
            )
        )
    )

    (def #_"int" WNode'rrbt-concat-threshold 33)
    (def- #_"int" WNode'max-extra-search-steps 2)

    (defn #_"node" WNode''slice-right [#_"node" this, #_"int" shift, #_"int" end]
        ;; => potentially return a short node, although it would be better to make sure a regular
        ;; leaf is always left at the right, with any items over the final 32 moved into tail
        ;; (and then potentially back into the tree should the tail become too long...)
        (when (pos? shift) => (WNode'new nil, (-> (anew end) (acopy! 0 (:array this) 0 end)), nil)
            (let [
                #_"array" a (:array this) #_"index" x (:index this)
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
                #_"node" child (WNode''slice-right (aget a m), (- shift 5), child-end)
                #_"index" y (:index child)
                #_"array" a' (-> (anew 32) (acopy! 0 a 0 m) (aset! m child))
                #_"index" x'
                    (when (or (some? x) (some? y))
                        (let [
                            x' (loop-when-recur [x' (anew 33) #_"int" j 0] (< j m) [(aset! x' j (if (some? x) (aget x j) (* (inc j) k))) (inc j)] => x')
                            #_"int" delta
                                (cond
                                    (nil? y)    (let [#_"int" e (rem child-end k) ] (if (zero? e) k e))
                                    (< 5 shift) (WNode'last-range y)
                                    :else       (alength (:array child))
                                )
                            x' (aset! x' m (+ (if (pos? m) (aget x' (dec m)) 0) delta))
                        ]
                            (-> x' (aset! 32 (inc m)))
                        )
                    )
            ]
                (WNode'new nil, a', x')
            )
        )
    )

    (defn #_"node" WNode''slice-left [#_"node" this, #_"int" shift, #_"int" start, #_"int" end]
        (if (zero? shift)
            ;; potentially return a short node
            (let [
                #_"array" a (:array this)
                #_"int" n (- (alength a) start)
            ]
                (WNode'new nil, (-> (anew n) (acopy! 0 a start n)), nil)
            )
            (let [
                #_"array" a (:array this) #_"index" x (:index this)
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
                #_"node" child
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
                        #_"index" x'
                            (if (some? x)
                                (loop-when-recur [x' (anew 33) #_"int" j 0 #_"int" i m]
                                                 (< j n)
                                                 [(aset! x' j (- (aget x i) start)) (inc j) (inc i)]
                                              => (aset! x' 32 n)
                                )
                                (let [
                                    #_"int" i
                                        (if (and (some? child) (some? (:index child)) (< 5 shift))
                                            (WNode'last-range (:index child))
                                            (- k (& (>>> start (- shift 5)) 0x1f))
                                        )
                                ]
                                    (loop-when-recur [x' (anew 33) #_"int" j 0 i i]
                                                     (< j n)
                                                     [(aset! x' j i) (inc j) (+ i k)]
                                                  => (-> (if (< 1 n) (aset! x' (dec n) (- end start)) x') (aset! 32 n))
                                    )
                                )
                            )
                        #_"array" a'
                            (if (some? child)
                                (-> (anew 32) (aset! 0 child) (acopy! 1 a (inc m) (dec n)))
                                (-> (anew 32) (acopy! 0 a (inc m) n))
                            )
                    ]
                        (WNode'new nil, a', x')
                    )
                )
            )
        )
    )

    (defn #_"node" WNode''shift-from-to [#_"node" this, #_"int" from, #_"int" to]
        (when-not (= from to) => this
            (let [
                #_"index" x'
                    (when (some? (:index this))
                        (-> (anew 33) (aset! 0 (WNode'last-range (:index this))) (aset! 32 1))
                    )
            ]
                (recur (WNode'new nil, (-> (anew 32) (aset! 0 this)), x') (+ 5 from) to)
            )
        )
    )

    (defn- #_"int" WNode''leaf-count [#_"node" this, #_"int" shift]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (cond
                (zero? shift) (alength a)
                (some? x)     (aget x 32)
                :else         (WNode'index-of-nil a)
            )
        )
    )

    (defn- #_"int" WNode''tree-count [#_"node" this, #_"int" shift]
        ;; NB. positive shifts only
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (loop-when-recur [#_"int" i 0 #_"int" n 0]
                             (if (some? x) (< i (aget x 32)) (and (< i 32) (some? (aget a i))))
                             [(inc i) (+ n (WNode''leaf-count (aget a i), (- shift 5)))]
                          => n
            )
        )
    )

    (defn- #_"seq" WNode''leaf-seq [#_"node" this]
        (let [
            #_"array" a (:array this)
        ]
            (mapcat :array (take (WNode'index-of-nil a) a))
        )
    )

    (defn- #_"[node node int]" WNode'rebalance-leaves [#_"node" node1, #_"node" node2, #_"int" delta]
        (let [
            #_"int" n1 (WNode''tree-count node1, 5) #_"int" n2 (WNode''tree-count node2, 5) #_"int" n (+ n1 n2)
        ]
            (when (< WNode'max-extra-search-steps (- (+ (WNode''leaf-count node1, 5) (WNode''leaf-count node2, 5)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
                (let [
                    #_"seq" s (map #(WNode'new nil, (anew %), nil) (-/partition-all 32 (concat (WNode''leaf-seq node1) (WNode''leaf-seq node2))))
                ]
                    (if (<= n (* 32 32))
                        (let [
                            #_"index" x' (when-not (zero? (rem n 32)) (WNode'n-index 5, n))
                        ]
                            [(WNode'new nil, (anew s), x') nil n2]
                        )
                        (let [
                            #_"index" x' (when-not (zero? (rem n 32)) (WNode'n-index 5, (- n (* 32 32))))
                        ]
                            [(WNode'new nil, (anew (take 32 s)), nil) (WNode'new nil, (anew (drop 32 s)), x') (- (* 32 32) n1)]
                        )
                    )
                )
            )
        )
    )

    (defn- #_"seq" WNode''child-seq [#_"node" this, #_"int" shift, #_"int" cnt]
        (let [
            f'cseq
                (fn [#_"node" this #_"int" cnt]
                    (let [
                        #_"index" x (or (:index this) (WNode'n-index (- shift 5), cnt))
                        #_"int" n (aget x 32)
                    ]
                        (take n (map list (:array this) (map - x (cons 0 x))))
                    )
                )
            #_"index" x (or (:index this) (WNode'n-index shift, cnt))
            #_"int" n (aget x 32)
        ]
            (mapcat f'cseq (take n (:array this)) (take n (map - x (cons 0 x))))
        )
    )

    (defn- #_"[node node int]" WNode'rebalance [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
        (when (some? node2) => [node1 nil delta]
            (let [
                #_"int" n1 (WNode''tree-count node1, shift) #_"int" n2 (WNode''tree-count node2, shift) #_"int" n (+ n1 n2)
            ]
                (when (< WNode'max-extra-search-steps (- (+ (WNode''leaf-count node1, shift) (WNode''leaf-count node2, shift)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
                    (let [
                        f'cnode
                            (fn [#_"seq" s]
                                (loop [#_"array" a (anew 32) #_"index" x (anew 33) #_"int" j 0 #_"int" k 0 s s]
                                    (when-first [[#_"node" c #_"int" r] s] => (WNode'new nil, a, (aset! x 32 j))
                                        (recur (aset! a j c) (aset! x j (+ k r)) (inc j) (+ k r) (next s))
                                    )
                                )
                            )
                        #_"seq" s (-/partition-all 32 (concat (WNode''child-seq node1, shift, cnt1) (WNode''child-seq node2, shift, cnt2)))
                    ]
                        (if (<= n (* 32 32))
                            (loop [#_"array" a (anew 32) #_"index" x (-> (anew 33) (aset! 32 0)) #_"int" i 0 s s]
                                (when-first [#_"seq" block s] => [(WNode'new nil, a, x) nil cnt2]
                                    (let [
                                        #_"node" c (f'cnode block)
                                        a (aset! a i c)
                                        x (aset! x i (+ (WNode'last-range (:index c)) (if (pos? i) (aget x (dec i)) 0)))
                                        x (aset! x 32 (inc i))
                                    ]
                                        (recur a x (inc i) (next s))
                                    )
                                )
                            )
                            (let [
                                #_"array" a1 (anew 32) #_"index" x1 (-> (anew 33) (aset! 32 0))
                                #_"array" a2 (anew 32) #_"index" x2 (-> (anew 33) (aset! 32 0))
                            ]
                                (loop [a1 a1 x1 x1 a2 a2 x2 x2 delta delta #_"int" i 0 s s]
                                    (when-first [#_"seq" block s] => [(WNode'new nil, a1, x1) (WNode'new nil, a2, x2) delta]
                                        (let [
                                            #_"node" c (f'cnode block) #_"index" y (:index c)
                                            delta
                                                (when (and (< i 32) (< n1 (+ (* i 32) (aget y 32)))) => delta
                                                    (let [
                                                        #_"int" k (- (+ (* i 32) (aget y 32)) n1)
                                                        #_"int" e (dec (aget y 32))
                                                    ]
                                                        (+ delta (if (< k 32) (- (aget y e) (aget y (- e k))) (aget y e)))
                                                    )
                                                )
                                            [a1 x1 a2 x2]
                                                (if (< i 32)
                                                    (let [
                                                        #_"int" m (rem i 32)
                                                        a1 (aset! a1 m c)
                                                        x1 (aset! x1 m (+ (WNode'last-range y) (if (pos? m) (aget x1 (dec m)) 0)))
                                                        x1 (aset! x1 32 (inc m))
                                                    ]
                                                        [a1 x1 a2 x2]
                                                    )
                                                    (let [
                                                        #_"int" m (rem i 32)
                                                        a2 (aset! a2 m c)
                                                        x2 (aset! x2 m (+ (WNode'last-range y) (if (pos? m) (aget x2 (dec m)) 0)))
                                                        x2 (aset! x2 32 (inc m))
                                                    ]
                                                        [a1 x1 a2 x2]
                                                    )
                                                )
                                        ]
                                            (recur a1 x1 a2 x2 delta (inc i) (next s))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"[node node int]" WNode'zip-path [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
        (if (= shift 5)
            (WNode'rebalance-leaves node1, node2, delta)
            (let [
                #_"node" c1 (WNode''last-child node1)
                #_"node" c2 (WNode''first-child node2)
                #_"int" k (<< 1 shift)
                #_"int" m1
                    (let [
                        #_"index" x1 (:index node1)
                    ]
                        (when (some? x1) => (let [#_"int" m (rem cnt1 k)] (if (zero? m) k m))
                            (let [#_"int" e (dec (aget x1 32))]
                                (if (pos? e) (- (aget x1 e) (aget x1 (dec e))) (aget x1 0))
                            )
                        )
                    )
                #_"int" m2
                    (let [
                        #_"index" x2 (:index node2)
                    ]
                        (when (some? x2) => (let [#_"int" m (rem cnt2 k)] (if (zero? m) k m))
                            (aget x2 0)
                        )
                    )
                [#_"node" c1' #_"node" c2' #_"int" d'] (WNode'zip-path (- shift 5), c1, m1, c2, m2, 0)
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

    (defn #_"[node node]" WNode'squash-nodes [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2]
        (let [
            #_"array" a1 (:array node1) #_"int" n1 (WNode'index-of-nil a1)
            #_"array" a2 (:array node2) #_"int" n2 (WNode'index-of-nil a2)
            #_"seq" slots (concat (take n1 a1) (take n2 a2))
        ]
            (when (<= (count slots) 32) => [node1 node2]
                (let [
                    #_"seq" s1 (take n1 (or (:index node1) (WNode'n-index shift, cnt1)))
                    #_"seq" s2 (take n2 (or (:index node2) (WNode'n-index shift, cnt2)))
                    #_"seq" index (concat s1 (let [#_"int" d (last s1)] (map #(+ % d) s2)))
                    #_"array" a (loop-when-recur [a (anew 32) #_"int" i 0 #_"seq" s (seq slots)] (some? s) [(aset! a i (first s)) (inc i) (next s)] => a)
                    #_"index" x (loop-when-recur [x (anew 33) #_"int" i 0 #_"seq" s (seq index)] (some? s) [(aset! x i (first s)) (inc i) (next s)] => (aset! x 32 i))
                ]
                    [(WNode'new nil, a, x) nil]
                )
            )
        )
    )
)

(about #_"TransientWector"
    (defq TransientWector [cnt, shift, root, tail, tlen] AFn)

    (defn #_"TransientWector" TransientWector'new
        ([#_"PersistentWector" w]
            (TransientWector'new (:cnt w), (:shift w), (WNode''editable-root (:root w)), (WNode'editable-tail (:tail w)), (alength (:tail w)))
        )
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen]
            (assoc! (TransientWector'class. (anew 5))
                #_"int" :cnt cnt
                #_"int" :shift shift
                #_"node" :root root
                #_"values" :tail tail
                #_"int" :tlen tlen
            )
        )
    )

    (defn- #_"int" TransientWector''tail-off [#_"TransientWector" this]
        (- (:cnt this) (:tlen this))
    )

    (defn- #_"values" TransientWector''array-for [#_"TransientWector" this, #_"int" i]
        (WNode''array-for (:root this), i, (:shift this), (:cnt this), (TransientWector''tail-off this), (:tail this))
    )

    (defm TransientWector Counted
        (#_"int" Counted'''count [#_"TransientWector" this]
            (WNode''assert-editable (:root this))
            (:cnt this)
        )
    )

    (defm TransientWector Indexed
        (#_"value" Indexed'''nth
            ([#_"TransientWector" this, #_"int" i]
                (WNode''assert-editable (:root this))
                (WNode''value-for (:root this), i, (:shift this), (:cnt this), (:tail this))
            )
            ([#_"TransientWector" this, #_"int" i, #_"value" not-found]
                (WNode''assert-editable (:root this))
                (when (< -1 i (:cnt this)) => not-found
                    (WNode''value-for (:root this), i, (:shift this), (:cnt this), (:tail this))
                )
            )
        )
    )

    (defm TransientWector ILookup
        (#_"value" ILookup'''valAt
            ([#_"TransientWector" this, #_"key" key] (ILookup'''valAt this, key, nil))
            ([#_"TransientWector" this, #_"key" key, #_"value" not-found]
                (WNode''assert-editable (:root this))
                (when (integer? key) => not-found
                    (let-when [#_"int" i (int key)] (< -1 i (:cnt this)) => not-found
                        (WNode''value-for (:root this), i, (:shift this), (:cnt this), (:tail this))
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
            (case (count args 1)
                1 (IFn'''invoke this, (first args))
            )
        )
    )

    (declare PersistentWector'new)

    (defm TransientWector ITransientCollection
        (#_"TransientWector" ITransientCollection'''conj! [#_"TransientWector" this, #_"value" val]
            (WNode''assert-editable (:root this))
            (if (< (:tlen this) 32)
                (let [
                    _ (aset! (:tail this) (:tlen this) val)
                ]
                    (-> this (update! :cnt inc) (update! :tlen inc))
                )
                (let [
                    #_"node" tail-node (WNode'new (:edit (:root this)), (:tail this), nil)
                    this (assoc! this :tail (-> (anew 32) (aset! 0 val)), :tlen 1)
                ]
                    (if (WNode''overflow? (:root this), (:shift this), (:cnt this))
                        (let [
                            #_"array" a
                                (-> (anew 32)
                                    (aset! 0 (:root this))
                                    (aset! 1 (WNode''new-path tail-node, (:edit (:root this)), (:shift this)))
                                )
                            #_"index" x
                                (when (some? (:index (:root this)))
                                    (let [
                                        #_"int" n (aget (:index (:root this)) 31)
                                    ]
                                        (-> (anew 33) (aset! 0 n) (aset! 1 (+ n 32)) (aset! 32 2))
                                    )
                                )
                            #_"node" root (WNode'new (:edit (:root this)), a, x)
                        ]
                            (-> this (assoc! :root root) (update! :shift + 5) (update! :cnt inc))
                        )
                        (let [
                            #_"node" root (WNode''push-tail (:root this), (:edit (:root this)), (:shift this), (:cnt this), tail-node)
                        ]
                            (-> this (assoc! :root root) (update! :cnt inc))
                        )
                    )
                )
            )
        )

        (#_"PersistentWector" ITransientCollection'''persistent! [#_"TransientWector" this]
            (WNode''assert-editable (:root this))
            (reset! (:edit (:root this)) nil)
            (let [
                #_"int" n (:tlen this)
            ]
                (PersistentWector'new (:cnt this), (:shift this), (:root this), (-> (anew n) (acopy! 0 (:tail this) 0 n)))
            )
        )
    )

    (defm TransientWector ITransientVector
        (#_"TransientWector" ITransientVector'''assocN! [#_"TransientWector" this, #_"int" i, #_"value" val]
            (WNode''assert-editable (:root this))
            (if (< -1 i (:cnt this))
                (let [
                    #_"int" tail-off (TransientWector''tail-off this)
                ]
                    (if (<= tail-off i)
                        (do
                            (aset! (:tail this) (- i tail-off) val)
                            this
                        )
                        (do
                            (assoc! this :root (WNode''do-assoc (:root this), (:edit (:root this)), (:shift this), i, val))
                        )
                    )
                )
                (when (= i (:cnt this)) => (throw! "index is out of bounds")
                    (ITransientCollection'''conj! this, val)
                )
            )
        )

        (#_"TransientWector" ITransientVector'''pop! [#_"TransientWector" this]
            (WNode''assert-editable (:root this))
            (cond
                (zero? (:cnt this))
                    (throw! "can't pop the empty vector")
                (= (:cnt this) 1)
                    (let [
                        this (assoc! this :cnt 0)
                        this (assoc! this :tlen 0)
                        _ (aset! (:tail this) 0 nil)
                    ]
                        this
                    )
                (< 1 (:tlen this))
                    (let [
                        this (update! this :cnt dec)
                        this (update! this :tlen dec)
                        _ (aset! (:tail this) (:tlen this) nil)
                    ]
                        this
                    )
                :else
                    (let [
                        #_"values" tail (aclone (TransientWector''array-for this, (- (:cnt this) 2)))
                        #_"node" root (WNode''pop-tail (:root this), (:edit (:root this)), (:shift this), (:cnt this))
                        this
                            (cond
                                (nil? root)
                                    (-> this
                                        (assoc! :root (WNode'new (:edit (:root this)), nil, nil))
                                    )
                                (and (< 5 (:shift this)) (nil? (aget (:array root) 1)))
                                    (-> this
                                        (update! :shift - 5)
                                        (assoc! :root (aget (:array root) 0))
                                    )
                                :else
                                    (-> this
                                        (assoc! :root root)
                                    )
                            )
                    ]
                        (-> this
                            (update! :cnt dec)
                            (assoc! :tail tail)
                            (assoc! :tlen (alength tail))
                        )
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
)

(about #_"PersistentWector"
    (defq PersistentWector [_meta, cnt, shift, root, tail] APersistentVector)

    (defn #_"PersistentWector" PersistentWector'new
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] (PersistentWector'new nil, cnt, shift, root, tail))
        ([#_"IPersistentMap" meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
            (assoc! (PersistentWector'class. (anew 5))
                #_"IPersistentMap" :_meta meta
                #_"int" :cnt cnt
                #_"int" :shift shift
                #_"node" :root root
                #_"values" :tail tail
            )
        )
    )

    (def #_"PersistentWector" PersistentWector'EMPTY (PersistentWector'new 0, 5, WNode'EMPTY, (anew 0)))

    (defn #_"PersistentWector" PersistentWector'create [& values]
        (when-some [#_"seq" s (seq values)] => PersistentWector'EMPTY
            (let [
                #_"values" tail (anew (take 32 s)) #_"int" n (alength tail)
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

    (defn- #_"int" PersistentWector''tail-off [#_"PersistentWector" this]
        (- (:cnt this) (alength (:tail this)))
    )

    (defn- #_"values" PersistentWector''array-for [#_"PersistentWector" this, #_"int" i]
        (WNode''array-for (:root this), i, (:shift this), (:cnt this), (PersistentWector''tail-off this), (:tail this))
    )

    (defm PersistentWector Indexed
        (#_"value" Indexed'''nth
            ([#_"PersistentWector" this, #_"int" i]
                (WNode''value-for (:root this), i, (:shift this), (:cnt this), (:tail this))
            )
            ([#_"PersistentWector" this, #_"int" i, #_"value" not-found]
                (when (< -1 i (:cnt this)) => not-found
                    (WNode''value-for (:root this), i, (:shift this), (:cnt this), (:tail this))
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
                        #_"values" tail (-> (anew (inc tail-len)) (acopy! 0 (:tail this) 0 tail-len) (aset! tail-len val))
                    ]
                        (PersistentWector'new (:_meta this), (inc (:cnt this)), (:shift this), (:root this), tail)
                    )
                    (let [
                        #_"node" tail-node (WNode'new (:edit (:root this)), (:tail this), nil)
                        #_"int" shift (:shift this)
                        [#_"node" root shift]
                            (if (WNode''overflow? (:root this), shift, (:cnt this))
                                (let [
                                    #_"array" a
                                        (-> (anew 32)
                                            (aset! 0 (:root this))
                                            (aset! 1 (WNode''new-path tail-node, (:edit (:root this)), shift))
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
                                    [(WNode'new (:edit (:root this)), a, x) (+ shift 5)]
                                )
                                [(WNode''push-tail (:root this), (:edit (:root this)), shift, (:cnt this), tail-node) shift]
                            )
                    ]
                        (PersistentWector'new (:_meta this), (inc (:cnt this)), shift, root, (anew [ val ]))
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
                    #_"int" tail-off (PersistentWector''tail-off this)
                ]
                    (if (<= tail-off i)
                        (let [
                            #_"int" n (alength (:tail this))
                            #_"values" tail (-> (anew n) (acopy! 0 (:tail this) 0 n) (aset! (- i tail-off) val))
                        ]
                            (PersistentWector'new (:_meta this), (:cnt this), (:shift this), (:root this), tail)
                        )
                        (PersistentWector'new (:_meta this), (:cnt this), (:shift this), (WNode''do-assoc (:root this), (:edit (:root this)), (:shift this), i, val), (:tail this))
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
                            #_"values" tail (-> (anew (dec tail-len)) (acopy! 0 (:tail this) 0 (dec tail-len)))
                        ]
                            (PersistentWector'new (:_meta this), (dec (:cnt this)), (:shift this), (:root this), tail)
                        )
                        (let [
                            #_"values" tail (PersistentWector''array-for this, (- (:cnt this) 2))
                            #_"int" shift (:shift this)
                            #_"node" root (WNode''pop-tail (:root this), (:edit (:root this)), shift, (PersistentWector''tail-off this)) ;; all the others: (:cnt this)
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
            (case (count args 1)
                1 (IFn'''invoke this, (first args))
            )
        )
    )

    (defm PersistentWector IReduce
        (#_"value" IReduce'''reduce
            ([#_"PersistentWector" this, #_"IFn" f]
                (when (pos? (:cnt this)) => (f)
                    (loop-when [#_"value" r (aget (PersistentWector''array-for this, 0) 0) #_"int" i 0] (< i (:cnt this)) => r
                        (let [#_"values" a (PersistentWector''array-for this, i)
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
                    (let [#_"values" a (PersistentWector''array-for this, i)
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
                    #_"values" a (PersistentWector''array-for this, i)
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
                        #_"int" tail-off (PersistentWector''tail-off this)
                    ]
                        (if (<= tail-off start)
                            (let [
                                #_"values" tail (-> (anew new-cnt) (acopy! 0 (:tail this) (- start tail-off) new-cnt))
                            ]
                                (PersistentWector'new (:_meta this), new-cnt, 5, WNode'EMPTY, tail)
                            )
                            (let [
                                #_"boolean" tail-cut? (< tail-off end)
                                #_"node" root (:root this)
                                root (if tail-cut? root (WNode''slice-right root, (:shift this), end))
                                root (if (zero? start) root (WNode''slice-left root, (:shift this), start, (min end tail-off)))
                                #_"values" tail
                                    (when tail-cut? => (WNode''array-for root, (dec new-cnt), (:shift this), new-cnt)
                                        (let [
                                            #_"int" n (- end tail-off)
                                        ]
                                            (-> (anew n) (acopy! 0 (:tail this) 0 n))
                                        )
                                    )
                                root
                                    (when-not tail-cut? => root
                                        (WNode''pop-tail root, nil, (:shift this), new-cnt)
                                    )
                            ]
                                (when (some? root) => (PersistentWector'new (:_meta this), new-cnt, 5, WNode'EMPTY, tail)
                                    (loop-when-recur [#_"node" node root #_"int" shift (:shift this)]
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
                            #_"node" r1 (:root this) #_"int" s1 (:shift this) #_"array" t1 (:tail this)
                            #_"boolean" overflow? (WNode''overflow? r1, s1, (+ c1 (- 32 (alength t1))))
                            r1
                                (when overflow? => (WNode''fold-tail r1, s1, (PersistentWector''tail-off this), t1)
                                    (let [
                                        #_"array" a'
                                            (-> (anew 32)
                                                (aset! 0 r1)
                                                (aset! 1 (WNode''new-path (WNode'new nil, t1, nil), nil, s1))
                                            )
                                        #_"index" x'
                                            (when (or (some? (:index r1)) (< (alength t1) 32))
                                                (-> (anew 33) (aset! 0 (- c1 (alength t1))) (aset! 1 c1) (aset! 32 2))
                                            )
                                    ]
                                        (WNode'new nil, a', x')
                                    )
                                )
                            s1 (if overflow? (+ s1 5) s1)
                            #_"node" r2 (:root that) #_"int" s2 (:shift that) #_"array" t2 (:tail that)
                            #_"int" shift (max s1 s2)
                            r1 (WNode''shift-from-to r1, s1, shift)
                            r2 (WNode''shift-from-to r2, s2, shift)
                            [#_"node" n1 #_"node" n2 #_"int" delta] (WNode'zip-path shift, r1, c1, r2, (- c2 (alength t2)), 0)
                            #_"int" c1' (+ c1 delta)
                            #_"int" c2' (- c2 (alength t2) delta)
                            [n1 n2] (if (identical? n2 r2) (WNode'squash-nodes shift, n1, c1', n2, c2') [n1 n2])
                        ]
                            (if (some? n2)
                                (let [
                                    #_"array" a' (-> (anew 32) (aset! 0 n1) (aset! 1 n2))
                                    #_"index" x' (-> (anew 33) (aset! 0 c1') (aset! 1 (+ c1' c2')) (aset! 32 2))
                                ]
                                    (PersistentWector'new nil, (+ c1 c2), (+ shift 5), (WNode'new nil, a', x'), t2)
                                )
                                (loop-when-recur [#_"node" node n1 shift shift]
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
    )
)
)

(about #_"arbace.wector"

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
