(ns arbace.arm.core
    (:refer-clojure :only []) (:require [clojure.core :as -])
)

(import!
    [java.lang Appendable Character CharSequence Class Comparable Integer Number Object String StringBuilder System Thread]
    [java.lang.reflect Array Constructor]
    [java.io Flushable PrintWriter PushbackReader Reader]
    [java.util Arrays Comparator]
    [java.util.regex Matcher Pattern]
    [arbace.util.concurrent.atomic AtomicReference]
)

(defn throw! [#_"String" s] (throw (Error. s)))

(defmacro about [& s] (cons 'do s))

(about #_"Numbers"
    (defn +
        ([] (int 0))
        ([x] (int x))
        ([x y] (-/unchecked-add-int (int x) (int y)))
        ([x y & s] (reduce + (+ x y) s))
    )

    (defn -
        ([x] (-/unchecked-negate-int (int x)))
        ([x y] (-/unchecked-subtract-int (int x) (int y)))
        ([x y & s] (reduce - (- x y) s))
    )

    (defn bit-and [x y] (int (-/bit-and x y)))
    (defn bit-or  [x y] (int (-/bit-or x y)))
    (defn bit-xor [x y] (int (-/bit-xor x y)))

    (defn <<  [x y] (int (-/bit-shift-left x y)))
    (defn >>  [x y] (int (-/bit-shift-right x y)))
    (defn >>> [x y] (int (-/unsigned-bit-shift-right (-/bit-and x 0xffffffff) y)))
)

(about #_"java.lang"

(about #_"Appendable"
    (defn #_"Appendable" Appendable''append [#_"Appendable" this, #_"char|CharSequence" x] (.append this, x))
)

(about #_"Character"
    (defn #_"int"       Character'digit        [#_"char" ch, #_"int" radix] (Character/digit ch, radix))
    (defn #_"boolean"   Character'isWhitespace [#_"char" ch]                (Character/isWhitespace ch))
    (defn #_"Character" Character'valueOf      [#_"char" ch]                (Character/valueOf ch))
)

(about #_"CharSequence"
    (defn char-sequence? [x] (-/instance? CharSequence x))

    (defn #_"char" CharSequence''charAt [#_"CharSequence" this, #_"int" i] (.charAt this, i))
    (defn #_"int"  CharSequence''length [#_"CharSequence" this]            (.length this))
)

(about #_"Comparable"
    (defn #_"int" Comparable''compareTo [#_"Comparable" this, #_"any" that] (.compareTo this, that))
)

(about #_"Integer"
    (def #_"int" Integer'MAX_VALUE Integer/MAX_VALUE)
    (def #_"int" Integer'MIN_VALUE Integer/MIN_VALUE)

    (defn #_"int"    Integer'bitCount   [#_"int" i]                (Integer/bitCount i))
    (defn #_"int"    Integer'parseInt   [#_"String" s]             (Integer/parseInt s))
    (defn #_"int"    Integer'rotateLeft [#_"int" x, #_"int" y]     (Integer/rotateLeft x, y))
    (defn #_"String" Integer'toString   [#_"int" i, #_"int" radix] (Integer/toString i, radix))
)

(about #_"Number"
    (defn number? [x] (-/instance? Number x))

    (defn #_"long"   Number''longValue [#_"Number" this] (.longValue this))
    (defn #_"String" Number''toString  [#_"Number" this] (.toString this))
)

(about #_"Object"
    (def Object'array (Class/forName "[Ljava.lang.Object;"))

    (defn #_"int"    Object''hashCode [#_"Object" this] (.hashCode this))
    (defn #_"String" Object''toString [#_"Object" this] (.toString this))
)

(about #_"String"
    (defn string? [x] (-/instance? String x))

    (defn #_"char"    String''charAt     [#_"String" this, #_"int" i]    (.charAt this, i))
    (defn #_"boolean" String''endsWith   [#_"String" this, #_"String" s] (.endsWith this, s))
    (defn #_"int"     String''indexOf   ([#_"String" this, #_"int" ch]   (.indexOf this, ch))     ([#_"String" this, #_"String" s, #_"int" from] (.indexOf this, s, from)))
    (defn #_"String"  String''intern     [#_"String" this]               (.intern this))
    (defn #_"int"     String''length     [#_"String" this]               (.length this))
    (defn #_"boolean" String''startsWith [#_"String" this, #_"String" s] (.startsWith this, s))
    (defn #_"String"  String''substring ([#_"String" this, #_"int" from] (.substring this, from)) ([#_"String" this, #_"int" from, #_"int" over] (.substring this, from, over)))
)

(about #_"StringBuilder"
    (defn #_"StringBuilder" StringBuilder'new [] (StringBuilder.))

    (defn #_"StringBuilder" StringBuilder''append   [#_"StringBuilder" this, #_"char" ch] (.append this, ch))
    (defn #_"String"        StringBuilder''toString [#_"StringBuilder" this]              (.toString this))
)

(about #_"System"
    (defn #_"void" System'arraycopy [#_"array" a, #_"int" i, #_"array" b, #_"int" j, #_"int" n] (System/arraycopy a, i, b, j, n))
)

(about #_"Thread"
    (defn thread [] (Thread/currentThread))
)
)

(about #_"java.lang.reflect"

(about #_"Array"
    (defn array? [x] (.isArray (-/class x)))

    (defn #_"any" Array'get       [#_"array" a, #_"int" i] (Array/get a, i))
    (defn #_"int" Array'getLength [#_"array" a]            (Array/getLength a))
)
)

(about #_"java.io"

(about #_"Flushable"
    (defn #_"void" Flushable''flush [#_"Flushable" this] (.flush this))
)

(about #_"PrintWriter"
    (defn #_"void" PrintWriter''println [#_"PrintWriter" this, #_"String" s] (.println this, s))
)

(about #_"PushbackReader"
    (defn pushback-reader? [x] (-/instance? PushbackReader x))

    (defn #_"PushbackReader" PushbackReader'new [#_"Reader" in] (PushbackReader. in))

    (defn #_"void" PushbackReader''unread [#_"PushbackReader" this, #_"int" x] (.unread this, x))
)

(about #_"Reader"
    (defn #_"int" Reader''read [#_"Reader" this] (.read this))
)
)

(about #_"java.util"

(about #_"Arrays"
    (defn #_"void" Arrays'sort [#_"array" a, #_"Comparator" cmp] (Arrays/sort a, cmp))
)

(about #_"Comparator"
    (defn #_"int" Comparator''compare [#_"Comparator" this, #_"any" x, #_"any" y] (.compare this, x, y))
)
)

(about #_"java.util.regex"

(about #_"Pattern"
    (defn pattern? [x] (-/instance? Pattern x))

    (defn #_"Pattern" Pattern'compile  [#_"String" s]                      (Pattern/compile s))
    (defn #_"Matcher" Pattern''matcher [#_"Pattern" this, #_"CharSequence" s] (.matcher this, s))
    (defn #_"String"  Pattern''pattern [#_"Pattern" this]                     (.pattern this))
)

(about #_"Matcher"
    (defn matcher? [x] (-/instance? Matcher x))

    (defn #_"String"  Matcher''group     ([#_"Matcher" this] (.group this)) ([#_"Matcher" this, #_"int" n] (.group this, n)))
    (defn #_"int"     Matcher''groupCount [#_"Matcher" this] (.groupCount this))
    (defn #_"boolean" Matcher''matches    [#_"Matcher" this] (.matches this))
)
)

(about #_"arbace.util.concurrent.atomic"

(about #_"AtomicReference"
    (defn #_"AtomicReference" AtomicReference'new [#_"any" init] (AtomicReference. init))

    (defn #_"boolean" AtomicReference''compareAndSet [#_"AtomicReference" this, #_"any" x, #_"any" y] (.compareAndSet this, x, y))
    (defn #_"any"     AtomicReference''get           [#_"AtomicReference" this]                       (.get this))
    (defn #_"void"    AtomicReference''set           [#_"AtomicReference" this, #_"any" x]            (.set this, x))
)
)

(defn identical? [a b] (-/identical? a b))

(defn A'new [n] (-/object-array n))

(defn A'clone  [^"[Ljava.lang.Object;" a]     (-/aclone a))
(defn A'get    [^"[Ljava.lang.Object;" a i]   (-/aget a i))
(defn A'length [^"[Ljava.lang.Object;" a]     (-/alength a))
(defn A'set    [^"[Ljava.lang.Object;" a i x] (-/aset a i x))

(defn new* [#_"Class" c & s] (.newInstance #_"Constructor" (first (.getConstructors c)), (A'new s)))

(defn M'get ([m k] (-/get m k)) ([m k not-found] (-/get m k not-found)))

(about #_"arbace.arm.Mutable"
    (defn #_"Mutable" Mutable''mutate! [#_"Mutable" this, #_"key" key, #_"value" val] (.mutate this, key, val))
)

(about #_"arbace.arm.Typed"
    (defn #_"type" Typed''type [#_"Typed" this] (.type this))
)

(defmacro case! [e & clauses] (if (odd? (count clauses)) `(condp = ~e ~@clauses) `(condp = ~e ~@clauses (throw! (str ~e " is definitely not that case!")))))

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
    `(letfn* ~(vec (interleave (map first fnspecs) (map #(cons `fn %) fnspecs))) ~@body)
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

(defmacro if-let
    ([bind then] `(if-let ~bind ~then nil))
    ([bind then else & _]
        `(let-when [x# ~(bind 1)] x# ~'=> ~else
            (let [~(bind 0) x#]
                ~then
            )
        )
    )
)

(defmacro cond-let [bind then & else]
    (let [bind (if (vector? bind) bind [`_# bind])]
        `(if-let ~bind ~then ~(when else `(cond-let ~@else)))
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

(defmacro if-first
    ([bind then] `(if-first ~bind ~then nil))
    ([bind then else & _]
        `(let-when [s# (seq ~(bind 1))] (some? s#) ~'=> ~else
            (let [~(bind 0) (first s#)]
                ~then
            )
        )
    )
)

(letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
    (defmacro when-let   [v & s] (let [[e & s] (=> s)] `(if-let   ~(vec v) (do ~@s) ~e)))
    (defmacro when-some  [v & s] (let [[e & s] (=> s)] `(if-some  ~(vec v) (do ~@s) ~e)))
    (defmacro when-first [v & s] (let [[e & s] (=> s)] `(if-first ~(vec v) (do ~@s) ~e)))
)

(defmacro condp [f? expr & clauses]
    (let [gpred (gensym "pred__") gexpr (gensym "expr__")
          emit-
            (fn emit- [f? expr args]
                (let [[[a b c :as clause] more] (split-at (if (= :>> (second args)) 3 2) args) n (count clause)]
                    (cond
                        (= 0 n) `(throw! (str "no matching clause: " ~expr))
                        (= 1 n) a
                        (= 2 n) `(if (~f? ~a ~expr)
                                    ~b
                                    ~(emit- f? expr more)
                                )
                        :else   `(if-let [p# (~f? ~a ~expr)]
                                    (~c p#)
                                    ~(emit- f? expr more)
                                )
                    )
                )
            )]
        `(let [~gpred ~f? ~gexpr ~expr]
            ~(emit- gpred gexpr clauses)
        )
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

(defmacro while [? & s]
    `(loop [] (when ~? ~@s (recur)))
)

(defmacro doseq [bindings & body]
    (letfn [(emit- [e r]
                (when e => [`(do ~@body) true]
                    (let [[k v & e] e]
                        (if (keyword? k)
                            (let [[f r?] (emit- e r)]
                                (case! k
                                    :let   [`(let ~v ~f) r?]
                                    :while [`(when ~v ~f ~@(when r? [r])) false]
                                    :when  [`(if ~v (do ~f ~@(when r? [r])) ~r) false]
                                )
                            )
                            (let [s (gensym "s__") r `(recur (next ~s)) [f r?] (emit- e r)]
                                [`(loop-when [~s (seq ~v)] ~s (let [~k (first ~s)] ~f ~@(when r? [r]))) true]
                            )
                        )
                    )
                )
            )]
        (first (emit- (seq bindings) nil))
    )
)

(defmacro -> [x & s]
    (when s => x
        (recur &form &env
            (let-when [f (first s)] (seq? f) => (list f x)
                (with-meta `(~(first f) ~x ~@(next f)) (meta f))
            )
            (next s)
        )
    )
)

(defmacro ->> [x & s]
    (when s => x
        (recur &form &env
            (let-when [f (first s)] (seq? f) => (list f x)
                (with-meta `(~(first f) ~@(next f) ~x) (meta f))
            )
            (next s)
        )
    )
)

(defmacro locking [x & body]
    `(let [lockee# ~x]
        (try
            (monitor-enter lockee#)
            ~@body
            (finally
                (monitor-exit lockee#)
            )
        )
    )
)

(about #_"defp, defq, defr, defm"

(about #_"defproto"

(defn gen-interface* [sym]
    (DynamicClassLoader''defineClass (var-get Compiler'LOADER), (str sym), (second (#'-/generate-interface (-/hash-map (-/keyword (-/name :name)) sym))), nil)
)

(defn emit-defproto* [name sigs]
    (let [
        iname (-/symbol (str (-/munge (-/namespace-munge -/*ns*)) "." (-/munge name)))
    ]
        `(do
            (defonce ~name (-/hash-map))
            (gen-interface* '~iname)
            (alter-var-root (var ~name) merge
                ~(-/hash-map :var (list 'var name), :on (list 'quote iname), :on-interface (list `-/resolve (list 'quote iname)))
            )
            ~@(map (fn [[f & _]] `(defmacro ~f [x# & s#] (list* (list -/find-protocol-method '~name ~(-/keyword (str f)) x#) x# s#))) sigs)
            '~name
        )
    )
)

(defmacro defproto [name & sigs]
    (emit-defproto* name sigs)
)
)

(defn parse-opts [s]
    (loop-when-recur [opts (hash-map) [k v & rs :as s] s] (keyword? k) [(-/assoc opts k v) rs] => [opts s])
)

(defn parse-impls [specs]
    (loop-when-recur [impls (hash-map) s specs] (seq s) [(-/assoc impls (first s) (take-while seq? (next s))) (drop-while seq? (next s))] => impls)
)

(defn parse-opts+specs [opts+specs]
    (let [
        [opts specs] (parse-opts opts+specs)
        impls        (parse-impls specs)
        interfaces   (-> (map #(if (#_var? (complement -/class?) (resolve %)) (:on (deref (resolve %))) %) (keys impls)) -/set (-/disj 'Object 'java.lang.Object) vec)
        methods      (map (fn [[name params & body]] (-/cons name (maybe-destructured params body))) (apply concat (vals impls)))
    ]
        [interfaces methods opts]
    )
)

(about #_"arbace.arm.Mutable"
    (defonce Mutable (-/hash-map)) #_alt #_(refer* 'Mutable)
    (DynamicClassLoader''defineClass (var-get Compiler'LOADER), "arbace.arm.core.Mutable", (second (#'-/generate-interface (-/hash-map (-/keyword (-/name :name)) 'arbace.arm.core.Mutable, (-/keyword (-/name :methods)) '[[mutate [java.lang.Object java.lang.Object] java.lang.Object nil]]))), nil)
    (alter-var-root #'Mutable merge (-/hash-map :var #'Mutable, :on 'arbace.arm.core.Mutable, :on-interface (-/resolve (-/symbol "arbace.arm.core.Mutable"))))

    (defn mutable? [x] (satisfies? Mutable x))
)

(about #_"arbace.arm.Typed"
    (defonce Typed (-/hash-map)) #_alt #_(refer* 'Typed)
    (DynamicClassLoader''defineClass (var-get Compiler'LOADER), "arbace.arm.core.Typed", (second (#'-/generate-interface (-/hash-map (-/keyword (-/name :name)) 'arbace.arm.core.Typed, (-/keyword (-/name :methods)) '[[type [] java.lang.Object nil]]))), nil)
    (alter-var-root #'Typed merge (-/hash-map :var #'Typed, :on 'arbace.arm.core.Typed, :on-interface (-/resolve (-/symbol "arbace.arm.core.Typed"))))

    (defn typed? [x] (satisfies? Typed x))
)

(about #_"defarray"

(defn emit-defarray* [tname cname fields interfaces methods opts]
    (let [
        classname  (-/with-meta (-/symbol (str (-/namespace-munge -/*ns*) "." cname)) (meta cname))
        interfaces (vec interfaces)
        fields     (map #(with-meta % nil) fields)
    ]
        (let [a '__array s (mapcat (fn [x y] [(-/name #_keyword y) x]) (range) fields)]
            (letfn [(ilookup [[i m]]
                        [
                            (conj i 'clojure.lang.ILookup)
                            (conj m
                                `(valAt [this# k#] (ILookup''valAt this# k# nil))
                                `(valAt [this# k# else#] (if-some [x# (case! (-/name k#) ~@s nil)] (#_A'get -/aget (. this# ~a) x#) else#))
                            )
                        ]
                    )
                    (mutable [[i m]]
                        [
                            (conj i 'arbace.arm.core.Mutable)
                            (conj m
                                `(mutate [this# k# v#] (let [x# (case! (-/name k#) ~@s)] (#_A'set -/aset (. this# ~a) x# v#) this#))
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

(defmacro defarray [name fields & opts+specs] #_alt #_`(refer* '~name)
    (let [[interfaces methods opts] (parse-opts+specs opts+specs)]
        `(do
            ~(emit-defarray* name name (vec fields) (vec interfaces) methods opts)
            (-/eval '~(-/list (-/symbol "clojure.core/import*") (str (-/namespace-munge -/*ns*) "." name)))
        )
    )
)
)

(about #_"defassoc"

(defn emit-defassoc* [tname cname interfaces methods opts]
    (let [
        classname  (-/with-meta (-/symbol (str (-/namespace-munge -/*ns*) "." cname)) (meta cname))
        interfaces (vec interfaces)
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
                    (iobj [[i m]]
                        [
                            (conj i 'clojure.lang.IObj)
                            (conj m
                                `(meta [this#] (.meta (. this# ~a)))
                                `(withMeta [this# m#] (new ~tname (.withMeta (. this# ~a) m#)))
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
                (let [[i m] (-> [interfaces methods] eqhash iobj ilookup imap typed)]
                    `(-/eval '~(-/read-string (str (list* 'deftype* (symbol (-/name (-/ns-name -/*ns*)) (-/name tname)) classname (vector a) :implements (vec i) m))))
                )
            )
        )
    )
)

(defmacro defassoc [name & opts+specs] #_alt #_`(refer* '~name)
    (let [[interfaces methods opts] (parse-opts+specs opts+specs)]
        `(do
            ~(emit-defassoc* name name (vec interfaces) methods opts)
            (-/eval '~(-/list (-/symbol "clojure.core/import*") (str (-/namespace-munge -/*ns*) "." name)))
        )
    )
)
)

(about #_"extend"

(defn extend [atype & proto+mmaps]
    (doseq [[proto mmap] (partition 2 proto+mmaps)]
        (when-not (#'-/protocol? proto)
            (throw! (str proto " is not a protocol"))
        )
        (when (#'-/implements? proto atype)
            (throw! (str atype " already directly implements " (:on-interface proto) " for protocol " (:var proto)))
        )
        (alter-var-root (:var proto) -/assoc-in [:impls atype] mmap)
    )
)

(defn emit-impl* [_ [p fs]]
    [p (-/zipmap (map #(-> % first -/name -/keyword) fs) (map #(let [% (next %)] (if (= '=> (first %)) (second %) (cons `fn %))) fs))]
)

(defmacro extend-type [t & specs]
    `(extend ~t ~@(mapcat (partial emit-impl* t) (#'-/parse-impls specs)))
)
)

(defmacro defp [p & s]                                      `(do (defproto ~p ~@s)             '~p))
(defmacro defq [r f & s] (let [c (-/symbol (str r "'class"))] `(do (defarray ~c ~(vec f) ~r ~@s) '~c)))
(defmacro defr [r]       (let [c (-/symbol (str r "'class"))] `(do (defassoc ~c ~r)              '~c)))
(defmacro defm [r & s]   (let [i `(:on-interface ~r)]       `(do (extend-type ~i ~@s)          ~i)))
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
        (#_"Object" ISeq'''first [#_"seq" this])
        (#_"seq" ISeq'''next [#_"seq" this])
    )

    (defn seq? [x] (satisfies? ISeq x))

    (defn first [s] (if (seq? s) (ISeq'''first s) (when-some [s (seq s)] (ISeq'''first s))))

    (defn #_"seq" next [s] (if (seq? s) (ISeq'''next s) (when-some [s (seq s)] (ISeq'''next s))))

    (defn second [s] (first (next s)))
    (defn third  [s] (first (next (next s))))
    (defn fourth [s] (first (next (next (next s)))))
    (defn last   [s] (if-some [r (next s)] (recur r) (first s)))
)

(about #_"arbace.arm.IObject"
    (defp IObject
        (#_"boolean" IObject'''equals [#_"IObject" this, #_"Object" that])
    )

    (-/extend-protocol IObject java.lang.Object
        (IObject'''equals [this, that] (.equals this, that))
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

    (-/extend-protocol Comparable java.lang.Comparable
        (Comparable'''compareTo [this, that] (.compareTo this, that))
    )

    (defn comparable? [x] (satisfies? Comparable x))
)

(about #_"arbace.arm.Comparator"
    (defp Comparator
        (#_"int" Comparator'''compare [#_"Comparator" this, #_"any" x, #_"any" y])
    )

    (-/extend-protocol Comparator java.util.Comparator
        (Comparator'''compare [this, x, y] (.compare this, x, y))
    )

    (defn comparator? [x] (satisfies? Comparator x))
)

(about #_"arbace.arm.Counted"
    (defp Counted
        (#_"int" Counted'''count [#_"Counted" this])
    )

    (-/extend-protocol Counted
        java.lang.CharSequence (Counted'''count [s] (.length s))
    )

    (-/extend-protocol Counted
        (do Object'array) (Counted'''count [a] (Array'getLength a))
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
        java.lang.Object (Hashed'''hash [o] (Object''hashCode o))
        java.lang.String (Hashed'''hash [s] (Murmur3'hashInt (Object''hashCode s)))
        java.lang.Number (Hashed'''hash [n] (Murmur3'hashLong (Number''longValue n)))
    )

    (defn hashed? [x] (satisfies? Hashed x))

    (defn f'hash [x] (if (some? x) (Hashed'''hash x) (int 0)))

    (defn f'hashcode [x] (if (some? x) (Object''hashCode x) (int 0)))

    (defn hash-combine [seed x]
        (bit-xor seed (+ (f'hashcode x) (int 0x9e3779b9) (<< seed 6) (>> seed 2)))
    )
)

(about #_"arbace.arm.IFn"
    (defp IFn
        (#_"Object" IFn'''invoke
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
        (#_"Object" IFn'''applyTo [#_"fn" this, #_"seq" args])
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

    (defn complement [f]
        (fn
            ([] (not (f)))
            ([x] (not (f x)))
            ([x y] (not (f x y)))
            ([x y & s] (not (apply f x y s)))
        )
    )
)

(about #_"arbace.arm.INamed"
    (defp INamed
        (#_"String" INamed'''getNamespace [#_"INamed" this])
        (#_"String" INamed'''getName [#_"INamed" this])
    )

    (defn named? [x] (satisfies? INamed x))

    (defn #_"String" namespace [#_"INamed" x] (INamed'''getNamespace x))

    (defn #_"String" name [x] (if (string? x) x (INamed'''getName #_"INamed" x)))
)

(about #_"arbace.arm.IMeta"
    (defp IMeta
        (#_"meta" IMeta'''meta [#_"IMeta" this])
    )

    (defn meta [x] (when (satisfies? IMeta x) (IMeta'''meta #_"IMeta" x)))
)

(about #_"arbace.arm.IObj"
    (defp IObj
        (#_"IObj" IObj'''withMeta [#_"IObj" this, #_"meta" meta])
    )

    (defn with-meta [#_"IObj" x m] (IObj'''withMeta x, m))

    (defn vary-meta [x f & args] (with-meta x (apply f (meta x) args)))
)

(about #_"arbace.arm.IReference"
    (defp IReference
        (#_"meta" IReference'''alterMeta [#_"IReference" this, #_"fn" f, #_"seq" args])
        (#_"meta" IReference'''resetMeta [#_"IReference" this, #_"meta" m])
    )

    (defn alter-meta! [#_"IReference" r f & args] (IReference'''alterMeta r, f, args))

    (defn reset-meta! [#_"IReference" r m] (IReference'''resetMeta r, m))
)

(about #_"arbace.arm.IDeref"
    (defp IDeref
        (#_"Object" IDeref'''deref [#_"IDeref" this])
    )

    (defn deref [#_"IDeref" ref] (IDeref'''deref ref))
)

(about #_"arbace.arm.IAtom"
    (defp IAtom
        (#_"boolean" IAtom'''compareAndSet [#_"IAtom" this, #_"Object" o, #_"Object" o'])
        (#_"Object" IAtom'''swap [#_"IAtom" this, #_"fn" f, #_"seq" args])
        (#_"Object" IAtom'''reset [#_"IAtom" this, #_"Object" o'])
    )
)

(about #_"arbace.arm.IPending"
    (defp IPending
        (#_"boolean" IPending'''isRealized [#_"IPending" this])
    )

    (defn realized? [#_"IPending" x] (IPending'''isRealized x))
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

(about #_"arbace.arm.Sorted"
    (defp Sorted
        (#_"Comparator" Sorted'''comparator [#_"Sorted" this])
        (#_"Object" Sorted'''entryKey [#_"Sorted" this, #_"Object" entry])
        (#_"seq" Sorted'''seq [#_"Sorted" this, #_"boolean" ascending?])
        (#_"seq" Sorted'''seqFrom [#_"Sorted" this, #_"Object" key, #_"boolean" ascending?])
    )

    (defn sorted? [x] (satisfies? Sorted x))
)

(about #_"arbace.arm.Indexed"
    (defp Indexed
        (#_"Object" Indexed'''nth
            [#_"Indexed" this, #_"int" i]
            [#_"Indexed" this, #_"int" i, #_"value" not-found]
        )
    )

    (defn indexed? [x] (satisfies? Indexed x))

    (defn nthnext [s n] (loop-when-recur [s (seq s) n n] (and s (pos? n)) [(next s) (dec n)] => s))
)

(about #_"arbace.arm.ILookup"
    (defp ILookup
        (#_"Object" ILookup'''valAt
            [#_"ILookup" this, #_"key" key]
            [#_"ILookup" this, #_"key" key, #_"value" not-found]
        )
    )
)

(about #_"arbace.arm.IPersistentCollection"
    (defp IPersistentCollection
        (#_"IPersistentCollection" IPersistentCollection'''conj [#_"IPersistentCollection" this, #_"Object" o])
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

(about #_"arbace.arm.IEditableCollection"
    (defp IEditableCollection
        (#_"ITransientCollection" IEditableCollection'''asTransient [#_"IEditableCollection" this])
    )

    (defn editable? [x] (satisfies? IEditableCollection x))

    (defn transient [#_"IEditableCollection" coll] (IEditableCollection'''asTransient coll))
)

(about #_"arbace.arm.IMapEntry"
    (defp IMapEntry
        (#_"Object" IMapEntry'''key [#_"IMapEntry" this])
        (#_"Object" IMapEntry'''val [#_"IMapEntry" this])
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
        (#_"Object" IPersistentSet'''get [#_"IPersistentSet" this, #_"key" key])
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
        (#_"Object" IPersistentStack'''peek [#_"IPersistentStack" this])
        (#_"IPersistentStack" IPersistentStack'''pop [#_"IPersistentStack" this])
    )

    (defn stack? [x] (satisfies? IPersistentStack x))

    (defn peek [s]
        (when (some? s)
            (IPersistentStack'''peek s)
        )
    )

    (defn butlast [s] (loop-when-recur [v (vector) s s] (next s) [(conj v (first s)) (next s)] => (seq v)))

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
        (#_"IPersistentVector" IPersistentVector'''slicev [#_"IPersistentVector" this, #_"int" start, #_"int" end])
        (#_"IPersistentVector" IPersistentVector'''splicev [#_"IPersistentVector" this, #_"IPersistentVector" that])
    )

    (defn vector? [x] (satisfies? IPersistentVector x))
)

(about #_"arbace.arm.ITransientCollection"
    (defp ITransientCollection
        (#_"ITransientCollection" ITransientCollection'''conj! [#_"ITransientCollection" this, #_"value" val])
        (#_"IPersistentCollection" ITransientCollection'''persistent! [#_"ITransientCollection" this])
    )

    (defn conj!
        ([] (transient (vector)))
        ([c] c)
        ([#_"ITransientCollection" c x] (ITransientCollection'''conj! c, x))
        ([c x & s]
            (let [c (conj! c x)]
                (recur-when s [c (first s) (next s)] => c)
            )
        )
    )

    (defn persistent! [#_"ITransientCollection" coll] (ITransientCollection'''persistent! coll))
)

(about #_"arbace.arm.ITransientAssociative"
    (defp ITransientAssociative
        (#_"ITransientAssociative" ITransientAssociative'''assoc! [#_"ITransientAssociative" this, #_"key" key, #_"value" val])
        (#_"boolean" ITransientAssociative'''containsKey [#_"ITransientAssociative" this, #_"key" key])
        (#_"IMapEntry" ITransientAssociative'''entryAt [#_"ITransientAssociative" this, #_"key" key])
    )

    (defn assoc!
        ([#_"ITransientAssociative" a k v] (ITransientAssociative'''assoc! a, k, v))
        ([a k v & kvs]
            (let-when [a (assoc! a k v)] kvs => a
                (when (next kvs) => (throw! "assoc! expects even number of arguments after map/vector, found odd number")
                    (recur a (first kvs) (second kvs) (next (next kvs)))
                )
            )
        )
    )
)

(about #_"arbace.arm.ITransientMap"
    (defp ITransientMap
        (#_"ITransientMap" ITransientMap'''dissoc! [#_"ITransientMap" this, #_"key" key])
    )

    (defn dissoc!
        ([m] m)
        ([#_"ITransientMap" m k] (ITransientMap'''dissoc! m, k))
        ([m k & ks]
            (let [m (dissoc! m k)]
                (recur-when ks [m (first ks) (next ks)] => m)
            )
        )
    )
)

(about #_"arbace.arm.ITransientSet"
    (defp ITransientSet
        (#_"ITransientSet" ITransientSet'''disj! [#_"ITransientSet" this, #_"key" key])
        (#_"boolean" ITransientSet'''contains? [#_"ITransientSet" this, #_"key" key])
        (#_"Object" ITransientSet'''get [#_"ITransientSet" this, #_"key" key])
    )

    (defn disj!
        ([s] s)
        ([#_"ITransientSet" s k] (ITransientSet'''disj! s, k))
        ([s k & ks]
            (let [s (disj! s k)]
                (recur-when ks [s (first ks) (next ks)] => s)
            )
        )
    )
)

(about #_"arbace.arm.ITransientVector"
    (defp ITransientVector
        (#_"ITransientVector" ITransientVector'''assocN! [#_"ITransientVector" this, #_"int" i, #_"value" val])
        (#_"ITransientVector" ITransientVector'''pop! [#_"ITransientVector" this])
    )

    (defn pop! [#_"ITransientVector" coll] (ITransientVector'''pop! coll))
)

(about #_"arbace.arm.IReduce"
    (defp IReduce
        (#_"Object" IReduce'''reduce
            [#_"IReduce" this, #_"fn" f]
            [#_"IReduce" this, #_"fn" f, #_"Object" r]
        )
    )
)

(about #_"arbace.arm.Numbers"
    (defp LongOps)

    (defn integer? [n] (or (int? n) (long? n) false (byte? n)))
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

(about #_"arbace.arm.ArraySeq"
    (defp ArraySeq)
)

(about #_"arbace.arm.ATransientMap"
    #_abstract
    (defp ATransientMap)
)

(about #_"arbace.arm.ATransientSet"
    #_abstract
    (defp ATransientSet)
)

(about #_"arbace.arm.Cons"
    (defp Cons)
)

(about #_"arbace.arm.Iterate"
    (defp Iterate)
)

(about #_"arbace.arm.MapEntry"
    (defp MapEntry)
)

(about #_"arbace.arm.Namespace"
    (defp Namespace)
)

(about #_"arbace.arm.PersistentArrayMap"
    (defp MSeq)
    (defp TransientArrayMap)
    (defp PersistentArrayMap)
)

(about #_"arbace.arm.PersistentHashMap"
    (defp INode
        (#_"INode" INode'''assoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key])
        (#_"IMapEntry|Object" INode'''find
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key]
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" not-found]
        )
        (#_"seq" INode'''nodeSeq [#_"INode" this])
        (#_"INode" INode'''assocT [#_"INode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissocT [#_"INode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf])
    )

    (defp HSeq)
    (defp NSeq)
    (defp TransientHashMap)
    (defp ANode)
    (defp BNode)
    (defp CNode)
    (defp PersistentHashMap)
)

(about #_"arbace.arm.PersistentHashSet"
    (defp TransientHashSet)
    (defp PersistentHashSet)
)

(about #_"arbace.arm.PersistentList"
    (defp EmptyList)
    (defp PersistentList)
)

(about #_"arbace.arm.PersistentTreeMap"
    (defp ITNode
        (#_"ITNode" ITNode'''addLeft [#_"ITNode" this, #_"ITNode" ins])
        (#_"ITNode" ITNode'''addRight [#_"ITNode" this, #_"ITNode" ins])
        (#_"ITNode" ITNode'''removeLeft [#_"ITNode" this, #_"ITNode" del])
        (#_"ITNode" ITNode'''removeRight [#_"ITNode" this, #_"ITNode" del])
        (#_"ITNode" ITNode'''blacken [#_"ITNode" this])
        (#_"ITNode" ITNode'''redden [#_"ITNode" this])
        (#_"ITNode" ITNode'''balanceLeft [#_"ITNode" this, #_"ITNode" parent])
        (#_"ITNode" ITNode'''balanceRight [#_"ITNode" this, #_"ITNode" parent])
        (#_"ITNode" ITNode'''replace [#_"ITNode" this, #_"key" key, #_"value" val, #_"ITNode" left, #_"ITNode" right])
    )

    #_abstract
    (defp TNode)
    (defp Black)
    (defp BlackVal)
    (defp BlackBranch)
    (defp BlackBranchVal)
    (defp Red)
    (defp RedVal)
    (defp RedBranch)
    (defp RedBranchVal)
    (defp TSeq)
    (defp PersistentTreeMap)
)

(about #_"arbace.arm.PersistentTreeSet"
    (defp PersistentTreeSet)
)

(about #_"arbace.arm.PersistentVector"
    (defp VNode)
    (defp TransientVector)
    (defp PersistentVector)
)

(about #_"arbace.arm.Repeat"
    (defp Repeat)
)

(about #_"arbace.arm.Range"
    (defp Range)
)

(about #_"arbace.arm.Reduced"
    (defp Reduced)

    (defn reduced? [x] (satisfies? Reduced x))
)

(about #_"arbace.arm.StringSeq"
    (defp StringSeq)
)

(about #_"arbace.arm.Var"
    (defp Unbound)
    (defp Var)

    (defn var? [v] (satisfies? Var v))
)

(about #_"defarray"
    (defn aget    [a i] (A'get a i))
    (defn alength [a]   (A'length a))

    (defn aclone [a]         (when (some? a) (A'clone a)))
    (defn acopy! [a i b j n] (System'arraycopy b, j, a, i, n) a)
    (defn aset!  [a i x]     (A'set a i x) a)
    (defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

    (defn anew [size-or-seq]
        (if (number? size-or-seq)
            (A'new (int size-or-seq))
            (let [#_"seq" s (seq size-or-seq) #_"int" n (count s)]
                (loop-when-recur [#_"array" a (A'new n) #_"int" i 0 s s] (and (< i n) (some? s)) [(aset! a i (first s)) (inc i) (next s)] => a)
            )
        )
    )

    (defn qset!
        ([a k v]    (Mutable''mutate! a, k, v))
        ([a k v & kvs]
            (let [a (Mutable''mutate! a, k, v)]
                (recur-when kvs [a (first kvs) (second kvs) (next (next kvs))] => a)
            )
        )
    )

    (defn qswap!
        ([a k f]         (Mutable''mutate! a, k,       (f (ILookup''valAt a, k))))
        ([a k f x]       (Mutable''mutate! a, k,       (f (ILookup''valAt a, k) x)))
        ([a k f x y]     (Mutable''mutate! a, k,       (f (ILookup''valAt a, k) x y)))
        ([a k f x y & z] (Mutable''mutate! a, k, (apply f (ILookup''valAt a, k) x y z)))
    )
)

(about #_"append, str, pr, prn"
    (def #_"{char String}" char-name-string
        (-/hash-map
            \newline   "newline"
            \tab       "tab"
            \space     "space"
            \backspace "backspace"
            \formfeed  "formfeed"
            \return    "return"
        )
    )

    (defn #_"Appendable" append-chr [#_"Appendable" a, #_"char" x]
        (-> a (Appendable''append "\\") (Appendable''append (M'get char-name-string x x)))
    )

    (def #_"{char String}" char-escape-string
        (-/hash-map
            \newline   "\\n"
            \tab       "\\t"
            \return    "\\r"
            \"         "\\\""
            \\         "\\\\"
            \formfeed  "\\f"
            \backspace "\\b"
        )
    )

    (defn #_"Appendable" append-str [#_"Appendable" a, #_"String" x]
        (let [
            a (Appendable''append a, "\"")
            a (-/reduce #(Appendable''append %1, (M'get char-escape-string %2 %2)) a x)
            a (Appendable''append a, "\"")
        ]
            a
        )
    )

    (defn #_"Appendable" append-rex [#_"Appendable" a, #_"Pattern" x]
        (let [
            a (Appendable''append a, "#\"")
            a
                (loop-when [a a [#_"char" c & #_"seq" r :as #_"seq" s] (seq (Pattern''pattern x)) q? false] (some? s) => a
                    (case! c
                        \\  (let [[c & r] r] (recur (-> a (Appendable''append "\\") (Appendable''append c)) r (if q? (not= c \E) (= c \Q))))
                        \"                   (recur (-> a (Appendable''append (if q? "\\E\\\"\\Q" "\\\""))) r q?)
                                             (recur (-> a (Appendable''append c))                           r q?)
                    )
                )
            a (Appendable''append a, "\"")
        ]
            a
        )
    )

    (defp SeqForm)
    (defp VecForm)
    (defp MapForm)
    (defp SetForm)

    (defn #_"Appendable" append* [#_"Appendable" a, #_"String" b, #_"fn" f'append, #_"String" c, #_"String" d, #_"Seqable" q]
        (let [a (let-when [a (Appendable''append a, b) #_"seq" s (seq q)] (some? s) => a
                    (loop [a a s s]
                        (let-when [a (f'append a (first s)) s (next s)] (some? s) => a
                            (recur (Appendable''append a, c) s)
                        )
                    )
                )]
            (Appendable''append a, d)
        )
    )

    (defn #_"Appendable" append-seq [#_"Appendable" a, #_"seq" x]    (append* a "(" append " " ")" x))
    (defn #_"Appendable" append-vec [#_"Appendable" a, #_"vector" x] (append* a "[" append " " "]" x))
    (defn #_"Appendable" append-map [#_"Appendable" a, #_"map" x]    (append* a "{" (fn [a e] (-> a (append (key e)) (Appendable''append " ") (append (val e)))) ", " "}" x))
    (defn #_"Appendable" append-set [#_"Appendable" a, #_"set" x]    (append* a "#{" append " " "}" x))

    (defn #_"Appendable" append [#_"Appendable" a, #_"any" x]
        (case! x
            nil   (Appendable''append a, "nil")
            false (Appendable''append a, "false")
            true  (Appendable''append a, "true")
            (cond
                (number? x) (Appendable''append a, (Number''toString x))
                (string? x) (append-str a x)
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
                        (pattern? x) (append-rex a x)
                        :else        (Appendable''append a, (Object''toString x))
                    )
                )
            )
        )
    )

    (defn #_"Appendable" append! [#_"Appendable" a, #_"any" x]
        (if (or (char-sequence? x) (char? x)) (Appendable''append a, x) (append a x))
    )

    (defn #_"String" str
        ([] "")
        ([x] (if (some? x) (-> (StringBuilder'new) (append! x) (StringBuilder''toString)) ""))
        ([x & s]
            ((fn [#_"StringBuilder" sb s] (recur-when s [(append! sb (first s)) (next s)] => (StringBuilder''toString sb)))
                (-> (StringBuilder'new) (append! x)) s
            )
        )
    )

    (defn space   [] (Appendable''append -/*out* \space)   nil)
    (defn newline [] (Appendable''append -/*out* \newline) nil)
    (defn flush   [] (Flushable''flush   -/*out*)          nil)

    (defn pr
        ([] nil)
        ([x] (append -/*out* x) nil)
        ([x & s]
            (pr x) (space)
            (let-when [[x & s] s] (some? s) => (pr x)
                (recur x s)
            )
        )
    )

    (defn print
        ([] nil)
        ([x] (append! -/*out* x) nil)
        ([x & s]
            (print x) (space)
            (let-when [[x & s] s] (some? s) => (print x)
                (recur x s)
            )
        )
    )

    (defn prn     [& s] (apply pr    s) (newline) (flush) nil)
    (defn println [& s] (apply print s) (newline) (flush) nil)
)

(about #_"arbace.arm.Murmur3"

(about #_"Murmur3"
    (def #_"int" Murmur3'seed (int 0))
    (def #_"int" Murmur3'C1 (int 0xcc9e2d51))
    (def #_"int" Murmur3'C2 (int 0x1b873593))

    (defn #_"int" Murmur3'mixK1 [#_"int" k1]
        (-> k1 (* Murmur3'C1) (Integer'rotateLeft 15) (* Murmur3'C2))
    )

    (defn #_"int" Murmur3'mixH1 [#_"int" h1, #_"int" k1]
        (-> h1 (bit-xor k1) (Integer'rotateLeft 13) (* (int 5)) (+ (int 0xe6546b64)))
    )

    (defn #_"int" Murmur3'fmix [#_"int" h1, #_"int" n]
        (let [h1 (bit-xor h1 n)    h1 (bit-xor h1 (>>> h1 16))
              h1 (* (int h1) (int 0x85ebca6b)) h1 (bit-xor h1 (>>> h1 13))
              h1 (* (int h1) (int 0xc2b2ae35)) h1 (bit-xor h1 (>>> h1 16))]
            h1
        )
    )

    (defn #_"int" Murmur3'hashInt [#_"int" input]
        (when-not (zero? input) => (int 0)
            (let [#_"int" k1 (Murmur3'mixK1 input)
                  #_"int" h1 (Murmur3'mixH1 Murmur3'seed, k1)]
                (Murmur3'fmix h1, (int 4))
            )
        )
    )

    (defn #_"int" Murmur3'hashLong [#_"long" input]
        (when-not (zero? input) => (int 0)
            (let [#_"int" low (int input)
                  #_"int" high (int (>>> input 32))
                  #_"int" k1 (Murmur3'mixK1 low)
                  #_"int" h1 (Murmur3'mixH1 Murmur3'seed, k1)
                  k1 (Murmur3'mixK1 high)
                  h1 (Murmur3'mixH1 h1, k1)]
                (Murmur3'fmix h1, (int 8))
            )
        )
    )

    (defn #_"int" Murmur3'hashUnencodedChars [#_"CharSequence" s]
        (let [#_"int" h1
                (loop-when [h1 Murmur3'seed #_"int" i 1] (< i (CharSequence''length s)) => h1
                    (let [#_"int" k1 (bit-or (int (CharSequence''charAt s, (dec i))) (<< (int (CharSequence''charAt s, i)) 16))]
                        (recur (Murmur3'mixH1 h1, (Murmur3'mixK1 k1)) (+ i 2))
                    )
                )
              h1
                (when (odd? (CharSequence''length s)) => h1
                    (let [#_"int" k1 (int (CharSequence''charAt s, (dec (CharSequence''length s))))]
                        (bit-xor h1 (Murmur3'mixK1 k1))
                    )
                )]
            (Murmur3'fmix h1, (<< (CharSequence''length s) 1))
        )
    )

    (defn #_"int" Murmur3'mixCollHash [#_"int" hash, #_"int" n]
        (Murmur3'fmix (Murmur3'mixH1 Murmur3'seed, (Murmur3'mixK1 hash)), n)
    )

    (defn #_"int" Murmur3'hashOrdered [#_"Seqable" items]
        (loop-when-recur [#_"int" hash (int 1) #_"int" n (int 0) #_"seq" s (seq items)]
                         (some? s)
                         [(+ (* (int 31) hash) (f'hash (first s))) (inc n) (next s)]
                      => (Murmur3'mixCollHash hash, n)
        )
    )

    (defn #_"int" Murmur3'hashUnordered [#_"Seqable" items]
        (loop-when-recur [#_"int" hash (int 0) #_"int" n (int 0) #_"seq" s (seq items)]
                         (some? s)
                         [(+ hash (f'hash (first s))) (inc n) (next s)]
                      => (Murmur3'mixCollHash hash, n)
        )
    )
)
)

(about #_"arbace.arm.Atom"

(about #_"Atom"
    (defq Atom [#_"AtomicReference" meta, #_"AtomicReference" data])

    (defn #_"Atom" Atom'new
        ([#_"Object" data] (Atom'new nil, data))
        ([#_"meta" meta, #_"Object" data]
            (new* Atom'class (anew [(AtomicReference'new meta), (AtomicReference'new data)]))
        )
    )

    (defn #_"meta" Atom''meta [#_"Atom" this]
        (AtomicReference''get (:meta this))
    )

    (defn #_"meta" Atom''alterMeta [#_"Atom" this, #_"fn" f, #_"seq" args]
        (loop []
            (let [#_"meta" m (AtomicReference''get (:meta this)) #_"meta" m' (apply f m args)]
                (when (AtomicReference''compareAndSet (:meta this), m, m') => (recur)
                    m'
                )
            )
        )
    )

    (defn #_"meta" Atom''resetMeta [#_"Atom" this, #_"meta" m']
        (AtomicReference''set (:meta this), m')
        m'
    )

    (defn #_"Object" Atom''deref [#_"Atom" this]
        (AtomicReference''get (:data this))
    )

    (defn #_"boolean" Atom''compareAndSet [#_"Atom" this, #_"Object" o, #_"Object" o']
        (AtomicReference''compareAndSet (:data this), o, o')
    )

    (defn #_"Object" Atom''swap [#_"Atom" this, #_"fn" f, #_"seq" args]
        (loop []
            (let [#_"Object" o (AtomicReference''get (:data this)) #_"Object" o' (apply f o args)]
                (when (AtomicReference''compareAndSet (:data this), o, o') => (recur)
                    o'
                )
            )
        )
    )

    (defn #_"Object" Atom''reset [#_"Atom" this, #_"Object" o']
        (AtomicReference''set (:data this), o')
        o'
    )

    (defm Atom IMeta
        (IMeta'''meta => Atom''meta)
    )

    (defm Atom IReference
        (IReference'''alterMeta => Atom''alterMeta)
        (IReference'''resetMeta => Atom''resetMeta)
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

(defn atom
    ([x] (Atom'new x))
    ([m x] (Atom'new m x))
)

(defn compare-and-set! [#_"IAtom" a x x'] (IAtom'''compareAndSet a, x, x'))

(defn swap! [#_"IAtom" a f & args] (IAtom'''swap a, f, args))

(defn reset! [#_"IAtom" a x'] (IAtom'''reset a, x'))
)

(about #_"arbace.arm.Reduced"

(about #_"Reduced"
    (defq Reduced [#_"Object" val])

    (defn #_"Reduced" Reduced'new [#_"Object" val]
        (new* Reduced'class (anew [val]))
    )

    (defm Reduced IDeref
        (IDeref'''deref => :val)
    )
)

(defn reduced [x] (Reduced'new x))

(defn ensure-reduced [x] (if (reduced? x) x (reduced x)))

(defn unreduced [x] (if (reduced? x) (deref x) x))

(defn preserving-reduced [f] #(let [r (f %1 %2)] (if (reduced? r) (reduced r) r)))

(defn reduce
    ([f s] (if-some [s (seq s)] (reduce f (first s) (next s)) (f)))
    ([f r s] (if-some [s (seq s)] (recur f (f r (first s)) (next s)) r))
)

(defn reduce!
    ([f s] (if-some [s (seq s)] (reduce! f (first s) (next s)) (f)))
    ([f r s] (persistent! (reduce f (transient r) s)))
)

(defn cat [f]
    (let [g (preserving-reduced f)]
        (fn
            ([] (f))
            ([s] (f s))
            ([s x] (reduce g s x))
        )
    )
)

(defn into [to from]
    (if (editable? to)
        (reduce! conj! to from)
        (reduce conj to from)
    )
)

(defn mapv
    ([f coll] (reduce! #(conj! %1 (f %2)) (vector) coll))
    ([f c1 c2] (into (vector) (map f c1 c2)))
    ([f c1 c2 c3] (into (vector) (map f c1 c2 c3)))
    ([f c1 c2 c3 & colls] (into (vector) (apply map f c1 c2 c3 colls)))
)
)

(about #_"arbace.arm.Util"

(about #_"Util"
    (defn #_"boolean" Util'equiv [#_"Object" a, #_"Object" b]
        (cond
            (identical? a b)              true
            (nil? a)                      false
            (and (number? a) (number? b)) #_(Numbers'equal a, b) (-/== a b)
            (coll? a)                     (IObject'''equals a, b)
            (coll? b)                     (IObject'''equals b, a)
            (-/instance? (:on-interface Symbol) a)  (Symbol''equals a, b)
            (-/instance? (:on-interface Symbol) b)  (Symbol''equals b, a)
            (-/instance? (:on-interface Keyword) a) (Keyword''equals a, b)
            (-/instance? (:on-interface Keyword) b) (Keyword''equals b, a)
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
    (defn #_"int" Util'compare [#_"Object" a, #_"Object" b]
        (cond
            (= a b)     0
            (nil? a)   -1
            (nil? b)    1
            (number? a) #_(Numbers'compare a, #_"Number" b) (-/compare a b)
            :else       (Comparable''compareTo a, b)
        )
    )
)

(defn compare [x y] (Util'compare x, y))
)

(about #_"arbace.arm.Numbers"

(about #_"LongOps"
    (defq LongOps [])

    (defn #_"LongOps" LongOps'new []
        (new* LongOps'class (anew []))
    )

    (defn #_"boolean" LongOps''eq  [#_"Number" x, #_"Number" y] (-/=  (Number''longValue x) (Number''longValue y)))
    (defn #_"boolean" LongOps''lt  [#_"Number" x, #_"Number" y] (-/<  (Number''longValue x) (Number''longValue y)))
    (defn #_"boolean" LongOps''lte [#_"Number" x, #_"Number" y] (-/<= (Number''longValue x) (Number''longValue y)))

    (defn #_"boolean" LongOps''isZero [#_"Number" x] (-/= (Number''longValue x) 0))
    (defn #_"boolean" LongOps''isPos  [#_"Number" x] (-/> (Number''longValue x) 0))
    (defn #_"boolean" LongOps''isNeg  [#_"Number" x] (-/< (Number''longValue x) 0))

    (defn #_"Number" LongOps''add [#_"Number" x, #_"Number" y] (Long'valueOf (-/unchecked-add-int (Number''longValue x) (Number''longValue y))))

    (defn #_"Number" LongOps''negate [#_"Number" x] (Long'valueOf (-/unchecked-negate-int (Number''longValue x))))

    (defn #_"Number" LongOps''inc [#_"Number" x] (Long'valueOf (-/unchecked-inc-int (Number''longValue x))))
    (defn #_"Number" LongOps''dec [#_"Number" x] (Long'valueOf (-/unchecked-dec-int (Number''longValue x))))

    (defn #_"Number" LongOps''multiply [#_"Number" x, #_"Number" y]
        (Long'valueOf (-/unchecked-multiply-int (Number''longValue x) (Number''longValue y)))
    )

    (defn #_"Number" LongOps''quotient  [#_"Number" x, #_"Number" y] (Long'valueOf (-/unchecked-divide-int    (Number''longValue x) (Number''longValue y))))
    (defn #_"Number" LongOps''remainder [#_"Number" x, #_"Number" y] (Long'valueOf (-/unchecked-remainder-int (Number''longValue x) (Number''longValue y))))
)

(about #_"Numbers"
    (defn #_"int" Numbers'compare [#_"Number" x, #_"Number" y]
        (cond (LongOps''lt x, y) -1 (LongOps''lt y, x) 1 :else 0)
    )

    (defn #_"boolean" Numbers'equal [#_"Number" x, #_"Number" y] (LongOps''eq x, y))

    (defn #_"boolean" Numbers'lt [#_"Number" x, #_"Number" y]  (LongOps''lt  x, y))
    (defn #_"boolean" Numbers'lte [#_"Number" x, #_"Number" y] (LongOps''lte x, y))
    (defn #_"boolean" Numbers'gt [#_"Number" x, #_"Number" y]  (LongOps''lt  y, x))
    (defn #_"boolean" Numbers'gte [#_"Number" x, #_"Number" y] (LongOps''lte y, x))

    (defn #_"boolean" Numbers'isZero [#_"Number" x] (LongOps''isZero x))
    (defn #_"boolean" Numbers'isPos  [#_"Number" x] (LongOps''isPos  x))
    (defn #_"boolean" Numbers'isNeg  [#_"Number" x] (LongOps''isNeg  x))

    (defn #_"Number" Numbers'add      [#_"Number" x, #_"Number" y] (LongOps''add x, y))
    (defn #_"Number" Numbers'subtract [#_"Number" x, #_"Number" y] (LongOps''add x, (LongOps''negate y)))

    (defn #_"Number" Numbers'negate [#_"Number" x] (LongOps''negate x))
    (defn #_"Number" Numbers'inc    [#_"Number" x] (LongOps''inc    x))
    (defn #_"Number" Numbers'dec    [#_"Number" x] (LongOps''dec    x))

    (defn #_"Number" Numbers'multiply [#_"Number" x, #_"Number" y] (LongOps''multiply x, y))

    (defn #_"Number" Numbers'divide [#_"Number" x, #_"Number" y]
        (when-not (LongOps''isZero y) => (throw! "divide by zero")
            (LongOps''quotient x, y)
        )
    )

    (defn #_"Number" Numbers'quotient [#_"Number" x, #_"Number" y]
        (when-not (LongOps''isZero y) => (throw! "divide by zero")
            (LongOps''quotient x, y)
        )
    )

    (defn #_"Number" Numbers'remainder [#_"Number" x, #_"Number" y]
        (when-not (LongOps''isZero y) => (throw! "divide by zero")
            (LongOps''remainder x, y)
        )
    )

    (defn #_"long" Numbers'bitOpsCast [#_"Number" x]
        (when (or (long? x) (int? x) (byte? x)) => (throw! (str "bit operation not supported on " x))
            (long x)
        )
    )

    (defn #_"long" Numbers'not [#_"Number" x] (-/bit-not (Numbers'bitOpsCast x)))

    (defn #_"long" Numbers'and [#_"Number" x, #_"Number" y] (-/bit-and (Numbers'bitOpsCast x) (Numbers'bitOpsCast y)))
    (defn #_"long" Numbers'or  [#_"Number" x, #_"Number" y] (-/bit-or (Numbers'bitOpsCast x) (Numbers'bitOpsCast y)))
    (defn #_"long" Numbers'xor [#_"Number" x, #_"Number" y] (-/bit-xor (Numbers'bitOpsCast x) (Numbers'bitOpsCast y)))

    (defn #_"long" Numbers'andNot [#_"Number" x, #_"Number" y] (-/bit-and (Numbers'bitOpsCast x) (-/bit-not (Numbers'bitOpsCast y))))

    (defn #_"long" Numbers'shiftLeft          [#_"Number" x, #_"Number" n] (-/bit-shift-left (Numbers'bitOpsCast x) (Numbers'bitOpsCast n)))
    (defn #_"long" Numbers'shiftRight         [#_"Number" x, #_"Number" n] (-/bit-shift-right (Numbers'bitOpsCast x) (Numbers'bitOpsCast n)))
    (defn #_"long" Numbers'unsignedShiftRight [#_"Number" x, #_"Number" n] (-/unsigned-bit-shift-right (Numbers'bitOpsCast x) (Numbers'bitOpsCast n)))

    (defn #_"long" Numbers'clearBit [#_"Number" x, #_"Number" n] (-/bit-and (Numbers'bitOpsCast x) (-/bit-not (-/bit-shift-left 1 (Numbers'bitOpsCast n)))))
    (defn #_"long" Numbers'setBit   [#_"Number" x, #_"Number" n] (-/bit-or (Numbers'bitOpsCast x) (-/bit-shift-left 1 (Numbers'bitOpsCast n))))
    (defn #_"long" Numbers'flipBit  [#_"Number" x, #_"Number" n] (-/bit-xor (Numbers'bitOpsCast x) (-/bit-shift-left 1 (Numbers'bitOpsCast n))))

    (defn #_"boolean" Numbers'testBit [#_"Number" x, #_"Number" n] (-/not= (-/bit-and (Numbers'bitOpsCast x) (-/bit-shift-left 1 (Numbers'bitOpsCast n))) 0))
)

(defn <
    ([x] true)
    ([x y] (Numbers'lt x y))
    ([x y & s] (and (< x y) (recur-when (next s) [y (first s) (next s)] => (< y (first s)))))
)

(defn <=
    ([x] true)
    ([x y] (Numbers'lte x y))
    ([x y & s] (and (<= x y) (recur-when (next s) [y (first s) (next s)] => (<= y (first s)))))
)

(defn >
    ([x] true)
    ([x y] (Numbers'gt x y))
    ([x y & s] (and (> x y) (recur-when (next s) [y (first s) (next s)] => (> y (first s)))))
)

(defn >=
    ([x] true)
    ([x y] (Numbers'gte x y))
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

(defn zero? [n] (Numbers'isZero n))
(defn pos?  [n] (Numbers'isPos  n))
(defn neg?  [n] (Numbers'isNeg  n))

(§ defn +
    ([] 0)
    ([x] #_"Number" x)
    ([x y] (Numbers'add x y))
    ([x y & s] (reduce + (+ x y) s))
)

(§ defn -
    ([x] (Numbers'negate x))
    ([x y] (Numbers'subtract x y))
    ([x y & s] (reduce - (- x y) s))
)

(defn abs [a] (if (neg? a) (- a) a))

(defn inc [x] (Numbers'inc x))
(defn dec [x] (Numbers'dec x))

(defn *
    ([] 1)
    ([x] #_"Number" x)
    ([x y] (Numbers'multiply x y))
    ([x y & s] (reduce * (* x y) s))
)

(defn quot [num div] (Numbers'quotient num div))
(defn rem [num div] (Numbers'remainder num div))

(defn mod [num div]
    (let-when [m (rem num div)] (or (zero? m) (= (pos? num) (pos? div))) => (+ m div)
        m
    )
)

(defn bit-not [x] (Numbers'not x))

(§ defn bit-and
    ([x y] (Numbers'and x y))
    ([x y & s] (reduce bit-and (bit-and x y) s))
)

(§ defn bit-or
    ([x y] (Numbers'or x y))
    ([x y & s] (reduce bit-or (bit-or x y) s))
)

(§ defn bit-xor
    ([x y] (Numbers'xor x y))
    ([x y & s] (reduce bit-xor (bit-xor x y) s))
)

(defn bit-and-not
    ([x y] (Numbers'andNot x y))
    ([x y & s] (reduce bit-and-not (bit-and-not x y) s))
)

(defn bit-clear [x i] (Numbers'clearBit x i))
(defn bit-set   [x i] (Numbers'setBit   x i))
(defn bit-flip  [x i] (Numbers'flipBit  x i))
(defn bit-test  [x i] (Numbers'testBit  x i))

(defn          bit-shift-left  [x n] (Numbers'shiftLeft          x n))
(defn          bit-shift-right [x n] (Numbers'shiftRight         x n))
(defn unsigned-bit-shift-right [x n] (Numbers'unsignedShiftRight x n))

(defn even? [n]
    (when (integer? n) => (throw! (str "argument must be an integer: " n))
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

    (defn #_"Object" AFn'applyTo [#_"fn" f, #_"seq" s]
        (case! (count s (inc 9))
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
    (defq Symbol [#_"meta" _meta, #_"String" ns, #_"String" name])

    #_inherit
    (defm Symbol AFn)

    (defn #_"Symbol" Symbol'new
        ([#_"String" ns, #_"String" name] (Symbol'new nil, ns, name))
        ([#_"meta" meta, #_"String" ns, #_"String" name]
            (new* Symbol'class (anew [meta, ns, name]))
        )
    )

    (defn #_"Symbol" Symbol''withMeta [#_"Symbol" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Symbol'new meta, (:ns this), (:name this))
        )
    )

    (defn #_"Symbol" Symbol'intern
        ([#_"String" nsname]
            (let [#_"int" i (String''indexOf nsname, (int \/))]
                (if (or (= i -1) (= nsname "/"))
                    (Symbol'new nil, nsname)
                    (Symbol'new (String''substring nsname, 0, i), (String''substring nsname, (inc i)))
                )
            )
        )
        ([#_"String" ns, #_"String" name]
            (Symbol'new ns, name)
        )
    )

    (defn #_"boolean" Symbol''equals [#_"Symbol" this, #_"Object" that]
        (or (identical? this that)
            (and (symbol? that) (= (:ns this) (#_:ns namespace that)) (= (:name this) (#_:name name that)))
        )
    )

    (defn #_"Appendable" Symbol''append [#_"Symbol" this, #_"Appendable" a]
        (if (some? (:ns this)) (-> a (Appendable''append (:ns this)) (Appendable''append "/") (Appendable''append (:name this))) (Appendable''append a, (:name this)))
    )

    (defn #_"int" Symbol''hash [#_"Symbol" this]
        (hash-combine (Murmur3'hashUnencodedChars (:name this)) (:ns this))
    )

    (defn #_"Object" Symbol''invoke
        ([#_"Symbol" this, #_"Object" obj] (get obj this))
        ([#_"Symbol" this, #_"Object" obj, #_"value" not-found] (get obj this not-found))
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

    (defm Symbol IMeta
        (IMeta'''meta => :_meta)
    )

    (defm Symbol IObj
        (IObj'''withMeta => Symbol''withMeta)
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

(-/defmethod -/print-method (:on-interface Symbol) [o w] (.write w, (str o)))
)

(about #_"arbace.arm.Keyword"

(about #_"Keyword"
    (defq Keyword [#_"Symbol" sym, #_"int" _hash])

    #_inherit
    (defm Keyword AFn)

    (defn #_"Keyword" Keyword'new [#_"Symbol" sym]
        (new* Keyword'class (anew [sym, (+ (f'hash sym) (int 0x9e3779b9))]))
    )

    (defn #_"Keyword" Keyword'intern [#_"Symbol" sym]
        (let [sym
                (when (some? (meta sym)) => sym
                    (with-meta sym nil)
                )]
            (Keyword'new sym)
        )
    )

    (defn #_"String" Keyword''getNamespace [#_"Keyword" this]
        (INamed'''getNamespace (:sym this))
    )

    (defn #_"String" Keyword''getName [#_"Keyword" this]
        (INamed'''getName (:sym this))
    )

    (defn #_"boolean" Keyword''equals [#_"Keyword" this, #_"Object" that]
        (or (identical? this that)
            (Symbol''equals (:sym this), (:sym that))
        )
    )

    (defn #_"Appendable" Keyword''append [#_"Keyword" this, #_"Appendable" a]
        (-> a (Appendable''append ":") (append (:sym this)))
    )

    (defn #_"Object" Keyword''invoke
        ([#_"Keyword" this, #_"Object" obj] (get obj this))
        ([#_"Keyword" this, #_"Object" obj, #_"value" not-found] (get obj this not-found))
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
            (string? name) (Keyword'intern (symbol #_"String" name))
        )
    )
    ([ns name] (Keyword'intern (symbol ns name)))
)

(-/defmethod -/print-method (:on-interface Keyword) [o w] (.write w, (str o)))
)

(about #_"arbace.arm.Fn"

(about #_"Fn"
    (defq Fn [])

    #_inherit
    (defm Fn AFn)

    (defn #_"Fn" Fn'new []
        (new* Fn'class (anew []))
    )

    (defn #_"Object" Fn''invoke
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

    (defn #_"int" Fn''compare [#_"Fn" this, #_"Object" o1, #_"Object" o2]
        (let [#_"Object" o (IFn'''invoke this, o1, o2)]
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
    (defq Closure [#_"meta" _meta, #_"FnExpr" fun, #_"map'" _env])

    #_inherit
    (defm Closure Fn AFn)

    (defn #_"Closure" Closure'new
        ([#_"FnExpr" fun, #_"map" env] (Closure'new nil, fun, env))
        ([#_"meta" meta, #_"FnExpr" fun, #_"map" env]
            (new* Closure'class (anew [meta, fun, (atom env)]))
        )
    )

    (defn #_"Closure" Closure''withMeta [#_"Closure" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (new* Closure'class (anew [meta, (:fun this), (:_env this)]))
        )
    )

    (defn #_"Object" Closure''invoke
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

    (defn #_"Object" Closure''applyTo [#_"Closure" this, #_"seq" args]
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

    (defm Closure IMeta
        (IMeta'''meta => :_meta)
    )

    (defm Closure IObj
        (IObj'''withMeta => Closure''withMeta)
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
    (defn #_"boolean" ASeq''equals [#_"ASeq" this, #_"Object" that]
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
    (defq Cons [#_"meta" _meta, #_"Object" car, #_"seq" cdr] SeqForm)

    #_inherit
    (defm Cons ASeq)

    (defn #_"Cons" Cons'new
        ([#_"Object" car, #_"seq" cdr] (Cons'new nil, car, cdr))
        ([#_"meta" meta, #_"Object" car, #_"seq" cdr]
            (new* Cons'class (anew [meta, car, cdr]))
        )
    )

    (defn #_"Cons" Cons''withMeta [#_"Cons" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Cons'new meta, (:car this), (:cdr this))
        )
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

    (defm Cons IMeta
        (IMeta'''meta => :_meta)
    )

    (defm Cons IObj
        (IObj'''withMeta => Cons''withMeta)
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

(about #_"arbace.arm.Iterate"

(about #_"Iterate"
    (defq Iterate [#_"meta" _meta, #_"fn" f, #_"Object" x, #_"Object'" y] SeqForm)

    #_inherit
    (defm Iterate ASeq)

    (defn #_"Iterate" Iterate'new
        ([#_"fn" f, #_"Object" x, #_"Object" y] (Iterate'new nil, f, x, y))
        ([#_"meta" meta, #_"fn" f, #_"Object" x, #_"Object" y]
            (new* Iterate'class (anew [meta, f, x, (atom y)]))
        )
    )

    (defn #_"Iterate" Iterate''withMeta [#_"Iterate" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Iterate'new meta, (:f this), (:x this), (deref (:y this)))
        )
    )

    (defn #_"seq" Iterate'create [#_"fn" f, #_"Object" y] (Iterate'new f, nil, y))

    (def #_"any" Iterate'UNREALIZED (anew 0))

    (defn #_"boolean" Iterate''isRealized [#_"Iterate" this]
        (not (identical? (deref (:y this)) Iterate'UNREALIZED))
    )

    (defn #_"seq" Iterate''seq [#_"Iterate" this]
        this
    )

    (defn #_"Object" Iterate''first [#_"Iterate" this]
        (let-when [#_"Object" y (deref (:y this))] (identical? y Iterate'UNREALIZED) => y
            (reset! (:y this) ((:f this) (:x this)))
        )
    )

    #_memoize!
    (defn #_"seq" Iterate''next [#_"Iterate" this]
        (Iterate'new (:f this), (first this), Iterate'UNREALIZED)
    )

    (defn #_"Object" Iterate''reduce
        ([#_"Iterate" this, #_"fn" f]
            (loop [#_"Object" r (first this) #_"Object" v ((:f this) r)]
                (let-when [r (f r v)] (reduced? r) => (recur r ((:f this) v))
                    (deref r)
                )
            )
        )
        ([#_"Iterate" this, #_"fn" f, #_"Object" r]
            (loop [r r #_"Object" v (first this)]
                (let-when [r (f r v)] (reduced? r) => (recur r ((:f this) v))
                    (deref r)
                )
            )
        )
    )

    (defm Iterate IMeta
        (IMeta'''meta => :_meta)
    )

    (defm Iterate IObj
        (IObj'''withMeta => Iterate''withMeta)
    )

    (defm Iterate IPending
        (IPending'''isRealized => Iterate''isRealized)
    )

    (defm Iterate Sequential)

    (defm Iterate Seqable
        (Seqable'''seq => Iterate''seq)
    )

    (defm Iterate ISeq
        (ISeq'''first => Iterate''first)
        (ISeq'''next => Iterate''next)
    )

    (defm Iterate IReduce
        (IReduce'''reduce => Iterate''reduce)
    )

    (defm Iterate Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Iterate IObject
        (IObject'''equals => ASeq''equals)
    )
)

(defn iterate [f x] (Iterate'create f x))
)

(about #_"arbace.arm.Repeat"

(about #_"Repeat"
    (defq Repeat [#_"meta" _meta, #_"long" cnt, #_"Object" val] SeqForm)

    #_inherit
    (defm Repeat ASeq)

    (def #_"long" Repeat'INFINITE -1)

    (defn #_"Repeat" Repeat'new
        ([#_"long" cnt, #_"Object" val] (Repeat'new nil, cnt, val))
        ([#_"meta" meta, #_"long" cnt, #_"Object" val]
            (new* Repeat'class (anew [meta, cnt, val]))
        )
    )

    (defn #_"Repeat" Repeat''withMeta [#_"Repeat" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Repeat'new meta, (:cnt this), (:val this))
        )
    )

    (defn #_"Repeat|ISeq" Repeat'create
        ([#_"Object" val] (Repeat'new Repeat'INFINITE, val))
        ([#_"long" n, #_"Object" val] (if (pos? n) (Repeat'new n, val) (list)))
    )

    (defn #_"seq" Repeat''seq [#_"Repeat" this]
        this
    )

    (defn #_"seq" Repeat''next [#_"Repeat" this]
        (cond
            (< 1 (:cnt this))               (Repeat'new (dec (:cnt this)), (:val this))
            (= (:cnt this) Repeat'INFINITE) this
        )
    )

    (defn #_"Object" Repeat''reduce
        ([#_"Repeat" this, #_"fn" f]
            (let [#_"Object" r (:val this)]
                (if (= (:cnt this) Repeat'INFINITE)
                    (loop [r r]
                        (let [r (f r (:val this))]
                            (if (reduced? r) (deref r) (recur r))
                        )
                    )
                    (loop-when [r r #_"long" i 1] (< i (:cnt this)) => r
                        (let [r (f r (:val this))]
                            (if (reduced? r) (deref r) (recur r (inc i)))
                        )
                    )
                )
            )
        )
        ([#_"Repeat" this, #_"fn" f, #_"Object" r]
            (if (= (:cnt this) Repeat'INFINITE)
                (loop [r r]
                    (let [r (f r (:val this))]
                        (if (reduced? r) (deref r) (recur r))
                    )
                )
                (loop-when [r r #_"long" i 0] (< i (:cnt this)) => r
                    (let [r (f r (:val this))]
                        (if (reduced? r) (deref r) (recur r (inc i)))
                    )
                )
            )
        )
    )

    (defm Repeat IMeta
        (IMeta'''meta => :_meta)
    )

    (defm Repeat IObj
        (IObj'''withMeta => Repeat''withMeta)
    )

    (defm Repeat Sequential)

    (defm Repeat Seqable
        (Seqable'''seq => Repeat''seq)
    )

    (defm Repeat ISeq
        (ISeq'''first => :val)
        (ISeq'''next => Repeat''next)
    )

    (defm Repeat IReduce
        (IReduce'''reduce => Repeat''reduce)
    )

    (defm Repeat Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Repeat IObject
        (IObject'''equals => ASeq''equals)
    )
)

(defn repeat
    ([  x] (Repeat'create   x))
    ([n x] (Repeat'create n x))
)
)

(about #_"arbace.arm.Range"

(about #_"Range"
    (defq Range [#_"meta" _meta, #_"Object" start, #_"Object" end, #_"Object" step, #_"fn" f'boundsCheck] SeqForm)

    #_inherit
    (defm Range ASeq)

    #_abstract
    (defm Range Counted)

    (defn #_"Range" Range'new
        ([#_"Object" start, #_"Object" end, #_"Object" step, #_"fn" f'boundsCheck]
            (Range'new nil, start, end, step, f'boundsCheck)
        )
        ([#_"meta" meta, #_"Object" start, #_"Object" end, #_"Object" step, #_"fn" f'boundsCheck]
            (new* Range'class (anew [meta, start, end, step, f'boundsCheck]))
        )
    )

    (defn #_"Range" Range''withMeta [#_"Range" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Range'new meta, (:end this), (:start this), (:step this), (:f'boundsCheck this))
        )
    )

    (defn #_"fn" Range'positiveStep [#_"Object" end] #(<= end %))
    (defn #_"fn" Range'negativeStep [#_"Object" end] #(<= % end))

    (defn #_"seq" Range'create
        ([#_"Object" end]
            (when (pos? end) => (list)
                (Range'new 0, end, 1, (Range'positiveStep end))
            )
        )
        ([#_"Object" start, #_"Object" end]
            (Range'create start, end, 1)
        )
        ([#_"Object" start, #_"Object" end, #_"Object" step]
            (cond
                (or (and (pos? step) (< end start))
                    (and (neg? step) (< start end))
                    (= start end)
                )
                    (list)
                (zero? step)
                    (Repeat'create start)
                :else
                    (Range'new start, end, step, (if (pos? step) (Range'positiveStep end) (Range'negativeStep end)))
            )
        )
    )

    (defn #_"seq" Range''seq [#_"Range" this]
        this
    )

    (defn #_"seq" Range''next [#_"Range" this]
        (let-when-not [#_"Object" n (+ (:start this) (:step this))] ((:f'boundsCheck this) n)
            (Range'new n, (:end this), (:step this), (:f'boundsCheck this))
        )
    )

    (defn #_"Object" Range''reduce
        ([#_"Range" this, #_"fn" f]
            (loop [#_"Object" r (:start this) #_"Number" n r]
                (let-when-not [n (+ n (:step this))] ((:f'boundsCheck this) n) => r
                    (let-when-not [r (f r n)] (reduced? r) => (deref r)
                        (recur r n)
                    )
                )
            )
        )
        ([#_"Range" this, #_"fn" f, #_"Object" r]
            (loop [r r #_"Object" n (:start this)]
                (let-when-not [r (f r n)] (reduced? r) => (deref r)
                    (let-when-not [n (+ n (:step this))] ((:f'boundsCheck this) n) => r
                        (recur r n)
                    )
                )
            )
        )
    )

    (defm Range IMeta
        (IMeta'''meta => :_meta)
    )

    (defm Range IObj
        (IObj'''withMeta => Range''withMeta)
    )

    (defm Range Sequential)

    (defm Range Seqable
        (Seqable'''seq => Range''seq)
    )

    (defm Range ISeq
        (ISeq'''first => :start)
        (ISeq'''next => Range''next)
    )

    (defm Range IReduce
        (IReduce'''reduce => Range''reduce)
    )

    (defm Range Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Range IObject
        (IObject'''equals => ASeq''equals)
    )
)

(defn range
    ([] (iterate inc 0))
    ([over] (Range'create over))
    ([from over] (Range'create from over))
    ([from over step] (Range'create from over step))
)
)

(about #_"arbace.arm.ArraySeq"

(about #_"ArraySeq"
    (defq ArraySeq [#_"meta" _meta, #_"array" a, #_"int" i] SeqForm)

    #_inherit
    (defm ArraySeq ASeq)

    (defn #_"ArraySeq" ArraySeq'new
        ([#_"array" a, #_"int" i] (ArraySeq'new nil, a, i))
        ([#_"meta" meta, #_"array" a, #_"int" i]
            (new* ArraySeq'class (anew [meta, a, i]))
        )
    )

    (defn #_"ArraySeq" ArraySeq''withMeta [#_"ArraySeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (ArraySeq'new meta, (:a this), (:i this))
        )
    )

    (defn #_"ArraySeq" ArraySeq'create [#_"array" a]
        (when (and (some? a) (pos? (alength a)))
            (ArraySeq'new a, 0)
        )
    )

    (-/extend-protocol Seqable (do Object'array)
        (#_"ArraySeq" Seqable'''seq [#_"array" a] (ArraySeq'create a))
    )

    (defn #_"seq" ArraySeq''seq [#_"ArraySeq" this]
        this
    )

    (defn #_"Object" ArraySeq''first [#_"ArraySeq" this]
        (when (some? (:a this))
            (aget (:a this) (:i this))
        )
    )

    (defn #_"seq" ArraySeq''next [#_"ArraySeq" this]
        (when (and (some? (:a this)) (< (inc (:i this)) (count (:a this))))
            (ArraySeq'new (:a this), (inc (:i this)))
        )
    )

    (defn #_"int" ArraySeq''count [#_"ArraySeq" this]
        (if (some? (:a this)) (- (count (:a this)) (:i this)) 0)
    )

    (defn #_"Object" ArraySeq''reduce
        ([#_"ArraySeq" this, #_"fn" f]
            (when-some [#_"array" a (:a this)]
                (let [#_"int" i (:i this) #_"int" n (count a)]
                    (loop-when [#_"Object" r (aget a i) i (inc i)] (< i n) => r
                        (let [r (f r (aget a i))]
                            (if (reduced? r) (deref r) (recur r (inc i)))
                        )
                    )
                )
            )
        )
        ([#_"ArraySeq" this, #_"fn" f, #_"Object" r]
            (when-some [#_"array" a (:a this)]
                (let [#_"int" i (:i this) #_"int" n (count a)]
                    (loop-when [r (f r (aget a i)) i (inc i)] (< i n) => (if (reduced? r) (deref r) r)
                        (if (reduced? r) (deref r) (recur (f r (aget a i)) (inc i)))
                    )
                )
            )
        )
    )

    (defm ArraySeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm ArraySeq IObj
        (IObj'''withMeta => ArraySeq''withMeta)
    )

    (defm ArraySeq Sequential)

    (defm ArraySeq Seqable
        (Seqable'''seq => ArraySeq''seq)
    )

    (defm ArraySeq ISeq
        (ISeq'''first => ArraySeq''first)
        (ISeq'''next => ArraySeq''next)
    )

    (defm ArraySeq Counted
        (Counted'''count => ArraySeq''count)
    )

    (defm ArraySeq IReduce
        (IReduce'''reduce => ArraySeq''reduce)
    )

    (defm ArraySeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm ArraySeq IObject
        (IObject'''equals => ASeq''equals)
    )
)
)

(about #_"arbace.arm.StringSeq"

(about #_"StringSeq"
    (defq StringSeq [#_"meta" _meta, #_"CharSequence" s, #_"int" i] SeqForm)

    #_inherit
    (defm StringSeq ASeq)

    (defn #_"StringSeq" StringSeq'new [#_"meta" meta, #_"CharSequence" s, #_"int" i]
        (new* StringSeq'class (anew [meta, s, i]))
    )

    (defn #_"StringSeq" StringSeq''withMeta [#_"StringSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (StringSeq'new meta, (:s this), (:i this))
        )
    )

    (defn #_"StringSeq" StringSeq'create [#_"CharSequence" s]
        (when (pos? (CharSequence''length s))
            (StringSeq'new nil, s, 0)
        )
    )

    (-/extend-protocol Seqable java.lang.CharSequence
        (#_"StringSeq" Seqable'''seq [#_"CharSequence" s] (StringSeq'create s))
    )

    (defn #_"seq" StringSeq''seq [#_"StringSeq" this]
        this
    )

    (defn #_"Object" StringSeq''first [#_"StringSeq" this]
        (Character'valueOf (CharSequence''charAt (:s this), (:i this)))
    )

    (defn #_"seq" StringSeq''next [#_"StringSeq" this]
        (when (< (inc (:i this)) (CharSequence''length (:s this)))
            (StringSeq'new (:_meta this), (:s this), (inc (:i this)))
        )
    )

    (defn #_"int" StringSeq''count [#_"StringSeq" this]
        (- (CharSequence''length (:s this)) (:i this))
    )

    (defn #_"Object" StringSeq''reduce
        ([#_"StringSeq" this, #_"fn" f]
            (let [#_"CharSequence" s (:s this) #_"int" i (:i this) #_"int" n (CharSequence''length s)]
                (loop-when [#_"Object" r (CharSequence''charAt s, i) i (inc i)] (< i n) => r
                    (let [r (f r (CharSequence''charAt s, i))]
                        (if (reduced? r) (deref r) (recur r (inc i)))
                    )
                )
            )
        )
        ([#_"StringSeq" this, #_"fn" f, #_"Object" r]
            (let [#_"CharSequence" s (:s this) #_"int" i (:i this) #_"int" n (CharSequence''length s)]
                (loop-when [r (f r (CharSequence''charAt s, i)) i (inc i)] (< i n) => (if (reduced? r) (deref r) r)
                    (if (reduced? r) (deref r) (recur (f r (CharSequence''charAt s, i)) (inc i)))
                )
            )
        )
    )

    (defm StringSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm StringSeq IObj
        (IObj'''withMeta => StringSeq''withMeta)
    )

    (defm StringSeq Sequential)

    (defm StringSeq Seqable
        (Seqable'''seq => StringSeq''seq)
    )

    (defm StringSeq ISeq
        (ISeq'''first => StringSeq''first)
        (ISeq'''next => StringSeq''next)
    )

    (defm StringSeq Counted
        (Counted'''count => StringSeq''count)
    )

    (defm StringSeq IReduce
        (IReduce'''reduce => StringSeq''reduce)
    )

    (defm StringSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm StringSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)
)

(about #_"arbace.arm.LazySeq"

(about #_"LazySeq"
    (defq LazySeq [#_"meta" _meta, #_"fn'" f, #_"Object'" o, #_"seq'" s] SeqForm)

    (defn #_"LazySeq" LazySeq'init [#_"meta" meta, #_"fn" f, #_"seq" s]
        (new* LazySeq'class (anew [meta, (atom f), (atom nil), (atom s)]))
    )

    (defn #_"LazySeq" LazySeq'new
        ([#_"fn" f]                 (LazySeq'init nil,  f,   nil))
        ([#_"meta" meta, #_"seq" s] (LazySeq'init meta, nil, s  ))
    )

    (defn #_"LazySeq" LazySeq''withMeta [#_"LazySeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (LazySeq'new meta, (seq this))
        )
    )

    (defn #_"cons" LazySeq''conj [#_"LazySeq" this, #_"Object" o]
        (cons o this)
    )

    (defn #_"IPersistentCollection" LazySeq''empty [#_"LazySeq" this]
        (list)
    )

    (defn #_"seq" LazySeq''seq [#_"LazySeq" this]
        (locking this
            (letfn [(step- [this]
                        (when-some [#_"fn" f (deref (:f this))]
                            (reset! (:f this) nil)
                            (reset! (:o this) (f))
                        )
                        (or (deref (:o this)) (deref (:s this)))
                    )]
                (step- this)
                (when-some [#_"Object" o (deref (:o this))]
                    (reset! (:o this) nil)
                    (reset! (:s this) (loop-when-recur o (satisfies? LazySeq o) (step- o) => (seq o)))
                )
                (deref (:s this))
            )
        )
    )

    (defn #_"Object" LazySeq''first [#_"LazySeq" this]
        (when-some [#_"seq" s (seq this)]
            (first s)
        )
    )

    (defn #_"seq" LazySeq''next [#_"LazySeq" this]
        (when-some [#_"seq" s (seq this)]
            (next s)
        )
    )

    (defn #_"boolean" LazySeq''equals [#_"LazySeq" this, #_"Object" that]
        (if-some [#_"seq" s (seq this)]
            (= s that)
            (and (sequential? that) (nil? (seq that)))
        )
    )

    (defn #_"boolean" LazySeq''isRealized [#_"LazySeq" this]
        (locking this
            (nil? (deref (:f this)))
        )
    )

    (defm LazySeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm LazySeq IObj
        (IObj'''withMeta => LazySeq''withMeta)
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

    (defm LazySeq IPending
        (IPending'''isRealized => LazySeq''isRealized)
    )
)

(defmacro lazy-seq [& body] `(LazySeq'new (fn* [] ~@body)))

(defn dorun
    ([s]
        (when-some [s (seq s)]
            (recur (next s))
        )
    )
    ([n s]
        (when (pos? n)
            (when-some [s (seq s)]
                (recur (dec n) (next s))
            )
        )
    )
)

(defn doall
    ([s] (dorun s) s)
    ([n s] (dorun n s) s)
)

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

(defn partial
    ([f] f)
    ([f a]
        (fn
            ([] (f a))
            ([x] (f a x))
            ([x y] (f a x y))
            ([x y z] (f a x y z))
            ([x y z & args] (apply f a x y z args))
        )
    )
    ([f a b]
        (fn
            ([] (f a b))
            ([x] (f a b x))
            ([x y] (f a b x y))
            ([x y z] (f a b x y z))
            ([x y z & args] (apply f a b x y z args))
        )
    )
    ([f a b c]
        (fn
            ([] (f a b c))
            ([x] (f a b c x))
            ([x y] (f a b c x y))
            ([x y z] (f a b c x y z))
            ([x y z & args] (apply f a b c x y z args))
        )
    )
    ([f a b c & more]
        (fn [& args] (apply f a b c (concat more args)))
    )
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
            (map #(apply f %) (map- (conj z s3 s2 s1)))
        )
    )
)

(defn mapcat
    ([f] (comp (map f) cat))
    ([f & s] (apply concat (apply map f s)))
)

(defn keep
    ([f]
        (fn [g]
            (fn
                ([] (g))
                ([s] (g s))
                ([s x]
                    (when-some [y (f x)] => s
                        (g s y)
                    )
                )
            )
        )
    )
    ([f s]
        (lazy-seq
            (when-some [s (seq s)]
                (when-some [y (f (first s))] => (keep f (next s))
                    (cons y (keep f (next s)))
                )
            )
        )
    )
)

(defn filter
    ([f?]
        (fn [g]
            (fn
                ([] (g))
                ([s] (g s))
                ([s x] (if (f? x) (g s x) s))
            )
        )
    )
    ([f? s]
        (lazy-seq
            (when-some [s (seq s)]
                (let-when [x (first s)] (f? x) => (filter f? (next s))
                    (cons x (filter f? (next s)))
                )
            )
        )
    )
)

(defn remove
    ([f?]   (filter (complement f?)  ))
    ([f? s] (filter (complement f?) s))
)

(defn take
    ([n]
        (fn [g]
            (let [n' (atom n)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (let [n (deref n') m (swap! n' dec) s (if (pos? n) (g s x) s)]
                            (if (pos? m) s (ensure-reduced s))
                        )
                    )
                )
            )
        )
    )
    ([n s]
        (lazy-seq
            (when (pos? n)
                (when-some [s (seq s)]
                    (cons (first s) (take (dec n) (next s)))
                )
            )
        )
    )
)

(defn take-while
    ([f?]
        (fn [g]
            (fn
                ([] (g))
                ([s] (g s))
                ([s x] (if (f? x) (g s x) (reduced s)))
            )
        )
    )
    ([f? s]
        (lazy-seq
            (when-some [s (seq s)]
                (let-when [x (first s)] (f? x)
                    (cons x (take-while f? (next s)))
                )
            )
        )
    )
)

(defn drop
    ([n]
        (fn [g]
            (let [n' (atom n)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x] (if (neg? (swap! n' dec)) (g s x) s))
                )
            )
        )
    )
    ([n s]
        (letfn [(drop- [n s]
                    (let [s (seq s)]
                        (recur-when (and (pos? n) s) [(dec n) (next s)] => s)
                    )
                )]
            (lazy-seq (drop- n s))
        )
    )
)

(defn drop-while
    ([f?]
        (fn [g]
            (let [drop? (atom true)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (when-not (and (deref drop?) (f? x)) => s
                            (reset! drop? nil)
                            (g s x)
                        )
                    )
                )
            )
        )
    )
    ([f? s]
        (letfn [(drop- [f? s]
                    (let [s (seq s)]
                        (recur-when (and s (f? (first s))) [f? (next s)] => s)
                    )
                )]
            (lazy-seq (drop- f? s))
        )
    )
)

(defn split-at [n s] [(take n s) (drop n s)])

(defn split-with [f? s] [(take-while f? s) (drop-while f? s)])

(defn take-nth
    ([n]
        (fn [g]
            (let [i' (atom -1)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (let-when [i (swap! i' inc)] (zero? (rem i n)) => s
                            (g s x)
                        )
                    )
                )
            )
        )
    )
    ([n s]
        (lazy-seq
            (when-some [s (seq s)]
                (cons (first s) (take-nth n (drop n s)))
            )
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

(defn partition-all
    ([n]
        (fn [g]
            (let [v' (atom (vector))]
                (fn
                    ([] (g))
                    ([x]
                        (let [x (when (seq (deref v')) => x
                                    (let [v (deref v') _ (swap! v' empty)]
                                        (unreduced (g x v))
                                    )
                                )]
                            (g x)
                        )
                    )
                    ([x y]
                        (swap! v' conj y)
                        (when (= (count (deref v')) n) => x
                            (let [v (deref v') _ (swap! v' empty)]
                                (g x v)
                            )
                        )
                    )
                )
            )
        )
    )
    ([n s] (partition-all n n s))
    ([n step s]
        (lazy-seq
            (when-some [s (seq s)]
                (let [p (doall (take n s))]
                    (cons p (partition-all n step (nthnext s step)))
                )
            )
        )
    )
)
)

(about #_"arbace.arm.APersistentMap"

(about #_"APersistentMap"
    (defn #_"IPersistentCollection" APersistentMap''conj [#_"APersistentMap" this, #_"Object" o]
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

    (defn #_"boolean" APersistentMap''equals [#_"APersistentMap" this, #_"Object" that]
        (or (identical? this that)
            (and (map? that) (= (count that) (count this))
                (loop-when [#_"seq" s (seq this)] (some? s) => true
                    (let [#_"pair" e (first s) #_"Object" k (key e)]
                        (and (contains? that k) (= (val e) (get that k))
                            (recur (next s))
                        )
                    )
                )
            )
        )
    )

    (defn #_"Object" APersistentMap''invoke
        ([#_"APersistentMap" this, #_"key" key] (get this key))
        ([#_"APersistentMap" this, #_"key" key, #_"value" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.arm.APersistentSet"

(about #_"APersistentSet"
    (defn #_"boolean" APersistentSet''equals [#_"APersistentSet" this, #_"Object" that]
        (or (identical? this that)
            (and (set? that) (= (count this) (count that))
                (loop-when [#_"seq" s (seq that)] (some? s) => true
                    (and (contains? this (first s)) (recur (next s)))
                )
            )
        )
    )

    (defn #_"Object" APersistentSet''invoke
        ([#_"APersistentSet" this, #_"key" key] (get this key))
        ([#_"APersistentSet" this, #_"key" key, #_"value" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.arm.APersistentVector"

(about #_"VSeq"
    (defq VSeq [#_"meta" _meta, #_"vector" v, #_"int" i] SeqForm)

    #_inherit
    (defm VSeq ASeq)

    (defn #_"VSeq" VSeq'new
        ([#_"vector" v, #_"int" i] (VSeq'new nil, v, i))
        ([#_"meta" meta, #_"vector" v, #_"int" i]
            (new* VSeq'class (anew [meta, v, i]))
        )
    )

    (defn #_"VSeq" VSeq''withMeta [#_"VSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (VSeq'new meta, (:v this), (:i this))
        )
    )

    (defn #_"seq" VSeq''seq [#_"VSeq" this]
        this
    )

    (defn #_"Object" VSeq''first [#_"VSeq" this]
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

    (defn #_"Object" VSeq''reduce
        ([#_"VSeq" this, #_"fn" f]
            (let [#_"vector" v (:v this) #_"int" i (:i this) #_"int" n (count v)]
                (loop-when [#_"Object" r (nth v i) i (inc i)] (< i n) => r
                    (let-when [r (f r (nth v i))] (reduced? r) => (recur r (inc i))
                        (deref r)
                    )
                )
            )
        )
        ([#_"VSeq" this, #_"fn" f, #_"Object" r]
            (let [#_"vector" v (:v this) #_"int" i (:i this) #_"int" n (count v)]
                (loop-when [r (f r (nth v i)) i (inc i)] (< i n) => (if (reduced? r) (deref r) r)
                    (when (reduced? r) => (recur (f r (nth v i)) (inc i))
                        (deref r)
                    )
                )
            )
        )
    )

    (defm VSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm VSeq IObj
        (IObj'''withMeta => VSeq''withMeta)
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

    (defm VSeq IReduce
        (IReduce'''reduce => VSeq''reduce)
    )

    (defm VSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm VSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"RSeq"
    (defq RSeq [#_"meta" _meta, #_"vector" v, #_"int" i] SeqForm)

    #_inherit
    (defm RSeq ASeq)

    (defn #_"RSeq" RSeq'new
        ([#_"vector" v, #_"int" i] (RSeq'new nil, v, i))
        ([#_"meta" meta, #_"vector" v, #_"int" i]
            (new* RSeq'class (anew [meta, v, i]))
        )
    )

    (defn #_"RSeq" RSeq''withMeta [#_"RSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (RSeq'new meta, (:v this), (:i this))
        )
    )

    (defn #_"seq" RSeq''seq [#_"RSeq" this]
        this
    )

    (defn #_"Object" RSeq''first [#_"RSeq" this]
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

    (defm RSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm RSeq IObj
        (IObj'''withMeta => RSeq''withMeta)
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
    (defn #_"Object" AMapEntry''nth
        ([#_"AMapEntry" this, #_"int" i]
            (case! i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) (throw! "index is out of bounds"))
        )
        ([#_"AMapEntry" this, #_"int" i, #_"value" not-found]
            (case! i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) not-found)
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

    (defn #_"boolean" AMapEntry''equals [#_"AMapEntry" this, #_"Object" that]
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
        (loop-when [#_"int" hash (int 1) #_"int" i (int 0)] (< i 2) => (Murmur3'mixCollHash hash, i)
            (recur (+ (* (int 31) hash) (f'hash (Indexed'''nth this, i))) (inc i))
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

(about #_"arbace.arm.ATransientMap"

(about #_"ATransientMap"
    (defn #_"Object" ATransientMap''invoke
        ([#_"ATransientMap" this, #_"key" key] (get this key))
        ([#_"ATransientMap" this, #_"key" key, #_"value" not-found] (get this key not-found))
    )

    (def #_"value" ATransientMap'NOT_FOUND (anew 0))

    (defn #_"boolean" ATransientMap''containsKey [#_"ATransientMap" this, #_"key" key]
        (not (identical? (get this key ATransientMap'NOT_FOUND) ATransientMap'NOT_FOUND))
    )

    (defn #_"IMapEntry" ATransientMap''entryAt [#_"ATransientMap" this, #_"key" key]
        (let [#_"Object" v (get this key ATransientMap'NOT_FOUND)]
            (when-not (identical? v ATransientMap'NOT_FOUND)
                (MapEntry'new key, v)
            )
        )
    )
)
)

(about #_"arbace.arm.ATransientSet"

(about #_"ATransientSet"
    (defn #_"Object" ATransientSet''invoke
        ([#_"ATransientSet" this, #_"key" key] (get this key))
        ([#_"ATransientSet" this, #_"key" key, #_"value" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.arm.PersistentList"

(about #_"EmptyList"
    (defq EmptyList [#_"meta" _meta] SeqForm)

    (defn #_"EmptyList" EmptyList'new [#_"meta" meta]
        (new* EmptyList'class (anew [meta]))
    )

    (defn #_"EmptyList" EmptyList''withMeta [#_"EmptyList" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (EmptyList'new meta)
        )
    )

    (def #_"int" EmptyList'HASH (Murmur3'hashOrdered nil))

    (defn #_"int" EmptyList''hash [#_"EmptyList" this]
        EmptyList'HASH
    )

    (defn #_"boolean" EmptyList''equals [#_"EmptyList" this, #_"Object" that]
        (and (sequential? that) (nil? (seq that)))
    )

    (defn #_"seq" EmptyList''seq [#_"EmptyList" this]
        nil
    )

    (defn #_"Object" EmptyList''first [#_"EmptyList" this]
        nil
    )

    (defn #_"seq" EmptyList''next [#_"EmptyList" this]
        nil
    )

    (defn #_"int" EmptyList''count [#_"EmptyList" this]
        0
    )

    (defn #_"PersistentList" EmptyList''conj [#_"EmptyList" this, #_"Object" o]
        (PersistentList'new (:_meta this), o, nil, 1)
    )

    (defn #_"EmptyList" EmptyList''empty [#_"EmptyList" this]
        this
    )

    (defn #_"Object" EmptyList''peek [#_"EmptyList" this]
        nil
    )

    (defn #_"IPersistentList" EmptyList''pop [#_"EmptyList" this]
        (throw! "can't pop the empty list")
    )

    (defm EmptyList IPersistentList Sequential)

    (defm EmptyList IMeta
        (IMeta'''meta => :_meta)
    )

    (defm EmptyList IObj
        (IObj'''withMeta => EmptyList''withMeta)
    )

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
    (defq PersistentList [#_"meta" _meta, #_"Object" car, #_"IPersistentList" cdr, #_"int" cnt] SeqForm)

    #_inherit
    (defm PersistentList ASeq)

    (defn #_"PersistentList" PersistentList'new
        ([#_"Object" car] (PersistentList'new nil, car, nil, 1))
        ([#_"meta" meta, #_"Object" car, #_"IPersistentList" cdr, #_"int" cnt]
            (new* PersistentList'class (anew [meta, car, cdr, cnt]))
        )
    )

    (def #_"EmptyList" PersistentList'EMPTY (EmptyList'new nil))

    (defn #_"PersistentList" PersistentList'create [#_"Reversible" init]
        (into PersistentList'EMPTY (if (satisfies? Reversible init) (rseq init) (reverse init)))
    )

    (defn #_"PersistentList" PersistentList''withMeta [#_"PersistentList" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentList'new meta, (:car this), (:cdr this), (:cnt this))
        )
    )

    (defn #_"seq" PersistentList''seq [#_"PersistentList" this]
        this
    )

    (defn #_"PersistentList" PersistentList''conj [#_"PersistentList" this, #_"Object" o]
        (PersistentList'new (:_meta this), o, this, (inc (:cnt this)))
    )

    (defn #_"PersistentList" PersistentList''empty [#_"PersistentList" this]
        (with-meta PersistentList'EMPTY (:_meta this))
    )

    (defn #_"IPersistentList" PersistentList''pop [#_"PersistentList" this]
        (or (:cdr this) (with-meta PersistentList'EMPTY (:_meta this)))
    )

    (defn #_"Object" PersistentList''reduce
        ([#_"PersistentList" this, #_"fn" f]
            (loop-when [#_"Object" r (:car this) #_"IPersistentList" l (:cdr this)] (some? l) => r
                (let [r (f r (:car l))]
                    (if (reduced? r) (deref r) (recur r (:cdr l)))
                )
            )
        )
        ([#_"PersistentList" this, #_"fn" f, #_"Object" r]
            (loop-when [r (f r (:car this)) #_"IPersistentList" l (:cdr this)] (some? l) => (if (reduced? r) (deref r) r)
                (if (reduced? r) (deref r) (recur (f r (:car l)) (:cdr l)))
            )
        )
    )

    (defm PersistentList IPersistentList Sequential)

    (defm PersistentList IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentList IObj
        (IObj'''withMeta => PersistentList''withMeta)
    )

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

    (defm PersistentList IReduce
        (IReduce'''reduce => PersistentList''reduce)
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
    (defq MSeq [#_"meta" _meta, #_"array" a, #_"int" i] SeqForm)

    #_inherit
    (defm MSeq ASeq)

    (defn #_"MSeq" MSeq'new
        ([#_"array" a, #_"int" i] (MSeq'new nil, a, i))
        ([#_"meta" meta, #_"array" a, #_"int" i]
            (new* MSeq'class (anew [meta, a, i]))
        )
    )

    (defn #_"MSeq" MSeq''withMeta [#_"MSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (MSeq'new meta, (:a this), (:i this))
        )
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

    (defm MSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm MSeq IObj
        (IObj'''withMeta => MSeq''withMeta)
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

(about #_"TransientArrayMap"
    (defq TransientArrayMap [#_"thread'" edit, #_"array" array, #_"int" cnt] #_"MapForm")

    #_inherit
    (defm TransientArrayMap ATransientMap AFn)

    (defn #_"TransientArrayMap" TransientArrayMap'new [#_"array" a]
        (let [#_"int" n (alength a) #_"int" m (max PersistentArrayMap'HASHTABLE_THRESHOLD n)]
            (new* TransientArrayMap'class (anew [(atom (thread)), (-> (anew m) (acopy! 0 a 0 n)), n]))
        )
    )

    (defn #_"void" TransientArrayMap''assert-editable [#_"TransientArrayMap" this]
        (or (deref (:edit this)) (throw! "transient used after persistent! call"))
        nil
    )

    (defn #_"int" TransientArrayMap''count [#_"TransientArrayMap" this]
        (TransientArrayMap''assert-editable this)
        (quot (:cnt this) 2)
    )

    (defn #_"int" TransientArrayMap'index-of [#_"array" a, #_"int" n, #_"key" key]
        (loop-when [#_"int" i 0] (< i n) => -1
            (if (= (aget a i) key) i (recur (+ i 2)))
        )
    )

    (defn #_"value" TransientArrayMap''valAt
        ([#_"TransientArrayMap" this, #_"key" key] (TransientArrayMap''valAt this, key, nil))
        ([#_"TransientArrayMap" this, #_"key" key, #_"value" not-found]
            (TransientArrayMap''assert-editable this)
            (let [
                #_"array" a (:array this) #_"int" n (:cnt this) #_"int" i (TransientArrayMap'index-of a, n, key)
            ]
                (if (< -1 i) (aget a (inc i)) not-found)
            )
        )
    )

    (defn #_"ITransientMap" TransientArrayMap''assoc! [#_"TransientArrayMap" this, #_"key" key, #_"value" val]
        (TransientArrayMap''assert-editable this)
        (let [
            #_"array" a (:array this) #_"int" n (:cnt this) #_"int" i (TransientArrayMap'index-of a, n, key)
        ]
            (cond
                (< -1 i)
                    (do
                        (aset! a (inc i) val)
                        this
                    )
                (< n (alength a))
                    (do
                        (aset! a      n  key)
                        (aset! a (inc n) val)
                        (qset! this :cnt (+ n 2))
                    )
                :else
                    (-> (PersistentHashMap'create-1a a) (transient) (assoc! key val))
            )
        )
    )

    (defn #_"ITransientMap" TransientArrayMap''dissoc! [#_"TransientArrayMap" this, #_"key" key]
        (TransientArrayMap''assert-editable this)
        (let [
            #_"array" a (:array this) #_"int" n (:cnt this) #_"int" i (TransientArrayMap'index-of a, n, key)
        ]
            (when (< -1 i) => this
                (let [
                    n (- n 2)
                ]
                    (when (< -1 n)
                        (aset! a      i  (aget a      n))
                        (aset! a (inc i) (aget a (inc n)))
                    )
                    (qset! this :cnt n)
                )
            )
        )
    )

    (defn #_"ITransientMap" TransientArrayMap''conj! [#_"TransientArrayMap" this, #_"pair" o]
        (TransientArrayMap''assert-editable this)
        (condp satisfies? o
            IMapEntry
                (assoc! this (key o) (val o))
            IPersistentVector
                (when (= (count o) 2) => (throw! "vector arg to map conj must be a pair")
                    (assoc! this (nth o 0) (nth o 1))
                )
            #_else
                (loop-when [this this #_"seq" s (seq o)] (some? s) => this
                    (let [#_"pair" e (first s)]
                        (recur (assoc! this (key e) (val e)) (next s))
                    )
                )
        )
    )

    (defn #_"IPersistentMap" TransientArrayMap''persistent! [#_"TransientArrayMap" this]
        (TransientArrayMap''assert-editable this)
        (reset! (:edit this) nil)
        (let [
            #_"int" n (:cnt this)
        ]
            (PersistentArrayMap'new (-> (anew n) (acopy! 0 (:array this) 0 n)))
        )
    )

    (defm TransientArrayMap Counted
        (Counted'''count => TransientArrayMap''count)
    )

    (defm TransientArrayMap ILookup
        (ILookup'''valAt => TransientArrayMap''valAt)
    )

    (defm TransientArrayMap IFn
        (IFn'''invoke => ATransientMap''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm TransientArrayMap ITransientAssociative
        (ITransientAssociative'''assoc! => TransientArrayMap''assoc!)
        (ITransientAssociative'''containsKey => ATransientMap''containsKey)
        (ITransientAssociative'''entryAt => ATransientMap''entryAt)
    )

    (defm TransientArrayMap ITransientMap
        (ITransientMap'''dissoc! => TransientArrayMap''dissoc!)
    )

    (defm TransientArrayMap ITransientCollection
        (ITransientCollection'''conj! => TransientArrayMap''conj!)
        (ITransientCollection'''persistent! => TransientArrayMap''persistent!)
    )
)

(about #_"PersistentArrayMap"
    (defq PersistentArrayMap [#_"meta" _meta, #_"array" array] MapForm)

    #_inherit
    (defm PersistentArrayMap APersistentMap AFn)

    (defn #_"PersistentArrayMap" PersistentArrayMap'new
        ([#_"array" a] (PersistentArrayMap'new nil, a))
        ([#_"meta" meta, #_"array" a]
            (new* PersistentArrayMap'class (anew [meta, (or a (anew 0))]))
        )
    )

    (def #_"PersistentArrayMap" PersistentArrayMap'EMPTY (PersistentArrayMap'new nil))

    (defn #_"PersistentArrayMap" PersistentArrayMap''create [#_"PersistentArrayMap" this, #_"array" init]
        (PersistentArrayMap'new (:_meta this), init)
    )

    (defn #_"PersistentArrayMap" PersistentArrayMap'createWithCheck [#_"array" init]
        (loop-when-recur [#_"int" i 0] (< i (alength init)) [(+ i 2)]
            (loop-when-recur [#_"int" j (+ i 2)] (< j (alength init)) [(+ j 2)]
                (when (= (aget init i) (aget init j))
                    (throw! (str "duplicate key: " (aget init i)))
                )
            )
        )
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

    (defn #_"PersistentArrayMap" PersistentArrayMap''withMeta [#_"PersistentArrayMap" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentArrayMap'new meta, (:array this))
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
                    (-> (PersistentHashMap'create-1a a) (assoc key val) (with-meta (:_meta this)))
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
                (let-when [#_"int" n (- (alength a) 2)] (pos? n) => (with-meta PersistentArrayMap'EMPTY (:_meta this))
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
        (with-meta PersistentArrayMap'EMPTY (:_meta this))
    )

    (defn #_"seq" PersistentArrayMap''seq [#_"PersistentArrayMap" this]
        (when (pos? (alength (:array this)))
            (MSeq'new (:array this), 0)
        )
    )

    (defn #_"ITransientMap" PersistentArrayMap''asTransient [#_"PersistentArrayMap" this]
        (TransientArrayMap'new (:array this))
    )

    (defm PersistentArrayMap IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentArrayMap IObj
        (IObj'''withMeta => PersistentArrayMap''withMeta)
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

    (defm PersistentArrayMap IEditableCollection
        (IEditableCollection'''asTransient => PersistentArrayMap''asTransient)
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
    (defq HSeq [#_"meta" _meta, #_"node[]" nodes, #_"int" i, #_"seq" s] SeqForm)

    #_inherit
    (defm HSeq ASeq)

    (defn #_"HSeq" HSeq'new [#_"meta" meta, #_"node[]" nodes, #_"int" i, #_"seq" s]
        (new* HSeq'class (anew [meta, nodes, i, s]))
    )

    (defn #_"HSeq" HSeq''withMeta [#_"HSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (HSeq'new meta, (:nodes this), (:i this), (:s this))
        )
    )

    (defn #_"seq" HSeq'create-4 [#_"meta" meta, #_"node[]" nodes, #_"int" i, #_"seq" s]
        (when (nil? s) => (HSeq'new meta, nodes, i, s)
            (loop-when i (< i (alength nodes))
                (when-some [#_"node" node (aget nodes i)] => (recur (inc i))
                    (when-some [s (INode'''nodeSeq node)] => (recur (inc i))
                        (HSeq'new meta, nodes, (inc i), s)
                    )
                )
            )
        )
    )

    (defn #_"seq" HSeq'create-1 [#_"node[]" nodes]
        (HSeq'create-4 nil, nodes, 0, nil)
    )

    (defn #_"seq" HSeq''seq [#_"HSeq" this]
        this
    )

    (defn #_"pair" HSeq''first [#_"HSeq" this]
        (first (:s this))
    )

    (defn #_"seq" HSeq''next [#_"HSeq" this]
        (HSeq'create-4 nil, (:nodes this), (:i this), (next (:s this)))
    )

    (defm HSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm HSeq IObj
        (IObj'''withMeta => HSeq''withMeta)
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
    (defq NSeq [#_"meta" _meta, #_"array" a, #_"int" i, #_"seq" s] SeqForm)

    #_inherit
    (defm NSeq ASeq)

    (defn #_"NSeq" NSeq'new
        ([#_"array" a, #_"int" i] (NSeq'new nil, a, i, nil))
        ([#_"meta" meta, #_"array" a, #_"int" i, #_"seq" s]
            (new* NSeq'class (anew [meta, a, i, s]))
        )
    )

    (defn #_"NSeq" NSeq''withMeta [#_"NSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (NSeq'new meta, (:a this), (:i this), (:s this))
        )
    )

    (defn #_"seq" NSeq'create-3 [#_"array" a, #_"int" i, #_"seq" s]
        (when (nil? s) => (NSeq'new nil, a, i, s)
            (loop-when i (< i (alength a))
                (when (nil? (aget a i)) => (NSeq'new nil, a, i, nil)
                    (or
                        (when-some [#_"node" node (aget a (inc i))]
                            (when-some [s (INode'''nodeSeq node)]
                                (NSeq'new nil, a, (+ i 2), s)
                            )
                        )
                        (recur (+ i 2))
                    )
                )
            )
        )
    )

    (defn #_"seq" NSeq'create-1 [#_"array" a]
        (NSeq'create-3 a, 0, nil)
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
            (NSeq'create-3 (:a this), (:i this), (next (:s this)))
            (NSeq'create-3 (:a this), (+ (:i this) 2), nil)
        )
    )

    (defm NSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm NSeq IObj
        (IObj'''withMeta => NSeq''withMeta)
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
        (bit-and (>>> hash shift) 0x1f)
    )

    (defn #_"int" PersistentHashMap'bitpos [#_"int" hash, #_"int" shift]
        (int (<< 1 (PersistentHashMap'mask hash, shift)))
    )

    (defn #_"array" PersistentHashMap'cloneAndSet
        ([#_"array" a, #_"int" i, #_"Object" x]                          (-> (aclone a) (aset! i x)))
        ([#_"array" a, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y] (-> (aclone a) (aset! i x) (aset! j y)))
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
                                [(bit-or bitmap (<< 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )
              bitmap
                (loop-when [bitmap bitmap j j #_"int" i (inc idx)] (< i (alength (:a this))) => bitmap
                    (let [[bitmap j]
                            (when-some [#_"node" ai (aget (:a this) i)] => [bitmap j]
                                (aset! a' j ai)
                                [(bit-or bitmap (<< 1 i)) (+ j 2)]
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
        (HSeq'create-1 (:a this))
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
        (Integer'bitCount (bit-and bitmap (dec bit)))
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
            (let [#_"int" b (:bitmap this) #_"int" n (Integer'bitCount b) #_"int" m (inc n)]
                (BNode'new edit, b, (-> (anew (* 2 m)) (acopy! 0 (:a this) 0 (* 2 n))))
            )
        )
    )

    (defn #_"BNode" BNode''editAndSet
        ([#_"BNode" this, #_"thread'" edit, #_"int" i, #_"Object" x]
            (let [#_"BNode" e (BNode''ensureEditable this, edit)]
                (aset! (:a e) i x)
                e
            )
        )
        ([#_"BNode" this, #_"thread'" edit, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y]
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
                (let [#_"int" n (Integer'bitCount (:bitmap this))]
                    (if (<= 16 n)
                        (let [
                            #_"node[]" nodes (anew #_"node" 32) #_"int" m (PersistentHashMap'mask hash, shift)
                            _ (aset! nodes m (INode'''assoc BNode'EMPTY, (+ shift 5), hash, key, val, addedLeaf))
                            _
                                (loop-when [#_"int" j 0 #_"int" i 0] (< i 32)
                                    (when (odd? (>>> (:bitmap this) i)) => (recur j (inc i))
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
        (NSeq'create-1 (:a this))
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
                (let [#_"int" n (Integer'bitCount (:bitmap this))]
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
                                        (when (odd? (>>> (:bitmap this) i)) => (recur j (inc i))
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
        ([#_"CNode" this, #_"thread'" edit, #_"int" i, #_"Object" x]
            (let [#_"CNode" e (CNode''ensureEditable this, edit)]
                (aset! (:a e) i x)
                e
            )
        )
        ([#_"CNode" this, #_"thread'" edit, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y]
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
        (NSeq'create-1 (:a this))
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

(about #_"TransientHashMap"
    (defq TransientHashMap [#_"thread'" edit, #_"node" root, #_"int" cnt, #_"boolean" has-nil?, #_"value" nil-value] #_"MapForm")

    #_inherit
    (defm TransientHashMap ATransientMap AFn)

    (defn #_"TransientHashMap" TransientHashMap'new
        ([#_"PersistentHashMap" m]
            (TransientHashMap'new (atom (thread)), (:root m), (:cnt m), (:has-nil? m), (:nil-value m))
        )
        ([#_"thread'" edit, #_"node" root, #_"int" cnt, #_"boolean" has-nil?, #_"value" nil-value]
            (new* TransientHashMap'class (anew [edit, root, cnt, has-nil?, nil-value]))
        )
    )

    (defn #_"void" TransientHashMap''assert-editable [#_"TransientHashMap" this]
        (or (deref (:edit this)) (throw! "transient used after persistent! call"))
        nil
    )

    (defn #_"int" TransientHashMap''count [#_"TransientHashMap" this]
        (TransientHashMap''assert-editable this)
        (:cnt this)
    )

    (defn #_"value" TransientHashMap''valAt
        ([#_"TransientHashMap" this, #_"key" key] (TransientHashMap''valAt this, key, nil))
        ([#_"TransientHashMap" this, #_"key" key, #_"value" not-found]
            (TransientHashMap''assert-editable this)
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

    (defn #_"ITransientMap" TransientHashMap''assoc! [#_"TransientHashMap" this, #_"key" key, #_"value" val]
        (TransientHashMap''assert-editable this)
        (if (nil? key)
            (let [
                this (if (= (:nil-value this) val) this (qset! this :nil-value val))
            ]
                (when-not (:has-nil? this) => this
                    (-> this (qswap! :cnt inc) (qset! :has-nil? true))
                )
            )
            (let [
                #_"boolean'" addedLeaf (atom false)
                #_"node" node (INode'''assocT (or (:root this) BNode'EMPTY), (:edit this), 0, (f'hash key), key, val, addedLeaf)
                this (if (= (:root this) node) this (qset! this :root node))
            ]
                (when (deref addedLeaf) => this
                    (-> this (qswap! :cnt inc))
                )
            )
        )
    )

    (defn #_"ITransientMap" TransientHashMap''dissoc! [#_"TransientHashMap" this, #_"key" key]
        (TransientHashMap''assert-editable this)
        (if (nil? key)
            (when (:has-nil? this) => this
                (-> this (qswap! :cnt dec) (qset! :has-nil? false, :nil-value nil))
            )
            (when (some? (:root this)) => this
                (let [
                    #_"boolean'" removedLeaf (atom false)
                    #_"node" node (INode'''dissocT (:root this), (:edit this), 0, (f'hash key), key, removedLeaf)
                    this (if (= (:root this) node) this (qset! this :root node))
                ]
                    (when (deref removedLeaf) => this
                        (-> this (qswap! :cnt dec))
                    )
                )
            )
        )
    )

    (defn #_"ITransientMap" TransientHashMap''conj! [#_"TransientHashMap" this, #_"pair" o]
        (TransientHashMap''assert-editable this)
        (condp satisfies? o
            IMapEntry
                (assoc! this (key o) (val o))
            IPersistentVector
                (when (= (count o) 2) => (throw! "vector arg to map conj must be a pair")
                    (assoc! this (nth o 0) (nth o 1))
                )
            #_else
                (loop-when [this this #_"seq" s (seq o)] (some? s) => this
                    (let [#_"pair" e (first s)]
                        (recur (assoc! this (key e) (val e)) (next s))
                    )
                )
        )
    )

    (defn #_"IPersistentMap" TransientHashMap''persistent! [#_"TransientHashMap" this]
        (TransientHashMap''assert-editable this)
        (reset! (:edit this) nil)
        (PersistentHashMap'new (:cnt this), (:root this), (:has-nil? this), (:nil-value this))
    )

    (defm TransientHashMap Counted
        (Counted'''count => TransientHashMap''count)
    )

    (defm TransientHashMap ILookup
        (ILookup'''valAt => TransientHashMap''valAt)
    )

    (defm TransientHashMap IFn
        (IFn'''invoke => ATransientMap''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm TransientHashMap ITransientAssociative
        (ITransientAssociative'''assoc! => TransientHashMap''assoc!)
        (ITransientAssociative'''containsKey => ATransientMap''containsKey)
        (ITransientAssociative'''entryAt => ATransientMap''entryAt)
    )

    (defm TransientHashMap ITransientMap
        (ITransientMap'''dissoc! => TransientHashMap''dissoc!)
    )

    (defm TransientHashMap ITransientCollection
        (ITransientCollection'''conj! => TransientHashMap''conj!)
        (ITransientCollection'''persistent! => TransientHashMap''persistent!)
    )
)

(about #_"PersistentHashMap"
    (defq PersistentHashMap [#_"meta" _meta, #_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value] MapForm)

    #_inherit
    (defm PersistentHashMap APersistentMap AFn)

    (defn #_"PersistentHashMap" PersistentHashMap'new
        ([#_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value] (PersistentHashMap'new nil, cnt, root, has-nil?, nil-value))
        ([#_"meta" meta, #_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value]
            (new* PersistentHashMap'class (anew [meta, cnt, root, has-nil?, nil-value]))
        )
    )

    (def #_"PersistentHashMap" PersistentHashMap'EMPTY (PersistentHashMap'new 0, nil, false, nil))

    (defn #_"PersistentHashMap" PersistentHashMap'create-1a [#_"array" init]
        (loop-when-recur [#_"ITransientMap" m (transient PersistentHashMap'EMPTY) #_"int" i 0]
                         (< i (alength init))
                         [(assoc! m (aget init i) (aget init (inc i))) (+ i 2)]
                      => (persistent! m)
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'create-1s [#_"Seqable" init]
        (let [#_"ITransientMap" m (transient PersistentHashMap'EMPTY)]
            (loop-when [m m #_"seq" s (seq init)] (some? s) => (persistent! m)
                (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                    (recur (assoc! m (first s) (second s)) (next (next s)))
                )
            )
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'createWithCheck-1a [#_"array" init]
        (let [#_"ITransientMap" m (transient PersistentHashMap'EMPTY)]
            (loop-when [m m #_"int" i 0] (< i (alength init)) => (persistent! m)
                (let [m (assoc! m (aget init i) (aget init (inc i)))]
                    (when (= (count m) (inc (quot i 2))) => (throw! (str "duplicate key: " (aget init i)))
                        (recur m (+ i 2))
                    )
                )
            )
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'createWithCheck-1s [#_"Seqable" init]
        (let [#_"ITransientMap" m (transient PersistentHashMap'EMPTY)]
            (loop-when [m m #_"seq" s (seq init) #_"int" n 0] (some? s) => (persistent! m)
                (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                    (let [m (assoc! m (first s) (second s))]
                        (when (= (count m) (inc n)) => (throw! (str "duplicate key: " (first s)))
                            (recur m (next (next s)) (inc n))
                        )
                    )
                )
            )
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap''withMeta [#_"PersistentHashMap" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentHashMap'new meta, (:cnt this), (:root this), (:has-nil? this), (:nil-value this))
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
                (PersistentHashMap'new (:_meta this), (+ (:cnt this) (if (:has-nil? this) 0 1)), (:root this), true, val)
            )
            (let [
                #_"boolean'" addedLeaf (atom false)
                #_"node" root (INode'''assoc (or (:root this) BNode'EMPTY), 0, (f'hash key), key, val, addedLeaf)
            ]
                (when-not (= root (:root this)) => this
                    (PersistentHashMap'new (:_meta this), (+ (:cnt this) (if (deref addedLeaf) 1 0)), root, (:has-nil? this), (:nil-value this))
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
                    (PersistentHashMap'new (:_meta this), (dec (:cnt this)), (:root this), false, nil)
                )
            (nil? (:root this))
                this
            :else
                (let [#_"node" root (INode'''dissoc (:root this), 0, (f'hash key), key)]
                    (when-not (= root (:root this)) => this
                        (PersistentHashMap'new (:_meta this), (dec (:cnt this)), root, (:has-nil? this), (:nil-value this))
                    )
                )
        )
    )

    (defn #_"IPersistentCollection" PersistentHashMap''empty [#_"PersistentHashMap" this]
        (with-meta PersistentHashMap'EMPTY (:_meta this))
    )

    (defn #_"seq" PersistentHashMap''seq [#_"PersistentHashMap" this]
        (let [#_"seq" s (when (some? (:root this)) (INode'''nodeSeq (:root this)))]
            (when (:has-nil? this) => s
                (Cons'new (MapEntry'new nil, (:nil-value this)), s)
            )
        )
    )

    (defm PersistentHashMap IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentHashMap IObj
        (IObj'''withMeta => PersistentHashMap''withMeta)
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

    (defm PersistentHashMap IEditableCollection
        (IEditableCollection'''asTransient => TransientHashMap'new)
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
        (reduce #(conj (or %1 (hash-map)) %2) maps)
    )
)

(defn merge-with [f & maps]
    (when (some identity maps)
        (letfn [(merge- [m e]
                    (let [k (key e) v (val e)]
                        (assoc m k (if (contains? m k) (f (get m k) v) v))
                    )
                )]
            (reduce #(reduce merge- (or %1 (hash-map)) %2) maps)
        )
    )
)

(defn zipmap [keys vals]
    (loop-when-recur [m (transient (hash-map)) ks (seq keys) vs (seq vals)]
                     (and ks vs)
                     [(assoc! m (first ks) (first vs)) (next ks) (next vs)]
                  => (persistent! m)
    )
)
)

(about #_"arbace.arm.PersistentHashSet"

(about #_"TransientHashSet"
    (defq TransientHashSet [#_"ITransientMap" impl] #_"SetForm")

    #_inherit
    (defm TransientHashSet ATransientSet AFn)

    (defn #_"TransientHashSet" TransientHashSet'new [#_"ITransientMap" impl]
        (new* TransientHashSet'class (anew [impl]))
    )

    (defn #_"int" TransientHashSet''count [#_"TransientHashSet" this]
        (count (:impl this))
    )

    (defn #_"ITransientSet" TransientHashSet''conj! [#_"TransientHashSet" this, #_"value" val]
        (let [#_"ITransientMap" m (assoc! (:impl this) val val)]
            (when-not (= m (:impl this)) => this
                (qset! this :impl m)
            )
        )
    )

    (defn #_"PersistentHashSet" TransientHashSet''persistent! [#_"TransientHashSet" this]
        (PersistentHashSet'new nil, (persistent! (:impl this)))
    )

    (defn #_"ITransientSet" TransientHashSet''disj! [#_"TransientHashSet" this, #_"key" key]
        (let [#_"ITransientMap" m (dissoc! (:impl this) key)]
            (when-not (= m (:impl this)) => this
                (qset! this :impl m)
            )
        )
    )

    (defn #_"boolean" TransientHashSet''contains? [#_"TransientHashSet" this, #_"key" key]
        (not (identical? (get (:impl this) key this) this))
    )

    (defn #_"value" TransientHashSet''get [#_"TransientHashSet" this, #_"key" key]
        (get (:impl this) key)
    )

    (defm TransientHashSet Counted
        (Counted'''count => TransientHashSet''count)
    )

    (defm TransientHashSet ITransientCollection
        (ITransientCollection'''conj! => TransientHashSet''conj!)
        (ITransientCollection'''persistent! => TransientHashSet''persistent!)
    )

    (defm TransientHashSet ITransientSet
        (ITransientSet'''disj! => TransientHashSet''disj!)
        (ITransientSet'''contains? => TransientHashSet''contains?)
        (ITransientSet'''get => TransientHashSet''get)
    )

    (defm TransientHashSet IFn
        (IFn'''invoke => ATransientSet''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )
)

(about #_"PersistentHashSet"
    (defq PersistentHashSet [#_"meta" _meta, #_"map" impl] SetForm)

    #_inherit
    (defm PersistentHashSet APersistentSet AFn)

    (defn #_"PersistentHashSet" PersistentHashSet'new [#_"meta" meta, #_"map" impl]
        (new* PersistentHashSet'class (anew [meta, impl]))
    )

    (def #_"PersistentHashSet" PersistentHashSet'EMPTY (PersistentHashSet'new nil, PersistentHashMap'EMPTY))

    (defn #_"PersistentHashSet" PersistentHashSet'create [#_"Seqable" init]
        (into PersistentHashSet'EMPTY init)
    )

    (defn #_"PersistentHashSet" PersistentHashSet'createWithCheck [#_"Seqable" init]
        (let [#_"ITransientSet" s (transient PersistentHashSet'EMPTY)]
            (loop-when [s s #_"seq" q (seq init) #_"int" n 0] (some? q) => (persistent! s)
                (let [s (conj! s (first q))]
                    (when (= (count s) (inc n)) => (throw! (str "duplicate key: " (first q)))
                        (recur s (next q) (inc n))
                    )
                )
            )
        )
    )

    (defn #_"PersistentHashSet" PersistentHashSet''withMeta [#_"PersistentHashSet" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentHashSet'new meta, (:impl this))
        )
    )

    (defn #_"int" PersistentHashSet''count [#_"PersistentHashSet" this]
        (count (:impl this))
    )

    (defn #_"PersistentHashSet" PersistentHashSet''conj [#_"PersistentHashSet" this, #_"value" val]
        (if (contains? (:impl this) val)
            this
            (PersistentHashSet'new (:_meta this), (assoc (:impl this) val val))
        )
    )

    (defn #_"PersistentHashSet" PersistentHashSet''empty [#_"PersistentHashSet" this]
        (with-meta PersistentHashSet'EMPTY (:_meta this))
    )

    (defn #_"IPersistentSet" PersistentHashSet''disj [#_"PersistentHashSet" this, #_"key" key]
        (if (contains? (:impl this) key)
            (PersistentHashSet'new (:_meta this), (dissoc (:impl this) key))
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

    (defn #_"ITransientCollection" PersistentHashSet''asTransient [#_"PersistentHashSet" this]
        (TransientHashSet'new (transient (:impl this)))
    )

    (defm PersistentHashSet IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentHashSet IObj
        (IObj'''withMeta => PersistentHashSet''withMeta)
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

    (defm PersistentHashSet IEditableCollection
        (IEditableCollection'''asTransient => PersistentHashSet''asTransient)
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

(defn set [s] (if (set? s) (with-meta s nil) (into (hash-set) s)))
)

(about #_"arbace.arm.PersistentTreeMap"

(about #_"Black"
    (defq Black [#_"key" key])

    #_inherit
    (defm Black TNode AMapEntry APersistentVector AFn)

    (defn #_"Black" Black'new [#_"key" key]
        (new* Black'class (anew [key]))
    )

    (defn #_"node" Black''addLeft [#_"Black" this, #_"node" ins]
        (ITNode'''balanceLeft ins, this)
    )

    (defn #_"node" Black''addRight [#_"Black" this, #_"node" ins]
        (ITNode'''balanceRight ins, this)
    )

    (defn #_"node" Black''removeLeft [#_"Black" this, #_"node" del]
        (PersistentTreeMap'balanceLeftDel (:key this), (:val this), del, (:right this))
    )

    (defn #_"node" Black''removeRight [#_"Black" this, #_"node" del]
        (PersistentTreeMap'balanceRightDel (:key this), (:val this), (:left this), del)
    )

    (defn #_"node" Black''blacken [#_"Black" this]
        this
    )

    (defn #_"node" Black''redden [#_"Black" this]
        (Red'new (:key this))
    )

    (defn #_"node" Black''balanceLeft [#_"Black" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), this, (:right parent))
    )

    (defn #_"node" Black''balanceRight [#_"Black" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), this)
    )

    (defn #_"node" Black''replace [#_"Black" this, #_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (PersistentTreeMap'black key, val, left, right)
    )

    (defm Black IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm Black ITNode
        (ITNode'''addLeft => Black''addLeft)
        (ITNode'''addRight => Black''addRight)
        (ITNode'''removeLeft => Black''removeLeft)
        (ITNode'''removeRight => Black''removeRight)
        (ITNode'''blacken => Black''blacken)
        (ITNode'''redden => Black''redden)
        (ITNode'''balanceLeft => Black''balanceLeft)
        (ITNode'''balanceRight => Black''balanceRight)
        (ITNode'''replace => Black''replace)
    )

    (defm Black Sequential)

    (defm Black Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm Black Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm Black Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm Black Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm Black IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm Black Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm Black Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"BlackVal"
    (defq BlackVal [#_"key" key, #_"value" val])

    #_inherit
    (defm BlackVal Black TNode AMapEntry APersistentVector AFn)

    (defn #_"BlackVal" BlackVal'new [#_"key" key, #_"value" val]
        (new* BlackVal'class (anew [key, val]))
    )

    (defn #_"node" BlackVal''redden [#_"BlackVal" this]
        (RedVal'new (:key this), (:val this))
    )

    (defm BlackVal IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm BlackVal ITNode
        (ITNode'''addLeft => Black''addLeft)
        (ITNode'''addRight => Black''addRight)
        (ITNode'''removeLeft => Black''removeLeft)
        (ITNode'''removeRight => Black''removeRight)
        (ITNode'''blacken => Black''blacken)
        (ITNode'''redden => BlackVal''redden)
        (ITNode'''balanceLeft => Black''balanceLeft)
        (ITNode'''balanceRight => Black''balanceRight)
        (ITNode'''replace => Black''replace)
    )

    (defm BlackVal Sequential)

    (defm BlackVal Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm BlackVal Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm BlackVal Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm BlackVal Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm BlackVal IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm BlackVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm BlackVal Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"BlackBranch"
    (defq BlackBranch [#_"key" key, #_"node" left, #_"node" right])

    #_inherit
    (defm BlackBranch Black TNode AMapEntry APersistentVector AFn)

    (defn #_"BlackBranch" BlackBranch'new [#_"key" key, #_"node" left, #_"node" right]
        (new* BlackBranch'class (anew [key, left, right]))
    )

    (defn #_"node" BlackBranch''redden [#_"BlackBranch" this]
        (RedBranch'new (:key this), (:left this), (:right this))
    )

    (defm BlackBranch IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm BlackBranch ITNode
        (ITNode'''addLeft => Black''addLeft)
        (ITNode'''addRight => Black''addRight)
        (ITNode'''removeLeft => Black''removeLeft)
        (ITNode'''removeRight => Black''removeRight)
        (ITNode'''blacken => Black''blacken)
        (ITNode'''redden => BlackBranch''redden)
        (ITNode'''balanceLeft => Black''balanceLeft)
        (ITNode'''balanceRight => Black''balanceRight)
        (ITNode'''replace => Black''replace)
    )

    (defm BlackBranch Sequential)

    (defm BlackBranch Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm BlackBranch Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm BlackBranch Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm BlackBranch Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm BlackBranch IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm BlackBranch Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm BlackBranch Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"BlackBranchVal"
    (defq BlackBranchVal [#_"key" key, #_"value" val, #_"node" left, #_"node" right])

    #_inherit
    (defm BlackBranchVal BlackBranch Black TNode AMapEntry APersistentVector AFn)

    (defn #_"BlackBranchVal" BlackBranchVal'new [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (new* BlackBranchVal'class (anew [key, val, left, right]))
    )

    (defn #_"node" BlackBranchVal''redden [#_"BlackBranchVal" this]
        (RedBranchVal'new (:key this), (:val this), (:left this), (:right this))
    )

    (defm BlackBranchVal IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm BlackBranchVal ITNode
        (ITNode'''addLeft => Black''addLeft)
        (ITNode'''addRight => Black''addRight)
        (ITNode'''removeLeft => Black''removeLeft)
        (ITNode'''removeRight => Black''removeRight)
        (ITNode'''blacken => Black''blacken)
        (ITNode'''redden => BlackBranchVal''redden)
        (ITNode'''balanceLeft => Black''balanceLeft)
        (ITNode'''balanceRight => Black''balanceRight)
        (ITNode'''replace => Black''replace)
    )

    (defm BlackBranchVal Sequential)

    (defm BlackBranchVal Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm BlackBranchVal Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm BlackBranchVal Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm BlackBranchVal Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm BlackBranchVal IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm BlackBranchVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm BlackBranchVal Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"Red"
    (defq Red [#_"key" key])

    #_inherit
    (defm Red TNode AMapEntry APersistentVector AFn)

    (defn #_"Red" Red'new [#_"key" key]
        (new* Red'class (anew [key]))
    )

    (defn #_"node" Red''addLeft [#_"Red" this, #_"node" ins]
        (PersistentTreeMap'red (:key this), (:val this), ins, (:right this))
    )

    (defn #_"node" Red''addRight [#_"Red" this, #_"node" ins]
        (PersistentTreeMap'red (:key this), (:val this), (:left this), ins)
    )

    (defn #_"node" Red''removeLeft [#_"Red" this, #_"node" del]
        (PersistentTreeMap'red (:key this), (:val this), del, (:right this))
    )

    (defn #_"node" Red''removeRight [#_"Red" this, #_"node" del]
        (PersistentTreeMap'red (:key this), (:val this), (:left this), del)
    )

    (defn #_"node" Red''blacken [#_"Red" this]
        (Black'new (:key this))
    )

    (defn #_"node" Red''redden [#_"Red" this]
        (throw! "invariant violation")
    )

    (defn #_"node" Red''balanceLeft [#_"Red" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), this, (:right parent))
    )

    (defn #_"node" Red''balanceRight [#_"Red" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), this)
    )

    (defn #_"node" Red''replace [#_"Red" this, #_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (PersistentTreeMap'red key, val, left, right)
    )

    (defm Red IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm Red ITNode
        (ITNode'''addLeft => Red''addLeft)
        (ITNode'''addRight => Red''addRight)
        (ITNode'''removeLeft => Red''removeLeft)
        (ITNode'''removeRight => Red''removeRight)
        (ITNode'''blacken => Red''blacken)
        (ITNode'''redden => Red''redden)
        (ITNode'''balanceLeft => Red''balanceLeft)
        (ITNode'''balanceRight => Red''balanceRight)
        (ITNode'''replace => Red''replace)
    )

    (defm Red Sequential)

    (defm Red Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm Red Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm Red Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm Red Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm Red IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm Red Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm Red Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"RedVal"
    (defq RedVal [#_"key" key, #_"value" val])

    #_inherit
    (defm RedVal Red TNode AMapEntry APersistentVector AFn)

    (defn #_"RedVal" RedVal'new [#_"key" key, #_"value" val]
        (new* RedVal'class (anew [key, val]))
    )

    (defn #_"node" RedVal''blacken [#_"RedVal" this]
        (BlackVal'new (:key this), (:val this))
    )

    (defm RedVal IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm RedVal ITNode
        (ITNode'''addLeft => Red''addLeft)
        (ITNode'''addRight => Red''addRight)
        (ITNode'''removeLeft => Red''removeLeft)
        (ITNode'''removeRight => Red''removeRight)
        (ITNode'''blacken => RedVal''blacken)
        (ITNode'''redden => Red''redden)
        (ITNode'''balanceLeft => Red''balanceLeft)
        (ITNode'''balanceRight => Red''balanceRight)
        (ITNode'''replace => Red''replace)
    )

    (defm RedVal Sequential)

    (defm RedVal Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm RedVal Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm RedVal Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm RedVal Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm RedVal IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm RedVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm RedVal Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"RedBranch"
    (defq RedBranch [#_"key" key, #_"node" left, #_"node" right])

    #_inherit
    (defm RedBranch Red TNode AMapEntry APersistentVector AFn)

    (defn #_"RedBranch" RedBranch'new [#_"key" key, #_"node" left, #_"node" right]
        (new* RedBranch'class (anew [key, left, right]))
    )

    (defn #_"node" RedBranch''blacken [#_"RedBranch" this]
        (BlackBranch'new (:key this), (:left this), (:right this))
    )

    (defn #_"node" RedBranch''balanceLeft [#_"RedBranch" this, #_"node" parent]
        (cond (satisfies? Red (:left this))
            (do
                (PersistentTreeMap'red (:key this), (:val this), (ITNode'''blacken (:left this)), (PersistentTreeMap'black (:key parent), (:val parent), (:right this), (:right parent)))
            )
            (satisfies? Red (:right this))
            (do
                (PersistentTreeMap'red (:key (:right this)), (:val (:right this)), (PersistentTreeMap'black (:key this), (:val this), (:left this), (:left (:right this))), (PersistentTreeMap'black (:key parent), (:val parent), (:right (:right this)), (:right parent)))
            )
            :else
            (do
                (PersistentTreeMap'black (:key parent), (:val parent), this, (:right parent))
            )
        )
    )

    (defn #_"node" RedBranch''balanceRight [#_"RedBranch" this, #_"node" parent]
        (cond (satisfies? Red (:right this))
            (do
                (PersistentTreeMap'red (:key this), (:val this), (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), (:left this)), (ITNode'''blacken (:right this)))
            )
            (satisfies? Red (:left this))
            (do
                (PersistentTreeMap'red (:key (:left this)), (:val (:left this)), (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), (:left (:left this))), (PersistentTreeMap'black (:key this), (:val this), (:right (:left this)), (:right this)))
            )
            :else
            (do
                (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), this)
            )
        )
    )

    (defm RedBranch IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm RedBranch ITNode
        (ITNode'''addLeft => Red''addLeft)
        (ITNode'''addRight => Red''addRight)
        (ITNode'''removeLeft => Red''removeLeft)
        (ITNode'''removeRight => Red''removeRight)
        (ITNode'''blacken => RedBranch''blacken)
        (ITNode'''redden => Red''redden)
        (ITNode'''balanceLeft => RedBranch''balanceLeft)
        (ITNode'''balanceRight => RedBranch''balanceRight)
        (ITNode'''replace => Red''replace)
    )

    (defm RedBranch Sequential)

    (defm RedBranch Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm RedBranch Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm RedBranch Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm RedBranch Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm RedBranch IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm RedBranch Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm RedBranch Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"RedBranchVal"
    (defq RedBranchVal [#_"key" key, #_"value" val, #_"node" left, #_"node" right])

    #_inherit
    (defm RedBranchVal RedBranch Red TNode AMapEntry APersistentVector AFn)

    (defn #_"RedBranchVal" RedBranchVal'new [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (new* RedBranchVal'class (anew [key, val, left, right]))
    )

    (defn #_"node" RedBranchVal''blacken [#_"RedBranchVal" this]
        (BlackBranchVal'new (:key this), (:val this), (:left this), (:right this))
    )

    (defm RedBranchVal IMapEntry
        (IMapEntry'''key => :key)
        (IMapEntry'''val => :val)
    )

    (defm RedBranchVal ITNode
        (ITNode'''addLeft => Red''addLeft)
        (ITNode'''addRight => Red''addRight)
        (ITNode'''removeLeft => Red''removeLeft)
        (ITNode'''removeRight => Red''removeRight)
        (ITNode'''blacken => RedBranchVal''blacken)
        (ITNode'''redden => Red''redden)
        (ITNode'''balanceLeft => RedBranch''balanceLeft)
        (ITNode'''balanceRight => RedBranch''balanceRight)
        (ITNode'''replace => Red''replace)
    )

    (defm RedBranchVal Sequential)

    (defm RedBranchVal Indexed
        (Indexed'''nth => AMapEntry''nth)
    )

    (defm RedBranchVal Counted
        (Counted'''count => AMapEntry''count)
    )

    (defm RedBranchVal Seqable
        (Seqable'''seq => AMapEntry''seq)
    )

    (defm RedBranchVal Reversible
        (Reversible'''rseq => AMapEntry''rseq)
    )

    (defm RedBranchVal IObject
        (IObject'''equals => AMapEntry''equals)
    )

    (defm RedBranchVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm RedBranchVal Comparable
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"TSeq"
    (defq TSeq [#_"meta" _meta, #_"seq" stack, #_"boolean" asc?, #_"int" cnt] SeqForm)

    #_inherit
    (defm TSeq ASeq)

    (defn #_"TSeq" TSeq'new
        ([#_"seq" stack, #_"boolean" asc?] (TSeq'new stack, asc?, -1))
        ([#_"seq" stack, #_"boolean" asc?, #_"int" cnt] (TSeq'new nil, stack, asc?, cnt))
        ([#_"meta" meta, #_"seq" stack, #_"boolean" asc?, #_"int" cnt]
            (new* TSeq'class (anew [meta, stack, asc?, cnt]))
        )
    )

    (defn #_"TSeq" TSeq''withMeta [#_"TSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (TSeq'new meta, (:stack this), (:asc? this), (:cnt this))
        )
    )

    (defn #_"seq" TSeq'push [#_"node" t, #_"seq" stack, #_"boolean" asc?]
        (loop-when [stack stack t t] (some? t) => stack
            (recur (cons t stack) (if asc? (:left t) (:right t)))
        )
    )

    (defn #_"TSeq" TSeq'create [#_"node" t, #_"boolean" asc?, #_"int" cnt]
        (TSeq'new (TSeq'push t, nil, asc?), asc?, cnt)
    )

    (defn #_"seq" TSeq''seq [#_"TSeq" this]
        this
    )

    (defn #_"Object" TSeq''first [#_"TSeq" this]
        (first (:stack this))
    )

    (defn #_"seq" TSeq''next [#_"TSeq" this]
        (let [#_"node" t #_"node" (first (:stack this)) #_"boolean" asc? (:asc? this)]
            (when-some [#_"seq" stack (TSeq'push (if asc? (:right t) (:left t)), (next (:stack this)), asc?)]
                (TSeq'new stack, asc?, (dec (:cnt this)))
            )
        )
    )

    (defn #_"int" TSeq''count [#_"TSeq" this]
        (when (neg? (:cnt this)) => (:cnt this)
            (count (:stack this))
        )
    )

    (defm TSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm TSeq IObj
        (IObj'''withMeta => TSeq''withMeta)
    )

    (defm TSeq Sequential)

    (defm TSeq Seqable
        (Seqable'''seq => TSeq''seq)
    )

    (defm TSeq ISeq
        (ISeq'''first => TSeq''first)
        (ISeq'''next => TSeq''next)
    )

    (defm TSeq Counted
        (Counted'''count => TSeq''count)
    )

    (defm TSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm TSeq IObject
        (IObject'''equals => ASeq''equals)
    )
)

(about #_"PersistentTreeMap"
    (defq PersistentTreeMap [#_"meta" _meta, #_"Comparator" cmp, #_"node" tree, #_"int" cnt] MapForm)

    #_inherit
    (defm PersistentTreeMap APersistentMap AFn)

    (defn #_"PersistentTreeMap" PersistentTreeMap'new
        ([] (PersistentTreeMap'new compare))
        ([#_"Comparator" cmp] (PersistentTreeMap'new nil, cmp))
        ([#_"meta" meta, #_"Comparator" cmp] (PersistentTreeMap'new meta, cmp, nil, 0))
        ([#_"meta" meta, #_"Comparator" cmp, #_"node" tree, #_"int" cnt]
            (new* PersistentTreeMap'class (anew [meta, cmp, tree, cnt]))
        )
    )

    (def #_"PersistentTreeMap" PersistentTreeMap'EMPTY (PersistentTreeMap'new))

    (defn #_"PersistentTreeMap" PersistentTreeMap'create
        ([#_"Seqable" keyvals] (PersistentTreeMap'create nil, keyvals))
        ([#_"Comparator" cmp, #_"Seqable" keyvals]
            (let [#_"PersistentTreeMap" m (if (some? cmp) (PersistentTreeMap'new cmp) PersistentTreeMap'EMPTY)]
                (loop-when [m m #_"seq" s (seq keyvals)] (some? s) => m
                    (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                        (recur (assoc m (first s) (second s)) (next (next s)))
                    )
                )
            )
        )
    )

    (defn #_"IPersistentCollection" PersistentTreeMap''empty [#_"PersistentTreeMap" this]
        (PersistentTreeMap'new (:_meta this), (:cmp this))
    )

    (defn #_"PersistentTreeMap" PersistentTreeMap''withMeta [#_"PersistentTreeMap" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentTreeMap'new meta, (:cmp this), (:tree this), (:cnt this))
        )
    )

    (defn #_"int" PersistentTreeMap''doCompare [#_"PersistentTreeMap" this, #_"key" a, #_"key" b]
        (Comparator''compare (:cmp this), a, b)
    )

    (defn #_"key" PersistentTreeMap''entryKey [#_"PersistentTreeMap" this, #_"pair" entry]
        (key entry)
    )

    (defn #_"seq" PersistentTreeMap''seq
        ([#_"PersistentTreeMap" this] (PersistentTreeMap''seq this, true))
        ([#_"PersistentTreeMap" this, #_"boolean" ascending?]
            (when (pos? (:cnt this))
                (TSeq'create (:tree this), ascending?, (:cnt this))
            )
        )
    )

    (defn #_"seq" PersistentTreeMap''seqFrom [#_"PersistentTreeMap" this, #_"key" key, #_"boolean" ascending?]
        (when (pos? (:cnt this))
            (loop-when [#_"seq" s nil #_"node" t (:tree this)] (some? t) => (when (some? s) (TSeq'new s, ascending?))
                (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                    (cond
                        (zero? cmp) (TSeq'new (cons t s), ascending?)
                        ascending?  (if (neg? cmp) (recur (cons t s) (:left t)) (recur s (:right t)))
                        :else       (if (pos? cmp) (recur (cons t s) (:right t)) (recur s (:left t)))
                    )
                )
            )
        )
    )

    (defn #_"seq" PersistentTreeMap''rseq [#_"PersistentTreeMap" this]
        (PersistentTreeMap''seq this, false)
    )

    (defn #_"node" PersistentTreeMap''entryAt [#_"PersistentTreeMap" this, #_"key" key]
        (loop-when [#_"node" t (:tree this)] (some? t) => t
            (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                (cond
                    (neg? cmp) (recur (:left t))
                    (pos? cmp) (recur (:right t))
                    :else      t
                )
            )
        )
    )

    (defn #_"value" PersistentTreeMap''valAt
        ([#_"PersistentTreeMap" this, #_"key" key] (PersistentTreeMap''valAt this, key, nil))
        ([#_"PersistentTreeMap" this, #_"key" key, #_"value" not-found]
            (when-some [#_"node" node (PersistentTreeMap''entryAt this, key)] => not-found
                (IMapEntry'''val node)
            )
        )
    )

    (defn #_"boolean" PersistentTreeMap''containsKey [#_"PersistentTreeMap" this, #_"key" key]
        (some? (PersistentTreeMap''entryAt this, key))
    )

    (defn #_"node" PersistentTreeMap''min [#_"PersistentTreeMap" this]
        (when-some [#_"node" t (:tree this)]
            (loop-when-recur t (some? (:left t)) (:left t) => t)
        )
    )

    (defn #_"node" PersistentTreeMap''max [#_"PersistentTreeMap" this]
        (when-some [#_"node" t (:tree this)]
            (loop-when-recur t (some? (:right t)) (:right t) => t)
        )
    )

    (defn #_"key" PersistentTreeMap''minKey [#_"PersistentTreeMap" this]
        (let [#_"node" t (PersistentTreeMap''min this)]
            (when (some? t) (:key t))
        )
    )

    (defn #_"key" PersistentTreeMap''maxKey [#_"PersistentTreeMap" this]
        (let [#_"node" t (PersistentTreeMap''max this)]
            (when (some? t) (:key t))
        )
    )

    (defn #_"int" PersistentTreeMap''depth
        ([#_"PersistentTreeMap" this] (PersistentTreeMap''depth this, (:tree this)))
        ([#_"PersistentTreeMap" this, #_"node" t]
            (when (some? t) => 0
                (inc (max (PersistentTreeMap''depth this, (:left t)) (PersistentTreeMap''depth this, (:right t))))
            )
        )
    )

    (defn #_"Red" PersistentTreeMap'red [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (if (and (nil? left) (nil? right))
            (if (nil? val)
                (Red'new key)
                (RedVal'new key, val)
            )
            (if (nil? val)
                (RedBranch'new key, left, right)
                (RedBranchVal'new key, val, left, right)
            )
        )
    )

    (defn #_"Black" PersistentTreeMap'black [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (if (and (nil? left) (nil? right))
            (if (nil? val)
                (Black'new key)
                (BlackVal'new key, val)
            )
            (if (nil? val)
                (BlackBranch'new key, left, right)
                (BlackBranchVal'new key, val, left, right)
            )
        )
    )

    (defn #_"node" PersistentTreeMap'rightBalance [#_"key" key, #_"value" val, #_"node" left, #_"node" ins]
        (cond
            (and (satisfies? Red ins) (satisfies? Red (:right ins)))
                (PersistentTreeMap'red (:key ins), (:val ins), (PersistentTreeMap'black key, val, left, (:left ins)), (ITNode'''blacken (:right ins)))
            (and (satisfies? Red ins) (satisfies? Red (:left ins)))
                (PersistentTreeMap'red (:key (:left ins)), (:val (:left ins)), (PersistentTreeMap'black key, val, left, (:left (:left ins))), (PersistentTreeMap'black (:key ins), (:val ins), (:right (:left ins)), (:right ins)))
            :else
                (PersistentTreeMap'black key, val, left, ins)
        )
    )

    (defn #_"node" PersistentTreeMap'balanceLeftDel [#_"key" key, #_"value" val, #_"node" del, #_"node" right]
        (cond
            (satisfies? Red del)
                (PersistentTreeMap'red key, val, (ITNode'''blacken del), right)
            (satisfies? Black right)
                (PersistentTreeMap'rightBalance key, val, del, (ITNode'''redden right))
            (and (satisfies? Red right) (satisfies? Black (:left right)))
                (PersistentTreeMap'red (:key (:left right)), (:val (:left right)), (PersistentTreeMap'black key, val, del, (:left (:left right))), (PersistentTreeMap'rightBalance (:key right), (:val right), (:right (:left right)), (ITNode'''redden (:right right))))
            :else
                (throw! "invariant violation")
        )
    )

    (defn #_"node" PersistentTreeMap'leftBalance [#_"key" key, #_"value" val, #_"node" ins, #_"node" right]
        (cond
            (and (satisfies? Red ins) (satisfies? Red (:left ins)))
                (PersistentTreeMap'red (:key ins), (:val ins), (ITNode'''blacken (:left ins)), (PersistentTreeMap'black key, val, (:right ins), right))
            (and (satisfies? Red ins) (satisfies? Red (:right ins)))
                (PersistentTreeMap'red (:key (:right ins)), (:val (:right ins)), (PersistentTreeMap'black (:key ins), (:val ins), (:left ins), (:left (:right ins))), (PersistentTreeMap'black key, val, (:right (:right ins)), right))
            :else
                (PersistentTreeMap'black key, val, ins, right)
        )
    )

    (defn #_"node" PersistentTreeMap'balanceRightDel [#_"key" key, #_"value" val, #_"node" left, #_"node" del]
        (cond
            (satisfies? Red del)
                (PersistentTreeMap'red key, val, left, (ITNode'''blacken del))
            (satisfies? Black left)
                (PersistentTreeMap'leftBalance key, val, (ITNode'''redden left), del)
            (and (satisfies? Red left) (satisfies? Black (:right left)))
                (PersistentTreeMap'red (:key (:right left)), (:val (:right left)), (PersistentTreeMap'leftBalance (:key left), (:val left), (ITNode'''redden (:left left)), (:left (:right left))), (PersistentTreeMap'black key, val, (:right (:right left)), del))
            :else
                (throw! "invariant violation")
        )
    )

    (defn #_"node" PersistentTreeMap''add [#_"PersistentTreeMap" this, #_"node" t, #_"key" key, #_"value" val, #_"node'" found]
        (if (nil? t)
            (if (nil? val)
                (Red'new key)
                (RedVal'new key, val)
            )
            (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                (if (zero? cmp)
                    (do
                        (reset! found t)
                        nil
                    )
                    (let [#_"node" ins (PersistentTreeMap''add this, (if (neg? cmp) (:left t) (:right t)), key, val, found)]
                        (when (some? ins) => nil
                            (if (neg? cmp) (ITNode'''addLeft t, ins) (ITNode'''addRight t, ins))
                        )
                    )
                )
            )
        )
    )

    (defn #_"node" PersistentTreeMap'append [#_"node" left, #_"node" right]
        (cond
            (nil? left)
                right
            (nil? right)
                left
            (satisfies? Red left)
                (if (satisfies? Red right)
                    (let [#_"node" app (PersistentTreeMap'append (:right left), (:left right))]
                        (if (satisfies? Red app)
                            (PersistentTreeMap'red (:key app), (:val app), (PersistentTreeMap'red (:key left), (:val left), (:left left), (:left app)), (PersistentTreeMap'red (:key right), (:val right), (:right app), (:right right)))
                            (PersistentTreeMap'red (:key left), (:val left), (:left left), (PersistentTreeMap'red (:key right), (:val right), app, (:right right)))
                        )
                    )
                    (PersistentTreeMap'red (:key left), (:val left), (:left left), (PersistentTreeMap'append (:right left), right))
                )
            (satisfies? Red right)
                (PersistentTreeMap'red (:key right), (:val right), (PersistentTreeMap'append left, (:left right)), (:right right))
            :else
                (let [#_"node" app (PersistentTreeMap'append (:right left), (:left right))]
                    (if (satisfies? Red app)
                        (PersistentTreeMap'red (:key app), (:val app), (PersistentTreeMap'black (:key left), (:val left), (:left left), (:left app)), (PersistentTreeMap'black (:key right), (:val right), (:right app), (:right right)))
                        (PersistentTreeMap'balanceLeftDel (:key left), (:val left), (:left left), (PersistentTreeMap'black (:key right), (:val right), app, (:right right)))
                    )
                )
        )
    )

    (defn #_"node" PersistentTreeMap''remove [#_"PersistentTreeMap" this, #_"node" t, #_"key" key, #_"node'" found]
        (when (some? t) => nil
            (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                (if (zero? cmp)
                    (do
                        (reset! found t)
                        (PersistentTreeMap'append (:left t), (:right t))
                    )
                    (let [#_"node" del (PersistentTreeMap''remove this, (if (neg? cmp) (:left t) (:right t)), key, found)]
                        (when (or (some? del) (some? (deref found))) => nil
                            (if (neg? cmp)
                                (if (satisfies? Black (:left t))
                                    (PersistentTreeMap'balanceLeftDel (:key t), (:val t), del, (:right t))
                                    (PersistentTreeMap'red (:key t), (:val t), del, (:right t))
                                )
                                (if (satisfies? Black (:right t))
                                    (PersistentTreeMap'balanceRightDel (:key t), (:val t), (:left t), del)
                                    (PersistentTreeMap'red (:key t), (:val t), (:left t), del)
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"node" PersistentTreeMap''replace [#_"PersistentTreeMap" this, #_"node" t, #_"key" key, #_"value" val]
        (let [
            #_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))
            #_"node" left  (if (neg? cmp) (PersistentTreeMap''replace this, (:left  t), key, val) (:left  t))
            #_"node" right (if (pos? cmp) (PersistentTreeMap''replace this, (:right t), key, val) (:right t))
        ]
            (ITNode'''replace t, (:key t), (if (zero? cmp) val (:val t)), left, right)
        )
    )

    (defn #_"PersistentTreeMap" PersistentTreeMap''assoc [#_"PersistentTreeMap" this, #_"key" key, #_"value" val]
        (let [#_"node'" found (atom nil) #_"node" t (PersistentTreeMap''add this, (:tree this), key, val, found)]
            (if (nil? t)
                (if (= (:val #_"node" (deref found)) val)
                    this
                    (PersistentTreeMap'new (:_meta this), (:cmp this), (PersistentTreeMap''replace this, (:tree this), key, val), (:cnt this))
                )
                (PersistentTreeMap'new (:_meta this), (:cmp this), (ITNode'''blacken t), (inc (:cnt this)))
            )
        )
    )

    (defn #_"PersistentTreeMap" PersistentTreeMap''dissoc [#_"PersistentTreeMap" this, #_"key" key]
        (let [#_"node'" found (atom nil) #_"node" t (PersistentTreeMap''remove this, (:tree this), key, found)]
            (if (nil? t)
                (if (nil? (deref found))
                    this
                    (PersistentTreeMap'new (:_meta this), (:cmp this))
                )
                (PersistentTreeMap'new (:_meta this), (:cmp this), (ITNode'''blacken t), (dec (:cnt this)))
            )
        )
    )

    (defm PersistentTreeMap IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentTreeMap IObj
        (IObj'''withMeta => PersistentTreeMap''withMeta)
    )

    (defm PersistentTreeMap Counted
        (Counted'''count => :cnt)
    )

    (defm PersistentTreeMap ILookup
        (ILookup'''valAt => PersistentTreeMap''valAt)
    )

    (defm PersistentTreeMap IFn
        (IFn'''invoke => APersistentMap''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm PersistentTreeMap Seqable
        (Seqable'''seq => PersistentTreeMap''seq)
    )

    (defm PersistentTreeMap Reversible
        (Reversible'''rseq => PersistentTreeMap''rseq)
    )

    (defm PersistentTreeMap IPersistentCollection
        (IPersistentCollection'''conj => APersistentMap''conj)
        (IPersistentCollection'''empty => PersistentTreeMap''empty)
    )

    (defm PersistentTreeMap Sorted
        (Sorted'''comparator => :cmp)
        (Sorted'''entryKey => PersistentTreeMap''entryKey)
        (Sorted'''seq => PersistentTreeMap''seq)
        (Sorted'''seqFrom => PersistentTreeMap''seqFrom)
    )

    (defm PersistentTreeMap Associative
        (Associative'''assoc => PersistentTreeMap''assoc)
        (Associative'''containsKey => PersistentTreeMap''containsKey)
        (Associative'''entryAt => PersistentTreeMap''entryAt)
    )

    (defm PersistentTreeMap IPersistentMap
        (IPersistentMap'''dissoc => PersistentTreeMap''dissoc)
    )

    (defm PersistentTreeMap IObject
        (IObject'''equals => APersistentMap''equals)
    )

    (defm PersistentTreeMap Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

(defn sorted-map [& keyvals] (PersistentTreeMap'create keyvals))

(defn sorted-map-by [cmp & keyvals] (PersistentTreeMap'create cmp keyvals))
)

(about #_"arbace.arm.PersistentTreeSet"

(about #_"PersistentTreeSet"
    (defq PersistentTreeSet [#_"meta" _meta, #_"map" impl] SetForm)

    #_inherit
    (defm PersistentTreeSet APersistentSet AFn)

    (defn #_"PersistentTreeSet" PersistentTreeSet'new [#_"meta" meta, #_"map" impl]
        (new* PersistentTreeSet'class (anew [meta, impl]))
    )

    (def #_"PersistentTreeSet" PersistentTreeSet'EMPTY (PersistentTreeSet'new nil, PersistentTreeMap'EMPTY))

    (defn #_"PersistentTreeSet" PersistentTreeSet'create
        ([                    #_"Seqable" init] (into PersistentTreeSet'EMPTY                                       init))
        ([#_"Comparator" cmp, #_"Seqable" init] (into (PersistentTreeSet'new nil, (PersistentTreeMap'new nil, cmp)) init))
    )

    (defn #_"PersistentTreeSet" PersistentTreeSet''withMeta [#_"PersistentTreeSet" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentTreeSet'new meta, (:impl this))
        )
    )

    (defn #_"int" PersistentTreeSet''count [#_"PersistentTreeSet" this]
        (count (:impl this))
    )

    (defn #_"PersistentTreeSet" PersistentTreeSet''conj [#_"PersistentTreeSet" this, #_"value" val]
        (if (contains? (:impl this) val)
            this
            (PersistentTreeSet'new (:_meta this), (assoc (:impl this) val val))
        )
    )

    (defn #_"PersistentTreeSet" PersistentTreeSet''empty [#_"PersistentTreeSet" this]
        (PersistentTreeSet'new (:_meta this), (empty (:impl this)))
    )

    (defn #_"IPersistentSet" PersistentTreeSet''disj [#_"PersistentTreeSet" this, #_"key" key]
        (if (contains? (:impl this) key)
            (PersistentTreeSet'new (:_meta this), (dissoc (:impl this) key))
            this
        )
    )

    (defn #_"boolean" PersistentTreeSet''contains? [#_"PersistentTreeSet" this, #_"key" key]
        (contains? (:impl this) key)
    )

    (defn #_"value" PersistentTreeSet''get [#_"PersistentTreeSet" this, #_"key" key]
        (get (:impl this) key)
    )

    (defn #_"Comparator" PersistentTreeSet''comparator [#_"PersistentTreeSet" this]
        (Sorted'''comparator (:impl this))
    )

    (defn #_"value" PersistentTreeSet''entryKey [#_"PersistentTreeSet" this, #_"value" entry]
        entry
    )

    (defn #_"seq" PersistentTreeSet''seq
        ([#_"PersistentTreeSet" this]
            (keys (:impl this))
        )
        ([#_"PersistentTreeSet" this, #_"boolean" ascending?]
            (keys (Sorted'''seq (:impl this), ascending?))
        )
    )

    (defn #_"seq" PersistentTreeSet''seqFrom [#_"PersistentTreeSet" this, #_"key" key, #_"boolean" ascending?]
        (keys (Sorted'''seqFrom (:impl this), key, ascending?))
    )

    (defn #_"seq" PersistentTreeSet''rseq [#_"PersistentTreeSet" this]
        (map key (rseq (:impl this)))
    )

    (defm PersistentTreeSet IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentTreeSet IObj
        (IObj'''withMeta => PersistentTreeSet''withMeta)
    )

    (defm PersistentTreeSet Counted
        (Counted'''count => PersistentTreeSet''count)
    )

    (defm PersistentTreeSet IPersistentCollection
        (IPersistentCollection'''conj => PersistentTreeSet''conj)
        (IPersistentCollection'''empty => PersistentTreeSet''empty)
    )

    (defm PersistentTreeSet IPersistentSet
        (IPersistentSet'''disj => PersistentTreeSet''disj)
        (IPersistentSet'''contains? => PersistentTreeSet''contains?)
        (IPersistentSet'''get => PersistentTreeSet''get)
    )

    (defm PersistentTreeSet Sorted
        (Sorted'''comparator => PersistentTreeSet''comparator)
        (Sorted'''entryKey => PersistentTreeSet''entryKey)
        (Sorted'''seq => PersistentTreeSet''seq)
        (Sorted'''seqFrom => PersistentTreeSet''seqFrom)
    )

    (defm PersistentTreeSet Seqable
        (Seqable'''seq => PersistentTreeSet''seq)
    )

    (defm PersistentTreeSet Reversible
        (Reversible'''rseq => PersistentTreeSet''rseq)
    )

    (defm PersistentTreeSet IFn
        (IFn'''invoke => APersistentSet''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    (defm PersistentTreeSet IObject
        (IObject'''equals => APersistentSet''equals)
    )

    (defm PersistentTreeSet Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

(defn sorted-set [& keys] (PersistentTreeSet'create keys))

(defn sorted-set-by [cmp & keys] (PersistentTreeSet'create cmp keys))
)

(about #_"arbace.arm.PersistentVector"

(about #_"VNode"
    (defq VNode [#_"thread'" edit, #_"array" array, #_"index" index])

    (defn #_"node" VNode'new [#_"thread'" edit, #_"array" array, #_"index" index]
        (new* VNode'class (anew [edit, (or array (anew 32)), index]))
    )

    (def #_"node" VNode'EMPTY (VNode'new nil, nil, nil))

    (defn #_"void" VNode''assert-editable [#_"node" this]
        (let [
            #_"thread" owner (deref (or (:edit this) (throw! "transient use of persistent data")))
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

    (defn #_"boolean" VNode''cow? [#_"node" this, #_"thread'" edit]
        (let [
            #_"thread'" e (:edit this)
        ]
            (or (nil? e) (nil? (deref e)) (not (or (identical? e edit) (throw! "transient cow!"))))
        )
    )

    (defn #_"node" VNode''editable-root [#_"node" this]
        (VNode'new (atom (thread)), (aclone (:array this)), (aclone (:index this)))
    )

    (defn #_"values" VNode'editable-tail [#_"values" tail]
        (-> (anew 32) (acopy! 0 tail 0 (alength tail)))
    )

    (defn #_"values" VNode''array-for
        ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt] (VNode''array-for this, i, shift, cnt, cnt, nil))
        ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
            (when (< -1 i cnt) => (throw! "index is out of bounds")
                (when (< i tail-off) => tail
                    (loop-when [i i #_"node" node this shift shift] (pos? shift) => (:array node)
                        (let [
                            #_"index" x (:index node)
                            #_"int" m (bit-and (>>> i shift) 0x1f)
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
                (loop-when [i i #_"node" node this shift shift] (pos? shift) => (aget (:array node) (bit-and (>>> i shift) 0x1f))
                    (let [
                        #_"index" x (:index node)
                        #_"int" m (bit-and (>>> i shift) 0x1f)
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

    (defn #_"node" VNode''new-path [#_"node" this, #_"thread'" edit, #_"int" shift]
        (when (pos? shift) => this
            (VNode'new edit, (-> (anew 32) (aset! 0 (VNode''new-path this, edit, (- shift 5)))), nil)
        )
    )

    (defn #_"int" VNode'last-range [#_"index" x]
        (aget x (dec (aget x 32)))
    )

    (defn #_"boolean" VNode''overflow? [#_"node" this, #_"int" shift, #_"int" cnt]
        (let [
            #_"index" x (:index this)
        ]
            (when (some? x) => (< (<< 1 shift) (>>> (inc cnt) 5))
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

    (defn #_"node" VNode''push-tail [#_"node" this, #_"thread'" edit, #_"int" shift, #_"int" cnt, #_"node" tail-node]
        (let [
            #_"boolean" cow? (VNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
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
                                    (VNode''push-tail (aget a e), edit, (- shift 5), (inc n), tail-node)
                                )
                            )
                        )
                    a (if cow? (aclone a) a) x (if cow? (aclone x) x)
                    [a x]
                        (if (some? child)
                            [(aset! a e child) (aswap! x e + 32)]
                            (let [
                                a (aset! a (inc e) (VNode''new-path tail-node, edit, (- shift 5)))
                                x (aset! x (inc e) (+ (aget x e) 32))
                            ]
                                [a (aswap! x 32 inc)]
                            )
                        )
                ]
                    (if cow? (VNode'new edit, a, x) this)
                )
                (let [
                    #_"int" e (bit-and (>>> (dec cnt) shift) 0x1f)
                    #_"node" child
                        (when (< 5 shift) => tail-node
                            (if-some [child (aget a e)]
                                (VNode''push-tail child, edit, (- shift 5), cnt, tail-node)
                                (VNode''new-path tail-node, edit, (- shift 5))
                            )
                        )
                    a (if cow? (aclone a) a)
                    a (aset! a e child)
                ]
                    (if cow? (VNode'new edit, a, nil) this)
                )
            )
        )
    )

    (defn #_"node" VNode''pop-tail [#_"node" this, #_"thread'" edit, #_"int" shift, #_"int" tail-off]
        (let [
            #_"boolean" cow? (VNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
            #_"int" e (bit-and (>>> (dec tail-off) shift) 0x1f)
        ]
            (if (some? x)
                (let [
                    e (loop-when-recur e (and (< e 31) (some? (aget x (inc e)))) (inc e) => e)
                ]
                    (cond
                        (< 5 shift)
                            (let [
                                #_"node" child (aget a e)
                                #_"node" child' (VNode''pop-tail child, edit, (- shift 5), (if (pos? e) (- (aget x e) (aget x (dec e))) (aget x 0)))
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
                                                            (- (VNode'last-range (:index child)) (VNode'last-range (:index child')))
                                                        )
                                                ]
                                                    (-> x (aswap! e - delta))
                                                )
                                                (-> x (aset! e nil) (aswap! 32 dec))
                                            )
                                    ]
                                        (if cow? (VNode'new edit, a, x) this)
                                    )
                                )
                            )
                        (pos? e)
                            (let [
                                a (-> (if cow? (aclone a) a) (aset! e nil))
                                x (-> (if cow? (aclone x) x) (aset! e nil) (aswap! 32 dec))
                            ]
                                (if cow? (VNode'new edit, a, x) this)
                            )
                    )
                )
                (cond
                    (< 5 shift)
                        (let [
                            #_"node" child (VNode''pop-tail (aget a e), edit, (- shift 5), tail-off)
                        ]
                            (when (or (some? child) (pos? e))
                                (let [
                                    a (if cow? (aclone a) a)
                                    a (aset! a e child)
                                ]
                                    (if cow? (VNode'new edit, a, nil) this)
                                )
                            )
                        )
                    (pos? e)
                        (let [
                            a (if cow? (aclone a) a)
                            a (aset! a e nil)
                        ]
                            (if cow? (VNode'new edit, a, nil) this)
                        )
                )
            )
        )
    )

    (defn #_"node" VNode''do-assoc [#_"node" this, #_"thread'" edit, #_"int" shift, #_"int" i, #_"value" val]
        (let [
            #_"boolean" cow? (VNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
            a (if cow? (aclone a) a)
            #_"int" m (bit-and (>>> i shift) 0x1f)
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
                        (aswap! a m VNode''do-assoc edit, (- shift 5), i, val)
                    )
                )
        ]
            (if cow? (VNode'new edit, a, x) this)
        )
    )

    (defn #_"index" VNode'n-index [#_"int" shift, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
        ]
            (loop-when-recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< i n) [(aset! x j i) (inc j) (+ i k)] => (-> x (aset! j n) (aset! 32 (inc j))))
        )
    )

    (defn #_"index" VNode'm-n-index [#_"int" shift, #_"int" m, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
        ]
            (loop-when-recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< j m) [(aset! x j i) (inc j) (+ i k)] => (-> x (aset! j n) (aset! 32 (inc j))))
        )
    )

    (defn #_"int" VNode'index-of-nil [#_"array" a]
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

    (defn #_"node" VNode''first-child [#_"node" this]
        (aget (:array this) 0)
    )

    (defn #_"node" VNode''last-child [#_"node" this]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (aget a (dec (if (some? x) (aget x 32) (VNode'index-of-nil a))))
        )
    )

    (defn #_"node" VNode''remove-leftmost-child [#_"node" this]
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
                    (VNode'new nil, (-> (anew 32) (acopy! 0 a 1 31)), x')
                )
            )
        )
    )

    (defn #_"node" VNode''replace-leftmost-child [#_"node" this, #_"int" shift, #_"int" cnt, #_"node" node, #_"int" delta]
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
                        #_"int" n (bit-and (>>> (dec cnt) shift) 0x1f)
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
            (VNode'new nil, a', x')
        )
    )

    (defn #_"node" VNode''replace-rightmost-child [#_"node" this, #_"int" shift, #_"node" node, #_"int" delta]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (if (some? x)
                (let [
                    #_"int" e (dec (aget x 32))
                ]
                    (VNode'new nil, (-> (aclone a) (aset! e node)), (-> (aclone x) (aset! e (+ (aget x e) delta))))
                )
                (let [
                    #_"int" m (dec (VNode'index-of-nil a))
                ]
                    (if (some? (:index node))
                        (VNode'new nil, (-> (anew 32) (acopy! 0 a 0 m) (aset! m node)), (VNode'm-n-index shift, m, (VNode'last-range (:index node))))
                        (VNode'new nil, (-> (aclone a) (aset! m node)), nil)
                    )
                )
            )
        )
    )

    (defn #_"node" VNode''fold-tail [#_"node" this, #_"int" shift, #_"int" tail-off, #_"values" tail]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
            #_"int" m (VNode'index-of-nil a)
            #_"node" tail-node
                (when (< 5 shift) => (VNode'new nil, tail, nil)
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
                        (VNode''fold-tail (aget a (dec m)), (- shift 5), n, tail)
                    )
                )
        ]
            (when (or (< m 32) (and (some? tail-node) (< 5 shift)))
                (let [
                    #_"int" n (alength tail)
                    #_"index" x'
                        (when (or (some? x) (< n 32))
                            (let [
                                x' (or (aclone x) (VNode'n-index shift, tail-off))
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
                            (aset! a' m (VNode''new-path (VNode'new nil, tail, nil), nil, (- shift 5)))
                        )
                ]
                    (VNode'new nil, a', x')
                )
            )
        )
    )

    (def #_"int" VNode'rrbt-concat-threshold 33)
    (def #_"int" VNode'max-extra-search-steps 2)

    (defn #_"node" VNode''slice-right [#_"node" this, #_"int" shift, #_"int" end]
        (when (pos? shift) => (VNode'new nil, (-> (anew end) (acopy! 0 (:array this) 0 end)), nil)
            (let [
                #_"array" a (:array this) #_"index" x (:index this)
                #_"int" m (bit-and (>>> (dec end) shift) 0x1f)
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
                #_"node" child (VNode''slice-right (aget a m), (- shift 5), child-end)
                #_"index" y (:index child)
                #_"array" a' (-> (anew 32) (acopy! 0 a 0 m) (aset! m child))
                #_"index" x'
                    (when (or (some? x) (some? y))
                        (let [
                            x' (loop-when-recur [x' (anew 33) #_"int" j 0] (< j m) [(aset! x' j (if (some? x) (aget x j) (* (inc j) k))) (inc j)] => x')
                            #_"int" delta
                                (cond
                                    (nil? y)    (let [#_"int" e (rem child-end k) ] (if (zero? e) k e))
                                    (< 5 shift) (VNode'last-range y)
                                    :else       (alength (:array child))
                                )
                            x' (aset! x' m (+ (if (pos? m) (aget x' (dec m)) 0) delta))
                        ]
                            (-> x' (aset! 32 (inc m)))
                        )
                    )
            ]
                (VNode'new nil, a', x')
            )
        )
    )

    (defn #_"node" VNode''slice-left [#_"node" this, #_"int" shift, #_"int" start, #_"int" end]
        (if (zero? shift)
            (let [
                #_"array" a (:array this)
                #_"int" n (- (alength a) start)
            ]
                (VNode'new nil, (-> (anew n) (acopy! 0 a start n)), nil)
            )
            (let [
                #_"array" a (:array this) #_"index" x (:index this)
                #_"int" m (bit-and (>>> start shift) 0x1f)
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
                        (VNode''slice-left (aget a m), (- shift 5), (if (pos? m) (- start i) start), (min k (if (pos? m) (- end i) end)))
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
                                            (VNode'last-range (:index child))
                                            (- k (bit-and (>>> start (- shift 5)) 0x1f))
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
                        (VNode'new nil, a', x')
                    )
                )
            )
        )
    )

    (defn #_"node" VNode''shift-from-to [#_"node" this, #_"int" from, #_"int" to]
        (when-not (= from to) => this
            (let [
                #_"index" x'
                    (when (some? (:index this))
                        (-> (anew 33) (aset! 0 (VNode'last-range (:index this))) (aset! 32 1))
                    )
            ]
                (recur (VNode'new nil, (-> (anew 32) (aset! 0 this)), x') (+ 5 from) to)
            )
        )
    )

    (defn #_"int" VNode''leaf-count [#_"node" this, #_"int" shift]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (cond
                (zero? shift) (alength a)
                (some? x)     (aget x 32)
                :else         (VNode'index-of-nil a)
            )
        )
    )

    (defn #_"int" VNode''tree-count [#_"node" this, #_"int" shift]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (loop-when-recur [#_"int" i 0 #_"int" n 0]
                             (if (some? x) (< i (aget x 32)) (and (< i 32) (some? (aget a i))))
                             [(inc i) (+ n (VNode''leaf-count (aget a i), (- shift 5)))]
                          => n
            )
        )
    )

    (defn #_"seq" VNode''leaf-seq [#_"node" this]
        (let [
            #_"array" a (:array this)
        ]
            (mapcat :array (take (VNode'index-of-nil a) a))
        )
    )

    (defn #_"[node node int]" VNode'rebalance-leaves [#_"node" node1, #_"node" node2, #_"int" delta]
        (let [
            #_"int" n1 (VNode''tree-count node1, 5) #_"int" n2 (VNode''tree-count node2, 5) #_"int" n (+ n1 n2)
        ]
            (when (< VNode'max-extra-search-steps (- (+ (VNode''leaf-count node1, 5) (VNode''leaf-count node2, 5)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
                (let [
                    #_"seq" s (map #(VNode'new nil, (anew %), nil) (partition-all 32 (concat (VNode''leaf-seq node1) (VNode''leaf-seq node2))))
                ]
                    (if (<= n (* 32 32))
                        (let [
                            #_"index" x' (when-not (zero? (rem n 32)) (VNode'n-index 5, n))
                        ]
                            [(VNode'new nil, (anew s), x') nil n2]
                        )
                        (let [
                            #_"index" x' (when-not (zero? (rem n 32)) (VNode'n-index 5, (- n (* 32 32))))
                        ]
                            [(VNode'new nil, (anew (take 32 s)), nil) (VNode'new nil, (anew (drop 32 s)), x') (- (* 32 32) n1)]
                        )
                    )
                )
            )
        )
    )

    (defn #_"seq" VNode''child-seq [#_"node" this, #_"int" shift, #_"int" cnt]
        (let [
            f'cseq
                (fn [#_"node" this #_"int" cnt]
                    (let [
                        #_"index" x (or (:index this) (VNode'n-index (- shift 5), cnt))
                        #_"int" n (aget x 32)
                    ]
                        (take n (map list (:array this) (map - x (cons 0 x))))
                    )
                )
            #_"index" x (or (:index this) (VNode'n-index shift, cnt))
            #_"int" n (aget x 32)
        ]
            (mapcat f'cseq (take n (:array this)) (take n (map - x (cons 0 x))))
        )
    )

    (defn #_"[node node int]" VNode'rebalance [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
        (when (some? node2) => [node1 nil delta]
            (let [
                #_"int" n1 (VNode''tree-count node1, shift) #_"int" n2 (VNode''tree-count node2, shift) #_"int" n (+ n1 n2)
            ]
                (when (< VNode'max-extra-search-steps (- (+ (VNode''leaf-count node1, shift) (VNode''leaf-count node2, shift)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
                    (let [
                        f'cnode
                            (fn [#_"seq" s]
                                (loop [#_"array" a (anew 32) #_"index" x (anew 33) #_"int" j 0 #_"int" k 0 s s]
                                    (when-first [[#_"node" c #_"int" r] s] => (VNode'new nil, a, (aset! x 32 j))
                                        (recur (aset! a j c) (aset! x j (+ k r)) (inc j) (+ k r) (next s))
                                    )
                                )
                            )
                        #_"seq" s (partition-all 32 (concat (VNode''child-seq node1, shift, cnt1) (VNode''child-seq node2, shift, cnt2)))
                    ]
                        (if (<= n (* 32 32))
                            (loop [#_"array" a (anew 32) #_"index" x (-> (anew 33) (aset! 32 0)) #_"int" i 0 s s]
                                (when-first [#_"seq" block s] => [(VNode'new nil, a, x) nil cnt2]
                                    (let [
                                        #_"node" c (f'cnode block)
                                        a (aset! a i c)
                                        x (aset! x i (+ (VNode'last-range (:index c)) (if (pos? i) (aget x (dec i)) 0)))
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
                                    (when-first [#_"seq" block s] => [(VNode'new nil, a1, x1) (VNode'new nil, a2, x2) delta]
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
                                                        x1 (aset! x1 m (+ (VNode'last-range y) (if (pos? m) (aget x1 (dec m)) 0)))
                                                        x1 (aset! x1 32 (inc m))
                                                    ]
                                                        [a1 x1 a2 x2]
                                                    )
                                                    (let [
                                                        #_"int" m (rem i 32)
                                                        a2 (aset! a2 m c)
                                                        x2 (aset! x2 m (+ (VNode'last-range y) (if (pos? m) (aget x2 (dec m)) 0)))
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

    (defn #_"[node node int]" VNode'zip-path [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
        (if (= shift 5)
            (VNode'rebalance-leaves node1, node2, delta)
            (let [
                #_"node" c1 (VNode''last-child node1)
                #_"node" c2 (VNode''first-child node2)
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
                [#_"node" c1' #_"node" c2' #_"int" d'] (VNode'zip-path (- shift 5), c1, m1, c2, m2, 0)
            ]
                (VNode'rebalance shift,
                    (if (identical? c1 c1') node1 (VNode''replace-rightmost-child node1, shift, c1', d')),
                    (+ cnt1 d'),
                    (if c2' (if (identical? c2 c2') node2 (VNode''replace-leftmost-child node2, shift, cnt2, c2', d')) (VNode''remove-leftmost-child node2)),
                    (- cnt2 d'),
                    (+ delta d')
                )
            )
        )
    )

    (defn #_"[node node]" VNode'squash-nodes [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2]
        (let [
            #_"array" a1 (:array node1) #_"int" n1 (VNode'index-of-nil a1)
            #_"array" a2 (:array node2) #_"int" n2 (VNode'index-of-nil a2)
            #_"seq" slots (concat (take n1 a1) (take n2 a2))
        ]
            (when (<= (count slots) 32) => [node1 node2]
                (let [
                    #_"seq" s1 (take n1 (or (:index node1) (VNode'n-index shift, cnt1)))
                    #_"seq" s2 (take n2 (or (:index node2) (VNode'n-index shift, cnt2)))
                    #_"seq" index (concat s1 (let [#_"int" d (last s1)] (map #(+ % d) s2)))
                    #_"array" a (loop-when-recur [a (anew 32) #_"int" i 0 #_"seq" s (seq slots)] (some? s) [(aset! a i (first s)) (inc i) (next s)] => a)
                    #_"index" x (loop-when-recur [x (anew 33) #_"int" i 0 #_"seq" s (seq index)] (some? s) [(aset! x i (first s)) (inc i) (next s)] => (aset! x 32 i))
                ]
                    [(VNode'new nil, a, x) nil]
                )
            )
        )
    )
)

(about #_"TransientVector"
    (defq TransientVector [#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen] #_"VecForm")

    #_inherit
    (defm TransientVector AFn)

    (defn #_"TransientVector" TransientVector'new
        ([#_"PersistentVector" w]
            (TransientVector'new (:cnt w), (:shift w), (VNode''editable-root (:root w)), (VNode'editable-tail (:tail w)), (alength (:tail w)))
        )
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen]
            (new* TransientVector'class (anew [cnt, shift, root, tail, tlen]))
        )
    )

    (defn #_"int" TransientVector''count [#_"TransientVector" this]
        (VNode''assert-editable (:root this))
        (:cnt this)
    )

    (defn #_"int" TransientVector''tail-off [#_"TransientVector" this]
        (- (:cnt this) (:tlen this))
    )

    (defn #_"values" TransientVector''array-for [#_"TransientVector" this, #_"int" i]
        (VNode''array-for (:root this), i, (:shift this), (:cnt this), (TransientVector''tail-off this), (:tail this))
    )

    (defn #_"value" TransientVector''value-for [#_"TransientVector" this, #_"int" i]
        (VNode''value-for (:root this), i, (:shift this), (:cnt this), (TransientVector''tail-off this), (:tail this))
    )

    (defn #_"value" TransientVector''nth
        ([#_"TransientVector" this, #_"int" i]
            (VNode''assert-editable (:root this))
            (TransientVector''value-for this, i)
        )
        ([#_"TransientVector" this, #_"int" i, #_"value" not-found]
            (VNode''assert-editable (:root this))
            (when (< -1 i (:cnt this)) => not-found
                (TransientVector''value-for this, i)
            )
        )
    )

    (defn #_"value" TransientVector''valAt
        ([#_"TransientVector" this, #_"key" key] (TransientVector''valAt this, key, nil))
        ([#_"TransientVector" this, #_"key" key, #_"value" not-found]
            (VNode''assert-editable (:root this))
            (when (integer? key) => not-found
                (let-when [#_"int" i (int key)] (< -1 i (:cnt this)) => not-found
                    (TransientVector''value-for this, i)
                )
            )
        )
    )

    (defn #_"value" TransientVector''invoke [#_"TransientVector" this, #_"key" arg]
        (when (integer? arg) => (throw! "arg must be integer")
            (Indexed'''nth this, (int arg))
        )
    )

    (defn #_"value" TransientVector''applyTo [#_"TransientVector" this, #_"seq" args]
        (case! (count args 1)
            1 (IFn'''invoke this, (first args))
        )
    )

    (defn #_"TransientVector" TransientVector''conj! [#_"TransientVector" this, #_"value" val]
        (VNode''assert-editable (:root this))
        (if (< (:tlen this) 32)
            (let [
                _ (aset! (:tail this) (:tlen this) val)
            ]
                (-> this (qswap! :cnt inc) (qswap! :tlen inc))
            )
            (let [
                #_"node" tail-node (VNode'new (:edit (:root this)), (:tail this), nil)
                this (qset! this :tail (-> (anew 32) (aset! 0 val)), :tlen 1)
            ]
                (if (VNode''overflow? (:root this), (:shift this), (:cnt this))
                    (let [
                        #_"array" a
                            (-> (anew 32)
                                (aset! 0 (:root this))
                                (aset! 1 (VNode''new-path tail-node, (:edit (:root this)), (:shift this)))
                            )
                        #_"index" x
                            (when (some? (:index (:root this)))
                                (let [
                                    #_"int" n (aget (:index (:root this)) 31)
                                ]
                                    (-> (anew 33) (aset! 0 n) (aset! 1 (+ n 32)) (aset! 32 2))
                                )
                            )
                        #_"node" root (VNode'new (:edit (:root this)), a, x)
                    ]
                        (-> this (qset! :root root) (qswap! :shift + 5) (qswap! :cnt inc))
                    )
                    (let [
                        #_"node" root (VNode''push-tail (:root this), (:edit (:root this)), (:shift this), (:cnt this), tail-node)
                    ]
                        (-> this (qset! :root root) (qswap! :cnt inc))
                    )
                )
            )
        )
    )

    (defn #_"PersistentVector" TransientVector''persistent! [#_"TransientVector" this]
        (VNode''assert-editable (:root this))
        (reset! (:edit (:root this)) nil)
        (let [
            #_"int" n (:tlen this)
        ]
            (PersistentVector'new (:cnt this), (:shift this), (:root this), (-> (anew n) (acopy! 0 (:tail this) 0 n)))
        )
    )

    (defn #_"TransientVector" TransientVector''assocN! [#_"TransientVector" this, #_"int" i, #_"value" val]
        (VNode''assert-editable (:root this))
        (if (< -1 i (:cnt this))
            (let [
                #_"int" tail-off (TransientVector''tail-off this)
            ]
                (if (<= tail-off i)
                    (do
                        (aset! (:tail this) (- i tail-off) val)
                        this
                    )
                    (do
                        (qset! this :root (VNode''do-assoc (:root this), (:edit (:root this)), (:shift this), i, val))
                    )
                )
            )
            (when (= i (:cnt this)) => (throw! "index is out of bounds")
                (ITransientCollection'''conj! this, val)
            )
        )
    )

    (defn #_"TransientVector" TransientVector''pop! [#_"TransientVector" this]
        (VNode''assert-editable (:root this))
        (cond
            (zero? (:cnt this))
                (throw! "can't pop the empty vector")
            (= (:cnt this) 1)
                (let [
                    this (qset! this :cnt 0)
                    this (qset! this :tlen 0)
                    _ (aset! (:tail this) 0 nil)
                ]
                    this
                )
            (< 1 (:tlen this))
                (let [
                    this (qswap! this :cnt dec)
                    this (qswap! this :tlen dec)
                    _ (aset! (:tail this) (:tlen this) nil)
                ]
                    this
                )
            :else
                (let [
                    #_"values" tail (aclone (TransientVector''array-for this, (- (:cnt this) 2)))
                    #_"node" root (VNode''pop-tail (:root this), (:edit (:root this)), (:shift this), (TransientVector''tail-off this))
                    this
                        (cond
                            (nil? root)
                                (-> this
                                    (qset! :root (VNode'new (:edit (:root this)), nil, nil))
                                )
                            (and (< 5 (:shift this)) (nil? (aget (:array root) 1)))
                                (-> this
                                    (qswap! :shift - 5)
                                    (qset! :root (aget (:array root) 0))
                                )
                            :else
                                (-> this
                                    (qset! :root root)
                                )
                        )
                ]
                    (-> this
                        (qswap! :cnt dec)
                        (qset! :tail tail)
                        (qset! :tlen (alength tail))
                    )
                )
        )
    )

    (defn #_"TransientVector" TransientVector''assoc! [#_"TransientVector" this, #_"key" key, #_"value" val]
        (when (integer? key) => (throw! "key must be integer")
            (ITransientVector'''assocN! this, (int key), val)
        )
    )

    (defn #_"boolean" TransientVector''containsKey [#_"TransientVector" this, #_"key" key]
        (and (integer? key) (< -1 (int key) (:cnt this)))
    )

    (defn #_"pair" TransientVector''entryAt [#_"TransientVector" this, #_"key" key]
        (when (integer? key)
            (let-when [#_"int" i (int key)] (< -1 i (:cnt this))
                (MapEntry'new key, (Indexed'''nth this, i))
            )
        )
    )

    (defm TransientVector Counted
        (Counted'''count => TransientVector''count)
    )

    (defm TransientVector Indexed
        (Indexed'''nth => TransientVector''nth)
    )

    (defm TransientVector ILookup
        (ILookup'''valAt => TransientVector''valAt)
    )

    (defm TransientVector IFn
        (IFn'''invoke => TransientVector''invoke)
        (IFn'''applyTo => TransientVector''applyTo)
    )

    (defm TransientVector ITransientCollection
        (ITransientCollection'''conj! => TransientVector''conj!)
        (ITransientCollection'''persistent! => TransientVector''persistent!)
    )

    (defm TransientVector ITransientVector
        (ITransientVector'''assocN! => TransientVector''assocN!)
        (ITransientVector'''pop! => TransientVector''pop!)
    )

    (defm TransientVector ITransientAssociative
        (ITransientAssociative'''assoc! => TransientVector''assoc!)
        (ITransientAssociative'''containsKey => TransientVector''containsKey)
        (ITransientAssociative'''entryAt => TransientVector''entryAt)
    )
)

(about #_"PersistentVector"
    (defq PersistentVector [#_"meta" _meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] VecForm)

    #_inherit
    (defm PersistentVector APersistentVector AFn)

    (defn #_"PersistentVector" PersistentVector'new
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] (PersistentVector'new nil, cnt, shift, root, tail))
        ([#_"meta" meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
            (new* PersistentVector'class (anew [meta, cnt, shift, root, tail]))
        )
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

    (defn #_"PersistentVector" PersistentVector''withMeta [#_"PersistentVector" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentVector'new meta, (:cnt this), (:shift this), (:root this), (:tail this))
        )
    )

    (defn #_"boolean" PersistentVector''equals [#_"PersistentVector" this, #_"Object" that]
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
        (loop-when [#_"int" hash (int 1) #_"int" i (int 0)] (< i (:cnt this)) => (Murmur3'mixCollHash hash, i)
            (recur (+ (* (int 31) hash) (f'hash (Indexed'''nth this, i))) (inc i))
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
                    (PersistentVector'new (:_meta this), (inc (:cnt this)), (:shift this), (:root this), tail)
                )
                (let [
                    #_"node" tail-node (VNode'new (:edit (:root this)), (:tail this), nil)
                    #_"int" shift (:shift this)
                    [#_"node" root shift]
                        (if (VNode''overflow? (:root this), shift, (:cnt this))
                            (let [
                                #_"array" a
                                    (-> (anew 32)
                                        (aset! 0 (:root this))
                                        (aset! 1 (VNode''new-path tail-node, (:edit (:root this)), shift))
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
                                [(VNode'new (:edit (:root this)), a, x) (+ shift 5)]
                            )
                            [(VNode''push-tail (:root this), (:edit (:root this)), shift, (:cnt this), tail-node) shift]
                        )
                ]
                    (PersistentVector'new (:_meta this), (inc (:cnt this)), shift, root, (anew [ val ]))
                )
            )
        )
    )

    (defn #_"PersistentVector" PersistentVector''empty [#_"PersistentVector" this]
        (IObj'''withMeta PersistentVector'EMPTY, (:_meta this))
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
                        (PersistentVector'new (:_meta this), (:cnt this), (:shift this), (:root this), tail)
                    )
                    (PersistentVector'new (:_meta this), (:cnt this), (:shift this), (VNode''do-assoc (:root this), (:edit (:root this)), (:shift this), i, val), (:tail this))
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
        (case! (:cnt this)
            0   (throw! "can't pop the empty vector")
            1   (IObj'''withMeta PersistentVector'EMPTY, (:_meta this))
            (let [
                #_"int" tail-len (alength (:tail this))
            ]
                (if (< 1 tail-len)
                    (let [
                        #_"values" tail (-> (anew (dec tail-len)) (acopy! 0 (:tail this) 0 (dec tail-len)))
                    ]
                        (PersistentVector'new (:_meta this), (dec (:cnt this)), (:shift this), (:root this), tail)
                    )
                    (let [
                        #_"values" tail (PersistentVector''array-for this, (- (:cnt this) 2))
                        #_"int" shift (:shift this)
                        #_"node" root (VNode''pop-tail (:root this), (:edit (:root this)), shift, (PersistentVector''tail-off this))
                        [shift root]
                            (cond
                                (nil? root)                                     [shift VNode'EMPTY]
                                (and (< 5 shift) (nil? (aget (:array root) 1))) [(- shift 5) (aget (:array root) 0)]
                                :else                                           [shift root]
                            )
                    ]
                        (PersistentVector'new (:_meta this), (dec (:cnt this)), shift, root, tail)
                    )
                )
            )
        )
    )

    (defn #_"value" PersistentVector''invoke [#_"PersistentVector" this, #_"key" arg]
        (when (integer? arg) => (throw! "arg must be integer")
            (Indexed'''nth this, (int arg))
        )
    )

    (defn #_"value" PersistentVector''applyTo [#_"PersistentVector" this, #_"seq" args]
        (case! (count args 1)
            1 (IFn'''invoke this, (first args))
        )
    )

    (defn #_"value" PersistentVector''reduce
        ([#_"PersistentVector" this, #_"fn" f]
            (when (pos? (:cnt this)) => (f)
                (loop-when [#_"value" r (aget (PersistentVector''array-for this, 0) 0) #_"int" i 0] (< i (:cnt this)) => r
                    (let [#_"values" a (PersistentVector''array-for this, i)
                          r (loop-when [r r #_"int" j (if (zero? i) 1 0)] (< j (alength a)) => r
                                (let [r (f r (aget a j))]
                                    (when-not (reduced? r) => r
                                        (recur r (inc j))
                                    )
                                )
                            )]
                        (when-not (reduced? r) => (deref r)
                            (recur r (+ i (alength a)))
                        )
                    )
                )
            )
        )
        ([#_"PersistentVector" this, #_"fn" f, #_"value" r]
            (loop-when [r r #_"int" i 0] (< i (:cnt this)) => r
                (let [#_"values" a (PersistentVector''array-for this, i)
                      r (loop-when [r r #_"int" j 0] (< j (alength a)) => r
                            (let [r (f r (aget a j))]
                                (when-not (reduced? r) => r
                                    (recur r (inc j))
                                )
                            )
                        )]
                    (when-not (reduced? r) => (deref r)
                        (recur r (+ i (alength a)))
                    )
                )
            )
        )
    )

    (defn #_"IPersistentVector" PersistentVector''assoc [#_"PersistentVector" this, #_"key" key, #_"value" val]
        (when (integer? key) => (throw! "key must be integer")
            (IPersistentVector'''assocN this, (int key), val)
        )
    )

    (defn #_"boolean" PersistentVector''containsKey [#_"PersistentVector" this, #_"key" key]
        (and (integer? key) (< -1 (int key) (:cnt this)))
    )

    (defn #_"pair" PersistentVector''entryAt [#_"PersistentVector" this, #_"key" key]
        (when (integer? key)
            (let-when [#_"int" i (int key)] (< -1 i (:cnt this))
                (MapEntry'new key, (Indexed'''nth this, i))
            )
        )
    )

    (defn #_"value" PersistentVector''valAt
        ([#_"PersistentVector" this, #_"key" key] (PersistentVector''valAt this, key, nil))
        ([#_"PersistentVector" this, #_"key" key, #_"value" not-found]
            (when (integer? key) => not-found
                (let-when [#_"int" i (int key)] (< -1 i (:cnt this)) => not-found
                    (PersistentVector''value-for this, i)
                )
            )
        )
    )

    (defn #_"PersistentVector" PersistentVector''slicev [#_"PersistentVector" this, #_"int" start, #_"int" end]
        (cond
            (or (neg? start) (< (:cnt this) end)) (throw! "index is out of bounds")
            (= start end)                         (IPersistentCollection'''empty this)
            (< end start)                         (throw! "start index greater than end index")
            :else
                (let [
                    #_"int" new-cnt (- end start)
                    #_"int" tail-off (PersistentVector''tail-off this)
                ]
                    (if (<= tail-off start)
                        (let [
                            #_"values" tail (-> (anew new-cnt) (acopy! 0 (:tail this) (- start tail-off) new-cnt))
                        ]
                            (PersistentVector'new (:_meta this), new-cnt, 5, VNode'EMPTY, tail)
                        )
                        (let [
                            #_"boolean" tail-cut? (< tail-off end)
                            #_"node" root (:root this)
                            root (if tail-cut? root (VNode''slice-right root, (:shift this), end))
                            root (if (zero? start) root (VNode''slice-left root, (:shift this), start, (min end tail-off)))
                            #_"values" tail
                                (when tail-cut? => (VNode''array-for root, (dec new-cnt), (:shift this), new-cnt)
                                    (let [
                                        #_"int" n (- end tail-off)
                                    ]
                                        (-> (anew n) (acopy! 0 (:tail this) 0 n))
                                    )
                                )
                            root
                                (when-not tail-cut? => root
                                    (VNode''pop-tail root, nil, (:shift this), new-cnt)
                                )
                        ]
                            (when (some? root) => (PersistentVector'new (:_meta this), new-cnt, 5, VNode'EMPTY, tail)
                                (loop-when-recur [#_"node" node root #_"int" shift (:shift this)]
                                                 (and (< 5 shift) (nil? (aget (:array node) 1)))
                                                 [(aget (:array node) 0) (- shift 5)]
                                              => (PersistentVector'new (:_meta this), new-cnt, shift, node, tail)
                                )
                            )
                        )
                    )
                )
        )
    )

    (defn #_"PersistentVector" PersistentVector''splicev [#_"PersistentVector" this, #_"PersistentVector" that]
        (let [
            #_"int" c1 (:cnt this) #_"int" c2 (:cnt that)
        ]
            (cond
                (zero? c1) that
                (< c2 VNode'rrbt-concat-threshold) (into this that)
                :else
                    (let [
                        #_"node" r1 (:root this) #_"int" s1 (:shift this) #_"array" t1 (:tail this) #_"int" o1 (PersistentVector''tail-off this)
                        #_"boolean" overflow? (VNode''overflow? r1, s1, (+ o1 32))
                        r1
                            (when overflow? => (VNode''fold-tail r1, s1, o1, t1)
                                (let [
                                    #_"array" a'
                                        (-> (anew 32)
                                            (aset! 0 r1)
                                            (aset! 1 (VNode''new-path (VNode'new nil, t1, nil), nil, s1))
                                        )
                                    #_"index" x'
                                        (when (or (some? (:index r1)) (< (alength t1) 32))
                                            (-> (anew 33) (aset! 0 o1) (aset! 1 c1) (aset! 32 2))
                                        )
                                ]
                                    (VNode'new nil, a', x')
                                )
                            )
                        s1 (if overflow? (+ s1 5) s1)
                        #_"node" r2 (:root that) #_"int" s2 (:shift that) #_"array" t2 (:tail that) #_"int" o2 (PersistentVector''tail-off that)
                        #_"int" shift (max s1 s2)
                        r1 (VNode''shift-from-to r1, s1, shift)
                        r2 (VNode''shift-from-to r2, s2, shift)
                        [#_"node" n1 #_"node" n2 #_"int" delta] (VNode'zip-path shift, r1, c1, r2, o2, 0)
                        #_"int" c1' (+ c1 delta)
                        #_"int" c2' (- o2 delta)
                        [n1 n2] (if (identical? n2 r2) (VNode'squash-nodes shift, n1, c1', n2, c2') [n1 n2])
                    ]
                        (if (some? n2)
                            (let [
                                #_"array" a' (-> (anew 32) (aset! 0 n1) (aset! 1 n2))
                                #_"index" x' (-> (anew 33) (aset! 0 c1') (aset! 1 (+ c1' c2')) (aset! 32 2))
                            ]
                                (PersistentVector'new nil, (+ c1 c2), (+ shift 5), (VNode'new nil, a', x'), t2)
                            )
                            (loop-when-recur [#_"node" node n1 shift shift]
                                             (and (< 5 shift) (nil? (aget (:array node) 1)))
                                             [(aget (:array node) 0) (- shift 5)]
                                          => (PersistentVector'new nil, (+ c1 c2), shift, node, t2)
                            )
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

    (defm PersistentVector IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentVector IObj
        (IObj'''withMeta => PersistentVector''withMeta)
    )

    (defm PersistentVector IObject
        (IObject'''equals => PersistentVector''equals)
    )

    (defm PersistentVector Hashed
        (Hashed'''hash => PersistentVector''hash)
    )

    (defm PersistentVector IEditableCollection
        (IEditableCollection'''asTransient => TransientVector'new)
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
        (IPersistentVector'''slicev => PersistentVector''slicev)
        (IPersistentVector'''splicev => PersistentVector''splicev)
    )

    (defm PersistentVector IPersistentStack
        (IPersistentStack'''peek => PersistentVector''peek)
        (IPersistentStack'''pop => PersistentVector''pop)
    )

    (defm PersistentVector IFn
        (IFn'''invoke => PersistentVector''invoke)
        (IFn'''applyTo => PersistentVector''applyTo)
    )

    (defm PersistentVector IReduce
        (IReduce'''reduce => PersistentVector''reduce)
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

(defn subvec
    ([v i]   (IPersistentVector'''slicev v, i, (count v)))
    ([v i e] (IPersistentVector'''slicev v, i, e))
)

(defn catvec
    ([] (vector))
    ([a]                                                                                                  a)
    ([a b]                                                                   (IPersistentVector'''splicev a, b))
    ([a b c]                                    (IPersistentVector'''splicev (IPersistentVector'''splicev a, b),                              c))
    ([a b c d]                                  (IPersistentVector'''splicev (IPersistentVector'''splicev a, b), (IPersistentVector'''splicev c, d)))
    ([a b c d & s] (IPersistentVector'''splicev (IPersistentVector'''splicev (IPersistentVector'''splicev a, b), (IPersistentVector'''splicev c, d)), (apply catvec s)))
)
)

(about #_"arbace.arm.RT"

(about #_"RT"
    (defn #_"Object" RT'get
        ([#_"Object" coll, #_"key" key]
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key)
                (nil? coll)
                    nil
                (set? coll)
                    (IPersistentSet'''get coll, key)
                (and (number? key) (or (string? coll) (array? coll)))
                    (let-when [#_"int" n (int key)] (< -1 n (count coll))
                        (nth coll n)
                    )
                (satisfies? ITransientSet coll)
                    (ITransientSet'''get coll, key)
            )
        )
        ([#_"Object" coll, #_"key" key, #_"value" not-found]
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key, not-found)
                (nil? coll)
                    not-found
                (set? coll)
                    (if (contains? coll key) (IPersistentSet'''get coll, key) not-found)
                (and (number? key) (or (string? coll) (array? coll)))
                    (let [#_"int" n (int key)]
                        (if (< -1 n (count coll)) (nth coll n) not-found)
                    )
                (satisfies? ITransientSet coll)
                    (if (contains? coll key) (ITransientSet'''get coll, key) not-found)
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

    (defn #_"Object" RT'contains [#_"Object" coll, #_"key" key]
        (cond
            (nil? coll)
                false
            (associative? coll)
                (if (Associative'''containsKey coll, key) true false)
            (set? coll)
                (if (IPersistentSet'''contains? coll, key) true false)
            (and (number? key) (or (string? coll) (array? coll)))
                (let [#_"int" n (int key)]
                    (if (< -1 n (count coll)) true false)
                )
            (satisfies? ITransientSet coll)
                (if (ITransientSet'''contains? coll, key) true false)
            (satisfies? ITransientAssociative coll)
                (if (ITransientAssociative'''containsKey coll, key) true false)
            :else
                (throw! (str "contains? not supported on " coll))
        )
    )

(defn contains? [coll key] (RT'contains coll key))

    (defn #_"Object" RT'find [#_"Object" coll, #_"key" key]
        (cond
            (nil? coll)
                nil
            (associative? coll)
                (Associative'''entryAt coll, key)
            (satisfies? ITransientAssociative coll)
                (ITransientAssociative'''entryAt coll, key)
            :else
                (throw! (str "find not supported on " coll))
        )
    )

(defn find [m k] (RT'find m k))

    (defn #_"seq" RT'findKey [#_"Keyword" key, #_"seq" keyvals]
        (loop-when keyvals (some? keyvals)
            (when-some [#_"seq" s (next keyvals)] => (throw! "malformed keyword argslist")
                (when-not (= (first keyvals) key) => s
                    (recur (next s))
                )
            )
        )
    )

    (defn #_"Object" RT'nth
        ([#_"Object" coll, #_"int" n]
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n)
                (nil? coll)
                    nil
                (char-sequence? coll)
                    (Character'valueOf (CharSequence''charAt coll, n))
                (array? coll)
                    (Array'get coll, n)
                (matcher? coll)
                    (Matcher''group coll, n)
                (map-entry? coll)
                    (let [#_"pair" e coll]
                        (case! n 0 (key e) 1 (val e) (throw! "index is out of bounds"))
                    )
                (sequential? coll)
                    (loop-when [#_"int" i 0 #_"seq" s (seq coll)] (and (<= i n) (some? s)) => (throw! "index is out of bounds")
                        (recur-when (< i n) [(inc i) (next s)] => (first s))
                    )
                :else
                    (throw! (str "nth not supported on " coll))
            )
        )
        ([#_"Object" coll, #_"int" n, #_"value" not-found]
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n, not-found)
                (nil? coll)
                    not-found
                (neg? n)
                    not-found
                (char-sequence? coll)
                    (let-when [#_"CharSequence" s coll] (< n (CharSequence''length s)) => not-found
                        (Character'valueOf (CharSequence''charAt s, n))
                    )
                (array? coll)
                    (when (< n (Array'getLength coll)) => not-found
                        (Array'get coll, n)
                    )
                (matcher? coll)
                    (let-when [#_"Matcher" m coll] (< n (Matcher''groupCount m)) => not-found
                        (Matcher''group m, n)
                    )
                (map-entry? coll)
                    (let [#_"pair" e coll]
                        (case! n 0 (key e) 1 (val e) not-found)
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

    (defn #_"IPersistentMap" RT'map [#_"Seqable" init]
        (cond
            (empty? init)
                PersistentArrayMap'EMPTY
            (<= (count init) PersistentArrayMap'HASHTABLE_THRESHOLD)
                (PersistentArrayMap'createWithCheck (anew init))
            :else
                (PersistentHashMap'createWithCheck-1s init)
        )
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
            (-> a (Appendable''append "#'") (append (:name ns)) (Appendable''append "/") (append sym))
            (-> a (Appendable''append "#_var nil #_\"") (append sym) (Appendable''append "\""))
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
        (-> a (Appendable''append "#_unbound ") (Var'append (:ns this), (:sym this)))
    )

    (defm Unbound IObject
        (IObject'''equals => identical?)
    )

    (defm Unbound IAppend
        (IAppend'''append => Unbound''append)
    )
)

(about #_"Var"
    (defq Var [#_"Namespace" ns, #_"Symbol" sym, #_"Object'" root])

    (defn #_"Var" Var'new
        ([#_"Namespace" ns, #_"Symbol" sym] (Var'new ns, sym, (Unbound'new ns, sym)))
        ([#_"Namespace" ns, #_"Symbol" sym, #_"Object" root]
            (new* Var'class (anew [ns, sym, (atom root)]))
        )
    )

    (defn #_"meta" Var''meta [#_"Var" this]
        (meta (:root this))
    )

    (defn #_"meta" Var''alterMeta [#_"Var" this, #_"fn" f, #_"seq" args]
        (apply alter-meta! (:root this) f args)
    )

    (defn #_"meta" Var''resetMeta [#_"Var" this, #_"meta" m]
        (reset-meta! (:root this) m)
    )

    (defn #_"Appendable" Var''append [#_"Var" this, #_"Appendable" a]
        (Var'append a, (:ns this), (:sym this))
    )

    (defn #_"boolean" Var''hasRoot [#_"Var" this]
        (not (satisfies? Unbound (deref (:root this))))
    )

    (defn #_"boolean" Var''isBound [#_"Var" this]
        (Var''hasRoot this)
    )

    (defn #_"Object" Var''get [#_"Var" this]
        (deref (:root this))
    )

(defn var-get [#_"var" x] (Var''get x))

    (defn #_"void" Var''setMacro [#_"Var" this]
        (alter-meta! this assoc :macro true)
        nil
    )

    (defn #_"boolean" Var''isMacro [#_"Var" this]
        (boolean (:macro (meta this)))
    )

    (defn #_"boolean" Var''isPublic [#_"Var" this]
        (not (:private (meta this)))
    )

    (defn #_"void" Var''bindRoot [#_"Var" this, #_"Object" root]
        (alter-meta! this dissoc :macro)
        (reset! (:root this) root)
        nil
    )

    (defn #_"Object" Var''alterRoot [#_"Var" this, #_"fn" f, #_"seq" args]
        (apply swap! (:root this) f args)
    )

    (defn #_"Var" Var'intern
        ([#_"Namespace" ns, #_"Symbol" sym]
            (Namespace''intern ns, sym)
        )
        ([#_"Namespace" ns, #_"Symbol" sym, #_"Object" root]
            (let [#_"Var" v (Namespace''intern ns, sym)]
                (Var''bindRoot v, root)
                v
            )
        )
    )

(defn intern
    ([ns name]
        (let [v (Var'intern (the-ns ns), name)]
            (when-some [m (meta name)]
                (reset-meta! v m)
            )
            v
        )
    )
    ([ns name root]
        (let [v (Var'intern (the-ns ns), name, root)]
            (when-some [m (meta name)]
                (reset-meta! v m)
            )
            v
        )
    )
)

    (defn #_"Object" Var''invoke
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

    (defn #_"Object" Var''applyTo [#_"Var" this, #_"seq" args]
        (IFn'''applyTo (deref this), args)
    )

    (defm Var IMeta
        (IMeta'''meta => Var''meta)
    )

    (defm Var IReference
        (IReference'''alterMeta => Var''alterMeta)
        (IReference'''resetMeta => Var''resetMeta)
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

(defn alter-var-root [#_"var" v f & args] (Var''alterRoot v f args))

(defn bound? [& vars] (every? #(Var''isBound #_"var" %) vars))

(defmacro defonce [name expr]
    `(let-when [v# (def ~name)] (not (Var''hasRoot v#))
        (def ~name ~expr)
    )
)
)

(about #_"arbace.arm.Namespace"

(about #_"Namespace"
    (defq Namespace [#_"Symbol" name, #_"{Symbol Var}'" mappings, #_"{Symbol Namespace}'" aliases])

    (def #_"{Symbol Namespace}'" Namespace'namespaces (atom (hash-map)))

    (defn #_"seq" Namespace'all []
        (vals (deref Namespace'namespaces))
    )

(defn all-ns [] (Namespace'all))

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
        (new* Namespace'class (anew [name, (atom (hash-map)), (atom (hash-map))]))
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

    (defn #_"Namespace" Namespace'remove [#_"Symbol" name]
        (when-not (= name 'arbace.arm.core) => (throw! "cannot remove core namespace")
            (get (first (swap-vals! Namespace'namespaces dissoc name)) name)
        )
    )

(defn remove-ns [sym] (Namespace'remove sym))

    (defn #_"Appendable" Namespace''append [#_"Namespace" this, #_"Appendable" a]
        (Appendable''append a, (:name (:name this)))
    )

(defn ns-name [ns] (:name (the-ns ns)))

    (defn #_"map" Namespace''getMappings [#_"Namespace" this]
        (deref (:mappings this))
    )

(defn ns-map [ns] (Namespace''getMappings (the-ns ns)))

    (defn #_"Object" Namespace''getMapping [#_"Namespace" this, #_"Symbol" name]
        (get (deref (:mappings this)) name)
    )

(defn filter-key [f f? m]
    (loop-when-recur [s (seq m) m (transient (hash-map))]
                     s
                     [(next s) (let [e (first s)] (if (f? (f e)) (assoc m (key e) (val e)) m))]
                  => (persistent! m)
    )
)

(defn ns-interns [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"var" v] (and (var? v) (= ns (:ns v)))) (ns-map ns))
    )
)

(defn ns-publics [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"var" v] (and (var? v) (= ns (:ns v)) (Var''isPublic v))) (ns-map ns))
    )
)

(defn ns-refers [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"var" v] (and (var? v) (not= ns (:ns v)))) (ns-map ns))
    )
)

    (defn #_"void" Namespace''warnOrFailOnReplace [#_"Namespace" this, #_"Symbol" sym, #_"Object" o, #_"var" var]
        (or
            (when (var? o)
                (when (= (:ns o) this) => (throw! (str sym " already refers to: " o " in namespace: " (:name this)))
                    :ok
                )
            )
            (PrintWriter''println -/*err*, (str "WARNING: " sym " already refers to: " o " in namespace: " (:name this) ", being replaced by: " var))
        )
        nil
    )

    (defn #_"var" Namespace''intern [#_"Namespace" this, #_"Symbol" sym]
        (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
            (let [#_"Object" o
                    (or (get (deref (:mappings this)) sym)
                        (let [#_"var" v (Var'new this, sym)]
                            (swap! (:mappings this) assoc sym v)
                            v
                        )
                    )]
                (when-not (and (var? o) (= (:ns o) this)) => o
                    (let [#_"var" v (Var'new this, sym)]
                        (Namespace''warnOrFailOnReplace this, sym, o, v)
                        (swap! (:mappings this) assoc sym v)
                        v
                    )
                )
            )
        )
    )

    (defn #_"var" Namespace''refer [#_"Namespace" this, #_"Symbol" sym, #_"var" var]
        (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
            (let [#_"Object" o
                    (or (get (deref (:mappings this)) sym)
                        (do
                            (swap! (:mappings this) assoc sym var)
                            var
                        )
                    )]
                (when-not (= o var)
                    (Namespace''warnOrFailOnReplace this, sym, o, var)
                    (swap! (:mappings this) assoc sym var)
                )
                var
            )
        )
    )

(defn refer [ns-sym & filters]
    (let [ns (the-ns ns-sym) ps* (ns-publics ns) fs* (apply hash-map filters)
          r (:refer fs*) s (if (= r :all) (keys ps*) (or r (:only fs*) (keys ps*)))]
        (when (sequential? s) => (throw! "the value of :only/:refer must be a sequential collection of symbols")
            (let [es* (set (:exclude fs*)) rs* (or (:rename fs*) (hash-map))]
                (doseq [x (remove es* s)]
                    (when-some [v (ps* x)] => (throw! (str x (if (get (ns-interns ns) x) " is not public" " does not exist")))
                        (Namespace''refer *ns* (or (rs* x) x) v)
                    )
                )
            )
        )
    )
)

    (defn #_"void" Namespace''unmap [#_"Namespace" this, #_"Symbol" sym]
        (when (nil? (:ns sym)) => (throw! "can't unintern namespace-qualified symbol")
            (swap! (:mappings this) dissoc sym)
        )
        nil
    )

(defn ns-unmap [ns sym] (Namespace''unmap (the-ns ns) sym))

    (defn #_"var" Namespace''findInternedVar [#_"Namespace" this, #_"Symbol" name]
        (let [#_"Object" o (get (deref (:mappings this)) name)]
            (when (and (var? o) (= (:ns o) this))
                o
            )
        )
    )

(defn #_"Var" find-var [#_"Symbol" sym]
    (when (some? (:ns sym)) => (throw! "symbol must be namespace-qualified")
        (let [#_"Namespace" ns (Namespace'find (Symbol'intern (:ns sym)))]
            (when (some? ns) => (throw! (str "no such namespace: " (:ns sym)))
                (Namespace''findInternedVar ns, (Symbol'intern (:name sym)))
            )
        )
    )
)

    (defn #_"map" Namespace''getAliases [#_"Namespace" this]
        (deref (:aliases this))
    )

(defn ns-aliases [ns]
    (Namespace''getAliases (the-ns ns))
)

    (defn #_"Namespace" Namespace''getAlias [#_"Namespace" this, #_"Symbol" alias]
        (get (deref (:aliases this)) alias)
    )

    (defn #_"void" Namespace''addAlias [#_"Namespace" this, #_"Symbol" alias, #_"Namespace" ns]
        (when (and (some? alias) (some? ns)) => (throw! "expecting Symbol + Namespace")
            (let [#_"Object" o
                    (or (get (deref (:aliases this)) alias)
                        (do
                            (swap! (:aliases this) assoc alias ns)
                            ns
                        )
                    )]
                (when-not (= o ns)
                    (throw! (str "alias " alias " already exists in namespace " (:name this) ", aliasing " o))
                )
            )
        )
        nil
    )

(defn alias [sym ns]
    (Namespace''addAlias *ns* sym (the-ns ns))
)

    (defn #_"void" Namespace''removeAlias [#_"Namespace" this, #_"Symbol" alias]
        (swap! (:aliases this) dissoc alias)
        nil
    )

(defn ns-unalias [ns sym]
    (Namespace''removeAlias (the-ns ns) sym)
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
                        (case! (first s)
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
                            (fn [m e] (reduce #(assoc %1 %2 ((val e) %2)) (dissoc m (key e)) ((key e) m)))
                            (dissoc x :as :or)
                            (reduce
                                (fn [m k]
                                    (when (keyword? k) => m
                                        (let [ns (namespace k)]
                                            (case! (name k)
                                                "keys" (assoc m k #(keyword (or ns (namespace %)) (name %)))
                                                "syms" (assoc m k #(list 'quote (symbol (or ns (namespace %)) (name %))))
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
                              local (if (satisfies? INamed x) (with-meta (symbol nil (name x)) (meta x)) x)
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
                (reduce #(destructure- %1 (first %2) (second %2)) (vector) pairs)
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
        (loop-when [s (seq pars) pars (with-meta (vector) (meta pars)) lets (vector)] s => `(~pars (let ~lets ~@body))
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
        (with-meta (if name (list* 'fn* name s) (cons 'fn* s)) (meta &form))
    )
)

#_oops!
(defmacro loop [bindings & body]
    (if (= (destructure bindings) bindings)
        `(loop* ~bindings ~@body)
        (let [s (take-nth 2 bindings) s' (map #(if (symbol? %) % (gensym)) s)
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

(about #_"def{n,macro}"
    (defn assert-valid-fdecl [fdecl]
        (when (seq fdecl) => (throw! "parameter declaration missing")
            (let [argdecls
                    (map
                        #(if (seq? %)
                            (first %)
                            (throw!
                                (if (seq? (first fdecl))
                                    (str "invalid signature \"" % "\" should be a list")
                                    (str "parameter declaration \"" % "\" should be a vector")
                                )
                            )
                        )
                        fdecl
                    )
                bad-args (seq (remove #(vector? %) argdecls))]
                (when bad-args
                    (throw! (str "parameter declaration \"" (first bad-args) "\" should be a vector"))
                )
            )
        )
    )

    #_oops!
    (defmacro defn [fname & s]
        (when (symbol? fname) => (throw! "first argument to defn must be a symbol")
            (let [m (if (map?    (first s)) (first s) (hash-map))
                  s (if (map?    (first s)) (next s)   s)
                  s (if (vector? (first s)) (list s)   s)
                  _ (assert-valid-fdecl s)
                  m (let [inline (:inline m) ifn (first inline) iname (second inline)]
                        (when (and (= 'fn ifn) (not (symbol? iname))) => m
                            (assoc m :inline (cons ifn (cons (symbol (str (:name fname) "__inliner")) (next inline))))
                        )
                    )
                  m (conj (or (meta fname) (hash-map)) m)]
                (list 'def (with-meta fname m) (cons `fn s))
            )
        )
    )

    #_oops!
    (defmacro defmacro [name & args]
        (let [[m s] (split-with map? args) s (if (vector? (first s)) (list s) s)
              s (map (fn [[bindings & body]] (cons (apply vector '&form '&env bindings) body)) s)]
            `(do (defn ~name ~@m ~@s) (Var''setMacro (var ~name)) (var ~name))
        )
    )
)

(defn comparator [f?]
    (fn [x y]
        (cond (f? x y) -1 (f? y x) 1 :else 0)
    )
)

(defn sort
    ([s] (sort compare s))
    ([#_"Comparator" cmp s]
        (when (seq s) => (list)
            (let [a (anew s)]
                (Arrays'sort a, cmp)
                (seq a)
            )
        )
    )
)

(defn sort-by
    ([f s] (sort-by f compare s))
    ([f #_"Comparator" cmp s] (sort #(Comparator''compare cmp, (f %1), (f %2)) s))
)

#_oops!
(defmacro for [bindings body]
    (letfn [(group- [bindings]
                (reduce
                    (fn [v [x y]]
                        (if (keyword? x)
                            (conj (pop v) (conj (peek v) [x y]))
                            (conj v [x y])
                        )
                    )
                    (vector) (partition 2 bindings)
                )
            )
            (emit- [[[x _ & z] & [[_ e] :as more]]]
                (let [f' (gensym "f__") s' (gensym "s__")]
                    (letfn [(mod- [[[k v] & z]]
                                (if (keyword? k)
                                    (case! k
                                        :let   `(let ~v ~(mod- z))
                                        :while `(when ~v ~(mod- z))
                                        :when  `(if ~v ~(mod- z) (recur (next ~s')))
                                    )
                                    (when more => `(cons ~body (~f' (next ~s')))
                                        `(let [f# ~(emit- more) s# (seq (f# ~e))]
                                            (if s#
                                                (concat s# (~f' (next ~s')))
                                                (recur (next ~s'))
                                            )
                                        )
                                    )
                                )
                            )]
                        (if more
                            #_"not the inner-most loop"
                            `(fn ~f' [~s']
                                (lazy-seq
                                    (loop [~s' ~s']
                                        (when-first [~x ~s']
                                            ~(mod- z)
                                        )
                                    )
                                )
                            )
                            #_"inner-most loop"
                            `(fn ~f' [~s']
                                (lazy-seq
                                    (loop [~s' ~s']
                                        (when-some [~s' (seq ~s')]
                                            (let [~x (first ~s')]
                                                ~(mod- z)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )]
        `(~(emit- (group- bindings)) ~(second bindings))
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

(defn seq-reduce
    ([s f] (if-some [s (seq s)] (seq-reduce (next s) f (first s)) (f)))
    ([s f r]
        (loop-when [r r s (seq s)] s => r
            (let [r (f r (first s))]
                (if (reduced? r) (deref r) (recur r (next s)))
            )
        )
    )
)

(defn reduce
    ([f s]
        (if (satisfies? IReduce s)
            (IReduce'''reduce s, f)
            (seq-reduce s f)
        )
    )
    ([f r s]
        (if (satisfies? IReduce s)
            (IReduce'''reduce s, f, r)
            (seq-reduce s f r)
        )
    )
)

(defn into
    ([] (vector))
    ([to] to)
    ([to from]
        (if (editable? to)
            (with-meta (reduce! conj! to from) (meta to))
            (reduce conj to from)
        )
    )
)

(defmacro cond-> [e & s]
    (let [e' (gensym)
          s (map (fn [[? x]] `(if ~? (-> ~e' ~x) ~e')) (partition 2 s))]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

(defmacro cond->> [e & s]
    (let [e' (gensym)
          s (map (fn [[? x]] `(if ~? (->> ~e' ~x) ~e')) (partition 2 s))]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

(defmacro as-> [e e' & s]
    `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
        ~(if (seq s) (last s) e')
    )
)

(defmacro some-> [e & s]
    (let [e' (gensym)
          s (map (fn [x] `(when (some? ~e') (-> ~e' ~x))) s)]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

(defmacro some->> [e & s]
    (let [e' (gensym)
          s (map (fn [x] `(when (some? ~e') (->> ~e' ~x))) s)]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

(defn halt-when
    ([f?] (halt-when f? nil))
    ([f? h]
        (fn [g]
            (fn
                ([] (g))
                ([s]
                    (when (and (map? s) (contains? s ::halt)) => (g s)
                        (::halt s)
                    )
                )
                ([s x]
                    (when (f? x) => (g s x)
                        (reduced {::halt (if (some? h) (h (g s) x) x)})
                    )
                )
            )
        )
    )
)

(defn run! [proc coll]
    (reduce #(proc %2) nil coll)
    nil
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
    (defp TheVarExpr)
    (defp BodyExpr)
    (defp MetaExpr)
    (defp IfExpr)
    (defp MapExpr)
    (defp SetExpr)
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
    (defp MonitorExpr)
    (defp CatchClause)
    (defp TryExpr)
    (defp ThrowExpr)
)

(about #_"arbace.arm.Machine"

(about #_"Machine"
    (defn #_"Object" Machine'compute [#_"code" code, #_"array" vars]
        (loop [#_"stack" s nil #_"int" i 0]
            (let [[x y] (nth code i)]
                (case! x
                    :and               (let [[  b a & s] s]                             (recur (cons (bit-and a b) s)      (inc i)))
                    :anew              (let [[    a & s] s]                             (recur (cons (anew a) s)           (inc i)))
                    :apply             (let [[  b a & s] s]                             (recur (cons (apply a b) s)        (inc i)))
                    :aset              (let [[c b a & s] s] (aset! a b c)               (recur s                           (inc i)))
                    :create            (let [[    a & s] s]                             (recur (cons (Closure'new y, a) s) (inc i)))
                    :dup               (let [[    a]     s]                             (recur (cons a s)                  (inc i)))
                    :get               (let [[    a & s] s]                             (recur (cons (get (deref (:_env a)) y) s) (inc i)))
                    :goto                                                               (recur s                        (deref y))
                    :if-eq?            (let [[  b a & s] s]                             (recur s        (if     (= a b) (deref y) (inc i))))
                    :if-ne?            (let [[  b a & s] s]                             (recur s        (if-not (= a b) (deref y) (inc i))))
                    :if-nil?           (let [[    a & s] s]                             (recur s        (if  (nil? a)   (deref y) (inc i))))
                    :if-not            (let [[    a & s] s]                             (recur s        (if-not    a    (deref y) (inc i))))
                    :invoke-1          (let [[    a & s] s]                             (recur (cons (y a) s)              (inc i)))
                    :invoke-2          (let [[  b a & s] s]                             (recur (cons (y a b) s)            (inc i)))
                    :load                                                               (recur (cons (aget vars y) s)      (inc i))
                    :monitor-enter     (let [[    a & s] s] (monitor-enter a)           (recur s                           (inc i)))
                    :monitor-exit      (let [[    a & s] s] (monitor-exit a)            (recur s                           (inc i)))
                    :number?           (let [[    a & s] s]                             (recur (cons (number? a) s)        (inc i)))
                    :pop                                                                (recur (next s)                    (inc i))
                    :push                                                               (recur (cons y s)                  (inc i))
                    :put               (let [[  b a & s] s] (swap! (:_env a) assoc y b) (recur s                           (inc i)))
                    :return                                 (first s)
                    :shr               (let [[  b a & s] s]                             (recur (cons (>> a b) s)           (inc i)))
                    :store             (let [[    a & s] s] (aset! vars y a)            (recur s                           (inc i)))
                    :swap              (let [[  b a & s] s]                             (recur (list* a b s)               (inc i)))
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

    (defn #_"gen" Gen''and           [#_"gen" gen]                          (conj gen [:and]))
    (defn #_"gen" Gen''anew          [#_"gen" gen]                          (conj gen [:anew]))
    (defn #_"gen" Gen''apply         [#_"gen" gen]                          (conj gen [:apply]))
    (defn #_"gen" Gen''aset          [#_"gen" gen]                          (conj gen [:aset]))
    (defn #_"gen" Gen''create        [#_"gen" gen, #_"FnExpr" fun]          (conj gen [:create fun]))
    (defn #_"gen" Gen''dup           [#_"gen" gen]                          (conj gen [:dup]))
    (defn #_"gen" Gen''get           [#_"gen" gen, #_"Symbol" name]         (conj gen [:get name]))
    (defn #_"gen" Gen''goto          [#_"gen" gen, #_"label" label]         (conj gen [:goto label]))
    (defn #_"gen" Gen''if-eq?        [#_"gen" gen, #_"label" label]         (conj gen [:if-eq? label]))
    (defn #_"gen" Gen''if-ne?        [#_"gen" gen, #_"label" label]         (conj gen [:if-ne? label]))
    (defn #_"gen" Gen''if-nil?       [#_"gen" gen, #_"label" label]         (conj gen [:if-nil? label]))
    (defn #_"gen" Gen''if-not        [#_"gen" gen, #_"label" label]         (conj gen [:if-not label]))
    (defn #_"gen" Gen''invoke        [#_"gen" gen, #_"fn" f, #_"int" arity] (conj gen [(-/keyword (str "invoke" \- arity)) f]))
    (defn #_"gen" Gen''load          [#_"gen" gen, #_"int" index]           (conj gen [:load index]))
    (defn #_"gen" Gen''monitor-enter [#_"gen" gen]                          (conj gen [:monitor-enter]))
    (defn #_"gen" Gen''monitor-exit  [#_"gen" gen]                          (conj gen [:monitor-exit]))
    (defn #_"gen" Gen''number?       [#_"gen" gen]                          (conj gen [:number?]))
    (defn #_"gen" Gen''pop           [#_"gen" gen]                          (conj gen [:pop]))
    (defn #_"gen" Gen''push          [#_"gen" gen, #_"value" value]         (conj gen [:push value]))
    (defn #_"gen" Gen''put           [#_"gen" gen, #_"Symbol" name]         (conj gen [:put name]))
    (defn #_"gen" Gen''return        [#_"gen" gen]                          (conj gen [:return]))
    (defn #_"gen" Gen''shr           [#_"gen" gen]                          (conj gen [:shr]))
    (defn #_"gen" Gen''store         [#_"gen" gen, #_"int" index]           (conj gen [:store index]))
    (defn #_"gen" Gen''swap          [#_"gen" gen]                          (conj gen [:swap]))
    (defn #_"gen" Gen''throw         [#_"gen" gen]                          (conj gen [:throw]))

    (defn #_"gen" Gen''lookup-switch [#_"gen" gen, #_"ints" values, #_"labels" labels, #_"label" default]
        (conj gen [:lookup-switch (vec values) (mapv deref labels) (deref default)])
    )

    (defn #_"gen" Gen''table-switch [#_"gen" gen, #_"int" low, #_"int" high, #_"labels" labels, #_"label" default]
        (conj gen [:table-switch low high (mapv deref labels) (deref default)])
    )

    (defn #_"gen" Gen''try-catch-finally [#_"gen" gen, #_"label" start, #_"label" end, #_"label" finally]
        (conj gen [:try-catch-finally (deref start) (deref end) (deref finally)])
    )
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
            (let [#_"Symbol" nsSym (symbol (:ns sym))]
                (or (Namespace''getAlias inns, nsSym) (find-ns nsSym))
            )
        )
    )

    (defn #_"Symbol" Compiler'resolveSymbol [#_"Symbol" sym]
        (cond
            (pos? (String''indexOf (:name sym), (int \.)))
                sym
            (some? (:ns sym))
                (let [#_"Namespace" ns (Compiler'namespaceFor sym)]
                    (if (and (some? ns) (not (and (some? (:name (:name ns))) (= (:name (:name ns)) (:ns sym)))))
                        (symbol (:name (:name ns)) (:name sym))
                        sym
                    )
                )
            :else
                (let [#_"Object" o (Namespace''getMapping *ns*, sym)]
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
                (let [#_"Object" o (Namespace''getMapping *ns*, sym)]
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

    (defn #_"Var" Compiler'maybeMacro [#_"Object" op, #_"map" scope]
        (when-not (and (symbol? op) (some? (get (deref (get scope :'local-env)) op)))
            (when (or (symbol? op) (var? op))
                (let [#_"Var" v (if (var? op) op (Compiler'lookupVar op, false))]
                    (when (and (some? v) (get (meta v) :macro))
                        (when (or (= (:ns v) *ns*) (not (get (meta v) :private))) => (throw! (str "var: " v " is private"))
                            v
                        )
                    )
                )
            )
        )
    )

    (defn #_"IFn" Compiler'maybeInline [#_"Object" op, #_"int" arity, #_"map" scope]
        (when-not (and (symbol? op) (some? (get (deref (get scope :'local-env)) op)))
            (when (or (symbol? op) (var? op))
                (when-some [#_"Var" v (if (var? op) op (Compiler'lookupVar op, false))]
                    (when (or (= (:ns v) *ns*) (not (get (meta v) :private))) => (throw! (str "var: " v " is private"))
                        (when-some [#_"IFn" f (get (meta v) :inline)]
                            (let [#_"IFn" arityPred (get (meta v) :inline-arities)]
                                (when (or (nil? arityPred) (IFn'''invoke arityPred, arity))
                                    f
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"Object" Compiler'resolveIn [#_"Namespace" n, #_"Symbol" sym, #_"boolean" allowPrivate]
        (cond
            (some? (:ns sym))
                (when-some [#_"Namespace" ns (Compiler'namespaceFor n, sym)]                     => (throw! (str "no such namespace: " (:ns sym)))
                    (when-some [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))] => (throw! (str "no such var: " sym))
                        (when (or (= (:ns v) *ns*) (not (get (meta v) :private)) allowPrivate)   => (throw! (str "var: " sym " is private"))
                            v
                        )
                    )
                )
            :else
                (or (Namespace''getMapping n, sym) (throw! (str "unable to resolve symbol: " sym " in this context")))
        )
    )

    (defn #_"Object" Compiler'resolve
        ([#_"Symbol" sym                          ] (Compiler'resolveIn *ns*, sym, false       ))
        ([#_"Symbol" sym, #_"boolean" allowPrivate] (Compiler'resolveIn *ns*, sym, allowPrivate))
    )

    (defn #_"Object" Compiler'maybeResolveIn [#_"Namespace" n, #_"Symbol" sym]
        (cond
            (some? (:ns sym))
                (when-some [#_"Namespace" ns (Compiler'namespaceFor n, sym)]
                    (when-some [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))]
                        v
                    )
                )
            :else
                (Namespace''getMapping n, sym)
        )
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
            gen (Gen''push gen, (<< (count locals) 1))
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

    (defn #_"LiteralExpr" LiteralExpr'new [#_"Object" value]
        (new* LiteralExpr'class
            (-/hash-map
                #_"Object" :value value
            )
        )
    )

    (def #_"LiteralExpr" LiteralExpr'NIL   (LiteralExpr'new nil))
    (def #_"LiteralExpr" LiteralExpr'TRUE  (LiteralExpr'new true))
    (def #_"LiteralExpr" LiteralExpr'FALSE (LiteralExpr'new false))

    (defn #_"Expr" LiteralExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"int" n (dec (count form))]
            (when (= n 1) => (throw! (str "wrong number of arguments passed to quote: " n))
                (let [#_"Object" value (second form)]
                    (case! value
                        nil                 LiteralExpr'NIL
                        true                LiteralExpr'TRUE
                        false               LiteralExpr'FALSE
                        (cond
                            (string? value) (LiteralExpr'new (String''intern value))
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
            (-/hash-map
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
            (-/hash-map
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

(about #_"TheVarExpr"
    (defr TheVarExpr)

    (defn #_"TheVarExpr" TheVarExpr'new [#_"Var" var]
        (new* TheVarExpr'class
            (-/hash-map
                #_"Var" :var var
            )
        )
    )

    (defn #_"Expr" TheVarExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"Symbol" sym (second form) #_"Var" v (Compiler'lookupVar sym, false)]
            (when (some? v) => (throw! (str "unable to resolve var: " sym " in this context"))
                (TheVarExpr'new v)
            )
        )
    )

    (defn #_"gen" TheVarExpr''emit [#_"TheVarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (Gen''push gen, (:var this))
        )
    )

    (defm TheVarExpr Expr
        (Expr'''emit => TheVarExpr''emit)
    )
)

(about #_"BodyExpr"
    (defr BodyExpr)

    (defn #_"BodyExpr" BodyExpr'new [#_"vector" exprs]
        (new* BodyExpr'class
            (-/hash-map
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

(about #_"MetaExpr"
    (defr MetaExpr)

    (defn #_"MetaExpr" MetaExpr'new [#_"Expr" expr, #_"Expr" meta]
        (new* MetaExpr'class
            (-/hash-map
                #_"Expr" :expr expr
                #_"Expr" :meta meta
            )
        )
    )

    (defn #_"gen" MetaExpr''emit [#_"MetaExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Expr'''emit (:meta this), :Context'EXPRESSION, scope, gen)
            gen (Gen''invoke gen, with-meta, 2)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm MetaExpr Expr
        (Expr'''emit => MetaExpr''emit)
    )
)

(about #_"IfExpr"
    (defr IfExpr)

    (defn #_"IfExpr" IfExpr'new [#_"Expr" test, #_"Expr" then, #_"Expr" else]
        (new* IfExpr'class
            (-/hash-map
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

(about #_"MapExpr"
    (defr MapExpr)

    (defn #_"MapExpr" MapExpr'new [#_"vector" args]
        (new* MapExpr'class
            (-/hash-map
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" MapExpr'parse [#_"map" form, #_"map" scope]
        (let [[#_"vector" args #_"boolean" literal?]
                (loop-when [args (vector), literal? true, #_"set" keys (hash-set), #_"seq" s (seq form)] (some? s) => [args literal?]
                    (let [#_"pair" e (first s) #_"Expr" k (Compiler'analyze (key e), scope) #_"Expr" v (Compiler'analyze (val e), scope)
                          [literal? keys]
                            (when (satisfies? LiteralExpr k) => [false keys]
                                (when-not (contains? keys (:value k)) => (throw! "duplicate constant keys in map")
                                    [literal? (conj keys (:value k))]
                                )
                            )]
                        (recur (conj args k v) (and literal? (satisfies? LiteralExpr v)) keys (next s))
                    )
                )
              #_"Expr" e
                (when literal? => (MapExpr'new args)
                    (LiteralExpr'new (apply hash-map (map :value args)))
                )]
            (when-some [#_"meta" m (meta form)] => e
                (MetaExpr'new e, (MapExpr'parse m, scope))
            )
        )
    )

    (defn #_"gen" MapExpr''emit [#_"MapExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            #_"int" n (count (:args this))
            [#_"boolean" literal? #_"boolean" unique?]
                (loop-when [literal? true, unique? true, #_"set" keys (hash-set), #_"int" i 0] (< i n) => [literal? unique?]
                    (let [#_"Expr" k (nth (:args this) i)
                          [literal? unique? keys]
                            (when (satisfies? LiteralExpr k) => [false unique? keys]
                                (when-not (contains? keys (:value k)) => [literal? false keys]
                                    [literal? unique? (conj keys (:value k))]
                                )
                            )]
                        (recur literal? unique? keys (+ i 2))
                    )
                )
            gen (Compiler'emitArgs scope, gen, (:args this))
            gen (Gen''invoke gen, (if (or (and literal? unique?) (<= n 2)) RT'mapUniqueKeys RT'map), 1)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm MapExpr Expr
        (Expr'''emit => MapExpr''emit)
    )
)

(about #_"SetExpr"
    (defr SetExpr)

    (defn #_"SetExpr" SetExpr'new [#_"vector" args]
        (new* SetExpr'class
            (-/hash-map
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" SetExpr'parse [#_"set" form, #_"map" scope]
        (let [[#_"vector" args #_"boolean" literal?]
                (loop-when [args (vector) literal? true #_"seq" s (seq form)] (some? s) => [args literal?]
                    (let [#_"Expr" e (Compiler'analyze (first s), scope)]
                        (recur (conj args e) (and literal? (satisfies? LiteralExpr e)) (next s))
                    )
                )
              #_"Expr" e
                (when literal? => (SetExpr'new args)
                    (LiteralExpr'new (apply hash-set (map :value args)))
                )]
            (when-some [#_"meta" m (meta form)] => e
                (MetaExpr'new e, (MapExpr'parse m, scope))
            )
        )
    )

    (defn #_"gen" SetExpr''emit [#_"SetExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (when (seq (:args this)) => (Gen''push gen, PersistentHashSet'EMPTY)
                    (let [gen (Compiler'emitArgs scope, gen, (:args this))]
                        (Gen''invoke gen, PersistentHashSet'createWithCheck, 1)
                    )
                )
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm SetExpr Expr
        (Expr'''emit => SetExpr''emit)
    )
)

(about #_"VectorExpr"
    (defr VectorExpr)

    (defn #_"VectorExpr" VectorExpr'new [#_"vector" args]
        (new* VectorExpr'class
            (-/hash-map
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
              #_"Expr" e
                (when literal? => (VectorExpr'new args)
                    (LiteralExpr'new (mapv :value args))
                )]
            (when-some [#_"meta" m (meta form)] => e
                (MetaExpr'new e, (MapExpr'parse m, scope))
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
            (-/hash-map
                #_"Expr" :fexpr fexpr
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" InvokeExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"Expr" fexpr (Compiler'analyze (first form), scope)
              #_"vector" args (mapv #(Compiler'analyze %, scope) (next form))]
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
            (-/hash-map
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
            (-/hash-map
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
            (-/hash-map
                #_"FnExpr" :fun fun
                #_"FnMethod" :parent parent
                #_"{int LocalBinding}'" :'locals (atom (hash-map))
                #_"Integer" :arity nil
                #_"Expr" :body nil
            )
        )
    )

    (defn #_"FnMethod" FnMethod'parse [#_"FnExpr" fun, #_"seq" form, #_"map" scope]
        (let [
            scope
                (-> scope
                    (update :fm (partial FnMethod'new fun))
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
            (-/hash-map
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
            fun
                (let [
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
        ]
            (MetaExpr'new fun, (MapExpr'parse (meta form), scope)) fun
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

    (defn #_"DefExpr" DefExpr'new [#_"Var" var, #_"Expr" init, #_"Expr" meta, #_"boolean" initProvided]
        (new* DefExpr'class
            (-/hash-map
                #_"Var" :var var
                #_"Expr" :init init
                #_"Expr" :meta meta
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
                                (DefExpr'new v, (Compiler'analyze (third form), scope), (Compiler'analyze (meta s), scope), (= n 3))
                            )
                        )
                    )
            )
        )
    )

    (defn #_"boolean" DefExpr''includesExplicitMetadata [#_"DefExpr" this, #_"MapExpr" expr]
        (loop-when [#_"int" i 0] (< i (count (:keyvals expr))) => false
            (recur-when (= (:k (nth (:keyvals expr) i)) :declared) [(+ i 2)] => true)
        )
    )

    (defn #_"gen" DefExpr''emit [#_"DefExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Gen''push gen, (:var this))
            gen
                (when (some? (:meta this)) => gen
                    (let [
                        gen (Gen''dup gen)
                        gen (Expr'''emit (:meta this), :Context'EXPRESSION, scope, gen)
                        gen (Gen''invoke gen, Var''resetMeta, 2)
                    ]
                        (Gen''pop gen)
                    )
                )
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
            (-/hash-map
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
            (-/hash-map
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
            (-/hash-map
                #_"vector" :loopLocals loopLocals
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" RecurExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (when (and (= context :Context'RETURN) (some? (get scope :loop-locals))) => (throw! "can only recur from tail position")
            (let [#_"vector" args (mapv #(Compiler'analyze %, scope) (next form)) #_"int" n (count args) #_"int" m (count (get scope :loop-locals))]
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

(about #_"MonitorExpr"
    (defr MonitorExpr)

    (defn #_"MonitorExpr" MonitorExpr'new [#_"Expr" target, #_"boolean" enter?]
        (new* MonitorExpr'class
            (-/hash-map
                #_"Expr" :target target
                #_"boolean" :enter? enter?
            )
        )
    )

    (defn #_"Expr" MonitorExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (MonitorExpr'new (Compiler'analyze (second form), scope), (= (first form) 'monitor-enter))
    )

    (defn #_"gen" MonitorExpr''emit [#_"MonitorExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:target this), :Context'EXPRESSION, scope, gen)
            gen (if (:enter? this) (Gen''monitor-enter gen) (Gen''monitor-exit gen))
            gen (Expr'''emit LiteralExpr'NIL, context, scope, gen)
        ]
            gen
        )
    )

    (defm MonitorExpr Expr
        (Expr'''emit => MonitorExpr''emit)
    )
)

(about #_"CatchClause"
    (defr CatchClause)

    (defn #_"CatchClause" CatchClause'new [#_"LocalBinding" lb, #_"Expr" handler]
        (new* CatchClause'class
            (-/hash-map
                #_"LocalBinding" :lb lb
                #_"Expr" :handler handler
            )
        )
    )
)

(about #_"TryExpr"
    (defr TryExpr)

    (defn #_"TryExpr" TryExpr'new [#_"Expr" tryExpr, #_"[CatchClause]" catches, #_"Expr" finallyExpr, #_"map" scope]
        (new* TryExpr'class
            (-/hash-map
                #_"Expr" :tryExpr tryExpr
                #_"[CatchClause]" :catches catches
                #_"Expr" :finallyExpr finallyExpr

                #_"int" :retLocal (swap! (get scope :'local-num) inc)
                #_"int" :finallyLocal (swap! (get scope :'local-num) inc)
            )
        )
    )

    (defn #_"Expr" TryExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [
            scope (dissoc scope :loop-locals)
            [#_"Expr" bodyExpr #_"[CatchClause]" catches #_"Expr" finallyExpr #_"vector" body]
                (loop-when [bodyExpr nil catches (vector) finallyExpr nil body (vector) #_"boolean" caught? false #_"seq" fs (next form)] (some? fs) => [bodyExpr catches finallyExpr body]
                    (let [#_"Object" f (first fs) #_"Object" op (when (seq? f) (first f))]
                        (if (any = op 'catch 'finally)
                            (let [bodyExpr (or bodyExpr (BodyExpr'parse (seq body), context, scope))]
                                (if (= op 'catch)
                                    (let [#_"symbol?" sym (third f)]
                                        (when (symbol? sym)        => (throw! (str "bad binding form, expected symbol, got: " sym))
                                            (when (nil? (:ns sym)) => (throw! (str "can't bind qualified name: " sym))
                                                (let [
                                                    scope (update scope :'local-env (comp atom deref))
                                                    scope (update scope :'local-num (comp atom deref))
                                                    #_"LocalBinding" lb (LocalBinding'new sym, nil, (swap! (get scope :'local-num) inc))
                                                    _ (swap! (get scope :'local-env) assoc (:sym lb) lb)
                                                    _ (swap! (:'locals (get scope :fm)) assoc (:uid lb) lb)
                                                    #_"Expr" handler (BodyExpr'parse (next (next (next f))), :Context'EXPRESSION, scope)
                                                    #_"CatchClause" clause (CatchClause'new lb, handler)
                                                ]
                                                    (recur bodyExpr (conj catches clause) finallyExpr body true (next fs))
                                                )
                                            )
                                        )
                                    )
                                    (when (nil? (next fs)) => (throw! "finally clause must be last in try expression")
                                        (let [finallyExpr (BodyExpr'parse (next f), :Context'STATEMENT, scope)]
                                            (recur bodyExpr catches finallyExpr body caught? (next fs))
                                        )
                                    )
                                )
                            )
                            (when-not caught? => (throw! "only catch or finally clause can follow catch in try expression")
                                (recur bodyExpr catches finallyExpr (conj body f) caught? (next fs))
                            )
                        )
                    )
                )
        ]
            (when (nil? bodyExpr) => (TryExpr'new bodyExpr, catches, finallyExpr, scope)
                (BodyExpr'parse (seq body), context, scope)
            )
        )
    )

    (defn #_"gen" TryExpr''emit [#_"TryExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            #_"label" l'start (Gen''mark gen)
            gen (Expr'''emit (:tryExpr this), context, scope, gen)
            gen
                (when-not (= context :Context'STATEMENT) => gen
                    (Gen''store gen, (:retLocal this))
                )
            #_"label" l'end (Gen''mark gen)
            gen
                (when (some? (:finallyExpr this)) => gen
                    (Expr'''emit (:finallyExpr this), :Context'STATEMENT, scope, gen)
                )
            #_"label" l'return (Gen''label gen)
            gen (Gen''goto gen, l'return)
            #_"int" n (count (:catches this)) #_"labels" l'starts (mapv Gen''label (repeat n gen)) #_"labels" l'ends (mapv Gen''label (repeat n gen))
            gen
                (loop-when [gen gen #_"int" i 0] (< i n) => gen
                    (let [
                        #_"CatchClause" clause (nth (:catches this) i)
                        gen (Gen''mark gen, (nth l'starts i))
                        gen (Gen''store gen, (:idx (:lb clause)))
                        gen (Expr'''emit (:handler clause), context, scope, gen)
                        gen
                            (when-not (= context :Context'STATEMENT) => gen
                                (Gen''store gen, (:retLocal this))
                            )
                        gen (Gen''mark gen, (nth l'ends i))
                        gen
                            (when (some? (:finallyExpr this)) => gen
                                (Expr'''emit (:finallyExpr this), :Context'STATEMENT, scope, gen)
                            )
                        gen (Gen''goto gen, l'return)
                    ]
                        (recur gen (inc i))
                    )
                )
            #_"label" l'finally (Gen''label gen)
            gen
                (when (some? (:finallyExpr this)) => gen
                    (let [
                        gen (Gen''mark gen, l'finally)
                        gen (Gen''store gen, (:finallyLocal this))
                        gen (Expr'''emit (:finallyExpr this), :Context'STATEMENT, scope, gen)
                        gen (Gen''load gen, (:finallyLocal this))
                        gen (Gen''throw gen)
                    ]
                        gen
                    )
                )
            gen (Gen''mark gen, l'return)
            gen
                (when-not (= context :Context'STATEMENT) => gen
                    (Gen''load gen, (:retLocal this))
                )
            gen (loop-when-recur [gen gen #_"int" i 0] (< i n) [(Gen''try-catch-finally gen, l'start, l'end, (nth l'starts i)) (inc i)] => gen)
        ]
            (when (some? (:finallyExpr this)) => gen
                (let [
                    gen (Gen''try-catch-finally gen, l'start, l'end, l'finally)
                ]
                    (loop-when-recur [gen gen #_"int" i 0] (< i n) [(Gen''try-catch-finally gen, (nth l'starts i), (nth l'ends i), l'finally) (inc i)] => gen)
                )
            )
        )
    )

    (defm TryExpr Expr
        (Expr'''emit => TryExpr''emit)
    )
)

(about #_"ThrowExpr"
    (defr ThrowExpr)

    (defn #_"ThrowExpr" ThrowExpr'new [#_"Expr" throwable]
        (new* ThrowExpr'class
            (-/hash-map
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
            '&             nil
            'catch         nil
            'def           DefExpr'parse
            'do            BodyExpr'parse
            'finally       nil
            'fn*           FnExpr'parse
            'if            IfExpr'parse
            'let*          LetExpr'parse
            'letfn*        LetFnExpr'parse
            'loop*         LetExpr'parse
            'monitor-enter MonitorExpr'parse
            'monitor-exit  MonitorExpr'parse
            'quote         LiteralExpr'parse
            'recur         RecurExpr'parse
            'throw         ThrowExpr'parse
            'try           TryExpr'parse
            'var           TheVarExpr'parse
        )
    )

    (defn #_"boolean" Compiler'isSpecial [#_"Object" sym]
        (contains? Compiler'specials sym)
    )

(defn special-symbol? [s] (Compiler'isSpecial s))

    (defn #_"edn" Compiler'macroexpand1
        ([#_"edn" form] (Compiler'macroexpand1 form, nil))
        ([#_"edn" form, #_"map" scope]
            (when (seq? form) => form
                (let-when [#_"Object" op (first form)] (not (Compiler'isSpecial op)) => form
                    (let-when [#_"Var" v (Compiler'maybeMacro op, scope)] (some? v) => form
                        (apply v form (deref (get scope :'local-env)) (next form))
                    )
                )
            )
        )
    )

    (defn #_"edn" Compiler'macroexpand [#_"edn" form, #_"map" scope]
        (let-when [#_"edn" f (Compiler'macroexpand1 form, scope)] (identical? f form) => (recur f, scope)
            form
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
            (when (nil? (:ns sym))
                (when-some [#_"LocalBinding" lb (get (deref (get scope :'local-env)) sym)]
                    (Compiler'closeOver lb, (get scope :fm))
                    (LocalBindingExpr'new lb)
                )
            )
            (let [#_"Object" o (Compiler'resolve sym)]
                (cond
                    (var? o)
                        (when (nil? (Compiler'maybeMacro o, scope)) => (throw! (str "can't take value of a macro: " o))
                            (VarExpr'new o)
                        )
                    (symbol? o)
                        (UnresolvedVarExpr'new o)
                    :else
                        (throw! (str "unable to resolve symbol: " sym " in this context"))
                )
            )
        )
    )

    (defn #_"Expr" Compiler'analyzeSeq [#_"seq" form, #_"Context" context, #_"map" scope]
        (let-when [#_"Object" me (Compiler'macroexpand1 form, scope)] (= me form) => (Compiler'analyze me, context, scope)
            (when-some [#_"Object" op (first form)] => (throw! (str "can't call nil, form: " form))
                (let [#_"IFn" inline (Compiler'maybeInline op, (count (next form)), scope)]
                    (if (some? inline)
                        (Compiler'analyze (IFn'''applyTo inline, (next form)), context, scope)
                        (let [#_"fn" f'parse (or (get Compiler'specials op) InvokeExpr'parse)]
                            (f'parse form, context, scope)
                        )
                    )
                )
            )
        )
    )

    (defn #_"Expr" Compiler'analyze
        ([#_"edn" form, #_"map" scope] (Compiler'analyze form, :Context'EXPRESSION, scope))
        ([#_"edn" form, #_"Context" context, #_"map" scope]
            (let [form
                    (when (satisfies? LazySeq form) => form
                        (with-meta (or (seq form) (list)) (meta form))
                    )]
                (case! form
                    nil                                  LiteralExpr'NIL
                    true                                 LiteralExpr'TRUE
                    false                                LiteralExpr'FALSE
                    (cond
                        (symbol? form)                   (Compiler'analyzeSymbol form, scope)
                        (string? form)                   (LiteralExpr'new (String''intern form))
                        (and (coll? form) (empty? form)) (LiteralExpr'new form)
                        (seq? form)                      (Compiler'analyzeSeq form, context, scope)
                        (vector? form)                   (VectorExpr'parse form, scope)
                        (map? form)                      (MapExpr'parse form, scope)
                        (set? form)                      (SetExpr'parse form, scope)
                        :else                            (LiteralExpr'new form)
                    )
                )
            )
        )
    )

    (defn #_"edn" Compiler'eval
        ([#_"edn" form] (Compiler'eval form, nil))
        ([#_"edn" form, #_"map" scope]
            (let [form (Compiler'macroexpand form, scope)]
                (-> (list 'fn* [] form)
                    (Compiler'analyze scope)
                    (Closure'new nil)
                    (IFn'''invoke)
                )
            )
        )
    )
)
)

(about #_"arbace.arm.LispReader"

(about #_"LispReader"
    (defn #_"Symbol" LispReader'garg [#_"int" n]
        (symbol (str (if (= n -1) "args" (str "arg" n)) "__" (next-id!) "#"))
    )

    (defn #_"Symbol" LispReader'registerArg [#_"map" scope, #_"int" n]
        (when (contains? scope :'arg-env) => (throw! "arg literal not in #()")
            (or (get (deref (get scope :'arg-env)) n)
                (let [#_"Symbol" sym (LispReader'garg n)]
                    (swap! (get scope :'arg-env) assoc n sym)
                    sym
                )
            )
        )
    )

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
        (and (LispReader'isMacro ch) (not (any = ch \# \' \%)))
    )

    (defn #_"boolean" LispReader'isDigit [#_"char" ch, #_"int" base]
        (not= (Character'digit ch, base) -1)
    )

    (defn #_"boolean" LispReader'isWhitespace [#_"char" ch]
        (or (Character'isWhitespace ch) (= ch \,))
    )

    (defn #_"Character" LispReader'read1 [#_"Reader" r]
        (let [#_"int" c (Reader''read r)]
            (when-not (= c -1)
                (char c)
            )
        )
    )

    (defn #_"void" LispReader'unread [#_"PushbackReader" r, #_"Character" ch]
        (when (some? ch)
            (PushbackReader''unread r, (int ch))
        )
        nil
    )

    (defn #_"void" LispReader'consumeWhitespaces [#_"PushbackReader" r]
        (loop-when-recur [#_"char" ch (LispReader'read1 r)] (and (some? ch) (LispReader'isWhitespace ch)) [(LispReader'read1 r)] => (LispReader'unread r, ch))
        nil
    )

    (def #_"Pattern" LispReader'rxInteger #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)")
    (def #_"Pattern" LispReader'rxRatio   #"([-+]?[0-9]+)/([0-9]+)")

    (defn #_"Object" LispReader'matchNumber [#_"String" s]
        (let [_ (or
                    (let-when [#_"Matcher" m (Pattern''matcher LispReader'rxInteger, s)] (Matcher''matches m)
                        (when (nil? (Matcher''group m, 2)) => (Long'valueOf 0)
                            (let [[#_"String" n #_"int" radix]
                                    (cond-some
                                        [n (Matcher''group m, 3)] [n 10]
                                        [n (Matcher''group m, 4)] [n 16]
                                        [n (Matcher''group m, 5)] [n 8]
                                        [n (Matcher''group m, 7)] [n (Integer'parseInt (Matcher''group m, 6))]
                                    )]
                                (when (some? n) => :nil
                                    (let [#_"BigInteger" bn (BigInteger'new n, radix) bn (if (= (Matcher''group m, 1) "-") (BigInteger''negate bn) bn)]
                                        (when (< (BigInteger''bitLength bn) 64) => bn
                                            (Long'valueOf (BigInteger''longValue bn))
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (let-when [#_"Matcher" m (Pattern''matcher LispReader'rxRatio, s)] (Matcher''matches m)
                        (let [#_"String" n (Matcher''group m, 1) n (if (String''startsWith n, "+") (String''substring n, 1) n)]
                            (Numbers'divide (BigInteger'new n), (BigInteger'new (Matcher''group m, 2)))
                        )
                    )
                )]
            (when-not (= _ :nil) _)
        )
    )

    (defn #_"Object" LispReader'readNumber [#_"PushbackReader" r, #_"char" ch]
        (let [#_"String" s
                (let [#_"StringBuilder" sb (StringBuilder'new) _ (StringBuilder''append sb, ch)]
                    (loop []
                        (let [ch (LispReader'read1 r)]
                            (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isMacro ch))
                                (do
                                    (LispReader'unread r, ch)
                                    (StringBuilder''toString sb)
                                )
                                (do
                                    (StringBuilder''append sb, ch)
                                    (recur)
                                )
                            )
                        )
                    )
                )]
            (or (LispReader'matchNumber s) (throw! (str "invalid number: " s)))
        )
    )

    (defn #_"String" LispReader'readToken [#_"PushbackReader" r, #_"char" ch]
        (let [#_"StringBuilder" sb (StringBuilder'new) _ (StringBuilder''append sb, ch)]
            (loop []
                (let [ch (LispReader'read1 r)]
                    (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isTerminatingMacro ch))
                        (do
                            (LispReader'unread r, ch)
                            (StringBuilder''toString sb)
                        )
                        (do
                            (StringBuilder''append sb, ch)
                            (recur)
                        )
                    )
                )
            )
        )
    )

    (def #_"Pattern" LispReader'rxSymbol #"[:]?([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)")

    (defn #_"Object" LispReader'matchSymbol [#_"String" s]
        (let-when [#_"Matcher" m (Pattern''matcher LispReader'rxSymbol, s)] (Matcher''matches m)
            (let [#_"String" ns (Matcher''group m, 1) #_"String" n (Matcher''group m, 2)]
                (cond
                    (or (and (some? ns) (String''endsWith ns, ":/")) (String''endsWith n, ":") (not= (String''indexOf s, "::", 1) -1))
                        nil
                    (String''startsWith s, "::")
                        (let [#_"Symbol" ks (symbol (String''substring s, 2))
                              #_"Namespace" kns (if (some? (:ns ks)) (Namespace''getAlias *ns*, (symbol (:ns ks))) *ns*)]
                            (when (some? kns)
                                (keyword (:name (:name kns)) (:name ks))
                            )
                        )
                    :else
                        (let [#_"boolean" kw? (= (String''charAt s, 0) \:) #_"Symbol" sym (symbol (String''substring s, (if kw? 1 0)))]
                            (if kw? (keyword sym) sym)
                        )
                )
            )
        )
    )

    (defn #_"Object" LispReader'interpretToken [#_"String" s]
        (case! s "nil" nil "true" true "false" false
            (or (LispReader'matchSymbol s) (throw! (str "invalid token: " s)))
        )
    )

    (defn #_"Object" LispReader'read
        ([#_"PushbackReader" r, #_"map" scope] (LispReader'read r, scope, true, nil))
        ([#_"PushbackReader" r, #_"map" scope, #_"boolean" eofIsError, #_"Object" eofValue] (LispReader'read r, scope, eofIsError, eofValue, nil, nil))
        ([#_"PushbackReader" r, #_"map" scope, #_"boolean" eofIsError, #_"Object" eofValue, #_"Character" returnOn, #_"Object" returnOnValue]
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
                                    (let [#_"Object" o (f'macro r scope ch)]
                                        (recur-when (identical? o r) [] => o)
                                    )
                                    (or
                                        (when (any = ch \+ \-)
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

    (defn #_"int" LispReader'scanDigits [#_"String" token, #_"int" offset, #_"int" n, #_"int" base]
        (when (= (+ offset n) (String''length token)) => (throw! (str "invalid unicode character: \\" token))
            (loop-when [#_"int" c 0 #_"int" i 0] (< i n) => c
                (let [#_"char" ch (String''charAt token, (+ offset i)) #_"int" d (Character'digit ch, base)]
                    (when-not (= d -1) => (throw! (str "invalid digit: " ch))
                        (recur (+ (* c base) d) (inc i))
                    )
                )
            )
        )
    )

    (defn #_"int" LispReader'readDigits [#_"PushbackReader" r, #_"char" ch, #_"int" base, #_"int" n, #_"boolean" exact?]
        (let-when-not [#_"int" c (Character'digit ch, base)] (= c -1) => (throw! (str "invalid digit: " ch))
            (let [[c #_"int" i]
                    (loop-when [c c i 1] (< i n) => [c i]
                        (let [ch (LispReader'read1 r)]
                            (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isMacro ch))
                                (do
                                    (LispReader'unread r, ch)
                                    [c i]
                                )
                                (let [#_"int" d (Character'digit ch, base)]
                                    (when-not (= d -1) => (throw! (str "invalid digit: " ch))
                                        (recur (+ (* c base) d) (inc i))
                                    )
                                )
                            )
                        )
                    )]
                (when (or (= i n) (not exact?)) => (throw! (str "invalid character length: " i ", should be: " n))
                    c
                )
            )
        )
    )

    (def #_"any" LispReader'READ_EOF (anew 0))
    (def #_"any" LispReader'READ_FINISHED (anew 0))

    (defn #_"vector" LispReader'readDelimitedForms [#_"PushbackReader" r, #_"map" scope, #_"char" delim]
        (loop [#_"vector" v (vector)]
            (let [#_"Object" form (LispReader'read r, scope, false, LispReader'READ_EOF, delim, LispReader'READ_FINISHED)]
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

(about #_"RegexReader"
    (defn #_"Pattern" regex-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (let [#_"StringBuilder" sb (StringBuilder'new)]
            (loop []
                (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading regex")
                    (when-not (= ch \")
                        (StringBuilder''append sb, ch)
                        (when (= ch \\)
                            (when-some [ch (LispReader'read1 r)] => (throw! "EOF while reading regex")
                                (StringBuilder''append sb, ch)
                            )
                        )
                        (recur)
                    )
                )
            )
            (Pattern'compile (StringBuilder''toString sb))
        )
    )
)

(about #_"StringReader"
    (defn #_"char" StringReader'escape [#_"PushbackReader" r]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading string")
            (case! ch
                \t  \tab
                \r  \return
                \n  \newline
                \\  ch
                \"  ch
                \b  \backspace
                \f  \formfeed
                \u  (let [ch (LispReader'read1 r)]
                        (when (and (some? ch) (LispReader'isDigit ch, 16)) => (throw! (str "invalid unicode escape: \\u" ch))
                            (char (LispReader'readDigits r, ch, 16, 4, true))
                        )
                    )
                (when (LispReader'isDigit ch, #_8 4) => (throw! (str "unsupported escape character: \\" ch))
                    (let [#_"int" c (LispReader'readDigits r, ch, 8, 3, false)]
                      #_(when (< 0377 c)
                            (throw! "octal escape sequence must be in range [0, 377]")
                        )
                        (char c)
                    )
                )
            )
        )
    )

    (defn #_"Object" string-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (let [#_"StringBuilder" sb (StringBuilder'new)]
            (loop []
                (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading string")
                    (when-not (= ch \")
                        (StringBuilder''append sb, (if (= ch \\) (StringReader'escape r) ch))
                        (recur)
                    )
                )
            )
            (StringBuilder''toString sb)
        )
    )
)

(about #_"CommentReader"
    (defn #_"Object" comment-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (while (not (any = (LispReader'read1 r) nil \newline \return)))
        r
    )
)

(about #_"DiscardReader"
    (defn #_"Object" discard-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (LispReader'read r, scope)
        r
    )
)

(about #_"QuoteReader"
    (defn #_"Object" quote-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (list 'quote (LispReader'read r, scope))
    )
)

(about #_"DerefReader"
    (defn #_"Object" deref-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (list `deref (LispReader'read r, scope))
    )
)

(about #_"VarReader"
    (defn #_"Object" var-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (list 'var (LispReader'read r, scope))
    )
)

(about #_"DispatchReader"
    (defn #_"Object" dispatch-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (let-when [#_"fn" f'macro (get LispReader'dispatchMacros ch)] (nil? f'macro) => (f'macro r scope ch)
                (LispReader'unread r, ch)
                (throw! (str "no dispatch macro for: " ch))
            )
        )
    )
)

(about #_"FnReader"
    (defn #_"Object" fn-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (when-not (contains? scope :'arg-env) => (throw! "nested #()s are not allowed")
            (let [scope (assoc scope :'arg-env (atom (sorted-map)))]
                (LispReader'unread r, \()
                (let [
                    #_"Object" form (LispReader'read r, scope)
                    #_"vector" args (vector)
                    args
                        (when-some [#_"seq" rs (rseq (deref (get scope :'arg-env)))] => args
                            (let [args
                                    (let-when [#_"int" n (key (first rs))] (pos? n) => args
                                        (loop-when-recur [args args #_"int" i 1]
                                                         (<= i n)
                                                         [(conj args (or (get (deref (get scope :'arg-env)) i) (LispReader'garg i))) (inc i)]
                                                      => args
                                        )
                                    )]
                                (when-some [#_"Object" rest (get (deref (get scope :'arg-env)) -1)] => args
                                    (conj args '& rest)
                                )
                            )
                        )
                ]
                    (list 'fn* args form)
                )
            )
        )
    )
)

(about #_"ArgReader"
    (defn #_"Object" arg-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (when (contains? scope :'arg-env) => (LispReader'interpretToken (LispReader'readToken r, \%))
            (let [#_"char" ch (LispReader'read1 r) _ (LispReader'unread r, ch)]
                (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isTerminatingMacro ch))
                    (LispReader'registerArg scope, 1)
                    (let [#_"Object" n (LispReader'read r, scope)]
                        (cond
                            (= n '&)    (LispReader'registerArg scope, -1)
                            (number? n) (LispReader'registerArg scope, (int n))
                            :else       (throw! "arg literal must be %, %& or %integer")
                        )
                    )
                )
            )
        )
    )
)

(about #_"MetaReader"
    (defn #_"Object" meta-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (let [#_"Object" _meta (LispReader'read r, scope)
              _meta
                (cond
                    (keyword? _meta) {_meta true}
                    (map? _meta)      _meta
                    :else (throw! "metadata must be Keyword or Map")
                )
              #_"Object" o (LispReader'read r, scope)]
            (when (satisfies? IMeta o) => (throw! "metadata can only be applied to IMetas")
                (if (satisfies? IReference o)
                    (do
                        (reset-meta! o _meta)
                        o
                    )
                    (let [#_"meta" m
                            (loop-when [m (meta o) #_"seq" s (seq _meta)] (some? s) => m
                                (let [#_"pair" e (first s)]
                                    (recur (assoc m (key e) (val e)) (next s))
                                )
                            )]
                        (with-meta o m)
                    )
                )
            )
        )
    )
)

(about #_"SyntaxQuoteReader"
(def unquote)

    (defn #_"boolean" SyntaxQuoteReader'isUnquote [#_"Object" form]
        (and (seq? form) (= (first form) `unquote))
    )

(def unquote-splicing)

    (defn #_"boolean" SyntaxQuoteReader'isUnquoteSplicing [#_"Object" form]
        (and (seq? form) (= (first form) `unquote-splicing))
    )

    (defn #_"seq" SyntaxQuoteReader'sqExpandList [#_"map" scope, #_"seq" s]
        (loop-when [#_"vector" v (vector) s s] (some? s) => (seq v)
            (let [#_"Object" item (first s)
                  v (cond
                        (SyntaxQuoteReader'isUnquote item)         (conj v (list `list (second item)))
                        (SyntaxQuoteReader'isUnquoteSplicing item) (conj v (second item))
                        :else                                      (conj v (list `list (SyntaxQuoteReader'syntaxQuote scope, item)))
                    )]
                (recur v (next s))
            )
        )
    )

    (defn #_"Object" SyntaxQuoteReader'syntaxQuote [#_"map" scope, #_"Object" form]
        (let [#_"Object" q
                (cond
                    (Compiler'isSpecial form)
                        (list 'quote form)
                    (symbol? form)
                        (let [#_"String" ns (:ns form) #_"String" n (:name form)
                              form
                                (cond
                                    (and (nil? ns) (String''endsWith n, "#"))
                                        (LispReader'registerGensym scope, (symbol (String''substring n, 0, (dec (String''length n)))))
                                    (and (nil? ns) (String''endsWith n, "."))
                                        (symbol (str (:name (Compiler'resolveSymbol (symbol (String''substring n, 0, (dec (String''length n)))))) "."))
                                    (and (nil? ns) (String''startsWith n, "."))
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
                    (or (keyword? form) (number? form) (char? form) (string? form))
                        form
                    :else
                        (list 'quote form)
                )]
            (when (and (satisfies? IObj form) (seq (meta form)) (not (SyntaxQuoteReader'isUnquote form))) => q
                (list `with-meta q (SyntaxQuoteReader'syntaxQuote scope, (meta form)))
            )
        )
    )

    (defn #_"Object" syntax-quote-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (let [scope (assoc scope :'gensym-env (atom (hash-map)))]
            (SyntaxQuoteReader'syntaxQuote scope, (LispReader'read r, scope))
        )
    )
)

(about #_"UnquoteReader"
    (defn #_"Object" unquote-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (if (= ch \@)
                (list `unquote-splicing (LispReader'read r, scope))
                (do
                    (LispReader'unread r, ch)
                    (list `unquote (LispReader'read r, scope))
                )
            )
        )
    )
)

(about #_"CharacterReader"
    (defn #_"Object" character-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (let [#_"String" token (LispReader'readToken r, ch)]
                (when-not (= (String''length token) 1) => (Character'valueOf (String''charAt token, 0))
                    (case! token
                        "newline"   \newline
                        "space"     \space
                        "tab"       \tab
                        "backspace" \backspace
                        "formfeed"  \formfeed
                        "return"    \return
                        (case! (String''charAt token, 0)
                            \u  (let [#_"int" c (LispReader'scanDigits token, 1, 4, 16)]
                                    (when (<= 0xd800 c 0xdfff)
                                        (throw! (str "invalid character constant: \\u" (Integer'toString c, 16)))
                                    )
                                    (char c)
                                )
                            \o  (let [#_"int" n (dec (String''length token))]
                                    (when (< 3 n)
                                        (throw! (str "invalid octal escape sequence length: " n))
                                    )
                                    (let [#_"int" c (LispReader'scanDigits token, 1, n, 8)]
                                        (when (< 0377 c)
                                            (throw! "octal escape sequence must be in range [0, 377]")
                                        )
                                        (char c)
                                    )
                                )
                            (throw! (str "unsupported character: \\" token))
                        )
                    )
                )
            )
        )
    )
)

(about #_"ListReader"
    (defn #_"Object" list-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (apply list (LispReader'readDelimitedForms r, scope, \)))
    )
)

(about #_"VectorReader"
    (defn #_"Object" vector-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (vec (LispReader'readDelimitedForms r, scope, \]))
    )
)

(about #_"MapReader"
    (defn #_"Object" map-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (let [#_"vector" v (LispReader'readDelimitedForms r, scope, \})]
            (when (even? (count v)) => (throw! "map literal must contain an even number of forms")
                (RT'map v)
            )
        )
    )
)

(about #_"SetReader"
    (defn #_"Object" set-reader [#_"PushbackReader" r, #_"map" scope, #_"char" _delim]
        (PersistentHashSet'createWithCheck (LispReader'readDelimitedForms r, scope, \}))
    )
)

(about #_"UnmatchedDelimiterReader"
    (defn #_"Object" unmatched-delimiter-reader [#_"PushbackReader" _r, #_"map" scope, #_"char" delim]
        (throw! (str "unmatched delimiter: " delim))
    )
)

(about #_"LispReader"
    (def #_"{char fn}" LispReader'macros
        (hash-map
            \"  string-reader
            \;  comment-reader
            \'  quote-reader
            \@  deref-reader
            \^  meta-reader
            \`  syntax-quote-reader
            \~  unquote-reader
            \(  list-reader,    \)  unmatched-delimiter-reader
            \[  vector-reader,  \]  unmatched-delimiter-reader
            \{  map-reader,     \}  unmatched-delimiter-reader
            \\  character-reader
            \%  arg-reader
            \#  dispatch-reader
        )
    )

    (def #_"{char fn}" LispReader'dispatchMacros
        (hash-map
            \^  meta-reader
            \'  var-reader
            \"  regex-reader
            \(  fn-reader
            \{  set-reader
            \!  comment-reader
            \_  discard-reader
        )
    )
)
)

(defn read
    ([] (read -/*in*))
    ([s] (read s true nil))
    ([s eof-error? eof-value] (LispReader'read s, nil, (boolean eof-error?), eof-value))
)

(about #_"arbace.arm.Compiler"

(about #_"Compiler"
    (defn #_"Object" Compiler'load [#_"Reader" reader]
        (let [
            #_"PushbackReader" r (if (pushback-reader? reader) reader (PushbackReader'new reader)) #_"any" EOF (anew 0)
            #_"map" scope (hash-map :'local-env (atom (hash-map)))
        ]
            (loop [#_"value" value nil]
                (LispReader'consumeWhitespaces r)
                (let-when [#_"edn" form (LispReader'read r, nil, false, EOF)] (identical? form EOF) => (recur (Compiler'eval form, scope))
                    value
                )
            )
        )
    )
)
)

(defn macroexpand-1 [form] (Compiler'macroexpand1 form))

(defn macroexpand [form]
    (let-when [e (macroexpand-1 form)] (identical? e form) => (recur e)
        form
    )
)

(defn eval [form] (Compiler'eval form))

(defn ns-resolve
    ([ns sym] (ns-resolve ns nil sym))
    ([ns env sym]
        (when-not (contains? env sym)
            (Compiler'maybeResolveIn (the-ns ns) sym)
        )
    )
)

(defn resolve
    ([    sym] (ns-resolve *ns*     sym))
    ([env sym] (ns-resolve *ns* env sym))
)
