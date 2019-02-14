(ns arbace.core
    (:refer-clojure :only [defmacro])
)

(defmacro § [& _])
(defmacro ß [& _])

(ns arbace.bore
    (:refer-clojure :only [*ns* -> = apply case conj cons defmacro defn defn- doseq fn get identity keys keyword let letfn map mapcat merge meta partial some? str symbol symbol? vary-meta vec vector when with-meta]) (:require [clojure.core :as -])
    (:require [flatland.ordered.map :refer [ordered-map]] #_[flatland.ordered.set :refer [ordered-set]])
)

(defmacro import! [& syms-or-seqs] `(do (doseq [n# (keys (-/ns-imports *ns*))] (-/ns-unmap *ns* n#)) (-/import ~@syms-or-seqs)))

(import! [java.lang Class Error #_String System Thread] #_[java.lang.reflect Method] [clojure.lang ILookup ITransientAssociative Namespace Symbol])

(defn #_"void" Bore'import-as [#_"Symbol" sym, #_"String" sig]
    (-/doto (#_"Class" .getDeclaredMethod Namespace, "referenceClass", (-/into-array [Symbol Class]))
        (#_"Method" .setAccessible true)
        (#_"Method" .invoke *ns*, (-/object-array [sym (Class/forName sig)]))
    )
    nil
)

(defmacro import-as [& s] (cons 'do (map (fn [[sym sig]] `(Bore'import-as (quote ~sym), ~sig)) (apply ordered-map s))))

(defmacro refer! [ns s]
    (let [f #(let [v (-/ns-resolve (-/the-ns (if (= ns '-) 'clojure.core ns)) %) n (vary-meta % merge (-/select-keys (meta v) [:private :macro]))] `(def ~n ~v))]
        (if (symbol? s) (f s) (cons 'do (map f s)))
    )
)

(defmacro about [& s] (cons 'do s))

(about #_"defarray"

(defn- emit-defarray* [tname cname fields interfaces methods opts]
    (let [
        classname  (with-meta (symbol (str (-/namespace-munge *ns*) "." cname)) (meta cname))
        interfaces (vec interfaces)
        fields     (map #(with-meta % nil) fields)
    ]
        (let [a '__data s (mapcat (fn [x y] [(keyword y) x]) (-/range) fields)]
            (letfn [(ilookup [[i m]]
                        [
                            (conj i 'clojure.lang.ILookup)
                            (conj m
                                `(valAt [this# k#] (.valAt this# k# nil))
                                `(valAt [this# k# else#] (let [x# (case k# ~@s)] (-/aget ~a x#)))
                            )
                        ]
                    )
                    (imap [[i m]]
                        [
                            (conj i 'clojure.lang.ITransientAssociative)
                            (conj m
                                `(assoc [this# k# v#] (let [x# (case k# ~@s)] (-/aset ~a x# v#) this#))
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

(about #_"extend-type"

(defn- emit-hinted-impl [_ [p fs]]
    [p (-/zipmap (map #(-> % -/first -/name keyword) fs) (map #(let [% (-/next %)] (if (= '=> (-/first %)) (-/second %) (cons `fn %))) fs))]
)

(defmacro extend-type [t & specs]
    `(-/extend ~t ~@(mapcat (partial emit-hinted-impl t) (#'-/parse-impls specs)))
)
)

(defmacro defp [p & s]   (let [i (symbol (str p "'iface"))] `(do (-/defprotocol ~p ~@s) (def ~i (:on-interface ~p)) ~p)))
(defmacro defq [r f & s] (let [c (symbol (str r "'class"))] `(do (defarray ~c ~(vec f) ~r ~@s)                      ~c)))
(defmacro defr [r [& s]] (let [c (symbol (str r "'class"))] `(do (-/defrecord ~c [] ~r ~@s)                         ~c)))
(defmacro defm [r & s]   (let [i `(:on-interface ~r)]       `(do (extend-type ~i ~@s)                               ~i)))

(defmacro throw! [#_"String" s] `(throw (Error. ~s)))

(refer! - [< <= > >= neg? pos? zero?])

(def +    -/unchecked-add-int)
(def neg  -/unchecked-negate-int)
(def -    -/unchecked-subtract-int)
(def inc  -/unchecked-inc-int)
(def dec  -/unchecked-dec-int)
(def *    -/unchecked-multiply-int)
(def quot -/unchecked-divide-int)
(def rem  -/unchecked-remainder-int)

(refer! - [bit-and bit-or bit-xor])

(def &     bit-and)
(def |     bit-or)
(def <<  -/bit-shift-left)
(def >>  -/bit-shift-right)
(def >>> -/unsigned-bit-shift-right)

(refer! - [aget alength])

(defn anew [size-or-seq] (-/object-array size-or-seq))
(defn aclone [a]         (when (some? a) (-/aclone a)))
(defn acopy! [a i b j n] (System/arraycopy b, j, a, i, n) a)
(defn aset!  [a i x]     (-/aset a i x) a)
(defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

(def assoc!! -/assoc!)

(defn update!!
    ([m k f] (assoc!! m k (f (get m k))))
    ([m k f x] (assoc!! m k (f (get m k) x)))
    ([m k f x y] (assoc!! m k (f (get m k) x y)))
    ([m k f x y & z] (assoc!! m k (apply f (get m k) x y z)))
)

(defn thread [] (Thread/currentThread))

(ns arbace.core
    (:refer-clojure :only [*err* *in* *ns* *out* *print-length* *warn-on-reflection* = atom boolean case char compare cons defmethod defn fn hash-map hash-set identical? int intern let list long loop make-array map merge object-array reify reset! satisfies? swap! symbol to-array]) (:require [clojure.core :as -])
    (:require [clojure.core.rrb-vector :refer [catvec subvec vec vector]])
    (:refer arbace.bore :only [& * + - < << <= > >= >> >>> about aclone acopy! aget alength anew aset! assoc!! aswap! bit-and bit-xor dec defm defp defq defr import! import-as inc neg? pos? quot refer! rem thread throw! update!! zero? |])
)

(import!
    [java.lang ArithmeticException Boolean Byte Character CharSequence Class #_ClassCastException ClassLoader ClassNotFoundException Comparable Exception Integer Long NoSuchMethodException Number Object String StringBuilder ThreadLocal Throwable Void]
    [java.io BufferedReader PushbackReader #_Reader #_StringReader StringWriter Writer]
    [java.lang.ref #_Reference ReferenceQueue SoftReference WeakReference]
    [java.lang.reflect Array #_Constructor #_Field #_Method Modifier]
    [java.security AccessController PrivilegedAction]
    [java.util Arrays Comparator IdentityHashMap]
    [java.util.regex Matcher Pattern]
    [jdk.vm.ci.hotspot CompilerToVM HotSpotJVMCIRuntime HotSpotVMConfig]
    [cloiure.asm #_ClassVisitor ClassWriter Label #_MethodVisitor Opcodes Type]
    [cloiure.asm.commons GeneratorAdapter Method]
    [arbace.math BigInteger]
    [arbace.util.concurrent.atomic AtomicReference]
)

(import-as
    boolean'array "[Z"
       byte'array "[B"
       char'array "[C"
        int'array "[I"
       long'array "[J"
     Object'array "[Ljava.lang.Object;"
)

(let [last-id' (atom 0)] (defn next-id! [] (swap! last-id' inc)))

;;;
 ; Returns a new symbol with a unique name. If a prefix string is supplied,
 ; the name is prefix# where # is some unique number.
 ; If prefix is not supplied, the prefix is 'G__'.
 ;;
(defn gensym
    ([] (gensym "G__"))
    ([prefix] (symbol (-/str prefix (next-id!))))
)

;;;
 ; defs the supplied var names with no bindings, useful for making forward declarations.
 ;;
(defmacro declare [& names] `(do ~@(map #(list 'def (-/vary-meta % -/assoc :declared true)) names)))

(defmacro def-      [x & s] `(def      ~(-/vary-meta x -/assoc :private true) ~@s))
(defmacro defn-     [x & s] `(defn     ~(-/vary-meta x -/assoc :private true) ~@s))
(defmacro defmacro- [x & s] `(defmacro ~(-/vary-meta x -/assoc :private true) ~@s))

(defn identity   [x] x)
(defn constantly [x] (fn [& _] x))

(defn nil?   [x] (identical? x nil))
(defn false? [x] (identical? x false))
(defn true?  [x] (identical? x true))
(defn not    [x] (if x false true))
(defn some?  [x] (not (nil? x)))
(defn any?   [_] true)

;;;
 ; Evaluates test. If logical false, evaluates and returns then expr,
 ; otherwise else expr, if supplied, else nil.
 ;;
(defmacro if-not
    ([? then] (if-not ? then nil))
    ([? then else] (list 'if ? else then))
)

;;;
 ; Evaluates exprs one at a time, from left to right. If a form returns logical false
 ; (nil or false), and returns that value and doesn't evaluate any of the other expressions,
 ; otherwise it returns the value of the last expr. (and) returns true.
 ;;
(defmacro and
    ([] true)
    ([x] x)
    ([x & s] `(let [and# ~x] (if and# (and ~@s) and#)))
)

;;;
 ; Evaluates exprs one at a time, from left to right. If a form returns a logical true value,
 ; or returns that value and doesn't evaluate any of the other expressions, otherwise it returns
 ; the value of the last expression. (or) returns nil.
 ;;
(defmacro or
    ([] nil)
    ([x] x)
    ([x & s] `(let [or# ~x] (if or# or# (or ~@s))))
)

(defmacro any
    ([f x y] `(~f ~x ~y))
    ([f x y & s] `(let [f# ~f x# ~x] (or (f# x# ~y) (any f# x# ~@s))))
)

;;;
 ; fnspec => (fname [params*] exprs) or (fname ([params*] exprs)+)
 ;
 ; Takes a vector of function specs and a body, and generates a set of
 ; bindings of functions to their names. All of the names are available
 ; in all of the definitions of the functions, as well as the body.
 ;;
(defmacro letfn [fnspecs & body]
    `(letfn* ~(vec (-/interleave (map -/first fnspecs) (map #(cons `fn %) fnspecs))) ~@body)
)

(letfn [(=> [s] (if (= '=> (-/first s)) (-/next s) (cons nil s)))]
    (defmacro     when       [? & s] (let [[e & s] (=> s)]               `(if     ~? (do ~@s) ~e)))
    (defmacro     when-not   [? & s] (let [[e & s] (=> s)]               `(if-not ~? (do ~@s) ~e)))
    (defmacro let-when     [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if     ~? (do ~@s) ~e))))
    (defmacro let-when-not [v ? & s] (let [[e & s] (=> s)] `(let ~(vec v) (if-not ~? (do ~@s) ~e))))
)

;;;
 ; Takes a set of test/expr pairs. It evaluates each test one at a time.
 ; If a test returns logical true, cond evaluates and returns the value of the
 ; corresponding expr and doesn't evaluate any of the other tests or exprs.
 ; (cond) returns nil.
 ;;
(defmacro cond [& s]
    (when s
        `(if ~(-/first s)
            ~(when (-/next s) => (throw! "cond requires an even number of forms")
                (-/second s)
            )
            (cond ~@(-/next (-/next s)))
        )
    )
)

(defmacro- assert-args [& s]
    `(when ~(-/first s) ~'=> (throw! (-/str (-/first ~'&form) " requires " ~(-/second s)))
        ~(let-when [s (-/next (-/next s))] s
            `(assert-args ~@s)
        )
    )
)

(defmacro if-let
    ([bind then] `(if-let ~bind ~then nil))
    ([bind then else & _]
        (assert-args
            (-/vector? bind) "a vector for its binding"
            (= 2 (-/count bind)) "exactly 2 forms in binding vector"
            (nil? _) "1 or 2 forms after binding vector"
        )
        `(let-when [x# ~(bind 1)] x# ~'=> ~else
            (let [~(bind 0) x#]
                ~then
            )
        )
    )
)

(defmacro cond-let [bind then & else]
    (let [bind (if (-/vector? bind) bind [`_# bind])]
        `(if-let ~bind ~then ~(when else `(cond-let ~@else)))
    )
)

(defmacro if-some
    ([bind then] `(if-some ~bind ~then nil))
    ([bind then else & _]
        (assert-args
            (-/vector? bind) "a vector for its binding"
            (= 2 (-/count bind)) "exactly 2 forms in binding vector"
            (nil? _) "1 or 2 forms after binding vector"
        )
        `(let-when [x# ~(bind 1)] (some? x#) ~'=> ~else
            (let [~(bind 0) x#]
                ~then
            )
        )
    )
)

(defmacro cond-some [bind then & else]
    (let [bind (if (-/vector? bind) bind [`_# bind])]
        `(if-some ~bind ~then ~(when else `(cond-some ~@else)))
    )
)

(defmacro if-first
    ([bind then] `(if-first ~bind ~then nil))
    ([bind then else & _]
        (assert-args
            (-/vector? bind) "a vector for its binding"
            (= 2 (-/count bind)) "exactly 2 forms in binding vector"
            (nil? _) "1 or 2 forms after binding vector"
        )
        `(let-when [s# (-/seq ~(bind 1))] (some? s#) ~'=> ~else
            (let [~(bind 0) (-/first s#)]
                ~then
            )
        )
    )
)

(ß defmacro when-let [bindings & body]
    (assert-args
        (-/vector? bindings) "a vector for its binding"
        (= 2 (-/count bindings)) "exactly 2 forms in binding vector"
    )
    `(let-when [x# ~(bindings 1)] x#
        (let [~(bindings 0) x#]
            ~@body
        )
    )
)

(ß defmacro when-some [bindings & body]
    (assert-args
        (-/vector? bindings) "a vector for its binding"
        (= 2 (-/count bindings)) "exactly 2 forms in binding vector"
    )
    `(let-when [x# ~(bindings 1)] (some? x#)
        (let [~(bindings 0) x#]
            ~@body
        )
    )
)

(ß defmacro when-first [bindings & body]
    (assert-args
        (-/vector? bindings) "a vector for its binding"
        (= 2 (-/count bindings)) "exactly 2 forms in binding vector"
    )
    `(when-some [s# (-/seq ~(bindings 1))]
        (let [~(bindings 0) (-/first s#)]
            ~@body
        )
    )
)

(letfn [(=> [s] (if (= '=> (-/first s)) (-/next s) (cons nil s)))]
    (defmacro when-let   [v & s] (let [[e & s] (=> s)] `(if-let   ~(vec v) (do ~@s) ~e)))
    (defmacro when-some  [v & s] (let [[e & s] (=> s)] `(if-some  ~(vec v) (do ~@s) ~e)))
    (defmacro when-first [v & s] (let [[e & s] (=> s)] `(if-first ~(vec v) (do ~@s) ~e)))
)

;;;
 ; Takes a binary predicate, an expression, and a set of clauses.
 ; Each clause can take the form of either:
 ;
 ; test-expr result-expr
 ;
 ; test-expr :>> result-fn
 ;
 ; Note :>> is an ordinary keyword.
 ;
 ; For each clause, (f? test-expr expr) is evaluated. If it returns logical true,
 ; the clause is a match. If a binary clause matches, the result-expr is returned,
 ; if a ternary clause matches, its result-fn, which must be a unary function, is
 ; called with the result of the predicate as its argument, the result of that call
 ; being the return value of condp. A single default expression can follow the clauses,
 ; and its value will be returned if no clause matches. If no default expression
 ; is provided and no clause matches, an IllegalArgumentException is thrown.
 ;;
(defmacro condp [f? expr & clauses]
    (let [gpred (gensym "pred__") gexpr (gensym "expr__")
          emit-
            (fn emit- [f? expr args]
                (let [[[a b c :as clause] more] (-/split-at (if (= :>> (-/second args)) 3 2) args) n (-/count clause)]
                    (cond
                        (= 0 n) `(throw! (-/str "no matching clause: " ~expr))
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

(letfn [(v' [v] (cond (-/vector? v) v (-/symbol? v) [v v] :else [`_# v]))
        (r' [r] (cond (-/vector? r) `((recur ~@r)) (some? r) `((recur ~r))))
        (=> [s] (if (= '=> (-/first s)) (-/next s) (cons nil s)))
        (l' [v ? r s] (let [r (r' r) [e & s] (=> s)] `(loop ~(v' v) (if ~? (do ~@s ~@r) ~e))))]
    (defmacro loop-when [v ? & s] (l' v ? nil s))
    (defmacro loop-when-recur [v ? r & s] (l' v ? r s))
)

(letfn [(r' [r] (cond (-/vector? r) `(recur ~@r) (some? r) `(recur ~r)))
        (=> [s] (if (= '=> (-/first s)) (-/second s)))]
    (defmacro recur-when [? r & s] `(if ~? ~(r' r) ~(=> s)))
)

;;;
 ; Repeatedly executes body while test expression is true. Presumes
 ; some side-effect will cause test to become false/nil. Returns nil.
 ;;
(defmacro while [? & s]
    `(loop [] (when ~? ~@s (recur)))
)

;;;
 ; Repeatedly executes body (presumably for side-effects) with bindings and filtering as provided by "for".
 ; Does not retain the head of the sequence. Returns nil.
 ;;
(defmacro doseq [bindings & body]
    (assert-args
        (-/vector? bindings) "a vector for its binding"
        (-/even? (-/count bindings)) "an even number of forms in binding vector"
    )
    (letfn [(emit- [e r]
                (when e => [`(do ~@body) true]
                    (let [[k v & e] e]
                        (if (-/keyword? k)
                            (let [[f r?] (emit- e r)]
                                (case k
                                    :let   [`(let ~v ~f) r?]
                                    :while [`(when ~v ~f ~@(when r? [r])) false]
                                    :when  [`(if ~v (do ~f ~@(when r? [r])) ~r) false]
                                )
                            )
                            (let [s (gensym "s__") r `(recur (-/next ~s)) [f r?] (emit- e r)]
                                [`(loop-when [~s (-/seq ~v)] ~s (let [~k (-/first ~s)] ~f ~@(when r? [r]))) true]
                            )
                        )
                    )
                )
            )]
        (-/first (emit- (-/seq bindings) nil))
    )
)

;;;
 ; bindings => name n
 ;
 ; Repeatedly executes body (presumably for side-effects) with name
 ; bound to integers from 0 through n-1.
 ;;
(defmacro dotimes [bindings & body]
    (assert-args
        (-/vector? bindings) "a vector for its binding"
        (= 2 (-/count bindings)) "exactly 2 forms in binding vector"
    )
    (let [[i n] bindings]
        `(let [n# (long ~n)]
            (loop-when-recur [~i 0] (< ~i n#) [(inc ~i)]
                ~@body
            )
        )
    )
)

;;;
 ; Evaluates x, then calls all of the methods and functions with the
 ; value of x supplied at the front of the given arguments. The forms
 ; are evaluated in order. Returns x.
 ;
 ; (doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
 ;;
(defmacro doto [x & s]
    (let [x' (gensym)]
        `(let [~x' ~x]
            ~@(map (fn [f] (-/with-meta (if (-/seq? f) `(~(-/first f) ~x' ~@(-/next f)) `(~f ~x')) (-/meta f))) s)
            ~x'
        )
    )
)

;;;
 ; Threads the expr through the forms. Inserts x as the second item
 ; in the first form, making a list of it if it is not a list already.
 ; If there are more forms, inserts the first form as the second item
 ; in second form, etc.
 ;;
(defmacro -> [x & s]
    (when s => x
        (recur &form &env
            (let-when [f (-/first s)] (-/seq? f) => (list f x)
                (-/with-meta `(~(-/first f) ~x ~@(-/next f)) (-/meta f))
            )
            (-/next s)
        )
    )
)

;;;
 ; Threads the expr through the forms. Inserts x as the last item
 ; in the first form, making a list of it if it is not a list already.
 ; If there are more forms, inserts the first form as the last item
 ; in second form, etc.
 ;;
(defmacro ->> [x & s]
    (when s => x
        (recur &form &env
            (let-when [f (-/first s)] (-/seq? f) => (list f x)
                (-/with-meta `(~(-/first f) ~@(-/next f) ~x) (-/meta f))
            )
            (-/next s)
        )
    )
)

;;;
 ; Executes exprs in an implicit do, while holding the monitor of x.
 ; Will release the monitor of x in all circumstances.
 ;;
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

;;;
 ; Returns the Class of x.
 ;;
(defn #_"Class" class [#_"Object" x] (when (some? x) (.getClass x)))

;;;
 ; Throws a ClassCastException if x is not a c, else returns x.
 ;;
(defn cast [#_"Class" c x] (.cast c x))

;;;
 ; Evaluates x and tests if it is an instance of class c. Returns true or false.
 ;;
(defn instance? [#_"Class" c x] (.isInstance c x))

(defn class?   [x] (instance? Class x))
(defn boolean? [x] (instance? Boolean x))
(defn char?    [x] (instance? Character x))
(defn number?  [x] (instance? Number x))
(defn string?  [x] (instance? String x))

(defn integer? [n]
    (or (instance? Long n)
        (instance? BigInteger n)
        (instance? Integer n)
        (instance? Byte n)
    )
)

(about #_"cloiure.core.Seqable"
    (defp Seqable
        (#_"seq" Seqable'''seq [#_"Seqable" this])
    )

    (-/extend-protocol Seqable clojure.lang.Seqable
        (Seqable'''seq [x] (.seq x))
    )
)

(defn seqable? [x] (satisfies? Seqable x))

;;;
 ; Returns a seq on coll. If coll is empty, returns nil.
 ; (seq nil) returns nil.
 ;;
(defn #_"seq" seq [x] (when (some? x) (Seqable'''seq x)))

;;;
 ; Returns true if coll has no items.
 ; Please use the idiom (seq x) rather than (not (empty? x)).
 ;;
(defn empty? [x] (not (seq x)))

(about #_"cloiure.core.ISeq"
    (defp ISeq
        (#_"Object" ISeq'''first [#_"seq" this])
        (#_"seq" ISeq'''next [#_"seq" this])
    )

    (-/extend-protocol ISeq clojure.lang.ISeq
        (ISeq'''first [s] (.first s))
        (ISeq'''next [s] (.next s))
    )
)

(defn seq? [x] (satisfies? ISeq x))

;;;
 ; Returns the first item in coll. Calls seq on its argument.
 ; If s is nil, returns nil.
 ;;
(defn first [s] (if (seq? s) (ISeq'''first s) (when-some [s (seq s)] (ISeq'''first s))))

;;;
 ; Returns a seq of the items after the first. Calls seq on its argument.
 ; If there are no more items, returns nil.
 ;;
(defn #_"seq" next [s] (if (seq? s) (ISeq'''next s) (when-some [s (seq s)] (ISeq'''next s))))

(defn second [s] (first (next s)))
(defn third  [s] (first (next (next s))))
(defn fourth [s] (first (next (next (next s)))))
(defn last   [s] (if-some [r (next s)] (recur r) (first s)))

(about #_"cloiure.core.IObject"
    (defp IObject
        (#_"boolean" IObject'''equals [#_"IObject" this, #_"Object" that])
        (#_"String" IObject'''toString [#_"IObject" this])
    )

    (-/extend-type Object IObject
        (#_"boolean" IObject'''equals [#_"Object" this, #_"Object" that] (.equals this, that))
        (#_"String" IObject'''toString [#_"Object" this] (.toString this))
    )
)

;;;
 ; With no args, returns the empty string. With one arg x, returns x.toString().
 ; (str nil) returns the empty string.
 ; With more than one arg, returns the concatenation of the str values of the args.
 ;;
(defn #_"String" str
    ([] "")
    ([x] (if (some? x) (IObject'''toString x) ""))
    ([x & y]
        ((fn [#_"StringBuilder" sb s] (recur-when s [(.append sb (str (first s))) (next s)] => (str sb)))
            (StringBuilder. (str x)) y
        )
    )
)

(about #_"cloiure.core.Counted"
    (defp Counted
        (#_"int" Counted'''count [#_"Counted" this])
    )

    (-/extend-protocol Counted
        clojure.lang.Counted (Counted'''count [o] (.count o))
        Object'array         (Counted'''count [a] (Array/getLength a))
        CharSequence         (Counted'''count [s] (.length s))
    )
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
                (throw! (str "count not supported on " (class x)))
        )
    )
)

(about #_"cloiure.core.Hashed"
    (defp Hashed
        (#_"int" Hashed'''hash [#_"Hashed" this])
    )

    (declare Murmur3'hashInt)
    (declare Murmur3'hashLong)

    (-/extend-protocol Hashed
        Object               (Hashed'''hash [o] (.hashCode o))
        String               (Hashed'''hash [s] (Murmur3'hashInt (.hashCode s)))
        Number               (Hashed'''hash [n] (Murmur3'hashLong (.longValue n)))
        BigInteger           (Hashed'''hash [i] (if (< (.bitLength i) 64) (Murmur3'hashLong (.longValue i)) (.hashCode i)))
        clojure.lang.IHashEq (Hashed'''hash [o] (.hasheq o))
    )
)

(defn hashed? [x] (satisfies? Hashed x))

;;;
 ; Returns the hash code of its argument. Note this is the hash code
 ; consistent with =, and thus is different from .hashCode for Integer,
 ; Byte and Clojure collections.
 ;;
(defn f'hash [x] (if (some? x) (Hashed'''hash x) 0))

(defn hash-combine [seed x]
    ;; a la boost
    (bit-xor seed (+ (f'hash x) 0x9e3779b9 (<< seed 6) (>> seed 2)))
)

(about #_"cloiure.core.Compiler"
    (defp Expr
        (#_"Object" Expr'''eval [#_"Expr" this])
        (#_"void" Expr'''emit [#_"Expr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen])
        (#_"Class" Expr'''getClass [#_"Expr" this])
    )

    (defp Assignable
        (#_"Object" Assignable'''evalAssign [#_"Assignable" this, #_"Expr" val])
        (#_"void" Assignable'''emitAssign [#_"Assignable" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" val])
    )

    (defp MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"MaybePrimitive" this])
        (#_"void" MaybePrimitive'''emitUnboxed [#_"MaybePrimitive" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen])
    )

    (defp Literal
        (#_"Object" Literal'''literal [#_"Literal" this])
    )

    (defp Untyped)

    (defp Interop)

    (defp IopMethod
        (#_"int" IopMethod'''numParams [#_"IopMethod" this])
        (#_"String" IopMethod'''getMethodName [#_"IopMethod" this])
        (#_"Type" IopMethod'''getReturnType [#_"IopMethod" this])
        (#_"Type[]" IopMethod'''getArgTypes [#_"IopMethod" this])
        (#_"void" IopMethod'''emit [#_"IopMethod" this, #_"IopObject" fn, #_"ClassVisitor" cv])
    )

    (defp IopObject
        (#_"boolean" IopObject'''supportsMeta [#_"IopObject" this])
        (#_"void" IopObject'''emitStatics [#_"IopObject" this, #_"ClassVisitor" gen])
        (#_"void" IopObject'''emitMethods [#_"IopObject" this, #_"ClassVisitor" gen])
    )

    (defp IParser
        (#_"Expr" IParser'''parse [#_"IParser" this, #_"Context" context, #_"seq" form])
    )

    (defp Recur)
)

(about #_"cloiure.core.Compiler"
    (defp NilExpr)
    (defp BooleanExpr)
    (defp MonitorEnterExpr)
    (defp MonitorExitExpr)
    (defp AssignExpr)
    (defp ImportExpr)
    (defp EmptyExpr)
    (defp ConstantExpr)
    (defp NumberExpr)
    (defp StringExpr)
    (defp KeywordExpr)
    (defp InstanceFieldExpr)
    (defp StaticFieldExpr)
    (defp InstanceMethodExpr)
    (defp StaticMethodExpr)
    (defp UnresolvedVarExpr)
    (defp VarExpr)
    (defp TheVarExpr)
    (defp BodyExpr)
    (defp CatchClause)
    (defp TryExpr)
    (defp ThrowExpr)
    (defp NewExpr)
    (defp MetaExpr)
    (defp IfExpr)
    (defp ListExpr)
    (defp MapExpr)
    (defp SetExpr)
    (defp VectorExpr)
    (defp KeywordInvokeExpr)
    (defp InstanceOfExpr)
    (defp InvokeExpr)
    (defp LocalBinding)
    (defp LocalBindingExpr)
    (defp MethodParamExpr)
    (defp FnMethod)
    (defp FnExpr)
    (defp DefExpr)
    (defp BindingInit)
    (defp LetFnExpr)
    (defp LetExpr)
    (defp RecurExpr)
    (defp NewInstanceMethod)
    (defp NewInstanceExpr)
    (defp CaseExpr)
)

(about #_"cloiure.core.IFn"
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

    (-/extend-protocol IFn clojure.lang.IFn
        (IFn'''invoke
            ([this]                                                   (.invoke this))
            ([this, a1]                                               (.invoke this, a1))
            ([this, a1, a2]                                           (.invoke this, a1, a2))
            ([this, a1, a2, a3]                                       (.invoke this, a1, a2, a3))
            ([this, a1, a2, a3, a4]                                   (.invoke this, a1, a2, a3, a4))
            ([this, a1, a2, a3, a4, a5]                               (.invoke this, a1, a2, a3, a4, a5))
            ([this, a1, a2, a3, a4, a5, a6]                           (.invoke this, a1, a2, a3, a4, a5, a6))
            ([this, a1, a2, a3, a4, a5, a6, a7]                       (.invoke this, a1, a2, a3, a4, a5, a6, a7))
            ([this, a1, a2, a3, a4, a5, a6, a7, a8]                   (.invoke this, a1, a2, a3, a4, a5, a6, a7, a8))
            ([this, a1, a2, a3, a4, a5, a6, a7, a8, a9]               (.invoke this, a1, a2, a3, a4, a5, a6, a7, a8, a9))
            ([this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args] (.invoke this, a1, a2, a3, a4, a5, a6, a7, a8, a9, (to-array args)))
        )
        (IFn'''applyTo [this, args] (.applyTo this, args))
    )
)

;;;
 ; Returns true if x implements IFn.
 ; Note that many data structures (e.g. sets and maps) implement IFn.
 ;;
(defn ifn? [x] (satisfies? IFn x))

(defn- spread [s]
    (cond
        (nil? s) nil
        (nil? (next s)) (seq (first s))
        :else (cons (first s) (spread (next s)))
    )
)

;;;
 ; Creates a new seq containing the items prepended to the rest,
 ; the last of which will be treated as a sequence.
 ;;
(defn list*
    ([s] (seq s))
    ([a s] (cons a s))
    ([a b s] (cons a (cons b s)))
    ([a b c s] (cons a (cons b (cons c s))))
    ([a b c d & s] (cons a (cons b (cons c (cons d (spread s))))))
)

;;;
 ; Applies fn f to the argument list formed by prepending intervening arguments to args.
 ;;
(defn apply
    ([#_"fn" f s] (IFn'''applyTo f, (seq s)))
    ([#_"fn" f a s] (IFn'''applyTo f, (list* a s)))
    ([#_"fn" f a b s] (IFn'''applyTo f, (list* a b s)))
    ([#_"fn" f a b c s] (IFn'''applyTo f, (list* a b c s)))
    ([#_"fn" f a b c d & s] (IFn'''applyTo f, (cons a (cons b (cons c (cons d (spread s)))))))
)

(about #_"cloiure.core.IType"
    (defp IType)

    (-/extend-protocol IType clojure.lang.IType)
)

(defn type? [x] (satisfies? IType x))

(about #_"cloiure.core.IRecord"
    (defp IRecord)

    (-/extend-protocol IRecord clojure.lang.IRecord)
)

(defn record? [x] (satisfies? IRecord x))

(about #_"cloiure.core.INamed"
    (defp INamed
        (#_"String" INamed'''getNamespace [#_"INamed" this])
        (#_"String" INamed'''getName [#_"INamed" this])
    )

    (-/extend-protocol INamed clojure.lang.Named
        (INamed'''getNamespace [this] (.getNamespace this))
        (INamed'''getName [this] (.getName this))
    )
)

(defn named? [x] (satisfies? INamed x))

;;;
 ; Returns the namespace String of a symbol or keyword, or nil if not present.
 ;;
(defn #_"String" namespace [#_"INamed" x] (INamed'''getNamespace x))

;;;
 ; Returns the name String of a string, symbol or keyword.
 ;;
(defn #_"String" name [x] (if (string? x) x (INamed'''getName #_"INamed" x)))

(about #_"cloiure.core.IMeta"
    (defp IMeta
        (#_"meta" IMeta'''meta [#_"IMeta" this])
    )

    (-/extend-protocol IMeta clojure.lang.IMeta
        (IMeta'''meta [this] (.meta this))
    )
)

;;;
 ; Returns the metadata of obj, returns nil if there is no metadata.
 ;;
(defn meta [x] (when (satisfies? IMeta x) (IMeta'''meta #_"IMeta" x)))

(about #_"cloiure.core.IObj"
    (defp IObj
        (#_"IObj" IObj'''withMeta [#_"IObj" this, #_"meta" meta])
    )

    (-/extend-protocol IObj clojure.lang.IObj
        (IObj'''withMeta [this, meta] (.withMeta this, meta))
    )
)

;;;
 ; Returns an object of the same type and value as obj, with map m as its metadata.
 ;;
(defn with-meta [#_"IObj" x m] (IObj'''withMeta x, m))

;;;
 ; Returns an object of the same type and value as x,
 ; with (apply f (meta x) args) as its metadata.
 ;;
(defn vary-meta [x f & args] (with-meta x (apply f (meta x) args)))

(about #_"cloiure.core.IReference"
    (defp IReference
        (#_"meta" IReference'''alterMeta [#_"IReference" this, #_"fn" f, #_"seq" args])
        (#_"meta" IReference'''resetMeta [#_"IReference" this, #_"meta" m])
    )

    (-/extend-protocol IReference clojure.lang.IReference
        (IReference'''alterMeta [this, f, args] (.alterMeta this, f, args))
        (IReference'''resetMeta [this, m] (.resetMeta this, m))
    )
)

;;;
 ; Atomically sets the metadata for a var/atom to be: (apply f its-current-meta args)
 ; f must be free of side-effects.
 ;;
(defn alter-meta! [#_"IReference" r f & args] (IReference'''alterMeta r, f, args))

;;;
 ; Atomically resets the metadata for a var/atom.
 ;;
(defn reset-meta! [#_"IReference" r m] (IReference'''resetMeta r, m))

(about #_"cloiure.core.IDeref"
    (defp IDeref
        (#_"Object" IDeref'''deref [#_"IDeref" this])
    )

    (-/extend-protocol IDeref clojure.lang.IDeref
        (IDeref'''deref [this] (.deref this))
    )
)

;;;
 ; When applied to a var or atom, returns its current state.
 ; When applied to a delay, forces it if not already forced.
 ; See also - realized?. Also reader macro: @.
 ;;
(defn deref [#_"IDeref" ref] (IDeref'''deref ref))

(about #_"cloiure.core.IAtom"
    (defp IAtom
        (#_"boolean" IAtom'''compareAndSet [#_"IAtom" this, #_"Object" o, #_"Object" o'])
        (#_"Object" IAtom'''swap [#_"IAtom" this, #_"fn" f, #_"seq" args])
        (#_"Object" IAtom'''reset [#_"IAtom" this, #_"Object" o'])
        (#_"[Object Object]" IAtom'''swapVals [#_"IAtom" this, #_"fn" f, #_"seq" args])
        (#_"[Object Object]" IAtom'''resetVals [#_"IAtom" this, #_"Object" o'])
    )

    (-/extend-protocol IAtom clojure.lang.IAtom2                         (IAtom'''compareAndSet [this, o, o'] (.compareAndSet this, o, o'))
        (IAtom'''swap     [this, f, args] (.swap     this, f, args)) (IAtom'''reset         [this,    o'] (.reset         this,    o'))
        (IAtom'''swapVals [this, f, args] (.swapVals this, f, args)) (IAtom'''resetVals     [this,    o'] (.resetVals     this,    o'))
    )
)

(about #_"cloiure.core.IPending"
    (defp IPending
        (#_"boolean" IPending'''isRealized [#_"IPending" this])
    )

    (-/extend-protocol IPending clojure.lang.IPending
        (IPending'''isRealized [this] (.isRealized this))
    )
)

;;;
 ; Returns true if a value has been produced for a delay or lazy sequence.
 ;;
(defn realized? [#_"IPending" x] (IPending'''isRealized x))

(about #_"cloiure.core.Sequential"
    (defp Sequential)

    (-/extend-protocol Sequential clojure.lang.Sequential)
)

(defn sequential? [x] (satisfies? Sequential x))

(about #_"cloiure.core.Reversible"
    (defp Reversible
        (#_"seq" Reversible'''rseq [#_"Reversible" this])
    )

    (-/extend-protocol Reversible clojure.lang.Reversible
        (Reversible'''rseq [this] (.rseq this))
    )
)

;;;
 ; Returns, in constant time, a seq of the items in rev (which can be a vector or sorted-map), in reverse order.
 ; If rev is empty, returns nil.
 ;;
(defn rseq [#_"Reversible" s] (Reversible'''rseq s))

(defn reversible? [x] (satisfies? Reversible x))

(about #_"cloiure.core.Sorted"
    (defp Sorted
        (#_"Comparator" Sorted'''comparator [#_"Sorted" this])
        (#_"Object" Sorted'''entryKey [#_"Sorted" this, #_"Object" entry])
        (#_"seq" Sorted'''seq [#_"Sorted" this, #_"boolean" ascending?])
        (#_"seq" Sorted'''seqFrom [#_"Sorted" this, #_"Object" key, #_"boolean" ascending?])
    )

    (-/extend-protocol Sorted clojure.lang.Sorted
        (Sorted'''comparator [this] (.comparator this))
        (Sorted'''entryKey [this, entry] (.entryKey this, entry))
        (Sorted'''seq [this, ascending?] (.seq this, ascending?))
        (Sorted'''seqFrom [this, key, ascending?] (.seqFrom this, key, ascending?))
    )
)

(defn sorted? [x] (satisfies? Sorted x))

(about #_"cloiure.core.Indexed"
    (defp Indexed
        (#_"Object" Indexed'''nth
            [#_"Indexed" this, #_"int" i]
            [#_"Indexed" this, #_"int" i, #_"Object" not-found]
        )
    )

    (-/extend-protocol Indexed clojure.lang.Indexed
        (Indexed'''nth
            ([this, i] (.nth this, i))
            ([this, i, not-found] (.nth this, i, not-found))
        )
    )
)

;;;
 ; Return true if x implements Indexed, indicating efficient lookup by index.
 ;;
(defn indexed? [x] (satisfies? Indexed x))

;;;
 ; Returns the nth next of coll, (seq coll) when n is 0.
 ;;
(defn nthnext [s n] (loop-when-recur [s (seq s) n n] (and s (pos? n)) [(next s) (dec n)] => s))

(about #_"cloiure.core.ILookup"
    (defp ILookup
        (#_"Object" ILookup'''valAt
            [#_"ILookup" this, #_"Object" key]
            [#_"ILookup" this, #_"Object" key, #_"Object" not-found]
        )
    )

    (-/extend-protocol ILookup clojure.lang.ILookup
        (ILookup'''valAt
            ([this, key] (.valAt this, key))
            ([this, key, not-found] (.valAt this, key, not-found))
        )
    )
)

(about #_"cloiure.core.ILookupSite"
    (defp ILookupSite
        (#_"ILookupThunk" ILookupSite'''fault [#_"ILookupSite" this, #_"Object" target])
    )

    (-/extend-protocol ILookupSite clojure.lang.ILookupSite
        (ILookupSite'''fault [this, target] (.fault this, target))
    )
)

(about #_"cloiure.core.ILookupThunk"
    (defp ILookupThunk
        (#_"Object" ILookupThunk'''get [#_"ILookupThunk" this, #_"Object" target])
    )

    (-/extend-protocol ILookupThunk clojure.lang.ILookupThunk
        (ILookupThunk'''get [this, target] (.get this, target))
    )
)

(about #_"cloiure.core.IMapEntry"
    (defp IMapEntry
        (#_"Object" IMapEntry'''key [#_"IMapEntry" this])
        (#_"Object" IMapEntry'''val [#_"IMapEntry" this])
    )

    (-/extend-protocol IMapEntry clojure.lang.IMapEntry
        (IMapEntry'''key [this] (.key this))
        (IMapEntry'''val [this] (.val this))
    )
)

(defn map-entry? [x] (satisfies? IMapEntry x))

;;;
 ; Returns the key/value of/in the map entry.
 ;;
(defn key [#_"IMapEntry" e] (IMapEntry'''key e))
(defn val [#_"IMapEntry" e] (IMapEntry'''val e))

;;;
 ; Returns a sequence of the map's keys/values, in the same order as (seq m).
 ;;
(defn keys [m] (map key m))
(defn vals [m] (map val m))

(about #_"cloiure.core.IPersistentCollection"
    (defp IPersistentCollection
        (#_"IPersistentCollection" IPersistentCollection'''conj [#_"IPersistentCollection" this, #_"Object" o])
        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"IPersistentCollection" this])
    )

    (-/extend-protocol IPersistentCollection clojure.lang.IPersistentCollection
        (IPersistentCollection'''conj [this, o] (.cons this, o))
        (IPersistentCollection'''empty [this] (.empty this))
    )
)

(defn coll? [x] (satisfies? IPersistentCollection x))

(about #_"cloiure.core.IEditableCollection"
    (defp IEditableCollection
        (#_"ITransientCollection" IEditableCollection'''asTransient [#_"IEditableCollection" this])
    )

    (-/extend-protocol IEditableCollection clojure.lang.IEditableCollection
        (IEditableCollection'''asTransient [this] (.asTransient this))
    )
)

(defn editable? [x] (satisfies? IEditableCollection x))

(about #_"cloiure.core.Associative"
    (defp Associative
        (#_"Associative" Associative'''assoc [#_"Associative" this, #_"Object" key, #_"Object" val])
        (#_"boolean" Associative'''containsKey [#_"Associative" this, #_"Object" key])
        (#_"IMapEntry" Associative'''entryAt [#_"Associative" this, #_"Object" key])
    )

    (-/extend-protocol Associative clojure.lang.Associative
        (Associative'''assoc [this, key, val] (.assoc this, key, val))
        (Associative'''containsKey [this, key] (.containsKey this, key))
        (Associative'''entryAt [this, key] (.entryAt this, key))
    )
)

(defn associative? [x] (satisfies? Associative x))

(about #_"cloiure.core.IPersistentMap"
    (defp IPersistentMap
        (#_"IPersistentMap" IPersistentMap'''dissoc [#_"IPersistentMap" this, #_"Object" key])
    )

    (-/extend-protocol IPersistentMap clojure.lang.IPersistentMap
        (IPersistentMap'''dissoc [this, key] (.without this, key))
    )
)

(defn map? [x] (satisfies? IPersistentMap x))

(about #_"cloiure.core.IPersistentSet"
    (defp IPersistentSet
        (#_"IPersistentSet" IPersistentSet'''disj [#_"IPersistentSet" this, #_"Object" key])
        (#_"boolean" IPersistentSet'''contains? [#_"IPersistentSet" this, #_"Object" key])
        (#_"Object" IPersistentSet'''get [#_"IPersistentSet" this, #_"Object" key])
    )

    (-/extend-protocol IPersistentSet clojure.lang.IPersistentSet
        (IPersistentSet'''disj [this, key] (.disjoin this, key))
        (IPersistentSet'''contains? [this, key] (.contains this, key))
        (IPersistentSet'''get [this, key] (.get this, key))
    )
)

(defn set? [x] (satisfies? IPersistentSet x))

(about #_"cloiure.core.IPersistentStack"
    (defp IPersistentStack
        (#_"Object" IPersistentStack'''peek [#_"IPersistentStack" this])
        (#_"IPersistentStack" IPersistentStack'''pop [#_"IPersistentStack" this])
    )

    (-/extend-protocol IPersistentStack clojure.lang.IPersistentStack
        (IPersistentStack'''peek [this] (.peek this))
        (IPersistentStack'''pop [this] (.pop this))
    )
)

(defn stack? [x] (satisfies? IPersistentStack x))

(about #_"cloiure.core.IPersistentList"
    (defp IPersistentList)

    (-/extend-protocol IPersistentList clojure.lang.IPersistentList)
)

(defn list? [x] (satisfies? IPersistentList x))

(about #_"cloiure.core.IPersistentVector"
    (defp IPersistentVector
        (#_"IPersistentVector" IPersistentVector'''assocN [#_"IPersistentVector" this, #_"int" i, #_"Object" val])
    )

    (-/extend-protocol IPersistentVector clojure.lang.IPersistentVector
        (IPersistentVector'''assocN [this, i, val] (.assocN this, i, val))
    )
)

(defn vector? [x] (satisfies? IPersistentVector x))

(about #_"cloiure.core.IPersistentWector"
    (defp IPersistentWector
        (#_"IPersistentWector" IPersistentWector'''slicew [#_"IPersistentWector" this, #_"int" start, #_"int" end])
        (#_"IPersistentWector" IPersistentWector'''splicew [#_"IPersistentWector" this, #_"IPersistentWector" that])
    )
)

(defn wector? [x] (satisfies? IPersistentWector x))

(about #_"cloiure.core.ITransientCollection"
    (defp ITransientCollection
        (#_"ITransientCollection" ITransientCollection'''conj! [#_"ITransientCollection" this, #_"Object" val])
        (#_"IPersistentCollection" ITransientCollection'''persistent! [#_"ITransientCollection" this])
    )

    (-/extend-protocol ITransientCollection clojure.lang.ITransientCollection
        (ITransientCollection'''conj! [this, val] (.conj this, val))
        (ITransientCollection'''persistent! [this] (.persistent this))
    )
)

(about #_"cloiure.core.ITransientAssociative"
    (defp ITransientAssociative
        (#_"ITransientAssociative" ITransientAssociative'''assoc! [#_"ITransientAssociative" this, #_"Object" key, #_"Object" val])
        (#_"boolean" ITransientAssociative'''containsKey [#_"ITransientAssociative" this, #_"Object" key])
        (#_"IMapEntry" ITransientAssociative'''entryAt [#_"ITransientAssociative" this, #_"Object" key])
    )

    (-/extend-protocol ITransientAssociative clojure.lang.ITransientAssociative2
        (ITransientAssociative'''assoc! [this, key, val] (.assoc this, key, val))
        (ITransientAssociative'''containsKey [this, key] (.containsKey this, key))
        (ITransientAssociative'''entryAt [this, key] (.entryAt this, key))
    )
)

(about #_"cloiure.core.ITransientMap"
    (defp ITransientMap
        (#_"ITransientMap" ITransientMap'''dissoc! [#_"ITransientMap" this, #_"Object" key])
    )

    (-/extend-protocol ITransientMap clojure.lang.ITransientMap
        (ITransientMap'''dissoc! [this, key] (.without this, key))
    )
)

(about #_"cloiure.core.ITransientSet"
    (defp ITransientSet
        (#_"ITransientSet" ITransientSet'''disj! [#_"ITransientSet" this, #_"Object" key])
        (#_"boolean" ITransientSet'''contains? [#_"ITransientSet" this, #_"Object" key])
        (#_"Object" ITransientSet'''get [#_"ITransientSet" this, #_"Object" key])
    )

    (-/extend-protocol ITransientSet clojure.lang.ITransientSet
        (ITransientSet'''disj! [this, key] (.disjoin this, key))
        (ITransientSet'''contains? [this, key] (.contains this, key))
        (ITransientSet'''get [this, key] (.get this, key))
    )
)

(about #_"cloiure.core.ITransientVector"
    (defp ITransientVector
        (#_"ITransientVector" ITransientVector'''assocN! [#_"ITransientVector" this, #_"int" i, #_"Object" val])
        (#_"ITransientVector" ITransientVector'''pop! [#_"ITransientVector" this])
    )

    (-/extend-protocol ITransientVector clojure.lang.ITransientVector
        (ITransientVector'''assocN! [this, i, val] (.assocN this, i, val))
        (ITransientVector'''pop! [this] (.pop this))
    )
)

(about #_"cloiure.core.PersistentHashMap"
    (defp INode
        (#_"INode" INode'''assoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key])
        (#_"IMapEntry|Object" INode'''find
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key]
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not-found]
        )
        (#_"seq" INode'''nodeSeq [#_"INode" this])
        (#_"INode" INode'''assocT [#_"INode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissocT [#_"INode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"boolean'" removedLeaf])
        (#_"Object" INode'''kvreduce [#_"INode" this, #_"fn" f, #_"Object" r])
    )

    (-/extend-protocol INode clojure.lang.PersistentHashMap$INode
        (INode'''assoc [this, shift, hash, key, val, addedLeaf] (.assoc this, shift, hash, key, val, addedLeaf))
        (INode'''dissoc [this, shift, hash, key] (.without this, shift, hash, key))
        (INode'''find
            ([this, shift, hash, key] (.find this, shift, hash, key))
            ([this, shift, hash, key, not-found] (.find this, shift, hash, key, not-found))
        )
        (INode'''nodeSeq [this] (.nodeSeq this))
        (INode'''assocT [this, edit, shift, hash, key, val, addedLeaf] (.assoc this, edit, shift, hash, key, val, addedLeaf))
        (INode'''dissocT [this, edit, shift, hash, key, removedLeaf] (.without this, edit, shift, hash, key, removedLeaf))
        (INode'''kvreduce [this, f, r] (.kvreduce this, f, r))
    )
)

(about #_"cloiure.core.IReduce"
    (defp IReduce
        (#_"Object" IReduce'''reduce
            [#_"IReduce" this, #_"fn" f]
            [#_"IReduce" this, #_"fn" f, #_"Object" r]
        )
    )

    (-/extend-protocol IReduce clojure.lang.IReduce
        (IReduce'''reduce
            ([this, f] (.reduce this, f))
            ([this, f, r] (.reduce this, f, r))
        )
    )
)

(about #_"cloiure.core.IKVReduce"
    (defp IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"IKVReduce" this, #_"fn" f, #_"Object" r])
    )

    (-/extend-protocol IKVReduce clojure.lang.IKVReduce
        (IKVReduce'''kvreduce [this, f, r] (.kvreduce this, f, r))
    )
)

(about #_"cloiure.core.Range"
    (defp RangeBoundsCheck
        (#_"boolean" RangeBoundsCheck'''exceededBounds [#_"RangeBoundsCheck" this, #_"Object" val])
    )
)

(about #_"cloiure.core.Ratio"
    (defp Ratio)
)

(about #_"cloiure.core.Numbers"
    (defp Ops
        (#_"Ops" Ops'''combine [#_"Ops" this, #_"Ops" y])
        (#_"Ops" Ops'''opsWithLong [#_"Ops" this, #_"LongOps" x])
        (#_"Ops" Ops'''opsWithRatio [#_"Ops" this, #_"RatioOps" x])
        (#_"Ops" Ops'''opsWithBigInt [#_"Ops" this, #_"BigIntOps" x])
        (#_"boolean" Ops'''eq [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"boolean" Ops'''lt [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"boolean" Ops'''lte [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"boolean" Ops'''isZero [#_"Ops" this, #_"Number" x])
        (#_"boolean" Ops'''isPos [#_"Ops" this, #_"Number" x])
        (#_"boolean" Ops'''isNeg [#_"Ops" this, #_"Number" x])
        (#_"Number" Ops'''add [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"Number" Ops'''negate [#_"Ops" this, #_"Number" x])
        (#_"Number" Ops'''inc [#_"Ops" this, #_"Number" x])
        (#_"Number" Ops'''dec [#_"Ops" this, #_"Number" x])
        (#_"Number" Ops'''multiply [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"Number" Ops'''divide [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"Number" Ops'''quotient [#_"Ops" this, #_"Number" x, #_"Number" y])
        (#_"Number" Ops'''remainder [#_"Ops" this, #_"Number" x, #_"Number" y])
    )

    (defp LongOps)
    (defp RatioOps)
    (defp BigIntOps)
)

(about #_"cloiure.core.Atom"
    (defp Atom)
)

(about #_"cloiure.core.AFn"
    #_abstract
    (defp AFn)
)

(about #_"cloiure.core.Symbol"
    (defp Symbol)

    (-/extend-protocol Symbol clojure.lang.Symbol)
)

(defn symbol? [x] (satisfies? Symbol x))

(about #_"cloiure.core.Keyword"
    (defp Keyword)

    (-/extend-protocol Keyword clojure.lang.Keyword)
)

(defn keyword? [x] (satisfies? Keyword x))

(about #_"cloiure.core.Fn"
    #_abstract
    (defp Fn)

    (-/extend-protocol Fn clojure.lang.Fn)
)

;;;
 ; Returns true if x is an object created via fn.
 ;;
(defn fn? [x] (satisfies? Fn x))

(about #_"cloiure.core.RestFn"
    (defp IRestFn
        (#_"int" IRestFn'''requiredArity [#_"IRestFn" this])
        (#_"Object" IRestFn'''doInvoke
            [#_"IRestFn" this, #_"seq" args]
            [#_"IRestFn" this, a1, #_"seq" args]
            [#_"IRestFn" this, a1, a2, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, a4, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, a4, a5, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, a4, a5, a6, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, a4, a5, a6, a7, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, a4, a5, a6, a7, a8, #_"seq" args]
            [#_"IRestFn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args]
        )
    )

    #_abstract
    (defp RestFn)
)

(about #_"cloiure.core.ASeq"
    #_abstract
    (defp ASeq)
)

(about #_"cloiure.core.LazySeq"
    (defp LazySeq)
)

(about #_"cloiure.core.APersistentMap"
    #_abstract
    (defp APersistentMap)
)

(about #_"cloiure.core.APersistentSet"
    #_abstract
    (defp APersistentSet)
)

(about #_"cloiure.core.APersistentVector"
    (defp VSeq)
    (defp RSeq)
    #_abstract
    (defp APersistentVector)
)

(about #_"cloiure.core.AMapEntry"
    #_abstract
    (defp AMapEntry)
)

(about #_"cloiure.core.ArraySeq"
    (defp ArraySeq)
)

(about #_"cloiure.core.ATransientMap"
    (defp IATransientMap
        (#_"void" IATransientMap'''assertEditable [#_"IATransientMap" this])
        (#_"ITransientMap" IATransientMap'''doAssoc [#_"IATransientMap" this, #_"Object" key, #_"Object" val])
        (#_"ITransientMap" IATransientMap'''doDissoc [#_"IATransientMap" this, #_"Object" key])
        (#_"Object" IATransientMap'''doValAt [#_"IATransientMap" this, #_"Object" key, #_"Object" not-found])
        (#_"int" IATransientMap'''doCount [#_"IATransientMap" this])
        (#_"IPersistentMap" IATransientMap'''doPersistent [#_"IATransientMap" this])
    )

    #_abstract
    (defp ATransientMap)
)

(about #_"cloiure.core.ATransientSet"
    #_abstract
    (defp ATransientSet)
)

(about #_"cloiure.core.Cons"
    (defp Cons)
)

(about #_"cloiure.core.Delay"
    (defp Delay)
)

(about #_"cloiure.core.Iterate"
    (defp Iterate)
)

(about #_"cloiure.core.KeywordLookupSite"
    (defp KeywordLookupSite)
)

(about #_"cloiure.core.MapEntry"
    (defp MapEntry)
)

(about #_"cloiure.core.MethodImplCache"
    (defp MethodImplCache)
)

(about #_"cloiure.core.Namespace"
    (defp Namespace)
)

(about #_"cloiure.core.PersistentArrayMap"
    (defp MSeq)
    (defp TransientArrayMap)
    (defp PersistentArrayMap)
)

(about #_"cloiure.core.PersistentHashMap"
    (defp TransientHashMap)
    (defp HSeq)
    (defp ArrayNode)
    (defp BitmapIndexedNode)
    (defp HashCollisionNode)
    (defp NodeSeq)
    (defp PersistentHashMap)
)

(about #_"cloiure.core.PersistentHashSet"
    (defp TransientHashSet)
    (defp PersistentHashSet)
)

(about #_"cloiure.core.PersistentList"
    (defp EmptyList)
    (defp PersistentList)
)

(about #_"cloiure.core.PersistentQueue"
    (defp QSeq)
    (defp PersistentQueue)
)

(about #_"cloiure.core.PersistentTreeMap"
    (defp ITNode
        (#_"ITNode" ITNode'''left [#_"ITNode" this])
        (#_"ITNode" ITNode'''right [#_"ITNode" this])
        (#_"ITNode" ITNode'''addLeft [#_"ITNode" this, #_"ITNode" ins])
        (#_"ITNode" ITNode'''addRight [#_"ITNode" this, #_"ITNode" ins])
        (#_"ITNode" ITNode'''removeLeft [#_"ITNode" this, #_"ITNode" del])
        (#_"ITNode" ITNode'''removeRight [#_"ITNode" this, #_"ITNode" del])
        (#_"ITNode" ITNode'''blacken [#_"ITNode" this])
        (#_"ITNode" ITNode'''redden [#_"ITNode" this])
        (#_"ITNode" ITNode'''balanceLeft [#_"ITNode" this, #_"ITNode" parent])
        (#_"ITNode" ITNode'''balanceRight [#_"ITNode" this, #_"ITNode" parent])
        (#_"ITNode" ITNode'''replace [#_"ITNode" this, #_"Object" key, #_"Object" val, #_"ITNode" left, #_"ITNode" right])
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

(about #_"cloiure.core.PersistentTreeSet"
    (defp PersistentTreeSet)
)

(about #_"arbace.wector"
    (defp WNode)
    (defp TransientWector)
    (defp PersistentWector)
)

(about #_"cloiure.core.Repeat"
    (defp Repeat)
)

(about #_"cloiure.core.Range"
    (defp Range)
)

(about #_"cloiure.core.Reduced"
    (defp Reduced)

    (-/extend-protocol Reduced clojure.lang.Reduced)
)

;;;
 ; Returns true if x is the result of a call to reduced.
 ;;
(defn reduced? [x] (satisfies? Reduced x))

(about #_"cloiure.core.StringSeq"
    (defp StringSeq)
)

(about #_"cloiure.core.Var"
    (defp Unbound)
    (defp Var)

    (-/extend-protocol Var clojure.lang.Var)
)

;;;
 ; Returns true if v is of type Var.
 ;;
(defn var? [v] (satisfies? Var v))

;; naïve reduce to be redefined later with IReduce

(defn reduce
    ([f s] (if-some [s (seq s)] (reduce f (first s) (next s)) (f)))
    ([f r s] (if-some [s (seq s)] (recur f (f r (first s)) (next s)) r))
)

(declare persistent!)
(declare transient)

(defn reduce!
    ([f s] (if-some [s (seq s)] (reduce! f (first s) (next s)) (f)))
    ([f r s] (persistent! (reduce f (transient r) s)))
)

(declare conj!)
(declare conj)

(defn into [to from]
    (if (editable? to)
        (reduce! conj! to from)
        (reduce conj to from)
    )
)

(defmacro update! [x f & z] `(set! ~x (~f ~x ~@z)))

(about #_"cloiure.core.Cache"

(declare get)
(declare dissoc)

(about #_"Cache"
    (defn #_"<K, V> void" Cache'purge [#_"ReferenceQueue" queue, #_"{K Reference<V>}'" cache]
        (when (some? (.poll queue))
            (while (some? (.poll queue)))
            (doseq [#_"IMapEntry<K, Reference<V>>" e @cache]
                (let-when [#_"Reference<V>" r (val e)] (and (some? r) (nil? (.get r)))
                    (swap! cache #(if (identical? (get % (key e)) r) (dissoc % (key e)) %))
                )
            )
        )
        nil
    )
)
)

(about #_"cloiure.core.Loader"

(about #_"Loader"
    (def- #_"{String Reference<Class>}'" Loader'cache (atom {}))
    (def- #_"ReferenceQueue" Loader'queue (ReferenceQueue.))

    (defn #_"Class<?>" Loader'findCached [#_"String" name]
        (when-some [#_"Reference<Class>" r (get @Loader'cache name)]
            (or (.get r) (do (swap! Loader'cache #(if (identical? (get % name) r) (dissoc % name) %)) nil))
        )
    )

    (defp Loader)

    (defn #_"Loader" Loader'new [#_"ClassLoader" parent]
        (-/proxy [ClassLoader arbace.core.Loader] [parent]
            (#_"Class<?>" findClass [#_"Loader" #_this, #_"String" name]
                (or (Loader'findCached name) (throw (ClassNotFoundException. name)))
            )
        )
    )

    (declare bound?)
    (declare ^:dynamic *class-loader*)

    (defn #_"ClassLoader" Loader'context [] (if (bound? #'*class-loader*) *class-loader* (.getContextClassLoader (thread))))

    (defn #_"ClassLoader" Loader'create [] (AccessController/doPrivileged (reify PrivilegedAction (run [_] (Loader'new (Loader'context))))))

    (declare assoc)

    (defn #_"Class" Loader''defineClass [#_"Loader" this, #_"String" name, #_"byte[]" bytes]
        (Cache'purge Loader'queue, Loader'cache)
        (let [#_"Class" c (.defineClass #_"ClassLoader" this, name, bytes, 0, (count bytes))]
            (swap! Loader'cache assoc name (SoftReference. c, Loader'queue))
            c
        )
    )

    (defn #_"Class" Loader'classForName
        ([#_"String" name] (Loader'classForName name, true))
        ([#_"String" name, #_"boolean" load?]
            (let [#_"ClassLoader" loader (Loader'context)
                  #_"Class" c
                    (when-not (satisfies? Loader loader)
                        (Loader'findCached name)
                    )]
                (or c (Class/forName name, load?, loader))
            )
        )
    )

    (defn #_"Class" Loader'classForNameNonLoading [#_"String" name]
        (Loader'classForName name, false)
    )
)
)

(about #_"cloiure.core.Intrinsics"

(about #_"Intrinsics"
    (def #_"{String int|[int]}" Intrinsics'ops
        (hash-map
            "public static int clojure.lang.Numbers.shiftLeftInt(int,int)"                  Opcodes/ISHL
            "public static int clojure.lang.Numbers.shiftRightInt(int,int)"                 Opcodes/ISHR
            "public static int clojure.lang.Numbers.unsignedShiftRightInt(int,int)"         Opcodes/IUSHR
            "public static int clojure.lang.Numbers.unchecked_int_add(int,int)"             Opcodes/IADD
            "public static int clojure.lang.Numbers.unchecked_int_subtract(int,int)"        Opcodes/ISUB
            "public static int clojure.lang.Numbers.unchecked_int_negate(int)"              Opcodes/INEG
            "public static int clojure.lang.Numbers.unchecked_int_inc(int)"               [ Opcodes/ICONST_1 Opcodes/IADD ]
            "public static int clojure.lang.Numbers.unchecked_int_dec(int)"               [ Opcodes/ICONST_1 Opcodes/ISUB ]
            "public static int clojure.lang.Numbers.unchecked_int_multiply(int,int)"        Opcodes/IMUL
            "public static int clojure.lang.Numbers.unchecked_int_divide(int,int)"          Opcodes/IDIV
            "public static int clojure.lang.Numbers.unchecked_int_remainder(int,int)"       Opcodes/IREM

            "public static long clojure.lang.Numbers.and(long,long)"                        Opcodes/LAND
            "public static long clojure.lang.Numbers.or(long,long)"                         Opcodes/LOR
            "public static long clojure.lang.Numbers.xor(long,long)"                        Opcodes/LXOR
            "public static long clojure.lang.Numbers.shiftLeft(long,long)"                [ Opcodes/L2I Opcodes/LSHL ]
            "public static long clojure.lang.Numbers.shiftRight(long,long)"               [ Opcodes/L2I Opcodes/LSHR ]
            "public static long clojure.lang.Numbers.unsignedShiftRight(long,long)"       [ Opcodes/L2I Opcodes/LUSHR ]
            "public static long clojure.lang.Numbers.quotient(long,long)"                   Opcodes/LDIV
            "public static long clojure.lang.Numbers.remainder(long,long)"                  Opcodes/LREM
            "public static long clojure.lang.Numbers.unchecked_add(long,long)"              Opcodes/LADD
            "public static long clojure.lang.Numbers.unchecked_minus(long)"                 Opcodes/LNEG
            "public static long clojure.lang.Numbers.unchecked_minus(long,long)"            Opcodes/LSUB
            "public static long clojure.lang.Numbers.unchecked_multiply(long,long)"         Opcodes/LMUL
            "public static long clojure.lang.Numbers.unchecked_inc(long)"                 [ Opcodes/LCONST_1 Opcodes/LADD ]
            "public static long clojure.lang.Numbers.unchecked_dec(long)"                 [ Opcodes/LCONST_1 Opcodes/LSUB ]

            "public static boolean clojure.lang.RT.aget(boolean[],int)"                     Opcodes/BALOAD
            "public static byte clojure.lang.RT.aget(byte[],int)"                           Opcodes/BALOAD
            "public static char clojure.lang.RT.aget(char[],int)"                           Opcodes/CALOAD
            "public static int clojure.lang.RT.aget(int[],int)"                             Opcodes/IALOAD
            "public static long clojure.lang.RT.aget(long[],int)"                           Opcodes/LALOAD
            "public static java.lang.Object clojure.lang.RT.aget(java.lang.Object[],int)"   Opcodes/AALOAD

            "public static int clojure.lang.RT.alength(boolean[])"             Opcodes/ARRAYLENGTH
            "public static int clojure.lang.RT.alength(byte[])"                Opcodes/ARRAYLENGTH
            "public static int clojure.lang.RT.alength(char[])"                Opcodes/ARRAYLENGTH
            "public static int clojure.lang.RT.alength(int[])"                 Opcodes/ARRAYLENGTH
            "public static int clojure.lang.RT.alength(long[])"                Opcodes/ARRAYLENGTH
            "public static int clojure.lang.RT.alength(java.lang.Object[])"    Opcodes/ARRAYLENGTH

            "public static long clojure.lang.RT.longCast(byte)"                Opcodes/I2L
            "public static long clojure.lang.RT.longCast(int)"                 Opcodes/I2L
            "public static long clojure.lang.RT.longCast(long)"                Opcodes/NOP

            "public static int clojure.lang.RT.uncheckedIntCast(byte)"         Opcodes/NOP
            "public static int clojure.lang.RT.uncheckedIntCast(char)"         Opcodes/NOP
            "public static int clojure.lang.RT.uncheckedIntCast(int)"          Opcodes/NOP
            "public static int clojure.lang.RT.uncheckedIntCast(long)"         Opcodes/L2I

            "public static long clojure.lang.RT.uncheckedLongCast(byte)"       Opcodes/I2L
            "public static long clojure.lang.RT.uncheckedLongCast(int)"        Opcodes/I2L
            "public static long clojure.lang.RT.uncheckedLongCast(long)"       Opcodes/NOP
        )
    )

    ;; map to instructions terminated with comparator for branch to false
    (def #_"{String [int]}" Intrinsics'preds
        (hash-map
            "public static boolean clojure.lang.Numbers.equiv(long,long)"     [ Opcodes/LCMP  Opcodes/IFNE ]
            "public static boolean clojure.lang.Numbers.lt(long,long)"        [ Opcodes/LCMP  Opcodes/IFGE ]
            "public static boolean clojure.lang.Numbers.lte(long,long)"       [ Opcodes/LCMP  Opcodes/IFGT ]
            "public static boolean clojure.lang.Numbers.gt(long,long)"        [ Opcodes/LCMP  Opcodes/IFLE ]
            "public static boolean clojure.lang.Numbers.gte(long,long)"       [ Opcodes/LCMP  Opcodes/IFLT ]

            "public static boolean clojure.lang.Util.equiv(long,long)"        [ Opcodes/LCMP  Opcodes/IFNE ]
            "public static boolean clojure.lang.Util.equiv(boolean,boolean)"  [ Opcodes/IF_ICMPNE ]

            "public static boolean clojure.lang.Numbers.isZero(long)"         [ Opcodes/LCONST_0 Opcodes/LCMP  Opcodes/IFNE ]
            "public static boolean clojure.lang.Numbers.isPos(long)"          [ Opcodes/LCONST_0 Opcodes/LCMP  Opcodes/IFLE ]
            "public static boolean clojure.lang.Numbers.isNeg(long)"          [ Opcodes/LCONST_0 Opcodes/LCMP  Opcodes/IFGE ]
        )
    )
)
)

(about #_"cloiure.core.Reflector"

(about #_"Reflector"
    (defn #_"Class" Reflector'classOf [#_"Object" o]
        (class o)
    )

    (defn #_"boolean" Reflector'isPrimitive [#_"Class" c]
        (and (some? c) (.isPrimitive c) (not (= c Void/TYPE)))
    )

    (defn #_"Field" Reflector'getField [#_"Class" c, #_"String" name, #_"boolean" static?]
        (let [#_"Field[]" allfields (.getFields c)]
            (loop-when [#_"int" i 0] (< i (count allfields))
                (let [#_"Field" f (aget allfields i)]
                    (if (and (= name (.getName f)) (= (Modifier/isStatic (.getModifiers f)) static?))
                        f
                        (recur (inc i))
                    )
                )
            )
        )
    )

    (declare nth)

    (defn #_"vector" Reflector'getMethods [#_"Class" c, #_"int" arity, #_"String" name, #_"boolean" static?]
        (let [matches- #(and (= name (.getName %)) (= (Modifier/isStatic (.getModifiers %)) static?) (= (count (.getParameterTypes %)) arity))
              #_"java.lang.reflect.Method[]" allmethods (.getMethods c)
              [#_"vector" methods #_"vector" bridges]
                (loop-when [methods [] bridges [] #_"int" i 0] (< i (count allmethods)) => [methods bridges]
                    (let [#_"java.lang.reflect.Method" m (aget allmethods i)
                          [methods bridges]
                            (when (matches- m) => [methods bridges]
                                (try
                                    (if (and (.isBridge m) (= (.getMethod c, (.getName m), (.getParameterTypes m)) m))
                                        [methods (conj bridges m)]
                                        [(conj methods m) bridges]
                                    )
                                    (catch NoSuchMethodException _
                                        [methods bridges]
                                    )
                                )
                            )]
                        (recur methods bridges (inc i))
                    )
                )
              methods
                (when (zero? (count methods)) => methods
                    (loop-when [methods methods #_"int" i 0] (< i (count bridges)) => methods
                        (recur (conj methods (nth bridges i)) (inc i))
                    )
                )
              methods
                (when (and (not static?) (.isInterface c)) => methods
                    (let [allmethods (.getMethods Object)]
                        (loop-when [methods methods #_"int" i 0] (< i (count allmethods)) => methods
                            (let [#_"java.lang.reflect.Method" m (aget allmethods i)]
                                (recur (if (matches- m) (conj methods m) methods) (inc i))
                            )
                        )
                    )
                )]
            methods
        )
    )

    (defn #_"Object" Reflector'boxArg [#_"Class" c, #_"Object" arg]
        (let [unexpected! #(throw! (str "unexpected param type, expected: " c ", given: " (.getName (class arg))))]
            (cond
                (not (.isPrimitive c)) (cast c arg)
                (= c Boolean/TYPE)     (cast Boolean arg)
                (= c Character/TYPE)   (cast Character arg)
                (number? arg)
                    (condp = c
                        Integer/TYPE   (.intValue #_"Number" arg)
                        Long/TYPE      (.longValue #_"Number" arg)
                        Byte/TYPE      (.byteValue #_"Number" arg)
                        (unexpected!)
                    )
                :else
                    (unexpected!)
            )
        )
    )

    (defn #_"Object[]" Reflector'boxArgs [#_"Class[]" params, #_"Object[]" args]
        (when (pos? (count params))
            (let [#_"Object[]" a (object-array (count params))]
                (dotimes [#_"int" i (count params)]
                    (aset! a i (Reflector'boxArg (aget params i), (aget args i)))
                )
                a
            )
        )
    )

    (defn #_"boolean" Reflector'paramArgTypeMatch [#_"Class" paramType, #_"Class" argType]
        (cond
            (nil? argType)
                (not (.isPrimitive paramType))
            (or (= paramType argType) (.isAssignableFrom paramType, argType))
                true
            :else
                (condp = paramType
                    Integer/TYPE   (any = argType Integer Long/TYPE Long Byte/TYPE)
                    Long/TYPE      (any = argType Long Integer/TYPE Byte/TYPE)
                    Character/TYPE (= argType Character)
                    Byte/TYPE      (= argType Byte)
                    Boolean/TYPE   (= argType Boolean)
                                   false
                )
        )
    )

    (defn #_"boolean" Reflector'isCongruent [#_"Class[]" params, #_"Object[]" args]
        (when (some? args) => (zero? (count params))
            (and (= (count params) (count args))
                (loop-when-recur [#_"boolean" ? true #_"int" i 0]
                                 (and ? (< i (count params)))
                                 [(Reflector'paramArgTypeMatch (aget params i), (class (aget args i))) (inc i)]
                              => ?
                )
            )
        )
    )

    (defn #_"boolean" Reflector'isMatch [#_"java.lang.reflect.Method" lhs, #_"java.lang.reflect.Method" rhs]
        (and (= (.getName lhs), (.getName rhs)) (Modifier/isPublic (.getModifiers (.getDeclaringClass lhs)))
            (let [#_"Class[]" types1 (.getParameterTypes lhs) #_"Class[]" types2 (.getParameterTypes rhs)]
                (and (= (count types1) (count types2))
                    (loop-when [#_"int" i 0] (< i (count types1)) => true
                        (and (.isAssignableFrom (aget types1 i), (aget types2 i))
                            (recur (inc i))
                        )
                    )
                )
            )
        )
    )

    (defn #_"java.lang.reflect.Method" Reflector'getAsMethodOfPublicBase [#_"Class" c, #_"java.lang.reflect.Method" m]
        (or
            (let [#_"Class[]" ifaces (.getInterfaces c)]
                (loop-when [#_"int" j 0] (< j (count ifaces))
                    (let [#_"java.lang.reflect.Method[]" methods (.getMethods (aget ifaces j))]
                        (or
                            (loop-when [#_"int" i 0] (< i (count methods))
                                (let-when [#_"java.lang.reflect.Method" im (aget methods i)] (Reflector'isMatch im, m) => (recur (inc i))
                                    im
                                )
                            )
                            (recur (inc j))
                        )
                    )
                )
            )
            (when-some [#_"Class" sc (.getSuperclass c)]
                (let [#_"java.lang.reflect.Method[]" methods (.getMethods sc)]
                    (loop-when [#_"int" i 0] (< i (count methods)) => (Reflector'getAsMethodOfPublicBase sc, m)
                        (let-when [#_"java.lang.reflect.Method" scm (aget methods i)] (Reflector'isMatch scm, m) => (recur (inc i))
                            scm
                        )
                    )
                )
            )
        )
    )

    (defn #_"Object" Reflector'prepRet [#_"Class" c, #_"Object" x]
        (cond
            (not (or (.isPrimitive c) (= c Boolean))) x
            (boolean? x)                          (if x true false)
            :else                                     x
        )
    )

    (defn #_"boolean" Reflector'subsumes [#_"Class[]" c1, #_"Class[]" c2]
        ;; presumes matching lengths
        (loop-when [#_"boolean" better false #_"int" i 0] (< i (count c1)) => better
            (when-not (= (aget c1 i) (aget c2 i)) => (recur better (inc i))
                (and (or (and (not (.isPrimitive (aget c1 i))) (.isPrimitive (aget c2 i))) (.isAssignableFrom (aget c2 i), (aget c1 i)))
                    (recur true (inc i))
                )
            )
        )
    )

    (defn #_"Object" Reflector'invokeMatchingMethod [#_"String" methodName, #_"vector" methods, #_"Object" target, #_"Object[]" args]
        (let-when [#_"int" n (count methods)] (pos? n) => (throw! (str "no matching method found: " methodName (when (some? target) (str " for " (class target)))))
            (let [[#_"java.lang.reflect.Method" m #_"Object[]" boxedArgs]
                    (if (= n 1)
                        (let [m (nth methods 0)]
                            [m (Reflector'boxArgs (.getParameterTypes m), args)]
                        )
                        ;; overloaded w/same arity
                        (loop-when [#_"java.lang.reflect.Method" found nil boxedArgs nil #_"seq" s (seq methods)] (some? s) => [found boxedArgs]
                            (let [m (first s) #_"Class[]" params (.getParameterTypes m)
                                  [found boxedArgs]
                                    (if (and (Reflector'isCongruent params, args) (or (nil? found) (Reflector'subsumes params, (.getParameterTypes found))))
                                        [m (Reflector'boxArgs params, args)]
                                        [found boxedArgs]
                                    )]
                                (recur found boxedArgs (next s))
                            )
                        )
                    )]
                (when (some? m) => (throw! (str "no matching method found: " methodName (when (some? target) (str " for " (class target)))))
                    (let [m (when-not (Modifier/isPublic (.getModifiers (.getDeclaringClass m))) => m
                                ;; public method of non-public class, try to find it in hierarchy
                                (or (Reflector'getAsMethodOfPublicBase (class target), m)
                                    (throw! (str "can't call public method of non-public class: " m))
                                )
                            )]
                        (try
                            (Reflector'prepRet (.getReturnType m), (.invoke m, target, boxedArgs))
                            (catch Exception e
                                (throw (or (.getCause e) e))
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"Object" Reflector'invokeInstanceMethod [#_"Object" target, #_"String" methodName, #_"Object[]" args]
        (let [#_"vector" methods (Reflector'getMethods (class target), (count args), methodName, false)]
            (Reflector'invokeMatchingMethod methodName, methods, target, args)
        )
    )

    (defn #_"Object" Reflector'invokeConstructor [#_"Class" c, #_"Object[]" args]
        (try
            (let [#_"Constructor[]" allctors (.getConstructors c)
                  #_"vector" ctors
                    (loop-when [ctors [] #_"int" i 0] (< i (count allctors)) => ctors
                        (let [#_"Constructor" ctor (aget allctors i)
                              ctors
                                (when (= (count (.getParameterTypes ctor)) (count args)) => ctors
                                    (conj ctors ctor)
                                )]
                            (recur ctors (inc i))
                        )
                    )]
                (condp = (count ctors)
                    0   (throw! (str "no matching ctor found for " c))
                    1   (let [#_"Constructor" ctor (nth ctors 0)]
                            (.newInstance ctor, (Reflector'boxArgs (.getParameterTypes ctor), args))
                        )
                    (or ;; overloaded w/same arity
                        (loop-when-recur [#_"seq" s (seq ctors)] (some? s) [(next s)]
                            (let [#_"Constructor" ctor (first s)]
                                (let-when [#_"Class[]" params (.getParameterTypes ctor)] (Reflector'isCongruent params, args)
                                    (.newInstance ctor, (Reflector'boxArgs params, args))
                                )
                            )
                        )
                        (throw! (str "no matching ctor found for " c))
                    )
                )
            )
            (catch Exception e
                (throw (or (.getCause e) e))
            )
        )
    )

    (defn #_"Object" Reflector'invokeStaticMethod [#_"Class" c, #_"String" methodName, #_"Object[]" args]
        (if (= methodName "new")
            (Reflector'invokeConstructor c, args)
            (let [#_"vector" methods (Reflector'getMethods c, (count args), methodName, true)]
                (Reflector'invokeMatchingMethod methodName, methods, nil, args)
            )
        )
    )

    (defn #_"Object" Reflector'getStaticField [#_"Class" c, #_"String" fieldName]
        (let [#_"Field" f (Reflector'getField c, fieldName, true)]
            (when (some? f) => (throw! (str "no matching field found: " fieldName " for " c))
                (Reflector'prepRet (.getType f), (.get f, nil))
            )
        )
    )

    (defn #_"Object" Reflector'setStaticField [#_"Class" c, #_"String" fieldName, #_"Object" val]
        (let [#_"Field" f (Reflector'getField c, fieldName, true)]
            (when (some? f) => (throw! (str "no matching field found: " fieldName " for " c))
                (.set f, nil, (Reflector'boxArg (.getType f), val))
                val
            )
        )
    )

    (defn #_"Object" Reflector'getInstanceField [#_"Object" target, #_"String" fieldName]
        (let [#_"Class" c (class target) #_"Field" f (Reflector'getField c, fieldName, false)]
            (when (some? f) => (throw! (str "no matching field found: " fieldName " for " c))
                (Reflector'prepRet (.getType f), (.get f, target))
            )
        )
    )

    (defn #_"Object" Reflector'setInstanceField [#_"Object" target, #_"String" fieldName, #_"Object" val]
        (let [#_"Class" c (class target) #_"Field" f (Reflector'getField c, fieldName, false)]
            (when (some? f) => (throw! (str "no matching field found: " fieldName " for " (class target)))
                (.set f, target, (Reflector'boxArg (.getType f), val))
                val
            )
        )
    )

    (defn #_"Object" Reflector'invokeNoArgInstanceMember [#_"Object" target, #_"String" name, #_"boolean" requireField]
        (let [#_"Class" c (class target)]
            (if requireField
                (let [#_"Field" f (Reflector'getField c, name, false)]
                    (if (some? f)
                        (Reflector'getInstanceField target, name)
                        (throw! (str "no matching field found: " name " for " (class target)))
                    )
                )
                (let [#_"vector" methods (Reflector'getMethods c, 0, name, false)]
                    (if (pos? (count methods))
                        (Reflector'invokeMatchingMethod name, methods, target, (object-array 0))
                        (Reflector'getInstanceField target, name)
                    )
                )
            )
        )
    )
)
)

(about #_"cloiure.core.Compiler"

(def Context'enum-set
    (hash-set
        :Context'STATEMENT ;; value ignored
        :Context'EXPRESSION ;; value required
        :Context'RETURN ;; tail position relative to enclosing recur frame
        :Context'EVAL
    )
)

(about #_"Compiler"
    (def #_"int" Compiler'MAX_POSITIONAL_ARITY 9)

    (def #_"String" Compiler'COMPILE_STUB_PREFIX "compile__stub")

    (def #_"Symbol" Compiler'FNONCE (with-meta 'fn* {:once true}))

    (defn #_"String" Compiler'cachedClassName [#_"int" n] (str "__cached_class__" n))
    (defn #_"String" Compiler'constantName    [#_"int" n] (str "const__" n))
    (defn #_"String" Compiler'siteNameStatic  [#_"int" n] (str "__site__" n "__"))
    (defn #_"String" Compiler'thunkNameStatic [#_"int" n] (str "__thunk__" n "__"))
)

(about #_"NilExpr"
    (defr NilExpr [])

    (defn #_"NilExpr" NilExpr'new []
        (NilExpr'class.)
    )

    (defm NilExpr Literal
        (#_"Object" Literal'''literal [#_"NilExpr" this]
            nil
        )
    )

    (defm NilExpr Expr
        (#_"Object" Expr'''eval [#_"NilExpr" this]
            (Literal'''literal this)
        )

        (#_"void" Expr'''emit [#_"NilExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (.visitInsn gen, Opcodes/ACONST_NULL)
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"NilExpr" this]
            nil
        )
    )

    (def #_"NilExpr" Compiler'NIL_EXPR (NilExpr'new))
)

(about #_"BooleanExpr"
    (defr BooleanExpr [])

    (defn #_"BooleanExpr" BooleanExpr'new [#_"boolean" val]
        (merge (BooleanExpr'class.)
            (hash-map
                #_"boolean" :val val
            )
        )
    )

    (defm BooleanExpr Literal
        (#_"Object" Literal'''literal [#_"BooleanExpr" this]
            (if (:val this) true false)
        )
    )

    (defm BooleanExpr Expr
        (#_"Object" Expr'''eval [#_"BooleanExpr" this]
            (Literal'''literal this)
        )

        (#_"void" Expr'''emit [#_"BooleanExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (.getStatic gen, (Type/getType Boolean), (if (:val this) "TRUE" "FALSE"), (Type/getType Boolean))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"BooleanExpr" this]
            Boolean
        )
    )

    (def #_"BooleanExpr" Compiler'TRUE_EXPR (BooleanExpr'new true))
    (def #_"BooleanExpr" Compiler'FALSE_EXPR (BooleanExpr'new false))
)

(about #_"Compiler"
    (def #_"Var" ^:dynamic *class-loader*      ) ;; Loader
    (def #_"Var" ^:dynamic *line*              ) ;; Integer
    (def #_"Var" ^:dynamic *last-unique-id*    ) ;; Integer
    (def #_"Var" ^:dynamic *closes*            ) ;; IPersistentMap
    (def #_"Var" ^:dynamic *method*            ) ;; FnFrame
    (def #_"Var" ^:dynamic *local-env*         ) ;; symbol->localbinding
    (def #_"Var" ^:dynamic *last-local-num*    ) ;; Integer
    (def #_"Var" ^:dynamic *loop-locals*       ) ;; vector<localbinding>
    (def #_"Var" ^:dynamic *loop-label*        ) ;; Label
    (def #_"Var" ^:dynamic *constants*         ) ;; vector<object>
    (def #_"Var" ^:dynamic *constant-ids*      ) ;; IdentityHashMap
    (def #_"Var" ^:dynamic *used-constants*    ) ;; IPersistentSet
    (def #_"Var" ^:dynamic *keyword-callsites* ) ;; vector<keyword>
    (def #_"Var" ^:dynamic *protocol-callsites*) ;; vector<var>
    (def #_"Var" ^:dynamic *keywords*          ) ;; keyword->constid
    (def #_"Var" ^:dynamic *vars*              ) ;; var->constid
    (def #_"Var" ^:dynamic *no-recur*          ) ;; Boolean
    (def #_"Var" ^:dynamic *in-catch-finally*  ) ;; Boolean
    (def #_"Var" ^:dynamic *in-return-context* ) ;; Boolean
    (def #_"Var" ^:dynamic *compile-stub-sym*  ) ;; Symbol
    (def #_"Var" ^:dynamic *compile-stub-class*) ;; Class

    (def #_"[Method]" Compiler'createTupleMethods
        (vector
            (Method/getMethod "arbace.core.IPersistentVector create()")
            (Method/getMethod "arbace.core.IPersistentVector create(Object)")
            (Method/getMethod "arbace.core.IPersistentVector create(Object, Object)")
            (Method/getMethod "arbace.core.IPersistentVector create(Object, Object, Object)")
            (Method/getMethod "arbace.core.IPersistentVector create(Object, Object, Object, Object)")
            (Method/getMethod "arbace.core.IPersistentVector create(Object, Object, Object, Object, Object)")
            (Method/getMethod "arbace.core.IPersistentVector create(Object, Object, Object, Object, Object, Object)")
        )
    )

    (def- #_"Type[][]" Compiler'ARG_TYPES
        (let [#_"int" n Compiler'MAX_POSITIONAL_ARITY
              #_"Type[][]" a (make-array (Class/forName "[Lcloiure.asm.Type;") (+ n 2))
              #_"Type" t (Type/getType Object)]
            (dotimes [#_"int" i (inc n)]
                (let [#_"Type[]" b (make-array Type i)]
                    (dotimes [#_"int" j i]
                        (aset! b j t)
                    )
                    (aset! a i b)
                )
            )
            (let [#_"Type[]" b (make-array Type (inc n))]
                (dotimes [#_"int" j n]
                    (aset! b j t)
                )
                (aset! b n (Type/getType Object'array))
                (aset! a (inc n) b)
                a
            )
        )
    )

    (def- #_"Type[]" Compiler'EXCEPTION_TYPES (make-array Type 0))

    (defn #_"boolean" Compiler'inTailCall [#_"Context" context]
        (and (= context :Context'RETURN) *in-return-context* (not *in-catch-finally*))
    )

    (declare Namespace''getAlias)
    (declare find-ns)

    (defn #_"Namespace" Compiler'namespaceFor
        ([#_"Symbol" sym] (Compiler'namespaceFor *ns*, sym))
        ([#_"Namespace" inns, #_"Symbol" sym]
            ;; note, presumes non-nil sym.ns
            (let [#_"Symbol" nsSym (symbol (:ns sym))]
                ;; first check against currentNS' aliases, otherwise check the Namespaces map
                (or (Namespace''getAlias inns, nsSym) (find-ns nsSym))
            )
        )
    )

    (declare Namespace''getMapping)

    (defn #_"Symbol" Compiler'resolveSymbol [#_"Symbol" sym]
        ;; already qualified or classname?
        (cond
            (pos? (.indexOf (:name sym), (int \.)))
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
                        (nil? o)   (symbol (:name (:name *ns*)) (:name sym))
                        (class? o) (symbol (.getName #_"Class" o))
                        (var? o)   (symbol (:name (:name (:ns o))) (:name (:sym o)))
                    )
                )
        )
    )

    (defn #_"Class" Compiler'maybePrimitiveType [#_"Expr" e]
        (let-when [#_"Class" c (Expr'''getClass e)] (Reflector'isPrimitive c)
            (when (and (satisfies? MaybePrimitive e) (MaybePrimitive'''canEmitPrimitive e))
                c
            )
        )
    )

    (defn #_"Class" Compiler'maybeClass [#_"vector" exprs]
        (loop-when [#_"Class" match nil #_"seq" s (seq exprs)] (some? s) => match
            (let [#_"Expr" e (first s)]
                (condp satisfies? e
                    NilExpr   (recur-when (nil? match) [match (next s)])
                    ThrowExpr (recur match (next s))
                              (let [#_"Class" c (Expr'''getClass e)]
                                  (recur-when (and (some? c) (any = match nil c)) [c (next s)])
                              )
                )
            )
        )
    )

    (defn #_"String" Compiler'getTypeStringForArgs [#_"vector" args]
        (let [#_"StringBuilder" sb (StringBuilder.)]
            (dotimes [#_"int" i (count args)]
                (let [#_"Class" c (Expr'''getClass (nth args i))]
                    (when (pos? i)
                        (.append sb, ", ")
                    )
                    (.append sb, (if (some? c) (.getName c) "unknown"))
                )
            )
            (.toString sb)
        )
    )

    (defn #_"int" Compiler'getMatchingParams [#_"String" methodName, #_"vector" pars, #_"vector" args, #_"vector" rets]
        ;; presumes matching lengths
        (let [[#_"int" matchIdx #_"boolean" tied]
                (loop-when [matchIdx -1 tied false #_"boolean" foundExact false #_"int" i 0] (< i (count pars)) => [matchIdx tied]
                    (let [[#_"int" exact #_"boolean" match]
                            (loop-when [exact 0 match true #_"int" p 0 #_"seq" s (seq args)] (and match (< p (count args)) (some? s)) => [exact match]
                                (let [#_"Class" aclass (Expr'''getClass (first s)) #_"Class" pclass (aget (nth pars i) p)
                                      [exact match]
                                        (if (and (some? aclass) (= aclass pclass))
                                            [(inc exact) match]
                                            [exact (Reflector'paramArgTypeMatch pclass, (or aclass Object))]
                                        )]
                                    (recur exact match (inc p) (next s))
                                )
                            )
                          [matchIdx tied foundExact]
                            (cond (= exact (count args))
                                (let [matchIdx
                                        (when (or (not foundExact) (= matchIdx -1) (.isAssignableFrom (nth rets matchIdx), (nth rets i))) => matchIdx
                                            i
                                        )]
                                    [matchIdx false true]
                                )
                                (and match (not foundExact))
                                (let [[matchIdx tied]
                                        (cond (= matchIdx -1)
                                            (do
                                                [i tied]
                                            )
                                            (Reflector'subsumes (nth pars i), (nth pars matchIdx))
                                            (do
                                                [i false]
                                            )
                                            (Arrays/equals (nth pars matchIdx), (nth pars i))
                                            (let [matchIdx
                                                    (when (.isAssignableFrom (nth rets matchIdx), (nth rets i)) => matchIdx
                                                        i
                                                    )]
                                                [matchIdx tied]
                                            )
                                            (not (Reflector'subsumes (nth pars matchIdx), (nth pars i)))
                                            (do
                                                [matchIdx true]
                                            )
                                            :else
                                            (do
                                                [matchIdx tied]
                                            )
                                        )]
                                    [matchIdx tied foundExact]
                                )
                                :else
                                (do
                                    [matchIdx tied foundExact]
                                )
                            )]
                        (recur matchIdx tied foundExact (inc i))
                    )
                )]
            (when tied
                (throw! (str "more than one matching method found: " methodName))
            )
            matchIdx
        )
    )

    (def #_"map" Compiler'CHAR_MAP
        (hash-map
            \- "_"
            \: "_COLON_"
            \+ "_PLUS_"
            \> "_GT_"
            \< "_LT_"
            \= "_EQ_"
            \~ "_TILDE_"
            \! "_BANG_"
            \@ "_CIRCA_"
            \# "_SHARP_"
            \' "_SINGLEQUOTE_"
            \" "_DOUBLEQUOTE_" ;; oops! "
            \% "_PERCENT_"
            \^ "_CARET_"
            \& "_AMPERSAND_"
            \* "_STAR_"
            \| "_BAR_"
            \{ "_LBRACE_"
            \} "_RBRACE_"
            \[ "_LBRACK_"
            \] "_RBRACK_"
            \/ "_SLASH_"
            \\ "_BSLASH_"
            \? "_QMARK_"
        )
    )

    (def #_"map" Compiler'DEMUNGE_MAP
        ;; DEMUNGE_MAP maps strings to characters in the opposite direction that CHAR_MAP does, plus it maps "$" to '/'.
        (loop-when [#_"map" m { "$" \/ } #_"seq" s (seq Compiler'CHAR_MAP)] (some? s) => m
            (let [#_"IMapEntry" e (first s)]
                (recur (-/assoc m (val e) (key e)) (next s))
            )
        )
    )

    (def #_"Pattern" Compiler'DEMUNGE_PATTERN
        ;; DEMUNGE_PATTERN searches for the first of any occurrence of the strings that are keys of DEMUNGE_MAP.
        ;; Note: Regex matching rules mean that #"_|_COLON_" "_COLON_" returns "_", but #"_COLON_|_" "_COLON_"
        ;; returns "_COLON_" as desired. Sorting string keys of DEMUNGE_MAP from longest to shortest ensures
        ;; correct matching behavior, even if some strings are prefixes of others.
        (let [#_"String[]" a (to-array (keys Compiler'DEMUNGE_MAP)) _ (Arrays/sort a, #(- (count %2) (count %1)))
              #_"StringBuilder" sb (StringBuilder.)]
            (dotimes [#_"int" i (count a)]
                (when (pos? i)
                    (.append sb, "|")
                )
                (.append sb, "\\Q")
                (.append sb, (aget a i))
                (.append sb, "\\E")
            )
            (Pattern/compile (.toString sb))
        )
    )

    (defn #_"String" Compiler'munge [#_"String" name]
        (let [#_"StringBuilder" sb (StringBuilder.)]
            (doseq [#_"char" ch name]
                (.append sb, (or (get Compiler'CHAR_MAP ch) ch))
            )
            (.toString sb)
        )
    )

    (defn #_"String" Compiler'demunge [#_"String" mean]
        (let [#_"StringBuilder" sb (StringBuilder.)
              #_"Matcher" m (.matcher Compiler'DEMUNGE_PATTERN, mean)
              #_"int" i
                (loop-when [i 0] (.find m) => i
                    (let [#_"int" start (.start m) #_"int" end (.end m)]
                        ;; keep everything before the match
                        (.append sb, (.substring mean, i, start))
                        ;; replace the match with DEMUNGE_MAP result
                        (.append sb, (get Compiler'DEMUNGE_MAP (.group m)))
                        (recur end)
                    )
                )]
            ;; keep everything after the last match
            (.append sb, (.substring mean, i))
            (.toString sb)
        )
    )

    (defn #_"int" Compiler'nextUniqueId []
        (update! *last-unique-id* inc)
    )

    (defn- #_"int" Compiler'nextLocalNum []
        (update! *last-local-num* inc)
    )

    (declare LocalBinding'new)
    (declare update)

    (defn #_"LocalBinding" Compiler'registerLocal [#_"Symbol" sym, #_"Symbol" tag, #_"Expr" init, #_"boolean" isArg]
        (let [#_"LocalBinding" lb (LocalBinding'new (Compiler'nextLocalNum), sym, tag, init, isArg)]
            (update! *local-env* assoc (:sym lb) lb)
            (update! *method* update :locals assoc (:uid lb) lb)
            lb
        )
    )

    (defn #_"LocalBinding" Compiler'complementLocalInit [#_"LocalBinding" lb, #_"Expr" init]
        (let [lb (assoc lb :init init)]
            (update! *local-env* assoc (:sym lb) lb)
            (update! *method* update :locals assoc (:uid lb) lb)
            lb
        )
    )

    (declare contains?)

    (defn- #_"void" Compiler'closeOver [#_"LocalBinding" lb, #_"IopMethod" m]
        (when (and (some? lb) (some? m) (not (contains? (:locals m) (:uid lb))))
            (update! *closes* update (:uid (:objx m)) assoc (:uid lb) lb)
            (Compiler'closeOver lb, (:parent m))
        )
        nil
    )

    (defn #_"LocalBinding" Compiler'referenceLocal [#_"Symbol" sym]
        (when-some [#_"LocalBinding" lb (get *local-env* sym)]
            (Compiler'closeOver lb, *method*)
            lb
        )
    )

    (defn- #_"int" Compiler'registerConstant [#_"Object" o]
        (when (bound? #'*constants*) => -1
            (or (.get *constant-ids*, o)
                (let [#_"int" n (count *constants*)]
                    (update! *constants* conj o)
                    (.put *constant-ids*, o, n)
                    n
                )
            )
        )
    )

    (defn- #_"int" Compiler'registerKeywordCallsite [#_"Keyword" k]
        (dec (count (update! *keyword-callsites* conj k)))
    )

    (defn- #_"int" Compiler'registerProtocolCallsite [#_"Var" v]
        (dec (count (update! *protocol-callsites* conj v)))
    )

    (defn- #_"void" Compiler'registerVar [#_"Var" var]
        (when (and (bound? #'*vars*) (nil? (get *vars* var)))
            (update! *vars* assoc var (Compiler'registerConstant var))
        )
        nil
    )

    (declare Namespace''intern)
    (declare Namespace''findInternedVar)

    (defn #_"Var" Compiler'lookupVar
        ([#_"Symbol" sym, #_"boolean" internNew] (Compiler'lookupVar sym, internNew, true))
        ([#_"Symbol" sym, #_"boolean" internNew, #_"boolean" registerMacro]
            ;; note - ns-qualified vars in other namespaces must already exist
            (let [#_"Var" var
                    (cond
                        (some? (:ns sym))
                            (when-some [#_"Namespace" ns (Compiler'namespaceFor sym)]
                                (let [#_"Symbol" name (symbol (:name sym))]
                                    (if (and internNew (= ns *ns*))
                                        (Namespace''intern ns, name)
                                        (Namespace''findInternedVar ns, name)
                                    )
                                )
                            )
                        (= sym 'ns)    #'ns
                        (= sym 'in-ns) #'in-ns
                        :else ;; is it mapped?
                            (let [#_"Object" o (Namespace''getMapping *ns*, sym)]
                                (cond
                                    (nil? o) ;; introduce a new var in the current ns
                                        (when internNew
                                            (Namespace''intern *ns*, (symbol (:name sym)))
                                        )
                                    (var? o)
                                        o
                                    :else
                                        (throw! (str "expecting var, but " sym " is mapped to " o))
                                )
                            )
                    )]
                (when (and (some? var) (or (not (get (meta var) :macro)) registerMacro))
                    (Compiler'registerVar var)
                )
                var
            )
        )
    )

    (defn #_"Var" Compiler'isMacro [#_"Object" op]
        ;; no local macros for now
        (when-not (and (symbol? op) (some? (Compiler'referenceLocal op)))
            (when (or (symbol? op) (var? op))
                (let [#_"Var" v (if (var? op) op (Compiler'lookupVar op, false, false))]
                    (when (and (some? v) (get (meta v) :macro))
                        (when (or (= (:ns v) *ns*) (not (get (meta v) :private))) => (throw! (str "var: " v " is private"))
                            v
                        )
                    )
                )
            )
        )
    )

    (defn #_"IFn" Compiler'isInline [#_"Object" op, #_"int" arity]
        ;; no local inlines for now
        (when-not (and (symbol? op) (some? (Compiler'referenceLocal op)))
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

    (defn #_"boolean" Compiler'namesStaticMember [#_"Symbol" sym]
        (and (some? (:ns sym)) (nil? (Compiler'namespaceFor sym)))
    )

    (defn- #_"Symbol" Compiler'tagOf [#_"Object" o]
        (let [#_"Object" tag (get (meta o) :tag)]
            (cond
                (symbol? tag) tag
                (string? tag) (symbol tag)
            )
        )
    )

    (defn #_"Object" Compiler'preserveTag [#_"seq" src, #_"Object" dst]
        (let-when [#_"Symbol" tag (Compiler'tagOf src)] (and (some? tag) (satisfies? IObj dst)) => dst
            (vary-meta dst assoc :tag tag)
        )
    )

    (defn #_"String" Compiler'destubClassName [#_"String" name]
        ;; skip over prefix + '.' or '/'
        (when (.startsWith name, Compiler'COMPILE_STUB_PREFIX) => name
            (.substring name, (inc (count Compiler'COMPILE_STUB_PREFIX)))
        )
    )

    (defn #_"Type" Compiler'getType [#_"Class" c]
        (let [#_"String" desc (.getDescriptor (Type/getType c))
              desc
                (when (.startsWith desc, "L") => desc
                    (str "L" (Compiler'destubClassName (.substring desc, 1)))
                )]
            (Type/getType desc)
        )
    )

    (defn #_"Object" Compiler'resolveIn [#_"Namespace" n, #_"Symbol" sym, #_"boolean" allowPrivate]
        ;; note - ns-qualified vars must already exist
        (cond
            (some? (:ns sym))
                (let-when [#_"Namespace" ns (Compiler'namespaceFor n, sym)] (some? ns)                    => (throw! (str "no such namespace: " (:ns sym)))
                    (let-when [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))] (some? v) => (throw! (str "no such var: " sym))
                        (when (or (= (:ns v) *ns*) (not (get (meta v) :private)) allowPrivate)            => (throw! (str "var: " sym " is private"))
                            v
                        )
                    )
                )
            (or (pos? (.indexOf (:name sym), (int \.))) (= (nth (:name sym) 0) \[)) (Loader'classForName (:name sym))
            (= sym 'ns)                #'ns
            (= sym 'in-ns)             #'in-ns
            (= sym *compile-stub-sym*) *compile-stub-class*
            :else (or (Namespace''getMapping n, sym) (throw! (str "unable to resolve symbol: " sym " in this context")))
        )
    )

    (defn #_"Object" Compiler'resolve
        ([#_"Symbol" sym                          ] (Compiler'resolveIn *ns*, sym, false       ))
        ([#_"Symbol" sym, #_"boolean" allowPrivate] (Compiler'resolveIn *ns*, sym, allowPrivate))
    )

    (defn #_"Object" Compiler'maybeResolveIn [#_"Namespace" n, #_"Symbol" sym]
        ;; note - ns-qualified vars must already exist
        (cond
            (some? (:ns sym))
                (when-some [#_"Namespace" ns (Compiler'namespaceFor n, sym)]
                    (when-some [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))]
                        v
                    )
                )
            (or (and (pos? (.indexOf (:name sym), (int \.))) (not (.endsWith (:name sym), "."))) (= (nth (:name sym) 0) \[))
                (Loader'classForName (:name sym))
            (= sym 'ns)
                #'ns
            (= sym 'in-ns)
                #'in-ns
            :else
                (Namespace''getMapping n, sym)
        )
    )

    (defn #_"boolean" Compiler'inty [#_"Class" c] (any = c Integer/TYPE Byte/TYPE Character/TYPE))

    (defn #_"Class" Compiler'retType [#_"Class" tc, #_"Class" ret]
        (cond
            (nil? tc)
                ret
            (nil? ret)
                tc
            (and (.isPrimitive ret) (.isPrimitive tc))
                (when (or (and (Compiler'inty ret) (Compiler'inty tc)) (= ret tc)) => (throw! (str "cannot coerce " ret " to " tc ": use a cast instead"))
                    tc
                )
            :else
                tc
        )
    )

    (defn #_"Class" Compiler'primClass [#_"Class" c]
        (if (.isPrimitive c) c Object)
    )

    (defn #_"Class" Compiler'boxClass [#_"Class" p]
        (when (.isPrimitive p) => p
            (condp = p
                Integer/TYPE   Integer
                Long/TYPE      Long
                Character/TYPE Character
                Byte/TYPE      Byte
                Boolean/TYPE   Boolean
                               nil
            )
        )
    )
)

(about #_"MonitorEnterExpr"
    (defr MonitorEnterExpr [])

    (defm MonitorEnterExpr Untyped)

    (defn #_"MonitorEnterExpr" MonitorEnterExpr'new [#_"Expr" target]
        (merge (MonitorEnterExpr'class.)
            (hash-map
                #_"Expr" :target target
            )
        )
    )

    (defm MonitorEnterExpr Expr
        (#_"Object" Expr'''eval [#_"MonitorEnterExpr" this]
            (throw! "can't eval monitor-enter")
        )

        (#_"void" Expr'''emit [#_"MonitorEnterExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
            (.monitorEnter gen)
            (Expr'''emit Compiler'NIL_EXPR, context, objx, gen)
            nil
        )

        (#_"Class" Expr'''getClass [#_"MonitorEnterExpr" this]
            nil
        )
    )
)

(declare Compiler'analyze)

(about #_"MonitorEnterParser"
    (defn #_"IParser" MonitorEnterParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (MonitorEnterExpr'new (Compiler'analyze :Context'EXPRESSION, (second form)))
            )
        )
    )
)

(about #_"MonitorExitExpr"
    (defr MonitorExitExpr [])

    (defm MonitorExitExpr Untyped)

    (defn #_"MonitorExitExpr" MonitorExitExpr'new [#_"Expr" target]
        (merge (MonitorExitExpr'class.)
            (hash-map
                #_"Expr" :target target
            )
        )
    )

    (defm MonitorExitExpr Expr
        (#_"Object" Expr'''eval [#_"MonitorExitExpr" this]
            (throw! "can't eval monitor-exit")
        )

        (#_"void" Expr'''emit [#_"MonitorExitExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
            (.monitorExit gen)
            (Expr'''emit Compiler'NIL_EXPR, context, objx, gen)
            nil
        )

        (#_"Class" Expr'''getClass [#_"MonitorExitExpr" this]
            nil
        )
    )
)

(about #_"MonitorExitParser"
    (defn #_"IParser" MonitorExitParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (MonitorExitExpr'new (Compiler'analyze :Context'EXPRESSION, (second form)))
            )
        )
    )
)

(about #_"AssignExpr"
    (defr AssignExpr [])

    (defn #_"AssignExpr" AssignExpr'new [#_"Assignable" target, #_"Expr" val]
        (merge (AssignExpr'class.)
            (hash-map
                #_"Assignable" :target target
                #_"Expr" :val val
            )
        )
    )

    (defm AssignExpr Expr
        (#_"Object" Expr'''eval [#_"AssignExpr" this]
            (Assignable'''evalAssign (:target this), (:val this))
        )

        (#_"void" Expr'''emit [#_"AssignExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Assignable'''emitAssign (:target this), context, objx, gen, (:val this))
            nil
        )

        (#_"Class" Expr'''getClass [#_"AssignExpr" this]
            (Expr'''getClass (:val this))
        )
    )
)

(about #_"AssignParser"
    (defn #_"IParser" AssignParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (when (= (count form) 3) => (throw! "malformed assignment, expecting (set! target val)")
                    (let [#_"Expr" target (Compiler'analyze :Context'EXPRESSION, (second form))]
                        (when (satisfies? Assignable target) => (throw! "invalid assignment target")
                            (AssignExpr'new target, (Compiler'analyze :Context'EXPRESSION, (third form)))
                        )
                    )
                )
            )
        )
    )
)

(about #_"ImportExpr"
    (defr ImportExpr [])

    (defn #_"ImportExpr" ImportExpr'new [#_"String" c]
        (merge (ImportExpr'class.)
            (hash-map
                #_"String" :c c
            )
        )
    )

    (declare Namespace''importClass)
    (defp RT)

    (defm ImportExpr Expr
        (#_"Object" Expr'''eval [#_"ImportExpr" this]
            (Namespace''importClass *ns*, (Loader'classForNameNonLoading (:c this)))
            nil
        )

        (#_"void" Expr'''emit [#_"ImportExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (.getStatic gen, (Type/getType RT'iface), "CURRENT_NS", (Type/getType Var'iface))
            (.invokeVirtual gen, (Type/getType Var'iface), (Method/getMethod "Object deref()"))
            (.checkCast gen, (Type/getType Namespace'iface))
            (.push gen, (:c this))
            (.invokeStatic gen, (Type/getType Loader'iface), (Method/getMethod "Class classForNameNonLoading(String)"))
            (.invokeVirtual gen, (Type/getType Namespace'iface), (Method/getMethod "Class importClass(Class)"))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"ImportExpr" this]
            nil
        )
    )
)

(about #_"ImportParser"
    (defn #_"IParser" ImportParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (ImportExpr'new (second form))
            )
        )
    )
)

(about #_"EmptyExpr"
    (defr EmptyExpr [])

    (defn #_"EmptyExpr" EmptyExpr'new [#_"Object" coll]
        (merge (EmptyExpr'class.)
            (hash-map
                #_"Object" :coll coll
            )
        )
    )

    (defm EmptyExpr Expr
        (#_"Object" Expr'''eval => :coll)

        (#_"void" Expr'''emit [#_"EmptyExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (condp satisfies? (:coll this)
                IPersistentList   (.getStatic gen, (Type/getType PersistentList'iface),     "EMPTY", (Type/getType EmptyList'iface))
                IPersistentVector (.getStatic gen, (Type/getType PersistentWector'iface),   "EMPTY", (Type/getType PersistentWector'iface))
                IPersistentMap    (.getStatic gen, (Type/getType PersistentArrayMap'iface), "EMPTY", (Type/getType PersistentArrayMap'iface))
                IPersistentSet    (.getStatic gen, (Type/getType PersistentHashSet'iface),  "EMPTY", (Type/getType PersistentHashSet'iface))
                                  (throw! "unknown collection type")
            )
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"EmptyExpr" this]
            (condp satisfies? (:coll this)
                IPersistentList   IPersistentList'iface
                IPersistentVector IPersistentVector'iface
                IPersistentMap    IPersistentMap'iface
                IPersistentSet    IPersistentSet'iface
                                  (throw! "unknown collection type")
            )
        )
    )
)

(about #_"ConstantExpr"
    (defr ConstantExpr [])

    (defn #_"ConstantExpr" ConstantExpr'new [#_"Object" v]
        (merge (ConstantExpr'class.)
            (hash-map
                #_"Object" :v v
                #_"int" :id (Compiler'registerConstant v)
            )
        )
    )

    (defm ConstantExpr Literal
        (#_"Object" Literal'''literal => :v)
    )

    (declare IopObject''emitConstant)

    (defm ConstantExpr Expr
        (#_"Object" Expr'''eval [#_"ConstantExpr" this]
            (Literal'''literal this)
        )

        (#_"void" Expr'''emit [#_"ConstantExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''emitConstant objx, gen, (:id this))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"ConstantExpr" this]
            (when (Modifier/isPublic (.getModifiers (class (:v this))))
                (condp satisfies? (:v this)
                    APersistentMap    APersistentMap'iface
                    APersistentSet    APersistentSet'iface
                    APersistentVector APersistentVector'iface
                                      (class (:v this))
                )
            )
        )
    )
)

(about #_"NumberExpr"
    (defr NumberExpr [])

    (defn #_"NumberExpr" NumberExpr'new [#_"Number" n]
        (merge (NumberExpr'class.)
            (hash-map
                #_"Number" :n n
                #_"int" :id (Compiler'registerConstant n)
            )
        )
    )

    (defm NumberExpr Literal
        (#_"Object" Literal'''literal => :n)
    )

    (defm NumberExpr Expr
        (#_"Object" Expr'''eval [#_"NumberExpr" this]
            (Literal'''literal this)
        )

        (#_"void" Expr'''emit [#_"NumberExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (when-not (= context :Context'STATEMENT)
                (IopObject''emitConstant objx, gen, (:id this))
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"NumberExpr" this]
            (condp instance? (:n this)
                Integer Long/TYPE
                Long    Long/TYPE
                        (throw! (str "unsupported Number type: " (.getName (class (:n this)))))
            )
        )
    )

    (defm NumberExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"NumberExpr" this]
            true
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"NumberExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (cond
                (instance? Integer (:n this)) (.push gen, (.longValue (:n this)))
                (instance? Long (:n this))    (.push gen, (.longValue (:n this)))
            )
            nil
        )
    )

    (defn #_"Expr" NumberExpr'parse [#_"Number" form]
        (if (or (instance? Integer form) (instance? Long form))
            (NumberExpr'new form)
            (ConstantExpr'new form)
        )
    )
)

(about #_"StringExpr"
    (defr StringExpr [])

    (defn #_"StringExpr" StringExpr'new [#_"String" str]
        (merge (StringExpr'class.)
            (hash-map
                #_"String" :str str
            )
        )
    )

    (defm StringExpr Literal
        (#_"Object" Literal'''literal => :str)
    )

    (defm StringExpr Expr
        (#_"Object" Expr'''eval [#_"StringExpr" this]
            (Literal'''literal this)
        )

        (#_"void" Expr'''emit [#_"StringExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (when-not (= context :Context'STATEMENT)
                (.push gen, (:str this))
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"StringExpr" this]
            String
        )
    )
)

(about #_"KeywordExpr"
    (defr KeywordExpr [])

    (defn #_"KeywordExpr" KeywordExpr'new [#_"Keyword" k]
        (merge (KeywordExpr'class.)
            (hash-map
                #_"Keyword" :k k
            )
        )
    )

    (defm KeywordExpr Literal
        (#_"Object" Literal'''literal => :k)
    )

    (declare IopObject''emitKeyword)

    (defm KeywordExpr Expr
        (#_"Object" Expr'''eval [#_"KeywordExpr" this]
            (Literal'''literal this)
        )

        (#_"void" Expr'''emit [#_"KeywordExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''emitKeyword objx, gen, (:k this))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"KeywordExpr" this]
            Keyword'iface
        )
    )
)

(about #_"ConstantParser"
    (defn #_"IParser" ConstantParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"int" n (dec (count form))]
                    (when (= n 1) => (throw! (str "wrong number of arguments passed to quote: " n))
                        (let [#_"Object" v (second form)]
                            (cond
                                (nil? v)                          Compiler'NIL_EXPR
                                (= v true)                        Compiler'TRUE_EXPR
                                (= v false)                       Compiler'FALSE_EXPR
                                (number? v)                       (NumberExpr'parse v)
                                (string? v)                       (StringExpr'new v)
                                (and (coll? v) (zero? (count v))) (EmptyExpr'new v)
                                :else                             (ConstantExpr'new v)
                            )
                        )
                    )
                )
            )
        )
    )
)

(defp Numbers)

(about #_"Interop"
    (defn #_"void" Interop'emitBoxReturn [#_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Class" returnType]
        (when (.isPrimitive returnType)
            (condp = returnType
                Boolean/TYPE
                    (let [#_"Label" falseLabel (.newLabel gen) #_"Label" endLabel (.newLabel gen)]
                        (.ifZCmp gen, GeneratorAdapter/EQ, falseLabel)
                        (.getStatic gen, (Type/getType Boolean), "TRUE", (Type/getType Boolean))
                        (.goTo gen, endLabel)
                        (.mark gen, falseLabel)
                        (.getStatic gen, (Type/getType Boolean), "FALSE", (Type/getType Boolean))
                        (.mark gen, endLabel)
                    )
                Byte/TYPE      (.invokeStatic gen, (Type/getType Byte), (Method/getMethod "Byte valueOf(byte)"))
                Character/TYPE (.invokeStatic gen, (Type/getType Character), (Method/getMethod "Character valueOf(char)"))
                Integer/TYPE   (.invokeStatic gen, (Type/getType Integer), (Method/getMethod "Integer valueOf(int)"))
                Long/TYPE      (.invokeStatic gen, (Type/getType Numbers'iface), (Method/getMethod "Number num(long)"))
                Void/TYPE      (Expr'''emit Compiler'NIL_EXPR, :Context'EXPRESSION, objx, gen)
            )
        )
        nil
    )

    (defn #_"void" Interop'emitUnboxArg [#_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Class" paramType]
        (when (.isPrimitive paramType) => (.checkCast gen, (Type/getType paramType))
            (condp = paramType
                Boolean/TYPE
                (do
                    (.checkCast gen, (Type/getType Boolean))
                    (.invokeVirtual gen, (Type/getType Boolean), (Method/getMethod "boolean booleanValue()"))
                )
                Character/TYPE
                (do
                    (.checkCast gen, (Type/getType Character))
                    (.invokeVirtual gen, (Type/getType Character), (Method/getMethod "char charValue()"))
                )
                (do
                    (.checkCast gen, (Type/getType Number))
                    (let [#_"Method" m
                            (condp = paramType
                                Integer/TYPE (Method/getMethod "int intCast(Object)")
                                Long/TYPE    (Method/getMethod "long longCast(Object)")
                              #_Byte/TYPE  #_(Method/getMethod "byte byteCast(Object)")
                                           #_nil
                            )]
                        (.invokeStatic gen, (Type/getType RT'iface), m)
                    )
                )
            )
        )
        nil
    )

    (defn #_"Class" Interop'maybeClass [#_"Object" form, #_"boolean" stringOk]
        (cond
            (class? form)
                form
            (symbol? form)
                (when (nil? (:ns form)) ;; if ns-qualified can't be classname
                    (cond
                        (= form *compile-stub-sym*)
                            *compile-stub-class*
                        (or (pos? (.indexOf (:name form), (int \.))) (= (nth (:name form) 0) \[))
                            (Loader'classForNameNonLoading (:name form))
                        :else
                            (let [#_"Object" o (Namespace''getMapping *ns*, form)]
                                (cond
                                    (class? o)
                                        o
                                    (contains? *local-env* form)
                                        nil
                                    :else
                                        (try
                                            (Loader'classForNameNonLoading (:name form))
                                            (catch Exception _
                                                nil
                                            )
                                        )
                                )
                            )
                    )
                )
            (and stringOk (string? form))
                (Loader'classForNameNonLoading form)
        )
    )

    (defn #_"Class" Interop'primClassForName [#_"Symbol" sym]
        (when (some? sym)
            (case (:name sym)
                "boolean" Boolean/TYPE
                "byte"    Byte/TYPE
                "char"    Character/TYPE
                "int"     Integer/TYPE
                "long"    Long/TYPE
                "void"    Void/TYPE
                          nil
            )
        )
    )

    (defn #_"Class" Interop'maybeSpecialTag [#_"Symbol" sym]
        (or (Interop'primClassForName sym)
            (case (:name sym)
                "booleans" boolean'array
                "bytes"    byte'array
                "chars"    char'array
                "ints"     int'array
                "longs"    long'array
                "objects"  Object'array
                           nil
            )
        )
    )

    (defn #_"Class" Interop'tagToClass [#_"Object" tag]
        (or
            (when (and (symbol? tag) (nil? (:ns tag))) ;; if ns-qualified can't be classname
                (Interop'maybeSpecialTag tag)
            )
            (Interop'maybeClass tag, true)
            (throw! (str "unable to resolve classname: " tag))
        )
    )

    (defn #_"Class" Interop'tagClass [#_"Object" tag]
        (when (some? tag) => Object
            (or
                (when (symbol? tag)
                    (Interop'primClassForName tag)
                )
                (Interop'tagToClass tag)
            )
        )
    )
)

(about #_"InstanceFieldExpr"
    (defr InstanceFieldExpr [])

    (defm InstanceFieldExpr Interop)

    (defn #_"InstanceFieldExpr" InstanceFieldExpr'new [#_"int" line, #_"Expr" target, #_"String" fieldName, #_"Symbol" tag, #_"boolean" requireField]
        (let [#_"Class" c (Expr'''getClass target)
              #_"Field" f (when (some? c) (Reflector'getField c, fieldName, false))]
            (when (and (nil? f) *warn-on-reflection*)
                (if (nil? c)
                    (.println *err*, (str "Reflection warning, line " line " - reference to field " fieldName " can't be resolved."))
                    (.println *err*, (str "Reflection warning, line " line " - reference to field " fieldName " on " (.getName c) " can't be resolved."))
                )
            )
            (merge (InstanceFieldExpr'class.)
                (hash-map
                    #_"Expr" :target target
                    #_"Class" :targetClass c
                    #_"Field" :field f
                    #_"String" :fieldName fieldName
                    #_"int" :line line
                    #_"Symbol" :tag tag
                    #_"boolean" :requireField requireField
                )
            )
        )
    )

    (defp Reflector)

    (defm InstanceFieldExpr Expr
        (#_"Object" Expr'''eval [#_"InstanceFieldExpr" this]
            (Reflector'invokeNoArgInstanceMember (Expr'''eval (:target this)), (:fieldName this), (:requireField this))
        )

        (#_"void" Expr'''emit [#_"InstanceFieldExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (if (and (some? (:targetClass this)) (some? (:field this)))
                (do
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (.checkCast gen, (Compiler'getType (:targetClass this)))
                    (.getField gen, (Compiler'getType (:targetClass this)), (:fieldName this), (Type/getType (.getType (:field this))))
                    (Interop'emitBoxReturn objx, gen, (.getType (:field this)))
                    (when (= context :Context'STATEMENT)
                        (.pop gen)
                    )
                )
                (do
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (.push gen, (:fieldName this))
                    (.push gen, (:requireField this))
                    (.invokeStatic gen, (Type/getType Reflector'iface), (Method/getMethod "Object invokeNoArgInstanceMember(Object, String, boolean)"))
                    (when (= context :Context'STATEMENT)
                        (.pop gen)
                    )
                )
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"InstanceFieldExpr" this]
            (cond (some? (:tag this)) (Interop'tagToClass (:tag this)) (some? (:field this)) (.getType (:field this)))
        )
    )

    (defm InstanceFieldExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"InstanceFieldExpr" this]
            (and (some? (:targetClass this)) (some? (:field this)) (Reflector'isPrimitive (.getType (:field this))))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"InstanceFieldExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (when (and (some? (:targetClass this)) (some? (:field this))) => (throw! "unboxed emit of unknown member")
                (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                (.visitLineNumber gen, (:line this), (.mark gen))
                (.checkCast gen, (Compiler'getType (:targetClass this)))
                (.getField gen, (Compiler'getType (:targetClass this)), (:fieldName this), (Type/getType (.getType (:field this))))
            )
            nil
        )
    )

    (defm InstanceFieldExpr Assignable
        (#_"Object" Assignable'''evalAssign [#_"InstanceFieldExpr" this, #_"Expr" val]
            (Reflector'setInstanceField (Expr'''eval (:target this)), (:fieldName this), (Expr'''eval val))
        )

        (#_"void" Assignable'''emitAssign [#_"InstanceFieldExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" val]
            (if (and (some? (:targetClass this)) (some? (:field this)))
                (do
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.checkCast gen, (Compiler'getType (:targetClass this)))
                    (Expr'''emit val, :Context'EXPRESSION, objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (.dupX1 gen)
                    (Interop'emitUnboxArg objx, gen, (.getType (:field this)))
                    (.putField gen, (Compiler'getType (:targetClass this)), (:fieldName this), (Type/getType (.getType (:field this))))
                )
                (do
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.push gen, (:fieldName this))
                    (Expr'''emit val, :Context'EXPRESSION, objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (.invokeStatic gen, (Type/getType Reflector'iface), (Method/getMethod "Object setInstanceField(Object, String, Object)"))
                )
            )
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )
    )
)

(about #_"StaticFieldExpr"
    (defr StaticFieldExpr [])

    (defm StaticFieldExpr Interop)

    (defn #_"StaticFieldExpr" StaticFieldExpr'new [#_"int" line, #_"Class" c, #_"String" fieldName, #_"Symbol" tag]
        (merge (StaticFieldExpr'class.)
            (hash-map
                #_"int" :line line
                #_"Class" :c c
                #_"String" :fieldName fieldName
                #_"Symbol" :tag tag

                #_"Field" :field (.getField c, fieldName)
            )
        )
    )

    (defm StaticFieldExpr Expr
        (#_"Object" Expr'''eval [#_"StaticFieldExpr" this]
            (Reflector'getStaticField (:c this), (:fieldName this))
        )

        (#_"void" Expr'''emit [#_"StaticFieldExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (.visitLineNumber gen, (:line this), (.mark gen))

            (.getStatic gen, (Type/getType (:c this)), (:fieldName this), (Type/getType (.getType (:field this))))
            (Interop'emitBoxReturn objx, gen, (.getType (:field this)))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"StaticFieldExpr" this]
            (if (some? (:tag this)) (Interop'tagToClass (:tag this)) (.getType (:field this)))
        )
    )

    (defm StaticFieldExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"StaticFieldExpr" this]
            (Reflector'isPrimitive (.getType (:field this)))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"StaticFieldExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (.visitLineNumber gen, (:line this), (.mark gen))
            (.getStatic gen, (Type/getType (:c this)), (:fieldName this), (Type/getType (.getType (:field this))))
            nil
        )
    )

    (defm StaticFieldExpr Assignable
        (#_"Object" Assignable'''evalAssign [#_"StaticFieldExpr" this, #_"Expr" val]
            (Reflector'setStaticField (:c this), (:fieldName this), (Expr'''eval val))
        )

        (#_"void" Assignable'''emitAssign [#_"StaticFieldExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" val]
            (Expr'''emit val, :Context'EXPRESSION, objx, gen)
            (.visitLineNumber gen, (:line this), (.mark gen))
            (.dup gen)
            (Interop'emitUnboxArg objx, gen, (.getType (:field this)))
            (.putStatic gen, (Type/getType (:c this)), (:fieldName this), (Type/getType (.getType (:field this))))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )
    )
)

(about #_"MethodExpr"
    (defn #_"void" MethodExpr'emitArgsAsArray [#_"vector" args, #_"IopObject" objx, #_"GeneratorAdapter" gen]
        (.push gen, (count args))
        (.newArray gen, (Type/getType Object))
        (dotimes [#_"int" i (count args)]
            (.dup gen)
            (.push gen, i)
            (Expr'''emit (nth args i), :Context'EXPRESSION, objx, gen)
            (.arrayStore gen, (Type/getType Object))
        )
        nil
    )

    (defn #_"void" MethodExpr'emitTypedArgs [#_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Class[]" parameterTypes, #_"vector" args]
        (dotimes [#_"int" i (count parameterTypes)]
            (let [#_"Expr" e (nth args i) #_"Class" primc (Compiler'maybePrimitiveType e)]
                (cond
                    (= primc (aget parameterTypes i))
                        (do
                            (MaybePrimitive'''emitUnboxed e, :Context'EXPRESSION, objx, gen)
                        )
                    (and (= primc Integer/TYPE) (= (aget parameterTypes i) Long/TYPE))
                        (do
                            (MaybePrimitive'''emitUnboxed e, :Context'EXPRESSION, objx, gen)
                            (.visitInsn gen, Opcodes/I2L)
                        )
                    (and (= primc Long/TYPE) (= (aget parameterTypes i) Integer/TYPE))
                        (do
                            (MaybePrimitive'''emitUnboxed e, :Context'EXPRESSION, objx, gen)
                            (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "int intCast(long)"))
                        )
                    :else
                        (do
                            (Expr'''emit e, :Context'EXPRESSION, objx, gen)
                            (Interop'emitUnboxArg objx, gen, (aget parameterTypes i))
                        )
                )
            )
        )
        nil
    )
)

(about #_"IopMethod"
    (defn #_"IopMethod" IopMethod'init [#_"IopObject" objx, #_"IopMethod" parent]
        (hash-map
            #_"IopObject" :objx objx
            ;; when closures are defined inside other closures,
            ;; the closed over locals need to be propagated to the enclosing objx
            #_"IopMethod" :parent parent
            ;; uid->localbinding
            #_"map" :locals {}
            #_"Expr" :body nil
            #_"vector" :argLocals nil
            #_"int" :line 0
            #_"meta" :methodMeta nil
        )
    )

    (defn #_"void" IopMethod'emitBody [#_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Class" retClass, #_"Expr" body]
        (if (and (Reflector'isPrimitive retClass) (MaybePrimitive'''canEmitPrimitive body))
            (let [#_"Class" c (Compiler'maybePrimitiveType body)]
                (cond (= c retClass)
                    (do
                        (MaybePrimitive'''emitUnboxed body, :Context'RETURN, objx, gen)
                    )
                    (and (= retClass Long/TYPE) (= c Integer/TYPE))
                    (do
                        (MaybePrimitive'''emitUnboxed body, :Context'RETURN, objx, gen)
                        (.visitInsn gen, Opcodes/I2L)
                    )
                    (and (= retClass Integer/TYPE) (= c Long/TYPE))
                    (do
                        (MaybePrimitive'''emitUnboxed body, :Context'RETURN, objx, gen)
                        (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "int intCast(long)"))
                    )
                    :else
                    (do
                        (throw! (str "mismatched primitive return, expected: " retClass ", had: " (Expr'''getClass body)))
                    )
                )
            )
            (do
                (Expr'''emit body, :Context'RETURN, objx, gen)
                (if (= retClass Void/TYPE)
                    (.pop gen)
                    (.unbox gen, (Type/getType retClass))
                )
            )
        )
        nil
    )

    (defn #_"void" IopMethod''emitClearLocals [#_"IopMethod" this, #_"GeneratorAdapter" gen]
        nil
    )

    (defn #_"void" IopMethod''emitClearThis [#_"IopMethod" this, #_"GeneratorAdapter" gen]
        (.visitInsn gen, Opcodes/ACONST_NULL)
        (.visitVarInsn gen, Opcodes/ASTORE, 0)
        nil
    )
)

(about #_"InstanceMethodExpr"
    (defr InstanceMethodExpr [])

    (defm InstanceMethodExpr Interop)

    (defn #_"InstanceMethodExpr" InstanceMethodExpr'new [#_"int" line, #_"Symbol" tag, #_"Expr" target, #_"String" methodName, #_"vector" args, #_"boolean" tailPosition]
        (let [#_"java.lang.reflect.Method" method
                (if (some? (Expr'''getClass target))
                    (let [#_"vector" methods (Reflector'getMethods (Expr'''getClass target), (count args), methodName, false)]
                        (if (zero? (count methods))
                            (do
                                (when *warn-on-reflection*
                                    (.println *err*, (str "Reflection warning, line " line " - call to method " methodName " on " (.getName (Expr'''getClass target)) " can't be resolved (no such method)."))
                                )
                                nil
                            )
                            (let [#_"int" methodidx
                                    (when (< 1 (count methods)) => 0
                                        (let [[#_"vector" pars #_"vector" rets]
                                                (loop-when [pars [] rets [] #_"int" i 0] (< i (count methods)) => [pars rets]
                                                    (let [#_"java.lang.reflect.Method" m (nth methods i)]
                                                        (recur (conj pars (.getParameterTypes m)) (conj rets (.getReturnType m)) (inc i))
                                                    )
                                                )]
                                            (Compiler'getMatchingParams methodName, pars, args, rets)
                                        )
                                    )
                                #_"java.lang.reflect.Method" m (when (<= 0 methodidx) (nth methods methodidx))
                                m (when (and (some? m) (not (Modifier/isPublic (.getModifiers (.getDeclaringClass m))))) => m
                                        ;; public method of non-public class, try to find it in hierarchy
                                        (Reflector'getAsMethodOfPublicBase (.getDeclaringClass m), m)
                                    )]
                                (when (and (nil? m) *warn-on-reflection*)
                                    (.println *err*, (str "Reflection warning, line " line " - call to method " methodName " on " (.getName (Expr'''getClass target)) " can't be resolved (argument types: " (Compiler'getTypeStringForArgs args) ")."))
                                )
                                m
                            )
                        )
                    )
                    (do
                        (when *warn-on-reflection*
                            (.println *err*, (str "Reflection warning, line " line " - call to method " methodName " can't be resolved (target class is unknown)."))
                        )
                        nil
                    )
                )]
            (merge (InstanceMethodExpr'class.)
                (hash-map
                    #_"int" :line line
                    #_"Symbol" :tag tag
                    #_"Expr" :target target
                    #_"String" :methodName methodName
                    #_"vector" :args args
                    #_"boolean" :tailPosition tailPosition

                    #_"java.lang.reflect.Method" :method method
                )
            )
        )
    )

    (defm InstanceMethodExpr Expr
        (#_"Object" Expr'''eval [#_"InstanceMethodExpr" this]
            (let [#_"Object" target (Expr'''eval (:target this)) #_"Object[]" args (object-array (count (:args this)))]
                (dotimes [#_"int" i (count (:args this))]
                    (aset! args i (Expr'''eval (nth (:args this) i)))
                )
                (if (some? (:method this))
                    (Reflector'invokeMatchingMethod (:methodName this), [(:method this)], target, args)
                    (Reflector'invokeInstanceMethod target, (:methodName this), args)
                )
            )
        )

        (#_"void" Expr'''emit [#_"InstanceMethodExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (if (some? (:method this))
                (let [#_"Type" type (Type/getType (.getDeclaringClass (:method this)))]
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.checkCast gen, type)
                    (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:method this)), (:args this))
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (when (= context :Context'RETURN)
                        (IopMethod''emitClearLocals *method*, gen)
                    )
                    (let [#_"Method" m (Method. (:methodName this), (Type/getReturnType (:method this)), (Type/getArgumentTypes (:method this)))]
                        (if (.isInterface (.getDeclaringClass (:method this)))
                            (.invokeInterface gen, type, m)
                            (.invokeVirtual gen, type, m)
                        )
                        (Interop'emitBoxReturn objx, gen, (.getReturnType (:method this)))
                    )
                )
                (do
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.push gen, (:methodName this))
                    (MethodExpr'emitArgsAsArray (:args this), objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (when (= context :Context'RETURN)
                        (IopMethod''emitClearLocals *method*, gen)
                    )
                    (.invokeStatic gen, (Type/getType Reflector'iface), (Method/getMethod "Object invokeInstanceMethod(Object, String, Object[])"))
                )
            )
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"InstanceMethodExpr" this]
            (Compiler'retType (when (some? (:tag this)) (Interop'tagToClass (:tag this))), (when (some? (:method this)) (.getReturnType (:method this))))
        )
    )

    (defm InstanceMethodExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"InstanceMethodExpr" this]
            (and (some? (:method this)) (Reflector'isPrimitive (.getReturnType (:method this))))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"InstanceMethodExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (when (some? (:method this)) => (throw! "unboxed emit of unknown member")
                (let [#_"Type" type (Type/getType (.getDeclaringClass (:method this)))]
                    (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen)
                    (.checkCast gen, type)
                    (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:method this)), (:args this))
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (when (:tailPosition this)
                        (IopMethod''emitClearThis *method*, gen)
                    )
                    (let [#_"Method" m (Method. (:methodName this), (Type/getReturnType (:method this)), (Type/getArgumentTypes (:method this)))]
                        (if (.isInterface (.getDeclaringClass (:method this)))
                            (.invokeInterface gen, type, m)
                            (.invokeVirtual gen, type, m)
                        )
                    )
                )
            )
            nil
        )
    )
)

(about #_"StaticMethodExpr"
    (defr StaticMethodExpr [])

    (defm StaticMethodExpr Interop)

    (defn #_"StaticMethodExpr" StaticMethodExpr'new [#_"int" line, #_"Symbol" tag, #_"Class" c, #_"String" methodName, #_"vector" args, #_"boolean" tailPosition]
        (let [#_"java.lang.reflect.Method" method
                (let [#_"vector" methods (Reflector'getMethods c, (count args), methodName, true)]
                    (when-not (zero? (count methods)) => (throw! (str "no matching method: " methodName))
                        (let [#_"int" methodidx
                                (when (< 1 (count methods)) => 0
                                    (let [[#_"vector" pars #_"vector" rets]
                                            (loop-when [pars [] rets [] #_"int" i 0] (< i (count methods)) => [pars rets]
                                                (let [#_"java.lang.reflect.Method" m (nth methods i)]
                                                    (recur (conj pars (.getParameterTypes m)) (conj rets (.getReturnType m)) (inc i))
                                                )
                                            )]
                                        (Compiler'getMatchingParams methodName, pars, args, rets)
                                    )
                                )
                              #_"java.lang.reflect.Method" m (when (<= 0 methodidx) (nth methods methodidx))]
                            (when (and (nil? m) *warn-on-reflection*)
                                (.println *err*, (str "Reflection warning, line " line " - call to static method " methodName " on " (.getName c) " can't be resolved (argument types: " (Compiler'getTypeStringForArgs args) ")."))
                            )
                            m
                        )
                    )
                )]
            (merge (StaticMethodExpr'class.)
                (hash-map
                    #_"int" :line line
                    #_"Symbol" :tag tag
                    #_"Class" :c c
                    #_"String" :methodName methodName
                    #_"vector" :args args
                    #_"boolean" :tailPosition tailPosition

                    #_"java.lang.reflect.Method" :method method
                )
            )
        )
    )

    (defn #_"boolean" StaticMethodExpr''canEmitIntrinsicPredicate [#_"StaticMethodExpr" this]
        (and (some? (:method this)) (some? (get Intrinsics'preds (str (:method this)))))
    )

    (declare pop)
    (declare peek)

    (defn #_"void" StaticMethodExpr''emitIntrinsicPredicate [#_"StaticMethodExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Label" falseLabel]
        (.visitLineNumber gen, (:line this), (.mark gen))
        (when (some? (:method this)) => (throw! "unboxed emit of unknown member")
            (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:method this)), (:args this))
            (when (= context :Context'RETURN)
                (IopMethod''emitClearLocals *method*, gen)
            )
            (let [#_"[int]" preds (get Intrinsics'preds (str (:method this)))]
                (doseq [#_"int" pred (pop preds)]
                    (.visitInsn gen, pred)
                )
                (.visitJumpInsn gen, (peek preds), falseLabel)
            )
        )
        nil
    )

    (defm StaticMethodExpr Expr
        (#_"Object" Expr'''eval [#_"StaticMethodExpr" this]
            (let [#_"Object[]" args (object-array (count (:args this)))]
                (dotimes [#_"int" i (count (:args this))]
                    (aset! args i (Expr'''eval (nth (:args this) i)))
                )
                (if (some? (:method this))
                    (Reflector'invokeMatchingMethod (:methodName this), [(:method this)], nil, args)
                    (Reflector'invokeStaticMethod (:c this), (:methodName this), args)
                )
            )
        )

        (#_"void" Expr'''emit [#_"StaticMethodExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (if (some? (:method this))
                (do
                    (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:method this)), (:args this))
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (when (:tailPosition this)
                        (IopMethod''emitClearThis *method*, gen)
                    )
                    (let [#_"Type" type (Type/getType (:c this))
                          #_"Method" m (Method. (:methodName this), (Type/getReturnType (:method this)), (Type/getArgumentTypes (:method this)))]
                        (.invokeStatic gen, type, m)
                        (when (= context :Context'STATEMENT) => (Interop'emitBoxReturn objx, gen, (.getReturnType (:method this)))
                            (let [#_"Class" rc (.getReturnType (:method this))]
                                (cond
                                    (= rc Long/TYPE)       (.pop2 gen)
                                    (not (= rc Void/TYPE)) (.pop gen)
                                )
                            )
                        )
                    )
                )
                (do
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (.push gen, (.getName (:c this)))
                    (.invokeStatic gen, (Type/getType Loader'iface), (Method/getMethod "Class classForName(String)"))
                    (.push gen, (:methodName this))
                    (MethodExpr'emitArgsAsArray (:args this), objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (when (= context :Context'RETURN)
                        (IopMethod''emitClearLocals *method*, gen)
                    )
                    (.invokeStatic gen, (Type/getType Reflector'iface), (Method/getMethod "Object invokeStaticMethod(Class, String, Object[])"))
                    (when (= context :Context'STATEMENT)
                        (.pop gen)
                    )
                )
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"StaticMethodExpr" this]
            (Compiler'retType (when (some? (:tag this)) (Interop'tagToClass (:tag this))), (when (some? (:method this)) (.getReturnType (:method this))))
        )
    )

    (defm StaticMethodExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"StaticMethodExpr" this]
            (and (some? (:method this)) (Reflector'isPrimitive (.getReturnType (:method this))))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"StaticMethodExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (when (some? (:method this)) => (throw! "unboxed emit of unknown member")
                (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:method this)), (:args this))
                (.visitLineNumber gen, (:line this), (.mark gen))
                (when (= context :Context'RETURN)
                    (IopMethod''emitClearLocals *method*, gen)
                )
                (let [#_"int|[int]" ops (get Intrinsics'ops (str (:method this)))]
                    (if (some? ops)
                        (if (vector? ops)
                            (doseq [#_"int" op ops]
                                (.visitInsn gen, op)
                            )
                            (.visitInsn gen, ops)
                        )
                        (let [#_"Method" m (Method. (:methodName this), (Type/getReturnType (:method this)), (Type/getArgumentTypes (:method this)))]
                            (.invokeStatic gen, (Type/getType (:c this)), m)
                        )
                    )
                )
            )
            nil
        )
    )
)

(declare not=)

(about #_"HostParser"
    (defn #_"IParser" HostParser'new []
        (reify IParser
            ;; (. x fieldname-sym) or
            ;; (. x 0-ary-method)
            ;; (. x methodname-sym args+)
            ;; (. x (methodname-sym args?))
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (when-not (< (count form) 3) => (throw! "malformed member expression, expecting (. target member ...)")
                    ;; determine static or instance
                    ;; static target must be symbol, either fully.qualified.Classname or Classname that has been imported
                    (let [#_"int" line *line* #_"Class" c (Interop'maybeClass (second form), false)
                          ;; at this point c will be non-null if static
                          #_"Expr" instance (when (nil? c) (Compiler'analyze (if (= context :Context'EVAL) context :Context'EXPRESSION), (second form)))
                          #_"boolean" maybeField (and (= (count form) 3) (symbol? (third form)))
                          maybeField
                            (when (and maybeField (not= (nth (:name (third form)) 0) \-)) => maybeField
                                (let [#_"String" name (:name (third form))]
                                    (cond
                                        (some? c)
                                            (zero? (count (Reflector'getMethods c, 0, (Compiler'munge name), true)))
                                        (and (some? instance) (some? (Expr'''getClass instance)))
                                            (zero? (count (Reflector'getMethods (Expr'''getClass instance), 0, (Compiler'munge name), false)))
                                        :else
                                            maybeField
                                    )
                                )
                            )]
                        (if maybeField
                            (let [? (= (nth (:name (third form)) 0) \-)
                                  #_"Symbol" sym (if ? (symbol (.substring (:name (third form)), 1)) (third form))
                                  #_"Symbol" tag (Compiler'tagOf form)]
                                (if (some? c)
                                    (StaticFieldExpr'new line, c, (Compiler'munge (:name sym)), tag)
                                    (InstanceFieldExpr'new line, instance, (Compiler'munge (:name sym)), tag, ?)
                                )
                            )
                            (let [#_"seq" call (if (seq? (third form)) (third form) (next (next form)))]
                                (when (symbol? (first call)) => (throw! "malformed member expression")
                                    (let [#_"Symbol" sym (first call)
                                          #_"Symbol" tag (Compiler'tagOf form)
                                          #_"boolean" tailPosition (Compiler'inTailCall context)
                                          #_"vector" args
                                            (loop-when-recur [args [] #_"seq" s (next call)]
                                                             (some? s)
                                                             [(conj args (Compiler'analyze (if (= context :Context'EVAL) context :Context'EXPRESSION), (first s))) (next s)]
                                                          => args
                                            )]
                                        (if (some? c)
                                            (StaticMethodExpr'new line, tag, c, (Compiler'munge (:name sym)), args, tailPosition)
                                            (InstanceMethodExpr'new line, tag, instance, (Compiler'munge (:name sym)), args, tailPosition)
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
)

(about #_"UnresolvedVarExpr"
    (defr UnresolvedVarExpr [])

    (defn #_"UnresolvedVarExpr" UnresolvedVarExpr'new [#_"Symbol" symbol]
        (merge (UnresolvedVarExpr'class.)
            (hash-map
                #_"Symbol" :symbol symbol
            )
        )
    )

    (defm UnresolvedVarExpr Expr
        (#_"Object" Expr'''eval [#_"UnresolvedVarExpr" this]
            (throw! "can't eval")
        )

        (#_"void" Expr'''emit [#_"UnresolvedVarExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            nil
        )

        (#_"Class" Expr'''getClass [#_"UnresolvedVarExpr" this]
            nil
        )
    )
)

(about #_"VarExpr"
    (defr VarExpr [])

    (defn #_"VarExpr" VarExpr'new [#_"Var" var, #_"Symbol" tag]
        (merge (VarExpr'class.)
            (hash-map
                #_"Var" :var var
                #_"Object" :tag (or tag (get (meta var) :tag))
            )
        )
    )

    (declare IopObject''emitVarValue)

    (defm VarExpr Expr
        (#_"Object" Expr'''eval [#_"VarExpr" this]
            (deref (:var this))
        )

        (#_"void" Expr'''emit [#_"VarExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''emitVarValue objx, gen, (:var this))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"VarExpr" this]
            (when (some? (:tag this)) (Interop'tagToClass (:tag this)))
        )
    )

    (declare var-set)
    (declare IopObject''emitVar)

    (defm VarExpr Assignable
        (#_"Object" Assignable'''evalAssign [#_"VarExpr" this, #_"Expr" val]
            (var-set (:var this) (Expr'''eval val))
        )

        (#_"void" Assignable'''emitAssign [#_"VarExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" val]
            (IopObject''emitVar objx, gen, (:var this))
            (Expr'''emit val, :Context'EXPRESSION, objx, gen)
            (.invokeVirtual gen, (Type/getType Var'iface), (Method/getMethod "Object set(Object)"))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )
    )
)

(about #_"TheVarExpr"
    (defr TheVarExpr [])

    (defn #_"TheVarExpr" TheVarExpr'new [#_"Var" var]
        (merge (TheVarExpr'class.)
            (hash-map
                #_"Var" :var var
            )
        )
    )

    (defm TheVarExpr Expr
        (#_"Object" Expr'''eval => :var)

        (#_"void" Expr'''emit [#_"TheVarExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''emitVar objx, gen, (:var this))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"TheVarExpr" this]
            Var'iface
        )
    )
)

(about #_"TheVarParser"
    (defn #_"IParser" TheVarParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"Symbol" sym (second form) #_"Var" v (Compiler'lookupVar sym, false)]
                    (when (some? v) => (throw! (str "unable to resolve var: " sym " in this context"))
                        (TheVarExpr'new v)
                    )
                )
            )
        )
    )
)

(about #_"BodyExpr"
    (defr BodyExpr [])

    (defn #_"BodyExpr" BodyExpr'new [#_"vector" exprs]
        (merge (BodyExpr'class.)
            (hash-map
                #_"vector" :exprs exprs
            )
        )
    )

    (defn- #_"Expr" BodyExpr''lastExpr [#_"BodyExpr" this]
        (nth (:exprs this) (dec (count (:exprs this))))
    )

    (defm BodyExpr Expr
        (#_"Object" Expr'''eval [#_"BodyExpr" this]
            (loop-when-recur [#_"Object" ret nil #_"seq" s (seq (:exprs this))] (some? s) [(Expr'''eval (first s)) (next s)] => ret)
        )

        (#_"void" Expr'''emit [#_"BodyExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (dotimes [#_"int" i (dec (count (:exprs this)))]
                (Expr'''emit (nth (:exprs this) i), :Context'STATEMENT, objx, gen)
            )
            (Expr'''emit (BodyExpr''lastExpr this), context, objx, gen)
            nil
        )

        (#_"Class" Expr'''getClass [#_"BodyExpr" this]
            (Expr'''getClass (BodyExpr''lastExpr this))
        )
    )

    (defm BodyExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"BodyExpr" this]
            (let [#_"Expr" e (BodyExpr''lastExpr this)]
                (and (satisfies? MaybePrimitive e) (MaybePrimitive'''canEmitPrimitive e))
            )
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"BodyExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (dotimes [#_"int" i (dec (count (:exprs this)))]
                (Expr'''emit (nth (:exprs this) i), :Context'STATEMENT, objx, gen)
            )
            (MaybePrimitive'''emitUnboxed (BodyExpr''lastExpr this), context, objx, gen)
            nil
        )
    )
)

(about #_"BodyParser"
    (defn #_"IParser" BodyParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"seq" s form s (if (= (first s) 'do) (next s) s)
                      #_"vector" v
                        (loop-when [v [] s s] (some? s) => v
                            (let [#_"Context" c (if (and (not= context :Context'EVAL) (or (= context :Context'STATEMENT) (some? (next s)))) :Context'STATEMENT context)]
                                (recur (conj v (Compiler'analyze c, (first s))) (next s))
                            )
                        )]
                    (BodyExpr'new (if (pos? (count v)) v (conj v Compiler'NIL_EXPR)))
                )
            )
        )
    )
)

(about #_"CatchClause"
    (defr CatchClause [])

    (defn #_"CatchClause" CatchClause'new [#_"Class" c, #_"LocalBinding" lb, #_"Expr" handler]
        (merge (CatchClause'class.)
            (hash-map
                #_"Class" :c c
                #_"LocalBinding" :lb lb
                #_"Expr" :handler handler
            )
        )
    )
)

(about #_"TryExpr"
    (defr TryExpr [])

    (defn #_"TryExpr" TryExpr'new [#_"Expr" tryExpr, #_"vector" catchExprs, #_"Expr" finallyExpr]
        (merge (TryExpr'class.)
            (hash-map
                #_"Expr" :tryExpr tryExpr
                #_"vector" :catchExprs catchExprs
                #_"Expr" :finallyExpr finallyExpr

                #_"int" :retLocal (Compiler'nextLocalNum)
                #_"int" :finallyLocal (Compiler'nextLocalNum)
            )
        )
    )

    (defm TryExpr Expr
        (#_"Object" Expr'''eval [#_"TryExpr" this]
            (throw! "can't eval try")
        )

        (#_"void" Expr'''emit [#_"TryExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (let [#_"Label" startTry (.newLabel gen) #_"Label" endTry (.newLabel gen) #_"Label" end (.newLabel gen) #_"Label" ret (.newLabel gen) #_"Label" finallyLabel (.newLabel gen)
                  #_"int" n (count (:catchExprs this)) #_"Label[]" labels (make-array Label n) #_"Label[]" endLabels (make-array Label n)]
                (dotimes [#_"int" i n]
                    (aset! labels i (.newLabel gen))
                    (aset! endLabels i (.newLabel gen))
                )

                (.mark gen, startTry)
                (Expr'''emit (:tryExpr this), context, objx, gen)
                (when-not (= context :Context'STATEMENT)
                    (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:retLocal this))
                )
                (.mark gen, endTry)
                (when (some? (:finallyExpr this))
                    (Expr'''emit (:finallyExpr this), :Context'STATEMENT, objx, gen)
                )
                (.goTo gen, ret)

                (dotimes [#_"int" i n]
                    (let [#_"CatchClause" clause (nth (:catchExprs this) i)]
                        (.mark gen, (aget labels i))
                        ;; exception should be on stack
                        ;; put in clause local
                        (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:idx (:lb clause)))
                        (Expr'''emit (:handler clause), context, objx, gen)
                        (when-not (= context :Context'STATEMENT)
                            (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:retLocal this))
                        )
                        (.mark gen, (aget endLabels i))

                        (when (some? (:finallyExpr this))
                            (Expr'''emit (:finallyExpr this), :Context'STATEMENT, objx, gen)
                        )
                        (.goTo gen, ret)
                    )
                )
                (when (some? (:finallyExpr this))
                    (.mark gen, finallyLabel)
                    ;; exception should be on stack
                    (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:finallyLocal this))
                    (Expr'''emit (:finallyExpr this), :Context'STATEMENT, objx, gen)
                    (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ILOAD), (:finallyLocal this))
                    (.throwException gen)
                )
                (.mark gen, ret)
                (when-not (= context :Context'STATEMENT)
                    (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ILOAD), (:retLocal this))
                )
                (.mark gen, end)
                (dotimes [#_"int" i n]
                    (let [#_"CatchClause" clause (nth (:catchExprs this) i)]
                        (.visitTryCatchBlock gen, startTry, endTry, (aget labels i), (.replace (.getName (:c clause)), \., \/))
                    )
                )
                (when (some? (:finallyExpr this))
                    (.visitTryCatchBlock gen, startTry, endTry, finallyLabel, nil)
                    (dotimes [#_"int" i n]
                        (let [#_"CatchClause" _clause (nth (:catchExprs this) i)]
                            (.visitTryCatchBlock gen, (aget labels i), (aget endLabels i), finallyLabel, nil)
                        )
                    )
                )
                (dotimes [#_"int" i n]
                    (let [#_"CatchClause" clause (nth (:catchExprs this) i)]
                        (.visitLocalVariable gen, (:name (:lb clause)), "Ljava/lang/Object;", nil, (aget labels i), (aget endLabels i), (:idx (:lb clause)))
                    )
                )
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"TryExpr" this]
            (Expr'''getClass (:tryExpr this))
        )
    )
)

(declare binding)

(about #_"TryParser"
    (defn #_"IParser" TryParser'new []
        (reify IParser
            ;; (try try-expr* catch-expr* finally-expr?)
            ;; catch-expr: (catch class sym expr*)
            ;; finally-expr: (finally expr*)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (when (= context :Context'RETURN) => (Compiler'analyze context, (list (list Compiler'FNONCE [] form)))
                    (let [[#_"Expr" bodyExpr #_"vector" catches #_"Expr" finallyExpr #_"vector" body]
                            (loop-when [bodyExpr nil catches [] finallyExpr nil body [] #_"boolean" caught? false #_"seq" fs (next form)] (some? fs) => [bodyExpr catches finallyExpr body]
                                (let [#_"Object" f (first fs) #_"Object" op (when (seq? f) (first f))]
                                    (if (any = op 'catch 'finally)
                                        (let [bodyExpr
                                                (when (nil? bodyExpr) => bodyExpr
                                                    (binding [*no-recur* true, *in-return-context* false]
                                                        (IParser'''parse (BodyParser'new), context, (seq body))
                                                    )
                                                )]
                                            (if (= op 'catch)
                                                (let-when [#_"Class" c (Interop'maybeClass (second f), false)] (some? c) => (throw! (str "unable to resolve classname: " (second f)))
                                                    (let-when [#_"Symbol" sym (third f)] (symbol? sym) => (throw! (str "bad binding form, expected symbol, got: " sym))
                                                        (when (nil? (namespace sym)) => (throw! (str "can't bind qualified name: " sym))
                                                            (let [catches
                                                                    (binding [*local-env* *local-env*, *last-local-num* *last-local-num*, *in-catch-finally* true]
                                                                        (let [#_"LocalBinding" lb (Compiler'registerLocal sym, (when (symbol? (second f)) (second f)), nil, false)
                                                                              #_"Expr" handler (IParser'''parse (BodyParser'new), :Context'EXPRESSION, (next (next (next f))))]
                                                                            (conj catches (CatchClause'new c, lb, handler))
                                                                        )
                                                                    )]
                                                                (recur bodyExpr catches finallyExpr body true (next fs))
                                                            )
                                                        )
                                                    )
                                                )
                                                (when (nil? (next fs)) => (throw! "finally clause must be last in try expression")
                                                    (let [finallyExpr
                                                            (binding [*in-catch-finally* true]
                                                                (IParser'''parse (BodyParser'new), :Context'STATEMENT, (next f))
                                                            )]
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
                            )]
                        (when (nil? bodyExpr) => (TryExpr'new bodyExpr, catches, finallyExpr)
                            ;; when there is neither catch nor finally, e.g. (try (expr)) return a body expr directly
                            (binding [*no-recur* true]
                                (IParser'''parse (BodyParser'new), context, (seq body))
                            )
                        )
                    )
                )
            )
        )
    )
)

(about #_"ThrowExpr"
    (defr ThrowExpr [])

    (defm ThrowExpr Untyped)

    (defn #_"ThrowExpr" ThrowExpr'new [#_"Expr" excExpr]
        (merge (ThrowExpr'class.)
            (hash-map
                #_"Expr" :excExpr excExpr
            )
        )
    )

    (defm ThrowExpr Expr
        (#_"Object" Expr'''eval [#_"ThrowExpr" this]
            (throw! "can't eval throw")
        )

        (#_"void" Expr'''emit [#_"ThrowExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Expr'''emit (:excExpr this), :Context'EXPRESSION, objx, gen)
            (.checkCast gen, (Type/getType Throwable))
            (.throwException gen)
            nil
        )

        (#_"Class" Expr'''getClass [#_"ThrowExpr" this]
            nil
        )
    )
)

(about #_"ThrowParser"
    (defn #_"IParser" ThrowParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (cond
                    (= context :Context'EVAL) (Compiler'analyze context, (list (list Compiler'FNONCE [] form)))
                    (= (count form) 1)        (throw! "too few arguments to throw: single Throwable expected")
                    (< 2 (count form))        (throw! "too many arguments to throw: single Throwable expected")
                    :else                     (ThrowExpr'new (Compiler'analyze :Context'EXPRESSION, (second form)))
                )
            )
        )
    )
)

(about #_"NewExpr"
    (defr NewExpr [])

    (defn #_"NewExpr" NewExpr'new [#_"Class" c, #_"vector" args, #_"int" line]
        (let [#_"Constructor" ctor
                (let [#_"Constructor[]" allctors (.getConstructors c)
                      [#_"vector" ctors #_"vector" pars #_"vector" rets]
                        (loop-when [ctors [] pars [] rets [] #_"int" i 0] (< i (count allctors)) => [ctors pars rets]
                            (let [#_"Constructor" ctor (aget allctors i) #_"Class[]" types (.getParameterTypes ctor)
                                  [ctors pars rets]
                                    (when (= (count types) (count args)) => [ctors pars rets]
                                        [(conj ctors ctor) (conj pars types) (conj rets c)]
                                    )]
                                (recur ctors pars rets (inc i))
                            )
                        )]
                    (let-when [#_"int" n (count ctors)] (< 0 n) => (throw! (str "no matching ctor found for " c))
                        (let [#_"int" i (if (< 1 n) (Compiler'getMatchingParams (.getName c), pars, args, rets) 0)
                              #_"Constructor" ctor (when (<= 0 i) (nth ctors i))]
                            (when (and (nil? ctor) *warn-on-reflection*)
                                (.println *err*, (str "Reflection warning, line " line " - call to " (.getName c) " ctor can't be resolved."))
                            )
                            ctor
                        )
                    )
                )]
            (merge (NewExpr'class.)
                (hash-map
                    #_"vector" :args args
                    #_"Constructor" :ctor ctor
                    #_"Class" :c c
                )
            )
        )
    )

    (defm NewExpr Expr
        (#_"Object" Expr'''eval [#_"NewExpr" this]
            (let [#_"Object[]" args (object-array (count (:args this)))]
                (dotimes [#_"int" i (count (:args this))]
                    (aset! args i (Expr'''eval (nth (:args this) i)))
                )
                (when (some? (:ctor this)) => (Reflector'invokeConstructor (:c this), args)
                    (.newInstance (:ctor this), (Reflector'boxArgs (.getParameterTypes (:ctor this)), args))
                )
            )
        )

        (#_"void" Expr'''emit [#_"NewExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (if (some? (:ctor this))
                (let [#_"Type" type (Compiler'getType (:c this))]
                    (.newInstance gen, type)
                    (.dup gen)
                    (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:ctor this)), (:args this))
                    (.invokeConstructor gen, type, (Method. "<init>", (Type/getConstructorDescriptor (:ctor this))))
                )
                (do
                    (.push gen, (Compiler'destubClassName (.getName (:c this))))
                    (.invokeStatic gen, (Type/getType Loader'iface), (Method/getMethod "Class classForName(String)"))
                    (MethodExpr'emitArgsAsArray (:args this), objx, gen)
                    (.invokeStatic gen, (Type/getType Reflector'iface), (Method/getMethod "Object invokeConstructor(Class, Object[])"))
                )
            )
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass => :c)
    )
)

(about #_"NewParser"
    (defn #_"IParser" NewParser'new []
        (reify IParser
            ;; (new Classname args...)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"int" line *line*]
                    (when (< 1 (count form)) => (throw! "wrong number of arguments, expecting: (new Classname args...)")
                        (let [#_"Class" c (Interop'maybeClass (second form), false)]
                            (when (some? c) => (throw! (str "unable to resolve classname: " (second form)))
                                (let [#_"vector" args
                                        (loop-when-recur [args [] #_"seq" s (next (next form))]
                                                         (some? s)
                                                         [(conj args (Compiler'analyze (if (= context :Context'EVAL) context :Context'EXPRESSION), (first s))) (next s)]
                                                      => args
                                        )]
                                    (NewExpr'new c, args, line)
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(about #_"MetaExpr"
    (defr MetaExpr [])

    (defn #_"MetaExpr" MetaExpr'new [#_"Expr" expr, #_"Expr" meta]
        (merge (MetaExpr'class.)
            (hash-map
                #_"Expr" :expr expr
                #_"Expr" :meta meta
            )
        )
    )

    (defm MetaExpr Expr
        (#_"Object" Expr'''eval [#_"MetaExpr" this]
            (with-meta (Expr'''eval (:expr this)) (Expr'''eval (:meta this)))
        )

        (#_"void" Expr'''emit [#_"MetaExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
            (.checkCast gen, (Type/getType IObj'iface))
            (Expr'''emit (:meta this), :Context'EXPRESSION, objx, gen)
            (.checkCast gen, (Type/getType IPersistentMap'iface))
            (.invokeInterface gen, (Type/getType IObj'iface), (Method/getMethod "arbace.core.IObj withMeta(arbace.core.IPersistentMap)"))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"MetaExpr" this]
            (Expr'''getClass (:expr this))
        )
    )
)

(about #_"IfExpr"
    (defr IfExpr [])

    (defn #_"IfExpr" IfExpr'new [#_"int" line, #_"Expr" testExpr, #_"Expr" thenExpr, #_"Expr" elseExpr]
        (merge (IfExpr'class.)
            (hash-map
                #_"int" :line line
                #_"Expr" :testExpr testExpr
                #_"Expr" :thenExpr thenExpr
                #_"Expr" :elseExpr elseExpr
            )
        )
    )

    (defn- #_"void" IfExpr''doEmit [#_"IfExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"boolean" emitUnboxed]
        (let [#_"Label" nullLabel (.newLabel gen) #_"Label" falseLabel (.newLabel gen) #_"Label" endLabel (.newLabel gen)]
            (.visitLineNumber gen, (:line this), (.mark gen))

            (cond (and (satisfies? StaticMethodExpr (:testExpr this)) (StaticMethodExpr''canEmitIntrinsicPredicate (:testExpr this)))
                (do
                    (StaticMethodExpr''emitIntrinsicPredicate (:testExpr this), :Context'EXPRESSION, objx, gen, falseLabel)
                )
                (= (Compiler'maybePrimitiveType (:testExpr this)) Boolean/TYPE)
                (do
                    (MaybePrimitive'''emitUnboxed (:testExpr this), :Context'EXPRESSION, objx, gen)
                    (.ifZCmp gen, GeneratorAdapter/EQ, falseLabel)
                )
                :else
                (do
                    (Expr'''emit (:testExpr this), :Context'EXPRESSION, objx, gen)
                    (.dup gen)
                    (.ifNull gen, nullLabel)
                    (.getStatic gen, (Type/getType Boolean), "FALSE", (Type/getType Boolean))
                    (.visitJumpInsn gen, Opcodes/IF_ACMPEQ, falseLabel)
                )
            )
            (if emitUnboxed
                (MaybePrimitive'''emitUnboxed (:thenExpr this), context, objx, gen)
                (Expr'''emit (:thenExpr this), context, objx, gen)
            )
            (.goTo gen, endLabel)
            (.mark gen, nullLabel)
            (.pop gen)
            (.mark gen, falseLabel)
            (if emitUnboxed
                (MaybePrimitive'''emitUnboxed (:elseExpr this), context, objx, gen)
                (Expr'''emit (:elseExpr this), context, objx, gen)
            )
            (.mark gen, endLabel)
        )
        nil
    )

    (defm IfExpr Expr
        (#_"Object" Expr'''eval [#_"IfExpr" this]
            (Expr'''eval (if (any = (Expr'''eval (:testExpr this)) nil false) (:elseExpr this) (:thenExpr this)))
        )

        (#_"void" Expr'''emit [#_"IfExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IfExpr''doEmit this, context, objx, gen, false)
            nil
        )

        (#_"Class" Expr'''getClass [#_"IfExpr" this]
            (let [#_"Expr" then (:thenExpr this) #_"Class" t (Expr'''getClass then)
                  #_"Expr" else (:elseExpr this) #_"Class" e (Expr'''getClass else)]
                (when (and (or (some? t) (satisfies? NilExpr then))
                           (or (some? e) (satisfies? NilExpr else))
                           (or (= t e)
                               (any = Recur'iface t e)
                               (and (nil? t) (not (.isPrimitive e)))
                               (and (nil? e) (not (.isPrimitive t)))))
                    (if (any = t nil Recur'iface) e t)
                )
            )
        )
    )

    (defm IfExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"IfExpr" this]
            (try
                (let [#_"Expr" then (:thenExpr this) #_"Expr" else (:elseExpr this)]
                    (and (satisfies? MaybePrimitive then)
                         (satisfies? MaybePrimitive else)
                         (let [#_"Class" t (Expr'''getClass then) #_"Class" e (Expr'''getClass else)]
                            (or (= t e)
                                (any = Recur'iface t e)))
                         (MaybePrimitive'''canEmitPrimitive then)
                         (MaybePrimitive'''canEmitPrimitive else))
                )
                (catch Exception _
                    false
                )
            )
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"IfExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IfExpr''doEmit this, context, objx, gen, true)
            nil
        )
    )
)

(about #_"IfParser"
    (defn #_"IParser" IfParser'new []
        (reify IParser
            ;; (if test then) or (if test then else)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (cond
                    (< 4 (count form)) (throw! "too many arguments to if")
                    (< (count form) 3) (throw! "too few arguments to if")
                )
                (let [#_"Expr" test (Compiler'analyze (if (= context :Context'EVAL) context :Context'EXPRESSION), (second form))
                      #_"Expr" then (Compiler'analyze context, (third form))
                      #_"Expr" else (Compiler'analyze context, (fourth form))]
                    (IfExpr'new *line*, test, then, else)
                )
            )
        )
    )
)

(about #_"ListExpr"
    (defr ListExpr [])

    (defn #_"ListExpr" ListExpr'new [#_"vector" args]
        (merge (ListExpr'class.)
            (hash-map
                #_"vector" :args args
            )
        )
    )

    (defm ListExpr Expr
        (#_"Object" Expr'''eval [#_"ListExpr" this]
            (loop-when-recur [#_"vector" v [] #_"int" i 0]
                             (< i (count (:args this)))
                             [(conj v (Expr'''eval (nth (:args this) i))) (inc i)]
                          => (seq v)
            )
        )

        (#_"void" Expr'''emit [#_"ListExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (MethodExpr'emitArgsAsArray (:args this), objx, gen)
            (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.ISeq arrayToSeq(Object[])"))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"ListExpr" this]
            IPersistentList'iface
        )
    )
)

(about #_"MapExpr"
    (defr MapExpr [])

    (defn #_"MapExpr" MapExpr'new [#_"vector" keyvals]
        (merge (MapExpr'class.)
            (hash-map
                #_"vector" :keyvals keyvals
            )
        )
    )

    (declare RT'map)

    (defm MapExpr Expr
        (#_"Object" Expr'''eval [#_"MapExpr" this]
            (let [#_"Object[]" a (object-array (count (:keyvals this)))]
                (dotimes [#_"int" i (count (:keyvals this))]
                    (aset! a i (Expr'''eval (nth (:keyvals this) i)))
                )
                (RT'map a)
            )
        )

        (#_"void" Expr'''emit [#_"MapExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (let [[#_"boolean" allKeysConstant #_"boolean" allConstantKeysUnique]
                    (loop-when [constant? true unique? true #_"IPersistentSet" keys #{} #_"int" i 0] (< i (count (:keyvals this))) => [constant? unique?]
                        (let [#_"Expr" k (nth (:keyvals this) i)
                              [constant? unique? keys]
                                (when (satisfies? Literal k) => [false unique? keys]
                                    (let-when-not [#_"Object" v (Expr'''eval k)] (contains? keys v) => [constant? false keys]
                                        [constant? unique? (conj keys v)]
                                    )
                                )]
                            (recur constant? unique? keys (+ i 2))
                        )
                    )]
                (MethodExpr'emitArgsAsArray (:keyvals this), objx, gen)
                (if (or (and allKeysConstant allConstantKeysUnique) (<= (count (:keyvals this)) 2))
                    (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.IPersistentMap mapUniqueKeys(Object[])"))
                    (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.IPersistentMap map(Object[])"))
                )
                (when (= context :Context'STATEMENT)
                    (.pop gen)
                )
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"MapExpr" this]
            IPersistentMap'iface
        )
    )

    (defn #_"Expr" MapExpr'parse [#_"Context" context, #_"map" form]
        (let [#_"Context" c (if (= context :Context'EVAL) context :Context'EXPRESSION)
              [#_"vector" keyvals #_"boolean" keysConstant #_"boolean" allConstantKeysUnique #_"boolean" valsConstant]
                (loop-when [keyvals [], keysConstant true, allConstantKeysUnique true, #_"IPersistentSet" constantKeys #{}, valsConstant true, #_"seq" s (seq form)] (some? s) => [keyvals keysConstant allConstantKeysUnique valsConstant]
                    (let [#_"IMapEntry" e (first s) #_"Expr" k (Compiler'analyze c, (key e)) #_"Expr" v (Compiler'analyze c, (val e))
                          [keysConstant allConstantKeysUnique constantKeys]
                            (when (satisfies? Literal k) => [false allConstantKeysUnique constantKeys]
                                (let [#_"Object" kval (Expr'''eval k)]
                                    (if (contains? constantKeys kval)
                                        [keysConstant false constantKeys]
                                        [keysConstant allConstantKeysUnique (conj constantKeys kval)]
                                    )
                                )
                            )]
                        (recur (conj keyvals k v) keysConstant allConstantKeysUnique constantKeys (and valsConstant (satisfies? Literal v)) (next s))
                    )
                )
              #_"Expr" e (MapExpr'new keyvals)]
            (cond
                (and (satisfies? IObj form) (some? (meta form)))
                    (MetaExpr'new e, (MapExpr'parse c, (meta form)))
                keysConstant
                    (when allConstantKeysUnique => (throw! "duplicate constant keys in map")
                        (when valsConstant => e
                            (loop-when-recur [#_"map" m {} #_"int" i 0]
                                             (< i (count keyvals))
                                             [(assoc m (Literal'''literal (nth keyvals i)) (Literal'''literal (nth keyvals (inc i)))) (+ i 2)]
                                          => (ConstantExpr'new m)
                            )
                        )
                    )
                :else
                    e
            )
        )
    )
)

(about #_"SetExpr"
    (defr SetExpr [])

    (defn #_"SetExpr" SetExpr'new [#_"vector" keys]
        (merge (SetExpr'class.)
            (hash-map
                #_"vector" :keys keys
            )
        )
    )

    (declare RT'set)

    (defm SetExpr Expr
        (#_"Object" Expr'''eval [#_"SetExpr" this]
            (let [#_"Object[]" a (object-array (count (:keys this)))]
                (dotimes [#_"int" i (count (:keys this))]
                    (aset! a i (Expr'''eval (nth (:keys this) i)))
                )
                (RT'set a)
            )
        )

        (#_"void" Expr'''emit [#_"SetExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (MethodExpr'emitArgsAsArray (:keys this), objx, gen)
            (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.IPersistentSet set(Object[])"))
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"SetExpr" this]
            IPersistentSet'iface
        )
    )

    (defn #_"Expr" SetExpr'parse [#_"Context" context, #_"IPersistentSet" form]
        (let [[#_"vector" keys #_"boolean" constant?]
                (loop-when [keys [] constant? true #_"seq" s (seq form)] (some? s) => [keys constant?]
                    (let [#_"Expr" e (Compiler'analyze (if (= context :Context'EVAL) context :Context'EXPRESSION), (first s))]
                        (recur (conj keys e) (and constant? (satisfies? Literal e)) (next s))
                    )
                )]
            (cond
                (and (satisfies? IObj form) (some? (meta form)))
                    (MetaExpr'new (SetExpr'new keys), (MapExpr'parse (if (= context :Context'EVAL) context :Context'EXPRESSION), (meta form)))
                constant?
                    (loop-when-recur [#_"IPersistentSet" s #{} #_"int" i 0]
                                     (< i (count keys))
                                     [(conj s (Literal'''literal (nth keys i))) (inc i)]
                                  => (ConstantExpr'new s)
                    )
                :else
                    (SetExpr'new keys)
            )
        )
    )
)

(about #_"VectorExpr"
    (defr VectorExpr [])

    (defn #_"VectorExpr" VectorExpr'new [#_"vector" args]
        (merge (VectorExpr'class.)
            (hash-map
                #_"vector" :args args
            )
        )
    )

    (declare Tuple'MAX_SIZE)
    (defp Tuple)

    (defm VectorExpr Expr
        (#_"Object" Expr'''eval [#_"VectorExpr" this]
            (loop-when-recur [#_"vector" v [] #_"int" i 0]
                             (< i (count (:args this)))
                             [(conj v (Expr'''eval (nth (:args this) i))) (inc i)]
                          => v
            )
        )

        (#_"void" Expr'''emit [#_"VectorExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (if (<= (count (:args this)) Tuple'MAX_SIZE)
                (do
                    (dotimes [#_"int" i (count (:args this))]
                        (Expr'''emit (nth (:args this) i), :Context'EXPRESSION, objx, gen)
                    )
                    (.invokeStatic gen, (Type/getType Tuple'iface), (nth Compiler'createTupleMethods (count (:args this))))
                )
                (do
                    (MethodExpr'emitArgsAsArray (:args this), objx, gen)
                    (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.IPersistentVector vector(Object[])"))
                )
            )

            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"VectorExpr" this]
            IPersistentVector'iface
        )
    )

    (defn #_"Expr" VectorExpr'parse [#_"Context" context, #_"vector" form]
        (let [[#_"vector" args #_"boolean" constant?]
                (loop-when [args [] constant? true #_"int" i 0] (< i (count form)) => [args constant?]
                    (let [#_"Expr" e (Compiler'analyze (if (= context :Context'EVAL) context :Context'EXPRESSION), (nth form i))]
                        (recur (conj args e) (and constant? (satisfies? Literal e)) (inc i))
                    )
                )]
            (cond
                (and (satisfies? IObj form) (some? (meta form)))
                    (MetaExpr'new (VectorExpr'new args), (MapExpr'parse (if (= context :Context'EVAL) context :Context'EXPRESSION), (meta form)))
                constant?
                    (loop-when-recur [#_"vector" v [] #_"int" i 0]
                                     (< i (count args))
                                     [(conj v (Literal'''literal (nth args i))) (inc i)]
                                  => (ConstantExpr'new v)
                    )
                :else
                    (VectorExpr'new args)
            )
        )
    )
)

(about #_"KeywordInvokeExpr"
    (defr KeywordInvokeExpr [])

    (defn #_"KeywordInvokeExpr" KeywordInvokeExpr'new [#_"int" line, #_"Symbol" tag, #_"KeywordExpr" kw, #_"Expr" target]
        (merge (KeywordInvokeExpr'class.)
            (hash-map
                #_"int" :line line
                #_"Object" :tag tag
                #_"KeywordExpr" :kw kw
                #_"Expr" :target target

                #_"int" :siteIndex (Compiler'registerKeywordCallsite (:k kw))
            )
        )
    )

    (defm KeywordInvokeExpr Expr
        (#_"Object" Expr'''eval [#_"KeywordInvokeExpr" this]
            (IFn'''invoke (:k (:kw this)), (Expr'''eval (:target this)))
        )

        (#_"void" Expr'''emit [#_"KeywordInvokeExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (let [#_"Label" endLabel (.newLabel gen) #_"Label" faultLabel (.newLabel gen)]
                (.visitLineNumber gen, (:line this), (.mark gen))
                (.getStatic gen, (:objType objx), (Compiler'thunkNameStatic (:siteIndex this)), (Type/getType ILookupThunk'iface))
                (.dup gen) ;; thunk, thunk
                (Expr'''emit (:target this), :Context'EXPRESSION, objx, gen) ;; thunk, thunk, target
                (.visitLineNumber gen, (:line this), (.mark gen))
                (.dupX2 gen) ;; target, thunk, thunk, target
                (.invokeInterface gen, (Type/getType ILookupThunk'iface), (Method/getMethod "Object get(Object)")) ;; target, thunk, result
                (.dupX2 gen) ;; result, target, thunk, result
                (.visitJumpInsn gen, Opcodes/IF_ACMPEQ, faultLabel) ;; result, target
                (.pop gen) ;; result
                (.goTo gen, endLabel)

                (.mark gen, faultLabel) ;; result, target
                (.swap gen) ;; target, result
                (.pop gen) ;; target
                (.dup gen) ;; target, target
                (.getStatic gen, (:objType objx), (Compiler'siteNameStatic (:siteIndex this)), (Type/getType KeywordLookupSite'iface)) ;; target, target, site
                (.swap gen) ;; target, site, target
                (.invokeInterface gen, (Type/getType ILookupSite'iface), (Method/getMethod "arbace.core.ILookupThunk fault(Object)")) ;; target, new-thunk
                (.dup gen) ;; target, new-thunk, new-thunk
                (.putStatic gen, (:objType objx), (Compiler'thunkNameStatic (:siteIndex this)), (Type/getType ILookupThunk'iface)) ;; target, new-thunk
                (.swap gen) ;; new-thunk, target
                (.invokeInterface gen, (Type/getType ILookupThunk'iface), (Method/getMethod "Object get(Object)")) ;; result

                (.mark gen, endLabel)
                (when (= context :Context'STATEMENT)
                    (.pop gen)
                )
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"KeywordInvokeExpr" this]
            (when (some? (:tag this)) (Interop'tagToClass (:tag this)))
        )
    )
)

(about #_"InstanceOfExpr"
    (defr InstanceOfExpr [])

    (defn #_"InstanceOfExpr" InstanceOfExpr'new [#_"Class" c, #_"Expr" expr]
        (merge (InstanceOfExpr'class.)
            (hash-map
                #_"Class" :c c
                #_"Expr" :expr expr
            )
        )
    )

    (defm InstanceOfExpr Expr
        (#_"Object" Expr'''eval [#_"InstanceOfExpr" this]
            (instance? (:c this) (Expr'''eval (:expr this)))
        )

        (#_"void" Expr'''emit [#_"InstanceOfExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (MaybePrimitive'''emitUnboxed this, context, objx, gen)
            (Interop'emitBoxReturn objx, gen, Boolean/TYPE)
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"InstanceOfExpr" this]
            Boolean/TYPE
        )
    )

    (defm InstanceOfExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"InstanceOfExpr" this]
            true
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"InstanceOfExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
            (.instanceOf gen, (Compiler'getType (:c this)))
            nil
        )
    )
)

(declare var-get)
(declare keyword)

(about #_"InvokeExpr"
    (defr InvokeExpr [])

    (defn #_"InvokeExpr" InvokeExpr'new [#_"int" line, #_"Symbol" tag, #_"Expr" fexpr, #_"vector" args, #_"boolean" tailPosition]
        (let [this
                (merge (InvokeExpr'class.)
                    (hash-map
                        #_"Expr" :fexpr fexpr
                        #_"Object" :tag (or tag (when (satisfies? VarExpr fexpr) (:tag fexpr)))
                        #_"vector" :args args
                        #_"int" :line line
                        #_"boolean" :tailPosition tailPosition

                        #_"boolean" :isProtocol false
                        #_"int" :siteIndex -1
                        #_"Class" :protocolOn nil
                        #_"java.lang.reflect.Method" :onMethod nil
                    )
                )]
            (when (satisfies? VarExpr fexpr) => this
                (let [#_"Var" fvar (:var fexpr) #_"Var" pvar (get (meta fvar) :protocol)]
                    (when (and (some? pvar) (bound? #'*protocol-callsites*)) => this
                        (let [this (assoc this :isProtocol true)
                              this (assoc this :siteIndex (Compiler'registerProtocolCallsite (:var fexpr)))
                              this (assoc this :protocolOn (Interop'maybeClass (get (var-get pvar) :on), false))]
                            (when (some? (:protocolOn this)) => this
                                (let [#_"map" mmap (get (var-get pvar) :method-map)
                                      #_"Keyword" mmapVal (get mmap (keyword (:sym fvar)))]
                                    (when (some? mmapVal) => (throw! (str "no method of interface: " (.getName (:protocolOn this)) " found for function: " (:sym fvar) " of protocol: " (:sym pvar)))
                                        (let [#_"String" mname (Compiler'munge (str (:sym mmapVal)))
                                              #_"vector" methods (Reflector'getMethods (:protocolOn this), (dec (count args)), mname, false)]
                                            (when (= (count methods) 1) => (throw! (str "no single method: " mname " of interface: " (.getName (:protocolOn this)) " found for function: " (:sym fvar) " of protocol: " (:sym pvar)))
                                                (assoc this :onMethod (nth methods 0))
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
    )

    (declare min)

    (defn #_"void" InvokeExpr''emitArgsAndCall [#_"InvokeExpr" this, #_"int" firstArgToEmit, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
        (loop-when-recur [#_"int" i firstArgToEmit] (< i (min Compiler'MAX_POSITIONAL_ARITY (count (:args this)))) [(inc i)]
            (Expr'''emit (nth (:args this) i), :Context'EXPRESSION, objx, gen)
        )
        (when (< Compiler'MAX_POSITIONAL_ARITY (count (:args this)))
            (let [#_"vector" restArgs
                    (loop-when-recur [restArgs [] #_"int" i Compiler'MAX_POSITIONAL_ARITY]
                                     (< i (count (:args this)))
                                     [(conj restArgs (nth (:args this) i)) (inc i)]
                                  => restArgs
                    )]
                (MethodExpr'emitArgsAsArray restArgs, objx, gen)
            )
        )
        (.visitLineNumber gen, (:line this), (.mark gen))

        (when (:tailPosition this)
            (IopMethod''emitClearThis *method*, gen)
        )

        (.invokeInterface gen, (Type/getType IFn'iface), (Method. "invoke", (Type/getType Object), (aget Compiler'ARG_TYPES (min (inc Compiler'MAX_POSITIONAL_ARITY) (count (:args this))))))
        nil
    )

    (defp Util)

    (defn #_"void" InvokeExpr''emitProto [#_"InvokeExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
        (let [#_"Label" onLabel (.newLabel gen) #_"Label" callLabel (.newLabel gen) #_"Label" endLabel (.newLabel gen)]
            (Expr'''emit (nth (:args this) 0), :Context'EXPRESSION, objx, gen)
            (.dup gen) ;; target, target
            (.invokeStatic gen, (Type/getType Util'iface), (Method/getMethod "Class classOf(Object)")) ;; target, class
            (.getStatic gen, (:objType objx), (Compiler'cachedClassName (:siteIndex this)), (Type/getType Class)) ;; target, class, cached-class
            (.visitJumpInsn gen, Opcodes/IF_ACMPEQ, callLabel) ;; target
            (when (some? (:protocolOn this))
                (.dup gen) ;; target, target
                (.instanceOf gen, (Type/getType (:protocolOn this)))
                (.ifZCmp gen, GeneratorAdapter/NE, onLabel)
            )
            (.dup gen) ;; target, target
            (.invokeStatic gen, (Type/getType Util'iface), (Method/getMethod "Class classOf(Object)")) ;; target, class
            (.putStatic gen, (:objType objx), (Compiler'cachedClassName (:siteIndex this)), (Type/getType Class)) ;; target
            (.mark gen, callLabel) ;; target
            (IopObject''emitVar objx, gen, (:var (:fexpr this)))
            (.invokeVirtual gen, (Type/getType Var'iface), (Method/getMethod "Object getRawRoot()")) ;; target, proto-fn
            (.swap gen)
            (InvokeExpr''emitArgsAndCall this, 1, context, objx, gen)
            (.goTo gen, endLabel)
            (.mark gen, onLabel) ;; target
            (when (some? (:protocolOn this))
                (.checkCast gen, (Type/getType (:protocolOn this)))
                (MethodExpr'emitTypedArgs objx, gen, (.getParameterTypes (:onMethod this)), (subvec (:args this) 1 (count (:args this))))
                (when (= context :Context'RETURN)
                    (IopMethod''emitClearLocals *method*, gen)
                )
                (let [#_"Method" m (Method. (.getName (:onMethod this)), (Type/getReturnType (:onMethod this)), (Type/getArgumentTypes (:onMethod this)))]
                    (.invokeInterface gen, (Type/getType (:protocolOn this)), m)
                    (Interop'emitBoxReturn objx, gen, (.getReturnType (:onMethod this)))
                )
            )
            (.mark gen, endLabel)
        )
        nil
    )

    (defm InvokeExpr Expr
        (#_"Object" Expr'''eval [#_"InvokeExpr" this]
            (let [#_"IFn" fn (Expr'''eval (:fexpr this))
                  #_"vector" v (loop-when-recur [v [] #_"int" i 0] (< i (count (:args this))) [(conj v (Expr'''eval (nth (:args this) i))) (inc i)] => v)]
                (IFn'''applyTo fn, (seq v))
            )
        )

        (#_"void" Expr'''emit [#_"InvokeExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (if (:isProtocol this)
                (do
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (InvokeExpr''emitProto this, context, objx, gen)
                )
                (do
                    (Expr'''emit (:fexpr this), :Context'EXPRESSION, objx, gen)
                    (.visitLineNumber gen, (:line this), (.mark gen))
                    (.checkCast gen, (Type/getType IFn'iface))
                    (InvokeExpr''emitArgsAndCall this, 0, context, objx, gen)
                )
            )
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"InvokeExpr" this]
            (when (some? (:tag this)) (Interop'tagToClass (:tag this)))
        )
    )

    (defn #_"Expr" InvokeExpr'parse [#_"Context" context, #_"seq" form]
        (let [#_"boolean" tailPosition (Compiler'inTailCall context) context (if (= context :Context'EVAL) context :Context'EXPRESSION)
              #_"Expr" fexpr (Compiler'analyze context, (first form))]
            (or
                (when (and (satisfies? VarExpr fexpr) (= (:var fexpr) #'instance?) (= (count form) 3))
                    (let-when [#_"Expr" sexpr (Compiler'analyze :Context'EXPRESSION, (second form))] (satisfies? ConstantExpr sexpr)
                        (let-when [#_"Object" val (Literal'''literal sexpr)] (class? val)
                            (InstanceOfExpr'new val, (Compiler'analyze context, (third form)))
                        )
                    )
                )

                (when (and (satisfies? KeywordExpr fexpr) (= (count form) 2) (bound? #'*keyword-callsites*))
                    (let [#_"Expr" target (Compiler'analyze context, (second form))]
                        (KeywordInvokeExpr'new *line*, (Compiler'tagOf form), fexpr, target)
                    )
                )

                (let [#_"vector" args
                        (loop-when-recur [args [] #_"seq" s (seq (next form))]
                                         (some? s)
                                         [(conj args (Compiler'analyze context, (first s))) (next s)]
                                      => args
                        )]
                    (InvokeExpr'new *line*, (Compiler'tagOf form), fexpr, args, tailPosition)
                )
            )
        )
    )
)

(about #_"LocalBinding"
    (defr LocalBinding [])

    (defn #_"LocalBinding" LocalBinding'new [#_"int" idx, #_"Symbol" sym, #_"Symbol" tag, #_"Expr" init, #_"boolean" isArg]
        (when (and (some? (Compiler'maybePrimitiveType init)) (some? tag))
            (throw! "can't type hint a local with a primitive initializer")
        )
        (merge (LocalBinding'class.)
            (hash-map
                #_"int" :uid (Compiler'nextUniqueId)
                #_"int" :idx idx
                #_"Symbol" :sym sym
                #_"Symbol" :tag tag
                #_"Expr" :init init
                #_"boolean" :isArg isArg

                #_"String" :name (Compiler'munge (:name sym))
                #_"boolean" :recurMistmatch false
            )
        )
    )

    #_memoize!
    (defn #_"Class" LocalBinding''getClass [#_"LocalBinding" this]
        (let [#_"Expr" e (:init this)]
            (if (some? (:tag this))
                (when-not (and (some? e) (Reflector'isPrimitive (Expr'''getClass e)) (not (satisfies? MaybePrimitive e)))
                    (Interop'tagToClass (:tag this))
                )
                (when (and (some? e) (not (and (Reflector'isPrimitive (Expr'''getClass e)) (not (satisfies? MaybePrimitive e)))))
                    (Expr'''getClass e)
                )
            )
        )
    )

    (defn #_"Class" LocalBinding''getPrimitiveType [#_"LocalBinding" this]
        (Compiler'maybePrimitiveType (:init this))
    )
)

(about #_"LocalBindingExpr"
    (defr LocalBindingExpr [])

    (defn #_"LocalBindingExpr" LocalBindingExpr'new [#_"LocalBinding" lb, #_"Symbol" tag]
        (when (or (nil? (LocalBinding''getPrimitiveType lb)) (nil? tag)) => (throw! "can't type hint a primitive local")
            (merge (LocalBindingExpr'class.)
                (hash-map
                    #_"LocalBinding" :lb lb
                    #_"Symbol" :tag tag
                )
            )
        )
    )

    (declare IopObject''emitLocal)

    (defm LocalBindingExpr Expr
        (#_"Object" Expr'''eval [#_"LocalBindingExpr" this]
            (throw! "can't eval locals")
        )

        (#_"void" Expr'''emit [#_"LocalBindingExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (when-not (= context :Context'STATEMENT)
                (IopObject''emitLocal objx, gen, (:lb this))
            )
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"LocalBindingExpr" this]
            (if (some? (:tag this)) (Interop'tagToClass (:tag this)) (LocalBinding''getClass (:lb this)))
        )
    )

    (declare IopObject''emitUnboxedLocal)

    (defm LocalBindingExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"LocalBindingExpr" this]
            (some? (LocalBinding''getPrimitiveType (:lb this)))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"LocalBindingExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''emitUnboxedLocal objx, gen, (:lb this))
            nil
        )
    )

    (declare IopObject''emitAssignLocal)

    (defm LocalBindingExpr Assignable
        (#_"Object" Assignable'''evalAssign [#_"LocalBindingExpr" this, #_"Expr" val]
            (throw! "can't eval locals")
        )

        (#_"void" Assignable'''emitAssign [#_"LocalBindingExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" val]
            (IopObject''emitAssignLocal objx, gen, (:lb this), val)
            (when-not (= context :Context'STATEMENT)
                (IopObject''emitLocal objx, gen, (:lb this))
            )
            nil
        )
    )
)

(about #_"MethodParamExpr"
    (defr MethodParamExpr [])

    (defn #_"MethodParamExpr" MethodParamExpr'new [#_"Class" c]
        (merge (MethodParamExpr'class.)
            (hash-map
                #_"Class" :c c
            )
        )
    )

    (defm MethodParamExpr Expr
        (#_"Object" Expr'''eval [#_"MethodParamExpr" this]
            (throw! "can't eval")
        )

        (#_"void" Expr'''emit [#_"MethodParamExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (throw! "can't emit")
        )

        (#_"Class" Expr'''getClass => :c)
    )

    (defm MethodParamExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"MethodParamExpr" this]
            (Reflector'isPrimitive (:c this))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"MethodParamExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (throw! "can't emit")
        )
    )
)

(about #_"FnMethod"
    (defr FnMethod [])

    (defn #_"FnMethod" FnMethod'new [#_"IopObject" objx, #_"IopMethod" parent]
        (merge (FnMethod'class.) (IopMethod'init objx, parent)
            (hash-map
                ;; localbinding->localbinding
                #_"vector" :reqParms nil
                #_"LocalBinding" :restParm nil
                #_"Type[]" :argTypes nil
                #_"Class[]" :argClasses nil
                #_"Class" :retClass nil
            )
        )
    )

    (defn #_"boolean" FnMethod''isVariadic [#_"FnMethod" this]
        (some? (:restParm this))
    )

    (defm FnMethod IopMethod
        (#_"int" IopMethod'''numParams [#_"FnMethod" this]
            (+ (count (:reqParms this)) (if (FnMethod''isVariadic this) 1 0))
        )

        (#_"String" IopMethod'''getMethodName [#_"FnMethod" this]
            (if (FnMethod''isVariadic this) "doInvoke" "invoke")
        )

        (#_"Type" IopMethod'''getReturnType [#_"FnMethod" this]
            (Type/getType Object)
        )

        (#_"Type[]" IopMethod'''getArgTypes [#_"FnMethod" this]
            (if (and (FnMethod''isVariadic this) (= (count (:reqParms this)) Compiler'MAX_POSITIONAL_ARITY))
                (let [#_"int" n (inc Compiler'MAX_POSITIONAL_ARITY) #_"Type[]" a (make-array Type n)]
                    (dotimes [#_"int" i n]
                        (aset! a i (Type/getType Object))
                    )
                    a
                )
                (aget Compiler'ARG_TYPES (IopMethod'''numParams this))
            )
        )

        (#_"void" IopMethod'''emit [#_"FnMethod" this, #_"IopObject" fn, #_"ClassVisitor" cv]
            (let [#_"Method" m (Method. (IopMethod'''getMethodName this), (IopMethod'''getReturnType this), (IopMethod'''getArgTypes this))
                  #_"GeneratorAdapter" gen (GeneratorAdapter. Opcodes/ACC_PUBLIC, m, nil, Compiler'EXCEPTION_TYPES, cv)]
                (.visitCode gen)
                (let [#_"Label" loopLabel (.mark gen)]
                    (.visitLineNumber gen, (:line this), loopLabel)
                    (binding [*loop-label* loopLabel, *method* this]
                        (Expr'''emit (:body this), :Context'RETURN, fn, gen)
                        (let [#_"Label" end (.mark gen)]
                            (.visitLocalVariable gen, "this", "Ljava/lang/Object;", nil, loopLabel, end, 0)
                            (loop-when-recur [#_"seq" lbs (seq (:argLocals this))] (some? lbs) [(next lbs)]
                                (let [#_"LocalBinding" lb (first lbs)]
                                    (.visitLocalVariable gen, (:name lb), "Ljava/lang/Object;", nil, loopLabel, end, (:idx lb))
                                )
                            )
                        )
                    )
                    (.returnValue gen)
                    (.endMethod gen)
                )
            )
            nil
        )
    )

    (declare into-array)

    (defn #_"FnMethod" FnMethod'parse [#_"IopObject" objx, #_"seq" form, #_"Object" retTag]
        ;; ([args] body...)
        (let [#_"vector" parms (first form) #_"seq" body (next form)
              #_"FnMethod" fm
                (-> (FnMethod'new objx, *method*)
                    (assoc :line *line*)
                )]
            ;; register as the current method and set up a new env frame
            (binding [*method*            fm
                      *local-env*         *local-env*
                      *last-local-num*    -1
                      *loop-locals*       nil
                      *in-return-context* true]
                (let [retTag (if (string? retTag) (symbol retTag) retTag)
                      retTag (when (and (symbol? retTag) (= (INamed'''getName retTag) "long")) retTag)
                      #_"Class" retClass
                        (let-when [retClass (Interop'tagClass (or (Compiler'tagOf parms) retTag))] (.isPrimitive retClass) => Object
                            (when-not (= retClass Long/TYPE) => retClass
                                (throw! "only long primitives are supported")
                            )
                        )
                      fm (assoc fm :retClass retClass)]
                    ;; register 'this' as local 0
                    (if (some? (:thisName objx))
                        (Compiler'registerLocal (symbol (:thisName objx)), nil, nil, false)
                        (Compiler'nextLocalNum)
                    )
                    (let [fm (assoc fm #_"vector" :argTypes [] #_"vector" :argClasses [] :reqParms [] :restParm nil :argLocals [])
                          fm (loop-when [fm fm #_"boolean" rest? false #_"int" i 0] (< i (count parms)) => fm
                                (when (symbol? (nth parms i)) => (throw! "fn params must be Symbols")
                                    (let [#_"Symbol" p (nth parms i)]
                                        (cond
                                            (some? (namespace p))
                                                (throw! (str "can't use qualified name as parameter: " p))
                                            (= p '&)
                                                (when-not rest? => (throw! "invalid parameter list")
                                                    (recur fm true (inc i))
                                                )
                                            :else
                                                (let [#_"Class" c (Compiler'primClass (Interop'tagClass (Compiler'tagOf p)))]
                                                    (when (and (.isPrimitive c) (not= c Long/TYPE))
                                                        (throw! (str "only long primitives are supported: " p))
                                                    )
                                                    (when (and rest? (some? (Compiler'tagOf p)))
                                                        (throw! "& arg cannot have type hint")
                                                    )
                                                    (let [c (if rest? ISeq'iface c)
                                                          fm (-> fm (update :argTypes conj (Type/getType c)) (update :argClasses conj c))
                                                          #_"LocalBinding" lb
                                                            (if (.isPrimitive c)
                                                                (Compiler'registerLocal p, nil, (MethodParamExpr'new c), true)
                                                                (Compiler'registerLocal p, (if rest? 'arbace.core.ISeq (Compiler'tagOf p)), nil, true)
                                                            )
                                                          fm (update fm :argLocals conj lb)]
                                                        (if-not rest?
                                                            (update fm :reqParms conj lb)
                                                            (assoc fm :restParm lb)
                                                        )
                                                    )
                                                )
                                        )
                                    )
                                )
                            )]
                        (when (< Compiler'MAX_POSITIONAL_ARITY (count (:reqParms fm)))
                            (throw! (str "can't specify more than " Compiler'MAX_POSITIONAL_ARITY " params"))
                        )
                        (set! *loop-locals* (:argLocals fm))
                        (-> fm
                            (update #_"Type[]" :argTypes #(into-array Type %))
                            (update #_"Class[]" :argClasses #(into-array Class %))
                            (assoc :body (IParser'''parse (BodyParser'new), :Context'RETURN, body))
                        )
                    )
                )
            )
        )
    )
)

(about #_"IopObject"
    (defn #_"IopObject" IopObject'init [#_"Object" tag]
        (hash-map
            #_"int" :uid (Compiler'nextUniqueId)
            #_"Object" :tag tag
            #_"String" :name nil
            #_"String" :internalName nil
            #_"String" :thisName nil
            #_"Type" :objType nil
            #_"vector" :closesExprs []
            #_"map" :fields nil
            #_"vector" :hintedFields []
            #_"map" :keywords {}
            #_"map" :vars {}
            #_"int" :line 0
            #_"vector" :constants nil
            #_"int" :altCtorDrops 0
            #_"vector" :keywordCallsites nil
            #_"vector" :protocolCallsites nil
            #_"boolean" :onceOnly false
            #_"map" :opts {}

            #_"Class" :compiledClass nil
        )
    )

    (defn #_"boolean" IopObject''isVolatile [#_"IopObject" this, #_"LocalBinding" lb]
        (and (contains? (:fields this) (:sym lb)) (get (meta (:sym lb)) :volatile))
    )

    (defn #_"boolean" IopObject''isMutable [#_"IopObject" this, #_"LocalBinding" lb]
        (or (IopObject''isVolatile this, lb) (and (contains? (:fields this) (:sym lb)) (get (meta (:sym lb)) :mutable)))
    )

    (defn #_"boolean" IopObject''isDeftype [#_"IopObject" this]
        (some? (:fields this))
    )

    (defn #_"Type" IopObject''constantType [#_"IopObject" this, #_"int" id]
        (let [#_"Object" o (nth (:constants this) id) #_"Class" c (Reflector'classOf o)]
            (or
                (when (and (some? c) (Modifier/isPublic (.getModifiers c)))
                    ;; can't emit derived fn types due to visibility
                    (cond
                        (.isAssignableFrom LazySeq'iface, c) (Type/getType ISeq'iface)
                        (= c Keyword'iface)                  (Type/getType Keyword'iface)
                        (.isAssignableFrom RestFn'iface, c)  (Type/getType IRestFn'iface)
                        (.isAssignableFrom AFn'iface, c)     (Type/getType AFn'iface)
                        (= c Var'iface)                      (Type/getType Var'iface)
                        (= c String)                         (Type/getType String)
                    )
                )
                (Type/getType Object)
            )
        )
    )

    (defn #_"Type[]" IopObject''ctorTypes [#_"IopObject" this]
        (let [#_"vector" v (if (IopObject'''supportsMeta this) [(Type/getType IPersistentMap'iface)] [])
              v (loop-when [v v #_"seq" s (vals (get *closes* (:uid this)))] (some? s) => v
                    (let [#_"Class" c (LocalBinding''getPrimitiveType (first s))]
                        (recur (conj v (if (some? c) (Type/getType c) (Type/getType Object))) (next s))
                    )
                )]
            (let [#_"Type[]" a (make-array Type (count v))]
                (dotimes [#_"int" i (count v)]
                    (aset! a i (nth v i))
                )
                a
            )
        )
    )

    (defn #_"Object" IopObject''doEval [#_"IopObject" this]
        (when-not (IopObject''isDeftype this)
            (.newInstance (:compiledClass this))
        )
    )

    (declare IopObject''emitValue)

    (defn- #_"void" IopObject''emitKeywordCallsites [#_"IopObject" this, #_"GeneratorAdapter" clinitgen]
        (dotimes [#_"int" i (count (:keywordCallsites this))]
            (let [#_"Keyword" k (nth (:keywordCallsites this) i)]
                (.newInstance clinitgen, (Type/getType KeywordLookupSite'iface))
                (.dup clinitgen)
                (IopObject''emitValue this, k, clinitgen)
                (.invokeConstructor clinitgen, (Type/getType KeywordLookupSite'iface), (Method/getMethod "void <init>(arbace.core.Keyword)"))
                (.dup clinitgen)
                (.putStatic clinitgen, (:objType this), (Compiler'siteNameStatic i), (Type/getType KeywordLookupSite'iface))
                (.putStatic clinitgen, (:objType this), (Compiler'thunkNameStatic i), (Type/getType ILookupThunk'iface))
            )
        )
        nil
    )

    (defn #_"void" IopObject''emitObjectArray [#_"IopObject" this, #_"Object[]" a, #_"GeneratorAdapter" gen]
        (.push gen, (count a))
        (.newArray gen, (Type/getType Object))
        (dotimes [#_"int" i (count a)]
            (.dup gen)
            (.push gen, i)
            (IopObject''emitValue this, (aget a i), gen)
            (.arrayStore gen, (Type/getType Object))
        )
        nil
    )

    (defn #_"void" IopObject''emitConstants [#_"IopObject" this, #_"GeneratorAdapter" clinitgen]
        (dotimes [#_"int" i (count (:constants this))]
            (when (contains? *used-constants* i)
                (IopObject''emitValue this, (nth (:constants this) i), clinitgen)
                (.checkCast clinitgen, (IopObject''constantType this, i))
                (.putStatic clinitgen, (:objType this), (Compiler'constantName i), (IopObject''constantType this, i))
            )
        )
        nil
    )

    (defn #_"void" IopObject''emitClearCloses [#_"IopObject" this, #_"GeneratorAdapter" gen]
        nil
    )

    (defn #_"void" IopObject''emitLetFnInits [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"IopObject" objx, #_"IPersistentSet" letFnLocals]
        ;; objx arg is enclosing objx, not this
        (.checkCast gen, (:objType this))

        (loop-when-recur [#_"seq" s (vals (get *closes* (:uid this)))] (some? s) [(next s)]
            (let [#_"LocalBinding" lb (first s)]
                (when (contains? letFnLocals lb)
                    (let [#_"Class" primc (LocalBinding''getPrimitiveType lb)]
                        (.dup gen)
                        (if (some? primc)
                            (do
                                (IopObject''emitUnboxedLocal objx, gen, lb)
                                (.putField gen, (:objType this), (:name lb), (Type/getType primc))
                            )
                            (do
                                (IopObject''emitLocal objx, gen, lb)
                                (.putField gen, (:objType this), (:name lb), (Type/getType Object))
                            )
                        )
                    )
                )
            )
        )
        (.pop gen)
        nil
    )

    (defn #_"void" IopObject''doEmit [#_"IopObject" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
        ;; emitting a Fn means constructing an instance, feeding closed-overs from enclosing scope, if any
        ;; objx arg is enclosing objx, not this
        (when-not (IopObject''isDeftype this) => (.visitInsn gen, Opcodes/ACONST_NULL)
            (.newInstance gen, (:objType this))
            (.dup gen)
            (when (IopObject'''supportsMeta this)
                (.visitInsn gen, Opcodes/ACONST_NULL)
            )
            (loop-when-recur [#_"seq" s (seq (:closesExprs this))] (some? s) [(next s)]
                (let [#_"LocalBindingExpr" lbe (first s) #_"LocalBinding" lb (:lb lbe)]
                    (if (some? (LocalBinding''getPrimitiveType lb))
                        (IopObject''emitUnboxedLocal objx, gen, lb)
                        (IopObject''emitLocal objx, gen, lb)
                    )
                )
            )
            (.invokeConstructor gen, (:objType this), (Method. "<init>", Type/VOID_TYPE, (IopObject''ctorTypes this)))
        )
        (when (= context :Context'STATEMENT)
            (.pop gen)
        )
        nil
    )

    (defn #_"void" IopObject''emitAssignLocal [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"LocalBinding" lb, #_"Expr" val]
        (when (IopObject''isMutable this, lb) => (throw! (str "cannot assign to non-mutable: " (:name lb)))
            (let [#_"Class" primc (LocalBinding''getPrimitiveType lb)]
                (.loadThis gen)
                (if (some? primc)
                    (do
                        (when-not (and (satisfies? MaybePrimitive val) (MaybePrimitive'''canEmitPrimitive val))
                            (throw! (str "must assign primitive to primitive mutable: " (:name lb)))
                        )
                        (MaybePrimitive'''emitUnboxed val, :Context'EXPRESSION, this, gen)
                        (.putField gen, (:objType this), (:name lb), (Type/getType primc))
                    )
                    (do
                        (Expr'''emit val, :Context'EXPRESSION, this, gen)
                        (.putField gen, (:objType this), (:name lb), (Type/getType Object))
                    )
                )
            )
        )
        nil
    )

    (defn- #_"void" IopObject''emitLocal [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"LocalBinding" lb]
        (let [#_"Class" primc (LocalBinding''getPrimitiveType lb)]
            (if (contains? (get *closes* (:uid this)) (:uid lb))
                (do
                    (.loadThis gen)
                    (.getField gen, (:objType this), (:name lb), (Type/getType (or primc Object)))
                )
                (if (:isArg lb)
                    (.loadArg gen, (dec (:idx lb)))
                    (.visitVarInsn gen, (.getOpcode (Type/getType (or primc Object)), Opcodes/ILOAD), (:idx lb))
                )
            )
            (when (some? primc)
                (Interop'emitBoxReturn this, gen, primc)
            )
        )
        nil
    )

    (defn- #_"void" IopObject''emitUnboxedLocal [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"LocalBinding" lb]
        (let [#_"Class" primc (LocalBinding''getPrimitiveType lb)]
            (if (contains? (get *closes* (:uid this)) (:uid lb))
                (do
                    (.loadThis gen)
                    (.getField gen, (:objType this), (:name lb), (Type/getType primc))
                )
                (if (:isArg lb)
                    (.loadArg gen, (dec (:idx lb)))
                    (.visitVarInsn gen, (.getOpcode (Type/getType primc), Opcodes/ILOAD), (:idx lb))
                )
            )
        )
        nil
    )

    (defn #_"void" IopObject''emitVar [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"Var" var]
        (IopObject''emitConstant this, gen, (get (:vars this) var))
        nil
    )

    (defn #_"void" IopObject''emitVarValue [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"Var" v]
        (IopObject''emitConstant this, gen, (get (:vars this) v))
        (.invokeVirtual gen, (Type/getType Var'iface), (Method/getMethod "Object get()"))
        nil
    )

    (defn #_"void" IopObject''emitKeyword [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"Keyword" k]
        (IopObject''emitConstant this, gen, (get (:keywords this) k))
        nil
    )

    (defn #_"void" IopObject''emitConstant [#_"IopObject" this, #_"GeneratorAdapter" gen, #_"int" id]
        (update! *used-constants* conj id)
        (.getStatic gen, (:objType this), (Compiler'constantName id), (IopObject''constantType this, id))
        nil
    )

    (declare array-map)
    (declare RT'seqToArray)
    (declare RT'printString)

    (defn #_"void" IopObject''emitValue [#_"IopObject" this, #_"Object" value, #_"GeneratorAdapter" gen]
        (let [#_"boolean" partial?
                (cond (nil? value)
                    (do
                        (.visitInsn gen, Opcodes/ACONST_NULL)
                        true
                    )
                    (string? value)
                    (do
                        (.push gen, value)
                        true
                    )
                    (boolean? value)
                    (do
                        (.getStatic gen, (Type/getType Boolean), (if (.booleanValue #_"Boolean" value) "TRUE" "FALSE"), (Type/getType Boolean))
                        true
                    )
                    (instance? Integer value)
                    (do
                        (.push gen, (.intValue #_"Integer" value))
                        (.invokeStatic gen, (Type/getType Integer), (Method/getMethod "Integer valueOf(int)"))
                        true
                    )
                    (instance? Long value)
                    (do
                        (.push gen, (.longValue #_"Long" value))
                        (.invokeStatic gen, (Type/getType Long), (Method/getMethod "Long valueOf(long)"))
                        true
                    )
                    (char? value)
                    (do
                        (.push gen, (.charValue #_"Character" value))
                        (.invokeStatic gen, (Type/getType Character), (Method/getMethod "Character valueOf(char)"))
                        true
                    )
                    (class? value)
                    (do
                        (if (.isPrimitive value)
                            (let [#_"Type" t
                                    (condp = value
                                        Integer/TYPE   (Type/getType Integer)
                                        Long/TYPE      (Type/getType Long)
                                        Boolean/TYPE   (Type/getType Boolean)
                                        Byte/TYPE      (Type/getType Byte)
                                        Character/TYPE (Type/getType Character)
                                        (throw! (str "can't embed unknown primitive in code: " value))
                                    )]
                                (.getStatic gen, t, "TYPE", (Type/getType Class))
                            )
                            (do
                                (.push gen, (Compiler'destubClassName (.getName value)))
                                (.invokeStatic gen, (Type/getType Loader'iface), (Method/getMethod "Class classForName(String)"))
                            )
                        )
                        true
                    )
                    (symbol? value)
                    (do
                        (.push gen, (:ns value))
                        (.push gen, (:name value))
                        (.invokeStatic gen, (Type/getType Symbol'iface), (Method/getMethod "arbace.core.Symbol intern(String, String)"))
                        true
                    )
                    (keyword? value)
                    (do
                        (.push gen, (:ns (:sym value)))
                        (.push gen, (:name (:sym value)))
                        (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.Keyword keyword(String, String)"))
                        true
                    )
                    (var? value)
                    (do
                        (.push gen, (str (:name (:ns value))))
                        (.push gen, (str (:sym value)))
                        (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.Var var(String, String)"))
                        true
                    )
                    (type? value)
                    (let [#_"Method" ctor (Method. "<init>", (Type/getConstructorDescriptor (aget (.getConstructors (class value)) 0)))]
                        (.newInstance gen, (Type/getType (class value)))
                        (.dup gen)
                        (let [#_"vector" fields (Reflector'invokeStaticMethod (class value), "getBasis", (object-array 0))]
                            (loop-when-recur [#_"seq" s (seq fields)] (some? s) [(next s)]
                                (let [#_"Symbol" field (first s)]
                                    (IopObject''emitValue this, (Reflector'getInstanceField value, (Compiler'munge (:name field))), gen)
                                    (let-when [#_"Class" c (Interop'tagClass (Compiler'tagOf field))] (.isPrimitive c)
                                        (let [#_"Type" b (Type/getType (Compiler'boxClass c))]
                                            (.invokeVirtual gen, b, (Method. (str (.getName c) "Value"), (str "()" (.getDescriptor (Type/getType c)))))
                                        )
                                    )
                                )
                            )
                            (.invokeConstructor gen, (Type/getType (class value)), ctor)
                        )
                        true
                    )
                    (record? value)
                    (let [#_"Class" c (class value)]
                        (IopObject''emitValue this, (apply array-map value), gen)
                        (.invokeStatic gen, (Compiler'getType c), (Method/getMethod (str (.getName c) " create(arbace.core.IPersistentMap)")))
                        true
                    )
                    (map? value)
                    (let [#_"vector" v
                            (loop-when [v [] #_"seq" s (seq value)] (some? s) => v
                                (let [#_"IMapEntry" e (first s)]
                                    (recur (conj v (key e) (val e)) (next s))
                                )
                            )]
                        (IopObject''emitObjectArray this, (to-array v), gen)
                        (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.IPersistentMap map(Object[])"))
                        true
                    )
                    (vector? value)
                    (let [#_"vector" args value]
                        (if (<= (count args) Tuple'MAX_SIZE)
                            (do
                                (dotimes [#_"int" i (count args)]
                                    (IopObject''emitValue this, (nth args i), gen)
                                )
                                (.invokeStatic gen, (Type/getType Tuple'iface), (nth Compiler'createTupleMethods (count args)))
                            )
                            (do
                                (IopObject''emitObjectArray this, (to-array args), gen)
                                (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "arbace.core.IPersistentVector vector(Object[])"))
                            )
                        )
                        true
                    )
                    (satisfies? PersistentHashSet value)
                    (let [#_"seq" vs (seq value)]
                        (if (nil? vs)
                            (do
                                (.getStatic gen, (Type/getType PersistentHashSet'iface), "EMPTY", (Type/getType PersistentHashSet'iface))
                            )
                            (do
                                (IopObject''emitObjectArray this, (RT'seqToArray vs), gen)
                                (.invokeStatic gen, (Type/getType PersistentHashSet'iface), (Method/getMethod "arbace.core.PersistentHashSet create(Object[])"))
                            )
                        )
                        true
                    )
                    (or (seq? value) (list? value))
                    (let [#_"seq" vs (seq value)]
                        (IopObject''emitObjectArray this, (RT'seqToArray vs), gen)
                        (.invokeStatic gen, (Type/getType PersistentList'iface), (Method/getMethod "arbace.core.IPersistentList create(Object[])"))
                        true
                    )
                    (instance? Pattern value)
                    (do
                        (IopObject''emitValue this, (str value), gen)
                        (.invokeStatic gen, (Type/getType Pattern), (Method/getMethod "java.util.regex.Pattern compile(String)"))
                        true
                    )
                    :else
                    (let [#_"String" cs
                            (try
                                (RT'printString value)
                                (catch Exception _
                                    (throw! (str "can't embed object in code: " value))
                                )
                            )]
                        (when (zero? (count cs))
                            (throw! (str "can't embed unreadable object in code: " value))
                        )
                        (when (.startsWith cs, "#<")
                            (throw! (str "can't embed unreadable object in code: " cs))
                        )
                        (.push gen, cs)
                        (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "Object readString(String)"))
                        false
                    )
                )]
            (when partial?
                (when (and (satisfies? IObj value) (pos? (count (meta value))))
                    (.checkCast gen, (Type/getType IObj'iface))
                    (IopObject''emitValue this, (meta value), gen)
                    (.checkCast gen, (Type/getType IPersistentMap'iface))
                    (.invokeInterface gen, (Type/getType IObj'iface), (Method/getMethod "arbace.core.IObj withMeta(arbace.core.IPersistentMap)"))
                )
            )
        )
        nil
    )

    (defn #_"IopObject" IopObject''compile [#_"IopObject" this, #_"String" superName, #_"String[]" interfaceNames, #_"boolean" _oneTimeUse]
        (binding [*used-constants* #{}]
            (let [#_"ClassWriter" cw (ClassWriter. ClassWriter/COMPUTE_MAXS) #_"ClassVisitor" cv cw]
                (.visit cv, Opcodes/V1_5, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER Opcodes/ACC_FINAL), (:internalName this), nil, superName, interfaceNames)
                (when (IopObject'''supportsMeta this)
                    (.visitField cv, Opcodes/ACC_FINAL, "__meta", (.getDescriptor (Type/getType IPersistentMap'iface)), nil, nil)
                )
                ;; instance fields for closed-overs
                (loop-when-recur [#_"seq" s (vals (get *closes* (:uid this)))] (some? s) [(next s)]
                    (let [#_"LocalBinding" lb (first s)
                          #_"String" fd
                            (if (some? (LocalBinding''getPrimitiveType lb))
                                (.getDescriptor (Type/getType (LocalBinding''getPrimitiveType lb)))
                                ;; todo - when closed-overs are fields, use more specific types here and in ctor and emitLocal?
                                (.getDescriptor (Type/getType Object))
                            )]
                        (if (IopObject''isDeftype this)
                            (let [#_"int" access
                                    (cond
                                        (IopObject''isVolatile this, lb) Opcodes/ACC_VOLATILE
                                        (IopObject''isMutable this, lb) 0
                                        :else (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
                                    )]
                                (.visitField cv, access, (:name lb), fd, nil, nil)
                            )
                            ;; todo - only enable this non-private+writability for letfns where we need it
                            (let [#_"int" access
                                    (if (some? (LocalBinding''getPrimitiveType lb))
                                        (if (IopObject''isVolatile this, lb) Opcodes/ACC_VOLATILE 0)
                                        0
                                    )]
                                (.visitField cv, access, (:name lb), fd, nil, nil)
                            )
                        )
                    )
                )

                ;; static fields for callsites and thunks
                (dotimes [#_"int" i (count (:protocolCallsites this))]
                    (.visitField cv, (+ Opcodes/ACC_PRIVATE Opcodes/ACC_STATIC), (Compiler'cachedClassName i), (.getDescriptor (Type/getType Class)), nil, nil)
                )

                ;; ctor that takes closed-overs and inits base + fields
                (let [#_"Method" m (Method. "<init>", Type/VOID_TYPE, (IopObject''ctorTypes this))
                      #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, m, nil, nil, cv)
                      #_"Label" start (.newLabel ctorgen) #_"Label" end (.newLabel ctorgen)]
                    (.visitCode ctorgen)
                    (.visitLineNumber ctorgen, (:line this), (.mark ctorgen))
                    (.visitLabel ctorgen, start)
                    (.loadThis ctorgen)
                    (.invokeConstructor ctorgen, (Type/getObjectType superName), (Method/getMethod "void <init>()"))

                    (when (IopObject'''supportsMeta this)
                        (.loadThis ctorgen)
                        (.visitVarInsn ctorgen, (.getOpcode (Type/getType IPersistentMap'iface), Opcodes/ILOAD), 1)
                        (.putField ctorgen, (:objType this), "__meta", (Type/getType IPersistentMap'iface))
                    )

                    (let [[this #_"int" a]
                            (loop-when [this this a (if (IopObject'''supportsMeta this) 2 1) #_"seq" s (vals (get *closes* (:uid this)))] (some? s) => [this a]
                                (let [#_"LocalBinding" lb (first s)]
                                    (.loadThis ctorgen)
                                    (let [#_"Class" primc (LocalBinding''getPrimitiveType lb)
                                          a (if (some? primc)
                                                (do
                                                    (.visitVarInsn ctorgen, (.getOpcode (Type/getType primc), Opcodes/ILOAD), a)
                                                    (.putField ctorgen, (:objType this), (:name lb), (Type/getType primc))
                                                    (if (= primc Long/TYPE) (inc a) a)
                                                )
                                                (do
                                                    (.visitVarInsn ctorgen, (.getOpcode (Type/getType Object), Opcodes/ILOAD), a)
                                                    (.putField ctorgen, (:objType this), (:name lb), (Type/getType Object))
                                                    a
                                                )
                                            )]
                                        (recur (update this :closesExprs conj (LocalBindingExpr'new lb, nil)) (inc a) (next s))
                                    )
                                )
                            )]

                        (.visitLabel ctorgen, end)
                        (.returnValue ctorgen)
                        (.endMethod ctorgen)

                        (when (pos? (:altCtorDrops this))
                            (let [#_"Type[]" ctorTypes (IopObject''ctorTypes this)]

                                ;; ctor that takes closed-overs and inits base + fields
                                (let [#_"Type[]" altCtorTypes (make-array Type (- (count ctorTypes) (:altCtorDrops this)))
                                      _ (dotimes [#_"int" i (count altCtorTypes)]
                                            (aset! altCtorTypes i (aget ctorTypes i))
                                        )
                                      #_"Method" alt (Method. "<init>", Type/VOID_TYPE, altCtorTypes)
                                      #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, alt, nil, nil, cv)]
                                    (.visitCode ctorgen)
                                    (.loadThis ctorgen)
                                    (.loadArgs ctorgen)

                                    (.visitInsn ctorgen, Opcodes/ACONST_NULL) ;; __meta
                                    (.visitInsn ctorgen, Opcodes/ACONST_NULL) ;; __extmap
                                    (.visitInsn ctorgen, Opcodes/ICONST_0) ;; __hash

                                    (.invokeConstructor ctorgen, (:objType this), (Method. "<init>", Type/VOID_TYPE, ctorTypes))

                                    (.returnValue ctorgen)
                                    (.endMethod ctorgen)
                                )

                                ;; alt ctor w/o __hash
                                (let [#_"Type[]" altCtorTypes (make-array Type (- (count ctorTypes) 2))
                                      _ (dotimes [#_"int" i (count altCtorTypes)]
                                            (aset! altCtorTypes i (aget ctorTypes i))
                                        )
                                      #_"Method" alt (Method. "<init>", Type/VOID_TYPE, altCtorTypes)
                                      #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, alt, nil, nil, cv)]
                                    (.visitCode ctorgen)
                                    (.loadThis ctorgen)
                                    (.loadArgs ctorgen)

                                    (.visitInsn ctorgen, Opcodes/ICONST_0) ;; __hash

                                    (.invokeConstructor ctorgen, (:objType this), (Method. "<init>", Type/VOID_TYPE, ctorTypes))

                                    (.returnValue ctorgen)
                                    (.endMethod ctorgen)
                                )
                            )
                        )

                        (when (IopObject'''supportsMeta this)
                            (let [#_"Type[]" ctorTypes (IopObject''ctorTypes this)]

                                ;; ctor that takes closed-overs but not meta
                                (let [#_"Type[]" noMetaCtorTypes (make-array Type (dec (count ctorTypes)))
                                      _ (loop-when-recur [#_"int" i 1] (< i (count ctorTypes)) [(inc i)]
                                            (aset! noMetaCtorTypes (dec i) (aget ctorTypes i))
                                        )
                                      #_"Method" alt (Method. "<init>", Type/VOID_TYPE, noMetaCtorTypes)
                                      #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, alt, nil, nil, cv)]
                                    (.visitCode ctorgen)
                                    (.loadThis ctorgen)
                                    (.visitInsn ctorgen, Opcodes/ACONST_NULL) ;; nil meta
                                    (.loadArgs ctorgen)
                                    (.invokeConstructor ctorgen, (:objType this), (Method. "<init>", Type/VOID_TYPE, ctorTypes))
                                    (.returnValue ctorgen)
                                    (.endMethod ctorgen)
                                )

                                ;; meta()
                                (let [#_"Method" meth (Method/getMethod "arbace.core.IPersistentMap meta()")
                                      #_"GeneratorAdapter" gen (GeneratorAdapter. Opcodes/ACC_PUBLIC, meth, nil, nil, cv)]
                                    (.visitCode gen)
                                    (.loadThis gen)
                                    (.getField gen, (:objType this), "__meta", (Type/getType IPersistentMap'iface))
                                    (.returnValue gen)
                                    (.endMethod gen)
                                )

                                ;; withMeta()
                                (let [#_"Method" meth (Method/getMethod "arbace.core.IObj withMeta(arbace.core.IPersistentMap)")
                                      #_"GeneratorAdapter" gen (GeneratorAdapter. Opcodes/ACC_PUBLIC, meth, nil, nil, cv)]
                                    (.visitCode gen)
                                    (.newInstance gen, (:objType this))
                                    (.dup gen)
                                    (.loadArg gen, 0)
                                    (loop-when-recur [a a #_"seq" s (vals (get *closes* (:uid this)))] (some? s) [(inc a) (next s)]
                                        (let [#_"LocalBinding" lb (first s)]
                                            (.loadThis gen)
                                            (let [#_"Class" primc (LocalBinding''getPrimitiveType lb)]
                                                (.getField gen, (:objType this), (:name lb), (if (some? primc) (Type/getType primc) (Type/getType Object)))
                                            )
                                        )
                                    )
                                    (.invokeConstructor gen, (:objType this), (Method. "<init>", Type/VOID_TYPE, ctorTypes))
                                    (.returnValue gen)
                                    (.endMethod gen)
                                )
                            )
                        )

                        (IopObject'''emitStatics this, cv)
                        (IopObject'''emitMethods this, cv)

                        ;; static fields for constants
                        (dotimes [#_"int" i (count (:constants this))]
                            (when (contains? *used-constants* i)
                                (.visitField cv, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC), (Compiler'constantName i), (.getDescriptor (IopObject''constantType this, i)), nil, nil)
                            )
                        )

                        ;; static fields for lookup sites
                        (dotimes [#_"int" i (count (:keywordCallsites this))]
                            (.visitField cv, (+ Opcodes/ACC_FINAL Opcodes/ACC_STATIC), (Compiler'siteNameStatic i), (.getDescriptor (Type/getType KeywordLookupSite'iface)), nil, nil)
                            (.visitField cv, Opcodes/ACC_STATIC, (Compiler'thunkNameStatic i), (.getDescriptor (Type/getType ILookupThunk'iface)), nil, nil)
                        )

                        ;; static init for constants, keywords and vars
                        (let [#_"GeneratorAdapter" clinitgen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), (Method/getMethod "void <clinit> ()"), nil, nil, cv)]
                            (.visitCode clinitgen)
                            (.visitLineNumber clinitgen, (:line this), (.mark clinitgen))

                            (IopObject''emitConstants this, clinitgen)
                            (IopObject''emitKeywordCallsites this, clinitgen)

                            (.returnValue clinitgen)
                            (.endMethod clinitgen)
                        )

                        ;; end of class
                        (.visitEnd cv)

                        (assoc this :compiledClass (Loader''defineClass *class-loader*, (:name this), (.toByteArray cw)))
                    )
                )
            )
        )
    )

    (defn #_"String" IopObject'trimGenID [#_"String" name]
        (let [#_"int" i (.lastIndexOf name, "__")]
            (if (= i -1) name (.substring name, 0, i))
        )
    )
)

(about #_"FnExpr"
    (defr FnExpr [])

    (defn #_"FnExpr" FnExpr'new [#_"Object" tag]
        (merge (FnExpr'class.) (IopObject'init tag)
            (hash-map
                ;; if there is a variadic overload (there can only be one) it is stored here
                #_"FnMethod" :variadicMethod nil
                #_"IPersistentCollection" :methods nil
                #_"boolean" :hasMeta false
                #_"boolean" :hasEnclosingMethod false
            )
        )
    )

    (defn #_"boolean" FnExpr''isVariadic [#_"FnExpr" this]
        (some? (:variadicMethod this))
    )

    (defm FnExpr Expr
        (#_"Object" Expr'''eval => IopObject''doEval)

        (#_"void" Expr'''emit [#_"FnExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''doEmit this, context, objx, gen)
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"FnExpr" this]
            (if (some? (:tag this)) (Interop'tagToClass (:tag this)) Fn'iface)
        )
    )

    (defm FnExpr IopObject
        (#_"boolean" IopObject'''supportsMeta => :hasMeta)

        (#_"void" IopObject'''emitStatics [#_"FnExpr" this, #_"ClassVisitor" gen]
            nil
        )

        (#_"void" IopObject'''emitMethods [#_"FnExpr" this, #_"ClassVisitor" cv]
            ;; override of invoke/doInvoke for each method
            (loop-when-recur [#_"seq" s (seq (:methods this))] (some? s) [(next s)]
                (IopMethod'''emit (first s), this, cv)
            )

            (when (FnExpr''isVariadic this)
                (let [#_"GeneratorAdapter" gen (GeneratorAdapter. Opcodes/ACC_PUBLIC, (Method/getMethod "int requiredArity()"), nil, nil, cv)]
                    (.visitCode gen)
                    (.push gen, (count (:reqParms (:variadicMethod this))))
                    (.returnValue gen)
                    (.endMethod gen)
                )
            )
            nil
        )
    )

    (defn #_"Expr" FnExpr'parse [#_"Context" context, #_"seq" form, #_"String" name]
        (let [#_"meta" fmeta (meta form)
              #_"IopMethod" owner *method*
              #_"FnExpr" fn
                (-> (FnExpr'new (Compiler'tagOf form))
                    (assoc :hasEnclosingMethod (some? owner) :line *line*)
                )
              fn (when (some? (meta (first form))) => fn
                    (assoc fn :onceOnly (boolean (get (meta (first form)) :once)))
                )
              #_"String" basename (if (some? owner) (:name (:objx owner)) (Compiler'munge (:name (:name *ns*))))
              [#_"Symbol" nm name]
                (if (symbol? (second form))
                    (let [nm (second form)]
                        [nm (str (:name nm) "__" (next-id!))]
                    )
                    (cond
                        (nil? name)   [nil (str "fn__" (next-id!))]
                        (some? owner) [nil (str name "__"(next-id!))]
                        :else         [nil name]
                    )
                )
              fn (assoc fn :name (str basename "$" (.replace (Compiler'munge name), ".", "_DOT_")))
              fn (assoc fn :internalName (.replace (:name fn), \., \/))
              fn (assoc fn :objType (Type/getObjectType (:internalName fn)))
              #_"Object" rettag (get fmeta :rettag)
              fn
                (binding [*constants*          []
                          *constant-ids*       (IdentityHashMap.)
                          *keywords*           {}
                          *vars*               {}
                          *keyword-callsites*  []
                          *protocol-callsites* []
                          *no-recur*           false]
                    ;; arglist might be preceded by symbol naming this fn
                    (let [[fn form]
                            (when (some? nm) => [fn form]
                                [(assoc fn :thisName (:name nm)) (cons 'fn* (next (next form)))]
                            )
                          ;; now (fn [args] body...) or (fn ([args] body...) ([args2] body2...) ...)
                          ;; turn former into latter
                          form
                            (when (vector? (second form)) => form
                                (list 'fn* (next form))
                            )
                          #_"FnMethod[]" a (make-array #_"FnMethod" Object (inc Compiler'MAX_POSITIONAL_ARITY))
                          #_"FnMethod" variadic
                            (loop-when [variadic nil #_"seq" s (next form)] (some? s) => variadic
                                (let [#_"FnMethod" f (FnMethod'parse fn, (first s), rettag)
                                      variadic
                                        (if (FnMethod''isVariadic f)
                                            (when (nil? variadic) => (throw! "can't have more than 1 variadic overload")
                                                f
                                            )
                                            (let [#_"int" n (count (:reqParms f))]
                                                (when (nil? (aget a n)) => (throw! "can't have 2 overloads with same arity")
                                                    (aset! a n f)
                                                    variadic
                                                )
                                            )
                                        )]
                                    (recur variadic (next s))
                                )
                            )]
                        (when (some? variadic)
                            (loop-when-recur [#_"int" i (inc (count (:reqParms variadic)))] (<= i Compiler'MAX_POSITIONAL_ARITY) [(inc i)]
                                (when (some? (aget a i))
                                    (throw! "can't have fixed arity function with more params than variadic function")
                                )
                            )
                        )
                        (let [#_"IPersistentCollection" methods
                                (loop-when-recur [methods nil #_"int" i 0]
                                                 (< i (count a))
                                                 [(if (some? (aget a i)) (conj methods (aget a i)) methods) (inc i)]
                                              => (if (some? variadic) (conj methods variadic) methods)
                                )]
                            (assoc fn
                                :methods methods
                                :variadicMethod variadic
                                :keywords *keywords*
                                :vars *vars*
                                :constants *constants*
                                :keywordCallsites *keyword-callsites*
                                :protocolCallsites *protocol-callsites*
                            )
                        )
                    )
                )
              fmeta
                (when (some? fmeta)
                    (dissoc fmeta :line :column :rettag)
                )
              fn (assoc fn :hasMeta (pos? (count fmeta)))
              fn (IopObject''compile fn, (if (FnExpr''isVariadic fn) "arbace/core/RestFn" "arbace/core/Fn"), nil, (:onceOnly fn))]
            (when (IopObject'''supportsMeta fn) => fn
                (MetaExpr'new fn, (MapExpr'parse (if (= context :Context'EVAL) context :Context'EXPRESSION), fmeta))
            )
        )
    )

    (defn #_"void" FnExpr''emitForDefn [#_"FnExpr" this, #_"IopObject" objx, #_"GeneratorAdapter" gen]
        (Expr'''emit this, :Context'EXPRESSION, objx, gen)
        nil
    )
)

(about #_"DefExpr"
    (defr DefExpr [])

    (defn #_"DefExpr" DefExpr'new [#_"int" line, #_"Var" var, #_"Expr" init, #_"Expr" meta, #_"boolean" initProvided, #_"boolean" shadowsCoreMapping]
        (merge (DefExpr'class.)
            (hash-map
                #_"int" :line line
                #_"Var" :var var
                #_"Expr" :init init
                #_"Expr" :meta meta
                #_"boolean" :initProvided initProvided
                #_"boolean" :shadowsCoreMapping shadowsCoreMapping
            )
        )
    )

    (defn- #_"boolean" DefExpr''includesExplicitMetadata [#_"DefExpr" this, #_"MapExpr" expr]
        (loop-when [#_"int" i 0] (< i (count (:keyvals expr))) => false
            (recur-when (any = (:k (nth (:keyvals expr) i)) :declared :line :column) [(+ i 2)] => true)
        )
    )

    (declare Var''bindRoot)

    (defm DefExpr Expr
        (#_"Object" Expr'''eval [#_"DefExpr" this]
            (when (:initProvided this)
                (Var''bindRoot (:var this), (Expr'''eval (:init this)))
            )
            (when (some? (:meta this))
                (reset-meta! (:var this) (Expr'''eval (:meta this)))
            )
            (:var this)
        )

        (#_"void" Expr'''emit [#_"DefExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''emitVar objx, gen, (:var this))
            (when (:shadowsCoreMapping this)
                (.dup gen)
                (.getField gen, (Type/getType Var'iface), "ns", (Type/getType Namespace'iface))
                (.swap gen)
                (.dup gen)
                (.getField gen, (Type/getType Var'iface), "sym", (Type/getType Symbol'iface))
                (.swap gen)
                (.invokeVirtual gen, (Type/getType Namespace'iface), (Method/getMethod "arbace.core.Var refer(arbace.core.Symbol, arbace.core.Var)"))
            )
            (when (some? (:meta this))
                (.dup gen)
                (Expr'''emit (:meta this), :Context'EXPRESSION, objx, gen)
                (.checkCast gen, (Type/getType IPersistentMap'iface))
                (.invokeVirtual gen, (Type/getType Var'iface), (Method/getMethod "void setMeta(arbace.core.IPersistentMap)"))
            )
            (when (:initProvided this)
                (.dup gen)
                (if (satisfies? FnExpr (:init this))
                    (FnExpr''emitForDefn (:init this), objx, gen)
                    (Expr'''emit (:init this), :Context'EXPRESSION, objx, gen)
                )
                (.invokeVirtual gen, (Type/getType Var'iface), (Method/getMethod "void bindRoot(Object)"))
            )
            (when (= context :Context'STATEMENT)
                (.pop gen)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"DefExpr" this]
            Var'iface
        )
    )
)

(about #_"DefParser"
    (defn #_"IParser" DefParser'new []
        (reify IParser
            ;; (def x) or (def x initexpr)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (cond
                    (< 3 (count form))            (throw! "too many arguments to def")
                    (< (count form) 2)            (throw! "too few arguments to def")
                    (not (symbol? (second form))) (throw! "first argument to def must be a Symbol")
                )
                (let [#_"Symbol" sym (second form) #_"Var" v (Compiler'lookupVar sym, true)]
                    (when (some? v) => (throw! "can't refer to qualified var that doesn't exist")
                        (let [[v #_"boolean" shadowsCoreMapping]
                                (when-not (= (:ns v) *ns*) => [v false]
                                    (when (nil? (:ns sym)) => (throw! "can't create defs outside of current ns")
                                        (let [v (Namespace''intern *ns*, sym)]
                                            (Compiler'registerVar v)
                                            [v true]
                                        )
                                    )
                                )
                              #_"Context" c (if (= context :Context'EVAL) context :Context'EXPRESSION)
                              #_"Expr" init (Compiler'analyze c, (third form), (:name (:sym v)))
                              #_"Expr" meta (Compiler'analyze c, (assoc (meta sym) :line *line*))]
                            (DefExpr'new *line*, v, init, meta, (= (count form) 3), shadowsCoreMapping)
                        )
                    )
                )
            )
        )
    )
)

(about #_"BindingInit"
    (defr BindingInit [])

    (defn #_"BindingInit" BindingInit'new [#_"LocalBinding" binding, #_"Expr" init]
        (merge (BindingInit'class.)
            (hash-map
                #_"LocalBinding" :binding binding
                #_"Expr" :init init
            )
        )
    )
)

(about #_"LetFnExpr"
    (defr LetFnExpr [])

    (defn #_"LetFnExpr" LetFnExpr'new [#_"vector" bindingInits, #_"Expr" body]
        (merge (LetFnExpr'class.)
            (hash-map
                #_"vector" :bindingInits bindingInits
                #_"Expr" :body body
            )
        )
    )

    (defm LetFnExpr Expr
        (#_"Object" Expr'''eval [#_"LetFnExpr" this]
            (throw! "can't eval letfns")
        )

        (#_"void" Expr'''emit [#_"LetFnExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (dotimes [#_"int" i (count (:bindingInits this))]
                (let [#_"BindingInit" bi (nth (:bindingInits this) i)]
                    (.visitInsn gen, Opcodes/ACONST_NULL)
                    (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:idx (:binding bi)))
                )
            )
            (let [#_"IPersistentSet" lbset
                    (loop-when [lbset #{} #_"int" i 0] (< i (count (:bindingInits this))) => lbset
                        (let [#_"BindingInit" bi (nth (:bindingInits this) i)]
                            (Expr'''emit (:init bi), :Context'EXPRESSION, objx, gen)
                            (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:idx (:binding bi)))
                            (recur (conj lbset (:binding bi)) (inc i))
                        )
                    )]
                (dotimes [#_"int" i (count (:bindingInits this))]
                    (let [#_"BindingInit" bi (nth (:bindingInits this) i)]
                        (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ILOAD), (:idx (:binding bi)))
                        (IopObject''emitLetFnInits (:init bi), gen, objx, lbset)
                    )
                )
                (let [#_"Label" loopLabel (.mark gen)]
                    (Expr'''emit (:body this), context, objx, gen)
                    (let [#_"Label" end (.mark gen)]
                        (loop-when-recur [#_"seq" bis (seq (:bindingInits this))] (some? bis) [(next bis)]
                            (let [#_"BindingInit" bi (first bis)
                                  #_"String" lname (:name (:binding bi)) lname (if (.endsWith lname, "__auto__") (str lname (next-id!)) lname)
                                  #_"Class" primc (Compiler'maybePrimitiveType (:init bi))]
                                (.visitLocalVariable gen, lname, (if (some? primc) (Type/getDescriptor primc) "Ljava/lang/Object;"), nil, loopLabel, end, (:idx (:binding bi)))
                            )
                        )
                    )
                )
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"LetFnExpr" this]
            (Expr'''getClass (:body this))
        )
    )
)

(declare even?)

(about #_"LetFnParser"
    (defn #_"IParser" LetFnParser'new []
        (reify IParser
            ;; (letfns* [var (fn [args] body) ...] body...)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (when (vector? (second form)) => (throw! "bad binding form, expected vector")
                    (let [#_"vector" bindings (second form)]
                        (when (even? (count bindings)) => (throw! "bad binding form, expected matched symbol expression pairs")
                            (if (= context :Context'EVAL)
                                (Compiler'analyze context, (list (list Compiler'FNONCE [] form)))
                                (binding [*local-env* *local-env*, *last-local-num* *last-local-num*]
                                    ;; pre-seed env (like Lisp labels)
                                    (let [#_"vector" lbs
                                            (loop-when [lbs [] #_"int" i 0] (< i (count bindings)) => lbs
                                                (let-when [#_"Object" sym (nth bindings i)] (symbol? sym) => (throw! (str "bad binding form, expected symbol, got: " sym))
                                                    (when (nil? (namespace sym)) => (throw! (str "can't let qualified name: " sym))
                                                        (recur (conj lbs (Compiler'registerLocal sym, (Compiler'tagOf sym), nil, false)) (+ i 2))
                                                    )
                                                )
                                            )
                                          #_"vector" bis
                                            (loop-when [bis [] #_"int" i 0] (< i (count bindings)) => bis
                                                (let [#_"Expr" init (Compiler'analyze :Context'EXPRESSION, (nth bindings (inc i)), (:name (nth bindings i)))
                                                      #_"LocalBinding" lb (Compiler'complementLocalInit (nth lbs (quot i 2)), init)]
                                                    (recur (conj bis (BindingInit'new lb, init)) (+ i 2))
                                                )
                                            )]
                                        (LetFnExpr'new bis, (IParser'''parse (BodyParser'new), context, (next (next form))))
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

(about #_"LetExpr"
    (defr LetExpr [])

    (defn #_"LetExpr" LetExpr'new [#_"vector" bindingInits, #_"Expr" body, #_"boolean" isLoop]
        (merge (LetExpr'class.)
            (hash-map
                #_"vector" :bindingInits bindingInits
                #_"Expr" :body body
                #_"boolean" :isLoop isLoop
            )
        )
    )

    (defn- #_"void" LetExpr''doEmit [#_"LetExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"boolean" emitUnboxed]
        (let [#_"{BindingInit Label}" bindingLabels
                (loop-when [bindingLabels {} #_"int" i 0] (< i (count (:bindingInits this))) => bindingLabels
                    (let [#_"BindingInit" bi (nth (:bindingInits this) i)
                          #_"Class" primc (Compiler'maybePrimitiveType (:init bi))]
                        (if (some? primc)
                            (do
                                (MaybePrimitive'''emitUnboxed (:init bi), :Context'EXPRESSION, objx, gen)
                                (.visitVarInsn gen, (.getOpcode (Type/getType primc), Opcodes/ISTORE), (:idx (:binding bi)))
                            )
                            (do
                                (Expr'''emit (:init bi), :Context'EXPRESSION, objx, gen)
                                (.visitVarInsn gen, (.getOpcode (Type/getType Object), Opcodes/ISTORE), (:idx (:binding bi)))
                            )
                        )
                        (recur (assoc bindingLabels bi (.mark gen)) (inc i))
                    )
                )
              #_"Label" loopLabel (.mark gen)]
            (if (:isLoop this)
                (binding [*loop-label* loopLabel]
                    (if emitUnboxed
                        (MaybePrimitive'''emitUnboxed (:body this), context, objx, gen)
                        (Expr'''emit (:body this), context, objx, gen)
                    )
                )
                (if emitUnboxed
                    (MaybePrimitive'''emitUnboxed (:body this), context, objx, gen)
                    (Expr'''emit (:body this), context, objx, gen)
                )
            )
            (let [#_"Label" end (.mark gen)]
                (loop-when-recur [#_"seq" bis (seq (:bindingInits this))] (some? bis) [(next bis)]
                    (let [#_"BindingInit" bi (first bis)
                          #_"String" lname (:name (:binding bi)) lname (if (.endsWith lname, "__auto__") (str lname (next-id!)) lname)
                          #_"Class" primc (Compiler'maybePrimitiveType (:init bi))]
                        (.visitLocalVariable gen, lname, (if (some? primc) (Type/getDescriptor primc) "Ljava/lang/Object;"), nil, (get bindingLabels bi), end, (:idx (:binding bi)))
                    )
                )
            )
        )
        nil
    )

    (defm LetExpr Expr
        (#_"Object" Expr'''eval [#_"LetExpr" this]
            (throw! "can't eval let/loop")
        )

        (#_"void" Expr'''emit [#_"LetExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (LetExpr''doEmit this, context, objx, gen, false)
            nil
        )

        (#_"Class" Expr'''getClass [#_"LetExpr" this]
            (Expr'''getClass (:body this))
        )
    )

    (defm LetExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"LetExpr" this]
            (and (satisfies? MaybePrimitive (:body this)) (MaybePrimitive'''canEmitPrimitive (:body this)))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"LetExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (LetExpr''doEmit this, context, objx, gen, true)
            nil
        )
    )
)

(declare repeat)
(declare push-thread-bindings)
(declare pop-thread-bindings)

(about #_"LetParser"
    (defn #_"IParser" LetParser'new []
        (reify IParser
            ;; (let [var val var2 val2 ...] body...)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"boolean" isLoop (= (first form) 'loop*)]
                    (when (vector? (second form)) => (throw! "bad binding form, expected vector")
                        (let [#_"vector" bindings (second form)]
                            (when (even? (count bindings)) => (throw! "bad binding form, expected matched symbol expression pairs")
                                (if (or (= context :Context'EVAL) (and (= context :Context'EXPRESSION) isLoop))
                                    (Compiler'analyze context, (list (list Compiler'FNONCE [] form)))
                                    (let [#_"seq" body (next (next form))
                                          #_"map" locals' (:locals *method*)]
                                        ;; may repeat once for each binding with a mismatch, return breaks
                                        (loop [#_"vector" rms (vec (repeat (quot (count bindings) 2) false))]
                                            (let [#_"map" dynamicBindings
                                                    (hash-map
                                                        #'*local-env*      *local-env*
                                                        #'*last-local-num* *last-local-num*
                                                    )
                                                  dynamicBindings
                                                    (when isLoop => dynamicBindings
                                                        (assoc dynamicBindings #'*loop-locals* nil)
                                                    )
                                                  _ (update! *method* assoc :locals locals')
                                                  [rms #_"LetExpr" letExpr]
                                                    (try
                                                        (push-thread-bindings dynamicBindings)
                                                        (let [[#_"vector" bindingInits #_"vector" loopLocals]
                                                                (loop-when [bindingInits [] loopLocals [] #_"int" i 0] (< i (count bindings)) => [bindingInits loopLocals]
                                                                    (let-when [#_"Object" sym (nth bindings i)] (symbol? sym) => (throw! (str "bad binding form, expected symbol, got: " sym))
                                                                        (when (nil? (namespace sym)) => (throw! (str "can't let qualified name: " sym))
                                                                            (let [#_"Expr" init (Compiler'analyze :Context'EXPRESSION, (nth bindings (inc i)), (:name sym))
                                                                                  init
                                                                                    (when isLoop => init
                                                                                        (if (and (some? rms) (nth rms (quot i 2)))
                                                                                            (do
                                                                                                (when *warn-on-reflection*
                                                                                                    (.println *err*, (str "Auto-boxing loop arg: " sym))
                                                                                                )
                                                                                                (StaticMethodExpr'new 0, nil, RT'iface, "box", [init], false)
                                                                                            )
                                                                                            (condp = (Compiler'maybePrimitiveType init)
                                                                                                Integer/TYPE (StaticMethodExpr'new 0, nil, RT'iface, "longCast", [init], false)
                                                                                                             init
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                  ;; sequential enhancement of env (like Lisp let*)
                                                                                  [bindingInits loopLocals]
                                                                                    (try
                                                                                        (when isLoop
                                                                                            (push-thread-bindings (hash-map #'*no-recur* false))
                                                                                        )
                                                                                        (let [#_"LocalBinding" lb (Compiler'registerLocal sym, (Compiler'tagOf sym), init, false)]
                                                                                            [(conj bindingInits (BindingInit'new lb, init)) (if isLoop (conj loopLocals lb) loopLocals)]
                                                                                        )
                                                                                        (finally
                                                                                            (when isLoop
                                                                                                (pop-thread-bindings)
                                                                                            )
                                                                                        )
                                                                                    )]
                                                                                (recur bindingInits loopLocals (+ i 2))
                                                                            )
                                                                        )
                                                                    )
                                                                )]
                                                            (when isLoop
                                                                (set! *loop-locals* loopLocals)
                                                            )
                                                            (let [#_"Expr" bodyExpr
                                                                    (try
                                                                        (when isLoop
                                                                            (push-thread-bindings
                                                                                (hash-map
                                                                                    #'*no-recur*          false
                                                                                    #'*in-return-context* (and (= context :Context'RETURN) *in-return-context*)
                                                                                )
                                                                            )
                                                                        )
                                                                        (IParser'''parse (BodyParser'new), (if isLoop :Context'RETURN context), body)
                                                                        (finally
                                                                            (when isLoop
                                                                                (pop-thread-bindings)
                                                                            )
                                                                        )
                                                                    )
                                                                  [rms #_"boolean" more?]
                                                                    (when isLoop => [rms false]
                                                                        (loop-when [rms rms more? false #_"int" i 0] (< i (count *loop-locals*)) => [rms more?]
                                                                            (let [[rms more?]
                                                                                    (when (:recurMistmatch (nth *loop-locals* i)) => [rms more?]
                                                                                        [(assoc rms i true) true]
                                                                                    )]
                                                                                (recur rms more? (inc i))
                                                                            )
                                                                        )
                                                                    )]
                                                                [rms (when-not more? (LetExpr'new bindingInits, bodyExpr, isLoop))]
                                                            )
                                                        )
                                                        (finally
                                                            (pop-thread-bindings)
                                                        )
                                                    )]
                                                (recur-when (nil? letExpr) [rms] => letExpr)
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
    )
)

(about #_"RecurExpr"
    (defr RecurExpr [])

    (defn #_"RecurExpr" RecurExpr'new [#_"vector" loopLocals, #_"vector" args, #_"int" line]
        (merge (RecurExpr'class.)
            (hash-map
                #_"vector" :loopLocals loopLocals
                #_"vector" :args args
                #_"int" :line line
            )
        )
    )

    (defm RecurExpr Expr
        (#_"Object" Expr'''eval [#_"RecurExpr" this]
            (throw! "can't eval recur")
        )

        (#_"void" Expr'''emit [#_"RecurExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (let-when [#_"Label" loopLabel *loop-label*] (some? loopLabel) => (throw! "recur misses loop label")
                (dotimes [#_"int" i (count (:loopLocals this))]
                    (let [#_"LocalBinding" lb (nth (:loopLocals this) i) #_"Expr" arg (nth (:args this) i)]
                        (when (some? (LocalBinding''getPrimitiveType lb)) => (Expr'''emit arg, :Context'EXPRESSION, objx, gen)
                            (let [#_"Class" primc (LocalBinding''getPrimitiveType lb) #_"Class" pc (Compiler'maybePrimitiveType arg)]
                                (cond (= primc pc)
                                    (do
                                        (MaybePrimitive'''emitUnboxed arg, :Context'EXPRESSION, objx, gen)
                                    )
                                    (and (= primc Long/TYPE) (= pc Integer/TYPE))
                                    (do
                                        (MaybePrimitive'''emitUnboxed arg, :Context'EXPRESSION, objx, gen)
                                        (.visitInsn gen, Opcodes/I2L)
                                    )
                                    (and (= primc Integer/TYPE) (= pc Long/TYPE))
                                    (do
                                        (MaybePrimitive'''emitUnboxed arg, :Context'EXPRESSION, objx, gen)
                                        (.invokeStatic gen, (Type/getType RT'iface), (Method/getMethod "int intCast(long)"))
                                    )
                                    :else
                                    (do
                                        (throw! (str "recur arg for primitive local: " (:name lb) " is not matching primitive, had: " (.getName (or (Expr'''getClass arg) Object)) ", needed: " (.getName primc)))
                                    )
                                )
                            )
                        )
                    )
                )
                (loop-when-recur [#_"int" i (dec (count (:loopLocals this)))] (<= 0 i) [(dec i)]
                    (let [#_"LocalBinding" lb (nth (:loopLocals this) i) #_"Class" primc (LocalBinding''getPrimitiveType lb)]
                        (if (:isArg lb)
                            (.storeArg gen, (dec (:idx lb)))
                            (.visitVarInsn gen, (.getOpcode (Type/getType (or primc Object)), Opcodes/ISTORE), (:idx lb))
                        )
                    )
                )
                (.goTo gen, loopLabel)
            )
            nil
        )

        (#_"Class" Expr'''getClass [#_"RecurExpr" this]
            Recur'iface
        )
    )

    (defm RecurExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"RecurExpr" this]
            true
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"RecurExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (Expr'''emit this, context, objx, gen)
            nil
        )
    )
)

(about #_"RecurParser"
    (defn #_"IParser" RecurParser'new []
        (reify IParser
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (when-not (and (= context :Context'RETURN) (some? *loop-locals*))
                    (throw! "can only recur from tail position")
                )
                (when *no-recur*
                    (throw! "cannot recur across try")
                )
                (let [#_"int" line *line*
                      #_"vector" args
                        (loop-when-recur [args [] #_"seq" s (seq (next form))]
                                         (some? s)
                                         [(conj args (Compiler'analyze :Context'EXPRESSION, (first s))) (next s)]
                                      => args
                        )]
                    (when-not (= (count args) (count *loop-locals*))
                        (throw! (str "mismatched argument count to recur, expected: " (count *loop-locals*) " args, got: " (count args)))
                    )
                    (dotimes [#_"int" i (count *loop-locals*)]
                        (let [#_"LocalBinding" lb (nth *loop-locals* i)]
                            (when-some [#_"Class" primc (LocalBinding''getPrimitiveType lb)]
                                (let [#_"Class" pc (Compiler'maybePrimitiveType (nth args i))
                                      #_"boolean" mismatch?
                                        (condp = primc
                                            Long/TYPE (not (any = pc Long/TYPE Integer/TYPE Character/TYPE Byte/TYPE))
                                                      false
                                        )]
                                    (when mismatch?
                                        (update! *loop-locals* update i assoc :recurMistmatch true)
                                        (when *warn-on-reflection*
                                            (.println *err*, (str "line " line ": recur arg for primitive local: " (:name lb) " is not matching primitive, had: " (if (some? pc) (.getName pc) "Object") ", needed: " (.getName primc)))
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (RecurExpr'new *loop-locals*, args, line)
                )
            )
        )
    )
)

(about #_"NewInstanceMethod"
    (defr NewInstanceMethod [])

    (defn #_"NewInstanceMethod" NewInstanceMethod'new [#_"IopObject" objx, #_"IopMethod" parent]
        (merge (NewInstanceMethod'class.) (IopMethod'init objx, parent)
            (hash-map
                #_"String" :name nil
                #_"Type[]" :argTypes nil
                #_"Type" :retType nil
                #_"Class" :retClass nil
                #_"Class[]" :exClasses nil

                #_"vector" :parms nil
            )
        )
    )

    (defm NewInstanceMethod IopMethod
        (#_"int" IopMethod'''numParams [#_"NewInstanceMethod" this]
            (count (:argLocals this))
        )

        (#_"String" IopMethod'''getMethodName => :name)

        (#_"Type" IopMethod'''getReturnType => :retType)

        (#_"Type[]" IopMethod'''getArgTypes => :argTypes)

        (#_"void" IopMethod'''emit [#_"NewInstanceMethod" this, #_"IopObject" obj, #_"ClassVisitor" cv]
            (let [#_"Method" m (Method. (IopMethod'''getMethodName this), (IopMethod'''getReturnType this), (IopMethod'''getArgTypes this))
                  #_"Type[]" exTypes
                    (let-when [#_"int" n (count (:exClasses this))] (pos? n)
                        (let [exTypes (make-array Type n)]
                            (dotimes [#_"int" i n]
                                (aset! exTypes i (Type/getType (aget (:exClasses this) i)))
                            )
                            exTypes
                        )
                    )
                  #_"GeneratorAdapter" gen (GeneratorAdapter. Opcodes/ACC_PUBLIC, m, nil, exTypes, cv)]
                (.visitCode gen)
                (let [#_"Label" loopLabel (.mark gen)]
                    (.visitLineNumber gen, (:line this), loopLabel)
                    (binding [*loop-label* loopLabel, *method* this]
                        (IopMethod'emitBody (:objx this), gen, (:retClass this), (:body this))
                        (let [#_"Label" end (.mark gen)]
                            (.visitLocalVariable gen, "this", (.getDescriptor (:objType obj)), nil, loopLabel, end, 0)
                            (loop-when-recur [#_"seq" lbs (seq (:argLocals this))] (some? lbs) [(next lbs)]
                                (let [#_"LocalBinding" lb (first lbs)]
                                    (.visitLocalVariable gen, (:name lb), (.getDescriptor (aget (:argTypes this) (dec (:idx lb)))), nil, loopLabel, end, (:idx lb))
                                )
                            )
                        )
                    )
                    (.returnValue gen)
                    (.endMethod gen)
                )
            )
            nil
        )
    )

    (defn- #_"map" NewInstanceMethod'findMethodsWithNameAndArity [#_"String" name, #_"int" arity, #_"map" overrideables]
        (loop-when [#_"map" found {} #_"seq" s (seq overrideables)] (some? s) => found
            (let [#_"IMapEntry" e (first s) #_"java.lang.reflect.Method" m (val e)
                  found
                    (when (and (= name (.getName m)) (= (count (.getParameterTypes m)) arity)) => found
                        (assoc found (key e) m)
                    )]
                (recur found (next s))
            )
        )
    )

    (defn #_"NewInstanceMethod" NewInstanceMethod'parse [#_"IopObject" objx, #_"seq" form, #_"Symbol" thistag, #_"map" overrideables]
        ;; (methodname [this-name args*] body...)
        ;; this-name might be nil
        (let [#_"NewInstanceMethod" nim
                (-> (NewInstanceMethod'new objx, *method*)
                    (assoc :line *line*)
                )
              #_"Symbol" dotname (first form) #_"Symbol" name (with-meta (symbol (Compiler'munge (:name dotname))) (meta dotname))
              #_"vector" parms (second form)]
            (when (pos? (count parms)) => (throw! (str "must supply at least one argument for 'this' in: " dotname))
                (let [#_"Symbol" thisName (nth parms 0) parms (subvec parms 1 (count parms))
                      #_"seq" body (next (next form))]
                    ;; register as the current method and set up a new env frame
                    (binding [*method*            nim
                              *local-env*         *local-env*
                              *last-local-num*    -1
                              *loop-locals*       nil
                              *in-return-context* true]
                        ;; register 'this' as local 0
                        (if (some? thisName)
                            (Compiler'registerLocal thisName, thistag, nil, false)
                            (Compiler'nextLocalNum)
                        )
                        (let [nim (assoc nim :retClass (Interop'tagClass (Compiler'tagOf name)))
                              nim (assoc nim :argTypes (make-array Type (count parms)))
                              #_"Class[]" pclasses (make-array Class (count parms))
                              #_"Symbol[]" psyms (make-array #_"Symbol" Object (count parms))
                              #_"boolean" hinted?
                                (loop-when [hinted? (some? (Compiler'tagOf name)) #_"int" i 0] (< i (count parms)) => hinted?
                                    (let-when [#_"Object" sym (nth parms i)] (symbol? sym) => (throw! "params must be Symbols")
                                        (let [#_"Object" tag (Compiler'tagOf sym) hinted? (or hinted? (some? tag))]
                                            (aset! pclasses i (Interop'tagClass tag))
                                            (aset! psyms i (if (some? (namespace sym)) (symbol (:name sym)) sym))
                                            (recur hinted? (inc i))
                                        )
                                    )
                                )
                              #_"map" matches (NewInstanceMethod'findMethodsWithNameAndArity (:name name), (count parms), overrideables)
                              #_"vector" mk [(:name name) (seq pclasses)]
                              [nim pclasses #_"java.lang.reflect.Method" m]
                                (case (count matches)
                                    0   (throw! (str "can't define method not in interfaces: " (:name name)))
                                    1   (if hinted? ;; validate match
                                            (let [m (get matches mk)]
                                                (when (nil? m)
                                                    (throw! (str "can't find matching method: " (:name name) ", leave off hints for auto match"))
                                                )
                                                (when-not (= (.getReturnType m) (:retClass nim))
                                                    (throw! (str "mismatched return type: " (:name name) ", expected: " (.getName (.getReturnType m)) ", had: " (.getName (:retClass nim))))
                                                )
                                                [nim pclasses m]
                                            )
                                            ;; adopt found method sig
                                            (let [m (val (first matches))]
                                                [(assoc nim :retClass (.getReturnType m)) (.getParameterTypes m) m]
                                            )
                                        )
                                        ;; must be hinted and match one method
                                        (when hinted? => (throw! (str "must hint overloaded method: " (:name name)))
                                            (let [m (get matches mk)]
                                                (when (nil? m)
                                                    (throw! (str "can't find matching overloaded method: " (:name name)))
                                                )
                                                (when-not (= (.getReturnType m) (:retClass nim))
                                                    (throw! (str "mismatched return type: " (:name name) ", expected: " (.getName (.getReturnType m)) ", had: " (.getName (:retClass nim))))
                                                )
                                                [nim pclasses m]
                                            )
                                        )
                                )
                              ;; validate unique name+arity among additional methods
                              nim (assoc nim :retType (Type/getType (:retClass nim)))
                              nim (assoc nim :exClasses (.getExceptionTypes m))
                              #_"vector" argLocals
                                (loop-when [argLocals [] #_"int" i 0] (< i (count parms)) => argLocals
                                    (let [#_"LocalBinding" lb (Compiler'registerLocal (aget psyms i), nil, (MethodParamExpr'new (aget pclasses i)), true)]
                                        (aset! (:argTypes nim) i (Type/getType (aget pclasses i)))
                                        (recur (conj argLocals lb) (inc i))
                                    )
                                )]
                            (dotimes [#_"int" i (count parms)]
                                (when (= (aget pclasses i) Long/TYPE)
                                    (Compiler'nextLocalNum)
                                )
                            )
                            (set! *loop-locals* argLocals)
                            (assoc nim
                                :name (:name name)
                                :methodMeta (meta name)
                                :parms parms
                                :argLocals argLocals
                                :body (IParser'''parse (BodyParser'new), :Context'RETURN, body)
                            )
                        )
                    )
                )
            )
        )
    )
)

(about #_"NewInstanceExpr"
    (defr NewInstanceExpr [])

    (defn- #_"NewInstanceExpr" NewInstanceExpr'new [#_"Object" tag]
        (merge (NewInstanceExpr'class.) (IopObject'init tag)
            (hash-map
                #_"IPersistentCollection" :methods nil

                #_"{IPersistentVector java.lang.reflect.Method}" :overrideables nil
                #_"{IPersistentVector {Class}}" :covariants nil
            )
        )
    )

    ;;;
     ; Current host interop uses reflection, which requires pre-existing classes.
     ; Work around this by:
     ; Generate a stub class that has the same interfaces and fields as the class we are generating.
     ; Use it as a type hint for this, and bind the simple name of the class to this stub (in resolve etc.)
     ; Unmunge the name (using a magic prefix) on any code gen for classes.
     ;;
    (defn #_"Class" NewInstanceExpr'compileStub [#_"String" superName, #_"NewInstanceExpr" ret, #_"String[]" interfaceNames, #_"seq" form]
        (let [#_"ClassWriter" cw (ClassWriter. ClassWriter/COMPUTE_MAXS) #_"ClassVisitor" cv cw]
            (.visit cv, Opcodes/V1_5, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER), (str Compiler'COMPILE_STUB_PREFIX "/" (:internalName ret)), nil, superName, interfaceNames)

            ;; instance fields for closed-overs
            (loop-when-recur [#_"seq" s (vals (get *closes* (:uid ret)))] (some? s) [(next s)]
                (let [#_"LocalBinding" lb (first s)
                      #_"int" access (+ Opcodes/ACC_PUBLIC (if (IopObject''isVolatile ret, lb) Opcodes/ACC_VOLATILE (if (IopObject''isMutable ret, lb) 0 Opcodes/ACC_FINAL)))]
                    (if (some? (LocalBinding''getPrimitiveType lb))
                        (.visitField cv, access, (:name lb), (.getDescriptor (Type/getType (LocalBinding''getPrimitiveType lb))), nil, nil)
                        ;; todo - when closed-overs are fields, use more specific types here and in ctor and emitLocal?
                        (.visitField cv, access, (:name lb), (.getDescriptor (Type/getType Object)), nil, nil)
                    )
                )
            )

            ;; ctor that takes closed-overs and does nothing
            (let [#_"Method" m (Method. "<init>", Type/VOID_TYPE, (IopObject''ctorTypes ret))
                  #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, m, nil, nil, cv)]
                (.visitCode ctorgen)
                (.loadThis ctorgen)
                (.invokeConstructor ctorgen, (Type/getObjectType superName), (Method/getMethod "void <init>()"))
                (.returnValue ctorgen)
                (.endMethod ctorgen)
            )

            (when (pos? (:altCtorDrops ret))
                (let [#_"Type[]" ctorTypes (IopObject''ctorTypes ret)]

                    (let [#_"Type[]" altCtorTypes (make-array Type (- (count ctorTypes) (:altCtorDrops ret)))
                          _ (dotimes [#_"int" i (count altCtorTypes)]
                                (aset! altCtorTypes i (aget ctorTypes i))
                            )
                          #_"Method" alt (Method. "<init>", Type/VOID_TYPE, altCtorTypes)
                          #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, alt, nil, nil, cv)]
                        (.visitCode ctorgen)
                        (.loadThis ctorgen)
                        (.loadArgs ctorgen)

                        (.visitInsn ctorgen, Opcodes/ACONST_NULL) ;; __meta
                        (.visitInsn ctorgen, Opcodes/ACONST_NULL) ;; __extmap
                        (.visitInsn ctorgen, Opcodes/ICONST_0) ;; __hash

                        (.invokeConstructor ctorgen, (Type/getObjectType (str Compiler'COMPILE_STUB_PREFIX "/" (:internalName ret))), (Method. "<init>", Type/VOID_TYPE, ctorTypes))

                        (.returnValue ctorgen)
                        (.endMethod ctorgen)
                    )

                    ;; alt ctor w/o __hash
                    (let [#_"Type[]" altCtorTypes (make-array Type (- (count ctorTypes) 2))
                          _ (dotimes [#_"int" i (count altCtorTypes)]
                                (aset! altCtorTypes i (aget ctorTypes i))
                            )
                          #_"Method" alt (Method. "<init>", Type/VOID_TYPE, altCtorTypes)
                          #_"GeneratorAdapter" ctorgen (GeneratorAdapter. Opcodes/ACC_PUBLIC, alt, nil, nil, cv)]
                        (.visitCode ctorgen)
                        (.loadThis ctorgen)
                        (.loadArgs ctorgen)

                        (.visitInsn ctorgen, Opcodes/ICONST_0) ;; __hash

                        (.invokeConstructor ctorgen, (Type/getObjectType (str Compiler'COMPILE_STUB_PREFIX "/" (:internalName ret))), (Method. "<init>", Type/VOID_TYPE, ctorTypes))

                        (.returnValue ctorgen)
                        (.endMethod ctorgen)
                    )
                )
            )

            ;; end of class
            (.visitEnd cv)

            (Loader''defineClass *class-loader*, (str Compiler'COMPILE_STUB_PREFIX "." (:name ret)), (.toByteArray cw))
        )
    )

    (defn #_"String" NewInstanceExpr'slashname [#_"Class" c]
        (.replace (.getName c), \., \/)
    )

    (defn #_"String[]" NewInstanceExpr'interfaceNames [#_"vector" interfaces]
        (let [#_"int" n (count interfaces)
              #_"String[]" inames (when (pos? n) (make-array String n))]
            (dotimes [#_"int" i n]
                (aset! inames i (NewInstanceExpr'slashname (nth interfaces i)))
            )
            inames
        )
    )

    (defm NewInstanceExpr Expr
        (#_"Object" Expr'''eval => IopObject''doEval)

        (#_"void" Expr'''emit [#_"NewInstanceExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (IopObject''doEmit this, context, objx, gen)
            nil
        )

        #_memoize!
        (#_"Class" Expr'''getClass [#_"NewInstanceExpr" this]
            (or (:compiledClass this)
                (if (some? (:tag this)) (Interop'tagToClass (:tag this)) IFn'iface)
            )
        )
    )

    (defm NewInstanceExpr IopObject
        (#_"boolean" IopObject'''supportsMeta [#_"NewInstanceExpr" this]
            (not (IopObject''isDeftype this))
        )

        (#_"void" IopObject'''emitStatics [#_"NewInstanceExpr" this, #_"ClassVisitor" cv]
            (when (IopObject''isDeftype this)
                ;; getBasis()
                (let [#_"Method" meth (Method/getMethod "arbace.core.IPersistentVector getBasis()")
                      #_"GeneratorAdapter" gen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), meth, nil, nil, cv)]
                    (IopObject''emitValue this, (:hintedFields this), gen)
                    (.returnValue gen)
                    (.endMethod gen)

                    (let-when [#_"int" n (count (:hintedFields this))] (< n (count (:fields this)))
                        ;; create(IPersistentMap)
                        (let [#_"String" className (.replace (:name this), \., \/)
                              #_"MethodVisitor" mv (.visitMethod cv, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), "create", (str "(Larbace/core/IPersistentMap;)L" className ";"), nil, nil)]
                            (.visitCode mv)

                            (loop-when-recur [#_"seq" s (seq (:hintedFields this)) #_"int" i 1] (some? s) [(next s) (inc i)]
                                (let [#_"String" bName (:name (first s))
                                      #_"Class" k (Interop'tagClass (Compiler'tagOf (first s)))]
                                    (.visitVarInsn mv, Opcodes/ALOAD, 0)
                                    (.visitLdcInsn mv, bName)
                                    (.visitMethodInsn mv, Opcodes/INVOKESTATIC, "arbace/core/Keyword", "intern", "(Ljava/lang/String;)Larbace/core/Keyword;")
                                    (.visitInsn mv, Opcodes/ACONST_NULL)
                                    (.visitMethodInsn mv, Opcodes/INVOKEINTERFACE, "arbace/core/IPersistentMap", "valAt", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
                                    (when (.isPrimitive k)
                                        (.visitTypeInsn mv, Opcodes/CHECKCAST, (.getInternalName (Type/getType (Compiler'boxClass k))))
                                    )
                                    (.visitVarInsn mv, Opcodes/ASTORE, i)
                                    (.visitVarInsn mv, Opcodes/ALOAD, 0)
                                    (.visitLdcInsn mv, bName)
                                    (.visitMethodInsn mv, Opcodes/INVOKESTATIC, "arbace/core/Keyword", "intern", "(Ljava/lang/String;)Larbace/core/Keyword;")
                                    (.visitMethodInsn mv, Opcodes/INVOKEINTERFACE, "arbace/core/IPersistentMap", "dissoc", "(Ljava/lang/Object;)Larbace/core/IPersistentMap;")
                                    (.visitVarInsn mv, Opcodes/ASTORE, 0)
                                )
                            )

                            (.visitTypeInsn mv, Opcodes/NEW, className)
                            (.visitInsn mv, Opcodes/DUP)

                            (let [#_"Method" ctor (Method. "<init>", Type/VOID_TYPE, (IopObject''ctorTypes this))]
                                (dotimes [#_"int" i n]
                                    (.visitVarInsn mv, Opcodes/ALOAD, (inc i))
                                    (let-when [#_"Class" k (Interop'tagClass (Compiler'tagOf (nth (:hintedFields this) i)))] (.isPrimitive k)
                                        (.visitMethodInsn mv, Opcodes/INVOKEVIRTUAL, (.getInternalName (Type/getType (Compiler'boxClass k))), (str (.getName k) "Value"), (str "()" (.getDescriptor (Type/getType k))))
                                    )
                                )

                                (.visitInsn mv, Opcodes/ACONST_NULL) ;; __meta
                                (.visitVarInsn mv, Opcodes/ALOAD, 0) ;; __extmap
                                (.visitMethodInsn mv, Opcodes/INVOKESTATIC, "arbace/core/RT", "seqOrElse", "(Ljava/lang/Object;)Ljava/lang/Object;")
                                (.visitInsn mv, Opcodes/ICONST_0) ;; __hash
                                (.visitMethodInsn mv, Opcodes/INVOKESPECIAL, className, "<init>", (.getDescriptor ctor))
                                (.visitInsn mv, Opcodes/ARETURN)
                                (.visitMaxs mv, (+ 4 n), (+ 1 n))
                                (.visitEnd mv)
                            )
                        )
                    )
                )
            )
            nil
        )

        (#_"void" IopObject'''emitMethods [#_"NewInstanceExpr" this, #_"ClassVisitor" cv]
            (loop-when-recur [#_"seq" s (seq (:methods this))] (some? s) [(next s)]
                (IopMethod'''emit (first s), this, cv)
            )
            ;; emit bridge methods
            (doseq [#_"IMapEntry" e (:covariants this)]
                (let [#_"java.lang.reflect.Method" m (get (:overrideables this) (key e))
                      #_"Class[]" params (.getParameterTypes m)
                      #_"Type[]" argTypes (make-array Type (count params))
                      _ (dotimes [#_"int" i (count params)]
                            (aset! argTypes i (Type/getType (aget params i)))
                        )
                      #_"Method" target (Method. (.getName m), (Type/getType (.getReturnType m)), argTypes)]
                    (doseq [#_"Class" retType (val e)]
                        (let [#_"Method" meth (Method. (.getName m), (Type/getType retType), argTypes)
                              #_"GeneratorAdapter" gen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_BRIDGE), meth, nil, Compiler'EXCEPTION_TYPES, cv)]
                            (.visitCode gen)
                            (.loadThis gen)
                            (.loadArgs gen)
                            (.invokeInterface gen, (Type/getType (.getDeclaringClass m)), target)
                            (.returnValue gen)
                            (.endMethod gen)
                        )
                    )
                )
            )
            nil
        )
    )

    (defn- #_"vector" NewInstanceExpr'considerMethod [#_"java.lang.reflect.Method" m]
        (let [#_"int" mods (.getModifiers m)]
            (when (and (or (Modifier/isPublic mods) (Modifier/isProtected mods)) (not (Modifier/isStatic mods)) (not (Modifier/isFinal mods)))
                [(.getName m) (seq (.getParameterTypes m)) (.getReturnType m)]
            )
        )
    )

    (declare assoc!)
    (declare concat)

    (defn- #_"ITransientMap" NewInstanceExpr'harvestMethods [#_"ITransientMap" m, #_"Class" c]
        (when (some? c) => m
            (let [m (reduce #(if-some [#_"vector" v (NewInstanceExpr'considerMethod %2)] (assoc! %1 v %2) %1)
                            m
                            (concat (.getMethods c) (.getDeclaredMethods c))
                    )]
                (recur m (.getSuperclass c))
            )
        )
    )

    (defn #_"[{IPersistentVector java.lang.reflect.Method} {IPersistentVector {Class}}]" NewInstanceExpr'gatherMethods [#_"Class" super, #_"seq" ifaces]
        (let [#_"map" all (reduce! NewInstanceExpr'harvestMethods {} (cons super ifaces))]
            (loop-when [#_"map" methods {} #_"map" covariants {} #_"seq" s (seq all)] (some? s) => [methods covariants]
                (let [#_"IMapEntry" e (first s) #_"vector" mk (pop (key e)) #_"java.lang.reflect.Method" m (val e)]
                    (if (contains? methods mk) ;; covariant return
                        (let [#_"Class" tk (.getReturnType (get methods mk)) #_"Class" t (.getReturnType m)
                              senj- #(conj (or %1 #{}) %2)]
                            (if (.isAssignableFrom tk, t)
                                (recur (assoc methods mk m) (update covariants mk senj- tk) (next s))
                                (recur        methods       (update covariants mk senj- t)  (next s))
                            )
                        )
                        (recur (assoc methods mk m) covariants (next s))
                    )
                )
            )
        )
    )

    (declare PersistentArrayMap'new)

    (defn #_"IopObject" NewInstanceExpr'build [#_"vector" interfaceSyms, #_"vector" fieldSyms, #_"Symbol" thisSym, #_"String" tagName, #_"Symbol" className, #_"Symbol" typeTag, #_"seq" methodForms, #_"seq" form, #_"map" opts]
        (let [#_"String" name (str className) #_"String" name' (.replace name, \., \/)
              #_"NewInstanceExpr" nie
                (-> (NewInstanceExpr'new nil)
                    (assoc :name name :internalName name' :objType (Type/getObjectType name') :opts opts)
                )
              nie (if (some? thisSym) (assoc nie :thisName (:name thisSym)) nie)
              nie
                (when (some? fieldSyms) => nie
                    (let [#_"Object[]" a (object-array (* 2 (count fieldSyms)))
                          #_"map" fmap
                            (loop-when [fmap {} #_"int" i 0] (< i (count fieldSyms)) => fmap
                                (let [#_"Symbol" sym (nth fieldSyms i)
                                      #_"LocalBinding" lb (LocalBinding'new -1, sym, nil, (MethodParamExpr'new (Interop'tagClass (Compiler'tagOf sym))), false)]
                                    (aset! a (* i 2) (:uid lb))
                                    (aset! a (inc (* i 2)) lb)
                                    (recur (assoc fmap sym lb) (inc i))
                                )
                            )
                          ;; use array map to preserve ctor order
                          _ (update! *closes* assoc (:uid nie) (PersistentArrayMap'new a))
                          nie (assoc nie :fields fmap)]
                        (loop-when-recur [nie nie #_"int" i (dec (count fieldSyms))]
                                         (and (<= 0 i) (any = (:name (nth fieldSyms i)) "__meta" "__extmap" "__hash"))
                                         [(update nie :altCtorDrops inc) (dec i)]
                                      => nie
                        )
                    )
                )
              #_"vector" ifaces
                (loop-when [ifaces [] #_"seq" s (seq interfaceSyms)] (some? s) => ifaces
                    (let [#_"Class" c (Compiler'resolve (first s))]
                        (when (.isInterface c) => (throw! (str "only interfaces are supported, had: " (.getName c)))
                            (recur (conj ifaces c) (next s))
                        )
                    )
                )
              #_"Class" super Object
              [#_"map" overrideables #_"map" covariants] (NewInstanceExpr'gatherMethods super, (seq ifaces))
              nie (assoc nie :overrideables overrideables :covariants covariants)
              #_"String[]" inames (NewInstanceExpr'interfaceNames ifaces)
              #_"Class" stub (NewInstanceExpr'compileStub (NewInstanceExpr'slashname super), nie, inames, form)
              #_"Symbol" thistag (symbol (.getName stub))
              nie
                (binding [*constants*          []
                          *constant-ids*       (IdentityHashMap.)
                          *keywords*           {}
                          *vars*               {}
                          *keyword-callsites*  []
                          *protocol-callsites* []
                          *no-recur*           false]
                    (try
                        (let [nie
                                (when (IopObject''isDeftype nie) => nie
                                    (push-thread-bindings
                                        (hash-map
                                            #'*method*             nil
                                            #'*local-env*          (:fields nie)
                                            #'*compile-stub-sym*   (symbol tagName)
                                            #'*compile-stub-class* stub
                                        )
                                    )
                                    (assoc nie :hintedFields (subvec fieldSyms 0 (- (count fieldSyms) (:altCtorDrops nie))))
                                )
                              ;; now (methodname [args] body)*
                              nie (assoc nie :line *line*)
                              #_"IPersistentCollection" methods
                                (loop-when [methods nil #_"seq" s methodForms] (some? s) => methods
                                    (let [#_"NewInstanceMethod" m (NewInstanceMethod'parse nie, (first s), thistag, overrideables)]
                                        (recur (conj methods m) (next s))
                                    )
                                )]
                            (assoc nie
                                :methods methods
                                :keywords *keywords*
                                :vars *vars*
                                :constants *constants*
                                :keywordCallsites *keyword-callsites*
                                :protocolCallsites *protocol-callsites*
                            )
                        )
                        (finally
                            (when (IopObject''isDeftype nie)
                                (pop-thread-bindings)
                            )
                        )
                    )
                )]
            (IopObject''compile nie, (NewInstanceExpr'slashname super), inames, false)
        )
    )
)

(about #_"ReifyParser"
    (defn #_"IParser" ReifyParser'new []
        (reify IParser
            ;; (reify this-name? [interfaces] (method-name [args] body)*)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"seq" s form                                       s (next s)
                      #_"vector" ifaces (conj (first s) 'arbace.core.IObj) s (next s)
                      #_"String" classname
                        (let [#_"IopMethod" owner *method*
                              #_"String" basename (if (some? owner) (IopObject'trimGenID (:name (:objx owner))) (Compiler'munge (:name (:name *ns*))))]
                            (str basename "$" "reify__" (next-id!))
                        )
                      #_"IopObject" nie (NewInstanceExpr'build ifaces, nil, nil, classname, (symbol classname), nil, s, form, nil)]
                    (when (and (satisfies? IObj form) (some? (meta form))) => nie
                        (MetaExpr'new nie, (MapExpr'parse (if (= context :Context'EVAL) context :Context'EXPRESSION), (meta form)))
                    )
                )
            )
        )
    )
)

(about #_"DeftypeParser"
    (defn #_"IParser" DeftypeParser'new []
        (reify IParser
            ;; (deftype* tagname classname [fields] :implements [interfaces] :tag tagname methods*)
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (let [#_"seq" s form                                  s (next s)
                      #_"String" tagname (INamed'''getName (first s)) s (next s)
                      #_"Symbol" classname (first s)                  s (next s)
                      #_"vector" fields (first s)                     s (next s)
                      [#_"map" opts s]
                        (loop-when-recur [opts {} s s]
                                         (and (some? s) (keyword? (first s)))
                                         [(assoc opts (first s) (second s)) (next (next s))]
                                      => [opts s]
                        )]
                    (NewInstanceExpr'build (get opts :implements []), fields, nil, tagname, classname, (get opts :tag), s, form, opts)
                )
            )
        )
    )
)

(about #_"CaseExpr"
    (defr CaseExpr [])

    ;; (case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
    (defn #_"CaseExpr" CaseExpr'new [#_"int" line, #_"LocalBindingExpr" expr, #_"int" shift, #_"int" mask, #_"int" low, #_"int" high, #_"Expr" defaultExpr, #_"sorted {Integer Expr}" tests, #_"{Integer Expr}" thens, #_"Keyword" switchType, #_"Keyword" testType, #_"{Integer}" skipCheck]
        (when-not (any = switchType :compact :sparse)
            (throw! (str "unexpected switch type: " switchType))
        )
        (when-not (any = testType :int :hash-equiv :hash-identity)
            (throw! (str "unexpected test type: " testType))
        )
        (when (and (pos? (count skipCheck)) *warn-on-reflection*)
            (.println *err*, (str "Performance warning, line " line " - hash collision of some case test constants; if selected, those entries will be tested sequentially."))
        )
        (merge (CaseExpr'class.)
            (hash-map
                #_"LocalBindingExpr" :expr expr
                #_"int" :shift shift
                #_"int" :mask mask
                #_"int" :low low
                #_"int" :high high
                #_"Expr" :defaultExpr defaultExpr
                #_"sorted {Integer Expr}" :tests tests
                #_"{Integer Expr}" :thens thens
                #_"Keyword" :switchType switchType
                #_"Keyword" :testType testType
                #_"{Integer}" :skipCheck skipCheck
                #_"Class" :returnType (Compiler'maybeClass (conj (vec (vals thens)) defaultExpr))
                #_"int" :line line
            )
        )
    )

    (defn- #_"boolean" CaseExpr''isShiftMasked [#_"CaseExpr" this]
        (not= (:mask this) 0)
    )

    (defn- #_"void" CaseExpr''emitShiftMask [#_"CaseExpr" this, #_"GeneratorAdapter" gen]
        (when (CaseExpr''isShiftMasked this)
            (.push gen, (:shift this))
            (.visitInsn gen, Opcodes/ISHR)
            (.push gen, (:mask this))
            (.visitInsn gen, Opcodes/IAND)
        )
        nil
    )

    (defn- #_"void" CaseExpr''emitExprForInts [#_"CaseExpr" this, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Type" exprType, #_"Label" defaultLabel]
        (cond (nil? exprType)
            (do
                (when *warn-on-reflection*
                    (.println *err*, (str "Performance warning, line " (:line this) " - case has int tests, but tested expression is not primitive."))
                )
                (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
                (.instanceOf gen, (Type/getType Number))
                (.ifZCmp gen, GeneratorAdapter/EQ, defaultLabel)
                (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
                (.checkCast gen, (Type/getType Number))
                (.invokeVirtual gen, (Type/getType Number), (Method/getMethod "int intValue()"))
                (CaseExpr''emitShiftMask this, gen)
            )
            (any = exprType Type/LONG_TYPE Type/INT_TYPE Type/BYTE_TYPE)
            (do
                (MaybePrimitive'''emitUnboxed (:expr this), :Context'EXPRESSION, objx, gen)
                (.cast gen, exprType, Type/INT_TYPE)
                (CaseExpr''emitShiftMask this, gen)
            )
            :else
            (do
                (.goTo gen, defaultLabel)
            )
        )
        nil
    )

    (defn- #_"void" CaseExpr'emitExpr [#_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" expr, #_"boolean" emitUnboxed]
        (if (and emitUnboxed (satisfies? MaybePrimitive expr))
            (MaybePrimitive'''emitUnboxed expr, :Context'EXPRESSION, objx, gen)
            (Expr'''emit expr, :Context'EXPRESSION, objx, gen)
        )
        nil
    )

    (defn- #_"void" CaseExpr''emitThenForInts [#_"CaseExpr" this, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Type" exprType, #_"Expr" test, #_"Expr" then, #_"Label" defaultLabel, #_"boolean" emitUnboxed]
        (cond (nil? exprType)
            (do
                (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
                (Expr'''emit test, :Context'EXPRESSION, objx, gen)
                (.invokeStatic gen, (Type/getType Util'iface), (Method/getMethod "boolean equiv(Object, Object)"))
                (.ifZCmp gen, GeneratorAdapter/EQ, defaultLabel)
                (CaseExpr'emitExpr objx, gen, then, emitUnboxed)
            )
            (= exprType Type/LONG_TYPE)
            (do
                (MaybePrimitive'''emitUnboxed test, :Context'EXPRESSION, objx, gen)
                (MaybePrimitive'''emitUnboxed (:expr this), :Context'EXPRESSION, objx, gen)
                (.ifCmp gen, Type/LONG_TYPE, GeneratorAdapter/NE, defaultLabel)
                (CaseExpr'emitExpr objx, gen, then, emitUnboxed)
            )
            (any = exprType Type/INT_TYPE Type/BYTE_TYPE)
            (do
                (when (CaseExpr''isShiftMasked this)
                    (MaybePrimitive'''emitUnboxed test, :Context'EXPRESSION, objx, gen)
                    (MaybePrimitive'''emitUnboxed (:expr this), :Context'EXPRESSION, objx, gen)
                    (.cast gen, exprType, Type/LONG_TYPE)
                    (.ifCmp gen, Type/LONG_TYPE, GeneratorAdapter/NE, defaultLabel)
                )
                (CaseExpr'emitExpr objx, gen, then, emitUnboxed)
            )
            :else
            (do
                (.goTo gen, defaultLabel)
            )
        )
        nil
    )

    (defn- #_"void" CaseExpr''emitExprForHashes [#_"CaseExpr" this, #_"IopObject" objx, #_"GeneratorAdapter" gen]
        (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
        (.invokeStatic gen, (Type/getType Util'iface), (Method/getMethod "int hash(Object)"))
        (CaseExpr''emitShiftMask this, gen)
        nil
    )

    (defn- #_"void" CaseExpr''emitThenForHashes [#_"CaseExpr" this, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"Expr" test, #_"Expr" then, #_"Label" defaultLabel, #_"boolean" emitUnboxed]
        (Expr'''emit (:expr this), :Context'EXPRESSION, objx, gen)
        (Expr'''emit test, :Context'EXPRESSION, objx, gen)
        (if (= (:testType this) :hash-identity)
            (do
                (.visitJumpInsn gen, Opcodes/IF_ACMPNE, defaultLabel)
            )
            (do
                (.invokeStatic gen, (Type/getType Util'iface), (Method/getMethod "boolean equiv(Object, Object)"))
                (.ifZCmp gen, GeneratorAdapter/EQ, defaultLabel)
            )
        )
        (CaseExpr'emitExpr objx, gen, then, emitUnboxed)
        nil
    )

    (declare sorted-map)

    (defn- #_"void" CaseExpr''doEmit [#_"CaseExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen, #_"boolean" emitUnboxed]
        (let [#_"Label" defaultLabel (.newLabel gen) #_"Label" endLabel (.newLabel gen)
              #_"sorted {Integer Label}" labels (reduce! #(assoc! %1 %2 (.newLabel gen)) (sorted-map) (keys (:tests this)))]
            (.visitLineNumber gen, (:line this), (.mark gen))
            (let [#_"Class" primExprClass (Compiler'maybePrimitiveType (:expr this))
                  #_"Type" primExprType (when (some? primExprClass) (Type/getType primExprClass))]
                (if (= (:testType this) :int)
                    (CaseExpr''emitExprForInts this, objx, gen, primExprType, defaultLabel)
                    (CaseExpr''emitExprForHashes this, objx, gen)
                )
                (if (= (:switchType this) :sparse)
                    (let [#_"Label[]" la (into-array Label (vals labels))]
                        (.visitLookupSwitchInsn gen, defaultLabel, (-/int-array (keys (:tests this))), la)
                    )
                    (let [#_"Label[]" la (make-array Label (inc (- (:high this) (:low this))))]
                        (loop-when-recur [#_"int" i (:low this)] (<= i (:high this)) [(inc i)]
                            (aset! la (- i (:low this)) (if (contains? labels i) (get labels i) defaultLabel))
                        )
                        (.visitTableSwitchInsn gen, (:low this), (:high this), defaultLabel, la)
                    )
                )
                (doseq [#_"Integer" i (keys labels)]
                    (.mark gen, (get labels i))
                    (cond
                        (= (:testType this) :int)
                            (CaseExpr''emitThenForInts this, objx, gen, primExprType, (get (:tests this) i), (get (:thens this) i), defaultLabel, emitUnboxed)
                        (contains? (:skipCheck this) i)
                            (CaseExpr'emitExpr objx, gen, (get (:thens this) i), emitUnboxed)
                        :else
                            (CaseExpr''emitThenForHashes this, objx, gen, (get (:tests this) i), (get (:thens this) i), defaultLabel, emitUnboxed)
                    )
                    (.goTo gen, endLabel)
                )
                (.mark gen, defaultLabel)
                (CaseExpr'emitExpr objx, gen, (:defaultExpr this), emitUnboxed)
                (.mark gen, endLabel)
                (when (= context :Context'STATEMENT)
                    (.pop gen)
                )
            )
        )
        nil
    )

    (defm CaseExpr Expr
        (#_"Object" Expr'''eval [#_"CaseExpr" this]
            (throw! "can't eval case")
        )

        (#_"void" Expr'''emit [#_"CaseExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (CaseExpr''doEmit this, context, objx, gen, false)
            nil
        )

        (#_"Class" Expr'''getClass => :returnType)
    )

    (defm CaseExpr MaybePrimitive
        (#_"boolean" MaybePrimitive'''canEmitPrimitive [#_"CaseExpr" this]
            (Reflector'isPrimitive (:returnType this))
        )

        (#_"void" MaybePrimitive'''emitUnboxed [#_"CaseExpr" this, #_"Context" context, #_"IopObject" objx, #_"GeneratorAdapter" gen]
            (CaseExpr''doEmit this, context, objx, gen, true)
            nil
        )
    )
)

(about #_"CaseParser"
    (defn #_"IParser" CaseParser'new []
        (reify IParser
            ;; (case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
            ;; prepared by case macro and presumed correct
            ;; case macro binds actual expr in let so expr is always a local,
            ;; no need to worry about multiple evaluation
            (#_"Expr" IParser'''parse [#_"IParser" _self, #_"Context" context, #_"seq" form]
                (if (= context :Context'EVAL)
                    (Compiler'analyze context, (list (list Compiler'FNONCE [] form)))
                    (let [#_"vector" args (vec (next form))
                          #_"Object" exprForm (nth args 0)
                          #_"int" shift (.intValue (nth args 1))
                          #_"int" mask (.intValue (nth args 2))
                          #_"Object" defaultForm (nth args 3)
                          #_"map" caseMap (nth args 4)
                          #_"Keyword" switchType (nth args 5)
                          #_"Keyword" testType (nth args 6)
                          #_"IPersistentSet" skipCheck (when (< 7 (count args)) (nth args 7))
                          #_"seq" keys (keys caseMap)
                          #_"int" low (.intValue (first keys))
                          #_"int" high (.intValue (nth keys (dec (count keys))))
                          #_"LocalBindingExpr" testExpr (Compiler'analyze :Context'EXPRESSION, exprForm)
                          [#_"sorted {Integer Expr}" tests #_"{Integer Expr}" thens]
                            (loop-when [tests (sorted-map) thens {} #_"seq" s (seq caseMap)] (some? s) => [tests thens]
                                (let [#_"IMapEntry" e (first s)
                                      #_"Integer" minhash (.intValue (key e)) #_"Object" pair (val e) ;; [test-val then-expr]
                                      #_"Expr" test (if (= testType :int) (NumberExpr'parse (.intValue (first pair))) (ConstantExpr'new (first pair)))
                                      #_"Expr" then (Compiler'analyze context, (second pair))]
                                    (recur (assoc tests minhash test) (assoc thens minhash then) (next s))
                                )
                            )
                          #_"Expr" defaultExpr (Compiler'analyze context, (nth args 3))]
                        (CaseExpr'new *line*, testExpr, shift, mask, low, high, defaultExpr, tests, thens, switchType, testType, skipCheck)
                    )
                )
            )
        )
    )
)

(about #_"Compiler"
    (def #_"map" Compiler'specials
        (hash-map
            'def           (DefParser'new)
            'loop*         (LetParser'new)
            'recur         (RecurParser'new)
            'if            (IfParser'new)
            'case*         (CaseParser'new)
            'let*          (LetParser'new)
            'letfn*        (LetFnParser'new)
            'do            (BodyParser'new)
            'fn*           nil
            'quote         (ConstantParser'new)
            'var           (TheVarParser'new)
            'import*       (ImportParser'new)
            '.             (HostParser'new)
            'set!          (AssignParser'new)
            'deftype*      (DeftypeParser'new)
            'reify*        (ReifyParser'new)
            'try           (TryParser'new)
            'throw         (ThrowParser'new)
            'monitor-enter (MonitorEnterParser'new)
            'monitor-exit  (MonitorExitParser'new)
            'catch         nil
            'finally       nil
            'new           (NewParser'new)
            '&             nil
        )
    )

    (defn #_"boolean" Compiler'isSpecial [#_"Object" sym]
        (contains? Compiler'specials sym)
    )

;;;
 ; Returns true if s names a special form.
 ;;
(defn special-symbol? [s] (contains? Compiler'specials s))

    (defn #_"Object" Compiler'macroexpand1 [#_"Object" form]
        (when (seq? form) => form
            (let-when [#_"Object" op (first form)] (not (Compiler'isSpecial op)) => form
                (let-when [#_"Var" v (Compiler'isMacro op)] (nil? v) => (apply v form *local-env* (next form)) ;; macro expansion
                    (when (symbol? op) => form
                        (let [#_"String" n (:name op)]
                            ;; (.substring s 2 5) => (. s substring 2 5)
                            (cond
                                (= (nth n 0) \.)
                                    (when (< 1 (count form)) => (throw! "malformed member expression, expecting (.member target ...)")
                                        (let [#_"Object" target (second form)
                                              target
                                                (when (some? (Interop'maybeClass target, false)) => target
                                                    (with-meta (list `identity target) {:tag 'java.lang.Class})
                                                )]
                                            (Compiler'preserveTag form, (list* '. target (symbol (.substring n, 1)) (next (next form))))
                                        )
                                    )
                                (Compiler'namesStaticMember op)
                                    (let-when [#_"Symbol" target (symbol (:ns op))] (some? (Interop'maybeClass target, false)) => form
                                        (Compiler'preserveTag form, (list* '. target (symbol n) (next form)))
                                    )
                                :else
                                    ;; (s.substring ...) => (. s substring ...)
                                    ;; (package.class.name ...) => (. package.class name ...)
                                    ;; (StringBuilder. ...) => (new StringBuilder ...)
                                    (let-when [#_"int" i (.lastIndexOf n, (int \.))] (= i (dec (count n))) => form
                                        (list* 'new (symbol (.substring n, 0, i)) (next form))
                                    )
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"Object" Compiler'macroexpand [#_"Object" form]
        (let [#_"Object" f (Compiler'macroexpand1 form)]
            (if (= f form) form (recur f))
        )
    )

    (defn- #_"Expr" Compiler'analyzeSymbol [#_"Symbol" sym]
        (let [#_"Symbol" tag (Compiler'tagOf sym)]
            (or
                (cond
                    (nil? (:ns sym)) ;; ns-qualified syms are always Vars
                        (when-some [#_"LocalBinding" lb (Compiler'referenceLocal sym)]
                            (LocalBindingExpr'new lb, tag)
                        )
                    (nil? (Compiler'namespaceFor sym))
                        (when-some [#_"Class" c (Interop'maybeClass (symbol (:ns sym)), false)]
                            (when (some? (Reflector'getField c, (:name sym), true)) => (throw! (str "unable to find static field: " (:name sym) " in " c))
                                (StaticFieldExpr'new *line*, c, (:name sym), tag)
                            )
                        )
                )
                (let [#_"Object" o (Compiler'resolve sym)]
                    (cond
                        (var? o)
                            (when (nil? (Compiler'isMacro o)) => (throw! (str "can't take value of a macro: " o))
                                (Compiler'registerVar o)
                                (VarExpr'new o, tag)
                            )
                        (class? o)
                            (ConstantExpr'new o)
                        (symbol? o)
                            (UnresolvedVarExpr'new o)
                        :else
                            (throw! (str "unable to resolve symbol: " sym " in this context"))
                    )
                )
            )
        )
    )

    (defn- #_"KeywordExpr" Compiler'registerKeyword [#_"Keyword" k]
        (when (bound? #'*keywords*)
            (let-when [#_"map" m *keywords*] (nil? (get m k))
                (set! *keywords* (assoc m k (Compiler'registerConstant k)))
            )
        )
        (KeywordExpr'new k)
    )

    (defn- #_"Expr" Compiler'analyzeSeq [#_"Context" context, #_"seq" form, #_"String" name]
        (let [#_"meta" meta (meta form)]
            (binding [*line* (if (contains? meta :line) (get meta :line) *line*)]
                (let-when [#_"Object" me (Compiler'macroexpand1 form)] (= me form) => (Compiler'analyze context, me, name)
                    (let-when [#_"Object" op (first form)] (some? op) => (throw! (str "can't call nil, form: " form))
                        (let [#_"IFn" inline (Compiler'isInline op, (count (next form)))]
                            (cond
                                (some? inline)
                                    (Compiler'analyze context, (Compiler'preserveTag form, (IFn'''applyTo inline, (next form))))
                                (= op 'fn*)
                                    (FnExpr'parse context, form, name)
                                :else
                                    (let [#_"IParser" p (get Compiler'specials op)]
                                        (if (some? p)
                                            (IParser'''parse p, context, form)
                                            (InvokeExpr'parse context, form)
                                        )
                                    )
                            )
                        )
                    )
                )
            )
        )
    )

    (defn #_"Expr" Compiler'analyze
        ([#_"Context" context, #_"Object" form] (Compiler'analyze context, form, nil))
        ([#_"Context" context, #_"Object" form, #_"String" name]
            (let [form
                    (when (satisfies? LazySeq form) => form
                        (with-meta (or (seq form) ()) (meta form))
                    )]
                (case form
                    nil                 Compiler'NIL_EXPR
                    true                Compiler'TRUE_EXPR
                    false               Compiler'FALSE_EXPR
                    (cond
                        (symbol? form)  (Compiler'analyzeSymbol form)
                        (keyword? form) (Compiler'registerKeyword form)
                        (number? form)  (NumberExpr'parse form)
                        (string? form)  (StringExpr'new (.intern #_"String" form))
                        (and (coll? form) (not (record? form)) (not (type? form)) (zero? (count form)))
                            (let-when [#_"Expr" e (EmptyExpr'new form)] (some? (meta form)) => e
                                (MetaExpr'new e, (MapExpr'parse (if (= context :Context'EVAL) context :Context'EXPRESSION), (meta form)))
                            )
                        (seq? form)     (Compiler'analyzeSeq context, form, name)
                        (vector? form)  (VectorExpr'parse context, form)
                        (map? form)     (MapExpr'parse context, form)
                        (set? form)     (SetExpr'parse context, form)
                        :else           (ConstantExpr'new form)
                    )
                )
            )
        )
    )

    (defn #_"Object" Compiler'eval [#_"Object" form]
        (let [#_"meta" meta (meta form)]
            (binding [*class-loader* (Loader'create), *line* (if (contains? meta :line) (get meta :line) *line*)]
                (let [form (Compiler'macroexpand form)]
                    (cond
                        (and (seq? form) (= (first form) 'do))
                            (loop-when-recur [#_"seq" s (next form)] (some? (next s)) [(next s)] => (Compiler'eval (first s))
                                (Compiler'eval (first s))
                            )
                        (or (type? form) (and (coll? form) (not (and (symbol? (first form)) (.startsWith (:name (first form)), "def")))))
                            (let [#_"IopObject" fexpr (Compiler'analyze :Context'EXPRESSION, (list 'fn* [] form), (str "eval" (next-id!)))]
                                (IFn'''invoke (Expr'''eval fexpr))
                            )
                        :else
                            (let [#_"Expr" expr (Compiler'analyze :Context'EVAL, form)]
                                (Expr'''eval expr)
                            )
                    )
                )
            )
        )
    )
)
)

(about #_"cloiure.core.LispReader"

(about #_"LispReader"
    (def #_"Var" ^:dynamic *arg-env*   ) ;; sorted-map num->gensymbol
    (def #_"Var" ^:dynamic *gensym-env*) ;; symbol->gensymbol

    (defn #_"Symbol" LispReader'garg [#_"int" n]
        (symbol (str (if (= n -1) "rest" (str "p" n)) "__" (next-id!) "#"))
    )

    (defn #_"Symbol" LispReader'registerArg [#_"int" n]
        (when (bound? #'*arg-env*) => (throw! "arg literal not in #()")
            (or (get *arg-env* n)
                (let [#_"Symbol" sym (LispReader'garg n)]
                    (update! *arg-env* assoc n sym)
                    sym
                )
            )
        )
    )

    (defn #_"Symbol" LispReader'registerGensym [#_"Symbol" sym]
        (when (bound? #'*gensym-env*) => (throw! "gensym literal not in syntax-quote")
            (or (get *gensym-env* sym)
                (let [#_"Symbol" gsym (symbol (str (:name sym) "__" (next-id!) "__auto__"))]
                    (update! *gensym-env* assoc sym gsym)
                    gsym
                )
            )
        )
    )

    (declare LispReader'macros)

    (defn- #_"boolean" LispReader'isMacro [#_"char" ch]
        (contains? LispReader'macros ch)
    )

    (defn- #_"boolean" LispReader'isTerminatingMacro [#_"char" ch]
        (and (LispReader'isMacro ch) (not (any = ch \# \' \%)))
    )

    (defn #_"boolean" LispReader'isDigit [#_"char" ch, #_"int" base]
        (not= (Character/digit ch, base) -1)
    )

    (defn #_"boolean" LispReader'isWhitespace [#_"char" ch]
        (or (Character/isWhitespace ch) (= ch \,))
    )

    (defn #_"Character" LispReader'read1 [#_"Reader" r]
        (let [#_"int" c (.read r)]
            (when-not (= c -1)
                (char c)
            )
        )
    )

    (defn #_"void" LispReader'unread [#_"PushbackReader" r, #_"Character" ch]
        (when (some? ch)
            (.unread r, (int ch))
        )
        nil
    )

    (defn- #_"void" LispReader'consumeWhitespaces [#_"PushbackReader" r]
        (loop-when-recur [#_"char" ch (LispReader'read1 r)] (LispReader'isWhitespace ch) [(LispReader'read1 r)] => (LispReader'unread r, ch))
        nil
    )

    (def- #_"Pattern" LispReader'rxInteger #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)")
    (def- #_"Pattern" LispReader'rxRatio   #"([-+]?[0-9]+)/([0-9]+)")

    (declare Numbers'divide)

    (defn- #_"Object" LispReader'matchNumber [#_"String" s]
        (let [_ (or
                    (let-when [#_"Matcher" m (.matcher LispReader'rxInteger, s)] (.matches m)
                        (when (nil? (.group m, 2)) => (Long/valueOf 0)
                            (let [[#_"String" n #_"int" radix]
                                    (cond-some
                                        [n (.group m, 3)] [n 10]
                                        [n (.group m, 4)] [n 16]
                                        [n (.group m, 5)] [n 8]
                                        [n (.group m, 7)] [n (Integer/parseInt (.group m, 6))]
                                    )]
                                (when (some? n) => :nil
                                    (let [#_"BigInteger" bn (BigInteger. n, radix) bn (if (= (.group m, 1) "-") (.negate bn) bn)]
                                        (when (< (.bitLength bn) 64) => bn
                                            (Long/valueOf (.longValue bn))
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (let-when [#_"Matcher" m (.matcher LispReader'rxRatio, s)] (.matches m)
                        (let [#_"String" n (.group m, 1) n (if (.startsWith n, "+") (.substring n, 1) n)]
                            (Numbers'divide (BigInteger. n), (BigInteger. (.group m, 2)))
                        )
                    )
                )]
            (when-not (= _ :nil) _)
        )
    )

    (defn- #_"Object" LispReader'readNumber [#_"PushbackReader" r, #_"char" ch]
        (let [#_"String" s
                (let [#_"StringBuilder" sb (StringBuilder.) _ (.append sb, ch)]
                    (loop []
                        (let [ch (LispReader'read1 r)]
                            (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isMacro ch))
                                (do
                                    (LispReader'unread r, ch)
                                    (.toString sb)
                                )
                                (do
                                    (.append sb, ch)
                                    (recur)
                                )
                            )
                        )
                    )
                )]
            (or (LispReader'matchNumber s) (throw! (str "invalid number: " s)))
        )
    )

    (defn- #_"String" LispReader'readToken [#_"PushbackReader" r, #_"char" ch]
        (let [#_"StringBuilder" sb (StringBuilder.) _ (.append sb, ch)]
            (loop []
                (let [ch (LispReader'read1 r)]
                    (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isTerminatingMacro ch))
                        (do
                            (LispReader'unread r, ch)
                            (.toString sb)
                        )
                        (do
                            (.append sb, ch)
                            (recur)
                        )
                    )
                )
            )
        )
    )

    (def- #_"Pattern" LispReader'rxSymbol #"[:]?([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)")

    (defn- #_"Object" LispReader'matchSymbol [#_"String" s]
        (let-when [#_"Matcher" m (.matcher LispReader'rxSymbol, s)] (.matches m)
            (let [#_"String" ns (.group m, 1) #_"String" n (.group m, 2)]
                (cond
                    (or (and (some? ns) (.endsWith ns, ":/")) (.endsWith n, ":") (not= (.indexOf s, "::", 1) -1))
                        nil
                    (.startsWith s, "::")
                        (let [#_"Symbol" ks (symbol (.substring s, 2))
                              #_"Namespace" kns (if (some? (:ns ks)) (Namespace''getAlias *ns*, (symbol (:ns ks))) *ns*)]
                            ;; auto-resolving keyword
                            (when (some? kns)
                                (keyword (:name (:name kns)) (:name ks))
                            )
                        )
                    :else
                        (let [#_"boolean" kw? (= (nth s 0) \:) #_"Symbol" sym (symbol (.substring s, (if kw? 1 0)))]
                            (if kw? (keyword sym) sym)
                        )
                )
            )
        )
    )

    (defn- #_"Object" LispReader'interpretToken [#_"String" s]
        (case s "nil" nil "true" true "false" false
            (or (LispReader'matchSymbol s) (throw! (str "invalid token: " s)))
        )
    )

    (defn #_"Object" LispReader'read
        ([#_"PushbackReader" r] (LispReader'read r, true, nil))
        ([#_"PushbackReader" r, #_"boolean" eofIsError, #_"Object" eofValue] (LispReader'read r, eofIsError, eofValue, nil, nil))
        ([#_"PushbackReader" r, #_"boolean" eofIsError, #_"Object" eofValue, #_"Character" returnOn, #_"Object" returnOnValue]
            (loop []
                (let [#_"char" ch (loop-when-recur [ch (LispReader'read1 r)] (LispReader'isWhitespace ch) [(LispReader'read1 r)] => ch)]
                    (cond
                        (nil? ch)
                            (if eofIsError (throw! "EOF while reading") eofValue)
                        (and (some? returnOn) (= returnOn ch))
                            returnOnValue
                        (LispReader'isDigit ch, 10)
                            (LispReader'readNumber r, ch)
                        :else
                            (let [#_"IFn" fn (get LispReader'macros ch)]
                                (if (some? fn)
                                    (let [#_"Object" o (fn r ch)]
                                        ;; no op macros return the reader
                                        (recur-when (identical? o r) [] => o)
                                    )
                                    (or
                                        (when (any = ch \+ \-)
                                            (let [#_"char" ch' (LispReader'read1 r) ? (LispReader'isDigit ch', 10)]
                                                (LispReader'unread r, ch')
                                                (when ?
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

    (defn- #_"int" LispReader'scanDigits [#_"String" token, #_"int" offset, #_"int" n, #_"int" base]
        (when (= (+ offset n) (count token)) => (throw! (str "invalid unicode character: \\" token))
            (loop-when [#_"int" c 0 #_"int" i 0] (< i n) => c
                (let [#_"char" ch (nth token (+ offset i)) #_"int" d (Character/digit ch, base)]
                    (when-not (= d -1) => (throw! (str "invalid digit: " ch))
                        (recur (+ (* c base) d) (inc i))
                    )
                )
            )
        )
    )

    (defn- #_"int" LispReader'readDigits [#_"PushbackReader" r, #_"char" ch, #_"int" base, #_"int" n, #_"boolean" exact?]
        (let-when-not [#_"int" c (Character/digit ch, base)] (= c -1) => (throw! (str "invalid digit: " ch))
            (let [[c #_"int" i]
                    (loop-when [c c i 1] (< i n) => [c i]
                        (let [ch (LispReader'read1 r)]
                            (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isMacro ch))
                                (do
                                    (LispReader'unread r, ch)
                                    [c i]
                                )
                                (let [#_"int" d (Character/digit ch, base)]
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

    (def- #_"Object" LispReader'READ_EOF (Object.))
    (def- #_"Object" LispReader'READ_FINISHED (Object.))

    (defn #_"vector" LispReader'readDelimitedForms [#_"PushbackReader" r, #_"char" delim]
        (loop [#_"vector" v []]
            (let [#_"Object" form (LispReader'read r, false, LispReader'READ_EOF, delim, LispReader'READ_FINISHED)]
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
    (defn #_"Object" regex-reader [#_"PushbackReader" r, #_"char" _delim]
        (let [#_"StringBuilder" sb (StringBuilder.)]
            (loop []
                (let-when [#_"char" ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading regex")
                    (when-not (= ch \") ;; oops! "
                        (.append sb, ch)
                        (when (= ch \\) ;; escape
                            (let-when [ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading regex")
                                (.append sb, ch)
                            )
                        )
                        (recur)
                    )
                )
            )
            (Pattern/compile (.toString sb))
        )
    )
)

(about #_"StringReader"
    (defn- #_"char" StringReader'escape [#_"PushbackReader" r]
        (let-when [#_"char" ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading string")
            (case ch
                \t  \tab
                \r  \return
                \n  \newline
                \\  ch
                \"  ch ;; oops! "
                \b  \backspace
                \f  \formfeed
                \u  (let [ch (LispReader'read1 r)]
                        (when (LispReader'isDigit ch, 16) => (throw! (str "invalid unicode escape: \\u" ch))
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

    (defn #_"Object" string-reader [#_"PushbackReader" r, #_"char" _delim]
        (let [#_"StringBuilder" sb (StringBuilder.)]
            (loop []
                (let-when [#_"char" ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading string")
                    (when-not (= ch \") ;; oops! "
                        (.append sb, (if (= ch \\) (StringReader'escape r) ch))
                        (recur)
                    )
                )
            )
            (.toString sb)
        )
    )
)

(about #_"CommentReader"
    (defn #_"Object" comment-reader [#_"PushbackReader" r, #_"char" _delim]
        (while (not (any = (LispReader'read1 r) nil \newline \return)))
        r
    )
)

(about #_"DiscardReader"
    (defn #_"Object" discard-reader [#_"PushbackReader" r, #_"char" _delim]
        (LispReader'read r)
        r
    )
)

(about #_"QuoteReader"
    (defn #_"Object" quote-reader [#_"PushbackReader" r, #_"char" _delim]
        (list 'quote (LispReader'read r))
    )
)

(about #_"DerefReader"
    (defn #_"Object" deref-reader [#_"PushbackReader" r, #_"char" _delim]
        (list `deref (LispReader'read r))
    )
)

(about #_"VarReader"
    (defn #_"Object" var-reader [#_"PushbackReader" r, #_"char" _delim]
        (list 'var (LispReader'read r))
    )
)

(about #_"DispatchReader"
    (declare LispReader'dispatchMacros)

    (defn #_"Object" dispatch-reader [#_"PushbackReader" r, #_"char" _delim]
        (let-when [#_"char" ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading character")
            (let-when [#_"IFn" fn (get LispReader'dispatchMacros ch)] (nil? fn) => (fn r ch)
                (LispReader'unread r, ch)
                (throw! (str "no dispatch macro for: " ch))
            )
        )
    )
)

(about #_"FnReader"
    (defn #_"Object" fn-reader [#_"PushbackReader" r, #_"char" _delim]
        (when-not (bound? #'*arg-env*) => (throw! "nested #()s are not allowed")
            (binding [*arg-env* (sorted-map)]
                (LispReader'unread r, \()
                (let [#_"vector" args []
                      args
                        (let-when [#_"seq" rs (rseq *arg-env*)] (some? rs) => args
                            (let [args
                                    (let-when [#_"int" n (key (first rs))] (pos? n) => args
                                        (loop-when-recur [args args #_"int" i 1]
                                                         (<= i n)
                                                         [(conj args (or (get *arg-env* i) (LispReader'garg i))) (inc i)]
                                                      => args
                                        )
                                    )]
                                (let-when [#_"Object" rest (get *arg-env* -1)] (some? rest) => args
                                    (conj args '& rest)
                                )
                            )
                        )]
                    (list 'fn* args (LispReader'read r))
                )
            )
        )
    )
)

(about #_"ArgReader"
    (defn #_"Object" arg-reader [#_"PushbackReader" r, #_"char" _delim]
        (when (bound? #'*arg-env*) => (LispReader'interpretToken (LispReader'readToken r, \%))
            (let [#_"char" ch (LispReader'read1 r) _ (LispReader'unread r, ch)]
                ;; % alone is first arg
                (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isTerminatingMacro ch))
                    (LispReader'registerArg 1)
                    (let [#_"Object" n (LispReader'read r)]
                        (cond
                            (= n '&)    (LispReader'registerArg -1)
                            (number? n) (LispReader'registerArg (.intValue #_"Number" n))
                            :else       (throw! "arg literal must be %, %& or %integer")
                        )
                    )
                )
            )
        )
    )
)

(about #_"MetaReader"
    (defn #_"Object" meta-reader [#_"PushbackReader" r, #_"char" _delim]
        (let [#_"Object" _meta (LispReader'read r)
              _meta
                (cond
                    (or (symbol? _meta) (string? _meta)) {:tag _meta}
                    (keyword? _meta)                          {_meta true}
                    (map? _meta)                               _meta
                    :else (throw! "metadata must be Symbol, Keyword, String or Map")
                )
              #_"Object" o (LispReader'read r)]
            (when (satisfies? IMeta o) => (throw! "metadata can only be applied to IMetas")
                (if (satisfies? IReference o)
                    (do
                        (reset-meta! o _meta)
                        o
                    )
                    (let [#_"meta" m
                            (loop-when [m (meta o) #_"seq" s (seq _meta)] (some? s) => m
                                (let [#_"IMapEntry" e (first s)]
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
    (defn- #_"vector" SyntaxQuoteReader'flattened [#_"map" m]
        (loop-when [#_"vector" v [] #_"seq" s (seq m)] (some? s) => v
            (let [#_"IMapEntry" e (first s)]
                (recur (conj v (key e) (val e)) (next s))
            )
        )
    )

    (defn #_"boolean" SyntaxQuoteReader'isUnquote [#_"Object" form]
        (and (seq? form) (= (first form) `unquote))
    )

    (defn #_"boolean" SyntaxQuoteReader'isUnquoteSplicing [#_"Object" form]
        (and (seq? form) (= (first form) `unquote-splicing))
    )

    (declare SyntaxQuoteReader'syntaxQuote)

    (defn- #_"seq" SyntaxQuoteReader'sqExpandList [#_"seq" s]
        (loop-when [#_"vector" v [] s s] (some? s) => (seq v)
            (let [#_"Object" item (first s)
                  v (cond
                        (SyntaxQuoteReader'isUnquote item)         (conj v (list `list (second item)))
                        (SyntaxQuoteReader'isUnquoteSplicing item) (conj v (second item))
                        :else                                      (conj v (list `list (SyntaxQuoteReader'syntaxQuote item)))
                    )]
                (recur v (next s))
            )
        )
    )

    (defn #_"Object" SyntaxQuoteReader'syntaxQuote [#_"Object" form]
        (let [#_"Object" q
                (cond
                    (Compiler'isSpecial form)
                        (list 'quote form)
                    (symbol? form)
                        (let [#_"String" ns (:ns form) #_"String" n (:name form)
                              form
                                (cond
                                    (and (nil? ns) (.endsWith n, "#"))
                                        (LispReader'registerGensym (symbol (.substring n, 0, (dec (count n)))))
                                    (and (nil? ns) (.endsWith n, "."))
                                        (symbol (str (:name (Compiler'resolveSymbol (symbol (.substring n, 0, (dec (count n)))))) "."))
                                    (and (nil? ns) (.startsWith n, "."))
                                        form ;; simply quote method names
                                    :else
                                        (let-when [#_"Object" c (when (some? ns) (Namespace''getMapping *ns*, (symbol ns)))] (class? c) => (Compiler'resolveSymbol form)
                                            ;; Classname/foo -> package.qualified.Classname/foo
                                            (symbol (.getName c) n)
                                        )
                                )]
                            (list 'quote form)
                        )
                    (SyntaxQuoteReader'isUnquote form)
                        (second form)
                    (SyntaxQuoteReader'isUnquoteSplicing form)
                        (throw! "splice not in list")
                    (coll? form)
                        (cond
                            (record? form)
                                form
                            (map? form)
                                (list `apply `hash-map (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList (seq (SyntaxQuoteReader'flattened form))))))
                            (vector? form)
                                (list `apply `vector (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList (seq form)))))
                            (set? form)
                                (list `apply `hash-set (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList (seq form)))))
                            (or (seq? form) (list? form))
                                (let-when [#_"seq" s (seq form)] (some? s) => (cons `list nil)
                                    (list `seq (cons `concat (SyntaxQuoteReader'sqExpandList s)))
                                )
                            :else
                                (throw! "unknown collection type")
                        )
                    (or (keyword? form) (number? form) (char? form) (string? form))
                        form
                    :else
                        (list 'quote form)
                )]
            (when (and (satisfies? IObj form) (seq (dissoc (meta form) :line :column)) (not (SyntaxQuoteReader'isUnquote form))) => q
                (list `with-meta q (SyntaxQuoteReader'syntaxQuote (meta form)))
            )
        )
    )

    (defn #_"Object" syntax-quote-reader [#_"PushbackReader" r, #_"char" _delim]
        (binding [*gensym-env* {}]
            (SyntaxQuoteReader'syntaxQuote (LispReader'read r))
        )
    )
)

(about #_"UnquoteReader"
    (defn #_"Object" unquote-reader [#_"PushbackReader" r, #_"char" _delim]
        (let-when [#_"char" ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading character")
            (if (= ch \@)
                (list `unquote-splicing (LispReader'read r))
                (do
                    (LispReader'unread r, ch)
                    (list `unquote (LispReader'read r))
                )
            )
        )
    )
)

(about #_"CharacterReader"
    (defn #_"Object" character-reader [#_"PushbackReader" r, #_"char" _delim]
        (let-when [#_"char" ch (LispReader'read1 r)] (some? ch) => (throw! "EOF while reading character")
            (let [#_"String" token (LispReader'readToken r, ch)]
                (when-not (= (count token) 1) => (Character/valueOf (nth token 0))
                    (case token
                        "newline"   \newline
                        "space"     \space
                        "tab"       \tab
                        "backspace" \backspace
                        "formfeed"  \formfeed
                        "return"    \return
                        (case (nth token 0)
                            \u  (let [#_"int" c (LispReader'scanDigits token, 1, 4, 16)]
                                    (when (<= 0xd800 c 0xdfff) ;; surrogate code unit?
                                        (throw! (str "invalid character constant: \\u" (Integer/toString c, 16)))
                                    )
                                    (char c)
                                )
                            \o  (let [#_"int" n (dec (count token))]
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
    (defn #_"Object" list-reader [#_"PushbackReader" r, #_"char" _delim]
        (let-when [#_"vector" v (LispReader'readDelimitedForms r, \))] (seq v) => ()
            (ß PersistentList/create #_(to-array v) v)
        )
    )
)

(about #_"VectorReader"
    (defn #_"Object" vector-reader [#_"PushbackReader" r, #_"char" _delim]
        (identity (LispReader'readDelimitedForms r, \]))
    )
)

(about #_"MapReader"
    (defn #_"Object" map-reader [#_"PushbackReader" r, #_"char" _delim]
        (let [#_"vector" v (LispReader'readDelimitedForms r, \})]
            (when (even? (count v)) => (throw! "map literal must contain an even number of forms")
                (RT'map (to-array v))
            )
        )
    )
)

(declare PersistentHashSet'createWithCheck-1s)

(about #_"SetReader"
    (defn #_"Object" set-reader [#_"PushbackReader" r, #_"char" _delim]
        (PersistentHashSet'createWithCheck-1s (LispReader'readDelimitedForms r, \}))
    )
)

(about #_"UnmatchedDelimiterReader"
    (defn #_"Object" unmatched-delimiter-reader [#_"PushbackReader" _r, #_"char" delim]
        (throw! (str "unmatched delimiter: " delim))
    )
)

(about #_"LispReader"
    (def #_"{char IFn}" LispReader'macros
        (hash-map
            \"  string-reader ;; oops! "
            \;  comment-reader
            \'  quote-reader
            \@  deref-reader
            \^  meta-reader
            \`  syntax-quote-reader
            \~  unquote-reader
            \(  list-reader,    \) unmatched-delimiter-reader
            \[  vector-reader,  \] unmatched-delimiter-reader
            \{  map-reader,     \} unmatched-delimiter-reader
            \\  character-reader
            \%  arg-reader
            \#  dispatch-reader
        )
    )

    (def #_"{char IFn}" LispReader'dispatchMacros
        (hash-map
            \^  meta-reader
            \'  var-reader
            \"  regex-reader ;; oops! "
            \(  fn-reader
            \{  set-reader
            \!  comment-reader
            \_  discard-reader
        )
    )
)
)

(about #_"cloiure.core.Compiler"

(about #_"Compiler"
    (defn #_"Object" Compiler'load [#_"Reader" reader]
        (let [#_"PushbackReader" r (if (instance? PushbackReader reader) reader (PushbackReader. reader))
              #_"Object" EOF (Object.)]
            (binding [*ns* *ns*, *warn-on-reflection* *warn-on-reflection*, *line* 0]
                (loop [#_"Object" val nil]
                    (LispReader'consumeWhitespaces r)
                    (let-when-not [#_"Object" form (LispReader'read r, false, EOF)] (identical? form EOF) => val
                        (recur
                            (binding [*last-unique-id*     -1
                                      *closes*             {}
                                      *no-recur*           false
                                      *in-catch-finally*   false
                                      *in-return-context*  false
                                      *compile-stub-sym*   nil
                                      *compile-stub-class* nil]
                                (Compiler'eval form)
                            )
                        )
                    )
                )
            )
        )
    )
)
)

;;;
 ; Evaluates the form data structure (not text!) and returns the result.
 ;;
(defn eval [form] (Compiler'eval form))

(about #_"cloiure.core.Murmur3"

;;;
 ; See http://smhasher.googlecode.com/svn/trunk/MurmurHash3.cpp
 ; MurmurHash3_x86_32
 ;
 ; @author Austin Appleby
 ; @author Dimitris Andreou
 ; @author Kurt Alfred Kluever
 ;;
(about #_"Murmur3"
    (def- #_"int" Murmur3'seed 0)
    (def- #_"int" Murmur3'C1 0xcc9e2d51)
    (def- #_"int" Murmur3'C2 0x1b873593)

    (defn- #_"int" Murmur3'mixK1 [#_"int" k1]
        (-> k1 (* Murmur3'C1) (Integer/rotateLeft 15) (* Murmur3'C2))
    )

    (defn- #_"int" Murmur3'mixH1 [#_"int" h1, #_"int" k1]
        (-> h1 (bit-xor k1) (Integer/rotateLeft 13) (* 5) (+ 0xe6546b64))
    )

    ;; finalization mix - force all bits of a hash block to avalanche
    (defn- #_"int" Murmur3'fmix [#_"int" h1, #_"int" n]
        (let [h1 (bit-xor h1 n)    h1 (bit-xor h1 (>>> h1 16))
              h1 (* h1 0x85ebca6b) h1 (bit-xor h1 (>>> h1 13))
              h1 (* h1 0xc2b2ae35) h1 (bit-xor h1 (>>> h1 16))]
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

    (defn #_"int" Murmur3'hashLong [#_"long" input]
        (when-not (zero? input) => 0
            (let [#_"int" low (int input)
                  #_"int" high (int (>>> input 32))
                  #_"int" k1 (Murmur3'mixK1 low)
                  #_"int" h1 (Murmur3'mixH1 Murmur3'seed, k1)
                  k1 (Murmur3'mixK1 high)
                  h1 (Murmur3'mixH1 h1, k1)]
                (Murmur3'fmix h1, 8)
            )
        )
    )

    (declare odd?)

    (defn #_"int" Murmur3'hashUnencodedChars [#_"CharSequence" s]
        (let [#_"int" h1 ;; step through the input 2 chars at a time
                (loop-when [h1 Murmur3'seed #_"int" i 1] (< i (.length s)) => h1
                    (let [#_"int" k1 (| (.charAt s, (dec i)) (<< (.charAt s, i) 16))]
                        (recur (Murmur3'mixH1 h1, (Murmur3'mixK1 k1)) (+ i 2))
                    )
                )
              h1 ;; deal with any remaining characters
                (when (odd? (.length s)) => h1
                    (let [#_"int" k1 (.charAt s, (dec (.length s)))]
                        (bit-xor h1 (Murmur3'mixK1 k1))
                    )
                )]
            (Murmur3'fmix h1, (* 2 (.length s)))
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

;;;
 ; Mix final collection hash for ordered or unordered collections.
 ; hash-basis is the combined collection hash, n is the number
 ; of elements included in the basis. Note this is the hash code
 ; consistent with =, different from .hashCode.
 ; See http://clojure.org/data_structures#hash for full algorithms.
 ;;
(defn #_"long" mix-collection-hash [#_"long" hash-basis #_"long" n] (Murmur3'mixCollHash hash-basis n))

;;;
 ; Returns the hash code, consistent with =, for an external, ordered
 ; collection implementing Seqable.
 ; See http://clojure.org/data_structures#hash for full algorithms.
 ;;
(defn #_"long" hash-ordered-coll [s] (Murmur3'hashOrdered s))

;;;
 ; Returns the hash code, consistent with =, for an external, unordered
 ; collection implementing Seqable. For maps, it should return
 ; map entries, whose hash is computed as (hash-ordered-coll [k v]).
 ; See http://clojure.org/data_structures#hash for full algorithms.
 ;;
(defn #_"long" hash-unordered-coll [s] (Murmur3'hashUnordered s))

(about #_"cloiure.core.Atom"

(about #_"Atom"
    (defr Atom [])

    (defn #_"Atom" Atom'new
        ([#_"Object" data] (Atom'new nil, data))
        ([#_"meta" meta, #_"Object" data]
            (merge (Atom'class.)
                (hash-map
                    #_"AtomicReference" :meta (AtomicReference. meta)
                    #_"AtomicReference" :data (AtomicReference. data)
                )
            )
        )
    )

    (defm Atom IMeta
        (#_"meta" IMeta'''meta [#_"Atom" this]
            (.get (:meta this))
        )
    )

    (defm Atom IReference
        (#_"meta" IReference'''alterMeta [#_"Atom" this, #_"fn" f, #_"seq" args]
            (loop []
                (let [#_"meta" m (.get (:meta this)) #_"meta" m' (apply f m args)]
                    (when (.compareAndSet (:meta this), m, m') => (recur)
                        m'
                    )
                )
            )
        )

        (#_"meta" IReference'''resetMeta [#_"Atom" this, #_"meta" m']
            (.set (:meta this), m')
            m'
        )
    )

    (defm Atom IDeref
        (#_"Object" IDeref'''deref [#_"Atom" this]
            (.get (:data this))
        )
    )

    (defm Atom IAtom
        (#_"boolean" IAtom'''compareAndSet [#_"Atom" this, #_"Object" o, #_"Object" o']
            (.compareAndSet (:data this), o, o')
        )

        (#_"Object" IAtom'''swap [#_"Atom" this, #_"fn" f, #_"seq" args]
            (loop []
                (let [#_"Object" o (.get (:data this)) #_"Object" o' (apply f o args)]
                    (when (.compareAndSet (:data this), o, o') => (recur)
                        o'
                    )
                )
            )
        )

        (#_"Object" IAtom'''reset [#_"Atom" this, #_"Object" o']
            (.set (:data this), o')
            o'
        )

        (#_"[Object Object]" IAtom'''swapVals [#_"Atom" this, #_"fn" f, #_"seq" args]
            (loop []
                (let [#_"Object" o (.get (:data this)) #_"Object" o' (apply f o args)]
                    (when (.compareAndSet (:data this), o, o') => (recur)
                        [o o']
                    )
                )
            )
        )

        (#_"[Object Object]" IAtom'''resetVals [#_"Atom" this, #_"Object" o']
            (loop []
                (let [#_"Object" o (.get (:data this))]
                    (when (.compareAndSet (:data this), o, o') => (recur)
                        [o o']
                    )
                )
            )
        )
    )
)
)

;;;
 ; Creates and returns an Atom with an initial value of x and optional meta m.
 ;;
(§ defn atom
    ([x] (Atom'new x))
    ([m x] (Atom'new m x))
)

;;;
 ; Atomically sets the value of atom to x' if and only if the current value of the atom is identical to x.
 ; Returns true if set happened, else false.
 ;;
(defn compare-and-set! [#_"IAtom" a x x'] (IAtom'''compareAndSet a, x, x'))

;;;
 ; Atomically swaps the value of atom to be: (apply f current-value-of-atom args).
 ; Note that f may be called multiple times, and thus should be free of side effects.
 ; Returns the value that was swapped in.
 ;;
(§ defn swap! [#_"IAtom" a f & args] (IAtom'''swap a, f, args))

;;;
 ; Sets the value of atom to x' without regard for the current value.
 ; Returns x'.
 ;;
(§ defn reset! [#_"IAtom" a x'] (IAtom'''reset a, x'))

;;;
 ; Atomically swaps the value of atom to be: (apply f current-value-of-atom args).
 ; Note that f may be called multiple times, and thus should be free of side effects.
 ; Returns [old new], the value of the atom before and after the swap.
 ;;
(defn #_"vector" swap-vals! [#_"IAtom" a f & args] (IAtom'''swapVals a, f, args))

;;;
 ; Sets the value of atom to x'. Returns [old new], the value of the
 ; atom before and after the reset.
 ;;
(defn #_"vector" reset-vals! [#_"IAtom" a x'] (IAtom'''resetVals a, x'))

(about #_"cloiure.core.Reduced"

(about #_"Reduced"
    (defr Reduced [])

    (defn #_"Reduced" Reduced'new [#_"Object" val]
        (merge (Reduced'class.)
            (hash-map
                #_"Object" :val val
            )
        )
    )

    (defm Reduced IDeref
        (#_"Object" IDeref'''deref => :val)
    )
)
)

;;;
 ; Wraps x in a way such that a reduce will terminate with the value x.
 ;;
(defn reduced [x] (Reduced'new x))

;;;
 ; If x is already reduced?, returns it, else returns (reduced x).
 ;;
(defn ensure-reduced [x] (if (reduced? x) x (reduced x)))

;;;
 ; If x is reduced?, returns (deref x), else returns x.
 ;;
(defn unreduced [x] (if (reduced? x) (deref x) x))

(about #_"cloiure.core.Util"

(about #_"Util"
    (declare Numbers'equal)

    (defn #_"boolean" Util'equiv [#_"Object" k1, #_"Object" k2]
        (cond
            (identical? k1 k2)              true
            (nil? k1)                       false
            (and (number? k1) (number? k2)) (Numbers'equal k1, k2)
            (coll? k1)                      (IObject'''equals k1, k2)
            (coll? k2)                      (IObject'''equals k2, k1)
            :else                           (IObject'''equals k1, k2)
        )
    )

    (declare Numbers'compare)

    (defn #_"int" Util'compare [#_"Object" k1, #_"Object" k2]
        (cond
            (= k1 k2)    0
            (nil? k1)    -1
            (nil? k2)    1
            (number? k1) (Numbers'compare k1, k2)
            :else        (.compareTo (cast Comparable k1), k2)
        )
    )
)
)

;;;
 ; Equality. Returns true if x equals y, false if not. Same as Java x.equals(y) except it also
 ; works for nil, and compares numbers and collections in a type-independent manner. Arbace's
 ; immutable data structures define equals() (and thus =) as a value, not an identity, comparison.
 ;;
(§ defn =
    ([x] true)
    ([x y] (Util'equiv x y))
    ([x y & s] (and (= x y) (recur-when (next s) [y (first s) (next s)] => (= y (first s)))))
)

;;;
 ; Same as (not (= obj1 obj2)).
 ;;
(defn #_"Boolean" not=
    ([x] false)
    ([x y] (not (= x y)))
    ([x y & s] (not (apply = x y s)))
)

;;;
 ; Comparator. Returns a negative number, zero, or a positive number when x is logically
 ; 'less than', 'equal to', or 'greater than' y. Same as Java x.compareTo(y) except it also
 ; works for nil, and compares numbers and collections in a type-independent manner.
 ; x must implement Comparable.
 ;;
(§ defn compare [x y] (Util'compare x y))

(about #_"cloiure.core.Ratio"

(about #_"Ratio"
    (defr Ratio [])

    (§ inherit Ratio #_"Number")

    (defn #_"Ratio" Ratio'new [#_"BigInteger" numerator, #_"BigInteger" denominator]
        (merge (Ratio'class.) (§ foreign Number'new)
            (hash-map
                #_"BigInteger" :n numerator
                #_"BigInteger" :d denominator
            )
        )
    )

    (defm Ratio Hashed
        (#_"int" Hashed'''hash [#_"Ratio" this]
            (bit-xor (f'hash (:n this)) (f'hash (:d this)))
        )
    )

    (defm Ratio IObject
        (#_"boolean" IObject'''equals [#_"Ratio" this, #_"Object" that]
            (and (satisfies? Ratio that) (= (:n that) (:n this)) (= (:d that) (:d this)))
        )

        (#_"String" IObject'''toString [#_"Ratio" this]
            (str (:n this) "/" (:d this))
        )
    )

    (defn #_"BigInteger" Ratio''bigIntegerValue [#_"Ratio" this]
        (.divide (:n this), (:d this))
    )

    (defn #_"long" Ratio''longValue [#_"Ratio" this]
        (.longValue (Ratio''bigIntegerValue this))
    )

    (defn #_"int" Ratio''intValue [#_"Ratio" this]
        (.intValue (Ratio''bigIntegerValue this))
    )

    #_foreign
    (§ defm Ratio #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"Ratio" this, #_"Object" that]
            (Numbers'compare this, (cast Number that))
        )
    )
)
)

(about #_"cloiure.core.Numbers"

(about #_"LongOps"
    (defr LongOps [])

    (defn #_"LongOps" LongOps'new []
        (LongOps'class.)
    )

    (defn #_"long" LongOps'gcd [#_"long" u, #_"long" v] (if (-/= v 0) u (recur v (-/rem u v))))

    (declare Numbers'RATIO_OPS)
    (declare Numbers'BIGINT_OPS)

    (defm LongOps Ops
        (#_"Ops" Ops'''combine [#_"LongOps" this, #_"Ops" y] (Ops'''opsWithLong y, this))

        (#_"Ops" Ops'''opsWithLong [#_"LongOps" this, #_"LongOps" x] this)
        (#_"Ops" Ops'''opsWithRatio [#_"LongOps" this, #_"RatioOps" x] Numbers'RATIO_OPS)
        (#_"Ops" Ops'''opsWithBigInt [#_"LongOps" this, #_"BigIntOps" x] Numbers'BIGINT_OPS)

        (#_"boolean" Ops'''eq [#_"LongOps" this, #_"Number" x, #_"Number" y] (-/= (.longValue x) (.longValue y)))
        (#_"boolean" Ops'''lt [#_"LongOps" this, #_"Number" x, #_"Number" y] (-/< (.longValue x) (.longValue y)))
        (#_"boolean" Ops'''lte [#_"LongOps" this, #_"Number" x, #_"Number" y] (-/<= (.longValue x) (.longValue y)))

        (#_"boolean" Ops'''isZero [#_"LongOps" this, #_"Number" x] (-/= (.longValue x) 0))
        (#_"boolean" Ops'''isPos [#_"LongOps" this, #_"Number" x] (-/> (.longValue x) 0))
        (#_"boolean" Ops'''isNeg [#_"LongOps" this, #_"Number" x] (-/< (.longValue x) 0))

        (#_"Number" Ops'''add [#_"LongOps" this, #_"Number" x, #_"Number" y]
            (let [#_"long" lx (.longValue x) #_"long" ly (.longValue y) #_"long" lz (-/+ lx ly)]
                (when (and (-/< (-/bit-xor lz lx) 0) (-/< (-/bit-xor lz ly) 0)) => (Long/valueOf lz)
                    (Ops'''add Numbers'BIGINT_OPS, x, y)
                )
            )
        )

        (#_"Number" Ops'''negate [#_"LongOps" this, #_"Number" x]
            (let [#_"long" lx (.longValue x)]
                (when (-/= lx Long/MIN_VALUE) => (Long/valueOf (-/- lx))
                    (.negate (BigInteger/valueOf lx))
                )
            )
        )

        (#_"Number" Ops'''inc [#_"LongOps" this, #_"Number" x]
            (let [#_"long" lx (.longValue x)]
                (when (-/= lx Long/MAX_VALUE) => (Long/valueOf (-/+ lx 1))
                    (Ops'''inc Numbers'BIGINT_OPS, x)
                )
            )
        )

        (#_"Number" Ops'''dec [#_"LongOps" this, #_"Number" x]
            (let [#_"long" lx (.longValue x)]
                (when (-/= lx Long/MIN_VALUE) => (Long/valueOf (-/- lx 1))
                    (Ops'''dec Numbers'BIGINT_OPS, x)
                )
            )
        )

        (#_"Number" Ops'''multiply [#_"LongOps" this, #_"Number" x, #_"Number" y]
            (let [#_"long" lx (.longValue x) #_"long" ly (.longValue y)]
                (when-not (and (-/= lx Long/MIN_VALUE) (-/< ly 0)) => (Ops'''multiply Numbers'BIGINT_OPS, x, y)
                    (let [#_"long" lz (-/* lx ly)]
                        (when (or (-/= ly 0) (-/= (-/quot lz ly) lx)) => (Ops'''multiply Numbers'BIGINT_OPS, x, y)
                            (Long/valueOf lz)
                        )
                    )
                )
            )
        )

        (#_"Number" Ops'''divide [#_"LongOps" this, #_"Number" x, #_"Number" y]
            (let [#_"long" lx (.longValue x) #_"long" ly (.longValue y)]
                (let-when-not [#_"long" gcd (LongOps'gcd lx, ly)] (-/= gcd 0) => (Long/valueOf 0)
                    (let-when-not [lx (-/quot lx gcd) ly (-/quot ly gcd)] (-/= ly 1) => (Long/valueOf lx)
                        (let [[lx ly]
                                (when (-/< ly 0) => [lx ly]
                                    [(-/- lx) (-/- ly)]
                                )]
                            (Ratio'new (BigInteger/valueOf lx), (BigInteger/valueOf ly))
                        )
                    )
                )
            )
        )

        (#_"Number" Ops'''quotient [#_"LongOps" this, #_"Number" x, #_"Number" y] (Long/valueOf (-/quot (.longValue x) (.longValue y))))
        (#_"Number" Ops'''remainder [#_"LongOps" this, #_"Number" x, #_"Number" y] (Long/valueOf (-/rem (.longValue x) (.longValue y))))
)
)

(about #_"RatioOps"
    (defr RatioOps [])

    (defn #_"RatioOps" RatioOps'new []
        (RatioOps'class.)
    )

    (declare Numbers'toRatio)
    (declare Numbers'subtract)
    (declare Numbers'multiply)
    (declare Numbers'lt)
    (declare Numbers'lte)
    (declare Numbers'gte)

    (defm RatioOps Ops
        (#_"Ops" Ops'''combine [#_"RatioOps" this, #_"Ops" y] (Ops'''opsWithRatio y, this))

        (#_"Ops" Ops'''opsWithLong [#_"RatioOps" this, #_"LongOps" x] this)
        (#_"Ops" Ops'''opsWithRatio [#_"RatioOps" this, #_"RatioOps" x] this)
        (#_"Ops" Ops'''opsWithBigInt [#_"RatioOps" this, #_"BigIntOps" x] this)

        (#_"boolean" Ops'''eq [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
                (and (-/= (:n rx) (:n ry)) (-/= (:d rx) (:d ry)))
            )
        )

        (#_"boolean" Ops'''lt [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
                (Numbers'lt (.multiply (:n rx), (:d ry)), (.multiply (:n ry), (:d rx)))
            )
        )

        (#_"boolean" Ops'''lte [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
                (Numbers'lte (.multiply (:n rx), (:d ry)), (.multiply (:n ry), (:d rx)))
            )
        )

        (#_"boolean" Ops'''isZero [#_"RatioOps" this, #_"Number" x] (-/= (.signum (:n #_"Ratio" x)) 0))
        (#_"boolean" Ops'''isPos [#_"RatioOps" this, #_"Number" x] (-/> (.signum (:n #_"Ratio" x)) 0))
        (#_"boolean" Ops'''isNeg [#_"RatioOps" this, #_"Number" x] (-/< (.signum (:n #_"Ratio" x)) 0))

        (#_"Number" Ops'''add [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
                (Ops'''divide this, (.add (.multiply (:n ry), (:d rx)), (.multiply (:n rx), (:d ry))), (.multiply (:d ry), (:d rx)))
            )
        )

        (#_"Number" Ops'''negate [#_"RatioOps" this, #_"Number" x]
            (let [#_"Ratio" r (Numbers'toRatio x)]
                (Ratio'new (.negate (:n r)), (:d r))
            )
        )

        (#_"Number" Ops'''inc [#_"RatioOps" this, #_"Number" x] (Ops'''add this, x, 1))
        (#_"Number" Ops'''dec [#_"RatioOps" this, #_"Number" x] (Ops'''add this, x, -1))

        (#_"Number" Ops'''multiply [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
                (Numbers'divide (.multiply (:n ry), (:n rx)), (.multiply (:d ry), (:d rx)))
            )
        )

        (#_"Number" Ops'''divide [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
                (Numbers'divide (.multiply (:d ry), (:n rx)), (.multiply (:n ry), (:d rx)))
            )
        )

        (#_"Number" Ops'''quotient [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (let [#_"Ratio" rx (Numbers'toRatio x) #_"Ratio" ry (Numbers'toRatio y)]
               (.divide (.multiply (:n rx), (:d ry)), (.multiply (:d rx), (:n ry)))
            )
        )

        (#_"Number" Ops'''remainder [#_"RatioOps" this, #_"Number" x, #_"Number" y]
            (Numbers'subtract x, (Numbers'multiply (Ops'''quotient this, x, y), y))
        )
    )
)

(about #_"BigIntOps"
    (defr BigIntOps [])

    (defn #_"BigIntOps" BigIntOps'new []
        (BigIntOps'class.)
    )

    (declare Numbers'toBigInteger)

    (defm BigIntOps Ops
        (#_"Ops" Ops'''combine [#_"BigIntOps" this, #_"Ops" y] (Ops'''opsWithBigInt y, this))

        (#_"Ops" Ops'''opsWithLong [#_"BigIntOps" this, #_"LongOps" x] this)
        (#_"Ops" Ops'''opsWithRatio [#_"BigIntOps" this, #_"RatioOps" x] Numbers'RATIO_OPS)
        (#_"Ops" Ops'''opsWithBigInt [#_"BigIntOps" this, #_"BigIntOps" x] this)

        (#_"boolean" Ops'''eq [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (-/= (Numbers'toBigInteger x) (Numbers'toBigInteger y))
        )

        (#_"boolean" Ops'''lt [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (-/< (.compareTo (Numbers'toBigInteger x), (Numbers'toBigInteger y)) 0)
        )

        (#_"boolean" Ops'''lte [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (-/<= (.compareTo (Numbers'toBigInteger x), (Numbers'toBigInteger y)) 0)
        )

        (#_"boolean" Ops'''isZero [#_"BigIntOps" this, #_"Number" x] (-/= (.signum (Numbers'toBigInteger x)) 0))
        (#_"boolean" Ops'''isPos [#_"BigIntOps" this, #_"Number" x] (-/> (.signum (Numbers'toBigInteger x)) 0))
        (#_"boolean" Ops'''isNeg [#_"BigIntOps" this, #_"Number" x] (-/< (.signum (Numbers'toBigInteger x)) 0))

        (#_"Number" Ops'''add [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (.add (Numbers'toBigInteger x), (Numbers'toBigInteger y))
        )

        (#_"Number" Ops'''negate [#_"BigIntOps" this, #_"Number" x] (.negate (Numbers'toBigInteger x)))

        (#_"Number" Ops'''inc [#_"BigIntOps" this, #_"Number" x] (.add (Numbers'toBigInteger x), BigInteger/ONE))
        (#_"Number" Ops'''dec [#_"BigIntOps" this, #_"Number" x] (.subtract (Numbers'toBigInteger x), BigInteger/ONE))

        (#_"Number" Ops'''multiply [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (.multiply (Numbers'toBigInteger x), (Numbers'toBigInteger y))
        )

        (#_"Number" Ops'''divide [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (let [#_"BigInteger" n (Numbers'toBigInteger x) #_"BigInteger" d (Numbers'toBigInteger y)]
                (when-not (-/= d BigInteger/ZERO) => (throw (ArithmeticException. "Divide by zero"))
                    (let [#_"BigInteger" gcd (.gcd n, d)]
                        (when-not (-/= gcd BigInteger/ZERO) => BigInteger/ZERO
                            (let [n (.divide n, gcd) d (.divide d, gcd)]
                                (condp -/= d
                                    BigInteger/ONE           n
                                    (.negate BigInteger/ONE) (.negate n)
                                                             (Ratio'new (if (-/< (.signum d) 0) (.negate n) n), (if (-/< (.signum d) 0) (.negate d) d))
                                )
                            )
                        )
                    )
                )
            )
        )

        (#_"Number" Ops'''quotient [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (.divide (Numbers'toBigInteger x), (Numbers'toBigInteger y))
        )

        (#_"Number" Ops'''remainder [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
            (.remainder (Numbers'toBigInteger x), (Numbers'toBigInteger y))
        )
    )
)

(about #_"Numbers"
    (def #_"LongOps"       Numbers'LONG_OPS       (LongOps'new)      )
    (def #_"RatioOps"      Numbers'RATIO_OPS      (RatioOps'new)     )
    (def #_"BigIntOps"     Numbers'BIGINT_OPS     (BigIntOps'new)    )

    (defn #_"Ops" Numbers'ops [#_"Object" x]
        (condp = (class x)
            BigInteger  Numbers'BIGINT_OPS
            Ratio'iface Numbers'RATIO_OPS
                        Numbers'LONG_OPS
        )
    )

    (defn #_"int" Numbers'compare [#_"Number" x, #_"Number" y]
        (let [#_"Ops" ops (Ops'''combine (Numbers'ops x), (Numbers'ops y))]
            (cond (Ops'''lt ops, x, y) -1 (Ops'''lt ops, y, x) 1 :else 0)
        )
    )

    (defn #_"boolean" Numbers'equal [#_"Number" x, #_"Number" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''eq x, y))
    )

    (defn #_"boolean" Numbers'lt [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lt (cast Number x), (cast Number y)))
    )

    (defn #_"boolean" Numbers'lte [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lte (cast Number x), (cast Number y)))
    )

    (defn #_"boolean" Numbers'gt [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lt (cast Number y), (cast Number x)))
    )

    (defn #_"boolean" Numbers'gte [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lte (cast Number y), (cast Number x)))
    )

    (defn #_"boolean" Numbers'isZero [#_"Object" x] (Ops'''isZero (Numbers'ops x), (cast Number x)))
    (defn #_"boolean" Numbers'isPos  [#_"Object" x] (Ops'''isPos  (Numbers'ops x), (cast Number x)))
    (defn #_"boolean" Numbers'isNeg  [#_"Object" x] (Ops'''isNeg  (Numbers'ops x), (cast Number x)))

    (defn #_"Number" Numbers'add [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''add (cast Number x), (cast Number y)))
    )

    (defn #_"Number" Numbers'subtract [#_"Object" x, #_"Object" y]
        (let [#_"Number" negativeY (Ops'''negate (Numbers'ops y), (cast Number y))]
            (-> (Ops'''combine (Numbers'ops x), (Numbers'ops negativeY)) (Ops'''add (cast Number x), negativeY))
        )
    )

    (defn #_"Number" Numbers'negate [#_"Object" x] (Ops'''negate (Numbers'ops x), (cast Number x)))
    (defn #_"Number" Numbers'inc    [#_"Object" x] (Ops'''inc    (Numbers'ops x), (cast Number x)))
    (defn #_"Number" Numbers'dec    [#_"Object" x] (Ops'''dec    (Numbers'ops x), (cast Number x)))

    (defn #_"Number" Numbers'multiply [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''multiply (cast Number x), (cast Number y)))
    )

    (defn #_"Number" Numbers'divide [#_"Object" x, #_"Object" y]
        (let-when-not [#_"Ops" yops (Numbers'ops y)] (Ops'''isZero yops, (cast Number y)) => (throw (ArithmeticException. "Divide by zero"))
            (-> (Ops'''combine (Numbers'ops x), yops) (Ops'''divide (cast Number x), (cast Number y)))
        )
    )

    (defn #_"Number" Numbers'quotient [#_"Object" x, #_"Object" y]
        (let-when-not [#_"Ops" yops (Numbers'ops y)] (Ops'''isZero yops, (cast Number y)) => (throw (ArithmeticException. "Divide by zero"))
            (-> (Ops'''combine (Numbers'ops x), yops) (Ops'''quotient (cast Number x), (cast Number y)))
        )
    )

    (defn #_"Number" Numbers'remainder [#_"Object" x, #_"Object" y]
        (let-when-not [#_"Ops" yops (Numbers'ops y)] (Ops'''isZero yops, (cast Number y)) => (throw (ArithmeticException. "Divide by zero"))
            (-> (Ops'''combine (Numbers'ops x), yops) (Ops'''remainder (cast Number x), (cast Number y)))
        )
    )

    (defn #_"BigInteger" Numbers'toBigInteger [#_"Object" x]
        (condp instance? x
            BigInteger x
                       (BigInteger/valueOf (.longValue (cast Number x)))
        )
    )

    (defn #_"Ratio" Numbers'toRatio [#_"Object" x]
        (condp satisfies? x
            Ratio x
                  (Ratio'new (Numbers'toBigInteger x), BigInteger/ONE)
        )
    )

    (defn- #_"long" Numbers'bitOpsCast [#_"Object" x]
        (let [#_"Class" c (class x)]
            (when (any = c Long Integer Byte) => (throw! (str "bit operation not supported on " c))
                (long x)
            )
        )
    )

    (defn #_"long" Numbers'not [#_"Object" x] (-/bit-not (Numbers'bitOpsCast x)))

    (defn #_"long" Numbers'and [#_"Object" x, #_"Object" y] (-/bit-and (Numbers'bitOpsCast x) (Numbers'bitOpsCast y)))
    (defn #_"long" Numbers'or  [#_"Object" x, #_"Object" y] (-/bit-or (Numbers'bitOpsCast x) (Numbers'bitOpsCast y)))
    (defn #_"long" Numbers'xor [#_"Object" x, #_"Object" y] (-/bit-xor (Numbers'bitOpsCast x) (Numbers'bitOpsCast y)))

    (defn #_"long" Numbers'andNot [#_"Object" x, #_"Object" y] (-/bit-and (Numbers'bitOpsCast x) (-/bit-not (Numbers'bitOpsCast y))))

    (defn #_"long" Numbers'shiftLeft          [#_"Object" x, #_"Object" n] (-/bit-shift-left (Numbers'bitOpsCast x) (Numbers'bitOpsCast n)))
    (defn #_"long" Numbers'shiftRight         [#_"Object" x, #_"Object" n] (-/bit-shift-right (Numbers'bitOpsCast x) (Numbers'bitOpsCast n)))
    (defn #_"long" Numbers'unsignedShiftRight [#_"Object" x, #_"Object" n] (-/unsigned-bit-shift-right (Numbers'bitOpsCast x) (Numbers'bitOpsCast n)))

    (defn #_"long" Numbers'clearBit [#_"Object" x, #_"Object" n] (-/bit-and (Numbers'bitOpsCast x) (-/bit-not (-/bit-shift-left 1 (Numbers'bitOpsCast n)))))
    (defn #_"long" Numbers'setBit   [#_"Object" x, #_"Object" n] (-/bit-or (Numbers'bitOpsCast x) (-/bit-shift-left 1 (Numbers'bitOpsCast n))))
    (defn #_"long" Numbers'flipBit  [#_"Object" x, #_"Object" n] (-/bit-xor (Numbers'bitOpsCast x) (-/bit-shift-left 1 (Numbers'bitOpsCast n))))

    (defn #_"boolean" Numbers'testBit [#_"Object" x, #_"Object" n] (-/not= (-/bit-and (Numbers'bitOpsCast x) (-/bit-shift-left 1 (Numbers'bitOpsCast n))) 0))
)
)

;;;
 ; Returns non-nil if nums are in monotonically increasing order, otherwise false.
 ;;
(§ defn <
    ([x] true)
    ([x y] (Numbers'lt x y))
    ([x y & s] (and (< x y) (recur-when (next s) [y (first s) (next s)] => (< y (first s)))))
)

;;;
 ; Returns non-nil if nums are in monotonically non-decreasing order, otherwise false.
 ;;
(§ defn <=
    ([x] true)
    ([x y] (Numbers'lte x y))
    ([x y & s] (and (<= x y) (recur-when (next s) [y (first s) (next s)] => (<= y (first s)))))
)

;;;
 ; Returns non-nil if nums are in monotonically decreasing order, otherwise false.
 ;;
(§ defn >
    ([x] true)
    ([x y] (Numbers'gt x y))
    ([x y & s] (and (> x y) (recur-when (next s) [y (first s) (next s)] => (> y (first s)))))
)

;;;
 ; Returns non-nil if nums are in monotonically non-increasing order, otherwise false.
 ;;
(§ defn >=
    ([x] true)
    ([x y] (Numbers'gte x y))
    ([x y & s] (and (>= x y) (recur-when (next s) [y (first s) (next s)] => (>= y (first s)))))
)

;;;
 ; Returns the greatest of the nums.
 ;;
(defn max
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & s] (reduce max (max x y) s))
)

;;;
 ; Returns the least of the nums.
 ;;
(defn min
    ([x] x)
    ([x y] (if (< x y) x y))
    ([x y & s] (reduce min (min x y) s))
)

;;;
 ; Returns true if n is zero | greater than zero | less than zero, else false.
 ;;
(§ defn zero? [n] (Numbers'isZero n))
(§ defn pos?  [n] (Numbers'isPos  n))
(§ defn neg?  [n] (Numbers'isNeg  n))

;;;
 ; Returns the sum of nums. (+) returns 0. Supports arbitrary precision.
 ;;
(§ defn +
    ([] 0)
    ([x] (cast Number x))
    ([x y] (Numbers'add x y))
    ([x y & s] (reduce + (+ x y) s))
)

;;;
 ; If no ys are supplied, returns the negation of x, else subtracts
 ; the ys from x and returns the result. Supports arbitrary precision.
 ;;
(§ defn -
    ([x] (Numbers'negate x))
    ([x y] (Numbers'subtract x y))
    ([x y & s] (reduce - (- x y) s))
)

;;;
 ; Returns a number one greater than num. Supports arbitrary precision.
 ;;
(§ defn inc [x] (Numbers'inc x))

;;;
 ; Returns a number one less than num. Supports arbitrary precision.
 ;;
(§ defn dec [x] (Numbers'dec x))

;;;
 ; Returns the product of nums. (*) returns 1. Supports arbitrary precision.
 ;;
(§ defn *
    ([] 1)
    ([x] (cast Number x))
    ([x y] (Numbers'multiply x y))
    ([x y & s] (reduce * (* x y) s))
)

;;;
 ; If no denominators are supplied, returns 1/numerator,
 ; else returns numerator divided by all of the denominators.
 ;;
(§ defn /
    ([x] (/ 1 x))
    ([x y] (Numbers'divide x y))
    ([x y & s] (reduce / (/ x y) s))
)

;;;
 ; quot[ient] of dividing numerator by denominator.
 ;;
(§ defn quot [num div] (Numbers'quotient num div))

;;;
 ; rem[ainder] of dividing numerator by denominator.
 ;;
(§ defn rem [num div] (Numbers'remainder num div))

;;;
 ; Modulus of num and div. Truncates toward negative infinity.
 ;;
(defn mod [num div]
    (let-when [m (rem num div)] (or (zero? m) (= (pos? num) (pos? div))) => (+ m div)
        m
    )
)

;;;
 ; Bitwise complement.
 ;;
(defn bit-not [x] (Numbers'not x))

;;;
 ; Bitwise and.
 ;;
(§ defn bit-and
    ([x y] (Numbers'and x y))
    ([x y & s] (reduce bit-and (bit-and x y) s))
)

;;;
 ; Bitwise or.
 ;;
(§ defn bit-or
    ([x y] (Numbers'or x y))
    ([x y & s] (reduce bit-or (bit-or x y) s))
)

;;;
 ; Bitwise exclusive or.
 ;;
(§ defn bit-xor
    ([x y] (Numbers'xor x y))
    ([x y & s] (reduce bit-xor (bit-xor x y) s))
)

;;;
 ; Bitwise and with complement.
 ;;
(defn bit-and-not
    ([x y] (Numbers'andNot x y))
    ([x y & s] (reduce bit-and-not (bit-and-not x y) s))
)

;;;
 ; Clear | set | flip | test bit at index i.
 ;;
(defn bit-clear [x i] (Numbers'clearBit x i))
(defn bit-set   [x i] (Numbers'setBit   x i))
(defn bit-flip  [x i] (Numbers'flipBit  x i))
(defn bit-test  [x i] (Numbers'testBit  x i))

;;;
 ; Bitwise shift left | right | right, without sign-extension.
 ;;
(defn          bit-shift-left  [x n] (Numbers'shiftLeft          x n))
(defn          bit-shift-right [x n] (Numbers'shiftRight         x n))
(defn unsigned-bit-shift-right [x n] (Numbers'unsignedShiftRight x n))

;;;
 ; Returns true if n is even, throws an exception if n is not an integer.
 ;;
(defn even? [n]
    (when (integer? n) => (throw! (str "argument must be an integer: " n))
        (zero? (bit-and n 1))
    )
)

;;;
 ; Returns true if n is odd, throws an exception if n is not an integer.
 ;;
(defn odd? [n] (not (even? n)))

(about #_"cloiure.core.AFn"

(about #_"AFn"
    (declare Compiler'demunge)

    (defn #_"void" AFn'throwArity [#_"fn" f, #_"int" n]
        (throw! (str "wrong number of args (" (if (neg? n) (str "more than " (dec (- n))) n) ") passed to: " (Compiler'demunge (.getName (class f)))))
    )

    (defn #_"Object" AFn'applyToHelper [#_"fn" f, #_"seq" s]
        (case (count s (inc 9))
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

(about #_"cloiure.core.Symbol"

(about #_"Symbol"
    (defr Symbol [])

    #_inherit
    (defm Symbol AFn)

    (defn- #_"Symbol" Symbol'new
        ([#_"String" ns, #_"String" name] (Symbol'new nil, ns, name))
        ([#_"meta" meta, #_"String" ns, #_"String" name]
            (merge (Symbol'class.)
                (hash-map
                    #_"meta" :_meta meta
                    #_"String" :ns ns
                    #_"String" :name name
                )
            )
        )
    )

    (defm Symbol IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm Symbol IObj
        (#_"Symbol" IObj'''withMeta [#_"Symbol" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (Symbol'new meta, (:ns this), (:name this))
            )
        )
    )

    (defn #_"Symbol" Symbol'intern
        ([#_"String" nsname]
            (let [#_"int" i (.indexOf nsname, (int \/))]
                (if (or (= i -1) (= nsname "/"))
                    (Symbol'new nil, nsname)
                    (Symbol'new (.substring nsname, 0, i), (.substring nsname, (inc i)))
                )
            )
        )
        ([#_"String" ns, #_"String" name]
            (Symbol'new ns, name)
        )
    )

    (defm Symbol INamed
        (#_"String" INamed'''getNamespace => :ns)

        (#_"String" INamed'''getName => :name)
    )

    (defm Symbol IObject
        (#_"boolean" IObject'''equals [#_"Symbol" this, #_"Object" that]
            (or (identical? this that)
                (and (symbol? that) (= (:ns this) (:ns that)) (= (:name this) (:name that)))
            )
        )

        (#_"String" IObject'''toString [#_"Symbol" this]
            (if (some? (:ns this)) (str (:ns this) "/" (:name this)) (:name this))
        )
    )

    (defm Symbol Hashed
        (#_"int" Hashed'''hash [#_"Symbol" this]
            (hash-combine (Murmur3'hashUnencodedChars (:name this)) (:ns this))
        )
    )

    #_foreign
    (§ defm Symbol #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"Symbol" this, #_"Symbol" that]
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
    )

    (defm Symbol IFn
        (#_"Object" IFn'''invoke
            ([#_"Symbol" this, #_"Object" obj] (get obj this))
            ([#_"Symbol" this, #_"Object" obj, #_"Object" not-found] (get obj this not-found))
        )

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )
)
)

(about #_"cloiure.core.Keyword"

(about #_"Keyword"
    (defr Keyword [])

    #_inherit
    (defm Keyword AFn)

    (def- #_"{Symbol Reference<Keyword>}'" Keyword'cache (atom {}))
    (def- #_"ReferenceQueue" Keyword'queue (ReferenceQueue.))

    (defn- #_"Keyword" Keyword'new [#_"Symbol" sym]
        (merge (Keyword'class.)
            (hash-map
                #_"Symbol" :sym sym
                #_"int" :hash (+ (f'hash sym) 0x9e3779b9)
            )
        )
    )

    (defn #_"Keyword" Keyword'intern [#_"Symbol" sym]
        (let [#_"Reference<Keyword>" r (get @Keyword'cache sym)
              [sym r #_"Keyword" k]
                (when (nil? r) => [sym r nil]
                    (Cache'purge Keyword'queue, Keyword'cache)
                    (let [sym
                            (when (some? (meta sym)) => sym
                                (with-meta sym nil)
                            )
                          k (Keyword'new sym) r (WeakReference. #_"<Keyword>" k, Keyword'queue)
                          _ (swap! Keyword'cache assoc sym r)]
                        [sym r k]
                    )
                )]
            (when (some? r) => k
                (or (.get r)
                    (do ;; entry died in the interim, do over
                        (swap! Keyword'cache #(if (identical? (get % sym) r) (dissoc % sym) %))
                        (recur #_"Keyword'intern" sym)
                    )
                )
            )
        )
    )

    (defn #_"Keyword" Keyword'find [#_"Symbol" sym]
        (when-some [#_"Reference<Keyword>" ref (get @Keyword'cache sym)]
            (.get ref)
        )
    )

    (defm Keyword INamed
        (#_"String" INamed'''getNamespace [#_"Keyword" this]
            (INamed'''getNamespace (:sym this))
        )

        (#_"String" INamed'''getName [#_"Keyword" this]
            (INamed'''getName (:sym this))
        )
    )

    (defm Keyword Hashed
        (#_"int" Hashed'''hash => :hash)
    )

    (defm Keyword IObject
        (#_"boolean" IObject'''equals [#_"Keyword" this, #_"Object" that]
            (identical? this that)
        )

        (#_"String" IObject'''toString [#_"Keyword" this]
            (str ":" (:sym this))
        )
    )

    #_foreign
    (§ defm Keyword #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"Keyword" this, #_"Keyword" that]
            (compare (:sym this) (:sym that))
        )
    )

    (defm Keyword IFn
        (#_"Object" IFn'''invoke
            ([#_"Keyword" this, #_"Object" obj] (get obj this))
            ([#_"Keyword" this, #_"Object" obj, #_"Object" not-found] (get obj this not-found))
        )

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )
)
)

(about #_"cloiure.core.Fn"

(about #_"Fn"
    (defr Fn [])

    #_inherit
    (defm Fn AFn)

    (defn #_"Fn" Fn'new []
        (merge (Fn'class.)
            (hash-map
                #_"MethodImplCache'" :__methodImplCache (atom nil)
            )
        )
    )

    (defm Fn IFn
        (#_"Object" IFn'''invoke
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

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )

    #_foreign
    (§ defm Fn #_"Comparator"
        (#_"int" Comparator'''compare [#_"Fn" this, #_"Object" o1, #_"Object" o2]
            (let [#_"Object" o (IFn'''invoke this, o1, o2)]
                (if (boolean? o)
                    (cond (boolean o) -1 (boolean (IFn'''invoke this, o2, o1)) 1 :else 0)
                    (.intValue (cast Number o))
                )
            )
        )
    )
)
)

(about #_"cloiure.core.RestFn"

(about #_"RestFn"
    (defr RestFn [])

    #_inherit
    (defm RestFn AFn Fn)

    (defn #_"RestFn" RestFn'new []
        (merge (RestFn'class.) (Fn'new))
    )

    (defm RestFn IRestFn
        ;; abstract IRestFn requiredArity

        (#_"Object" IRestFn'''doInvoke
            ([#_"RestFn" this, #_"seq" args]                                     nil)
            ([#_"RestFn" this, a1, #_"seq" args]                                 nil)
            ([#_"RestFn" this, a1, a2, #_"seq" args]                             nil)
            ([#_"RestFn" this, a1, a2, a3, #_"seq" args]                         nil)
            ([#_"RestFn" this, a1, a2, a3, a4, #_"seq" args]                     nil)
            ([#_"RestFn" this, a1, a2, a3, a4, a5, #_"seq" args]                 nil)
            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, #_"seq" args]             nil)
            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7, #_"seq" args]         nil)
            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7, a8, #_"seq" args]     nil)
            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args] nil)
        )
    )

    (defm RestFn IFn
        (#_"Object" IFn'''invoke
            ([#_"RestFn" this]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, nil)
                      (AFn'throwArity this, 0)
                )
            )

            ([#_"RestFn" this, a1]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1))
                    1 (IRestFn'''doInvoke this, a1, nil)
                      (AFn'throwArity this, 1)
                )
            )

            ([#_"RestFn" this, a1, a2]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2))
                    1 (IRestFn'''doInvoke this, a1, (list a2))
                    2 (IRestFn'''doInvoke this, a1, a2, nil)
                      (AFn'throwArity this, 2)
                )
            )

            ([#_"RestFn" this, a1, a2, a3]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, nil)
                      (AFn'throwArity this, 3)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3 a4))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3 a4))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3 a4))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list a4))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, nil)
                      (AFn'throwArity this, 4)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4, a5]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3 a4 a5))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3 a4 a5))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3 a4 a5))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list a4 a5))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, (list a5))
                    5 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, nil)
                      (AFn'throwArity this, 5)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3 a4 a5 a6))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3 a4 a5 a6))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3 a4 a5 a6))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list a4 a5 a6))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, (list a5 a6))
                    5 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, (list a6))
                    6 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, nil)
                      (AFn'throwArity this, 6)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3 a4 a5 a6 a7))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3 a4 a5 a6 a7))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3 a4 a5 a6 a7))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list a4 a5 a6 a7))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, (list a5 a6 a7))
                    5 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, (list a6 a7))
                    6 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, (list a7))
                    7 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, nil)
                      (AFn'throwArity this, 7)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7, a8]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3 a4 a5 a6 a7 a8))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3 a4 a5 a6 a7 a8))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3 a4 a5 a6 a7 a8))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list a4 a5 a6 a7 a8))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, (list a5 a6 a7 a8))
                    5 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, (list a6 a7 a8))
                    6 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, (list a7 a8))
                    7 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, (list a8))
                    8 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, nil)
                      (AFn'throwArity this, 8)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list a1 a2 a3 a4 a5 a6 a7 a8 a9))
                    1 (IRestFn'''doInvoke this, a1, (list a2 a3 a4 a5 a6 a7 a8 a9))
                    2 (IRestFn'''doInvoke this, a1, a2, (list a3 a4 a5 a6 a7 a8 a9))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list a4 a5 a6 a7 a8 a9))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, (list a5 a6 a7 a8 a9))
                    5 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, (list a6 a7 a8 a9))
                    6 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, (list a7 a8 a9))
                    7 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, (list a8 a9))
                    8 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, (list a9))
                    9 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, a9, nil)
                      (AFn'throwArity this, 9)
                )
            )

            ([#_"RestFn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args]
                (case (IRestFn'''requiredArity this)
                    0 (IRestFn'''doInvoke this, (list* a1 a2 a3 a4 a5 a6 a7 a8 a9 args))
                    1 (IRestFn'''doInvoke this, a1, (list* a2 a3 a4 a5 a6 a7 a8 a9 args))
                    2 (IRestFn'''doInvoke this, a1, a2, (list* a3 a4 a5 a6 a7 a8 a9 args))
                    3 (IRestFn'''doInvoke this, a1, a2, a3, (list* a4 a5 a6 a7 a8 a9 args))
                    4 (IRestFn'''doInvoke this, a1, a2, a3, a4, (list* a5 a6 a7 a8 a9 args))
                    5 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, (list* a6 a7 a8 a9 args))
                    6 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, (list* a7 a8 a9 args))
                    7 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, (list* a8 a9 args))
                    8 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, (list* a9 args))
                    9 (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, a9, args)
                      (AFn'throwArity this, -10)
                )
            )
        )

        (#_"Object" IFn'''applyTo [#_"RestFn" this, #_"seq" s]
            (let-when [#_"int" n (IRestFn'''requiredArity this)] (< n (count s (inc n))) => (AFn'applyToHelper this, s)
                (case n
                    0                                           (IRestFn'''doInvoke this, s)
                    1 (let [[a1 & s] s]                         (IRestFn'''doInvoke this, a1, s))
                    2 (let [[a1 a2 & s] s]                      (IRestFn'''doInvoke this, a1, a2, s))
                    3 (let [[a1 a2 a3 & s] s]                   (IRestFn'''doInvoke this, a1, a2, a3, s))
                    4 (let [[a1 a2 a3 a4 & s] s]                (IRestFn'''doInvoke this, a1, a2, a3, a4, s))
                    5 (let [[a1 a2 a3 a4 a5 & s] s]             (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, s))
                    6 (let [[a1 a2 a3 a4 a5 a6 & s] s]          (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, s))
                    7 (let [[a1 a2 a3 a4 a5 a6 a7 & s] s]       (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, s))
                    8 (let [[a1 a2 a3 a4 a5 a6 a7 a8 & s] s]    (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, s))
                    9 (let [[a1 a2 a3 a4 a5 a6 a7 a8 a9 & s] s] (IRestFn'''doInvoke this, a1, a2, a3, a4, a5, a6, a7, a8, a9, s))
                      (AFn'throwArity this, -10)
                )
            )
        )
    )

    (§ defm RestFn #_"Comparator"
        ;; inherit Fn compare
    )
)
)

(about #_"cloiure.core.ASeq"

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

(about #_"cloiure.core.LazySeq"

(about #_"LazySeq"
    (defq LazySeq [_meta, f, o, s])

    (defn- #_"LazySeq" LazySeq'init [#_"meta" meta, #_"fn" f, #_"seq" s]
        (assoc!! (LazySeq'class. (anew 4))
            #_"meta" :_meta meta
            #_"fn'" :f (atom f)
            #_"Object'" :o (atom nil)
            #_"seq'" :s (atom s)
        )
    )

    (defn- #_"LazySeq" LazySeq'new
        ([#_"fn" f]                 (LazySeq'init nil,  f,   nil))
        ([#_"meta" meta, #_"seq" s] (LazySeq'init meta, nil, s  ))
    )

    (defm LazySeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm LazySeq IObj
        (#_"LazySeq" IObj'''withMeta [#_"LazySeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (LazySeq'new meta, (seq this))
            )
        )
    )

    (defm LazySeq IPersistentCollection
        ;; abstract IPersistentCollection conj

        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"LazySeq" this]
            ()
        )
    )

    (defm LazySeq Sequential)

    (defm LazySeq Seqable
        (#_"seq" Seqable'''seq [#_"LazySeq" this]
            (locking this
                (letfn [(step- [this]
                            (when-some [#_"fn" f @(:f this)]
                                (reset! (:f this) nil)
                                (reset! (:o this) (f))
                            )
                            (or @(:o this) @(:s this))
                        )]
                    (step- this)
                    (when-some [#_"Object" o @(:o this)]
                        (reset! (:o this) nil)
                        (reset! (:s this) (loop-when-recur o (satisfies? LazySeq o) (step- o) => (seq o)))
                    )
                    @(:s this)
                )
            )
        )
    )

    (defm LazySeq ISeq
        (#_"Object" ISeq'''first [#_"LazySeq" this]
            (when-some [#_"seq" s (seq this)]
                (first s)
            )
        )

        (#_"seq" ISeq'''next [#_"LazySeq" this]
            (when-some [#_"seq" s (seq this)]
                (next s)
            )
        )
    )

    (defm LazySeq IObject
        (#_"boolean" IObject'''equals [#_"LazySeq" this, #_"Object" that]
            (if-some [#_"seq" s (seq this)]
                (= s that)
                (and (sequential? that) (nil? (seq that)))
            )
        )

        ;; abstract IObject toString
    )

    (defm LazySeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm LazySeq IPending
        (#_"boolean" IPending'''isRealized [#_"LazySeq" this]
            (locking this
                (nil? @(:f this))
            )
        )
    )
)
)

(about #_"cloiure.core.APersistentMap"

(about #_"APersistentMap"
    (defr APersistentMap [])

    #_inherit
    (defm APersistentMap AFn)

    (defn #_"APersistentMap" APersistentMap'new []
        (APersistentMap'class.)
    )

    #_abstract
    (defm APersistentMap Associative Counted ILookup IPersistentMap Seqable)

    (defm APersistentMap IPersistentCollection
        (#_"IPersistentCollection" IPersistentCollection'''conj [#_"APersistentMap" this, #_"Object" o]
            (condp satisfies? o
                IMapEntry
                    (assoc this (key o) (val o))
                IPersistentVector
                    (when (= (count o) 2) => (throw! "vector arg to map conj must be a pair")
                        (assoc this (nth o 0) (nth o 1))
                    )
                #_else
                    (loop-when [this this #_"seq" s (seq o)] (some? s) => this
                        (let [#_"IMapEntry" e (first s)]
                            (recur (assoc this (key e) (val e)) (next s))
                        )
                    )
            )
        )

        ;; abstract IPersistentCollection empty
    )

    (declare RT'printString)

    (defm APersistentMap IObject
        (#_"boolean" IObject'''equals [#_"APersistentMap" this, #_"Object" that]
            (or (identical? this that)
                (and (map? that) (= (count that) (count this))
                    (loop-when [#_"seq" s (seq this)] (some? s) => true
                        (let [#_"IMapEntry" e (first s) #_"Object" k (key e)]
                            (and (contains? that k) (= (val e) (get that k))
                                (recur (next s))
                            )
                        )
                    )
                )
            )
        )

        (#_"String" IObject'''toString => RT'printString)
    )

    (defm APersistentMap Hashed
        (#_"int" Hashed'''hash => Murmur3'hashUnordered)
    )

    (defm APersistentMap IFn
        (#_"Object" IFn'''invoke
            ([#_"APersistentMap" this, #_"Object" key] (get this key))
            ([#_"APersistentMap" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
        )

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )
)
)

(about #_"cloiure.core.APersistentSet"

(about #_"APersistentSet"
    (defr APersistentSet [])

    #_inherit
    (defm APersistentSet AFn)

    #_abstract
    (defm APersistentSet IPersistentCollection)

    (defn #_"APersistentSet" APersistentSet'new [#_"map" impl]
        (merge (APersistentSet'class.)
            (hash-map
                #_"map" :impl impl
            )
        )
    )

    (defm APersistentSet IPersistentSet
        ;; abstract IPersistentSet disj

        (#_"boolean" IPersistentSet'''contains? [#_"APersistentSet" this, #_"Object" key]
            (contains? (:impl this) key)
        )

        (#_"Object" IPersistentSet'''get [#_"APersistentSet" this, #_"Object" key]
            (get (:impl this) key)
        )
    )

    (defm APersistentSet Counted
        (#_"int" Counted'''count [#_"APersistentSet" this]
            (count (:impl this))
        )
    )

    (defm APersistentSet Seqable
        (#_"seq" Seqable'''seq [#_"APersistentSet" this]
            (keys (:impl this))
        )
    )

    (defm APersistentSet IFn
        (#_"Object" IFn'''invoke
            ([#_"APersistentSet" this, #_"Object" key] (get this key))
            ([#_"APersistentSet" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
        )

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )

    (defm APersistentSet IObject
        (#_"boolean" IObject'''equals [#_"APersistentSet" this, #_"Object" that]
            (or (identical? this that)
                (and (set? that) (= (count this) (count that))
                    (loop-when [#_"seq" s (seq that)] (some? s) => true
                        (and (contains? this (first s)) (recur (next s)))
                    )
                )
            )
        )

        (#_"String" IObject'''toString => RT'printString)
    )

    (defm APersistentSet Hashed
        (#_"int" Hashed'''hash => Murmur3'hashUnordered)
    )
)
)

(about #_"cloiure.core.APersistentVector"

(about #_"VSeq"
    (defq VSeq [_meta, v, i])

    #_inherit
    (defm VSeq ASeq)

    (defn #_"VSeq" VSeq'new
        ([#_"vector" v, #_"int" i] (VSeq'new nil, v, i))
        ([#_"meta" meta, #_"vector" v, #_"int" i]
            (assoc!! (VSeq'class. (anew 3))
                #_"meta" :_meta meta
                #_"vector" :v v
                #_"int" :i i
            )
        )
    )

    (defm VSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm VSeq IObj
        (#_"VSeq" IObj'''withMeta [#_"VSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (VSeq'new meta, (:v this), (:i this))
            )
        )
    )

    (defm VSeq ISeq
        (#_"Object" ISeq'''first [#_"VSeq" this]
            (nth (:v this) (:i this))
        )

        (#_"seq" ISeq'''next [#_"VSeq" this]
            (when (< (inc (:i this)) (count (:v this)))
                (VSeq'new (:v this), (inc (:i this)))
            )
        )
    )

    (defm VSeq Counted
        (#_"int" Counted'''count [#_"VSeq" this]
            (- (count (:v this)) (:i this))
        )
    )

    (defm VSeq IReduce
        (#_"Object" IReduce'''reduce
            ([#_"VSeq" this, #_"fn" f]
                (let [#_"vector" v (:v this) #_"int" i (:i this) #_"int" n (count v)]
                    (loop-when [#_"Object" r (nth v i) i (inc i)] (< i n) => r
                        (let-when [r (f r (nth v i))] (reduced? r) => (recur r (inc i))
                            @r
                        )
                    )
                )
            )
            ([#_"VSeq" this, #_"fn" f, #_"Object" r]
                (let [#_"vector" v (:v this) #_"int" i (:i this) #_"int" n (count v)]
                    (loop-when [r (f r (nth v i)) i (inc i)] (< i n) => (if (reduced? r) @r r)
                        (when (reduced? r) => (recur (f r (nth v i)) (inc i))
                            @r
                        )
                    )
                )
            )
        )
    )

    (defm VSeq Sequential)

    (defm VSeq Seqable
        (#_"seq" Seqable'''seq [#_"VSeq" this]
            this
        )
    )

    (defm VSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm VSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)

(about #_"RSeq"
    (defq RSeq [_meta, v, i])

    #_inherit
    (defm RSeq ASeq)

    (defn #_"RSeq" RSeq'new
        ([#_"vector" v, #_"int" i] (RSeq'new nil, v, i))
        ([#_"meta" meta, #_"vector" v, #_"int" i]
            (assoc!! (RSeq'class. (anew 3))
                #_"meta" :_meta meta
                #_"vector" :v v
                #_"int" :i i
            )
        )
    )

    (defm RSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm RSeq IObj
        (#_"RSeq" IObj'''withMeta [#_"RSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (RSeq'new meta, (:v this), (:i this))
            )
        )
    )

    (defm RSeq ISeq
        (#_"Object" ISeq'''first [#_"RSeq" this]
            (nth (:v this) (:i this))
        )

        (#_"seq" ISeq'''next [#_"RSeq" this]
            (when (pos? (:i this))
                (RSeq'new (:v this), (dec (:i this)))
            )
        )
    )

    (defm RSeq Counted
        (#_"int" Counted'''count [#_"RSeq" this]
            (inc (:i this))
        )
    )

    (defm RSeq Sequential)

    (defm RSeq Seqable
        (#_"seq" Seqable'''seq [#_"RSeq" this]
            this
        )
    )

    (defm RSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm RSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.AMapEntry"

(about #_"AMapEntry"
    #_inherit
    (defm AMapEntry AFn APersistentVector)

    #_abstract
    (defm AMapEntry IMapEntry)

    (defm AMapEntry Indexed
        (#_"Object" Indexed'''nth
            ([#_"AMapEntry" this, #_"int" i]
                (case i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) (throw! "index is out of bounds"))
            )
            ([#_"AMapEntry" this, #_"int" i, #_"Object" not-found]
                (case i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) not-found)
            )
        )
    )

    (defm AMapEntry Counted
        (#_"int" Counted'''count [#_"AMapEntry" this]
            2
        )
    )

    (defm AMapEntry Seqable
        (#_"seq" Seqable'''seq [#_"AMapEntry" this]
            (VSeq'new this, 0)
        )
    )

    (defm AMapEntry Reversible
        (#_"seq" Reversible'''rseq [#_"AMapEntry" this]
            (RSeq'new this, 1)
        )
    )

    (defm AMapEntry Sequential)

    (defm AMapEntry IObject
        (#_"boolean" IObject'''equals [#_"AMapEntry" this, #_"Object" that]
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

        (#_"String" IObject'''toString => RT'printString)
    )

    (defm AMapEntry Hashed
        (#_"int" Hashed'''hash [#_"AMapEntry" this]
            (loop-when [#_"int" hash 1 #_"int" i 0] (< i 2) => (Murmur3'mixCollHash hash, i)
                (recur (+ (* 31 hash) (f'hash (Indexed'''nth this, i))) (inc i))
            )
        )
    )

    #_foreign
    (§ defm AMapEntry #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"AMapEntry" this, #_"IPersistentVector" that]
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
)

(about #_"cloiure.core.MapEntry"

(about #_"MapEntry"
    (defr MapEntry [])

    #_inherit
    (defm MapEntry AFn APersistentVector AMapEntry)

    (defn- #_"MapEntry" MapEntry'new [#_"Object" key, #_"Object" val]
        (merge (MapEntry'class.)
            (hash-map
                #_"Object" :_key key
                #_"Object" :_val val
            )
        )
    )

    (defm MapEntry IMapEntry
        (#_"Object" IMapEntry'''key => :_key)

        (#_"Object" IMapEntry'''val => :_val)
    )

    (defm MapEntry Sequential)

    (§ inherit MapEntry AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
)
)

(about #_"cloiure.core.ArraySeq"

(about #_"ArraySeq"
    (defr ArraySeq [])

    #_inherit
    (defm ArraySeq ASeq)

    (defn #_"ArraySeq" ArraySeq'new
        ([#_"Object[]" a, #_"int" i] (ArraySeq'new nil, a, i))
        ([#_"meta" meta, #_"Object[]" a, #_"int" i]
            (merge (ArraySeq'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"Object[]" :a a
                    #_"int" :i i
                )
            )
        )
    )

    (defm ArraySeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm ArraySeq IObj
        (#_"ArraySeq" IObj'''withMeta [#_"ArraySeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (ArraySeq'new meta, (:a this), (:i this))
            )
        )
    )

    (defn #_"ArraySeq" ArraySeq'create [#_"Object[]" a]
        (when (and (some? a) (pos? (count a)))
            (ArraySeq'new a, 0)
        )
    )

    (-/extend-protocol Seqable Object'array
        (#_"ArraySeq" Seqable'''seq [#_"Object[]" a] (#_ArraySeq'create -/seq a))
    )

    (defm ArraySeq ISeq
        (#_"Object" ISeq'''first [#_"ArraySeq" this]
            (when (some? (:a this))
                (aget (:a this) (:i this))
            )
        )

        (#_"seq" ISeq'''next [#_"ArraySeq" this]
            (when (and (some? (:a this)) (< (inc (:i this)) (count (:a this))))
                (ArraySeq'new (:a this), (inc (:i this)))
            )
        )
    )

    (defm ArraySeq Counted
        (#_"int" Counted'''count [#_"ArraySeq" this]
            (if (some? (:a this)) (- (count (:a this)) (:i this)) 0)
        )
    )

    (defm ArraySeq IReduce
        (#_"Object" IReduce'''reduce
            ([#_"ArraySeq" this, #_"fn" f]
                (when-some [#_"Object[]" a (:a this)]
                    (let [#_"int" i (:i this) #_"int" n (count a)]
                        (loop-when [#_"Object" r (aget a i) i (inc i)] (< i n) => r
                            (let [r (f r (aget a i))]
                                (if (reduced? r) @r (recur r (inc i)))
                            )
                        )
                    )
                )
            )
            ([#_"ArraySeq" this, #_"fn" f, #_"Object" r]
                (when-some [#_"Object[]" a (:a this)]
                    (let [#_"int" i (:i this) #_"int" n (count a)]
                        (loop-when [r (f r (aget a i)) i (inc i)] (< i n) => (if (reduced? r) @r r)
                            (if (reduced? r) @r (recur (f r (aget a i)) (inc i)))
                        )
                    )
                )
            )
        )
    )

    (defm ArraySeq Sequential)

    (defm ArraySeq Seqable
        (#_"seq" Seqable'''seq [#_"ArraySeq" this]
            this
        )
    )

    (defm ArraySeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm ArraySeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.ATransientMap"

(about #_"ATransientMap"
    (defr ATransientMap [])

    #_inherit
    (defm ATransientMap AFn)

    #_abstract
    (defm ATransientMap IATransientMap)

    (defn #_"ATransientMap" ATransientMap'new []
        (ATransientMap'class.)
    )

    (defm ATransientMap IFn
        (#_"Object" IFn'''invoke
            ([#_"ATransientMap" this, #_"Object" key] (get this key))
            ([#_"ATransientMap" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
        )

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )

    (defm ATransientMap ILookup
        (#_"Object" ILookup'''valAt
            ([#_"ATransientMap" this, #_"Object" key] (ILookup'''valAt this, key, nil))
            ([#_"ATransientMap" this, #_"Object" key, #_"Object" not-found]
                (IATransientMap'''assertEditable this)
                (IATransientMap'''doValAt this, key, not-found)
            )
        )
    )

    (def- #_"Object" ATransientMap'NOT_FOUND (Object.))

    (defm ATransientMap ITransientAssociative
        (#_"ITransientMap" ITransientAssociative'''assoc! [#_"ATransientMap" this, #_"Object" key, #_"Object" val]
            (IATransientMap'''assertEditable this)
            (IATransientMap'''doAssoc this, key, val)
        )

        (#_"boolean" ITransientAssociative'''containsKey [#_"ATransientMap" this, #_"Object" key]
            (not (identical? (get this key ATransientMap'NOT_FOUND) ATransientMap'NOT_FOUND))
        )

        (#_"IMapEntry" ITransientAssociative'''entryAt [#_"ATransientMap" this, #_"Object" key]
            (let [#_"Object" v (get this key ATransientMap'NOT_FOUND)]
                (when-not (identical? v ATransientMap'NOT_FOUND)
                    (MapEntry'new key, v)
                )
            )
        )
    )

    (defm ATransientMap ITransientMap
        (#_"ITransientMap" ITransientMap'''dissoc! [#_"ATransientMap" this, #_"Object" key]
            (IATransientMap'''assertEditable this)
            (IATransientMap'''doDissoc this, key)
        )
    )

    (defm ATransientMap ITransientCollection
        (#_"ITransientMap" ITransientCollection'''conj! [#_"ATransientMap" this, #_"Object" o]
            (IATransientMap'''assertEditable this)
            (condp satisfies? o
                IMapEntry
                    (assoc this (key o) (val o))
                IPersistentVector
                    (when (= (count o) 2) => (throw! "vector arg to map conj must be a pair")
                        (assoc this (nth o 0) (nth o 1))
                    )
                #_else
                    (loop-when [this this #_"seq" s (seq o)] (some? s) => this
                        (let [#_"IMapEntry" e (first s)]
                            (recur (assoc this (key e) (val e)) (next s))
                        )
                    )
            )
        )

        (#_"IPersistentMap" ITransientCollection'''persistent! [#_"ATransientMap" this]
            (IATransientMap'''assertEditable this)
            (IATransientMap'''doPersistent this)
        )
    )

    (defm ATransientMap Counted
        (#_"int" Counted'''count [#_"ATransientMap" this]
            (IATransientMap'''assertEditable this)
            (IATransientMap'''doCount this)
        )
    )
)
)

(about #_"cloiure.core.ATransientSet"

(about #_"ATransientSet"
    (defr ATransientSet [])

    #_inherit
    (defm ATransientSet AFn)

    (defn #_"ATransientSet" ATransientSet'new [#_"ITransientMap" impl]
        (merge (ATransientSet'class.)
            (hash-map
                #_"ITransientMap" :impl impl
            )
        )
    )

    (defm ATransientSet Counted
        (#_"int" Counted'''count [#_"ATransientSet" this]
            (count (:impl this))
        )
    )

    (defm ATransientSet ITransientCollection
        (#_"ITransientSet" ITransientCollection'''conj! [#_"ATransientSet" this, #_"Object" val]
            (let [#_"ITransientMap" m (assoc (:impl this) val val)]
                (when-not (= m (:impl this)) => this
                    (assoc this :impl m)
                )
            )
        )

        ;; abstract ITransientCollection persistent!
    )

    (defm ATransientSet ITransientSet
        (#_"ITransientSet" ITransientSet'''disj! [#_"ATransientSet" this, #_"Object" key]
            (let [#_"ITransientMap" m (dissoc (:impl this) key)]
                (when-not (= m (:impl this)) => this
                    (assoc this :impl m)
                )
            )
        )

        (#_"boolean" ITransientSet'''contains? [#_"ATransientSet" this, #_"Object" key]
            (not (identical? (get (:impl this) key this) this))
        )

        (#_"Object" ITransientSet'''get [#_"ATransientSet" this, #_"Object" key]
            (get (:impl this) key)
        )
    )

    (defm ATransientSet IFn
        (#_"Object" IFn'''invoke
            ([#_"ATransientSet" this, #_"Object" key] (get (:impl this) key))
            ([#_"ATransientSet" this, #_"Object" key, #_"Object" not-found] (get (:impl this) key not-found))
        )

        (#_"Object" IFn'''applyTo => AFn'applyToHelper)
    )
)
)

(about #_"cloiure.core.Cons"

(about #_"Cons"
    (defr Cons [])

    #_inherit
    (defm Cons ASeq)

    (defn #_"Cons" Cons'new
        ([#_"Object" _first, #_"seq" _more] (Cons'new nil, _first, _more))
        ([#_"meta" meta, #_"Object" _first, #_"seq" _more]
            (merge (Cons'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"Object" :_first _first
                    #_"seq" :_more _more
                )
            )
        )
    )

    (defm Cons IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm Cons IObj
        (#_"Cons" IObj'''withMeta [#_"Cons" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (Cons'new meta, (:_first this), (:_more this))
            )
        )
    )

    (defm Cons ISeq
        (#_"Object" ISeq'''first => :_first)

        (#_"seq" ISeq'''next [#_"Cons" this]
            (seq (:_more this))
        )
    )

    (defm Cons Counted
        (#_"int" Counted'''count [#_"Cons" this]
            (inc (count (:_more this)))
        )
    )

    (defm Cons Sequential)

    (defm Cons Seqable
        (#_"seq" Seqable'''seq [#_"Cons" this]
            this
        )
    )

    (defm Cons Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Cons IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.Delay"

(about #_"Delay"
    (defq Delay [f, o, e])

    (defn #_"Delay" Delay'new [#_"fn" f]
        (assoc!! (Delay'class. (anew 3))
            #_"fn'" :f (atom f)
            #_"Object'" :o (atom nil)
            #_"Throwable'" :e (atom nil)
        )
    )

    (defn #_"Object" Delay'force [#_"Object" x]
        (if (satisfies? Delay x) (deref x) x)
    )

    (defm Delay IDeref
        (#_"Object" IDeref'''deref [#_"Delay" this]
            (when (some? @(:f this))
                (locking this
                    ;; double check
                    (when-some [#_"fn" f @(:f this)]
                        (reset! (:f this) nil)
                        (try
                            (reset! (:o this) (f))
                            (catch Throwable t
                                (reset! (:e this) t)
                            )
                        )
                    )
                )
            )
            (when-some [#_"Throwable" e @(:e this)]
                (throw e)
            )
            @(:o this)
        )
    )

    (defm Delay IPending
        (#_"boolean" IPending'''isRealized [#_"Delay" this]
            (locking this
                (nil? @(:f this))
            )
        )
    )
)
)

(about #_"cloiure.core.Iterate"

(about #_"Iterate"
    (defq Iterate [_meta, f, x, y])

    #_inherit
    (defm Iterate ASeq)

    (defn- #_"Iterate" Iterate'new
        ([#_"fn" f, #_"Object" x, #_"Object" y] (Iterate'new nil, f, x, y))
        ([#_"meta" meta, #_"fn" f, #_"Object" x, #_"Object" y]
            (assoc!! (Iterate'class. (anew 4))
                #_"meta" :_meta meta
                #_"fn" :f f ;; never nil
                #_"Object" :x x
                #_"Object'" :y (atom y) ;; lazily realized
            )
        )
    )

    (defm Iterate IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm Iterate IObj
        (#_"Iterate" IObj'''withMeta [#_"Iterate" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (Iterate'new meta, (:f this), (:x this), @(:y this))
            )
        )
    )

    (defn #_"seq" Iterate'create [#_"fn" f, #_"Object" y] (Iterate'new f, nil, y))

    (def- #_"Object" Iterate'UNREALIZED (Object.))

    (defm Iterate IPending
        (#_"boolean" IPending'''isRealized [#_"Iterate" this]
            (not (identical? @(:y this) Iterate'UNREALIZED))
        )
    )

    (defm Iterate ISeq
        (#_"Object" ISeq'''first [#_"Iterate" this]
            (let-when [#_"Object" y @(:y this)] (identical? y Iterate'UNREALIZED) => y
                (reset! (:y this) ((:f this) (:x this)))
            )
        )

        #_memoize!
        (#_"seq" ISeq'''next [#_"Iterate" this]
            (Iterate'new (:f this), (first this), Iterate'UNREALIZED)
        )
    )

    (defm Iterate IReduce
        (#_"Object" IReduce'''reduce
            ([#_"Iterate" this, #_"fn" f]
                (loop [#_"Object" r (first this) #_"Object" v ((:f this) r)]
                    (let-when [r (f r v)] (reduced? r) => (recur r ((:f this) v))
                        @r
                    )
                )
            )
            ([#_"Iterate" this, #_"fn" f, #_"Object" r]
                (loop [r r #_"Object" v (first this)]
                    (let-when [r (f r v)] (reduced? r) => (recur r ((:f this) v))
                        @r
                    )
                )
            )
        )
    )

    (defm Iterate Sequential)

    (defm Iterate Seqable
        (#_"seq" Seqable'''seq [#_"Iterate" this]
            this
        )
    )

    (defm Iterate Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Iterate IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.KeywordLookupSite"

(about #_"KeywordLookupSite"
    (defr KeywordLookupSite [])

    (defn #_"KeywordLookupSite" KeywordLookupSite'new [#_"Keyword" k]
        (merge (KeywordLookupSite'class.)
            (hash-map
                #_"Keyword" :k k
            )
        )
    )

    (defn- #_"ILookupThunk" KeywordLookupSite''ilookupThunk [#_"KeywordLookupSite" this, #_"Class" c]
        (reify ILookupThunk
            (#_"Object" ILookupThunk'''get [#_"ILookupThunk" self, #_"Object" target]
                (if (and (some? target) (= (class target) c))
                    (ILookup'''valAt #_"ILookup" target, (:k this))
                    self
                )
            )
        )
    )

    (defm KeywordLookupSite ILookupSite
        (#_"ILookupThunk" ILookupSite'''fault [#_"KeywordLookupSite" this, #_"Object" target]
            (if (satisfies? ILookup target)
                (KeywordLookupSite''ilookupThunk this, (class target))
                this
            )
        )
    )

    (defm KeywordLookupSite ILookupThunk
        (#_"Object" ILookupThunk'''get [#_"KeywordLookupSite" this, #_"Object" target]
            (if (satisfies? ILookup target)
                this
                (get target (:k this))
            )
        )
    )
)
)

(about #_"cloiure.core.MethodImplCache"

(about #_"MethodImplCache"
    (defr MethodImplCache [])

    (defn #_"MethodImplCache" MethodImplCache'new [#_"map" protocol, #_"Keyword" methodk]
        (merge (MethodImplCache'class.)
            (hash-map
                #_"map" :protocol protocol
                #_"Keyword" :methodk methodk
            )
        )
    )

    (defn #_"MethodImplCache" MethodImplCache''assoc [#_"MethodImplCache" this, #_"Class" c, #_"fn" f]
        (assoc this (cast Class c) f)
    )

    (defn #_"fn" MethodImplCache''get [#_"MethodImplCache" this, #_"Class" c]
        (get this (cast Class c))
    )
)
)

(about #_"cloiure.core.Namespace"

(about #_"Namespace"
    (defr Namespace [])

    (def #_"{Symbol Namespace}'" Namespace'namespaces (atom {}))

    (defn #_"Namespace" Namespace'new [#_"Symbol" name]
        (merge (Namespace'class.)
            (hash-map
                #_"Symbol" :name name

                #_"{Symbol Class|Var}'" :mappings (atom {})
                #_"{Symbol Namespace}'" :aliases (atom {})
            )
        )
    )

    (defm Namespace IObject
        ;; abstract IObject equals

        (#_"String" IObject'''toString [#_"Namespace" this]
            (:name (:name this))
        )
    )

    (defn #_"seq" Namespace'all []
        (vals @Namespace'namespaces)
    )

    (defn #_"map" Namespace''getMappings [#_"Namespace" this]
        @(:mappings this)
    )

    (defn #_"Object" Namespace''getMapping [#_"Namespace" this, #_"Symbol" name]
        (get @(:mappings this) name)
    )

    (defn- #_"void" Namespace''warnOrFailOnReplace [#_"Namespace" this, #_"Symbol" sym, #_"Object" o, #_"Var" var]
        (or
            (when (var? o)
                (when (= (:ns o) this) => (throw! (str sym " already refers to: " o " in namespace: " (:name this)))
                    :ok
                )
            )
            (.println *err*, (str "WARNING: " sym " already refers to: " o " in namespace: " (:name this) ", being replaced by: " var))
        )
        nil
    )

    (declare Var'new)

    (defn #_"Var" Namespace''intern [#_"Namespace" this, #_"Symbol" sym]
        (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
            (let [#_"Object" o
                    (or (get @(:mappings this) sym)
                        (let [#_"Var" v (Var'new this, sym)]
                            (swap! (:mappings this) assoc sym v)
                            v
                        )
                    )]
                (when-not (and (var? o) (= (:ns o) this)) => o
                    (let [#_"Var" v (Var'new this, sym)]
                        (Namespace''warnOrFailOnReplace this, sym, o, v)
                        (swap! (:mappings this) assoc sym v)
                        v
                    )
                )
            )
        )
    )

    (defn #_"Var" Namespace''referenceVar [#_"Namespace" this, #_"Symbol" sym, #_"Var" var]
        (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
            (let [#_"Object" o
                    (or (get @(:mappings this) sym)
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

    (defn- #_"boolean" Namespace'areDifferentInstancesOfSameClassName [#_"Class" c1, #_"Class" c2]
        (and (not= c1 c2) (= (.getName c1) (.getName c2)))
    )

    (defn #_"Class" Namespace''referenceClass [#_"Namespace" this, #_"Symbol" sym, #_"Class" cls]
        (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
            (let [#_"Class" c
                    (let [c (get @(:mappings this) sym)]
                        (when (or (nil? c) (Namespace'areDifferentInstancesOfSameClassName c, cls)) => c
                            (swap! (:mappings this) assoc sym cls)
                            cls
                        )
                    )]
                (when (= c cls) => (throw! (str sym " already refers to: " c " in namespace: " (:name this)))
                    c
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

    (defn #_"Class" Namespace''importClass [#_"Namespace" this, #_"Class" cls]
        (let [#_"String" s (.getName cls)]
            (Namespace''referenceClass this, (Symbol'intern (.substring s, (inc (.lastIndexOf s, (int \.))))), cls)
        )
    )

    (defn #_"Var" Namespace''refer [#_"Namespace" this, #_"Symbol" sym, #_"Var" var]
        (Namespace''referenceVar this, sym, var)
    )

    (defn #_"Namespace" Namespace'find [#_"Symbol" name]
        (get @Namespace'namespaces name)
    )

    (defn #_"Namespace" Namespace'findOrCreate [#_"Symbol" name]
        (or (Namespace'find name)
            (let [#_"Namespace" ns (Namespace'new name)]
                (swap! Namespace'namespaces assoc name ns)
                ns
            )
        )
    )

    (defn #_"Namespace" Namespace'remove [#_"Symbol" name]
        (when-not (= name 'arbace.core) => (throw! "cannot remove core namespace")
            (get (first (swap-vals! Namespace'namespaces dissoc name)) name)
        )
    )

    (defn #_"Var" Namespace''findInternedVar [#_"Namespace" this, #_"Symbol" name]
        (let [#_"Object" o (get @(:mappings this) name)]
            (when (and (var? o) (= (:ns o) this))
                o
            )
        )
    )

    (defn #_"map" Namespace''getAliases [#_"Namespace" this]
        @(:aliases this)
    )

    (defn #_"Namespace" Namespace''getAlias [#_"Namespace" this, #_"Symbol" alias]
        (get @(:aliases this) alias)
    )

    (defn #_"void" Namespace''addAlias [#_"Namespace" this, #_"Symbol" alias, #_"Namespace" ns]
        (when (and (some? alias) (some? ns)) => (throw! "expecting Symbol + Namespace")
            (let [#_"Object" o
                    (or (get @(:aliases this) alias)
                        (do
                            (swap! (:aliases this) assoc alias ns)
                            ns
                        )
                    )]
                ;; you can rebind an alias, but only to the initially-aliased namespace
                (when-not (= o ns)
                    (throw! (str "alias " alias " already exists in namespace " (:name this) ", aliasing " o))
                )
            )
        )
        nil
    )

    (defn #_"void" Namespace''removeAlias [#_"Namespace" this, #_"Symbol" alias]
        (swap! (:aliases this) dissoc alias)
        nil
    )
)
)

(about #_"cloiure.core.PersistentArrayMap"

(about #_"MSeq"
    (defr MSeq [])

    #_inherit
    (defm MSeq ASeq)

    (defn #_"MSeq" MSeq'new
        ([#_"Object[]" a, #_"int" i] (MSeq'new nil, a, i))
        ([#_"meta" meta, #_"Object[]" a, #_"int" i]
            (merge (MSeq'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"Object[]" :a a
                    #_"int" :i i
                )
            )
        )
    )

    (defm MSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm MSeq IObj
        (#_"MSeq" IObj'''withMeta [#_"MSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (MSeq'new meta, (:a this), (:i this))
            )
        )
    )

    (defm MSeq ISeq
        (#_"Object" ISeq'''first [#_"MSeq" this]
            (MapEntry'new (aget (:a this) (:i this)), (aget (:a this) (inc (:i this))))
        )

        (#_"seq" ISeq'''next [#_"MSeq" this]
            (when (< (+ (:i this) 2) (count (:a this)))
                (MSeq'new (:a this), (+ (:i this) 2))
            )
        )
    )

    (defm MSeq Counted
        (#_"int" Counted'''count [#_"MSeq" this]
            (quot (- (count (:a this)) (:i this)) 2)
        )
    )

    (defm MSeq Sequential)

    (defm MSeq Seqable
        (#_"seq" Seqable'''seq [#_"MSeq" this]
            this
        )
    )

    (defm MSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm MSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)

;;;
 ; Simple implementation of persistent map on an array.
 ;
 ; Note that instances of this class are constant values, i.e. add/remove etc return new values.
 ; Copies array on every change, so only appropriate for _very_small_ maps. nil keys and values are
 ; ok, but you won't be able to distinguish a nil value via valAt, use contains/entryAt for that.
 ;;
(about #_"PersistentArrayMap"
    (defr PersistentArrayMap [])

    #_inherit
    (defm PersistentArrayMap AFn APersistentMap)

    (def #_"int" PersistentArrayMap'HASHTABLE_THRESHOLD 16)

    (defn #_"PersistentArrayMap" PersistentArrayMap'new
        ([] (PersistentArrayMap'new nil))
        ;; This ctor captures/aliases the passed array, so do not modify it later.
        ([#_"Object[]" a] (PersistentArrayMap'new nil, a))
        ([#_"meta" meta, #_"Object[]" a]
            (merge (PersistentArrayMap'class.) (APersistentMap'new)
                (hash-map
                    #_"meta" :_meta meta
                    #_"Object[]" :a (or a (object-array 0))
                )
            )
        )
    )

    (defm PersistentArrayMap IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentArrayMap IObj
        (#_"PersistentArrayMap" IObj'''withMeta [#_"PersistentArrayMap" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentArrayMap'new meta, (:a this))
            )
        )
    )

    (def #_"PersistentArrayMap" PersistentArrayMap'EMPTY (PersistentArrayMap'new))

    (defn #_"PersistentArrayMap" PersistentArrayMap''create [#_"PersistentArrayMap" this & #_"Object..." init]
        (PersistentArrayMap'new (meta this), init)
    )

    (defn #_"PersistentArrayMap" PersistentArrayMap'createWithCheck [#_"Object[]" init]
        (loop-when-recur [#_"int" i 0] (< i (count init)) [(+ i 2)]
            (loop-when-recur [#_"int" j (+ i 2)] (< j (count init)) [(+ j 2)]
                (when (= (aget init i) (aget init j))
                    (throw! (str "duplicate key: " (aget init i)))
                )
            )
        )
        (PersistentArrayMap'new init)
    )

    (defn #_"PersistentArrayMap" PersistentArrayMap'createAsIfByAssoc [#_"Object[]" init]
        (when (odd? (count init))
            (throw! (str "no value supplied for key: " (aget init (dec (count init)))))
        )
        ;; If this looks like it is doing busy-work, it is because it is achieving these goals: O(n^2) run time
        ;; like createWithCheck(), never modify init arg, and only allocate memory if there are duplicate keys.
        (let [#_"int" n
                (loop-when [n 0 #_"int" i 0] (< i (count init)) => n
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
                (when (< n (count init)) => init
                    ;; Create a new, shorter array with unique keys, and the last value associated with each key.
                    ;; To behave like assoc, the first occurrence of each key must be used, since its metadata
                    ;; may be different than later equal keys.
                    (let [#_"Object[]" nodups (object-array n)
                          #_"int" m
                            (loop-when [m 0 #_"int" i 0] (< i (count init)) => m
                                (let [#_"boolean" dup?
                                        (loop-when [dup? false #_"int" j 0] (< j m) => dup?
                                            (or (= (aget init i) (aget nodups j))
                                                (recur dup? (+ j 2))
                                            )
                                        )
                                      m (when-not dup? => m
                                            (let [#_"int" j
                                                    (loop-when [j (- (count init) 2)] (<= i j) => j
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

    (defm PersistentArrayMap Counted
        (#_"int" Counted'''count [#_"PersistentArrayMap" this]
            (quot (count (:a this)) 2)
        )
    )

    (defn- #_"int" PersistentArrayMap''indexOf [#_"PersistentArrayMap" this, #_"Object" key]
        (loop-when [#_"int" i 0] (< i (count (:a this))) => -1
            (if (= key (aget (:a this) i)) i (recur (+ i 2)))
        )
    )

    (declare PersistentHashMap'create-1a)

    (defm PersistentArrayMap Associative
        (#_"IPersistentMap" Associative'''assoc [#_"PersistentArrayMap" this, #_"Object" key, #_"Object" val]
            (let [#_"int" i (PersistentArrayMap''indexOf this, key)]
                (if (<= 0 i) ;; already have key, same-sized replacement
                    (if (= (aget (:a this) (inc i)) val) ;; no change, no op
                        this
                        (PersistentArrayMap''create this, (doto (aclone (:a this)) (aset! (inc i) val)))
                    )
                    ;; didn't have key, grow
                    (if (< PersistentArrayMap'HASHTABLE_THRESHOLD (count (:a this)))
                        (-> (PersistentHashMap'create-1a (:a this)) (assoc key val) (with-meta (meta this)))
                        (let [
                            #_"int" n (count (:a this))
                            #_"Object[]" a (object-array (+ n 2))
                            _
                                (when (pos? n)
                                    (acopy! a 0 (:a this) 0 n)
                                )
                            _ (aset! a n key)
                            _ (aset! a (inc n) val)
                        ]
                            (PersistentArrayMap''create this, a)
                        )
                    )
                )
            )
        )

        (#_"boolean" Associative'''containsKey [#_"PersistentArrayMap" this, #_"Object" key]
            (<= 0 (PersistentArrayMap''indexOf this, key))
        )

        (#_"IMapEntry" Associative'''entryAt [#_"PersistentArrayMap" this, #_"Object" key]
            (let-when [#_"int" i (PersistentArrayMap''indexOf this, key)] (<= 0 i)
                (MapEntry'new (aget (:a this) i), (aget (:a this) (inc i)))
            )
        )
    )

    (declare empty)

    (defm PersistentArrayMap IPersistentMap
        (#_"IPersistentMap" IPersistentMap'''dissoc [#_"PersistentArrayMap" this, #_"Object" key]
            (let-when [#_"int" i (PersistentArrayMap''indexOf this, key)] (<= 0 i) => this ;; don't have key, no op
                ;; have key, will remove
                (let-when [#_"int" n (- (count (:a this)) 2)] (pos? n) => (empty this)
                    (let [
                        #_"Object[]" a (doto (object-array n) (acopy! 0 (:a this) 0 i) (acopy! i (:a this) (+ i 2) (- n i)))
                    ]
                        (PersistentArrayMap''create this, a)
                    )
                )
            )
        )
    )

    (defm PersistentArrayMap IPersistentCollection
        ;; inherit APersistentMap conj

        (#_"IPersistentMap" IPersistentCollection'''empty [#_"PersistentArrayMap" this]
            (with-meta PersistentArrayMap'EMPTY (meta this))
        )
    )

    (defm PersistentArrayMap ILookup
        (#_"Object" ILookup'''valAt
            ([#_"PersistentArrayMap" this, #_"Object" key] (ILookup'''valAt this, key, nil))
            ([#_"PersistentArrayMap" this, #_"Object" key, #_"Object" not-found]
                (let [#_"int" i (PersistentArrayMap''indexOf this, key)]
                    (if (<= 0 i) (aget (:a this) (inc i)) not-found)
                )
            )
        )
    )

    (defn #_"int" PersistentArrayMap''capacity [#_"PersistentArrayMap" this]
        (count this)
    )

    (defm PersistentArrayMap Seqable
        (#_"seq" Seqable'''seq [#_"PersistentArrayMap" this]
            (when (pos? (count (:a this)))
                (MSeq'new (:a this), 0)
            )
        )
    )

    (defm PersistentArrayMap IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"PersistentArrayMap" this, #_"fn" f, #_"Object" r]
            (loop-when [r r #_"int" i 0] (< i (count (:a this))) => r
                (let [r (f r (aget (:a this) i), (aget (:a this) (inc i)))]
                    (if (reduced? r) @r (recur r (+ i 2)))
                )
            )
        )
    )

    (declare TransientArrayMap'new)

    (defm PersistentArrayMap IEditableCollection
        (#_"ITransientMap" IEditableCollection'''asTransient [#_"PersistentArrayMap" this]
            (TransientArrayMap'new (:a this))
        )
    )

    (§ inherit PersistentArrayMap APersistentMap IObject'''equals IObject'''toString Hashed'''hash IFn'''invoke IFn'''applyTo)
)

(about #_"TransientArrayMap"
    (defr TransientArrayMap [])

    #_inherit
    (defm TransientArrayMap AFn ATransientMap)

    (defn #_"TransientArrayMap" TransientArrayMap'new [#_"Object[]" a]
        (let [#_"int" n (count a)]
            (merge (TransientArrayMap'class.) (ATransientMap'new)
                (hash-map
                    #_"Object[]" :a (doto (object-array (max PersistentArrayMap'HASHTABLE_THRESHOLD n)) (acopy! 0 a 0 n))
                    #_"int" :n n

                    #_"Thread'" :edit (atom (thread))
                )
            )
        )
    )

    (defn- #_"int" TransientArrayMap''indexOf [#_"TransientArrayMap" this, #_"Object" key]
        (loop-when [#_"int" i 0] (< i (:n this)) => -1
            (if (= (aget (:a this) i) key) i (recur (+ i 2)))
        )
    )

    (defm TransientArrayMap IATransientMap
        (#_"void" IATransientMap'''assertEditable [#_"TransientArrayMap" this]
            (or @(:edit this) (throw! "transient used after persistent! call"))
            nil
        )

        (#_"ITransientMap" IATransientMap'''doAssoc [#_"TransientArrayMap" this, #_"Object" key, #_"Object" val]
            (let [#_"int" i (TransientArrayMap''indexOf this, key)]
                (cond (<= 0 i) ;; already have key,
                    (do
                        (when-not (= (aget (:a this) (inc i)) val) ;; no change, no op
                            (aset! (:a this) (inc i) val)
                        )
                        this
                    )
                    :else ;; didn't have key, grow
                    (if (< (:n this) (count (:a this)))
                        (let [_ (aset! (:a this) (:n this) key) this (update this :n inc)
                              _ (aset! (:a this) (:n this) val) this (update this :n inc)]
                            this
                        )
                        (-> (PersistentHashMap'create-1a (:a this)) (transient) (assoc key val))
                    )
                )
            )
        )

        (#_"ITransientMap" IATransientMap'''doDissoc [#_"TransientArrayMap" this, #_"Object" key]
            (let-when [#_"int" i (TransientArrayMap''indexOf this, key)] (<= 0 i) => this
                ;; have key, will remove
                (when (<= 2 (:n this))
                    (aset! (:a this) i (aget (:a this) (- (:n this) 2)))
                    (aset! (:a this) (inc i) (aget (:a this) (- (:n this) 1)))
                )
                (update this :n - 2)
            )
        )

        (#_"Object" IATransientMap'''doValAt [#_"TransientArrayMap" this, #_"Object" key, #_"Object" not-found]
            (let [#_"int" i (TransientArrayMap''indexOf this, key)]
                (if (<= 0 i) (aget (:a this) (inc i)) not-found)
            )
        )

        (#_"int" IATransientMap'''doCount [#_"TransientArrayMap" this]
            (quot (:n this) 2)
        )

        (#_"IPersistentMap" IATransientMap'''doPersistent [#_"TransientArrayMap" this]
            (IATransientMap'''assertEditable this)
            (reset! (:edit this) nil)
            (PersistentArrayMap'new (doto (object-array (:n this)) (acopy! 0 (:a this) 0 (:n this))))
        )
    )

    (§ inherit TransientArrayMap ATransientMap IFn'''invoke IFn'''applyTo ILookup'''valAt ITransientAssociative'''assoc! ITransientAssociative'''containsKey ITransientAssociative'''entryAt ITransientMap'''dissoc! ITransientCollection'''conj! ITransientCollection'''persistent! Counted'''count)
)
)

(about #_"cloiure.core.PersistentHashMap"

(about #_"HSeq"
    (defr HSeq [])

    #_inherit
    (defm HSeq ASeq)

    (defn- #_"HSeq" HSeq'new [#_"meta" meta, #_"INode[]" nodes, #_"int" i, #_"seq" s]
        (merge (HSeq'class.)
            (hash-map
                #_"meta" :_meta meta

                #_"INode[]" :nodes nodes
                #_"int" :i i
                #_"seq" :s s
            )
        )
    )

    (defm HSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm HSeq IObj
        (#_"HSeq" IObj'''withMeta [#_"HSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (HSeq'new meta, (:nodes this), (:i this), (:s this))
            )
        )
    )

    (defn- #_"seq" HSeq'create-4 [#_"meta" meta, #_"INode[]" nodes, #_"int" i, #_"seq" s]
        (when (nil? s) => (HSeq'new meta, nodes, i, s)
            (loop-when i (< i (count nodes))
                (let-when [#_"INode" ai (aget nodes i)] (some? ai) => (recur (inc i))
                    (let-when [s (INode'''nodeSeq ai)] (some? s) => (recur (inc i))
                        (HSeq'new meta, nodes, (inc i), s)
                    )
                )
            )
        )
    )

    (defn #_"seq" HSeq'create-1 [#_"INode[]" nodes]
        (HSeq'create-4 nil, nodes, 0, nil)
    )

    (defm HSeq ISeq
        (#_"Object" ISeq'''first [#_"HSeq" this]
            (first (:s this))
        )

        (#_"seq" ISeq'''next [#_"HSeq" this]
            (HSeq'create-4 nil, (:nodes this), (:i this), (next (:s this)))
        )
    )

    (defm HSeq Sequential)

    (defm HSeq Seqable
        (#_"seq" Seqable'''seq [#_"HSeq" this]
            this
        )
    )

    (defm HSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm HSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)

(about #_"NodeSeq"
    (defr NodeSeq [])

    #_inherit
    (defm NodeSeq ASeq)

    (defn #_"NodeSeq" NodeSeq'new
        ([#_"Object[]" a, #_"int" i] (NodeSeq'new nil, a, i, nil))
        ([#_"meta" meta, #_"Object[]" a, #_"int" i, #_"seq" s]
            (merge (NodeSeq'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"Object[]" :a a
                    #_"int" :i i
                    #_"seq" :s s
                )
            )
        )
    )

    (defm NodeSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm NodeSeq IObj
        (#_"NodeSeq" IObj'''withMeta [#_"NodeSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (NodeSeq'new meta, (:a this), (:i this), (:s this))
            )
        )
    )

    (defn- #_"seq" NodeSeq'create-3 [#_"Object[]" a, #_"int" i, #_"seq" s]
        (when (nil? s) => (NodeSeq'new nil, a, i, s)
            (loop-when i (< i (count a))
                (when (nil? (aget a i)) => (NodeSeq'new nil, a, i, nil)
                    (or
                        (when-some [#_"INode" node #_"INode" (aget a (inc i))]
                            (when-some [s (INode'''nodeSeq node)]
                                (NodeSeq'new nil, a, (+ i 2), s)
                            )
                        )
                        (recur (+ i 2))
                    )
                )
            )
        )
    )

    (defn #_"seq" NodeSeq'create-1 [#_"Object[]" a]
        (NodeSeq'create-3 a, 0, nil)
    )

    (defm NodeSeq ISeq
        (#_"Object" ISeq'''first [#_"NodeSeq" this]
            (if (some? (:s this))
                (first (:s this))
                (MapEntry'new (aget (:a this) (:i this)), (aget (:a this) (inc (:i this))))
            )
        )

        (#_"seq" ISeq'''next [#_"NodeSeq" this]
            (if (some? (:s this))
                (NodeSeq'create-3 (:a this), (:i this), (next (:s this)))
                (NodeSeq'create-3 (:a this), (+ (:i this) 2), nil)
            )
        )
    )

    (defn #_"Object" NodeSeq'kvreduce [#_"Object[]" a, #_"fn" f, #_"Object" r]
        (loop-when [r r #_"int" i 0] (< i (count a)) => r
            (let [r (if (some? (aget a i))
                        (f r (aget a i), (aget a (inc i)))
                        (let-when [#_"INode" node #_"INode" (aget a (inc i))] (some? node) => r
                            (INode'''kvreduce node, f, r)
                        )
                    )]
                (when-not (reduced? r) => r
                    (recur r (+ i 2))
                )
            )
        )
    )

    (defm NodeSeq Sequential)

    (defm NodeSeq Seqable
        (#_"seq" Seqable'''seq [#_"NodeSeq" this]
            this
        )
    )

    (defm NodeSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm NodeSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)

(about #_"PersistentHashMap"
    (defn #_"int" PersistentHashMap'mask [#_"int" hash, #_"int" shift]
        (& (>>> hash shift) 0x1f)
    )

    (defn- #_"int" PersistentHashMap'bitpos [#_"int" hash, #_"int" shift]
        (<< 1 (PersistentHashMap'mask hash, shift))
    )

    (defn- #_"Object[]" PersistentHashMap'cloneAndSet
        ([#_"Object[]" a, #_"int" i, #_"Object" x]                          (doto (aclone a) (aset! i x)))
        ([#_"Object[]" a, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y] (doto (aclone a) (aset! i x) (aset! j y)))
    )

    (defn- #_"Object[]" PersistentHashMap'removePair [#_"Object[]" a, #_"int" i]
        (let [#_"int" n (- (count a) 2) #_"int" m (* 2 i)]
            (doto (object-array n) (acopy! 0 a 0 m) (acopy! m a (+ m 2) (- n m)))
        )
    )
)

(about #_"ArrayNode"
    (defr ArrayNode [])

    (defn #_"ArrayNode" ArrayNode'new [#_"Thread'" edit, #_"int" n, #_"INode[]" a]
        (merge (ArrayNode'class.)
            (hash-map
                #_"Thread'" :edit edit
                #_"int" :n n
                #_"INode[]" :a a
            )
        )
    )

    (defn- #_"ArrayNode" ArrayNode''ensureEditable [#_"ArrayNode" this, #_"Thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (ArrayNode'new edit, (:n this), (aclone (:a this)))
        )
    )

    (defn- #_"ArrayNode" ArrayNode''editAndSet [#_"ArrayNode" this, #_"Thread'" edit, #_"int" i, #_"INode" node]
        (let [#_"ArrayNode" e (ArrayNode''ensureEditable this, edit)]
            (aset! (:a e) i node)
            e
        )
    )

    (declare BitmapIndexedNode'new)

    (defn- #_"INode" ArrayNode''pack [#_"ArrayNode" this, #_"Thread'" edit, #_"int" idx]
        (let [#_"Object[]" a' (object-array (* 2 (dec (:n this))))
              [#_"int" bitmap #_"int" j]
                (loop-when [bitmap 0 j 1 #_"int" i 0] (< i idx) => [bitmap j]
                    (let [[bitmap j]
                            (when (some? (aget (:a this) i)) => [bitmap j]
                                (aset! a' j (aget (:a this) i))
                                [(| bitmap (<< 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )
              bitmap
                (loop-when [bitmap bitmap j j #_"int" i (inc idx)] (< i (count (:a this))) => bitmap
                    (let [[bitmap j]
                            (when (some? (aget (:a this) i)) => [bitmap j]
                                (aset! a' j (aget (:a this) i))
                                [(| bitmap (<< 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )]
            (BitmapIndexedNode'new edit, bitmap, a')
        )
    )

    (declare BitmapIndexedNode'EMPTY)

    (defm ArrayNode INode
        (#_"INode" INode'''assoc [#_"ArrayNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf]
            (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"INode" ai (aget (:a this) i)]
                (if (some? ai)
                    (let [#_"INode" node (INode'''assoc ai, (+ shift 5), hash, key, val, addedLeaf)]
                        (when-not (= node ai) => this
                            (ArrayNode'new nil, (:n this), (PersistentHashMap'cloneAndSet (:a this), i, node))
                        )
                    )
                    (let [#_"INode" node (INode'''assoc BitmapIndexedNode'EMPTY, (+ shift 5), hash, key, val, addedLeaf)]
                        (ArrayNode'new nil, (inc (:n this)), (PersistentHashMap'cloneAndSet (:a this), i, node))
                    )
                )
            )
        )

        (#_"INode" INode'''dissoc [#_"ArrayNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
            (let-when [#_"int" i (PersistentHashMap'mask hash, shift) #_"INode" ai (aget (:a this) i)] (some? ai) => this
                (let-when-not [#_"INode" node (INode'''dissoc ai, (+ shift 5), hash, key)] (= node ai) => this
                    (cond
                        (some? node)     (ArrayNode'new nil, (:n this), (PersistentHashMap'cloneAndSet (:a this), i, node))
                        (<= (:n this) 8) (ArrayNode''pack this, nil, i) ;; shrink
                        :else            (ArrayNode'new nil, (dec (:n this)), (PersistentHashMap'cloneAndSet (:a this), i, node))
                    )
                )
            )
        )

        (#_"IMapEntry|Object" INode'''find
            ([#_"ArrayNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
                (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"INode" node (aget (:a this) i)]
                    (when (some? node)
                        (INode'''find node, (+ shift 5), hash, key)
                    )
                )
            )
            ([#_"ArrayNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not-found]
                (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"INode" node (aget (:a this) i)]
                    (when (some? node) => not-found
                        (INode'''find node, (+ shift 5), hash, key, not-found)
                    )
                )
            )
        )

        (#_"seq" INode'''nodeSeq [#_"ArrayNode" this]
            (HSeq'create-1 (:a this))
        )

        (#_"INode" INode'''assocT [#_"ArrayNode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf]
            (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"INode" ai (aget (:a this) i)]
                (if (some? ai)
                    (let [#_"INode" node (INode'''assocT ai, edit, (+ shift 5), hash, key, val, addedLeaf)]
                        (when-not (= node ai) => this
                            (ArrayNode''editAndSet this, edit, i, node)
                        )
                    )
                    (-> (ArrayNode''editAndSet this, edit, i, (INode'''assocT BitmapIndexedNode'EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf))
                        (update :n inc)
                    )
                )
            )
        )

        (#_"INode" INode'''dissocT [#_"ArrayNode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"boolean'" removedLeaf]
            (let-when [#_"int" i (PersistentHashMap'mask hash, shift) #_"INode" ai (aget (:a this) i)] (some? ai) => this
                (let-when-not [#_"INode" node (INode'''dissocT ai, edit, (+ shift 5), hash, key, removedLeaf)] (= node ai) => this
                    (cond
                        (some? node)     (ArrayNode''editAndSet this, edit, i, node)
                        (<= (:n this) 8) (ArrayNode''pack this, edit, i) ;; shrink
                        :else            (-> (ArrayNode''editAndSet this, edit, i, node) (update :n dec))
                    )
                )
            )
        )

        (#_"Object" INode'''kvreduce [#_"ArrayNode" this, #_"fn" f, #_"Object" r]
            (let [#_"INode[]" a (:a this)]
                (loop-when [r r #_"int" i 0] (< i (count a)) => r
                    (let-when [#_"INode" node (aget a i)] (some? node) => (recur r (inc i))
                        (let [r (INode'''kvreduce node, f, r)]
                            (when-not (reduced? r) => r
                                (recur r (inc i))
                            )
                        )
                    )
                )
            )
        )
    )
)

(about #_"BitmapIndexedNode"
    (defr BitmapIndexedNode [])

    (defn #_"BitmapIndexedNode" BitmapIndexedNode'new [#_"Thread'" edit, #_"int" bitmap, #_"Object[]" a]
        (merge (BitmapIndexedNode'class.)
            (hash-map
                #_"Thread'" :edit edit
                #_"int" :bitmap bitmap
                #_"Object[]" :a a
            )
        )
    )

    (def #_"BitmapIndexedNode" BitmapIndexedNode'EMPTY (BitmapIndexedNode'new nil, 0, (object-array 0)))

    (defn #_"int" BitmapIndexedNode''index [#_"BitmapIndexedNode" this, #_"int" bit]
        (Integer/bitCount (& (:bitmap this) (dec bit)))
    )

    (declare HashCollisionNode'new)

    (defn- #_"INode" BitmapIndexedNode'createNode-6 [#_"int" shift, #_"Object" key1, #_"Object" val1, #_"int" key2hash, #_"Object" key2, #_"Object" val2]
        (let [#_"int" key1hash (f'hash key1)]
            (when-not (= key1hash key2hash) => (HashCollisionNode'new nil, key1hash, 2, (object-array [ key1, val1, key2, val2 ]))
                (let [#_"boolean'" addedLeaf (atom false) #_"Thread'" edit (atom nil)]
                    (-> BitmapIndexedNode'EMPTY
                        (INode'''assocT edit, shift, key1hash, key1, val1, addedLeaf)
                        (INode'''assocT edit, shift, key2hash, key2, val2, addedLeaf)
                    )
                )
            )
        )
    )

    (defn- #_"BitmapIndexedNode" BitmapIndexedNode''ensureEditable [#_"BitmapIndexedNode" this, #_"Thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (let [#_"int" n (Integer/bitCount (:bitmap this)) #_"Object[]" a' (object-array (* 2 (inc n)))] ;; make room for next assoc
                (acopy! a' 0 (:a this) 0 (* 2 n))
                (BitmapIndexedNode'new edit, (:bitmap this), a')
            )
        )
    )

    (defn- #_"BitmapIndexedNode" BitmapIndexedNode''editAndSet-4 [#_"BitmapIndexedNode" this, #_"Thread'" edit, #_"int" i, #_"Object" x]
        (let [#_"BitmapIndexedNode" e (BitmapIndexedNode''ensureEditable this, edit)]
            (aset! (:a e) i x)
            e
        )
    )

    (defn- #_"BitmapIndexedNode" BitmapIndexedNode''editAndSet-6 [#_"BitmapIndexedNode" this, #_"Thread'" edit, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y]
        (let [#_"BitmapIndexedNode" e (BitmapIndexedNode''ensureEditable this, edit)]
            (aset! (:a e) i x)
            (aset! (:a e) j y)
            e
        )
    )

    (defn- #_"BitmapIndexedNode" BitmapIndexedNode''editAndRemovePair [#_"BitmapIndexedNode" this, #_"Thread'" edit, #_"int" bit, #_"int" i]
        (when-not (= (:bitmap this) bit)
            (let [#_"BitmapIndexedNode" e (-> (BitmapIndexedNode''ensureEditable this, edit) (update :bitmap bit-xor bit))
                  #_"Object[]" a (:a e) #_"int" n (count a)]
                (acopy! a (* 2 i) a (* 2 (inc i)) (- n (* 2 (inc i))))
                (aset! a (- n 2) nil)
                (aset! a (- n 1) nil)
                e
            )
        )
    )

    (defn- #_"INode" BitmapIndexedNode'createNode-7 [#_"Thread'" edit, #_"int" shift, #_"Object" key1, #_"Object" val1, #_"int" key2hash, #_"Object" key2, #_"Object" val2]
        (let [#_"int" key1hash (f'hash key1)]
            (when-not (= key1hash key2hash) => (HashCollisionNode'new nil, key1hash, 2, (object-array [ key1, val1, key2, val2 ]))
                (let [#_"boolean'" addedLeaf (atom false)]
                    (-> BitmapIndexedNode'EMPTY
                        (INode'''assocT edit, shift, key1hash, key1, val1, addedLeaf)
                        (INode'''assocT edit, shift, key2hash, key2, val2, addedLeaf)
                    )
                )
            )
        )
    )

    (defm BitmapIndexedNode INode
        (#_"INode" INode'''assoc [#_"BitmapIndexedNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf]
            (let [#_"int" bit (PersistentHashMap'bitpos hash, shift) #_"int" idx (BitmapIndexedNode''index this, bit)]
                (if-not (zero? (& (:bitmap this) bit))
                    (let [#_"Object" keyOrNull (aget (:a this) (* 2 idx))
                          #_"Object" valOrNode (aget (:a this) (inc (* 2 idx)))
                          _ (cond
                                (nil? keyOrNull)
                                    (let [#_"INode" node (INode'''assoc #_"INode" valOrNode, (+ shift 5), hash, key, val, addedLeaf)]
                                        (when-not (= node valOrNode)
                                            (PersistentHashMap'cloneAndSet (:a this), (inc (* 2 idx)), node)
                                        )
                                    )
                                (= key keyOrNull)
                                    (when-not (= val valOrNode)
                                        (PersistentHashMap'cloneAndSet (:a this), (inc (* 2 idx)), val)
                                    )
                                :else
                                    (let [_ (reset! addedLeaf true)]
                                        (PersistentHashMap'cloneAndSet (:a this), (* 2 idx), nil, (inc (* 2 idx)), (BitmapIndexedNode'createNode-6 (+ shift 5), keyOrNull, valOrNode, hash, key, val))
                                    )
                            )]
                        (if (some? _) (BitmapIndexedNode'new nil, (:bitmap this), _) this)
                    )
                    (let [#_"int" n (Integer/bitCount (:bitmap this))]
                        (if (<= 16 n)
                            (let [#_"INode[]" nodes (make-array #_"INode" Object 32) #_"int" jdx (PersistentHashMap'mask hash, shift)]
                                (aset! nodes jdx (INode'''assoc BitmapIndexedNode'EMPTY, (+ shift 5), hash, key, val, addedLeaf))
                                (loop-when [#_"int" j 0 #_"int" i 0] (< i 32)
                                    (when (odd? (>>> (:bitmap this) i)) => (recur j (inc i))
                                        (if (some? (aget (:a this) j))
                                            (aset! nodes i (INode'''assoc BitmapIndexedNode'EMPTY, (+ shift 5), (f'hash (aget (:a this) j)), (aget (:a this) j), (aget (:a this) (inc j)), addedLeaf))
                                            (aset! nodes i #_"INode" (aget (:a this) (inc j)))
                                        )
                                        (recur (+ j 2) (inc i))
                                    )
                                )
                                (ArrayNode'new nil, (inc n), nodes)
                            )
                            (let [#_"Object[]" a' (object-array (* 2 (inc n)))]
                                (acopy! a' 0 (:a this) 0 (* 2 idx))
                                (aset! a' (* 2 idx) key)
                                (reset! addedLeaf true)
                                (aset! a' (inc (* 2 idx)) val)
                                (acopy! a' (* 2 (inc idx)) (:a this) (* 2 idx) (* 2 (- n idx)))
                                (BitmapIndexedNode'new nil, (| (:bitmap this) bit), a')
                            )
                        )
                    )
                )
            )
        )

        (#_"INode" INode'''dissoc [#_"BitmapIndexedNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
            (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit)) => this
                (let [#_"int" i (BitmapIndexedNode''index this, bit) #_"int" ii (* 2 i)
                      #_"Object" keyOrNull (aget (:a this) ii)
                      #_"Object" valOrNode (aget (:a this) (inc ii))]
                    (if (some? keyOrNull)
                        (when (= key keyOrNull) => this
                            ;; TODO: collapse
                            (BitmapIndexedNode'new nil, (bit-xor (:bitmap this) bit), (PersistentHashMap'removePair (:a this), i))
                        )
                        (let [#_"INode" node (INode'''dissoc #_"INode" valOrNode, (+ shift 5), hash, key)]
                            (cond
                                (= node valOrNode)
                                    this
                                (some? node)
                                    (BitmapIndexedNode'new nil, (:bitmap this), (PersistentHashMap'cloneAndSet (:a this), (inc ii), node))
                                (= (:bitmap this) bit)
                                    nil
                                :else
                                    (BitmapIndexedNode'new nil, (bit-xor (:bitmap this) bit), (PersistentHashMap'removePair (:a this), i))
                            )
                        )
                    )
                )
            )
        )

        (#_"IMapEntry|Object" INode'''find
            ([#_"BitmapIndexedNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
                (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit))
                    (let [#_"int" i (BitmapIndexedNode''index this, bit)
                        #_"Object" keyOrNull (aget (:a this) (* 2 i))
                        #_"Object" valOrNode (aget (:a this) (inc (* 2 i)))]
                        (cond
                            (nil? keyOrNull)  (INode'''find #_"INode" valOrNode, (+ shift 5), hash, key)
                            (= key keyOrNull) (MapEntry'new keyOrNull, valOrNode)
                        )
                    )
                )
            )
            ([#_"BitmapIndexedNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not-found]
                (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit)) => not-found
                    (let [#_"int" i (BitmapIndexedNode''index this, bit)
                        #_"Object" keyOrNull (aget (:a this) (* 2 i))
                        #_"Object" valOrNode (aget (:a this) (inc (* 2 i)))]
                        (cond
                            (nil? keyOrNull)  (INode'''find #_"INode" valOrNode, (+ shift 5), hash, key, not-found)
                            (= key keyOrNull) valOrNode
                            :else             not-found
                        )
                    )
                )
            )
        )

        (#_"seq" INode'''nodeSeq [#_"BitmapIndexedNode" this]
            (NodeSeq'create-1 (:a this))
        )

        (#_"INode" INode'''assocT [#_"BitmapIndexedNode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf]
            (let [#_"int" bit (PersistentHashMap'bitpos hash, shift) #_"int" idx (BitmapIndexedNode''index this, bit)]
                (if-not (zero? (& (:bitmap this) bit))
                    (let [#_"Object" keyOrNull (aget (:a this) (* 2 idx))
                          #_"Object" valOrNode (aget (:a this) (inc (* 2 idx)))]
                        (cond
                            (nil? keyOrNull)
                                (let [#_"INode" node (INode'''assocT #_"INode" valOrNode, edit, (+ shift 5), hash, key, val, addedLeaf)]
                                    (when-not (= node valOrNode) => this
                                        (BitmapIndexedNode''editAndSet-4 this, edit, (inc (* 2 idx)), node)
                                    )
                                )
                            (= key keyOrNull)
                                (when-not (= val valOrNode) => this
                                    (BitmapIndexedNode''editAndSet-4 this, edit, (inc (* 2 idx)), val)
                                )
                            :else
                                (let [_ (reset! addedLeaf true)]
                                    (BitmapIndexedNode''editAndSet-6 this, edit, (* 2 idx), nil, (inc (* 2 idx)), (BitmapIndexedNode'createNode-7 edit, (+ shift 5), keyOrNull, valOrNode, hash, key, val))
                                )
                        )
                    )
                    (let [#_"int" n (Integer/bitCount (:bitmap this))]
                        (cond
                            (< (* n 2) (count (:a this)))
                                (let [_ (reset! addedLeaf true)
                                      #_"BitmapIndexedNode" e (-> (BitmapIndexedNode''ensureEditable this, edit) (update :bitmap | bit))]
                                    (acopy! (:a e) (* 2 (inc idx)) (:a e) (* 2 idx) (* 2 (- n idx)))
                                    (aset! (:a e) (* 2 idx) key)
                                    (aset! (:a e) (inc (* 2 idx)) val)
                                    e
                                )
                            (<= 16 n)
                                (let [#_"INode[]" nodes (make-array #_"INode" Object 32) #_"int" jdx (PersistentHashMap'mask hash, shift)]
                                    (aset! nodes jdx (INode'''assocT BitmapIndexedNode'EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf))
                                    (loop-when [#_"int" j 0 #_"int" i 0] (< i 32)
                                        (when (odd? (>>> (:bitmap this) i)) => (recur j (inc i))
                                            (if (some? (aget (:a this) j))
                                                (aset! nodes i (INode'''assocT BitmapIndexedNode'EMPTY, edit, (+ shift 5), (f'hash (aget (:a this) j)), (aget (:a this) j), (aget (:a this) (inc j)), addedLeaf))
                                                (aset! nodes i #_"INode" (aget (:a this) (inc j)))
                                            )
                                            (recur (+ j 2) (inc i))
                                        )
                                    )
                                    (ArrayNode'new edit, (inc n), nodes)
                                )
                            :else
                                (let [#_"Object[]" a' (object-array (* 2 (+ n 4)))]
                                    (acopy! a' 0 (:a this) 0 (* 2 idx))
                                    (aset! a' (* 2 idx) key)
                                    (reset! addedLeaf true)
                                    (aset! a' (inc (* 2 idx)) val)
                                    (acopy! a' (* 2 (inc idx)) (:a this) (* 2 idx) (* 2 (- n idx)))
                                    (-> (BitmapIndexedNode''ensureEditable this, edit)
                                        (assoc :a a')
                                        (update :bitmap | bit)
                                    )
                                )
                        )
                    )
                )
            )
        )

        (#_"INode" INode'''dissocT [#_"BitmapIndexedNode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"boolean'" removedLeaf]
            (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit)) => this
                (let [#_"int" i (BitmapIndexedNode''index this, bit) #_"int" ii (* 2 i)
                      #_"Object" keyOrNull (aget (:a this) ii)
                      #_"Object" valOrNode (aget (:a this) (inc ii))]
                    (if (some? keyOrNull)
                        (when (= key keyOrNull) => this
                            (reset! removedLeaf true)
                            ;; TODO: collapse
                            (BitmapIndexedNode''editAndRemovePair this, edit, bit, i)
                        )
                        (let [#_"INode" node (INode'''dissocT #_"INode" valOrNode, edit, (+ shift 5), hash, key, removedLeaf)]
                            (cond
                                (= node valOrNode)
                                    this
                                (some? node)
                                    (BitmapIndexedNode''editAndSet-4 this, edit, (inc ii), node)
                                (= (:bitmap this) bit)
                                    nil
                                :else
                                    (BitmapIndexedNode''editAndRemovePair this, edit, bit, i)
                            )
                        )
                    )
                )
            )
        )

        (#_"Object" INode'''kvreduce [#_"BitmapIndexedNode" this, #_"fn" f, #_"Object" r]
            (NodeSeq'kvreduce (:a this), f, r)
        )
    )
)

(about #_"HashCollisionNode"
    (defr HashCollisionNode [])

    (defn #_"HashCollisionNode" HashCollisionNode'new [#_"Thread'" edit, #_"int" hash, #_"int" n & #_"Object..." a]
        (merge (HashCollisionNode'class.)
            (hash-map
                #_"Thread'" :edit edit
                #_"int" :hash hash
                #_"int" :n n
                #_"Object[]" :a a
            )
        )
    )

    (defn #_"int" HashCollisionNode''findIndex [#_"HashCollisionNode" this, #_"Object" key]
        (let [#_"int" m (* 2 (:n this))]
            (loop-when [#_"int" i 0] (< i m) => -1
                (if (= key (aget (:a this) i)) i (recur (+ i 2)))
            )
        )
    )

    (defn- #_"HashCollisionNode" HashCollisionNode''ensureEditable-2 [#_"HashCollisionNode" this, #_"Thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (let [#_"int" n (:n this) #_"Object[]" a' (object-array (* 2 (inc n)))] ;; make room for next assoc
                (acopy! a' 0 (:a this) 0 (* 2 n))
                (HashCollisionNode'new edit, (:hash this), n, a')
            )
        )
    )

    (defn- #_"HashCollisionNode" HashCollisionNode''ensureEditable-4 [#_"HashCollisionNode" this, #_"Thread'" edit, #_"int" n, #_"Object[]" a]
        (when-not (identical? (:edit this) edit) => (assoc this :a a :n n)
            (HashCollisionNode'new edit, (:hash this), n, a)
        )
    )

    (defn- #_"HashCollisionNode" HashCollisionNode''editAndSet-4 [#_"HashCollisionNode" this, #_"Thread'" edit, #_"int" i, #_"Object" x]
        (let [#_"HashCollisionNode" e (HashCollisionNode''ensureEditable-2 this, edit)]
            (aset! (:a e) i x)
            e
        )
    )

    (defn- #_"HashCollisionNode" HashCollisionNode''editAndSet-6 [#_"HashCollisionNode" this, #_"Thread'" edit, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y]
        (let [#_"HashCollisionNode" e (HashCollisionNode''ensureEditable-2 this, edit)]
            (aset! (:a e) i x)
            (aset! (:a e) j y)
            e
        )
    )

    (defm HashCollisionNode INode
        (#_"INode" INode'''assoc [#_"HashCollisionNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf]
            (if (= hash (:hash this))
                (let [#_"int" i (HashCollisionNode''findIndex this, key)]
                    (if (<= 0 i)
                        (when-not (= (aget (:a this) (inc i)) val) => this
                            (HashCollisionNode'new nil, hash, (:n this), (PersistentHashMap'cloneAndSet (:a this), (inc i), val))
                        )
                        (let [#_"int" n (:n this) #_"Object[]" a' (object-array (* 2 (inc n)))]
                            (acopy! a' 0 (:a this) 0 (* 2 n))
                            (aset! a' (* 2 n) key)
                            (aset! a' (inc (* 2 n)) val)
                            (reset! addedLeaf true)
                            (HashCollisionNode'new (:edit this), hash, (inc n), a')
                        )
                    )
                )
                ;; nest it in a bitmap node
                (let [#_"BitmapIndexedNode" node (BitmapIndexedNode'new nil, (PersistentHashMap'bitpos (:hash this), shift), (object-array [ nil, this ]))]
                    (INode'''assoc node, shift, hash, key, val, addedLeaf)
                )
            )
        )

        (#_"INode" INode'''dissoc [#_"HashCollisionNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
            (let-when [#_"int" i (HashCollisionNode''findIndex this, key)] (<= 0 i) => this
                (let-when [#_"int" n (:n this)] (< 1 n)
                    (HashCollisionNode'new nil, hash, (dec n), (PersistentHashMap'removePair (:a this), (quot i 2)))
                )
            )
        )

        (#_"IMapEntry|Object" INode'''find
            ([#_"HashCollisionNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
                (let-when [#_"int" i (HashCollisionNode''findIndex this, key)] (<= 0 i)
                    (let-when [#_"Object" ai (aget (:a this) i)] (= key ai)
                        (MapEntry'new ai, (aget (:a this) (inc i)))
                    )
                )
            )
            ([#_"HashCollisionNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not-found]
                (let-when [#_"int" i (HashCollisionNode''findIndex this, key)] (<= 0 i) => not-found
                    (when (= key (aget (:a this) i)) => not-found
                        (aget (:a this) (inc i))
                    )
                )
            )
        )

        (#_"seq" INode'''nodeSeq [#_"HashCollisionNode" this]
            (NodeSeq'create-1 (:a this))
        )

        (#_"INode" INode'''assocT [#_"HashCollisionNode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf]
            (if (= hash (:hash this))
                (let [#_"int" i (HashCollisionNode''findIndex this, key)]
                    (if (<= 0 i)
                        (when-not (= (aget (:a this) (inc i)) val) => this
                            (HashCollisionNode''editAndSet-4 this, edit, (inc i), val)
                        )
                        (let [#_"int" n (:n this) #_"int" m (count (:a this))]
                            (if (< (* 2 n) m)
                                (let [_ (reset! addedLeaf true)]
                                    (-> (HashCollisionNode''editAndSet-6 this, edit, (* 2 n), key, (inc (* 2 n)), val)
                                        (update :n inc)
                                    )
                                )
                                (let [#_"Object[]" a' (object-array (+ m 2))]
                                    (acopy! a' 0 (:a this) 0 m)
                                    (aset! a' m key)
                                    (aset! a' (inc m) val)
                                    (reset! addedLeaf true)
                                    (HashCollisionNode''ensureEditable-4 this, edit, (inc n), a')
                                )
                            )
                        )
                    )
                )
                ;; nest it in a bitmap node
                (let [#_"BitmapIndexedNode" node (BitmapIndexedNode'new edit, (PersistentHashMap'bitpos (:hash this), shift), (object-array [ nil, this, nil, nil ]))]
                    (INode'''assocT node, edit, shift, hash, key, val, addedLeaf)
                )
            )
        )

        (#_"INode" INode'''dissocT [#_"HashCollisionNode" this, #_"Thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"boolean'" removedLeaf]
            (let-when [#_"int" i (HashCollisionNode''findIndex this, key)] (<= 0 i) => this
                (reset! removedLeaf true)
                (let-when [#_"int" n (:n this)] (< 1 n)
                    (let [#_"HashCollisionNode" e (-> (HashCollisionNode''ensureEditable-2 this, edit) (update :n dec))
                          #_"int" m (* 2 n)]
                        (aset! (:a e) i (aget (:a e) (- m 2)))
                        (aset! (:a e) (inc i) (aget (:a e) (- m 1)))
                        (aset! (:a e) (- m 2) nil)
                        (aset! (:a e) (- m 1) nil)
                        e
                    )
                )
            )
        )

        (#_"Object" INode'''kvreduce [#_"HashCollisionNode" this, #_"fn" f, #_"Object" r]
            (NodeSeq'kvreduce (:a this), f, r)
        )
    )
)

(about #_"TransientHashMap"
    (defr TransientHashMap [])

    #_inherit
    (defm TransientHashMap AFn ATransientMap)

    (defn #_"TransientHashMap" TransientHashMap'new
        ([#_"PersistentHashMap" m]
            (TransientHashMap'new (atom (thread)), (:root m), (:n m), (:hasNull m), (:nullValue m))
        )
        ([#_"Thread'" edit, #_"INode" root, #_"int" n, #_"boolean" hasNull, #_"Object" nullValue]
            (merge (TransientHashMap'class.) (ATransientMap'new)
                (hash-map
                    #_"Thread'" :edit edit
                    #_"INode" :root root
                    #_"int" :n n
                    #_"boolean" :hasNull hasNull
                    #_"Object" :nullValue nullValue
                )
            )
        )
    )

    (declare PersistentHashMap'new)

    (defm TransientHashMap IATransientMap
        (#_"void" IATransientMap'''assertEditable [#_"TransientHashMap" this]
            (or @(:edit this) (throw! "transient used after persistent! call"))
            nil
        )

        (#_"ITransientMap" IATransientMap'''doAssoc [#_"TransientHashMap" this, #_"Object" key, #_"Object" val]
            (if (nil? key)
                (let [this (if (= (:nullValue this) val) this (assoc this :nullValue val))]
                    (when-not (:hasNull this) => this
                        (-> this (update :n inc) (assoc :hasNull true))
                    )
                )
                (let [#_"boolean'" addedLeaf (atom false)
                      #_"INode" node (INode'''assocT (or (:root this) BitmapIndexedNode'EMPTY), (:edit this), 0, (f'hash key), key, val, addedLeaf)
                      this (if (= (:root this) node) this (assoc this :root node))]
                    (when @addedLeaf => this
                        (update this :n inc)
                    )
                )
            )
        )

        (#_"ITransientMap" IATransientMap'''doDissoc [#_"TransientHashMap" this, #_"Object" key]
            (if (nil? key)
                (when (:hasNull this) => this
                    (-> this (assoc :hasNull false :nullValue nil) (update :n dec))
                )
                (when (some? (:root this)) => this
                    (let [#_"boolean'" removedLeaf (atom false)
                          #_"INode" node (INode'''dissocT (:root this), (:edit this), 0, (f'hash key), key, removedLeaf)
                          this (if (= (:root this) node) this (assoc this :root node))]
                        (when @removedLeaf => this
                            (update this :n dec)
                        )
                    )
                )
            )
        )

        (#_"Object" IATransientMap'''doValAt [#_"TransientHashMap" this, #_"Object" key, #_"Object" not-found]
            (if (nil? key)
                (when (:hasNull this) => not-found
                    (:nullValue this)
                )
                (when (some? (:root this)) => not-found
                    (INode'''find (:root this), 0, (f'hash key), key, not-found)
                )
            )
        )

        (#_"int" IATransientMap'''doCount => :n)

        (#_"IPersistentMap" IATransientMap'''doPersistent [#_"TransientHashMap" this]
            (reset! (:edit this) nil)
            (PersistentHashMap'new (:n this), (:root this), (:hasNull this), (:nullValue this))
        )
    )

    (§ inherit TransientHashMap ATransientMap IFn'''invoke IFn'''applyTo ILookup'''valAt ITransientAssociative'''assoc! ITransientAssociative'''containsKey ITransientAssociative'''entryAt ITransientMap'''dissoc! ITransientCollection'''conj! ITransientCollection'''persistent! Counted'''count)
)

;;;
 ; A persistent rendition of Phil Bagwell's Hash Array Mapped Trie.
 ;
 ; Uses path copying for persistence,
 ; hash collision leaves vs. extended hashing,
 ; node polymorphism vs. conditionals,
 ; no sub-tree pools or root-resizing.
 ;
 ; Any errors are my own.
 ;;
(about #_"PersistentHashMap"
    (defr PersistentHashMap [])

    #_inherit
    (defm PersistentHashMap AFn APersistentMap)

    (defn #_"PersistentHashMap" PersistentHashMap'new
        ([#_"int" n, #_"INode" root, #_"boolean" hasNull, #_"Object" nullValue] (PersistentHashMap'new nil, n, root, hasNull, nullValue))
        ([#_"meta" meta, #_"int" n, #_"INode" root, #_"boolean" hasNull, #_"Object" nullValue]
            (merge (PersistentHashMap'class.) (APersistentMap'new)
                (hash-map
                    #_"meta" :_meta meta
                    #_"int" :n n
                    #_"INode" :root root
                    #_"boolean" :hasNull hasNull
                    #_"Object" :nullValue nullValue
                )
            )
        )
    )

    (defm PersistentHashMap IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentHashMap IObj
        (#_"PersistentHashMap" IObj'''withMeta [#_"PersistentHashMap" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentHashMap'new meta, (:n this), (:root this), (:hasNull this), (:nullValue this))
            )
        )
    )

    (def #_"PersistentHashMap" PersistentHashMap'EMPTY (PersistentHashMap'new 0, nil, false, nil))

    (defn #_"PersistentHashMap" PersistentHashMap'create-1a [& #_"Object..." a]
        (loop-when-recur [#_"ITransientMap" m (transient PersistentHashMap'EMPTY) #_"int" i 0]
                         (< i (count a))
                         [(assoc! m (aget a i) (aget a (inc i))) (+ i 2)]
                      => (persistent! m)
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'create-1s [#_"Seqable" keyvals]
        (let [#_"ITransientMap" m (transient PersistentHashMap'EMPTY)
              m (loop-when [m m #_"seq" s (seq keyvals)] (some? s) => m
                    (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                        (recur (assoc! m (first s) (second s)) (next (next s)))
                    )
                )]
            (persistent! m)
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'createWithCheck-1a [& #_"Object..." a]
        (let [#_"ITransientMap" m (transient PersistentHashMap'EMPTY)
              m (loop-when [m m #_"int" i 0] (< i (count a)) => m
                    (let [m (assoc! m (aget a i) (aget a (inc i)))]
                        (when (= (count m) (inc (quot i 2))) => (throw! (str "duplicate key: " (aget a i)))
                            (recur m (+ i 2))
                        )
                    )
                )]
            (persistent! m)
        )
    )

    (defn #_"PersistentHashMap" PersistentHashMap'createWithCheck-1s [#_"Seqable" keyvals]
        (let [#_"ITransientMap" m (transient PersistentHashMap'EMPTY)
              m (loop-when [m m #_"seq" s (seq keyvals) #_"int" i 0] (some? s) => m
                    (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                        (let [m (assoc! m (first s) (second s))]
                            (when (= (count m) (inc i)) => (throw! (str "duplicate key: " (first s)))
                                (recur m (next (next s)) (inc i))
                            )
                        )
                    )
                )]
            (persistent! m)
        )
    )

    (def- #_"Object" PersistentHashMap'NOT_FOUND (Object.))

    (defm PersistentHashMap Associative
        (#_"IPersistentMap" Associative'''assoc [#_"PersistentHashMap" this, #_"Object" key, #_"Object" val]
            (if (nil? key)
                (when-not (and (:hasNull this) (= val (:nullValue this))) => this
                    (PersistentHashMap'new (meta this), (+ (:n this) (if (:hasNull this) 0 1)), (:root this), true, val)
                )
                (let [#_"boolean'" addedLeaf (atom false)
                      #_"INode" newroot (INode'''assoc (or (:root this) BitmapIndexedNode'EMPTY), 0, (f'hash key), key, val, addedLeaf)]
                    (when-not (= newroot (:root this)) => this
                        (PersistentHashMap'new (meta this), (+ (:n this) (if @addedLeaf 1 0)), newroot, (:hasNull this), (:nullValue this))
                    )
                )
            )
        )

        (#_"boolean" Associative'''containsKey [#_"PersistentHashMap" this, #_"Object" key]
            (if (nil? key)
                (:hasNull this)
                (and (some? (:root this))
                    (not (identical? (INode'''find (:root this), 0, (f'hash key), key, PersistentHashMap'NOT_FOUND) PersistentHashMap'NOT_FOUND))
                )
            )
        )

        (#_"IMapEntry" Associative'''entryAt [#_"PersistentHashMap" this, #_"Object" key]
            (if (nil? key)
                (when (:hasNull this)
                    (MapEntry'new nil, (:nullValue this))
                )
                (when (some? (:root this))
                    (INode'''find (:root this), 0, (f'hash key), key)
                )
            )
        )
    )

    (defm PersistentHashMap ILookup
        (#_"Object" ILookup'''valAt
            ([#_"PersistentHashMap" this, #_"Object" key] (ILookup'''valAt this, key, nil))
            ([#_"PersistentHashMap" this, #_"Object" key, #_"Object" not-found]
                (if (nil? key)
                    (when (:hasNull this) => not-found
                        (:nullValue this)
                    )
                    (when (some? (:root this)) => not-found
                        (INode'''find (:root this), 0, (f'hash key), key, not-found)
                    )
                )
            )
        )
    )

    (defm PersistentHashMap IPersistentMap
        (#_"IPersistentMap" IPersistentMap'''dissoc [#_"PersistentHashMap" this, #_"Object" key]
            (cond
                (nil? key)
                    (if (:hasNull this) (PersistentHashMap'new (meta this), (dec (:n this)), (:root this), false, nil) this)
                (nil? (:root this))
                    this
                :else
                    (let [#_"INode" newroot (INode'''dissoc (:root this), 0, (f'hash key), key)]
                        (when-not (= newroot (:root this)) => this
                            (PersistentHashMap'new (meta this), (dec (:n this)), newroot, (:hasNull this), (:nullValue this))
                        )
                    )
            )
        )
    )

    (defm PersistentHashMap IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"PersistentHashMap" this, #_"fn" f, #_"Object" r]
            (let [r (if (:hasNull this) (f r nil (:nullValue this)) r)]
                (when-not (reduced? r) => @r
                    (when (some? (:root this)) => r
                        (let [r (INode'''kvreduce (:root this), f, r)]
                            (when-not (reduced? r) => @r
                                r
                            )
                        )
                    )
                )
            )
        )
    )

    (defm PersistentHashMap Counted
        (#_"int" Counted'''count => :n)
    )

    (defm PersistentHashMap Seqable
        (#_"seq" Seqable'''seq [#_"PersistentHashMap" this]
            (let [#_"seq" s (when (some? (:root this)) (INode'''nodeSeq (:root this)))]
                (if (:hasNull this) (Cons'new (MapEntry'new nil, (:nullValue this)), s) s)
            )
        )
    )

    (defm PersistentHashMap IPersistentCollection
        ;; inherit APersistentMap conj

        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"PersistentHashMap" this]
            (with-meta PersistentHashMap'EMPTY (meta this))
        )
    )

    (defm PersistentHashMap IEditableCollection
        (#_"TransientHashMap" IEditableCollection'''asTransient => TransientHashMap'new)
    )

    (§ inherit PersistentHashMap APersistentMap IObject'''equals IObject'''toString Hashed'''hash IFn'''invoke IFn'''applyTo)
)
)

(about #_"cloiure.core.PersistentHashSet"

(about #_"TransientHashSet"
    (defr TransientHashSet [])

    #_inherit
    (defm TransientHashSet AFn ATransientSet)

    (defn #_"TransientHashSet" TransientHashSet'new [#_"ITransientMap" impl]
        (merge (TransientHashSet'class.) (ATransientSet'new impl))
    )

    (declare PersistentHashSet'new)

    (defm TransientHashSet ITransientCollection
        ;; inherit ATransientSet conj!

        (#_"PersistentHashSet" ITransientCollection'''persistent! [#_"TransientHashSet" this]
            (PersistentHashSet'new nil, (persistent! (:impl this)))
        )
    )

    (§ inherit TransientHashSet ATransientSet Counted'''count ITransientSet'''disj! ITransientSet'''contains? ITransientSet'''get IFn'''invoke IFn'''applyTo)
)

(about #_"PersistentHashSet"
    (defr PersistentHashSet [])

    #_inherit
    (defm PersistentHashSet AFn APersistentSet)

    (defn #_"PersistentHashSet" PersistentHashSet'new [#_"meta" meta, #_"map" impl]
        (merge (PersistentHashSet'class.) (APersistentSet'new impl)
            (hash-map
                #_"meta" :_meta meta
            )
        )
    )

    (defm PersistentHashSet IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentHashSet IObj
        (#_"PersistentHashSet" IObj'''withMeta [#_"PersistentHashSet" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentHashSet'new meta, (:impl this))
            )
        )
    )

    (def #_"PersistentHashSet" PersistentHashSet'EMPTY (PersistentHashSet'new nil, PersistentHashMap'EMPTY))

    (defn #_"PersistentHashSet" PersistentHashSet'create-1a [& #_"Object..." items]
        (loop-when-recur [#_"ITransientSet" s (transient PersistentHashSet'EMPTY) #_"int" i 0]
                         (< i (count items))
                         [(conj! s (aget items i)) (inc i)]
                      => (persistent! s)
        )
    )

    (defn #_"PersistentHashSet" PersistentHashSet'create-1s [#_"Seqable" items]
        (loop-when-recur [#_"ITransientSet" s (transient PersistentHashSet'EMPTY) #_"seq" q (seq items)]
                         (some? q)
                         [(conj! s (first q)) (next q)]
                      => (persistent! s)
        )
    )

    (defn #_"PersistentHashSet" PersistentHashSet'createWithCheck-1a [& #_"Object..." items]
        (let [#_"ITransientSet" s (transient PersistentHashSet'EMPTY)
              s (loop-when [s s #_"int" i 0] (< i (count items)) => s
                    (let [s (conj! s (aget items i))]
                        (when (= (count s) (inc i)) => (throw! (str "duplicate key: " (aget items i)))
                            (recur s (inc i))
                        )
                    )
                )]
            (persistent! s)
        )
    )

    (defn #_"PersistentHashSet" PersistentHashSet'createWithCheck-1s [#_"Seqable" items]
        (let [#_"ITransientSet" s (transient PersistentHashSet'EMPTY)
              s (loop-when [s s #_"seq" q (seq items) #_"int" i 0] (some? q) => s
                    (let [#_"Object" key (first q) s (conj! s key)]
                        (when (= (count s) (inc i)) => (throw! (str "duplicate key: " key))
                            (recur s (next q) (inc i))
                        )
                    )
                )]
            (persistent! s)
        )
    )

    (defm PersistentHashSet IPersistentSet
        (#_"IPersistentSet" IPersistentSet'''disj [#_"PersistentHashSet" this, #_"Object" key]
            (if (contains? this key)
                (PersistentHashSet'new (meta this), (dissoc (:impl this) key))
                this
            )
        )

        ;; inherit APersistentSet contains? get
    )

    (defm PersistentHashSet IPersistentCollection
        (#_"PersistentHashSet" IPersistentCollection'''conj [#_"PersistentHashSet" this, #_"Object" o]
            (if (contains? this o)
                this
                (PersistentHashSet'new (meta this), (assoc (:impl this) o o))
            )
        )

        (#_"PersistentHashSet" IPersistentCollection'''empty [#_"PersistentHashSet" this]
            (with-meta PersistentHashSet'EMPTY (meta this))
        )
    )

    (defm PersistentHashSet IEditableCollection
        (#_"ITransientCollection" IEditableCollection'''asTransient [#_"PersistentHashSet" this]
            (TransientHashSet'new (transient (:impl this)))
        )
    )

    (§ inherit PersistentHashSet APersistentSet IPersistentSet'''get Counted'''count Seqable'''seq IFn'''invoke IFn'''applyTo IObject'''equals IObject'''toString Hashed'''hash)
)
)

(about #_"cloiure.core.PersistentList"

(about #_"EmptyList"
    (defr EmptyList [])

    (defm EmptyList IPersistentList Sequential)

    (def #_"int" EmptyList'HASH (§ soon Murmur3'hashOrdered nil))

    (defn #_"EmptyList" EmptyList'new [#_"meta" meta]
        (merge (EmptyList'class.)
            (hash-map
                #_"meta" :_meta meta
            )
        )
    )

    (defm EmptyList IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm EmptyList IObj
        (#_"EmptyList" IObj'''withMeta [#_"EmptyList" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (EmptyList'new meta)
            )
        )
    )

    (defm EmptyList Hashed
        (#_"int" Hashed'''hash [#_"EmptyList" this]
            EmptyList'HASH
        )
    )

    (defm EmptyList IObject
        (#_"boolean" IObject'''equals [#_"EmptyList" this, #_"Object" that]
            (and (sequential? that) (nil? (seq that)))
        )

        (#_"String" IObject'''toString [#_"EmptyList" this]
            "()"
        )
    )

    (defm EmptyList ISeq
        (#_"Object" ISeq'''first [#_"EmptyList" this]
            nil
        )

        (#_"seq" ISeq'''next [#_"EmptyList" this]
            nil
        )
    )

    (declare PersistentList'new)

    (defm EmptyList IPersistentCollection
        (#_"PersistentList" IPersistentCollection'''conj [#_"EmptyList" this, #_"Object" o]
            (PersistentList'new (meta this), o, nil, 1)
        )

        (#_"EmptyList" IPersistentCollection'''empty [#_"EmptyList" this]
            this
        )
    )

    (defm EmptyList IPersistentStack
        (#_"Object" IPersistentStack'''peek [#_"EmptyList" this]
            nil
        )

        (#_"IPersistentList" IPersistentStack'''pop [#_"EmptyList" this]
            (throw! "can't pop the empty list")
        )
    )

    (defm EmptyList Counted
        (#_"int" Counted'''count [#_"EmptyList" this]
            0
        )
    )

    (defm EmptyList Seqable
        (#_"seq" Seqable'''seq [#_"EmptyList" this]
            nil
        )
    )
)

(about #_"PersistentList"
    (defr PersistentList [])

    #_inherit
    (defm PersistentList ASeq)

    (def #_"EmptyList" PersistentList'EMPTY (EmptyList'new nil))

    (defn #_"PersistentList" PersistentList'new
        ([#_"Object" _first] (PersistentList'new nil, _first, nil, 1))
        ([#_"meta" meta, #_"Object" _first, #_"IPersistentList" _rest, #_"int" _count]
            (merge (PersistentList'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"Object" :_first _first
                    #_"IPersistentList" :_rest _rest
                    #_"int" :_count _count
                )
            )
        )
    )

    (defm PersistentList IPersistentList Sequential)

    (defm PersistentList IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentList IObj
        (#_"PersistentList" IObj'''withMeta [#_"PersistentList" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentList'new meta, (:_first this), (:_rest this), (:_count this))
            )
        )
    )

    (defn #_"IPersistentList" PersistentList'create [#_"Object[]" a]
        (loop-when-recur [#_"IPersistentList" l PersistentList'EMPTY #_"int" i (dec (count a))]
                         (<= 0 i)
                         [(conj l (aget a i)) (dec i)]
                      => l
        )
    )

    (defm PersistentList ISeq
        (#_"Object" ISeq'''first => :_first)

        (#_"seq" ISeq'''next [#_"PersistentList" this]
            (when-not (= (:_count this) 1)
                (:_rest this)
            )
        )
    )

    (defm PersistentList IPersistentStack
        (#_"Object" IPersistentStack'''peek [#_"PersistentList" this]
            (first this)
        )

        (#_"IPersistentList" IPersistentStack'''pop [#_"PersistentList" this]
            (or (:_rest this) (with-meta PersistentList'EMPTY (:_meta this)))
        )
    )

    (defm PersistentList Counted
        (#_"int" Counted'''count => :_count)
    )

    (defm PersistentList IPersistentCollection
        (#_"PersistentList" IPersistentCollection'''conj [#_"PersistentList" this, #_"Object" o]
            (PersistentList'new (meta this), o, this, (inc (:_count this)))
        )

        (#_"PersistentList" IPersistentCollection'''empty [#_"PersistentList" this]
            (with-meta PersistentList'EMPTY (meta this))
        )
    )

    (defm PersistentList IReduce
        (#_"Object" IReduce'''reduce
            ([#_"PersistentList" this, #_"fn" f]
                (loop-when [#_"Object" r (first this) #_"seq" s (next this)] (some? s) => r
                    (let [r (f r (first s))]
                        (if (reduced? r) @r (recur r (next s)))
                    )
                )
            )
            ([#_"PersistentList" this, #_"fn" f, #_"Object" r]
                (loop-when [r (f r (first this)) #_"seq" s (next this)] (some? s) => (if (reduced? r) @r r)
                    (if (reduced? r) @r (recur (f r (first s)) (next s)))
                )
            )
        )
    )

    (defm PersistentList Seqable
        (#_"seq" Seqable'''seq [#_"PersistentList" this]
            this
        )
    )

    (defm PersistentList Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm PersistentList IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.PersistentQueue"

(about #_"QSeq"
    (defr QSeq [])

    #_inherit
    (defm QSeq ASeq)

    (defn #_"QSeq" QSeq'new
        ([#_"seq" f, #_"seq" rseq] (QSeq'new nil, f, rseq))
        ([#_"meta" meta, #_"seq" f, #_"seq" rseq]
            (merge (QSeq'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"seq" :f f
                    #_"seq" :rseq rseq
                )
            )
        )
    )

    (defm QSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm QSeq IObj
        (#_"QSeq" IObj'''withMeta [#_"QSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (QSeq'new meta, (:f this), (:rseq this))
            )
        )
    )

    (defm QSeq ISeq
        (#_"Object" ISeq'''first [#_"QSeq" this]
            (first (:f this))
        )

        (#_"seq" ISeq'''next [#_"QSeq" this]
            (let [#_"seq" f (next (:f this)) #_"seq" r (:rseq this)]
                (cond
                    (some? f) (QSeq'new f, r)
                    (some? r) (QSeq'new r, nil)
                )
            )
        )
    )

    (defm QSeq Counted
        (#_"int" Counted'''count [#_"QSeq" this]
            (+ (count (:f this)) (count (:rseq this)))
        )
    )

    (defm QSeq Sequential)

    (defm QSeq Seqable
        (#_"seq" Seqable'''seq [#_"QSeq" this]
            this
        )
    )

    (defm QSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm QSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)

;;;
 ; conses onto rear, peeks/pops from front
 ;
 ; See Okasaki's Batched Queues.
 ; Differs in that, it uses a PersistentVector as the rear, which is in-order,
 ; so no reversing or suspensions required for persistent use.
 ;;
(about #_"PersistentQueue"
    (defr PersistentQueue [])

    (defm PersistentQueue IPersistentList Sequential)

    (defn #_"PersistentQueue" PersistentQueue'new [#_"meta" meta, #_"int" cnt, #_"seq" f, #_"vector" r]
        (merge (PersistentQueue'class.)
            (hash-map
                #_"meta" :_meta meta
                #_"int" :cnt cnt
                #_"seq" :f f
                #_"vector" :r r
            )
        )
    )

    (defm PersistentQueue IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentQueue IObj
        (#_"PersistentQueue" IObj'''withMeta [#_"PersistentQueue" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentQueue'new meta, (:cnt this), (:f this), (:r this))
            )
        )
    )

    (def #_"PersistentQueue" PersistentQueue'EMPTY (PersistentQueue'new nil, 0, nil, nil))

    (defm PersistentQueue IObject
        (#_"boolean" IObject'''equals [#_"PersistentQueue" this, #_"Object" that]
            (or (identical? this that)
                (and (sequential? that)
                    (loop-when [#_"seq" s (seq this) #_"seq" z (seq that)] (some? s) => (nil? z)
                        (and (some? z) (= (first s) (first z))
                            (recur (next s) (next z))
                        )
                    )
                )
            )
        )

        ;; abstract IObject toString
    )

    (defm PersistentQueue Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm PersistentQueue IPersistentStack
        (#_"Object" IPersistentStack'''peek [#_"PersistentQueue" this]
            (first (:f this))
        )

        (#_"PersistentQueue" IPersistentStack'''pop [#_"PersistentQueue" this]
            (when (some? (:f this)) => this ;; hmmm... pop of empty queue -> empty queue?
                (let [#_"seq" f (next (:f this)) #_"vector" r (:r this)
                      [f r]
                        (when (nil? f) => [f r]
                            [(seq r) nil]
                        )]
                    (PersistentQueue'new (meta this), (dec (:cnt this)), f, r)
                )
            )
        )
    )

    (defm PersistentQueue Counted
        (#_"int" Counted'''count => :cnt)
    )

    (defm PersistentQueue Seqable
        (#_"seq" Seqable'''seq [#_"PersistentQueue" this]
            (when (some? (:f this))
                (QSeq'new (:f this), (seq (:r this)))
            )
        )
    )

    (defm PersistentQueue IPersistentCollection
        (#_"PersistentQueue" IPersistentCollection'''conj [#_"PersistentQueue" this, #_"Object" o]
            (let [[#_"seq" f #_"vector" r]
                    (if (nil? (:f this)) ;; empty
                        [(list o) nil]
                        [(:f this) (conj (or (:r this) []) o)]
                    )]
                (PersistentQueue'new (meta this), (inc (:cnt this)), f, r)
            )
        )

        (#_"PersistentQueue" IPersistentCollection'''empty [#_"PersistentQueue" this]
            (with-meta PersistentQueue'EMPTY (meta this))
        )
    )
)
)

(about #_"cloiure.core.PersistentTreeMap"

(about #_"TNode"
    (defr TNode [])

    #_inherit
    (defm TNode AFn APersistentVector AMapEntry)

    (defn #_"TNode" TNode'new [#_"Object" key]
        (merge (TNode'class.)
            (hash-map
                #_"Object" :key key
            )
        )
    )

    (defm TNode IMapEntry
        (#_"Object" IMapEntry'''key => :key)

        (#_"Object" IMapEntry'''val [#_"TNode" this]
            nil
        )
    )

    (declare PersistentTreeMap'black)

    (defm TNode ITNode
        (#_"TNode" ITNode'''left [#_"TNode" this]
            nil
        )

        (#_"TNode" ITNode'''right [#_"TNode" this]
            nil
        )

        ;; abstract ITNode addLeft addRight removeLeft removeRight blacken redden

        (#_"TNode" ITNode'''balanceLeft [#_"TNode" this, #_"TNode" parent]
            (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), this, (ITNode'''right parent))
        )

        (#_"TNode" ITNode'''balanceRight [#_"TNode" this, #_"TNode" parent]
            (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), (ITNode'''left parent), this)
        )

        ;; abstract ITNode replace
    )

    (defm TNode IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"TNode" this, #_"fn" f, #_"Object" r]
            (or
                (when (some? (ITNode'''left this))
                    (let [r (INode'''kvreduce (ITNode'''left this), f, r)]
                        (when (reduced? r)
                            r
                        )
                    )
                )
                (let [r (f r (key this) (val this))]
                    (cond
                        (reduced? r)                  r
                        (some? (ITNode'''right this)) (INode'''kvreduce (ITNode'''right this), f, r)
                        :else                         r
                    )
                )
            )
        )
    )

    (defm TNode Sequential)

    (§ inherit TNode AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
)

(about #_"Black"
    (defr Black [])

    #_inherit
    (defm Black AFn APersistentVector AMapEntry TNode)

    (defn #_"Black" Black'new [#_"Object" key]
        (merge (Black'class.) (TNode'new key))
    )

    (declare PersistentTreeMap'balanceLeftDel)
    (declare PersistentTreeMap'balanceRightDel)
    (declare Red'new)

    (defm Black ITNode
        ;; inherit TNode left right

        (#_"TNode" ITNode'''addLeft [#_"Black" this, #_"TNode" ins]
            (ITNode'''balanceLeft ins, this)
        )

        (#_"TNode" ITNode'''addRight [#_"Black" this, #_"TNode" ins]
            (ITNode'''balanceRight ins, this)
        )

        (#_"TNode" ITNode'''removeLeft [#_"Black" this, #_"TNode" del]
            (PersistentTreeMap'balanceLeftDel (:key this), (IMapEntry'''val this), del, (ITNode'''right this))
        )

        (#_"TNode" ITNode'''removeRight [#_"Black" this, #_"TNode" del]
            (PersistentTreeMap'balanceRightDel (:key this), (IMapEntry'''val this), (ITNode'''left this), del)
        )

        (#_"TNode" ITNode'''blacken [#_"Black" this]
            this
        )

        (#_"TNode" ITNode'''redden [#_"Black" this]
            (Red'new (:key this))
        )

        ;; inherit TNode balanceLeft balanceRight

        (#_"TNode" ITNode'''replace [#_"Black" this, #_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" right]
            (PersistentTreeMap'black key, val, left, right)
        )
    )

    (defm Black Sequential)

    (§ inherit Black AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit Black TNode IMapEntry'''key IMapEntry'''val IKVReduce'''kvreduce)
)

(about #_"BlackVal"
    (defr BlackVal [])

    #_inherit
    (defm BlackVal AFn APersistentVector AMapEntry TNode Black)

    (defn #_"BlackVal" BlackVal'new [#_"Object" key, #_"Object" val]
        (merge (BlackVal'class.) (Black'new key)
            (hash-map
                #_"Object" :val val
            )
        )
    )

    (defm BlackVal IMapEntry
        ;; inherit TNode key

        (#_"Object" IMapEntry'''val => :val)
    )

    (declare RedVal'new)

    (defm BlackVal ITNode
        ;; inherit Black left right addLeft addRight removeLeft removeRight blacken

        (#_"TNode" ITNode'''redden [#_"BlackVal" this]
            (RedVal'new (:key this), (:val this))
        )

        ;; inherit Black balanceLeft balanceRight replace
    )

    (defm BlackVal Sequential)

    (§ inherit BlackVal AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit BlackVal TNode IKVReduce'''kvreduce)
)

(about #_"BlackBranch"
    (defr BlackBranch [])

    #_inherit
    (defm BlackBranch AFn APersistentVector AMapEntry TNode Black)

    (defn #_"BlackBranch" BlackBranch'new [#_"Object" key, #_"TNode" left, #_"TNode" right]
        (merge (BlackBranch'class.) (Black'new key)
            (hash-map
                #_"TNode" :left left
                #_"TNode" :right right
            )
        )
    )

    (declare RedBranch'new)

    (defm BlackBranch ITNode
        (#_"TNode" ITNode'''left => :left)

        (#_"TNode" ITNode'''right => :right)

        ;; inherit Black addLeft addRight removeLeft removeRight blacken

        (#_"TNode" ITNode'''redden [#_"BlackBranch" this]
            (RedBranch'new (:key this), (:left this), (:right this))
        )

        ;; inherit Black balanceLeft balanceRight replace
    )

    (defm BlackBranch Sequential)

    (§ inherit BlackBranch AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit BlackBranch TNode IMapEntry'''key IMapEntry'''val IKVReduce'''kvreduce)
)

(about #_"BlackBranchVal"
    (defr BlackBranchVal [])

    #_inherit
    (defm BlackBranchVal AFn APersistentVector AMapEntry TNode Black BlackBranch)

    (defn #_"BlackBranchVal" BlackBranchVal'new [#_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" right]
        (merge (BlackBranchVal'class.) (BlackBranch'new key, left, right)
            (hash-map
                #_"Object" :val val
            )
        )
    )

    (defm BlackBranchVal IMapEntry
        ;; inherit TNode key

        (#_"Object" IMapEntry'''val => :val)
    )

    (declare RedBranchVal'new)

    (defm BlackBranchVal ITNode
        ;; inherit BlackBranch left right addLeft addRight removeLeft removeRight blacken

        (#_"TNode" ITNode'''redden [#_"BlackBranchVal" this]
            (RedBranchVal'new (:key this), (:val this), (:left this), (:right this))
        )

        ;; inherit BlackBranch balanceLeft balanceRight replace
    )

    (defm BlackBranchVal Sequential)

    (§ inherit BlackBranchVal AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit BlackBranchVal TNode IKVReduce'''kvreduce)
)

(about #_"Red"
    (defr Red [])

    #_inherit
    (defm Red AFn APersistentVector AMapEntry TNode)

    (defn #_"Red" Red'new [#_"Object" key]
        (merge (Red'class.) (TNode'new key))
    )

    (declare PersistentTreeMap'red)

    (defm Red ITNode
        ;; inherit TNode left right

        (#_"TNode" ITNode'''addLeft [#_"Red" this, #_"TNode" ins]
            (PersistentTreeMap'red (:key this), (IMapEntry'''val this), ins, (ITNode'''right this))
        )

        (#_"TNode" ITNode'''addRight [#_"Red" this, #_"TNode" ins]
            (PersistentTreeMap'red (:key this), (IMapEntry'''val this), (ITNode'''left this), ins)
        )

        (#_"TNode" ITNode'''removeLeft [#_"Red" this, #_"TNode" del]
            (PersistentTreeMap'red (:key this), (IMapEntry'''val this), del, (ITNode'''right this))
        )

        (#_"TNode" ITNode'''removeRight [#_"Red" this, #_"TNode" del]
            (PersistentTreeMap'red (:key this), (IMapEntry'''val this), (ITNode'''left this), del)
        )

        (#_"TNode" ITNode'''blacken [#_"Red" this]
            (Black'new (:key this))
        )

        (#_"TNode" ITNode'''redden [#_"Red" this]
            (throw! "invariant violation")
        )

        ;; inherit TNode balanceLeft balanceRight

        (#_"TNode" ITNode'''replace [#_"Red" this, #_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" right]
            (PersistentTreeMap'red key, val, left, right)
        )
    )

    (defm Red Sequential)

    (§ inherit Red AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit Red TNode IMapEntry'''key IMapEntry'''val IKVReduce'''kvreduce)
)

(about #_"RedVal"
    (defr RedVal [])

    #_inherit
    (defm RedVal AFn APersistentVector AMapEntry TNode Red)

    (defn #_"RedVal" RedVal'new [#_"Object" key, #_"Object" val]
        (merge (RedVal'class.) (Red'new key)
            (hash-map
                #_"Object" :val val
            )
        )
    )

    (defm RedVal IMapEntry
        ;; inherit TNode key

        (#_"Object" IMapEntry'''val => :val)
    )

    (defm RedVal ITNode
        ;; inherit Red left right addLeft addRight removeLeft removeRight

        (#_"TNode" ITNode'''blacken [#_"RedVal" this]
            (BlackVal'new (:key this), (:val this))
        )

        ;; inherit Red redden balanceLeft balanceRight replace
    )

    (defm RedVal Sequential)

    (§ inherit RedVal AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit RedVal TNode IKVReduce'''kvreduce)
)

(about #_"RedBranch"
    (defr RedBranch [])

    #_inherit
    (defm RedBranch AFn APersistentVector AMapEntry TNode Red)

    (defn #_"RedBranch" RedBranch'new [#_"Object" key, #_"TNode" left, #_"TNode" right]
        (merge (RedBranch'class.) (Red'new key)
            (hash-map
                #_"TNode" :left left
                #_"TNode" :right right
            )
        )
    )

    (defm RedBranch ITNode
        (#_"TNode" ITNode'''left => :left)

        (#_"TNode" ITNode'''right => :right)

        ;; inherit Red addLeft addRight removeLeft removeRight

        (#_"TNode" ITNode'''blacken [#_"RedBranch" this]
            (BlackBranch'new (:key this), (:left this), (:right this))
        )

        ;; inherit Red redden

        (#_"TNode" ITNode'''balanceLeft [#_"RedBranch" this, #_"TNode" parent]
            (cond (satisfies? Red (:left this))
                (do
                    (PersistentTreeMap'red (:key this), (IMapEntry'''val this), (ITNode'''blacken (:left this)), (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), (:right this), (ITNode'''right parent)))
                )
                (satisfies? Red (:right this))
                (do
                    (PersistentTreeMap'red (:key (:right this)), (IMapEntry'''val (:right this)), (PersistentTreeMap'black (:key this), (IMapEntry'''val this), (:left this), (ITNode'''left (:right this))), (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), (ITNode'''right (:right this)), (ITNode'''right parent)))
                )
                :else
                (do
                    (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), this, (ITNode'''right parent))
                )
            )
        )

        (#_"TNode" ITNode'''balanceRight [#_"RedBranch" this, #_"TNode" parent]
            (cond (satisfies? Red (:right this))
                (do
                    (PersistentTreeMap'red (:key this), (IMapEntry'''val this), (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), (ITNode'''left parent), (:left this)), (ITNode'''blacken (:right this)))
                )
                (satisfies? Red (:left this))
                (do
                    (PersistentTreeMap'red (:key (:left this)), (IMapEntry'''val (:left this)), (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), (ITNode'''left parent), (ITNode'''left (:left this))), (PersistentTreeMap'black (:key this), (IMapEntry'''val this), (ITNode'''right (:left this)), (:right this)))
                )
                :else
                (do
                    (PersistentTreeMap'black (:key parent), (IMapEntry'''val parent), (ITNode'''left parent), this)
                )
            )
        )

        ;; inherit Red replace
    )

    (defm RedBranch Sequential)

    (§ inherit RedBranch AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit RedBranch TNode IMapEntry'''key IMapEntry'''val IKVReduce'''kvreduce)
)

(about #_"RedBranchVal"
    (defr RedBranchVal [])

    #_inherit
    (defm RedBranchVal AFn APersistentVector AMapEntry TNode Red RedBranch)

    (defn #_"RedBranchVal" RedBranchVal'new [#_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" right]
        (merge (RedBranchVal'class.) (RedBranch'new key, left, right)
            (hash-map
                #_"Object" :val val
            )
        )
    )

    (defm RedBranchVal IMapEntry
        ;; inherit TNode key

        (#_"Object" IMapEntry'''val => :val)
    )

    (defm RedBranchVal ITNode
        ;; inherit RedBranch left right addLeft addRight removeLeft removeRight

        (#_"TNode" ITNode'''blacken [#_"RedBranchVal" this]
            (BlackBranchVal'new (:key this), (:val this), (:left this), (:right this))
        )

        ;; inherit RedBranch redden balanceLeft balanceRight replace
    )

    (defm RedBranchVal Sequential)

    (§ inherit RedBranchVal AMapEntry [Counted] [Indexed] Seqable'''seq Reversible'''rseq IObject'''equals IObject'''toString Hashed'''hash Comparable'''compareTo)
    (§ inherit RedBranchVal TNode IKVReduce'''kvreduce)
)

(about #_"TSeq"
    (defr TSeq [])

    #_inherit
    (defm TSeq ASeq)

    (defn #_"TSeq" TSeq'new
        ([#_"seq" stack, #_"boolean" asc?] (TSeq'new stack, asc?, -1))
        ([#_"seq" stack, #_"boolean" asc?, #_"int" cnt] (TSeq'new nil, stack, asc?, cnt))
        ([#_"meta" meta, #_"seq" stack, #_"boolean" asc?, #_"int" cnt]
            (merge (TSeq'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"seq" :stack stack
                    #_"boolean" :asc? asc?
                    #_"int" :cnt cnt
                )
            )
        )
    )

    (defm TSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm TSeq IObj
        (#_"TSeq" IObj'''withMeta [#_"TSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (TSeq'new meta, (:stack this), (:asc? this), (:cnt this))
            )
        )
    )

    (defn #_"seq" TSeq'push [#_"TNode" t, #_"seq" stack, #_"boolean" asc?]
        (loop-when [stack stack t t] (some? t) => stack
            (recur (cons t stack) (if asc? (ITNode'''left t) (ITNode'''right t)))
        )
    )

    (defn #_"TSeq" TSeq'create [#_"TNode" t, #_"boolean" asc?, #_"int" cnt]
        (TSeq'new (TSeq'push t, nil, asc?), asc?, cnt)
    )

    (defm TSeq ISeq
        (#_"Object" ISeq'''first [#_"TSeq" this]
            (first (:stack this))
        )

        (#_"seq" ISeq'''next [#_"TSeq" this]
            (let [#_"TNode" t #_"TNode" (first (:stack this)) #_"boolean" asc? (:asc? this)]
                (when-some [#_"seq" stack (TSeq'push (if asc? (ITNode'''right t) (ITNode'''left t)), (next (:stack this)), asc?)]
                    (TSeq'new stack, asc?, (dec (:cnt this)))
                )
            )
        )
    )

    (defm TSeq Counted
        (#_"int" Counted'''count [#_"TSeq" this]
            (when (neg? (:cnt this)) => (:cnt this)
                (count (:stack this))
            )
        )
    )

    (defm TSeq Sequential)

    (defm TSeq Seqable
        (#_"seq" Seqable'''seq [#_"TSeq" this]
            this
        )
    )

    (defm TSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm TSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)

;;;
 ; Persistent Red Black Tree.
 ;
 ; Note that instances of this class are constant values,
 ; i.e. add/remove etc return new values.
 ;
 ; See Okasaki, Kahrs, Larsen, et al.
 ;;
(about #_"PersistentTreeMap"
    (defr PersistentTreeMap [])

    #_inherit
    (defm PersistentTreeMap AFn APersistentMap)

    (defn #_"PersistentTreeMap" PersistentTreeMap'new
        ([] (PersistentTreeMap'new compare))
        ([#_"Comparator" cmp] (PersistentTreeMap'new nil, cmp))
        ([#_"meta" meta, #_"Comparator" cmp] (PersistentTreeMap'new meta, cmp, nil, 0))
        ([#_"meta" meta, #_"Comparator" cmp, #_"TNode" tree, #_"int" _count]
            (merge (PersistentTreeMap'class.) (APersistentMap'new)
                (hash-map
                    #_"meta" :_meta meta
                    #_"Comparator" :cmp cmp
                    #_"TNode" :tree tree
                    #_"int" :_count _count
                )
            )
        )
    )

    (defm PersistentTreeMap IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentTreeMap IObj
        (#_"PersistentTreeMap" IObj'''withMeta [#_"PersistentTreeMap" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentTreeMap'new meta, (:cmp this), (:tree this), (:_count this))
            )
        )
    )

    (def #_"PersistentTreeMap" PersistentTreeMap'EMPTY (PersistentTreeMap'new))

    (defn #_"PersistentTreeMap" PersistentTreeMap'create
        ([#_"Seqable" keyvals]
            (loop-when [#_"PersistentTreeMap" m PersistentTreeMap'EMPTY #_"seq" s (seq keyvals)] (some? s) => m
                (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                    (recur (assoc m (first s) (second s)) (next (next s)))
                )
            )
        )
        ([#_"Comparator" cmp, #_"Seqable" keyvals]
            (loop-when [#_"PersistentTreeMap" m (PersistentTreeMap'new cmp) #_"seq" s (seq keyvals)] (some? s) => m
                (when (some? (next s)) => (throw! (str "no value supplied for key: " (first s)))
                    (recur (assoc m (first s) (second s)) (next (next s)))
                )
            )
        )
    )

    (defm PersistentTreeMap Seqable
        (#_"seq" Seqable'''seq [#_"PersistentTreeMap" this]
            (when (pos? (:_count this))
                (TSeq'create (:tree this), true, (:_count this))
            )
        )
    )

    (defm PersistentTreeMap IPersistentCollection
        ;; inherit APersistentMap conj

        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"PersistentTreeMap" this]
            (PersistentTreeMap'new (meta this), (:cmp this))
        )
    )

    (defm PersistentTreeMap Reversible
        (#_"seq" Reversible'''rseq [#_"PersistentTreeMap" this]
            (when (pos? (:_count this))
                (TSeq'create (:tree this), false, (:_count this))
            )
        )
    )

    (defn #_"int" PersistentTreeMap''doCompare [#_"PersistentTreeMap" this, #_"Object" k1, #_"Object" k2]
        (.compare (:cmp this), k1, k2)
    )

    (defm PersistentTreeMap Sorted
        (#_"Comparator" Sorted'''comparator => :cmp)

        (#_"Object" Sorted'''entryKey [#_"PersistentTreeMap" this, #_"Object" entry]
            (key entry)
        )

        (#_"seq" Sorted'''seq [#_"PersistentTreeMap" this, #_"boolean" ascending?]
            (when (pos? (:_count this))
                (TSeq'create (:tree this), ascending?, (:_count this))
            )
        )

        (#_"seq" Sorted'''seqFrom [#_"PersistentTreeMap" this, #_"Object" key, #_"boolean" ascending?]
            (when (pos? (:_count this))
                (loop-when [#_"seq" s nil #_"TNode" t (:tree this)] (some? t) => (when (some? s) (TSeq'new s, ascending?))
                    (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                        (cond
                            (zero? cmp) (TSeq'new (cons t s), ascending?)
                            ascending?  (if (neg? cmp) (recur (cons t s) (ITNode'''left t)) (recur s (ITNode'''right t)))
                            :else       (if (pos? cmp) (recur (cons t s) (ITNode'''right t)) (recur s (ITNode'''left t)))
                        )
                    )
                )
            )
        )
    )

    (defm PersistentTreeMap IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"PersistentTreeMap" this, #_"fn" f, #_"Object" r]
            (let [r (if (some? (:tree this)) (INode'''kvreduce (:tree this), f, r) r)]
                (if (reduced? r) @r r)
            )
        )
    )

    (defn #_"TNode" PersistentTreeMap''min [#_"PersistentTreeMap" this]
        (when-some [#_"TNode" t (:tree this)]
            (loop-when-recur t (some? (ITNode'''left t)) (ITNode'''left t) => t)
        )
    )

    (defn #_"TNode" PersistentTreeMap''max [#_"PersistentTreeMap" this]
        (when-some [#_"TNode" t (:tree this)]
            (loop-when-recur t (some? (ITNode'''right t)) (ITNode'''right t) => t)
        )
    )

    (defn #_"Object" PersistentTreeMap''minKey [#_"PersistentTreeMap" this]
        (let [#_"TNode" t (PersistentTreeMap''min this)]
            (when (some? t) (:key t))
        )
    )

    (defn #_"Object" PersistentTreeMap''maxKey [#_"PersistentTreeMap" this]
        (let [#_"TNode" t (PersistentTreeMap''max this)]
            (when (some? t) (:key t))
        )
    )

    (defn #_"int" PersistentTreeMap''depth-2 [#_"PersistentTreeMap" this, #_"TNode" t]
        (when (some? t) => 0
            (inc (max (PersistentTreeMap''depth-2 this, (ITNode'''left t)) (PersistentTreeMap''depth-2 this, (ITNode'''right t))))
        )
    )

    (defn #_"int" PersistentTreeMap''depth-1 [#_"PersistentTreeMap" this]
        (PersistentTreeMap''depth-2 this, (:tree this))
    )

    (declare find)

    (defm PersistentTreeMap ILookup
        (#_"Object" ILookup'''valAt
            ([#_"PersistentTreeMap" this, #_"Object" key] (ILookup'''valAt this, key, nil))
            ([#_"PersistentTreeMap" this, #_"Object" key, #_"Object" not-found]
                (let [#_"TNode" node (find this key)]
                    (if (some? node) (IMapEntry'''val node) not-found)
                )
            )
        )
    )

    (defn #_"int" PersistentTreeMap''capacity [#_"PersistentTreeMap" this]
        (:_count this)
    )

    (defm PersistentTreeMap Counted
        (#_"int" Counted'''count => :_count)
    )

    (defn #_"TNode" PersistentTreeMap'rightBalance [#_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" ins]
        (cond
            (and (satisfies? Red ins) (satisfies? Red (ITNode'''right ins)))
                (PersistentTreeMap'red (:key ins), (IMapEntry'''val ins), (PersistentTreeMap'black key, val, left, (ITNode'''left ins)), (ITNode'''blacken (ITNode'''right ins)))
            (and (satisfies? Red ins) (satisfies? Red (ITNode'''left ins)))
                (PersistentTreeMap'red (:key (ITNode'''left ins)), (IMapEntry'''val (ITNode'''left ins)), (PersistentTreeMap'black key, val, left, (ITNode'''left (ITNode'''left ins))), (PersistentTreeMap'black (:key ins), (IMapEntry'''val ins), (ITNode'''right (ITNode'''left ins)), (ITNode'''right ins)))
            :else
                (PersistentTreeMap'black key, val, left, ins)
        )
    )

    (defn #_"TNode" PersistentTreeMap'balanceLeftDel [#_"Object" key, #_"Object" val, #_"TNode" del, #_"TNode" right]
        (cond
            (satisfies? Red del)
                (PersistentTreeMap'red key, val, (ITNode'''blacken del), right)
            (satisfies? Black right)
                (PersistentTreeMap'rightBalance key, val, del, (ITNode'''redden right))
            (and (satisfies? Red right) (satisfies? Black (ITNode'''left right)))
                (PersistentTreeMap'red (:key (ITNode'''left right)), (IMapEntry'''val (ITNode'''left right)), (PersistentTreeMap'black key, val, del, (ITNode'''left (ITNode'''left right))), (PersistentTreeMap'rightBalance (:key right), (IMapEntry'''val right), (ITNode'''right (ITNode'''left right)), (ITNode'''redden (ITNode'''right right))))
            :else
                (throw! "invariant violation")
        )
    )

    (defn #_"TNode" PersistentTreeMap'leftBalance [#_"Object" key, #_"Object" val, #_"TNode" ins, #_"TNode" right]
        (cond
            (and (satisfies? Red ins) (satisfies? Red (ITNode'''left ins)))
                (PersistentTreeMap'red (:key ins), (IMapEntry'''val ins), (ITNode'''blacken (ITNode'''left ins)), (PersistentTreeMap'black key, val, (ITNode'''right ins), right))
            (and (satisfies? Red ins) (satisfies? Red (ITNode'''right ins)))
                (PersistentTreeMap'red (:key (ITNode'''right ins)), (IMapEntry'''val (ITNode'''right ins)), (PersistentTreeMap'black (:key ins), (IMapEntry'''val ins), (ITNode'''left ins), (ITNode'''left (ITNode'''right ins))), (PersistentTreeMap'black key, val, (ITNode'''right (ITNode'''right ins)), right))
            :else
                (PersistentTreeMap'black key, val, ins, right)
        )
    )

    (defn #_"TNode" PersistentTreeMap'balanceRightDel [#_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" del]
        (cond
            (satisfies? Red del)
                (PersistentTreeMap'red key, val, left, (ITNode'''blacken del))
            (satisfies? Black left)
                (PersistentTreeMap'leftBalance key, val, (ITNode'''redden left), del)
            (and (satisfies? Red left) (satisfies? Black (ITNode'''right left)))
                (PersistentTreeMap'red (:key (ITNode'''right left)), (IMapEntry'''val (ITNode'''right left)), (PersistentTreeMap'leftBalance (:key left), (IMapEntry'''val left), (ITNode'''redden (ITNode'''left left)), (ITNode'''left (ITNode'''right left))), (PersistentTreeMap'black key, val, (ITNode'''right (ITNode'''right left)), del))
            :else
                (throw! "invariant violation")
        )
    )

    (defn #_"TNode" PersistentTreeMap''add [#_"PersistentTreeMap" this, #_"TNode" t, #_"Object" key, #_"Object" val, #_"Atom" found]
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
                    (let [#_"TNode" ins (if (neg? cmp) (PersistentTreeMap''add this, (ITNode'''left t), key, val, found) (PersistentTreeMap''add this, (ITNode'''right t), key, val, found))]
                        (cond
                            (nil? ins) nil ;; found below
                            (neg? cmp) (ITNode'''addLeft t, ins)
                            :else      (ITNode'''addRight t, ins)
                        )
                    )
                )
            )
        )
    )

    (defn- #_"TNode" PersistentTreeMap'append [#_"TNode" left, #_"TNode" right]
        (cond
            (nil? left)
                right
            (nil? right)
                left
            (satisfies? Red left)
                (if (satisfies? Red right)
                    (let [#_"TNode" app (PersistentTreeMap'append (ITNode'''right left), (ITNode'''left right))]
                        (if (satisfies? Red app)
                            (PersistentTreeMap'red (:key app), (IMapEntry'''val app), (PersistentTreeMap'red (:key left), (IMapEntry'''val left), (ITNode'''left left), (ITNode'''left app)), (PersistentTreeMap'red (:key right), (IMapEntry'''val right), (ITNode'''right app), (ITNode'''right right)))
                            (PersistentTreeMap'red (:key left), (IMapEntry'''val left), (ITNode'''left left), (PersistentTreeMap'red (:key right), (IMapEntry'''val right), app, (ITNode'''right right)))
                        )
                    )
                    (PersistentTreeMap'red (:key left), (IMapEntry'''val left), (ITNode'''left left), (PersistentTreeMap'append (ITNode'''right left), right))
                )
            (satisfies? Red right)
                (PersistentTreeMap'red (:key right), (IMapEntry'''val right), (PersistentTreeMap'append left, (ITNode'''left right)), (ITNode'''right right))
            :else ;; black/black
                (let [#_"TNode" app (PersistentTreeMap'append (ITNode'''right left), (ITNode'''left right))]
                    (if (satisfies? Red app)
                        (PersistentTreeMap'red (:key app), (IMapEntry'''val app), (PersistentTreeMap'black (:key left), (IMapEntry'''val left), (ITNode'''left left), (ITNode'''left app)), (PersistentTreeMap'black (:key right), (IMapEntry'''val right), (ITNode'''right app), (ITNode'''right right)))
                        (PersistentTreeMap'balanceLeftDel (:key left), (IMapEntry'''val left), (ITNode'''left left), (PersistentTreeMap'black (:key right), (IMapEntry'''val right), app, (ITNode'''right right)))
                    )
                )
        )
    )

    (defn #_"TNode" PersistentTreeMap''remove [#_"PersistentTreeMap" this, #_"TNode" t, #_"Object" key, #_"Atom" found]
        (when (some? t) => nil ;; not found indicator
            (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                (if (zero? cmp)
                    (do
                        (reset! found t)
                        (PersistentTreeMap'append (ITNode'''left t), (ITNode'''right t))
                    )
                    (let [#_"TNode" del (if (neg? cmp) (PersistentTreeMap''remove this, (ITNode'''left t), key, found) (PersistentTreeMap''remove this, (ITNode'''right t), key, found))]
                        (when (or (some? del) (some? @found)) => nil ;; not found below
                            (if (neg? cmp)
                                (if (satisfies? Black (ITNode'''left t))
                                    (PersistentTreeMap'balanceLeftDel (:key t), (IMapEntry'''val t), del, (ITNode'''right t))
                                    (PersistentTreeMap'red (:key t), (IMapEntry'''val t), del, (ITNode'''right t))
                                )
                                (if (satisfies? Black (ITNode'''right t))
                                    (PersistentTreeMap'balanceRightDel (:key t), (IMapEntry'''val t), (ITNode'''left t), del)
                                    (PersistentTreeMap'red (:key t), (IMapEntry'''val t), (ITNode'''left t), del)
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (defn- #_"TNode" PersistentTreeMap''replace [#_"PersistentTreeMap" this, #_"TNode" t, #_"Object" key, #_"Object" val]
        (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
            (ITNode'''replace t, (:key t), (if (zero? cmp) val (IMapEntry'''val t)), (if (neg? cmp) (PersistentTreeMap''replace this, (ITNode'''left t), key, val) (ITNode'''left t)), (if (pos? cmp) (PersistentTreeMap''replace this, (ITNode'''right t), key, val) (ITNode'''right t)))
        )
    )

    (defn #_"Red" PersistentTreeMap'red [#_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" right]
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

    (defn #_"Black" PersistentTreeMap'black [#_"Object" key, #_"Object" val, #_"TNode" left, #_"TNode" right]
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

    (defm PersistentTreeMap Associative
        (#_"PersistentTreeMap" Associative'''assoc [#_"PersistentTreeMap" this, #_"Object" key, #_"Object" val]
            (let [#_"Atom" found (atom nil) #_"TNode" t (PersistentTreeMap''add this, (:tree this), key, val, found)]
                (if (nil? t)
                    (if (= (IMapEntry'''val #_"TNode" @found) val)
                        this
                        (PersistentTreeMap'new (meta this), (:cmp this), (PersistentTreeMap''replace this, (:tree this), key, val), (:_count this))
                    )
                    (PersistentTreeMap'new (meta this), (:cmp this), (ITNode'''blacken t), (inc (:_count this)))
                )
            )
        )

        (#_"boolean" Associative'''containsKey [#_"PersistentTreeMap" this, #_"Object" key]
            (some? (find this key))
        )

        (#_"TNode" Associative'''entryAt [#_"PersistentTreeMap" this, #_"Object" key]
            (loop-when [#_"TNode" t (:tree this)] (some? t) => t
                (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                    (cond
                        (neg? cmp) (recur (ITNode'''left t))
                        (pos? cmp) (recur (ITNode'''right t))
                        :else      t
                    )
                )
            )
        )
    )

    (defm PersistentTreeMap IPersistentMap
        (#_"PersistentTreeMap" IPersistentMap'''dissoc [#_"PersistentTreeMap" this, #_"Object" key]
            (let [#_"Atom" found (atom nil) #_"TNode" t (PersistentTreeMap''remove this, (:tree this), key, found)]
                (if (nil? t)
                    (if (nil? @found)
                        this
                        (PersistentTreeMap'new (meta this), (:cmp this))
                    )
                    (PersistentTreeMap'new (meta this), (:cmp this), (ITNode'''blacken t), (dec (:_count this)))
                )
            )
        )
    )

    (§ inherit PersistentTreeMap APersistentMap IObject'''equals IObject'''toString Hashed'''hash IFn'''invoke IFn'''applyTo)
)
)

(about #_"cloiure.core.PersistentTreeSet"

(about #_"PersistentTreeSet"
    (defr PersistentTreeSet [])

    #_inherit
    (defm PersistentTreeSet AFn APersistentSet)

    (defn #_"PersistentTreeSet" PersistentTreeSet'new [#_"meta" meta, #_"map" impl]
        (merge (PersistentTreeSet'class.) (APersistentSet'new impl)
            (hash-map
                #_"meta" :_meta meta
            )
        )
    )

    (defm PersistentTreeSet IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentTreeSet IObj
        (#_"PersistentTreeSet" IObj'''withMeta [#_"PersistentTreeSet" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentTreeSet'new meta, (:impl this))
            )
        )
    )

    (def #_"PersistentTreeSet" PersistentTreeSet'EMPTY (PersistentTreeSet'new nil, PersistentTreeMap'EMPTY))

    (defn #_"PersistentTreeSet" PersistentTreeSet'create
        ([                    #_"Seqable" items] (into PersistentTreeSet'EMPTY                                       items))
        ([#_"Comparator" cmp, #_"Seqable" items] (into (PersistentTreeSet'new nil, (PersistentTreeMap'new nil, cmp)) items))
    )

    (defm PersistentTreeSet IPersistentSet
        (#_"IPersistentSet" IPersistentSet'''disj [#_"PersistentTreeSet" this, #_"Object" key]
            (if (contains? this key)
                (PersistentTreeSet'new (meta this), (dissoc (:impl this) key))
                this
            )
        )

        ;; inherit APersistentSet contains? get
    )

    (defm PersistentTreeSet IPersistentCollection
        (#_"PersistentTreeSet" IPersistentCollection'''conj [#_"PersistentTreeSet" this, #_"Object" o]
            (if (contains? this o)
                this
                (PersistentTreeSet'new (meta this), (assoc (:impl this) o o))
            )
        )

        (#_"PersistentTreeSet" IPersistentCollection'''empty [#_"PersistentTreeSet" this]
            (PersistentTreeSet'new (meta this), (empty (:impl this)))
        )
    )

    (defm PersistentTreeSet Reversible
        (#_"seq" Reversible'''rseq [#_"PersistentTreeSet" this]
            (map key (rseq (:impl this)))
        )
    )

    (defm PersistentTreeSet Sorted
        (#_"Comparator" Sorted'''comparator [#_"PersistentTreeSet" this]
            (Sorted'''comparator (:impl this))
        )

        (#_"Object" Sorted'''entryKey [#_"PersistentTreeSet" this, #_"Object" entry]
            entry
        )

        (#_"seq" Sorted'''seq [#_"PersistentTreeSet" this, #_"boolean" ascending?]
            (keys (Sorted'''seq (:impl this), ascending?))
        )

        (#_"seq" Sorted'''seqFrom [#_"PersistentTreeSet" this, #_"Object" key, #_"boolean" ascending?]
            (keys (Sorted'''seqFrom (:impl this), key, ascending?))
        )
    )

    (§ inherit PersistentTreeSet APersistentSet IPersistentSet'''get Counted'''count Seqable'''seq IFn'''invoke IFn'''applyTo IObject'''equals IObject'''toString Hashed'''hash)
)
)

(about #_"arbace.wector"

(about #_"WNode"
    (defq WNode [edit, array, index])

    (defn #_"node" WNode'new [#_"Thread'" edit, #_"array" array, #_"index" index]
        (assoc!! (WNode'class. (anew 3))
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

    (defn #_"value" WNode''value-for [#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
        (when (< -1 i cnt) => (throw! "index is out of bounds")
            (when (< i tail-off) => (aget tail (- i tail-off))
                (loop-when [i i #_"node" node this shift shift] (pos? shift) => (aget (:array node) (& (>>> i shift) 0x1f))
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

    (defn #_"node" WNode''pop-tail [#_"node" this, #_"Thread'" edit, #_"int" shift, #_"int" tail-off]
        (let [
            #_"boolean" cow? (WNode''cow? this, edit) #_"array" a (:array this) #_"index" x (:index this)
            #_"int" e (& (>>> (dec tail-off) shift) 0x1f)
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
                            #_"node" child (WNode''pop-tail (aget a e), edit, (- shift 5), tail-off)
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

    (declare mapcat)
    (declare take)

    (defn- #_"seq" WNode''leaf-seq [#_"node" this]
        (let [
            #_"array" a (:array this)
        ]
            (mapcat :array (take (WNode'index-of-nil a) a))
        )
    )

    (declare drop)

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
    (defq TransientWector [cnt, shift, root, tail, tlen])

    #_inherit
    (defm TransientWector AFn)

    (defn #_"TransientWector" TransientWector'new
        ([#_"PersistentWector" w]
            (TransientWector'new (:cnt w), (:shift w), (WNode''editable-root (:root w)), (WNode'editable-tail (:tail w)), (alength (:tail w)))
        )
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen]
            (assoc!! (TransientWector'class. (anew 5))
                #_"int" :cnt cnt
                #_"int" :shift shift
                #_"node" :root root
                #_"values" :tail tail
                #_"int" :tlen tlen
            )
        )
    )

    (defm TransientWector Counted
        (#_"int" Counted'''count [#_"TransientWector" this]
            (WNode''assert-editable (:root this))
            (:cnt this)
        )
    )

    (defn- #_"int" TransientWector''tail-off [#_"TransientWector" this]
        (- (:cnt this) (:tlen this))
    )

    (defn- #_"values" TransientWector''array-for [#_"TransientWector" this, #_"int" i]
        (WNode''array-for (:root this), i, (:shift this), (:cnt this), (TransientWector''tail-off this), (:tail this))
    )

    (defn- #_"value" TransientWector''value-for [#_"TransientWector" this, #_"int" i]
        (WNode''value-for (:root this), i, (:shift this), (:cnt this), (TransientWector''tail-off this), (:tail this))
    )

    (defm TransientWector Indexed
        (#_"value" Indexed'''nth
            ([#_"TransientWector" this, #_"int" i]
                (WNode''assert-editable (:root this))
                (TransientWector''value-for this, i)
            )
            ([#_"TransientWector" this, #_"int" i, #_"value" not-found]
                (WNode''assert-editable (:root this))
                (when (< -1 i (:cnt this)) => not-found
                    (TransientWector''value-for this, i)
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
                        (TransientWector''value-for this, i)
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
                    _ (aset! (:tail this) (:tlen this) val)
                ]
                    (-> this (update!! :cnt inc) (update!! :tlen inc))
                )
                (let [
                    #_"node" tail-node (WNode'new (:edit (:root this)), (:tail this), nil)
                    this (assoc!! this :tail (-> (anew 32) (aset! 0 val)), :tlen 1)
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
                            (-> this (assoc!! :root root) (update!! :shift + 5) (update!! :cnt inc))
                        )
                        (let [
                            #_"node" root (WNode''push-tail (:root this), (:edit (:root this)), (:shift this), (:cnt this), tail-node)
                        ]
                            (-> this (assoc!! :root root) (update!! :cnt inc))
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
                            (aset! (:tail this) (- i tail-off) val)
                            this
                        )
                        (do
                            (assoc!! this :root (WNode''do-assoc (:root this), (:edit (:root this)), (:shift this), i, val))
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
                        this (assoc!! this :cnt 0)
                        this (assoc!! this :tlen 0)
                        _ (aset! (:tail this) 0 nil)
                    ]
                        this
                    )
                (< 1 (:tlen this))
                    (let [
                        this (update!! this :cnt dec)
                        this (update!! this :tlen dec)
                        _ (aset! (:tail this) (:tlen this) nil)
                    ]
                        this
                    )
                :else
                    (let [
                        #_"values" tail (aclone (TransientWector''array-for this, (- (:cnt this) 2)))
                        #_"node" root (WNode''pop-tail (:root this), (:edit (:root this)), (:shift this), (TransientWector''tail-off this))
                        this
                            (cond
                                (nil? root)
                                    (-> this
                                        (assoc!! :root (WNode'new (:edit (:root this)), nil, nil))
                                    )
                                (and (< 5 (:shift this)) (nil? (aget (:array root) 1)))
                                    (-> this
                                        (update!! :shift - 5)
                                        (assoc!! :root (aget (:array root) 0))
                                    )
                                :else
                                    (-> this
                                        (assoc!! :root root)
                                    )
                            )
                    ]
                        (-> this
                            (update!! :cnt dec)
                            (assoc!! :tail tail)
                            (assoc!! :tlen (alength tail))
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
                    (MapEntry'new key, (Indexed'''nth this, i))
                )
            )
        )
    )
)

(about #_"PersistentWector"
    (defq PersistentWector [_meta, cnt, shift, root, tail])

    #_inherit
    (defm PersistentWector AFn APersistentVector)

    (defn #_"PersistentWector" PersistentWector'new
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] (PersistentWector'new nil, cnt, shift, root, tail))
        ([#_"meta" meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
            (assoc!! (PersistentWector'class. (anew 5))
                #_"meta" :_meta meta
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
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm PersistentWector IObj
        (#_"PersistentWector" IObj'''withMeta [#_"PersistentWector" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (PersistentWector'new meta, (:cnt this), (:shift this), (:root this), (:tail this))
            )
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

        (#_"String" IObject'''toString => RT'printString)
    )

    (defm PersistentWector Hashed
        (#_"int" Hashed'''hash [#_"PersistentWector" this]
            (loop-when [#_"int" hash 1 #_"int" i 0] (< i (:cnt this)) => (Murmur3'mixCollHash hash, i)
                (recur (+ (* 31 hash) (f'hash (Indexed'''nth this, i))) (inc i))
            )
        )
    )

    (defm PersistentWector IEditableCollection
        (#_"TransientWector" IEditableCollection'''asTransient => TransientWector'new)
    )

    (defm PersistentWector Counted
        (#_"int" Counted'''count => :cnt)
    )

    (defn- #_"int" PersistentWector''tail-off [#_"PersistentWector" this]
        (- (:cnt this) (alength (:tail this)))
    )

    (defn- #_"values" PersistentWector''array-for [#_"PersistentWector" this, #_"int" i]
        (WNode''array-for (:root this), i, (:shift this), (:cnt this), (PersistentWector''tail-off this), (:tail this))
    )

    (defn- #_"value" PersistentWector''value-for [#_"PersistentWector" this, #_"int" i]
        (WNode''value-for (:root this), i, (:shift this), (:cnt this), (PersistentWector''tail-off this), (:tail this))
    )

    (defm PersistentWector Indexed
        (#_"value" Indexed'''nth
            ([#_"PersistentWector" this, #_"int" i]
                (PersistentWector''value-for this, i)
            )
            ([#_"PersistentWector" this, #_"int" i, #_"value" not-found]
                (when (< -1 i (:cnt this)) => not-found
                    (PersistentWector''value-for this, i)
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
            (case (:cnt this)
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
                            #_"node" root (WNode''pop-tail (:root this), (:edit (:root this)), shift, (PersistentWector''tail-off this))
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
            ([#_"PersistentWector" this, #_"fn" f]
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
            ([#_"PersistentWector" this, #_"fn" f, #_"value" r]
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
        (#_"value" IKVReduce'''kvreduce [#_"PersistentWector" this, #_"fn" f, #_"value" r]
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
                    (MapEntry'new key, (Indexed'''nth this, i))
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
                        (PersistentWector''value-for this, i)
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
                            #_"node" r1 (:root this) #_"int" s1 (:shift this) #_"array" t1 (:tail this) #_"int" o1 (PersistentWector''tail-off this)
                            #_"boolean" overflow? (WNode''overflow? r1, s1, (+ o1 32))
                            r1
                                (when overflow? => (WNode''fold-tail r1, s1, o1, t1)
                                    (let [
                                        #_"array" a'
                                            (-> (anew 32)
                                                (aset! 0 r1)
                                                (aset! 1 (WNode''new-path (WNode'new nil, t1, nil), nil, s1))
                                            )
                                        #_"index" x'
                                            (when (or (some? (:index r1)) (< (alength t1) 32))
                                                (-> (anew 33) (aset! 0 o1) (aset! 1 c1) (aset! 32 2))
                                            )
                                    ]
                                        (WNode'new nil, a', x')
                                    )
                                )
                            s1 (if overflow? (+ s1 5) s1)
                            #_"node" r2 (:root that) #_"int" s2 (:shift that) #_"array" t2 (:tail that) #_"int" o2 (PersistentWector''tail-off that)
                            #_"int" shift (max s1 s2)
                            r1 (WNode''shift-from-to r1, s1, shift)
                            r2 (WNode''shift-from-to r2, s2, shift)
                            [#_"node" n1 #_"node" n2 #_"int" delta] (WNode'zip-path shift, r1, c1, r2, o2, 0)
                            #_"int" c1' (+ c1 delta)
                            #_"int" c2' (- o2 delta)
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

(about #_"cloiure.core.Repeat"

(about #_"Repeat"
    (defr Repeat [])

    #_inherit
    (defm Repeat ASeq)

    (def- #_"long" Repeat'INFINITE -1)

    (defn- #_"Repeat" Repeat'new
        ([#_"long" cnt, #_"Object" val] (Repeat'new nil, cnt, val))
        ([#_"meta" meta, #_"long" cnt, #_"Object" val]
            (merge (Repeat'class.)
                (hash-map
                    #_"meta" :_meta meta

                    #_"long" :cnt cnt ;; always INFINITE or pos?
                    #_"Object" :val val
                )
            )
        )
    )

    (defm Repeat IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm Repeat IObj
        (#_"Repeat" IObj'''withMeta [#_"Repeat" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (Repeat'new meta, (:cnt this), (:val this))
            )
        )
    )

    (defn #_"Repeat|ISeq" Repeat'create
        ([#_"Object" val] (Repeat'new Repeat'INFINITE, val))
        ([#_"long" n, #_"Object" val] (if (pos? n) (Repeat'new n, val) ()))
    )

    (defm Repeat ISeq
        (#_"Object" ISeq'''first => :val)

        (#_"seq" ISeq'''next [#_"Repeat" this]
            (cond
                (< 1 (:cnt this))               (Repeat'new (dec (:cnt this)), (:val this))
                (= (:cnt this) Repeat'INFINITE) this
            )
        )
    )

    (defm Repeat IReduce
        (#_"Object" IReduce'''reduce
            ([#_"Repeat" this, #_"fn" f]
                (let [#_"Object" r (:val this)]
                    (if (= (:cnt this) Repeat'INFINITE)
                        (loop [r r]
                            (let [r (f r (:val this))]
                                (if (reduced? r) @r (recur r))
                            )
                        )
                        (loop-when [r r #_"long" i 1] (< i (:cnt this)) => r
                            (let [r (f r (:val this))]
                                (if (reduced? r) @r (recur r (inc i)))
                            )
                        )
                    )
                )
            )
            ([#_"Repeat" this, #_"fn" f, #_"Object" r]
                (if (= (:cnt this) Repeat'INFINITE)
                    (loop [r r]
                        (let [r (f r (:val this))]
                            (if (reduced? r) @r (recur r))
                        )
                    )
                    (loop-when [r r #_"long" i 0] (< i (:cnt this)) => r
                        (let [r (f r (:val this))]
                            (if (reduced? r) @r (recur r (inc i)))
                        )
                    )
                )
            )
        )
    )

    (defm Repeat Sequential)

    (defm Repeat Seqable
        (#_"seq" Seqable'''seq [#_"Repeat" this]
            this
        )
    )

    (defm Repeat Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Repeat IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.Range"

;;;
 ; Implements generic numeric (potentially infinite) range.
 ;;
(about #_"Range"
    (defr Range [])

    #_inherit
    (defm Range ASeq)

    #_abstract
    (defm Range Counted)

    (defn- #_"RangeBoundsCheck" Range'positiveStep [#_"Object" end]
        (reify RangeBoundsCheck
            (#_"boolean" RangeBoundsCheck'''exceededBounds [#_"RangeBoundsCheck" _self, #_"Object" val]
                (<= end val)
            )
        )
    )

    (defn- #_"RangeBoundsCheck" Range'negativeStep [#_"Object" end]
        (reify RangeBoundsCheck
            (#_"boolean" RangeBoundsCheck'''exceededBounds [#_"RangeBoundsCheck" _self, #_"Object" val]
                (<= val end)
            )
        )
    )

    (defn- #_"Range" Range'new
        ([#_"Object" start, #_"Object" end, #_"Object" step, #_"RangeBoundsCheck" boundsCheck]
            (Range'new nil, start, end, step, boundsCheck)
        )
        ([#_"meta" meta, #_"Object" start, #_"Object" end, #_"Object" step, #_"RangeBoundsCheck" boundsCheck]
            (merge (Range'class.)
                (hash-map
                    #_"meta" :_meta meta

                    ;; Invariants guarantee this is never an "empty" seq
                    #_"Object" :start start
                    #_"Object" :end end
                    #_"Object" :step step
                    #_"RangeBoundsCheck" :boundsCheck boundsCheck
                )
            )
        )
    )

    (defm Range IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm Range IObj
        (#_"Range" IObj'''withMeta [#_"Range" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (Range'new meta, (:end this), (:start this), (:step this), (:boundsCheck this))
            )
        )
    )

    (defn #_"seq" Range'create
        ([#_"Object" end]
            (when (pos? end) => ()
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
                    ()
                (zero? step)
                    (Repeat'create start)
                :else
                    (Range'new start, end, step, (if (pos? step) (Range'positiveStep end) (Range'negativeStep end)))
            )
        )
    )

    (defm Range ISeq
        (#_"Object" ISeq'''first => :start)

        (#_"seq" ISeq'''next [#_"Range" this]
            (let-when-not [#_"Object" n (+ (:start this) (:step this))] (RangeBoundsCheck'''exceededBounds (:boundsCheck this), n)
                (Range'new n, (:end this), (:step this), (:boundsCheck this))
            )
        )
    )

    (defm Range IReduce
        (#_"Object" IReduce'''reduce
            ([#_"Range" this, #_"fn" f]
                (loop [#_"Object" r (:start this) #_"Number" n r]
                    (let-when-not [n (+ n (:step this))] (RangeBoundsCheck'''exceededBounds (:boundsCheck this), n) => r
                        (let-when-not [r (f r n)] (reduced? r) => @r
                            (recur r n)
                        )
                    )
                )
            )
            ([#_"Range" this, #_"fn" f, #_"Object" r]
                (loop [r r #_"Object" n (:start this)]
                    (let-when-not [r (f r n)] (reduced? r) => @r
                        (let-when-not [n (+ n (:step this))] (RangeBoundsCheck'''exceededBounds (:boundsCheck this), n) => r
                            (recur r n)
                        )
                    )
                )
            )
        )
    )

    (defm Range Sequential)

    (defm Range Seqable
        (#_"seq" Seqable'''seq [#_"Range" this]
            this
        )
    )

    (defm Range Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm Range IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.StringSeq"

(about #_"StringSeq"
    (defr StringSeq [])

    #_inherit
    (defm StringSeq ASeq)

    (defn- #_"StringSeq" StringSeq'new [#_"meta" meta, #_"CharSequence" s, #_"int" i]
        (merge (StringSeq'class.)
            (hash-map
                #_"meta" :_meta meta

                #_"CharSequence" :s s
                #_"int" :i i
            )
        )
    )

    (defm StringSeq IMeta
        (#_"meta" IMeta'''meta => :_meta)
    )

    (defm StringSeq IObj
        (#_"StringSeq" IObj'''withMeta [#_"StringSeq" this, #_"meta" meta]
            (when-not (= meta (:_meta this)) => this
                (StringSeq'new meta, (:s this), (:i this))
            )
        )
    )

    (defn #_"StringSeq" StringSeq'create [#_"CharSequence" s]
        (when (pos? (count s))
            (StringSeq'new nil, s, 0)
        )
    )

    (-/extend-protocol Seqable CharSequence
        (#_"StringSeq" Seqable'''seq [#_"CharSequence" s] (#_StringSeq'create -/seq s))
    )

    (defm StringSeq ISeq
        (#_"Object" ISeq'''first [#_"StringSeq" this]
            (Character/valueOf (nth (:s this) (:i this)))
        )

        (#_"seq" ISeq'''next [#_"StringSeq" this]
            (when (< (inc (:i this)) (count (:s this)))
                (StringSeq'new (:_meta this), (:s this), (inc (:i this)))
            )
        )
    )

    (defm StringSeq Counted
        (#_"int" Counted'''count [#_"StringSeq" this]
            (- (count (:s this)) (:i this))
        )
    )

    (defm StringSeq IReduce
        (#_"Object" IReduce'''reduce
            ([#_"StringSeq" this, #_"fn" f]
                (let [#_"CharSequence" s (:s this) #_"int" i (:i this) #_"int" n (count s)]
                    (loop-when [#_"Object" r (nth s i) i (inc i)] (< i n) => r
                        (let [r (f r (nth s i))]
                            (if (reduced? r) @r (recur r (inc i)))
                        )
                    )
                )
            )
            ([#_"StringSeq" this, #_"fn" f, #_"Object" r]
                (let [#_"CharSequence" s (:s this) #_"int" i (:i this) #_"int" n (count s)]
                    (loop-when [r (f r (nth s i)) i (inc i)] (< i n) => (if (reduced? r) @r r)
                        (if (reduced? r) @r (recur (f r (nth s i)) (inc i)))
                    )
                )
            )
        )
    )

    (defm StringSeq Sequential)

    (defm StringSeq Seqable
        (#_"seq" Seqable'''seq [#_"StringSeq" this]
            this
        )
    )

    (defm StringSeq Hashed
        (#_"int" Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm StringSeq IObject
        (#_"boolean" IObject'''equals => ASeq''equals)

        (#_"String" IObject'''toString => RT'printString)
    )
)
)

(about #_"cloiure.core.Tuple"

(about #_"Tuple"
    (def #_"int" Tuple'MAX_SIZE 6)

    (defn #_"vector" Tuple'create
        ([] [])
        ([#_"Object" v0] (vector v0))
        ([#_"Object" v0, #_"Object" v1] (vector v0 v1))
        ([#_"Object" v0, #_"Object" v1, #_"Object" v2] (vector v0 v1 v2))
        ([#_"Object" v0, #_"Object" v1, #_"Object" v2, #_"Object" v3] (vector v0 v1 v2 v3))
        ([#_"Object" v0, #_"Object" v1, #_"Object" v2, #_"Object" v3, #_"Object" v4] (vector v0 v1 v2 v3 v4))
        ([#_"Object" v0, #_"Object" v1, #_"Object" v2, #_"Object" v3, #_"Object" v4, #_"Object" v5] (vector v0 v1 v2 v3 v4 v5))
    )
)
)

(about #_"cloiure.core.Var"

(about #_"Unbound"
    (defr Unbound [])

    #_inherit
    (defm Unbound AFn)

    (defn #_"Unbound" Unbound'new [#_"Namespace" ns, #_"Symbol" sym]
        (merge (Unbound'class.)
            (hash-map
                #_"Namespace" :ns ns
                #_"Symbol" :sym sym
            )
        )
    )

    (declare Var'toString)

    (defm Unbound IObject
        ;; abstract IObject equals

        (#_"String" IObject'''toString [#_"Unbound" this]
            (str "Unbound: " (Var'toString (:ns this), (:sym this)))
        )
    )
)

(about #_"Var"
    (defr Var [])

    (def #_"ThreadLocal" Var'dvals (ThreadLocal.))

    (defn #_"Var" Var'find [#_"Symbol" sym]
        (when (some? (:ns sym)) => (throw! "symbol must be namespace-qualified")
            (let [#_"Namespace" ns (Namespace'find (Symbol'intern (:ns sym)))]
                (when (some? ns) => (throw! (str "no such namespace: " (:ns sym)))
                    (Namespace''findInternedVar ns, (Symbol'intern (:name sym)))
                )
            )
        )
    )

    (defn #_"Var" Var'new
        ([#_"Namespace" ns, #_"Symbol" sym] (Var'new ns, sym, (Unbound'new ns, sym)))
        ([#_"Namespace" ns, #_"Symbol" sym, #_"Object" root]
            (merge (Var'class.)
                (hash-map
                    #_"Namespace" :ns ns
                    #_"Symbol" :sym sym
                    #_"Object'" :root (atom root)
                )
            )
        )
    )

    (defm Var IMeta
        (#_"meta" IMeta'''meta [#_"Var" this]
            (meta (:root this))
        )
    )

    (defm Var IReference
        (#_"meta" IReference'''alterMeta [#_"Var" this, #_"fn" f, #_"seq" args]
            (apply alter-meta! (:root this) f args)
        )

        (#_"meta" IReference'''resetMeta [#_"Var" this, #_"meta" m]
            (reset-meta! (:root this) m)
        )
    )

    (defn- #_"String" Var'toString [#_"Namespace" ns, #_"Symbol" sym]
        (if (some? ns)
            (str "#'" (:name ns) "/" sym)
            (str "#<Var: " (or sym "--unnamed--") ">")
        )
    )

    (defm Var IObject
        ;; abstract IObject equals

        (#_"String" IObject'''toString [#_"Var" this]
            (Var'toString (:ns this), (:sym this))
        )
    )

    (defn #_"boolean" Var''hasRoot [#_"Var" this]
        (not (satisfies? Unbound @(:root this)))
    )

    (defn #_"boolean" Var''isBound [#_"Var" this]
        (or (Var''hasRoot this) (contains? (first (.get Var'dvals)) this))
    )

    (defn #_"Atom" Var''getThreadBinding [#_"Var" this]
        (get (first (.get Var'dvals)) this)
    )

    (defn #_"Object" Var''get [#_"Var" this]
        @(or (Var''getThreadBinding this) (:root this))
    )

    (defm Var IDeref
        (#_"Object" IDeref'''deref => Var''get)
    )

    (defn #_"Object" Var''set [#_"Var" this, #_"Object" val]
        (let [#_"Atom" v (Var''getThreadBinding this)]
            (when (some? v) => (throw! (str "can't change/establish root binding of: " (:sym this) " with var-set/set!"))
                (reset! v val)
            )
        )
    )

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

    (defn #_"Object" Var''getRawRoot [#_"Var" this]
        @(:root this)
    )

    (defn #_"void" Var''bindRoot [#_"Var" this, #_"Object" root]
        ;; binding root always clears macro flag
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

(declare the-ns)

;;;
 ; Finds or creates a var named by the symbol name in the namespace
 ; ns (which can be a symbol or a namespace), setting its root binding
 ; to val if supplied. The namespace must exist. The var will adopt
 ; any metadata from the name symbol. Returns the var.
 ;;
(§ defn intern
    ([ns name]
        (let [v (Var'intern (the-ns ns) name)]
            (when-some [m (meta name)]
                (reset-meta! v m)
            )
            v
        )
    )
    ([ns name o]
        (let [v (Var'intern (the-ns ns) name o)]
            (when-some [m (meta name)]
                (reset-meta! v m)
            )
            v
        )
    )
)

    (defn #_"void" Var'pushThreadBindings [#_"{Var Object}" bindings]
        (let [#_"seq" l (.get Var'dvals)]
            (loop-when [#_"{Var Atom}" m (first l) #_"seq" s (seq bindings)] (some? s) => (.set Var'dvals, (cons m l))
                (let [#_"IMapEntry" e (first s)]
                    (recur (assoc m (key e) (atom (val e))) (next s))
                )
            )
        )
        nil
    )

    (defn #_"void" Var'popThreadBindings []
        (let-when [#_"seq" s (.get Var'dvals)] (some? s) => (throw! "pop without matching push")
            (.set Var'dvals, (next s))
        )
        nil
    )

    (defn #_"{Var Object}" Var'getThreadBindings []
        (loop-when [#_"{Var Object}" m (transient {}) #_"seq" s (seq (first (.get Var'dvals)))] (some? s) => (persistent! m)
            (let [#_"IMapEntry" e (first s)]
                (recur (assoc! m (key e) @(val e)) (next s))
            )
        )
    )

    (defm Var IFn
        (#_"Object" IFn'''invoke
            ([#_"Var" this]                                                   (IFn'''invoke @this))
            ([#_"Var" this, a1]                                               (IFn'''invoke @this, a1))
            ([#_"Var" this, a1, a2]                                           (IFn'''invoke @this, a1, a2))
            ([#_"Var" this, a1, a2, a3]                                       (IFn'''invoke @this, a1, a2, a3))
            ([#_"Var" this, a1, a2, a3, a4]                                   (IFn'''invoke @this, a1, a2, a3, a4))
            ([#_"Var" this, a1, a2, a3, a4, a5]                               (IFn'''invoke @this, a1, a2, a3, a4, a5))
            ([#_"Var" this, a1, a2, a3, a4, a5, a6]                           (IFn'''invoke @this, a1, a2, a3, a4, a5, a6))
            ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7]                       (IFn'''invoke @this, a1, a2, a3, a4, a5, a6, a7))
            ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8]                   (IFn'''invoke @this, a1, a2, a3, a4, a5, a6, a7, a8))
            ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]               (IFn'''invoke @this, a1, a2, a3, a4, a5, a6, a7, a8, a9))
            ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args] (IFn'''invoke @this, a1, a2, a3, a4, a5, a6, a7, a8, a9, args))
        )

        (#_"Object" IFn'''applyTo [#_"Var" this, #_"seq" args]
            (IFn'''applyTo @this, args)
        )
    )
)
)

(about #_"cloiure.core.RT"

(about #_"RT"
    (defn #_"Object" RT'seqOrElse [#_"Object" o]
        (when (some? (seq o))
            o
        )
    )

    (defn #_"IPersistentCollection" RT'conj [#_"IPersistentCollection" coll, #_"Object" x]
        (if (some? coll) (IPersistentCollection'''conj coll, x) (list x))
    )

;;;
 ; conj[oin].
 ; Returns a new collection with the items 'added'. (conj nil item) returns (item).
 ; The 'addition' may happen at different 'places' depending on the concrete type.
 ;;
(defn conj
    ([] [])
    ([coll] coll)
    ([coll x] (RT'conj coll x))
    ([coll x & s] (recur-when s [(conj coll x) (first s) (next s)] => (conj coll x)))
)

;;;
 ; Returns a new seq where x is the first element and s is the rest.
 ;;
(§ defn cons [x s] (Cons'new x, (seq s)))

    (defn #_"Object" RT'peek [#_"IPersistentStack" s]
        (when (some? s)
            (IPersistentStack'''peek s)
        )
    )

;;;
 ; For a list or queue, same as first, for a vector, same as, but much
 ; more efficient than, last. If the collection is empty, returns nil.
 ;;
(defn peek [s] (RT'peek s))

    (defn #_"Object" RT'pop [#_"IPersistentStack" s]
        (when (some? s)
            (IPersistentStack'''pop s)
        )
    )

;;;
 ; Return a seq of all but the last item in coll, in linear time.
 ;;
(defn butlast [s] (loop-when-recur [v [] s s] (next s) [(conj v (first s)) (next s)] => (seq v)))

;;;
 ; For a list or queue, returns a new list/queue without the first item,
 ; for a vector, returns a new vector without the last item.
 ; If the collection is empty, throws an exception.
 ; Note - not the same as next/butlast.
 ;;
(defn pop [s] (RT'pop s))

    (defn #_"Object" RT'get
        ([#_"Object" coll, #_"Object" key]
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key)
                (nil? coll)
                    nil
                (set? coll)
                    (IPersistentSet'''get coll, key)
                (and (number? key) (or (string? coll) (.isArray (class coll))))
                    (let-when [#_"int" n (.intValue #_"Number" key)] (< -1 n (count coll))
                        (nth coll n)
                    )
                (satisfies? ITransientSet coll)
                    (ITransientSet'''get coll, key)
            )
        )
        ([#_"Object" coll, #_"Object" key, #_"Object" not-found]
            (cond
                (satisfies? ILookup coll)
                    (ILookup'''valAt coll, key, not-found)
                (nil? coll)
                    not-found
                (set? coll)
                    (if (contains? coll key) (IPersistentSet'''get coll, key) not-found)
                (and (number? key) (or (string? coll) (.isArray (class coll))))
                    (let [#_"int" n (.intValue #_"Number" key)]
                        (if (< -1 n (count coll)) (nth coll n) not-found)
                    )
                (satisfies? ITransientSet coll)
                    (if (contains? coll key) (ITransientSet'''get coll, key) not-found)
                :else
                    not-found
            )
        )
    )

;;;
 ; Returns the value mapped to key, not-found or nil if key not present.
 ;;
(defn get
    ([coll key          ] (RT'get coll key          ))
    ([coll key not-found] (RT'get coll key not-found))
)

    (defn #_"Associative" RT'assoc [#_"Object" coll, #_"Object" key, #_"Object" val]
        (if (some? coll)
            (Associative'''assoc #_"Associative" coll, key, val)
            #_(PersistentArrayMap'new (object-array [ key, val ])) (-/assoc coll key val)
        )
    )

;;;
 ; assoc[iate].
 ; When applied to a map, returns a new map of the same (hashed/sorted) type, that contains the mapping of key(s) to val(s).
 ; When applied to a vector, returns a new vector that contains val at index. Note - index must be <= (count vector).
 ;;
(defn assoc
    ([a k v] (RT'assoc a k v))
    ([a k v & kvs]
        (let-when [a (assoc a k v)] kvs => a
            (when (next kvs) => (throw! "assoc expects even number of arguments after map/vector, found odd number")
                (recur a (first kvs) (second kvs) (next (next kvs)))
            )
        )
    )
)

    (defn #_"Object" RT'contains [#_"Object" coll, #_"Object" key]
        (cond
            (nil? coll)
                false
            (associative? coll)
                (if (Associative'''containsKey coll, key) true false)
            (set? coll)
                (if (IPersistentSet'''contains? coll, key) true false)
            (and (number? key) (or (string? coll) (.isArray (class coll))))
                (let [#_"int" n (.intValue #_"Number" key)]
                    (if (< -1 n (count coll)) true false)
                )
            (satisfies? ITransientSet coll)
                (if (ITransientSet'''contains? coll, key) true false)
            (satisfies? ITransientAssociative coll)
                (if (ITransientAssociative'''containsKey coll, key) true false)
            :else
                (throw! (str "contains? not supported on " (class coll)))
        )
    )

;;;
 ; Returns true if key is present in the given collection, otherwise
 ; returns false. Note that for numerically indexed collections, like
 ; vectors and Java arrays, this tests if the numeric key is within the
 ; range of indexes. 'contains?' operates constant or logarithmic time;
 ; it will not perform a linear search for a value. See also 'some'.
 ;;
(defn contains? [coll key] (RT'contains coll key))

    (defn #_"Object" RT'find [#_"Object" coll, #_"Object" key]
        (cond
            (nil? coll)
                nil
            (associative? coll)
                (Associative'''entryAt coll, key)
            (satisfies? ITransientAssociative coll)
                (ITransientAssociative'''entryAt coll, key)
            :else
                (throw! (str "find not supported on " (class coll)))
        )
    )

;;;
 ; Returns the map entry for k, or nil if key not present.
 ;;
(defn find [m k] (RT'find m k))

    ;; takes a seq of key, val, key, val
    ;; returns tail starting at val of matching key if found, else nil

    (defn #_"seq" RT'findKey [#_"Keyword" key, #_"seq" keyvals]
        (loop-when keyvals (some? keyvals)
            (let-when [#_"seq" s (next keyvals)] (some? s) => (throw! "malformed keyword argslist")
                (when-not (= (first keyvals) key) => s
                    (recur (next s))
                )
            )
        )
    )

    (defn #_"Object" RT'dissoc [#_"Object" coll, #_"Object" key]
        (when (some? coll)
            (IPersistentMap'''dissoc #_"IPersistentMap" coll, key)
        )
    )

;;;
 ; dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
 ; that does not contain a mapping for key(s).
 ;;
(defn dissoc
    ([m] m)
    ([m k] (RT'dissoc m k))
    ([m k & ks] (let [m (dissoc m k)] (recur-when ks [m (first ks) (next ks)] => m)))
)

    (defn #_"Object" RT'nth
        ([#_"Object" coll, #_"int" n]
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n)
                (nil? coll)
                    nil
                (instance? CharSequence coll)
                    (Character/valueOf (.charAt #_"CharSequence" coll, n))
                (.isArray (class coll))
                    (Reflector'prepRet (.getComponentType (class coll)), (Array/get coll, n))
                (instance? Matcher coll)
                    (.group #_"Matcher" coll, n)
                (map-entry? coll)
                    (let [#_"IMapEntry" e coll]
                        (case n 0 (key e) 1 (val e) (throw! "index is out of bounds"))
                    )
                (sequential? coll)
                    (loop-when [#_"int" i 0 #_"seq" s (seq coll)] (and (<= i n) (some? s)) => (throw! "index is out of bounds")
                        (recur-when (< i n) [(inc i) (next s)] => (first s))
                    )
                :else
                    (throw! (str "nth not supported on " (class coll)))
            )
        )
        ([#_"Object" coll, #_"int" n, #_"Object" not-found]
            (cond
                (indexed? coll)
                    (Indexed'''nth coll, n, not-found)
                (nil? coll)
                    not-found
                (neg? n)
                    not-found
                (instance? CharSequence coll)
                    (let-when [#_"CharSequence" s coll] (< n (.length s)) => not-found
                        (Character/valueOf (.charAt s, n))
                    )
                (.isArray (class coll))
                    (when (< n (Array/getLength coll)) => not-found
                        (Reflector'prepRet (.getComponentType (class coll)), (Array/get coll, n))
                    )
                (instance? Matcher coll)
                    (let-when [#_"Matcher" m coll] (< n (.groupCount m)) => not-found
                        (.group m, n)
                    )
                (map-entry? coll)
                    (let [#_"IMapEntry" e coll]
                        (case n 0 (key e) 1 (val e) not-found)
                    )
                (sequential? coll)
                    (loop-when [#_"int" i 0 #_"seq" s (seq coll)] (and (<= i n) (some? s)) => not-found
                        (recur-when (< i n) [(inc i) (next s)] => (first s))
                    )
                :else
                    (throw! (str "nth not supported on " (class coll)))
            )
        )
    )

;;;
 ; Returns the value at the index.
 ; get returns nil if index out of bounds, nth throws an exception unless not-found is supplied.
 ; nth also works for strings, arrays, regex matchers and lists, and, in O(n) time, for sequences.
 ;;
(defn nth
    ([s i]           (RT'nth s i          ))
    ([s i not-found] (RT'nth s i not-found))
)

    (defn #_"Object"    RT'box [#_"Object"  x] x)
    (defn #_"Character" RT'box-1c [#_"char"    x] (Character/valueOf x))
    (defn #_"Object"    RT'box-1z [#_"boolean" x] (if x true false))
    (defn #_"Object"    RT'box-1Z [#_"Boolean" x] x)
    (defn #_"Number"    RT'box-1b [#_"byte"    x] x)
    (defn #_"Number"    RT'box-1i [#_"int"     x] x)
    (defn #_"Number"    RT'box-1l [#_"long"    x] x)

    (defn #_"boolean" RT'booleanCast-1b [#_"boolean" x]
        x
    )

    (defn #_"boolean" RT'booleanCast [#_"Object" x]
        (if (boolean? x) (.booleanValue #_"Boolean" x) (some? x))
    )

    (defn #_"int" RT'intCast-1b [#_"byte"  x] x)
    (defn #_"int" RT'intCast-1c [#_"char"  x] x)
    (defn #_"int" RT'intCast-1i [#_"int"   x] x)

    (defn #_"int" RT'intCast-1l [#_"long" x]
        (let [#_"int" i (int x)]
            (when (= i x) => (throw! (str "value out of range for int: " x))
                i
            )
        )
    )

    (defn #_"int" RT'intCast [#_"Object" x]
        (cond
            (instance? Integer x) (.intValue #_"Integer" x)
            (number? x)           (RT'intCast-1l (long x))
            :else                 (.charValue (cast Character x))
        )
    )

    (defn #_"long" RT'longCast-1b [#_"byte"  x] x)
    (defn #_"long" RT'longCast-1i [#_"int"   x] x)
    (defn #_"long" RT'longCast-1l [#_"long"  x] x)

    (defn #_"long" RT'longCast [#_"Object" x]
        (cond
            (or (instance? Long x) (instance? Integer x) (instance? Byte x))
                (.longValue x)
            (instance? BigInteger x)
                (when (< (.bitLength x) 64) => (throw! (str "value out of range for long: " x))
                    (.longValue x)
                )
            (satisfies? Ratio x)
                (long (Ratio''bigIntegerValue x))
            (char? x)
                (RT'longCast-1l (.charValue #_"Character" x))
            :else
                (throw! (str "unexpected value type cast for long: " x))
        )
    )

    (defn #_"IPersistentMap" RT'map [& #_"Object..." init]
        (cond
            (nil? init)
                PersistentArrayMap'EMPTY
            (<= (count init) PersistentArrayMap'HASHTABLE_THRESHOLD)
                (PersistentArrayMap'createWithCheck init)
            :else
                (PersistentHashMap'createWithCheck-1a init)
        )
    )

    (defn #_"IPersistentMap" RT'mapUniqueKeys [& #_"Object..." init]
        (cond
            (nil? init)
                PersistentArrayMap'EMPTY
            (<= (count init) PersistentArrayMap'HASHTABLE_THRESHOLD)
                (PersistentArrayMap'new init)
            :else
                (PersistentHashMap'create-1a init)
        )
    )

    (defn #_"IPersistentSet" RT'set [& #_"Object..." init]
        (PersistentHashSet'createWithCheck-1a init)
    )

    (defn #_"seq" RT'arrayToSeq [#_"Object[]" a]
        (loop-when-recur [#_"seq" s nil #_"int" i (dec (count a))] (<= 0 i) [(cons (aget a i) s) (dec i)] => s)
    )

    (defn #_"Object[]" RT'seqToArray [#_"seq" s]
        (let [#_"Object[]" a (make-array Object (count s))]
            (loop-when-recur [#_"int" i 0 s s] (some? s) [(inc i) (next s)]
                (aset! a i (first s))
            )
            a
        )
    )

    (defn #_"?[]" RT'seqToTypedArray [#_"Class" type, #_"seq" s]
        (let [#_"?[]" a (make-array (or type (class (first s)) Object) (count s))]
            (loop-when-recur [#_"int" i 0 s s] (some? s) [(inc i) (next s)]
                (aset! a i (first s))
            )
            a
        )
    )

    (defn #_"Object[]" RT'toArray [#_"Object" coll]
        (cond
            (nil? coll)
                (object-array 0)
            (instance? Object'array coll)
                coll
            (indexed? coll)
                (let [#_"int" n (count coll) #_"Object[]" a (object-array n)]
                    (dotimes [#_"int" i n]
                        (aset! a i (nth coll i))
                    )
                    a
                )
            (seqable? coll)
                (RT'seqToArray (seq coll))
            (string? coll)
                (let [#_"char[]" chars (.toCharArray coll)
                      #_"Object[]" a (object-array (count chars))]
                    (dotimes [#_"int" i (count chars)]
                        (aset! a i (aget chars i))
                    )
                    a
                )
            (.isArray (class coll))
                (let [#_"seq" s (seq coll)
                      #_"Object[]" a (object-array (count s))]
                    (loop-when-recur [#_"int" i 0 s s] (< i (count a)) [(inc i) (next s)]
                        (aset! a i (first s))
                    )
                    a
                )
            :else
                (throw! (str "unable to convert: " (class coll) " to Object[]"))
        )
    )

;;;
 ; Returns an array of Objects containing the contents of s.
 ;;
(§ defn #_"Object[]" to-array [s] (RT'toArray s))

    (declare LispReader'read)

    (defn #_"Object" RT'readString [#_"String" s]
        (let [#_"PushbackReader" r (PushbackReader. (java.io.StringReader. s))]
            (LispReader'read r)
        )
    )

    (declare pr-on)

    (defn #_"String" RT'printString [#_"Object" x]
        (let [#_"StringWriter" w (StringWriter.)]
            (pr-on x w) ;; call multimethod
            (.toString w)
        )
    )
)
)

(about #_"cloiure.core"

(declare remove)

;;;
 ; A good fdecl looks like (([a] ...) ([a b] ...)) near the end of defn.
 ;;
(defn- ^:dynamic assert-valid-fdecl [fdecl]
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

(declare some)
(declare mapv)

(defn- sigs [s]
    (assert-valid-fdecl s)
    (letfn [(sig- [s]
                (let [v (first s) s (next s) v (if (= '&form (first v)) (subvec v 2) v)] ;; elide implicit macro args
                    (let-when [m (first s)] (and (map? m) (next s)) => v
                        (with-meta v (conj (or (meta v) {}) m))
                    )
                )
            )
            (tag- [s]
                (let [v (sig- s) m (meta v) #_"Symbol" tag (:tag m)]
                    (when (and (symbol? tag) (not (some #{\.} (:name tag))) (not (Interop'maybeSpecialTag tag))) => v
                        (let [c (Interop'maybeClass tag false)]
                            (when c => v
                                (with-meta v (assoc m :tag (symbol (.getName c))))
                            )
                        )
                    )
                )
            )]
        (when (seq? (first s)) => (list (tag- s))
            (seq (mapv tag- s))
        )
    )
)

;;;
 ; Same as (def name (fn [params*] exprs*)) or (def name (fn ([params*] exprs*)+)) with any attrs added to the var metadata.
 ;;
(§ defmacro defn [&form &env fname & s]
    ;; note: cannot delegate this check to def because of the call to (with-meta name ...)
    (when (symbol? fname) => (throw! "first argument to defn must be a symbol")
        (let [m (if (map?    (first s)) (first s) {})
              s (if (map?    (first s)) (next s)   s)
              s (if (vector? (first s)) (list s)   s)
              m (conj {:arglists (list 'quote (sigs s))} m)
              m (let [inline (:inline m) ifn (first inline) iname (second inline)]
                    (when (and (= 'fn ifn) (not (symbol? iname))) => m
                        ;; inserts the same fn name to the inline fn if it does not have one
                        (assoc m :inline (cons ifn (cons (symbol (str (:name fname) "__inliner")) (next inline))))
                    )
                )
              m (conj (or (meta fname) {}) m)]
            (list 'def (with-meta fname m)
                ;; todo - restore propagation of fn name
                ;; must figure out how to convey primitive hints to self calls first
                (with-meta (cons `fn s) {:rettag (:tag m)})
            )
        )
    )
)

;;;
 ; Constructs an array-map.
 ; If any keys are equal, they are handled as if by repeated uses of assoc.
 ;;
(defn array-map
    ([] PersistentArrayMap'EMPTY)
    ([& keyvals] (PersistentArrayMap'createAsIfByAssoc (to-array keyvals)))
)

;;;
 ; keyval => key val
 ; Returns a new hash map with supplied mappings.
 ; If any keys are equal, they are handled as if by repeated uses of assoc.
 ;;
(§ defn hash-map
    ([] {})
    ([& keyvals] (PersistentHashMap'create keyvals))
)

;;;
 ; Returns a new hash set with supplied keys.
 ; Any equal keys are handled as if by repeated uses of conj.
 ;;
(§ defn hash-set
    ([] #{})
    ([& keys] (PersistentHashSet'create keys))
)

;;;
 ; keyval => key val
 ; Returns a new sorted map with supplied mappings.
 ; If any keys are equal, they are handled as if by repeated uses of assoc.
 ;;
(defn sorted-map [& keyvals] (PersistentTreeMap'create keyvals))

;;;
 ; keyval => key val
 ; Returns a new sorted map with supplied mappings, using the supplied comparator.
 ; If any keys are equal, they are handled as if by repeated uses of assoc.
 ;;
(defn sorted-map-by [cmp & keyvals] (PersistentTreeMap'create cmp keyvals))

;;;
 ; Returns a new sorted set with supplied keys.
 ; Any equal keys are handled as if by repeated uses of conj.
 ;;
(defn sorted-set [& keys] (PersistentTreeSet'create keys))

;;;
 ; Returns a new sorted set with supplied keys, using the supplied comparator.
 ; Any equal keys are handled as if by repeated uses of conj.
 ;;
(defn sorted-set-by [cmp & keys] (PersistentTreeSet'create cmp keys))

;;;
 ; Like defn, but the resulting function name is declared as a macro
 ; and will be used as a macro by the compiler when it is called.
 ;;
(§ defmacro defmacro [&form &env name & args]
    (let [[m s] (split-with map? args) s (if (vector? (first s)) (list s) s)
          s (map (fn [bindings & body] (cons (apply vector '&form '&env bindings) body)) s)]
        `(do (defn ~name ~@m ~@s) (Var''setMacro (var ~name)) (var ~name))
    )
)

;;;
 ; Returns a Symbol with the given namespace and name.
 ;;
(§ defn #_"Symbol" symbol
    ([name] (if (symbol? name) name (Symbol'intern name)))
    ([ns name] (Symbol'intern ns name))
)

;;;
 ; Returns a Keyword with the given namespace and name.
 ; Do not use ":" in the keyword strings, it will be added automatically.
 ;;
(defn #_"Keyword" keyword
    ([name]
        (cond
            (keyword? name) name
            (symbol? name) (Keyword'intern #_"Symbol" name)
            (string? name) (Keyword'intern (symbol #_"String" name))
        )
    )
    ([ns name] (Keyword'intern (symbol ns name)))
)

;;;
 ; Returns a Keyword with the given namespace and name if one already exists.
 ; This function will not intern a new keyword. If the keyword has not already
 ; been interned, it will return nil.
 ; Do not use ":" in the keyword strings, it will be added automatically.
 ;;
(defn #_"Keyword" find-keyword
    ([name]
        (cond
            (keyword? name) name
            (symbol? name) (Keyword'find #_"Symbol" name)
            (string? name) (Keyword'find (symbol #_"String" name))
        )
    )
    ([ns name] (Keyword'find (symbol ns name)))
)

;;;
 ; Takes a body of expressions that returns an ISeq or nil, and yields
 ; a Seqable object that will invoke the body only the first time seq
 ; is called, and will cache the result and return it on all subsequent
 ; seq calls. See also - realized?
 ;;
(refer! - lazy-seq)
(ß defmacro lazy-seq [& body] `(LazySeq'new (^{:once true} fn* [] ~@body)))

;;;
 ; Returns a lazy seq representing the concatenation of the elements in the supplied colls.
 ;;
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

;;;
 ; Takes a body of expressions and yields a Delay object that will invoke
 ; the body only the first time it is forced (with force or deref/@), and
 ; will cache the result and return it on all subsequent force calls.
 ; See also - realized?
 ;;
(defmacro delay [& body] `(Delay'new (^{:once true} fn* [] ~@body)))

;;;
 ; Returns true if x is a Delay created with delay.
 ;;
(defn delay? [x] (satisfies? Delay x))

;;;
 ; If x is a Delay, returns the (possibly cached) value of its expression, else returns x.
 ;;
(defn force [x] (Delay'force x))

;;;
 ; Coerce to boolean/int/long.
 ;;
(§ defn boolean [x] (RT'booleanCast x))
(§ defn int     [x] (RT'intCast     x))
(§ defn long    [x] (RT'longCast    x))

;;;
 ; Returns a seq of the items in coll in reverse order. Not lazy.
 ;;
(defn reverse [s] (into () s))

;;;
 ; Takes a fn f and returns a fn that takes the same arguments as f,
 ; has the same effects, if any, and returns the opposite truth value.
 ;;
(defn complement [f]
    (fn
        ([] (not (f)))
        ([x] (not (f x)))
        ([x y] (not (f x y)))
        ([x y & s] (not (apply f x y s)))
    )
)

;;;
 ; disj[oin]. Returns a new set of the same (hashed/sorted) type,
 ; that does not contain key(s).
 ;;
(defn disj
    ([s] s)
    ([#_"IPersistentSet" s k]
        (when s
            (IPersistentSet'''disj s, k)
        )
    )
    ([s k & ks]
        (when s
            (let [s (disj s k)]
                (recur-when ks [s (first ks) (next ks)] => s)
            )
        )
    )
)

;;;
 ; Returns a map containing only those entries in m whose key is in keys.
 ;;
(defn select-keys [m keys] (with-meta (into {} (map #(find m %) keys)) (meta m)))

;;;
 ; WARNING: This is a low-level function.
 ; Prefer high-level macros like binding where ever possible.
 ;
 ; Takes a map of Var/value pairs. Binds each Var to the associated value for
 ; the current thread. Each call *MUST* be accompanied by a matching call to
 ; pop-thread-bindings wrapped in a try-finally!
 ;
 ; (push-thread-bindings bindings)
 ; (try
 ; ...
 ; (finally
 ; (pop-thread-bindings)))
 ;;
(defn push-thread-bindings [bindings] (Var'pushThreadBindings bindings))

;;;
 ; Pop one set of bindings pushed with push-binding before.
 ; It is an error to pop bindings without pushing before.
 ;;
(defn pop-thread-bindings [] (Var'popThreadBindings))

;;;
 ; Get a map with the Var/value pairs which is currently in effect for the current thread.
 ;;
(defn get-thread-bindings [] (Var'getThreadBindings))

;;;
 ; binding => var-symbol init-expr
 ;
 ; Creates new bindings for the (already-existing) vars, with the
 ; supplied initial values, executes the exprs in an implicit do, then
 ; re-establishes the bindings that existed before. The new bindings
 ; are made in parallel (unlike let); all init-exprs are evaluated
 ; before the vars are bound to their new values.
 ;;
(§ defmacro binding [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    (letfn [(var-ize [var-vals]
                (loop-when-recur [v [] s (seq var-vals)] s [(conj v `(var ~(first s)) (second s)) (next (next s))] => (seq v))
            )]
        `(do
            (push-thread-bindings (hash-map ~@(var-ize bindings)))
            (try
                ~@body
                (finally
                    (pop-thread-bindings)
                )
            )
        )
    )
)

;;;
 ; Takes a map of Var/value pairs. Installs for the given Vars the associated
 ; values as thread-local bindings. Then calls f with the supplied arguments.
 ; Pops the installed bindings after f returned. Returns whatever f returns.
 ;;
(defn with-bindings* [binding-map f & args]
    (push-thread-bindings binding-map)
    (try
        (apply f args)
        (finally
            (pop-thread-bindings)
        )
    )
)

;;;
 ; Takes a map of Var/value pairs. Installs for the given Vars the associated
 ; values as thread-local bindings. Then executes body. Pops the installed
 ; bindings after body was evaluated. Returns the value of body.
 ;;
(defmacro with-bindings [binding-map & body]
    `(with-bindings* ~binding-map (fn [] ~@body))
)

;;;
 ; Returns a function, which will install the same bindings in effect as in
 ; the thread at the time bound-fn* was called and then call f with any given
 ; arguments. This may be used to define a helper function which runs on a
 ; different thread, but needs the same bindings in place.
 ;;
(defn- bound-fn* [f]
    (let [bindings (get-thread-bindings)]
        (fn [& args] (apply with-bindings* bindings f args))
    )
)

;;;
 ; Returns a function defined by the given tail, which will install the
 ; same bindings in effect as in the thread at the time bound-fn was called.
 ; This may be used to define a helper function which runs on a different
 ; thread, but needs the same bindings in place.
 ;;
(defmacro bound-fn [& tail] `(bound-fn* (fn ~@tail)))

;;;
 ; Returns the global var named by the namespace-qualified symbol,
 ; or nil if no var with that name.
 ;;
(defn find-var [sym] (Var'find sym))

;;;
 ; Takes a set of functions and returns a fn that is the composition
 ; of those fns. The returned fn takes a variable number of args,
 ; applies the rightmost of fns to the args, the next
 ; fn (right-to-left) to the result, etc.
 ;;
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

;;;
 ; Takes a set of functions and returns a fn that is the juxtaposition
 ; of those fns. The returned fn takes a variable number of args, and
 ; returns a vector containing the result of applying each fn to the
 ; args (left-to-right).
 ; ((juxt a b c) x) => [(a x) (b x) (c x)]
 ;;
(defn juxt
    ([f]
        (fn
            ([] [(f)])
            ([x] [(f x)])
            ([x y] [(f x y)])
            ([x y & z] [(apply f x y z)])
        )
    )
    ([f g]
        (fn
            ([] [(f) (g)])
            ([x] [(f x) (g x)])
            ([x y] [(f x y) (g x y)])
            ([x y & z] [(apply f x y z) (apply g x y z)])
        )
    )
    ([f g h]
        (fn
            ([] [(f) (g) (h)])
            ([x] [(f x) (g x) (h x)])
            ([x y] [(f x y) (g x y) (h x y)])
            ([x y & z] [(apply f x y z) (apply g x y z) (apply h x y z)])
        )
    )
    ([f g h & fs]
        (let [fs (list* f g h fs)]
            (fn
                ([] (reduce #(conj %1 (%2)) [] fs))
                ([x] (reduce #(conj %1 (%2 x)) [] fs))
                ([x y] (reduce #(conj %1 (%2 x y)) [] fs))
                ([x y & z] (reduce #(conj %1 (apply %2 x y z)) [] fs))
            )
        )
    )
)

;;;
 ; Takes a function f and fewer than the normal arguments to f, and
 ; returns a fn that takes a variable number of additional args. When
 ; called, the returned function calls f with args + additional args.
 ;;
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

;;;
 ; Returns true if (f? x) is logical true for every x in coll, else false.
 ;;
(defn #_"Boolean" every? [f? s]
    (cond
        (nil? (seq s)) true
        (f? (first s)) (recur f? (next s))
        :else false
    )
)

;;;
 ; Returns false if (f? x) is logical true for every x in coll, else true.
 ;;
(def #_"Boolean" not-every? (comp not every?))

(defn index-of [s x]
    (loop-when [i 0 s (seq s)] (some? s) => -1
        (when-not (= (first s) x) => i
            (recur (inc i) (next s))
        )
    )
)

;;;
 ; Returns the first logical true value of (f? x) for any x in coll,
 ; else nil. One common idiom is to use a set as f?, for example
 ; this will return :fred if :fred is in the sequence, otherwise nil:
 ; (some #{:fred} coll).
 ;;
(defn some [f? s]
    (when (seq s)
        (or (f? (first s)) (recur f? (next s)))
    )
)

;;;
 ; Returns false if (f? x) is logical true for any x in coll, else true.
 ;;
(def #_"Boolean" not-any? (comp not some))

;;;
 ; Returns a lazy sequence consisting of the result of applying f to
 ; the set of first items of each coll, followed by applying f to the
 ; set of second items in each coll, until any one of the colls is
 ; exhausted. Any remaining items in other colls are ignored. Function
 ; f should accept number-of-colls arguments. Returns a transducer when
 ; no collection is provided.
 ;;
(§ defn map
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

(declare cat)

;;;
 ; Returns the result of applying concat to the result of applying map to f and colls.
 ; Thus function f should return a collection.
 ; Returns a transducer when no collections are provided.
 ;;
(defn mapcat
    ([f] (comp (map f) cat))
    ([f & s] (apply concat (apply map f s)))
)

;;;
 ; Returns a lazy sequence of the items in coll for which (f? item) returns logical true.
 ; f? must be free of side-effects.
 ; Returns a transducer when no collection is provided.
 ;;
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

;;;
 ; Returns a lazy sequence of the items in coll for which (f? item) returns logical false.
 ; f? must be free of side-effects.
 ; Returns a transducer when no collection is provided.
 ;;
(defn remove
    ([f?]   (filter (complement f?)  ))
    ([f? s] (filter (complement f?) s))
)

;;;
 ; Returns a lazy sequence of the first n items in coll, or all items if there are fewer than n.
 ; Returns a stateful transducer when no collection is provided.
 ;;
(defn take
    ([n]
        (fn [g]
            (let [n' (atom n)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (let [n @n' m (swap! n' dec) s (if (pos? n) (g s x) s)]
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

;;;
 ; Returns a lazy sequence of successive items from coll while (f? item) returns logical true.
 ; f? must be free of side-effects.
 ; Returns a transducer when no collection is provided.
 ;;
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

;;;
 ; Returns a lazy sequence of all but the first n items in coll.
 ; Returns a stateful transducer when no collection is provided.
 ;;
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

;;;
 ; Return a lazy sequence of all but the last n (default 1) items in coll.
 ;;
(defn drop-last
    ([s] (drop-last 1 s))
    ([n s] (map (fn [x _] x) s (drop n s)))
)

;;;
 ; Returns a seq of the last n items in coll. Depending on the type of coll
 ; may be no better than linear time. For vectors, see also subvec.
 ;;
(defn take-last [n coll]
    (loop-when-recur [s (seq coll) z (seq (drop n coll))] z [(next s) (next z)] => s)
)

;;;
 ; Returns a lazy sequence of the items in coll starting from the
 ; first item for which (f? item) returns logical false.
 ; Returns a stateful transducer when no collection is provided.
 ;;
(defn drop-while
    ([f?]
        (fn [g]
            (let [drop? (atom true)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (when-not (and @drop? (f? x)) => s
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

;;;
 ; Returns a vector of [(take n coll) (drop n coll)].
 ;;
(defn split-at [n s] [(take n s) (drop n s)])

;;;
 ; Returns a vector of [(take-while f? coll) (drop-while f? coll)].
 ;;
(defn split-with [f? s] [(take-while f? s) (drop-while f? s)])

;;;
 ; Returns a lazy (infinite!, or length n if supplied) sequence of xs.
 ;;
(defn repeat
    ([  x] (Repeat'create   x))
    ([n x] (Repeat'create n x))
)

;;;
 ; Returns a lazy sequence of x, (f x), (f (f x)), etc.
 ; f must be free of side-effects.
 ;;
(defn iterate [f x] (Iterate'create f x))

;;;
 ; Returns a lazy seq of nums from start (inclusive) to end (exclusive),
 ; by step, where start defaults to 0, step to 1, and end to infinity.
 ; When step is equal to 0, returns an infinite sequence of start.
 ; When start is equal to end, returns empty list.
 ;;
(defn range
    ([] (iterate inc 0))
    ([over] (Range'create over))
    ([from over] (Range'create from over))
    ([from over step] (Range'create from over step))
)

;;;
 ; Returns a map that consists of the rest of the maps conj-ed onto
 ; the first. If a key occurs in more than one map, the mapping from
 ; the latter (left-to-right) will be the mapping in the result.
 ;;
(§ defn merge [& maps]
    (when (some identity maps)
        (reduce #(conj (or %1 {}) %2) maps)
    )
)

;;;
 ; Returns a map that consists of the rest of the maps conj-ed onto
 ; the first. If a key occurs in more than one map, the mapping(s)
 ; from the latter (left-to-right) will be combined with the mapping in
 ; the result by calling (f val-in-result val-in-latter).
 ;;
(defn merge-with [f & maps]
    (when (some identity maps)
        (letfn [(merge- [m e]
                    (let [k (key e) v (val e)]
                        (assoc m k (if (contains? m k) (f (get m k) v) v))
                    )
                )]
            (reduce #(reduce merge- (or %1 {}) %2) maps)
        )
    )
)

;;;
 ; Returns a map with the keys mapped to the corresponding vals.
 ;;
(defn zipmap [keys vals]
    (loop-when-recur [m (transient {}) ks (seq keys) vs (seq vals)]
                     (and ks vs)
                     [(assoc! m (first ks) (first vs)) (next ks) (next vs)]
                  => (persistent! m)
    )
)

;;;
 ; Returns the lines of text from r as a lazy sequence of strings.
 ; r must implement java.io.BufferedReader.
 ;;
(defn line-seq [#_"BufferedReader" r]
    (when-some [line (.readLine r)]
        (cons line (lazy-seq (line-seq r)))
    )
)

;;;
 ; Returns an implementation of java.util.Comparator based upon f?.
 ;;
(defn comparator [f?]
    (fn [x y]
        (cond (f? x y) -1 (f? y x) 1 :else 0)
    )
)

;;;
 ; Returns a sorted sequence of the items in coll.
 ; If no comparator is supplied, uses compare. comparator must implement java.util.Comparator.
 ; Guaranteed to be stable: equal elements will not be reordered.
 ; If coll is a Java array, it will be modified. To avoid this, sort a copy of the array.
 ;;
(defn sort
    ([s] (sort compare s))
    ([#_"Comparator" cmp s]
        (when (seq s) => ()
            (let [a (to-array s)]
                (Arrays/sort a cmp)
                (seq a)
            )
        )
    )
)

;;;
 ; Returns a sorted sequence of the items in coll, where the sort order is determined by comparing (keyfn item).
 ; If no comparator is supplied, uses compare. comparator must implement java.util.Comparator.
 ; Guaranteed to be stable: equal elements will not be reordered.
 ; If coll is a Java array, it will be modified. To avoid this, sort a copy of the array.
 ;;
(defn sort-by
    ([f s] (sort-by f compare s))
    ([f #_"Comparator" cmp s] (sort #(.compare cmp (f %1) (f %2)) s))
)

;;;
 ; When lazy sequences are produced via functions that have side
 ; effects, any effects other than those needed to produce the first
 ; element in the seq do not occur until the seq is consumed. dorun can
 ; be used to force any effects. Walks through the successive nexts of
 ; the seq, does not retain the head and returns nil.
 ;;
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

;;;
 ; When lazy sequences are produced via functions that have side
 ; effects, any effects other than those needed to produce the first
 ; element in the seq do not occur until the seq is consumed. doall can
 ; be used to force any effects. Walks through the successive nexts of
 ; the seq, retains the head and returns it, thus causing the entire
 ; seq to reside in memory at one time.
 ;;
(defn doall
    ([s] (dorun s) s)
    ([n s] (dorun n s) s)
)

;;;
 ; Returns a lazy sequence of lists of n items each, at offsets step apart.
 ; If step is not supplied, defaults to n, i.e. the partitions do not overlap.
 ; If a pad is supplied, use it as necessary to complete the last partition upto n items.
 ; In case there are not enough padding elements, return a partition with less than n items.
 ;;
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

;;;
 ; Returns a lazy sequence of lists like partition, but may include
 ; partitions with fewer than n items at the end. Returns a stateful
 ; transducer when no collection is provided.
 ;;
(defn partition-all
    ([n]
        (fn [g]
            (let [v' (atom [])]
                (fn
                    ([] (g))
                    ([x]
                        (let [x (when (seq @v') => x
                                    (let [v @v' _ (swap! v' empty)]
                                        (unreduced (g x v))
                                    )
                                )]
                            (g x)
                        )
                    )
                    ([x y]
                        (swap! v' conj y)
                        (when (= (count @v') n) => x
                            (let [v @v' _ (swap! v' empty)]
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

;;;
 ; Returns a new, transient version of the collection, in constant time.
 ;;
(defn transient [#_"IEditableCollection" coll] (IEditableCollection'''asTransient coll))

;;;
 ; Returns a new, persistent version of the transient collection, in
 ; constant time. The transient collection cannot be used after this
 ; call, any such use will throw an exception.
 ;;
(defn persistent! [#_"ITransientCollection" coll] (ITransientCollection'''persistent! coll))

;;;
 ; Adds x to the transient collection, and return coll. The 'addition'
 ; may happen at different 'places' depending on the concrete type.
 ;;
(defn conj!
    ([] (transient []))
    ([coll] coll)
    ([#_"ITransientCollection" coll x] (ITransientCollection'''conj! coll, x))
)

;;;
 ; When applied to a transient map, adds mapping of key(s) to val(s).
 ; When applied to a transient vector, sets the val at index.
 ; Note - index must be <= (count vector). Returns coll.
 ;;
(defn assoc!
    ([#_"ITransientAssociative" a k v] (ITransientAssociative'''assoc! a, k, v))
    ([a k v & kvs]
        (let [a (assoc! a k v)]
            (recur-when kvs [a (first kvs) (second kvs) (next (next kvs))] => a)
        )
    )
)

;;;
 ; Returns a transient map that doesn't contain a mapping for key(s).
 ;;
(defn dissoc!
    ([#_"ITransientMap" m k] (ITransientMap'''dissoc! m, k))
    ([m k & ks]
        (let [m (dissoc! m k)]
            (recur-when ks [m (first ks) (next ks)] => m)
        )
    )
)

;;;
 ; Removes the last item from a transient vector.
 ; If the collection is empty, throws an exception. Returns coll.
 ;;
(defn pop! [#_"ITransientVector" coll] (ITransientVector'''pop! coll))

;;;
 ; disj[oin].
 ; Returns a transient set of the same (hashed/sorted) type, that does not contain key(s).
 ;;
(defn disj!
    ([s] s)
    ([#_"ITransientSet" s k] (ITransientSet'''disj! s, k))
    ([s k & ks]
        (let [s (disj! s k)]
            (recur-when ks [s (first ks) (next ks)] => s)
        )
    )
)

;;;
 ; import-list => (package-symbol class-name-symbols*)
 ;
 ; For each name in class-name-symbols, adds a mapping from name to the class named by package.name
 ; to the current namespace. Use :import in the ns macro in preference to calling this directly.
 ;;
(defmacro import [& s]
    `(do
        ~@(map #(list 'import* %)
            (reduce
                (fn [v s]
                    (if (symbol? s)
                        (conj v (name s))
                        (into v (map #(str (first s) "." %) (next s)))
                    )
                )
                [] (map #(if (and (seq? %) (= 'quote (first %))) (second %) %) s)
            )
        )
    )
)

;;;
 ; Returns an array with components set to the values in aseq.
 ; The array's component type is type if provided, or the type of the first value in aseq if present, or Object.
 ; All values in aseq must be compatible with the component type.
 ;;
(defn into-array
    ([s] (into-array nil s))
    ([type s] (RT'seqToTypedArray type (seq s)))
)

(defn array [& s] (into-array s))

;;;
 ; Returns the :type metadata of x, or its Class if none.
 ;;
(defn type [x] (or (:type (meta x)) (class x)))

;;;
 ; Returns true if n is a Ratio.
 ;;
(defn ratio? [n] (satisfies? Ratio n))

;;;
 ; Returns true if n is a rational number.
 ;;
(defn rational? [n] (or (integer? n) (ratio? n)))

;;;
 ; Coerce to BigInteger.
 ;;
(defn #_"BigInteger" biginteger [x]
    (cond
        (instance? BigInteger x) x
        (ratio? x)               (Ratio''bigIntegerValue #_"Ratio" x)
        (number? x)              (BigInteger/valueOf (long x))
        :else                    (BigInteger. x)
    )
)

;;;
 ; Reads the next object from stream, which must be an instance of
 ; java.io.PushbackReader or some derivee. stream defaults to the
 ; current value of *in*.
 ;
 ; Opts is a persistent map with valid keys:
 ;
 ; :eof - on eof, return value unless :eofthrow, then throw.
 ;        if not specified, will throw.
 ;;
(defn read
    ([] (read *in*))
    ([s] (read s true nil))
    ([s eof-error? eof-value] (LispReader'read s (boolean eof-error?) eof-value))
)

;;;
 ; Reads one object from the string s.
 ;;
(defn read-string [s] (RT'readString s))

;;;
 ; If form represents a macro form, returns its expansion, else returns form.
 ;;
(defn macroexpand-1 [form] (Compiler'macroexpand1 form))

;;;
 ; Repeatedly calls macroexpand-1 on form until it no longer
 ; represents a macro form, then returns it. Note neither
 ; macroexpand-1 nor macroexpand expand macros in subforms.
 ;;
(defn macroexpand [form]
    (let-when [e (macroexpand-1 form)] (identical? e form) => (recur e)
        form
    )
)

;;;
 ; Sequentially read and evaluate the set of forms contained in the stream.
 ;;
(defn load-reader [r] (Compiler'load r))

;;;
 ; Sequentially read and evaluate the set of forms contained in the string.
 ;;
(defn load-string [s] (load-reader (-> s (java.io.StringReader.) (PushbackReader.))))

;;;
 ; Returns a set of the distinct elements of coll.
 ;;
(defn set [s] (if (set? s) (with-meta s nil) (into #{} s)))

(defn- filter-key [f f? m]
    (loop-when-recur [s (seq m) m (transient {})]
                     s
                     [(next s) (let [e (first s)] (if (f? (f e)) (assoc m (key e) (val e)) m))]
                  => (persistent! m)
    )
)

;;;
 ; Returns the namespace named by the symbol or nil if it doesn't exist.
 ;;
(defn find-ns [sym] (Namespace'find sym))

;;;
 ; Create a new namespace named by the symbol if one doesn't already exist,
 ; returns it or the already-existing namespace of the same name.
 ;;
(defn create-ns [sym] (Namespace'findOrCreate sym))

;;;
 ; Removes the namespace named by the symbol. Use with caution.
 ; Cannot be used to remove the arbace namespace.
 ;;
(defn remove-ns [sym] (Namespace'remove sym))

;;;
 ; Returns a sequence of all namespaces.
 ;;
(defn all-ns [] (Namespace'all))

;;;
 ; If passed a namespace, returns it. Else, when passed a symbol,
 ; returns the namespace named by it, throwing an exception if not found.
 ;;
(§ defn #_"Namespace" the-ns [x]
    (if (satisfies? Namespace x)
        x
        (or (find-ns x) (throw! (str "no namespace: " x " found")))
    )
)

;;;
 ; Returns the name of the namespace, a symbol.
 ;;
(defn ns-name [ns] (:name (the-ns ns)))

;;;
 ; Returns a map of all the mappings for the namespace.
 ;;
(defn ns-map [ns] (Namespace''getMappings (the-ns ns)))

;;;
 ; Removes the mappings for the symbol from the namespace.
 ;;
(defn ns-unmap [ns sym] (Namespace''unmap (the-ns ns) sym))

;;;
 ; Returns a map of the intern mappings for the namespace.
 ;;
(defn ns-interns [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"Var" v] (and (var? v) (= ns (:ns v)))) (ns-map ns))
    )
)

;;;
 ; Returns a map of the public intern mappings for the namespace.
 ;;
(defn ns-publics [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"Var" v] (and (var? v) (= ns (:ns v)) (Var''isPublic v))) (ns-map ns))
    )
)

;;;
 ; Returns a map of the import mappings for the namespace.
 ;;
(defn ns-imports [ns]
    (filter-key val class? (ns-map ns))
)

;;;
 ; refers to all public vars of ns, subject to filters.
 ; filters can include at most one each of:
 ;
 ; :exclude list-of-symbols
 ; :only    list-of-symbols
 ; :rename  map-of-fromsymbol-tosymbol
 ;
 ; For each public interned var in the namespace named by the symbol, adds a mapping
 ; from the name of the var to the var to the current namespace. Throws an exception
 ; if name is already mapped to something else in the current namespace. Filters can
 ; be used to select a subset, via inclusion or exclusion, or to provide a mapping
 ; to a symbol different from the var's name, in order to prevent clashes.
 ;;
(defn refer [ns-sym & filters]
    (let [ns (the-ns ns-sym) ps* (ns-publics ns) fs* (apply hash-map filters)
          r (:refer fs*) s (if (= r :all) (keys ps*) (or r (:only fs*) (keys ps*)))]
        (when (sequential? s) => (throw! "the value of :only/:refer must be a sequential collection of symbols")
            (let [es* (set (:exclude fs*)) rs* (or (:rename fs*) {})]
                (doseq [x (remove es* s)]
                    (let-when [v (ps* x)] (some? v) => (throw! (str x (if (get (ns-interns ns) x) " is not public" " does not exist")))
                        (Namespace''refer *ns* (or (rs* x) x) v)
                    )
                )
            )
        )
    )
)

(defmacro refer-arbace [& filters]
    `(refer '~'arbace.core ~@filters)
)

;;;
 ; Returns a map of the refer mappings for the namespace.
 ;;
(defn ns-refers [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"Var" v] (and (var? v) (not= ns (:ns v)))) (ns-map ns))
    )
)

;;;
 ; Add an alias in the current namespace to another namespace.
 ; Arguments are two symbols: the alias to be used, and [the symbolic name of] the target namespace.
 ; Use :as in the ns macro in preference to calling this directly.
 ;;
(defn alias [sym ns]
    (Namespace''addAlias *ns* sym (the-ns ns))
)

;;;
 ; Returns a map of the aliases for the namespace.
 ;;
(defn ns-aliases [ns]
    (Namespace''getAliases (the-ns ns))
)

;;;
 ; Removes the alias for the symbol from the namespace.
 ;;
(defn ns-unalias [ns sym]
    (Namespace''removeAlias (the-ns ns) sym)
)

;;;
 ; Returns a lazy seq of every nth item in coll.
 ; Returns a stateful transducer when no collection is provided.
 ;;
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

;;;
 ; Returns a lazy seq of the first item in each coll, then the second, etc.
 ;;
(defn interleave
    ([] ())
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

;;;
 ; Gets the value in the var object.
 ;;
(defn var-get [#_"Var" x] (Var''get x))

;;;
 ; Sets the value in the var object to val.
 ; The var must be thread-locally bound.
 ;;
(defn var-set [#_"Var" x val] (Var''set x val))

;;;
 ; varbinding => symbol init-expr
 ;
 ; Executes the exprs in a context in which the symbols are bound to vars
 ; with per-thread bindings to the init-exprs. The symbols refer to the
 ; var objects themselves, and must be accessed with var-get and var-set.
 ;;
(defmacro with-local-vars [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    `(let [~@(interleave (take-nth 2 bindings) (repeat '(Var'new nil, nil)))]
        (push-thread-bindings (hash-map ~@bindings))
        (try
            ~@body
            (finally
                (pop-thread-bindings)
            )
        )
    )
)

;;;
 ; Returns the var or Class to which a symbol will be resolved in the namespace
 ; (unless found in the environment), else nil. Note that if the symbol is fully qualified,
 ; the var/Class to which it resolves need not be present in the namespace.
 ;;
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

;; redefine let and loop with destructuring

(defn destructure [bindings]
    (letfn [(vec- [v x y]
                (let [v' (gensym "v__") s' (gensym "s__") f' (gensym "f__") amp (some #{'&} x)]
                    (loop-when [v (let [v (conj v v' y)] (if amp (conj v s' `(seq ~v')) v)) n 0 s (seq x) amp? false] s => v
                        (case (first s)
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
                (let [m' (gensym "m__") ms' (with-meta m' {:tag 'arbace.core.ISeq}) as (:as x) or* (:or x)
                      v (conj v m' y m' `(if (seq? ~m') (apply hash-map ~ms') ~m')) v (if as (conj v as m') v)
                      s (reduce
                            (fn [m e] (reduce #(assoc %1 %2 ((val e) %2)) (dissoc m (key e)) ((key e) m)))
                            (dissoc x :as :or)
                            (reduce
                                (fn [m k]
                                    (when (keyword? k) => m
                                        (let [ns (namespace k)]
                                            (case (name k)
                                                "keys" (assoc m k #(keyword (or ns (namespace %)) (name %)))
                                                "syms" (assoc m k #(list 'quote (symbol (or ns (namespace %)) (name %))))
                                                "strs" (assoc m k str)
                                                       m
                                            )
                                        )
                                    )
                                )
                                {} (keys x)
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
                (reduce #(destructure- %1 (first %2) (second %2)) [] pairs)
            )
        )
    )
)

;;;
 ; binding => binding-form init-expr
 ;
 ; Evaluates the exprs in a lexical context in which the symbols in the
 ; binding-forms are bound to their respective init-exprs or parts therein.
 ;;
(§ defmacro let [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    `(let* ~(destructure bindings) ~@body)
)

(defn- maybe-destructured [pars body]
    (if (every? symbol? pars)
        (cons (vec pars) body)
        (loop-when [s (seq pars) pars (with-meta [] (meta pars)) lets []] s => `(~pars (let ~lets ~@body))
            (if (symbol? (first s))
                (recur (next s) (conj pars (first s)) lets)
                (let [p' (gensym "p__")]
                    (recur (next s) (conj pars p') (conj lets (first s) p'))
                )
            )
        )
    )
)

;; redefine fn with destructuring

;;;
 ; params => positional-params*, or positional-params* & next-param
 ; positional-param => binding-form
 ; next-param => binding-form
 ; name => symbol
 ;
 ; Defines a function.
 ;;
(§ defmacro fn [& s]
    (let [name (when (symbol? (first s)) (first s)) s (if name (next s) s)
          s (if (vector? (first s))
                (list s)
                (if (seq? (first s))
                    s
                    ;; assume single arity syntax
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
                ;; ensure correct type before destructuring sig
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

;;;
 ; Evaluates the exprs in a lexical context in which the symbols in
 ; the binding-forms are bound to their respective init-exprs or parts
 ; therein. Acts as a recur target.
 ;;
(§ defmacro loop [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    (if (= (destructure bindings) bindings)
        `(loop* ~bindings ~@body)
        (let [s (take-nth 2 bindings) s' (map #(if (symbol? %) % (gensym)) s)
              v (reduce
                    (fn [v [x y z]] (if (symbol? x) (conj v z y) (conj v z y x z)))
                    [] (map vector s (take-nth 2 (drop 1 bindings)) s')
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

;;;
 ; Expands to code which yields a lazy sequence of the concatenation of
 ; the supplied colls. Each coll expr is not evaluated until it is needed.
 ;
 ; (lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))
 ;;
(defmacro lazy-cat [& s]
    `(concat ~@(map #(list `lazy-seq %) s))
)

;;;
 ; List comprehension.
 ;
 ; Takes a vector of one or more binding-form/collection-expr pairs, each followed
 ; by zero or more modifiers, and yields a lazy sequence of evaluations of expr.
 ; Collections are iterated in a nested fashion, rightmost fastest, and nested
 ; coll-exprs can refer to bindings created in prior binding-forms.
 ; Supported modifiers are: :let [binding-form expr ...], :while test, :when test.
 ;
 ; (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))
 ;;
(defmacro for [bindings body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    (letfn [(group- [bindings]
                (reduce
                    (fn [v [x y]]
                        (if (keyword? x)
                            (conj (pop v) (conj (peek v) [x y]))
                            (conj v [x y])
                        )
                    )
                    [] (partition 2 bindings)
                )
            )
            (emit- [[[x _ & z] & [[_ e] :as more]]]
                (let [f' (gensym "f__") s' (gensym "s__")]
                    (letfn [(mod- [[[k v] & z]]
                                (if (keyword? k)
                                    (case k
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

;;;
 ; Returns an instance of java.util.regex.Pattern, for use, e.g. in re-matcher.
 ;;
(defn #_"Pattern" re-pattern [s] (if (instance? Pattern s) s (Pattern/compile s)))

;;;
 ; Returns an instance of java.util.regex.Matcher, for use, e.g. in re-find.
 ;;
(defn #_"Matcher" re-matcher [#_"Pattern" re s] (.matcher re s))

;;;
 ; Returns the groups from the most recent match/find. If there are no
 ; nested groups, returns a string of the entire match. If there are
 ; nested groups, returns a vector of the groups, the first element
 ; being the entire match.
 ;;
(defn re-groups [#_"Matcher" m]
    (let-when [n (.groupCount m)] (pos? n) => (.group m)
        (into [] (§ soon for [i (range (inc n))] (.group m i)))
    )
)

;;;
 ; Returns a lazy sequence of successive matches of pattern in string,
 ; using java.util.regex.Matcher.find(), each such match processed with
 ; re-groups.
 ;;
(defn re-seq [#_"Pattern" re s]
    (let [m (re-matcher re s)]
        ((fn step []
            (when (.find m)
                (cons (re-groups m) (lazy-seq (step)))
            )
        ))
    )
)

;;;
 ; Returns the match, if any, of string to pattern,
 ; using java.util.regex.Matcher.matches().
 ; Uses re-groups to return the groups.
 ;;
(defn re-matches [#_"Pattern" re s]
    (let-when [m (re-matcher re s)] (.matches m)
        (re-groups m)
    )
)

;;;
 ; Returns the next regex match, if any, of string to pattern,
 ; using java.util.regex.Matcher.find().
 ; Uses re-groups to return the groups.
 ;;
(defn re-find
    ([#_"Matcher" m]
        (when (.find m)
            (re-groups m)
        )
    )
    ([#_"Pattern" re s]
        (let [m (re-matcher re s)]
            (re-find m)
        )
    )
)

;;;
 ; Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
 ; branch? must be a fn of one arg that returns true if passed a node
 ; that can have children (but may not). children must be a fn of one arg
 ; that returns a sequence of the children. Will only be called on nodes
 ; for which branch? returns true. Root is the root node of the tree.
 ;;
(defn tree-seq [branch? children root]
    (letfn [(walk- [node]
                (lazy-seq
                    (cons node (when (branch? node) (mapcat walk- (children node))))
                )
            )]
        (walk- root)
    )
)

;;;
 ; Returns the x for which (k x), a number, is greatest.
 ; If there are multiple such xs, the last one is returned.
 ;;
(defn max-key
    ([k x] x)
    ([k x y] (if (> (k x) (k y)) x y))
    ([k x y & s]
        (let [kx (k x) ky (k y) [v kv] (if (> kx ky) [x kx] [y ky])]
            (loop-when [v v kv kv s s] s => v
                (let [w (first s) kw (k w)]
                    (if (>= kw kv)
                        (recur w kw (next s))
                        (recur v kv (next s))
                    )
                )
            )
        )
    )
)

;;;
 ; Returns the x for which (k x), a number, is least.
 ; If there are multiple such xs, the last one is returned.
 ;;
(defn min-key
    ([k x] x)
    ([k x y] (if (< (k x) (k y)) x y))
    ([k x y & s]
        (let [kx (k x) ky (k y) [v kv] (if (< kx ky) [x kx] [y ky])]
            (loop-when [v v kv kv s s] s => v
                (let [w (first s) kw (k w)]
                    (if (<= kw kv)
                        (recur w kw (next s))
                        (recur v kv (next s))
                    )
                )
            )
        )
    )
)

;;;
 ; Returns a lazy sequence of the elements of coll with duplicates removed.
 ; Returns a stateful transducer when no collection is provided.
 ;;
(defn distinct
    ([]
        (fn [g]
            (let [seen (atom #{})]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (when-not (contains? @seen x) => s
                            (swap! seen conj x)
                            (g s x)
                        )
                    )
                )
            )
        )
    )
    ([s]
        (letfn [(step- [s seen]
                    (lazy-seq
                        ((fn [[x :as s] seen]
                            (when-some [s (seq s)]
                                (when-not (contains? seen x) => (recur (next s) seen)
                                    (cons x (step- (next s) (conj seen x)))
                                )
                            ))
                            s seen
                        )
                    )
                )]
            (step- s #{})
        )
    )
)

;;;
 ; Given a map of replacement pairs and a vector/collection, returns
 ; a vector/seq with any elements = a key in smap replaced with the
 ; corresponding val in smap. Returns a transducer when no collection
 ; is provided.
 ;;
(defn replace
    ([m] (map #(if-some [e (find m %)] (val e) %)))
    ([m s]
        (when (vector? s) => (map #(if-some [e (find m %)] (val e) %) s)
            (reduce
                (fn [v i]
                    (if-some [e (find m (nth v i))]
                        (assoc v i (val e))
                        v
                    )
                )
                s (range (count s))
            )
        )
    )
)

(defn- mk-bound-fn [#_"Sorted" sc test key]
    (fn [e] (test (.compare (Sorted'''comparator sc) (Sorted'''entryKey sc, e) key) 0))
)

;;;
 ; sc must be a sorted collection, test(s) one of <, <=, > or >=.
 ; Returns a seq of those entries with keys ek for which
 ; (test (.. sc comparator (compare ek key)) 0) is true.
 ;;
(defn subseq
    ([#_"Sorted" sc test key]
        (let [keep? (mk-bound-fn sc test key)]
            (if (#{> >=} test)
                (when-some [[e :as s] (Sorted'''seqFrom sc, key, true)]
                    (if (keep? e) s (next s))
                )
                (take-while keep? (Sorted'''seq sc, true))
            )
        )
    )
    ([#_"Sorted" sc test key test' key']
        (when-some [[e :as s] (Sorted'''seqFrom sc, key, true)]
            (take-while (mk-bound-fn sc test' key') (if ((mk-bound-fn sc test key) e) s (next s)))
        )
    )
)

;;;
 ; sc must be a sorted collection, test(s) one of <, <=, > or >=.
 ; Returns a reverse seq of those entries with keys ek for which
 ; (test (.. sc comparator (compare ek key)) 0) is true.
 ;;
(defn rsubseq
    ([#_"Sorted" sc test key]
        (let [keep? (mk-bound-fn sc test key)]
            (if (#{< <=} test)
                (when-some [[e :as s] (Sorted'''seqFrom sc, key, false)]
                    (if (keep? e) s (next s))
                )
                (take-while keep? (Sorted'''seq sc, false))
            )
        )
    )
    ([#_"Sorted" sc test key test' key']
        (when-some [[e :as s] (Sorted'''seqFrom sc, key', false)]
            (take-while (mk-bound-fn sc test key) (if ((mk-bound-fn sc test' key') e) s (next s)))
        )
    )
)

;;;
 ; Takes a function of no args, presumably with side effects, and returns
 ; an infinite (or length n if supplied) lazy sequence of calls to it.
 ;;
(defn repeatedly
    ([f] (lazy-seq (cons (f) (repeatedly f))))
    ([n f] (take n (repeatedly f)))
)

;;;
 ; Returns a lazy seq of the elements of coll separated by sep.
 ; Returns a stateful transducer when no collection is provided.
 ;;
(defn interpose
    ([sep]
        (fn [g]
            (let [started (atom false)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (when @started => (do (reset! started true) (g s x))
                            (let [r (g s sep)]
                                (if (reduced? r) r (g r x))
                            )
                        )
                    )
                )
            )
        )
    )
    ([sep coll] (drop 1 (interleave (repeat sep) coll)))
)

;;;
 ; Experimental - like defmacro, except defines a named function whose
 ; body is the expansion, calls to which may be expanded inline as if
 ; it were a macro. Cannot be used with variadic (&) args.
 ;;
(defmacro definline [name & decl]
    (let [[pre-args [args expr]] (split-with (comp not vector?) decl)]
        `(do
            (defn ~name ~@pre-args ~args ~(apply (eval (list `fn args expr)) args))
            (alter-meta! (var ~name) assoc :inline (fn ~name ~args ~expr))
            (var ~name)
        )
    )
)

;;;
 ; Returns an empty collection of the same category as coll, or nil.
 ;;
(defn empty [coll]
    (when (coll? coll)
        (IPersistentCollection'''empty #_"IPersistentCollection" coll)
    )
)

;;;
 ; If coll is empty, returns nil, else coll.
 ;;
(defn not-empty [coll] (when (seq coll) coll))

;;;
 ; Atomically alters the root binding of var v by applying f to its current value plus any args.
 ;;
(defn alter-var-root [#_"Var" v f & args] (Var''alterRoot v f args))

;;;
 ; Returns true if all of the vars provided as arguments have any bound value, root or thread-local.
 ; Implies that deref'ing the provided vars will succeed. Returns true if no vars are provided.
 ;;
(defn bound? [& vars] (every? #(Var''isBound #_"Var" %) vars))

;;;
 ; Returns true if all of the vars provided as arguments have thread-local bindings.
 ; Implies that set!'ing the provided vars will succeed. Returns true if no vars are provided.
 ;;
(defn thread-bound? [& vars] (every? #(Var''getThreadBinding #_"Var" %) vars))

;;;
 ; Returns the immediate superclass and direct interfaces of c, if any.
 ;;
(defn bases [#_"Class" c]
    (when c
        (let [i (seq (.getInterfaces c)) s (.getSuperclass c)]
            (if s (cons s i) i)
        )
    )
)

;;;
 ; Returns the immediate and indirect superclasses and interfaces of c, if any.
 ;;
(defn supers [#_"Class" c]
    (loop-when [s (set (bases c)) cs s] (seq cs) => (not-empty s)
        (let [c (first cs) bs (bases c)]
            (recur (into s bs) (into (disj cs c) bs))
        )
    )
)

;;;
 ; Returns true if (= child parent), or child is directly or indirectly derived
 ; from parent via Java type inheritance relationship.
 ;;
(defn isa? [child parent]
    (or (= child parent)
        (and (class? parent) (class? child) (.isAssignableFrom #_"Class" parent child))
        (and (vector? parent) (vector? child) (= (count parent) (count child))
            (loop-when-recur [? true i 0] (and ? (< i (count parent))) [(isa? (child i) (parent i)) (inc i)] => ?)
        )
    )
)

;;;
 ; Returns true if no two of the arguments are =.
 ;;
(defn #_"Boolean" distinct?
    ([x] true)
    ([x y] (not (= x y)))
    ([x y & z]
        (and (distinct? x y)
            (loop-when [s* #{x y} z z] z => true
                (and (not (contains? s* (first z)))
                    (recur (conj s* (first z)) (next z))
                )
            )
        )
    )
)

(defmacro- with-loading-context [& body]
    `((fn loading# []
        (binding [*class-loader* (.getClassLoader (.getClass #_"Object" loading#))]
            ~@body
        )
    ))
)

;;;
 ; Sets *ns* to the namespace named by name (unevaluated), creating it if needed.
 ;
 ; references can be zero or more of:
 ; (:refer-arbace ...) (:import ...)
 ; with the syntax of refer-arbace/import respectively,
 ; except the arguments are unevaluated and need not be quoted.
 ;
 ; If :refer-arbace is not used, a default (refer 'arbace.core) is used.
 ; Use of ns is preferred to individual calls to in-ns/import:
 ;
 ; (ns foo.bar
 ;   (:refer-arbace :exclude [format printf])
 ;   (:import (java.util Date Timer Random)
 ;            (java.sql Connection Statement)))
 ;;
(§ defmacro ns [n & s]
    (let [m (let-when [m (first s)] (map? m) m) s (if m (next s) s) n (if m (vary-meta n merge m) n) m (meta n)]
        `(do
            (in-ns '~n)
            ~@(when m
                `((reset-meta! (Namespace'find '~n) ~m))
            )
            (with-loading-context
                ~@(when (and (not= n 'arbace.core) (not-any? #(= :refer-arbace (first %)) s))
                    `((refer '~'arbace.core))
                )
                ~@(map (fn [[k & s]] `(~(symbol "arbace.core" (name k)) ~@(map #(list 'quote %) s))) s)
            )
            nil
        )
    )
)

;;;
 ; defs name to have the root value of the expr iff the named var has no root value,
 ; else expr is unevaluated.
 ;;
(defmacro defonce [name expr]
    `(let-when [v# (def ~name)] (not (Var''hasRoot v#))
        (def ~name ~expr)
    )
)

;;;
 ; Returns the value in a nested associative structure,
 ; where ks is a sequence of keys. Returns nil if the key
 ; is not present, or the not-found value if supplied.
 ;;
(defn get-in
    ([m ks] (reduce get m ks))
    ([m ks not-found]
        (loop-when [o (Object.) m m ks (seq ks)] ks => m
            (let-when [m (get m (first ks) o)] (identical? o m) => (recur o m (next ks))
                not-found
            )
        )
    )
)

;;;
 ; Associates a value in a nested associative structure, where ks is
 ; a sequence of keys and v is the new value and returns a new nested
 ; structure. If any levels do not exist, hash-maps will be created.
 ;;
(defn assoc-in [m [k & ks] v]
    (if ks
        (assoc m k (assoc-in (get m k) ks v))
        (assoc m k v)
    )
)

;;;
 ; 'Updates' a value in a nested associative structure, where ks is
 ; a sequence of keys and f is a function that will take the old value
 ; and any supplied args and return the new value, and returns a new
 ; nested structure. If any levels do not exist, hash-maps will be
 ; created.
 ;;
(defn update-in [m ks f & args]
    (let [[k & ks] ks]
        (if ks
            (assoc m k (apply update-in (get m k) ks f args))
            (assoc m k (apply f (get m k) args))
        )
    )
)

;;;
 ; 'Updates' a value in an associative structure, where k is a key and f is a function
 ; that will take the old value and any supplied args and return the new value, and
 ; returns a new structure. If the key does not exist, nil is passed as the old value.
 ;;
(defn update
    ([m k f] (assoc m k (f (get m k))))
    ([m k f x] (assoc m k (f (get m k) x)))
    ([m k f x y] (assoc m k (f (get m k) x y)))
    ([m k f x y & z] (assoc m k (apply f (get m k) x y z)))
)

;;;
 ; trampoline can be used to convert algorithms requiring mutual recursion without
 ; stack consumption. Calls f with supplied args, if any. If f returns a fn, calls
 ; that fn with no arguments, and continues to repeat, until the return value is
 ; not a fn, then returns that non-fn value. Note that if you want to return a fn
 ; as a final value, you must wrap it in some data structure and unpack it after
 ; trampoline returns.
 ;;
(defn trampoline
    ([f]
        (let-when [r (f)] (fn? r) => r
            (recur r)
        )
    )
    ([f & args] (trampoline #(apply f args)))
)

;;;
 ; Returns a memoized version of a referentially transparent function.
 ; The memoized version of the function keeps a cache of the mapping from
 ; arguments to results and, when calls with the same arguments are repeated
 ; often, has higher performance at the expense of higher memory use.
 ;;
(defn memoize [f]
    (let [mem (atom {})]
        (fn [& args]
            (if-some [e (find @mem args)]
                (val e)
                (let [r (apply f args)]
                    (swap! mem assoc args r)
                    r
                )
            )
        )
    )
)

;;;
 ; Takes a function f, and returns a function that calls f, replacing a nil first argument
 ; to f with the supplied value x. Higher arity versions can replace arguments in the second
 ; and third positions (y, z). Note that the function f can take any number of arguments,
 ; not just the one(s) being nil-patched.
 ;;
(defn fnil
    ([f x]
        (fn
            ([a]               (f (if (nil? a) x a)))
            ([a b]             (f (if (nil? a) x a) b))
            ([a b c]           (f (if (nil? a) x a) b c))
            ([a b c & s] (apply f (if (nil? a) x a) b c s))
        )
    )
    ([f x y]
        (fn
            ([a b]             (f (if (nil? a) x a) (if (nil? b) y b)))
            ([a b c]           (f (if (nil? a) x a) (if (nil? b) y b) c))
            ([a b c & s] (apply f (if (nil? a) x a) (if (nil? b) y b) c s))
        )
    )
    ([f x y z]
        (fn
            ([a b]             (f (if (nil? a) x a) (if (nil? b) y b)))
            ([a b c]           (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)))
            ([a b c & s] (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) s))
        )
    )
)

(defn- shift-mask [shift mask x] (-> x (>> shift) (& mask)))

(def- max-mask-bits 13)
(def- max-switch-table-size (<< 1 max-mask-bits))

;;;
 ; Takes a collection of hashes and returns [shift mask] or nil if none found.
 ;;
(defn- maybe-min-hash [hashes]
    (first
        (filter (fn [[s m]] (apply distinct? (map #(shift-mask s m %) hashes)))
            (§ soon for [mask (map #(dec (<< 1 %)) (range 1 (inc max-mask-bits))) shift (range 0 31)]
                [shift mask]
            )
        )
    )
)

;;;
 ; Transforms a sequence of test constants and a corresponding sequence of then
 ; expressions into a sorted map to be consumed by case*. The form of the map
 ; entries are {(case-f test) [(test-f test) then]}.
 ;;
(defn- case-map [case-f test-f tests thens]
    (into (sorted-map)
        (zipmap
            (map case-f tests)
            (map vector (map test-f tests) thens)
        )
    )
)

;;;
 ; Returns true if the collection of ints can fit within the max-table-switch-size,
 ; false otherwise.
 ;;
(defn- fits-table? [ints]
    (< (- (apply max (seq ints)) (apply min (seq ints))) max-switch-table-size)
)

;;;
 ; Takes a sequence of int-sized test constants and a corresponding sequence of
 ; then expressions. Returns a tuple of [shift mask case-map switch-type] where
 ; case-map is a map of int case values to [test then] tuples, and switch-type
 ; is either :sparse or :compact.
 ;;
(defn- prep-ints [tests thens]
    (if (fits-table? tests)
        ;; compact case ints, no shift-mask
        [0 0 (case-map int int tests thens) :compact]
        (let [[shift mask] (or (maybe-min-hash (map int tests)) [0 0])]
            (if (zero? mask)
                ;; sparse case ints, no shift-mask
                [0 0 (case-map int int tests thens) :sparse]
                ;; compact case ints, with shift-mask
                [shift mask (case-map #(shift-mask shift mask (int %)) int tests thens) :compact]
            )
        )
    )
)

;;;
 ; Takes a case expression, default expression, and a sequence of test constants
 ; and a corresponding sequence of then expressions. Returns a tuple of
 ; [tests thens skip-check-set] where no tests have the same hash. Each set of
 ; input test constants with the same hash is replaced with a single test
 ; constant (the case int), and their respective thens are combined into:
 ;
 ; (condp = expr test-1 then-1 ... test-n then-n default).
 ;
 ; The skip-check is a set of case ints for which post-switch equivalence
 ; checking must not be done (the cases holding the above condp thens).
 ;;
(defn- merge-hash-collisions [expr-sym default tests thens]
    (let [buckets
            (loop-when-recur [m {} ks tests vs thens]
                             (and ks vs)
                             [(update m (f'hash (first ks)) (fnil conj []) [(first ks) (first vs)]) (next ks) (next vs)]
                          => m
            )
          assoc-multi
            (fn [m h bucket] (assoc m h `(condp = ~expr-sym ~@(apply concat bucket) ~default)))
          hmap
            (reduce
                (fn [m [h bucket]]
                    (if (= (count bucket) 1)
                        (assoc m (first (first bucket)) (second (first bucket)))
                        (assoc-multi m h bucket)
                    )
                )
                {} buckets
            )
          skip-check
            (->> buckets
                (filter #(< 1 (count (second %))))
                (map first)
                (into #{})
            )]
        [(keys hmap) (vals hmap) skip-check]
    )
)

;;;
 ; Takes a sequence of test constants and a corresponding sequence of then
 ; expressions. Returns a tuple of [shift mask case-map switch-type skip-check]
 ; where case-map is a map of int case values to [test then] tuples, switch-type
 ; is either :sparse or :compact, and skip-check is a set of case ints for which
 ; post-switch equivalence checking must not be done (occurs with hash collisions).
 ;;
(defn- prep-hashes [expr-sym default tests thens]
    (let [hashes (into #{} (map f'hash tests))]
        (if (= (count tests) (count hashes))
            (if (fits-table? hashes)
                ;; compact case ints, no shift-mask
                [0 0 (case-map f'hash identity tests thens) :compact]
                (let [[shift mask] (or (maybe-min-hash hashes) [0 0])]
                    (if (zero? mask)
                        ;; sparse case ints, no shift-mask
                        [0 0 (case-map f'hash identity tests thens) :sparse]
                        ;; compact case ints, with shift-mask
                        [shift mask (case-map #(shift-mask shift mask (f'hash %)) identity tests thens) :compact]
                    )
                )
            )
            ;; resolve hash collisions and try again
            (let [[tests thens skip-check] (merge-hash-collisions expr-sym default tests thens)
                  [shift mask case-map switch-type] (prep-hashes expr-sym default tests thens)
                  skip-check
                    (if (zero? mask)
                        skip-check
                        (into #{} (map #(shift-mask shift mask %) skip-check))
                    )]
                [shift mask case-map switch-type skip-check]
            )
        )
    )
)

;;;
 ; Takes an expression, and a set of clauses.
 ;
 ; Each clause can take the form of either:
 ;
 ; test-constant result-expr
 ;
 ; (test-constant1 ... test-constantN) result-expr
 ;
 ; The test-constants are not evaluated. They must be compile-time
 ; literals, and need not be quoted. If the expression is equal to a
 ; test-constant, the corresponding result-expr is returned. A single
 ; default expression can follow the clauses, and its value will be
 ; returned if no clause matches. If no default expression is provided
 ; and no clause matches, an IllegalArgumentException is thrown.
 ;
 ; Unlike cond and condp, case does a constant-time dispatch, the
 ; clauses are not considered sequentially. All manner of constant
 ; expressions are acceptable in case, including numbers, strings,
 ; symbols, keywords, and (Arbace) composites thereof. Note that since
 ; lists are used to group multiple constants that map to the same
 ; expression, a vector can be used to match a list if needed. The
 ; test-constants need not be all of the same type.
 ;;
(§ defmacro case [e & clauses]
    (let [e' (with-meta (gensym) {:tag Object})
          default
            (when (odd? (count clauses)) => `(throw! (str "no matching clause: " ~e'))
                (last clauses)
            )]
        (when (<= 2 (count clauses)) => `(let [~e' ~e] ~default)
            (let [pairs (partition 2 clauses)
                  assoc-test
                    (fn [m test expr]
                        (when-not (contains? m test) => (throw! (str "duplicate case test constant: " test))
                            (assoc m test expr)
                        )
                    )
                  pairs
                    (reduce
                        (fn [m [test expr]]
                            (if (seq? test)
                                (reduce #(assoc-test %1 %2 expr) m test)
                                (assoc-test m test expr)
                            )
                        )
                        {} pairs
                    )
                  tests (keys pairs)
                  thens (vals pairs)
                  mode
                    (cond
                        (every? #(and (integer? %) (<= Integer/MIN_VALUE % Integer/MAX_VALUE)) tests) :ints
                        (every? keyword? tests) :identity
                        :else :hashes
                    )]
                (condp = mode
                    :ints
                        (let [[shift mask imap switch-type] (prep-ints tests thens)]
                            `(let [~e' ~e] (case* ~e' ~shift ~mask ~default ~imap ~switch-type :int))
                        )
                    :hashes
                        (let [[shift mask imap switch-type skip-check] (prep-hashes e' default tests thens)]
                            `(let [~e' ~e] (case* ~e' ~shift ~mask ~default ~imap ~switch-type :hash-equiv ~skip-check))
                        )
                    :identity
                        (let [[shift mask imap switch-type skip-check] (prep-hashes e' default tests thens)]
                            `(let [~e' ~e] (case* ~e' ~shift ~mask ~default ~imap ~switch-type :hash-identity ~skip-check))
                        )
                )
            )
        )
    )
)

(defn- print-sequential [#_"String" begin, p, #_"String" sep, #_"String" end, o, #_"Writer" w]
    (.write w begin)
    (when-some [s (seq o)]
        (loop [[x & s] s]
            (p x w)
            (when s
                (.write w sep)
                (recur s)
            )
        )
    )
    (.write w end)
)

(defn- pr-on [o w]
    (-/print-method o w)
    nil
)

(§ defmethod -/print-method ISeq'iface [o, #_"Writer" w]
    (print-sequential "(" pr-on " " ")" o w)
)

(§ defmethod -/print-method IPersistentCollection'iface [o, #_"Writer" w]
    (print-sequential "(" pr-on " " ")" o w)
)

(§ defmethod -/print-method IPersistentVector'iface [o, #_"Writer" w]
    (print-sequential "[" pr-on " " "]" o w)
)

(§ defmethod -/print-method IPersistentMap'iface [o, #_"Writer" w]
    (print-sequential "{" (fn [e #_"Writer" w] (pr-on (key e) w) (.append w \space) (pr-on (val e) w)) ", " "}" o w)
)

(§ defmethod -/print-method IPersistentSet'iface [o, #_"Writer" w]
    (print-sequential "#{" pr-on " " "}" o w)
)

(def- prim->class
     (hash-map
        'boolean Boolean/TYPE   'booleans boolean'array
        'byte    Byte/TYPE      'bytes    byte'array
        'char    Character/TYPE 'chars    char'array
        'int     Integer/TYPE   'ints     int'array
        'long    Long/TYPE      'longs    long'array
        'void    Void/TYPE
    )
)

(defn- #_"Class" the-class [x]
    (cond
        (class? x) x
        (contains? prim->class x) (prim->class x)
        :else (let [s (str x)] (Loader'classForName (if (some #{\. \[} s) s (str "java.lang." s))))
    )
)

;;;
 ; Returns an asm Type object for c, which may be a primitive class (such as Integer/TYPE),
 ; any other class (such as Long), or a fully-qualified class name given as a string or symbol
 ; (such as 'java.lang.String).
 ;;
(defn- #_"Type" asm-type [c]
    (if (or (class? c) (prim->class c))
        (Type/getType (the-class c))
        (let [s (str c)]
            (Type/getObjectType (.replace (if (some #{\. \[} s) s (str "java.lang." s)) "." "/"))
        )
    )
)

(defn- generate-interface [{:keys [iname extends methods]}]
    (when-not (some #(-> % first name (.contains "-")) methods) => (throw! "interface methods must not contain '-'")
        (let [iname (.replace (str iname) "." "/") cv (ClassWriter. ClassWriter/COMPUTE_MAXS)]
            (.visit cv Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE) iname nil "java/lang/Object"
                (when (seq extends)
                    (into-array (map #(.getInternalName (asm-type %)) extends))
                )
            )
            (doseq [[mname pclasses rclass pmetas] methods]
                (let [md (Type/getMethodDescriptor (asm-type rclass) (if pclasses (into-array Type (map asm-type pclasses)) (make-array Type 0)))
                      mv (.visitMethod cv (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) (str mname) md nil nil)]
                    (.visitEnd mv)
                )
            )
            (.visitEnd cv)
            [iname (.toByteArray cv)]
        )
    )
)

;;;
 ; In all subsequent sections taking types, the primitive types can be
 ; referred to by their Java names (int, long, etc.), and classes in the
 ; java.lang package can be used without a package qualifier. All other
 ; classes must be fully qualified.
 ;
 ; Options should be a set of key/value pairs, all except for :name are
 ; optional:
 ;
 ; :name aname
 ;
 ; The package-qualified name of the class to be generated.
 ;
 ; :extends [interface ...]
 ;
 ; One or more interfaces, which will be extended by this interface.
 ;
 ; :methods [ [name [param-types] return-type], ...]
 ;
 ; This parameter is used to specify the signatures of the methods of the
 ; generated interface. Do not repeat superinterface signatures here.
 ;;
(defmacro gen-interface [& options]
    (let [opts (apply hash-map options) name (str (:name opts)) [_ code] (generate-interface opts)]
        (Loader''defineClass *class-loader*, name, code)
    )
)

;;;
 ; Convert a Arbace namespace name to a legal Java package name.
 ;;
(defn- namespace-munge [ns] (.replace (str ns) \- \_))

(defn- parse-opts [s] (loop-when-recur [m {} [k v & s'] s] (keyword? k) [(assoc m k v) s'] => [m s]))

(defn- parse-impls [s] (loop-when-recur [m {} s s] (seq s) [(assoc m (first s) (take-while seq? (next s))) (drop-while seq? (next s))] => m))

(defn- parse-opts+specs [opts+specs]
    (let [[opts specs] (parse-opts opts+specs)
          impls (parse-impls specs)
          ifaces (-> (map #(let [v (resolve %)] (if (var? v) (:on @v) %)) (keys impls)) set (disj 'Object 'java.lang.Object) vec)
          methods (map #(let [[name pars & body] %] (cons name (maybe-destructured pars body))) (apply concat (vals impls)))]
        (when-some [bad-opts (seq (keys opts))]
            (throw! (apply str "unsupported option(s): " (interpose ", " bad-opts)))
        )
        [ifaces methods opts]
    )
)

;;;
 ; reify is a macro with the following structure:
 ;
 ; (reify options* specs*)
 ;
 ; Currently there are no options.
 ;
 ; Each spec consists of the protocol or interface name followed by
 ; zero or more method bodies:
 ;
 ; protocol-or-interface-or-Object
 ;  (methodName [args+] body)*
 ;
 ; Methods should be supplied for all methods of the desired protocol(s)
 ; and interface(s). You can also define overrides for methods of Object.
 ; Note that the first parameter must be supplied to correspond to the
 ; target object ('this' in Java parlance). Thus methods for interfaces
 ; will take one more argument than do the interface declarations. Note
 ; also that recur calls to the method head should *not* pass the target
 ; object, it will be supplied automatically and can not be substituted.
 ;
 ; The return type can be indicated by a type hint on the method name, and
 ; arg types can be indicated by a type hint on arg names. If you leave out
 ; all hints, reify will try to match on same name/arity method in the
 ; protocol(s)/interface(s) - this is preferred. If you supply any hints at
 ; all, no inference is done, so all hints (or default of Object) must be
 ; correct, for both arguments and return type. If a method is overloaded
 ; in a protocol/interface, multiple independent method definitions must be
 ; supplied. If overloaded with same arity in an interface you must specify
 ; complete hints to disambiguate - a missing hint implies Object.
 ;
 ; recur works to method heads. The method bodies of reify are lexical
 ; closures, and can refer to the surrounding local scope:
 ;
 ; (str (let [f "foo"]
 ;  (reify Object
 ;   (toString [this] f))))
 ; => "foo"
 ;
 ; (seq (let [f "foo"]
 ;  (reify Seqable
 ;   (seq [this] (seq f)))))
 ; => (\f \o \o)
 ;
 ; reify always implements IObj and transfers meta data of the form
 ; to the created object.
 ;
 ; (meta ^{:k :v} (reify Object (toString [this] "foo")))
 ; => {:k :v}
 ;;
(§ defmacro reify [& opts+specs]
    (let [[interfaces methods] (parse-opts+specs opts+specs)]
        (with-meta `(reify* ~interfaces ~@methods) (meta &form))
    )
)

(defn munge [s] ((if (symbol? s) symbol str) (Compiler'munge (str s))))

(defn- validate-fields [fields name]
    (when-not (vector? fields)
        (throw! "no fields vector given")
    )
    (let-when [specials '#{__meta __extmap __hash}] (some specials fields)
        (throw! (str "the names in " specials " cannot be used as field names for types or records"))
    )
    (let-when [non-syms (remove symbol? fields)] (seq non-syms)
        (throw! (apply str "defrecord and deftype fields must be symbols, " *ns* "." name " had: " (interpose ", " non-syms)))
    )
)

;;;
 ; Do not use this directly, use deftype.
 ;;
(defn- emit-deftype* [tagname cname fields interfaces methods opts]
    `(deftype* ~(symbol (name (ns-name *ns*)) (name tagname))
        ~(with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
        ~fields
        :implements ~(conj interfaces 'arbace.core.IType)
        ~@(mapcat identity opts)
        ~@methods
    )
)

;;;
 ; (deftype name [fields*] options* specs*)
 ;
 ; Options are expressed as sequential keywords and arguments (in any order).
 ;
 ; Each spec consists of a protocol or interface name followed by zero
 ; or more method bodies:
 ;
 ; protocol-or-interface-or-Object
 ; (methodName [args*] body)*
 ;
 ; Dynamically generates compiled bytecode for class with the given name,
 ; in a package with the same name as the current namespace, the given fields,
 ; and, optionally, methods for protocols and/or interfaces.
 ;
 ; The class will have the (by default, immutable) fields named by fields, which
 ; can have type hints. Protocols/interfaces and methods are optional. The only
 ; methods that can be supplied are those declared in the protocols/interfaces.
 ; Note that method bodies are not closures, the local environment includes only
 ; the named fields, and those fields can be accessed directly. Fields can be
 ; qualified with the metadata :volatile true or :mutable true,
 ; at which point (set! afield aval) will be supported in method bodies. Note well
 ; that mutable fields are extremely difficult to use correctly, and are present only
 ; to facilitate the building of higher level constructs, such as Arbace's reference
 ; types, in Arbace itself. They are for experts only - if the semantics and
 ; implications of :volatile or :mutable are not immediately
 ; apparent to you, you should not be using them.
 ;
 ; Method definitions take the form:
 ;
 ; (methodname [args*] body)
 ;
 ; The argument and return types can be hinted on the arg and methodname
 ; symbols. If not supplied, they will be inferred, so type hints should be
 ; reserved for disambiguation.
 ;
 ; Methods should be supplied for all methods of the desired protocol(s)
 ; and interface(s). You can also define overrides for methods of Object.
 ; Note that a parameter must be supplied to correspond to the target object
 ; ('this' in Java parlance). Thus methods for interfaces will take one more
 ; argument than do the interface declarations. Note also that recur calls
 ; to the method head should *not* pass the target object, it will be
 ; supplied automatically and can not be substituted.
 ;
 ; In the method bodies, the (unqualified) name can be used to name the
 ; class (for calls to new, instance?, etc).
 ;
 ; One constructor will be defined, taking the designated fields. Note
 ; that the field names __meta, __extmap and __hash are currently
 ; reserved and should not be used when defining your own types.
 ;;
(defmacro deftype [name fields & opts+specs]
    (validate-fields fields name)
    (let [[interfaces methods opts] (parse-opts+specs opts+specs)]
        `(do
            ~(emit-deftype* name name (vec fields) (vec interfaces) methods opts)
            (import ~(symbol (str (namespace-munge *ns*) "." name)))
        )
    )
)

;;;
 ; Do not use this directly, use defrecord.
 ;;
(defn- emit-defrecord* [tagname cname fields interfaces methods opts]
    (let [classname      (with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
          interfaces     (vec interfaces)
          hinted-fields  fields
          fields         (mapv #(with-meta % nil) fields)
          base-fields    fields
          fields         (conj fields '__meta '__extmap '^:mutable __hash)
          type-hash      (f'hash classname)]
        (when (some #{:volatile :mutable} (mapcat (comp keys meta) hinted-fields))
            (throw! ":volatile or :mutable not supported for record fields")
        )
        (let [gs (gensym)]
            (letfn [(irecord [[i m]] [(conj i 'arbace.core.IRecord) m])
                    (eqhash [[i m]]
                        [
                            (conj i 'arbace.core.Hashed 'arbace.core.IObject)
                            (conj m
                                `(Hashed'''hash [this#]
                                    (let-when [hash# ~'__hash] (zero? hash#) => hash#
                                        (set! ~'__hash (int (bit-xor ~type-hash (Murmur3'hashUnordered this#))))
                                    )
                                )
                                `(IObject'''equals [this# ~gs]
                                    (or (identical? this# ~gs)
                                        (and (identical? (class this#) (class ~gs))
                                            (let [~gs ~(with-meta gs {:tag tagname})]
                                                (and ~@(map (fn [%] `(= ~% (. ~gs ~(symbol (str "-" %))))) base-fields)
                                                    (= ~'__extmap (. ~gs ~'__extmap))
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        ]
                    )
                    (iobj [[i m]]
                        [
                            (conj i 'arbace.core.IMeta 'arbace.core.IObj)
                            (conj m
                                `(IMeta'''meta [_] ~'__meta)
                                `(IObj'''withMeta [_ ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))
                            )
                        ]
                    )
                    (ilookup [[i m]]
                        [
                            (conj i 'arbace.core.ILookup)
                            (conj m
                                `(ILookup'''valAt [this# k#] (ILookup'''valAt this# k# nil))
                                `(ILookup'''valAt [_ k# else#]
                                    (case k#
                                        ~@(mapcat (fn [%] [(keyword %) %]) base-fields)
                                        (get ~'__extmap k# else#)
                                    )
                                )
                            )
                        ]
                    )
                    (imap [[i m]]
                        [
                            (conj i 'arbace.core.Associative 'arbace.core.Counted 'arbace.core.IPersistentCollection 'arbace.core.IPersistentMap 'arbace.core.Seqable)
                            (conj m
                                `(Associative'''assoc [_ k# ~gs]
                                    (condp identical? k#
                                        ~@(mapcat
                                            (fn [%] [(keyword %) (list* `new tagname (replace {% gs} (remove '#{__hash} fields)))])
                                            base-fields
                                        )
                                        (new ~tagname ~@(remove '#{__extmap __hash} fields) (assoc ~'__extmap k# ~gs))
                                    )
                                )
                                `(Associative'''containsKey [this# k#] (not (identical? (ILookup'''valAt this# k# this#) this#)))
                                `(Associative'''entryAt [this# k#]
                                    (let-when [v# (ILookup'''valAt this# k# this#)] (not (identical? v# this#))
                                        (MapEntry'new k# v#)
                                    )
                                )
                                `(Counted'''count [_] (+ ~(count base-fields) (count ~'__extmap)))
                                `(IPersistentCollection'''empty [_] (throw! (str "empty not supported on " ~(str classname))))
                                `(IPersistentCollection'''conj [this# o#]
                                    (cond
                                        (map-entry? o#) (let [#_"IMapEntry" e# o#] (Associative'''assoc this# (key e#) (val e#)))
                                        (indexed? o#) (let [#_"Indexed" i# o#] (Associative'''assoc this# (nth i# 0) (nth i# 1)))
                                        :else
                                            (loop-when [this# this# s# (seq o#)] s# => this#
                                                (let [#_"IMapEntry" e# (first s#)]
                                                    (recur (Associative'''assoc this# (key e#) (val e#)) (next s#))
                                                )
                                            )
                                    )
                                )
                                `(IPersistentMap'''dissoc [this# k#]
                                    (if (contains? #{~@(map keyword base-fields)} k#)
                                        (dissoc (with-meta (into {} this#) ~'__meta) k#)
                                        (new ~tagname ~@(remove '#{__extmap __hash} fields) (not-empty (dissoc ~'__extmap k#)))
                                    )
                                )
                                `(Seqable'''seq [_] (seq (concat [~@(map #(list `MapEntry'new (keyword %) %) base-fields)] ~'__extmap)))
                            )
                        ]
                    )]
                (let [[i m] (-> [interfaces methods] irecord eqhash iobj ilookup imap)]
                    `(deftype* ~(symbol (name (ns-name *ns*)) (name tagname))
                        ~classname
                        ~(conj hinted-fields '__meta '__extmap '^int ^:mutable __hash)
                        :implements ~(vec i)
                        ~@(mapcat identity opts)
                        ~@m
                    )
                )
            )
        )
    )
)

;;;
 ; (defrecord name [fields*] options* specs*)
 ;
 ; Options are expressed as sequential keywords and arguments (in any order).
 ;
 ; Each spec consists of a protocol or interface name followed by zero
 ; or more method bodies:
 ;
 ; protocol-or-interface-or-Object
 ; (methodName [args*] body)*
 ;
 ; Dynamically generates compiled bytecode for class with the given name,
 ; in a package with the same name as the current namespace, the given fields,
 ; and, optionally, methods for protocols and/or interfaces.
 ;
 ; The class will have the (immutable) fields named by fields, which can have
 ; type hints. Protocols/interfaces and methods are optional. The only methods
 ; that can be supplied are those declared in the protocols/interfaces. Note
 ; that method bodies are not closures, the local environment includes only
 ; the named fields, and those fields can be accessed directly.
 ;
 ; Method definitions take the form:
 ;
 ; (methodname [args*] body)
 ;
 ; The argument and return types can be hinted on the arg and methodname
 ; symbols. If not supplied, they will be inferred, so type hints should be
 ; reserved for disambiguation.
 ;
 ; Methods should be supplied for all methods of the desired protocol(s)
 ; and interface(s). You can also define overrides for methods of Object.
 ; Note that a parameter must be supplied to correspond to the target object
 ; ('this' in Java parlance). Thus methods for interfaces will take one more
 ; argument than do the interface declarations. Note also that recur calls
 ; to the method head should *not* pass the target object, it will be
 ; supplied automatically and can not be substituted.
 ;
 ; In the method bodies, the (unqualified) name can be used to name the
 ; class (for calls to new, instance?, etc).
 ;
 ; The class will have implementations of several (arbace.core)
 ; interfaces generated automatically: IObj (metadata support) and
 ; IPersistentMap, and all of their superinterfaces.
 ;
 ; In addition, defrecord will define type-and-value-based =,
 ; and will defined Java .hashCode and .equals consistent with the
 ; contract for java.util.Map.
 ;
 ; Two constructors will be defined, one taking the designated fields
 ; followed by a metadata map (nil for none) and an extension field map
 ; (nil for none), and one taking only the fields (using nil for meta and
 ; extension fields). Note that the field names __meta, __extmap and
 ; __hash are currently reserved and should not be used when defining
 ; your own records.
 ;
 ; Given (defrecord TypeName ...), two factory functions will be defined:
 ; ->TypeName, taking positional parameters for the fields, and
 ; map->TypeName, taking a map of keywords to field values.
 ;;
(defmacro defrecord [name fields & opts+specs]
    (validate-fields fields name)
    (let [[interfaces methods opts] (parse-opts+specs opts+specs)]
        `(do
            ~(emit-defrecord* name name (vec fields) (vec interfaces) methods opts)
            (import ~(symbol (str (namespace-munge *ns*) "." name)))
        )
    )
)

(defn- super-chain [#_"Class" c]
    (when c
        (cons c (super-chain (.getSuperclass c)))
    )
)

(defn find-protocol-impl [protocol x]
    (if (instance? (:on-interface protocol) x)
        x
        (let [find- #(get (:impls protocol) %)
              pref-
                (fn
                    ([] nil)
                    ([a] a)
                    ([#_"Class" a #_"Class" b] (if (.isAssignableFrom a b) b a))
                )
              c (class x)]
            (or (find- c)
                (and c
                    (or (some find- (butlast (super-chain c)))
                        (when-some [t (reduce pref- (filter find- (disj (supers c) Object)))]
                            (find- t)
                        )
                        (find- Object)
                    )
                )
            )
        )
    )
)

(defn find-protocol-method [protocol methodk x]
    (get (find-protocol-impl protocol x) methodk)
)

(defn- protocol? [maybe-p]
    (boolean (:on-interface maybe-p))
)

(defn- implements? [protocol atype]
    (and atype (.isAssignableFrom #_"Class" (:on-interface protocol) atype))
)

;;;
 ; Returns true if atype extends protocol.
 ;;
(defn extends? [protocol atype]
    (boolean (or (implements? protocol atype) (get (:impls protocol) atype)))
)

;;;
 ; Returns a collection of the types explicitly extending protocol.
 ;;
(defn extenders [protocol] (keys (:impls protocol)))

;;;
 ; Returns true if x satisfies the protocol.
 ;;
(§ defn satisfies? [protocol x]
    (boolean (find-protocol-impl protocol x))
)

(defn -cache-protocol-fn [#_"Fn" this x #_"Class" c #_"IFn" ifn]
    (let [cache @(:__methodImplCache this)
          f (if (instance? c x) ifn (find-protocol-method (:protocol cache) (:methodk cache) x))]
        (when-not f
            (throw!
                (str "no implementation of method: " (:methodk cache)
                     " of protocol: " (:var (:protocol cache))
                     " found for class: " (if (some? x) (.getName (class x)) "nil"))
            )
        )
        (reset! (:__methodImplCache this) (MethodImplCache''assoc cache (class x) f))
        f
    )
)

(defn- emit-method-builder [on-interface method on-method arglists]
    (let [methodk (keyword method) this' (with-meta (gensym) {:tag 'arbace.core.Fn}) ifn' (gensym)]
        `(fn [cache#]
            (let [~ifn'
                    (fn ~@(map
                        (fn [args]
                            (let [args' (map #(gensym (str "gf__" % "__")) args) target (first args')]
                                `([~@args'] (. ~(with-meta target {:tag on-interface}) (~(or on-method method) ~@(next args'))))
                            )
                        )
                        arglists
                    ))
                  #_"Fn" f#
                    (fn ~this' ~@(map
                        (fn [args]
                            (let [args' (map #(gensym (str "gf__" % "__")) args) target (first args')]
                                `([~@args']
                                    (let [cache# @(:__methodImplCache ~this')
                                          f# (MethodImplCache''get cache# (Reflector'classOf ~target))]
                                        ((or f# (-cache-protocol-fn ~this' ~target ~on-interface ~ifn')) ~@args')
                                    )
                                )
                            )
                        )
                        arglists
                    ))]
                (reset! (:__methodImplCache f#) cache#)
                f#
            )
        )
    )
)

(defn -reset-methods [protocol]
    (doseq [[#_"Var" v build] (:method-builders protocol)]
        (Var''bindRoot v (build (MethodImplCache'new protocol (keyword (:sym v)))))
    )
)

(defn- assert-same-protocol [protocol-var method-syms]
    (doseq [m method-syms]
        (let [v (resolve m) pv (:protocol (meta v))]
            (when (and v (bound? v) (not= protocol-var pv))
                (.println *err*,
                    (str "WARNING: protocol " protocol-var " is overwriting "
                        (if pv
                            (str "method " (:sym v) " of protocol " (:sym pv))
                            (str "function " (:sym v))
                        )
                    )
                )
            )
        )
    )
)

(defn- emit-protocol [name opts+sigs]
    (let [iname (symbol (str (munge (namespace-munge *ns*)) "." (munge name)))
          [opts sigs]
            (loop [opts {:on (list 'quote iname) :on-interface iname} sigs opts+sigs]
                (condp #(%1 %2) (first sigs)
                    keyword? (recur (assoc opts (first sigs) (second sigs)) (next (next sigs)))
                    [opts sigs]
                )
            )
          sigs
            (when sigs
                (reduce
                    (fn [m s]
                        (let [nmeta (meta (first s)) mname (with-meta (first s) nil) arglists (take-while vector? (next s))]
                            (when (some zero? (map count arglists))
                                (throw! (str "definition of function " mname " in protocol " name " must take at least one arg"))
                            )
                            (when (m (keyword mname))
                                (throw! (str "function " mname " in protocol " name " was redefined: specify all arities in single definition"))
                            )
                            (assoc m (keyword mname) (merge nmeta {:name (vary-meta mname assoc :arglists arglists) :arglists arglists}))
                        )
                    )
                    {} sigs
                )
            )
          meths
            (mapcat
                (fn [sig]
                    (let [m (munge (:name sig))]
                        (map #(vector m (vec (repeat (dec (count %)) 'Object)) 'Object) (:arglists sig))
                    )
                )
                (vals sigs)
            )]
        `(do
            (defonce ~name {})
            (gen-interface :name ~iname :methods ~meths)
            ~(when sigs
                `(#'assert-same-protocol (var ~name) '~(map :name (vals sigs)))
            )
            (alter-var-root (var ~name) merge
                (assoc ~opts
                    :sigs '~sigs
                    :var (var ~name)
                    :method-map
                        ~(and (:on opts)
                            (apply hash-map
                                (mapcat
                                    (fn [sig] [(keyword (:name sig)) (keyword (or (:on sig) (:name sig)))])
                                    (vals sigs)
                                )
                            )
                        )
                    :method-builders
                        ~(apply hash-map
                            (mapcat
                                (fn [sig]
                                    [
                                        `(intern *ns* (with-meta '~(:name sig) (merge '~sig {:protocol (var ~name)})))
                                        (emit-method-builder (:on-interface opts) (:name sig) (:on sig) (:arglists sig))
                                    ]
                                )
                                (vals sigs)
                            )
                        )
                )
            )
            (-reset-methods ~name)
            '~name
        )
    )
)

;;;
 ; A protocol is a named set of named methods and their signatures:
 ;
 ; (defprotocol AProtocolName
 ;
 ;  ;; method signatures
 ;  (bar [this a b])
 ;  (baz [this a] [this a b] [this a b c]))
 ;
 ; No implementations are provided. The above yields a set of polymorphic
 ; functions and a protocol object. All are namespace-qualified by the ns
 ; enclosing the definition The resulting functions dispatch on the type of
 ; their first argument, which is required and corresponds to the implicit
 ; target object ('this' in Java parlance). defprotocol is dynamic, has no
 ; special compile-time effect, and defines no new types or classes.
 ; Implementations of the protocol methods can be provided using extend.
 ;
 ; defprotocol will automatically generate a corresponding interface, with
 ; the same name as the protocol, i.e. given a protocol: my.ns/Protocol, an
 ; interface: my.ns.Protocol. The interface will have methods corresponding
 ; to the protocol functions, and the protocol will automatically work with
 ; instances of the interface.
 ;
 ; Note that you should not use this interface with deftype or reify, as
 ; they support the protocol directly:
 ;
 ; (defprotocol P
 ;  (foo [this])
 ;  (bar-me [this] [this y]))
 ;
 ; (deftype Foo [a b c]
 ;  P
 ;  (foo [this] a)
 ;  (bar-me [this] b)
 ;  (bar-me [this y] (+ c y)))
 ;
 ; (bar-me (Foo. 1 2 3) 42)
 ; => 45
 ;
 ; (foo
 ;  (let [x 42]
 ;   (reify P
 ;    (foo [this] 17)
 ;    (bar-me [this] x)
 ;    (bar-me [this y] x))))
 ; => 17
 ;;
(defmacro defprotocol [name & opts+sigs]
    (emit-protocol name opts+sigs)
)

;;;
 ; Implementations of protocol methods can be provided using the extend
 ; construct:
 ;
 ; (extend AType
 ;  AProtocol
 ;  {:foo an-existing-fn
 ;   :bar (fn [a b] ...)
 ;   :baz (fn ([a]...) ([a b] ...)...)}
 ;  BProtocol
 ;   {...}
 ;  ...)
 ;
 ; extend takes a type/class (or interface, see below), and one or more
 ; protocol + method map pairs. It will extend the polymorphism of the
 ; protocol's methods to call the supplied methods when an AType is
 ; provided as the first argument.
 ;
 ; Method maps are maps of the keyword-ized method names to ordinary
 ; fns. This facilitates easy reuse of existing fns and fn maps, for
 ; code reuse/mixins without derivation or composition. You can extend
 ; an interface to a protocol. This is primarily to facilitate interop
 ; with the host (e.g. Java) but opens the door to incidental multiple
 ; inheritance of implementation since a class can inherit from more
 ; than one interface, both of which extend the protocol. It is TBD how
 ; to specify which impl to use. You can extend a protocol on nil.
 ;
 ; If you are supplying the definitions explicitly (i.e. not reusing
 ; exsting functions or mixin maps), you may find it more convenient to
 ; use the extend-type or extend-protocol macros.
 ;
 ; Note that multiple independent extend clauses can exist for the same
 ; type, not all protocols need be defined in a single extend call.
 ;
 ; See also: extends?, satisfies?, extenders.
 ;;
(defn extend [atype & proto+mmaps]
    (doseq [[proto mmap] (partition 2 proto+mmaps)]
        (when-not (protocol? proto)
            (throw! (str proto " is not a protocol"))
        )
        (when (implements? proto atype)
            (throw! (str atype " already directly implements " (:on-interface proto) " for protocol:" (:var proto)))
        )
        (-reset-methods (alter-var-root (:var proto) assoc-in [:impls atype] mmap))
    )
)

(defn- emit-impl [[p fs]]
    [p (zipmap (map #(-> % first keyword) fs) (map #(cons `fn (drop 1 %)) fs))]
)

(defn- emit-hinted-impl [c [p fs]]
    (letfn [(hint- [s]
                (let [s (if (vector? (first s)) (list s) s)]
                    (map
                        (fn [[[target & args] & body]]
                            (cons (apply vector (vary-meta target assoc :tag c) args) body)
                        )
                        s
                    )
                )
            )]
        [p (zipmap (map #(-> % first name keyword) fs) (map #(cons `fn (hint- (drop 1 %))) fs))]
    )
)

;;;
 ; A macro that expands into an extend call. Useful when you are supplying
 ; the definitions explicitly inline, extend-type automatically creates
 ; the maps required by extend. Propagates the class as a type hint on the
 ; first argument of all fns.
 ;
 ; (extend-type MyType
 ;  Countable
 ;  (cnt [c] ...)
 ;  Foo
 ;  (bar [x y] ...)
 ;  (baz ([x] ...) ([x y & zs] ...)))
 ;
 ; expands into:
 ;
 ; (extend MyType
 ;  Countable
 ;  {:cnt (fn [c] ...)}
 ;  Foo
 ;  {:baz (fn ([x] ...) ([x y & zs] ...))
 ;   :bar (fn [x y] ...)})
 ;;
(defmacro extend-type [t & specs]
    `(extend ~t ~@(mapcat (partial emit-hinted-impl t) (parse-impls specs)))
)

;;;
 ; Useful when you want to provide several implementations of the same
 ; protocol all at once. Takes a single protocol and the implementation
 ; of that protocol for one or more types. Expands into calls to
 ; extend-type:
 ;
 ; (extend-protocol Protocol
 ;  AType
 ;  (foo [x] ...)
 ;  (bar [x y] ...)
 ;  BType
 ;  (foo [x] ...)
 ;  (bar [x y] ...)
 ;  AClass
 ;  (foo [x] ...)
 ;  (bar [x y] ...)
 ;  nil
 ;  (foo [x] ...)
 ;  (bar [x y] ...))
 ;
 ; expands into:
 ;
 ; (do
 ;  (arbace.core/extend-type AType Protocol
 ;   (foo [x] ...)
 ;   (bar [x y] ...))
 ;  (arbace.core/extend-type BType Protocol
 ;   (foo [x] ...)
 ;   (bar [x y] ...))
 ;  (arbace.core/extend-type AClass Protocol
 ;   (foo [x] ...)
 ;   (bar [x y] ...))
 ;  (arbace.core/extend-type nil Protocol
 ;   (foo [x] ...)
 ;   (bar [x y] ...)))
 ;;
(defmacro extend-protocol [p & specs]
    `(do ~@(map (fn [[t fs]] `(extend-type ~t ~p ~@fs)) (parse-impls specs)))
)
)

(about #_"cloiure.core.protocols"

(defn- seq-reduce
    ([s f] (if-some [s (seq s)] (seq-reduce (next s) f (first s)) (f)))
    ([s f r]
        (loop-when [r r s (seq s)] s => r
            (let [r (f r (first s))]
                (if (reduced? r) @r (recur r (next s)))
            )
        )
    )
)

;; redefine reduce with IReduce

;;;
 ; f should be a function of 2 arguments. If val is not supplied, returns
 ; the result of applying f to the first 2 items in coll, then applying f
 ; to that result and the 3rd item, etc. If coll contains no items, f must
 ; accept no arguments as well, and reduce returns the result of calling f
 ; with no arguments. If coll has only 1 item, it is returned and f is not
 ; called. If val is supplied, returns the result of applying f to val and
 ; the first item in coll, then applying f to that result and the 2nd item,
 ; etc. If coll contains no items, returns val and f is not called.
 ;;
(defn reduce
    ([f s]
        (if (satisfies? IReduce s)
            (IReduce'''reduce #_"IReduce" s f)
            (seq-reduce s f)
        )
    )
    ([f r s]
        (if (satisfies? IReduce s)
            (IReduce'''reduce #_"IReduce" s f r)
            (seq-reduce s f r)
        )
    )
)

;;;
 ; Protocol for concrete associative types that can reduce themselves via
 ; a function of key and val faster than first/next recursion over map entries.
 ; Called by reduce-kv, and has same semantics (just different arg order).
 ;;
(§ defprotocol KVReduce
    (kv-reduce [m f r])
)

(§ extend-protocol KVReduce
    nil
    (kv-reduce [_ _ r] r)

    ;; slow path default
    IPersistentMap
    (kv-reduce [m f r] (reduce (fn [r [k v]] (f r k v)) r m))

    IKVReduce
    (kv-reduce [m f r] (IKVReduce'''kvreduce m, f, r))
)

;;;
 ; Reduces an associative collection. f should be a function of 3 arguments.
 ; Returns the result of applying f to init, the first key and the first value
 ; in coll, then applying f to that result and the 2nd key and value, etc.
 ; If coll contains no entries, returns init and f is not called. Note that
 ; reduce-kv is supported on vectors, where the keys will be the ordinals.
 ;;
(§ defn reduce-kv [f r m] (kv-reduce m f r))

;;;
 ; Takes a reducing function f of 2 args and returns a fn suitable for
 ; transduce by adding an arity-1 signature that calls cf (default -
 ; identity) on the result argument.
 ;;
(defn completing
    ([f] (completing f identity))
    ([f cf]
        (fn
            ([] (f))
            ([x] (cf x))
            ([x y] (f x y))
        )
    )
)

;;;
 ; reduce with a transformation of f (xf). If init is not supplied, (f) will
 ; be called to produce it. f should be a reducing step function that accepts
 ; both 1 and 2 arguments, if it accepts only 2 you can add the arity-1 with
 ; 'completing'. Returns the result of applying (the transformed) xf to init
 ; and the first item in coll, then applying xf to that result and the 2nd
 ; item, etc. If coll contains no items, returns init and f is not called.
 ; Note that certain transforms may inject or skip items.
 ;;
(defn transduce
    ([xform f s] (transduce xform f (f) s))
    ([xform f r s] (let [f (xform f)] (f (reduce f r s))))
)

;;;
 ; Returns a new coll consisting of to-coll with all of the items of from-coll
 ; conjoined. A transducer may be supplied.
 ;;
(defn into
    ([] [])
    ([to] to)
    ([to from]
        (if (editable? to)
            (with-meta (reduce! conj! to from) (meta to))
            (reduce conj to from)
        )
    )
    ([to xform from]
        (if (editable? to)
            (with-meta (persistent! (transduce xform conj! (transient to) from)) (meta to))
            (transduce xform conj to from)
        )
    )
)

;;;
 ; Returns a vector consisting of the result of applying f to the set of first
 ; items of each coll, followed by applying f to the set of second items in each
 ; coll, until any one of the colls is exhausted. Any remaining items in other
 ; colls are ignored. Function f should accept number-of-colls arguments.
 ;;
(defn mapv
    ([f coll] (reduce! #(conj! %1 (f %2)) [] coll))
    ([f c1 c2] (into [] (map f c1 c2)))
    ([f c1 c2 c3] (into [] (map f c1 c2 c3)))
    ([f c1 c2 c3 & colls] (into [] (apply map f c1 c2 c3 colls)))
)

;;;
 ; Returns a vector of the items in coll for which (f? item)
 ; returns logical true. f? must be free of side-effects.
 ;;
(defn filterv [f? s] (reduce! #(if (f? %2) (conj! %1 %2) %1) [] s))

;;;
 ; Takes any nested combination of sequential things (lists, vectors, etc.)
 ; and returns their contents as a single, flat sequence.
 ; (flatten nil) returns an empty sequence.
 ;;
(defn flatten [s] (remove sequential? (next (tree-seq sequential? seq s))))

;;;
 ; Returns a map of the elements of coll keyed by the result of
 ; f on each element. The value at each key will be a vector of the
 ; corresponding elements, in the order they appeared in coll.
 ;;
(defn group-by [f s] (reduce! #(let [k (f %2)] (assoc! %1 k (conj (get %1 k []) %2))) {} s))

;;;
 ; Applies f to each value in coll, splitting it each time f returns
 ; a new value. Returns a lazy seq of partitions. Returns a stateful
 ; transducer when no collection is provided.
 ;;
(defn partition-by
    ([f]
        (fn [g]
            (let [l' (atom []) p' (atom ::none)]
                (fn
                    ([] (g))
                    ([s]
                        (let [s (when (seq @l') => s
                                    (let [l @l' _ (swap! l' empty)]
                                        (unreduced (g s l))
                                    )
                                )]
                            (g s)
                        )
                    )
                    ([s x]
                        (let [p @p' y (f x) _ (reset! p' y)]
                            (if (or (identical? p ::none) (= y p))
                                (do
                                    (swap! l' conj x)
                                    s
                                )
                                (let [l @l' _ (swap! l' empty) s' (g s l)]
                                    (when-not (reduced? s')
                                        (swap! l' conj x)
                                    )
                                    s'
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    ([f s]
        (lazy-seq
            (when-some [s (seq s)]
                (let [x (first s) fx (f x)
                      s' (cons x (take-while #(= (f %) fx) (next s)))]
                    (cons s' (partition-by f (drop (count s') s)))
                )
            )
        )
    )
)

;;;
 ; Returns a map from distinct items in coll to the number of times they appear.
 ;;
(defn frequencies [s] (reduce! #(assoc! %1 %2 (inc (get %1 %2 0))) {} s))

;;;
 ; Returns a lazy seq of the intermediate values of the reduction (as per reduce)
 ; of coll by f, starting with init.
 ;;
(defn reductions
    ([f coll]
        (lazy-seq
            (if-some [s (seq coll)]
                (reductions f (first s) (next s))
                (list (f))
            )
        )
    )
    ([f init coll]
        (if (reduced? init)
            (list @init)
            (cons init
                (lazy-seq
                    (when-some [s (seq coll)]
                        (reductions f (f init (first s)) (next s))
                    )
                )
            )
        )
    )
)

;;;
 ; Returns a lazy sequence consisting of the result of applying f to 0
 ; and the first item of coll, followed by applying f to 1 and the second
 ; item in coll, etc, until coll is exhausted. Thus function f should
 ; accept 2 arguments, index and item. Returns a stateful transducer when
 ; no collection is provided.
 ;;
(defn map-indexed
    ([f]
        (fn [g]
            (let [i' (atom -1)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x] (g s (f (swap! i' inc) x)))
                )
            )
        )
    )
    ([f s]
        (letfn [(mapi- [i s]
                    (lazy-seq
                        (when-some [s (seq s)]
                            (cons (f i (first s)) (mapi- (inc i) (next s)))
                        )
                    )
                )]
            (mapi- 0 s)
        )
    )
)

;;;
 ; Returns a lazy sequence of the non-nil results of (f item). Note,
 ; this means false return values will be included. f must be free of
 ; side-effects. Returns a transducer when no collection is provided.
 ;;
(defn keep
    ([f]
        (fn [g]
            (fn
                ([] (g))
                ([s] (g s))
                ([s x]
                    (let-when [y (f x)] (some? y) => s
                        (g s y)
                    )
                )
            )
        )
    )
    ([f s]
        (lazy-seq
            (when-some [s (seq s)]
                (let-when [y (f (first s))] (some? y) => (keep f (next s))
                    (cons y (keep f (next s)))
                )
            )
        )
    )
)

;;;
 ; Returns a lazy sequence of the non-nil results of (f index item).
 ; Note, this means false return values will be included. f must be free
 ; of side-effects. Returns a stateful transducer when no collection is
 ; provided.
 ;;
(defn keep-indexed
    ([f]
        (fn [g]
            (let [i' (atom -1)]
                (fn
                    ([] (g))
                    ([s] (g s))
                    ([s x]
                        (let-when [y (f (swap! i' inc) x)] (some? y) => s
                            (g s y)
                        )
                    )
                )
            )
        )
    )
    ([f s]
        (letfn [(keepi- [i s]
                    (lazy-seq
                        (when-some [s (seq s)]
                            (let-when [y (f i (first s))] (some? y) => (keepi- (inc i) (next s))
                                (cons y (keepi- (inc i) (next s)))
                            )
                        )
                    )
                )]
            (keepi- 0 s)
        )
    )
)

;;;
 ; Takes a set of predicates and returns a function f that returns true if all
 ; of its composing predicates return a logical true value against all of its
 ; arguments, else it returns false. Note that f is short-circuiting in that
 ; it will stop execution on the first argument that triggers a logical false
 ; result against the original predicates.
 ;;
(defn every-pred
    ([p]
        (fn ep-
            ([] true)
            ([x] (boolean (p x)))
            ([x y] (boolean (and (p x) (p y))))
            ([x y z] (boolean (and (p x) (p y) (p z))))
            ([x y z & args] (boolean (and (ep- x y z) (every? p args))))
        )
    )
    ([p1 p2]
        (fn ep-
            ([] true)
            ([x] (boolean (and (p1 x) (p2 x))))
            ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
            ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
            ([x y z & args] (boolean (and (ep- x y z) (every? #(and (p1 %) (p2 %)) args))))
        )
    )
    ([p1 p2 p3]
        (fn ep-
            ([] true)
            ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
            ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
            ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
            ([x y z & args] (boolean (and (ep- x y z) (every? #(and (p1 %) (p2 %) (p3 %)) args))))
        )
    )
    ([p1 p2 p3 & ps]
        (let [ps (list* p1 p2 p3 ps)]
            (fn ep-
                ([] true)
                ([x] (every? #(% x) ps))
                ([x y] (every? #(and (% x) (% y)) ps))
                ([x y z] (every? #(and (% x) (% y) (% z)) ps))
                ([x y z & args] (boolean (and (ep- x y z) (every? #(every? % args) ps))))
            )
        )
    )
)

;;;
 ; Takes a set of predicates and returns a function f that returns the first
 ; logical true value returned by one of its composing predicates against any of
 ; its arguments, else it returns logical false. Note that f is short-circuiting
 ; in that it will stop execution on the first argument that triggers a logical
 ; true result against the original predicates.
 ;;
(defn some-fn
    ([p]
        (fn sp-
            ([] nil)
            ([x] (p x))
            ([x y] (or (p x) (p y)))
            ([x y z] (or (p x) (p y) (p z)))
            ([x y z & args] (or (sp- x y z) (some p args)))
        )
    )
    ([p1 p2]
        (fn sp-
            ([] nil)
            ([x] (or (p1 x) (p2 x)))
            ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
            ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
            ([x y z & args] (or (sp- x y z) (some #(or (p1 %) (p2 %)) args)))
        )
    )
    ([p1 p2 p3]
        (fn sp-
            ([] nil)
            ([x] (or (p1 x) (p2 x) (p3 x)))
            ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
            ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
            ([x y z & args] (or (sp- x y z) (some #(or (p1 %) (p2 %) (p3 %)) args)))
        )
    )
    ([p1 p2 p3 & ps]
        (let [ps (list* p1 p2 p3 ps)]
            (fn sp-
                ([] nil)
                ([x] (some #(% x) ps))
                ([x y] (some #(or (% x) (% y)) ps))
                ([x y z] (some #(or (% x) (% y) (% z)) ps))
                ([x y z & args] (or (sp- x y z) (some #(some % args) ps)))
            )
        )
    )
)

;;;
 ; Takes an expression and a set of test/form pairs. Threads expr (via ->)
 ; through each form for which the corresponding test expression is true.
 ; Note that, unlike cond branching, cond-> threading does not short circuit
 ; after the first true test expression.
 ;;
(defmacro cond-> [e & s]
    (assert-args
        (even? (count s)) "an even number of forms as clauses"
    )
    (let [e' (gensym)
          s (map (fn [[? x]] `(if ~? (-> ~e' ~x) ~e')) (partition 2 s))]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

;;;
 ; Takes an expression and a set of test/form pairs. Threads expr (via ->>)
 ; through each form for which the corresponding test expression is true.
 ; Note that, unlike cond branching, cond->> threading does not short circuit
 ; after the first true test expression.
 ;;
(defmacro cond->> [e & s]
    (assert-args
        (even? (count s)) "an even number of forms as clauses"
    )
    (let [e' (gensym)
          s (map (fn [[? x]] `(if ~? (->> ~e' ~x) ~e')) (partition 2 s))]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

;;;
 ; Binds name to expr, evaluates the first form in the lexical context
 ; of that binding, then binds name to that result, repeating for each
 ; successive form, returning the result of the last form.
 ;;
(defmacro as-> [e e' & s]
    `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
        ~(if (seq s) (last s) e')
    )
)

;;;
 ; When expr is not nil, threads it into the first form (via ->),
 ; and when that result is not nil, through the next, etc.
 ;;
(defmacro some-> [e & s]
    (let [e' (gensym)
          s (map (fn [x] `(when (some? ~e') (-> ~e' ~x))) s)]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

;;;
 ; When expr is not nil, threads it into the first form (via ->>),
 ; and when that result is not nil, through the next, etc.
 ;;
(defmacro some->> [e & s]
    (let [e' (gensym)
          s (map (fn [x] `(when (some? ~e') (->> ~e' ~x))) s)]
        `(let [~e' ~e ~@(interleave (repeat e') (butlast s))]
            ~(if (seq s) (last s) e')
        )
    )
)

(defn- preserving-reduced [f] #(let [r (f %1 %2)] (if (reduced? r) (reduced r) r)))

;;;
 ; A transducer which concatenates the contents of each input, which must
 ; be a collection, into the reduction.
 ;;
(defn cat [f]
    (let [g (preserving-reduced f)]
        (fn
            ([] (f))
            ([s] (f s))
            ([s x] (reduce g s x))
        )
    )
)

;;;
 ; Returns a transducer that ends transduction when f? returns true for an input.
 ; When retf is supplied it must be a fn of 2 arguments - it will be passed the
 ; (completed) result so far and the input that triggered the predicate, and its
 ; return value (if it does not throw an exception) will be the return value of the
 ; transducer. If retf is not supplied, the input that triggered the predicate will
 ; be returned. If the predicate never returns true the transduction is unaffected.
 ;;
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

;;;
 ; Runs the supplied procedure (via reduce), for purposes of side effects,
 ; on successive items in the collection. Returns nil.
 ;;
(defn run! [proc coll]
    (reduce #(proc %2) nil coll)
    nil
)
)

(about #_"cloiure.set"

;;;
 ; Move a maximal element of coll according to fn k (which returns a number) to the front of coll.
 ;;
(defn- bubble-max-key [k s]
    (let [m (apply max-key k s)]
        (cons m (remove #(identical? m %) s))
    )
)

;;;
 ; Return a set that is the union of the input sets.
 ;;
(defn union
    ([] #{})
    ([x] x)
    ([x y] (if (< (count x) (count y)) (into y x) (into x y)))
    ([x y & s]
        (let [[x & y] (bubble-max-key count (into s y x))]
            (reduce into x y)
        )
    )
)

;;;
 ; Return a set that is the intersection of the input sets.
 ;;
(defn intersection
    ([x] x)
    ([x y] (recur-when (< (count y) (count x)) [y x] => (reduce #(if (contains? y %2) %1 (disj %1 %2)) x x)))
    ([x y & s]
        (let [[x & y] (bubble-max-key (comp - count) (conj s y x))]
            (reduce intersection x y)
        )
    )
)

;;;
 ; Return a set that is the first set without elements of the remaining sets.
 ;;
(defn difference
    ([x] x)
    ([x y]
        (if (< (count x) (count y))
            (reduce #(if (contains? y %2) (disj %1 %2) %1) x x)
            (reduce disj x y)
        )
    )
    ([x y & s] (reduce difference x (conj s y)))
)

;;;
 ; Returns a set of the elements for which f? is true.
 ;;
(defn select [f? xset]
    (reduce (fn [s k] (if (f? k) s (disj s k))) xset xset)
)

;;;
 ; Returns a rel of the elements of xrel with only the keys in ks.
 ;;
(defn project [xrel ks]
    (with-meta (set (map #(select-keys % ks) xrel)) (meta xrel))
)

;;;
 ; Returns m with the keys in r renamed to the vals in r.
 ;;
(defn rename-keys [m r]
    (reduce
        (fn [m' [k k']]
            (if (contains? m k)
                (assoc m' k' (get m k))
                m'
            )
        )
        (apply dissoc m (keys r)) r
    )
)

;;;
 ; Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap.
 ;;
(defn rename [xrel kmap]
    (with-meta (set (map #(rename-keys % kmap) xrel)) (meta xrel))
)

;;;
 ; Returns a map of the distinct values of ks in the xrel mapped to
 ; a set of the maps in xrel with the corresponding values of ks.
 ;;
(defn index [xrel ks]
    (reduce
        (fn [m x]
            (let [ik (select-keys x ks)]
                (assoc m ik (conj (get m ik #{}) x))
            )
        )
        {} xrel
    )
)

;;;
 ; Returns the map with the vals mapped to the keys.
 ;;
(defn map-invert [m] (reduce (fn [m [k v]] (assoc m v k)) {} m))

;;;
 ; When passed 2 rels, returns the rel corresponding to the natural join.
 ; When passed an additional keymap, joins on the corresponding keys.
 ;;
(defn join
    ([a b] ;; natural join
        (when (and (seq a) (seq b)) => #{}
            (let [k* (intersection (set (keys (first a))) (set (keys (first b))))
                  [a b] (if (<= (count a) (count b)) [a b] [b a])
                  i* (index a k*)]
                (reduce
                    (fn [s x]
                        (if-let [found (i* (select-keys x k*))]
                            (reduce #(conj %1 (merge %2 x)) s found)
                            s
                        )
                    )
                    #{} b
                )
            )
        )
    )
    ([a b m] ;; arbitrary key mapping
        (let [[a b m] (if (<= (count a) (count b)) [a b (map-invert m)] [b a m])
              i* (index a (vals m))]
            (reduce
                (fn [s x]
                    (if-let [found (i* (rename-keys (select-keys x (keys m)) m))]
                        (reduce #(conj %1 (merge %2 x)) s found)
                        s
                    )
                )
                #{} b
            )
        )
    )
)

(defn #_"Boolean" subset?   [a b] (and (<= (count a) (count b)) (every? #(contains? b %) a)))
(defn #_"Boolean" superset? [a b] (and (<= (count b) (count a)) (every? #(contains? a %) b)))
)

;;;
 ; This namespace defines a generic tree walker for Arbace data structures.
 ; It takes any data structure (list, vector, map, set, seq), calls a function
 ; on every element, and uses the return value of the function in place of the
 ; original. This makes it fairly easy to write recursive search-and-replace
 ; functions, as shown in the examples.
 ;
 ; Note: "walk" supports all Arbace data structures EXCEPT maps created with
 ; sorted-map-by. There is no (obvious) way to retrieve the sorting function.
 ;;
(about #_"cloiure.walk"

;;;
 ; Traverses form, an arbitrary data structure. inner and outer are functions.
 ; Applies inner to each element of form, building up a data structure of the
 ; same type, then applies outer to the result. Recognizes all Arbace data
 ; structures. Consumes seqs as with doall.
 ;;
(defn walk [inner outer form]
    (cond
        (list? form)      (outer (apply list (map inner form)))
        (map-entry? form) (outer (vec (map inner form)))
        (seq? form)       (outer (doall (map inner form)))
        (record? form)    (outer (reduce #(conj %1 (inner %2)) form form))
        (coll? form)      (outer (into (empty form) (map inner form)))
        :else             (outer form)
    )
)

;;;
 ; Performs a depth-first, post-order traversal of form. Calls f on
 ; each sub-form, uses f's return value in place of the original.
 ; Recognizes all Arbace data structures. Consumes seqs as with doall.
 ;;
(defn postwalk [f form] (walk (partial postwalk f) f form))

;;;
 ; Like postwalk, but does pre-order traversal.
 ;;
(defn prewalk [f form] (walk (partial prewalk f) identity (f form)))

;;;
 ; Recursively transforms form by replacing keys in m with their
 ; values. Like arbace/replace but works on any data structure. Does
 ; replacement at the root of the tree first.
 ;;
(defn prewalk-replace [m form] (prewalk #(if (contains? m %) (m %) %) form))

;;;
 ; Recursively transforms form by replacing keys in m with their
 ; values. Like arbace/replace but works on any data structure. Does
 ; replacement at the leaves of the tree first.
 ;;
(defn postwalk-replace [m form] (postwalk #(if (contains? m %) (m %) %) form))

;;;
 ; Recursively performs all possible macroexpansions in form.
 ;;
(defn macroexpand-all [form] (prewalk #(if (seq? %) (macroexpand %) %) form))
)

(defn abs [a] (if (neg? a) (- a) a))

(defn assoc'  [v i x & s] (apply assoc  (vec v) i x s))
(defn conj'   [v   x & s] (apply conj   (vec v)   x s))
(defn into'   [v       s]       (into   (vec v)     s))
(defn peek'   [v]               (peek   (vec v)      ))
(defn pop'    [v]               (pop    (vec v)      ))
(defn update' [v i f & s] (apply update (vec v) i f s))

(defn dissoc' [v i] (let [v (vec v)] (catvec (subvec v 0 i) (subvec v (inc i)))))

(about #_"HotSpot"
    (def #_"HotSpotJVMCIRuntime" JVMCI'runtime (HotSpotJVMCIRuntime/runtime))

    (def #_"CompilerToVM"    HotSpot'native (#_"HotSpotJVMCIRuntime" .getCompilerToVM JVMCI'runtime))
    (def #_"HotSpotVMConfig" HotSpot'config (#_"HotSpotJVMCIRuntime" .getConfig       JVMCI'runtime))

    (def #_"boolean" HotSpot'useG1GC (.getFlag HotSpot'config, "UseG1GC", Boolean))

    (def #_"boolean" HotSpot'useCompressedOops          (.getFlag HotSpot'config, "UseCompressedOops",          Boolean))
    (def #_"boolean" HotSpot'useCompressedClassPointers (.getFlag HotSpot'config, "UseCompressedClassPointers", Boolean))

    (when-not (and HotSpot'useG1GC HotSpot'useCompressedOops HotSpot'useCompressedClassPointers)
        (throw! "“Use the Force, Luke!”")
    )
)

(about #_"Arbace")

(defn -main [& args])
