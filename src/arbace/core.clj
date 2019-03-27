(ns arbace.core
    (:refer-clojure :only [defmacro])
)

(defmacro § [& _])
(defmacro ß [& _])

(ns arbace.bore
    (:refer-clojure :only [*ns* -> = case conj cons defmacro defn defn- doseq fn identity if-some int keys keyword let letfn map mapcat merge meta partial some? str symbol symbol? vary-meta vec vector when with-meta]) (:require [clojure.core :as -])
    #_(:require [flatland.ordered.map :refer [ordered-map]] [flatland.ordered.set :refer [ordered-set]])
)

(defmacro import! [& syms-or-seqs] `(do (doseq [n# (keys (-/ns-imports *ns*))] (-/ns-unmap *ns* n#)) (-/import ~@syms-or-seqs)))

(import! [java.lang Error #_String Thread])

(defmacro refer! [ns s]
    (let [f #(let [v (-/ns-resolve (-/the-ns (if (= ns '-) 'clojure.core ns)) %) n (vary-meta % merge (-/select-keys (meta v) [:dynamic :macro :private]))] `(def ~n ~v))]
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
                                `(valAt [this# k# else#] (if-some [x# (case k# ~@s nil)] (-/aget ~a x#) else#))
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
(defmacro defr [r [& s]] (let [c (symbol (str r "'class"))] `(do (-/defrecord ~c [] ~r ~@s)                         ~c)))
(defmacro defm [r & s]   (let [i `(:on-interface ~r)]       `(do (extend-type ~i ~@s)                               ~i)))

(defmacro class! [r] (let [c (symbol (str r "'class"))] (-/list 'assoc (-/list 'new c) :class c)))

(defmacro throw! [#_"String" s] `(throw (Error. ~s)))

(refer! - [< <= > >= neg? pos? zero?])

(defn int! [n] (.intValue #_"Number" n))

(defn +
    ([] (int 0))
    ([x] (int! x))
    ([x y] (-/unchecked-add-int (int! x) (int! y)))
    ([x y & s] (-/reduce + (+ x y) s))
)

(def neg  -/unchecked-negate-int)
(def -    -/unchecked-subtract-int)
(def inc  -/unchecked-inc-int)
(def dec  -/unchecked-dec-int)
(def *    -/unchecked-multiply-int)
(def quot -/unchecked-divide-int)
(def rem  -/unchecked-remainder-int)

(defn bit-and [x y] (int! (-/bit-and x y)))
(defn bit-or  [x y] (int! (-/bit-or x y)))
(defn bit-xor [x y] (int! (-/bit-xor x y)))

(def & bit-and)
(def | bit-or)

(defn <<  [x y] (int! (-/bit-shift-left x y)))
(defn >>  [x y] (int! (-/bit-shift-right x y)))
(defn >>> [x y] (int! (-/unsigned-bit-shift-right (-/bit-and x 0xffffffff) y)))

(defn thread [] (Thread/currentThread))

(ns arbace.core
    (:refer-clojure :only [*ns* boolean case char compare defn fn identical? int let long loop satisfies?]) (:require [clojure.core :as -])
    (:refer arbace.bore :only [& * + - < << <= > >= >> >>> about bit-xor class! dec defm defp defq defr import! inc neg? pos? quot refer! rem thread throw! zero? |])
)

(import!
    [java.lang ArithmeticException Boolean Byte Character CharSequence Class Comparable Integer Long Number Object String StringBuilder System ThreadLocal Throwable Void]
    [java.io BufferedReader PushbackReader #_Reader #_StringReader StringWriter Writer]
    [java.lang.ref #_Reference ReferenceQueue WeakReference]
    [java.lang.reflect Array]
    [java.util Arrays Comparator]
    [java.util.regex Matcher Pattern]
    [jdk.vm.ci.hotspot CompilerToVM HotSpotJVMCIRuntime HotSpotVMConfig]
    [arbace.math BigInteger]
    [arbace.util.concurrent.atomic AtomicReference]
)

(let [last-id' (-/atom 0)] (defn next-id! [] (-/swap! last-id' inc)))

;;;
 ; Returns a new symbol with a unique name. If a prefix string is supplied,
 ; the name is prefix# where # is some unique number.
 ; If prefix is not supplied, the prefix is 'G__'.
 ;;
(defn gensym
    ([] (gensym "G__"))
    ([prefix] (-/symbol (-/str prefix (next-id!))))
)

;;;
 ; defs the supplied var names with no bindings, useful for making forward declarations.
 ;;
(defmacro declare [& names] `(do ~@(-/map #(-/list 'def (-/vary-meta % -/assoc :declared true)) names)))

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
    ([? then else] (-/list 'if ? else then))
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
    `(letfn* ~(-/vec (-/interleave (-/map -/first fnspecs) (-/map #(-/cons `fn %) fnspecs))) ~@body)
)

(letfn [(=> [s] (if (-/= '=> (-/first s)) (-/next s) (-/cons nil s)))]
    (defmacro     when       [? & s] (let [[e & s] (=> s)]                 `(if     ~? (do ~@s) ~e)))
    (defmacro     when-not   [? & s] (let [[e & s] (=> s)]                 `(if-not ~? (do ~@s) ~e)))
    (defmacro let-when     [v ? & s] (let [[e & s] (=> s)] `(let ~(-/vec v) (if     ~? (do ~@s) ~e))))
    (defmacro let-when-not [v ? & s] (let [[e & s] (=> s)] `(let ~(-/vec v) (if-not ~? (do ~@s) ~e))))
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
            (-/= 2 (-/count bind)) "exactly 2 forms in binding vector"
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
            (-/= 2 (-/count bind)) "exactly 2 forms in binding vector"
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
            (-/= 2 (-/count bind)) "exactly 2 forms in binding vector"
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
        (-/= 2 (-/count bindings)) "exactly 2 forms in binding vector"
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
        (-/= 2 (-/count bindings)) "exactly 2 forms in binding vector"
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
        (-/= 2 (-/count bindings)) "exactly 2 forms in binding vector"
    )
    `(when-some [s# (-/seq ~(bindings 1))]
        (let [~(bindings 0) (-/first s#)]
            ~@body
        )
    )
)

(letfn [(=> [s] (if (-/= '=> (-/first s)) (-/next s) (-/cons nil s)))]
    (defmacro when-let   [v & s] (let [[e & s] (=> s)] `(if-let   ~(-/vec v) (do ~@s) ~e)))
    (defmacro when-some  [v & s] (let [[e & s] (=> s)] `(if-some  ~(-/vec v) (do ~@s) ~e)))
    (defmacro when-first [v & s] (let [[e & s] (=> s)] `(if-first ~(-/vec v) (do ~@s) ~e)))
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
                (let [[[a b c :as clause] more] (-/split-at (if (-/= :>> (-/second args)) 3 2) args) n (-/count clause)]
                    (cond
                        (-/= 0 n) `(throw! (-/str "no matching clause: " ~expr))
                        (-/= 1 n) a
                        (-/= 2 n) `(if (~f? ~a ~expr)
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
        (=> [s] (if (-/= '=> (-/first s)) (-/next s) (-/cons nil s)))
        (l' [v ? r s] (let [r (r' r) [e & s] (=> s)] `(loop ~(v' v) (if ~? (do ~@s ~@r) ~e))))]
    (defmacro loop-when [v ? & s] (l' v ? nil s))
    (defmacro loop-when-recur [v ? r & s] (l' v ? r s))
)

(letfn [(r' [r] (cond (-/vector? r) `(recur ~@r) (some? r) `(recur ~r)))
        (=> [s] (if (-/= '=> (-/first s)) (-/second s)))]
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
        (-/= 2 (-/count bindings)) "exactly 2 forms in binding vector"
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
            ~@(-/map (fn [f] (-/with-meta (if (-/seq? f) `(~(-/first f) ~x' ~@(-/next f)) `(~f ~x')) (-/meta f))) s)
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
            (let-when [f (-/first s)] (-/seq? f) => (-/list f x)
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
            (let-when [f (-/first s)] (-/seq? f) => (-/list f x)
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

(defn int! [n] (.intValue #_"Number" n))

(about #_"arbace.Seqable"
    (defp Seqable
        (#_"seq" Seqable'''seq [#_"Seqable" this])
    )

    (-/extend-protocol Seqable clojure.lang.Seqable
        (Seqable'''seq [x] (.seq x))
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
)

(about #_"arbace.ISeq"
    (defp ISeq
        (#_"Object" ISeq'''first [#_"seq" this])
        (#_"seq" ISeq'''next [#_"seq" this])
    )

    (-/extend-protocol ISeq clojure.lang.ISeq
        (ISeq'''first [s] (.first s))
        (ISeq'''next [s] (.next s))
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
)

(about #_"arbace.IObject"
    (defp IObject
        (#_"boolean" IObject'''equals [#_"IObject" this, #_"Object" that])
        (#_"String" IObject'''toString [#_"IObject" this])
    )

    (-/extend-type Object IObject
        (#_"boolean" IObject'''equals [#_"Object" this, #_"Object" that] (.equals this, that))
        (#_"String" IObject'''toString [#_"Object" this] (.toString this))
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
)

(about #_"arbace.Counted"
    (defp Counted
        (#_"int" Counted'''count [#_"Counted" this])
    )

    (-/extend-protocol Counted
        clojure.lang.Counted                  (Counted'''count [o] (.count o))
      ; (Class/forName "[Ljava.lang.Object;") (Counted'''count [a] (Array/getLength a))
        CharSequence                          (Counted'''count [s] (.length s))
    )

    (-/extend-protocol Counted
        (Class/forName "[Ljava.lang.Object;") (Counted'''count [a] (Array/getLength a))
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
)

(about #_"arbace.Hashed"
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
        clojure.lang.Ratio   (Hashed'''hash [r] (.hashCode r))
        clojure.lang.IHashEq (Hashed'''hash [o] (.hasheq o))
    )

    (defn hashed? [x] (satisfies? Hashed x))

    ;;;
     ; Returns the hash code of its argument. Note this is the hash code
     ; consistent with =, and thus is different from .hashCode for Integer,
     ; Byte and Clojure collections.
     ;;
    (defn f'hash [x] (if (some? x) (Hashed'''hash x) (int 0)))

    (defn f'hashcode [x] (if (some? x) (.hashCode x) (int 0)))

    (defn hash-combine [seed x]
        ;; a la boost
        (bit-xor seed (+ (f'hashcode x) (int! 0x9e3779b9) (<< seed 6) (>> seed 2)))
    )
)

(about #_"arbace.IFn"
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

    (declare anew)

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
            ([this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"seq" args] (.invoke this, a1, a2, a3, a4, a5, a6, a7, a8, a9, (anew args)))
        )
        (IFn'''applyTo [this, args] (.applyTo this, args))
    )

    ;;;
     ; Returns true if x implements IFn.
     ; Note that many data structures (e.g. sets and maps) implement IFn.
     ;;
    (defn ifn? [x] (satisfies? IFn x))

    (declare cons)

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
)

(about #_"arbace.INamed"
    (defp INamed
        (#_"String" INamed'''getNamespace [#_"INamed" this])
        (#_"String" INamed'''getName [#_"INamed" this])
    )

    (-/extend-protocol INamed clojure.lang.Named
        (INamed'''getNamespace [this] (.getNamespace this))
        (INamed'''getName [this] (.getName this))
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
)

(about #_"arbace.IMeta"
    (defp IMeta
        (#_"meta" IMeta'''meta [#_"IMeta" this])
    )

    (-/extend-protocol IMeta clojure.lang.IMeta
        (IMeta'''meta [this] (.meta this))
    )

    ;;;
     ; Returns the metadata of obj, returns nil if there is no metadata.
     ;;
    (defn meta [x] (when (satisfies? IMeta x) (IMeta'''meta #_"IMeta" x)))
)

(about #_"arbace.IObj"
    (defp IObj
        (#_"IObj" IObj'''withMeta [#_"IObj" this, #_"meta" meta])
    )

    (-/extend-protocol IObj clojure.lang.IObj
        (IObj'''withMeta [this, meta] (.withMeta this, (-/into {} meta)))
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
)

(about #_"arbace.IReference"
    (defp IReference
        (#_"meta" IReference'''alterMeta [#_"IReference" this, #_"fn" f, #_"seq" args])
        (#_"meta" IReference'''resetMeta [#_"IReference" this, #_"meta" m])
    )

    (-/extend-protocol IReference clojure.lang.IReference
        (IReference'''alterMeta [this, f, args] (.alterMeta this, f, args))
        (IReference'''resetMeta [this, m] (.resetMeta this, m))
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
)

(about #_"arbace.IDeref"
    (defp IDeref
        (#_"Object" IDeref'''deref [#_"IDeref" this])
    )

    (-/extend-protocol IDeref clojure.lang.IDeref
        (IDeref'''deref [this] (.deref this))
    )

    ;;;
     ; When applied to a var or atom, returns its current state.
     ; When applied to a delay, forces it if not already forced.
     ; See also - realized?. Also reader macro: @.
     ;;
    (defn deref [#_"IDeref" ref] (IDeref'''deref ref))
)

(about #_"arbace.IAtom"
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

(about #_"arbace.IPending"
    (defp IPending
        (#_"boolean" IPending'''isRealized [#_"IPending" this])
    )

    (-/extend-protocol IPending clojure.lang.IPending
        (IPending'''isRealized [this] (.isRealized this))
    )

    ;;;
     ; Returns true if a value has been produced for a delay or lazy sequence.
     ;;
    (defn realized? [#_"IPending" x] (IPending'''isRealized x))
)

(about #_"arbace.Sequential"
    (defp Sequential)

    (-/extend-protocol Sequential clojure.lang.Sequential)

    (defn sequential? [x] (satisfies? Sequential x))
)

(about #_"arbace.Reversible"
    (defp Reversible
        (#_"seq" Reversible'''rseq [#_"Reversible" this])
    )

    (-/extend-protocol Reversible clojure.lang.Reversible
        (Reversible'''rseq [this] (.rseq this))
    )

    (defn reversible? [x] (satisfies? Reversible x))

    ;;;
     ; Returns, in constant time, a seq of the items in rev (which can be a vector or sorted-map), in reverse order.
     ; If rev is empty, returns nil.
     ;;
    (defn rseq [#_"Reversible" s] (Reversible'''rseq s))
)

(about #_"arbace.Sorted"
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

    (defn sorted? [x] (satisfies? Sorted x))
)

(about #_"arbace.Indexed"
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

    ;;;
     ; Return true if x implements Indexed, indicating efficient lookup by index.
     ;;
    (defn indexed? [x] (satisfies? Indexed x))

    ;;;
     ; Returns the nth next of coll, (seq coll) when n is 0.
     ;;
    (defn nthnext [s n] (loop-when-recur [s (seq s) n n] (and s (pos? n)) [(next s) (dec n)] => s))
)

(about #_"arbace.ILookup"
    (defp ILookup
        (#_"Object" ILookup'''valAt
            [#_"ILookup" this, #_"Object" key]
            [#_"ILookup" this, #_"Object" key, #_"Object" not-found]
        )
    )

    (§ -/extend-protocol ILookup clojure.lang.ILookup
        (ILookup'''valAt
            ([this, key] (.valAt this, key))
            ([this, key, not-found] (.valAt this, key, not-found))
        )
    )
)

(about #_"arbace.IPersistentCollection"
    (defp IPersistentCollection
        (#_"IPersistentCollection" IPersistentCollection'''conj [#_"IPersistentCollection" this, #_"Object" o])
        (#_"IPersistentCollection" IPersistentCollection'''empty [#_"IPersistentCollection" this])
    )

    (-/extend-protocol IPersistentCollection clojure.lang.IPersistentCollection
        (IPersistentCollection'''conj [this, o] (.cons this, o))
        (IPersistentCollection'''empty [this] (.empty this))
    )

    (defn coll? [x] (satisfies? IPersistentCollection x))

    (declare vector)
    (declare list)

    ;;;
     ; conj[oin].
     ; Returns a new collection with the items 'added'. (conj nil item) returns (item).
     ; The 'addition' may happen at different 'places' depending on the concrete type.
     ;;
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
)

(about #_"arbace.IEditableCollection"
    (defp IEditableCollection
        (#_"ITransientCollection" IEditableCollection'''asTransient [#_"IEditableCollection" this])
    )

    (-/extend-protocol IEditableCollection clojure.lang.IEditableCollection
        (IEditableCollection'''asTransient [this] (.asTransient this))
    )

    (defn editable? [x] (satisfies? IEditableCollection x))

    ;;;
     ; Returns a new, transient version of the collection, in constant time.
     ;;
    (defn transient [#_"IEditableCollection" coll] (IEditableCollection'''asTransient coll))
)

(about #_"arbace.IMapEntry"
    (defp IMapEntry
        (#_"Object" IMapEntry'''key [#_"IMapEntry" this])
        (#_"Object" IMapEntry'''val [#_"IMapEntry" this])
    )

    (-/extend-protocol IMapEntry clojure.lang.IMapEntry
        (IMapEntry'''key [this] (.key this))
        (IMapEntry'''val [this] (.val this))
    )

    (defn map-entry? [x] (satisfies? IMapEntry x))

    ;;;
     ; Returns the key/value of/in the map entry.
     ;;
    (defn key [#_"IMapEntry" e] (IMapEntry'''key e))
    (defn val [#_"IMapEntry" e] (IMapEntry'''val e))

    (declare map)

    ;;;
     ; Returns a sequence of the map's keys/values, in the same order as (seq m).
     ;;
    (defn keys [m] (not-empty (map key m)))
    (defn vals [m] (not-empty (map val m)))
)

(about #_"arbace.Associative"
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

    (defn associative? [x] (satisfies? Associative x))

    (declare PersistentArrayMap'new)

    ;;;
     ; assoc[iate].
     ; When applied to a map, returns a new map of the same (hashed/sorted) type,
     ; that contains the mapping of key(s) to val(s).
     ; When applied to a vector, returns a new vector that contains val at index.
     ; Note - index must be <= (count vector).
     ;;
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

    (declare get)

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
)

(about #_"arbace.IPersistentMap"
    (defp IPersistentMap
        (#_"IPersistentMap" IPersistentMap'''dissoc [#_"IPersistentMap" this, #_"Object" key])
    )

    (-/extend-protocol IPersistentMap clojure.lang.IPersistentMap
        (IPersistentMap'''dissoc [this, key] (.without this, key))
    )

    (defn map? [x] (satisfies? IPersistentMap x))

    ;;;
     ; dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
     ; that does not contain a mapping for key(s).
     ;;
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

(about #_"arbace.IPersistentSet"
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

    (defn set? [x] (satisfies? IPersistentSet x))

    ;;;
     ; disj[oin]. Returns a new set of the same (hashed/sorted) type,
     ; that does not contain key(s).
     ;;
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

(about #_"arbace.IPersistentStack"
    (defp IPersistentStack
        (#_"Object" IPersistentStack'''peek [#_"IPersistentStack" this])
        (#_"IPersistentStack" IPersistentStack'''pop [#_"IPersistentStack" this])
    )

    (-/extend-protocol IPersistentStack clojure.lang.IPersistentStack
        (IPersistentStack'''peek [this] (.peek this))
        (IPersistentStack'''pop [this] (.pop this))
    )

    (defn stack? [x] (satisfies? IPersistentStack x))

    ;;;
     ; For a list or queue, same as first, for a vector, same as, but much
     ; more efficient than, last. If the collection is empty, returns nil.
     ;;
    (defn peek [s]
        (when (some? s)
            (IPersistentStack'''peek s)
        )
    )

    ;;;
     ; Return a seq of all but the last item in coll, in linear time.
     ;;
    (defn butlast [s] (loop-when-recur [v (vector) s s] (next s) [(conj v (first s)) (next s)] => (seq v)))

    ;;;
     ; For a list or queue, returns a new list/queue without the first item,
     ; for a vector, returns a new vector without the last item.
     ; If the collection is empty, throws an exception.
     ; Note - not the same as next/butlast.
     ;;
    (defn pop [s]
        (when (some? s)
            (IPersistentStack'''pop s)
        )
    )
)

(about #_"arbace.IPersistentList"
    (defp IPersistentList)

    (-/extend-protocol IPersistentList clojure.lang.IPersistentList)

    (defn list? [x] (satisfies? IPersistentList x))
)

(about #_"arbace.IPersistentVector"
    (defp IPersistentVector
        (#_"IPersistentVector" IPersistentVector'''assocN [#_"IPersistentVector" this, #_"int" i, #_"Object" val])
        (#_"IPersistentVector" IPersistentVector'''slicev [#_"IPersistentVector" this, #_"int" start, #_"int" end])
        (#_"IPersistentVector" IPersistentVector'''splicev [#_"IPersistentVector" this, #_"IPersistentVector" that])
    )

    (-/extend-protocol IPersistentVector clojure.lang.IPersistentVector
        (IPersistentVector'''assocN [this, i, val] (.assocN this, i, val))
    )

    (defn vector? [x] (satisfies? IPersistentVector x))
)

(about #_"arbace.ITransientCollection"
    (defp ITransientCollection
        (#_"ITransientCollection" ITransientCollection'''conj! [#_"ITransientCollection" this, #_"Object" val])
        (#_"IPersistentCollection" ITransientCollection'''persistent! [#_"ITransientCollection" this])
    )

    (§ -/extend-protocol ITransientCollection clojure.lang.ITransientCollection
        (ITransientCollection'''conj! [this, val] (.conj this, val))
        (ITransientCollection'''persistent! [this] (.persistent this))
    )

    ;;;
     ; conj[oin].
     ; Adds x to the transient collection, and return c.
     ; The 'addition' may happen at different 'places' depending on the concrete type.
     ;;
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

    ;;;
     ; Returns a new, persistent version of the transient collection, in
     ; constant time. The transient collection cannot be used after this
     ; call, any such use will throw an exception.
     ;;
    (defn persistent! [#_"ITransientCollection" coll] (ITransientCollection'''persistent! coll))
)

(about #_"arbace.ITransientAssociative"
    (defp ITransientAssociative
        (#_"ITransientAssociative" ITransientAssociative'''assoc! [#_"ITransientAssociative" this, #_"Object" key, #_"Object" val])
        (#_"boolean" ITransientAssociative'''containsKey [#_"ITransientAssociative" this, #_"Object" key])
        (#_"IMapEntry" ITransientAssociative'''entryAt [#_"ITransientAssociative" this, #_"Object" key])
    )

    (§ -/extend-protocol ITransientAssociative clojure.lang.ITransientAssociative2
        (ITransientAssociative'''assoc! [this, key, val] (.assoc this, key, val))
        (ITransientAssociative'''containsKey [this, key] (.containsKey this, key))
        (ITransientAssociative'''entryAt [this, key] (.entryAt this, key))
    )

    ;;;
     ; assoc[iate].
     ; When applied to a transient map, adds mapping of key(s) to val(s).
     ; When applied to a transient vector, sets the val at index.
     ; Note - index must be <= (count vector). Returns coll.
     ;;
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

(about #_"arbace.ITransientMap"
    (defp ITransientMap
        (#_"ITransientMap" ITransientMap'''dissoc! [#_"ITransientMap" this, #_"Object" key])
    )

    (-/extend-protocol ITransientMap clojure.lang.ITransientMap
        (ITransientMap'''dissoc! [this, key] (.without this, key))
    )

    ;;;
     ; dissoc[iate]. Returns a transient map of the same (hashed/sorted) type,
     ; that doesn't contain a mapping for key(s).
     ;;
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

(about #_"arbace.ITransientSet"
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

    ;;;
     ; disj[oin]. Returns a transient set of the same (hashed/sorted) type,
     ; that does not contain key(s).
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
)

(about #_"arbace.ITransientVector"
    (defp ITransientVector
        (#_"ITransientVector" ITransientVector'''assocN! [#_"ITransientVector" this, #_"int" i, #_"Object" val])
        (#_"ITransientVector" ITransientVector'''pop! [#_"ITransientVector" this])
    )

    (-/extend-protocol ITransientVector clojure.lang.ITransientVector
        (ITransientVector'''assocN! [this, i, val] (.assocN this, i, val))
        (ITransientVector'''pop! [this] (.pop this))
    )

    ;;;
     ; Removes the last item from a transient vector.
     ; If the collection is empty, throws an exception. Returns coll.
     ;;
    (defn pop! [#_"ITransientVector" coll] (ITransientVector'''pop! coll))
)

(about #_"arbace.IReduce"
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

(about #_"arbace.IKVReduce"
    (defp IKVReduce
        (#_"Object" IKVReduce'''kvreduce [#_"IKVReduce" this, #_"fn" f, #_"Object" r])
    )

    (-/extend-protocol IKVReduce clojure.lang.IKVReduce
        (IKVReduce'''kvreduce [this, f, r] (.kvreduce this, f, r))
    )
)

(about #_"arbace.Range"
    (defp RangeBoundsCheck
        (#_"boolean" RangeBoundsCheck'''exceededBounds [#_"RangeBoundsCheck" this, #_"Object" val])
    )
)

(about #_"arbace.Ratio"
    (defp Ratio)

    ;;;
     ; Returns true if n is a Ratio.
     ;;
    (defn ratio? [n] (satisfies? Ratio n))

    ;;;
     ; Returns true if n is a rational number.
     ;;
    (defn rational? [n] (or (integer? n) (ratio? n)))
)

(about #_"arbace.Numbers"
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

(about #_"arbace.Atom"
    (defp Atom)
)

(about #_"arbace.AFn"
    #_abstract
    (defp AFn)
)

(about #_"arbace.Symbol"
    (defp Symbol)

    (-/extend-protocol Symbol clojure.lang.Symbol)

    (defn symbol? [x] (satisfies? Symbol x))
)

(about #_"arbace.Keyword"
    (defp Keyword)

    (-/extend-protocol Keyword clojure.lang.Keyword)

    (defn keyword? [x] (satisfies? Keyword x))
)

(about #_"arbace.Fn"
    #_abstract
    (defp Fn)

    (-/extend-protocol Fn clojure.lang.Fn)

    ;;;
     ; Returns true if x is an object created via fn.
     ;;
    (defn fn? [x] (satisfies? Fn x))
)

(about #_"arbace.Closure"
    #_abstract
    (defp Closure)
)

(about #_"arbace.ASeq"
    #_abstract
    (defp ASeq)
)

(about #_"arbace.LazySeq"
    (defp LazySeq)
)

(about #_"arbace.APersistentMap"
    #_abstract
    (defp APersistentMap)
)

(about #_"arbace.APersistentSet"
    #_abstract
    (defp APersistentSet)
)

(about #_"arbace.APersistentVector"
    (defp VSeq)
    (defp RSeq)
    #_abstract
    (defp APersistentVector)
)

(about #_"arbace.AMapEntry"
    #_abstract
    (defp AMapEntry)
)

(about #_"arbace.ArraySeq"
    (defp ArraySeq)
)

(about #_"arbace.ATransientMap"
    #_abstract
    (defp ATransientMap)
)

(about #_"arbace.ATransientSet"
    #_abstract
    (defp ATransientSet)
)

(about #_"arbace.Cons"
    (defp Cons)
)

(about #_"arbace.Delay"
    (defp Delay)
)

(about #_"arbace.Iterate"
    (defp Iterate)
)

(about #_"arbace.MapEntry"
    (defp MapEntry)
)

(about #_"arbace.Namespace"
    (defp Namespace)
)

(about #_"arbace.PersistentArrayMap"
    (defp MSeq)
    (defp TransientArrayMap)
    (defp PersistentArrayMap)
)

(about #_"arbace.PersistentHashMap"
    (defp INode
        (#_"INode" INode'''assoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissoc [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key])
        (#_"IMapEntry|Object" INode'''find
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key]
            [#_"INode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not-found]
        )
        (#_"seq" INode'''nodeSeq [#_"INode" this])
        (#_"INode" INode'''assocT [#_"INode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"boolean'" addedLeaf])
        (#_"INode" INode'''dissocT [#_"INode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"boolean'" removedLeaf])
        (#_"Object" INode'''kvreduce [#_"INode" this, #_"fn" f, #_"Object" r])
    )

    (defp HSeq)
    (defp NSeq)
    (defp TransientHashMap)
    (defp ANode)
    (defp BNode)
    (defp CNode)
    (defp PersistentHashMap)
)

(about #_"arbace.PersistentHashSet"
    (defp TransientHashSet)
    (defp PersistentHashSet)
)

(about #_"arbace.PersistentList"
    (defp EmptyList)
    (defp PersistentList)
)

(about #_"arbace.PersistentQueue"
    (defp QSeq)
    (defp PersistentQueue)
)

(about #_"arbace.PersistentTreeMap"
    (defp ITNode
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

(about #_"arbace.PersistentTreeSet"
    (defp PersistentTreeSet)
)

(about #_"arbace.PersistentVector"
    (defp VNode)
    (defp TransientVector)
    (defp PersistentVector)
)

(about #_"arbace.Repeat"
    (defp Repeat)
)

(about #_"arbace.Range"
    (defp Range)
)

(about #_"arbace.Reduced"
    (defp Reduced)

    (-/extend-protocol Reduced clojure.lang.Reduced)

    ;;;
     ; Returns true if x is the result of a call to reduced.
     ;;
    (defn reduced? [x] (satisfies? Reduced x))
)

(about #_"arbace.StringSeq"
    (defp StringSeq)
)

(about #_"arbace.Var"
    (defp Unbound)
    (defp Var)

    (-/extend-protocol Unbound clojure.lang.Var$Unbound)
    (-/extend-protocol Var clojure.lang.Var)

    ;;;
     ; Returns true if v is of type Var.
     ;;
    (defn var? [v] (satisfies? Var v))
)

(about #_"defarray"
    (refer! - [aget alength])

    (defn aclone [a]         (when (some? a) (-/aclone a)))
    (defn acopy! [a i b j n] (System/arraycopy b, j, a, i, n) a)
    (defn aset!  [a i x]     (-/aset a i x) a)
    (defn aswap! [a i f & s] (aset! a i (apply f (aget a i) s)))

    (defn anew [size-or-seq]
        (if (number? size-or-seq)
            (-/make-array Object (int! size-or-seq))
            (let [#_"seq" s (seq size-or-seq) #_"int" n (count s)]
                (loop-when-recur [#_"array" a (-/make-array Object n) #_"int" i 0 s s] (and (< i n) (some? s)) [(aset! a i (first s)) (inc i) (next s)] => a)
            )
        )
    )

    (defn- assoc!!
        ([a k v]    (.assoc a, k, v))
        ([a k v & kvs]
            (let [a (.assoc a, k, v)]
                (recur-when kvs [a (first kvs) (second kvs) (next (next kvs))] => a)
            )
        )
    )

    (defn- update!!
        ([a k f]         (.assoc a, k,       (f (.valAt a, k))))
        ([a k f x]       (.assoc a, k,       (f (.valAt a, k) x)))
        ([a k f x y]     (.assoc a, k,       (f (.valAt a, k) x y)))
        ([a k f x y & z] (.assoc a, k, (apply f (.valAt a, k) x y z)))
    )
)

(about #_"-/print-method"
    (defp SeqForm)
    (defp VecForm)
    (defp MapForm)
    (defp SetForm)
    (defp RecForm)

    (defn- print-sequential [#_"String" begin, f'p, #_"String" sep, #_"String" end, s'q, #_"Writer" w]
        (.write w, begin)
        (when-some [s (seq s'q)]
            (loop [[x & s] s]
                (f'p x w)
                (when s
                    (.write w, sep)
                    (recur s)
                )
            )
        )
        (.write w, end)
    )

    (defn- pr-on [#_"Object" o, #_"Writer" w]
        (-/print-method o w)
        nil
    )

    (-/defmethod -/print-method SeqForm'iface [#_"Seqable" q, #_"Writer" w]
        (print-sequential "(" pr-on " " ")" q w)
    )

    (-/defmethod -/print-method VecForm'iface [#_"Seqable" q, #_"Writer" w]
        (print-sequential "[" pr-on " " "]" q w)
    )

    (-/prefer-method -/print-method VecForm'iface clojure.lang.ISeq)
    (-/prefer-method -/print-method VecForm'iface clojure.lang.IPersistentVector)

    (-/defmethod -/print-method MapForm'iface [#_"Seqable" q, #_"Writer" w]
        (print-sequential "{" (fn [e #_"Writer" w] (pr-on (key e) w) (.append w \space) (pr-on (val e) w)) ", " "}" q w)
    )

    (-/prefer-method -/print-method MapForm'iface clojure.lang.ISeq)
    (-/prefer-method -/print-method MapForm'iface java.util.Map)

    (-/defmethod -/print-method SetForm'iface [#_"Seqable" q, #_"Writer" w]
        (print-sequential "#{" pr-on " " "}" q w)
    )

    (-/defmethod -/print-method RecForm'iface [#_"Seqable" q, #_"Writer" w]
        (print-sequential "[" pr-on " " "]" (.__data q) w)
    )

    (defn #_"String" print-string [#_"Object" o]
        (let [#_"StringWriter" w (StringWriter. (<< 1 5))]
            (pr-on o w)
            (.toString w)
        )
    )
)

(about #_"arbace.Murmur3"

;;;
 ; See http://smhasher.googlecode.com/svn/trunk/MurmurHash3.cpp
 ; MurmurHash3_x86_32
 ;
 ; @author Austin Appleby
 ; @author Dimitris Andreou
 ; @author Kurt Alfred Kluever
 ;;
(about #_"Murmur3"
    (def- #_"int" Murmur3'seed (int 0))
    (def- #_"int" Murmur3'C1 (int! 0xcc9e2d51))
    (def- #_"int" Murmur3'C2 (int! 0x1b873593))

    (defn- #_"int" Murmur3'mixK1 [#_"int" k1]
        (-> k1 (* Murmur3'C1) (Integer/rotateLeft 15) (* Murmur3'C2))
    )

    (defn- #_"int" Murmur3'mixH1 [#_"int" h1, #_"int" k1]
        (-> h1 (bit-xor k1) (Integer/rotateLeft 13) (* (int 5)) (+ (int! 0xe6546b64)))
    )

    ;; finalization mix - force all bits of a hash block to avalanche
    (defn- #_"int" Murmur3'fmix [#_"int" h1, #_"int" n]
        (let [h1 (bit-xor h1 n)    h1 (bit-xor h1 (>>> h1 16))
              h1 (* (int! h1) (int! 0x85ebca6b)) h1 (bit-xor h1 (>>> h1 13))
              h1 (* (int! h1) (int! 0xc2b2ae35)) h1 (bit-xor h1 (>>> h1 16))]
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
            (let [#_"int" low (int! input)
                  #_"int" high (int! (>>> input 32))
                  #_"int" k1 (Murmur3'mixK1 low)
                  #_"int" h1 (Murmur3'mixH1 Murmur3'seed, k1)
                  k1 (Murmur3'mixK1 high)
                  h1 (Murmur3'mixH1 h1, k1)]
                (Murmur3'fmix h1, (int 8))
            )
        )
    )

    (declare odd?)

    (defn #_"int" Murmur3'hashUnencodedChars [#_"CharSequence" s]
        (let [#_"int" h1 ;; step through the input 2 chars at a time
                (loop-when [h1 Murmur3'seed #_"int" i 1] (< i (.length s)) => h1
                    (let [#_"int" k1 (| (int (.charAt s, (dec i))) (<< (int (.charAt s, i)) 16))]
                        (recur (Murmur3'mixH1 h1, (Murmur3'mixK1 k1)) (+ i 2))
                    )
                )
              h1 ;; deal with any remaining characters
                (when (odd? (.length s)) => h1
                    (let [#_"int" k1 (int (.charAt s, (dec (.length s))))]
                        (bit-xor h1 (Murmur3'mixK1 k1))
                    )
                )]
            (Murmur3'fmix h1, (<< (.length s) 1))
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
)

(about #_"arbace.Atom"

(about #_"Atom"
    (declare Atom''deref)

    (defq Atom [#_"AtomicReference" meta, #_"AtomicReference" data]
        java.util.concurrent.Future (get [_] (Atom''deref _))
    )

    (defn #_"Atom" Atom'new
        ([#_"Object" data] (Atom'new nil, data))
        ([#_"meta" meta, #_"Object" data]
            (Atom'class. (anew [(AtomicReference. meta), (AtomicReference. data)]))
        )
    )

    (defn- #_"meta" Atom''meta [#_"Atom" this]
        (.get (:meta this))
    )

    (defn- #_"meta" Atom''alterMeta [#_"Atom" this, #_"fn" f, #_"seq" args]
        (loop []
            (let [#_"meta" m (.get (:meta this)) #_"meta" m' (apply f m args)]
                (when (.compareAndSet (:meta this), m, m') => (recur)
                    m'
                )
            )
        )
    )

    (defn- #_"meta" Atom''resetMeta [#_"Atom" this, #_"meta" m']
        (.set (:meta this), m')
        m'
    )

    (defn- #_"Object" Atom''deref [#_"Atom" this]
        (.get (:data this))
    )

    (defn- #_"boolean" Atom''compareAndSet [#_"Atom" this, #_"Object" o, #_"Object" o']
        (.compareAndSet (:data this), o, o')
    )

    (defn- #_"Object" Atom''swap [#_"Atom" this, #_"fn" f, #_"seq" args]
        (loop []
            (let [#_"Object" o (.get (:data this)) #_"Object" o' (apply f o args)]
                (when (.compareAndSet (:data this), o, o') => (recur)
                    o'
                )
            )
        )
    )

    (defn- #_"Object" Atom''reset [#_"Atom" this, #_"Object" o']
        (.set (:data this), o')
        o'
    )

    (defn- #_"[Object Object]" Atom''swapVals [#_"Atom" this, #_"fn" f, #_"seq" args]
        (loop []
            (let [#_"Object" o (.get (:data this)) #_"Object" o' (apply f o args)]
                (when (.compareAndSet (:data this), o, o') => (recur)
                    [o o']
                )
            )
        )
    )

    (defn- #_"[Object Object]" Atom''resetVals [#_"Atom" this, #_"Object" o']
        (loop []
            (let [#_"Object" o (.get (:data this))]
                (when (.compareAndSet (:data this), o, o') => (recur)
                    [o o']
                )
            )
        )
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
        (IAtom'''swapVals => Atom''swapVals)
        (IAtom'''resetVals => Atom''resetVals)
    )
)

(-/defmethod -/print-method Atom'iface [#_"Atom" a, #_"Writer" w]
    (.write w, (str (Atom''deref a) \'))
)

;;;
 ; Creates and returns an Atom with an initial value of x and optional meta m.
 ;;
(defn atom
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
(defn swap! [#_"IAtom" a f & args] (IAtom'''swap a, f, args))

;;;
 ; Sets the value of atom to x' without regard for the current value.
 ; Returns x'.
 ;;
(defn reset! [#_"IAtom" a x'] (IAtom'''reset a, x'))

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
)

(about #_"arbace.Delay"

(about #_"Delay"
    (defq Delay [#_"fn'" f, #_"Object'" o, #_"Throwable'" e])

    (defn #_"Delay" Delay'new [#_"fn" f]
        (Delay'class. (anew [(atom f), (atom nil), (atom nil)]))
    )

    (defn #_"Object" Delay'force [#_"Object" x]
        (if (satisfies? Delay x) (deref x) x)
    )

    (defn- #_"Object" Delay''deref [#_"Delay" this]
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

    (defn- #_"boolean" Delay''isRealized [#_"Delay" this]
        (locking this
            (nil? @(:f this))
        )
    )

    (defm Delay IDeref
        (IDeref'''deref => Delay''deref)
    )

    (defm Delay IPending
        (IPending'''isRealized => Delay''isRealized)
    )
)

;;;
 ; Takes a body of expressions and yields a Delay object that will invoke
 ; the body only the first time it is forced (with force or deref/@), and
 ; will cache the result and return it on all subsequent force calls.
 ; See also - realized?
 ;;
(defmacro delay [& body] `(Delay'new (fn* [] ~@body)))

;;;
 ; Returns true if x is a Delay created with delay.
 ;;
(defn delay? [x] (satisfies? Delay x))

;;;
 ; If x is a Delay, returns the (possibly cached) value of its expression, else returns x.
 ;;
(defn force [x] (Delay'force x))
)

(about #_"arbace.Reduced"

(about #_"Reduced"
    (defq Reduced [#_"Object" val])

    (defn #_"Reduced" Reduced'new [#_"Object" val]
        (Reduced'class. (anew [val]))
    )

    (defm Reduced IDeref
        (IDeref'''deref => :val)
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

(defn- preserving-reduced [f] #(let [r (f %1 %2)] (if (reduced? r) (reduced r) r)))

;; naïve reduce to be redefined later with IReduce

(defn reduce
    ([f s] (if-some [s (seq s)] (reduce f (first s) (next s)) (f)))
    ([f r s] (if-some [s (seq s)] (recur f (f r (first s)) (next s)) r))
)

(defn reduce!
    ([f s] (if-some [s (seq s)] (reduce! f (first s) (next s)) (f)))
    ([f r s] (persistent! (reduce f (transient r) s)))
)

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

(defn into [to from]
    (if (editable? to)
        (reduce! conj! to from)
        (reduce conj to from)
    )
)

;;;
 ; Returns a vector consisting of the result of applying f to the set of first
 ; items of each coll, followed by applying f to the set of second items in each
 ; coll, until any one of the colls is exhausted. Any remaining items in other
 ; colls are ignored. Function f should accept number-of-colls arguments.
 ;;
(defn mapv
    ([f coll] (reduce! #(conj! %1 (f %2)) (vector) coll))
    ([f c1 c2] (into (vector) (map f c1 c2)))
    ([f c1 c2 c3] (into (vector) (map f c1 c2 c3)))
    ([f c1 c2 c3 & colls] (into (vector) (apply map f c1 c2 c3 colls)))
)

;;;
 ; Returns a vector of the items in coll for which (f? item)
 ; returns logical true. f? must be free of side-effects.
 ;;
(defn filterv [f? s] (reduce! #(if (f? %2) (conj! %1 %2) %1) (vector) s))
)

(about #_"arbace.Util"

(about #_"Util"
    (declare Numbers'equal)
    (declare Symbol''equals)
    (declare Keyword''equals)

    (defn #_"boolean" Util'equiv [#_"Object" k1, #_"Object" k2]
        (cond
            (identical? k1 k2)              true
            (nil? k1)                       false
            (and (number? k1) (number? k2)) (Numbers'equal k1, k2)
            (coll? k1)                      (IObject'''equals k1, k2)
            (coll? k2)                      (IObject'''equals k2, k1)
            (instance? Symbol'iface k1)   (Symbol''equals k1, k2)
            (instance? Symbol'iface k2)   (Symbol''equals k2, k1)
            (instance? Keyword'iface k1)  (Keyword''equals k1, k2)
            (instance? Keyword'iface k2)  (Keyword''equals k2, k1)
            :else                           (IObject'''equals k1, k2)
        )
    )
)

;;;
 ; Equality. Returns true if x equals y, false if not. Same as Java x.equals(y) except it also
 ; works for nil, and compares numbers and collections in a type-independent manner. Arbace's
 ; immutable data structures define equals() (and thus =) as a value, not an identity, comparison.
 ;;
(defn =
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

(about #_"Util"
    (declare Numbers'compare)

    (defn #_"int" Util'compare [#_"Object" k1, #_"Object" k2]
        (cond
            (= k1 k2)    0
            (nil? k1)    -1
            (nil? k2)    1
            (number? k1) (Numbers'compare k1, k2)
            :else        (.compareTo #_"Comparable" k1, k2)
        )
    )
)

;;;
 ; Comparator. Returns a negative number, zero, or a positive number when x is logically
 ; 'less than', 'equal to', or 'greater than' y. Same as Java x.compareTo(y) except it also
 ; works for nil, and compares numbers and collections in a type-independent manner.
 ; x must implement Comparable.
 ;;
(§ defn compare [x y] (Util'compare x y))
)

(about #_"arbace.Ratio"

(about #_"Ratio"
    (declare Ratio''hashcode)

    (defq Ratio [#_"BigInteger" n, #_"BigInteger" d]
        java.lang.Object (hashCode [_] (Ratio''hashcode _))
    )

    (§ inherit Ratio #_"Number")

    (defn #_"Ratio" Ratio'new [#_"BigInteger" numerator, #_"BigInteger" denominator]
        (Ratio'class. (anew [numerator, denominator]))
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

    (defn- #_"int" Ratio''hashcode [#_"Ratio" this]
        (bit-xor (.hashCode (:n this)) (.hashCode (:d this)))
    )

    (defn- #_"boolean" Ratio''equals [#_"Ratio" this, #_"Object" that]
        (and (satisfies? Ratio that) (= (:n that) (:n this)) (= (:d that) (:d this)))
    )

    (defn- #_"String" Ratio''toString [#_"Ratio" this]
        (str (:n this) "/" (:d this))
    )

    (defm Ratio Hashed
        (Hashed'''hash => Ratio''hashcode)
    )

    (defm Ratio IObject
        (IObject'''equals => Ratio''equals)
        (IObject'''toString => Ratio''toString)
    )

    #_foreign
    (§ defm Ratio #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"Ratio" this, #_"Object" that]
            (Numbers'compare this, #_"Number" that)
        )
    )
)

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
)

(about #_"arbace.Numbers"

(about #_"LongOps"
    (defq LongOps [])

    (defn #_"LongOps" LongOps'new []
        (LongOps'class. (anew []))
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
    (defq RatioOps [])

    (defn #_"RatioOps" RatioOps'new []
        (RatioOps'class. (anew []))
    )

    (declare Numbers'toRatio)
    (declare Numbers'divide)
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
    (defq BigIntOps [])

    (defn #_"BigIntOps" BigIntOps'new []
        (BigIntOps'class. (anew []))
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
    (def #_"LongOps"   Numbers'LONG_OPS   (LongOps'new))
    (def #_"RatioOps"  Numbers'RATIO_OPS  (RatioOps'new))
    (def #_"BigIntOps" Numbers'BIGINT_OPS (BigIntOps'new))

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
        #_(-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''eq x, y)) (-/== x y)
    )

    (defn #_"boolean" Numbers'lt [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lt #_"Number" x, #_"Number" y))
    )

    (defn #_"boolean" Numbers'lte [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lte #_"Number" x, #_"Number" y))
    )

    (defn #_"boolean" Numbers'gt [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lt #_"Number" y, #_"Number" x))
    )

    (defn #_"boolean" Numbers'gte [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''lte #_"Number" y, #_"Number" x))
    )

    (defn #_"boolean" Numbers'isZero [#_"Object" x] (Ops'''isZero (Numbers'ops x), #_"Number" x))
    (defn #_"boolean" Numbers'isPos  [#_"Object" x] (Ops'''isPos  (Numbers'ops x), #_"Number" x))
    (defn #_"boolean" Numbers'isNeg  [#_"Object" x] (Ops'''isNeg  (Numbers'ops x), #_"Number" x))

    (defn #_"Number" Numbers'add [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''add #_"Number" x, #_"Number" y))
    )

    (defn #_"Number" Numbers'subtract [#_"Object" x, #_"Object" y]
        (let [#_"Number" negativeY (Ops'''negate (Numbers'ops y), #_"Number" y)]
            (-> (Ops'''combine (Numbers'ops x), (Numbers'ops negativeY)) (Ops'''add #_"Number" x, negativeY))
        )
    )

    (defn #_"Number" Numbers'negate [#_"Object" x] (Ops'''negate (Numbers'ops x), #_"Number" x))
    (defn #_"Number" Numbers'inc    [#_"Object" x] (Ops'''inc    (Numbers'ops x), #_"Number" x))
    (defn #_"Number" Numbers'dec    [#_"Object" x] (Ops'''dec    (Numbers'ops x), #_"Number" x))

    (defn #_"Number" Numbers'multiply [#_"Object" x, #_"Object" y]
        (-> (Ops'''combine (Numbers'ops x), (Numbers'ops y)) (Ops'''multiply #_"Number" x, #_"Number" y))
    )

    (defn #_"Number" Numbers'divide [#_"Object" x, #_"Object" y]
        (let-when-not [#_"Ops" yops (Numbers'ops y)] (Ops'''isZero yops, #_"Number" y) => (throw (ArithmeticException. "Divide by zero"))
            (-> (Ops'''combine (Numbers'ops x), yops) (Ops'''divide #_"Number" x, #_"Number" y))
        )
    )

    (defn #_"Number" Numbers'quotient [#_"Object" x, #_"Object" y]
        (let-when-not [#_"Ops" yops (Numbers'ops y)] (Ops'''isZero yops, #_"Number" y) => (throw (ArithmeticException. "Divide by zero"))
            (-> (Ops'''combine (Numbers'ops x), yops) (Ops'''quotient #_"Number" x, #_"Number" y))
        )
    )

    (defn #_"Number" Numbers'remainder [#_"Object" x, #_"Object" y]
        (let-when-not [#_"Ops" yops (Numbers'ops y)] (Ops'''isZero yops, #_"Number" y) => (throw (ArithmeticException. "Divide by zero"))
            (-> (Ops'''combine (Numbers'ops x), yops) (Ops'''remainder #_"Number" x, #_"Number" y))
        )
    )

    (defn #_"BigInteger" Numbers'toBigInteger [#_"Object" x]
        (condp instance? x
            BigInteger x
                       (BigInteger/valueOf (.longValue #_"Number" x))
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
    ([x] #_"Number" x)
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

(defn abs [a] (if (neg? a) (-/- a) a))

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
    ([x] #_"Number" x)
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
        (zero? (& n 1))
    )
)

;;;
 ; Returns true if n is odd, throws an exception if n is not an integer.
 ;;
(defn odd? [n] (not (even? n)))
)

(about #_"arbace.AFn"

(about #_"AFn"
    (defn #_"void" AFn'throwArity [#_"fn" f, #_"int" n]
        (throw! (str "wrong number of args (" (if (neg? n) (str "more than " (dec (-/- n))) n) ") passed to: " (class f)))
    )

    (defn #_"Object" AFn'applyTo [#_"fn" f, #_"seq" s]
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

(about #_"arbace.Symbol"

(about #_"Symbol"
    (declare Symbol''withMeta Symbol''hash Symbol''equals)

    (defq Symbol [#_"meta" _meta, #_"String" ns, #_"String" name]
        clojure.lang.IMeta (meta [_] (-/into {} (:_meta _)))
        clojure.lang.IObj (withMeta [_, m] (Symbol''withMeta _, m))
        clojure.lang.IHashEq (hasheq [_] (Symbol''hash _))
        java.lang.Object (equals [_, o] (Symbol''equals _, o)) (hashCode [_] (hash-combine (.hashCode (:name _)) (:ns _)))
    )

    #_inherit
    (defm Symbol AFn)

    (defn- #_"Symbol" Symbol'new
        ([#_"String" ns, #_"String" name] (Symbol'new nil, ns, name))
        ([#_"meta" meta, #_"String" ns, #_"String" name]
            (Symbol'class. (anew [meta, ns, name]))
        )
    )

    (defn- #_"Symbol" Symbol''withMeta [#_"Symbol" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Symbol'new meta, (:ns this), (:name this))
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

    (defn- #_"boolean" Symbol''equals [#_"Symbol" this, #_"Object" that]
        (or (identical? this that)
            (and (symbol? that) (= (:ns this) (#_:ns namespace that)) (= (:name this) (#_:name name that)))
        )
    )

    (defn- #_"String" Symbol''toString [#_"Symbol" this]
        (if (some? (:ns this)) (str (:ns this) "/" (:name this)) (:name this))
    )

    (defn- #_"int" Symbol''hash [#_"Symbol" this]
        (hash-combine (Murmur3'hashUnencodedChars (:name this)) (:ns this))
    )

    (defn- #_"Object" Symbol''invoke
        ([#_"Symbol" this, #_"Object" obj] (get obj this))
        ([#_"Symbol" this, #_"Object" obj, #_"Object" not-found] (get obj this not-found))
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
        (IObject'''toString => Symbol''toString)
    )

    (defm Symbol Hashed
        (Hashed'''hash => Symbol''hash)
    )

    (defm Symbol IFn
        (IFn'''invoke => Symbol''invoke)
        (IFn'''applyTo => AFn'applyTo)
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
)

(-/defmethod -/print-method Symbol'iface [#_"Symbol" s, #_"Writer" w]
    (.write w, (str s))
)

;;;
 ; Returns a Symbol with the given namespace and name.
 ;;
(defn symbol
    ([name] (if (symbol? name) name (Symbol'intern name)))
    ([ns name] (Symbol'intern ns, name))
)

(defn- symbol! [s] (symbol (if (instance? clojure.lang.Symbol s) (name s) s)))
)

(about #_"arbace.Keyword"

(about #_"Keyword"
    (declare Keyword''equals Keyword''invoke)

    (defq Keyword [#_"Symbol" sym, #_"int" _hash]
        clojure.lang.IHashEq (hasheq [_] (:_hash _))
        java.lang.Object (equals [_, o] (Keyword''equals _, o)) (hashCode [_] (+ (.hashCode (:sym _)) (int! 0x9e3779b9)))
        clojure.lang.IFn (invoke [_, a] (Keyword''invoke _, a))
    )

    #_inherit
    (defm Keyword AFn)

    (def- #_"{Symbol Reference<Keyword>}'" Keyword'cache (atom (-/hash-map)))
    (def- #_"ReferenceQueue" Keyword'queue (ReferenceQueue.))

    (defn- #_"Keyword" Keyword'new [#_"Symbol" sym]
        (Keyword'class. (anew [sym, (+ (f'hash sym) (int! 0x9e3779b9))]))
    )

    (declare Cache'purge)

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

    (defn- #_"String" Keyword''getNamespace [#_"Keyword" this]
        (INamed'''getNamespace (:sym this))
    )

    (defn- #_"String" Keyword''getName [#_"Keyword" this]
        (INamed'''getName (:sym this))
    )

    (defn- #_"boolean" Keyword''equals [#_"Keyword" this, #_"Object" that]
        (or (identical? this that)
            (and (instance? clojure.lang.Keyword that) (Symbol''equals (:sym this), (.sym that)))
        )
    )

    (defn- #_"String" Keyword''toString [#_"Keyword" this]
        (str ":" (:sym this))
    )

    (defn- #_"Object" Keyword''invoke
        ([#_"Keyword" this, #_"Object" obj] (get obj this))
        ([#_"Keyword" this, #_"Object" obj, #_"Object" not-found] (get obj this not-found))
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
        (IObject'''toString => Keyword''toString)
    )

    (defm Keyword IFn
        (IFn'''invoke => Keyword''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    #_foreign
    (§ defm Keyword #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"Keyword" this, #_"Keyword" that]
            (compare (:sym this) (:sym that))
        )
    )
)

(-/defmethod -/print-method Keyword'iface [#_"Keyword" k, #_"Writer" w]
    (.write w, (str k))
)

;;;
 ; Returns a Keyword with the given namespace and name.
 ; Do not use ":" in the keyword strings, it will be added automatically.
 ;;
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

(defn- keyword! [k] (keyword (if (instance? clojure.lang.Keyword k) (name k) k)))

;;;
 ; Returns a Keyword with the given namespace and name if one already exists.
 ; This function will not intern a new keyword. If the keyword has not already
 ; been interned, it will return nil.
 ; Do not use ":" in the keyword strings, it will be added automatically.
 ;;
(defn find-keyword
    ([name]
        (cond
            (keyword? name) name
            (symbol? name) (Keyword'find #_"Symbol" name)
            (string? name) (Keyword'find (symbol #_"String" name))
        )
    )
    ([ns name] (Keyword'find (symbol ns name)))
)
)

(about #_"arbace.Fn"

(about #_"Fn"
    (defq Fn [])

    #_inherit
    (defm Fn AFn)

    (defn #_"Fn" Fn'new []
        (Fn'class. (anew []))
    )

    (defn- #_"Object" Fn''invoke
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

    (defm Fn IFn
        (IFn'''invoke => Fn''invoke)
        (IFn'''applyTo => AFn'applyTo)
    )

    #_foreign
    (§ defm Fn #_"Comparator"
        (#_"int" Comparator'''compare [#_"Fn" this, #_"Object" o1, #_"Object" o2]
            (let [#_"Object" o (IFn'''invoke this, o1, o2)]
                (if (boolean? o)
                    (cond (boolean o) -1 (boolean (IFn'''invoke this, o2, o1)) 1 :else 0)
                    (int! o)
                )
            )
        )
    )
)
)

(about #_"arbace.Closure"

(about #_"Closure"
    (defq Closure [#_"meta" _meta, #_"FnExpr" fun, #_"map" env])

    #_inherit
    (defm Closure Fn AFn)

    (defn #_"Closure" Closure'new
        ([#_"FnExpr" fun, #_"map" env] (Closure'new nil, fun, env))
        ([#_"meta" meta, #_"FnExpr" fun, #_"map" env]
            (Closure'class. (anew [meta, fun, env]))
        )
    )

    (defn- #_"Closure" Closure''withMeta [#_"Closure" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Closure'new meta, (:fun this), (:env this))
        )
    )

    (defn- #_"Object" Closure''invoke
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

    (declare Compiler'MAX_POSITIONAL_ARITY)

    (defn- #_"Object" Closure''applyTo [#_"Closure" this, #_"seq" args]
        (let [
            #_"int" m (inc Compiler'MAX_POSITIONAL_ARITY) #_"int" n (min (count args m) m)
            #_"FnMethod" fm
                (or (get (:regulars (:fun this)) n)
                    (let-when [fm (:variadic (:fun this))] (and (some? fm) (<= (dec (-/- (:arity fm))) n)) => (AFn'throwArity this, (if (< n m) n (-/- m)))
                        fm
                    )
                )
        ]
            fm
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

    (§ defm Closure #_"Comparator"
        ;; inherit Fn compare
    )
)
)

(about #_"arbace.ASeq"

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

(about #_"arbace.Cons"

(about #_"Cons"
    (declare Cons''withMeta Cons''seq Cons''next Cons''count)

    (defq Cons [#_"meta" _meta, #_"Object" car, #_"seq" cdr] #_"SeqForm"
        clojure.lang.IMeta (meta [_] (-/into {} (:_meta _)))
        clojure.lang.IObj (withMeta [_, m] (Cons''withMeta _, m))
        clojure.lang.ISeq (seq [_] (Cons''seq _)) (first [_] (:car _)) (next [_] (Cons''next _)) (more [_] (or (.next _) ()))
        clojure.lang.IPersistentCollection (count [_] (Cons''count _))
        clojure.lang.Sequential
    )

    #_inherit
    (defm Cons ASeq)

    (defn #_"Cons" Cons'new
        ([#_"Object" car, #_"seq" cdr] (Cons'new nil, car, cdr))
        ([#_"meta" meta, #_"Object" car, #_"seq" cdr]
            (Cons'class. (anew [meta, car, cdr]))
        )
    )

    (defn- #_"Cons" Cons''withMeta [#_"Cons" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Cons'new meta, (:car this), (:cdr this))
        )
    )

    (defn- #_"seq" Cons''seq [#_"Cons" this]
        this
    )

    (defn- #_"seq" Cons''next [#_"Cons" this]
        (seq (:cdr this))
    )

    (defn- #_"int" Cons''count [#_"Cons" this]
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
        (IObject'''toString => print-string)
    )
)

;;;
 ; Returns a new seq where x is the first element and s is the rest.
 ;;
(defn cons [x s] (Cons'new x, (seq s)))
)

(about #_"arbace.Iterate"

(about #_"Iterate"
    (declare Iterate''seq Iterate''first Iterate''next)

    (defq Iterate [#_"meta" _meta, #_"fn" f, #_"Object" x, #_"Object'" y] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (Iterate''seq _)) (first [_] (Iterate''first _)) (next [_] (Iterate''next _))
    )

    #_inherit
    (defm Iterate ASeq)

    (defn- #_"Iterate" Iterate'new
        ([#_"fn" f, #_"Object" x, #_"Object" y] (Iterate'new nil, f, x, y))
        ([#_"meta" meta, #_"fn" f, #_"Object" x, #_"Object" y]
            (Iterate'class. (anew [meta, f, x, (atom y)])) ;; f never nil ;; y lazily realized
        )
    )

    (defn- #_"Iterate" Iterate''withMeta [#_"Iterate" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Iterate'new meta, (:f this), (:x this), @(:y this))
        )
    )

    (defn #_"seq" Iterate'create [#_"fn" f, #_"Object" y] (Iterate'new f, nil, y))

    (def- #_"Object" Iterate'UNREALIZED (Object.))

    (defn- #_"boolean" Iterate''isRealized [#_"Iterate" this]
        (not (identical? @(:y this) Iterate'UNREALIZED))
    )

    (defn- #_"seq" Iterate''seq [#_"Iterate" this]
        this
    )

    (defn- #_"Object" Iterate''first [#_"Iterate" this]
        (let-when [#_"Object" y @(:y this)] (identical? y Iterate'UNREALIZED) => y
            (reset! (:y this) ((:f this) (:x this)))
        )
    )

    #_memoize!
    (defn- #_"seq" Iterate''next [#_"Iterate" this]
        (Iterate'new (:f this), (first this), Iterate'UNREALIZED)
    )

    (defn- #_"Object" Iterate''reduce
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
        (IObject'''toString => print-string)
    )
)

;;;
 ; Returns a lazy sequence of x, (f x), (f (f x)), etc.
 ; f must be free of side-effects.
 ;;
(defn iterate [f x] (Iterate'create f x))
)

(about #_"arbace.Repeat"

(about #_"Repeat"
    (declare Repeat''seq Repeat''next)

    (defq Repeat [#_"meta" _meta, #_"long" cnt, #_"Object" val] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (Repeat''seq _)) (first [_] (:val _)) (next [_] (Repeat''next _))
    )

    #_inherit
    (defm Repeat ASeq)

    (def- #_"long" Repeat'INFINITE -1)

    (defn- #_"Repeat" Repeat'new
        ([#_"long" cnt, #_"Object" val] (Repeat'new nil, cnt, val))
        ([#_"meta" meta, #_"long" cnt, #_"Object" val]
            (Repeat'class. (anew [meta, cnt, val])) ;; cnt always INFINITE or pos?
        )
    )

    (defn- #_"Repeat" Repeat''withMeta [#_"Repeat" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Repeat'new meta, (:cnt this), (:val this))
        )
    )

    (declare list)

    (defn #_"Repeat|ISeq" Repeat'create
        ([#_"Object" val] (Repeat'new Repeat'INFINITE, val))
        ([#_"long" n, #_"Object" val] (if (pos? n) (Repeat'new n, val) (list)))
    )

    (defn- #_"seq" Repeat''seq [#_"Repeat" this]
        this
    )

    (defn- #_"seq" Repeat''next [#_"Repeat" this]
        (cond
            (< 1 (:cnt this))               (Repeat'new (dec (:cnt this)), (:val this))
            (= (:cnt this) Repeat'INFINITE) this
        )
    )

    (defn- #_"Object" Repeat''reduce
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
        (IObject'''toString => print-string)
    )
)

;;;
 ; Returns a lazy (infinite!, or length n if supplied) sequence of xs.
 ;;
(defn repeat
    ([  x] (Repeat'create   x))
    ([n x] (Repeat'create n x))
)
)

(about #_"arbace.Range"

;;;
 ; Implements generic numeric (potentially infinite) range.
 ;;
(about #_"Range"
    (declare Range''seq Range''next)

    (defq Range [#_"meta" _meta, #_"Object" start, #_"Object" end, #_"Object" step, #_"RangeBoundsCheck" boundsCheck] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (Range''seq _)) (first [_] (:start _)) (next [_] (Range''next _)) (more [_] (or (.next _) ()))
    )

    #_inherit
    (defm Range ASeq)

    #_abstract
    (defm Range Counted)

    (defn- #_"RangeBoundsCheck" Range'positiveStep [#_"Object" end]
        (-/reify RangeBoundsCheck
            (#_"boolean" RangeBoundsCheck'''exceededBounds [#_"RangeBoundsCheck" _self, #_"Object" val]
                (<= end val)
            )
        )
    )

    (defn- #_"RangeBoundsCheck" Range'negativeStep [#_"Object" end]
        (-/reify RangeBoundsCheck
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
            ;; invariants guarantee this is never an "empty" seq
            (Range'class. (anew [meta, start, end, step, boundsCheck]))
        )
    )

    (defn- #_"Range" Range''withMeta [#_"Range" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (Range'new meta, (:end this), (:start this), (:step this), (:boundsCheck this))
        )
    )

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

    (defn- #_"seq" Range''seq [#_"Range" this]
        this
    )

    (defn- #_"seq" Range''next [#_"Range" this]
        (let-when-not [#_"Object" n (+ (:start this) (:step this))] (RangeBoundsCheck'''exceededBounds (:boundsCheck this), n)
            (Range'new n, (:end this), (:step this), (:boundsCheck this))
        )
    )

    (defn- #_"Object" Range''reduce
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
        (IObject'''toString => print-string)
    )
)

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
)

(about #_"arbace.ArraySeq"

(about #_"ArraySeq"
    (declare ArraySeq''seq ArraySeq''first ArraySeq''next)

    (defq ArraySeq [#_"meta" _meta, #_"array" a, #_"int" i] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (ArraySeq''seq _)) (first [_] (ArraySeq''first _)) (next [_] (ArraySeq''next _))
    )

    #_inherit
    (defm ArraySeq ASeq)

    (defn #_"ArraySeq" ArraySeq'new
        ([#_"array" a, #_"int" i] (ArraySeq'new nil, a, i))
        ([#_"meta" meta, #_"array" a, #_"int" i]
            (ArraySeq'class. (anew [meta, a, i]))
        )
    )

    (defn- #_"ArraySeq" ArraySeq''withMeta [#_"ArraySeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (ArraySeq'new meta, (:a this), (:i this))
        )
    )

    (defn #_"ArraySeq" ArraySeq'create [#_"array" a]
        (when (and (some? a) (pos? (alength a)))
            (ArraySeq'new a, 0)
        )
    )

    (-/extend-protocol Seqable (Class/forName "[Ljava.lang.Object;")
        (#_"ArraySeq" Seqable'''seq [#_"array" a] (ArraySeq'create a))
    )

    (defn- #_"seq" ArraySeq''seq [#_"ArraySeq" this]
        this
    )

    (defn- #_"Object" ArraySeq''first [#_"ArraySeq" this]
        (when (some? (:a this))
            (aget (:a this) (:i this))
        )
    )

    (defn- #_"seq" ArraySeq''next [#_"ArraySeq" this]
        (when (and (some? (:a this)) (< (inc (:i this)) (count (:a this))))
            (ArraySeq'new (:a this), (inc (:i this)))
        )
    )

    (defn- #_"int" ArraySeq''count [#_"ArraySeq" this]
        (if (some? (:a this)) (- (count (:a this)) (:i this)) 0)
    )

    (defn- #_"Object" ArraySeq''reduce
        ([#_"ArraySeq" this, #_"fn" f]
            (when-some [#_"array" a (:a this)]
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
            (when-some [#_"array" a (:a this)]
                (let [#_"int" i (:i this) #_"int" n (count a)]
                    (loop-when [r (f r (aget a i)) i (inc i)] (< i n) => (if (reduced? r) @r r)
                        (if (reduced? r) @r (recur (f r (aget a i)) (inc i)))
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
        (IObject'''toString => print-string)
    )
)
)

(about #_"arbace.StringSeq"

(about #_"StringSeq"
    (declare StringSeq''seq StringSeq''first StringSeq''next)

    (defq StringSeq [#_"meta" _meta, #_"CharSequence" s, #_"int" i] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (StringSeq''seq _)) (first [_] (StringSeq''first _)) (next [_] (StringSeq''next _))
    )

    #_inherit
    (defm StringSeq ASeq)

    (defn- #_"StringSeq" StringSeq'new [#_"meta" meta, #_"CharSequence" s, #_"int" i]
        (StringSeq'class. (anew [meta, s, i]))
    )

    (defn- #_"StringSeq" StringSeq''withMeta [#_"StringSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (StringSeq'new meta, (:s this), (:i this))
        )
    )

    (defn #_"StringSeq" StringSeq'create [#_"CharSequence" s]
        (when (pos? (.length s))
            (StringSeq'new nil, s, 0)
        )
    )

    (-/extend-protocol Seqable CharSequence
        (#_"StringSeq" Seqable'''seq [#_"CharSequence" s] (StringSeq'create s))
    )

    (defn- #_"seq" StringSeq''seq [#_"StringSeq" this]
        this
    )

    (defn- #_"Object" StringSeq''first [#_"StringSeq" this]
        (Character/valueOf (.charAt (:s this), (:i this)))
    )

    (defn- #_"seq" StringSeq''next [#_"StringSeq" this]
        (when (< (inc (:i this)) (count (:s this)))
            (StringSeq'new (:_meta this), (:s this), (inc (:i this)))
        )
    )

    (defn- #_"int" StringSeq''count [#_"StringSeq" this]
        (- (count (:s this)) (:i this))
    )

    (defn- #_"Object" StringSeq''reduce
        ([#_"StringSeq" this, #_"fn" f]
            (let [#_"CharSequence" s (:s this) #_"int" i (:i this) #_"int" n (count s)]
                (loop-when [#_"Object" r (.charAt s, i) i (inc i)] (< i n) => r
                    (let [r (f r (.charAt s, i))]
                        (if (reduced? r) @r (recur r (inc i)))
                    )
                )
            )
        )
        ([#_"StringSeq" this, #_"fn" f, #_"Object" r]
            (let [#_"CharSequence" s (:s this) #_"int" i (:i this) #_"int" n (count s)]
                (loop-when [r (f r (.charAt s, i)) i (inc i)] (< i n) => (if (reduced? r) @r r)
                    (if (reduced? r) @r (recur (f r (.charAt s, i)) (inc i)))
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
        (IObject'''toString => print-string)
    )
)
)

(about #_"arbace.LazySeq"

(about #_"LazySeq"
    (declare LazySeq''seq LazySeq''first LazySeq''next)

    (defq LazySeq [#_"meta" _meta, #_"fn'" f, #_"Object'" o, #_"seq'" s] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (LazySeq''seq _)) (first [_] (LazySeq''first _)) (next [_] (LazySeq''next _)) (more [_] (or (.next _) ()))
        clojure.lang.Sequential
    )

    (defn- #_"LazySeq" LazySeq'init [#_"meta" meta, #_"fn" f, #_"seq" s]
        (LazySeq'class. (anew [meta, (atom f), (atom nil), (atom s)]))
    )

    (defn- #_"LazySeq" LazySeq'new
        ([#_"fn" f]                 (LazySeq'init nil,  f,   nil))
        ([#_"meta" meta, #_"seq" s] (LazySeq'init meta, nil, s  ))
    )

    (defn- #_"LazySeq" LazySeq''withMeta [#_"LazySeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (LazySeq'new meta, (seq this))
        )
    )

    (defn- #_"IPersistentCollection" LazySeq''empty [#_"LazySeq" this]
        (list)
    )

    (defn- #_"seq" LazySeq''seq [#_"LazySeq" this]
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

    (defn- #_"Object" LazySeq''first [#_"LazySeq" this]
        (when-some [#_"seq" s (seq this)]
            (first s)
        )
    )

    (defn- #_"seq" LazySeq''next [#_"LazySeq" this]
        (when-some [#_"seq" s (seq this)]
            (next s)
        )
    )

    (defn- #_"boolean" LazySeq''equals [#_"LazySeq" this, #_"Object" that]
        (if-some [#_"seq" s (seq this)]
            (= s that)
            (and (sequential? that) (nil? (seq that)))
        )
    )

    (defn- #_"boolean" LazySeq''isRealized [#_"LazySeq" this]
        (locking this
            (nil? @(:f this))
        )
    )

    (defm LazySeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm LazySeq IObj
        (IObj'''withMeta => LazySeq''withMeta)
    )

    (defm LazySeq IPersistentCollection
        ;; abstract IPersistentCollection conj
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
        ;; abstract IObject toString
    )

    (defm LazySeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm LazySeq IPending
        (IPending'''isRealized => LazySeq''isRealized)
    )
)

;;;
 ; Takes a body of expressions that returns an ISeq or nil, and yields
 ; a Seqable object that will invoke the body only the first time seq
 ; is called, and will cache the result and return it on all subsequent
 ; seq calls. See also - realized?
 ;;
(defmacro lazy-seq [& body] `(LazySeq'new (fn* [] ~@body)))

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
                ([] (reduce #(conj %1 (%2)) (vector) fs))
                ([x] (reduce #(conj %1 (%2 x)) (vector) fs))
                ([x y] (reduce #(conj %1 (%2 x y)) (vector) fs))
                ([x y & z] (reduce #(conj %1 (apply %2 x y z)) (vector) fs))
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
 ; Returns the result of applying concat to the result of applying map to f and colls.
 ; Thus function f should return a collection.
 ; Returns a transducer when no collections are provided.
 ;;
(defn mapcat
    ([f] (comp (map f) cat))
    ([f & s] (apply concat (apply map f s)))
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
                        (when-some [y (f (swap! i' inc) x)] => s
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
                            (when-some [y (f i (first s))] => (keepi- (inc i) (next s))
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
            (let [v' (atom (vector))]
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
 ; Applies f to each value in coll, splitting it each time f returns
 ; a new value. Returns a lazy seq of partitions. Returns a stateful
 ; transducer when no collection is provided.
 ;;
(defn partition-by
    ([f]
        (fn [g]
            (let [l' (atom (vector)) p' (atom ::none)]
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
 ; Takes a function of no args, presumably with side effects, and returns
 ; an infinite (or length n if supplied) lazy sequence of calls to it.
 ;;
(defn repeatedly
    ([f] (lazy-seq (cons (f) (repeatedly f))))
    ([n f] (take n (repeatedly f)))
)

(declare hash-set)
(declare contains?)

;;;
 ; Returns a lazy sequence of the elements of coll with duplicates removed.
 ; Returns a stateful transducer when no collection is provided.
 ;;
(defn distinct
    ([]
        (fn [g]
            (let [seen (atom (hash-set))]
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
            (step- s (hash-set))
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
)

(about #_"arbace.APersistentMap"

(about #_"APersistentMap"
    (declare nth)

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
        ([#_"APersistentMap" this, #_"Object" key] (get this key))
        ([#_"APersistentMap" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.APersistentSet"

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
        ([#_"APersistentSet" this, #_"Object" key] (get this key))
        ([#_"APersistentSet" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.APersistentVector"

(about #_"VSeq"
    (declare VSeq''seq VSeq''first VSeq''next)

    (defq VSeq [#_"meta" _meta, #_"vector" v, #_"int" i] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (VSeq''seq _)) (first [_] (VSeq''first _)) (next [_] (VSeq''next _)) (more [_] (or (.next _) ()))
        clojure.lang.Sequential
    )

    #_inherit
    (defm VSeq ASeq)

    (defn #_"VSeq" VSeq'new
        ([#_"vector" v, #_"int" i] (VSeq'new nil, v, i))
        ([#_"meta" meta, #_"vector" v, #_"int" i]
            (VSeq'class. (anew [meta, v, i]))
        )
    )

    (defn- #_"VSeq" VSeq''withMeta [#_"VSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (VSeq'new meta, (:v this), (:i this))
        )
    )

    (defn- #_"seq" VSeq''seq [#_"VSeq" this]
        this
    )

    (defn- #_"Object" VSeq''first [#_"VSeq" this]
        (nth (:v this) (:i this))
    )

    (defn- #_"seq" VSeq''next [#_"VSeq" this]
        (when (< (inc (:i this)) (count (:v this)))
            (VSeq'new (:v this), (inc (:i this)))
        )
    )

    (defn- #_"int" VSeq''count [#_"VSeq" this]
        (- (count (:v this)) (:i this))
    )

    (defn- #_"Object" VSeq''reduce
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
        (IObject'''toString => print-string)
    )
)

(about #_"RSeq"
    (declare RSeq''seq RSeq''first RSeq''next)

    (defq RSeq [#_"meta" _meta, #_"vector" v, #_"int" i] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (RSeq''seq _)) (first [_] (RSeq''first _)) (next [_] (RSeq''next _))
    )

    #_inherit
    (defm RSeq ASeq)

    (defn #_"RSeq" RSeq'new
        ([#_"vector" v, #_"int" i] (RSeq'new nil, v, i))
        ([#_"meta" meta, #_"vector" v, #_"int" i]
            (RSeq'class. (anew [meta, v, i]))
        )
    )

    (defn- #_"RSeq" RSeq''withMeta [#_"RSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (RSeq'new meta, (:v this), (:i this))
        )
    )

    (defn- #_"seq" RSeq''seq [#_"RSeq" this]
        this
    )

    (defn- #_"Object" RSeq''first [#_"RSeq" this]
        (nth (:v this) (:i this))
    )

    (defn- #_"seq" RSeq''next [#_"RSeq" this]
        (when (pos? (:i this))
            (RSeq'new (:v this), (dec (:i this)))
        )
    )

    (defn- #_"int" RSeq''count [#_"RSeq" this]
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
        (IObject'''toString => print-string)
    )
)
)

(about #_"arbace.AMapEntry"

(about #_"AMapEntry"
    (defn #_"Object" AMapEntry''nth
        ([#_"AMapEntry" this, #_"int" i]
            (case i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) (throw! "index is out of bounds"))
        )
        ([#_"AMapEntry" this, #_"int" i, #_"Object" not-found]
            (case i 0 (IMapEntry'''key this) 1 (IMapEntry'''val this) not-found)
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

(about #_"arbace.MapEntry"

(about #_"MapEntry"
    (defq MapEntry [#_"key" k, #_"value" v] VecForm
        java.util.Map$Entry (getKey [_] (:k _)) (getValue [_] (:v _))
    )

    #_inherit
    (defm MapEntry AMapEntry APersistentVector AFn)

    (defn- #_"MapEntry" MapEntry'new [#_"key" k, #_"value" v]
        (MapEntry'class. (anew [k, v]))
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
        (IObject'''toString => print-string)
    )

    (defm MapEntry Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    #_foreign
    (§ defm MapEntry #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)
)

(about #_"arbace.ATransientMap"

(about #_"ATransientMap"
    (defn #_"Object" ATransientMap''invoke
        ([#_"ATransientMap" this, #_"Object" key] (get this key))
        ([#_"ATransientMap" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
    )

    (def- #_"Object" ATransientMap'NOT_FOUND (Object.))

    (defn #_"boolean" ATransientMap''containsKey [#_"ATransientMap" this, #_"Object" key]
        (not (identical? (get this key ATransientMap'NOT_FOUND) ATransientMap'NOT_FOUND))
    )

    (defn #_"IMapEntry" ATransientMap''entryAt [#_"ATransientMap" this, #_"Object" key]
        (let [#_"Object" v (get this key ATransientMap'NOT_FOUND)]
            (when-not (identical? v ATransientMap'NOT_FOUND)
                (MapEntry'new key, v)
            )
        )
    )
)
)

(about #_"arbace.ATransientSet"

(about #_"ATransientSet"
    (defn #_"Object" ATransientSet''invoke
        ([#_"ATransientSet" this, #_"Object" key] (get this key))
        ([#_"ATransientSet" this, #_"Object" key, #_"Object" not-found] (get this key not-found))
    )
)
)

(about #_"arbace.PersistentList"

(about #_"EmptyList"
    (declare EmptyList''seq EmptyList''first EmptyList''next EmptyList''conj EmptyList''empty)

    (defq EmptyList [#_"meta" _meta] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (EmptyList''seq _)) (first [_] (EmptyList''first _)) (next [_] (EmptyList''next _))
        clojure.lang.IPersistentCollection (cons [_ o] (EmptyList''conj _, o)) (empty [_] (EmptyList''empty _))
    )

    (defn #_"EmptyList" EmptyList'new [#_"meta" meta]
        (EmptyList'class. (anew [meta]))
    )

    (defn- #_"EmptyList" EmptyList''withMeta [#_"EmptyList" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (EmptyList'new meta)
        )
    )

    (def #_"int" EmptyList'HASH (Murmur3'hashOrdered nil))

    (defn- #_"int" EmptyList''hash [#_"EmptyList" this]
        EmptyList'HASH
    )

    (defn- #_"boolean" EmptyList''equals [#_"EmptyList" this, #_"Object" that]
        (and (sequential? that) (nil? (seq that)))
    )

    (defn- #_"String" EmptyList''toString [#_"EmptyList" this]
        "()"
    )

    (defn- #_"seq" EmptyList''seq [#_"EmptyList" this]
        nil
    )

    (defn- #_"Object" EmptyList''first [#_"EmptyList" this]
        nil
    )

    (defn- #_"seq" EmptyList''next [#_"EmptyList" this]
        nil
    )

    (defn- #_"int" EmptyList''count [#_"EmptyList" this]
        0
    )

    (declare PersistentList'new)

    (defn- #_"PersistentList" EmptyList''conj [#_"EmptyList" this, #_"Object" o]
        (PersistentList'new (:_meta this), o, nil, 1)
    )

    (defn- #_"EmptyList" EmptyList''empty [#_"EmptyList" this]
        this
    )

    (defn- #_"Object" EmptyList''peek [#_"EmptyList" this]
        nil
    )

    (defn- #_"IPersistentList" EmptyList''pop [#_"EmptyList" this]
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
        (IObject'''toString => EmptyList''toString)
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
    (declare PersistentList''seq PersistentList''conj PersistentList''empty)

    (defq PersistentList [#_"meta" _meta, #_"Object" car, #_"IPersistentList" cdr, #_"int" cnt] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (PersistentList''seq _)) (first [_] (:car _)) (next [_] (:cdr _)) (more [_] (or (.next _) ()))
        clojure.lang.IPersistentCollection (cons [_ o] (PersistentList''conj _, o)) (empty [_] (PersistentList''empty _)) (equiv [_, o] (ASeq''equals _, o)) (count [_] (:cnt _))
    )

    #_inherit
    (defm PersistentList ASeq)

    (defn #_"PersistentList" PersistentList'new
        ([#_"Object" car] (PersistentList'new nil, car, nil, 1))
        ([#_"meta" meta, #_"Object" car, #_"IPersistentList" cdr, #_"int" cnt]
            (PersistentList'class. (anew [meta, car, cdr, cnt]))
        )
    )

    (def #_"EmptyList" PersistentList'EMPTY (EmptyList'new nil))

    (declare reverse)

    (defn #_"PersistentList" PersistentList'create [#_"Reversible" init]
        (into PersistentList'EMPTY (if (satisfies? Reversible init) (rseq init) (reverse init)))
    )

    (defn- #_"PersistentList" PersistentList''withMeta [#_"PersistentList" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentList'new meta, (:car this), (:cdr this), (:cnt this))
        )
    )

    (defn- #_"seq" PersistentList''seq [#_"PersistentList" this]
        this
    )

    (defn- #_"PersistentList" PersistentList''conj [#_"PersistentList" this, #_"Object" o]
        (PersistentList'new (:_meta this), o, this, (inc (:cnt this)))
    )

    (defn- #_"PersistentList" PersistentList''empty [#_"PersistentList" this]
        (with-meta PersistentList'EMPTY (:_meta this))
    )

    (defn- #_"IPersistentList" PersistentList''pop [#_"PersistentList" this]
        (or (:cdr this) (with-meta PersistentList'EMPTY (:_meta this)))
    )

    (defn- #_"Object" PersistentList''reduce
        ([#_"PersistentList" this, #_"fn" f]
            (loop-when [#_"Object" r (:car this) #_"IPersistentList" l (:cdr this)] (some? l) => r
                (let [r (f r (:car l))]
                    (if (reduced? r) @r (recur r (:cdr l)))
                )
            )
        )
        ([#_"PersistentList" this, #_"fn" f, #_"Object" r]
            (loop-when [r (f r (:car this)) #_"IPersistentList" l (:cdr this)] (some? l) => (if (reduced? r) @r r)
                (if (reduced? r) @r (recur (f r (:car l)) (:cdr l)))
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
        (IObject'''toString => print-string)
    )
)

(defn list
    ([] PersistentList'EMPTY)
    ([& s] (PersistentList'create s))
)

;;;
 ; Returns a seq of the items in coll in reverse order. Not lazy.
 ;;
(defn reverse [s] (into (list) s))
)

(about #_"arbace.PersistentArrayMap"

(about #_"MSeq"
    (declare MSeq''seq MSeq''first MSeq''next)

    (defq MSeq [#_"meta" _meta, #_"array" a, #_"int" i] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (MSeq''seq _)) (first [_] (MSeq''first _)) (next [_] (MSeq''next _))
    )

    #_inherit
    (defm MSeq ASeq)

    (defn #_"MSeq" MSeq'new
        ([#_"array" a, #_"int" i] (MSeq'new nil, a, i))
        ([#_"meta" meta, #_"array" a, #_"int" i]
            (MSeq'class. (anew [meta, a, i]))
        )
    )

    (defn- #_"MSeq" MSeq''withMeta [#_"MSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (MSeq'new meta, (:a this), (:i this))
        )
    )

    (defn- #_"seq" MSeq''seq [#_"MSeq" this]
        this
    )

    (defn- #_"pair" MSeq''first [#_"MSeq" this]
        (MapEntry'new (aget (:a this) (:i this)), (aget (:a this) (inc (:i this))))
    )

    (defn- #_"seq" MSeq''next [#_"MSeq" this]
        (when (< (+ (:i this) 2) (alength (:a this)))
            (MSeq'new (:a this), (+ (:i this) 2))
        )
    )

    (defn- #_"int" MSeq''count [#_"MSeq" this]
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
        (IObject'''toString => print-string)
    )
)

(about #_"TransientArrayMap"
    (defq TransientArrayMap [#_"thread'" edit, #_"array" array, #_"int" cnt] MapForm)

    #_inherit
    (defm TransientArrayMap ATransientMap AFn)

    (declare PersistentArrayMap'HASHTABLE_THRESHOLD)

    (defn #_"TransientArrayMap" TransientArrayMap'new [#_"array" a]
        (let [#_"int" n (alength a) #_"int" m (max PersistentArrayMap'HASHTABLE_THRESHOLD n)]
            (TransientArrayMap'class. (anew [(atom (thread)), (-> (anew m) (acopy! 0 a 0 n)), n]))
        )
    )

    (defn- #_"void" TransientArrayMap''assert-editable [#_"TransientArrayMap" this]
        (or @(:edit this) (throw! "transient used after persistent! call"))
        nil
    )

    (defn- #_"int" TransientArrayMap''count [#_"TransientArrayMap" this]
        (TransientArrayMap''assert-editable this)
        (quot (:cnt this) 2)
    )

    (defn- #_"int" TransientArrayMap'index-of [#_"array" a, #_"int" n, #_"key" key]
        (loop-when [#_"int" i 0] (< i n) => -1
            (if (= (aget a i) key) i (recur (+ i 2)))
        )
    )

    (defn- #_"value" TransientArrayMap''valAt
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

    (declare PersistentHashMap'create-1a)

    (defn- #_"ITransientMap" TransientArrayMap''assoc! [#_"TransientArrayMap" this, #_"key" key, #_"value" val]
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
                        (assoc!! this :cnt (+ n 2))
                    )
                :else
                    (-> (PersistentHashMap'create-1a a) (transient) (assoc! key val))
            )
        )
    )

    (defn- #_"ITransientMap" TransientArrayMap''dissoc! [#_"TransientArrayMap" this, #_"key" key]
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
                    (assoc!! this :cnt n)
                )
            )
        )
    )

    (defn- #_"ITransientMap" TransientArrayMap''conj! [#_"TransientArrayMap" this, #_"pair" o]
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

    (defn- #_"IPersistentMap" TransientArrayMap''persistent! [#_"TransientArrayMap" this]
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

;;;
 ; Simple implementation of persistent map on an array.
 ;
 ; Note that instances of this class are constant values, i.e. add/remove etc return new values.
 ; Copies array on every change, so only appropriate for _very_small_ maps. nil keys and values are
 ; ok, but you won't be able to distinguish a nil value via valAt, use contains/entryAt for that.
 ;;
(about #_"PersistentArrayMap"
    (defq PersistentArrayMap [#_"meta" _meta, #_"array" array] MapForm)

    #_inherit
    (defm PersistentArrayMap APersistentMap AFn)

    (defn #_"PersistentArrayMap" PersistentArrayMap'new
        ;; This ctor captures/aliases the passed array, so do not modify it later.
        ([#_"array" a] (PersistentArrayMap'new nil, a))
        ([#_"meta" meta, #_"array" a]
            (PersistentArrayMap'class. (anew [meta, (or a (anew 0))]))
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
        ;; If this looks like it is doing busy-work, it is because it is achieving these goals: O(n^2) run time
        ;; like createWithCheck(), never modify init arg, and only allocate memory if there are duplicate keys.
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
                    ;; Create a new, shorter array with unique keys, and the last value associated with each key.
                    ;; To behave like assoc, the first occurrence of each key must be used, since its metadata
                    ;; may be different than later equal keys.
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

    (defn- #_"PersistentArrayMap" PersistentArrayMap''withMeta [#_"PersistentArrayMap" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentArrayMap'new meta, (:array this))
        )
    )

    (defn- #_"int" PersistentArrayMap''count [#_"PersistentArrayMap" this]
        (quot (alength (:array this)) 2)
    )

    (defn- #_"int" PersistentArrayMap'index-of [#_"array" a, #_"key" key]
        (loop-when [#_"int" i 0] (< i (alength a)) => -1
            (if (= (aget a i) key) i (recur (+ i 2)))
        )
    )

    (defn- #_"value" PersistentArrayMap''valAt
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

    (defn- #_"IPersistentMap" PersistentArrayMap''assoc [#_"PersistentArrayMap" this, #_"key" key, #_"value" val]
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

    (defn- #_"boolean" PersistentArrayMap''containsKey [#_"PersistentArrayMap" this, #_"key" key]
        (< -1 (PersistentArrayMap'index-of (:array this), key))
    )

    (defn- #_"pair" PersistentArrayMap''entryAt [#_"PersistentArrayMap" this, #_"key" key]
        (let [
            #_"array" a (:array this) #_"int" i (PersistentArrayMap'index-of a, key)
        ]
            (when (< -1 i)
                (MapEntry'new (aget a i), (aget a (inc i)))
            )
        )
    )

    (defn- #_"IPersistentMap" PersistentArrayMap''dissoc [#_"PersistentArrayMap" this, #_"key" key]
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

    (defn- #_"IPersistentMap" PersistentArrayMap''empty [#_"PersistentArrayMap" this]
        (with-meta PersistentArrayMap'EMPTY (:_meta this))
    )

    (defn- #_"seq" PersistentArrayMap''seq [#_"PersistentArrayMap" this]
        (when (pos? (alength (:array this)))
            (MSeq'new (:array this), 0)
        )
    )

    (defn- #_"value" PersistentArrayMap''kvreduce [#_"PersistentArrayMap" this, #_"fn" f, #_"value" r]
        (let [#_"array" a (:array this) #_"int" n (alength a)]
            (loop-when [r r #_"int" i 0] (< i n) => r
                (let [r (f r (aget a i), (aget a (inc i)))]
                    (when-not (reduced? r) => @r
                        (recur r (+ i 2))
                    )
                )
            )
        )
    )

    (defn- #_"ITransientMap" PersistentArrayMap''asTransient [#_"PersistentArrayMap" this]
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

    (defm PersistentArrayMap IKVReduce
        (IKVReduce'''kvreduce => PersistentArrayMap''kvreduce)
    )

    (defm PersistentArrayMap IEditableCollection
        (IEditableCollection'''asTransient => PersistentArrayMap''asTransient)
    )

    (defm PersistentArrayMap IObject
        (IObject'''equals => APersistentMap''equals)
        (IObject'''toString => print-string)
    )

    (defm PersistentArrayMap Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

;;;
 ; Constructs an array-map.
 ; If any keys are equal, they are handled as if by repeated uses of assoc.
 ;;
(defn array-map
    ([] PersistentArrayMap'EMPTY)
    ([& keyvals] (PersistentArrayMap'createAsIfByAssoc (anew keyvals)))
)
)

(about #_"arbace.PersistentHashMap"

(about #_"HSeq"
    (declare HSeq''seq HSeq''first HSeq''next)

    (defq HSeq [#_"meta" _meta, #_"node[]" nodes, #_"int" i, #_"seq" s] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (HSeq''seq _)) (first [_] (HSeq''first _)) (next [_] (HSeq''next _))
    )

    #_inherit
    (defm HSeq ASeq)

    (defn- #_"HSeq" HSeq'new [#_"meta" meta, #_"node[]" nodes, #_"int" i, #_"seq" s]
        (HSeq'class. (anew [meta, nodes, i, s]))
    )

    (defn- #_"HSeq" HSeq''withMeta [#_"HSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (HSeq'new meta, (:nodes this), (:i this), (:s this))
        )
    )

    (defn- #_"seq" HSeq'create-4 [#_"meta" meta, #_"node[]" nodes, #_"int" i, #_"seq" s]
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

    (defn- #_"seq" HSeq''seq [#_"HSeq" this]
        this
    )

    (defn- #_"pair" HSeq''first [#_"HSeq" this]
        (first (:s this))
    )

    (defn- #_"seq" HSeq''next [#_"HSeq" this]
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
        (IObject'''toString => print-string)
    )
)

(about #_"NSeq"
    (declare NSeq''seq NSeq''first NSeq''next)

    (defq NSeq [#_"meta" _meta, #_"array" a, #_"int" i, #_"seq" s] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (NSeq''seq _)) (first [_] (NSeq''first _)) (next [_] (NSeq''next _)) (more [_] (or (.next _) ()))
    )

    #_inherit
    (defm NSeq ASeq)

    (defn #_"NSeq" NSeq'new
        ([#_"array" a, #_"int" i] (NSeq'new nil, a, i, nil))
        ([#_"meta" meta, #_"array" a, #_"int" i, #_"seq" s]
            (NSeq'class. (anew [meta, a, i, s]))
        )
    )

    (defn- #_"NSeq" NSeq''withMeta [#_"NSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (NSeq'new meta, (:a this), (:i this), (:s this))
        )
    )

    (defn- #_"seq" NSeq'create-3 [#_"array" a, #_"int" i, #_"seq" s]
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

    (defn- #_"seq" NSeq''seq [#_"NSeq" this]
        this
    )

    (defn- #_"pair" NSeq''first [#_"NSeq" this]
        (if (some? (:s this))
            (first (:s this))
            (MapEntry'new (aget (:a this) (:i this)), (aget (:a this) (inc (:i this))))
        )
    )

    (defn- #_"seq" NSeq''next [#_"NSeq" this]
        (if (some? (:s this))
            (NSeq'create-3 (:a this), (:i this), (next (:s this)))
            (NSeq'create-3 (:a this), (+ (:i this) 2), nil)
        )
    )

    (defn #_"value" NSeq'kvreduce [#_"array" a, #_"fn" f, #_"value" r]
        (loop-when [r r #_"int" i 0] (< i (alength a)) => r
            (let [#_"key" k (aget a i) #_"value|node" v (aget a (inc i))
                  r (cond
                        (some? k) (f r k v)
                        (some? v) (INode'''kvreduce v, f, r)
                        :else     r
                    )]
                (when-not (reduced? r) => r
                    (recur r (+ i 2))
                )
            )
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
        (IObject'''toString => print-string)
    )
)

(about #_"PersistentHashMap"
    (defn- #_"int" PersistentHashMap'mask [#_"int" hash, #_"int" shift]
        (& (>>> hash shift) 0x1f)
    )

    (defn- #_"int" PersistentHashMap'bitpos [#_"int" hash, #_"int" shift]
        (int! (<< 1 (PersistentHashMap'mask hash, shift)))
    )

    (defn- #_"array" PersistentHashMap'cloneAndSet
        ([#_"array" a, #_"int" i, #_"Object" x]                          (-> (aclone a) (aset! i x)))
        ([#_"array" a, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y] (-> (aclone a) (aset! i x) (aset! j y)))
    )

    (defn- #_"array" PersistentHashMap'removePair [#_"array" a, #_"int" i]
        (let [#_"int" n (- (alength a) 2) #_"int" m (* 2 i)]
            (-> (anew n) (acopy! 0 a 0 m) (acopy! m a (+ m 2) (- n m)))
        )
    )
)

(about #_"ANode"
    (defq ANode [#_"thread'" edit, #_"int" n, #_"node[]" a])

    (defn #_"ANode" ANode'new [#_"thread'" edit, #_"int" n, #_"node[]" a]
        (ANode'class. (anew [edit, n, a]))
    )

    (defn- #_"ANode" ANode''ensureEditable [#_"ANode" this, #_"thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (ANode'new edit, (:n this), (aclone (:a this)))
        )
    )

    (defn- #_"ANode" ANode''editAndSet [#_"ANode" this, #_"thread'" edit, #_"int" i, #_"node" node]
        (let [#_"ANode" e (ANode''ensureEditable this, edit)]
            (aset! (:a e) i node)
            e
        )
    )

    (declare BNode'new)

    (defn- #_"node" ANode''pack [#_"ANode" this, #_"thread'" edit, #_"int" idx]
        (let [#_"array" a' (anew (* 2 (dec (:n this))))
              [#_"int" bitmap #_"int" j]
                (loop-when [bitmap 0 j 1 #_"int" i 0] (< i idx) => [bitmap j]
                    (let [[bitmap j]
                            (when-some [#_"node" ai (aget (:a this) i)] => [bitmap j]
                                (aset! a' j ai)
                                [(| bitmap (<< 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )
              bitmap
                (loop-when [bitmap bitmap j j #_"int" i (inc idx)] (< i (alength (:a this))) => bitmap
                    (let [[bitmap j]
                            (when-some [#_"node" ai (aget (:a this) i)] => [bitmap j]
                                (aset! a' j ai)
                                [(| bitmap (<< 1 i)) (+ j 2)]
                            )]
                        (recur bitmap j (inc i))
                    )
                )]
            (BNode'new edit, bitmap, a')
        )
    )

    (declare BNode'EMPTY)

    (defn- #_"node" ANode''assoc [#_"ANode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
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

    (defn- #_"node" ANode''dissoc [#_"ANode" this, #_"int" shift, #_"int" hash, #_"key" key]
        (let-when [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)] (some? ai) => this
            (let-when-not [#_"node" node (INode'''dissoc ai, (+ shift 5), hash, key)] (= node ai) => this
                (cond
                    (some? node)     (ANode'new nil, (:n this), (PersistentHashMap'cloneAndSet (:a this), i, node))
                    (<= (:n this) 8) (ANode''pack this, nil, i) ;; shrink
                    :else            (ANode'new nil, (dec (:n this)), (PersistentHashMap'cloneAndSet (:a this), i, node))
                )
            )
        )
    )

    (defn- #_"IMapEntry|value" ANode''find
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

    (defn- #_"seq" ANode''nodeSeq [#_"ANode" this]
        (HSeq'create-1 (:a this))
    )

    (defn- #_"node" ANode''assocT [#_"ANode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)]
            (if (some? ai)
                (let [#_"node" node (INode'''assocT ai, edit, (+ shift 5), hash, key, val, addedLeaf)]
                    (when-not (= node ai) => this
                        (ANode''editAndSet this, edit, i, node)
                    )
                )
                (let [#_"node" node (INode'''assocT BNode'EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf)]
                    (-> (ANode''editAndSet this, edit, i, node) (update!! :n inc))
                )
            )
        )
    )

    (defn- #_"node" ANode''dissocT [#_"ANode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf]
        (let-when [#_"int" i (PersistentHashMap'mask hash, shift) #_"node" ai (aget (:a this) i)] (some? ai) => this
            (let-when-not [#_"node" node (INode'''dissocT ai, edit, (+ shift 5), hash, key, removedLeaf)] (= node ai) => this
                (cond
                    (some? node)     (ANode''editAndSet this, edit, i, node)
                    (<= (:n this) 8) (ANode''pack this, edit, i) ;; shrink
                    :else            (-> (ANode''editAndSet this, edit, i, node) (update!! :n dec))
                )
            )
        )
    )

    (defn- #_"value" ANode''kvreduce [#_"ANode" this, #_"fn" f, #_"value" r]
        (loop-when [r r #_"int" i 0] (< i (alength (:a this))) => r
            (when-some [#_"node" node (aget (:a this) i)] => (recur r (inc i))
                (let [r (INode'''kvreduce node, f, r)]
                    (when-not (reduced? r) => r
                        (recur r (inc i))
                    )
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
        (INode'''kvreduce => ANode''kvreduce)
    )
)

(about #_"BNode"
    (defq BNode [#_"thread'" edit, #_"int" bitmap, #_"array" a])

    (defn #_"BNode" BNode'new [#_"thread'" edit, #_"int" bitmap, #_"array" a]
        (BNode'class. (anew [edit, bitmap, a]))
    )

    (def #_"BNode" BNode'EMPTY (BNode'new nil, 0, (anew 0)))

    (defn- #_"int" BNode'index [#_"int" bitmap, #_"int" bit]
        (Integer/bitCount (& bitmap (dec bit)))
    )

    (declare CNode'new)

    (defn- #_"node" BNode'create [#_"int" shift, #_"key" key1, #_"value" val1, #_"int" hash2, #_"key" key2, #_"value" val2]
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

    (defn- #_"BNode" BNode''ensureEditable [#_"BNode" this, #_"thread'" edit]
        (when-not (identical? (:edit this) edit) => this
            (let [#_"int" b (:bitmap this) #_"int" n (Integer/bitCount b) #_"int" m (inc n)] ;; make room for next assoc
                (BNode'new edit, b, (-> (anew (* 2 m)) (acopy! 0 (:a this) 0 (* 2 n))))
            )
        )
    )

    (defn- #_"BNode" BNode''editAndSet
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

    (defn- #_"BNode" BNode''editAndRemovePair [#_"BNode" this, #_"thread'" edit, #_"int" bit, #_"int" i]
        (when-not (= (:bitmap this) bit)
            (let [
                #_"BNode" e (-> (BNode''ensureEditable this, edit) (update!! :bitmap bit-xor bit))
                #_"array" a (:a e) #_"int" n (alength a) #_"int" m (* 2 (inc i))
            ]
                (acopy! a (* 2 i) a m (- n m))
                (aset! a (- n 2) nil)
                (aset! a (- n 1) nil)
                e
            )
        )
    )

    (defn- #_"node" BNode'createT [#_"thread'" edit, #_"int" shift, #_"key" key1, #_"value" val1, #_"int" hash2, #_"key" key2, #_"value" val2]
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

    (defn- #_"node" BNode''assoc [#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" bit (PersistentHashMap'bitpos hash, shift) #_"int" x (BNode'index (:bitmap this), bit)]
            (if-not (zero? (& (:bitmap this) bit))
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
                (let [#_"int" n (Integer/bitCount (:bitmap this))]
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
                            (BNode'new nil, (| (:bitmap this) bit), a')
                        )
                    )
                )
            )
        )
    )

    (defn- #_"node" BNode''dissoc [#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key]
        (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit)) => this
            (let [
                #_"int" x (BNode'index (:bitmap this), bit)
                #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
            ]
                (if (some? k)
                    (when (= key k) => this
                        ;; TODO: collapse
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

    (defn- #_"IMapEntry|value" BNode''find
        ([#_"BNode" this, #_"int" shift, #_"int" hash, #_"key" key]
            (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit))
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
            (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit)) => not-found
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

    (defn- #_"seq" BNode''nodeSeq [#_"BNode" this]
        (NSeq'create-1 (:a this))
    )

    (defn- #_"node" BNode''assocT [#_"BNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
        (let [#_"int" bit (PersistentHashMap'bitpos hash, shift) #_"int" x (BNode'index (:bitmap this), bit)]
            (if-not (zero? (& (:bitmap this) bit))
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
                (let [#_"int" n (Integer/bitCount (:bitmap this))]
                    (cond
                        (< (* n 2) (alength (:a this)))
                            (let [
                                #_"BNode" e (-> (BNode''ensureEditable this, edit) (update!! :bitmap | bit)) _ (reset! addedLeaf true)
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
                                    (assoc!! :a a')
                                    (update!! :bitmap | bit)
                                )
                            )
                    )
                )
            )
        )
    )

    (defn- #_"node" BNode''dissocT [#_"BNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf]
        (let-when-not [#_"int" bit (PersistentHashMap'bitpos hash, shift)] (zero? (& (:bitmap this) bit)) => this
            (let [
                #_"int" x (BNode'index (:bitmap this), bit)
                #_"key|nil" k (aget (:a this) (* 2 x)) #_"value|node" v (aget (:a this) (inc (* 2 x)))
            ]
                (if (some? k)
                    (when (= key k) => this
                        (reset! removedLeaf true)
                        ;; TODO: collapse
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

    (defn- #_"value" BNode''kvreduce [#_"BNode" this, #_"fn" f, #_"value" r]
        (NSeq'kvreduce (:a this), f, r)
    )

    (defm BNode INode
        (INode'''assoc => BNode''assoc)
        (INode'''dissoc => BNode''dissoc)
        (INode'''find => BNode''find)
        (INode'''nodeSeq => BNode''nodeSeq)
        (INode'''assocT => BNode''assocT)
        (INode'''dissocT => BNode''dissocT)
        (INode'''kvreduce => BNode''kvreduce)
    )
)

(about #_"CNode"
    (defq CNode [#_"thread'" edit, #_"int" hash, #_"int" n, #_"array" a])

    (defn #_"CNode" CNode'new [#_"thread'" edit, #_"int" hash, #_"int" n, #_"array" a]
        (CNode'class. (anew [edit, hash, n, a]))
    )

    (defn- #_"int" CNode''findIndex [#_"CNode" this, #_"key" key]
        (let [#_"array" a (:a this) #_"int" m (* 2 (:n this))]
            (loop-when [#_"int" i 0] (< i m) => -1
                (if (= (aget a i) key) i (recur (+ i 2)))
            )
        )
    )

    (defn- #_"CNode" CNode''ensureEditable
        ([#_"CNode" this, #_"thread'" edit]
            (when-not (identical? (:edit this) edit) => this
                (let [
                    #_"int" n (:n this) #_"int" m (inc n) ;; make room for next assoc
                    #_"array" a' (-> (anew (* 2 m)) (acopy! 0 (:a this) 0 (* 2 n)))
                ]
                    (CNode'new edit, (:hash this), n, a')
                )
            )
        )
        ([#_"CNode" this, #_"thread'" edit, #_"int" n, #_"array" a]
            (when-not (identical? (:edit this) edit) => (assoc!! this :a a, :n n)
                (CNode'new edit, (:hash this), n, a)
            )
        )
    )

    (defn- #_"CNode" CNode''editAndSet
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

    (defn- #_"node" CNode''assoc [#_"CNode" this, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
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
            ;; nest it in a bitmap node
            (let [#_"BNode" node (BNode'new nil, (PersistentHashMap'bitpos (:hash this), shift), (anew [ nil, this ]))]
                (INode'''assoc node, shift, hash, key, val, addedLeaf)
            )
        )
    )

    (defn- #_"node" CNode''dissoc [#_"CNode" this, #_"int" shift, #_"int" hash, #_"key" key]
        (let-when [#_"int" i (CNode''findIndex this, key)] (< -1 i) => this
            (let-when [#_"int" n (:n this)] (< 1 n)
                (CNode'new nil, hash, (dec n), (PersistentHashMap'removePair (:a this), (quot i 2)))
            )
        )
    )

    (defn- #_"IMapEntry|value" CNode''find
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

    (defn- #_"seq" CNode''nodeSeq [#_"CNode" this]
        (NSeq'create-1 (:a this))
    )

    (defn- #_"node" CNode''assocT [#_"CNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"value" val, #_"boolean'" addedLeaf]
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
                                    (update!! :n inc)
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
            ;; nest it in a bitmap node
            (let [#_"BNode" node (BNode'new edit, (PersistentHashMap'bitpos (:hash this), shift), (anew [ nil, this, nil, nil ]))]
                (INode'''assocT node, edit, shift, hash, key, val, addedLeaf)
            )
        )
    )

    (defn- #_"node" CNode''dissocT [#_"CNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"key" key, #_"boolean'" removedLeaf]
        (let-when [#_"int" i (CNode''findIndex this, key)] (< -1 i) => this
            (reset! removedLeaf true)
            (let-when [#_"int" n (:n this)] (< 1 n)
                (let [
                    #_"CNode" e (-> (CNode''ensureEditable this, edit) (update!! :n dec))
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

    (defn- #_"value" CNode''kvreduce [#_"CNode" this, #_"fn" f, #_"value" r]
        (NSeq'kvreduce (:a this), f, r)
    )

    (defm CNode INode
        (INode'''assoc => CNode''assoc)
        (INode'''dissoc => CNode''dissoc)
        (INode'''find => CNode''find)
        (INode'''nodeSeq => CNode''nodeSeq)
        (INode'''assocT => CNode''assocT)
        (INode'''dissocT => CNode''dissocT)
        (INode'''kvreduce => CNode''kvreduce)
    )
)

(about #_"TransientHashMap"
    (defq TransientHashMap [#_"thread'" edit, #_"node" root, #_"int" cnt, #_"boolean" has-nil?, #_"value" nil-value] MapForm)

    #_inherit
    (defm TransientHashMap ATransientMap AFn)

    (defn #_"TransientHashMap" TransientHashMap'new
        ([#_"PersistentHashMap" m]
            (TransientHashMap'new (atom (thread)), (:root m), (:cnt m), (:has-nil? m), (:nil-value m))
        )
        ([#_"thread'" edit, #_"node" root, #_"int" cnt, #_"boolean" has-nil?, #_"value" nil-value]
            (TransientHashMap'class. (anew [edit, root, cnt, has-nil?, nil-value]))
        )
    )

    (defn- #_"void" TransientHashMap''assert-editable [#_"TransientHashMap" this]
        (or @(:edit this) (throw! "transient used after persistent! call"))
        nil
    )

    (defn- #_"int" TransientHashMap''count [#_"TransientHashMap" this]
        (TransientHashMap''assert-editable this)
        (:cnt this)
    )

    (defn- #_"value" TransientHashMap''valAt
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

    (defn- #_"ITransientMap" TransientHashMap''assoc! [#_"TransientHashMap" this, #_"key" key, #_"value" val]
        (TransientHashMap''assert-editable this)
        (if (nil? key)
            (let [
                this (if (= (:nil-value this) val) this (assoc!! this :nil-value val))
            ]
                (when-not (:has-nil? this) => this
                    (-> this (update!! :cnt inc) (assoc!! :has-nil? true))
                )
            )
            (let [
                #_"boolean'" addedLeaf (atom false)
                #_"node" node (INode'''assocT (or (:root this) BNode'EMPTY), (:edit this), 0, (f'hash key), key, val, addedLeaf)
                this (if (= (:root this) node) this (assoc!! this :root node))
            ]
                (when @addedLeaf => this
                    (-> this (update!! :cnt inc))
                )
            )
        )
    )

    (defn- #_"ITransientMap" TransientHashMap''dissoc! [#_"TransientHashMap" this, #_"key" key]
        (TransientHashMap''assert-editable this)
        (if (nil? key)
            (when (:has-nil? this) => this
                (-> this (update!! :cnt dec) (assoc!! :has-nil? false, :nil-value nil))
            )
            (when (some? (:root this)) => this
                (let [
                    #_"boolean'" removedLeaf (atom false)
                    #_"node" node (INode'''dissocT (:root this), (:edit this), 0, (f'hash key), key, removedLeaf)
                    this (if (= (:root this) node) this (assoc!! this :root node))
                ]
                    (when @removedLeaf => this
                        (-> this (update!! :cnt dec))
                    )
                )
            )
        )
    )

    (defn- #_"ITransientMap" TransientHashMap''conj! [#_"TransientHashMap" this, #_"pair" o]
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

    (declare PersistentHashMap'new)

    (defn- #_"IPersistentMap" TransientHashMap''persistent! [#_"TransientHashMap" this]
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
    (declare PersistentHashMap''seq)

    (defq PersistentHashMap [#_"meta" _meta, #_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value] MapForm
        clojure.lang.Seqable (seq [_] (PersistentHashMap''seq _))
    )

    #_inherit
    (defm PersistentHashMap APersistentMap AFn)

    (defn #_"PersistentHashMap" PersistentHashMap'new
        ([#_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value] (PersistentHashMap'new nil, cnt, root, has-nil?, nil-value))
        ([#_"meta" meta, #_"int" cnt, #_"node" root, #_"boolean" has-nil?, #_"value" nil-value]
            (PersistentHashMap'class. (anew [meta, cnt, root, has-nil?, nil-value]))
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

    (defn- #_"PersistentHashMap" PersistentHashMap''withMeta [#_"PersistentHashMap" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentHashMap'new meta, (:cnt this), (:root this), (:has-nil? this), (:nil-value this))
        )
    )

    (defn- #_"value" PersistentHashMap''valAt
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

    (def- #_"value" PersistentHashMap'NOT_FOUND (Object.))

    (defn- #_"IPersistentMap" PersistentHashMap''assoc [#_"PersistentHashMap" this, #_"key" key, #_"value" val]
        (if (nil? key)
            (when-not (and (:has-nil? this) (= (:nil-value this) val)) => this
                (PersistentHashMap'new (:_meta this), (+ (:cnt this) (if (:has-nil? this) 0 1)), (:root this), true, val)
            )
            (let [
                #_"boolean'" addedLeaf (atom false)
                #_"node" root (INode'''assoc (or (:root this) BNode'EMPTY), 0, (f'hash key), key, val, addedLeaf)
            ]
                (when-not (= root (:root this)) => this
                    (PersistentHashMap'new (:_meta this), (+ (:cnt this) (if @addedLeaf 1 0)), root, (:has-nil? this), (:nil-value this))
                )
            )
        )
    )

    (defn- #_"boolean" PersistentHashMap''containsKey [#_"PersistentHashMap" this, #_"key" key]
        (if (nil? key)
            (:has-nil? this)
            (and (some? (:root this))
                (not (identical? (INode'''find (:root this), 0, (f'hash key), key, PersistentHashMap'NOT_FOUND) PersistentHashMap'NOT_FOUND))
            )
        )
    )

    (defn- #_"pair" PersistentHashMap''entryAt [#_"PersistentHashMap" this, #_"key" key]
        (if (nil? key)
            (when (:has-nil? this)
                (MapEntry'new nil, (:nil-value this))
            )
            (when (some? (:root this))
                (INode'''find (:root this), 0, (f'hash key), key)
            )
        )
    )

    (defn- #_"IPersistentMap" PersistentHashMap''dissoc [#_"PersistentHashMap" this, #_"key" key]
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

    (defn- #_"IPersistentCollection" PersistentHashMap''empty [#_"PersistentHashMap" this]
        (with-meta PersistentHashMap'EMPTY (:_meta this))
    )

    (defn- #_"seq" PersistentHashMap''seq [#_"PersistentHashMap" this]
        (let [#_"seq" s (when (some? (:root this)) (INode'''nodeSeq (:root this)))]
            (when (:has-nil? this) => s
                (Cons'new (MapEntry'new nil, (:nil-value this)), s)
            )
        )
    )

    (defn- #_"value" PersistentHashMap''kvreduce [#_"PersistentHashMap" this, #_"fn" f, #_"value" r]
        (let [r (if (:has-nil? this) (f r nil (:nil-value this)) r)]
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

    (defm PersistentHashMap IKVReduce
        (IKVReduce'''kvreduce => PersistentHashMap''kvreduce)
    )

    (defm PersistentHashMap IEditableCollection
        (IEditableCollection'''asTransient => TransientHashMap'new)
    )

    (defm PersistentHashMap IObject
        (IObject'''equals => APersistentMap''equals)
        (IObject'''toString => print-string)
    )

    (defm PersistentHashMap Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

;;;
 ; keyval => key val
 ; Returns a new hash map with supplied mappings.
 ; If any keys are equal, they are handled as if by repeated uses of assoc.
 ;;
(defn hash-map
    ([] PersistentHashMap'EMPTY)
    ([& keyvals] (PersistentHashMap'create-1s keyvals))
)

;;;
 ; Returns a map that consists of the rest of the maps conj-ed onto
 ; the first. If a key occurs in more than one map, the mapping from
 ; the latter (left-to-right) will be the mapping in the result.
 ;;
(defn merge [& maps]
    (when (some identity maps)
        (reduce #(conj (or %1 (hash-map)) %2) maps)
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
            (reduce #(reduce merge- (or %1 (hash-map)) %2) maps)
        )
    )
)

;;;
 ; Returns a map with the keys mapped to the corresponding vals.
 ;;
(defn zipmap [keys vals]
    (loop-when-recur [m (transient (hash-map)) ks (seq keys) vs (seq vals)]
                     (and ks vs)
                     [(assoc! m (first ks) (first vs)) (next ks) (next vs)]
                  => (persistent! m)
    )
)
)

(about #_"arbace.PersistentHashSet"

(about #_"TransientHashSet"
    (defq TransientHashSet [#_"ITransientMap" impl] SetForm)

    #_inherit
    (defm TransientHashSet ATransientSet AFn)

    (defn #_"TransientHashSet" TransientHashSet'new [#_"ITransientMap" impl]
        (TransientHashSet'class. (anew [impl]))
    )

    (defn- #_"int" TransientHashSet''count [#_"TransientHashSet" this]
        (count (:impl this))
    )

    (defn- #_"ITransientSet" TransientHashSet''conj! [#_"TransientHashSet" this, #_"value" val]
        (let [#_"ITransientMap" m (assoc! (:impl this) val val)]
            (when-not (= m (:impl this)) => this
                (assoc!! this :impl m)
            )
        )
    )

    (declare PersistentHashSet'new)

    (defn- #_"PersistentHashSet" TransientHashSet''persistent! [#_"TransientHashSet" this]
        (PersistentHashSet'new nil, (persistent! (:impl this)))
    )

    (defn- #_"ITransientSet" TransientHashSet''disj! [#_"TransientHashSet" this, #_"key" key]
        (let [#_"ITransientMap" m (dissoc! (:impl this) key)]
            (when-not (= m (:impl this)) => this
                (assoc!! this :impl m)
            )
        )
    )

    (defn- #_"boolean" TransientHashSet''contains? [#_"TransientHashSet" this, #_"key" key]
        (not (identical? (get (:impl this) key this) this))
    )

    (defn- #_"value" TransientHashSet''get [#_"TransientHashSet" this, #_"key" key]
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
        (PersistentHashSet'class. (anew [meta, impl]))
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

    (defn- #_"PersistentHashSet" PersistentHashSet''withMeta [#_"PersistentHashSet" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentHashSet'new meta, (:impl this))
        )
    )

    (defn- #_"int" PersistentHashSet''count [#_"PersistentHashSet" this]
        (count (:impl this))
    )

    (defn- #_"PersistentHashSet" PersistentHashSet''conj [#_"PersistentHashSet" this, #_"value" val]
        (if (contains? (:impl this) val)
            this
            (PersistentHashSet'new (:_meta this), (assoc (:impl this) val val))
        )
    )

    (defn- #_"PersistentHashSet" PersistentHashSet''empty [#_"PersistentHashSet" this]
        (with-meta PersistentHashSet'EMPTY (:_meta this))
    )

    (defn- #_"IPersistentSet" PersistentHashSet''disj [#_"PersistentHashSet" this, #_"key" key]
        (if (contains? (:impl this) key)
            (PersistentHashSet'new (:_meta this), (dissoc (:impl this) key))
            this
        )
    )

    (defn- #_"boolean" PersistentHashSet''contains? [#_"PersistentHashSet" this, #_"key" key]
        (contains? (:impl this) key)
    )

    (defn- #_"value" PersistentHashSet''get [#_"PersistentHashSet" this, #_"key" key]
        (get (:impl this) key)
    )

    (defn- #_"seq" PersistentHashSet''seq [#_"PersistentHashSet" this]
        (keys (:impl this))
    )

    (defn- #_"ITransientCollection" PersistentHashSet''asTransient [#_"PersistentHashSet" this]
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
        (IObject'''toString => print-string)
    )

    (defm PersistentHashSet Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

;;;
 ; Returns a new hash set with supplied keys.
 ; Any equal keys are handled as if by repeated uses of conj.
 ;;
(defn hash-set
    ([] PersistentHashSet'EMPTY)
    ([& keys] (PersistentHashSet'create keys))
)

;;;
 ; Returns a set of the distinct elements of coll.
 ;;
(defn set [s] (if (set? s) (with-meta s nil) (into (hash-set) s)))
)

(about #_"arbace.PersistentTreeMap"

(about #_"TNode"
    (defn #_"value" TNode''kvreduce [#_"node" this, #_"fn" f, #_"value" r]
        (or
            (when (some? (:left this))
                (let [r (INode'''kvreduce (:left this), f, r)]
                    (when (reduced? r)
                        r
                    )
                )
            )
            (let [r (f r (key this) (val this))]
                (cond
                    (reduced? r)          r
                    (some? (:right this)) (INode'''kvreduce (:right this), f, r)
                    :else                 r
                )
            )
        )
    )
)

(about #_"Black"
    (defq Black [#_"key" key])

    #_inherit
    (defm Black TNode AMapEntry APersistentVector AFn)

    (defn #_"Black" Black'new [#_"key" key]
        (Black'class. (anew [key]))
    )

    (defn- #_"node" Black''addLeft [#_"Black" this, #_"node" ins]
        (ITNode'''balanceLeft ins, this)
    )

    (defn- #_"node" Black''addRight [#_"Black" this, #_"node" ins]
        (ITNode'''balanceRight ins, this)
    )

    (declare PersistentTreeMap'balanceLeftDel)

    (defn- #_"node" Black''removeLeft [#_"Black" this, #_"node" del]
        (PersistentTreeMap'balanceLeftDel (:key this), (:val this), del, (:right this))
    )

    (declare PersistentTreeMap'balanceRightDel)

    (defn- #_"node" Black''removeRight [#_"Black" this, #_"node" del]
        (PersistentTreeMap'balanceRightDel (:key this), (:val this), (:left this), del)
    )

    (defn- #_"node" Black''blacken [#_"Black" this]
        this
    )

    (declare Red'new)

    (defn- #_"node" Black''redden [#_"Black" this]
        (Red'new (:key this))
    )

    (declare PersistentTreeMap'black)

    (defn- #_"node" Black''balanceLeft [#_"Black" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), this, (:right parent))
    )

    (defn- #_"node" Black''balanceRight [#_"Black" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), this)
    )

    (defn- #_"node" Black''replace [#_"Black" this, #_"key" key, #_"value" val, #_"node" left, #_"node" right]
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
        (IObject'''toString => print-string)
    )

    (defm Black Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm Black IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm Black #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"BlackVal"
    (defq BlackVal [#_"key" key, #_"value" val]
        java.util.Map$Entry (getKey [_] (:key _)) (getValue [_] (:val _))
    )

    #_inherit
    (defm BlackVal Black TNode AMapEntry APersistentVector AFn)

    (defn #_"BlackVal" BlackVal'new [#_"key" key, #_"value" val]
        (BlackVal'class. (anew [key, val]))
    )

    (declare RedVal'new)

    (defn- #_"node" BlackVal''redden [#_"BlackVal" this]
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
        (IObject'''toString => print-string)
    )

    (defm BlackVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm BlackVal IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm BlackVal #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"BlackBranch"
    (defq BlackBranch [#_"key" key, #_"node" left, #_"node" right])

    #_inherit
    (defm BlackBranch Black TNode AMapEntry APersistentVector AFn)

    (defn #_"BlackBranch" BlackBranch'new [#_"key" key, #_"node" left, #_"node" right]
        (BlackBranch'class. (anew [key, left, right]))
    )

    (declare RedBranch'new)

    (defn- #_"node" BlackBranch''redden [#_"BlackBranch" this]
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
        (IObject'''toString => print-string)
    )

    (defm BlackBranch Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm BlackBranch IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm BlackBranch #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"BlackBranchVal"
    (defq BlackBranchVal [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        java.util.Map$Entry (getKey [_] (:key _)) (getValue [_] (:val _))
    )

    #_inherit
    (defm BlackBranchVal BlackBranch Black TNode AMapEntry APersistentVector AFn)

    (defn #_"BlackBranchVal" BlackBranchVal'new [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (BlackBranchVal'class. (anew [key, val, left, right]))
    )

    (declare RedBranchVal'new)

    (defn- #_"node" BlackBranchVal''redden [#_"BlackBranchVal" this]
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
        (IObject'''toString => print-string)
    )

    (defm BlackBranchVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm BlackBranchVal IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm BlackBranchVal #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"Red"
    (defq Red [#_"key" key])

    #_inherit
    (defm Red TNode AMapEntry APersistentVector AFn)

    (defn #_"Red" Red'new [#_"key" key]
        (Red'class. (anew [key]))
    )

    (declare PersistentTreeMap'red)

    (defn- #_"node" Red''addLeft [#_"Red" this, #_"node" ins]
        (PersistentTreeMap'red (:key this), (:val this), ins, (:right this))
    )

    (defn- #_"node" Red''addRight [#_"Red" this, #_"node" ins]
        (PersistentTreeMap'red (:key this), (:val this), (:left this), ins)
    )

    (defn- #_"node" Red''removeLeft [#_"Red" this, #_"node" del]
        (PersistentTreeMap'red (:key this), (:val this), del, (:right this))
    )

    (defn- #_"node" Red''removeRight [#_"Red" this, #_"node" del]
        (PersistentTreeMap'red (:key this), (:val this), (:left this), del)
    )

    (defn- #_"node" Red''blacken [#_"Red" this]
        (Black'new (:key this))
    )

    (defn- #_"node" Red''redden [#_"Red" this]
        (throw! "invariant violation")
    )

    (defn- #_"node" Red''balanceLeft [#_"Red" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), this, (:right parent))
    )

    (defn- #_"node" Red''balanceRight [#_"Red" this, #_"node" parent]
        (PersistentTreeMap'black (:key parent), (:val parent), (:left parent), this)
    )

    (defn- #_"node" Red''replace [#_"Red" this, #_"key" key, #_"value" val, #_"node" left, #_"node" right]
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
        (IObject'''toString => print-string)
    )

    (defm Red Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm Red IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm Red #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"RedVal"
    (defq RedVal [#_"key" key, #_"value" val]
        java.util.Map$Entry (getKey [_] (:key _)) (getValue [_] (:val _))
    )

    #_inherit
    (defm RedVal Red TNode AMapEntry APersistentVector AFn)

    (defn #_"RedVal" RedVal'new [#_"key" key, #_"value" val]
        (RedVal'class. (anew [key, val]))
    )

    (defn- #_"node" RedVal''blacken [#_"RedVal" this]
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
        (IObject'''toString => print-string)
    )

    (defm RedVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm RedVal IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm RedVal #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"RedBranch"
    (defq RedBranch [#_"key" key, #_"node" left, #_"node" right])

    #_inherit
    (defm RedBranch Red TNode AMapEntry APersistentVector AFn)

    (defn #_"RedBranch" RedBranch'new [#_"key" key, #_"node" left, #_"node" right]
        (RedBranch'class. (anew [key, left, right]))
    )

    (defn- #_"node" RedBranch''blacken [#_"RedBranch" this]
        (BlackBranch'new (:key this), (:left this), (:right this))
    )

    (defn- #_"node" RedBranch''balanceLeft [#_"RedBranch" this, #_"node" parent]
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

    (defn- #_"node" RedBranch''balanceRight [#_"RedBranch" this, #_"node" parent]
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
        (IObject'''toString => print-string)
    )

    (defm RedBranch Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm RedBranch IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm RedBranch #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"RedBranchVal"
    (defq RedBranchVal [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        java.util.Map$Entry (getKey [_] (:key _)) (getValue [_] (:val _))
    )

    #_inherit
    (defm RedBranchVal RedBranch Red TNode AMapEntry APersistentVector AFn)

    (defn #_"RedBranchVal" RedBranchVal'new [#_"key" key, #_"value" val, #_"node" left, #_"node" right]
        (RedBranchVal'class. (anew [key, val, left, right]))
    )

    (defn- #_"node" RedBranchVal''blacken [#_"RedBranchVal" this]
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
        (IObject'''toString => print-string)
    )

    (defm RedBranchVal Hashed
        (Hashed'''hash => AMapEntry''hash)
    )

    (defm RedBranchVal IKVReduce
        (IKVReduce'''kvreduce => TNode''kvreduce)
    )

    #_foreign
    (§ defm RedBranchVal #_"Comparable"
        (Comparable'''compareTo => AMapEntry''compareTo)
    )
)

(about #_"TSeq"
    (declare TSeq''seq TSeq''first TSeq''next)

    (defq TSeq [#_"meta" _meta, #_"seq" stack, #_"boolean" asc?, #_"int" cnt] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (TSeq''seq _)) (first [_] (TSeq''first _)) (next [_] (TSeq''next _))
    )

    #_inherit
    (defm TSeq ASeq)

    (defn #_"TSeq" TSeq'new
        ([#_"seq" stack, #_"boolean" asc?] (TSeq'new stack, asc?, -1))
        ([#_"seq" stack, #_"boolean" asc?, #_"int" cnt] (TSeq'new nil, stack, asc?, cnt))
        ([#_"meta" meta, #_"seq" stack, #_"boolean" asc?, #_"int" cnt]
            (TSeq'class. (anew [meta, stack, asc?, cnt]))
        )
    )

    (defn- #_"TSeq" TSeq''withMeta [#_"TSeq" this, #_"meta" meta]
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

    (defn- #_"seq" TSeq''seq [#_"TSeq" this]
        this
    )

    (defn- #_"Object" TSeq''first [#_"TSeq" this]
        (first (:stack this))
    )

    (defn- #_"seq" TSeq''next [#_"TSeq" this]
        (let [#_"node" t #_"node" (first (:stack this)) #_"boolean" asc? (:asc? this)]
            (when-some [#_"seq" stack (TSeq'push (if asc? (:right t) (:left t)), (next (:stack this)), asc?)]
                (TSeq'new stack, asc?, (dec (:cnt this)))
            )
        )
    )

    (defn- #_"int" TSeq''count [#_"TSeq" this]
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
        (IObject'''toString => print-string)
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
    (declare PersistentTreeMap''seq)

    (defq PersistentTreeMap [#_"meta" _meta, #_"Comparator" cmp, #_"node" tree, #_"int" cnt] MapForm
        clojure.lang.Seqable (seq [_] (PersistentTreeMap''seq _))
        java.util.Map (entrySet [_] (-/into #{} _))
    )

    #_inherit
    (defm PersistentTreeMap APersistentMap AFn)

    (defn #_"PersistentTreeMap" PersistentTreeMap'new
        ([] (PersistentTreeMap'new compare))
        ([#_"Comparator" cmp] (PersistentTreeMap'new nil, cmp))
        ([#_"meta" meta, #_"Comparator" cmp] (PersistentTreeMap'new meta, cmp, nil, 0))
        ([#_"meta" meta, #_"Comparator" cmp, #_"node" tree, #_"int" cnt]
            (PersistentTreeMap'class. (anew [meta, cmp, tree, cnt]))
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

    (defn- #_"IPersistentCollection" PersistentTreeMap''empty [#_"PersistentTreeMap" this]
        (PersistentTreeMap'new (:_meta this), (:cmp this))
    )

    (defn- #_"PersistentTreeMap" PersistentTreeMap''withMeta [#_"PersistentTreeMap" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentTreeMap'new meta, (:cmp this), (:tree this), (:cnt this))
        )
    )

    (defn- #_"int" PersistentTreeMap''doCompare [#_"PersistentTreeMap" this, #_"key" k1, #_"key" k2]
        (.compare (:cmp this), k1, k2)
    )

    (defn- #_"key" PersistentTreeMap''entryKey [#_"PersistentTreeMap" this, #_"pair" entry]
        (key entry)
    )

    (defn- #_"seq" PersistentTreeMap''seq
        ([#_"PersistentTreeMap" this] (PersistentTreeMap''seq this, true))
        ([#_"PersistentTreeMap" this, #_"boolean" ascending?]
            (when (pos? (:cnt this))
                (TSeq'create (:tree this), ascending?, (:cnt this))
            )
        )
    )

    (defn- #_"seq" PersistentTreeMap''seqFrom [#_"PersistentTreeMap" this, #_"key" key, #_"boolean" ascending?]
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

    (defn- #_"seq" PersistentTreeMap''rseq [#_"PersistentTreeMap" this]
        (PersistentTreeMap''seq this, false)
    )

    (defn- #_"node" PersistentTreeMap''entryAt [#_"PersistentTreeMap" this, #_"key" key]
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

    (defn- #_"value" PersistentTreeMap''valAt
        ([#_"PersistentTreeMap" this, #_"key" key] (PersistentTreeMap''valAt this, key, nil))
        ([#_"PersistentTreeMap" this, #_"key" key, #_"value" not-found]
            (when-some [#_"node" node (PersistentTreeMap''entryAt this, key)] => not-found
                (IMapEntry'''val node)
            )
        )
    )

    (defn- #_"boolean" PersistentTreeMap''containsKey [#_"PersistentTreeMap" this, #_"key" key]
        (some? (PersistentTreeMap''entryAt this, key))
    )

    (defn- #_"value" PersistentTreeMap''kvreduce [#_"PersistentTreeMap" this, #_"fn" f, #_"value" r]
        (let [r (if (some? (:tree this)) (INode'''kvreduce (:tree this), f, r) r)]
            (if (reduced? r) @r r)
        )
    )

    (defn- #_"node" PersistentTreeMap''min [#_"PersistentTreeMap" this]
        (when-some [#_"node" t (:tree this)]
            (loop-when-recur t (some? (:left t)) (:left t) => t)
        )
    )

    (defn- #_"node" PersistentTreeMap''max [#_"PersistentTreeMap" this]
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

    (defn- #_"node" PersistentTreeMap'rightBalance [#_"key" key, #_"value" val, #_"node" left, #_"node" ins]
        (cond
            (and (satisfies? Red ins) (satisfies? Red (:right ins)))
                (PersistentTreeMap'red (:key ins), (:val ins), (PersistentTreeMap'black key, val, left, (:left ins)), (ITNode'''blacken (:right ins)))
            (and (satisfies? Red ins) (satisfies? Red (:left ins)))
                (PersistentTreeMap'red (:key (:left ins)), (:val (:left ins)), (PersistentTreeMap'black key, val, left, (:left (:left ins))), (PersistentTreeMap'black (:key ins), (:val ins), (:right (:left ins)), (:right ins)))
            :else
                (PersistentTreeMap'black key, val, left, ins)
        )
    )

    (defn- #_"node" PersistentTreeMap'balanceLeftDel [#_"key" key, #_"value" val, #_"node" del, #_"node" right]
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

    (defn- #_"node" PersistentTreeMap'leftBalance [#_"key" key, #_"value" val, #_"node" ins, #_"node" right]
        (cond
            (and (satisfies? Red ins) (satisfies? Red (:left ins)))
                (PersistentTreeMap'red (:key ins), (:val ins), (ITNode'''blacken (:left ins)), (PersistentTreeMap'black key, val, (:right ins), right))
            (and (satisfies? Red ins) (satisfies? Red (:right ins)))
                (PersistentTreeMap'red (:key (:right ins)), (:val (:right ins)), (PersistentTreeMap'black (:key ins), (:val ins), (:left ins), (:left (:right ins))), (PersistentTreeMap'black key, val, (:right (:right ins)), right))
            :else
                (PersistentTreeMap'black key, val, ins, right)
        )
    )

    (defn- #_"node" PersistentTreeMap'balanceRightDel [#_"key" key, #_"value" val, #_"node" left, #_"node" del]
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

    (defn- #_"node" PersistentTreeMap''add [#_"PersistentTreeMap" this, #_"node" t, #_"key" key, #_"value" val, #_"node'" found]
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
                        (when (some? ins) => nil ;; found below
                            (if (neg? cmp) (ITNode'''addLeft t, ins) (ITNode'''addRight t, ins))
                        )
                    )
                )
            )
        )
    )

    (defn- #_"node" PersistentTreeMap'append [#_"node" left, #_"node" right]
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
            :else ;; black/black
                (let [#_"node" app (PersistentTreeMap'append (:right left), (:left right))]
                    (if (satisfies? Red app)
                        (PersistentTreeMap'red (:key app), (:val app), (PersistentTreeMap'black (:key left), (:val left), (:left left), (:left app)), (PersistentTreeMap'black (:key right), (:val right), (:right app), (:right right)))
                        (PersistentTreeMap'balanceLeftDel (:key left), (:val left), (:left left), (PersistentTreeMap'black (:key right), (:val right), app, (:right right)))
                    )
                )
        )
    )

    (defn- #_"node" PersistentTreeMap''remove [#_"PersistentTreeMap" this, #_"node" t, #_"key" key, #_"node'" found]
        (when (some? t) => nil ;; not found indicator
            (let [#_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))]
                (if (zero? cmp)
                    (do
                        (reset! found t)
                        (PersistentTreeMap'append (:left t), (:right t))
                    )
                    (let [#_"node" del (PersistentTreeMap''remove this, (if (neg? cmp) (:left t) (:right t)), key, found)]
                        (when (or (some? del) (some? @found)) => nil ;; not found below
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

    (defn- #_"node" PersistentTreeMap''replace [#_"PersistentTreeMap" this, #_"node" t, #_"key" key, #_"value" val]
        (let [
            #_"int" cmp (PersistentTreeMap''doCompare this, key, (:key t))
            #_"node" left  (if (neg? cmp) (PersistentTreeMap''replace this, (:left  t), key, val) (:left  t))
            #_"node" right (if (pos? cmp) (PersistentTreeMap''replace this, (:right t), key, val) (:right t))
        ]
            (ITNode'''replace t, (:key t), (if (zero? cmp) val (:val t)), left, right)
        )
    )

    (defn- #_"PersistentTreeMap" PersistentTreeMap''assoc [#_"PersistentTreeMap" this, #_"key" key, #_"value" val]
        (let [#_"node'" found (atom nil) #_"node" t (PersistentTreeMap''add this, (:tree this), key, val, found)]
            (if (nil? t)
                (if (= (:val #_"node" @found) val)
                    this
                    (PersistentTreeMap'new (:_meta this), (:cmp this), (PersistentTreeMap''replace this, (:tree this), key, val), (:cnt this))
                )
                (PersistentTreeMap'new (:_meta this), (:cmp this), (ITNode'''blacken t), (inc (:cnt this)))
            )
        )
    )

    (defn- #_"PersistentTreeMap" PersistentTreeMap''dissoc [#_"PersistentTreeMap" this, #_"key" key]
        (let [#_"node'" found (atom nil) #_"node" t (PersistentTreeMap''remove this, (:tree this), key, found)]
            (if (nil? t)
                (if (nil? @found)
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

    (defm PersistentTreeMap IKVReduce
        (IKVReduce'''kvreduce => PersistentTreeMap''kvreduce)
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
        (IObject'''toString => print-string)
    )

    (defm PersistentTreeMap Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
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
)

(about #_"arbace.PersistentTreeSet"

(about #_"PersistentTreeSet"
    (defq PersistentTreeSet [#_"meta" _meta, #_"map" impl] SetForm)

    #_inherit
    (defm PersistentTreeSet APersistentSet AFn)

    (defn #_"PersistentTreeSet" PersistentTreeSet'new [#_"meta" meta, #_"map" impl]
        (PersistentTreeSet'class. (anew [meta, impl]))
    )

    (def #_"PersistentTreeSet" PersistentTreeSet'EMPTY (PersistentTreeSet'new nil, PersistentTreeMap'EMPTY))

    (defn #_"PersistentTreeSet" PersistentTreeSet'create
        ([                    #_"Seqable" init] (into PersistentTreeSet'EMPTY                                       init))
        ([#_"Comparator" cmp, #_"Seqable" init] (into (PersistentTreeSet'new nil, (PersistentTreeMap'new nil, cmp)) init))
    )

    (defn- #_"PersistentTreeSet" PersistentTreeSet''withMeta [#_"PersistentTreeSet" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentTreeSet'new meta, (:impl this))
        )
    )

    (defn- #_"int" PersistentTreeSet''count [#_"PersistentTreeSet" this]
        (count (:impl this))
    )

    (defn- #_"PersistentTreeSet" PersistentTreeSet''conj [#_"PersistentTreeSet" this, #_"value" val]
        (if (contains? (:impl this) val)
            this
            (PersistentTreeSet'new (:_meta this), (assoc (:impl this) val val))
        )
    )

    (defn- #_"PersistentTreeSet" PersistentTreeSet''empty [#_"PersistentTreeSet" this]
        (PersistentTreeSet'new (:_meta this), (empty (:impl this)))
    )

    (defn- #_"IPersistentSet" PersistentTreeSet''disj [#_"PersistentTreeSet" this, #_"key" key]
        (if (contains? (:impl this) key)
            (PersistentTreeSet'new (:_meta this), (dissoc (:impl this) key))
            this
        )
    )

    (defn- #_"boolean" PersistentTreeSet''contains? [#_"PersistentTreeSet" this, #_"key" key]
        (contains? (:impl this) key)
    )

    (defn- #_"value" PersistentTreeSet''get [#_"PersistentTreeSet" this, #_"key" key]
        (get (:impl this) key)
    )

    (defn- #_"Comparator" PersistentTreeSet''comparator [#_"PersistentTreeSet" this]
        (Sorted'''comparator (:impl this))
    )

    (defn- #_"value" PersistentTreeSet''entryKey [#_"PersistentTreeSet" this, #_"value" entry]
        entry
    )

    (defn- #_"seq" PersistentTreeSet''seq
        ([#_"PersistentTreeSet" this]
            (keys (:impl this))
        )
        ([#_"PersistentTreeSet" this, #_"boolean" ascending?]
            (keys (Sorted'''seq (:impl this), ascending?))
        )
    )

    (defn- #_"seq" PersistentTreeSet''seqFrom [#_"PersistentTreeSet" this, #_"key" key, #_"boolean" ascending?]
        (keys (Sorted'''seqFrom (:impl this), key, ascending?))
    )

    (defn- #_"seq" PersistentTreeSet''rseq [#_"PersistentTreeSet" this]
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
        (IObject'''toString => print-string)
    )

    (defm PersistentTreeSet Hashed
        (Hashed'''hash => Murmur3'hashUnordered)
    )
)

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
)

(about #_"arbace.PersistentVector"

(about #_"VNode"
    (defq VNode [#_"thread'" edit, #_"array" array, #_"index" index])

    (defn #_"node" VNode'new [#_"thread'" edit, #_"array" array, #_"index" index]
        (VNode'class. (anew [edit, (or array (anew 32)), index]))
    )

    (def #_"node" VNode'EMPTY (VNode'new nil, nil, nil))

    (defn #_"void" VNode''assert-editable [#_"node" this]
        (let [
            #_"thread" owner @(or (:edit this) (throw! "transient use of persistent data"))
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
            (or (nil? e) (nil? @e) (not (or (identical? e edit) (throw! "transient cow!"))))
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

    (defn #_"value" VNode''value-for [#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail-off, #_"values" tail]
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
                    #_"int" e (& (>>> (dec cnt) shift) 0x1f)
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
                        (aswap! a m VNode''do-assoc edit, (- shift 5), i, val)
                    )
                )
        ]
            (if cow? (VNode'new edit, a, x) this)
        )
    )

    (defn- #_"index" VNode'n-index [#_"int" shift, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
        ]
            (loop-when-recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< i n) [(aset! x j i) (inc j) (+ i k)] => (-> x (aset! j n) (aset! 32 (inc j))))
        )
    )

    (defn- #_"index" VNode'm-n-index [#_"int" shift, #_"int" m, #_"int" n]
        (let [
            #_"int" k (<< 1 shift)
        ]
            (loop-when-recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< j m) [(aset! x j i) (inc j) (+ i k)] => (-> x (aset! j n) (aset! 32 (inc j))))
        )
    )

    (defn- #_"int" VNode'index-of-nil [#_"array" a]
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

    (defn- #_"node" VNode''first-child [#_"node" this]
        (aget (:array this) 0)
    )

    (defn- #_"node" VNode''last-child [#_"node" this]
        (let [
            #_"array" a (:array this) #_"index" x (:index this)
        ]
            (aget a (dec (if (some? x) (aget x 32) (VNode'index-of-nil a))))
        )
    )

    (defn- #_"node" VNode''remove-leftmost-child [#_"node" this]
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

    (defn- #_"node" VNode''replace-leftmost-child [#_"node" this, #_"int" shift, #_"int" cnt, #_"node" node, #_"int" delta]
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
            (VNode'new nil, a', x')
        )
    )

    (defn- #_"node" VNode''replace-rightmost-child [#_"node" this, #_"int" shift, #_"node" node, #_"int" delta]
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
    (def- #_"int" VNode'max-extra-search-steps 2)

    (defn #_"node" VNode''slice-right [#_"node" this, #_"int" shift, #_"int" end]
        ;; => potentially return a short node, although it would be better to make sure a regular
        ;; leaf is always left at the right, with any items over the final 32 moved into tail
        ;; (and then potentially back into the tree should the tail become too long...)
        (when (pos? shift) => (VNode'new nil, (-> (anew end) (acopy! 0 (:array this) 0 end)), nil)
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
            ;; potentially return a short node
            (let [
                #_"array" a (:array this)
                #_"int" n (- (alength a) start)
            ]
                (VNode'new nil, (-> (anew n) (acopy! 0 a start n)), nil)
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

    (defn- #_"int" VNode''leaf-count [#_"node" this, #_"int" shift]
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

    (defn- #_"int" VNode''tree-count [#_"node" this, #_"int" shift]
        ;; NB. positive shifts only
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

    (defn- #_"seq" VNode''leaf-seq [#_"node" this]
        (let [
            #_"array" a (:array this)
        ]
            (mapcat :array (take (VNode'index-of-nil a) a))
        )
    )

    (defn- #_"[node node int]" VNode'rebalance-leaves [#_"node" node1, #_"node" node2, #_"int" delta]
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

    (defn- #_"seq" VNode''child-seq [#_"node" this, #_"int" shift, #_"int" cnt]
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

    (defn- #_"[node node int]" VNode'rebalance [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
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
    (defq TransientVector [#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen] VecForm)

    #_inherit
    (defm TransientVector AFn)

    (defn #_"TransientVector" TransientVector'new
        ([#_"PersistentVector" w]
            (TransientVector'new (:cnt w), (:shift w), (VNode''editable-root (:root w)), (VNode'editable-tail (:tail w)), (alength (:tail w)))
        )
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen]
            (TransientVector'class. (anew [cnt, shift, root, tail, tlen]))
        )
    )

    (defn- #_"int" TransientVector''count [#_"TransientVector" this]
        (VNode''assert-editable (:root this))
        (:cnt this)
    )

    (defn- #_"int" TransientVector''tail-off [#_"TransientVector" this]
        (- (:cnt this) (:tlen this))
    )

    (defn- #_"values" TransientVector''array-for [#_"TransientVector" this, #_"int" i]
        (VNode''array-for (:root this), i, (:shift this), (:cnt this), (TransientVector''tail-off this), (:tail this))
    )

    (defn- #_"value" TransientVector''value-for [#_"TransientVector" this, #_"int" i]
        (VNode''value-for (:root this), i, (:shift this), (:cnt this), (TransientVector''tail-off this), (:tail this))
    )

    (defn- #_"value" TransientVector''nth
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

    (defn- #_"value" TransientVector''valAt
        ([#_"TransientVector" this, #_"key" key] (TransientVector''valAt this, key, nil))
        ([#_"TransientVector" this, #_"key" key, #_"value" not-found]
            (VNode''assert-editable (:root this))
            (when (integer? key) => not-found
                (let-when [#_"int" i (int! key)] (< -1 i (:cnt this)) => not-found
                    (TransientVector''value-for this, i)
                )
            )
        )
    )

    (defn- #_"value" TransientVector''invoke [#_"TransientVector" this, #_"key" arg]
        (when (integer? arg) => (throw! "arg must be integer")
            (Indexed'''nth this, (int! arg))
        )
    )

    (defn- #_"value" TransientVector''applyTo [#_"TransientVector" this, #_"seq" args]
        (case (count args 1)
            1 (IFn'''invoke this, (first args))
        )
    )

    (defn- #_"TransientVector" TransientVector''conj! [#_"TransientVector" this, #_"value" val]
        (VNode''assert-editable (:root this))
        (if (< (:tlen this) 32)
            (let [
                _ (aset! (:tail this) (:tlen this) val)
            ]
                (-> this (update!! :cnt inc) (update!! :tlen inc))
            )
            (let [
                #_"node" tail-node (VNode'new (:edit (:root this)), (:tail this), nil)
                this (assoc!! this :tail (-> (anew 32) (aset! 0 val)), :tlen 1)
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
                        (-> this (assoc!! :root root) (update!! :shift + 5) (update!! :cnt inc))
                    )
                    (let [
                        #_"node" root (VNode''push-tail (:root this), (:edit (:root this)), (:shift this), (:cnt this), tail-node)
                    ]
                        (-> this (assoc!! :root root) (update!! :cnt inc))
                    )
                )
            )
        )
    )

    (declare PersistentVector'new)

    (defn- #_"PersistentVector" TransientVector''persistent! [#_"TransientVector" this]
        (VNode''assert-editable (:root this))
        (reset! (:edit (:root this)) nil)
        (let [
            #_"int" n (:tlen this)
        ]
            (PersistentVector'new (:cnt this), (:shift this), (:root this), (-> (anew n) (acopy! 0 (:tail this) 0 n)))
        )
    )

    (defn- #_"TransientVector" TransientVector''assocN! [#_"TransientVector" this, #_"int" i, #_"value" val]
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
                        (assoc!! this :root (VNode''do-assoc (:root this), (:edit (:root this)), (:shift this), i, val))
                    )
                )
            )
            (when (= i (:cnt this)) => (throw! "index is out of bounds")
                (ITransientCollection'''conj! this, val)
            )
        )
    )

    (defn- #_"TransientVector" TransientVector''pop! [#_"TransientVector" this]
        (VNode''assert-editable (:root this))
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
                    #_"values" tail (aclone (TransientVector''array-for this, (- (:cnt this) 2)))
                    #_"node" root (VNode''pop-tail (:root this), (:edit (:root this)), (:shift this), (TransientVector''tail-off this))
                    this
                        (cond
                            (nil? root)
                                (-> this
                                    (assoc!! :root (VNode'new (:edit (:root this)), nil, nil))
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

    (defn- #_"TransientVector" TransientVector''assoc! [#_"TransientVector" this, #_"key" key, #_"value" val]
        (when (integer? key) => (throw! "key must be integer")
            (ITransientVector'''assocN! this, (int! key), val)
        )
    )

    (defn- #_"boolean" TransientVector''containsKey [#_"TransientVector" this, #_"key" key]
        (and (integer? key) (< -1 (int! key) (:cnt this)))
    )

    (defn- #_"pair" TransientVector''entryAt [#_"TransientVector" this, #_"key" key]
        (when (integer? key)
            (let-when [#_"int" i (int! key)] (< -1 i (:cnt this))
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
    (declare PersistentVector''seq PersistentVector''rseq PersistentVector''conj PersistentVector''empty PersistentVector''nth)

    (defq PersistentVector [#_"meta" _meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] VecForm
        clojure.lang.Seqable (seq [_] (PersistentVector''seq _))
        clojure.lang.Reversible (rseq [_] (PersistentVector''rseq _))
        clojure.lang.IPersistentCollection (cons [_ o] (PersistentVector''conj _, o)) (empty [_] (PersistentVector''empty _))
        clojure.lang.IPersistentVector
        clojure.lang.Counted (count [_] (:cnt _))
        clojure.lang.Indexed (nth [_, i] (PersistentVector''nth _, i))
    )

    #_inherit
    (defm PersistentVector APersistentVector AFn)

    (defn #_"PersistentVector" PersistentVector'new
        ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] (PersistentVector'new nil, cnt, shift, root, tail))
        ([#_"meta" meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
            (PersistentVector'class. (anew [meta, cnt, shift, root, tail]))
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

    (defn- #_"PersistentVector" PersistentVector''withMeta [#_"PersistentVector" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentVector'new meta, (:cnt this), (:shift this), (:root this), (:tail this))
        )
    )

    (defn- #_"boolean" PersistentVector''equals [#_"PersistentVector" this, #_"Object" that]
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

    (defn- #_"int" PersistentVector''hash [#_"PersistentVector" this]
        (loop-when [#_"int" hash (int 1) #_"int" i (int 0)] (< i (:cnt this)) => (Murmur3'mixCollHash hash, i)
            (recur (+ (* (int 31) hash) (f'hash (Indexed'''nth this, i))) (inc i))
        )
    )

    (defn- #_"int" PersistentVector''tail-off [#_"PersistentVector" this]
        (- (:cnt this) (alength (:tail this)))
    )

    (defn- #_"values" PersistentVector''array-for [#_"PersistentVector" this, #_"int" i]
        (VNode''array-for (:root this), i, (:shift this), (:cnt this), (PersistentVector''tail-off this), (:tail this))
    )

    (defn- #_"value" PersistentVector''value-for [#_"PersistentVector" this, #_"int" i]
        (VNode''value-for (:root this), i, (:shift this), (:cnt this), (PersistentVector''tail-off this), (:tail this))
    )

    (defn- #_"value" PersistentVector''nth
        ([#_"PersistentVector" this, #_"int" i]
            (PersistentVector''value-for this, i)
        )
        ([#_"PersistentVector" this, #_"int" i, #_"value" not-found]
            (when (< -1 i (:cnt this)) => not-found
                (PersistentVector''value-for this, i)
            )
        )
    )

    (defn- #_"PersistentVector" PersistentVector''conj [#_"PersistentVector" this, #_"value" val]
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

    (defn- #_"PersistentVector" PersistentVector''empty [#_"PersistentVector" this]
        (IObj'''withMeta PersistentVector'EMPTY, (:_meta this))
    )

    (defn- #_"PersistentVector" PersistentVector''assocN [#_"PersistentVector" this, #_"int" i, #_"value" val]
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

    (defn- #_"value" PersistentVector''peek [#_"PersistentVector" this]
        (when (pos? (:cnt this))
            (Indexed'''nth this, (dec (:cnt this)))
        )
    )

    (defn- #_"PersistentVector" PersistentVector''pop [#_"PersistentVector" this]
        (case (:cnt this)
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

    (defn- #_"value" PersistentVector''invoke [#_"PersistentVector" this, #_"key" arg]
        (when (integer? arg) => (throw! "arg must be integer")
            (Indexed'''nth this, (int! arg))
        )
    )

    (defn- #_"value" PersistentVector''applyTo [#_"PersistentVector" this, #_"seq" args]
        (case (count args 1)
            1 (IFn'''invoke this, (first args))
        )
    )

    (defn- #_"value" PersistentVector''reduce
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
                        (when-not (reduced? r) => @r
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
                    (when-not (reduced? r) => @r
                        (recur r (+ i (alength a)))
                    )
                )
            )
        )
    )

    (defn- #_"value" PersistentVector''kvreduce [#_"PersistentVector" this, #_"fn" f, #_"value" r]
        (loop-when [r r #_"int" i 0] (< i (:cnt this)) => r
            (let [
                #_"values" a (PersistentVector''array-for this, i)
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

    (defn- #_"IPersistentVector" PersistentVector''assoc [#_"PersistentVector" this, #_"key" key, #_"value" val]
        (when (integer? key) => (throw! "key must be integer")
            (IPersistentVector'''assocN this, (int! key), val)
        )
    )

    (defn- #_"boolean" PersistentVector''containsKey [#_"PersistentVector" this, #_"key" key]
        (and (integer? key) (< -1 (int! key) (:cnt this)))
    )

    (defn- #_"pair" PersistentVector''entryAt [#_"PersistentVector" this, #_"key" key]
        (when (integer? key)
            (let-when [#_"int" i (int! key)] (< -1 i (:cnt this))
                (MapEntry'new key, (Indexed'''nth this, i))
            )
        )
    )

    (defn- #_"value" PersistentVector''valAt
        ([#_"PersistentVector" this, #_"key" key] (PersistentVector''valAt this, key, nil))
        ([#_"PersistentVector" this, #_"key" key, #_"value" not-found]
            (when (integer? key) => not-found
                (let-when [#_"int" i (int! key)] (< -1 i (:cnt this)) => not-found
                    (PersistentVector''value-for this, i)
                )
            )
        )
    )

    (defn- #_"PersistentVector" PersistentVector''slicev [#_"PersistentVector" this, #_"int" start, #_"int" end]
        (cond
            (or (neg? start) (< (:cnt this) end)) (throw! "index is out of bounds")
            (= start end)                         (IPersistentCollection'''empty this) ;; NB. preserves metadata
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

    (defn- #_"PersistentVector" PersistentVector''splicev [#_"PersistentVector" this, #_"PersistentVector" that]
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

    (defn- #_"seq" PersistentVector''seq [#_"PersistentVector" this]
        (when (pos? (:cnt this))
            (VSeq'new this, 0)
        )
    )

    (defn- #_"seq" PersistentVector''rseq [#_"PersistentVector" this]
        (when (pos? (:cnt this))
            (RSeq'new this, (dec (:cnt this)))
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
        (IObject'''toString => print-string)
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

    (defm PersistentVector IKVReduce
        (IKVReduce'''kvreduce => PersistentVector''kvreduce)
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

    #_foreign
    (§ defm PersistentVector #_"Comparable"
        (#_"int" Comparable'''compareTo [#_"PersistentVector" this, #_"IPersistentVector" that]
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

(defn assoc'  [v i x & s] (apply assoc  (vec v) i x s))
(defn conj'   [v   x & s] (apply conj   (vec v)   x s))
(defn into'   [v       s]       (into   (vec v)     s))
(defn peek'   [v]               (peek   (vec v)      ))
(defn pop'    [v]               (pop    (vec v)      ))
(defn update' [v i f & s] (apply update (vec v) i f s))

(defn dissoc' [v i] (let [v (vec v)] (catvec (subvec v 0 i) (subvec v (inc i)))))
)

(about #_"arbace.PersistentQueue"

(about #_"QSeq"
    (declare QSeq''seq QSeq''first QSeq''next)

    (defq QSeq [#_"meta" _meta, #_"seq" f, #_"seq" rseq] #_"SeqForm"
        clojure.lang.ISeq (seq [_] (QSeq''seq _)) (first [_] (QSeq''first _)) (next [_] (QSeq''next _))
    )

    #_inherit
    (defm QSeq ASeq)

    (defn #_"QSeq" QSeq'new
        ([#_"seq" f, #_"seq" rseq] (QSeq'new nil, f, rseq))
        ([#_"meta" meta, #_"seq" f, #_"seq" rseq]
            (QSeq'class. (anew [meta, f, rseq]))
        )
    )

    (defn- #_"QSeq" QSeq''withMeta [#_"QSeq" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (QSeq'new meta, (:f this), (:rseq this))
        )
    )

    (defn- #_"seq" QSeq''seq [#_"QSeq" this]
        this
    )

    (defn- #_"Object" QSeq''first [#_"QSeq" this]
        (first (:f this))
    )

    (defn- #_"seq" QSeq''next [#_"QSeq" this]
        (let [#_"seq" f (next (:f this)) #_"seq" r (:rseq this)]
            (cond
                (some? f) (QSeq'new f, r)
                (some? r) (QSeq'new r, nil)
            )
        )
    )

    (defn- #_"int" QSeq''count [#_"QSeq" this]
        (+ (count (:f this)) (count (:rseq this)))
    )

    (defm QSeq IMeta
        (IMeta'''meta => :_meta)
    )

    (defm QSeq IObj
        (IObj'''withMeta => QSeq''withMeta)
    )

    (defm QSeq Sequential)

    (defm QSeq Seqable
        (Seqable'''seq => QSeq''seq)
    )

    (defm QSeq ISeq
        (ISeq'''first => QSeq''first)
        (ISeq'''next => QSeq''next)
    )

    (defm QSeq Counted
        (Counted'''count => QSeq''count)
    )

    (defm QSeq Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm QSeq IObject
        (IObject'''equals => ASeq''equals)
        (IObject'''toString => print-string)
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
    (defq PersistentQueue [#_"meta" _meta, #_"int" cnt, #_"seq" f, #_"vector" r] VecForm)

    (defn #_"PersistentQueue" PersistentQueue'new [#_"meta" meta, #_"int" cnt, #_"seq" f, #_"vector" r]
        (PersistentQueue'class. (anew [meta, cnt, f, r]))
    )

    (defn- #_"PersistentQueue" PersistentQueue''withMeta [#_"PersistentQueue" this, #_"meta" meta]
        (when-not (= meta (:_meta this)) => this
            (PersistentQueue'new meta, (:cnt this), (:f this), (:r this))
        )
    )

    (def #_"PersistentQueue" PersistentQueue'EMPTY (PersistentQueue'new nil, 0, nil, nil))

    (defn- #_"boolean" PersistentQueue''equals [#_"PersistentQueue" this, #_"Object" that]
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

    (defn- #_"Object" PersistentQueue''peek [#_"PersistentQueue" this]
        (first (:f this))
    )

    (defn- #_"PersistentQueue" PersistentQueue''pop [#_"PersistentQueue" this]
        (when (some? (:f this)) => this ;; hmmm... pop of empty queue -> empty queue?
            (let [#_"seq" f (next (:f this)) #_"vector" r (:r this)
                  [f r]
                    (when (nil? f) => [f r]
                        [(seq r) nil]
                    )]
                (PersistentQueue'new (:_meta this), (dec (:cnt this)), f, r)
            )
        )
    )

    (defn- #_"seq" PersistentQueue''seq [#_"PersistentQueue" this]
        (when (some? (:f this))
            (QSeq'new (:f this), (seq (:r this)))
        )
    )

    (defn- #_"PersistentQueue" PersistentQueue''conj [#_"PersistentQueue" this, #_"Object" o]
        (let [[#_"seq" f #_"vector" r]
                (if (nil? (:f this)) ;; empty
                    [(list o) nil]
                    [(:f this) (conj (or (:r this) (vector)) o)]
                )]
            (PersistentQueue'new (:_meta this), (inc (:cnt this)), f, r)
        )
    )

    (defn- #_"PersistentQueue" PersistentQueue''empty [#_"PersistentQueue" this]
        (with-meta PersistentQueue'EMPTY (:_meta this))
    )

    (defm PersistentQueue IPersistentList Sequential)

    (defm PersistentQueue IMeta
        (IMeta'''meta => :_meta)
    )

    (defm PersistentQueue IObj
        (IObj'''withMeta => PersistentQueue''withMeta)
    )

    (defm PersistentQueue IObject
        (IObject'''equals => PersistentQueue''equals)
        (IObject'''toString => print-string)
    )

    (defm PersistentQueue Hashed
        (Hashed'''hash => Murmur3'hashOrdered)
    )

    (defm PersistentQueue IPersistentStack
        (IPersistentStack'''peek => PersistentQueue''peek)
        (IPersistentStack'''pop => PersistentQueue''pop)
    )

    (defm PersistentQueue Counted
        (Counted'''count => :cnt)
    )

    (defm PersistentQueue Seqable
        (Seqable'''seq => PersistentQueue''seq)
    )

    (defm PersistentQueue IPersistentCollection
        (IPersistentCollection'''conj => PersistentQueue''conj)
        (IPersistentCollection'''empty => PersistentQueue''empty)
    )
)
)

(about #_"arbace.RT"

(about #_"RT"
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
                    (let-when [#_"int" n (int! key)] (< -1 n (count coll))
                        (nth coll n)
                    )
                (satisfies? ITransientSet coll)
                    (ITransientSet'''get coll, key)
                (instance? clojure.lang.ILookup coll)
                    (.valAt coll, key)
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
                    (let [#_"int" n (int! key)]
                        (if (< -1 n (count coll)) (nth coll n) not-found)
                    )
                (satisfies? ITransientSet coll)
                    (if (contains? coll key) (ITransientSet'''get coll, key) not-found)
                (instance? clojure.lang.ILookup coll)
                    (.valAt coll, key, not-found)
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

    (defn #_"Object" RT'contains [#_"Object" coll, #_"Object" key]
        (cond
            (nil? coll)
                false
            (associative? coll)
                (if (Associative'''containsKey coll, key) true false)
            (set? coll)
                (if (IPersistentSet'''contains? coll, key) true false)
            (and (number? key) (or (string? coll) (.isArray (class coll))))
                (let [#_"int" n (int! key)]
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
            (when-some [#_"seq" s (next keyvals)] => (throw! "malformed keyword argslist")
                (when-not (= (first keyvals) key) => s
                    (recur (next s))
                )
            )
        )
    )

    (defn- #_"Object" RT'prepRet [#_"Class" c, #_"Object" x]
        (cond
            (not (or (.isPrimitive c) (= c Boolean))) x
            (boolean? x)                          (if x true false)
            :else                                     x
        )
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
                    (RT'prepRet (.getComponentType (class coll)), (Array/get coll, n))
                (instance? Matcher coll)
                    (.group #_"Matcher" coll, n)
                (map-entry? coll)
                    (let [#_"pair" e coll]
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
                        (RT'prepRet (.getComponentType (class coll)), (Array/get coll, n))
                    )
                (instance? Matcher coll)
                    (let-when [#_"Matcher" m coll] (< n (.groupCount m)) => not-found
                        (.group m, n)
                    )
                (map-entry? coll)
                    (let [#_"pair" e coll]
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

    (defn #_"boolean" RT'booleanCast [#_"Object" x]
        (if (boolean? x) (.booleanValue #_"Boolean" x) (some? x))
    )

    (defn #_"int" RT'intCast-1b [#_"byte"  x] x)
    (defn #_"int" RT'intCast-1c [#_"char"  x] x)
    (defn #_"int" RT'intCast-1i [#_"int"   x] x)

    (defn #_"int" RT'intCast-1l [#_"long" x]
        (let [#_"int" i (int! x)]
            (when (= i x) => (throw! (str "value out of range for int: " x))
                i
            )
        )
    )

    (defn #_"int" RT'intCast [#_"Object" x]
        (cond
            (instance? Integer x) (.intValue #_"Integer" x)
            (number? x)           (RT'intCast-1l (long x))
            :else                 (.charValue #_"Character" x)
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

;;;
 ; Coerce to boolean/int/long.
 ;;
(§ defn boolean [x] (RT'booleanCast x))
(§ defn int     [x] (RT'intCast     x))
(§ defn long    [x] (RT'longCast    x))

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

(about #_"arbace.Var"

(about #_"Var"
    (def #_"ThreadLocal" Var'dvals (ThreadLocal.))

    (defn- #_"String" Var'toString [#_"Namespace" ns, #_"Symbol" sym]
        (if (some? ns)
            (str "#'" (:name ns) "/" sym)
            (str "#<Var: " (or sym "--unnamed--") ">")
        )
    )
)

(about #_"Unbound"
    (defq Unbound [#_"Namespace" ns, #_"Symbol" sym])

    #_inherit
    (defm Unbound AFn)

    (defn #_"Unbound" Unbound'new [#_"Namespace" ns, #_"Symbol" sym]
        (Unbound'class. (anew [ns, sym]))
    )

    (defn- #_"String" Unbound''toString [#_"Unbound" this]
        (str "Unbound: " (Var'toString (:ns this), (:sym this)))
    )

    (defm Unbound IObject
        (IObject'''equals => identical?)
        (IObject'''toString => Unbound''toString)
    )
)

(about #_"Var"
    (declare Var''get)

    (defq Var [#_"Class" class, #_"Namespace" ns, #_"Symbol" sym, #_"Object'" root] RecForm
        java.util.concurrent.Future (get [_] (Var''get _))
    )

    (defn #_"Var" Var'new
        ([#_"Namespace" ns, #_"Symbol" sym] (Var'new ns, sym, (Unbound'new ns, sym)))
        ([#_"Namespace" ns, #_"Symbol" sym, #_"Object" root]
            (Var'class. (anew [Var'class, ns, sym, (atom root)]))
        )
    )

    (defn- #_"meta" Var''meta [#_"Var" this]
        (meta (:root this))
    )

    (defn- #_"meta" Var''alterMeta [#_"Var" this, #_"fn" f, #_"seq" args]
        (apply alter-meta! (:root this) f args)
    )

    (defn- #_"meta" Var''resetMeta [#_"Var" this, #_"meta" m]
        (reset-meta! (:root this) m)
    )

    (defn- #_"String" Var''toString [#_"Var" this]
        (Var'toString (:ns this), (:sym this))
    )

    (defn #_"boolean" Var''hasRoot [#_"Var" this]
        (not (satisfies? Unbound @(:root this)))
    )

    (defn #_"boolean" Var''isBound [#_"Var" this]
        (when-not (instance? clojure.lang.Var this) => (.isBound this)
            (or (Var''hasRoot this) (contains? (first (.get Var'dvals)) this))
        )
    )

    (defn #_"atom" Var''getThreadBinding [#_"Var" this]
        (get (first (.get Var'dvals)) this)
    )

    (defn- #_"Object" Var''get [#_"Var" this]
        @(or (Var''getThreadBinding this) (:root this))
    )

;;;
 ; Gets the value in the var object.
 ;;
(defn var-get [#_"var" x] (Var''get x))

    (defn- #_"Object" Var''set [#_"Var" this, #_"Object" val]
        (let [#_"atom" v (Var''getThreadBinding this)]
            (when (some? v) => (throw! (str "can't change/establish root binding of: " (:sym this) " with var-set/set!"))
                (reset! v val)
            )
        )
    )

;;;
 ; Sets the value in the var object to val.
 ; The var must be thread-locally bound.
 ;;
(defn var-set [#_"var" x val] (Var''set x val))

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
        ;; binding root always clears macro flag
        (alter-meta! this dissoc :macro)
        (reset! (:root this) root)
        nil
    )

    (defn #_"Object" Var''alterRoot [#_"Var" this, #_"fn" f, #_"seq" args]
        (apply swap! (:root this) f args)
    )

    (declare Namespace''intern)

    (defn- #_"Var" Var'intern
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

(declare resolve)

(defmacro var! [v] `(resolve '~v))

(declare the-ns)

;;;
 ; Finds or creates a var named by the symbol name in the namespace
 ; ns (which can be a symbol or a namespace), setting its root binding
 ; to val if supplied. The namespace must exist. The var will adopt
 ; any metadata from the name symbol. Returns the var.
 ;;
(defn intern
    ([ns name]
        (let [v (Var'intern (the-ns ns), name)]
            (when-some [m (meta name)]
                (reset-meta! v m)
            )
            v
        )
    )
    ([ns name o]
        (let [v (Var'intern (the-ns ns), name, o)]
            (when-some [m (meta name)]
                (reset-meta! v m)
            )
            v
        )
    )
)

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
(defn #_"void" push-thread-bindings [#_"{var value}" bindings]
    (let [#_"seq" l (.get Var'dvals)]
        (loop-when [#_"{var atom}" m (first l) #_"seq" s (seq bindings)] (some? s) => (.set Var'dvals, (cons m l))
            (let [#_"pair" e (first s)]
                (recur (assoc m (key e) (atom (val e))) (next s))
            )
        )
    )
    nil
)

;;;
 ; Pop one set of bindings pushed with push-binding before.
 ; It is an error to pop bindings without pushing before.
 ;;
(defn #_"void" pop-thread-bindings []
    (when-some [#_"seq" s (.get Var'dvals)] => (throw! "pop without matching push")
        (.set Var'dvals, (next s))
    )
    nil
)

;;;
 ; Get a map with the Var/value pairs which is currently in effect for the current thread.
 ;;
(defn #_"{var value}" get-thread-bindings []
    (loop-when [#_"{var value}" m (transient (hash-map)) #_"seq" s (seq (first (.get Var'dvals)))] (some? s) => (persistent! m)
        (let [#_"pair" e (first s)]
            (recur (assoc! m (key e) @(val e)) (next s))
        )
    )
)

;;;
 ; binding => var-symbol init-expr
 ;
 ; Creates new bindings for the (already-existing) vars, with the
 ; supplied initial values, executes the exprs in an implicit do, then
 ; re-establishes the bindings that existed before. The new bindings
 ; are made in parallel (unlike let); all init-exprs are evaluated
 ; before the vars are bound to their new values.
 ;;
(defmacro binding [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    (letfn [(var-ize [var-vals]
                (loop-when-recur [v (vector) s (seq var-vals)] s [(conj v `(var! ~(first s)) (second s)) (next (next s))] => (seq v))
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

    (defn- #_"Object" Var''invoke
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

    (defn- #_"Object" Var''applyTo [#_"Var" this, #_"seq" args]
        (IFn'''applyTo @this, args)
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
        (IObject'''toString => Var''toString)
    )

    (defm Var IDeref
        (IDeref'''deref => Var''get)
    )

    (defm Var IFn
        (IFn'''invoke => Var''invoke)
        (IFn'''applyTo => Var''applyTo)
    )
)

;;;
 ; Atomically alters the root binding of var v by applying f to its current value plus any args.
 ;;
(defn alter-var-root [#_"var" v f & args] (Var''alterRoot v f args))

;;;
 ; Returns true if all of the vars provided as arguments have any bound value, root or thread-local.
 ; Implies that deref'ing the provided vars will succeed. Returns true if no vars are provided.
 ;;
(defn bound? [& vars] (every? #(Var''isBound #_"var" %) vars))

;;;
 ; Returns true if all of the vars provided as arguments have thread-local bindings.
 ; Implies that set!'ing the provided vars will succeed. Returns true if no vars are provided.
 ;;
(defn thread-bound? [& vars] (every? #(Var''getThreadBinding #_"var" %) vars))

;;;
 ; defs name to have the root value of the expr iff the named var has no root value,
 ; else expr is unevaluated.
 ;;
(defmacro defonce [name expr]
    `(let-when [v# (def ~name)] (not (Var''hasRoot v#))
        (def ~name ~expr)
    )
)

(defmacro var-get! [x]   `(var-get (var! ~x)))
(defmacro var-set! [x y] `(var-set (var! ~x) ~y))

(defmacro var-swap! [x f & z] `(var-set! ~x (~f (var-get! ~x) ~@z)))
)

(about #_"arbace.Namespace"

(about #_"Namespace"
    (defq Namespace [#_"Class" class, #_"Symbol" name, #_"{Symbol Class|Var}'" mappings, #_"{Symbol Namespace}'" aliases])

    (def #_"{Symbol Namespace}'" Namespace'namespaces (atom (hash-map)))

    (defn #_"seq" Namespace'all []
        (vals @Namespace'namespaces)
    )

;;;
 ; Returns a sequence of all namespaces.
 ;;
(defn all-ns [] (Namespace'all))

    (defn #_"Namespace" Namespace'find [#_"Symbol" name]
        (get @Namespace'namespaces name)
    )

;;;
 ; Returns the namespace named by the symbol or nil if it doesn't exist.
 ;;
(defn find-ns [sym] (Namespace'find sym))

;;;
 ; If passed a namespace, returns it. Else, when passed a symbol,
 ; returns the namespace named by it, throwing an exception if not found.
 ;;
(defn #_"Namespace" the-ns [x]
    (when-not (instance? clojure.lang.Namespace x) => x
        (if (satisfies? Namespace x)
            x
            (or (find-ns x) (throw! (str "no namespace: " x " found")))
        )
    )
)

    (defn- #_"Namespace" Namespace'new [#_"Symbol" name]
        (Namespace'class. (anew [Namespace'class, name, (atom (hash-map)), (atom (hash-map))]))
    )

    (defn #_"Namespace" Namespace'findOrCreate [#_"Symbol" name]
        (or (Namespace'find name)
            (let [#_"Namespace" ns (Namespace'new name)]
                (swap! Namespace'namespaces assoc name ns)
                ns
            )
        )
    )

;;;
 ; Create a new namespace named by the symbol if one doesn't already exist,
 ; returns it or the already-existing namespace of the same name.
 ;;
(defn create-ns [sym] (Namespace'findOrCreate sym))

    (defn #_"Namespace" Namespace'remove [#_"Symbol" name]
        (when-not (= name 'arbace.core) => (throw! "cannot remove core namespace")
            (get (first (swap-vals! Namespace'namespaces dissoc name)) name)
        )
    )

;;;
 ; Removes the namespace named by the symbol. Use with caution.
 ; Cannot be used to remove the arbace namespace.
 ;;
(defn remove-ns [sym] (Namespace'remove sym))

    (defn- #_"String" Namespace''toString [#_"Namespace" this]
        (:name (:name this))
    )

;;;
 ; Returns the name of the namespace, a symbol.
 ;;
(defn ns-name [ns] (:name (the-ns ns)))

    (defn #_"map" Namespace''getMappings [#_"Namespace" this]
        @(:mappings this)
    )

;;;
 ; Returns a map of all the mappings for the namespace.
 ;;
(defn ns-map [ns] (Namespace''getMappings (the-ns ns)))

    (defn #_"Object" Namespace''getMapping [#_"Namespace" this, #_"Symbol" name]
        (when-not (instance? clojure.lang.Namespace this) => (.getMapping this, name)
            (get @(:mappings this) name)
        )
    )

(defn- filter-key [f f? m]
    (loop-when-recur [s (seq m) m (transient (hash-map))]
                     s
                     [(next s) (let [e (first s)] (if (f? (f e)) (assoc m (key e) (val e)) m))]
                  => (persistent! m)
    )
)

;;;
 ; Returns a map of the intern mappings for the namespace.
 ;;
(defn ns-interns [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"var" v] (and (var? v) (= ns (:ns v)))) (ns-map ns))
    )
)

;;;
 ; Returns a map of the public intern mappings for the namespace.
 ;;
(defn ns-publics [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"var" v] (and (var? v) (= ns (:ns v)) (Var''isPublic v))) (ns-map ns))
    )
)

;;;
 ; Returns a map of the refer mappings for the namespace.
 ;;
(defn ns-refers [ns]
    (let [ns (the-ns ns)]
        (filter-key val (fn [#_"var" v] (and (var? v) (not= ns (:ns v)))) (ns-map ns))
    )
)

    (defn- #_"void" Namespace''warnOrFailOnReplace [#_"Namespace" this, #_"Symbol" sym, #_"Object" o, #_"var" var]
        (or
            (when (var? o)
                (when (= (:ns o) this) => (throw! (str sym " already refers to: " o " in namespace: " (:name this)))
                    :ok
                )
            )
            (.println -/*err*, (str "WARNING: " sym " already refers to: " o " in namespace: " (:name this) ", being replaced by: " var))
        )
        nil
    )

    (defn #_"var" Namespace''intern [#_"Namespace" this, #_"Symbol" sym]
        (when-not (instance? clojure.lang.Namespace this) => (.intern this, sym)
            (when (nil? (:ns sym)) => (throw! "can't intern namespace-qualified symbol")
                (let [#_"Object" o
                        (or (get @(:mappings this) sym)
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
    )

    (defn #_"var" Namespace''refer [#_"Namespace" this, #_"Symbol" sym, #_"var" var]
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

(declare ^:dynamic *arbace-ns*)

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
            (let [es* (set (:exclude fs*)) rs* (or (:rename fs*) (hash-map))]
                (doseq [x (remove es* s)]
                    (when-some [v (ps* x)] => (throw! (str x (if (get (ns-interns ns) x) " is not public" " does not exist")))
                        (Namespace''refer *arbace-ns* (or (rs* x) x) v)
                    )
                )
            )
        )
    )
)

(defmacro refer-arbace [& filters]
    `(refer '~'arbace.core ~@filters)
)

    (defn #_"void" Namespace''unmap [#_"Namespace" this, #_"Symbol" sym]
        (when (nil? (:ns sym)) => (throw! "can't unintern namespace-qualified symbol")
            (swap! (:mappings this) dissoc sym)
        )
        nil
    )

;;;
 ; Removes the mappings for the symbol from the namespace.
 ;;
(defn ns-unmap [ns sym] (Namespace''unmap (the-ns ns) sym))

    (defn #_"var" Namespace''findInternedVar [#_"Namespace" this, #_"Symbol" name]
        (let [#_"Object" o (get @(:mappings this) name)]
            (when (and (var? o) (= (:ns o) this))
                o
            )
        )
    )

;;;
 ; Returns the global var named by the namespace-qualified symbol,
 ; or nil if no var with that name.
 ;;
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
        @(:aliases this)
    )

;;;
 ; Returns a map of the aliases for the namespace.
 ;;
(defn ns-aliases [ns]
    (Namespace''getAliases (the-ns ns))
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

;;;
 ; Add an alias in the current namespace to another namespace.
 ; Arguments are two symbols: the alias to be used, and [the symbolic name of] the target namespace.
 ; Use :as in the ns macro in preference to calling this directly.
 ;;
(defn alias [sym ns]
    (Namespace''addAlias *arbace-ns* sym (the-ns ns))
)

    (defn #_"void" Namespace''removeAlias [#_"Namespace" this, #_"Symbol" alias]
        (swap! (:aliases this) dissoc alias)
        nil
    )

;;;
 ; Removes the alias for the symbol from the namespace.
 ;;
(defn ns-unalias [ns sym]
    (Namespace''removeAlias (the-ns ns) sym)
)

    (defm Namespace IObject
        (IObject'''equals => identical?)
        (IObject'''toString => Namespace''toString)
    )
)

;;;
 ; Sets *arbace-ns* to the namespace named by name (unevaluated), creating it if needed.
 ;;
(defmacro arbace-ns [n & s]
    (let [m (let-when [m (first s)] (map? m) m) s (if m (next s) s) n (if m (vary-meta n merge m) n) m (meta n)]
        `(do
            (ß in-ns '~n)
            ~@(when m
                `((reset-meta! (Namespace'find '~n) ~m))
            )
            ~@(when (and (not= n 'arbace.core) (not-any? #(= :refer-arbace (first %)) s))
                `((refer '~'arbace.core))
            )
            ~@(map (fn [[k & s]] `(~(symbol "arbace.core" (name k)) ~@(map #(list 'quote %) s))) s)
            nil
        )
    )
)
)

(about #_"cloiure.core"

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
                (let [m' (gensym "m__") as (:as x) or* (:or x)
                      v (conj v m' y m' `(if (seq? ~m') (apply hash-map ~m') ~m')) v (if as (conj v as m') v)
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

;;;
 ; binding => binding-form init-expr
 ;
 ; Evaluates the exprs in a lexical context in which the symbols in the
 ; binding-forms are bound to their respective init-exprs or parts therein.
 ;;
(defmacro let [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
    `(let* ~(destructure bindings) ~@body)
)

(defn- maybe-destructured [pars body]
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

;; redefine fn with destructuring

;;;
 ; params => positional-params*, or positional-params* & next-param
 ; positional-param => binding-form
 ; next-param => binding-form
 ; name => symbol
 ;
 ; Defines a function.
 ;;
(defmacro fn [& s]
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
(defmacro loop [bindings & body]
    (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector"
    )
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
    ;;;
     ; A good fdecl looks like (([a] ...) ([a b] ...)) near the end of defn.
     ;;
    (defn- assert-valid-fdecl [fdecl]
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

    ;;;
     ; Same as (def name (fn [params*] exprs*)) or (def name (fn ([params*] exprs*)+)) with any attrs added to the var metadata.
     ;;
    (defmacro defn [fname & s]
        ;; note: cannot delegate this check to def because of the call to (with-meta name ...)
        (when (symbol? fname) => (throw! "first argument to defn must be a symbol")
            (let [m (if (map?    (first s)) (first s) (hash-map))
                  s (if (map?    (first s)) (next s)   s)
                  s (if (vector? (first s)) (list s)   s)
                  _ (assert-valid-fdecl s)
                  m (let [inline (:inline m) ifn (first inline) iname (second inline)]
                        (when (and (= 'fn ifn) (not (symbol? iname))) => m
                            ;; inserts the same fn name to the inline fn if it does not have one
                            (assoc m :inline (cons ifn (cons (symbol (str (:name fname) "__inliner")) (next inline))))
                        )
                    )
                  m (conj (or (meta fname) (hash-map)) m)]
                (list 'def (with-meta fname m) (cons `fn s))
            )
        )
    )

    ;;;
     ; Like defn, but the resulting function name is declared as a macro
     ; and will be used as a macro by the compiler when it is called.
     ;;
    (defmacro defmacro [name & args]
        (let [[m s] (split-with map? args) s (if (vector? (first s)) (list s) s)
              s (map (fn [[bindings & body]] (cons (apply vector '&form '&env bindings) body)) s)]
            `(do (defn ~name ~@m ~@s) (Var''setMacro (var ~name)) (var ~name))
        )
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
        (when (seq s) => (list)
            (let [a (anew s)]
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
                    (vector) (partition 2 bindings)
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
        (into (vector) (for [i (range (inc n))] (.group m i)))
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
 ; Takes any nested combination of sequential things (lists, vectors, etc.)
 ; and returns their contents as a single, flat sequence.
 ; (flatten nil) returns an empty sequence.
 ;;
(defn flatten [s] (remove sequential? (next (tree-seq sequential? seq s))))

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
    (let [mem (atom (hash-map))]
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

(about #_"case"

(defn- shift-mask [shift mask x] (-> x (>> shift) (& mask)))

(def- max-mask-bits 13)
(def- max-switch-table-size (<< 1 max-mask-bits))

;;;
 ; Takes a collection of hashes and returns [shift mask] or nil if none found.
 ;;
(defn- maybe-min-hash [hashes]
    (first
        (-/filter (fn [[s m]] (apply distinct? (map #(shift-mask s m %) hashes)))
            (-/for [mask (map #(dec (<< 1 %)) (range 1 (inc max-mask-bits))) shift (range 0 31)]
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
    (< (-/- (apply max (seq ints)) (apply min (seq ints))) max-switch-table-size)
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
            (loop-when-recur [m (hash-map) ks tests vs thens]
                             (and ks vs)
                             [(update m (f'hashcode (first ks)) (fnil conj (vector)) [(first ks) (first vs)]) (next ks) (next vs)]
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
                (hash-map) buckets
            )
          skip-check
            (->> buckets
                (filter #(< 1 (count (second %))))
                (map first)
                (into (hash-set))
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
    (let [hashes (into (hash-set) (map f'hashcode tests))]
        (if (= (count tests) (count hashes))
            (if (fits-table? hashes)
                ;; compact case ints, no shift-mask
                [0 0 (case-map f'hashcode identity tests thens) :compact]
                (let [[shift mask] (or (maybe-min-hash hashes) [0 0])]
                    (if (zero? mask)
                        ;; sparse case ints, no shift-mask
                        [0 0 (case-map f'hashcode identity tests thens) :sparse]
                        ;; compact case ints, with shift-mask
                        [shift mask (case-map #(shift-mask shift mask (f'hashcode %)) identity tests thens) :compact]
                    )
                )
            )
            ;; resolve hash collisions and try again
            (let [[tests thens skip-check] (merge-hash-collisions expr-sym default tests thens)
                  [shift mask case-map switch-type] (prep-hashes expr-sym default tests thens)
                  skip-check
                    (if (zero? mask)
                        skip-check
                        (into (hash-set) (map #(shift-mask shift mask %) skip-check))
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
(defmacro case [e & clauses]
    (let [e' (gensym)
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
                        (hash-map) pairs
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
)

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
(defn reduce
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
(§ -/defprotocol KVReduce
    (kv-reduce [m f r])
)

(§ -/extend-protocol KVReduce
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
(defn into
    ([] (vector))
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
 ; Returns a map of the elements of coll keyed by the result of
 ; f on each element. The value at each key will be a vector of the
 ; corresponding elements, in the order they appeared in coll.
 ;;
(defn group-by [f s] (reduce! #(let [k (f %2)] (assoc! %1 k (conj (get %1 k (vector)) %2))) (hash-map) s))

;;;
 ; Returns a map from distinct items in coll to the number of times they appear.
 ;;
(defn frequencies [s] (reduce! #(assoc! %1 %2 (inc (get %1 %2 0))) (hash-map) s))

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
    ([] (hash-set))
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
 ; Returns a map containing only those entries in m whose key is in keys.
 ;;
(defn select-keys [m keys] (with-meta (into (hash-map) (map #(find m %) keys)) (meta m)))

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
                (assoc m ik (conj (get m ik (hash-set)) x))
            )
        )
        (hash-map) xrel
    )
)

;;;
 ; Returns the map with the vals mapped to the keys.
 ;;
(defn map-invert [m] (reduce (fn [m [k v]] (assoc m v k)) (hash-map) m))

;;;
 ; When passed 2 rels, returns the rel corresponding to the natural join.
 ; When passed an additional keymap, joins on the corresponding keys.
 ;;
(defn join
    ([a b] ;; natural join
        (when (and (seq a) (seq b)) => (hash-set)
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
                    (hash-set) b
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
                (hash-set) b
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
)

(about #_"graalfn.HotSpot"

(about #_"HotSpot"
    (def #_"HotSpotJVMCIRuntime" JVMCI'runtime (HotSpotJVMCIRuntime/runtime))

    (def #_"CompilerToVM"    HotSpot'native (#_"HotSpotJVMCIRuntime" .getCompilerToVM JVMCI'runtime))
    (def #_"HotSpotVMConfig" HotSpot'config (#_"HotSpotJVMCIRuntime" .getConfig       JVMCI'runtime))

    (def #_"boolean" HotSpot'useG1GC (.getFlag HotSpot'config, "UseG1GC", Boolean))

    (def #_"boolean" HotSpot'useCompressedOops          (.getFlag HotSpot'config, "UseCompressedOops",          Boolean))
    (def #_"boolean" HotSpot'useCompressedClassPointers (.getFlag HotSpot'config, "UseCompressedClassPointers", Boolean))

    (when-not (and HotSpot'useG1GC HotSpot'useCompressedOops HotSpot'useCompressedClassPointers)
        (throw! "use G1 with compressed oops")
    )
)
)

(about #_"arbace.Compiler"
    (defp Expr
        (#_"gen" Expr'''emit [#_"Expr" this, #_"Context" context, #_"map" scope, #_"gen" gen])
    )

    (defp Assignable
        (#_"gen" Assignable'''emitAssign [#_"Assignable" this, #_"Context" context, #_"map" scope, #_"gen" gen, #_"Expr" val])
    )

    (defp Recur)
)

(about #_"arbace.Compiler"
    (defp LiteralExpr)
    (defp AssignExpr)
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
    (defp CaseExpr)
    (defp MonitorExpr)
    (defp CatchClause)
    (defp TryExpr)
    (defp ThrowExpr)
)

(about #_"arbace.Cache"

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

(about #_"arbace.Compiler"

(about #_"asm"
    (defn- #_"gen" Gen'new [] (vector))

    (defn- #_"label" Gen''label [#_"gen" gen] (atom nil))

    (defn- Gen''mark
        (#_"label" [#_"gen" gen] (atom (count gen)))
        (#_"gen" [#_"gen" gen, #_"label" label] (reset! label (count gen)) (conj gen [:label label]))
    )

    (defn- #_"gen" Gen''and             [#_"gen" gen]                   (conj gen :and))
    (defn- #_"gen" Gen''anew            [#_"gen" gen]                   (conj gen :anew))
    (defn- #_"gen" Gen''aset            [#_"gen" gen]                   (conj gen :aset))
    (defn- #_"gen" Gen''call            [#_"gen" gen, #_"Symbol" name, #_"int" arity] (conj gen [:call name arity]))
    (defn- #_"gen" Gen''create          [#_"gen" gen]                   (conj gen :create))
    (defn- #_"gen" Gen''dup             [#_"gen" gen]                   (conj gen :dup))
    (defn- #_"gen" Gen''get             [#_"gen" gen, #_"Symbol" name]  (conj gen [:get name]))
    (defn- #_"gen" Gen''goto            [#_"gen" gen, #_"label" label]  (conj gen [:goto label]))
    (defn- #_"gen" Gen''if-eq?          [#_"gen" gen, #_"label" label]  (conj gen [:if-eq? label]))
    (defn- #_"gen" Gen''if-ne?          [#_"gen" gen, #_"label" label]  (conj gen [:if-ne? label]))
    (defn- #_"gen" Gen''if-nil?         [#_"gen" gen, #_"label" label]  (conj gen [:if-nil? label]))
    (defn- #_"gen" Gen''if-not          [#_"gen" gen, #_"label" label]  (conj gen [:if-not label]))
    (defn- #_"gen" Gen''init            [#_"gen" gen]                   (conj gen :init))
    (defn- #_"gen" Gen''load            [#_"gen" gen, #_"int" index]    (conj gen [:load index]))
    (defn- #_"gen" Gen''monitor-enter   [#_"gen" gen]                   (conj gen :monitor-enter))
    (defn- #_"gen" Gen''monitor-exit    [#_"gen" gen]                   (conj gen :monitor-exit))
    (defn- #_"gen" Gen''number?         [#_"gen" gen]                   (conj gen :number?))
    (defn- #_"gen" Gen''pop             [#_"gen" gen]                   (conj gen :pop))
    (defn- #_"gen" Gen''push            [#_"gen" gen, #_"value" value]  (conj gen [:push value]))
    (defn- #_"gen" Gen''put             [#_"gen" gen, #_"Symbol" name]  (conj gen [:put name]))
    (defn- #_"gen" Gen''return          [#_"gen" gen]                   (conj gen :return))
    (defn- #_"gen" Gen''store           [#_"gen" gen, #_"int" index]    (conj gen [:store index]))
    (defn- #_"gen" Gen''shr             [#_"gen" gen]                   (conj gen :shr))
    (defn- #_"gen" Gen''swap            [#_"gen" gen]                   (conj gen :swap))
    (defn- #_"gen" Gen''throw           [#_"gen" gen]                   (conj gen :throw))

    (defn- #_"gen" Gen''lookup-switch [#_"gen" gen, #_"ints" values, #_"labels" labels, #_"label" default]
        (conj gen [:lookup-switch (vec values) (mapv deref labels) @default])
    )

    (defn- #_"gen" Gen''table-switch [#_"gen" gen, #_"int" low, #_"int" high, #_"labels" labels, #_"label" default]
        (conj gen [:table-switch low high (mapv deref labels) @default])
    )

    (defn- #_"gen" Gen''try-catch-finally [#_"gen" gen, #_"label" start, #_"label" end, #_"label" finally]
        (conj gen [:try-catch-finally @start @end @finally])
    )
)

(def Context'enum-set
    (hash-set
        :Context'STATEMENT ;; value ignored
        :Context'EXPRESSION ;; value required
        :Context'RETURN ;; tail position relative to enclosing recur frame
    )
)

(about #_"Compiler"
    (def #_"int" Compiler'MAX_POSITIONAL_ARITY 9)

    (defn #_"Namespace" Compiler'namespaceFor
        ([#_"Symbol" sym] (Compiler'namespaceFor *arbace-ns*, sym))
        ([#_"Namespace" inns, #_"Symbol" sym]
            ;; note, presumes non-nil sym.ns
            (let [#_"Symbol" nsSym (symbol (:ns sym))]
                ;; first check against currentNS' aliases, otherwise check the Namespaces map
                (or (Namespace''getAlias inns, nsSym) (find-ns nsSym))
            )
        )
    )

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
                (let [#_"Object" o (Namespace''getMapping *arbace-ns*, sym)]
                    (cond
                        (nil? o)   (symbol (:name (:name *arbace-ns*)) (:name sym))
                        (class? o) (symbol (.getName #_"Class" o))
                        (var? o)   (symbol (:name (:name (:ns o))) (:name (:sym o)))
                    )
                )
        )
    )

    (defn #_"Var" Compiler'lookupVar [#_"Symbol" sym, #_"boolean" intern?]
        (let [sym (symbol! sym)]
            ;; note - ns-qualified vars in other namespaces must already exist
            (cond
                (some? (:ns sym))
                    (when-some [#_"Namespace" ns (Compiler'namespaceFor sym)]
                        (let [#_"Symbol" name (symbol (:name sym))]
                            (if (and intern? (= ns *arbace-ns*))
                                (Namespace''intern ns, name)
                                (Namespace''findInternedVar ns, name)
                            )
                        )
                    )
                (= sym 'ns)    #'ns
                (= sym 'in-ns) #'in-ns
                :else ;; is it mapped?
                    (let [#_"Object" o (Namespace''getMapping *arbace-ns*, sym)]
                        (cond
                            (nil? o) ;; introduce a new var in the current ns
                                (when intern?
                                    (Namespace''intern *arbace-ns*, (symbol (:name sym)))
                                )
                            (var? o)
                                o
                            :else
                                (throw! (str "expecting var, but " sym " is mapped to " o))
                        )
                    )
            )
        )
    )

    (defn #_"Var" Compiler'maybeMacro [#_"Object" op, #_"map" scope]
        ;; no local macros for now
        (when-not (and (symbol? op) (some? (get @(:'local-env scope) op)))
            (when (or (symbol? op) (var? op))
                (let [#_"Var" v (if (var? op) op (Compiler'lookupVar op, false))]
                    (when (and (some? v) (get (meta v) :macro))
                        (when (or (= (:ns v) *arbace-ns*) (not (get (meta v) :private))) => (throw! (str "var: " v " is private"))
                            v
                        )
                    )
                )
            )
        )
    )

    (defn #_"IFn" Compiler'maybeInline [#_"Object" op, #_"int" arity, #_"map" scope]
        ;; no local inlines for now
        (when-not (and (symbol? op) (some? (get @(:'local-env scope) op)))
            (when (or (symbol? op) (var? op))
                (when-some [#_"Var" v (if (var? op) op (Compiler'lookupVar op, false))]
                    (when (or (= (:ns v) *arbace-ns*) (not (get (meta v) :private))) => (throw! (str "var: " v " is private"))
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
        (let [sym (symbol! sym)]
            ;; note - ns-qualified vars must already exist
            (cond
                (some? (:ns sym))
                    (when-some [#_"Namespace" ns (Compiler'namespaceFor n, sym)]                          => (throw! (str "no such namespace: " (:ns sym)))
                        (when-some [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))]      => (throw! (str "no such var: " sym))
                            (when (or (= (:ns v) *arbace-ns*) (not (get (meta v) :private)) allowPrivate) => (throw! (str "var: " sym " is private"))
                                v
                            )
                        )
                    )
                (= sym 'ns)    #'ns
                (= sym 'in-ns) #'in-ns
                :else          (or (Namespace''getMapping n, sym) (throw! (str "unable to resolve symbol: " sym " in this context")))
            )
        )
    )

    (defn #_"Object" Compiler'resolve
        ([#_"Symbol" sym                          ] (Compiler'resolveIn *arbace-ns*, sym, false       ))
        ([#_"Symbol" sym, #_"boolean" allowPrivate] (Compiler'resolveIn *arbace-ns*, sym, allowPrivate))
    )

    (defn #_"Object" Compiler'maybeResolveIn [#_"Namespace" n, #_"Symbol" sym]
        (let [sym (symbol! sym)]
            ;; note - ns-qualified vars must already exist
            (cond
                (some? (:ns sym))
                    (when-some [#_"Namespace" ns (Compiler'namespaceFor n, sym)]
                        (when-some [#_"Var" v (Namespace''findInternedVar ns, (symbol (:name sym)))]
                            v
                        )
                    )
                (= sym 'ns)    #'ns
                (= sym 'in-ns) #'in-ns
                :else          (Namespace''getMapping n, sym)
            )
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
)

(about #_"LiteralExpr"
    (defr LiteralExpr [])

    (defn #_"LiteralExpr" LiteralExpr'new [#_"Object" value]
        (merge (class! LiteralExpr)
            (hash-map
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
                    (case value
                        nil                 LiteralExpr'NIL
                        true                LiteralExpr'TRUE
                        false               LiteralExpr'FALSE
                        (cond
                            (string? value) (LiteralExpr'new (.intern #_"String" value))
                            :else           (LiteralExpr'new value)
                        )
                    )
                )
            )
        )
    )

    (defn- #_"gen" LiteralExpr''emit [#_"LiteralExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (Gen''push gen, (:value this))
        )
    )

    (defm LiteralExpr Expr
        (Expr'''emit => LiteralExpr''emit)
    )
)

(about #_"AssignExpr"
    (defr AssignExpr [])

    (defn #_"AssignExpr" AssignExpr'new [#_"Assignable" target, #_"Expr" val]
        (merge (class! AssignExpr)
            (hash-map
                #_"Assignable" :target target
                #_"Expr" :val val
            )
        )
    )

    (declare Compiler'analyze)

    (defn #_"Expr" AssignExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (when (= (count form) 3) => (throw! "malformed assignment, expecting (set! target val)")
            (let [#_"Expr" target (Compiler'analyze (second form), scope)]
                (when (satisfies? Assignable target) => (throw! "invalid assignment target")
                    (AssignExpr'new target, (Compiler'analyze (third form), scope))
                )
            )
        )
    )

    (defn- #_"gen" AssignExpr''emit [#_"AssignExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (Assignable'''emitAssign (:target this), context, scope, gen, (:val this))
    )

    (defm AssignExpr Expr
        (Expr'''emit => AssignExpr''emit)
    )
)

(about #_"UnresolvedVarExpr"
    (defr UnresolvedVarExpr [])

    (defn #_"UnresolvedVarExpr" UnresolvedVarExpr'new [#_"Symbol" symbol]
        (merge (class! UnresolvedVarExpr)
            (hash-map
                #_"Symbol" :symbol symbol
            )
        )
    )

    (defn- #_"gen" UnresolvedVarExpr''emit [#_"UnresolvedVarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        gen
    )

    (defm UnresolvedVarExpr Expr
        (Expr'''emit => UnresolvedVarExpr''emit)
    )
)

(about #_"VarExpr"
    (defr VarExpr [])

    (defn #_"VarExpr" VarExpr'new [#_"Var" var]
        (merge (class! VarExpr)
            (hash-map
                #_"Var" :var var
            )
        )
    )

    (defn- #_"gen" VarExpr''emit [#_"VarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Gen''push gen, (:var this))
            gen (Gen''call gen, 'Var''get, 1)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defn- #_"gen" VarExpr''emitAssign [#_"VarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen, #_"Expr" val]
        (let [
            gen (Gen''push gen, (:var this))
            gen (Expr'''emit val, :Context'EXPRESSION, scope, gen)
            gen (Gen''call gen, 'Var''set, 2)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm VarExpr Expr
        (Expr'''emit => VarExpr''emit)
    )

    (defm VarExpr Assignable
        (Assignable'''emitAssign => VarExpr''emitAssign)
    )
)

(about #_"TheVarExpr"
    (defr TheVarExpr [])

    (defn #_"TheVarExpr" TheVarExpr'new [#_"Var" var]
        (merge (class! TheVarExpr)
            (hash-map
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

    (defn- #_"gen" TheVarExpr''emit [#_"TheVarExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (Gen''push gen, (:var this))
        )
    )

    (defm TheVarExpr Expr
        (Expr'''emit => TheVarExpr''emit)
    )
)

(about #_"BodyExpr"
    (defr BodyExpr [])

    (defn #_"BodyExpr" BodyExpr'new [#_"vector" exprs]
        (merge (class! BodyExpr)
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

    (defn- #_"gen" BodyExpr''emit [#_"BodyExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
    (defr MetaExpr [])

    (defn #_"MetaExpr" MetaExpr'new [#_"Expr" expr, #_"Expr" meta]
        (merge (class! MetaExpr)
            (hash-map
                #_"Expr" :expr expr
                #_"Expr" :meta meta
            )
        )
    )

    (defn- #_"gen" MetaExpr''emit [#_"MetaExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Expr'''emit (:meta this), :Context'EXPRESSION, scope, gen)
            gen (Gen''call gen, 'IObj'''withMeta, 2)
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
    (defr IfExpr [])

    (defn #_"IfExpr" IfExpr'new [#_"Expr" test, #_"Expr" then, #_"Expr" else]
        (merge (class! IfExpr)
            (hash-map
                #_"Expr" :test test
                #_"Expr" :then then
                #_"Expr" :else else
            )
        )
    )

    ;; (if test then) or (if test then else)
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

    (defn- #_"gen" IfExpr''emit [#_"IfExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
    (defr MapExpr [])

    (defn #_"MapExpr" MapExpr'new [#_"vector" args]
        (merge (class! MapExpr)
            (hash-map
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

    (defn- #_"gen" MapExpr''emit [#_"MapExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
            gen (Gen''call gen, (if (or (and literal? unique?) (<= n 2)) 'RT'mapUniqueKeys 'RT'map), 1)
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
    (defr SetExpr [])

    (defn #_"SetExpr" SetExpr'new [#_"vector" args]
        (merge (class! SetExpr)
            (hash-map
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

    (defn- #_"gen" SetExpr''emit [#_"SetExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (when (seq (:args this)) => (Gen''push gen, PersistentHashSet'EMPTY)
                    (let [gen (Compiler'emitArgs scope, gen, (:args this))]
                        (Gen''call gen, 'PersistentHashSet'createWithCheck, 1)
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
    (defr VectorExpr [])

    (defn #_"VectorExpr" VectorExpr'new [#_"vector" args]
        (merge (class! VectorExpr)
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
              #_"Expr" e
                (when literal? => (VectorExpr'new args)
                    (LiteralExpr'new (mapv :value args))
                )]
            (when-some [#_"meta" m (meta form)] => e
                (MetaExpr'new e, (MapExpr'parse m, scope))
            )
        )
    )

    (defn- #_"gen" VectorExpr''emit [#_"VectorExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (when (seq (:args this)) => (Gen''push gen, PersistentVector'EMPTY)
                    (let [gen (Compiler'emitArgs scope, gen, (:args this))]
                        (Gen''call gen, 'vec, 1)
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
    (defr InvokeExpr [])

    (defn #_"InvokeExpr" InvokeExpr'new [#_"Expr" fexpr, #_"vector" args]
        (merge (class! InvokeExpr)
            (hash-map
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

    (defn- #_"gen" InvokeExpr''emit [#_"InvokeExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:fexpr this), :Context'EXPRESSION, scope, gen)
            gen (Compiler'emitArgs scope, gen, (:args this))
            gen (Gen''call gen, 'IFn'''applyTo, 2)
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
    (defr LocalBinding [])

    (defn #_"LocalBinding" LocalBinding'new [#_"Symbol" sym, #_"Expr" init, #_"int" idx]
        (merge (class! LocalBinding)
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
    (defr LocalBindingExpr [])

    (defn #_"LocalBindingExpr" LocalBindingExpr'new [#_"LocalBinding" lb]
        (merge (class! LocalBindingExpr)
            (hash-map
                #_"LocalBinding" :lb lb
            )
        )
    )

    (declare FnMethod''emitLocal)

    (defn- #_"gen" LocalBindingExpr''emit [#_"LocalBindingExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-not (= context :Context'STATEMENT) => gen
            (FnMethod''emitLocal (:fm scope), gen, (:lb this))
        )
    )

    (defm LocalBindingExpr Expr
        (Expr'''emit => LocalBindingExpr''emit)
    )
)

(about #_"FnMethod"
    (defr FnMethod [])

    (defn #_"FnMethod" FnMethod'new [#_"FnExpr" fun, #_"FnMethod" parent]
        (merge (class! FnMethod)
            (hash-map
                #_"FnExpr" :fun fun
                ;; when closures are defined inside other closures,
                ;; the closed over locals need to be propagated to the enclosing fun
                #_"FnMethod" :parent parent
                ;; uid->localbinding
                #_"{int LocalBinding}'" :'locals (atom (hash-map))
                #_"Integer" :arity nil
                #_"Expr" :body nil
            )
        )
    )

    (defn #_"FnMethod" FnMethod'parse [#_"FnExpr" fun, #_"seq" form, #_"map" scope]
        ;; ([args] body...)
        (let [
            scope
                (-> scope
                    (update :fm (partial FnMethod'new fun))
                    (update :'local-env (comp atom deref))
                    (assoc :'local-num (atom 0))
                )
            _
                (when-some [#_"Symbol" f (:fname fun)]
                    (let [#_"LocalBinding" lb (LocalBinding'new f, nil, @(:'local-num scope))]
                        (swap! (:'local-env scope) assoc (:sym lb) lb)
                        (swap! (:'locals (:fm scope)) assoc (:uid lb) lb)
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
                                            arity (if-not variadic? (inc arity) (-/- (inc arity)))
                                            #_"LocalBinding" lb (LocalBinding'new sym, nil, (swap! (:'local-num scope) inc))
                                        ]
                                            (swap! (:'local-env scope) assoc (:sym lb) lb)
                                            (swap! (:'locals (:fm scope)) assoc (:uid lb) lb)
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
            (assoc (:fm scope) :body (BodyExpr'parse (next form), :Context'RETURN, scope))
        )
    )

    (defn #_"gen" FnMethod''emitLocal [#_"FnMethod" this, #_"gen" gen, #_"LocalBinding" lb]
        (if (contains? @(:'closes (:fun this)) (:uid lb))
            (let [
                gen (Gen''load gen, 0) ;; this
                gen (Gen''get gen, (:sym lb))
            ]
                gen
            )
            (Gen''load gen, (:idx lb))
        )
    )

    (defn #_"gen" FnMethod''compile [#_"FnMethod" this]
        (let [
            #_"map" scope (-/hash-map :fm this)
            #_"gen" gen (Gen'new)
            scope (assoc scope :loop-label (Gen''mark gen))
            gen (Expr'''emit (:body this), :Context'RETURN, scope, gen)
        ]
            (Gen''return gen)
        )
    )
)

(about #_"FnExpr"
    (defr FnExpr [])

    (defn #_"FnExpr" FnExpr'new []
        (merge (class! FnExpr)
            (hash-map
                #_"Symbol" :fname nil
                #_"{int FnMethod}" :regulars nil
                ;; optional variadic overload (there can only be one)
                #_"FnMethod" :variadic nil
                ;; uid->localbinding
                #_"{int LocalBinding}'" :'closes (atom (hash-map))
            )
        )
    )

    (defn #_"Expr" FnExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [
            #_"FnExpr" fun (FnExpr'new)
            ;; arglist might be preceded by symbol naming this fn
            [fun form]
                (when (symbol? (second form)) => [fun form]
                    [(assoc fun :fname (second form)) (cons (symbol! 'fn*) (next (next form)))]
                )
            ;; now (fn [args] body...) or (fn ([args] body...) ([args2] body2...) ...)
            ;; turn former into latter
            form
                (when (vector? (second form)) => form
                    (list (symbol! 'fn*) (next form))
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
                        (loop-when-recur [#_"int" n (-/- (:arity variadic))] (<= n Compiler'MAX_POSITIONAL_ARITY) [(inc n)]
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

    (defn- #_"gen" FnExpr''emit [#_"FnExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        ;; emitting a Fn means constructing an instance, feeding closed-overs from enclosing scope, if any
        ;; fun arg is enclosing fun, not this
        (let [
            gen (Gen''create gen)
            gen (Gen''dup gen)
            gen (reduce (partial FnMethod''emitLocal (:fm scope)) gen (vals @(:'closes this)))
            gen (Gen''init gen)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm FnExpr Expr
        (Expr'''emit => FnExpr''emit)
    )
)

(about #_"DefExpr"
    (defr DefExpr [])

    (defn #_"DefExpr" DefExpr'new [#_"Var" var, #_"Expr" init, #_"Expr" meta, #_"boolean" initProvided, #_"boolean" shadowsCoreMapping]
        (merge (class! DefExpr)
            (hash-map
                #_"Var" :var var
                #_"Expr" :init init
                #_"Expr" :meta meta
                #_"boolean" :initProvided initProvided
                #_"boolean" :shadowsCoreMapping shadowsCoreMapping
            )
        )
    )

    ;; (def x) or (def x initexpr)
    (defn #_"Expr" DefExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (cond
            (< 3 (count form))            (throw! "too many arguments to def")
            (< (count form) 2)            (throw! "too few arguments to def")
            (not (symbol? (second form))) (throw! "first argument to def must be a Symbol")
        )
        (let [#_"Symbol" sym (second form) #_"Var" v (Compiler'lookupVar sym, true)]
            (when (some? v) => (throw! "can't refer to qualified var that doesn't exist")
                (let [[v #_"boolean" shadowsCoreMapping]
                        (when-not (= (:ns v) *arbace-ns*) => [v false]
                            (when (nil? (:ns sym)) => (throw! "can't create defs outside of current ns")
                                [(Namespace''intern *arbace-ns*, sym) true]
                            )
                        )
                      #_"Expr" init (Compiler'analyze (third form), scope)
                      #_"Expr" _meta (Compiler'analyze (meta sym), scope)]
                    (DefExpr'new v, init, _meta, (= (count form) 3), shadowsCoreMapping)
                )
            )
        )
    )

    (defn- #_"boolean" DefExpr''includesExplicitMetadata [#_"DefExpr" this, #_"MapExpr" expr]
        (loop-when [#_"int" i 0] (< i (count (:keyvals expr))) => false
            (recur-when (= (:k (nth (:keyvals expr) i)) :declared) [(+ i 2)] => true)
        )
    )

    (defn- #_"gen" DefExpr''emit [#_"DefExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen (Gen''push gen, (:var this))
            gen
                (when (ß :shadowsCoreMapping this) => gen
                    (let [
                        gen (Gen''dup gen)
                        gen (ß Gen''get gen, 'ns)
                        gen (Gen''swap gen)
                        gen (Gen''dup gen)
                        gen (ß Gen''get gen, 'sym)
                        gen (Gen''swap gen)
                        gen (Gen''call gen, 'Namespace''refer, 3)
                    ]
                        gen
                    )
                )
            gen
                (when (some? (:meta this)) => gen
                    (let [
                        gen (Gen''dup gen)
                        gen (Expr'''emit (:meta this), :Context'EXPRESSION, scope, gen)
                        gen (Gen''call gen, 'Var''resetMeta, 2)
                    ]
                        gen
                    )
                )
            gen
                (when (:initProvided this) => gen
                    (let [
                        gen (Gen''dup gen)
                        gen (Expr'''emit (:init this), :Context'EXPRESSION, scope, gen)
                        gen (Gen''call gen, 'Var''bindRoot, 2)
                    ]
                        gen
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
    (defr LetFnExpr [])

    (defn #_"LetFnExpr" LetFnExpr'new [#_"[LocalBinding]" bindings, #_"Expr" body]
        (merge (class! LetFnExpr)
            (hash-map
                #_"[LocalBinding]" :bindings bindings
                #_"Expr" :body body
            )
        )
    )

    ;; (letfn* [var (fn [args] body) ...] body...)
    (defn #_"Expr" LetFnExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"vector?" bindings (second form)]
            (when (vector? bindings)           => (throw! "bad binding form, expected vector")
                (when (even? (count bindings)) => (throw! "bad binding form, expected matched symbol expression pairs")
                    (let [
                        scope (update scope :'local-env (comp atom deref))
                        scope (update scope :'local-num (comp atom deref))
                        ;; pre-seed env (like Lisp labels)
                        #_"[LocalBinding]" lbs
                            (loop-when [lbs (vector) #_"seq" s (seq bindings)] (some? s) => lbs
                                (let [#_"symbol?" sym (first s)]
                                    (when (symbol? sym)        => (throw! (str "bad binding form, expected symbol, got: " sym))
                                        (when (nil? (:ns sym)) => (throw! (str "can't let qualified name: " sym))
                                            (let [
                                                #_"LocalBinding" lb (LocalBinding'new sym, nil, (swap! (:'local-num scope) inc))
                                            ]
                                                (swap! (:'local-env scope) assoc (:sym lb) lb)
                                                (swap! (:'locals (:fm scope)) assoc (:uid lb) lb)
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

    (defn- #_"gen" LetFnExpr''emit [#_"LetFnExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
                        gen (Expr'''emit @(:'init lb), :Context'EXPRESSION, scope, gen)
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
                            (loop-when [gen gen #_"seq" s (vals @(:'closes @(:'init lb)))] (some? s) => gen
                                (let [
                                    gen
                                        (let-when [#_"LocalBinding" lb (first s)] (contains? lbset (:uid lb)) => gen
                                            (let [
                                                gen (Gen''dup gen)
                                                gen (FnMethod''emitLocal (:fm scope), gen, lb)
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
    (defr LetExpr [])

    (defn #_"LetExpr" LetExpr'new [#_"[LocalBinding]" bindings, #_"Expr" body, #_"boolean" loop?]
        (merge (class! LetExpr)
            (hash-map
                #_"[LocalBinding]" :bindings bindings
                #_"Expr" :body body
                #_"boolean" :loop? loop?
            )
        )
    )

    ;; (let* [var val var2 val2 ...] body...)
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
                        ;; sequential enhancement of env (like Lisp let*)
                        #_"[LocalBinding]" lbs
                            (loop-when [lbs (vector) #_"seq" s (seq bindings)] (some? s) => lbs
                                (let [#_"symbol?" sym (first s)]
                                    (when (symbol? sym)        => (throw! (str "bad binding form, expected symbol, got: " sym))
                                        (when (nil? (:ns sym)) => (throw! (str "can't let qualified name: " sym))
                                            (let [
                                                #_"Expr" init (Compiler'analyze (second s), scope)
                                                #_"LocalBinding" lb (LocalBinding'new sym, init, (swap! (:'local-num scope) inc))
                                            ]
                                                (swap! (:'local-env scope) assoc (:sym lb) lb)
                                                (swap! (:'locals (:fm scope)) assoc (:uid lb) lb)
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

    (defn- #_"gen" LetExpr''emit [#_"LetExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            gen
                (loop-when [gen gen #_"seq" s (seq (:bindings this))] (some? s) => gen
                    (let [
                        #_"LocalBinding" lb (first s)
                        gen (Expr'''emit @(:'init lb), :Context'EXPRESSION, scope, gen)
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
    (defr RecurExpr [])

    (defn #_"RecurExpr" RecurExpr'new [#_"vector" loopLocals, #_"vector" args]
        (merge (class! RecurExpr)
            (hash-map
                #_"vector" :loopLocals loopLocals
                #_"vector" :args args
            )
        )
    )

    (defn #_"Expr" RecurExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (when (and (= context :Context'RETURN) (some? (:loop-locals scope))) => (throw! "can only recur from tail position")
            (let [#_"vector" args (mapv #(Compiler'analyze %, scope) (next form)) #_"int" n (count args) #_"int" m (count (:loop-locals scope))]
                (when (= n m) => (throw! (str "mismatched argument count to recur, expected: " m " args, got: " n))
                    (RecurExpr'new (:loop-locals scope), args)
                )
            )
        )
    )

    (defn- #_"gen" RecurExpr''emit [#_"RecurExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (when-some [#_"label" l'loop (:loop-label scope)] => (throw! "recur misses loop label")
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

(about #_"CaseExpr"
    (defr CaseExpr [])

    ;; (case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
    (defn #_"CaseExpr" CaseExpr'new [#_"LocalBindingExpr" expr, #_"int" shift, #_"int" mask, #_"int" low, #_"int" high, #_"Expr" defaultExpr, #_"sorted {Integer Expr}" tests, #_"{Integer Expr}" thens, #_"Keyword" switchType, #_"Keyword" testType, #_"{Integer}" skipCheck]
        (when-not (any = switchType :compact :sparse)
            (throw! (str "unexpected switch type: " switchType))
        )
        (when-not (any = testType :int :hash-equiv :hash-identity)
            (throw! (str "unexpected test type: " testType))
        )
        (merge (class! CaseExpr)
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
            )
        )
    )

    ;; (case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
    ;; prepared by case macro and presumed correct
    ;; case macro binds actual expr in let so expr is always a local,
    ;; no need to worry about multiple evaluation
    (defn #_"Expr" CaseExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (let [#_"vector" args (vec (next form))
              #_"Object" exprForm (nth args 0)
              #_"int" shift (int! (nth args 1))
              #_"int" mask (int! (nth args 2))
              #_"Object" defaultForm (nth args 3)
              #_"map" caseMap (nth args 4)
              #_"Keyword" switchType (nth args 5)
              #_"Keyword" testType (nth args 6)
              #_"IPersistentSet" skipCheck (when (< 7 (count args)) (nth args 7))
              #_"seq" keys (keys caseMap)
              #_"int" low (int! (first keys))
              #_"int" high (int! (nth keys (dec (count keys))))
              #_"LocalBindingExpr" testExpr (Compiler'analyze exprForm, scope)
              [#_"sorted {Integer Expr}" tests #_"{Integer Expr}" thens]
                (loop-when [tests (sorted-map) thens (hash-map) #_"seq" s (seq caseMap)] (some? s) => [tests thens]
                    (let [#_"pair" e (first s)
                          #_"Integer" minhash (int! (key e)) #_"Object" pair (val e) ;; [test-val then-expr]
                          #_"Expr" test (LiteralExpr'new (first pair))
                          #_"Expr" then (Compiler'analyze (second pair), context, scope)]
                        (recur (assoc tests minhash test) (assoc thens minhash then) (next s))
                    )
                )
              #_"Expr" defaultExpr (Compiler'analyze (nth args 3), context, scope)]
            (CaseExpr'new testExpr, shift, mask, low, high, defaultExpr, tests, thens, switchType, testType, skipCheck)
        )
    )

    (defn- #_"gen" CaseExpr''emitShiftMask [#_"CaseExpr" this, #_"gen" gen]
        (when-not (zero? (:mask this)) => gen
            (let [
                gen (Gen''push gen, (:shift this))
                gen (Gen''shr gen)
                gen (Gen''push gen, (:mask this))
                gen (Gen''and gen)
            ]
                gen
            )
        )
    )

    (defn- #_"gen" CaseExpr''emitExprForInts [#_"CaseExpr" this, #_"map" scope, #_"gen" gen, #_"label" l'default]
        (let [
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Gen''number? gen)
            gen (Gen''if-not gen, l'default)
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Gen''call gen, 'int!, 1)
        ]
            (CaseExpr''emitShiftMask this, gen)
        )
    )

    (defn- #_"gen" CaseExpr'emitExpr [#_"map" scope, #_"gen" gen, #_"Expr" expr]
        (Expr'''emit expr, :Context'EXPRESSION, scope, gen)
    )

    (defn- #_"gen" CaseExpr''emitThenForInts [#_"CaseExpr" this, #_"map" scope, #_"gen" gen, #_"Expr" test, #_"Expr" then, #_"label" l'default]
        (let [
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Expr'''emit test, :Context'EXPRESSION, scope, gen)
            gen (Gen''call gen, 'Util'equiv, 2)
            gen (Gen''if-not gen, l'default)
        ]
            (CaseExpr'emitExpr scope, gen, then)
        )
    )

    (defn- #_"gen" CaseExpr''emitExprForHashes [#_"CaseExpr" this, #_"map" scope, #_"gen" gen]
        (let [
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Gen''call gen, 'f'hashcode, 1)
        ]
            (CaseExpr''emitShiftMask this, gen)
        )
    )

    (defn- #_"gen" CaseExpr''emitThenForHashes [#_"CaseExpr" this, #_"map" scope, #_"gen" gen, #_"Expr" test, #_"Expr" then, #_"label" l'default]
        (let [
            gen (Expr'''emit (:expr this), :Context'EXPRESSION, scope, gen)
            gen (Expr'''emit test, :Context'EXPRESSION, scope, gen)
            gen
                (if (= (:testType this) :hash-identity)
                    (Gen''if-ne? gen, l'default)
                    (let [
                        gen (Gen''call gen, 'Util'equiv, 2)
                        gen (Gen''if-not gen, l'default)
                    ]
                        gen
                    )
                )
        ]
            (CaseExpr'emitExpr scope, gen, then)
        )
    )

    (defn- #_"gen" CaseExpr''emit [#_"CaseExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
        (let [
            #_"label" l'default (Gen''label gen)
            gen
                (if (= (:testType this) :int)
                    (CaseExpr''emitExprForInts this, scope, gen, l'default)
                    (CaseExpr''emitExprForHashes this, scope, gen)
                )
            #_"sorted {Integer Label}" labels (reduce! #(assoc! %1 %2 (Gen''label gen)) (sorted-map) (keys (:tests this)))
            gen
                (if (= (:switchType this) :sparse)
                    (Gen''lookup-switch gen, (keys (:tests this)), (vals labels), l'default)
                    (let [
                        #_"labels" ls
                            (for [#_"int" i (range (:low this) (inc (:high this)))]
                                (if (contains? labels i) (get labels i) l'default)
                            )
                    ]
                        (Gen''table-switch gen, (:low this), (:high this), ls, l'default)
                    )
                )
            #_"label" l'end (Gen''label gen)
            gen
                (loop-when [gen gen #_"seq" s (keys labels)] (some? s) => gen
                    (let [
                        #_"Integer" i (first s)
                        gen (Gen''mark gen, (get labels i))
                        gen
                            (cond
                                (= (:testType this) :int)
                                    (CaseExpr''emitThenForInts this, scope, gen, (get (:tests this) i), (get (:thens this) i), l'default)
                                (contains? (:skipCheck this) i)
                                    (CaseExpr'emitExpr scope, gen, (get (:thens this) i))
                                :else
                                    (CaseExpr''emitThenForHashes this, scope, gen, (get (:tests this) i), (get (:thens this) i), l'default)
                            )
                        gen (Gen''goto gen, l'end)
                    ]
                        (recur gen (next s))
                    )
                )
            gen (Gen''mark gen, l'default)
            gen (CaseExpr'emitExpr scope, gen, (:defaultExpr this))
            gen (Gen''mark gen, l'end)
        ]
            (when (= context :Context'STATEMENT) => gen
                (Gen''pop gen)
            )
        )
    )

    (defm CaseExpr Expr
        (Expr'''emit => CaseExpr''emit)
    )
)

(about #_"MonitorExpr"
    (defr MonitorExpr [])

    (defn #_"MonitorExpr" MonitorExpr'new [#_"Expr" target, #_"boolean" enter?]
        (merge (class! MonitorExpr)
            (hash-map
                #_"Expr" :target target
                #_"boolean" :enter? enter?
            )
        )
    )

    (defn #_"Expr" MonitorExpr'parse [#_"seq" form, #_"Context" context, #_"map" scope]
        (MonitorExpr'new (Compiler'analyze (second form), scope), (= (first form) 'monitor-enter))
    )

    (defn- #_"gen" MonitorExpr''emit [#_"MonitorExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
    (defr CatchClause [])

    (defn #_"CatchClause" CatchClause'new [#_"LocalBinding" lb, #_"Expr" handler]
        (merge (class! CatchClause)
            (hash-map
                #_"LocalBinding" :lb lb
                #_"Expr" :handler handler
            )
        )
    )
)

(about #_"TryExpr"
    (defr TryExpr [])

    (defn #_"TryExpr" TryExpr'new [#_"Expr" tryExpr, #_"[CatchClause]" catches, #_"Expr" finallyExpr, #_"map" scope]
        (merge (class! TryExpr)
            (hash-map
                #_"Expr" :tryExpr tryExpr
                #_"[CatchClause]" :catches catches
                #_"Expr" :finallyExpr finallyExpr

                #_"int" :retLocal (swap! (:'local-num scope) inc)
                #_"int" :finallyLocal (swap! (:'local-num scope) inc)
            )
        )
    )

    ;; (try try-expr* catch-expr* finally-expr?)
    ;; catch-expr: (catch class sym expr*)
    ;; finally-expr: (finally expr*)
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
                                                    #_"LocalBinding" lb (LocalBinding'new sym, nil, (swap! (:'local-num scope) inc))
                                                    _ (swap! (:'local-env scope) assoc (:sym lb) lb)
                                                    _ (swap! (:'locals (:fm scope)) assoc (:uid lb) lb)
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
                ;; when there is neither catch nor finally, return a body expr directly
                (BodyExpr'parse (seq body), context, scope)
            )
        )
    )

    (defn- #_"gen" TryExpr''emit [#_"TryExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
                        ;; exception should be on stack
                        ;; put in clause local
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
                        ;; exception should be on stack
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
    (defr ThrowExpr [])

    (defn #_"ThrowExpr" ThrowExpr'new [#_"Expr" throwable]
        (merge (class! ThrowExpr)
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

    (defn- #_"gen" ThrowExpr''emit [#_"ThrowExpr" this, #_"Context" context, #_"map" scope, #_"gen" gen]
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
        (let [
            #_"map" m
                (hash-map
                    '&             nil
                    'case*         CaseExpr'parse
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
                    'set!          AssignExpr'parse
                    'throw         ThrowExpr'parse
                    'try           TryExpr'parse
                    'var           TheVarExpr'parse
                )
        ]
            (into (#_empty identity m) (map (fn [[s f]] [(symbol! s) f]) m))
        )
    )

    (defn #_"boolean" Compiler'isSpecial [#_"Object" sym]
        (contains? Compiler'specials sym)
    )

;;;
 ; Returns true if s names a special form.
 ;;
(defn special-symbol? [s] (Compiler'isSpecial s))

    (defn #_"edn" Compiler'macroexpand1
        ([#_"edn" form] (Compiler'macroexpand1 form, nil))
        ([#_"edn" form, #_"map" scope]
            (when (seq? form) => form
                (let-when [#_"Object" op (first form)] (not (Compiler'isSpecial op)) => form
                    (let-when [#_"Var" v (Compiler'maybeMacro op, scope)] (some? v) => form
                        (apply v form @(:'local-env scope) (next form)) ;; macro expansion
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

    (defn- #_"void" Compiler'closeOver [#_"LocalBinding" lb, #_"FnMethod" fm]
        (when (and (some? lb) (some? fm) (not (contains? @(:'locals fm) (:uid lb))))
            (swap! (:'closes (:fun fm)) assoc (:uid lb) lb)
            (Compiler'closeOver lb, (:parent fm))
        )
        nil
    )

    (defn- #_"Expr" Compiler'analyzeSymbol [#_"Symbol" sym, #_"map" scope]
        (or
            (when (nil? (:ns sym)) ;; ns-qualified syms are always Vars
                (when-some [#_"LocalBinding" lb (get @(:'local-env scope) sym)]
                    (Compiler'closeOver lb, (:fm scope))
                    (LocalBindingExpr'new lb)
                )
            )
            (let [#_"Object" o (Compiler'resolve sym)]
                (cond
                    (var? o)
                        (when (nil? (Compiler'maybeMacro o, scope)) => (throw! (str "can't take value of a macro: " o))
                            (VarExpr'new o)
                        )
                    (class? o)
                        (LiteralExpr'new o)
                    (symbol? o)
                        (UnresolvedVarExpr'new o)
                    :else
                        (throw! (str "unable to resolve symbol: " sym " in this context"))
                )
            )
        )
    )

    (defn- #_"Expr" Compiler'analyzeSeq [#_"seq" form, #_"Context" context, #_"map" scope]
        (let-when [#_"Object" me (Compiler'macroexpand1 form, scope)] (= me form) => (Compiler'analyze me, context, scope)
            (when-some [#_"Object" op (first form)] => (throw! (str "can't call nil, form: " form))
                (let [#_"IFn" inline (Compiler'maybeInline op, (count (next form)), scope)]
                    (if (some? inline)
                        (Compiler'analyze (IFn'''applyTo inline, (next form)), context, scope)
                        (let [#_"fn" f'parse (get Compiler'specials op InvokeExpr'parse)]
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
                (case form
                    nil                                  LiteralExpr'NIL
                    true                                 LiteralExpr'TRUE
                    false                                LiteralExpr'FALSE
                    (cond
                        (symbol? form)                   (Compiler'analyzeSymbol form, scope)
                        (string? form)                   (LiteralExpr'new (.intern #_"String" form))
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
                (-> (list (symbol! 'fn*) [] form)
                    (Compiler'analyze scope)
                    (Closure'new nil)
                    (IFn'''invoke)
                )
            )
        )
    )
)
)

(about #_"arbace.LispReader"

(about #_"LispReader"
    (def #_"Var" ^:dynamic *arg-env*   ) ;; sorted-map num->gensymbol
    (def #_"Var" ^:dynamic *gensym-env*) ;; symbol->gensymbol

    (defn #_"Symbol" LispReader'garg [#_"int" n]
        (symbol (str (if (= n -1) "rest" (str "p" n)) "__" (next-id!) "#"))
    )

    (defn #_"Symbol" LispReader'registerArg [#_"int" n]
        (when (bound? (var! *arg-env*)) => (throw! "arg literal not in #()")
            (or (get (var-get! *arg-env*) n)
                (let [#_"Symbol" sym (LispReader'garg n)]
                    (var-swap! *arg-env* assoc n sym)
                    sym
                )
            )
        )
    )

    (defn #_"Symbol" LispReader'registerGensym [#_"Symbol" sym]
        (when (bound? (var! *gensym-env*)) => (throw! "gensym literal not in syntax-quote")
            (or (get (var-get! *gensym-env*) sym)
                (let [#_"Symbol" gsym (symbol (str (:name sym) "__" (next-id!) "__auto__"))]
                    (var-swap! *gensym-env* assoc sym gsym)
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
        (loop-when-recur [#_"char" ch (LispReader'read1 r)] (and (some? ch) (LispReader'isWhitespace ch)) [(LispReader'read1 r)] => (LispReader'unread r, ch))
        nil
    )

    (def- #_"Pattern" LispReader'rxInteger #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)")
    (def- #_"Pattern" LispReader'rxRatio   #"([-+]?[0-9]+)/([0-9]+)")

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
                              #_"Namespace" kns (if (some? (:ns ks)) (Namespace''getAlias *arbace-ns*, (symbol (:ns ks))) *arbace-ns*)]
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
                (let [#_"char" ch (loop-when-recur [ch (LispReader'read1 r)] (and (some? ch) (LispReader'isWhitespace ch)) [(LispReader'read1 r)] => ch)]
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
        (loop [#_"vector" v (vector)]
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
                (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading regex")
                    (when-not (= ch \") ;; oops! "
                        (.append sb, ch)
                        (when (= ch \\) ;; escape
                            (when-some [ch (LispReader'read1 r)] => (throw! "EOF while reading regex")
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
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading string")
            (case ch
                \t  \tab
                \r  \return
                \n  \newline
                \\  ch
                \"  ch ;; oops! "
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

    (defn #_"Object" string-reader [#_"PushbackReader" r, #_"char" _delim]
        (let [#_"StringBuilder" sb (StringBuilder.)]
            (loop []
                (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading string")
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
        (list (symbol! 'quote) (LispReader'read r))
    )
)

(about #_"DerefReader"
    (defn #_"Object" deref-reader [#_"PushbackReader" r, #_"char" _delim]
        (list (symbol! `deref) (LispReader'read r))
    )
)

(about #_"VarReader"
    (defn #_"Object" var-reader [#_"PushbackReader" r, #_"char" _delim]
        (list (symbol! 'var) (LispReader'read r))
    )
)

(about #_"DispatchReader"
    (declare LispReader'dispatchMacros)

    (defn #_"Object" dispatch-reader [#_"PushbackReader" r, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (let-when [#_"IFn" fn (get LispReader'dispatchMacros ch)] (nil? fn) => (fn r ch)
                (LispReader'unread r, ch)
                (throw! (str "no dispatch macro for: " ch))
            )
        )
    )
)

(about #_"FnReader"
    (defn #_"Object" fn-reader [#_"PushbackReader" r, #_"char" _delim]
        (when-not (bound? (var! *arg-env*)) => (throw! "nested #()s are not allowed")
            (binding [*arg-env* (sorted-map)]
                (LispReader'unread r, \()
                (let [#_"vector" args (vector)
                      args
                        (when-some [#_"seq" rs (rseq (var-get! *arg-env*))] => args
                            (let [args
                                    (let-when [#_"int" n (key (first rs))] (pos? n) => args
                                        (loop-when-recur [args args #_"int" i 1]
                                                         (<= i n)
                                                         [(conj args (or (get (var-get! *arg-env*) i) (LispReader'garg i))) (inc i)]
                                                      => args
                                        )
                                    )]
                                (when-some [#_"Object" rest (get (var-get! *arg-env*) -1)] => args
                                    (conj args (symbol! '&) rest)
                                )
                            )
                        )]
                    (list (symbol! 'fn*) args (LispReader'read r))
                )
            )
        )
    )
)

(about #_"ArgReader"
    (defn #_"Object" arg-reader [#_"PushbackReader" r, #_"char" _delim]
        (when (bound? (var! *arg-env*)) => (LispReader'interpretToken (LispReader'readToken r, \%))
            (let [#_"char" ch (LispReader'read1 r) _ (LispReader'unread r, ch)]
                ;; % alone is first arg
                (if (or (nil? ch) (LispReader'isWhitespace ch) (LispReader'isTerminatingMacro ch))
                    (LispReader'registerArg 1)
                    (let [#_"Object" n (LispReader'read r)]
                        (cond
                            (= n '&)    (LispReader'registerArg -1)
                            (number? n) (LispReader'registerArg (int! n))
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
                    (keyword? _meta) {_meta true}
                    (map? _meta)      _meta
                    :else (throw! "metadata must be Keyword or Map")
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

    (declare SyntaxQuoteReader'syntaxQuote)

    (defn- #_"seq" SyntaxQuoteReader'sqExpandList [#_"seq" s]
        (loop-when [#_"vector" v (vector) s s] (some? s) => (seq v)
            (let [#_"Object" item (first s)
                  v (cond
                        (SyntaxQuoteReader'isUnquote item)         (conj v (list (symbol! `list) (second item)))
                        (SyntaxQuoteReader'isUnquoteSplicing item) (conj v (second item))
                        :else                                      (conj v (list (symbol! `list) (SyntaxQuoteReader'syntaxQuote item)))
                    )]
                (recur v (next s))
            )
        )
    )

    (defn #_"Object" SyntaxQuoteReader'syntaxQuote [#_"Object" form]
        (let [#_"Object" q
                (cond
                    (Compiler'isSpecial form)
                        (list (symbol! 'quote) form)
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
                                        (let-when [#_"Object" c (when (some? ns) (Namespace''getMapping *arbace-ns*, (symbol ns)))] (class? c) => (Compiler'resolveSymbol form)
                                            ;; Classname/foo -> package.qualified.Classname/foo
                                            (symbol (.getName c) n)
                                        )
                                )]
                            (list (symbol! 'quote) form)
                        )
                    (SyntaxQuoteReader'isUnquote form)
                        (second form)
                    (SyntaxQuoteReader'isUnquoteSplicing form)
                        (throw! "splice not in list")
                    (coll? form)
                        (cond
                            (map? form)
                                (list (symbol! `apply) (symbol! `hash-map) (list (symbol! `seq) (cons (symbol! `concat) (SyntaxQuoteReader'sqExpandList (seq (mapcat identity form))))))
                            (vector? form)
                                (list (symbol! `apply) (symbol! `vector) (list (symbol! `seq) (cons (symbol! `concat) (SyntaxQuoteReader'sqExpandList (seq form)))))
                            (set? form)
                                (list (symbol! `apply) (symbol! `hash-set) (list (symbol! `seq) (cons (symbol! `concat) (SyntaxQuoteReader'sqExpandList (seq form)))))
                            (or (seq? form) (list? form))
                                (when-some [#_"seq" s (seq form)] => (cons (symbol! `list) nil)
                                    (list (symbol! `seq) (cons (symbol! `concat) (SyntaxQuoteReader'sqExpandList s)))
                                )
                            :else
                                (throw! "unknown collection type")
                        )
                    (or (keyword? form) (number? form) (char? form) (string? form))
                        form
                    :else
                        (list (symbol! 'quote) form)
                )]
            (when (and (satisfies? IObj form) (seq (meta form)) (not (SyntaxQuoteReader'isUnquote form))) => q
                (list (symbol! `with-meta) q (SyntaxQuoteReader'syntaxQuote (meta form)))
            )
        )
    )

    (defn #_"Object" syntax-quote-reader [#_"PushbackReader" r, #_"char" _delim]
        (binding [*gensym-env* (hash-map)]
            (SyntaxQuoteReader'syntaxQuote (LispReader'read r))
        )
    )
)

(about #_"UnquoteReader"
    (defn #_"Object" unquote-reader [#_"PushbackReader" r, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
            (if (= ch \@)
                (list (symbol! `unquote-splicing) (LispReader'read r))
                (do
                    (LispReader'unread r, ch)
                    (list (symbol! `unquote) (LispReader'read r))
                )
            )
        )
    )
)

(about #_"CharacterReader"
    (defn #_"Object" character-reader [#_"PushbackReader" r, #_"char" _delim]
        (when-some [#_"char" ch (LispReader'read1 r)] => (throw! "EOF while reading character")
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
        (apply list (LispReader'readDelimitedForms r, \)))
    )
)

(about #_"VectorReader"
    (defn #_"Object" vector-reader [#_"PushbackReader" r, #_"char" _delim]
        (vec (LispReader'readDelimitedForms r, \]))
    )
)

(about #_"MapReader"
    (defn #_"Object" map-reader [#_"PushbackReader" r, #_"char" _delim]
        (let [#_"vector" v (LispReader'readDelimitedForms r, \})]
            (when (even? (count v)) => (throw! "map literal must contain an even number of forms")
                (RT'map v)
            )
        )
    )
)

(about #_"SetReader"
    (defn #_"Object" set-reader [#_"PushbackReader" r, #_"char" _delim]
        (PersistentHashSet'createWithCheck (LispReader'readDelimitedForms r, \}))
    )
)

(about #_"UnmatchedDelimiterReader"
    (defn #_"Object" unmatched-delimiter-reader [#_"PushbackReader" _r, #_"char" delim]
        (throw! (str "unmatched delimiter: " delim))
    )
)

(about #_"LispReader"
    (def #_"{char fn}" LispReader'macros
        (hash-map
            \"  string-reader ;; oops! "
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
            \"  regex-reader ;; oops! "
            \(  fn-reader
            \{  set-reader
            \!  comment-reader
            \_  discard-reader
        )
    )
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
    ([] (read -/*in*))
    ([s] (read s true nil))
    ([s eof-error? eof-value] (LispReader'read s (boolean eof-error?) eof-value))
)

;;;
 ; Reads one object from the string s.
 ;;
(defn read-string [s]
    (let [#_"PushbackReader" r (PushbackReader. (java.io.StringReader. s))]
        (LispReader'read r)
    )
)

(about #_"arbace.Compiler"

(about #_"Compiler"
    (defn #_"Object" Compiler'load [#_"Reader" reader]
        (let [
            #_"PushbackReader" r (if (instance? PushbackReader reader) reader (PushbackReader. reader)) #_"Object" EOF (Object.)
            #_"map" scope (-/hash-map :'local-env (atom (hash-map)))
        ]
            (loop [#_"value" value nil]
                (LispReader'consumeWhitespaces r)
                (let-when [#_"edn" form (LispReader'read r, false, EOF)] (identical? form EOF) => (recur (Compiler'eval form, scope))
                    value
                )
            )
        )
    )
)
)

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
 ; Recursively performs all possible macroexpansions in form.
 ;;
(defn macroexpand-all [form] (prewalk #(if (seq? %) (macroexpand %) %) form))

;;;
 ; Evaluates the form data structure (not text!) and returns the result.
 ;;
(defn eval [form] (Compiler'eval form))

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
 ; Sequentially read and evaluate the set of forms contained in the stream.
 ;;
(defn load-reader [r] (Compiler'load r))

;;;
 ; Sequentially read and evaluate the set of forms contained in the string.
 ;;
(defn load-string [s] (load-reader (-> s (java.io.StringReader.) (PushbackReader.))))

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
    ([    sym] (ns-resolve *arbace-ns*     sym))
    ([env sym] (ns-resolve *arbace-ns* env sym))
)

(about #_"Arbace"

(about #_"*arbace-ns*"
    (def #_"Var" ^:dynamic *arbace-ns* (create-ns (symbol "arbace.core")))

    (doseq [[#_"symbol" s #_"class|var" v] (-/ns-map *ns*)]
        (intern *arbace-ns*, (with-meta (symbol! s) (when (var? v) (select-keys (meta v) [:dynamic :macro :private]))), (if (var? v) @v v))
    )

    (alias (symbol "-"), *ns*)
)

(defn repl []
    (let [#_"map" scope (-/hash-map :'local-env (atom (hash-map)))]
        (loop []
            (-/print "\033[31mArbace \033[32m=> \033[0m")
            (.flush -/*out*)
            (-> (read) (Compiler'eval scope) (-/prn))
            (recur)
        )
    )
)
)

(defn -main [& args])
