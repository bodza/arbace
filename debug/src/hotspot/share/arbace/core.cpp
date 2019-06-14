#include <iostream>

#define nil nullptr

namespace arbace {
} using namespace arbace;

namespace java_lang {
    class Object {
///     (defn #_"int" Object_2_hashCode [#_"Object" this] (.hashCode this))
///     (defn #_"String" Object_2_toString [#_"Object" this] (.toString this))
    };

    class Number : Object {
///     (defn #_"long" Number_2_longValue [#_"Number" this] (.longValue this))
///     (defn #_"String" Number_2_toString [#_"Number" this] (.toString this))
    };

    class Integer : Number/*, Comparable<Integer>*/ {
        public: static const int MAX_VALUE = 0x7fffffff;
        public: static const int MIN_VALUE = 0x80000000;

///     (defn #_"int" Integer_1_bitCount [#_"int" i] (Integer/bitCount i))
///     (defn #_"int" Integer_1_parseInt [#_"String" s] (Integer/parseInt s))
///     (defn #_"int" Integer_1_rotateLeft [#_"int" x, #_"int" y] (Integer/rotateLeft x, y))
///     (defn #_"String" Integer_1_toString [#_"int" i, #_"int" radix] (Integer/toString i, radix))
    };

    class Long : Number/*, Comparable<Long>*/ {
        public: static const long MAX_VALUE = 0x7fffffffffffffff;
        public: static const long MIN_VALUE = 0x8000000000000000;

///     (defn #_"Long" Long_1_valueOf [#_"long" l] (Long/valueOf l))
    };

    class Character : Object/*, Comparable<Character>*/ {
///     (defn #_"int" Character_1_digit [#_"char" ch, #_"int" radix] (Character/digit ch, radix))
///     (defn #_"bool" Character_1_isWhitespace [#_"char" ch] (Character/isWhitespace ch))
///     (defn #_"Character" Character_1_valueOf [#_"char" ch] (Character/valueOf ch))
    };

    class CharSequence {
        public: virtual char CharSequence_3_charAt(int i) = 0;
        public: virtual int CharSequence_3_length() = 0;
    };

    class Appendable {
        public: virtual Appendable* Appendable_3_append(char x) = 0;
        public: virtual Appendable* Appendable_3_append(const CharSequence* x) = 0;
    };

    class String : Object, CharSequence/*, Comparable<String>*/ {
///     (defn #_"char" String_2_charAt [#_"String" this, #_"int" i] (.charAt this, i))
///     (defn #_"bool" String_2_endsWith [#_"String" this, #_"String" s] (.endsWith this, s))
///     (defn #_"int" String_2_indexOf ([#_"String" this, #_"int" ch] (.indexOf this, ch)) ([#_"String" this, #_"String" s, #_"int" from] (.indexOf this, s, from)))
///     (defn #_"String" String_2_intern [#_"String" this] (.intern this))
///     (defn #_"int" String_2_length [#_"String" this] (.length this))
///     (defn #_"bool" String_2_startsWith [#_"String" this, #_"String" s] (.startsWith this, s))
///     (defn #_"String" String_2_substring ([#_"String" this, #_"int" from] (.substring this, from)) ([#_"String" this, #_"int" from, #_"int" over] (.substring this, from, over)))
    };

    class StringBuilder : Object, Appendable, CharSequence/*, Comparable<StringBuilder>*/ {
///     (defn #_"StringBuilder" StringBuilder_1_new [] (StringBuilder.))

///     (defn #_"StringBuilder" StringBuilder_2_append [#_"StringBuilder" this, #_"char" ch] (.append this, ch))
///     (defn #_"String" StringBuilder_2_toString [#_"StringBuilder" this] (.toString this))
    };

    class Thread/* : Object, Runnable*/ {
///     (defn thread [] (Thread/currentThread))
    };
} using namespace java_lang;

namespace java_lang_ref {
///     #_abstract
    class Reference/*<T>*/ : Object {
///     (defn #_"Object" Reference_2_get [#_"Reference" this] (.get this))
    };

    class ReferenceQueue/*<T>*/ : Object {
///     (defn #_"ReferenceQueue" ReferenceQueue_1_new [] (ReferenceQueue.))

///     (defn #_"Reference" ReferenceQueue_2_poll [#_"ReferenceQueue" this] (.poll this))
    };

    class WeakReference/*<T>*/ : Reference/*<T>*/ {
///     (defn #_"WeakReference" WeakReference_1_new [#_"Object" x, #_"ReferenceQueue" q] (WeakReference. x, q))
    };
} using namespace java_lang_ref;

namespace java_lang_reflect {
    class Array/* : Object*/ {
///     (defn array_9_ [x] (.isArray (-/class x)))
    };
} using namespace java_lang_reflect;

namespace java_util_regex {
    class Matcher : Object/*, MatchResult*/ {
///     (defn #_"bool" Matcher_2_find [#_"Matcher" this] (.find this))
///     (defn #_"String" Matcher_2_group ([#_"Matcher" this] (.group this)) ([#_"Matcher" this, #_"int" n] (.group this, n)))
///     (defn #_"int" Matcher_2_groupCount [#_"Matcher" this] (.groupCount this))
///     (defn #_"bool" Matcher_2_matches [#_"Matcher" this] (.matches this))
    };

    class Pattern : Object {
///     (defn #_"Pattern" Pattern_1_compile [#_"String" s] (Pattern/compile s))
///     (defn #_"Matcher" Pattern_2_matcher [#_"Pattern" this, #_"CharSequence" s] (.matcher this, s))
///     (defn #_"String" Pattern_2_pattern [#_"Pattern" this] (.pattern this))
    };
} using namespace java_util_regex;

namespace arbace_math {
    class BigInteger : Number/*, Comparable<BigInteger>*/ {
///     (defn #_"BigInteger" BigInteger_1_new ([#_"String" s] (BigInteger. s)) ([#_"String" s, #_"int" radix] (BigInteger. s, radix)))

///     (def #_"BigInteger" BigInteger_1_ZERO BigInteger/ZERO)
///     (def #_"BigInteger" BigInteger_1_ONE BigInteger/ONE)

///     (defn #_"BigInteger" BigInteger_2_add [#_"BigInteger" this, #_"BigInteger" x] (.add this, x))
///     (defn #_"int" BigInteger_2_bitLength [#_"BigInteger" this] (.bitLength this))
///     (defn #_"BigInteger" BigInteger_2_divide [#_"BigInteger" this, #_"BigInteger" x] (.divide this, x))
///     (defn #_"BigInteger" BigInteger_2_gcd [#_"BigInteger" this, #_"BigInteger" x] (.gcd this, x))
///     (defn #_"int" BigInteger_2_intValue [#_"BigInteger" this] (.intValue this))
///     (defn #_"long" BigInteger_2_longValue [#_"BigInteger" this] (.longValue this))
///     (defn #_"BigInteger" BigInteger_2_multiply [#_"BigInteger" this, #_"BigInteger" x] (.multiply this, x))
///     (defn #_"BigInteger" BigInteger_2_negate [#_"BigInteger" this] (.negate this))
///     (defn #_"BigInteger" BigInteger_2_remainder [#_"BigInteger" this, #_"BigInteger" x] (.remainder this, x))
///     (defn #_"int" BigInteger_2_signum [#_"BigInteger" this] (.signum this))
///     (defn #_"BigInteger" BigInteger_2_subtract [#_"BigInteger" this, #_"BigInteger" x] (.subtract this, x))
///     (defn #_"String" BigInteger_2_toString [#_"BigInteger" this] (.toString this))
///     (defn #_"BigInteger" BigInteger_1_valueOf [#_"long" x] (BigInteger/valueOf x))
    };
} using namespace arbace_math;

namespace arbace_util_concurrent_atomic {
    class AtomicReference/*<V>*/ : Object {
///     (defn #_"AtomicReference" AtomicReference_1_new [#_"Object" init] (AtomicReference. init))

///     (defn #_"bool" AtomicReference_2_compareAndSet [#_"AtomicReference" this, #_"Object" x, #_"Object" y] (.compareAndSet this, x, y))
///     (defn #_"Object" AtomicReference_2_get [#_"AtomicReference" this] (.get this))
///     (defn #_"void" AtomicReference_2_set [#_"AtomicReference" this, #_"Object" x] (.set this, x))
    };
} using namespace arbace_util_concurrent_atomic;

namespace cloiure {
    template<typename Base, typename T>
    inline bool instance_9_(const T* x) {
        return dynamic_cast<const Base*>(x) != nil;
    }

///    inline bool char_9_(const Object* x) { return instance_9_<Character>(x); }
///    inline bool char_sequence_9_(const Object* x) { return instance_9_<CharSequence>(x); }
///    inline bool int_9_(const Object* x) { return instance_9_<Integer>(x); }
///    inline bool long_9_(const Object* x) { return instance_9_<Long>(x); }
///    inline bool number_9_(const Object* x) { return instance_9_<Number>(x); }
///    inline bool string_9_(const Object* x) { return instance_9_<String>(x); }
///    inline bool matcher_9_(const Object* x) { return instance_9_<Matcher>(x); }
///    inline bool pattern_9_(const Object* x) { return instance_9_<Pattern>(x); }
///    inline bool biginteger_9_(const Object* x) { return instance_9_<BigInteger>(x); }
} using namespace cloiure;

/// (refer! - [< <= > >= defn first int let neg_9_ pos_9_ reduce zero_9_])

// (about #_"Numbers"
///     (defn int_4_ [#_"Number" n] (.intValue n))

///     (defn +
///         ([] (int 0))
///         ([x] (int_4_ x))
///         ([x y] (-/unchecked_add_int (int_4_ x) (int_4_ y)))
///         ([x y & s] (reduce + (+ x y) s))
///     )

///     (defn -
///         ([x] (-/unchecked_negate_int (int_4_ x)))
///         ([x y] (-/unchecked_subtract_int (int_4_ x) (int_4_ y)))
///         ([x y & s] (reduce - (- x y) s))
///     )

///     (def inc  -/unchecked_inc_int)
///     (def dec  -/unchecked_dec_int)
///     (def *    -/unchecked_multiply_int)
///     (def quot -/unchecked_divide_int)
///     (def rem  -/unchecked_remainder_int)

///     (defn bit_and [x y] (int_4_ (-/bit_and x y)))
///     (defn bit_or  [x y] (int_4_ (-/bit_or x y)))
///     (defn bit_xor [x y] (int_4_ (-/bit_xor x y)))

///     (defn <<  [x y] (int_4_ (-/bit_shift_left x y)))
///     (defn >>  [x y] (int_4_ (-/bit_shift_right x y)))
///     (defn >>> [x y] (int_4_ (-/unsigned_bit_shift_right (-/bit_and x 0xffffffff) y)))
// )

/// (defn identical_9_ [a b] (-/identical_9_ a b))

/// (defn -'=       [a b] (-/= a b))
/// (defn -'==      [a b] (-/== a b))
/// (defn -'<       [a b] (-/< a b))
/// (defn -'<=      [a b] (-/<= a b))
/// (defn -'>       [a b] (-/> a b))
/// (defn -'compare [a b] (-/compare a b))
/// (defn -'+       [a b] (-/+ a b))
/// (defn -'-       [a b] (-/- a b))
/// (defn -'*       [a b] (-/* a b))
/// (defn -'quot    [a b] (-/quot a b))
/// (defn -'rem     [a b] (-/rem a b))
/// (defn -'bit_not [a]   (-/bit_not a))
/// (defn -'bit_and [a b] (-/bit_and a b))
/// (defn -'bit_or  [a b] (-/bit_or a b))
/// (defn -'bit_xor [a b] (-/bit_xor a b))
/// (defn -'bit_shift_left [a b] (-/bit_shift_left a b))
/// (defn -'bit_shift_right [a b] (-/bit_shift_right a b))
/// (defn -'unsigned_bit_shift_right [a b] (-/unsigned_bit_shift_right a b))

/// (defn A_1_new [n] (-/object_array n))

/// (defn A_1_clone  [#_"[Ljava.lang.Object;" a]     (-/aclone a))
/// (defn A_1_get    [#_"[Ljava.lang.Object;" a i]   (-/aget a i))
/// (defn A_1_length [#_"[Ljava.lang.Object;" a]     (-/alength a))
/// (defn A_1_set    [#_"[Ljava.lang.Object;" a i x] (-/aset a i x))

/// (defn new_8_ [#_"Class" c & s] (.newInstance #_"Constructor" (first (.getConstructors c)), (A_1_new s)))

/// (defn M_1_get ([m k] (-/get m k)) ([m k not_found] (-/get m k not_found)))

// (about #_"arbace.Mutable"
///     (defn #_"Mutable" Mutable_2_mutate_4_ [#_"Mutable" this, #_"Object" key, #_"Object" val] (.mutate this, key, val))
// )

// (about #_"arbace.Typed"
///     (defn #_"type" Typed_2_type [#_"Typed" this] (.type this))
// )

/// (ns arbace.core
///     (refer! - [boolean char long satisfies_9_])
///     (_0_refer arbace.bore _0_only
///         [
///             int int_4_ thread array_9_ identical_9_
///             -'= -'== -'< -'<= -'> -'compare -'+ -'- -'* -'quot -'rem -'bit_not -'bit_and -'bit_or -'bit_xor -'bit_shift_left -'bit_shift_right -'unsigned_bit_shift_right
///             A_1_new A_1_clone A_1_get A_1_length A_1_set new_8_ M_1_get
///             Mutable_2_mutate_4_ Typed_2_type
///         ]
///     )
/// )

/// (refer! - [= alter_var_root conj cons count defmacro defn defonce even_9_ first fn hash_map interleave keyword keyword_9_ let list list_8_ loop map mapcat merge meta next not= nth odd_9_ partial partition range second seq seq_9_ sequential_9_ split_at str symbol symbol_9_ var_get vary_meta vec vector vector_9_ with_meta zipmap])
/// (refer! arbace.bore [* + - < << <= > >= >> >>> bit_xor dec inc neg_9_ pos_9_ quot rem zero_9_])

/// (defmacro case_4_ [e & clauses] (if (odd_9_ (count clauses)) `(condp = ~e ~@clauses) `(condp = ~e ~@clauses (throw (str ~e " is definitely not that case!")))))

/// (let [last_id_1_ (-/atom 0)] (defn next_id_4_ [] (-/swap_4_ last_id_1_ inc)))

/// (defn gensym
///     ([] (gensym "G__"))
///     ([prefix] (-/symbol (str prefix (next_id_4_))))
/// )

/// (defmacro declare [& names] `(do ~@(map #(list _1_def (vary_meta % -/assoc _0_declared true)) names)))

/// (defmacro def_      [x & s] `(def      ~(vary_meta x -/assoc _0_private true) ~@s))
/// (defmacro defn_     [x & s] `(defn     ~(vary_meta x -/assoc _0_private true) ~@s))
/// (defmacro defmacro_ [x & s] `(defmacro ~(vary_meta x -/assoc _0_private true) ~@s))

/// (defn identity   [x] x)
/// (defn constantly [x] (fn [& _] x))

/// (defn nil_9_   [x] (identical_9_ x nil))
/// (defn false_9_ [x] (identical_9_ x false))
/// (defn true_9_  [x] (identical_9_ x true))
/// (defn not    [x] (if x false true))
/// (defn some_9_  [x] (not (nil_9_ x)))
/// (defn any_9_   [_] true)

/// (defmacro if_not
///     ([_9_ then] (if_not _9_ then nil))
///     ([_9_ then else] (list _1_if _9_ else then))
/// )

/// (defmacro and
///     ([] true)
///     ([x] x)
///     ([x & s] `(let [and# ~x] (if and# (and ~@s) and#)))
/// )

/// (defmacro or
///     ([] nil)
///     ([x] x)
///     ([x & s] `(let [or# ~x] (if or# or# (or ~@s))))
/// )

/// (defmacro any
///     ([f x y] `(~f ~x ~y))
///     ([f x y & s] `(let [f# ~f x# ~x] (or (f# x# ~y) (any f# x# ~@s))))
/// )

/// (defmacro letfn [fnspecs & body]
///     `(letfn_8_ ~(vec (interleave (map first fnspecs) (map #(cons _7_fn %) fnspecs))) ~@body)
/// )

/// (letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
///     (defmacro     when       [_9_ & s] (let [[e & s] (=> s)]               `(if     ~_9_ (do ~@s) ~e)))
///     (defmacro     when_not   [_9_ & s] (let [[e & s] (=> s)]               `(if_not ~_9_ (do ~@s) ~e)))
///     (defmacro let_when     [v _9_ & s] (let [[e & s] (=> s)] `(let ~(vec v) (if     ~_9_ (do ~@s) ~e))))
///     (defmacro let_when_not [v _9_ & s] (let [[e & s] (=> s)] `(let ~(vec v) (if_not ~_9_ (do ~@s) ~e))))
/// )

/// (defmacro cond [& s]
///     (when s
///         `(if ~(first s)
///             ~(when (next s) => (throw "cond requires an even number of forms")
///                 (second s)
///             )
///             (cond ~@(next (next s)))
///         )
///     )
/// )

/// (defmacro_ assert_args [& s]
///     `(when ~(first s) ~'=> (throw (str (first ~'&form) " requires " ~(second s)))
///         ~(let_when [s (next (next s))] s
///             `(assert_args ~@s)
///         )
///     )
/// )

/// (defmacro if_let
///     ([bind then] `(if_let ~bind ~then nil))
///     ([bind then else & _]
///         (assert_args
///             (vector_9_ bind) "a vector for its binding"
///             (= 2 (count bind)) "exactly 2 forms in binding vector"
///             (nil_9_ _) "1 or 2 forms after binding vector"
///         )
///         `(let_when [x# ~(bind 1)] x# ~'=> ~else
///             (let [~(bind 0) x#]
///                 ~then
///             )
///         )
///     )
/// )

/// (defmacro cond_let [bind then & else]
///     (let [bind (if (vector_9_ bind) bind [`_# bind])]
///         `(if_let ~bind ~then ~(when else `(cond_let ~@else)))
///     )
/// )

/// (defmacro if_some
///     ([bind then] `(if_some ~bind ~then nil))
///     ([bind then else & _]
///         (assert_args
///             (vector_9_ bind) "a vector for its binding"
///             (= 2 (count bind)) "exactly 2 forms in binding vector"
///             (nil_9_ _) "1 or 2 forms after binding vector"
///         )
///         `(let_when [x# ~(bind 1)] (some_9_ x#) ~'=> ~else
///             (let [~(bind 0) x#]
///                 ~then
///             )
///         )
///     )
/// )

/// (defmacro cond_some [bind then & else]
///     (let [bind (if (vector_9_ bind) bind [`_# bind])]
///         `(if_some ~bind ~then ~(when else `(cond_some ~@else)))
///     )
/// )

/// (defmacro if_first
///     ([bind then] `(if_first ~bind ~then nil))
///     ([bind then else & _]
///         (assert_args
///             (vector_9_ bind) "a vector for its binding"
///             (= 2 (count bind)) "exactly 2 forms in binding vector"
///             (nil_9_ _) "1 or 2 forms after binding vector"
///         )
///         `(let_when [s# (seq ~(bind 1))] (some_9_ s#) ~'=> ~else
///             (let [~(bind 0) (first s#)]
///                 ~then
///             )
///         )
///     )
/// )

/// (ß defmacro when_let [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (= 2 (count bindings)) "exactly 2 forms in binding vector"
///     )
///     `(let_when [x# ~(bindings 1)] x#
///         (let [~(bindings 0) x#]
///             ~@body
///         )
///     )
/// )

/// (ß defmacro when_some [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (= 2 (count bindings)) "exactly 2 forms in binding vector"
///     )
///     `(let_when [x# ~(bindings 1)] (some_9_ x#)
///         (let [~(bindings 0) x#]
///             ~@body
///         )
///     )
/// )

/// (ß defmacro when_first [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (= 2 (count bindings)) "exactly 2 forms in binding vector"
///     )
///     `(when_some [s# (seq ~(bindings 1))]
///         (let [~(bindings 0) (first s#)]
///             ~@body
///         )
///     )
/// )

/// (letfn [(=> [s] (if (= '=> (first s)) (next s) (cons nil s)))]
///     (defmacro when_let   [v & s] (let [[e & s] (=> s)] `(if_let   ~(vec v) (do ~@s) ~e)))
///     (defmacro when_some  [v & s] (let [[e & s] (=> s)] `(if_some  ~(vec v) (do ~@s) ~e)))
///     (defmacro when_first [v & s] (let [[e & s] (=> s)] `(if_first ~(vec v) (do ~@s) ~e)))
/// )

/// (defmacro condp [f_9_ expr & clauses]
///     (let [gpred (gensym "pred__") gexpr (gensym "expr__")
///           emit_
///             (fn emit_ [f_9_ expr args]
///                 (let [[[a b c _0_as clause] more] (split_at (if (= :>> (second args)) 3 2) args) n (count clause)]
///                     (cond
///                         (= 0 n) `(throw (str "no matching clause: " ~expr))
///                         (= 1 n) a
///                         (= 2 n) `(if (~f_9_ ~a ~expr)
///                                     ~b
///                                     ~(emit_ f_9_ expr more)
///                                 )
///                         _0_else   `(if_let [p# (~f_9_ ~a ~expr)]
///                                     (~c p#)
///                                     ~(emit_ f_9_ expr more)
///                                 )
///                     )
///                 )
///             )]
///         `(let [~gpred ~f_9_ ~gexpr ~expr]
///             ~(emit_ gpred gexpr clauses)
///         )
///     )
/// )

/// (letfn [(v_1_ [v] (cond (vector_9_ v) v (symbol_9_ v) [v v] _0_else [`_# v]))
///         (r_1_ [r] (cond (vector_9_ r) `((recur ~@r)) (some_9_ r) `((recur ~r))))
///         (=> [s] (if (= '=> (first s)) (next s) (cons nil s)))
///         (l_1_ [v _9_ r s] (let [r (r_1_ r) [e & s] (=> s)] `(loop ~(v_1_ v) (if ~_9_ (do ~@s ~@r) ~e))))]
///     (defmacro loop_when [v _9_ & s] (l_1_ v _9_ nil s))
///     (defmacro loop_when_recur [v _9_ r & s] (l_1_ v _9_ r s))
/// )

/// (letfn [(r_1_ [r] (cond (vector_9_ r) `(recur ~@r) (some_9_ r) `(recur ~r)))
///         (=> [s] (if (= '=> (first s)) (second s)))]
///     (defmacro recur_when [_9_ r & s] `(if ~_9_ ~(r_1_ r) ~(=> s)))
/// )

/// (defmacro while [_9_ & s]
///     `(loop [] (when ~_9_ ~@s (recur)))
/// )

/// (defmacro doseq [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (even_9_ (count bindings)) "an even number of forms in binding vector"
///     )
///     (letfn [(emit_ [e r]
///                 (when e => [`(do ~@body) true]
///                     (let [[k v & e] e]
///                         (if (keyword_9_ k)
///                             (let [[f r_9_] (emit_ e r)]
///                                 (case_4_ k
///                                     _0_let   [`(let ~v ~f) r_9_]
///                                     _0_while [`(when ~v ~f ~@(when r_9_ [r])) false]
///                                     _0_when  [`(if ~v (do ~f ~@(when r_9_ [r])) ~r) false]
///                                 )
///                             )
///                             (let [s (gensym "s__") r `(recur (next ~s)) [f r_9_] (emit_ e r)]
///                                 [`(loop_when [~s (seq ~v)] ~s (let [~k (first ~s)] ~f ~@(when r_9_ [r]))) true]
///                             )
///                         )
///                     )
///                 )
///             )]
///         (first (emit_ (seq bindings) nil))
///     )
/// )

/// (defmacro dotimes [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (= 2 (count bindings)) "exactly 2 forms in binding vector"
///     )
///     (let [[i n] bindings]
///         `(let [n# (long ~n)]
///             (loop_when_recur [~i 0] (< ~i n#) [(inc ~i)]
///                 ~@body
///             )
///         )
///     )
/// )

/// (defmacro doto [x & s]
///     (let [x_1_ (gensym)]
///         `(let [~x_1_ ~x]
///             ~@(map (fn [f] (with_meta (if (seq_9_ f) `(~(first f) ~x_1_ ~@(next f)) `(~f ~x_1_)) (meta f))) s)
///             ~x_1_
///         )
///     )
/// )

/// (defmacro -> [x & s]
///     (when s => x
///         (recur &form &env
///             (let_when [f (first s)] (seq_9_ f) => (list f x)
///                 (with_meta `(~(first f) ~x ~@(next f)) (meta f))
///             )
///             (next s)
///         )
///     )
/// )

/// (defmacro ->> [x & s]
///     (when s => x
///         (recur &form &env
///             (let_when [f (first s)] (seq_9_ f) => (list f x)
///                 (with_meta `(~(first f) ~@(next f) ~x) (meta f))
///             )
///             (next s)
///         )
///     )
/// )

/// (defmacro locking [x & body]
///     `(let [lockee# ~x]
///         (try
///             (monitor_enter lockee#)
///             ~@body
///             (finally
///                 (monitor_exit lockee#)
///             )
///         )
///     )
/// )

// (about #_"defp, defq, defr, defm"

// (about #_"defproto"

/// #_bore!
/// (defn_ gen_interface_8_ [sym]
///     (DynamicClassLoader_2_defineClass (var_get Compiler_1_LOADER), (str sym), (second (#'-/generate_interface (hash_map (-/keyword (-/name _0_name)) sym))), nil)
/// )

/// (defn_ emit_defproto_8_ [name sigs]
///     (let [
///         iname (-/symbol (str (-/munge (-/namespace_munge -/_8_ns_8_)) "." (-/munge name)))
///     ]
///         `(do
///             #_bore!
///             (defonce ~name (hash_map)) #_alt #_(declare ~name) #_(refer_8_ '~name)
///             #_bore!
///             (gen_interface_8_ '~iname)
///             (alter_var_root (var ~name) merge
///                 ~(hash_map _0_var (list _1_var name), _0_on (list _1_quote iname), _0_on_interface (list `-/resolve (list _1_quote iname)))
///             )
///             ~@(map (fn [[f & _]] `(defmacro ~f [x# & s#] (list_8_ (list -/find_protocol_method '~name ~(-/keyword (str f)) x#) x# s#))) sigs)
///             '~name
///         )
///     )
/// )

/// (defmacro defproto [name & sigs]
///     (emit_defproto_8_ name sigs)
/// )
// )

/// #_bore!
/// (defn_ parse_opts [s]
///     (loop_when_recur [opts {} [k v & rs _0_as s] s] (keyword_9_ k) [(-/assoc opts k v) rs] => [opts s])
/// )

/// #_bore!
/// (refer! - [take_while drop_while])

/// #_bore!
/// (defn_ parse_impls [specs]
///     (loop_when_recur [impls {} s specs] (seq s) [(-/assoc impls (first s) (take_while seq_9_ (next s))) (drop_while seq_9_ (next s))] => impls)
/// )

/// #_bore!
/// (refer! - [#_var_9_ complement resolve deref keys maybe_destructured apply concat vals])

/// #_bore!
/// (defn_ parse_opts+specs [opts+specs]
///     (let [
///         [opts specs] (parse_opts opts+specs)
///         impls        (parse_impls specs)
///         interfaces   (-> (map #(if (#_var_9_ (complement -/class_9_) (resolve %)) (_0_on (deref (resolve %))) %) (keys impls)) -/set (-/disj _1_Object _1_java.lang.Object) vec)
///         methods      (map (fn [[name params & body]] (-/cons name (maybe_destructured params body))) (apply concat (vals impls)))
///     ]
///         [interfaces methods opts]
///     )
/// )

// (about #_"arbace.Mutable"
///     (ß defp Mutable
///         (#_"Mutable" Mutable_3_mutate_4_ [#_"Mutable" this, #_"Object" key, #_"Object" val])
///     )

///     #_bore!
///     (defonce Mutable (hash_map)) #_alt #_(refer_8_ _1_Mutable)
///     #_bore!
///     (DynamicClassLoader_2_defineClass (var_get Compiler_1_LOADER), "arbace.core.Mutable", (second (#'-/generate_interface (hash_map (-/keyword (-/name _0_name)) _1_arbace.core.Mutable, (-/keyword (-/name _0_methods)) '[[mutate [java.lang.Object java.lang.Object] java.lang.Object nil]]))), nil)
///     (alter_var_root #_1_Mutable merge (hash_map _0_var #_1_Mutable, _0_on _1_arbace.core.Mutable, _0_on_interface (-/resolve (-/symbol "arbace.core.Mutable"))))
///     (ß defmacro Mutable_3_mutate_4_ [this, key, val] ((-/find_protocol_method _1_Mutable (-/keyword "Mutable_3_mutate_4_") this) this, key, val))

///     (ß -/extend_protocol Mutable arbace.core.Mutable
///         (Mutable_3_mutate_4_ [this, key, val] (Mutable_2_mutate_4_ this, key, val))
///     )

///     (defn mutable_9_ [x] (satisfies_9_ Mutable x))
// )

// (about #_"arbace.Typed"
///     (ß defp Typed
///         (#_"type" Typed_3_type [#_"Typed" this])
///     )

///     #_bore!
///     (defonce Typed (hash_map)) #_alt #_(refer_8_ _1_Typed)
///     #_bore!
///     (DynamicClassLoader_2_defineClass (var_get Compiler_1_LOADER), "arbace.core.Typed", (second (#'-/generate_interface (hash_map (-/keyword (-/name _0_name)) _1_arbace.core.Typed, (-/keyword (-/name _0_methods)) '[[type [] java.lang.Object nil]]))), nil)
///     (alter_var_root #_1_Typed merge (hash_map _0_var #_1_Typed, _0_on _1_arbace.core.Typed, _0_on_interface (-/resolve (-/symbol "arbace.core.Typed"))))
///     (ß defmacro Typed_3_type [this] ((-/find_protocol_method _1_Typed (-/keyword "Typed_3_type") this) this))

///     (ß -/extend_protocol Typed arbace.core.Typed
///         (Typed_3_type [this] (Typed_2_type this))
///     )

///     (defn typed_9_ [x] (satisfies_9_ Typed x))
// )

// (about #_"defarray"

/// #_bore!
/// (defn_ emit_defarray_8_ [tname cname fields interfaces methods opts]
///     (let [
///         classname  (-/with_meta (-/symbol (str (-/namespace_munge -/_8_ns_8_) "." cname)) (meta cname))
///         interfaces (vec interfaces)
///         fields     (map #(with_meta % nil) fields)
///     ]
///         (let [a '__array s (mapcat (fn [x y] [(-/name #_keyword y) x]) (range) fields)]
///             (letfn [(ilookup [[i m]]
///                         [
///                             (conj i _1_clojure.lang.ILookup)
///                             (conj m
///                                 `(valAt [this# k#] (ILookup_2_valAt this# k# nil))
///                                 `(valAt [this# k# else#] (if_some [x# (case_4_ (-/name k#) ~@s nil)] (#_A_1_get -/aget (. this# ~a) x#) else#))
///                             )
///                         ]
///                     )
///                     (mutable [[i m]]
///                         [
///                             (conj i _1_arbace.core.Mutable)
///                             (conj m
///                                 `(mutate [this# k# v#] (let [x# (case_4_ (-/name k#) ~@s)] (#_A_1_set -/aset (. this# ~a) x# v#) this#))
///                             )
///                         ]
///                     )
///                     (typed [[i m]]
///                         [
///                             (conj i _1_arbace.core.Typed)
///                             (conj m
///                                 `(type [this#] '~classname)
///                             )
///                         ]
///                     )]
///                 (let [[i m] (-> [interfaces methods] ilookup mutable typed)]
///                     `(-/eval '~(-/read_string (str (list_8_ _1_deftype_8_ (symbol (-/name (-/ns_name -/_8_ns_8_)) (-/name tname)) classname (vector a) _0_implements (vec i) m))))
///                 )
///             )
///         )
///     )
/// )

/// #_bore!
/// (defmacro defarray [name fields & opts+specs] #_alt #_`(refer_8_ '~name)
///     (ß #'-/validate_fields fields name)
///     (let [[interfaces methods opts] (parse_opts+specs opts+specs)]
///         `(do
///             ~(emit_defarray_8_ name name (vec fields) (vec interfaces) methods opts)
///             (-/eval '~(-/list (-/symbol "clojure.core/import_8_") (str (-/namespace_munge -/_8_ns_8_) "." name)))
///         )
///     )
/// )
// )

// (about #_"defassoc"

/// #_bore!
/// (defn_ emit_defassoc_8_ [tname cname interfaces methods opts]
///     (let [
///         classname  (-/with_meta (-/symbol (str (-/namespace_munge -/_8_ns_8_) "." cname)) (meta cname))
///         interfaces (vec interfaces)
///         type_hash  (IHashEq_2_hasheq classname)
///     ]
///         (let [a '__assoc]
///             (letfn [(eqhash [[i m]]
///                         [
///                             (conj i _1_clojure.lang.IHashEq)
///                             (conj m
///                                 `(hasheq [this#] (-/int (bit_xor ~type_hash (.hasheq (. this# ~a)))))
///                                 `(hashCode [this#] (.hashCode (. this# ~a)))
///                                 `(equals [this# that#] (and #_(some_9_ that#) (instance_9_ ~tname that#) (.equals (. this# ~a) (. that# ~a))))
///                             )
///                         ]
///                     )
///                     (iobj [[i m]]
///                         [
///                             (conj i _1_clojure.lang.IObj)
///                             (conj m
///                                 `(meta [this#] (.meta (. this# ~a)))
///                                 `(withMeta [this# m#] (new ~tname (.withMeta (. this# ~a) m#)))
///                             )
///                         ]
///                     )
///                     (ilookup [[i m]]
///                         [
///                             (conj i _1_clojure.lang.ILookup)
///                             (conj m
///                                 `(valAt [this# k#] (.valAt this# k# nil))
///                                 `(valAt [this# k# else#] (.valAt (. this# ~a) k# else#))
///                             )
///                         ]
///                     )
///                     (imap [[i m]]
///                         [
///                             (conj i _1_clojure.lang.IPersistentMap)
///                             (conj m
///                                 `(count [this#] (.count (. this# ~a)))
///                                 `(empty [this#] (new ~tname (.empty (. this# ~a))))
///                                 `(cons [this# e#] (new ~tname (.cons (. this# ~a) e#)))
///                                 `(equiv [this# that#]
///                                     (or (identical_9_ this# that#)
///                                         (and (identical_9_ (-/class this#) (-/class that#))
///                                             (= (. this# ~a) (. that# ~a))
///                                         )
///                                     )
///                                 )
///                                 `(containsKey [this# k#] (.containsKey (. this# ~a) k#))
///                                 `(entryAt [this# k#] (.entryAt (. this# ~a) k#))
///                                 `(seq [this#] (.seq (. this# ~a)))
///                                 `(assoc [this# k# v#] (new ~tname (.assoc (. this# ~a) k# v#)))
///                                 `(without [this# k#] (new ~tname (.without (. this# ~a) k#)))
///                             )
///                         ]
///                     )
///                     (typed [[i m]]
///                         [
///                             (conj i _1_arbace.core.Typed)
///                             (conj m
///                                 `(type [this#] '~classname)
///                             )
///                         ]
///                     )]
///                 (let [[i m] (-> [interfaces methods] eqhash iobj ilookup imap typed)]
///                     `(-/eval '~(-/read_string (str (list_8_ _1_deftype_8_ (symbol (-/name (-/ns_name -/_8_ns_8_)) (-/name tname)) classname (vector a) _0_implements (vec i) m))))
///                 )
///             )
///         )
///     )
/// )

/// #_bore!
/// (defmacro defassoc [name & opts+specs] #_alt #_`(refer_8_ '~name)
///     (ß #'-/validate_fields [] name)
///     (let [[interfaces methods opts] (parse_opts+specs opts+specs)]
///         `(do
///             ~(emit_defassoc_8_ name name (vec interfaces) methods opts)
///             (-/eval '~(-/list (-/symbol "clojure.core/import_8_") (str (-/namespace_munge -/_8_ns_8_) "." name)))
///         )
///     )
/// )
// )

// (about #_"extend"

/// (defn extend [atype & proto+mmaps]
///     (doseq [[proto mmap] (partition 2 proto+mmaps)]
///         (when_not (#'-/protocol_9_ proto)
///             (throw (str proto " is not a protocol"))
///         )
///         (when (#'-/implements_9_ proto atype)
///             (throw (str atype " already directly implements " (_0_on_interface proto) " for protocol " (_0_var proto)))
///         )
///         (alter_var_root (_0_var proto) -/assoc_in [_0_impls atype] mmap)
///     )
/// )

/// (defn_ emit_impl_8_ [_ [p fs]]
///     [p (-/zipmap (map #(-> % first -/name -/keyword) fs) (map #(let [% (next %)] (if (= '=> (first %)) (second %) (cons _7_fn %))) fs))]
/// )

/// (defmacro extend_type [t & specs]
///     `(extend ~t ~@(mapcat (partial emit_impl_8_ t) (#'-/parse_impls specs)))
/// )
// )

/// (defmacro defp [p & s]                                      `(do (defproto ~p ~@s)             '~p))
/// (defmacro defq [r f & s] (let [c (-/symbol (str r "'class"))] `(do (defarray ~c ~(vec f) ~r ~@s) '~c)))
/// (defmacro defr [r]       (let [c (-/symbol (str r "'class"))] `(do (defassoc ~c ~r)              '~c)))
/// (defmacro defm [r & s]   (let [i `(_0_on_interface ~r)]       `(do (extend_type ~i ~@s)          ~i)))
// )

namespace arbace {
    class ISeq;

    class Seqable {
        public: virtual ISeq* Seqable_3_seq() = 0;
    };

///     (defn seqable_9_ [x] (satisfies_9_ Seqable x))

///     (defn #_"ISeq" seq [x] (when (some_9_ x) (Seqable_3_seq x)))

///     (defn empty_9_ [x] (not (seq x)))
}

namespace arbace {
    class ISeq {
        public: virtual Object* ISeq_3_first() = 0;
        public: virtual ISeq* ISeq_3_next() = 0;
    };

///     (defn seq_9_ [x] (satisfies_9_ ISeq x))

///     (defn first [s] (if (seq_9_ s) (ISeq_3_first s) (when_some [s (seq s)] (ISeq_3_first s))))

///     (defn #_"ISeq" next [s] (if (seq_9_ s) (ISeq_3_next s) (when_some [s (seq s)] (ISeq_3_next s))))

///     (defn second [s] (first (next s)))
///     (defn third  [s] (first (next (next s))))
///     (defn fourth [s] (first (next (next (next s)))))
///     (defn last   [s] (if_some [r (next s)] (recur r) (first s)))
}

namespace arbace {
    class IObject {
        public: virtual bool IObject_3_equals(const Object* that) = 0;
    };

///     (-/extend_protocol IObject java.lang.Object
///         (IObject_3_equals [this, that] (.equals this, that))
///     )
}

namespace arbace {
    class IAppend {
        public: virtual Appendable* IAppend_3_append(const Appendable* a) = 0;
    };
}

namespace arbace {
    class Comparable {
        public: virtual int Comparable_3_compareTo(const Object* that) = 0;
    };

///     (defn comparable_9_ [x] (satisfies_9_ Comparable x))
}

namespace arbace {
    class Comparator {
        public: virtual int Comparator_3_compare(const Object* x, const Object* y) = 0;
    };

///     (defn comparator_9_ [x] (satisfies_9_ Comparator x))
}

namespace arbace {
    class Counted {
        public: virtual int Counted_3_count() = 0;
    };

///     (-/extend_protocol Counted
///         java.lang.CharSequence (Counted_3_count [s] (.length s))
///     )

///     (-/extend_protocol Counted
///         (do Object_1_array) (Counted_3_count [a] (Array_1_getLength a))
///     )

///     (defn counted_9_ [x] (satisfies_9_ Counted x))

///     (defn count
///         ([x] (count x -1))
///         ([x m]
///             (cond
///                 (nil_9_ x)
///                     0
///                 (counted_9_ x)
///                     (Counted_3_count x)
///                 (seqable_9_ x)
///                     (loop_when [n 0 s (seq x)] (and (some_9_ s) (or (neg_9_ m) (< n m))) => n
///                         (when (counted_9_ s) => (recur (inc n) (next s))
///                             (+ n (Counted_3_count s))
///                         )
///                     )
///                 _0_else
///                     (throw (str "count not supported on " x))
///             )
///         )
///     )
}

namespace arbace {
    class Hashed {
        public: virtual int Hashed_3_hash() = 0;
    };

///     (declare Murmur3_1_hashInt)
///     (declare Murmur3_1_hashLong)

///     (-/extend_protocol Hashed
///         java.lang.Object       (Hashed_3_hash [o] (Object_2_hashCode o))
///         java.lang.String       (Hashed_3_hash [s] (Murmur3_1_hashInt (Object_2_hashCode s)))
///         java.lang.Number       (Hashed_3_hash [n] (Murmur3_1_hashLong (Number_2_longValue n)))
///         arbace.math.BigInteger (Hashed_3_hash [i] (if (< (BigInteger_2_bitLength i) 64) (Murmur3_1_hashLong (BigInteger_2_longValue i)) (Object_2_hashCode i)))
///         clojure.lang.Ratio     (Hashed_3_hash [r] (Object_2_hashCode r))
///     )

///     (defn hashed_9_ [x] (satisfies_9_ Hashed x))

///     (defn f_1_hash [x] (if (some_9_ x) (Hashed_3_hash x) (int 0)))

///     (defn f_1_hashcode [x] (if (some_9_ x) (Object_2_hashCode x) (int 0)))

///     (defn hash_combine [seed x]
///         (bit_xor seed (+ (f_1_hashcode x) (int_4_ 0x9e3779b9) (<< seed 6) (>> seed 2)))
///     )
}

namespace arbace {
    class IFn {
        public: virtual Object* IFn_3_invoke() = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4, const Object* a5) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4, const Object* a5, const Object* a6) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4, const Object* a5, const Object* a6, const Object* a7) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4, const Object* a5, const Object* a6, const Object* a7, const Object* a8) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4, const Object* a5, const Object* a6, const Object* a7, const Object* a8, const Object* a9) = 0;
        public: virtual Object* IFn_3_invoke(const Object* a1, const Object* a2, const Object* a3, const Object* a4, const Object* a5, const Object* a6, const Object* a7, const Object* a8, const Object* a9, const ISeq* args) = 0;
        public: virtual Object* IFn_3_applyTo(const ISeq* args) = 0;
    };

///     (declare anew)

///     (defn ifn_9_ [x] (satisfies_9_ IFn x))

///     (declare cons)

///     (defn_ spread [s]
///         (cond
///             (nil_9_ s) nil
///             (nil_9_ (next s)) (seq (first s))
///             _0_else (cons (first s) (spread (next s)))
///         )
///     )

///     (defn list_8_
///         ([s] (seq s))
///         ([a s] (cons a s))
///         ([a b s] (cons a (cons b s)))
///         ([a b c s] (cons a (cons b (cons c s))))
///         ([a b c d & s] (cons a (cons b (cons c (cons d (spread s))))))
///     )

///     (defn apply
///         ([#_"IFn" f s] (IFn_3_applyTo f, (seq s)))
///         ([#_"IFn" f a s] (IFn_3_applyTo f, (list_8_ a s)))
///         ([#_"IFn" f a b s] (IFn_3_applyTo f, (list_8_ a b s)))
///         ([#_"IFn" f a b c s] (IFn_3_applyTo f, (list_8_ a b c s)))
///         ([#_"IFn" f a b c d & s] (IFn_3_applyTo f, (cons a (cons b (cons c (cons d (spread s)))))))
///     )

///     (defn complement [f]
///         (fn
///             ([] (not (f)))
///             ([x] (not (f x)))
///             ([x y] (not (f x y)))
///             ([x y & s] (not (apply f x y s)))
///         )
///     )
}

namespace arbace {
    class INamed {
        public: virtual String* INamed_3_getNamespace() = 0;
        public: virtual String* INamed_3_getName() = 0;
    };

///     (defn named_9_ [x] (satisfies_9_ INamed x))

///     (defn #_"String" namespace [#_"INamed" x] (INamed_3_getNamespace x))

///     (defn #_"String" name [x] (if (string_9_ x) x (INamed_3_getName #_"INamed" x)))
}

namespace arbace {
    class IPersistentMap;

    class IMeta {
        public: virtual IPersistentMap* IMeta_3_meta() = 0;
    };

///     (defn meta [x] (when (satisfies_9_ IMeta x) (IMeta_3_meta #_"IMeta" x)))
}

namespace arbace {
    class IObj {
        public: virtual IObj* IObj_3_withMeta(const IPersistentMap* meta) = 0;
    };

///     (defn with_meta [#_"IObj" x m] (IObj_3_withMeta x, m))

///     (defn vary_meta [x f & args] (with_meta x (apply f (meta x) args)))
}

namespace arbace {
    class IReference {
        public: virtual IPersistentMap* IReference_3_alterMeta(const IFn* f, const ISeq* args) = 0;
        public: virtual IPersistentMap* IReference_3_resetMeta(const IPersistentMap* m) = 0;
    };

///     (defn alter_meta_4_ [#_"IReference" r f & args] (IReference_3_alterMeta r, f, args))

///     (defn reset_meta_4_ [#_"IReference" r m] (IReference_3_resetMeta r, m))
}

namespace arbace {
    class IDeref {
        public: virtual Object* IDeref_3_deref() = 0;
    };

///     (defn deref [#_"IDeref" ref] (IDeref_3_deref ref))
}

namespace arbace {
    class IAtom {
        public: virtual bool IAtom_3_compareAndSet(const Object* o, const Object* o_1_) = 0;
        public: virtual Object* IAtom_3_swap(const IFn* f, const ISeq* args) = 0;
        public: virtual Object* IAtom_3_reset(const Object* o_1_) = 0;
///        public: virtual "[Object Object]" IAtom_3_swapVals(const IFn* f, const ISeq* args) = 0;
///        public: virtual "[Object Object]" IAtom_3_resetVals(const Object* o_1_) = 0;
    };
}

namespace arbace {
    class IPending {
        public: virtual bool IPending_3_isRealized() = 0;
    };

///     (defn realized_9_ [#_"IPending" x] (IPending_3_isRealized x))
}

namespace arbace {
    class Sequential { };

///     (defn sequential_9_ [x] (satisfies_9_ Sequential x))
}

namespace arbace {
    class Reversible {
        public: virtual ISeq* Reversible_3_rseq() = 0;
    };

///     (defn reversible_9_ [x] (satisfies_9_ Reversible x))

///     (defn rseq [#_"Reversible" s] (Reversible_3_rseq s))
}

namespace arbace {
    class Sorted {
        public: virtual Comparator* Sorted_3_comparator() = 0;
        public: virtual Object* Sorted_3_entryKey(const Object* entry) = 0;
        public: virtual ISeq* Sorted_3_seq(bool ascending_9_) = 0;
        public: virtual ISeq* Sorted_3_seqFrom(const Object* key, bool ascending_9_) = 0;
    };

///     (defn sorted_9_ [x] (satisfies_9_ Sorted x))
}

namespace arbace {
    class Indexed {
        public: virtual Object* Indexed_3_nth(int i) = 0;
        public: virtual Object* Indexed_3_nth(int i, const Object* not_found) = 0;
    };

///     (defn indexed_9_ [x] (satisfies_9_ Indexed x))

///     (defn nthnext [s n] (loop_when_recur [s (seq s) n n] (and s (pos_9_ n)) [(next s) (dec n)] => s))
}

namespace arbace {
    class ILookup {
        public: virtual Object* ILookup_3_valAt(const Object* key) = 0;
        public: virtual Object* ILookup_3_valAt(const Object* key, const Object* not_found) = 0;
    };
}

namespace arbace {
    class IPersistentCollection {
        public: virtual IPersistentCollection* IPersistentCollection_3_conj(const Object* o) = 0;
        public: virtual IPersistentCollection* IPersistentCollection_3_empty() = 0;
    };

///     (defn coll_9_ [x] (satisfies_9_ IPersistentCollection x))

///     (declare vector)
///     (declare list)

///     (defn conj
///         ([] (vector))
///         ([c] c)
///         ([c x] (if (some_9_ c) (IPersistentCollection_3_conj c, x) (list x)))
///         ([c x & s]
///             (let [c (conj c x)]
///                 (recur_when s [c (first s) (next s)] => c)
///             )
///         )
///     )

///     (defn empty [coll]
///         (when (coll_9_ coll)
///             (IPersistentCollection_3_empty #_"IPersistentCollection" coll)
///         )
///     )

///     (defn not_empty [coll] (when (seq coll) coll))
}

namespace arbace {
    class ITransientCollection;

    class IEditableCollection {
        public: virtual ITransientCollection* IEditableCollection_3_asTransient() = 0;
    };

///     (defn editable_9_ [x] (satisfies_9_ IEditableCollection x))

///     (defn transient [#_"IEditableCollection" coll] (IEditableCollection_3_asTransient coll))
}

namespace arbace {
    class IMapEntry {
        public: virtual Object* IMapEntry_3_key() = 0;
        public: virtual Object* IMapEntry_3_val() = 0;
    };

///     (defn map_entry_9_ [x] (satisfies_9_ IMapEntry x))

///     (defn key [#_"IMapEntry" e] (IMapEntry_3_key e))
///     (defn val [#_"IMapEntry" e] (IMapEntry_3_val e))

///     (declare map)

///     (defn keys [m] (not_empty (map key m)))
///     (defn vals [m] (not_empty (map val m)))
}

namespace arbace {
    class Associative {
        public: virtual Associative* Associative_3_assoc(const Object* key, const Object* val) = 0;
        public: virtual bool Associative_3_containsKey(const Object* key) = 0;
        public: virtual IMapEntry* Associative_3_entryAt(const Object* key) = 0;
    };

///     (defn associative_9_ [x] (satisfies_9_ Associative x))

///     (declare PersistentArrayMap_1_new)

///     (defn assoc
///         ([#_"Associative" a k v]
///             (if (some_9_ a)
///                 (Associative_3_assoc a, k, v)
///                 (PersistentArrayMap_1_new (anew [ k, v ]))
///             )
///         )
///         ([a k v & kvs]
///             (let_when [a (assoc a k v)] kvs => a
///                 (when (next kvs) => (throw "assoc expects even number of arguments after map/vector, found odd number")
///                     (recur a (first kvs) (second kvs) (next (next kvs)))
///                 )
///             )
///         )
///     )

///     (declare get)

///     (defn assoc_in [m [k & ks] v]
///         (if ks
///             (assoc m k (assoc_in (get m k) ks v))
///             (assoc m k v)
///         )
///     )

///     (defn update
///         ([m k f] (assoc m k (f (get m k))))
///         ([m k f x] (assoc m k (f (get m k) x)))
///         ([m k f x y] (assoc m k (f (get m k) x y)))
///         ([m k f x y & z] (assoc m k (apply f (get m k) x y z)))
///     )

///     (defn update_in [m ks f & args]
///         (let [[k & ks] ks]
///             (if ks
///                 (assoc m k (apply update_in (get m k) ks f args))
///                 (assoc m k (apply f (get m k) args))
///             )
///         )
///     )
}

namespace arbace {
    class IPersistentMap {
        public: virtual IPersistentMap* IPersistentMap_3_dissoc(const Object* key) = 0;
    };

///     (defn map_9_ [x] (satisfies_9_ IPersistentMap x))

///     (defn dissoc
///         ([m] m)
///         ([#_"IPersistentMap" m k] (when (some_9_ m) (IPersistentMap_3_dissoc m, k)))
///         ([m k & ks]
///             (when_some [m (dissoc m k)]
///                 (recur_when ks [m (first ks) (next ks)] => m)
///             )
///         )
///     )
}

namespace arbace {
    class IPersistentSet {
        public: virtual IPersistentSet* IPersistentSet_3_disj(const Object* key) = 0;
        public: virtual bool IPersistentSet_3_contains_9_(const Object* key) = 0;
        public: virtual Object* IPersistentSet_3_get(const Object* key) = 0;
    };

///     (defn set_9_ [x] (satisfies_9_ IPersistentSet x))

///     (defn disj
///         ([s] s)
///         ([#_"IPersistentSet" s k] (when (some_9_ s) (IPersistentSet_3_disj s, k)))
///         ([s k & ks]
///             (when_some [s (disj s k)]
///                 (recur_when ks [s (first ks) (next ks)] => s)
///             )
///         )
///     )
}

namespace arbace {
    class IPersistentStack {
        public: virtual Object* IPersistentStack_3_peek() = 0;
        public: virtual IPersistentStack* IPersistentStack_3_pop() = 0;
    };

///     (defn stack_9_ [x] (satisfies_9_ IPersistentStack x))

///     (defn peek [s]
///         (when (some_9_ s)
///             (IPersistentStack_3_peek s)
///         )
///     )

///     (defn butlast [s] (loop_when_recur [v (vector) s s] (next s) [(conj v (first s)) (next s)] => (seq v)))

///     (defn pop [s]
///         (when (some_9_ s)
///             (IPersistentStack_3_pop s)
///         )
///     )
}

namespace arbace {
    class IPersistentList { };

///     (defn list_9_ [x] (satisfies_9_ IPersistentList x))
}

namespace arbace {
    class IPersistentVector {
        public: virtual IPersistentVector* IPersistentVector_3_assocN(int i, const Object* val) = 0;
        public: virtual IPersistentVector* IPersistentVector_3_slicev(int start, int end) = 0;
        public: virtual IPersistentVector* IPersistentVector_3_splicev(const IPersistentVector* that) = 0;
    };

///     (defn vector_9_ [x] (satisfies_9_ IPersistentVector x))
}

namespace arbace {
    class ITransientCollection {
        public: virtual ITransientCollection* ITransientCollection_3_conj_4_(const Object* val) = 0;
        public: virtual IPersistentCollection* ITransientCollection_3_persistent_4_() = 0;
    };

///     (defn conj_4_
///         ([] (transient (vector)))
///         ([c] c)
///         ([#_"ITransientCollection" c x] (ITransientCollection_3_conj_4_ c, x))
///         ([c x & s]
///             (let [c (conj_4_ c x)]
///                 (recur_when s [c (first s) (next s)] => c)
///             )
///         )
///     )

///     (defn persistent_4_ [#_"ITransientCollection" coll] (ITransientCollection_3_persistent_4_ coll))
}

namespace arbace {
    class ITransientAssociative {
        public: virtual ITransientAssociative* ITransientAssociative_3_assoc_4_(const Object* key, const Object* val) = 0;
        public: virtual bool ITransientAssociative_3_containsKey(const Object* key) = 0;
        public: virtual IMapEntry* ITransientAssociative_3_entryAt(const Object* key) = 0;
    };

///     (defn assoc_4_
///         ([#_"ITransientAssociative" a k v] (ITransientAssociative_3_assoc_4_ a, k, v))
///         ([a k v & kvs]
///             (let_when [a (assoc_4_ a k v)] kvs => a
///                 (when (next kvs) => (throw "assoc_4_ expects even number of arguments after map/vector, found odd number")
///                     (recur a (first kvs) (second kvs) (next (next kvs)))
///                 )
///             )
///         )
///     )
}

namespace arbace {
    class ITransientMap {
        public: virtual ITransientMap* ITransientMap_3_dissoc_4_(const Object* key) = 0;
    };

///     (defn dissoc_4_
///         ([m] m)
///         ([#_"ITransientMap" m k] (ITransientMap_3_dissoc_4_ m, k))
///         ([m k & ks]
///             (let [m (dissoc_4_ m k)]
///                 (recur_when ks [m (first ks) (next ks)] => m)
///             )
///         )
///     )
}

namespace arbace {
    class ITransientSet {
        public: virtual ITransientSet* ITransientSet_3_disj_4_(const Object* key) = 0;
        public: virtual bool ITransientSet_3_contains_9_(const Object* key) = 0;
        public: virtual Object* ITransientSet_3_get(const Object* key) = 0;
    };

///     (defn disj_4_
///         ([s] s)
///         ([#_"ITransientSet" s k] (ITransientSet_3_disj_4_ s, k))
///         ([s k & ks]
///             (let [s (disj_4_ s k)]
///                 (recur_when ks [s (first ks) (next ks)] => s)
///             )
///         )
///     )
}

namespace arbace {
    class ITransientVector {
        public: virtual ITransientVector* ITransientVector_3_assocN_4_(int i, const Object* val) = 0;
        public: virtual ITransientVector* ITransientVector_3_pop_4_() = 0;
    };

///     (defn pop_4_ [#_"ITransientVector" coll] (ITransientVector_3_pop_4_ coll))
}

namespace arbace {
    class IReduce {
        public: virtual Object* IReduce_3_reduce(const IFn* f) = 0;
        public: virtual Object* IReduce_3_reduce(const IFn* f, const Object* r) = 0;
    };
}

namespace arbace {
    class IKVReduce {
        public: virtual Object* IKVReduce_3_kvreduce(const IFn* f, const Object* r) = 0;
    };
}

namespace arbace {
    class Ratio { };

///     (defn ratio_9_ [n] (satisfies_9_ Ratio n))

///     (defn integer_9_ [n] (or (int_9_ n) (long_9_ n) (biginteger_9_ n) (byte_9_ n)))

///     (defn rational_9_ [n] (or (integer_9_ n) (ratio_9_ n)))
}

namespace arbace {
    class LongOps;
    class RatioOps;
    class BigIntOps;

    class Ops {
        public: virtual Ops* Ops_3_combine(const Ops* y) = 0;
        public: virtual Ops* Ops_3_opsWithLong(const LongOps* x) = 0;
        public: virtual Ops* Ops_3_opsWithRatio(const RatioOps* x) = 0;
        public: virtual Ops* Ops_3_opsWithBigInt(const BigIntOps* x) = 0;
        public: virtual bool Ops_3_eq(const Number* x, const Number* y) = 0;
        public: virtual bool Ops_3_lt(const Number* x, const Number* y) = 0;
        public: virtual bool Ops_3_lte(const Number* x, const Number* y) = 0;
        public: virtual bool Ops_3_isZero(const Number* x) = 0;
        public: virtual bool Ops_3_isPos(const Number* x) = 0;
        public: virtual bool Ops_3_isNeg(const Number* x) = 0;
        public: virtual Number* Ops_3_add(const Number* x, const Number* y) = 0;
        public: virtual Number* Ops_3_negate(const Number* x) = 0;
        public: virtual Number* Ops_3_inc(const Number* x) = 0;
        public: virtual Number* Ops_3_dec(const Number* x) = 0;
        public: virtual Number* Ops_3_multiply(const Number* x, const Number* y) = 0;
        public: virtual Number* Ops_3_divide(const Number* x, const Number* y) = 0;
        public: virtual Number* Ops_3_quotient(const Number* x, const Number* y) = 0;
        public: virtual Number* Ops_3_remainder(const Number* x, const Number* y) = 0;
    };

    class LongOps { };
    class RatioOps { };
    class BigIntOps { };
}

namespace arbace {
    class Atom { };
}

namespace arbace {
///     #_abstract
    class AFn { };
}

namespace arbace {
    class Symbol { };

///     (defn symbol_9_ [x] (satisfies_9_ Symbol x))
}

namespace arbace {
    class Keyword { };

///     (defn keyword_9_ [x] (satisfies_9_ Keyword x))
}

namespace arbace {
///     #_abstract
    class Fn { };

///     (defn fn_9_ [x] (satisfies_9_ Fn x))
}

namespace arbace {
    class Closure { };
}

namespace arbace {
///     #_abstract
    class ASeq { };
}

namespace arbace {
    class LazySeq { };
}

namespace arbace {
///     #_abstract
    class APersistentMap { };
}

namespace arbace {
///     #_abstract
    class APersistentSet { };
}

namespace arbace {
    class VSeq { };
    class RSeq { };
///     #_abstract
    class APersistentVector { };
}

namespace arbace {
///     #_abstract
    class AMapEntry { };
}

namespace arbace {
    class ArraySeq { };
}

namespace arbace {
///     #_abstract
    class ATransientMap { };
}

namespace arbace {
///     #_abstract
    class ATransientSet { };
}

namespace arbace {
    class Cons { };
}

namespace arbace {
    class Delay { };
}

namespace arbace {
    class Iterate { };
}

namespace arbace {
    class MapEntry { };
}

namespace arbace {
    class Namespace { };
}

namespace arbace {
    class MSeq { };
    class TransientArrayMap { };
    class PersistentArrayMap { };
}

namespace arbace {
    class INode {
///        public: virtual INode* INode_3_assoc(int shift, int hash, const Object* key, const Object* val, "bool'" addedLeaf) = 0;
        public: virtual INode* INode_3_dissoc(int shift, int hash, const Object* key) = 0;
///        public: virtual "IMapEntry|Object" INode_3_find(int shift, int hash, const Object* key) = 0;
///        public: virtual "IMapEntry|Object" INode_3_find(int shift, int hash, const Object* key, const Object* not_found) = 0;
        public: virtual ISeq* INode_3_nodeSeq() = 0;
///        public: virtual INode* INode_3_assocT("thread'" edit, int shift, int hash, const Object* key, const Object* val, "bool'" addedLeaf) = 0;
///        public: virtual INode* INode_3_dissocT("thread'" edit, int shift, int hash, const Object* key, "bool'" removedLeaf) = 0;
        public: virtual Object* INode_3_kvreduce(const IFn* f, const Object* r) = 0;
    };

    class HSeq { };
    class NSeq { };
    class TransientHashMap { };
    class ANode { };
    class BNode { };
    class CNode { };
    class PersistentHashMap { };
}

namespace arbace {
    class TransientHashSet { };
    class PersistentHashSet { };
}

namespace arbace {
    class EmptyList { };
    class PersistentList { };
}

namespace arbace {
    class QSeq { };
    class PersistentQueue { };
}

namespace arbace {
    class ITNode {
        public: virtual ITNode* ITNode_3_addLeft(const ITNode* ins) = 0;
        public: virtual ITNode* ITNode_3_addRight(const ITNode* ins) = 0;
        public: virtual ITNode* ITNode_3_removeLeft(const ITNode* del) = 0;
        public: virtual ITNode* ITNode_3_removeRight(const ITNode* del) = 0;
        public: virtual ITNode* ITNode_3_blacken() = 0;
        public: virtual ITNode* ITNode_3_redden() = 0;
        public: virtual ITNode* ITNode_3_balanceLeft(const ITNode* parent) = 0;
        public: virtual ITNode* ITNode_3_balanceRight(const ITNode* parent) = 0;
        public: virtual ITNode* ITNode_3_replace(const Object* key, const Object* val, const ITNode* left, const ITNode* right) = 0;
    };

///     #_abstract
    class TNode { };
    class Black { };
    class BlackVal { };
    class BlackBranch { };
    class BlackBranchVal { };
    class Red { };
    class RedVal { };
    class RedBranch { };
    class RedBranchVal { };
    class TSeq { };
    class PersistentTreeMap { };
}

namespace arbace {
    class PersistentTreeSet { };
}

namespace arbace {
    class VNode { };
    class TransientVector { };
    class PersistentVector { };
}

namespace arbace {
    class Repeat { };
}

namespace arbace {
    class Range { };
}

namespace arbace {
    class Reduced { };

///     (defn reduced_9_ [x] (satisfies_9_ Reduced x))
}

namespace arbace {
    class StringSeq { };
}

namespace arbace {
    class Unbound { };
    class Var { };

///     (defn var_9_ [v] (satisfies_9_ Var v))
}

// (about #_"defarray"
///     (defn aget    [a i] (A_1_get a i))
///     (defn alength [a]   (A_1_length a))

///     (defn aclone [a]         (when (some_9_ a) (A_1_clone a)))
///     (defn acopy_4_ [a i b j n] (System_1_arraycopy b, j, a, i, n) a)
///     (defn aset_4_  [a i x]     (A_1_set a i x) a)
///     (defn aswap_4_ [a i f & s] (aset_4_ a i (apply f (aget a i) s)))

///     (defn anew [size_or_seq]
///         (if (number_9_ size_or_seq)
///             (A_1_new (int_4_ size_or_seq))
///             (let [#_"ISeq" s (seq size_or_seq) #_"int" n (count s)]
///                 (loop_when_recur [#_"array" a (A_1_new n) #_"int" i 0 s s] (and (< i n) (some_9_ s)) [(aset_4_ a i (first s)) (inc i) (next s)] => a)
///             )
///         )
///     )

///     (defn_ qset_4_
///         ([a k v]    (Mutable_2_mutate_4_ a, k, v))
///         ([a k v & kvs]
///             (let [a (Mutable_2_mutate_4_ a, k, v)]
///                 (recur_when kvs [a (first kvs) (second kvs) (next (next kvs))] => a)
///             )
///         )
///     )

///     (defn_ qswap_4_
///         ([a k f]         (Mutable_2_mutate_4_ a, k,       (f (ILookup_2_valAt a, k))))
///         ([a k f x]       (Mutable_2_mutate_4_ a, k,       (f (ILookup_2_valAt a, k) x)))
///         ([a k f x y]     (Mutable_2_mutate_4_ a, k,       (f (ILookup_2_valAt a, k) x y)))
///         ([a k f x y & z] (Mutable_2_mutate_4_ a, k, (apply f (ILookup_2_valAt a, k) x y z)))
///     )
// )

// (about #_"append, str, pr, prn"
///     (def_ #_"{char String}" char_name_string
///         (hash_map
///             \newline   "newline"
///             \tab       "tab"
///             \space     "space"
///             \backspace "backspace"
///             \formfeed  "formfeed"
///             \return    "return"
///         )
///     )

///     (defn_ #_"Appendable" append_chr [#_"Appendable" a, #_"char" x]
///         (-> a (Appendable_3_append "\\") (Appendable_3_append (M_1_get char_name_string x x)))
///     )

///     (def_ #_"{char String}" char_escape_string
///         (hash_map
///             \newline   "\\n"
///             \tab       "\\t"
///             \return    "\\r"
///             \"         "\\\""
///             \\         "\\\\"
///             \formfeed  "\\f"
///             \backspace "\\b"
///         )
///     )

///     (defn_ #_"Appendable" append_str [#_"Appendable" a, #_"String" x]
///         (let [
///             a (Appendable_3_append a, "\"")
///             a (-/reduce #(Appendable_3_append %1, (M_1_get char_escape_string %2 %2)) a x)
///             a (Appendable_3_append a, "\"")
///         ]
///             a
///         )
///     )

///     (defn_ #_"Appendable" append_rex [#_"Appendable" a, #_"Pattern" x]
///         (let [
///             a (Appendable_3_append a, "#\"")
///             a
///                 (loop_when [a a [#_"char" c & #_"ISeq" r _0_as #_"ISeq" s] (seq (Pattern_2_pattern x)) q_9_ false] (some_9_ s) => a
///                     (case_4_ c
///                         \\  (let [[c & r] r] (recur (-> a (Appendable_3_append "\\") (Appendable_3_append c)) r (if q_9_ (not= c \E) (= c \Q))))
///                         \"                   (recur (-> a (Appendable_3_append (if q_9_ "\\E\\\"\\Q" "\\\""))) r q_9_)
///                                              (recur (-> a (Appendable_3_append c))                           r q_9_)
///                     )
///                 )
///             a (Appendable_3_append a, "\"")
///         ]
///             a
///         )
///     )

    class SeqForm { };
    class VecForm { };
    class MapForm { };
    class SetForm { };

///     (defn_ #_"Appendable" append_8_ [#_"Appendable" a, #_"String" b, #_"IFn" f_1_append, #_"String" c, #_"String" d, #_"Seqable" q]
///         (let [a (let_when [a (Appendable_3_append a, b) #_"ISeq" s (seq q)] (some_9_ s) => a
///                     (loop [a a s s]
///                         (let_when [a (f_1_append a (first s)) s (next s)] (some_9_ s) => a
///                             (recur (Appendable_3_append a, c) s)
///                         )
///                     )
///                 )]
///             (Appendable_3_append a, d)
///         )
///     )

///     (declare append)

///     (defn_ #_"Appendable" append_seq [#_"Appendable" a, #_"ISeq" x]    (append_8_ a "(" append " " ")" x))
///     (defn_ #_"Appendable" append_vec [#_"Appendable" a, #_"vector" x] (append_8_ a "[" append " " "]" x))
///     (defn_ #_"Appendable" append_map [#_"Appendable" a, #_"IPersistentMap" x]    (append_8_ a "{" (fn [a e] (-> a (append (key e)) (Appendable_3_append " ") (append (val e)))) ", " "}" x))
///     (defn_ #_"Appendable" append_set [#_"Appendable" a, #_"set" x]    (append_8_ a "#{" append " " "}" x))

///     (defn #_"Appendable" append [#_"Appendable" a, #_"Object" x]
///         (case_4_ x
///             nil   (Appendable_3_append a, "nil")
///             false (Appendable_3_append a, "false")
///             true  (Appendable_3_append a, "true")
///             (cond
///                 (number_9_ x) (Appendable_3_append a, (Number_2_toString x))
///                 (string_9_ x) (append_str a x)
///                 _0_else
///                 (condp satisfies_9_ x
///                     IAppend (IAppend_3_append x, a)
///                     SeqForm (append_seq a x)
///                     VecForm (append_vec a x)
///                     MapForm (append_map a x)
///                     SetForm (append_set a x)
///                     (cond
///                         (seq_9_ x)     (append_seq a x)
///                         (vector_9_ x)  (append_vec a x)
///                         (map_9_ x)     (append_map a x)
///                         (set_9_ x)     (append_set a x)
///                         (char_9_ x)    (append_chr a x)
///                         (pattern_9_ x) (append_rex a x)
///                         _0_else        (Appendable_3_append a, (Object_2_toString x))
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Appendable" append_4_ [#_"Appendable" a, #_"Object" x]
///         (if (or (char_sequence_9_ x) (char_9_ x)) (Appendable_3_append a, x) (append a x))
///     )

///     (defn #_"String" str
///         ([] "")
///         ([x] (if (some_9_ x) (-> (StringBuilder_1_new) (append_4_ x) (StringBuilder_2_toString)) ""))
///         ([x & s]
///             ((fn [#_"StringBuilder" sb s] (recur_when s [(append_4_ sb (first s)) (next s)] => (StringBuilder_2_toString sb)))
///                 (-> (StringBuilder_1_new) (append_4_ x)) s
///             )
///         )
///     )

///     (defn space   [] (Appendable_3_append std::cout \space)   nil)
///     (defn newline [] (Appendable_3_append std::cout \newline) nil)
///     (defn flush   [] (Flushable_3_flush   std::cout)          nil)

///     (defn pr
///         ([] nil)
///         ([x] (append std::cout x) nil)
///         ([x & s]
///             (pr x) (space)
///             (let_when [[x & s] s] (some_9_ s) => (pr x)
///                 (recur x s)
///             )
///         )
///     )

///     (defn print
///         ([] nil)
///         ([x] (append_4_ std::cout x) nil)
///         ([x & s]
///             (print x) (space)
///             (let_when [[x & s] s] (some_9_ s) => (print x)
///                 (recur x s)
///             )
///         )
///     )

///     (defn prn     [& s] (apply pr    s) (newline) (flush) nil)
///     (defn println [& s] (apply print s) (newline) (flush) nil)
// )

namespace arbace {

// (about #_"Murmur3"
///     (def_ #_"int" Murmur3_1_seed (int 0))
///     (def_ #_"int" Murmur3_1_C1 (int_4_ 0xcc9e2d51))
///     (def_ #_"int" Murmur3_1_C2 (int_4_ 0x1b873593))

///     (defn_ #_"int" Murmur3_1_mixK1 [#_"int" k1]
///         (-> k1 (* Murmur3_1_C1) (Integer_1_rotateLeft 15) (* Murmur3_1_C2))
///     )

///     (defn_ #_"int" Murmur3_1_mixH1 [#_"int" h1, #_"int" k1]
///         (-> h1 (bit_xor k1) (Integer_1_rotateLeft 13) (* (int 5)) (+ (int_4_ 0xe6546b64)))
///     )

///     (defn_ #_"int" Murmur3_1_fmix [#_"int" h1, #_"int" n]
///         (let [h1 (bit_xor h1 n)    h1 (bit_xor h1 (>>> h1 16))
///               h1 (* (int_4_ h1) (int_4_ 0x85ebca6b)) h1 (bit_xor h1 (>>> h1 13))
///               h1 (* (int_4_ h1) (int_4_ 0xc2b2ae35)) h1 (bit_xor h1 (>>> h1 16))]
///             h1
///         )
///     )

///     (defn #_"int" Murmur3_1_hashInt [#_"int" input]
///         (when_not (zero_9_ input) => (int 0)
///             (let [#_"int" k1 (Murmur3_1_mixK1 input)
///                   #_"int" h1 (Murmur3_1_mixH1 Murmur3_1_seed, k1)]
///                 (Murmur3_1_fmix h1, (int 4))
///             )
///         )
///     )

///     (defn #_"int" Murmur3_1_hashLong [#_"long" input]
///         (when_not (zero_9_ input) => (int 0)
///             (let [#_"int" low (int_4_ input)
///                   #_"int" high (int_4_ (>>> input 32))
///                   #_"int" k1 (Murmur3_1_mixK1 low)
///                   #_"int" h1 (Murmur3_1_mixH1 Murmur3_1_seed, k1)
///                   k1 (Murmur3_1_mixK1 high)
///                   h1 (Murmur3_1_mixH1 h1, k1)]
///                 (Murmur3_1_fmix h1, (int 8))
///             )
///         )
///     )

///     (declare odd_9_)

///     (defn #_"int" Murmur3_1_hashUnencodedChars [#_"CharSequence" s]
///         (let [#_"int" h1
///                 (loop_when [h1 Murmur3_1_seed #_"int" i 1] (< i (CharSequence_3_length s)) => h1
///                     (let [#_"int" k1 (bit_or (int (CharSequence_3_charAt s, (dec i))) (<< (int (CharSequence_3_charAt s, i)) 16))]
///                         (recur (Murmur3_1_mixH1 h1, (Murmur3_1_mixK1 k1)) (+ i 2))
///                     )
///                 )
///               h1
///                 (when (odd_9_ (CharSequence_3_length s)) => h1
///                     (let [#_"int" k1 (int (CharSequence_3_charAt s, (dec (CharSequence_3_length s))))]
///                         (bit_xor h1 (Murmur3_1_mixK1 k1))
///                     )
///                 )]
///             (Murmur3_1_fmix h1, (<< (CharSequence_3_length s) 1))
///         )
///     )

///     (defn #_"int" Murmur3_1_mixCollHash [#_"int" hash, #_"int" n]
///         (Murmur3_1_fmix (Murmur3_1_mixH1 Murmur3_1_seed, (Murmur3_1_mixK1 hash)), n)
///     )

///     (defn #_"int" Murmur3_1_hashOrdered [#_"Seqable" items]
///         (loop_when_recur [#_"int" hash (int 1) #_"int" n (int 0) #_"ISeq" s (seq items)]
///                          (some_9_ s)
///                          [(+ (* (int 31) hash) (f_1_hash (first s))) (inc n) (next s)]
///                       => (Murmur3_1_mixCollHash hash, n)
///         )
///     )

///     (defn #_"int" Murmur3_1_hashUnordered [#_"Seqable" items]
///         (loop_when_recur [#_"int" hash (int 0) #_"int" n (int 0) #_"ISeq" s (seq items)]
///                          (some_9_ s)
///                          [(+ hash (f_1_hash (first s))) (inc n) (next s)]
///                       => (Murmur3_1_mixCollHash hash, n)
///         )
///     )
// )

/// (defn #_"long" mix_collection_hash [#_"long" hash_basis #_"long" n] (Murmur3_1_mixCollHash hash_basis n))

/// (defn #_"long" hash_ordered_coll [s] (Murmur3_1_hashOrdered s))

/// (defn #_"long" hash_unordered_coll [s] (Murmur3_1_hashUnordered s))
}

namespace arbace {

// (about #_"Atom"
///     (declare Atom_2_deref)

///     (defq Atom [#_"AtomicReference" meta, #_"AtomicReference" data]
///         java.util.concurrent.Future (get [_] (Atom_2_deref _))
///     )

///     (defn #_"Atom" Atom_1_new
///         ([#_"Object" data] (Atom_1_new nil, data))
///         ([#_"IPersistentMap" meta, #_"Object" data]
///             (new_8_ Atom_1_class (anew [(AtomicReference_1_new meta), (AtomicReference_1_new data)]))
///         )
///     )

///     (defn_ #_"IPersistentMap" Atom_2_meta [#_"Atom" this]
///         (AtomicReference_2_get (_0_meta this))
///     )

///     (defn_ #_"IPersistentMap" Atom_2_alterMeta [#_"Atom" this, #_"IFn" f, #_"ISeq" args]
///         (loop []
///             (let [#_"IPersistentMap" m (AtomicReference_2_get (_0_meta this)) #_"IPersistentMap" m_1_ (apply f m args)]
///                 (when (AtomicReference_2_compareAndSet (_0_meta this), m, m_1_) => (recur)
///                     m_1_
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentMap" Atom_2_resetMeta [#_"Atom" this, #_"IPersistentMap" m_1_]
///         (AtomicReference_2_set (_0_meta this), m_1_)
///         m_1_
///     )

///     (defn_ #_"Object" Atom_2_deref [#_"Atom" this]
///         (AtomicReference_2_get (_0_data this))
///     )

///     (defn_ #_"bool" Atom_2_compareAndSet [#_"Atom" this, #_"Object" o, #_"Object" o_1_]
///         (AtomicReference_2_compareAndSet (_0_data this), o, o_1_)
///     )

///     (defn_ #_"Object" Atom_2_swap [#_"Atom" this, #_"IFn" f, #_"ISeq" args]
///         (loop []
///             (let [#_"Object" o (AtomicReference_2_get (_0_data this)) #_"Object" o_1_ (apply f o args)]
///                 (when (AtomicReference_2_compareAndSet (_0_data this), o, o_1_) => (recur)
///                     o_1_
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" Atom_2_reset [#_"Atom" this, #_"Object" o_1_]
///         (AtomicReference_2_set (_0_data this), o_1_)
///         o_1_
///     )

///     (defn_ #_"[Object Object]" Atom_2_swapVals [#_"Atom" this, #_"IFn" f, #_"ISeq" args]
///         (loop []
///             (let [#_"Object" o (AtomicReference_2_get (_0_data this)) #_"Object" o_1_ (apply f o args)]
///                 (when (AtomicReference_2_compareAndSet (_0_data this), o, o_1_) => (recur)
///                     [o o_1_]
///                 )
///             )
///         )
///     )

///     (defn_ #_"[Object Object]" Atom_2_resetVals [#_"Atom" this, #_"Object" o_1_]
///         (loop []
///             (let [#_"Object" o (AtomicReference_2_get (_0_data this))]
///                 (when (AtomicReference_2_compareAndSet (_0_data this), o, o_1_) => (recur)
///                     [o o_1_]
///                 )
///             )
///         )
///     )

///     (defm Atom IMeta
///         (IMeta_3_meta => Atom_2_meta)
///     )

///     (defm Atom IReference
///         (IReference_3_alterMeta => Atom_2_alterMeta)
///         (IReference_3_resetMeta => Atom_2_resetMeta)
///     )

///     (defm Atom IDeref
///         (IDeref_3_deref => Atom_2_deref)
///     )

///     (defm Atom IAtom
///         (IAtom_3_compareAndSet => Atom_2_compareAndSet)
///         (IAtom_3_swap => Atom_2_swap)
///         (IAtom_3_reset => Atom_2_reset)
///         (IAtom_3_swapVals => Atom_2_swapVals)
///         (IAtom_3_resetVals => Atom_2_resetVals)
///     )
// )

/// (defn atom
///     ([x] (Atom_1_new x))
///     ([m x] (Atom_1_new m x))
/// )

/// (defn compare_and_set_4_ [#_"IAtom" a x x_1_] (IAtom_3_compareAndSet a, x, x_1_))

/// (defn swap_4_ [#_"IAtom" a f & args] (IAtom_3_swap a, f, args))

/// (defn reset_4_ [#_"IAtom" a x_1_] (IAtom_3_reset a, x_1_))

/// (defn #_"vector" swap_vals_4_ [#_"IAtom" a f & args] (IAtom_3_swapVals a, f, args))

/// (defn #_"vector" reset_vals_4_ [#_"IAtom" a x_1_] (IAtom_3_resetVals a, x_1_))
}

namespace arbace {

// (about #_"Delay"
///     (defq Delay [#_"fn'" f, #_"Object'" o, #_"Throwable'" e])

///     (defn #_"Delay" Delay_1_new [#_"IFn" f]
///         (new_8_ Delay_1_class (anew [(atom f), (atom nil), (atom nil)]))
///     )

///     (defn #_"Object" Delay_1_force [#_"Object" x]
///         (if (satisfies_9_ Delay x) (deref x) x)
///     )

///     (defn_ #_"Object" Delay_2_deref [#_"Delay" this]
///         (when (some_9_ (deref (_0_f this)))
///             (locking this
///                 (when_some [#_"IFn" f (deref (_0_f this))]
///                     (reset_4_ (_0_f this) nil)
///                     (try
///                         (reset_4_ (_0_o this) (f))
///                         (catch java.lang.Throwable t
///                             (reset_4_ (_0_e this) t)
///                         )
///                     )
///                 )
///             )
///         )
///         (when_some [#_"Throwable" e (deref (_0_e this))]
///             (throw e)
///         )
///         (deref (_0_o this))
///     )

///     (defn_ #_"bool" Delay_2_isRealized [#_"Delay" this]
///         (locking this
///             (nil_9_ (deref (_0_f this)))
///         )
///     )

///     (defm Delay IDeref
///         (IDeref_3_deref => Delay_2_deref)
///     )

///     (defm Delay IPending
///         (IPending_3_isRealized => Delay_2_isRealized)
///     )
// )

/// (defmacro delay [& body] `(Delay_1_new (fn_8_ [] ~@body)))

/// (defn delay_9_ [x] (satisfies_9_ Delay x))

/// (defn force [x] (Delay_1_force x))
}

namespace arbace {

// (about #_"Reduced"
///     (defq Reduced [#_"Object" val])

///     (defn #_"Reduced" Reduced_1_new [#_"Object" val]
///         (new_8_ Reduced_1_class (anew [val]))
///     )

///     (defm Reduced IDeref
///         (IDeref_3_deref => _0_val)
///     )
// )

/// (defn reduced [x] (Reduced_1_new x))

/// (defn ensure_reduced [x] (if (reduced_9_ x) x (reduced x)))

/// (defn unreduced [x] (if (reduced_9_ x) (deref x) x))

/// (defn_ preserving_reduced [f] #(let [r (f %1 %2)] (if (reduced_9_ r) (reduced r) r)))

// naïve reduce to be redefined later with IReduce

/// (defn reduce
///     ([f s] (if_some [s (seq s)] (reduce f (first s) (next s)) (f)))
///     ([f r s] (if_some [s (seq s)] (recur f (f r (first s)) (next s)) r))
/// )

/// (defn reduce_4_
///     ([f s] (if_some [s (seq s)] (reduce_4_ f (first s) (next s)) (f)))
///     ([f r s] (persistent_4_ (reduce f (transient r) s)))
/// )

/// (defn cat [f]
///     (let [g (preserving_reduced f)]
///         (fn
///             ([] (f))
///             ([s] (f s))
///             ([s x] (reduce g s x))
///         )
///     )
/// )

/// (defn into [to from]
///     (if (editable_9_ to)
///         (reduce_4_ conj_4_ to from)
///         (reduce conj to from)
///     )
/// )

/// (defn mapv
///     ([f coll] (reduce_4_ #(conj_4_ %1 (f %2)) (vector) coll))
///     ([f c1 c2] (into (vector) (map f c1 c2)))
///     ([f c1 c2 c3] (into (vector) (map f c1 c2 c3)))
///     ([f c1 c2 c3 & colls] (into (vector) (apply map f c1 c2 c3 colls)))
/// )

/// (defn filterv [f_9_ s] (reduce_4_ #(if (f_9_ %2) (conj_4_ %1 %2) %1) (vector) s))
}

namespace arbace {

// (about #_"Util"
///     (declare Symbol_2_equals)
///     (declare Keyword_2_equals)

///     (defn #_"bool" Util_1_equiv [#_"Object" a, #_"Object" b]
///         (cond
///             (identical_9_ a b)              true
///             (nil_9_ a)                      false
///             (and (number_9_ a) (number_9_ b)) #_(Numbers_1_equal a, b) (-'== a b)
///             (coll_9_ a)                     (IObject_3_equals a, b)
///             (coll_9_ b)                     (IObject_3_equals b, a)
///             _0_else                         (IObject_3_equals a, b)
///         )
///     )
// )

/// #_oops!
/// (defn =
///     ([x] true)
///     ([x y] (Util_1_equiv x y))
///     ([x y & s] (and (= x y) (recur_when (next s) [y (first s) (next s)] => (= y (first s)))))
/// )

/// (defn not=
///     ([x] false)
///     ([x y] (not (= x y)))
///     ([x y & s] (not (apply = x y s)))
/// )

// (about #_"Util"
///     (declare Numbers_1_compare)

///     (defn #_"int" Util_1_compare [#_"Object" a, #_"Object" b]
///         (cond
///             (= a b)     0
///             (nil_9_ a)   -1
///             (nil_9_ b)    1
///             (number_9_ a) #_(Numbers_1_compare a, #_"Number" b) (-'compare a b)
///             _0_else       (Comparable_3_compareTo a, b)
///         )
///     )
// )

/// (defn compare [x y] (Util_1_compare x, y))
}

namespace arbace {

// (about #_"Ratio"
///     (declare Ratio_2_hashcode)

///     (defq Ratio [#_"BigInteger" n, #_"BigInteger" d]
///         java.lang.Object (hashCode [_] (Ratio_2_hashcode _))
///     )

///     (§ inherit Ratio #_"Number")

///     (defn #_"Ratio" Ratio_1_new [#_"BigInteger" numerator, #_"BigInteger" denominator]
///         (new_8_ Ratio_1_class (anew [numerator, denominator]))
///     )

///     (defn #_"BigInteger" Ratio_2_bigIntegerValue [#_"Ratio" this]
///         (BigInteger_2_divide (_0_n this), (_0_d this))
///     )

///     (defn #_"long" Ratio_2_longValue [#_"Ratio" this]
///         (BigInteger_2_longValue (Ratio_2_bigIntegerValue this))
///     )

///     (defn #_"int" Ratio_2_intValue [#_"Ratio" this]
///         (BigInteger_2_intValue (Ratio_2_bigIntegerValue this))
///     )

///     (defn_ #_"int" Ratio_2_hashcode [#_"Ratio" this]
///         (bit_xor (Object_2_hashCode (_0_n this)) (Object_2_hashCode (_0_d this)))
///     )

///     (defn_ #_"bool" Ratio_2_equals [#_"Ratio" this, #_"Object" that]
///         (and (satisfies_9_ Ratio that) (= (_0_n that) (_0_n this)) (= (_0_d that) (_0_d this)))
///     )

///     (defn_ #_"Appendable" Ratio_2_append [#_"Ratio" this, #_"Appendable" a]
///         (-> a (Appendable_3_append (BigInteger_2_toString (_0_n this))) (Appendable_3_append "/") (Appendable_3_append (BigInteger_2_toString (_0_d this))))
///     )

///     (defn_ #_"int" Ratio_2_compareTo [#_"Ratio" this, #_"Number" that]
///         #_(Numbers_1_compare this, that) (-'compare this that)
///     )

///     (defm Ratio Hashed
///         (Hashed_3_hash => Ratio_2_hashcode)
///     )

///     (defm Ratio IObject
///         (IObject_3_equals => Ratio_2_equals)
///     )

///     (defm Ratio IAppend
///         (IAppend_3_append => Ratio_2_append)
///     )

///     (defm Ratio Comparable
///         (Comparable_3_compareTo => Ratio_2_compareTo)
///     )
// )

/// (defn #_"BigInteger" biginteger [x]
///     (cond
///         (biginteger_9_ x) x
///         (ratio_9_ x)      (Ratio_2_bigIntegerValue #_"Ratio" x)
///         (number_9_ x)     (BigInteger_1_valueOf (long x))
///         _0_else           (BigInteger_1_new #_"String|byte[]" x)
///     )
/// )
}

namespace arbace {

// (about #_"LongOps"
///     (defq LongOps [])

///     (defn #_"LongOps" LongOps_1_new []
///         (new_8_ LongOps_1_class (anew []))
///     )

///     (defn #_"long" LongOps_1_gcd [#_"long" u, #_"long" v] (if (-'= v 0) u (recur v (-'rem u v))))

///     (declare Numbers_1_RATIO_OPS)
///     (declare Numbers_1_BIGINT_OPS)

///     (defn_ #_"Ops" LongOps_2_combine [#_"LongOps" this, #_"Ops" y] (Ops_3_opsWithLong y, this))

///     (defn_ #_"Ops" LongOps_2_opsWithLong [#_"LongOps" this, #_"LongOps" x] this)
///     (defn_ #_"Ops" LongOps_2_opsWithRatio [#_"LongOps" this, #_"RatioOps" x] Numbers_1_RATIO_OPS)
///     (defn_ #_"Ops" LongOps_2_opsWithBigInt [#_"LongOps" this, #_"BigIntOps" x] Numbers_1_BIGINT_OPS)

///     (defn_ #_"bool" LongOps_2_eq [#_"LongOps" this, #_"Number" x, #_"Number" y] (-'= (Number_2_longValue x) (Number_2_longValue y)))
///     (defn_ #_"bool" LongOps_2_lt [#_"LongOps" this, #_"Number" x, #_"Number" y] (-'< (Number_2_longValue x) (Number_2_longValue y)))
///     (defn_ #_"bool" LongOps_2_lte [#_"LongOps" this, #_"Number" x, #_"Number" y] (-'<= (Number_2_longValue x) (Number_2_longValue y)))

///     (defn_ #_"bool" LongOps_2_isZero [#_"LongOps" this, #_"Number" x] (-'= (Number_2_longValue x) 0))
///     (defn_ #_"bool" LongOps_2_isPos [#_"LongOps" this, #_"Number" x] (-'> (Number_2_longValue x) 0))
///     (defn_ #_"bool" LongOps_2_isNeg [#_"LongOps" this, #_"Number" x] (-'< (Number_2_longValue x) 0))

///     (defn_ #_"Number" LongOps_2_add [#_"LongOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"long" lx (Number_2_longValue x) #_"long" ly (Number_2_longValue y) #_"long" lz (-'+ lx ly)]
///             (when (and (-'< (-'bit_xor lz lx) 0) (-'< (-'bit_xor lz ly) 0)) => (Long_1_valueOf lz)
///                 (Ops_3_add Numbers_1_BIGINT_OPS, x, y)
///             )
///         )
///     )

///     (defn_ #_"Number" LongOps_2_negate [#_"LongOps" this, #_"Number" x]
///         (let [#_"long" lx (Number_2_longValue x)]
///             (when (-'= lx Long::MIN_VALUE) => (Long_1_valueOf (-'- lx))
///                 (BigInteger_2_negate (BigInteger_1_valueOf lx))
///             )
///         )
///     )

///     (defn_ #_"Number" LongOps_2_inc [#_"LongOps" this, #_"Number" x]
///         (let [#_"long" lx (Number_2_longValue x)]
///             (when (-'= lx Long::MAX_VALUE) => (Long_1_valueOf (-'+ lx 1))
///                 (Ops_3_inc Numbers_1_BIGINT_OPS, x)
///             )
///         )
///     )

///     (defn_ #_"Number" LongOps_2_dec [#_"LongOps" this, #_"Number" x]
///         (let [#_"long" lx (Number_2_longValue x)]
///             (when (-'= lx Long::MIN_VALUE) => (Long_1_valueOf (-'- lx 1))
///                 (Ops_3_dec Numbers_1_BIGINT_OPS, x)
///             )
///         )
///     )

///     (defn_ #_"Number" LongOps_2_multiply [#_"LongOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"long" lx (Number_2_longValue x) #_"long" ly (Number_2_longValue y)]
///             (when_not (and (-'= lx Long::MIN_VALUE) (-'< ly 0)) => (Ops_3_multiply Numbers_1_BIGINT_OPS, x, y)
///                 (let [#_"long" lz (-'* lx ly)]
///                     (when (or (-'= ly 0) (-'= (-'quot lz ly) lx)) => (Ops_3_multiply Numbers_1_BIGINT_OPS, x, y)
///                         (Long_1_valueOf lz)
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Number" LongOps_2_divide [#_"LongOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"long" lx (Number_2_longValue x) #_"long" ly (Number_2_longValue y)]
///             (let_when_not [#_"long" gcd (LongOps_1_gcd lx, ly)] (-'= gcd 0) => (Long_1_valueOf 0)
///                 (let_when_not [lx (-'quot lx gcd) ly (-'quot ly gcd)] (-'= ly 1) => (Long_1_valueOf lx)
///                     (let [[lx ly]
///                             (when (-'< ly 0) => [lx ly]
///                                 [(-'- lx) (-'- ly)]
///                             )]
///                         (Ratio_1_new (BigInteger_1_valueOf lx), (BigInteger_1_valueOf ly))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Number" LongOps_2_quotient [#_"LongOps" this, #_"Number" x, #_"Number" y] (Long_1_valueOf (-'quot (Number_2_longValue x) (Number_2_longValue y))))
///     (defn_ #_"Number" LongOps_2_remainder [#_"LongOps" this, #_"Number" x, #_"Number" y] (Long_1_valueOf (-'rem (Number_2_longValue x) (Number_2_longValue y))))

///     (defm LongOps Ops
///         (Ops_3_combine => LongOps_2_combine)
///         (Ops_3_opsWithLong => LongOps_2_opsWithLong)
///         (Ops_3_opsWithRatio => LongOps_2_opsWithRatio)
///         (Ops_3_opsWithBigInt => LongOps_2_opsWithBigInt)
///         (Ops_3_eq => LongOps_2_eq)
///         (Ops_3_lt => LongOps_2_lt)
///         (Ops_3_lte => LongOps_2_lte)
///         (Ops_3_isZero => LongOps_2_isZero)
///         (Ops_3_isPos => LongOps_2_isPos)
///         (Ops_3_isNeg => LongOps_2_isNeg)
///         (Ops_3_add => LongOps_2_add)
///         (Ops_3_negate => LongOps_2_negate)
///         (Ops_3_inc => LongOps_2_inc)
///         (Ops_3_dec => LongOps_2_dec)
///         (Ops_3_multiply => LongOps_2_multiply)
///         (Ops_3_divide => LongOps_2_divide)
///         (Ops_3_quotient => LongOps_2_quotient)
///         (Ops_3_remainder => LongOps_2_remainder)
///     )
// )

// (about #_"RatioOps"
///     (defq RatioOps [])

///     (defn #_"RatioOps" RatioOps_1_new []
///         (new_8_ RatioOps_1_class (anew []))
///     )

///     (declare Numbers_1_toRatio)
///     (declare Numbers_1_divide)
///     (declare Numbers_1_subtract)
///     (declare Numbers_1_multiply)
///     (declare Numbers_1_lt)
///     (declare Numbers_1_lte)
///     (declare Numbers_1_gte)

///     (defn_ #_"Ops" RatioOps_2_combine [#_"RatioOps" this, #_"Ops" y] (Ops_3_opsWithRatio y, this))

///     (defn_ #_"Ops" RatioOps_2_opsWithLong [#_"RatioOps" this, #_"LongOps" x] this)
///     (defn_ #_"Ops" RatioOps_2_opsWithRatio [#_"RatioOps" this, #_"RatioOps" x] this)
///     (defn_ #_"Ops" RatioOps_2_opsWithBigInt [#_"RatioOps" this, #_"BigIntOps" x] this)

///     (defn_ #_"bool" RatioOps_2_eq [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (and (-'= (_0_n rx) (_0_n ry)) (-'= (_0_d rx) (_0_d ry)))
///         )
///     )

///     (defn_ #_"bool" RatioOps_2_lt [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (Numbers_1_lt (BigInteger_2_multiply (_0_n rx), (_0_d ry)), (BigInteger_2_multiply (_0_n ry), (_0_d rx)))
///         )
///     )

///     (defn_ #_"bool" RatioOps_2_lte [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (Numbers_1_lte (BigInteger_2_multiply (_0_n rx), (_0_d ry)), (BigInteger_2_multiply (_0_n ry), (_0_d rx)))
///         )
///     )

///     (defn_ #_"bool" RatioOps_2_isZero [#_"RatioOps" this, #_"Number" x] (-'= (BigInteger_2_signum (_0_n #_"Ratio" x)) 0))
///     (defn_ #_"bool" RatioOps_2_isPos [#_"RatioOps" this, #_"Number" x] (-'> (BigInteger_2_signum (_0_n #_"Ratio" x)) 0))
///     (defn_ #_"bool" RatioOps_2_isNeg [#_"RatioOps" this, #_"Number" x] (-'< (BigInteger_2_signum (_0_n #_"Ratio" x)) 0))

///     (defn_ #_"Number" RatioOps_2_add [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (Ops_3_divide this, (BigInteger_2_add (BigInteger_2_multiply (_0_n ry), (_0_d rx)), (BigInteger_2_multiply (_0_n rx), (_0_d ry))), (BigInteger_2_multiply (_0_d ry), (_0_d rx)))
///         )
///     )

///     (defn_ #_"Number" RatioOps_2_negate [#_"RatioOps" this, #_"Number" x]
///         (let [#_"Ratio" r (Numbers_1_toRatio x)]
///             (Ratio_1_new (BigInteger_2_negate (_0_n r)), (_0_d r))
///         )
///     )

///     (defn_ #_"Number" RatioOps_2_inc [#_"RatioOps" this, #_"Number" x] (Ops_3_add this, x, 1))
///     (defn_ #_"Number" RatioOps_2_dec [#_"RatioOps" this, #_"Number" x] (Ops_3_add this, x, -1))

///     (defn_ #_"Number" RatioOps_2_multiply [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (Numbers_1_divide (BigInteger_2_multiply (_0_n ry), (_0_n rx)), (BigInteger_2_multiply (_0_d ry), (_0_d rx)))
///         )
///     )

///     (defn_ #_"Number" RatioOps_2_divide [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (Numbers_1_divide (BigInteger_2_multiply (_0_d ry), (_0_n rx)), (BigInteger_2_multiply (_0_n ry), (_0_d rx)))
///         )
///     )

///     (defn_ #_"Number" RatioOps_2_quotient [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"Ratio" rx (Numbers_1_toRatio x) #_"Ratio" ry (Numbers_1_toRatio y)]
///             (BigInteger_2_divide (BigInteger_2_multiply (_0_n rx), (_0_d ry)), (BigInteger_2_multiply (_0_d rx), (_0_n ry)))
///         )
///     )

///     (defn_ #_"Number" RatioOps_2_remainder [#_"RatioOps" this, #_"Number" x, #_"Number" y]
///         (Numbers_1_subtract x, (Numbers_1_multiply (Ops_3_quotient this, x, y), y))
///     )

///     (defm RatioOps Ops
///         (Ops_3_combine => RatioOps_2_combine)
///         (Ops_3_opsWithLong => RatioOps_2_opsWithLong)
///         (Ops_3_opsWithRatio => RatioOps_2_opsWithRatio)
///         (Ops_3_opsWithBigInt => RatioOps_2_opsWithBigInt)
///         (Ops_3_eq => RatioOps_2_eq)
///         (Ops_3_lt => RatioOps_2_lt)
///         (Ops_3_lte => RatioOps_2_lte)
///         (Ops_3_isZero => RatioOps_2_isZero)
///         (Ops_3_isPos => RatioOps_2_isPos)
///         (Ops_3_isNeg => RatioOps_2_isNeg)
///         (Ops_3_add => RatioOps_2_add)
///         (Ops_3_negate => RatioOps_2_negate)
///         (Ops_3_inc => RatioOps_2_inc)
///         (Ops_3_dec => RatioOps_2_dec)
///         (Ops_3_multiply => RatioOps_2_multiply)
///         (Ops_3_divide => RatioOps_2_divide)
///         (Ops_3_quotient => RatioOps_2_quotient)
///         (Ops_3_remainder => RatioOps_2_remainder)
///     )
// )

// (about #_"BigIntOps"
///     (defq BigIntOps [])

///     (defn #_"BigIntOps" BigIntOps_1_new []
///         (new_8_ BigIntOps_1_class (anew []))
///     )

///     (declare Numbers_1_toBigInteger)

///     (defn_ #_"Ops" BigIntOps_2_combine [#_"BigIntOps" this, #_"Ops" y] (Ops_3_opsWithBigInt y, this))

///     (defn_ #_"Ops" BigIntOps_2_opsWithLong [#_"BigIntOps" this, #_"LongOps" x] this)
///     (defn_ #_"Ops" BigIntOps_2_opsWithRatio [#_"BigIntOps" this, #_"RatioOps" x] Numbers_1_RATIO_OPS)
///     (defn_ #_"Ops" BigIntOps_2_opsWithBigInt [#_"BigIntOps" this, #_"BigIntOps" x] this)

///     (defn_ #_"bool" BigIntOps_2_eq [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (-'= (Numbers_1_toBigInteger x) (Numbers_1_toBigInteger y))
///     )

///     (defn_ #_"bool" BigIntOps_2_lt [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (-'< (Comparable_3_compareTo (Numbers_1_toBigInteger x), (Numbers_1_toBigInteger y)) 0)
///     )

///     (defn_ #_"bool" BigIntOps_2_lte [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (-'<= (Comparable_3_compareTo (Numbers_1_toBigInteger x), (Numbers_1_toBigInteger y)) 0)
///     )

///     (defn_ #_"bool" BigIntOps_2_isZero [#_"BigIntOps" this, #_"Number" x] (-'= (BigInteger_2_signum (Numbers_1_toBigInteger x)) 0))
///     (defn_ #_"bool" BigIntOps_2_isPos [#_"BigIntOps" this, #_"Number" x] (-'> (BigInteger_2_signum (Numbers_1_toBigInteger x)) 0))
///     (defn_ #_"bool" BigIntOps_2_isNeg [#_"BigIntOps" this, #_"Number" x] (-'< (BigInteger_2_signum (Numbers_1_toBigInteger x)) 0))

///     (defn_ #_"Number" BigIntOps_2_add [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (BigInteger_2_add (Numbers_1_toBigInteger x), (Numbers_1_toBigInteger y))
///     )

///     (defn_ #_"Number" BigIntOps_2_negate [#_"BigIntOps" this, #_"Number" x] (BigInteger_2_negate (Numbers_1_toBigInteger x)))

///     (defn_ #_"Number" BigIntOps_2_inc [#_"BigIntOps" this, #_"Number" x] (BigInteger_2_add (Numbers_1_toBigInteger x), BigInteger_1_ONE))
///     (defn_ #_"Number" BigIntOps_2_dec [#_"BigIntOps" this, #_"Number" x] (BigInteger_2_subtract (Numbers_1_toBigInteger x), BigInteger_1_ONE))

///     (defn_ #_"Number" BigIntOps_2_multiply [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (BigInteger_2_multiply (Numbers_1_toBigInteger x), (Numbers_1_toBigInteger y))
///     )

///     (defn_ #_"Number" BigIntOps_2_divide [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (let [#_"BigInteger" n (Numbers_1_toBigInteger x) #_"BigInteger" d (Numbers_1_toBigInteger y)]
///             (when_not (-'= d BigInteger_1_ZERO) => (throw "divide by zero")
///                 (let [#_"BigInteger" gcd (BigInteger_2_gcd n, d)]
///                     (when_not (-'= gcd BigInteger_1_ZERO) => BigInteger_1_ZERO
///                         (let [n (BigInteger_2_divide n, gcd) d (BigInteger_2_divide d, gcd)]
///                             (condp -'= d
///                                 BigInteger_1_ONE           n
///                                 (BigInteger_2_negate BigInteger_1_ONE) (BigInteger_2_negate n)
///                                                             (Ratio_1_new (if (-'< (BigInteger_2_signum d) 0) (BigInteger_2_negate n) n), (if (-'< (BigInteger_2_signum d) 0) (BigInteger_2_negate d) d))
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Number" BigIntOps_2_quotient [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (BigInteger_2_divide (Numbers_1_toBigInteger x), (Numbers_1_toBigInteger y))
///     )

///     (defn_ #_"Number" BigIntOps_2_remainder [#_"BigIntOps" this, #_"Number" x, #_"Number" y]
///         (BigInteger_2_remainder (Numbers_1_toBigInteger x), (Numbers_1_toBigInteger y))
///     )

///     (defm BigIntOps Ops
///         (Ops_3_combine => BigIntOps_2_combine)
///         (Ops_3_opsWithLong => BigIntOps_2_opsWithLong)
///         (Ops_3_opsWithRatio => BigIntOps_2_opsWithRatio)
///         (Ops_3_opsWithBigInt => BigIntOps_2_opsWithBigInt)
///         (Ops_3_eq => BigIntOps_2_eq)
///         (Ops_3_lt => BigIntOps_2_lt)
///         (Ops_3_lte => BigIntOps_2_lte)
///         (Ops_3_isZero => BigIntOps_2_isZero)
///         (Ops_3_isPos => BigIntOps_2_isPos)
///         (Ops_3_isNeg => BigIntOps_2_isNeg)
///         (Ops_3_add => BigIntOps_2_add)
///         (Ops_3_negate => BigIntOps_2_negate)
///         (Ops_3_inc => BigIntOps_2_inc)
///         (Ops_3_dec => BigIntOps_2_dec)
///         (Ops_3_multiply => BigIntOps_2_multiply)
///         (Ops_3_divide => BigIntOps_2_divide)
///         (Ops_3_quotient => BigIntOps_2_quotient)
///         (Ops_3_remainder => BigIntOps_2_remainder)
///     )
// )

// (about #_"Numbers"
///     (def #_"LongOps"   Numbers_1_LONG_OPS   (LongOps_1_new))
///     (def #_"RatioOps"  Numbers_1_RATIO_OPS  (RatioOps_1_new))
///     (def #_"BigIntOps" Numbers_1_BIGINT_OPS (BigIntOps_1_new))

///     (defn #_"Ops" Numbers_1_ops [#_"Number" x]
///         (cond
///             (biginteger_9_ x) Numbers_1_BIGINT_OPS
///             (ratio_9_ x)      Numbers_1_RATIO_OPS
///             _0_else           Numbers_1_LONG_OPS
///         )
///     )

///     (defn #_"int" Numbers_1_compare [#_"Number" x, #_"Number" y]
///         (let [#_"Ops" ops (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y))]
///             (cond (Ops_3_lt ops, x, y) -1 (Ops_3_lt ops, y, x) 1 _0_else 0)
///         )
///     )

///     (defn #_"bool" Numbers_1_equal [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_eq x, y))
///     )

///     (defn #_"bool" Numbers_1_lt [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_lt x, y))
///     )

///     (defn #_"bool" Numbers_1_lte [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_lte x, y))
///     )

///     (defn #_"bool" Numbers_1_gt [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_lt y, x))
///     )

///     (defn #_"bool" Numbers_1_gte [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_lte y, x))
///     )

///     (defn #_"bool" Numbers_1_isZero [#_"Number" x] (Ops_3_isZero (Numbers_1_ops x), x))
///     (defn #_"bool" Numbers_1_isPos  [#_"Number" x] (Ops_3_isPos  (Numbers_1_ops x), x))
///     (defn #_"bool" Numbers_1_isNeg  [#_"Number" x] (Ops_3_isNeg  (Numbers_1_ops x), x))

///     (defn #_"Number" Numbers_1_add [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_add x, y))
///     )

///     (defn #_"Number" Numbers_1_subtract [#_"Number" x, #_"Number" y]
///         (let [#_"Number" negativeY (Ops_3_negate (Numbers_1_ops y), y)]
///             (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops negativeY)) (Ops_3_add x, negativeY))
///         )
///     )

///     (defn #_"Number" Numbers_1_negate [#_"Number" x] (Ops_3_negate (Numbers_1_ops x), x))
///     (defn #_"Number" Numbers_1_inc    [#_"Number" x] (Ops_3_inc    (Numbers_1_ops x), x))
///     (defn #_"Number" Numbers_1_dec    [#_"Number" x] (Ops_3_dec    (Numbers_1_ops x), x))

///     (defn #_"Number" Numbers_1_multiply [#_"Number" x, #_"Number" y]
///         (-> (Ops_3_combine (Numbers_1_ops x), (Numbers_1_ops y)) (Ops_3_multiply x, y))
///     )

///     (defn #_"Number" Numbers_1_divide [#_"Number" x, #_"Number" y]
///         (let_when_not [#_"Ops" yops (Numbers_1_ops y)] (Ops_3_isZero yops, y) => (throw "divide by zero")
///             (-> (Ops_3_combine (Numbers_1_ops x), yops) (Ops_3_divide x, y))
///         )
///     )

///     (defn #_"Number" Numbers_1_quotient [#_"Number" x, #_"Number" y]
///         (let_when_not [#_"Ops" yops (Numbers_1_ops y)] (Ops_3_isZero yops, y) => (throw "divide by zero")
///             (-> (Ops_3_combine (Numbers_1_ops x), yops) (Ops_3_quotient x, y))
///         )
///     )

///     (defn #_"Number" Numbers_1_remainder [#_"Number" x, #_"Number" y]
///         (let_when_not [#_"Ops" yops (Numbers_1_ops y)] (Ops_3_isZero yops, y) => (throw "divide by zero")
///             (-> (Ops_3_combine (Numbers_1_ops x), yops) (Ops_3_remainder x, y))
///         )
///     )

///     (defn #_"BigInteger" Numbers_1_toBigInteger [#_"Number" x]
///         (if (biginteger_9_ x) x (BigInteger_1_valueOf (Number_2_longValue x)))
///     )

///     (defn #_"Ratio" Numbers_1_toRatio [#_"Number" x]
///         (if (ratio_9_ x) x (Ratio_1_new (Numbers_1_toBigInteger x), BigInteger_1_ONE))
///     )

///     (defn_ #_"long" Numbers_1_bitOpsCast [#_"Number" x]
///         (when (or (long_9_ x) (int_9_ x) (byte_9_ x)) => (throw (str "bit operation not supported on " x))
///             (long x)
///         )
///     )

///     (defn #_"long" Numbers_1_not [#_"Number" x] (-'bit_not (Numbers_1_bitOpsCast x)))

///     (defn #_"long" Numbers_1_and [#_"Number" x, #_"Number" y] (-'bit_and (Numbers_1_bitOpsCast x) (Numbers_1_bitOpsCast y)))
///     (defn #_"long" Numbers_1_or  [#_"Number" x, #_"Number" y] (-'bit_or (Numbers_1_bitOpsCast x) (Numbers_1_bitOpsCast y)))
///     (defn #_"long" Numbers_1_xor [#_"Number" x, #_"Number" y] (-'bit_xor (Numbers_1_bitOpsCast x) (Numbers_1_bitOpsCast y)))

///     (defn #_"long" Numbers_1_andNot [#_"Number" x, #_"Number" y] (-'bit_and (Numbers_1_bitOpsCast x) (-'bit_not (Numbers_1_bitOpsCast y))))

///     (defn #_"long" Numbers_1_shiftLeft          [#_"Number" x, #_"Number" n] (-'bit_shift_left (Numbers_1_bitOpsCast x) (Numbers_1_bitOpsCast n)))
///     (defn #_"long" Numbers_1_shiftRight         [#_"Number" x, #_"Number" n] (-'bit_shift_right (Numbers_1_bitOpsCast x) (Numbers_1_bitOpsCast n)))
///     (defn #_"long" Numbers_1_unsignedShiftRight [#_"Number" x, #_"Number" n] (-'unsigned_bit_shift_right (Numbers_1_bitOpsCast x) (Numbers_1_bitOpsCast n)))

///     (defn #_"long" Numbers_1_clearBit [#_"Number" x, #_"Number" n] (-'bit_and (Numbers_1_bitOpsCast x) (-'bit_not (-'bit_shift_left 1 (Numbers_1_bitOpsCast n)))))
///     (defn #_"long" Numbers_1_setBit   [#_"Number" x, #_"Number" n] (-'bit_or (Numbers_1_bitOpsCast x) (-'bit_shift_left 1 (Numbers_1_bitOpsCast n))))
///     (defn #_"long" Numbers_1_flipBit  [#_"Number" x, #_"Number" n] (-'bit_xor (Numbers_1_bitOpsCast x) (-'bit_shift_left 1 (Numbers_1_bitOpsCast n))))

///     (defn #_"bool" Numbers_1_testBit [#_"Number" x, #_"Number" n] (-/not= (-'bit_and (Numbers_1_bitOpsCast x) (-'bit_shift_left 1 (Numbers_1_bitOpsCast n))) 0))
// )

/// (§ defn <
///     ([x] true)
///     ([x y] (Numbers_1_lt x y))
///     ([x y & s] (and (< x y) (recur_when (next s) [y (first s) (next s)] => (< y (first s)))))
/// )

/// (§ defn <=
///     ([x] true)
///     ([x y] (Numbers_1_lte x y))
///     ([x y & s] (and (<= x y) (recur_when (next s) [y (first s) (next s)] => (<= y (first s)))))
/// )

/// (§ defn >
///     ([x] true)
///     ([x y] (Numbers_1_gt x y))
///     ([x y & s] (and (> x y) (recur_when (next s) [y (first s) (next s)] => (> y (first s)))))
/// )

/// (§ defn >=
///     ([x] true)
///     ([x y] (Numbers_1_gte x y))
///     ([x y & s] (and (>= x y) (recur_when (next s) [y (first s) (next s)] => (>= y (first s)))))
/// )

/// (defn max
///     ([x] x)
///     ([x y] (if (> x y) x y))
///     ([x y & s] (reduce max (max x y) s))
/// )

/// (defn min
///     ([x] x)
///     ([x y] (if (< x y) x y))
///     ([x y & s] (reduce min (min x y) s))
/// )

/// (§ defn zero_9_ [n] (Numbers_1_isZero n))
/// (§ defn pos_9_  [n] (Numbers_1_isPos  n))
/// (§ defn neg_9_  [n] (Numbers_1_isNeg  n))

/// (§ defn +
///     ([] 0)
///     ([x] #_"Number" x)
///     ([x y] (Numbers_1_add x y))
///     ([x y & s] (reduce + (+ x y) s))
/// )

/// (§ defn -
///     ([x] (Numbers_1_negate x))
///     ([x y] (Numbers_1_subtract x y))
///     ([x y & s] (reduce - (- x y) s))
/// )

/// (defn abs [a] (if (neg_9_ a) (- a) a))

/// (§ defn inc [x] (Numbers_1_inc x))

/// (§ defn dec [x] (Numbers_1_dec x))

/// (§ defn *
///     ([] 1)
///     ([x] #_"Number" x)
///     ([x y] (Numbers_1_multiply x y))
///     ([x y & s] (reduce * (* x y) s))
/// )

/// (§ defn /
///     ([x] (/ 1 x))
///     ([x y] (Numbers_1_divide x y))
///     ([x y & s] (reduce / (/ x y) s))
/// )

/// (§ defn quot [num div] (Numbers_1_quotient num div))

/// (§ defn rem [num div] (Numbers_1_remainder num div))

/// (defn mod [num div]
///     (let_when [m (rem num div)] (or (zero_9_ m) (= (pos_9_ num) (pos_9_ div))) => (+ m div)
///         m
///     )
/// )

/// (defn bit_not [x] (Numbers_1_not x))

/// (§ defn bit_and
///     ([x y] (Numbers_1_and x y))
///     ([x y & s] (reduce bit_and (bit_and x y) s))
/// )

/// (§ defn bit_or
///     ([x y] (Numbers_1_or x y))
///     ([x y & s] (reduce bit_or (bit_or x y) s))
/// )

/// (§ defn bit_xor
///     ([x y] (Numbers_1_xor x y))
///     ([x y & s] (reduce bit_xor (bit_xor x y) s))
/// )

/// (defn bit_and_not
///     ([x y] (Numbers_1_andNot x y))
///     ([x y & s] (reduce bit_and_not (bit_and_not x y) s))
/// )

/// (defn bit_clear [x i] (Numbers_1_clearBit x i))
/// (defn bit_set   [x i] (Numbers_1_setBit   x i))
/// (defn bit_flip  [x i] (Numbers_1_flipBit  x i))
/// (defn bit_test  [x i] (Numbers_1_testBit  x i))

/// (defn          bit_shift_left  [x n] (Numbers_1_shiftLeft          x n))
/// (defn          bit_shift_right [x n] (Numbers_1_shiftRight         x n))
/// (defn unsigned_bit_shift_right [x n] (Numbers_1_unsignedShiftRight x n))

/// (defn even_9_ [n]
///     (when (integer_9_ n) => (throw (str "argument must be an integer: " n))
///         (zero_9_ (bit_and n 1))
///     )
/// )

/// (defn odd_9_ [n] (not (even_9_ n)))
}

namespace arbace {

// (about #_"AFn"
///     (defn #_"void" AFn_1_throwArity [#_"IFn" f, #_"int" n]
///         (throw (str "wrong number of args (" (if (neg_9_ n) (str "more than " (dec (- n))) n) ") passed to " f))
///     )

///     (defn #_"Object" AFn_1_applyTo [#_"IFn" f, #_"ISeq" s]
///         (case_4_ (count s (inc 9))
///             0                                           (IFn_3_invoke f)
///             1 (let [[a1] s]                             (IFn_3_invoke f, a1))
///             2 (let [[a1 a2] s]                          (IFn_3_invoke f, a1, a2))
///             3 (let [[a1 a2 a3] s]                       (IFn_3_invoke f, a1, a2, a3))
///             4 (let [[a1 a2 a3 a4] s]                    (IFn_3_invoke f, a1, a2, a3, a4))
///             5 (let [[a1 a2 a3 a4 a5] s]                 (IFn_3_invoke f, a1, a2, a3, a4, a5))
///             6 (let [[a1 a2 a3 a4 a5 a6] s]              (IFn_3_invoke f, a1, a2, a3, a4, a5, a6))
///             7 (let [[a1 a2 a3 a4 a5 a6 a7] s]           (IFn_3_invoke f, a1, a2, a3, a4, a5, a6, a7))
///             8 (let [[a1 a2 a3 a4 a5 a6 a7 a8] s]        (IFn_3_invoke f, a1, a2, a3, a4, a5, a6, a7, a8))
///             9 (let [[a1 a2 a3 a4 a5 a6 a7 a8 a9] s]     (IFn_3_invoke f, a1, a2, a3, a4, a5, a6, a7, a8, a9))
///               (let [[a1 a2 a3 a4 a5 a6 a7 a8 a9 & s] s] (IFn_3_invoke f, a1, a2, a3, a4, a5, a6, a7, a8, a9, s))
///         )
///     )
// )
}

namespace arbace {

// (about #_"Symbol"
///     (declare Symbol_2_withMeta Symbol_2_hash Symbol_2_equals)

///     (defq Symbol [#_"IPersistentMap" _meta, #_"String" ns, #_"String" name]
///         clojure.lang.IMeta (meta [_] (-/into {} (_0__meta _)))
///         clojure.lang.IObj (withMeta [_, m] (Symbol_2_withMeta _, m))
///         clojure.lang.IHashEq (hasheq [_] (Symbol_2_hash _))
///         clojure.lang.Named (getNamespace [_] (_0_ns _)) (getName [_] (_0_name _))
///         java.lang.Object (equals [_, o] (Symbol_2_equals _, o)) (hashCode [_] (hash_combine (Object_2_hashCode (_0_name _)) (_0_ns _))) (toString [_] (str _))
///     )

///     #_inherit
///     (defm Symbol AFn)

///     (defn_ #_"Symbol" Symbol_1_new
///         ([#_"String" ns, #_"String" name] (Symbol_1_new nil, ns, name))
///         ([#_"IPersistentMap" meta, #_"String" ns, #_"String" name]
///             (new_8_ Symbol_1_class (anew [meta, ns, name]))
///         )
///     )

///     (defn_ #_"Symbol" Symbol_2_withMeta [#_"Symbol" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (Symbol_1_new meta, (_0_ns this), (_0_name this))
///         )
///     )

///     (defn #_"Symbol" Symbol_1_intern
///         ([#_"String" nsname]
///             (let [#_"int" i (String_2_indexOf nsname, (int \/))]
///                 (if (or (= i -1) (= nsname "/"))
///                     (Symbol_1_new nil, nsname)
///                     (Symbol_1_new (String_2_substring nsname, 0, i), (String_2_substring nsname, (inc i)))
///                 )
///             )
///         )
///         ([#_"String" ns, #_"String" name]
///             (Symbol_1_new ns, name)
///         )
///     )

///     (defn_ #_"bool" Symbol_2_equals [#_"Symbol" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (and (symbol_9_ that) (= (_0_ns this) (#__0_ns namespace that)) (= (_0_name this) (#__0_name name that)))
///         )
///     )

///     (defn_ #_"Appendable" Symbol_2_append [#_"Symbol" this, #_"Appendable" a]
///         (if (some_9_ (_0_ns this)) (-> a (Appendable_3_append (_0_ns this)) (Appendable_3_append "/") (Appendable_3_append (_0_name this))) (Appendable_3_append a, (_0_name this)))
///     )

///     (defn_ #_"int" Symbol_2_hash [#_"Symbol" this]
///         (hash_combine (Murmur3_1_hashUnencodedChars (_0_name this)) (_0_ns this))
///     )

///     (defn_ #_"Object" Symbol_2_invoke
///         ([#_"Symbol" this, #_"Object" obj] (get obj this))
///         ([#_"Symbol" this, #_"Object" obj, #_"Object" not_found] (get obj this not_found))
///     )

///     (defn_ #_"int" Symbol_2_compareTo [#_"Symbol" this, #_"Symbol" that]
///         (cond
///             (= this that)                              0
///             (and (nil_9_ (_0_ns this)) (some_9_ (_0_ns that))) -1
///             (nil_9_ (_0_ns this))                          (compare (_0_name this) (_0_name that))
///             (nil_9_ (_0_ns that))                          1
///             _0_else
///                 (let_when [#_"int" cmp (compare (_0_ns this) (_0_ns that))] (zero_9_ cmp) => cmp
///                     (compare (_0_name this) (_0_name that))
///                 )
///         )
///     )

///     (defm Symbol IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm Symbol IObj
///         (IObj_3_withMeta => Symbol_2_withMeta)
///     )

///     (defm Symbol INamed
///         (INamed_3_getNamespace => _0_ns)
///         (INamed_3_getName => _0_name)
///     )

///     (defm Symbol IObject
///         (IObject_3_equals => Symbol_2_equals)
///     )

///     (defm Symbol IAppend
///         (IAppend_3_append => Symbol_2_append)
///     )

///     (defm Symbol Hashed
///         (Hashed_3_hash => Symbol_2_hash)
///     )

///     (defm Symbol IFn
///         (IFn_3_invoke => Symbol_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm Symbol Comparable
///         (Comparable_3_compareTo => Symbol_2_compareTo)
///     )
// )

/// (defn symbol
///     ([name] (if (symbol_9_ name) name (Symbol_1_intern name)))
///     ([ns name] (Symbol_1_intern ns, name))
/// )

/// (-/defmethod -/print_method (_0_on_interface Symbol) [o w] (.write w, (str o)))
}

namespace arbace {

// (about #_"Keyword"
///     (declare Keyword_2_equals Keyword_2_invoke)

///     (defq Keyword [#_"Symbol" sym, #_"int" _hash]
///         clojure.lang.IHashEq (hasheq [_] (_0__hash _))
///         clojure.lang.Named (getNamespace [_] (_0_ns (_0_sym _))) (getName [_] (_0_name (_0_sym _)))
///         java.lang.Object (equals [_, o] (Keyword_2_equals _, o)) (hashCode [_] (+ (Object_2_hashCode (_0_sym _)) (int_4_ 0x9e3779b9))) (toString [_] (str _))
///         clojure.lang.IFn (invoke [_, a] (Keyword_2_invoke _, a))
///     )

///     #_inherit
///     (defm Keyword AFn)

///     (def_ #_"{Symbol Reference<Keyword>}'" Keyword_1_cache (atom (hash_map)))
///     (def_ #_"ReferenceQueue" Keyword_1_queue (ReferenceQueue_1_new))

///     (defn_ #_"Keyword" Keyword_1_new [#_"Symbol" sym]
///         (new_8_ Keyword_1_class (anew [sym, (+ (f_1_hash sym) (int_4_ 0x9e3779b9))]))
///     )

///     (declare Cache_1_purge)

///     (defn #_"Keyword" Keyword_1_intern [#_"Symbol" sym]
///         (let [#_"Reference<Keyword>" r (get (deref Keyword_1_cache) sym)
///               [sym r #_"Keyword" k]
///                 (when (nil_9_ r) => [sym r nil]
///                     (Cache_1_purge Keyword_1_queue, Keyword_1_cache)
///                     (let [sym
///                             (when (some_9_ (meta sym)) => sym
///                                 (with_meta sym nil)
///                             )
///                           k (Keyword_1_new sym) r (WeakReference_1_new #_"<Keyword>" k, Keyword_1_queue)
///                           _ (swap_4_ Keyword_1_cache assoc sym r)]
///                         [sym r k]
///                     )
///                 )]
///             (when (some_9_ r) => k
///                 (or (Reference_2_get r)
///                     (do
///                         (swap_4_ Keyword_1_cache #(if (identical_9_ (get % sym) r) (dissoc % sym) %))
///                         (recur #_"Keyword_1_intern" sym)
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Keyword" Keyword_1_find [#_"Symbol" sym]
///         (when_some [#_"Reference<Keyword>" ref (get (deref Keyword_1_cache) sym)]
///             (Reference_2_get ref)
///         )
///     )

///     (defn_ #_"String" Keyword_2_getNamespace [#_"Keyword" this]
///         (INamed_3_getNamespace (_0_sym this))
///     )

///     (defn_ #_"String" Keyword_2_getName [#_"Keyword" this]
///         (INamed_3_getName (_0_sym this))
///     )

///     (defn_ #_"bool" Keyword_2_equals [#_"Keyword" this, #_"Object" that]
///         (identical_9_ this that)
///     )

///     (defn_ #_"Appendable" Keyword_2_append [#_"Keyword" this, #_"Appendable" a]
///         (-> a (Appendable_3_append ":") (append (_0_sym this)))
///     )

///     (defn_ #_"Object" Keyword_2_invoke
///         ([#_"Keyword" this, #_"Object" obj] (get obj this))
///         ([#_"Keyword" this, #_"Object" obj, #_"Object" not_found] (get obj this not_found))
///     )

///     (defn_ #_"int" Keyword_2_compareTo [#_"Keyword" this, #_"Keyword" that]
///         (compare (_0_sym this) (_0_sym that))
///     )

///     (defm Keyword INamed
///         (INamed_3_getNamespace => Keyword_2_getNamespace)
///         (INamed_3_getName => Keyword_2_getName)
///     )

///     (defm Keyword Hashed
///         (Hashed_3_hash => _0__hash)
///     )

///     (defm Keyword IObject
///         (IObject_3_equals => Keyword_2_equals)
///     )

///     (defm Keyword IAppend
///         (IAppend_3_append => Keyword_2_append)
///     )

///     (defm Keyword IFn
///         (IFn_3_invoke => Keyword_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm Keyword Comparable
///         (Comparable_3_compareTo => Keyword_2_compareTo)
///     )
// )

/// (defn keyword
///     ([name]
///         (cond
///             (keyword_9_ name) name
///             (symbol_9_ name) (Keyword_1_intern #_"Symbol" name)
///             (string_9_ name) (Keyword_1_intern (symbol #_"String" name))
///         )
///     )
///     ([ns name] (Keyword_1_intern (symbol ns name)))
/// )

/// (defn find_keyword
///     ([name]
///         (cond
///             (keyword_9_ name) name
///             (symbol_9_ name) (Keyword_1_find #_"Symbol" name)
///             (string_9_ name) (Keyword_1_find (symbol #_"String" name))
///         )
///     )
///     ([ns name] (Keyword_1_find (symbol ns name)))
/// )

/// (-/defmethod -/print_method (_0_on_interface Keyword) [o w] (.write w, (str o)))
}

namespace arbace {

// (about #_"Fn"
///     (defq Fn [])

///     #_inherit
///     (defm Fn AFn)

///     (defn #_"Fn" Fn_1_new []
///         (new_8_ Fn_1_class (anew []))
///     )

///     (defn_ #_"Object" Fn_2_invoke
///         ([#_"Fn" this]                                                   (AFn_1_throwArity this,   0))
///         ([#_"Fn" this, a1]                                               (AFn_1_throwArity this,   1))
///         ([#_"Fn" this, a1, a2]                                           (AFn_1_throwArity this,   2))
///         ([#_"Fn" this, a1, a2, a3]                                       (AFn_1_throwArity this,   3))
///         ([#_"Fn" this, a1, a2, a3, a4]                                   (AFn_1_throwArity this,   4))
///         ([#_"Fn" this, a1, a2, a3, a4, a5]                               (AFn_1_throwArity this,   5))
///         ([#_"Fn" this, a1, a2, a3, a4, a5, a6]                           (AFn_1_throwArity this,   6))
///         ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7]                       (AFn_1_throwArity this,   7))
///         ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7, a8]                   (AFn_1_throwArity this,   8))
///         ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]               (AFn_1_throwArity this,   9))
///         ([#_"Fn" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"ISeq" args] (AFn_1_throwArity this, -10))
///     )

///     (defn_ #_"int" Fn_2_compare [#_"Fn" this, #_"Object" o1, #_"Object" o2]
///         (let [#_"Object" o (IFn_3_invoke this, o1, o2)]
///             (if (boolean_9_ o)
///                 (cond (boolean o) -1 (boolean (IFn_3_invoke this, o2, o1)) 1 _0_else 0)
///                 (int_4_ o)
///             )
///         )
///     )

///     (defm Fn IFn
///         (IFn_3_invoke => Fn_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm Fn Comparator
///         (Comparator_3_compare => Fn_2_compare)
///     )
// )
}

namespace arbace {

// (about #_"Closure"
///     (declare Closure_2_invoke Closure_2_applyTo)

///     (defq Closure [#_"IPersistentMap" _meta, #_"FnExpr" fun, #_"map'" _env]
///         clojure.lang.IFn (invoke [_] (Closure_2_invoke _)) (invoke [_, a1] (Closure_2_invoke _, a1)) (invoke [_, a1, a2] (Closure_2_invoke _, a1, a2)) (applyTo [_, args] (Closure_2_applyTo _, args))
///     )

///     #_inherit
///     (defm Closure Fn AFn)

///     (defn #_"Closure" Closure_1_new
///         ([#_"FnExpr" fun, #_"IPersistentMap" env] (Closure_1_new nil, fun, env))
///         ([#_"IPersistentMap" meta, #_"FnExpr" fun, #_"IPersistentMap" env]
///             (new_8_ Closure_1_class (anew [meta, fun, (atom env)]))
///         )
///     )

///     (defn_ #_"Closure" Closure_2_withMeta [#_"Closure" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (new_8_ Closure_1_class (anew [meta, (_0_fun this), (_0__env this)]))
///         )
///     )

///     (defn_ #_"Object" Closure_2_invoke
///         ([#_"Closure" this]                                                 (IFn_3_applyTo this, nil))
///         ([#_"Closure" this, a1]                                             (IFn_3_applyTo this, (list a1)))
///         ([#_"Closure" this, a1, a2]                                         (IFn_3_applyTo this, (list a1 a2)))
///         ([#_"Closure" this, a1, a2, a3]                                     (IFn_3_applyTo this, (list a1 a2 a3)))
///         ([#_"Closure" this, a1, a2, a3, a4]                                 (IFn_3_applyTo this, (list a1 a2 a3 a4)))
///         ([#_"Closure" this, a1, a2, a3, a4, a5]                             (IFn_3_applyTo this, (list a1 a2 a3 a4 a5)))
///         ([#_"Closure" this, a1, a2, a3, a4, a5, a6]                         (IFn_3_applyTo this, (list a1 a2 a3 a4 a5 a6)))
///         ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7]                     (IFn_3_applyTo this, (list a1 a2 a3 a4 a5 a6 a7)))
///         ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7, a8]                 (IFn_3_applyTo this, (list a1 a2 a3 a4 a5 a6 a7 a8)))
///         ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]             (IFn_3_applyTo this, (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
///         ([#_"Closure" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"ISeq" a_8_] (IFn_3_applyTo this, (list_8_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a_8_)))
///     )

///     (declare Compiler_1_MAX_POSITIONAL_ARITY)
///     (declare Machine_1_compute)
///     (declare compile_and_memoize)

///     (defn_ #_"Object" Closure_2_applyTo [#_"Closure" this, #_"ISeq" args]
///         (let [
///             #_"FnMethod" fm
///                 (let [#_"int" m (inc Compiler_1_MAX_POSITIONAL_ARITY) #_"int" n (min (count args m) m)]
///                     (or (get (_0_regulars (_0_fun this)) n)
///                         (let_when [fm (_0_variadic (_0_fun this))] (and (some_9_ fm) (<= (dec (- (_0_arity fm))) n)) => (AFn_1_throwArity this, (if (< n m) n (- m)))
///                             fm
///                         )
///                     )
///                 )
///             #_"array" vars
///                 (let [
///                     #_"int" m (inc (reduce max (inc -1) (map _0_idx (vals (deref (_0_1_locals fm))))))
///                     #_"int" n (_0_arity fm) n (if (neg_9_ n) (- n) (inc n))
///                 ]
///                     (loop_when_recur [vars (-> (anew m) (aset_4_ 0 this)) #_"int" i 1 #_"ISeq" s (seq args)]
///                                      (< i n)
///                                      [(aset_4_ vars i (first s)) (inc i) (next s)]
///                                   => (if (some_9_ s) (aset_4_ vars i s) vars)
///                     )
///                 )
///         ]
///             (Machine_1_compute (compile_and_memoize fm), vars)
///         )
///     )

///     (defm Closure IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm Closure IObj
///         (IObj_3_withMeta => Closure_2_withMeta)
///     )

///     (defm Closure IFn
///         (IFn_3_invoke => Closure_2_invoke)
///         (IFn_3_applyTo => Closure_2_applyTo)
///     )

///     (defm Closure Comparator
///         (Comparator_3_compare => Fn_2_compare)
///     )
// )
}

namespace arbace {

// (about #_"ASeq"
///     (defn #_"bool" ASeq_2_equals [#_"ASeq" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (and (sequential_9_ that)
///                 (loop_when [#_"ISeq" s (seq this) #_"ISeq" z (seq that)] (some_9_ s) => (nil_9_ z)
///                     (and (some_9_ z) (= (first s) (first z)) (recur (next s) (next z)))
///                 )
///             )
///         )
///     )
// )
}

namespace arbace {

// (about #_"Cons"
///     (declare Cons_2_withMeta Cons_2_seq Cons_2_next Cons_2_count)
///     (declare cons)

///     (defq Cons [#_"IPersistentMap" _meta, #_"Object" car, #_"ISeq" cdr] SeqForm
///         clojure.lang.IMeta (meta [_] (-/into {} (_0__meta _)))
///         clojure.lang.IObj (withMeta [_, m] (Cons_2_withMeta _, m))
///         clojure.lang.ISeq (seq [_] (Cons_2_seq _)) (first [_] (_0_car _)) (next [_] (Cons_2_next _)) (more [_] (or (Cons_2_next _) ()))
///         clojure.lang.IPersistentCollection (cons [_, o] (cons o _)) (count [_] (Cons_2_count _)) (equiv [_, o] (ASeq_2_equals _, o))
///         clojure.lang.Sequential
///     )

///     #_inherit
///     (defm Cons ASeq)

///     (defn #_"Cons" Cons_1_new
///         ([#_"Object" car, #_"ISeq" cdr] (Cons_1_new nil, car, cdr))
///         ([#_"IPersistentMap" meta, #_"Object" car, #_"ISeq" cdr]
///             (new_8_ Cons_1_class (anew [meta, car, cdr]))
///         )
///     )

///     (defn_ #_"Cons" Cons_2_withMeta [#_"Cons" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (Cons_1_new meta, (_0_car this), (_0_cdr this))
///         )
///     )

///     (defn_ #_"ISeq" Cons_2_seq [#_"Cons" this]
///         this
///     )

///     (defn_ #_"ISeq" Cons_2_next [#_"Cons" this]
///         (seq (_0_cdr this))
///     )

///     (defn_ #_"int" Cons_2_count [#_"Cons" this]
///         (inc (count (_0_cdr this)))
///     )

///     (defm Cons IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm Cons IObj
///         (IObj_3_withMeta => Cons_2_withMeta)
///     )

///     (defm Cons Sequential)

///     (defm Cons Seqable
///         (Seqable_3_seq => Cons_2_seq)
///     )

///     (defm Cons ISeq
///         (ISeq_3_first => _0_car)
///         (ISeq_3_next => Cons_2_next)
///     )

///     (defm Cons Counted
///         (Counted_3_count => Cons_2_count)
///     )

///     (defm Cons Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm Cons IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

/// (defn cons [x s] (Cons_1_new x, (seq s)))
}

namespace arbace {

// (about #_"Iterate"
///     (declare Iterate_2_seq Iterate_2_first Iterate_2_next)

///     (defq Iterate [#_"IPersistentMap" _meta, #_"IFn" f, #_"Object" x, #_"Object'" y] SeqForm
///         clojure.lang.ISeq (seq [_] (Iterate_2_seq _)) (first [_] (Iterate_2_first _)) (next [_] (Iterate_2_next _)) (more [_] (or (Iterate_2_next _) ()))
///     )

///     #_inherit
///     (defm Iterate ASeq)

///     (defn_ #_"Iterate" Iterate_1_new
///         ([#_"IFn" f, #_"Object" x, #_"Object" y] (Iterate_1_new nil, f, x, y))
///         ([#_"IPersistentMap" meta, #_"IFn" f, #_"Object" x, #_"Object" y]
///             (new_8_ Iterate_1_class (anew [meta, f, x, (atom y)]))
///         )
///     )

///     (defn_ #_"Iterate" Iterate_2_withMeta [#_"Iterate" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (Iterate_1_new meta, (_0_f this), (_0_x this), (deref (_0_y this)))
///         )
///     )

///     (defn #_"ISeq" Iterate_1_create [#_"IFn" f, #_"Object" y] (Iterate_1_new f, nil, y))

///     (def_ #_"Object" Iterate_1_UNREALIZED (anew 0))

///     (defn_ #_"bool" Iterate_2_isRealized [#_"Iterate" this]
///         (not (identical_9_ (deref (_0_y this)) Iterate_1_UNREALIZED))
///     )

///     (defn_ #_"ISeq" Iterate_2_seq [#_"Iterate" this]
///         this
///     )

///     (defn_ #_"Object" Iterate_2_first [#_"Iterate" this]
///         (let_when [#_"Object" y (deref (_0_y this))] (identical_9_ y Iterate_1_UNREALIZED) => y
///             (reset_4_ (_0_y this) ((_0_f this) (_0_x this)))
///         )
///     )

///     #_memoize!
///     (defn_ #_"ISeq" Iterate_2_next [#_"Iterate" this]
///         (Iterate_1_new (_0_f this), (first this), Iterate_1_UNREALIZED)
///     )

///     (defn_ #_"Object" Iterate_2_reduce
///         ([#_"Iterate" this, #_"IFn" f]
///             (loop [#_"Object" r (first this) #_"Object" v ((_0_f this) r)]
///                 (let_when [r (f r v)] (reduced_9_ r) => (recur r ((_0_f this) v))
///                     (deref r)
///                 )
///             )
///         )
///         ([#_"Iterate" this, #_"IFn" f, #_"Object" r]
///             (loop [r r #_"Object" v (first this)]
///                 (let_when [r (f r v)] (reduced_9_ r) => (recur r ((_0_f this) v))
///                     (deref r)
///                 )
///             )
///         )
///     )

///     (defm Iterate IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm Iterate IObj
///         (IObj_3_withMeta => Iterate_2_withMeta)
///     )

///     (defm Iterate IPending
///         (IPending_3_isRealized => Iterate_2_isRealized)
///     )

///     (defm Iterate Sequential)

///     (defm Iterate Seqable
///         (Seqable_3_seq => Iterate_2_seq)
///     )

///     (defm Iterate ISeq
///         (ISeq_3_first => Iterate_2_first)
///         (ISeq_3_next => Iterate_2_next)
///     )

///     (defm Iterate IReduce
///         (IReduce_3_reduce => Iterate_2_reduce)
///     )

///     (defm Iterate Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm Iterate IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

/// (defn iterate [f x] (Iterate_1_create f x))
}

namespace arbace {

// (about #_"Repeat"
///     (declare Repeat_2_seq Repeat_2_next)

///     (defq Repeat [#_"IPersistentMap" _meta, #_"long" cnt, #_"Object" val] SeqForm
///         clojure.lang.ISeq (seq [_] (Repeat_2_seq _)) (first [_] (_0_val _)) (next [_] (Repeat_2_next _)) (more [_] (or (Repeat_2_next _) ()))
///     )

///     #_inherit
///     (defm Repeat ASeq)

///     (def_ #_"long" Repeat_1_INFINITE -1)

///     (defn_ #_"Repeat" Repeat_1_new
///         ([#_"long" cnt, #_"Object" val] (Repeat_1_new nil, cnt, val))
///         ([#_"IPersistentMap" meta, #_"long" cnt, #_"Object" val]
///             (new_8_ Repeat_1_class (anew [meta, cnt, val]))
///         )
///     )

///     (defn_ #_"Repeat" Repeat_2_withMeta [#_"Repeat" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (Repeat_1_new meta, (_0_cnt this), (_0_val this))
///         )
///     )

///     (declare list)

///     (defn #_"Repeat|ISeq" Repeat_1_create
///         ([#_"Object" val] (Repeat_1_new Repeat_1_INFINITE, val))
///         ([#_"long" n, #_"Object" val] (if (pos_9_ n) (Repeat_1_new n, val) (list)))
///     )

///     (defn_ #_"ISeq" Repeat_2_seq [#_"Repeat" this]
///         this
///     )

///     (defn_ #_"ISeq" Repeat_2_next [#_"Repeat" this]
///         (cond
///             (< 1 (_0_cnt this))               (Repeat_1_new (dec (_0_cnt this)), (_0_val this))
///             (= (_0_cnt this) Repeat_1_INFINITE) this
///         )
///     )

///     (defn_ #_"Object" Repeat_2_reduce
///         ([#_"Repeat" this, #_"IFn" f]
///             (let [#_"Object" r (_0_val this)]
///                 (if (= (_0_cnt this) Repeat_1_INFINITE)
///                     (loop [r r]
///                         (let [r (f r (_0_val this))]
///                             (if (reduced_9_ r) (deref r) (recur r))
///                         )
///                     )
///                     (loop_when [r r #_"long" i 1] (< i (_0_cnt this)) => r
///                         (let [r (f r (_0_val this))]
///                             (if (reduced_9_ r) (deref r) (recur r (inc i)))
///                         )
///                     )
///                 )
///             )
///         )
///         ([#_"Repeat" this, #_"IFn" f, #_"Object" r]
///             (if (= (_0_cnt this) Repeat_1_INFINITE)
///                 (loop [r r]
///                     (let [r (f r (_0_val this))]
///                         (if (reduced_9_ r) (deref r) (recur r))
///                     )
///                 )
///                 (loop_when [r r #_"long" i 0] (< i (_0_cnt this)) => r
///                     (let [r (f r (_0_val this))]
///                         (if (reduced_9_ r) (deref r) (recur r (inc i)))
///                     )
///                 )
///             )
///         )
///     )

///     (defm Repeat IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm Repeat IObj
///         (IObj_3_withMeta => Repeat_2_withMeta)
///     )

///     (defm Repeat Sequential)

///     (defm Repeat Seqable
///         (Seqable_3_seq => Repeat_2_seq)
///     )

///     (defm Repeat ISeq
///         (ISeq_3_first => _0_val)
///         (ISeq_3_next => Repeat_2_next)
///     )

///     (defm Repeat IReduce
///         (IReduce_3_reduce => Repeat_2_reduce)
///     )

///     (defm Repeat Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm Repeat IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

/// (defn repeat
///     ([  x] (Repeat_1_create   x))
///     ([n x] (Repeat_1_create n x))
/// )
}

namespace arbace {

// (about #_"Range"
///     (declare Range_2_seq Range_2_next)

///     (defq Range [#_"IPersistentMap" _meta, #_"Object" start, #_"Object" end, #_"Object" step, #_"IFn" f_1_boundsCheck] SeqForm
///         clojure.lang.ISeq (seq [_] (Range_2_seq _)) (first [_] (_0_start _)) (next [_] (Range_2_next _)) (more [_] (or (Range_2_next _) ()))
///     )

///     #_inherit
///     (defm Range ASeq)

///     #_abstract
///     (defm Range Counted)

///     (defn_ #_"Range" Range_1_new
///         ([#_"Object" start, #_"Object" end, #_"Object" step, #_"IFn" f_1_boundsCheck]
///             (Range_1_new nil, start, end, step, f_1_boundsCheck)
///         )
///         ([#_"IPersistentMap" meta, #_"Object" start, #_"Object" end, #_"Object" step, #_"IFn" f_1_boundsCheck]
///             (new_8_ Range_1_class (anew [meta, start, end, step, f_1_boundsCheck]))
///         )
///     )

///     (defn_ #_"Range" Range_2_withMeta [#_"Range" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (Range_1_new meta, (_0_end this), (_0_start this), (_0_step this), (_0_f_1_boundsCheck this))
///         )
///     )

///     (defn_ #_"IFn" Range_1_positiveStep [#_"Object" end] #(<= end %))
///     (defn_ #_"IFn" Range_1_negativeStep [#_"Object" end] #(<= % end))

///     (defn #_"ISeq" Range_1_create
///         ([#_"Object" end]
///             (when (pos_9_ end) => (list)
///                 (Range_1_new 0, end, 1, (Range_1_positiveStep end))
///             )
///         )
///         ([#_"Object" start, #_"Object" end]
///             (Range_1_create start, end, 1)
///         )
///         ([#_"Object" start, #_"Object" end, #_"Object" step]
///             (cond
///                 (or (and (pos_9_ step) (< end start))
///                     (and (neg_9_ step) (< start end))
///                     (= start end)
///                 )
///                     (list)
///                 (zero_9_ step)
///                     (Repeat_1_create start)
///                 _0_else
///                     (Range_1_new start, end, step, (if (pos_9_ step) (Range_1_positiveStep end) (Range_1_negativeStep end)))
///             )
///         )
///     )

///     (defn_ #_"ISeq" Range_2_seq [#_"Range" this]
///         this
///     )

///     (defn_ #_"ISeq" Range_2_next [#_"Range" this]
///         (let_when_not [#_"Object" n (+ (_0_start this) (_0_step this))] ((_0_f_1_boundsCheck this) n)
///             (Range_1_new n, (_0_end this), (_0_step this), (_0_f_1_boundsCheck this))
///         )
///     )

///     (defn_ #_"Object" Range_2_reduce
///         ([#_"Range" this, #_"IFn" f]
///             (loop [#_"Object" r (_0_start this) #_"Number" n r]
///                 (let_when_not [n (+ n (_0_step this))] ((_0_f_1_boundsCheck this) n) => r
///                     (let_when_not [r (f r n)] (reduced_9_ r) => (deref r)
///                         (recur r n)
///                     )
///                 )
///             )
///         )
///         ([#_"Range" this, #_"IFn" f, #_"Object" r]
///             (loop [r r #_"Object" n (_0_start this)]
///                 (let_when_not [r (f r n)] (reduced_9_ r) => (deref r)
///                     (let_when_not [n (+ n (_0_step this))] ((_0_f_1_boundsCheck this) n) => r
///                         (recur r n)
///                     )
///                 )
///             )
///         )
///     )

///     (defm Range IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm Range IObj
///         (IObj_3_withMeta => Range_2_withMeta)
///     )

///     (defm Range Sequential)

///     (defm Range Seqable
///         (Seqable_3_seq => Range_2_seq)
///     )

///     (defm Range ISeq
///         (ISeq_3_first => _0_start)
///         (ISeq_3_next => Range_2_next)
///     )

///     (defm Range IReduce
///         (IReduce_3_reduce => Range_2_reduce)
///     )

///     (defm Range Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm Range IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

/// (defn range
///     ([] (iterate inc 0))
///     ([over] (Range_1_create over))
///     ([from over] (Range_1_create from over))
///     ([from over step] (Range_1_create from over step))
/// )
}

namespace arbace {

// (about #_"ArraySeq"
///     (declare ArraySeq_2_seq ArraySeq_2_first ArraySeq_2_next)

///     (defq ArraySeq [#_"IPersistentMap" _meta, #_"array" a, #_"int" i] SeqForm
///         clojure.lang.ISeq (seq [_] (ArraySeq_2_seq _)) (first [_] (ArraySeq_2_first _)) (next [_] (ArraySeq_2_next _)) (more [_] (or (ArraySeq_2_next _) ()))
///         clojure.lang.Sequential
///     )

///     #_inherit
///     (defm ArraySeq ASeq)

///     (defn #_"ArraySeq" ArraySeq_1_new
///         ([#_"array" a, #_"int" i] (ArraySeq_1_new nil, a, i))
///         ([#_"IPersistentMap" meta, #_"array" a, #_"int" i]
///             (new_8_ ArraySeq_1_class (anew [meta, a, i]))
///         )
///     )

///     (defn_ #_"ArraySeq" ArraySeq_2_withMeta [#_"ArraySeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (ArraySeq_1_new meta, (_0_a this), (_0_i this))
///         )
///     )

///     (defn #_"ArraySeq" ArraySeq_1_create [#_"array" a]
///         (when (and (some_9_ a) (pos_9_ (alength a)))
///             (ArraySeq_1_new a, 0)
///         )
///     )

///     (-/extend_protocol Seqable (do Object_1_array)
///         (#_"ArraySeq" Seqable_3_seq [#_"array" a] (ArraySeq_1_create a))
///     )

///     (defn_ #_"ISeq" ArraySeq_2_seq [#_"ArraySeq" this]
///         this
///     )

///     (defn_ #_"Object" ArraySeq_2_first [#_"ArraySeq" this]
///         (when (some_9_ (_0_a this))
///             (aget (_0_a this) (_0_i this))
///         )
///     )

///     (defn_ #_"ISeq" ArraySeq_2_next [#_"ArraySeq" this]
///         (when (and (some_9_ (_0_a this)) (< (inc (_0_i this)) (count (_0_a this))))
///             (ArraySeq_1_new (_0_a this), (inc (_0_i this)))
///         )
///     )

///     (defn_ #_"int" ArraySeq_2_count [#_"ArraySeq" this]
///         (if (some_9_ (_0_a this)) (- (count (_0_a this)) (_0_i this)) 0)
///     )

///     (defn_ #_"Object" ArraySeq_2_reduce
///         ([#_"ArraySeq" this, #_"IFn" f]
///             (when_some [#_"array" a (_0_a this)]
///                 (let [#_"int" i (_0_i this) #_"int" n (count a)]
///                     (loop_when [#_"Object" r (aget a i) i (inc i)] (< i n) => r
///                         (let [r (f r (aget a i))]
///                             (if (reduced_9_ r) (deref r) (recur r (inc i)))
///                         )
///                     )
///                 )
///             )
///         )
///         ([#_"ArraySeq" this, #_"IFn" f, #_"Object" r]
///             (when_some [#_"array" a (_0_a this)]
///                 (let [#_"int" i (_0_i this) #_"int" n (count a)]
///                     (loop_when [r (f r (aget a i)) i (inc i)] (< i n) => (if (reduced_9_ r) (deref r) r)
///                         (if (reduced_9_ r) (deref r) (recur (f r (aget a i)) (inc i)))
///                     )
///                 )
///             )
///         )
///     )

///     (defm ArraySeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm ArraySeq IObj
///         (IObj_3_withMeta => ArraySeq_2_withMeta)
///     )

///     (defm ArraySeq Sequential)

///     (defm ArraySeq Seqable
///         (Seqable_3_seq => ArraySeq_2_seq)
///     )

///     (defm ArraySeq ISeq
///         (ISeq_3_first => ArraySeq_2_first)
///         (ISeq_3_next => ArraySeq_2_next)
///     )

///     (defm ArraySeq Counted
///         (Counted_3_count => ArraySeq_2_count)
///     )

///     (defm ArraySeq IReduce
///         (IReduce_3_reduce => ArraySeq_2_reduce)
///     )

///     (defm ArraySeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm ArraySeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )
}

namespace arbace {

// (about #_"StringSeq"
///     (declare StringSeq_2_seq StringSeq_2_first StringSeq_2_next)

///     (defq StringSeq [#_"IPersistentMap" _meta, #_"CharSequence" s, #_"int" i] SeqForm
///         clojure.lang.ISeq (seq [_] (StringSeq_2_seq _)) (first [_] (StringSeq_2_first _)) (next [_] (StringSeq_2_next _)) (more [_] (or (StringSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm StringSeq ASeq)

///     (defn_ #_"StringSeq" StringSeq_1_new [#_"IPersistentMap" meta, #_"CharSequence" s, #_"int" i]
///         (new_8_ StringSeq_1_class (anew [meta, s, i]))
///     )

///     (defn_ #_"StringSeq" StringSeq_2_withMeta [#_"StringSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (StringSeq_1_new meta, (_0_s this), (_0_i this))
///         )
///     )

///     (defn #_"StringSeq" StringSeq_1_create [#_"CharSequence" s]
///         (when (pos_9_ (CharSequence_3_length s))
///             (StringSeq_1_new nil, s, 0)
///         )
///     )

///     (-/extend_protocol Seqable java.lang.CharSequence
///         (#_"StringSeq" Seqable_3_seq [#_"CharSequence" s] (StringSeq_1_create s))
///     )

///     (defn_ #_"ISeq" StringSeq_2_seq [#_"StringSeq" this]
///         this
///     )

///     (defn_ #_"Object" StringSeq_2_first [#_"StringSeq" this]
///         (Character_1_valueOf (CharSequence_3_charAt (_0_s this), (_0_i this)))
///     )

///     (defn_ #_"ISeq" StringSeq_2_next [#_"StringSeq" this]
///         (when (< (inc (_0_i this)) (CharSequence_3_length (_0_s this)))
///             (StringSeq_1_new (_0__meta this), (_0_s this), (inc (_0_i this)))
///         )
///     )

///     (defn_ #_"int" StringSeq_2_count [#_"StringSeq" this]
///         (- (CharSequence_3_length (_0_s this)) (_0_i this))
///     )

///     (defn_ #_"Object" StringSeq_2_reduce
///         ([#_"StringSeq" this, #_"IFn" f]
///             (let [#_"CharSequence" s (_0_s this) #_"int" i (_0_i this) #_"int" n (CharSequence_3_length s)]
///                 (loop_when [#_"Object" r (CharSequence_3_charAt s, i) i (inc i)] (< i n) => r
///                     (let [r (f r (CharSequence_3_charAt s, i))]
///                         (if (reduced_9_ r) (deref r) (recur r (inc i)))
///                     )
///                 )
///             )
///         )
///         ([#_"StringSeq" this, #_"IFn" f, #_"Object" r]
///             (let [#_"CharSequence" s (_0_s this) #_"int" i (_0_i this) #_"int" n (CharSequence_3_length s)]
///                 (loop_when [r (f r (CharSequence_3_charAt s, i)) i (inc i)] (< i n) => (if (reduced_9_ r) (deref r) r)
///                     (if (reduced_9_ r) (deref r) (recur (f r (CharSequence_3_charAt s, i)) (inc i)))
///                 )
///             )
///         )
///     )

///     (defm StringSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm StringSeq IObj
///         (IObj_3_withMeta => StringSeq_2_withMeta)
///     )

///     (defm StringSeq Sequential)

///     (defm StringSeq Seqable
///         (Seqable_3_seq => StringSeq_2_seq)
///     )

///     (defm StringSeq ISeq
///         (ISeq_3_first => StringSeq_2_first)
///         (ISeq_3_next => StringSeq_2_next)
///     )

///     (defm StringSeq Counted
///         (Counted_3_count => StringSeq_2_count)
///     )

///     (defm StringSeq IReduce
///         (IReduce_3_reduce => StringSeq_2_reduce)
///     )

///     (defm StringSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm StringSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )
}

namespace arbace {

// (about #_"LazySeq"
///     (declare LazySeq_2_conj LazySeq_2_seq LazySeq_2_first LazySeq_2_next)

///     (defq LazySeq [#_"IPersistentMap" _meta, #_"fn'" f, #_"Object'" o, #_"seq'" s] SeqForm
///         clojure.lang.IPersistentCollection (cons [_, o] (LazySeq_2_conj _, o))
///         clojure.lang.ISeq (seq [_] (LazySeq_2_seq _)) (first [_] (LazySeq_2_first _)) (next [_] (LazySeq_2_next _)) (more [_] (or (LazySeq_2_next _) ()))
///         clojure.lang.Sequential
///     )

///     (defn_ #_"LazySeq" LazySeq_1_init [#_"IPersistentMap" meta, #_"IFn" f, #_"ISeq" s]
///         (new_8_ LazySeq_1_class (anew [meta, (atom f), (atom nil), (atom s)]))
///     )

///     (defn_ #_"LazySeq" LazySeq_1_new
///         ([#_"IFn" f]                 (LazySeq_1_init nil,  f,   nil))
///         ([#_"IPersistentMap" meta, #_"ISeq" s] (LazySeq_1_init meta, nil, s  ))
///     )

///     (defn_ #_"LazySeq" LazySeq_2_withMeta [#_"LazySeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (LazySeq_1_new meta, (seq this))
///         )
///     )

///     (defn_ #_"cons" LazySeq_2_conj [#_"LazySeq" this, #_"Object" o]
///         (cons o this)
///     )

///     (defn_ #_"IPersistentCollection" LazySeq_2_empty [#_"LazySeq" this]
///         (list)
///     )

///     (defn_ #_"ISeq" LazySeq_2_seq [#_"LazySeq" this]
///         (locking this
///             (letfn [(step_ [this]
///                         (when_some [#_"IFn" f (deref (_0_f this))]
///                             (reset_4_ (_0_f this) nil)
///                             (reset_4_ (_0_o this) (f))
///                         )
///                         (or (deref (_0_o this)) (deref (_0_s this)))
///                     )]
///                 (step_ this)
///                 (when_some [#_"Object" o (deref (_0_o this))]
///                     (reset_4_ (_0_o this) nil)
///                     (reset_4_ (_0_s this) (loop_when_recur o (satisfies_9_ LazySeq o) (step_ o) => (seq o)))
///                 )
///                 (deref (_0_s this))
///             )
///         )
///     )

///     (defn_ #_"Object" LazySeq_2_first [#_"LazySeq" this]
///         (when_some [#_"ISeq" s (seq this)]
///             (first s)
///         )
///     )

///     (defn_ #_"ISeq" LazySeq_2_next [#_"LazySeq" this]
///         (when_some [#_"ISeq" s (seq this)]
///             (next s)
///         )
///     )

///     (defn_ #_"bool" LazySeq_2_equals [#_"LazySeq" this, #_"Object" that]
///         (if_some [#_"ISeq" s (seq this)]
///             (= s that)
///             (and (sequential_9_ that) (nil_9_ (seq that)))
///         )
///     )

///     (defn_ #_"bool" LazySeq_2_isRealized [#_"LazySeq" this]
///         (locking this
///             (nil_9_ (deref (_0_f this)))
///         )
///     )

///     (defm LazySeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm LazySeq IObj
///         (IObj_3_withMeta => LazySeq_2_withMeta)
///     )

///     (defm LazySeq IPersistentCollection
///         (IPersistentCollection_3_conj => LazySeq_2_conj)
///         (IPersistentCollection_3_empty => LazySeq_2_empty)
///     )

///     (defm LazySeq Sequential)

///     (defm LazySeq Seqable
///         (Seqable_3_seq => LazySeq_2_seq)
///     )

///     (defm LazySeq ISeq
///         (ISeq_3_first => LazySeq_2_first)
///         (ISeq_3_next => LazySeq_2_next)
///     )

///     (defm LazySeq IObject
///         (IObject_3_equals => LazySeq_2_equals)
///     )

///     (defm LazySeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm LazySeq IPending
///         (IPending_3_isRealized => LazySeq_2_isRealized)
///     )
// )

/// (defmacro lazy_seq [& body] `(LazySeq_1_new (fn_8_ [] ~@body)))

/// (defn dorun
///     ([s]
///         (when_some [s (seq s)]
///             (recur (next s))
///         )
///     )
///     ([n s]
///         (when (pos_9_ n)
///             (when_some [s (seq s)]
///                 (recur (dec n) (next s))
///             )
///         )
///     )
/// )

/// (defn doall
///     ([s] (dorun s) s)
///     ([n s] (dorun n s) s)
/// )

/// (defn concat
///     ([] (lazy_seq nil))
///     ([x] (lazy_seq x))
///     ([x y]
///         (lazy_seq
///             (let_when [s (seq x)] s => y
///                 (cons (first s) (concat (next s) y))
///             )
///         )
///     )
///     ([x y & z]
///         (letfn [(cat_ [s z]
///                     (lazy_seq
///                         (let [s (seq s)]
///                             (cond
///                                 s (cons (first s) (cat_ (next s) z))
///                                 z (cat_ (first z) (next z))
///                             )
///                         )
///                     )
///                 )]
///             (cat_ (concat x y) z)
///         )
///     )
/// )

/// (defn comp
///     ([] identity)
///     ([f] f)
///     ([f g]
///         (fn
///             ([] (f (g)))
///             ([x] (f (g x)))
///             ([x y] (f (g x y)))
///             ([x y & z] (f (apply g x y z)))
///         )
///     )
///     ([f g & fs] (reduce comp (list_8_ f g fs)))
/// )

/// (defn juxt
///     ([f]
///         (fn
///             ([] [(f)])
///             ([x] [(f x)])
///             ([x y] [(f x y)])
///             ([x y & z] [(apply f x y z)])
///         )
///     )
///     ([f g]
///         (fn
///             ([] [(f) (g)])
///             ([x] [(f x) (g x)])
///             ([x y] [(f x y) (g x y)])
///             ([x y & z] [(apply f x y z) (apply g x y z)])
///         )
///     )
///     ([f g h]
///         (fn
///             ([] [(f) (g) (h)])
///             ([x] [(f x) (g x) (h x)])
///             ([x y] [(f x y) (g x y) (h x y)])
///             ([x y & z] [(apply f x y z) (apply g x y z) (apply h x y z)])
///         )
///     )
///     ([f g h & fs]
///         (let [fs (list_8_ f g h fs)]
///             (fn
///                 ([] (reduce #(conj %1 (%2)) (vector) fs))
///                 ([x] (reduce #(conj %1 (%2 x)) (vector) fs))
///                 ([x y] (reduce #(conj %1 (%2 x y)) (vector) fs))
///                 ([x y & z] (reduce #(conj %1 (apply %2 x y z)) (vector) fs))
///             )
///         )
///     )
/// )

/// (defn partial
///     ([f] f)
///     ([f a]
///         (fn
///             ([] (f a))
///             ([x] (f a x))
///             ([x y] (f a x y))
///             ([x y z] (f a x y z))
///             ([x y z & args] (apply f a x y z args))
///         )
///     )
///     ([f a b]
///         (fn
///             ([] (f a b))
///             ([x] (f a b x))
///             ([x y] (f a b x y))
///             ([x y z] (f a b x y z))
///             ([x y z & args] (apply f a b x y z args))
///         )
///     )
///     ([f a b c]
///         (fn
///             ([] (f a b c))
///             ([x] (f a b c x))
///             ([x y] (f a b c x y))
///             ([x y z] (f a b c x y z))
///             ([x y z & args] (apply f a b c x y z args))
///         )
///     )
///     ([f a b c & more]
///         (fn [& args] (apply f a b c (concat more args)))
///     )
/// )

/// (defn fnil
///     ([f x]
///         (fn
///             ([a]               (f (if (nil_9_ a) x a)))
///             ([a b]             (f (if (nil_9_ a) x a) b))
///             ([a b c]           (f (if (nil_9_ a) x a) b c))
///             ([a b c & s] (apply f (if (nil_9_ a) x a) b c s))
///         )
///     )
///     ([f x y]
///         (fn
///             ([a b]             (f (if (nil_9_ a) x a) (if (nil_9_ b) y b)))
///             ([a b c]           (f (if (nil_9_ a) x a) (if (nil_9_ b) y b) c))
///             ([a b c & s] (apply f (if (nil_9_ a) x a) (if (nil_9_ b) y b) c s))
///         )
///     )
///     ([f x y z]
///         (fn
///             ([a b]             (f (if (nil_9_ a) x a) (if (nil_9_ b) y b)))
///             ([a b c]           (f (if (nil_9_ a) x a) (if (nil_9_ b) y b) (if (nil_9_ c) z c)))
///             ([a b c & s] (apply f (if (nil_9_ a) x a) (if (nil_9_ b) y b) (if (nil_9_ c) z c) s))
///         )
///     )
/// )

/// (defn every_9_ [f_9_ s]
///     (cond
///         (nil_9_ (seq s)) true
///         (f_9_ (first s)) (recur f_9_ (next s))
///         _0_else false
///     )
/// )

/// (def not_every_9_ (comp not every_9_))

/// (defn index_of [s x]
///     (loop_when [i 0 s (seq s)] (some_9_ s) => -1
///         (when_not (= (first s) x) => i
///             (recur (inc i) (next s))
///         )
///     )
/// )

/// (defn some [f_9_ s]
///     (when (seq s)
///         (or (f_9_ (first s)) (recur f_9_ (next s)))
///     )
/// )

/// (def not_any_9_ (comp not some))

/// (defn map
///     ([f]
///         (fn [g]
///             (fn
///                 ([] (g))
///                 ([x] (g x))
///                 ([x y] (g x (f y)))
///                 ([x y & s] (g x (apply f y s)))
///             )
///         )
///     )
///     ([f s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (cons (f (first s)) (map f (next s)))
///             )
///         )
///     )
///     ([f s1 s2]
///         (lazy_seq
///             (let_when [s1 (seq s1) s2 (seq s2)] (and s1 s2)
///                 (cons (f (first s1) (first s2)) (map f (next s1) (next s2)))
///             )
///         )
///     )
///     ([f s1 s2 s3]
///         (lazy_seq
///             (let_when [s1 (seq s1) s2 (seq s2) s3 (seq s3)] (and s1 s2 s3)
///                 (cons (f (first s1) (first s2) (first s3)) (map f (next s1) (next s2) (next s3)))
///             )
///         )
///     )
///     ([f s1 s2 s3 & z]
///         (letfn [(map_ [s]
///                     (lazy_seq
///                         (let_when [s (map seq s)] (every_9_ identity s)
///                             (cons (map first s) (map_ (map next s)))
///                         )
///                     )
///                 )]
///             (map #(apply f %) (map_ (conj z s3 s2 s1)))
///         )
///     )
/// )

/// (defn map_indexed
///     ([f]
///         (fn [g]
///             (let [i_1_ (atom -1)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x] (g s (f (swap_4_ i_1_ inc) x)))
///                 )
///             )
///         )
///     )
///     ([f s]
///         (letfn [(mapi_ [i s]
///                     (lazy_seq
///                         (when_some [s (seq s)]
///                             (cons (f i (first s)) (mapi_ (inc i) (next s)))
///                         )
///                     )
///                 )]
///             (mapi_ 0 s)
///         )
///     )
/// )

/// (defn mapcat
///     ([f] (comp (map f) cat))
///     ([f & s] (apply concat (apply map f s)))
/// )

/// (defmacro lazy_cat [& s]
///     `(concat ~@(map #(list _7_lazy_seq %) s))
/// )

/// (defn keep
///     ([f]
///         (fn [g]
///             (fn
///                 ([] (g))
///                 ([s] (g s))
///                 ([s x]
///                     (when_some [y (f x)] => s
///                         (g s y)
///                     )
///                 )
///             )
///         )
///     )
///     ([f s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (when_some [y (f (first s))] => (keep f (next s))
///                     (cons y (keep f (next s)))
///                 )
///             )
///         )
///     )
/// )

/// (defn keep_indexed
///     ([f]
///         (fn [g]
///             (let [i_1_ (atom -1)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x]
///                         (when_some [y (f (swap_4_ i_1_ inc) x)] => s
///                             (g s y)
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([f s]
///         (letfn [(keepi_ [i s]
///                     (lazy_seq
///                         (when_some [s (seq s)]
///                             (when_some [y (f i (first s))] => (keepi_ (inc i) (next s))
///                                 (cons y (keepi_ (inc i) (next s)))
///                             )
///                         )
///                     )
///                 )]
///             (keepi_ 0 s)
///         )
///     )
/// )

/// (defn filter
///     ([f_9_]
///         (fn [g]
///             (fn
///                 ([] (g))
///                 ([s] (g s))
///                 ([s x] (if (f_9_ x) (g s x) s))
///             )
///         )
///     )
///     ([f_9_ s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (let_when [x (first s)] (f_9_ x) => (filter f_9_ (next s))
///                     (cons x (filter f_9_ (next s)))
///                 )
///             )
///         )
///     )
/// )

/// (defn remove
///     ([f_9_]   (filter (complement f_9_)  ))
///     ([f_9_ s] (filter (complement f_9_) s))
/// )

/// (defn take
///     ([n]
///         (fn [g]
///             (let [n_1_ (atom n)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x]
///                         (let [n (deref n_1_) m (swap_4_ n_1_ dec) s (if (pos_9_ n) (g s x) s)]
///                             (if (pos_9_ m) s (ensure_reduced s))
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([n s]
///         (lazy_seq
///             (when (pos_9_ n)
///                 (when_some [s (seq s)]
///                     (cons (first s) (take (dec n) (next s)))
///                 )
///             )
///         )
///     )
/// )

/// (defn take_while
///     ([f_9_]
///         (fn [g]
///             (fn
///                 ([] (g))
///                 ([s] (g s))
///                 ([s x] (if (f_9_ x) (g s x) (reduced s)))
///             )
///         )
///     )
///     ([f_9_ s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (let_when [x (first s)] (f_9_ x)
///                     (cons x (take_while f_9_ (next s)))
///                 )
///             )
///         )
///     )
/// )

/// (defn drop
///     ([n]
///         (fn [g]
///             (let [n_1_ (atom n)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x] (if (neg_9_ (swap_4_ n_1_ dec)) (g s x) s))
///                 )
///             )
///         )
///     )
///     ([n s]
///         (letfn [(drop_ [n s]
///                     (let [s (seq s)]
///                         (recur_when (and (pos_9_ n) s) [(dec n) (next s)] => s)
///                     )
///                 )]
///             (lazy_seq (drop_ n s))
///         )
///     )
/// )

/// (defn drop_last
///     ([s] (drop_last 1 s))
///     ([n s] (map (fn [x _] x) s (drop n s)))
/// )

/// (defn take_last [n coll]
///     (loop_when_recur [s (seq coll) z (seq (drop n coll))] z [(next s) (next z)] => s)
/// )

/// (defn drop_while
///     ([f_9_]
///         (fn [g]
///             (let [drop_9_ (atom true)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x]
///                         (when_not (and (deref drop_9_) (f_9_ x)) => s
///                             (reset_4_ drop_9_ nil)
///                             (g s x)
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([f_9_ s]
///         (letfn [(drop_ [f_9_ s]
///                     (let [s (seq s)]
///                         (recur_when (and s (f_9_ (first s))) [f_9_ (next s)] => s)
///                     )
///                 )]
///             (lazy_seq (drop_ f_9_ s))
///         )
///     )
/// )

/// (defn split_at [n s] [(take n s) (drop n s)])

/// (defn split_with [f_9_ s] [(take_while f_9_ s) (drop_while f_9_ s)])

/// (defn take_nth
///     ([n]
///         (fn [g]
///             (let [i_1_ (atom -1)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x]
///                         (let_when [i (swap_4_ i_1_ inc)] (zero_9_ (rem i n)) => s
///                             (g s x)
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([n s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (cons (first s) (take_nth n (drop n s)))
///             )
///         )
///     )
/// )

/// (defn interleave
///     ([] (list))
///     ([c1] (lazy_seq c1))
///     ([c1 c2]
///         (lazy_seq
///             (let_when [s1 (seq c1) s2 (seq c2)] (and s1 s2)
///                 (cons (first s1) (cons (first s2) (interleave (next s1) (next s2))))
///             )
///         )
///     )
///     ([c1 c2 & cs]
///         (lazy_seq
///             (let_when [ss (map seq (conj cs c2 c1))] (every_9_ identity ss)
///                 (concat (map first ss) (apply interleave (map next ss)))
///             )
///         )
///     )
/// )

/// (defn interpose
///     ([sep]
///         (fn [g]
///             (let [started (atom false)]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x]
///                         (when (deref started) => (do (reset_4_ started true) (g s x))
///                             (let [r (g s sep)]
///                                 (if (reduced_9_ r) r (g r x))
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([sep coll] (drop 1 (interleave (repeat sep) coll)))
/// )

/// (defn partition
///     ([n s] (partition n n s))
///     ([n step s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (let_when [p (take n s)] (= (count p) n)
///                     (cons p (partition n step (nthnext s step)))
///                 )
///             )
///         )
///     )
///     ([n step pad s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (let_when [p (take n s)] (= (count p) n) => (list (take n (concat p pad)))
///                     (cons p (partition n step pad (nthnext s step)))
///                 )
///             )
///         )
///     )
/// )

/// (defn partition_all
///     ([n]
///         (fn [g]
///             (let [v_1_ (atom (vector))]
///                 (fn
///                     ([] (g))
///                     ([x]
///                         (let [x (when (seq (deref v_1_)) => x
///                                     (let [v (deref v_1_) _ (swap_4_ v_1_ empty)]
///                                         (unreduced (g x v))
///                                     )
///                                 )]
///                             (g x)
///                         )
///                     )
///                     ([x y]
///                         (swap_4_ v_1_ conj y)
///                         (when (= (count (deref v_1_)) n) => x
///                             (let [v (deref v_1_) _ (swap_4_ v_1_ empty)]
///                                 (g x v)
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([n s] (partition_all n n s))
///     ([n step s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (let [p (doall (take n s))]
///                     (cons p (partition_all n step (nthnext s step)))
///                 )
///             )
///         )
///     )
/// )

/// (defn partition_by
///     ([f]
///         (fn [g]
///             (let [l_1_ (atom (vector)) p_1_ (atom _0_0_none)]
///                 (fn
///                     ([] (g))
///                     ([s]
///                         (let [s (when (seq (deref l_1_)) => s
///                                     (let [l (deref l_1_) _ (swap_4_ l_1_ empty)]
///                                         (unreduced (g s l))
///                                     )
///                                 )]
///                             (g s)
///                         )
///                     )
///                     ([s x]
///                         (let [p (deref p_1_) y (f x) _ (reset_4_ p_1_ y)]
///                             (if (or (identical_9_ p _0_0_none) (= y p))
///                                 (do
///                                     (swap_4_ l_1_ conj x)
///                                     s
///                                 )
///                                 (let [l (deref l_1_) _ (swap_4_ l_1_ empty) s_1_ (g s l)]
///                                     (when_not (reduced_9_ s_1_)
///                                         (swap_4_ l_1_ conj x)
///                                     )
///                                     s_1_
///                                 )
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([f s]
///         (lazy_seq
///             (when_some [s (seq s)]
///                 (let [x (first s) fx (f x)
///                       s_1_ (cons x (take_while #(= (f %) fx) (next s)))]
///                     (cons s_1_ (partition_by f (drop (count s_1_) s)))
///                 )
///             )
///         )
///     )
/// )

/// (defn repeatedly
///     ([f] (lazy_seq (cons (f) (repeatedly f))))
///     ([n f] (take n (repeatedly f)))
/// )

/// (declare hash_set)
/// (declare contains_9_)

/// (defn distinct
///     ([]
///         (fn [g]
///             (let [seen (atom (hash_set))]
///                 (fn
///                     ([] (g))
///                     ([s] (g s))
///                     ([s x]
///                         (when_not (contains_9_ (deref seen) x) => s
///                             (swap_4_ seen conj x)
///                             (g s x)
///                         )
///                     )
///                 )
///             )
///         )
///     )
///     ([s]
///         (letfn [(step_ [s seen]
///                     (lazy_seq
///                         ((fn [[x _0_as s] seen]
///                             (when_some [s (seq s)]
///                                 (when_not (contains_9_ seen x) => (recur (next s) seen)
///                                     (cons x (step_ (next s) (conj seen x)))
///                                 )
///                             ))
///                             s seen
///                         )
///                     )
///                 )]
///             (step_ s (hash_set))
///         )
///     )
/// )

/// (defn distinct_9_
///     ([x] true)
///     ([x y] (not (= x y)))
///     ([x y & z]
///         (and (distinct_9_ x y)
///             (loop_when [s_8_ #{x y} z z] z => true
///                 (and (not (contains_9_ s_8_ (first z)))
///                     (recur (conj s_8_ (first z)) (next z))
///                 )
///             )
///         )
///     )
/// )
}

namespace arbace {

// (about #_"APersistentMap"
///     (defn #_"IPersistentCollection" APersistentMap_2_conj [#_"APersistentMap" this, #_"Object" o]
///         (condp satisfies_9_ o
///             IMapEntry
///                 (assoc this (key o) (val o))
///             IPersistentVector
///                 (when (= (count o) 2) => (throw "vector arg to map conj must be a pair")
///                     (assoc this (nth o 0) (nth o 1))
///                 )
///             #_else
///                 (loop_when [this this #_"ISeq" s (seq o)] (some_9_ s) => this
///                     (let [#_"pair" e (first s)]
///                         (recur (assoc this (key e) (val e)) (next s))
///                     )
///                 )
///         )
///     )

///     (defn #_"bool" APersistentMap_2_equals [#_"APersistentMap" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (and (map_9_ that) (= (count that) (count this))
///                 (loop_when [#_"ISeq" s (seq this)] (some_9_ s) => true
///                     (let [#_"pair" e (first s) #_"Object" k (key e)]
///                         (and (contains_9_ that k) (= (val e) (get that k))
///                             (recur (next s))
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Object" APersistentMap_2_invoke
///         ([#_"APersistentMap" this, #_"Object" key] (get this key))
///         ([#_"APersistentMap" this, #_"Object" key, #_"Object" not_found] (get this key not_found))
///     )
// )
}

namespace arbace {

// (about #_"APersistentSet"
///     (defn #_"bool" APersistentSet_2_equals [#_"APersistentSet" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (and (set_9_ that) (= (count this) (count that))
///                 (loop_when [#_"ISeq" s (seq that)] (some_9_ s) => true
///                     (and (contains_9_ this (first s)) (recur (next s)))
///                 )
///             )
///         )
///     )

///     (defn #_"Object" APersistentSet_2_invoke
///         ([#_"APersistentSet" this, #_"Object" key] (get this key))
///         ([#_"APersistentSet" this, #_"Object" key, #_"Object" not_found] (get this key not_found))
///     )
// )
}

namespace arbace {

// (about #_"VSeq"
///     (declare VSeq_2_seq VSeq_2_first VSeq_2_next)

///     (defq VSeq [#_"IPersistentMap" _meta, #_"vector" v, #_"int" i] SeqForm
///         clojure.lang.ISeq (seq [_] (VSeq_2_seq _)) (first [_] (VSeq_2_first _)) (next [_] (VSeq_2_next _)) (more [_] (or (VSeq_2_next _) ()))
///         clojure.lang.Sequential
///     )

///     #_inherit
///     (defm VSeq ASeq)

///     (defn #_"VSeq" VSeq_1_new
///         ([#_"vector" v, #_"int" i] (VSeq_1_new nil, v, i))
///         ([#_"IPersistentMap" meta, #_"vector" v, #_"int" i]
///             (new_8_ VSeq_1_class (anew [meta, v, i]))
///         )
///     )

///     (defn_ #_"VSeq" VSeq_2_withMeta [#_"VSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (VSeq_1_new meta, (_0_v this), (_0_i this))
///         )
///     )

///     (defn_ #_"ISeq" VSeq_2_seq [#_"VSeq" this]
///         this
///     )

///     (defn_ #_"Object" VSeq_2_first [#_"VSeq" this]
///         (nth (_0_v this) (_0_i this))
///     )

///     (defn_ #_"ISeq" VSeq_2_next [#_"VSeq" this]
///         (when (< (inc (_0_i this)) (count (_0_v this)))
///             (VSeq_1_new (_0_v this), (inc (_0_i this)))
///         )
///     )

///     (defn_ #_"int" VSeq_2_count [#_"VSeq" this]
///         (- (count (_0_v this)) (_0_i this))
///     )

///     (defn_ #_"Object" VSeq_2_reduce
///         ([#_"VSeq" this, #_"IFn" f]
///             (let [#_"vector" v (_0_v this) #_"int" i (_0_i this) #_"int" n (count v)]
///                 (loop_when [#_"Object" r (nth v i) i (inc i)] (< i n) => r
///                     (let_when [r (f r (nth v i))] (reduced_9_ r) => (recur r (inc i))
///                         (deref r)
///                     )
///                 )
///             )
///         )
///         ([#_"VSeq" this, #_"IFn" f, #_"Object" r]
///             (let [#_"vector" v (_0_v this) #_"int" i (_0_i this) #_"int" n (count v)]
///                 (loop_when [r (f r (nth v i)) i (inc i)] (< i n) => (if (reduced_9_ r) (deref r) r)
///                     (when (reduced_9_ r) => (recur (f r (nth v i)) (inc i))
///                         (deref r)
///                     )
///                 )
///             )
///         )
///     )

///     (defm VSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm VSeq IObj
///         (IObj_3_withMeta => VSeq_2_withMeta)
///     )

///     (defm VSeq Sequential)

///     (defm VSeq Seqable
///         (Seqable_3_seq => VSeq_2_seq)
///     )

///     (defm VSeq ISeq
///         (ISeq_3_first => VSeq_2_first)
///         (ISeq_3_next => VSeq_2_next)
///     )

///     (defm VSeq Counted
///         (Counted_3_count => VSeq_2_count)
///     )

///     (defm VSeq IReduce
///         (IReduce_3_reduce => VSeq_2_reduce)
///     )

///     (defm VSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm VSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

// (about #_"RSeq"
///     (declare RSeq_2_seq RSeq_2_first RSeq_2_next)

///     (defq RSeq [#_"IPersistentMap" _meta, #_"vector" v, #_"int" i] SeqForm
///         clojure.lang.ISeq (seq [_] (RSeq_2_seq _)) (first [_] (RSeq_2_first _)) (next [_] (RSeq_2_next _)) (more [_] (or (RSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm RSeq ASeq)

///     (defn #_"RSeq" RSeq_1_new
///         ([#_"vector" v, #_"int" i] (RSeq_1_new nil, v, i))
///         ([#_"IPersistentMap" meta, #_"vector" v, #_"int" i]
///             (new_8_ RSeq_1_class (anew [meta, v, i]))
///         )
///     )

///     (defn_ #_"RSeq" RSeq_2_withMeta [#_"RSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (RSeq_1_new meta, (_0_v this), (_0_i this))
///         )
///     )

///     (defn_ #_"ISeq" RSeq_2_seq [#_"RSeq" this]
///         this
///     )

///     (defn_ #_"Object" RSeq_2_first [#_"RSeq" this]
///         (nth (_0_v this) (_0_i this))
///     )

///     (defn_ #_"ISeq" RSeq_2_next [#_"RSeq" this]
///         (when (pos_9_ (_0_i this))
///             (RSeq_1_new (_0_v this), (dec (_0_i this)))
///         )
///     )

///     (defn_ #_"int" RSeq_2_count [#_"RSeq" this]
///         (inc (_0_i this))
///     )

///     (defm RSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm RSeq IObj
///         (IObj_3_withMeta => RSeq_2_withMeta)
///     )

///     (defm RSeq Sequential)

///     (defm RSeq Seqable
///         (Seqable_3_seq => RSeq_2_seq)
///     )

///     (defm RSeq ISeq
///         (ISeq_3_first => RSeq_2_first)
///         (ISeq_3_next => RSeq_2_next)
///     )

///     (defm RSeq Counted
///         (Counted_3_count => RSeq_2_count)
///     )

///     (defm RSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm RSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )
}

namespace arbace {

// (about #_"AMapEntry"
///     (defn #_"Object" AMapEntry_2_nth
///         ([#_"AMapEntry" this, #_"int" i]
///             (case_4_ i 0 (IMapEntry_3_key this) 1 (IMapEntry_3_val this) (throw "index is out of bounds"))
///         )
///         ([#_"AMapEntry" this, #_"int" i, #_"Object" not_found]
///             (case_4_ i 0 (IMapEntry_3_key this) 1 (IMapEntry_3_val this) not_found)
///         )
///     )

///     (defn #_"int" AMapEntry_2_count [#_"AMapEntry" this]
///         2
///     )

///     (defn #_"ISeq" AMapEntry_2_seq [#_"AMapEntry" this]
///         (VSeq_1_new this, 0)
///     )

///     (defn #_"ISeq" AMapEntry_2_rseq [#_"AMapEntry" this]
///         (RSeq_1_new this, 1)
///     )

///     (defn #_"bool" AMapEntry_2_equals [#_"AMapEntry" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (cond
///                 (vector_9_ that)
///                     (and (= (count that) 2) (= (nth that 0) (IMapEntry_3_key this)) (= (nth that 1) (IMapEntry_3_val this)))
///                 (sequential_9_ that)
///                     (loop_when [#_"int" i 0 #_"ISeq" s (seq that)] (< i 2) => (nil_9_ s)
///                         (recur_when (and (some_9_ s) (= (Indexed_3_nth this, i) (first s))) [(inc i) (next s)] => false)
///                     )
///                 _0_else
///                     false
///             )
///         )
///     )

///     (defn #_"int" AMapEntry_2_hash [#_"AMapEntry" this]
///         (loop_when [#_"int" hash (int 1) #_"int" i (int 0)] (< i 2) => (Murmur3_1_mixCollHash hash, i)
///             (recur (+ (* (int 31) hash) (f_1_hash (Indexed_3_nth this, i))) (inc i))
///         )
///     )

///     (defn #_"int" AMapEntry_2_compareTo [#_"AMapEntry" this, #_"IPersistentVector" that]
///         (when_not (identical_9_ this that) => 0
///             (let [#_"int" m (count that)]
///                 (cond (< 2 m) -1 (< m 2) 1
///                     _0_else
///                         (loop_when [#_"int" i 0] (< i 2) => 0
///                             (let [#_"int" cmp (compare (Indexed_3_nth this, i) (Indexed_3_nth that, i))]
///                                 (recur_when (zero_9_ cmp) [(inc i)] => cmp)
///                             )
///                         )
///                 )
///             )
///         )
///     )
// )
}

namespace arbace {

// (about #_"MapEntry"
///     (defq MapEntry [#_"Object" k, #_"Object" v] VecForm
///         java.util.Map$Entry (getKey [_] (_0_k _)) (getValue [_] (_0_v _))
///     )

///     #_inherit
///     (defm MapEntry AMapEntry APersistentVector AFn)

///     (defn_ #_"MapEntry" MapEntry_1_new [#_"Object" k, #_"Object" v]
///         (new_8_ MapEntry_1_class (anew [k, v]))
///     )

///     (defm MapEntry IMapEntry
///         (IMapEntry_3_key => _0_k)
///         (IMapEntry_3_val => _0_v)
///     )

///     (defm MapEntry Sequential)

///     (defm MapEntry Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm MapEntry Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm MapEntry Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm MapEntry Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm MapEntry IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm MapEntry Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm MapEntry Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )
}

namespace arbace {

// (about #_"ATransientMap"
///     (defn #_"Object" ATransientMap_2_invoke
///         ([#_"ATransientMap" this, #_"Object" key] (get this key))
///         ([#_"ATransientMap" this, #_"Object" key, #_"Object" not_found] (get this key not_found))
///     )

///     (def_ #_"Object" ATransientMap_1_NOT_FOUND (anew 0))

///     (defn #_"bool" ATransientMap_2_containsKey [#_"ATransientMap" this, #_"Object" key]
///         (not (identical_9_ (get this key ATransientMap_1_NOT_FOUND) ATransientMap_1_NOT_FOUND))
///     )

///     (defn #_"IMapEntry" ATransientMap_2_entryAt [#_"ATransientMap" this, #_"Object" key]
///         (let [#_"Object" v (get this key ATransientMap_1_NOT_FOUND)]
///             (when_not (identical_9_ v ATransientMap_1_NOT_FOUND)
///                 (MapEntry_1_new key, v)
///             )
///         )
///     )
// )
}

namespace arbace {

// (about #_"ATransientSet"
///     (defn #_"Object" ATransientSet_2_invoke
///         ([#_"ATransientSet" this, #_"Object" key] (get this key))
///         ([#_"ATransientSet" this, #_"Object" key, #_"Object" not_found] (get this key not_found))
///     )
// )
}

namespace arbace {

// (about #_"EmptyList"
///     (declare EmptyList_2_seq EmptyList_2_first EmptyList_2_next EmptyList_2_conj EmptyList_2_empty EmptyList_2_equals)

///     (defq EmptyList [#_"IPersistentMap" _meta] SeqForm
///         clojure.lang.ISeq (seq [_] (EmptyList_2_seq _)) (first [_] (EmptyList_2_first _)) (next [_] (EmptyList_2_next _)) (more [_] (or (EmptyList_2_next _) ()))
///         clojure.lang.IPersistentCollection (cons [_, o] (EmptyList_2_conj _, o)) (empty [_] (EmptyList_2_empty _)) (equiv [_, o] (EmptyList_2_equals _, o))
///     )

///     (defn #_"EmptyList" EmptyList_1_new [#_"IPersistentMap" meta]
///         (new_8_ EmptyList_1_class (anew [meta]))
///     )

///     (defn_ #_"EmptyList" EmptyList_2_withMeta [#_"EmptyList" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (EmptyList_1_new meta)
///         )
///     )

///     (def #_"int" EmptyList_1_HASH (Murmur3_1_hashOrdered nil))

///     (defn_ #_"int" EmptyList_2_hash [#_"EmptyList" this]
///         EmptyList_1_HASH
///     )

///     (defn_ #_"bool" EmptyList_2_equals [#_"EmptyList" this, #_"Object" that]
///         (and (sequential_9_ that) (nil_9_ (seq that)))
///     )

///     (defn_ #_"ISeq" EmptyList_2_seq [#_"EmptyList" this]
///         nil
///     )

///     (defn_ #_"Object" EmptyList_2_first [#_"EmptyList" this]
///         nil
///     )

///     (defn_ #_"ISeq" EmptyList_2_next [#_"EmptyList" this]
///         nil
///     )

///     (defn_ #_"int" EmptyList_2_count [#_"EmptyList" this]
///         0
///     )

///     (declare PersistentList_1_new)

///     (defn_ #_"PersistentList" EmptyList_2_conj [#_"EmptyList" this, #_"Object" o]
///         (PersistentList_1_new (_0__meta this), o, nil, 1)
///     )

///     (defn_ #_"EmptyList" EmptyList_2_empty [#_"EmptyList" this]
///         this
///     )

///     (defn_ #_"Object" EmptyList_2_peek [#_"EmptyList" this]
///         nil
///     )

///     (defn_ #_"IPersistentList" EmptyList_2_pop [#_"EmptyList" this]
///         (throw "can't pop the empty list")
///     )

///     (defm EmptyList IPersistentList Sequential)

///     (defm EmptyList IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm EmptyList IObj
///         (IObj_3_withMeta => EmptyList_2_withMeta)
///     )

///     (defm EmptyList Hashed
///         (Hashed_3_hash => EmptyList_2_hash)
///     )

///     (defm EmptyList IObject
///         (IObject_3_equals => EmptyList_2_equals)
///     )

///     (defm EmptyList Seqable
///         (Seqable_3_seq => EmptyList_2_seq)
///     )

///     (defm EmptyList ISeq
///         (ISeq_3_first => EmptyList_2_first)
///         (ISeq_3_next => EmptyList_2_next)
///     )

///     (defm EmptyList Counted
///         (Counted_3_count => EmptyList_2_count)
///     )

///     (defm EmptyList IPersistentCollection
///         (IPersistentCollection_3_conj => EmptyList_2_conj)
///         (IPersistentCollection_3_empty => EmptyList_2_empty)
///     )

///     (defm EmptyList IPersistentStack
///         (IPersistentStack_3_peek => EmptyList_2_peek)
///         (IPersistentStack_3_pop => EmptyList_2_pop)
///     )
// )

// (about #_"PersistentList"
///     (declare PersistentList_2_seq PersistentList_2_conj PersistentList_2_empty)

///     (defq PersistentList [#_"IPersistentMap" _meta, #_"Object" car, #_"IPersistentList" cdr, #_"int" cnt] SeqForm
///         clojure.lang.ISeq (seq [_] (PersistentList_2_seq _)) (first [_] (_0_car _)) (next [_] (_0_cdr _)) (more [_] (or (_0_cdr _) ()))
///         clojure.lang.IPersistentCollection (cons [_, o] (PersistentList_2_conj _, o)) (empty [_] (PersistentList_2_empty _)) (equiv [_, o] (ASeq_2_equals _, o)) (count [_] (_0_cnt _))
///     )

///     #_inherit
///     (defm PersistentList ASeq)

///     (defn #_"PersistentList" PersistentList_1_new
///         ([#_"Object" car] (PersistentList_1_new nil, car, nil, 1))
///         ([#_"IPersistentMap" meta, #_"Object" car, #_"IPersistentList" cdr, #_"int" cnt]
///             (new_8_ PersistentList_1_class (anew [meta, car, cdr, cnt]))
///         )
///     )

///     (def #_"EmptyList" PersistentList_1_EMPTY (EmptyList_1_new nil))

///     (declare reverse)

///     (defn #_"PersistentList" PersistentList_1_create [#_"Reversible" init]
///         (into PersistentList_1_EMPTY (if (satisfies_9_ Reversible init) (rseq init) (reverse init)))
///     )

///     (defn_ #_"PersistentList" PersistentList_2_withMeta [#_"PersistentList" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentList_1_new meta, (_0_car this), (_0_cdr this), (_0_cnt this))
///         )
///     )

///     (defn_ #_"ISeq" PersistentList_2_seq [#_"PersistentList" this]
///         this
///     )

///     (defn_ #_"PersistentList" PersistentList_2_conj [#_"PersistentList" this, #_"Object" o]
///         (PersistentList_1_new (_0__meta this), o, this, (inc (_0_cnt this)))
///     )

///     (defn_ #_"PersistentList" PersistentList_2_empty [#_"PersistentList" this]
///         (with_meta PersistentList_1_EMPTY (_0__meta this))
///     )

///     (defn_ #_"IPersistentList" PersistentList_2_pop [#_"PersistentList" this]
///         (or (_0_cdr this) (with_meta PersistentList_1_EMPTY (_0__meta this)))
///     )

///     (defn_ #_"Object" PersistentList_2_reduce
///         ([#_"PersistentList" this, #_"IFn" f]
///             (loop_when [#_"Object" r (_0_car this) #_"IPersistentList" l (_0_cdr this)] (some_9_ l) => r
///                 (let [r (f r (_0_car l))]
///                     (if (reduced_9_ r) (deref r) (recur r (_0_cdr l)))
///                 )
///             )
///         )
///         ([#_"PersistentList" this, #_"IFn" f, #_"Object" r]
///             (loop_when [r (f r (_0_car this)) #_"IPersistentList" l (_0_cdr this)] (some_9_ l) => (if (reduced_9_ r) (deref r) r)
///                 (if (reduced_9_ r) (deref r) (recur (f r (_0_car l)) (_0_cdr l)))
///             )
///         )
///     )

///     (defm PersistentList IPersistentList Sequential)

///     (defm PersistentList IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentList IObj
///         (IObj_3_withMeta => PersistentList_2_withMeta)
///     )

///     (defm PersistentList Seqable
///         (Seqable_3_seq => PersistentList_2_seq)
///     )

///     (defm PersistentList ISeq
///         (ISeq_3_first => _0_car)
///         (ISeq_3_next => _0_cdr)
///     )

///     (defm PersistentList Counted
///         (Counted_3_count => _0_cnt)
///     )

///     (defm PersistentList IPersistentCollection
///         (IPersistentCollection_3_conj => PersistentList_2_conj)
///         (IPersistentCollection_3_empty => PersistentList_2_empty)
///     )

///     (defm PersistentList IPersistentStack
///         (IPersistentStack_3_peek => _0_car)
///         (IPersistentStack_3_pop => PersistentList_2_pop)
///     )

///     (defm PersistentList IReduce
///         (IReduce_3_reduce => PersistentList_2_reduce)
///     )

///     (defm PersistentList Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm PersistentList IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

/// (defn list
///     ([] PersistentList_1_EMPTY)
///     ([& s] (PersistentList_1_create s))
/// )

/// (defn reverse [s] (into (list) s))
}

namespace arbace {

// (about #_"MSeq"
///     (declare MSeq_2_seq MSeq_2_first MSeq_2_next)

///     (defq MSeq [#_"IPersistentMap" _meta, #_"array" a, #_"int" i] SeqForm
///         clojure.lang.ISeq (seq [_] (MSeq_2_seq _)) (first [_] (MSeq_2_first _)) (next [_] (MSeq_2_next _)) (more [_] (or (MSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm MSeq ASeq)

///     (defn #_"MSeq" MSeq_1_new
///         ([#_"array" a, #_"int" i] (MSeq_1_new nil, a, i))
///         ([#_"IPersistentMap" meta, #_"array" a, #_"int" i]
///             (new_8_ MSeq_1_class (anew [meta, a, i]))
///         )
///     )

///     (defn_ #_"MSeq" MSeq_2_withMeta [#_"MSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (MSeq_1_new meta, (_0_a this), (_0_i this))
///         )
///     )

///     (defn_ #_"ISeq" MSeq_2_seq [#_"MSeq" this]
///         this
///     )

///     (defn_ #_"pair" MSeq_2_first [#_"MSeq" this]
///         (MapEntry_1_new (aget (_0_a this) (_0_i this)), (aget (_0_a this) (inc (_0_i this))))
///     )

///     (defn_ #_"ISeq" MSeq_2_next [#_"MSeq" this]
///         (when (< (+ (_0_i this) 2) (alength (_0_a this)))
///             (MSeq_1_new (_0_a this), (+ (_0_i this) 2))
///         )
///     )

///     (defn_ #_"int" MSeq_2_count [#_"MSeq" this]
///         (quot (- (alength (_0_a this)) (_0_i this)) 2)
///     )

///     (defm MSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm MSeq IObj
///         (IObj_3_withMeta => MSeq_2_withMeta)
///     )

///     (defm MSeq Sequential)

///     (defm MSeq Seqable
///         (Seqable_3_seq => MSeq_2_seq)
///     )

///     (defm MSeq ISeq
///         (ISeq_3_first => MSeq_2_first)
///         (ISeq_3_next => MSeq_2_next)
///     )

///     (defm MSeq Counted
///         (Counted_3_count => MSeq_2_count)
///     )

///     (defm MSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm MSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

// (about #_"TransientArrayMap"
///     (defq TransientArrayMap [#_"thread'" edit, #_"array" array, #_"int" cnt] #_"MapForm")

///     #_inherit
///     (defm TransientArrayMap ATransientMap AFn)

///     (declare PersistentArrayMap_1_HASHTABLE_THRESHOLD)

///     (defn #_"TransientArrayMap" TransientArrayMap_1_new [#_"array" a]
///         (let [#_"int" n (alength a) #_"int" m (max PersistentArrayMap_1_HASHTABLE_THRESHOLD n)]
///             (new_8_ TransientArrayMap_1_class (anew [(atom (thread)), (-> (anew m) (acopy_4_ 0 a 0 n)), n]))
///         )
///     )

///     (defn_ #_"void" TransientArrayMap_2_assert_editable [#_"TransientArrayMap" this]
///         (or (deref (_0_edit this)) (throw "transient used after persistent! call"))
///         nil
///     )

///     (defn_ #_"int" TransientArrayMap_2_count [#_"TransientArrayMap" this]
///         (TransientArrayMap_2_assert_editable this)
///         (quot (_0_cnt this) 2)
///     )

///     (defn_ #_"int" TransientArrayMap_1_index_of [#_"array" a, #_"int" n, #_"Object" key]
///         (loop_when [#_"int" i 0] (< i n) => -1
///             (if (= (aget a i) key) i (recur (+ i 2)))
///         )
///     )

///     (defn_ #_"Object" TransientArrayMap_2_valAt
///         ([#_"TransientArrayMap" this, #_"Object" key] (TransientArrayMap_2_valAt this, key, nil))
///         ([#_"TransientArrayMap" this, #_"Object" key, #_"Object" not_found]
///             (TransientArrayMap_2_assert_editable this)
///             (let [
///                 #_"array" a (_0_array this) #_"int" n (_0_cnt this) #_"int" i (TransientArrayMap_1_index_of a, n, key)
///             ]
///                 (if (< -1 i) (aget a (inc i)) not_found)
///             )
///         )
///     )

///     (declare PersistentHashMap_1_create_1a)

///     (defn_ #_"ITransientMap" TransientArrayMap_2_assoc_4_ [#_"TransientArrayMap" this, #_"Object" key, #_"Object" val]
///         (TransientArrayMap_2_assert_editable this)
///         (let [
///             #_"array" a (_0_array this) #_"int" n (_0_cnt this) #_"int" i (TransientArrayMap_1_index_of a, n, key)
///         ]
///             (cond
///                 (< -1 i)
///                     (do
///                         (aset_4_ a (inc i) val)
///                         this
///                     )
///                 (< n (alength a))
///                     (do
///                         (aset_4_ a      n  key)
///                         (aset_4_ a (inc n) val)
///                         (qset_4_ this _0_cnt (+ n 2))
///                     )
///                 _0_else
///                     (-> (PersistentHashMap_1_create_1a a) (transient) (assoc_4_ key val))
///             )
///         )
///     )

///     (defn_ #_"ITransientMap" TransientArrayMap_2_dissoc_4_ [#_"TransientArrayMap" this, #_"Object" key]
///         (TransientArrayMap_2_assert_editable this)
///         (let [
///             #_"array" a (_0_array this) #_"int" n (_0_cnt this) #_"int" i (TransientArrayMap_1_index_of a, n, key)
///         ]
///             (when (< -1 i) => this
///                 (let [
///                     n (- n 2)
///                 ]
///                     (when (< -1 n)
///                         (aset_4_ a      i  (aget a      n))
///                         (aset_4_ a (inc i) (aget a (inc n)))
///                     )
///                     (qset_4_ this _0_cnt n)
///                 )
///             )
///         )
///     )

///     (defn_ #_"ITransientMap" TransientArrayMap_2_conj_4_ [#_"TransientArrayMap" this, #_"pair" o]
///         (TransientArrayMap_2_assert_editable this)
///         (condp satisfies_9_ o
///             IMapEntry
///                 (assoc_4_ this (key o) (val o))
///             IPersistentVector
///                 (when (= (count o) 2) => (throw "vector arg to map conj must be a pair")
///                     (assoc_4_ this (nth o 0) (nth o 1))
///                 )
///             #_else
///                 (loop_when [this this #_"ISeq" s (seq o)] (some_9_ s) => this
///                     (let [#_"pair" e (first s)]
///                         (recur (assoc_4_ this (key e) (val e)) (next s))
///                     )
///                 )
///         )
///     )

///     (defn_ #_"IPersistentMap" TransientArrayMap_2_persistent_4_ [#_"TransientArrayMap" this]
///         (TransientArrayMap_2_assert_editable this)
///         (reset_4_ (_0_edit this) nil)
///         (let [
///             #_"int" n (_0_cnt this)
///         ]
///             (PersistentArrayMap_1_new (-> (anew n) (acopy_4_ 0 (_0_array this) 0 n)))
///         )
///     )

///     (defm TransientArrayMap Counted
///         (Counted_3_count => TransientArrayMap_2_count)
///     )

///     (defm TransientArrayMap ILookup
///         (ILookup_3_valAt => TransientArrayMap_2_valAt)
///     )

///     (defm TransientArrayMap IFn
///         (IFn_3_invoke => ATransientMap_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm TransientArrayMap ITransientAssociative
///         (ITransientAssociative_3_assoc_4_ => TransientArrayMap_2_assoc_4_)
///         (ITransientAssociative_3_containsKey => ATransientMap_2_containsKey)
///         (ITransientAssociative_3_entryAt => ATransientMap_2_entryAt)
///     )

///     (defm TransientArrayMap ITransientMap
///         (ITransientMap_3_dissoc_4_ => TransientArrayMap_2_dissoc_4_)
///     )

///     (defm TransientArrayMap ITransientCollection
///         (ITransientCollection_3_conj_4_ => TransientArrayMap_2_conj_4_)
///         (ITransientCollection_3_persistent_4_ => TransientArrayMap_2_persistent_4_)
///     )
// )

// (about #_"PersistentArrayMap"
///     (declare PersistentArrayMap_2_seq PersistentArrayMap_2_assoc PersistentArrayMap_2_containsKey)

///     (defq PersistentArrayMap [#_"IPersistentMap" _meta, #_"array" array] MapForm
///         clojure.lang.Seqable (seq [_] (PersistentArrayMap_2_seq _))
///         clojure.lang.Associative (assoc [_, key, val] (PersistentArrayMap_2_assoc _, key, val)) (containsKey [_, key] (PersistentArrayMap_2_containsKey _, key))
///     )

///     #_inherit
///     (defm PersistentArrayMap APersistentMap AFn)

///     (defn #_"PersistentArrayMap" PersistentArrayMap_1_new
///         ([#_"array" a] (PersistentArrayMap_1_new nil, a))
///         ([#_"IPersistentMap" meta, #_"array" a]
///             (new_8_ PersistentArrayMap_1_class (anew [meta, (or a (anew 0))]))
///         )
///     )

///     (def #_"PersistentArrayMap" PersistentArrayMap_1_EMPTY (PersistentArrayMap_1_new nil))

///     (defn #_"PersistentArrayMap" PersistentArrayMap_2_create [#_"PersistentArrayMap" this, #_"array" init]
///         (PersistentArrayMap_1_new (_0__meta this), init)
///     )

///     (defn #_"PersistentArrayMap" PersistentArrayMap_1_createWithCheck [#_"array" init]
///         (loop_when_recur [#_"int" i 0] (< i (alength init)) [(+ i 2)]
///             (loop_when_recur [#_"int" j (+ i 2)] (< j (alength init)) [(+ j 2)]
///                 (when (= (aget init i) (aget init j))
///                     (throw (str "duplicate key: " (aget init i)))
///                 )
///             )
///         )
///         (PersistentArrayMap_1_new init)
///     )

///     (defn #_"PersistentArrayMap" PersistentArrayMap_1_createAsIfByAssoc [#_"array" init]
///         (when (odd_9_ (alength init))
///             (throw (str "no value supplied for key: " (aget init (dec (alength init)))))
///         )
///         (let [#_"int" n
///                 (loop_when [n 0 #_"int" i 0] (< i (alength init)) => n
///                     (let [#_"bool" dup_9_
///                             (loop_when [dup_9_ false #_"int" j 0] (< j i) => dup_9_
///                                 (or (= (aget init i) (aget init j))
///                                     (recur dup_9_ (+ j 2))
///                                 )
///                             )]
///                         (recur (if dup_9_ n (+ n 2)) (+ i 2))
///                     )
///                 )
///               init
///                 (when (< n (alength init)) => init
///                     (let [#_"array" nodups (anew n)
///                           #_"int" m
///                             (loop_when [m 0 #_"int" i 0] (< i (alength init)) => m
///                                 (let [#_"bool" dup_9_
///                                         (loop_when [dup_9_ false #_"int" j 0] (< j m) => dup_9_
///                                             (or (= (aget init i) (aget nodups j))
///                                                 (recur dup_9_ (+ j 2))
///                                             )
///                                         )
///                                       m (when_not dup_9_ => m
///                                             (let [#_"int" j
///                                                     (loop_when [j (- (alength init) 2)] (<= i j) => j
///                                                         (if (= (aget init i) (aget init j))
///                                                             j
///                                                             (recur (- j 2))
///                                                         )
///                                                     )]
///                                                 (aset_4_ nodups m (aget init i))
///                                                 (aset_4_ nodups (inc m) (aget init (inc j)))
///                                                 (+ m 2)
///                                             )
///                                         )]
///                                     (recur m (+ i 2))
///                                 )
///                             )]
///                         (when (= m n) => (throw (str "internal error: m=" m))
///                             nodups
///                         )
///                     )
///                 )]
///             (PersistentArrayMap_1_new init)
///         )
///     )

///     (defn_ #_"PersistentArrayMap" PersistentArrayMap_2_withMeta [#_"PersistentArrayMap" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentArrayMap_1_new meta, (_0_array this))
///         )
///     )

///     (defn_ #_"int" PersistentArrayMap_2_count [#_"PersistentArrayMap" this]
///         (quot (alength (_0_array this)) 2)
///     )

///     (defn_ #_"int" PersistentArrayMap_1_index_of [#_"array" a, #_"Object" key]
///         (loop_when [#_"int" i 0] (< i (alength a)) => -1
///             (if (= (aget a i) key) i (recur (+ i 2)))
///         )
///     )

///     (defn_ #_"Object" PersistentArrayMap_2_valAt
///         ([#_"PersistentArrayMap" this, #_"Object" key] (PersistentArrayMap_2_valAt this, key, nil))
///         ([#_"PersistentArrayMap" this, #_"Object" key, #_"Object" not_found]
///             (let [
///                 #_"array" a (_0_array this) #_"int" i (PersistentArrayMap_1_index_of a, key)
///             ]
///                 (if (< -1 i) (aget a (inc i)) not_found)
///             )
///         )
///     )

///     (def #_"int" PersistentArrayMap_1_HASHTABLE_THRESHOLD 16)

///     (defn_ #_"IPersistentMap" PersistentArrayMap_2_assoc [#_"PersistentArrayMap" this, #_"Object" key, #_"Object" val]
///         (let [
///             #_"array" a (_0_array this) #_"int" i (PersistentArrayMap_1_index_of a, key)
///         ]
///             (if (< -1 i)
///                 (if (= (aget a (inc i)) val)
///                     this
///                     (PersistentArrayMap_2_create this, (-> (aclone a) (aset_4_ (inc i) val)))
///                 )
///                 (if (< PersistentArrayMap_1_HASHTABLE_THRESHOLD (alength a))
///                     (-> (PersistentHashMap_1_create_1a a) (assoc key val) (with_meta (_0__meta this)))
///                     (let [
///                         #_"int" n (alength a)
///                         #_"array" a_1_ (anew (+ n 2))
///                         a_1_ (if (pos_9_ n) (acopy_4_ a_1_ 0 a 0 n) a_1_)
///                     ]
///                         (PersistentArrayMap_2_create this, (-> a_1_ (aset_4_ n key) (aset_4_ (inc n) val)))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"bool" PersistentArrayMap_2_containsKey [#_"PersistentArrayMap" this, #_"Object" key]
///         (< -1 (PersistentArrayMap_1_index_of (_0_array this), key))
///     )

///     (defn_ #_"pair" PersistentArrayMap_2_entryAt [#_"PersistentArrayMap" this, #_"Object" key]
///         (let [
///             #_"array" a (_0_array this) #_"int" i (PersistentArrayMap_1_index_of a, key)
///         ]
///             (when (< -1 i)
///                 (MapEntry_1_new (aget a i), (aget a (inc i)))
///             )
///         )
///     )

///     (defn_ #_"IPersistentMap" PersistentArrayMap_2_dissoc [#_"PersistentArrayMap" this, #_"Object" key]
///         (let [
///             #_"array" a (_0_array this) #_"int" i (PersistentArrayMap_1_index_of a, key)
///         ]
///             (when (< -1 i) => this
///                 (let_when [#_"int" n (- (alength a) 2)] (pos_9_ n) => (with_meta PersistentArrayMap_1_EMPTY (_0__meta this))
///                     (let [
///                         #_"array" a_1_ (-> (anew n) (acopy_4_ 0 a 0 i) (acopy_4_ i a (+ i 2) (- n i)))
///                     ]
///                         (PersistentArrayMap_2_create this, a_1_)
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentMap" PersistentArrayMap_2_empty [#_"PersistentArrayMap" this]
///         (with_meta PersistentArrayMap_1_EMPTY (_0__meta this))
///     )

///     (defn_ #_"ISeq" PersistentArrayMap_2_seq [#_"PersistentArrayMap" this]
///         (when (pos_9_ (alength (_0_array this)))
///             (MSeq_1_new (_0_array this), 0)
///         )
///     )

///     (defn_ #_"Object" PersistentArrayMap_2_kvreduce [#_"PersistentArrayMap" this, #_"IFn" f, #_"Object" r]
///         (let [#_"array" a (_0_array this) #_"int" n (alength a)]
///             (loop_when [r r #_"int" i 0] (< i n) => r
///                 (let [r (f r (aget a i), (aget a (inc i)))]
///                     (when_not (reduced_9_ r) => (deref r)
///                         (recur r (+ i 2))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"ITransientMap" PersistentArrayMap_2_asTransient [#_"PersistentArrayMap" this]
///         (TransientArrayMap_1_new (_0_array this))
///     )

///     (defm PersistentArrayMap IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentArrayMap IObj
///         (IObj_3_withMeta => PersistentArrayMap_2_withMeta)
///     )

///     (defm PersistentArrayMap Counted
///         (Counted_3_count => PersistentArrayMap_2_count)
///     )

///     (defm PersistentArrayMap ILookup
///         (ILookup_3_valAt => PersistentArrayMap_2_valAt)
///     )

///     (defm PersistentArrayMap IFn
///         (IFn_3_invoke => APersistentMap_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm PersistentArrayMap Associative
///         (Associative_3_assoc => PersistentArrayMap_2_assoc)
///         (Associative_3_containsKey => PersistentArrayMap_2_containsKey)
///         (Associative_3_entryAt => PersistentArrayMap_2_entryAt)
///     )

///     (defm PersistentArrayMap IPersistentMap
///         (IPersistentMap_3_dissoc => PersistentArrayMap_2_dissoc)
///     )

///     (defm PersistentArrayMap IPersistentCollection
///         (IPersistentCollection_3_conj => APersistentMap_2_conj)
///         (IPersistentCollection_3_empty => PersistentArrayMap_2_empty)
///     )

///     (defm PersistentArrayMap Seqable
///         (Seqable_3_seq => PersistentArrayMap_2_seq)
///     )

///     (defm PersistentArrayMap IKVReduce
///         (IKVReduce_3_kvreduce => PersistentArrayMap_2_kvreduce)
///     )

///     (defm PersistentArrayMap IEditableCollection
///         (IEditableCollection_3_asTransient => PersistentArrayMap_2_asTransient)
///     )

///     (defm PersistentArrayMap IObject
///         (IObject_3_equals => APersistentMap_2_equals)
///     )

///     (defm PersistentArrayMap Hashed
///         (Hashed_3_hash => Murmur3_1_hashUnordered)
///     )
// )

/// (defn array_map
///     ([] PersistentArrayMap_1_EMPTY)
///     ([& keyvals] (PersistentArrayMap_1_createAsIfByAssoc (anew keyvals)))
/// )
}

namespace arbace {

// (about #_"HSeq"
///     (declare HSeq_2_seq HSeq_2_first HSeq_2_next)

///     (defq HSeq [#_"IPersistentMap" _meta, #_"node[]" nodes, #_"int" i, #_"ISeq" s] SeqForm
///         clojure.lang.ISeq (seq [_] (HSeq_2_seq _)) (first [_] (HSeq_2_first _)) (next [_] (HSeq_2_next _)) (more [_] (or (HSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm HSeq ASeq)

///     (defn_ #_"HSeq" HSeq_1_new [#_"IPersistentMap" meta, #_"node[]" nodes, #_"int" i, #_"ISeq" s]
///         (new_8_ HSeq_1_class (anew [meta, nodes, i, s]))
///     )

///     (defn_ #_"HSeq" HSeq_2_withMeta [#_"HSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (HSeq_1_new meta, (_0_nodes this), (_0_i this), (_0_s this))
///         )
///     )

///     (defn_ #_"ISeq" HSeq_1_create_4 [#_"IPersistentMap" meta, #_"node[]" nodes, #_"int" i, #_"ISeq" s]
///         (when (nil_9_ s) => (HSeq_1_new meta, nodes, i, s)
///             (loop_when i (< i (alength nodes))
///                 (when_some [#_"node" node (aget nodes i)] => (recur (inc i))
///                     (when_some [s (INode_3_nodeSeq node)] => (recur (inc i))
///                         (HSeq_1_new meta, nodes, (inc i), s)
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"ISeq" HSeq_1_create_1 [#_"node[]" nodes]
///         (HSeq_1_create_4 nil, nodes, 0, nil)
///     )

///     (defn_ #_"ISeq" HSeq_2_seq [#_"HSeq" this]
///         this
///     )

///     (defn_ #_"pair" HSeq_2_first [#_"HSeq" this]
///         (first (_0_s this))
///     )

///     (defn_ #_"ISeq" HSeq_2_next [#_"HSeq" this]
///         (HSeq_1_create_4 nil, (_0_nodes this), (_0_i this), (next (_0_s this)))
///     )

///     (defm HSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm HSeq IObj
///         (IObj_3_withMeta => HSeq_2_withMeta)
///     )

///     (defm HSeq Sequential)

///     (defm HSeq Seqable
///         (Seqable_3_seq => HSeq_2_seq)
///     )

///     (defm HSeq ISeq
///         (ISeq_3_first => HSeq_2_first)
///         (ISeq_3_next => HSeq_2_next)
///     )

///     (defm HSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm HSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

// (about #_"NSeq"
///     (declare NSeq_2_seq NSeq_2_first NSeq_2_next)

///     (defq NSeq [#_"IPersistentMap" _meta, #_"array" a, #_"int" i, #_"ISeq" s] SeqForm
///         clojure.lang.ISeq (seq [_] (NSeq_2_seq _)) (first [_] (NSeq_2_first _)) (next [_] (NSeq_2_next _)) (more [_] (or (NSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm NSeq ASeq)

///     (defn #_"NSeq" NSeq_1_new
///         ([#_"array" a, #_"int" i] (NSeq_1_new nil, a, i, nil))
///         ([#_"IPersistentMap" meta, #_"array" a, #_"int" i, #_"ISeq" s]
///             (new_8_ NSeq_1_class (anew [meta, a, i, s]))
///         )
///     )

///     (defn_ #_"NSeq" NSeq_2_withMeta [#_"NSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (NSeq_1_new meta, (_0_a this), (_0_i this), (_0_s this))
///         )
///     )

///     (defn_ #_"ISeq" NSeq_1_create_3 [#_"array" a, #_"int" i, #_"ISeq" s]
///         (when (nil_9_ s) => (NSeq_1_new nil, a, i, s)
///             (loop_when i (< i (alength a))
///                 (when (nil_9_ (aget a i)) => (NSeq_1_new nil, a, i, nil)
///                     (or
///                         (when_some [#_"node" node (aget a (inc i))]
///                             (when_some [s (INode_3_nodeSeq node)]
///                                 (NSeq_1_new nil, a, (+ i 2), s)
///                             )
///                         )
///                         (recur (+ i 2))
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"ISeq" NSeq_1_create_1 [#_"array" a]
///         (NSeq_1_create_3 a, 0, nil)
///     )

///     (defn_ #_"ISeq" NSeq_2_seq [#_"NSeq" this]
///         this
///     )

///     (defn_ #_"pair" NSeq_2_first [#_"NSeq" this]
///         (if (some_9_ (_0_s this))
///             (first (_0_s this))
///             (MapEntry_1_new (aget (_0_a this) (_0_i this)), (aget (_0_a this) (inc (_0_i this))))
///         )
///     )

///     (defn_ #_"ISeq" NSeq_2_next [#_"NSeq" this]
///         (if (some_9_ (_0_s this))
///             (NSeq_1_create_3 (_0_a this), (_0_i this), (next (_0_s this)))
///             (NSeq_1_create_3 (_0_a this), (+ (_0_i this) 2), nil)
///         )
///     )

///     (defn #_"Object" NSeq_1_kvreduce [#_"array" a, #_"IFn" f, #_"Object" r]
///         (loop_when [r r #_"int" i 0] (< i (alength a)) => r
///             (let [#_"Object" k (aget a i) #_"value|node" v (aget a (inc i))
///                   r (cond
///                         (some_9_ k) (f r k v)
///                         (some_9_ v) (INode_3_kvreduce v, f, r)
///                         _0_else     r
///                     )]
///                 (when_not (reduced_9_ r) => r
///                     (recur r (+ i 2))
///                 )
///             )
///         )
///     )

///     (defm NSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm NSeq IObj
///         (IObj_3_withMeta => NSeq_2_withMeta)
///     )

///     (defm NSeq Sequential)

///     (defm NSeq Seqable
///         (Seqable_3_seq => NSeq_2_seq)
///     )

///     (defm NSeq ISeq
///         (ISeq_3_first => NSeq_2_first)
///         (ISeq_3_next => NSeq_2_next)
///     )

///     (defm NSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm NSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

// (about #_"PersistentHashMap"
///     (defn_ #_"int" PersistentHashMap_1_mask [#_"int" hash, #_"int" shift]
///         (bit_and (>>> hash shift) 0x1f)
///     )

///     (defn_ #_"int" PersistentHashMap_1_bitpos [#_"int" hash, #_"int" shift]
///         (int_4_ (<< 1 (PersistentHashMap_1_mask hash, shift)))
///     )

///     (defn_ #_"array" PersistentHashMap_1_cloneAndSet
///         ([#_"array" a, #_"int" i, #_"Object" x]                          (-> (aclone a) (aset_4_ i x)))
///         ([#_"array" a, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y] (-> (aclone a) (aset_4_ i x) (aset_4_ j y)))
///     )

///     (defn_ #_"array" PersistentHashMap_1_removePair [#_"array" a, #_"int" i]
///         (let [#_"int" n (- (alength a) 2) #_"int" m (* 2 i)]
///             (-> (anew n) (acopy_4_ 0 a 0 m) (acopy_4_ m a (+ m 2) (- n m)))
///         )
///     )
// )

// (about #_"ANode"
///     (defq ANode [#_"thread'" edit, #_"int" n, #_"node[]" a])

///     (defn #_"ANode" ANode_1_new [#_"thread'" edit, #_"int" n, #_"node[]" a]
///         (new_8_ ANode_1_class (anew [edit, n, a]))
///     )

///     (defn_ #_"ANode" ANode_2_ensureEditable [#_"ANode" this, #_"thread'" edit]
///         (when_not (identical_9_ (_0_edit this) edit) => this
///             (ANode_1_new edit, (_0_n this), (aclone (_0_a this)))
///         )
///     )

///     (defn_ #_"ANode" ANode_2_editAndSet [#_"ANode" this, #_"thread'" edit, #_"int" i, #_"node" node]
///         (let [#_"ANode" e (ANode_2_ensureEditable this, edit)]
///             (aset_4_ (_0_a e) i node)
///             e
///         )
///     )

///     (declare BNode_1_new)

///     (defn_ #_"node" ANode_2_pack [#_"ANode" this, #_"thread'" edit, #_"int" idx]
///         (let [#_"array" a_1_ (anew (* 2 (dec (_0_n this))))
///               [#_"int" bitmap #_"int" j]
///                 (loop_when [bitmap 0 j 1 #_"int" i 0] (< i idx) => [bitmap j]
///                     (let [[bitmap j]
///                             (when_some [#_"node" ai (aget (_0_a this) i)] => [bitmap j]
///                                 (aset_4_ a_1_ j ai)
///                                 [(bit_or bitmap (<< 1 i)) (+ j 2)]
///                             )]
///                         (recur bitmap j (inc i))
///                     )
///                 )
///               bitmap
///                 (loop_when [bitmap bitmap j j #_"int" i (inc idx)] (< i (alength (_0_a this))) => bitmap
///                     (let [[bitmap j]
///                             (when_some [#_"node" ai (aget (_0_a this) i)] => [bitmap j]
///                                 (aset_4_ a_1_ j ai)
///                                 [(bit_or bitmap (<< 1 i)) (+ j 2)]
///                             )]
///                         (recur bitmap j (inc i))
///                     )
///                 )]
///             (BNode_1_new edit, bitmap, a_1_)
///         )
///     )

///     (declare BNode_1_EMPTY)

///     (defn_ #_"node" ANode_2_assoc [#_"ANode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"bool'" addedLeaf]
///         (let [#_"int" i (PersistentHashMap_1_mask hash, shift) #_"node" ai (aget (_0_a this) i)]
///             (if (some_9_ ai)
///                 (let [#_"node" node (INode_3_assoc ai, (+ shift 5), hash, key, val, addedLeaf)]
///                     (when_not (= node ai) => this
///                         (ANode_1_new nil, (_0_n this), (PersistentHashMap_1_cloneAndSet (_0_a this), i, node))
///                     )
///                 )
///                 (let [#_"node" node (INode_3_assoc BNode_1_EMPTY, (+ shift 5), hash, key, val, addedLeaf)]
///                     (ANode_1_new nil, (inc (_0_n this)), (PersistentHashMap_1_cloneAndSet (_0_a this), i, node))
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" ANode_2_dissoc [#_"ANode" this, #_"int" shift, #_"int" hash, #_"Object" key]
///         (let_when [#_"int" i (PersistentHashMap_1_mask hash, shift) #_"node" ai (aget (_0_a this) i)] (some_9_ ai) => this
///             (let_when_not [#_"node" node (INode_3_dissoc ai, (+ shift 5), hash, key)] (= node ai) => this
///                 (cond
///                     (some_9_ node)     (ANode_1_new nil, (_0_n this), (PersistentHashMap_1_cloneAndSet (_0_a this), i, node))
///                     (<= (_0_n this) 8) (ANode_2_pack this, nil, i)
///                     _0_else            (ANode_1_new nil, (dec (_0_n this)), (PersistentHashMap_1_cloneAndSet (_0_a this), i, node))
///                 )
///             )
///         )
///     )

///     (defn_ #_"IMapEntry|value" ANode_2_find
///         ([#_"ANode" this, #_"int" shift, #_"int" hash, #_"Object" key]
///             (let [#_"int" i (PersistentHashMap_1_mask hash, shift) #_"node" node (aget (_0_a this) i)]
///                 (when (some_9_ node)
///                     (INode_3_find node, (+ shift 5), hash, key)
///                 )
///             )
///         )
///         ([#_"ANode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not_found]
///             (let [#_"int" i (PersistentHashMap_1_mask hash, shift) #_"node" node (aget (_0_a this) i)]
///                 (when (some_9_ node) => not_found
///                     (INode_3_find node, (+ shift 5), hash, key, not_found)
///                 )
///             )
///         )
///     )

///     (defn_ #_"ISeq" ANode_2_nodeSeq [#_"ANode" this]
///         (HSeq_1_create_1 (_0_a this))
///     )

///     (defn_ #_"node" ANode_2_assocT [#_"ANode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"bool'" addedLeaf]
///         (let [#_"int" i (PersistentHashMap_1_mask hash, shift) #_"node" ai (aget (_0_a this) i)]
///             (if (some_9_ ai)
///                 (let [#_"node" node (INode_3_assocT ai, edit, (+ shift 5), hash, key, val, addedLeaf)]
///                     (when_not (= node ai) => this
///                         (ANode_2_editAndSet this, edit, i, node)
///                     )
///                 )
///                 (let [#_"node" node (INode_3_assocT BNode_1_EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf)]
///                     (-> (ANode_2_editAndSet this, edit, i, node) (qswap_4_ _0_n inc))
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" ANode_2_dissocT [#_"ANode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"bool'" removedLeaf]
///         (let_when [#_"int" i (PersistentHashMap_1_mask hash, shift) #_"node" ai (aget (_0_a this) i)] (some_9_ ai) => this
///             (let_when_not [#_"node" node (INode_3_dissocT ai, edit, (+ shift 5), hash, key, removedLeaf)] (= node ai) => this
///                 (cond
///                     (some_9_ node)     (ANode_2_editAndSet this, edit, i, node)
///                     (<= (_0_n this) 8) (ANode_2_pack this, edit, i)
///                     _0_else            (-> (ANode_2_editAndSet this, edit, i, node) (qswap_4_ _0_n dec))
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" ANode_2_kvreduce [#_"ANode" this, #_"IFn" f, #_"Object" r]
///         (loop_when [r r #_"int" i 0] (< i (alength (_0_a this))) => r
///             (when_some [#_"node" node (aget (_0_a this) i)] => (recur r (inc i))
///                 (let [r (INode_3_kvreduce node, f, r)]
///                     (when_not (reduced_9_ r) => r
///                         (recur r (inc i))
///                     )
///                 )
///             )
///         )
///     )

///     (defm ANode INode
///         (INode_3_assoc => ANode_2_assoc)
///         (INode_3_dissoc => ANode_2_dissoc)
///         (INode_3_find => ANode_2_find)
///         (INode_3_nodeSeq => ANode_2_nodeSeq)
///         (INode_3_assocT => ANode_2_assocT)
///         (INode_3_dissocT => ANode_2_dissocT)
///         (INode_3_kvreduce => ANode_2_kvreduce)
///     )
// )

// (about #_"BNode"
///     (defq BNode [#_"thread'" edit, #_"int" bitmap, #_"array" a])

///     (defn #_"BNode" BNode_1_new [#_"thread'" edit, #_"int" bitmap, #_"array" a]
///         (new_8_ BNode_1_class (anew [edit, bitmap, a]))
///     )

///     (def #_"BNode" BNode_1_EMPTY (BNode_1_new nil, 0, (anew 0)))

///     (defn_ #_"int" BNode_1_index [#_"int" bitmap, #_"int" bit]
///         (Integer_1_bitCount (bit_and bitmap (dec bit)))
///     )

///     (declare CNode_1_new)

///     (defn_ #_"node" BNode_1_create [#_"int" shift, #_"Object" key1, #_"Object" val1, #_"int" hash2, #_"Object" key2, #_"Object" val2]
///         (let [#_"int" hash1 (f_1_hash key1)]
///             (when_not (= hash1 hash2) => (CNode_1_new nil, hash1, 2, (anew [ key1, val1, key2, val2 ]))
///                 (let [#_"bool'" addedLeaf (atom false) #_"thread'" edit (atom nil)]
///                     (-> BNode_1_EMPTY
///                         (INode_3_assocT edit, shift, hash1, key1, val1, addedLeaf)
///                         (INode_3_assocT edit, shift, hash2, key2, val2, addedLeaf)
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"BNode" BNode_2_ensureEditable [#_"BNode" this, #_"thread'" edit]
///         (when_not (identical_9_ (_0_edit this) edit) => this
///             (let [#_"int" b (_0_bitmap this) #_"int" n (Integer_1_bitCount b) #_"int" m (inc n)]
///                 (BNode_1_new edit, b, (-> (anew (* 2 m)) (acopy_4_ 0 (_0_a this) 0 (* 2 n))))
///             )
///         )
///     )

///     (defn_ #_"BNode" BNode_2_editAndSet
///         ([#_"BNode" this, #_"thread'" edit, #_"int" i, #_"Object" x]
///             (let [#_"BNode" e (BNode_2_ensureEditable this, edit)]
///                 (aset_4_ (_0_a e) i x)
///                 e
///             )
///         )
///         ([#_"BNode" this, #_"thread'" edit, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y]
///             (let [#_"BNode" e (BNode_2_ensureEditable this, edit)]
///                 (aset_4_ (_0_a e) i x)
///                 (aset_4_ (_0_a e) j y)
///                 e
///             )
///         )
///     )

///     (defn_ #_"BNode" BNode_2_editAndRemovePair [#_"BNode" this, #_"thread'" edit, #_"int" bit, #_"int" i]
///         (when_not (= (_0_bitmap this) bit)
///             (let [
///                 #_"BNode" e (-> (BNode_2_ensureEditable this, edit) (qswap_4_ _0_bitmap bit_xor bit))
///                 #_"array" a (_0_a e) #_"int" n (alength a) #_"int" m (* 2 (inc i))
///             ]
///                 (acopy_4_ a (* 2 i) a m (- n m))
///                 (aset_4_ a (- n 2) nil)
///                 (aset_4_ a (- n 1) nil)
///                 e
///             )
///         )
///     )

///     (defn_ #_"node" BNode_1_createT [#_"thread'" edit, #_"int" shift, #_"Object" key1, #_"Object" val1, #_"int" hash2, #_"Object" key2, #_"Object" val2]
///         (let [#_"int" hash1 (f_1_hash key1)]
///             (when_not (= hash1 hash2) => (CNode_1_new nil, hash1, 2, (anew [ key1, val1, key2, val2 ]))
///                 (let [#_"bool'" addedLeaf (atom false)]
///                     (-> BNode_1_EMPTY
///                         (INode_3_assocT edit, shift, hash1, key1, val1, addedLeaf)
///                         (INode_3_assocT edit, shift, hash2, key2, val2, addedLeaf)
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" BNode_2_assoc [#_"BNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"bool'" addedLeaf]
///         (let [#_"int" bit (PersistentHashMap_1_bitpos hash, shift) #_"int" x (BNode_1_index (_0_bitmap this), bit)]
///             (if_not (zero_9_ (bit_and (_0_bitmap this) bit))
///                 (let [
///                     #_"key|nil" k (aget (_0_a this) (* 2 x)) #_"value|node" v (aget (_0_a this) (inc (* 2 x)))
///                     #_"array" a_1_
///                         (cond
///                             (nil_9_ k)
///                                 (let [#_"node" node (INode_3_assoc #_"node" v, (+ shift 5), hash, key, val, addedLeaf)]
///                                     (when_not (= node v)
///                                         (PersistentHashMap_1_cloneAndSet (_0_a this), (inc (* 2 x)), node)
///                                     )
///                                 )
///                             (= key k)
///                                 (when_not (= val v)
///                                     (PersistentHashMap_1_cloneAndSet (_0_a this), (inc (* 2 x)), val)
///                                 )
///                             _0_else
///                                 (let [#_"node" node (BNode_1_create (+ shift 5), k, v, hash, key, val) _ (reset_4_ addedLeaf true)]
///                                     (PersistentHashMap_1_cloneAndSet (_0_a this), (* 2 x), nil, (inc (* 2 x)), node)
///                                 )
///                         )
///                 ]
///                     (when (some_9_ a_1_) => this
///                         (BNode_1_new nil, (_0_bitmap this), a_1_)
///                     )
///                 )
///                 (let [#_"int" n (Integer_1_bitCount (_0_bitmap this))]
///                     (if (<= 16 n)
///                         (let [
///                             #_"node[]" nodes (anew #_"node" 32) #_"int" m (PersistentHashMap_1_mask hash, shift)
///                             _ (aset_4_ nodes m (INode_3_assoc BNode_1_EMPTY, (+ shift 5), hash, key, val, addedLeaf))
///                             _
///                                 (loop_when [#_"int" j 0 #_"int" i 0] (< i 32)
///                                     (when (odd_9_ (>>> (_0_bitmap this) i)) => (recur j (inc i))
///                                         (let [#_"key|nil" k (aget (_0_a this) j) #_"value|node" v (aget (_0_a this) (inc j))]
///                                             (if (some_9_ k)
///                                                 (aset_4_ nodes i (INode_3_assoc BNode_1_EMPTY, (+ shift 5), (f_1_hash k), k, v, addedLeaf))
///                                                 (aset_4_ nodes i #_"node" v)
///                                             )
///                                             (recur (+ j 2) (inc i))
///                                         )
///                                     )
///                                 )
///                         ]
///                             (ANode_1_new nil, (inc n), nodes)
///                         )
///                         (let [
///                             #_"array" a_1_ (anew (* 2 (inc n)))
///                             _ (acopy_4_ a_1_ 0 (_0_a this) 0 (* 2 x))
///                             _ (aset_4_ a_1_ (* 2 x) key)
///                             _ (reset_4_ addedLeaf true)
///                             _ (aset_4_ a_1_ (inc (* 2 x)) val)
///                             _ (acopy_4_ a_1_ (* 2 (inc x)) (_0_a this) (* 2 x) (* 2 (- n x)))
///                         ]
///                             (BNode_1_new nil, (bit_or (_0_bitmap this) bit), a_1_)
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" BNode_2_dissoc [#_"BNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
///         (let_when_not [#_"int" bit (PersistentHashMap_1_bitpos hash, shift)] (zero_9_ (bit_and (_0_bitmap this) bit)) => this
///             (let [
///                 #_"int" x (BNode_1_index (_0_bitmap this), bit)
///                 #_"key|nil" k (aget (_0_a this) (* 2 x)) #_"value|node" v (aget (_0_a this) (inc (* 2 x)))
///             ]
///                 (if (some_9_ k)
///                     (when (= key k) => this
///                         (BNode_1_new nil, (bit_xor (_0_bitmap this) bit), (PersistentHashMap_1_removePair (_0_a this), x))
///                     )
///                     (let [#_"node" node (INode_3_dissoc #_"node" v, (+ shift 5), hash, key)]
///                         (cond
///                             (= node v)
///                                 this
///                             (some_9_ node)
///                                 (BNode_1_new nil, (_0_bitmap this), (PersistentHashMap_1_cloneAndSet (_0_a this), (inc (* 2 x)), node))
///                             (= (_0_bitmap this) bit)
///                                 nil
///                             _0_else
///                                 (BNode_1_new nil, (bit_xor (_0_bitmap this) bit), (PersistentHashMap_1_removePair (_0_a this), x))
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"IMapEntry|value" BNode_2_find
///         ([#_"BNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
///             (let_when_not [#_"int" bit (PersistentHashMap_1_bitpos hash, shift)] (zero_9_ (bit_and (_0_bitmap this) bit))
///                 (let [
///                     #_"int" x (BNode_1_index (_0_bitmap this), bit)
///                     #_"key|nil" k (aget (_0_a this) (* 2 x)) #_"value|node" v (aget (_0_a this) (inc (* 2 x)))
///                 ]
///                     (cond
///                         (nil_9_ k)  (INode_3_find #_"node" v, (+ shift 5), hash, key)
///                         (= key k) (MapEntry_1_new k, v)
///                     )
///                 )
///             )
///         )
///         ([#_"BNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not_found]
///             (let_when_not [#_"int" bit (PersistentHashMap_1_bitpos hash, shift)] (zero_9_ (bit_and (_0_bitmap this) bit)) => not_found
///                 (let [
///                     #_"int" x (BNode_1_index (_0_bitmap this), bit)
///                     #_"key|nil" k (aget (_0_a this) (* 2 x)) #_"value|node" v (aget (_0_a this) (inc (* 2 x)))
///                 ]
///                     (cond
///                         (nil_9_ k)  (INode_3_find #_"node" v, (+ shift 5), hash, key, not_found)
///                         (= key k) v
///                         _0_else     not_found
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"ISeq" BNode_2_nodeSeq [#_"BNode" this]
///         (NSeq_1_create_1 (_0_a this))
///     )

///     (defn_ #_"node" BNode_2_assocT [#_"BNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"bool'" addedLeaf]
///         (let [#_"int" bit (PersistentHashMap_1_bitpos hash, shift) #_"int" x (BNode_1_index (_0_bitmap this), bit)]
///             (if_not (zero_9_ (bit_and (_0_bitmap this) bit))
///                 (let [
///                     #_"key|nil" k (aget (_0_a this) (* 2 x)) #_"value|node" v (aget (_0_a this) (inc (* 2 x)))
///                 ]
///                     (cond
///                         (nil_9_ k)
///                             (let [#_"node" node (INode_3_assocT #_"node" v, edit, (+ shift 5), hash, key, val, addedLeaf)]
///                                 (when_not (= node v) => this
///                                     (BNode_2_editAndSet this, edit, (inc (* 2 x)), node)
///                                 )
///                             )
///                         (= key k)
///                             (when_not (= val v) => this
///                                 (BNode_2_editAndSet this, edit, (inc (* 2 x)), val)
///                             )
///                         _0_else
///                             (let [#_"node" node (BNode_1_createT edit, (+ shift 5), k, v, hash, key, val) _ (reset_4_ addedLeaf true)]
///                                 (BNode_2_editAndSet this, edit, (* 2 x), nil, (inc (* 2 x)), node)
///                             )
///                     )
///                 )
///                 (let [#_"int" n (Integer_1_bitCount (_0_bitmap this))]
///                     (cond
///                         (< (* n 2) (alength (_0_a this)))
///                             (let [
///                                 #_"BNode" e (-> (BNode_2_ensureEditable this, edit) (qswap_4_ _0_bitmap bit_or bit)) _ (reset_4_ addedLeaf true)
///                                 _ (acopy_4_ (_0_a e) (* 2 (inc x)) (_0_a e) (* 2 x) (* 2 (- n x)))
///                                 _ (aset_4_ (_0_a e) (* 2 x) key)
///                                 _ (aset_4_ (_0_a e) (inc (* 2 x)) val)
///                             ]
///                                 e
///                             )
///                         (<= 16 n)
///                             (let [
///                                 #_"node[]" nodes (anew #_"node" 32) #_"int" m (PersistentHashMap_1_mask hash, shift)
///                                 _ (aset_4_ nodes m (INode_3_assocT BNode_1_EMPTY, edit, (+ shift 5), hash, key, val, addedLeaf))
///                                 _
///                                     (loop_when [#_"int" j 0 #_"int" i 0] (< i 32)
///                                         (when (odd_9_ (>>> (_0_bitmap this) i)) => (recur j (inc i))
///                                             (let [#_"key|nil" k (aget (_0_a this) j) #_"value|node" v (aget (_0_a this) (inc j))]
///                                                 (if (some_9_ k)
///                                                     (aset_4_ nodes i (INode_3_assocT BNode_1_EMPTY, edit, (+ shift 5), (f_1_hash k), k, v, addedLeaf))
///                                                     (aset_4_ nodes i #_"node" v)
///                                                 )
///                                                 (recur (+ j 2) (inc i))
///                                             )
///                                         )
///                                     )
///                             ]
///                                 (ANode_1_new edit, (inc n), nodes)
///                             )
///                         _0_else
///                             (let [
///                                 #_"array" a_1_ (anew (* 2 (+ n 4)))
///                                 _ (acopy_4_ a_1_ 0 (_0_a this) 0 (* 2 x))
///                                 _ (aset_4_ a_1_ (* 2 x) key)
///                                 _ (reset_4_ addedLeaf true)
///                                 _ (aset_4_ a_1_ (inc (* 2 x)) val)
///                                 _ (acopy_4_ a_1_ (* 2 (inc x)) (_0_a this) (* 2 x) (* 2 (- n x)))
///                             ]
///                                 (-> (BNode_2_ensureEditable this, edit)
///                                     (qset_4_ _0_a a_1_)
///                                     (qswap_4_ _0_bitmap bit_or bit)
///                                 )
///                             )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" BNode_2_dissocT [#_"BNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"bool'" removedLeaf]
///         (let_when_not [#_"int" bit (PersistentHashMap_1_bitpos hash, shift)] (zero_9_ (bit_and (_0_bitmap this) bit)) => this
///             (let [
///                 #_"int" x (BNode_1_index (_0_bitmap this), bit)
///                 #_"key|nil" k (aget (_0_a this) (* 2 x)) #_"value|node" v (aget (_0_a this) (inc (* 2 x)))
///             ]
///                 (if (some_9_ k)
///                     (when (= key k) => this
///                         (reset_4_ removedLeaf true)
///                         (BNode_2_editAndRemovePair this, edit, bit, x)
///                     )
///                     (let [#_"node" node (INode_3_dissocT #_"node" v, edit, (+ shift 5), hash, key, removedLeaf)]
///                         (cond
///                             (= node v)
///                                 this
///                             (some_9_ node)
///                                 (BNode_2_editAndSet this, edit, (inc (* 2 x)), node)
///                             (= (_0_bitmap this) bit)
///                                 nil
///                             _0_else
///                                 (BNode_2_editAndRemovePair this, edit, bit, x)
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" BNode_2_kvreduce [#_"BNode" this, #_"IFn" f, #_"Object" r]
///         (NSeq_1_kvreduce (_0_a this), f, r)
///     )

///     (defm BNode INode
///         (INode_3_assoc => BNode_2_assoc)
///         (INode_3_dissoc => BNode_2_dissoc)
///         (INode_3_find => BNode_2_find)
///         (INode_3_nodeSeq => BNode_2_nodeSeq)
///         (INode_3_assocT => BNode_2_assocT)
///         (INode_3_dissocT => BNode_2_dissocT)
///         (INode_3_kvreduce => BNode_2_kvreduce)
///     )
// )

// (about #_"CNode"
///     (defq CNode [#_"thread'" edit, #_"int" hash, #_"int" n, #_"array" a])

///     (defn #_"CNode" CNode_1_new [#_"thread'" edit, #_"int" hash, #_"int" n, #_"array" a]
///         (new_8_ CNode_1_class (anew [edit, hash, n, a]))
///     )

///     (defn_ #_"int" CNode_2_findIndex [#_"CNode" this, #_"Object" key]
///         (let [#_"array" a (_0_a this) #_"int" m (* 2 (_0_n this))]
///             (loop_when [#_"int" i 0] (< i m) => -1
///                 (if (= (aget a i) key) i (recur (+ i 2)))
///             )
///         )
///     )

///     (defn_ #_"CNode" CNode_2_ensureEditable
///         ([#_"CNode" this, #_"thread'" edit]
///             (when_not (identical_9_ (_0_edit this) edit) => this
///                 (let [
///                     #_"int" n (_0_n this) #_"int" m (inc n)
///                     #_"array" a_1_ (-> (anew (* 2 m)) (acopy_4_ 0 (_0_a this) 0 (* 2 n)))
///                 ]
///                     (CNode_1_new edit, (_0_hash this), n, a_1_)
///                 )
///             )
///         )
///         ([#_"CNode" this, #_"thread'" edit, #_"int" n, #_"array" a]
///             (when_not (identical_9_ (_0_edit this) edit) => (qset_4_ this _0_a a, _0_n n)
///                 (CNode_1_new edit, (_0_hash this), n, a)
///             )
///         )
///     )

///     (defn_ #_"CNode" CNode_2_editAndSet
///         ([#_"CNode" this, #_"thread'" edit, #_"int" i, #_"Object" x]
///             (let [#_"CNode" e (CNode_2_ensureEditable this, edit)]
///                 (aset_4_ (_0_a e) i x)
///                 e
///             )
///         )
///         ([#_"CNode" this, #_"thread'" edit, #_"int" i, #_"Object" x, #_"int" j, #_"Object" y]
///             (let [#_"CNode" e (CNode_2_ensureEditable this, edit)]
///                 (aset_4_ (_0_a e) i x)
///                 (aset_4_ (_0_a e) j y)
///                 e
///             )
///         )
///     )

///     (defn_ #_"node" CNode_2_assoc [#_"CNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"bool'" addedLeaf]
///         (if (= (_0_hash this) hash)
///             (let [#_"array" a (_0_a this) #_"int" i (CNode_2_findIndex this, key) #_"int" n (_0_n this)]
///                 (if (< -1 i)
///                     (when_not (= (aget a (inc i)) val) => this
///                         (CNode_1_new nil, hash, n, (PersistentHashMap_1_cloneAndSet a, (inc i), val))
///                     )
///                     (let [
///                         #_"array" a_1_ (-> (anew (* 2 (inc n))) (acopy_4_ 0 a 0 (* 2 n)) (aset_4_ (* 2 n) key) (aset_4_ (inc (* 2 n)) val))
///                         _ (reset_4_ addedLeaf true)
///                     ]
///                         (CNode_1_new (_0_edit this), hash, (inc n), a_1_)
///                     )
///                 )
///             )
///             (let [#_"BNode" node (BNode_1_new nil, (PersistentHashMap_1_bitpos (_0_hash this), shift), (anew [ nil, this ]))]
///                 (INode_3_assoc node, shift, hash, key, val, addedLeaf)
///             )
///         )
///     )

///     (defn_ #_"node" CNode_2_dissoc [#_"CNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
///         (let_when [#_"int" i (CNode_2_findIndex this, key)] (< -1 i) => this
///             (let_when [#_"int" n (_0_n this)] (< 1 n)
///                 (CNode_1_new nil, hash, (dec n), (PersistentHashMap_1_removePair (_0_a this), (quot i 2)))
///             )
///         )
///     )

///     (defn_ #_"IMapEntry|value" CNode_2_find
///         ([#_"CNode" this, #_"int" shift, #_"int" hash, #_"Object" key]
///             (let_when [#_"int" i (CNode_2_findIndex this, key)] (< -1 i)
///                 (let_when [#_"Object" ai (aget (_0_a this) i)] (= ai key)
///                     (MapEntry_1_new ai, (aget (_0_a this) (inc i)))
///                 )
///             )
///         )
///         ([#_"CNode" this, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" not_found]
///             (let_when [#_"int" i (CNode_2_findIndex this, key)] (< -1 i) => not_found
///                 (when (= (aget (_0_a this) i) key) => not_found
///                     (aget (_0_a this) (inc i))
///                 )
///             )
///         )
///     )

///     (defn_ #_"ISeq" CNode_2_nodeSeq [#_"CNode" this]
///         (NSeq_1_create_1 (_0_a this))
///     )

///     (defn_ #_"node" CNode_2_assocT [#_"CNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"Object" val, #_"bool'" addedLeaf]
///         (if (= (_0_hash this) hash)
///             (let [#_"array" a (_0_a this) #_"int" i (CNode_2_findIndex this, key)]
///                 (if (< -1 i)
///                     (when_not (= (aget a (inc i)) val) => this
///                         (CNode_2_editAndSet this, edit, (inc i), val)
///                     )
///                     (let [#_"int" n (_0_n this) #_"int" m (alength a)]
///                         (if (< (* 2 n) m)
///                             (let [_ (reset_4_ addedLeaf true)]
///                                 (-> (CNode_2_editAndSet this, edit, (* 2 n), key, (inc (* 2 n)), val)
///                                     (qswap_4_ _0_n inc)
///                                 )
///                             )
///                             (let [
///                                 #_"array" a_1_ (-> (anew (+ m 2)) (acopy_4_ 0 a 0 m) (aset_4_ m key) (aset_4_ (inc m) val))
///                                 _ (reset_4_ addedLeaf true)
///                             ]
///                                 (CNode_2_ensureEditable this, edit, (inc n), a_1_)
///                             )
///                         )
///                     )
///                 )
///             )
///             (let [#_"BNode" node (BNode_1_new edit, (PersistentHashMap_1_bitpos (_0_hash this), shift), (anew [ nil, this, nil, nil ]))]
///                 (INode_3_assocT node, edit, shift, hash, key, val, addedLeaf)
///             )
///         )
///     )

///     (defn_ #_"node" CNode_2_dissocT [#_"CNode" this, #_"thread'" edit, #_"int" shift, #_"int" hash, #_"Object" key, #_"bool'" removedLeaf]
///         (let_when [#_"int" i (CNode_2_findIndex this, key)] (< -1 i) => this
///             (reset_4_ removedLeaf true)
///             (let_when [#_"int" n (_0_n this)] (< 1 n)
///                 (let [
///                     #_"CNode" e (-> (CNode_2_ensureEditable this, edit) (qswap_4_ _0_n dec))
///                     #_"int" m (* 2 n)
///                     _ (aset_4_ (_0_a e) i (aget (_0_a e) (- m 2)))
///                     _ (aset_4_ (_0_a e) (inc i) (aget (_0_a e) (- m 1)))
///                     _ (aset_4_ (_0_a e) (- m 2) nil)
///                     _ (aset_4_ (_0_a e) (- m 1) nil)
///                 ]
///                     e
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" CNode_2_kvreduce [#_"CNode" this, #_"IFn" f, #_"Object" r]
///         (NSeq_1_kvreduce (_0_a this), f, r)
///     )

///     (defm CNode INode
///         (INode_3_assoc => CNode_2_assoc)
///         (INode_3_dissoc => CNode_2_dissoc)
///         (INode_3_find => CNode_2_find)
///         (INode_3_nodeSeq => CNode_2_nodeSeq)
///         (INode_3_assocT => CNode_2_assocT)
///         (INode_3_dissocT => CNode_2_dissocT)
///         (INode_3_kvreduce => CNode_2_kvreduce)
///     )
// )

// (about #_"TransientHashMap"
///     (defq TransientHashMap [#_"thread'" edit, #_"node" root, #_"int" cnt, #_"bool" has_nil_9_, #_"Object" nil_value] #_"MapForm")

///     #_inherit
///     (defm TransientHashMap ATransientMap AFn)

///     (defn #_"TransientHashMap" TransientHashMap_1_new
///         ([#_"PersistentHashMap" m]
///             (TransientHashMap_1_new (atom (thread)), (_0_root m), (_0_cnt m), (_0_has_nil_9_ m), (_0_nil_value m))
///         )
///         ([#_"thread'" edit, #_"node" root, #_"int" cnt, #_"bool" has_nil_9_, #_"Object" nil_value]
///             (new_8_ TransientHashMap_1_class (anew [edit, root, cnt, has_nil_9_, nil_value]))
///         )
///     )

///     (defn_ #_"void" TransientHashMap_2_assert_editable [#_"TransientHashMap" this]
///         (or (deref (_0_edit this)) (throw "transient used after persistent! call"))
///         nil
///     )

///     (defn_ #_"int" TransientHashMap_2_count [#_"TransientHashMap" this]
///         (TransientHashMap_2_assert_editable this)
///         (_0_cnt this)
///     )

///     (defn_ #_"Object" TransientHashMap_2_valAt
///         ([#_"TransientHashMap" this, #_"Object" key] (TransientHashMap_2_valAt this, key, nil))
///         ([#_"TransientHashMap" this, #_"Object" key, #_"Object" not_found]
///             (TransientHashMap_2_assert_editable this)
///             (if (nil_9_ key)
///                 (when (_0_has_nil_9_ this) => not_found
///                     (_0_nil_value this)
///                 )
///                 (when (some_9_ (_0_root this)) => not_found
///                     (INode_3_find (_0_root this), 0, (f_1_hash key), key, not_found)
///                 )
///             )
///         )
///     )

///     (defn_ #_"ITransientMap" TransientHashMap_2_assoc_4_ [#_"TransientHashMap" this, #_"Object" key, #_"Object" val]
///         (TransientHashMap_2_assert_editable this)
///         (if (nil_9_ key)
///             (let [
///                 this (if (= (_0_nil_value this) val) this (qset_4_ this _0_nil_value val))
///             ]
///                 (when_not (_0_has_nil_9_ this) => this
///                     (-> this (qswap_4_ _0_cnt inc) (qset_4_ _0_has_nil_9_ true))
///                 )
///             )
///             (let [
///                 #_"bool'" addedLeaf (atom false)
///                 #_"node" node (INode_3_assocT (or (_0_root this) BNode_1_EMPTY), (_0_edit this), 0, (f_1_hash key), key, val, addedLeaf)
///                 this (if (= (_0_root this) node) this (qset_4_ this _0_root node))
///             ]
///                 (when (deref addedLeaf) => this
///                     (-> this (qswap_4_ _0_cnt inc))
///                 )
///             )
///         )
///     )

///     (defn_ #_"ITransientMap" TransientHashMap_2_dissoc_4_ [#_"TransientHashMap" this, #_"Object" key]
///         (TransientHashMap_2_assert_editable this)
///         (if (nil_9_ key)
///             (when (_0_has_nil_9_ this) => this
///                 (-> this (qswap_4_ _0_cnt dec) (qset_4_ _0_has_nil_9_ false, _0_nil_value nil))
///             )
///             (when (some_9_ (_0_root this)) => this
///                 (let [
///                     #_"bool'" removedLeaf (atom false)
///                     #_"node" node (INode_3_dissocT (_0_root this), (_0_edit this), 0, (f_1_hash key), key, removedLeaf)
///                     this (if (= (_0_root this) node) this (qset_4_ this _0_root node))
///                 ]
///                     (when (deref removedLeaf) => this
///                         (-> this (qswap_4_ _0_cnt dec))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"ITransientMap" TransientHashMap_2_conj_4_ [#_"TransientHashMap" this, #_"pair" o]
///         (TransientHashMap_2_assert_editable this)
///         (condp satisfies_9_ o
///             IMapEntry
///                 (assoc_4_ this (key o) (val o))
///             IPersistentVector
///                 (when (= (count o) 2) => (throw "vector arg to map conj must be a pair")
///                     (assoc_4_ this (nth o 0) (nth o 1))
///                 )
///             #_else
///                 (loop_when [this this #_"ISeq" s (seq o)] (some_9_ s) => this
///                     (let [#_"pair" e (first s)]
///                         (recur (assoc_4_ this (key e) (val e)) (next s))
///                     )
///                 )
///         )
///     )

///     (declare PersistentHashMap_1_new)

///     (defn_ #_"IPersistentMap" TransientHashMap_2_persistent_4_ [#_"TransientHashMap" this]
///         (TransientHashMap_2_assert_editable this)
///         (reset_4_ (_0_edit this) nil)
///         (PersistentHashMap_1_new (_0_cnt this), (_0_root this), (_0_has_nil_9_ this), (_0_nil_value this))
///     )

///     (defm TransientHashMap Counted
///         (Counted_3_count => TransientHashMap_2_count)
///     )

///     (defm TransientHashMap ILookup
///         (ILookup_3_valAt => TransientHashMap_2_valAt)
///     )

///     (defm TransientHashMap IFn
///         (IFn_3_invoke => ATransientMap_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm TransientHashMap ITransientAssociative
///         (ITransientAssociative_3_assoc_4_ => TransientHashMap_2_assoc_4_)
///         (ITransientAssociative_3_containsKey => ATransientMap_2_containsKey)
///         (ITransientAssociative_3_entryAt => ATransientMap_2_entryAt)
///     )

///     (defm TransientHashMap ITransientMap
///         (ITransientMap_3_dissoc_4_ => TransientHashMap_2_dissoc_4_)
///     )

///     (defm TransientHashMap ITransientCollection
///         (ITransientCollection_3_conj_4_ => TransientHashMap_2_conj_4_)
///         (ITransientCollection_3_persistent_4_ => TransientHashMap_2_persistent_4_)
///     )
// )

// (about #_"PersistentHashMap"
///     (declare PersistentHashMap_2_seq)

///     (defq PersistentHashMap [#_"IPersistentMap" _meta, #_"int" cnt, #_"node" root, #_"bool" has_nil_9_, #_"Object" nil_value] MapForm
///         clojure.lang.Seqable (seq [_] (PersistentHashMap_2_seq _))
///     )

///     #_inherit
///     (defm PersistentHashMap APersistentMap AFn)

///     (defn #_"PersistentHashMap" PersistentHashMap_1_new
///         ([#_"int" cnt, #_"node" root, #_"bool" has_nil_9_, #_"Object" nil_value] (PersistentHashMap_1_new nil, cnt, root, has_nil_9_, nil_value))
///         ([#_"IPersistentMap" meta, #_"int" cnt, #_"node" root, #_"bool" has_nil_9_, #_"Object" nil_value]
///             (new_8_ PersistentHashMap_1_class (anew [meta, cnt, root, has_nil_9_, nil_value]))
///         )
///     )

///     (def #_"PersistentHashMap" PersistentHashMap_1_EMPTY (PersistentHashMap_1_new 0, nil, false, nil))

///     (defn #_"PersistentHashMap" PersistentHashMap_1_create_1a [#_"array" init]
///         (loop_when_recur [#_"ITransientMap" m (transient PersistentHashMap_1_EMPTY) #_"int" i 0]
///                          (< i (alength init))
///                          [(assoc_4_ m (aget init i) (aget init (inc i))) (+ i 2)]
///                       => (persistent_4_ m)
///         )
///     )

///     (defn #_"PersistentHashMap" PersistentHashMap_1_create_1s [#_"Seqable" init]
///         (let [#_"ITransientMap" m (transient PersistentHashMap_1_EMPTY)]
///             (loop_when [m m #_"ISeq" s (seq init)] (some_9_ s) => (persistent_4_ m)
///                 (when (some_9_ (next s)) => (throw (str "no value supplied for key: " (first s)))
///                     (recur (assoc_4_ m (first s) (second s)) (next (next s)))
///                 )
///             )
///         )
///     )

///     (defn #_"PersistentHashMap" PersistentHashMap_1_createWithCheck_1a [#_"array" init]
///         (let [#_"ITransientMap" m (transient PersistentHashMap_1_EMPTY)]
///             (loop_when [m m #_"int" i 0] (< i (alength init)) => (persistent_4_ m)
///                 (let [m (assoc_4_ m (aget init i) (aget init (inc i)))]
///                     (when (= (count m) (inc (quot i 2))) => (throw (str "duplicate key: " (aget init i)))
///                         (recur m (+ i 2))
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"PersistentHashMap" PersistentHashMap_1_createWithCheck_1s [#_"Seqable" init]
///         (let [#_"ITransientMap" m (transient PersistentHashMap_1_EMPTY)]
///             (loop_when [m m #_"ISeq" s (seq init) #_"int" n 0] (some_9_ s) => (persistent_4_ m)
///                 (when (some_9_ (next s)) => (throw (str "no value supplied for key: " (first s)))
///                     (let [m (assoc_4_ m (first s) (second s))]
///                         (when (= (count m) (inc n)) => (throw (str "duplicate key: " (first s)))
///                             (recur m (next (next s)) (inc n))
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"PersistentHashMap" PersistentHashMap_2_withMeta [#_"PersistentHashMap" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentHashMap_1_new meta, (_0_cnt this), (_0_root this), (_0_has_nil_9_ this), (_0_nil_value this))
///         )
///     )

///     (defn_ #_"Object" PersistentHashMap_2_valAt
///         ([#_"PersistentHashMap" this, #_"Object" key] (PersistentHashMap_2_valAt this, key, nil))
///         ([#_"PersistentHashMap" this, #_"Object" key, #_"Object" not_found]
///             (if (nil_9_ key)
///                 (when (_0_has_nil_9_ this) => not_found
///                     (_0_nil_value this)
///                 )
///                 (when (some_9_ (_0_root this)) => not_found
///                     (INode_3_find (_0_root this), 0, (f_1_hash key), key, not_found)
///                 )
///             )
///         )
///     )

///     (def_ #_"Object" PersistentHashMap_1_NOT_FOUND (anew 0))

///     (defn_ #_"IPersistentMap" PersistentHashMap_2_assoc [#_"PersistentHashMap" this, #_"Object" key, #_"Object" val]
///         (if (nil_9_ key)
///             (when_not (and (_0_has_nil_9_ this) (= (_0_nil_value this) val)) => this
///                 (PersistentHashMap_1_new (_0__meta this), (+ (_0_cnt this) (if (_0_has_nil_9_ this) 0 1)), (_0_root this), true, val)
///             )
///             (let [
///                 #_"bool'" addedLeaf (atom false)
///                 #_"node" root (INode_3_assoc (or (_0_root this) BNode_1_EMPTY), 0, (f_1_hash key), key, val, addedLeaf)
///             ]
///                 (when_not (= root (_0_root this)) => this
///                     (PersistentHashMap_1_new (_0__meta this), (+ (_0_cnt this) (if (deref addedLeaf) 1 0)), root, (_0_has_nil_9_ this), (_0_nil_value this))
///                 )
///             )
///         )
///     )

///     (defn_ #_"bool" PersistentHashMap_2_containsKey [#_"PersistentHashMap" this, #_"Object" key]
///         (if (nil_9_ key)
///             (_0_has_nil_9_ this)
///             (and (some_9_ (_0_root this))
///                 (not (identical_9_ (INode_3_find (_0_root this), 0, (f_1_hash key), key, PersistentHashMap_1_NOT_FOUND) PersistentHashMap_1_NOT_FOUND))
///             )
///         )
///     )

///     (defn_ #_"pair" PersistentHashMap_2_entryAt [#_"PersistentHashMap" this, #_"Object" key]
///         (if (nil_9_ key)
///             (when (_0_has_nil_9_ this)
///                 (MapEntry_1_new nil, (_0_nil_value this))
///             )
///             (when (some_9_ (_0_root this))
///                 (INode_3_find (_0_root this), 0, (f_1_hash key), key)
///             )
///         )
///     )

///     (defn_ #_"IPersistentMap" PersistentHashMap_2_dissoc [#_"PersistentHashMap" this, #_"Object" key]
///         (cond
///             (nil_9_ key)
///                 (when (_0_has_nil_9_ this) => this
///                     (PersistentHashMap_1_new (_0__meta this), (dec (_0_cnt this)), (_0_root this), false, nil)
///                 )
///             (nil_9_ (_0_root this))
///                 this
///             _0_else
///                 (let [#_"node" root (INode_3_dissoc (_0_root this), 0, (f_1_hash key), key)]
///                     (when_not (= root (_0_root this)) => this
///                         (PersistentHashMap_1_new (_0__meta this), (dec (_0_cnt this)), root, (_0_has_nil_9_ this), (_0_nil_value this))
///                     )
///                 )
///         )
///     )

///     (defn_ #_"IPersistentCollection" PersistentHashMap_2_empty [#_"PersistentHashMap" this]
///         (with_meta PersistentHashMap_1_EMPTY (_0__meta this))
///     )

///     (defn_ #_"ISeq" PersistentHashMap_2_seq [#_"PersistentHashMap" this]
///         (let [#_"ISeq" s (when (some_9_ (_0_root this)) (INode_3_nodeSeq (_0_root this)))]
///             (when (_0_has_nil_9_ this) => s
///                 (Cons_1_new (MapEntry_1_new nil, (_0_nil_value this)), s)
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentHashMap_2_kvreduce [#_"PersistentHashMap" this, #_"IFn" f, #_"Object" r]
///         (let [r (if (_0_has_nil_9_ this) (f r nil (_0_nil_value this)) r)]
///             (when_not (reduced_9_ r) => (deref r)
///                 (when (some_9_ (_0_root this)) => r
///                     (let [r (INode_3_kvreduce (_0_root this), f, r)]
///                         (when_not (reduced_9_ r) => (deref r)
///                             r
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defm PersistentHashMap IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentHashMap IObj
///         (IObj_3_withMeta => PersistentHashMap_2_withMeta)
///     )

///     (defm PersistentHashMap Counted
///         (Counted_3_count => _0_cnt)
///     )

///     (defm PersistentHashMap ILookup
///         (ILookup_3_valAt => PersistentHashMap_2_valAt)
///     )

///     (defm PersistentHashMap IFn
///         (IFn_3_invoke => APersistentMap_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm PersistentHashMap Associative
///         (Associative_3_assoc => PersistentHashMap_2_assoc)
///         (Associative_3_containsKey => PersistentHashMap_2_containsKey)
///         (Associative_3_entryAt => PersistentHashMap_2_entryAt)
///     )

///     (defm PersistentHashMap IPersistentMap
///         (IPersistentMap_3_dissoc => PersistentHashMap_2_dissoc)
///     )

///     (defm PersistentHashMap IPersistentCollection
///         (IPersistentCollection_3_conj => APersistentMap_2_conj)
///         (IPersistentCollection_3_empty => PersistentHashMap_2_empty)
///     )

///     (defm PersistentHashMap Seqable
///         (Seqable_3_seq => PersistentHashMap_2_seq)
///     )

///     (defm PersistentHashMap IKVReduce
///         (IKVReduce_3_kvreduce => PersistentHashMap_2_kvreduce)
///     )

///     (defm PersistentHashMap IEditableCollection
///         (IEditableCollection_3_asTransient => TransientHashMap_1_new)
///     )

///     (defm PersistentHashMap IObject
///         (IObject_3_equals => APersistentMap_2_equals)
///     )

///     (defm PersistentHashMap Hashed
///         (Hashed_3_hash => Murmur3_1_hashUnordered)
///     )
// )

/// (defn hash_map
///     ([] PersistentHashMap_1_EMPTY)
///     ([& keyvals] (PersistentHashMap_1_create_1s keyvals))
/// )

/// (defn merge [& maps]
///     (when (some identity maps)
///         (reduce #(conj (or %1 (hash_map)) %2) maps)
///     )
/// )

/// (defn merge_with [f & maps]
///     (when (some identity maps)
///         (letfn [(merge_ [m e]
///                     (let [k (key e) v (val e)]
///                         (assoc m k (if (contains_9_ m k) (f (get m k) v) v))
///                     )
///                 )]
///             (reduce #(reduce merge_ (or %1 (hash_map)) %2) maps)
///         )
///     )
/// )

/// (defn zipmap [keys vals]
///     (loop_when_recur [m (transient (hash_map)) ks (seq keys) vs (seq vals)]
///                      (and ks vs)
///                      [(assoc_4_ m (first ks) (first vs)) (next ks) (next vs)]
///                   => (persistent_4_ m)
///     )
/// )
}

namespace arbace {

// (about #_"TransientHashSet"
///     (defq TransientHashSet [#_"ITransientMap" impl] #_"SetForm")

///     #_inherit
///     (defm TransientHashSet ATransientSet AFn)

///     (defn #_"TransientHashSet" TransientHashSet_1_new [#_"ITransientMap" impl]
///         (new_8_ TransientHashSet_1_class (anew [impl]))
///     )

///     (defn_ #_"int" TransientHashSet_2_count [#_"TransientHashSet" this]
///         (count (_0_impl this))
///     )

///     (defn_ #_"ITransientSet" TransientHashSet_2_conj_4_ [#_"TransientHashSet" this, #_"Object" val]
///         (let [#_"ITransientMap" m (assoc_4_ (_0_impl this) val val)]
///             (when_not (= m (_0_impl this)) => this
///                 (qset_4_ this _0_impl m)
///             )
///         )
///     )

///     (declare PersistentHashSet_1_new)

///     (defn_ #_"PersistentHashSet" TransientHashSet_2_persistent_4_ [#_"TransientHashSet" this]
///         (PersistentHashSet_1_new nil, (persistent_4_ (_0_impl this)))
///     )

///     (defn_ #_"ITransientSet" TransientHashSet_2_disj_4_ [#_"TransientHashSet" this, #_"Object" key]
///         (let [#_"ITransientMap" m (dissoc_4_ (_0_impl this) key)]
///             (when_not (= m (_0_impl this)) => this
///                 (qset_4_ this _0_impl m)
///             )
///         )
///     )

///     (defn_ #_"bool" TransientHashSet_2_contains_9_ [#_"TransientHashSet" this, #_"Object" key]
///         (not (identical_9_ (get (_0_impl this) key this) this))
///     )

///     (defn_ #_"Object" TransientHashSet_2_get [#_"TransientHashSet" this, #_"Object" key]
///         (get (_0_impl this) key)
///     )

///     (defm TransientHashSet Counted
///         (Counted_3_count => TransientHashSet_2_count)
///     )

///     (defm TransientHashSet ITransientCollection
///         (ITransientCollection_3_conj_4_ => TransientHashSet_2_conj_4_)
///         (ITransientCollection_3_persistent_4_ => TransientHashSet_2_persistent_4_)
///     )

///     (defm TransientHashSet ITransientSet
///         (ITransientSet_3_disj_4_ => TransientHashSet_2_disj_4_)
///         (ITransientSet_3_contains_9_ => TransientHashSet_2_contains_9_)
///         (ITransientSet_3_get => TransientHashSet_2_get)
///     )

///     (defm TransientHashSet IFn
///         (IFn_3_invoke => ATransientSet_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )
// )

// (about #_"PersistentHashSet"
///     (defq PersistentHashSet [#_"IPersistentMap" _meta, #_"IPersistentMap" impl] SetForm
///         clojure.lang.IFn (invoke [_, a] (APersistentSet_2_invoke _, a))
///     )

///     #_inherit
///     (defm PersistentHashSet APersistentSet AFn)

///     (defn #_"PersistentHashSet" PersistentHashSet_1_new [#_"IPersistentMap" meta, #_"IPersistentMap" impl]
///         (new_8_ PersistentHashSet_1_class (anew [meta, impl]))
///     )

///     (def #_"PersistentHashSet" PersistentHashSet_1_EMPTY (PersistentHashSet_1_new nil, PersistentHashMap_1_EMPTY))

///     (defn #_"PersistentHashSet" PersistentHashSet_1_create [#_"Seqable" init]
///         (into PersistentHashSet_1_EMPTY init)
///     )

///     (defn #_"PersistentHashSet" PersistentHashSet_1_createWithCheck [#_"Seqable" init]
///         (let [#_"ITransientSet" s (transient PersistentHashSet_1_EMPTY)]
///             (loop_when [s s #_"ISeq" q (seq init) #_"int" n 0] (some_9_ q) => (persistent_4_ s)
///                 (let [s (conj_4_ s (first q))]
///                     (when (= (count s) (inc n)) => (throw (str "duplicate key: " (first q)))
///                         (recur s (next q) (inc n))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"PersistentHashSet" PersistentHashSet_2_withMeta [#_"PersistentHashSet" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentHashSet_1_new meta, (_0_impl this))
///         )
///     )

///     (defn_ #_"int" PersistentHashSet_2_count [#_"PersistentHashSet" this]
///         (count (_0_impl this))
///     )

///     (defn_ #_"PersistentHashSet" PersistentHashSet_2_conj [#_"PersistentHashSet" this, #_"Object" val]
///         (if (contains_9_ (_0_impl this) val)
///             this
///             (PersistentHashSet_1_new (_0__meta this), (assoc (_0_impl this) val val))
///         )
///     )

///     (defn_ #_"PersistentHashSet" PersistentHashSet_2_empty [#_"PersistentHashSet" this]
///         (with_meta PersistentHashSet_1_EMPTY (_0__meta this))
///     )

///     (defn_ #_"IPersistentSet" PersistentHashSet_2_disj [#_"PersistentHashSet" this, #_"Object" key]
///         (if (contains_9_ (_0_impl this) key)
///             (PersistentHashSet_1_new (_0__meta this), (dissoc (_0_impl this) key))
///             this
///         )
///     )

///     (defn_ #_"bool" PersistentHashSet_2_contains_9_ [#_"PersistentHashSet" this, #_"Object" key]
///         (contains_9_ (_0_impl this) key)
///     )

///     (defn_ #_"Object" PersistentHashSet_2_get [#_"PersistentHashSet" this, #_"Object" key]
///         (get (_0_impl this) key)
///     )

///     (defn_ #_"ISeq" PersistentHashSet_2_seq [#_"PersistentHashSet" this]
///         (keys (_0_impl this))
///     )

///     (defn_ #_"ITransientCollection" PersistentHashSet_2_asTransient [#_"PersistentHashSet" this]
///         (TransientHashSet_1_new (transient (_0_impl this)))
///     )

///     (defm PersistentHashSet IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentHashSet IObj
///         (IObj_3_withMeta => PersistentHashSet_2_withMeta)
///     )

///     (defm PersistentHashSet Counted
///         (Counted_3_count => PersistentHashSet_2_count)
///     )

///     (defm PersistentHashSet IPersistentCollection
///         (IPersistentCollection_3_conj => PersistentHashSet_2_conj)
///         (IPersistentCollection_3_empty => PersistentHashSet_2_empty)
///     )

///     (defm PersistentHashSet IPersistentSet
///         (IPersistentSet_3_disj => PersistentHashSet_2_disj)
///         (IPersistentSet_3_contains_9_ => PersistentHashSet_2_contains_9_)
///         (IPersistentSet_3_get => PersistentHashSet_2_get)
///     )

///     (defm PersistentHashSet Seqable
///         (Seqable_3_seq => PersistentHashSet_2_seq)
///     )

///     (defm PersistentHashSet IEditableCollection
///         (IEditableCollection_3_asTransient => PersistentHashSet_2_asTransient)
///     )

///     (defm PersistentHashSet IFn
///         (IFn_3_invoke => APersistentSet_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm PersistentHashSet IObject
///         (IObject_3_equals => APersistentSet_2_equals)
///     )

///     (defm PersistentHashSet Hashed
///         (Hashed_3_hash => Murmur3_1_hashUnordered)
///     )
// )

/// (defn hash_set
///     ([] PersistentHashSet_1_EMPTY)
///     ([& keys] (PersistentHashSet_1_create keys))
/// )

/// (defn set [s] (if (set_9_ s) (with_meta s nil) (into (hash_set) s)))
}

namespace arbace {

// (about #_"TNode"
///     (defn #_"Object" TNode_2_kvreduce [#_"node" this, #_"IFn" f, #_"Object" r]
///         (or
///             (when (some_9_ (_0_left this))
///                 (let [r (INode_3_kvreduce (_0_left this), f, r)]
///                     (when (reduced_9_ r)
///                         r
///                     )
///                 )
///             )
///             (let [r (f r (key this) (val this))]
///                 (cond
///                     (reduced_9_ r)          r
///                     (some_9_ (_0_right this)) (INode_3_kvreduce (_0_right this), f, r)
///                     _0_else                 r
///                 )
///             )
///         )
///     )
// )

// (about #_"Black"
///     (defq Black [#_"Object" key])

///     #_inherit
///     (defm Black TNode AMapEntry APersistentVector AFn)

///     (defn #_"Black" Black_1_new [#_"Object" key]
///         (new_8_ Black_1_class (anew [key]))
///     )

///     (defn_ #_"node" Black_2_addLeft [#_"Black" this, #_"node" ins]
///         (ITNode_3_balanceLeft ins, this)
///     )

///     (defn_ #_"node" Black_2_addRight [#_"Black" this, #_"node" ins]
///         (ITNode_3_balanceRight ins, this)
///     )

///     (declare PersistentTreeMap_1_balanceLeftDel)

///     (defn_ #_"node" Black_2_removeLeft [#_"Black" this, #_"node" del]
///         (PersistentTreeMap_1_balanceLeftDel (_0_key this), (_0_val this), del, (_0_right this))
///     )

///     (declare PersistentTreeMap_1_balanceRightDel)

///     (defn_ #_"node" Black_2_removeRight [#_"Black" this, #_"node" del]
///         (PersistentTreeMap_1_balanceRightDel (_0_key this), (_0_val this), (_0_left this), del)
///     )

///     (defn_ #_"node" Black_2_blacken [#_"Black" this]
///         this
///     )

///     (declare Red_1_new)

///     (defn_ #_"node" Black_2_redden [#_"Black" this]
///         (Red_1_new (_0_key this))
///     )

///     (declare PersistentTreeMap_1_black)

///     (defn_ #_"node" Black_2_balanceLeft [#_"Black" this, #_"node" parent]
///         (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), this, (_0_right parent))
///     )

///     (defn_ #_"node" Black_2_balanceRight [#_"Black" this, #_"node" parent]
///         (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_left parent), this)
///     )

///     (defn_ #_"node" Black_2_replace [#_"Black" this, #_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         (PersistentTreeMap_1_black key, val, left, right)
///     )

///     (defm Black IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm Black ITNode
///         (ITNode_3_addLeft => Black_2_addLeft)
///         (ITNode_3_addRight => Black_2_addRight)
///         (ITNode_3_removeLeft => Black_2_removeLeft)
///         (ITNode_3_removeRight => Black_2_removeRight)
///         (ITNode_3_blacken => Black_2_blacken)
///         (ITNode_3_redden => Black_2_redden)
///         (ITNode_3_balanceLeft => Black_2_balanceLeft)
///         (ITNode_3_balanceRight => Black_2_balanceRight)
///         (ITNode_3_replace => Black_2_replace)
///     )

///     (defm Black Sequential)

///     (defm Black Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm Black Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm Black Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm Black Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm Black IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm Black Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm Black IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm Black Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"BlackVal"
///     (defq BlackVal [#_"Object" key, #_"Object" val]
///         java.util.Map$Entry (getKey [_] (_0_key _)) (getValue [_] (_0_val _))
///     )

///     #_inherit
///     (defm BlackVal Black TNode AMapEntry APersistentVector AFn)

///     (defn #_"BlackVal" BlackVal_1_new [#_"Object" key, #_"Object" val]
///         (new_8_ BlackVal_1_class (anew [key, val]))
///     )

///     (declare RedVal_1_new)

///     (defn_ #_"node" BlackVal_2_redden [#_"BlackVal" this]
///         (RedVal_1_new (_0_key this), (_0_val this))
///     )

///     (defm BlackVal IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm BlackVal ITNode
///         (ITNode_3_addLeft => Black_2_addLeft)
///         (ITNode_3_addRight => Black_2_addRight)
///         (ITNode_3_removeLeft => Black_2_removeLeft)
///         (ITNode_3_removeRight => Black_2_removeRight)
///         (ITNode_3_blacken => Black_2_blacken)
///         (ITNode_3_redden => BlackVal_2_redden)
///         (ITNode_3_balanceLeft => Black_2_balanceLeft)
///         (ITNode_3_balanceRight => Black_2_balanceRight)
///         (ITNode_3_replace => Black_2_replace)
///     )

///     (defm BlackVal Sequential)

///     (defm BlackVal Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm BlackVal Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm BlackVal Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm BlackVal Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm BlackVal IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm BlackVal Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm BlackVal IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm BlackVal Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"BlackBranch"
///     (defq BlackBranch [#_"Object" key, #_"node" left, #_"node" right])

///     #_inherit
///     (defm BlackBranch Black TNode AMapEntry APersistentVector AFn)

///     (defn #_"BlackBranch" BlackBranch_1_new [#_"Object" key, #_"node" left, #_"node" right]
///         (new_8_ BlackBranch_1_class (anew [key, left, right]))
///     )

///     (declare RedBranch_1_new)

///     (defn_ #_"node" BlackBranch_2_redden [#_"BlackBranch" this]
///         (RedBranch_1_new (_0_key this), (_0_left this), (_0_right this))
///     )

///     (defm BlackBranch IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm BlackBranch ITNode
///         (ITNode_3_addLeft => Black_2_addLeft)
///         (ITNode_3_addRight => Black_2_addRight)
///         (ITNode_3_removeLeft => Black_2_removeLeft)
///         (ITNode_3_removeRight => Black_2_removeRight)
///         (ITNode_3_blacken => Black_2_blacken)
///         (ITNode_3_redden => BlackBranch_2_redden)
///         (ITNode_3_balanceLeft => Black_2_balanceLeft)
///         (ITNode_3_balanceRight => Black_2_balanceRight)
///         (ITNode_3_replace => Black_2_replace)
///     )

///     (defm BlackBranch Sequential)

///     (defm BlackBranch Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm BlackBranch Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm BlackBranch Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm BlackBranch Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm BlackBranch IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm BlackBranch Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm BlackBranch IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm BlackBranch Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"BlackBranchVal"
///     (defq BlackBranchVal [#_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         java.util.Map$Entry (getKey [_] (_0_key _)) (getValue [_] (_0_val _))
///     )

///     #_inherit
///     (defm BlackBranchVal BlackBranch Black TNode AMapEntry APersistentVector AFn)

///     (defn #_"BlackBranchVal" BlackBranchVal_1_new [#_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         (new_8_ BlackBranchVal_1_class (anew [key, val, left, right]))
///     )

///     (declare RedBranchVal_1_new)

///     (defn_ #_"node" BlackBranchVal_2_redden [#_"BlackBranchVal" this]
///         (RedBranchVal_1_new (_0_key this), (_0_val this), (_0_left this), (_0_right this))
///     )

///     (defm BlackBranchVal IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm BlackBranchVal ITNode
///         (ITNode_3_addLeft => Black_2_addLeft)
///         (ITNode_3_addRight => Black_2_addRight)
///         (ITNode_3_removeLeft => Black_2_removeLeft)
///         (ITNode_3_removeRight => Black_2_removeRight)
///         (ITNode_3_blacken => Black_2_blacken)
///         (ITNode_3_redden => BlackBranchVal_2_redden)
///         (ITNode_3_balanceLeft => Black_2_balanceLeft)
///         (ITNode_3_balanceRight => Black_2_balanceRight)
///         (ITNode_3_replace => Black_2_replace)
///     )

///     (defm BlackBranchVal Sequential)

///     (defm BlackBranchVal Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm BlackBranchVal Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm BlackBranchVal Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm BlackBranchVal Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm BlackBranchVal IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm BlackBranchVal Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm BlackBranchVal IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm BlackBranchVal Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"Red"
///     (defq Red [#_"Object" key])

///     #_inherit
///     (defm Red TNode AMapEntry APersistentVector AFn)

///     (defn #_"Red" Red_1_new [#_"Object" key]
///         (new_8_ Red_1_class (anew [key]))
///     )

///     (declare PersistentTreeMap_1_red)

///     (defn_ #_"node" Red_2_addLeft [#_"Red" this, #_"node" ins]
///         (PersistentTreeMap_1_red (_0_key this), (_0_val this), ins, (_0_right this))
///     )

///     (defn_ #_"node" Red_2_addRight [#_"Red" this, #_"node" ins]
///         (PersistentTreeMap_1_red (_0_key this), (_0_val this), (_0_left this), ins)
///     )

///     (defn_ #_"node" Red_2_removeLeft [#_"Red" this, #_"node" del]
///         (PersistentTreeMap_1_red (_0_key this), (_0_val this), del, (_0_right this))
///     )

///     (defn_ #_"node" Red_2_removeRight [#_"Red" this, #_"node" del]
///         (PersistentTreeMap_1_red (_0_key this), (_0_val this), (_0_left this), del)
///     )

///     (defn_ #_"node" Red_2_blacken [#_"Red" this]
///         (Black_1_new (_0_key this))
///     )

///     (defn_ #_"node" Red_2_redden [#_"Red" this]
///         (throw "invariant violation")
///     )

///     (defn_ #_"node" Red_2_balanceLeft [#_"Red" this, #_"node" parent]
///         (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), this, (_0_right parent))
///     )

///     (defn_ #_"node" Red_2_balanceRight [#_"Red" this, #_"node" parent]
///         (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_left parent), this)
///     )

///     (defn_ #_"node" Red_2_replace [#_"Red" this, #_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         (PersistentTreeMap_1_red key, val, left, right)
///     )

///     (defm Red IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm Red ITNode
///         (ITNode_3_addLeft => Red_2_addLeft)
///         (ITNode_3_addRight => Red_2_addRight)
///         (ITNode_3_removeLeft => Red_2_removeLeft)
///         (ITNode_3_removeRight => Red_2_removeRight)
///         (ITNode_3_blacken => Red_2_blacken)
///         (ITNode_3_redden => Red_2_redden)
///         (ITNode_3_balanceLeft => Red_2_balanceLeft)
///         (ITNode_3_balanceRight => Red_2_balanceRight)
///         (ITNode_3_replace => Red_2_replace)
///     )

///     (defm Red Sequential)

///     (defm Red Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm Red Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm Red Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm Red Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm Red IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm Red Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm Red IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm Red Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"RedVal"
///     (defq RedVal [#_"Object" key, #_"Object" val]
///         java.util.Map$Entry (getKey [_] (_0_key _)) (getValue [_] (_0_val _))
///     )

///     #_inherit
///     (defm RedVal Red TNode AMapEntry APersistentVector AFn)

///     (defn #_"RedVal" RedVal_1_new [#_"Object" key, #_"Object" val]
///         (new_8_ RedVal_1_class (anew [key, val]))
///     )

///     (defn_ #_"node" RedVal_2_blacken [#_"RedVal" this]
///         (BlackVal_1_new (_0_key this), (_0_val this))
///     )

///     (defm RedVal IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm RedVal ITNode
///         (ITNode_3_addLeft => Red_2_addLeft)
///         (ITNode_3_addRight => Red_2_addRight)
///         (ITNode_3_removeLeft => Red_2_removeLeft)
///         (ITNode_3_removeRight => Red_2_removeRight)
///         (ITNode_3_blacken => RedVal_2_blacken)
///         (ITNode_3_redden => Red_2_redden)
///         (ITNode_3_balanceLeft => Red_2_balanceLeft)
///         (ITNode_3_balanceRight => Red_2_balanceRight)
///         (ITNode_3_replace => Red_2_replace)
///     )

///     (defm RedVal Sequential)

///     (defm RedVal Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm RedVal Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm RedVal Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm RedVal Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm RedVal IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm RedVal Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm RedVal IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm RedVal Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"RedBranch"
///     (defq RedBranch [#_"Object" key, #_"node" left, #_"node" right])

///     #_inherit
///     (defm RedBranch Red TNode AMapEntry APersistentVector AFn)

///     (defn #_"RedBranch" RedBranch_1_new [#_"Object" key, #_"node" left, #_"node" right]
///         (new_8_ RedBranch_1_class (anew [key, left, right]))
///     )

///     (defn_ #_"node" RedBranch_2_blacken [#_"RedBranch" this]
///         (BlackBranch_1_new (_0_key this), (_0_left this), (_0_right this))
///     )

///     (defn_ #_"node" RedBranch_2_balanceLeft [#_"RedBranch" this, #_"node" parent]
///         (cond (satisfies_9_ Red (_0_left this))
///             (do
///                 (PersistentTreeMap_1_red (_0_key this), (_0_val this), (ITNode_3_blacken (_0_left this)), (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_right this), (_0_right parent)))
///             )
///             (satisfies_9_ Red (_0_right this))
///             (do
///                 (PersistentTreeMap_1_red (_0_key (_0_right this)), (_0_val (_0_right this)), (PersistentTreeMap_1_black (_0_key this), (_0_val this), (_0_left this), (_0_left (_0_right this))), (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_right (_0_right this)), (_0_right parent)))
///             )
///             _0_else
///             (do
///                 (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), this, (_0_right parent))
///             )
///         )
///     )

///     (defn_ #_"node" RedBranch_2_balanceRight [#_"RedBranch" this, #_"node" parent]
///         (cond (satisfies_9_ Red (_0_right this))
///             (do
///                 (PersistentTreeMap_1_red (_0_key this), (_0_val this), (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_left parent), (_0_left this)), (ITNode_3_blacken (_0_right this)))
///             )
///             (satisfies_9_ Red (_0_left this))
///             (do
///                 (PersistentTreeMap_1_red (_0_key (_0_left this)), (_0_val (_0_left this)), (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_left parent), (_0_left (_0_left this))), (PersistentTreeMap_1_black (_0_key this), (_0_val this), (_0_right (_0_left this)), (_0_right this)))
///             )
///             _0_else
///             (do
///                 (PersistentTreeMap_1_black (_0_key parent), (_0_val parent), (_0_left parent), this)
///             )
///         )
///     )

///     (defm RedBranch IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm RedBranch ITNode
///         (ITNode_3_addLeft => Red_2_addLeft)
///         (ITNode_3_addRight => Red_2_addRight)
///         (ITNode_3_removeLeft => Red_2_removeLeft)
///         (ITNode_3_removeRight => Red_2_removeRight)
///         (ITNode_3_blacken => RedBranch_2_blacken)
///         (ITNode_3_redden => Red_2_redden)
///         (ITNode_3_balanceLeft => RedBranch_2_balanceLeft)
///         (ITNode_3_balanceRight => RedBranch_2_balanceRight)
///         (ITNode_3_replace => Red_2_replace)
///     )

///     (defm RedBranch Sequential)

///     (defm RedBranch Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm RedBranch Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm RedBranch Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm RedBranch Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm RedBranch IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm RedBranch Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm RedBranch IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm RedBranch Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"RedBranchVal"
///     (defq RedBranchVal [#_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         java.util.Map$Entry (getKey [_] (_0_key _)) (getValue [_] (_0_val _))
///     )

///     #_inherit
///     (defm RedBranchVal RedBranch Red TNode AMapEntry APersistentVector AFn)

///     (defn #_"RedBranchVal" RedBranchVal_1_new [#_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         (new_8_ RedBranchVal_1_class (anew [key, val, left, right]))
///     )

///     (defn_ #_"node" RedBranchVal_2_blacken [#_"RedBranchVal" this]
///         (BlackBranchVal_1_new (_0_key this), (_0_val this), (_0_left this), (_0_right this))
///     )

///     (defm RedBranchVal IMapEntry
///         (IMapEntry_3_key => _0_key)
///         (IMapEntry_3_val => _0_val)
///     )

///     (defm RedBranchVal ITNode
///         (ITNode_3_addLeft => Red_2_addLeft)
///         (ITNode_3_addRight => Red_2_addRight)
///         (ITNode_3_removeLeft => Red_2_removeLeft)
///         (ITNode_3_removeRight => Red_2_removeRight)
///         (ITNode_3_blacken => RedBranchVal_2_blacken)
///         (ITNode_3_redden => Red_2_redden)
///         (ITNode_3_balanceLeft => RedBranch_2_balanceLeft)
///         (ITNode_3_balanceRight => RedBranch_2_balanceRight)
///         (ITNode_3_replace => Red_2_replace)
///     )

///     (defm RedBranchVal Sequential)

///     (defm RedBranchVal Indexed
///         (Indexed_3_nth => AMapEntry_2_nth)
///     )

///     (defm RedBranchVal Counted
///         (Counted_3_count => AMapEntry_2_count)
///     )

///     (defm RedBranchVal Seqable
///         (Seqable_3_seq => AMapEntry_2_seq)
///     )

///     (defm RedBranchVal Reversible
///         (Reversible_3_rseq => AMapEntry_2_rseq)
///     )

///     (defm RedBranchVal IObject
///         (IObject_3_equals => AMapEntry_2_equals)
///     )

///     (defm RedBranchVal Hashed
///         (Hashed_3_hash => AMapEntry_2_hash)
///     )

///     (defm RedBranchVal IKVReduce
///         (IKVReduce_3_kvreduce => TNode_2_kvreduce)
///     )

///     (defm RedBranchVal Comparable
///         (Comparable_3_compareTo => AMapEntry_2_compareTo)
///     )
// )

// (about #_"TSeq"
///     (declare TSeq_2_seq TSeq_2_first TSeq_2_next)

///     (defq TSeq [#_"IPersistentMap" _meta, #_"ISeq" stack, #_"bool" asc_9_, #_"int" cnt] SeqForm
///         clojure.lang.ISeq (seq [_] (TSeq_2_seq _)) (first [_] (TSeq_2_first _)) (next [_] (TSeq_2_next _)) (more [_] (or (TSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm TSeq ASeq)

///     (defn #_"TSeq" TSeq_1_new
///         ([#_"ISeq" stack, #_"bool" asc_9_] (TSeq_1_new stack, asc_9_, -1))
///         ([#_"ISeq" stack, #_"bool" asc_9_, #_"int" cnt] (TSeq_1_new nil, stack, asc_9_, cnt))
///         ([#_"IPersistentMap" meta, #_"ISeq" stack, #_"bool" asc_9_, #_"int" cnt]
///             (new_8_ TSeq_1_class (anew [meta, stack, asc_9_, cnt]))
///         )
///     )

///     (defn_ #_"TSeq" TSeq_2_withMeta [#_"TSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (TSeq_1_new meta, (_0_stack this), (_0_asc_9_ this), (_0_cnt this))
///         )
///     )

///     (defn #_"ISeq" TSeq_1_push [#_"node" t, #_"ISeq" stack, #_"bool" asc_9_]
///         (loop_when [stack stack t t] (some_9_ t) => stack
///             (recur (cons t stack) (if asc_9_ (_0_left t) (_0_right t)))
///         )
///     )

///     (defn #_"TSeq" TSeq_1_create [#_"node" t, #_"bool" asc_9_, #_"int" cnt]
///         (TSeq_1_new (TSeq_1_push t, nil, asc_9_), asc_9_, cnt)
///     )

///     (defn_ #_"ISeq" TSeq_2_seq [#_"TSeq" this]
///         this
///     )

///     (defn_ #_"Object" TSeq_2_first [#_"TSeq" this]
///         (first (_0_stack this))
///     )

///     (defn_ #_"ISeq" TSeq_2_next [#_"TSeq" this]
///         (let [#_"node" t #_"node" (first (_0_stack this)) #_"bool" asc_9_ (_0_asc_9_ this)]
///             (when_some [#_"ISeq" stack (TSeq_1_push (if asc_9_ (_0_right t) (_0_left t)), (next (_0_stack this)), asc_9_)]
///                 (TSeq_1_new stack, asc_9_, (dec (_0_cnt this)))
///             )
///         )
///     )

///     (defn_ #_"int" TSeq_2_count [#_"TSeq" this]
///         (when (neg_9_ (_0_cnt this)) => (_0_cnt this)
///             (count (_0_stack this))
///         )
///     )

///     (defm TSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm TSeq IObj
///         (IObj_3_withMeta => TSeq_2_withMeta)
///     )

///     (defm TSeq Sequential)

///     (defm TSeq Seqable
///         (Seqable_3_seq => TSeq_2_seq)
///     )

///     (defm TSeq ISeq
///         (ISeq_3_first => TSeq_2_first)
///         (ISeq_3_next => TSeq_2_next)
///     )

///     (defm TSeq Counted
///         (Counted_3_count => TSeq_2_count)
///     )

///     (defm TSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm TSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

// (about #_"PersistentTreeMap"
///     (declare PersistentTreeMap_2_seq)

///     (defq PersistentTreeMap [#_"IPersistentMap" _meta, #_"Comparator" cmp, #_"node" tree, #_"int" cnt] MapForm
///         clojure.lang.Seqable (seq [_] (PersistentTreeMap_2_seq _))
///         java.util.Map (entrySet [_] (-/into #{} _))
///     )

///     #_inherit
///     (defm PersistentTreeMap APersistentMap AFn)

///     (defn #_"PersistentTreeMap" PersistentTreeMap_1_new
///         ([] (PersistentTreeMap_1_new compare))
///         ([#_"Comparator" cmp] (PersistentTreeMap_1_new nil, cmp))
///         ([#_"IPersistentMap" meta, #_"Comparator" cmp] (PersistentTreeMap_1_new meta, cmp, nil, 0))
///         ([#_"IPersistentMap" meta, #_"Comparator" cmp, #_"node" tree, #_"int" cnt]
///             (new_8_ PersistentTreeMap_1_class (anew [meta, cmp, tree, cnt]))
///         )
///     )

///     (def #_"PersistentTreeMap" PersistentTreeMap_1_EMPTY (PersistentTreeMap_1_new))

///     (defn #_"PersistentTreeMap" PersistentTreeMap_1_create
///         ([#_"Seqable" keyvals] (PersistentTreeMap_1_create nil, keyvals))
///         ([#_"Comparator" cmp, #_"Seqable" keyvals]
///             (let [#_"PersistentTreeMap" m (if (some_9_ cmp) (PersistentTreeMap_1_new cmp) PersistentTreeMap_1_EMPTY)]
///                 (loop_when [m m #_"ISeq" s (seq keyvals)] (some_9_ s) => m
///                     (when (some_9_ (next s)) => (throw (str "no value supplied for key: " (first s)))
///                         (recur (assoc m (first s) (second s)) (next (next s)))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentCollection" PersistentTreeMap_2_empty [#_"PersistentTreeMap" this]
///         (PersistentTreeMap_1_new (_0__meta this), (_0_cmp this))
///     )

///     (defn_ #_"PersistentTreeMap" PersistentTreeMap_2_withMeta [#_"PersistentTreeMap" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentTreeMap_1_new meta, (_0_cmp this), (_0_tree this), (_0_cnt this))
///         )
///     )

///     (defn_ #_"int" PersistentTreeMap_2_doCompare [#_"PersistentTreeMap" this, #_"Object" a, #_"Object" b]
///         (Comparator_3_compare (_0_cmp this), a, b)
///     )

///     (defn_ #_"Object" PersistentTreeMap_2_entryKey [#_"PersistentTreeMap" this, #_"pair" entry]
///         (key entry)
///     )

///     (defn_ #_"ISeq" PersistentTreeMap_2_seq
///         ([#_"PersistentTreeMap" this] (PersistentTreeMap_2_seq this, true))
///         ([#_"PersistentTreeMap" this, #_"bool" ascending_9_]
///             (when (pos_9_ (_0_cnt this))
///                 (TSeq_1_create (_0_tree this), ascending_9_, (_0_cnt this))
///             )
///         )
///     )

///     (defn_ #_"ISeq" PersistentTreeMap_2_seqFrom [#_"PersistentTreeMap" this, #_"Object" key, #_"bool" ascending_9_]
///         (when (pos_9_ (_0_cnt this))
///             (loop_when [#_"ISeq" s nil #_"node" t (_0_tree this)] (some_9_ t) => (when (some_9_ s) (TSeq_1_new s, ascending_9_))
///                 (let [#_"int" cmp (PersistentTreeMap_2_doCompare this, key, (_0_key t))]
///                     (cond
///                         (zero_9_ cmp) (TSeq_1_new (cons t s), ascending_9_)
///                         ascending_9_  (if (neg_9_ cmp) (recur (cons t s) (_0_left t)) (recur s (_0_right t)))
///                         _0_else       (if (pos_9_ cmp) (recur (cons t s) (_0_right t)) (recur s (_0_left t)))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"ISeq" PersistentTreeMap_2_rseq [#_"PersistentTreeMap" this]
///         (PersistentTreeMap_2_seq this, false)
///     )

///     (defn_ #_"node" PersistentTreeMap_2_entryAt [#_"PersistentTreeMap" this, #_"Object" key]
///         (loop_when [#_"node" t (_0_tree this)] (some_9_ t) => t
///             (let [#_"int" cmp (PersistentTreeMap_2_doCompare this, key, (_0_key t))]
///                 (cond
///                     (neg_9_ cmp) (recur (_0_left t))
///                     (pos_9_ cmp) (recur (_0_right t))
///                     _0_else      t
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentTreeMap_2_valAt
///         ([#_"PersistentTreeMap" this, #_"Object" key] (PersistentTreeMap_2_valAt this, key, nil))
///         ([#_"PersistentTreeMap" this, #_"Object" key, #_"Object" not_found]
///             (when_some [#_"node" node (PersistentTreeMap_2_entryAt this, key)] => not_found
///                 (IMapEntry_3_val node)
///             )
///         )
///     )

///     (defn_ #_"bool" PersistentTreeMap_2_containsKey [#_"PersistentTreeMap" this, #_"Object" key]
///         (some_9_ (PersistentTreeMap_2_entryAt this, key))
///     )

///     (defn_ #_"Object" PersistentTreeMap_2_kvreduce [#_"PersistentTreeMap" this, #_"IFn" f, #_"Object" r]
///         (let [r (if (some_9_ (_0_tree this)) (INode_3_kvreduce (_0_tree this), f, r) r)]
///             (if (reduced_9_ r) (deref r) r)
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_2_min [#_"PersistentTreeMap" this]
///         (when_some [#_"node" t (_0_tree this)]
///             (loop_when_recur t (some_9_ (_0_left t)) (_0_left t) => t)
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_2_max [#_"PersistentTreeMap" this]
///         (when_some [#_"node" t (_0_tree this)]
///             (loop_when_recur t (some_9_ (_0_right t)) (_0_right t) => t)
///         )
///     )

///     (defn #_"Object" PersistentTreeMap_2_minKey [#_"PersistentTreeMap" this]
///         (let [#_"node" t (PersistentTreeMap_2_min this)]
///             (when (some_9_ t) (_0_key t))
///         )
///     )

///     (defn #_"Object" PersistentTreeMap_2_maxKey [#_"PersistentTreeMap" this]
///         (let [#_"node" t (PersistentTreeMap_2_max this)]
///             (when (some_9_ t) (_0_key t))
///         )
///     )

///     (defn #_"int" PersistentTreeMap_2_depth
///         ([#_"PersistentTreeMap" this] (PersistentTreeMap_2_depth this, (_0_tree this)))
///         ([#_"PersistentTreeMap" this, #_"node" t]
///             (when (some_9_ t) => 0
///                 (inc (max (PersistentTreeMap_2_depth this, (_0_left t)) (PersistentTreeMap_2_depth this, (_0_right t))))
///             )
///         )
///     )

///     (defn #_"Red" PersistentTreeMap_1_red [#_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         (if (and (nil_9_ left) (nil_9_ right))
///             (if (nil_9_ val)
///                 (Red_1_new key)
///                 (RedVal_1_new key, val)
///             )
///             (if (nil_9_ val)
///                 (RedBranch_1_new key, left, right)
///                 (RedBranchVal_1_new key, val, left, right)
///             )
///         )
///     )

///     (defn #_"Black" PersistentTreeMap_1_black [#_"Object" key, #_"Object" val, #_"node" left, #_"node" right]
///         (if (and (nil_9_ left) (nil_9_ right))
///             (if (nil_9_ val)
///                 (Black_1_new key)
///                 (BlackVal_1_new key, val)
///             )
///             (if (nil_9_ val)
///                 (BlackBranch_1_new key, left, right)
///                 (BlackBranchVal_1_new key, val, left, right)
///             )
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_1_rightBalance [#_"Object" key, #_"Object" val, #_"node" left, #_"node" ins]
///         (cond
///             (and (satisfies_9_ Red ins) (satisfies_9_ Red (_0_right ins)))
///                 (PersistentTreeMap_1_red (_0_key ins), (_0_val ins), (PersistentTreeMap_1_black key, val, left, (_0_left ins)), (ITNode_3_blacken (_0_right ins)))
///             (and (satisfies_9_ Red ins) (satisfies_9_ Red (_0_left ins)))
///                 (PersistentTreeMap_1_red (_0_key (_0_left ins)), (_0_val (_0_left ins)), (PersistentTreeMap_1_black key, val, left, (_0_left (_0_left ins))), (PersistentTreeMap_1_black (_0_key ins), (_0_val ins), (_0_right (_0_left ins)), (_0_right ins)))
///             _0_else
///                 (PersistentTreeMap_1_black key, val, left, ins)
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_1_balanceLeftDel [#_"Object" key, #_"Object" val, #_"node" del, #_"node" right]
///         (cond
///             (satisfies_9_ Red del)
///                 (PersistentTreeMap_1_red key, val, (ITNode_3_blacken del), right)
///             (satisfies_9_ Black right)
///                 (PersistentTreeMap_1_rightBalance key, val, del, (ITNode_3_redden right))
///             (and (satisfies_9_ Red right) (satisfies_9_ Black (_0_left right)))
///                 (PersistentTreeMap_1_red (_0_key (_0_left right)), (_0_val (_0_left right)), (PersistentTreeMap_1_black key, val, del, (_0_left (_0_left right))), (PersistentTreeMap_1_rightBalance (_0_key right), (_0_val right), (_0_right (_0_left right)), (ITNode_3_redden (_0_right right))))
///             _0_else
///                 (throw "invariant violation")
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_1_leftBalance [#_"Object" key, #_"Object" val, #_"node" ins, #_"node" right]
///         (cond
///             (and (satisfies_9_ Red ins) (satisfies_9_ Red (_0_left ins)))
///                 (PersistentTreeMap_1_red (_0_key ins), (_0_val ins), (ITNode_3_blacken (_0_left ins)), (PersistentTreeMap_1_black key, val, (_0_right ins), right))
///             (and (satisfies_9_ Red ins) (satisfies_9_ Red (_0_right ins)))
///                 (PersistentTreeMap_1_red (_0_key (_0_right ins)), (_0_val (_0_right ins)), (PersistentTreeMap_1_black (_0_key ins), (_0_val ins), (_0_left ins), (_0_left (_0_right ins))), (PersistentTreeMap_1_black key, val, (_0_right (_0_right ins)), right))
///             _0_else
///                 (PersistentTreeMap_1_black key, val, ins, right)
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_1_balanceRightDel [#_"Object" key, #_"Object" val, #_"node" left, #_"node" del]
///         (cond
///             (satisfies_9_ Red del)
///                 (PersistentTreeMap_1_red key, val, left, (ITNode_3_blacken del))
///             (satisfies_9_ Black left)
///                 (PersistentTreeMap_1_leftBalance key, val, (ITNode_3_redden left), del)
///             (and (satisfies_9_ Red left) (satisfies_9_ Black (_0_right left)))
///                 (PersistentTreeMap_1_red (_0_key (_0_right left)), (_0_val (_0_right left)), (PersistentTreeMap_1_leftBalance (_0_key left), (_0_val left), (ITNode_3_redden (_0_left left)), (_0_left (_0_right left))), (PersistentTreeMap_1_black key, val, (_0_right (_0_right left)), del))
///             _0_else
///                 (throw "invariant violation")
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_2_add [#_"PersistentTreeMap" this, #_"node" t, #_"Object" key, #_"Object" val, #_"node'" found]
///         (if (nil_9_ t)
///             (if (nil_9_ val)
///                 (Red_1_new key)
///                 (RedVal_1_new key, val)
///             )
///             (let [#_"int" cmp (PersistentTreeMap_2_doCompare this, key, (_0_key t))]
///                 (if (zero_9_ cmp)
///                     (do
///                         (reset_4_ found t)
///                         nil
///                     )
///                     (let [#_"node" ins (PersistentTreeMap_2_add this, (if (neg_9_ cmp) (_0_left t) (_0_right t)), key, val, found)]
///                         (when (some_9_ ins) => nil
///                             (if (neg_9_ cmp) (ITNode_3_addLeft t, ins) (ITNode_3_addRight t, ins))
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_1_append [#_"node" left, #_"node" right]
///         (cond
///             (nil_9_ left)
///                 right
///             (nil_9_ right)
///                 left
///             (satisfies_9_ Red left)
///                 (if (satisfies_9_ Red right)
///                     (let [#_"node" app (PersistentTreeMap_1_append (_0_right left), (_0_left right))]
///                         (if (satisfies_9_ Red app)
///                             (PersistentTreeMap_1_red (_0_key app), (_0_val app), (PersistentTreeMap_1_red (_0_key left), (_0_val left), (_0_left left), (_0_left app)), (PersistentTreeMap_1_red (_0_key right), (_0_val right), (_0_right app), (_0_right right)))
///                             (PersistentTreeMap_1_red (_0_key left), (_0_val left), (_0_left left), (PersistentTreeMap_1_red (_0_key right), (_0_val right), app, (_0_right right)))
///                         )
///                     )
///                     (PersistentTreeMap_1_red (_0_key left), (_0_val left), (_0_left left), (PersistentTreeMap_1_append (_0_right left), right))
///                 )
///             (satisfies_9_ Red right)
///                 (PersistentTreeMap_1_red (_0_key right), (_0_val right), (PersistentTreeMap_1_append left, (_0_left right)), (_0_right right))
///             _0_else
///                 (let [#_"node" app (PersistentTreeMap_1_append (_0_right left), (_0_left right))]
///                     (if (satisfies_9_ Red app)
///                         (PersistentTreeMap_1_red (_0_key app), (_0_val app), (PersistentTreeMap_1_black (_0_key left), (_0_val left), (_0_left left), (_0_left app)), (PersistentTreeMap_1_black (_0_key right), (_0_val right), (_0_right app), (_0_right right)))
///                         (PersistentTreeMap_1_balanceLeftDel (_0_key left), (_0_val left), (_0_left left), (PersistentTreeMap_1_black (_0_key right), (_0_val right), app, (_0_right right)))
///                     )
///                 )
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_2_remove [#_"PersistentTreeMap" this, #_"node" t, #_"Object" key, #_"node'" found]
///         (when (some_9_ t) => nil
///             (let [#_"int" cmp (PersistentTreeMap_2_doCompare this, key, (_0_key t))]
///                 (if (zero_9_ cmp)
///                     (do
///                         (reset_4_ found t)
///                         (PersistentTreeMap_1_append (_0_left t), (_0_right t))
///                     )
///                     (let [#_"node" del (PersistentTreeMap_2_remove this, (if (neg_9_ cmp) (_0_left t) (_0_right t)), key, found)]
///                         (when (or (some_9_ del) (some_9_ (deref found))) => nil
///                             (if (neg_9_ cmp)
///                                 (if (satisfies_9_ Black (_0_left t))
///                                     (PersistentTreeMap_1_balanceLeftDel (_0_key t), (_0_val t), del, (_0_right t))
///                                     (PersistentTreeMap_1_red (_0_key t), (_0_val t), del, (_0_right t))
///                                 )
///                                 (if (satisfies_9_ Black (_0_right t))
///                                     (PersistentTreeMap_1_balanceRightDel (_0_key t), (_0_val t), (_0_left t), del)
///                                     (PersistentTreeMap_1_red (_0_key t), (_0_val t), (_0_left t), del)
///                                 )
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" PersistentTreeMap_2_replace [#_"PersistentTreeMap" this, #_"node" t, #_"Object" key, #_"Object" val]
///         (let [
///             #_"int" cmp (PersistentTreeMap_2_doCompare this, key, (_0_key t))
///             #_"node" left  (if (neg_9_ cmp) (PersistentTreeMap_2_replace this, (_0_left  t), key, val) (_0_left  t))
///             #_"node" right (if (pos_9_ cmp) (PersistentTreeMap_2_replace this, (_0_right t), key, val) (_0_right t))
///         ]
///             (ITNode_3_replace t, (_0_key t), (if (zero_9_ cmp) val (_0_val t)), left, right)
///         )
///     )

///     (defn_ #_"PersistentTreeMap" PersistentTreeMap_2_assoc [#_"PersistentTreeMap" this, #_"Object" key, #_"Object" val]
///         (let [#_"node'" found (atom nil) #_"node" t (PersistentTreeMap_2_add this, (_0_tree this), key, val, found)]
///             (if (nil_9_ t)
///                 (if (= (_0_val #_"node" (deref found)) val)
///                     this
///                     (PersistentTreeMap_1_new (_0__meta this), (_0_cmp this), (PersistentTreeMap_2_replace this, (_0_tree this), key, val), (_0_cnt this))
///                 )
///                 (PersistentTreeMap_1_new (_0__meta this), (_0_cmp this), (ITNode_3_blacken t), (inc (_0_cnt this)))
///             )
///         )
///     )

///     (defn_ #_"PersistentTreeMap" PersistentTreeMap_2_dissoc [#_"PersistentTreeMap" this, #_"Object" key]
///         (let [#_"node'" found (atom nil) #_"node" t (PersistentTreeMap_2_remove this, (_0_tree this), key, found)]
///             (if (nil_9_ t)
///                 (if (nil_9_ (deref found))
///                     this
///                     (PersistentTreeMap_1_new (_0__meta this), (_0_cmp this))
///                 )
///                 (PersistentTreeMap_1_new (_0__meta this), (_0_cmp this), (ITNode_3_blacken t), (dec (_0_cnt this)))
///             )
///         )
///     )

///     (defm PersistentTreeMap IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentTreeMap IObj
///         (IObj_3_withMeta => PersistentTreeMap_2_withMeta)
///     )

///     (defm PersistentTreeMap Counted
///         (Counted_3_count => _0_cnt)
///     )

///     (defm PersistentTreeMap ILookup
///         (ILookup_3_valAt => PersistentTreeMap_2_valAt)
///     )

///     (defm PersistentTreeMap IFn
///         (IFn_3_invoke => APersistentMap_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm PersistentTreeMap Seqable
///         (Seqable_3_seq => PersistentTreeMap_2_seq)
///     )

///     (defm PersistentTreeMap Reversible
///         (Reversible_3_rseq => PersistentTreeMap_2_rseq)
///     )

///     (defm PersistentTreeMap IPersistentCollection
///         (IPersistentCollection_3_conj => APersistentMap_2_conj)
///         (IPersistentCollection_3_empty => PersistentTreeMap_2_empty)
///     )

///     (defm PersistentTreeMap Sorted
///         (Sorted_3_comparator => _0_cmp)
///         (Sorted_3_entryKey => PersistentTreeMap_2_entryKey)
///         (Sorted_3_seq => PersistentTreeMap_2_seq)
///         (Sorted_3_seqFrom => PersistentTreeMap_2_seqFrom)
///     )

///     (defm PersistentTreeMap IKVReduce
///         (IKVReduce_3_kvreduce => PersistentTreeMap_2_kvreduce)
///     )

///     (defm PersistentTreeMap Associative
///         (Associative_3_assoc => PersistentTreeMap_2_assoc)
///         (Associative_3_containsKey => PersistentTreeMap_2_containsKey)
///         (Associative_3_entryAt => PersistentTreeMap_2_entryAt)
///     )

///     (defm PersistentTreeMap IPersistentMap
///         (IPersistentMap_3_dissoc => PersistentTreeMap_2_dissoc)
///     )

///     (defm PersistentTreeMap IObject
///         (IObject_3_equals => APersistentMap_2_equals)
///     )

///     (defm PersistentTreeMap Hashed
///         (Hashed_3_hash => Murmur3_1_hashUnordered)
///     )
// )

/// (defn sorted_map [& keyvals] (PersistentTreeMap_1_create keyvals))

/// (defn sorted_map_by [cmp & keyvals] (PersistentTreeMap_1_create cmp keyvals))
}

namespace arbace {

// (about #_"PersistentTreeSet"
///     (defq PersistentTreeSet [#_"IPersistentMap" _meta, #_"IPersistentMap" impl] SetForm)

///     #_inherit
///     (defm PersistentTreeSet APersistentSet AFn)

///     (defn #_"PersistentTreeSet" PersistentTreeSet_1_new [#_"IPersistentMap" meta, #_"IPersistentMap" impl]
///         (new_8_ PersistentTreeSet_1_class (anew [meta, impl]))
///     )

///     (def #_"PersistentTreeSet" PersistentTreeSet_1_EMPTY (PersistentTreeSet_1_new nil, PersistentTreeMap_1_EMPTY))

///     (defn #_"PersistentTreeSet" PersistentTreeSet_1_create
///         ([                    #_"Seqable" init] (into PersistentTreeSet_1_EMPTY                                       init))
///         ([#_"Comparator" cmp, #_"Seqable" init] (into (PersistentTreeSet_1_new nil, (PersistentTreeMap_1_new nil, cmp)) init))
///     )

///     (defn_ #_"PersistentTreeSet" PersistentTreeSet_2_withMeta [#_"PersistentTreeSet" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentTreeSet_1_new meta, (_0_impl this))
///         )
///     )

///     (defn_ #_"int" PersistentTreeSet_2_count [#_"PersistentTreeSet" this]
///         (count (_0_impl this))
///     )

///     (defn_ #_"PersistentTreeSet" PersistentTreeSet_2_conj [#_"PersistentTreeSet" this, #_"Object" val]
///         (if (contains_9_ (_0_impl this) val)
///             this
///             (PersistentTreeSet_1_new (_0__meta this), (assoc (_0_impl this) val val))
///         )
///     )

///     (defn_ #_"PersistentTreeSet" PersistentTreeSet_2_empty [#_"PersistentTreeSet" this]
///         (PersistentTreeSet_1_new (_0__meta this), (empty (_0_impl this)))
///     )

///     (defn_ #_"IPersistentSet" PersistentTreeSet_2_disj [#_"PersistentTreeSet" this, #_"Object" key]
///         (if (contains_9_ (_0_impl this) key)
///             (PersistentTreeSet_1_new (_0__meta this), (dissoc (_0_impl this) key))
///             this
///         )
///     )

///     (defn_ #_"bool" PersistentTreeSet_2_contains_9_ [#_"PersistentTreeSet" this, #_"Object" key]
///         (contains_9_ (_0_impl this) key)
///     )

///     (defn_ #_"Object" PersistentTreeSet_2_get [#_"PersistentTreeSet" this, #_"Object" key]
///         (get (_0_impl this) key)
///     )

///     (defn_ #_"Comparator" PersistentTreeSet_2_comparator [#_"PersistentTreeSet" this]
///         (Sorted_3_comparator (_0_impl this))
///     )

///     (defn_ #_"Object" PersistentTreeSet_2_entryKey [#_"PersistentTreeSet" this, #_"Object" entry]
///         entry
///     )

///     (defn_ #_"ISeq" PersistentTreeSet_2_seq
///         ([#_"PersistentTreeSet" this]
///             (keys (_0_impl this))
///         )
///         ([#_"PersistentTreeSet" this, #_"bool" ascending_9_]
///             (keys (Sorted_3_seq (_0_impl this), ascending_9_))
///         )
///     )

///     (defn_ #_"ISeq" PersistentTreeSet_2_seqFrom [#_"PersistentTreeSet" this, #_"Object" key, #_"bool" ascending_9_]
///         (keys (Sorted_3_seqFrom (_0_impl this), key, ascending_9_))
///     )

///     (defn_ #_"ISeq" PersistentTreeSet_2_rseq [#_"PersistentTreeSet" this]
///         (map key (rseq (_0_impl this)))
///     )

///     (defm PersistentTreeSet IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentTreeSet IObj
///         (IObj_3_withMeta => PersistentTreeSet_2_withMeta)
///     )

///     (defm PersistentTreeSet Counted
///         (Counted_3_count => PersistentTreeSet_2_count)
///     )

///     (defm PersistentTreeSet IPersistentCollection
///         (IPersistentCollection_3_conj => PersistentTreeSet_2_conj)
///         (IPersistentCollection_3_empty => PersistentTreeSet_2_empty)
///     )

///     (defm PersistentTreeSet IPersistentSet
///         (IPersistentSet_3_disj => PersistentTreeSet_2_disj)
///         (IPersistentSet_3_contains_9_ => PersistentTreeSet_2_contains_9_)
///         (IPersistentSet_3_get => PersistentTreeSet_2_get)
///     )

///     (defm PersistentTreeSet Sorted
///         (Sorted_3_comparator => PersistentTreeSet_2_comparator)
///         (Sorted_3_entryKey => PersistentTreeSet_2_entryKey)
///         (Sorted_3_seq => PersistentTreeSet_2_seq)
///         (Sorted_3_seqFrom => PersistentTreeSet_2_seqFrom)
///     )

///     (defm PersistentTreeSet Seqable
///         (Seqable_3_seq => PersistentTreeSet_2_seq)
///     )

///     (defm PersistentTreeSet Reversible
///         (Reversible_3_rseq => PersistentTreeSet_2_rseq)
///     )

///     (defm PersistentTreeSet IFn
///         (IFn_3_invoke => APersistentSet_2_invoke)
///         (IFn_3_applyTo => AFn_1_applyTo)
///     )

///     (defm PersistentTreeSet IObject
///         (IObject_3_equals => APersistentSet_2_equals)
///     )

///     (defm PersistentTreeSet Hashed
///         (Hashed_3_hash => Murmur3_1_hashUnordered)
///     )
// )

/// (defn sorted_set [& keys] (PersistentTreeSet_1_create keys))

/// (defn sorted_set_by [cmp & keys] (PersistentTreeSet_1_create cmp keys))
}

namespace arbace {

// (about #_"VNode"
///     (defq VNode [#_"thread'" edit, #_"array" array, #_"index" index])

///     (defn #_"node" VNode_1_new [#_"thread'" edit, #_"array" array, #_"index" index]
///         (new_8_ VNode_1_class (anew [edit, (or array (anew 32)), index]))
///     )

///     (def #_"node" VNode_1_EMPTY (VNode_1_new nil, nil, nil))

///     (defn #_"void" VNode_2_assert_editable [#_"node" this]
///         (let [
///             #_"thread" owner (deref (or (_0_edit this) (throw "transient use of persistent data")))
///         ]
///             (when_not (identical_9_ (thread) owner)
///                 (if (some_9_ owner)
///                     (throw "transient used by non_owner thread")
///                     (throw "transient used after persistent! call")
///                 )
///             )
///         )
///         nil
///     )

///     (defn #_"bool" VNode_2_cow_9_ [#_"node" this, #_"thread'" edit]
///         (let [
///             #_"thread'" e (_0_edit this)
///         ]
///             (or (nil_9_ e) (nil_9_ (deref e)) (not (or (identical_9_ e edit) (throw "transient cow!"))))
///         )
///     )

///     (defn #_"node" VNode_2_editable_root [#_"node" this]
///         (VNode_1_new (atom (thread)), (aclone (_0_array this)), (aclone (_0_index this)))
///     )

///     (defn #_"values" VNode_1_editable_tail [#_"values" tail]
///         (-> (anew 32) (acopy_4_ 0 tail 0 (alength tail)))
///     )

///     (defn #_"values" VNode_2_array_for
///         ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt] (VNode_2_array_for this, i, shift, cnt, cnt, nil))
///         ([#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail_off, #_"values" tail]
///             (when (< -1 i cnt) => (throw "index is out of bounds")
///                 (when (< i tail_off) => tail
///                     (loop_when [i i #_"node" node this shift shift] (pos_9_ shift) => (_0_array node)
///                         (let [
///                             #_"index" x (_0_index node)
///                             #_"int" m (bit_and (>>> i shift) 0x1f)
///                             [m i]
///                                 (when (some_9_ x) => [m i]
///                                     (let [
///                                         m (loop_when_recur m (<= (aget x m) i) (inc m) => m)
///                                     ]
///                                         [m (if (pos_9_ m) (- i (aget x (dec m))) i)]
///                                     )
///                                 )
///                         ]
///                             (recur i (aget (_0_array node) m) (- shift 5))
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Object" VNode_2_value_for [#_"node" this, #_"int" i, #_"int" shift, #_"int" cnt, #_"int" tail_off, #_"values" tail]
///         (when (< -1 i cnt) => (throw "index is out of bounds")
///             (when (< i tail_off) => (aget tail (- i tail_off))
///                 (loop_when [i i #_"node" node this shift shift] (pos_9_ shift) => (aget (_0_array node) (bit_and (>>> i shift) 0x1f))
///                     (let [
///                         #_"index" x (_0_index node)
///                         #_"int" m (bit_and (>>> i shift) 0x1f)
///                         [m i]
///                             (when (some_9_ x) => [m i]
///                                 (let [
///                                     m (loop_when_recur m (<= (aget x m) i) (inc m) => m)
///                                 ]
///                                     [m (if (pos_9_ m) (- i (aget x (dec m))) i)]
///                                 )
///                             )
///                     ]
///                         (recur i (aget (_0_array node) m) (- shift 5))
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"node" VNode_2_new_path [#_"node" this, #_"thread'" edit, #_"int" shift]
///         (when (pos_9_ shift) => this
///             (VNode_1_new edit, (-> (anew 32) (aset_4_ 0 (VNode_2_new_path this, edit, (- shift 5)))), nil)
///         )
///     )

///     (defn #_"int" VNode_1_last_range [#_"index" x]
///         (aget x (dec (aget x 32)))
///     )

///     (defn #_"bool" VNode_2_overflow_9_ [#_"node" this, #_"int" shift, #_"int" cnt]
///         (let [
///             #_"index" x (_0_index this)
///         ]
///             (when (some_9_ x) => (< (<< 1 shift) (>>> (inc cnt) 5))
///                 (and (= (aget x 32) 32)
///                     (or (= shift 5)
///                         (recur
///                             (aget (_0_array this) 31)
///                             (- shift 5)
///                             (+ (- (aget x 31) (aget x 30)) 32)
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"node" VNode_2_push_tail [#_"node" this, #_"thread'" edit, #_"int" shift, #_"int" cnt, #_"node" tail_node]
///         (let [
///             #_"bool" cow_9_ (VNode_2_cow_9_ this, edit) #_"array" a (_0_array this) #_"index" x (_0_index this)
///         ]
///             (if (some_9_ x)
///                 (let [
///                     #_"int" e (dec (aget x 32))
///                     #_"node" child
///                         (when (< 5 shift)
///                             (let [
///                                 #_"int" n (if (pos_9_ e) (- (aget x e) (aget x (dec e))) (aget x 0))
///                             ]
///                                 (when (< n (<< 1 shift))
///                                     (VNode_2_push_tail (aget a e), edit, (- shift 5), (inc n), tail_node)
///                                 )
///                             )
///                         )
///                     a (if cow_9_ (aclone a) a) x (if cow_9_ (aclone x) x)
///                     [a x]
///                         (if (some_9_ child)
///                             [(aset_4_ a e child) (aswap_4_ x e + 32)]
///                             (let [
///                                 a (aset_4_ a (inc e) (VNode_2_new_path tail_node, edit, (- shift 5)))
///                                 x (aset_4_ x (inc e) (+ (aget x e) 32))
///                             ]
///                                 [a (aswap_4_ x 32 inc)]
///                             )
///                         )
///                 ]
///                     (if cow_9_ (VNode_1_new edit, a, x) this)
///                 )
///                 (let [
///                     #_"int" e (bit_and (>>> (dec cnt) shift) 0x1f)
///                     #_"node" child
///                         (when (< 5 shift) => tail_node
///                             (if_some [child (aget a e)]
///                                 (VNode_2_push_tail child, edit, (- shift 5), cnt, tail_node)
///                                 (VNode_2_new_path tail_node, edit, (- shift 5))
///                             )
///                         )
///                     a (if cow_9_ (aclone a) a)
///                     a (aset_4_ a e child)
///                 ]
///                     (if cow_9_ (VNode_1_new edit, a, nil) this)
///                 )
///             )
///         )
///     )

///     (defn #_"node" VNode_2_pop_tail [#_"node" this, #_"thread'" edit, #_"int" shift, #_"int" tail_off]
///         (let [
///             #_"bool" cow_9_ (VNode_2_cow_9_ this, edit) #_"array" a (_0_array this) #_"index" x (_0_index this)
///             #_"int" e (bit_and (>>> (dec tail_off) shift) 0x1f)
///         ]
///             (if (some_9_ x)
///                 (let [
///                     e (loop_when_recur e (and (< e 31) (some_9_ (aget x (inc e)))) (inc e) => e)
///                 ]
///                     (cond
///                         (< 5 shift)
///                             (let [
///                                 #_"node" child (aget a e)
///                                 #_"node" child_1_ (VNode_2_pop_tail child, edit, (- shift 5), (if (pos_9_ e) (- (aget x e) (aget x (dec e))) (aget x 0)))
///                             ]
///                                 (when (or (some_9_ child_1_) (pos_9_ e))
///                                     (let [
///                                         a (if cow_9_ (aclone a) a)
///                                         a (-> a (aset_4_ e child_1_))
///                                         x (if cow_9_ (aclone x) x)
///                                         x
///                                             (if (some_9_ child_1_)
///                                                 (let [
///                                                     #_"int" delta
///                                                         (when (some_9_ (_0_index child)) => 32
///                                                             (- (VNode_1_last_range (_0_index child)) (VNode_1_last_range (_0_index child_1_)))
///                                                         )
///                                                 ]
///                                                     (-> x (aswap_4_ e - delta))
///                                                 )
///                                                 (-> x (aset_4_ e nil) (aswap_4_ 32 dec))
///                                             )
///                                     ]
///                                         (if cow_9_ (VNode_1_new edit, a, x) this)
///                                     )
///                                 )
///                             )
///                         (pos_9_ e)
///                             (let [
///                                 a (-> (if cow_9_ (aclone a) a) (aset_4_ e nil))
///                                 x (-> (if cow_9_ (aclone x) x) (aset_4_ e nil) (aswap_4_ 32 dec))
///                             ]
///                                 (if cow_9_ (VNode_1_new edit, a, x) this)
///                             )
///                     )
///                 )
///                 (cond
///                     (< 5 shift)
///                         (let [
///                             #_"node" child (VNode_2_pop_tail (aget a e), edit, (- shift 5), tail_off)
///                         ]
///                             (when (or (some_9_ child) (pos_9_ e))
///                                 (let [
///                                     a (if cow_9_ (aclone a) a)
///                                     a (aset_4_ a e child)
///                                 ]
///                                     (if cow_9_ (VNode_1_new edit, a, nil) this)
///                                 )
///                             )
///                         )
///                     (pos_9_ e)
///                         (let [
///                             a (if cow_9_ (aclone a) a)
///                             a (aset_4_ a e nil)
///                         ]
///                             (if cow_9_ (VNode_1_new edit, a, nil) this)
///                         )
///                 )
///             )
///         )
///     )

///     (defn #_"node" VNode_2_do_assoc [#_"node" this, #_"thread'" edit, #_"int" shift, #_"int" i, #_"Object" val]
///         (let [
///             #_"bool" cow_9_ (VNode_2_cow_9_ this, edit) #_"array" a (_0_array this) #_"index" x (_0_index this)
///             a (if cow_9_ (aclone a) a)
///             #_"int" m (bit_and (>>> i shift) 0x1f)
///             a
///                 (when (pos_9_ shift) => (aset_4_ a m val)
///                     (let [
///                         [m i]
///                             (when (some_9_ x) => [m i]
///                                 (let [
///                                     m (loop_when_recur m (<= (aget x m) i) (inc m) => m)
///                                 ]
///                                     [m (if (pos_9_ m) (- i (aget x (dec m))) i)]
///                                 )
///                             )
///                     ]
///                         (aswap_4_ a m VNode_2_do_assoc edit, (- shift 5), i, val)
///                     )
///                 )
///         ]
///             (if cow_9_ (VNode_1_new edit, a, x) this)
///         )
///     )

///     (defn_ #_"index" VNode_1_n_index [#_"int" shift, #_"int" n]
///         (let [
///             #_"int" k (<< 1 shift)
///         ]
///             (loop_when_recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< i n) [(aset_4_ x j i) (inc j) (+ i k)] => (-> x (aset_4_ j n) (aset_4_ 32 (inc j))))
///         )
///     )

///     (defn_ #_"index" VNode_1_m_n_index [#_"int" shift, #_"int" m, #_"int" n]
///         (let [
///             #_"int" k (<< 1 shift)
///         ]
///             (loop_when_recur [#_"index" x (anew 33) #_"int" j 0 #_"int" i k] (< j m) [(aset_4_ x j i) (inc j) (+ i k)] => (-> x (aset_4_ j n) (aset_4_ 32 (inc j))))
///         )
///     )

///     (defn_ #_"int" VNode_1_index_of_nil [#_"array" a]
///         (loop_when [#_"int" l 0 #_"int" h 31] (< l (dec h)) => (cond (nil_9_ (aget a l)) l (nil_9_ (aget a h)) h _0_else 32)
///             (let [
///                 #_"int" m (+ l (>>> (- h l) 1))
///             ]
///                 (if (nil_9_ (aget a m))
///                     (recur l m)
///                     (recur (inc m) h)
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" VNode_2_first_child [#_"node" this]
///         (aget (_0_array this) 0)
///     )

///     (defn_ #_"node" VNode_2_last_child [#_"node" this]
///         (let [
///             #_"array" a (_0_array this) #_"index" x (_0_index this)
///         ]
///             (aget a (dec (if (some_9_ x) (aget x 32) (VNode_1_index_of_nil a))))
///         )
///     )

///     (defn_ #_"node" VNode_2_remove_leftmost_child [#_"node" this]
///         (let [
///             #_"array" a (_0_array this)
///         ]
///             (when (some_9_ (aget a 1))
///                 (let [
///                     #_"index" x (_0_index this)
///                     #_"index" x_1_
///                         (when (some_9_ x)
///                             (let [
///                                 #_"int" k (aget x 0)
///                                 #_"int" e (dec (aget x 32))
///                             ]
///                                 (loop_when_recur [x_1_ (anew 33) #_"int" j 0] (< j e) [(aset_4_ x_1_ j (- (aget x (inc j)) k)) (inc j)] => (aset_4_ x_1_ 32 e))
///                             )
///                         )
///                 ]
///                     (VNode_1_new nil, (-> (anew 32) (acopy_4_ 0 a 1 31)), x_1_)
///                 )
///             )
///         )
///     )

///     (defn_ #_"node" VNode_2_replace_leftmost_child [#_"node" this, #_"int" shift, #_"int" cnt, #_"node" node, #_"int" delta]
///         (let [
///             #_"array" a (_0_array this) #_"index" x (_0_index this)
///             [#_"array" a_1_ #_"index" x_1_]
///                 (if (some_9_ x)
///                     (let [
///                         #_"int" n (aget x 32)
///                         x_1_
///                             (loop_when_recur [x_1_ (anew 33) #_"int" j 0]
///                                              (< j n)
///                                              [(aset_4_ x_1_ j (- (aget x j) delta)) (inc j)]
///                                           => (aset_4_ x_1_ 32 n)
///                             )
///                     ]
///                         [(-> (aclone a) (aset_4_ 0 node)) x_1_]
///                     )
///                     (let [
///                         #_"int" k (<< 1 shift)
///                         #_"int" n (bit_and (>>> (dec cnt) shift) 0x1f)
///                         x_1_
///                             (loop_when_recur [x_1_ (-> (anew 33) (aset_4_ 0 (- k delta))) #_"int" j 0]
///                                              (< j n)
///                                              [(aset_4_ x_1_ (inc j) (+ (aget x_1_ j) k)) (inc j)]
///                                           => (-> x_1_ (aset_4_ n (- cnt delta)) (aset_4_ 32 (inc n)))
///                             )
///                     ]
///                         [(-> (anew 32) (aset_4_ 0 node) (acopy_4_ 1 a 1 n)) x_1_]
///                     )
///                 )
///         ]
///             (VNode_1_new nil, a_1_, x_1_)
///         )
///     )

///     (defn_ #_"node" VNode_2_replace_rightmost_child [#_"node" this, #_"int" shift, #_"node" node, #_"int" delta]
///         (let [
///             #_"array" a (_0_array this) #_"index" x (_0_index this)
///         ]
///             (if (some_9_ x)
///                 (let [
///                     #_"int" e (dec (aget x 32))
///                 ]
///                     (VNode_1_new nil, (-> (aclone a) (aset_4_ e node)), (-> (aclone x) (aset_4_ e (+ (aget x e) delta))))
///                 )
///                 (let [
///                     #_"int" m (dec (VNode_1_index_of_nil a))
///                 ]
///                     (if (some_9_ (_0_index node))
///                         (VNode_1_new nil, (-> (anew 32) (acopy_4_ 0 a 0 m) (aset_4_ m node)), (VNode_1_m_n_index shift, m, (VNode_1_last_range (_0_index node))))
///                         (VNode_1_new nil, (-> (aclone a) (aset_4_ m node)), nil)
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"node" VNode_2_fold_tail [#_"node" this, #_"int" shift, #_"int" tail_off, #_"values" tail]
///         (let [
///             #_"array" a (_0_array this) #_"index" x (_0_index this)
///             #_"int" m (VNode_1_index_of_nil a)
///             #_"node" tail_node
///                 (when (< 5 shift) => (VNode_1_new nil, tail, nil)
///                     (let [
///                         #_"int" n
///                             (when (some_9_ x) => (rem tail_off (<< 1 shift))
///                                 (let [
///                                     #_"int" e (dec (aget x 32))
///                                 ]
///                                     (if (pos_9_ e) (- (aget x e) (aget x (dec e))) (aget x 0))
///                                 )
///                             )
///                     ]
///                         (VNode_2_fold_tail (aget a (dec m)), (- shift 5), n, tail)
///                     )
///                 )
///         ]
///             (when (or (< m 32) (and (some_9_ tail_node) (< 5 shift)))
///                 (let [
///                     #_"int" n (alength tail)
///                     #_"index" x_1_
///                         (when (or (some_9_ x) (< n 32))
///                             (let [
///                                 x_1_ (or (aclone x) (VNode_1_n_index shift, tail_off))
///                             ]
///                                 (if (and (some_9_ tail_node) (< 5 shift))
///                                     (let [
///                                         x_1_ (if (pos_9_ m) (aswap_4_ x_1_ (dec m) + n) x_1_)
///                                     ]
///                                         (-> x_1_ (aset_4_ 32 m))
///                                     )
///                                     (let [
///                                         x_1_ (aset_4_ x_1_ m (+ (if (pos_9_ m) (aget x_1_ (dec m)) 0) n))
///                                     ]
///                                         (-> x_1_ (aset_4_ 32 (inc m)))
///                                     )
///                                 )
///                             )
///                         )
///                     #_"array" a_1_ (-> (anew 32) (acopy_4_ 0 a 0 m))
///                     a_1_
///                         (if (some_9_ tail_node)
///                             (aset_4_ a_1_ (if (< 5 shift) (dec m) m) tail_node)
///                             (aset_4_ a_1_ m (VNode_2_new_path (VNode_1_new nil, tail, nil), nil, (- shift 5)))
///                         )
///                 ]
///                     (VNode_1_new nil, a_1_, x_1_)
///                 )
///             )
///         )
///     )

///     (def #_"int" VNode_1_rrbt_concat_threshold 33)
///     (def_ #_"int" VNode_1_max_extra_search_steps 2)

///     (defn #_"node" VNode_2_slice_right [#_"node" this, #_"int" shift, #_"int" end]
///         (when (pos_9_ shift) => (VNode_1_new nil, (-> (anew end) (acopy_4_ 0 (_0_array this) 0 end)), nil)
///             (let [
///                 #_"array" a (_0_array this) #_"index" x (_0_index this)
///                 #_"int" m (bit_and (>>> (dec end) shift) 0x1f)
///                 m
///                     (when (some_9_ x) => m
///                         (loop_when_recur m (< (aget x m) end) (inc m) => m)
///                     )
///                 #_"int" k (<< 1 shift)
///                 #_"int" child_end
///                     (cond
///                         (nil_9_ x) (let [#_"int" e (rem end k)] (if (zero_9_ e) k e))
///                         (pos_9_ m) (- end (aget x (dec m)))
///                         _0_else    end
///                     )
///                 #_"node" child (VNode_2_slice_right (aget a m), (- shift 5), child_end)
///                 #_"index" y (_0_index child)
///                 #_"array" a_1_ (-> (anew 32) (acopy_4_ 0 a 0 m) (aset_4_ m child))
///                 #_"index" x_1_
///                     (when (or (some_9_ x) (some_9_ y))
///                         (let [
///                             x_1_ (loop_when_recur [x_1_ (anew 33) #_"int" j 0] (< j m) [(aset_4_ x_1_ j (if (some_9_ x) (aget x j) (* (inc j) k))) (inc j)] => x_1_)
///                             #_"int" delta
///                                 (cond
///                                     (nil_9_ y)    (let [#_"int" e (rem child_end k) ] (if (zero_9_ e) k e))
///                                     (< 5 shift) (VNode_1_last_range y)
///                                     _0_else       (alength (_0_array child))
///                                 )
///                             x_1_ (aset_4_ x_1_ m (+ (if (pos_9_ m) (aget x_1_ (dec m)) 0) delta))
///                         ]
///                             (-> x_1_ (aset_4_ 32 (inc m)))
///                         )
///                     )
///             ]
///                 (VNode_1_new nil, a_1_, x_1_)
///             )
///         )
///     )

///     (defn #_"node" VNode_2_slice_left [#_"node" this, #_"int" shift, #_"int" start, #_"int" end]
///         (if (zero_9_ shift)
///             (let [
///                 #_"array" a (_0_array this)
///                 #_"int" n (- (alength a) start)
///             ]
///                 (VNode_1_new nil, (-> (anew n) (acopy_4_ 0 a start n)), nil)
///             )
///             (let [
///                 #_"array" a (_0_array this) #_"index" x (_0_index this)
///                 #_"int" m (bit_and (>>> start shift) 0x1f)
///                 m
///                     (when (some_9_ x) => m
///                         (loop_when_recur m (<= (aget x m) start) (inc m) => m)
///                     )
///                 #_"int" n
///                     (when (nil_9_ x) => (aget x 32)
///                         (loop_when_recur [n m] (and (< n 32) (some_9_ (aget a n))) [(inc n)] => n)
///                     )
///                 #_"int" k (<< 1 shift)
///                 #_"node" child
///                     (let [
///                         #_"int" i (if (some_9_ x) (aget x (dec m)) (* m k))
///                     ]
///                         (VNode_2_slice_left (aget a m), (- shift 5), (if (pos_9_ m) (- start i) start), (min k (if (pos_9_ m) (- end i) end)))
///                     )
///                 n (- n m)
///                 n (if (some_9_ child) n (dec n))
///             ]
///                 (when (pos_9_ n)
///                     (let [
///                         #_"index" x_1_
///                             (if (some_9_ x)
///                                 (loop_when_recur [x_1_ (anew 33) #_"int" j 0 #_"int" i m]
///                                                  (< j n)
///                                                  [(aset_4_ x_1_ j (- (aget x i) start)) (inc j) (inc i)]
///                                               => (aset_4_ x_1_ 32 n)
///                                 )
///                                 (let [
///                                     #_"int" i
///                                         (if (and (some_9_ child) (some_9_ (_0_index child)) (< 5 shift))
///                                             (VNode_1_last_range (_0_index child))
///                                             (- k (bit_and (>>> start (- shift 5)) 0x1f))
///                                         )
///                                 ]
///                                     (loop_when_recur [x_1_ (anew 33) #_"int" j 0 i i]
///                                                      (< j n)
///                                                      [(aset_4_ x_1_ j i) (inc j) (+ i k)]
///                                                   => (-> (if (< 1 n) (aset_4_ x_1_ (dec n) (- end start)) x_1_) (aset_4_ 32 n))
///                                     )
///                                 )
///                             )
///                         #_"array" a_1_
///                             (if (some_9_ child)
///                                 (-> (anew 32) (aset_4_ 0 child) (acopy_4_ 1 a (inc m) (dec n)))
///                                 (-> (anew 32) (acopy_4_ 0 a (inc m) n))
///                             )
///                     ]
///                         (VNode_1_new nil, a_1_, x_1_)
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"node" VNode_2_shift_from_to [#_"node" this, #_"int" from, #_"int" to]
///         (when_not (= from to) => this
///             (let [
///                 #_"index" x_1_
///                     (when (some_9_ (_0_index this))
///                         (-> (anew 33) (aset_4_ 0 (VNode_1_last_range (_0_index this))) (aset_4_ 32 1))
///                     )
///             ]
///                 (recur (VNode_1_new nil, (-> (anew 32) (aset_4_ 0 this)), x_1_) (+ 5 from) to)
///             )
///         )
///     )

///     (defn_ #_"int" VNode_2_leaf_count [#_"node" this, #_"int" shift]
///         (let [
///             #_"array" a (_0_array this) #_"index" x (_0_index this)
///         ]
///             (cond
///                 (zero_9_ shift) (alength a)
///                 (some_9_ x)     (aget x 32)
///                 _0_else         (VNode_1_index_of_nil a)
///             )
///         )
///     )

///     (defn_ #_"int" VNode_2_tree_count [#_"node" this, #_"int" shift]
///         (let [
///             #_"array" a (_0_array this) #_"index" x (_0_index this)
///         ]
///             (loop_when_recur [#_"int" i 0 #_"int" n 0]
///                              (if (some_9_ x) (< i (aget x 32)) (and (< i 32) (some_9_ (aget a i))))
///                              [(inc i) (+ n (VNode_2_leaf_count (aget a i), (- shift 5)))]
///                           => n
///             )
///         )
///     )

///     (defn_ #_"ISeq" VNode_2_leaf_seq [#_"node" this]
///         (let [
///             #_"array" a (_0_array this)
///         ]
///             (mapcat _0_array (take (VNode_1_index_of_nil a) a))
///         )
///     )

///     (defn_ #_"[node node int]" VNode_1_rebalance_leaves [#_"node" node1, #_"node" node2, #_"int" delta]
///         (let [
///             #_"int" n1 (VNode_2_tree_count node1, 5) #_"int" n2 (VNode_2_tree_count node2, 5) #_"int" n (+ n1 n2)
///         ]
///             (when (< VNode_1_max_extra_search_steps (- (+ (VNode_2_leaf_count node1, 5) (VNode_2_leaf_count node2, 5)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
///                 (let [
///                     #_"ISeq" s (map #(VNode_1_new nil, (anew %), nil) (partition_all 32 (concat (VNode_2_leaf_seq node1) (VNode_2_leaf_seq node2))))
///                 ]
///                     (if (<= n (* 32 32))
///                         (let [
///                             #_"index" x_1_ (when_not (zero_9_ (rem n 32)) (VNode_1_n_index 5, n))
///                         ]
///                             [(VNode_1_new nil, (anew s), x_1_) nil n2]
///                         )
///                         (let [
///                             #_"index" x_1_ (when_not (zero_9_ (rem n 32)) (VNode_1_n_index 5, (- n (* 32 32))))
///                         ]
///                             [(VNode_1_new nil, (anew (take 32 s)), nil) (VNode_1_new nil, (anew (drop 32 s)), x_1_) (- (* 32 32) n1)]
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"ISeq" VNode_2_child_seq [#_"node" this, #_"int" shift, #_"int" cnt]
///         (let [
///             f_1_cseq
///                 (fn [#_"node" this #_"int" cnt]
///                     (let [
///                         #_"index" x (or (_0_index this) (VNode_1_n_index (- shift 5), cnt))
///                         #_"int" n (aget x 32)
///                     ]
///                         (take n (map list (_0_array this) (map - x (cons 0 x))))
///                     )
///                 )
///             #_"index" x (or (_0_index this) (VNode_1_n_index shift, cnt))
///             #_"int" n (aget x 32)
///         ]
///             (mapcat f_1_cseq (take n (_0_array this)) (take n (map - x (cons 0 x))))
///         )
///     )

///     (defn_ #_"[node node int]" VNode_1_rebalance [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
///         (when (some_9_ node2) => [node1 nil delta]
///             (let [
///                 #_"int" n1 (VNode_2_tree_count node1, shift) #_"int" n2 (VNode_2_tree_count node2, shift) #_"int" n (+ n1 n2)
///             ]
///                 (when (< VNode_1_max_extra_search_steps (- (+ (VNode_2_leaf_count node1, shift) (VNode_2_leaf_count node2, shift)) (inc (quot (dec n) 32)))) => [node1 node2 delta]
///                     (let [
///                         f_1_cnode
///                             (fn [#_"ISeq" s]
///                                 (loop [#_"array" a (anew 32) #_"index" x (anew 33) #_"int" j 0 #_"int" k 0 s s]
///                                     (when_first [[#_"node" c #_"int" r] s] => (VNode_1_new nil, a, (aset_4_ x 32 j))
///                                         (recur (aset_4_ a j c) (aset_4_ x j (+ k r)) (inc j) (+ k r) (next s))
///                                     )
///                                 )
///                             )
///                         #_"ISeq" s (partition_all 32 (concat (VNode_2_child_seq node1, shift, cnt1) (VNode_2_child_seq node2, shift, cnt2)))
///                     ]
///                         (if (<= n (* 32 32))
///                             (loop [#_"array" a (anew 32) #_"index" x (-> (anew 33) (aset_4_ 32 0)) #_"int" i 0 s s]
///                                 (when_first [#_"ISeq" block s] => [(VNode_1_new nil, a, x) nil cnt2]
///                                     (let [
///                                         #_"node" c (f_1_cnode block)
///                                         a (aset_4_ a i c)
///                                         x (aset_4_ x i (+ (VNode_1_last_range (_0_index c)) (if (pos_9_ i) (aget x (dec i)) 0)))
///                                         x (aset_4_ x 32 (inc i))
///                                     ]
///                                         (recur a x (inc i) (next s))
///                                     )
///                                 )
///                             )
///                             (let [
///                                 #_"array" a1 (anew 32) #_"index" x1 (-> (anew 33) (aset_4_ 32 0))
///                                 #_"array" a2 (anew 32) #_"index" x2 (-> (anew 33) (aset_4_ 32 0))
///                             ]
///                                 (loop [a1 a1 x1 x1 a2 a2 x2 x2 delta delta #_"int" i 0 s s]
///                                     (when_first [#_"ISeq" block s] => [(VNode_1_new nil, a1, x1) (VNode_1_new nil, a2, x2) delta]
///                                         (let [
///                                             #_"node" c (f_1_cnode block) #_"index" y (_0_index c)
///                                             delta
///                                                 (when (and (< i 32) (< n1 (+ (* i 32) (aget y 32)))) => delta
///                                                     (let [
///                                                         #_"int" k (- (+ (* i 32) (aget y 32)) n1)
///                                                         #_"int" e (dec (aget y 32))
///                                                     ]
///                                                         (+ delta (if (< k 32) (- (aget y e) (aget y (- e k))) (aget y e)))
///                                                     )
///                                                 )
///                                             [a1 x1 a2 x2]
///                                                 (if (< i 32)
///                                                     (let [
///                                                         #_"int" m (rem i 32)
///                                                         a1 (aset_4_ a1 m c)
///                                                         x1 (aset_4_ x1 m (+ (VNode_1_last_range y) (if (pos_9_ m) (aget x1 (dec m)) 0)))
///                                                         x1 (aset_4_ x1 32 (inc m))
///                                                     ]
///                                                         [a1 x1 a2 x2]
///                                                     )
///                                                     (let [
///                                                         #_"int" m (rem i 32)
///                                                         a2 (aset_4_ a2 m c)
///                                                         x2 (aset_4_ x2 m (+ (VNode_1_last_range y) (if (pos_9_ m) (aget x2 (dec m)) 0)))
///                                                         x2 (aset_4_ x2 32 (inc m))
///                                                     ]
///                                                         [a1 x1 a2 x2]
///                                                     )
///                                                 )
///                                         ]
///                                             (recur a1 x1 a2 x2 delta (inc i) (next s))
///                                         )
///                                     )
///                                 )
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"[node node int]" VNode_1_zip_path [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2, #_"int" delta]
///         (if (= shift 5)
///             (VNode_1_rebalance_leaves node1, node2, delta)
///             (let [
///                 #_"node" c1 (VNode_2_last_child node1)
///                 #_"node" c2 (VNode_2_first_child node2)
///                 #_"int" k (<< 1 shift)
///                 #_"int" m1
///                     (let [
///                         #_"index" x1 (_0_index node1)
///                     ]
///                         (when (some_9_ x1) => (let [#_"int" m (rem cnt1 k)] (if (zero_9_ m) k m))
///                             (let [#_"int" e (dec (aget x1 32))]
///                                 (if (pos_9_ e) (- (aget x1 e) (aget x1 (dec e))) (aget x1 0))
///                             )
///                         )
///                     )
///                 #_"int" m2
///                     (let [
///                         #_"index" x2 (_0_index node2)
///                     ]
///                         (when (some_9_ x2) => (let [#_"int" m (rem cnt2 k)] (if (zero_9_ m) k m))
///                             (aget x2 0)
///                         )
///                     )
///                 [#_"node" c1' #_"node" c2' #_"int" d_1_] (VNode_1_zip_path (- shift 5), c1, m1, c2, m2, 0)
///             ]
///                 (VNode_1_rebalance shift,
///                     (if (identical_9_ c1 c1') node1 (VNode_2_replace_rightmost_child node1, shift, c1', d_1_)),
///                     (+ cnt1 d_1_),
///                     (if c2' (if (identical_9_ c2 c2') node2 (VNode_2_replace_leftmost_child node2, shift, cnt2, c2', d_1_)) (VNode_2_remove_leftmost_child node2)),
///                     (- cnt2 d_1_),
///                     (+ delta d_1_)
///                 )
///             )
///         )
///     )

///     (defn #_"[node node]" VNode_1_squash_nodes [#_"int" shift, #_"node" node1, #_"int" cnt1, #_"node" node2, #_"int" cnt2]
///         (let [
///             #_"array" a1 (_0_array node1) #_"int" n1 (VNode_1_index_of_nil a1)
///             #_"array" a2 (_0_array node2) #_"int" n2 (VNode_1_index_of_nil a2)
///             #_"ISeq" slots (concat (take n1 a1) (take n2 a2))
///         ]
///             (when (<= (count slots) 32) => [node1 node2]
///                 (let [
///                     #_"ISeq" s1 (take n1 (or (_0_index node1) (VNode_1_n_index shift, cnt1)))
///                     #_"ISeq" s2 (take n2 (or (_0_index node2) (VNode_1_n_index shift, cnt2)))
///                     #_"ISeq" index (concat s1 (let [#_"int" d (last s1)] (map #(+ % d) s2)))
///                     #_"array" a (loop_when_recur [a (anew 32) #_"int" i 0 #_"ISeq" s (seq slots)] (some_9_ s) [(aset_4_ a i (first s)) (inc i) (next s)] => a)
///                     #_"index" x (loop_when_recur [x (anew 33) #_"int" i 0 #_"ISeq" s (seq index)] (some_9_ s) [(aset_4_ x i (first s)) (inc i) (next s)] => (aset_4_ x 32 i))
///                 ]
///                     [(VNode_1_new nil, a, x) nil]
///                 )
///             )
///         )
///     )
// )

// (about #_"TransientVector"
///     (defq TransientVector [#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen] #_"VecForm")

///     #_inherit
///     (defm TransientVector AFn)

///     (defn #_"TransientVector" TransientVector_1_new
///         ([#_"PersistentVector" w]
///             (TransientVector_1_new (_0_cnt w), (_0_shift w), (VNode_2_editable_root (_0_root w)), (VNode_1_editable_tail (_0_tail w)), (alength (_0_tail w)))
///         )
///         ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail, #_"int" tlen]
///             (new_8_ TransientVector_1_class (anew [cnt, shift, root, tail, tlen]))
///         )
///     )

///     (defn_ #_"int" TransientVector_2_count [#_"TransientVector" this]
///         (VNode_2_assert_editable (_0_root this))
///         (_0_cnt this)
///     )

///     (defn_ #_"int" TransientVector_2_tail_off [#_"TransientVector" this]
///         (- (_0_cnt this) (_0_tlen this))
///     )

///     (defn_ #_"values" TransientVector_2_array_for [#_"TransientVector" this, #_"int" i]
///         (VNode_2_array_for (_0_root this), i, (_0_shift this), (_0_cnt this), (TransientVector_2_tail_off this), (_0_tail this))
///     )

///     (defn_ #_"Object" TransientVector_2_value_for [#_"TransientVector" this, #_"int" i]
///         (VNode_2_value_for (_0_root this), i, (_0_shift this), (_0_cnt this), (TransientVector_2_tail_off this), (_0_tail this))
///     )

///     (defn_ #_"Object" TransientVector_2_nth
///         ([#_"TransientVector" this, #_"int" i]
///             (VNode_2_assert_editable (_0_root this))
///             (TransientVector_2_value_for this, i)
///         )
///         ([#_"TransientVector" this, #_"int" i, #_"Object" not_found]
///             (VNode_2_assert_editable (_0_root this))
///             (when (< -1 i (_0_cnt this)) => not_found
///                 (TransientVector_2_value_for this, i)
///             )
///         )
///     )

///     (defn_ #_"Object" TransientVector_2_valAt
///         ([#_"TransientVector" this, #_"Object" key] (TransientVector_2_valAt this, key, nil))
///         ([#_"TransientVector" this, #_"Object" key, #_"Object" not_found]
///             (VNode_2_assert_editable (_0_root this))
///             (when (integer_9_ key) => not_found
///                 (let_when [#_"int" i (int_4_ key)] (< -1 i (_0_cnt this)) => not_found
///                     (TransientVector_2_value_for this, i)
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" TransientVector_2_invoke [#_"TransientVector" this, #_"Object" arg]
///         (when (integer_9_ arg) => (throw "arg must be integer")
///             (Indexed_3_nth this, (int_4_ arg))
///         )
///     )

///     (defn_ #_"Object" TransientVector_2_applyTo [#_"TransientVector" this, #_"ISeq" args]
///         (case_4_ (count args 1)
///             1 (IFn_3_invoke this, (first args))
///         )
///     )

///     (defn_ #_"TransientVector" TransientVector_2_conj_4_ [#_"TransientVector" this, #_"Object" val]
///         (VNode_2_assert_editable (_0_root this))
///         (if (< (_0_tlen this) 32)
///             (let [
///                 _ (aset_4_ (_0_tail this) (_0_tlen this) val)
///             ]
///                 (-> this (qswap_4_ _0_cnt inc) (qswap_4_ _0_tlen inc))
///             )
///             (let [
///                 #_"node" tail_node (VNode_1_new (_0_edit (_0_root this)), (_0_tail this), nil)
///                 this (qset_4_ this _0_tail (-> (anew 32) (aset_4_ 0 val)), _0_tlen 1)
///             ]
///                 (if (VNode_2_overflow_9_ (_0_root this), (_0_shift this), (_0_cnt this))
///                     (let [
///                         #_"array" a
///                             (-> (anew 32)
///                                 (aset_4_ 0 (_0_root this))
///                                 (aset_4_ 1 (VNode_2_new_path tail_node, (_0_edit (_0_root this)), (_0_shift this)))
///                             )
///                         #_"index" x
///                             (when (some_9_ (_0_index (_0_root this)))
///                                 (let [
///                                     #_"int" n (aget (_0_index (_0_root this)) 31)
///                                 ]
///                                     (-> (anew 33) (aset_4_ 0 n) (aset_4_ 1 (+ n 32)) (aset_4_ 32 2))
///                                 )
///                             )
///                         #_"node" root (VNode_1_new (_0_edit (_0_root this)), a, x)
///                     ]
///                         (-> this (qset_4_ _0_root root) (qswap_4_ _0_shift + 5) (qswap_4_ _0_cnt inc))
///                     )
///                     (let [
///                         #_"node" root (VNode_2_push_tail (_0_root this), (_0_edit (_0_root this)), (_0_shift this), (_0_cnt this), tail_node)
///                     ]
///                         (-> this (qset_4_ _0_root root) (qswap_4_ _0_cnt inc))
///                     )
///                 )
///             )
///         )
///     )

///     (declare PersistentVector_1_new)

///     (defn_ #_"PersistentVector" TransientVector_2_persistent_4_ [#_"TransientVector" this]
///         (VNode_2_assert_editable (_0_root this))
///         (reset_4_ (_0_edit (_0_root this)) nil)
///         (let [
///             #_"int" n (_0_tlen this)
///         ]
///             (PersistentVector_1_new (_0_cnt this), (_0_shift this), (_0_root this), (-> (anew n) (acopy_4_ 0 (_0_tail this) 0 n)))
///         )
///     )

///     (defn_ #_"TransientVector" TransientVector_2_assocN_4_ [#_"TransientVector" this, #_"int" i, #_"Object" val]
///         (VNode_2_assert_editable (_0_root this))
///         (if (< -1 i (_0_cnt this))
///             (let [
///                 #_"int" tail_off (TransientVector_2_tail_off this)
///             ]
///                 (if (<= tail_off i)
///                     (do
///                         (aset_4_ (_0_tail this) (- i tail_off) val)
///                         this
///                     )
///                     (do
///                         (qset_4_ this _0_root (VNode_2_do_assoc (_0_root this), (_0_edit (_0_root this)), (_0_shift this), i, val))
///                     )
///                 )
///             )
///             (when (= i (_0_cnt this)) => (throw "index is out of bounds")
///                 (ITransientCollection_3_conj_4_ this, val)
///             )
///         )
///     )

///     (defn_ #_"TransientVector" TransientVector_2_pop_4_ [#_"TransientVector" this]
///         (VNode_2_assert_editable (_0_root this))
///         (cond
///             (zero_9_ (_0_cnt this))
///                 (throw "can't pop the empty vector")
///             (= (_0_cnt this) 1)
///                 (let [
///                     this (qset_4_ this _0_cnt 0)
///                     this (qset_4_ this _0_tlen 0)
///                     _ (aset_4_ (_0_tail this) 0 nil)
///                 ]
///                     this
///                 )
///             (< 1 (_0_tlen this))
///                 (let [
///                     this (qswap_4_ this _0_cnt dec)
///                     this (qswap_4_ this _0_tlen dec)
///                     _ (aset_4_ (_0_tail this) (_0_tlen this) nil)
///                 ]
///                     this
///                 )
///             _0_else
///                 (let [
///                     #_"values" tail (aclone (TransientVector_2_array_for this, (- (_0_cnt this) 2)))
///                     #_"node" root (VNode_2_pop_tail (_0_root this), (_0_edit (_0_root this)), (_0_shift this), (TransientVector_2_tail_off this))
///                     this
///                         (cond
///                             (nil_9_ root)
///                                 (-> this
///                                     (qset_4_ _0_root (VNode_1_new (_0_edit (_0_root this)), nil, nil))
///                                 )
///                             (and (< 5 (_0_shift this)) (nil_9_ (aget (_0_array root) 1)))
///                                 (-> this
///                                     (qswap_4_ _0_shift - 5)
///                                     (qset_4_ _0_root (aget (_0_array root) 0))
///                                 )
///                             _0_else
///                                 (-> this
///                                     (qset_4_ _0_root root)
///                                 )
///                         )
///                 ]
///                     (-> this
///                         (qswap_4_ _0_cnt dec)
///                         (qset_4_ _0_tail tail)
///                         (qset_4_ _0_tlen (alength tail))
///                     )
///                 )
///         )
///     )

///     (defn_ #_"TransientVector" TransientVector_2_assoc_4_ [#_"TransientVector" this, #_"Object" key, #_"Object" val]
///         (when (integer_9_ key) => (throw "key must be integer")
///             (ITransientVector_3_assocN_4_ this, (int_4_ key), val)
///         )
///     )

///     (defn_ #_"bool" TransientVector_2_containsKey [#_"TransientVector" this, #_"Object" key]
///         (and (integer_9_ key) (< -1 (int_4_ key) (_0_cnt this)))
///     )

///     (defn_ #_"pair" TransientVector_2_entryAt [#_"TransientVector" this, #_"Object" key]
///         (when (integer_9_ key)
///             (let_when [#_"int" i (int_4_ key)] (< -1 i (_0_cnt this))
///                 (MapEntry_1_new key, (Indexed_3_nth this, i))
///             )
///         )
///     )

///     (defm TransientVector Counted
///         (Counted_3_count => TransientVector_2_count)
///     )

///     (defm TransientVector Indexed
///         (Indexed_3_nth => TransientVector_2_nth)
///     )

///     (defm TransientVector ILookup
///         (ILookup_3_valAt => TransientVector_2_valAt)
///     )

///     (defm TransientVector IFn
///         (IFn_3_invoke => TransientVector_2_invoke)
///         (IFn_3_applyTo => TransientVector_2_applyTo)
///     )

///     (defm TransientVector ITransientCollection
///         (ITransientCollection_3_conj_4_ => TransientVector_2_conj_4_)
///         (ITransientCollection_3_persistent_4_ => TransientVector_2_persistent_4_)
///     )

///     (defm TransientVector ITransientVector
///         (ITransientVector_3_assocN_4_ => TransientVector_2_assocN_4_)
///         (ITransientVector_3_pop_4_ => TransientVector_2_pop_4_)
///     )

///     (defm TransientVector ITransientAssociative
///         (ITransientAssociative_3_assoc_4_ => TransientVector_2_assoc_4_)
///         (ITransientAssociative_3_containsKey => TransientVector_2_containsKey)
///         (ITransientAssociative_3_entryAt => TransientVector_2_entryAt)
///     )
// )

// (about #_"PersistentVector"
///     (declare PersistentVector_2_seq PersistentVector_2_rseq PersistentVector_2_conj PersistentVector_2_empty PersistentVector_2_equals PersistentVector_2_nth PersistentVector_2_invoke PersistentVector_2_applyTo)

///     (defq PersistentVector [#_"IPersistentMap" _meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] VecForm
///         clojure.lang.Seqable (seq [_] (PersistentVector_2_seq _))
///         clojure.lang.Reversible (rseq [_] (PersistentVector_2_rseq _))
///         clojure.lang.IPersistentCollection (cons [_, o] (PersistentVector_2_conj _, o)) (empty [_] (PersistentVector_2_empty _)) (equiv [_, o] (PersistentVector_2_equals _, o))
///         clojure.lang.IPersistentVector
///         clojure.lang.Counted (count [_] (_0_cnt _))
///         clojure.lang.Indexed (nth [_, i] (PersistentVector_2_nth _, i)) (nth [_, i, not_found] (PersistentVector_2_nth _, i, not_found))
///         clojure.lang.IFn (invoke [_, a] (PersistentVector_2_invoke _, a)) (applyTo [_, args] (PersistentVector_2_applyTo _, args))
///     )

///     #_inherit
///     (defm PersistentVector APersistentVector AFn)

///     (defn #_"PersistentVector" PersistentVector_1_new
///         ([#_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail] (PersistentVector_1_new nil, cnt, shift, root, tail))
///         ([#_"IPersistentMap" meta, #_"int" cnt, #_"int" shift, #_"node" root, #_"values" tail]
///             (new_8_ PersistentVector_1_class (anew [meta, cnt, shift, root, tail]))
///         )
///     )

///     (def #_"PersistentVector" PersistentVector_1_EMPTY (PersistentVector_1_new 0, 5, VNode_1_EMPTY, (anew 0)))

///     (defn #_"PersistentVector" PersistentVector_1_create [& values]
///         (when_some [#_"ISeq" s (seq values)] => PersistentVector_1_EMPTY
///             (let [
///                 #_"values" tail (anew (take 32 s)) #_"int" n (alength tail)
///                 #_"PersistentVector" w (PersistentVector_1_new n, 5, VNode_1_EMPTY, tail)
///             ]
///                 (when_some [s (seq (drop 32 s))] => w
///                     (into w s)
///                 )
///             )
///         )
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_withMeta [#_"PersistentVector" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentVector_1_new meta, (_0_cnt this), (_0_shift this), (_0_root this), (_0_tail this))
///         )
///     )

///     (defn_ #_"bool" PersistentVector_2_equals [#_"PersistentVector" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (cond
///                 (vector_9_ that)
///                     (when (= (_0_cnt this) (_0_cnt that)) => false
///                         (loop_when [#_"int" i 0] (< i (_0_cnt this)) => true
///                             (recur_when (= (Indexed_3_nth this, i) (Indexed_3_nth that, i)) [(inc i)] => false)
///                         )
///                     )
///                 (sequential_9_ that)
///                     (loop_when [#_"int" i 0 #_"ISeq" s (seq that)] (< i (_0_cnt this)) => (nil_9_ s)
///                         (recur_when (and (some_9_ s) (= (Indexed_3_nth this, i) (first s))) [(inc i) (next s)] => false)
///                     )
///                 _0_else
///                     false
///             )
///         )
///     )

///     (defn_ #_"int" PersistentVector_2_hash [#_"PersistentVector" this]
///         (loop_when [#_"int" hash (int 1) #_"int" i (int 0)] (< i (_0_cnt this)) => (Murmur3_1_mixCollHash hash, i)
///             (recur (+ (* (int 31) hash) (f_1_hash (Indexed_3_nth this, i))) (inc i))
///         )
///     )

///     (defn_ #_"int" PersistentVector_2_tail_off [#_"PersistentVector" this]
///         (- (_0_cnt this) (alength (_0_tail this)))
///     )

///     (defn_ #_"values" PersistentVector_2_array_for [#_"PersistentVector" this, #_"int" i]
///         (VNode_2_array_for (_0_root this), i, (_0_shift this), (_0_cnt this), (PersistentVector_2_tail_off this), (_0_tail this))
///     )

///     (defn_ #_"Object" PersistentVector_2_value_for [#_"PersistentVector" this, #_"int" i]
///         (VNode_2_value_for (_0_root this), i, (_0_shift this), (_0_cnt this), (PersistentVector_2_tail_off this), (_0_tail this))
///     )

///     (defn_ #_"Object" PersistentVector_2_nth
///         ([#_"PersistentVector" this, #_"int" i]
///             (PersistentVector_2_value_for this, i)
///         )
///         ([#_"PersistentVector" this, #_"int" i, #_"Object" not_found]
///             (when (< -1 i (_0_cnt this)) => not_found
///                 (PersistentVector_2_value_for this, i)
///             )
///         )
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_conj [#_"PersistentVector" this, #_"Object" val]
///         (let [
///             #_"int" tail_len (alength (_0_tail this))
///         ]
///             (if (< tail_len 32)
///                 (let [
///                     #_"values" tail (-> (anew (inc tail_len)) (acopy_4_ 0 (_0_tail this) 0 tail_len) (aset_4_ tail_len val))
///                 ]
///                     (PersistentVector_1_new (_0__meta this), (inc (_0_cnt this)), (_0_shift this), (_0_root this), tail)
///                 )
///                 (let [
///                     #_"node" tail_node (VNode_1_new (_0_edit (_0_root this)), (_0_tail this), nil)
///                     #_"int" shift (_0_shift this)
///                     [#_"node" root shift]
///                         (if (VNode_2_overflow_9_ (_0_root this), shift, (_0_cnt this))
///                             (let [
///                                 #_"array" a
///                                     (-> (anew 32)
///                                         (aset_4_ 0 (_0_root this))
///                                         (aset_4_ 1 (VNode_2_new_path tail_node, (_0_edit (_0_root this)), shift))
///                                     )
///                                 #_"index" x
///                                     (when (some_9_ (_0_index (_0_root this)))
///                                         (let [
///                                             #_"int" n (aget (_0_index (_0_root this)) 31)
///                                         ]
///                                             (-> (anew 33) (aset_4_ 0 n) (aset_4_ 1 (+ n 32)) (aset_4_ 32 2))
///                                         )
///                                     )
///                             ]
///                                 [(VNode_1_new (_0_edit (_0_root this)), a, x) (+ shift 5)]
///                             )
///                             [(VNode_2_push_tail (_0_root this), (_0_edit (_0_root this)), shift, (_0_cnt this), tail_node) shift]
///                         )
///                 ]
///                     (PersistentVector_1_new (_0__meta this), (inc (_0_cnt this)), shift, root, (anew [ val ]))
///                 )
///             )
///         )
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_empty [#_"PersistentVector" this]
///         (IObj_3_withMeta PersistentVector_1_EMPTY, (_0__meta this))
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_assocN [#_"PersistentVector" this, #_"int" i, #_"Object" val]
///         (if (< -1 i (_0_cnt this))
///             (let [
///                 #_"int" tail_off (PersistentVector_2_tail_off this)
///             ]
///                 (if (<= tail_off i)
///                     (let [
///                         #_"int" n (alength (_0_tail this))
///                         #_"values" tail (-> (anew n) (acopy_4_ 0 (_0_tail this) 0 n) (aset_4_ (- i tail_off) val))
///                     ]
///                         (PersistentVector_1_new (_0__meta this), (_0_cnt this), (_0_shift this), (_0_root this), tail)
///                     )
///                     (PersistentVector_1_new (_0__meta this), (_0_cnt this), (_0_shift this), (VNode_2_do_assoc (_0_root this), (_0_edit (_0_root this)), (_0_shift this), i, val), (_0_tail this))
///                 )
///             )
///             (when (= i (_0_cnt this)) => (throw "index is out of bounds")
///                 (IPersistentCollection_3_conj this, val)
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentVector_2_peek [#_"PersistentVector" this]
///         (when (pos_9_ (_0_cnt this))
///             (Indexed_3_nth this, (dec (_0_cnt this)))
///         )
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_pop [#_"PersistentVector" this]
///         (case_4_ (_0_cnt this)
///             0   (throw "can't pop the empty vector")
///             1   (IObj_3_withMeta PersistentVector_1_EMPTY, (_0__meta this))
///             (let [
///                 #_"int" tail_len (alength (_0_tail this))
///             ]
///                 (if (< 1 tail_len)
///                     (let [
///                         #_"values" tail (-> (anew (dec tail_len)) (acopy_4_ 0 (_0_tail this) 0 (dec tail_len)))
///                     ]
///                         (PersistentVector_1_new (_0__meta this), (dec (_0_cnt this)), (_0_shift this), (_0_root this), tail)
///                     )
///                     (let [
///                         #_"values" tail (PersistentVector_2_array_for this, (- (_0_cnt this) 2))
///                         #_"int" shift (_0_shift this)
///                         #_"node" root (VNode_2_pop_tail (_0_root this), (_0_edit (_0_root this)), shift, (PersistentVector_2_tail_off this))
///                         [shift root]
///                             (cond
///                                 (nil_9_ root)                                     [shift VNode_1_EMPTY]
///                                 (and (< 5 shift) (nil_9_ (aget (_0_array root) 1))) [(- shift 5) (aget (_0_array root) 0)]
///                                 _0_else                                           [shift root]
///                             )
///                     ]
///                         (PersistentVector_1_new (_0__meta this), (dec (_0_cnt this)), shift, root, tail)
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentVector_2_invoke [#_"PersistentVector" this, #_"Object" arg]
///         (when (integer_9_ arg) => (throw "arg must be integer")
///             (Indexed_3_nth this, (int_4_ arg))
///         )
///     )

///     (defn_ #_"Object" PersistentVector_2_applyTo [#_"PersistentVector" this, #_"ISeq" args]
///         (case_4_ (count args 1)
///             1 (IFn_3_invoke this, (first args))
///         )
///     )

///     (defn_ #_"Object" PersistentVector_2_reduce
///         ([#_"PersistentVector" this, #_"IFn" f]
///             (when (pos_9_ (_0_cnt this)) => (f)
///                 (loop_when [#_"Object" r (aget (PersistentVector_2_array_for this, 0) 0) #_"int" i 0] (< i (_0_cnt this)) => r
///                     (let [#_"values" a (PersistentVector_2_array_for this, i)
///                           r (loop_when [r r #_"int" j (if (zero_9_ i) 1 0)] (< j (alength a)) => r
///                                 (let [r (f r (aget a j))]
///                                     (when_not (reduced_9_ r) => r
///                                         (recur r (inc j))
///                                     )
///                                 )
///                             )]
///                         (when_not (reduced_9_ r) => (deref r)
///                             (recur r (+ i (alength a)))
///                         )
///                     )
///                 )
///             )
///         )
///         ([#_"PersistentVector" this, #_"IFn" f, #_"Object" r]
///             (loop_when [r r #_"int" i 0] (< i (_0_cnt this)) => r
///                 (let [#_"values" a (PersistentVector_2_array_for this, i)
///                       r (loop_when [r r #_"int" j 0] (< j (alength a)) => r
///                             (let [r (f r (aget a j))]
///                                 (when_not (reduced_9_ r) => r
///                                     (recur r (inc j))
///                                 )
///                             )
///                         )]
///                     (when_not (reduced_9_ r) => (deref r)
///                         (recur r (+ i (alength a)))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentVector_2_kvreduce [#_"PersistentVector" this, #_"IFn" f, #_"Object" r]
///         (loop_when [r r #_"int" i 0] (< i (_0_cnt this)) => r
///             (let [
///                 #_"values" a (PersistentVector_2_array_for this, i)
///                 r
///                     (loop_when [r r #_"int" j 0] (< j (alength a)) => r
///                         (let [
///                             r (f r (+ i j) (aget a j))
///                         ]
///                             (when_not (reduced_9_ r) => r
///                                 (recur r (inc j))
///                             )
///                         )
///                     )
///             ]
///                 (when_not (reduced_9_ r) => (deref r)
///                     (recur r (+ i (alength a)))
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" PersistentVector_2_assoc [#_"PersistentVector" this, #_"Object" key, #_"Object" val]
///         (when (integer_9_ key) => (throw "key must be integer")
///             (IPersistentVector_3_assocN this, (int_4_ key), val)
///         )
///     )

///     (defn_ #_"bool" PersistentVector_2_containsKey [#_"PersistentVector" this, #_"Object" key]
///         (and (integer_9_ key) (< -1 (int_4_ key) (_0_cnt this)))
///     )

///     (defn_ #_"pair" PersistentVector_2_entryAt [#_"PersistentVector" this, #_"Object" key]
///         (when (integer_9_ key)
///             (let_when [#_"int" i (int_4_ key)] (< -1 i (_0_cnt this))
///                 (MapEntry_1_new key, (Indexed_3_nth this, i))
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentVector_2_valAt
///         ([#_"PersistentVector" this, #_"Object" key] (PersistentVector_2_valAt this, key, nil))
///         ([#_"PersistentVector" this, #_"Object" key, #_"Object" not_found]
///             (when (integer_9_ key) => not_found
///                 (let_when [#_"int" i (int_4_ key)] (< -1 i (_0_cnt this)) => not_found
///                     (PersistentVector_2_value_for this, i)
///                 )
///             )
///         )
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_slicev [#_"PersistentVector" this, #_"int" start, #_"int" end]
///         (cond
///             (or (neg_9_ start) (< (_0_cnt this) end)) (throw "index is out of bounds")
///             (= start end)                         (IPersistentCollection_3_empty this)
///             (< end start)                         (throw "start index greater than end index")
///             _0_else
///                 (let [
///                     #_"int" new_cnt (- end start)
///                     #_"int" tail_off (PersistentVector_2_tail_off this)
///                 ]
///                     (if (<= tail_off start)
///                         (let [
///                             #_"values" tail (-> (anew new_cnt) (acopy_4_ 0 (_0_tail this) (- start tail_off) new_cnt))
///                         ]
///                             (PersistentVector_1_new (_0__meta this), new_cnt, 5, VNode_1_EMPTY, tail)
///                         )
///                         (let [
///                             #_"bool" tail_cut_9_ (< tail_off end)
///                             #_"node" root (_0_root this)
///                             root (if tail_cut_9_ root (VNode_2_slice_right root, (_0_shift this), end))
///                             root (if (zero_9_ start) root (VNode_2_slice_left root, (_0_shift this), start, (min end tail_off)))
///                             #_"values" tail
///                                 (when tail_cut_9_ => (VNode_2_array_for root, (dec new_cnt), (_0_shift this), new_cnt)
///                                     (let [
///                                         #_"int" n (- end tail_off)
///                                     ]
///                                         (-> (anew n) (acopy_4_ 0 (_0_tail this) 0 n))
///                                     )
///                                 )
///                             root
///                                 (when_not tail_cut_9_ => root
///                                     (VNode_2_pop_tail root, nil, (_0_shift this), new_cnt)
///                                 )
///                         ]
///                             (when (some_9_ root) => (PersistentVector_1_new (_0__meta this), new_cnt, 5, VNode_1_EMPTY, tail)
///                                 (loop_when_recur [#_"node" node root #_"int" shift (_0_shift this)]
///                                                  (and (< 5 shift) (nil_9_ (aget (_0_array node) 1)))
///                                                  [(aget (_0_array node) 0) (- shift 5)]
///                                               => (PersistentVector_1_new (_0__meta this), new_cnt, shift, node, tail)
///                                 )
///                             )
///                         )
///                     )
///                 )
///         )
///     )

///     (defn_ #_"PersistentVector" PersistentVector_2_splicev [#_"PersistentVector" this, #_"PersistentVector" that]
///         (let [
///             #_"int" c1 (_0_cnt this) #_"int" c2 (_0_cnt that)
///         ]
///             (cond
///                 (zero_9_ c1) that
///                 (< c2 VNode_1_rrbt_concat_threshold) (into this that)
///                 _0_else
///                     (let [
///                         #_"node" r1 (_0_root this) #_"int" s1 (_0_shift this) #_"array" t1 (_0_tail this) #_"int" o1 (PersistentVector_2_tail_off this)
///                         #_"bool" overflow_9_ (VNode_2_overflow_9_ r1, s1, (+ o1 32))
///                         r1
///                             (when overflow_9_ => (VNode_2_fold_tail r1, s1, o1, t1)
///                                 (let [
///                                     #_"array" a_1_
///                                         (-> (anew 32)
///                                             (aset_4_ 0 r1)
///                                             (aset_4_ 1 (VNode_2_new_path (VNode_1_new nil, t1, nil), nil, s1))
///                                         )
///                                     #_"index" x_1_
///                                         (when (or (some_9_ (_0_index r1)) (< (alength t1) 32))
///                                             (-> (anew 33) (aset_4_ 0 o1) (aset_4_ 1 c1) (aset_4_ 32 2))
///                                         )
///                                 ]
///                                     (VNode_1_new nil, a_1_, x_1_)
///                                 )
///                             )
///                         s1 (if overflow_9_ (+ s1 5) s1)
///                         #_"node" r2 (_0_root that) #_"int" s2 (_0_shift that) #_"array" t2 (_0_tail that) #_"int" o2 (PersistentVector_2_tail_off that)
///                         #_"int" shift (max s1 s2)
///                         r1 (VNode_2_shift_from_to r1, s1, shift)
///                         r2 (VNode_2_shift_from_to r2, s2, shift)
///                         [#_"node" n1 #_"node" n2 #_"int" delta] (VNode_1_zip_path shift, r1, c1, r2, o2, 0)
///                         #_"int" c1' (+ c1 delta)
///                         #_"int" c2' (- o2 delta)
///                         [n1 n2] (if (identical_9_ n2 r2) (VNode_1_squash_nodes shift, n1, c1', n2, c2') [n1 n2])
///                     ]
///                         (if (some_9_ n2)
///                             (let [
///                                 #_"array" a_1_ (-> (anew 32) (aset_4_ 0 n1) (aset_4_ 1 n2))
///                                 #_"index" x_1_ (-> (anew 33) (aset_4_ 0 c1') (aset_4_ 1 (+ c1' c2')) (aset_4_ 32 2))
///                             ]
///                                 (PersistentVector_1_new nil, (+ c1 c2), (+ shift 5), (VNode_1_new nil, a_1_, x_1_), t2)
///                             )
///                             (loop_when_recur [#_"node" node n1 shift shift]
///                                              (and (< 5 shift) (nil_9_ (aget (_0_array node) 1)))
///                                              [(aget (_0_array node) 0) (- shift 5)]
///                                           => (PersistentVector_1_new nil, (+ c1 c2), shift, node, t2)
///                             )
///                         )
///                     )
///             )
///         )
///     )

///     (defn_ #_"ISeq" PersistentVector_2_seq [#_"PersistentVector" this]
///         (when (pos_9_ (_0_cnt this))
///             (VSeq_1_new this, 0)
///         )
///     )

///     (defn_ #_"ISeq" PersistentVector_2_rseq [#_"PersistentVector" this]
///         (when (pos_9_ (_0_cnt this))
///             (RSeq_1_new this, (dec (_0_cnt this)))
///         )
///     )

///     (defn_ #_"int" PersistentVector_2_compareTo [#_"PersistentVector" this, #_"IPersistentVector" that]
///         (when_not (identical_9_ this that) => 0
///             (let [#_"int" n (_0_cnt this) #_"int" m (count that)]
///                 (cond (< n m) -1 (< m n) 1
///                     _0_else
///                         (loop_when [#_"int" i 0] (< i n) => 0
///                             (let [#_"int" cmp (compare (Indexed_3_nth this, i) (Indexed_3_nth that, i))]
///                                 (recur_when (zero_9_ cmp) [(inc i)] => cmp)
///                             )
///                         )
///                 )
///             )
///         )
///     )

///     (defm PersistentVector IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentVector IObj
///         (IObj_3_withMeta => PersistentVector_2_withMeta)
///     )

///     (defm PersistentVector IObject
///         (IObject_3_equals => PersistentVector_2_equals)
///     )

///     (defm PersistentVector Hashed
///         (Hashed_3_hash => PersistentVector_2_hash)
///     )

///     (defm PersistentVector IEditableCollection
///         (IEditableCollection_3_asTransient => TransientVector_1_new)
///     )

///     (defm PersistentVector Counted
///         (Counted_3_count => _0_cnt)
///     )

///     (defm PersistentVector Indexed
///         (Indexed_3_nth => PersistentVector_2_nth)
///     )

///     (defm PersistentVector IPersistentCollection
///         (IPersistentCollection_3_conj => PersistentVector_2_conj)
///         (IPersistentCollection_3_empty => PersistentVector_2_empty)
///     )

///     (defm PersistentVector IPersistentVector
///         (IPersistentVector_3_assocN => PersistentVector_2_assocN)
///         (IPersistentVector_3_slicev => PersistentVector_2_slicev)
///         (IPersistentVector_3_splicev => PersistentVector_2_splicev)
///     )

///     (defm PersistentVector IPersistentStack
///         (IPersistentStack_3_peek => PersistentVector_2_peek)
///         (IPersistentStack_3_pop => PersistentVector_2_pop)
///     )

///     (defm PersistentVector IFn
///         (IFn_3_invoke => PersistentVector_2_invoke)
///         (IFn_3_applyTo => PersistentVector_2_applyTo)
///     )

///     (defm PersistentVector IReduce
///         (IReduce_3_reduce => PersistentVector_2_reduce)
///     )

///     (defm PersistentVector IKVReduce
///         (IKVReduce_3_kvreduce => PersistentVector_2_kvreduce)
///     )

///     (defm PersistentVector Associative
///         (Associative_3_assoc => PersistentVector_2_assoc)
///         (Associative_3_containsKey => PersistentVector_2_containsKey)
///         (Associative_3_entryAt => PersistentVector_2_entryAt)
///     )

///     (defm PersistentVector ILookup
///         (ILookup_3_valAt => PersistentVector_2_valAt)
///     )

///     (defm PersistentVector Sequential)

///     (defm PersistentVector Seqable
///         (Seqable_3_seq => PersistentVector_2_seq)
///     )

///     (defm PersistentVector Reversible
///         (Reversible_3_rseq => PersistentVector_2_rseq)
///     )

///     (defm PersistentVector Comparable
///         (Comparable_3_compareTo => PersistentVector_2_compareTo)
///     )
// )

/// (defn vector
///     ([]                   PersistentVector_1_EMPTY)
///     ([a]                 (PersistentVector_1_create a))
///     ([a b]               (PersistentVector_1_create a b))
///     ([a b c]             (PersistentVector_1_create a b c))
///     ([a b c d]           (PersistentVector_1_create a b c d))
///     ([a b c d & s] (apply PersistentVector_1_create a b c d s))
/// )

/// (defn vec [s]
///     (if (vector_9_ s) s (apply vector s))
/// )

/// (defn subvec
///     ([v i]   (IPersistentVector_3_slicev v, i, (count v)))
///     ([v i e] (IPersistentVector_3_slicev v, i, e))
/// )

/// (defn catvec
///     ([] (vector))
///     ([a]                                                                                                  a)
///     ([a b]                                                                   (IPersistentVector_3_splicev a, b))
///     ([a b c]                                    (IPersistentVector_3_splicev (IPersistentVector_3_splicev a, b),                              c))
///     ([a b c d]                                  (IPersistentVector_3_splicev (IPersistentVector_3_splicev a, b), (IPersistentVector_3_splicev c, d)))
///     ([a b c d & s] (IPersistentVector_3_splicev (IPersistentVector_3_splicev (IPersistentVector_3_splicev a, b), (IPersistentVector_3_splicev c, d)), (apply catvec s)))
/// )

/// (defn assoc_1_  [v i x & s] (apply assoc  (vec v) i x s))
/// (defn conj_1_   [v   x & s] (apply conj   (vec v)   x s))
/// (defn into_1_   [v       s]       (into   (vec v)     s))
/// (defn peek_1_   [v]               (peek   (vec v)      ))
/// (defn pop_1_    [v]               (pop    (vec v)      ))
/// (defn update_1_ [v i f & s] (apply update (vec v) i f s))

/// (defn dissoc_1_ [v i] (let [v (vec v)] (catvec (subvec v 0 i) (subvec v (inc i)))))
}

namespace arbace {

// (about #_"QSeq"
///     (declare QSeq_2_seq QSeq_2_first QSeq_2_next)

///     (defq QSeq [#_"IPersistentMap" _meta, #_"ISeq" f, #_"ISeq" rseq] SeqForm
///         clojure.lang.ISeq (seq [_] (QSeq_2_seq _)) (first [_] (QSeq_2_first _)) (next [_] (QSeq_2_next _)) (more [_] (or (QSeq_2_next _) ()))
///     )

///     #_inherit
///     (defm QSeq ASeq)

///     (defn #_"QSeq" QSeq_1_new
///         ([#_"ISeq" f, #_"ISeq" rseq] (QSeq_1_new nil, f, rseq))
///         ([#_"IPersistentMap" meta, #_"ISeq" f, #_"ISeq" rseq]
///             (new_8_ QSeq_1_class (anew [meta, f, rseq]))
///         )
///     )

///     (defn_ #_"QSeq" QSeq_2_withMeta [#_"QSeq" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (QSeq_1_new meta, (_0_f this), (_0_rseq this))
///         )
///     )

///     (defn_ #_"ISeq" QSeq_2_seq [#_"QSeq" this]
///         this
///     )

///     (defn_ #_"Object" QSeq_2_first [#_"QSeq" this]
///         (first (_0_f this))
///     )

///     (defn_ #_"ISeq" QSeq_2_next [#_"QSeq" this]
///         (let [#_"ISeq" f (next (_0_f this)) #_"ISeq" r (_0_rseq this)]
///             (cond
///                 (some_9_ f) (QSeq_1_new f, r)
///                 (some_9_ r) (QSeq_1_new r, nil)
///             )
///         )
///     )

///     (defn_ #_"int" QSeq_2_count [#_"QSeq" this]
///         (+ (count (_0_f this)) (count (_0_rseq this)))
///     )

///     (defm QSeq IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm QSeq IObj
///         (IObj_3_withMeta => QSeq_2_withMeta)
///     )

///     (defm QSeq Sequential)

///     (defm QSeq Seqable
///         (Seqable_3_seq => QSeq_2_seq)
///     )

///     (defm QSeq ISeq
///         (ISeq_3_first => QSeq_2_first)
///         (ISeq_3_next => QSeq_2_next)
///     )

///     (defm QSeq Counted
///         (Counted_3_count => QSeq_2_count)
///     )

///     (defm QSeq Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm QSeq IObject
///         (IObject_3_equals => ASeq_2_equals)
///     )
// )

// (about #_"PersistentQueue"
///     (defq PersistentQueue [#_"IPersistentMap" _meta, #_"int" cnt, #_"ISeq" f, #_"vector" r] VecForm)

///     (defn #_"PersistentQueue" PersistentQueue_1_new [#_"IPersistentMap" meta, #_"int" cnt, #_"ISeq" f, #_"vector" r]
///         (new_8_ PersistentQueue_1_class (anew [meta, cnt, f, r]))
///     )

///     (defn_ #_"PersistentQueue" PersistentQueue_2_withMeta [#_"PersistentQueue" this, #_"IPersistentMap" meta]
///         (when_not (= meta (_0__meta this)) => this
///             (PersistentQueue_1_new meta, (_0_cnt this), (_0_f this), (_0_r this))
///         )
///     )

///     (def #_"PersistentQueue" PersistentQueue_1_EMPTY (PersistentQueue_1_new nil, 0, nil, nil))

///     (defn_ #_"bool" PersistentQueue_2_equals [#_"PersistentQueue" this, #_"Object" that]
///         (or (identical_9_ this that)
///             (and (sequential_9_ that)
///                 (loop_when [#_"ISeq" s (seq this) #_"ISeq" z (seq that)] (some_9_ s) => (nil_9_ z)
///                     (and (some_9_ z) (= (first s) (first z))
///                         (recur (next s) (next z))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" PersistentQueue_2_peek [#_"PersistentQueue" this]
///         (first (_0_f this))
///     )

///     (defn_ #_"PersistentQueue" PersistentQueue_2_pop [#_"PersistentQueue" this]
///         (when (some_9_ (_0_f this)) => this
///             (let [#_"ISeq" f (next (_0_f this)) #_"vector" r (_0_r this)
///                   [f r]
///                     (when (nil_9_ f) => [f r]
///                         [(seq r) nil]
///                     )]
///                 (PersistentQueue_1_new (_0__meta this), (dec (_0_cnt this)), f, r)
///             )
///         )
///     )

///     (defn_ #_"ISeq" PersistentQueue_2_seq [#_"PersistentQueue" this]
///         (when (some_9_ (_0_f this))
///             (QSeq_1_new (_0_f this), (seq (_0_r this)))
///         )
///     )

///     (defn_ #_"PersistentQueue" PersistentQueue_2_conj [#_"PersistentQueue" this, #_"Object" o]
///         (let [[#_"ISeq" f #_"vector" r]
///                 (if (nil_9_ (_0_f this))
///                     [(list o) nil]
///                     [(_0_f this) (conj (or (_0_r this) (vector)) o)]
///                 )]
///             (PersistentQueue_1_new (_0__meta this), (inc (_0_cnt this)), f, r)
///         )
///     )

///     (defn_ #_"PersistentQueue" PersistentQueue_2_empty [#_"PersistentQueue" this]
///         (with_meta PersistentQueue_1_EMPTY (_0__meta this))
///     )

///     (defm PersistentQueue IPersistentList Sequential)

///     (defm PersistentQueue IMeta
///         (IMeta_3_meta => _0__meta)
///     )

///     (defm PersistentQueue IObj
///         (IObj_3_withMeta => PersistentQueue_2_withMeta)
///     )

///     (defm PersistentQueue IObject
///         (IObject_3_equals => PersistentQueue_2_equals)
///     )

///     (defm PersistentQueue Hashed
///         (Hashed_3_hash => Murmur3_1_hashOrdered)
///     )

///     (defm PersistentQueue IPersistentStack
///         (IPersistentStack_3_peek => PersistentQueue_2_peek)
///         (IPersistentStack_3_pop => PersistentQueue_2_pop)
///     )

///     (defm PersistentQueue Counted
///         (Counted_3_count => _0_cnt)
///     )

///     (defm PersistentQueue Seqable
///         (Seqable_3_seq => PersistentQueue_2_seq)
///     )

///     (defm PersistentQueue IPersistentCollection
///         (IPersistentCollection_3_conj => PersistentQueue_2_conj)
///         (IPersistentCollection_3_empty => PersistentQueue_2_empty)
///     )
// )
}

namespace arbace {

// (about #_"RT"
///     (defn #_"Object" RT_1_get
///         ([#_"Object" coll, #_"Object" key]
///             (cond
///                 (satisfies_9_ ILookup coll)
///                     (ILookup_3_valAt coll, key)
///                 (nil_9_ coll)
///                     nil
///                 (set_9_ coll)
///                     (IPersistentSet_3_get coll, key)
///                 (and (number_9_ key) (or (string_9_ coll) (array_9_ coll)))
///                     (let_when [#_"int" n (int_4_ key)] (< -1 n (count coll))
///                         (nth coll n)
///                     )
///                 (satisfies_9_ ITransientSet coll)
///                     (ITransientSet_3_get coll, key)
///             )
///         )
///         ([#_"Object" coll, #_"Object" key, #_"Object" not_found]
///             (cond
///                 (satisfies_9_ ILookup coll)
///                     (ILookup_3_valAt coll, key, not_found)
///                 (nil_9_ coll)
///                     not_found
///                 (set_9_ coll)
///                     (if (contains_9_ coll key) (IPersistentSet_3_get coll, key) not_found)
///                 (and (number_9_ key) (or (string_9_ coll) (array_9_ coll)))
///                     (let [#_"int" n (int_4_ key)]
///                         (if (< -1 n (count coll)) (nth coll n) not_found)
///                     )
///                 (satisfies_9_ ITransientSet coll)
///                     (if (contains_9_ coll key) (ITransientSet_3_get coll, key) not_found)
///                 _0_else
///                     not_found
///             )
///         )
///     )

/// (defn get
///     ([coll key          ] (RT_1_get coll key          ))
///     ([coll key not_found] (RT_1_get coll key not_found))
/// )

/// (defn get_in
///     ([m ks] (reduce get m ks))
///     ([m ks not_found]
///         (loop_when [m m o (anew 0) ks (seq ks)] ks => m
///             (let_when [m (get m (first ks) o)] (identical_9_ m o) => (recur m o (next ks))
///                 not_found
///             )
///         )
///     )
/// )

///     (defn #_"Object" RT_1_contains [#_"Object" coll, #_"Object" key]
///         (cond
///             (nil_9_ coll)
///                 false
///             (associative_9_ coll)
///                 (if (Associative_3_containsKey coll, key) true false)
///             (set_9_ coll)
///                 (if (IPersistentSet_3_contains_9_ coll, key) true false)
///             (and (number_9_ key) (or (string_9_ coll) (array_9_ coll)))
///                 (let [#_"int" n (int_4_ key)]
///                     (if (< -1 n (count coll)) true false)
///                 )
///             (satisfies_9_ ITransientSet coll)
///                 (if (ITransientSet_3_contains_9_ coll, key) true false)
///             (satisfies_9_ ITransientAssociative coll)
///                 (if (ITransientAssociative_3_containsKey coll, key) true false)
///             _0_else
///                 (throw (str "contains_9_ not supported on " coll))
///         )
///     )

/// (defn contains_9_ [coll key] (RT_1_contains coll key))

///     (defn #_"Object" RT_1_find [#_"Object" coll, #_"Object" key]
///         (cond
///             (nil_9_ coll)
///                 nil
///             (associative_9_ coll)
///                 (Associative_3_entryAt coll, key)
///             (satisfies_9_ ITransientAssociative coll)
///                 (ITransientAssociative_3_entryAt coll, key)
///             _0_else
///                 (throw (str "find not supported on " coll))
///         )
///     )

/// (defn find [m k] (RT_1_find m k))

///     (defn #_"ISeq" RT_1_findKey [#_"Keyword" key, #_"ISeq" keyvals]
///         (loop_when keyvals (some_9_ keyvals)
///             (when_some [#_"ISeq" s (next keyvals)] => (throw "malformed keyword argslist")
///                 (when_not (= (first keyvals) key) => s
///                     (recur (next s))
///                 )
///             )
///         )
///     )

///     (defn #_"Object" RT_1_nth
///         ([#_"Object" coll, #_"int" n]
///             (cond
///                 (indexed_9_ coll)
///                     (Indexed_3_nth coll, n)
///                 (nil_9_ coll)
///                     nil
///                 (char_sequence_9_ coll)
///                     (Character_1_valueOf (CharSequence_3_charAt coll, n))
///                 (array_9_ coll)
///                     (Array_1_get coll, n)
///                 (matcher_9_ coll)
///                     (Matcher_2_group coll, n)
///                 (map_entry_9_ coll)
///                     (let [#_"pair" e coll]
///                         (case_4_ n 0 (key e) 1 (val e) (throw "index is out of bounds"))
///                     )
///                 (sequential_9_ coll)
///                     (loop_when [#_"int" i 0 #_"ISeq" s (seq coll)] (and (<= i n) (some_9_ s)) => (throw "index is out of bounds")
///                         (recur_when (< i n) [(inc i) (next s)] => (first s))
///                     )
///                 _0_else
///                     (throw (str "nth not supported on " coll))
///             )
///         )
///         ([#_"Object" coll, #_"int" n, #_"Object" not_found]
///             (cond
///                 (indexed_9_ coll)
///                     (Indexed_3_nth coll, n, not_found)
///                 (nil_9_ coll)
///                     not_found
///                 (neg_9_ n)
///                     not_found
///                 (char_sequence_9_ coll)
///                     (let_when [#_"CharSequence" s coll] (< n (CharSequence_3_length s)) => not_found
///                         (Character_1_valueOf (CharSequence_3_charAt s, n))
///                     )
///                 (array_9_ coll)
///                     (when (< n (Array_1_getLength coll)) => not_found
///                         (Array_1_get coll, n)
///                     )
///                 (matcher_9_ coll)
///                     (let_when [#_"Matcher" m coll] (< n (Matcher_2_groupCount m)) => not_found
///                         (Matcher_2_group m, n)
///                     )
///                 (map_entry_9_ coll)
///                     (let [#_"pair" e coll]
///                         (case_4_ n 0 (key e) 1 (val e) not_found)
///                     )
///                 (sequential_9_ coll)
///                     (loop_when [#_"int" i 0 #_"ISeq" s (seq coll)] (and (<= i n) (some_9_ s)) => not_found
///                         (recur_when (< i n) [(inc i) (next s)] => (first s))
///                     )
///                 _0_else
///                     (throw (str "nth not supported on " coll))
///             )
///         )
///     )

/// (defn nth
///     ([s i]           (RT_1_nth s i          ))
///     ([s i not_found] (RT_1_nth s i not_found))
/// )

///     (defn #_"IPersistentMap" RT_1_map [#_"Seqable" init]
///         (cond
///             (empty_9_ init)
///                 PersistentArrayMap_1_EMPTY
///             (<= (count init) PersistentArrayMap_1_HASHTABLE_THRESHOLD)
///                 (PersistentArrayMap_1_createWithCheck (anew init))
///             _0_else
///                 (PersistentHashMap_1_createWithCheck_1s init)
///         )
///     )

///     (defn #_"IPersistentMap" RT_1_mapUniqueKeys [#_"Seqable" init]
///         (cond
///             (empty_9_ init)
///                 PersistentArrayMap_1_EMPTY
///             (<= (count init) PersistentArrayMap_1_HASHTABLE_THRESHOLD)
///                 (PersistentArrayMap_1_new (anew init))
///             _0_else
///                 (PersistentHashMap_1_create_1s init)
///         )
///     )
// )
}

namespace arbace {

// (about #_"Var"
///     (defn_ #_"Appendable" Var_1_append [#_"Appendable" a, #_"Namespace" ns, #_"Symbol" sym]
///         (if (some_9_ ns)
///             (-> a (Appendable_3_append "#'") (append (_0_name ns)) (Appendable_3_append "/") (append sym))
///             (-> a (Appendable_3_append "#_var nil #_\"") (append sym) (Appendable_3_append "\""))
///         )
///     )
// )

// (about #_"Unbound"
///     (defq Unbound [#_"Namespace" ns, #_"Symbol" sym])

///     #_inherit
///     (defm Unbound AFn)

///     (defn #_"Unbound" Unbound_1_new [#_"Namespace" ns, #_"Symbol" sym]
///         (new_8_ Unbound_1_class (anew [ns, sym]))
///     )

///     (defn_ #_"Appendable" Unbound_2_append [#_"Unbound" this, #_"Appendable" a]
///         (-> a (Appendable_3_append "#_unbound ") (Var_1_append (_0_ns this), (_0_sym this)))
///     )

///     (defm Unbound IObject
///         (IObject_3_equals => identical_9_)
///     )

///     (defm Unbound IAppend
///         (IAppend_3_append => Unbound_2_append)
///     )
// )

// (about #_"Var"
///     (declare Var_2_get)

///     (defq Var [#_"Namespace" ns, #_"Symbol" sym, #_"Object'" root]
///         java.util.concurrent.Future (get [_] (Var_2_get _))
///     )

///     (defn #_"Var" Var_1_new
///         ([#_"Namespace" ns, #_"Symbol" sym] (Var_1_new ns, sym, (Unbound_1_new ns, sym)))
///         ([#_"Namespace" ns, #_"Symbol" sym, #_"Object" root]
///             (new_8_ Var_1_class (anew [ns, sym, (atom root)]))
///         )
///     )

///     (defn_ #_"IPersistentMap" Var_2_meta [#_"Var" this]
///         (meta (_0_root this))
///     )

///     (defn_ #_"IPersistentMap" Var_2_alterMeta [#_"Var" this, #_"IFn" f, #_"ISeq" args]
///         (apply alter_meta_4_ (_0_root this) f args)
///     )

///     (defn_ #_"IPersistentMap" Var_2_resetMeta [#_"Var" this, #_"IPersistentMap" m]
///         (reset_meta_4_ (_0_root this) m)
///     )

///     (defn_ #_"Appendable" Var_2_append [#_"Var" this, #_"Appendable" a]
///         (Var_1_append a, (_0_ns this), (_0_sym this))
///     )

///     (defn #_"bool" Var_2_hasRoot [#_"Var" this]
///         (not (satisfies_9_ Unbound (deref (_0_root this))))
///     )

///     (defn #_"bool" Var_2_isBound [#_"Var" this]
///         (Var_2_hasRoot this)
///     )

///     (defn_ #_"Object" Var_2_get [#_"Var" this]
///          (_0_root this)
///     )

/// (defn var_get [#_"var" x] (Var_2_get x))

///     (defn #_"void" Var_2_setMacro [#_"Var" this]
///         (alter_meta_4_ this assoc _0_macro true)
///         nil
///     )

///     (defn #_"bool" Var_2_isMacro [#_"Var" this]
///         (boolean (_0_macro (meta this)))
///     )

///     (defn #_"bool" Var_2_isPublic [#_"Var" this]
///         (not (_0_private (meta this)))
///     )

///     (defn #_"void" Var_2_bindRoot [#_"Var" this, #_"Object" root]
///         (alter_meta_4_ this dissoc _0_macro)
///         (reset_4_ (_0_root this) root)
///         nil
///     )

///     (defn #_"Object" Var_2_alterRoot [#_"Var" this, #_"IFn" f, #_"ISeq" args]
///         (apply swap_4_ (_0_root this) f args)
///     )

///     (declare Namespace_2_intern)

///     (defn_ #_"Var" Var_1_intern
///         ([#_"Namespace" ns, #_"Symbol" sym]
///             (Namespace_2_intern ns, sym)
///         )
///         ([#_"Namespace" ns, #_"Symbol" sym, #_"Object" root]
///             (let [#_"Var" v (Namespace_2_intern ns, sym)]
///                 (Var_2_bindRoot v, root)
///                 v
///             )
///         )
///     )

/// (declare the_ns)

/// (defn intern
///     ([ns name]
///         (let [v (Var_1_intern (the_ns ns), name)]
///             (when_some [m (meta name)]
///                 (reset_meta_4_ v m)
///             )
///             v
///         )
///     )
///     ([ns name root]
///         (let [v (Var_1_intern (the_ns ns), name, root)]
///             (when_some [m (meta name)]
///                 (reset_meta_4_ v m)
///             )
///             v
///         )
///     )
/// )

///     (defn_ #_"Object" Var_2_invoke
///         ([#_"Var" this]                                                   (IFn_3_invoke (deref this)))
///         ([#_"Var" this, a1]                                               (IFn_3_invoke (deref this), a1))
///         ([#_"Var" this, a1, a2]                                           (IFn_3_invoke (deref this), a1, a2))
///         ([#_"Var" this, a1, a2, a3]                                       (IFn_3_invoke (deref this), a1, a2, a3))
///         ([#_"Var" this, a1, a2, a3, a4]                                   (IFn_3_invoke (deref this), a1, a2, a3, a4))
///         ([#_"Var" this, a1, a2, a3, a4, a5]                               (IFn_3_invoke (deref this), a1, a2, a3, a4, a5))
///         ([#_"Var" this, a1, a2, a3, a4, a5, a6]                           (IFn_3_invoke (deref this), a1, a2, a3, a4, a5, a6))
///         ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7]                       (IFn_3_invoke (deref this), a1, a2, a3, a4, a5, a6, a7))
///         ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8]                   (IFn_3_invoke (deref this), a1, a2, a3, a4, a5, a6, a7, a8))
///         ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8, a9]               (IFn_3_invoke (deref this), a1, a2, a3, a4, a5, a6, a7, a8, a9))
///         ([#_"Var" this, a1, a2, a3, a4, a5, a6, a7, a8, a9, #_"ISeq" args] (IFn_3_invoke (deref this), a1, a2, a3, a4, a5, a6, a7, a8, a9, args))
///     )

///     (defn_ #_"Object" Var_2_applyTo [#_"Var" this, #_"ISeq" args]
///         (IFn_3_applyTo (deref this), args)
///     )

///     (defm Var IMeta
///         (IMeta_3_meta => Var_2_meta)
///     )

///     (defm Var IReference
///         (IReference_3_alterMeta => Var_2_alterMeta)
///         (IReference_3_resetMeta => Var_2_resetMeta)
///     )

///     (defm Var IObject
///         (IObject_3_equals => identical_9_)
///     )

///     (defm Var IAppend
///         (IAppend_3_append => Var_2_append)
///     )

///     (defm Var IDeref
///         (IDeref_3_deref => Var_2_get)
///     )

///     (defm Var IFn
///         (IFn_3_invoke => Var_2_invoke)
///         (IFn_3_applyTo => Var_2_applyTo)
///     )
// )

/// (defn alter_var_root [#_"var" v f & args] (Var_2_alterRoot v f args))

/// (defn bound_9_ [& vars] (every_9_ #(Var_2_isBound #_"var" %) vars))

/// (defmacro defonce [name expr]
///     `(let_when [v# (def ~name)] (not (Var_2_hasRoot v#))
///         (def ~name ~expr)
///     )
/// )
}

namespace arbace {

// (about #_"Namespace"
///     (defq Namespace [#_"Symbol" name, #_"{Symbol Var}'" mappings, #_"{Symbol Namespace}'" aliases])

///     (def #_"{Symbol Namespace}'" Namespace_1_namespaces (atom (hash_map)))

///     (defn #_"ISeq" Namespace_1_all []
///         (vals (deref Namespace_1_namespaces))
///     )

/// (defn all_ns [] (Namespace_1_all))

///     (defn #_"Namespace" Namespace_1_find [#_"Symbol" name]
///         (get (deref Namespace_1_namespaces) name)
///     )

/// (defn find_ns [sym] (Namespace_1_find sym))

/// (defn #_"Namespace" the_ns [x]
///     (if (satisfies_9_ Namespace x)
///         x
///         (or (find_ns x) (throw (str "no namespace: " x " found")))
///     )
/// )

///     (defn_ #_"Namespace" Namespace_1_new [#_"Symbol" name]
///         (new_8_ Namespace_1_class (anew [name, (atom (hash_map)), (atom (hash_map))]))
///     )

///     (defn #_"Namespace" Namespace_1_findOrCreate [#_"Symbol" name]
///         (or (Namespace_1_find name)
///             (let [#_"Namespace" ns (Namespace_1_new name)]
///                 (swap_4_ Namespace_1_namespaces assoc name ns)
///                 ns
///             )
///         )
///     )

/// (defn create_ns [sym] (Namespace_1_findOrCreate sym))

///     (defn #_"Namespace" Namespace_1_remove [#_"Symbol" name]
///         (when_not (= name _1_arbace.core) => (throw "cannot remove core namespace")
///             (get (first (swap_vals_4_ Namespace_1_namespaces dissoc name)) name)
///         )
///     )

/// (defn remove_ns [sym] (Namespace_1_remove sym))

///     (defn_ #_"Appendable" Namespace_2_append [#_"Namespace" this, #_"Appendable" a]
///         (Appendable_3_append a, (_0_name (_0_name this)))
///     )

/// (defn ns_name [ns] (_0_name (the_ns ns)))

///     (defn #_"IPersistentMap" Namespace_2_getMappings [#_"Namespace" this]
///          (_0_mappings this)
///     )

/// (defn ns_map [ns] (Namespace_2_getMappings (the_ns ns)))

///     (defn #_"Object" Namespace_2_getMapping [#_"Namespace" this, #_"Symbol" name]
///         (get (deref (_0_mappings this)) name)
///     )

/// (defn_ filter_key [f f_9_ m]
///     (loop_when_recur [s (seq m) m (transient (hash_map))]
///                      s
///                      [(next s) (let [e (first s)] (if (f_9_ (f e)) (assoc m (key e) (val e)) m))]
///                   => (persistent_4_ m)
///     )
/// )

/// (defn ns_interns [ns]
///     (let [ns (the_ns ns)]
///         (filter_key val (fn [#_"var" v] (and (var_9_ v) (= ns (_0_ns v)))) (ns_map ns))
///     )
/// )

/// (defn ns_publics [ns]
///     (let [ns (the_ns ns)]
///         (filter_key val (fn [#_"var" v] (and (var_9_ v) (= ns (_0_ns v)) (Var_2_isPublic v))) (ns_map ns))
///     )
/// )

/// (defn ns_refers [ns]
///     (let [ns (the_ns ns)]
///         (filter_key val (fn [#_"var" v] (and (var_9_ v) (not= ns (_0_ns v)))) (ns_map ns))
///     )
/// )

///     (defn_ #_"void" Namespace_2_warnOrFailOnReplace [#_"Namespace" this, #_"Symbol" sym, #_"Object" o, #_"var" var]
///         (or
///             (when (var_9_ o)
///                 (when (= (_0_ns o) this) => (throw (str sym " already refers to: " o " in namespace: " (_0_name this)))
///                     _0_ok
///                 )
///             )
///             (PrintWriter_2_println std::cerr, (str "WARNING: " sym " already refers to: " o " in namespace: " (_0_name this) ", being replaced by: " var))
///         )
///         nil
///     )

///     (defn #_"var" Namespace_2_intern [#_"Namespace" this, #_"Symbol" sym]
///         (when (nil_9_ (_0_ns sym)) => (throw "can't intern namespace_qualified symbol")
///             (let [#_"Object" o
///                     (or (get (deref (_0_mappings this)) sym)
///                         (let [#_"var" v (Var_1_new this, sym)]
///                             (swap_4_ (_0_mappings this) assoc sym v)
///                             v
///                         )
///                     )]
///                 (when_not (and (var_9_ o) (= (_0_ns o) this)) => o
///                     (let [#_"var" v (Var_1_new this, sym)]
///                         (Namespace_2_warnOrFailOnReplace this, sym, o, v)
///                         (swap_4_ (_0_mappings this) assoc sym v)
///                         v
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"var" Namespace_2_refer [#_"Namespace" this, #_"Symbol" sym, #_"var" var]
///         (when (nil_9_ (_0_ns sym)) => (throw "can't intern namespace_qualified symbol")
///             (let [#_"Object" o
///                     (or (get (deref (_0_mappings this)) sym)
///                         (do
///                             (swap_4_ (_0_mappings this) assoc sym var)
///                             var
///                         )
///                     )]
///                 (when_not (= o var)
///                     (Namespace_2_warnOrFailOnReplace this, sym, o, var)
///                     (swap_4_ (_0_mappings this) assoc sym var)
///                 )
///                 var
///             )
///         )
///     )

/// (declare ^_0_dynamic _8_ns_8_)

/// (defn refer [ns_sym & filters]
///     (let [ns (the_ns ns_sym) ps_8_ (ns_publics ns) fs_8_ (apply hash_map filters)
///           r (_0_refer fs_8_) s (if (= r _0_all) (keys ps_8_) (or r (_0_only fs_8_) (keys ps_8_)))]
///         (when (sequential_9_ s) => (throw "the value of :only/:refer must be a sequential collection of symbols")
///             (let [es_8_ (set (_0_exclude fs_8_)) rs_8_ (or (_0_rename fs_8_) (hash_map))]
///                 (doseq [x (remove es_8_ s)]
///                     (when_some [v (ps_8_ x)] => (throw (str x (if (get (ns_interns ns) x) " is not public" " does not exist")))
///                         (Namespace_2_refer _8_ns_8_ (or (rs_8_ x) x) v)
///                     )
///                 )
///             )
///         )
///     )
/// )

///     (defn #_"void" Namespace_2_unmap [#_"Namespace" this, #_"Symbol" sym]
///         (when (nil_9_ (_0_ns sym)) => (throw "can't unintern namespace_qualified symbol")
///             (swap_4_ (_0_mappings this) dissoc sym)
///         )
///         nil
///     )

/// (defn ns_unmap [ns sym] (Namespace_2_unmap (the_ns ns) sym))

///     (defn #_"var" Namespace_2_findInternedVar [#_"Namespace" this, #_"Symbol" name]
///         (let [#_"Object" o (get (deref (_0_mappings this)) name)]
///             (when (and (var_9_ o) (= (_0_ns o) this))
///                 o
///             )
///         )
///     )

/// (defn #_"Var" find_var [#_"Symbol" sym]
///     (when (some_9_ (_0_ns sym)) => (throw "symbol must be namespace_qualified")
///         (let [#_"Namespace" ns (Namespace_1_find (Symbol_1_intern (_0_ns sym)))]
///             (when (some_9_ ns) => (throw (str "no such namespace: " (_0_ns sym)))
///                 (Namespace_2_findInternedVar ns, (Symbol_1_intern (_0_name sym)))
///             )
///         )
///     )
/// )

///     (defn #_"IPersistentMap" Namespace_2_getAliases [#_"Namespace" this]
///         (deref (_0_aliases this))
///     )

/// (defn ns_aliases [ns]
///     (Namespace_2_getAliases (the_ns ns))
/// )

///     (defn #_"Namespace" Namespace_2_getAlias [#_"Namespace" this, #_"Symbol" alias]
///         (get (deref (_0_aliases this)) alias)
///     )

///     (defn #_"void" Namespace_2_addAlias [#_"Namespace" this, #_"Symbol" alias, #_"Namespace" ns]
///         (when (and (some_9_ alias) (some_9_ ns)) => (throw "expecting Symbol + Namespace")
///             (let [#_"Object" o
///                     (or (get (deref (_0_aliases this)) alias)
///                         (do
///                             (swap_4_ (_0_aliases this) assoc alias ns)
///                             ns
///                         )
///                     )]
///                 (when_not (= o ns)
///                     (throw (str "alias " alias " already exists in namespace " (_0_name this) ", aliasing " o))
///                 )
///             )
///         )
///         nil
///     )

/// (defn alias [sym ns]
///     (Namespace_2_addAlias _8_ns_8_ sym (the_ns ns))
/// )

///     (defn #_"void" Namespace_2_removeAlias [#_"Namespace" this, #_"Symbol" alias]
///         (swap_4_ (_0_aliases this) dissoc alias)
///         nil
///     )

/// (defn ns_unalias [ns sym]
///     (Namespace_2_removeAlias (the_ns ns) sym)
/// )

///     (defm Namespace IObject
///         (IObject_3_equals => identical_9_)
///     )

///     (defm Namespace IAppend
///         (IAppend_3_append => Namespace_2_append)
///     )
// )
}

namespace cloiure {

// redefine let and loop with destructuring

/// (defn destructure [bindings]
///     (letfn [(vec_ [v x y]
///                 (let [v_1_ (gensym "v__") s_1_ (gensym "s__") f_1_ (gensym "f__") amp (some #{'&} x)]
///                     (loop_when [v (let [v (conj v v_1_ y)] (if amp (conj v s_1_ `(seq ~v_1_)) v)) n 0 s (seq x) amp_9_ false] s => v
///                         (case_4_ (first s)
///                             '&  (recur (destructure_ v (second s) s_1_) n (next (next s)) true)
///                             _0_as (destructure_ v (second s) v_1_)
///                                 (when_not amp_9_ => (throw "unsupported binding form, only :as can follow & parameter")
///                                     (recur
///                                         (destructure_ (if amp (conj v f_1_ `(first ~s_1_) s_1_ `(next ~s_1_)) v)
///                                             (first s)
///                                             (if amp f_1_ `(nth ~v_1_ ~n nil))
///                                         )
///                                         (inc n) (next s) amp_9_
///                                     )
///                                 )
///                         )
///                     )
///                 )
///             )
///             (map_ [v x y]
///                 (let [m_1_ (gensym "m__") as (_0_as x) or_8_ (_0_or x)
///                       v (conj v m_1_ y m_1_ `(if (seq_9_ ~m_1_) (apply hash_map ~m_1_) ~m_1_)) v (if as (conj v as m_1_) v)
///                       s (reduce
///                             (fn [m e] (reduce #(assoc %1 %2 ((val e) %2)) (dissoc m (key e)) ((key e) m)))
///                             (dissoc x _0_as _0_or)
///                             (reduce
///                                 (fn [m k]
///                                     (when (keyword_9_ k) => m
///                                         (let [ns (namespace k)]
///                                             (case_4_ (name k)
///                                                 "keys" (assoc m k #(keyword (or ns (namespace %)) (name %)))
///                                                 "syms" (assoc m k #(list _1_quote (symbol (or ns (namespace %)) (name %))))
///                                                 "strs" (assoc m k str)
///                                                        m
///                                             )
///                                         )
///                                     )
///                                 )
///                                 (hash_map) (keys x)
///                             )
///                         )]
///                     (loop_when [v v s (seq s)] s => v
///                         (let [x (key (first s)) k (val (first s))
///                               local (if (satisfies_9_ INamed x) (with_meta (symbol nil (name x)) (meta x)) x)
///                               y (if (contains_9_ or_8_ local)
///                                     `(get ~m_1_ ~k ~(or_8_ local))
///                                     `(get ~m_1_ ~k)
///                                 )]
///                             (recur (if (or (symbol_9_ x) (keyword_9_ x)) (conj v local y) (destructure_ v x y)) (next s))
///                         )
///                     )
///                 )
///             )
///             (destructure_ [v x y]
///                 (cond
///                     (symbol_9_ x) (conj v x y)
///                     (vector_9_ x) (vec_ v x y)
///                     (map_9_ x)    (map_ v x y)
///                     _0_else       (throw (str "unsupported binding form: " x))
///                 )
///             )]
///         (let [pairs (partition 2 bindings)]
///             (if (every_9_ symbol_9_ (map first pairs))
///                 bindings
///                 (reduce #(destructure_ %1 (first %2) (second %2)) (vector) pairs)
///             )
///         )
///     )
/// )

/// #_oops!
/// (defmacro let [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (even_9_ (count bindings)) "an even number of forms in binding vector"
///     )
///     `(let_8_ ~(destructure bindings) ~@body)
/// )

/// (defn_ maybe_destructured [pars body]
///     (if (every_9_ symbol_9_ pars)
///         (cons (vec pars) body)
///         (loop_when [s (seq pars) pars (with_meta (vector) (meta pars)) lets (vector)] s => `(~pars (let ~lets ~@body))
///             (if (symbol_9_ (first s))
///                 (recur (next s) (conj pars (first s)) lets)
///                 (let [p_1_ (gensym "p__")]
///                     (recur (next s) (conj pars p_1_) (conj lets (first s) p_1_))
///                 )
///             )
///         )
///     )
/// )

// redefine fn with destructuring

/// #_oops!
/// (defmacro fn [& s]
///     (let [name (when (symbol_9_ (first s)) (first s)) s (if name (next s) s)
///           s (if (vector_9_ (first s))
///                 (list s)
///                 (if (seq_9_ (first s))
///                     s
///                     (throw
///                         (if (seq s)
///                             (str "parameter declaration " (first s) " should be a vector")
///                             (str "parameter declaration missing")
///                         )
///                     )
///                 )
///             )
///           sig_
///             (fn_8_ [sig]
///                 (when (seq_9_ sig) => (throw (str "invalid signature " sig " should be a list"))
///                     (let_when [[pars & body] sig] (vector_9_ pars) => (throw
///                                                                         (if (seq_9_ (first s))
///                                                                             (str "parameter declaration " pars " should be a vector")
///                                                                             (str "invalid signature " sig " should be a list")
///                                                                         )
///                                                                     )
///                         (maybe_destructured pars (or (and (map_9_ (first body)) (next body)) body))
///                     )
///                 )
///             )
///           s (map sig_ s)]
///         (with_meta (if name (list_8_ _1_fn_8_ name s) (cons _1_fn_8_ s)) (meta &form))
///     )
/// )

/// #_oops!
/// (defmacro loop [bindings & body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (even_9_ (count bindings)) "an even number of forms in binding vector"
///     )
///     (if (= (destructure bindings) bindings)
///         `(loop_8_ ~bindings ~@body)
///         (let [s (take_nth 2 bindings) s_1_ (map #(if (symbol_9_ %) % (gensym)) s)
///               v (reduce
///                     (fn [v [x y z]] (if (symbol_9_ x) (conj v z y) (conj v z y x z)))
///                     (vector) (map vector s (take_nth 2 (drop 1 bindings)) s_1_)
///                 )]
///             `(let ~v
///                 (loop_8_ ~(vec (interleave s_1_ s_1_))
///                     (let ~(vec (interleave s s_1_))
///                         ~@body
///                     )
///                 )
///             )
///         )
///     )
/// )

// (about #_"def{n,macro}"
///     (defn_ assert_valid_fdecl [fdecl]
///         (when (seq fdecl) => (throw "parameter declaration missing")
///             (let [argdecls
///                     (map
///                         #(if (seq_9_ %)
///                             (first %)
///                             (throw
///                                 (if (seq_9_ (first fdecl))
///                                     (str "invalid signature \"" % "\" should be a list")
///                                     (str "parameter declaration \"" % "\" should be a vector")
///                                 )
///                             )
///                         )
///                         fdecl
///                     )
///                 bad_args (seq (remove #(vector_9_ %) argdecls))]
///                 (when bad_args
///                     (throw (str "parameter declaration \"" (first bad_args) "\" should be a vector"))
///                 )
///             )
///         )
///     )

///     #_oops!
///     (defmacro defn [fname & s]
///         (when (symbol_9_ fname) => (throw "first argument to defn must be a symbol")
///             (let [m (if (map_9_    (first s)) (first s) (hash_map))
///                   s (if (map_9_    (first s)) (next s)   s)
///                   s (if (vector_9_ (first s)) (list s)   s)
///                   _ (assert_valid_fdecl s)
///                   m (let [inline (_0_inline m) ifn (first inline) iname (second inline)]
///                         (when (and (= _1_fn ifn) (not (symbol_9_ iname))) => m
///                             (assoc m _0_inline (cons ifn (cons (symbol (str (_0_name fname) "__inliner")) (next inline))))
///                         )
///                     )
///                   m (conj (or (meta fname) (hash_map)) m)]
///                 (list _1_def (with_meta fname m) (cons _7_fn s))
///             )
///         )
///     )

///     #_oops!
///     (defmacro defmacro [name & args]
///         (let [[m s] (split_with map_9_ args) s (if (vector_9_ (first s)) (list s) s)
///               s (map (fn [[bindings & body]] (cons (apply vector '&form '&env bindings) body)) s)]
///             `(do (defn ~name ~@m ~@s) (Var_2_setMacro (var ~name)) (var ~name))
///         )
///     )
// )

/// (defn comparator [f_9_]
///     (fn [x y]
///         (cond (f_9_ x y) -1 (f_9_ y x) 1 _0_else 0)
///     )
/// )

/// (defn sort
///     ([s] (sort compare s))
///     ([#_"Comparator" cmp s]
///         (when (seq s) => (list)
///             (let [a (anew s)]
///                 (Arrays_1_sort a, cmp)
///                 (seq a)
///             )
///         )
///     )
/// )

/// (defn sort_by
///     ([f s] (sort_by f compare s))
///     ([f #_"Comparator" cmp s] (sort #(Comparator_3_compare cmp, (f %1), (f %2)) s))
/// )

/// #_oops!
/// (defmacro for [bindings body]
///     (assert_args
///         (vector_9_ bindings) "a vector for its binding"
///         (even_9_ (count bindings)) "an even number of forms in binding vector"
///     )
///     (letfn [(group_ [bindings]
///                 (reduce
///                     (fn [v [x y]]
///                         (if (keyword_9_ x)
///                             (conj (pop v) (conj (peek v) [x y]))
///                             (conj v [x y])
///                         )
///                     )
///                     (vector) (partition 2 bindings)
///                 )
///             )
///             (emit_ [[[x _ & z] & [[_ e] _0_as more]]]
///                 (let [f_1_ (gensym "f__") s_1_ (gensym "s__")]
///                     (letfn [(mod_ [[[k v] & z]]
///                                 (if (keyword_9_ k)
///                                     (case_4_ k
///                                         _0_let   `(let ~v ~(mod_ z))
///                                         _0_while `(when ~v ~(mod_ z))
///                                         _0_when  `(if ~v ~(mod_ z) (recur (next ~s_1_)))
///                                     )
///                                     (when more => `(cons ~body (~f_1_ (next ~s_1_)))
///                                         `(let [f# ~(emit_ more) s# (seq (f# ~e))]
///                                             (if s#
///                                                 (concat s# (~f_1_ (next ~s_1_)))
///                                                 (recur (next ~s_1_))
///                                             )
///                                         )
///                                     )
///                                 )
///                             )]
///                         (if more
///                             #_"not the inner_most loop"
///                             `(fn ~f_1_ [~s_1_]
///                                 (lazy_seq
///                                     (loop [~s_1_ ~s_1_]
///                                         (when_first [~x ~s_1_]
///                                             ~(mod_ z)
///                                         )
///                                     )
///                                 )
///                             )
///                             #_"inner_most loop"
///                             `(fn ~f_1_ [~s_1_]
///                                 (lazy_seq
///                                     (loop [~s_1_ ~s_1_]
///                                         (when_some [~s_1_ (seq ~s_1_)]
///                                             (let [~x (first ~s_1_)]
///                                                 ~(mod_ z)
///                                             )
///                                         )
///                                     )
///                                 )
///                             )
///                         )
///                     )
///                 )
///             )]
///         `(~(emit_ (group_ bindings)) ~(second bindings))
///     )
/// )

/// (defn #_"Pattern" re_pattern [s] (if (pattern_9_ s) s (Pattern_1_compile s)))

/// (defn #_"Matcher" re_matcher [#_"Pattern" re s] (Pattern_2_matcher re, s))

/// (defn re_groups [#_"Matcher" m]
///     (let_when [n (Matcher_2_groupCount m)] (pos_9_ n) => (Matcher_2_group m)
///         (into (vector) (for [i (range (inc n))] (Matcher_2_group m, i)))
///     )
/// )

/// (defn re_seq [#_"Pattern" re s]
///     (let [m (re_matcher re s)]
///         ((fn step []
///             (when (Matcher_2_find m)
///                 (cons (re_groups m) (lazy_seq (step)))
///             )
///         ))
///     )
/// )

/// (defn re_matches [#_"Pattern" re s]
///     (let_when [m (re_matcher re s)] (Matcher_2_matches m)
///         (re_groups m)
///     )
/// )

/// (defn re_find
///     ([#_"Matcher" m]
///         (when (Matcher_2_find m)
///             (re_groups m)
///         )
///     )
///     ([#_"Pattern" re s]
///         (let [m (re_matcher re s)]
///             (re_find m)
///         )
///     )
/// )

/// (defn tree_seq [branch_9_ children root]
///     (letfn [(walk_ [node]
///                 (lazy_seq
///                     (cons node (when (branch_9_ node) (mapcat walk_ (children node))))
///                 )
///             )]
///         (walk_ root)
///     )
/// )

/// (defn flatten [s] (remove sequential_9_ (next (tree_seq sequential_9_ seq s))))

/// (defn max_key
///     ([k x] x)
///     ([k x y] (if (> (k x) (k y)) x y))
///     ([k x y & s]
///         (let [kx (k x) ky (k y) [v kv] (if (> kx ky) [x kx] [y ky])]
///             (loop_when [v v kv kv s s] s => v
///                 (let [w (first s) kw (k w)]
///                     (if (>= kw kv)
///                         (recur w kw (next s))
///                         (recur v kv (next s))
///                     )
///                 )
///             )
///         )
///     )
/// )

/// (defn min_key
///     ([k x] x)
///     ([k x y] (if (< (k x) (k y)) x y))
///     ([k x y & s]
///         (let [kx (k x) ky (k y) [v kv] (if (< kx ky) [x kx] [y ky])]
///             (loop_when [v v kv kv s s] s => v
///                 (let [w (first s) kw (k w)]
///                     (if (<= kw kv)
///                         (recur w kw (next s))
///                         (recur v kv (next s))
///                     )
///                 )
///             )
///         )
///     )
/// )

/// (defn replace
///     ([m] (map #(if_some [e (find m %)] (val e) %)))
///     ([m s]
///         (when (vector_9_ s) => (map #(if_some [e (find m %)] (val e) %) s)
///             (reduce
///                 (fn [v i]
///                     (if_some [e (find m (nth v i))]
///                         (assoc v i (val e))
///                         v
///                     )
///                 )
///                 s (range (count s))
///             )
///         )
///     )
/// )

/// (defn_ mk_bound_fn [#_"Sorted" sc f_1_test key]
///     (fn [e] (f_1_test (Comparator_3_compare (Sorted_3_comparator sc), (Sorted_3_entryKey sc, e), key) 0))
/// )

/// (defn subseq
///     ([#_"Sorted" sc f_1_test key]
///         (let [keep_9_ (mk_bound_fn sc f_1_test key)]
///             (if (#{> >=} f_1_test)
///                 (when_some [[e _0_as s] (Sorted_3_seqFrom sc, key, true)]
///                     (if (keep_9_ e) s (next s))
///                 )
///                 (take_while keep_9_ (Sorted_3_seq sc, true))
///             )
///         )
///     )
///     ([#_"Sorted" sc f_1_test key f_1_test_1_ key_1_]
///         (when_some [[e _0_as s] (Sorted_3_seqFrom sc, key, true)]
///             (take_while (mk_bound_fn sc f_1_test_1_ key_1_) (if ((mk_bound_fn sc f_1_test key) e) s (next s)))
///         )
///     )
/// )

/// (defn rsubseq
///     ([#_"Sorted" sc f_1_test key]
///         (let [keep_9_ (mk_bound_fn sc f_1_test key)]
///             (if (#{< <=} f_1_test)
///                 (when_some [[e _0_as s] (Sorted_3_seqFrom sc, key, false)]
///                     (if (keep_9_ e) s (next s))
///                 )
///                 (take_while keep_9_ (Sorted_3_seq sc, false))
///             )
///         )
///     )
///     ([#_"Sorted" sc f_1_test key f_1_test_1_ key_1_]
///         (when_some [[e _0_as s] (Sorted_3_seqFrom sc, key_1_, false)]
///             (take_while (mk_bound_fn sc f_1_test key) (if ((mk_bound_fn sc f_1_test_1_ key_1_) e) s (next s)))
///         )
///     )
/// )

/// (defn trampoline
///     ([f]
///         (let_when [r (f)] (fn_9_ r) => r
///             (recur r)
///         )
///     )
///     ([f & args] (trampoline #(apply f args)))
/// )

/// (defn memoize [f]
///     (let [mem (atom (hash_map))]
///         (fn [& args]
///             (if_some [e (find (deref mem) args)]
///                 (val e)
///                 (let [r (apply f args)]
///                     (swap_4_ mem assoc args r)
///                     r
///                 )
///             )
///         )
///     )
/// )

// (about #_"case"

/// (defn_ shift_mask [shift mask x] (-> x (>> shift) (bit_and mask)))

/// (def_ max_mask_bits 13)
/// (def_ max_switch_table_size (<< 1 max_mask_bits))

/// (defn_ maybe_min_hash [hashes]
///     (first
///         (#_"-/" filter (fn [[s m]] (apply distinct_9_ (map #(shift_mask s m %) hashes)))
///             (#_"-/" for [mask (map #(dec (<< 1 %)) (range 1 (inc max_mask_bits))) shift (range 0 31)]
///                 [shift mask]
///             )
///         )
///     )
/// )

/// (defn_ case_map [case_f test_f tests thens]
///     (into (sorted_map)
///         (zipmap
///             (map case_f tests)
///             (map vector (map test_f tests) thens)
///         )
///     )
/// )

/// (defn_ fits_table_9_ [ints]
///     (< (-'- (apply max (seq ints)) (apply min (seq ints))) max_switch_table_size)
/// )

/// (defn_ prep_ints [tests thens]
///     (if (fits_table_9_ tests)
///         [0 0 (case_map int int tests thens) _0_compact]
///         (let [[shift mask] (or (maybe_min_hash (map int tests)) [0 0])]
///             (if (zero_9_ mask)
///                 [0 0 (case_map int int tests thens) _0_sparse]
///                 [shift mask (case_map #(shift_mask shift mask (int %)) int tests thens) _0_compact]
///             )
///         )
///     )
/// )

/// (defn_ merge_hash_collisions [expr_sym default tests thens]
///     (let [buckets
///             (loop_when_recur [m (hash_map) ks tests vs thens]
///                              (and ks vs)
///                              [(update m (f_1_hashcode (first ks)) (fnil conj (vector)) [(first ks) (first vs)]) (next ks) (next vs)]
///                           => m
///             )
///           assoc_multi
///             (fn [m h bucket] (assoc m h `(condp = ~expr_sym ~@(apply concat bucket) ~default)))
///           hmap
///             (reduce
///                 (fn [m [h bucket]]
///                     (if (= (count bucket) 1)
///                         (assoc m (first (first bucket)) (second (first bucket)))
///                         (assoc_multi m h bucket)
///                     )
///                 )
///                 (hash_map) buckets
///             )
///           skip_check
///             (->> buckets
///                 (filter #(< 1 (count (second %))))
///                 (map first)
///                 (into (hash_set))
///             )]
///         [(keys hmap) (vals hmap) skip_check]
///     )
/// )

/// (defn_ prep_hashes [expr_sym default tests thens]
///     (let [hashes (into (hash_set) (map f_1_hashcode tests))]
///         (if (= (count tests) (count hashes))
///             (if (fits_table_9_ hashes)
///                 [0 0 (case_map f_1_hashcode identity tests thens) _0_compact]
///                 (let [[shift mask] (or (maybe_min_hash hashes) [0 0])]
///                     (if (zero_9_ mask)
///                         [0 0 (case_map f_1_hashcode identity tests thens) _0_sparse]
///                         [shift mask (case_map #(shift_mask shift mask (f_1_hashcode %)) identity tests thens) _0_compact]
///                     )
///                 )
///             )
///             (let [[tests thens skip_check] (merge_hash_collisions expr_sym default tests thens)
///                   [shift mask case_map switch_type] (prep_hashes expr_sym default tests thens)
///                   skip_check
///                     (if (zero_9_ mask)
///                         skip_check
///                         (into (hash_set) (map #(shift_mask shift mask %) skip_check))
///                     )]
///                 [shift mask case_map switch_type skip_check]
///             )
///         )
///     )
/// )

/// (defmacro case [e & clauses]
///     (let [e_1_ (gensym)
///           default
///             (when (odd_9_ (count clauses)) => `(throw (str "no matching clause: " ~e_1_))
///                 (last clauses)
///             )]
///         (when (<= 2 (count clauses)) => `(let [~e_1_ ~e] ~default)
///             (let [pairs (partition 2 clauses)
///                   assoc_test
///                     (fn [m test expr]
///                         (when_not (contains_9_ m test) => (throw (str "duplicate case test constant: " test))
///                             (assoc m test expr)
///                         )
///                     )
///                   pairs
///                     (reduce
///                         (fn [m [test expr]]
///                             (if (seq_9_ test)
///                                 (reduce #(assoc_test %1 %2 expr) m test)
///                                 (assoc_test m test expr)
///                             )
///                         )
///                         (hash_map) pairs
///                     )
///                   tests (keys pairs)
///                   thens (vals pairs)
///                   mode
///                     (cond
///                         (every_9_ #(and (integer_9_ %) (<= Integer::MIN_VALUE % Integer::MAX_VALUE)) tests) _0_ints
///                         (every_9_ keyword_9_ tests) _0_identity
///                         _0_else _0_hashes
///                     )]
///                 (condp = mode
///                     _0_ints
///                         (let [[shift mask imap switch_type] (prep_ints tests thens)]
///                             `(let [~e_1_ ~e] (case_8_ ~e_1_ ~shift ~mask ~default ~imap ~switch_type _0_int))
///                         )
///                     _0_hashes
///                         (let [[shift mask imap switch_type skip_check] (prep_hashes e_1_ default tests thens)]
///                             `(let [~e_1_ ~e] (case_8_ ~e_1_ ~shift ~mask ~default ~imap ~switch_type _0_hash_equiv ~skip_check))
///                         )
///                     _0_identity
///                         (let [[shift mask imap switch_type skip_check] (prep_hashes e_1_ default tests thens)]
///                             `(let [~e_1_ ~e] (case_8_ ~e_1_ ~shift ~mask ~default ~imap ~switch_type _0_hash_identity ~skip_check))
///                         )
///                 )
///             )
///         )
///     )
/// )
// )

// redefine reduce with IReduce

/// (defn_ seq_reduce
///     ([s f] (if_some [s (seq s)] (seq_reduce (next s) f (first s)) (f)))
///     ([s f r]
///         (loop_when [r r s (seq s)] s => r
///             (let [r (f r (first s))]
///                 (if (reduced_9_ r) (deref r) (recur r (next s)))
///             )
///         )
///     )
/// )

/// (defn reduce
///     ([f s]
///         (if (satisfies_9_ IReduce s)
///             (IReduce_3_reduce s, f)
///             (seq_reduce s f)
///         )
///     )
///     ([f r s]
///         (if (satisfies_9_ IReduce s)
///             (IReduce_3_reduce s, f, r)
///             (seq_reduce s f r)
///         )
///     )
/// )

/// (defn reduce_kv [f r m]
///     (when (some_9_ m) => r
///         (condp satisfies_9_ m
///             IKVReduce      (IKVReduce_3_kvreduce m, f, r)
///             IPersistentMap (reduce (fn [r [k v]] (f r k v)) r m)
///         )
///     )
/// )

/// (defn completing
///     ([f] (completing f identity))
///     ([f cf]
///         (fn
///             ([] (f))
///             ([x] (cf x))
///             ([x y] (f x y))
///         )
///     )
/// )

/// (defn transduce
///     ([xform f s] (transduce xform f (f) s))
///     ([xform f r s] (let [f (xform f)] (f (reduce f r s))))
/// )

/// (defn into
///     ([] (vector))
///     ([to] to)
///     ([to from]
///         (if (editable_9_ to)
///             (with_meta (reduce_4_ conj_4_ to from) (meta to))
///             (reduce conj to from)
///         )
///     )
///     ([to xform from]
///         (if (editable_9_ to)
///             (with_meta (persistent_4_ (transduce xform conj_4_ (transient to) from)) (meta to))
///             (transduce xform conj to from)
///         )
///     )
/// )

/// (defn group_by [f s] (reduce_4_ #(let [k (f %2)] (assoc_4_ %1 k (conj (get %1 k (vector)) %2))) (hash_map) s))

/// (defn frequencies [s] (reduce_4_ #(assoc_4_ %1 %2 (inc (get %1 %2 0))) (hash_map) s))

/// (defn reductions
///     ([f coll]
///         (lazy_seq
///             (if_some [s (seq coll)]
///                 (reductions f (first s) (next s))
///                 (list (f))
///             )
///         )
///     )
///     ([f init coll]
///         (if (reduced_9_ init)
///             (list (deref init))
///             (cons init
///                 (lazy_seq
///                     (when_some [s (seq coll)]
///                         (reductions f (f init (first s)) (next s))
///                     )
///                 )
///             )
///         )
///     )
/// )

/// (defn every_pred
///     ([p]
///         (fn ep_
///             ([] true)
///             ([x] (boolean (p x)))
///             ([x y] (boolean (and (p x) (p y))))
///             ([x y z] (boolean (and (p x) (p y) (p z))))
///             ([x y z & args] (boolean (and (ep_ x y z) (every_9_ p args))))
///         )
///     )
///     ([p1 p2]
///         (fn ep_
///             ([] true)
///             ([x] (boolean (and (p1 x) (p2 x))))
///             ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
///             ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
///             ([x y z & args] (boolean (and (ep_ x y z) (every_9_ #(and (p1 %) (p2 %)) args))))
///         )
///     )
///     ([p1 p2 p3]
///         (fn ep_
///             ([] true)
///             ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
///             ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
///             ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
///             ([x y z & args] (boolean (and (ep_ x y z) (every_9_ #(and (p1 %) (p2 %) (p3 %)) args))))
///         )
///     )
///     ([p1 p2 p3 & ps]
///         (let [ps (list_8_ p1 p2 p3 ps)]
///             (fn ep_
///                 ([] true)
///                 ([x] (every_9_ #(% x) ps))
///                 ([x y] (every_9_ #(and (% x) (% y)) ps))
///                 ([x y z] (every_9_ #(and (% x) (% y) (% z)) ps))
///                 ([x y z & args] (boolean (and (ep_ x y z) (every_9_ #(every_9_ % args) ps))))
///             )
///         )
///     )
/// )

/// (defn some_fn
///     ([p]
///         (fn sp_
///             ([] nil)
///             ([x] (p x))
///             ([x y] (or (p x) (p y)))
///             ([x y z] (or (p x) (p y) (p z)))
///             ([x y z & args] (or (sp_ x y z) (some p args)))
///         )
///     )
///     ([p1 p2]
///         (fn sp_
///             ([] nil)
///             ([x] (or (p1 x) (p2 x)))
///             ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
///             ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
///             ([x y z & args] (or (sp_ x y z) (some #(or (p1 %) (p2 %)) args)))
///         )
///     )
///     ([p1 p2 p3]
///         (fn sp_
///             ([] nil)
///             ([x] (or (p1 x) (p2 x) (p3 x)))
///             ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
///             ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
///             ([x y z & args] (or (sp_ x y z) (some #(or (p1 %) (p2 %) (p3 %)) args)))
///         )
///     )
///     ([p1 p2 p3 & ps]
///         (let [ps (list_8_ p1 p2 p3 ps)]
///             (fn sp_
///                 ([] nil)
///                 ([x] (some #(% x) ps))
///                 ([x y] (some #(or (% x) (% y)) ps))
///                 ([x y z] (some #(or (% x) (% y) (% z)) ps))
///                 ([x y z & args] (or (sp_ x y z) (some #(some % args) ps)))
///             )
///         )
///     )
/// )

/// (defmacro cond_> [e & s]
///     (assert_args
///         (even_9_ (count s)) "an even number of forms as clauses"
///     )
///     (let [e_1_ (gensym)
///           s (map (fn [[_9_ x]] `(if ~_9_ (-> ~e_1_ ~x) ~e_1_)) (partition 2 s))]
///         `(let [~e_1_ ~e ~@(interleave (repeat e_1_) (butlast s))]
///             ~(if (seq s) (last s) e_1_)
///         )
///     )
/// )

/// (defmacro cond_>> [e & s]
///     (assert_args
///         (even_9_ (count s)) "an even number of forms as clauses"
///     )
///     (let [e_1_ (gensym)
///           s (map (fn [[_9_ x]] `(if ~_9_ (->> ~e_1_ ~x) ~e_1_)) (partition 2 s))]
///         `(let [~e_1_ ~e ~@(interleave (repeat e_1_) (butlast s))]
///             ~(if (seq s) (last s) e_1_)
///         )
///     )
/// )

/// (defmacro as_> [e e_1_ & s]
///     `(let [~e_1_ ~e ~@(interleave (repeat e_1_) (butlast s))]
///         ~(if (seq s) (last s) e_1_)
///     )
/// )

/// (defmacro some_> [e & s]
///     (let [e_1_ (gensym)
///           s (map (fn [x] `(when (some_9_ ~e_1_) (-> ~e_1_ ~x))) s)]
///         `(let [~e_1_ ~e ~@(interleave (repeat e_1_) (butlast s))]
///             ~(if (seq s) (last s) e_1_)
///         )
///     )
/// )

/// (defmacro some_>> [e & s]
///     (let [e_1_ (gensym)
///           s (map (fn [x] `(when (some_9_ ~e_1_) (->> ~e_1_ ~x))) s)]
///         `(let [~e_1_ ~e ~@(interleave (repeat e_1_) (butlast s))]
///             ~(if (seq s) (last s) e_1_)
///         )
///     )
/// )

/// (defn halt_when
///     ([f_9_] (halt_when f_9_ nil))
///     ([f_9_ h]
///         (fn [g]
///             (fn
///                 ([] (g))
///                 ([s]
///                     (when (and (map_9_ s) (contains_9_ s _0_0_halt)) => (g s)
///                         (_0_0_halt s)
///                     )
///                 )
///                 ([s x]
///                     (when (f_9_ x) => (g s x)
///                         (reduced {_0_0_halt (if (some_9_ h) (h (g s) x) x)})
///                     )
///                 )
///             )
///         )
///     )
/// )

/// (defn run_4_ [proc coll]
///     (reduce #(proc %2) nil coll)
///     nil
/// )
}

namespace arbace {
    /* oops! enum */class Context;

    class Expr {
        public: virtual IPersistentVector* Expr_3_emit(const Context* context, const IPersistentMap* scope, const IPersistentVector* gen) = 0;
    };

    class Recur { };

    class LiteralExpr { };
    class UnresolvedVarExpr { };
    class VarExpr { };
    class TheVarExpr { };
    class BodyExpr { };
    class MetaExpr { };
    class IfExpr { };
    class MapExpr { };
    class SetExpr { };
    class VectorExpr { };
    class InvokeExpr { };
    class LocalBinding { };
    class LocalBindingExpr { };
    class FnMethod { };
    class FnExpr { };
    class DefExpr { };
    class LetFnExpr { };
    class LetExpr { };
    class RecurExpr { };
    class CaseExpr { };
    class MonitorExpr { };
    class CatchClause { };
    class TryExpr { };
    class ThrowExpr { };
}

namespace arbace {

// (about #_"Cache"
///     (defn #_"<K, V> void" Cache_1_purge [#_"ReferenceQueue" queue, #_"{K Reference<V>}'" cache]
///         (when (some_9_ (ReferenceQueue_2_poll queue))
///             (while (some_9_ (ReferenceQueue_2_poll queue)))
///             (doseq [#_"IMapEntry<K, Reference<V>>" e (deref cache)]
///                 (let_when [#_"Reference<V>" r (val e)] (and (some_9_ r) (nil_9_ (Reference_2_get r)))
///                     (swap_4_ cache #(if (identical_9_ (get % (key e)) r) (dissoc % (key e)) %))
///                 )
///             )
///         )
///         nil
///     )
// )
}

namespace arbace {

// (about #_"Machine"
///     (defn #_"Object" Machine_1_compute [#_"code" code, #_"array" vars]
///         (loop [#_"stack" s nil #_"int" i 0]
///             (let [[x y] (nth code i)]
///                 (case_4_ x
///                     _0_and               (let [[  b a & s] s]                             (recur (cons (bit_and a b) s)            (inc i)))
///                     _0_anew              (let [[    a & s] s]                             (recur (cons (anew a) s)           (inc i)))
///                     _0_apply             (let [[  b a & s] s]                             (recur (cons (apply a b) s)        (inc i)))
///                     _0_aset              (let [[c b a & s] s] (aset_4_ a b c)               (recur s                           (inc i)))
///                     _0_create            (let [[    a & s] s]                             (recur (cons (Closure_1_new y, a) s) (inc i)))
///                     _0_dup               (let [[    a]     s]                             (recur (cons a s)                  (inc i)))
///                     _0_get               (let [[    a & s] s]                             (recur (cons (get (deref (_0__env a)) y) s) (inc i)))
///                     _0_goto                                                               (recur s                        (deref y))
///                     _0_if_eq_9_            (let [[  b a & s] s]                             (recur s        (if     (= a b) (deref y) (inc i))))
///                     _0_if_ne_9_            (let [[  b a & s] s]                             (recur s        (if_not (= a b) (deref y) (inc i))))
///                     _0_if_nil_9_           (let [[    a & s] s]                             (recur s        (if  (nil_9_ a)   (deref y) (inc i))))
///                     _0_if_not            (let [[    a & s] s]                             (recur s        (if_not    a    (deref y) (inc i))))
///                     _0_invoke_1          (let [[    a & s] s]                             (recur (cons (y a) s)              (inc i)))
///                     _0_invoke_2          (let [[  b a & s] s]                             (recur (cons (y a b) s)            (inc i)))
///                     _0_load                                                               (recur (cons (aget vars y) s)      (inc i))
///                     _0_monitor_enter     (let [[    a & s] s] (monitor_enter a)           (recur s                           (inc i)))
///                     _0_monitor_exit      (let [[    a & s] s] (monitor_exit a)            (recur s                           (inc i)))
///                     _0_number_9_           (let [[    a & s] s]                             (recur (cons (number_9_ a) s)        (inc i)))
///                     _0_pop                                                                (recur (next s)                    (inc i))
///                     _0_push                                                               (recur (cons y s)                  (inc i))
///                     _0_put               (let [[  b a & s] s] (swap_4_ (_0__env a) assoc y b) (recur s                           (inc i)))
///                     _0_return                                 (first s)
///                     _0_shr               (let [[  b a & s] s]                             (recur (cons (>> a b) s)           (inc i)))
///                     _0_store             (let [[    a & s] s] (aset_4_ vars y a)            (recur s                           (inc i)))
///                     _0_swap              (let [[  b a & s] s]                             (recur (list_8_ a b s)               (inc i)))
///                     _0_throw                                  (throw (first s))
///                 )
///             )
///         )
///     )
// )
}

namespace arbace {

// (about #_"asm"
///     (defn_ #_"IPersistentVector" Gen_1_new [] (vector))

///     (defn_ #_"label" Gen_2_label [#_"IPersistentVector" gen] (atom nil))

///     (defn_ Gen_2_mark
///         (#_"label" [#_"IPersistentVector" gen] (atom (count gen)))
///         (#_"IPersistentVector" [#_"IPersistentVector" gen, #_"label" label] (reset_4_ label (count gen)) gen)
///     )

///     (defn_ #_"IPersistentVector" Gen_2_and           [#_"IPersistentVector" gen]                          (conj gen [_0_and]))
///     (defn_ #_"IPersistentVector" Gen_2_anew          [#_"IPersistentVector" gen]                          (conj gen [_0_anew]))
///     (defn_ #_"IPersistentVector" Gen_2_apply         [#_"IPersistentVector" gen]                          (conj gen [_0_apply]))
///     (defn_ #_"IPersistentVector" Gen_2_aset          [#_"IPersistentVector" gen]                          (conj gen [_0_aset]))
///     (defn_ #_"IPersistentVector" Gen_2_create        [#_"IPersistentVector" gen, #_"FnExpr" fun]          (conj gen [_0_create fun]))
///     (defn_ #_"IPersistentVector" Gen_2_dup           [#_"IPersistentVector" gen]                          (conj gen [_0_dup]))
///     (defn_ #_"IPersistentVector" Gen_2_get           [#_"IPersistentVector" gen, #_"Symbol" name]         (conj gen [_0_get name]))
///     (defn_ #_"IPersistentVector" Gen_2_goto          [#_"IPersistentVector" gen, #_"label" label]         (conj gen [_0_goto label]))
///     (defn_ #_"IPersistentVector" Gen_2_if_eq_9_        [#_"IPersistentVector" gen, #_"label" label]         (conj gen [_0_if_eq_9_ label]))
///     (defn_ #_"IPersistentVector" Gen_2_if_ne_9_        [#_"IPersistentVector" gen, #_"label" label]         (conj gen [_0_if_ne_9_ label]))
///     (defn_ #_"IPersistentVector" Gen_2_if_nil_9_       [#_"IPersistentVector" gen, #_"label" label]         (conj gen [_0_if_nil_9_ label]))
///     (defn_ #_"IPersistentVector" Gen_2_if_not        [#_"IPersistentVector" gen, #_"label" label]         (conj gen [_0_if_not label]))
///     (defn_ #_"IPersistentVector" Gen_2_invoke        [#_"IPersistentVector" gen, #_"IFn" f, #_"int" arity] (conj gen [(-/keyword (str "invoke" \- arity)) f]))
///     (defn_ #_"IPersistentVector" Gen_2_load          [#_"IPersistentVector" gen, #_"int" index]           (conj gen [_0_load index]))
///     (defn_ #_"IPersistentVector" Gen_2_monitor_enter [#_"IPersistentVector" gen]                          (conj gen [_0_monitor_enter]))
///     (defn_ #_"IPersistentVector" Gen_2_monitor_exit  [#_"IPersistentVector" gen]                          (conj gen [_0_monitor_exit]))
///     (defn_ #_"IPersistentVector" Gen_2_number_9_       [#_"IPersistentVector" gen]                          (conj gen [_0_number_9_]))
///     (defn_ #_"IPersistentVector" Gen_2_pop           [#_"IPersistentVector" gen]                          (conj gen [_0_pop]))
///     (defn_ #_"IPersistentVector" Gen_2_push          [#_"IPersistentVector" gen, #_"Object" value]         (conj gen [_0_push value]))
///     (defn_ #_"IPersistentVector" Gen_2_put           [#_"IPersistentVector" gen, #_"Symbol" name]         (conj gen [_0_put name]))
///     (defn_ #_"IPersistentVector" Gen_2_return        [#_"IPersistentVector" gen]                          (conj gen [_0_return]))
///     (defn_ #_"IPersistentVector" Gen_2_shr           [#_"IPersistentVector" gen]                          (conj gen [_0_shr]))
///     (defn_ #_"IPersistentVector" Gen_2_store         [#_"IPersistentVector" gen, #_"int" index]           (conj gen [_0_store index]))
///     (defn_ #_"IPersistentVector" Gen_2_swap          [#_"IPersistentVector" gen]                          (conj gen [_0_swap]))
///     (defn_ #_"IPersistentVector" Gen_2_throw         [#_"IPersistentVector" gen]                          (conj gen [_0_throw]))

///     (defn_ #_"IPersistentVector" Gen_2_lookup_switch [#_"IPersistentVector" gen, #_"ints" values, #_"labels" labels, #_"label" default]
///         (conj gen [_0_lookup_switch (vec values) (mapv deref labels) (deref default)])
///     )

///     (defn_ #_"IPersistentVector" Gen_2_table_switch [#_"IPersistentVector" gen, #_"int" low, #_"int" high, #_"labels" labels, #_"label" default]
///         (conj gen [_0_table_switch low high (mapv deref labels) (deref default)])
///     )

///     (defn_ #_"IPersistentVector" Gen_2_try_catch_finally [#_"IPersistentVector" gen, #_"label" start, #_"label" end, #_"label" finally]
///         (conj gen [_0_try_catch_finally (deref start) (deref end) (deref finally)])
///     )
// )

/// (def Context_1_enum_set
///     (hash_set
///         _0_Context_1_STATEMENT
///         _0_Context_1_EXPRESSION
///         _0_Context_1_RETURN
///     )
/// )

// (about #_"Compiler"
///     (def #_"int" Compiler_1_MAX_POSITIONAL_ARITY #_9 (+ 9 2))

///     (defn #_"Namespace" Compiler_1_namespaceFor
///         ([#_"Symbol" sym] (Compiler_1_namespaceFor _8_ns_8_, sym))
///         ([#_"Namespace" inns, #_"Symbol" sym]
///             (let [#_"Symbol" nsSym (symbol (_0_ns sym))]
///                 (or (Namespace_2_getAlias inns, nsSym) (find_ns nsSym))
///             )
///         )
///     )

///     (defn #_"Symbol" Compiler_1_resolveSymbol [#_"Symbol" sym]
///         (cond
///             (pos_9_ (String_2_indexOf (_0_name sym), (int \.)))
///                 sym
///             (some_9_ (_0_ns sym))
///                 (let [#_"Namespace" ns (Compiler_1_namespaceFor sym)]
///                     (if (and (some_9_ ns) (not (and (some_9_ (_0_name (_0_name ns))) (= (_0_name (_0_name ns)) (_0_ns sym)))))
///                         (symbol (_0_name (_0_name ns)) (_0_name sym))
///                         sym
///                     )
///                 )
///             _0_else
///                 (let [#_"Object" o (Namespace_2_getMapping _8_ns_8_, sym)]
///                     (cond
///                         (nil_9_ o) (symbol (_0_name (_0_name _8_ns_8_)) (_0_name sym))
///                         (var_9_ o) (symbol (_0_name (_0_name (_0_ns o))) (_0_name (_0_sym o)))
///                     )
///                 )
///         )
///     )

///     (defn #_"Var" Compiler_1_lookupVar [#_"Symbol" sym, #_"bool" intern_9_]
///         (cond
///             (some_9_ (_0_ns sym))
///                 (when_some [#_"Namespace" ns (Compiler_1_namespaceFor sym)]
///                     (let [#_"Symbol" name (symbol (_0_name sym))]
///                         (if (and intern_9_ (= ns _8_ns_8_))
///                             (Namespace_2_intern ns, name)
///                             (Namespace_2_findInternedVar ns, name)
///                         )
///                     )
///                 )
///             _0_else
///                 (let [#_"Object" o (Namespace_2_getMapping _8_ns_8_, sym)]
///                     (cond
///                         (nil_9_ o)
///                             (when intern_9_
///                                 (Namespace_2_intern _8_ns_8_, (symbol (_0_name sym)))
///                             )
///                         (var_9_ o)
///                             o
///                         _0_else
///                             (throw (str "expecting var, but " sym " is mapped to " o))
///                     )
///                 )
///         )
///     )

///     (defn #_"Var" Compiler_1_maybeMacro [#_"Object" op, #_"IPersistentMap" scope]
///         (when_not (and (symbol_9_ op) (some_9_ (get (deref (get scope _0_1_local_env)) op)))
///             (when (or (symbol_9_ op) (var_9_ op))
///                 (let [#_"Var" v (if (var_9_ op) op (Compiler_1_lookupVar op, false))]
///                     (when (and (some_9_ v) (get (meta v) _0_macro))
///                         (when (or (= (_0_ns v) _8_ns_8_) (not (get (meta v) _0_private))) => (throw (str "var: " v " is private"))
///                             v
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"IFn" Compiler_1_maybeInline [#_"Object" op, #_"int" arity, #_"IPersistentMap" scope]
///         (when_not (and (symbol_9_ op) (some_9_ (get (deref (get scope _0_1_local_env)) op)))
///             (when (or (symbol_9_ op) (var_9_ op))
///                 (when_some [#_"Var" v (if (var_9_ op) op (Compiler_1_lookupVar op, false))]
///                     (when (or (= (_0_ns v) _8_ns_8_) (not (get (meta v) _0_private))) => (throw (str "var: " v " is private"))
///                         (when_some [#_"IFn" f (get (meta v) _0_inline)]
///                             (let [#_"IFn" arityPred (get (meta v) _0_inline_arities)]
///                                 (when (or (nil_9_ arityPred) (IFn_3_invoke arityPred, arity))
///                                     f
///                                 )
///                             )
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Object" Compiler_1_resolveIn [#_"Namespace" n, #_"Symbol" sym, #_"bool" allowPrivate]
///         (cond
///             (some_9_ (_0_ns sym))
///                 (when_some [#_"Namespace" ns (Compiler_1_namespaceFor n, sym)]                     => (throw (str "no such namespace: " (_0_ns sym)))
///                     (when_some [#_"Var" v (Namespace_2_findInternedVar ns, (symbol (_0_name sym)))] => (throw (str "no such var: " sym))
///                         (when (or (= (_0_ns v) _8_ns_8_) (not (get (meta v) _0_private)) allowPrivate)   => (throw (str "var: " sym " is private"))
///                             v
///                         )
///                     )
///                 )
///             _0_else
///                 (or (Namespace_2_getMapping n, sym) (throw (str "unable to resolve symbol: " sym " in this context")))
///         )
///     )

///     (defn #_"Object" Compiler_1_resolve
///         ([#_"Symbol" sym                          ] (Compiler_1_resolveIn _8_ns_8_, sym, false       ))
///         ([#_"Symbol" sym, #_"bool" allowPrivate] (Compiler_1_resolveIn _8_ns_8_, sym, allowPrivate))
///     )

///     (defn #_"Object" Compiler_1_maybeResolveIn [#_"Namespace" n, #_"Symbol" sym]
///         (cond
///             (some_9_ (_0_ns sym))
///                 (when_some [#_"Namespace" ns (Compiler_1_namespaceFor n, sym)]
///                     (when_some [#_"Var" v (Namespace_2_findInternedVar ns, (symbol (_0_name sym)))]
///                         v
///                     )
///                 )
///             _0_else
///                 (Namespace_2_getMapping n, sym)
///         )
///     )

///     (defn #_"IPersistentVector" Compiler_1_emitArgs [#_"IPersistentMap" scope, #_"IPersistentVector" gen, #_"indexed" args]
///         (let [
///             gen (Gen_2_push gen, (count args))
///             gen (Gen_2_anew gen)
///         ]
///             (loop_when [gen gen #_"int" i 0] (< i (count args)) => gen
///                 (let [
///                     gen (Gen_2_dup gen)
///                     gen (Gen_2_push gen, i)
///                     gen (Expr_3_emit (nth args i), _0_Context_1_EXPRESSION, scope, gen)
///                     gen (Gen_2_aset gen)
///                 ]
///                     (recur gen (inc i))
///                 )
///             )
///         )
///     )

///     (declare FnMethod_2_emitLocal)

///     (defn #_"IPersistentVector" Compiler_1_emitLocals [#_"IPersistentMap" scope, #_"IPersistentVector" gen, #_"IPersistentMap" locals]
///         (let [
///             gen (Gen_2_push gen, (<< (count locals) 1))
///             gen (Gen_2_anew gen)
///         ]
///             (loop_when [gen gen #_"int" i 0 #_"ISeq" s (vals locals)] (some_9_ s) => gen
///                 (let [
///                     #_"LocalBinding" lb (first s)
///                     gen (Gen_2_dup gen)
///                     gen (Gen_2_push gen, i)
///                     gen (Gen_2_push gen, (_0_sym lb))
///                     gen (Gen_2_aset gen)
///                     i (inc i)
///                     gen (Gen_2_dup gen)
///                     gen (Gen_2_push gen, i)
///                     gen (FnMethod_2_emitLocal (get scope _0_fm), gen, lb)
///                     gen (Gen_2_aset gen)
///                     i (inc i)
///                 ]
///                     (recur gen i (next s))
///                 )
///             )
///         )
///     )
// )

// (about #_"LiteralExpr"
///     (defr LiteralExpr)

///     (defn #_"LiteralExpr" LiteralExpr_1_new [#_"Object" value]
///         (new_8_ LiteralExpr_1_class
///             (hash_map
///                 #_"Object" _0_value value
///             )
///         )
///     )

///     (def #_"LiteralExpr" LiteralExpr_1_NIL   (LiteralExpr_1_new nil))
///     (def #_"LiteralExpr" LiteralExpr_1_TRUE  (LiteralExpr_1_new true))
///     (def #_"LiteralExpr" LiteralExpr_1_FALSE (LiteralExpr_1_new false))

///     (defn #_"Expr" LiteralExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"int" n (dec (count form))]
///             (when (= n 1) => (throw (str "wrong number of arguments passed to quote: " n))
///                 (let [#_"Object" value (second form)]
///                     (case_4_ value
///                         nil                 LiteralExpr_1_NIL
///                         true                LiteralExpr_1_TRUE
///                         false               LiteralExpr_1_FALSE
///                         (cond
///                             (string_9_ value) (LiteralExpr_1_new (String_2_intern value))
///                             _0_else           (LiteralExpr_1_new value)
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" LiteralExpr_2_emit [#_"LiteralExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (when_not (= context _0_Context_1_STATEMENT) => gen
///             (Gen_2_push gen, (_0_value this))
///         )
///     )

///     (defm LiteralExpr Expr
///         (Expr_3_emit => LiteralExpr_2_emit)
///     )
// )

// (about #_"UnresolvedVarExpr"
///     (defr UnresolvedVarExpr)

///     (defn #_"UnresolvedVarExpr" UnresolvedVarExpr_1_new [#_"Symbol" symbol]
///         (new_8_ UnresolvedVarExpr_1_class
///             (hash_map
///                 #_"Symbol" _0_symbol symbol
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" UnresolvedVarExpr_2_emit [#_"UnresolvedVarExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         gen
///     )

///     (defm UnresolvedVarExpr Expr
///         (Expr_3_emit => UnresolvedVarExpr_2_emit)
///     )
// )

// (about #_"VarExpr"
///     (defr VarExpr)

///     (defn #_"VarExpr" VarExpr_1_new [#_"Var" var]
///         (new_8_ VarExpr_1_class
///             (hash_map
///                 #_"Var" _0_var var
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" VarExpr_2_emit [#_"VarExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen (Gen_2_push gen, (_0_var this))
///             gen (Gen_2_invoke gen, var_get, 1)
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm VarExpr Expr
///         (Expr_3_emit => VarExpr_2_emit)
///     )
// )

// (about #_"TheVarExpr"
///     (defr TheVarExpr)

///     (defn #_"TheVarExpr" TheVarExpr_1_new [#_"Var" var]
///         (new_8_ TheVarExpr_1_class
///             (hash_map
///                 #_"Var" _0_var var
///             )
///         )
///     )

///     (defn #_"Expr" TheVarExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"Symbol" sym (second form) #_"Var" v (Compiler_1_lookupVar sym, false)]
///             (when (some_9_ v) => (throw (str "unable to resolve var: " sym " in this context"))
///                 (TheVarExpr_1_new v)
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" TheVarExpr_2_emit [#_"TheVarExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (when_not (= context _0_Context_1_STATEMENT) => gen
///             (Gen_2_push gen, (_0_var this))
///         )
///     )

///     (defm TheVarExpr Expr
///         (Expr_3_emit => TheVarExpr_2_emit)
///     )
// )

// (about #_"BodyExpr"
///     (defr BodyExpr)

///     (defn #_"BodyExpr" BodyExpr_1_new [#_"vector" exprs]
///         (new_8_ BodyExpr_1_class
///             (hash_map
///                 #_"vector" _0_exprs exprs
///             )
///         )
///     )

///     (declare Compiler_1_analyze)

///     (defn #_"Expr" BodyExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"ISeq" s form s (if (= (first s) _1_do) (next s) s)
///               #_"vector" v
///                 (loop_when [v (vector) s s] (some_9_ s) => v
///                     (let [#_"Context" c (if (or (= context _0_Context_1_STATEMENT) (some_9_ (next s))) _0_Context_1_STATEMENT context)]
///                         (recur (conj v (Compiler_1_analyze (first s), c, scope)) (next s))
///                     )
///                 )]
///             (BodyExpr_1_new (if (pos_9_ (count v)) v (conj v LiteralExpr_1_NIL)))
///         )
///     )

///     (defn_ #_"IPersistentVector" BodyExpr_2_emit [#_"BodyExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (loop_when_recur [gen gen #_"ISeq" s (seq (_0_exprs this))]
///                          (some_9_ (next s))
///                          [(Expr_3_emit (first s), _0_Context_1_STATEMENT, scope, gen) (next s)]
///                       => (Expr_3_emit (first s), context, scope, gen)
///         )
///     )

///     (defm BodyExpr Expr
///         (Expr_3_emit => BodyExpr_2_emit)
///     )
// )

// (about #_"MetaExpr"
///     (defr MetaExpr)

///     (defn #_"MetaExpr" MetaExpr_1_new [#_"Expr" expr, #_"Expr" meta]
///         (new_8_ MetaExpr_1_class
///             (hash_map
///                 #_"Expr" _0_expr expr
///                 #_"Expr" _0_meta meta
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" MetaExpr_2_emit [#_"MetaExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen (Expr_3_emit (_0_expr this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Expr_3_emit (_0_meta this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Gen_2_invoke gen, with_meta, 2)
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm MetaExpr Expr
///         (Expr_3_emit => MetaExpr_2_emit)
///     )
// )

// (about #_"IfExpr"
///     (defr IfExpr)

///     (defn #_"IfExpr" IfExpr_1_new [#_"Expr" test, #_"Expr" then, #_"Expr" else]
///         (new_8_ IfExpr_1_class
///             (hash_map
///                 #_"Expr" _0_test test
///                 #_"Expr" _0_then then
///                 #_"Expr" _0_else else
///             )
///         )
///     )

///     (defn #_"Expr" IfExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (cond
///             (< 4 (count form)) (throw "too many arguments to if")
///             (< (count form) 3) (throw "too few arguments to if")
///         )
///         (let [#_"Expr" test (Compiler_1_analyze (second form), scope)
///               #_"Expr" then (Compiler_1_analyze (third form), context, scope)
///               #_"Expr" else (Compiler_1_analyze (fourth form), context, scope)]
///             (IfExpr_1_new test, then, else)
///         )
///     )

///     (defn_ #_"IPersistentVector" IfExpr_2_emit [#_"IfExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             #_"label" l_1_nil (Gen_2_label gen) #_"label" l_1_false (Gen_2_label gen) #_"label" l_1_end (Gen_2_label gen)
///             gen (Expr_3_emit (_0_test this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Gen_2_dup gen)
///             gen (Gen_2_if_nil_9_ gen, l_1_nil)
///             gen (Gen_2_push gen, false)
///             gen (Gen_2_if_eq_9_ gen, l_1_false)
///             gen (Expr_3_emit (_0_then this), context, scope, gen)
///             gen (Gen_2_goto gen, l_1_end)
///             gen (Gen_2_mark gen, l_1_nil)
///             gen (Gen_2_pop gen)
///             gen (Gen_2_mark gen, l_1_false)
///             gen (Expr_3_emit (_0_else this), context, scope, gen)
///             gen (Gen_2_mark gen, l_1_end)
///         ]
///             gen
///         )
///     )

///     (defm IfExpr Expr
///         (Expr_3_emit => IfExpr_2_emit)
///     )
// )

// (about #_"MapExpr"
///     (defr MapExpr)

///     (defn #_"MapExpr" MapExpr_1_new [#_"vector" args]
///         (new_8_ MapExpr_1_class
///             (hash_map
///                 #_"vector" _0_args args
///             )
///         )
///     )

///     (defn #_"Expr" MapExpr_1_parse [#_"IPersistentMap" form, #_"IPersistentMap" scope]
///         (let [[#_"vector" args #_"bool" literal_9_]
///                 (loop_when [args (vector), literal_9_ true, #_"set" keys (hash_set), #_"ISeq" s (seq form)] (some_9_ s) => [args literal_9_]
///                     (let [#_"pair" e (first s) #_"Expr" k (Compiler_1_analyze (key e), scope) #_"Expr" v (Compiler_1_analyze (val e), scope)
///                           [literal_9_ keys]
///                             (when (satisfies_9_ LiteralExpr k) => [false keys]
///                                 (when_not (contains_9_ keys (_0_value k)) => (throw "duplicate constant keys in map")
///                                     [literal_9_ (conj keys (_0_value k))]
///                                 )
///                             )]
///                         (recur (conj args k v) (and literal_9_ (satisfies_9_ LiteralExpr v)) keys (next s))
///                     )
///                 )
///               #_"Expr" e
///                 (when literal_9_ => (MapExpr_1_new args)
///                     (LiteralExpr_1_new (apply hash_map (map _0_value args)))
///                 )]
///             (when_some [#_"IPersistentMap" m (meta form)] => e
///                 (MetaExpr_1_new e, (MapExpr_1_parse m, scope))
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" MapExpr_2_emit [#_"MapExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             #_"int" n (count (_0_args this))
///             [#_"bool" literal_9_ #_"bool" unique_9_]
///                 (loop_when [literal_9_ true, unique_9_ true, #_"set" keys (hash_set), #_"int" i 0] (< i n) => [literal_9_ unique_9_]
///                     (let [#_"Expr" k (nth (_0_args this) i)
///                           [literal_9_ unique_9_ keys]
///                             (when (satisfies_9_ LiteralExpr k) => [false unique_9_ keys]
///                                 (when_not (contains_9_ keys (_0_value k)) => [literal_9_ false keys]
///                                     [literal_9_ unique_9_ (conj keys (_0_value k))]
///                                 )
///                             )]
///                         (recur literal_9_ unique_9_ keys (+ i 2))
///                     )
///                 )
///             gen (Compiler_1_emitArgs scope, gen, (_0_args this))
///             gen (Gen_2_invoke gen, (if (or (and literal_9_ unique_9_) (<= n 2)) RT_1_mapUniqueKeys RT_1_map), 1)
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm MapExpr Expr
///         (Expr_3_emit => MapExpr_2_emit)
///     )
// )

// (about #_"SetExpr"
///     (defr SetExpr)

///     (defn #_"SetExpr" SetExpr_1_new [#_"vector" args]
///         (new_8_ SetExpr_1_class
///             (hash_map
///                 #_"vector" _0_args args
///             )
///         )
///     )

///     (defn #_"Expr" SetExpr_1_parse [#_"set" form, #_"IPersistentMap" scope]
///         (let [[#_"vector" args #_"bool" literal_9_]
///                 (loop_when [args (vector) literal_9_ true #_"ISeq" s (seq form)] (some_9_ s) => [args literal_9_]
///                     (let [#_"Expr" e (Compiler_1_analyze (first s), scope)]
///                         (recur (conj args e) (and literal_9_ (satisfies_9_ LiteralExpr e)) (next s))
///                     )
///                 )
///               #_"Expr" e
///                 (when literal_9_ => (SetExpr_1_new args)
///                     (LiteralExpr_1_new (apply hash_set (map _0_value args)))
///                 )]
///             (when_some [#_"IPersistentMap" m (meta form)] => e
///                 (MetaExpr_1_new e, (MapExpr_1_parse m, scope))
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" SetExpr_2_emit [#_"SetExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen
///                 (when (seq (_0_args this)) => (Gen_2_push gen, PersistentHashSet_1_EMPTY)
///                     (let [gen (Compiler_1_emitArgs scope, gen, (_0_args this))]
///                         (Gen_2_invoke gen, PersistentHashSet_1_createWithCheck, 1)
///                     )
///                 )
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm SetExpr Expr
///         (Expr_3_emit => SetExpr_2_emit)
///     )
// )

// (about #_"VectorExpr"
///     (defr VectorExpr)

///     (defn #_"VectorExpr" VectorExpr_1_new [#_"vector" args]
///         (new_8_ VectorExpr_1_class
///             (hash_map
///                 #_"vector" _0_args args
///             )
///         )
///     )

///     (defn #_"Expr" VectorExpr_1_parse [#_"vector" form, #_"IPersistentMap" scope]
///         (let [[#_"vector" args #_"bool" literal_9_]
///                 (loop_when [args (vector) literal_9_ true #_"ISeq" s (seq form)] (some_9_ s) => [args literal_9_]
///                     (let [#_"Expr" e (Compiler_1_analyze (first s), scope)]
///                         (recur (conj args e) (and literal_9_ (satisfies_9_ LiteralExpr e)) (next s))
///                     )
///                 )
///               #_"Expr" e
///                 (when literal_9_ => (VectorExpr_1_new args)
///                     (LiteralExpr_1_new (mapv _0_value args))
///                 )]
///             (when_some [#_"IPersistentMap" m (meta form)] => e
///                 (MetaExpr_1_new e, (MapExpr_1_parse m, scope))
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" VectorExpr_2_emit [#_"VectorExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen
///                 (when (seq (_0_args this)) => (Gen_2_push gen, PersistentVector_1_EMPTY)
///                     (let [gen (Compiler_1_emitArgs scope, gen, (_0_args this))]
///                         (Gen_2_invoke gen, vec, 1)
///                     )
///                 )
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm VectorExpr Expr
///         (Expr_3_emit => VectorExpr_2_emit)
///     )
// )

// (about #_"InvokeExpr"
///     (defr InvokeExpr)

///     (defn #_"InvokeExpr" InvokeExpr_1_new [#_"Expr" fexpr, #_"vector" args]
///         (new_8_ InvokeExpr_1_class
///             (hash_map
///                 #_"Expr" _0_fexpr fexpr
///                 #_"vector" _0_args args
///             )
///         )
///     )

///     (defn #_"Expr" InvokeExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"Expr" fexpr (Compiler_1_analyze (first form), scope)
///               #_"vector" args (mapv #(Compiler_1_analyze %, scope) (next form))]
///             (InvokeExpr_1_new fexpr, args)
///         )
///     )

///     (defn_ #_"IPersistentVector" InvokeExpr_2_emit [#_"InvokeExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen (Expr_3_emit (_0_fexpr this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Compiler_1_emitArgs scope, gen, (_0_args this))
///             gen (Gen_2_apply gen)
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm InvokeExpr Expr
///         (Expr_3_emit => InvokeExpr_2_emit)
///     )
// )

// (about #_"LocalBinding"
///     (defr LocalBinding)

///     (defn #_"LocalBinding" LocalBinding_1_new [#_"Symbol" sym, #_"Expr" init, #_"int" idx]
///         (new_8_ LocalBinding_1_class
///             (hash_map
///                 #_"int" _0_uid (next_id_4_)
///                 #_"Symbol" _0_sym sym
///                 #_"Expr'" _0_1_init (atom init)
///                 #_"int" _0_idx idx
///             )
///         )
///     )
// )

// (about #_"LocalBindingExpr"
///     (defr LocalBindingExpr)

///     (defn #_"LocalBindingExpr" LocalBindingExpr_1_new [#_"LocalBinding" lb]
///         (new_8_ LocalBindingExpr_1_class
///             (hash_map
///                 #_"LocalBinding" _0_lb lb
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" LocalBindingExpr_2_emit [#_"LocalBindingExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (when_not (= context _0_Context_1_STATEMENT) => gen
///             (FnMethod_2_emitLocal (get scope _0_fm), gen, (_0_lb this))
///         )
///     )

///     (defm LocalBindingExpr Expr
///         (Expr_3_emit => LocalBindingExpr_2_emit)
///     )
// )

// (about #_"FnMethod"
///     (defr FnMethod)

///     (defn #_"FnMethod" FnMethod_1_new [#_"FnExpr" fun, #_"FnMethod" parent]
///         (new_8_ FnMethod_1_class
///             (hash_map
///                 #_"FnExpr" _0_fun fun
///                 #_"FnMethod" _0_parent parent
///                 #_"{int LocalBinding}'" _0_1_locals (atom (hash_map))
///                 #_"Integer" _0_arity nil
///                 #_"Expr" _0_body nil
///             )
///         )
///     )

///     (defn #_"FnMethod" FnMethod_1_parse [#_"FnExpr" fun, #_"ISeq" form, #_"IPersistentMap" scope]
///         (let [
///             scope
///                 (-> scope
///                     (update _0_fm (partial FnMethod_1_new fun))
///                     (update _0_1_local_env (comp atom deref))
///                     (assoc _0_1_local_num (atom 0))
///                 )
///             _
///                 (when_some [#_"Symbol" f (_0_fname fun)]
///                     (let [#_"LocalBinding" lb (LocalBinding_1_new f, nil, (deref (get scope _0_1_local_num)))]
///                         (swap_4_ (get scope _0_1_local_env) assoc (_0_sym lb) lb)
///                         (swap_4_ (_0_1_locals (get scope _0_fm)) assoc (_0_uid lb) lb)
///                     )
///                 )
///             [#_"[LocalBinding]" lbs #_"int" arity]
///                 (loop_when [lbs (vector) arity 0 #_"bool" variadic_9_ false #_"ISeq" s (seq (first form))] (some_9_ s) => (if (and variadic_9_ (not (neg_9_ arity))) (throw "missing variadic parameter") [lbs arity])
///                     (let [#_"symbol?" sym (first s)]
///                         (when (symbol_9_ sym)        => (throw "function parameters must be symbols")
///                             (when (nil_9_ (_0_ns sym)) => (throw (str "can't use qualified name as parameter: " sym))
///                                 (cond
///                                     (= sym '&)
///                                         (when_not variadic_9_ => (throw "overkill variadic parameter list")
///                                             (recur lbs arity true (next s))
///                                         )
///                                     (neg_9_ arity)
///                                         (throw (str "excess variadic parameter: " sym))
///                                     ((if variadic_9_ <= <) arity Compiler_1_MAX_POSITIONAL_ARITY)
///                                         (let [
///                                             arity (if_not variadic_9_ (inc arity) (- (inc arity)))
///                                             #_"LocalBinding" lb (LocalBinding_1_new sym, nil, (swap_4_ (get scope _0_1_local_num) inc))
///                                         ]
///                                             (swap_4_ (get scope _0_1_local_env) assoc (_0_sym lb) lb)
///                                             (swap_4_ (_0_1_locals (get scope _0_fm)) assoc (_0_uid lb) lb)
///                                             (recur (conj lbs lb) arity variadic_9_ (next s))
///                                         )
///                                     _0_else
///                                         (throw (str "can't specify more than " Compiler_1_MAX_POSITIONAL_ARITY " positional parameters"))
///                                 )
///                             )
///                         )
///                     )
///                 )
///             scope
///                 (-> scope
///                     (assoc _0_loop_locals lbs)
///                     (update _0_fm assoc _0_arity arity)
///                 )
///         ]
///             (assoc (get scope _0_fm) _0_body (BodyExpr_1_parse (next form), _0_Context_1_RETURN, scope))
///         )
///     )

///     (defn #_"IPersistentVector" FnMethod_2_emitLocal [#_"FnMethod" this, #_"IPersistentVector" gen, #_"LocalBinding" lb]
///         (if (contains_9_ (deref (_0_1_closes (_0_fun this))) (_0_uid lb))
///             (let [
///                 gen (Gen_2_load gen, 0)
///                 gen (Gen_2_get gen, (_0_sym lb))
///             ]
///                 gen
///             )
///             (Gen_2_load gen, (_0_idx lb))
///         )
///     )

///     (defn #_"IPersistentVector" FnMethod_2_compile [#_"FnMethod" this]
///         (let [
///             #_"IPersistentMap" scope (hash_map _0_fm this)
///             #_"IPersistentVector" gen (Gen_1_new)
///             scope (assoc scope _0_loop_label (Gen_2_mark gen))
///             gen (Expr_3_emit (_0_body this), _0_Context_1_RETURN, scope, gen)
///         ]
///             (Gen_2_return gen)
///         )
///     )

///     (def compile_and_memoize (-/memoize FnMethod_2_compile))
// )

// (about #_"FnExpr"
///     (defr FnExpr)

///     (defn #_"FnExpr" FnExpr_1_new []
///         (new_8_ FnExpr_1_class
///             (hash_map
///                 #_"Symbol" _0_fname nil
///                 #_"{int FnMethod}" _0_regulars nil
///                 #_"FnMethod" _0_variadic nil
///                 #_"{int LocalBinding}'" _0_1_closes (atom (hash_map))
///             )
///         )
///     )

///     (defn #_"Expr" FnExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [
///             #_"FnExpr" fun (FnExpr_1_new)
///             [fun form]
///                 (when (symbol_9_ (second form)) => [fun form]
///                     [(assoc fun _0_fname (second form)) (cons _1_fn_8_ (next (next form)))]
///                 )
///             form
///                 (when (vector_9_ (second form)) => form
///                     (list _1_fn_8_ (next form))
///                 )
///             fun
///                 (let [
///                     [#_"{int FnMethod}" regulars #_"FnMethod" variadic]
///                         (loop_when [regulars (hash_map) variadic nil #_"ISeq" s (next form)] (some_9_ s) => [regulars variadic]
///                             (let [#_"FnMethod" fm (FnMethod_1_parse fun, (first s), scope) #_"int" n (_0_arity fm)]
///                                 (if (neg_9_ n)
///                                     (when (nil_9_ variadic) => (throw "can't have more than 1 variadic overload")
///                                         (recur regulars fm (next s))
///                                     )
///                                     (when (nil_9_ (get regulars n)) => (throw "can't have 2 overloads with same arity")
///                                         (recur (assoc regulars n fm) variadic (next s))
///                                     )
///                                 )
///                             )
///                         )
///                 ]
///                     (when (some_9_ variadic)
///                         (loop_when_recur [#_"int" n (- (_0_arity variadic))] (<= n Compiler_1_MAX_POSITIONAL_ARITY) [(inc n)]
///                             (when (some_9_ (get regulars n))
///                                 (throw "can't have fixed arity function with more params than variadic function")
///                             )
///                         )
///                     )
///                     (assoc fun _0_regulars regulars, _0_variadic variadic)
///                 )
///         ]
///             (MetaExpr_1_new fun, (MapExpr_1_parse (meta form), scope)) fun
///         )
///     )

///     (defn_ #_"IPersistentVector" FnExpr_2_emit [#_"FnExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (when_not (= context _0_Context_1_STATEMENT) => gen
///             (let [
///                 gen (Compiler_1_emitLocals scope, gen, (deref (_0_1_closes this)))
///                 gen (Gen_2_invoke gen, RT_1_mapUniqueKeys, 1)
///             ]
///                 (Gen_2_create gen, this)
///             )
///         )
///     )

///     (defm FnExpr Expr
///         (Expr_3_emit => FnExpr_2_emit)
///     )
// )

// (about #_"DefExpr"
///     (defr DefExpr)

///     (defn #_"DefExpr" DefExpr_1_new [#_"Var" var, #_"Expr" init, #_"Expr" meta, #_"bool" initProvided]
///         (new_8_ DefExpr_1_class
///             (hash_map
///                 #_"Var" _0_var var
///                 #_"Expr" _0_init init
///                 #_"Expr" _0_meta meta
///                 #_"bool" _0_initProvided initProvided
///             )
///         )
///     )

///     (defn #_"Expr" DefExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"int" n (count form)]
///             (cond
///                 (< 3 n) (throw "too many arguments to def")
///                 (< n 2) (throw "too few arguments to def")
///                 _0_else
///                     (let_when [#_"symbol?" s (second form)] (symbol_9_ s)     => (throw "first argument to def must be a symbol")
///                         (when_some [#_"Var" v (Compiler_1_lookupVar s, true)] => (throw "can't refer to qualified var that doesn't exist")
///                             (let [v (when_not (= (_0_ns v) _8_ns_8_) => v
///                                         (when (nil_9_ (_0_ns s))                => (throw "can't create defs outside of current ns")
///                                             (Namespace_2_intern _8_ns_8_, s)
///                                         )
///                                     )]
///                                 (DefExpr_1_new v, (Compiler_1_analyze (third form), scope), (Compiler_1_analyze (meta s), scope), (= n 3))
///                             )
///                         )
///                     )
///             )
///         )
///     )

///     (defn_ #_"bool" DefExpr_2_includesExplicitMetadata [#_"DefExpr" this, #_"MapExpr" expr]
///         (loop_when [#_"int" i 0] (< i (count (_0_keyvals expr))) => false
///             (recur_when (= (_0_k (nth (_0_keyvals expr) i)) _0_declared) [(+ i 2)] => true)
///         )
///     )

///     (defn_ #_"IPersistentVector" DefExpr_2_emit [#_"DefExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen (Gen_2_push gen, (_0_var this))
///             gen
///                 (when (some_9_ (_0_meta this)) => gen
///                     (let [
///                         gen (Gen_2_dup gen)
///                         gen (Expr_3_emit (_0_meta this), _0_Context_1_EXPRESSION, scope, gen)
///                         gen (Gen_2_invoke gen, Var_2_resetMeta, 2)
///                     ]
///                         (Gen_2_pop gen)
///                     )
///                 )
///             gen
///                 (when (_0_initProvided this) => gen
///                     (let [
///                         gen (Gen_2_dup gen)
///                         gen (Expr_3_emit (_0_init this), _0_Context_1_EXPRESSION, scope, gen)
///                         gen (Gen_2_invoke gen, Var_2_bindRoot, 2)
///                     ]
///                         (Gen_2_pop gen)
///                     )
///                 )
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm DefExpr Expr
///         (Expr_3_emit => DefExpr_2_emit)
///     )
// )

// (about #_"LetFnExpr"
///     (defr LetFnExpr)

///     (defn #_"LetFnExpr" LetFnExpr_1_new [#_"[LocalBinding]" bindings, #_"Expr" body]
///         (new_8_ LetFnExpr_1_class
///             (hash_map
///                 #_"[LocalBinding]" _0_bindings bindings
///                 #_"Expr" _0_body body
///             )
///         )
///     )

///     (defn #_"Expr" LetFnExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"vector?" bindings (second form)]
///             (when (vector_9_ bindings)           => (throw "bad binding form, expected vector")
///                 (when (even_9_ (count bindings)) => (throw "bad binding form, expected matched symbol expression pairs")
///                     (let [
///                         scope (update scope _0_1_local_env (comp atom deref))
///                         scope (update scope _0_1_local_num (comp atom deref))
///                         #_"[LocalBinding]" lbs
///                             (loop_when [lbs (vector) #_"ISeq" s (seq bindings)] (some_9_ s) => lbs
///                                 (let [#_"symbol?" sym (first s)]
///                                     (when (symbol_9_ sym)        => (throw (str "bad binding form, expected symbol, got: " sym))
///                                         (when (nil_9_ (_0_ns sym)) => (throw (str "can't let qualified name: " sym))
///                                             (let [
///                                                 #_"LocalBinding" lb (LocalBinding_1_new sym, nil, (swap_4_ (get scope _0_1_local_num) inc))
///                                             ]
///                                                 (swap_4_ (get scope _0_1_local_env) assoc (_0_sym lb) lb)
///                                                 (swap_4_ (_0_1_locals (get scope _0_fm)) assoc (_0_uid lb) lb)
///                                                 (recur (conj lbs lb) (next (next s)))
///                                             )
///                                         )
///                                     )
///                                 )
///                             )
///                         _
///                             (loop_when_recur [#_"int" i 0] (< i (count bindings)) [(+ i 2)]
///                                 (reset_4_ (_0_1_init (nth lbs (quot i 2))) (Compiler_1_analyze (nth bindings (inc i)), scope))
///                             )
///                     ]
///                         (LetFnExpr_1_new lbs, (BodyExpr_1_parse (next (next form)), context, scope))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" LetFnExpr_2_emit [#_"LetFnExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen
///                 (loop_when [gen gen #_"ISeq" s (seq (_0_bindings this))] (some_9_ s) => gen
///                     (let [
///                         #_"LocalBinding" lb (first s)
///                         gen (Gen_2_push gen, nil)
///                         gen (Gen_2_store gen, (_0_idx lb))
///                     ]
///                         (recur gen (next s))
///                     )
///                 )
///             [#_"{int}" lbset gen]
///                 (loop_when [lbset (hash_set) gen gen #_"ISeq" s (seq (_0_bindings this))] (some_9_ s) => [lbset gen]
///                     (let [
///                         #_"LocalBinding" lb (first s)
///                         gen (Expr_3_emit (deref (_0_1_init lb)), _0_Context_1_EXPRESSION, scope, gen)
///                         gen (Gen_2_store gen, (_0_idx lb))
///                     ]
///                         (recur (conj lbset (_0_uid lb)) gen (next s))
///                     )
///                 )
///             gen
///                 (loop_when [gen gen #_"ISeq" s (seq (_0_bindings this))] (some_9_ s) => gen
///                     (let [
///                         #_"LocalBinding" lb (first s)
///                         gen (Gen_2_load gen, (_0_idx lb))
///                         gen
///                             (loop_when [gen gen #_"ISeq" s (vals (deref (_0_1_closes (deref (_0_1_init lb)))))] (some_9_ s) => gen
///                                 (let [
///                                     gen
///                                         (let_when [#_"LocalBinding" lb (first s)] (contains_9_ lbset (_0_uid lb)) => gen
///                                             (let [
///                                                 gen (Gen_2_dup gen)
///                                                 gen (FnMethod_2_emitLocal (get scope _0_fm), gen, lb)
///                                                 gen (Gen_2_put gen, (_0_sym lb))
///                                             ]
///                                                 gen
///                                             )
///                                         )
///                                 ]
///                                     (recur gen (next s))
///                                 )
///                             )
///                         gen (Gen_2_pop gen)
///                     ]
///                         (recur gen (next s))
///                     )
///                 )
///         ]
///             (Expr_3_emit (_0_body this), context, scope, gen)
///         )
///     )

///     (defm LetFnExpr Expr
///         (Expr_3_emit => LetFnExpr_2_emit)
///     )
// )

// (about #_"LetExpr"
///     (defr LetExpr)

///     (defn #_"LetExpr" LetExpr_1_new [#_"[LocalBinding]" bindings, #_"Expr" body, #_"bool" loop_9_]
///         (new_8_ LetExpr_1_class
///             (hash_map
///                 #_"[LocalBinding]" _0_bindings bindings
///                 #_"Expr" _0_body body
///                 #_"bool" _0_loop_9_ loop_9_
///             )
///         )
///     )

///     (defn #_"Expr" LetExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"vector?" bindings (second form)]
///             (when (vector_9_ bindings)           => (throw "bad binding form, expected vector")
///                 (when (even_9_ (count bindings)) => (throw "bad binding form, expected matched symbol expression pairs")
///                     (let [
///                         scope (update scope _0_1_local_env (comp atom deref))
///                         scope (update scope _0_1_local_num (comp atom deref))
///                         #_"bool" loop_9_ (= (first form) _1_loop_8_)
///                         scope
///                             (when loop_9_ => scope
///                                 (dissoc scope _0_loop_locals)
///                             )
///                         #_"[LocalBinding]" lbs
///                             (loop_when [lbs (vector) #_"ISeq" s (seq bindings)] (some_9_ s) => lbs
///                                 (let [#_"symbol?" sym (first s)]
///                                     (when (symbol_9_ sym)        => (throw (str "bad binding form, expected symbol, got: " sym))
///                                         (when (nil_9_ (_0_ns sym)) => (throw (str "can't let qualified name: " sym))
///                                             (let [
///                                                 #_"Expr" init (Compiler_1_analyze (second s), scope)
///                                                 #_"LocalBinding" lb (LocalBinding_1_new sym, init, (swap_4_ (get scope _0_1_local_num) inc))
///                                             ]
///                                                 (swap_4_ (get scope _0_1_local_env) assoc (_0_sym lb) lb)
///                                                 (swap_4_ (_0_1_locals (get scope _0_fm)) assoc (_0_uid lb) lb)
///                                                 (recur (conj lbs lb) (next (next s)))
///                                             )
///                                         )
///                                     )
///                                 )
///                             )
///                         scope
///                             (when loop_9_ => scope
///                                 (assoc scope _0_loop_locals lbs)
///                             )
///                         #_"Expr" body (BodyExpr_1_parse (next (next form)), (if loop_9_ _0_Context_1_RETURN context), scope)
///                     ]
///                         (LetExpr_1_new lbs, body, loop_9_)
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" LetExpr_2_emit [#_"LetExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen
///                 (loop_when [gen gen #_"ISeq" s (seq (_0_bindings this))] (some_9_ s) => gen
///                     (let [
///                         #_"LocalBinding" lb (first s)
///                         gen (Expr_3_emit (deref (_0_1_init lb)), _0_Context_1_EXPRESSION, scope, gen)
///                         gen (Gen_2_store gen, (_0_idx lb))
///                     ]
///                         (recur gen (next s))
///                     )
///                 )
///             scope
///                 (when (_0_loop_9_ this) => scope
///                     (assoc scope _0_loop_label (Gen_2_mark gen))
///                 )
///         ]
///             (Expr_3_emit (_0_body this), context, scope, gen)
///         )
///     )

///     (defm LetExpr Expr
///         (Expr_3_emit => LetExpr_2_emit)
///     )
// )

// (about #_"RecurExpr"
///     (defr RecurExpr)

///     (defn #_"RecurExpr" RecurExpr_1_new [#_"vector" loopLocals, #_"vector" args]
///         (new_8_ RecurExpr_1_class
///             (hash_map
///                 #_"vector" _0_loopLocals loopLocals
///                 #_"vector" _0_args args
///             )
///         )
///     )

///     (defn #_"Expr" RecurExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (when (and (= context _0_Context_1_RETURN) (some_9_ (get scope _0_loop_locals))) => (throw "can only recur from tail position")
///             (let [#_"vector" args (mapv #(Compiler_1_analyze %, scope) (next form)) #_"int" n (count args) #_"int" m (count (get scope _0_loop_locals))]
///                 (when (= n m) => (throw (str "mismatched argument count to recur, expected: " m " args, got: " n))
///                     (RecurExpr_1_new (get scope _0_loop_locals), args)
///                 )
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" RecurExpr_2_emit [#_"RecurExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (when_some [#_"label" l_1_loop (get scope _0_loop_label)] => (throw "recur misses loop label")
///             (let [
///                 gen
///                     (loop_when_recur [gen gen #_"ISeq" s (seq (_0_args this))]
///                                      (some_9_ s)
///                                      [(Expr_3_emit (first s), _0_Context_1_EXPRESSION, scope, gen) (next s)]
///                                   => gen
///                     )
///                 gen
///                     (loop_when_recur [gen gen #_"ISeq" s (rseq (_0_loopLocals this))]
///                                      (some_9_ s)
///                                      [(Gen_2_store gen, (_0_idx (first s))) (next s)]
///                                   => gen
///                     )
///             ]
///                 (Gen_2_goto gen, l_1_loop)
///             )
///         )
///     )

///     (defm RecurExpr Expr
///         (Expr_3_emit => RecurExpr_2_emit)
///     )
// )

// (about #_"CaseExpr"
///     (defr CaseExpr)

///     (defn #_"CaseExpr" CaseExpr_1_new [#_"LocalBindingExpr" expr, #_"int" shift, #_"int" mask, #_"int" low, #_"int" high, #_"Expr" defaultExpr, #_"sorted {Integer Expr}" tests, #_"{Integer Expr}" thens, #_"Keyword" switchType, #_"Keyword" testType, #_"{Integer}" skipCheck]
///         (when_not (any = switchType _0_compact _0_sparse)
///             (throw (str "unexpected switch type: " switchType))
///         )
///         (when_not (any = testType _0_int _0_hash_equiv _0_hash_identity)
///             (throw (str "unexpected test type: " testType))
///         )
///         (new_8_ CaseExpr_1_class
///             (hash_map
///                 #_"LocalBindingExpr" _0_expr expr
///                 #_"int" _0_shift shift
///                 #_"int" _0_mask mask
///                 #_"int" _0_low low
///                 #_"int" _0_high high
///                 #_"Expr" _0_defaultExpr defaultExpr
///                 #_"sorted {Integer Expr}" _0_tests tests
///                 #_"{Integer Expr}" _0_thens thens
///                 #_"Keyword" _0_switchType switchType
///                 #_"Keyword" _0_testType testType
///                 #_"{Integer}" _0_skipCheck skipCheck
///             )
///         )
///     )

///     (defn #_"Expr" CaseExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [#_"vector" args (vec (next form))
///               #_"Object" exprForm (nth args 0)
///               #_"int" shift (int_4_ (nth args 1))
///               #_"int" mask (int_4_ (nth args 2))
///               #_"Object" defaultForm (nth args 3)
///               #_"IPersistentMap" caseMap (nth args 4)
///               #_"Keyword" switchType (nth args 5)
///               #_"Keyword" testType (nth args 6)
///               #_"IPersistentSet" skipCheck (when (< 7 (count args)) (nth args 7))
///               #_"ISeq" keys (keys caseMap)
///               #_"int" low (int_4_ (first keys))
///               #_"int" high (int_4_ (nth keys (dec (count keys))))
///               #_"LocalBindingExpr" testExpr (Compiler_1_analyze exprForm, scope)
///               [#_"sorted {Integer Expr}" tests #_"{Integer Expr}" thens]
///                 (loop_when [tests (sorted_map) thens (hash_map) #_"ISeq" s (seq caseMap)] (some_9_ s) => [tests thens]
///                     (let [#_"pair" e (first s)
///                           #_"Integer" minhash (int_4_ (key e)) #_"Object" pair (val e)
///                           #_"Expr" test (LiteralExpr_1_new (first pair))
///                           #_"Expr" then (Compiler_1_analyze (second pair), context, scope)]
///                         (recur (assoc tests minhash test) (assoc thens minhash then) (next s))
///                     )
///                 )
///               #_"Expr" defaultExpr (Compiler_1_analyze (nth args 3), context, scope)]
///             (CaseExpr_1_new testExpr, shift, mask, low, high, defaultExpr, tests, thens, switchType, testType, skipCheck)
///         )
///     )

///     (defn_ #_"IPersistentVector" CaseExpr_2_emitShiftMask [#_"CaseExpr" this, #_"IPersistentVector" gen]
///         (when_not (zero_9_ (_0_mask this)) => gen
///             (let [
///                 gen (Gen_2_push gen, (_0_shift this))
///                 gen (Gen_2_shr gen)
///                 gen (Gen_2_push gen, (_0_mask this))
///                 gen (Gen_2_and gen)
///             ]
///                 gen
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" CaseExpr_2_emitExpr [#_"CaseExpr" this, #_"IPersistentMap" scope, #_"IPersistentVector" gen, #_"label" l_1_default]
///         (let [
///             gen (Expr_3_emit (_0_expr this), _0_Context_1_EXPRESSION, scope, gen)
///             gen
///                 (when (= (_0_testType this) _0_int) => (Gen_2_invoke gen, f_1_hashcode, 1)
///                     (let [
///                         gen (Gen_2_number_9_ gen)
///                         gen (Gen_2_if_not gen, l_1_default)
///                         gen (Expr_3_emit (_0_expr this), _0_Context_1_EXPRESSION, scope, gen)
///                     ]
///                         (Gen_2_invoke gen, int_4_, 1)
///                     )
///                 )
///         ]
///             (CaseExpr_2_emitShiftMask this, gen)
///         )
///     )

///     (defn_ #_"IPersistentVector" CaseExpr_2_emitThen [#_"CaseExpr" this, #_"IPersistentMap" scope, #_"IPersistentVector" gen, #_"Expr" test, #_"Expr" then, #_"label" l_1_default]
///         (let [
///             gen (Expr_3_emit (_0_expr this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Expr_3_emit test, _0_Context_1_EXPRESSION, scope, gen)
///             gen (Gen_2_if_ne_9_ gen, l_1_default)
///         ]
///             (Expr_3_emit then, _0_Context_1_EXPRESSION, scope, gen)
///         )
///     )

///     (defn_ #_"IPersistentVector" CaseExpr_2_emit [#_"CaseExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             #_"label" l_1_default (Gen_2_label gen)
///             gen (CaseExpr_2_emitExpr this, scope, gen, l_1_default)
///             #_"sorted {Integer Label}" labels (reduce #__4_ #(assoc #__4_ %1 %2 (Gen_2_label gen)) (sorted_map) (keys (_0_tests this)))
///             gen
///                 (if (= (_0_switchType this) _0_sparse)
///                     (Gen_2_lookup_switch gen, (keys (_0_tests this)), (vals labels), l_1_default)
///                     (let [
///                         #_"labels" ls
///                             (for [#_"int" i (range (_0_low this) (inc (_0_high this)))]
///                                 (if (contains_9_ labels i) (get labels i) l_1_default)
///                             )
///                     ]
///                         (Gen_2_table_switch gen, (_0_low this), (_0_high this), ls, l_1_default)
///                     )
///                 )
///             #_"label" l_1_end (Gen_2_label gen)
///             gen
///                 (loop_when [gen gen #_"ISeq" s (keys labels)] (some_9_ s) => gen
///                     (let [
///                         #_"Integer" i (first s)
///                         gen (Gen_2_mark gen, (get labels i))
///                         gen
///                             (cond
///                                 (= (_0_testType this) _0_int)
///                                     (CaseExpr_2_emitThen this, scope, gen, (get (_0_tests this) i), (get (_0_thens this) i), l_1_default)
///                                 (contains_9_ (_0_skipCheck this) i)
///                                     (Expr_3_emit (get (_0_thens this) i), _0_Context_1_EXPRESSION, scope, gen)
///                                 _0_else
///                                     (CaseExpr_2_emitThen this, scope, gen, (get (_0_tests this) i), (get (_0_thens this) i), l_1_default)
///                             )
///                         gen (Gen_2_goto gen, l_1_end)
///                     ]
///                         (recur gen (next s))
///                     )
///                 )
///             gen (Gen_2_mark gen, l_1_default)
///             gen (Expr_3_emit (_0_defaultExpr this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Gen_2_mark gen, l_1_end)
///         ]
///             (when (= context _0_Context_1_STATEMENT) => gen
///                 (Gen_2_pop gen)
///             )
///         )
///     )

///     (defm CaseExpr Expr
///         (Expr_3_emit => CaseExpr_2_emit)
///     )
// )

// (about #_"MonitorExpr"
///     (defr MonitorExpr)

///     (defn #_"MonitorExpr" MonitorExpr_1_new [#_"Expr" target, #_"bool" enter_9_]
///         (new_8_ MonitorExpr_1_class
///             (hash_map
///                 #_"Expr" _0_target target
///                 #_"bool" _0_enter_9_ enter_9_
///             )
///         )
///     )

///     (defn #_"Expr" MonitorExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (MonitorExpr_1_new (Compiler_1_analyze (second form), scope), (= (first form) _1_monitor_enter))
///     )

///     (defn_ #_"IPersistentVector" MonitorExpr_2_emit [#_"MonitorExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen (Expr_3_emit (_0_target this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (if (_0_enter_9_ this) (Gen_2_monitor_enter gen) (Gen_2_monitor_exit gen))
///             gen (Expr_3_emit LiteralExpr_1_NIL, context, scope, gen)
///         ]
///             gen
///         )
///     )

///     (defm MonitorExpr Expr
///         (Expr_3_emit => MonitorExpr_2_emit)
///     )
// )

// (about #_"CatchClause"
///     (defr CatchClause)

///     (defn #_"CatchClause" CatchClause_1_new [#_"LocalBinding" lb, #_"Expr" handler]
///         (new_8_ CatchClause_1_class
///             (hash_map
///                 #_"LocalBinding" _0_lb lb
///                 #_"Expr" _0_handler handler
///             )
///         )
///     )
// )

// (about #_"TryExpr"
///     (defr TryExpr)

///     (defn #_"TryExpr" TryExpr_1_new [#_"Expr" tryExpr, #_"[CatchClause]" catches, #_"Expr" finallyExpr, #_"IPersistentMap" scope]
///         (new_8_ TryExpr_1_class
///             (hash_map
///                 #_"Expr" _0_tryExpr tryExpr
///                 #_"[CatchClause]" _0_catches catches
///                 #_"Expr" _0_finallyExpr finallyExpr

///                 #_"int" _0_retLocal (swap_4_ (get scope _0_1_local_num) inc)
///                 #_"int" _0_finallyLocal (swap_4_ (get scope _0_1_local_num) inc)
///             )
///         )
///     )

///     (defn #_"Expr" TryExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let [
///             scope (dissoc scope _0_loop_locals)
///             [#_"Expr" bodyExpr #_"[CatchClause]" catches #_"Expr" finallyExpr #_"vector" body]
///                 (loop_when [bodyExpr nil catches (vector) finallyExpr nil body (vector) #_"bool" caught_9_ false #_"ISeq" fs (next form)] (some_9_ fs) => [bodyExpr catches finallyExpr body]
///                     (let [#_"Object" f (first fs) #_"Object" op (when (seq_9_ f) (first f))]
///                         (if (any = op _1_catch _1_finally)
///                             (let [bodyExpr (or bodyExpr (BodyExpr_1_parse (seq body), context, scope))]
///                                 (if (= op _1_catch)
///                                     (let [#_"symbol?" sym (third f)]
///                                         (when (symbol_9_ sym)        => (throw (str "bad binding form, expected symbol, got: " sym))
///                                             (when (nil_9_ (_0_ns sym)) => (throw (str "can't bind qualified name: " sym))
///                                                 (let [
///                                                     scope (update scope _0_1_local_env (comp atom deref))
///                                                     scope (update scope _0_1_local_num (comp atom deref))
///                                                     #_"LocalBinding" lb (LocalBinding_1_new sym, nil, (swap_4_ (get scope _0_1_local_num) inc))
///                                                     _ (swap_4_ (get scope _0_1_local_env) assoc (_0_sym lb) lb)
///                                                     _ (swap_4_ (_0_1_locals (get scope _0_fm)) assoc (_0_uid lb) lb)
///                                                     #_"Expr" handler (BodyExpr_1_parse (next (next (next f))), _0_Context_1_EXPRESSION, scope)
///                                                     #_"CatchClause" clause (CatchClause_1_new lb, handler)
///                                                 ]
///                                                     (recur bodyExpr (conj catches clause) finallyExpr body true (next fs))
///                                                 )
///                                             )
///                                         )
///                                     )
///                                     (when (nil_9_ (next fs)) => (throw "finally clause must be last in try expression")
///                                         (let [finallyExpr (BodyExpr_1_parse (next f), _0_Context_1_STATEMENT, scope)]
///                                             (recur bodyExpr catches finallyExpr body caught_9_ (next fs))
///                                         )
///                                     )
///                                 )
///                             )
///                             (when_not caught_9_ => (throw "only catch or finally clause can follow catch in try expression")
///                                 (recur bodyExpr catches finallyExpr (conj body f) caught_9_ (next fs))
///                             )
///                         )
///                     )
///                 )
///         ]
///             (when (nil_9_ bodyExpr) => (TryExpr_1_new bodyExpr, catches, finallyExpr, scope)
///                 (BodyExpr_1_parse (seq body), context, scope)
///             )
///         )
///     )

///     (defn_ #_"IPersistentVector" TryExpr_2_emit [#_"TryExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             #_"label" l_1_start (Gen_2_mark gen)
///             gen (Expr_3_emit (_0_tryExpr this), context, scope, gen)
///             gen
///                 (when_not (= context _0_Context_1_STATEMENT) => gen
///                     (Gen_2_store gen, (_0_retLocal this))
///                 )
///             #_"label" l_1_end (Gen_2_mark gen)
///             gen
///                 (when (some_9_ (_0_finallyExpr this)) => gen
///                     (Expr_3_emit (_0_finallyExpr this), _0_Context_1_STATEMENT, scope, gen)
///                 )
///             #_"label" l_1_return (Gen_2_label gen)
///             gen (Gen_2_goto gen, l_1_return)
///             #_"int" n (count (_0_catches this)) #_"labels" l_1_starts (mapv Gen_2_label (repeat n gen)) #_"labels" l_1_ends (mapv Gen_2_label (repeat n gen))
///             gen
///                 (loop_when [gen gen #_"int" i 0] (< i n) => gen
///                     (let [
///                         #_"CatchClause" clause (nth (_0_catches this) i)
///                         gen (Gen_2_mark gen, (nth l_1_starts i))
///                         gen (Gen_2_store gen, (_0_idx (_0_lb clause)))
///                         gen (Expr_3_emit (_0_handler clause), context, scope, gen)
///                         gen
///                             (when_not (= context _0_Context_1_STATEMENT) => gen
///                                 (Gen_2_store gen, (_0_retLocal this))
///                             )
///                         gen (Gen_2_mark gen, (nth l_1_ends i))
///                         gen
///                             (when (some_9_ (_0_finallyExpr this)) => gen
///                                 (Expr_3_emit (_0_finallyExpr this), _0_Context_1_STATEMENT, scope, gen)
///                             )
///                         gen (Gen_2_goto gen, l_1_return)
///                     ]
///                         (recur gen (inc i))
///                     )
///                 )
///             #_"label" l_1_finally (Gen_2_label gen)
///             gen
///                 (when (some_9_ (_0_finallyExpr this)) => gen
///                     (let [
///                         gen (Gen_2_mark gen, l_1_finally)
///                         gen (Gen_2_store gen, (_0_finallyLocal this))
///                         gen (Expr_3_emit (_0_finallyExpr this), _0_Context_1_STATEMENT, scope, gen)
///                         gen (Gen_2_load gen, (_0_finallyLocal this))
///                         gen (Gen_2_throw gen)
///                     ]
///                         gen
///                     )
///                 )
///             gen (Gen_2_mark gen, l_1_return)
///             gen
///                 (when_not (= context _0_Context_1_STATEMENT) => gen
///                     (Gen_2_load gen, (_0_retLocal this))
///                 )
///             gen (loop_when_recur [gen gen #_"int" i 0] (< i n) [(Gen_2_try_catch_finally gen, l_1_start, l_1_end, (nth l_1_starts i)) (inc i)] => gen)
///         ]
///             (when (some_9_ (_0_finallyExpr this)) => gen
///                 (let [
///                     gen (Gen_2_try_catch_finally gen, l_1_start, l_1_end, l_1_finally)
///                 ]
///                     (loop_when_recur [gen gen #_"int" i 0] (< i n) [(Gen_2_try_catch_finally gen, (nth l_1_starts i), (nth l_1_ends i), l_1_finally) (inc i)] => gen)
///                 )
///             )
///         )
///     )

///     (defm TryExpr Expr
///         (Expr_3_emit => TryExpr_2_emit)
///     )
// )

// (about #_"ThrowExpr"
///     (defr ThrowExpr)

///     (defn #_"ThrowExpr" ThrowExpr_1_new [#_"Expr" throwable]
///         (new_8_ ThrowExpr_1_class
///             (hash_map
///                 #_"Expr" _0_throwable throwable
///             )
///         )
///     )

///     (defn #_"Expr" ThrowExpr_1_parse [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (cond
///             (= (count form) 1) (throw "too few arguments to throw: single Throwable expected")
///             (< 2 (count form)) (throw "too many arguments to throw: single Throwable expected")
///             _0_else              (ThrowExpr_1_new (Compiler_1_analyze (second form), scope))
///         )
///     )

///     (defn_ #_"IPersistentVector" ThrowExpr_2_emit [#_"ThrowExpr" this, #_"Context" context, #_"IPersistentMap" scope, #_"IPersistentVector" gen]
///         (let [
///             gen (Expr_3_emit (_0_throwable this), _0_Context_1_EXPRESSION, scope, gen)
///             gen (Gen_2_throw gen)
///         ]
///             gen
///         )
///     )

///     (defm ThrowExpr Expr
///         (Expr_3_emit => ThrowExpr_2_emit)
///     )
// )

// (about #_"Compiler"
///     (def #_"IPersistentMap" Compiler_1_specials
///         (hash_map
///             '&             nil
///             _1_case_8_         CaseExpr_1_parse
///             _1_catch         nil
///             _1_def           DefExpr_1_parse
///             _1_do            BodyExpr_1_parse
///             _1_finally       nil
///             _1_fn_8_           FnExpr_1_parse
///             _1_if            IfExpr_1_parse
///             _1_let_8_          LetExpr_1_parse
///             _1_letfn_8_        LetFnExpr_1_parse
///             _1_loop_8_         LetExpr_1_parse
///             _1_monitor_enter MonitorExpr_1_parse
///             _1_monitor_exit  MonitorExpr_1_parse
///             _1_quote         LiteralExpr_1_parse
///             _1_recur         RecurExpr_1_parse
///             _1_throw         ThrowExpr_1_parse
///             _1_try           TryExpr_1_parse
///             _1_var           TheVarExpr_1_parse
///         )
///     )

///     (defn #_"bool" Compiler_1_isSpecial [#_"Object" sym]
///         (contains_9_ Compiler_1_specials sym)
///     )

/// (defn special_symbol_9_ [s] (Compiler_1_isSpecial s))

///     (defn #_"edn" Compiler_1_macroexpand1
///         ([#_"edn" form] (Compiler_1_macroexpand1 form, nil))
///         ([#_"edn" form, #_"IPersistentMap" scope]
///             (when (seq_9_ form) => form
///                 (let_when [#_"Object" op (first form)] (not (Compiler_1_isSpecial op)) => form
///                     (let_when [#_"Var" v (Compiler_1_maybeMacro op, scope)] (some_9_ v) => form
///                         (apply v form (deref (get scope _0_1_local_env)) (next form))
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"edn" Compiler_1_macroexpand [#_"edn" form, #_"IPersistentMap" scope]
///         (let_when [#_"edn" f (Compiler_1_macroexpand1 form, scope)] (identical_9_ f form) => (recur f, scope)
///             form
///         )
///     )

///     (defn_ #_"void" Compiler_1_closeOver [#_"LocalBinding" lb, #_"FnMethod" fm]
///         (when (and (some_9_ lb) (some_9_ fm) (not (contains_9_ (deref (_0_1_locals fm)) (_0_uid lb))))
///             (swap_4_ (_0_1_closes (_0_fun fm)) assoc (_0_uid lb) lb)
///             (Compiler_1_closeOver lb, (_0_parent fm))
///         )
///         nil
///     )

///     (defn_ #_"Expr" Compiler_1_analyzeSymbol [#_"Symbol" sym, #_"IPersistentMap" scope]
///         (or
///             (when (nil_9_ (_0_ns sym))
///                 (when_some [#_"LocalBinding" lb (get (deref (get scope _0_1_local_env)) sym)]
///                     (Compiler_1_closeOver lb, (get scope _0_fm))
///                     (LocalBindingExpr_1_new lb)
///                 )
///             )
///             (let [#_"Object" o (Compiler_1_resolve sym)]
///                 (cond
///                     (var_9_ o)
///                         (when (nil_9_ (Compiler_1_maybeMacro o, scope)) => (throw (str "can't take value of a macro: " o))
///                             (VarExpr_1_new o)
///                         )
///                     (symbol_9_ o)
///                         (UnresolvedVarExpr_1_new o)
///                     _0_else
///                         (throw (str "unable to resolve symbol: " sym " in this context"))
///                 )
///             )
///         )
///     )

///     (defn_ #_"Expr" Compiler_1_analyzeSeq [#_"ISeq" form, #_"Context" context, #_"IPersistentMap" scope]
///         (let_when [#_"Object" me (Compiler_1_macroexpand1 form, scope)] (= me form) => (Compiler_1_analyze me, context, scope)
///             (when_some [#_"Object" op (first form)] => (throw (str "can't call nil, form: " form))
///                 (let [#_"IFn" inline (Compiler_1_maybeInline op, (count (next form)), scope)]
///                     (if (some_9_ inline)
///                         (Compiler_1_analyze (IFn_3_applyTo inline, (next form)), context, scope)
///                         (let [#_"IFn" f_1_parse (or (get Compiler_1_specials op) InvokeExpr_1_parse)]
///                             (f_1_parse form, context, scope)
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Expr" Compiler_1_analyze
///         ([#_"edn" form, #_"IPersistentMap" scope] (Compiler_1_analyze form, _0_Context_1_EXPRESSION, scope))
///         ([#_"edn" form, #_"Context" context, #_"IPersistentMap" scope]
///             (let [form
///                     (when (satisfies_9_ LazySeq form) => form
///                         (with_meta (or (seq form) (list)) (meta form))
///                     )]
///                 (case_4_ form
///                     nil                                  LiteralExpr_1_NIL
///                     true                                 LiteralExpr_1_TRUE
///                     false                                LiteralExpr_1_FALSE
///                     (cond
///                         (symbol_9_ form)                   (Compiler_1_analyzeSymbol form, scope)
///                         (string_9_ form)                   (LiteralExpr_1_new (String_2_intern form))
///                         (and (coll_9_ form) (empty_9_ form)) (LiteralExpr_1_new form)
///                         (seq_9_ form)                      (Compiler_1_analyzeSeq form, context, scope)
///                         (vector_9_ form)                   (VectorExpr_1_parse form, scope)
///                         (map_9_ form)                      (MapExpr_1_parse form, scope)
///                         (set_9_ form)                      (SetExpr_1_parse form, scope)
///                         _0_else                            (LiteralExpr_1_new form)
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"edn" Compiler_1_eval
///         ([#_"edn" form] (Compiler_1_eval form, nil))
///         ([#_"edn" form, #_"IPersistentMap" scope]
///             (let [form (Compiler_1_macroexpand form, scope)]
///                 (-> (list _1_fn_8_ [] form)
///                     (Compiler_1_analyze scope)
///                     (Closure_1_new nil)
///                     (IFn_3_invoke)
///                 )
///             )
///         )
///     )
// )
}

namespace arbace {

// (about #_"LispReader"
///     (defn #_"Symbol" LispReader_1_garg [#_"int" n]
///         (symbol (str (if (= n -1) "args" (str "arg" n)) "__" (next_id_4_) "#"))
///     )

///     (defn #_"Symbol" LispReader_1_registerArg [#_"IPersistentMap" scope, #_"int" n]
///         (when (contains_9_ scope _0_1_arg_env) => (throw "arg literal not in #()")
///             (or (get (deref (get scope _0_1_arg_env)) n)
///                 (let [#_"Symbol" sym (LispReader_1_garg n)]
///                     (swap_4_ (get scope _0_1_arg_env) assoc n sym)
///                     sym
///                 )
///             )
///         )
///     )

///     (defn #_"Symbol" LispReader_1_registerGensym [#_"IPersistentMap" scope, #_"Symbol" sym]
///         (when (contains_9_ scope _0_1_gensym_env) => (throw "gensym literal not in syntax_quote")
///             (or (get (deref (get scope _0_1_gensym_env)) sym)
///                 (let [#_"Symbol" gsym (symbol (str (_0_name sym) "__" (next_id_4_) "__auto__"))]
///                     (swap_4_ (get scope _0_1_gensym_env) assoc sym gsym)
///                     gsym
///                 )
///             )
///         )
///     )

///     (declare LispReader_1_macros)

///     (defn_ #_"bool" LispReader_1_isMacro [#_"char" ch]
///         (contains_9_ LispReader_1_macros ch)
///     )

///     (defn_ #_"bool" LispReader_1_isTerminatingMacro [#_"char" ch]
///         (and (LispReader_1_isMacro ch) (not (any = ch \# \' \%)))
///     )

///     (defn #_"bool" LispReader_1_isDigit [#_"char" ch, #_"int" base]
///         (not= (Character_1_digit ch, base) -1)
///     )

///     (defn #_"bool" LispReader_1_isWhitespace [#_"char" ch]
///         (or (Character_1_isWhitespace ch) (= ch \,))
///     )

///     (defn #_"Character" LispReader_1_read1 [#_"Reader" r]
///         (let [#_"int" c (Reader_2_read r)]
///             (when_not (= c -1)
///                 (char c)
///             )
///         )
///     )

///     (defn #_"void" LispReader_1_unread [#_"PushbackReader" r, #_"Character" ch]
///         (when (some_9_ ch)
///             (PushbackReader_2_unread r, (int ch))
///         )
///         nil
///     )

///     (defn_ #_"void" LispReader_1_consumeWhitespaces [#_"PushbackReader" r]
///         (loop_when_recur [#_"char" ch (LispReader_1_read1 r)] (and (some_9_ ch) (LispReader_1_isWhitespace ch)) [(LispReader_1_read1 r)] => (LispReader_1_unread r, ch))
///         nil
///     )

///     (def_ #_"Pattern" LispReader_1_rxInteger #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)")
///     (def_ #_"Pattern" LispReader_1_rxRatio   #"([-+]?[0-9]+)/([0-9]+)")

///     (defn_ #_"Object" LispReader_1_matchNumber [#_"String" s]
///         (let [_ (or
///                     (let_when [#_"Matcher" m (Pattern_2_matcher LispReader_1_rxInteger, s)] (Matcher_2_matches m)
///                         (when (nil_9_ (Matcher_2_group m, 2)) => (Long_1_valueOf 0)
///                             (let [[#_"String" n #_"int" radix]
///                                     (cond_some
///                                         [n (Matcher_2_group m, 3)] [n 10]
///                                         [n (Matcher_2_group m, 4)] [n 16]
///                                         [n (Matcher_2_group m, 5)] [n 8]
///                                         [n (Matcher_2_group m, 7)] [n (Integer_1_parseInt (Matcher_2_group m, 6))]
///                                     )]
///                                 (when (some_9_ n) => _0_nil
///                                     (let [#_"BigInteger" bn (BigInteger_1_new n, radix) bn (if (= (Matcher_2_group m, 1) "-") (BigInteger_2_negate bn) bn)]
///                                         (when (< (BigInteger_2_bitLength bn) 64) => bn
///                                             (Long_1_valueOf (BigInteger_2_longValue bn))
///                                         )
///                                     )
///                                 )
///                             )
///                         )
///                     )
///                     (let_when [#_"Matcher" m (Pattern_2_matcher LispReader_1_rxRatio, s)] (Matcher_2_matches m)
///                         (let [#_"String" n (Matcher_2_group m, 1) n (if (String_2_startsWith n, "+") (String_2_substring n, 1) n)]
///                             (Numbers_1_divide (BigInteger_1_new n), (BigInteger_1_new (Matcher_2_group m, 2)))
///                         )
///                     )
///                 )]
///             (when_not (= _ _0_nil) _)
///         )
///     )

///     (defn_ #_"Object" LispReader_1_readNumber [#_"PushbackReader" r, #_"char" ch]
///         (let [#_"String" s
///                 (let [#_"StringBuilder" sb (StringBuilder_1_new) _ (StringBuilder_2_append sb, ch)]
///                     (loop []
///                         (let [ch (LispReader_1_read1 r)]
///                             (if (or (nil_9_ ch) (LispReader_1_isWhitespace ch) (LispReader_1_isMacro ch))
///                                 (do
///                                     (LispReader_1_unread r, ch)
///                                     (StringBuilder_2_toString sb)
///                                 )
///                                 (do
///                                     (StringBuilder_2_append sb, ch)
///                                     (recur)
///                                 )
///                             )
///                         )
///                     )
///                 )]
///             (or (LispReader_1_matchNumber s) (throw (str "invalid number: " s)))
///         )
///     )

///     (defn_ #_"String" LispReader_1_readToken [#_"PushbackReader" r, #_"char" ch]
///         (let [#_"StringBuilder" sb (StringBuilder_1_new) _ (StringBuilder_2_append sb, ch)]
///             (loop []
///                 (let [ch (LispReader_1_read1 r)]
///                     (if (or (nil_9_ ch) (LispReader_1_isWhitespace ch) (LispReader_1_isTerminatingMacro ch))
///                         (do
///                             (LispReader_1_unread r, ch)
///                             (StringBuilder_2_toString sb)
///                         )
///                         (do
///                             (StringBuilder_2_append sb, ch)
///                             (recur)
///                         )
///                     )
///                 )
///             )
///         )
///     )

///     (def_ #_"Pattern" LispReader_1_rxSymbol #"[:]?([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)")

///     (defn_ #_"Object" LispReader_1_matchSymbol [#_"String" s]
///         (let_when [#_"Matcher" m (Pattern_2_matcher LispReader_1_rxSymbol, s)] (Matcher_2_matches m)
///             (let [#_"String" ns (Matcher_2_group m, 1) #_"String" n (Matcher_2_group m, 2)]
///                 (cond
///                     (or (and (some_9_ ns) (String_2_endsWith ns, ":/")) (String_2_endsWith n, ":") (not= (String_2_indexOf s, "::", 1) -1))
///                         nil
///                     (String_2_startsWith s, "::")
///                         (let [#_"Symbol" ks (symbol (String_2_substring s, 2))
///                               #_"Namespace" kns (if (some_9_ (_0_ns ks)) (Namespace_2_getAlias _8_ns_8_, (symbol (_0_ns ks))) _8_ns_8_)]
///                             (when (some_9_ kns)
///                                 (keyword (_0_name (_0_name kns)) (_0_name ks))
///                             )
///                         )
///                     _0_else
///                         (let [#_"bool" kw_9_ (= (String_2_charAt s, 0) \:) #_"Symbol" sym (symbol (String_2_substring s, (if kw_9_ 1 0)))]
///                             (if kw_9_ (keyword sym) sym)
///                         )
///                 )
///             )
///         )
///     )

///     (defn_ #_"Object" LispReader_1_interpretToken [#_"String" s]
///         (case_4_ s "nil" nil "true" true "false" false
///             (or (LispReader_1_matchSymbol s) (throw (str "invalid token: " s)))
///         )
///     )

///     (defn #_"Object" LispReader_1_read
///         ([#_"PushbackReader" r, #_"IPersistentMap" scope] (LispReader_1_read r, scope, true, nil))
///         ([#_"PushbackReader" r, #_"IPersistentMap" scope, #_"bool" eofIsError, #_"Object" eofValue] (LispReader_1_read r, scope, eofIsError, eofValue, nil, nil))
///         ([#_"PushbackReader" r, #_"IPersistentMap" scope, #_"bool" eofIsError, #_"Object" eofValue, #_"Character" returnOn, #_"Object" returnOnValue]
///             (loop []
///                 (let [#_"char" ch (loop_when_recur [ch (LispReader_1_read1 r)] (and (some_9_ ch) (LispReader_1_isWhitespace ch)) [(LispReader_1_read1 r)] => ch)]
///                     (cond
///                         (nil_9_ ch)
///                             (if eofIsError (throw "EOF while reading") eofValue)
///                         (and (some_9_ returnOn) (= returnOn ch))
///                             returnOnValue
///                         (LispReader_1_isDigit ch, 10)
///                             (LispReader_1_readNumber r, ch)
///                         _0_else
///                             (let [#_"IFn" f_1_macro (get LispReader_1_macros ch)]
///                                 (if (some_9_ f_1_macro)
///                                     (let [#_"Object" o (f_1_macro r scope ch)]
///                                         (recur_when (identical_9_ o r) [] => o)
///                                     )
///                                     (or
///                                         (when (any = ch \+ \-)
///                                             (let [#_"char" ch_1_ (LispReader_1_read1 r) _ (LispReader_1_unread r, ch_1_)]
///                                                 (when (and (some_9_ ch_1_) (LispReader_1_isDigit ch_1_, 10))
///                                                     (LispReader_1_readNumber r, ch)
///                                                 )
///                                             )
///                                         )
///                                         (LispReader_1_interpretToken (LispReader_1_readToken r, ch))
///                                     )
///                                 )
///                             )
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"int" LispReader_1_scanDigits [#_"String" token, #_"int" offset, #_"int" n, #_"int" base]
///         (when (= (+ offset n) (String_2_length token)) => (throw (str "invalid unicode character: \\" token))
///             (loop_when [#_"int" c 0 #_"int" i 0] (< i n) => c
///                 (let [#_"char" ch (String_2_charAt token, (+ offset i)) #_"int" d (Character_1_digit ch, base)]
///                     (when_not (= d -1) => (throw (str "invalid digit: " ch))
///                         (recur (+ (* c base) d) (inc i))
///                     )
///                 )
///             )
///         )
///     )

///     (defn_ #_"int" LispReader_1_readDigits [#_"PushbackReader" r, #_"char" ch, #_"int" base, #_"int" n, #_"bool" exact_9_]
///         (let_when_not [#_"int" c (Character_1_digit ch, base)] (= c -1) => (throw (str "invalid digit: " ch))
///             (let [[c #_"int" i]
///                     (loop_when [c c i 1] (< i n) => [c i]
///                         (let [ch (LispReader_1_read1 r)]
///                             (if (or (nil_9_ ch) (LispReader_1_isWhitespace ch) (LispReader_1_isMacro ch))
///                                 (do
///                                     (LispReader_1_unread r, ch)
///                                     [c i]
///                                 )
///                                 (let [#_"int" d (Character_1_digit ch, base)]
///                                     (when_not (= d -1) => (throw (str "invalid digit: " ch))
///                                         (recur (+ (* c base) d) (inc i))
///                                     )
///                                 )
///                             )
///                         )
///                     )]
///                 (when (or (= i n) (not exact_9_)) => (throw (str "invalid character length: " i ", should be: " n))
///                     c
///                 )
///             )
///         )
///     )

///     (def_ #_"Object" LispReader_1_READ_EOF (anew 0))
///     (def_ #_"Object" LispReader_1_READ_FINISHED (anew 0))

///     (defn #_"vector" LispReader_1_readDelimitedForms [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" delim]
///         (loop [#_"vector" v (vector)]
///             (let [#_"Object" form (LispReader_1_read r, scope, false, LispReader_1_READ_EOF, delim, LispReader_1_READ_FINISHED)]
///                 (condp identical_9_ form
///                     LispReader_1_READ_EOF
///                         (throw "EOF while reading")
///                     LispReader_1_READ_FINISHED
///                         v
///                     (recur (conj v form))
///                 )
///             )
///         )
///     )
// )

// (about #_"RegexReader"
///     (defn #_"Pattern" regex_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (let [#_"StringBuilder" sb (StringBuilder_1_new)]
///             (loop []
///                 (when_some [#_"char" ch (LispReader_1_read1 r)] => (throw "EOF while reading regex")
///                     (when_not (= ch \")
///                         (StringBuilder_2_append sb, ch)
///                         (when (= ch \\)
///                             (when_some [ch (LispReader_1_read1 r)] => (throw "EOF while reading regex")
///                                 (StringBuilder_2_append sb, ch)
///                             )
///                         )
///                         (recur)
///                     )
///                 )
///             )
///             (Pattern_1_compile (StringBuilder_2_toString sb))
///         )
///     )
// )

// (about #_"StringReader"
///     (defn_ #_"char" StringReader_1_escape [#_"PushbackReader" r]
///         (when_some [#_"char" ch (LispReader_1_read1 r)] => (throw "EOF while reading string")
///             (case_4_ ch
///                 \t  \tab
///                 \r  \return
///                 \n  \newline
///                 \\  ch
///                 \"  ch
///                 \b  \backspace
///                 \f  \formfeed
///                 \u  (let [ch (LispReader_1_read1 r)]
///                         (when (and (some_9_ ch) (LispReader_1_isDigit ch, 16)) => (throw (str "invalid unicode escape: \\u" ch))
///                             (char (LispReader_1_readDigits r, ch, 16, 4, true))
///                         )
///                     )
///                 (when (LispReader_1_isDigit ch, #_8 4) => (throw (str "unsupported escape character: \\" ch))
///                     (let [#_"int" c (LispReader_1_readDigits r, ch, 8, 3, false)]
///                       #_(when (< 0377 c)
///                             (throw "octal escape sequence must be in range [0, 377]")
///                         )
///                         (char c)
///                     )
///                 )
///             )
///         )
///     )

///     (defn #_"Object" string_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (let [#_"StringBuilder" sb (StringBuilder_1_new)]
///             (loop []
///                 (when_some [#_"char" ch (LispReader_1_read1 r)] => (throw "EOF while reading string")
///                     (when_not (= ch \")
///                         (StringBuilder_2_append sb, (if (= ch \\) (StringReader_1_escape r) ch))
///                         (recur)
///                     )
///                 )
///             )
///             (StringBuilder_2_toString sb)
///         )
///     )
// )

// (about #_"CommentReader"
///     (defn #_"Object" comment_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (while (not (any = (LispReader_1_read1 r) nil \newline \return)))
///         r
///     )
// )

// (about #_"DiscardReader"
///     (defn #_"Object" discard_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (LispReader_1_read r, scope)
///         r
///     )
// )

// (about #_"QuoteReader"
///     (defn #_"Object" quote_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (list _1_quote (LispReader_1_read r, scope))
///     )
// )

// (about #_"DerefReader"
///     (defn #_"Object" deref_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (list _7_deref (LispReader_1_read r, scope))
///     )
// )

// (about #_"VarReader"
///     (defn #_"Object" var_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (list _1_var (LispReader_1_read r, scope))
///     )
// )

// (about #_"DispatchReader"
///     (declare LispReader_1_dispatchMacros)

///     (defn #_"Object" dispatch_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (when_some [#_"char" ch (LispReader_1_read1 r)] => (throw "EOF while reading character")
///             (let_when [#_"IFn" f_1_macro (get LispReader_1_dispatchMacros ch)] (nil_9_ f_1_macro) => (f_1_macro r scope ch)
///                 (LispReader_1_unread r, ch)
///                 (throw (str "no dispatch macro for: " ch))
///             )
///         )
///     )
// )

// (about #_"FnReader"
///     (defn #_"Object" fn_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (when_not (contains_9_ scope _0_1_arg_env) => (throw "nested #()s are not allowed")
///             (let [scope (assoc scope _0_1_arg_env (atom (sorted_map)))]
///                 (LispReader_1_unread r, \()
///                 (let [
///                     #_"Object" form (LispReader_1_read r, scope)
///                     #_"vector" args (vector)
///                     args
///                         (when_some [#_"ISeq" rs (rseq (deref (get scope _0_1_arg_env)))] => args
///                             (let [args
///                                     (let_when [#_"int" n (key (first rs))] (pos_9_ n) => args
///                                         (loop_when_recur [args args #_"int" i 1]
///                                                          (<= i n)
///                                                          [(conj args (or (get (deref (get scope _0_1_arg_env)) i) (LispReader_1_garg i))) (inc i)]
///                                                       => args
///                                         )
///                                     )]
///                                 (when_some [#_"Object" rest (get (deref (get scope _0_1_arg_env)) -1)] => args
///                                     (conj args '& rest)
///                                 )
///                             )
///                         )
///                 ]
///                     (list _1_fn_8_ args form)
///                 )
///             )
///         )
///     )
// )

// (about #_"ArgReader"
///     (defn #_"Object" arg_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (when (contains_9_ scope _0_1_arg_env) => (LispReader_1_interpretToken (LispReader_1_readToken r, \%))
///             (let [#_"char" ch (LispReader_1_read1 r) _ (LispReader_1_unread r, ch)]
///                 (if (or (nil_9_ ch) (LispReader_1_isWhitespace ch) (LispReader_1_isTerminatingMacro ch))
///                     (LispReader_1_registerArg scope, 1)
///                     (let [#_"Object" n (LispReader_1_read r, scope)]
///                         (cond
///                             (= n '&)    (LispReader_1_registerArg scope, -1)
///                             (number_9_ n) (LispReader_1_registerArg scope, (int_4_ n))
///                             _0_else       (throw "arg literal must be %, %& or %integer")
///                         )
///                     )
///                 )
///             )
///         )
///     )
// )

// (about #_"MetaReader"
///     (defn #_"Object" meta_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (let [#_"Object" _meta (LispReader_1_read r, scope)
///               _meta
///                 (cond
///                     (keyword_9_ _meta) {_meta true}
///                     (map_9_ _meta)      _meta
///                     _0_else (throw "metadata must be Keyword or Map")
///                 )
///               #_"Object" o (LispReader_1_read r, scope)]
///             (when (satisfies_9_ IMeta o) => (throw "metadata can only be applied to IMetas")
///                 (if (satisfies_9_ IReference o)
///                     (do
///                         (reset_meta_4_ o _meta)
///                         o
///                     )
///                     (let [#_"IPersistentMap" m
///                             (loop_when [m (meta o) #_"ISeq" s (seq _meta)] (some_9_ s) => m
///                                 (let [#_"pair" e (first s)]
///                                     (recur (assoc m (key e) (val e)) (next s))
///                                 )
///                             )]
///                         (with_meta o m)
///                     )
///                 )
///             )
///         )
///     )
// )

// (about #_"SyntaxQuoteReader"
/// (def unquote)

///     (defn #_"bool" SyntaxQuoteReader_1_isUnquote [#_"Object" form]
///         (and (seq_9_ form) (= (first form) _7_unquote))
///     )

/// (def unquote_splicing)

///     (defn #_"bool" SyntaxQuoteReader_1_isUnquoteSplicing [#_"Object" form]
///         (and (seq_9_ form) (= (first form) _7_unquote_splicing))
///     )

///     (declare SyntaxQuoteReader_1_syntaxQuote)

///     (defn_ #_"ISeq" SyntaxQuoteReader_1_sqExpandList [#_"IPersistentMap" scope, #_"ISeq" s]
///         (loop_when [#_"vector" v (vector) s s] (some_9_ s) => (seq v)
///             (let [#_"Object" item (first s)
///                   v (cond
///                         (SyntaxQuoteReader_1_isUnquote item)         (conj v (list _7_list (second item)))
///                         (SyntaxQuoteReader_1_isUnquoteSplicing item) (conj v (second item))
///                         _0_else                                      (conj v (list _7_list (SyntaxQuoteReader_1_syntaxQuote scope, item)))
///                     )]
///                 (recur v (next s))
///             )
///         )
///     )

///     (defn #_"Object" SyntaxQuoteReader_1_syntaxQuote [#_"IPersistentMap" scope, #_"Object" form]
///         (let [#_"Object" q
///                 (cond
///                     (Compiler_1_isSpecial form)
///                         (list _1_quote form)
///                     (symbol_9_ form)
///                         (let [#_"String" ns (_0_ns form) #_"String" n (_0_name form)
///                               form
///                                 (cond
///                                     (and (nil_9_ ns) (String_2_endsWith n, "#"))
///                                         (LispReader_1_registerGensym scope, (symbol (String_2_substring n, 0, (dec (String_2_length n)))))
///                                     (and (nil_9_ ns) (String_2_endsWith n, "."))
///                                         (symbol (str (_0_name (Compiler_1_resolveSymbol (symbol (String_2_substring n, 0, (dec (String_2_length n)))))) "."))
///                                     (and (nil_9_ ns) (String_2_startsWith n, "."))
///                                         form
///                                     _0_else
///                                         (Compiler_1_resolveSymbol form)
///                                 )]
///                             (list _1_quote form)
///                         )
///                     (SyntaxQuoteReader_1_isUnquote form)
///                         (second form)
///                     (SyntaxQuoteReader_1_isUnquoteSplicing form)
///                         (throw "splice not in list")
///                     (coll_9_ form)
///                         (cond
///                             (map_9_ form)
///                                 (list _7_apply _7_hash_map (list _7_seq (cons _7_concat (SyntaxQuoteReader_1_sqExpandList scope, (seq (mapcat identity form))))))
///                             (vector_9_ form)
///                                 (list _7_apply _7_vector (list _7_seq (cons _7_concat (SyntaxQuoteReader_1_sqExpandList scope, (seq form)))))
///                             (set_9_ form)
///                                 (list _7_apply _7_hash_set (list _7_seq (cons _7_concat (SyntaxQuoteReader_1_sqExpandList scope, (seq form)))))
///                             (or (seq_9_ form) (list_9_ form))
///                                 (when_some [#_"ISeq" s (seq form)] => (cons _7_list nil)
///                                     (list _7_seq (cons _7_concat (SyntaxQuoteReader_1_sqExpandList scope, s)))
///                                 )
///                             _0_else
///                                 (throw "unknown collection type")
///                         )
///                     (or (keyword_9_ form) (number_9_ form) (char_9_ form) (string_9_ form))
///                         form
///                     _0_else
///                         (list _1_quote form)
///                 )]
///             (when (and (satisfies_9_ IObj form) (seq (meta form)) (not (SyntaxQuoteReader_1_isUnquote form))) => q
///                 (list _7_with_meta q (SyntaxQuoteReader_1_syntaxQuote scope, (meta form)))
///             )
///         )
///     )

///     (defn #_"Object" syntax_quote_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (let [scope (assoc scope _0_1_gensym_env (atom (hash_map)))]
///             (SyntaxQuoteReader_1_syntaxQuote scope, (LispReader_1_read r, scope))
///         )
///     )
// )

// (about #_"UnquoteReader"
///     (defn #_"Object" unquote_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (when_some [#_"char" ch (LispReader_1_read1 r)] => (throw "EOF while reading character")
///             (if (= ch \@)
///                 (list _7_unquote_splicing (LispReader_1_read r, scope))
///                 (do
///                     (LispReader_1_unread r, ch)
///                     (list _7_unquote (LispReader_1_read r, scope))
///                 )
///             )
///         )
///     )
// )

// (about #_"CharacterReader"
///     (defn #_"Object" character_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (when_some [#_"char" ch (LispReader_1_read1 r)] => (throw "EOF while reading character")
///             (let [#_"String" token (LispReader_1_readToken r, ch)]
///                 (when_not (= (String_2_length token) 1) => (Character_1_valueOf (String_2_charAt token, 0))
///                     (case_4_ token
///                         "newline"   \newline
///                         "space"     \space
///                         "tab"       \tab
///                         "backspace" \backspace
///                         "formfeed"  \formfeed
///                         "return"    \return
///                         (case_4_ (String_2_charAt token, 0)
///                             \u  (let [#_"int" c (LispReader_1_scanDigits token, 1, 4, 16)]
///                                     (when (<= 0xd800 c 0xdfff)
///                                         (throw (str "invalid character constant: \\u" (Integer_1_toString c, 16)))
///                                     )
///                                     (char c)
///                                 )
///                             \o  (let [#_"int" n (dec (String_2_length token))]
///                                     (when (< 3 n)
///                                         (throw (str "invalid octal escape sequence length: " n))
///                                     )
///                                     (let [#_"int" c (LispReader_1_scanDigits token, 1, n, 8)]
///                                         (when (< 0377 c)
///                                             (throw "octal escape sequence must be in range [0, 377]")
///                                         )
///                                         (char c)
///                                     )
///                                 )
///                             (throw (str "unsupported character: \\" token))
///                         )
///                     )
///                 )
///             )
///         )
///     )
// )

// (about #_"ListReader"
///     (defn #_"Object" list_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (apply list (LispReader_1_readDelimitedForms r, scope, \)))
///     )
// )

// (about #_"VectorReader"
///     (defn #_"Object" vector_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (vec (LispReader_1_readDelimitedForms r, scope, \]))
///     )
// )

// (about #_"MapReader"
///     (defn #_"Object" map_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (let [#_"vector" v (LispReader_1_readDelimitedForms r, scope, \})]
///             (when (even_9_ (count v)) => (throw "map literal must contain an even number of forms")
///                 (RT_1_map v)
///             )
///         )
///     )
// )

// (about #_"SetReader"
///     (defn #_"Object" set_reader [#_"PushbackReader" r, #_"IPersistentMap" scope, #_"char" _delim]
///         (PersistentHashSet_1_createWithCheck (LispReader_1_readDelimitedForms r, scope, \}))
///     )
// )

// (about #_"UnmatchedDelimiterReader"
///     (defn #_"Object" unmatched_delimiter_reader [#_"PushbackReader" _r, #_"IPersistentMap" scope, #_"char" delim]
///         (throw (str "unmatched delimiter: " delim))
///     )
// )

// (about #_"LispReader"
///     (def #_"{char fn}" LispReader_1_macros
///         (hash_map
///             \"  string_reader
///             \;  comment_reader
///             \'  quote_reader
///             \@  deref_reader
///             \^  meta_reader
///             \`  syntax_quote_reader
///             \~  unquote_reader
///             \(  list_reader,    \)  unmatched_delimiter_reader
///             \[  vector_reader,  \]  unmatched_delimiter_reader
///             \{  map_reader,     \}  unmatched_delimiter_reader
///             \\  character_reader
///             \%  arg_reader
///             \#  dispatch_reader
///         )
///     )

///     (def #_"{char fn}" LispReader_1_dispatchMacros
///         (hash_map
///             \^  meta_reader
///             \'  var_reader
///             \"  regex_reader
///             \(  fn_reader
///             \{  set_reader
///             \!  comment_reader
///             \_  discard_reader
///         )
///     )
// )
}

/// (defn read
///     ([] (read std::cin))
///     ([s] (read s true nil))
///     ([s eof_error_9_ eof_value] (LispReader_1_read s, nil, (boolean eof_error_9_), eof_value))
/// )

namespace arbace {

// (about #_"Compiler"
///     (defn #_"Object" Compiler_1_load [#_"Reader" reader]
///         (let [
///             #_"PushbackReader" r (if (pushback_reader_9_ reader) reader (PushbackReader_1_new reader)) #_"Object" EOF (anew 0)
///             #_"IPersistentMap" scope (hash_map _0_1_local_env (atom (hash_map)))
///         ]
///             (loop [#_"Object" value nil]
///                 (LispReader_1_consumeWhitespaces r)
///                 (let_when [#_"edn" form (LispReader_1_read r, nil, false, EOF)] (identical_9_ form EOF) => (recur (Compiler_1_eval form, scope))
///                     value
///                 )
///             )
///         )
///     )
// )
}

/// (defn macroexpand_1 [form] (Compiler_1_macroexpand1 form))

/// (defn macroexpand [form]
///     (let_when [e (macroexpand_1 form)] (identical_9_ e form) => (recur e)
///         form
///     )
/// )

/// (defn eval [form] (Compiler_1_eval form))

/// (defmacro definline [name & decl]
///     (let [[pre_args [args expr]] (split_with (comp not vector_9_) decl)]
///         `(do
///             (defn ~name ~@pre_args ~args ~(apply (eval (list _7_fn args expr)) args))
///             (alter_meta_4_ (var ~name) assoc _0_inline (fn ~name ~args ~expr))
///             (var ~name)
///         )
///     )
/// )

/// (defn ns_resolve
///     ([ns sym] (ns_resolve ns nil sym))
///     ([ns env sym]
///         (when_not (contains_9_ env sym)
///             (Compiler_1_maybeResolveIn (the_ns ns) sym)
///         )
///     )
/// )

/// (defn resolve
///     ([    sym] (ns_resolve _8_ns_8_     sym))
///     ([env sym] (ns_resolve _8_ns_8_ env sym))
/// )

int main() {
    std::cout << "áéíóöőúüű" << std::endl;
}
