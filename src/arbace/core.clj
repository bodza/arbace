(ns arbace.core
    (:refer-clojure :only [*ns* = apply assoc cond conj cons defmacro #_def defn defprotocol defrecord #_do doseq extend-type first #_if if-not import inc into keys let letfn loop next ns-imports ns-unmap or peek pop #_recur second seq some? str symbol symbol? #_throw update vary-meta vector?])
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
