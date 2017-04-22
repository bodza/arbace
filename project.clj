(defproject arbace "x.y.z"
    :dependencies [[org.clojure/clojure "1.8.0"]]
    :plugins [[lein-try "0.4.3"]]
;   :global-vars {*warn-on-reflection* true}
    :jvm-opts ["-Xmx12g"]
;   :javac-options ["-g"]
    :source-paths ["src"] :java-source-paths ["src"] :resource-paths ["resources"] :test-paths ["src"]
    :main arbace.core
    :aliases {"arbace" ["run" "-m" "arbace.core"]})
