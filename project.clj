(defproject arbace "x.y.z"
    :dependencies [[org.clojure/clojure "1.9.0"]
                   [org.clojure/core.rrb-vector "0.0.11"]
;                  [org.clojure/data.priority-map "0.0.10"]
                   [org.flatland/ordered "1.5.6"]
                  ]
    :plugins [[lein-try "0.4.3"]]
;   :global-vars {*warn-on-reflection* true}
    :jvm-opts ["-Xmx6g"]
    :javac-options ["-g"]
    :source-paths ["src"] :java-source-paths ["src"] :resource-paths ["resources"] :test-paths ["src"]
;   :main arbace.core
    :aliases {"arbace" ["run" "-m" "arbace.core"]})
