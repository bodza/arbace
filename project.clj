(defproject arbace "x.y.z"
    :dependencies [[org.clojure/clojure "1.9.0"]
                   [org.clojure/core.rrb-vector "0.0.11"]
;                  [org.clojure/data.priority-map "0.0.10"]
                   [org.flatland/ordered "1.5.6"]
;                  [org.graalvm/graal-sdk "1.0.0-rc1"]
                  ]
    :plugins [[lein-try "0.4.3"]]
;   :global-vars {*warn-on-reflection* true}
    :jvm-opts ["-Xmx6g"
;                   "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.amd64=ALL-UNNAMED"
                    "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.code=ALL-UNNAMED"
                    "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.code.site=ALL-UNNAMED"
                    "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.hotspot=ALL-UNNAMED"
;                   "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.hotspot.amd64=ALL-UNNAMED"
                    "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.meta=ALL-UNNAMED"
                    "--add-opens=jdk.internal.vm.ci/jdk.vm.ci.runtime=ALL-UNNAMED"
               "-XX:+UnlockExperimentalVMOptions"
               "-XX:+EnableJVMCI"]
    :javac-options ["-g"
                    "--add-modules=jdk.internal.vm.ci"
;                   "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.amd64=ALL-UNNAMED"
                    "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.code=ALL-UNNAMED"
                    "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.code.site=ALL-UNNAMED"
                    "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.hotspot=ALL-UNNAMED"
;                   "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.hotspot.amd64=ALL-UNNAMED"
                    "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.meta=ALL-UNNAMED"
                    "--add-exports=jdk.internal.vm.ci/jdk.vm.ci.runtime=ALL-UNNAMED"
                   ]
    :source-paths ["src"] :java-source-paths ["src"] :resource-paths ["resources"] :test-paths ["src"]
;   :main arbace.core
    :aliases {"arbace" ["run" "-m" "arbace.core"]})
