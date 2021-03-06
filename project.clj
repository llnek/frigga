;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/frigga "1.1.0"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :description ""
  :url "https://github.com/llnek/frigga"

  :dependencies
  [[org.clojure/clojurescript "1.9.542"]
   [org.clojure/clojure "1.8.0"]
   [io.czlab/wabbit-cons "1.0.0"]
   [io.czlab/loki "1.0.0"]
   [io.czlab/wabbit "1.0.0"]
   [io.czlab/wabbit-plugs "1.0.1"]]

  :plugins [[wabbit/lein-template "1.0.0"]
            [cider/cider-nrepl "0.14.0"]
            [lein-codox "0.10.3"]
            [lein-cprint "1.2.0"]]

  :kill-port "localhost:4444"
  :profiles
  {:podify
   {:agentlib "-agentlib:jdwp=transport=dt_socket,server=y,address=8787,suspend=n"
    :jvm-opts ^:replace
    ["-XX:+CMSClassUnloadingEnabled"
     "-XX:+UseConcMarkSweepGC"
     "-Xms1g"
     "-Xmx8g"
     "-Dwabbit.kill.port=@@kill-port@@"
     "-Dlog4j.configurationFile=file:etc/log4j2d.xml"]}
   :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* true}
  :target-path "out/%s"
  :aot :all

  :aliases {"deploy" ["with-profile"
                      "podify" "wabbit"]
            "run" ["trampoline"
                   "run" "-m" "czlab.wabbit.core"]}

  :test-selectors {:ttt :tictactoe
                   :ecs :ecs
                   :pong :pong}

  ;;:java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  ;;:resource-paths ["src/main/resources"]

  :jvm-opts ["-Dlog4j.configurationFile=file:etc/log4j2c.xml"]
  :javac-options ["-source" "8"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


