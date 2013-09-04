(defproject shoelace "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ring "1.1.8"]
                 [hiccup "1.0.4"]
                 [hiccups "0.2.0"]
                 [prismatic/dommy "0.1.1"]
                 [cljs-ajax "0.1.6"]
                 [org.clojure/core.async "0.1.0-SNAPSHOT"]]
  :plugins [[lein-cljsbuild "0.3.2"]
            [lein-ring "0.8.5"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild {
    :builds {
      :main {
        :source-paths ["src/cljs"]
        :compiler {:output-to "resources/public/js/cljs.js"
                   :optimizations :simple
                   :pretty-print true}
        :jar true}}}
  :min-lein-version "2.0.0"
  :main shoelace.server
  :ring {:handler shoelace.server/app})
