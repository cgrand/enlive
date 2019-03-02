(defproject enlive "1.1.6"
  :description "a HTML selector-based (à la CSS) templating and transformation system for Clojure"
  :url "http://github.com/cgrand/enlive/"
  :min-lein-version "2.7.1"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2.1"]
                 [org.jsoup/jsoup "1.11.3"]
                 [tupelo "0.9.133"]]

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]]}
             :dev      {:dependencies [[org.clojure/clojure "1.10.0"]]}
             :1.8      {:dependencies [[org.clojure/clojure "1.8.0"]]}}

  :source-paths ["src"]
  :test-paths ["test"]
  :resource-paths ["test/resources"]
  :target-path "target/%s"
  )
