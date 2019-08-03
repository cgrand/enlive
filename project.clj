(defproject enlive "1.1.6"
  :min-lein-version "2.0.0"
  :description "a HTML selector-based (à la CSS) templating and transformation system for Clojure"
  :url "http://github.com/cgrand/enlive/"
  :profiles {:dev {:resource-paths ["test/resources"]}
             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]
                     :plugins [[lein-codox "0.10.3"]]
                     :codox {:project {:name "enlive"}
                             :metadata {:doc/format :markdown}
                             :themes [:rdash]
                             :doc-paths ["doc"]
                             :output-path "gh-pages"}}}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2.1"]
                 [org.jsoup/jsoup "1.7.2"]])
