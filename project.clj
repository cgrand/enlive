(defproject co.uk.egao/enlive "1.1.6-SNAPSHOT"
  :min-lein-version "2.0.0"
  :description "a HTML selector-based (à la CSS) templating and transformation system for Clojure"
  :url "http://github.com/egao1980/enlive"
  :profiles     {:dev {:resource-paths ["test/resources"]}}
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2.1"]
                 [org.jsoup/jsoup "1.7.2"]
                 [org.clojure/tools.namespace "0.2.4"]])
