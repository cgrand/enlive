(ns net.cgrand.enlive-html.examples
  (:use [net.cgrand.enlive-html :as html :only [clone-for deftemplate at content set-attr attr? strict-mode substitute]] :reload))

(deftemplate microblog-template "net/cgrand/enlive_html/example.html"
 [title posts]
   [:title] (content title)
   [:h1] (content title)
   [:div.no-msg] (if (empty? posts)
                   identity
                   (substitute nil))
   [:div.post] (clone-for [{:keys [title body]} posts]
                          [:h2 :a] (content title)
                          [:p] (content body))
   [[:a (attr? :href)]] (set-attr :title "it's a link"))

(defn render
  [nodes]
  (apply str nodes))

(comment
  (render (microblog-template "Hello user!"
               [{:title "post #1"
                 :body "hello with dangerous chars: <>&"}
                {:title "post #2"
                 :body "dolor ipsum"}])))
