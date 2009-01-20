(ns net.cgrand.enlive-html.examples-with-ring
  (:require [ring.jetty])
  (:require [net.cgrand.enlive-html.examples :as examples]) 
  (:use [net.cgrand.enlive-html :as html :only [deftemplate at]]))

(defn app [req] {:status 200
	:headers {"Content-Type" "text/html"}
	:body (examples/microblog-template "Hello you!" 
             [{:title "post #1" 
               :body "hello with dangerous chars: <>&"}
              {:title "post #2" 
               :body "dolor ipsum"}])})
               
(ring.jetty/run {:port 8080} app)