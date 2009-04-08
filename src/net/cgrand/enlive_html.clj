;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html
  (:require [clojure.xml :as xml])
  (:require [clojure.zip :as z])
  (:require [clojure.contrib.zip-filter :as zf])
  (:require [clojure.contrib.zip-filter.xml :as zfx])
  (:use [clojure.contrib.test-is :as test-is :only [set-test with-test is]]))

;; enlive-html is a selector-based templating engine
;;
;; EXAMPLES: see net.cgrand.enlive-html.examples

;; HTML I/O stuff

(defn tag? [node]
  (map? node))

(defn- startparse-tagsoup [s ch]
  (let [p (org.ccil.cowan.tagsoup.Parser.)]
    (.setFeature p "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
    (.setFeature p "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
    (.setContentHandler p ch)
    (.parse p s)))

(defn load-html-resource 
 "Loads and parse an HTML resource and closes the stream."
 [stream] 
  (with-open [stream stream]
    (xml/parse (org.xml.sax.InputSource. stream) startparse-tagsoup)))

(defmulti html-resource type)

(defmethod html-resource java.util.Map
 [xml-data]
  xml-data)

(defmethod html-resource String
 [path]
  (load-html-resource (-> (clojure.lang.RT/baseLoader) (.getResourceAsStream path))))

(defmethod html-resource java.io.File
 [file]
  (load-html-resource (java.io.FileInputStream. file)))

(defmethod html-resource java.net.URL
 [#^java.net.URL url]
  (load-html-resource (.getContent url)))

(defmethod html-resource java.net.URI
 [#^java.net.URI uri]
  (html-resource (.toURL uri)))


(defn xml-str
 "Like clojure.core/str but escapes < > and &."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))
  
(defn attr-str
 "Like clojure.core/str but escapes < > & and \"."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;") (.replace "\"" "&quot;")))

(def *self-closing-tags* #{:area :base :basefont :br :hr :input :img :link :meta})

(declare emit)

(defn emit-attrs [attrs]
  (mapcat (fn [[k v]]
            [" " (name k) "=\"" (attr-str v) "\""]) attrs))

(defn emit-tag [tag]
  (let [name (-> tag :tag name)]
    (concat ["<" name]
      (emit-attrs (:attrs tag))
      (if-let [s (seq (:content tag))]
        (concat [">"] (mapcat emit s) ["</" name ">"])
        (if (*self-closing-tags* tag) 
          [" />"]
          ["></" name ">"])))))

(defn emit [node]
  (if (map? node)
    (emit-tag node)
    [(xml-str node)]))
    
(defn emit* [node-or-nodes]
  (if (map? node-or-nodes) (emit node-or-nodes) (mapcat emit node-or-nodes)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- not-node? [x]
  (cond (string? x) false (map? x) false :else true))

(defn- flatten [x]
  (remove not-node? (tree-seq not-node? seq x)))

(defn convert-selector [selector]
  (loop [xs (seq selector) descendants true zfx-selector []]
    (if-let [[x & xs] xs]
      (cond
        (= :> x) (recur xs false zfx-selector)
        descendants (recur xs true (conj zfx-selector zf/descendants x))
        :else (recur xs true (conj zfx-selector x)))
      zfx-selector)))

(defn- transform-1 [node selector transformation]
  (let [root-loc (z/xml-zip node)
        selected-locs (set (apply zfx/xml-> root-loc (convert-selector selector)))
        _ (prn "selected:" (count selected-locs) "/" selector)
        xform (fn xform [loc]
                (if (z/branch? loc)
                  (let [children (flatten (map xform (zf/children loc)))
                        node (z/make-node loc (z/node loc) children)]
                    (if (selected-locs loc) (transformation node) node))
                  (z/node loc)))]
    (xform root-loc)))

(defn transform [nodes selector transformation]
  (flatten (map #(transform-1 % selector transformation) nodes)))

(defn content [& xs]
  #(assoc % :content xs))

(defn at* [& rules]
  (fn [node] 
    (reduce #(apply transform %1 %2) [node] (partition 2 rules))))
    
(defn at [node & rules]
  ((apply at* rules) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn content [& values]
  #(assoc % :content values))
    
(defn set-attr [& kvs]
	#(let [attrs (into (:attrs %) (partition 2 kvs))]
 	  (assoc % :attrs attrs)))
     
(defn remove-attr [xml & attr-names]
  #(let [attrs (apply dissoc (:attrs %) attr-names)]
    (assoc % :attrs attrs)))

(comment deftemplate-macro xhtml-strict [xml & forms]
  `(escaped (list
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" 
     (apply-template-macro ~(assoc-in xml [:attrs :xmlns] "http://www.w3.org/1999/xhtml") (at ~@forms))))) 

;; main macros
(defmacro template 
 "A template returns a seq of string."
 ([xml-or-path args form & forms] 
   `(template ~xml-or-path ~args (at* ~form ~@forms)))
 ([xml-or-path args form]
   (let [xml (html-resource xml-or-path)]
     `(fn ~args (emit* (~form ~xml))))))

(defmacro deftemplate
 "Defines a template as a function that returns a seq of strings." 
 [name xml-or-path args & forms] 
  `(def ~name (template ~xml-or-path ~args ~@forms)))

(defmacro snippet 
 "A snippet is a function that returns nodes or nested collections of nodes."
 [xml-or-path selector args & forms]
  (let [xml (html-resource xml-or-path)
        nodes (map z/node (apply zfx/xml-> (z/xml-zip xml) selector))]
    `(fn ~args
       (map (at* ~@forms) [~@nodes]))))

(defmacro defsnippet
 "Define a named snippet -- equivalent to (def name (snippet xml-or-path selector args ...))."
 [name xml-or-path selector args & forms]
 `(def ~name (snippet ~xml-or-path ~selector ~args ~@forms)))
   
(defmacro defsnippets
 [xml-or-path & specs]
  (let [xml (html-resource xml-or-path)]
   `(do
     ~@(map (fn [[name selector args & forms]]
              `(def ~name (snippet ~xml ~selector ~args ~@forms)))
         specs))))
