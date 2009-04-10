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
      
;; utilities

(defn- not-node? [x]
  (cond (string? x) false (map? x) false :else true))

(defn- flatten [x]
  (remove not-node? (tree-seq not-node? seq x)))
  
(defn- flatmap [f xs]
  (flatten (map f xs)))

;; state machine stuff
;; a state is a pair consisting of a boolean (acceptance) and a seq of functions (functions from loc to state) 

(def accept? first)

(defn hopeless?
 "Returns true if the state machine cannot succeed. (It's not a necessary condition.)" 
 [state] (empty? (second state)))

(defn step [state loc]
  (let [states (map #(% loc) (second state))]
    [(some accept? states) (mapcat second states)]))    
  
(defn union 
 "Returns a state machine which succeeds as soon as one of the specified state machines succeeds."
 [& states]
  [(some accept? states) (mapcat second states)])
  
(defn intersection
 "Returns a state machine which succeeds when all specified state machines succeed." 
 [& states]
  [(every? accept? states)
   (when-not (some hopeless? states) 
     [(fn [loc] (apply intersection (map #(step % loc) states)))])]) 

(defn chain [[x1 fns1] s2]
  (let [chained-fns1 (map #(fn [loc] (chain (% loc) s2)) fns1)]
    (if x1
      [(accept? s2) (concat (second s2) chained-fns1)]
      [false chained-fns1])))

(def descendants-or-self
  [true (lazy-seq [(constantly descendants-or-self)])])

(def accept [true nil])

(defn pred [f]
  [false [(fn [loc]
            [(and (z/branch? loc) (f (z/node loc))) nil])]])

(defn tag= [tag-name]
  (pred #(= (:tag %) tag-name)))

(defn id= [id]
  (pred #(= (-> % :attrs :id) id)))

(defn has-class [classes]
  (pred #(let [elt-classes (set (-> % :attrs (:class "") (.split "\\s+")))]
           (every? elt-classes classes))))

;; selector syntax
(defn compile-keyword [kw]
  (let [[tag-name & etc] (.split (name kw) "(?=[#.])")
        tag-pred (if (#{"" "*"} tag-name) [] [(tag= (keyword tag-name))])
        ids-pred (for [s etc :when (= \# (first s))] (id= (subs s 1)))
        classes (set (for [s etc :when (= \. (first s))] (subs s 1)))
        class-pred (when (seq classes) [(has-class classes)])] 
    (apply intersection (concat tag-pred ids-pred class-pred))))
    
(declare compile-step)

(defn compile-union [s]
  (apply union (map compile-step s)))      
    
(defn compile-intersection [s]
  (apply union (map compile-step s)))      

(defn compile-step [s]
  (cond
    (keyword? s) (compile-keyword s)    
    (set? s) (compile-union s)    
    (vector? s) (compile-intersection s)
    :else (throw (RuntimeException. (str "Unsupported selector step: " (pr-str s))))))

(defn compile-chain [s]
  (let [[child-ops [step & next-steps :as steps]] (split-with #{:>} s)
        next-chain (if (seq steps) 
                     (chain (compile-step step) (compile-chain next-steps))
                     accept)]
    (if (seq child-ops)
      next-chain
      (chain descendants-or-self next-chain)))) 

(defn compile-selector [s]
  (if (set? s)
    (apply union (map compile-selector s)) 
    (compile-chain s)))

;; core 
  
(defn- children-locs [loc]
  (take-while identity (iterate z/right (z/down loc))))

(defn- transform-loc [loc previous-state transformation]
  (if (z/branch? loc)
    (let [state (step previous-state loc)
          children (flatmap #(transform-loc % state transformation) (children-locs loc))
          node (z/make-node loc (z/node loc) children)]
      (if (accept? state)
        (transformation node)
        node))
    (z/node loc)))
      
(defn- transform-1 [node selector transformation]
  (let [root-loc (z/xml-zip node)
        state (compile-selector selector)]
    (transform-loc root-loc state transformation)))

(defn transform [nodes selector transformation]
  (flatmap #(transform-1 % selector transformation) nodes))

(defn at* [& rules]
  (fn [node] 
    (reduce #(apply transform %1 %2) [node] (partition 2 rules))))
    
(defn at [node & rules]
  ((apply at* rules) node))

(defn select* [loc previous-state]
  (let [state (step previous-state loc)]
    (if (accept? state)
      (list (z/node loc))
      (mapcat #(select* % state) (children-locs loc))))) 
      
(defn select [node selector]
  (let [state (compile-selector selector)
        root-loc (z/xml-zip node)]
    (select* root-loc state)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn content [& values]
  #(assoc % :content (flatten values)))
    
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
   `(let [xml# ~(html-resource xml-or-path)]
      (fn ~args (emit* (~form xml#))))))

(defmacro deftemplate
 "Defines a template as a function that returns a seq of strings." 
 [name xml-or-path args & forms] 
  `(def ~name (template ~xml-or-path ~args ~@forms)))

(defmacro snippet 
 "A snippet is a function that returns nodes or nested collections of nodes."
 [xml-or-path selector args & forms]
  (let [xml (html-resource xml-or-path)
        nodes (select xml selector)]
    `(let [nodes# [~@nodes]] 
       (fn ~args
         (flatmap (at* ~@forms) nodes#)))))

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
