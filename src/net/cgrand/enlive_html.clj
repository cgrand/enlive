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
 "Loads and parse an HTML resource."
 [path] 
  (with-open [stream (-> (clojure.lang.RT/baseLoader) (.getResourceAsStream path))]
    (xml/parse (org.xml.sax.InputSource. stream) startparse-tagsoup)))

(defn flatten 
 "Flattens nested lists."
 [s]
  (remove #(or (seq? %) (nil? %)) (tree-seq seq? seq s)))

(defn xml-str
 "Like clojure.core/str but escapes < > and &."
 [& xs]
  (apply str (map #(-> % str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")) xs)))
(defn attr-str
 "Like clojure.core/str but escapes < > & and \"."
 [& xs]
  (apply str (map #(-> % str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;") (.replace "\"" "&quot;")) xs)))

(def *non-empty-tags* #{:script})

(declare compile-node)

(defn- compile-attr [v]
  (if (seq? v) 
    v ;code
    (attr-str v)))

(defn- compile-element [xml]
  (concat
    ["<" (-> xml :tag name)] 
    (mapcat (fn [[k v]] [" " (name k) "=\"" (compile-attr v) "\""]) 
      (:attrs xml))
    (if-not (or (:content xml) (-> xml :tag *non-empty-tags*))
      [" />"]
      (concat [">"] 
        (mapcat compile-node (:content xml)) 
        ["</" (-> xml :tag name) ">"]))))

(defn- compile-node [node]
  (cond
    (map? node) (compile-element node)
    (seq? node) [node] ; it's code
    :else [(xml-str node)]))

(with-test
  (defn- merge-str [coll]
    (when (seq coll)
      (let [[strs etc] (split-with string? coll)]
        (if strs
          (lazy-cons (apply str strs) (merge-str etc))
          (lazy-cons (first coll) (merge-str (rest coll)))))))

  ;; tests
  (is (= (merge-str ["ab" "cd" ["fe"] "gh" \c "i" "j"])
        ["abcd" ["fe"] "gh" \c "ij"])))

;;
(defn- unquote? [form]
  (and (seq? form) (= (first form) `unquote)))
          
(with-test  
  (defn- replace-unquote [form replacement-fn]
    (let [replace 
           (fn replace [form]
             (cond 
               (unquote? form) (replacement-fn (second form))
               (seq? form) (map replace form)
               (vector? form) (vec (map replace form))
               (map? form) (into {} (map (fn [[k v]] 
                                           [(replace k) (replace v)]) form))
               (set? form) (set (map replace form))
               :else form))]
      (replace form)))      

  ;; tests
  (is (= (replace-unquote '(fred ethel ~someone) (constantly 'lucy))
        '(fred ethel lucy)))
  (is (= (replace-unquote '(fred [ethel ~someone]) (constantly 'lucy))
        '(fred [ethel lucy])))
  (is (= (replace-unquote '(fred {ethel ~someone}) (constantly 'lucy))
        '(fred {ethel lucy})))
  (is (= (replace-unquote '(fred {~someone ethel}) (constantly 'lucy))
        '(fred {lucy ethel}))))

;;
(declare template-macro)

(defmacro deftemplate-macro
 "Define a macro to be used inside a template. The first arg to a template-macro 
  is the current xml subtree being templated."
 [name bindings & forms]
 (let [[bindings doc-string forms] (if (string? bindings) 
                                     [(first forms) bindings (rest forms)]
                                     [bindings nil forms])] 
   `(defmacro ~name {:doc ~doc-string :arglists '([~@(rest bindings)])} 
     [& args#]
      (let [macro-fn# (fn ~bindings ~@forms)]
        (apply list `template-macro macro-fn# args#)))))   

(deftemplate-macro text [xml & forms]
  (if (tag? xml)
    (assoc xml :content [`(xml-str ~@forms)])
    `(xml-str ~@forms)))
     
(with-test
  (defn- expand-til-template-macro [xml form]
    (if (seq? form)
      (let [x (first form)]  
        (if (and (symbol? x) (= (resolve x) #'template-macro))
          (apply (second form) xml (rrest form))
          (let [ex-form (macroexpand-1 form)]
            (if (= ex-form form)
              (replace-unquote form #(list `apply-template-macro xml %)) 
              (recur xml ex-form)))))
      (recur xml (list `text form))))

  ;; tests    
  (is (= (expand-til-template-macro 'XML '(unexpandable-form))
        '(unexpandable-form)))
  (is (= (expand-til-template-macro 'XML '(unexpandable-form (with-nested ~ops)))
        '(unexpandable-form (with-nested (net.cgrand.enlive-html/apply-template-macro XML ops)))))
  (is (= (expand-til-template-macro 'XML (list `text 'hello 'world))
        '(net.cgrand.enlive-html/xml-str hello world)))
  (is (= (expand-til-template-macro 'XML 'a-symbol)
        '(net.cgrand.enlive-html/xml-str a-symbol))))
        

;(with-test  ; commented out because of demacro forms returning nil      
  (defmacro apply-template-macro 
   [xml form]
    (let [code (expand-til-template-macro xml form)]
      (cons `list (-> code compile-node merge-str))))
      
(set-test apply-template-macro
  ;;tests
  (is (= (macroexpand-1 (list `apply-template-macro 
                          {:tag :hello :content ["world" '(some code)] :attrs {:a "b"}}
                          (list `template-macro (fn [x & _] x))))
        '(clojure.core/list "<hello a=\"b\">world" (some code) "</hello>"))))

(defmacro do->
 "Chains (composes) several template-macros."
 [& forms]
  (let [fs (map (comp second expand-til-template-macro) forms)
        f (apply comp (reverse fs))]
    `(template-macro ~f)))  

;; simple template macros
(deftemplate-macro show [xml]
  xml)
     
(deftemplate-macro set-attr [xml & forms]
  (if (tag? xml)
    (let [attrs (reduce (fn [attrs [name form]] (assoc attrs name `(attr-str ~form)))
                  (:attrs xml) (partition 2 forms))]
      (assoc xml :attrs attrs)) 
    xml))
     
(deftemplate-macro remove-attr [xml & attr-names]
  (if (tag? xml)
    (let [attrs (apply dissoc (:attrs xml) attr-names)]
      (assoc xml :attrs attrs)) 
    xml))

(deftemplate-macro xhtml-strict [xml & forms]
  `(list
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" 
     (apply-template-macro ~(assoc-in xml [:attrs :xmlns] "http://www.w3.org/1999/xhtml") (at ~@forms)))) 

;; selectors stuff
(defn- step-selector
 "Selectors are states in an infinite state machine
  and step-selector is their transition function.
  Returns [new-selector action-or-nil]." 
 [selector node]
  (selector node))

(defn- run-selector
 "For testing purpose."
  [sel s]
   (second (reduce #(step-selector (first %1) %2) [sel] s)))
   
(defn- null-selector [_]
  [null-selector false]) 

(with-test
  (defn- self-selector [pred action]
   "Returns a selector that matches only the current node (and it needs to 
    satisfy the supplied predicate."
    (fn this [node]
      [null-selector (when (pred node) action)]))
      
  ;; tests
  (let [sel (self-selector #(= % :a) true)]
    (is (run-selector sel [:a])) 
    (is (not (run-selector sel [:b])))
    (is (not (run-selector sel [:a :b])))
    (is (not (run-selector sel [:b :a])))
    (is (not (run-selector sel [:b :c])))))

(with-test
  (defn- self-or-descendants-selector [pred action]
   "Returns a selector that matches the current node or any of its descendants
    as long as they satisfy the supplied predicate."
    (fn this [node]
      [this (when (pred node) action)])) 
      
  ;; tests
  (let [sel (self-or-descendants-selector #(= % :a) true)]
    (is (run-selector sel [:a])) 
    (is (not (run-selector sel [:b])))
    (is (not (run-selector sel [:a :b])))
    (is (run-selector sel [:b :a]))
    (is (not (run-selector sel [:b :c]))) 
    (is (run-selector sel [:b :c :a]))
    (is (not (run-selector sel [:b :a :c]))))) 

(with-test
  (defn- merge-selectors
   "Returns the union of supplied selectors. When succesful a merged selector 
    returns the action of its leftmost successful selector."   
    ([] null-selector)
    ([selector] selector)
    ([selector & selectors]
      (fn [node]
        (let [results (map #(% node) (cons selector selectors))]
          [(apply merge-selectors (map first results))
           (some identity (map second results))]))))
  
  ;; tests         
  (let [sel-a (self-or-descendants-selector #(= % :a) :true-a)
        sel-b (self-or-descendants-selector #(= % :b) :true-b)
        sel (merge-selectors sel-a sel-b)]
    (is (= :true-a (run-selector sel [:a]))) 
    (is (= :true-b (run-selector sel [:b])))
    (is (not (run-selector sel [:c])))
    (is (= :true-b (run-selector sel [:a :b])))
    (is (= :true-a (run-selector sel [:c :a])))
    (is (not (run-selector sel [:b :c]))) 
    (is (= :true-a (run-selector sel [:b :c :a])))
    (is (not (run-selector sel [:b :a :c]))))) 
           
(with-test
  (defn- chain-selectors
   "Composes selectors from left to right."  
    ([] null-selector)
    ([selector] selector)
    ([selector & next-selectors]
      (let [next-selector (apply chain-selectors next-selectors)]
        (fn [node]
          (let [[subselector r] (selector node)
                chained-subselector (chain-selectors subselector next-selector)]
            (if r
              [(merge-selectors next-selector chained-subselector) false] 
              [chained-subselector false]))))))

  ;; tests         
  (let [sel-a (self-or-descendants-selector #(= % :a) :true-a)
        sel-b (self-or-descendants-selector #(= % :b) :true-b)
        sel (chain-selectors sel-a sel-b)]
    (is (not (run-selector sel [:a]))) 
    (is (not (run-selector sel [:b])))
    (is (= :true-b (run-selector sel [:a :b])))
    (is (not (run-selector sel [:c :a])))
    (is (not (run-selector sel [:b :c]))) 
    (is (not (run-selector sel [:b :c :a])))
    (is (= :true-b (run-selector sel [:a :c :b])))
    (is (= :true-b (run-selector sel [:c :a :b])))
    (is (= :true-b (run-selector sel [:d :a :c :b])))
    (is (not (run-selector sel [:b :a :c]))))) 
           
     
;; the "at" template-macro: allows to apply other template-macros to subtrees using selectors.

(declare transform-node)  

(defn- transform-tag [{:keys [content] :as node} selector action]
  (let [transformed-node
         (assoc node :content 
           (vec (map #(apply transform-node % (step-selector selector %)) 
                  content)))] 
    (if action
      `(apply-template-macro ~transformed-node ~action)
      transformed-node)))
      
(defn- transform-node [node selectors-actions action]
  (if (tag? node)
    (transform-tag node selectors-actions action)
    node))

(defn- keyword-pred [kw]
  (let [segments (.split (name kw) "(?=[#.])")
        preds (map (fn [#^String s] (condp = (first s)
                      \. #(-> % :attrs (:class "") (.split "\\s+") set (get (.substring s 1))) 
                      \# #(= (.substring s 1) (-> % :attrs :id))
                      #(= s (name (:tag %))))) segments)]
    (fn [x]
      (and (tag? x) (every? identity (map #(% x) preds))))))

(defn- compile-selector-step [form]
  (cond 
    (seq? form) 
      (eval `(fn [x#] (~(first form) x# ~@(rest form))))
    (keyword? form)
      (keyword-pred form)
    :else 
      (eval form)))   

(defn- compile-selector
 "Evals a selector form. If the form is anything but a vector,
  it's simply evaluated. If the form is a vector, each element
  is expected to evaluate to a predicate on nodes.
  There's special rules for keywords (see keyword-pred) and
  lists ((a b c) yields #(a % b)).
  Predicates are chained in a hierarchical way à la CSS."
 [selector-form action]
  (if-not (vector? selector-form)
    (eval selector-form)
    (loop [items (seq selector-form) 
           selector-type self-or-descendants-selector
           selectors []]
      (if-let [[x & xs] items]
        (if (= :> x)
          (recur xs self-selector selectors)
          (let [pred (compile-selector-step x)]
            (recur xs self-or-descendants-selector
              (conj selectors (selector-type pred action)))))
        (apply chain-selectors selectors))))) 
               
  
(deftemplate-macro at
 "Allows to apply other template-macros to subtrees using selectors." 
 [xml & forms]
  (let [selector (apply merge-selectors (map #(apply compile-selector %) 
                                          (partition 2 forms)))]
    (apply transform-node xml (step-selector selector xml))))

;; main macro
(defmacro deftemplate
 "Defines a template as a function that returns a seq of strings." 
 ([name path args form]
  (let [xml (load-html-resource path)]
    `(defn ~name ~args (flatten (apply-template-macro ~xml ~form)))))
 ([name path args form & forms] 
   `(deftemplate ~name ~path ~args (at ~form ~@forms))))
