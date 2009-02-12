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

(defn- node-seq [branch? children x]
  (remove branch? (tree-seq branch? children x))) 

(defn flatten 
 "Flattens nested lists."
 [s]
  (node-seq #(or (seq? %) (nil? %)) seq s))

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

(defn escaped [x]
  (vary-meta (if (seq? x) x (list x)) assoc ::escaped true))

(defn escaped? [x]
  (-> x meta ::escaped))

(defn escape-user-code [esc x]
  (escaped ((fn this [x]
              (cond
                (escaped? x) x
                (seq? x) (escaped (map this x))
                :else (escaped (esc x)))) x))) 
  
(defn- user-code [esc x] `(escape-user-code ~esc ~x))
(defn- user-code? [x] (and (seq? x) (= `escape-user-code (first x))))

(defn- flatten-transformed [node]
  (node-seq #(and (coll? %) (not (user-code? %))) seq node)) 

(defn- compile-attr [v]
  (if (seq? v) 
    (user-code `attr-str v)
    (attr-str v)))

(defn- compile-element [xml]
  ["<" (-> xml :tag name) 
    (map (fn [[k v]] [" " (name k) "=\"" (compile-attr v) "\""]) 
      (:attrs xml))
    (if-not (or (:content xml) (-> xml :tag *non-empty-tags*))
      " />"
      [">" 
        (map compile-node (:content xml)) 
        "</" (-> xml :tag name) ">"])])

(defn- compile-node [node]
  (cond 
    (map? node) (compile-element node)
    (string? node) (xml-str node)
    :else (user-code `xml-str node)))

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
    (assoc xml :content [`(str ~@forms)])
    `(str ~@forms)))
     
(with-test
  (defn- expand-til-template-macro [xml form]
    (let [form (macroexpand form)]
      (if (seq? form)
        (let [x (first form)]  
          (if (and (symbol? x) (= (resolve x) #'template-macro))
            (apply (second form) xml (rrest form))
            (replace-unquote form #(list `apply-template-macro xml %)))) 
        (recur xml (list `text form)))))

  ;; tests    
  (is (= (expand-til-template-macro 'XML '(unexpandable-form))
        '(unexpandable-form)))
  (is (= (expand-til-template-macro 'XML '(unexpandable-form (with-nested ~ops)))
        '(unexpandable-form (with-nested (net.cgrand.enlive-html/apply-template-macro XML ops)))))
  (is (= (expand-til-template-macro 'XML (list `text 'hello 'world))
        (list `str 'hello 'world)))
  (is (= (expand-til-template-macro 'XML 'a-symbol)
        (list `str 'a-symbol))))
        

(with-test      
  (defmacro apply-template-macro 
   [xml form]
    (let [code (expand-til-template-macro xml form)]
      (list `escaped
        (cons `list (-> code compile-node flatten-transformed merge-str)))))
      
  ;;tests
  (is (= (macroexpand-1 (list `apply-template-macro 
                          {:tag :hello :content ["world" '(some code)] :attrs {:a "b"}}
                          (list `template-macro (fn [x & _] x))))
        `(escaped (list "<hello a=\"b\">world" (escape-user-code xml-str ~'(some code)) "</hello>")))))

(deftemplate-macro do->
 "Chains (composes) several template-macros."
 [xml & forms]
  (reduce expand-til-template-macro xml forms))

;; simple template macros
(deftemplate-macro show [xml]
  xml)
     
(deftemplate-macro set-attr [xml & forms]
  (if (tag? xml)
    (let [attrs (reduce (fn [attrs [name form]] (assoc attrs name `(str ~form)))
                  (:attrs xml) (partition 2 forms))]
      (assoc xml :attrs attrs)) 
    xml))
     
(deftemplate-macro remove-attr [xml & attr-names]
  (if (tag? xml)
    (let [attrs (apply dissoc (:attrs xml) attr-names)]
      (assoc xml :attrs attrs)) 
    xml))

(deftemplate-macro xhtml-strict [xml & forms]
  `(escaped (list
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" 
     (apply-template-macro ~(assoc-in xml [:attrs :xmlns] "http://www.w3.org/1999/xhtml") (at ~@forms))))) 

;; selectors stuff
(defn- action [selector]
  (second selector))

(defn- transition-fn [selector]
  (first selector))

(defn- step-selector
 "Selectors are states in an infinite state machine
  and step-selector is their transition function.
  Returns [new-selector action-or-nil]." 
 [selector node]
  ((transition-fn selector) node))
  
(defn- run-selector
 "For testing purpose."
  [sel s]
   (action (reduce step-selector sel s)))
   
(def #^{:private true} null-selector
  [(fn [_] null-selector) false])
  
(defn- null-transition [_] null-selector)   

(with-test
  (defn- self-selector [pred action]
   "Returns a selector that matches only the current node (and it needs to 
    satisfy the supplied predicate."
    [(fn this [node]
       [null-transition (when (pred node) action)]) 
     false])
      
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
    [(fn this [node]
       [this (when (pred node) action)])
     false]) 
      
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
    ([& selectors]
      [(fn [node]
         (let [subselectors (map #(step-selector % node) selectors)]
           (apply merge-selectors subselectors)))
       (some action selectors)]))
  
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
        [(fn [node]
           (let [subselector (step-selector selector node)
                 chained-subselector (chain-selectors subselector next-selector)]
             (if (action subselector)
               (merge-selectors next-selector chained-subselector) 
               chained-subselector)))
         (when (action selector) (action next-selector))])))

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

;; selector helpers
(defn attr= [e attr value]
  (= value (-> e :attrs attr)))
  
(defn attr? [e & attrs]
  (let [a (:attrs e)]
    (every? #(get a %) attrs)))           
     
;; the "at" template-macro: allows to apply other template-macros to subtrees using selectors.

(declare transform-node)  

(defn- transform-tag [{:keys [content] :as node} selector]
  (let [transformed-node
         (assoc node :content 
           (vec (map #(transform-node % (step-selector selector %)) 
                  content)))] 
    (if-let [action (action selector)]
      `(apply-template-macro ~transformed-node ~action)
      transformed-node)))
      
(defn- transform-node [node selector]
  (if (tag? node)
    (transform-tag node selector)
    node))

(defn- keyword-pred [kw]
  (let [segments (.split (name kw) "(?=[#.])")
        preds (map (fn [#^String s] (condp = (first s)
                      \. #(-> % :attrs (:class "") (.split "\\s+") set (get (.substring s 1))) 
                      \# #(= (.substring s 1) (-> % :attrs :id))
                      #(= s (name (:tag %))))) segments)]
    (fn [x]
      (and (tag? x) (every? identity (map #(% x) preds))))))

(with-test
  (defn- compile-selector-step
   "Returns a predicate."
   [form]
    (cond 
      (seq? form) 
        (eval `(fn [x#] (~(first form) x# ~@(rest form))))
      (= :* form)
        (constantly true)
      (keyword? form)
        (keyword-pred form)
      (vector? form)
        (let [preds (map compile-selector-step form)]
          (fn [x] 
            (every? #(% x) preds))) 
      :else 
        (eval form)))
  
  ;; tests
  ;; - seqs
  (is ((compile-selector-step '(= :a)) :a))
  (is (not ((compile-selector-step '(= :a)) :b)))
  ;; - keywords
  (is ((compile-selector-step :a) {:tag :a :attrs nil :content nil}))
  (is (not ((compile-selector-step :a) {:tag :b :attrs nil :content nil})))
  (is (not ((compile-selector-step :a) :a)))
  ;; - vector
  (is ((compile-selector-step [:a]) {:tag :a :attrs nil :content nil}))
  (is (not ((compile-selector-step '[:a (= :a)]) 
             {:tag :a :attrs nil :content nil})))
  (is (not ((compile-selector-step '[:a (-> :attrs :href)]) 
             {:tag :a :attrs nil :content nil})))
  (is ((compile-selector-step '[:a (-> :attrs :href)]) 
        {:tag :a :attrs {:href "http://cgrand.net/"} :content nil}))
  ;; - else          
  (is (identical? (compile-selector-step 'identity) identity)))   

(defn- compile-selector
 "Evals a selector form. If the form is anything but a vector,
  it's simply evaluated. If the form is a vector, each element
  is expected to evaluate to a predicate on nodes.
  There's special rules for keywords (see keyword-pred) and
  lists ((a b c) yields #(a % b)).
  Predicates are chained in a hierarchical way � la CSS."
 [selector-form action]
  (cond
    (vector? selector-form)
      (loop [items (seq selector-form) 
             selector-type self-or-descendants-selector
             selectors []]
        (if-let [[x & xs] items]
          (if (= :> x)
            (recur xs self-selector selectors)
            (let [pred (compile-selector-step x)]
              (recur xs self-or-descendants-selector
                (conj selectors (selector-type pred action)))))
          (apply chain-selectors selectors)))
    (set? selector-form)
      (apply merge-selectors (map #(compile-selector % action) selector-form))
    :else
      (eval selector-form))) 
               
  
(deftemplate-macro at
 "Allows to apply other template-macros to subtrees using selectors." 
 [xml & forms]
  (let [selector (apply merge-selectors (map #(apply compile-selector %) 
                                          (partition 2 forms)))]
    (transform-node xml (step-selector selector xml))))

;; main macros
(defmacro template 
 ([path args form]
  (let [xml (load-html-resource path)]
    `(fn ~args (escaped (flatten (apply-template-macro ~xml ~form))))))
 ([path args form & forms] 
   `(template ~path ~args (at ~form ~@forms))))
  
(defmacro deftemplate
 "Defines a template as a function that returns a seq of strings." 
 [name path args & forms] 
  `(def ~name (template ~path ~args ~@forms)))
