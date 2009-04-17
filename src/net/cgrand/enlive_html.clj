;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html
  (:refer-clojure :exclude [empty])
  (:require [clojure.xml :as xml])
  (:require [clojure.zip :as z])
  (:use [clojure.contrib.test-is :as test-is :only [set-test with-test is]]))

;; enlive-html is a selector-based templating engine
;;
;; EXAMPLES: see net.cgrand.enlive-html.examples

;; HTML I/O stuff

(defn- startparse-tagsoup [s ch]
  (let [p (org.ccil.cowan.tagsoup.Parser.)]
    (.setFeature p "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
    (.setFeature p "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
    (.setContentHandler p ch)
    (.parse p s)))

(defn- load-html-resource 
 "Loads and parse an HTML resource and closes the stream."
 [stream] 
  (list 
    (with-open [stream stream]
      (xml/parse (org.xml.sax.InputSource. stream) startparse-tagsoup))))

(defmulti html-resource type)

(defmethod html-resource clojure.lang.IPersistentMap
 [xml-data]
  (list xml-data))

(defmethod html-resource clojure.lang.IPersistentCollection
 [nodes]
  (seq nodes))

(defmethod html-resource String
 [path]
  (load-html-resource (-> (clojure.lang.RT/baseLoader) (.getResourceAsStream path))))

(defmethod html-resource java.io.File
 [file]
  (load-html-resource (java.io.FileInputStream. file)))

(defmethod html-resource java.io.Reader
 [reader]
  (load-html-resource reader))

(defmethod html-resource java.io.InputStream
 [stream]
  (load-html-resource stream))

(defmethod html-resource java.net.URL
 [#^java.net.URL url]
  (load-html-resource (.getContent url)))

(defmethod html-resource java.net.URI
 [#^java.net.URI uri]
  (html-resource (.toURL uri)))


(defn- xml-str
 "Like clojure.core/str but escapes < > and &."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))
  
(defn- attr-str
 "Like clojure.core/str but escapes < > & and \"."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;") (.replace "\"" "&quot;")))

(def *self-closing-tags* #{:area :base :basefont :br :hr :input :img :link :meta})

(declare emit)

(defn- emit-attrs [attrs]
  (mapcat (fn [[k v]]
            [" " (name k) "=\"" (attr-str v) "\""]) attrs))

(defn- emit-tag [tag]
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
  
(defn flatmap [f xs]
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
   (when-let [states (seq (remove hopeless? states))] 
     [(fn [loc] (apply intersection (map #(step % loc) states)))])]) 

(defn chain 
  ([s] s)
  ([[x1 fns1] s2]
    (let [chained-fns1 (map #(fn [loc] (chain (% loc) s2)) fns1)]
      (if x1
        [(accept? s2) (concat (second s2) chained-fns1)]
        [false chained-fns1])))
  ([s1 s2 & etc] (reduce chain (chain s1 s2) etc)))

(def descendants-or-self
  [true (lazy-seq [(constantly descendants-or-self)])])

(def accept [true nil])

(defn pred 
 "Turns a predicate function on elements into a predicate-step usable in selectors."
 [f]
  [false [(fn [loc]
            [(and (z/branch? loc) (f (z/node loc))) nil])]])

(defn loc-pred 
 "Turns a predicate function on locs (see clojure.core.zip) into a predicate-step usable in selectors."
 [f]
  [false [(fn [loc]
            [(and (z/branch? loc) (f loc)) nil])]])

(defn tag= 
 "Selector predicate, :foo is as short-hand for (tag= :foo)."
 [tag-name]
  (pred #(= (:tag %) tag-name)))

(defn id=
 "Selector predicate, :#foo is as short-hand for (id= \"foo\")."
 [id]
  (pred #(= (-> % :attrs :id) id)))

(defn attr-values 
 "Returns the whitespace-separated values of the specified attr as a set."
 [node attr]
  (set (-> node :attrs (attr "") (.split "\\s+"))))

(defn attr? 
 "Selector predicate, tests if the specified attributes are present."
 [& kws]
  (pred #(every? (-> % :attrs keys set) kws)))
  
(defn- every?+ [pred & colls]
  (every? #(apply pred %) (apply map vector colls))) 

(defn- multi-attr-pred 
 [single-attr-pred]
  (fn [& kvs]
    (let [ks (take-nth 2 kvs)
          vs (take-nth 2 (rest kvs))]
      (pred #(when-let [attrs (:attrs %)]
               (every?+ single-attr-pred (map attrs ks) vs))))))           

(def #^{:doc "Selector predicate, tests if the specified attributes have the specified values."} 
 attr= 
  (multi-attr-pred =))

(defn attr-has
 "Selector predicate, tests if the specified whitespace-seperated attribute contains the specified values. See CSS ~="
 [attr & values]
  (pred #(every? (attr-values % attr) values)))
 
(defn has-class 
 "Selector predicate, :.foo.bar is as short-hand for (has-class \"foo\" \"bar\")."
 [& classes]
  (apply attr-has :class classes)) 

(defn- starts-with? [#^String s #^String prefix]
  (and s (.startsWith s prefix)))

(defn- ends-with? [#^String s #^String suffix]
  (and s (.endsWith s suffix)))

(defn- contains-substring? [#^String s #^String substring]
  (and s (<= 0 (.indexOf s substring))))

(def attr-starts
  (multi-attr-pred starts-with?))

(def attr-ends
  (multi-attr-pred ends-with?))

(def attr-contains
  (multi-attr-pred contains-substring?))

(defn- is-first-segment? [#^String s #^String segment]
  (and s 
    (.startsWith s segment)
    (= \- (.charAt s (count segment)))))
             
(def attr|=           
  (multi-attr-pred is-first-segment?))

(def root 
  (loc-pred #(-> % z/up nil?)))

(defn- nth? 
 [f a b]
  (if (zero? a)
    #(= (-> (f %) count inc) b)
    #(let [an+b (-> (filter map? (f %)) count inc)
           an (- an+b b)]
       (and (zero? (rem an a)) (<= 0 (quot an a))))))
      
(defn nth-child
 ([b] (nth-child 0 b))
 ([a b] (loc-pred (nth? z/lefts a b))))
      
(defn nth-last-child
 ([b] (nth-last-child 0 b))
 ([a b] (loc-pred (nth? z/rights a b))))

(defn- filter-of-type [f]
  (fn [loc]
    (let [tag (-> loc z/node :tag)
          pred #(= (:tag %) tag)]
      (filter pred (f loc)))))

(defn nth-of-type
 ([b] (nth-of-type 0 b))
 ([a b] (loc-pred (nth? (filter-of-type z/lefts) a b))))

(defn nth-last-of-type
 ([b] (nth-last-of-type 0 b))
 ([a b] (loc-pred (nth? (filter-of-type z/rights) a b))))

(def first-child (nth-child 1))      
      
(def last-child (nth-last-child 1))      
      
(def first-of-type (nth-of-type 1))      
      
(def last-of-type (nth-last-of-type 1))      

(def only-child (intersection first-child last-child))  

(def only-of-type (intersection first-of-type last-of-type))

(def empty (pred #(empty? (remove empty? (:content %)))))

(def odd (nth-child 2 1))

(def even (nth-child 2 0))

;; selector syntax
(defn- simplify-associative [[op & forms]]
  (if (next forms)
    (cons op (mapcat #(if (and (seq? %) (= op (first %))) (rest %) (list %)) forms)) 
    (first forms)))

(defn- emit-union [forms]
  (simplify-associative (cons `union forms)))

(defn- emit-intersection [forms]
  (simplify-associative (cons `intersection forms)))

(defn- emit-chain [forms]
  (simplify-associative (cons `chain forms)))

(defn- compile-keyword [kw]
  (let [[tag-name & etc] (.split (name kw) "(?=[#.])")
        tag-pred (if (#{"" "*"} tag-name) [] [`(tag= ~(keyword tag-name))])
        ids-pred (for [s etc :when (= \# (first s))] `(id= ~(subs s 1)))
        classes (set (for [s etc :when (= \. (first s))] (subs s 1)))
        class-pred (when (seq classes) [`(has-class ~@classes)])] 
    (emit-intersection (concat tag-pred ids-pred class-pred))))
    
(declare compile-step)

(defn- compile-union [s]
  (emit-union (map compile-step s)))      
    
(defn- compile-intersection [s]
  (emit-intersection (map compile-step s)))      

(defn- compile-step [s]
  (cond
    (keyword? s) (compile-keyword s)    
    (set? s) (compile-union s)    
    (vector? s) (compile-intersection s)
    :else s))

(defn- compile-chain [s]
  (let [[child-ops [step & next-steps :as steps]] (split-with #{:>} s)
        next-chain (when (seq steps)
                     (if (seq next-steps)
                       (emit-chain [(compile-step step) (compile-chain next-steps)])
                       (compile-step step)))]
    (if (seq child-ops)
      next-chain      
      (emit-chain [`descendants-or-self next-chain])))) 

(defn compile-selector [s]
  (if (set? s)
    (emit-union (map compile-selector s)) 
    (compile-chain s)))

;; core 
  
(defn- children-locs [loc]
  (take-while identity (iterate z/right (z/down loc))))

(defn- transform-loc [loc previous-state transformation]
  (if (z/branch? loc)
    (let [state (step previous-state loc)
          children (flatmap #(transform-loc % state transformation) (children-locs loc))
          node (if (= children (z/children loc)) 
                 (z/node loc) 
                 (z/make-node loc (z/node loc) children))]
      (if (accept? state)
        (transformation node)
        node))
    (z/node loc)))
      
(defn transform [nodes [state transformation]]
  (flatmap #(transform-loc (z/xml-zip %) state transformation) nodes))

(defn at* [nodes & rules]
  (reduce transform nodes (partition 2 rules)))

(defmacro selector [selector]
  (compile-selector selector))

(defmacro at [node & rules]
  `(at* [~node] ~@(map #(%1 %2) (cycle [#(list `selector %) identity]) rules)))

(defn select* [nodes state]
  (let [select1 
         (fn select1 [loc previous-state] 
           (let [state (step previous-state loc)]
             (if (accept? state)
               (list (z/node loc))
               (mapcat #(select1 % state) (children-locs loc)))))]
    (mapcat #(select1 (z/xml-zip %) state) nodes)))
      
(defmacro select [nodes selector]
  `(select* ~nodes (selector ~selector)))

;; transformations

(defn content
 "Replaces the content of the node. Values can be nodes or nested collection of nodes." 
 [& values]
  #(assoc % :content (flatten values)))

(defn html-content
 "Replaces the content of the node. Values are string of html."
 [& values]
  #(let [content (-> (apply str "<bogon>" values) java.io.StringReader. html-resource first :content)]
     (assoc % :content content))) 

(defn set-attr
 [& kvs]
  #(assoc % :attrs (apply assoc (:attrs % {}) kvs)))
     
(defn remove-attr 
 [xml & attr-names]
  #(assoc % :attrs (apply dissoc (:attrs %) attr-names)))
    
(defn add-class 
 [& classes]
  #(let [classes (into (attr-values % :class) classes)]
     (assoc-in % [:attrs :class] (apply str (interpose \space classes)))))

(defn remove-class 
 [& classes]
  #(let [classes (apply disj (attr-values % :class) classes)
         attrs (:attrs %)
         attrs (if (empty? classes) 
                 (dissoc attrs :class) 
                 (assoc attrs :class (apply str (interpose \space classes))))]
     (assoc % :attrs attrs)))

(comment deftemplate-macro xhtml-strict [xml & forms]
  `(escaped (list
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" 
     (apply-template-macro ~(assoc-in xml [:attrs :xmlns] "http://www.w3.org/1999/xhtml") (at ~@forms))))) 

(defn do->
 "Chains (composes) several transformations. Applies functions from left to right." 
 [& fns]
  #(reduce (fn [nodes f] (flatmap f nodes)) [%] fns))

;; main macros
(defmacro snippet* [nodes args & forms]
  (let [transform (if (next forms) `(fn [node#] (at node# ~@forms)) (first forms))]
    `(let [nodes# ~nodes]
       (fn ~args
         (flatmap ~transform nodes#)))))
    
(defmacro snippet 
 "A snippet is a function that returns a seq of nodes."
 [source selector args & forms]
  `(snippet* (select (html-resource ~source) ~selector) ~args ~@forms))  

(defmacro template 
 "A template returns a seq of string."
 ([source args & forms]
   `(comp emit* (snippet* (html-resource ~source) ~args ~@forms))))

(defmacro defsnippet
 "Define a named snippet -- equivalent to (def name (snippet source selector args ...))."
 [name source selector args & forms]
 `(def ~name (snippet ~source ~selector ~args ~@forms)))
   
(defmacro deftemplate
 "Defines a template as a function that returns a seq of strings." 
 [name source args & forms] 
  `(def ~name (template ~source ~args ~@forms)))

(defmacro defsnippets
 [source & specs]
  (let [xml (html-resource source)]
   `(do
     ~@(map (fn [[name selector args & forms]]
              `(def ~name (snippet ~xml ~selector ~args ~@forms)))
         specs))))
