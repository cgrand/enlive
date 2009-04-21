;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html
  (:refer-clojure :exclude [empty complement])
  (:require [clojure.xml :as xml])
  (:require [clojure.zip :as z])
  (:use [clojure.contrib.test-is :as test-is :only [set-test with-test is are]]))

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

(defmulti html-resource "Loads an HTML resource, returns a seq of nodes." type)

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

(defn attr-values 
 "Returns the whitespace-separated values of the specified attr as a set."
 [node attr]
  (disj (set (-> node :attrs (attr "") (.split "\\s+"))) ""))

;; state machine stuff
;; a state is a pair consisting of a boolean (acceptance) and a seq of functions (functions from loc to state)

(def accept? first)

(defn hopeless?
 "Returns true if the state machine cannot succeed. (It's not a necessary condition.)" 
 [state] (empty? (second state)))

(defn step
 "Returns the next state."  
 [state loc]
  (let [states (map #(% loc) (second state))]
    [(some accept? states) (mapcat second states)]))    

(with-test 
  (defn union 
   "Returns a state machine which succeeds as soon as one of the specified state machines succeeds."
   [& states]
    [(some accept? states) (mapcat second states)])
       
  (is (accept? (step (union [false nil] [false [(constantly [true nil])]]) :a)))
  (is (not (accept? (step (union [false nil] [false [(constantly [false nil])]]) :a))))
  (is (accept? (step (union [false [(constantly [false nil])]] [false [(constantly [true nil])]]) :a)))
  (is (accept? (step (union [false [(constantly [true nil])]] [false [(constantly [true nil])]]) :a)))) 
  
(with-test
  (defn intersection
   "Returns a state machine which succeeds when all specified state machines succeed." 
   [& states]
    [(every? accept? states)
     (when (seq (remove hopeless? states))
       [(fn [loc] (apply intersection (map #(step % loc) states)))])])
       
  (is (not (accept? (step (intersection [false nil] [false [(constantly [true nil])]]) :a))))
  (is (not (accept? (step (intersection [false [(constantly [false nil])]] [false [(constantly [true nil])]]) :a))))
  (is (accept? (step (intersection [false [(constantly [true nil])]] [false [(constantly [true nil])]]) :a)))) 

(defn complement 
 [[x fs]]
  [(not x) (map (partial comp complement) fs)])

(defn complement-next
 [state]
  [(accept? state) (second (complement state))]) 

(with-test
  (defn chain 
    ([s] s)
    ([[x1 fns1] s2]
      (let [chained-fns1 (map #(fn [loc] (chain (% loc) s2)) fns1)]
        (if x1
          [(accept? s2) (concat (second s2) chained-fns1)]
          [false chained-fns1])))
    ([s1 s2 & etc] (reduce chain (chain s1 s2) etc)))
    
  (are (= _1 (boolean (accept? (reduce step (chain [false [#(vector (= :a %1) nil)]] [false [#(vector (= :b %1) nil)]])  _2))))
    true [:a :b]
    false [:a :c]
    false [:c :b]
    false [:a :a]
    false [:b :b]))

(def descendants-or-self
  [true (lazy-seq [(constantly descendants-or-self)])])


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
        tag-pred (when-not (#{"" "*"} tag-name) [`(tag= ~(keyword tag-name))])
        ids-pred (for [s etc :when (= \# (first s))] `(id= ~(subs s 1)))
        classes (set (for [s etc :when (= \. (first s))] (subs s 1)))
        class-pred (when (seq classes) [`(has-class ~@classes)])
        all-preds (concat tag-pred ids-pred class-pred)] 
    (emit-intersection (or (seq all-preds) [`any]))))
    
(declare compile-step)

(defn- compile-union [s]
  (emit-union (map compile-step s)))      
    
(defn- compile-intersection [s]
  (emit-intersection (map compile-step s)))      

(defn compile-step [s]
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
  (cond
    (set? s) (emit-union (map compile-selector s))
    (vector? s) (compile-chain s)
    :else s))

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
  (when transformation
    (flatmap #(transform-loc (z/xml-zip %) state transformation) nodes)))

(defn at* [nodes & rules]
  (reduce transform nodes (partition 2 rules)))

(defmacro selector
 "Turns the selector into clojure code." 
 [selector]
  (compile-selector selector))

(defmacro selector-step
 "Turns the selector step into clojure code." 
 [selector-step]
  (compile-step selector-step))

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
      
(defmacro select
 "Returns the seq of nodes and sub-nodes matched by the specified selector."
 [nodes selector]
  `(select* ~nodes (selector ~selector)))

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

;; test utilities
(defn- htmlize [node]
  (cond
    (map? node)
      (-> node
        (assoc-in [:attrs :class] (attr-values node :class))
        (update-in [:content] htmlize))
    (or (coll? node) (seq? node))
      (map htmlize node)
    :else node))

(defn- html [s]
  (htmlize 
    (if (string? s)
      (html-resource (java.io.StringReader. s))
      s))) 

(defn- src [s]
  (first (html-resource (java.io.StringReader. s))))

(defn- same? [& xs]
  (apply = (map html xs)))

(defn- elt 
 ([tag] (elt tag nil))
 ([tag attrs & content]
   {:tag tag
    :attrs attrs
    :content content}))

(defmacro #^{:private true} 
 is-same
 [& forms]
 `(is (same? ~@forms)))

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

(defn wrap 
 ([tag] (wrap tag nil))
 ([tag attrs]
   #(array-map :tag tag :attrs attrs :content [%])))

(def unwrap :content)

(defn set-attr
 "Assocs attributes on the selected node."
 [& kvs]
  #(assoc % :attrs (apply assoc (:attrs % {}) kvs)))
     
(defn remove-attr 
 "Dissocs attributes on the selected node."
 [& attr-names]
  #(assoc % :attrs (apply dissoc (:attrs %) attr-names)))
    
(defn add-class
 "Adds the specified classes to the selected node." 
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


;; predicates utils
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

;; predicates
(defn- test-step [expected state node]
  (= expected (boolean (accept? (step state (z/xml-zip node))))))

(def any (pred (constantly true)))

(with-test
  (defn tag= 
   "Selector predicate, :foo is as short-hand for (tag= :foo)."
   [tag-name]
    (pred #(= (:tag %) tag-name)))
    
  (are (test-step _1 _2 _3)
    true (tag= :foo) (elt :foo)
    false (tag= :bar) (elt :foo)))

(with-test
  (defn id=
   "Selector predicate, :#foo is as short-hand for (id= \"foo\")."
   [id]
    (pred #(= (-> % :attrs :id) id)))

  (are (test-step _1 _2 _3)
    true (id= "foo") (elt :div {:id "foo"})
    false (id= "bar") (elt :div {:id "foo"})
    false (id= "foo") (elt :div)))

(with-test  
  (defn attr? 
   "Selector predicate, tests if the specified attributes are present."
   [& kws]
    (pred #(every? (-> % :attrs keys set) kws)))

  (are (test-step _1 _2 _3)
    true (attr? :href) (elt :a {:href "http://cgrand.net/"})
    false (attr? :href) (elt :a {:name "toc"})
    false (attr? :href :title) (elt :a {:href "http://cgrand.net/"})
    true (attr? :href :title) (elt :a {:href "http://cgrand.net/" :title "home"})))
  
(defn- every?+ [pred & colls]
  (every? #(apply pred %) (apply map vector colls))) 

(defn- multi-attr-pred 
 [single-attr-pred]
  (fn [& kvs]
    (let [ks (take-nth 2 kvs)
          vs (take-nth 2 (rest kvs))]
      (pred #(when-let [attrs (:attrs %)]
               (every?+ single-attr-pred (map attrs ks) vs))))))           

(with-test
  (def #^{:doc "Selector predicate, tests if the specified attributes have the specified values."} 
   attr= 
    (multi-attr-pred =))
    
  (are (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr= :href "http://cgrand.net/")
    false (attr= :href "http://clojure.org/")
    false (attr= :href "http://cgrand.net/" :name "home") 
    false (attr= :href "http://cgrand.net/" :title "homepage")
    true (attr= :href "http://cgrand.net/" :title "home")))

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

(with-test
  (def #^{:doc "Selector predicate, tests if the specified attributes start with the specified values. See CSS ^= ."} 
   attr-starts
    (multi-attr-pred starts-with?))

  (are (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr-starts :href "http://cgr")
    false (attr-starts :href "http://clo")
    false (attr-starts :href "http://cgr" :name "ho")
    false (attr-starts :href "http://cgr" :title "x") 
    true (attr-starts :href "http://cgr" :title "ho")))

(with-test
  (def #^{:doc "Selector predicate, tests if the specified attributes end with the specified values. See CSS $= ."} 
   attr-ends
    (multi-attr-pred ends-with?))

  (are (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr-ends :href "d.net/")
    false (attr-ends :href "e.org/")
    false (attr-ends :href "d.net/" :name "me")
    false (attr-ends :href "d.net/" :title "hom")
    true (attr-ends :href "d.net/" :title "me")))

(with-test
  (def #^{:doc "Selector predicate, tests if the specified attributes contain the specified values. See CSS *= ."} 
   attr-contains
    (multi-attr-pred contains-substring?))
    
  (are (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr-contains :href "rand")
    false (attr-contains :href "jure")
    false (attr-contains :href "rand" :name "om") 
    false (attr-contains :href "rand" :title "pa")
    true (attr-contains :href "rand" :title "om")))

(defn- is-first-segment? [#^String s #^String segment]
  (and s 
    (.startsWith s segment)
    (= \- (.charAt s (count segment)))))
             
(def #^{:doc "Selector predicate, tests if the specified attributes start with the specified values. See CSS |= ."}
 attr|=           
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

(with-test      
  (defn nth-child
   "Selector step, tests if the node has an+b-1 siblings on its left. See CSS :nth-child."
   ([b] (nth-child 0 b))
   ([a b] (loc-pred (nth? z/lefts a b))))

  (are (same? _2 (at (src "<dl><dt>1<dt>2<dt>3<dt>4<dt>5") _1 (add-class "foo")))    
    [[:dt (nth-child 2)]] "<dl><dt>1<dt class=foo>2<dt>3<dt>4<dt>5" 
    [[:dt (nth-child 2 0)]] "<dl><dt>1<dt class=foo>2<dt>3<dt class=foo>4<dt>5" 
    [[:dt (nth-child 3 1)]] "<dl><dt class=foo>1<dt>2<dt>3<dt class=foo>4<dt>5" 
    [[:dt (nth-child -1 3)]] "<dl><dt class=foo>1<dt class=foo>2<dt class=foo>3<dt>4<dt>5" 
    [[:dt (nth-child 3 -1)]] "<dl><dt>1<dt class=foo>2<dt>3<dt>4<dt class=foo>5"))
      
(defn nth-last-child
 "Selector step, tests if the node has an+b-1 siblings on its right. See CSS :nth-last-child."
 ([b] (nth-last-child 0 b))
 ([a b] (loc-pred (nth? z/rights a b))))

(defn- filter-of-type [f]
  (fn [loc]
    (let [tag (-> loc z/node :tag)
          pred #(= (:tag %) tag)]
      (filter pred (f loc)))))

(defn nth-of-type
 "Selector step, tests if the node has an+b-1 siblings of the same type (tag name) on its left. See CSS :nth-of-type."
 ([b] (nth-of-type 0 b))
 ([a b] (loc-pred (nth? (filter-of-type z/lefts) a b))))

(defn nth-last-of-type
 "Selector step, tests if the node has an+b-1 siblings of the same type (tag name) on its right. See CSS :nth-last-of-type."
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

(defn- select? [nodes state]
  (boolean (seq (select* nodes state))))

(defn has* [state]
  (pred #(select? [%] state)))

(with-test
  (defmacro has
   "Selector predicate, matches elements which contain at least one element that matches the specified selector. See jQuery's :has" 
   [selector]
    `(has* (chain any (selector ~selector))))
    
  (is-same "<div><p>XXX<p class='ok'><a>link</a><p>YYY" 
    (at (src "<div><p>XXX<p><a>link</a><p>YYY") 
      [[:p (has [:a])]] (add-class "ok"))))

(with-test
  (defmacro but
   "Selector predicate, matches elements which are rejected by the specified selector-step. See CSS :not" 
   [selector-step]
    `(complement-next (selector-step ~selector-step)))
    
  (is-same "<div><p>XXX<p><a class='ok'>link</a><p>YYY" 
    (at (src "<div><p>XXX<p><a>link</a><p>YYY") 
      [:div (but :p)] (add-class "ok")))
      
  (is-same "<div><p class='ok'>XXX<p><a>link</a><p class='ok'>YYY" 
    (at (src "<div><p>XXX<p><a>link</a><p>YYY") 
      [[:p (but (has [:a]))]] (add-class "ok"))))

(defn left* [state]
 (loc-pred 
   #(when-let [sibling (first (filter map? (reverse (z/lefts %))))]
      (select? [sibling] state))))

(with-test
  (defmacro left 
   [selector-step]
    `(left* (selector-step ~selector-step)))

  (are (same? _2 (at (src "<h1>T1<h2>T2<h3>T3<p>XXX") _1 (add-class "ok"))) 
    [[:h3 (left :h2)]] "<h1>T1<h2>T2<h3 class=ok>T3<p>XXX" 
    [[:h3 (left :h1)]] "<h1>T1<h2>T2<h3>T3<p>XXX" 
    [[:h3 (left :p)]] "<h1>T1<h2>T2<h3>T3<p>XXX"))

(defn lefts* [state]
 (loc-pred 
   #(select? (filter map? (z/lefts %)) state)))
  
(with-test
  (defmacro lefts
   [selector-step]
    `(lefts* (selector-step ~selector-step)))
  
  (are (same? _2 (at (src "<h1>T1<h2>T2<h3>T3<p>XXX") _1 (add-class "ok"))) 
    [[:h3 (lefts :h2)]] "<h1>T1<h2>T2<h3 class=ok>T3<p>XXX" 
    [[:h3 (lefts :h1)]] "<h1>T1<h2>T2<h3 class=ok>T3<p>XXX" 
    [[:h3 (lefts :p)]] "<h1>T1<h2>T2<h3>T3<p>XXX")) 
      

(defn right* [state]
 (loc-pred 
   #(when-let [sibling (first (filter map? (z/rights %)))]
      (select? [sibling] state))))

(with-test
  (defmacro right 
   [selector-step]
    `(right* (selector-step ~selector-step)))

  (are (same? _2 (at (src "<h1>T1<h2>T2<h3>T3<p>XXX") _1 (add-class "ok"))) 
    [[:h2 (right :h3)]] "<h1>T1<h2 class=ok>T2<h3>T3<p>XXX" 
    [[:h2 (right :p)]] "<h1>T1<h2>T2<h3>T3<p>XXX" 
    [[:h2 (right :h1)]] "<h1>T1<h2>T2<h3>T3<p>XXX")) 

(defn rights* [state]
 (loc-pred 
   #(select? (filter map? (z/rights %)) state)))
  
(with-test
  (defmacro rights 
   [selector-step]
    `(rights* (selector-step ~selector-step)))
  
  (are (same? _2 (at (src "<h1>T1<h2>T2<h3>T3<p>XXX") _1 (add-class "ok"))) 
    [[:h2 (rights :h3)]] "<h1>T1<h2 class=ok>T2<h3>T3<p>XXX" 
    [[:h2 (rights :p)]] "<h1>T1<h2 class=ok>T2<h3>T3<p>XXX" 
    [[:h2 (rights :h1)]] "<h1>T1<h2>T2<h3>T3<p>XXX")) 
  