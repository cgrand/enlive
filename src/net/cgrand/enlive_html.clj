;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html
  "enlive-html is a selector-based transformation and extraction engine."
  (:require [net.cgrand.xml :as xml])
  (:require [clojure.zip :as z])
  (:require [net.cgrand.enlive-html.state-machine :as sm]))

;; EXAMPLES: see net.cgrand.enlive-html.examples

;; HTML I/O stuff

(defn- startparse-tagsoup [s ch]
  (doto (org.ccil.cowan.tagsoup.Parser.)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/ignorable-whitespace" true)
    (.setContentHandler ch)
    (.setProperty "http://xml.org/sax/properties/lexical-handler" ch)
    (.parse s)))

(defn- load-html-resource 
 "Loads and parse an HTML resource and closes the stream."
 [stream] 
  (with-open [#^java.io.Closeable stream stream]
    (xml/parse (org.xml.sax.InputSource. stream) startparse-tagsoup)))

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
 [#^java.io.File file]
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

(defn- content-emitter [tag-name]
  (if (#{"script" "style"} tag-name) (fn [x] [(str x)]) emit))

(defn- emit-tag [tag]
  (let [name (-> tag :tag name)]
    (concat ["<" name]
      (emit-attrs (:attrs tag))
      (if-let [s (seq (:content tag))]
        (concat [">"] (mapcat (content-emitter name) s) ["</" name ">"])
        (if (*self-closing-tags* (:tag tag)) 
          [" />"]
          ["></" name ">"])))))

(defn- emit-comment [node]
  ["<!--" (str (:data node)) "-->"])
  
(defn- annotations [x]
  (-> x meta ::annotations))

(defn- emit [node]
  (cond 
    (xml/tag? node) ((:emit (annotations node) emit-tag) node)
    (xml/comment? node) (emit-comment node) 
    :else [(xml-str node)]))

(defn- emit-root [node]
  (if-let [preamble (-> node meta ::preamble)]
    (cons preamble (emit node))
    (emit node)))
  
(defn emit* [node-or-nodes]
  (if (xml/tag? node-or-nodes) (emit-root node-or-nodes) (mapcat emit-root node-or-nodes)))

(defn- tag-emitter [{:keys [tag content attrs] :as node}]
  (let [name (name tag)
        attrs-str (apply str (emit-attrs attrs))
        open (str "<" name attrs-str ">")
        close (str "</" name ">")
        empty [(if (*self-closing-tags* tag)
                 (str "<" name attrs-str " />")
                 (str open close))]
        open [open]
        close [close]
        emit (content-emitter name)
        full [(apply str (emit-tag node))]]
    (fn [elt]
      (cond
        (= node elt) full
        (and (= tag (:tag elt)) (= attrs (:attrs elt)))
          (if-let [content (seq (:content elt))]
            (concat open (mapcat emit content) close)
            empty)
        :else (emit-tag elt)))))

(defn- comment-emitter [{data :data :as node}]
  (let [s (apply str (emit-comment node))]
    #(if (= node %) s (emit-comment node))))

(defn annotate [node]
  (cond
    (xml/tag? node)
      (let [node (update-in node [:content] #(map annotate %))] 
        (vary-meta node assoc ::annotations {:emit (tag-emitter node)}))
    (xml/comment? node)
      (vary-meta node assoc ::annotations {:emit (comment-emitter node)})  
    :else node))
      
;; utilities

(defn- not-node? [x]
  (not (or (string? x) (map? x))))

(defn- flatten [x]
  (remove not-node? (tree-seq not-node? seq x)))
  
(defn flatmap [f xs]
  (flatten (map f xs)))

(defn attr-values 
 "Returns the whitespace-separated values of the specified attr as a set."
 [node attr]
  (disj (set (-> node :attrs (attr "") str (.split "\\s+"))) ""))

;; selector syntax
(defn- simplify-associative [[op & forms]]
  (if (next forms)
    (cons op (mapcat #(if (and (seq? %) (= op (first %))) (rest %) (list %)) forms)) 
    (first forms)))

(defn- emit-union [forms]
  (simplify-associative (cons `sm/union forms)))

(defn- emit-intersection [forms]
  (simplify-associative (cons `sm/intersection forms)))

(defn- emit-chain [forms]
  (simplify-associative (cons `sm/chain forms)))

(defn- compile-keyword [kw]
  (let [[[first-letter :as tag-name] :as segments] (.split (name kw) "(?=[#.])")
        tag-pred (when-not (contains? #{nil \* \# \.} first-letter) [`(tag= ~(keyword tag-name))])
        ids-pred (for [s segments :when (= \# (first s))] `(id= ~(subs s 1)))
        classes (set (for [s segments :when (= \. (first s))] (subs s 1)))
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
      (emit-chain [`sm/descendants-or-self next-chain])))) 

(defn compile-selector [s]
  (cond
    (set? s) (emit-union (map compile-selector s))
    (vector? s) (compile-chain s)
    :else s))

;; core 
  
(defn- children-locs [loc]
  (when (z/branch? loc) (take-while identity (iterate z/right (z/down loc)))))

(defn- transform-loc [loc previous-state transformation]
  (let [state (sm/step previous-state loc)
        children (flatmap #(transform-loc % state transformation) (children-locs loc))
        node (if (and (z/branch? loc) (not= children (z/children loc)))
                 (z/make-node loc (z/node loc) children) 
                 (z/node loc))]
    (if (sm/accept? state)
      (transformation node)
      node)))

(defn transform [nodes [state transformation]]
  (flatmap #(transform-loc (xml/xml-zip %) state (or transformation (constantly nil))) nodes))

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

(defn zip-select* [locs state]
  (let [select1 
         (fn select1 [loc previous-state] 
           (let [state (sm/step previous-state loc)]
             (concat (when (sm/accept? state) (list loc))
               (mapcat #(select1 % state) (children-locs loc)))))]
    (mapcat #(select1 % state) locs)))
      
(defn select* [nodes state]
  (map z/node (zip-select* (map xml/xml-zip nodes) state))) 
      
(defmacro select
 "Returns the seq of nodes and sub-nodes matched by the specified selector."
 [nodes selector]
  `(select* ~nodes (selector ~selector)))

(defmacro zip-select
 "Returns the seq of locs matched by the specified selector."
 [locs selector]
  `(zip-select* ~locs (selector ~selector)))

;; main macros

(defmacro transformation
 ([] `identity)
 ([form] form)
 ([form & forms] `(fn [node#] (at node# ~form ~@forms))))

(defmacro snippet* [nodes args & forms]
  `(let [nodes# (map annotate ~nodes)]
     (fn ~args
       (flatmap (transformation ~@forms) nodes#))))
    
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

;; transformations

(defn content
 "Replaces the content of the node. Values can be nodes or nested collection of nodes." 
 [& values]
  #(assoc % :content (flatten values)))

(defn html-content
 "Replaces the content of the node. Values are strings containing html code."
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
 "Removes the specified classes from the selected node." 
 [& classes]
  #(let [classes (apply disj (attr-values % :class) classes)
         attrs (:attrs %)
         attrs (if (empty? classes) 
                 (dissoc attrs :class) 
                 (assoc attrs :class (apply str (interpose \space classes))))]
     (assoc % :attrs attrs)))

(defn do->
 "Chains (composes) several transformations. Applies functions from left to right." 
 [& fns]
  #(reduce (fn [nodes f] (flatmap f nodes)) [%] fns))

(defmacro clone-for
 [comprehension & forms]
  `(fn [node#]
     (for ~comprehension ((transformation ~@forms) node#))))

(defn append
 "Appends the values to the actual content."
 [& values]
  #(assoc % :content (concat (:content %) (flatten values)))) 

(defn prepend
 "Prepends the values to the actual content."
 [& values]
  #(assoc % :content (concat (flatten values) (:content %)))) 

(defn after
 "Inserts the values after the current element."
 [& values]
  #(cons % (flatten values)))

(defn before
 "Inserts the values before the current element."
 [& values]
  #(concat (flatten values) [%]))

(defn substitute
 "Replaces the current element."
 [& values]
 (constantly (flatten values)))

(defmacro move
 "Takes all nodes (under the current element) matched by src-selector, removes
  them and combines them with the elements matched by dest-selector.
  By default, destination elements are replaced." 
 ([src-selector dest-selector] `(move ~src-selector ~dest-selector substitute))
 ([src-selector dest-selector combiner]
  `(fn [node#]
     (let [nodes# (select [node#] ~src-selector)]
       (at node#
         ~src-selector nil
         ~dest-selector (apply ~combiner nodes#)))))) 
     
(defn strict-mode* [node]
  (-> node
    (assoc-in [:attrs :xmlns] "http://www.w3.org/1999/xhtml")
    (vary-meta assoc ::preamble 
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")))

(defmacro strict-mode
 "Adds xhtml-transitional DTD to switch browser in 'strict' mode." 
 [& forms]
  `(do-> (transformation ~@forms) strict-mode*)) 

;; predicates utils
(defn zip-pred 
 "Turns a predicate function on elements locs into a predicate-step usable in selectors."
 [f]
  (sm/pred #(and (z/branch? %) (f %))))

(defn pred 
 "Turns a predicate function on elements into a predicate-step usable in selectors."
 [f]
  (zip-pred #(f (z/node %))))

(defn text-pred 
 "Turns a predicate function on strings (text nodes) into a predicate-step usable in selectors."
 [f]
  (sm/pred #(let [n (z/node %)] (and (string? n) (f n)))))

;; predicates
(def any (pred (constantly true)))

(defn tag= 
 "Selector predicate, :foo is as short-hand for (tag= :foo)."
 [tag-name]
  (pred #(= (:tag %) tag-name)))

(defn id=
 "Selector predicate, :#foo is as short-hand for (id= \"foo\")."
 [id]
  (pred #(= (-> % :attrs :id) id)))

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

(def #^{:doc "Selector predicate, tests if the specified attributes start with the specified values. See CSS ^= ."} 
 attr-starts
  (multi-attr-pred starts-with?))

(def #^{:doc "Selector predicate, tests if the specified attributes end with the specified values. See CSS $= ."} 
 attr-ends
  (multi-attr-pred ends-with?))

(def #^{:doc "Selector predicate, tests if the specified attributes contain the specified values. See CSS *= ."} 
 attr-contains
  (multi-attr-pred contains-substring?))

(defn- is-first-segment? [#^String s #^String segment]
  (and s 
    (.startsWith s segment)
    (= \- (.charAt s (count segment)))))
             
(def #^{:doc "Selector predicate, tests if the specified attributes start with the specified values. See CSS |= ."}
 attr|=           
  (multi-attr-pred is-first-segment?))

(def root 
  (zip-pred #(-> % z/up nil?)))

(defn- nth? 
 [f a b]
  (if (zero? a)
    #(= (-> (filter xml/tag? (f %)) count inc) b)
    #(let [an+b (-> (filter xml/tag? (f %)) count inc)
           an (- an+b b)]
       (and (zero? (rem an a)) (<= 0 (quot an a))))))

(defn nth-child
 "Selector step, tests if the node has an+b-1 siblings on its left. See CSS :nth-child."
 ([b] (nth-child 0 b))
 ([a b] (zip-pred (nth? z/lefts a b))))

(defn nth-last-child
 "Selector step, tests if the node has an+b-1 siblings on its right. See CSS :nth-last-child."
 ([b] (nth-last-child 0 b))
 ([a b] (zip-pred (nth? z/rights a b))))

(defn- filter-of-type [f]
  (fn [loc]
    (let [tag (-> loc z/node :tag)
          pred #(= (:tag %) tag)]
      (filter pred (f loc)))))

(defn nth-of-type
 "Selector step, tests if the node has an+b-1 siblings of the same type (tag name) on its left. See CSS :nth-of-type."
 ([b] (nth-of-type 0 b))
 ([a b] (zip-pred (nth? (filter-of-type z/lefts) a b))))
 
(defn nth-last-of-type
 "Selector step, tests if the node has an+b-1 siblings of the same type (tag name) on its right. See CSS :nth-last-of-type."
 ([b] (nth-last-of-type 0 b))
 ([a b] (zip-pred (nth? (filter-of-type z/rights) a b))))

(def first-child (nth-child 1))      
      
(def last-child (nth-last-child 1))      
      
(def first-of-type (nth-of-type 1))      
      
(def last-of-type (nth-last-of-type 1))      

(def only-child (sm/intersection first-child last-child))  

(def only-of-type (sm/intersection first-of-type last-of-type))

(def void (pred #(empty? (remove empty? (:content %)))))

(def odd (nth-child 2 1))

(def even (nth-child 2 0))

(defn- select? [nodes state]
  (boolean (seq (select* nodes state))))

(defn has* [state]
  (pred #(select? [%] state)))

(defmacro has
 "Selector predicate, matches elements which contain at least one element that matches the specified selector. See jQuery's :has" 
 [selector]
  `(has* (sm/chain any (selector ~selector))))

(defmacro but-node
 "Selector predicate, matches nodes which are rejected by the specified selector-step. See CSS :not" 
 [selector-step]
  `(sm/complement-next (selector-step ~selector-step)))

(defmacro but
 "Selector predicate, matches elements which are rejected by the specified selector-step. See CSS :not" 
 [selector-step]
  `(sm/intersection any (but-node ~selector-step)))

(defn left* [state]
 (sm/pred 
   #(when-let [sibling (first (filter xml/tag? (reverse (z/lefts %))))]
      (select? [sibling] state))))

(defmacro left 
 [selector-step]
  `(left* (selector-step ~selector-step)))

(defn lefts* [state]
 (sm/pred 
   #(select? (filter xml/tag? (z/lefts %)) state)))
  
(defmacro lefts
 [selector-step]
  `(lefts* (selector-step ~selector-step)))

(defn right* [state]
 (sm/pred 
   #(when-let [sibling (first (filter xml/tag? (z/rights %)))]
      (select? [sibling] state))))

(defmacro right 
 [selector-step]
  `(right* (selector-step ~selector-step)))

(defn rights* [state]
 (sm/pred 
   #(select? (filter xml/tag? (z/rights %)) state)))
  
(defmacro rights 
 [selector-step]
  `(rights* (selector-step ~selector-step)))

(def any-node (sm/pred (constantly true)))

(def text-node (sm/pred #(string? (z/node %))))

(def comment-node (sm/pred #(xml/comment? (z/node %)))) 