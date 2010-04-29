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
  (:refer-clojure :exclude [flatten])
  (:require [net.cgrand.xml :as xml])
  (:require [clojure.zip :as z]))

;; EXAMPLES: see net.cgrand.enlive-html.examples

(defn- mapknit 
 ([f coll]
   (mapknit f coll nil))
 ([f coll etc]
  (lazy-seq
    (if (seq coll)
      (f (first coll) (mapknit f (rest coll) etc))
      etc))))

(defn- iterate-while 
 ([f x]
  (lazy-seq (when x (cons x (iterate-while f (f x)))))) 
 ([f x pred]
  (lazy-seq (when (pred x) (cons x (iterate-while f (f x) pred)))))) 
  
    
;; HTML I/O stuff

(defn- startparse-tagsoup [s ch]
  (doto (org.ccil.cowan.tagsoup.Parser.)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/ignorable-whitespace" true)
    (.setContentHandler ch)
    (.setProperty "http://www.ccil.org/~cowan/tagsoup/properties/auto-detector"
      (proxy [org.ccil.cowan.tagsoup.AutoDetector] []
        (autoDetectingReader [#^java.io.InputStream is]
          (java.io.InputStreamReader. is "UTF-8"))))
    (.setProperty "http://xml.org/sax/properties/lexical-handler" ch)
    (.parse s)))

(defn- load-html-resource 
 "Loads and parse an HTML resource and closes the stream."
 [stream]
  (filter map?
    (with-open [#^java.io.Closeable stream stream]
      (xml/parse (org.xml.sax.InputSource. stream) startparse-tagsoup))))

(defn- load-xml-resource 
 "Loads and parse a XML resource and closes the stream."
 [stream] 
  (with-open [#^java.io.Closeable stream stream]
    (xml/parse (org.xml.sax.InputSource. stream))))

(defmulti #^{:arglists '([resource loader])} get-resource 
 "Loads a resource, using the specified loader. Returns a seq of nodes." 
 (fn [res _] (type res)))

(defn html-resource 
 "Loads an HTML resource, returns a seq of nodes."
 [resource]
  (get-resource resource load-html-resource))

(defn xml-resource 
 "Loads an XML resource, returns a seq of nodes."
 [resource]
  (get-resource resource load-xml-resource))

(defmethod get-resource clojure.lang.IPersistentMap
 [xml-data _]
  (list xml-data))

(defmethod get-resource clojure.lang.IPersistentCollection
 [nodes _]
  (seq nodes))

(defmethod get-resource String
 [path loader]
  (-> (clojure.lang.RT/baseLoader) (.getResourceAsStream path) loader))

(defmethod get-resource java.io.File
 [#^java.io.File file loader]
  (loader (java.io.FileInputStream. file)))

(defmethod get-resource java.io.Reader
 [reader loader]
  (loader reader))

(defmethod get-resource java.io.InputStream
 [stream loader]
  (loader stream))

(defmethod get-resource java.net.URL
 [#^java.net.URL url loader]
  (loader (.getContent url)))

(defmethod get-resource java.net.URI
 [#^java.net.URI uri loader]
  (get-resource (.toURL uri) loader))


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

(defn- emit-attrs [attrs etc]
  (mapknit (fn [[k v] etc]
             (list* " " (name k) "=\"" (attr-str v) "\"" etc)) attrs etc))

(defn- content-emitter [tag-name]
  (if (#{"script" "style"} tag-name) (fn [x etc] (cons (str x) etc)) emit))

(defn- emit-tag [tag etc]
  (let [name (-> tag :tag name)
        etc (if-let [s (seq (:content tag))]
              (->> etc (list* "</" name ">") 
                (mapknit (content-emitter name) s)
                (cons ">")) 
              (if (*self-closing-tags* (:tag tag)) 
                (cons " />" etc)
                (list* "></" name ">" etc)))
        etc (emit-attrs (:attrs tag) etc)]
    (list* "<" name etc)))

(defn- emit-comment [node etc]
  (list* "<!--" (str (:data node)) "-->" etc))
  
(defn- annotations [x]
  (-> x meta ::annotations))

(defn- emit [node etc]
  (cond 
    (xml/tag? node) ((:emit (annotations node) emit-tag) node etc)
    (xml/comment? node) (emit-comment node etc) 
    :else (cons (xml-str node) etc)))

(defn- emit-root [node etc]
  (if-let [[name public-id system-id] (-> node meta ::xml/dtd)]
    (let [preamble (cond
                     public-id 
                       (str "<!DOCTYPE " name " PUBLIC \"" public-id "\"\n    \"" system-id "\">\n")
                     system-id   
                       (str "<!DOCTYPE " name " SYSTEM \"" system-id "\">\n")
                     :else
                       (str "<!DOCTYPE " name ">\n"))]
      (cons preamble (emit node etc)))
    (emit node etc)))
  
(defn emit* [node-or-nodes]
  (if (xml/tag? node-or-nodes) (emit-root node-or-nodes nil) (mapknit emit-root node-or-nodes)))

(defn- tag-emitter [{:keys [tag content attrs] :as node}]
  (let [name (name tag)
        attrs-str (apply str (emit-attrs attrs nil))
        open (str "<" name attrs-str ">")
        close (str "</" name ">")
        empty (if (*self-closing-tags* tag)
                (str "<" name attrs-str " />")
                (str open close))
        emit (content-emitter name)
        full (apply str (emit-tag node nil))]
    (fn [elt etc]
      (cond
        (= node elt) (cons full etc)
        (and (= tag (:tag elt)) (= attrs (:attrs elt)))
          (if-let [content (seq (:content elt))]
            (->> etc (cons close) (mapknit emit content) (cons open))
            (cons empty etc))
        :else (emit-tag elt etc)))))

(defn- comment-emitter [{data :data :as node}]
  (let [s (apply str (emit-comment node nil))]
    #(if (= node %) (cons s %2) (emit-comment node %2))))

(defn annotate [node]
  (cond
    (xml/tag? node)
      (let [node (update-in node [:content] #(map annotate %))] 
        (vary-meta node assoc ::annotations {:emit (tag-emitter node)}))
    (xml/comment? node)
      (vary-meta node assoc ::annotations {:emit (comment-emitter node)})  
    :else node))
      
;; utilities

(defn- node? [x]
  (or (string? x) (map? x)))

(defn as-nodes [node-or-nodes]
  (if (node? node-or-nodes)
    [node-or-nodes] 
    node-or-nodes))

(defn flatten [x]
  (letfn [(flat* [x stack]
            (if (node? x) 
              (cons x (when (seq stack) (flat (peek stack) (pop stack))))
              (if-let [[x & xs] (seq x)]
                (recur x (conj stack xs))
                (when (seq stack)
                  (recur (peek stack) (pop stack))))))
          (flat [x stack]
            (lazy-seq (flat* x stack)))]
    (flat x ())))

(defn flatmap [f node-or-nodes]
  (flatten (map f (as-nodes node-or-nodes))))

(defn attr-values 
 "Returns the whitespace-separated values of the specified attr as a set or nil."
 [node attr]
  (when-let [v (-> node :attrs (get attr))]
    (set (re-seq #"\S+" v))))

;; predicates utils
(defn zip-pred 
 "Turns a predicate function on elements locs into a predicate-step usable in selectors."
 [f]
  #(and (z/branch? %) (f %)))

(defn pred 
 "Turns a predicate function on elements into a predicate-step usable in selectors."
 [f]
  (zip-pred #(f (z/node %))))

(defn text-pred 
 "Turns a predicate function on strings (text nodes) into a predicate-step usable in selectors."
 [f]
  #(let [n (z/node %)] (and (string? n) (f n))))

(defn re-pred 
 "Turns a predicate function on strings (text nodes) into a predicate-step usable in selectors."
 [re]
  (text-pred #(re-matches re %)))

(def whitespace (re-pred #"\s*"))

;; core predicates
(def any (pred (constantly true)))

(defn tag= 
 "Selector predicate, :foo is as short-hand for (tag= :foo)."
 [tag-name]
  (pred #(= (:tag %) tag-name)))

(defn id=
 "Selector predicate, :#foo is as short-hand for (id= \"foo\")."
 [id]
  (pred #(= (-> % :attrs :id) id)))

(defn attr-has
 "Selector predicate, tests if the specified whitespace-seperated attribute contains the specified values. See CSS ~="
 [attr & values]
  (pred #(when-let [v (attr-values % attr)] (every? v values))))
 
(defn has-class 
 "Selector predicate, :.foo.bar is as short-hand for (has-class \"foo\" \"bar\")."
 [& classes]
  (apply attr-has :class classes))
   
;; selector syntax
(defn intersection [preds]
  (condp = (count preds)
    1 (first preds)
    2 (let [[f g] preds] #(and (f %) (g %)))
    3 (let [[f g h] preds] #(and (f %) (g %) (h %)))
    4 (let [[f g h k] preds] #(and (f %) (g %) (h %) (k %)))
    (fn [x] (every? #(% x) preds))))

(defn union [preds]
  (condp = (count preds)
    1 (first preds)
    2 (let [[f g] preds] #(or (f %) (g %)))
    3 (let [[f g h] preds] #(or (f %) (g %) (h %)))
    4 (let [[f g h k] preds] #(or (f %) (g %) (h %) (k %)))
    (fn [x] (some #(% x) preds))))

(def #^{:private true} compile-keyword 
  (memoize 
    (fn [kw]
      (if (= :> kw)
        :>
        (let [[[first-letter :as tag-name] :as segments] 
                (.split (name kw) "(?=[#.])")
              classes (for [s segments :when (= \. (first s))] (subs s 1))
              preds (when (seq classes) (list (apply has-class classes)))
              preds (if (contains? #{nil \* \# \.} first-letter)
                      preds
                      (conj preds (tag= (keyword tag-name))))
              preds (reduce (fn [preds [x :as segment]]
                              (if (= \# x)
                                (conj preds (id= (subs segment 1)))
                                preds)) preds segments)]
         (if (seq preds) (intersection preds) any))))))
    
(defn- compile-step [step]
  (cond
    (keyword? step) (compile-keyword step)  
    (set? step) (union (map compile-step step))
    (vector? step) (intersection (map compile-step step))
    :else step))      
    
(defn- compile-chain [chain]
  (map compile-step chain))

(defn- selector-chains [selector id]
  (for [x (tree-seq set? seq selector) :when (not (set? x))]
    (compile-chain (concat x [id]))))

(defn- predset [preds]
  (condp = (count preds)
    1 (let [[f] preds] #(if (f %) 1 0))
    2 (let [[f g] preds] #(+ (if (f %) 1 0) (if (g %) 2 0)))
    3 (let [[f g h] preds] #(-> (if (f %) 1 0) (+ (if (g %) 2 0))
                              (+ (if (h %) 4 0))))
    4 (let [[f g h k] preds] #(-> (if (f %) 1 0) (+ (if (g %) 2 0))
                                (+ (if (h %) 4 0)) (+ (if (k %) 8 0))))
    #(loop [i 1 r 0 preds (seq preds)]
       (if-let [[pred & preds] preds]
         (recur (bit-shift-left i 1) (if (pred %) (+ i r) r) preds)
         r))))

(defn- states [init chains-seq]
  (fn [#^Number n]
    (loop [n n s (set init) [chains & etc] chains-seq]
      (cond
        (odd? n) (recur (bit-shift-right n 1) (into s chains) etc) 
        (zero? n) s
        :else (recur (bit-shift-right n 1) s etc)))))

(defn- make-state [chains]
  (let [derivations 
          (reduce
            (fn [derivations chain]
              (cond
                (= :> (first chain))
                  (let [pred (second chain)]
                    (assoc derivations pred (conj (derivations pred) (nnext chain))))
                (next chain)
                  (let [pred (first chain)]
                    (-> derivations
                      (assoc nil (conj (derivations nil) chain)) 
                      (assoc pred (conj (derivations pred) (next chain)))))
                :else
                  (assoc derivations :accepts (first chain)))) {} chains)
        always (derivations nil)
        accepts (derivations :accepts)
        derivations (dissoc derivations nil :accepts)
        ps (predset (keys derivations))
        next-states (memoize #(make-state ((states always (vals derivations)) %)))]
    [accepts (when (seq chains) (comp next-states ps))]))

(defn cacheable [selector] (vary-meta selector assoc ::cacheable true))
(defn cacheable? [selector] (-> selector meta ::cacheable))

(defn- automaton* [selector]
  (make-state (-> selector (selector-chains 0) set)))

(defn- lockstep-automaton* [selectors]
  (make-state (set (mapcat selector-chains selectors (iterate inc 0)))))

(def #^{:private true} memoized-automaton* (memoize automaton*))

(def #^{:private true} memoized-lockstep-automaton* (memoize lockstep-automaton*))
    
(defn- automaton [selector]
  ((if (cacheable? selector) memoized-automaton* automaton*) selector))

(defn- lockstep-automaton [selectors]
  ((if (every? cacheable? selectors) memoized-lockstep-automaton* lockstep-automaton*) selectors))

(defn- accept-key [s] (nth s 0))
(defn- step [s x] (when-let [f (and s (nth s 1))] (f x)))

(defn fragment-selector? [selector]
  (map? selector))

(defn node-selector? [selector]
  (not (fragment-selector? selector)))

(defn- static-selector? [selector]
  (or (keyword? selector) 
    (and (coll? selector) (every? static-selector? selector))))

;; core 
  
(defn- children-locs [loc]
  (iterate-while z/right (z/down loc)))

(defn- transform-loc [loc previous-state transformations etc]
  (if-let [state (step previous-state loc)]
    (let [node (if-let [children (and (z/branch? loc) 
                                   (mapknit #(transform-loc %1 state transformations %2) (children-locs loc)))]
                 (z/make-node loc (z/node loc) children)
                 (z/node loc))]
      (if-let [k (accept-key state)]
        (let [result ((transformations k) node)]
          ((if (node? result) cons concat) result etc)) 
        (cons node etc)))
    (cons (z/node loc) etc)))

(defn- transform-node [nodes selector transformation]
  (let [transformation (or transformation (constantly nil))
        transformations (constantly transformation)  
        state (automaton selector)]
    (mapknit #(transform-loc (xml/xml-zip %1) state transformations %2) nodes)))

(defn- transform-fragment-locs [locs from-state to-state transformation]
  (if (and from-state to-state)
    (let [transform-fragment-loc 
           (fn [loc from-state to-state]
             (let [children (transform-fragment-locs (children-locs loc)
                              from-state to-state transformation)]
               [(if (and (z/branch? loc) (not= children (z/children loc)))
                  (z/make-node loc (z/node loc) children) 
                  (z/node loc))
                (accept-key from-state)
                (accept-key to-state)]))
          from-states (map #(step from-state %) locs)
          to-states (map #(step to-state %) locs)
          nodes+ (map transform-fragment-loc locs from-states to-states)]
      (loop [nodes+ nodes+ fragment nil transformed-nodes []]
        (if-let [[[node start? end?] & etc] nodes+]
          (if fragment
            (if end?
              (recur etc nil
                (conj transformed-nodes (transformation (conj fragment node))))
              (recur etc (conj fragment node) transformed-nodes))
            (if start? 
              (recur nodes+ [] transformed-nodes)
              (recur etc nil (conj transformed-nodes node))))
          (flatten (into transformed-nodes fragment)))))
    (map z/node locs)))

(defn- transform-fragment [nodes selector transformation]
  (if (= identity transformation)
    nodes
    (let [[from-selector to-selector] (first selector)
          transformation (or transformation (constantly nil))]
      (flatten (transform-fragment-locs (map xml/xml-zip nodes) 
                 (automaton from-selector) (automaton to-selector) 
                 transformation)))))

(defn transform [nodes selector transformation]
  (cond
    (= identity transformation)
      nodes
    (node-selector? selector)
      (transform-node nodes selector transformation)
    :else ;fragment
      (transform-fragment nodes selector transformation)))

(defn- transform-node [nodes selector transformation]
  (let [transformation (or transformation (constantly nil))
        transformations (constantly transformation)  
        state (automaton selector)]
    (mapknit #(transform-loc (xml/xml-zip %1) state transformations %2) nodes)))

(defn lockstep-transform [nodes transformations-map]
  (let [state (lockstep-automaton (keys transformations-map))
        transformations (vec (map #(or % (constantly nil)) 
                               (vals transformations-map)))]
    (mapknit #(transform-loc (xml/xml-zip %1) state transformations %2) nodes)))

(defn at* [node-or-nodes rules]
  (reduce (fn [nodes [s t]] (transform nodes s t))
    (as-nodes node-or-nodes) rules))

(defmacro at [node-or-nodes & rules]
  `(-> ~node-or-nodes as-nodes 
     ~@(for [[s t] (partition 2 rules)]
         (if (= :lockstep s)
           `(lockstep-transform 
              ~(into {} (for [[s t] t] 
                          [(if (static-selector? s) (cacheable s) s) t])))
           `(transform ~(if (static-selector? s) (cacheable s) s) ~t)))))

(defn zip-select-nodes* [locs state]
  (letfn [(select1 [loc previous-state] 
            (when-let [state (step previous-state loc)]
              (let [descendants (mapcat #(select1 % state) (children-locs loc))]
                (if (accept-key state) (cons loc descendants) descendants))))]
    (mapcat #(select1 % state) locs)))
      
(defn select-nodes* [nodes selector]
  (let [state (automaton selector)]
    (map z/node (zip-select-nodes* (map xml/xml-zip nodes) state)))) 
      
(defn zip-select-fragments* [locs state-from state-to]
  (letfn [(select1 [locs previous-state-from previous-state-to] 
            (when (and previous-state-from previous-state-to)
              (let [states-from (map #(step previous-state-from %) locs)
                    states-to (map #(step previous-state-to %) locs)
                    descendants (reduce into []
                                  (map #(select1 (children-locs %1) %2 %3) 
                                    locs states-from states-to))]
                (loop [fragments descendants fragment nil 
                       locs locs states-from states-from states-to states-to]
                  (if-let [[loc & etc] (seq locs)]
                    (if fragment
                      (let [fragment (conj fragment loc)]
                        (if (accept-key (first states-to))
                          (recur (conj fragments fragment) nil etc 
                            (rest states-from) (rest states-to))
                          (recur fragments fragment etc 
                            (rest states-from) (rest states-to))))
                      (if (accept-key (first states-from))
                        (recur fragments [] locs states-from states-to)
                        (recur fragments nil etc 
                          (rest states-from) (rest states-to))))
                    fragments)))))]
    (select1 locs state-from state-to)))
      
(defn select-fragments* [nodes selector]
  (let [[selector-from selector-to] (first selector) 
        state-from (automaton selector-from)
        state-to (automaton selector-to)]
    (map #(map z/node %) 
      (zip-select-fragments* (map xml/xml-zip nodes) state-from state-to)))) 

(defn select
 "Returns the seq of nodes or fragments matched by the specified selector."
 [node-or-nodes selector]
  (let [nodes (as-nodes node-or-nodes)]
    (if (node-selector? selector)
      (select-nodes* nodes selector) 
      (select-fragments* nodes selector))))
  
(defn zip-select 
 "Returns the seq of locs matched by the specified selector."
 [locs selector]
  (if (node-selector? selector)
    (apply zip-select-nodes* locs selector) 
    (apply zip-select-fragments* locs selector)))
      

;; main macros
(defmacro transformation
 ([] `identity)
 ([form] form)
 ([form & forms] `(fn [node#] (at node# ~form ~@forms))))

(defmacro lockstep-transformation
 [& forms] `(fn [node#] (at node# :lockstep ~(apply array-map forms))))

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
 `(let [xml# (html-resource ~source)] 
   ~@(map (fn [[name selector args & forms]]
            `(def ~name (snippet xml# ~selector ~args ~@forms)))
       specs)))

;; transformations

(defn content
 "Replaces the content of the element. Values can be nodes or collection of nodes." 
 [& values]
  #(assoc % :content (flatten values)))

(defmacro transform-content [& body]
 `(let [f# (transformation ~@body)]
    (fn [elt#] 
      (assoc elt# :content (flatmap f# (:content elt#))))))

(defn html-snippet [& values]
 "Concatenate values as a string and then parse it with tagsoup.
  html-snippet doesn't insert missing <html> or <body> tags."
  (-> (apply str "<bogon>" values) 
    java.io.StringReader. html-resource first :content))
  
(defn html-content
 "Replaces the content of the element. Values are strings containing html code."
 [& values]
  #(assoc % :content (apply html-snippet values))) 

(defn wrap 
 ([tag] (wrap tag nil))
 ([tag attrs]
   #(array-map :tag tag :attrs attrs :content (as-nodes %))))

(def unwrap :content)

(defn set-attr
 "Assocs attributes on the selected element."
 [& kvs]
  #(assoc % :attrs (apply assoc (:attrs % {}) kvs)))
     
(defn remove-attr 
 "Dissocs attributes on the selected element."
 [& attr-names]
  #(assoc % :attrs (apply dissoc (:attrs %) attr-names)))
    
(defn add-class
 "Adds the specified classes to the selected element." 
 [& classes]
  #(let [classes (into (or (attr-values % :class) #{}) classes)]
     (assoc-in % [:attrs :class] (apply str (interpose \space classes)))))

(defn remove-class 
 "Removes the specified classes from the selected element." 
 [& classes]
  #(let [classes (when-let [cl (attr-values % :class)] 
                   (reduce disj cl classes)) 
         attrs (:attrs %)
         attrs (if (empty? classes) 
                 (dissoc attrs :class) 
                 (assoc attrs :class (apply str (interpose \space classes))))]
     (assoc % :attrs attrs)))

(defn do->
 "Chains (composes) several transformations. Applies functions from left to right." 
 [& fns]
  #(reduce (fn [nodes f] (flatmap f nodes)) (as-nodes %) fns))

(defmacro clone-for
 [comprehension & forms]
  `(fn [node#]
     (flatten (for ~comprehension ((transformation ~@forms) node#)))))

(defn append
 "Appends the values to the content of the selected element."
 [& values]
  #(assoc % :content (concat (:content %) (flatten values)))) 

(defn prepend
 "Prepends the values to the content of the selected element."
 [& values]
  #(assoc % :content (concat (flatten values) (:content %)))) 

(defn after
 "Inserts the values after the current selection (node or fragment)."
 [& values]
  #(flatten (cons % values)))

(defn before
 "Inserts the values before the current selection (node or fragment)."
 [& values]
  #(flatten (concat values [%])))

(defn substitute
 "Replaces the current selection (node or fragment)."
 [& values]
 (constantly (flatten values)))

(defn move
 "Takes all nodes (under the current element) matched by src-selector, removes
  them and combines them with the elements matched by dest-selector.
  By default, destination elements are replaced." 
 ([src-selector dest-selector] (move src-selector dest-selector substitute))
 ([src-selector dest-selector combiner]
  (fn [node-or-nodes]
    (let [nodes (select node-or-nodes src-selector)]
      (at node-or-nodes
        src-selector nil
        dest-selector (apply combiner nodes)))))) 
     
(defn strict-mode* [node]
  (if (xml/tag? node)
    (-> node
      (assoc-in [:attrs :xmlns] "http://www.w3.org/1999/xhtml")
      (vary-meta assoc ::xml/dtd 
        ["html" "-//W3C//DTD XHTML 1.0 Transitional//EN" 
         "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"]))
    node))

(defmacro strict-mode
 "Adds xhtml-transitional DTD to switch browser in 'strict' mode." 
 [& forms]
  `(do-> (transformation ~@forms) strict-mode*)) 

;; other predicates
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

(def only-child (intersection [first-child last-child]))  

(def only-of-type (intersection [first-of-type last-of-type]))

(def void (pred #(empty? (remove empty? (:content %)))))

(def odd (nth-child 2 1))

(def even (nth-child 2 0))

(defn- select? [node-or-nodes selector]
  (-> node-or-nodes as-nodes (select selector) seq boolean))

(defn has 
 "Selector predicate, matches elements which contain at least one element that 
  matches the specified selector. See jQuery's :has" 
 [selector]
  (pred #(select? (:content %) selector)))
  
(defn but-node
 "Selector predicate, matches nodes which are rejected by the specified selector-step. See CSS :not" 
 [selector-step]
  (complement (compile-step selector-step)))

(defn but
 "Selector predicate, matches elements which are rejected by the specified selector-step. See CSS :not" 
 [selector-step]
  (intersection [any (but-node selector-step)]))

(defn left [selector-step]
  (let [selector [:> selector-step]]
    #(when-let [sibling (first (filter xml/tag? (reverse (z/lefts %))))]
       (select? sibling selector))))

(defn lefts [selector-step]
  (let [selector [:> selector-step]]
    #(select? (filter xml/tag? (z/lefts %)) selector)))
  
(defn right [selector-step]
  (let [selector [:> selector-step]]
    #(when-let [sibling (first (filter xml/tag? (z/rights %)))]
       (select? sibling selector))))

(defn rights [selector-step]
  (let [selector [:> selector-step]]
    #(select? (filter xml/tag? (z/rights %)) selector)))
  
(def any-node (constantly true))

(def this-node [:> any-node])

(def text-node #(string? (z/node %)))

(def comment-node #(xml/comment? (z/node %)))

;; screen-scraping utils
(defn text
 "Returns the text value of a node." 
 {:tag String}
 [node]
  (cond
    (string? node) node
    (xml/tag? node) (apply str (map text (:content node))) 
    :else ""))
    
(defn texts
 "Returns the text value of a nodes collection." 
 {:tag String}
 [nodes]
  (map text nodes))

(defmacro let-select
 "For each node or fragment, performs a subselect and bind it to a local, 
  then evaluates body.
  bindings is a vector of binding forms and selectors." 
 [nodes-or-fragments bindings & body]
  (let [node-or-fragment (gensym "node-or-fragment__")
        bindings 
          (map (fn [f x] (f x)) 
            (cycle [identity (fn [spec] `(select ~node-or-fragment ~spec))])
            bindings)] 
    `(map (fn [~node-or-fragment] 
            (let [~@bindings]
              ~@body)) ~nodes-or-fragments)))
 
 ;; repl-utils
(defn sniptest* [nodes f]
  (apply str (emit* (flatmap f nodes))))
    
(defmacro sniptest
 "A handy macro for experimenting at the repl" 
 [source-string & forms]
  `(sniptest* (html-snippet ~source-string) (transformation ~@forms))) 
