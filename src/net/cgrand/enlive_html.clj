;   Copyright (c) Christophe Grand, 2009-2013. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html
  "enlive-html is a selector-based transformation and extraction engine."
  (:require [net.cgrand.tagsoup :as tagsoup])
  (:require [net.cgrand.xml :as xml])
  (:require [clojure.string :as str])
  (:require [clojure.zip :as z]))

;; EXAMPLES: see net.cgrand.enlive-html.examples

;; I/O stuff

(def ^{:dynamic true} *options* {:parser tagsoup/parser})

(defmacro with-options [m & body]
  `(binding [*options* (merge *options* ~m)]
     ~@body))

(defn ns-options
  ([] (ns-options *ns*))
  ([ns] (::options (meta ns) {})))

(defn set-ns-options!
  "Sets the default options to use by all templates and snippets in the
   declaring ns."
  [options]
  (alter-meta! *ns* assoc ::options options))

(defn alter-ns-options!
  "Sets the default options to use by all templates and snippets in the
   declaring ns."
  [f & args]
  (set-ns-options! (apply f (ns-options) args)))

(defn set-ns-parser!
  "Sets the default parser to use by all templates and snippets in the
   declaring ns."
  [parser]
  (alter-ns-options! assoc :parser parser))

(defn xml-parser
 "Loads and parse a XML resource and closes the stream."
 [stream]
  (with-open [^java.io.Closeable stream stream]
    (xml/parse (org.xml.sax.InputSource. stream))))

(defmulti ^{:arglists '([resource loader])} get-resource
 "Loads a resource, using the specified loader. Returns a seq of nodes."
 (fn [res _] (type res)))

(defmulti register-resource! type)

(defmethod register-resource! :default [_]
  #_(do nothing))

(defn html-resource
 "Loads an HTML resource, returns a seq of nodes."
 ([resource]
   (get-resource resource (:parser *options*)))
 ([resource options]
   (with-options options
     (html-resource resource))))

(defn xml-resource
 "Loads an XML resource, returns a seq of nodes."
 [resource]
  (get-resource resource xml-parser))

(defmethod get-resource clojure.lang.IPersistentMap
 [xml-data _]
  (list xml-data))

(defmethod get-resource clojure.lang.IPersistentCollection
 [nodes _]
  (seq nodes))

(defmethod get-resource String
 [path loader]
  (-> (clojure.lang.RT/baseLoader) (.getResourceAsStream path) loader))

(defmethod register-resource! String [path]
  (register-resource! (.getResource (clojure.lang.RT/baseLoader) path)))

(defmethod get-resource java.io.File
 [^java.io.File file loader]
  (loader (java.io.FileInputStream. file)))

(defmethod register-resource! java.io.File [^java.io.File file]
  (register-resource! (.toURL file)))

(defmethod get-resource java.io.Reader
 [reader loader]
  (loader reader))

(defmethod get-resource java.io.InputStream
 [stream loader]
  (loader stream))

(defmethod register-resource! java.net.URL 
  [^java.net.URL url]
  (alter-meta! *ns* update-in [:net.cgrand.reload/deps] (fnil conj #{}) url))

(defmethod get-resource java.net.URL
 [^java.net.URL url loader]
  (loader (.getContent url)))

(defmethod get-resource java.net.URI
 [^java.net.URI uri loader]
  (get-resource (.toURL uri) loader))


(defn- xml-str
 "Like clojure.core/str but escapes < > and &."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))

(defn- attr-str
 "Like clojure.core/str but escapes < > & and \"."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;") (.replace "\"" "&quot;")))

(def self-closing-tags #{:area :base :basefont :br :hr :input :img :link :meta})

(declare emit)

(defn append!
  ([t] (-> t))
  ([t a] (-> t (conj! a)))
  ([t a b] (-> t (conj! a) (conj! b)))
  ([t a b c] (-> t (conj! a) (conj! b) (conj! c)))
  ([t a b c d] (-> t (conj! a) (conj! b) (conj! c) (conj! d)))
  ([t a b c d e] (-> t (conj! a) (conj! b) (conj! c) (conj! d) (conj! e)))
  ([t a b c d e f] (-> t (conj! a) (conj! b) (conj! c) (conj! d) (conj! e) (conj! f)))
  ([t a b c d e f g] (-> t (conj! a) (conj! b) (conj! c) (conj! d) (conj! e) (conj! f) (conj! g)))
  ([t a b c d e f g & more] 
    (reduce conj! (-> t (conj! a) (conj! b) (conj! c) (conj! d) (conj! e) (conj! f) (conj! g))
      more)))

(defn- emit-attrs [t attrs]
  (reduce (fn [t [k v]]
            (append! t " " (name k) "=\"" (attr-str v) "\""))
    t attrs))

(defn- content-emitter [tag-name]
  (if (#{"script" "style"} tag-name)
    (fn [t x]
      (append! t (str x)))
    emit))

(defn emit-tag [tag t]
  (let [name (-> tag :tag name)
        t (-> t
            (append! "<" name)
            (emit-attrs (:attrs tag)))]
    (if-let [s (seq (:content tag))]
      (->
        (reduce (content-emitter name)
          (append! t ">")
          s)
        (append! "</" name ">"))
      (if (self-closing-tags (:tag tag))
        (append! t " />")
        (append! t "></" name ">")))))

(defn- emit-comment [node t]
  (append! t "<!--" (str (:data node)) "-->"))

(defn- emit-dtd [{[name public-id system-id] :data} t]
  (append!
   t
   (cond
    public-id
    (str "<!DOCTYPE " name " PUBLIC \"" public-id "\"\n    \"" system-id "\">\n")
    system-id
    (str "<!DOCTYPE " name " SYSTEM \"" system-id "\">\n")
    :else
    (str "<!DOCTYPE " name ">\n"))))

(defn- annotations [x]
  (-> x meta ::annotations))

(defn- emit [t node]
  (cond
   (xml/tag? node) ((:emit (annotations node) emit-tag) node t)
   (xml/comment? node) (emit-comment node t)
   (xml/dtd? node) (emit-dtd node t)
   :else (append! t (xml-str node))))

(defn- mapknitv [f coll]
  (persistent! (reduce f (transient []) coll)))

(defn emit* [node-or-nodes]
  (seq (mapknitv emit (if (xml/tag? node-or-nodes) [node-or-nodes] node-or-nodes))))

(defn annotate [node]
  (cond
    (xml/tag? node)
      (let [node (update-in node [:content] #(map annotate %))]
        (vary-meta node assoc ::annotations {:emit emit-tag}))
    (xml/comment? node)
      (vary-meta node assoc ::annotations {:emit emit-comment})
    :else node))

;; utilities

(defn- node? [x]
  (or (string? x) (map? x)))

(defn as-nodes [node-or-nodes]
  (if (node? node-or-nodes)
    [node-or-nodes]
    node-or-nodes))

(defn flatten-nodes-coll [x]
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
  (flatten-nodes-coll (map f (as-nodes node-or-nodes))))

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
 "Turns a regex into a predicate-step on text nodes usable in selectors."
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

(def ^{:private true} compile-keyword
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
  (fn [^Number n]
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

(def ^{:private true} memoized-automaton* (memoize automaton*))

(def ^{:private true} memoized-lockstep-automaton* (memoize lockstep-automaton*))

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
  (loop [v (transient []) loc (z/down loc)]
    (if loc
      (recur (conj! v loc) (z/right loc))
      (persistent! v))))

(defn- transform-loc [loc previous-state transformations t]
  (if-let [state (step previous-state loc)]
    (let [node (if-let [children (and (z/branch? loc)
                                   (mapknitv #(transform-loc %2 state transformations %1)
                                     (children-locs loc)))]
                 (z/make-node loc (z/node loc) children)
                 (z/node loc))]
      (if-let [k (accept-key state)]
        (let [result ((transformations k) node)]
          (if (node? result)
            (append! t result)
            (reduce append! t result)))
        (append! t node)))
    (append! t (z/node loc))))

(defn- transform-node [nodes selector transformation]
  (let [transformation (or transformation (constantly nil))
        transformations (constantly transformation)
        state (automaton selector)]
    (mapknitv #(transform-loc (xml/xml-zip %2) state transformations %1)
      nodes)))

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
          (flatten-nodes-coll (into transformed-nodes fragment)))))
    (map z/node locs)))

(defn- transform-fragment [nodes selector transformation]
  (if (= identity transformation)
    nodes
    (let [[from-selector to-selector] (first selector)
          transformation (or transformation (constantly nil))]
      (flatten-nodes-coll (transform-fragment-locs (map xml/xml-zip nodes)
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

(defn lockstep-transform [nodes transformations-map]
  (let [state (lockstep-automaton (keys transformations-map))
        transformations (vec (map #(or % (constantly nil))
                               (vals transformations-map)))]
    (mapknitv #(transform-loc (xml/xml-zip %2) state transformations %1)
      nodes)))

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

(defn- pad-unless [pred value s]
  (if (pred (first s))
    (seq s)
    (cons value s)))

(defn- bodies [forms]
  (if (vector? (first forms))
    (list forms)
    forms))

;; note: options may be triggered by :options as the 1st arg
(defmacro snippet* [nodes & body]
  (let [nodesym (gensym "nodes")]
    `(let [~nodesym (map annotate ~nodes)]
       (fn ~@(for [[args & forms] (bodies body)]
           `(~args
              (doall (flatmap (transformation ~@forms) ~nodesym))))))))

(defmacro snippet
 "A snippet is a function that returns a seq of nodes."
 [source selector args & forms]
  (let [[options source selector args & forms]
         (pad-unless map? {} (list* source selector args forms))]
    `(let [opts# (merge (ns-options (find-ns '~(ns-name *ns*)))
                   ~options)
           source# ~source]
       (register-resource! source#)
       (snippet* (select (html-resource source# opts#) ~selector) ~args ~@forms))))

(defmacro template
 "A template returns a seq of string."
 ([source args & forms]
   (let [[options source & body]
           (pad-unless map? {} (list* source args forms))]
     `(let [opts# (merge (ns-options (find-ns '~(ns-name *ns*)))
                    ~options)
            source# ~source]
        (register-resource! source#)
        (comp emit* (snippet* (html-resource source# opts#) ~@body))))))

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
 (let [xml-sym (gensym "xml")]
   `(let [~xml-sym (html-resource ~source)]
      ~@(for [[name selector args & forms] specs]
               `(def ~name (snippet ~xml-sym ~selector ~args ~@forms))))))


;; transformations

(defn content
 "Replaces the content of the element. Values can be nodes or collection of nodes."
 [& values]
  #(assoc % :content (flatten-nodes-coll values)))

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

(defn replace-vars
  "By default, takes a map (or function) of keywords to strings and replaces
   all occurences of ${foo} by (m :foo) in text nodes and attributes.
   Does not recurse, you have to pair it with an appropriate selector.
   re is a regex whose first group will be passed to (comp m f) and f by
   default is #'keyword."
  ([m] (replace-vars #"\$\{\s*([^}]*[^\s}])\s*}" m))
  ([re m] (replace-vars re m keyword))
  ([re m f]
    (let [replacement (comp str m f second)
          substitute-vars #(str/replace % re replacement)]
      (fn [node]
        (cond
          (string? node) (substitute-vars node)
          (xml/tag? node) (assoc node :attrs
                            (into {} (for [[k v] (:attrs node)]
                                       [k (substitute-vars v)])))
          :else node)))))

(defn replace-words [words-to-replacements]
  "Takes a map of words to replacement strings and replaces 
   all occurences. Does not recurse, you have to pair it with an appropriate
   selector."
  (replace-vars
    (java.util.regex.Pattern/compile (str "\\b(" (str/join "|" (map #(java.util.regex.Pattern/quote %) (keys words-to-replacements))) ")\\b")) 
    words-to-replacements
    identity))

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
     (flatten-nodes-coll (for ~comprehension ((transformation ~@forms) node#)))))

(defn append
 "Appends the values to the content of the selected element."
 [& values]
  #(assoc % :content (concat (:content %) (flatten-nodes-coll values))))

(defn prepend
 "Prepends the values to the content of the selected element."
 [& values]
  #(assoc % :content (concat (flatten-nodes-coll values) (:content %))))

(defn after
 "Inserts the values after the current selection (node or fragment)."
 [& values]
  #(flatten-nodes-coll (cons % values)))

(defn before
 "Inserts the values before the current selection (node or fragment)."
 [& values]
  #(flatten-nodes-coll (concat values [%])))

(defn substitute
 "Replaces the current selection (node or fragment)."
 [& values]
 (constantly (flatten-nodes-coll values)))

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

(def ^{:doc "Selector predicate, tests if the specified attributes have the specified values."}
 attr=
  (multi-attr-pred =))

(defn- starts-with? [^String s ^String prefix]
  (and s (.startsWith s prefix)))

(defn- ends-with? [^String s ^String suffix]
  (and s (.endsWith s suffix)))

(defn- contains-substring? [^String s ^String substring]
  (and s (<= 0 (.indexOf s substring))))

(def ^{:doc "Selector predicate, tests if the specified attributes start with the specified values. See CSS ^= ."}
 attr-starts
  (multi-attr-pred starts-with?))

(def ^{:doc "Selector predicate, tests if the specified attributes end with the specified values. See CSS $= ."}
 attr-ends
  (multi-attr-pred ends-with?))

(def ^{:doc "Selector predicate, tests if the specified attributes contain the specified values. See CSS *= ."}
 attr-contains
  (multi-attr-pred contains-substring?))

(defn- is-first-segment? [^String s ^String segment]
  (and s
    (.startsWith s segment)
    (or (= (count s) (count segment))
        (= \- (.charAt s (count segment))))))

(def ^{:doc "Selector predicate, tests if the specified attributes start with the specified values. See CSS |= ."}
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

(defn left
 "Selector predicate, matches nodes whose immediate left sibling element is
  matched by the specified selector-step."
 [selector-step]
  (let [selector [:> selector-step]]
    #(when-let [sibling (first (filter xml/tag? (reverse (z/lefts %))))]
       (select? sibling selector))))

(defn lefts
 "Selector predicate, matches nodes whose one left sibling element is matched by
  the specified selector-step."
 [selector-step]
  (let [selector [:> selector-step]]
    #(select? (filter xml/tag? (z/lefts %)) selector)))

(defn right
 "Selector predicate, matches nodes whose immediate right sibling element is
  matched by the specified selector-step."
 [selector-step]
  (let [selector [:> selector-step]]
    #(when-let [sibling (first (filter xml/tag? (z/rights %)))]
       (select? sibling selector))))

(defn rights
 "Selector predicate, matches nodes whose one left sibling element is matched by
  the specified selector-step."
 [selector-step]
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


;; hiccup-style inline fragments
(defn- attr-map? [node-spec]
  (and (map? node-spec) (not (keyword? (:tag node-spec)))))

(defn- nodify [node-spec]
  (cond
    (string? node-spec) node-spec
    (vector? node-spec)
      (let [[tag & [m & ms :as more]] node-spec
            [tag-name & segments] (.split (name tag) "(?=[#.])")
            id (some (fn [^String seg]
                       (when (= \# (.charAt seg 0)) (subs seg 1))) segments)
            classes (keep (fn [^String seg]
                            (when (= \. (.charAt seg 0)) (subs seg 1)))
                          segments)
            node {:tag (keyword tag-name) :attrs (if (attr-map? m) m {})
                  :content (flatmap nodify (if (attr-map? m) ms more))}
            node (if id (assoc-in node [:attrs :id] id) node)
            node (if (seq classes)
                   (assoc-in node [:attrs :class]
                             (apply str (interpose \space classes)))
                   node)]
        node)
    (sequential? node-spec) (flatmap nodify node-spec)
    (map? node-spec) (update-in node-spec [:content] (comp nodify seq))
    :else (str node-spec)))

(defn html
  "Allows to define inline fragments with a hiccup-like syntax."
  [& nodes-specs]
  (flatmap nodify nodes-specs))
