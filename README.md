# Enlive

Enlive is a selector-based (à la CSS) templating library for Clojure.

## Installation

In your leiningen project.clj:

```clojure
[enlive "1.0.1"]
```

## Basic Usage

An Enlive template has two parts: a HTML file and a `deftemplate` form somewhere in a clj file. For example:

```html
<!DOCTYPE html>
<html>
  <!-- Enlive reads your comments too. -->
  <head>
    <title></title>
  </head>
  <body>
    <h1></h1>
    <p class="content" id="content1"></p>
    <p class="content" id="content2">I'm going to replace this.</p>
  </body>
</html>
```

```clojure
(ns enlive-test.core
  (:require [net.cgrand.enlive-html :as e]))

;; Loads the template file into enlive.
;; (templates directory is in <project-root>/resources)
(def tmpl
  (e/html-resource "templates/test.html"))

(defn -main
  "Just processes a template and dumps it out."
  [& args]
  (print
   (apply str
          (e/emit*
           (e/at tmpl
               [:h1]
                 (e/content "This is my heading!")
               [:#content2]
                 (e/content "This is my new text...")
               [:.content]
                 (e/append "This will be appended to both paragraphs."))))))
```

And, voila!

```html
<!DOCTYPE html>
<html>
  <!-- Enlive reads your comments too. -->
  <head>
    <title></title>
  </head>
  <body>
    <h1>This is my heading!</h1>
    <p id="content1" class="content">This will be appended to both paragraphs.</p>
    <p id="content2" class="content">This is my new text...This will be appended to both paragraphs.</p>
  </body>
</html>
```

## Documentation

* API Docs ()
* Wiki (https://github.com/cgrand/enlive/wiki/_pages)
* Some examples of selector syntax: http://enlive.cgrand.net/syntax.html
* A nice tutorial by David Nolen: http://github.com/swannodette/enlive-tutorial/
* Another tutorial, by Brian Marick: https://github.com/cgrand/enlive/wiki/Table-and-Layout-Tutorial,-Part-1:-The-Goal

## Selectors

Enlive selectors can match either nodes or fragments (several adjacent nodes).   

### Selectors 101

At the core, *every selector is a vector*. The items of this vector are called
*steps*.

A step is a predicate, for example `:h1`, `:p.some-class` or even 
`(attr? :lang)`.

To select elements which match several predicates, you need to group
predicates into a vector: *inside steps, vectors mean "and"*. This may seem
confusing but the rule is simple: the outer-most vector hierarchically 
chains steps, all other vectors denote intersection (and) between steps.

So `[:p (attr? :lang)]` is going to match any elements with a `lang` attribute
inside a `p` element. On the other hand, `[:p (attr? :lang)]` is going to match
any `p` with a `lang` attribute.

Similarly, sets group predicates in an union. Hence *inside steps, sets mean
"or."* So `[#{:div.class1 :div.class2}]` match every `div` which has either 
`class1` or `class2`. This can alternatively be written 
as `[[:div #{:.class1 .class2}]]`. Indeed you can have nested "ors" and "ands"
which means nested sets and vectors.

At the top level you can have a big "or" between selectors by wrapping several 
selectors in a set. `#{[:td :em] [:th :em]}` is going to match any `em` insides
 either a `th` or a `td`. This is equivalent to `[#{:td :th} :em]`.

### Syntax

(See [syntax.html](http://enlive.cgrand.net/syntax.html])

Some examples:

```clojure
Enlive                                       CSS
========================================================================================================
[:div]                                       div
[:body :script]                              body script
#{[:ul.outline :> :li] [:ol.outline :> li]}  ul.outline > li, ol.outline > li 
[#{:ul.outline :ol.outline} :> :li]          ul.outline > li, ol.outline > li
[[#{:ul :ol} :.outline] :> :li]              ul.outline > li, ol.outline > li
[:div :> :*]                                 div > *
[:div :> text-node]                          (text children of a div) 
[:div :> any-node]                           (all children (including text nodes and comments) of a div)
{[:dt] [:dl]}                                (fragments starting by DT and ending at the *next* DD)    
```
## Templates and snippets

A snippet is a function that returns a seq of nodes. It can be used as a
building block for more complex templates.

A template is a function that returns a seq of strings--basically it's a
snippet whose output is serialized. Templates return a seq of strings to avoid
building the whole string.

Templates and snippets transform a source (specified as a path (to access 
resources on the classpath), a File, a Reader, an InputStream, a URI, a URL,
an element or a seq of nodes).

### The `at` form

The `at` form is the most important form in Enlive. There are implicit `at` 
forms in `snippet` and `template`.  

```clojure
(at a-node
  [:a :selector] a-transformation
  [:another :selector] another-transformation
  ... )
```

The right-hand value of a rule can be nil. It's the idiomatic way to remove an
element.

Transformations (described in the next section) are closures which take one arg (the selected node) and return
nil, another node or an arbitrarily nested collection of nodes.

Rules are applied top-down: the first rule transforms the whole tree and the
resulting tree is passed to the next rules.

## Transformations

Transformations (the right-hand parts of rules) are plain old closures. 
These functions take one arg (the selected node) and return nil, another node 
or a collection of nodes.

Enlive defines many transformation helper functions:

```clojure
  content            (content "xyz" a-node "abc")             
  html-content       (html-content "<blink>please no</blink>")
  wrap               (wrap :div) or (wrap :div {:class "foo"}) 
  unwrap             unwrap
  set-attr           (set-attr :attr1 "val1" :attr2 "val2")
  remove-attr        (remove-attr :attr1 :attr2) 
  add-class          (add-class "foo" "bar")
  remove-class       (remove-class "foo" "bar")
  do->               (do-> transformation1 transformation2) 
  clone-for          (clone-for [item items] transformation)
                     or (clone-for [item items] 
                          selector1 transformation1
                          selector2 transformation2)
  append             (append "xyz" a-node "abc")
  prepend            (prepend "xyz" a-node "abc")
  after              (after "xyz" a-node "abc")
  before             (before "xyz" a-node "abc")
  substitute         (substitute "xyz" a-node "abc")
  move               (move [:.footnote] [:#footnotes] content)
```

By default selector-transformation pairs are run sequentially. When you know
that several transformations are independent, you can now specify (as an 
optimization) to process them in lockstep. **Note that this doesn't work with
fragment selectors.**

Example:

```clojure
[:a :selector] a-transformation
[:another :selector] another-transformation
[:a :dependent :selector] yet-another-transformation
```
If the first two transformations are independent, you can rewrite this code as:

```clojure
:lockstep
{ [:a :selector] a-transformation
  [:another :selector] another-transformation }
[:a :dependent :selector] yet-another-transformation
```
Transformations are now slightly restricted in their return values: a node or 
a collection of nodes (instead of freely nested collections of nodes).

### Dynamic selectors

Selectors aren't compiled anymore. It means that you don't 
need to wrap them in (selector ...) forms anymore nor to eval them in the most
dynamic cases.

### Fragment selectors

Fragment selectors allow to select adjacent nodes. They are denoted by a map of
two node selectors (e.g. `{[:h1] [:p]}`). Bounds are inclusive and they select
the smallest matching fragments.   

Rules are applied top-down: the first rule transforms the whole tree and the 
resulting tree is passed to the next rules.

Nodes are transformed depth-first, that is: if a selector selects several nodes,
descendants are transformed first. Hence, when the transformation is applied to
an ancestor, you can "see" the transformed descendants (but you can not see
your transformed siblings).

Take this node tree:

      A
     / \
    C   B

If A and B are selected and transformed by T then the resulting tree is

    (T A)
     / \
    C   (T B)

## Where do I get support?

On the group: http://groups.google.com/group/enlive-clj

## Known limitations/problems

* No namespaces support (hence unsuitable for most XML)
