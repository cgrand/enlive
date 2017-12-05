# Selectors syntax

<style type='text/css'>
code {font-style: normal; font-weight: bold; color: black; background-color: #eee}
.syntax { color: #666; font-style: italic;}
.syntax dd:before {content: "| "}
.css-equiv {font-size: 60%}
h3 {margin: 0; font-size: 100%;}
pre {margin: 0; margin-left: 1ex; color: #666;}
dt {margin-top: 1ex;}</style>
</head>
<dl class="syntax">
<dt id="selector"><a href="#selector">selector</a></dt>
<dd><code>[</code></code><a href="#selector-step">selector-step</a> (<code>:></code>? </code><a href="#selector-step">selector-step</a>)*<code>]</code></dd>
<dd><code>#{</code><a href="#selector">selector</a>*<code>}</code> ; grouping</dd>
<dd><code>a-symbol</code> ; must evaluate to a state-machine</dd>
<dd><code>(some clojure code)</code> ; must evaluate to a state-machine</dd>
<dt id="selector-step"><a href="#selector-step">selector-step</a></dt>
<dd><code>:a-keyword</code> ; can be :* :.class :tag :#id or any combination eg :div#foo.bar.baz</dd>
<dd><code>#{</code><a href="#selector-step">selector-step</a>*<code>}</code> ; union</dd>
<dd><code>[</code><a href="#selector-step">selector-step</a>*<code>]</code> ; intersection</dd>
<dd><code>a-symbol</code> ; must evaluate to a state-machine — <a href="#predefined-selector-steps">some are already defined</a></dd>
<dd><code>(some clojure code)</code> ; must evaluate to a state-machine — better built using some <a href="#predefined-functions">predefined functions</a>, <a href="#predicate-builders">predicate builders</a> or <a href="#useful-macros">macros</a></dd>
</dl>

<h2 id="predefined-selector-steps">Predefined selector-steps</h2>
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#root-pseudo"><code>root</code></a>,
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#first-child-pseudo"><code>first-child</code></a>,                      
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#last-child-pseudo"><code>last-child</code></a>,                    
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#first-of-type-pseudo"><code>first-of-type</code></a>,                    
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#last-of-type-pseudo"><code>last-of-type</code></a>,                  
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#only-child-pseudo"><code>only-child</code></a>,                  
<a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#only-of-type-pseudo"><code>only-of-type</code></a>,                     
<code>void</code> (CSS's <a href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#empty-pseudo"><code>:empty</code></a>),                             
<code>odd</code> and
<code>even</code>.                             

<h2 id="predefined-functions">Predefined functions</h2>
<dl>
<dt><code>attr?</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-representation">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr?</code> attribute-keyword*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr? :href) ; *[href]
(attr? :href :title) ; *[href][title]</pre>

<dt><code>attr=</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-representation">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr=</code> (attribute-keyword value)*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr= :href "foo") ; *[href=foo]
(attr= :href "foo" :title "bar") ; *[href=foo][title=bar]</pre>

<dt><code>attr-has</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-representation">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr-has</code> (attribute-keyword value)*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr-has :foo "bar" "baz") ; *[foo~=bar][foo~=baz]</pre>

<dt><code>attr-starts</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-substrings">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr-starts</code> (attribute-keyword value)*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr-starts :href "foo" :title "bar"); *[href^=foo][title^=bar]</pre>   

<dt><code>attr-ends</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-substrings">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr-ends</code> (attribute-keyword value)*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr-ends :href "foo" :title "bar") ; *[href$=foo][title$=bar]</pre>

<dt><code>attr-contains</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-substrings">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr-contains</code> (attribute-keyword value)*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr-contains :href "foo" :title "bar") ; *[href*=foo][title*=bar]</pre>

<dt><code>attr|=</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#attribute-representation">CSS</a></dt>
<dd>
<div class="syntax"><code>(attr|=</code> (attribute-keyword value)*<code>)</code></div>
<h3>sample usage:</h3>
<pre>(attr|= :lang "fr") ; *[lang|=fr]</pre>
  
<dt><code>nth-child</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#nth-child-pseudo">CSS</a></dt>
<dd>
<div class="syntax"><code>(nth-child</code> stride? offset<code>)</code></div>
<h3>sample usage:</h3>
<pre>(nth-child 3) ; *:nth-child(3)
(nth-child 4 2) ; *:nth-child(4n+2)</pre>

<dt><code>nth-last-child</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#nth-last-child-pseudo">CSS</a></dt>
<dd>
<div class="syntax"><code>(nth-last-child</code> stride? offset<code>)</code></div>
<h3>sample usage:</h3>
<pre>(nth-last-child 3) ; *:nth-last-child(3)
(nth-last-child 4 2) ; *:nth-last-child(4n+2)</pre>

<dt><code>nth-of-type</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#nth-of-type-pseudo">CSS</a></dt>
<dd>
<div class="syntax"><code>(nth-of-type</code> stride? offset<code>)</code></div>
<h3>sample usage:</h3>
<pre>(nth-of-type 3) ; *:nth-of-type(3)
(nth-of-type 4 2) ; *:nth-of-type(4n+2)</pre>

<dt><code>nth-last-of-type</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#nth-last-of-type-pseudo">CSS</a></dt>
<dd>
<div class="syntax"><code>(nth-last-of-type</code> stride? offset<code>)</code></div>
<h3>sample usage:</h3>
<pre>(nth-last-of-type 3) ; *:nth-last-of-type(3)
(nth-last-of-type 4 2) ; *:nth-last-of-type(4n+2)</pre>

<dt><code>but</code> <a class="css-equiv" href="http://www.w3.org/TR/2009/WD-css3-selectors-20090310/#negation">CSS</a></dt>
<dd>
<div class="syntax"><code>(but</code> <a href="#selector-step">selector-step</a><code>)</code></div>
<h3>sample usage:</h3>
<pre>(but :a) ; :not(a)</pre>

<dt><code>has</code></dt>
<dd><div class="syntax"><code>(has</code> <a href="#selector">selector</a><code>)</code></div>
<h3>sample usage:</h3>
<pre>(has [:a])</pre>

</dl>

<h2 id="predicate-builders">Predicate builders</h2>
These functions take a predicate and return a state-machine.
<dl>
<dt><code>pred</code>
<dd>
<div class="syntax"><code>(pred</code> predicate-on-elements<code>)</code></div>
<h3>sample usage:</h3>
<pre>(pred #(= (:tag %) tag-name))</pre>

<dt><code>text-pred</code>
<dd>
<div class="syntax"><code>(text-pred</code> predicate-on-text-nodes<code>)</code></div>
<h3>sample usage:</h3>
<pre>(text-pred #(re-matches #"\d+" %))</pre>

<dt><code>zip-pred</code>
<dd>
<div class="syntax"><code>(zip-pred</code> predicate-on-elements-locs<code>)</code></div>

<dt><code>sm/pred</code> (where sm aliases net.cgrand.enlive-html.state-machine)  
<dd>
<div class="syntax"><code>(sm/pred</code> predicate-on-locs<code>)</code></div>

</dl>

<h2 id="useful-macros">Useful macros</h2>
<code>selector</code> takes a selector and evaluates to a state-machine, <code>selector-step</code> takes a selector-step and evaluates to a state-machine.<br />
They are backed by <code>compile-selector</code> and <code>compile-step</code>.  
