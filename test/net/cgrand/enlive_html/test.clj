;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html.test
  (:use net.cgrand.enlive-html)
  (:require [net.cgrand.xml :as xml])
  (:require [clojure.zip :as z])
  (:use [clojure.test :only [deftest is are]]))

;; test utilities
(defn- normalize [x]
  (if (string? x)
    (html-snippet x)
    (html-resource x)))

(defn- same? [& xs]
  (apply = (map normalize xs)))

(defmacro #^{:private true}
 is-same
 [& forms]
 `(is (same? ~@forms)))

(defn- test-step [expected pred node]
  (= expected (boolean (pred (xml/xml-zip node)))))

(defn- elt
 ([tag] (elt tag nil))
 ([tag attrs & content]
   {:tag tag
    :attrs attrs
    :content content}))



(deftest tag=-test
  (are [_1 _2 _3] (test-step _1 _2 _3)
    true (tag= :foo) (elt :foo)
    false (tag= :bar) (elt :foo)))

(deftest id=-test
  (are [_1 _2 _3] (test-step _1 _2 _3)
    true (id= "foo") (elt :div {:id "foo"})
    false (id= "bar") (elt :div {:id "foo"})
    false (id= "foo") (elt :div)))

(deftest attr?-test
  (are [_1 _2 _3] (test-step _1 _2 _3)
    true (attr? :href) (elt :a {:href "http://cgrand.net/"})
    false (attr? :href) (elt :a {:name "toc"})
    false (attr? :href :title) (elt :a {:href "http://cgrand.net/"})
    true (attr? :href :title) (elt :a {:href "http://cgrand.net/" :title "home"})))

(deftest attr=-test
  (are [_1 _2] (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr= :href "http://cgrand.net/")
    false (attr= :href "http://clojure.org/")
    false (attr= :href "http://cgrand.net/" :name "home")
    false (attr= :href "http://cgrand.net/" :title "homepage")
    true (attr= :href "http://cgrand.net/" :title "home")))

(deftest attr-starts-test
  (are [_1 _2] (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr-starts :href "http://cgr")
    false (attr-starts :href "http://clo")
    false (attr-starts :href "http://cgr" :name "ho")
    false (attr-starts :href "http://cgr" :title "x")
    true (attr-starts :href "http://cgr" :title "ho")))

(deftest attr-ends-test
  (are [_1 _2] (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr-ends :href "d.net/")
    false (attr-ends :href "e.org/")
    false (attr-ends :href "d.net/" :name "me")
    false (attr-ends :href "d.net/" :title "hom")
    true (attr-ends :href "d.net/" :title "me")))

(deftest attr-contains-test
  (are [_1 _2] (test-step _1 _2 (elt :a {:href "http://cgrand.net/" :title "home"}))
    true (attr-contains :href "rand")
    false (attr-contains :href "jure")
    false (attr-contains :href "rand" :name "om")
    false (attr-contains :href "rand" :title "pa")
    true (attr-contains :href "rand" :title "om")))

(deftest nth-child-test
  (are [_1 _2] (same? _2 (sniptest "<dl><dt>1<dt>2<dt>3<dt>4<dt>5" _1 (add-class "foo")))
    [[:dt (nth-child 2)]] "<dl><dt>1<dt class=foo>2<dt>3<dt>4<dt>5"
    [[:dt (nth-child 2 0)]] "<dl><dt>1<dt class=foo>2<dt>3<dt class=foo>4<dt>5"
    [[:dt (nth-child 3 1)]] "<dl><dt class=foo>1<dt>2<dt>3<dt class=foo>4<dt>5"
    [[:dt (nth-child -1 3)]] "<dl><dt class=foo>1<dt class=foo>2<dt class=foo>3<dt>4<dt>5"
    [[:dt (nth-child 3 -1)]] "<dl><dt>1<dt class=foo>2<dt>3<dt>4<dt class=foo>5"))

(deftest nth-last-child-test
  (are [_1 _2] (same? _2 (sniptest "<dl><dt>1<dt>2<dt>3<dt>4<dt>5" _1 (add-class "foo")))
    [[:dt (nth-last-child 2)]] "<dl><dt>1<dt>2<dt>3<dt class=foo>4<dt>5"
    [[:dt (nth-last-child 2 0)]] "<dl><dt>1<dt class=foo>2<dt>3<dt class=foo>4<dt>5"
    [[:dt (nth-last-child 3 1)]] "<dl><dt>1<dt class=foo>2<dt>3<dt>4<dt class=foo>5"
    [[:dt (nth-last-child -1 3)]] "<dl><dt>1<dt>2<dt class=foo>3<dt class=foo>4<dt class=foo>5"
    [[:dt (nth-last-child 3 -1)]] "<dl><dt class=foo>1<dt>2<dt>3<dt class=foo>4<dt>5"))

(deftest nth-of-type-test
  (are [_1 _2] (same? _2 (sniptest "<dl><dt>1<dd>def #1<dt>2<dt>3<dd>def #3<dt>4<dt>5" _1 (add-class "foo")))
    [[:dt (nth-of-type 2)]] "<dl><dt>1<dd>def #1<dt class=foo>2<dt>3<dd>def #3<dt>4<dt>5"
    [[:dt (nth-of-type 2 0)]] "<dl><dt>1<dd>def #1<dt class=foo>2<dt>3<dd>def #3<dt class=foo>4<dt>5"
    [[:dt (nth-of-type 3 1)]] "<dl><dt class=foo>1<dd>def #1<dt>2<dt>3<dd>def #3<dt class=foo>4<dt>5"
    [[:dt (nth-of-type -1 3)]] "<dl><dt class=foo>1<dd>def #1<dt class=foo>2<dt class=foo>3<dd>def #3<dt>4<dt>5"
    [[:dt (nth-of-type 3 -1)]] "<dl><dt>1<dd>def #1<dt class=foo>2<dt>3<dd>def #3<dt>4<dt class=foo>5"))

(deftest nth-last-of-type-test
  (are [_1 _2] (same? _2 (sniptest "<dl><dt>1<dd>def #1<dt>2<dt>3<dd>def #3<dt>4<dt>5" _1 (add-class "foo")))
    [[:dt (nth-last-of-type 2)]] "<dl><dt>1<dd>def #1<dt>2<dt>3<dd>def #3<dt class=foo>4<dt>5"
    [[:dt (nth-last-of-type 2 0)]] "<dl><dt>1<dd>def #1<dt class=foo>2<dt>3<dd>def #3<dt class=foo>4<dt>5"
    [[:dt (nth-last-of-type 3 1)]] "<dl><dt>1<dd>def #1<dt class=foo>2<dt>3<dd>def #3<dt>4<dt class=foo>5"
    [[:dt (nth-last-of-type -1 3)]] "<dl><dt>1<dd>def #1<dt>2<dt class=foo>3<dd>def #3<dt class=foo>4<dt class=foo>5"
    [[:dt (nth-last-of-type 3 -1)]] "<dl><dt class=foo>1<dd>def #1<dt>2<dt>3<dd>def #3<dt class=foo>4<dt>5"))

(deftest has-test
  (is-same "<div><p>XXX<p class='ok'><a>link</a><p>YYY"
    (sniptest "<div><p>XXX<p><a>link</a><p>YYY"
      [[:p (has [:a])]] (add-class "ok"))))

(deftest but-test
  (is-same "<div><p>XXX<p><a class='ok'>link</a><p>YYY"
    (sniptest "<div><p>XXX<p><a>link</a><p>YYY"
      [:div (but :p)] (add-class "ok")))

  (is-same "<div><p class='ok'>XXX<p><a>link</a><p class='ok'>YYY"
    (sniptest "<div><p>XXX<p><a>link</a><p>YYY"
      [[:p (but (has [:a]))]] (add-class "ok"))))

(deftest left-test
  (are [_1 _2] (same? _2 (sniptest "<div><h1>T1<h2>T2<h3>T3<p>XXX" _1 (add-class "ok")))
    [[:h3 (left :h2)]] "<div><h1>T1<h2>T2<h3 class=ok>T3<p>XXX"
    [[:h3 (left :h1)]] "<div><h1>T1<h2>T2<h3>T3<p>XXX"
    [[:h3 (left :p)]] "<div><h1>T1<h2>T2<h3>T3<p>XXX"))

(deftest lefts-test
  (are [_1 _2] (same? _2 (sniptest "<div><h1>T1<h2>T2<h3>T3<p>XXX" _1 (add-class "ok")))
    [[:h3 (lefts :h2)]] "<div><h1>T1<h2>T2<h3 class=ok>T3<p>XXX"
    [[:h3 (lefts :h1)]] "<div><h1>T1<h2>T2<h3 class=ok>T3<p>XXX"
    [[:h3 (lefts :p)]] "<div><h1>T1<h2>T2<h3>T3<p>XXX"))

(deftest right-test
  (are [_1 _2] (same? _2 (sniptest "<div><h1>T1<h2>T2<h3>T3<p>XXX" _1 (add-class "ok")))
    [[:h2 (right :h3)]] "<div><h1>T1<h2 class=ok>T2<h3>T3<p>XXX"
    [[:h2 (right :p)]] "<div><h1>T1<h2>T2<h3>T3<p>XXX"
    [[:h2 (right :h1)]] "<div><h1>T1<h2>T2<h3>T3<p>XXX"))

(deftest rights-test
  (are [_1 _2] (same? _2 (sniptest "<div><h1>T1<h2>T2<h3>T3<p>XXX" _1 (add-class "ok")))
    [[:h2 (rights :h3)]] "<div><h1>T1<h2 class=ok>T2<h3>T3<p>XXX"
    [[:h2 (rights :p)]] "<div><h1>T1<h2 class=ok>T2<h3>T3<p>XXX"
    [[:h2 (rights :h1)]] "<div><h1>T1<h2>T2<h3>T3<p>XXX"))

(deftest any-node-test
  (is (= 3 (-> "<html><body><i>this</i> is a <i>test</i>" html-snippet
             (select [:body :> any-node]) count))))

(deftest transform-test
  (is-same "<div>" (sniptest "<div><span>" [:span] nil))
  (is-same "<!-- comment -->" (sniptest "<!-- comment -->" [:span] nil))
  (is-same "\n<feed xml:lang=\"en-us\">\n <title>1</title>\n <id>1</id>\n <link href=\"./\" />\n <link rel=\"self\" href=\"\" />\n <subtitle>1</subtitle>\n <updated>1</updated>\n\n<entry xml:base=\"http://orcloud.org/\">\n  <title>1</title>\n  <link href=\"1\" />\n  <id>1</id>\n  <published>1</published>\n  \n  <category term=\"devops\" scheme=\"http://hugoduncan.org/tags\">1</category>\n  <summary type=\"xhtml\"><div>1</div></summary>\n  <content type=\"xhtml\"><div>1</div></content>\n</entry><entry xml:base=\"http://orcloud.org/\">\n  <title>1</title>\n  <link href=\"1\" />\n  <id>1</id>\n  <published>1</published>\n  \n  <category term=\"devops\" scheme=\"http://hugoduncan.org/tags\">1</category>\n  <summary type=\"xhtml\"><div>1</div></summary>\n  <content type=\"xhtml\"><div>1</div></content>\n</entry>\n</feed>\n"
           (sniptest "<?xml version='1.0' encoding='UTF-8'?>
<feed xml:lang='en-us' xmlns='http://www.w3.org/2005/Atom'>
 <title>Feed title</title>
 <id>feed id</id>
 <link href='./'/>
 <link href='' rel='self'/>
 <subtitle>subtitle</subtitle>
 <updated></updated>

<entry xml:base='http://orcloud.org/'>
  <title>title</title>
  <link href=''/>
  <id></id>
  <published>2010-05-11T20:00:00.000000-04:00</published>
  <updated>2010-05-11T20:00:00.000000-04:00</updated>
  <category scheme='http://hugoduncan.org/tags' term='devops'/>
  <summary type='xhtml'><div xmlns='http://www.w3.org/1999/xhtml'></div></summary>
  <content type='xhtml'><div xmlns='http://www.w3.org/1999/xhtml'></div></content>
</entry>
</feed>
" [:feed :title] (content "1")
  [:feed :id] (content "1")
  [:feed :subtitle] (content "1")
  [:feed :updated] (content "1")
  [:entry] (clone-for [x ["one" "two"]]
                      [:published] (content "1")
                      [:updated] nil
                      [:category] (content "1")
                      [:link] (set-attr :href "1")
                      [:id] (content "1")
                      [:summary :div] (content "1")
                      [:content :div] (content "1"))
            )))

(deftest clone-for-test
  ;; node selector
  (is-same "<ul><li>one<li>two"
    (sniptest "<ul><li>" [:li] (clone-for [x ["one" "two"]] (content x))))
  ;; fragment selector
  (is-same "<dl><dt>term #1<dd>desc #1<dt>term #2<dd>desc #2"
    (sniptest "<dl><dt>Sample term<dd>sample description"
      {[:dt] [:dd]} (clone-for [[t d] {"term #1" "desc #1" "term #2" "desc #2"}]
                      [:dt] (content t)
                      [:dd] (content d)))))

(deftest move-test
  (are [_1 _2] (same?
                _2
                (sniptest "<body><span>1</span><div id=target>here</div><span>2</span>"
                          (move [:span] [:div] _1) ))
    substitute "<body><span>1</span><span>2</span>"
    content "<body><div id=target><span>1</span><span>2</span></div>"
    after "<body><div id=target>here</div><span>1</span><span>2</span>"
    before "<body><span>1</span><span>2</span><div id=target>here</div>"
    append "<body><div id=target>here<span>1</span><span>2</span></div>"
    prepend "<body><div id=target><span>1</span><span>2</span>here</div>")
  (are [_1 _2] (same?
                _2
                (sniptest "<div><h1>Title1<p>blabla<hr><h2>Title2<p>blibli"
                          (move {[:h1] [:p]} {[:h2] [:p]} _1) ))
    substitute "<div><hr><h1>Title1<p>blabla"
    after "<div><hr><h2>Title2<p>blibli<h1>Title1<p>blabla"
    before "<div><hr><h1>Title1<p>blabla<h2>Title2<p>blibli")
  (are [_1 _2] (same?
                _2
                (sniptest "<div><h1>Title1<p>blabla<hr><h2>Title2<p>blibli"
                          (move {[:h1] [:p]} [:h2] _1) ))
    substitute "<div><hr><h1>Title1<p>blabla<p>blibli"
    content "<div><hr><h2><h1>Title1</h1><p>blabla</p></h2><p>blibli"
    after "<div><hr><h2>Title2<h1>Title1<p>blabla<p>blibli"
    before "<div><hr><h1>Title1<p>blabla<h2>Title2<p>blibli"
    append "<div><hr><h2>Title2<h1>Title1</h1><p>blabla</p></h2><p>blibli"
    prepend "<div><hr><h2><h1>Title1</h1><p>blabla</p>Title2</h2><p>blibli")
  (are [_1 _2] (same? _2
                      (sniptest "<div><h1>Title1<p>blabla<hr><h2>Title2<p>blibli"
                                (move [:h1] {[:h2] [:p]} _1) ))
    substitute "<div><p>blabla<hr><h1>Title1"
    after "<div><p>blabla<hr><h2>Title2<p>blibli<h1>Title1"
    before "<div><p>blabla<hr><h1>Title1<h2>Title2<p>blibli"))

(deftest wrap-test
  (is-same "<dl><ol><dt>Sample term</dt></ol><dd>sample description</dd></dl>"
    (sniptest "<dl><dt>Sample term<dd>sample description" [:dt] (wrap :ol)))
  (is-same "<dl><ol><dt>Sample term</dt><dd>sample description</dd></ol></dl>"
    (sniptest "<dl><dt>Sample term<dd>sample description" {[:dt] [:dd]} (wrap :ol))))

(deftest select-test
  (is (= 3 (-> "<html><body><h1>hello</h1>" html-snippet (select [:*]) count))))

(deftest emit*-test
  (is (= "<h1>hello&lt;<script>if (im < bad) document.write('&lt;')</script></h1>"
         (sniptest "<h1>hello&lt;<script>if (im < bad) document.write('&lt;')"))))

(deftest transform-content-test
  (is-same "<div><div class='bar'><div>"
    (sniptest "<div><div><div>"
      [:> :div] (transform-content [:> :div] (add-class "bar")))))

(deftemplate case-insensitive-doctype-template "resources/templates/doctype_case.html"
  [])

(deftest case-insensitive-doctype-test
  (.startsWith "<!DOCTYPE" (apply str (case-insensitive-doctype-template))))

(deftest templates-return-seqs
  (seq? (case-insensitive-doctype-template)))

(deftest hiccup-like
  (is-same "<div><b>world"
    (sniptest "<div>"
      [:div] (content (html [:b "world"]))))
  (is-same "<div><b id=foo>world"
    (sniptest "<div>"
      [:div] (content (html [:b#foo "world"]))))
  (is-same "<div><a id=foo class=\"link home\" href=\"http://clojure.org/\">world"
    (sniptest "<div>"
      [:div] (content (html [:a.link#foo.home {:href "http://clojure.org/"}
                             "world"]))))
  (is-same "<div><ul><li>a<li>b<li>c"
    (sniptest "<div>"
      [:div] (content (html [:ul (for [s ["a" "b" "c"]] [:li s])])))))

(deftest hiccup-mixed
  (is-same "<div><p><i>big</i><b>world</b></p>"
    (sniptest "<div>"
      [:div] (content (html [:p '({:tag :i :content ["big"]}
                                  {:tag :b :content ["world"]})])))
    (sniptest "<div>"
      [:div] (content (html [:p {:tag :i :content ["big"]}
                             {:tag :b :content ["world"]}])))
    (sniptest "<div>"
      [:div] (content (html {:tag :p
                             :content [[:i "big"] [:b "world"]]}))))
  (is-same "<div><a href='http://clojure.org/'><i>big</i><b>world</b></a>"
    (sniptest "<div>"
      [:div] (content (html [:a {:href "http://clojure.org/"} {:tag :i :content ["big"]}
                             {:tag :b :content ["world"]}])))))

(deftest replace-vars-test
  (is-same "<div><h1>untouched ${name}<p class=hello>hello world"
           (sniptest "<div><h1>untouched ${name}<p class=\"${class}\">hello ${name}"
      #{[:p] [:p any-node]} (replace-vars {:name "world" :class "hello"})))
  (is (= ((replace-vars {:a "A" :b "B"}) "${a} ${b}")
        "A B"))
  (is (= ((replace-vars {:a "A" :b 42}) "${a} ${b}")
         "A 42"))
  (is (= ((replace-words {"Donald" "Mickey" "Duck" "Mouse"}) "Donald Duckling Duck")
        "Mickey Duckling Mouse")))
