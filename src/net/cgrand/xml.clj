;   Copyright (c) Christophe Grand. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.xml
  (:require [clojure.zip :as z])
  (:require [net.cgrand.insertion-point :as ip])
  (:import (org.xml.sax ContentHandler Attributes SAXException XMLReader)
           (org.xml.sax.ext DefaultHandler2)
           (javax.xml.parsers SAXParser SAXParserFactory)))

(defstruct element :tag :attrs :content)

(def tag (accessor element :tag))
(def attrs (accessor element :attrs))
(def content (accessor element :content))

(def tag? :tag)
(defn document? [x] (= :document (:type x)))
(defn comment? [x] (= :comment (:type x)))

(defn xml-zip 
 "Returns a zipper for xml elements (as from xml/parse),
 given a root element"
 [root]
   (z/zipper tag? :content #(assoc %1 :content (and %2 (apply vector %2))) root))

(defn- insert-element [ip e]
  (-> ip (ip/insert-right e) ip/down-right))

(defn- merge-text-left [ip s]
  (if-let [l (ip/left-loc ip)]
    (if (-> l z/node string?)
      (-> ip ip/remove-left (ip/insert-left (str (z/node l) s)))
      (ip/insert-left ip s))
    (ip/insert-left ip s)))

(defn- handler [ip]
  (proxy [DefaultHandler2] []
    (startElement [uri local-name q-name #^Attributes atts]
      (let [e (struct element 
                (clojure.lang.Keyword/intern (symbol q-name))
                (when (pos? (. atts (getLength)))
                  (reduce #(assoc %1 (clojure.lang.Keyword/intern (symbol (.getQName atts %2))) (.getValue atts %2)) 
                    {} (range (. atts (getLength))))))]
        (swap! ip insert-element e))) 
    (endElement [uri local-name q-name]
      (swap! ip ip/up-right))
    (characters [ch start length]
      (swap! ip merge-text-left (String. ch start length)))
    (ignorableWhitespace [ch start length]
      (swap! ip merge-text-left (String. ch start length)))
    (comment [ch start length]
      (swap! ip ip/insert-left {:type :comment :data (String. ch start length)}))))

(defn startparse-sax [s ch]
  (.. SAXParserFactory (newInstance) (newSAXParser) (parse s ch)))

(defn parse
  "Parses and loads the source s, which can be a File, InputStream or
  String naming a URI. Returns a tree of the xml/element struct-map,
  which has the keys :tag, :attrs, and :content. and accessor fns tag,
  attrs, and content. Other parsers can be supplied by passing
  startparse, a fn taking a source and a ContentHandler and returning
  a parser"
  ([s] (parse s startparse-sax))
  ([s startparse]
    (let [ip (atom (-> {:type :document :content nil} xml-zip (ip/insertion-point :append)))
          content-handler (handler ip)]
      (startparse s content-handler)
      (-> @ip ip/up-loc z/root :content first)))) 

