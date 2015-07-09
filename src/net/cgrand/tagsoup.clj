;   Copyright (c) Christophe Grand, 2009-2013. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.tagsoup
  (:require [net.cgrand.xml :as xml]))

(defn- startparse-tagsoup [s ch]
  (doto (org.ccil.cowan.tagsoup.Parser.)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/ignorable-whitespace" true)
    (.setContentHandler ch)
    (.setProperty "http://www.ccil.org/~cowan/tagsoup/properties/auto-detector"
      (proxy [org.ccil.cowan.tagsoup.AutoDetector] []
        (autoDetectingReader [^java.io.InputStream is]
          (java.io.InputStreamReader. is "UTF-8"))))
    (.setProperty "http://xml.org/sax/properties/lexical-handler" ch)
    (.parse s)))

(defn parser 
 "Loads and parse an HTML resource and closes the stream."
 [stream]
  (when-not stream 
    (throw (NullPointerException. "HTML resource not found.")))
  (filter map?
    (with-open [^java.io.Closeable stream stream]
      (xml/parse (org.xml.sax.InputSource. stream) startparse-tagsoup))))

