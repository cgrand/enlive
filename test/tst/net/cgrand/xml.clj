(ns tst.net.cgrand.xml
  (:use clojure.test)
  (:require
    [clojure.java.io :as io]
    [net.cgrand.xml :as xml] ))

(deftest t-xml-7
  (let [xml-str  "<foo>
                    <name>John</name>
                    <address>1 hacker way</address>
                    <phone></phone>
                    <school>
                        <name>Joe</name>
                        <state>CA</state>
                        <type>FOOBAR</type>
                    </school>
                    <college>
                        <name>mit</name>
                        <address></address>
                        <state>Denial</state>
                    </college>
                  </foo> "

        xml-data (xml/parse (io/input-stream (.getBytes xml-str))) ]
    (is (= xml-data
          [{:tag   :foo,
            :attrs nil,
            :content
             [ "\n                    "
              {:tag :name, :attrs nil, :content ["John"]}
              "\n                    "
              {:tag :address, :attrs nil, :content ["1 hacker way"]}
              "\n                    "
              {:tag :phone, :attrs nil, :content nil}
              "\n                    "
              {:tag   :school,
               :attrs nil,
               :content
                ["\n                        "
                 {:tag :name, :attrs nil, :content ["Joe"]}
                 "\n                        "
                 {:tag :state, :attrs nil, :content ["CA"]}
                 "\n                        "
                 {:tag :type, :attrs nil, :content ["FOOBAR"]}
                 "\n                    "]}
              "\n                    "
              {:tag   :college,
               :attrs nil,
               :content
                ["\n                        "
                 {:tag :name, :attrs nil, :content ["mit"]}
                 "\n                        "
                 {:tag :address, :attrs nil, :content nil}
                 "\n                        "
                 {:tag :state, :attrs nil, :content ["Denial"]}
                 "\n                    "]}
              "\n                  "]}])) ))

