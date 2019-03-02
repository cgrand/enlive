(ns tst.net.cgrand.xml
  (:use tupelo.core tupelo.test)
  (:require
    [net.cgrand.xml :as xml]
    [tupelo.string :as ts]
  ))


(dotest
  (let [xml-str "<foo>
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

        xml-data (xml/parse (ts/string->stream xml-str))
        ]
    (spyx-pretty xml-data)

    ))

