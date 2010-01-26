(declare layout)
(def lookup {"layout" #'layout})

(defn parent-template [nodes]
  (when-let [id (-> nodes 
                  (select [[:meta (attr= :name "parent")]])
                  first :attrs :content)]
    (lookup id)))

(defmacro inheriting [source args & forms]
  `(let [source# (html-resource ~source)
         parent# (parent-template source#)]
     (comp (or parent# emit*) (snippet* source# ~args ~@forms))))

(def layout (inheriting "net/cgrand/enlive_html/examples/inheritance/layout.html" [child]
  [:col1] (substitute (select child [:col1 :> :*]))
  [:col2] (substitute (select child [:col2 :> :*]))
  [:col3] (substitute (select child [:col3 :> :*]))))

(def child (inheriting "net/cgrand/enlive_html/examples/inheritance/child.html" [msg]
[:col2 :div] (content msg)))

(println (apply str (child "hello")))

(comment
<html>
   <head></head>
   <body>
       <header></header>
       <main>
           <div>ccol1</div>
           <div>hello</div>
           <div>ccol3</div>
       </main>
       <footer></footer>
   </body>
</html>
)

