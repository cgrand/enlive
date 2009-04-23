;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html.state-machine
  "state machine stuff for enlive-html"
  (:refer-clojure :exclude [complement])
  (:use [clojure.contrib.test-is :as test-is :only [set-test with-test is are]]))

;; a state is a pair consisting of a boolean (acceptance) and a seq of functions (functions from loc to state)

(def accept? first)

(defn hopeless?
 "Returns true if the state machine cannot succeed. (It's not a necessary condition.)" 
 [state] (empty? (second state)))

(defn step
 "Returns the next state."  
 [state loc]
  (let [states (map #(% loc) (second state))]
    [(some accept? states) (mapcat second states)]))    

(with-test 
  (defn union 
   "Returns a state machine which succeeds as soon as one of the specified state machines succeeds."
   [& states]
    [(some accept? states) (mapcat second states)])
       
  (is (accept? (step (union [false nil] [false [(constantly [true nil])]]) :a)))
  (is (not (accept? (step (union [false nil] [false [(constantly [false nil])]]) :a))))
  (is (accept? (step (union [false [(constantly [false nil])]] [false [(constantly [true nil])]]) :a)))
  (is (accept? (step (union [false [(constantly [true nil])]] [false [(constantly [true nil])]]) :a)))) 
  
(with-test
  (defn intersection
   "Returns a state machine which succeeds when all specified state machines succeed." 
   [& states]
    [(every? accept? states)
     (when (seq (remove hopeless? states))
       [(fn [loc] (apply intersection (map #(step % loc) states)))])])
       
  (is (not (accept? (step (intersection [false nil] [false [(constantly [true nil])]]) :a))))
  (is (not (accept? (step (intersection [false [(constantly [false nil])]] [false [(constantly [true nil])]]) :a))))
  (is (accept? (step (intersection [false [(constantly [true nil])]] [false [(constantly [true nil])]]) :a)))) 

(defn complement 
 [[x fs]]
  [(not x) (map (partial comp complement) fs)])

(defn complement-next
 [state]
  [(accept? state) (second (complement state))]) 

(with-test
  (defn chain 
    ([s] s)
    ([[x1 fns1] s2]
      (let [chained-fns1 (map #(fn [loc] (chain (% loc) s2)) fns1)]
        (if x1
          [(accept? s2) (concat (second s2) chained-fns1)]
          [false chained-fns1])))
    ([s1 s2 & etc] (reduce chain (chain s1 s2) etc)))
    
  (are (= _1 (boolean (accept? (reduce step (chain [false [#(vector (= :a %1) nil)]] [false [#(vector (= :b %1) nil)]])  _2))))
    true [:a :b]
    false [:a :c]
    false [:c :b]
    false [:a :a]
    false [:b :b]))

(def descendants-or-self
  [true (lazy-seq [(constantly descendants-or-self)])])

(defn pred 
 "Turns a predicate function on locs into a state-machine that accepts anything that satisfies the predicate."
 [f]
  [false [(fn [loc]
            [(f loc) nil])]])

  