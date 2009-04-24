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

;; a state consists of a boolean (acceptance) and a function (from loc to state or nil)

(let [s (create-struct :accept :transition-fn)]
  (defn state 
   ([accept transition-fn]
     (struct s accept transition-fn))
   ([accept transition-fn & etc]
     (apply assoc (state accept transition-fn) etc))))

(def accept? :accept)

(def fail (state false nil))

(def accept (state true nil))

(def descendants-or-self
  (state true (fn [_] descendants-or-self)))

(defn step
 "Returns the next state."  
 [s loc]
  (or (when-let [f (:transition-fn s)] (f loc)) fail))

(with-test 
  (defn union 
   "Returns a state machine which succeeds as soon as one of the specified state machines succeeds."
   ([] fail)
   ([& states]
     (state (some accept? states) 
       (let [states (remove #{fail} states)]
         (fn [loc] (apply union (map #(step % loc) states)))))))
       
  (is (accept? (step (union fail (state false (constantly accept))) :a)))
  (is (not (accept? (step (union fail (state false (constantly fail))) :a))))
  (is (accept? (step (union (state false (constantly fail)) (state false (constantly accept))) :a)))
  (is (accept? (step (union (state false (constantly accept)) (state false (constantly accept))) :a)))) 
  
(with-test
  (defn intersection
   "Returns a state machine which succeeds when all specified state machines succeed."
   ([] fail) 
   ([& states]
     (when-not (some #{fail} states)
       (state (every? accept? states)
         (fn [loc] (apply intersection (map #(step % loc) states)))))))
       
  (is (not (accept? (step (intersection fail (state false (constantly accept))) :a))))
  (is (not (accept? (step (intersection (state false (constantly fail)) (state false (constantly accept))) :a))))
  (is (accept? (step (intersection (state false (constantly accept)) (state false (constantly accept))) :a)))) 

(with-test
  (defn complement 
   [s]
    (condp = s
      fail descendants-or-self
      descendants-or-self fail
      (state (not (accept? s))
        #(complement (step s %)))))
  
  (is (accept? (complement fail)))
  (is (not (accept? (complement accept))))
  (is (accept? (step (complement fail) :a)))
  (is (accept? (step (complement accept) :a))))

(with-test
  (defn complement-next
   [s]
    (state (accept? s)
      #(complement (step s %)))) 

  (is (accept? (complement-next accept)))
  (is (not (accept? (complement-next fail))))
  (is (accept? (step (complement-next fail) :a)))
  (is (accept? (step (complement-next accept) :a))))

(with-test
  (defn chain
    ([s] s)
    ([s1 s2]
      (let [chained-fn #(chain (step s1 %) s2)]  
        (if (accept? s1)
          (state (accept? s2) #(union (step s2 %) (chained-fn %)))
          (state false chained-fn))))
    ([s1 s2 & etc] (reduce chain (chain s1 s2) etc)))
    
  (are (= _1 (boolean (accept? (reduce step (chain (state false #(state (= :a %1) nil)) (state false #(state (= :b %1) nil)))  _2))))
    true [:a :b]
    false [:a :c]
    false [:c :b]
    false [:a :a]
    false [:b :b]))

(defn pred 
 "Turns a predicate function on locs into a state-machine that accepts anything that satisfies the predicate."
 [f]
  (state false (fn [loc]
                 (state (f loc) nil))))
