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
  (:refer-clojure :exclude [complement]))

;; a state consists of a boolean (acceptance) and a function (from loc to state or nil)
;; nil is a valid state, it's the hopeless state

(let [s (create-struct :accept :transition-fn)]
  (defn state 
   ([accept transition-fn]
     (struct s accept transition-fn))
   ([accept transition-fn & etc]
     (apply assoc (state accept transition-fn) etc))))

(def accept (state true nil))
(def accept? :accept)

(def accept (state true nil))

(def descendants-or-self
  (state true (fn [_] descendants-or-self)))

(defn step
 "Returns the next state."  
 [s loc]
  (when-let [f (:transition-fn s)] (f loc)))

(defn union 
 "Returns a state machine which succeeds as soon as one of the specified state machines succeeds."
 ([] nil)
 ([& states]
   (state (some accept? states) 
     (let [states (remove nil? states)]
       (fn [loc] (apply union (map #(step % loc) states)))))))
  
(defn intersection
 "Returns a state machine which succeeds when all specified state machines succeed."
 ([] nil) 
 ([& states]
   (when-not (some nil? states)
     (state (every? accept? states)
       (fn [loc] (apply intersection (map #(step % loc) states)))))))

(defn complement 
 [s]
  (condp = s
    nil descendants-or-self
    descendants-or-self nil
    (state (not (accept? s))
      #(complement (step s %)))))
  
(defn complement-next
 [s]
  (state (accept? s)
    #(complement (step s %)))) 

(defn chain
  ([s] s)
  ([s1 s2]
    (when s1
      (let [chained-fn #(chain (step s1 %) s2)]  
        (if (accept? s1)
          (state (accept? s2) #(union (step s2 %) (chained-fn %)))
          (state false chained-fn)))))
  ([s1 s2 & etc] (reduce chain (chain s1 s2) etc)))

(defn pred 
 "Turns a predicate function on locs into a state-machine that accepts anything that satisfies the predicate."
 [f]
  (state false #(when-let [a (f %)] accept)))
