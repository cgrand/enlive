;   Copyright (c) Christophe Grand, 2009. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.cgrand.enlive-html.state-machine.test
  (:refer-clojure :exclude [complement])
  (:use net.cgrand.enlive-html.state-machine)
  (:use [clojure.contrib.test-is :as test-is :only [set-test is are]]))

(set-test union 
  (is (accept? (step (union nil (state false (constantly accept))) :a)))
  (is (not (accept? (step (union nil (state false (constantly nil))) :a))))
  (is (accept? (step (union (state false (constantly nil)) (state false (constantly accept))) :a)))
  (is (accept? (step (union (state false (constantly accept)) (state false (constantly accept))) :a)))) 
  
(set-test intersection
  (is (not (accept? (step (intersection nil (state false (constantly accept))) :a))))
  (is (not (accept? (step (intersection (state false (constantly nil)) (state false (constantly accept))) :a))))
  (is (accept? (step (intersection (state false (constantly accept)) (state false (constantly accept))) :a)))) 

(set-test complement 
  (is (accept? (complement nil)))
  (is (not (accept? (complement accept))))
  (is (accept? (step (complement nil) :a)))
  (is (accept? (step (complement accept) :a))))

(set-test complement1
  (is (accept? (complement1 accept)))
  (is (not (accept? (complement1 nil))))
  (is (accept? (step (complement1 nil) :a)))
  (is (accept? (step (complement1 accept) :a))))

(set-test chain
  (are (= _1 (boolean (accept? (reduce step (chain (state false #(state (= :a %1) nil)) (state false #(state (= :b %1) nil)))  _2))))
    true [:a :b]
    false [:a :c]
    false [:c :b]
    false [:a :a]
    false [:b :b]))
