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
  (is (accept? (step (union fail (state false (constantly accept))) :a)))
  (is (not (accept? (step (union fail (state false (constantly fail))) :a))))
  (is (accept? (step (union (state false (constantly fail)) (state false (constantly accept))) :a)))
  (is (accept? (step (union (state false (constantly accept)) (state false (constantly accept))) :a)))) 
  
(set-test intersection
  (is (not (accept? (step (intersection fail (state false (constantly accept))) :a))))
  (is (not (accept? (step (intersection (state false (constantly fail)) (state false (constantly accept))) :a))))
  (is (accept? (step (intersection (state false (constantly accept)) (state false (constantly accept))) :a)))) 

(set-test complement 
  (is (accept? (complement fail)))
  (is (not (accept? (complement accept))))
  (is (accept? (step (complement fail) :a)))
  (is (accept? (step (complement accept) :a))))

(set-test complement-next
  (is (accept? (complement-next accept)))
  (is (not (accept? (complement-next fail))))
  (is (accept? (step (complement-next fail) :a)))
  (is (accept? (step (complement-next accept) :a))))

(set-test chain
  (are (= _1 (boolean (accept? (reduce step (chain (state false #(state (= :a %1) nil)) (state false #(state (= :b %1) nil)))  _2))))
    true [:a :b]
    false [:a :c]
    false [:c :b]
    false [:a :a]
    false [:b :b]))
