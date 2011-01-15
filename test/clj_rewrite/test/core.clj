
(ns clj-rewrite.test.core
  "Test of term rewriting"
  (:use clojure.test)
  (:use clj-rewrite.core))

(defn- double-up
  "If the given expression is (+ x x) returns (* 2 x)"
  [e]
  (if (and (seq? e)
           (= 3 (count e))
           (= '+ (first e))
           (= (nth e 1) (nth e 2)))
    (list '* 2 (nth e 1))))

(defn- associate
  "If the given expression is (* x (* y z)) returns (* x y z)"
  [e]
  (if (and (seq? e)
           (= 3 (count e))
           (= '* (first e)))
    (let [inner (nth e 2)]
      (if (and (seq? inner)
               (= 3 (count inner))
               (= '* (first inner)))
        (list '* (nth e 1) (nth inner 1) (nth inner 2))))))

(deftest rewrite-fns
  (let [e '(+ (+ a a) (+ a a))]
    (is (= (rewrite e [double-up])
           '(* 2 (* 2 a))))
    (is (= (rewrite e [double-up associate])
           '(* 2 2 a)))))
