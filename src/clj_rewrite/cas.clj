
(ns clj-rewrite.cas
  "Simple computer algebraic system"
  (:use clj-rewrite.dsl)
  (:use clj-rewrite.core))

;; TODO: move into a separate project


(def-rewrite multiply-fractions
  (* a* (/ b f) c* (/ d g) e*)    (/ (* a b c d e) (* f g)))

(def-rewrite additive-inverse
  (+ a* x b* (* -1 x) c*)    (+ a b c))

(def-rewrite identities
  (+ a* 0 b*)   (+ a b)
  (* a* 1 b*)   (* a b)
  (* a* 0 b*)   0
  (* a)         a
  (+ b)         b)


(defn free-of?
  "Indicates if t is not some complete subexpression of e"
  [e t]
  (if (seq? e)
    (and
     (every? #(free-of? % t) e)
     (not= e t))
    (not= e t)))

(def-rewrite d
  "Differentiation rules"
  (d x _) :when (number? x)     0 
  (d x x)                       1
  (d x y) :when (free-of? x y)  0
  (d (+ e1 e2) v)               (+ (d e1 v) (d e2 v)))
