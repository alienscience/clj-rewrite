
(ns 'clj-rewrite.cas
  "Simple computer algebraic system"
  (:use clj-rewrite.dsl)
  (:use clj-rewrite.core))

;; TODO: move into a separate project


(def-rewrite multiply-fractions
  (* a* (/ b f) c* (/ d g) e*)    (/ (* a b c d e) (* f g)))

(def-rewrite additive-inverse
  (+ a* x b* (* -1 x) c*)    (+ a b c)))

;; TODO: have __ match a*
(def-rewrite identities
  (+ a* 0 b*)   (+ a b)
  (* a* 1 b*)   (* a b)
  (* a* 0 b*)   0
  (* a)         a
  (+ b)         b)
