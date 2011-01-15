
;; Example of reorder

(def-rewrite reorder
  "Reorder in DSL form"
  [op a* x y b*]
  :when
  (and (contains? #{'+ '*} op)
       (not (inorder? x y)))      :-> (op a y x b))

;; Example of distributive rule
(def-rewrite distributive-rule
  ['* a* ['+ &x] b*]           :-> ('+ (&x ('* a x b))))

;; Above rule in functional form
(rule ['* (match* :a) ['+ (each :x)] (match* :b)]
      (build-list '+
                  (for-all :x
                           (build-list '* (sub :a) (sub :x) (sub :b)))))

;; Multiply fractions
(def-rewrite multiply-fractions
  ['* a* ['/ b f] c* ['/ d g] e*] :-> ('/ ('* a b c d e) ('* f g)))

;; Example of additive inverse
(def-rewrite additive-inverse
  ['+ a* x b* ['* -1 x] c*]       :-> ('+ a b c))

;; Identities
(def-rewrite identities
  ['+ a* 0 b*]  :-> ('+ a b)
  ['* a* 1 b*]  :-> ('* a b)
  ['* .* 0 .*]  :-> 0)

;; Top rule above in functional form
(rule ['* (match* a) 0 (match* b)]
      (build-list '+ (sub :a) (sub :b)))

;; Example of differentiation

(def-rewrite d
  "Differentiation rules"
  ['d x .] :when (number? x)     :-> 0 
  ['d x x]                       :-> 1
  ['d x y] :when (free-of? x y)  :-> 0
  ['d [+ e1 e2] v]               :-> ('+ ('d e1 v) ('d e2 v)))

