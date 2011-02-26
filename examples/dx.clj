
;; IMPORTANT: reorder as a function



;; Example of distributive rule
(def-rewrite distributive-rule
  (* a* (+ xs*) b*)     (+ (foreach x xs (* a x b))))

;; Above rule in functional form
(rule ['* (match* :a) ['+ (match* :xs)] (match* :b)]
      (build-list '+
                  (fn [m]
                    (for [x (:xs m)]
                      (build-list '* (sub :a) x (sub :b))))))

;; Multiply fractions
(def-rewrite multiply-fractions
  (* a* (/ b f) c* (/ d g) e*)   (/ (* a b c d e) (* f g)))

;; Example of additive inverse
(def-rewrite additive-inverse
  (+ a* x b* (* -1 x) c*)   (+ a b c))

;; Identities
(def-rewrite identities
  (+ a* 0 b*)   (+ a b)
  (* a* 1 b*)   (* a b)
  (* __ 0 __)   0)

;; Top rule above in functional form
(rule ['* (match* :a) 0 (match* :b)]
      (build-list '+ (sub :a) (sub :b)))


;; Identities - 2-ary rules, O(e) perf?
(def-edit identities
  (+ a 0)     a
  (+ 0 a)     a
  (- a 0)     a
  (* a 1)     a
  (* 1 a)     a
  (* _ 0)     0
  (* 0 _)     0)

;; Example of differentiation

(def-rewrite d
  "Differentiation rules"
  (d x _) :when (number? x)     0 
  (d x x)                       1
  (d x y) :when (free-of? x y)  0
  (d (+ xs*) v)                 (+ (foreach x xs (d x v))))

;; Last rule in functional form
(rule ['d ['+ (match* :xs)] (match :v)]
      (build-list '+
                  (foreach :x :xs
                           (build-list 'd (sub :x) (sub :v)))))  
