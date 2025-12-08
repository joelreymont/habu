;;; Test LAMBDA and FUNCALL compilation in h0-compile

;; Simple lambda with no free variables
(lambda (x) (+ x 1))

;; Lambda with free variable
(let ((y 10))
  (lambda (x) (+ x y)))

;; Funcall with literal lambda
(funcall (lambda (x) (* x 2)) 5)

;; Funcall with variable
(let ((f (lambda (x) (+ x 1))))
  (funcall f 10))

;; Nested lambdas
(lambda (x)
  (lambda (y)
    (+ x y)))
