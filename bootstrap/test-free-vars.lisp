;;;; Test free variable detection

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "~%Testing Free Variable Detection~%")
(format t "================================~%~%")

;; Test 1: No free variables
(format t "1. Lambda with no free variables: (lambda (x) (+ x 1))~%")
(let ((free-vars (find-free-variables '(+ x 1) '(x))))
  (format t "   Free variables: ~A~%" free-vars)
  (if (null free-vars)
      (format t "   [32m✓[0m Correct - no free variables~%")
      (format t "   [31m✗[0m Expected none, got ~A~%" free-vars)))

;; Test 2: One free variable
(format t "~%2. Lambda with one free variable: (lambda (y) (+ x y))~%")
(let ((free-vars (find-free-variables '(+ x y) '(y))))
  (format t "   Free variables: ~A~%" free-vars)
  (if (and (= (length free-vars) 1) (member 'x free-vars))
      (format t "   [32m✓[0m Correct - x is free~%")
      (format t "   [31m✗[0m Expected (x), got ~A~%" free-vars)))

;; Test 3: Multiple free variables
(format t "~%3. Lambda with multiple free variables: (lambda (z) (+ x (+ y z)))~%")
(let ((free-vars (find-free-variables '(+ x (+ y z)) '(z))))
  (format t "   Free variables: ~A~%" free-vars)
  (if (and (= (length free-vars) 2) (member 'x free-vars) (member 'y free-vars))
      (format t "   [32m✓[0m Correct - x and y are free~%")
      (format t "   [31m✗[0m Expected (x y), got ~A~%" free-vars)))

;; Test 4: Nested lambda
(format t "~%4. Nested lambda: (lambda (x) (lambda (y) (+ x y)))~%")
(let ((free-vars (find-free-variables '(lambda (y) (+ x y)) '(x))))
  (format t "   Free variables in outer lambda: ~A~%" free-vars)
  (if (null free-vars)
      (format t "   [32m✓[0m Correct - x is bound in outer scope~%")
      (format t "   [31m✗[0m Expected none, got ~A~%" free-vars)))

;; Test 5: Parse lambda to check IR type
(format t "~%5. Parse lambda - check IR type~%")
(let ((no-closure (parse '(lambda (x) (+ x 1))))
      (with-closure (parse '(lambda (y) (+ x y)))))
  (format t "   No free vars - type: ~A~%" (expr-type no-closure))
  (format t "   With free var - type: ~A~%" (expr-type with-closure))
  (if (and (eq (expr-type no-closure) 'lambda)
           (eq (expr-type with-closure) 'closure))
      (format t "   [32m✓[0m Correct - lambda vs closure~%")
      (format t "   [31m✗[0m Wrong types~%")))

(format t "~%Free variable detection tests complete!~%")

(sb-ext:quit)
