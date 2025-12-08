;; Comprehensive SETQ tests
;; These tests verify that SETQ properly mutates variable bindings

;; Test 1: Basic SETQ
;; Should return 2
(let ((x 1))
  (setq x 2)
  x)

;; Test 2: SETQ with computation
;; Should return 42
(let ((x 10))
  (setq x (+ x 32))
  x)

;; Test 3: Multiple SETQ in sequence
;; Should return 15
(let ((x 0))
  (setq x 5)
  (setq x (+ x 10))
  x)

;; Test 4: SETQ in nested let
;; Should return 100
(let ((x 1))
  (let ((y 99))
    (setq x y)
    (setq x (+ x 1)))
  x)

;; Test 5: SETQ returning the assigned value
;; Should return 42
(let ((x 0))
  (setq x 42))
