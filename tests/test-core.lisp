;;;; Core Compiler Tests
;;;; Tests basic arithmetic, control flow, and data structures.
;;;; Loaded via ASDF as part of habu/tests system.

(in-package :habu-test)

(defun run-core-tests ()
  "Run all core compiler tests."
  (define-test-suite "Core Compiler Tests"
    ;; Arithmetic
    (test "add" "(+ 20 22)" 42)
    (test "sub" "(- 100 58)" 42)
    (test "mul" "(* 6 7)" 42)
    (test "div" "(/ 84 2)" 42)
    (test "mod" "(mod 47 5)" 2)
    (test "nested-arith" "(+ (* 3 4) (+ 5 7))" 24)

    ;; Comparisons
    (test "eq-t" "(if (= 5 5) 42 0)" 42)
    (test "eq-f" "(if (= 5 6) 0 42)" 42)
    (test "lt" "(if (< 3 5) 42 0)" 42)
    (test "gt" "(if (> 7 5) 42 0)" 42)
    (test "le" "(if (<= 5 5) 42 0)" 42)
    (test "ge" "(if (>= 5 5) 42 0)" 42)

    ;; Let bindings
    (test "let" "(let ((x 42)) x)" 42)
    (test "let-star" "(let* ((x 6) (y (* x 7))) y)" 42)
    (test "let-nested" "(let ((x 10)) (let ((y 32)) (+ x y)))" 42)
    (test "let-multi" "(let ((a 10) (b 20) (c 12)) (+ a (+ b c)))" 42)

    ;; Cons cells
    (test "car" "(car (cons 42 0))" 42)
    (test "cdr" "(cdr (cons 0 42))" 42)
    (test "cadr" "(car (cdr (cons 1 (cons 42 nil))))" 42)
    (test "quote-list" "(car (cdr (cdr (quote (1 2 42)))))" 42)

    ;; Defun
    (test-full "defun-simple"
      "(defun foo () 42) (sys-exit (foo))" 42)
    (test-full "defun-args"
      "(defun add (x y) (+ x y)) (sys-exit (add 20 22))" 42)
    (test-full "defun-recursive"
      "(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) (sys-exit (fact 5))" 120)

    ;; Conditionals
    (test "if-then" "(if t 42 0)" 42)
    (test "if-else" "(if nil 0 42)" 42)
    (test "when" "(when t 42)" 42)
    (test "unless" "(unless nil 42)" 42)

    ;; Boolean logic
    (test "and-t" "(if (and t t) 42 0)" 42)
    (test "and-f" "(if (and t nil) 0 42)" 42)
    (test "or-t" "(if (or nil t) 42 0)" 42)
    (test "or-f" "(if (or nil nil) 0 42)" 42)
    (test "not" "(if (not nil) 42 0)" 42)

    ;; Bitwise
    (test "logand" "(logand 255 15)" 15)
    (test "logior" "(logior 8 4)" 12)
    (test "logxor" "(logxor 15 7)" 8)
    (test "ash-left" "(ash 1 4)" 16)
    (test "ash-right" "(ash 16 -2)" 4)))

;; Auto-run tests when file is loaded
(run-core-tests)
