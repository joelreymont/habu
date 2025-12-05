;;;; Macro System Tests
;;;; Tests defmacro, macroexpand, and macro-function.

(in-package :habu-test)

(defun run-macro-tests ()
  "Run all macro system tests."
  (define-test-suite "Macro System Tests"
    ;; Simple macro with no parameters
    (test-full "macro-simple"
      "(defmacro always-42 () 42)
       (sys-exit (always-42))"
      42)

    ;; Macro with single parameter
    (test-full "macro-one-param"
      "(defmacro double (x) (list '+ x x))
       (sys-exit (double 21))"
      42)

    ;; Macro with multiple parameters
    (test-full "macro-multi-params"
      "(defmacro add3 (a b c) (list '+ a (list '+ b c)))
       (sys-exit (add3 10 20 12))"
      42)

    ;; Macro expanding to function call
    (test-full "macro-funcall"
      "(defun helper (x) (+ x 1))
       (defmacro inc (x) (list 'helper x))
       (sys-exit (inc 41))"
      42)

    ;; Nested macro calls
    (test-full "macro-nested"
      "(defmacro inc (x) (list '+ x 1))
       (defmacro double (x) (list '+ x x))
       (sys-exit (inc (double 20)))"
      41)

    ;; Macro in let binding
    (test-full "macro-in-let"
      "(defmacro square (x) (list '* x x))
       (sys-exit (let ((a (square 6)))
                   (+ a 6)))"
      42)

    ;; Macro in function body
    (test-full "macro-in-defun"
      "(defmacro add1 (x) (list '+ x 1))
       (defun test (n)
         (add1 n))
       (sys-exit (test 41))"
      42)

    ;; Macro with quasiquote-like expansion
    (test-full "macro-complex-expansion"
      "(defmacro when-positive (val then)
         (list 'if (list '> val 0) then 0))
       (sys-exit (when-positive 5 42))"
      42)

    (test-full "macro-when-negative"
      "(defmacro when-positive (val then)
         (list 'if (list '> val 0) then 0))
       (sys-exit (when-positive -5 42))"
      0)

    ;; Macro in recursive function
    (test-full "macro-in-recursive"
      "(defmacro dec (x) (list '- x 1))
       (defun countdown (n)
         (if (= n 0)
             42
             (countdown (dec n))))
       (sys-exit (countdown 10))"
      42)))

;; Auto-run tests when file is loaded
(run-macro-tests)
