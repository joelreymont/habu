;;;; Keyword Argument Tests
;;;; Tests &key support in the Habu compiler.
;;;; Loaded via ASDF as part of habu/tests system.

(in-package :habu-test)

(defun run-keyword-tests ()
  "Run all keyword argument tests."
  (define-test-suite "Keyword Argument Tests"
    ;; Simple keyword with default
    (test-full "kw-default"
      "(defun add-with-offset (a &key (offset 0))
         (+ a offset))
       (sys-exit (add-with-offset 10))"
      10)

    ;; Keyword argument specified
    (test-full "kw-specified"
      "(defun add-with-offset (a &key (offset 0))
         (+ a offset))
       (sys-exit (add-with-offset 10 :offset 5))"
      15)

    ;; Multiple keyword arguments
    (test-full "kw-multiple"
      "(defun compute (base &key (mult 1) (add 0))
         (+ (* base mult) add))
       (sys-exit (compute 5 :mult 2 :add 3))"
      13)

    ;; Keyword without default
    (test-full "kw-no-default"
      "(defun foo (x &key y)
         (if y (+ x y) x))
       (sys-exit (foo 10 :y 7))"
      17)

    ;; Mix positional and keyword
    (test-full "kw-positional-mix"
      "(defun bar (a b &key c)
         (+ a b (if c c 0)))
       (sys-exit (bar 3 4 :c 5))"
      12)

    ;; Keyword arg rewriting at call site
    (test-full "kw-rewrite"
      "(defun shift-and-add (x &key (shift 0))
         (+ (ash x shift) 1))
       (sys-exit (shift-and-add 2 :shift 2))"
      9)))

;; Auto-run tests when file is loaded
(run-keyword-tests)
