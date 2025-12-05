;;;; Match Macro Tests
;;;; Tests the pattern matching macro.

(in-package :habu-test)

(defun run-match-tests ()
  "Run all match macro tests."
  (define-test-suite "Match Macro Tests"
    ;; Wildcard pattern
    (test-full "match-wildcard"
      "(defun test (x)
         (match x
           (_ 42)))
       (sys-exit (test 99))"
      42)

    ;; Variable binding
    (test-full "match-var"
      "(defun test (x)
         (match x
           (y (+ y 1))))
       (sys-exit (test 41))"
      42)

    ;; nil literal
    (test-full "match-nil"
      "(defun test (x)
         (match x
           (nil 42)
           (_ 0)))
       (sys-exit (test nil))"
      42)

    ;; Number literal
    (test-full "match-number"
      "(defun test (x)
         (match x
           (1 10)
           (2 20)
           (3 42)
           (_ 0)))
       (sys-exit (test 3))"
      42)

    ;; Cons destructuring
    (test-full "match-cons"
      "(defun test (x)
         (match x
           ((cons a b) (+ a b))))
       (sys-exit (test (cons 20 22)))"
      42)

    ;; Nested cons
    (test-full "match-nested-cons"
      "(defun test (x)
         (match x
           ((cons a (cons b c)) (+ a (+ b c)))))
       (sys-exit (test (cons 10 (cons 12 20))))"
      42)

    ;; List pattern
    (test-full "match-list"
      "(defun test (x)
         (match x
           ((list a b c) (+ a (+ b c)))))
       (sys-exit (test (list 10 12 20)))"
      42)

    ;; Multiple clauses - first match wins
    (test-full "match-multi-clause"
      "(defun test (x)
         (match x
           (nil 0)
           ((cons 1 _) 42)
           ((cons _ _) 99)))
       (sys-exit (test (cons 1 2)))"
      42)

    ;; Fall through to later clause
    (test-full "match-fallthrough"
      "(defun test (x)
         (match x
           (nil 0)
           ((cons 1 _) 10)
           ((cons _ _) 42)))
       (sys-exit (test (cons 2 3)))"
      42)

    ;; No match returns nil (0 when used as exit code)
    (test-full "match-no-match"
      "(defun test (x)
         (match x
           ((cons _ _) 42)))
       (sys-exit (if (test 5) 1 0))"
      0)))

;; Auto-run tests when file is loaded
(run-match-tests)
