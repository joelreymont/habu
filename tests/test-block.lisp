;;;; Block/Return-from Tests
;;;; Tests block and return-from for non-local exit.

(in-package :habu-test)

(defun run-block-tests ()
  "Run all block/return-from tests."
  (define-test-suite "Block/Return-from Tests"
    ;; Simple block without return-from
    (test-full "block-simple"
      "(sys-exit (block foo 42))"
      42)

    ;; Block with early return
    (test-full "block-return-from"
      "(sys-exit (block foo
                   (return-from foo 42)
                   99))"
      42)

    ;; Block with conditional return
    (test-full "block-conditional-return"
      "(sys-exit (block result
                   (if t
                       (return-from result 42)
                       (return-from result 0))
                   99))"
      42)

    ;; Nested blocks with different names
    (test-full "block-nested"
      "(sys-exit (block outer
                   (block inner
                     (return-from outer 42))
                   99))"
      42)

    ;; Return from inner block
    (test-full "block-return-inner"
      "(sys-exit (block outer
                   (+ (block inner
                        (return-from inner 40))
                      2)))"
      42)

    ;; nil-named block
    (test-full "block-nil-name"
      "(sys-exit (block nil
                   (return-from nil 42)
                   0))"
      42)

    ;; return shorthand (return-from nil)
    (test-full "block-return"
      "(sys-exit (block nil
                   (return 42)
                   0))"
      42)

    ;; Block in function
    (test-full "block-in-defun"
      "(defun test ()
         (block done
           (return-from done 42)
           0))
       (sys-exit (test))"
      42)

    ;; Return value from deeper nesting
    (test-full "block-deep-nesting"
      "(sys-exit (block result
                   (let ((x 10))
                     (if (> x 5)
                         (return-from result (+ x 32))
                         0))
                   99))"
      42)

    ;; Multiple returns (first one wins)
    (test-full "block-multiple-returns"
      "(sys-exit (block test
                   (return-from test 42)
                   (return-from test 0)
                   99))"
      42)))

;; Auto-run tests when file is loaded
(run-block-tests)
