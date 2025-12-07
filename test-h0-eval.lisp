;; Load habu0 functions
(load "habu0.lisp")

(defun test-main ()
  (init-compile-ops)
  ;; Skip file reading, just test h0-eval directly
  (h0-eval 42 nil nil))

(test-main)
