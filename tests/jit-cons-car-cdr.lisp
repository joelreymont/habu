(load "run-habu.lisp")

(defun expect-eq (got expected label)
  (if (= got expected)
      (format t "[PASS] ~A -> ~A~%" label got)
      (error "[FAIL] ~A got ~A expected ~A" label got expected)))

(defun untag-fixnum (v) (/ v 16))

(defun test-cons-car-cdr ()
  (habu-sbcl:ensure-runtime-addrs)
  (let* ((cons-result (habu-sbcl:jit-eval '(cons 1 2)))
         (car-result (untag-fixnum (habu-sbcl:jit-eval '(car (cons 1 2)))))
         (cdr-result (untag-fixnum (habu-sbcl:jit-eval '(cdr (cons 1 2))))))
    (expect-eq car-result 1 "car(cons 1 2)")
    (expect-eq cdr-result 2 "cdr(cons 1 2)")
    (format t "[INFO] cons result raw: ~A~%" cons-result)))

(let ((jit-allowed (or (string= (or (sb-posix:getenv "HABU_JIT_TEST") "") "1")
                       (boundp '*enable-jit-smoke*) ; dev flag in run-habu.lisp
                       nil)))
  (cond
    ((not (member :arm64 *features*))
     (format t "[SKIP] ARM64-only JIT test.~%"))
    ((not jit-allowed)
     (format t "[SKIP] HABU_JIT_TEST not set; opt-in required.~%"))
    (t
     (test-cons-car-cdr))))
