;;; Test keyword argument (&key) support in Habu compiler
;;; Load order based on ASDF dependencies
(load "arm64/asm.lisp")
(load "bootstrap/compiler-sbcl.lisp")
(load "bootstrap/optimize.lisp")
(load "bootstrap/gc.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/codegen.lisp")
(in-package :habu)

(format t "~%=== Keyword Argument Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-compile-full (name source expected)
  "Test compilation with full source (already has sys-exit)."
  (handler-case
    (let ((output-path (format nil "/tmp/test_kw_~A" name)))
      (deliver source output-path)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))
        (ignore-errors (delete-file output-path))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; Test 1: Simple keyword argument with default
(test-compile-full "kw-default"
  "(defun add-with-offset (a &key (offset 0))
     (+ a offset))
   (sys-exit (add-with-offset 10))"
  10)

;;; Test 2: Keyword argument specified
(test-compile-full "kw-specified"
  "(defun add-with-offset (a &key (offset 0))
     (+ a offset))
   (sys-exit (add-with-offset 10 :offset 5))"
  15)

;;; Test 3: Multiple keyword arguments
(test-compile-full "kw-multiple"
  "(defun compute (base &key (mult 1) (add 0))
     (+ (* base mult) add))
   (sys-exit (compute 5 :mult 2 :add 3))"
  13)

;;; Test 4: Keyword arguments without defaults
(test-compile-full "kw-no-default"
  "(defun foo (x &key y)
     (if y (+ x y) x))
   (sys-exit (foo 10 :y 7))"
  17)

;;; Test 5: Mix positional and keyword
(test-compile-full "kw-positional-mix"
  "(defun bar (a b &key c)
     (+ a b (if c c 0)))
   (sys-exit (bar 3 4 :c 5))"
  12)

;;; Test 6: Keyword arg at call site only (test rewriting)
(test-compile-full "kw-rewrite"
  "(defun shift-and-add (x &key (shift 0))
     (+ (ash x shift) 1))
   (sys-exit (shift-and-add 2 :shift 2))"
  9)

(format t "~%Results: ~A passed, ~A failed~%" *pass-count* *fail-count*)
(when (> *fail-count* 0)
  (sb-ext:exit :code 1))
