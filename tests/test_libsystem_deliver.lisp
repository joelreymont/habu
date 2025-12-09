;;; Tests for deliver - native executables using libSystem
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-libsystem-deliver
  (:use :cl)
  (:import-from :habu #:deliver))
(in-package :habu-test-libsystem-deliver)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== libSystem Delivery Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-libsystem (name source expected-output expected-code)
  "Test deliver: builds executable, runs it, checks stdout and exit code."
  (handler-case
    (let ((output-path (format nil "/tmp/libsys_~A" name)))
      (deliver source output-path)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil
                                       :output :stream :error nil :wait t))
             (stdout (with-output-to-string (s)
                      (loop for line = (read-line (sb-ext:process-output proc) nil)
                            while line do (write-line line s))))
             (code (sb-ext:process-exit-code proc)))
        (if (and (string= (string-trim '(#\Newline #\Space) stdout)
                         (string-trim '(#\Newline #\Space) expected-output))
                 (= code expected-code))
            (progn (format t "[PASS] ~A~%" name)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected output ~S/code ~A, got ~S/~A~%"
                          name expected-output expected-code stdout code)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

(defun test-libsystem-code (name source expected-code)
  "Test deliver: builds executable, runs it, checks exit code only."
  (handler-case
    (let ((output-path (format nil "/tmp/libsys_~A" name)))
      (deliver source output-path)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (code (sb-ext:process-exit-code proc)))
        (if (= code expected-code)
            (progn (format t "[PASS] ~A = ~A~%" name code)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected-code code)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; Basic sys-write tests
(test-libsystem "write-hello" "(sys-write 1 \"Hello\" 5)" "Hello" 5)
(test-libsystem "write-world" "(sys-write 1 \"World!\" 6)" "World!" 6)
(test-libsystem "write-newline" "(sys-write 1 \"Hi
\" 3)" "Hi" 3)

;;; sys-write with heap allocation (string on heap)
(test-libsystem "write-heap-str" "(let ((s \"Test\")) (sys-write 1 s 4))" "Test" 4)

;;; Multiple writes
(test-libsystem "multi-write"
  "(progn (sys-write 1 \"A\" 1) (sys-write 1 \"B\" 1))"
  "AB" 1)

;;; No imports - uses direct execution
(test-libsystem-code "no-imports" "(+ 20 22)" 42)
(test-libsystem-code "no-imports-func" "(defun f (x) (* x 2)) (f 21)" 42)

;;; Report results
(format t "~%Total: ~A passed, ~A failed~%" *pass-count* *fail-count*)
(sb-ext:exit :code (if (zerop *fail-count*) 0 1))
