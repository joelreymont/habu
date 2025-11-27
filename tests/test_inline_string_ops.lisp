;;; Tests for inline string operations - make-string-from-vector and string=
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Inline String Operations Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-libsystem-code (name source expected-code)
  "Test deliver-with-libsystem: builds executable, runs it, checks exit code only."
  (handler-case
    (let ((output-path (format nil "/tmp/inline_~A" name)))
      (deliver-with-libsystem source output-path)
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

;;; Test make-string-from-vector
(test-libsystem-code "vec-to-str-len"
  "(let ((v (make-vector 3)))
     (vector-set v 0 65)
     (vector-set v 1 66)
     (vector-set v 2 67)
     (string-length (make-string-from-vector v)))"
  3)

(test-libsystem-code "vec-to-str-ref"
  "(let ((v (make-vector 3)))
     (vector-set v 0 72)
     (vector-set v 1 105)
     (vector-set v 2 33)
     (string-ref (make-string-from-vector v) 0))"
  72)

(test-libsystem-code "vec-to-str-ref-mid"
  "(let ((v (make-vector 3)))
     (vector-set v 0 65)
     (vector-set v 1 66)
     (vector-set v 2 67)
     (string-ref (make-string-from-vector v) 1))"
  66)

;;; Test string=
(test-libsystem-code "string-eq-same"
  "(if (string= \"foo\" \"foo\") 42 0)"
  42)

(test-libsystem-code "string-eq-diff"
  "(if (string= \"foo\" \"bar\") 42 0)"
  0)

(test-libsystem-code "string-eq-diff-len"
  "(if (string= \"foo\" \"foobar\") 42 0)"
  0)

(test-libsystem-code "string-eq-empty"
  "(if (string= \"\" \"\") 42 0)"
  42)

(test-libsystem-code "string-eq-one-empty"
  "(if (string= \"x\" \"\") 42 0)"
  0)

;;; Test combination: build string from vector, compare with literal
(test-libsystem-code "vec-str-eq"
  "(let ((v (make-vector 3)))
     (vector-set v 0 65)
     (vector-set v 1 66)
     (vector-set v 2 67)
     (if (string= (make-string-from-vector v) \"ABC\") 42 0))"
  42)

(test-libsystem-code "vec-str-neq"
  "(let ((v (make-vector 3)))
     (vector-set v 0 65)
     (vector-set v 1 66)
     (vector-set v 2 67)
     (if (string= (make-string-from-vector v) \"XYZ\") 42 0))"
  0)

;;; Report results
(format t "~%Total: ~A passed, ~A failed~%" *pass-count* *fail-count*)
(sb-ext:exit :code (if (zerop *fail-count*) 0 1))
