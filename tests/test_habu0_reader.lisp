;;; Test habu0 native reader - tests that habu0 correctly parses Lisp expressions
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-habu0-reader
  (:use :cl)
  (:import-from :habu #:deliver-file))
(in-package :habu-test-habu0-reader)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test habu0 Reader ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun compile-and-build ()
  "Build habu0 executable once for all tests"
  (handler-case
      (progn
        (deliver-file "habu0.lisp" "/tmp/habu0-test" :verbose nil)
        (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" "-f" "/tmp/habu0-test")
                            :output nil :error nil :wait t)
        t)
    (error (e)
      (format t "Build failed: ~A~%" e)
      nil)))

(defun test-habu0 (name input expected)
  "Write input to file, run habu0, check exit code"
  (with-open-file (f "/tmp/habu0-input.lisp" :direction :output :if-exists :supersede)
    (write-string "#x100 " f)
    (write-string input f))
  ;; Update habu0 to read from /tmp/habu0-input.lisp
  (let* ((proc (sb-ext:run-program "/tmp/habu0-test" nil :output nil :error nil :wait t))
         (code (sb-ext:process-exit-code proc)))
    (if (= code expected)
        (progn
          (format t "[PASS] ~A = ~A~%" name code)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
          (incf *tests-failed*)))))

;; Since habu0 reads from input.lisp, we need to modify input.lisp for each test
(defun test-with-input (name input expected)
  "Write to input.lisp, run habu0, check exit code"
  (with-open-file (f "input.lisp" :direction :output :if-exists :supersede)
    (format f "#x100 ~A~%" input))
  (let* ((proc (sb-ext:run-program "./habu0" nil :output nil :error nil :wait t))
         (code (sb-ext:process-exit-code proc)))
    (if (= code expected)
        (progn
          (format t "[PASS] ~A = ~A~%" name code)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
          (incf *tests-failed*)))))

;; Build habu0 first if needed
(unless (probe-file "habu0")
  (format t "Building habu0...~%")
  (deliver-file "habu0.lisp" "habu0" :verbose nil)
  (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" "-f" "habu0")
                      :output nil :error nil :wait t))

;;; Reader tests - these test habu0's ability to parse various Lisp forms

;; Test 1: Hex numbers
(test-with-input "hex-number" "#x2A" 42)

;; Test 2: Decimal numbers
(test-with-input "decimal-number" "42" 42)

;; Test 3: Negative hex
(test-with-input "negative-hex" "(- #x0 #xD6)" 42)  ;; 0 - (-42) but habu doesn't support negative literals directly

;; Test 4: Simple arithmetic
(test-with-input "simple-add" "(+ #x1 #x2)" 3)

;; Test 5: Nested arithmetic
(test-with-input "nested-arith" "(+ (* #x3 #x4) (+ #x5 #x7))" 24)

;; Test 6: Simple let
(test-with-input "simple-let" "(let ((x #x2A)) x)" 42)

;; Test 7: Multiple let bindings
(test-with-input "multi-let" "(let ((x #xA) (y #x14)) (+ x y))" 30)

;; Test 8: Nested let
(test-with-input "nested-let" "(let ((x #x5)) (let ((y #x3)) (* x y)))" 15)

;; Test 9: Symbols parsing
(test-with-input "symbol-var" "(let ((foo #x10) (bar #x12)) (+ foo bar))" 42)

;; Test 10: Quoted symbol
(test-with-input "quoted-sym" "(if (eq 'foo 'foo) #x2A #x0)" 42)

;; Test 11: If expression
(test-with-input "if-true" "(if (= #x1 #x1) #x2A #x0)" 42)

;; Test 12: If false branch
(test-with-input "if-false" "(if (= #x1 #x2) #x0 #x2A)" 42)

;; Test 13: Cons and car
(test-with-input "cons-car" "(car (cons #x2A #x0))" 42)

;; Test 14: Cons and cdr
(test-with-input "cons-cdr" "(cdr (cons #x0 #x2A))" 42)

;; Test 15: List operations
(test-with-input "list-car" "(car (list #x2A #x1 #x2))" 42)

;; Test 16: Comparison less-than
(test-with-input "cmp-lt" "(if (< #x1 #x2) #x2A #x0)" 42)

;; Test 17: Comparison greater-than
(test-with-input "cmp-gt" "(if (> #x3 #x2) #x2A #x0)" 42)

;; Test 18: Null check
(test-with-input "null-nil" "(if (null nil) #x2A #x0)" 42)

;; Test 19: Null non-nil
(test-with-input "null-cons" "(if (null (cons #x1 #x2)) #x0 #x2A)" 42)

;; Test 20: Consp check
(test-with-input "consp-true" "(if (consp (cons #x1 #x2)) #x2A #x0)" 42)

;; Restore input.lisp
(with-open-file (f "input.lisp" :direction :output :if-exists :supersede)
  (format f "#x100 (let ((x 42)) x)~%"))

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
