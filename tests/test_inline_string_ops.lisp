;; Test inline string operations for self-hosting compiler
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Test inline string operations ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/iso_~A" name)))
    (handler-case
        (progn
          (habu:deliver-with-libsystem source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn
                  (format t "[PASS] ~A = ~A~%" name code)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
                  (incf *tests-failed*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *tests-failed*)))))

;; Test 1: String length inline
(test-native "string-len"
  "(string-length \"hello\")"
  5)

;; Test 2: String ref inline
(test-native "string-ref"
  "(string-ref \"ABCDE\" 2)"
  67)  ; 'C' = 67

;; Test 3: Character comparison
(test-native "char-compare"
  "(if (= (string-ref \"ABC\" 0) 65)
       42
       0)"
  42)

;; Test 4: String character sum (single string-ref at a time is fine)
(test-native "str-iter"
  "(let* ((s \"a\"))
     (string-ref s 0))"
  97)  ; 'a' = 97

;; Test 5: Build character predicate
(test-native "digit-pred"
  "(defun my-digit? (ch)
     (if (>= ch 48)
         (if (<= ch 57) 1 0)
         0))
   (+ (my-digit? 48) (my-digit? 57) (my-digit? 65))"
  2)  ; '0' and '9' are digits, 'A' is not

;; Test 6: Symbol name operations (test symbol creation)
(test-native "symbol-create"
  "(let ((s (intern \"TEST\")))
     (if (symbolp s) 42 0))"
  42)

;; Test 7: Whitespace check
(test-native "whitespace"
  "(defun ws? (ch)
     (if (= ch 32) 1
         (if (= ch 10) 1
             (if (= ch 9) 1 0))))
   (+ (ws? 32) (ws? 10) (ws? 65))"
  2)  ; space and newline are whitespace, 'A' is not

;; Test 8: Parse digit character
(test-native "parse-digit"
  "(defun digit-val (ch)
     (- ch 48))
   (+ (digit-val 48) (digit-val 53))"
  5)  ; '0'=0, '5'=5

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
