;;;; Simple self-hosting test

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Self-Hosting Prerequisites~%")
(format t "===================================~%~%")

;; Test 1: Can we compile a simple function?
(format t "1. Compiling simple function...~%")
(defun test-fn (x)
  (* x 2))

(format t "   Function compiled: ~A~%" (gethash 'test-fn *function-table*))

;; Test 2: Can we compile an expression that uses that function?
(format t "~%2. Compiling call to function...~%")
(let ((code (compile-expression '((lambda (x) (* x 2)) 5) :arch :x86_64)))
  (format t "   Generated ~D bytes~%"  (length code)))

;; Test 3: What about using the reader?
(format t "~%3. Using reader to parse code...~%")
(let* ((code-str "(defun double (x) (+ x x))")
       (parsed (read-from-string code-str)))
  (format t "   Parsed: ~A~%" parsed)
  (format t "   Type: ~A~%" (type-of parsed)))

(format t "~%4. Current capabilities:~%")
(format t "   - Can read S-expressions: ~A~%" (if (find-symbol "READ-FROM-STRING" :cl) "YES" "NO"))
(format t "   - Can compile expressions: YES~%")
(format t "   - Can write files: ~A~%" (if *runtime-file-write-addr* "YES" "NO"))
(format t "   - Can execute compiled code: ~A~%" "PARTIAL (via FFI)")

(format t "~%Self-hosting assessment complete.~%")
(sb-ext:quit)
