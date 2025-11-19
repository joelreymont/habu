;;;; Test defvar and symbol-value integration

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing defvar and symbol-value~%")
(format t "================================~%~%")

;; Test 1: Define global variables
(format t "1. Defining global variables~%")
(compile-expression '(defvar *x* 42) :arch :x86_64)
(compile-expression '(defvar *y* 100) :arch :x86_64)
(compile-expression '(defvar *z* 0) :arch :x86_64)
(format t "  (includes 0 - now supported!)~%")

;; Test 2: Read global variables using symbol-value
(format t "~%2. Reading global variables~%")
(let ((code1 (compile-expression '(symbol-value '*x*) :arch :x86_64))
      (code2 (compile-expression '(symbol-value '*y*) :arch :x86_64))
      (code3 (compile-expression '(symbol-value '*z*) :arch :x86_64)))
  (format t "  (symbol-value '*x*) => ~D bytes~%" (length code1))
  (format t "  (symbol-value '*y*) => ~D bytes~%" (length code2))
  (format t "  (symbol-value '*z*) => ~D bytes~%" (length code3)))

;; Test 3: Use symbol-value in expressions
(format t "~%3. Using symbol-value in expressions~%")
(let ((code1 (compile-expression '(+ (symbol-value '*x*) 10) :arch :x86_64))
      (code2 (compile-expression '(* (symbol-value '*x*) (symbol-value '*y*)) :arch :x86_64)))
  (format t "  (+ (symbol-value '*x*) 10) => ~D bytes~%" (length code1))
  (format t "  (* (symbol-value '*x*) (symbol-value '*y*)) => ~D bytes~%" (length code2)))

;; Test 4: Verify symbols are interned
(format t "~%4. Verifying symbol table~%")
(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime))))
  (dolist (name '("*X*" "*Y*" "*Z*"))
    (let ((sym (gethash name sym-table)))
      (if sym
          (let ((val-fn (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)))
            (let ((val (funcall val-fn sym)))
              (format t "  ✓ ~A = ~D (tagged: ~X, symbol: ~X)~%"
                      name (ash val -4) val sym)))
          (format t "  ✗ ~A not found~%" name)))))

;; Test 5: Error handling - unbound variable
(format t "~%5. Testing error handling~%")
(handler-case
    (progn
      (compile-expression '(symbol-value '*undefined*) :arch :x86_64)
      (format t "  ✗ Should have errored on unbound variable~%"))
  (error (e)
    (format t "  ✓ Correctly caught error: ~A~%" e)))

(format t "~%✓ All tests complete!~%")
(sb-ext:quit)
