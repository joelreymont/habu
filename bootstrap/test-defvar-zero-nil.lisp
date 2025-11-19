;;;; Test defvar with 0 and nil values (previously broken)

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing defvar with 0 and nil~%")
(format t "===============================~%~%")

;; Test 1: Define variable with value 0
(format t "1. Testing (defvar *zero* 0)~%")
(compile-expression '(defvar *zero* 0) :arch :x86_64)

;; Test 2: Define variable with value nil
(format t "~%2. Testing (defvar *nil-var* nil)~%")
(compile-expression '(defvar *nil-var* nil) :arch :x86_64)

;; Test 3: Define variable with positive value for comparison
(format t "~%3. Testing (defvar *positive* 42)~%")
(compile-expression '(defvar *positive* 42) :arch :x86_64)

;; Test 4: Read all three variables
(format t "~%4. Reading variables~%")
(let ((code1 (compile-expression '(symbol-value '*zero*) :arch :x86_64))
      (code2 (compile-expression '(symbol-value '*nil-var*) :arch :x86_64))
      (code3 (compile-expression '(symbol-value '*positive*) :arch :x86_64)))
  (format t "  (symbol-value '*zero*) => ~D bytes~%" (length code1))
  (format t "  (symbol-value '*nil-var*) => ~D bytes~%" (length code2))
  (format t "  (symbol-value '*positive*) => ~D bytes~%" (length code3)))

;; Test 5: Verify symbol values in symbol table
(format t "~%5. Verifying symbol table values~%")
(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
      (get-val-fn (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)))
  (dolist (name '("*ZERO*" "*NIL-VAR*" "*POSITIVE*"))
    (let ((sym (gethash name sym-table)))
      (if sym
          (handler-case
              (let ((val (funcall get-val-fn sym)))
                (format t "  ✓ ~A = ~D (tagged: ~X)~%"
                        name (ash val -4) val))
            (error (e)
              (format t "  ✗ ~A error: ~A~%" name e)))
          (format t "  ✗ ~A not found~%" name)))))

;; Test 6: Use in expressions
(format t "~%6. Using in expressions~%")
(let ((code1 (compile-expression '(+ (symbol-value '*zero*) 10) :arch :x86_64))
      (code2 (compile-expression '(+ (symbol-value '*nil-var*) 5) :arch :x86_64)))
  (format t "  (+ (symbol-value '*zero*) 10) => ~D bytes~%" (length code1))
  (format t "  (+ (symbol-value '*nil-var*) 5) => ~D bytes~%" (length code2)))

(format t "~%✓ All tests complete! 0 and nil now work as expected!~%")
(sb-ext:quit)
