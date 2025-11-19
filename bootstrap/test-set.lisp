;;;; Test set for modifying global variables

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing set for Global Variables~%")
(format t "=================================~%~%")

;; Test 1: Define a variable
(format t "1. Define initial variable~%")
(compile-expression '(defvar *counter* 0) :arch :x86_64)
(format t "  Initial value: ~A~%~%"
        (ash (funcall (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)
                     (funcall (find-symbol "RUNTIME-INTERN" :habu-runtime) "*COUNTER*"))
             -4))

;; Test 2: Modify with set
(format t "2. Modify with set~%")
(compile-expression '(set '*counter* 10) :arch :x86_64)
(let ((new-val (ash (funcall (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)
                             (funcall (find-symbol "RUNTIME-INTERN" :habu-runtime) "*COUNTER*"))
                   -4)))
  (format t "  After (set '*counter* 10): ~A~%~%" new-val))

;; Test 3: Modify again
(format t "3. Modify again~%")
(compile-expression '(set '*counter* 42) :arch :x86_64)
(let ((new-val (ash (funcall (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)
                             (funcall (find-symbol "RUNTIME-INTERN" :habu-runtime) "*COUNTER*"))
                   -4)))
  (format t "  After (set '*counter* 42): ~A~%~%" new-val))

;; Test 4: Read modified value with symbol-value
(format t "4. Read modified value with symbol-value~%")
(let ((code (compile-expression '(symbol-value '*counter*) :arch :x86_64)))
  (format t "  (symbol-value '*counter*) compiles to ~D bytes~%~%" (length code)))

;; Test 5: Use modified value in expression
(format t "5. Use modified value in expression~%")
(let ((code (compile-expression '(+ (symbol-value '*counter*) 10) :arch :x86_64)))
  (format t "  (+ (symbol-value '*counter*) 10) compiles to ~D bytes~%~%" (length code)))

;; Test 6: Multiple variables
(format t "6. Multiple variables~%")
(compile-expression '(defvar *x* 1) :arch :x86_64)
(compile-expression '(defvar *y* 2) :arch :x86_64)
(compile-expression '(set '*x* 100) :arch :x86_64)
(compile-expression '(set '*y* 200) :arch :x86_64)

(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
      (get-val-fn (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)))
  (dolist (name '("*X*" "*Y*" "*COUNTER*"))
    (let* ((sym (gethash name sym-table))
           (val (funcall get-val-fn sym)))
      (format t "  ~A = ~D~%" name (ash val -4)))))

(format t "~%✓ All tests complete!~%")
(sb-ext:quit)
