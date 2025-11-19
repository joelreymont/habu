;;;; Test runtime closure support

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Runtime Closure Support~%")
(format t "================================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%" name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Create a simple closure with no captured variables
(format t "~%[34m1. Simple Closure Creation[0m~%")
(format t "==========================~%")

(let* ((make-closure-fn (find-symbol "MAKE-CLOSURE" :habu-runtime))
       (closure?-fn (find-symbol "CLOSURE?" :habu-runtime))
       (code-ptr #x123456)  ; Dummy function pointer
       (arity 1)
       (captured-vars '())
       (closure (funcall make-closure-fn code-ptr arity captured-vars)))
  (test "CLOSURE-CREATED" (not (null closure)))
  (test "CLOSURE-IS-TAGGED" (funcall closure?-fn closure))
  (format t "   Closure pointer: ~X~%" closure))

;; Test 2: Create closure with one captured variable
(format t "~%[34m2. Closure with One Captured Variable[0m~%")
(format t "=======================================~%")

(let* ((make-closure-fn (find-symbol "MAKE-CLOSURE" :habu-runtime))
       (closure-env-ref-fn (find-symbol "CLOSURE-ENV-REF" :habu-runtime))
       (closure-env-size-fn (find-symbol "CLOSURE-ENV-SIZE" :habu-runtime))
       (code-ptr #xABCDEF)
       (arity 1)
       (x-value (ash 42 4))  ; Tagged fixnum 42
       (captured-vars (list x-value))
       (closure (funcall make-closure-fn code-ptr arity captured-vars)))
  (test "CLOSURE-ENV-SIZE-1" (= (funcall closure-env-size-fn closure) 1))
  (test "CAPTURED-VAR-VALUE" (= (funcall closure-env-ref-fn closure 0) x-value))
  (format t "   Captured variable 0: ~D (untagged: ~D)~%"
          (funcall closure-env-ref-fn closure 0)
          (ash (funcall closure-env-ref-fn closure 0) -4)))

;; Test 3: Closure with multiple captured variables
(format t "~%[34m3. Closure with Multiple Captured Variables[0m~%")
(format t "============================================~%")

(let* ((make-closure-fn (find-symbol "MAKE-CLOSURE" :habu-runtime))
       (closure-env-ref-fn (find-symbol "CLOSURE-ENV-REF" :habu-runtime))
       (closure-env-size-fn (find-symbol "CLOSURE-ENV-SIZE" :habu-runtime))
       (code-ptr #x111222)
       (arity 2)
       (x-value (ash 10 4))
       (y-value (ash 20 4))
       (z-value (ash 30 4))
       (captured-vars (list x-value y-value z-value))
       (closure (funcall make-closure-fn code-ptr arity captured-vars)))
  (test "CLOSURE-ENV-SIZE-3" (= (funcall closure-env-size-fn closure) 3))
  (test "CAPTURED-VAR-0" (= (funcall closure-env-ref-fn closure 0) x-value))
  (test "CAPTURED-VAR-1" (= (funcall closure-env-ref-fn closure 1) y-value))
  (test "CAPTURED-VAR-2" (= (funcall closure-env-ref-fn closure 2) z-value))
  (format t "   Captured variables: ~D, ~D, ~D~%"
          (ash (funcall closure-env-ref-fn closure 0) -4)
          (ash (funcall closure-env-ref-fn closure 1) -4)
          (ash (funcall closure-env-ref-fn closure 2) -4)))

;; Test 4: Closure accessors
(format t "~%[34m4. Closure Accessors[0m~%")
(format t "====================~%")

(let* ((make-closure-fn (find-symbol "MAKE-CLOSURE" :habu-runtime))
       (closure-code-pointer-fn (find-symbol "CLOSURE-CODE-POINTER" :habu-runtime))
       (closure-arity-fn (find-symbol "CLOSURE-ARITY" :habu-runtime))
       (closure-env-size-fn (find-symbol "CLOSURE-ENV-SIZE" :habu-runtime))
       (code-ptr #xDEADBEEF)
       (arity 3)
       (captured-vars (list (ash 1 4) (ash 2 4)))
       (closure (funcall make-closure-fn code-ptr arity captured-vars)))
  (test "CODE-POINTER-CORRECT"
        (= (funcall closure-code-pointer-fn closure) code-ptr))
  (test "ARITY-CORRECT"
        (= (funcall closure-arity-fn closure) arity))
  (test "ENV-SIZE-CORRECT"
        (= (funcall closure-env-size-fn closure) 2))
  (format t "   Code pointer: ~X~%" (funcall closure-code-pointer-fn closure))
  (format t "   Arity: ~D~%" (funcall closure-arity-fn closure))
  (format t "   Env size: ~D~%" (funcall closure-env-size-fn closure)))

;; Test 5: Closure info for debugging
(format t "~%[34m5. Closure Info[0m~%")
(format t "===============~%")

(let* ((make-closure-fn (find-symbol "MAKE-CLOSURE" :habu-runtime))
       (closure-info-fn (find-symbol "CLOSURE-INFO" :habu-runtime))
       (code-ptr #xCAFEBABE)
       (arity 2)
       (captured-vars (list (ash 100 4)))
       (closure (funcall make-closure-fn code-ptr arity captured-vars)))
  (test "CLOSURE-INFO-AVAILABLE" (not (null (funcall closure-info-fn closure))))
  (format t "   ~A~%" (funcall closure-info-fn closure)))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%" *pass-count* *test-count*)
(format t "~%")

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll runtime closure tests passed![0m~%")

(sb-ext:quit)
