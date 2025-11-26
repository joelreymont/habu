;;; Test that the bootstrap compiler can compile a mini-compiler
;;; This validates self-hosting capability by compiling a compiler
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Bootstrap Compiler Self-Hosting Test ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-compile (name source expected)
  (handler-case
    (let* ((forms (nc-read-all source))
           (bytes (nc-compile-program forms nil))
           (output-path (format nil "/tmp/bootstrap_~A" name)))
      (habu-macho:deliver-native-with-heap output-path bytes)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; Test 1: Mini expression compiler
;;; Compiles arithmetic expressions to a simple representation
(test-compile "mini-expr-compiler"
  "(defun compile-expr (e)
     (if (numberp e)
         e
         (if (consp e)
             (let ((op (car e)))
               (if (eq op (quote add))
                   (+ (compile-expr (cadr e)) (compile-expr (caddr e)))
                   (if (eq op (quote mul))
                       (* (compile-expr (cadr e)) (compile-expr (caddr e)))
                       0)))
             0)))
   (compile-expr (cons (quote add) (cons 10 (cons (cons (quote mul) (cons 4 (cons 8 nil))) nil))))"
  42)

;;; Test 2: IR node builder
;;; Builds IR-like structures
(test-compile "ir-builder"
  "(defun make-lit (n) (cons (quote lit) (cons n nil)))
   (defun make-add (l r) (cons (quote add) (cons l (cons r nil))))
   (defun ir-value (ir)
     (let ((tag (car ir)))
       (if (eq tag (quote lit))
           (cadr ir)
           (if (eq tag (quote add))
               (+ (ir-value (cadr ir)) (ir-value (caddr ir)))
               0))))
   (ir-value (make-add (make-lit 10) (make-add (make-lit 20) (make-lit 12))))"
  42)

;;; Test 3: Symbol table operations
;;; Tests env-like lookup used in compilers
(test-compile "symbol-table"
  "(defun env-add (name val env) (cons (cons name val) env))
   (defun env-get (name env)
     (if (null env) 0
         (if (eq name (car (car env)))
             (cdr (car env))
             (env-get name (cdr env)))))
   (let* ((e1 (env-add (quote x) 10 nil))
          (e2 (env-add (quote y) 20 e1))
          (e3 (env-add (quote z) 12 e2))
          (vx (env-get (quote x) e3))
          (vy (env-get (quote y) e3))
          (vz (env-get (quote z) e3)))
     (+ vx (+ vy vz)))"
  42)

;;; Test 4: Code generation helper
;;; Simulates bytecode emission
(test-compile "codegen-helper"
  "(defun emit (bytes code) (cons code bytes))
   (defun emit-all (bytes codes)
     (if (null codes)
         bytes
         (emit-all (emit bytes (car codes)) (cdr codes))))
   (defun count-bytes (bytes) (length bytes))
   (let* ((code nil)
          (c1 (emit-all code (cons 1 (cons 2 (cons 3 nil)))))
          (c2 (emit-all c1 (cons 4 (cons 5 nil))))
          (c3 (emit c2 6)))
     (* (count-bytes c3) 7))"
  42)

;;; Test 5: Recursive descent compiler pattern
;;; Pattern matching on AST nodes
(test-compile "recursive-descent"
  "(defun compile-ast (ast env)
     (if (numberp ast)
         ast
         (if (eq (car ast) (quote var))
             (let ((name (cadr ast)))
               (if (null env) 0
                   (if (eq name (car (car env)))
                       (cdr (car env))
                       (compile-ast ast (cdr env)))))
             (if (eq (car ast) (quote add))
                 (+ (compile-ast (cadr ast) env)
                    (compile-ast (caddr ast) env))
                 0))))
   (let ((env (cons (cons (quote x) 10) (cons (cons (quote y) 32) nil))))
     (compile-ast (cons (quote add)
                        (cons (cons (quote var) (cons (quote x) nil))
                              (cons (cons (quote var) (cons (quote y) nil)) nil)))
                  env))"
  42)

;;; Test 6: Lambda lifting pattern
;;; Count nested lambdas in code structure
(test-compile "lambda-lifter"
  "(defun count-lambdas (expr)
     (if (consp expr)
         (let* ((is-lam (if (eq (car expr) (quote lambda)) 1 0))
                (car-cnt (count-lambdas (car expr)))
                (cdr-cnt (count-lambdas (cdr expr))))
           (+ is-lam (+ car-cnt cdr-cnt)))
         0))
   (let* ((inner-lambda (cons (quote lambda) (cons (cons (quote y) nil) (cons (quote body) nil))))
          (code (cons (quote defun) (cons (quote f)
                 (cons (cons (quote x) nil) (cons inner-lambda nil)))))
          (cnt (count-lambdas code)))
     (+ 41 cnt))"
  42)

;;; Test 7: Instruction encoder
;;; Encode instructions like ARM64 codegen (simplified)
(test-compile "instr-encoder"
  "(defun encode (rd rn)
     (let* ((rd-part rd)
            (rn-part (ash rn 3)))
       (+ rd-part rn-part)))
   (let* ((i1 (encode 2 5))
          (i2 (encode 0 0)))
     (+ i1 i2))"
  42)

;;; Test 8: Fixup pass
;;; Resolve forward references like a linker
(test-compile "fixup-pass"
  "(defun find-label (name labels)
     (if (null labels) 0
         (if (eq name (car (car labels)))
             (cdr (car labels))
             (find-label name (cdr labels)))))
   (defun apply-fixup (code labels)
     (if (consp code)
         (if (eq (car code) (quote ref))
             (find-label (cadr code) labels)
             (let* ((car-fixed (apply-fixup (car code) labels))
                    (cdr-fixed (apply-fixup (cdr code) labels)))
               (cons car-fixed cdr-fixed)))
         code))
   (let* ((labels (cons (cons (quote fn1) 100)
                        (cons (cons (quote fn2) 200) nil)))
          (code (cons (cons (quote ref) (cons (quote fn1) nil))
                      (cons (cons (quote ref) (cons (quote fn2) nil)) nil)))
          (fixed (apply-fixup code labels))
          (first-val (car fixed))
          (second-val (cadr fixed)))
     (mod (+ first-val second-val) 256))"
  44)

;;; Test 9: Multiple function compilation
;;; Like compiling multiple defuns
(test-compile "multi-fn-compile"
  "(defun fn1 (x) (+ x 10))
   (defun fn2 (x) (* x 2))
   (defun fn3 (x) (fn1 (fn2 x)))
   (fn3 8)"
  26)

;;; Test 10: Higher-order function in compiler
;;; mapcar-like transform over code
(test-compile "ho-transform"
  "(defun transform (fn lst)
     (if (null lst) nil
         (let* ((head (funcall fn (car lst)))
                (tail (transform fn (cdr lst))))
           (cons head tail))))
   (defun sum-list (lst)
     (if (null lst) 0
         (let* ((hd (car lst))
                (tl-sum (sum-list (cdr lst))))
           (+ hd tl-sum))))
   (let* ((code (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))))
          (doubled (transform (lambda (x) (* x 2)) code))
          (result (sum-list doubled)))
     (mod result 256))"
  30)

(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *pass-count* *fail-count*)
(sb-ext:exit :code *fail-count*)
