;;;; IR to C Code Generator
;;;; Converts S-expression IR to C source code
;;;; This completes the pipeline: Habu → IR → C

;;; Check if a cons has a specific tag
(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

;;; Generate C code from IR expression
(defun ir-to-c (ir)
  (if (fixnum? ir)
    ;;; Literal number → fixnum_to_value(N)
    ir
    (if (has-tag? ir (quote lit))
      ;;; (lit N) → fixnum_to_value(N)
      (car (cdr ir))
      (if (has-tag? ir (quote var))
        ;;; (var SYM) → lookup variable (TODO: needs env)
        (quote var-ref)
        (if (has-tag? ir (quote call))
          ;;; (call OP ARG1 ARG2) → habu_add/sub/mul/etc
          (let ((op (car (cdr ir))))
            (if (symbol=? op (quote +))
              (list (quote c-add)
                    (ir-to-c (car (cdr (cdr ir))))
                    (ir-to-c (car (cdr (cdr (cdr ir))))))
              (if (symbol=? op (quote *))
                (list (quote c-mul)
                      (ir-to-c (car (cdr (cdr ir))))
                      (ir-to-c (car (cdr (cdr (cdr ir))))))
                (if (symbol=? op (quote -))
                  (list (quote c-sub)
                        (ir-to-c (car (cdr (cdr ir))))
                        (ir-to-c (car (cdr (cdr (cdr ir))))))
                  (quote unknown-op)))))
          ir)))))

;;; Test: Compile Habu → IR, then IR → C
(defun test-pipeline (expr)
  (ir-to-c (compile-expr expr)))

;;; First we need compile-expr
(defun compile-expr (expr)
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      (list (quote var) expr)
      (if (cons? expr)
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (cons? args)
              (let ((arg1 (car args)))
                (let ((rest (cdr args)))
                  (if (cons? rest)
                    (list (quote call) op
                          (compile-expr arg1)
                          (compile-expr (car rest)))
                    (list (quote call) op (compile-expr arg1)))))
              (list (quote call) op))))
        expr))))

;;; Test the full pipeline
(test-pipeline (quote (+ 1 2)))
(test-pipeline (quote (* 3 (+ 4 5))))
