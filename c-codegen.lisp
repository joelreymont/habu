;;;; Habu C Code Generator
;;;; Converts S-expression IR to C code
;;;; This is a key step toward self-hosting!

;;; Helper: Check if symbol matches a name
(defun is-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

;;; Generate C code from IR
(defun codegen-expr (ir)
  (if (fixnum? ir)
    ir
    (if (is-tag? ir (quote lit))
      (car (cdr ir))
      (if (is-tag? ir (quote var))
        (car (cdr ir))
        (if (is-tag? ir (quote call))
          (let ((op (car (cdr ir))))
            (if (symbol=? op (quote +))
              (list (quote add)
                    (codegen-expr (car (cdr (cdr ir))))
                    (codegen-expr (car (cdr (cdr (cdr ir))))))
              (if (symbol=? op (quote *))
                (list (quote mul)
                      (codegen-expr (car (cdr (cdr ir))))
                      (codegen-expr (car (cdr (cdr (cdr ir))))))
                (if (symbol=? op (quote -))
                  (list (quote sub)
                        (codegen-expr (car (cdr (cdr ir))))
                        (codegen-expr (car (cdr (cdr (cdr ir))))))
                  (list (quote call) op)))))
          ir)))))

;;; Test the code generator
(defun test-codegen ()
  (progn
    (codegen-expr (quote (lit 42)))
    (codegen-expr (quote (call + (lit 1) (lit 2))))
    (codegen-expr (quote (call * (lit 3) (call + (lit 4) (lit 5)))))))

(test-codegen)
