;;;; Test Minimal Evaluator
;;;; Simple arithmetic evaluator using only supported compiler operations

;;; Simple evaluator for arithmetic
(defun eval-add (args)
  "Evaluate addition"
  (+ (car args) (car (cdr args))))

(defun eval-sub (args)
  "Evaluate subtraction"
  (- (car args) (car (cdr args))))

(defun eval-mul (args)
  "Evaluate multiplication"
  (* (car args) (car (cdr args))))

(defun eval-div (args)
  "Evaluate division"
  (/ (car args) (car (cdr args))))

;;; Symbolic operator matching
(defun op-is-add? (op)
  "Test if operator is +"
  ;; In full system would use symbol=?, for now use numeric codes
  (= op 1))

(defun op-is-sub? (op)
  "Test if operator is -"
  (= op 2))

(defun op-is-mul? (op)
  "Test if operator is *"
  (= op 3))

(defun op-is-div? (op)
  "Test if operator is /"
  (= op 4))

;;; Evaluate expression
(defun eval-expr (expr)
  "Evaluate simple expression"
  (if (cons? expr)
      (let ((op (car expr)))
        (let ((args (cdr expr)))
          (if (op-is-add? op)
              (eval-add args)
              (if (op-is-sub? op)
                  (eval-sub args)
                  (if (op-is-mul? op)
                      (eval-mul args)
                      (if (op-is-div? op)
                          (eval-div args)
                          0))))))
      expr))

;;; Test cases
(defun test-eval ()
  "Test evaluator with simple expressions"
  (let ((expr1 (cons 1 (cons 10 (cons 5 nil)))))  ; (+ 10 5)
    (let ((result1 (eval-expr expr1)))
      (if (= result1 15)
          1  ; Pass
          0))))  ; Fail

;;; Entry point
(test-eval)
