;;;; Minimal REPL - Simple Read-Eval-Print Loop
;;;; Designed to be compilable to ARM64 machine code

;;; Simple eval for arithmetic expressions
(defun eval-expr (expr env)
  "Evaluate expression in environment"
  (if (fixnum? expr)
      expr
      (if (cons? expr)
          (let ((op (car expr)))
            (if (symbol=? op (quote +))
                (+ (eval-expr (car (cdr expr)) env)
                   (eval-expr (car (cdr (cdr expr))) env))
                (if (symbol=? op (quote -))
                    (- (eval-expr (car (cdr expr)) env)
                       (eval-expr (car (cdr (cdr expr))) env))
                    (if (symbol=? op (quote *))
                        (* (eval-expr (car (cdr expr)) env)
                           (eval-expr (car (cdr (cdr expr))) env))
                        (if (symbol=? op (quote /))
                            (/ (eval-expr (car (cdr expr)) env)
                               (eval-expr (car (cdr (cdr expr))) env))
                            (quote 0))))))
          (quote 0))))

;;; Print a value
(defun print-value (val)
  "Print value to output"
  (if (fixnum? val)
      (print-fixnum val)
      (if (nil? val)
          (print-string "nil")
          (print-string "<value>"))))

(defun print-fixnum (n)
  "Print fixnum (simplified)"
  (if (< n 0)
      (progn
        (print-string "-")
        (print-fixnum (- 0 n)))
      (if (< n 10)
          (print-digit n)
          (progn
            (print-fixnum (/ n 10))
            (print-digit (mod n 10))))))

(defun print-digit (d)
  "Print single digit"
  (write-byte (+ 48 d)))

(defun print-string (str)
  "Print string character by character"
  (print-string-loop str 0))

(defun print-string-loop (str i)
  "Helper for printing string"
  (if (< i (string-length-raw str))
      (progn
        (write-byte (string-ref str i))
        (print-string-loop str (+ i 1)))
      (quote 0)))

(defun print-newline ()
  "Print newline character"
  (write-byte 10))

;;; Simple read (placeholder - needs full reader)
(defun read-expr ()
  "Read expression from input (simplified)"
  ;; For now, return a hardcoded expression to test
  ;; In full version, this would parse input
  (quote (+ 2 3)))

;;; REPL loop
(defun repl-loop (env)
  "Main REPL loop"
  (progn
    (print-string "habu> ")
    (let ((expr (read-expr)))
      (if (nil? expr)
          (quote 0)  ; Exit on nil
          (progn
            (let ((result (eval-expr expr env)))
              (progn
                (print-value result)
                (print-newline)
                (repl-loop env))))))))

;;; Entry point
(defun main ()
  "REPL entry point"
  (progn
    (print-string "Habu Minimal REPL")
    (print-newline)
    (print-string "Type expressions to evaluate")
    (print-newline)
    (repl-loop (quote nil))))

;;; Start REPL
(main)
