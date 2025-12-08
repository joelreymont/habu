;;; Trace the compilation of string=-loop

(load "bootstrap/habu.asd")
(asdf:load-system :habu)
(in-package :habu)

;; Define the function to compile
(defparameter *test-code*
  '(defun string=-loop (s1 s2 len i)
     (if (>= i len)
         t
         (if (= (string-ref s1 i) (string-ref s2 i))
             (string=-loop s1 s2 len (+ i 1))
             nil))))

;; Compile it and show IR
(format t "~%=== Compiling string=-loop ===~%")
(let* ((name (cadr *test-code*))
       (params (caddr *test-code*))
       (body (cdddr *test-code*))
       ;; Build fenv with the function itself for recursion
       ;; Use a large dummy body so it won't inline (inlinable? checks size < 20)
       (fenv (list (list name params (make-list 30))))  ; name, params, large dummy body
       (body-expr (if (null (cdr body)) (car body) (cons 'progn body)))
       (mir (habu::compile-expr-full body-expr params fenv)))
  (format t "~%MIR (Middle IR):~%")
  (pprint mir)

  ;; Convert to TAC
  (format t "~%~%=== TAC (Three-Address Code) ===~%")
  (let* ((counter (list 0))
         (tac-result (habu::ir-to-tac mir counter))
         (tac-instrs (car tac-result)))
    (habu::print-tac tac-instrs)

    ;; Show label positions
    (format t "~%~%=== Label Analysis ===~%")
    (let ((pos 0))
      (dolist (instr tac-instrs)
        (when (eq (car instr) 'habu::tac-label)
          (format t "Label ~A at TAC position ~D~%" (cadr instr) pos))
        (incf pos)))

    ;; Now compile to machine code and show branch resolution
    (format t "~%~%=== Branch Resolution (showing inner if only) ===~%")
    (format t "Inner if starts at TAC position 16: (TAC-IF 14 16 17)~%")
    (format t "  Label 16 is at TAC position 17 (then - recursive call)~%")
    (format t "  Label 17 is at TAC position 27 (else - return nil)~%")
    (format t "~%When chars ARE equal (condition != nil):~%")
    (format t "  Should branch to label 16 (position 17) -> recursive call~%")
    (format t "When chars DIFFER (condition == nil):~%")
    (format t "  Should fall through to branch to label 17 (position 27) -> return nil~%")))

(sb-ext:exit)
