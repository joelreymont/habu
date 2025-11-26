;;; Bootstrap Compiler - Self-hosting entry point
;;;
;;; This file is compiled to a standalone executable that can compile
;;; Lisp programs without requiring SBCL.
;;;
;;; Usage: ./habu-compiler input.lisp output.bin
;;;
;;; The output.bin can then be executed via run-bytecode or wrapped
;;; with the C runtime to create a standalone executable.

;;; For now, we use a simplified approach:
;;; 1. Compile a test program inline
;;; 2. Verify the compiler can compile itself

;;; Test: compile factorial and run it
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(defun main ()
  (println (factorial 10)))

(main)
