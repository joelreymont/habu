;;; Native Compiler Main Entry Point
;;; This is designed to be compiled to a native executable
;;; Uses ONLY Habu primitives - no SBCL dependencies

(in-package :habu)

;;; Main entry point for native compiler executable
;;; Expects command-line args passed as a list of strings
(defun native-compiler-main (args)
  "Main entry point for native compiler.
   Args: (program-name input-file output-file)
   Returns exit code as integer"
  (let* ((input-file (if (>= (length args) 2) (nth 1 args) nil))
         (output-file (if (>= (length args) 3) (nth 2 args) nil)))
    (if (or (null input-file) (null output-file))
        ;; Error: Missing arguments
        (progn
          (sys-write 2 "Usage: compiler <input.lisp> <output>\n" 39)
          1)  ;; Exit code 1
        ;; Read source, compile, write output
        (let ((source (native-read-file input-file)))
          (if source
              (progn
                ;; Compile source to native executable
                (deliver source output-file :verbose nil)
                0)  ;; Exit code 0 (success)
              ;; Error: Cannot read input file
              (progn
                (sys-write 2 "Error: Cannot read " 19)
                (sys-write 2 input-file (string-length input-file))
                (sys-write 2 "\n" 1)
                1))))))  ;; Exit code 1

;;; Test with sys-exit wrapper
(sys-exit (native-compiler-main (list "compiler" "/tmp/test_factorial.lisp" "/tmp/test_out")))
