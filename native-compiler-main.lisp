;;; Native Compiler Main - Pure Habu Entry Point
;;; This program can be compiled to native ARM64 and run standalone
;;; Usage after compilation: ./habu-compiler <input.lisp> <output>

;; Read command line arguments
;; For now, hardcode the input/output paths since we don't have command-line arg parsing yet
;; This is just a proof-of-concept to demonstrate the native compiler can compile files

(defun compile-and-link (source-path output-path)
  "Compile Lisp source file to native executable using pure-Habu file I/O"
  ;; Read source file using native file I/O
  (let ((source (native-read-file-large source-path)))
    ;; Compile to bytecode
    ;; This would call the compiler, but we can't actually do this yet
    ;; because the compiler itself uses SBCL features
    ;; This is a placeholder for now
    (sys-write 1 "Compilation not yet implemented in native code\n" 47)
    (sys-exit 1)))

;; Main entry point
;; For proof of concept, try to read a file and print its length
(defun main ()
  "Native compiler entry point"
  ;; Test: read this source file and print its size
  (let* ((source (native-read-file-large "native-compiler-main.lisp"))
         (len (string-length source)))
    ;; Print the length
    (sys-write 1 "Read file, length: " 19)
    ;; TODO: convert number to string and print it
    ;; For now just exit with the length as exit code
    (sys-exit len)))

;; Call main
(main)
