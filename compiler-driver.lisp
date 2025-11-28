;;; Minimal Habu Compiler Driver for Bootstrap Self-Hosting
;;; This wraps the essential compiler functions from bootstrap/compiler.lisp
;;; Usage: sbcl --script compiler-driver.lisp <input.lisp> <output>

(load "bootstrap/compiler.lisp")
(load "macho-linker.lisp")

(defun read-source-file (path)
  "Read entire file contents as a string"
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (if stream
        (let* ((length (file-length stream))
               (buffer (make-string length)))
          (read-sequence buffer stream)
          buffer)
        nil)))

(defun main ()
  "Compiler driver entry point - reads source, compiles to native executable"
  (let* ((args sb-ext:*posix-argv*)
         (input-file (if (>= (length args) 2) (nth 1 args) nil))
         (output-file (if (>= (length args) 3) (nth 2 args) nil)))
    (cond
      ((or (null input-file) (null output-file))
       (format *error-output* "Usage: ~A <input.lisp> <output>~%" (nth 0 args))
       (sb-ext:exit :code 1))
      (t
       (let ((source (read-source-file input-file)))
         (if source
             (progn
               (format t "Compiling ~A -> ~A~%" input-file output-file)
               (habu:deliver-with-libsystem source output-file :verbose t)
               (format t "Success!~%")
               (sb-ext:exit :code 0))
             (progn
               (format *error-output* "Error: Cannot read input file ~A~%" input-file)
               (sb-ext:exit :code 1))))))))

;; Run main
(main)
