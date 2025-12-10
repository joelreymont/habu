;;;; compile-file.lisp - Compile a Habu source file

(load "compiler.lisp")
(in-package :habu-compiler)

(defun compile-habu-file (input-file &key (arch :x86_64) (output-file nil) (verbose t))
  "Compile a Habu source file to machine code.
   Returns list of (form . bytecode) pairs."
  (when verbose
    (format t "~%Compiling ~A for ~A...~%" input-file arch))

  (initialize-runtime-integration)

  ;; Read entire file as string
  (with-open-file (in input-file :direction :input)
    (let ((file-contents (make-string (file-length in))))
      (read-sequence file-contents in)

      ;; Parse all top-level forms
      (let ((forms nil)
            (pos 0)
            (compiled-forms nil))

        ;; Read all forms from the file - NO SILENT FALLBACKS
        (loop
          (multiple-value-bind (form new-pos)
              (read-from-string file-contents nil :eof :start pos)
            (when (eq form :eof)
              (return))
            (push form forms)
            (setf pos new-pos)))

        (setf forms (reverse forms))

        (when verbose
          (format t "Found ~D top-level forms~%~%" (length forms)))

        ;; Compile each form
        (loop for form in forms
              for i from 1
              do
              (when verbose
                (format t "~D. ~A~%" i (if (consp form) (first form) form)))

              ;; Let errors propagate - NO SILENT FALLBACKS
              (let ((code (compile-expression form :arch arch)))
                (when verbose
                  (format t "   => ~D bytes~%" (length code)))
                (push (cons form code) compiled-forms)))

        (setf compiled-forms (reverse compiled-forms))

        ;; Optionally write output
        (when output-file
          (with-open-file (out output-file
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
            (loop for (form . code) in compiled-forms
                  when code
                  do (write-sequence code out)))
          (when verbose
            (format t "~%Wrote compiled code to ~A~%" output-file)))

        ;; Summary
        (let ((success-count (count-if #'cdr compiled-forms))
              (total-bytes (reduce #'+ compiled-forms :key (lambda (x) (length (cdr x))))))
          (when verbose
            (format t "~%Compilation complete:~%")
            (format t "  Forms: ~D/~D compiled successfully~%"
                    success-count (length compiled-forms))
            (format t "  Total: ~D bytes~%~%" total-bytes)))

        compiled-forms))))

;; Test with example.habu
(when (probe-file "example.habu")
  (compile-habu-file "example.habu" :output-file "example.o"))

(sb-ext:quit)
