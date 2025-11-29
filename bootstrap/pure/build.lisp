;;; Incremental Build System for Pure Habu Compiler
;;; Only recompiles FASL if source is newer

(load "bootstrap/compiler.lisp")

(defun file-newer-p (file1 file2)
  "Check if file1 is newer than file2 (by modification time)"
  (and (probe-file file1)
       (probe-file file2)
       (> (file-write-date file1)
          (file-write-date file2))))

(defun needs-recompile-p (source-path fasl-path)
  "Check if source needs recompilation"
  (or (not (probe-file fasl-path))           ;; FASL doesn't exist
      (file-newer-p source-path fasl-path))) ;; Source is newer

(defun compile-if-needed (source-path)
  "Compile source to FASL only if needed (incremental build)"
  (let* ((fasl-path (concatenate 'string 
                                  (subseq source-path 0 
                                          (- (length source-path) 5))
                                  ".fasl")))
    (if (needs-recompile-p source-path fasl-path)
        (progn
          (format t "Compiling ~A -> ~A~%" source-path fasl-path)
          (habu:compile-file-to-fasl source-path fasl-path))
        (progn
          (format t "Up to date: ~A~%" fasl-path)
          fasl-path))))

(defun build-pure-compiler ()
  "Build pure compiler from modules (incremental)"
  (let ((modules '("bootstrap/pure/utils.lisp"
                   ;; Add more as we create them:
                   ;;"bootstrap/pure/ir.lisp"
                   ;;"bootstrap/pure/codegen.lisp"
                   ;;"bootstrap/pure/main.lisp"
                   )))
    (format t "=== Incremental Build ===~%")
    (let ((fasls (mapcar #'compile-if-needed modules)))
      (format t "~%Build complete. Generated FASLs:~%")
      (dolist (f fasls)
        (format t "  - ~A~%" f))
      fasls)))

;;; Export build functions
(export '(compile-if-needed build-pure-compiler) :habu)
