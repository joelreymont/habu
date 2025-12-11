;;; Build habu0 native executable with all modules for mode 1024
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)

;; Collect forms from all needed files (read with #-sbcl to get native versions)
(let ((forms nil)
      (*features* (remove :sbcl *features*))
      (files '("shared/macros.lisp"
               "arm64/asm.lisp"
               "bootstrap/reader.lisp"
               "habu0.lisp"
               "bootstrap/reg-alloc.lisp"
               "bootstrap/codegen.lisp")))
  (dolist (file files)
    (let ((*package* (find-package :habu)))
      (with-open-file (s file)
        (loop for form = (cl:read s nil :eof)
              until (eq form :eof)
              do (cond
                   ;; Package forms: use CL:EVAL (not HABU:EVAL) for side effects but don't compile
                   ((and (consp form) (eq (car form) 'defpackage))
                    (cl:eval form))
                   ((and (consp form) (eq (car form) 'in-package))
                    (setq *package* (find-package (cadr form))))
                   ;; All other forms: collect for compilation
                   (t (push form forms)))))))
  (format t "Read ~A forms from ~A files~%" (length forms) (length files))
  (deliver-forms (nreverse forms) "habu0"))

(format t "~%Built habu0 native executable~%")
(sb-ext:quit :unix-status 0)
