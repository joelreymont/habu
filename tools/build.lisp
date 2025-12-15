;;;; build.lisp - Build habu0 binary
;;;; Usage: sbcl --load tools/build.lisp [-- output-name]

(require :asdf)
(push #p"bootstrap/" asdf:*central-registry*)
(asdf:load-system :habu)
(load "shared/macros.lisp")
(load "shared/tags.lisp")

(defun expand-habu-macros (form)
  "Recursively expand habu macros before passing to habu compiler.
   Special handling: defvar macros get spliced at top level only."
  (cond
    ((atom form) form)
    ((eq (car form) 'quote) form)
    ;; eval-when: SBCL-only, skip entirely (contains +op-specs+ etc for macro expansion)
    ((eq (car form) 'eval-when) nil)
    ;; Defvar macros: splice at top level
    ((member (car form) '(habu::define-op-defvars habu::define-kw-defvars))
     (let ((expanded (macroexpand-1 form)))
       (cons :splice (mapcar #'expand-habu-macros (cdr expanded)))))
    ;; Setq macros: expand but keep as progn (used inside function bodies)
    ((member (car form) '(habu::init-all-op-symbols habu::init-all-kw-symbols))
     (expand-habu-macros (macroexpand-1 form)))
    ;; Other habu macros
    ((and (symbolp (car form))
          (macro-function (car form))
          (member (car form) '(habu::while habu::sym-case habu::sym-eq)))
     (expand-habu-macros (macroexpand-1 form)))
    (t (mapcar #'expand-habu-macros form))))

(defun splice-forms (forms)
  "Flatten forms, splicing any (:splice ...) markers. Skip nil entries."
  (let ((result nil))
    (dolist (form forms (nreverse result))
      (cond
        ((null form) nil)  ; skip nil (from eval-when filtering)
        ((and (consp form) (eq (car form) :splice))
         (dolist (f (cdr form))
           (when f (push f result))))
        (t (push form result))))))

(defun collect-forms ()
  "Read and collect all source forms for habu0."
  (let ((forms nil)
        (*features* (remove :sbcl *features*)))
    (dolist (file '("shared/macros.lisp"
                    "arm64/asm.lisp"
                    "bootstrap/reader.lisp"
                    "habu0.lisp"
                    "bootstrap/reg-alloc.lisp"
                    "bootstrap/codegen.lisp"))
      (let ((*package* (find-package :habu)))
        (with-open-file (s file)
          (loop for form = (read s nil :eof)
                until (eq form :eof)
                do (cond
                     ((and (consp form) (eq (car form) 'defpackage))
                      (eval form))
                     ((and (consp form) (eq (car form) 'in-package))
                      (setq *package* (find-package (cadr form)))))
                do (push form forms)))))
    (splice-forms (mapcar #'expand-habu-macros (nreverse forms)))))

(defun main ()
  (let* ((args (remove "--" (uiop:command-line-arguments) :test #'string=))
         (output (or (first args) "habu0"))
         (heap-size (or (parse-integer (or (second args) "") :junk-allowed t)
                        67108864)))
    (format t "Building ~A (heap ~A bytes)...~%" output heap-size)
    (habu:deliver-forms (collect-forms) output heap-size)
    (format t "Done: ~A~%" output)))

(main)
(sb-ext:exit :code 0)
