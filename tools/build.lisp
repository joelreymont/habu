;;;; build.lisp - Build habu0 binary
;;;; Usage: sbcl --load tools/build.lisp [-- output-name]

(require :asdf)
(push #p"bootstrap/" asdf:*central-registry*)
(asdf:load-system :habu)
(load "shared/macros.lisp")
(load "shared/tags.lisp")
(load "shared/types.lisp")  ; ADT system - deftype macro

(defun proper-list-p (x)
  "Check if X is a proper list (not dotted)."
  (or (null x)
      (and (consp x)
           (proper-list-p (cdr x)))))

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
    ;; habu.types:deftype - expand and filter out registry updates (gethash calls)
    ((and (symbolp (car form))
          (macro-function (car form))
          (eq (car form) 'habu.types:deftype))
     (let ((expanded (macroexpand-1 form)))
       ;; Filter out SETF GETHASH calls, keep only DEFUNs and the final quote
       (cons 'progn
             (remove-if (lambda (f)
                          (and (consp f) (eq (car f) 'setf)
                               (consp (cadr f)) (eq (car (cadr f)) 'gethash)))
                        (cdr expanded)))))
    ;; habu.types:match - expand but don't recurse (produces standard CL)
    ((and (symbolp (car form))
          (macro-function (car form))
          (eq (car form) 'habu.types:match))
     (macroexpand-1 form))
    ;; Other habu macros: expand and recurse
    ((and (symbolp (car form))
          (macro-function (car form))
          (member (car form) '(habu::while habu::sym-case habu::sym-eq)))
     (expand-habu-macros (macroexpand-1 form)))
    ;; Proper list - can safely map
    ((proper-list-p form)
     (mapcar #'expand-habu-macros form))
    ;; Dotted list or improper - just return as-is
    (t form)))

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
  "Read and collect all source forms for habu0.
   Expands macros during reading to preserve correct package context."
  (let ((forms nil)
        (*features* (remove :sbcl *features*)))
    ;; Note: shared/types.lisp is loaded for macro expansion only (deftype, match)
    ;; but NOT included in habu0 since it uses gethash for type registry
    (dolist (file '("shared/ir.lisp"
                    "shared/macros.lisp"
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
                ;; Expand macros now while we have the correct *package*
                do (push (expand-habu-macros form) forms)))))
    (splice-forms (nreverse forms))))

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
