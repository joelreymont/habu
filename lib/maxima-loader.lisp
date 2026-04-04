;; Maxima source loader for Habu.

(load "lib/maxima-manifest.lisp")

(defun maxima-source-has-core-p (dir)
  (and dir
       (probe-file (concatenate 'string dir "lmdcls.lisp"))))

(defparameter *maxima-source-dir* (habu-maxima-manifest-value :srcdir))
(defparameter *maxima-package-init* (habu-maxima-manifest-value :package-init))

(let ((root (habu-maxima-manifest-value :root)))
  (when root
    (%add-trusted-load-root root)))

;; Prefer upstream package definitions when available so package semantics
;; match Maxima source without local rewrites.
(when (and *maxima-package-init*
           (probe-file *maxima-package-init*))
  (load *maxima-package-init*))

(load "lib/maxima-stubs.lisp")

(defun maxima-bootstrap-cl-user-state ()
  (let* ((pkg (find-package :cl-user))
         (sym (and pkg (intern "*MAXIMA-BUILD-TIME*" pkg))))
    (when sym
      (unless (boundp sym)
        (set sym (multiple-value-list (get-decoded-time))))
      (export sym pkg))))

(maxima-bootstrap-cl-user-state)

(defparameter *maxima-files* *habu-maxima-files*)

(defvar *maxima-ok-count* 0)
(defvar *maxima-failed* nil)
(defvar *maxima-attempted-count* 0)
(defvar *maxima-last-missing-bindings* nil)

(defun maxima-source-path (source-dir module-id)
  (concatenate 'string source-dir module-id ".lisp"))

(defun maxima-try-load (source-dir module-id &key (verbose t) (habu-trace nil))
  (let ((path (maxima-source-path source-dir module-id)))
    (setq *maxima-attempted-count* (+ *maxima-attempted-count* 1))
    (when habu-trace
      (format t "[TRACE] ~A => ~A~%" module-id path))
    (handler-case
        (progn
          (load path)
          (setq *maxima-ok-count* (+ *maxima-ok-count* 1))
          (when verbose
            (format t "[OK] ~A~%" module-id))
          t)
      (condition (e)
        (setq *maxima-failed* (cons (cons module-id e) *maxima-failed*))
        (when verbose
          (format t "[ERR] ~A :: ~A~%" module-id e))
        nil))))

(defun %maxima-binding-present-p (sym)
  (or (fboundp sym)
      (macro-function sym)))

(defun %maxima-missing-bindings (syms)
  (let ((missing nil))
    (dolist (sym syms)
      (unless (%maxima-binding-present-p sym)
        (setq missing (cons sym missing))))
    (nreverse missing)))

(defun %maxima-proper-list-p (x)
  (cond
    ((null x) t)
    ((consp x) (%maxima-proper-list-p (cdr x)))
    (t nil)))

(defun maxima-load-all (&key
                         (source-dir *maxima-source-dir*)
                         (files *maxima-files*)
                         (verbose t)
                         (habu-stop-on-error nil)
                         (habu-trace nil)
                         (habu-reset-context t)
                         (habu-required-bindings nil)
                         &allow-other-keys)
  (unless (maxima-source-has-core-p source-dir)
    (let ((total (length files)))
      (setq *maxima-last-missing-bindings*
            (if habu-required-bindings
                (%maxima-missing-bindings habu-required-bindings)
                nil))
      (when verbose
        (format t "~%=== Maxima Loader Summary ===~%")
        (format t "root: ~A~%" (habu-maxima-manifest-value :root))
        (format t "source: ~A~%" source-dir)
        (format t "loaded: 0/~D~%" total)
        (format t "failed: ~D~%" total)
        (format t "missing source root: candidates ~S~%" (habu-maxima-manifest-value :root-candidates)))
      (return-from maxima-load-all
        (values 0 total total *maxima-last-missing-bindings* 0))))
  (setq *maxima-ok-count* 0)
  (setq *maxima-failed* nil)
  (setq *maxima-attempted-count* 0)
  (dolist (module-id files)
    (unless (maxima-try-load source-dir module-id :verbose verbose :habu-trace habu-trace)
      (when habu-stop-on-error
        (return))))
  ;; DB initializes MAXIMA::CONTEXT to GLOBAL while globals/compar use
  ;; MAXIMA::$CONTEXT/$CONTEXTS with $GLOBAL. Keep them aligned for
  ;; with-new-context users (e.g. $integrate path via mfuncall/$supcontext).
  (when (and habu-reset-context
             (boundp 'maxima::context)
             (boundp 'maxima::$context)
             (boundp 'maxima::$contexts)
             (symbolp maxima::$context)
             (%maxima-proper-list-p maxima::$contexts)
             (null (member maxima::context maxima::$contexts :test #'eq))
             (member maxima::$context maxima::$contexts :test #'eq))
    (setf maxima::context maxima::$context))
  (setq *maxima-last-missing-bindings*
        (if habu-required-bindings
            (%maxima-missing-bindings habu-required-bindings)
            nil))
  (unless (%maxima-proper-list-p *maxima-failed*)
    (error "maxima-load-all invariant: *MAXIMA-FAILED* is not a proper list: ~S"
           *maxima-failed*))
  (let ((total (length files))
        (fail (length *maxima-failed*)))
    (when verbose
      (format t "~%=== Maxima Loader Summary ===~%")
      (format t "root: ~A~%" (habu-maxima-manifest-value :root))
      (format t "source: ~A~%" source-dir)
      (format t "modules: ~D~%" (habu-maxima-manifest-value :module-count))
      (format t "loaded: ~D/~D~%" *maxima-ok-count* total)
      (format t "attempted: ~D~%" *maxima-attempted-count*)
      (format t "failed: ~D~%" fail)
      (when (> fail 0)
        (format t "~%failed files:~%")
        (dolist (it (reverse *maxima-failed*))
          (format t "  ~A~%" (car it))))
      (when *maxima-last-missing-bindings*
        (format t "~%missing requested bindings:~%")
        (dolist (sym *maxima-last-missing-bindings*)
          (format t "  ~A~%" sym))))
    (values *maxima-ok-count*
            total
            fail
            *maxima-last-missing-bindings*
            *maxima-attempted-count*)))

;; Manual entrypoint:
;;   (maxima-load-all)
