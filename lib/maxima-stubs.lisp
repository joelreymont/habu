;; Stubs for missing packages
(defpackage :intl (:use :common-lisp)
  (:export #:gettext #:ngettext #:dgettext #:dngettext
           #:setlocale #:textdomain #:*locale* #:*locale-directories*
           #:*default-domain* #:read-translatable-string))
(in-package :intl)
(defun gettext (s) s)
(defun ngettext (s p n) (if (= n 1) s p))
(defun dgettext (domain s) (declare (ignore domain)) s)
(defvar *default-domain* "maxima")
(defvar *locale* "C")
(defvar *locale-directories* nil)
(defun read-translatable-string (stream char)
  (declare (ignore char))
  (let ((c (peek-char nil stream nil nil t)))
    (cond ((eql c #\") (read stream t nil t))
          ((eql c #\N) (read-char stream t nil t) nil)
          (t '_))))

(defpackage :pregexp (:use :common-lisp)
  (:export #:pregexp #:pregexp-match-positions #:pregexp-match
           #:pregexp-replace #:pregexp-quote))
(in-package :pregexp)
(defun pregexp (pat) pat)
(defun pregexp-match-positions (pat str &optional start end)
  (declare (ignore pat str start end)) nil)
(defun pregexp-match (pat str &optional start end)
  (declare (ignore pat str start end)) nil)
(defun pregexp-replace (pat str repl) (declare (ignore pat str repl)) "")
(defun pregexp-quote (str) str)

;; Bigfloat package (referenced 215 times across Maxima)
(defpackage :bigfloat (:use :common-lisp)
  (:export #:to #:signum #:+ #:- #:* #:/ #:= #:< #:> #:<= #:>= #:/=
           #:1+ #:1- #:zerop #:plusp #:minusp #:abs #:expt #:sqrt
           #:log #:exp #:sin #:cos #:tan #:asin #:acos #:atan
           #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh
           #:floor #:ceiling #:truncate #:round #:realpart #:imagpart
           #:complex #:conjugate #:random #:max #:min #:scale-float
           #:integer-decode-float #:float #:rationalize))
(in-package :bigfloat)
;; Stub: bigfloat:to just returns its argument (identity)
(defun to (x) x)
;; Stub: bigfloat:signum delegates to cl:signum
(defun signum (x) (cl:signum x))

;; Other packages referenced in Maxima
(defpackage :cl-info (:use :common-lisp)
  (:export #:get-cl-info-hashtable))

(defpackage :slatec (:use :common-lisp))

(defpackage :mt19937 (:use :common-lisp)
  (:export #:make-random-state #:random-state-p))

(in-package :maxima)

;; Note: shadowed CL symbols (float, exp, signum, etc.) are redefined by
;; Maxima's own code (clmacs.lisp defines macros for float, etc.).
;; We do NOT define function stubs here because at this point the shadow
;; isn't in place yet, so defun would overwrite the CL originals.

;; Functions from defmfun-check.lisp (can't load — complex macros)
(defun arg-count-check (required-arg-count expr &optional pretty-name)
  (declare (ignore pretty-name))
  (unless (= required-arg-count (length (rest expr)))
    (error "Wrong number of arguments: expected ~A, got ~A" required-arg-count (length (rest expr)))))
(defun oneargcheck (expr) (arg-count-check 1 expr))
(defun twoargcheck (expr) (arg-count-check 2 expr))

;; check-integer-facts: compar.lisp version uses complex do/setq patterns
(defun check-integer-facts (x &optional mode)
  (declare (ignore x mode))
  nil)

;; maybe-invert-string-case: commac.lisp uses LOOP features we don't support yet
(defun maybe-invert-string-case (str) str)

;; maknam: commac.lisp version uses LOOP 'collecting into' which we don't support yet
(defun maknam (lis)
  (let ((tem nil))
    (dolist (v lis)
      (cond ((symbolp v) (push (char (symbol-name v) 0) tem))
            ((characterp v) (push v tem))))
    (make-symbol (coerce (nreverse tem) 'string))))

(defmacro defmfun (name &rest body)
  ;; Handle (name :option val ...) pattern from real defmfun
  (if (consp name)
    `(defun ,(car name) ,@body)
    `(defun ,name ,@body)))

;; Simplified def-simplifier: (def-simplifier NAME (args...) body...)
;; Expands to defun for the simplifier and sets the operators property
(defmacro def-simplifier (base-name lambda-list &body body)
  (let* ((noun-name (intern (concatenate 'string "%" (string base-name))))
         (verb-name (intern (concatenate 'string "$" (string base-name))))
         (simp-name (intern (concatenate 'string "SIMP-" (string noun-name))))
         (form-arg (intern "FORM"))
         (z-arg (intern "%%SIMPFLAG"))
         (unused-arg (gensym "UNUSED-"))
         (arg-forms (loop for arg in lambda-list
                          for count from 1
                          collect (list arg `(simpcheck (nth ,count ,form-arg) ,z-arg)))))
    `(progn
       (defmfun ,verb-name (,@lambda-list)
         (ftake ',noun-name ,@lambda-list))
       (defprop ,verb-name ,noun-name alias)
       (defprop ,noun-name ,verb-name reversealias)
       (defprop ,noun-name ,simp-name operators)
       (defprop ,noun-name ,verb-name noun)
       (defprop ,verb-name ,noun-name verb)
       (defun ,simp-name (,form-arg ,unused-arg ,z-arg)
         (declare (ignore ,unused-arg)
                  (ignorable ,z-arg))
         (arg-count-check ,(length lambda-list) ,form-arg nil)
         (let ,arg-forms
           (flet ((give-up (&key (noun-name ',noun-name)
                                 (args (list ,@lambda-list)))
                    (eqtest (list* (list noun-name) args) ,form-arg)))
             ,@body))))))
