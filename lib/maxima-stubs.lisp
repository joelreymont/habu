;; Stubs for missing packages
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :intl)
    (defpackage :intl (:use :common-lisp)
      (:export #:gettext #:ngettext #:dgettext #:dngettext
               #:setlocale #:textdomain #:*locale* #:*locale-directories*
               #:*default-domain* #:read-translatable-string))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :pregexp)
    (defpackage :pregexp (:use :common-lisp)
      (:export #:pregexp #:pregexp-match-positions #:pregexp-match
               #:pregexp-replace #:pregexp-quote))))
(in-package :pregexp)
(defun pregexp (pat) pat)
(defun pregexp-match-positions (pat str &optional start end)
  (declare (ignore pat str start end)) nil)
(defun pregexp-match (pat str &optional start end)
  (declare (ignore pat str start end)) nil)
(defun pregexp-replace (pat str repl) (declare (ignore pat str repl)) "")
(defun pregexp-quote (str) str)

;; Bigfloat package setup.
;; Mirror Maxima's maxima-package.lisp shadowing model so BIGFLOAT's
;; arithmetic symbols are distinct from COMMON-LISP symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bigfloat)
    (defpackage :bigfloat-impl
      (:use :common-lisp)
      (:shadow #:+ #:- #:* #:/ #:1+ #:1- #:zerop #:plusp #:minusp #:abs
               #:sqrt #:log #:exp #:sin #:cos #:tan #:asin #:acos #:atan
               #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh #:expt #:=
               #:/= #:< #:> #:<= #:>= #:scale-float #:realpart #:imagpart
               #:complex #:conjugate #:max #:min #:cis #:phase #:floor
               #:ffloor #:incf #:decf #:realp #:complexp #:numberp
               #:integer-decode-float #:decode-float #:float #:ceiling
               #:fceiling #:truncate #:ftruncate #:round #:fround #:random
               #:signum #:float-sign #:float-digits #:rational
               #:rationalize #:coerce)
      (:export #:bigfloat #:complex-bigfloat #:to #:maybe-to #:epsilon #:%pi #:%e
               #:+ #:- #:* #:/ #:1+ #:1- #:zerop #:plusp #:minusp #:abs
               #:sqrt #:log #:exp #:sin #:cos #:tan #:asin #:acos #:atan
               #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh #:expt #:=
               #:/= #:< #:> #:<= #:>= #:scale-float #:realpart #:imagpart
               #:complex #:conjugate #:max #:min #:cis #:phase #:floor
               #:ffloor #:incf #:decf #:realp #:complexp #:numberp
               #:integer-decode-float #:decode-float #:float #:ceiling
               #:fceiling #:truncate #:ftruncate #:round #:fround #:random
               #:signum #:float-sign #:float-digits #:rational
               #:rationalize #:coerce))

    (defpackage :bigfloat
      (:use :common-lisp :bigfloat-impl)
      (:export #:lentz #:sum-power-series #:format-e #:format-f #:format-g))

    (shadowing-import
     '(bigfloat-impl:+ bigfloat-impl:- bigfloat-impl:* bigfloat-impl:/
       bigfloat-impl:1+ bigfloat-impl:1- bigfloat-impl:zerop
       bigfloat-impl:plusp bigfloat-impl:minusp bigfloat-impl:abs
       bigfloat-impl:sqrt bigfloat-impl:log bigfloat-impl:exp
       bigfloat-impl:sin bigfloat-impl:cos bigfloat-impl:tan
       bigfloat-impl:asin bigfloat-impl:acos bigfloat-impl:atan
       bigfloat-impl:sinh bigfloat-impl:cosh bigfloat-impl:tanh
       bigfloat-impl:asinh bigfloat-impl:acosh bigfloat-impl:atanh
       bigfloat-impl:expt bigfloat-impl:= bigfloat-impl:/=
       bigfloat-impl:< bigfloat-impl:> bigfloat-impl:<= bigfloat-impl:>=
       bigfloat-impl:scale-float bigfloat-impl:realpart bigfloat-impl:imagpart
       bigfloat-impl:complex bigfloat-impl:conjugate bigfloat-impl:max
       bigfloat-impl:min bigfloat-impl:cis bigfloat-impl:phase
       bigfloat-impl:floor bigfloat-impl:ffloor bigfloat-impl:incf
       bigfloat-impl:decf bigfloat-impl:realp bigfloat-impl:complexp
       bigfloat-impl:numberp bigfloat-impl:integer-decode-float
       bigfloat-impl:decode-float bigfloat-impl:float bigfloat-impl:ceiling
       bigfloat-impl:fceiling bigfloat-impl:truncate bigfloat-impl:ftruncate
       bigfloat-impl:round bigfloat-impl:fround bigfloat-impl:random
       bigfloat-impl:signum bigfloat-impl:float-sign bigfloat-impl:float-digits
       bigfloat-impl:rational bigfloat-impl:rationalize bigfloat-impl:coerce)
     :bigfloat)
    (do-external-symbols (s '#:bigfloat-impl)
      (export s '#:bigfloat))))

(in-package :bigfloat-impl)

;; Maxima's package setup shadowing-imports many CL numeric symbols from
;; BIGFLOAT-IMPL and later expects them to be fbound/macrobound.
;; Bind them to CL operators when present, otherwise to stable local fallbacks.
(defun %op-missing (name)
  (lambda (&rest args)
    (declare (ignore args))
    (error "BIGFLOAT-IMPL operator unavailable: ~S" name)))

(defun %asin-fallback (x)
  (cl:atan (/ x (cl:sqrt (- 1 (* x x))))))

(defun %acos-fallback (x)
  (- (/ cl:pi 2) (%asin-fallback x)))

(defun %asinh-fallback (x)
  (cl:log (+ x (cl:sqrt (+ (* x x) 1)))))

(defun %acosh-fallback (x)
  (cl:log (+ x (* (cl:sqrt (- x 1)) (cl:sqrt (+ x 1))))))

(defun %atanh-fallback (x)
  (* 0.5d0 (cl:log (/ (+ 1 x) (- 1 x)))))

(defun %bind-op (dst src &optional fallback)
  (setf (symbol-function dst)
        (cond
          ((fboundp src) (symbol-function src))
          (fallback (symbol-function fallback))
          (t (%op-missing src)))))

(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (pair
           '((+ cl:+)
             (- cl:-)
             (* cl:*)
             (/ cl:/)
             (1+ cl:1+)
             (1- cl:1-)
             (zerop cl:zerop)
             (plusp cl:plusp)
             (minusp cl:minusp)
             (abs cl:abs)
             (sqrt cl:sqrt)
             (log cl:log)
             (exp cl:exp)
             (sin cl:sin)
             (cos cl:cos)
             (tan cl:tan)
             (atan cl:atan)
             (sinh cl:sinh)
             (cosh cl:cosh)
             (tanh cl:tanh)
             (expt cl:expt)
             (= cl:=)
             (/= cl:/=)
             (< cl:<)
             (> cl:>)
             (<= cl:<=)
             (>= cl:>=)
             (scale-float cl:scale-float)
             (realpart cl:realpart)
             (imagpart cl:imagpart)
             (complex cl:complex)
             (conjugate cl:conjugate)
             (max cl:max)
             (min cl:min)
             (cis cl:cis)
             (phase cl:phase)
             (floor cl:floor)
             (ffloor cl:ffloor)
             (realp cl:realp)
             (complexp cl:complexp)
             (numberp cl:numberp)
             (integer-decode-float cl:integer-decode-float)
             (decode-float cl:decode-float)
             (float cl:float)
             (ceiling cl:ceiling)
             (fceiling cl:fceiling)
             (truncate cl:truncate)
             (ftruncate cl:ftruncate)
             (round cl:round)
             (fround cl:fround)
             (random cl:random)
             (signum cl:signum)
             (float-sign cl:float-sign)
             (float-digits cl:float-digits)
             (rational cl:rational)
             (rationalize cl:rationalize)
             (coerce cl:coerce)))
    (%bind-op (car pair) (cadr pair)))
  (%bind-op 'asin 'cl:asin '%asin-fallback)
  (%bind-op 'acos 'cl:acos '%acos-fallback)
  (%bind-op 'asinh 'cl:asinh '%asinh-fallback)
  (%bind-op 'acosh 'cl:acosh '%acosh-fallback)
  (%bind-op 'atanh 'cl:atanh '%atanh-fallback))

(defparameter epsilon 1d-16)
(defparameter %pi cl:pi)
(defparameter %e (cl:exp 1d0))

(defun bigfloat (x) x)
(defun complex-bigfloat (real &optional (imag 0))
  (complex real imag))
(defun to (x) x)
(defun maybe-to (x) x)

(in-package :bigfloat)
;; Stub: bigfloat:to just returns its argument (identity)
(defun to (x) x)
;; Stub: bigfloat:maybe-to just returns its argument (identity)
(defun maybe-to (x) x)
;; Stub: bigfloat:signum delegates to cl:signum
(defun signum (x) (cl:signum x))

;; Other packages referenced in Maxima
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-info)
    (defpackage :cl-info (:use :common-lisp)
      (:export #:get-cl-info-hashtable)))
  (unless (find-package :slatec)
    (defpackage :slatec (:use :common-lisp)))
  (unless (find-package :mt19937)
    (defpackage :mt19937 (:use :common-lisp)
      (:export #:make-random-state #:random-state-p))))

;; Maxima expects these names to be shadowed in :maxima (see
;; /tmp/maxima/src/maxima-package.lisp). Without this, files like commac.lisp
;; redefine CL symbols (notably FUNCTIONP), which breaks later loads.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :maxima)
    (defpackage :maxima
      (:use :common-lisp)
      (:nicknames :cl-macsyma :cl-maxima :macsyma)))
  (shadow '(continue float functionp array exp signum
            asin acos asinh acosh atanh tanh cosh sinh tan
            break gcd)
          :maxima))

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

;; def-simplifier compatible with Maxima defmfun-check semantics used by
;; simplification modules (supports options used in core Maxima files).
(defmacro def-simplifier (base-name-and-options lambda-list &body body)
  ;; Keep this stub macro intentionally simple and robust for Habu:
  ;; avoid complex destructuring/loop expansion-time features.
  (let* ((base-name (if (symbolp base-name-and-options)
                        base-name-and-options
                        (car base-name-and-options)))
         (opts (if (symbolp base-name-and-options) nil (cdr base-name-and-options)))
         (simpcheck (or (getf opts :simpcheck) :default))
         (custom-defmfun (getf opts :custom-defmfun))
         (skip-properties (or (getf opts :skip-properties) nil))
         (noun-name (intern (concatenate 'string "%" (symbol-name base-name))))
         (verb-name (intern (concatenate 'string "$" (symbol-name base-name))))
         (simp-name (intern (concatenate 'string "SIMP-" (symbol-name noun-name))))
         (form-arg (intern "FORM"))
         (z-arg (intern "%%SIMPFLAG"))
         (unused-arg (gensym "UNUSED-"))
         (arg-forms nil)
         (arg-count 0))
    (do ((l lambda-list (cdr l))
         (idx 1 (+ idx 1)))
        ((null l))
      (let ((arg (car l)))
        (unless (and (symbolp arg)
                     (> (length (symbol-name arg)) 0)
                     (char= (char (symbol-name arg) 0) #\&))
          (setq arg-count (+ arg-count 1))
          (push
           (list arg
                 (if (eq simpcheck :custom)
                     (list 'nth idx form-arg)
                     (list 'simpcheck (list 'nth idx form-arg) z-arg)))
           arg-forms))))
    (setq arg-forms (nreverse arg-forms))
    `(progn
       ,@(unless custom-defmfun
           `((defmfun ,verb-name (,@lambda-list)
               (ftake ',noun-name ,@lambda-list))))
       ,@(unless (member 'alias skip-properties)
           `((defprop ,verb-name ,noun-name alias)))
       ,@(unless (member 'reversealias skip-properties)
           `((defprop ,noun-name ,verb-name reversealias)))
       (defprop ,noun-name ,simp-name operators)
       (defprop ,noun-name ,verb-name noun)
       (defprop ,verb-name ,noun-name verb)
       (defun ,simp-name (,form-arg ,unused-arg ,z-arg)
         (declare (ignore ,unused-arg)
                  (ignorable ,z-arg))
         (arg-count-check ,arg-count ,form-arg nil)
         (let ,arg-forms
           (flet ((give-up (&key (noun-name ',noun-name)
                                 (args (list ,@lambda-list)))
                    (eqtest (list* (list noun-name) args) ,form-arg)))
             ,@body))))))
