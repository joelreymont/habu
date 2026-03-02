;; Bootstrap-only package declarations for external libraries that may be absent.
;; This file must not provide semantic stand-ins for Maxima runtime behavior.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :intl)
    (defpackage :intl
      (:use :common-lisp)
      (:export #:gettext #:ngettext #:dgettext #:dngettext
               #:setlocale #:textdomain #:*locale* #:*locale-directories*
               #:*default-domain* #:read-translatable-string))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :pregexp)
    (defpackage :pregexp
      (:use :common-lisp)
      (:export #:pregexp #:pregexp-match-positions #:pregexp-match
               #:pregexp-replace #:pregexp-quote))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bigfloat-impl)
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
               #:rationalize #:coerce))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bigfloat)
    (defpackage :bigfloat
      (:use :common-lisp :bigfloat-impl)
      (:export #:lentz #:sum-power-series #:format-e #:format-f #:format-g))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-info)
    (defpackage :cl-info
      (:use :common-lisp)
      (:export #:get-cl-info-hashtable)))
  (unless (find-package :slatec)
    (defpackage :slatec
      (:use :common-lisp)))
  (unless (find-package :mt19937)
    (defpackage :mt19937
      (:use :common-lisp)
      (:export #:make-random-state #:random-state-p))))
