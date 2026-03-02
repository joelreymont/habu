;; Maxima source loader for Habu.
;;
;; Loads a broad ordered subset of Maxima source files with per-file
;; error reporting so we can iteratively raise compatibility.

(defparameter *maxima-source-candidates*
  '("../maxima/src/" "../maxima/src/src/" "../maxima/"
    "/tmp/maxima/src/" "/tmp/maxima/src/src/" "/tmp/maxima/"))

(defun maxima-source-has-core-p (dir)
  (and dir
       (probe-file (concatenate 'string dir "lmdcls.lisp"))))

(defun detect-maxima-source-dir ()
  (find-if #'maxima-source-has-core-p *maxima-source-candidates*))

(defparameter *maxima-source-dir* (detect-maxima-source-dir))
(defparameter *maxima-package-init*
  (and *maxima-source-dir*
       (concatenate 'string *maxima-source-dir* "maxima-package.lisp")))

;; Prefer upstream package definitions when available so package semantics
;; match Maxima source without local rewrites.
(when (and *maxima-package-init*
           (probe-file *maxima-package-init*))
  (load *maxima-package-init*))

(load "lib/maxima-stubs.lisp")

(defparameter *maxima-files*
  '(
    ;; bootstrap
    "lmdcls" "letmac" "generr" "clmacs" "commac" "mormac" "globals" "compat"
    "defcal" "maxmac" "mopers" "mforma" "mrgmac" "rzmac" "strmac"
    "displm" "safe-recursion" "ratmac" "mhayat"
    "opers"
    "utils" "merror" "mutils"

    ;; core language/runtime
    "sumcon" "sublis" "mformt" "outmis" "ar"
    "comm" "comm2" "mlisp" "mmacro" "buildq"
    "simp" "float" "csimp" "csimp2" "zero"
    "logarc" "rpart" "numeric" "server" "macsys" "testsuite" "mload"
    "float-properties"
    "suprv1" "mactex" "dskfn" "mtrace" "mdebug"

    ;; algebraic database
    "inmis" "db"

    ;; factoring / rational
    "compar" "askp" "lesfac" "factor" "algfac" "nalgfa" "ufact"
    "ifactor" "rat3a" "rat3b" "rat3c" "rat3d" "rat3e" "nrat4"
    "ratout" "result"

    ;; translator and evaluator support
    ;; transl.lisp depends on DEF%TR from transm.lisp.
    "transm" "transl" "transs" "trans1" "trans2" "trans3" "trans4" "trans5"
    "transf" "troper" "trutil" "trmode" "trdata" "trpred" "transq"
    "acall" "fcall" "evalw" "trprop" "mdefun"

    ;; pattern / reader / display / docs helpers
    "schatc" "matcom" "matrun" "nisimp" "nparse"
    "displa" "nforma" "grind" "macdes"

    ;; algebra and trig
    "spgcd" "ezgcd" "trigi" "trigo" "trgred"
    "bessel" "ellipt" "airy" "intpol"

    ;; calculus and special functions
    "sinint" "sin" "risch" "specfn"

    ;; matrix / determinant / limits / solve
    "mat" "linnew" "matrix" "sprdet" "newinv" "newdet"
    "tlimit" "limit"
    "solve" "psolve" "algsys" "sqrtdenest" "polyrz" "cpoly" "polynomialp"

    ;; misc high-traffic modules
    "scs" "asum" "optim" "marray" "mdot" "irinte" "series"
    "numth" "laplac" "pade" "homog" "combin" "nset"
    "pois2" "pois3"
    "rand-mt19937" "maxmin" "nummod" "conjugate"
    "expintegral" "gamma" "mstuff"

    ;; definite integration / residue support now load cleanly after the
    ;; current core bootstrap; keep them late until we tighten dependency order.
    "residu" "defint"

    ;; translated packages / numerics that load cleanly in this tree
    "desoln" "elim" "invert" "hypgeo" "hyp" "hypergeometric" "nfloat"

    ;; final init
    "autol" "max_ext" "init-cl"))

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
        (format t "source: ~A~%" source-dir)
        (format t "loaded: 0/~D~%" total)
        (format t "failed: ~D~%" total)
        (format t "missing source root: expected lmdcls.lisp under one of ~S~%" *maxima-source-candidates*))
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
      (format t "source: ~A~%" source-dir)
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
