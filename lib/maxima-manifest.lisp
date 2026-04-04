;; Authoritative Maxima source manifest for Habu.

(defparameter *habu-maxima-root-candidates*
  '("../maxima/"))

(defparameter *habu-maxima-files*
  '(
    ;; bootstrap
    "pregexp"
    "numerical/f2cl-package" "numerical/slatec" "numerical/f2cl-lib"
    "globals" "lmdcls" "letmac" "generr" "clmacs" "defmfun-check" "float-properties"
    "commac" "mormac" "compat"
    "defcal" "maxmac" "mopers" "mforma" "mrgmac" "rzmac" "strmac"
    "displm" "safe-recursion" "ratmac" "mhayat"
    "opers"
    "utils" "merror" "mutils"

    ;; core language/runtime
    "sumcon" "sublis" "mformt" "outmis" "ar"
    "comm" "comm2" "mlisp" "mmacro" "buildq"
    "simp" "float" "csimp" "csimp2" "zero"
    "logarc" "rpart" "numeric" "server" "macsys" "testsuite" "mload"
    "suprv1" "mactex" "dskfn" "mtrace" "mdebug"

    ;; algebraic database
    "inmis" "db"

    ;; factoring / rational
    "compar" "askp" "lesfac" "factor" "algfac" "nalgfa" "ufact"
    "ifactor" "rat3a" "rat3b" "rat3c" "rat3d" "rat3e" "nrat4"
    "ratout" "result"

    ;; translator and evaluator support
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

    ;; definite integration / residue support
    "residu" "defint"

    ;; translated packages / numerics that load cleanly in this tree
    "desoln" "elim" "invert" "hypgeo" "hyp" "hypergeometric" "nfloat"

    ;; final init
    "autol" "max_ext" "init-cl"))

(defun habu-maxima-path (root relpath)
  (and root (concatenate 'string root relpath)))

(defun habu-maxima-root-valid-p (root)
  (and root
       (probe-file (habu-maxima-path root "configure.ac"))
       (probe-file (habu-maxima-path root "src/lmdcls.lisp"))))

(defun habu-detect-maxima-root ()
  (find-if #'habu-maxima-root-valid-p *habu-maxima-root-candidates*))

(defun habu-maxima-dir (root subdir witness)
  (let ((dir (habu-maxima-path root subdir)))
    (and dir (probe-file (habu-maxima-path root witness)) dir)))

(defun habu-build-maxima-manifest (&optional (root (habu-detect-maxima-root)))
  (let ((srcdir (habu-maxima-dir root "src/" "src/mload.lisp"))
        (sharedir (habu-maxima-dir root "share/" "share/stringproc/stringproc.lisp"))
        (demodir (habu-maxima-dir root "demo/" "demo/manual.demo"))
        (docdir (or (habu-maxima-dir root "doc/" "doc/share/romberg.usg")
                    (habu-maxima-dir root "doc/" "doc/info")))
        (testsdir (habu-maxima-dir root "tests/" "tests/rtest1.mac")))
    (list :root root
          :srcdir srcdir
          :sharedir sharedir
          :demodir demodir
          :docdir docdir
          :testsdir testsdir
          :package-init (and srcdir (concatenate 'string srcdir "maxima-package.lisp"))
          :modules *habu-maxima-files*
          :module-count (length *habu-maxima-files*)
          :root-candidates *habu-maxima-root-candidates*)))

(defparameter *habu-maxima-manifest* (habu-build-maxima-manifest))

(defun habu-maxima-manifest-value (key)
  (getf *habu-maxima-manifest* key))

(defun habu-refresh-maxima-manifest ()
  (setq *habu-maxima-manifest* (habu-build-maxima-manifest)))
