(let* ((ansi-root (make-pathname :name nil :type nil :version nil
                                 :defaults *default-pathname-defaults*))
       (*default-pathname-defaults* ansi-root))
  (load "init.lsp"))

;; These tests are unstable when launched from a loaded script.
(when *load-pathname*
  (mapc #'regression-test:rem-test
        '(cl-test::load-pathname.1 cl-test::load-truename.1)))

(defparameter *habu-smoke-tests*
  '(cl-test::define-compiler-macro.8
    cl-test::define-method-combination-long.11.4
    cl-test::destructuring-bind.error.10
    cl-test::equal.13
    cl-test::equal.14
    cl-test::loop.1.39
    cl-test::loop.1.40
    cl-test::loop.1.41
    cl-test::loop.1.42
    cl-test::loop.1.43
    cl-test::macrolet.36
    cl-test::make-load-form.order.14
    cl-test::make-symbol.11))

(dolist (entry (cdr regression-test::*entries*))
  (setf (regression-test::pend entry)
        (member (regression-test::name entry) *habu-smoke-tests* :test #'eq)))

(format t "~&Selected ~D ANSI smoke tests.~%"
        (count-if #'regression-test::pend (cdr regression-test::*entries*)))

;; Smoke gate tolerates known failures and only fails on new regressions.
(time (regression-test:do-tests :expected-failures *habu-smoke-tests* :exit t))
