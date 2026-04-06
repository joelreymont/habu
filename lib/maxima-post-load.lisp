;; Bootstrap-only Maxima post-load wiring.
;; This file must not change upstream Maxima semantics.

(unless (boundp '*habu-maxima-manifest*)
  (load "lib/maxima-manifest.lisp"))

(in-package :maxima)

(defvar *habu-runtime-globals-initialized* nil)

(defun habu-default-maxima-userdir ()
  (handler-case
      (concatenate 'string (namestring (user-homedir-pathname)) ".maxima/")
    (condition () nil)))

(defun habu-build-patterns (dir extensions &optional recursivep)
  (when dir
    (mapcar (lambda (ext)
              (pathname (if recursivep
                            (concatenate 'string dir "**/*." ext)
                            (concatenate 'string dir "*." ext))))
            extensions)))

(defun habu-search-mlist (&rest pattern-groups)
  (cons '(mlist) (remove nil (apply #'append (remove nil pattern-groups)))))

(let ((topdir (cl-user::habu-maxima-manifest-value :root))
      (srcdir (cl-user::habu-maxima-manifest-value :srcdir))
      (sharedir (cl-user::habu-maxima-manifest-value :sharedir))
      (demodir (cl-user::habu-maxima-manifest-value :demodir))
      (docdir (cl-user::habu-maxima-manifest-value :docdir))
      (testsdir (cl-user::habu-maxima-manifest-value :testsdir))
      (userdir (habu-default-maxima-userdir)))
  (when topdir
    (setf *maxima-topdir* topdir))
  (when srcdir
    (setf *maxima-srcdir* srcdir))
  (when sharedir
    (setf *maxima-sharedir* sharedir))
  (when demodir
    (setf *maxima-demodir* demodir))
  (when docdir
    (setf *maxima-docdir* docdir))
  (when testsdir
    (setf *maxima-testsdir* testsdir))
  (when userdir
    (setf *maxima-userdir* userdir)
    (when (boundp '$maxima_userdir)
      (setq $maxima_userdir userdir))
    (when (boundp '*variable-initial-values*)
      (setf (gethash '$maxima_userdir *variable-initial-values*) userdir)))

  (setf $file_search_lisp
        (habu-search-mlist
         (habu-build-patterns userdir '("lisp") t)
         (habu-build-patterns sharedir '("lisp") t)
         (habu-build-patterns srcdir '("lisp"))
         (habu-build-patterns topdir '("lisp"))
         (habu-build-patterns testsdir '("lisp"))))

  (setf $file_search_maxima
        (habu-search-mlist
         (habu-build-patterns userdir '("mac" "wxm") t)
         (habu-build-patterns sharedir '("mac" "wxm") t)
         (habu-build-patterns srcdir '("mac" "wxm"))
         (habu-build-patterns topdir '("mac" "wxm"))
         (habu-build-patterns testsdir '("mac" "wxm"))))

  (setf $file_search_demo
        (habu-search-mlist
         (habu-build-patterns sharedir '("demo" "dem" "dm1" "dm2" "dm3" "dmt") t)
         (habu-build-patterns demodir '("demo" "dem" "dm1" "dm2" "dm3" "dmt"))))

  (setf $file_search_usage
        (habu-search-mlist
         (habu-build-patterns sharedir '("usg") t)
         (habu-build-patterns docdir '("usg") t)))

  (when testsdir
    (setf $file_search_tests
          (habu-search-mlist
           (habu-build-patterns testsdir '("lisp" "mac" "wxm"))))))

(unless *habu-runtime-globals-initialized*
  (initialize-runtime-globals)
  (setq *habu-runtime-globals-initialized* t))
