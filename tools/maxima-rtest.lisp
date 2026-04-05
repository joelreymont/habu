;; Canonical Maxima rtest runner for Habu.
;;
;; Uses Maxima's own test-batch semantics instead of a custom comparison loop.
;; This keeps triage aligned with upstream error handling, expected-failure data,
;; *read-base* binding, and answers-from-file behavior.
;;
;; Usage patterns:
;;   ./zig-out/bin/habu tools/maxima-rtest.lisp        ; defaults to rtest1
;;   ;; or from the REPL/script after loading this file:
;;   (maxima::run-rtest "rtest12")

(load "lib/stdlib.habu")
(load "lib/maxima-manifest.lisp")

(defun habu-rtest-count (x)
  (cond ((null x) 0)
        ((consp x) (length (cdr x)))
        (t 0)))

(defun habu-normalize-test-name (name)
  (cond ((stringp name) name)
        ((symbolp name) (string-downcase (symbol-name name)))
        (t (format nil "~A" name))))

(defun habu-cli-test-name ()
  (let ((args (and (boundp '*command-line-args*) *command-line-args*)))
    (habu-normalize-test-name
     (or (and (consp args) (car args)) "rtest1"))))

(defun habu-absolute-maxima-root ()
  (namestring (truename (habu-maxima-manifest-value :root))))

(defun habu-maxima-srcdir ()
  (concatenate 'string (habu-absolute-maxima-root) "/src/"))

(defun habu-maxima-testsuite-file ()
  (concatenate 'string (habu-maxima-srcdir) "testsuite.lisp"))

(defun habu-maxima-package-init-file ()
  (concatenate 'string (habu-maxima-srcdir) "maxima-package.lisp"))

(defun habu-maxima-symbol (name)
  (nth-value 0 (find-symbol name (find-package :maxima))))

(defun habu-maxima-value (name)
  (let ((sym (habu-maxima-symbol name)))
    (unless sym
      (error "[HABU-RTEST] missing MAXIMA symbol ~A" name))
    (symbol-value sym)))

(defun habu-maxima-fdefinition (name)
  (let ((sym (habu-maxima-symbol name)))
    (unless sym
      (error "[HABU-RTEST] missing MAXIMA function ~A" name))
    (unless (fboundp sym)
      (error "[HABU-RTEST] MAXIMA function not bound ~A" name))
    (symbol-function sym)))

(defmacro habu-with-maxima-package (&body body)
  `(let ((*package* (or (find-package :maxima)
                        (error "[HABU-RTEST] MAXIMA package missing"))))
     ,@body))

(defun habu-load-testsuite-registry ()
  (load (habu-maxima-package-init-file))
  (load "lib/maxima-stubs.lisp")
  (load (habu-maxima-testsuite-file)))

(defun habu-ensure-clean-loader ()
  (multiple-value-bind (ok total fail missing attempted)
      (maxima-load-all :verbose nil)
    (when (or (/= fail 0)
              (/= attempted total)
              *maxima-failed*
              missing)
      (error "[HABU-RTEST] refusing dirty loader state: ok=~A total=~A fail=~A attempted=~A missing=~S failed=~S"
             ok total fail attempted missing *maxima-failed*))
    (values ok total fail attempted)))

(defun habu-testsuite-files ()
  (habu-maxima-value "$TESTSUITE_FILES"))

(defun habu-share-testsuite-files ()
  (habu-maxima-value "$SHARE_TESTSUITE_FILES"))

(defun habu-testsuite-entry-name (entry)
  (if (atom entry)
      entry
      (second entry)))

(defun habu-testsuite-entry-expected-failures (entry)
  (cond ((atom entry) nil)
        ((consp (caddr entry)) (cdaddr entry))
        (t (cddr entry))))

(defun habu-find-testsuite-entry (name)
  (let ((target (habu-normalize-test-name name)))
    (or (find-if (lambda (entry)
                   (equal (habu-testsuite-entry-name entry) target))
                 (cdr (habu-testsuite-files)))
        (find-if (lambda (entry)
                   (equal (habu-testsuite-entry-name entry) target))
                 (cdr (habu-share-testsuite-files))))))

(defun habu-resolve-rtest-path (entry)
  (let* ((target (habu-testsuite-entry-name entry))
         (file-search (habu-maxima-fdefinition "$FILE_SEARCH")))
    (habu-with-maxima-package
      (or (handler-case
              (funcall file-search target (habu-maxima-value "$FILE_SEARCH_TESTS"))
            (condition (e) nil))
          (handler-case
              (funcall file-search target (habu-maxima-value "$FILE_SEARCH_MAXIMA"))
            (condition (e) nil))))))

(defparameter *habu-rtest-name* nil)
(setq *habu-rtest-name* (habu-cli-test-name))
(habu-load-testsuite-registry)
(in-package :cl-user)
(unless (habu-find-testsuite-entry *habu-rtest-name*)
  (error "[HABU-RTEST] unknown canonical test ~A" *habu-rtest-name*))

(load "lib/maxima-loader.lisp")
(habu-ensure-clean-loader)
(load "lib/maxima-post-load.lisp")
(in-package :cl-user)

(defun run-rtest (name &key (show-all nil) (show-known-bugs nil) (showtime nil) (answers-from-file t))
  (let* ((entry (habu-find-testsuite-entry name))
         (expected-failures (and entry
                                 (habu-testsuite-entry-expected-failures entry)))
         (path (and entry (habu-resolve-rtest-path entry)))
         (test-batch (habu-maxima-fdefinition "TEST-BATCH"))
         (answers-var (habu-maxima-symbol "$BATCH_ANSWERS_FROM_FILE"))
         (filename nil)
         (diff nil)
         (unexpected-pass nil)
         (total nil))
    (unless entry
      (error "[HABU-RTEST] unknown canonical test ~A" name))
    (unless path
      (error "[HABU-RTEST] cannot resolve canonical test path for ~A" name))
    (habu-with-maxima-package
      (progv (list answers-var) (list answers-from-file)
        (or (errset
              (multiple-value-setq (filename diff unexpected-pass total)
                (funcall test-batch path expected-failures
                         :show-expected show-known-bugs
                         :show-all show-all
                         :showtime showtime)))
            (error "[HABU-RTEST] canonical test-batch hit an error break for ~A" path))
        (let ((diff-count (habu-rtest-count diff))
              (unexpected-pass-count (habu-rtest-count unexpected-pass)))
          (format t "~%[HABU-RTEST] file=~A total=~A diffs=~A unexpected-pass=~A~%"
                  filename
                  total
                  diff-count
                  unexpected-pass-count)
          (when (or (> diff-count 0) (> unexpected-pass-count 0))
            (error "[HABU-RTEST] canonical test-batch failed for ~A" filename)))
        (values filename diff unexpected-pass total)))))

(run-rtest *habu-rtest-name*)
