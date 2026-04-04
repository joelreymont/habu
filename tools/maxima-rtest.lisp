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
(load "lib/maxima-loader.lisp")

(defun habu-rtest-count (x)
  (cond ((null x) 0)
        ((consp x) (length (cdr x)))
        (t 0)))

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

(habu-ensure-clean-loader)
(load "lib/maxima-post-load.lisp")
(in-package :maxima)

(defun habu-normalize-test-name (name)
  (cond ((stringp name) name)
        ((symbolp name) (string-downcase (symbol-name name)))
        (t (format nil "~A" name))))

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
                 (cdr $testsuite_files))
        (find-if (lambda (entry)
                   (equal (habu-testsuite-entry-name entry) target))
                 (cdr $share_testsuite_files)))))

(defun habu-resolve-rtest-path (entry)
  (let ((target (habu-testsuite-entry-name entry)))
    (or (handler-case ($file_search target $file_search_tests)
          (condition (e) nil))
        (handler-case ($file_search target $file_search_maxima)
          (condition (e) nil)))))

(defun run-rtest (name &key (show-all nil) (show-known-bugs nil) (showtime nil) (answers-from-file t))
  (let* ((entry (habu-find-testsuite-entry name))
         (expected-failures (and entry
                                 (habu-testsuite-entry-expected-failures entry)))
         (path (and entry (habu-resolve-rtest-path entry)))
         (filename nil)
         (diff nil)
         (unexpected-pass nil)
         (total nil))
    (unless entry
      (error "[HABU-RTEST] unknown canonical test ~A" name))
    (unless path
      (error "[HABU-RTEST] cannot resolve canonical test path for ~A" name))
    (let (($batch_answers_from_file answers-from-file))
      (declare (special $batch_answers_from_file))
      (or (errset
            (multiple-value-setq (filename diff unexpected-pass total)
              (test-batch path expected-failures
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
      (values filename diff unexpected-pass total))))

(let* ((args (and (boundp '*command-line-args*) *command-line-args*))
       (test-name (or (and (consp args) (cadr args)) "rtest1")))
  (run-rtest test-name))
