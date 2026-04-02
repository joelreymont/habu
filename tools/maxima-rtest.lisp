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
(maxima-load-all :verbose nil)
(load "lib/maxima-post-load.lisp")
(in-package :maxima)

(defun habu-normalize-test-name (name)
  (cond ((stringp name) name)
        ((symbolp name) (subseq (print-invert-case name) 1))
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

(defun habu-resolve-rtest-path (name)
  (let* ((target (habu-normalize-test-name name))
         (testsdir (habu-maxima-manifest-value :testsdir)))
    (or (handler-case ($file_search target $file_search_tests)
          (condition (e) nil))
        (handler-case ($file_search target $file_search_maxima)
          (condition (e) nil))
        (and testsdir
             (format nil "~A~A.mac" testsdir target)))))

(defun run-rtest (name &key (show-all nil) (show-known-bugs nil) (showtime nil) (answers-from-file t))
  (let* ((entry (habu-find-testsuite-entry name))
         (expected-failures (if entry
                                (habu-testsuite-entry-expected-failures entry)
                                nil))
         (path (habu-resolve-rtest-path name))
         (filename nil)
         (diff nil)
         (unexpected-pass nil)
         (total nil))
    (unless path
      (format t "Cannot resolve ~A~%" name)
      (return-from run-rtest nil))
    (let (($batch_answers_from_file answers-from-file))
      (declare (special $batch_answers_from_file))
      (or (errset
            (multiple-value-setq (filename diff unexpected-pass total)
              (test-batch path expected-failures
                          :show-expected show-known-bugs
                          :show-all show-all
                          :showtime showtime)))
          (progn
            (format t "~%[HABU-RTEST] canonical test-batch hit an error break for ~A~%" path)
            (return-from run-rtest nil)))
      (format t "~%[HABU-RTEST] file=~A total=~A diffs=~A unexpected-pass=~A~%"
              filename
              total
              (length (cdr diff))
              (length (cdr unexpected-pass)))
      (values filename diff unexpected-pass total))))

(let* ((args (and (boundp '*command-line-args*) *command-line-args*))
       (test-name (or (and (consp args) (cadr args)) "rtest1")))
  (run-rtest test-name))
