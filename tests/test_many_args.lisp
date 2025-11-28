;;; Test functions with more than 8 arguments
;;; Uses let* pattern to avoid deep nesting bug
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Many Arguments (>8) Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-native (name source expected)
  (handler-case
    (let* ((forms (nc-read-all source))
           (bytes (nc-compile-program forms nil))
           (output-path (format nil "/tmp/native_~A" name)))
      (habu-macho:deliver-native-with-heap output-path bytes)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;; Test 1: Exactly 8 args (boundary test) - using flat nesting to avoid depth bug
(test-native "eight-args"
             "(defun sum8 (a b c d e f g h)
                (+ (+ (+ a b) (+ c d)) (+ (+ e f) (+ g h))))
              (sum8 1 2 3 4 5 6 7 8)"
             36)

;; Test 2: 9 args (first stack arg) - access all args
(test-native "nine-args-access"
             "(defun f (a b c d e f g h i)
                (let* ((s1 (+ a b))
                       (s2 (+ c d))
                       (s3 (+ e f))
                       (s4 (+ g h))
                       (s5 (+ s1 s2))
                       (s6 (+ s3 s4))
                       (s7 (+ s5 s6)))
                  (+ s7 i)))
              (f 1 2 3 4 5 6 7 8 9)"
             45)

;; Test 3: 10-arg function accessing arg 8 (9th arg, first stack arg)
(test-native "tenth-arg-ninth"
             "(defun get-ninth (a b c d e f g h i j) i)
              (get-ninth 10 20 30 40 50 60 70 80 90 100)"
             90)

;; Test 4: 10-arg function accessing arg 9 (10th arg, second stack arg)
(test-native "tenth-arg-tenth"
             "(defun get-tenth (a b c d e f g h i j) j)
              (get-tenth 10 20 30 40 50 60 70 80 90 100)"
             100)

;; Test 5: 10-arg function using all args
(test-native "ten-args-sum"
             "(defun sum10 (a b c d e f g h i j)
                (let* ((s1 (+ a b)) (s2 (+ c d)) (s3 (+ e f)) (s4 (+ g h)) (s5 (+ i j))
                       (s6 (+ s1 s2)) (s7 (+ s3 s4)) (s8 (+ s6 s7)))
                  (+ s8 s5)))
              (sum10 1 2 3 4 5 6 7 8 9 10)"
             55)

;; Test 6: 12-arg function
(test-native "twelve-args"
             "(defun sum12 (a b c d e f g h i j k l)
                (let* ((s1 (+ a b)) (s2 (+ c d)) (s3 (+ e f)) (s4 (+ g h))
                       (s5 (+ i j)) (s6 (+ k l))
                       (s7 (+ s1 s2)) (s8 (+ s3 s4)) (s9 (+ s5 s6))
                       (s10 (+ s7 s8)))
                  (+ s10 s9)))
              (sum12 1 2 3 4 5 6 7 8 9 10 11 12)"
             78)

;; Test 7: funcall with many args (via closure)
(test-native "funcall-ten-args"
             "(defun sum10 (a b c d e f g h i j)
                (let* ((s1 (+ a b)) (s2 (+ c d)) (s3 (+ e f)) (s4 (+ g h)) (s5 (+ i j))
                       (s6 (+ s1 s2)) (s7 (+ s3 s4)) (s8 (+ s6 s7)))
                  (+ s8 s5)))
              (funcall #'sum10 1 2 3 4 5 6 7 8 9 10)"
             55)

;; Test 8: labels with many args
(test-native "labels-ten-args"
             "(labels ((lsum (a b c d e f g h i j)
                        (let* ((s1 (+ a b)) (s2 (+ c d)) (s3 (+ e f)) (s4 (+ g h)) (s5 (+ i j))
                               (s6 (+ s1 s2)) (s7 (+ s3 s4)) (s8 (+ s6 s7)))
                          (+ s8 s5))))
                (lsum 1 2 3 4 5 6 7 8 9 10))"
             55)

(format t "~%Results: ~A passed, ~A failed~%" *pass-count* *fail-count*)
(sb-ext:exit :code (if (= *fail-count* 0) 0 1))
