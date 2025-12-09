;; Test compiling compiler subsets to native executables
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

;; Use test-specific package to avoid polluting :habu namespace
(defpackage :habu-test-compiler-subsets
  (:use :cl))
(in-package :habu-test-compiler-subsets)

(format t "~%=== Test compiler subsets native compilation ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/cps_~A" name)))
    (handler-case
        (progn
          (habu:deliver source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn
                  (format t "[PASS] ~A = ~A~%" name code)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
                  (incf *tests-failed*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *tests-failed*)))))

;; Test 1: ARM64 encoder functions
(test-native "arm64-encode"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)
           (logand (ash word -16) #xFF)
           (logand (ash word -24) #xFF)))
   (defun gen-ret ()
     (encode-word #xD65F03C0))
   (car (gen-ret))"
  #xC0)

;; Test 2: IR generation with if-chains - simpler version
(test-native "ir-gen"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (list 'lit expr)
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                   (list 'lit 0)))
             (list 'lit 0))))
   (let ((ir (compile-expr (list '+ 10 32))))
     (if (eq (car ir) 'add)
         42
         1))"
  42)

;; Test 3: IR evaluation
(test-native "ir-eval"
  "(defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'add)
             (+ (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             (if (eq (car ir) 'mul)
                 (* (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                 0))))
   (eval-ir (list 'add (list 'mul (list 'lit 3) (list 'lit 4)) (list 'lit 5)))"
  17)

;; Test 4: Combined compile + eval
(test-native "compile-eval"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (list 'lit expr)
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                   (if (eq op '*)
                       (list 'mul (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                       (list 'lit 0))))
             (list 'lit 0))))
   (defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'add)
             (+ (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             (if (eq (car ir) 'mul)
                 (* (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                 0))))
   (eval-ir (compile-expr (list '+ (list '* 3 4) 5)))"
  17)

;; Test 5: Environment lookup with labels
(test-native "env-lookup"
  "(defun lookup (name env depth)
     (labels ((find-in (e d)
                (if (null e)
                    (list 'global name)
                    (let ((binding (car e)))
                      (if (eq (car binding) name)
                          (list 'local d (cdr binding))
                          (find-in (cdr e) (+ d 1)))))))
       (find-in env depth)))
   (let* ((env (cons (cons 'x 0) (cons (cons 'y 1) nil)))
          (result (lookup 'y env 0)))
     (if (eq (car result) 'local)
         (caddr result)
         99))"
  1)

;; Test 6: Instruction encoder - single function test
;; MOVZ x0, #42 = #xD2800540 -> second byte #x05 = 5
(test-native "multi-encode"
  "(defun encode (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)))
   (defun movz (rd imm)
     (encode (logior #xD2800000 (ash imm 5) rd)))
   (let ((bytes (movz 0 42)))
     (+ (car bytes) (cadr bytes)))"
  69)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
