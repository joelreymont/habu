#!/usr/bin/env sbcl --script
;;; Stage 1 Bootstrap Test
;;;
;;; Attempt to compile real functions from habu-arm64-codegen-clean.lisp
;;; using the Habu compiler running in SBCL.

(load "run-habu.lisp")

(format t "~%=== STAGE 1 BOOTSTRAP TEST ===~%")
(format t "Attempting to compile real compiler functions...~%~%")

;; Phase 1: Byte Utility Functions
(format t "Phase 1: Byte Utility Functions~%")
(format t "================================~%")

;; Test 1: my-mod
(format t "~%Test 1: my-mod~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (my-mod #x11 #x5)))))  ; 17 mod 5 = 2
  (format t "  Result: ~A (expected 2)~%" result)
  (unless (= result #x2)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 2: word-to-bytes
(format t "~%Test 2: word-to-bytes~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 ;; Test: 0x12345678 -> (0x78 0x56 0x34 0x12)
                 (car (word-to-bytes #x12345678))))))
  (format t "  Result: ~A (expected 120 = 0x78)~%" result)
  (unless (= result #x78)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 3: make-word
(format t "~%Test 3: make-word~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun make-word (b0 b1 b2 b3)
                   (+ b0 (* b1 #x100) (* b2 #x10000) (* b3 #x1000000)))
                 (make-word #x78 #x56 #x34 #x12)))))
  (format t "  Result: ~A (expected 305419896 = 0x12345678)~%" result)
  (unless (= result #x12345678)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Phase 2: ARM64 Instruction Encoders
(format t "~%~%Phase 2: ARM64 Instruction Encoders~%")
(format t "====================================~%")

;; Test 4: encode-word (wrapper)
(format t "~%Test 4: encode-word~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun encode-word (w)
                   (word-to-bytes w))
                 (length (encode-word #x12345678))))))
  (format t "  Result: ~A (expected 4)~%" result)
  (unless (= result #x4)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 5: arm64-movz
(format t "~%Test 5: arm64-movz~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun encode-word (w)
                   (word-to-bytes w))
                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((shifted-imm (* imm #x20)))
                       (let ((encoded (+ base (+ shifted-imm rd))))
                         (encode-word encoded)))))
                 ;; MOVZ X0, #42 -> encoded instruction
                 (length (arm64-movz #x0 #x2a))))))
  (format t "  Result: ~A (expected 4)~%" result)
  (unless (= result #x4)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 6: arm64-add
(format t "~%Test 6: arm64-add~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun encode-word (w)
                   (word-to-bytes w))
                 (defun arm64-add (rd rn rm)
                   (let ((base #x8B000000))
                     (let ((shifted-rm (* rm #x10000)))
                       (let ((shifted-rn (* rn #x20)))
                         (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
                           (encode-word encoded))))))
                 ;; ADD X0, X1, X2 -> encoded instruction
                 (length (arm64-add #x0 #x1 #x2))))))
  (format t "  Result: ~A (expected 4)~%" result)
  (unless (= result #x4)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Phase 3: Helper Functions
(format t "~%~%Phase 3: Helper Functions~%")
(format t "=========================~%")

;; Test 7: has-tag?
(format t "~%Test 7: has-tag?~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))
                 (+ (if (has-tag? (list 'lit #x5) 'lit) #x1 #x0)
                    (if (has-tag? (list 'var #x0) 'lit) #x10 #x0)
                    (if (has-tag? #x5 'lit) #x100 #x0))))))
  (format t "  Result: ~A (expected 1)~%" result)
  (unless (= result #x1)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 8: append-code
(format t "~%Test 8: append-code~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun append-code (c1 c2)
                   (append c1 c2))
                 (length (append-code (list #x1 #x2) (list #x3 #x4 #x5)))))))
  (format t "  Result: ~A (expected 5)~%" result)
  (unless (= result #x5)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 9: count-instrs
(format t "~%Test 9: count-instrs~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun count-instrs (code)
                   (/ (length code) #x4))
                 (count-instrs (list #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8))))))
  (format t "  Result: ~A (expected 2)~%" result)
  (unless (= result #x2)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Phase 4: Environment Functions
(format t "~%~%Phase 4: Environment Functions~%")
(format t "===============================~%")

;; Test 10: runtime-lookup
(format t "~%Test 10: runtime-lookup~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun runtime-lookup (name runtime-addrs)
                   (if (nil? runtime-addrs)
                       #x0
                       (if (eq (car (car runtime-addrs)) name)
                           (cdr (car runtime-addrs))
                           (runtime-lookup name (cdr runtime-addrs)))))
                 (let ((addrs (list (cons 'cons #x100)
                                    (cons 'car #x200)
                                    (cons 'cdr #x300))))
                   (runtime-lookup 'car addrs))))))
  (format t "  Result: ~A (expected 512 = 0x200)~%" result)
  (unless (= result #x200)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

(format t "~%=== ALL STAGE 1 BOOTSTRAP TESTS PASSED ===~%")
(format t "~%Successfully compiled:~%")
(format t "  - Byte utilities (my-mod, word-to-bytes, make-word)~%")
(format t "  - ARM64 encoders (encode-word, arm64-movz, arm64-add)~%")
(format t "  - IR helpers (has-tag?, append-code, count-instrs)~%")
(format t "  - Environment (runtime-lookup)~%")

(sb-ext:quit :unix-status 0)
