#!/usr/bin/env sbcl --script
;;; Test: Run Compiled Codegen Functions
;;;
;;; Verify that the codegen functions compiled by Habu produce
;;; correct ARM64 machine code.

(load "run-habu.lisp")

(format t "~%=== TESTING COMPILED CODEGEN FUNCTIONS ===~%~%")

;; Test 1: arm64-movz instruction generator
(format t "Test 1: arm64-movz~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))

                 (defun get-byte (n offset)
                   (my-mod (/ n (expt #x100 offset)) #x100))

                 (defun make-word (b0 b1 b2 b3)
                   (+ b0 (* b1 #x100) (* b2 #x10000) (* b3 #x1000000)))

                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((rd-bits rd)
                           (imm-bits (* imm #x20)))
                       (+ base rd-bits imm-bits))))

                 ;; Test: movz x0, #42 should produce 0xD2800540
                 ;; rd=0, imm=42 -> 0xD2800000 + 0 + (42 * 32) = 0xD2800000 + 1344 = 0xD2800540
                 (arm64-movz #x0 #x2a)))))
  (format t "  Result: ~X (expected D2800540)~%" result)
  (unless (= result #xD2800540)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 2: arm64-add instruction generator
(format t "~%Test 2: arm64-add~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun arm64-add (rd rn rm)
                   (let ((base #x8B000000))
                     (+ base rd (* rn #x20) (* rm #x10000))))

                 ;; Test: add x0, x1, x2 -> 0x8B020020
                 (arm64-add #x0 #x1 #x2)))))
  (format t "  Result: ~X (expected 8B020020)~%" result)
  (unless (= result #x8B020020)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 3: arm64-ldr instruction generator
(format t "~%Test 3: arm64-ldr~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun arm64-ldr (rt rn offset)
                   (let ((base #xF9400000))
                     (let ((imm12 (/ offset #x8)))
                       (+ base rt (* rn #x20) (* imm12 #x400)))))

                 ;; Test: ldr x0, [x19, #8] -> offset/8=1, so 0xF9400260
                 (arm64-ldr #x0 #x13 #x8)))))
  (format t "  Result: ~X (expected F9400660)~%" result)
  (unless (= result #xF9400660)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 4: has-tag? utility
(format t "~%Test 4: has-tag?~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))

                 (let ((lit-ir (list 'lit #x42))
                       (var-ir (list 'var #x1)))
                   (+ (if (has-tag? lit-ir 'lit) #x1 #x0)
                      (if (has-tag? var-ir 'var) #x10 #x0)
                      (if (has-tag? lit-ir 'var) #x100 #x0)))))))
  (format t "  Result: ~X (expected 11)~%" result)
  (unless (= result #x11)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 5: runtime-lookup
(format t "~%Test 5: runtime-lookup~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun runtime-lookup (name runtime-addrs)
                   (if (nil? runtime-addrs)
                       nil
                       (if (eq (car (car runtime-addrs)) name)
                           (cdr (car runtime-addrs))
                           (runtime-lookup name (cdr runtime-addrs)))))

                 (let ((addrs (list (cons 'habu_cons #x1000)
                                    (cons 'habu_car #x2000)
                                    (cons 'habu_cdr #x3000))))
                   (runtime-lookup 'habu_car addrs))))))
  (format t "  Result: ~X (expected 2000)~%" result)
  (unless (= result #x2000)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 6: count-instrs
(format t "~%Test 6: count-instrs~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun count-instrs (code)
                   (if (nil? code)
                       #x0
                       (+ #x1 (count-instrs (cddddr code)))))

                 ;; Each instruction is 4 bytes. List with 8 bytes = 2 instructions
                 (count-instrs (list #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8))))))
  (format t "  Result: ~A (expected 2)~%" result)
  (unless (= result #x2)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 7: append-code
(format t "~%Test 7: append-code~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun append-code (c1 c2)
                   (append c1 c2))

                 (length (append-code (list #x1 #x2 #x3 #x4)
                                      (list #x5 #x6 #x7 #x8)))))))
  (format t "  Result: ~A (expected 8)~%" result)
  (unless (= result #x8)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 8: Full instruction sequence generation
(format t "~%Test 8: Generate add x0, x0, x1 sequence~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun arm64-add (rd rn rm)
                   (let ((base #x8B000000))
                     (+ base rd (* rn #x20) (* rm #x10000))))

                 (defun encode-word (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))

                 (defun my-mod (n d)
                   (- n (* d (/ n d))))

                 ;; Generate: add x0, x0, x1
                 (let ((instr (arm64-add #x0 #x0 #x1)))
                   ;; Verify instruction value
                   instr)))))
  (format t "  Result: ~X (expected 8B010000)~%" result)
  (unless (= result #x8B010000)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 9: Test cmp-zero helper
(format t "~%Test 9: cmp-zero~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun arm64-cmp (rn rm)
                   (let ((base #xEB00001F))
                     (+ base (* rn #x20) (* rm #x10000))))

                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (+ base rd (* imm #x20))))

                 (defun cmp-zero ()
                   (list (arm64-movz #x9 #x0)
                         (arm64-cmp #x0 #x9)))

                 ;; Generate cmp x0, #0 sequence
                 (car (cdr (cmp-zero)))))))  ; Get the CMP instruction
  (format t "  Result: ~X (expected EB09001F)~%" result)
  (unless (= result #xEB09001F)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 10: Test temp-slot-offset calculation
(format t "~%Test 10: temp-slot-offset~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun temp-slot-offset (depth)
                   (- #x0 (* (+ depth #x1) #x10)))

                 ;; depth 0 -> -16, depth 1 -> -32, depth 2 -> -48
                 (+ (+ #x100 (temp-slot-offset #x0))    ; 256 - 16 = 240
                    (+ #x100 (temp-slot-offset #x1))    ; 256 - 32 = 224
                    (+ #x100 (temp-slot-offset #x2))))))) ; 256 - 48 = 208
  ;; Expected: 240 + 224 + 208 = 672 = 0x2A0
  (format t "  Result: ~X (expected 2A0)~%" result)
  (unless (= result #x2A0)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

(format t "~%=== ALL COMPILED CODEGEN TESTS PASSED ===~%")
(format t "~%Successfully tested:~%")
(format t "  - arm64-movz instruction generation~%")
(format t "  - arm64-add instruction generation~%")
(format t "  - arm64-ldr instruction generation~%")
(format t "  - has-tag? IR utility~%")
(format t "  - runtime-lookup symbol table~%")
(format t "  - count-instrs code counter~%")
(format t "  - append-code sequence builder~%")
(format t "  - Full instruction sequences~%")
(format t "  - cmp-zero helper~%")
(format t "  - temp-slot-offset calculation~%")

(sb-ext:quit :unix-status 0)
