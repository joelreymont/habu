#!/usr/bin/env sbcl --script
;;; Stage 1 Bootstrap Test - Part C
;;;
;;; Full codegen test: compile a mini-codegen that generates real ARM64 bytecode
;;; and verify the bytecode is correct.

(load "run-habu.lisp")

(format t "~%=== STAGE 1 BOOTSTRAP TEST - Part C ===~%")
(format t "Full mini-codegen compilation and verification...~%~%")

;; Test 1: Generate correct bytecode for MOVZ X0, #42
;; Expected: D2800540 (little-endian: 40 05 80 D2)
(format t "Test 1: Generate MOVZ X0, #42~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((shifted-imm (* imm #x20)))
                       (let ((encoded (+ base (+ shifted-imm rd))))
                         (word-to-bytes encoded)))))
                 ;; Generate MOVZ X0, #42
                 ;; Tagged value: 42 << 4 = 672 = 0x2A0
                 ;; Encoded: 0xD2800000 | (0x2A0 << 5) | 0 = 0xD2805400
                 (let ((code (arm64-movz #x0 #x2A0)))
                   ;; Return first byte (should be 0x00)
                   (car code))))))
  (format t "  First byte: ~A (expected 0)~%" result)
  (unless (= result #x0)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 2: Verify full instruction encoding
(format t "~%Test 2: Verify full MOVZ encoding~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((shifted-imm (* imm #x20)))
                       (let ((encoded (+ base (+ shifted-imm rd))))
                         (word-to-bytes encoded)))))
                 ;; MOVZ X0, #42 (tagged: 0x2A0)
                 ;; Encoding: D2800000 | (0x2A0 << 5) | 0
                 ;; = D2800000 | 5400 = D2805400
                 (let ((code (arm64-movz #x0 #x2A0)))
                   ;; Reconstruct and verify
                   (+ (car code)
                      (* (cadr code) #x100)
                      (* (caddr code) #x10000)
                      (* (cadddr code) #x1000000)))))))
  (format t "  Encoded word: 0x~X (expected 0xD2805400)~%" result)
  (unless (= result #xD2805400)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 3: Full mini-codegen for literals
(format t "~%Test 3: Mini-codegen for literals~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))
                 (defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((shifted-imm (* imm #x20)))
                       (let ((encoded (+ base (+ shifted-imm rd))))
                         (word-to-bytes encoded)))))
                 ;; Mini codegen
                 (defun mini-codegen (ir)
                   (cond
                     ((has-tag? ir 'lit)
                      (let ((value (cadr ir)))
                        (let ((tagged (* value #x10)))
                          (arm64-movz #x0 tagged))))
                     (t (list))))
                 ;; Generate code for (lit 42)
                 (let ((code (mini-codegen (list 'lit #x2a))))
                   ;; Verify it's MOVZ X0, #(42<<4) = MOVZ X0, #0x2A0
                   (+ (car code)
                      (* (cadr code) #x100)
                      (* (caddr code) #x10000)
                      (* (cadddr code) #x1000000)))))))
  (format t "  Encoded word: 0x~X (expected 0xD2805400)~%" result)
  (unless (= result #xD2805400)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 4: Codegen with arithmetic
(format t "~%Test 4: Codegen with ADD~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))
                 (defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((shifted-imm (* imm #x20)))
                       (let ((encoded (+ base (+ shifted-imm rd))))
                         (word-to-bytes encoded)))))
                 (defun arm64-add (rd rn rm)
                   (let ((base #x8B000000))
                     (let ((shifted-rm (* rm #x10000)))
                       (let ((shifted-rn (* rn #x20)))
                         (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
                           (word-to-bytes encoded))))))
                 (defun arm64-mov (rd rn)
                   ;; MOV as ORR Xd, XZR, Xn
                   (let ((base #xAA0003E0))
                     (let ((shifted-rn (* rn #x10000)))
                       (let ((encoded (+ base (+ shifted-rn rd))))
                         (word-to-bytes encoded)))))
                 ;; Mini codegen with add
                 (defun mini-codegen (ir)
                   (cond
                     ((has-tag? ir 'lit)
                      (arm64-movz #x0 (* (cadr ir) #x10)))
                     ((has-tag? ir 'add)
                      (let ((left-code (mini-codegen (cadr ir)))
                            (save-code (arm64-mov #x1 #x0))
                            (right-code (mini-codegen (caddr ir)))
                            (add-code (arm64-add #x0 #x1 #x0)))
                        (append left-code
                                save-code
                                right-code
                                add-code)))
                     (t (list))))
                 ;; Generate code for (add (lit 5) (lit 3))
                 ;; Should be 4 instructions = 16 bytes
                 (length (mini-codegen (list 'add
                                             (list 'lit #x5)
                                             (list 'lit #x3))))))))
  (format t "  Code length: ~A bytes (expected 16)~%" result)
  (unless (= result #x10)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 5: Verify ADD instruction encoding
(format t "~%Test 5: Verify ADD X0, X1, X0 encoding~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun arm64-add (rd rn rm)
                   ;; ADD Xd, Xn, Xm
                   ;; Base: 0x8B000000 | (rm << 16) | (rn << 5) | rd
                   (let ((base #x8B000000))
                     (let ((shifted-rm (* rm #x10000)))
                       (let ((shifted-rn (* rn #x20)))
                         (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
                           (word-to-bytes encoded))))))
                 ;; ADD X0, X1, X0
                 ;; = 0x8B000000 | (0 << 16) | (1 << 5) | 0
                 ;; = 0x8B000020
                 (let ((code (arm64-add #x0 #x1 #x0)))
                   (+ (car code)
                      (* (cadr code) #x100)
                      (* (caddr code) #x10000)
                      (* (cadddr code) #x1000000)))))))
  (format t "  Encoded word: 0x~X (expected 0x8B000020)~%" result)
  (unless (= result #x8B000020)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 6: End-to-end: generate and verify complete (add (lit 5) (lit 3))
(format t "~%Test 6: End-to-end codegen verification~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))
                 (defun my-mod (n d)
                   (- n (* d (/ n d))))
                 (defun word-to-bytes (w)
                   (list (my-mod w #x100)
                         (my-mod (/ w #x100) #x100)
                         (my-mod (/ w #x10000) #x100)
                         (my-mod (/ w #x1000000) #x100)))
                 (defun reconstruct-word (bytes)
                   (+ (car bytes)
                      (* (cadr bytes) #x100)
                      (* (caddr bytes) #x10000)
                      (* (cadddr bytes) #x1000000)))
                 (defun arm64-movz (rd imm)
                   (let ((base #xD2800000))
                     (let ((shifted-imm (* imm #x20)))
                       (let ((encoded (+ base (+ shifted-imm rd))))
                         (word-to-bytes encoded)))))
                 (defun arm64-add (rd rn rm)
                   (let ((base #x8B000000))
                     (let ((shifted-rm (* rm #x10000)))
                       (let ((shifted-rn (* rn #x20)))
                         (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
                           (word-to-bytes encoded))))))
                 (defun arm64-mov (rd rn)
                   (let ((base #xAA0003E0))
                     (let ((shifted-rn (* rn #x10000)))
                       (let ((encoded (+ base (+ shifted-rn rd))))
                         (word-to-bytes encoded)))))
                 (defun mini-codegen (ir)
                   (cond
                     ((has-tag? ir 'lit)
                      (arm64-movz #x0 (* (cadr ir) #x10)))
                     ((has-tag? ir 'add)
                      (append (mini-codegen (cadr ir))
                              (arm64-mov #x1 #x0)
                              (mini-codegen (caddr ir))
                              (arm64-add #x0 #x1 #x0)))
                     (t (list))))
                 (defun nth-word (code n)
                   ;; Get nth 4-byte word from code
                   (let ((start (* n #x4)))
                     (list (nth start code)
                           (nth (+ start #x1) code)
                           (nth (+ start #x2) code)
                           (nth (+ start #x3) code))))
                 ;; Generate and verify
                 (let ((code (mini-codegen (list 'add
                                                 (list 'lit #x5)
                                                 (list 'lit #x3)))))
                   ;; Word 0: MOVZ X0, #0x50 (5 << 4)
                   ;; Word 1: MOV X1, X0
                   ;; Word 2: MOVZ X0, #0x30 (3 << 4)
                   ;; Word 3: ADD X0, X1, X0
                   ;; Verify word 3 is ADD
                   (reconstruct-word (nth-word code #x3)))))))
  (format t "  Last instruction: 0x~X (expected 0x8B000020 = ADD X0, X1, X0)~%" result)
  (unless (= result #x8B000020)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

(format t "~%=== ALL STAGE 1C BOOTSTRAP TESTS PASSED ===~%")
(format t "~%Successfully demonstrated:~%")
(format t "  - Mini-codegen compiled by Habu~%")
(format t "  - Generated correct ARM64 MOVZ instructions~%")
(format t "  - Generated correct ARM64 ADD instructions~%")
(format t "  - End-to-end: IR -> ARM64 bytecode~%")
(format t "~%The Habu compiler can compile a working codegen!~%")

(sb-ext:quit :unix-status 0)
