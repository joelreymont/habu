;;; Simple test for let bindings - to be compiled and tested standalone
;;; Usage: Generate bytes from this, then execute in C test

;;; Test 1: (let ((x 5)) x) -> 5
;;; IR: (let x (lit 5) (var 0))
;;; Code:
;;;   1. movz x0, #80 (5 << 4) - evaluate value
;;;   2. str x0, [sp, #-16]! - save on stack
;;;   3. ldr x0, [sp] - load from stack (var 0)
;;;   4. add sp, sp, #16 - restore stack

;;; Test 2: (let ((x 5)) (+ x 3)) -> 8
;;; IR: (let x (lit 5) (call + (var 0) (lit 3)))
;;; Code:
;;;   1. movz x0, #80 - evaluate value (5)
;;;   2. str x0, [sp, #-16]! - save on stack
;;;   3. ldr x0, [sp] - load x
;;;   4. str x0, [sp, #-16]! - save x for addition
;;;   5. movz x0, #48 - load 3
;;;   6. mov x1, x0
;;;   7. ldr x0, [sp], #16 - load saved x
;;;   8. add x0, x0, x1 - add
;;;   9. add sp, sp, #16 - restore stack from let

(print "Let binding test expressions")
