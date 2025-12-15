;;; types-test.lisp - Test the habu0 type system
;;;
;;; Run with: sbcl --load native/types.lisp --load native/types-test.lisp

;;; Define a simple IR type
(habu-deftype ir-node :prefix ir
  (lit value)
  (var offset)
  (add left right)
  (if cond then else))

;;; Test constructors
(defun test-constructors ()
  (let ((lit-node (ir-lit 42))
        (var-node (ir-var 0))
        (add-node (ir-add (ir-lit 1) (ir-lit 2))))
    ;; Print results
    (print "Testing constructors:")
    (print lit-node)   ; Should be (:IR-LIT 42)
    (print var-node)   ; Should be (:IR-VAR 0)
    (print add-node))) ; Should be (:IR-ADD (:IR-LIT 1) (:IR-LIT 2))

;;; Test predicates
(defun test-predicates ()
  (let ((lit-node (ir-lit 42))
        (var-node (ir-var 0)))
    (print "Testing predicates:")
    (print (ir-lit-p lit-node))    ; Should be T
    (print (ir-var-p lit-node))    ; Should be NIL
    (print (ir-var-p var-node))    ; Should be T
    (print (ir-node-p lit-node)))) ; Should be T

;;; Test accessors
(defun test-accessors ()
  (let ((lit-node (ir-lit 42))
        (add-node (ir-add (ir-lit 1) (ir-lit 2))))
    (print "Testing accessors:")
    (print (ir-lit-value lit-node))      ; Should be 42
    (print (ir-add-left add-node))       ; Should be (:IR-LIT 1)
    (print (ir-add-right add-node))))    ; Should be (:IR-LIT 2)

;;; Test match
(defun eval-ir (node)
  "Evaluate an IR node"
  (habu-match ir-node node
    (lit (value)
         value)
    (var (offset)
         (error "Variables not supported in this test"))
    (add (left right)
         (+ (eval-ir left) (eval-ir right)))
    (if (cond then else)
        (if (eval-ir cond)
            (eval-ir then)
            (eval-ir else)))))

(defun test-match ()
  (print "Testing match:")
  (print (eval-ir (ir-lit 42)))                           ; Should be 42
  (print (eval-ir (ir-add (ir-lit 10) (ir-lit 32))))      ; Should be 42
  (print (eval-ir (ir-if (ir-lit 1)
                        (ir-lit 100)
                        (ir-lit 200)))))                  ; Should be 100

;;; Run all tests
(defun run-tests ()
  (test-constructors)
  (test-predicates)
  (test-accessors)
  (test-match)
  (print "All tests complete!"))

;;; For interactive testing
; (run-tests)
