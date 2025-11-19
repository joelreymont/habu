;;;; runtime/loops.lisp - Loop iteration support for Phase 1
;;;; Uses closures to implement dotimes and dolist

(in-package :habu-runtime)

;;; Phase 1 implementation: loops via runtime functions + closures
;;; This avoids the named-let issue and works with existing closure support

(defun runtime-dotimes (count-value body-fn-ptr result-value)
  "Execute body function COUNT times with loop variable, return RESULT.
   count-value: Habu fixnum (tagged)
   body-fn-ptr: Habu closure pointer (1-arg function taking loop index)
   result-value: Habu value to return after loop"
  ;; Convert count from tagged fixnum
  (let ((count (ash count-value -4)))
    (unless (and (integerp count) (>= count 0))
      (error "dotimes count must be non-negative fixnum, got ~X" count-value))

    ;; Get the body function
    (let ((body-fn (get-function-from-pointer body-fn-ptr)))
      ;; Execute loop
      (dotimes (i count)
        ;; Call body with tagged fixnum index
        (funcall body-fn (ash i 4))))

    ;; Return result value
    result-value))

(defun runtime-dolist (list-value body-fn-ptr result-value)
  "Iterate over list, executing body function for each element, return RESULT.
   list-value: Habu list (cons cells or nil)
   body-fn-ptr: Habu closure pointer (1-arg function taking element)
   result-value: Habu value to return after loop"
  ;; Get the body function
  (let ((body-fn (get-function-from-pointer body-fn-ptr)))
    ;; Iterate over list
    (let ((current list-value))
      (loop
        (when (= current 0)  ; nil is 0
          (return))

        ;; Verify it's a cons cell (tag 0x1 = TAG_CONS)
        (unless (= (logand current #xF) #x1)
          (error "dolist list must be proper list, got ~X at position" current))

        ;; Get car and cdr
        (let* ((cons-addr (- current #x1))  ; Remove cons tag
               (car-val (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 0))
               (cdr-val (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 8)))

          ;; Call body with element
          (funcall body-fn car-val)

          ;; Move to next
          (setf current cdr-val)))))

  ;; Return result value
  result-value)

(defun get-function-from-pointer (fn-ptr)
  "Convert Habu function pointer to callable Lisp function.
   For Phase 1, we use SBCL's alien-funcall to invoke raw machine code."
  (cond
    ;; Closure: tag 0x7
    ((= (logand fn-ptr #xF) #x7)
     (let* ((closure-addr (- fn-ptr #x7))
            (code-ptr (sb-sys:sap-ref-64 (sb-sys:int-sap closure-addr) 0)))
       ;; Create a callable that invokes the closure code
       ;; The closure takes one argument (the loop variable)
       (lambda (arg)
         ;; Call the machine code with the argument in RDI/X0
         #+sbcl
         (sb-alien:alien-funcall
          (sb-alien:sap-alien (sb-sys:int-sap code-ptr)
                              (* (sb-alien:function sb-alien:unsigned-long
                                                    sb-alien:unsigned-long)))
          arg))))

    ;; Compiled function pointer (raw code address)
    ((evenp fn-ptr)
     ;; Create a callable for raw machine code
     (lambda (arg)
       #+sbcl
       (sb-alien:alien-funcall
        (sb-alien:sap-alien (sb-sys:int-sap fn-ptr)
                            (* (sb-alien:function sb-alien:unsigned-long
                                                  sb-alien:unsigned-long)))
        arg)))

    (t
     (error "Invalid function pointer: ~X" fn-ptr))))
