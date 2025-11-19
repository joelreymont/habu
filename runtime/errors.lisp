;;;; Habu Runtime - Error Handling
;;;; Implements catch/throw for non-local exits

(in-package :habu-runtime)

(export '(runtime-catch
          runtime-throw))

;;; Catch/Throw Implementation (Phase 1: Bootstrap via SBCL)
;;;
;;; For Phase 1, we use SBCL's catch/throw mechanism directly.
;;; The catch tag is a Habu value (symbol or fixnum).
;;; The body is a Habu closure (function pointer).

(defun runtime-catch (tag-value body-fn-ptr)
  "Establish catch point and execute body function
   tag-value: Habu value (symbol or fixnum) identifying this catch
   body-fn-ptr: Habu closure to execute
   Returns: Habu value from body or thrown value"
  ;; Convert Habu tag to Lisp tag
  (let ((tag (cond
               ;; Check if it's a symbol (tag & 0xF == 0x5)
               ((= (logand tag-value #xF) +tag-symbol+)
                ;; Get symbol name for tag
                (let ((sym-name (symbol->lisp-name tag-value)))
                  (intern sym-name (find-package :habu-runtime))))

               ;; Fixnum tag (tag & 0xF == 0)
               ((= (logand tag-value #xF) 0)
                ;; Use fixnum value directly
                (ash tag-value -4))

               (t
                (error "Catch tag must be symbol or fixnum, got ~X" tag-value)))))

    ;; Establish catch point using SBCL's catch
    ;; Call body function (0-argument function)
    (catch tag
      (let ((fn (get-function-from-pointer body-fn-ptr)))
        (funcall fn)))))

(defun runtime-throw (tag-value throw-value)
  "Throw to matching catch point (never returns)
   tag-value: Habu value (symbol or fixnum) identifying catch point
   throw-value: Habu value to return from catch
   Never returns - transfers control to catch"
  ;; Convert Habu tag to Lisp tag
  (let ((tag (cond
               ;; Symbol tag
               ((= (logand tag-value #xF) +tag-symbol+)
                (let ((sym-name (symbol->lisp-name tag-value)))
                  (intern sym-name (find-package :habu-runtime))))

               ;; Fixnum tag
               ((= (logand tag-value #xF) 0)
                (ash tag-value -4))

               (t
                (error "Throw tag must be symbol or fixnum, got ~X" tag-value)))))

    ;; Throw to matching catch (never returns)
    (throw tag throw-value)))

;;; Helper: Get symbol name from Habu symbol pointer
(defun symbol->lisp-name (sym-ptr)
  "Extract symbol name from Habu symbol pointer"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))

  ;; Read symbol name hash field (offset 8)
  (let ((name-hash (sb-sys:sap-ref-64 (sb-sys:int-sap (logand sym-ptr (lognot #xF))) 8)))
    ;; For now, use hash as name (Phase 1 limitation)
    ;; In Phase 2, symbols will have actual string names
    (format nil "SYM-~X" (ash name-hash -4))))

;;; Helper: Get function from closure pointer
(defun get-function-from-pointer (fn-ptr)
  "Get executable function from Habu closure pointer"
  (unless (= (logand fn-ptr #xF) +tag-closure+)
    (error "Not a closure: ~X" fn-ptr))

  ;; Read function pointer field (offset 8)
  (let ((code-ptr (sb-sys:sap-ref-64 (sb-sys:int-sap (logand fn-ptr (lognot #xF))) 8)))
    ;; Create alien-function wrapper for 0-argument function
    (lambda ()
      (sb-alien:alien-funcall
       (sb-alien:cast (sb-sys:int-sap code-ptr)
                      (sb-alien:* (sb-alien:function sb-alien:unsigned-long)))))))
