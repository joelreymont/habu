;;;; Test GC functionality

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing GC Integration~%")
(format t "=====================~%~%")

;; Get heap stats
(let ((stats (funcall (find-symbol "HEAP-STATS" :habu-runtime))))
  (format t "Initial heap state:~%")
  (format t "  Size: ~:D bytes~%" (getf stats :size))
  (format t "  Allocated: ~:D bytes~%" (getf stats :allocated))
  (format t "  Free: ~:D bytes~%" (getf stats :free))
  (format t "  Objects: ~D~%~%" (getf stats :objects)))

;; Allocate some cons cells
(format t "Allocating 10 cons cells...~%")
(let ((cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime)))
  (dotimes (i 10)
    (funcall cons-fn (ash i 4) (ash (+ i 1) 4))))  ; Create cons cells

(let ((stats (funcall (find-symbol "HEAP-STATS" :habu-runtime))))
  (format t "After allocation:~%")
  (format t "  Allocated: ~:D bytes~%" (getf stats :allocated))
  (format t "  Objects: ~D~%~%" (getf stats :objects)))

;; Manually trigger GC with empty roots (should collect everything)
(format t "Running GC with empty roots...~%")
(let ((gc-fn (find-symbol "GC" :habu-runtime))
      (heap (symbol-value (find-symbol "*HEAP*" :habu-runtime))))
  (let ((result (funcall gc-fn heap nil)))
    (format t "GC Result:~%")
    (format t "  Freed: ~:D bytes~%" (getf result :freed-bytes))
    (format t "  Freed objects: ~D~%" (getf result :freed-objects))
    (format t "  GC time: ~,6F seconds~%" (getf result :gc-time))
    (format t "  GC count: ~D~%~%" (getf result :gc-count))))

(let ((stats (funcall (find-symbol "HEAP-STATS" :habu-runtime))))
  (format t "After GC:~%")
  (format t "  Allocated: ~:D bytes~%" (getf stats :allocated))
  (format t "  Objects: ~D~%~%" (getf stats :objects)))

;; Test with roots
(format t "Allocating with roots...~%")
(let ((cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime))
      (roots nil))
  ;; Create a chain of cons cells and keep root
  (let ((cell (funcall cons-fn (ash 1 4) (ash 2 4))))
    (push cell roots)
    (dotimes (i 5)
      (setf cell (funcall cons-fn (ash (+ i 3) 4) cell))
      (when (= i 4)  ; Keep the last one as root
        (push cell roots))))
  
  ;; Allocate some garbage
  (dotimes (i 10)
    (funcall cons-fn (ash 100 4) (ash 200 4)))
  
  (let ((stats (funcall (find-symbol "HEAP-STATS" :habu-runtime))))
    (format t "Before GC: ~D objects~%" (getf stats :objects)))
  
  ;; GC with roots
  (let ((gc-fn (find-symbol "GC" :habu-runtime))
        (heap (symbol-value (find-symbol "*HEAP*" :habu-runtime))))
    (let ((result (funcall gc-fn heap roots)))
      (format t "GC with roots freed ~D objects~%" (getf result :freed-objects))))
  
  (let ((stats (funcall (find-symbol "HEAP-STATS" :habu-runtime))))
    (format t "After GC: ~D objects (should keep ~D)~%~%" 
            (getf stats :objects)
            (length roots))))

(format t "✓ GC tests complete!~%")
(sb-ext:quit)
