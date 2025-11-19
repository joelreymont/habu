;;;; Test automatic GC triggering

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Automatic GC~%")
(format t "===================~%~%")

(let ((cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime))
      (register-root (find-symbol "REGISTER-GC-ROOT" :habu-runtime))
      (heap-stats-fn (find-symbol "HEAP-STATS" :habu-runtime))
      (heap (symbol-value (find-symbol "*HEAP*" :habu-runtime))))
  
  ;; Create a persistent root
  (let ((root (funcall cons-fn (ash 42 4) (ash 43 4))))
    (funcall register-root root)
    (format t "Created root: ~X~%" root)
    
    (let ((stats (funcall heap-stats-fn)))
      (format t "Heap size: ~:D bytes~%~%" (getf stats :size)))
    
    ;; Allocate many cons cells (each is 32 bytes with header)
    ;; With 1MB heap, we can fit ~32K cons cells
    ;; Let's allocate 40K to force GC
    (format t "Allocating 40,000 cons cells (will force GC)...~%")
    (dotimes (i 40000)
      (funcall cons-fn (ash i 4) (ash (1+ i) 4))
      (when (zerop (mod i 5000))
        (let ((stats (funcall heap-stats-fn)))
          (format t "  ~5D: ~D objects, GC count=~D~%" 
                  i
                  (getf stats :objects)
                  (getf stats :gc-count)))))
    
    (let ((stats (funcall heap-stats-fn)))
      (format t "~%Final:~%")
      (format t "  Objects: ~D~%" (getf stats :objects))
      (format t "  GC cycles: ~D~%" (getf stats :gc-count))
      
      (if (> (getf stats :gc-count) 0)
          (format t "~%✓ GC was triggered automatically!~%")
          (format t "~%✗ GC was NOT triggered (unexpected)~%")))))

(sb-ext:quit)
