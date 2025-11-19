(load "compiler.lisp")
(in-package :habu-compiler)
(initialize-runtime-integration)

(let ((cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime))
      (register-root (find-symbol "REGISTER-GC-ROOT" :habu-runtime))
      (heap-stats-fn (find-symbol "HEAP-STATS" :habu-runtime)))
  
  ;; Create root
  (let ((root (funcall cons-fn 16 32)))
    (funcall register-root root)
    (format t "Root: ~X~%" root)
    
    ;; Allocate 1000 cells
    (format t "Allocating 1000 cells...~%")
    (dotimes (i 1000)
      (funcall cons-fn (ash i 4) (ash (1+ i) 4)))
    
    (let ((stats (funcall heap-stats-fn)))
      (format t "Objects: ~D, GC: ~D~%" 
              (getf stats :objects)
              (getf stats :gc-count)))))

(sb-ext:quit)
