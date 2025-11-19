;;;; Stress test GC with automatic triggering

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%GC Stress Test~%")
(format t "==============~%~%")

(let ((cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime))
      (heap (symbol-value (find-symbol "*HEAP*" :habu-runtime)))
      (register-root (find-symbol "REGISTER-GC-ROOT" :habu-runtime))
      (heap-stats-fn (find-symbol "HEAP-STATS" :habu-runtime)))
  
  (format t "Initial heap: ~:D bytes~%~%" 
          (getf (funcall heap-stats-fn) :size))
  
  ;; Create a root list that we'll keep
  (format t "Creating persistent list: (1 2 3 4 5)~%")
  (let ((persistent-list 0))  ; Start with nil
    ;; Build list from right to left
    (loop for i from 5 downto 1 do
      (setf persistent-list 
            (funcall cons-fn (ash i 4) persistent-list)))
    
    ;; Register the head of the list as a GC root
    (funcall register-root persistent-list)
    (format t "Registered root: ~X~%~%" persistent-list)
    
    ;; Allocate LOTS of garbage to trigger GC
    (format t "Allocating garbage until GC triggers...~%")
    (let ((initial-gc-count (getf (funcall heap-stats-fn) :gc-count)))
      (dotimes (i 50000)
        (funcall cons-fn (ash i 4) (ash (+ i 1) 4))
        (when (and (zerop (mod i 10000))
                   (> i 0))
          (let ((stats (funcall heap-stats-fn)))
            (format t "  ~:D allocations: ~D objects, ~D GC cycles~%" 
                    i
                    (getf stats :objects)
                    (getf stats :gc-count)))))
      
      (let ((stats (funcall heap-stats-fn)))
        (format t "~%Final state:~%")
        (format t "  Objects: ~D~%" (getf stats :objects))
        (format t "  Allocated: ~:D bytes~%" (getf stats :allocated))
        (format t "  GC cycles: ~D~%" (getf stats :gc-count))
        (format t "  Total GC time: ~,6F seconds~%~%" (getf stats :gc-time))
        
        (when (> (getf stats :gc-count) initial-gc-count)
          (format t "✓ GC was triggered automatically!~%"))
        
        (when (< (getf stats :objects) 100)
          (format t "✓ Most garbage was collected!~%"))
        
        (format t "~%Test complete!~%")))))

(sb-ext:quit)
