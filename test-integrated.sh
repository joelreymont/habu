#!/bin/bash
# Integrated test: compile with run-bytecode's runtime addresses

set -e

EXPR="${1:-(cons 42 99)}"
echo "Testing: $EXPR"
echo

# Step 1: Get runtime addresses from run-bytecode's address space
echo "Step 1: Getting runtime addresses from run-bytecode process..."
cat > /tmp/get-addrs.c << 'EOF'
#include <stdio.h>
#include <stdint.h>
#include "runtime/habu.h"

extern void* habu_cons(void*, void*);
extern void* habu_car(void*);
extern void* habu_cdr(void*);

int main() {
    habu_init(1024*1024);
    printf("HABU_CONS_ADDR=%p\n", habu_cons);
    printf("HABU_CAR_ADDR=%p\n", habu_car);
    printf("HABU_CDR_ADDR=%p\n", habu_cdr);
    return 0;
}
EOF

gcc -o /tmp/get-addrs /tmp/get-addrs.c runtime/*.o -lm -I.
ADDRS=$(/tmp/get-addrs)
echo "$ADDRS"
echo

# Step 2: Create Lisp script that uses these addresses
echo "Step 2: Compiling with runtime addresses..."
cat > /tmp/compile-with-addrs.lisp << 'LISP'
(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(defun parse-runtime-addr-line (line)
  (when (and (> (length line) 0) (position #\= line))
    (let* ((equals-pos (position #\= line))
           (name-str (subseq line 0 equals-pos))
           (addr-str (subseq line (+ equals-pos 1)))
           (symbol-name (intern (substitute #\_ #\- name-str) :habu-sbcl-codegen)))
      (cons symbol-name
            (parse-integer addr-str :start (if (char= (char addr-str 0) #\0) 2 0)
                          :radix 16 :junk-allowed t)))))

(defun get-runtime-addrs (addr-string)
  (let ((lines (loop for start = 0 then (1+ pos)
                     for pos = (position #\Newline addr-string :start start)
                     collect (subseq addr-string start pos)
                     while pos)))
    (mapcan (lambda (line)
              (let ((parsed (parse-runtime-addr-line line)))
                (when parsed (list parsed))))
            lines)))

(defun write-bytecode-to-file (code-list filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (dolist (byte code-list)
      (write-byte byte out))))

(let* ((addr-string (sb-ext:posix-getenv "RUNTIME_ADDRS"))
       (runtime-addrs (get-runtime-addrs addr-string))
       (expr (read-from-string (sb-ext:posix-getenv "EXPR")))
       (code (habu-sbcl-codegen:compile-to-arm64-with-runtime expr runtime-addrs)))
  (format t "Expression: ~S~%" expr)
  (format t "Code size: ~D bytes~%" (length code))
  (write-bytecode-to-file code "/tmp/test.bin")
  (format t "Wrote bytecode to /tmp/test.bin~%"))
LISP

RUNTIME_ADDRS="$ADDRS" EXPR="$EXPR" sbcl --script /tmp/compile-with-addrs.lisp
echo

# Step 3: Execute with run-bytecode
echo "Step 3: Executing bytecode..."
./run-bytecode /tmp/test.bin
