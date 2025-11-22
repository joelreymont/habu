#!/bin/bash
# Debug script for let binding issues

echo "=== Compiling simple let expression ==="
sbcl --script compile-and-save.lisp "(let ((x 10)) x)"

echo ""
echo "=== Disassembling generated code ==="
./disasm-bytecode.sh output.bin

echo ""
echo "=== Running with lldb to catch crash ==="
echo "run output.bin" | lldb -b ./run-bytecode