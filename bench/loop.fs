\ loop.fs — emit a caf-compiled xorshift loop binary (its ACTUAL codegen: register
\ allocator + shifted-EOR fusion + register loop counter). Build:
\   gforth bench/loop.fs -e "100000000 BUILD-LOOP bye"   -> /tmp/caf-loop
\ then time /tmp/caf-loop vs `clang -O3 bench/loop.c && time ./a.out`.
require ../src/cg/walk.fs
: BUILD-LOOP ( n -- )
   s" 1 SWAP 0 ?DO DUP 13 LSHIFT XOR DUP 7 RSHIFT XOR DUP 17 LSHIFT XOR LOOP"
   rot COMPILE-WORD  s" /tmp/caf-loop" EMIT-EXE ;
