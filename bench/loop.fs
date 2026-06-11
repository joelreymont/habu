\ loop.fs — emit a habu-compiled xorshift loop binary (its ACTUAL codegen: register
\ allocator + shifted-EOR fusion + register loop counter + register-RESIDENT carry,
\ so the loop body is pure registers with zero per-iteration memory traffic). It
\ ties clang -O3 on this benchmark: ~0.20s for 1e8 iters, exit 221. Build:
\   gforth bench/loop.fs -e "100000000 BUILD-LOOP bye"   -> /tmp/habu-loop
\ then time /tmp/habu-loop vs `clang -O3 bench/loop.c && time ./a.out`.
require ../src/cg/walk.fs
: BUILD-LOOP ( n -- )
   s" 1 SWAP 0 ?DO DUP 13 LSHIFT XOR DUP 7 RSHIFT XOR DUP 17 LSHIFT XOR LOOP"
   rot COMPILE-WORD  s" /tmp/habu-loop" EMIT-EXE ;
