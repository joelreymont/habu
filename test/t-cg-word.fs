\ t-cg-word.fs — compile real Forth stack-code bodies to native Mac executables
\ and check the results. The bridge from checked-Forth source to ARM64 machine
\ code. Slow (codesign+exec per case); run explicitly, not in all.fs:
\   gforth test/t-cg-word.fs -e bye
require tester.fs
require ../src/cg/walk.fs
\ NATIVE-EVAL ( body-addr body-u input -- exit-code )

T{ s" DUP *"          7 NATIVE-EVAL -> 49 }T   \ square
T{ s" DUP DUP * *"    3 NATIVE-EVAL -> 27 }T   \ 3 -> 3*3*3
T{ s" DUP +"         10 NATIVE-EVAL -> 20 }T   \ double
T{ s" 1+ 1+"          5 NATIVE-EVAL ->  7 }T   \ +2
T{ s" 1-"             9 NATIVE-EVAL ->  8 }T
T{ s" DUP DUP * SWAP DROP" 4 NATIVE-EVAL -> 16 }T   \ keep the square
T{ s" 7 *"            6 NATIVE-EVAL -> 42 }T   \ literal in body
T{ s" 3 + 2 *"       20 NATIVE-EVAL -> 46 }T   \ (20+3)*2
T{ s" DUP OVER + +"   8 NATIVE-EVAL -> 24 }T   \ 8 -> 8 8 8; + + -> 24

\ --- constant folding: all-literal arithmetic folds at compile time ---
T{ s" 3 4 +"          0 NATIVE-EVAL ->  7 }T   \ 3+4 folded
T{ s" 2 3 4 * +"      0 NATIVE-EVAL -> 14 }T   \ 2 + (3*4) folded
T{ s" 5 1+ 1+ 2*"     0 NATIVE-EVAL -> 14 }T   \ ((5+1+1)*2) folded
T{ s" 10 3 - 2 *"     9 NATIVE-EVAL -> 14 }T   \ (10-3)*2; runtime input 9 ignored
T{ s" 1 7 LSHIFT"     0 NATIVE-EVAL -> 128 }T  \ 1<<7 folded (exit is low byte)
\ const shift amount + runtime value -> immediate shift (LSL/LSR #k), not register
T{ s" 3 LSHIFT"       5 NATIVE-EVAL -> 40 }T   \ 5<<3 via lsl #3
T{ s" 1 RSHIFT"      40 NATIVE-EVAL -> 20 }T   \ 40>>1 via lsr #1
T{ s" DUP 2 LSHIFT +" 5 NATIVE-EVAL -> 25 }T   \ 5 + (5<<2) = 25 — fused ADD,…,lsl#2
T{ s" DUP 1 RSHIFT XOR" 6 NATIVE-EVAL -> 5 }T  \ 6 ^ (6>>1) = 6^3 = 5 — fused EOR,…,lsr#1
T{ s" DUP 3 LSHIFT XOR" 1 NATIVE-EVAL -> 9 }T  \ 1 ^ (1<<3) = 1^8 = 9 — fused EOR,…,lsl#3
T{ s" 5 + 3 *"        4 NATIVE-EVAL -> 27 }T   \ mixes runtime input with consts: (4+5)*3

\ --- register-allocated stack shuffles (ROT/-ROT/2DUP/2DROP/TUCK) ---
T{ s" 10 20 ROT - -"  5 NATIVE-EVAL -> 251 }T \ 5 10 20 -> ROT 10 20 5 -> (-5) low byte
T{ s" 7 TUCK - -"     5 NATIVE-EVAL ->  9 }T  \ 5 7 -> TUCK 7 5 7 -> 7-(5-7)=9
T{ s" 3 2DUP + + +"   4 NATIVE-EVAL -> 14 }T  \ 4+3+4+3 = 14
T{ s" 1 2 2DROP"      5 NATIVE-EVAL ->  5 }T  \ drop the two literals
T{ s" 10 20 -ROT"     5 NATIVE-EVAL -> 10 }T  \ 5 10 20 -> -ROT 20 5 10 -> TOS 10
\ --- const-operand immediate ADD/SUB (ADDI/SUBI #imm, no materialisation) ---
T{ s" 5 +"           10 NATIVE-EVAL -> 15 }T  \ x + 5 via ADDI #5
T{ s" 30 -"          50 NATIVE-EVAL -> 20 }T  \ x - 30 via SUBI #30

\ --- memory ops compile (LDR/STR emission; runtime round-trip in the corpus) ---
2variable CW-SRC
: CW-RUN ( -- )  CW-SRC 2@ 0 COMPILE-WORD ;
: COMPILES? ( a u -- f )  CW-SRC 2!  ['] CW-RUN catch 0= ;
T{ s" DUP @"   COMPILES? -> true }T
T{ s" SWAP !"  COMPILES? -> true }T
T{ s" DUP c@"  COMPILES? -> true }T
T{ s" SWAP +!" COMPILES? -> true }T

\ --- division: correct result, and ÷0 TRAPS (killed by signal) not silent 0 ---
T{ s" 3 /"   12 NATIVE-EVAL -> 4 }T            \ 12/3
T{ s" 5 MOD" 17 NATIVE-EVAL -> 2 }T            \ 17 mod 5
: DIVZ-WSTAT ( -- wstat )                      \ build `0 /`, run, return raw wait-status
   s" 0 /" 10 COMPILE-WORD  s" /tmp/habu-divz" 2dup EMIT-EXE
   cmd( [char] ' c+ cs+ [char] ' c+ )run ;
\ Div-by-zero traps (SIGTRAP); the in-binary crash handler (crash.fs) catches it
\ and exit(134)s, so the OS sees a clean exit 134, not a signal-kill.
T{ DIVZ-WSTAT WSTAT>RC -> 134 }T

\ --- logical-immediate AND/OR/XOR (const operand -> AND/ORR/EOR #imm) ---
T{ s" 255 AND"  4660 NATIVE-EVAL -> 52 }T      \ 0x1234 & 0xFF
T{ s" 240 AND"   171 NATIVE-EVAL -> 160 }T     \ 0xAB & 0xF0
T{ s" 8 OR"        5 NATIVE-EVAL -> 13 }T       \ 5 | 8
T{ s" 10 XOR"     12 NATIVE-EVAL -> 6 }T        \ 12 ^ 10  (10 not encodable -> register, still correct)

\ --- optimizer output locked by instruction count (regression guard) ---
: LIVE-IC ( -- n )  0 #IC @ 0 ?do i IC-OP IOP-DEAD <> if 1+ then loop ;
: BODY-IC ( a u -- n )  ICODE-RESET cf-reset WALK-BODY OPTIMIZE LIVE-IC ;
T{ s" DUP *"                 BODY-IC -> 5 }T   \ load,mul,store + framing (register-resident)
T{ s" DUP 13 LSHIFT XOR DUP 7 RSHIFT XOR DUP 17 LSHIFT XOR" BODY-IC -> 7 }T  \ fused EORs (LLVM)

\ --- register-pool exhaustion ABORTS (never silently miscompiles) ---
: DEEP ( -- )  s" DUP DUP DUP DUP DUP DUP DUP DUP DUP" 5 COMPILE-WORD ;
T{ ' DEEP catch 0<> -> true }T                 \ too deep for the 8-reg pool -> clean abort

\ --- locals {: a b :} (frame slots; survive control flow / spills) ---
T{ s" {: x :} x x *"            7 NATIVE-EVAL -> 49 }T
T{ s" {: x :} x x + x +"        5 NATIVE-EVAL -> 15 }T
T{ s" {: a :} 10 a -"           3 NATIVE-EVAL ->  7 }T
T{ s" {: x :} x 0< IF x NEGATE ELSE x THEN" -5 NATIVE-EVAL -> 5 }T   \ local read across IF/ELSE
T{ s" {: x :} x 0< IF x NEGATE ELSE x THEN"  6 NATIVE-EVAL -> 6 }T
T{ s" {: x :} 0 5 0 ?DO x + LOOP"  3 NATIVE-EVAL -> 15 }T            \ local read inside a loop (0 + 5×3)

\ --- AOT quotation/combinator inlining ([: … ;] EXECUTE / DIP, no execute) ---
T{ s" [: 1+ ;] EXECUTE"        5 NATIVE-EVAL ->  6 }T
T{ s" [: DUP * ;] EXECUTE"     7 NATIVE-EVAL -> 49 }T
T{ s" [: 2 * ;] EXECUTE 1+"    4 NATIVE-EVAL ->  9 }T               \ inlined quot + following op
T{ s" 10 [: 1+ ;] DIP DROP"    5 NATIVE-EVAL ->  6 }T               \ DIP runs under the top

\ --- bump heap (HERE/ALLOT/,/C,) + memory round-trips ---
T{ s" HERE 42 OVER ! @"            0 NATIVE-EVAL -> 42 }T
T{ s" HERE 10 OVER ! 5 OVER +! @"  0 NATIVE-EVAL -> 15 }T           \ +!
T{ s" HERE 7 OVER ! 8 ALLOT HERE 99 OVER ! DROP @"  0 NATIVE-EVAL -> 7 }T  \ ALLOT separates cells
\ u8 width correct by construction: c! truncates the low byte (= gforth truncate-at-store)
T{ s" HERE 200 OVER C! C@"         0 NATIVE-EVAL -> 200 }T
T{ s" HERE 300 OVER C! C@"         0 NATIVE-EVAL -> 44 }T           \ 300 & 0xFF
T{ s" HERE 511 OVER C! C@"         0 NATIVE-EVAL -> 255 }T          \ 511 & 0xFF

\ --- floating point (f64 bits on the data stack; FP ops via D-regs) ---
T{ s" 2.0 3.0 F* F>S"            0 NATIVE-EVAL ->  6 }T
T{ s" 2.5 3.5 F+ F>S"            0 NATIVE-EVAL ->  6 }T
T{ s" 10.0 3.0 F- F>S"           0 NATIVE-EVAL ->  7 }T
T{ s" 12.0 4.0 F/ F>S"           0 NATIVE-EVAL ->  3 }T
T{ s" 16.0 FSQRT F>S"            0 NATIVE-EVAL ->  4 }T
T{ s" 5.0 FNEGATE F>S NEGATE"    0 NATIVE-EVAL ->  5 }T   \ -(-5)=5
T{ s" 5.0 FNEGATE FABS F>S"      0 NATIVE-EVAL ->  5 }T
T{ s" 7 S>F 2.0 F* F>S"          0 NATIVE-EVAL -> 14 }T   \ int->float round-trip
T{ s" 2.0 3.0 F< NEGATE"         0 NATIVE-EVAL ->  1 }T   \ 2<3 true (flag 0/-1, NEGATE->1)
T{ s" 3.0 2.0 F< NEGATE"         0 NATIVE-EVAL ->  0 }T
T{ s" 3.0 2.0 F> NEGATE"         0 NATIVE-EVAL ->  1 }T
T{ s" 3.0 3.0 F= NEGATE"         0 NATIVE-EVAL ->  1 }T
T{ s" 5.0 FNEGATE F0< NEGATE"    0 NATIVE-EVAL ->  1 }T
T{ s" 0.0 F0= NEGATE"            0 NATIVE-EVAL ->  1 }T
T{ s" 1.5e1 F>S"                 0 NATIVE-EVAL -> 15 }T   \ exponent literal

\ --- FP register residency: chained ops stay in the D-file; floats survive
\ shuffles and control-flow spills (V-FREG <-> memory round-trips) ---
T{ s" 2.0 3.0 F+ 4.0 F* F>S"     0 NATIVE-EVAL -> 20 }T   \ (2+3)*4, chained D-resident
T{ s" 1.0 2.0 3.0 F+ F+ F>S"     0 NATIVE-EVAL ->  6 }T   \ deep chain
T{ s" 3.0 DUP F* F>S"            0 NATIVE-EVAL ->  9 }T   \ DUP copies an FP-resident value
T{ s" 2.0 3.0 SWAP F- F>S"       0 NATIVE-EVAL ->  1 }T   \ SWAP then 3-2
T{ s" 2.0 3.0 OVER F+ F+ F>S"    0 NATIVE-EVAL ->  7 }T   \ OVER: 2 + (3+2)
T{ s" 5.0 F>S 0 > IF 2.0 ELSE 3.0 THEN F>S"  0 NATIVE-EVAL -> 2 }T  \ float built after a spill
T{ s" 2.0 3.0 F* 0 5 0 ?DO DROP 2.0 LOOP F>S"  0 NATIVE-EVAL -> 2 }T \ float spilled across a loop

\ --- register-resident DO..LOOP: straight-line carry stays in registers across
\ the back-edge (no per-iteration memory traffic); reconciliation handles SWAP;
\ ?DO skip and the memory-path fallback (non-VS body token) stay correct ---
T{ s" 0 10 0 ?DO 1+ LOOP"                    0 NATIVE-EVAL ->  10 }T   \ acc in a register
T{ s" 0 100 0 ?DO 1+ LOOP"                   0 NATIVE-EVAL -> 100 }T
T{ s" 0 5 0 ?DO I + LOOP"                    0 NATIVE-EVAL ->  10 }T   \ 0+1+2+3+4
T{ s" 0 5 1 ?DO I + LOOP"                    0 NATIVE-EVAL ->  10 }T   \ ?DO start=1
T{ s" 1 5 0 ?DO 2 * LOOP"                    0 NATIVE-EVAL ->  32 }T   \ 2^5
T{ s" 0 0 5 0 ?DO 1+ SWAP 1+ SWAP LOOP +"    0 NATIVE-EVAL ->  10 }T   \ depth-2 carry + SWAP recon
T{ s" 7 3 3 ?DO 1+ LOOP"                     0 NATIVE-EVAL ->   7 }T   \ ?DO skip (3>=3)
T{ s" 9 0 0 ?DO 1+ LOOP"                     0 NATIVE-EVAL ->   9 }T   \ DO 0..0 skips body? (0>=0 for ?DO is skip; DO runs 0)
T{ s" 0 3 0 ?DO 5 0 ?DO 1+ LOOP LOOP"        0 NATIVE-EVAL ->  15 }T   \ nested loops -> both memory path; 3*5 increments
T{ s" 0 3 0 ?DO 0 5 0 ?DO 1+ LOOP LOOP"      0 NATIVE-EVAL ->   5 }T   \ inner resets acc each outer trip (matches gforth)
T{ s" 0 4 0 ?DO 1+ I 0> IF 1+ THEN LOOP"     0 NATIVE-EVAL ->   7 }T   \ inner IF -> memory-path fallback
T{ s" 5 0 ?DO 1+ LOOP"                       7 NATIVE-EVAL ->  12 }T   \ body reads acc from memory: register attempt fails (RL-FAIL), clean memory fallback
T{ s" 10 20 30 1 0 ?DO ROT LOOP"           0 NATIVE-EVAL ->  10 }T   \ 3-cycle parallel move (1 ROT)
T{ s" 10 20 30 3 0 ?DO ROT LOOP"           0 NATIVE-EVAL ->  30 }T   \ ROT^3 = identity
T{ s" 1 2 5 0 ?DO SWAP LOOP +"             0 NATIVE-EVAL ->   3 }T   \ swap-carry, odd trips
T{ s" 1 SWAP 0 ?DO DUP 13 LSHIFT XOR DUP 7 RSHIFT XOR DUP 17 LSHIFT XOR LOOP" 20 NATIVE-EVAL ->  92 }T  \ xorshift, 20 iters (cross-checked vs C)

\ --- in-binary crash handler: a faulting word is caught by the installed signal
\ handler, which dumps registers to stderr and exit(134) (not a silent signal
\ death). Stderr is redirected here so the suite stays quiet. ---
: CRASH-CODE ( a u -- code )
   10 COMPILE-WORD  s" /tmp/habu-crash" 2dup EMIT-EXE
   cmd(  [char] ' c+  cs+  [char] ' c+  s"  2>/dev/null" cs+  )run  WSTAT>RC ;
T{ s" 0 @"        CRASH-CODE -> 134 }T          \ load from NULL -> SIGSEGV -> handler
T{ s" 5"          CRASH-CODE ->   5 }T          \ non-crashing path unaffected
