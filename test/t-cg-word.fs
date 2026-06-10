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
   s" 0 /" 10 COMPILE-WORD  s" /tmp/caf-divz" 2dup EMIT-EXE
   cmd( [char] ' c+ cs+ [char] ' c+ )run ;
T{ DIVZ-WSTAT $7F and 0<> -> true }T           \ low 7 bits set = killed by a signal (trapped)

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
