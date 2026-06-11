\ t-disasm-core.fs — the ARM64 DECODE core (disasm-core.fs) is CERTIFIED by habu's own
\ checker (all CHECK-CODE = 0) AND decodes instruction fields correctly (inverse of the
\ encoders). The disassembler's math, in checked Forth. Run: gforth test/t-disasm-core.fs -e bye
require ../src/habu.fs
require tester.fs
variable WORST  0 WORST !
: NOTE ( -- )  CHECK-CODE @ ?dup if WORST ! then ;
: D-FLD ( a b c -- d )  >r  rshift  r> 1 swap lshift 1 - and ;  NOTE
: D-RD  ( a -- b )  31 and ;  NOTE
: D-RN  ( a -- b )  5 rshift 31 and ;  NOTE
: D-RM  ( a -- b )  16 rshift 31 and ;  NOTE
: D-SX  ( a b -- c )  1 - 1 swap lshift  tuck xor swap - ;  NOTE
T{ WORST @ -> 0 }T                               \ habu certified the decode core
\ correctness: add x1,x2,x3 = 2332229697 -> rd=1 rn=2 rm=3
T{ 2332229697 D-RD -> 1 }T
T{ 2332229697 D-RN -> 2 }T
T{ 2332229697 D-RM -> 3 }T
T{ 5 3 D-FLD -> 0 }T                             \ bits[3..5] of 5 (0b101) = 0
T{ 7 3 D-SX -> -1 }T                             \ 0b111 as signed 3-bit = -1
T{ 3 3 D-SX -> 3 }T                              \ 0b011 as signed 3-bit = 3
