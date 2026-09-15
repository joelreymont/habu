\ The C6000 EABI helpers TI's reused libraries need, emitted as C66x instruction
\ words from the constructors in src/arch/tic6x/asm.f: the integer division
\ family within SPRAB89's Table 8-9 register sets, and memcpy/memset under the
\ standard convention. One instruction per execute packet with explicit delay
\ slots; correctness first, scheduling later. Radar's docs/c6x-eabi-helpers.md
\ records the contracts and decisions.
require lib/errors.f
require src/arch/tic6x/asm.f

package C6XEABI
public

E-C6XEABI-OPERAND constant E-OPERAND
E-C6XEABI-CAPACITY constant E-CAPACITY
9 constant HELPER-COUNT

private
using C6XASM

1024 constant WORDS-MAX
16 constant FIXUP-MAX
create WORDS WORDS-MAX cells allot
create FIXUPS FIXUP-MAX 3 * cells allot     \ word index, guard register code or -1, inverted
create PERMITTED HELPER-COUNT cells allot   \ register bit masks: A0..A31 bits 0..31, B0..B31 bits 32..63
variable LEN
variable FIXUP-COUNT


: A ( n -- gpr ) A-REG ;
: B ( n -- gpr ) B-REG ;


\ ---- the program builder -------------------------------------------------------------

: EMIT ( instruction -- )
   LEN @ WORDS-MAX >= if E-CAPACITY throw then
   INSTRUCTION>N WORDS LEN @ cells + ! 1 LEN +! ;

: HERE ( -- n ) LEN @ ;

\ Branch displacements count from the fetch packet holding the branch; every
\ helper therefore starts on a 32-byte boundary.
: DISPLACEMENT ( n n -- branch-offset ) {: from:n target:n :}
   target 4 * from 4 * $FFFFFFE0 and - >BRANCH-OFFSET ;

\ Guards for branches: a register code or -1, and whether the zero case branches.
: NEVER ( -- n n ) -1 0 ;
: WHEN ( gpr -- n n ) GPR>N 0 ;
: UNLESS ( gpr -- n n ) GPR>N 1 ;

: GUARDED ( instruction n n -- instruction ) {: opcode:instruction guard:n inverted:n :}
   guard 0 < if opcode exit then
   inverted 0 <> if opcode guard >GPR WHEN-ZERO else opcode guard >GPR WHEN-NONZERO then ;

\ A branch back to an earlier word, with its five delay slots.
: BACK ( n n n -- ) {: target:n guard:n inverted:n :}
   0 >SIDE HERE target DISPLACEMENT ENC-B-REL guard inverted GUARDED EMIT 5 ENC-NOP EMIT ;

\ A branch forward to a word not yet emitted; RESOLVE aims it at HERE.
: FORWARD ( n n -- n ) {: guard:n inverted:n :}
   FIXUP-COUNT @ FIXUP-MAX >= if E-CAPACITY throw then
   FIXUP-COUNT @ 3 * cells FIXUPS + {: row:ptr :}
   HERE row ! guard row 1 cells + ! inverted row 2 cells + !
   1 ENC-NOP EMIT 5 ENC-NOP EMIT
   FIXUP-COUNT @ 1 FIXUP-COUNT +! ;

: RESOLVE ( n -- ) {: ref:n :}
   ref 3 * cells FIXUPS + {: row:ptr :}
   0 >SIDE row @ HERE DISPLACEMENT ENC-B-REL row 1 cells + @ row 2 cells + @ GUARDED
   INSTRUCTION>N WORDS row @ cells + ! ;

: RETURN ( -- ) 3 B ENC-B-REG EMIT 5 ENC-NOP EMIT ;


\ ---- unsigned division ------------------------------------------------------------------

\ Divides n by d: the quotient lands in q, the remainder stays in n. s (on d's
\ bank) becomes the aligned divisor, t (n's bank) and u (d's bank) are scratch,
\ g guards compares (A0..A2 on n's bank) and c counts iterations (B0..B2 on
\ d's bank). A zero divisor gives a quotient of $FFFFFFFF and leaves n.
: DIVIDE-U ( gpr gpr gpr gpr gpr gpr gpr gpr -- ) {: n:gpr d:gpr q:gpr s:gpr t:gpr u:gpr g:gpr c:gpr :}
   q 0 ENC-MVK EMIT
   t 0 ENC-MVK EMIT
   g t d ENC-CMPEQ-L EMIT
   q -1 ENC-MVK g WHEN-NONZERO EMIT
   g WHEN FORWARD {: zero-ref:n :}
   u d ALWAYS ENC-NORM-L EMIT                       \ leading zeros of the divisor
   c d 0 ALWAYS ENC-CMPGT-I5 EMIT                   \ unless its top bit is set
   u u 1 ENC-ADD-I5 EMIT
   u 0 ENC-MVK c WHEN-NONZERO EMIT
   t n ALWAYS ENC-NORM-L EMIT                       \ leading zeros of the dividend
   g n 0 ALWAYS ENC-CMPGT-I5 EMIT
   t t 1 ENC-ADD-I5 EMIT
   t 0 ENC-MVK g WHEN-NONZERO EMIT
   u u t ENC-SUB-L EMIT                             \ the shift aligning the divisor's top bit
   g u 0 ALWAYS ENC-CMPGT-I5 EMIT                   \ negative: the dividend is smaller
   g WHEN FORWARD {: small-ref:n :}
   s d u ALWAYS ENC-SHL-S EMIT
   c u 1 ENC-ADD-I5 EMIT
   HERE {: loop:n :}
   g n s ALWAYS ENC-CMPLTU-L EMIT
   q q 1 ALWAYS ENC-SHL-U5 EMIT
   n n s ENC-SUB-L g WHEN-ZERO EMIT
   q q 1 ENC-ADD-I5 g WHEN-ZERO EMIT
   s s 1 ALWAYS ENC-SHRU-U5 EMIT
   c c -1 ENC-ADD-I5 EMIT
   loop c WHEN BACK
   small-ref RESOLVE zero-ref RESOLVE ;


\ Negates r in place when guard is nonzero, through a zero in scratch.
: NEGATE-WHEN ( gpr gpr gpr -- ) {: r:gpr scratch:gpr guard:gpr :}
   scratch 0 ENC-MVK EMIT
   r scratch r ENC-SUB-L guard WHEN-NONZERO EMIT ;


\ ---- the helpers ---------------------------------------------------------------------

: DIVU ( -- )
   4 A 4 B 6 A 4 B 1 A 1 B 0 A 0 B DIVIDE-U
   4 A 6 A ENC-MV-L EMIT RETURN ;

: REMU ( -- )
   4 A 4 B 5 A 4 B 7 A 1 B 1 A 0 B DIVIDE-U
   RETURN ;

: DIVREMU ( -- )
   4 A 4 B 6 A 4 B 1 A 1 B 0 A 0 B DIVIDE-U
   5 A 4 A ENC-MV-L EMIT 4 A 6 A ENC-MV-L EMIT RETURN ;

: MAGNITUDES ( -- )
   4 A 4 A ALWAYS ENC-ABS-L EMIT 4 B 4 B ALWAYS ENC-ABS-L EMIT ;

: DIVI ( -- )
   5 B 4 B 4 A ENC-XOR-L EMIT                       \ the quotient's sign
   MAGNITUDES
   4 A 4 B 6 A 4 B 1 A 1 B 0 A 0 B DIVIDE-U
   2 B 5 B 0 ALWAYS ENC-CMPGT-I5 EMIT
   6 A 1 A 2 B NEGATE-WHEN
   4 A 6 A ENC-MV-L EMIT RETURN ;

: REMI ( -- )
   2 B 4 A 0 ALWAYS ENC-CMPGT-I5 EMIT               \ the remainder takes the dividend's sign
   MAGNITUDES
   4 A 4 B 6 A 4 B 2 A 1 B 1 A 0 B DIVIDE-U
   4 A 1 A 2 B NEGATE-WHEN
   RETURN ;

: DIVREMI ( -- )
   31 B 4 B 4 A ENC-XOR-L EMIT
   2 B 4 A 0 ALWAYS ENC-CMPGT-I5 EMIT
   MAGNITUDES
   4 A 4 B 6 A 4 B 2 A 1 B 1 A 0 B DIVIDE-U
   4 A 1 A 2 B NEGATE-WHEN
   2 B 31 B 0 ALWAYS ENC-CMPGT-I5 EMIT
   6 A 1 A 2 B NEGATE-WHEN
   5 A 4 A ENC-MV-L EMIT 4 A 6 A ENC-MV-L EMIT RETURN ;

\ memcpy(dst A4, src B4, n A6) returns dst; a byte loop.
: MEMCPY ( -- )
   5 A 4 A ENC-MV-L EMIT 1 A 6 A ENC-MV-L EMIT
   1 A UNLESS FORWARD {: done:n :}
   HERE {: loop:n :}
   7 A 4 B 1 >BYTE-OFFSET ALWAYS ENC-LDB++ EMIT
   4 ENC-NOP EMIT
   7 A 5 A 1 >BYTE-OFFSET ALWAYS ENC-STB++ EMIT
   1 A 1 A -1 ENC-ADD-I5 EMIT
   loop 1 A WHEN BACK
   done RESOLVE RETURN ;

\ memset(dst A4, c B4, n A6) returns dst.
: MEMSET ( -- )
   5 A 4 A ENC-MV-L EMIT 1 A 6 A ENC-MV-L EMIT
   1 A UNLESS FORWARD {: done:n :}
   HERE {: loop:n :}
   4 B 5 A 1 >BYTE-OFFSET ALWAYS ENC-STB++ EMIT
   1 A 1 A -1 ENC-ADD-I5 EMIT
   loop 1 A WHEN BACK
   done RESOLVE RETURN ;


\ ---- float32 division: bit-exact IEEE 754, round to nearest even -------------------

\ divf(x A4, y B4) under the standard convention. Significands divide by a
\ restoring loop into 26 bits (24 plus guard and round) with the remainder as
\ sticky; denormal results shift right first; NaNs return the first NaN operand
\ quieted, invalid operations the default NaN $7FC00000.
: DIVF ( -- )
   5 A 4 A 1 24 ALWAYS ENC-EXTU-S EMIT                  \ A5 = exponent of x
   5 B 4 B 1 24 ALWAYS ENC-EXTU-S EMIT                  \ B5 = exponent of y
   6 A 4 A 9 9 ALWAYS ENC-EXTU-S EMIT                   \ A6 = fraction of x
   6 B 4 B 9 9 ALWAYS ENC-EXTU-S EMIT                   \ B6 = fraction of y
   7 A 4 A 4 B ENC-XOR-L EMIT                           \ A7 = the result's sign bit
   7 A 7 A 0 30 ALWAYS ENC-CLR-S EMIT
   17 A 255 ENC-MVK EMIT
   0 A 17 A 5 A ENC-CMPEQ-L EMIT                        \ A0 = x has the top exponent
   1 A 17 A 5 B ENC-CMPEQ-L EMIT                        \ A1 = y has the top exponent
   2 A 6 A 0 ALWAYS ENC-CMPLTU-U4 EMIT
   2 A 2 A 0 A ENC-AND-L EMIT                           \ x is NaN
   2 A WHEN FORWARD {: x-nan:n :}
   2 B 6 B 0 ALWAYS ENC-CMPLTU-U4 EMIT
   2 B 2 B 1 A ENC-AND-L EMIT                           \ y is NaN
   2 B WHEN FORWARD {: y-nan:n :}
   2 A 0 A 1 A ENC-AND-L EMIT                           \ both infinite
   2 A WHEN FORWARD {: invalid:n :}
   0 A WHEN FORWARD {: infinite:n :}                    \ x infinite
   1 A WHEN FORWARD {: zero:n :}                        \ y infinite
   0 B 5 B 1 ALWAYS ENC-CMPGTU-U4 EMIT                  \ B0 = y has a zero exponent
   1 B 6 B 1 ALWAYS ENC-CMPGTU-U4 EMIT
   2 B 0 B 1 B ENC-AND-L EMIT                           \ y is zero
   0 A 5 A 1 ALWAYS ENC-CMPGTU-U4 EMIT                  \ A0 = x has a zero exponent
   1 A 6 A 1 ALWAYS ENC-CMPGTU-U4 EMIT
   2 A 0 A 1 A ENC-AND-L EMIT                           \ x is zero
   1 B 2 B 2 A ENC-AND-L EMIT
   1 B WHEN FORWARD {: invalid2:n :}                    \ 0 / 0
   2 B WHEN FORWARD {: infinite2:n :}                   \ x / 0
   2 A WHEN FORWARD {: zero2:n :}                       \ 0 / y
   17 A 6 A ALWAYS ENC-NORM-L EMIT                      \ normalise a denormal x
   17 A 17 A -7 ENC-ADD-I5 EMIT
   6 A 6 A 17 A 0 A IF-NONZERO ENC-SHL-S EMIT
   5 A 1 ENC-MVK 0 A WHEN-NONZERO EMIT
   5 A 5 A 17 A ENC-SUB-L 0 A WHEN-NONZERO EMIT
   6 A 6 A 23 23 0 A IF-ZERO ENC-SET-S EMIT              \ or restore the hidden bit
   9 B 6 B ALWAYS ENC-NORM-L EMIT                       \ the same for y
   9 B 9 B -7 ENC-ADD-I5 EMIT
   6 B 6 B 9 B 0 B IF-NONZERO ENC-SHL-S EMIT
   5 B 1 ENC-MVK 0 B WHEN-NONZERO EMIT
   5 B 5 B 9 B ENC-SUB-L 0 B WHEN-NONZERO EMIT
   6 B 6 B 23 23 0 B IF-ZERO ENC-SET-S EMIT
   8 A 5 A 5 B ENC-SUB-L EMIT                           \ A8 = ex - ey + 127
   17 A 127 ENC-MVK EMIT
   8 A 8 A 17 A ENC-ADD-L EMIT
   0 A 6 A 6 B ALWAYS ENC-CMPLTU-L EMIT                 \ A0 = mx < my: a quotient below one
   9 A 1 ENC-MVK EMIT
   16 A 6 A 6 B ENC-SUB-L EMIT
   1 B 25 ENC-MVK EMIT
   9 A 0 ENC-MVK 0 A WHEN-NONZERO EMIT
   16 A 6 A ENC-MV-L 0 A WHEN-NONZERO EMIT
   8 A 8 A -1 ENC-ADD-I5 0 A WHEN-NONZERO EMIT
   1 B 26 ENC-MVK 0 A WHEN-NONZERO EMIT
   HERE {: loop:n :}
   16 A 16 A 1 ALWAYS ENC-SHL-U5 EMIT
   9 A 9 A 1 ALWAYS ENC-SHL-U5 EMIT
   1 A 16 A 6 B ALWAYS ENC-CMPLTU-L EMIT
   16 A 16 A 6 B ENC-SUB-L 1 A WHEN-ZERO EMIT
   9 A 9 A 1 ENC-ADD-I5 1 A WHEN-ZERO EMIT
   1 B 1 B -1 ENC-ADD-I5 EMIT
   loop 1 B WHEN BACK
   8 B 16 A 0 ALWAYS ENC-CMPLTU-U4 EMIT                 \ B8 = sticky
   1 A 8 A 1 ALWAYS ENC-CMPGT-I5 EMIT                   \ A1 = the result is denormal
   17 A 1 ENC-MVK EMIT
   17 A 17 A 8 A ENC-SUB-L EMIT                         \ A17 = extra right shift
   17 A 0 ENC-MVK 1 A WHEN-ZERO EMIT
   18 A 27 ENC-MVK EMIT
   2 A 17 A 18 A ALWAYS ENC-CMPGT-L EMIT
   17 A 27 ENC-MVK 2 A WHEN-NONZERO EMIT
   18 A 1 ENC-MVK EMIT
   18 A 18 A 17 A ALWAYS ENC-SHL-S EMIT
   18 A 18 A -1 ENC-ADD-I5 EMIT
   18 A 18 A 9 A ENC-AND-L EMIT                         \ bits about to be shifted out
   2 A 18 A 0 ALWAYS ENC-CMPLTU-U4 EMIT
   8 B 8 B 2 A ENC-OR-L EMIT
   9 A 9 A 17 A ALWAYS ENC-SHRU-S EMIT
   8 A 0 ENC-MVK 1 A WHEN-NONZERO EMIT
   18 A 9 A 30 31 ALWAYS ENC-EXTU-S EMIT                \ guard
   19 A 9 A 31 31 ALWAYS ENC-EXTU-S EMIT                \ round
   9 A 9 A 2 ALWAYS ENC-SHRU-U5 EMIT
   20 A 9 A 31 31 ALWAYS ENC-EXTU-S EMIT                \ lowest kept bit
   19 A 19 A 8 B ENC-OR-L EMIT
   19 A 19 A 20 A ENC-OR-L EMIT
   18 A 18 A 19 A ENC-AND-L EMIT                        \ increment
   20 A 254 ENC-MVK EMIT
   2 A 8 A 20 A ALWAYS ENC-CMPGT-L EMIT
   2 A WHEN FORWARD {: infinite3:n :}                   \ overflow
   8 A 8 A -1 ENC-ADD-I5 1 A WHEN-ZERO EMIT             \ the hidden bit carries one exponent unit
   8 A 8 A 23 ALWAYS ENC-SHL-U5 EMIT
   8 A 8 A 9 A ENC-ADD-L EMIT
   8 A 8 A 18 A ENC-ADD-L EMIT
   4 A 7 A 8 A ENC-OR-L EMIT
   RETURN
   x-nan RESOLVE
   4 A 4 A 22 22 ALWAYS ENC-SET-S EMIT RETURN
   y-nan RESOLVE
   4 A 4 B ENC-MV-L EMIT 4 A 4 A 22 22 ALWAYS ENC-SET-S EMIT RETURN
   invalid RESOLVE invalid2 RESOLVE
   4 A $7FC00000 ENC-MVKL EMIT 4 A $7FC00000 ENC-MVKH EMIT RETURN
   infinite RESOLVE infinite2 RESOLVE infinite3 RESOLVE
   8 A $7F800000 ENC-MVKL EMIT 8 A $7F800000 ENC-MVKH EMIT 4 A 7 A 8 A ENC-OR-L EMIT RETURN
   zero RESOLVE zero2 RESOLVE
   4 A 7 A ENC-MV-L EMIT RETURN ;


\ ---- register sets (SPRAB89 Table 8-9; divremu adds A5, which the contract returns) ----

: A-BIT ( n -- n ) 1 swap lshift ;
: B-BIT ( n -- n ) 32 + 1 swap lshift ;
: COMMON ( -- n ) 1 A-BIT 4 A-BIT or 0 B-BIT or 1 B-BIT or 2 B-BIT or 4 B-BIT or 30 B-BIT or 31 B-BIT or ;
: CALLER-SAVED ( -- n )
   0 32 0 ?do i 10 < i 15 > or if i A-BIT or i B-BIT or then loop ;

: PERMITTED! ( -- )
   COMMON 0 A-BIT or 2 A-BIT or 6 A-BIT or 5 B-BIT or PERMITTED 0 cells + !          \ divi
   COMMON 0 A-BIT or 2 A-BIT or 6 A-BIT or PERMITTED 1 cells + !                      \ divu
   COMMON 2 A-BIT or 5 A-BIT or 6 A-BIT or PERMITTED 2 cells + !                      \ remi
   COMMON 5 A-BIT or 7 A-BIT or PERMITTED 3 cells + !                                 \ remu
   COMMON 2 A-BIT or 5 A-BIT or 6 A-BIT or PERMITTED 4 cells + !                      \ divremi
   COMMON 0 A-BIT or 2 A-BIT or 5 A-BIT or 6 A-BIT or PERMITTED 5 cells + !           \ divremu
   CALLER-SAVED PERMITTED 6 cells + ! CALLER-SAVED PERMITTED 7 cells + !
   CALLER-SAVED PERMITTED 8 cells + ! ;
PERMITTED!

public

: HELPER-NAME$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 = if s" __c6xabi_divi" exit then
   idx 1 = if s" __c6xabi_divu" exit then
   idx 2 = if s" __c6xabi_remi" exit then
   idx 3 = if s" __c6xabi_remu" exit then
   idx 4 = if s" __c6xabi_divremi" exit then
   idx 5 = if s" __c6xabi_divremu" exit then
   idx 6 = if s" memcpy" exit then
   idx 7 = if s" memset" exit then
   idx 8 = if s" __c6xabi_divf" exit then
   E-OPERAND throw ;


\ Emits helper idx into the program buffer; PROGRAM$ reads it back.
: EMIT-HELPER ( n -- ) {: idx:n :}
   0 LEN ! 0 FIXUP-COUNT !
   idx 0 = if DIVI exit then
   idx 1 = if DIVU exit then
   idx 2 = if REMI exit then
   idx 3 = if REMU exit then
   idx 4 = if DIVREMI exit then
   idx 5 = if DIVREMU exit then
   idx 6 = if MEMCPY exit then
   idx 7 = if MEMSET exit then
   idx 8 = if DIVF exit then
   E-OPERAND throw ;

: PROGRAM$ ( -- ptr a n ) WORDS LEN @ ;

\ TRUE when helper idx may modify the register with that code (A0..A31 = 0..31,
\ B0..B31 = 32..63); B3 is the return address and is never checked.
: PERMITTED? ( n n -- bool ) {: idx:n code:n :}
   idx 0 < idx HELPER-COUNT >= or if E-OPERAND throw then
   code 0 < code 63 > or if E-OPERAND throw then
   PERMITTED idx cells + @ 1 code lshift and 0 <> ;

;package
