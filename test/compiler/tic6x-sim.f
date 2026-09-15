require lib/test.f
require test/checker-assert.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/sim.f

package C6XSIM-TEST
private
using C6XASM
using C6XSIM

64 constant PROGRAM-MAX
create PROGRAM PROGRAM-MAX cells allot
variable PROGRAM-LEN

: P-RESET ( -- ) 0 PROGRAM-LEN ! RESET ;
: P+ ( C6XASM:instruction -- )
   PROGRAM-LEN @ PROGRAM-MAX >= if C6XASM:E-OPERAND throw then
   INSTRUCTION>N PROGRAM PROGRAM-LEN @ cells + ! 1 PROGRAM-LEN +! ;
: P-RETURN ( -- ) 3 B-REG ENC-B-REG P+ 5 ENC-NOP P+ ;
: P-RUN ( -- n ) PROGRAM PROGRAM-LEN @ CALL ;
: OFFSET ( n n -- C6XASM:branch-offset ) {: from:n target:n :}
   target 4 * from 4 * $FFFFFFE0 and - >BRANCH-OFFSET ;


: MOVE-CASES ( -- )
   P-RESET
   4 A-REG -5 ENC-MVK P+
   5 A-REG $1234 ENC-MVKL P+ 5 A-REG $ABCD1234 ENC-MVKH P+
   6 B-REG $8000 ENC-MVKL P+ 6 B-REG 0 ENC-MVKLH P+
   P-RETURN P-RUN 11 T=
   4 A@ $FFFFFFFB T= 5 A@ $ABCD1234 T= 6 B@ $8000 T= ;


: ARITHMETIC-CASES ( -- )
   P-RESET 7 1 A! 3 2 B! $80000000 3 A! -1 4 B!
   5 A-REG 1 A-REG 2 B-REG ENC-ADD-L P+                       \ 7 + 3
   6 A-REG 1 A-REG 2 B-REG ENC-SUB-L P+                       \ 7 - 3
   7 A-REG 1 A-REG -8 ENC-ADD-I5 P+                           \ 7 - 8
   8 A-REG 1 A-REG 2 B-REG ENC-AND-L P+ 9 A-REG 1 A-REG 2 B-REG ENC-XOR-L P+
   10 A-REG 3 A-REG ALWAYS ENC-ABS-L P+                       \ INT_MIN stays INT_MIN
   11 A-REG 4 B-REG ALWAYS ENC-NORM-L P+                      \ -1 has 31 redundant sign bits
   12 A-REG 1 A-REG ALWAYS ENC-NORM-L P+                      \ 7 has 28
   13 A-REG 3 A-REG ALWAYS ENC-NORM-L P+                      \ INT_MIN has none
   14 A-REG 1 A-REG 2 B-REG ENC-CMPEQ-L P+
   0 A-REG 3 A-REG 1 A-REG ALWAYS ENC-CMPGT-L P+              \ INT_MIN > 7 signed: no
   1 B-REG 4 B-REG 1 A-REG ALWAYS ENC-CMPGTU-L P+             \ $FFFFFFFF > 7 unsigned: yes
   2 B-REG 2 B-REG 1 A-REG ALWAYS ENC-CMPLT-L P+              \ 3 < 7: yes
   0 B-REG 4 B-REG 1 A-REG ALWAYS ENC-CMPLTU-L P+             \ $FFFFFFFF < 7 unsigned: no
   15 A-REG 1 A-REG -3 ALWAYS ENC-CMPGT-I5 P+                 \ -3 > 7: no
   16 A-REG 1 A-REG 9 ALWAYS ENC-CMPGTU-U4 P+                 \ 9 > 7: yes
   17 A-REG 1 A-REG -3 ALWAYS ENC-CMPLT-I5 P+                 \ -3 < 7: yes
   18 A-REG 1 A-REG 9 ALWAYS ENC-CMPLTU-U4 P+                 \ 9 < 7: no
   P-RETURN P-RUN drop
   5 A@ 10 T= 6 A@ 4 T= 7 A@ $FFFFFFFF T= 8 A@ 3 T= 9 A@ 4 T=
   10 A@ $80000000 T= 11 A@ 31 T= 12 A@ 28 T= 13 A@ 0 T= 14 A@ 0 T=
   0 A@ 0 T= 1 B@ 1 T= 2 B@ 1 T= 0 B@ 0 T= 15 A@ 0 T= 16 A@ 1 T= 17 A@ 1 T= 18 A@ 0 T= ;


: SUBC-CASES ( -- )
   P-RESET $125A 1 A! $1F12 2 A! 5 3 A! 3 4 B! $FFFFFFFF 5 A! 1 6 A!
   1 A-REG 1 A-REG 2 A-REG ALWAYS ENC-SUBC-L P+               \ the reference example: 0x125A < 0x1F12
   3 A-REG 3 A-REG 4 B-REG ALWAYS ENC-SUBC-L P+               \ 5 - 3 >= 0: (2 << 1) + 1
   5 A-REG 5 A-REG 6 A-REG ALWAYS ENC-SUBC-L P+               \ the comparison is unsigned
   P-RETURN P-RUN drop
   1 A@ $24B4 T= 3 A@ 5 T= 5 A@ $FFFFFFFD T= ;


: SHIFT-CASES ( -- )
   P-RESET $80000001 1 A! 4 2 A! 33 3 A! -8 4 B!
   5 A-REG 1 A-REG 2 A-REG ALWAYS ENC-SHL-S P+
   6 A-REG 1 A-REG 2 A-REG ALWAYS ENC-SHR-S P+
   7 A-REG 1 A-REG 2 A-REG ALWAYS ENC-SHRU-S P+
   8 A-REG 1 A-REG 3 A-REG ALWAYS ENC-SHL-S P+                \ counts past 31 clear
   9 A-REG 1 A-REG 3 A-REG ALWAYS ENC-SHR-S P+                \ and fill with the sign
   10 A-REG 4 B-REG 1 ALWAYS ENC-SHR-U5 P+
   11 A-REG 4 B-REG 1 ALWAYS ENC-SHRU-U5 P+
   12 A-REG 1 A-REG 31 ALWAYS ENC-SHL-U5 P+
   P-RETURN P-RUN drop
   5 A@ $10 T= 6 A@ $F8000000 T= 7 A@ $08000000 T= 8 A@ 0 T= 9 A@ $FFFFFFFF T=
   10 A@ $FFFFFFFC T= 11 A@ $7FFFFFFC T= 12 A@ $80000000 T= ;


: FIELD-CASES ( -- )
   P-RESET $07A4E01F 1 A! $A5A5A5A5 2 A!
   3 A-REG 1 A-REG 12 23 ALWAYS ENC-EXTU-S P+                 \ bits 19..11: 0x9C
   4 A-REG 1 A-REG 12 23 ALWAYS ENC-EXT-S P+
   5 A-REG 2 A-REG 15 23 ALWAYS ENC-SET-S P+                  \ bits 15..23 become ones
   6 A-REG 2 A-REG 15 23 ALWAYS ENC-CLR-S P+
   7 A-REG 1 A-REG 0 31 ALWAYS ENC-EXT-S P+                   \ sign of bit 31
   P-RETURN P-RUN drop
   3 A@ $9C T= 4 A@ $9C T= 5 A@ $A5FFA5A5 T= 6 A@ $A50025A5 T=
   7 A@ 0 T= ;


: MEMORY-CASES ( -- )
   P-RESET MEMORY-BASE 1 A! $7F 3 A! $80 4 A! $11223344 10 A! $55667788 11 A!
   3 A-REG 1 A-REG 0 >BYTE-OFFSET ALWAYS ENC-STB P+
   4 A-REG 1 A-REG 1 >BYTE-OFFSET ALWAYS ENC-STB P+
   5 A-REG 1 A-REG 1 >BYTE-OFFSET ALWAYS ENC-LDB P+           \ signed byte: -128
   6 A-REG 1 A-REG 1 >BYTE-OFFSET ALWAYS ENC-LDBU P+          \ unsigned: 128
   7 A-REG 5 A-REG ENC-MV-L P+                                \ still the old value in the delay slots
   4 ENC-NOP P+
   8 A-REG 5 A-REG ENC-MV-L P+                                \ the loaded value has landed
   10 A-REG 1 A-REG 8 >BYTE-OFFSET ALWAYS ENC-STDW P+
   12 A-REG 1 A-REG 8 >BYTE-OFFSET ALWAYS ENC-LDDW P+
   4 ENC-NOP P+
   9 A-REG 1 A-REG 1 >BYTE-OFFSET ALWAYS ENC-LDB++ P+         \ loads *base, then base moves on
   4 ENC-NOP P+
   14 A-REG 1 A-REG 3 ALWAYS ENC-ADDAW-U5 P+
   15 A-REG 1 A-REG 5 ALWAYS ENC-ADDAB-U5 P+
   P-RETURN P-RUN drop
   MEMORY-BASE MEMORY@ $7F T= MEMORY-BASE 1+ MEMORY@ $80 T=
   5 A@ $FFFFFF80 T= 6 A@ $80 T= 7 A@ 0 T= 8 A@ $FFFFFF80 T=
   MEMORY-BASE 8 + MEMORY-WORD@ $11223344 T= MEMORY-BASE 12 + MEMORY-WORD@ $55667788 T=
   12 A@ $11223344 T= 13 A@ $55667788 T=
   9 A@ $7F T= 1 A@ MEMORY-BASE 1+ T= 14 A@ MEMORY-BASE 13 + T= 15 A@ MEMORY-BASE 6 + T= ;


: BRANCH-CASES ( -- )
   P-RESET
   4 A-REG 1 ENC-MVK P+
   0 >SIDE 1 8 OFFSET ENC-B-REL P+
   5 A-REG 2 ENC-MVK P+ 6 A-REG 3 ENC-MVK P+ 7 A-REG 4 ENC-MVK P+   \ five delay slots execute
   8 A-REG 5 ENC-MVK P+ 9 A-REG 6 ENC-MVK P+
   10 A-REG 7 ENC-MVK P+                                       \ skipped
   11 A-REG 8 ENC-MVK P+                                       \ the target
   P-RETURN P-RUN 14 T=
   4 A@ 1 T= 9 A@ 6 T= 10 A@ 0 T= 11 A@ 8 T=
   P-RESET
   0 >SIDE 0 3 OFFSET ENC-B-REL 0 A-REG WHEN-NONZERO P+        \ A0 is zero: not taken
   10 A-REG 7 ENC-MVK P+ 5 ENC-NOP P+
   P-RETURN P-RUN drop
   10 A@ 7 T=
   P-RESET
   1 A-REG 3 ENC-MVK P+ 2 A-REG 0 ENC-MVK P+
   2 A-REG 2 A-REG 5 ENC-ADD-I5 P+                             \ the loop body at word 2
   1 A-REG 1 A-REG -1 ENC-ADD-I5 P+
   0 >SIDE 4 2 OFFSET ENC-B-REL 1 A-REG WHEN-NONZERO P+
   5 ENC-NOP P+
   P-RETURN P-RUN drop
   2 A@ 15 T= 1 A@ 0 T= ;


: PREDICATE-CASES ( -- )
   P-RESET -5 1 A! 0 0 A! 1 1 B!
   5 A-REG 1 A-REG 0 A-REG IF-NONZERO ENC-ABS-L P+             \ A0 is zero: skipped
   6 A-REG 1 A-REG 0 A-REG IF-ZERO ENC-ABS-L P+
   7 A-REG 1 A-REG 1 B-REG IF-NONZERO ENC-ABS-L P+
   8 A-REG 1 A-REG 1 B-REG IF-ZERO ENC-ABS-L P+
   P-RETURN P-RUN drop
   5 A@ 0 T= 6 A@ 5 T= 7 A@ 5 T= 8 A@ 0 T= ;


: UNDECODABLE ( -- ) P-RESET $FFFFFFFF >INSTRUCTION P+ P-RETURN P-RUN drop ;
: OUTSIDE ( -- ) P-RESET 5 A-REG 1 A-REG 0 >BYTE-OFFSET ALWAYS ENC-LDB P+ P-RETURN P-RUN drop ;
: FOREVER ( -- ) P-RESET 0 >SIDE 0 0 OFFSET ENC-B-REL P+ 5 ENC-NOP P+ 1000 BUDGET! P-RUN drop ;
: EMPTY ( -- ) P-RESET P-RUN drop ;

: REFUSALS ( -- )
   [: UNDECODABLE ;] E-DECODE TTHROWSQ
   [: OUTSIDE ;] E-FAULT TTHROWSQ
   [: FOREVER ;] E-LIMIT TTHROWSQ
   [: EMPTY ;] C6XSIM:E-OPERAND TTHROWSQ ;


: RUN ( -- )
   MOVE-CASES ARITHMETIC-CASES SUBC-CASES SHIFT-CASES FIELD-CASES MEMORY-CASES
   BRANCH-CASES PREDICATE-CASES REFUSALS
   T-REPORT ;

RUN
;package
