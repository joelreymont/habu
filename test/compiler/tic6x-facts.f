require lib/test.f
require test/checker-assert.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/facts.f

package C6XFACTS-TEST
private
using C6XASM
using C6XFACTS

variable T-UNIT
variable T-CROSS
variable T-READS
variable T-WRITES
variable T-LOADS
variable T-MEMORY
variable T-SIDE
variable T-BRANCH
variable T-IDLE

: A ( n -- gpr ) A-REG ;
: B ( n -- gpr ) B-REG ;
: BIT ( n -- n ) MASK-BIT ;
: B-BIT ( n -- n ) 32 + MASK-BIT ;

\ Classifies the instruction and keeps its facts for the checks below.
: SEE ( instruction -- )
   INSTRUCTION>N CLASSIFY C6XFACTS-FACTS:UNMAKE
   {: unit:n cross:n reads:n writes:n loads:n memory:n side:n branch:bool idle:n :}
   unit T-UNIT ! cross T-CROSS ! reads T-READS ! writes T-WRITES ! loads T-LOADS !
   memory T-MEMORY ! side T-SIDE ! branch if 1 else 0 then T-BRANCH ! idle T-IDLE ! ;

: SHAPE ( n n n n n -- ) {: unit:n cross:n reads:n writes:n loads:n :}
   T-UNIT @ unit T= T-CROSS @ cross T= T-READS @ reads T= T-WRITES @ writes T= T-LOADS @ loads T= ;


: ARITHMETIC-CASES ( -- )
   5 A 1 A 2 B ENC-ADD-L SEE                             \ .L1 reading B2 through 1X
   0 34 1 BIT 2 B-BIT or 5 BIT 0 SHAPE
   T-MEMORY @ 0 T= T-BRANCH @ 0 T= T-IDLE @ 0 T=
   5 B 1 B 2 B ENC-SUB-L SEE                             \ .L2 on its own side
   1 -1 1 B-BIT 2 B-BIT or 5 B-BIT 0 SHAPE
   6 A 1 A -3 ENC-ADD-I5 SEE                             \ no src1 register
   0 -1 1 BIT 6 BIT 0 SHAPE
   6 A 4 B ENC-MV-L SEE
   0 36 4 B-BIT 6 BIT 0 SHAPE
   7 A 3 A ALWAYS ENC-NORM-L SEE
   0 -1 3 BIT 7 BIT 0 SHAPE
   0 A 1 A 2 B 1 B IF-NONZERO ENC-SUBC-L SEE             \ the guard is read too
   0 34 1 BIT 2 B-BIT or 33 BIT or 0 BIT 0 SHAPE
   2 B 1 A 0 0 A IF-ZERO ENC-CMPGT-I5 SEE
   1 1 1 BIT 0 BIT or 2 B-BIT 0 SHAPE ;


: SHIFT-CASES ( -- )
   5 A 1 B 2 A ALWAYS ENC-SHL-S SEE                      \ .S1, value through 1X, count on side A
   2 33 1 B-BIT 2 BIT or 5 BIT 0 SHAPE
   5 B 1 B 3 ALWAYS ENC-SHRU-U5 SEE
   3 -1 1 B-BIT 5 B-BIT 0 SHAPE
   5 A 1 A 12 23 ALWAYS ENC-EXTU-S SEE                  \ field forms have no cross path
   2 -1 1 BIT 5 BIT 0 SHAPE
   5 A 1 A 3 3 ALWAYS ENC-SET-S SEE
   2 -1 1 BIT 5 BIT 0 SHAPE ;


: MOVE-CASES ( -- )
   4 A 7 ENC-MVK SEE
   2 -1 0 4 BIT 0 SHAPE
   4 B $12345678 ENC-MVKL SEE
   3 -1 0 4 B-BIT 0 SHAPE
   4 B $12345678 ENC-MVKH SEE                            \ keeps the low half
   3 -1 4 B-BIT 4 B-BIT 0 SHAPE
   4 A 5 A 3 ALWAYS ENC-ADDAB-U5 SEE
   4 -1 5 BIT 4 BIT 0 SHAPE
   4 B 5 B 6 B ALWAYS ENC-ADDAW-D SEE
   5 -1 5 B-BIT 6 B-BIT or 4 B-BIT 0 SHAPE ;


: MEMORY-CASES ( -- )
   7 A 4 B 1 >BYTE-OFFSET ALWAYS ENC-LDB++ SEE           \ .D2 by the base, data on side A
   5 -1 4 B-BIT 4 B-BIT 7 BIT SHAPE
   T-MEMORY @ 1 T= T-SIDE @ 0 T=
   7 A 5 A 1 >BYTE-OFFSET ALWAYS ENC-STB++ SEE
   4 -1 7 BIT 5 BIT or 5 BIT 0 SHAPE
   T-MEMORY @ 2 T= T-SIDE @ 0 T=
   16 A 4 B 8 >BYTE-OFFSET ALWAYS ENC-LDDW++ SEE         \ the pair lands
   5 -1 4 B-BIT 4 B-BIT 16 BIT 17 BIT or SHAPE
   6 B 5 A 0 >BYTE-OFFSET ALWAYS ENC-STDW SEE            \ no base update without ++
   4 -1 6 B-BIT 7 B-BIT or 5 BIT or 0 0 SHAPE
   T-MEMORY @ 2 T= T-SIDE @ 1 T=
   9 A 1 A 4 >BYTE-OFFSET ENC-LDW SEE
   4 -1 1 BIT 0 9 BIT SHAPE ;


: BRANCH-CASES ( -- )
   3 B ENC-B-REG SEE                                     \ .S2 reading B3 on its side
   3 -1 3 B-BIT 0 0 SHAPE T-BRANCH @ 1 T=
   4 A ENC-B-REG SEE                                     \ .S2 reading A4 through 2X
   3 4 4 BIT 0 0 SHAPE T-BRANCH @ 1 T=
   0 >SIDE 32 >BRANCH-OFFSET ENC-B-REL 1 A WHEN-ZERO SEE
   2 -1 1 BIT 0 0 SHAPE T-BRANCH @ 1 T=
   1 >SIDE -64 >BRANCH-OFFSET ENC-B-REL SEE
   3 -1 0 0 0 SHAPE T-BRANCH @ 1 T=
   1 ENC-NOP SEE
   -1 -1 0 0 0 SHAPE T-IDLE @ 1 T= T-BRANCH @ 0 T=
   7 ENC-NOP PARALLEL-NEXT SEE                           \ the p bit is not a fact
   T-IDLE @ 7 T=
   5 A 1 A 2 B ENC-ADD-L PARALLEL-NEXT SEE
   0 34 1 BIT 2 B-BIT or 5 BIT 0 SHAPE ;


: DISCARD ( facts -- ) C6XFACTS-FACTS:UNMAKE drop drop drop drop drop drop drop drop drop ;
: UNDECODABLE ( -- ) $FFFFFFFF CLASSIFY DISCARD ;
: STRAY-L ( -- ) 5 A 1 A 2 B ENC-ADD-L INSTRUCTION>N $1F 5 lshift or 3 5 lshift xor CLASSIFY DISCARD ;   \ an unmade .L op

: REFUSALS ( -- )
   [: UNDECODABLE ;] E-DECODE TTHROWSQ
   [: STRAY-L ;] E-DECODE TTHROWSQ ;


: RUN ( -- )
   ARITHMETIC-CASES SHIFT-CASES MOVE-CASES MEMORY-CASES BRANCH-CASES REFUSALS
   T-REPORT ;

RUN
;package
