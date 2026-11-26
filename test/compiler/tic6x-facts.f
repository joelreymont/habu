require src/lib/test/assert.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/facts.f

package C6XFACTS-TEST
private

variable T-UNIT
variable T-CROSS
variable T-READS
variable T-WRITES
variable T-LOADS
variable T-MEMORY
variable T-SIDE
variable T-BRANCH
variable T-IDLE

: A ( n -- C6XASM:gpr ) C6XASM:A-REG ;
: B ( n -- C6XASM:gpr ) C6XASM:B-REG ;
: BIT ( n -- n ) C6XFACTS:MASK-BIT ;
: B-BIT ( n -- n ) 32 + C6XFACTS:MASK-BIT ;

: SEE ( C6XASM:instruction -- )
   C6XASM:INSTRUCTION>N C6XFACTS:CLASSIFY C6XFACTS-FACTS:UNMAKE
   {: unit:n cross:n reads:n writes:n loads:n memory:n side:n branch:bool idle:n :}
   unit T-UNIT ! cross T-CROSS ! reads T-READS ! writes T-WRITES ! loads T-LOADS !
   memory T-MEMORY ! side T-SIDE ! branch if 1 else 0 then T-BRANCH ! idle T-IDLE ! ;

: SHAPE ( n n n n n -- ) {: unit:n cross:n reads:n writes:n loads:n :}
   T-UNIT @ unit T= T-CROSS @ cross T= T-READS @ reads T= T-WRITES @ writes T= T-LOADS @ loads T= ;


: ARITHMETIC-CASES ( -- )
   5 A 1 A 2 B C6XASM:ENC-ADD-L SEE                             \ .L1 reading B2 through 1X
   0 34 1 BIT 2 B-BIT or 5 BIT 0 SHAPE
   T-MEMORY @ 0 T= T-BRANCH @ 0 T= T-IDLE @ 0 T=
   5 B 1 B 2 B C6XASM:ENC-SUB-L SEE                             \ .L2 on its own side
   1 -1 1 B-BIT 2 B-BIT or 5 B-BIT 0 SHAPE
   6 A 1 A -3 C6XASM:ENC-ADD-I5 SEE                             \ no src1 register
   0 -1 1 BIT 6 BIT 0 SHAPE
   6 A 4 B C6XASM:ENC-MV-L SEE
   0 36 4 B-BIT 6 BIT 0 SHAPE
   7 A 3 A C6XASM:ALWAYS C6XASM:ENC-NORM-L SEE
   0 -1 3 BIT 7 BIT 0 SHAPE
   0 A 1 A 2 B 1 B C6XASM:IF-NONZERO C6XASM:ENC-SUBC-L SEE             \ the guard is read too
   0 34 1 BIT 2 B-BIT or 33 BIT or 0 BIT 0 SHAPE
   2 B 1 A 0 0 A C6XASM:IF-ZERO C6XASM:ENC-CMPGT-I5 SEE
   1 1 1 BIT 0 BIT or 2 B-BIT 0 SHAPE ;


: SHIFT-CASES ( -- )
   5 A 1 B 2 A C6XASM:ALWAYS C6XASM:ENC-SHL-S SEE                      \ .S1, value through 1X, count on side A
   2 33 1 B-BIT 2 BIT or 5 BIT 0 SHAPE
   5 B 1 B 3 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 SEE
   3 -1 1 B-BIT 5 B-BIT 0 SHAPE
   5 A 1 A 12 23 C6XASM:ALWAYS C6XASM:ENC-EXTU-S SEE                  \ field forms have no cross path
   2 -1 1 BIT 5 BIT 0 SHAPE
   5 A 1 A 3 3 C6XASM:ALWAYS C6XASM:ENC-SET-S SEE
   2 -1 1 BIT 5 BIT 0 SHAPE ;


: MOVE-CASES ( -- )
   4 A 7 C6XASM:ENC-MVK SEE
   2 -1 0 4 BIT 0 SHAPE
   4 B $12345678 C6XASM:ENC-MVKL SEE
   3 -1 0 4 B-BIT 0 SHAPE
   4 B $12345678 C6XASM:ENC-MVKH SEE                            \ keeps the low half
   3 -1 4 B-BIT 4 B-BIT 0 SHAPE
   4 A 5 A 3 C6XASM:ALWAYS C6XASM:ENC-ADDAB-U5 SEE
   4 -1 5 BIT 4 BIT 0 SHAPE
   4 B 5 B 6 B C6XASM:ALWAYS C6XASM:ENC-ADDAW-D SEE
   5 -1 5 B-BIT 6 B-BIT or 4 B-BIT 0 SHAPE ;


: MEMORY-CASES ( -- )
   7 A 4 B 1 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-LDB++ SEE           \ .D2 by the base, data on side A
   5 -1 4 B-BIT 4 B-BIT 7 BIT SHAPE
   T-MEMORY @ 1 T= T-SIDE @ 0 T=
   7 A 5 A 1 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-STB++ SEE
   4 -1 7 BIT 5 BIT or 5 BIT 0 SHAPE
   T-MEMORY @ 2 T= T-SIDE @ 0 T=
   16 A 4 B 8 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-LDDW++ SEE         \ the pair lands
   5 -1 4 B-BIT 4 B-BIT 16 BIT 17 BIT or SHAPE
   6 B 5 A 0 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-STDW SEE            \ no base update without ++
   4 -1 6 B-BIT 7 B-BIT or 5 BIT or 0 0 SHAPE
   T-MEMORY @ 2 T= T-SIDE @ 1 T=
   9 A 1 A 4 C6XASM:>BYTE-OFFSET C6XASM:ENC-LDW SEE
   4 -1 1 BIT 0 9 BIT SHAPE ;


: BRANCH-CASES ( -- )
   3 B C6XASM:ENC-B-REG SEE                                     \ .S2 reading B3 on its side
   3 -1 3 B-BIT 0 0 SHAPE T-BRANCH @ 1 T=
   4 A C6XASM:ENC-B-REG SEE                                     \ .S2 reading A4 through 2X
   3 4 4 BIT 0 0 SHAPE T-BRANCH @ 1 T=
   0 C6XASM:>SIDE 32 C6XASM:>BRANCH-OFFSET C6XASM:ENC-B-REL 1 A C6XASM:WHEN-ZERO SEE
   2 -1 1 BIT 0 0 SHAPE T-BRANCH @ 1 T=
   1 C6XASM:>SIDE -64 C6XASM:>BRANCH-OFFSET C6XASM:ENC-B-REL SEE
   3 -1 0 0 0 SHAPE T-BRANCH @ 1 T=
   1 C6XASM:ENC-NOP SEE
   -1 -1 0 0 0 SHAPE T-IDLE @ 1 T= T-BRANCH @ 0 T=
   7 C6XASM:ENC-NOP C6XASM:PARALLEL-NEXT SEE                           \ the p bit is not a fact
   T-IDLE @ 7 T=
   5 A 1 A 2 B C6XASM:ENC-ADD-L C6XASM:PARALLEL-NEXT SEE
   0 34 1 BIT 2 B-BIT or 5 BIT 0 SHAPE ;


: DISCARD ( C6XFACTS:facts -- ) C6XFACTS-FACTS:UNMAKE drop drop drop drop drop drop drop drop drop ;
: UNDECODABLE ( -- ) $FFFFFFFF C6XFACTS:CLASSIFY DISCARD ;
: STRAY-L ( -- ) 5 A 1 A 2 B C6XASM:ENC-ADD-L C6XASM:INSTRUCTION>N $1F 5 lshift or 3 5 lshift xor C6XFACTS:CLASSIFY DISCARD ;   \ an unmade .L op

: REJECTIONS ( -- )
   [: UNDECODABLE ;] C6XFACTS:E-DECODE TTHROWSQ
   [: STRAY-L ;] C6XFACTS:E-DECODE TTHROWSQ ;


: RUN ( -- )
   ARITHMETIC-CASES SHIFT-CASES MOVE-CASES MEMORY-CASES BRANCH-CASES REJECTIONS
   T-REPORT ;

RUN
;package
