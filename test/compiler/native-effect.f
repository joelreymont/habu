\ native-effect.f - acceptance suite for the typed ARM64 routine-effect schema.
\
\ Covers src/compiler/native-effect.f through its public production words. Five
\ things are owed and each is proved rather than sampled:
\
\ 1. THE BOUNDS ARE THE ASSEMBLER'S. The schema states how many registers a file
\    holds, whether x18 is platform-reserved, and how far a frame slot can sit
\    from the stack pointer. Every one of those numbers is asserted against the
\    constant in src/arch/arm64/asm.f it was derived from, so a bound that moved
\    in the shipped encoder reddens here instead of silently disagreeing. The
\    assembler is loaded for its constants only; nothing in this suite calls an
\    encoder, so no stack-effect declaration is needed for it.
\
\ 2. AN INCOHERENT CONTRACT IS REFUSED, ONE CASE PER RULE, WITH ITS EXACT CODE.
\    A missing or role-swapped argument is a CHECKER refusal instead, proved with
\    candidate definitions the checker declines to certify, so it never reaches
\    runtime at all.
\
\ 3. A FORGED CONTRACT CARRIES NO IDENTITY. `routine` is a public family, so its
\    generated MAKE can assemble a record that never passed the constructor. Every
\    word that answers with an identity or a decision is shown to refuse one.
\
\ 4. WHAT IS PRESERVED IS THE COMPLEMENT OF WHAT IS RETURNED AND DESTROYED. The
\    derived set is checked against an independently computed complement over the
\    whole legal file, not against a restatement of the module's own arithmetic.
\    The other three derived sets are here for the same reason: the two interface
\    sets, which are what the ordered argument and result lists name, and the
\    writable set an allocator hands registers out of - which has to hold the
\    result register precisely because the destroyed set cannot.
\
\ 6. AN ORDERED REGISTER LIST IS ORDERED, BOUNDED AND CANONICAL. Position order
\    is the declaration's and not the register number's; a register named twice
\    is refused, because a caller cannot put two values in one place; a register
\    no routine may hold state in is refused by the set rule's own name; a
\    position past the end has no answer; and a packed cell the generated
\    constructor assembled with bits past its length is refused, which is what
\    keeps the digest agreeing with the structural comparison.
\
\ 5. EVERY FIELD CHANGES IDENTITY. The suite enumerates 392 legal contracts - one
\    sweep over the register roles and the flag family, one over the traits and
\    every legal shape of control, link, frame and stack delta - and shows their
\    digests are pairwise distinct and that digest equality holds exactly where
\    the structural comparison holds. The enumeration is driven by this suite's
\    own index arithmetic and its own legality rules, not by the module under
\    test, and the row count is asserted, so a rule that silently widened shows up
\    as a changed legal-domain size.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require src/compiler/native-effect.f
require src/arch/arm64/machine.f
require src/arch/arm64/asm.f

package NEFF-TEST
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM
private

\ ---- consuming contract values -----------------------------------------------
\ A contract is fourteen stack cells, so a test that only wants the throw has to
\ unmake what it built.
: DROP-ROUTINE ( NEFF:routine -- )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop drop drop ;

: DROP-DIGEST ( CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE drop drop drop drop ;

\ ---- named register sets -----------------------------------------------------
: X0 ( -- NEFF:gprs )   0 NEFF:GPR-REG ;
: X1 ( -- NEFF:gprs )   1 NEFF:GPR-REG ;
: X2 ( -- NEFF:gprs )   2 NEFF:GPR-REG ;
: D0 ( -- NEFF:fprs )   0 NEFF:FPR-REG ;
: D1 ( -- NEFF:fprs )   1 NEFF:FPR-REG ;
: D2 ( -- NEFF:fprs )   2 NEFF:FPR-REG ;

\ ---- named register lists ----------------------------------------------------
\ One position, and two, so a case can say which register arrives where without
\ spelling the packing out.
: SQ ( n -- NEFF:placeseq )
   NEFF:SEQ-NONE swap NEFF:SEQ-WITH ;

: SQ2 ( n n -- NEFF:placeseq )
   {: a:n b:n :}
   a SQ b NEFF:SEQ-WITH ;

: SQ-NONE ( -- NEFF:placeseq )   NEFF:SEQ-NONE ;

\ ---- forged set values -------------------------------------------------------
\ A set the generated constructor assembled without passing the checked one, so
\ it is not a set at all: its sign bit is set, and a set is a collection of
\ members and never a negative number.
: FORGED-GPRS ( -- NEFF:gprs )     1 63 lshift NEFF-GPRS:MAKE ;
: FORGED-FPRS ( -- NEFF:fprs )     1 63 lshift NEFF-FPRS:MAKE ;
: FORGED-TRAITS ( -- NEFF:traits ) 8 NEFF-TRAITS:MAKE ;

\ ---- contract shorthands -----------------------------------------------------
\ Each fixes the fields a case is not about, so a case reads as the one fact it
\ is making.

: R-GPR ( NEFF:placeseq NEFF:placeseq NEFF:gprs -- NEFF:routine )
   {: gi:placeseq gr:placeseq gc:gprs :}
   NEFF-CONV:REGISTER gi gr gc
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

\ The same with the other convention declared, for the cases whose subject is a
\ contract that passes its values through the caller's data stack.
: R-DSTACK ( NEFF:placeseq NEFF:placeseq NEFF:gprs -- NEFF:routine )
   {: gi:placeseq gr:placeseq gc:gprs :}
   NEFF-CONV:DSTACK gi gr gc
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

: R-FPR ( NEFF:fprs NEFF:fprs NEFF:fprs -- NEFF:routine )
   {: fi:fprs fr:fprs fc:fprs :}
   NEFF-CONV:REGISTER SQ-NONE SQ-NONE NEFF:GPR-NONE fi fr fc
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

: R-STACK ( NEFF:control n n -- NEFF:routine )
   {: c:control size:n delta:n :}
   NEFF-CONV:REGISTER SQ-NONE SQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED c
   NEFF:TRAITS-NONE size delta A64M:MACHINE NEFF:ROUTINE ;

: R-LINK ( NEFF:control NEFF:link -- NEFF:routine )
   {: c:control l:link :}
   NEFF-CONV:REGISTER SQ-NONE SQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED l c NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

: R-RESULT ( NEFF:placeseq NEFF:fprs NEFF:nzcv NEFF:control -- NEFF:routine )
   {: gr:placeseq fr:fprs z:nzcv c:control :}
   NEFF-CONV:REGISTER SQ-NONE gr NEFF:GPR-NONE
   NEFF:FPR-NONE fr NEFF:FPR-NONE
   z NEFF-LINK:PRESERVED c NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

\ A leaf that touches nothing: the neutral contract every case starts from.
: LEAF ( -- NEFF:routine )
   SQ-NONE SQ-NONE NEFF:GPR-NONE R-GPR ;

\ A routine with a frame, for the slot cases.
: FRAMED ( n -- NEFF:routine )
   {: size:n :}
   NEFF-CONTROL:RETURNS size 0 R-STACK ;

\ ---- asking the machine about one register ------------------------------------
\ A set of registers is a value and says nothing about any machine, so a register
\ the ARM64 machine gave another owner is refused where the machine is named: at
\ the contract. These are the two routes a caller has into that question - a mask
\ and a single register - and each builds the contract that would hand the
\ register out.
: CLOBBERS ( n -- )
   NEFF:GPR-SET {: gc:NEFF:gprs :}
   SQ-NONE SQ-NONE gc R-GPR DROP-ROUTINE ;

: CLOBBERS-REG ( n -- )
   NEFF:GPR-REG {: gc:NEFF:gprs :}
   SQ-NONE SQ-NONE gc R-GPR DROP-ROUTINE ;

: FCLOBBERS ( n -- )
   NEFF:FPR-SET {: fc:NEFF:fprs :}
   NEFF:FPR-NONE NEFF:FPR-NONE fc R-FPR DROP-ROUTINE ;

\ And the same question through the ordered convention: a register named as the
\ place an argument arrives in is a register the routine reads.
: ARRIVES-IN ( n -- )
   SQ SQ-NONE NEFF:GPR-NONE R-GPR DROP-ROUTINE ;

: X18-RESERVED-MASK ( -- n )
   HB-TARGET-LINUX? if 0 exit then
   HB-TARGET-MACOS? if 1 ARM-X18 lshift exit then
   E-CTGT-ABI throw ;

\ ---- 1. the bounds are the assembler's ---------------------------------------
\ REG-LIM, ARM-X18 and IMM12-LIM are the shipped encoder's own
\ constants. Every schema bound is stated as a function of one of them.
: MACHINE-FACTS ( -- )
   A64M:FILE-SIZE REG-LIM T=
   A64M:RESERVED-GPRS 1 ARM-X18 lshift and X18-RESERVED-MASK T=
   A64M:ZERO-GPR REG-LIM 1- T=
   A64M:LINK-GPR 30 T=
   A64M:SP-ALIGN 16 T=
   8 A64M:SLOT-REACH IMM12-LIM 1- 8 * T=
   4 A64M:SLOT-REACH IMM12-LIM 1- 4 * T=
   1 A64M:SLOT-REACH IMM12-LIM 1- T=
   8 A64M:SLOT-REACH dup A64M:SP-ALIGN mod - A64M:FRAME-MAX T=
   \ The whole file less BOTH owners' claims: the target's optional x18 plus
   \ x30/x31 and the
   \ five registers the running engine occupies (x19 data stack, x20 DATA/RBASE,
   \ x26 DBASE, x27 NDICT, x28 CP). Written as the derivation rather than as one
   \ hex number, so a register claimed in src/habu/layout.f moves this assertion
   \ with it instead of reddening it.
   A64M:MACHINE NEFF:GPR-ALL NEFF:GPRS-N
      1 A64M:FILE-SIZE lshift 1 -  A64M:RESERVED-GPRS invert and  T=
   \ and the engine's half of that claim is the layout owner's, not this file's
   A64M:ENGINE-GPRS ENGINE-GPR:MASK T=
   A64M:MACHINE NEFF:FPR-ALL NEFF:FPRS-N $FFFFFFFF T= ;

: X18-VOCABULARY ( -- )
   1 18 lshift NEFF:GPR-SET NEFF:GPRS-N 1 18 lshift T=
   18 NEFF:GPR-REG NEFF:GPRS-N 1 18 lshift T=
   HB-TARGET-LINUX? if
      1 18 lshift CLOBBERS
      18 CLOBBERS-REG
      exit
   then
   HB-TARGET-MACOS? if
      [: 1 18 lshift CLOBBERS ;] E-NEFF-GPR TTHROWSQ
      [: 18 CLOBBERS-REG ;] E-NEFF-GPR TTHROWSQ
      exit
   then
   E-CTGT-ABI throw ;

\ ---- 2a. the register vocabulary ---------------------------------------------
\ x19, x30 and x31 may never be named; x18 follows the host policy above. A bit
\ past the file is also refused. The floating file has no reserved member, so its
\ whole width is nameable.
: VOCABULARY ( -- )
   NEFF:GPR-NONE NEFF:GPRS-N 0 T=
   0 NEFF:GPR-SET NEFF:GPRS-N 0 T=
   X0 NEFF:GPRS-N 1 T=
   X18-VOCABULARY
   [: 1 19 lshift CLOBBERS ;] E-NEFF-GPR TTHROWSQ
   [: 1 30 lshift CLOBBERS ;] E-NEFF-GPR TTHROWSQ
   [: 1 31 lshift CLOBBERS ;] E-NEFF-GPR TTHROWSQ
   [: 1 32 lshift CLOBBERS ;] E-NEFF-GPR TTHROWSQ
   [: 19 CLOBBERS-REG ;] E-NEFF-GPR TTHROWSQ
   [: 30 CLOBBERS-REG ;] E-NEFF-GPR TTHROWSQ
   [: 31 CLOBBERS-REG ;] E-NEFF-GPR TTHROWSQ
   [: -1 NEFF:GPR-SET NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   [: 32 NEFF:GPR-REG NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   [: -1 NEFF:GPR-REG NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   17 NEFF:GPR-REG NEFF:GPRS-N 1 17 lshift T=
   17 CLOBBERS-REG
   \ x21: the first register past the engine's x20 claim, so the positive case
   \ sits right against the refused neighbour instead of safely far from it
   21 NEFF:GPR-REG NEFF:GPRS-N 1 21 lshift T=
   29 NEFF:GPR-REG NEFF:GPRS-N 1 29 lshift T=
   18 NEFF:FPR-REG NEFF:FPRS-N 1 18 lshift T=
   31 NEFF:FPR-REG NEFF:FPRS-N 1 31 lshift T=
   [: 1 32 lshift FCLOBBERS ;] E-NEFF-FPR TTHROWSQ
   [: -1 NEFF:FPR-SET NEFF:FPRS-N drop ;] E-NEFF-FPR TTHROWSQ
   [: 32 NEFF:FPR-REG NEFF:FPRS-N drop ;] E-NEFF-FPR TTHROWSQ
   [: -1 NEFF:FPR-REG NEFF:FPRS-N drop ;] E-NEFF-FPR TTHROWSQ ;

\ ---- 2b. set algebra ---------------------------------------------------------
: ALGEBRA ( -- )
   X0 X1 NEFF:GPR-WITH NEFF:GPRS-N 3 T=
   X0 X1 NEFF:GPR-WITH X0 NEFF:GPR-WITHOUT NEFF:GPRS-N 2 T=
   X0 X1 NEFF:GPR-WITH X0 NEFF:GPR-HAS? TTRUE
   X0 X1 NEFF:GPR-HAS? TFALSE
   X0 NEFF:GPR-NONE NEFF:GPR-HAS? TTRUE
   D0 D1 NEFF:FPR-WITH NEFF:FPRS-N 3 T=
   D0 D1 NEFF:FPR-WITH D1 NEFF:FPR-WITHOUT NEFF:FPRS-N 1 T=
   D0 D1 NEFF:FPR-WITH D1 NEFF:FPR-HAS? TTRUE
   D0 D1 NEFF:FPR-HAS? TFALSE
   NEFF:TRAITS-NONE NEFF:TRAITS-N 0 T=
   NEFF:T-CALL NEFF:T-SYSCALL NEFF:TRAITS-WITH NEFF:TRAITS-N 5 T=
   NEFF:T-CALL NEFF:T-SYSCALL NEFF:TRAITS-WITH NEFF:T-CALL NEFF:TRAITS-HAS? TTRUE
   NEFF:T-CALL NEFF:T-INDIRECT NEFF:TRAITS-HAS? TFALSE
   [: 8 NEFF:TRAIT-SET NEFF:TRAITS-N drop ;] E-NEFF-TRAIT TTHROWSQ
   [: -1 NEFF:TRAIT-SET NEFF:TRAITS-N drop ;] E-NEFF-TRAIT TTHROWSQ
   [: FORGED-GPRS X0 NEFF:GPR-WITH NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   [: FORGED-GPRS X0 NEFF:GPR-WITHOUT NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   [: X0 FORGED-GPRS NEFF:GPR-WITHOUT NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   [: FORGED-GPRS X0 NEFF:GPR-HAS? drop ;] E-NEFF-GPR TTHROWSQ
   [: X0 FORGED-GPRS NEFF:GPR-HAS? drop ;] E-NEFF-GPR TTHROWSQ
   [: FORGED-FPRS D0 NEFF:FPR-WITH NEFF:FPRS-N drop ;] E-NEFF-FPR TTHROWSQ
   [: FORGED-FPRS D0 NEFF:FPR-WITHOUT NEFF:FPRS-N drop ;] E-NEFF-FPR TTHROWSQ
   [: FORGED-FPRS D0 NEFF:FPR-HAS? drop ;] E-NEFF-FPR TTHROWSQ
   [: FORGED-TRAITS NEFF:T-CALL NEFF:TRAITS-WITH NEFF:TRAITS-N drop ;]
      E-NEFF-TRAIT TTHROWSQ
   [: FORGED-TRAITS NEFF:T-CALL NEFF:TRAITS-HAS? drop ;] E-NEFF-TRAIT TTHROWSQ
   [: NEFF:T-CALL FORGED-TRAITS NEFF:TRAITS-HAS? drop ;] E-NEFF-TRAIT TTHROWSQ ;

\ ---- 2b2. the ordered register list -------------------------------------------
\ A list of as many positions as one can hold, so the limit is reached by
\ appending rather than by a number written here.
: LONG-SEQ ( n -- NEFF:placeseq )
   {: n:n :}
   SQ-NONE
   n 0 ?do i NEFF:SEQ-WITH loop ;

: X18-SEQUENCE ( -- )
   18 SQ 0 NEFF:SEQ-REG@ 18 T=
   HB-TARGET-LINUX? if 18 ARRIVES-IN exit then
   HB-TARGET-MACOS? if
      [: 18 ARRIVES-IN ;] E-NEFF-GPR TTHROWSQ
      exit
   then
   E-CTGT-ABI throw ;

: SEQUENCE ( -- )
   SQ-NONE NEFF:SEQ-LEN 0 T=
   SQ-NONE NEFF:SEQ-SET NEFF:GPRS-N 0 T=
   0 SQ NEFF:SEQ-LEN 1 T=
   0 SQ 0 NEFF:SEQ-REG@ 0 T=
   0 1 SQ2 NEFF:SEQ-LEN 2 T=
   0 1 SQ2 0 NEFF:SEQ-REG@ 0 T=
   0 1 SQ2 1 NEFF:SEQ-REG@ 1 T=
   2 0 SQ2 0 NEFF:SEQ-REG@ 2 T=
   2 0 SQ2 1 NEFF:SEQ-REG@ 0 T=
   0 1 SQ2 NEFF:SEQ-SET NEFF:GPRS-N X0 X1 NEFF:GPR-WITH NEFF:GPRS-N T=
   2 0 SQ2 NEFF:SEQ-SET NEFF:GPRS-N X0 X2 NEFF:GPR-WITH NEFF:GPRS-N T=
   NEFF:SEQ-LIMIT LONG-SEQ NEFF:SEQ-LEN NEFF:SEQ-LIMIT T=
   NEFF:SEQ-LIMIT LONG-SEQ NEFF:SEQ-LIMIT 1- NEFF:SEQ-REG@ NEFF:SEQ-LIMIT 1- T=
   17 SQ 0 NEFF:SEQ-REG@ 17 T=
   X18-SEQUENCE
   29 SQ 0 NEFF:SEQ-REG@ 29 T=
   [: 0 0 SQ2 NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   [: 2 1 SQ2 1 NEFF:SEQ-WITH NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   \ x30 and x31 are places this schema can write down, because a list is a value
   \ and names no machine: the link register and the stack pointer are the ARM64
   \ machine's answer, and they are refused where that machine is named - at the
   \ contract that would have the routine hold state in them.
   30 SQ 0 NEFF:SEQ-REG@ 30 T=
   31 SQ 0 NEFF:SEQ-REG@ 31 T=
   [: 30 ARRIVES-IN ;] E-NEFF-GPR TTHROWSQ
   [: 31 ARRIVES-IN ;] E-NEFF-GPR TTHROWSQ
   [: 32 SQ NEFF:SEQ-LEN drop ;] E-NEFF-GPR TTHROWSQ
   [: -1 SQ NEFF:SEQ-LEN drop ;] E-NEFF-GPR TTHROWSQ
   [: NEFF:SEQ-LIMIT 1+ LONG-SEQ NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   [: SQ-NONE 0 NEFF:SEQ-REG@ drop ;] E-NEFF-SEQ TTHROWSQ
   [: 0 SQ 1 NEFF:SEQ-REG@ drop ;] E-NEFF-SEQ TTHROWSQ
   [: 0 SQ -1 NEFF:SEQ-REG@ drop ;] E-NEFF-SEQ TTHROWSQ
   [: 1 NEFF-PLACESEQ:MAKE NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   [: 15 60 lshift NEFF-PLACESEQ:MAKE NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   \ and a forged list that names the link register at position zero: the packed
   \ cell is a legal list, and the contract built over it is what refuses it
   1 60 lshift 30 or NEFF-PLACESEQ:MAKE 0 NEFF:SEQ-REG@ 30 T=
   [: 1 60 lshift 30 or NEFF-PLACESEQ:MAKE SQ-NONE NEFF:GPR-NONE R-GPR
      DROP-ROUTINE ;] E-NEFF-GPR TTHROWSQ
   [: -1 NEFF-PLACESEQ:MAKE NEFF:SEQ-SET NEFF:GPRS-N drop ;]
      E-NEFF-SEQ TTHROWSQ ;

\ ---- 2b3. the other kind of place --------------------------------------------
\ A position can name a slot of the caller's data stack instead of a register,
\ which is what design section 7.6's convention needs. Four things are owed: the
\ two kinds are told apart, a payload read as the wrong kind is refused rather
\ than answered with a number that would read as the other one, a slot is not a
\ register anywhere a set is derived, and one register and one slot with the same
\ number are two different places rather than a repeat.
: DQ ( n -- NEFF:placeseq )
   SQ-NONE swap NEFF:SEQ-WITH-SLOT ;

: DQ2 ( n n -- NEFF:placeseq )
   {: a:n b:n :}
   a DQ b NEFF:SEQ-WITH-SLOT ;

: PLACES ( -- )
   0 DQ NEFF:SEQ-LEN 1 T=
   0 DQ 0 NEFF:SEQ-SLOT@ 0 T=
   0 DQ NEFF:SEQ-SLOTS 1 T=
   0 DQ 0 NEFF:SEQ-KIND@ NEFF-PKIND:DSLOT NEFF-PKIND:EQ TTRUE
   0 SQ 0 NEFF:SEQ-KIND@ NEFF-PKIND:GPR NEFF-PKIND:EQ TTRUE
   0 1 DQ2 NEFF:SEQ-LEN 2 T=
   0 1 DQ2 0 NEFF:SEQ-SLOT@ 0 T=
   0 1 DQ2 1 NEFF:SEQ-SLOT@ 1 T=
   0 1 DQ2 NEFF:SEQ-SLOTS 2 T=
   0 1 SQ2 NEFF:SEQ-SLOTS 0 T=
   \ a data-stack place is in no register set, so a routine that takes everything
   \ off the stack reads and returns no register on account of its convention
   0 1 DQ2 NEFF:SEQ-SET NEFF:GPRS-N 0 T=
   0 DQ 0 DQ X2 R-DSTACK NEFF:GPR-IN@ NEFF:GPRS-N 0 T=
   0 DQ 0 DQ X2 R-DSTACK NEFF:GPR-RESULT@ NEFF:GPRS-N 0 T=
   \ one register and one slot with the same number are two places, not a repeat
   SQ-NONE 0 NEFF:SEQ-WITH 0 NEFF:SEQ-WITH-SLOT NEFF:SEQ-LEN 2 T=
   NEFF:SEQ-SLOT-LIMIT DQ 0 NEFF:SEQ-SLOT@ NEFF:SEQ-SLOT-LIMIT T=
   \ and the refusals
   [: 0 DQ 0 NEFF:SEQ-REG@ drop ;] E-NEFF-KIND TTHROWSQ
   [: 0 SQ 0 NEFF:SEQ-SLOT@ drop ;] E-NEFF-KIND TTHROWSQ
   [: 0 0 DQ2 NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   [: NEFF:SEQ-SLOT-LIMIT 1+ DQ NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ
   [: -1 DQ NEFF:SEQ-LEN drop ;] E-NEFF-SEQ TTHROWSQ ;

: STACK-RANGES ( -- )
   0 NEFF:SEQ-DSTACK NEFF:SEQ-LEN 0 T=
   2 NEFF:SEQ-DSTACK NEFF:SEQ-LEN 2 T=
   2 NEFF:SEQ-DSTACK 1 NEFF:SEQ-SLOT@ 1 T=
   64 NEFF:SEQ-DSTACK NEFF:SEQ-LEN 64 T=
   64 NEFF:SEQ-DSTACK 63 NEFF:SEQ-SLOT@ 63 T=
   64 NEFF:SEQ-DSTACK NEFF:SEQ-SLOTS 64 T=
   64 NEFF:SEQ-DSTACK NEFF:SEQ-SET NEFF:GPRS-N 0 T=
   64 NEFF:SEQ-DSTACK 63 NEFF:SEQ-KIND@ NEFF-PKIND:DSLOT NEFF-PKIND:EQ TTRUE
   [: 64 NEFF:SEQ-DSTACK 64 NEFF:SEQ-SLOT@ drop ;] E-NEFF-SEQ TTHROWSQ
   [: 64 NEFF:SEQ-DSTACK 0 NEFF:SEQ-REG@ drop ;] E-NEFF-KIND TTHROWSQ
   [: -1 NEFF:SEQ-DSTACK drop ;] E-NEFF-SEQ TTHROWSQ ;

\ ---- 2b5. the declared convention and the places are one statement -----------
\ An empty place list is silent about which convention a routine speaks - it is
\ what a routine passing nothing has either way - so the contract states the
\ convention and the constructor holds the two against each other. Both
\ directions are refused, and refused HERE, where the contract is built, so no
\ pass downstream ever holds one: a data-stack convention naming a register and a
\ register convention naming a slot of the caller's stack.
\
\ AND A LIST OF NOTHING SATISFIES EITHER, which is the case the whole field was
\ minted for: a ( -- ) word declares no place and is still entered through the
\ data stack, and the contract can now say so.
: CONV-CASES ( -- )
   NEFF-CONV:DSTACK NEFF-CONV:DSTACK NEFF-CONV:EQ TTRUE
   NEFF-CONV:DSTACK NEFF-CONV:REGISTER NEFF-CONV:EQ TFALSE
   SQ-NONE SQ-NONE NEFF:GPR-NONE R-DSTACK NEFF:CONV@
      NEFF-CONV:DSTACK NEFF-CONV:EQ TTRUE
   SQ-NONE SQ-NONE NEFF:GPR-NONE R-GPR NEFF:CONV@
      NEFF-CONV:REGISTER NEFF-CONV:EQ TTRUE
   0 DQ 1 DQ X2 R-DSTACK NEFF:CONV@
      NEFF-CONV:DSTACK NEFF-CONV:EQ TTRUE
   \ two contracts alike in every other field are not the same contract
   SQ-NONE SQ-NONE NEFF:GPR-NONE R-DSTACK
   SQ-NONE SQ-NONE NEFF:GPR-NONE R-GPR NEFF:SAME? TFALSE
   \ a data-stack convention may not name a register, on either side
   [: 0 SQ SQ-NONE NEFF:GPR-NONE R-DSTACK DROP-ROUTINE ;]
      E-NEFF-CONV TTHROWSQ
   [: SQ-NONE 0 SQ X2 R-DSTACK DROP-ROUTINE ;]
      E-NEFF-CONV TTHROWSQ
   \ and a register convention may not name a data-stack slot, on either side
   [: 0 DQ SQ-NONE NEFF:GPR-NONE R-GPR DROP-ROUTINE ;]
      E-NEFF-CONV TTHROWSQ
   [: SQ-NONE 0 DQ NEFF:GPR-NONE R-GPR DROP-ROUTINE ;]
      E-NEFF-CONV TTHROWSQ
   \ a side that mixes the two kinds is refused whichever is declared
   [: SQ-NONE 0 NEFF:SEQ-WITH 0 NEFF:SEQ-WITH-SLOT
      SQ-NONE NEFF:GPR-NONE R-DSTACK DROP-ROUTINE ;]
      E-NEFF-CONV TTHROWSQ
   [: SQ-NONE 0 NEFF:SEQ-WITH 0 NEFF:SEQ-WITH-SLOT
      SQ-NONE NEFF:GPR-NONE R-GPR DROP-ROUTINE ;]
      E-NEFF-CONV TTHROWSQ ;

\ ---- 2b4. the engine's registers are unbuildable ------------------------------
\ The running engine holds its data-stack pointer, DATA/RBASE, DBASE, NDICT and
\ CP in general registers, and no routine this schema can describe may hold
\ state in any of them: they are out of the general-register mask exactly as
\ x30 and 31 are, so every route into a contract refuses them and there is
\ no contract that hands one out to be allocated from. Each register under test
\ comes from src/habu/layout.f's own per-register constant - the emitters'
\ authority - so the claim proved here is the engine's actual claim: dropping a
\ register from ENGINE-GPR:MASK while the engine still occupies it reddens the
\ route fixtures below, which is the drift the one-authority design must refuse.
\ Each of the five routes into a contract is tried, because a check that only
\ closes the door a caller happens to use is not a closed door.
variable ER-REG

: ER ( -- n )   ER-REG @ ;

: ENGINE-ROUTES ( n -- )
   ER-REG !
   A64M:MACHINE NEFF:GPR-ALL NEFF:GPRS-N  1 ER lshift and  0 T=
   [: ER CLOBBERS-REG ;] E-NEFF-GPR TTHROWSQ
   [: 1 ER lshift CLOBBERS ;] E-NEFF-GPR TTHROWSQ
   [: ER ARRIVES-IN ;] E-NEFF-GPR TTHROWSQ
   [: SQ-NONE SQ-NONE  1 ER lshift NEFF-GPRS:MAKE  R-GPR
      DROP-ROUTINE ;] E-NEFF-GPR TTHROWSQ
   [: SQ-NONE SQ-NONE  1 ER lshift NEFF-GPRS:MAKE  R-GPR
      NEFF:GPR-WRITABLE NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ ;

: ENGINE-RESERVED ( -- )
   A64M:DSTACK-GPR ENGINE-GPR:DSTACK T=
   A64M:DSTACK-GPR 19 T=
   ENGINE-GPR:DSTACK ENGINE-ROUTES
   XREG-RBASE ENGINE-ROUTES
   DBASE ENGINE-ROUTES
   NDICT ENGINE-ROUTES
   CP ENGINE-ROUTES ;

\ Every register in the file, against the published reserved set: the
\ constructor accepts exactly the registers RESERVED-GPRS does not name, so the
\ refusal rule and the set a consumer plans against cannot disagree about a
\ single register.
: SWEEP-REG ( n -- )
   ER-REG !
   ER NEFF:GPR-REG NEFF:GPRS-N  1 ER lshift  T=
   1 ER lshift A64M:RESERVED-GPRS and 0<> if
      [: ER CLOBBERS-REG ;] E-NEFF-GPR TTHROWSQ
      exit then
   ER CLOBBERS-REG ;

: FILE-SWEEP ( -- )
   A64M:FILE-SIZE 0 ?do i SWEEP-REG loop ;

\ What the two derived sets and the writable set answer. The writable set is the
\ one an allocator hands registers out of, and it is asserted to hold the result
\ register - which the destroyed set deliberately cannot name, because one
\ register is not in two roles - as well as everything destroyed.
: DERIVED ( -- )
   LEAF NEFF:GPR-IN@ NEFF:GPRS-N 0 T=
   LEAF NEFF:GPR-RESULT@ NEFF:GPRS-N 0 T=
   LEAF NEFF:GPR-WRITABLE NEFF:GPRS-N 0 T=
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-IN@ NEFF:GPRS-N X0 NEFF:GPRS-N T=
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-RESULT@ NEFF:GPRS-N X1 NEFF:GPRS-N T=
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-WRITABLE
      X1 X2 NEFF:GPR-WITH NEFF:GPR-HAS? TTRUE
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-WRITABLE X0 NEFF:GPR-HAS? TFALSE
   0 1 SQ2 SQ-NONE X2 R-GPR NEFF:GPR-IN@
      X0 X1 NEFF:GPR-WITH NEFF:GPR-HAS? TTRUE ;

\ ---- 2c. one register cannot be returned and destroyed ------------------------
: ROLE-REJECTS ( -- )
   [: SQ-NONE 1 SQ X1 R-GPR DROP-ROUTINE ;] E-NEFF-ROLE TTHROWSQ
   [: SQ-NONE 0 1 SQ2 X1 R-GPR DROP-ROUTINE ;]
      E-NEFF-ROLE TTHROWSQ
   [: D0 D1 D1 R-FPR DROP-ROUTINE ;] E-NEFF-ROLE TTHROWSQ
   SQ-NONE 0 SQ X1 R-GPR DROP-ROUTINE
   D2 D0 D1 R-FPR DROP-ROUTINE ;

\ ---- 2d. the stack -----------------------------------------------------------
: STACK-REJECTS ( -- )
   [: NEFF-CONTROL:NO-RETURN -16 0 R-STACK DROP-ROUTINE ;]
      E-NEFF-FRAME TTHROWSQ
   [: NEFF-CONTROL:NO-RETURN 8 0 R-STACK DROP-ROUTINE ;]
      E-NEFF-FRAME TTHROWSQ
   [: NEFF-CONTROL:NO-RETURN A64M:FRAME-MAX A64M:SP-ALIGN + 0 R-STACK
      DROP-ROUTINE ;] E-NEFF-FRAME TTHROWSQ
   [: NEFF-CONTROL:NO-RETURN 16 16 R-STACK DROP-ROUTINE ;] E-NEFF-SP TTHROWSQ
   [: NEFF-CONTROL:NO-RETURN 32 -8 R-STACK DROP-ROUTINE ;] E-NEFF-SP TTHROWSQ
   [: NEFF-CONTROL:NO-RETURN 16 -32 R-STACK DROP-ROUTINE ;] E-NEFF-SP TTHROWSQ
   [: NEFF-CONTROL:RETURNS 32 -16 R-STACK DROP-ROUTINE ;] E-NEFF-SP TTHROWSQ
   [: NEFF-CONTROL:TAIL-CALL 32 -16 R-STACK DROP-ROUTINE ;] E-NEFF-SP TTHROWSQ
   NEFF-CONTROL:NO-RETURN 32 -32 R-STACK DROP-ROUTINE
   NEFF-CONTROL:RETURNS A64M:FRAME-MAX 0 R-STACK DROP-ROUTINE ;

\ ---- 2e. the link register ---------------------------------------------------
: LINK-REJECTS ( -- )
   [: NEFF-CONTROL:RETURNS NEFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-NEFF-LINK TTHROWSQ
   [: NEFF-CONTROL:TAIL-CALL NEFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-NEFF-LINK TTHROWSQ
   NEFF-CONTROL:NO-RETURN NEFF-LINK:CLOBBERED R-LINK DROP-ROUTINE
   NEFF-CONTROL:RETURNS NEFF-LINK:PRESERVED R-LINK DROP-ROUTINE ;

\ ---- 2f. a routine that never comes back delivers nothing ---------------------
: CONTROL-REJECTS ( -- )
   [: 0 SQ NEFF:FPR-NONE NEFF-NZCV:UNTOUCHED NEFF-CONTROL:NO-RETURN
      R-RESULT DROP-ROUTINE ;] E-NEFF-CONTROL TTHROWSQ
   [: SQ-NONE D0 NEFF-NZCV:UNTOUCHED NEFF-CONTROL:NO-RETURN
      R-RESULT DROP-ROUTINE ;] E-NEFF-CONTROL TTHROWSQ
   [: SQ-NONE NEFF:FPR-NONE NEFF-NZCV:DELIVERED
      NEFF-CONTROL:NO-RETURN R-RESULT DROP-ROUTINE ;]
      E-NEFF-CONTROL TTHROWSQ
   SQ-NONE NEFF:FPR-NONE NEFF-NZCV:CLOBBERED
      NEFF-CONTROL:NO-RETURN R-RESULT DROP-ROUTINE
   0 SQ D0 NEFF-NZCV:DELIVERED NEFF-CONTROL:RETURNS R-RESULT DROP-ROUTINE
   0 SQ D0 NEFF-NZCV:DELIVERED NEFF-CONTROL:TAIL-CALL R-RESULT DROP-ROUTINE ;

\ ---- 4. what is preserved is the complement ----------------------------------
\ The expected value is computed here from the whole legal file and the two sets
\ the case declares, so it is not a restatement of the module's arithmetic.
: WANT-PRESERVED ( NEFF:gprs NEFF:gprs -- n )
   {: res:gprs clob:gprs :}
   A64M:MACHINE NEFF:GPR-ALL NEFF:GPRS-N
   res NEFF:GPRS-N invert and
   clob NEFF:GPRS-N invert and ;

: PRESERVED ( -- )
   LEAF NEFF:GPR-PRESERVED NEFF:GPRS-N
      NEFF:GPR-NONE NEFF:GPR-NONE WANT-PRESERVED T=
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-PRESERVED NEFF:GPRS-N X1 X2 WANT-PRESERVED T=
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-PRESERVED X1 NEFF:GPR-HAS? TFALSE
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-PRESERVED X2 NEFF:GPR-HAS? TFALSE
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-PRESERVED X0 NEFF:GPR-HAS? TTRUE
   D0 D1 D2 R-FPR NEFF:FPR-PRESERVED NEFF:FPRS-N
      A64M:MACHINE NEFF:FPR-ALL NEFF:FPRS-N D1 NEFF:FPRS-N invert and
      D2 NEFF:FPRS-N invert and T=
   D0 D1 D2 R-FPR NEFF:FPR-PRESERVED D0 NEFF:FPR-HAS? TTRUE
   LEAF NEFF:RETURNS? TTRUE
   NEFF-CONTROL:TAIL-CALL 0 0 R-STACK NEFF:RETURNS? TTRUE
   NEFF-CONTROL:NO-RETURN 0 0 R-STACK NEFF:RETURNS? TFALSE ;

\ ---- frame slots -------------------------------------------------------------
\ The reach case needs a frame deeper than a narrow access can address: a
\ one-byte slot cannot name a byte past IMM12-LIM-1, however large the frame is.
: SLOTS ( -- )
   0 8 64 FRAMED NEFF:CHECK-SLOT
   56 8 64 FRAMED NEFF:CHECK-SLOT
   60 4 64 FRAMED NEFF:CHECK-SLOT
   63 1 64 FRAMED NEFF:CHECK-SLOT
   [: 0 2 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: 0 3 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: 0 0 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: -8 8 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: 4 8 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: 2 4 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: 64 8 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   [: 60 8 64 FRAMED NEFF:CHECK-SLOT ;] E-NEFF-SLOT TTHROWSQ
   1 A64M:SLOT-REACH 1 A64M:FRAME-MAX FRAMED NEFF:CHECK-SLOT
   [: 1 A64M:SLOT-REACH 1+ 1 A64M:FRAME-MAX FRAMED NEFF:CHECK-SLOT ;]
      E-NEFF-SLOT TTHROWSQ
   [: 4 A64M:SLOT-REACH 4 + 4 A64M:FRAME-MAX FRAMED NEFF:CHECK-SLOT ;]
      E-NEFF-SLOT TTHROWSQ
   \ the reach of a width is the MACHINE's answer now, so a width no load or
   \ store form of it moves is refused with the machine's code and not the
   \ schema's; CHECK-SLOT above still answers for the slot with E-NEFF-SLOT.
   [: 2 A64M:SLOT-REACH drop ;] E-NMACH TTHROWSQ
   [: 0 A64M:SLOT-REACH drop ;] E-NMACH TTHROWSQ ;

\ ---- 3. a forged contract carries no identity ---------------------------------
\ Assembled by the generated constructor with a stack delta a returning routine
\ cannot have, so every word that revalidates refuses it with that rule's code.
: FORGED ( -- NEFF:routine )
   NEFF-CONV:REGISTER SQ-NONE SQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 32 -16 A64M:MACHINE NEFF-ROUTINE:MAKE ;

\ A second forgery, this one naming the reserved register at argument position
\ zero of a list the checked constructor would never have accepted.
: FORGED-RESERVED ( -- NEFF:routine )
   NEFF-CONV:REGISTER
   1 60 lshift 30 or NEFF-PLACESEQ:MAKE SQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF-ROUTINE:MAKE ;

: FORGERY-REJECTS ( -- )
   [: FORGED NEFF:VALIDATE DROP-ROUTINE ;] E-NEFF-SP TTHROWSQ
   [: FORGED NEFF:DIGEST DROP-DIGEST ;] E-NEFF-SP TTHROWSQ
   [: FORGED NEFF:ENCODE drop drop ;] E-NEFF-SP TTHROWSQ
   [: FORGED LEAF NEFF:SAME? drop ;] E-NEFF-SP TTHROWSQ
   [: LEAF FORGED NEFF:SAME? drop ;] E-NEFF-SP TTHROWSQ
   [: FORGED NEFF:GPR-PRESERVED NEFF:GPRS-N drop ;] E-NEFF-SP TTHROWSQ
   [: FORGED NEFF:FPR-PRESERVED NEFF:FPRS-N drop ;] E-NEFF-SP TTHROWSQ
   [: FORGED NEFF:RETURNS? drop ;] E-NEFF-SP TTHROWSQ
   [: 0 8 FORGED NEFF:CHECK-SLOT ;] E-NEFF-SP TTHROWSQ
   [: FORGED-RESERVED NEFF:VALIDATE DROP-ROUTINE ;] E-NEFF-GPR TTHROWSQ
   [: FORGED-RESERVED NEFF:DIGEST DROP-DIGEST ;] E-NEFF-GPR TTHROWSQ
   [: FORGED-RESERVED NEFF:GPR-PRESERVED NEFF:GPRS-N drop ;]
      E-NEFF-GPR TTHROWSQ
   [: FORGED-RESERVED NEFF:GPR-IN@ NEFF:GPRS-N drop ;] E-NEFF-GPR TTHROWSQ
   [: FORGED-RESERVED NEFF:GPR-WRITABLE NEFF:GPRS-N drop ;]
      E-NEFF-GPR TTHROWSQ ;

\ A field reader only projects, so it answers about a forged record without
\ pretending the record is declarable. That is the documented split, and it is
\ pinned here so a later "helpful" revalidation in a reader is a visible change.
: READERS ( -- )
   FORGED NEFF:FRAME@ 32 T=
   FORGED NEFF:DELTA@ -16 T=
   FORGED-RESERVED NEFF:ARGS@ NEFF-PLACESEQ:UNMAKE
      1 60 lshift 30 or T=
   0 SQ 1 SQ X2 R-GPR NEFF:ARGS@ 0 NEFF:SEQ-REG@ 0 T=
   0 SQ 1 SQ X2 R-GPR NEFF:RESULTS@ 0 NEFF:SEQ-REG@ 1 T=
   0 SQ 1 SQ X2 R-GPR NEFF:GPR-CLOBBER@ NEFF:GPRS-N X2 NEFF:GPRS-N T=
   D0 D1 D2 R-FPR NEFF:FPR-IN@ NEFF:FPRS-N D0 NEFF:FPRS-N T=
   D0 D1 D2 R-FPR NEFF:FPR-RESULT@ NEFF:FPRS-N D1 NEFF:FPRS-N T=
   D0 D1 D2 R-FPR NEFF:FPR-CLOBBER@ NEFF:FPRS-N D2 NEFF:FPRS-N T=
   LEAF NEFF:NZCV@ NEFF-NZCV:UNTOUCHED NEFF-NZCV:EQ TTRUE
   LEAF NEFF:LINK@ NEFF-LINK:PRESERVED NEFF-LINK:EQ TTRUE
   LEAF NEFF:CONTROL@ NEFF-CONTROL:RETURNS NEFF-CONTROL:EQ TTRUE
   LEAF NEFF:TRAITS@ NEFF:TRAITS-N 0 T= ;

\ ---- 2g. a missing or role-swapped argument never reaches runtime -------------
\ -1 is accepted by the checker, 0 refused.
: STATIC-REJECTS ( -- )
   s" A64T-OK ( NEFF:conv NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SHORT ( NEFF:conv NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-FILES ( NEFF:conv NEFF:fprs NEFF:fprs NEFF:fprs NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SWAP ( NEFF:conv NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:link NEFF:nzcv NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-BARE ( NEFF:conv n NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-NOCONV ( NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-CONVSEQ ( NEFF:placeseq NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SETOK ( n -- NEFF:gprs ) NEFF:GPR-SET"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SETX ( n -- NEFF:fprs ) NEFF:GPR-SET"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-WITHX ( NEFF:gprs NEFF:fprs -- NEFF:gprs ) NEFF:GPR-WITH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SETS ( NEFF:conv NEFF:gprs NEFF:gprs NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n -- NEFF:routine ) A64M:MACHINE NEFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SEQOK ( NEFF:placeseq n -- NEFF:placeseq ) NEFF:SEQ-WITH"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SEQSET ( NEFF:gprs n -- NEFF:placeseq ) NEFF:SEQ-WITH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SEQOUT ( NEFF:placeseq -- NEFF:placeseq ) NEFF:SEQ-SET"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SLOTOK ( n n NEFF:routine -- ) NEFF:CHECK-SLOT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SLOTSWAP ( NEFF:routine n n -- ) NEFF:CHECK-SLOT"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- 5a. the canonical preimage ----------------------------------------------
: PREIMAGE ( -- )
   0 SQ 1 SQ X2 R-GPR NEFF:ENCODE {: base:ptr len:n :}
   len 128 T=
   base 0 CDIGEST:SLOT@ CDIGEST:TAG-A64-ROUTINE T=
   base 1 CDIGEST:SLOT@ 5 T=
   base 2 CDIGEST:SLOT@ 1 T=
   base 3 CDIGEST:SLOT@ 0 SQ NEFF-PLACESEQ:UNMAKE T=
   base 4 CDIGEST:SLOT@ 1 SQ NEFF-PLACESEQ:UNMAKE T=
   base 5 CDIGEST:SLOT@ X2 NEFF:GPRS-N T=
   base 6 CDIGEST:SLOT@ 0 T=
   base 7 CDIGEST:SLOT@ 0 T=
   base 8 CDIGEST:SLOT@ 0 T=
   base 9 CDIGEST:SLOT@ 0 T=
   base 10 CDIGEST:SLOT@ 0 T=
   base 11 CDIGEST:SLOT@ 0 T=
   base 12 CDIGEST:SLOT@ 0 T=
   base 13 CDIGEST:SLOT@ 0 T=
   base 14 CDIGEST:SLOT@ 0 T=
   base 15 CDIGEST:SLOT@ A64M:MACHINE NMACH:MARK T=
   0 DQ 1 DQ X2 R-DSTACK NEFF:ENCODE {: db:ptr dlen:n :}
   db 2 CDIGEST:SLOT@ 0 T=
   NEFF-CONTROL:NO-RETURN 32 -32 R-STACK NEFF:ENCODE {: nb:ptr nlen:n :}
   nb 11 CDIGEST:SLOT@ 2 T=
   nb 13 CDIGEST:SLOT@ 32 T=
   nb 14 CDIGEST:SLOT@ -32 T=
   SQ-NONE NEFF:FPR-NONE NEFF-NZCV:READ-CLOBBERED
      NEFF-CONTROL:RETURNS R-RESULT NEFF:ENCODE {: zb:ptr zlen:n :}
   zb 9 CDIGEST:SLOT@ 4 T=
   NEFF-CONTROL:NO-RETURN NEFF-LINK:CLOBBERED R-LINK
      NEFF:ENCODE {: lb:ptr llen:n :}
   lb 10 CDIGEST:SLOT@ 1 T= ;

\ The guard that keeps the digest load-bearing: the record's DECLARED field
\ count, read back out of the type registry, is tied to the width of its
\ canonical preimage. Adding a field and forgetting to encode it therefore
\ reddens this suite instead of quietly producing a record that can change a
\ compilation while keeping its old identity.
: ROUTINE$ ( -- ptr u8 n ptr u8 n )  s" routine" s" NEFF-ROUTINE" ;
: GPRS$ ( -- ptr u8 n ptr u8 n )     s" gprs" s" NEFF-GPRS" ;
: PLACESEQ$ ( -- ptr u8 n ptr u8 n ) s" placeseq" s" NEFF-PLACESEQ" ;
: FPRS$ ( -- ptr u8 n ptr u8 n )     s" fprs" s" NEFF-FPRS" ;
: TRAITS$ ( -- ptr u8 n ptr u8 n )   s" traits" s" NEFF-TRAITS" ;

: SLOTS-OF ( ptr u8 n -- n )
   nip CDIGEST:SLOT-BYTES / ;

: SCHEMA-PINS ( -- )
   PLACESEQ$ REFLECT:FAMS 1 T=
   PLACESEQ$ REFLECT:FLDS 1 T=
   PLACESEQ$ REFLECT:WIDTH 1 T=
   ROUTINE$ REFLECT:FAMS 1 T=
   ROUTINE$ REFLECT:FLDS 14 T=
   ROUTINE$ REFLECT:WIDTH 14 T=
   LEAF NEFF:ENCODE SLOTS-OF ROUTINE$ REFLECT:FLDS 2 + T=
   GPRS$ REFLECT:FAMS 1 T=
   GPRS$ REFLECT:FLDS 1 T=
   GPRS$ REFLECT:WIDTH 1 T=
   FPRS$ REFLECT:FAMS 1 T=
   FPRS$ REFLECT:FLDS 1 T=
   TRAITS$ REFLECT:FAMS 1 T=
   TRAITS$ REFLECT:FLDS 1 T=
   s" nzcv" s" NEFF-NZCV" REFLECT:VARS 5 T=
   s" link" s" NEFF-LINK" REFLECT:VARS 3 T=
   s" control" s" NEFF-CONTROL" REFLECT:VARS 3 T= ;

\ The tag is what keeps a routine contract's digest out of every other compiler
\ record's space, so it is asserted to be its own value and not a neighbour's.
: DOMAIN-SEPARATION ( -- )
   CDIGEST:TAG-A64-ROUTINE 6 T=
   CDIGEST:TAG-A64-ROUTINE CDIGEST:TAG-TARGET = TFALSE
   CDIGEST:TAG-A64-ROUTINE CDIGEST:TAG-NUMERIC = TFALSE
   CDIGEST:TAG-A64-ROUTINE CDIGEST:TAG-BINDING = TFALSE
   CDIGEST:TAG-A64-ROUTINE CDIGEST:TAG-SCHEMA = TFALSE
   CDIGEST:TAG-A64-ROUTINE CDIGEST:TAG-SCHEMA-TABLE = TFALSE ;

\ ---- 5b. the enumeration -----------------------------------------------------
\ Sweep A varies the six register-role sets and the whole flag family over a
\ returning leaf: 2^6 * 5 = 320 rows. Sweep B varies the eight trait masks over
\ nine legal shapes of (link, control, frame, delta): 72 rows. No sweep-B shape
\ is sweep A's fixed shape, so the two are disjoint and the whole enumeration is
\ injective - which is what lets the pairwise check assert that two rows compare
\ equal exactly when they are the same row.

320 constant SWEEP-A
72 constant SWEEP-B
SWEEP-A SWEEP-B + constant ROWS
392 constant ROWS-EXPECTED

create DGA ROWS 4 * cells allot

: DG! ( n n n n n -- )
   {: w0:n w1:n w2:n w3:n idx:n :}
   idx 4 * {: at:n :}
   w0 DGA at cells + !
   w1 DGA at 1+ cells + !
   w2 DGA at 2 + cells + !
   w3 DGA at 3 + cells + ! ;

: DG= ( n n -- bool )
   {: x:n y:n :}
   x 4 * {: px:n :}
   y 4 * {: py:n :}
   DGA px cells + @      DGA py cells + @ =
   DGA px 1+ cells + @   DGA py 1+ cells + @ = and
   DGA px 2 + cells + @  DGA py 2 + cells + @ = and
   DGA px 3 + cells + @  DGA py 3 + cells + @ = and ;

: TBOOL= ( bool bool -- )
   {: got:bool want:bool :}
   want if got TTRUE exit then
   got TFALSE ;

\ Sweep A projections. The index is a mixed-radix number, flag family fastest.
: A-NZCV ( n -- NEFF:nzcv )
   5 mod {: k:n :}
   k 0= if NEFF-NZCV:UNTOUCHED exit then
   k 1 = if NEFF-NZCV:CLOBBERED exit then
   k 2 = if NEFF-NZCV:DELIVERED exit then
   k 3 = if NEFF-NZCV:READ-PRESERVED exit then
   NEFF-NZCV:READ-CLOBBERED ;

: BIT-AT ( n n -- n )   \ index, position -> 0 or 1
   {: a:n p:n :}
   a 5 / p rshift 1 and ;

: A-GPR ( n n n -- NEFF:gprs )   \ index, bit position, register
   {: a:n p:n r:n :}
   a p BIT-AT 0= if NEFF:GPR-NONE exit then
   r NEFF:GPR-REG ;

\ The same for an interface position: either the routine declares one there or
\ it declares none.
: A-SEQ ( n n n -- NEFF:placeseq )   \ index, bit position, register
   {: a:n p:n r:n :}
   a p BIT-AT 0= if SQ-NONE exit then
   r SQ ;

: A-FPR ( n n n -- NEFF:fprs )
   {: a:n p:n r:n :}
   a p BIT-AT 0= if NEFF:FPR-NONE exit then
   r NEFF:FPR-REG ;

: SWEEP-A>ROUTINE ( n -- NEFF:routine )
   {: a:n :}
   NEFF-CONV:REGISTER
   a 5 0 A-SEQ  a 4 1 A-SEQ  a 3 2 A-GPR
   a 2 0 A-FPR  a 1 1 A-FPR  a 0 2 A-FPR
   a A-NZCV NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 16 0 A64M:MACHINE NEFF:ROUTINE ;

\ Sweep B shapes: link, control, frame, delta. None is (preserved, returns, 16,
\ 0), which is sweep A's fixed shape.
: B-LINK ( n -- NEFF:link )
   6 < if NEFF-LINK:PRESERVED exit then NEFF-LINK:CLOBBERED ;

: B-CONTROL ( n -- NEFF:control )
   {: s:n :}
   s 3 < if NEFF-CONTROL:RETURNS exit then
   s 3 = if NEFF-CONTROL:TAIL-CALL exit then
   NEFF-CONTROL:NO-RETURN ;

: B-FRAME ( n -- n )
   {: s:n :}
   s 0= if 0 exit then
   s 1 = if 32 exit then
   s 2 = if 48 exit then
   s 7 = if 32 exit then
   s 8 = if 0 exit then
   16 ;

: B-DELTA ( n -- n )
   {: s:n :}
   s 5 = if -16 exit then
   s 7 = if -32 exit then
   0 ;

: SWEEP-B>ROUTINE ( n -- NEFF:routine )
   {: b:n :}
   b 9 mod {: s:n :}
   b 9 / {: t:n :}
   NEFF-CONV:REGISTER SQ-NONE SQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED s B-LINK s B-CONTROL
   t NEFF:TRAIT-SET s B-FRAME s B-DELTA A64M:MACHINE NEFF:ROUTINE ;

: IX>ROUTINE ( n -- NEFF:routine )
   dup SWEEP-A < if SWEEP-A>ROUTINE exit then
   SWEEP-A - SWEEP-B>ROUTINE ;

: COLLECT ( -- )
   ROWS ROWS-EXPECTED T=
   ROWS 0 ?do
      i IX>ROUTINE NEFF:DIGEST CDIGEST-DIGEST:UNMAKE i DG!
   loop ;

\ Two contracts compare equal exactly when they digest equal, and - because the
\ enumeration is injective - exactly when they are the same row. The second
\ clause is what makes this a distinctness proof and not just a consistency one.
: PAIRWISE ( -- )
   ROWS ROWS * 0 ?do
      i ROWS / {: x:n :}
      i ROWS mod {: y:n :}
      x IX>ROUTINE y IX>ROUTINE NEFF:SAME? {: same:bool :}
      x y DG= same TBOOL=
      same x y = TBOOL=
   loop ;

\ A record built twice by separate calls is the same record and the same digest.
: STABLE ( -- )
   0 SQ 1 SQ X2 R-GPR 0 SQ 1 SQ X2 R-GPR NEFF:SAME? TTRUE
   0 SQ 1 SQ X2 R-GPR NEFF:DIGEST
   0 SQ 1 SQ X2 R-GPR NEFF:DIGEST CDIGEST-DIGEST:EQ TTRUE
   LEAF 0 SQ 1 SQ X2 R-GPR NEFF:SAME? TFALSE
   LEAF NEFF:DIGEST LEAF NEFF:DIGEST CDIGEST-DIGEST:EQ TTRUE ;

public

: RUN ( -- )
   T-RESET
   MACHINE-FACTS
   VOCABULARY
   ALGEBRA
   SEQUENCE
   PLACES
   STACK-RANGES
   CONV-CASES
   ENGINE-RESERVED
   FILE-SWEEP
   DERIVED
   ROLE-REJECTS
   STACK-REJECTS
   LINK-REJECTS
   CONTROL-REJECTS
   PRESERVED
   SLOTS
   FORGERY-REJECTS
   READERS
   STATIC-REJECTS
   PREIMAGE
   SCHEMA-PINS
   DOMAIN-SEPARATION
   STABLE
   COLLECT
   PAIRWISE
   T-REPORT ;

;using
;package

NEFF-TEST:RUN
