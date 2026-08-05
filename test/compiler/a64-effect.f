\ a64-effect.f - acceptance suite for the typed ARM64 routine-effect schema.
\
\ Covers src/compiler/a64-effect.f through its public production words. Five
\ things are owed and each is proved rather than sampled:
\
\ 1. THE BOUNDS ARE THE ASSEMBLER'S. The schema states how many registers a file
\    holds, which register is platform-reserved, and how far a frame slot can sit
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
require src/compiler/a64-effect.f
require src/arch/arm64/asm.f

package A64EFF-TEST
private

\ ---- consuming contract values -----------------------------------------------
\ A contract is twelve stack cells, so a test that only wants the throw has to
\ unmake what it built.
: DROP-ROUTINE ( A64EFF:routine -- )
   A64EFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop ;

: DROP-DIGEST ( CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE drop drop drop drop ;

\ ---- named register sets -----------------------------------------------------
: X0 ( -- A64EFF:gprs )   0 A64EFF:GPR-REG ;
: X1 ( -- A64EFF:gprs )   1 A64EFF:GPR-REG ;
: X2 ( -- A64EFF:gprs )   2 A64EFF:GPR-REG ;
: D0 ( -- A64EFF:fprs )   0 A64EFF:FPR-REG ;
: D1 ( -- A64EFF:fprs )   1 A64EFF:FPR-REG ;
: D2 ( -- A64EFF:fprs )   2 A64EFF:FPR-REG ;

\ ---- named register lists ----------------------------------------------------
\ One position, and two, so a case can say which register arrives where without
\ spelling the packing out.
: SQ ( n -- A64EFF:placeseq )
   A64EFF:SEQ-NONE swap A64EFF:SEQ-WITH ;

: SQ2 ( n n -- A64EFF:placeseq )
   {: a:n b:n :}
   a SQ b A64EFF:SEQ-WITH ;

: SQ-NONE ( -- A64EFF:placeseq )   A64EFF:SEQ-NONE ;

\ ---- forged set values -------------------------------------------------------
\ A set the generated constructor assembled without passing the checked one, so
\ it names something the schema says no routine can hold state in.
: FORGED-GPRS ( -- A64EFF:gprs )     1 18 lshift A64EFF-GPRS:MAKE ;
: FORGED-FPRS ( -- A64EFF:fprs )     1 32 lshift A64EFF-FPRS:MAKE ;
: FORGED-TRAITS ( -- A64EFF:traits ) 8 A64EFF-TRAITS:MAKE ;

\ ---- contract shorthands -----------------------------------------------------
\ Each fixes the fields a case is not about, so a case reads as the one fact it
\ is making.

: R-GPR ( A64EFF:placeseq A64EFF:placeseq A64EFF:gprs -- A64EFF:routine )
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: R-FPR ( A64EFF:fprs A64EFF:fprs A64EFF:fprs -- A64EFF:routine )
   {: fi:fprs fr:fprs fc:fprs :}
   SQ-NONE SQ-NONE A64EFF:GPR-NONE fi fr fc
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: R-STACK ( A64EFF:control n n -- A64EFF:routine )
   {: c:control size:n delta:n :}
   SQ-NONE SQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED c
   A64EFF:TRAITS-NONE size delta A64EFF:ROUTINE ;

: R-LINK ( A64EFF:control A64EFF:link -- A64EFF:routine )
   {: c:control l:link :}
   SQ-NONE SQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED l c A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: R-RESULT ( A64EFF:placeseq A64EFF:fprs A64EFF:nzcv A64EFF:control -- A64EFF:routine )
   {: gr:placeseq fr:fprs z:nzcv c:control :}
   SQ-NONE gr A64EFF:GPR-NONE
   A64EFF:FPR-NONE fr A64EFF:FPR-NONE
   z A64EFF-LINK:PRESERVED c A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ A leaf that touches nothing: the neutral contract every case starts from.
: LEAF ( -- A64EFF:routine )
   SQ-NONE SQ-NONE A64EFF:GPR-NONE R-GPR ;

\ A routine with a frame, for the slot cases.
: FRAMED ( n -- A64EFF:routine )
   {: size:n :}
   A64EFF-CONTROL:RETURNS size 0 R-STACK ;

\ ---- 1. the bounds are the assembler's ---------------------------------------
\ REG-LIM, ARM-RESERVED-REG and IMM12-LIM are the shipped encoder's own
\ constants. Every schema bound is stated as a function of one of them.
: MACHINE-FACTS ( -- )
   A64EFF:FILE-SIZE REG-LIM T=
   A64EFF:RESERVED-GPR ARM-RESERVED-REG T=
   A64EFF:ZERO-GPR REG-LIM 1- T=
   A64EFF:LINK-GPR 30 T=
   A64EFF:SP-ALIGN 16 T=
   8 A64EFF:SLOT-REACH IMM12-LIM 1- 8 * T=
   4 A64EFF:SLOT-REACH IMM12-LIM 1- 4 * T=
   1 A64EFF:SLOT-REACH IMM12-LIM 1- T=
   8 A64EFF:SLOT-REACH dup A64EFF:SP-ALIGN mod - A64EFF:FRAME-MAX T=
   A64EFF:GPR-ALL A64EFF:GPRS-N $3FF3FFFF T=
   A64EFF:FPR-ALL A64EFF:FPRS-N $FFFFFFFF T= ;

\ ---- 2a. the register vocabulary ---------------------------------------------
\ The three registers a general set may not name, each because another owner has
\ the fact, plus a bit past the file. The floating file has no reserved member,
\ so its whole width is nameable and only a bit past the file is refused.
: VOCABULARY ( -- )
   A64EFF:GPR-NONE A64EFF:GPRS-N 0 T=
   0 A64EFF:GPR-SET A64EFF:GPRS-N 0 T=
   X0 A64EFF:GPRS-N 1 T=
   [: 1 18 lshift A64EFF:GPR-SET A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 1 19 lshift A64EFF:GPR-SET A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 1 30 lshift A64EFF:GPR-SET A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 1 31 lshift A64EFF:GPR-SET A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 1 32 lshift A64EFF:GPR-SET A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 18 A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 19 A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 30 A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 31 A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 32 A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: -1 A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   17 A64EFF:GPR-REG A64EFF:GPRS-N 1 17 lshift T=
   20 A64EFF:GPR-REG A64EFF:GPRS-N 1 20 lshift T=
   29 A64EFF:GPR-REG A64EFF:GPRS-N 1 29 lshift T=
   18 A64EFF:FPR-REG A64EFF:FPRS-N 1 18 lshift T=
   31 A64EFF:FPR-REG A64EFF:FPRS-N 1 31 lshift T=
   [: 1 32 lshift A64EFF:FPR-SET A64EFF:FPRS-N drop ;] E-A64EFF-FPR TTHROWSQ
   [: 32 A64EFF:FPR-REG A64EFF:FPRS-N drop ;] E-A64EFF-FPR TTHROWSQ
   [: -1 A64EFF:FPR-REG A64EFF:FPRS-N drop ;] E-A64EFF-FPR TTHROWSQ ;

\ ---- 2b. set algebra ---------------------------------------------------------
: ALGEBRA ( -- )
   X0 X1 A64EFF:GPR-WITH A64EFF:GPRS-N 3 T=
   X0 X1 A64EFF:GPR-WITH X0 A64EFF:GPR-WITHOUT A64EFF:GPRS-N 2 T=
   X0 X1 A64EFF:GPR-WITH X0 A64EFF:GPR-HAS? TTRUE
   X0 X1 A64EFF:GPR-HAS? TFALSE
   X0 A64EFF:GPR-NONE A64EFF:GPR-HAS? TTRUE
   D0 D1 A64EFF:FPR-WITH A64EFF:FPRS-N 3 T=
   D0 D1 A64EFF:FPR-WITH D1 A64EFF:FPR-WITHOUT A64EFF:FPRS-N 1 T=
   D0 D1 A64EFF:FPR-WITH D1 A64EFF:FPR-HAS? TTRUE
   D0 D1 A64EFF:FPR-HAS? TFALSE
   A64EFF:TRAITS-NONE A64EFF:TRAITS-N 0 T=
   A64EFF:T-CALL A64EFF:T-SYSCALL A64EFF:TRAITS-WITH A64EFF:TRAITS-N 5 T=
   A64EFF:T-CALL A64EFF:T-SYSCALL A64EFF:TRAITS-WITH A64EFF:T-CALL A64EFF:TRAITS-HAS? TTRUE
   A64EFF:T-CALL A64EFF:T-INDIRECT A64EFF:TRAITS-HAS? TFALSE
   [: 8 A64EFF:TRAIT-SET A64EFF:TRAITS-N drop ;] E-A64EFF-TRAIT TTHROWSQ
   [: -1 A64EFF:TRAIT-SET A64EFF:TRAITS-N drop ;] E-A64EFF-TRAIT TTHROWSQ
   [: FORGED-GPRS X0 A64EFF:GPR-WITH A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: FORGED-GPRS X0 A64EFF:GPR-WITHOUT A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: X0 FORGED-GPRS A64EFF:GPR-WITHOUT A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: FORGED-GPRS X0 A64EFF:GPR-HAS? drop ;] E-A64EFF-GPR TTHROWSQ
   [: X0 FORGED-GPRS A64EFF:GPR-HAS? drop ;] E-A64EFF-GPR TTHROWSQ
   [: FORGED-FPRS D0 A64EFF:FPR-WITH A64EFF:FPRS-N drop ;] E-A64EFF-FPR TTHROWSQ
   [: FORGED-FPRS D0 A64EFF:FPR-WITHOUT A64EFF:FPRS-N drop ;] E-A64EFF-FPR TTHROWSQ
   [: FORGED-FPRS D0 A64EFF:FPR-HAS? drop ;] E-A64EFF-FPR TTHROWSQ
   [: FORGED-TRAITS A64EFF:T-CALL A64EFF:TRAITS-WITH A64EFF:TRAITS-N drop ;]
      E-A64EFF-TRAIT TTHROWSQ
   [: FORGED-TRAITS A64EFF:T-CALL A64EFF:TRAITS-HAS? drop ;] E-A64EFF-TRAIT TTHROWSQ
   [: A64EFF:T-CALL FORGED-TRAITS A64EFF:TRAITS-HAS? drop ;] E-A64EFF-TRAIT TTHROWSQ ;

\ ---- 2b2. the ordered register list -------------------------------------------
\ A list of as many positions as one can hold, so the limit is reached by
\ appending rather than by a number written here.
: LONG-SEQ ( n -- A64EFF:placeseq )
   {: n:n :}
   SQ-NONE
   n 0 ?do i A64EFF:SEQ-WITH loop ;

: SEQUENCE ( -- )
   SQ-NONE A64EFF:SEQ-LEN 0 T=
   SQ-NONE A64EFF:SEQ-SET A64EFF:GPRS-N 0 T=
   0 SQ A64EFF:SEQ-LEN 1 T=
   0 SQ 0 A64EFF:SEQ-REG@ 0 T=
   0 1 SQ2 A64EFF:SEQ-LEN 2 T=
   0 1 SQ2 0 A64EFF:SEQ-REG@ 0 T=
   0 1 SQ2 1 A64EFF:SEQ-REG@ 1 T=
   2 0 SQ2 0 A64EFF:SEQ-REG@ 2 T=
   2 0 SQ2 1 A64EFF:SEQ-REG@ 0 T=
   0 1 SQ2 A64EFF:SEQ-SET A64EFF:GPRS-N X0 X1 A64EFF:GPR-WITH A64EFF:GPRS-N T=
   2 0 SQ2 A64EFF:SEQ-SET A64EFF:GPRS-N X0 X2 A64EFF:GPR-WITH A64EFF:GPRS-N T=
   A64EFF:SEQ-LIMIT LONG-SEQ A64EFF:SEQ-LEN A64EFF:SEQ-LIMIT T=
   A64EFF:SEQ-LIMIT LONG-SEQ A64EFF:SEQ-LIMIT 1- A64EFF:SEQ-REG@ A64EFF:SEQ-LIMIT 1- T=
   17 SQ 0 A64EFF:SEQ-REG@ 17 T=
   29 SQ 0 A64EFF:SEQ-REG@ 29 T=
   [: 0 0 SQ2 A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 2 1 SQ2 1 A64EFF:SEQ-WITH A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 18 SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   [: 30 SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   [: 31 SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   [: 32 SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   [: -1 SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   [: A64EFF:SEQ-LIMIT 1+ LONG-SEQ A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: SQ-NONE 0 A64EFF:SEQ-REG@ drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 0 SQ 1 A64EFF:SEQ-REG@ drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 0 SQ -1 A64EFF:SEQ-REG@ drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 1 A64EFF-PLACESEQ:MAKE A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 15 60 lshift A64EFF-PLACESEQ:MAKE A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: 1 60 lshift 18 or A64EFF-PLACESEQ:MAKE A64EFF:SEQ-LEN drop ;]
      E-A64EFF-GPR TTHROWSQ
   [: -1 A64EFF-PLACESEQ:MAKE A64EFF:SEQ-SET A64EFF:GPRS-N drop ;]
      E-A64EFF-SEQ TTHROWSQ ;

\ ---- 2b3. the other kind of place --------------------------------------------
\ A position can name a slot of the caller's data stack instead of a register,
\ which is what design section 7.6's convention needs. Four things are owed: the
\ two kinds are told apart, a payload read as the wrong kind is refused rather
\ than answered with a number that would read as the other one, a slot is not a
\ register anywhere a set is derived, and one register and one slot with the same
\ number are two different places rather than a repeat.
: DQ ( n -- A64EFF:placeseq )
   SQ-NONE swap A64EFF:SEQ-WITH-SLOT ;

: DQ2 ( n n -- A64EFF:placeseq )
   {: a:n b:n :}
   a DQ b A64EFF:SEQ-WITH-SLOT ;

: PLACES ( -- )
   0 DQ A64EFF:SEQ-LEN 1 T=
   0 DQ 0 A64EFF:SEQ-SLOT@ 0 T=
   0 DQ A64EFF:SEQ-SLOTS 1 T=
   0 DQ 0 A64EFF:SEQ-KIND@ A64EFF-PKIND:DSLOT A64EFF-PKIND:EQ TTRUE
   0 SQ 0 A64EFF:SEQ-KIND@ A64EFF-PKIND:GPR A64EFF-PKIND:EQ TTRUE
   0 1 DQ2 A64EFF:SEQ-LEN 2 T=
   0 1 DQ2 0 A64EFF:SEQ-SLOT@ 0 T=
   0 1 DQ2 1 A64EFF:SEQ-SLOT@ 1 T=
   0 1 DQ2 A64EFF:SEQ-SLOTS 2 T=
   0 1 SQ2 A64EFF:SEQ-SLOTS 0 T=
   \ a data-stack place is in no register set, so a routine that takes everything
   \ off the stack reads and returns no register on account of its convention
   0 1 DQ2 A64EFF:SEQ-SET A64EFF:GPRS-N 0 T=
   0 DQ 0 SQ X2 R-GPR A64EFF:GPR-IN@ A64EFF:GPRS-N 0 T=
   0 SQ 0 DQ X2 R-GPR A64EFF:GPR-RESULT@ A64EFF:GPRS-N 0 T=
   \ one register and one slot with the same number are two places, not a repeat
   SQ-NONE 0 A64EFF:SEQ-WITH 0 A64EFF:SEQ-WITH-SLOT A64EFF:SEQ-LEN 2 T=
   A64EFF:SEQ-SLOT-LIMIT DQ 0 A64EFF:SEQ-SLOT@ A64EFF:SEQ-SLOT-LIMIT T=
   \ and the refusals
   [: 0 DQ 0 A64EFF:SEQ-REG@ drop ;] E-A64EFF-KIND TTHROWSQ
   [: 0 SQ 0 A64EFF:SEQ-SLOT@ drop ;] E-A64EFF-KIND TTHROWSQ
   [: 0 0 DQ2 A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: A64EFF:SEQ-SLOT-LIMIT 1+ DQ A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   [: -1 DQ A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ ;

\ ---- 2b4. the data-stack register is unbuildable ------------------------------
\ The engine keeps the running data-stack pointer in one register, and no routine
\ this schema can describe may hold state there: it is out of the general-register
\ mask exactly as x18, x30 and 31 are, so every route into a contract refuses it
\ and there is no contract that hands it out to be allocated from. Each of the
\ five routes is tried, because a check that only closes the door a caller
\ happens to use is not a closed door.
: DSTACK-RESERVED ( -- )
   A64EFF:DSTACK-GPR 19 T=
   A64EFF:GPR-ALL A64EFF:GPRS-N  1 A64EFF:DSTACK-GPR lshift and  0 T=
   [: A64EFF:DSTACK-GPR A64EFF:GPR-REG A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 1 A64EFF:DSTACK-GPR lshift A64EFF:GPR-SET A64EFF:GPRS-N drop ;]
      E-A64EFF-GPR TTHROWSQ
   [: A64EFF:DSTACK-GPR SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   [: SQ-NONE SQ-NONE  1 A64EFF:DSTACK-GPR lshift A64EFF-GPRS:MAKE  R-GPR
      DROP-ROUTINE ;] E-A64EFF-GPR TTHROWSQ
   [: SQ-NONE SQ-NONE  1 A64EFF:DSTACK-GPR lshift A64EFF-GPRS:MAKE  R-GPR
      A64EFF:GPR-WRITABLE A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ ;

\ What the two derived sets and the writable set answer. The writable set is the
\ one an allocator hands registers out of, and it is asserted to hold the result
\ register - which the destroyed set deliberately cannot name, because one
\ register is not in two roles - as well as everything destroyed.
: DERIVED ( -- )
   LEAF A64EFF:GPR-IN@ A64EFF:GPRS-N 0 T=
   LEAF A64EFF:GPR-RESULT@ A64EFF:GPRS-N 0 T=
   LEAF A64EFF:GPR-WRITABLE A64EFF:GPRS-N 0 T=
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-IN@ A64EFF:GPRS-N X0 A64EFF:GPRS-N T=
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-RESULT@ A64EFF:GPRS-N X1 A64EFF:GPRS-N T=
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-WRITABLE
      X1 X2 A64EFF:GPR-WITH A64EFF:GPR-HAS? TTRUE
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-WRITABLE X0 A64EFF:GPR-HAS? TFALSE
   0 1 SQ2 SQ-NONE X2 R-GPR A64EFF:GPR-IN@
      X0 X1 A64EFF:GPR-WITH A64EFF:GPR-HAS? TTRUE ;

\ ---- 2c. one register cannot be returned and destroyed ------------------------
: ROLE-REJECTS ( -- )
   [: SQ-NONE 1 SQ X1 R-GPR DROP-ROUTINE ;] E-A64EFF-ROLE TTHROWSQ
   [: SQ-NONE 0 1 SQ2 X1 R-GPR DROP-ROUTINE ;]
      E-A64EFF-ROLE TTHROWSQ
   [: D0 D1 D1 R-FPR DROP-ROUTINE ;] E-A64EFF-ROLE TTHROWSQ
   SQ-NONE 0 SQ X1 R-GPR DROP-ROUTINE
   D2 D0 D1 R-FPR DROP-ROUTINE ;

\ ---- 2d. the stack -----------------------------------------------------------
: STACK-REJECTS ( -- )
   [: A64EFF-CONTROL:NO-RETURN -16 0 R-STACK DROP-ROUTINE ;]
      E-A64EFF-FRAME TTHROWSQ
   [: A64EFF-CONTROL:NO-RETURN 8 0 R-STACK DROP-ROUTINE ;]
      E-A64EFF-FRAME TTHROWSQ
   [: A64EFF-CONTROL:NO-RETURN A64EFF:FRAME-MAX A64EFF:SP-ALIGN + 0 R-STACK
      DROP-ROUTINE ;] E-A64EFF-FRAME TTHROWSQ
   [: A64EFF-CONTROL:NO-RETURN 16 16 R-STACK DROP-ROUTINE ;] E-A64EFF-SP TTHROWSQ
   [: A64EFF-CONTROL:NO-RETURN 32 -8 R-STACK DROP-ROUTINE ;] E-A64EFF-SP TTHROWSQ
   [: A64EFF-CONTROL:NO-RETURN 16 -32 R-STACK DROP-ROUTINE ;] E-A64EFF-SP TTHROWSQ
   [: A64EFF-CONTROL:RETURNS 32 -16 R-STACK DROP-ROUTINE ;] E-A64EFF-SP TTHROWSQ
   [: A64EFF-CONTROL:TAIL-CALL 32 -16 R-STACK DROP-ROUTINE ;] E-A64EFF-SP TTHROWSQ
   A64EFF-CONTROL:NO-RETURN 32 -32 R-STACK DROP-ROUTINE
   A64EFF-CONTROL:RETURNS A64EFF:FRAME-MAX 0 R-STACK DROP-ROUTINE ;

\ ---- 2e. the link register ---------------------------------------------------
: LINK-REJECTS ( -- )
   [: A64EFF-CONTROL:RETURNS A64EFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-A64EFF-LINK TTHROWSQ
   [: A64EFF-CONTROL:TAIL-CALL A64EFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-A64EFF-LINK TTHROWSQ
   A64EFF-CONTROL:NO-RETURN A64EFF-LINK:CLOBBERED R-LINK DROP-ROUTINE
   A64EFF-CONTROL:RETURNS A64EFF-LINK:PRESERVED R-LINK DROP-ROUTINE ;

\ ---- 2f. a routine that never comes back delivers nothing ---------------------
: CONTROL-REJECTS ( -- )
   [: 0 SQ A64EFF:FPR-NONE A64EFF-NZCV:UNTOUCHED A64EFF-CONTROL:NO-RETURN
      R-RESULT DROP-ROUTINE ;] E-A64EFF-CONTROL TTHROWSQ
   [: SQ-NONE D0 A64EFF-NZCV:UNTOUCHED A64EFF-CONTROL:NO-RETURN
      R-RESULT DROP-ROUTINE ;] E-A64EFF-CONTROL TTHROWSQ
   [: SQ-NONE A64EFF:FPR-NONE A64EFF-NZCV:RESULT
      A64EFF-CONTROL:NO-RETURN R-RESULT DROP-ROUTINE ;]
      E-A64EFF-CONTROL TTHROWSQ
   SQ-NONE A64EFF:FPR-NONE A64EFF-NZCV:CLOBBERED
      A64EFF-CONTROL:NO-RETURN R-RESULT DROP-ROUTINE
   0 SQ D0 A64EFF-NZCV:RESULT A64EFF-CONTROL:RETURNS R-RESULT DROP-ROUTINE
   0 SQ D0 A64EFF-NZCV:RESULT A64EFF-CONTROL:TAIL-CALL R-RESULT DROP-ROUTINE ;

\ ---- 4. what is preserved is the complement ----------------------------------
\ The expected value is computed here from the whole legal file and the two sets
\ the case declares, so it is not a restatement of the module's arithmetic.
: WANT-PRESERVED ( A64EFF:gprs A64EFF:gprs -- n )
   {: res:gprs clob:gprs :}
   A64EFF:GPR-ALL A64EFF:GPRS-N
   res A64EFF:GPRS-N invert and
   clob A64EFF:GPRS-N invert and ;

: PRESERVED ( -- )
   LEAF A64EFF:GPR-PRESERVED A64EFF:GPRS-N
      A64EFF:GPR-NONE A64EFF:GPR-NONE WANT-PRESERVED T=
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-PRESERVED A64EFF:GPRS-N X1 X2 WANT-PRESERVED T=
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-PRESERVED X1 A64EFF:GPR-HAS? TFALSE
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-PRESERVED X2 A64EFF:GPR-HAS? TFALSE
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-PRESERVED X0 A64EFF:GPR-HAS? TTRUE
   D0 D1 D2 R-FPR A64EFF:FPR-PRESERVED A64EFF:FPRS-N
      A64EFF:FPR-ALL A64EFF:FPRS-N D1 A64EFF:FPRS-N invert and
      D2 A64EFF:FPRS-N invert and T=
   D0 D1 D2 R-FPR A64EFF:FPR-PRESERVED D0 A64EFF:FPR-HAS? TTRUE
   LEAF A64EFF:RETURNS? TTRUE
   A64EFF-CONTROL:TAIL-CALL 0 0 R-STACK A64EFF:RETURNS? TTRUE
   A64EFF-CONTROL:NO-RETURN 0 0 R-STACK A64EFF:RETURNS? TFALSE ;

\ ---- frame slots -------------------------------------------------------------
\ The reach case needs a frame deeper than a narrow access can address: a
\ one-byte slot cannot name a byte past IMM12-LIM-1, however large the frame is.
: SLOTS ( -- )
   0 8 64 FRAMED A64EFF:CHECK-SLOT
   56 8 64 FRAMED A64EFF:CHECK-SLOT
   60 4 64 FRAMED A64EFF:CHECK-SLOT
   63 1 64 FRAMED A64EFF:CHECK-SLOT
   [: 0 2 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: 0 3 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: 0 0 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: -8 8 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: 4 8 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: 2 4 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: 64 8 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   [: 60 8 64 FRAMED A64EFF:CHECK-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   1 A64EFF:SLOT-REACH 1 A64EFF:FRAME-MAX FRAMED A64EFF:CHECK-SLOT
   [: 1 A64EFF:SLOT-REACH 1+ 1 A64EFF:FRAME-MAX FRAMED A64EFF:CHECK-SLOT ;]
      E-A64EFF-SLOT TTHROWSQ
   [: 4 A64EFF:SLOT-REACH 4 + 4 A64EFF:FRAME-MAX FRAMED A64EFF:CHECK-SLOT ;]
      E-A64EFF-SLOT TTHROWSQ
   [: 2 A64EFF:SLOT-REACH drop ;] E-A64EFF-SLOT TTHROWSQ
   [: 0 A64EFF:SLOT-REACH drop ;] E-A64EFF-SLOT TTHROWSQ ;

\ ---- 3. a forged contract carries no identity ---------------------------------
\ Assembled by the generated constructor with a stack delta a returning routine
\ cannot have, so every word that revalidates refuses it with that rule's code.
: FORGED ( -- A64EFF:routine )
   SQ-NONE SQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 32 -16 A64EFF-ROUTINE:MAKE ;

\ A second forgery, this one naming the reserved register at argument position
\ zero of a list the checked constructor would never have accepted.
: FORGED-X18 ( -- A64EFF:routine )
   1 60 lshift 18 or A64EFF-PLACESEQ:MAKE SQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF-ROUTINE:MAKE ;

: FORGERY-REJECTS ( -- )
   [: FORGED A64EFF:VALIDATE DROP-ROUTINE ;] E-A64EFF-SP TTHROWSQ
   [: FORGED A64EFF:DIGEST DROP-DIGEST ;] E-A64EFF-SP TTHROWSQ
   [: FORGED A64EFF:ENCODE drop drop ;] E-A64EFF-SP TTHROWSQ
   [: FORGED LEAF A64EFF:SAME? drop ;] E-A64EFF-SP TTHROWSQ
   [: LEAF FORGED A64EFF:SAME? drop ;] E-A64EFF-SP TTHROWSQ
   [: FORGED A64EFF:GPR-PRESERVED A64EFF:GPRS-N drop ;] E-A64EFF-SP TTHROWSQ
   [: FORGED A64EFF:FPR-PRESERVED A64EFF:FPRS-N drop ;] E-A64EFF-SP TTHROWSQ
   [: FORGED A64EFF:RETURNS? drop ;] E-A64EFF-SP TTHROWSQ
   [: 0 8 FORGED A64EFF:CHECK-SLOT ;] E-A64EFF-SP TTHROWSQ
   [: FORGED-X18 A64EFF:VALIDATE DROP-ROUTINE ;] E-A64EFF-GPR TTHROWSQ
   [: FORGED-X18 A64EFF:DIGEST DROP-DIGEST ;] E-A64EFF-GPR TTHROWSQ
   [: FORGED-X18 A64EFF:GPR-PRESERVED A64EFF:GPRS-N drop ;]
      E-A64EFF-GPR TTHROWSQ
   [: FORGED-X18 A64EFF:GPR-IN@ A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: FORGED-X18 A64EFF:GPR-WRITABLE A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ ;

\ A field reader only projects, so it answers about a forged record without
\ pretending the record is declarable. That is the documented split, and it is
\ pinned here so a later "helpful" revalidation in a reader is a visible change.
: READERS ( -- )
   FORGED A64EFF:FRAME@ 32 T=
   FORGED A64EFF:DELTA@ -16 T=
   FORGED-X18 A64EFF:ARGS@ A64EFF-PLACESEQ:UNMAKE 1 60 lshift 18 or T=
   0 SQ 1 SQ X2 R-GPR A64EFF:ARGS@ 0 A64EFF:SEQ-REG@ 0 T=
   0 SQ 1 SQ X2 R-GPR A64EFF:RESULTS@ 0 A64EFF:SEQ-REG@ 1 T=
   0 SQ 1 SQ X2 R-GPR A64EFF:GPR-CLOBBER@ A64EFF:GPRS-N X2 A64EFF:GPRS-N T=
   D0 D1 D2 R-FPR A64EFF:FPR-IN@ A64EFF:FPRS-N D0 A64EFF:FPRS-N T=
   D0 D1 D2 R-FPR A64EFF:FPR-RESULT@ A64EFF:FPRS-N D1 A64EFF:FPRS-N T=
   D0 D1 D2 R-FPR A64EFF:FPR-CLOBBER@ A64EFF:FPRS-N D2 A64EFF:FPRS-N T=
   LEAF A64EFF:NZCV@ A64EFF-NZCV:UNTOUCHED A64EFF-NZCV:EQ TTRUE
   LEAF A64EFF:LINK@ A64EFF-LINK:PRESERVED A64EFF-LINK:EQ TTRUE
   LEAF A64EFF:CONTROL@ A64EFF-CONTROL:RETURNS A64EFF-CONTROL:EQ TTRUE
   LEAF A64EFF:TRAITS@ A64EFF:TRAITS-N 0 T= ;

\ ---- 2g. a missing or role-swapped argument never reaches runtime -------------
\ -1 is accepted by the checker, 0 refused.
: STATIC-REJECTS ( -- )
   s" A64T-OK ( A64EFF:placeseq A64EFF:placeseq A64EFF:gprs A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:nzcv A64EFF:link A64EFF:control A64EFF:traits n n -- A64EFF:routine ) A64EFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SHORT ( A64EFF:placeseq A64EFF:placeseq A64EFF:gprs A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:nzcv A64EFF:link A64EFF:control A64EFF:traits n -- A64EFF:routine ) A64EFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-FILES ( A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:placeseq A64EFF:placeseq A64EFF:gprs A64EFF:nzcv A64EFF:link A64EFF:control A64EFF:traits n n -- A64EFF:routine ) A64EFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SWAP ( A64EFF:placeseq A64EFF:placeseq A64EFF:gprs A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:link A64EFF:nzcv A64EFF:control A64EFF:traits n n -- A64EFF:routine ) A64EFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-BARE ( n A64EFF:placeseq A64EFF:gprs A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:nzcv A64EFF:link A64EFF:control A64EFF:traits n n -- A64EFF:routine ) A64EFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SETOK ( n -- A64EFF:gprs ) A64EFF:GPR-SET"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SETX ( n -- A64EFF:fprs ) A64EFF:GPR-SET"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-WITHX ( A64EFF:gprs A64EFF:fprs -- A64EFF:gprs ) A64EFF:GPR-WITH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SETS ( A64EFF:gprs A64EFF:gprs A64EFF:gprs A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:nzcv A64EFF:link A64EFF:control A64EFF:traits n n -- A64EFF:routine ) A64EFF:ROUTINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SEQOK ( A64EFF:placeseq n -- A64EFF:placeseq ) A64EFF:SEQ-WITH"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SEQSET ( A64EFF:gprs n -- A64EFF:placeseq ) A64EFF:SEQ-WITH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SEQOUT ( A64EFF:placeseq -- A64EFF:placeseq ) A64EFF:SEQ-SET"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" A64T-SLOTOK ( n n A64EFF:routine -- ) A64EFF:CHECK-SLOT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" A64T-SLOTSWAP ( A64EFF:routine n n -- ) A64EFF:CHECK-SLOT"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- 5a. the canonical preimage ----------------------------------------------
: PREIMAGE ( -- )
   0 SQ 1 SQ X2 R-GPR A64EFF:ENCODE {: base:ptr len:n :}
   len 112 T=
   base 0 CDIGEST:SLOT@ CDIGEST:TAG-A64-ROUTINE T=
   base 1 CDIGEST:SLOT@ 3 T=
   base 2 CDIGEST:SLOT@ 0 SQ A64EFF-PLACESEQ:UNMAKE T=
   base 3 CDIGEST:SLOT@ 1 SQ A64EFF-PLACESEQ:UNMAKE T=
   base 4 CDIGEST:SLOT@ X2 A64EFF:GPRS-N T=
   base 5 CDIGEST:SLOT@ 0 T=
   base 6 CDIGEST:SLOT@ 0 T=
   base 7 CDIGEST:SLOT@ 0 T=
   base 8 CDIGEST:SLOT@ 0 T=
   base 9 CDIGEST:SLOT@ 0 T=
   base 10 CDIGEST:SLOT@ 0 T=
   base 11 CDIGEST:SLOT@ 0 T=
   base 12 CDIGEST:SLOT@ 0 T=
   base 13 CDIGEST:SLOT@ 0 T=
   A64EFF-CONTROL:NO-RETURN 32 -32 R-STACK A64EFF:ENCODE {: nb:ptr nlen:n :}
   nb 10 CDIGEST:SLOT@ 2 T=
   nb 12 CDIGEST:SLOT@ 32 T=
   nb 13 CDIGEST:SLOT@ -32 T=
   SQ-NONE A64EFF:FPR-NONE A64EFF-NZCV:READ-CLOBBERED
      A64EFF-CONTROL:RETURNS R-RESULT A64EFF:ENCODE {: zb:ptr zlen:n :}
   zb 8 CDIGEST:SLOT@ 4 T=
   A64EFF-CONTROL:NO-RETURN A64EFF-LINK:CLOBBERED R-LINK
      A64EFF:ENCODE {: lb:ptr llen:n :}
   lb 9 CDIGEST:SLOT@ 1 T= ;

\ The guard that keeps the digest load-bearing: the record's DECLARED field
\ count, read back out of the type registry, is tied to the width of its
\ canonical preimage. Adding a field and forgetting to encode it therefore
\ reddens this suite instead of quietly producing a record that can change a
\ compilation while keeping its old identity.
: ROUTINE$ ( -- ptr u8 n ptr u8 n )  s" routine" s" A64EFF-ROUTINE" ;
: GPRS$ ( -- ptr u8 n ptr u8 n )     s" gprs" s" A64EFF-GPRS" ;
: PLACESEQ$ ( -- ptr u8 n ptr u8 n ) s" placeseq" s" A64EFF-PLACESEQ" ;
: FPRS$ ( -- ptr u8 n ptr u8 n )     s" fprs" s" A64EFF-FPRS" ;
: TRAITS$ ( -- ptr u8 n ptr u8 n )   s" traits" s" A64EFF-TRAITS" ;

: SLOTS-OF ( ptr u8 n -- n )
   nip CDIGEST:SLOT-BYTES / ;

: SCHEMA-PINS ( -- )
   PLACESEQ$ REFLECT:FAMS 1 T=
   PLACESEQ$ REFLECT:FLDS 1 T=
   PLACESEQ$ REFLECT:WIDTH 1 T=
   ROUTINE$ REFLECT:FAMS 1 T=
   ROUTINE$ REFLECT:FLDS 12 T=
   ROUTINE$ REFLECT:WIDTH 12 T=
   LEAF A64EFF:ENCODE SLOTS-OF ROUTINE$ REFLECT:FLDS 2 + T=
   GPRS$ REFLECT:FAMS 1 T=
   GPRS$ REFLECT:FLDS 1 T=
   GPRS$ REFLECT:WIDTH 1 T=
   FPRS$ REFLECT:FAMS 1 T=
   FPRS$ REFLECT:FLDS 1 T=
   TRAITS$ REFLECT:FAMS 1 T=
   TRAITS$ REFLECT:FLDS 1 T=
   s" nzcv" s" A64EFF-NZCV" REFLECT:VARS 5 T=
   s" link" s" A64EFF-LINK" REFLECT:VARS 2 T=
   s" control" s" A64EFF-CONTROL" REFLECT:VARS 3 T= ;

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
: A-NZCV ( n -- A64EFF:nzcv )
   5 mod {: k:n :}
   k 0= if A64EFF-NZCV:UNTOUCHED exit then
   k 1 = if A64EFF-NZCV:CLOBBERED exit then
   k 2 = if A64EFF-NZCV:RESULT exit then
   k 3 = if A64EFF-NZCV:READ-PRESERVED exit then
   A64EFF-NZCV:READ-CLOBBERED ;

: BIT-AT ( n n -- n )   \ index, position -> 0 or 1
   {: a:n p:n :}
   a 5 / p rshift 1 and ;

: A-GPR ( n n n -- A64EFF:gprs )   \ index, bit position, register
   {: a:n p:n r:n :}
   a p BIT-AT 0= if A64EFF:GPR-NONE exit then
   r A64EFF:GPR-REG ;

\ The same for an interface position: either the routine declares one there or
\ it declares none.
: A-SEQ ( n n n -- A64EFF:placeseq )   \ index, bit position, register
   {: a:n p:n r:n :}
   a p BIT-AT 0= if SQ-NONE exit then
   r SQ ;

: A-FPR ( n n n -- A64EFF:fprs )
   {: a:n p:n r:n :}
   a p BIT-AT 0= if A64EFF:FPR-NONE exit then
   r A64EFF:FPR-REG ;

: SWEEP-A>ROUTINE ( n -- A64EFF:routine )
   {: a:n :}
   a 5 0 A-SEQ  a 4 1 A-SEQ  a 3 2 A-GPR
   a 2 0 A-FPR  a 1 1 A-FPR  a 0 2 A-FPR
   a A-NZCV A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 16 0 A64EFF:ROUTINE ;

\ Sweep B shapes: link, control, frame, delta. None is (preserved, returns, 16,
\ 0), which is sweep A's fixed shape.
: B-LINK ( n -- A64EFF:link )
   6 < if A64EFF-LINK:PRESERVED exit then A64EFF-LINK:CLOBBERED ;

: B-CONTROL ( n -- A64EFF:control )
   {: s:n :}
   s 3 < if A64EFF-CONTROL:RETURNS exit then
   s 3 = if A64EFF-CONTROL:TAIL-CALL exit then
   A64EFF-CONTROL:NO-RETURN ;

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

: SWEEP-B>ROUTINE ( n -- A64EFF:routine )
   {: b:n :}
   b 9 mod {: s:n :}
   b 9 / {: t:n :}
   SQ-NONE SQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED s B-LINK s B-CONTROL
   t A64EFF:TRAIT-SET s B-FRAME s B-DELTA A64EFF:ROUTINE ;

: IX>ROUTINE ( n -- A64EFF:routine )
   dup SWEEP-A < if SWEEP-A>ROUTINE exit then
   SWEEP-A - SWEEP-B>ROUTINE ;

: COLLECT ( -- )
   ROWS ROWS-EXPECTED T=
   ROWS 0 ?do
      i IX>ROUTINE A64EFF:DIGEST CDIGEST-DIGEST:UNMAKE i DG!
   loop ;

\ Two contracts compare equal exactly when they digest equal, and - because the
\ enumeration is injective - exactly when they are the same row. The second
\ clause is what makes this a distinctness proof and not just a consistency one.
: PAIRWISE ( -- )
   ROWS ROWS * 0 ?do
      i ROWS / {: x:n :}
      i ROWS mod {: y:n :}
      x IX>ROUTINE y IX>ROUTINE A64EFF:SAME? {: same:bool :}
      x y DG= same TBOOL=
      same x y = TBOOL=
   loop ;

\ A record built twice by separate calls is the same record and the same digest.
: STABLE ( -- )
   0 SQ 1 SQ X2 R-GPR 0 SQ 1 SQ X2 R-GPR A64EFF:SAME? TTRUE
   0 SQ 1 SQ X2 R-GPR A64EFF:DIGEST
   0 SQ 1 SQ X2 R-GPR A64EFF:DIGEST CDIGEST-DIGEST:EQ TTRUE
   LEAF 0 SQ 1 SQ X2 R-GPR A64EFF:SAME? TFALSE
   LEAF A64EFF:DIGEST LEAF A64EFF:DIGEST CDIGEST-DIGEST:EQ TTRUE ;

public

: RUN ( -- )
   T-RESET
   MACHINE-FACTS
   VOCABULARY
   ALGEBRA
   SEQUENCE
   PLACES
   DSTACK-RESERVED
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

;package

A64EFF-TEST:RUN
