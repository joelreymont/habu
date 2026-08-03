\ native-clobber.f - what a published routine destroys, and what a call site does
\ with the answer. One concern: src/compiler/native/clobber.f and the narrowing
\ every stage of the chain hangs off it.
\
\ WHAT THIS SUITE HAS TO SHOW. Four things, and the last two are the ones the
\ record exists for.
\
\   1. That the record answers about an ADDRESS, keeps what it was told, and
\      hands an address it has no row for back to the caller's own worst case.
\   2. That it will NARROW a row and will not WIDEN one. Every call site
\      compiled against a row skipped saving exactly the registers outside it, so
\      a row that grew would make code already emitted wrong; the refusal is
\      E-NCLOB-WIDEN and it is the whole reason a narrow contract is sound.
\   3. That a real migration writes a row for the address it published at, and
\      that the row is what the emission's own allocation said - not the whole
\      pool, which is what the answer would be if the derivation had quietly
\      given up.
\   4. That the narrowing is REAL and that its absence is real too. Two callers
\      of the same shape are migrated: one calls a word the chain published and
\      the other calls a word the ENGINE compiled, which has no row and never
\      will. Their emitted code is counted instruction by instruction - how many
\      stores and loads against the engine's data-stack pointer each contains -
\      and the first has fewer. That is the measurement the whole change is for,
\      and the second is the proof that a callee nobody knows anything about
\      still gets the full caller-save discipline.
\
\ WHY THE COUNT IS OF INSTRUCTIONS AND NOT OF BYTES. A byte count moves for any
\ reason at all. What the narrowing removes is exactly the traffic between a
\ caller's registers and its own data-stack slots, so what is counted is that:
\ the Str and Ldr forms whose base register is the one the running engine keeps
\ its data-stack pointer in. A change that made the code smaller some other way
\ would not move these numbers, and a change that stopped narrowing would.
\
\ AND WHY THE CALLEE IS DELIBERATELY NOT A TINY ONE. A callee small enough that
\ copying its body into a caller costs no more than the call did is not called at
\ all - src/compiler/native/inline.f records such a body and the elaborator
\ splices it - and a caller with no call in it saves nothing, so it would measure
\ this record's narrowing at zero against zero. The callee below is therefore
\ sized past that rule on purpose: what this suite is about is what a CALL that
\ really happens costs, and the copying is measured where it belongs, in
\ test/compiler/native-inline.f.

require lib/test.f
require src/compiler/native/migrate.f
require src/compiler/native/clobber.f

package NCLOB-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

8 constant REGS                      \ scratch registers the migrated routines may use
0 constant GLOBAL-WID
4 constant INSN-BYTES

\ ---- addresses no code occupies ----------------------------------------------
\ The row rules are about a table keyed by an address, so the cases that are
\ about the TABLE use addresses of their own rather than borrowing a published
\ routine's: a case that recorded a second row over a real publication would be
\ changing what a real caller was compiled against. They are instruction aligned
\ because every address this record is ever handed is.
$10000 constant A1
$10004 constant A2
$10008 constant A3

: GPRS ( n -- A64EFF:gprs )
   A64EFF:GPR-SET ;

: FPRS ( n -- A64EFF:fprs )
   A64EFF:FPR-SET ;

: GPR-AT ( n -- n )
   {: e:n :}
   e A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N ;

: FPR-AT ( n -- n )
   {: e:n :}
   e A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N ;

: FLAG# ( bool -- n )
   if 1 else 0 then ;

: RECORD-CASES ( -- )
   s" an address with no row answers the worst case the caller states" T-LABEL
   A3 GPR-AT  A64EFF:GPR-ALL A64EFF:GPRS-N T=
   A3 NCLOB:KNOWN? FLAG# 0 T=

   s" a recorded row answers what it was told, per file" T-LABEL
   A1  $5 GPRS  $3 FPRS  NCLOB:RECORD
   A1 NCLOB:KNOWN? FLAG# 1 T=
   A1 GPR-AT $5 T=
   A1 FPR-AT $3 T=

   s" and a second address is a second row, not an overwrite" T-LABEL
   A2  $8 GPRS  $0 FPRS  NCLOB:RECORD
   A2 GPR-AT $8 T=
   A1 GPR-AT $5 T=

   s" a row may be re-recorded NARROWER, because every caller saved more" T-LABEL
   A1  $4 GPRS  $1 FPRS  NCLOB:RECORD
   A1 GPR-AT $4 T=
   A1 FPR-AT $1 T=

   s" and the same set again is the same row" T-LABEL
   A1  $4 GPRS  $1 FPRS  NCLOB:RECORD
   A1 GPR-AT $4 T= ;

\ A widening is refused per FILE, because a caller skipped saving in both.
: WIDEN-CASES ( -- )
   s" a row that would destroy a general register it did not is refused" T-LABEL
   [: A1  $C GPRS  $1 FPRS  NCLOB:RECORD ;] E-NCLOB-WIDEN TTHROWSQ

   s" and so is one that would destroy a floating register it did not" T-LABEL
   [: A1  $4 GPRS  $3 FPRS  NCLOB:RECORD ;] E-NCLOB-WIDEN TTHROWSQ

   s" a refused widening leaves the row it was refused against" T-LABEL
   A1 GPR-AT $4 T=
   A1 FPR-AT $1 T= ;

\ ---- reading a live word's own machine code ----------------------------------
\ The code start and the code length come off the word's own dictionary record,
\ which is where the publication seam wrote them.
variable CODE-AT

: CODE-PTR ( -- ptr u8 )
   CODE-AT 0 ptr-field @ ;

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

\ The unsigned-offset Str and Ldr of a whole cell, against the data-stack
\ pointer. The two encodings differ in one bit of the opc field, and the base
\ register is the field bits nine to five, so this is the form and the register
\ and nothing else about the instruction.
$FFC00000 constant MEM-MASK
$F9000000 constant STR-OP
$F9400000 constant LDR-OP

: DSTACK-AT? ( n n -- bool ) {: w:n op:n :}
   w MEM-MASK and op =
   w 5 rshift $1F and  A64EFF:DSTACK-GPR =  and ;

: DS-COUNT ( ptr u8 n n -- n ) {: a:ptr u:n op:n :}
   a u GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   dup XREF-START CODE-AT !
   XREF-LEN {: len:n :}
   0
   len INSN-BYTES / 0 ?do
      CODE-PTR i INSN-BYTES * + U32@ op DSTACK-AT? if 1+ then
   loop ;

: DS-STORES ( ptr u8 n -- n )
   STR-OP DS-COUNT ;

: DS-LOADS ( ptr u8 n -- n )
   LDR-OP DS-COUNT ;

: ENTRY-OF ( ptr u8 n -- n )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   XREF-START ;

\ ---- what a real publication records -----------------------------------------
\ The callee both loss cases below call. It is migrated, so the chain compiled it
\ and the seam recorded what it destroys. Its body is six additions rather than
\ one because a one-addition body is one the chain COPIES into its caller instead
\ of calling: the head of this file says why that would leave nothing to measure.
: MIGRATE-CALLEE ( -- )
   s" : NCLOB-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" 1 1 REGS NMIGRATE:DEFINE ;

\ The same body, compiled by the ENGINE and never migrated. Nothing knows what it
\ destroys and nothing ever will, so a call site that reaches it saves
\ everything - and that is the whole difference between the two callers below.
\ It is defined through the interpret path so that it lands beside the migrated
\ words, in the wordlist a plain name resolves in. It is the same body as the
\ migrated one, so the two callers below differ in nothing but which callee they
\ reach.
: DEFINE-ENGINE-CALLEE ( -- )
   s" : NCLOB-ENGINE-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV ;

: PUBLISHED-CASES ( -- )
   s" a migration records a row for the address it published at" T-LABEL
   s" NCLOB-STEP" ENTRY-OF NCLOB:KNOWN? FLAG# 1 T=

   s" and the row is narrower than the whole register file" T-LABEL
   s" NCLOB-STEP" ENTRY-OF GPR-AT  A64EFF:GPR-ALL A64EFF:GPRS-N T<>

   s" and it names no register outside the pool the routine was given" T-LABEL
   s" NCLOB-STEP" ENTRY-OF GPR-AT
   1 REGS lshift 1 -  invert and  0 T=

   s" a word the engine compiled has no row and never will" T-LABEL
   s" NCLOB-ENGINE-STEP" ENTRY-OF NCLOB:KNOWN? FLAG# 0 T= ;

\ ---- the narrowing, measured -------------------------------------------------
\ Two callers of one shape. Two values are live across every call in both - the
\ loop's index and its limit; the accumulator is the call's own argument and goes
\ out through a slot either way - so the difference between their counts is the
\ discipline and nothing else. Four calls in all, two values live across each:
\ eight stores and eight loads that the narrowing removes, four of each measured
\ here because the two rows differ by exactly that.
2 constant CALL-IN
1 constant CALL-OUT

: MIGRATE-NARROW ( -- )
   s" NCLOB-STEP" s" NCLOB-STEP" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NCLOB-NARROW ( n n -- n ) {: seed:n len:n :} seed len 0 ?do NCLOB-STEP NCLOB-STEP loop ;"
   CALL-IN CALL-OUT REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-WIDE ( -- )
   s" NCLOB-ENGINE-STEP"  s" NCLOB-ENGINE-STEP" ENTRY-OF  1 1 NMIGRATE:CALLEE
   s" : NCLOB-WIDE ( n n -- n ) {: seed:n len:n :} seed len 0 ?do NCLOB-ENGINE-STEP NCLOB-ENGINE-STEP loop ;"
   CALL-IN CALL-OUT REGS NMIGRATE:DEFINE-CALLING ;

: NARROW-CASES ( -- )
   s" both callers answer what their body says, on the same inputs" T-LABEL
   s" 0 4 NCLOB-NARROW" EV-N 168 T=
   s" 0 4 NCLOB-WIDE" EV-N 168 T=
   s" 5 0 NCLOB-NARROW" EV-N 5 T=
   s" 5 0 NCLOB-WIDE" EV-N 5 T=

   s" the caller of a word the chain published saves less than the other" T-LABEL
   s" NCLOB-NARROW" DS-STORES  s" NCLOB-WIDE" DS-STORES  < TTRUE
   s" NCLOB-NARROW" DS-LOADS  s" NCLOB-WIDE" DS-LOADS  < TTRUE

   s" and what it still stores is its arguments, not its live values" T-LABEL
   s" NCLOB-NARROW" DS-STORES 3 T=
   s" NCLOB-NARROW" DS-LOADS 4 T=

   s" while the caller of an engine-compiled word keeps the whole discipline" T-LABEL
   s" NCLOB-WIDE" DS-STORES 7 T=
   s" NCLOB-WIDE" DS-LOADS 8 T= ;

public

: RUN ( -- )
   T-RESET
   RECORD-CASES
   WIDEN-CASES
   DEFINE-ENGINE-CALLEE
   MIGRATE-CALLEE
   PUBLISHED-CASES
   MIGRATE-NARROW
   MIGRATE-WIDE
   NARROW-CASES
   T-REPORT ;

;package

NCLOB-TEST:RUN
