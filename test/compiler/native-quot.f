\ native-quot.f - a quotation, through the whole chain and running. One concern:
\ what `[: ... ;]` becomes when the native chain compiles the definition holding
\ it.
\
\ WHAT THIS SUITE HAS TO SHOW, AND WHY NOTHING SHORTER WOULD.
\
\   1. That the migrated word IS the chain's code and that the address it leaves
\      RUNS. A quotation is an address somebody executes, so "the migration
\      returned" says nothing at all: an emission holding a wrong address returns
\      exactly the same way and fails when the address is used. So every case
\      here executes the quotation and reads the answer it computed.
\   2. That the address is the body's OWN entry. An answer alone cannot say that
\      either - a routine that fell through into the body would answer the same -
\      so the emitted instructions are DECODED: exactly one Adr per definition,
\      and the address it computes is the instruction after the enclosing
\      routine's return, which is where the second function begins. That
\      derivation is independent of the emitter's own tables.
\   3. That a `[:` the source never wrote opens nothing. The reader is the real
\      one here - the engine compiles the definition and the chain elaborates the
\      tape the checker filled - so a `[:` inside a comment and a `[:` inside a
\      string literal are put in front of a real one and the emission still holds
\      exactly one Adr. test/compiler/native-elaborate.f makes the same claim
\      about a hand-built tape, where the two tokens can be given the wrong KIND;
\      this one makes it about text a programmer could really write.
\   4. That a definition holding one is never RECORDED for copying. A copied body
\      would carry the Adr into another routine, where it is pc-relative to a
\      different instruction and names a different address - so the recording has
\      to decline, and the case beside it records a body of the same size with NO
\      quotation in it, so the decline is not simply "nothing this small is ever
\      recorded".
\   5. That the reach of the address form is bounded and that the bound cannot be
\      reached. The field holds a signed twenty-one-bit BYTE delta; the emission
\      is bounded at INSN-MAX instructions; and the second number times four is
\      less than the first, so no emission this chain can build puts a body out of
\      reach. Both halves are asserted, because the invariant is what makes
\      E-A64EMIT-REACH unreachable and a raised INSN-MAX would silently end that.

require lib/test.f
require src/compiler/native/migrate.f
require src/compiler/native/codewalk.f

package NQUOT-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled. Every execution below goes through it rather than through a
\ compiled call site, for the reason LESSONS.md records: a call site can be
\ copied by the inliner, and a test written as one then proves nothing about the
\ record it meant to test.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

4 constant REGS
0 constant GLOBAL-WID

here CELL 1- and CELL swap - CELL 1- and allot
variable OLD-START

: REC ( ptr u8 n -- ptr a )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NPUB-NAME throw then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

: REC-LEN ( ptr u8 n -- n )
   REC XREF-LEN ;

\ ---- reading the emitted instructions ----------------------------------------
\ THE TWO INSTRUCTION FORMS THIS SUITE HAS TO RECOGNISE, and both are decoded
\ from the encoding rather than searched for. An Adr is the form that adds a
\ signed byte displacement to its own address; a Ret is what ends a function. The
\ masks are the architecture's: bit 31 separates Adr from Adrp, and bits 28..24
\ are the form's own five.
$9F000000 constant ADR-MASK
$10000000 constant ADR-FORM
$D65F03C0 constant RET-WORD

: ADR? ( n -- bool )
   ADR-MASK and ADR-FORM = ;

: RET? ( n -- bool )
   RET-WORD = ;

\ The displacement an Adr carries: two low bits at 30..29 and nineteen high ones
\ at 23..5, sign extended from the twenty-one bits they make together. It is
\ built back up here rather than compared against an expected encoding, so a case
\ states the ADDRESS it means and not a bit pattern.
1 20 lshift constant ADR-SIGN

: ADR-DELTA ( n -- n )
   {: w:n :}
   w 29 rshift 3 and            \ immlo
   w 5 rshift $7FFFF and 2 lshift or {: d:n :}
   d ADR-SIGN and 0<> if d ADR-SIGN 2 * - exit then
   d ;

4 constant INSN-BYTES

: INSN-AT ( n n -- n )
   {: start:n k:n :}
   start k INSN-BYTES * + NWALK:INSN@ ;

: INSNS ( ptr u8 n -- n )
   REC-LEN INSN-BYTES / ;

\ How many Adr instructions the word's code holds. A definition with one
\ quotation has exactly one, and that is the count a `[:` the source never wrote
\ would move.
: ADRS ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   0
   a u INSNS 0 ?do
      start i INSN-AT ADR? if 1+ then
   loop ;

\ Where the FIRST Adr of the word's code stands, in instructions, and -1 when
\ there is none.
: ADR-AT ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   -1
   a u INSNS 0 ?do
      start i INSN-AT ADR? if drop i leave then
   loop ;

\ And where the first Ret stands. The enclosing routine is the FIRST function of
\ the emission and a straight-line body leaves through one return, so the
\ instruction after this one is where the second function begins - which is the
\ address the Adr has to compute, derived from the code itself rather than from
\ the emitter's own table of function starts.
: RET-AT ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   -1
   a u INSNS 0 ?do
      start i INSN-AT RET? if drop i leave then
   loop ;

\ The address the word's first Adr computes.
: ADR-TARGET ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u ADR-AT {: k:n :}
   a u REC-START  k INSN-BYTES * +  {: site:n :}
   site  a u REC-START k INSN-AT ADR-DELTA  + ;

\ And the address the second function of the emission begins at, derived the
\ other way.
: BODY-START ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START  a u RET-AT 1+ INSN-BYTES *  + ;

\ ---- the definitions the chain compiles --------------------------------------
\ Each is migrated at top level, so the word it publishes is global and the
\ callers this suite evaluates afterwards reach it as any program would.
: DEF-INC ( -- )
   s" : NQ-INC ( -- [ n -- n ] ) [: 1 + ;] ;" 0 1 REGS NMIGRATE:DEFINE ;

: DEF-TAKE ( -- )
   s" : NQ-TAKE ( [ n -- n ] n -- n ) swap execute ;" EV ;

: DEF-USE ( -- )
   s" : NQ-USE ( n -- n ) [: 3 * ;] swap NQ-TAKE ;" 1 1 REGS NMIGRATE:DEFINE ;

: DEF-THREE ( -- )
   s" : NQ-THREE ( -- n [ n n -- n ] [ n n n -- n ] ) 0 [: drop ;] [: drop drop ;] ;"
   0 3 REGS NMIGRATE:DEFINE ;

\ A `[:` inside a parenthesised comment and another inside a string literal,
\ both in front of the real one. Neither is a token the checker's reader hands
\ over as an opener: the comment is consumed and never becomes a token at all,
\ and the string is ONE token whose kind is string-literal.
\
\ THE TEXT IS SPELLED AS ITS BYTES because it holds a string literal of its own,
\ and a `s"` inside a `s"` is not something this dialect writes.
\ `: NQ-HID ( -- [ n -- n ] ) ( [: ;] ) s" [: ;]" 2drop [: 1 + ;] ;`
create HID-TXT
   58 c, 32 c, 78 c, 81 c, 45 c, 72 c, 73 c, 68 c, 32 c, 40 c,
   32 c, 45 c, 45 c, 32 c, 91 c, 32 c, 110 c, 32 c, 45 c, 45 c,
   32 c, 110 c, 32 c, 93 c, 32 c, 41 c, 32 c, 40 c, 32 c, 91 c,
   58 c, 32 c, 59 c, 93 c, 32 c, 41 c, 32 c, 115 c, 34 c, 32 c,
   91 c, 58 c, 32 c, 59 c, 93 c, 34 c, 32 c, 50 c, 100 c, 114 c,
   111 c, 112 c, 32 c, 91 c, 58 c, 32 c, 49 c, 32 c, 43 c, 32 c,
   59 c, 93 c, 32 c, 59 c,
64 constant HID-N

: DEF-HIDDEN ( -- )
   HID-TXT HID-N 0 1 REGS NMIGRATE:DEFINE ;

\ The pair the recording case needs: a body of the same shape with no quotation
\ in it, which the recorder DOES keep, and the quotation-carrying one beside it.
: DEF-PLAIN ( -- )
   s" : NQ-PLAIN ( n -- n ) 1 + ;" 1 1 REGS NMIGRATE:DEFINE ;

\ ---- the cases ---------------------------------------------------------------
: RUNS-CASE ( -- )
   DEF-INC
   s" the address a migrated definition leaves runs its own body" T-LABEL
   s" NQ-INC" REC-START  s" NQ-INC" GLOBAL-WID NPUB:NEW-START T=
   s" 41 NQ-INC execute" EV-N 42 T=

   DEF-TAKE DEF-USE
   s" a quotation handed to a callee across a made call still runs" T-LABEL
   s" NQ-USE" REC-START  s" NQ-USE" GLOBAL-WID NPUB:NEW-START T=
   s" 14 NQ-USE" EV-N 42 T=

   DEF-THREE
   s" three bodies in one definition are three routines, each its own" T-LABEL
   s" NQ-THREE" REC-START  s" NQ-THREE" GLOBAL-WID NPUB:NEW-START T=
   s" : NQ-T2 ( -- n ) NQ-THREE {: z q2 q3 :} 2 3 q2 execute ;" EV
   s" : NQ-T3 ( -- n ) NQ-THREE {: z q2 q3 :} 4 5 6 q3 execute ;" EV
   s" NQ-T2" EV-N 2 T=
   s" NQ-T3" EV-N 4 T= ;

: DECODE-CASE ( -- )
   s" the emission holds exactly one Adr, and it names the body's entry" T-LABEL
   s" NQ-INC" ADRS 1 T=
   \ The Adr stands in the FIRST function and the body is the second, so the
   \ address it computes is one past the enclosing routine's return.
   s" NQ-INC" ADR-TARGET  s" NQ-INC" BODY-START  T=
   \ And that address is inside the word's own code, which is what makes it an
   \ address of this emission rather than of whatever stands after it.
   s" NQ-INC" ADR-TARGET  s" NQ-INC" REC-START  >= TTRUE
   s" NQ-INC" ADR-TARGET
   s" NQ-INC" REC-START  s" NQ-INC" REC-LEN +  < TTRUE

   s" three bodies are three Adrs" T-LABEL
   s" NQ-THREE" ADRS 2 T= ;

: HIDDEN-CASE ( -- )
   DEF-HIDDEN
   s" a `[:` in a comment or a string opens nothing" T-LABEL
   s" NQ-HID" ADRS 1 T=
   s" NQ-HID" ADR-TARGET  s" NQ-HID" BODY-START  T=
   s" 41 NQ-HID execute" EV-N 42 T= ;

\ WHAT DECLINES IT, so a reader does not go looking for a quotation-shaped rule.
\ `[:` and `;]` are CONTROL tokens, and the elaborator's one staging table
\ (SPLICE-STAGING) says a copy has nothing to stage for a control token - the
\ same answer it gives `if`. So the recording stops at the opener and the row is
\ never claimed. What makes that the RIGHT answer rather than a coincidence is
\ the Adr: it is pc-relative, so the same instruction copied into another routine
\ names another address, and there is no rewriting step in a splice that could
\ correct it.
: RECORD-CASE ( -- )
   DEF-PLAIN
   s" a body with no quotation in it IS recorded for copying" T-LABEL
   s" NQ-PLAIN" REC-START NINL:KNOWN? TTRUE
   s" a body holding a quotation is NOT" T-LABEL
   s" NQ-INC" REC-START NINL:KNOWN? TFALSE ;

\ ---- the one body shape the tree writes and this leaf declines ----------------
\ A BODY THAT NEVER COMES BACK. `NQ-DIE` throws, so the fall-through of the body
\ that calls it is dead: there is no return to stage, and a caller reaching that
\ routine with an ordinary call would be handed one that leaves by a path its own
\ control flow does not have. The consumer's DECLARED effect says the quotation
\ returns - src/compiler/native/dict.f's three-clause question is about the
\ declaration and passes - so the disagreement is only visible once the body
\ itself has been walked, which is where it is refused.
\
\ IT IS THE LAST `[:` REFUSAL IN THE TREE. The census over src and lib answers
\ one, and it is lib/test/suite.f DEFAULTS, which writes `[: RUN-MISSING ;]`.
\ Compiling a body that never returns is a capability this leaf does not have:
\ the function it would build has no return, so what the emitter writes for it
\ and what a caller may do with its address are both open questions.
\
\ AND THE WORD IS LEFT AS THE ENGINE COMPILED IT. The refusal happens after the
\ engine has published the definition, so what a refusal must not do is damage
\ it: the case runs the word afterwards and reads its record.
: DEF-DIE ( -- )
   s" : NQ-DIE ( n -- ) drop E-FS-OPEN throw ;" EV
   s" : NQ-KEEP ( [ n -- ] n -- n ) swap drop ;" EV ;

: DEAD-BEFORE ( -- )
   s" : NQ-DEAD ( n -- n ) [: NQ-DIE ;] swap NQ-KEEP ;" EV
   s" NQ-DEAD" REC-START OLD-START ! ;

: DEF-DEAD ( -- )
   s" : NQ-DEAD2 ( n -- n ) [: NQ-DIE ;] swap NQ-KEEP ;" 1 1 REGS NMIGRATE:DEFINE ;

: DEAD-CASE ( -- )
   DEF-DIE
   DEAD-BEFORE
   s" a quotation body that never comes back is refused by name" T-LABEL
   [: DEF-DEAD ;] E-NELAB-QUOT TTHROWSQ
   \ Nothing is asserted about the refusal RECORD here. The migration entry
   \ unwinds the chain before it rethrows, so what a caller reads afterwards is
   \ the record of whatever elaborated last; which token each refusal names is
   \ test/compiler/native-elaborate.f's measurement, taken where the record is
   \ still the one the refusal left.
   s" and the word the engine published still runs its own code" T-LABEL
   s" NQ-DEAD" REC-START OLD-START @ T=
   s" 7 NQ-DEAD" EV-N 7 T=
   s" 7 NQ-DEAD2" EV-N 7 T=
   s" NQ-DEAD2" GLOBAL-WID NPUB:REPUBLISHED? TFALSE ;

\ ---- the reach of the address form -------------------------------------------
\ The field's own boundary, asked of the dialect's reader, and the standing
\ invariant that keeps the emitter away from it. The first says the check is a
\ bound and not a mask; the second says no emission this chain can build reaches
\ that bound, which is why E-A64EMIT-REACH cannot fire for an Adr today. Raise
\ INSN-MAX past the field and the second assertion is what goes red.
1 20 lshift constant ADR-HI

: REACH-CASE ( -- )
   s" the address field is a bound and not a mask" T-LABEL
   ADR-HI 1- A64IR:ADR-FITS? TTRUE
   ADR-HI negate A64IR:ADR-FITS? TTRUE
   ADR-HI A64IR:ADR-FITS? TFALSE
   ADR-HI negate 1- A64IR:ADR-FITS? TFALSE
   s" and no emission this chain can build reaches it" T-LABEL
   A64EMIT:INSN-MAX INSN-BYTES *  ADR-HI  < TTRUE ;

public

: RUN ( -- )
   T-RESET
   RUNS-CASE
   DECODE-CASE
   HIDDEN-CASE
   RECORD-CASE
   DEAD-CASE
   REACH-CASE
   T-REPORT ;

;package

NQUOT-TEST:RUN
