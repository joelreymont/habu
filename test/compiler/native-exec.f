\ native-exec.f - `execute` through the whole chain. One concern: what the native
\ chain compiles the token `execute` into, and where the arity it moves comes
\ from.
\
\ WHAT THIS SUITE HAS TO SHOW, AND WHY NOTHING SHORTER WOULD.
\
\   1. That the migrated word really ENTERS the quotation. "The migration
\      returned" says nothing: a word that dropped the token and returned its
\      argument answers the same way for a body whose arity is symmetric, which
\      is the trap LESSONS.md records. So every case here executes a quotation
\      that CHANGES its argument, and reads the answer.
\   2. That the arity is the CERTIFIED effect and not the site's stack shape.
\      The two paths a quotation reaches `execute` by are measured separately -
\      one handed straight over, one bound to a local first - and each computes,
\      because guessing from the depth would be right for both and wrong for the
\      one that has no answer at all.
\   3. That a cell nothing certified as a quotation is REFUSED by name rather
\      than executed under a guess. A body this definition wrote and no term ever
\      gave an arity is exactly that cell: the checker admits the program, and
\      the chain has no published number for it.
\   4. That a quotation whose effect is not an ordinary routine's - one with a
\      return-stack clause - is refused too, and by the same name, because the
\      question is asked of the checker rather than re-derived here.
\   5. That the emission is a CALL to the engine's own `execute` and not
\      something this chain invented. The bytes are decoded: the branch is there
\      and it goes to the address the dictionary answers for `execute`.
\   6. That the real multishot site in the library migrates and RUNS - the same
\      body, over a real array, executing the quotation once per element.

require lib/test.f
require src/compiler/native/migrate.f
require src/compiler/native/codewalk.f
require lib/array.f

\ ---- the library's own multishot body, re-compiled through the chain ----------
\ See ARRAY-CASE below for why this stands here rather than inside a case.
package ARRAY
: NX-A-SRC ( -- ptr u8 n )
   s" : A-MAPI! ( ptr a len [ idx a -- a ] -- ) {: arr:ptr len q :} len A-CHECK-WHOLE len LEN>N 0 ?do i A-IDX arr len i A-IDX A@ q execute arr len i A-IDX A! loop ;" ;
: NX-A-GO ( -- )
   NX-A-SRC 3 0 12 NMIGRATE:DEFINE ;
NX-A-GO
;package

\ The array the case maps over. It is global because the caller that maps it is
\ compiled by `evaluate` at run time, in the scope a program would write it in.
create NX-BUF 4 cells allot

package NEXEC-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled. Every execution below goes through it rather than through
\ a compiled call site, for the reason LESSONS.md records: a call site can be
\ copied by the inliner, and a test written as one then proves nothing.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

4 constant REGS
0 constant GLOBAL-WID

: REC ( ptr u8 n -- ptr a )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NPUB-NAME throw then ;

: REC-START ( ptr u8 n -- n )   REC XREF-START ;
: REC-LEN ( ptr u8 n -- n )     REC XREF-LEN ;

\ ---- reading the emitted instructions ----------------------------------------
\ A branch-with-link is the top six bits `100101` and a signed twenty-six-bit
\ word displacement; the address it reaches is its own address plus four times
\ that. Decoded from the encoding, so a case states the ADDRESS it means.
$FC000000 constant BL-MASK
$94000000 constant BL-FORM
1 25 lshift constant BL-SIGN
4 constant INSN-BYTES

: BL? ( n -- bool )
   BL-MASK and BL-FORM = ;

: BL-DELTA ( n -- n )
   {: w:n :}
   w $3FFFFFF and {: d:n :}
   d BL-SIGN and 0<> if d BL-SIGN 2 * - exit then
   d ;

: INSN-AT ( n n -- n )
   {: start:n k:n :}
   start k INSN-BYTES * + NWALK:INSN@ ;

: INSNS ( ptr u8 n -- n )
   REC-LEN INSN-BYTES / ;

: BLS ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   0
   a u INSNS 0 ?do
      start i INSN-AT BL? if 1+ then
   loop ;

: BL-AT ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   -1
   a u INSNS 0 ?do
      start i INSN-AT BL? if drop i leave then
   loop ;

: BL-TARGET ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u BL-AT {: k:n :}
   a u REC-START  k INSN-BYTES * +  {: site:n :}
   site  a u REC-START k INSN-AT BL-DELTA INSN-BYTES *  + ;

\ ---- the two paths a quotation reaches `execute` by ---------------------------
\ THE FIRST IS THE ONE THE DECLARATION NAMES DIRECTLY: the value is handed over
\ and executed where it stands. The second binds it to a LOCAL first, which is
\ what every real site in the tree does - and a local is a fresh push onto the
\ compile-time vector, so the fact that this cell is a quotation has to travel on
\ the NAME rather than on the entry the value happened to occupy.
\
\ BOTH BODIES ARE ASYMMETRIC, which is the whole reason they are multiplications
\ rather than increments: a routine that never entered the body at all, and
\ simply handed its argument back, answers correctly for anything symmetric.
: DEF-APPLY ( -- )
   s" : NX-APPLY ( [ n -- n ] n -- n ) swap execute ;" 2 1 REGS NMIGRATE:DEFINE ;

: DEF-LOCAL ( -- )
   s" : NX-LOCAL ( [ n -- n ] n -- n ) {: q v :} v q execute ;" 2 1 REGS
   NMIGRATE:DEFINE ;

: PARAM-CASE ( -- )
   DEF-APPLY
   s" a quotation handed straight over is entered and computes" T-LABEL
   s" NX-APPLY" REC-START  s" NX-APPLY" GLOBAL-WID NPUB:NEW-START T=
   s" : NX-U1 ( n -- n ) [: 3 * ;] swap NX-APPLY ;" EV
   s" 14 NX-U1" EV-N 42 T=
   s" and a different body through the same routine computes differently" T-LABEL
   s" : NX-U2 ( n -- n ) [: 5 * ;] swap NX-APPLY ;" EV
   s" 8 NX-U2" EV-N 40 T=

   DEF-LOCAL
   s" a quotation bound to a local first is entered and computes" T-LABEL
   s" NX-LOCAL" REC-START  s" NX-LOCAL" GLOBAL-WID NPUB:NEW-START T=
   s" : NX-U3 ( n -- n ) [: 3 * ;] swap NX-LOCAL ;" EV
   s" 14 NX-U3" EV-N 42 T= ;

\ WHAT THE DECODE RULES OUT. `execute` enters a routine nobody can name at
\ compile time, so there is nothing to inline and nothing to fold: the emission
\ has to hold a branch, and it has to go to the engine's own `execute`. A chain
\ that had invented a dispatch of its own would show a different address, and one
\ that had folded the call away would show no branch at all.
: DECODE-CASE ( -- )
   s" the emission holds one branch" T-LABEL
   s" NX-APPLY" BLS 1 T=
   s" and it goes to the engine's own execute" T-LABEL
   s" NX-APPLY" BL-TARGET  s" execute" NDICT:CALL-TARGET  T= ;

\ ---- the cells that have no certified arity ----------------------------------
\ A BODY THIS DEFINITION WROTE AND NOTHING CONSUMED. The checker admits
\ `[: 1 + ;] execute` - it unifies the quotation against the live row at the
\ `execute` itself - but that unification is a fact about the SITE and the
\ checker publishes effects by NAME, so there is no published number the chain
\ could read. Guessing it from the depth of the compile-time vector would be a
\ value heuristic exactly where a structural fact is missing, so it is refused.
\
\ A QUOTATION WHOSE EFFECT IS NOT AN ORDINARY ROUTINE'S is the second, and the
\ question is not re-derived here: src/compiler/native/dict.f asks the checker's
\ own EFFECT-QUOT-SIMPLE?, and a declaration carrying a return-stack clause
\ answers no quotation there - so no row is ever opened for it and the same
\ refusal would fire. It is asserted at the RESOLVER rather than through a
\ migration, because a program that executes such a parameter never reaches the
\ chain at all: the checker refuses the definition itself, measured below.
: SELF-EXEC ( -- )
   s" : NX-SELF ( n -- n ) [: 1 + ;] execute ;" 1 1 REGS NMIGRATE:MEASURE-HELD ;

70 constant CHECK-RC                 \ the engine refusing a definition it cannot certify

: RSTACK-EXEC ( -- )
   s" : NX-RS ( [ n -- n | a -- a ] n -- n ) swap execute ;" EV ;

: REFUSE-CASE ( -- )
   s" a body this definition wrote that nothing gave an arity is refused by name"
   T-LABEL
   [: SELF-EXEC ;] E-NELAB-QUOT TTHROWSQ
   s" and the refusal names the `execute` that had no answer" T-LABEL
   NELAB:REFUSED$ s" execute" T$=
   \ Terms are counted from the TOP of the row and cells from the bottom, so the
   \ quotation of `( [ n -- n ] n -- n )` is term ONE - which is the same index
   \ the parameter rows are opened by, and asking term zero here would answer
   \ about the plain `n` above it.
   s" an ordinary quotation parameter answers the arity its declaration states"
   T-LABEL
   s" NX-APPLY" 1 NDICT:SPELL-QUOT-DIN 1 T= 1 T=
   s" a return-stack quotation answers no quotation at all" T-LABEL
   s" : NX-RET ( [ n -- n | a -- a ] n -- n ) swap drop ;" EV
   s" NX-RET" 1 NDICT:SPELL-QUOT-DIN
   NDICT:QUOT-NONE T= NDICT:QUOT-NONE T=
   s" and executing one never reaches the chain: the checker refuses it first"
   T-LABEL
   [: RSTACK-EXEC ;] CHECK-RC TTHROWSQ ;

\ ---- the real multishot site --------------------------------------------------
\ THE LIBRARY'S OWN BODY, RE-COMPILED IN ITS OWN PACKAGE. `A-MAPI!` executes its
\ quotation once per element, inside a counted loop, with three locals live
\ across every turn - which is what "multishot" costs: the value has to survive
\ the call it is the argument of, every time round.
\
\ THE MIGRATION IS AT FILE LEVEL AND INSIDE `package ARRAY`, which is not a
\ convenience: the body names ARRAY's private helpers, and the migration entry
\ evaluates the source in whatever scope is open, so the definition only
\ resolves where the library's own compilation resolved it. `package` is a
\ parser directive, so no word can open a package from inside itself - and the
\ migration is its own assertion either way, because NMIGRATE:DEFINE publishes
\ or throws.

: BUF! ( -- )
   1 NX-BUF 0 cells + !
   2 NX-BUF 1 cells + !
   3 NX-BUF 2 cells + !
   4 NX-BUF 3 cells + ! ;

TRUSTED: BUF@ ( n -- n )
   cells NX-BUF + @ ;

: ARRAY-CASE ( -- )
   s" the migrated multishot site runs the quotation once per element" T-LABEL
   BUF!
   s" : NX-BUMP ( -- ) NX-BUF 4 ARRAY:A-LEN [: swap IDX>N 10 * + ;] ARRAY:A-MAPI! ;"
   EV
   s" NX-BUMP" EV
   0 BUF@ 1 T=
   1 BUF@ 12 T=
   2 BUF@ 23 T=
   3 BUF@ 34 T= ;

public

: RUN ( -- )
   T-RESET
   PARAM-CASE
   DECODE-CASE
   REFUSE-CASE
   ARRAY-CASE
   T-REPORT ;

;package

NEXEC-TEST:RUN
