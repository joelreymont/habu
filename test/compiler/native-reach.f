\ native-reach.f - carrying a migration back to callers that were already
\ compiled. One concern: src/compiler/native/reach.f, and the branch reader it
\ moves call sites with.
\
\ WHAT THIS SUITE HAS TO SHOW. Six things.
\
\   1. THAT THE BRANCH ARITHMETIC IS THE MACHINE'S. A `bl` is read out of a
\      caller the engine really compiled and its decoded target is held against
\      the callee's own dictionary record - so the reader is pinned to a number
\      the engine wrote rather than to a second copy of the same formula. The
\      encoder is then held against the decoder over the whole displacement
\      field, including both ends of it, and one instruction past either end is
\      a refusal.
\   2. THAT A REDIRECTION REALLY MOVES A CALLER. A callee is published by the
\      engine, a caller is compiled against it - which is the only moment the
\      engine decides call-or-copy and writes the displacement - and only then
\      is the same body migrated. Before the redirection the caller's call
\      instruction enters the engine's record; afterwards it enters the chain's,
\      and the caller answers exactly what it answered before on pinned inputs.
\      Both halves matter: a redirection that moved nothing would keep the
\      answers too.
\   3. THAT A BODY THE ENGINE COPIED IS A LOUD REFUSAL. The other half of the
\      finding this work came from: a small straight-line callee is pasted into
\      its caller, so there is no call instruction to move and republishing it
\      reaches nobody. That is E-NREACH-NONE rather than an answer of zero,
\      because a quiet zero is exactly how a migration comes to be reported as
\      landing when it did not.
\   4. THAT AN UNSAFE MOVE IS REFUSED, AND THAT THE TEST IS DIRECTIONAL. Two
\      routines with the same name and the same effect, one destroying strictly
\      more registers than the other. Moving a caller onto the wider one is
\      refused by name: the site was compiled against what the narrow address
\      destroys and skipped saving everything outside it. The SAME PAIR in the
\      other order gets past that test and fails for a different reason
\      entirely, which is what says the refusal is a decision about these two
\      sets and not a refusal of everything.
\   5. THAT A REFUSAL COSTS NOTHING. After the refused move, the caller still
\      enters the record it entered and still answers the same. Everything this
\      seam can refuse is asked before the first instruction is written, and
\      this is the case that would go red if that stopped being true. The sites
\      are counted BEFORE the refusal as well as after and the two counts held
\      against each other, because "left every site where it found it" is a
\      claim about a difference: a caller holding none would satisfy the
\      after-count on its own, and that is the shape this case actually failed
\      in once - see the note above NRT-NARROW.
\   6. THAT THE REMAINING REFUSALS ARE REACHABLE, one fixture each: a name that
\      resolves to nothing, both names resolving to one record, two records
\      carrying different words, a target the publication seam never wrote, two
\      words declaring different effects, and a routine that itself calls the
\      code being redirected.
\
\ WHAT HAS NO FIXTURE AND WHY. E-NREACH-RANGE, the seam's own refusal for a
\ site too far from its new target, cannot be reached by a fixture: the whole
\ code arena is eight megabytes and the branch field spans a hundred and
\ twenty-eight, so no two words this process can publish are far enough apart.
\ What CAN be pinned is the arithmetic behind it, and case 1 pins it at both
\ ends of the field and one instruction past each - which is the part that
\ would be wrong if it were wrong.
\
\ THE ORDER OF THIS FILE IS THE EXPERIMENT. A caller has to be compiled after
\ its callee and before the migration, because the engine decides call-or-copy
\ and writes the displacement while it compiles the caller. So the fixtures are
\ published at load time in that order, between the case words, exactly as
\ tools/codegen-workload-test.f publishes its own.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require src/compiler/native/branch.f
require src/compiler/native/migrate.f
require src/compiler/native/reach.f
require tools/codegen-workload-scan.f

package NREACH-TEST

\ The register budgets the migrated fixtures are published with. They are public
\ because the fixtures that use them are published at the bottom of this file,
\ outside the package, and naming them here is what keeps a budget from being
\ written down twice and drifting.
public

8 constant REGS                      \ scratch registers the migrated routines may use
12 constant WIDE-REGS                \ enough for the pressure body below to keep eight sums live
3 constant NARROW-REGS               \ and few enough that the wide row is strictly wider

private

4 constant INSN-BYTES

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite runs a caller that names a word by a string.
TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

: ENTRY-OF ( ptr u8 n -- n )
   CODEGEN-SCAN:WORD-ENTRY ;

: GPR-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u ENTRY-OF A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N ;

\ Where the first call instruction of this word goes, read out of the word's own
\ compiled code through the branch reader under test. -1 when the word holds no
\ call instruction at all, which is an answer no code address can be.
: FIRST-BL-TARGET ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u ENTRY-OF {: s:n :}
   -1
   a u CODEGEN-SCAN:WORD-INSNS 0 ?do
      a u i CODEGEN-SCAN:WORD-INSN-AT NBR:BL? if
         drop
         s i INSN-BYTES * +
         a u i CODEGEN-SCAN:WORD-INSN-AT
         NBR:BL-TARGET
         leave
      then
   loop ;

\ ---- the branch field, from both ends ----------------------------------------
\ An address far enough from either end of the field that both extremes are
\ still addresses.
$10000000 constant FAR-AT
33554431 constant IMM26-MAX          \ the largest instruction count the field holds
-33554432 constant IMM26-MIN         \ and the smallest

: ROUND-TRIP ( n -- n ) {: d:n :}
   FAR-AT  FAR-AT d INSN-BYTES * +  NBR:BL-WORD {: w:n :}
   FAR-AT w NBR:BL-TARGET ;

\ The same round trip through the TAIL form, whose displacement the reader has
\ to decode from an instruction nothing in the chain writes yet. The instruction
\ is therefore built here out of the call form and the one opcode bit that
\ separates them, so what is being read back is a real branch word and not a
\ number this file also produced by the code under test.
$FC000000 constant BRANCH-MASK
$14000000 constant B-OP

: AS-B ( n -- n ) {: w:n :}
   w BRANCH-MASK invert and  B-OP or ;

: B-ROUND-TRIP ( n -- n ) {: d:n :}
   FAR-AT  FAR-AT d INSN-BYTES * +  NBR:BL-WORD AS-B {: w:n :}
   FAR-AT w NBR:B-TARGET ;

public

: BRANCH-CASES ( -- )
   s" a call instruction the engine wrote decodes to the callee's own record" T-LABEL
   s" NRT-BASE:NRT-CALLER" FIRST-BL-TARGET  s" NRT-BASE:NRT-STEP" ENTRY-OF T=

   s" and a word the engine copied its callee into holds no call at all" T-LABEL
   s" NRT-BASE:NRT-TINY-CALLER" FIRST-BL-TARGET -1 T=

   s" what the encoder builds is a call instruction" T-LABEL
   FAR-AT FAR-AT NBR:BL-WORD NBR:BL? TTRUE

   s" and the decoder reads back every displacement the encoder writes" T-LABEL
   0 ROUND-TRIP FAR-AT T=
   1 ROUND-TRIP FAR-AT INSN-BYTES + T=
   -1 ROUND-TRIP FAR-AT INSN-BYTES - T=
   IMM26-MAX ROUND-TRIP  FAR-AT IMM26-MAX INSN-BYTES * + T=
   IMM26-MIN ROUND-TRIP  FAR-AT IMM26-MIN INSN-BYTES * + T=

   s" one instruction past either end of the field is out of reach" T-LABEL
   FAR-AT  FAR-AT IMM26-MAX INSN-BYTES * +          NBR:REACHES? TTRUE
   FAR-AT  FAR-AT IMM26-MAX 1+ INSN-BYTES * +       NBR:REACHES? TFALSE
   FAR-AT  FAR-AT IMM26-MIN INSN-BYTES * +          NBR:REACHES? TTRUE
   FAR-AT  FAR-AT IMM26-MIN 1- INSN-BYTES * +       NBR:REACHES? TFALSE

   s" and so is an address that is not a whole instruction" T-LABEL
   FAR-AT  FAR-AT 2 +  NBR:REACHES? TFALSE
   FAR-AT 2 +  FAR-AT  NBR:REACHES? TFALSE

   s" a branch nothing could encode is refused rather than built" T-LABEL
   [: FAR-AT  FAR-AT IMM26-MAX 1+ INSN-BYTES * +  NBR:BL-WORD drop ;]
      E-NBR-RANGE TTHROWSQ
   [: FAR-AT  FAR-AT 2 +  NBR:BL-WORD drop ;] E-NBR-RANGE TTHROWSQ

   s" the tail form carries the displacement through the same field" T-LABEL
   FAR-AT FAR-AT NBR:BL-WORD AS-B NBR:B? TTRUE
   0 B-ROUND-TRIP FAR-AT T=
   1 B-ROUND-TRIP FAR-AT INSN-BYTES + T=
   -1 B-ROUND-TRIP FAR-AT INSN-BYTES - T=
   IMM26-MAX B-ROUND-TRIP  FAR-AT IMM26-MAX INSN-BYTES * + T=
   IMM26-MIN B-ROUND-TRIP  FAR-AT IMM26-MIN INSN-BYTES * + T=

   s" the two forms are told apart, which is the whole point of reading both" T-LABEL
   FAR-AT FAR-AT NBR:BL-WORD AS-B  NBR:BL? TFALSE
   FAR-AT FAR-AT NBR:BL-WORD       NBR:B?  TFALSE
   FAR-AT FAR-AT NBR:BL-WORD AS-B  FAR-AT FAR-AT NBR:BL-WORD <> TTRUE ;

\ ---- the caller, before the migration ----------------------------------------
: BEFORE-CASES ( -- )
   s" the engine calls the sized callee and copies the small one" T-LABEL
   s" NRT-BASE:NRT-STEP" CODEGEN-SCAN:ENGINE-COPIES? TFALSE
   s" NRT-BASE:NRT-TINY" CODEGEN-SCAN:ENGINE-COPIES? TTRUE

   s" so the caller of the first holds call sites and the other holds none" T-LABEL
   s" NRT-BASE:NRT-STEP" CODEGEN-SCAN:CALL-SITES 2 T=
   s" NRT-BASE:NRT-CALLER" s" NRT-BASE:NRT-STEP" CODEGEN-SCAN:CALLS-IN 2 T=
   s" NRT-BASE:NRT-TINY" CODEGEN-SCAN:CALL-SITES 0 T=

   s" and the caller answers what its body says" T-LABEL
   s" 0 NRT-BASE:NRT-CALLER" EV-N 42 T=
   s" 5 NRT-BASE:NRT-CALLER" EV-N 47 T= ;

private

variable OLD-ENTRY
variable MOVED

\ How many sites the clobber caller held BEFORE the refused move. The count
\ after the refusal is held against this rather than against a number written
\ down a second time, so the case cannot be satisfied by a caller that never had
\ any sites - the failure this file was carrying when the constant fold shrank
\ its callee under the engine's copy limit.
variable CL-SITES

public

\ ---- the redirection ---------------------------------------------------------
: MOVE ( -- )
   s" NRT-BASE:NRT-STEP" ENTRY-OF OLD-ENTRY !
   s" NRT-BASE:NRT-STEP" s" NRT-CHAIN:NRT-STEP" NREACH:REDIRECT MOVED ! ;

: AFTER-CASES ( -- )
   s" every call site the old code had was moved, and counted" T-LABEL
   MOVED @ 2 T=
   s" NRT-BASE:NRT-STEP" CODEGEN-SCAN:CALL-SITES 0 T=

   s" the caller now enters the record the chain published" T-LABEL
   s" NRT-BASE:NRT-CALLER" s" NRT-CHAIN:NRT-STEP" CODEGEN-SCAN:CALLS-IN 2 T=
   s" NRT-BASE:NRT-CALLER" FIRST-BL-TARGET  s" NRT-CHAIN:NRT-STEP" ENTRY-OF T=

   s" and it answers exactly what it answered before" T-LABEL
   s" 0 NRT-BASE:NRT-CALLER" EV-N 42 T=
   s" 5 NRT-BASE:NRT-CALLER" EV-N 47 T=

   s" the old word keeps its own record and its own code" T-LABEL
   s" NRT-BASE:NRT-STEP" ENTRY-OF  OLD-ENTRY @ T=
   s" 0 NRT-BASE:NRT-STEP" EV-N 21 T=

   s" and the two records are two records, not one" T-LABEL
   s" NRT-CHAIN:NRT-STEP" ENTRY-OF  OLD-ENTRY @ T<> ;

\ ---- a body the engine copied has nothing to move ----------------------------
: COPY-CASES ( -- )
   s" a word whose body the engine copies has no call site to move" T-LABEL
   s" NRT-BASE:NRT-TINY" CODEGEN-SCAN:CALL-SITES 0 T=
   [: s" NRT-BASE:NRT-TINY" s" NRT-CHAIN:NRT-TINY" NREACH:REDIRECT drop ;]
      E-NREACH-NONE TTHROWSQ

   s" and its caller still carries the copy and answers the same" T-LABEL
   s" NRT-BASE:NRT-TINY-CALLER" s" NRT-CHAIN:NRT-TINY" CODEGEN-SCAN:CALLS-IN 0 T=
   s" 15 NRT-BASE:NRT-TINY-CALLER" EV-N 7 T= ;

\ ---- the unsafe move ---------------------------------------------------------
\ The pair is asserted to have the shape the case needs before the case is made:
\ the wide routine really does destroy registers the narrow one does not, and
\ the narrow one destroys none the wide one does not. Without those two lines a
\ record that stopped narrowing at all would leave both directions passing.
\
\ AND THE SITES ARE COUNTED BEFORE THE REFUSAL AS WELL AS AFTER. "The refused
\ move left every site where it found it" is a claim about a difference, so it
\ needs both ends: a caller holding no sites at all satisfies an after-count of
\ zero just as happily as a refusal that behaved. So the count is taken first,
\ asserted to be the two calls the caller was written to make, and the count
\ after the refusal is held against THAT rather than against a literal. The
\ engine's copy decision is asked about the callee in the same breath, because a
\ callee the engine copies is precisely how the before-count goes to zero.
: CLOBBER-CASES ( -- )
   s" the narrow routine is one the engine calls, not one it copies" T-LABEL
   s" NRT-NARROW:NRT-CL" CODEGEN-SCAN:ENGINE-COPIES? TFALSE
   s" NRT-BASE:NRT-CL-CALLER" s" NRT-NARROW:NRT-CL" CODEGEN-SCAN:CALLS-IN
      CL-SITES !
   CL-SITES @ 2 T=

   s" the wide routine destroys registers the narrow one does not" T-LABEL
   s" NRT-WIDE-P:NRT-CL" GPR-AT  s" NRT-NARROW:NRT-CL" GPR-AT invert and  0 T<>
   s" NRT-NARROW:NRT-CL" GPR-AT  s" NRT-WIDE-P:NRT-CL" GPR-AT invert and  0 T=

   s" a caller of the narrow one may not be moved onto the wide one" T-LABEL
   [: s" NRT-NARROW:NRT-CL" s" NRT-WIDE-P:NRT-CL" NREACH:REDIRECT drop ;]
      E-NREACH-CLOBBER TTHROWSQ

   s" and the same pair the other way round gets past that test" T-LABEL
   [: s" NRT-WIDE-P:NRT-CL" s" NRT-NARROW:NRT-CL" NREACH:REDIRECT drop ;]
      E-NREACH-NONE TTHROWSQ

   s" the refused move left every site where it found it" T-LABEL
   s" NRT-BASE:NRT-CL-CALLER" s" NRT-NARROW:NRT-CL" CODEGEN-SCAN:CALLS-IN
      CL-SITES @ T=
   s" NRT-BASE:NRT-CL-CALLER" s" NRT-WIDE-P:NRT-CL" CODEGEN-SCAN:CALLS-IN 0 T=
   s" 0 NRT-BASE:NRT-CL-CALLER" EV-N 2413 T= ;

\ ---- the remaining refusals --------------------------------------------------
: REFUSAL-CASES ( -- )
   s" a name that resolves to no record is refused" T-LABEL
   [: s" NRT-NO-SUCH-WORD" s" NRT-CHAIN:NRT-STEP" NREACH:REDIRECT drop ;]
      E-NREACH-NAME TTHROWSQ
   [: s" NRT-BASE:NRT-STEP" s" NRT-NO-SUCH-WORD" NREACH:REDIRECT drop ;]
      E-NREACH-NAME TTHROWSQ

   s" and so are two names that resolve to one record" T-LABEL
   [: s" NRT-CHAIN:NRT-STEP" s" NRT-CHAIN:NRT-STEP" NREACH:REDIRECT drop ;]
      E-NREACH-NAME TTHROWSQ

   s" a redirection may not point a word's callers at another word" T-LABEL
   [: s" NRT-BASE:NRT-CL-CALLER" s" NRT-CHAIN:NRT-STEP" NREACH:REDIRECT drop ;]
      E-NREACH-WORD TTHROWSQ

   s" nor at code the publication seam did not write" T-LABEL
   [: s" NRT-BASE:NRT-STEP" s" NRT-ENGINE:NRT-STEP" NREACH:REDIRECT drop ;]
      E-NREACH-ROUTINE TTHROWSQ

   s" nor at a word declaring another effect" T-LABEL
   [: s" NRT-BASE:NRT-ARITY" s" NRT-CHAIN:NRT-ARITY" NREACH:REDIRECT drop ;]
      E-NREACH-EFFECT TTHROWSQ

   s" and a routine that itself calls the code being moved is refused" T-LABEL
   [: s" NRT-BASE:NRT-SELF" s" NRT-SELFP:NRT-SELF" NREACH:REDIRECT drop ;]
      E-NREACH-SELF TTHROWSQ ;

;package

\ ---- the fixtures, in the order that makes them fixtures ---------------------
\ Each callee first, then the caller that names it: the engine decides
\ call-or-copy and writes the displacement while it compiles the CALLER, so a
\ caller compiled first would prove nothing about either.

T-RESET

package NRT-BASE
public

\ The sized callee, and a caller compiled against the engine's record.
: NRT-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;
: NRT-CALLER ( n -- n ) NRT-STEP NRT-STEP ;

\ The small straight-line callee the engine copies into its caller instead.
: NRT-TINY ( n -- n ) 7 and ;
: NRT-TINY-CALLER ( n -- n ) NRT-TINY NRT-TINY ;

\ A word with an effect of its own, for the effect refusal.
: NRT-ARITY ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;

\ A word whose migrated twin will call it, for the self refusal.
: NRT-SELF ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;

;package

NREACH-TEST:BRANCH-CASES
NREACH-TEST:BEFORE-CASES

\ The same bodies, compiled by the chain and published as records of their own.
\ They cannot be published under the old names in the old wordlist: a tail may
\ be defined once in a wordlist, so a word that already exists is migrated into
\ a package and its callers are moved afterwards.
package NRT-CHAIN
public
s" : NRT-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" 1 1 NREACH-TEST:REGS NMIGRATE:DEFINE
s" : NRT-TINY ( n -- n ) 7 and ;" 1 1 NREACH-TEST:REGS NMIGRATE:DEFINE
s" : NRT-ARITY ( n n -- n ) + 1 + 2 + 3 + 4 + 5 + ;" 2 1 NREACH-TEST:REGS NMIGRATE:DEFINE
;package

\ The same body once more, compiled by the ENGINE under the same tail. Nothing
\ published it through the seam, so nothing may be moved onto it.
package NRT-ENGINE
public
: NRT-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;
;package

\ The two routines the unsafe-move case is about: one body that needs three
\ scratch registers and one that keeps eight sums live at once, so the second
\ really destroys registers the first does not.
\
\ AND THE NARROW ONE HAS TO BE A ROUTINE THE ENGINE CALLS. The whole case is
\ about call sites surviving a refusal, so a narrow body the engine COPIES into
\ its caller leaves the case with no sites to be about - which is exactly what
\ happened when this body was six additions of small constants: once the
\ selection stage emitted the immediate forms none of those constants was
\ materialised, the whole emission fitted in thirty-two bytes against the
\ engine's forty-byte copy limit, and the caller stopped holding a call at all.
\ So the body is chosen for a pressure no later pass can take away, the same way
\ native-clobber.f chooses its own: a multiplication has no immediate form for a
\ selection stage to fold, five distinct constants each read once give a memo
\ nothing to share, and mixing xor and and in makes the routine not a linear
\ function of its argument, so no reassociation can gather it into one operation.
\ The case does not take that on trust either - it asks the scanner whether the
\ engine would copy this routine before it makes any claim about sites, so a
\ compiler that did shrink it again fails at a case that names the real reason.
package NRT-NARROW
public
s" : NRT-CL ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 NREACH-TEST:NARROW-REGS NMIGRATE:DEFINE
;package

package NRT-WIDE-P
public
s" : NRT-CL ( n -- n ) {: s:n :} s 1 + s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + + + + + + + + ;"
   1 1 NREACH-TEST:WIDE-REGS NMIGRATE:DEFINE
;package

\ A caller of the narrow routine, compiled after it, so the refused move has
\ something it would have written to.
package NRT-BASE
public
: NRT-CL-CALLER ( n -- n ) NRT-NARROW:NRT-CL NRT-NARROW:NRT-CL ;
;package

\ A migrated routine that calls the word it shares its name with.
package NRT-SELFP
public
s" NRT-BASE:NRT-SELF" s" NRT-BASE:NRT-SELF" CODEGEN-SCAN:WORD-ENTRY 1 1 NMIGRATE:CALLEE
s" : NRT-SELF ( n -- n ) NRT-BASE:NRT-SELF 1 + ;" 1 1 NREACH-TEST:REGS NMIGRATE:DEFINE-CALLING
;package

package NREACH-TEST
public

: MAIN ( -- )
   COPY-CASES
   CLOBBER-CASES
   REFUSAL-CASES
   MOVE
   AFTER-CASES
   T-REPORT
   s" native-reach: ok" type cr ;

;package

NREACH-TEST:MAIN
