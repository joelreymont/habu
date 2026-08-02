\ native-migrate.f - the production entry, end to end: a definition the engine
\ compiles, recompiled by the native chain and republished under its own name.
\ One concern: src/compiler/native/migrate.f.
\
\ WHAT THIS SUITE HAS TO SHOW.
\
\   1. That the migrated word IS the chain's code. The old emitter and the new
\      chain compute the same answers - that is the point of the chain - so an
\      answer test alone cannot tell which one ran. What tells them apart is the
\      record: after the migration the word's code address is the one the
\      publication seam claimed, and its length is the emission's, which the code
\      the engine compiled was not.
\   2. That every existing caller reaches it as an ordinary word. A definition
\      compiled after the migration calls it, and the interpreter enters it, and
\      both answer what the definition says - with no address pushed and nothing
\      executed.
\   3. That a word the chain CANNOT compile is refused by name and left alone.
\      The engine has already published it by then, so the failure has to leave a
\      working word behind: the case below migrates a body with a word outside
\      the dialect's vocabulary, checks the refusal is the dialect's own, and then
\      checks that the word still runs and that its record never moved.
\
\ THE TAPE IS THE CHECKER'S. Nothing in this suite lexes anything. The source is
\ handed to the engine, the engine compiles it the way it compiles every
\ definition, and the tape the chain elaborates is the one the checker's own
\ reader filled while it certified that body - which is why the migrated word and
\ the word the checker accepted cannot be two different programs.

require lib/test.f
require test/checker-assert.f
require src/compiler/native/migrate.f

package NMIGRATE-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

4 constant REGS
4 constant INSN-BYTES
0 constant GLOBAL-WID

variable OLD-START
variable OLD-LEN

\ The migrations run at top level, so the words they publish are global: that is
\ what makes them reachable by the callers this suite compiles afterwards, and it
\ is the position an ordinary program's definitions occupy.
: REC ( ptr u8 n -- ptr a )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NPUB-NAME throw then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

: REC-LEN ( ptr u8 n -- n )
   REC XREF-LEN ;

\ Whether a record of this name exists at all. A refusal that happens before the
\ engine has compiled the definition leaves no word behind, which is a different
\ statement from "the word is there and unchanged" and needs a reader that does
\ not throw on a name nothing carries.
: DEFINED? ( ptr u8 n -- bool )
   GLOBAL-WID XREF-FIND-WL XREF-FOUND? ;

\ ---- a word the chain can compile --------------------------------------------
: SQ-SRC ( -- ptr u8 n )
   s" : NMG-SQ ( n -- n ) dup * ;" ;

: MIGRATE-SQ ( -- )
   SQ-SRC 1 1 REGS NMIGRATE:DEFINE ;

: MIGRATED-CASE ( -- )
   MIGRATE-SQ

   s" the migration answers the name the source published" T-LABEL
   NMIGRATE:NAME$ s" NMG-SQ" T$=
   NMIGRATE:WID GLOBAL-WID T=

   s" the record points at the code the publication seam claimed" T-LABEL
   s" NMG-SQ" REC-START
   s" NMG-SQ" GLOBAL-WID NPUB:NEW-START T=

   s" and its length is the chain's emission, not the engine's code" T-LABEL
   s" NMG-SQ" REC-LEN  s" NMG-SQ" GLOBAL-WID NPUB:NEW-LEN T=
   s" NMG-SQ" REC-LEN  s" NMG-SQ" GLOBAL-WID NPUB:OLD-LEN T<>

   s" a definition compiled afterwards calls it" T-LABEL
   s" : NMG-CALL ( n -- n ) NMG-SQ ;" EV
   s" 7 NMG-CALL" EV-N 49 T=
   s" 11 NMG-CALL" EV-N 121 T=

   s" and the interpreter enters it" T-LABEL
   s" 12 NMG-SQ" EV-N 144 T= ;

\ ---- one chain-compiled word calling another ---------------------------------
\ THE FIRST TWO-WORD PROGRAM THE CHAIN HAS COMPILED. Both words are migrated:
\ the callee first, so that it is a published record with an address of its own,
\ and then the caller, whose body names it. Nothing is entered through an address
\ anywhere in it - the interpreter runs the caller by name, the caller reaches
\ the callee with one direct branch, and the answer says both halves were right.
\
\ WHAT THE ANSWER PINS THAT NOTHING ELSE CAN. The two words compute different
\ things (`dup +` doubles, `1+` adds one), and the caller composes them, so
\ 5 -> 11 can only come out of a branch that landed on the callee's ENTRY and
\ came back. A displacement one instruction out lands after the callee's frame
\ has been taken, or in the middle of its argument load, and answers something
\ else or does not return at all; a branch to the wrong word answers that word's
\ function of 5.
\
\ AND WHERE THE CALLEE'S ADDRESS COMES FROM. The publication seam's own log,
\ which is the same authority the seam wrote the record from. This suite never
\ writes an address down.
: DBL-SRC ( -- ptr u8 n )
   s" : NMG-DBL ( n -- n ) dup + ;" ;

: USE-SRC ( -- ptr u8 n )
   s" : NMG-USE ( n -- n ) NMG-DBL 1+ ;" ;

: MIGRATE-DBL ( -- )
   DBL-SRC 1 1 REGS NMIGRATE:DEFINE ;

: DBL-ENTRY ( -- n )
   s" NMG-DBL" GLOBAL-WID NPUB:NEW-START ;

: MIGRATE-USE ( -- )
   USE-SRC
   s" NMG-DBL" DBL-ENTRY 1 1
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: CALL-CASE ( -- )
   MIGRATE-DBL
   MIGRATE-USE

   s" both words of the program are the chain's code" T-LABEL
   s" NMG-DBL" REC-START DBL-ENTRY T=
   s" NMG-USE" REC-START
   s" NMG-USE" GLOBAL-WID NPUB:NEW-START T=

   s" the caller reaches the callee and the program computes" T-LABEL
   s" 5 NMG-USE" EV-N 11 T=
   s" 0 NMG-USE" EV-N 1 T=
   s" -3 NMG-USE" EV-N -5 T=

   s" and the callee still answers on its own" T-LABEL
   s" 5 NMG-DBL" EV-N 10 T= ;

\ ---- three words deep --------------------------------------------------------
\ The same again with the caller itself called. NMG-L3 is a leaf, NMG-L2 calls
\ it, and NMG-L1 calls NMG-L2 - so the middle word is entered by a chain-compiled
\ branch AND makes one, which is the case that would find a routine that saved
\ its caller's return address in the wrong place: NMG-L2's own return address is
\ destroyed by the call it makes, so it has to be in its frame when it returns,
\ and the deepest call is what would overwrite it if it were not.
\
\ THE THREE STEPS ARE DIFFERENT FUNCTIONS ON PURPOSE. Doubling, adding one and
\ multiplying by three do not commute, so 2 -> ((2*2)+1)*3 = 15 fixes the whole
\ composition: any pair of the three branches going to the wrong word answers
\ something else.
: L3-SRC ( -- ptr u8 n )
   s" : NMG-L3 ( n -- n ) dup + ;" ;

: L2-SRC ( -- ptr u8 n )
   s" : NMG-L2 ( n -- n ) NMG-L3 1+ ;" ;

: L1-SRC ( -- ptr u8 n )
   s" : NMG-L1 ( n -- n ) NMG-L2 3 * ;" ;

: MIGRATE-L3 ( -- )
   L3-SRC 1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-L2 ( -- )
   L2-SRC
   s" NMG-L3"  s" NMG-L3" GLOBAL-WID NPUB:NEW-START  1 1
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-L1 ( -- )
   L1-SRC
   s" NMG-L2"  s" NMG-L2" GLOBAL-WID NPUB:NEW-START  1 1
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: DEEP-CASE ( -- )
   MIGRATE-L3
   MIGRATE-L2
   MIGRATE-L1

   s" all three words are the chain's code" T-LABEL
   s" NMG-L3" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NMG-L2" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NMG-L1" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" a call through a word that itself calls composes correctly" T-LABEL
   s" 2 NMG-L1" EV-N 15 T=
   s" 2 NMG-L2" EV-N 5 T=
   s" 2 NMG-L3" EV-N 4 T=
   s" 5 NMG-L1" EV-N 33 T= ;

\ ---- a chain-compiled word calling an ENGINE-compiled one --------------------
\ THE INTEROP QUESTION, ASKED WHERE IT CAN BE ANSWERED. NMG-ENG is never
\ migrated: it is compiled by the engine's own emitter and stays that way, and a
\ chain-compiled caller branches to its record. Nothing about the call site
\ changes - the same three instructions, the same slots - which is the point.
\
\ WHY THE CALLER'S DISCIPLINE COVERS IT. The site writes every value the caller
\ still holds into a slot of the caller's own data stack BELOW the callee's
\ argument base and reads it back out afterwards, so it assumes nothing about
\ which registers the callee destroys. What it does assume is the convention
\ itself: the callee takes its arguments out of the caller's slots, leaves its
\ results in them, keeps the engine's data-stack pointer, and returns through the
\ link register. That is what a word of this engine IS - it is how the
\ interpreter enters every one of them - so an engine-compiled callee keeps it by
\ construction. The live value in this body is what proves it: `dup` keeps a copy
\ of the argument across the call, so a callee that clobbered a caller register
\ would show up in the sum.
: ENG-SRC ( -- ptr u8 n )
   s" : NMG-ENG ( n -- n ) dup + 7 + ;" ;

: VIA-SRC ( -- ptr u8 n )
   s" : NMG-VIA ( n -- n ) dup NMG-ENG + ;" ;

: MIGRATE-VIA ( -- )
   VIA-SRC
   s" NMG-ENG"  s" NMG-ENG" REC-START  1 1
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: INTEROP-CASE ( -- )
   ENG-SRC EV

   s" the callee is the engine's own code, not the chain's" T-LABEL
   s" NMG-ENG" GLOBAL-WID NPUB:REPUBLISHED? TFALSE

   MIGRATE-VIA

   s" the caller is the chain's" T-LABEL
   s" NMG-VIA" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" a chain-compiled word calls an engine-compiled one and keeps its own value"
   T-LABEL
   s" 4 NMG-VIA" EV-N 19 T=
   s" 0 NMG-VIA" EV-N 7 T=
   s" -2 NMG-VIA" EV-N 1 T= ;

\ ---- a call from inside a counted loop ---------------------------------------
\ THE SHAPE THAT MISCOMPILED, AND WHY AN ANSWER TEST FINDS IT. A counted loop
\ carries its index and its limit in registers, and a chain-compiled callee's
\ contract declares the whole register pool destroyed - both pools starting at
\ the same register. The call site used to publish only the values the compile
\ time vector held, so the callee came back having overwritten the loop's own
\ counter: `4 NMG-LC` answered 0 where 12 is right, and the variant that never
\ reads the index answered 36 where 24 is right, because the LIMIT was the value
\ the callee had trampled and the loop ran six turns instead of four. Dot
\ habu-save-the-loop-5f07e0c3.
\
\ THE TWO BODIES SEPARATE THE TWO HALVES OF THE STATE. NMG-LC reads `i` in its
\ body, so a wrong index shows up in the sum; NMG-LF never mentions `i`, so
\ nothing but the TURN COUNT can be wrong, and 24 against 36 is exactly the
\ measurement of "the limit survived the call". A third body nests one loop
\ inside another, where the outer loop's counters have to cross the inner loop's
\ edges as well as the call.
\
\ AND THE CALLEE IS CHAIN-COMPILED ON PURPOSE. An engine-compiled callee answers
\ correctly here whatever the site saves, because the engine's emitter happens to
\ use registers the loop is not carrying - INTEROP-CASE above measures that, and
\ it is not evidence about this. NMG-DBL is migrated first, so both halves of the
\ program are the chain's code and the callee's declared contract is what the
\ caller has to survive.
: LC-SRC ( -- ptr u8 n )
   s" : NMG-LC ( n -- n ) 0 swap 0 ?do i NMG-DBL + loop ;" ;

: LF-SRC ( -- ptr u8 n )
   s" : NMG-LF ( n -- n ) 0 swap 0 ?do 3 NMG-DBL + loop ;" ;

: LN-SRC ( -- ptr u8 n )
   s" : NMG-LN ( n -- n ) 0 swap 0 ?do 4 0 ?do i NMG-DBL + loop loop ;" ;

\ A loop carries its two counters and each call site publishes them beside the
\ vector, so this budget is wider than a leaf's. It is a budget: dot
\ habu-choose-the-register-a95390ac carries taking the number off the routine.
16 constant LOOP-REGS

: MIGRATE-LC ( -- )
   LC-SRC  s" NMG-DBL" DBL-ENTRY 1 1  1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-LF ( -- )
   LF-SRC  s" NMG-DBL" DBL-ENTRY 1 1  1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-LN ( -- )
   LN-SRC  s" NMG-DBL" DBL-ENTRY 1 1  1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

: LOOP-CALL-CASE ( -- )
   MIGRATE-LC
   MIGRATE-LF
   MIGRATE-LN

   s" all three loops and their callee are the chain's code" T-LABEL
   s" NMG-DBL" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NMG-LC" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NMG-LF" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NMG-LN" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" a chain-compiled callee in a ?do body leaves the loop's index alone"
   T-LABEL
   s" 4 NMG-LC" EV-N 12 T=
   s" 1 NMG-LC" EV-N 0 T=
   s" 0 NMG-LC" EV-N 0 T=
   s" 5 NMG-LC" EV-N 20 T=

   s" and leaves its LIMIT alone, which is the turn count" T-LABEL
   s" 4 NMG-LF" EV-N 24 T=
   s" 1 NMG-LF" EV-N 6 T=
   s" 0 NMG-LF" EV-N 0 T=
   s" 6 NMG-LF" EV-N 36 T=

   s" and a loop inside a loop keeps both of its counters" T-LABEL
   s" 4 NMG-LN" EV-N 48 T=
   s" 1 NMG-LN" EV-N 12 T=
   s" 0 NMG-LN" EV-N 0 T= ;

\ ---- what a call to another word refuses -------------------------------------
\ Each of these is a callee statement the chain cannot turn into a branch, and
\ each is refused by the authority that owns the fact - not one code for "the
\ call is wrong". They are run through the production entry, on real source, so
\ what is measured is the path a program takes.
\
\   the null address        no code lives there, and the word model says so
\   an address that is not  no instruction begins there, and the machine dialect
\     four-byte aligned     says so
\   an address out of reach a Bl carries a 26-bit displacement, and the emitter
\                           says so - the encoder masks that field rather than
\                           bounding it, so a target too far away would silently
\                           become a branch somewhere else
\   more arguments than     the vector cannot hand over values it does not hold,
\     the caller holds      and the elaborator says so
\
\ AND EVERY ONE OF THEM LEAVES THE WORD THE ENGINE PUBLISHED RUNNING, which the
\ last case measures: the definitions are compiled by the engine before the chain
\ refuses them, so a refusal that had already written something would show up as
\ a word that no longer answers.
\ Each case gets a definition of its own, because the engine PUBLISHES the word
\ before the chain refuses it - that is the state a refusal has to leave working
\ - so a second case reusing the name would be a duplicate definition rather than
\ a second measurement.
$10000000 constant FAR-ENOUGH         \ 256 MiB: well past the reach of a Bl's 26-bit field

: MIGRATE-NULL-ENTRY ( -- )
   s" : NMG-B1 ( n -- n ) NMG-DBL 1+ ;"
   s" NMG-DBL" 0 1 1  1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-ODD-ENTRY ( -- )
   s" : NMG-B2 ( n -- n ) NMG-DBL 1+ ;"
   s" NMG-DBL" DBL-ENTRY 2 + 1 1  1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-FAR-ENTRY ( -- )
   s" : NMG-B3 ( n -- n ) NMG-DBL 1+ ;"
   s" NMG-DBL" DBL-ENTRY FAR-ENOUGH + 1 1  1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ A body the checker certifies, whose callee is STATED to take two values where
\ the compile-time vector holds one. It is the arity half of the same statement:
\ what a call site publishes is the arity it was told, and a site told to publish
\ more than the caller holds has nothing to publish.
: MIGRATE-DEEP-ARITY ( -- )
   s" : NMG-B4 ( n -- n ) NMG-DBL ;"
   s" NMG-DBL" DBL-ENTRY 2 1  1 1 REGS NMIGRATE:DEFINE-CALLING ;

: CALL-REFUSAL-CASES ( -- )
   s" a callee at the null address is refused by the word model" T-LABEL
   [: MIGRATE-NULL-ENTRY ;] E-HIR-CALLEE TTHROWSQ

   s" a callee address that is no instruction is refused by the dialect" T-LABEL
   [: MIGRATE-ODD-ENTRY ;] E-A64IR-ENTRY TTHROWSQ

   s" a callee out of the branch's reach is refused by the emitter" T-LABEL
   [: MIGRATE-FAR-ENTRY ;] E-A64EMIT-REACH TTHROWSQ

   s" a call site told to publish more than the caller holds is refused" T-LABEL
   [: MIGRATE-DEEP-ARITY ;] E-NELAB-CALL TTHROWSQ

   s" a callee the word model refuses is refused before the engine compiles"
   T-LABEL
   s" NMG-B1" DEFINED? TFALSE

   s" every word the engine did publish still runs its own code" T-LABEL
   s" 5 NMG-B2" EV-N 11 T=
   s" 5 NMG-B3" EV-N 11 T=
   s" 5 NMG-B4" EV-N 10 T=
   s" NMG-B2" GLOBAL-WID NPUB:REPUBLISHED? TFALSE
   s" NMG-B3" GLOBAL-WID NPUB:REPUBLISHED? TFALSE
   s" NMG-B4" GLOBAL-WID NPUB:REPUBLISHED? TFALSE

   s" and a migration still runs after every one of those refusals" T-LABEL
   s" : NMG-AFTER ( n -- n ) NMG-DBL 1+ ;"
   s" NMG-DBL" DBL-ENTRY 1 1  1 1 REGS NMIGRATE:DEFINE-CALLING
   s" 6 NMG-AFTER" EV-N 13 T=
   s" NMG-AFTER" GLOBAL-WID NPUB:REPUBLISHED? TTRUE ;

\ ---- a word the chain cannot compile ------------------------------------------
\ `mod` is not one of the source words the dialect models, so the elaborator
\ refuses the body by that dialect's own name. The engine has already published
\ the word at that point, which is exactly the state a refusal has to leave
\ working.
: MOD-SRC ( -- ptr u8 n )
   s" : NMG-MOD ( n -- n ) dup 5 mod + ;" ;

: MIGRATE-MOD ( -- )
   MOD-SRC 1 1 REGS NMIGRATE:DEFINE ;

: REFUSED-CASE ( -- )
   s" a body outside the dialect is refused with the dialect's own code" T-LABEL
   [: MIGRATE-MOD ;] E-HIR-UNMODELED TTHROWSQ

   s" the word the engine published is still there and still runs" T-LABEL
   s" 12 NMG-MOD" EV-N 14 T=
   s" 40 NMG-MOD" EV-N 40 T=

   s" and the publication seam never logged it" T-LABEL
   s" NMG-MOD" GLOBAL-WID NPUB:REPUBLISHED? TFALSE ;

\ The record of the refused word, read before the migration is attempted and
\ again after it, so "untouched" is a measurement rather than an inference. The
\ definition is made here and migrated in the case above, which is why the two
\ halves are separate words: the record has to be read between them.
: MOD-BEFORE ( -- )
   s" : NMG-MOD2 ( n -- n ) dup 5 mod + ;" EV
   s" NMG-MOD2" REC-START OLD-START !
   s" NMG-MOD2" REC-LEN OLD-LEN ! ;

: MIGRATE-MOD2 ( -- )
   s" : NMG-MOD3 ( n -- n ) dup 5 mod + ;" 1 1 REGS NMIGRATE:DEFINE ;

: UNTOUCHED-CASE ( -- )
   MOD-BEFORE
   [: MIGRATE-MOD2 ;] E-HIR-UNMODELED TTHROWSQ

   s" a refusal leaves the record of the word it was given exactly as it was" T-LABEL
   s" NMG-MOD3" REC-START  s" NMG-MOD3" REC-LEN  {: st:n ln:n :}
   s" NMG-MOD2" REC-LEN OLD-LEN @ T=
   s" NMG-MOD2" REC-START OLD-START @ T=

   s" and the word it refused still runs the code the engine compiled for it" T-LABEL
   ln s" NMG-MOD2" REC-LEN T=
   st 0 T<>
   s" 12 NMG-MOD3" EV-N 14 T= ;

\ ---- what the entry itself refuses --------------------------------------------
\ A source that publishes no definition never opens a scan, so the recorder's own
\ state machine is what refuses: the unit is still armed when it is asked to
\ close. A source with a second declaration of any kind - another definition, or
\ a `variable` - opens a second scan, and the recorder refuses that too, because
\ one unit is one scan.
\
\ THE MIGRATION'S OWN COUNT OF PUBLISHED RECORDS HAS NO FIXTURE, AND THAT IS
\ WORTH SAYING. It refuses a source that published more than one dictionary
\ record from ONE scan, which would leave the migration holding one definition's
\ tape and another definition's record. Every source a caller can write reaches
\ the recorder's one-scan rule first, so the count is a guard against a shape the
\ recorder cannot see rather than one this suite can build. It is load-bearing
\ all the same: without it the newest record is whatever the source published
\ last, and the tape would be elaborated onto it.
: NO-DEFINITION ( -- )
   s" 1 2 + drop" 0 0 REGS NMIGRATE:DEFINE ;

: TWO-DEFINITIONS ( -- )
   s" : NMG-A ( -- n ) 1 ; : NMG-B ( -- n ) 2 ;" 0 1 REGS NMIGRATE:DEFINE ;

\ One checked definition and one declaration beside it.
: DEFINITION-AND-DATA ( -- )
   s" : NMG-C ( -- n ) 3 ; variable NMG-V" 0 1 REGS NMIGRATE:DEFINE ;

: ENTRY-CASES ( -- )
   s" a source that publishes no definition records no scan" T-LABEL
   [: NO-DEFINITION ;] E-NFEED-STATE TTHROWSQ

   s" a source with two definitions opens a second scan and is refused" T-LABEL
   [: TWO-DEFINITIONS ;] E-NFEED-SCAN TTHROWSQ

   s" so does a source that declares anything beside its definition" T-LABEL
   [: DEFINITION-AND-DATA ;] E-NFEED-SCAN TTHROWSQ

   s" and a migration still runs after every one of those" T-LABEL
   s" : NMG-TRIPLE ( n -- n ) dup dup + + ;" 1 1 REGS NMIGRATE:DEFINE
   s" 7 NMG-TRIPLE" EV-N 21 T=
   s" NMG-TRIPLE" GLOBAL-WID NPUB:REPUBLISHED? TTRUE ;

public

\ ---- the scalar float vocabulary, end to end ---------------------------------
\ Every float word the dialect models, migrated through the production entry and
\ compared with the INTERPRETED word on the same input. Nothing here states an
\ expected number: a table of them would be a second opinion about IEEE754, and
\ what has to be true is that the compiled word and the engine's own primitive
\ answer the same cell. The cell is the whole comparison - two doubles that
\ differ in one bit are two different numbers - so the sign of a zero, a NaN
\ payload and a rounding are all inside it.
\
\ THE INPUTS ARE THE ONES A FLOAT CODE GENERATOR CAN GET WRONG WHILE EVERY
\ ORDINARY INPUT STILL LOOKS RIGHT: a division by zero, which answers an infinity
\ rather than trapping; zero over zero and the square root of a negative, which
\ answer the default NaN; a negative zero, which is a different cell from zero and
\ the same number; 2^53+1, which does not fit a double and pins `s>f`'s
\ round-to-nearest; and a truncating `f>s` on both signs and on a NaN. They are
\ the facts the survey at the head of tools/codegen-compare-corpus3.f establishes
\ about this engine.
: FLOAT-MIGRATIONS ( -- )
   s" : NMG-FADD ( r r -- r ) f+ ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FSUB ( r r -- r ) f- ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FMUL ( r r -- r ) f* ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FDIV ( r r -- r ) f/ ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FNEG ( r -- r ) fnegate ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-FABS ( r -- r ) fabs ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-FSQRT ( r -- r ) fsqrt ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-SF ( n -- r ) s>f ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-FS ( r -- n ) f>s ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-FLIT ( r -- r ) 0.25 f+ ;" 1 1 REGS NMIGRATE:DEFINE ;

: FLOAT-CASE ( -- )
   FLOAT-MIGRATIONS

   s" the four float operations answer what the engine's own primitives answer" T-LABEL
   s" 1.5 2.25 NMG-FADD" EV-N  s" 1.5 2.25 f+" EV-N T=
   s" 1.5 2.25 NMG-FSUB" EV-N  s" 1.5 2.25 f-" EV-N T=
   s" 1.5 2.25 NMG-FMUL" EV-N  s" 1.5 2.25 f*" EV-N T=
   s" 1.5 2.25 NMG-FDIV" EV-N  s" 1.5 2.25 f/" EV-N T=

   s" and the subtraction and the division keep the side they take from" T-LABEL
   s" 1.5 2.25 NMG-FSUB" EV-N  s" 2.25 1.5 f-" EV-N T<>
   s" 1.5 2.25 NMG-FDIV" EV-N  s" 2.25 1.5 f/" EV-N T<>

   s" a division by zero answers an infinity and does not trap" T-LABEL
   s" 1.0 0.0 NMG-FDIV" EV-N  s" 1.0 0.0 f/" EV-N T=
   s" -1.0 0.0 NMG-FDIV" EV-N  s" -1.0 0.0 f/" EV-N T=

   s" and zero over zero and the root of a negative answer the same NaN" T-LABEL
   s" 0.0 0.0 NMG-FDIV" EV-N  s" 0.0 0.0 f/" EV-N T=
   s" -1.0 NMG-FSQRT" EV-N  s" -1.0 fsqrt" EV-N T=
   s" 0.0 0.0 NMG-FDIV" EV-N  s" -1.0 NMG-FSQRT" EV-N T=

   s" the three unary operations answer what their primitives answer" T-LABEL
   s" -2.5 NMG-FNEG" EV-N  s" -2.5 fnegate" EV-N T=
   s" -2.5 NMG-FABS" EV-N  s" -2.5 fabs" EV-N T=
   s" 2.0 NMG-FSQRT" EV-N  s" 2.0 fsqrt" EV-N T=

   s" and negating a zero answers the OTHER zero, which is another cell" T-LABEL
   s" 0.0 NMG-FNEG" EV-N  s" 0.0 fnegate" EV-N T=
   s" 0.0 NMG-FNEG" EV-N  s" 0.0" EV-N T<>

   s" the two conversions round the two different ways their instructions do" T-LABEL
   s" 9007199254740993 NMG-SF" EV-N  s" 9007199254740993 s>f" EV-N T=
   s" 2.7 NMG-FS" EV-N  2 T=
   s" -2.7 NMG-FS" EV-N  -2 T=
   s" -0.5 NMG-FS" EV-N  0 T=
   s" 0.0 0.0 f/ NMG-FS" EV-N  0 T=

   s" a float literal in a compiled body is the double the interpreter pushes" T-LABEL
   s" 1.5 NMG-FLIT" EV-N  s" 1.5 0.25 f+" EV-N T=
   s" -0.25 NMG-FLIT" EV-N  s" 0.0" EV-N T= ;

\ ---- the two straight-line shapes the third corpus is measured on ------------
\ A locals frame with float arithmetic over it, and the conversion body. They are
\ tools/codegen-compare-corpus3.f's SGD and SEG-1/SQRT, and they are here as well
\ as there because the comparison harness is not a gate for the cost column while
\ this is: what this asserts is that the compiled word and the interpreted word
\ agree, on the inputs that separate one lowering from another.
: SHAPE-MIGRATIONS ( -- )
   s" : NMG-SGD ( r r r -- r ) {: w g lr :} w  lr g f* f- ;" 3 1 REGS NMIGRATE:DEFINE
   s" : NMG-SEG ( n -- r ) {: d:n :} 1.0 d s>f fsqrt f/ ;" 1 1 REGS NMIGRATE:DEFINE ;

: SHAPE-CASE ( -- )
   SHAPE-MIGRATIONS

   s" float arithmetic over a locals frame answers what the same body answers" T-LABEL
   s" 1.0 0.5 0.25 NMG-SGD" EV-N  s" 1.0 0.5 0.25 f* f-" EV-N T=
   s" -2.0 -0.5 0.25 NMG-SGD" EV-N  s" -2.0 -0.5 0.25 f* f-" EV-N T=
   s" -0.0 0.0 1.0 NMG-SGD" EV-N  s" -0.0 0.0 1.0 f* f-" EV-N T=

   s" and a step from a negative zero stays a negative zero" T-LABEL
   s" -0.0 0.0 1.0 NMG-SGD" EV-N  s" 0.0" EV-N T<>

   s" the conversion body answers what the same body answers" T-LABEL
   s" 4 NMG-SEG" EV-N  s" 1.0 4 s>f fsqrt f/" EV-N T=
   s" 2 NMG-SEG" EV-N  s" 1.0 2 s>f fsqrt f/" EV-N T=
   s" 9007199254740993 NMG-SEG" EV-N  s" 1.0 9007199254740993 s>f fsqrt f/" EV-N T=

   s" including the degenerate lengths, which do not trap" T-LABEL
   s" 0 NMG-SEG" EV-N  s" 1.0 0.0 f/" EV-N T=
   s" -4 NMG-SEG" EV-N  s" 0.0 0.0 f/" EV-N T= ;

\ ---- what a float body may NOT do yet, refused by name -----------------------
\ A double is a value of a second register class, and this leaf places it in a
\ straight line only. Two shapes of well typed Habu are therefore refused by the
\ chain, each with the elaborator's own E-NELAB-TYPE rather than as a wrong
\ lowering discovered later: a double stored into a memory cell, and a double
\ carried across a loop edge. Dots habu-store-a-double-a31b313e and
\ habu-carry-a-double-570d2f5c carry them.
\
\ THE THIRD SHAPE NEVER REACHES THE CHAIN, AND THAT IS THE RESULT. Handing a
\ double to an operation that computes with cells - `1.0 f+ 1 +` - is refused by
\ the CHECKER, before the engine has compiled anything and before a tape exists,
\ so the elaborator's own refusal for it is fail-closed rather than reachable
\ from checked source. It is still written, and the case below is what says which
\ of the two authorities does the refusing: a leaf that assumed the checker would
\ always be there first would have no answer the day a body reaches the chain
\ some other way.
: FLOAT-STORE ( -- )
   s" : NMG-BAD2 ( r ptr a -- ) {: v:r b:ptr :} v 1.0 f+ b ! ;" 2 0 REGS NMIGRATE:DEFINE ;

: FLOAT-EDGE ( -- )
   s" : NMG-BAD3 ( r n -- r ) 0 ?do 1.0 f+ loop ;" 2 1 REGS NMIGRATE:DEFINE ;

: FLOAT-REFUSAL-CASES ( -- )
   s" a double stored into a memory cell is refused - the crossing is not placed yet" T-LABEL
   [: FLOAT-STORE ;] E-NELAB-TYPE TTHROWSQ
   s" a double carried across a loop edge is refused, not handed over as a cell" T-LABEL
   [: FLOAT-EDGE ;] E-NELAB-TYPE TTHROWSQ

   s" a double handed to an integer operation never reaches the chain at all" T-LABEL
   s" NMG-BAD1 ( r -- n ) 1.0 f+ 1 +" CHECK-QUIET-CANDIDATE! 0 T=
   s" NMG-OKAY ( r -- r ) 1.0 f+" CHECK-QUIET-CANDIDATE! -1 T=

   s" and a body the chain refused keeps the record the engine compiled for it" T-LABEL
   s" NMG-BAD2" DEFINED? TTRUE
   s" NMG-BAD3" DEFINED? TTRUE
   s" 2.5 3 NMG-BAD3" EV-N  s" 2.5 1.0 f+ 1.0 f+ 1.0 f+" EV-N T= ;

: RUN ( -- )
   T-RESET
   MIGRATED-CASE
   CALL-CASE
   DEEP-CASE
   INTEROP-CASE
   LOOP-CALL-CASE
   CALL-REFUSAL-CASES
   REFUSED-CASE
   UNTOUCHED-CASE
   ENTRY-CASES
   FLOAT-CASE
   SHAPE-CASE
   FLOAT-REFUSAL-CASES
   T-REPORT ;

;package

NMIGRATE-TEST:RUN
