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
require src/compiler/native/codewalk.f

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

\ ---- a word that passes nothing and calls -------------------------------------
\ THE SHAPE THAT WAS REFUSED UNTIL THE CONTRACT DECLARED ITS CONVENTION. A word
\ of arity ( -- ) names no data-stack place, because it has nothing to name one
\ for, and the selector used to read that silence as a register convention: it
\ built no entry, so there was no data-stack pointer for a call site to write
\ through and no frame for a return address, and every call in such a word was
\ refused with E-A64SEL-CALL. The whole-tree census counted 327 definitions
\ refused for exactly this and nothing else.
\
\ THE CASE RUNS THE WORD AND NOT ONLY THE COMPILER. A refusal that has been
\ turned off is worth nothing on its own; what has to be true is that the code
\ the chain emits for this shape does what the source says. So the migrated word
\ is entered twice through the interpreter and its effect on a cell nobody else
\ writes is read back: two calls to a word that adds one, and one to a word that
\ adds three, is five each time round.
: DEFINE-VOID-PARTS ( -- )
   s" variable NMG-VCELL" EV
   s" : NMG-VBUMP ( -- ) NMG-VCELL @ 1 + NMG-VCELL ! ;" EV
   s" : NMG-VADD3 ( n -- n ) 3 + ;" EV ;

: MIGRATE-VOID ( -- )
   s" : NMG-VCALLS ( -- ) NMG-VBUMP NMG-VBUMP NMG-VCELL @ NMG-VADD3 NMG-VCELL ! ;"
   0 0 REGS NMIGRATE:DEFINE ;

: VOID-CALL-CASE ( -- )
   DEFINE-VOID-PARTS
   MIGRATE-VOID

   s" a ( -- ) word that calls is compiled by the chain" T-LABEL
   NMIGRATE:NAME$ s" NMG-VCALLS" T$=
   s" NMG-VCALLS" REC-START
   s" NMG-VCALLS" GLOBAL-WID NPUB:NEW-START T=
   s" NMG-VCALLS" REC-LEN  s" NMG-VCALLS" GLOBAL-WID NPUB:NEW-LEN T=

   s" and running it does what its source says, twice over" T-LABEL
   s" 0 NMG-VCELL !" EV
   s" NMG-VCALLS NMG-VCELL @" EV-N 5 T=
   s" NMG-VCALLS NMG-VCELL @" EV-N 10 T=

   s" a definition compiled afterwards calls it too" T-LABEL
   s" : NMG-VOUTER ( -- n ) NMG-VCALLS NMG-VCELL @ ;" EV
   s" NMG-VOUTER" EV-N 15 T= ;

\ ---- a word whose values do not all fit its registers -------------------------
\ THE FIRST DEFINITION THE PRODUCTION ENTRY HAS COMPILED THROUGH THE SPILL
\ LOWERING. Eight sums of the argument are computed before any of them is added
\ up, so all eight are live at once, against a routine allowed four scratch
\ registers. Five of them cannot be kept in a register and go to the frame.
\
\ IT USED TO BE REFUSED, AND NOT FOR A REASON ABOUT THE PROGRAM. The allocator
\ was held to the frame its contract declared, migrate.f declared none, and so the
\ first value that needed a slot ended the compilation with E-A64RA-PRESSURE. The
\ frame is derived from what the walk needs now (habu-derive-a-routine-84ed36b6)
\ and EMITTED puts A64SPILL:REWRITE between the allocation and the acceptance,
\ which is what this case exists to hold.
\
\ WHY THE ANSWERS ARE THE PROOF AND NOT THE COMPILATION. A spilled value is one
\ stored to a slot and read back later, and every way of getting that wrong -
\ storing to the wrong slot, reloading before the store, reloading the slot
\ another value went to, losing a store - leaves a routine that still compiles
\ and answers something else. The eight terms are the argument plus one through
\ eight, so the result is eight times the argument plus thirty-six: the two
\ calls below pin those two numbers separately, which is to say they pin how many
\ terms survived AND which ones. A dropped term, a doubled term or two terms
\ confused with each other moves one of the answers.
\
\ AND THE SPILL COUNT IS ASSERTED BECAUSE THE ANSWERS ALONE CANNOT SEE IT. The
\ same body compiled with registers to spare answers 116 too. NMIGRATE:SPILLS is
\ what says this definition went through the lowering rather than round it, so a
\ change that quietly stopped spilling fails here instead of passing for the
\ wrong reason.
: SPILL-SRC ( -- ptr u8 n )
   s" : NMG-SPILL ( n -- n ) {: s:n :} s 1+ s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + + + + + + + + ;" ;

4 constant SPILL-REGS

: MIGRATE-SPILL ( -- )
   SPILL-SRC 1 1 SPILL-REGS NMIGRATE:DEFINE ;

: SPILL-CASE ( -- )
   MIGRATE-SPILL

   \ FOUR AND NOT FIVE SINCE THE CONSTANTS STOPPED TAKING REGISTERS. Every one of
   \ this body's eight terms adds a small number to the same argument, and the
   \ combine pass now folds each of those numbers into the addition's own
   \ immediate field, so eight move-wides are gone and the registers they were
   \ written into are never claimed. The pressure that decides this count is what
   \ the body needs LIVE AT ONCE, and eight of those live values were constants,
   \ so one fewer value reaches the frame. The count is still five short of what
   \ the definition needs, which is what keeps this a spill case at all - and the
   \ answers below are unchanged, which is what says the spill that remains is
   \ still correct.
   s" four values went to the frame, and the migration says so" T-LABEL
   NMIGRATE:SPILLS 4 T=

   s" the record points at the code the publication seam claimed" T-LABEL
   s" NMG-SPILL" REC-START
   s" NMG-SPILL" GLOBAL-WID NPUB:NEW-START T=

   s" and its length is the chain's emission, not the engine's code" T-LABEL
   s" NMG-SPILL" REC-LEN  s" NMG-SPILL" GLOBAL-WID NPUB:NEW-LEN T=

   s" the interpreter enters it and every term survives its slot" T-LABEL
   s" 0 NMG-SPILL" EV-N 36 T=
   s" 10 NMG-SPILL" EV-N 116 T=

   s" a definition compiled afterwards calls it" T-LABEL
   s" : NMG-SPILL-CALL ( n -- n ) NMG-SPILL ;" EV
   s" 100 NMG-SPILL-CALL" EV-N 836 T= ;

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
   s" NMG-DBL" DBL-ENTRY 1 1 NMIGRATE:CALLEE
   USE-SRC 1 1 REGS NMIGRATE:DEFINE-CALLING ;

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
   s" NMG-L3"  s" NMG-L3" GLOBAL-WID NPUB:NEW-START  1 1 NMIGRATE:CALLEE
   L2-SRC 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-L1 ( -- )
   s" NMG-L2"  s" NMG-L2" GLOBAL-WID NPUB:NEW-START  1 1 NMIGRATE:CALLEE
   L1-SRC 1 1 REGS NMIGRATE:DEFINE-CALLING ;

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
   s" NMG-ENG"  s" NMG-ENG" REC-START  1 1 NMIGRATE:CALLEE
   VIA-SRC 1 1 REGS NMIGRATE:DEFINE-CALLING ;

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
   s" NMG-DBL" DBL-ENTRY 1 1 NMIGRATE:CALLEE
   LC-SRC 1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-LF ( -- )
   s" NMG-DBL" DBL-ENTRY 1 1 NMIGRATE:CALLEE
   LF-SRC 1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-LN ( -- )
   s" NMG-DBL" DBL-ENTRY 1 1 NMIGRATE:CALLEE
   LN-SRC 1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

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
\ A callee is stated as a spelling and an address, and those are ONE fact: the
\ caller got the address by resolving the spelling, and the body reaches the
\ routine by writing it. So the first four cases below are all one statement
\ contradicting itself - a spelling whose word does not begin where the address
\ says - and the migration entry refuses every one of them by its own name, at
\ the moment they are staged, before the engine has compiled anything at all.
\
\   the null address        no word begins at zero
\   an address that is not  no word begins in the middle of an instruction
\     four-byte aligned
\   an address out of reach nothing this process published lives 256 MiB from
\                           the routine that would branch to it
\   a spelling that no      there is no word to have an address, and no second
\     lookup answers        authority to prefer
\
\ WHAT THE THREE ADDRESS SHAPES USED TO REACH, AND WHERE EACH IS PROVED NOW. A
\ caller could once state any number, so these three cases carried the refusals of
\ the three stages that would have met one: the word model's E-HIR-CALLEE, the
\ machine dialect's E-A64IR-ENTRY and the emitter's E-A64EMIT-REACH. Those stages
\ still refuse, and no production path can reach them any more, because a resolved
\ address is a real code address of this process by construction. The word model's
\ refusal is proved directly in test/compiler/native-hir.f, and the dialect's - a
\ null and an unaligned entry - in test/compiler/native-a64ir.f, where the reach
\ bound's own predicate is pinned at its exact edge as well. Restoring an
\ end-to-end assertion of the emitter's reach refusal, which now needs a module
\ built for it rather than a migration, is dot habu-reach-the-emitter-e23caccb.
\
\ AND NONE OF THEM PUBLISHES A WORD AT ALL, which the cases measure: the refusal
\ is a whole migration earlier than it was, so the definition that would have used
\ the staged callee is never handed to the engine. Each case keeps a name of its
\ own so that measurement is one assertion per case rather than one for all four.
\
\ THE ARITY HALF IS STILL THE CALLER'S TO GET WRONG, and the last two cases are
\ that half: what a call site publishes is the arity it was told, and a site told
\ to publish more than the caller holds has nothing to publish. Those two run the
\ whole migration, so the word IS published by the engine before the chain refuses
\ it - the state a refusal has to leave working.
$10000000 constant FAR-ENOUGH         \ 256 MiB: well past the reach of a Bl's 26-bit field

: MIGRATE-NULL-ENTRY ( -- )
   s" NMG-DBL" 0 1 1 NMIGRATE:CALLEE
   s" : NMG-B1 ( n -- n ) NMG-DBL 1+ ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-ODD-ENTRY ( -- )
   s" NMG-DBL" DBL-ENTRY 2 + 1 1 NMIGRATE:CALLEE
   s" : NMG-B2 ( n -- n ) NMG-DBL 1+ ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-FAR-ENTRY ( -- )
   s" NMG-DBL" DBL-ENTRY FAR-ENOUGH + 1 1 NMIGRATE:CALLEE
   s" : NMG-B3 ( n -- n ) NMG-DBL 1+ ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ A spelling nothing in this image carries, stated against a real address. It is
\ the other half of the same rule: an address is only a callee's when a spelling
\ says so, and a spelling that says nothing leaves the address unclaimed.
: MIGRATE-NO-SUCH-CALLEE ( -- )
   s" NMG-NOBODY" DBL-ENTRY 1 1 NMIGRATE:CALLEE
   s" : NMG-B6 ( n -- n ) NMG-NOBODY 1+ ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ A body the checker certifies, whose callee is STATED to take two values where
\ the compile-time vector holds one. It is the arity half of the same statement:
\ what a call site publishes is the arity it was told, and a site told to publish
\ more than the caller holds has nothing to publish.
\
\ THE CALLEE IS ONE THE ENGINE COMPILED, and that is what this case is about
\ rather than an accident of which word was handy. The chain recorded no body for
\ it and never will, so the caller's statement stands as made and the refusal is
\ the elaborator's own: the vector cannot hand over a value it does not hold. The
\ same lie about a callee the CHAIN compiled is caught earlier and by a different
\ name, because the callee's own migration recorded what it really declares, and
\ the case below measures that.
: DEFINE-ENGINE-DBL ( -- )
   s" : NMG-EDBL ( n -- n ) dup + ;" EV ;

: MIGRATE-DEEP-ARITY ( -- )
   s" NMG-EDBL" s" NMG-EDBL" REC-START 2 1 NMIGRATE:CALLEE
   s" : NMG-B4 ( n -- n ) NMG-EDBL ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ The same lie about a callee whose body the chain DID record, with enough on the
\ vector that nothing underflows: the caller says the callee takes two values and
\ the callee's own migration recorded that it takes one. Two authorities about
\ one routine, and they are held against each other rather than one of them being
\ believed - a caller compiled against the wrong effect would publish its
\ arguments at the wrong slots whether the body was copied in or branched to.
: MIGRATE-WRONG-ARITY ( -- )
   s" NMG-DBL" DBL-ENTRY 2 1 NMIGRATE:CALLEE
   s" : NMG-B5 ( n n -- n ) NMG-DBL + ;" 2 1 REGS NMIGRATE:DEFINE-CALLING ;

: CALL-REFUSAL-CASES ( -- )
   s" a callee stated at the null address is not the word the spelling names"
   T-LABEL
   [: MIGRATE-NULL-ENTRY ;] E-NMIGRATE-CALLEE TTHROWSQ

   s" nor is one stated in the middle of that word's first instruction" T-LABEL
   [: MIGRATE-ODD-ENTRY ;] E-NMIGRATE-CALLEE TTHROWSQ

   s" nor is one stated a quarter of a gigabyte away from it" T-LABEL
   [: MIGRATE-FAR-ENTRY ;] E-NMIGRATE-CALLEE TTHROWSQ

   s" and a spelling no lookup answers leaves a real address unclaimed" T-LABEL
   [: MIGRATE-NO-SUCH-CALLEE ;] E-NMIGRATE-CALLEE TTHROWSQ

   s" every one of them is refused before the engine compiles the caller"
   T-LABEL
   s" NMG-B1" DEFINED? TFALSE
   s" NMG-B2" DEFINED? TFALSE
   s" NMG-B3" DEFINED? TFALSE
   s" NMG-B6" DEFINED? TFALSE

   s" a call site told to publish more than the caller holds is refused" T-LABEL
   DEFINE-ENGINE-DBL
   [: MIGRATE-DEEP-ARITY ;] E-NELAB-CALL TTHROWSQ

   s" and an effect that is not the one the callee recorded is refused" T-LABEL
   [: MIGRATE-WRONG-ARITY ;] E-NELAB-INLINE TTHROWSQ

   s" while the words those two DID publish still run their own code" T-LABEL
   s" 5 NMG-B4" EV-N 10 T=
   s" 3 5 NMG-B5" EV-N 13 T=
   s" NMG-B4" GLOBAL-WID NPUB:REPUBLISHED? TFALSE
   s" NMG-B5" GLOBAL-WID NPUB:REPUBLISHED? TFALSE

   s" and a migration still runs after every one of those refusals" T-LABEL
   s" NMG-DBL" DBL-ENTRY 1 1 NMIGRATE:CALLEE
   s" : NMG-AFTER ( n -- n ) NMG-DBL 1+ ;" 1 1 REGS NMIGRATE:DEFINE-CALLING
   s" 6 NMG-AFTER" EV-N 13 T=
   s" NMG-AFTER" GLOBAL-WID NPUB:REPUBLISHED? TTRUE ;

\ ---- a word the chain cannot compile ------------------------------------------
\ WHY A NAME REACHED THROUGH `using` IS THE FIXTURE. A name the dialect does not
\ model is not refused for that alone: src/compiler/native/elaborate.f
\ RESOLVE-SCAN puts it to the engine, and a word the engine can name and the
\ checker can size becomes a call. That now includes every named CONSTANT - dot
\ habu-export-the-checker-2bbc831c gave the checker's stored effects a per-cell
\ width, so `-- a` sizes at one cell and `NMG-K +` compiles - which is why this
\ fixture is no longer written with a bare constant. It needs a body the chain
\ still cannot compile AFTER asking.
\
\ A `using` import is exactly that, and for a reason the resolver states about
\ itself. src/compiler/native/dict.f walks the open package's two wordlists and
\ then the global one, and deliberately does NOT walk the used-publics leg the
\ engine reaches after it: a word named through a `using` answers absent there
\ and the compilation is refused rather than made against an answer the chain
\ did not confirm. So the ENGINE compiles this body without complaint - the
\ import is ordinary, checked source - and the CHAIN declines the same spelling.
\ The elaborator refuses by the dialect's own name and says which token it was.
\
\ IT IS COUPLED TO A CAPABILITY AND THE COUPLING IS DELIBERATE. Dot
\ habu-walk-the-used-96694010 carries walking that leg; when it lands this body
\ compiles and this fixture must be changed with it. That is the point of a
\ fixture: it fails when the thing it describes stops being true. Both the
\ package and the bodies are published through the engine's own interpret path,
\ so the bodies name the constant at the same scope they are compiled in.
: MOD-CONST ( -- )
   s" package NMG-AWAY public 5 constant NMG-K ;package" EV ;

: MOD-SRC ( -- ptr u8 n )
   s" using NMG-AWAY : NMG-MOD ( n -- n ) NMG-K + ; ;using" ;

: MIGRATE-MOD ( -- )
   MOD-SRC 1 1 REGS NMIGRATE:DEFINE ;

\ ---- and the same shape the chain CAN compile ---------------------------------
\ The capability the refusal above is measured against, asserted rather than
\ assumed. A named constant reached the ordinary way - a global spelling, in
\ scope - is a word the engine names and the checker sizes at one cell, so the
\ chain compiles a call to it. That is what dot habu-export-the-checker-2bbc831c
\ landed: before it, the checker published an effect as a count of TERMS and a
\ per-term family that left `-- a` gray, and a body naming ANY constant was
\ refused exactly as the `using` body above still is. The two cases differ in one
\ thing only - whether the chain's resolver can reach the spelling - so putting
\ them beside each other is what stops the refusal from passing for the wrong
\ reason. The migrated word is RUN, because a call compiled against a width the
\ chain guessed would still migrate; only the answer shows the width was right.
: CONST-GLOBAL ( -- )
   s" 5 constant NMG-GK" EV ;

: MIGRATE-CONST ( -- )
   s" : NMG-KC ( n -- n ) NMG-GK + ;" 1 1 REGS NMIGRATE:DEFINE ;

: CONST-CALL-CASE ( -- )
   CONST-GLOBAL
   s" a body naming an in-scope constant is compiled, not refused" T-LABEL
   MIGRATE-CONST
   s" NMG-KC" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the migrated word answers what the constant is worth" T-LABEL
   s" 12 NMG-KC" EV-N 17 T=
   s" 40 NMG-KC" EV-N 45 T= ;

: REFUSED-CASE ( -- )
   s" a body outside the dialect is refused with the dialect's own code" T-LABEL
   [: MIGRATE-MOD ;] E-HIR-UNMODELED TTHROWSQ

   s" the word the engine published is still there and still runs" T-LABEL
   s" 12 NMG-MOD" EV-N 17 T=
   s" 40 NMG-MOD" EV-N 45 T=

   s" and the publication seam never logged it" T-LABEL
   s" NMG-MOD" GLOBAL-WID NPUB:REPUBLISHED? TFALSE ;

\ The record of the refused word, read before the migration is attempted and
\ again after it, so "untouched" is a measurement rather than an inference. The
\ definition is made here and migrated in the case above, which is why the two
\ halves are separate words: the record has to be read between them.
: MOD-BEFORE ( -- )
   s" using NMG-AWAY : NMG-MOD2 ( n -- n ) NMG-K + ; ;using" EV
   s" NMG-MOD2" REC-START OLD-START !
   s" NMG-MOD2" REC-LEN OLD-LEN ! ;

: MIGRATE-MOD2 ( -- )
   s" using NMG-AWAY : NMG-MOD3 ( n -- n ) NMG-K + ; ;using" 1 1 REGS NMIGRATE:DEFINE ;

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
   s" 12 NMG-MOD3" EV-N 17 T= ;

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

\ ---- a data word's address is the engine's answer, and it stays current -------
\ WHAT THIS SECTION IS FOR. A definition the chain compiles may name a `create`d
\ data word, and the routine it emits has to materialise that word's address as a
\ number. The caller used to hand the number over, having obtained it by running
\ the word a moment earlier; the chain asks the engine for it now, inside the
\ migration, through src/compiler/native/dict.f. These cases are what makes that
\ a change in kind rather than a change in spelling.
\
\ FIRST, THE ANSWER IS THE RIGHT ONE, AND IT IS CHECKED AGAINST THE OTHER ROUTE
\ TO IT. The word model resolves a wordlist and enters the record; the assertion
\ hands the same spelling to the interpreter. Two different paths through the
\ engine, one answer, so the agreement is a statement about the resolver and not
\ a restatement of it.
\
\ SECOND, THE SAME NUMBER EMITS THE SAME CODE. Two migrations of one body, under
\ two names, are compared instruction word by instruction word. This is what
\ makes the third case mean something: without it, "the bytes changed" could be
\ any two migrations differing for any reason at all.
\
\ THIRD, AND THIS IS THE ONE THE OLD SEAM COULD NOT SURVIVE. The data word is
\ retired and created again, which puts it at a different address, and the same
\ body is migrated a third time. Its code MUST differ from the first two, and the
\ routine MUST bump the new cell. The routine compiled before the retirement is
\ still published and still holds the old address, so running it must leave the
\ new cell alone - which is how a stale address is caught by name, with no test
\ reading raw memory. Under the caller-stated seam a harness that cached the
\ address kept compiling routines pointed at retired storage, and nothing
\ anywhere could tell the stale number from the live one. There is no number to
\ cache now.
64 constant SPAN-MAX                 \ instruction words one compared span may hold

create SPAN-A SPAN-MAX cells allot
create SPAN-B SPAN-MAX cells allot
here CELL 1- and CELL swap - CELL 1- and allot
variable SPAN-N
variable KEEP-N

\ The walk's callback. It writes into a cell rather than onto the stack for the
\ reason BRANCH-NOTE gives: a quotation cannot read the enclosing word's locals.
: SPAN-NOTE ( n n -- )
   {: at:n w:n :}
   SPAN-N @ SPAN-MAX >= if E-TEST-CAPACITY throw then
   w  SPAN-N @ cells SPAN-A + !
   SPAN-N @ 1+ SPAN-N ! ;

\ The instruction words of the record this spelling names, read through the same
\ walker the branch counts above use, and answering how many there were.
: SPAN-READ ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   0 SPAN-N !
   a u REC-START  a u REC-LEN  [: SPAN-NOTE ;] NWALK:SPAN-EACH
   SPAN-N @ ;

\ What was just read, kept aside so the next read can be held against it.
: SPAN-KEEP ( n -- )
   {: n:n :}
   n KEEP-N !
   n 0 ?do  i cells SPAN-A + @  i cells SPAN-B + !  loop ;

: SPAN-SAME? ( n -- bool )
   {: n:n :}
   n KEEP-N @ <> if false exit then
   n 0 ?do
      i cells SPAN-A + @  i cells SPAN-B + @ <> if false unloop exit then
   loop
   true ;

\ The data word the three migrations name, and the three bodies that name it.
\ They are one body under three names: the whole point of the comparison is that
\ nothing about the source differs between them.
: DAT-NEW ( -- )
   s" create NMG-DAT 1 cells allot" EV ;

: DAT-RETIRE ( -- )
   s" undefine NMG-DAT" EV ;

: BUMP1 ( -- )
   s" : NMG-BUMP1 ( n -- n ) NMG-DAT ! NMG-DAT @ 1+ dup NMG-DAT ! ;"
   s" NMG-DAT" 1 1 REGS NMIGRATE:DEFINE-DATA ;

: BUMP2 ( -- )
   s" : NMG-BUMP2 ( n -- n ) NMG-DAT ! NMG-DAT @ 1+ dup NMG-DAT ! ;"
   s" NMG-DAT" 1 1 REGS NMIGRATE:DEFINE-DATA ;

: BUMP3 ( -- )
   s" : NMG-BUMP3 ( n -- n ) NMG-DAT ! NMG-DAT @ 1+ dup NMG-DAT ! ;"
   s" NMG-DAT" 1 1 REGS NMIGRATE:DEFINE-DATA ;

\ The two shapes the query has no answer for. A word that leaves nothing and a
\ word that leaves two are both refused: what a fixed row holds is the one value
\ a word of that kind pushes, and neither of these is a word of that kind.
: VOID-WORDS ( -- )
   s" : NMG-VOID ( -- ) ;" EV
   s" : NMG-TWO ( -- n n ) 1 2 ;" EV ;

: ASK-ABSENT ( -- )
   s" NMG-NOT-A-WORD" NDICT:FIXED-VALUE drop ;

: ASK-VOID ( -- )
   s" NMG-VOID" NDICT:FIXED-VALUE drop ;

: ASK-TWO ( -- )
   s" NMG-TWO" NDICT:FIXED-VALUE drop ;

: DATA-CASES ( -- )
   DAT-NEW
   VOID-WORDS

   s" the engine's answer for a data word is the address its own name evaluates to" T-LABEL
   s" NMG-DAT" NDICT:FIXED-VALUE  s" NMG-DAT" EV-N T=

   s" a spelling that denotes no word has no answer" T-LABEL
   [: ASK-ABSENT ;] E-NDICT-NAME TTHROWSQ

   s" nor has a word that leaves no value where it was entered" T-LABEL
   [: ASK-VOID ;] E-NDICT-VALUE TTHROWSQ

   s" nor one that leaves two" T-LABEL
   [: ASK-TWO ;] E-NDICT-VALUE TTHROWSQ

   BUMP1
   BUMP2

   s" one body migrated twice emits the same instruction words" T-LABEL
   s" NMG-BUMP1" SPAN-READ SPAN-KEEP
   s" NMG-BUMP2" SPAN-READ SPAN-SAME? TTRUE

   s" and both routines bump the cell the word names" T-LABEL
   s" 4000 NMG-BUMP1" EV-N 4001 T=
   s" NMG-DAT @" EV-N 4001 T=
   s" 8000 NMG-BUMP2" EV-N 8001 T=
   s" NMG-DAT @" EV-N 8001 T=

   DAT-RETIRE
   DAT-NEW
   BUMP3

   s" the same body compiled after the word was retired and made again differs" T-LABEL
   s" NMG-BUMP3" SPAN-READ SPAN-SAME? 0= TTRUE

   s" the new routine bumps the new cell" T-LABEL
   s" 4000 NMG-BUMP3" EV-N 4001 T=
   s" NMG-DAT @" EV-N 4001 T=

   s" and the routine compiled before it does not: its address is the old one" T-LABEL
   s" 7000 NMG-BUMP1" EV-N 7001 T=
   s" NMG-DAT @" EV-N 4001 T= ;

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

\ ---- the five float comparisons, both ways they can be lowered ---------------
\ Each of the five is migrated twice: once as a body whose whole content is the
\ comparison, which materialises the Habu flag, and once as the comparison
\ feeding an `if`, which the selector fuses into a compare-and-branch. Both
\ shapes have to answer what the interpreted body answers, and they are two
\ different lowerings of the same source word - so a table that got one right and
\ the other wrong is caught here rather than in whichever corpus row happened to
\ use the other shape.
\
\ WHY THE INTERPRETED SIDE OF THE FUSED CASES IS A WORD AND NOT AN EXPRESSION.
\ `if` may not stand at the top level, so the engine's own answer for the branch
\ shapes is a definition beside the compiled one, spelled exactly the same way.
\ It is the ENGINE's compilation of that source, which is what the compiled word
\ has to agree with.
\
\ THE INPUTS ARE THE ONES A LOWERING CAN GET WRONG WHILE EVERY ORDINARY PAIR
\ STILL LOOKS RIGHT, and there are three families of them.
\
\   A NaN IN EACH OPERAND POSITION. Every float comparison this engine has
\   answers FALSE when either operand is a NaN (the survey at the head of
\   tools/codegen-compare-corpus3.f measured it), because an Fcmp raises the
\   unordered condition and the three conditions the engine names - MI, GT and
\   EQ - are all false under it. Under `lt`, `le` or `ne` the same flags read
\   TRUE, so a lowering that took a float comparison's condition from its
\   relation's NAME answers the opposite here, and the fused cases take the
\   opposite ARM. Both operand positions are asked because a lowering could be
\   wrong on one side only.
\
\   BOTH ARGUMENT ORDERS. `f<` and `f>` are the same instruction under two
\   conditions, so a swapped operand pair computes the other relation and is
\   invisible on any input where the two agree. The ordered pairs below disagree.
\
\   THE TWO ZEROS. A negative zero is a different cell from zero and the same
\   number, so `-0.0 f0=` is true and `-0.0 f0<` is false; a comparison done on
\   bits rather than on numbers answers the other way for both.
: FCMP-MIGRATIONS ( -- )
   s" : NMG-FLT ( r r -- bool ) f< ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FGT ( r r -- bool ) f> ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FEQ ( r r -- bool ) f= ;" 2 1 REGS NMIGRATE:DEFINE
   s" : NMG-FLTZ ( r -- bool ) f0< ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-FEQZ ( r -- bool ) f0= ;" 1 1 REGS NMIGRATE:DEFINE
   s" : NMG-BLT ( r r -- n ) {: x:r y:r :} x y f< if 1 else 2 then ;"
      2 1 REGS NMIGRATE:DEFINE
   s" : NMG-BGT ( r r -- n ) {: x:r y:r :} x y f> if 1 else 2 then ;"
      2 1 REGS NMIGRATE:DEFINE
   s" : NMG-BEQ ( r r -- n ) {: x:r y:r :} x y f= if 1 else 2 then ;"
      2 1 REGS NMIGRATE:DEFINE
   s" : NMG-BLTZ ( r -- n ) {: x:r :} x f0< if 1 else 2 then ;"
      1 1 REGS NMIGRATE:DEFINE
   s" : NMG-BEQZ ( r -- n ) {: x:r :} x f0= if 1 else 2 then ;"
      1 1 REGS NMIGRATE:DEFINE ;

: FCMP-FLAG-CASE ( -- )
   s" the five comparisons answer what the engine's own primitives answer" T-LABEL
   s" 1.5 2.25 NMG-FLT" EV-N   s" 1.5 2.25 f<" EV-N T=
   s" 2.25 1.5 NMG-FLT" EV-N   s" 2.25 1.5 f<" EV-N T=
   s" 2.25 1.5 NMG-FGT" EV-N   s" 2.25 1.5 f>" EV-N T=
   s" 1.5 2.25 NMG-FGT" EV-N   s" 1.5 2.25 f>" EV-N T=
   s" 1.5 1.5 NMG-FEQ" EV-N    s" 1.5 1.5 f=" EV-N T=
   s" 1.5 2.25 NMG-FEQ" EV-N   s" 1.5 2.25 f=" EV-N T=
   s" -1.5 NMG-FLTZ" EV-N      s" -1.5 f0<" EV-N T=
   s" 1.5 NMG-FLTZ" EV-N       s" 1.5 f0<" EV-N T=
   s" 0.0 NMG-FEQZ" EV-N       s" 0.0 f0=" EV-N T=
   s" 1.5 NMG-FEQZ" EV-N       s" 1.5 f0=" EV-N T=

   s" a comparison answers a Habu flag: all bits set or none, never one" T-LABEL
   s" 1.5 2.25 NMG-FLT" EV-N  -1 T=
   s" 2.25 1.5 NMG-FLT" EV-N  0 T=
   s" -1.5 NMG-FLTZ" EV-N  -1 T=
   s" 0.0 NMG-FEQZ" EV-N  -1 T=
   s" 1.5 2.25 NMG-FLT" EV-N  1 T<>

   s" every one of them is asked on all three orderings of its operands" T-LABEL
   s" 1.5 1.5 NMG-FLT" EV-N    0 T=
   s" 1.5 1.5 NMG-FGT" EV-N    0 T=
   s" 2.25 1.5 NMG-FEQ" EV-N   0 T=
   s" 1.5 1.5 NMG-FLT" EV-N    s" 1.5 1.5 f<" EV-N T=
   s" 1.5 1.5 NMG-FGT" EV-N    s" 1.5 1.5 f>" EV-N T=
   s" 2.25 1.5 NMG-FEQ" EV-N   s" 2.25 1.5 f=" EV-N T=
   s" 0.0 NMG-FLTZ" EV-N       0 T=
   s" -1.5 NMG-FEQZ" EV-N      0 T=
   s" 0.0 NMG-FLTZ" EV-N       s" 0.0 f0<" EV-N T=
   s" -1.5 NMG-FEQZ" EV-N      s" -1.5 f0=" EV-N T=

   s" and the two-operand ones compare the sides they are handed" T-LABEL
   s" 1.5 2.25 NMG-FLT" EV-N  s" 2.25 1.5 NMG-FLT" EV-N T<>
   s" 1.5 2.25 NMG-FLT" EV-N  s" 1.5 2.25 NMG-FGT" EV-N T<>
   s" 1.5 2.25 NMG-FLT" EV-N  s" 2.25 1.5 NMG-FGT" EV-N T=

   s" a NaN in either position answers false, for every one of the five" T-LABEL
   s" 0.0 0.0 f/ 1.5 NMG-FLT" EV-N   0 T=
   s" 1.5 0.0 0.0 f/ NMG-FLT" EV-N   0 T=
   s" 0.0 0.0 f/ 0.0 0.0 f/ NMG-FLT" EV-N  0 T=
   s" 0.0 0.0 f/ 1.5 NMG-FGT" EV-N   0 T=
   s" 1.5 0.0 0.0 f/ NMG-FGT" EV-N   0 T=
   s" 0.0 0.0 f/ 1.5 NMG-FEQ" EV-N   0 T=
   s" 1.5 0.0 0.0 f/ NMG-FEQ" EV-N   0 T=
   s" 0.0 0.0 f/ 0.0 0.0 f/ NMG-FEQ" EV-N  0 T=
   s" 0.0 0.0 f/ NMG-FLTZ" EV-N      0 T=
   s" 0.0 0.0 f/ NMG-FEQZ" EV-N      0 T=

   s" which is what the engine's own primitives answer for the same NaN" T-LABEL
   s" 0.0 0.0 f/ 1.5 NMG-FLT" EV-N   s" 0.0 0.0 f/ 1.5 f<" EV-N T=
   s" 1.5 0.0 0.0 f/ NMG-FGT" EV-N   s" 1.5 0.0 0.0 f/ f>" EV-N T=
   s" 0.0 0.0 f/ 0.0 0.0 f/ NMG-FEQ" EV-N  s" 0.0 0.0 f/ 0.0 0.0 f/ f=" EV-N T=
   s" 0.0 0.0 f/ NMG-FLTZ" EV-N      s" 0.0 0.0 f/ f0<" EV-N T=
   s" 0.0 0.0 f/ NMG-FEQZ" EV-N      s" 0.0 0.0 f/ f0=" EV-N T=

   s" the two zeros are equal numbers in different cells, and compare as numbers" T-LABEL
   s" -0.0 NMG-FEQZ" EV-N    -1 T=
   s" -0.0 NMG-FLTZ" EV-N    0 T=
   s" -0.0 0.0 NMG-FEQ" EV-N -1 T=
   s" -0.0 0.0 NMG-FLT" EV-N 0 T=
   s" -0.0 NMG-FEQZ" EV-N    s" -0.0 f0=" EV-N T=
   s" -0.0 NMG-FLTZ" EV-N    s" -0.0 f0<" EV-N T= ;

\ The interpreted twins of the five branch bodies, spelled exactly as the
\ migrated sources above are. They are what the fused lowering has to agree with.
: NMI-BLT ( r r -- n ) {: x:r y:r :} x y f< if 1 else 2 then ;
: NMI-BGT ( r r -- n ) {: x:r y:r :} x y f> if 1 else 2 then ;
: NMI-BEQ ( r r -- n ) {: x:r y:r :} x y f= if 1 else 2 then ;
: NMI-BLTZ ( r -- n ) {: x:r :} x f0< if 1 else 2 then ;
: NMI-BEQZ ( r -- n ) {: x:r :} x f0= if 1 else 2 then ;

: NMI-NAN ( -- r ) 0.0 0.0 f/ ;

\ Which arm the branch bodies above take for a given flag. It is here so that the
\ two lowerings of one source word can be held against EACH OTHER as well as
\ against the engine: the branch bodies answer 1 where the flag is set and 2
\ where it is clear, which is what `if ... else ... then` over a Habu flag means.
: FLAG-ARM ( n -- n )
   0<> if 1 else 2 then ;

: FCMP-FUSED-CASE ( -- )
   s" the fused branch takes the arm the interpreted body takes" T-LABEL
   s" 1.5 2.25 NMG-BLT" EV-N    1.5 2.25 NMI-BLT T=
   s" 2.25 1.5 NMG-BLT" EV-N    2.25 1.5 NMI-BLT T=
   s" 2.25 1.5 NMG-BGT" EV-N    2.25 1.5 NMI-BGT T=
   s" 1.5 2.25 NMG-BGT" EV-N    1.5 2.25 NMI-BGT T=
   s" 1.5 1.5 NMG-BEQ" EV-N     1.5 1.5 NMI-BEQ T=
   s" 1.5 2.25 NMG-BEQ" EV-N    1.5 2.25 NMI-BEQ T=
   s" -1.5 NMG-BLTZ" EV-N       -1.5 NMI-BLTZ T=
   s" 1.5 NMG-BLTZ" EV-N        1.5 NMI-BLTZ T=
   s" 0.0 NMG-BEQZ" EV-N        0.0 NMI-BEQZ T=
   s" 1.5 NMG-BEQZ" EV-N        1.5 NMI-BEQZ T=

   s" on all three orderings, which is what separates one condition from its neighbours" T-LABEL
   s" 1.5 1.5 NMG-BLT" EV-N    2 T=
   s" 1.5 1.5 NMG-BGT" EV-N    2 T=
   s" 2.25 1.5 NMG-BEQ" EV-N   2 T=
   s" 1.5 1.5 NMG-BLT" EV-N    1.5 1.5 NMI-BLT T=
   s" 1.5 1.5 NMG-BGT" EV-N    1.5 1.5 NMI-BGT T=
   s" 2.25 1.5 NMG-BEQ" EV-N   2.25 1.5 NMI-BEQ T=
   s" 0.0 NMG-BLTZ" EV-N       2 T=
   s" -1.5 NMG-BEQZ" EV-N      2 T=
   s" 0.0 NMG-BLTZ" EV-N       0.0 NMI-BLTZ T=
   s" -1.5 NMG-BEQZ" EV-N      -1.5 NMI-BEQZ T=

   s" and a NaN in either position takes the arm the comparison did NOT choose" T-LABEL
   s" 0.0 0.0 f/ 1.5 NMG-BLT" EV-N   2 T=
   s" 1.5 0.0 0.0 f/ NMG-BLT" EV-N   2 T=
   s" 0.0 0.0 f/ 0.0 0.0 f/ NMG-BLT" EV-N  2 T=
   s" 0.0 0.0 f/ 1.5 NMG-BGT" EV-N   2 T=
   s" 1.5 0.0 0.0 f/ NMG-BGT" EV-N   2 T=
   s" 0.0 0.0 f/ 1.5 NMG-BEQ" EV-N   2 T=
   s" 1.5 0.0 0.0 f/ NMG-BEQ" EV-N   2 T=
   s" 0.0 0.0 f/ 0.0 0.0 f/ NMG-BEQ" EV-N  2 T=
   s" 0.0 0.0 f/ NMG-BLTZ" EV-N      2 T=
   s" 0.0 0.0 f/ NMG-BEQZ" EV-N      2 T=

   s" which is the arm the interpreted body takes for the same NaN" T-LABEL
   s" 0.0 0.0 f/ 1.5 NMG-BLT" EV-N   NMI-NAN 1.5 NMI-BLT T=
   s" 1.5 0.0 0.0 f/ NMG-BLT" EV-N   1.5 NMI-NAN NMI-BLT T=
   s" 0.0 0.0 f/ 1.5 NMG-BGT" EV-N   NMI-NAN 1.5 NMI-BGT T=
   s" 1.5 0.0 0.0 f/ NMG-BGT" EV-N   1.5 NMI-NAN NMI-BGT T=
   s" 0.0 0.0 f/ 0.0 0.0 f/ NMG-BEQ" EV-N  NMI-NAN NMI-NAN NMI-BEQ T=
   s" 0.0 0.0 f/ NMG-BLTZ" EV-N      NMI-NAN NMI-BLTZ T=
   s" 0.0 0.0 f/ NMG-BEQZ" EV-N      NMI-NAN NMI-BEQZ T=

   s" the fused branch and the materialised flag agree on every input" T-LABEL
   s" 1.5 2.25 NMG-BLT" EV-N   s" 1.5 2.25 NMG-FLT" EV-N FLAG-ARM T=
   s" 0.0 0.0 f/ 1.5 NMG-BLT" EV-N
      s" 0.0 0.0 f/ 1.5 NMG-FLT" EV-N FLAG-ARM T=
   s" -0.0 NMG-BLTZ" EV-N      s" -0.0 NMG-FLTZ" EV-N FLAG-ARM T=
   s" -0.0 NMG-BEQZ" EV-N      s" -0.0 NMG-FEQZ" EV-N FLAG-ARM T=

   s" and the two zeros take the arms their numbers choose" T-LABEL
   s" -0.0 NMG-BLTZ" EV-N      -0.0 NMI-BLTZ T=
   s" -0.0 NMG-BEQZ" EV-N      -0.0 NMI-BEQZ T=
   s" -0.0 0.0 NMG-BEQ" EV-N   -0.0 0.0 NMI-BEQ T=
   s" -0.0 0.0 NMG-BLT" EV-N   -0.0 0.0 NMI-BLT T= ;


\ ---- what a converted selection leaves in the published record ---------------
\ THE ONE PLACE THE CLAIM CAN BE MADE ABOUT REAL BYTES. Every other statement
\ about the if-conversion in src/compiler/native/select.f is about a module: the
\ selected operations, the block count, the operands. This is the record the
\ engine will actually enter, walked instruction by instruction through
\ src/compiler/native/codewalk.f - the same walk the redirection seam and the
\ workload scan use - and counted for branches through the form tests below and
\ src/compiler/native/branch.f's own reader for the one that is a call. Nothing
\ here re-derives where a record starts or how long it is; both come off the
\ dictionary, which is where a caller finds them.
\
\ TWO BODIES, ONE SOURCE-LEVEL DIFFERENCE. Both are the range fold that
\ docs/codegen-placement.md measured - two tests, each leaving the word from the
\ middle - and they differ in one thing: the second divides in an arm. A
\ division may raise, so its arm cannot be run on a path the program would not
\ have taken, the region is refused, and the branches stay. That is the
\ admission rule read from the outside: the same shape converts or does not
\ according to one operation in one arm, and the record says which.
$FC000000 constant B-MASK            \ the unconditional branch
$14000000 constant B-FORM
$FF000010 constant BCOND-MASK        \ the conditional branch
$54000000 constant BCOND-FORM
$FF000000 constant CBZ-MASK          \ the two compare-with-zero branches
$B4000000 constant CBZ-FORM
$B5000000 constant CBNZ-FORM

variable BRANCH-N

\ Is this instruction word one that can move control? The branch-with-link is
\ asked of src/compiler/native/branch.f, which owns that form for the three
\ seams that read call sites; the other four are named here by their own
\ encodings because nothing else in the tree has to know them.
: BRANCH-INSN? ( n -- bool )
   {: w:n :}
   w NBR:BL? if true exit then
   w B-MASK and B-FORM = if true exit then
   w BCOND-MASK and BCOND-FORM = if true exit then
   w CBZ-MASK and CBZ-FORM = if true exit then
   w CBZ-MASK and CBNZ-FORM = ;

\ The walk's callback. It counts into a cell rather than onto the stack because
\ a quotation cannot read the enclosing word's locals, which is the same reason
\ every other caller of this walk parks its answer.
: BRANCH-NOTE ( n n -- )
   {: at:n w:n :}
   w BRANCH-INSN? if BRANCH-N @ 1+ BRANCH-N ! then ;

: BRANCHES-IN ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   0 BRANCH-N !
   a u REC-START  a u REC-LEN  [: BRANCH-NOTE ;] NWALK:SPAN-EACH
   BRANCH-N @ ;

: FOLD-MIGRATION ( -- )
   s" : NMG-FOLD ( n -- n ) {: c:n :} c 65 < if c exit then c 90 > if c exit then c 32 or ;"
      1 1 REGS NMIGRATE:DEFINE ;

: TRAPPING-MIGRATION ( -- )
   s" : NMG-FOLDIV ( n -- n ) {: c:n :} c 65 < if 100 c / exit then c 90 > if c exit then c 32 or ;"
      1 1 REGS NMIGRATE:DEFINE ;

\ The interpreted twins, compiled by the engine from the same text, so the
\ answers are held against the emitter this chain is replacing and not against
\ numbers written here.
: NMI-FOLD ( n -- n ) {: c:n :}
   c 65 < if c exit then c 90 > if c exit then c 32 or ;

: NMI-FOLDIV ( n -- n ) {: c:n :}
   c 65 < if 100 c / exit then c 90 > if c exit then c 32 or ;

: BRANCHLESS-CASE ( -- )
   FOLD-MIGRATION
   TRAPPING-MIGRATION

   s" the converted selection leaves no branch instruction in the record" T-LABEL
   s" NMG-FOLD" BRANCHES-IN 0 T=

   s" a selection whose arm may trap keeps its branches" T-LABEL
   s" NMG-FOLDIV" BRANCHES-IN 0 T<>

   s" and both answer what the engine's compilation of the same source answers"
   T-LABEL
   s" 64 NMG-FOLD" EV-N   64 NMI-FOLD T=
   s" 65 NMG-FOLD" EV-N   65 NMI-FOLD T=
   s" 77 NMG-FOLD" EV-N   77 NMI-FOLD T=
   s" 90 NMG-FOLD" EV-N   90 NMI-FOLD T=
   s" 91 NMG-FOLD" EV-N   91 NMI-FOLD T=
   s" 64 NMG-FOLDIV" EV-N 64 NMI-FOLDIV T=
   s" 50 NMG-FOLDIV" EV-N 50 NMI-FOLDIV T=
   s" 91 NMG-FOLDIV" EV-N 91 NMI-FOLDIV T= ;

\ ---- the order the blocks are written in -------------------------------------
\ WHAT THESE CASES ARE ABOUT. src/compiler/native/emit.f chooses which block's
\ instructions follow which, instead of writing them in the order the elaborator
\ happened to build them. The choice is worth nothing except through one rule -
\ a terminator's trailing unconditional branch is not emitted when its target is
\ the block written next - so what it BUYS is branches that are not there, and
\ that is what these cases count, in the published record, with the walk the
\ redirection seam and the workload scan use.
\
\ THE SHAPE THE CHOICE WAS MADE FOR. A `begin … while … repeat` loop is built
\ header, exit stub, body: the stub sits between the header and the loop body, so
\ the header's branch to the body could not fall through and neither could the
\ stub's branch to the block after the loop. Two unconditional branches, neither
\ of which the program requires. Written body-first the header falls into the
\ body and the stub falls into the block after the loop, and what is left is the
\ ONE branch a loop cannot do without: the back edge. NMG-WCALL below held three
\ of them before the order was chosen and holds one now.
\
\ WHY ITS TEST IS A CALL AND NOT A COMPARISON. A comparison standing above its
\ branch FUSES into it, and the fused form names the condition-holds arm first -
\ which for a `while` is staying in the loop - so its trailing half names the
\ exit stub and the fall-through goes there instead of into the body. That loop
\ keeps two branches, and taking the second needs the condition inverted rather
\ than the blocks moved (dot habu-choose-which-arm-ffe23e64). NMG-WCALL's test is
\ a call, so nothing fuses and the branch is the two-way one this case is about.
\
\ AND THE SHAPE IT MUST NOT DISTURB. A `begin … until` loop is built in an order
\ that already falls through everywhere it can, so a chooser that shuffled blocks
\ for the sake of shuffling would move its bytes for nothing. NMG-UNTIL pins that
\ its order is the one the module recorded, block for block.
\
\ AND THE END OF THE ROUTINE, WHICH IS NOT THE CHOOSER'S TO MOVE.
\ src/compiler/native/publish.f records a word's length as the emission LESS ONE
\ INSTRUCTION, because the engine's records exclude a word's trailing return -
\ that is the span its inliner copies. So the emission has to END in the return,
\ and a chooser left to itself would happily end NMG-WGT on its back edge and
\ publish a record with the last branch of the body cut off. NMG-WGT pins the
\ two halves of that: the last instruction emitted is a return, and the recorded
\ length is the emission less exactly that instruction.
$02000000 constant B-BACK            \ the sign bit of a branch's displacement field
$D65F03C0 constant RET-FORM          \ `ret`, the one instruction control leaves through

variable UNCOND-N
variable BACK-N

\ The walk's callback again, counting only the branch that carries no condition -
\ the one the fall-through rule can delete - and, of those, the ones that go
\ backwards. A loop's back edge is the only backward branch a structured body
\ builds, so "one unconditional branch and it goes backwards" is the statement
\ that every forward one was deleted.
: UNCOND-NOTE ( n n -- )
   {: at:n w:n :}
   w B-MASK and B-FORM = 0= if exit then
   UNCOND-N @ 1+ UNCOND-N !
   w B-BACK and 0<> if BACK-N @ 1+ BACK-N ! then ;

: UNCOND-COUNT ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   0 UNCOND-N !
   0 BACK-N !
   a u REC-START  a u REC-LEN  [: UNCOND-NOTE ;] NWALK:SPAN-EACH ;

: UNCOND-IN ( ptr u8 n -- n )
   UNCOND-COUNT UNCOND-N @ ;

: BACKWARD-IN ( ptr u8 n -- n )
   UNCOND-COUNT BACK-N @ ;

\ How many blocks the chosen order left where the module recorded them. Equal to
\ the block count means the order IS the module's order and nothing was moved.
: SELF-PLACED ( -- n )
   0
   A64EMIT:BLOCKS 0 ?do
      i A64EMIT:BLOCK-AT-POS@ i = if 1+ then
   loop ;

\ The loop whose exit test is a real call, which is the shape the measurement was
\ taken on. Its test cannot fuse into the branch - the comparison is inside the
\ callee - so the loop header ends in the two-way branch whose trailing half
\ names the body, and that is the branch the order deletes. The callee's body
\ names a local, which is not a token a recorded body may hold, so it is a call
\ the elaboration cannot copy into the caller and the loop really does call it.
: STEP-SRC ( -- ptr u8 n )
   s" : NMG-STEP ( n -- n bool ) {: k:n :} k 1- dup 0 > ;" ;

: WCALL-SRC ( -- ptr u8 n )
   s" : NMG-WCALL ( n -- n ) begin NMG-STEP while repeat ;" ;

: UNTIL-SRC ( -- ptr u8 n )
   s" : NMG-UNTIL ( n -- n ) begin 1- dup 0 <= until ;" ;

: WGT-SRC ( -- ptr u8 n )
   s" : NMG-WGT ( n -- n ) begin dup 0 > while 1- repeat ;" ;

: MIGRATE-STEP ( -- )
   STEP-SRC 1 2 LOOP-REGS NMIGRATE:DEFINE ;

: MIGRATE-WCALL ( -- )
   s" NMG-STEP"  s" NMG-STEP" GLOBAL-WID NPUB:NEW-START  1 2 NMIGRATE:CALLEE
   WCALL-SRC 1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-UNTIL ( -- )
   UNTIL-SRC 1 1 LOOP-REGS NMIGRATE:DEFINE ;

: MIGRATE-WGT ( -- )
   WGT-SRC 1 1 LOOP-REGS NMIGRATE:DEFINE ;

\ The interpreted twins, compiled by the engine from the same text, so the
\ answers below are held against the emitter this chain replaces.
: NMI-STEP ( n -- n bool ) {: k:n :}
   k 1- dup 0 > ;

: NMI-WCALL ( n -- n )
   begin NMI-STEP while repeat ;

: NMI-UNTIL ( n -- n )
   begin 1- dup 0 <= until ;

: NMI-WGT ( n -- n )
   begin dup 0 > while 1- repeat ;

: ORDER-CASE ( -- )
   MIGRATE-STEP
   MIGRATE-WCALL

   \ THE ORDER USED TO BE THE WHOLE ANSWER HERE AND IS NOW HALF OF IT. This shape
   \ builds five blocks, two of which do nothing before their branch: the exit
   \ stub and the join. Choosing the order deleted the branches INTO them; the
   \ collapse deletes the blocks themselves, because every branch that named one
   \ is sent to the far end instead and nothing reaches it any more. So the
   \ routine is written in three positions and the ordinals that survive are the
   \ header, the body and the block control leaves through.
   s" the loop's do-nothing stubs are branched past and never written" T-LABEL
   A64EMIT:BLOCKS 3 T=
   A64EMIT:DROPPED 2 T=
   SELF-PLACED 2 T=
   2 A64EMIT:BLOCK-AT-POS@ 4 T=

   \ And a position past the last one laid is a refusal, not whichever block sat
   \ there in an earlier emission - the table is not cleared beyond what this
   \ routine used, so the bound is what makes a stale read impossible.
   [: 3 A64EMIT:BLOCK-AT-POS@ drop ;] E-A64EMIT-BLOCK TTHROWSQ

   s" so its record holds one unconditional branch, and that one is the back edge"
   T-LABEL
   s" NMG-WCALL" UNCOND-IN 1 T=
   s" NMG-WCALL" BACKWARD-IN 1 T=

   s" it is still a loop with a call and a two-way exit, not a straight line" T-LABEL
   s" NMG-WCALL" BRANCHES-IN 3 T=

   MIGRATE-UNTIL

   \ NINE AND NOT TEN FOR THE DECREMENT'S SAKE. `begin 1- dup 0 <= until` holds
   \ two constants, and exactly one of them folds: the 1 is subtracted from a
   \ register, so it becomes the subtraction's own immediate and its move-wide is
   \ never written, while the 0 is read by the COMPARISON, which has no immediate
   \ form in this dialect and still needs the number in a register. One
   \ instruction goes and the block count does not move, which is the point of
   \ pinning both here.
   s" a loop whose build order was already the best is written out unmoved" T-LABEL
   A64EMIT:BLOCKS 4 T=
   SELF-PLACED  A64EMIT:BLOCKS T=
   A64EMIT:INSNS 9 T=

   s" and it too keeps only its back edge" T-LABEL
   s" NMG-UNTIL" UNCOND-IN 1 T=
   s" NMG-UNTIL" BACKWARD-IN 1 T=

   MIGRATE-WGT

   s" the block control leaves through is written last, whatever the trace wanted"
   T-LABEL
   A64EMIT:BLOCKS 5 T=
   SELF-PLACED  A64EMIT:BLOCKS T=
   A64EMIT:INSNS 1- A64EMIT:WORD@ RET-FORM T=

   s" so the recorded length is the emission less exactly that return" T-LABEL
   s" NMG-WGT" REC-LEN  A64EMIT:SIZE INSN-BYTES - T=

   s" and the two-armed body's join follows the arm that reaches it" T-LABEL
   s" NMG-FOLDIV" UNCOND-IN 1 T=
   s" NMG-FOLDIV" BACKWARD-IN 0 T=

   s" every one of them answers what the engine's compilation of the same source answers"
   T-LABEL
   s" 5 NMG-WCALL" EV-N   5 NMI-WCALL T=
   s" 1 NMG-WCALL" EV-N   1 NMI-WCALL T=
   s" 0 NMG-WCALL" EV-N   0 NMI-WCALL T=
   s" -3 NMG-WCALL" EV-N  -3 NMI-WCALL T=
   s" 5 NMG-UNTIL" EV-N   5 NMI-UNTIL T=
   s" 1 NMG-UNTIL" EV-N   1 NMI-UNTIL T=
   s" 0 NMG-UNTIL" EV-N   0 NMI-UNTIL T=
   s" 5 NMG-WGT" EV-N     5 NMI-WGT T=
   s" 0 NMG-WGT" EV-N     0 NMI-WGT T=
   s" -3 NMG-WGT" EV-N    -3 NMI-WGT T= ;

\ maki/autograd.f:48 verbatim, which is the third corpus's MAX-F: the shape
\ where the fused branch's arms carry the compared values themselves. What it
\ adds to the cases above is that the value the branch ANSWERS is one of the two
\ operands, so an arm taken the wrong way is visible as the wrong number rather
\ than only as the wrong flag - and on a NaN it answers `x` in both operand
\ positions, which is the whole content of the NaN rule for this body.
: MAXF-MIGRATION ( -- )
   s" : NMG-MAXF ( r r -- r ) {: x:r y:r :} x y f< if y else x then ;"
      2 1 REGS NMIGRATE:DEFINE ;

\ The interpreted twin, compiled by the ENGINE from the same source the migration
\ is handed. It is defined through `evaluate` rather than written here as an
\ ordinary definition because it answers a double, and the comparison below reads
\ the whole cell - which is what `evaluate` hands back.
: MAXF-INTERPRETED ( -- )
   s" : NMI-MAXF ( r r -- r ) {: x:r y:r :} x y f< if y else x then ;" EV ;

: MAXF-CASE ( -- )
   MAXF-MIGRATION
   MAXF-INTERPRETED

   s" the branch row answers what the same body answers, on both orders" T-LABEL
   s" 1.5 -2.5 NMG-MAXF" EV-N   s" 1.5 -2.5 NMI-MAXF" EV-N T=
   s" -2.5 1.5 NMG-MAXF" EV-N   s" -2.5 1.5 NMI-MAXF" EV-N T=
   s" 1.5 -2.5 NMG-MAXF" EV-N   s" -2.5 1.5 NMG-MAXF" EV-N T=

   s" on a NaN in either position it answers the first argument, as the body does" T-LABEL
   s" 0.0 0.0 f/ 1.5 NMG-MAXF" EV-N   s" 0.0 0.0 f/ 1.5 NMI-MAXF" EV-N T=
   s" 1.5 0.0 0.0 f/ NMG-MAXF" EV-N   s" 1.5 0.0 0.0 f/ NMI-MAXF" EV-N T=
   s" 0.0 0.0 f/ 1.5 NMG-MAXF" EV-N   s" 0.0 0.0 f/" EV-N T=
   s" 1.5 0.0 0.0 f/ NMG-MAXF" EV-N   s" 1.5" EV-N T=

   s" and the sign of a zero survives, which a compare of numbers alone cannot see" T-LABEL
   s" 0.0 -0.0 NMG-MAXF" EV-N   s" 0.0 -0.0 NMI-MAXF" EV-N T=
   s" -0.0 0.0 NMG-MAXF" EV-N   s" -0.0 0.0 NMI-MAXF" EV-N T=
   s" -0.0 0.0 NMG-MAXF" EV-N   s" -0.0" EV-N T=
   s" -0.0 0.0 NMG-MAXF" EV-N   s" 0.0" EV-N T<> ;

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

\ ---- a double where a straight line does not reach ---------------------------
\ A double is a value of a second register class, and three shapes of well typed
\ Habu put one somewhere a straight line does not: across a block edge, across a
\ call, and into a memory cell. The first two are compiled here and checked
\ against the engine's own answer for the same source; the third is still refused
\ by name, and dot habu-store-a-double-a31b313e carries it.
\
\ WHY THE FIRST TWO ARE ONE STATEMENT AND NOT TWO. Both are a double leaving one
\ place and arriving in another through a position whose type this pass has to
\ state before the value reaches it - a block argument at a join, a data-stack
\ slot at a call - and both are answered by the same crossing, which moves eight
\ bytes between the two register files and reads none of them. So the thing to
\ check is the same in both: the compiled word answers the SAME CELL the engine's
\ own compilation of that source answers. The cell is the whole comparison, so a
\ double that came back through the wrong file, or an accumulation the loop
\ carried in the wrong order, is a different number here rather than a near one.
: FLOAT-STORE ( -- )
   s" : NMG-BAD2 ( r ptr a -- ) {: v:r b:ptr :} v 1.0 f+ b ! ;" 2 0 REGS NMIGRATE:DEFINE ;

\ The loop-carried accumulator: the double enters the header from the block above
\ it and comes back to it from the latch, so the header's argument is a floating
\ register and every turn's Fadd feeds the next.
: FLOAT-EDGE ( -- )
   s" : NMG-FLOOP ( r n -- r ) 0 ?do 1.0 f+ loop ;" 2 1 LOOP-REGS NMIGRATE:DEFINE ;

\ The join of a two-armed branch whose arms hand over values of DIFFERENT
\ classes: `0.0` is a double and `x` is the cell the argument arrived in. It is
\ RELU's shape, and it is the case the join-type rule exists for - one arm states
\ the type and the other crosses to it.
: FLOAT-JOIN ( -- )
   s" : NMG-FRELU ( r -- r ) {: x :} x f0< if 0.0 else x then ;" 1 1 REGS NMIGRATE:DEFINE ;

\ The callee is migrated first, so both halves are the chain's code. The caller's
\ `dup` is what makes this the whole statement about a call: one of the two
\ doubles is the call's ARGUMENT and the other is live ACROSS it, and the machine
\ stage puts both into data-stack slots - the argument at the callee's base and
\ the survivor below it - so a body that got either slot wrong answers a
\ different number.
: FLOAT-CALLEE ( -- )
   s" : NMG-FD ( r -- r ) 2.0 f* ;" 1 1 REGS NMIGRATE:DEFINE ;

: FLOAT-CALL ( -- )
   s" NMG-FD"  s" NMG-FD" GLOBAL-WID NPUB:NEW-START  1 1 NMIGRATE:CALLEE
   s" : NMG-FCALL ( r -- r ) 1.0 f* dup NMG-FD f+ ;"
   1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

\ TWO DOUBLES CROSSING ONE JOIN IN SWAPPED ORDER. This is MAX2's shape in the
\ other register file, and it is the case the edge SPLIT exists for: one arm
\ hands the join (b, a) and the other hands it (a, b), so the two arguments and
\ the two values would collapse into one class holding both a and b at once
\ unless every value crossing the edge is copied into a value of its own first.
\ The copy is a64.fmovdd, and a body that reached the join without it would
\ answer `a-b` on one path and `b-a` on the other. The condition is `x f0<`,
\ which has nothing to do with the two values' order, so BOTH arms are reachable
\ with the same pair of operands and the two answers differ in the sign - which
\ is what makes a collapsed pair visible rather than absorbed.
: FLOAT-SWAP ( -- )
   s" : NMG-FSWAP ( r r -- r ) {: x:r y:r :} x 1.0 f* y 1.0 f* x f0< if swap then f- ;"
   2 1 REGS NMIGRATE:DEFINE ;

\ The engine's own compilation of the join bodies, spelled exactly as the migrated
\ ones are. It goes through `evaluate` for the reason MAXF-INTERPRETED does: it
\ answers a double, and the comparison below reads the whole cell, which is what
\ `evaluate` hands back.
: FLOAT-JOIN-INTERPRETED ( -- )
   s" : NMI-FRELU ( r -- r ) {: x :} x f0< if 0.0 else x then ;" EV
   s" : NMI-FSWAP ( r r -- r ) {: x:r y:r :} x 1.0 f* y 1.0 f* x f0< if swap then f- ;" EV ;

: FLOAT-PLACE-CASES ( -- )
   FLOAT-CALLEE
   FLOAT-EDGE
   FLOAT-JOIN
   FLOAT-SWAP
   FLOAT-CALL
   FLOAT-JOIN-INTERPRETED

   s" a double carried across a loop edge accumulates what the same body accumulates" T-LABEL
   s" 2.5 3 NMG-FLOOP" EV-N  s" 2.5 1.0 f+ 1.0 f+ 1.0 f+" EV-N T=
   s" 0.0 0 NMG-FLOOP" EV-N  s" 0.0" EV-N T=

   s" and it is a LEFT fold, so the order the turns add in is the recorded one" T-LABEL
   s" 9007199254740992.0 2 NMG-FLOOP" EV-N
   s" 9007199254740992.0 1.0 f+ 1.0 f+" EV-N T=
   s" 9007199254740992.0 2 NMG-FLOOP" EV-N
   s" 1.0 1.0 f+ 9007199254740992.0 f+" EV-N T<>

   s" a join whose arms hand over a double and a cell answers the same cell" T-LABEL
   s" 1.5 NMG-FRELU" EV-N  s" 1.5 NMI-FRELU" EV-N T=
   s" -1.5 NMG-FRELU" EV-N  s" -1.5 NMI-FRELU" EV-N T=

   s" including the two zeros, which are one number and two cells" T-LABEL
   s" -0.0 NMG-FRELU" EV-N  s" -0.0 NMI-FRELU" EV-N T=
   s" -0.0 NMG-FRELU" EV-N  s" -0.0" EV-N T=

   s" and a NaN, which compares false and therefore takes the ELSE arm" T-LABEL
   s" 0.0 0.0 f/ NMG-FRELU" EV-N  s" 0.0 0.0 f/ NMI-FRELU" EV-N T=
   s" 0.0 0.0 f/ NMG-FRELU" EV-N  s" 0.0 0.0 f/" EV-N T=

   s" two doubles crossing one join in swapped order keep their places" T-LABEL
   s" 1.5 2.25 NMG-FSWAP" EV-N  s" 1.5 2.25 NMI-FSWAP" EV-N T=
   s" -1.5 2.25 NMG-FSWAP" EV-N  s" -1.5 2.25 NMI-FSWAP" EV-N T=
   s" 2.25 1.5 NMG-FSWAP" EV-N  s" 2.25 1.5 NMI-FSWAP" EV-N T=
   s" -2.25 1.5 NMG-FSWAP" EV-N  s" -2.25 1.5 NMI-FSWAP" EV-N T=

   s" and the arm that swaps really answers something else, so a collapsed pair shows" T-LABEL
   s" 1.5 2.25 NMG-FSWAP" EV-N  s" -1.5 2.25 NMG-FSWAP" EV-N T<>
   s" 1.5 2.25 NMG-FSWAP" EV-N  s" 0.75 fnegate" EV-N T=
   s" -1.5 2.25 NMG-FSWAP" EV-N  s" 3.75" EV-N T=

   s" a double crossing a call - as its argument and live beside it - comes back the double it was" T-LABEL
   s" 1.5 NMG-FCALL" EV-N  s" 1.5 1.0 f* dup 2.0 f* f+" EV-N T=
   s" -0.5 NMG-FCALL" EV-N  s" -0.5 1.0 f* dup 2.0 f* f+" EV-N T=

   s" including a negative zero, which is a different CELL from zero" T-LABEL
   s" -0.0 NMG-FCALL" EV-N  s" -0.0 1.0 f* dup 2.0 f* f+" EV-N T=
   s" -0.0 NMG-FCALL" EV-N  s" -0.0" EV-N T=
   s" -0.0 NMG-FCALL" EV-N  s" 0.0" EV-N T<>

   s" and a NaN, whose payload survives the two slots unchanged" T-LABEL
   s" 0.0 0.0 f/ NMG-FCALL" EV-N  s" 0.0 0.0 f/ 1.0 f* dup 2.0 f* f+" EV-N T= ;

: FLOAT-REFUSAL-CASES ( -- )
   s" a double stored into a memory cell is refused - the crossing is not placed yet" T-LABEL
   [: FLOAT-STORE ;] E-NELAB-TYPE TTHROWSQ

   s" a double handed to an integer operation never reaches the chain at all" T-LABEL
   s" NMG-BAD1 ( r -- n ) 1.0 f+ 1 +" CHECK-QUIET-CANDIDATE! 0 T=
   s" NMG-OKAY ( r -- r ) 1.0 f+" CHECK-QUIET-CANDIDATE! -1 T=

   s" and a body the chain refused keeps the record the engine compiled for it" T-LABEL
   s" NMG-BAD2" DEFINED? TTRUE ;

\ ---- compiling without publishing --------------------------------------------
\ Every case above migrates a word the ENGINE published: the chain is a second
\ pass, and the record it rewrites already had the old emitter's code in it.
\ These cases are the other thing - the definition is certified and WITHHELD, so
\ nothing is published under its name until the chain's own publisher commits a
\ record. That is what makes the old emitter's emission unnecessary rather than
\ prerequisite, which is the whole point of the mode.
variable HELD-ND0
variable HELD-ND1

: HELD-MIGRATE ( -- )
   ndict@ HELD-ND0 !
   s" : NMG-HELD ( n -- n ) 3 + ;" 1 1 REGS NMIGRATE:DEFINE-HELD
   ndict@ HELD-ND1 ! ;

: HELD-CASE ( -- )
   s" a held definition compiles end to end and answers" T-LABEL
   HELD-MIGRATE
   s" 5 NMG-HELD" EV-N 8 T=
   s" 0 NMG-HELD" EV-N 3 T=
   s" -3 NMG-HELD" EV-N 0 T=

   s" exactly one record appeared, and the chain's publisher is what added it" T-LABEL
   HELD-ND1 @ HELD-ND0 @ 1 + T=

   s" the record points at the chain's emission" T-LABEL
   s" NMG-HELD" REC-START  s" NMG-HELD" GLOBAL-WID NPUB:NEW-START T=

   s" and the old emitter produced NOTHING for that name, which is the point" T-LABEL
   s" NMG-HELD" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NMG-HELD" GLOBAL-WID NPUB:OLD-START 0 T=
   s" NMG-HELD" GLOBAL-WID NPUB:OLD-LEN 0 T=

   s" a definition compiled afterwards calls it as an ordinary word" T-LABEL
   s" : NMG-HELD-CALLER ( n -- n ) NMG-HELD NMG-HELD ;" EV
   s" 1 NMG-HELD-CALLER" EV-N 7 T= ;

\ A held migration the CHAIN refuses. The engine has published nothing by then,
\ so unlike every refusal case above there is no word left behind to keep
\ running - the correct outcome is that the name does not exist at all.
: HELD-REFUSED-MIGRATE ( -- )
   s" using NMG-AWAY : NMG-HELD-BAD ( n -- n ) NMG-K + ; ;using" 1 1 REGS NMIGRATE:DEFINE-HELD ;

: HELD-REFUSAL-CASE ( -- )
   s" a held body outside the dialect is refused with the dialect's own code" T-LABEL
   ndict@ HELD-ND0 !
   [: HELD-REFUSED-MIGRATE ;] E-HIR-UNMODELED TTHROWSQ

   s" and it leaves NOTHING published - no record, no name, no log row" T-LABEL
   ndict@ HELD-ND0 @ T=
   s" NMG-HELD-BAD" DEFINED? TFALSE
   s" NMG-HELD-BAD" GLOBAL-WID NPUB:REPUBLISHED? TFALSE

   s" the recorder recovered, so the NEXT held migration still works" T-LABEL
   s" : NMG-HELD-AFTER ( n -- n ) 7 + ;" 1 1 REGS NMIGRATE:DEFINE-HELD
   s" 1 NMG-HELD-AFTER" EV-N 8 T=

   s" and the refused name is free again: the checker's signature went with it" T-LABEL
   s" : NMG-HELD-BAD ( n -- n ) 1 + ;" EV
   s" 4 NMG-HELD-BAD" EV-N 5 T= ;

\ ---- which token the chain refused, read through the real entry --------------
\ src/compiler/native/elaborate.f writes down the body token its refusal was
\ about, and test/compiler/native-elaborate.f proves what it writes. What only
\ this suite can show is the part that is about DRIVING the chain: the record
\ says nothing about attempts that never reach the elaborator, and a driver
\ compiling one definition after another has to clear it itself.
\
\ THE HAZARD IS THE POINT OF THE CASE, so it is asserted and not only guarded
\ against. An engine-level refusal - an undefined word in the body - is raised
\ while `evaluate` is still resolving names, before any tape is sealed, so
\ NELAB:COLON is never entered and the clear that is COLON's first act never
\ runs. A driver that read the record then would read the PREVIOUS definition's
\ word as this one's. That is asserted here exactly as it happens, so the case
\ fails if the reset is ever quietly made unnecessary, and then the cure is
\ asserted beside it.
\
\ Reasoning the hazard away by which codes can come out of where is not the same
\ guarantee: it would rest on E-HIR-UNMODELED never being thrown outside an
\ admit, and src/compiler/native/hir-word.f throws it from readers an admit is
\ not the only caller of. The clear does not rest on anything.
70 constant REJECT-RC                \ src/core/checker.f PKGCTX-REJECT-RC (private there)

: HELD-MOD-MIGRATE ( -- )
   s" using NMG-AWAY : NMG-HELD-MOD ( n -- n ) NMG-K + ; ;using" 1 1 REGS NMIGRATE:DEFINE-HELD ;

: HELD-UNDEF-MIGRATE ( -- )
   s" : NMG-HELD-UA ( n -- n ) NMG-NO-SUCH-WORD-A and ;" 1 1 REGS NMIGRATE:DEFINE-HELD ;

: HELD-UNDEF2-MIGRATE ( -- )
   s" : NMG-HELD-UB ( n -- n ) NMG-NO-SUCH-WORD-B and ;" 1 1 REGS NMIGRATE:DEFINE-HELD ;

: HELD-RECORD-CASE ( -- )
   s" a held body outside the dialect names the offending word through the chain's own record" T-LABEL
   NELAB:REFUSED-RESET
   [: HELD-MOD-MIGRATE ;] E-HIR-UNMODELED TTHROWSQ
   NELAB:REFUSED$ s" NMG-K" T$=

   s" a refusal the engine raises before elaboration leaves that record standing" T-LABEL
   [: HELD-UNDEF-MIGRATE ;] REJECT-RC TTHROWSQ
   NELAB:REFUSED$ s" NMG-K" T$=

   s" so a driver clears the record before each attempt, and then reads no word at all" T-LABEL
   NELAB:REFUSED-RESET
   [: HELD-UNDEF2-MIGRATE ;] REJECT-RC TTHROWSQ
   NELAB:REFUSED-ROW -1 T=
   NELAB:REFUSED$ nip 0 T=

   s" and the chain recovered: the next held migration compiles and leaves no record" T-LABEL
   s" : NMG-HELD-AGAIN ( n -- n ) 9 + ;" 1 1 REGS NMIGRATE:DEFINE-HELD
   s" 1 NMG-HELD-AGAIN" EV-N 10 T=
   NELAB:REFUSED-ROW -1 T= ;

\ ---- asking the chain without spending anything on the answer ----------------
\ THE THIRD MODE, AND THE ONE A MEASUREMENT NEEDS. A held migration commits its
\ emission, and committing is permanent: it takes a code slot, a row of the
\ clobber record and a row of the replacement log, and neither record may drop a
\ row to make space, because a row is the whole of what a caller compiled against
\ it. So a caller that wants to know whether the chain can compile a definition -
\ rather than to have it compiled - could only find out by paying for it, and
\ after enough questions the answer it got back was that a table was full. That is
\ what tools/chain-census-core.f measured: it reported the size of the first table
\ it filled as the size of the compilable tree.
\
\ MEASURE-HELD RUNS EVERY STAGE AND KEEPS NONE OF IT. What the cases below assert
\ is that "keeps none of it" is each of the four things a publication would have
\ kept, one assertion apiece, plus the fifth thing the checker holds: the
\ certified signature under the definition's name, which is retracted the way a
\ refused held run retracts it.
\
\ THE SAME NAME IS MEASURED OVER AND OVER ON PURPOSE. A second certified
\ definition of one name is refused, so a measurement that failed to retract the
\ first one would make the second attempt fail - and the loop below would report
\ that failure rather than the counts. Reusing the name is how the retraction is
\ tested on the path where nothing went wrong, which is the path a census spends
\ all its time on.
16 constant MEASURE-N
variable MEAS-ND0
variable MEAS-CP0
variable MEAS-ROWS0
variable MEAS-PUB0

: MEASURE-SUBJECT ( -- )
   s" : NMG-MEASURED ( n -- n ) 3 + 4 * ;" 1 1 REGS NMIGRATE:MEASURE-HELD ;

: MEASURE-BEFORE ( -- )
   ndict@ MEAS-ND0 !
   cp@ MEAS-CP0 !
   NCLOB:ROWS MEAS-ROWS0 !
   NPUB:REPUBLISHED MEAS-PUB0 ! ;

: MEASURE-MANY ( -- )
   MEASURE-N 0 ?do MEASURE-SUBJECT loop ;

: MEASURE-CASE ( -- )
   MEASURE-BEFORE
   MEASURE-MANY

   s" a measured definition publishes no record and no name" T-LABEL
   ndict@ MEAS-ND0 @ T=
   s" NMG-MEASURED" DEFINED? TFALSE
   s" NMG-MEASURED" GLOBAL-WID NPUB:REPUBLISHED? TFALSE

   s" it writes no code and leaves the free slot where it found it" T-LABEL
   cp@ MEAS-CP0 @ T=

   s" and it spends no row of either record a publication keeps" T-LABEL
   NCLOB:ROWS MEAS-ROWS0 @ T=
   NPUB:REPUBLISHED MEAS-PUB0 @ T=

   s" the name is free afterwards: the certified signature went with it" T-LABEL
   s" : NMG-MEASURED ( n -- n ) 3 + 4 * ;" EV
   s" 5 NMG-MEASURED" EV-N 32 T= ;

\ What measuring answers has to be what publishing answers, or the count is a
\ measurement of a second chain. The same source text is measured and then HELD -
\ under the same name, which the measurement had to leave free - and the word
\ that appears computes what the body says.
: MEASURE-AGREE-CASE ( -- )
   s" a body the measurement accepts is one the held publication accepts" T-LABEL
   s" : NMG-MEASURE-TWIN ( n -- n ) 5 * 2 + ;" 1 1 REGS NMIGRATE:MEASURE-HELD
   s" NMG-MEASURE-TWIN" DEFINED? TFALSE
   s" : NMG-MEASURE-TWIN ( n -- n ) 5 * 2 + ;" 1 1 REGS NMIGRATE:DEFINE-HELD
   s" 3 NMG-MEASURE-TWIN" EV-N 17 T=

   s" and the record it published is the chain's own emission" T-LABEL
   s" NMG-MEASURE-TWIN" GLOBAL-WID NPUB:REPUBLISHED? TTRUE ;

: MEASURE-REFUSED-MIGRATE ( -- )
   s" using NMG-AWAY : NMG-MEASURE-BAD ( n -- n ) NMG-K + ; ;using" 1 1 REGS NMIGRATE:MEASURE-HELD ;

: MEASURE-REFUSAL-CASE ( -- )
   s" a measured body outside the dialect is refused with the dialect's own code" T-LABEL
   MEASURE-BEFORE
   [: MEASURE-REFUSED-MIGRATE ;] E-HIR-UNMODELED TTHROWSQ

   s" and a refused measurement leaves exactly what an accepted one leaves" T-LABEL
   ndict@ MEAS-ND0 @ T=
   cp@ MEAS-CP0 @ T=
   NCLOB:ROWS MEAS-ROWS0 @ T=
   NPUB:REPUBLISHED MEAS-PUB0 @ T=
   s" NMG-MEASURE-BAD" DEFINED? TFALSE

   s" the chain recovered, so the next measurement still answers" T-LABEL
   s" : NMG-MEASURE-AFTER ( n -- n ) 6 + ;" 1 1 REGS NMIGRATE:MEASURE-HELD
   s" NMG-MEASURE-AFTER" DEFINED? TFALSE ;

\ ---- a name resolved off the engine, with nothing staged ---------------------
\ THE CASE THE WHOLE TRANCHE IS ABOUT. Nothing below stages a callee, a data word
\ or an address. Each body simply NAMES a word, and the chain resolves the name:
\ src/compiler/native/dict.f answers where its code starts, in the order the
\ engine resolves that same body, and the CHECKER answers how many cells a call
\ to it moves. If either answer were wrong the migrated routine would branch
\ somewhere else or move the wrong number of cells, and the answers below would
\ not be the answers the engine's own compilation gives.
\
\ THE ARITHMETIC IS CHOSEN SO EVERY CELL COUNT MATTERS. RES-ADD3 takes two and
\ leaves one, so a call site that moved one cell too few or too many would hand
\ the callee the wrong operand or take back a value nobody produced; the answers
\ are asymmetric in their arguments, so a swapped or shifted operand shows up as
\ a different number rather than the same one.
: RESOLVED-SETUP ( -- )
   s" : RES-ADD3 ( n n -- n ) + 3 + ;" EV
   s" variable RES-VAR" EV
   s" 41 RES-VAR !" EV ;

: MIGRATE-RESOLVED-CALL ( -- )
   s" : RES-CALLER ( n n -- n ) RES-ADD3 2 * ;" 2 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-RESOLVED-DATA ( -- )
   s" : RES-READ ( -- n ) RES-VAR @ 1 + ;" 0 1 REGS NMIGRATE:DEFINE ;

: RESOLVED-CASE ( -- )
   RESOLVED-SETUP

   s" a body that NAMES a tree word migrates with nothing staged" T-LABEL
   MIGRATE-RESOLVED-CALL
   s" RES-CALLER" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and it computes what the callee really computes, not what a caller said" T-LABEL
   s" 10 20 RES-CALLER" EV-N 66 T=
   s" 1 2 RES-CALLER" EV-N 12 T=

   s" a body that NAMES a data word migrates with nothing staged" T-LABEL
   MIGRATE-RESOLVED-DATA
   s" RES-READ" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" RES-READ" EV-N 42 T=

   s" and the address it reads is the one that word really pushes" T-LABEL
   s" 7 RES-VAR !" EV
   s" RES-READ" EV-N 8 T= ;

\ ---- the resolution is the LIVE dictionary's, not a copy of it ---------------
\ THE STALENESS CLASS, WHICH IS WHY THE ANSWER IS TAKEN WHERE IT IS USED. A
\ caller that stated a callee's address stated a number it had looked up a moment
\ earlier, and nothing downstream could tell that number from a live one once the
\ callee was retired and redefined somewhere else. Here the callee is redefined
\ between two migrations of the SAME source text, and the second migration has to
\ compile against the second callee - not against the first, and not against a
\ remembered address.
: STALE-SETUP ( -- )
   s" : STL-CALLEE ( n -- n ) 10 * ;" EV ;

: STALE-REDEFINE ( -- )
   s" undefine STL-CALLEE" EV
   s" : STL-CALLEE ( n -- n ) 100 * ;" EV ;

: MIGRATE-STALE-1 ( -- )
   s" : STL-A ( n -- n ) STL-CALLEE 1 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-STALE-2 ( -- )
   s" : STL-B ( n -- n ) STL-CALLEE 1 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: STALE-CASE ( -- )
   STALE-SETUP
   s" the first migration compiles against the callee that exists then" T-LABEL
   MIGRATE-STALE-1
   s" 5 STL-A" EV-N 51 T=

   s" and after the callee is redefined the NEXT migration follows the new one" T-LABEL
   STALE-REDEFINE
   MIGRATE-STALE-2
   s" 5 STL-B" EV-N 501 T= ;

\ ---- as many locals groups as the body writes -------------------------------
\ THE SHAPE THE TREE IS FULL OF: bind, compute, name the result, compute again.
\ Every body below is compiled by the production entry from its own text, so the
\ tape the chain elaborates is the one the checker filled, and every answer is
\ held against arithmetic that a wrongly-bound name cannot reproduce.
\
\ THE ARITHMETIC IS CHOSEN SO A MIS-BOUND NAME SHOWS. LGP-TWO subtracts before
\ it squares, so binding the second group to the wrong value - the entry
\ argument rather than the difference, or the arguments the other way round -
\ answers a different number for the same call. LGP-THREE chains three groups
\ and ends by subtracting the FIRST group's name from the third's, so it also
\ says that an earlier group's names are still readable after a later one binds.
: LGP-TWO-SRC ( -- ptr u8 n )
   s" : LGP-TWO ( n n -- n ) {: a:n b:n :} b a - {: d:n :} d d * ;" ;

: LGP-THREE-SRC ( -- ptr u8 n )
   s" : LGP-THREE ( n n -- n ) {: a:n b:n :} a b + {: s:n :} s s * {: q:n :} q a - ;" ;

\ The group that opens AFTER a call, which is what makes the prefix rule a
\ measurement rather than an argument. `a` is named on both sides of the call, so
\ it travels across it; `d` does not exist yet when that call is staged and must
\ not be handed over with it; and the loop after the second group carries both
\ names across its edges. A seam that read the whole name table instead of the
\ bound prefix would hand the call an operand for a name holding no value.
: LGP-CALL-SRC ( -- ptr u8 n )
   s" : LGP-CALL ( n n -- n ) {: a:n b:n :} b LGP-DBL {: d:n :} d begin dup a > while a - repeat ;" ;

\ A name that is a CALL before the group and the local afterwards. The engine
\ compiles this body and answers 22 for 5 (measured), because a token is
\ resolved where it stands - so a chain that read the first `LGP-DBL` as the
\ local would be compiling a program the checker never certified.
: LGP-SHADOW-SETUP ( -- )
   s" : LGP-DBL ( n -- n ) 2 * ;" EV ;

: LGP-SHADOW-SRC ( -- ptr u8 n )
   s" : LGP-SHADOW ( n -- n ) LGP-DBL 1 + {: LGP-DBL:n :} LGP-DBL LGP-DBL + ;" ;

\ A group that opens after a CLOSED early-exit guard, which docs/forth.md admits
\ as ordinary source: the fall-through path is live, so the group binds on it.
\ The `exit` branches to a block that takes the outputs and nothing else, and
\ the group that comes later must not change what that branch handed over - so
\ both paths are executed below, not just the one the group is on.
: LGP-EXIT-SRC ( -- ptr u8 n )
   s" : LGP-EXIT ( n -- n ) {: a:n :} a 0 < if 0 exit then a 3 * {: d:n :} d a + ;" ;

: MIGRATE-LGP-TWO ( -- )
   LGP-TWO-SRC 2 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-LGP-THREE ( -- )
   LGP-THREE-SRC 2 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-LGP-CALL ( -- )
   LGP-CALL-SRC 2 1 LOOP-REGS NMIGRATE:DEFINE ;

: MIGRATE-LGP-SHADOW ( -- )
   LGP-SHADOW-SRC 1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-LGP-EXIT ( -- )
   LGP-EXIT-SRC 1 1 REGS NMIGRATE:DEFINE ;

\ The engine's own compilations of the same text, under names of their own, so
\ the answers below are held against the emitter this chain replaces rather than
\ against numbers written out by hand.
: LGP-INTERPRETED ( -- )
   s" : LGI-TWO ( n n -- n ) {: a:n b:n :} b a - {: d:n :} d d * ;" EV
   s" : LGI-THREE ( n n -- n ) {: a:n b:n :} a b + {: s:n :} s s * {: q:n :} q a - ;" EV
   s" : LGI-CALL ( n n -- n ) {: a:n b:n :} b LGP-DBL {: d:n :} d begin dup a > while a - repeat ;" EV
   s" : LGI-SHADOW ( n -- n ) LGP-DBL 1 + {: LGP-DBL:n :} LGP-DBL LGP-DBL + ;" EV
   s" : LGI-EXIT ( n -- n ) {: a:n :} a 0 < if 0 exit then a 3 * {: d:n :} d a + ;" EV ;

: LOCALS-GROUPS-CASE ( -- )
   LGP-SHADOW-SETUP
   LGP-INTERPRETED

   s" a body with two locals groups is the chain's code" T-LABEL
   MIGRATE-LGP-TWO
   s" LGP-TWO" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the second group names what the first group's names computed" T-LABEL
   s" 3 10 LGP-TWO" EV-N 49 T=
   s" 3 10 LGP-TWO" EV-N  s" 3 10 LGI-TWO" EV-N T=
   s" 10 3 LGP-TWO" EV-N  s" 10 3 LGI-TWO" EV-N T=

   s" three groups chain, each naming the one before it" T-LABEL
   MIGRATE-LGP-THREE
   s" 2 5 LGP-THREE" EV-N 47 T=
   s" 2 5 LGP-THREE" EV-N  s" 2 5 LGI-THREE" EV-N T=
   s" 5 2 LGP-THREE" EV-N  s" 5 2 LGI-THREE" EV-N T=

   s" a group opened after a call carries only the names that exist there" T-LABEL
   MIGRATE-LGP-CALL
   s" LGP-CALL" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" 7 10 LGP-CALL" EV-N 6 T=
   s" 4 6 LGP-CALL" EV-N 4 T=
   s" 7 10 LGP-CALL" EV-N  s" 7 10 LGI-CALL" EV-N T=
   s" 4 6 LGP-CALL" EV-N  s" 4 6 LGI-CALL" EV-N T=

   s" a name is the call before its group and the local after it" T-LABEL
   MIGRATE-LGP-SHADOW
   s" 5 LGP-SHADOW" EV-N 22 T=
   s" 5 LGP-SHADOW" EV-N  s" 5 LGI-SHADOW" EV-N T=
   s" 8 LGP-SHADOW" EV-N  s" 8 LGI-SHADOW" EV-N T=

   s" a group after a closed early-exit guard binds on the path that reaches it" T-LABEL
   MIGRATE-LGP-EXIT
   s" 5 LGP-EXIT" EV-N 20 T=
   s" -2 LGP-EXIT" EV-N 0 T=
   s" 5 LGP-EXIT" EV-N  s" 5 LGI-EXIT" EV-N T=
   s" -2 LGP-EXIT" EV-N  s" -2 LGI-EXIT" EV-N T= ;

: RUN ( -- )
   T-RESET
   MOD-CONST
   RESOLVED-CASE
   STALE-CASE
   LOCALS-GROUPS-CASE
   MIGRATED-CASE
   VOID-CALL-CASE
   SPILL-CASE
   CALL-CASE
   DEEP-CASE
   INTEROP-CASE
   LOOP-CALL-CASE
   CALL-REFUSAL-CASES
   CONST-CALL-CASE
   REFUSED-CASE
   UNTOUCHED-CASE
   ENTRY-CASES
   DATA-CASES
   FLOAT-CASE
   FCMP-MIGRATIONS
   FCMP-FLAG-CASE
   FCMP-FUSED-CASE
   BRANCHLESS-CASE
   ORDER-CASE
   MAXF-CASE
   SHAPE-CASE
   FLOAT-PLACE-CASES
   FLOAT-REFUSAL-CASES
   HELD-CASE
   HELD-REFUSAL-CASE
   HELD-RECORD-CASE
   MEASURE-CASE
   MEASURE-AGREE-CASE
   MEASURE-REFUSAL-CASE
   T-REPORT ;

;package

NMIGRATE-TEST:RUN
