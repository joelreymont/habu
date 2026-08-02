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
\ `rshift` is not one of the source words the dialect models, so the elaborator
\ refuses the body by that dialect's own name. The engine has already published
\ the word at that point, which is exactly the state a refusal has to leave
\ working.
: SHIFT-SRC ( -- ptr u8 n )
   s" : NMG-SHIFT ( n -- n ) dup 2 rshift + ;" ;

: MIGRATE-SHIFT ( -- )
   SHIFT-SRC 1 1 REGS NMIGRATE:DEFINE ;

: REFUSED-CASE ( -- )
   s" a body outside the dialect is refused with the dialect's own code" T-LABEL
   [: MIGRATE-SHIFT ;] E-HIR-UNMODELED TTHROWSQ

   s" the word the engine published is still there and still runs" T-LABEL
   s" 12 NMG-SHIFT" EV-N 15 T=
   s" 40 NMG-SHIFT" EV-N 50 T=

   s" and the publication seam never logged it" T-LABEL
   s" NMG-SHIFT" GLOBAL-WID NPUB:REPUBLISHED? TFALSE ;

\ The record of the refused word, read before the migration is attempted and
\ again after it, so "untouched" is a measurement rather than an inference. The
\ definition is made here and migrated in the case above, which is why the two
\ halves are separate words: the record has to be read between them.
: SHIFT-BEFORE ( -- )
   s" : NMG-SHIFT2 ( n -- n ) dup 2 rshift + ;" EV
   s" NMG-SHIFT2" REC-START OLD-START !
   s" NMG-SHIFT2" REC-LEN OLD-LEN ! ;

: MIGRATE-SHIFT2 ( -- )
   s" : NMG-SHIFT3 ( n -- n ) dup 2 rshift + ;" 1 1 REGS NMIGRATE:DEFINE ;

: UNTOUCHED-CASE ( -- )
   SHIFT-BEFORE
   [: MIGRATE-SHIFT2 ;] E-HIR-UNMODELED TTHROWSQ

   s" a refusal leaves the record of the word it was given exactly as it was" T-LABEL
   s" NMG-SHIFT3" REC-START  s" NMG-SHIFT3" REC-LEN  {: st:n ln:n :}
   s" NMG-SHIFT2" REC-LEN OLD-LEN @ T=
   s" NMG-SHIFT2" REC-START OLD-START @ T=

   s" and the word it refused still runs the code the engine compiled for it" T-LABEL
   ln s" NMG-SHIFT2" REC-LEN T=
   st 0 T<>
   s" 12 NMG-SHIFT3" EV-N 15 T= ;

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

: RUN ( -- )
   T-RESET
   MIGRATED-CASE
   CALL-CASE
   DEEP-CASE
   INTEROP-CASE
   CALL-REFUSAL-CASES
   REFUSED-CASE
   UNTOUCHED-CASE
   ENTRY-CASES
   T-REPORT ;

;package

NMIGRATE-TEST:RUN
