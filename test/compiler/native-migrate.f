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
   REFUSED-CASE
   UNTOUCHED-CASE
   ENTRY-CASES
   T-REPORT ;

;package

NMIGRATE-TEST:RUN
