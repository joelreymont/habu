\ codegen-spill-probe.f - where the register allocator's spill wall actually is,
\ measured through the real migration entry. One concern: pinning the five facts
\ that say WHICH property of a body reaches E-A64RA-SPILL.
\
\ WHY THIS EXISTS. Two corpus rows were refused with the same code, -8508, and
\ the obvious reading of them - "too many values live inside a loop" - was wrong
\ for one of the two. Reading it wrongly sends the fix at the wrong pass, so the
\ discriminating cases are kept here and run as a suite member rather than left
\ in a scratch file. One of the two rows compiles now, and this file is where
\ what moved it is measured. Every case below goes through NMIGRATE's own entry,
\ the same one tools/codegen-compare-new4.f uses, so what is measured is the
\ production chain and not a model of it.
\
\ WHAT THE CASES ESTABLISH, IN ORDER.
\
\   PRESSURE-LOOP's wall is width inside a loop body. Fourteen values loaded and
\   held live inside the body is refused; thirteen compiles. Nothing crosses a
\   call here, so this row really is about how much a loop body may hold.
\
\   A CROSSING WALL IS NOT THAT. Eight values live ACROSS a loop that makes no
\   call compile fine, so being live across a loop is not what refuses them.
\
\   NOR IS IT THE CALL BY ITSELF. The same eight values live across the same call
\   with NO loop around it compile fine. So neither the loop alone nor the call
\   alone reaches the wall.
\
\   IT IS THE TWO TOGETHER, AND THE MECHANISM IS THE CROSSING. A local read after
\   a call is marked as one that must survive one (src/compiler/native/
\   elaborate.f CROSS-STEP), and a surviving local that has to TRAVEL is threaded
\   through the loop twice over: as a BLOCK ARGUMENT of every block on the path
\   (LOCAL-ARGS+) and as an OPERAND AND RESULT of the call itself
\   (CALL-OPERANDS+). Both of those are what put it beyond MB-SPILLABLE? in
\   src/compiler/native/regalloc.f.
\
\   AND WHICH OF THE TWO IS LOAD-BEARING IS A MEASUREMENT, NOT A READING. Neither
\   alone is. Removing the block-argument marking from MB-KEEP-BLOCK leaves the
\   refusal exactly where it was; so does cutting the middle block's rule down to
\   the entry block's; so does relaxing the multi-value class exclusion on its
\   own. Only relaxing BOTH the KEEP test and the class-size test together moves
\   it, and then A64RAV refuses the result with E-A64RAV-REGISTER. The reason is
\   that the classes holding registers at the failing position are excluded for
\   DIFFERENT reasons - some kept, some tied by an edge into a class of more than
\   one value - so lifting any single exclusion still leaves every candidate
\   excluded by another. Anyone reading one of those exclusions as "the cause"
\   and fixing it will find the refusal unmoved; that is what these mutations are
\   recorded for.
\
\   AND THE DECIDING PAIR IS SP-PRE8-N AND SP-POST8-N. The same eight locals, the
\   same loop, the same call, the same budget - read BEFORE the loop they
\   compile, read AFTER it they are refused. Nothing else differs, so the
\   crossing is the whole of it.
\
\   AND WHETHER A LOCAL TRAVELS AT ALL IS THE CALLEE'S ANSWER, WHICH IS THE FIFTH
\   FACT AND THE ONE THAT MOVED A CORPUS ROW. Travelling buys a data-stack slot,
\   and it is worth buying only when no register would have survived the call.
\   There is one exactly when the callee published what it destroys, so the
\   elaborator asks (elaborate.f CALL-KEEPS?) and hands the local over only when
\   the answer is no. The corpus writes C-LONG once and both compilers make a
\   routine of it, so the two answers are measured with everything else held
\   still: the body SP-POST8-N is refused for compiles unchanged the moment it
\   names the chain's compilation of the same callee. That body is corpus 4's
\   CALL-PRESSURE, which was the second -8508 row and is a measured row now.
\
\ WHERE THE WALL SITS, AND WHY THE COUNT IN THESE CASES HAS MOVED BEFORE. It is
\ at SEVEN crossing values against a callee that published nothing, and six is
\ the control beside it, so it is pinned from both sides. It has moved once
\ already: when every local travelled whatever the callee did, the same shape's
\ wall was at seven and had been at six until the selection stage began emitting
\ the add and subtract immediate forms. WHICH register those forms handed back
\ was deliberately not claimed then and is not claimed now - it would be a guess,
\ and these cases are worth having precisely because the wall's position is not
\ derivable by reading the pass that moved it. Whether that stage moved the wall
\ these cases now straddle by the same one is NOT measured, and is not asserted.
\
\ EACH TIME, THE CASES WERE RE-DERIVED TO STRADDLE THE WALL RATHER THAN RE-PINNED
\ to the new answers. What this file is for is the DISCRIMINATION - which
\ property reaches the refusal - and a case that has drifted to the compiling
\ side discriminates nothing.
\
\ WHAT A CHANGE TO THIS FILE MEANS. These are the current walls, not desired
\ ones. A pass that lets a crossing local live in a frame slot across a loop
\ turns SP-PP14-N and SP-EPOST7-N green - both walls at once, because both are
\ the same shortage of somewhere to put a value - and this file is where that is
\ recorded rather than discovered, so each case is asserted with its code and a
\ fix must come here and say what it moved.

require lib/test.f
require lib/string.f
require tools/codegen-compare-cases4.f
require src/compiler/native/clobber.f
require src/compiler/native/migrate.f

package CODEGEN-SPILL-PROBE

private

\ The register budget every case is measured at: the largest the architecture
\ allows, so a refusal is never a budget that was set too low.
18 constant REGS

PTR-VARIABLE TRY-SRC
variable TRY-U
variable TRY-IN

\ The migration, caught. Its throw code IS the measurement, so it is carried out
\ as data rather than allowed to end the run.
: MIGRATE-RC ( -- n )
   [: TRY-SRC @ TRY-U @ TRY-IN @ 1 REGS NMIGRATE:DEFINE ;] catch ;

: MIGRATE-CALLING-RC ( -- n )
   [: TRY-SRC @ TRY-U @ TRY-IN @ 1 REGS NMIGRATE:DEFINE-CALLING ;] catch ;

\ Where a word's code starts, read off its own dictionary record. A name this
\ image does not hold is refused rather than answered with an address, because
\ every case below is about WHICH routine is branched to.
: ENTRY-OF ( ptr u8 n -- n ) {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-NMIGRATE-CALLEE throw then
   XREF-START ;

\ The callee the calling cases branch to, staged from the dictionary so that what
\ is refused is the body under test and never a missing routine.
: CALLEE-AT ( ptr u8 n -- ) {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u
   a u ENTRY-OF 1 1 NMIGRATE:CALLEE ;

\ Does this routine publish what it destroys? It is the one property the two
\ callees below differ in, so it is measured off the record rather than argued
\ from which compiler made them.
: PUBLISHES-CLOBBER? ( ptr u8 n -- bool )
   ENTRY-OF NCLOB:KNOWN? ;

: STAGE ( ptr u8 n n -- ) {: a:ptr u:n in:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a TRY-SRC ! u TRY-U ! in TRY-IN ! ;

: TRY ( ptr u8 n n -- n )
   STAGE MIGRATE-RC ;

\ WHICH ROUTINE A CASE BRANCHES TO IS ITS OWN TEXT'S ANSWER, NOT THIS STAGING'S,
\ and it was measured rather than assumed: staging the chain's C-LONG-N under a
\ body that names the engine's C-LONG changes no answer below, because
\ DEFINE-CALLING resolves the body's names off the dictionary. What the staging
\ is for is the entry's own precondition - a migration with no callee staged is
\ refused - so one staged row serves every calling case here, and a case that
\ wants the other compilation of the callee says so in its own source.
: TRY-CALLING ( ptr u8 n n -- n )
   STAGE
   s" CODEGEN-CORPUS4:C-LONG-N" CALLEE-AT
   MIGRATE-CALLING-RC ;

\ ---- the two walls the corpus already names ----------------------------------

: LOOP-WIDTH-CASES ( -- )
   s" fourteen values live inside a loop body is refused (PRESSURE-LOOP)" T-LABEL
   s" : SP-PP14-N ( ptr n n -- n ) {: base:ptr len:n :} 0 len 0 ?do base @ base 8 + @ base 16 + @ base 24 + @ base 32 + @ base 40 + @ base 48 + @ base 56 + @ base 64 + @ base 72 + @ base 80 + @ base 88 + @ base 96 + @ base 104 + @ + + + + + + + + + + + + + + loop ;"
   2 TRY E-A64RA-SPILL T=

   s" and thirteen in the same body compiles" T-LABEL
   s" : SP-PP13-N ( ptr n n -- n ) {: base:ptr len:n :} 0 len 0 ?do base @ base 8 + @ base 16 + @ base 24 + @ base 32 + @ base 40 + @ base 48 + @ base 56 + @ base 64 + @ base 72 + @ base 80 + @ base 88 + @ base 96 + @ + + + + + + + + + + + + + loop ;"
   2 TRY 0 T= ;

\ ---- what CALL-PRESSURE's refusal is NOT -------------------------------------

: NOT-THE-LOOP-CASES ( -- )
   s" eight values live ACROSS a callless loop compile: residency is not it"
   T-LABEL
   s" : SP-ACROSS8-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n len:n :} seed len 0 ?do 1 + loop a + b + c + d + e + f + g + h + ;"
   10 TRY 0 T= ;

: NOT-THE-CALL-CASES ( -- )
   s" eight locals across a call with NO loop compile: the call is not it"
   T-LABEL
   s" : SP-SL8-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n :} seed CODEGEN-CORPUS4:C-LONG-N a + b + c + d + e + f + g + h + ;"
   9 TRY-CALLING 0 T= ;

\ ---- what it IS: the crossing, measured by moving one thing ------------------
\ The same eight locals, the same loop, the same call, the same budget. The only
\ difference between the two cases is whether the locals are read before the loop
\ or after it, which is exactly what decides whether they must survive the call.
\
\ AND THE CALLEE IS THE ENGINE'S C-LONG, WHICH IS NOT A DETAIL. A local is handed
\ over at a call only when no register would have survived it, and what decides
\ that is whether the callee published a record of what it destroys. The engine's
\ compilation published none, so against it the locals really do travel and this
\ pair measures the crossing. Against a callee that DID publish one they do not
\ travel at all, and the same two bodies both compile - which is the next
\ section, not a hole in this one.
: CROSSING-CASES ( -- )
   s" eight locals read BEFORE a loop that calls compile" T-LABEL
   s" : SP-PRE8-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n len:n :} a b + c + d + e + f + g + h + seed + len 0 ?do CODEGEN-CORPUS4:C-LONG loop ;"
   10 TRY-CALLING 0 T=

   s" and read AFTER it they are refused: they had to travel" T-LABEL
   s" : SP-POST8-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG loop a + b + c + d + e + f + g + h + ;"
   10 TRY-CALLING E-A64RA-SPILL T= ;

\ ---- and what decides whether the crossing happens at all --------------------
\ WHAT THE CASES ABOVE MEASURE IS THE PRICE OF TRAVELLING, AND THIS SECTION IS
\ WHAT DECIDES WHETHER IT IS PAID. A routine this chain compiled records which
\ registers its accepted allocation writes (src/compiler/native/clobber.f), and
\ everything downstream reads that record: the allocator keeps a crossing value
\ out of those registers (src/compiler/native/regalloc.f MB-FORBID) and the
\ validator re-derives the same bar (regalloc-verify.f CLOB-AT). So against such
\ a callee a value that survives the call has somewhere to be, and the elaborator
\ leaves it there (src/compiler/native/elaborate.f CALL-KEEPS?). A routine with NO
\ row is taken to destroy the whole pool by both readers, nothing survives, and
\ the data-stack slot the call's operand list buys is the only home left.
\
\ THE CORPUS WRITES C-LONG ONCE AND BOTH COMPILERS MAKE A ROUTINE OF IT, which is
\ what lets the difference be measured with everything else held still. The very
\ body the section above is refused for compiles when the only change is which
\ compilation of the callee it names - and that is CALL-PRESSURE, corpus 4's row,
\ which was refused until the elaborator started asking.
\
\ THE ENGINE-CALLEE WALL IS STILL THERE AND IS PINNED FROM BOTH SIDES. Seven
\ crossing values are refused and six compile, so a change that moved it says so
\ here. The chain-callee wall is not pinned from its far side and cannot be on
\ this shape: nothing travels, so what would refuse is ordinary register
\ pressure, and eight is already the widest crossing this shape reaches - a ninth
\ needs an eleventh argument, and src/compiler/a64-effect.f refuses a routine of
\ eleven places (E-A64EFF-SEQ, and it is a place list rather than a register
\ budget). Two traps for whoever writes the next case here: that arity ceiling,
\ and a generated locals list that reaches `i`, which declares a name the dialect
\ already models and is refused E-NELAB-LOCAL - a code that reads exactly like a
\ register wall and is not one.
: RECORD-CASES ( -- )
   s" the chain's callee publishes what it destroys" T-LABEL
   s" CODEGEN-CORPUS4:C-LONG-N" PUBLISHES-CLOBBER? TTRUE

   s" and the engine's compilation of the same text does not" T-LABEL
   s" CODEGEN-CORPUS4:C-LONG" PUBLISHES-CLOBBER? TFALSE

   s" the refused body compiles against the callee that published one" T-LABEL
   s" : SP-CPOST8-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG-N loop a + b + c + d + e + f + g + h + ;"
   10 TRY-CALLING 0 T=

   s" seven crossing the callee that published none are refused" T-LABEL
   s" : SP-EPOST7-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG loop a + b + c + d + e + f + g + ;"
   9 TRY-CALLING E-A64RA-SPILL T=

   s" and six across it compile" T-LABEL
   s" : SP-EPOST6-N ( n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG loop a + b + c + d + e + f + ;"
   8 TRY-CALLING 0 T= ;

public

: RUN ( -- )
   T-RESET
   LOOP-WIDTH-CASES
   NOT-THE-LOOP-CASES
   NOT-THE-CALL-CASES
   CROSSING-CASES
   RECORD-CASES
   T-REPORT ;

;package

CODEGEN-SPILL-PROBE:RUN
