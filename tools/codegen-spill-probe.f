\ codegen-spill-probe.f - where the register allocator's spill wall actually is,
\ measured through the real migration entry. One concern: pinning the five facts
\ that say WHICH property of a body reaches E-A64RA-SPILL.
\
\ WHY THIS EXISTS. Two corpus rows are refused with the same code, -8508, and the
\ obvious reading of them - "too many values live inside a loop" - is wrong for
\ one of the two. Reading it wrongly sends the fix at the wrong pass, so the
\ discriminating cases are kept here and run as a suite member rather than left
\ in a scratch file. Every case below goes through NMIGRATE's own entry, the same
\ one tools/codegen-compare-new4.f uses, so what is measured is the production
\ chain and not a model of it.
\
\ WHAT THE CASES ESTABLISH, IN ORDER.
\
\   PRESSURE-LOOP's wall is width inside a loop body. Fourteen values loaded and
\   held live inside the body is refused; thirteen compiles. Nothing crosses a
\   call here, so this row really is about how much a loop body may hold.
\
\   CALL-PRESSURE's wall is NOT that. Eight values live ACROSS a loop that makes
\   no call compile fine, so being live across a loop is not what refuses them.
\
\   NOR IS IT THE CALL BY ITSELF. The same eight values live across the same call
\   with NO loop around it compile fine. So neither the loop alone nor the call
\   alone reaches the wall.
\
\   IT IS THE TWO TOGETHER, AND THE MECHANISM IS THE CROSSING. A local read after
\   a call is marked as one that must survive one (src/compiler/native/
\   elaborate.f CROSS-STEP), and a surviving local is then threaded through the
\   loop twice over: as a BLOCK ARGUMENT of every block on the path (LOCAL-ARGS+)
\   and as an OPERAND AND RESULT of the call itself (CALL-OPERANDS+). Both of
\   those are what put it beyond MB-SPILLABLE? in src/compiler/native/regalloc.f.
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
\   AND THE DECIDING PAIR IS SP-PRE8-N AND SP-POST8-N. The same eight locals,
\   the same loop, the same call, the same budget - read BEFORE the loop they
\   compile, read AFTER it they are refused. Nothing else differs, so the
\   crossing is the whole of it, and a fix has to stop the crossing rather than
\   re-place what it creates.
\
\   AND STOPPING THE CROSSING IS NOT FREE, WHICH IS THE FIFTH FACT. The crossing
\   is also the only HOME a surviving local has when the callee publishes no
\   record of what it destroys, and the wall sits one value lower there: seven
\   cross the chain's C-LONG-N and compile, seven cross the engine's C-LONG - the
\   same text, the other compiler - and are refused. The section at RECORD-CASES
\   says what suppressing the threading does to each of those walls, measured.
\
\ WHERE THE WALL SITS, AND WHY THE COUNT IN THESE CASES MOVED ONCE. It is at
\ EIGHT crossing values today and it was at seven until the selection stage began
\ emitting the add and subtract immediate forms. That is a measurement through
\ the entry below and not a reading of the allocator: the same body that threw
\ -8508 at seven compiles at seven now, and eight is what throws. WHICH register
\ the immediate forms handed back is deliberately not claimed here - it would be
\ a guess, and the cases below are worth having precisely because the wall's
\ position is not derivable by reading the pass that moved it. The cases were
\ re-derived to straddle the new wall rather than re-pinned to the new answers,
\ because what this file is for is the DISCRIMINATION - which property reaches
\ the refusal - and a case that has drifted to the compiling side of the wall
\ discriminates nothing. The seven-value body is kept as the control directly
\ beneath, so the wall's position is pinned from both sides.
\
\ WHAT A CHANGE TO THIS FILE MEANS. These are the current walls, not desired
\ ones. A pass that lets a crossing local live in a frame slot across a loop
\ turns POST8 AND EPOST7 green - both walls at once, because both are the same
\ shortage of somewhere to put a value - and this file is where that is recorded
\ rather than discovered, so each case is asserted with its code and a fix must
\ come here and say what it moved.

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
\ The same seven locals, the same loop, the same call, the same budget. The only
\ difference between the two cases is whether the locals are read before the loop
\ or after it, which is exactly what decides whether they must survive the call.

: CROSSING-CASES ( -- )
   s" eight locals read BEFORE a loop that calls compile" T-LABEL
   s" : SP-PRE8-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n len:n :} a b + c + d + e + f + g + h + seed + len 0 ?do CODEGEN-CORPUS4:C-LONG-N loop ;"
   10 TRY-CALLING 0 T=

   s" and read AFTER it they are refused - this is CALL-PRESSURE" T-LABEL
   s" : SP-POST8-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG-N loop a + b + c + d + e + f + g + h + ;"
   10 TRY-CALLING E-A64RA-SPILL T=

   s" seven across the same call in the same loop compiles" T-LABEL
   s" : SP-POST7-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG-N loop a + b + c + d + e + f + g + ;"
   9 TRY-CALLING 0 T= ;

\ ---- and what the threading is FOR, which is a second wall -------------------
\ THE CASES ABOVE ARE ALL MEASURED AGAINST A CALLEE THE CHAIN PUBLISHED, and that
\ is not a neutral choice: such a routine records which registers its accepted
\ allocation writes (src/compiler/native/clobber.f), so some register survives the
\ branch and a crossing value may stay in one. A routine with no such row destroys
\ the whole pool as far as every reader is concerned, and then NO register
\ survives - the only place a crossing value can be is the data-stack slot the
\ call's own operand list buys it.
\
\ SO THE SAME BODY HAS TWO WALLS AND THE RECORD IS THE DIFFERENCE. The corpus
\ writes C-LONG once; the engine compiles it and the chain compiles it again as
\ C-LONG-N, and the case below names the engine's where SP-POST7-N above names
\ the chain's. Seven values cross the chain's and compile; seven cross the
\ engine's and are refused, and six compile. Nothing else about the two cases
\ differs - same locals, same loop, same budget, same text for the callee - so
\ the record is worth exactly one crossing value here, and it is measured rather
\ than reasoned from what a record ought to buy.
\
\ AND THE ENGINE'S CALLEE IS REALLY CALLED AND NOT COPIED, which the pair says
\ without a second reader: a body the chain had inlined would hold no call at
\ all, and SP-ACROSS8-N above is exactly that body - eight values across a
\ callless loop - and compiles. Seven refused is therefore a call.
\
\ WHICH IS WHY THE THREADING CANNOT SIMPLY BE DELETED, and that was measured too
\ rather than argued. Suppressing it - making CROSS-L answer nought, so a
\ surviving local is neither a block argument of the blocks on its path nor an
\ operand of the call - lifts the chain-callee wall from seven to eight and turns
\ CALL-PRESSURE green, and it takes the engine-callee wall from six to NONE: a
\ body with ONE local live across a call to a routine with no record is refused
\ E-A64RA-POOL, because the value has nowhere at all to be. Two programs the tree
\ compiles today are exactly that shape - the stdlib's own multishot site
\ ARRAY:A-MAPI!, migrated at file level in test/compiler/native-exec.f, and
\ LGP-CALL in test/compiler/native-migrate.f - and both refuse under the
\ suppression. The threading is a HOME and not only a guard, and a change that
\ removes it has to give the value another one.
: RECORD-CASES ( -- )
   s" the chain's callee publishes what it destroys" T-LABEL
   s" CODEGEN-CORPUS4:C-LONG-N" PUBLISHES-CLOBBER? TTRUE

   s" and the engine's compilation of the same text does not" T-LABEL
   s" CODEGEN-CORPUS4:C-LONG" PUBLISHES-CLOBBER? TFALSE

   s" seven crossing the engine's callee are refused where seven crossed the chain's"
   T-LABEL
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
