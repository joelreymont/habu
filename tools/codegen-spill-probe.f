\ codegen-spill-probe.f - where the register allocator's spill wall actually is,
\ measured through the real migration entry. One concern: pinning WHICH property
\ of a body reaches a refusal, and WHICH refusal it reaches.
\
\ WHY THIS EXISTS. Two corpus rows were refused with the same code, -8508, and
\ the obvious reading of them - "too many values live inside a loop" - was wrong
\ for one of the two. Reading it wrongly sends the fix at the wrong pass, so the
\ discriminating cases are kept here and run as a suite member rather than left
\ in a scratch file. Every case below goes through NMIGRATE's own entry, the same
\ one tools/codegen-compare-new4.f uses, so what is measured is the production
\ chain and not a model of it.
\
\ THERE ARE TWO WALLS AND THEY INTERLEAVE, WHICH IS THE FIRST THING TO KNOW.
\ E-A64RA-SPILL is the register allocator with nowhere left to put a value.
\ E-IR-CTX-SCRATCH is the migration CONTEXT with no mapping left: one run holds
\ up to four modules at once and they all come out of its 512K
\ (src/compiler/ir/context.f:85-105). Since the pool became the machine's whole
\ writable set rather than a run of eighteen, the two walls sit close enough to
\ cross, and the loads shape below meets them in the order compile, scratch,
\ register, register, scratch as its count climbs by one. So every case asserts
\ WHICH code, never merely that something was refused, and a reader pricing a
\ register fix off one of these rows has to check the code first.
\
\ WHAT THE CASES ESTABLISH, IN ORDER.
\
\   THE LOADS SHAPE'S WALL IS WIDTH, AND WHERE THE VALUES ARE HELD DECIDES IT.
\   The reads and the additions between them no longer live inside the body at
\   all - src/compiler/native/loop.f moves work that cannot change with the turn
\   into the pre-header and folds what is left - so the count that matters is
\   what the PRE-HEADER may hold. Nothing crosses a call here, so this row really
\   is about how much one block may hold and about nothing else.
\
\   A CROSSING WALL IS NOT THAT. Ten values live across a loop that makes no call
\   compile, so being live across a loop is not what refuses them.
\
\   NOR IS IT THE CALL BY ITSELF. The same ten values live across the same call
\   with NO loop around it compile. So neither the loop alone nor the call alone
\   reaches the wall.
\
\   IT IS THE TWO TOGETHER, AND THE MECHANISM IS THE CROSSING. A local read after
\   a call is marked as one that must survive one (src/compiler/native/
\   elaborate.f CROSS-STEP), and a surviving value that has to TRAVEL is threaded
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
\   AND THE DECIDING PAIR IS SP-PRE10-N AND SP-EPOST10-N. The same ten values,
\   the same loop, the same call, the same pool - folded into the accumulator
\   BEFORE the loop they compile, read AFTER it they are refused. Nothing else
\   differs, so the crossing is the whole of it.
\
\   AND WHETHER A VALUE TRAVELS AT ALL IS THE CALLEE'S ANSWER, WHICH IS THE FIFTH
\   FACT AND THE ONE THAT MOVED A CORPUS ROW. Travelling buys a data-stack slot,
\   and it is worth buying only when no register would have survived the call.
\   There is one exactly when the callee published what it destroys, so the
\   elaborator asks (elaborate.f CALL-KEEPS?) and hands the value over only when
\   the answer is no. The corpus writes C-LONG once and both compilers make a
\   routine of it, so the two answers are measured with everything else held
\   still: the body SP-EPOST10-N is refused for compiles unchanged the moment it
\   names the chain's compilation of the same callee. What the record is WORTH is
\   pinned too - exactly one more crossing value, ten against eleven.
\
\   AND A CONSTANT IS NOT A LOAD, WHICH IS WHERE THE LAST FOUR CASES START. The
\   same count of values live inside one body is refused when they are loads and
\   compiles when they are one-move-wide constants, because a constant can be
\   written again where it is read instead of put away. What that did NOT move is
\   the same constants live ACROSS the loop, and the sixty-four-bit ones.
\
\ WHERE THE WALLS SIT, AND WHY THESE COUNTS HAVE MOVED BEFORE. The crossing wall
\ is at TEN values against a callee that published nothing, and nine is the
\ control beside it, so it is pinned from both sides. It has moved twice: at a
\ budget of eighteen registers it was at seven, and before the selection stage
\ began emitting the add and subtract immediate forms it was at six. WHICH
\ register those forms handed back was deliberately not claimed then and is not
\ claimed now - it would be a guess, and these cases are worth having precisely
\ because a wall's position is not derivable by reading the pass that moved it.
\
\ EACH TIME, THE CASES WERE RE-DERIVED TO STRADDLE THE WALL RATHER THAN RE-PINNED
\ to the new answers. What this file is for is the DISCRIMINATION - which
\ property reaches the refusal - and a case that has drifted to the compiling
\ side discriminates nothing.
\
\ WHAT A CHANGE TO THIS FILE MEANS. These are the current walls, not desired
\ ones. Each case is asserted with its code, and a fix must come here and say
\ what it moved. A pass that lets a crossing value live in a frame slot across a
\ loop would turn SP-EPOST10-N green; a context that sized its mapping from the
\ function would move every -6644 row here and none of the -8508 ones.

require lib/test.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-cases4.f
require src/compiler/native/clobber.f
require src/compiler/native/migrate.f

package CODEGEN-SPILL-PROBE

private

\ Every case is measured at NABI:SCRATCH, the machine's whole pool, so a refusal
\ here is never a budget that was set too low.

PTR-VARIABLE TRY-SRC
variable TRY-U

\ The migration, caught. Its throw code IS the measurement, so it is carried out
\ as data rather than allowed to end the run.
: MIGRATE-RC ( -- n )
   [: TRY-SRC @ TRY-U @ NMIGRATE:DEFINE ;] catch ;

\ Where a word's code starts, read off its own dictionary record. A name this
\ image does not hold is refused rather than answered with an address, because
\ every case below is about WHICH routine is branched to.
: ENTRY-OF ( ptr u8 n -- n ) {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   XREF-START ;

\ Does this routine publish what it destroys? It is the one property the two
\ callees below differ in, so it is measured off the record rather than argued
\ from which compiler made them.
: PUBLISHES-CLOBBER? ( ptr u8 n -- bool )
   ENTRY-OF NCLOB:KNOWN? ;

: STAGE ( ptr u8 n -- ) {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a TRY-SRC ! u TRY-U ! ;

: TRY ( ptr u8 n -- n )
   STAGE MIGRATE-RC ;

\ ---- every body here is written by counting rather than by hand --------------
\ THE CASES BELOW STRADDLE FIVE WALLS, so no body is transcribed. What each case
\ states is a number - the count either side of a wall - rather than a line of
\ literals a reader has to count to check, and moving a wall means changing the
\ number and nothing else.
1024 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-U

: SRC-RESET ( -- )
   0 SRC-U ! ;

: SRC+ ( ptr u8 n -- )
   {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   SRC-U @ u + SRC-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a  SRC-BUF SRC-U @ +  u STR-LEN BYTE-COPY-LEN
   SRC-U @ u + SRC-U ! ;

\ One number, spelled the way the source spells it. It goes through the shared
\ string builder and is copied straight back out, so the assembled source lives
\ in this file's own buffer: what is handed to a migration has to survive
\ everything the migration itself does with that builder.
: SRC-N+ ( n -- )
   SB-RESET FMT:SB-INT SB$ SRC+ ;

: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

\ The smallest constant here needs a move-wide of its own, and the step keeps
\ every one of them distinct AND past the addition's own immediate field - so
\ each really is a value the body has to hold, rather than an operand the combine
\ pass folds into the add that reads it.
40001 constant NARROW-BASE
37 constant CONST-STEP

\ A constant every one of whose four halfwords is non-zero. Materialising it is a
\ move-wide and three overwrites, which is the CHAIN a re-emission may never
\ stand for: the step is small enough that every constant built from it keeps all
\ four halfwords.
1234605616436508552 constant WIDE-BASE

: HEAD+ ( ptr u8 n n -- )
   {: a:ptr u:n k:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   s" : SP-" SRC+ a u SRC+ k SRC-N+
   s" -N ( n n -- n ) {: s:n l:n :} " SRC+ ;

: CONSTS+ ( n n -- )
   {: base:n k:n :}
   k 0 ?do base i CONST-STEP * + SRC-N+ s"  " SRC+ loop ;

: ADDS+ ( n -- )
   0 ?do s" + " SRC+ loop ;

\ K constants materialised INSIDE the loop body and folded into the accumulator
\ there. Every one of them is written and read within one turn, so all K are live
\ at once at the point the last is written and none of them crosses a block edge.
: INSIDE-SRC ( n n ptr u8 n -- ptr u8 n )
   {: base:n k:n a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   SRC-RESET
   a u k HEAD+
   s" s l 0 ?do " SRC+
   base k CONSTS+
   k ADDS+
   s" loop ;" SRC+
   SRC$ ;

\ The same K constants written BEFORE the loop and read AFTER it. Each one is
\ then live across the loop's edges, which makes it a class of more than one
\ value, and the eligibility test refuses such a class before it ever looks at
\ the operation that wrote it.
: ACROSS-SRC ( n n ptr u8 n -- ptr u8 n )
   {: base:n k:n a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   SRC-RESET
   a u k HEAD+
   base k CONSTS+
   s" s l 0 ?do 1 + loop " SRC+
   k ADDS+
   s" ;" SRC+
   SRC$ ;

\ K reads of one base pointer, every one of them inside the loop body.
: LOADS+ ( n -- )
   {: k:n :}
   s" base @ " SRC+
   k 1 ?do s" base " SRC+ i 8 * SRC-N+ s"  + @ " SRC+ loop ;

: LOADS-SRC ( n -- ptr u8 n )
   {: k:n :}
   SRC-RESET
   s" : SP-PP" SRC+ k SRC-N+
   s" -N ( ptr n n -- n ) {: base:ptr len:n :} 0 len 0 ?do " SRC+
   k LOADS+
   k ADDS+
   s" loop ;" SRC+
   SRC$ ;

\ ---- K values a body derives for itself, which is how the counts get past ten -
\ A ROUTINE MAY DECLARE TEN PLACES AND NO MORE - src/compiler/a64-effect.f
\ refuses an eleventh with E-A64EFF-SEQ, and it is a place list rather than a
\ register budget. So a crossing case wider than that cannot take its values as
\ ARGUMENTS: it derives them from one seed instead, each distinct and each read
\ exactly once, which leaves the count of live values free of the arity. The
\ other trap for whoever writes the next case here is a generated locals list
\ that reaches `i` or `j`: those name what the dialect already models and are
\ refused E-NELAB-LOCAL, a code that reads exactly like a register wall.

: TERMS+ ( n -- )
   {: k:n :}
   k 1 + 1 ?do s" s " SRC+ i SRC-N+ s"  + " SRC+ loop ;

\ The K terms folded into the accumulator BEFORE the loop, so not one of them is
\ live when the call is made.
: PRE-SRC ( n -- ptr u8 n )
   {: k:n :}
   SRC-RESET
   s" : SP-PRE" SRC+ k SRC-N+
   s" -N ( n n -- n ) {: s:n len:n :} " SRC+
   k TERMS+
   k 1 - ADDS+
   s" len 0 ?do CODEGEN-CORPUS4:C-LONG loop ;" SRC+
   SRC$ ;

\ The same K terms read AFTER the loop, so every one of them crosses the call the
\ loop makes. The callee is named rather than assumed: which compilation of it
\ the body reaches is the fifth fact.
: POST-SRC ( n ptr u8 n ptr u8 n -- ptr u8 n )
   {: k:n t:ptr tu:n c:ptr cu:n :} \ typed-local-lint: allow-bare-local - t and c keep the ptr u8 byte-span role
   SRC-RESET
   s" : SP-" SRC+ t tu SRC+ s" POST" SRC+ k SRC-N+
   s" -N ( n n -- n ) {: s:n len:n :} " SRC+
   k TERMS+
   s" s len 0 ?do " SRC+ c cu SRC+ s"  loop " SRC+
   k ADDS+
   s" ;" SRC+
   SRC$ ;

: EPOST-SRC ( n -- ptr u8 n )
   s" E" s" CODEGEN-CORPUS4:C-LONG" POST-SRC ;

: CPOST-SRC ( n -- ptr u8 n )
   s" C" s" CODEGEN-CORPUS4:C-LONG-N" POST-SRC ;

\ The same K terms live across a loop that makes NO call.
: NOCALL-SRC ( n -- ptr u8 n )
   {: k:n :}
   SRC-RESET
   s" : SP-NL" SRC+ k SRC-N+
   s" -N ( n n -- n ) {: s:n len:n :} " SRC+
   k TERMS+
   s" s len 0 ?do 1 + loop " SRC+
   k ADDS+
   s" ;" SRC+
   SRC$ ;

\ And the same K terms live across a call with NO loop around it.
: NOLOOP-SRC ( n -- ptr u8 n )
   {: k:n :}
   SRC-RESET
   s" : SP-NC" SRC+ k SRC-N+
   s" -N ( n n -- n ) {: s:n len:n :} " SRC+
   k TERMS+
   s" s CODEGEN-CORPUS4:C-LONG " SRC+
   k ADDS+
   s" ;" SRC+
   SRC$ ;

\ ---- the loads shape, which meets both walls ---------------------------------

\ WHERE EACH WALL IS AND WHICH ONE COMES FIRST. Twenty reads a turn compile.
\ Twenty-one are refused by the CONTEXT: the allocator gets through it, plans
\ five re-emissions, and the module the rewrite writes is the one that does not
\ fit. Twenty-two through twenty-four are refused by the ALLOCATOR, which is what
\ the file is named for - no rewrite is planned at all there, so the context is
\ never asked. Twenty-five is refused by the context again, and this time before
\ any allocation: selecting and combining the body alone already exhausts the
\ mapping. The corpus row's own fourteen sits well inside all of it and compiles,
\ which is what the loop-invariant move bought this shape.
: LOOP-WIDTH-CASES ( -- )
   s" the corpus row's own fourteen, which was the refusal this file opened for"
   T-LABEL
   14 LOADS-SRC TRY 0 T=

   s" and twenty read a turn, which is the widest that compiles" T-LABEL
   20 LOADS-SRC TRY 0 T=

   s" twenty-one plan five re-emissions and the rewritten module wants arena"
   T-LABEL
   21 LOADS-SRC TRY E-IR-CTX-SCRATCH T=

   s" twenty-two plan none and are refused a register instead" T-LABEL
   22 LOADS-SRC TRY E-A64RA-SPILL T=

   s" so are twenty-four, which is that wall's far side" T-LABEL
   24 LOADS-SRC TRY E-A64RA-SPILL T=

   s" and twenty-five run the mapping out before a register is ever asked for"
   T-LABEL
   25 LOADS-SRC TRY E-IR-CTX-SCRATCH T= ;

\ ---- what the crossing refusal is NOT ----------------------------------------
\ Both controls carry the SAME count as the refusal below, which is the only way
\ they control for anything.

: NOT-THE-LOOP-CASES ( -- )
   s" ten values live ACROSS a callless loop compile: residency is not it"
   T-LABEL
   10 NOCALL-SRC TRY 0 T= ;

: NOT-THE-CALL-CASES ( -- )
   s" ten values across a call with NO loop compile: the call is not it" T-LABEL
   10 NOLOOP-SRC TRY 0 T= ;

\ ---- what it IS: the crossing, measured by moving one thing ------------------
\ The same ten values, the same loop, the same call, the same pool. The only
\ difference between the two cases is whether they are folded into the
\ accumulator before the loop or read after it, which is exactly what decides
\ whether they must survive the call.
\
\ AND THE CALLEE IS THE ENGINE'S C-LONG, WHICH IS NOT A DETAIL. A value is handed
\ over at a call only when no register would have survived it, and what decides
\ that is whether the callee published a record of what it destroys. The engine's
\ compilation published none, so against it these values really do travel and
\ this pair measures the crossing. Against a callee that DID publish one they
\ travel one value later, which is the next section, not a hole in this one.
: CROSSING-CASES ( -- )
   s" ten values folded in BEFORE a loop that calls compile" T-LABEL
   10 PRE-SRC TRY 0 T=

   s" and read AFTER it they are refused: they had to travel" T-LABEL
   10 EPOST-SRC TRY E-A64RA-SPILL T= ;

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
\ BOTH WALLS ARE PINNED FROM BOTH SIDES HERE, WHICH IS WHAT PRICES THE RECORD.
\ Against the callee that published nothing the wall is at ten and nine compile;
\ against the one that published a record it is at eleven and ten compile. So
\ what a clobber record is worth to a crossing body is exactly one more live
\ value, and a change claiming more or less has a row here to move.
: RECORD-CASES ( -- )
   s" the chain's callee publishes what it destroys" T-LABEL
   s" CODEGEN-CORPUS4:C-LONG-N" PUBLISHES-CLOBBER? TTRUE

   s" and the engine's compilation of the same text does not" T-LABEL
   s" CODEGEN-CORPUS4:C-LONG" PUBLISHES-CLOBBER? TFALSE

   s" the refused body compiles against the callee that published one" T-LABEL
   10 CPOST-SRC TRY 0 T=

   s" and one value wider that callee runs out too" T-LABEL
   11 CPOST-SRC TRY E-A64RA-SPILL T=

   s" nine crossing the callee that published none compile" T-LABEL
   9 EPOST-SRC TRY 0 T= ;

\ ---- the wall a value that can be WRITTEN AGAIN moves ------------------------
\ A CLASS WHOSE ONE VALUE WAS WRITTEN BY A MOVE-WIDE NEEDS NO FRAME SLOT, because
\ writing it again where it is read costs exactly the one instruction the load it
\ replaces would have been (src/compiler/native/regalloc.f MB-REMATABLE?). These
\ four cases are what that moved, what it did NOT move, and where it stops.
\
\   THE WALL IT MOVED. Twenty constants materialised inside a loop body are held
\   outright; the twenty-first is compiled by writing one of them again, with an
\   EMPTY FRAME - which is the whole of the claim: this is not a spill that found
\   somewhere to put the value, it is a value that needed nowhere. Forty-three go
\   the same way on twenty-three re-emissions.
\
\   THE WALL IT DID NOT MOVE, AND WHY THAT IS THE SAME RULE. The same constants
\   written before the loop and read after it stop at fifteen. Each of them is
\   live across the loop's edges, which joins it with the block arguments
\   carrying it round into a class of MORE THAN ONE value, and a class of more
\   than one value is not a constant this pass can write again - there is no
\   single operation to re-emit. That shape then meets BOTH walls in turn:
\   sixteen still plan re-emissions for what they can and run the context out,
\   twenty plan none and are refused a register.
\
\   AND THE CHAIN IS REFUSED WHERE THE SINGLE MOVE-WIDE IS ADMITTED, which is the
\   one-variable contrast this file is for. Twenty-two constants inside the body
\   compile when each is one move-wide and are refused when each is sixty-four
\   bits wide: same count, same shape, same pool, and the only difference is that
\   a wide constant is a move-wide followed by three overwrites. Re-emitting the
\   end of that chain would mean re-emitting all of it, so it is not one
\   instruction and not a candidate. Twenty of the wide ones compile, which is
\   what says the refusal is the re-emission and not the width. That is what
\   protects CODEGEN-CORPUS4:BIG-CONSTS, whose four constants are exactly this
\   shape, and it is asserted here as a refusal rather than as a byte count
\   because a byte count would pass for a body that never reached the wall.
\
\   WHICH OF THE TWO EXCLUSIONS EACH CASE HOLDS IS A MEASUREMENT. The pass
\   refuses a chain twice over - the tie makes it a class of MORE THAN ONE value,
\   and the opcode is not the one form that stands alone - and only one of the
\   two is what this wide case moves. Deleting the class-size exclusion turns it
\   from E-A64RA-SPILL into re-emissions and E-IR-OP-OWNER, so the wide case
\   holds that one; deleting the OPCODE test leaves it exactly where it is,
\   because the size test has already refused the chain before the opcode is
\   read. What holds the opcode test is the loads shape at the top of this file:
\   its reads become re-emissions and E-IR-VERIFY-ATTRKEY without it, because a
\   load is a class of one value that no move-wide wrote - a move-wide is then
\   emitted carrying the load's own attributes and the module itself refuses it.
\   Neither case covers both, and a reader taking one of them for the whole rule
\   would find a deletion it expected to redden going green.
\
\   WHERE IT STOPS IS NOT THE ALLOCATOR. Forty-four of them is refused with
\   E-IR-CTX-SCRATCH, which is the migration context's mapping and not a register
\   at all. It is pinned from both sides here, and it is held out against dot
\   habu-who-owns-the-82b7ceb2, which asks who owns that limit;
\   habu-fit-a-rewritten-59aa92b7 is the same capacity seen from the rewrite's
\   side.
\
\ AND WHAT THE COUNTS SAY ABOUT THE LOADS SHAPE AT THE TOP OF THIS FILE. The
\ constants shape keeps its loop where the loads shape no longer has one, and
\ that is not about how many values either holds: a turn's worth of constants is
\ a NUMBER, and src/compiler/native/loop.f folds numbers into what a turn adds
\ rather than moving them, so a body whose addend is a TREE of constants is
\ declined and its values are still held a turn at a time. What a body may hold
\ therefore still depends on what the values ARE - the loads shape also holds its
\ base pointer live across the whole body while the constants shape holds nothing
\ else at all.
: REMAT-INSIDE-CASES ( -- )
   s" twenty one-movz constants inside a loop body fit with nothing decided"
   T-LABEL
   NARROW-BASE 20 s" RIN" INSIDE-SRC TRY 0 T=
   NMIGRATE:REMATS 0 T=

   s" and twenty-one compile by writing one of them again, taking no frame"
   T-LABEL
   NARROW-BASE 21 s" RIN" INSIDE-SRC TRY 0 T=
   NMIGRATE:REMATS 1 T=
   NMIGRATE:SPILLS 0 T= ;

: REMAT-ACROSS-CASES ( -- )
   s" fifteen of the same constants live ACROSS the loop compile" T-LABEL
   NARROW-BASE 15 s" RAC" ACROSS-SRC TRY 0 T=

   s" sixteen re-emit what they can and run the context out" T-LABEL
   NARROW-BASE 16 s" RAC" ACROSS-SRC TRY E-IR-CTX-SCRATCH T=

   s" and twenty re-emit nothing: a class of two values is no candidate" T-LABEL
   NARROW-BASE 20 s" RAC" ACROSS-SRC TRY E-A64RA-SPILL T= ;

: REMAT-WIDE-CASES ( -- )
   s" twenty-two one-movz constants inside the body compile on two re-emissions"
   T-LABEL
   NARROW-BASE 22 s" RWC" INSIDE-SRC TRY 0 T=
   NMIGRATE:REMATS 2 T=

   s" and twenty-two sixty-four-bit ones are refused: a chain is not one form"
   T-LABEL
   WIDE-BASE 22 s" RWI" INSIDE-SRC TRY E-A64RA-SPILL T=

   s" while twenty of the wide ones compile: it is the re-emission, not the width"
   T-LABEL
   WIDE-BASE 20 s" RWI" INSIDE-SRC TRY 0 T= ;

: REMAT-SCRATCH-CASES ( -- )
   s" forty-three re-emitted constants compile" T-LABEL
   NARROW-BASE 43 s" RIN" INSIDE-SRC TRY 0 T=
   NMIGRATE:REMATS 23 T=

   s" and forty-four do not fit the migration context (who-owns-the-scratch)"
   T-LABEL
   NARROW-BASE 44 s" RIN" INSIDE-SRC TRY E-IR-CTX-SCRATCH T= ;

public

: RUN ( -- )
   T-RESET
   LOOP-WIDTH-CASES
   NOT-THE-LOOP-CASES
   NOT-THE-CALL-CASES
   CROSSING-CASES
   RECORD-CASES
   REMAT-INSIDE-CASES
   REMAT-ACROSS-CASES
   REMAT-WIDE-CASES
   REMAT-SCRATCH-CASES
   T-REPORT ;

;package

CODEGEN-SPILL-PROBE:RUN
