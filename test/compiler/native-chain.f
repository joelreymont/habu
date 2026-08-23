\ native-chain.f - SOURCE TEXT TO EXECUTED BYTES: the native chain's first
\ end-to-end run.
\
\ Every other suite in this family tests one leaf against fixtures it builds
\ itself. This one builds nothing. It hands a colon definition to `evaluate`,
\ lets the engine compile it the way it compiles every definition, and follows
\ the ONE artefact that comes back out of that compilation - the sealed source
\ tape the checker's own reader filled - through every remaining stage of the
\ native pipeline until the result is machine code, and then it runs that code
\ and compares its answer with what the interpreted word computes on the same
\ input.
\
\ THE CHAIN, IN THE ORDER IT RUNS:
\   evaluate                 the engine compiles `: NCH-SQ ( n -- n ) dup * ;`
\   engine check hook        the checker scans the reconstructed definition
\   NFEED unit               one row per token, in consumption order, and the
\                            sealed tape and verdict the unit answers
\   NELAB:COLON              the tape becomes HIR operations
\   NFIX:RUN                 select, allocate, accept, emit
\   NRUN:PUBLISH + EXEC1     the words become a routine this process calls
\
\ NOTHING IS HAND-BUILT ANYWHERE IN IT. This suite has no lexer, pushes no tape
\ row and assembles no instruction - and, unlike every other caller of the shared
\ chain fixtures, it does not even state the source text as a line the fixture
\ lexer will re-read: the tape comes off the engine's own compilation. The only
\ inputs are the definition's source text and the two counts this suite hands
\ NELAB:COLON directly, which is that word's own interface; everything
\ else is produced by the stage before it and checked by the stage after it. That
\ is what makes this case the chain's acceptance rather than another leaf test:
\ it can only pass if every leaf agrees with its neighbours about the same
\ definition.
\
\ THE BACK HALF IS THE SHARED FIXTURE. Selection, allocation, validation and
\ emission are driven through test/compiler/native-chain-fixture.f and published
\ through test/compiler/native-run-fixture.f, the same two files the emission
\ suite and the code generator comparison drive them through. So the one thing
\ this suite contributes is the FRONT half - a tape nobody typed - and the rest
\ of the chain is entered exactly as its own suites enter it.
\
\ WHY THE ANSWER IS COMPARED WITH THE INTERPRETED WORD. A table of expected
\ instruction words can only disagree with an emitter that CHANGED, never with
\ one that was always wrong. Here the compiled routine's answer is compared with
\ the answer the engine's own compilation of the same source gives, so the two
\ paths have to agree about the program.
\
\ ONE FIXTURE, ONE CONTEXT. This case builds a source module AND a machine
\ module, which is already most of the sixty-four live arena slots the registry
\ holds (see the note in test/compiler/native-emit.f), so it is the only case
\ that runs in its context.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require src/compiler/native/migrate.f
require test/compiler/native-chain-fixture.f
require test/compiler/native-run-fixture.f

package NCHAIN-TEST
private

\ ---- the boundaries this suite needs -----------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model and the
\ only way to put a definition through the real compile path from inside a test.
\ Publishing and calling the emitted bytes are NRUN's two trusted words, not this
\ file's.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

\ ---- counting the register-to-register copies that survived ------------------
\ How many of the emitted instructions are a copy from one register to another.
\ It is the measure of two rules at once, and the two cases below read it in
\ opposite directions.
\
\ WHERE THE COPIES COME FROM. src/compiler/native/select.f puts one a64.mov in
\ front of every value crossing an argument-carrying edge, because a block
\ argument and everything handed to it are one physical register and handing over
\ a value the program still holds would need two. Most of those copies are
\ unnecessary: the two ends usually are not live at the same instant, and then
\ src/compiler/native/regalloc.f gives them one register and
\ src/compiler/native/emit.f writes no instruction for a copy into the register
\ it comes from. What is left is the copies that had to stay.
\
\ WHY THE INSTRUCTION IS FOUND BY ITS SHAPE RATHER THAN COUNTED IN THE MODULE. A
\ module still holds the a64.mov whether or not it was emitted, so counting
\ operations would measure the selector and not the emitter. The bytes are what
\ runs. A copy of this dialect is Orr with the zero register, which is what
\ ENC-MOV builds and the only Orr anything in the chain emits, so the form with
\ its two register fields cleared identifies it exactly.
$FFE0FFE0 constant MOV-SHAPE         \ the Orr-with-zero-register form, register fields cleared
: MOV-FORM ( -- n )
   0 0 A64ASM:ENC-MOV MOV-SHAPE and ;

: COPIES ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ MOV-SHAPE and MOV-FORM = if 1+ then
   loop ;

\ ---- what the run parks ------------------------------------------------------
\ A quotation cannot read the enclosing word's locals, and the whole run is one
\ word, so the handles that cross a stage boundary are parked rather than bound.
here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER R-CTX IR-CTX:ctx
1 TYPED-BUFFER R-BLD IR-BUILD:builder
1 TYPED-BUFFER R-TAPE IR-ARENA:view
variable R-VERDICT                    \ the verdict the unit answered

: CC ( -- IR-CTX:ctx )           0 R-CTX @ ;
: BB ( -- IR-BUILD:builder )     0 R-BLD @ ;
: TAPE ( -- IR-ARENA:view )      0 R-TAPE @ ;
: VERDICT ( -- n )               R-VERDICT @ ;

\ The buffer the recorded definition's text is kept in. The producer copies the
\ reader's text here as the scan opens; instruction selection reads it back to
\ carry the spans into the machine module, and refuses it unless it digests to
\ the source the HIR module was compiled from.
256 constant TEXT-CAP
create TXT TEXT-CAP allot

64 constant TAPE-CAP
4 constant REGS

\ ---- stage N0: the definition the engine compiles -----------------------------
\ The source is the whole input of this suite. Squaring is the shape the value
\ vector is for: `dup` is a compile-time rename, so a correct chain spends one
\ multiply and no stack traffic on it.
: SRC ( -- ptr u8 n )
   s" : NCH-SQ ( n -- n ) dup * ;" ;

: HIR-MOD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

: MODEL ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   p r ;

\ Compile the definition through the production path with a unit open, and park
\ the sealed tape and the verdict the unit answers.
: RECORD ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

\ How many bytes the reader handed over, as the registry recorded them. The
\ source is named off the tape's own first span, so the length asked for is the
\ length of the source the recorded rows span into. It is read off the LIVE
\ builder, because the text has to be presented to instruction selection and
\ selection takes its binding before the module freezes.
: TEXT-LEN ( -- n )
   CC BB  TAPE BB IR-BUILD:MODULE-KEY 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC
   IR-BUILD:SOURCE-LEN ;

\ ---- stage N1 and the machine stages -----------------------------------------
\ Elaborate the produced tape into the module the tape was recorded into. The
\ freeze and everything after it belong to the shared chain fixture, so this word
\ stops at the point where the two halves meet.
\
\ THE ONE SEAM WHERE ARITY ENTERS, AND IT IS THIS SUITE'S AND NOT THE ENTRY'S.
\ `1 1` is the word's declared effect, handed to NELAB:COLON because this suite
\ drives the elaborator stage by stage and the elaborator takes the counts as
\ arguments. The PRODUCTION entry states nothing: src/compiler/native/migrate.f
\ KEEP-ARITY reads what the definition takes and leaves off the checker's
\ certificate through NDICT:SPELL-ARITY. So these two numbers stay on this line,
\ and what they are is a fixture's business rather than a compiler's.
: ELABORATE ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 1 1 NELAB:COLON drop ;

\ ---- the run -----------------------------------------------------------------
\ The text presented to selection is the one the unit kept, and selection refuses
\ it unless it digests to the source the HIR module carries - so a copy that
\ drifted stops the run here instead of moving spans onto other bytes. That
\ digest check is where the bytes are bound to the module in production, and it
\ is the only place that binding is stated: there is no separate certificate
\ value to restate it.
: CHAIN-BODY ( IR-CTX:ctx -- n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD
   TAPE NTAPE:TOKENS
   VERDICT
   p r ELABORATE
   CC BB TXT TEXT-LEN REGS NFIX:RUN
   A64EMIT:INSNS
   NFIX:RESULT-REG
   7 NRUN:PUBLISH NRUN:EXEC1
   s" 7 NCH-SQ" EV-N
   s" 11 NCH-SQ" EV-N ;

: CHAIN-CASE ( -- )
   s" a colon definition compiles from source text to executed bytes" T-LABEL
   NFIX:BINDING [: CHAIN-BODY ;] IR-CTX:WITH-CONTEXT
   121 T= 49 T= 49 T= 0 T= 2 T= -1 T= 3 T= ;

\ ---- the same definition, entered the way a word is --------------------------
\ Design section 7.6's convention, end to end: the routine is compiled to take
\ argument zero out of data-stack slot zero of the CALLER's stack and to leave
\ result zero in slot zero, published into code space, and then entered by the
\ branch the interpreter itself uses - `execute` on the address, with the data
\ stack live. Nothing marshals anything; the argument is already where the
\ routine reads it.
\
\ WHY THIS CASE IS THE PROOF THAT THE REGISTER IS THE RIGHT ONE. Which register
\ the running engine keeps its data-stack pointer in is a fact about the engine,
\ and this case is the only thing in the suite that can be wrong about it. The
\ emitted routine reads its argument through that register and writes its result
\ back through it; if it were any other register the value read would be whatever
\ that register happened to hold and the answer would not be the square of seven -
\ and the data-stack pointer would be left somewhere the engine could not run on,
\ so the comparison against the interpreted word on the line after would not even
\ be reached. Mutating A64EFF:DSTACK-GPR to any other allocatable number reddens
\ here, which is what makes the constant load-bearing rather than declared.
: SRC2 ( -- ptr u8 n )
   s" : NCH-SQD ( n -- n ) dup * ;" ;

: RECORD2 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC2 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: HABU-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD2
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 REGS 1 1 NFIX:RUN-HABU
   A64EMIT:INSNS
   7 NRUN:PUBLISH NRUN:ENTER1
   s" 7 NCH-SQD" EV-N
   s" 11 NCH-SQD" EV-N ;

\ Four instructions: the load, the multiply, the store and the return. NEITHER
\ pointer move is written, and that is the placement rather than an omission: the
\ routine takes one cell and leaves one, so the place the caller left the pointer
\ and the place it expects it back are the same place, the routine stands there,
\ and both adjustments are distances of zero. The two accesses reach their cell
\ one below that place, in the unscaled signed form - which is where the answer
\ below comes from, so a placement that stood somewhere else would read the wrong
\ cell and not the square of seven.
: HABU-CASE ( -- )
   s" a compiled word is entered and left through the data stack" T-LABEL
   NFIX:BINDING [: HABU-BODY ;] IR-CTX:WITH-CONTEXT
   121 T= 49 T= 49 T= 4 T= ;

\ ---- a definition whose two arms become a select -----------------------------
\ The same run over a body whose answer depends on which of two values it
\ chooses. `2dup < if swap then drop` is a two-armed selection whose arms are
\ single values and whose join is one block, which is exactly the shape
\ src/compiler/native/select.f converts: the two arms and the two-way branch go
\ away, both answers are computed, and a Csel picks between them. What comes out
\ is TWO blocks - the entry, which computes and chooses, and the join, which
\ publishes and returns - where the branching shape had four.
\
\ WHY THAT IS THE SHAPE WORTH PINNING HERE. docs/codegen-placement.md measured
\ this chain's branch-around idiom costing twenty-eight per cent at matched
\ placement against the engine on data no predictor can learn, and being twice as
\ sensitive to where the routine lands. A select removes the branch rather than
\ moving it, which is the only change that improves every placement at once, and
\ this case is the end-to-end evidence that the whole chain really emits one:
\ no conditional branch anywhere in the routine, and the answers unchanged.
\
\ THE ANSWERS ARE WHAT HOLD THE POLARITY. A Csel writes its FIRST source when
\ the condition holds, and the arm a Habu `if` takes is the source branch's
\ SECOND successor, so the conversion puts the second arm's value first. Getting
\ that the wrong way round answers 3 for `3 4 NCH-MAX` instead of 4 and 9 for
\ `9 -1` instead of -1's partner - which is why both an ordered pair and a pair
\ with a negative in it are run.
\
\ AND THE COPIES ARE ZERO, WHICH IS THE OTHER SIDE OF THE OLD SWAP. The
\ branching shape could not coalesce its four edge copies, because the two arms
\ handed the join the same two values the other way round and every copy was
\ made while both were still live. With no arms there are no such edges: the
\ selects write their answers straight into the registers the join's arguments
\ were given, and the copies the conversion's own edge asks for are all
\ coalescible. A copy left here would mean the allocator stopped merging a copy
\ whose ends do not interfere.
$FFE00C00 constant CSEL-SHAPE        \ the Csel form, register and condition fields cleared
$9A800000 constant CSEL-FORM
$FFE0FC1F constant CMP-SHAPE         \ the Cmp form with its two register fields cleared
$EB00001F constant CMP-FORM          \ Subs xzr, rn, rm - what a Cmp is

: CSELS ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ CSEL-SHAPE and CSEL-FORM = if 1+ then
   loop ;

\ The conditional SET, counted the same way: Csinc with both source registers
\ the zero register, which is what a Cset is and what the middle instruction of
\ a materialised Habu flag is. No fused form of this dialect emits one, so its
\ presence is what says a comparison answered a number rather than being folded
\ into the thing that read it.
$FFFF0FE0 constant CSET-SHAPE
$9A9F07E0 constant CSET-FORM

: CSETS ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ CSET-SHAPE and CSET-FORM = if 1+ then
   loop ;

\ And the conditional branch, counted the same way. Zero of them is the whole
\ claim this case makes about the shape: not "fewer branches" but none.
$FF000010 constant BCOND-SHAPE
$54000000 constant BCOND-FORM

: BCONDS ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ BCOND-SHAPE and BCOND-FORM = if 1+ then
   loop ;

: SRC3 ( -- ptr u8 n )
   s" : NCH-MAX ( n n -- n ) 2dup < if swap then drop ;" ;

: RECORD3 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC3 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELABORATE2 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 2 1 NELAB:COLON drop ;

: BRANCH-BODY ( IR-CTX:ctx -- n n n n n n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD3
   TAPE NTAPE:TOKENS
   VERDICT
   p r ELABORATE2
   CC BB TXT TEXT-LEN 0 REGS 2 1 NFIX:RUN-HABU
   A64EMIT:INSNS
   A64EMIT:BLOCKS
   0 A64EMIT:BLOCK-START@
   1 A64EMIT:BLOCK-START@
   3 A64EMIT:WORD@ CMP-SHAPE and
   CSELS
   BCONDS
   COPIES
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   9 -1 fn NRUN:ENTER2
   s" 3 4 NCH-MAX" EV-N
   s" 9 -1 NCH-MAX" EV-N ;

: BRANCH-CASE ( -- )
   s" a definition whose arms are single values compiles to a select and runs"
   T-LABEL
   NFIX:BINDING [: BRANCH-BODY ;] IR-CTX:WITH-CONTEXT
   9 T= 4 T= 9 T= 4 T=
   0 T= 0 T= 2 T= CMP-FORM T=
   7 T= 0 T= 2 T= 9 T= -1 T= 7 T= ;

\ ---- a definition that reads and writes memory -------------------------------
\ The same run over a body whose whole point is a side effect, and the first one
\ whose source names something outside the dialect's vocabulary: a `create`d data
\ word. Three things make this case the acceptance of the memory leaf rather than
\ another shape test.
\
\ FIRST, THE DATA WORD IS THE ENGINE'S. The cell is created by evaluating
\ `create NCH-CELL 1 cells allot` through the same front end the definition goes
\ through, and the word model is told nothing but its NAME - the address is the
\ engine's answer, asked for while the row is declared. Nothing in this file
\ writes an address down or reads one back, and everything else the case uses -
\ the spelling, the tape, the spans - comes off the engine's own compilation.
\
\ SECOND, THE CELL IS POISONED BEFORE EACH CALL. `CELL-BUMP`'s answer is its
\ argument plus one, which a routine that never touched memory could compute just
\ as well. So the cell is set to a value that is not the argument first: if the
\ store were dropped, the load would read the poison and the routine would answer
\ 100 instead of 42, and if the second store were dropped the cell would still
\ hold the poison afterwards. Both the answer and the cell are recorded.
\
\ THIRD, THE INTERPRETED WORD IS RUN FROM THE SAME POISONED STATE. The engine's
\ own compilation of the same source is put through the same two steps, so the
\ two paths are compared on the memory they left as well as on the value they
\ answered.
\
\ AND THE ARGUMENT IS BIGGER THAN A BYTE ON PURPOSE. The dialect has a cell
\ access and a byte access and the assembler has an encoder for each; a
\ cell-width access written with the byte-width encoder would keep only the low
\ eight bits, so the argument is 4000 and the answer 4001, which those bits
\ cannot carry. The case below this one holds the same line from the other end.
: SRC4 ( -- ptr u8 n )
   s" : NCH-BUMP ( n -- n ) NCH-CELL ! NCH-CELL @ 1+ dup NCH-CELL ! ;" ;

\ The word model this definition needs: the dialect's own vocabulary plus the one
\ data word the body names, committed to one row more than REGISTER-WORDS writes.
: MODEL-MEM ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" NCH-CELL" IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED
   p r ;

: RECORD4 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC4 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELABORATE4 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 1 1 NELAB:COLON drop ;

\ The poison the cell holds before each call, and the argument each call is
\ made with. All three of the poison, the argument and the answer differ, so no
\ two of the four recorded numbers can agree by accident, and the argument is
\ past the reach of a byte.
: POISON ( -- )
   s" 99 NCH-CELL !" EV ;

4001 constant BUMPED

: MEM-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   s" create NCH-CELL 1 cells allot" EV
   CC BB MODEL-MEM {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD4
   p r ELABORATE4
   CC BB TXT TEXT-LEN 0 REGS 1 1 NFIX:RUN-HABU
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   POISON
   4000 fn NRUN:ENTER1
   s" NCH-CELL @" EV-N
   POISON
   s" 4000 NCH-BUMP" EV-N
   s" NCH-CELL @" EV-N ;

: MEM-CASE ( -- )
   s" a definition that stores and loads compiles, runs and leaves the cell" T-LABEL
   NFIX:BINDING [: MEM-BODY ;] IR-CTX:WITH-CONTEXT
   BUMPED T= BUMPED T= BUMPED T= BUMPED T= 1 T= ;

\ ---- a definition with typed locals and a division ---------------------------
\ The same run over a body that binds three names with `{: … :}` and divides.
\ Nothing about the group is typed into this file as a tape row: `evaluate`
\ compiles the definition, the checker's own reader consumes `{:`, the three
\ `name:type` tokens and `:}`, and the elaborator reads that grid off the tape
\ the producer sealed. So this case is the acceptance of the locals leaf - if the
\ engine's reader carried the declaration some other way, the chain would refuse
\ here rather than compile something else.
\
\ WHY THESE ARGUMENTS AND NOT THE CORPUS'S. A locals frame's one job is to bind
\ the FIRST name to the DEEPEST value, and most argument triples cannot see a
\ binding that got that wrong: `10 20 50 LERP` answers 15 whether a and b are
\ swapped or not, because the interpolation is symmetric about its midpoint. So
\ the arguments here are chosen to make every permutation of the three bindings
\ answer a different number - 3, 17, 40 answers 8, and swapping any two names
\ answers 12, 9 or 40 - and the answer is compared with what the engine's own
\ compilation of the same source computes. Swapping the division's two operands
\ answers 3.
\
\ AND WHAT THE DIVISION IS. `hir.div` selects to `a64.sdiv`, which is three
\ instructions and not one: the divisor is tested, a `brk` is jumped over when it
\ is not zero, and only then does the divide run. That is exactly what the
\ engine's own `/` compiles to (src/habu/habu1.f BDIV0? then BDIV), so the
\ compiled routine and the interpreted word agree on a zero divisor as well as on
\ the rounding - both truncate toward zero, which the two negative-quotient cases
\ below pin.
: SRC5 ( -- ptr u8 n )
   s" : NCH-LERP ( n n n -- n ) {: a:n b:n t:n :} b a - t * 100 / a + ;" ;

: RECORD5 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC5 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELABORATE5 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 3 1 NELAB:COLON drop ;

\ Thirteen instructions: the pointer to where the body stands, three loads, the
\ subtraction, the multiply, the constant, the division's three, the addition,
\ the store and the return. There is no second pointer move: the routine leaves
\ one result, so the place the caller expects the pointer back IS the place the
\ body stands, and the publication is a distance of zero.
: LOCALS-BODY ( IR-CTX:ctx -- n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD5
   TAPE NTAPE:TOKENS
   VERDICT
   p r ELABORATE5
   CC BB TXT TEXT-LEN 0 REGS 3 1 NFIX:RUN-HABU
   A64EMIT:INSNS
   NRUN:PUBLISH {: fn:n :}
   3 17 40 fn NRUN:ENTER3
   0 100 25 fn NRUN:ENTER3
   -40 0 30 fn NRUN:ENTER3
   s" 3 17 40 NCH-LERP" EV-N
   s" -40 0 30 NCH-LERP" EV-N ;

\ The token count is the whole grid the reader produced: the name, `{:`, three
\ declarations, `:}`, and the nine body tokens. Asserting it is what says the
\ declaration reached the tape at all rather than being consumed before it.
\
\ The third argument triple divides a negative product: (0 - -40) * 30 is 1200
\ over 100, which is exact, so it is the SECOND number that pins the rounding -
\ -40 + 12 is -28 either way. What it really pins is the sign: a routine that
\ divided the other way round, or that mixed the bindings up, answers something
\ else, and the interpreted word is run on the same triple to say so.
: LOCALS-CASE ( -- )
   s" a definition with typed locals and a division compiles and runs" T-LABEL
   NFIX:BINDING [: LOCALS-BODY ;] IR-CTX:WITH-CONTEXT
   -28 T= 8 T= -28 T= 25 T= 8 T= 13 T= -1 T= 15 T= ;

\ ---- a definition that reads BYTES in a loop ---------------------------------
\ The same run over a body that walks a byte span: `c@` inside a counted loop,
\ which is where the two capabilities of this leaf meet. It is the acceptance of
\ both.
\
\ WHAT THE BYTE WIDTH HAS TO GET RIGHT, AND HOW THE ANSWER SAYS SO. `c@` reads
\ ONE byte. A load emitted at cell width would read eight bytes starting at the
\ same address, so every position of the scan would answer a number in the
\ millions instead of a character code, the sum would not be 416, and the last
\ positions would read past the end of the buffer altogether. Nothing about the
\ shape of the module would change - the same operation in the same place - so
\ this is the check that the WIDTH reached the encoder.
\
\ AND WHAT THE MEMORY ORDER HAS TO GET RIGHT. The load is inside the loop, so the
\ order it reads on the second turn is the one the first turn left, and that
\ order reaches the loop body as a block argument. Before this leaf the module
\ could not be built at all: the order was minted where the first memory word
\ stood, inside the body, and a value defined in a loop body does not dominate
\ the header that hands it back - the freeze verifier refused it by name. So a
\ green run here is the whole of the crossing: elaboration, the freeze verifier's
\ dominance rule, the allocation validator's per-path order rule, and the answer.
\
\ THE BUFFER IS THE ENGINE'S, LIKE THE CELL ABOVE. It is created by evaluating a
\ `BUFFER:` declaration through the same front end, filled one byte at a time
\ through the engine's own `c!`, and its address is read back by evaluating its
\ name. The four bytes are `habu`, whose codes add up to 416, and no two of them
\ are equal - so a scan that read one position twice, or that stopped one short,
\ answers something else.
: SRC6 ( -- ptr u8 n )
   s" : NCH-BSUM ( ptr u8 n -- n ) {: a:ptr u:n :} 0 u 0 ?do i a + c@ + loop ;" ;

: SRC7 ( -- ptr u8 n )
   s" : NCH-BFIND ( ptr u8 n n -- n ) {: a:ptr u:n c:n :} u 0 ?do i a + c@ c = if i unloop exit then loop -1 ;" ;

8 constant LOOP-REGS                 \ a loop's carried values each hold one

\ `habu`: 104, 97, 98, 117, which add up to 416.
416 constant HABU-SUM
98 constant LETTER-B                 \ the third byte
122 constant LETTER-Z                \ no byte of the buffer

: BUFFER-MAKE ( -- )
   s" 4 BUFFER: NCH-BUF" EV
   s" 104 NCH-BUF c!" EV
   s" 97 NCH-BUF 1 + c!" EV
   s" 98 NCH-BUF 2 + c!" EV
   s" 117 NCH-BUF 3 + c!" EV ;

: RECORD6 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC6 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: RECORD7 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC7 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELABORATE6 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 2 1 NELAB:COLON drop ;

: ELABORATE7 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 3 1 NELAB:COLON drop ;

\ The address is handed over as a number, which is what the routine reads it as:
\ a compiled word takes its arguments out of the caller's data-stack slots and
\ the slots hold cells. The interpreted word is called on the same buffer through
\ the engine, where the same two cells are a byte span.
: BSUM-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD6
   p r ELABORATE6
   CC BB TXT TEXT-LEN 0 LOOP-REGS 2 1 NFIX:RUN-HABU
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   s" NCH-BUF" EV-N {: buf:n :}
   buf 4 fn NRUN:ENTER2
   buf 0 fn NRUN:ENTER2
   s" NCH-BUF 4 NCH-BSUM" EV-N ;

: BSUM-CASE ( -- )
   \ SIX POSITIONS AND NOT SEVEN. One of the loop's blocks does nothing before
   \ its branch, so every branch that named it now names the far end and it is
   \ left out of the order entirely - see ORDER-BLOCKS. The answers are what this
   \ case is really about and they are unchanged.
   s" a definition that reads bytes in a loop compiles and runs" T-LABEL
   NFIX:BINDING [: BSUM-BODY ;] IR-CTX:WITH-CONTEXT
   HABU-SUM T= 0 T= HABU-SUM T= 6 T= ;

\ ---- a definition that leaves from the middle of a loop ----------------------
\ The same run over a scan that stops as soon as it finds the byte it wants.
\ `exit` inside a counted loop is a branch to the block the definition's one
\ return is in, handing it the value the word leaves - so the module still has
\ exactly one place control leaves through, which is what everything downstream
\ of the elaborator is written against.
\
\ THE TWO PINNED ARGUMENTS ARE THE TWO WAYS OUT. `b` is the third byte of the
\ buffer, so the early exit fires on the third turn and the answer is 2; `z` is
\ in no position, so the loop runs to its end and the answer is the -1 after it.
\ A branch to the wrong block cannot answer both: reaching the return early
\ answers 0 or -1 for the hit, and never reaching it answers 2 for the miss.
\ AND IT IS THE ONE FIXTURE IN THIS SUITE THAT CARRIES A SCHEMA TIE. The `-1` it
\ answers on a miss has all four halves set, so it is materialised as a move-wide
\ zero followed by three overwrites, and an overwrite names ONE register field
\ for its operand and its result - the schema says so, and every consumer has to
\ put the two values in one physical register. src/compiler/native/regalloc.f
\ states that as a union, so the four values of the chain are one class and one
\ register by construction. It used to be left to the scan, which does not
\ enforce it and only came out right because the operand of an overwrite dies at
\ the overwrite and the lowest free register was usually the one it had just
\ given up; coalescing fixes registers on purpose and broke that luck at once.
\ Removing the union puts this case back to E-A64RAV-TIE, which is the validator
\ refusing the whole routine rather than a wrong answer reaching the buffer.
: BFIND-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD7
   p r ELABORATE7
   CC BB TXT TEXT-LEN 0 LOOP-REGS 3 1 NFIX:RUN-HABU
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   s" NCH-BUF" EV-N {: buf:n :}
   buf 4 LETTER-B fn NRUN:ENTER3
   buf 4 LETTER-Z fn NRUN:ENTER3
   s" NCH-BUF 4 98 NCH-BFIND" EV-N ;

: BFIND-CASE ( -- )
   \ SIX POSITIONS AND NOT ELEVEN. A loop with an early exit builds the most
   \ do-nothing blocks of any shape here - the exit stub, the join, and the
   \ latch - and all of them are branched past and never written.
   s" a definition that leaves from the middle of a loop compiles and runs" T-LABEL
   NFIX:BINDING [: BFIND-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= -1 T= 2 T= 6 T= ;

\ ---- a definition that calls itself ------------------------------------------
\ The same run over the one shape that needs a call: `RECURSE`, which is a call
\ to the word being compiled and therefore the plain word-call-and-return shape
\ as well. Three things have to be right at once and the answer says so.
\
\ FIRST, THE RETURN ADDRESS. The routine reserves its own frame and puts x30 in
\ slot zero before it reads a single argument, and takes it back before it
\ returns. A call writes x30, so a routine that did not save it would return to
\ wherever the innermost call left off - the process would run away rather than
\ answer 3628800.
\
\ SECOND, THE CALLER'S OWN LIVE VALUE. `dup 1- RECURSE *` still needs `n` after
\ the call, and no register holds a value across a call to this routine: its
\ contract destroys exactly the pool the allocator hands out, and the recursive
\ instance writes it. So `n` crosses the call on the caller's data stack, below
\ the argument the callee reads, and comes back out of the slot it went into. A
\ routine that kept it in a register answers 10 rather than 3628800, because the
\ multiply would read whatever the deepest call left there.
\
\ THIRD, THE BRANCH ITSELF. The call's displacement is measured to block zero of
\ this routine - the prologue - so the callee arrives with its frame taken and
\ its arguments read. A displacement one instruction out lands on the link save
\ instead and the frame is never taken.
\
\ AND THE TWO PINNED ARGUMENTS ARE THE TWO WAYS THROUGH. Ten recurses ten deep;
\ one takes the base-case arm on the first turn and never calls at all, so the
\ prologue and the epilogue are exercised with no call between them. The
\ interpreted word is run on both, which is what says the two paths agree.
: SRC8 ( -- ptr u8 n )
   s" : NCH-FACT ( n -- n ) dup 1 <= if drop 1 exit then dup 1- RECURSE * ;" ;

: RECORD8 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC8 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

\ AND EVERY EDGE COPY IN IT IS GONE, which is the coalescing rule read the other
\ way from NCH-MAX. This routine's edges hand values on without permuting them,
\ so each copy's source dies at the copy and its two ends are never live at the
\ same instant. src/compiler/native/regalloc.f gives each pair one register and
\ src/compiler/native/emit.f writes nothing for a copy into the register it came
\ from, so the count below is zero: a value that crosses an edge unchanged costs
\ no instruction. A coalescer that stopped preferring, or an emitter that emitted
\ a copy from a register into itself, puts them back and this goes red while
\ every answer stays correct - which is the point of asserting it.
: FACT-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD8
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU-CALL
   A64EMIT:BLOCKS
   COPIES
   NRUN:PUBLISH {: fn:n :}
   10 fn NRUN:ENTER1
   1 fn NRUN:ENTER1
   s" 10 NCH-FACT" EV-N
   s" 1 NCH-FACT" EV-N ;

: FACT-CASE ( -- )
   \ THREE POSITIONS AND NOT FIVE. The two blocks the recursion's early exit
   \ leaves empty are branched past, which is also what closes the fall-through
   \ this routine used to miss: they were the zero-length blocks that stood
   \ between a block and its successor and made the branch over them survive.
   s" a definition that calls itself compiles and runs" T-LABEL
   NFIX:BINDING [: FACT-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 3628800 T= 1 T= 3628800 T= 0 T= 3 T= ;

\ ---- the same call, staged one function deeper -------------------------------
\ FACT-CASE's self-call written inside a quotation. `RECURSE` names the
\ DEFINITION and not the body the token stands in, at any depth: the engine's own
\ compilation branches to the open definition's entry (src/habu/habu2.f
\ J-RECURSE loads PEND), and the checker types the token as the definition's
\ declared effect however many `[:` are open around it. Both of those are right.
\
\ WHAT WAS WRONG WAS THE CHAIN, IN THREE PASSES THAT EACH SAID "THIS FUNCTION"
\ WHERE THE MODEL SAYS "FUNCTION ZERO". src/compiler/native/select.f SELF-SHAPE
\ took the call's shape off the arity of the function being SELECTED;
\ src/compiler/native/emit.f PUT-CALL branched to block zero of the function
\ being WRITTEN; and src/compiler/native/regalloc-verify.f VDNET-CK re-derived
\ the site's pointer movement from the net movement of the function being
\ VERIFIED. Inside function zero those are three names for one number and every
\ answer was right, which is why FACT-CASE above never saw any of it. Inside a
\ quotation's function they are three different numbers, and the routine the
\ chain published had the quotation calling ITSELF. The elaborator was right all
\ along: it stages the self-call with the DEFINITION's arity wherever the token
\ stands.
\
\ SO THE MISS BELONGS TO THE CODEGEN MODEL AND NOT TO THE CHECKER, and that is
\ why this pair is a differential and not a rejection: a quotation-depth guard on
\ the checker's CF-RECURSE would refuse a program the engine compiles, runs and
\ answers correctly.
\
\ THE TWO CASES ARE THE TWO WAYS IT CAME OUT. When the body's window and the
\ definition's arity AGREE nothing refused anything - the three numbers still
\ matched each other, they just all named the quotation - so the published
\ routine recursed inside the quotation, bottomed out at zero and ran the
\ definition's tail exactly once: a flat 100 at every depth where the word
\ answers 100, 200, 300, 400, at exit zero, with no diagnostic anywhere. When
\ they DISAGREE the chain refused, but with E-A64SEL-CALL, which lib/errors.f
\ documents as unreachable from any contract src/compiler/native/abi.f builds -
\ "a defect in the chain rather than a program it cannot compile". So the second
\ case pins a refusal that was a self-diagnosis rather than a verdict, and both
\ of them now answer what the interpreted word answers.
\
\ AND THE ENTRY IS THE MIGRATION RATHER THAN THIS SUITE'S STAGE FIXTURE. A
\ quotation makes the emission multi-function and turns its `catch` into a call
\ to a routine of the engine's, which needs a placement declared before emission
\ to measure its branch from; the stage fixture declares none and answers
\ E-A64EMIT-PLACE, so these two go through NMIGRATE:DEFINE - the production
\ entry the other quotation suites drive.

\ The two the engine compiles, which are the spec.
: NCH-QWALK ( n -- n )
   [: dup 0 > if 1- RECURSE then ;] catch drop 100 + ;

: NCH-QWIDE ( n -- n n )
   [: dup 0 > if 1- RECURSE drop 10 + then ;] catch ;

\ And the same two texts, character for character but for the suffix on each
\ name, put through the chain. They are published as the file loads, because a
\ chain that cannot compile them at all has to say so where it happens.
: QWALK-DEF ( -- )
   s" : NCH-QWALK-N ( n -- n ) [: dup 0 > if 1- RECURSE then ;] catch drop 100 + ;"
   NMIGRATE:DEFINE ;

\ The one whose quotation window is not the definition's arity: the body takes
\ and leaves one cell, the definition leaves two, and the self-call inside the
\ body is the site where those two counts meet.
: QWIDE-DEF ( -- )
   s" : NCH-QWIDE-N ( n -- n n ) [: dup 0 > if 1- RECURSE drop 10 + then ;] catch ;"
   NMIGRATE:DEFINE ;

QWALK-DEF
QWIDE-DEF

\ THE ANSWERS ARE BOUND BEFORE EITHER IS COMPARED for the pair that leaves two
\ cells: a catch site publishes its window AND its code, so four cells over two
\ bare comparators would hold each call's answer against its own.
: QWALK= ( n -- ) {: v:n :}
   v NCH-QWALK  v NCH-QWALK-N  T= ;

: QWIDE= ( n -- ) {: v:n :}
   v NCH-QWIDE   v NCH-QWIDE-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

\ FIVE DEPTHS AND NOT ONE. Zero never enters the recursion at all, so it is the
\ depth a quotation calling itself gets RIGHT; every depth above it is one the
\ two targets disagree about, and the wrong one answers the same number at all
\ of them.
: QWALK-CASE ( -- )
   s" a definition that calls itself from inside a quotation runs" T-LABEL
   0 QWALK=  1 QWALK=  2 QWALK=  3 QWALK=  7 QWALK= ;

: QWIDE-CASE ( -- )
   s" the same call where the body's window is not the definition's arity"
   T-LABEL
   0 QWIDE=  1 QWIDE=  2 QWIDE=  3 QWIDE= ;

\ ---- a definition that does not fit in its registers -------------------------
\ The same run over a routine of more than one block whose values do not all fit
\ in the pool it is given. Three locals are read on both sides of a branch and
\ three registers also have to hold what the body computes, so the allocator puts
\ the locals in the routine's own frame - and each store it plans belongs in the
\ block that COMPUTES the value while each load belongs in the block that reads
\ it back, which is what a plan row naming its block is for. Before that landed
\ this definition was refused outright, whatever frame it declared.
\
\ WHY THE LOCALS ARE THE VALUES THAT SPILL AND THE SUM IS NOT. A value the
\ compile-time vector holds at an `if` is handed to the join as a block argument,
\ so the sum and the arm's answer are one class of several values across an edge
\ - and a class of more than one value would write one frame slot more than once,
\ which the validator refuses by name. A LOCAL is not on that vector: it is read
\ in the join by dominance, so it is a class of one and may go into a slot.
\
\ WHAT IT PROVES BEYOND COMPILING. The answer is compared with the interpreted
\ word on two inputs, so a store or a load placed in the wrong block gives a
\ different number rather than a different instruction count - and the spill
\ count is asserted too, because a run that produced the right answer without
\ spilling anything would be measuring nothing at all.
: SRC12 ( -- ptr u8 n )
   s" : NCH-SPILL ( n n n -- n ) {: a:n b:n c:n :} a b + c + 0 a < if 1+ then a + b + c + ;" ;

3 constant TIGHT-REGS                \ fewer than the definition below needs
3 constant TIGHT-SLOTS               \ frame slots declared for the ones that do not fit

: RECORD12 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC12 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELABORATE12 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 3 1 NELAB:COLON drop ;

: SPILL-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD12
   p r ELABORATE12
   CC BB TXT TEXT-LEN 0 TIGHT-REGS 3 1 TIGHT-SLOTS NFIX:RUN-HABU-SPILL
   NFIX:SPILLED
   NRUN:PUBLISH {: fn:n :}
   2 3 4 fn NRUN:ENTER3
   s" 2 3 4 NCH-SPILL" EV-N ;

: SPILL-CASE ( -- )
   s" a definition whose values do not fit in its registers spills and runs"
   T-LABEL
   NFIX:BINDING [: SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   19 T= 19 T= 3 T= ;

\ ---- a definition that calls itself AND does not fit -------------------------
\ The one shape that exercises both owners of a routine's frame at once. A
\ calling routine already keeps its caller's return address in the bottom slot of
\ its own frame, and here the register allocator has to put a value away as well
\ - so the two claims are on one frame and the layout in
\ src/compiler/native/frame.f is what keeps them apart: the link slot first, the
\ allocator's slots above it, both derived from the contract's own trait. A
\ spilled value on top of the return address returns to its own data rather than
\ to its caller, and this case is what would find that.
\ WHY NO LITERAL REPEATS IN THE ENTRY BLOCK. What this case pins is the frame
\ layout - one spilled value sitting above the link slot - so its body has to
\ need exactly one spill for a reason that is about REGISTERS, not about how many
\ times a number is written. The body used to open `a 1 + a 2 + a 3 + a 4 +` and
\ then guard with `1 a <`, which put the literal 1 in this block twice; block-
\ local literal CSE (src/compiler/native/elaborate.f EMIT-LIT) collapses such a
\ pair into one value whose live range then spans the whole add chain, and in a
\ frame this deliberately starved that turns one spilled value into two. The pin
\ would have had to move to admit it, which would have thrown away the very fact
\ it exists to hold. So the literals in this block are distinct instead: 5, 2, 3,
\ 4 and the guard's 1 are five different numbers, no memo can fold any two of
\ them, and the case's pressure character is the same whether the CSE runs or
\ not.
\ THE `1` IN `a 1-` IS A REPEAT OF THE GUARD'S, AND IT IS NOW FOLDED. The memo
\ crosses into the THEN block, because the branch above dominates it - see the
\ memo's header in src/compiler/native/elaborate.f - so the two ones are one
\ value whose live range spans the branch. Measured, the frame still spills
\ exactly one, and for a reason rather than by luck: the guard's constant is
\ minted AFTER the add chain has been folded down to a single value, so carrying
\ it into the arm lengthens a range that overlaps nothing which was live at the
\ pressure peak. What this case pins is that peak, and the peak is in the add
\ chain, which is why the five distinct numbers above are still what the body
\ needs.
: SRC13 ( -- ptr u8 n )
   s" : NCH-RSPILL ( n -- n ) {: a:n :} a 5 + a 2 + a 3 + a 4 + + + + 1 a < if a 1- RECURSE + then ;" ;

1 constant RSPILL-SLOTS              \ one slot above the link slot
4 constant RSPILL-REGS               \ fewer than the definition above needs

: RECORD13 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC13 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: RSPILL-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD13
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 RSPILL-REGS 1 1 RSPILL-SLOTS NFIX:RUN-HABU-CALL-SPILL
   NFIX:SPILLED
   NRUN:PUBLISH {: fn:n :}
   1 fn NRUN:ENTER1
   3 fn NRUN:ENTER1
   s" 3 NCH-RSPILL" EV-N ;

: RSPILL-CASE ( -- )
   s" a definition that calls itself and does not fit spills above the link slot"
   T-LABEL
   NFIX:BINDING [: RSPILL-BODY ;] IR-CTX:WITH-CONTEXT
   66 T= 66 T= 18 T= 1 T= ;

\ ---- the two comparisons that must NOT fuse ----------------------------------
\ Fusing a comparison into the branch below it is only legal when the branch is
\ the comparison's ONLY reader. These two cases are the other side of that rule,
\ run all the way through and executed, because a fusion that ignored the rule
\ would produce a routine that computes something else rather than a module that
\ fails a shape assertion.
\
\ THE FIRST IS A COMPARISON WITH NO BRANCH UNDER IT AT ALL: what the word leaves
\ IS the flag, so the flag has to be materialised as the number a Habu flag is.
\ Nine instructions - the pointer down over the two arguments, two loads, the
\ comparison's three, the store, the pointer up, the return - which is one more
\ than a fused pair would be and two more than the branchless part of it.
: SRC10 ( -- ptr u8 n )
   s" : NCH-ISLT ( n n -- bool ) < ;" ;

: RECORD10 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC10 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELABORATE10 ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 2 1 NELAB:COLON drop ;

: ISLT-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD10
   p r ELABORATE10
   CC BB TXT TEXT-LEN 0 REGS 2 1 NFIX:RUN-HABU
   A64EMIT:INSNS
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   9 -1 fn NRUN:ENTER2
   s" 3 4 NCH-ISLT" EV-N
   s" 9 -1 NCH-ISLT" EV-N ;

: ISLT-CASE ( -- )
   s" a comparison whose answer is the word's result keeps its flag and runs"
   T-LABEL
   NFIX:BINDING [: ISLT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= -1 T= 0 T= -1 T= 1 T= 8 T= ;

\ THE SECOND IS A COMPARISON THAT IS BOTH BRANCHED ON AND KEPT. `< dup if then`
\ is the smallest checked source there is for it: the comparison answers one
\ flag, `dup` puts a second reference to that same SSA value on the compile-time
\ vector, the `if` tests one of them, and the other is what the word leaves - so
\ the value has three readers, the two-way branch and the two edges that carry it
\ to the join. The empty arm is the point rather than an oversight: nothing else
\ is needed to give the flag a second reader, and adding anything would only make
\ the fixture harder to read.
\
\ WHAT IT ASSERTS, AND WHAT SAYS THE FLAG WAS MATERIALISED. Both arms of this
\ `if` are empty, so both hand the join the same value and the selection is
\ converted with no select emitted at all - there is nothing to choose between.
\ What is left is the comparison itself, and the question this case exists for
\ is whether it was materialised or folded into something. One Cset says it was:
\ a64.flag is a Cmp, a Cset and a negation, and the Cset is the instruction no
\ fused form of this dialect emits. A fusion that ignored the use count would
\ put the comparison INTO the branch or the select instead, leaving no Cset and
\ the value the two edges carry defined by nothing at all.
\
\ AND NO CONDITIONAL BRANCH AND NO SELECT, which is the conversion. The two-way
\ branch and both stubs are gone, so eight instructions and two blocks where the
\ branching shape was fourteen and four.
: SRC11 ( -- ptr u8 n )
   s" : NCH-LTKEEP ( n n -- bool ) < dup if then ;" ;

$FF000000 constant BRANCH-KIND       \ the byte that says which branch form this is
$B4000000 constant CBZ-KIND          \ Cbz - the unfused two-way branch

: RECORD11 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC11 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: LTKEEP-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD11
   p r ELABORATE10
   CC BB TXT TEXT-LEN 0 REGS 2 1 NFIX:RUN-HABU
   A64EMIT:INSNS
   A64EMIT:BLOCKS
   BCONDS
   CSELS
   CSETS
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   9 -1 fn NRUN:ENTER2
   s" 3 4 NCH-LTKEEP" EV-N
   s" 9 -1 NCH-LTKEEP" EV-N ;

: LTKEEP-CASE ( -- )
   s" a comparison that is branched on and kept keeps its flag and runs" T-LABEL
   NFIX:BINDING [: LTKEEP-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= -1 T= 0 T= -1 T=
   1 T= 0 T= 0 T= 2 T= 8 T= ;

\ ---- the loop the corpus actually writes -------------------------------------
\ `begin … while … repeat` outnumbers `begin … until` nine to one in real Habu,
\ and lib/string.f's COUNT-CHAR is the shape it is usually written in: a byte
\ scan carrying an accumulator and an index round the loop, with an `if` inside
\ the body that adds to the accumulator without changing how deep the stack is.
\ It is compiled here from the same source text the library holds, with the
\ locals annotated, and run against the interpreted word on the same span.
\
\ THE FOUR ARGUMENTS ARE THE FOUR THINGS A LOOP LIKE THIS CAN GET WRONG. Five
\ bytes with three matches says the accumulator survives every turn and is added
\ to only on a match; a length of ZERO says the loop runs no turns at all, which
\ is a `while` whose test fails the first time it is asked and is the boundary an
\ `until` loop cannot even express; a length of ONE says the single turn runs and
\ the back edge is taken exactly once; and a byte that is in no position says the
\ loop runs every turn with the body's `if` never taken. A polarity that is the
\ wrong way round cannot answer the first and the third the same way the
\ interpreted word does - it either runs no turns and answers zero, or never
\ leaves at all.
: SRC15 ( -- ptr u8 n )
   s" : NCH-COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u:n c:n :} 0 0 begin dup u < while dup a + c@ c = if swap 1+ swap then 1+ repeat drop ;" ;

\ `aabca`: five bytes with the letter a in three of the five positions and no
\ two neighbouring positions alike after the first pair, so a scan that read one
\ position twice or stopped one short answers something other than three.
97 constant LETTER-A

\ This scan carries more at once than the counted-loop scans above it: three
\ locals that live to the end, the accumulator and the index round the loop, and
\ the byte and the flag inside the body. The pool is stated large enough for all
\ of them, because what this case is about is the loop's shape and not how
\ tightly it packs registers - the routine that does have to spill is
\ SPILL-CASE, which asks for a frame on purpose.
12 constant SCAN-REGS

: BUFFER-MAKE2 ( -- )
   s" 5 BUFFER: NCH-BUF2" EV
   s" 97 NCH-BUF2 c!" EV
   s" 97 NCH-BUF2 1 + c!" EV
   s" 98 NCH-BUF2 2 + c!" EV
   s" 99 NCH-BUF2 3 + c!" EV
   s" 97 NCH-BUF2 4 + c!" EV ;

: RECORD15 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC15 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: COUNTCHAR-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD15
   p r ELABORATE7
   CC BB TXT TEXT-LEN 0 SCAN-REGS 3 1 NFIX:RUN-HABU
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   s" NCH-BUF2" EV-N {: buf:n :}
   buf 5 LETTER-A fn NRUN:ENTER3
   buf 0 LETTER-A fn NRUN:ENTER3
   buf 1 LETTER-A fn NRUN:ENTER3
   buf 5 LETTER-Z fn NRUN:ENTER3
   s" NCH-BUF2 5 97 NCH-COUNT-CHAR" EV-N ;

\ Six blocks: the entry, the loop header, the stub the `while` leaves through,
\ the body - which is now one block, because the `if` inside it adds to the
\ accumulator without branching and is converted whole - and the block after the
\ loop. The body's `if` used to be three blocks of its own: the stub it skipped
\ its arm through, the arm, and the join that was also the latch. The LOOP is
\ still a loop: its back edge goes to a lower ordinal, which is exactly what the
\ conversion refuses to speculate over.
: COUNTCHAR-CASE ( -- )
   s" the corpus's while-repeat byte scan compiles and runs" T-LABEL
   NFIX:BINDING [: COUNTCHAR-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 0 T= 1 T= 0 T= 3 T= 6 T= ;

\ ---- a while's test is a comparison, and it fuses ----------------------------
\ The test of a `while` is a comparison standing immediately above the two-way
\ branch that reads it, with no other reader - which is exactly the shape
\ src/compiler/native/select.f fuses into one compare-and-branch. Nothing was
\ added there for `while`: the rule is stated over the block's last two
\ operations and the value's use count, so a loop test meets it for the same
\ reason an `if`'s test does. This case is what says the rule really does reach
\ the new construction rather than being assumed to.
\
\ IT IS COUNTED BY FORM RATHER THAN AT AN INDEX. This loop has exactly one
\ conditional branch in it. Fused, that branch is a B.cond after a Cmp; unfused,
\ the comparison would be materialised into a register and the branch would be a
\ Cbz over it. So one B.cond and no Cbz is the fusion, and a fusion that stopped
\ reaching a `while` reverses both numbers - while the answers below stay
\ correct either way, which is why the counts are asserted at all.
: SRC18 ( -- ptr u8 n )
   s" : NCH-WDOWN ( n -- n ) begin dup 0 > while 1- repeat ;" ;

$54000000 constant BCOND-KIND        \ B.cond - the conditional half of a fused compare-and-branch

: FORM-COUNT ( n -- n )
   {: want:n :}
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ BRANCH-KIND and want = if 1+ then
   loop ;

: RECORD18 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC18 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: WFUSE-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD18
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU
   BCOND-KIND FORM-COUNT
   CBZ-KIND FORM-COUNT
   NRUN:PUBLISH {: fn:n :}
   5 fn NRUN:ENTER1
   0 fn NRUN:ENTER1
   s" 5 NCH-WDOWN" EV-N ;

: WFUSE-CASE ( -- )
   s" a while's comparison fuses into the branch that tests it" T-LABEL
   NFIX:BINDING [: WFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= 0 T= 1 T= ;

\ ---- the smallest `else` there is, and one with two values in each arm --------
\ maki's MAX-DIM is `a b > if a else b then` and nothing else, which is the whole
\ of what an `else` adds: the false path runs a second arm instead of falling
\ into the join, and BOTH arms leave a value where the structure opened with
\ none. The four arguments run each arm at least once and take the equal case,
\ which is the one a `>` written as a `>=` would answer differently.
: SRC16 ( -- ptr u8 n )
   s" : NCH-MAX-DIM ( n n -- n ) {: a:n b:n :} a b > if a else b then ;" ;

: RECORD16 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC16 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: MAXDIM-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD16
   p r ELABORATE10
   CC BB TXT TEXT-LEN 0 REGS 2 1 NFIX:RUN-HABU
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   3 7 fn NRUN:ENTER2
   7 3 fn NRUN:ENTER2
   5 5 fn NRUN:ENTER2
   -4 -9 fn NRUN:ENTER2
   s" 3 7 NCH-MAX-DIM" EV-N ;

: MAXDIM-CASE ( -- )
   s" the minimal else compiles and runs both arms" T-LABEL
   NFIX:BINDING [: MAXDIM-BODY ;] IR-CTX:WITH-CONTEXT
   7 T= -4 T= 5 T= 7 T= 7 T= 2 T= ;

\ The same structure with two values in each arm and different work in each, so
\ the join is two wide and neither arm can be mistaken for the other. `swap`
\ leaves the two the caller gave in the other order and `nip dup` leaves the
\ second one twice, and the sum after the join tells the two apart: the larger
\ pair adds to the same total either way ONLY if the arms were confused.
: SRC17 ( -- ptr u8 n )
   s" : NCH-SUMMAX ( n n -- n ) 2dup > if swap else nip dup then + ;" ;

: RECORD17 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC17 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: SUMMAX-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD17
   p r ELABORATE10
   CC BB TXT TEXT-LEN 0 REGS 2 1 NFIX:RUN-HABU
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   9 2 fn NRUN:ENTER2
   3 7 fn NRUN:ENTER2
   s" 9 2 NCH-SUMMAX" EV-N
   s" 3 7 NCH-SUMMAX" EV-N ;

: SUMMAX-CASE ( -- )
   s" an else whose arms both leave two values compiles and runs" T-LABEL
   NFIX:BINDING [: SUMMAX-BODY ;] IR-CTX:WITH-CONTEXT
   14 T= 11 T= 14 T= 11 T= 2 T= ;

\ ---- two structures one after the other, in one definition -------------------
\ A control frame is a slot the walk reuses: the second structure of a body sits
\ at the same depth the first one did, so it reads the same frame. What each of
\ these two definitions proves is that it does NOT read what the first structure
\ left there.
\
\ THE FIRST IS A `while` LOOP FOLLOWED BY AN `until` LOOP. A loop that a `while`
\ left may not be closed by `until` - the values the `while` handed over would
\ arrive nowhere - so the second loop is refused outright if the first loop's
\ count of `while`s is still in the frame. The two arguments run the first loop
\ round and skip it entirely, and the second loop answers differently either way.
: SRC19 ( -- ptr u8 n )
   s" : NCH-TWOLOOP ( n -- n ) begin dup 0 > while 1- repeat begin 1+ dup 0 >= until ;" ;

: RECORD19 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC19 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: TWOLOOP-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD19
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU
   NRUN:PUBLISH {: fn:n :}
   5 fn NRUN:ENTER1
   -2 fn NRUN:ENTER1
   s" 5 NCH-TWOLOOP" EV-N
   s" -2 NCH-TWOLOOP" EV-N ;

: TWOLOOP-CASE ( -- )
   s" a while loop and an until loop in one definition compile and run" T-LABEL
   NFIX:BINDING [: TWOLOOP-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 0 T= 1 T= ;

\ THE SECOND IS AN `if` WITH AN `else` FOLLOWED BY AN `if` WITHOUT ONE. The join
\ of a two-armed structure is as wide as its arms left the stack and the join of
\ a one-armed structure is as wide as the structure opened; a frame still holding
\ the first structure's arm leaves the second one measuring itself against the
\ wrong number, which the join-width rule then refuses. The three arguments run
\ the first structure's two arms and the second structure's one.
: SRC20 ( -- ptr u8 n )
   s" : NCH-TWOIF ( n n -- n ) 2dup > if drop else nip then dup 0 < if 0 swap - then ;" ;

: RECORD20 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC20 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: TWOIF-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD20
   p r ELABORATE10
   CC BB TXT TEXT-LEN 0 LOOP-REGS 2 1 NFIX:RUN-HABU
   NRUN:PUBLISH {: fn:n :}
   9 2 fn NRUN:ENTER2
   3 7 fn NRUN:ENTER2
   -8 -3 fn NRUN:ENTER2
   s" -8 -3 NCH-TWOIF" EV-N ;

: TWOIF-CASE ( -- )
   s" a two-armed if and a one-armed if in one definition compile and run" T-LABEL
   NFIX:BINDING [: TWOIF-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 3 T= 7 T= 9 T= ;

\ ---- one structure inside another --------------------------------------------
\ A `while` loop inside a `while` loop, which is two frames of the control stack
\ open at once with a `while` in each. The inner loop burns a copy of the outer
\ counter down and the outer one counts itself down, so the answer says the two
\ loops each went round their own number of times and each left through its own
\ block: an inner loop that left through the OUTER loop's exit would leave the
\ word after one turn.
: SRC21 ( -- ptr u8 n )
   s" : NCH-NESTW ( n -- n ) begin dup 0 > while dup begin dup 0 > while 1- repeat drop 1- repeat ;" ;

: RECORD21 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC21 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: NESTW-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD21
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU
   NRUN:PUBLISH {: fn:n :}
   3 fn NRUN:ENTER1
   -2 fn NRUN:ENTER1
   s" 3 NCH-NESTW" EV-N
   s" -2 NCH-NESTW" EV-N ;

: NESTW-CASE ( -- )
   s" a while loop inside a while loop compiles and runs" T-LABEL
   NFIX:BINDING [: NESTW-BODY ;] IR-CTX:WITH-CONTEXT
   -2 T= 0 T= -2 T= 0 T= ;

\ An `else` arm that leaves the word from inside itself. The join then has ONE
\ path into it - the first arm's - and its width is the one that arm left, which
\ is the rule an `else` records rather than a special case for `exit`. The other
\ order, an `exit` in the FIRST arm with an `else` after it, is refused by name
\ and carried by dot habu-let-exit-stand-d74f14ec.
: SRC22 ( -- ptr u8 n )
   s" : NCH-ELSEXIT ( n -- n ) dup 0 < if drop 0 else drop 7 exit then ;" ;

: RECORD22 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC22 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: ELSEXIT-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD22
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU
   NRUN:PUBLISH {: fn:n :}
   -3 fn NRUN:ENTER1
   5 fn NRUN:ENTER1
   s" -3 NCH-ELSEXIT" EV-N
   s" 5 NCH-ELSEXIT" EV-N ;

: ELSEXIT-CASE ( -- )
   s" an else arm that leaves the word compiles and runs" T-LABEL
   NFIX:BINDING [: ELSEXIT-BODY ;] IR-CTX:WITH-CONTEXT
   7 T= 0 T= 7 T= 0 T= ;

\ ---- the contract and the body have to agree about calling -------------------
\ Whether a routine calls is the contract's declaration, and the selector builds
\ the frame and the link save from it. Two ways for that declaration to be wrong,
\ and both are refused before a byte is emitted rather than discovered by a
\ routine that returns to the wrong place.
\
\ A BODY THAT CALLS UNDER A CONTRACT THAT DOES NOT SAY SO would get no frame and
\ no link save, so the first call would destroy the caller's return address. The
\ same recursive definition is put through the leaf contract to prove it stops.
\
\ The other direction - a contract that says so over a body that does not call,
\ which would reserve a frame and save a return address for nothing - is the same
\ refusal from the other side and is measured in test/compiler/native-select.f,
\ which has a context to spare for it. A refused case abandons a context holding
\ two modules, so this suite can afford exactly one.
: SRC9 ( -- ptr u8 n )
   s" : NCH-FACT2 ( n -- n ) dup 1 <= if drop 1 exit then dup 1- RECURSE * ;" ;

: RECORD9 ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC9 EV
   NFEED:END-UNIT  R-VERDICT !  0 R-TAPE ! ;

: LEAF-FACT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD9
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU ;

: LEAF-FACT ( -- )
   NFIX:BINDING [: LEAF-FACT-BODY ;] IR-CTX:WITH-CONTEXT ;

: AGREE-CASES ( -- )
   s" a body that calls under a contract that does not is refused" T-LABEL
   [: LEAF-FACT ;] E-A64SEL-CALL TTHROWSQ ;

\ ---- CG-13's crash shape is unrepresentable ----------------------------------
\ The reviewed defect: a select -> allocate -> emit -> republish probe whose pool
\ was based at x20 replaced the engine's DATA/RBASE value and the process died
\ 134. The production convention builder is the only way the chain states a
\ pool, so the regression is that NABI cannot build such a contract at all: the
\ refusal fires at contract construction, with A64EFF's own code, before any
\ module, selection or emission exists. Three doors: the exact crash register,
\ a pool RANGE that merely straddles the engine's upper block (x26-x28), and
\ the whole framed-leaf entry the chain's cases go through.
: DROP-ROUTINE ( A64EFF:routine -- )
   A64EFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop drop ;

: POOL-CASES ( -- )
   s" a pool naming an engine register is refused before selection" T-LABEL
   [: 20 4 NABI:POOL A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 24 4 NABI:POOL A64EFF:GPRS-N drop ;] E-A64EFF-GPR TTHROWSQ
   [: 20 4 NABI:POOL 1 1 0 NABI:LEAF-FRAMED DROP-ROUTINE ;] E-A64EFF-GPR TTHROWSQ ;

\ ---- what the no-return form declares, against the calling form it is not -----
\ The two forms describe routines that make the same instruction - an ordinary
\ Bl - and differ in what happens after it. Reading the fields side by side is
\ what says the difference is one capability and not a second convention: the
\ trait is the same, the arity is the same, and the caller's return address and
\ the frame are what change. A form that quietly kept the slot would be caught
\ here rather than as bytes nobody could account for.
\
\ EACH CONTRACT IS BUILT AT EVERY READ because a routine is thirteen cells and a
\ value of more than one cell cannot be bound to a typed local - the same reason
\ src/compiler/native/abi.f states its own contract more than once.
: NR0 ( -- A64EFF:routine )   0 4 NABI:POOL 1 0 0 NABI:NORET-FRAMED ;
: CF0 ( -- A64EFF:routine )   0 4 NABI:POOL 1 0 0 NABI:CALL-FRAMED ;
: NR4 ( -- A64EFF:routine )   0 4 NABI:POOL 1 0 4 NABI:NORET-FRAMED ;
: CF4 ( -- A64EFF:routine )   0 4 NABI:POOL 1 0 4 NABI:CALL-FRAMED ;

\ A contract of one control value and one link value, for the pair of questions
\ LINK-SPLIT-CASE below asks about every way control can leave.
: R-LINK ( A64EFF:control A64EFF:link -- A64EFF:routine )
   {: c:A64EFF:control l:A64EFF:link :}
   A64EFF-CONV:DSTACK A64EFF:SEQ-NONE A64EFF:SEQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED l c
   A64EFF:T-CALL 0 0 A64EFF:ROUTINE ;

: NORET-TRAIT-CASE ( -- )
   s" both forms declare the direct call, because both really call" T-LABEL
   NR0 A64EFF:TRAITS@ A64EFF:T-CALL A64EFF:TRAITS-HAS? TTRUE
   CF0 A64EFF:TRAITS@ A64EFF:T-CALL A64EFF:TRAITS-HAS? TTRUE ;

: NORET-FIELD-CASE ( -- )
   s" the calling form keeps the caller's return address and a slot for it"
   T-LABEL
   CF0 A64EFF:LINK@ A64EFF-LINK:PRESERVED A64EFF-LINK:EQ TTRUE
   CF0 A64EFF:FRAME@ A64EFF:SP-ALIGN T=
   CF0 A64EFF:CONTROL@ A64EFF-CONTROL:RETURNS A64EFF-CONTROL:EQ TTRUE
   CF0 A64EFF:RETURNS? TTRUE

   s" the no-return form declares it destroyed and owns no frame at all" T-LABEL
   NR0 A64EFF:LINK@ A64EFF-LINK:CLOBBERED A64EFF-LINK:EQ TTRUE
   NR0 A64EFF:FRAME@ 0 T=
   NR0 A64EFF:DELTA@ 0 T=
   NR0 A64EFF:CONTROL@ A64EFF-CONTROL:NO-RETURN A64EFF-CONTROL:EQ TTRUE
   NR0 A64EFF:RETURNS? TFALSE ;

\ The layout the two fields decide, read out of the file that owns it. It is the
\ same pair of questions every pass asks - the selector to decide whether to
\ build the save, the allocator to decide where its slots start - so a form whose
\ two fields disagreed with its declared frame would show up as one of these.
: NORET-LAYOUT-CASE ( -- )
   s" the layout keeps a slot for the calling form and none for the other"
   T-LABEL
   NR0 A64EFF:TRAITS@ NR0 A64EFF:LINK@ A64FRAME:LINK-KEPT? TFALSE
   CF0 A64EFF:TRAITS@ CF0 A64EFF:LINK@ A64FRAME:LINK-KEPT? TTRUE
   NR0 A64EFF:TRAITS@ NR0 A64EFF:LINK@ A64FRAME:SPILL-BASE 0 T=
   CF0 A64EFF:TRAITS@ CF0 A64EFF:LINK@ A64FRAME:SPILL-BASE
      A64FRAME:LINK-SLOT A64IR:SLOT-WIDTH + T= ;

\ ---- why the split cannot lose a return address ------------------------------
\ The link save used to be built from the trait alone, and it is now built from
\ the trait AND the link declaration together. What makes that safe is not this
\ file's care: it is A64EFF:LINK-CK, which refuses `link clobbered` for every
\ routine control comes back from. So for a routine that RETURNS or that leaves
\ through a callee there is exactly one link value a contract can carry, and
\ LINK-KEPT? over it is the trait and nothing else - the split can only ever drop
\ the save from a routine no return address is read in.
\
\ THE THREE DOORS ARE THE THREE CONTROL VALUES, so a variant added to that family
\ without an answer here would leave a routine whose link rule nobody stated.
: LINK-SPLIT-CASE ( -- )
   s" a routine control comes back from cannot declare the address destroyed"
   T-LABEL
   [: A64EFF-CONTROL:RETURNS A64EFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-A64EFF-LINK TTHROWSQ
   [: A64EFF-CONTROL:TAIL-CALL A64EFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-A64EFF-LINK TTHROWSQ

   s" so a calling routine that returns always keeps a slot for it" T-LABEL
   A64EFF-CONTROL:RETURNS A64EFF-LINK:PRESERVED R-LINK
      A64EFF:TRAITS@ A64EFF-LINK:PRESERVED A64FRAME:LINK-KEPT? TTRUE
   A64EFF-CONTROL:TAIL-CALL A64EFF-LINK:PRESERVED R-LINK
      A64EFF:TRAITS@ A64EFF-LINK:PRESERVED A64FRAME:LINK-KEPT? TTRUE

   s" and only one that never comes back may say otherwise" T-LABEL
   A64EFF-CONTROL:NO-RETURN A64EFF-LINK:CLOBBERED R-LINK
      A64EFF:TRAITS@ A64EFF-LINK:CLOBBERED A64FRAME:LINK-KEPT? TFALSE ;

\ And a spilling one, where the slot the prologue owns is the whole difference:
\ four values fit a no-return routine's frame and push a calling routine's over
\ the next alignment step. Where the no-return one leaves the machine stack
\ pointer is its whole frame below where it was entered, because nothing gives it
\ back and nothing needs it back.
: NORET-SPILL-CASE ( -- )
   s" four spill slots fit one form's frame and not the other's" T-LABEL
   NR4 A64EFF:FRAME@  4 A64IR:SLOT-WIDTH *  A64EFF:FRAME-ROUND  T=
   CF4 A64EFF:FRAME@  5 A64IR:SLOT-WIDTH *  A64EFF:FRAME-ROUND  T=
   NR4 A64EFF:FRAME@  CF4 A64EFF:FRAME@  T<>

   s" and the pointer is declared where a routine that never returns leaves it"
   T-LABEL
   NR4 A64EFF:DELTA@  NR4 A64EFF:FRAME@ negate  T=
   CF4 A64EFF:DELTA@ 0 T= ;

public

: RUN ( -- )
   T-RESET
   CHAIN-CASE
   HABU-CASE
   BRANCH-CASE
   MEM-CASE
   LOCALS-CASE
   BUFFER-MAKE
   BUFFER-MAKE2
   BSUM-CASE
   BFIND-CASE
   COUNTCHAR-CASE
   WFUSE-CASE
   MAXDIM-CASE
   SUMMAX-CASE
   TWOLOOP-CASE
   TWOIF-CASE
   NESTW-CASE
   ELSEXIT-CASE
   FACT-CASE
   QWALK-CASE
   QWIDE-CASE
   ISLT-CASE
   LTKEEP-CASE
   SPILL-CASE
   RSPILL-CASE
   AGREE-CASES
   POOL-CASES
   NORET-TRAIT-CASE
   NORET-FIELD-CASE
   NORET-LAYOUT-CASE
   LINK-SPLIT-CASE
   NORET-SPILL-CASE
   T-REPORT ;

;package

NCHAIN-TEST:RUN
