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
\ inputs are the definition's source text and the two counts the elaborator is
\ still told rather than reads (dot habu-bind-checker-env-ed4f9f87); everything
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
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
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
\ THE ONE SEAM WHERE ARITY ENTERS. `1 1` is the word's declared effect, stated
\ here rather than read from the checker that just accepted it. The checker knows
\ - it parsed `( n -- n )` during this very scan - but it publishes an effect
\ only through a lookup by NAME into its live store, which answers about whoever
\ carries that name now and not about the definition this tape is. Binding the
\ accepted effect to the recorded unit is dot habu-bind-checker-env-ed4f9f87,
\ reached through habu-bind-the-colon-ea509e61; when it lands these two numbers
\ come off this line and nothing else here changes.
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

\ Five instructions: the pointer down over the one argument, the load, the
\ multiply, the store, the pointer up over the one result - and the return.
: HABU-CASE ( -- )
   s" a compiled word is entered and left through the data stack" T-LABEL
   NFIX:BINDING [: HABU-BODY ;] IR-CTX:WITH-CONTEXT
   121 T= 49 T= 49 T= 6 T= ;

\ ---- a definition with a branch, all the way through -------------------------
\ The same run over a body whose answer depends on which way a branch went. Four
\ blocks come out of `if … then`: the entry that compares and branches, the stub
\ the false arm reaches the join through, the true arm, and the join that takes
\ both arms' values as its arguments. What this case adds to the two above is the
\ layout and the fixups - the block starts are asserted exactly, and so are the
\ two branch instructions at the end of the entry block, decoded as the numbers
\ the assembler produces for them.
\
\ WHY THE TWO BRANCH WORDS ARE PINNED. A branch is the one instruction whose
\ operand is not in the module: it is the distance to a block, computed from the
\ layout. Asserting the emitted word is the only way to say the distance was
\ computed from the right end - a fixup to the wrong block, or a displacement
\ measured from the branch's block instead of the branch itself, changes exactly
\ these two numbers and nothing else the suite can see. The `cbz` at index 6
\ carries +2, which lands on block one; the `b` at index 7 carries +4, which
\ lands on block two. Swapping the two successors swaps those two numbers.
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
   2 A64EMIT:BLOCK-START@
   3 A64EMIT:BLOCK-START@
   6 A64EMIT:WORD@
   7 A64EMIT:WORD@
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   9 -1 fn NRUN:ENTER2
   s" 3 4 NCH-MAX" EV-N
   s" 9 -1 NCH-MAX" EV-N ;

: BRANCH-CASE ( -- )
   s" a definition with a branch compiles, lays out and runs" T-LABEL
   NFIX:BINDING [: BRANCH-BODY ;] IR-CTX:WITH-CONTEXT
   9 T= 4 T= 9 T= 4 T=
   335544324 T= 3019898946 T=
   14 T= 11 T= 8 T= 0 T=
   4 T= 17 T= -1 T= 7 T= ;

\ ---- a definition that reads and writes memory -------------------------------
\ The same run over a body whose whole point is a side effect, and the first one
\ whose source names something outside the dialect's vocabulary: a `create`d data
\ word. Three things make this case the acceptance of the memory leaf rather than
\ another shape test.
\
\ FIRST, THE DATA WORD IS THE ENGINE'S. The cell is created by evaluating
\ `create NCH-CELL 1 cells allot` through the same front end the definition goes
\ through, and its address is read back by evaluating its name. Nothing in this
\ file writes an address down. What the word model is TOLD is that one number,
\ which is the seam dot habu-resolve-a-data-a1c8067f closes; everything else -
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
\ AND THE ARGUMENT IS BIGGER THAN A BYTE ON PURPOSE. The dialect has one access
\ width and the assembler has four encoders; a cell-width load written with the
\ byte-width encoder would answer the same number for every argument under 256,
\ so the argument is 4000 and the answer 4001, which the low eight bits cannot
\ carry.
: SRC4 ( -- ptr u8 n )
   s" : NCH-BUMP ( n -- n ) NCH-CELL ! NCH-CELL @ 1+ dup NCH-CELL ! ;" ;

\ The word model this definition needs: the dialect's own vocabulary plus the one
\ data word the body names, committed to one row more than REGISTER-WORDS writes.
: MODEL-MEM ( IR-CTX:ctx IR-BUILD:builder n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" NCH-CELL" IR-BUILD:INTERN-SYMBOL  v HIR-WORD:DECLARE-FIXED
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
   CC BB  s" NCH-CELL" EV-N  MODEL-MEM {: p:IR-ARENA:arena r:IR-ARENA:arena :}
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

public

: RUN ( -- )
   T-RESET
   CHAIN-CASE
   HABU-CASE
   BRANCH-CASE
   MEM-CASE
   T-REPORT ;

;package

NCHAIN-TEST:RUN
