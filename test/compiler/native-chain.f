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
\ AND THE ARGUMENT IS BIGGER THAN A BYTE ON PURPOSE. The dialect has a cell
\ access and a byte access and the assembler has an encoder for each; a
\ cell-width access written with the byte-width encoder would keep only the low
\ eight bits, so the argument is 4000 and the answer 4001, which those bits
\ cannot carry. The case below this one holds the same line from the other end.
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

\ Fourteen instructions: the pointer down over the three arguments, three loads,
\ the subtraction, the multiply, the constant, the division's three, the
\ addition, the store, the pointer up over the one result, and the return.
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
   -28 T= 8 T= -28 T= 25 T= 8 T= 14 T= -1 T= 15 T= ;

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
   s" a definition that reads bytes in a loop compiles and runs" T-LABEL
   NFIX:BINDING [: BSUM-BODY ;] IR-CTX:WITH-CONTEXT
   HABU-SUM T= 0 T= HABU-SUM T= 7 T= ;

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
   s" a definition that leaves from the middle of a loop compiles and runs" T-LABEL
   NFIX:BINDING [: BFIND-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= -1 T= 2 T= 11 T= ;

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

: FACT-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD8
   p r ELABORATE
   CC BB TXT TEXT-LEN 0 LOOP-REGS 1 1 NFIX:RUN-HABU-CALL
   A64EMIT:BLOCKS
   NRUN:PUBLISH {: fn:n :}
   10 fn NRUN:ENTER1
   1 fn NRUN:ENTER1
   s" 10 NCH-FACT" EV-N
   s" 1 NCH-FACT" EV-N ;

: FACT-CASE ( -- )
   s" a definition that calls itself compiles and runs" T-LABEL
   NFIX:BINDING [: FACT-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 3628800 T= 1 T= 3628800 T= 5 T= ;

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

public

: RUN ( -- )
   T-RESET
   CHAIN-CASE
   HABU-CASE
   BRANCH-CASE
   MEM-CASE
   LOCALS-CASE
   BUFFER-MAKE
   BSUM-CASE
   BFIND-CASE
   FACT-CASE
   AGREE-CASES
   T-REPORT ;

;package

NCHAIN-TEST:RUN
