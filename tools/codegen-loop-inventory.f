\ codegen-loop-inventory.f - what a loop in the code this chain emits actually
\ holds. One concern: finding the loops of a published routine and counting the
\ work inside them that does not depend on the turn.
\
\ WHY IT IS MEASURED BEFORE ANYTHING IS BUILT. Loop-invariant code motion and
\ unrolling are worth their weight only where the shapes they move actually
\ occur, and which shapes occur is a fact about what this chain emits for this
\ corpus - not about what loop optimisers are worth in general. So this lane
\ counts first and builds second, exactly as tools/codegen-combine-inventory.f
\ did for the multiply-add, and a pattern whose count here is zero is reported as
\ a measured zero instead of being written.
\
\ WHAT A LOOP IS HERE. A back edge, and nothing else. The elaborator gives a
\ loop a header block that control reaches twice, and
\ src/compiler/native/emit.f lays the blocks out so that the header falls into
\ the body, the exit stub sinks below the latch, and the branch that survives is
\ an unconditional one from the latch back up to the header. So a loop in the
\ emitted code is an unconditional branch whose target is INSIDE this routine's
\ record and at or below the branch itself, and the body is the span between
\ them. That target test is the one tools/codegen-tail-probe.f already makes for
\ the opposite reason - it recognises a TAIL branch by the target being OUTSIDE
\ the record, and its header says in as many words that a loop's back edge is an
\ unconditional branch too.
\
\ AND WHY THE DISPLACEMENT IS NOT DECODED HERE. src/compiler/native/branch.f is
\ the one reader of that arithmetic and exists because there were three copies of
\ it. It answers an ADDRESS from an address and an instruction word, so this file
\ works in a coordinate system whose origin is the routine's first instruction:
\ index k sits at byte 4k, NBR:B-TARGET answers 4(k+d), and dividing by the
\ instruction size gives the target INDEX. The arithmetic is still branch.f's.
\
\ THE TWO PATTERNS COUNTED, AND WHY THESE TWO.
\
\   A MOVE-WIDE INSIDE A LOOP BODY. `movz`/`movk` build a literal into a
\   register and read NO register at all, so an occurrence inside a loop body is
\   loop-invariant by construction - there is no operand that could vary with the
\   turn. It needs no dataflow argument and no alias argument, which is what
\   makes it the cleanest possible witness that a hoist has something to move.
\   CODEGEN-CORPUS4:BIG-CONSTS is the row built to hold four 64-bit literals a
\   turn and is where this count is expected to land.
\
\   A LOAD WHOSE BASE THE BODY NEVER WRITES, in a body that holds no store and
\   no call. Then no access inside the loop can reach the cell this load reads,
\   and the address it reads through is the same every turn. This is the shape
\   the dot leaf calls an invariant load, counted without any alias analysis:
\   the absence of a store and of a call in the body is what stands in for one,
\   and it is exactly the fact the a64 dialect's memory token chain carries.
\
\ BOTH COUNTS ARE A FLOOR, AND THE REASON IS THE SAME ONE THE COMBINE INVENTORY
\ GIVES. This file reads FINISHED code, after the register allocator has reused
\ registers for unrelated values. A value that is loop-invariant can land in a
\ register some other value writes inside the body, and then this file calls it
\ variant. It also over-approximates what an instruction writes: it treats the
\ low five bits of every word as a destination register, which for a store is
\ the value being stored and for a branch is part of a displacement. Both errors
\ push the counts DOWN. So a positive count here is evidence that a shape really
\ occurs, and a zero is evidence about the emitted code rather than a proof about
\ the values - a pass running before the allocator, where invariance is a
\ property of an SSA value, can only find more.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/arch/arm64/asm.f
require src/compiler/native/branch.f
require tools/codegen-tail-probe.f
require tools/codegen-combine-inventory.f

package NLOOPINV

private

NBR:INSN-BYTES constant INSN-BYTES

\ The move-wide forms' fields. The immediate is sixteen bits and the shift is the
\ two-bit `hw` that says which quarter of the register it lands in; both are put
\ back through src/arch/arm64/asm.f's own encoder below, so this file names the
\ positions and the assembler keeps the layout.
$FFFF constant IMM16-MASK
3 constant HW-MASK

\ The unsigned immediate an ARM64 add or subtract carries in its own field. A
\ constant below it never needed a register of its own. This file's own copy of
\ A64ASM:IMM12-LIM: qualifying the two encoder calls below rather than importing
\ A64ASM keeps this local name from silently shadowing the published one.
$1000 constant IMM12-LIM

: FIMM16 ( n -- n ) {: w:n :}
   w 5 rshift IMM16-MASK and ;

: FHW ( n -- n ) {: w:n :}
   w 21 rshift HW-MASK and ;

\ x18 is Darwin platform-reserved and every X-register encoder in
\ src/arch/arm64/asm.f ends the process on it, so a word whose register field
\ decodes to x18 was not written by that assembler and is screened before any
\ field is handed to one - the same guard tools/codegen-combine-inventory.f
\ carries, and for the same reason.
18 constant RESERVED-REG

: ENCODABLE? ( n -- bool ) {: r:n :}
   r RESERVED-REG <> ;

public

\ ---- the two move-wide classifiers -------------------------------------------
\ Decode the destination, the immediate and the quarter, and ask the assembler to
\ write the instruction again from them: the word IS that form exactly when the
\ encoder reproduces it bit for bit.

: MOVZ? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRD ENCODABLE? 0= if false exit then
   w NCOMBINV:FRD  w FIMM16  w FHW  A64ASM:MOVZHW  w = ;

: MOVK? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRD ENCODABLE? 0= if false exit then
   w NCOMBINV:FRD  w FIMM16  w FHW  A64ASM:MOVKHW  w = ;

\ Either half of a materialised literal.
: MOVE-WIDE? ( n -- bool ) {: w:n :}
   w MOVZ? if true exit then
   w MOVK? ;

private

\ ---- the row under the counters ----------------------------------------------
\ The subject travels as package state for the reason
\ tools/codegen-combine-inventory.f gives: a quotation cannot read the enclosing
\ word's locals, so the routine being counted cannot be one. The walk itself is
\ that file's, so a routine's instructions have one reader here.

: INSNS ( -- n )
   NCOMBINV:INSNS ;

: INSN@ ( n -- n ) {: k:n :}
   k NCOMBINV:INSN@ ;

\ Where the branch at this index goes, as an index. The origin is the routine's
\ first instruction, so branch.f's address arithmetic answers a byte offset that
\ divides back into an index.
: B-TARGET-IX ( n -- n ) {: k:n :}
   k INSN-BYTES *  k INSN@  NBR:B-TARGET  INSN-BYTES / ;

\ ---- reachability inside a span, and why a back edge needs it -----------------
\
\ A BRANCH THAT POINTS BACKWARD IS NOT YET A LOOP, which this file learned by
\ measuring rather than by reasoning. CODEGEN-CORPUS:BYTE-FIND returns -1 when
\ its scan finds nothing; that -1 is a four-instruction literal chain, the
\ elaborator gives it a block of its own, and the emitter lays that block out
\ BEFORE the loop's exit stub branches to it. So the routine holds an
\ unconditional branch to a lower address which is a perfectly ordinary forward
\ edge in the control-flow graph and a backward one in the layout, and a rule
\ that called every backward branch a back edge counted that as a second loop and
\ charged the -1 chain to a loop body it is not in.
\
\ WHAT TELLS THEM APART IS A CYCLE, WHICH IS WHAT A LOOP IS. The span between a
\ candidate branch and its target is a loop exactly when control entering at the
\ target can arrive back at the branch. For the -1 block it cannot: the block ends
\ in an unconditional branch to the epilogue, so nothing that enters it ever
\ reaches the branch below. That is not a heuristic about layout, it is the
\ definition, and it is decided here by walking the span's own control flow.
\
\ THE WALK IS CONSERVATIVE IN THE DIRECTION THAT REFUSES. A fall-through is a
\ successor unless the instruction is an unconditional branch or a return; a
\ branch of any of the four forms adds its target when the target is inside the
\ span. An instruction this file cannot classify contributes its fall-through
\ only, so an unrecognised control transfer can lose a path and make a real loop
\ look unreachable - it declines a candidate, never invents one.

\ The largest routine this walk will follow. The corpus's biggest chain
\ compilation is 41 instructions and the engine's biggest is a few hundred, so
\ this is room to spare rather than a bound anybody is near; a span past it is
\ declined by name below instead of overrunning the mark.
4096 constant SPAN-MAX
create REACHED SPAN-MAX allot
create CANREACH SPAN-MAX allot

: REACHED-CLEAR ( -- )
   SPAN-MAX 0 ?do 0 REACHED i + c! loop ;

: REACHED? ( n -- bool ) {: j:n :}
   REACHED j + c@ 0<> ;

: REACH! ( n -- ) {: j:n :}
   1 REACHED j + c! ;

: CANREACH-CLEAR ( -- )
   SPAN-MAX 0 ?do 0 CANREACH i + c! loop ;

: CANREACH? ( n -- bool ) {: j:n :}
   CANREACH j + c@ 0<> ;

: CANREACH! ( n -- ) {: j:n :}
   1 CANREACH j + c! ;

\ Whether this instruction lets control fall into the next one.
: FALLS? ( n -- bool ) {: w:n :}
   w NBR:B? if false exit then
   w NBR:RET? if false exit then
   true ;

\ The branch target of any form that has one, as an index, or -1.
: TARGET-IX ( n -- n ) {: k:n :}
   k INSN@ {: w:n :}
   w NBR:B? if k B-TARGET-IX exit then
   w NBR:COND? if k INSN-BYTES * w NBR:COND-TARGET INSN-BYTES / exit then
   -1 ;

\ One round of the walk: mark every successor of an already-reached instruction
\ that stays inside the span. Answers whether anything changed.
: REACH-ROUND ( n n -- bool ) {: lo:n hi:n :}
   false
   hi 1+ lo ?do
      i REACHED? if
         i INSN@ FALLS? i 1+ hi <= and i 1+ REACHED? 0= and if
            i 1+ REACH! drop true
         then
         i TARGET-IX {: t:n :}
         t lo >= t hi <= and t REACHED? 0= and if
            t REACH! drop true
         then
      then
   loop ;

\ Whether control entering the span at its first instruction can arrive at its
\ last. The rounds only ever add, and the span is bounded, so the walk settles.
: SPAN-REACHES? ( n n -- bool ) {: lo:n hi:n :}
   hi SPAN-MAX >= if false exit then
   REACHED-CLEAR
   lo REACH!
   begin lo hi REACH-ROUND while repeat
   hi REACHED? ;

\ ---- which instructions of the span are really the loop -----------------------
\
\ THE SPAN IS NOT THE BODY. The emitter lays a loop's blocks out contiguously,
\ but it may lay other blocks out among them: BYTE-FIND's -1 return block sits
\ between the loop's own blocks, reached only when the scan finishes without a
\ match. Counting the span would charge that literal chain to a loop that never
\ executes it, which is the same mistake the back-edge rule above already made
\ once.
\
\ THE BODY IS THE CYCLE, and an instruction is on the cycle exactly when control
\ can arrive at it from the header AND can carry on from it to the back edge.
\ The first is the walk above; the second is the same walk run backwards. Their
\ intersection is the natural loop, and the -1 block fails the second half
\ because it ends in an unconditional branch to the epilogue.

\ One round of the backward walk: mark an instruction whose successor is already
\ marked. Answers whether anything changed.
: CANREACH-ROUND ( n n -- bool ) {: lo:n hi:n :}
   false
   hi 1+ lo ?do
      i CANREACH? 0= if
         i INSN@ FALLS? i 1+ hi <= and i 1+ CANREACH? and if
            i CANREACH! drop true
         then
         i TARGET-IX {: t:n :}
         t lo >= t hi <= and t CANREACH? and i CANREACH? 0= and if
            i CANREACH! drop true
         then
      then
   loop ;

\ Fill both marks for this loop. Every body word below calls it first, because
\ the two arrays are package state shared with the back-edge test.
: BODY-MARK ( n n -- ) {: lo:n hi:n :}
   REACHED-CLEAR
   CANREACH-CLEAR
   lo REACH!
   begin lo hi REACH-ROUND while repeat
   hi CANREACH!
   begin lo hi CANREACH-ROUND while repeat ;

: IN-BODY? ( n -- bool ) {: j:n :}
   j REACHED? j CANREACH? and ;

\ Whether the instruction at this index is a loop's back edge: a branch of any
\ form whose target is inside this record and at or above the branch itself in
\ the layout, and whose span really is a cycle.
\
\ IT USED TO INSIST THE BRANCH WAS UNCONDITIONAL, AND THAT STOPPED BEING TRUE.
\ The emitter's collapse (src/compiler/native/emit.f, ORDER-BLOCKS) branches past
\ any block that emits nothing before its terminator, and a counted loop's latch
\ is exactly such a block: the conditional at the bottom of the body used to
\ reach the header through the latch's unconditional branch, and now names the
\ header itself. So a loop's back edge is routinely a `b.cc`, and a rule that
\ read only `b` reported the corpus's counted loops as no loops at all.
\
\ DROPPING THE OPCODE TEST COSTS NOTHING, because it was never what did the
\ discriminating work. The hazard this file exists to avoid - a branch pointing
\ backward in the LAYOUT that is an ordinary forward edge in the graph - is
\ refused by the cycle test below, which asks whether control entering at the
\ target can arrive back at the branch. CODEGEN-CORPUS:BYTE-FIND's -1 literal
\ chain is declined by that test and not by its opcode.
: BACK-EDGE? ( n -- bool ) {: k:n :}
   k INSN@ {: w:n :}
   w NBR:B? w NBR:COND? or 0= if false exit then
   k TARGET-IX {: t:n :}
   t 0 < if false exit then
   t k > if false exit then
   t k SPAN-REACHES? ;

public

\ How many loops the routine holds.
: LOOPS ( -- n )
   0
   INSNS 0 ?do
      i BACK-EDGE? if 1+ then
   loop ;

private

\ How many back edges stand before this index. Counting them is how the k-th one
\ is named below without a running counter the loop would have to carry.
: EDGES-BEFORE ( n -- n ) {: k:n :}
   0
   k 0 ?do
      i BACK-EDGE? if 1+ then
   loop ;

\ The k-th back edge's index, or -1 when the routine holds fewer than k+1 loops.
: EDGE-IX ( n -- n ) {: k:n :}
   -1
   INSNS 0 ?do
      i BACK-EDGE? i EDGES-BEFORE k = and if drop i leave then
   loop ;

public

\ The body of the k-th loop, as the half-open index span between the header the
\ back edge names and the back edge itself. The branch is not part of the body:
\ it is the turn, not the work.
\
\ THE TARGET IS READ BY THE FORM AND NOT AS AN UNCONDITIONAL BRANCH. A back edge
\ is now routinely a `b.cc` - see BACK-EDGE? - and the two forms carry their
\ displacement in different fields, so decoding a conditional branch with the
\ unconditional reader answers a number that is not an index of anything. It did
\ exactly that here: the low bits of a `b.cc` read as a 26-bit displacement gave
\ a body span millions of instructions long and the marking walk ran off its
\ arrays. TARGET-IX asks src/compiler/native/branch.f for the form it really is.
: LOOP-LO ( n -- n ) {: k:n :}
   k EDGE-IX dup 0 < if exit then
   TARGET-IX ;

: LOOP-HI ( n -- n ) {: k:n :}
   k EDGE-IX ;

private

\ ---- what a body holds --------------------------------------------------------

\ Whether any instruction of the span writes this register. Every word's low five
\ bits are read as a destination, which over-counts writes - a store's are the
\ value it stores and a branch's are displacement bits - and therefore only ever
\ refuses a candidate this file would otherwise have counted.
: WRITTEN-IN? ( n n n -- bool ) {: lo:n hi:n r:n :}
   false
   hi lo ?do
      i INSN@ NCOMBINV:FRD r = if drop true leave then
   loop ;

\ Whether the span holds a call. A call may reach any cell the program can, so a
\ load in a body that makes one cannot be shown invariant without knowing what
\ the callee touches, which this file does not.
: CALLS-IN? ( n n -- bool ) {: lo:n hi:n :}
   false
   hi lo ?do
      i INSN@ NBR:BL? if drop true leave then
   loop ;

\ And whether it holds a store, which is the other way a cell this load reads
\ could change between turns.
: STORES-IN? ( n n -- bool ) {: lo:n hi:n :}
   false
   hi lo ?do
      i INSN@ NCOMBINV:STR? if drop true leave then
   loop ;

public

\ The instructions of the k-th loop's body: the ones on the cycle, not the ones
\ the layout happens to put between the header and the back edge.
: BODY-OPS ( n -- n ) {: k:n :}
   k LOOP-LO {: lo:n :}
   lo 0 < if 0 exit then
   k LOOP-HI {: hi:n :}
   lo hi BODY-MARK
   0
   hi lo ?do
      i IN-BODY? if 1+ then
   loop ;

\ The move-wide instructions inside it: literal halves rebuilt every turn, each
\ one loop-invariant by construction because it reads no register at all.
: BODY-CONSTS ( n -- n ) {: k:n :}
   k LOOP-LO {: lo:n :}
   lo 0 < if 0 exit then
   k LOOP-HI {: hi:n :}
   lo hi BODY-MARK
   0
   hi lo ?do
      i IN-BODY? i INSN@ MOVE-WIDE? and if 1+ then
   loop ;

\ Of those move-wides, the ones a different transform would remove outright
\ rather than move. A LONE `movz` into the bottom quarter of a register with an
\ immediate small enough for an arithmetic instruction's own field is not a
\ 64-bit literal at all: it is a small number materialised into a register
\ because something selected a register-register form to read it. Hoisting one
\ would save an instruction per turn and cost a register held across the loop;
\ folding it into the reading instruction saves the instruction and costs
\ nothing. So this column is counted separately and is NOT a hoisting
\ opportunity - it is the measurement saying a cheaper transform owns these.
\
\ THAT TRANSFORM NOW EXISTS, so read this column as what is LEFT rather than as
\ what is available. src/compiler/native/combine.f folds a small constant into
\ the add, the subtract, the bitwise operation or the COMPARISON that reads it,
\ and this tool measures the code the chain finally emits, so a constant that
\ folded is no longer a move-wide here at all. What still counts is the residue
\ the fold deliberately leaves: a constant whose reader is one of the forms that
\ has no immediate at all - a multiply is the one this corpus keeps hitting,
\ which is why `cells` leaves an 8 in a register in every loop that indexes -
\ and a constant with more than one reader, which has to stay in a register for
\ the readers that did not fold. Both are correct refusals, and counting them
\ keeps them visible instead of letting a later reader assume the column went to
\ zero because the work is finished.
: BODY-FOLDABLE ( n -- n ) {: k:n :}
   k LOOP-LO {: lo:n :}
   lo 0 < if 0 exit then
   k LOOP-HI {: hi:n :}
   lo hi BODY-MARK
   0
   hi lo ?do
      i IN-BODY? i INSN@ MOVZ? and
      i INSN@ FHW 0= and
      i INSN@ FIMM16 IMM12-LIM < and
      i 1+ hi < i 1+ INSN@ MOVK? and
      i 1+ INSN@ NCOMBINV:FRD i INSN@ NCOMBINV:FRD = and 0= and
      if 1+ then
   loop ;

\ The loads inside it whose base the body never writes, counted only when the
\ body holds neither a store nor a call - so nothing inside the loop can reach
\ the cell being read and the address is the same every turn.
: BODY-INV-LOADS ( n -- n ) {: k:n :}
   k LOOP-LO {: lo:n :}
   lo 0 < if 0 exit then
   k LOOP-HI {: hi:n :}
   lo hi CALLS-IN? if 0 exit then
   lo hi STORES-IN? if 0 exit then
   lo hi BODY-MARK
   0
   hi lo ?do
      i IN-BODY? i INSN@ NCOMBINV:LDR? and if
         lo hi  i INSN@ NCOMBINV:FRN  WRITTEN-IN? 0= if 1+ then
      then
   loop ;

\ ---- the routine's totals -----------------------------------------------------
\ One word per column of the report, so that the file that PRINTS the inventory
\ and the file that ASSERTS it read the same measurement rather than two
\ spellings of it.

: BODY-TOTAL ( -- n )
   0
   LOOPS 0 ?do
      i BODY-OPS +
   loop ;

: CONSTS-TOTAL ( -- n )
   0
   LOOPS 0 ?do
      i BODY-CONSTS +
   loop ;

: FOLDABLE-TOTAL ( -- n )
   0
   LOOPS 0 ?do
      i BODY-FOLDABLE +
   loop ;

: INV-LOADS-TOTAL ( -- n )
   0
   LOOPS 0 ?do
      i BODY-INV-LOADS +
   loop ;

\ The subject, set through the walk's own owner so both inventories name a
\ routine the same way.
: ROW! ( ptr u8 n -- )
   NCOMBINV:ROW! ;

;package
