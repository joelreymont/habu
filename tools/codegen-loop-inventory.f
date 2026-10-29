\ codegen-loop-inventory.f - finds the loops of a published routine's emitted
\ code and counts the turn-independent work inside them, on ARM64 and Intel. A
\ loop is a back edge whose span is a cycle. Counts read finished code after
\ register allocation, which can put an invariant value in a register the body
\ writes, so they are a floor: a pass on SSA values before the allocator can
\ only find more.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/arch/arm64/asm.f
require src/compiler/native/branch.f
require src/arch/x86-64/disasm.f
require src/habu/code-bytes.f
require tools/codegen-tail-probe.f
require tools/codegen-combine-inventory.f

package NLOOPINV

private

NBR:INSN-BYTES constant INSN-BYTES

\ The cycle walker has one mark per decoded instruction. Intel instructions have
\ variable widths, so their byte starts and flow facts (X64DIS:STEP; a call
\ falls through, a return or unconditional escape ends the path) are recorded
\ once before the reachability walk uses instruction indices.
4096 constant SPAN-MAX
create X-OFF SPAN-MAX cells allot
create X-FLOW SPAN-MAX cells allot
create X-TARGET SPAN-MAX cells allot
variable X-N
variable X-LEN
variable X-CURSOR

: X-OFF@ ( n -- n ) cells X-OFF + @ ;
: X-FLOW@ ( n -- n ) cells X-FLOW + @ ;
: X-TARGET@ ( n -- n ) cells X-TARGET + @ ;

: X-INDEX ( n -- n ) {: off:n :}
   X-N @ 0 ?do
      i X-OFF@ off = if i unloop exit then
   loop -1 ;

: X-TARGET-IX ( n -- n ) {: k:n :}
   k X-TARGET@ {: off:n :}
   off 0 < off X-LEN @ >= or if -1 exit then
   off X-INDEX dup 0 < if E-CODEGEN-PROBE-EXTENT throw then ;

: X-ROW! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-PROBE-SUBJECT throw then
   {: rec:ptr :}
   rec XREF-START rec XREF-CODE-BYTES CODE-BYTES:AT
   {: code:ptr bytes:n :}
   bytes 0 <= if E-CODEGEN-PROBE-EXTENT throw then
   bytes X-LEN ! 0 X-N ! 0 X-CURSOR !
   begin X-CURSOR @ bytes < while
      X-N @ SPAN-MAX >= if E-CODEGEN-PROBE-EXTENT throw then
      X-N @ {: k:n :}
      X-CURSOR @ k cells X-OFF + !
      code X-CURSOR @ + bytes X-CURSOR @ - X-CURSOR @ X64DIS:STEP
      {: size:n flow:n target:n :}
      flow k cells X-FLOW + !
      target k cells X-TARGET + !
      X-CURSOR @ size + X-CURSOR !
      k 1+ X-N !
   repeat ;

\ The move-wide fields. src/arch/arm64/asm.f's encoder puts them back below, so
\ the assembler keeps the layout.
$FFFF constant IMM16-MASK
3 constant HW-MASK

\ A constant below the add/subtract unsigned immediate never needed a register.
\ A local copy of A64ASM:IMM12-LIM: the encoder calls below are qualified rather
\ than importing A64ASM, so this name shadows nothing.
$1000 constant IMM12-LIM

: FIMM16 ( n -- n ) {: w:n :}
   w 5 rshift IMM16-MASK and ;

: FHW ( n -- n ) {: w:n :}
   w 21 rshift HW-MASK and ;

\ On Darwin, screen x18 before asking an encoder that must refuse it. Linux can
\ re-encode it normally.
18 constant RESERVED-REG

: ENCODABLE? ( n -- bool ) {: r:n :}
   r RESERVED-REG <> if true exit then
   HB-TARGET-KNOWN? 0= if E-CTGT-ABI throw then
   HB-TARGET-LINUX? ;

public

\ ---- the two move-wide classifiers -------------------------------------------
\ The word is that form exactly when the encoder reproduces it bit for bit from
\ the decoded fields.

: MOVZ? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRD ENCODABLE? 0= if false exit then
   w NCOMBINV:FRD  w FIMM16  w FHW  A64ASM:MOVZHW  w = ;

: MOVK? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRD ENCODABLE? 0= if false exit then
   w NCOMBINV:FRD  w FIMM16  w FHW  A64ASM:MOVKHW  w = ;

: MOVE-WIDE? ( n -- bool ) {: w:n :}
   w MOVZ? if true exit then
   w MOVK? ;

private

\ ---- the row under the counters ----------------------------------------------
\ Package state, as in tools/codegen-combine-inventory.f: a quotation cannot
\ read the enclosing word's locals. The walk is that file's, so a routine's
\ instructions have one reader.

: INSNS ( -- n )
   HB-TARGET-LINUX-X86-64? if X-N @ exit then
   NCOMBINV:INSNS ;

: INSN@ ( n -- n ) {: k:n :}
   k NCOMBINV:INSN@ ;

\ The origin is the routine's first instruction: src/compiler/native/branch.f,
\ the one reader of the displacement, answers byte 4(k+d), which divides back
\ into an index.
: B-TARGET-IX ( n -- n ) {: k:n :}
   k INSN-BYTES *  k INSN@  NBR:B-TARGET  INSN-BYTES / ;

\ ---- reachability inside a span, and why a back edge needs it -----------------
\ A branch backward in the layout is not yet a loop: a scan's -1 literal chain,
\ measured, is a block laid out before the exit stub that branches to it, a
\ forward edge in the graph. A span is a loop exactly when control entering at
\ the target can arrive back at the branch.
\ The walk errs toward refusal: a fall-through is a successor unless the
\ instruction is an unconditional branch or a return, and a branch adds its
\ target only inside the span. An ARM instruction this file cannot classify only
\ falls through and may decline a real loop. Intel refuses unknown or truncated
\ opcodes while making its instruction map, before any zero-loop result is
\ possible.

\ The largest routine the walk follows: the corpus's biggest chain compilation
\ is 41 instructions and the engine's a few hundred. A longer span is declined
\ by name below instead of overrunning the mark.
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

: FALLS? ( n -- bool ) {: w:n :}
   HB-TARGET-LINUX-X86-64? if
      w X-FLOW@ dup X64DIS:FLOW-JUMP = over X64DIS:FLOW-END = or
      swap X64DIS:FLOW-INDIRECT = or 0= exit
   then
   w INSN@ dup NBR:B? if drop false exit then
   NBR:RET? if false exit then
   true ;

\ The branch target of any form, as an index, or -1.
: TARGET-IX ( n -- n ) {: k:n :}
   HB-TARGET-LINUX-X86-64? if
      k X-FLOW@ dup X64DIS:FLOW-COND = swap X64DIS:FLOW-JUMP = or if
         k X-TARGET-IX exit
      then -1 exit
   then
   k INSN@ {: w:n :}
   w NBR:B? if k B-TARGET-IX exit then
   w NBR:COND? if k INSN-BYTES * w NBR:COND-TARGET INSN-BYTES / exit then
   -1 ;

\ Answers whether anything changed. A mark is read only inside the span: a
\ branch target can lie at any distance, and `and` evaluates both operands, so a
\ span test beside the read would not stop it.
: REACH-ROUND ( n n -- bool ) {: lo:n hi:n :}
   false
   hi 1+ lo ?do
      i REACHED? if
         i FALLS? i 1+ hi <= and if
            i 1+ REACHED? 0= if i 1+ REACH! drop true then
         then
         i TARGET-IX {: t:n :}
         t lo >= t hi <= and if
            t REACHED? 0= if t REACH! drop true then
         then
      then
   loop ;

\ The rounds only add and the span is bounded, so the walk settles.
: SPAN-REACHES? ( n n -- bool ) {: lo:n hi:n :}
   hi SPAN-MAX >= if false exit then
   REACHED-CLEAR
   lo REACH!
   begin lo hi REACH-ROUND while repeat
   hi REACHED? ;

\ ---- which instructions of the span are really the loop -----------------------
\ The span is not the body: the emitter may lay other blocks among a loop's, as
\ that -1 return block. The body is the natural loop: the instructions
\ reached from the header (the walk above) that also reach the back edge (the
\ same walk run backwards).
: CANREACH-ROUND ( n n -- bool ) {: lo:n hi:n :}
   false
   hi 1+ lo ?do
      i CANREACH? 0= if
         i FALLS? i 1+ hi <= and if
            i 1+ CANREACH? if i CANREACH! drop true then
         then
         i TARGET-IX {: t:n :}
         t lo >= t hi <= and if
            t CANREACH? i CANREACH? 0= and if i CANREACH! drop true then
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

\ A back edge is a branch of any form whose target is inside this record at or
\ before the branch, and whose span is a cycle. tools/codegen-tail-probe.f makes
\ the same target test the other way: a target outside the record is a tail
\ branch. The emitter's collapse (src/compiler/native/emit.f ORDER-BLOCKS)
\ branches past an empty latch, so a counted loop's back edge is often a `b.cc`;
\ the cycle test, not the opcode, refuses a layout-backward forward edge.
: BACK-EDGE? ( n -- bool ) {: k:n :}
   HB-TARGET-LINUX-X86-64? if
      k X-FLOW@ dup X64DIS:FLOW-JUMP = swap X64DIS:FLOW-COND = or 0= if
         false exit
      then
   else
      k INSN@ {: w:n :}
      w NBR:B? w NBR:COND? or 0= if false exit then
   then
   k TARGET-IX {: t:n :}
   t 0 < if false exit then
   t k > if false exit then
   t k SPAN-REACHES? ;

public

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

\ The body of the k-th loop: the half-open span from the header the back edge
\ names to the back edge, which is the turn, not the work. TARGET-IX reads the
\ target by its form: `b` and `b.cc` keep the displacement in different fields,
\ and a `b.cc` read as a `b` gives a span millions of instructions long.
: LOOP-LO ( n -- n ) {: k:n :}
   k EDGE-IX dup 0 < if exit then
   TARGET-IX ;

: LOOP-HI ( n -- n ) {: k:n :}
   k EDGE-IX ;

private

\ ---- what a body holds --------------------------------------------------------

: ARM-SHAPE ( -- )
   HB-TARGET-LINUX-X86-64? if E-CTGT-ABI throw then ;

\ Every word's low five bits are read as a destination, which over-counts writes
\ (a store's value register, a branch's displacement bits) and so only refuses a
\ candidate.
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

\ Literal halves rebuilt every turn, each loop-invariant by construction because
\ a move-wide reads no register.
: BODY-CONSTS ( n -- n ) {: k:n :}
   ARM-SHAPE
   k LOOP-LO {: lo:n :}
   lo 0 < if 0 exit then
   k LOOP-HI {: hi:n :}
   lo hi BODY-MARK
   0
   hi lo ?do
      i IN-BODY? i INSN@ MOVE-WIDE? and if 1+ then
   loop ;

\ Of those, the ones folding into the reader removes: a lone `movz` into the
\ bottom quarter with an immediate below IMM12-LIM is a small number a
\ register-register form reads, not a hoisting opportunity.
\ src/compiler/native/select.f folds such constants into the add, subtract,
\ bitwise operation or comparison that reads them, so this column counts the
\ residue: a reader with no immediate form (a multiply: `cells` leaves an 8 in a
\ register in every indexing loop) and a constant with more than one reader.
: BODY-FOLDABLE ( n -- n ) {: k:n :}
   ARM-SHAPE
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

\ Loads whose base the body never writes, counted only when the body holds
\ neither a store nor a call: nothing in the loop can reach the cell, and the
\ address is the same every turn. That absence stands in for alias analysis; it
\ is the fact the a64 dialect's memory token chain carries.
: BODY-INV-LOADS ( n -- n ) {: k:n :}
   ARM-SHAPE
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
\ One word per report column, so the printer and the tests that assert it read
\ one measurement.

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
   HB-TARGET-LINUX-X86-64? if X-ROW! exit then
   NCOMBINV:ROW! ;

;package
