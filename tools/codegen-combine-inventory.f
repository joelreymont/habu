\ codegen-combine-inventory.f - how many pairs of emitted instructions one ARM64
\ three-source instruction would replace. One concern: counting those pairs in
\ the code a published routine actually holds.
\
\ WHY IT IS MEASURED BEFORE ANYTHING IS BUILT. A peephole library is worth its
\ weight only where the shapes it matches occur, and which shapes occur is a fact
\ about what this chain emits for this corpus - not about what ARM64 rewards in
\ general. So the combining lane counts first and builds second, and a pattern
\ whose count here is zero is reported as a measured zero instead of being
\ written. The counts below are the evidence for that decision and the row-by-row
\ prediction the finished pass is then held against.
\
\ WHY IT READS THE CODE AND NOT THE SOURCE. The same reason
\ tools/codegen-tail-probe.f does, and it reads through that file's walk rather
\ than opening a second one: a Habu body that multiplies and then adds does not
\ necessarily emit a multiply followed by an addition. Constants are materialised
\ into registers, callees are copied into their callers, and the register
\ allocator decides which register each value lands in - and that last decision
\ is what makes or breaks this particular fusion. Only the emitted code knows.
\
\ WHAT A CLASSIFIER HERE IS. Not a second table of bit layouts. Each `?` word
\ decodes the register fields out of a word and asks src/arch/arm64/asm.f's own
\ encoder to write the instruction again from them: the word IS that form exactly
\ when the encoder reproduces it bit for bit. So the layout has one authority, the
\ one that emitted the code, and a classifier cannot drift from the assembler it
\ classifies - a changed encoder changes both sides of the comparison at once.
\ The cost of that choice is the reserved-register guard below: those encoders end
\ the process rather than return a value when a field names x18, so every
\ classifier screens its fields first.
\
\ TWO NUMBERS PER PATTERN, AND WHY NEITHER ALONE IS THE ANSWER. `pairs` counts
\ adjacent dependent pairs - a multiply whose product the very next instruction
\ adds - and is the ceiling on what an adjacency peephole over emitted code could
\ fuse. `safe` counts the subset where the second instruction OVERWRITES the
\ product's register, which is the case a rewriter can take without knowing
\ whether anything later reads that product. The gap between them is exactly the
\ liveness question this file cannot answer and the machine dialect answers for
\ free: src/compiler/native/a64ir.f is SSA, so "the product has one use" is a
\ property of the value there rather than a dataflow pass. Read `safe` as the
\ floor and `pairs` as the ceiling of what a post-selection combine gets.
\
\ AND WHY BOTH ARE A FLOOR FOR THE PASS THAT WILL BE WRITTEN. This file reads the
\ FINISHED code, after the register allocator has reused registers for unrelated
\ values. A multiply and an addition that a combine would fuse can end up with a
\ third instruction between them writing the very register the multiply read, and
\ then no rewriter of emitted bytes may touch them - the measured C-MAD case. The
\ combining pass runs BEFORE the allocator, on values rather than registers,
\ where that collision does not exist. So a count here is evidence that a shape
\ occurs, and an undercount of what fusing it earlier is worth.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/arch/arm64/asm.f
require tools/codegen-tail-probe.f

package NCOMBINV

private

\ x18 is Darwin platform-reserved and every X-register encoder in
\ src/arch/arm64/asm.f ends the process on it, so no emitted routine holds it.
\ A word whose register field decodes to x18 was therefore not written by that
\ assembler and is not the form being tested - the classifiers ask this before
\ they re-encode, so a screening answer replaces a process exit.
18 constant RESERVED-REG

$1F constant REG-MASK              \ a register operand is a five-bit field
$FFF constant IMM12-MASK           \ the unsigned load/store offset field
8 constant SLOT-BYTES              \ one 64-bit slot: the load/store scale, and
                                   \ the distance a pairable pair's offsets differ by

: FRD ( n -- n ) {: w:n :}
   w REG-MASK and ;

: FRN ( n -- n ) {: w:n :}
   w 5 rshift REG-MASK and ;

: FRM ( n -- n ) {: w:n :}
   w 16 rshift REG-MASK and ;

: FI12 ( n -- n ) {: w:n :}
   w 10 rshift IMM12-MASK and ;

$3F constant SHIFT-MASK            \ a shift amount for a 64-bit register
$1FFF constant NIS-MASK            \ the packed N:immr:imms of a logical immediate
63 constant SHIFT-TOP              \ the largest shift, and the `imms` a zero shift leaves

\ The shift amount a left shift carries. src/arch/arm64/asm.f writes LSL as a
\ bitfield move whose `imms` field is 63 - sh, so the amount is read back out of
\ that field rather than standing in one of its own.
: FSHL ( n -- n ) {: w:n :}
   SHIFT-TOP  w 10 rshift SHIFT-MASK and  - ;

\ And the one a right shift carries, which does stand in a field of its own.
: FSHR ( n -- n ) {: w:n :}
   w 16 rshift SHIFT-MASK and ;

: FNIS ( n -- n ) {: w:n :}
   w 10 rshift NIS-MASK and ;

: ENCODABLE? ( n -- bool ) {: r:n :}
   r RESERVED-REG <> ;

\ The three-register forms share one screen: all three fields must be registers
\ an encoder will accept before any of them is handed to one.
: R3-OK? ( n -- bool ) {: w:n :}
   w FRD ENCODABLE?
   w FRN ENCODABLE? and
   w FRM ENCODABLE? and ;

\ The load/store forms name two registers; the offset is a field, not a register.
: R2-OK? ( n -- bool ) {: w:n :}
   w FRD ENCODABLE?
   w FRN ENCODABLE? and ;

public

: MUL? ( n -- bool ) {: w:n :}
   w R3-OK? 0= if false exit then
   w FRD w FRN w FRM ENC-MUL w = ;

: ADD? ( n -- bool ) {: w:n :}
   w R3-OK? 0= if false exit then
   w FRD w FRN w FRM ENC-ADD w = ;

: SUB? ( n -- bool ) {: w:n :}
   w R3-OK? 0= if false exit then
   w FRD w FRN w FRM ENC-SUB w = ;

: LDR? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FI12 SLOT-BYTES * ENC-LDR w = ;

: STR? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FI12 SLOT-BYTES * ENC-STR w = ;

: LSLI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FSHL ENC-LSLI w = ;

: LSRI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FSHR ENC-LSRI w = ;

: ASRI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FSHR ENC-ASRI w = ;

: ANDI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FNIS ENC-ANDI w = ;

\ Either shift whose amount an add or subtract could carry instead. ARM64's
\ shifted-register arithmetic holds a left shift and both right shifts, so all
\ three are asked about together.
: SHIFTI? ( n -- bool ) {: w:n :}
   w LSLI? if true exit then
   w LSRI? if true exit then
   w ASRI? ;

private

\ ---- the pair predicates ------------------------------------------------------

\ Whether the second instruction reads the register the first one wrote. It is
\ the dependence that makes a pair a candidate at all.
: READS-PRODUCT? ( n n -- bool ) {: m:n u:n :}
   u FRN m FRD =
   u FRM m FRD = or ;

\ Whether the second instruction also OVERWRITES that register. A rewriter that
\ drops the multiply takes the product away from everything that reads it later,
\ and this is the one case where nothing can: the very next instruction puts its
\ own result in that register, so no later reader could have been reading the
\ product.
: KILLS-PRODUCT? ( n n -- bool ) {: m:n u:n :}
   u FRD m FRD = ;

public

\ A multiply whose product the next instruction adds: what one Madd replaces.
: MADD-PAIR? ( n n -- bool ) {: m:n a:n :}
   m MUL? 0= if false exit then
   a ADD? 0= if false exit then
   m a READS-PRODUCT? ;

\ A multiply whose product the next instruction SUBTRACTS: what one Msub
\ replaces. Only the subtrahend position counts. Msub computes ra - rn*rm, so a
\ subtraction that takes the product away from something is the form; one that
\ takes something away FROM the product is not, and reversing it would change
\ the answer.
: MSUB-PAIR? ( n n -- bool ) {: m:n s:n :}
   m MUL? 0= if false exit then
   s SUB? 0= if false exit then
   s FRM m FRD = ;

: MADD-SAFE? ( n n -- bool ) {: m:n a:n :}
   m a MADD-PAIR? 0= if false exit then
   m a KILLS-PRODUCT? ;

: MSUB-SAFE? ( n n -- bool ) {: m:n s:n :}
   m s MSUB-PAIR? 0= if false exit then
   m s KILLS-PRODUCT? ;

\ A shift whose result the next instruction adds or subtracts: what one
\ shifted-register add or subtract replaces. Neither the model nor the shipped
\ encoders carry a shifted-operand form - src/arch/arm64/asm.f writes every
\ add and subtract with a zero shift field - so this is counted to size the
\ pattern, not because anything may emit it.
: SHIFT-FOLD-PAIR? ( n n -- bool ) {: s:n u:n :}
   s SHIFTI? 0= if false exit then
   u ADD? u SUB? or 0= if false exit then
   s u READS-PRODUCT? ;

\ A right shift whose result the next instruction masks: what one Ubfx
\ replaces. Counted on the same terms - the bitfield-extract form is modelled
\ nowhere in this tree.
: BITFIELD-PAIR? ( n n -- bool ) {: s:n m:n :}
   s LSRI? 0= if false exit then
   m ANDI? 0= if false exit then
   m FRN s FRD = ;

private

\ Two accesses one pairing instruction could carry: the same base register, and
\ offsets one slot apart. Which of the two is the lower address is not asked -
\ Ldp names them in address order and a rewriter would order them itself.
: ADJACENT-SLOTS? ( n n -- bool ) {: p:n q:n :}
   p FI12 q FI12 - 1 = if true exit then
   q FI12 p FI12 - 1 = ;

\ Whether the two accesses go through the same base register and one slot apart.
: SAME-BASE? ( n n -- bool ) {: p:n q:n :}
   p FRN q FRN = ;

\ Whether a pair of LOADS could be carried by one instruction that writes both
\ registers. Ldp writes two registers at once, so it needs two DIFFERENT ones -
\ and neither load may be the one that computes the other's base, which the
\ second load would then be reading through a register the first had already
\ overwritten. Asking this is what keeps the count a count of pairs that could
\ really be carried rather than of pairs that merely look adjacent.
: LOADS-INDEPENDENT? ( n n -- bool ) {: p:n q:n :}
   p FRD q FRD = if false exit then
   p FRD q FRN = if false exit then
   p FRD p FRN = if false exit then
   q FRD q FRN <> ;

public

: LDP-PAIR? ( n n -- bool ) {: p:n q:n :}
   p LDR? 0= if false exit then
   q LDR? 0= if false exit then
   p q SAME-BASE? 0= if false exit then
   p q LOADS-INDEPENDENT? 0= if false exit then
   p q ADJACENT-SLOTS? ;

\ A pair of STORES has no such restriction: a store reads both its registers and
\ writes neither, so the same register may be stored twice and the base may be
\ one of the values.
: STP-PAIR? ( n n -- bool ) {: p:n q:n :}
   p STR? 0= if false exit then
   q STR? 0= if false exit then
   p q SAME-BASE? 0= if false exit then
   p q ADJACENT-SLOTS? ;

private

\ ---- the row under the counters -----------------------------------------------
\ One routine at a time, held here so the two counting loops below can be written
\ once each and handed a predicate. A quotation cannot read the enclosing word's
\ locals, so the routine being counted cannot travel to the predicate as one;
\ it is package state for exactly that reason and for no other.

PTR-VARIABLE ROW-A
variable ROW-U

: ROW-NAME ( -- ptr u8 n )
   ROW-A @ ROW-U @ ;

public

: ROW! ( ptr u8 n -- )
   ROW-U ! ROW-A ! ;

: INSNS ( -- n )
   ROW-NAME NTAILPROBE:INSNS ;

: INSN@ ( n -- n ) {: k:n :}
   ROW-NAME k NTAILPROBE:INSN@ ;

\ How many instructions of the row satisfy a predicate.
\
\ typed-local-lint: allow-bare-local - q receives one instruction word, and a
\ local annotation cannot carry a quotation effect.
: COUNT1 ( [ n -- bool ] -- n ) {: q :}
   0
   INSNS 0 ?do
      i INSN@ q execute if 1+ then
   loop ;

\ How many ADJACENT pairs of the row satisfy a predicate. A row of fewer than
\ two instructions holds no pair, and is answered rather than looped over: the
\ bound below would otherwise run backwards.
\
\ typed-local-lint: allow-bare-local - q receives the two instruction words.
: COUNT2 ( [ n n -- bool ] -- n ) {: q :}
   0
   INSNS 2 < if exit then
   INSNS 1- 0 ?do
      i INSN@  i 1+ INSN@  q execute if 1+ then
   loop ;

\ ---- the counts themselves ----------------------------------------------------
\ One word per column of the report, so that the file that PRINTS the inventory
\ and the file that ASSERTS it are reading the same measurement rather than two
\ spellings of it.

: MULS ( -- n )
   [: MUL? ;] COUNT1 ;

: ADDS ( -- n )
   [: ADD? ;] COUNT1 ;

: SUBS ( -- n )
   [: SUB? ;] COUNT1 ;

: LDRS ( -- n )
   [: LDR? ;] COUNT1 ;

: STRS ( -- n )
   [: STR? ;] COUNT1 ;

: SHIFTS ( -- n )
   [: SHIFTI? ;] COUNT1 ;

: MADDS ( -- n )
   [: MADD-PAIR? ;] COUNT2 ;

: MADD-SAFES ( -- n )
   [: MADD-SAFE? ;] COUNT2 ;

: MSUBS ( -- n )
   [: MSUB-PAIR? ;] COUNT2 ;

: MSUB-SAFES ( -- n )
   [: MSUB-SAFE? ;] COUNT2 ;

: LDPS ( -- n )
   [: LDP-PAIR? ;] COUNT2 ;

: STPS ( -- n )
   [: STP-PAIR? ;] COUNT2 ;

: SHIFT-FOLDS ( -- n )
   [: SHIFT-FOLD-PAIR? ;] COUNT2 ;

: BITFIELDS ( -- n )
   [: BITFIELD-PAIR? ;] COUNT2 ;

;package
