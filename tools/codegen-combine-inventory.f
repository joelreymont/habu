\ codegen-combine-inventory.f - counts the pairs of emitted ARM64 instructions
\ one three-source, shifted or folded form would replace, in the code published
\ routines hold. It reads emitted code through tools/codegen-tail-probe.f's walk:
\ inlining, materialised constants and register allocation decide which pairs
\ exist. Counts read finished code, so they undercount what a combine before
\ register allocation can fuse. test/compiler/native-fold.f asserts them.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/arch/arm64/asm.f
require tools/codegen-tail-probe.f

package NCOMBINV
using A64ASM

private

\ Each classifier re-encodes the fields it read with src/arch/arm64/asm.f's
\ encoders and compares the whole word, so the layout has one authority. On
\ Darwin those encoders end the process for x18, so each classifier screens its
\ fields first; Linux re-encodes it normally.
18 constant RESERVED-REG

$1F constant REG-MASK              \ a register operand is a five-bit field
$FFF constant IMM12-MASK           \ the unsigned load/store offset field
1 8 lshift constant SIMM9-SIGN     \ the unscaled offset's sign bit, and the span
1 9 lshift constant SIMM9-SPAN     \ a set one subtracts to sign-extend it
8 constant SLOT-BYTES              \ one 64-bit slot: the load/store scale, and
                                   \ the distance a pairable pair's offsets differ by
31 constant ZERO-REG               \ the addend field's value that makes a
                                   \ multiply-add a plain multiply

public

\ Public: tools/codegen-loop-inventory.f decodes register fields with these,
\ never a second copy.
: FRD ( n -- n ) {: w:n :}
   w REG-MASK and ;

: FRN ( n -- n ) {: w:n :}
   w 5 rshift REG-MASK and ;

: FRM ( n -- n ) {: w:n :}
   w 16 rshift REG-MASK and ;

: FI12 ( n -- n ) {: w:n :}
   w 10 rshift IMM12-MASK and ;

\ Signed: an access below the base register has no spelling in the unsigned
\ forms.
: FSIMM9 ( n -- n ) {: w:n :}
   w SIMM9-SHIFT rshift SIMM9-MASK and {: v:n :}
   v SIMM9-SIGN and 0<> if v SIMM9-SPAN - exit then
   v ;

private

: FRA ( n -- n ) {: w:n :}
   w 10 rshift REG-MASK and ;

$3F constant SHIFT-MASK            \ a shift amount for a 64-bit register
$1FFF constant NIS-MASK            \ the packed N:immr:imms of a logical immediate
63 constant SHIFT-TOP              \ the largest shift, and the `imms` a zero shift leaves

\ src/arch/arm64/asm.f writes LSL as a bitfield move whose `imms` is 63 - sh, so
\ the amount is read back out of that field.
: FSHL ( n -- n ) {: w:n :}
   SHIFT-TOP  w 10 rshift SHIFT-MASK and  - ;

: FSHR ( n -- n ) {: w:n :}
   w 16 rshift SHIFT-MASK and ;

: FNIS ( n -- n ) {: w:n :}
   w 10 rshift NIS-MASK and ;

: ENCODABLE? ( n -- bool ) {: r:n :}
   r RESERVED-REG <> if true exit then
   HB-TARGET-KNOWN? 0= if E-CTGT-ABI throw then
   HB-TARGET-LINUX? ;

: R3-OK? ( n -- bool ) {: w:n :}
   w FRD ENCODABLE?
   w FRN ENCODABLE? and
   w FRM ENCODABLE? and ;

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

\ The multiply-add shares its opcode with the plain multiply and is told apart by
\ the addend field: a zero-register addend reproduces as a multiply and answers
\ false.
: MADD? ( n -- bool ) {: w:n :}
   w R3-OK? 0= if false exit then
   w FRA ENCODABLE? 0= if false exit then
   w FRA ZERO-REG = if false exit then
   w FRD w FRN w FRM w FRA ENC-MADD w = ;

: SUB? ( n -- bool ) {: w:n :}
   w R3-OK? 0= if false exit then
   w FRD w FRN w FRM ENC-SUB w = ;

\ A shifted immediate, which these encoders do not emit, reproduces as something
\ else and answers false.
: ADDI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FI12 ENC-ADDI w = ;

: SUBI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FI12 ENC-SUBI w = ;

\ Every comparison this chain emits opens with one, in the flag-materialising
\ trio and the fused branch alike. The register form writes the zero register, so
\ only its two sources are screened; a Subs writing a real register reproduces as
\ something else and answers false.
: CMP? ( n -- bool ) {: w:n :}
   w FRN ENCODABLE? 0= if false exit then
   w FRM ENCODABLE? 0= if false exit then
   w FRN w FRM ENC-CMP w = ;

: CMPI? ( n -- bool ) {: w:n :}
   w FRN ENCODABLE? 0= if false exit then
   w FRN w FI12 ENC-CMPI w = ;

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

\ The nis field is compared as the instruction holds it; the mask is never
\ reconstructed.
: ORRI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FNIS ENC-ORRI w = ;

: EORI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w FRD w FRN w FNIS ENC-EORI w = ;

\ ARM64's shifted-register arithmetic holds a left shift and both right shifts,
\ so all three count.
: SHIFTI? ( n -- bool ) {: w:n :}
   w LSLI? if true exit then
   w LSRI? if true exit then
   w ASRI? ;

private

\ ---- the pair predicates ------------------------------------------------------

: READS-PRODUCT? ( n n -- bool ) {: m:n u:n :}
   u FRN m FRD =
   u FRM m FRD = or ;

\ The `safe` count: the next instruction overwrites the product, so no later
\ reader can need it and a rewriter may drop the multiply. READS-PRODUCT? alone
\ (`pairs`) is the ceiling an adjacency peephole could fuse; the gap is liveness,
\ a one-use property of the value in src/compiler/native/a64ir.f's SSA.
: KILLS-PRODUCT? ( n n -- bool ) {: m:n u:n :}
   u FRD m FRD = ;

public

: MADD-PAIR? ( n n -- bool ) {: m:n a:n :}
   m MUL? 0= if false exit then
   a ADD? 0= if false exit then
   m a READS-PRODUCT? ;

\ Only the subtrahend position counts: Msub computes ra - rn*rm, and a
\ subtraction that takes something away from the product is not that form.
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

\ Counted to size the pattern: neither the model nor src/arch/arm64/asm.f carries
\ a shifted-operand add or subtract.
: SHIFT-FOLD-PAIR? ( n n -- bool ) {: s:n u:n :}
   s SHIFTI? 0= if false exit then
   u ADD? u SUB? or 0= if false exit then
   s u READS-PRODUCT? ;

\ Counted on the same terms: no bitfield-extract form is modelled in this tree.
: BITFIELD-PAIR? ( n n -- bool ) {: s:n m:n :}
   s LSRI? 0= if false exit then
   m ANDI? 0= if false exit then
   m FRN s FRD = ;

private

\ Address order is not asked: Ldp names its registers in address order and a
\ rewriter orders them itself.
: ADJACENT-SLOTS? ( n n -- bool ) {: p:n q:n :}
   p FI12 q FI12 - 1 = if true exit then
   q FI12 p FI12 - 1 = ;

: SAME-BASE? ( n n -- bool ) {: p:n q:n :}
   p FRN q FRN = ;

\ Ldp writes two registers at once, so they must differ, and neither load may be
\ the one that computes the other's base.
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
\ The routine being counted is package state because a quotation cannot read the
\ enclosing word's locals.

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

\ The local annotation cannot carry a quotation effect.
: COUNT1 ( [ n -- bool ] -- n ) {: q :}
   0
   INSNS 0 ?do
      i INSN@ q execute if 1+ then
   loop ;

\ A row of fewer than two instructions is answered before the loop, whose bound
\ would otherwise run backwards.
: COUNT2 ( [ n n -- bool ] -- n ) {: q :}
   0
   INSNS 2 < if exit then
   INSNS 1- 0 ?do
      i INSN@  i 1+ INSN@  q execute if 1+ then
   loop ;

\ ---- the counts themselves ----------------------------------------------------
\ One word per report column, so the printer and test/compiler/native-fold.f read
\ one measurement.

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

\ What a combining pass put there, as opposed to the pairs above, which are what
\ one could still take.
: MADD-INSNS ( -- n )
   [: MADD? ;] COUNT1 ;

\ Folded constants the row holds, on the same terms.
: ADDI-INSNS ( -- n )
   [: ADDI? ;] COUNT1 ;

: SUBI-INSNS ( -- n )
   [: SUBI? ;] COUNT1 ;

: ANDI-INSNS ( -- n )
   [: ANDI? ;] COUNT1 ;

: ORRI-INSNS ( -- n )
   [: ORRI? ;] COUNT1 ;

: EORI-INSNS ( -- n )
   [: EORI? ;] COUNT1 ;

\ How many comparisons the row makes against a register, and how many against a
\ number the instruction carries. The pair is read together: a fold that took a
\ comparison off the register path has to show up as one fewer of the first and
\ one more of the second, and a row where only the second moved gained a
\ comparison rather than folding one.
: CMP-INSNS ( -- n )
   [: CMP? ;] COUNT1 ;

: CMPI-INSNS ( -- n )
   [: CMPI? ;] COUNT1 ;

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

;using
;package
