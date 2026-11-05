\ branch.f - reading and writing the target of a `bl` instruction.
\
\ AArch64 encodes `bl` as $94000000 plus a signed 26-bit count of INSTRUCTIONS
\ from the site; A64ASM throws E-A64EMIT-REACH for a distance that is not a
\ whole instruction or that the field cannot hold.

require src/lib/prelude.f
require src/arch/arm64/asm.f

package NBR

public

4 constant INSN-BYTES

private

$FC000000 constant BL-MASK
$94000000 constant BL-OP
$14000000 constant B-OP
$03FFFFFF constant IMM26
$02000000 constant IMM26-SIGN         \ the top bit of the twenty-six-bit field
$04000000 constant IMM26-SPAN         \ two to the twenty-sixth, the wrap the sign is taken against

public

: BL? ( n -- bool )
   BL-MASK and BL-OP = ;

\ Same mask and same 26-bit field as BL?, one opcode bit apart.
: B? ( n -- bool )
   BL-MASK and B-OP = ;

private

: TARGET ( n n -- n ) {: at:n w:n :}
   w IMM26 and {: d:n :}
   d IMM26-SIGN and 0<> if d IMM26-SPAN - INSN-BYTES * at + exit then
   d INSN-BYTES * at + ;

public

: BL-TARGET ( n n -- n )
   TARGET ;

: B-TARGET ( n n -- n )
   TARGET ;

: BL-WORD ( n n -- n )
   {: at:n target:n :}
   target at - A64ASM:ENC-BL ;


: B-WORD ( n n -- n )
   {: at:n target:n :}
   target at - A64ASM:ENC-B ;

seal-package

;package
