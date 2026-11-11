\ regalloc.f - the arm64 register pool's constants. VRPACK and VRPACK2 are the
\ pool table, one byte per slot with idx 0 low.

require src/habu/regalloc-abi.f

\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
using REGALLOC-ABI

package REGALLOC
public

private
$0F0E0D0C0B0A09 constant VRPACK     \ x9..x15 (byte per slot, idx 0 low)
$0807061615181719 constant VRPACK2  \ overflow: x25,x23,x24,x21,x22,x6,x7,x8
: PK# ( n -- n ) 0 BEGIN over 0= 0= WHILE 1 + swap 8 rshift swap REPEAT nip ;
VRPACK PK# constant #POOL1
#POOL1 VRPACK2 PK# + constant #POOL
public
1 #POOL lshift 1 - constant VRALL

private
24 constant #FPOOL              \ d8..d31 (d0-d7 stay prim scratch; contiguous,
public
                                \ so idx+8 math replaces a table here)
1 #FPOOL lshift 1 - constant FRALL

;package

;using
;using
;using
