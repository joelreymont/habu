\ The window DATA cell value's varint codec at each width boundary.
\ src/habu/aot-decl.f package AOT-WINDOW is the authority for the grammar; the
\ two emitted ARM64 decoders (habu2.f APPLY-CELLS for the baked window,
\ aot-link-arm.f EMIT-VGET for a stripped image's own DATA) are exercised by
\ every build.
require src/lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

package AOT-CELL-VALUE-TEST
using AOT-WINDOW

create VBUF 16 allot

\ Write, then read back: the width the encoder charges, the width it wrote, the
\ width the reader spent and the value it answers are one number and one value.
: ROUND ( n n -- ) {: v:n want:n :}
   v CELL-VLEN want T=
   v VBUF CELL-V! want T=
   VBUF 16 CELL-V@ {: got:n w:n :}
   w want T=
   got v T= ;

: WIDTHS ( -- )
   s" seven bits travel in one byte" T-LABEL
   0 1 ROUND
   s" seven bits travel in one byte" T-LABEL
   1 1 ROUND
   s" seven bits travel in one byte" T-LABEL
   $7F 1 ROUND
   s" the eighth bit opens a second group" T-LABEL
   $80 2 ROUND
   s" fourteen bits still fit two groups" T-LABEL
   $3FFF 2 ROUND
   s" the fifteenth bit opens a third group" T-LABEL
   $4000 3 ROUND
   s" twenty-eight bits still fit four groups" T-LABEL
   $FFFFFFF 4 ROUND
   s" a full u32 spends five groups" T-LABEL
   $FFFFFFFF 5 ROUND
   s" fifty-six bits fit eight groups" T-LABEL
   $FFFFFFFFFFFFFF 8 ROUND
   s" the fifty-seventh bit opens a ninth group" T-LABEL
   $100000000000000 9 ROUND
   s" sixty-three bits still fit nine groups" T-LABEL
   $7FFFFFFFFFFFFFFF 9 ROUND
   s" a cell with its top bit set is the widest value" T-LABEL
   -1 VMAX ROUND
   s" ... and so is the cell that holds only that bit" T-LABEL
   $8000000000000000 VMAX ROUND ;

: RUN ( -- )
   T-RESET
   WIDTHS
   T-REPORT ;

RUN
;using
;package
