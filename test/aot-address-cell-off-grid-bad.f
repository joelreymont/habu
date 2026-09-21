\ A declared cell inside a real DATA window must lie on the window's cell grid.
\
\ THE FIELD IS CREATED INSIDE THE WINDOW and marked four bytes into it, with a
\ second field behind it so the marked cell ends well below the window's upper
\ edge: this case is about the grid and not about an edge, so the straddle
\ refusal must not be the one that fires (src/habu/aot-capture.f classifies the
\ edges first for exactly that reason). A capture describes its span as one
\ presence bit and one varint per whole cell (src/habu/aot-decl.f package
\ AOT-WINDOW), so a cell beginning part way into one has no bit that carries it.
\
\ THE PRELUDE MARK IS THE WINDOW'S OWN START, so the band is empty: this case is
\ about the cell's offset and not about which process owns the address.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

AOT-ARM:WINDOW-OPEN
package AOT-XTCELL-OFF-GRID
create FIELD 8 allot
FIELD 4 + ptr-cell-mark
create TAIL 16 allot
;package
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-OFF-GRID-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
