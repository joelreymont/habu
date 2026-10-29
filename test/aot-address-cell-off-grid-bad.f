\ A declared cell inside a real DATA window must lie on the window's cell grid.

\ The prelude mark is the window's own start, so the band is empty.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

AOT-ARM:WINDOW-OPEN
\ The field sits four bytes into the window, with a second field behind it so
\ the marked cell ends well below the upper edge: the straddle refusal must not
\ fire (src/habu/aot-capture.f classifies the edges first). A capture describes
\ its span as one presence bit and one varint per whole cell
\ (src/habu/aot-decl.f package AOT-WINDOW), so a cell starting part way into one
\ has no bit.
package AOT-XTCELL-OFF-GRID
create SLOT 8 allot
SLOT 4 + ptr-cell-mark
create TAIL 16 allot
;package
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-OFF-GRID-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
