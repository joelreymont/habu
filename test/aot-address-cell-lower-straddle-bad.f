\ A declared cell beginning below a real DATA window may not leak its upper bytes.

\ The prelude mark is the window's own start, so the band is empty.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

\ The field is created before the window opens, so its last cell ends where the
\ window's DATA base begins, and a mark four bytes into it declares a cell whose
\ lower half is the booting engine's and upper half the window's. Nothing allots
\ between `;package` and WINDOW-OPEN, which makes EDGE+8 the window's base; were
\ that to change the cell would lie wholly below the window and the capture
\ would succeed, so the case fails on the exit code.
\ test/aot-address-cell-upper-straddle-bad.f creates the field inside the window
\ instead.
package AOT-XTCELL-LOWER-STRADDLE
public

create EDGE 8 allot

;package

AOT-ARM:WINDOW-OPEN
AOT-XTCELL-LOWER-STRADDLE:EDGE 4 + ptr-cell-mark
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-LOWER-STRADDLE-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
