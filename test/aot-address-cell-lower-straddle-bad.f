\ A declared cell beginning below a real DATA window may not leak its upper bytes.
\
\ THE FIELD IS CREATED BEFORE THE WINDOW OPENS, so its last cell ends exactly where
\ the window's DATA base begins and a mark four bytes into it declares a cell whose
\ lower half is the booting engine's and whose upper half is the window's. Nothing
\ allots between `;package` and WINDOW-OPEN, which is what makes EDGE+8 the window's
\ base; if that ever stopped holding the cell would lie wholly below the window and
\ the capture would succeed, so the case fails on the exit code rather than falling
\ silent. Its upper sibling is the same construction with the field created INSIDE
\ the window, so the pair differs only in which edge the cell crosses.
\
\ THE PRELUDE MARK IS THE WINDOW'S OWN START, so the band is empty: this case is
\ about the cell's extent and not about which process owns the address.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

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
