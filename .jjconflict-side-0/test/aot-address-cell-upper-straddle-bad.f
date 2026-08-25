\ A declared cell beginning inside a real DATA window may not overrun its upper edge.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

AOT-ARM:WINDOW-OPEN
package AOT-XTCELL-UPPER-STRADDLE
create EDGE 8 allot
EDGE 4 + ptr-cell-mark
;package
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-UPPER-STRADDLE-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
