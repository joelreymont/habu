\ A declared cell beginning below a real DATA window may not leak its upper bytes.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-XTCELL-LOWER-STRADDLE

TRUSTED: MARK ( n -- ) ptr-cell-mark ;

;package

AOT-ARM:WINDOW-OPEN
AOT-ARM:D0 @ 4 - AOT-XTCELL-LOWER-STRADDLE:MARK
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-LOWER-STRADDLE-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
