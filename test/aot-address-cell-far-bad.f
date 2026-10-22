\ A declared engine cell past 2^31 bytes into DATA may not become a fixed row.
\
\ A fixed row stores the cell's DATA offset in a 32-bit field whose readers take
\ the low 31 bits as the offset and the top bit as the window tag, and DATA spans
\ more than 2^31 bytes. Truncating FAR's offset would hand the seed a window cell
\ at the offset's residue, so the capture refuses (src/habu/aot-capture.f
\ ACAP-XTCELL-LOC).
\
\ FAR is a PERSISTED-PTR-VARIABLE allotted 2 GiB past the engine's DATA, outside
\ the window, and becomes a row only because it holds a window target: the same
\ program without the store captures.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

$80000000 allot
package AOT-XTCELL-FAR-PRE
public

PERSISTED-PTR-VARIABLE FAR

;package

AOT-ARM:WINDOW-OPEN
package AOT-XTCELL-FAR
public

create INSIDE 8 allot

;package
AOT-ARM:WINDOW-CLOSE
AOT-XTCELL-FAR:INSIDE AOT-XTCELL-FAR-PRE:FAR !
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-FAR-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
