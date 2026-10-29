\ A declared engine cell past 2^31 bytes into DATA may not become a fixed row.
\
\ A fixed row stores the cell's DATA offset in a 32-bit field: the low 31 bits
\ are the offset, the top bit the window tag, and DATA spans more than 2^31
\ bytes. A truncated offset would hand the seed a window cell at the residue, so
\ the capture refuses (src/habu/aot-capture.f ACAP-XTCELL-LOC).

\ FAR is a PERSISTED-PTR-VARIABLE 2 GiB past the engine's DATA, outside the
\ window, and becomes a row only because it holds a window target: without the
\ store the program captures.

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
