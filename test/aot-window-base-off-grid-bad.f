\ A captured DATA window must begin on a whole cell.
\
\ THE FIELD ALLOTS ONE BYTE BEFORE THE WINDOW OPENS, so the dictionary pointer -
\ and with it the window's DATA base - stands one byte above the cell `create`
\ rounded it to, and the window's own cells would be one byte out of step with
\ every other window's. A capture describes its span as one presence bit and one
\ varint per whole cell (src/habu/aot-decl.f package AOT-WINDOW), and a merge
\ splices one window's cells into another's bitmap (src/habu/aot-file.f
\ PLACE-WDATA), so the grid is shared and a base off it ends the build.
\
\ THE FIELD INSIDE THE WINDOW is what a real capture always has; the base is
\ refused before anything is scanned, so its size does not matter.
\
\ THE PRELUDE MARK IS THE WINDOW'S OWN START, so the band is empty: this case is
\ about the window's base and not about which process owns the address.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-WINDOW-BASE-OFF-GRID
public

create EDGE 1 allot

;package

AOT-ARM:WINDOW-OPEN
package AOT-WINDOW-BASE-OFF-GRID-FIELD
create FIELD 16 allot
;package
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-WINDOW-BASE-OFF-GRID-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
