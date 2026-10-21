\ A captured DATA window begins on a whole cell, whatever DP the opener meets.
\
\ THE FIELD ALLOTS ONE BYTE BEFORE THE WINDOW OPENS, so the dictionary pointer
\ stands one byte above the cell `create` rounded it to. A capture describes its
\ span as one presence bit and one varint per whole cell (src/habu/aot-decl.f
\ package AOT-WINDOW), and a merge splices one window's cells into another's
\ bitmap (src/habu/aot-file.f PLACE-WDATA), so the grid is shared: the opener
\ (src/habu/aot-arm.f LATCH-OPEN) aligns before it latches the base, and the
\ capture of a window opened here must complete on that base. Without the
\ alignment the base is one byte off the grid and this dies before capturing.
\
\ THE PRELUDE MARK IS THE WINDOW'S OWN START, so the band is empty: this case is
\ about the window's base and not about which process owns the address.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-WINDOW-BASE-GRID
public

\ Reads the latched base after the window closed, from a body defined below it.
: ?GRID ( -- )
   AOT-ARM:D0 @ AOT-WINDOW:CELL-BYTES mod 0<> if
      s" aot-window-base-grid: the window base is off the cell grid" 74 die then ;

create EDGE 1 allot

;package

AOT-ARM:WINDOW-OPEN
package AOT-WINDOW-BASE-GRID-FIELD
create SLOT 16 allot
;package
AOT-ARM:WINDOW-CLOSE
AOT-WINDOW-BASE-GRID:?GRID
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" AOT-WINDOW-BASE-GRID-CAPTURED" type cr
