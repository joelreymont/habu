\ A captured DATA window begins on a whole cell, whatever DP the opener meets.

\ The prelude mark is the window's own start, so the band is empty.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-WINDOW-BASE-GRID
public

\ A capture describes its span as one presence bit and one varint per whole cell
\ (src/habu/aot-decl.f package AOT-WINDOW), and a merge splices one window's
\ cells into another's bitmap (src/habu/aot-file.f PLACE-WDATA), so the opener
\ (src/habu/aot-arm.f LATCH-OPEN) aligns before it latches the base; without
\ that this dies before capturing.
\ Reads the latched base after the window closed, from a body defined below it.
: ?GRID ( -- )
   AOT-ARM:D0 @ AOT-WINDOW:CELL-BYTES mod 0<> if
      s" aot-window-base-grid: the window base is off the cell grid" 74 die then ;

\ The field allots one byte before the window opens, so DP stands one byte above
\ the cell `create` rounded it to.
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
