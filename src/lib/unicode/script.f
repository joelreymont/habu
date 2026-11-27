\ script.f - the pinned Unicode Script property of a scalar.
\
\ STORAGE CLASS. PROCESS-WIDE read-only table (src/lib/unicode/script-data.f,
\ generated, which also declares the script family); any task may query.

require src/lib/unicode/table.f
require src/lib/unicode/script-data.f

package UNICODE-SCRIPT

private

\ The table's tags are the family's tags, which the generator and verifier pin.
CAST: >SCRIPT-CELL ( ^n -- ^script )

public

\ Unicode Script per Scripts.txt; unknown where no range covers n, which
\ includes surrogates and every n outside 0..$10FFFF.
: SCRIPT ( n -- script ) {: cp:n :}
   cp 0 RANGE-TABLE RANGE-COUNT 3 UNICODE-TABLE:RANGE-ROW
   0= if drop construct script unknown exit then
   2 cells + >SCRIPT-CELL @ ;

;package
