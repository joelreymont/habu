\ script.f - the pinned Unicode Script property of a scalar.
\
\ STORAGE CLASS. PROCESS-WIDE read-only table (src/lib/unicode/script-data.f,
\ generated, which also declares the script family); any task may query.

require src/lib/sort.f
require src/lib/unicode/script-data.f

package UNICODE-SCRIPT

private

\ The table's tags are the family's tags, which the generator and verifier pin.
CAST: >SCRIPT-CELL ( ^n -- ^script )

\ The first cell of entry idx; its last scalar and tag follow.
: ENTRY ( n -- ^n )
   3 * RANGE-TABLE ;

public

\ Unicode Script per Scripts.txt; unknown where no range covers n, which
\ includes surrogates and every n outside 0..$10FFFF.
: SCRIPT ( n -- script ) {: cp:n :}
   0 RANGE-COUNT [: ENTRY @ cp <= ;] SORT:PARTITION-POINT
   {: at:n :}
   at 0= if construct script unknown exit then
   at 1- ENTRY {: entry:^ :}
   cp entry cell+ @ > if construct script unknown exit then
   entry 2 cells + >SCRIPT-CELL @ ;

;package
