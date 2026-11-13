\ script.f - the pinned Unicode Script property of a scalar.
\
\ STORAGE CLASS. PROCESS-WIDE read-only table (src/lib/unicode/script-data.f,
\ generated, which also declares the script family); any task may query.

require src/lib/unicode/script-data.f

package UNICODE-SCRIPT

private

\ The table's tags are the family's tags, which the generator and verifier pin.
CAST: >SCRIPT ( n -- script )

\ The first cell of entry idx; its last scalar and tag follow.
: ENTRY ( n -- ptr n )
   3 * RANGE-TABLE ;

\ How many entries have a first cell at most cp.
: UPPER-BOUND ( n -- n ) {: cp:n :}
   0 RANGE-COUNT
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      cp mid ENTRY @ < if lo mid else mid 1+ hi then
   repeat
   drop ;

public

\ Unicode Script per Scripts.txt; unknown where no range covers n, which
\ includes surrogates and every n outside 0..$10FFFF.
: SCRIPT ( n -- script ) {: cp:n :}
   cp UPPER-BOUND {: at:n :}
   at 0= if construct script unknown exit then
   at 1- ENTRY {: entry:ptr :}
   cp entry cell+ @ > if construct script unknown exit then
   entry 2 cells + @ >SCRIPT ;

;package
