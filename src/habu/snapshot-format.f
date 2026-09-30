\ Snapshot wire format and the baked loader's immutable capability.
\ Keep this separate from the address-cell storage ABI: an old donor may load
\ current source while its native startup still understands only an older
\ image format.
require src/habu/layout.f

package SNAPSHOT-FORMAT
public

\ Version 12 stores the user heap in the src/habu/cell-grid.f form whenever
\ that is smaller than the heap's bytes, and says which form in the trailer's
\ HEAP-FIELD. A version 11 loader would copy the grid stream as heap bytes, and
\ its text-base check reads the field as corruption, so the two versions refuse
\ each other (rc 80) instead.
12 constant VERSION

\ The trailer cell that says how the stored DATA carries the heap, the bytes
\ from DATA-START to the exact extent. Through version 11 this cell held the
\ canonical text base, always zero. It is named here, beside the version, and
\ not in src/habu/layout.f: its meaning changed with the format, and a donor
\ whose baked layout predates it must still compile the writer far enough for
\ VERIFY to refuse it by name.
8 constant HEAP-FIELD
0 constant HEAP-RAW                  \ the heap's bytes, verbatim
1 constant HEAP-GRID                 \ GRID-FRAME, then the src/habu/cell-grid.f form
16 constant GRID-FRAME               \ the grid's framing: groups u64, stored group bytes u64

private
TRUSTED: VERSION-XT ( n -- [ -- n ] ) ;
TRUSTED: TEXT-BASE ( -- n ) data-base RBASE-CELL + @ ;
TRUSTED: TEXT-SIZE ( -- n )
   TEXT-BASE CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - ;

public

\ Resolve wordlist zero directly: a source definition with the same spelling
\ cannot supply the loader's capability. Retained engine text is the authority.
: SUPPORTED? ( -- bool )
   s" snapshot-format" 0 search-wl {: xt:n :}
   xt 0= if false exit then
   xt TEXT-BASE < xt TEXT-BASE - TEXT-SIZE >= or if
      s" snap: format capability is not an engine primitive" 74 die
   then
   xt VERSION-XT execute VERSION = ;

: VERIFY ( -- )
   SUPPORTED? 0= if
      s" snap: donor does not support snapshot format" 74 die
   then ;

;package
