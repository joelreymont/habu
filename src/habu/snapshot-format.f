\ Snapshot wire format and the baked loader's immutable capability.
\ Keep this separate from the address-cell storage ABI: an old donor may load
\ current source while its native startup still understands only an older
\ image format.
require src/habu/layout.f

package SNAPSHOT-FORMAT
public

\ The unnamed-code span table and blob positions are stored as DATA offsets and
\ rebased at boot; donor and loader refuse any other version (rc 74 at capture,
\ rc 80 at restore). The user heap is stored in src/habu/cell-grid.f form when
\ that is smaller than its bytes, its form in the trailer's HEAP-FIELD.
13 constant VERSION

\ The trailer cell that says how the stored DATA carries the heap, the bytes from
\ DATA-START to the exact extent. Named here, not in layout.f: a donor whose baked
\ layout predates it must still compile the writer far enough for VERIFY to
\ refuse it by name.
8 constant HEAP-FIELD
0 constant HEAP-RAW                  \ the heap's bytes, verbatim
1 constant HEAP-GRID                 \ GRID-FRAME, then the src/habu/cell-grid.f form
16 constant GRID-FRAME               \ the grid's framing: groups u64, stored group bytes u64

private
\ The capability's code entry is a raw execution token; this view states the
\ effect every format version gives it.
CAST: VERSION-XT ( n -- [ -- n ] )

: TEXT-BASE ( -- n )
   data-base RBASE-CELL + @ ;

: TEXT-SIZE ( -- n )
   data-base TEXT-SIZE-CELL + @ ;

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
