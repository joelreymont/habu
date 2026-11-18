split-str
\ Snapshot wire format and the baked loader's immutable capability.
\ Keep this separate from the address-cell storage ABI: an old donor may load
\ current source while its native startup still understands only an older
\ image format.
require src/habu/layout.f
require src/habu/wl.f

package SNAPSHOT-FORMAT
public

\ The unnamed-code span table and blob positions are stored as DATA offsets and
\ rebased at boot; the loader rejects any other version (rc 80 at restore).
13 constant VERSION

\ The trailer cell that says how the stored DATA carries the heap, the bytes from
\ DATA-START to the exact extent. Named here, not in layout.f: a donor whose baked
\ layout predates it must still compile the writer.
8 constant HEAP-FIELD
0 constant HEAP-RAW                  \ the heap's bytes, verbatim
1 constant HEAP-GRID                 \ GRID-FRAME, then the src/habu/cell-grid.f form
16 constant GRID-FRAME               \ the grid's framing: groups u64, stored group bytes u64

;package
