\ heap-start-cell.f - the engine states its own DP-heap floor, and states it
\ truthfully (dot habu-classify-captured-addr-68fcc1df).
\
\ WHAT THIS IS FOR. tools/native-build.f classifies the HOST's declared address
\ rows into the engine cells below the heap and the retired heap above it. The
\ boundary has to be the host's own floor: a tree whose reserved bands grew moves
\ DATA-START while the host that builds it does not move with it, and a build that
\ took the boundary from the source kept the host's own heap rows, which the
\ capture then refused by value (exit 74). So the engine publishes its floor at
\ boot in layout.f BOOT-LAYOUT:HEAP-START-CELL, out of the same register
\ EM-DATA-INIT gives DP, and the build reads that instead of a constant.
\
\ EVERY CLAIM BELOW IS READ OUT OF THE RUNNING ENGINE, never out of the source.
\ `require src/habu/layout.f` is a no-op in a booted engine -- its own prefix
\ already registered that path (measured 2026-09-12) -- so DATA-START and the cell
\ offset here are this engine's own, whatever tree built it. That is the point: the
\ pair has to agree in EVERY generation, including one whose host layout differed
\ from its tree's.
\
\ IT ALSO COVERS THE MIRROR, WITHOUT READING SOURCE. habu2.f compiles against the
\ host's dictionary, so it spells the offset itself (EM-LAYOUT:HEAP-START-OFF). If
\ that number ever drifts from BOOT-LAYOUT:HEAP-START-CELL, the engine stores the
\ floor at one offset and this test reads the untouched zero at the other, which is
\ the first case below.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/heap-start-cell.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/layout.f

package HEAP-START-CELL-TEST
private

: RECORDED ( -- n ) data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

\ ---- case one: the engine published its floor, and it is the right one --------

: PUBLISHED-CASE ( -- )
   s" the engine published a heap floor at boot" T-LABEL
   RECORDED 0 <> TTRUE

   s" ... and it is exactly this engine's own DATA-START" T-LABEL
   RECORDED DATA-START T=

   s" the floor is above the reserved band's own cells, as a floor must be" T-LABEL
   RECORDED BOOT-LAYOUT:HEAP-START-CELL > TTRUE ;

\ ---- case three: the floor really is where the heap begins -------------------
\ Case one would still pass if EM-DATA-INIT published a stale copy of some other
\ band's end, so tie the number to the two things the classification depends on:
\ the live heap lies above it, and the address table the build reads lies below it.

: HEAP-LEN ( -- n ) here data-base - ;

: HEAP-CASE ( -- )
   s" the boot allotted above the published floor" T-LABEL
   HEAP-LEN RECORDED > TTRUE

   s" the address table the build classifies lies wholly below the floor" T-LABEL
   SNAP-RELOC:XTCELL-END RECORDED <= TTRUE ;

public

: RUN ( -- )
   PUBLISHED-CASE
   HEAP-CASE
   T-REPORT
   s" heap-start-cell: ok" type cr ;

;package

HEAP-START-CELL-TEST:RUN
