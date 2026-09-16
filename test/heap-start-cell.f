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

\ The ceiling src/habu/layout.f measures for a cell a compiled routine names
\ directly: `DATA <off> LDR`/`STR` is a 12-bit immediate scaled by eight, and
\ EM-DATA-INIT names this cell with exactly that form.
$7FF8 constant LDR-CEILING

: RECORDED ( -- n ) data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

\ ---- case one: the engine published its floor, and it is the right one --------

: PUBLISHED-CASE ( -- )
   s" the engine published a heap floor at boot" T-LABEL
   RECORDED 0 <> TTRUE

   s" ... and it is exactly this engine's own DATA-START" T-LABEL
   RECORDED DATA-START T=

   s" the floor is above the reserved band's own cells, as a floor must be" T-LABEL
   RECORDED BOOT-LAYOUT:HEAP-START-CELL > TTRUE ;

\ ---- case two: the cell was legal to take -------------------------------------
\ AOT-SIG took the two cells below it out of the same unclaimed run PROT:RHI/CF
\ opened; this is the next one, and each clause is why taking it was legal.

: BAND-CASE ( -- )
   s" the cell sits one cell above the last one AOT-SIG took" T-LABEL
   BOOT-LAYOUT:HEAP-START-CELL AOT-SIG:LEN-CELL - CELL T=

   s" ... and below the lowering transaction state that ends the run" T-LABEL
   BOOT-LAYOUT:HEAP-START-CELL CELL + TXN-STATE-OFF <= TTRUE

   s" it is addressable by the `DATA <off> STR` form EM-DATA-INIT uses" T-LABEL
   BOOT-LAYOUT:HEAP-START-CELL LDR-CEILING < TTRUE

   s" it is below DATA-START, so no compiled source can reach it" T-LABEL
   BOOT-LAYOUT:HEAP-START-CELL DATA-START < TTRUE

   s" it is cell-aligned, as an atomic read of it requires" T-LABEL
   BOOT-LAYOUT:HEAP-START-CELL CELL mod 0 T=

   s" it collides with no other cell named in this band" T-LABEL
   BOOT-LAYOUT:HEAP-START-CELL PROT:RHI <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL PROT:CF <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL AOT-SIG:POOL-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL AOT-SIG:LEN-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL EVAL-TOP-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL AOT-WINDOW:T0-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL AOT-WINDOW:D0-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL AOT-WINDOW:B0-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL APP-ENTRY:XT-CELL <> TTRUE
   BOOT-LAYOUT:HEAP-START-CELL TXN-STATE-OFF <> TTRUE ;

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
   BAND-CASE
   HEAP-CASE
   T-REPORT
   s" heap-start-cell: ok" type cr ;

;package

HEAP-START-CELL-TEST:RUN
