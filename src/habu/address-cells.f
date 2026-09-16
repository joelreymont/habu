\ The engine owns address-cell declarations. Readers acquire a fresh span:
\ appending a row may move its backing storage, but never changes a row's index.
require src/habu/layout.f

package ADDRESS-CELLS
public

1 constant ABI-VERSION
9 constant SNAPSHOT-VERSION
\ Process mutex in fixed image DATA, outside the header and row backing. The
\ MATCH stack ends before $1A0; $1A0 remains the seal fixture's poke cell, and
\ CMFAM starts at $1B0. Task USER storage is USER-BAND, far above at $5300.
$1A8 constant LOCK-CELL
\ Derived process storage, never an address declaration or part of the row
\ schema. $36C8 is the single unused cell between BPA ($36C0) and the eight
\ breakpoint records ($36D0..$37D0), below FFI/task USER and outside JIT arrays.
\ Existing storage-v1 engines leave it zero, so new source can still use them.
$36C8 constant INDEX-CELL
0 constant INDEX-SLOTS
8 constant INDEX-COUNT
16 constant INDEX-HEADER
$7FFFFFFFFFFFFFFF INDEX-HEADER - CELL / constant INDEX-MAX-SLOTS
\ The high six bytes spell HBADDR; the low 16 bits identify header schema 1.
$4842414444520001 constant MAGIC
8 constant MAGIC-FIELD
16 constant BASE-FIELD
24 constant CAP-FIELD
32 constant MODE-FIELD
40 constant HEADER-BYTES
SNAP-RELOC:XTCELL-N-CELL HEADER-BYTES + constant BOOT-OFF
SNAP-RELOC:XTCELL-END BOOT-OFF - CELL / constant BOOT-CAP
$7FFFFFFFFFFFFFFF CELL / constant MAX-ROWS

private
TRUSTED: VERSION-XT ( n -- [ -- n ] ) ;
TRUSTED: TEXT-BASE ( -- n ) data-base RBASE-CELL + @ ;
TRUSTED: TEXT-SIZE ( -- n )
   TEXT-BASE CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - ;

public
\ Wordlist zero contains the immutable engine primitive. Reject a source word
\ shadowing it: only executable engine text may supply the discriminator.
\ Recovery returns zero; old native hosts have no primitive at all.
: CURRENT? ( -- bool )
   s" addr-cells-abi" 0 search-wl {: xt:n :}
   xt 0= if 0 0 <> exit then
   xt TEXT-BASE < xt TEXT-BASE - TEXT-SIZE >= or if
      s" address-cells: version probe is not an engine primitive" 96 die
   then
   xt VERSION-XT execute {: version:n :}
   version 0= if 0 0 <> exit then
   version ABI-VERSION <> if
      s" address-cells: unsupported engine storage version" 96 die
   then
   0 0= ;
: SNAPSHOT-FORMAT ( -- n ) CURRENT? if SNAPSHOT-VERSION else 8 then ;

private

: REFUSE ( -- ) s" address-cells: invalid storage header" 96 die ;
: SHAPE ( n n -- ) {: count:n cap:n :}
   cap 0 <= cap MAX-ROWS > or count 0 < or if REFUSE then
   count cap > if REFUSE then ;

TRUSTED: HEADER ( -- ptr n ) data-base SNAP-RELOC:XTCELL-N-CELL + ;
TRUSTED: N>ROWS ( n -- ptr n ) ;
TRUSTED: ROWS>N ( ptr n -- n ) ;
TRUSTED: DATA-ROWS ( n -- ptr n ) data-base + ;

: LOCK-ADDR ( -- ptr n ) data-base LOCK-CELL + ;
: INDEX-ADDR ( -- ptr n ) data-base INDEX-CELL + ;
: LOCK ( -- ) begin 0 1 LOCK-ADDR atomic-cas 0= until ;
: UNLOCK ( -- ) 0 LOCK-ADDR atomic! ;

\ Main-owner preparation is quiescent, as it was before the index. Hold the
\ registrar mutex through release AND the following row/header mutation, so a
\ marker cannot rebuild the derived view between invalidation and compaction.
: INDEX-RELEASE ( -- )
   INDEX-ADDR @ dup 0= if drop exit then {: address:n :}
   address 0 < address 7 and 0<> or
   address $7FFFFFFFFFFFFFFF INDEX-HEADER - > or if REFUSE then
   address N>ROWS INDEX-SLOTS + @ {: slots:n :}
   slots 2 < slots INDEX-MAX-SLOTS > or if REFUSE then
   slots slots 1- and 0<> if REFUSE then
   slots cells INDEX-HEADER + {: bytes:n :}
   bytes $7FFFFFFFFFFFFFFF address - > if REFUSE then
   address N>ROWS bytes munmap 0<> if
      s" address-cells: cannot release index" 96 die then
   0 INDEX-ADDR ! ;

\ Subtraction precedes addition/multiplication, including on malformed input.
: WITHIN ( n n n -- ) {: off:n cap:n bytes:n :}
   off 0 < off bytes > or if REFUSE then
   cap bytes off - CELL / > if REFUSE then ;

: LIVE-HEADER ( -- ptr n n n )
   HEADER {: h:ptr :}
   h MAGIC-FIELD + @ MAGIC <> if REFUSE then
   h @ h CAP-FIELD + @ SHAPE
   h BASE-FIELD + @ {: base:n :}
   h CAP-FIELD + @ {: cap:n :}
   h MODE-FIELD + @ {: mode:n :}
   mode 0= if
      base cap here data-base - WITHIN
      base DATA-ROWS cap 0 exit
   then
   mode 1 <> base 0 <= or base 7 and 0 <> or if REFUSE then
   cap $7FFFFFFFFFFFFFFF base - CELL / > if REFUSE then
   base N>ROWS cap 1 ;

\ A source rewind can retire the DATA backing of a restored vector. Secure
\ outside storage first, including when the cut crosses its unused capacity.
: KEEP-STORAGE ( n -- ) {: floor:n :}
   LIVE-HEADER {: old:ptr cap:n mode:n :}
   mode 1 = if exit then
   old data-base - {: off:n :}
   off floor <= if cap floor off - CELL / <= if exit then then
   cap cells map-anon 0 <> if
      drop s" address-cells: storage allocation failed" 96 die then {: fresh:ptr :}
   cap 0 ?do old i cells + @ fresh i cells + ! loop
   fresh ROWS>N HEADER BASE-FIELD + !
   1 HEADER MODE-FIELD + ! ;

public

: LIVE-SPAN ( -- ptr n n )
   CURRENT? if LIVE-HEADER 2drop HEADER @ exit then
   HEADER @ SNAP-RELOC:XTCELL-CAP SHAPE
   SNAP-RELOC:XTCELL-ROWS-OFF DATA-ROWS HEADER @ ;

: ROW@ ( n -- n ) {: row:n :}
   LIVE-SPAN {: base:ptr count:n :}
   row 0 < row count >= or if REFUSE then
   base row cells + @ ;

private

: KEEP-ROWS ( n -- ) {: floor:n :}
   LIVE-SPAN {: base:ptr count:n :}
   0 count 0 ?do
      base i cells + @ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and floor < if
         dup cells base + row swap ! 1+
      then
   loop
   HEADER ! ;

: PERSIST-ROWS ( -- )
   LIVE-HEADER {: old:ptr cap:n mode:n :}
   mode 0= if exit then
   cap cells {: bytes:n :}
   here data-base - cap DATA-SIZE WITHIN
   here {: fresh:ptr :}
   bytes allot
   cap 0 ?do old i cells + @ fresh i cells + cell-view ! loop
   old bytes munmap 0 <> if
      bytes negate allot s" address-cells: cannot release storage" 96 die
   then
   fresh data-base - HEADER BASE-FIELD + !
   0 HEADER MODE-FIELD + ! ;

: KEEP-LOCKED ( n -- )
   INDEX-RELEASE dup KEEP-STORAGE KEEP-ROWS ;

\ allot and protected memory sinks can throw through an active evaluator.
\ Cleanup must release the mutex before such a caller resumes after catch.
: WITH-LOCK ( R [ R -- S ] -- S )
   LOCK [: UNLOCK ;] finally ;

public

\ Native-build owns the explicit lifetime cut through its retired source heap.
\ Preserve registration order and invalidate even when the count is unchanged.
: KEEP-BELOW ( n -- )
   CURRENT? if
      [: KEEP-LOCKED ;] WITH-LOCK
   else KEEP-ROWS then ;

\ Called outside the registrar, before snapshot DATA length is frozen. DATA
\ allocation inside ptr-cell-mark would split `here ptr-cell-mark 0 ,`.
\ An already DATA-backed vector still releases its process-owned index.
: PERSIST ( -- )
   CURRENT? 0= if exit then
   [: INDEX-RELEASE PERSIST-ROWS ;] WITH-LOCK ;

\ Strict v9 reader for a complete snapshot DATA copy. Legacy admission is a
\ separate caller decision; malformed v9 headers never take that branch.
: DATA-SPAN ( ptr u8 n -- ptr n n ) {: data:ptr bytes:n :}
   bytes SNAP-RELOC:XTCELL-N-CELL HEADER-BYTES + < if REFUSE then
   data SNAP-RELOC:XTCELL-N-CELL + cell-view {: h:ptr :}
   h MAGIC-FIELD + @ MAGIC <> if REFUSE then
   h MODE-FIELD + @ 0 <> if REFUSE then
   h @ h CAP-FIELD + @ SHAPE
   h BASE-FIELD + @ h CAP-FIELD + @ bytes WITHIN
   data h BASE-FIELD + @ + cell-view h @ ;

;package
