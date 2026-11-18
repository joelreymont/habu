split-str
\ The engine owns address-cell declarations. Readers acquire a fresh span:
\ appending a row may move its backing storage, but never changes a row's index.

package ADDRESS-CELLS
public

\ Process mutex in fixed image DATA, outside the header and row backing. The
\ MATCH stack ends before $1A0.
$1A8 constant LOCK-CELL
\ Derived process storage, never an address declaration or part of the row
\ schema: the one unused cell between BPTRAMP ($36C0) and the breakpoint records
\ ($36D0..$37D0).
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

: REFUSE ( -- ) s" address-cells: invalid storage header" EXIT-STATUS:INTERNAL die ;
: SHAPE ( n n -- ) {: count:n cap:n :}
   cap 0 <= cap MAX-ROWS > or count 0 < or if REFUSE then
   count cap > if REFUSE then ;

: HEADER ( -- ^n ) data-base SNAP-RELOC:XTCELL-N-CELL + ;

\ The header's base and the index cell hold an outside mapping's address as an integer.
CAST: N>ROWS ( n -- ^n )
: DATA-ROWS ( n -- ^n ) data-base + ;

: LOCK-ADDR ( -- ^n ) data-base LOCK-CELL + ;
: INDEX-ADDR ( -- ^n ) data-base INDEX-CELL + ;
: LOCK ( -- ) begin 0 1 LOCK-ADDR atomic-cas 0= until ;
: UNLOCK ( -- ) 0 LOCK-ADDR atomic! ;

public
\ Unmaps the index the cell holds and zeroes the cell. On a table markers reach, the caller holds
\ its mutex through this AND the row/header mutation after, so no marker rebuilds the view between.
: INDEX-RELEASE ( ^n -- )
   {: held:^ :}
   held @ dup 0= if drop exit then {: address:n :}
   address 0 < address 7 and 0 <> or
   address $7FFFFFFFFFFFFFFF INDEX-HEADER - > or if REFUSE then
   address N>ROWS INDEX-SLOTS + @ {: slots:n :}
   slots 2 < slots INDEX-MAX-SLOTS > or if REFUSE then
   slots slots 1- and 0 <> if REFUSE then
   slots cells INDEX-HEADER + {: bytes:n :}
   bytes $7FFFFFFFFFFFFFFF address - > if REFUSE then
   address N>ROWS bytes munmap 0 <> if E-MEM-UNMAP throw then
   0 held ! ;
private

\ Subtraction precedes addition/multiplication, including on malformed input.
: WITHIN ( n n n -- ) {: off:n cap:n bytes:n :}
   off 0 < off bytes > or if REFUSE then
   cap bytes off - CELL / > if REFUSE then ;

: LIVE-HEADER ( -- ^n n n )
   HEADER {: h:^ :}
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

public

: LIVE-SPAN ( -- ^n n )
   LIVE-HEADER 2drop HEADER @ ;

: ROW@ ( n -- n ) {: row:n :}
   LIVE-SPAN {: base:^ count:n :}
   row 0 < row count >= or if REFUSE then
   base row cells + @ ;

private

\ Mode-0 rows live only in the boot area, below every DP a rewind returns to,
\ so a rewind keeps them in place.
: KEEP-ROWS ( n -- ) {: floor:n :}
   LIVE-SPAN {: base:^ count:n :}
   0 count 0 ?do
      base i cells + @ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and floor < if
         dup cells base + row swap ! 1+
      then
   loop
   HEADER ! ;

: KEEP-LOCKED ( n -- )
   INDEX-ADDR INDEX-RELEASE KEEP-ROWS ;

\ allot and protected memory sinks can throw through an active evaluator.
\ Cleanup must release the mutex before such a caller resumes after catch.
: WITH-LOCK ( R [ R -- S ] -- S )
   LOCK [: UNLOCK ;] finally ;

public

\ PROC-FORK:RAW runs its fork with the registrar held. The registrar unmaps old
\ rows and index before publishing their replacements, and commits an index
\ slot before its counts (habu2.f EMIT-MARK): a child forked mid-update could
\ neither wait for that task nor resume it. Held, nobody is inside it when the
\ process is copied; parent and child each free their own copy. The lock is
\ found through data-base, so a fork made on a task holds the task-local
\ address-cell lock, not the registrar's.
: ACROSS-FORK ( [ -- n ] -- n )
   WITH-LOCK ;

\ Native-build owns the explicit lifetime cut through its retired source heap.
\ Preserve registration order and invalidate even when the count is unchanged.
: KEEP-BELOW ( n -- )
   [: KEEP-LOCKED ;] WITH-LOCK ;

\ Drop every row in [first, limit), code and data alike; rows outside stay.
private
: REMOVE-LOCKED ( n n -- ) {: first:n limit:n :}
   INDEX-ADDR INDEX-RELEASE
   LIVE-SPAN {: rows:^ count:n :}
   0
   count 0 ?do
      rows i cells + @ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and {: off:n :}
      off first >= off limit CELL - <= and 0= if
         dup cells rows + row swap ! 1+
      then
   loop
   HEADER ! ;

public

: REMOVE-SPAN ( n n -- ) {: first:n bytes:n :}
   first 0 < bytes 0 < or if REFUSE then
   first here data-base - > if REFUSE then
   bytes here data-base - first - > if REFUSE then
   first first bytes + [: REMOVE-LOCKED ;] WITH-LOCK ;

\ Reads a complete snapshot DATA copy.
: DATA-SPAN ( str -- ^n n ) {: data:^ bytes:n :}
   bytes SNAP-RELOC:XTCELL-N-CELL HEADER-BYTES + < if REFUSE then
   data SNAP-RELOC:XTCELL-N-CELL + cell-view {: h:^ :}
   h MAGIC-FIELD + @ MAGIC <> if REFUSE then
   h MODE-FIELD + @ 0 <> if REFUSE then
   h @ h CAP-FIELD + @ SHAPE
   h BASE-FIELD + @ h CAP-FIELD + @ bytes WITHIN
   data h BASE-FIELD + @ + cell-view h @ ;

;package
