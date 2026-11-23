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
RELOC:XTCELL-N-CELL HEADER-BYTES + constant BOOT-OFF
RELOC:XTCELL-END BOOT-OFF - CELL / constant BOOT-CAP
$7FFFFFFFFFFFFFFF CELL / constant MAX-ROWS

private
EXPORT BOOT:data-base

: REFUSE ( -- ) s" address-cells: invalid storage header" EXIT-STATUS:INTERNAL die ;

: HEADER ( -- ^n ) data-base RELOC:XTCELL-N-CELL + ;

\ The header's base and the index cell hold an outside mapping's address as an integer.
CAST: N>ROWS ( n -- ^n )

: LOCK-ADDR ( -- ^n ) data-base LOCK-CELL + ;
: INDEX-ADDR ( -- ^n ) data-base INDEX-CELL + ;
: LOCK ( -- ) begin 0 1 LOCK-ADDR atomic-cas 0= until ;
: UNLOCK ( -- ) 0 LOCK-ADDR atomic! ;

: HEADER-MODE ( ^n -- n )
   {: h:^ :}
   h MODE-FIELD + @ ;

\ A mode-1 table's rows, in the outside mapping the kernel's growth made.
: OWN-ROWS ( ^n -- ^n )
   {: h:^ :}
   h BASE-FIELD + @ N>ROWS ;

public
\ Unmaps the index the cell holds and zeroes the cell. On a table markers reach, the caller holds
\ its mutex through this AND the row/header mutation after, so no marker rebuilds the view between.
: INDEX-RELEASE ( ^n -- )
   {: held:^ :}
   held @ dup 0= if drop exit then {: address:n :}
   address N>ROWS INDEX-SLOTS + @ {: slots:n :}
   slots cells INDEX-HEADER + {: bytes:n :}
   address N>ROWS bytes munmap 0 <> if E-MEM-UNMAP throw then
   0 held ! ;

\ Unmaps a mode-1 table's rows, cap cells as the kernel's growth mapped them; a
\ mode-0 header, or one not yet laid (all zero), holds none.
: ROWS-RELEASE ( ^n -- )
   {: h:^ :}
   h MODE-FIELD + @ 0= if exit then
   h OWN-ROWS h CAP-FIELD + @ cells munmap
   0 <> if E-MEM-UNMAP throw then ;
private

\ Subtraction precedes addition/multiplication, including on malformed input.
: WITHIN ( n n n -- ) {: off:n cap:n bytes:n :}
   off 0 < off bytes > or if REFUSE then
   cap bytes off - CELL / > if REFUSE then ;

\ The header in DATA bytes [data, data+bytes): its rows and count.
: READ-HEADER ( str -- ^n n )
   {: data:^ bytes:n :}
   data RELOC:XTCELL-N-CELL + cell-view {: h:^ :}
   h HEADER-MODE {: mode:n :}
   h CAP-FIELD + @ {: cap:n :}
   mode 0= if
      h BASE-FIELD + @ {: base:n :}
      base cap bytes WITHIN
      data base + cell-view h @ exit
   then
   h OWN-ROWS h @ ;

public

: LIVE-SPAN ( -- ^n n )
   data-base BYTE-VIEW here data-base - READ-HEADER ;

: ROW@ ( n -- n ) {: row:n :}
   LIVE-SPAN {: base:^ count:n :}
   row 0 < row count >= or if REFUSE then
   base row cells + @ ;

private

\ Mode-0 rows lie below the DP the program starts with, in the band or after the
\ linked data; every rewind returns to a DP recorded at run time, so they stay in place.
: KEEP-ROWS ( n -- ) {: floor:n :}
   LIVE-SPAN {: base:^ count:n :}
   0 count 0 ?do
      base i cells + @ {: row:n :}
      row RELOC:XTCELL-OFF-MASK and floor < if
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
      row RELOC:XTCELL-OFF-MASK and {: off:n :}
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

\ Reads a DATA copy's table: its rows lie in the copy or, once grown, in a mapping
\ this process made.
: DATA-SPAN ( str -- ^n n ) {: data:^ bytes:n :}
   bytes RELOC:XTCELL-N-CELL HEADER-BYTES + < if REFUSE then
   data bytes READ-HEADER ;

;package
