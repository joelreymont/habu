\ The engine owns address-cell declarations. Readers acquire a fresh span:
\ appending a row may move its backing storage, but never changes a row's index.
s" src/habu/layout.f" required
s" src/habu/xref.f" required

package ADDRESS-CELLS
public

1 constant ABI-VERSION
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

\ All engine emitters publish these primitives consecutively in global
\ wordlist zero. Retirement changes a marker's wordlist, but preserves its
\ name and position; a later redefinition must not replace the original pair.
\ Resolve it on each call because a captured image has different code addresses
\ from its build host. A foreign target layout's image-header offsets also do
\ not describe the running engine.
: ENGINE-ABI-XT ( -- n )
   ndict@ 1- 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-NAME$ s" ptr-cell-mark" CORE-STR=CI if
         rec XREF-WORDLIST {: wid:n :}
         wid 0<> wid XREF-RETIRED-WL <> and if 0 unloop exit then
         i 1+ XREF-REC {: next:ptr :}
         next XREF-WORDLIST 0= if
            next XREF-NAME$ s" addr-cells-abi" CORE-STR=CI if
               next XREF-START unloop exit
            then
         then
         0 unloop exit
      then
   loop
   0 ;

public
\ Wordlist zero contains the immutable engine primitive. Reject a source word
\ shadowing it: only executable engine text may supply the discriminator.
\ Recovery returns zero; old native hosts have no primitive at all.
: CURRENT? ( -- bool )
   s" addr-cells-abi" 0 search-wl {: xt:n :}
   xt 0= if 0 0 <> exit then
   xt ENGINE-ABI-XT <> if
      s" address-cells: version probe is not an engine primitive" 96 die
   then
   xt VERSION-XT execute {: version:n :}
   version 0= if 0 0 <> exit then
   version ABI-VERSION <> if
      s" address-cells: unsupported engine storage version" 96 die
   then
   0 0= ;

private

: REFUSE ( -- ) s" address-cells: invalid storage header" 96 die ;
: SHAPE ( n n -- ) {: count:n cap:n :}
   cap 0 <= cap MAX-ROWS > or count 0 < or if REFUSE then
   count cap > if REFUSE then ;

: HEADER ( -- ptr n ) data-base SNAP-RELOC:XTCELL-N-CELL + ;
\ The header's base and the index cell hold an outside mapping's address as an
\ integer. An old donor loading this source refuses a pointer CAST:.
TRUSTED: N>ROWS ( n -- ptr n ) ;
: ROWS>N ( ptr n -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW - ;
: DATA-ROWS ( n -- ptr n ) data-base + ;

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

\ Runs a fork with the registrar held; PROC-FORK:RAW is its caller. The
\ registrar unmaps its old rows and index before it publishes their
\ replacements, and commits an index slot before its counts (habu2.f
\ EMIT-MARK), so a child forked while another task was inside it could neither
\ wait for that task nor carry on where it stopped. Held, nobody is inside it
\ when the process is copied, and parent and child each free their own copy.
\ The lock is found through data-base, a task's own context on a task, so a
\ fork made on a task holds the task-local address-cell lock, not the
\ registrar's, until dot bb84a527.
: ACROSS-FORK ( [ -- n ] -- n )
   WITH-LOCK ;

\ Native-build owns the explicit lifetime cut through its retired source heap.
\ Preserve registration order and invalidate even when the count is unchanged.
: KEEP-BELOW ( n -- )
   CURRENT? if
      [: KEEP-LOCKED ;] WITH-LOCK
   else KEEP-ROWS then ;

\ A fixed typed allocation can change which sum payload cells contain code.
\ Replace only its XT declarations; DATA declarations and outside rows stay.
private
: REMOVE-XT-LOCKED ( n n -- ) {: first:n limit:n :}
   INDEX-RELEASE
   LIVE-SPAN {: rows:ptr count:n :}
   0
   count 0 ?do
      rows i cells + @ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and {: off:n :}
      row SNAP-RELOC:XTCELL-DATA-TAG and 0= off first >= and
      off limit CELL - <= and 0= if
         dup cells rows + row swap ! 1+
      then
   loop
   HEADER ! ;

public

: REMOVE-XT-SPAN ( n n -- ) {: first:n bytes:n :}
   first 0 < bytes 0 < or if REFUSE then
   first here data-base - > if REFUSE then
   bytes here data-base - first - > if REFUSE then
   first first bytes + [: REMOVE-XT-LOCKED ;] WITH-LOCK ;

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
