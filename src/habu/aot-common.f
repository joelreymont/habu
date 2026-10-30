\ Shared closure planning, carried cells, and target mapping for stripped images.
require src/os/env-base.f
require src/habu/stack-abi.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/fdio.f
require src/lib/le.f
require src/habu/address-carrier.f
require src/habu/aot-decl.f
require src/habu/aot-window-latch.f
require src/habu/aot-owned-cells.f
require src/habu/aot-closure.f

package AOT-LINK
private

: AOT-OUT  s" hb-aot-got" TMP-PATH ;
: AOT-OBJ ( -- ptr u8 n )  s" hb-aot-obj" TMP-PATH ;

: AOT-FALSE ( -- bool ) 0 0= 0= ;

DYNAMIC-BUFFER NEWOFF n   \ each member's offset in the emitted text
DYNAMIC-BUFFER BLEN n     \ ... and the bytes planned for it
: PLAN-TABLES ( n -- ) {: rows:n :}
   rows 1 < IF s" aot: no closure members to plan" 74 die THEN
   rows NEWOFF-RESERVE  rows BLEN-RESERVE ;

\ Only recorded DATA carriers enter this table. Rows are in member and source
\ order; PSTART/PEND delimit each member's sparse shortening map. PCUM holds
\ the bytes removed through a row, so every consumer maps an old offset by the
\ same arithmetic, including targets that name ordinary bytes between sites.
DYNAMIC-BUFFER PSITE ptr u8
DYNAMIC-BUFFER PMODE n
DYNAMIC-BUFFER PGROUP n
DYNAMIC-BUFFER PHOLD n
DYNAMIC-BUFFER PCUM n
DYNAMIC-BUFFER PSTART n
DYNAMIC-BUFFER PEND n
variable PN
DYNAMIC-BUFFER GTARGET n
DYNAMIC-BUFFER GSAVE n
DYNAMIC-BUFFER GOFF n
DYNAMIC-BUFFER GON n
variable GN
variable POOL-OFF
variable POOL-N
: POOL-TABLES ( n -- ) {: rows:n :}
   rows PSITE-RESERVE rows PMODE-RESERVE
   rows PGROUP-RESERVE rows PHOLD-RESERVE rows PCUM-RESERVE
   rows GTARGET-RESERVE rows GSAVE-RESERVE rows GOFF-RESERVE rows GON-RESERVE
   NCLO @ PSTART-RESERVE NCLO @ PEND-RESERVE ;

\ The persistent data region is the span aot-window-latch.f latched: the window
\ opens before the application's `require` runs, long before this file loads.
\ DSCAN is a DATA address as an integer, like the bounds it walks between; its
\ one read goes through aot-closure.f DATA-CELL@.
variable DSCAN

\ THE CARRIED CELLS. Each claim named CARRIED in aot-owned-cells.f is copied, by
\ its declared byte length, into the run aot-window-latch.f CARRY-RESERVE
\ reserved, in claim order and cell-aligned (a carried table is read with `@`;
\ sha256's KK is 64 cells). The destination is recorded so aot-closure.f
\ CARRIED-TARGET maps a spelled address to the copy at the same interior offset.
\ The copies go in before AOT-DATA-TEXTPTR-CHECK and the closure walk, so carried
\ bytes face the window checks and no pass meets a half-carried window.
variable CARRY-USED

: CARRY-CELL ( n -- ) {: i:n :}
   i AOT-OWNED:LEN {: len:n :}
   \ The source is read through DATA-PTR, whose every caller bounds-checks the
   \ address it hands over: a claim's declared length is the only number here
   \ the linker did not compute itself.
   i AOT-OWNED:AT DATA-ADDRESS? i AOT-OWNED:AT len + DATA-ADDRESS? and 0= IF
      s" aot: a carried claim reaches outside the DATA mapping" 74 die THEN
   \ A CARRIED CLAIM IS ENGINE DATA, which is all below the window: the image
   \ restores the window itself, so a claim reaching into it would be copied into
   \ the run and never consulted - aot-closure.f MAPPED-DATA answers an in-window
   \ address with itself before it ever asks a claim - and the same bytes would
   \ ship twice. The bound above admits the whole mapping and cannot see this.
   i AOT-OWNED:AT len + BLOB-SRC @ > IF
      s" aot: a carried claim reaches into the capture window" 74 die THEN
   CARRY-USED @ len + CARRY-BYTES @ > IF
      s" aot: carried engine cells exceed the window's carried run" 74 die THEN
   i AOT-OWNED:AT DATA-PTR  CARRY-BASE$ CARRY-USED @ +  len  BYTE-COPY
   CARRY-BASE @ CARRY-USED @ +  i AOT-OWNED:DEST!
   CARRY-USED @ len + 7 + 8 / 8 *  CARRY-USED ! ;

: CARRY-CELLS ( -- )
   0 CARRY-USED !
   AOT-OWNED:N 0 ?do
      i AOT-OWNED:CARRIED? IF i CARRY-CELL THEN
   loop ;

\ Keep the application's counts and capacities even if a later linker load
\ declares maker rows. The two bases are published by the stripped entry.
variable FFI-MARK-INDEX

: FFI-MARK-VALUE ( -- n )
   FFI-MARK-INDEX @ {: idx:n :}
   idx 0= IF FFI-FN-MARK @ exit THEN
   idx 1 = IF FFI-LIB-MARK @ exit THEN
   idx 2 = IF FFI-FN-CAP-MARK @ exit THEN
   FFI-LIB-CAP-MARK @ ;

: CARRY-FFI-MARK-CELL ( ptr u8 -- ) {: source:ptr :}
   source data-base BYTE-VIEW - DATA-VA VA>N + {: at:n :}
   at BLOB-SRC @ < IF
      at CARRIED-TARGET {: dest:n :}
      dest -1 = IF s" aot: FFI boundary has no carried cell" 74 die THEN
      FFI-MARK-VALUE dest DATA-PTR CELL-VIEW !
   THEN
   1 FFI-MARK-INDEX +! ;

: CARRY-FFI-MARK ( -- )
   0 FFI-MARK-INDEX !
   [: CARRY-FFI-MARK-CELL ;] FFI:OWNED-COUNTS
   [: CARRY-FFI-MARK-CELL ;] FFI:OWNED-CAPS ;

\ A linker loaded after the application can allocate its own TASK user slots.
\ Keep the application-close watermark in the carried runtime cell.
: CARRY-TASK-USER-CELL ( ptr u8 -- ) {: source:ptr :}
   source data-base BYTE-VIEW - DATA-VA VA>N + {: at:n :}
   at BLOB-SRC @ < IF
      at CARRIED-TARGET {: dest:n :}
      dest -1 = IF s" aot: TASK user boundary has no carried cell" 74 die THEN
      TASK-USER-MARK @ dest DATA-PTR CELL-VIEW !
   THEN ;

: CARRY-TASK-USER-MARK ( -- )
   [: CARRY-TASK-USER-CELL ;] TASK:OWNED-USER-NEXT ;

\ TASK's carried base cell still needs its declared DATA relocation. FFI's
\ table bases are fresh engine cells initialized by EMIT-FFI-BASES instead.
: CARRY-TASK-BASE ( ptr u8 -- ) {: source:ptr :}
   source data-base BYTE-VIEW - DATA-VA VA>N + {: at:n :}
   at BLOB-SRC @ < IF
      at CARRIED-TARGET {: dest:n :}
      dest -1 = IF s" aot: TASK table base has no carried cell" 74 die THEN
      dest DATA-PTR ptr-cell-mark
   THEN ;

: CARRY-TASK-BASES ( -- )
   [: CARRY-TASK-BASE ;] TASK:OWNED-ROW-BASE ;

\ TASK's typed exit table may have declaration-time callbacks. Its carried
\ bytes hold builder code addresses, so mark each active destination as an xt
\ cell while the builder's address-cell registry is still live. The normal
\ collection, closure root and entry relocation then handle those quotations.
\ The destination is the exact carried copy of TASK-EXIT-QT's typed slot.
CAST: TASK-EXIT-SLOT ( ptr n -- ptr [ -- ] )

: CARRY-TASK-EXIT ( -- )
   TASK:OWNED-EXIT-N 0 ?do
      i TASK:OWNED-EXIT-AT {: source:ptr q :}
      source BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + {: at:n :}
      at BLOB-SRC @ < IF
         at CARRIED-TARGET {: dest:n :}
         dest -1 = IF s" aot: task exit quotation has no carried cell" 74 die THEN
         q dest DATA-PTR CELL-VIEW TASK-EXIT-SLOT xt!
      THEN
   loop ;

\ Every window cell NOTHING DECLARED, classified by aot-closure.f CELL-TEXTPTR?
\ (live engine extents). Declared xt cells (COLLECT-XT-CELLS, EMIT-XT-ROWS) and
\ string-literal bodies (aot-closure.f CELL-LITERAL?) are skipped by their
\ declaration, not their value, so the two answers cannot disagree about a cell.
\ What is left is a code or dictionary pointer no declaration accounts for (a
\ `' word ,` table); the first is reported with its owning word, its DATA offset
\ and the pointer it holds.
\ RAW AND TYPED STORAGE BOTH STAY UNDER THE VALUE SCAN: a value can come from a
\ `0 set-check` window or an unchecked body, and a declared pointer cell's type
\ does not say whether its mapping survives capture. Nothing stores into a
\ literal body.
\ The value decides only WHICH refusal; whether one is drawn is the
\ declaration's answer, so CELL-LITERAL? is asked before either refusal. It is
\ asked second because its row walk is a pass over the pools while the two
\ extent tests are arithmetic.
: SCAN-DATA-CELL ( n -- ) {: at:n :}
   at DATA-CELL@ {: v:n :}
   v CELL-TEXTPTR? v CELL-MAPPED? or 0= IF exit THEN
   at AOT-CELL:NONCELL-WINDOW? IF exit THEN
   at CELL-LITERAL? IF exit THEN
   v CELL-TEXTPTR? IF at v REFUSE-UNDECLARED-CELL THEN
   v CELL-MAPPED? IF at v REFUSE-MAPPED-CELL THEN ;

\ Carried byte fields can resemble a pointer when read across adjacent bytes.
\ Check only their declared cell fields before the general window scan.
: AOT-CARRIED-CELLS-CHECK ( -- )
   XTC-REWIND
   AOT-OWNED:N 0 ?do
      i AOT-OWNED:CARRIED-CELLS? IF
         i AOT-OWNED:LEN CELL / 0 ?do
            j AOT-OWNED:DEST i CELL * + {: at:n :}
            at XTC-DECLARED? 0= IF at SCAN-DATA-CELL THEN
         loop
      THEN
   loop ;

: CARRIED-WINDOW? ( n -- bool ) {: at:n :}
   at CARRY-BASE @ >=
   at CELL + CARRY-BASE @ CARRY-USED @ + <= and ;

: AOT-DATA-TEXTPTR-CHECK ( -- )
   0 AOT-CELL:PREPARE
   AOT-CARRIED-CELLS-CHECK
   XTC-REWIND
   BLOB-END @ BLOB-SRC @ - 16 >= IF
      0 CELL-TEXTPTR? 0 CELL-MAPPED? or 0=
   ELSE false THEN {: zero-safe:bool :}
   BLOB-SRC @ DSCAN !
   BEGIN DSCAN @ 8 + BLOB-END @ <= WHILE
      \ Two complete zero cells prove all eight overlapping windows zero.
      \ Keep the byte walk at transitions and whenever zero itself is refused.
      zero-safe DSCAN @ 16 + BLOB-END @ <= and IF
         DSCAN @ DATA-CELL@ DSCAN @ 8 + DATA-CELL@ or 0=
      ELSE false THEN IF
         DSCAN @ 8 + DSCAN !
      ELSE
         DSCAN @ CARRIED-WINDOW? 0= IF
            DSCAN @ XTC-DECLARED? 0= IF DSCAN @ SCAN-DATA-CELL THEN
         THEN
         DSCAN @ 1+ DSCAN !
      THEN
   REPEAT
   AOT-CELL:RESET ;

: OWNED-PUBLISHED? ( n -- bool ) {: k:n :}
   k AOT-OWNED:ENTRY-XT?  k AOT-OWNED:TEXT-BASE? or ;

\ The span in whole cells: the grid the bitmap covers. The last cell may reach
\ above the latched span end, and the writer reads those bytes as the zeros they
\ are, so the image's own DP is this rounded end and nothing is ever stored
\ above it.
: SPAN-CELLS ( -- n )
   BLOB-LEN @ AOT-WINDOW:CELL-BYTES 1- + AOT-WINDOW:CELL-BYTES / ;

: BLOB-SRC@ ( -- ptr u8 ) BLOB-SRC @ DATA-PTR ;

variable NEXT-OFF
\ THE MEMBERS IN ENTRY ORDER. The closure walk adds members in call order
\ (aot-closure.f ADD-CLO); both lookups below binary-search this index, filled
\ once per link for the same NCLO rows as NEWOFF and BLEN. A scan per relocated
\ instruction made the link quadratic in NCLO.
\ THE MEMBERS ARE DISJOINT BY CONSTRUCTION, so one position answers for an
\ address. Two anonymous bodies of one record are the only members that can nest
\ (under tier 1 the lower one runs to the record's end), and aot-closure.f
\ DROP-NESTED-CLO drops the contained one after the walk. MORD-DISJOINT checks
\ it once per link and names both members' records, entries and lengths.
\ Adjacency is admitted: a target at a member's end belongs to the next member
\ (MAP-IN-MEMBER's >=). Measured: zero overlapping and 27,000 touching pairs
\ over a 27,004-member chain; no link of the AOT suites reaches the refusal.
DYNAMIC-BUFFER MORD n         \ member indices, ascending by CLO-AT
variable MOI                                  \ the fill and disjointness cursor
variable MSR  variable MSC  variable MSN      \ a sift's root, child and heap size
variable MHI  variable MHE                    \ the heapify and extract cursors
variable MBLO  variable MBHI  variable MBMID  \ a search's window and its probe
variable MBP                                  \ ... and the best position it saw
: MORD-AT ( n -- n ) MORD @ ;                 \ the member at order position p
: MORD-KEY ( n -- ptr u8 ) MORD-AT CLO-AT ;   \ ... and that member's entry
: MORD-SWAP ( n n -- ) {: a:n b:n :}
   a MORD-AT {: x:n :}
   b MORD-AT a MORD !  x b MORD ! ;
\ Heapsort: one column, no scratch, no recursion, O(N log N) in any walk order.
\ THE WALK ORDER IS NOT NEARLY SORTED: a 27,004-member chain of words each
\ calling the previous descends at 27,001 of 27,003 adjacent pairs (the walk
\ enters at the tail), a nine-member library program at 6 of 8. An insertion
\ sort would be the quadratic this pass removes.
: MORD-CHILD ( -- )           \ MSC = the larger of the root's two children
   MSC @ 1+ MSN @ < IF
      MSC @ MORD-KEY  MSC @ 1+ MORD-KEY  < IF MSC @ 1+ MSC ! THEN THEN ;
: MORD-SIFT ( n n -- ) {: root:n rows:n :}
   root MSR !  rows MSN !
   BEGIN MSR @ 2 * 1+ MSN @ < WHILE
      MSR @ 2 * 1+ MSC !
      MORD-CHILD
      MSR @ MORD-KEY  MSC @ MORD-KEY  < 0= IF EXIT THEN
      MSR @ MSC @ MORD-SWAP
      MSC @ MSR !
   REPEAT ;
: MORD-SORT ( n -- ) {: rows:n :}
   rows 2 / 1- MHI !
   BEGIN MHI @ 0 >= WHILE  MHI @ rows MORD-SIFT  MHI @ 1- MHI !  REPEAT
   rows 1- MHE !
   BEGIN MHE @ 0 > WHILE
      0 MHE @ MORD-SWAP
      0 MHE @ MORD-SIFT
      MHE @ 1- MHE !  REPEAT ;
: MORD-OVERLAP-DIE ( n n -- ) {: a:n b:n :}
   s" aot: closure members overlap site=" AETXT
   a CLO-REC@ AEREC-TXT
   s"  at=" AETXT a CLO-AT CODE-N AEJNUM
   s"  bytes=" AETXT a CLO-BYTES AEJNUM
   s"  and=" AETXT b CLO-REC@ AEREC-TXT
   s"  at=" AETXT b CLO-AT CODE-N AEJNUM
   s"  bytes=" AETXT b CLO-BYTES AEJNUM
   10 AE1
   s" aot: closure members overlap" 74 die ;
: MORD-DISJOINT ( -- )
   0 MOI !
   BEGIN MOI @ 1+ NCLO @ < WHILE
      MOI @ MORD-KEY MOI @ MORD-AT CLO-BYTES +  MOI @ 1+ MORD-KEY > IF
         MOI @ MORD-AT  MOI @ 1+ MORD-AT  MORD-OVERLAP-DIE THEN
      MOI @ 1+ MOI ! REPEAT ;
\ Once per link, from PLAN-BLOBS, where the closure is final; the first lookup
\ of a link is in COPY-BLOBS. A test that fills the closure tables by hand calls
\ this as it calls PLAN-TABLES. A lookup before it gets E-LAYOUT-BOUNDS off the
\ unfilled order: fail-closed.
: MEMBER-ORDER ( -- )
   NCLO @ 1 < IF s" aot: no closure members to order" 74 die THEN
   NCLO @ MORD-RESERVE
   0 MOI ! BEGIN MOI @ NCLO @ < WHILE  MOI @ MOI @ MORD !  MOI @ 1+ MOI ! REPEAT
   NCLO @ MORD-SORT
   MORD-DISJOINT ;
: MORD-FIND ( ptr u8 -- n ) {: start:ptr :}   \ the position of this exact entry, or -1
   0 MBLO !  NCLO @ 1- MBHI !
   BEGIN MBLO @ MBHI @ <= WHILE
      MBLO @ MBHI @ + 2 / MBMID !
      MBMID @ MORD-KEY start = IF MBMID @ EXIT THEN
      MBMID @ MORD-KEY start < IF MBMID @ 1+ MBLO ! ELSE MBMID @ 1- MBHI ! THEN
   REPEAT  -1 ;
: MORD-BELOW ( ptr u8 -- n ) {: t:ptr :}      \ the last position at or below t, or -1
   0 MBLO !  NCLO @ 1- MBHI !  -1 MBP !
   BEGIN MBLO @ MBHI @ <= WHILE
      MBLO @ MBHI @ + 2 / MBMID !
      t MBMID @ MORD-KEY < IF MBMID @ 1- MBHI !
      ELSE MBMID @ MBP !  MBMID @ 1+ MBLO ! THEN
   REPEAT  MBP @ ;
\ The closure member whose entry this is, or -1. The entry is a member's
\ identity (aot-closure.f ADD-CLO), so this is what a record pointer or a span
\ row is resolved through before anything asks where the member is going.
: MEMBER-AT {: start:ptr :} ( ptr u8 -- n )
   start MORD-FIND {: p:n :}
   p 0 < IF -1 EXIT THEN
   p MORD-AT ;
: MEMBER-NEWOFF ( n -- n ) NEWOFF @ ;
: CLO-AT-N ( n -- n ) CLO-AT CODE-N ;      \ the same entry, for value-domain arithmetic

variable TNEW
\ A target at a member's end is the adjacent member's start, not this one's.
variable MREM
: MAP-IN-MEMBER {: m:n t:ptr :} ( n ptr u8 -- n )
   t m CLO-AT < IF -1 EXIT THEN
   t m CLO-AT m CLO-BYTES + >= IF -1 EXIT THEN
   0 MREM !
   PN @ 0 > IF
      m PEND @ m PSTART @ ?do
         i PMODE @ 0<> IF
            t i PSITE @ > t i PSITE @ ADDRESS-CARRIER:DATA-CHAIN-BYTES + < and IF
               s" aot: code target inside shortened DATA carrier" 74 die
            THEN
            t i PSITE @ ADDRESS-CARRIER:DATA-CHAIN-BYTES + >= IF
               i PCUM @ MREM !
            THEN
         THEN
      loop
   THEN
   m MEMBER-NEWOFF  t m CLO-AT -  + MREM @ - ;
\ The one member that can cover t is the last whose entry is at or below it;
\ MAP-IN-MEMBER answers whether t is inside it.
: OLD>NEW {: t:ptr :} ( ptr u8 -- n )
   t MORD-BELOW {: p:n :}
   p 0 < IF -1 EXIT THEN
   p MORD-AT t MAP-IN-MEMBER ;
: MAP-TARGET {: i:n t:ptr :} ( n ptr u8 -- n )
   i t MAP-IN-MEMBER dup -1 <> IF EXIT THEN drop  t OLD>NEW ;
\ A TARGET THE COMPACTED IMAGE HAS NO ADDRESS FOR IS NAMED: the relocated
\ member's record is the word to edit, and the target's owner, when the building
\ dictionary still knows one, says what was not carried. A target no record owns
\ is named `NAME+off` by the record below it, the rule aot-closure.f
\ CODE-ADDR-TXT names the span refusal's site by.
: MAP-TARGET! {: i:n t:ptr :} ( n ptr u8 -- )
   i t MAP-TARGET TNEW !
   TNEW @ -1 = IF
      s" aot: PC-relative target removed or outside closure site=" AETXT
      i CLO-REC@ AEREC-TXT
      s"  target=" AETXT t CODE-N AEJNUM
      s"  target-word=" AETXT t CODE-N ADDRESS-OWNER t CODE-ADDR-TXT
      10 AE1
      s" " 74 die THEN ;

\ ptr-cell-mark is the same declaration for a DATA pointer cell, but its
\ primitive pops the cell address before registering it. Keep that pop when
\ stripping the registry call, or each such declaration leaks one stack cell.
: PTR-MARK-TARGET? ( ptr u8 -- bool ) {: t:ptr :}
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? 0= IF AOT-FALSE EXIT THEN
   callee s" ptr-cell-mark" REC-NAME= ;
: DECLARATION-TARGET? ( ptr u8 -- bool ) {: t:ptr :}
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? 0= IF AOT-FALSE EXIT THEN
   callee AOT-DECLARATION? ;
: DP-REFUSAL-TARGET? ( ptr u8 -- bool ) {: t:ptr :}
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? 0= IF AOT-FALSE EXIT THEN
   callee AOT-DP-REFUSAL? ;

\ A declared xt cell's row target goes through the same OLD>NEW mapping as a
\ relocated branch target. An address no member covers dies by name: the
\ emit-time proof that the image carries the code the cell will point at.
: XT-CELL-TARGET ( n -- n ) {: k:n :}
   k XTC-VAL@ CODE-PTR OLD>NEW {: t:n :}
   t 0 < IF s" aot: declared xt cell target is outside the closure" 74 die THEN
   t ;

defer LINK-TARGET ( -- )

public
: LINK ( -- )
   AOT-APP-POOL
   CARRY-CELLS
   CARRY-TASK-USER-MARK
   CARRY-FFI-MARK
   CARRY-TASK-BASES
   CARRY-TASK-EXIT
   COLLECT-XT-CELLS
   AOT-DATA-TEXTPTR-CHECK
   CLOSURE
   LINK-TARGET ;

;package
