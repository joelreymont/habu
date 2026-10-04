\ Shared closure planning, carried cells, and target mapping for stripped images.
require src/os/env-base.f
require src/habu/stack-abi.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/fdio.f
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

\ --- the persistent data region this file emits is the span
\ src/habu/aot-window-latch.f latched: AOT-DATA-START and AOT-DATA-SPAN live there
\ because the window must open before the application's `require` runs, which is
\ long before this file is loaded. The scan cursor below is this file's own, and
\ it is a DATA address as an integer like the bounds it walks between - nothing
\ dereferences it, the one cell it reads goes through aot-closure.f DATA-CELL@,
\ which is where a DATA address becomes a pointer again.
variable DSCAN

\ THE CARRIED CELLS, COPIED INTO THE WINDOW BEFORE ANYTHING READS OR MAPS IT.
\ Each claim named CARRIED in src/habu/aot-owned-cells.f is copied, by its
\ declared byte length, into the run aot-window-latch.f CARRY-RESERVE reserved
\ inside the span, and the destination is recorded so aot-closure.f
\ CARRIED-TARGET can map a spelled address to the copy at the same interior
\ offset. Runs are placed in claim order and cell-aligned, because a carried table
\ is read with `@` (sha256's KK is 64 cells); the copies go in BEFORE
\ AOT-DATA-TEXTPTR-CHECK and the closure walk, so carried bytes face the same
\ window checks the application's own data does and no pass can meet a
\ half-carried window.
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

\ Every cell the capture window covers that NOTHING DECLARED, classified by the
\ ONE predicate aot-closure.f publishes (CELL-TEXTPTR?, by live engine extents).
\ A declared xt cell is relocated instead (COLLECT-XT-CELLS above it, EMIT-XT-ROWS
\ below), and it is skipped here by its declaration and not by its value, so the
\ two answers cannot disagree about one cell. A STRING LITERAL'S BODY is the other
\ declaration the window carries: its bytes are characters by the row the compiler
\ wrote when it placed them (aot-closure.f CELL-LITERAL?), so they are skipped
\ here for the same reason and by the same rule. What is left is a code or
\ dictionary pointer no declaration accounts for - a `' word ,` table - and the
\ scan reports the first one with the three facts a program has to be edited by:
\ its owning word, its DATA offset and the pointer it holds.
\ RAW AND TYPED STORAGE BOTH STAY UNDER THE VALUE SCAN. Inside a definition the
\ checker refuses a pointer or a token in either kind of cell, but the top level
\ is not certified: `variable V ' FOO V !` and `TYPED-VARIABLE V n ' FOO V !`
\ both bake a code address (measured on this engine), so no storage declaration
\ says what a baked cell holds until the top level checks typed stores. A
\ literal body has no such door: nothing stores into it.
\ ONE CELL, JUDGED BY WHAT DECLARED ITS BYTES. The value decides only WHICH
\ refusal a cell would draw; whether it draws one at all is the declaration's
\ answer, so the second declaration - the compiler's literal rows
\ (aot-closure.f CELL-LITERAL?) - is asked before either refusal and not after.
\ It is asked SECOND because the row walk costs a pass over the pools while the
\ two extent tests are arithmetic: a cell whose value is neither a live engine
\ extent nor a mapping is data on any reading and needs no declaration to say so.
: SCAN-DATA-CELL ( n -- ) {: at:n :}
   at DATA-CELL@ {: v:n :}
   v CELL-TEXTPTR? v CELL-MAPPED? or 0= IF exit THEN
   at CELL-LITERAL? IF exit THEN
   v CELL-TEXTPTR? IF at v REFUSE-UNDECLARED-CELL THEN
   v CELL-MAPPED? IF at v REFUSE-MAPPED-CELL THEN ;

: AOT-DATA-TEXTPTR-CHECK ( -- )
   XTC-REWIND
   BLOB-SRC @ DSCAN !
   BEGIN DSCAN @ 8 + BLOB-END @ <= WHILE
      DSCAN @ XTC-DECLARED? 0= IF DSCAN @ SCAN-DATA-CELL THEN
      DSCAN @ 8 + DSCAN !
   REPEAT ;

: OWNED-PUBLISHED? ( n -- bool ) {: k:n :}
   k AOT-OWNED:IMAGE-BASE?  k AOT-OWNED:ENTRY-XT? or  k AOT-OWNED:TEXT-BASE? or ;

\ The span in whole cells: the grid the bitmap covers. The last cell may reach
\ above the latched span end, and the writer reads those bytes as the zeros they
\ are, so the image's own DP is this rounded end and nothing is ever stored
\ above it.
: SPAN-CELLS ( -- n )
   BLOB-LEN @ AOT-WINDOW:CELL-BYTES 1- + AOT-WINDOW:CELL-BYTES / ;

\ BLOB-SRC is a plain address cell; pin its byte-pointer role once here so
\ every scan/copy site below reads it as a span, not a bare number.
: BLOB-SRC@ ( -- ptr u8 ) BLOB-SRC @ DATA-PTR ;

64 constant SEED-MAX
create SEED-CELLS SEED-MAX cells allot   variable SEED-N
0 SEED-N !
: SEED-RESET ( -- )  0 SEED-N ! ;
: SEED+ ( n -- )
   SEED-N @ SEED-MAX >= IF s" aot: too many preseed cells" 74 die THEN
   SEED-CELLS SEED-N @ cells + !  SEED-N @ 1 + SEED-N ! ;

variable NEXT-OFF
\ --- THE MEMBERS IN ENTRY ORDER, the third per-member column. The closure walk
\ discovers members in CALL order (aot-closure.f ADD-CLO appends what it reaches),
\ so the entries in CLO are unordered, and the two lookups below - the exact-entry
\ one and the one that finds the member covering an address - scanned the whole
\ closure per relocated instruction, which made the link quadratic in NCLO. This
\ index is filled once per link, beside NEWOFF and BLEN and for the same NCLO
\ rows, and both lookups binary-search it.
\ THE MEMBERS ARE DISJOINT BY CONSTRUCTION, which is what lets ONE position
\ answer for an address. Two anonymous bodies of one record are the only members
\ that can nest - under tier 1 the lower one runs to the record's end and holds
\ the higher one's code - and aot-closure.f DROP-NESTED-CLO drops the contained
\ one after the walk, at the source of the overlap and with the offset arithmetic
\ that makes the container answer for it; the closure this file plans never
\ overlaps. MORD-DISJOINT is the INVARIANT CHECK on that, run once per link over
\ the sorted order - each member's end at or below the next one's start - and it
\ names both members' records, entries and lengths rather than picking one of
\ them, the way the scans it replaced did (they answered the LOWEST index
\ covering a target, so the discovery order decided). Adjacency is admitted and
\ is what MAP-IN-MEMBER's >= boundary is about: a target at a member's end
\ belongs to the next member. A WALK CANNOT REACH THE REFUSAL, so what reaches it
\ is a table filled by hand - test/gate-aot-negative-cases.f fills two rows, the
\ second inside the first, and expects exit 74 with the named line. Over real
\ links: zero overlapping pairs and 27,000 touching ones over the sorted order of
\ the 27,004-member chain this file's head measures, and every link
\ tools/hb-build-test.f and the gate's AOT suites make is green with the check in
\ place.
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
\ Heapsort: one column, no scratch, no recursion, O(N log N) whatever the walk
\ order is. THE WALK ORDER IS NOT NEARLY SORTED, and on a chain it is the reverse
\ - the worst case an insertion sort has. Measured over the closures this file
\ links: a 27,004-member chain of words each calling the previous descends at
\ 27,001 of its 27,003 adjacent index pairs (the walk enters the chain at its
\ tail and works back), and a nine-member library program at 6 of 8. An insertion
\ sort would be the quadratic this pass exists to remove.
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
\ Fill, sort, assert - once per link, from PLAN-BLOBS, because the closure is
\ final there and nothing before it asks where a member is or where it lands. A
\ test that fills the closure tables by hand calls this the way it calls
\ PLAN-TABLES: the lookups below read no other order.
\ NOTHING MAY ASK FOR A MEMBER BEFORE THIS RUNS, and nothing in a link does - the
\ first lookup of a link is in COPY-BLOBS, which is PLAN-BLOBS and then the copy.
\ A whitebox that asks earlier gets E-LAYOUT-BOUNDS off the unfilled order rather
\ than an answer, which is the fail-closed direction and how the order is proved
\ to be what the lookups read (test/compiler/aot-nested-body-cases.f plans first
\ for that reason; test/compiler/native-code-span-cases.f already did).
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
: MAP-IN-MEMBER {: i:n t:ptr :} ( n ptr u8 -- n )
   t i CLO-AT < IF -1 EXIT THEN
   t i CLO-AT i CLO-BYTES + >= IF -1 EXIT THEN
   i MEMBER-NEWOFF  t i CLO-AT -  + ;
\ THE ONE MEMBER THAT CAN COVER t is the last one whose entry is at or below it
\ (MEMBER-ORDER above asserted the members are disjoint), so the search answers a
\ position and MAP-IN-MEMBER answers whether t is inside that member at all.
: OLD>NEW {: t:ptr :} ( ptr u8 -- n )
   t MORD-BELOW {: p:n :}
   p 0 < IF -1 EXIT THEN
   p MORD-AT t MAP-IN-MEMBER ;
: MAP-TARGET {: i:n t:ptr :} ( n ptr u8 -- n )
   i t MAP-IN-MEMBER dup -1 <> IF EXIT THEN drop  t OLD>NEW ;
\ A TARGET THE COMPACTED IMAGE HAS NO ADDRESS FOR NAMES ITSELF. The member being
\ relocated is the site, so its record is the word to edit; the target is the
\ address the closure walk never reached, and the word that owns it - when the
\ building dictionary still knows one - says what was not carried. Without those
\ three facts the refusal is a bare sentence and the next case costs a bisection.
\ A target NO record owns is named by the record below it, `NAME+off`, the rule
\ the span refusal's site is named by (aot-closure.f CODE-ADDR-TXT): the target of
\ a branch into a stripped word, and any target an exact owner cannot be found
\ for, is otherwise a bare address the reader has nothing to open.
: MAP-TARGET! {: i:n t:ptr :} ( n ptr u8 -- )
   i t MAP-TARGET TNEW !
   TNEW @ -1 = IF
      s" aot: PC-relative target removed or outside closure site=" AETXT
      i CLO-REC@ AEREC-TXT
      s"  target=" AETXT t CODE-N AEJNUM
      s"  target-word=" AETXT t CODE-N ADDRESS-OWNER t CODE-ADDR-TXT
      10 AE1
      s" " 74 die THEN ;

: DECLARATION-TARGET? ( ptr u8 -- bool ) {: t:ptr :}
   t FINDADDR-PTR {: callee:ptr :}
   callee XREF-FOUND? 0= IF AOT-FALSE EXIT THEN
   callee AOT-DECLARATION? ;

\ The closure member whose span holds this code address, or -1 when nothing in
\ the closure covers it. The owner record answers for a word the image still
\ names; the payload's span table answers for one it does not, and an address
\ neither can place is refused here rather than relocated to a guess.
: ADDRESS-MEMBER ( n -- n ) {: v:n :}
   v ADDRESS-OWNER {: owner:ptr :}
   owner XREF-FOUND? if owner REC-CODE-PTR@ MEMBER-AT exit then
   v SPAN-OWNER {: k:n :}
   k 0 < if s" aot: code address has no dictionary owner" 74 die then
   k SPAN-START MEMBER-AT ;

\ A declared xt cell's row target is the new offset of the code it names, from
\ the SAME mapping a relocated branch target goes through (OLD>NEW: the member
\ that covers the address, plus the address's own distance into it, which
\ COPY-COMPACT-BLOB's instruction-for-instruction copy preserves). An address no
\ member covers dies by name here: the emit-time proof that this image carries
\ the code the cell will point at, after the root pass put it in the closure.
: XT-CELL-TARGET ( n -- n ) {: k:n :}
   k XTC-VAL@ CODE-PTR OLD>NEW {: t:n :}
   t 0 < IF s" aot: declared xt cell target is outside the closure" 74 die THEN
   t ;

defer LINK-TARGET ( -- )

public
: LINK ( -- )
   AOT-APP-POOL                                     \ reached literals are copied into the window's pool
   CARRY-CELLS                                      \ the engine constants this image carries
   CARRY-TASK-EXIT                                  \ active carried task exit quotations
   COLLECT-XT-CELLS                                 \ the window's DECLARED cells: xt rows out, DATA cells mapped
   AOT-DATA-TEXTPTR-CHECK                           \ ... and no undeclared code pointer beside them
   CLOSURE
   LINK-TARGET ;

;package
