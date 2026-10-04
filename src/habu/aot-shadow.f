\ aot-shadow.f - the capture's reader of a second target's routines: it walks
\ src/compiler/native/shadow.f's map (NSHADOW) and fills src/habu/aot-decl.f's
\ shadow tables (package AOT-SHADOW), which src/habu/aot-file.f carries.
\
\ WHY IT IS ITS OWN FILE. src/habu/aot-capture.f also compiles in the stdin
\ metabuild host and the recovery bootstrap, which carry no native compiler and so
\ no NSHADOW. And the map must be read through the NSHADOW this file was compiled
\ against: a native build resets the dictionary logically before it loads the
\ target, so a name looked up at capture time would find the target's own,
\ never-opened shadow, while the host's compiler filed the map in its own. So a
\ host that can have a shadow open loads this file beside aot-capture.f, before
\ its window opens, and it reopens the capture's package.
\
\ WHEN IT RUNS. SHADOW-CAPTURE runs right after AOT-CAPTURE:CAPTURE, which latches
\ the window, builds the xt -> record index for the live dictionary and fills the
\ name pool this reuses; it refuses to run against any other dictionary. It reads
\ the map before anything closes the shadow: NCOMP:CAPTURE-PREPARE closes the
\ shadow of the compiler that runs it, and a native build's preparation of its
\ target (tools/native-build-core.f PREPARE-TARGET) runs the target window's own
\ compiler, not the host's that filed the map. With no shadow open the tables stay
\ as CAPTURE left them, empty.
\
\ THE X86 REACH USES X86 EDGES. Shipped record entries and declared code cells
\ root the shadow map; its recorded call and code-address sites close the graph.
\ ARM and x86 can make different optimizations, so ARM code reach cannot answer
\ which x86 routines are needed. A reached routine with a shipped record uses
\ that row; a stripped or retired routine travels anonymously, without making
\ its name available to the target. A public EXPORT alias can share a stripped
\ or retired source's entry, so exact host xt identity attaches the alias's
\ shipped row to that source's shadow emission. The copy follows NSHADOW's
\ publication order and keeps one copy of each reached emission. A target in
\ the window must resolve to a reached shadow row, and a prefix target must
\ resolve by name; otherwise capture refuses it.

require src/compiler/native/hir.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/address-carrier.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f

package AOT-CAPTURE
using AOT-BUF

variable SH-REC                      \ the window record being filed, for a refusal
variable SH-E                        \ the map's emission last copied
variable SH-AT                       \ where its bytes start in the shadow code
variable SH-LEN                      \ and how many there are
variable SH-PREV                     \ the record filed before this one
variable SH-SHIP-N                   \ the shipped records numbered so far
DYNAMIC-BUFFER SH-ROWS n             \ capture record -> its shipped row, or -1
DYNAMIC-BUFFER SH-SOURCE n           \ window record -> NSHADOW row, or -1
DYNAMIC-BUFFER SH-HEAD n             \ window record -> first shipped alias + 1
DYNAMIC-BUFFER SH-NEXT n             \ capture record -> next alias + 1
DYNAMIC-BUFFER SH-MARK n             \ NSHADOW row reached by x86 code
DYNAMIC-BUFFER SH-WORK n             \ reached rows awaiting their site scan
DYNAMIC-BUFFER SH-OUT n              \ NSHADOW row -> shadow record row, or -1
DYNAMIC-BUFFER SH-EMROOT n           \ emission -> first published NSHADOW row
variable SH-QN
DYNAMIC-BUFFER SH-QXT n              \ exact host entry of an anonymous function
DYNAMIC-BUFFER SH-QEM n              \ its owning shadow emission
DYNAMIC-BUFFER SH-QFUN n             \ its HIR function ordinal
DYNAMIC-BUFFER SH-QROW n             \ its assigned anonymous shadow row
variable SH-WORK-N
variable SH-OUT-N

\ ---- refusals ------------------------------------------------------------------
: SH-WHERE ( n -- ) {: off:n :}
   s" aot-capture: the shadow routine of " type SH-REC @ ACAP-NAME.
   s"  at emission byte " type off .INT ;

: SH-UNNAMED ( n n -- ) {: off:n v:n :}
   off SH-WHERE s"  names " type v .INT
   s"  which is no window record's entry and no word of the engine's own prefix" type cr
   s" aot-capture: a shadow site names a target the image cannot resolve" 74 die ;

: SH-DATA-OUT ( n n -- ) {: off:n v:n :}
   off SH-WHERE s"  holds DATA address " type v .INT
   s"  which is " type v ACAP-BAND. cr
   s" aot-capture: a shadow DATA literal outside the window's DATA span" 74 die ;

: SH-NOT-MOVABS ( n -- ) {: off:n :}
   off SH-WHERE s"  is no mov r64, imm64 inside its emission" type cr
   s" aot-capture: a shadow address site is not a MOVABS" 74 die ;

: SH-BAD-KIND ( n n -- ) {: off:n kind:n :}
   off SH-WHERE s"  has site kind " type kind .INT cr
   s" aot-capture: a shadow site of a kind the capture does not carry" 74 die ;

: SH-ORDER-REFUSE ( n -- ) {: idx:n :}
   s" aot-capture: shadow record " type idx .INT
   s"  is filed after a record at or above it" type cr
   s" aot-capture: the shadow's records are not in publication order" 74 die ;

: SH-XT-REFUSE ( n n -- ) {: celloff:n v:n :}
   s" aot-capture: declared code cell at DATA+" type celloff .INT
   s"  holds " type v .INT
   s"  which is window code no shipped record enters" type cr
   s" aot-capture: a shadowed code cell targets code no shipped record enters" 74 die ;

: SH-UNSHIPPED ( n n -- ) {: off:n idx:n :}
   off SH-WHERE s"  names window record " type idx ACAP-NAME.
   s" , which the capture does not ship" type cr
   s" aot-capture: a shadow site names a record the capture strips" 74 die ;

\ ---- the shipped rows -----------------------------------------------------------
\ ACAP-COMPACT-ONE gives a record a row of the compact table exactly when it sets
\ the record's named bit, in capture order, so a record's row is the count of
\ shipped records below it; ACAP-PROVE-RECS, which CAPTURE runs after the last
\ change to either, has proved the count over the window is the table's.
: SH-NUMBER ( -- )
   ACAP-REC-ALL @ {: n:n :}
   n SH-ROWS-RESERVE
   0 SH-SHIP-N !
   n 0 ?do
      i ACAP-NAMED-BIT @ 0<> if
         SH-SHIP-N @ i SH-ROWS !  1 SH-SHIP-N +!
      else
         -1 i SH-ROWS !
      then
   loop ;

: SH-WINDOW? ( n -- bool ) {: idx:n :}
   idx ACAP-W-R0 @ >= idx ACAP-W-R1 @ < and ;

: SH-SLOT ( n -- n ) ACAP-W-R0 @ - ;

: SH-SOURCE@ ( n -- n ) {: idx:n :}
   idx SH-WINDOW? 0= if -1 exit then
   idx SH-SLOT SH-SOURCE @ ;

: SH-HEAD@ ( n -- n ) {: idx:n :}
   idx SH-WINDOW? 0= if 0 exit then
   idx SH-SLOT SH-HEAD @ ;

\ The lowest dictionary record for an xt is the capture's target identity.
\ A later EXPORT alias can ship that same entry after the source was retired
\ or stripped, so attach its shipped row to the source's shadow map row.
: SH-INDEX ( -- )
   ACAP-W-R1 @ ACAP-W-R0 @ - {: n:n :}
   n SH-SOURCE-RESERVE  n SH-HEAD-RESERVE
   ACAP-REC-ALL @ SH-NEXT-RESERVE
   NSHADOW:RECORDS SH-MARK-RESERVE
   NSHADOW:RECORDS SH-WORK-RESERVE
   NSHADOW:RECORDS SH-OUT-RESERVE
   NSHADOW:EMISSIONS SH-EMROOT-RESERVE
   n 0 ?do -1 i SH-SOURCE !  0 i SH-HEAD ! loop
   ACAP-REC-ALL @ 0 ?do 0 i SH-NEXT ! loop
   NSHADOW:EMISSIONS 0 ?do -1 i SH-EMROOT ! loop
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ {: idx:n :}
      idx SH-WINDOW? if i idx SH-SLOT SH-SOURCE ! then
      i NSHADOW:EMISSION@ {: e:n :}
      e SH-EMROOT @ 0< if i e SH-EMROOT ! then
      0 i SH-MARK !  -1 i SH-OUT !
   loop
   ACAP-W-R1 @ ACAP-W-R0 @ ?do
      i ACAP-DICT>CAP {: k:n :}
      k 0 >= if
         k SH-ROWS @ 0 >=  i AOT-REC AOT-RWID -1 <> and if
            i AOT-REC AOT-RXT ACAP-TGT>REC {: source:n :}
            source SH-WINDOW? if
               source SH-HEAD@ k SH-NEXT !
               k 1+ source SH-SLOT SH-HEAD !
            then
         then
      then
   loop
   0 SH-WORK-N !  0 SH-OUT-N !  0 SH-QN ! ;

\ ---- rows ---------------------------------------------------------------------
: SH-SITE+ ( n n n -- ) {: at:n kind:n target:n :}
   AOT-SHADOW:SITE-N @ {: k:n :}
   k 1+ AOT-SHADOW:SITE-N !
   AOT-SHADOW:SITE-BUF@ k AOT-SHADOW:SITE-ROW * + {: r:ptr :}
   at r AOT-P32!  kind r 4 + AOT-P32!  target r 8 + AOT-P32! ;

: SH-REC+ ( n n -- ) {: row:n entry:n :}
   AOT-SHADOW:REC-N @ {: k:n :}
   k 1+ AOT-SHADOW:REC-N !
   AOT-SHADOW:REC-BUF@ k AOT-SHADOW:REC-ROW * + {: r:ptr :}
   row r AOT-P32!
   SH-AT @ r 4 + AOT-P32!
   SH-LEN @ r 8 + AOT-P32!
   entry r 12 + AOT-P32! ;

\ A window record's site target: a shipped alias of its exact entry, or its
\ own anonymous shadow row. -1 means no reached x86 routine carries the entry.
: SH-MAPPED ( n -- n ) {: idx:n :}
   idx SH-SOURCE@ {: r:n :}
   r 0 < if -1 exit then
   r SH-OUT @ {: row:n :}
   row 0 < if -1 exit then
   idx SH-HEAD@ {: head:n :}
   head 0<> if head 1- SH-ROWS @ SITE-REC-TAG or exit then
   row SITE-SHADOW-TAG or ;

: SH-QFIND ( n -- n ) {: xt:n :}
   SH-QN @ 0 ?do i SH-QXT @ xt = if i unloop exit then loop
   -1 ;

: SH-QTARGET ( n -- n )
   SH-QFIND dup 0 < if exit then
   SH-QROW @ SITE-SHADOW-TAG or ;

\ ---- what a host address names -------------------------------------------------
\ A shipped window record's entry, by its shipped row, or a word of the engine's
\ own prefix, by the name the ARM64 named code sites carry (ACAP-TARGET-NAME?: the
\ name resolves to exactly this entry below the prelude mark).
: SH-TARGET ( n n -- n ) {: off:n v:n :}
   v ACAP-TGT>REC {: j:n :}
   j ACAP-W-R0 @ >= j ACAP-W-R1 @ < and if
      j SH-MAPPED dup -1 = if
         drop off j SH-UNSHIPPED
      then exit
   then
   j 0 < if v SH-QTARGET dup 0 >= if exit then drop then
   v ACAP-TARGET-NAME? if ACAP-POOL-ADD SITE-NAME-TAG or exit then 2drop
   off v SH-UNNAMED ;

\ A call or a branch that leaves the routine. Its rel32 is the zero an unplaced
\ emission writes, so only the row changes.
: SH-CALL ( n n -- ) {: e:n k:n :}
   e k NSHADOW:CALL-SITE@ {: off:n :}
   e k NSHADOW:CALL-KIND@ {: nk:n :}
   nk NEMIT:CALL = nk NEMIT:TAIL = or 0= if off nk SH-BAD-KIND then
   nk NEMIT:TAIL = if AOT-SHADOW:TAIL else AOT-SHADOW:CALL then {: kind:n :}
   off  e k NSHADOW:CALL-TARGET@  SH-TARGET {: target:n :}
   SH-AT @ off +  kind  target  SH-SITE+ ;

\ Where one of emission e's functions starts. The x86-64 emitter writes a
\ `codeaddr` as that offset and every other code literal as a host address in the
\ code region, which lies above every emission's size.
: SH-FUN? ( n n -- bool ) {: e:n v:n :}
   v 0 <  v e NSHADOW:SIZE >= or if false exit then
   e NSHADOW:FUNCTIONS 0 ?do
      e i NSHADOW:FUNCTION-OFFSET@ v = if true unloop exit then
   loop
   false ;

\ CAPTURE has already canonicalised AOT-DATA-D0 to the live base's residue, so the
\ live window is ACAP-W-D0 and a literal's window coordinate is what an ARM64
\ DATA site holds: its offset from the live base plus that residue.
: SH-DATA? ( n -- bool ) {: v:n :}
   v ACAP-W-D0 @ >=  v ACAP-W-D0 @ AOT-DATA-SIZE @ + <  and ;

: SH-MARK-ROW ( n -- ) {: r:n :}
   r 0 < if exit then
   r SH-MARK @ 0<> if exit then
   1 r SH-MARK !
   r SH-WORK-N @ SH-WORK !
   SH-WORK-N @ 1+ SH-WORK-N ! ;

\ A quotation's function entry is not a dictionary record. Only the exact
\ source address paired at publication can identify its x86 function; the
\ owner emission is then a reach root, so all of its recorded sites are seen.
: SH-QNEED ( n -- n ) {: xt:n :}
   xt SH-QFIND dup 0 >= if exit then drop
   xt NSHADOW:SOURCE-FUNCTION {: e:n f:n :}
   e 0 < if -1 exit then
   e SH-EMROOT @ {: root:n :}
   root 0 < if -1 exit then
   SH-QN @ {: q:n :}
   q 1+ SH-QXT-RESERVE  q 1+ SH-QEM-RESERVE
   q 1+ SH-QFUN-RESERVE  q 1+ SH-QROW-RESERVE
   xt q SH-QXT !  e q SH-QEM !  f q SH-QFUN !  -1 q SH-QROW !
   q 1+ SH-QN !
   root SH-MARK-ROW
   q ;

: SH-MARK-TARGET ( n -- ) {: v:n :}
   v ACAP-TGT>REC {: idx:n :}
   idx 0 >= if idx SH-SOURCE@ SH-MARK-ROW exit then
   v SH-QNEED drop ;

\ The second target's edges are the shadow emitter's recorded rows, not the
\ ARM instruction graph. A target can be live only on x86-64, or ARM can retain
\ a body whose x86-64 routine has no incoming edge.
: SH-SCAN ( n -- ) {: r:n :}
   r NSHADOW:RECORD@ SH-REC !
   r NSHADOW:EMISSION@ {: e:n :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-TARGET@ SH-MARK-TARGET
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ HIR:ADDR-CODE = if
         e i NSHADOW:ADDR-SITE@ {: off:n :}
         off 0 < off ADDRESS-CARRIER:MOVABS-BYTES + e NSHADOW:SIZE > or if
            off SH-NOT-MOVABS then
         e NSHADOW:BYTES off + {: p:ptr :}
         p ADDRESS-CARRIER:MOVABS-SITE? 0= if off SH-NOT-MOVABS then
         p ADDRESS-CARRIER:MOVABSV {: v:n :}
         e v SH-FUN? 0= if v SH-MARK-TARGET then
      then
   loop ;

\ A declared CODE cell is a root even if the word that stores it is private.
\ Return its live target, or zero for a DATA, null or named-prefix cell.
: SH-CELL-OFF ( n -- n ) {: row:n :}
   row ACAP-XTOFF@ {: loc:n :}
   loc AOT-WINDOW:XTOFF-LOC-MASK and {: off:n :}
   loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0<> if
      off ACAP-W-D0 @ AOT-DATA-N - +
   else off then ;

: SH-CELL-V ( n -- n ) {: row:n :}
   row ACAP-XTMETA@ {: meta:n :}
   meta AOT-WINDOW:XTOFF-KIND-MASK and 0<> if 0 exit then
   meta AOT-WINDOW:XTOFF-VALUE-MASK and 0= if 0 exit then
   AOT-LIVE-DATA row SH-CELL-OFF + AOT-CELL@ ;

: SH-REACH ( -- )
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ SH-HEAD@ 0<> if i SH-MARK-ROW then
   loop
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i SH-CELL-V dup 0<> if SH-MARK-TARGET else drop then
   loop
   begin SH-WORK-N @ 0 > while
      SH-WORK-N @ 1- SH-WORK-N !
      SH-WORK-N @ SH-WORK @ SH-SCAN
   repeat ;

\ Reserve each reached map row's place in publication order before copying any
\ emission: a call can target a routine published later than its caller.
: SH-QASSIGN ( n -- ) {: e:n :}
   SH-QN @ 0 ?do
      i SH-QEM @ e = if
         SH-OUT-N @ i SH-QROW !
         1 SH-OUT-N +!
      then
   loop ;

: SH-ASSIGN ( -- )
   NSHADOW:RECORDS 0 ?do
      i SH-MARK @ 0<> if
         i NSHADOW:EMISSION@ {: e:n :}
         SH-OUT-N @ i SH-OUT !
         i NSHADOW:RECORD@ SH-HEAD@ {: head:n :}
         head 0= if
            1 SH-OUT-N +!
         else
            head begin dup 0<> while
               1 SH-OUT-N +!
               1- SH-NEXT @
            repeat drop
         then
         i e SH-EMROOT @ = if e SH-QASSIGN then
      then
   loop ;

\ An address literal, a MOVABS whose kind HIR's elaboration fixed and the x86-64
\ rows carried through (x64ir.f spells HIR's kinds). A function's offset stays; a
\ word's entry becomes its row's target and the field 0; a window DATA address
\ becomes its window coordinate.
: SH-ADDR ( n n -- ) {: e:n k:n :}
   e k NSHADOW:ADDR-SITE@ {: off:n :}
   e k NSHADOW:ADDR-SITE-KIND@ {: ak:n :}
   off 0 <  off ADDRESS-CARRIER:MOVABS-BYTES + SH-LEN @ > or if off SH-NOT-MOVABS then
   AOT-SHADOW:CODE-BUF@ SH-AT @ + off + {: p:ptr :}
   p ADDRESS-CARRIER:MOVABS-SITE? 0= if off SH-NOT-MOVABS then
   p ADDRESS-CARRIER:MOVABSV {: v:n :}
   SH-AT @ off + {: at:n :}
   ak HIR:ADDR-CODE = if
      e v SH-FUN? if at AOT-SHADOW:FUN 0 SH-SITE+ exit then
      off v SH-TARGET {: target:n :}
      p 0 ADDRESS-CARRIER:SET-MOVABS
      at AOT-SHADOW:CODE target SH-SITE+ exit
   then
   ak HIR:ADDR-DATA = if
      v SH-DATA? 0= if off v SH-DATA-OUT then
      p  v ACAP-W-D0 @ - AOT-DATA-D0 @ +  ADDRESS-CARRIER:SET-MOVABS
      at AOT-SHADOW:DATA 0 SH-SITE+ exit
   then
   off ak SH-BAD-KIND ;

\ A defer's native record carries a magic and dispatch cell after its body.
\ Copy that trailer into the shadow and mark the cell for the x86 linker.
: SH-DEFER-CELL ( n -- n ) {: idx:n :}
   idx AOT-REC {: rec:ptr :}
   rec AOT-RWID DICT-WL:NAMESPACE = if 0 exit then
   rec AOT-RXT rec AOT-RBODY + {: meta:n :}
   meta AOT-N>U8 CELL-VIEW AOT-CELL@ DEFER-MAGIC <> if 0 exit then
   meta CELL + AOT-N>U8 CELL-VIEW AOT-CELL@ ;

CELL 2 * constant SH-TRAILER-BYTES

: SH-TRAILER ( n -- ) {: v:n :}
   v SH-DATA? 0= if SH-LEN @ CELL + v SH-DATA-OUT then
   AOT-SHADOW:CODE-LEN @ {: at:n :}
   at SH-TRAILER-BYTES + AOT-SHADOW:CODE-LEN !
   AOT-SHADOW:CODE-BUF@ at + {: p:ptr :}
   DEFER-MAGIC p AOT-N-C!
   v ACAP-W-D0 @ - AOT-DATA-D0 @ +  p CELL + AOT-N-C!
   at CELL + AOT-SHADOW:DCELL 0 SH-SITE+ ;

\ ---- one emission ---------------------------------------------------------------
: SH-COPY ( n -- ) {: e:n :}
   e NSHADOW:SIZE {: size:n :}
   AOT-SHADOW:CODE-LEN @ {: at:n :}
   at size + AOT-SHADOW:CODE-LEN !
   e NSHADOW:BYTES  AOT-SHADOW:CODE-BUF@ at +  size BYTE-COPY
   e SH-E !  at SH-AT !  size SH-LEN !
   e NSHADOW:CALL-SITES 0 ?do e i SH-CALL loop
   e NSHADOW:ADDR-SITES 0 ?do e i SH-ADDR loop
   SH-REC @ SH-DEFER-CELL {: cell:n :}
   cell 0<> if cell SH-TRAILER then ;

: SH-ORDER ( -- )
   -1 SH-PREV !
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ {: idx:n :}
      idx SH-PREV @ <= if idx SH-ORDER-REFUSE then
      idx SH-PREV !
   loop ;

: SH-QCOPY ( n -- ) {: e:n :}
   SH-QN @ 0 ?do
      i SH-QEM @ e = if
         AOT-SHADOW:RETIRED-REC
         e i SH-QFUN @ NSHADOW:FUNCTION-OFFSET@ SH-REC+
      then
   loop ;

: SH-WALK ( -- )
   -1 SH-E !
   NSHADOW:RECORDS 0 ?do
      i SH-MARK @ 0<> if
         i NSHADOW:RECORD@ {: idx:n :}
         idx SH-REC !
         i NSHADOW:EMISSION@ {: e:n :}
         e SH-E @ <> if e SH-COPY then
         idx SH-HEAD@ {: head:n :}
         head 0= if
            idx ACAP-DICT>CAP {: k:n :}
            k 0 >= if k AOT-SHADOW:ANON-REC or else AOT-SHADOW:RETIRED-REC then
            i NSHADOW:ENTRY@ SH-REC+
         else
            head begin dup 0<> while
               dup 1- SH-ROWS @ i NSHADOW:ENTRY@ SH-REC+
               1- SH-NEXT @
            repeat drop
         then
         i e SH-EMROOT @ = if e SH-QCOPY then
      then
   loop ;

\ ---- the address cells ---------------------------------------------------------
\ An address-cell row whose target is window code carries a blob offset, which
\ names an ARM64 entry; the target's image needs the record it enters. The live
\ cell still holds the host entry the row was classified from, at the location
\ the row states (ACAP-XTCELL-LOC), so the record comes from the same index, and
\ the cell names its shipped row.
: SH-XTCELL ( n -- ) {: row:n :}
   row ACAP-XTMETA@ {: meta:n :}
   meta AOT-WINDOW:XTOFF-KIND-MASK and 0<> if exit then
   meta AOT-WINDOW:XTOFF-VALUE-MASK and 0= if exit then
   row SH-CELL-OFF {: celloff:n :}
   row SH-CELL-V {: v:n :}
   v ACAP-TGT>REC {: idx:n :}
   idx 0 >= if idx SH-MAPPED else v SH-QTARGET then {: target:n :}
   target 0 < if celloff v SH-XT-REFUSE then
   target SITE-TARGET-MASK invert and SITE-REC-TAG = if
      target SITE-TARGET-MASK and
   else target then {: key:n :}
   AOT-SHADOW:XT-N @ {: x:n :}
   x 1+ AOT-SHADOW:XT-N !
   AOT-SHADOW:XT-BUF@ x AOT-SHADOW:XT-ROW * + {: r:ptr :}
   row r AOT-P32!  key r 4 + AOT-P32! ;

public

\ Fill the shadow tables from an open shadow's map, after CAPTURE and against the
\ dictionary it captured.
: SHADOW-CAPTURE ( -- )
   ACAP-TIDX-ND @ ndict@ <> if
      s" aot-capture: the shadow is read only right after the capture it belongs to" 74 die
   then
   AOT-SHADOW:RESET
   NSHADOW:OPEN? 0= if exit then
   SH-ORDER
   SH-NUMBER
   SH-INDEX
   SH-REACH
   SH-ASSIGN
   SH-WALK
   AOT-WINDOW:XTOFF-N @ 0 ?do i SH-XTCELL loop ;

;using
;package
