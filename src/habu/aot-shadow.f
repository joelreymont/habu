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
\ WHEN IT RUNS. TARGET-CAPTURE prepares the capture, retains live private emitted
\ records from this map, completes the capture, then SHADOW-CAPTURE reads the
\ tables. The reader refuses to run against any other dictionary. It reads
\ the map before anything closes the shadow: NCOMP:CAPTURE-PREPARE closes the
\ shadow of the compiler that runs it, and a native build's preparation of its
\ target (tools/native-build-core.f PREPARE-TARGET) runs the target window's own
\ compiler, not the host's that filed the map. With no shadow open the tables stay
\ as CAPTURE left them, empty.
\
\ ONE WALK, KEYED BY THE SHIPPED ROW. The map's rows are filed in publication
\ order, so their records ascend: the walk keeps the rows whose record is in the
\ window and copies each emission once, when the first row filed over it arrives -
\ a `does>` companion follows its definer and shares its emission. Records that
\ do not ascend are a map from another dictionary history, filed before a logical
\ reset, and are refused rather than read. The artifact's record table holds only
\ the records the capture ships, and a dead stripped private word moves every later
\ one down a row, so a routine, a site target and a code cell each name their
\ record by its row in that table (SH-NUMBER), never by its window index. Nor is
\ the window index the capture's own: the capture does not record a retired
\ record at all, so the walk reads a capture table only through ACAP-DICT>CAP.
\ A stripped or retired record files no row - a retired one has no name to ship.
\ A shipped record over the same emission carries a live one's routine (a
\ private definer's companion, kept for the children it made), a live one
\ nothing carries is refused by name, as is a site naming a stripped or retired
\ record, and a dead one's routine goes: no live code reaches its body
\ (ACAP-SHADOW-LIVE?), though the ARM64 payload may keep that body, as it keeps
\ every retired one and what such a body calls. Every host address a routine names
\ is resolved here, through the xt -> record index the ARM64 call sites use
\ (ACAP-TGT>REC), into a shipped record or into the name of a word of the
\ engine's own prefix; a host address nothing resolves is refused by name, as an
\ ARM64 site is.

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

: SH-STRIPPED ( -- )
   s" aot-capture: the shadow routine of " type SH-REC @ ACAP-NAME.
   s"  is live, and the capture ships neither its record nor one sharing its emission" type cr
   s" aot-capture: a live shadow routine no shipped record carries" 74 die ;

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

\ The shipped row of dictionary record idx, or -1 when it lies outside the window
\ or the capture retires or strips it.
: SH-SHIPPED ( n -- n )
   ACAP-DICT>CAP {: k:n :}
   k 0 < if -1 exit then
   k SH-ROWS @ ;

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

\ ---- what a host address names -------------------------------------------------
\ A shipped window record's entry, by its shipped row, or a word of the engine's
\ own prefix, by the name the ARM64 named code sites carry (ACAP-TARGET-NAME?: the
\ name resolves to exactly this entry below the prelude mark).
: SH-TARGET ( n n -- n ) {: off:n v:n :}
   v ACAP-TGT>REC {: j:n :}
   j ACAP-W-R0 @ >= j ACAP-W-R1 @ < and if
      j SH-SHIPPED {: row:n :}
      row 0 < if off j SH-UNSHIPPED then
      row SITE-REC-TAG or exit
   then
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

\ ---- one emission ---------------------------------------------------------------
: SH-COPY ( n -- ) {: e:n :}
   e NSHADOW:SIZE {: size:n :}
   AOT-SHADOW:CODE-LEN @ {: at:n :}
   at size + AOT-SHADOW:CODE-LEN !
   e NSHADOW:BYTES  AOT-SHADOW:CODE-BUF@ at +  size BYTE-COPY
   e SH-E !  at SH-AT !  size SH-LEN !
   e NSHADOW:CALL-SITES 0 ?do e i SH-CALL loop
   e NSHADOW:ADDR-SITES 0 ?do e i SH-ADDR loop ;

: SH-ORDER ( -- )
   -1 SH-PREV !
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ {: idx:n :}
      idx SH-PREV @ <= if idx SH-ORDER-REFUSE then
      idx SH-PREV !
   loop ;

\ Only an emitted private routine reached by live code needs its own dictionary
\ row. The map row proves emission; the capture index excludes retired records.
: SH-KEEP-ONE ( n -- ) {: idx:n :}
   idx ACAP-DICT>CAP {: k:n :}
   k 0 < if exit then
   idx ACAP-SHADOW-LIVE? 0= if exit then
   k ACAP-REC-DST {: rec:ptr :}
   rec 40 + ACAP-W32@ ACAP-PRIVATE? 0= if exit then
   rec ACAP-INTERNAL? if exit then
   k ACAP-NAMED-BIT @ 0= if 1 k ACAP-NAMED-BIT ! then ;

: SH-KEEP-PRIVATE ( -- )
   NSHADOW:RECORDS 0 ?do i NSHADOW:RECORD@ SH-KEEP-ONE loop ;

\ Whether map row r is over emission e and files a record the capture ships: a
\ `does>` definer and its companion are filed together, two adjacent rows over one
\ emission.
: SH-OWNER? ( n n -- bool ) {: r:n e:n :}
   r 0 <  r NSHADOW:RECORDS >= or if false exit then
   r NSHADOW:EMISSION@ e <> if false exit then
   r NSHADOW:RECORD@ SH-SHIPPED 0 >= ;

\ Map row r files a record the capture ships no row for. A shipped row over its
\ emission carries its routine; otherwise a dead one goes with its code, and a
\ live one is refused. Stripped and retired alike, a record is live when the
\ capture's shadow reach reaches its body from live code (ACAP-SHADOW-LIVE?), not
\ when ARM64 retention keeps it: that retention scans every gap as a root, and a
\ retired body is a gap, live or not.
: SH-STRIP ( n -- ) {: r:n :}
   r NSHADOW:EMISSION@ {: e:n :}
   r 1- e SH-OWNER?  r 1+ e SH-OWNER? or if exit then
   r NSHADOW:RECORD@ ACAP-SHADOW-LIVE? if SH-STRIPPED then ;

: SH-WALK ( -- )
   -1 SH-E !
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ {: idx:n :}
      idx ACAP-W-R0 @ >=  idx ACAP-W-R1 @ <  and if
         idx SH-REC !
         idx SH-SHIPPED {: row:n :}
         row 0 >= if
            i NSHADOW:EMISSION@ {: e:n :}
            e SH-E @ <> if e SH-COPY then
            row  i NSHADOW:ENTRY@  SH-REC+
         else
            i SH-STRIP
         then
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
   row ACAP-XTOFF@ {: loc:n :}
   loc AOT-WINDOW:XTOFF-LOC-MASK and {: off:n :}
   loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0<> if
      off ACAP-W-D0 @ AOT-DATA-N - +
   else off then {: celloff:n :}
   AOT-LIVE-DATA celloff + AOT-CELL@ {: v:n :}
   v ACAP-TGT>REC SH-SHIPPED {: k:n :}
   k 0 < if celloff v SH-XT-REFUSE then
   AOT-SHADOW:XT-N @ {: x:n :}
   x 1+ AOT-SHADOW:XT-N !
   AOT-SHADOW:XT-BUF@ x AOT-SHADOW:XT-ROW * + {: r:ptr :}
   row r AOT-P32!  k r 4 + AOT-P32! ;

public

\ Fill the shadow tables from an open shadow's map after capture completion and
\ against the dictionary it captured.
: SHADOW-CAPTURE ( -- )
   ACAP-TIDX-ND @ ndict@ <> if
      s" aot-capture: the shadow is read only right after the capture it belongs to" 74 die
   then
   AOT-SHADOW:RESET
   NSHADOW:OPEN? 0= if exit then
   SH-ORDER
   SH-NUMBER
   SH-WALK
   AOT-WINDOW:XTOFF-N @ 0 ?do i SH-XTCELL loop ;

: TARGET-CAPTURE ( n n n n n n -- )
   {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart bend rstart rend d0 d1 CAPTURE-PREPARE
   SH-KEEP-PRIVATE
   bstart bend d0 CAPTURE-COMPLETE
   SHADOW-CAPTURE ;

;using
;package
