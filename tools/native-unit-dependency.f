\ Observed inputs to one native definition and to one source file. Section 14
\ carries declaration identities, checked contracts, frozen primitive schemas
\ and fixed values; anything it cannot re-verify is an ORDERED row. A nested
\ definition keeps a separate span in the definition arena. A file frame holds
\ one row per kind and key, only about records the file did not make itself:
\ those before its first ordinal or made by a child file it loaded.

require src/lib/errors.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/sort.f
require src/core/sha256.f
require src/core/checker-owner-abi.f
require src/core/checker-owner-extension.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/aot-data-cell.f
require src/habu/address-cells.f
require src/compiler/native/dict.f
require src/compiler/native/host.f
require src/compiler/native/checker-owner.f
require src/compiler/native/feed.f
require tools/native-file-reference.f
require tools/native-file-scope.f
require tools/native-unit-capture.f

package NUNIT-DEP
private

1 constant BINDING
2 constant VALUE
3 constant EXECUTED
4 constant ORDERED
5 constant CHECKER-ROLE
6 constant SCOPE
7 constant RESOLUTION
8 constant PRIMITIVE
9 constant COMPILER-ROLE
10 constant READER-FLAGS
11 constant DATA
16 constant HEADER-BYTES
24 constant ROW-BYTES
48 constant RES-ROW-BYTES
32 constant RES-KEY-BYTES
32 constant PRIMITIVE-HEAD
\ A file frame's cells: its row offset, its first record ordinal, where its
\ children's record ranges start, where its first ORDERED reason starts, and
\ its entry here.
0 constant F-OFF
1 constant F-FIRST
2 constant F-RBASE
3 constant F-WHY
4 constant F-HERE
5 constant FRAME-CELLS
32 constant DIGEST-BYTES
\ A DATA address read at compile time is an integer.
CAST: N>U8 ( n -- ptr u8 )
1024 constant SLOTS-MIN
$cbf29ce484222325 constant FNV-BASIS
$100000001b3 constant FNV-PRIME

DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER FRAMES n
DYNAMIC-BUFFER RES-KEY u8
DYNAMIC-BUFFER PRIMITIVE-DATA u8
\ File frames: rows, frame cells, closed child ranges (first, end) of every
\ open frame, first ORDERED reasons, and the slots indexing the open rows.
DYNAMIC-BUFFER FBYTES u8
DYNAMIC-BUFFER FFRAMES n
DYNAMIC-BUFFER FRANGES n
DYNAMIC-BUFFER FWHY u8
DYNAMIC-BUFFER FSLOTS n
\ Each open definition frame's ordinal and entry here, two cells per depth.
DYNAMIC-BUFFER DLIMITS n
\ A closure walk: the epoch stamp of each record it reached, its pending
\ records, its members, and its digest context and result.
DYNAMIC-BUFFER SEEN n
DYNAMIC-BUFFER WSTACK n
DYNAMIC-BUFFER MEMBERS n
DYNAMIC-BUFFER HWORK u8
\ The declared address cells inside one DATA extent, as offsets into it.
DYNAMIC-BUFFER PTRS n
variable PTR-N
\ A run's queue: the epoch stamp of each record queued, and the queue of the
\ file's own records to walk through and the other records to capture.
DYNAMIC-BUFFER RMARK n
DYNAMIC-BUFFER RQUEUE n
\ The DATA origins of the records below OW, two cells an entry sorted by
\ origin then ordinal; per ordinal below OW the greatest origin indexed at or
\ below it; and the records that had no code yet or were retired when indexed.
DYNAMIC-BUFFER OIDX n
DYNAMIC-BUFFER OPMAX n
DYNAMIC-BUFFER OPEND n
0 CWORK-VARIABLE OI-N n
0 CWORK-VARIABLE OW n
0 CWORK-VARIABLE OPEND-N n
0 CWORK-VARIABLE SEEN-N n
0 CWORK-VARIABLE EPOCH n
0 CWORK-VARIABLE WSP n
0 CWORK-VARIABLE MEM-N n
0 CWORK-VARIABLE WCAPTURE bool
0 CWORK-VARIABLE WOK bool
0 CWORK-VARIABLE WTRANS bool
0 CWORK-VARIABLE WROOT n
0 CWORK-VARIABLE RMARK-N n
0 CWORK-VARIABLE REPOCH n
0 CWORK-VARIABLE RQ-N n
0 CWORK-VARIABLE FAIL-P ptr u8
0 CWORK-VARIABLE FAIL-U n
0 CWORK-VARIABLE FAIL-ORD n
0 CWORK-VARIABLE S-OWNER n
0 CWORK-VARIABLE S-BASE n
0 CWORK-VARIABLE S-END n
0 CWORK-VARIABLE USED n
0 CWORK-VARIABLE DEPTH n
0 CWORK-VARIABLE FUSED n
0 CWORK-VARIABLE FDEPTH n
0 CWORK-VARIABLE FRN n
0 CWORK-VARIABLE WHYU n
0 CWORK-VARIABLE FCAP n
0 CWORK-VARIABLE FLIVE n
0 CWORK-VARIABLE CHECK-OFF n
0 CWORK-VARIABLE CHECK-OK bool
0 CWORK-VARIABLE MISS-P ptr u8
0 CWORK-VARIABLE MISS-U n
0 CWORK-VARIABLE SCAN-OFF n
create CELL-BYTES 8 allot

\ The checker owner installs an exact-occurrence query at its publication
\ boundary. An older owner has none, and that FILE is explicitly ordered.
defer CONTRACT$ ( n -- ptr u8 n bool )
: NO-CONTRACT ( n -- ptr u8 n bool ) drop NULL-PTR 0 false ;
defer CHECKER-ROLE$ ( -- ptr u8 n bool )
: NO-ROLE ( -- ptr u8 n bool ) NULL-PTR 0 false ;
defer COMPILER-ROLE$ ( -- ptr u8 n bool )
defer PRIMITIVE-STATE ( n n -- n bool )
: NO-PRIMITIVE ( n n -- n bool ) 2drop 0 false ;
defer SYM-IDENTITY ( n -- ptr u8 n ptr u8 n n )
: NO-IDENTITY ( n -- ptr u8 n ptr u8 n n )
   drop NULL-PTR 0 NULL-PTR 0 0 ;
defer ORIGIN-SYM ( ptr u8 n n ptr u8 n -- n )
: NO-ORIGIN ( ptr u8 n n ptr u8 n -- n )
   2drop drop 2drop 0 ;
: INSTALL-DEFAULT ( -- )
   ['] NO-CONTRACT is CONTRACT$
   ['] NO-ROLE is CHECKER-ROLE$
   ['] NO-ROLE is COMPILER-ROLE$
   ['] NO-PRIMITIVE is PRIMITIVE-STATE
   ['] NO-IDENTITY is SYM-IDENTITY
   ['] NO-ORIGIN is ORIGIN-SYM ;
INSTALL-DEFAULT

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: MALFORMED ( -- ) E-NUNIT-FILE throw ;

: FRAME ( -- n )
   DEPTH @ 0 <= if BAD then
   DEPTH @ 1- FRAMES @ ;

\ Field n of the innermost file frame.
: FFIELD ( n -- ptr n )
   FDEPTH @ 0 <= if BAD then
   FDEPTH @ 1- FRAME-CELLS * + FFRAMES ;

: OPEN? ( -- bool ) DEPTH @ 0 > FDEPTH @ 0 > or ;

\ A record the innermost file made itself: at or after its first ordinal and
\ outside every child file it loaded. Rows about it are internal to the file.
: OWN? ( n -- bool )
   {: ord:n :}
   FDEPTH @ 0= if false exit then
   ord F-FIRST FFIELD @ < if false exit then
   FRN @ F-RBASE FFIELD @ ?do
      ord i FRANGES @ >= ord i 1+ FRANGES @ < and if false unloop exit then
   2 +loop
   true ;

\ No open frame keeps a row about this record.
: SKIP? ( n -- bool )
   DEPTH @ 0 > if drop false exit then
   OWN? FDEPTH @ 0= or ;

\ A package record the file made and loaded no child file since: every
\ lookup in it is decided by the file's own source. Its absence before the
\ file is the `package` decision row's fact (OBSERVE-RESOLUTION).
: OWN-NAMESPACE? ( n -- bool )
   {: ns:n :}
   ns OWN? 0= if false exit then
   FRN @ F-RBASE FFIELD @ = if true exit then
   ns FRN @ 1- FRANGES @ >= ;

: ROOM ( n -- n )
   {: need:n :}
   need 0 < need MEM-MAX-N USED @ - > or if BAD then
   USED @ need + BYTES-RESERVE
   USED @ dup need + USED ! ;

: FROOM ( n -- n )
   {: need:n :}
   need 0 < need MEM-MAX-N FUSED @ - > or if BAD then
   FUSED @ need + FBYTES-RESERVE
   FUSED @ dup need + FUSED ! ;

: ROW-SIZE ( n n n -- n )
   {: kind:n keyu:n datau:n :}
   keyu 0 < datau 0 < or if BAD then
   kind ORDERED <> kind CHECKER-ROLE <> and kind COMPILER-ROLE <> and
      kind SCOPE <> and
      keyu 0= and if BAD then
   keyu MEM-MAX-N ROW-BYTES - > if BAD then
   datau MEM-MAX-N ROW-BYTES - keyu - > if BAD then
   ROW-BYTES keyu + datau + ;

\ A zero-length field ends exactly at the reserved bound for a trailing
\ ORDERED row; nothing is copied there.
: ROW! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: row:ptr kind:n key:ptr keyu:n data:ptr datau:n :}
   kind row LE:U64!
   keyu row 8 + LE:U64!
   datau row 16 + LE:U64!
   keyu 0 > if key row ROW-BYTES + keyu BYTE-COPY then
   datau 0 > if data row ROW-BYTES keyu + + datau BYTE-COPY then ;

: COUNT+ ( ptr u8 -- ) 8 + dup LE:U64@ 1+ swap LE:U64! ;

: DEF-ROW ( n ptr u8 n ptr u8 n -- )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   kind keyu datau ROW-SIZE ROOM {: off:n :}
   off BYTES kind key keyu data datau ROW!
   FRAME BYTES COUNT+ ;

: KEY-HASH ( n ptr u8 n -- n )
   {: kind:n key:ptr keyu:n :}
   FNV-BASIS kind xor FNV-PRIME *
   keyu 0 ?do key i + c@ xor FNV-PRIME * loop ;

: ROW-SPAN ( ptr u8 -- n )
   dup 8 + LE:U64@ swap 16 + LE:U64@ + ROW-BYTES + ;

: SAME-ROW? ( n n ptr u8 n -- bool )
   {: off:n kind:n key:ptr keyu:n :}
   off FBYTES {: row:ptr :}
   row LE:U64@ kind <> if false exit then
   row 8 + LE:U64@ keyu <> if false exit then
   keyu 0= if true exit then
   row ROW-BYTES + keyu key keyu STR= ;

\ Whether slot idx holds kind and key, and whether it ends the probe. A slot
\ below the innermost frame belongs to an outer frame and never matches.
: SLOT-END ( n n ptr u8 n -- bool bool )
   {: idx:n kind:n key:ptr keyu:n :}
   idx FSLOTS @ {: v:n :}
   v 0= if false true exit then
   v 1- F-OFF FFIELD @ < if false false exit then
   v 1- kind key keyu SAME-ROW? dup ;

\ The slot holding the innermost frame's row for kind and key, else the empty
\ slot that ends its probe.
: PROBE ( n ptr u8 n -- n bool )
   {: kind:n key:ptr keyu:n :}
   kind key keyu KEY-HASH FCAP @ 1- and
   begin dup kind key keyu SLOT-END 0= while
      drop 1+ FCAP @ 1- and
   repeat ;

\ Index the row at off in the first empty slot of its probe; the next row.
: PLACE ( n -- n )
   {: off:n :}
   off FBYTES {: row:ptr :}
   row LE:U64@ row ROW-BYTES + row 8 + LE:U64@ KEY-HASH FCAP @ 1- and
   begin dup FSLOTS @ 0<> while 1+ FCAP @ 1- and repeat
   off 1+ swap FSLOTS !
   off row ROW-SPAN + ;

: INDEX-FRAME ( n -- )
   FRAME-CELLS * F-OFF + FFRAMES @ {: off:n :}
   off HEADER-BYTES +
   off FBYTES 8 + LE:U64@ 0 ?do PLACE loop
   drop ;

\ Outer frames first, so an outer row's probe never passes an inner slot and
\ closing a frame can empty its slots alone.
: REINDEX ( n -- )
   {: cap:n :}
   cap FSLOTS-RESERVE
   cap FCAP !
   cap 0 ?do 0 i FSLOTS ! loop
   FDEPTH @ 0 ?do i INDEX-FRAME loop ;

: FILE-ROW ( n ptr u8 n ptr u8 n -- )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   FLIVE @ 1+ 2 * FCAP @ > if FCAP @ 2 * SLOTS-MIN max REINDEX then
   kind key keyu PROBE {: idx:n found:bool :}
   found if exit then
   kind keyu datau ROW-SIZE FROOM {: off:n :}
   off FBYTES kind key keyu data datau ROW!
   off 1+ idx FSLOTS !
   1 FLIVE +!
   F-OFF FFIELD @ FBYTES COUNT+ ;

\ The definition frame takes each observation; the file frame one row per
\ kind and key about a record it did not make.
: ADD-ROW ( n n ptr u8 n ptr u8 n -- )
   {: ord:n kind:n key:ptr keyu:n data:ptr datau:n :}
   OPEN? 0= if BAD then
   DEPTH @ 0 > if kind key keyu data datau DEF-ROW then
   FDEPTH @ 0 > ord OWN? 0= and if kind key keyu data datau FILE-ROW then ;

: BINDING-ROW ( n ptr u8 n ptr u8 n -- )
   {: ord:n key:ptr keyu:n contract:ptr contractu:n :}
   contractu 0= if BAD then
   ord BINDING key keyu contract contractu ADD-ROW ;

: VALUE-ROW ( n ptr u8 n ptr u8 n -- )
   {: ord:n key:ptr keyu:n result:ptr resultu:n :}
   resultu 8 <> if BAD then
   ord VALUE key keyu result resultu ADD-ROW ;

: REMEMBER-MISS ( ptr u8 n -- )
   MISS-P @ NULL-PTR = if
      MISS-U ! MISS-P !
   else 2drop then ;

: CURRENT ( ptr u8 n -- ptr n n bool )
   NFILE-REF:KEY-RECORD {: rec:ptr ord:n :}
   ord 0 < if rec ord false exit then
   ord NFILE-REF:RECORD-RETIRED? if rec ord false exit then
   rec ord true ;

\ A word run at compile time reaches records through its result edges and
\ through the addresses its data holds. The walk visits each record once,
\ keeps the members whose results it compares, and hashes each data record
\ it reaches; a capturing walk also makes that hash a DATA row. The first
\ record it cannot account for stops it.
: HCTX ( -- ptr u8 ) 0 HWORK ;
: HOUT ( -- ptr u8 ) SHA256-CTX-BYTES HWORK ;
: H-BYTES ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 > if HCTX a u SHA256-FEED then ;
: H-N ( n -- ) CELL-BYTES LE:U64! CELL-BYTES 8 H-BYTES ;
: H-S ( ptr u8 n -- )
   {: a:ptr u:n :}
   u H-N a u H-BYTES ;

: HERE-N ( -- n ) here BYTE-VIEW NULL-PTR BYTE-VIEW - ;

: FAIL ( ptr u8 n n -- )
   {: why:ptr whyu:n ord:n :}
   WOK @ 0= if exit then
   false WOK ! why FAIL-P ! whyu FAIL-U ! ord FAIL-ORD ! ;

: WALK-BEGIN ( bool -- )
   {: capture:bool :}
   ndict@ {: n:n :}
   n SEEN-RESERVE
   n SEEN-N @ ?do 0 i SEEN ! loop
   n SEEN-N @ max SEEN-N !
   SHA256-CTX-BYTES DIGEST-BYTES + HWORK-RESERVE
   1 EPOCH +!
   0 WSP ! 0 MEM-N !
   capture WCAPTURE ! true WOK ! ;

: VISIT ( n -- )
   {: ord:n :}
   ord 0 < ord ndict@ >= or if BAD then
   ord SEEN @ EPOCH @ = if exit then
   EPOCH @ ord SEEN !
   WSP @ 1+ WSTACK-RESERVE
   ord WSP @ WSTACK !
   1 WSP +! ;

\ The record ordinal and here bounding a DATA extent read while ord's frame
\ compiles: the file's entry for a record from before the file, else the
\ open definition's entry, else the live state a check runs at.
: LIMITS ( n -- n n )
   {: ord:n :}
   FDEPTH @ 0 > if
      ord F-FIRST FFIELD @ < if F-FIRST FFIELD @ F-HERE FFIELD @ exit then
   then
   DEPTH @ 0 > if
      DEPTH @ 1- 2 * DLIMITS @ DEPTH @ 1- 2 * 1+ DLIMITS @ exit
   then
   ndict@ HERE-N ;

\ The origin index. A record enters it once, when a query first covers its
\ ordinal, so a query reads only the entries next to its address. Its facts
\ are then fixed: an origin can only vanish, by retirement, and each answer
\ re-reads the live origin of the entries it returns. A record with no code
\ yet or retired waits in OPEND until a query finds it settled. A dictionary
\ cut drops what it removed (DICT-CUT).
$8000000000000000 constant ORIGIN-FLOOR

: OI-O ( n -- n ) 2 * OIDX @ ;
: OI-R ( n -- n ) 2 * 1+ OIDX @ ;
: OI! ( n n n -- )
   {: j:n o:n r:n :}
   o j 2 * OIDX ! r j 2 * 1+ OIDX ! ;

\ Entry j-1 moves up to j.
: OI-SHIFT ( n -- )
   {: j:n :}
   j j 1- OI-O j 1- OI-R OI! ;

\ Entry j sorts after origin o of record r.
: OI-AFTER? ( n n n -- bool )
   {: j:n o:n r:n :}
   j OI-O o > if true exit then
   j OI-O o = j OI-R r > and ;

: OI-PUT ( n n -- )
   {: o:n r:n :}
   OI-N @ 1+ 2 * OIDX-RESERVE
   OI-N @ begin dup 0 > if dup 1- o r OI-AFTER? else false then while
      dup OI-SHIFT 1-
   repeat {: j:n :}
   j o r OI!
   1 OI-N +!
   OW @ r ?do i OPMAX @ o max i OPMAX ! loop ;

\ ord's DATA origin may still appear.
: UNSETTLED? ( n -- bool )
   XREF-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if true exit then
   rec XREF-START 0= if true exit then
   rec XREF-RETIRED? ;

: OI-ADD ( n -- )
   {: ord:n :}
   ord NFILE-REF:DATA-ORIGIN-BASE {: o:n known:bool :}
   known if o ord OI-PUT exit then
   ord UNSETTLED? if
      OPEND-N @ 1+ OPEND-RESERVE
      ord OPEND-N @ OPEND !
      1 OPEND-N +!
   then ;

public

\ A dictionary cut keeping kept records drops the origins it removed.
: DICT-CUT ( n -- )
   {: kept:n :}
   kept OW @ >= if exit then
   0 OI-N @ 0 ?do
      i OI-R kept < if dup i OI-O i OI-R OI! 1+ then
   loop OI-N !
   0 OPEND-N @ 0 ?do
      i OPEND @ kept < if i OPEND @ over OPEND ! 1+ then
   loop OPEND-N !
   kept OW ! ;

private

\ Index every record below limit that has settled.
: EXTEND ( n -- )
   {: limit:n :}
   \ A kernel without the cut notice lowers the count unseen; it writes no
   \ file object (NUNIT-OBJECT FILE-BEGIN), and the index drops what the cut did.
   ndict@ OW @ < if ndict@ DICT-CUT then
   limit OW @ > if
      limit OPMAX-RESERVE
      limit OW @ ?do
         i 0 > if i 1- OPMAX @ else ORIGIN-FLOOR then i OPMAX !
         i 1+ OW !
         i OI-ADD
      loop
   then
   0 OPEND-N @ 0 ?do
      i OPEND @ {: ord:n :}
      ord limit < ord UNSETTLED? 0= and if ord OI-ADD
      else ord over OPEND ! 1+ then
   loop OPEND-N ! ;

\ Entry j is a live origin of a record below limit.
: LIVE? ( n n -- bool )
   {: j:n limit:n :}
   j OI-R {: r:n :}
   r limit >= if false exit then
   r NFILE-REF:DATA-ORIGIN-BASE {: o:n known:bool :}
   known o j OI-O = and ;

\ The first entry whose origin is above at.
: ABOVE ( n -- n )
   {: at:n :}
   0 OI-N @ begin 2dup < while
      2dup + 2 / dup OI-O at <= if 1+ rot drop swap else nip then
   repeat drop ;

\ Below entry up: the greatest live origin and its least record.
: BELOW ( n n -- )
   {: up:n limit:n :}
   up 1- begin dup 0 >= if dup limit LIVE? 0= else false then while 1- repeat
   {: j:n :}
   j 0 < if exit then
   j OI-O S-BASE ! j OI-R S-OWNER !
   j 1- begin dup 0 >= if dup OI-O S-BASE @ = else false then while
      dup limit LIVE? if dup OI-R S-OWNER ! then
      1-
   repeat drop ;

\ From entry up: the least live origin, if below lim-here.
: UPPER ( n n n -- )
   {: up:n limit:n lim-here:n :}
   OI-N @ up ?do
      i limit LIVE? if i OI-O lim-here min S-END ! unloop exit then
   loop ;

\ Among the records below limit: the one with the greatest DATA origin at or
\ below at (-1 when none), that origin, and the least origin above at, else
\ lim-here.
: SPAN ( n n n -- n n n )
   {: at:n limit:n lim-here:n :}
   limit ndict@ min 0 max {: lim:n :}
   lim EXTEND
   at ABOVE {: up:n :}
   -1 S-OWNER ! 0 S-BASE ! lim-here S-END !
   up lim BELOW
   lim 0 > if lim 1- OPMAX @ at > if up lim lim-here UPPER then then
   S-OWNER @ S-BASE @ S-END @ ;

\ A variable's slot and a defer's cell may hold an address.
: POINTER-CELL? ( n -- bool )
   XREF-REC {: rec:ptr :}
   rec XREF-FLAGS DNAME-CELL and 0<> if true exit then
   rec NDICT:REC-DEFER-CELL 0<> ;

variable REACH-XT
variable FIND-XT
CAST: AS-FIND ( n -- [ ptr u8 n -- n ] )
CAST: AS-REACH ( n -- [ n -- n ] )
\ The scope-find flag "the bound record is a seeded primitive"
\ (src/compiler/native/dict.f, NDICT-private); SCOPE-FIND-AMBIGUOUS is the
\ global one (src/core/util.f).
1 constant SCOPE-SEEDED

\ The REACH class of a seed primitive's row (PRIM-SPEC:REACH@), 0 for any
\ other record. The spelling must bind this very record as a seeded
\ primitive: NDICT:SPELL-PRIM? answers true for a spelling that binds
\ nothing, and a package word whose tail spells a primitive binds its own
\ record. The producers are bound by name: `require src/habu/prims.f` is a
\ no-op on every host, and a retained host lacks REACH@.
: PRIM-REACH ( n -- n )
   {: ord:n :}
   REACH-XT @ 0= FIND-XT @ 0= or if 0 exit then
   ord XREF-REC XREF-NAME$ {: name:ptr nameu:n :}
   name nameu scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   used drop used2 drop
   flags SCOPE-FIND-AMBIGUOUS and 0<> if 0 exit then
   rec XREF-FOUND? 0= if 0 exit then
   rec NFILE-REF:REC-ORD ord <> if 0 exit then
   flags SCOPE-SEEDED and 0= if 0 exit then
   name nameu FIND-XT @ AS-FIND execute {: row:n :}
   row 0 < if 0 exit then
   row REACH-XT @ AS-REACH execute ;

\ Whether seed record ord is a sealed engine helper (src/habu/
\ primitive-registry.f HELPER-REGISTER): a routine a primitive's lowering
\ branches to, with no row and no effect of its own. The compiler emits
\ two (src/compiler/native/select.f): (PROT-SPAN), the guard of a store
\ whose body reason is MEMORY, and (DIV-ZERO), the trap of a division
\ whose body reason is UNKNOWN. The op's host reason classes the body, so
\ the edge to its helper is admitted. NDICT:HELPER-TARGET answers only a
\ record of the sealed wordlist inside the engine's text, by exact name;
\ the entry must be this record's own.
: ENGINE-HELPER? ( n -- bool )
   {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-NAME$ NDICT:HELPER-TARGET {: entry:n :}
   entry 0<> entry rec XREF-START = and ;

\ A seed record's kind: 0 code, 1 data, 2 value, 3 admitted: a primitive with a
\ REACH class or the engine helper a primitive's lowering branches to.
: SEED-KIND ( n -- n )
   {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-FLAGS {: flags:n :}
   flags DKIND:MASK and DKIND:VAL = if 2 exit then
   flags DNAME-CELL and 0<> flags DNAME-ALLOC and 0<> or
      flags DKIND:MASK and DKIND:ADDR = or rec NDICT:REC-DEFER-CELL 0<> or if 1 exit then
   ord PRIM-REACH 0<> if 3 exit then
   ord ENGINE-HELPER? if 3 exit then
   0 ;

\ The refusal a reached member's host reason names; SAFE and MEMORY have none.
: REASON-WHY$ ( n -- ptr u8 n )
   {: reason:n :}
   reason NHOST:CAST = if s" run-host-cast" exit then
   reason NHOST:INDIRECT = if s" run-host-indirect" exit then
   reason NHOST:UNCHECKED = if s" run-host-unchecked" exit then
   reason NHOST:FOREIGN = if s" run-host-foreign" exit then
   s" run-host-unknown" ;

: H-REF ( n n n -- )
   {: target:n addend:n holder:n :}
   target NFILE-REF:DECL-KEY? {: key:ptr keyu:n known:bool :}
   known 0= if s" executed-closure-unknown" holder FAIL exit then
   1 H-N key keyu H-S addend H-N
   target VISIT ;

\ v lies in the dictionary and code region (AOT-CELL keeps its own test
\ private, and a host built before an export must still load this file).
: REGION? ( n -- bool ) {: v:n :} v dbase@ >= v dbase@ REGION + < and ;

\ holder's pointer cell holds v: an address in the dictionary, code or the
\ DATA before the limits is its record's key and the offset into it, and the
\ walk follows that record. An address there that no record owns, or another
\ mapped address, is state no row can name; any other value is a number.
: H-POINTER ( n n n n -- )
   {: v:n holder:n limit:n lim-here:n :}
   v AOT-CELL:DICT-PTR? if
      v dbase@ - {: off:n :}
      off DREC / off DREC mod holder H-REF exit
   then
   v AOT-CELL:CODE-PTR? if
      v NFILE-REF:IMPL-OWNER {: owner:n found:bool :}
      found 0= if s" executed-closure-unknown" holder FAIL exit then
      owner v owner XREF-REC XREF-START - holder H-REF exit
   then
   v AOT-CELL:DATA-ADDRESS? v lim-here < and if
      v limit lim-here SPAN {: owner:n base:n end:n :}
      end drop
      owner 0 >= if owner v base - holder H-REF exit then
   then
   v AOT-CELL:DATA-ADDRESS? v REGION? or if
      s" executed-closure-unknown" holder FAIL exit
   then
   v AOT-CELL:MAPPED? if s" mapped-compile-time-read" holder FAIL exit then
   0 H-N v H-N ;

\ The cells in [start, end) the engine declared address cells
\ (TYPED-VARIABLE, PTR-VARIABLE, a TYPED-BUFFER of code), ascending in PTRS
\ as offsets from start; their count.
: DECLARED ( n n -- n )
   {: start:n end:n :}
   0 PTR-N !
   data-base BYTE-VIEW NULL-PTR BYTE-VIEW - {: data:n :}
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ SNAP-RELOC:XTCELL-OFF-MASK and data + {: site:n :}
      site start >= site CELL + end <= and if
         PTR-N @ 1+ PTRS-RESERVE
         site start - PTR-N @ PTRS !
         1 PTR-N +!
      then
   loop
   PTR-N @ 1 > if 0 PTRS PTR-N @ [: < ;] SORT:SORT! then
   PTR-N @ ;

\ ord's DATA digest in HOUT over its extent up to the next record origin
\ within its limits, and that extent in bytes; 0 when ord has no DATA origin
\ or its extent is empty. A cell that may hold an address goes through
\ H-POINTER: a variable's or defer's first cell and each declared address
\ cell. Its raw bytes hold a process address. The declared cells' count and
\ offsets precede the bytes, so each extent state has one stream layout.
: DATA-HASH ( n -- n )
   {: ord:n :}
   ord NFILE-REF:DATA-ORIGIN-BASE {: origin:n known:bool :}
   known 0= if 0 exit then
   ord LIMITS {: limit:n lim-here:n :}
   origin limit lim-here SPAN {: owner:n base:n end:n :}
   owner drop base drop
   end origin - {: size:n :}
   size 0 <= if 0 exit then
   HCTX SHA256-BEGIN
   s" habu-data-v2" H-S
   size H-N
   ord POINTER-CELL? size 8 >= and if
      origin N>U8 LE:U64@ ord limit lim-here H-POINTER
      origin 8 +
   else origin then {: start:n :}
   start end DECLARED {: ptrs:n :}
   ptrs H-N
   ptrs 0 ?do start i PTRS @ + origin - H-N loop
   start
   ptrs 0 ?do
      {: run:n :}
      start i PTRS @ + {: site:n :}
      run N>U8 site run - H-BYTES
      site N>U8 LE:U64@ ord limit lim-here H-POINTER
      site CELL +
   loop {: run:n :}
   run N>U8 end run - H-BYTES
   HCTX HOUT SHA256-END
   size ;

: REACH-DATA ( n -- )
   {: ord:n :}
   ord DATA-HASH 0= if s" executed-closure-unknown" ord FAIL exit then
   WOK @ 0= if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n known:bool :}
   known 0= if s" executed-closure-unknown" ord FAIL exit then
   WCAPTURE @ if ord DATA key keyu HOUT DIGEST-BYTES ADD-ROW then ;

: MEMBER+ ( n -- )
   MEM-N @ 1+ MEMBERS-RESERVE
   MEM-N @ MEMBERS !
   1 MEM-N +! ;

\ Queue ord once per run.
: QUEUE ( n -- )
   {: ord:n :}
   ord RMARK @ REPOCH @ = if exit then
   REPOCH @ ord RMARK !
   RQ-N @ 1+ RQUEUE-RESERVE
   ord RQ-N @ RQUEUE !
   1 RQ-N +! ;

: EDGES ( n -- )
   {: ord:n :}
   ord NFILE-REF:RESULT-EDGES 0 ?do
      ord i NFILE-REF:RESULT-EDGE {: kind:n target:n :}
      kind drop
      target 0 < if s" executed-closure-unknown" ord FAIL else target VISIT then
   loop ;

\ A seed record lies below the floor: a seed value is terminal, a classed
\ primitive or an engine helper is admitted, and any other seed record
\ refuses, a seed defer's cell among them (the `is` keyword that rebinds
\ one is a later lane).
: SEED-REACH ( n -- )
   {: ord:n :}
   ord SEED-KIND {: k:n :}
   k 2 = k 3 = or if exit then
   k 1 = if s" run-reaches-kernel-data" else s" run-reaches-kernel-word" then ord FAIL ;

\ A record the file made is not in a closure: no row could be checked at the
\ file's entry. A capturing walk queues it, and a walk through it (WTRANS)
\ queues every record it reaches but a seed record, which it classes as a
\ walk from the file's entry does (SEED-REACH). A defer made after the seed
\ is its cell, since an earlier file may rebind it; a definition with a
\ result is a member when its host reason is admitted (SAFE, MEMORY, FIXED)
\ and refuses otherwise; a constant is its VALUE row; a does> instance's
\ clause has no result yet; anything else must be a data record.
: REACH ( n -- )
   {: ord:n :}
   WTRANS @ if
      ord WROOT @ <> if
         ord NFILE-REF:SEED-RECORD? if ord SEED-REACH else ord QUEUE then exit
      then
   else
      ord OWN? if WCAPTURE @ if ord QUEUE then exit then
   then
   ord XREF-REC {: rec:ptr :}
   ord NFILE-REF:SEED-RECORD? if ord SEED-REACH exit then
   rec NDICT:REC-DEFER-CELL 0<> if ord REACH-DATA exit then
   ord NFILE-REF:RESULT-DIGEST {: digest:ptr known:bool :}
   digest drop
   known if
      ord NFILE-REF:RESULT-REASON drop {: reason:n :}
      reason NFILE-REF:REASON-ADMITTED? 0= if reason REASON-WHY$ ord FAIL exit then
      ord MEMBER+ ord EDGES exit
   then
   rec XREF-FLAGS {: flags:n :}
   flags DKIND:MASK and DKIND:VAL = if exit then
   flags DNAME-ALLOC and 0<> flags DKIND:MASK and 0= and if
      s" executed-closure-unknown" ord FAIL exit
   then
   ord REACH-DATA ;

: WALK ( n bool -- )
   {: root:n capture:bool :}
   capture WALK-BEGIN
   root VISIT
   begin WSP @ 0 > WOK @ and while
      -1 WSP +!
      WSP @ WSTACK @ REACH
   repeat ;

: KEY< ( n n -- bool )
   {: a:n b:n :}
   a NFILE-REF:DECL-KEY$ b NFILE-REF:DECL-KEY$ STR< ;

\ The walk's members in key order, each with its result digest, into HOUT.
: CLOSURE-HASH ( -- )
   MEM-N @ 1 > if 0 MEMBERS MEM-N @ [: KEY< ;] SORT:SORT! then
   HCTX SHA256-BEGIN
   s" habu-executed-v1" H-S
   MEM-N @ H-N
   MEM-N @ 0 ?do
      i MEMBERS @ {: m:n :}
      m NFILE-REF:DECL-KEY$ H-S
      m NFILE-REF:RESULT-DIGEST drop DIGEST-BYTES H-BYTES
   loop
   HCTX HOUT SHA256-END ;

: CHECK-EXECUTED ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found if
      \ REACH's refusals fail this walk too: a warm walk that meets one misses.
      ord false WALK
      WOK @ if CLOSURE-HASH HOUT DIGEST-BYTES want wantu STR= else false then
   else false then
   dup 0= if s" executed closure changed" REMEMBER-MISS then ;

: CHECK-DATA ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found if
      false WALK-BEGIN
      ord DATA-HASH 0<> WOK @ and if HOUT DIGEST-BYTES want wantu STR= else false then
   else false then
   dup 0= if s" data changed" REMEMBER-MISS then ;

\ The owner retained this exact fixed result at publication. CHECK reads only
\ its record fact; it never enters a definition to reconstruct the value.
: FIXED-RAW? ( n -- n bool )
   CONTRACT$ {: contract:ptr size:n known:bool :}
   known 0= if 0 false exit then
   size CHECKER-OWNER-EXT:MIN-BYTES < if 0 false exit then
   contract LE:U64@ CHECKER-OWNER-EXT:CONTRACT-MAGIC <> if
      0 false exit
   then
   contract CHECKER-OWNER-EXT:FIXED-KIND-OFF + LE:U64@
      CHECKER-OWNER-EXT:FIXED-VAL <> if 0 false exit then
   contract CHECKER-OWNER-EXT:FIXED-VALUE-OFF + LE:U64@ true ;

: CHECK-BINDING ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" binding absent" REMEMBER-MISS false exit then
   ord CONTRACT$ {: got:ptr gotu:n known:bool :}
   known 0= if s" contract unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR= dup 0= if s" contract changed" REMEMBER-MISS then ;

: CHECK-READER-FLAGS ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   ord drop
   found 0= if s" reader binding absent" REMEMBER-MISS false exit then
   rec XREF-FLAGS DNAME-IMM and CELL-BYTES LE:U64!
   CELL-BYTES 8 want wantu STR=
   dup 0= if s" reader flags changed" REMEMBER-MISS then ;

: CHECK-VALUE ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" value binding absent" REMEMBER-MISS false exit then
   ord FIXED-RAW? {: value:n known:bool :}
   known 0= if s" value fact unknown" REMEMBER-MISS false exit then
   value CELL-BYTES LE:U64!
   CELL-BYTES 8 want wantu STR=
   dup 0= if s" value changed" REMEMBER-MISS then ;

: CHECK-CHECKER-ROLE ( ptr u8 n -- bool )
   {: want:ptr wantu:n :}
   CHECKER-ROLE$ {: got:ptr gotu:n known:bool :}
   known 0= if s" checker producer unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR=
   dup 0= if s" checker producer changed" REMEMBER-MISS then ;

: CHECK-COMPILER-ROLE ( ptr u8 n -- bool )
   {: want:ptr wantu:n :}
   COMPILER-ROLE$ {: got:ptr gotu:n known:bool :}
   known 0= if s" emitting compiler unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR=
   dup 0= if s" emitting compiler changed" REMEMBER-MISS then ;

: CHECK-PRIMITIVE ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n data:ptr datau:n :}
   datau PRIMITIVE-HEAD < if MALFORMED then
   data 8 + LE:U64@ {: pkgu:n :}
   data 16 + LE:U64@ {: vis:n :}
   data 24 + LE:U64@ {: tailu:n :}
   pkgu 0 < tailu 0 <= or vis 0 < or vis 2 > or if MALFORMED then
   pkgu datau PRIMITIVE-HEAD - > if MALFORMED then
   tailu datau PRIMITIVE-HEAD pkgu + - <> if MALFORMED then
   vis 0= if pkgu 0<> if MALFORMED then
   else pkgu 0= if MALFORMED then then
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" primitive binding absent" REMEMBER-MISS false exit then
   data PRIMITIVE-HEAD + pkgu + {: tail:ptr :}
   data PRIMITIVE-HEAD + pkgu vis tail tailu ORIGIN-SYM
      {: sym:n :}
   sym 0= if s" primitive symbol absent" REMEMBER-MISS false exit then
   ord sym PRIMITIVE-STATE {: control:n known:bool :}
   known 0= if s" primitive state changed" REMEMBER-MISS false exit then
   control data LE:U64@ = dup 0= if
      s" primitive control changed" REMEMBER-MISS
   then ;

: CHECK-RESOLUTION ( ptr u8 n ptr u8 n -- bool )
   {: row:ptr rowu:n want:ptr wantu:n :}
   rowu RES-KEY-BYTES < if MALFORMED then
   row LE:U64@ {: role:n :}
   row 8 + LE:U64@ {: shape:n :}
   row 16 + LE:U64@ {: spellingu:n :}
   row 24 + LE:U64@ {: namespaceu:n :}
   role 0 < role 9 > or shape 0 < shape 3 > or or
      spellingu 0 < namespaceu 0 < or or if MALFORMED then
   spellingu rowu RES-KEY-BYTES - > if MALFORMED then
   namespaceu rowu RES-KEY-BYTES spellingu + - <> if MALFORMED then
   role row RES-KEY-BYTES + spellingu
      row RES-KEY-BYTES spellingu + + namespaceu
      NFILE-REF:RESOLVE-CANDIDATE {: ord:n got-shape:n :}
   ord -2 = if s" ambiguous resolution" REMEMBER-MISS false exit then
   got-shape shape <> if s" namespace shape changed" REMEMBER-MISS false exit then
   ord -1 = if
      wantu 0= exit
   then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n known:bool :}
   known 0= if s" candidate identity unknown" REMEMBER-MISS false exit then
   key keyu want wantu STR= dup 0= if
      s" candidate changed" REMEMBER-MISS
   then ;

: CHECK-ROW ( n ptr u8 n ptr u8 n -- bool )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   kind BINDING = if
      keyu 0= datau 0= or if MALFORMED then
      key keyu data datau CHECK-BINDING exit
   then
   kind READER-FLAGS = if
      keyu 0= datau 8 <> or if MALFORMED then
      key keyu data datau CHECK-READER-FLAGS exit
   then
   kind VALUE = if
      keyu 0= datau 8 <> or if MALFORMED then
      key keyu data datau CHECK-VALUE exit
   then
   kind EXECUTED = if
      keyu 0= datau DIGEST-BYTES <> or if MALFORMED then
      key keyu data datau CHECK-EXECUTED exit
   then
   kind DATA = if
      keyu 0= datau DIGEST-BYTES <> or if MALFORMED then
      key keyu data datau CHECK-DATA exit
   then
   kind CHECKER-ROLE = if
      keyu 0<> datau 32 <> or if MALFORMED then
      data datau CHECK-CHECKER-ROLE exit
   then
   kind COMPILER-ROLE = if
      keyu 0<> datau 32 <> or if MALFORMED then
      data datau CHECK-COMPILER-ROLE exit
   then
   kind PRIMITIVE = if
      keyu 0= if MALFORMED then
      key keyu data datau CHECK-PRIMITIVE exit
   then
   kind SCOPE = if
      keyu 0<> datau 0= or if MALFORMED then
      data datau NFILE-SCOPE:SCOPE-MATCH? exit
   then
   kind RESOLUTION = if
      keyu RES-KEY-BYTES < if MALFORMED then
      key keyu data datau CHECK-RESOLUTION exit
   then
   kind ORDERED = if
      keyu 0<> datau 0<> or if MALFORMED then
      s" ordered source action" REMEMBER-MISS false exit
   then
   MALFORMED ;

\ The row at CHECK-OFF, its lengths checked against the bytes; CHECK-OFF
\ moves past it.
: ROW-AT ( ptr u8 n -- n ptr u8 n ptr u8 n )
   {: bytes:ptr size:n :}
   size CHECK-OFF @ - ROW-BYTES < if MALFORMED then
   bytes CHECK-OFF @ + {: row:ptr :}
   row 8 + LE:U64@ {: keyu:n :}
   row 16 + LE:U64@ {: datau:n :}
   keyu 0 < datau 0 < or if MALFORMED then
   keyu size CHECK-OFF @ - ROW-BYTES - > if MALFORMED then
   datau size CHECK-OFF @ - ROW-BYTES - keyu - > if MALFORMED then
   CHECK-OFF @ ROW-BYTES keyu + datau + + CHECK-OFF !
   row LE:U64@ row ROW-BYTES + keyu
      row ROW-BYTES keyu + + datau ;

: CHECK-NEXT ( ptr u8 n -- )
   ROW-AT CHECK-ROW 0= if false CHECK-OK ! then ;

\ The header's row count, bounded by the bytes; CHECK-OFF starts at row 0.
: HEAD ( ptr u8 n -- n )
   {: bytes:ptr size:n :}
   size HEADER-BYTES < if MALFORMED then
   bytes 8 s" HBFDEP02" STR= 0= if MALFORMED then
   bytes 8 + LE:U64@ {: count:n :}
   count 0 < count size HEADER-BYTES - ROW-BYTES / > or if MALFORMED then
   HEADER-BYTES CHECK-OFF !
   count ;

: HAS-ORDERED? ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   HEADER-BYTES SCAN-OFF !
   begin SCAN-OFF @ size < while
      bytes SCAN-OFF @ + {: row:ptr :}
      row LE:U64@ ORDERED = if true exit then
      row 8 + LE:U64@ row 16 + LE:U64@ + ROW-BYTES +
         SCAN-OFF +!
   repeat
   false ;

public

\ Whether this session prints cache diagnostics. NDEF-CACHE's OPEN sets it
\ from HABU_NATIVE_CACHE_DIAG and its close clears it. A diagnostic reads this
\ flag, never the environment: GETENV records an input row in the file being
\ loaded, and the cache's own diagnostics are no input of any file.
variable DIAG?

private

SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL DIAG-PATH

: DIAG-W2 ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   0 begin dup size < while
      dup {: sent:n :}
      2 bytes sent + size sent - write
      dup 0 <= if drop E-FS-IO throw then
      +
   repeat drop ;

: REPORT-MISS ( -- )
   DIAG? @ 0= if exit then
   MISS-P @ NULL-PTR = if exit then
   s" native-file: dependency miss: " DIAG-W2
   MISS-P @ MISS-U @ DIAG-W2
   S\" \n" DIAG-W2 ;

: DIAG-N ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   s" 0123456789" drop value 10 mod + 1 DIAG-W2 ;

: DIAG-PACKAGE ( n -- )
   {: wid:n :}
   wid 0= if exit then
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC wid = rec XREF-PKG-PRIVATE wid = or if
            s"  package=" DIAG-W2 rec XREF-NAME$ DIAG-W2
            unloop exit
         then
      then
   loop ;

\ Report before the first ORDERED row loses the dependency's identity.
\ The existing frame supplies the first-row test; diagnostics keep no state.
: DIAG-ORDERED-NAME ( ptr u8 n ptr u8 n n -- )
   {: reason:ptr reasonu:n name:ptr nameu:n ord:n :}
   DIAG? @ 0= if exit then
   FRAME {: off:n :}
   off BYTES USED @ off - HAS-ORDERED? if exit then
   s" native-cache: ordered-dependency reason=" DIAG-W2
   reason reasonu DIAG-W2
   nameu 0 > if s"  dependency=" DIAG-W2 name nameu DIAG-W2 then
   ord 0 >= if
      s"  ord=" DIAG-W2 ord DIAG-N
      ord XREF-REC XREF-WORDLIST DIAG-PACKAGE
   then
   data-base SRCLOC:PATHLEN-CELL + @ {: pathu:n :}
   pathu 0 > if s"  path=" DIAG-W2 DIAG-PATH @ pathu DIAG-W2 then
   S\" \n" DIAG-W2 ;

: WHY+ ( ptr u8 n -- )
   {: text:ptr textu:n :}
   textu 0= if exit then
   WHYU @ textu + FWHY-RESERVE
   text WHYU @ FWHY textu BYTE-COPY
   textu WHYU +! ;

: FILE-ORDERED? ( -- bool ) WHYU @ F-WHY FFIELD @ > ;

\ The definition frame reports its first ORDERED row now; the file frame keeps
\ its first reason for FILE-REPORT.
: ORDERED-AT ( ptr u8 n ptr u8 n n -- )
   {: reason:ptr reasonu:n name:ptr nameu:n ord:n :}
   DEPTH @ 0 > if reason reasonu name nameu ord DIAG-ORDERED-NAME then
   FDEPTH @ 0 > ord OWN? 0= and if
      FILE-ORDERED? 0= if
         s"  reason=" WHY+ reason reasonu WHY+
         nameu 0 > if s"  word=" WHY+ name nameu WHY+ then
      then
   then
   ord ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW ;

: ORDERED-BY ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   reason reasonu
   ord 0 >= if ord XREF-REC XREF-NAME$ else NULL-PTR 0 then
   ord ORDERED-AT ;

: CAPTURE-BIND ( n -- )
   {: ord:n :}
   ord SKIP? if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" binding-key-unknown" ord ORDERED-BY exit then
   ord CONTRACT$ {: contract:ptr contractu:n known:bool :}
   known 0= if s" binding-contract-unknown" ord ORDERED-BY exit then
   ord key keyu contract contractu BINDING-ROW ;

: CAPTURE-READER-FLAGS ( n n -- )
   {: ord:n flags:n :}
   ord SKIP? if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" reader-key-unknown" ord ORDERED-BY exit then
   flags DNAME-IMM and CELL-BYTES LE:U64!
   ord READER-FLAGS key keyu CELL-BYTES 8 ADD-ROW ;

: CAPTURE-VALUE ( n -- )
   {: ord:n :}
   ord SKIP? if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" value-key-unknown" ord ORDERED-BY exit then
   ord FIXED-RAW? {: value:n known:bool :}
   known 0= if s" fixed-value-unknown" ord ORDERED-BY exit then
   value CELL-BYTES LE:U64!
   ord key keyu CELL-BYTES 8 VALUE-ROW ;

: ADD-RESOLUTION ( n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ord:n role:n shape:n spelling:ptr spellingu:n namespace:ptr namespaceu:n
      key:ptr keyu:n :}
   spellingu 0 < namespaceu 0 < or if BAD then
   spellingu MEM-MAX-N RES-KEY-BYTES - > if BAD then
   namespaceu MEM-MAX-N RES-KEY-BYTES spellingu + - > if BAD then
   RES-KEY-BYTES spellingu + namespaceu + RES-KEY-RESERVE
   role 0 RES-KEY LE:U64!
   shape 8 RES-KEY LE:U64!
   spellingu 16 RES-KEY LE:U64!
   namespaceu 24 RES-KEY LE:U64!
   spellingu 0<> if
      spelling 32 RES-KEY spellingu BYTE-COPY
   then
   namespaceu 0<> if
      namespace 32 spellingu + RES-KEY namespaceu BYTE-COPY
   then
   ord RESOLUTION 0 RES-KEY RES-KEY-BYTES spellingu + namespaceu +
      key keyu ADD-ROW ;

\ The record whose ownership decides whether the file frame keeps a
\ resolution row (ADD-ROW): the namespace record when the file owns that
\ package, else the candidate, -1 when there is none. A global leg has no
\ namespace.
: ROW-OWNER ( n n ptr u8 n -- n )
   {: ord:n role:n namespace:ptr namespaceu:n :}
   role 0= if ord exit then
   namespace namespaceu NFILE-REF:FIND-NAMESPACE {: ns:n :}
   ns OWN-NAMESPACE? if ns else ord then ;

: OBSERVE-RESOLUTION ( ptr u8 n -- )
   {: row:ptr rowu:n :}
   OPEN? 0= if exit then
   rowu RES-ROW-BYTES < if BAD then
   row LE:U64@ drop
   row 8 + LE:U64@ {: role:n :}
   row 16 + LE:U64@ {: ord:n :}
   row 24 + LE:U64@ {: shape:n :}
   row 32 + LE:U64@ {: spellingu:n :}
   row 40 + LE:U64@ {: namespaceu:n :}
   role 0 < role 9 > or shape 0 < shape 3 > or or
      spellingu 0 < namespaceu 0 < or or if BAD then
   spellingu rowu RES-ROW-BYTES - > if BAD then
   namespaceu rowu RES-ROW-BYTES spellingu + - <> if BAD then
   ord -1 < if
      s" resolution-ambiguous" row RES-ROW-BYTES + spellingu -1 ORDERED-AT
      exit
   then
   ord 0 >= if
      ord SKIP? if exit then
      ord NFILE-REF:CURRENT-RECORD? if exit then
      ord NFILE-REF:DECL-KEY? 0= if
         2drop s" resolution-key-unknown" ord ORDERED-BY exit
      then 2drop
   then
   ord role row RES-ROW-BYTES spellingu + + namespaceu ROW-OWNER {: owner:n :}
   ord -1 = if
      owner role shape row RES-ROW-BYTES + spellingu
         row RES-ROW-BYTES spellingu + + namespaceu
         NULL-PTR 0 ADD-RESOLUTION
   else
      ord NFILE-REF:DECL-KEY$ {: key:ptr keyu:n :}
      owner role shape row RES-ROW-BYTES + spellingu
         row RES-ROW-BYTES spellingu + + namespaceu
         key keyu ADD-RESOLUTION
   then ;

\ The token the interpreter resolved: the word it runs, or the name after a
\ top-level tick (habu2.f C-TICK).
: RUN-TOKEN-A ( -- ptr u8 ) data-base TKA-CELL + 0 ptr-field @ ;
: RUN-TOKEN$ ( -- ptr u8 n ) RUN-TOKEN-A data-base TKL-CELL + @ ;

variable RUN-ORD
variable RUN-HITS
variable TOP-XT

CAST: AS-TOP ( n -- [ ptr u8 n [ ptr u8 n -- ] -- ] )

\ The refusal of the open file for a run of its own record whose walk reached
\ what it cannot account for (reason, the record it failed on). NUNIT-OBJECT
\ requires this file, so the definition cache binds it, as it binds TOP-XT.
defer RUN-REFUSE ( ptr u8 n ptr n -- )
: NO-RUN-REFUSE ( ptr u8 n ptr n -- ) drop 2drop BAD ;
: DEFAULT-REFUSE ( -- ) ['] NO-RUN-REFUSE is RUN-REFUSE ;
DEFAULT-REFUSE

\ One leg of the run's token: the row a compiled reference to the same
\ spelling in the same scope adds, counted when it holds the record that ran.
: RUN-LEG ( ptr u8 n -- )
   {: row:ptr rowu:n :}
   row rowu OBSERVE-RESOLUTION
   row 16 + LE:U64@ RUN-ORD @ = if 1 RUN-HITS +! then ;

: CAPTURE-CHECKER-ROLE ( -- )
   OPEN? 0= if exit then
   CHECKER-ROLE$ {: digest:ptr digestu:n known:bool :}
   known 0= if s" checker-producer-unknown" -1 ORDERED-BY exit then
   digestu 32 <> if BAD then
   -1 CHECKER-ROLE NULL-PTR 0 digest digestu ADD-ROW ;

: CAPTURE-COMPILER-ROLE ( -- )
   OPEN? 0= if exit then
   COMPILER-ROLE$ {: digest:ptr digestu:n known:bool :}
   known 0= if s" compiler-producer-unknown" -1 ORDERED-BY exit then
   digestu 32 <> if BAD then
   -1 COMPILER-ROLE NULL-PTR 0 digest digestu ADD-ROW ;

: BOUND@ ( ptr u8 n -- n )
   {: row:ptr field:n :}
   row field cells + CELL-VIEW @ ;

: PRIMITIVE-DATA$ ( n n -- ptr u8 n )
   {: sym:n control:n :}
   sym SYM-IDENTITY {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
   pkgu 0 < tailu 0 <= or vis 0 < or vis 2 > or if BAD then
   pkgu MEM-MAX-N PRIMITIVE-HEAD - > if BAD then
   tailu MEM-MAX-N PRIMITIVE-HEAD pkgu + - > if BAD then
   PRIMITIVE-HEAD pkgu + tailu + PRIMITIVE-DATA-RESERVE
   control 0 PRIMITIVE-DATA LE:U64!
   pkgu 8 PRIMITIVE-DATA LE:U64!
   vis 16 PRIMITIVE-DATA LE:U64!
   tailu 24 PRIMITIVE-DATA LE:U64!
   pkg PRIMITIVE-HEAD PRIMITIVE-DATA pkgu BYTE-COPY
   tail PRIMITIVE-HEAD pkgu + PRIMITIVE-DATA tailu BYTE-COPY
   0 PRIMITIVE-DATA PRIMITIVE-HEAD pkgu + tailu + ;

: CAPTURE-PRIMITIVE ( ptr u8 n -- )
   {: row:ptr ord:n :}
   row CHECKER-OWNER-ABI:BOUND-SYM BOUND@ {: sym:n :}
   ord sym PRIMITIVE-STATE {: control:n known:bool :}
   known 0= if s" primitive-state-unknown" ord ORDERED-BY exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" primitive-key-unknown" ord ORDERED-BY exit then
   sym control PRIMITIVE-DATA$ {: data:ptr datau:n :}
   ord PRIMITIVE key keyu data datau ADD-ROW
   CAPTURE-CHECKER-ROLE ;

\ This row is the checker's exact resolution from the successful source scan.
\ The tape's symbol interner is only a name; it is never a declaration key.
: OBSERVE-BOUND ( ptr u8 n -- )
   {: row:ptr size:n :}
   OPEN? 0= if exit then
   size CHECKER-OWNER-ABI:BOUND-CELLS cells <> if
      CHECKER-OWNER-ABI:BINDING-RC throw
   then
   row CHECKER-OWNER-ABI:BOUND-KIND BOUND@ {: kind:n :}
   kind CHECKER-OWNER-ABI:BOUND-DICT = if
      row CHECKER-OWNER-ABI:BOUND-RECORD BOUND@ {: ord:n :}
      ord 0 < if CHECKER-OWNER-ABI:BINDING-RC throw then
      ord SKIP? if exit then
      row CHECKER-OWNER-ABI:BOUND-CTL BOUND@
         CHECKER-OWNER-ABI:BOUND-SEEDED and 0<> if
         row ord CAPTURE-PRIMITIVE
      else ord CAPTURE-BIND then
      row CHECKER-OWNER-ABI:BOUND-FLAGS BOUND@ {: flags:n :}
      ord flags CAPTURE-READER-FLAGS
      flags DKIND:MASK and DKIND:VAL = if ord CAPTURE-VALUE then
      exit
   then
   kind CHECKER-OWNER-ABI:BOUND-INTRINSIC = if
      CAPTURE-CHECKER-ROLE
   then ;

\ The feed reads the source owner's candidates after each scan; `package` and
\ `using` send their namespace decision as they run (checker.f
\ RWIN-NAMESPACE). Enable both while a frame is open.
: OBSERVE ( -- )
   ['] OBSERVE-BOUND NFEED:USE-BINDINGS
   ['] OBSERVE-RESOLUTION NFEED:USE-RESOLUTIONS
   ['] OBSERVE-RESOLUTION CHECKER-OWNER:RESOLUTION-NS-INSTALL ;

: UNOBSERVE ( -- )
   OPEN? if exit then
   NFEED:BINDINGS-DEFAULT
   NFEED:RESOLUTIONS-DEFAULT
   CHECKER-OWNER:RESOLUTION-NS-DEFAULT ;

: HEADER! ( ptr u8 -- )
   {: head:ptr :}
   s" HBFDEP02" drop head 8 BYTE-COPY
   0 head 8 + LE:U64! ;

: RANGE+ ( n n -- )
   {: first:n end:n :}
   FRN @ 2 + FRANGES-RESERVE
   first FRN @ FRANGES !
   end FRN @ 1+ FRANGES !
   2 FRN +! ;

: TALLY ( n n n -- n n )
   {: tally:n off:n kind:n :}
   off FBYTES {: row:ptr :}
   row LE:U64@ kind = if tally 1+ else tally then
   off row ROW-SPAN + ;

: KIND-ROWS ( n -- n )
   {: kind:n :}
   F-OFF FFIELD @ {: off:n :}
   0 off HEADER-BYTES +
   off FBYTES 8 + LE:U64@ 0 ?do kind TALLY loop
   drop ;

\ What a run of ord reads: a constant's VALUE row; for a data word its DATA
\ row; for a definition, defer or does> instance one EXECUTED row over the
\ members its closure reached and a DATA row per data record. A seed value is
\ its VALUE row; a seed data record refuses; a classed primitive or an
\ engine helper is admitted and a seed code root records nothing here (its
\ RESOLUTION row is its only fact; its trace judges it). A seed record
\ reaches here only as the run's root: a walk through an own record classes
\ the seed records it reaches.
: CAPTURE-ONE ( n -- )
   {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-FLAGS DKIND:MASK and DKIND:VAL = if ord CAPTURE-VALUE exit then
   ord NFILE-REF:SEED-RECORD? if
      ord SEED-KIND 1 = if s" run-reaches-kernel-data" ord FAIL then exit
   then
   ord true WALK
   WOK @ 0= if exit then
   ord NFILE-REF:RESULT-DIGEST nip rec NDICT:REC-DEFER-CELL 0<> or if
      CLOSURE-HASH
      ord EXECUTED ord NFILE-REF:DECL-KEY$ HOUT DIGEST-BYTES ADD-ROW
   then ;

\ A walk through the file's own record ord, recording nothing: it queues each
\ record ord reaches directly.
: THROUGH ( n -- )
   {: ord:n :}
   ord WROOT ! true WTRANS !
   ord false WALK
   false WTRANS ! ;

\ The run of ord as rows keyed by records that exist at the file's entry:
\ each record the run reaches is captured as if run directly when the file
\ did not make it, and walked through when it did. The first walk that
\ cannot account for a record decides: under an own root it refuses the file
\ by the failing record's name, under an earlier file's root it orders the
\ frame.
: CAPTURE-RUN ( n -- )
   {: ord:n :}
   ndict@ {: n:n :}
   n RMARK-RESERVE
   n RMARK-N @ ?do 0 i RMARK ! loop
   n RMARK-N @ max RMARK-N !
   1 REPOCH +!
   0 RQ-N ! true WOK !
   ord QUEUE
   0 begin dup RQ-N @ < while
      dup RQUEUE @ {: r:n :}
      r OWN? if r THROUGH else r CAPTURE-ONE then
      WOK @ 0= if
         drop
         FAIL-P @ FAIL-U @ FAIL-ORD @ XREF-REC {: why:ptr whyu:n rec:ptr :}
         r OWN? if why whyu rec RUN-REFUSE else why whyu rec XREF-NAME$ r ORDERED-AT then
         exit
      then
      1+
   repeat drop ;

\ The frame's SCOPE row names a package an earlier session made, whose
\ declaration has no key, so no later build can verify it: refuse.
: SCOPE-KNOWN ( -- )
   NFILE-SCOPE:SCOPE-UNKEYED {: ord:n :}
   ord 0 >= if s" scope-key-unknown" ord ORDERED-BY then ;

public

\ The running checker owner's CHECKER-OWNER:RESOLUTION-TOP, bound for the
\ session by the definition cache before it installs the run observer, and 0
\ after. A build tool resolves names against its host engine, which may predate
\ the word, so OBSERVE-RUN reaches it by execution token.
: USE-TOP ( n -- ) TOP-XT ! ;

\ The open file's refusal for a failed run of its own record, bound for the
\ session by the definition cache beside USE-TOP, and the default after.
: USE-REFUSE ( [ ptr u8 n ptr n -- ] -- ) is RUN-REFUSE ;
: DROP-REFUSE ( -- ) DEFAULT-REFUSE ;

\ A word the source ran at top level, by an immediate or under a top-level
\ tick: the checker's resolution rows for its token, one per leg, the legs
\ without the name included, and then, when one of them holds the record that
\ ran, what the run read (CAPTURE-RUN).
: OBSERVE-RUN ( n -- )
   {: ord:n :}
   OPEN? 0= if exit then
   ord RUN-ORD !
   0 RUN-HITS !
   RUN-TOKEN$ ['] RUN-LEG TOP-XT @ AS-TOP execute
   RUN-HITS @ 0= if s" resolution-leg-unknown" ord ORDERED-BY exit then
   ord CAPTURE-RUN ;

\ Open a file frame at the file's first record ordinal.
: BEGIN-UNIT ( -- )
   s" PRIM-SPEC:REACH@" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT REACH-XT !
   s" PRIM-SPEC:FIND" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT FIND-XT !
   FDEPTH @ 1+ FRAME-CELLS * FFRAMES-RESERVE
   FDEPTH @ FRAME-CELLS * {: base:n :}
   FUSED @ base F-OFF + FFRAMES !
   ndict@ base F-FIRST + FFRAMES !
   FRN @ base F-RBASE + FFRAMES !
   WHYU @ base F-WHY + FFRAMES !
   HERE-N base F-HERE + FFRAMES !
   1 FDEPTH +!
   HEADER-BYTES FROOM FBYTES HEADER!
   NFILE-SCOPE:SCOPE-INPUT$ {: scope:ptr scopeu:n :}
   SCOPE NULL-PTR 0 scope scopeu FILE-ROW
   SCOPE-KNOWN
   OBSERVE ;

\ Close the innermost file frame. Its records become a child range of the
\ enclosing file frame, whose rows may name them.
: CLOSE-UNIT ( -- )
   F-OFF FFIELD @ {: off:n :}
   F-FIRST FFIELD @ {: first:n :}
   off FBYTES 8 + LE:U64@ negate FLIVE +!
   FCAP @ 0 ?do
      i FSLOTS @ off > if 0 i FSLOTS ! then
   loop
   off FUSED !
   F-RBASE FFIELD @ FRN !
   F-WHY FFIELD @ WHYU !
   -1 FDEPTH +!
   FDEPTH @ 0 > if first ndict@ RANGE+ then
   UNOBSERVE ;

\ The innermost file frame's cache diagnostic line, on stderr.
: FILE-REPORT ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   s" native-file: frame path=" DIAG-W2 path pathu DIAG-W2
   s"  rows=" DIAG-W2 F-OFF FFIELD @ FBYTES 8 + LE:U64@ DIAG-N
   s"  ordered=" DIAG-W2 FILE-ORDERED? if 1 else 0 then DIAG-N
   s"  binding=" DIAG-W2 BINDING KIND-ROWS DIAG-N
   s"  value=" DIAG-W2 VALUE KIND-ROWS DIAG-N
   s"  resolution=" DIAG-W2 RESOLUTION KIND-ROWS DIAG-N
   s"  executed=" DIAG-W2 EXECUTED KIND-ROWS DIAG-N
   s"  data=" DIAG-W2 DATA KIND-ROWS DIAG-N
   FILE-ORDERED? if
      F-WHY FFIELD @ {: why:n :}
      why FWHY WHYU @ why - DIAG-W2
   then
   S\" \n" DIAG-W2 ;

: BEGIN-DEFINITION ( n -- )
   {: ord:n :}
   ord NFILE-REF:BEGIN-DEFINITION
   DEPTH @ 1+ FRAMES-RESERVE
   USED @ DEPTH @ FRAMES !
   DEPTH @ 1+ 2 * DLIMITS-RESERVE
   ord DEPTH @ 2 * DLIMITS !
   HERE-N DEPTH @ 2 * 1+ DLIMITS !
   1 DEPTH +!
   HEADER-BYTES ROOM BYTES HEADER!
   NFILE-SCOPE:SCOPE-INPUT$ {: scope:ptr scopeu:n :}
   SCOPE NULL-PTR 0 scope scopeu DEF-ROW
   SCOPE-KNOWN
   OBSERVE ;

: COMPILED! ( -- ) CAPTURE-COMPILER-ROLE ;

\ The frame's rows, and whether the definition may be stored: an ORDERED row
\ names a fact no later build can re-verify.
: EXPORT ( -- ptr u8 n bool )
   FRAME {: off:n :}
   off BYTES USED @ off - {: deps:ptr depsu:n :}
   deps depsu deps depsu HAS-ORDERED? 0= ;

\ The innermost file frame's rows, and whether its file may be stored: no
\ ORDERED row and no ORDERED reason.
: FILE-EXPORT ( -- ptr u8 n bool )
   F-OFF FFIELD @ {: off:n :}
   off FBYTES FUSED @ off - {: deps:ptr depsu:n :}
   deps depsu
   deps depsu HAS-ORDERED? 0= FILE-ORDERED? 0= and ;

\ Whether the innermost file stands in the scope it began in: its first row
\ is the SCOPE row BEGIN-UNIT recorded.
: FILE-SCOPE-KEPT? ( -- bool )
   F-OFF FFIELD @ {: off:n :}
   off FBYTES FUSED @ off - {: deps:ptr depsu:n :}
   deps depsu HEAD 0= if BAD then
   deps depsu ROW-AT {: kind:n key:ptr keyu:n data:ptr datau:n :}
   kind SCOPE <> if BAD then
   data datau NFILE-SCOPE:SCOPE-MATCH? ;

: CHECK ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   bytes size HEAD {: count:n :}
   true CHECK-OK !
   NULL-PTR MISS-P ! 0 MISS-U !
   count 0 ?do bytes size CHECK-NEXT loop
   CHECK-OFF @ size <> if MALFORMED then
   CHECK-OK @ 0= if REPORT-MISS then
   CHECK-OK @ ;

\ The innermost file frame's dependency row count so far.
: FILE-ROWS ( -- n ) F-OFF FFIELD @ FBYTES 8 + LE:U64@ ;

\ Whether rows [done, mark) of a file object's dependency bytes still hold;
\ rows below done are framed, not checked. Each call restarts at HEAD and
\ keeps nothing after it returns, so a caller may load child files, which
\ check their own objects, between ranges.
: CHECK-ROWS ( ptr u8 n n n -- bool )
   {: bytes:ptr size:n done:n mark:n :}
   bytes size HEAD {: count:n :}
   done 0 < mark done < or mark count > or if MALFORMED then
   NULL-PTR MISS-P ! 0 MISS-U !
   mark 0 ?do
      bytes size ROW-AT {: kind:n key:ptr keyu:n data:ptr datau:n :}
      i done >= if
         kind key keyu data datau CHECK-ROW 0= if
            REPORT-MISS false unloop exit
         then
      then
   loop
   true ;

\ The row count of well-framed dependency bytes, without checking any row
\ against this session.
: FORMAT ( ptr u8 n -- n )
   {: bytes:ptr size:n :}
   bytes size HEAD {: count:n :}
   count 0 ?do bytes size ROW-AT 2drop 2drop drop loop
   CHECK-OFF @ size <> if MALFORMED then
   count ;

: RESET ( -- )
   NFILE-REF:END-DEFINITION
   0 DEPTH !
   0 USED !
   0 FDEPTH !
   0 FUSED !
   0 FRN !
   0 WHYU !
   0 FCAP !
   0 FLIVE !
   UNOBSERVE
   BYTES-RELEASE FRAMES-RELEASE
   FBYTES-RELEASE FFRAMES-RELEASE FRANGES-RELEASE FWHY-RELEASE FSLOTS-RELEASE
   RES-KEY-RELEASE PRIMITIVE-DATA-RELEASE
   DLIMITS-RELEASE SEEN-RELEASE WSTACK-RELEASE MEMBERS-RELEASE HWORK-RELEASE
   RMARK-RELEASE RQUEUE-RELEASE OIDX-RELEASE OPMAX-RELEASE OPEND-RELEASE
   0 SEEN-N ! 0 RMARK-N ! 0 RQ-N ! 0 OI-N ! 0 OW ! 0 OPEND-N !
   0 REACH-XT ! 0 FIND-XT !
   8 0 ?do 0 CELL-BYTES i + c! loop ;

\ Close the innermost definition frame.
: CLOSE ( -- )
   FRAME {: off:n :}
   NFILE-REF:END-DEFINITION
   off USED !
   -1 DEPTH +!
   UNOBSERVE ;

\ Installation is an explicit boundary: no owner field means no reusable
\ checked binding, while an actual old object with no section is malformed.
: USE-CONTRACT ( [ n -- ptr u8 n bool ] -- ) is CONTRACT$ ;
: USE-CHECKER-ROLE ( [ -- ptr u8 n bool ] -- ) is CHECKER-ROLE$ ;
: USE-COMPILER-ROLE ( [ -- ptr u8 n bool ] -- ) is COMPILER-ROLE$ ;
: USE-PRIMITIVE-STATE ( [ n n -- n bool ] -- ) is PRIMITIVE-STATE ;
: USE-SYM-IDENTITY ( [ n -- ptr u8 n ptr u8 n n ] -- ) is SYM-IDENTITY ;
: USE-ORIGIN-SYM ( [ ptr u8 n n ptr u8 n -- n ] -- ) is ORIGIN-SYM ;
: UNBIND ( -- ) INSTALL-DEFAULT ;

\ The scanner supplies the exact resolved record ordinal, not a spelling or
\ an interned name. A missing contract is visible as an ordered FILE.
: BIND-ORD ( n -- )
   CAPTURE-BIND ;

\ The file depends on the DATA of ord, a record it did not make, as a run
\ that read it would: by ord's DATA row, whose digest covers ord's extent up
\ to the next record origin, so an edit that moves that end is stale. The
\ extent in bytes; 0, with no row, when ord has no DATA, no key, or a cell
\ that holds an address no row can name.
: DATA-ORD ( n -- n )
   {: ord:n :}
   false WALK-BEGIN
   ord DATA-HASH {: size:n :}
   size 0= WOK @ 0= or if 0 exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n known:bool :}
   known 0= if 0 exit then
   ord DATA key keyu HOUT DIGEST-BYTES ADD-ROW
   size ;

;package
