\ Portable identities and references for one native FILE build. The loader binds
\ actual published records here; section 13 keeps identities instead of live
\ dictionary ordinals or DATA addresses.

require src/lib/errors.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require src/core/sha256.f
require src/core/checker-owner-abi.f
require src/habu/xref.f
require src/habu/layout.f
require src/habu/aot-arm.f
require src/compiler/native/dict.f
require src/compiler/native/host.f
require tools/native-unit-file.f

package NFILE-REF
private

32 constant REF-BYTES
\ ENCODE writes the reference, import and declaration counts first.
24 constant HEAD-BYTES
\ A declaration row: index, key and contract lengths, then key and contract.
24 constant DECL-HEAD
6 constant REC-CELLS
0 constant REC-OFF
1 constant REC-LEN
2 constant REC-CON-OFF
3 constant REC-CON-U
4 constant REC-TOKEN
5 constant REC-PROJECTED
4 constant GROUP-CELLS
5 constant KEY-CELLS
4096 constant HASH-SLOTS
9 constant SEED-DECL
\ A WID-OWNERS row: the owning namespace record's ordinal and visibility, then
\ the instance row (-1 none) of the file that made the wordlist and its item
\ among that instance's wordlists.
4 constant WID-CELLS
\ An INSTANCE-ROWS row: key offset and length, constructor counter and the
\ wordlists the instance made so far.
4 constant INSTANCE-CELLS
\ The WID-KEY? sub-tag of a wordlist named by the file that made it: its
\ instance key and its index among that instance's wordlists.
6 constant WID-FILE

DYNAMIC-BUFFER KEY-BUF u8
DYNAMIC-BUFFER INPUT-KEY u8
DYNAMIC-BUFFER KEY-ARENA u8
DYNAMIC-BUFFER REC-ROWS n
DYNAMIC-BUFFER GROUP-ROWS n
DYNAMIC-BUFFER GROUP-HEADS n
DYNAMIC-BUFFER KEY-ROWS n
DYNAMIC-BUFFER KEY-HEADS n
DYNAMIC-BUFFER WID-OWNERS n
DYNAMIC-BUFFER IMPL-ROWS n
DYNAMIC-BUFFER IMPL-TREE n
DYNAMIC-BUFFER EXACT-ROWS n
DYNAMIC-BUFFER EXACT-HEADS n
DYNAMIC-BUFFER INSTANCE-ROWS n
DYNAMIC-BUFFER CON-ROWS n
DYNAMIC-BUFFER FILE-STACK n
DYNAMIC-BUFFER ALLOC-ROWS n
DYNAMIC-BUFFER REF-ROWS u8
DYNAMIC-BUFFER IMPORT-BYTES u8
DYNAMIC-BUFFER PREP-IMPORT-OFF n
DYNAMIC-BUFFER OUTPUT u8
variable KEY-U
variable ARENA-U
variable REC-N
variable GROUP-N
variable KEY-N
variable INDEX-READY
variable WID-CAP
variable IMPL-N
variable IMPL-CAP
variable EXACT-N
variable EXACT-READY
variable PENDING-ORD
variable PENDING-GROUP
variable PENDING-OCC
variable PENDING-KEY-OFF
variable PENDING-KEY-U
variable INSTANCE-N
variable CURRENT-INSTANCE
\ The first wordlist of the innermost file's open run, the one it gives its
\ next wordlist; meaningful while FILE-DEPTH is positive.
variable SEG-FIRST
\ Non-zero once a wordlist rewind may have handed a bound number to another
\ maker (WIDS-SYNC): no file of the session names a wordlist after that.
variable WIDS-WITHDRAWN
variable CON-N
variable FILE-DEPTH
variable FILE-OCC
variable ALLOC-N
variable REF-N
variable IMPORT-N
variable IMPORT-U
variable OUTPUT-U
PTR-VARIABLE PREP-REFS
variable PREP-REF-N
PTR-VARIABLE PREP-SRC
variable PREP-IMPORT-N
variable PREP-REC-BASE
variable PREP-REC-COUNT
variable PREP-CODE-BASE
variable PREP-CODE-U
variable PREP-WIDS-INDEXED
variable PREP-CURSOR
\ While a FILE object's references are prepared (PREP-MAPPED 1), its LOCAL
\ record, wordlist and DATA references resolve through LOCAL-AT: kind, item
\ and addend to the live ordinal, wordlist or address. The object's
\ installer supplies it when the session opens, as it does RECORD-AT, so a
\ build that replays an object holds the hook a cold build does, and refuses
\ an item it has not installed.
TYPED-VARIABLE LOCAL-AT [ n n n -- n ]
variable PREP-MAPPED
variable CAP-CODE
variable CAP-CODE-U
variable CAP-REC
variable CAP-REC-U
variable CAP-READY
variable DEFINITION-ORD
TYPED-VARIABLE RECORD-AT [ n -- ptr n ]
: LIVE-WID ( n -- n ) XREF-REC XREF-WORDLIST ;
defer LIVE-CONTRACT$ ( n -- ptr u8 n bool )
: NO-CONTRACT ( n -- ptr u8 n bool ) drop NULL-PTR 0 false ;
: CONTRACT-DEFAULT ( -- ) ['] NO-CONTRACT is LIVE-CONTRACT$ ;
CONTRACT-DEFAULT
defer FIXED-READ ( n -- n n bool )
: NO-FIXED ( n -- n n bool ) drop 0 0 false ;
: FIXED-DEFAULT ( -- ) ['] NO-FIXED is FIXED-READ ;
FIXED-DEFAULT
CAST: N>U8 ( n -- ptr u8 )
CAST: FAMILY-IDENTITY-XT ( n -- [ n -- ptr u8 n ptr u8 n bool ] )
CAST: FAMILY-FIND-XT ( n -- [ ptr u8 n ptr u8 n -- n bool ] )
variable FAMILY-IDENTITY
variable FAMILY-FIND

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: KEY-ROOM ( n -- ) {: bytes:n :}
   bytes 0 < bytes MEM-MAX-N KEY-U @ - > or if BAD then
   KEY-U @ bytes + KEY-BUF-RESERVE ;
: K-N ( n -- ) {: value:n :}
   8 KEY-ROOM
   value KEY-U @ KEY-BUF LE:U64!
   8 KEY-U +! ;
: K-S ( ptr u8 n -- ) {: a:ptr u:n :}
   u K-N
   u 0= if exit then
   u KEY-ROOM
   a KEY-U @ KEY-BUF u BYTE-COPY
   u KEY-U +! ;
: K-FOLD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u K-S
   KEY-U @ u - {: first:n :}
   u 0 ?do
      first i + KEY-BUF c@ XREF-FOLD-C first i + KEY-BUF c!
   loop ;
: KEY$ ( -- ptr u8 n ) 0 KEY-BUF KEY-U @ ;
: SAVE-KEY ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 <= u MEM-MAX-N ARENA-U @ - > or if BAD then
   ARENA-U @ {: off:n :}
   off u + KEY-ARENA-RESERVE
   a off KEY-ARENA u BYTE-COPY
   u ARENA-U +!
   off ;
: ROW@ ( n n -- n ) {: ord:n field:n :}
   ord REC-CELLS * field + REC-ROWS @ ;
: ROW! ( n n n -- ) {: x:n ord:n field:n :}
   x ord REC-CELLS * field + REC-ROWS ! ;
: GROUP@ ( n n -- n ) {: row:n field:n :}
   row GROUP-CELLS * field + GROUP-ROWS @ ;
: GROUP! ( n n n -- ) {: value:n row:n field:n :}
   value row GROUP-CELLS * field + GROUP-ROWS ! ;
: KEY@ ( n n -- n ) {: row:n field:n :}
   row KEY-CELLS * field + KEY-ROWS @ ;
: KEY! ( n n n -- ) {: value:n row:n field:n :}
   value row KEY-CELLS * field + KEY-ROWS ! ;
: INSTANCE@ ( n n -- n ) {: row:n field:n :}
   row INSTANCE-CELLS * field + INSTANCE-ROWS @ ;
: INSTANCE! ( n n n -- ) {: value:n row:n field:n :}
   value row INSTANCE-CELLS * field + INSTANCE-ROWS ! ;
: CON@ ( n n -- n ) {: row:n field:n :}
   row 3 * field + CON-ROWS @ ;
: CON! ( n n n -- ) {: value:n row:n field:n :}
   value row 3 * field + CON-ROWS ! ;
: ALLOC@ ( n n -- n ) {: row:n field:n :}
   row 6 * field + ALLOC-ROWS @ ;
: ALLOC! ( n n n -- ) {: value:n row:n field:n :}
   value row 6 * field + ALLOC-ROWS ! ;
: GROW-RECS ( n -- ) {: want:n :}
   want REC-N @ <= if exit then
   want 0 < want MEM-MAX-N REC-CELLS / CELL / > or if BAD then
   want REC-CELLS * REC-ROWS-RESERVE
   want REC-N @ ?do
      -1 i REC-OFF ROW! 0 i REC-LEN ROW!
      -1 i REC-CON-OFF ROW! 0 i REC-CON-U ROW!
      0 i REC-TOKEN ROW! 0 i REC-PROJECTED ROW!
   loop
   want REC-N ! ;
: RECORD-KEY$ ( n -- ptr u8 n ) {: ord:n :}
   ord 0 < ord REC-N @ >= or if BAD then
   ord REC-OFF ROW@ {: off:n :}
   off 0 < if BAD then
   off KEY-ARENA ord REC-LEN ROW@ ;
: SAME-KEY? ( ptr u8 n ptr u8 n -- bool ) STR= ;

: HASH-KEY ( ptr u8 n -- n )
   {: key:ptr size:n :}
   5381
   \ Cell reads stay inside the key; mix high bits into the bucket bits.
   size CELL / 0 ?do
      key i CELL * + CELL-VIEW @ xor $9E3779B185EBCA87 *
      dup 32 rshift xor
   loop
   size size CELL / CELL * ?do 33 * key i + c@ + loop
   $7FFFFFFFFFFFFFFF and HASH-SLOTS mod ;

: INDEX-INIT ( -- )
   INDEX-READY @ if exit then
   HASH-SLOTS GROUP-HEADS-RESERVE
   HASH-SLOTS KEY-HEADS-RESERVE
   HASH-SLOTS 0 ?do
      -1 i GROUP-HEADS ! -1 i KEY-HEADS !
   loop
   1 INDEX-READY ! ;

: GROUP-FIND ( ptr u8 n -- n ) {: key:ptr size:n :}
   INDEX-INIT
   key size HASH-KEY GROUP-HEADS @
   begin dup 0 >= while
      dup 0 GROUP@ KEY-ARENA over 1 GROUP@ key size STR= if exit then
      3 GROUP@
   repeat ;

: GROUP-GET ( ptr u8 n -- n ) {: key:ptr size:n :}
   key size GROUP-FIND dup 0 >= if exit then drop
   GROUP-N @ {: row:n :}
   row 1+ GROUP-CELLS * GROUP-ROWS-RESERVE
   key size SAVE-KEY row 0 GROUP!
   size row 1 GROUP!
   0 row 2 GROUP!
   key size HASH-KEY {: slot:n :}
   slot GROUP-HEADS @ row 3 GROUP!
   row slot GROUP-HEADS !
   1 GROUP-N +! row ;

: KEY-FIND ( ptr u8 n -- n ) {: key:ptr size:n :}
   INDEX-INIT
   key size HASH-KEY KEY-HEADS @
   begin dup 0 >= while
      dup 0 KEY@ KEY-ARENA over 1 KEY@ key size STR= if exit then
      4 KEY@
   repeat ;

: KEY-PUT ( ptr u8 n n n -- )
   {: key:ptr size:n ord:n token:n :}
   key size KEY-FIND dup 0 >= if
      {: row:n :}
      ord row 2 KEY! token row 3 KEY! exit
   then drop
   KEY-N @ {: row:n :}
   row 1+ KEY-CELLS * KEY-ROWS-RESERVE
   key size SAVE-KEY row 0 KEY!
   size row 1 KEY!
   ord row 2 KEY! token row 3 KEY!
   key size HASH-KEY {: slot:n :}
   slot KEY-HEADS @ row 4 KEY!
   row slot KEY-HEADS !
   1 KEY-N +! ;

: GROUP-OBSERVE ( ptr u8 n -- ) {: key:ptr size:n :}
   \ A prepared object's external declaration key can be opaque to this
   \ session. Only keys minted by this owner advance a name occurrence.
   size 16 < if exit then
   key LE:U64@ 2 <> if exit then
   key size 8 - GROUP-GET {: row:n :}
   key size 8 - + LE:U64@ {: occurrence:n :}
   occurrence 0 < if BAD then
   occurrence row 2 GROUP@ >= if occurrence 1+ row 2 GROUP! then ;

: WID-ROW ( n -- n ) {: wid:n :}
   wid 0 <= if BAD then
   wid WID-CAP @ >= if
      wid 1+ WID-CELLS * WID-OWNERS-RESERVE
      wid 1+ WID-CAP @ ?do
         i WID-CELLS * {: row:n :}
         -1 row WID-OWNERS ! -1 row 1+ WID-OWNERS !
         -1 row 2 + WID-OWNERS ! 0 row 3 + WID-OWNERS !
      loop
      wid 1+ WID-CAP !
   then
   wid WID-CELLS * ;

: WID-BIND ( n n n -- ) {: wid:n ord:n vis:n :}
   wid 0= if exit then
   wid WID-ROW {: row:n :}
   ord row WID-OWNERS !
   vis row 1+ WID-OWNERS ! ;

public

\ The live namespace row whose public (vis 1) or private (vis 2) wordlist wid
\ is, and true; 0 0 false when none is: a bare `wordlist`'s, or a rolled-back
\ declaration's whose record was cut. The live dictionary is scanned from the
\ newest row, as xref.f WID-PKG scans for a public wordlist: a reopen's
\ namespace-private binds nothing here.
: WID-MAKER ( n -- n n bool )
   {: wid:n :}
   ndict@ 0 ?do
      ndict@ 1- i - {: ord:n :}
      ord XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC wid = if ord 1 true unloop exit then
         rec XREF-PKG-PRIVATE wid = if ord 2 true unloop exit then
      then
   loop
   0 0 false ;

private

\ True when record ord, a projected one while its projection lasts, else a
\ live one, is a namespace row whose public (vis 1) or private (vis 2)
\ wordlist is wid. A cut leaves a binding naming a record that is gone or
\ another declaration's.
: HOLDS? ( n n n -- bool ) {: wid:n ord:n vis:n :}
   ord PREP-REC-BASE @ >= ord PREP-REC-BASE @ PREP-REC-COUNT @ + < and if
      ord RECORD-AT @ execute
   else
      ord ndict@ >= if false exit then
      ord XREF-REC
   then {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> if false exit then
   vis 1 = if rec XREF-PKG-PUBLIC else rec XREF-PKG-PRIVATE then wid = ;

\ The namespace row that owns wordlist wid, its visibility and true. A
\ namespace row binds its wordlists when it publishes; a private wordlist
\ namespace-private gives it later (a reopen, or an install that makes the
\ private one apart from its package row) is bound on its first lookup. A
\ binding whose record no longer holds wid is dropped and wid looked up
\ again.
: WID-OWNER ( n -- n n bool ) {: wid:n :}
   wid 0 <= if -1 0 false exit then
   wid WID-CAP @ < if
      wid WID-CELLS * {: row:n :}
      row WID-OWNERS @ {: ord:n :}
      row 1+ WID-OWNERS @ {: vis:n :}
      ord 0 >= if
         wid ord vis HOLDS? if ord vis true exit then
         -1 row WID-OWNERS ! -1 row 1+ WID-OWNERS !
      then
   then
   wid WID-MAKER {: ord:n vis:n found:bool :}
   found 0= if -1 0 false exit then
   wid ord vis WID-BIND
   ord vis true ;

: WID-BIND-REC ( n -- ) {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> if exit then
   rec XREF-PKG-PUBLIC ord 1 WID-BIND
   rec XREF-PKG-PRIVATE ord 2 WID-BIND ;

: IMPL@ ( n n -- n ) {: row:n field:n :}
   row 4 * field + IMPL-ROWS @ ;
: IMPL! ( n n n -- ) {: value:n row:n field:n :}
   value row 4 * field + IMPL-ROWS ! ;

\ Sorted starts select a prefix; this tree skips subtrees whose intervals
\ have all ended before the requested address.
: IMPL-TREE-BUILD ( -- )
   IMPL-CAP @ 0= if 1 IMPL-CAP ! then
   begin IMPL-CAP @ IMPL-N @ < while
      IMPL-CAP @ 2 * IMPL-CAP !
   repeat
   IMPL-CAP @ 2 * IMPL-TREE-RESERVE
   IMPL-CAP @ 0 ?do 0 IMPL-CAP @ i + IMPL-TREE ! loop
   IMPL-N @ 0 ?do
      i 1 IMPL@ IMPL-CAP @ i + IMPL-TREE !
   loop
   IMPL-CAP @ 1- begin dup 0 > while
      dup 2 * IMPL-TREE @ over 2 * 1+ IMPL-TREE @ max
      over IMPL-TREE ! 1-
   repeat drop ;

: IMPL-TREE-APPEND ( n -- ) {: row:n :}
   row 1 IMPL@ row IMPL-CAP @ + IMPL-TREE !
   row IMPL-CAP @ +
   begin dup 1 > while
      2 /
      dup 2 * IMPL-TREE @ over 2 * 1+ IMPL-TREE @ max
      over IMPL-TREE !
   repeat drop ;

: IMPL-TREE-REFRESH ( n n -- ) {: row:n old:n :}
   IMPL-N @ IMPL-CAP @ > row old < or if
      IMPL-TREE-BUILD exit
   then
   row IMPL-TREE-APPEND ;

: EXACT@ ( n n -- n ) {: row:n field:n :}
   row 4 * field + EXACT-ROWS @ ;
: EXACT! ( n n n -- ) {: value:n row:n field:n :}
   value row 4 * field + EXACT-ROWS ! ;
: EXACT-SLOT ( n -- n ) HASH-SLOTS mod ;
: EXACT-INIT ( -- )
   EXACT-READY @ if exit then
   HASH-SLOTS EXACT-HEADS-RESERVE
   HASH-SLOTS 0 ?do -1 i EXACT-HEADS ! loop
   1 EXACT-READY ! ;

: LIVE-TOKEN ( n -- n ) {: ord:n :}
   ord XREF-REC DEF-OCC:SELECT {: slot:n occurrence:n :}
   slot ord <> if BAD then occurrence ;

\ An exact anonymous entry can sit inside its parent's code span. The
\ occurrence keeps a reclaimed address from naming a later occupant.
: REGISTER-IMPL ( n n -- ) {: entry:n ord:n :}
   entry 0 < ord 0 < ord ndict@ >= or or if BAD then
   ord LIVE-TOKEN {: occurrence:n :}
   EXACT-INIT
   EXACT-N @ {: row:n :}
   row 1+ 4 * EXACT-ROWS-RESERVE
   entry row 0 EXACT!
   ord row 1 EXACT!
   occurrence row 2 EXACT!
   entry EXACT-SLOT {: slot:n :}
   slot EXACT-HEADS @ row 3 EXACT!
   row slot EXACT-HEADS !
   1 EXACT-N +! ;

: RESOLVE-KEEP ( n n -- n n )
   2dup DEF-OCC:RESOLVE drop ;

: LIVE-RECORD? ( n n -- ptr n bool )
   [: RESOLVE-KEEP ;] catch {: code:n :}
   code 0= if drop XREF-REC true exit then
   code DEF-OCC:E-STALE = if 2drop XREF-NULL false exit then
   2drop code throw ;

: IMPL-LIVE? ( n -- bool ) {: row:n :}
   row 2 IMPL@ row 3 IMPL@ LIVE-RECORD?
      if drop true else drop false then ;

\ The first row whose start lies past the address. The bounds live on the stack,
\ so a search leaves no state in the image.
: IMPL-UPPER ( n -- n )
   {: entry:n :}
   0 IMPL-N @
   begin 2dup < while
      2dup + 2 / {: mid:n :}
      mid 0 IMPL@ entry <= if nip mid 1+ swap else drop mid then
   repeat drop ;

: REGISTER-IMPL-RANGE ( n n n -- )
   {: entry:n size:n ord:n :}
   size 0 <= if exit then
   entry 0 < ord 0 < ord ndict@ >= or or if BAD then
   ord LIVE-TOKEN {: occurrence:n :}
   entry IMPL-UPPER {: row:n :}
   row 0 ?do
      row 1- i - {: prior:n :}
      prior 0 IMPL@ entry <> if leave then
      prior 1 IMPL@ entry size + =
      prior 2 IMPL@ ord = and
      prior 3 IMPL@ occurrence = and if unloop exit then
   loop
   IMPL-N @ {: old:n :}
   old 1+ 4 * IMPL-ROWS-RESERVE
   old row - 0 ?do
      old i - 1- {: source:n :}
      4 0 ?do
         source i IMPL@ source 1+ i IMPL!
      loop
   loop
   entry row 0 IMPL!
   entry size + row 1 IMPL!
   ord row 2 IMPL!
   occurrence row 3 IMPL!
   1 IMPL-N +!
   row old IMPL-TREE-REFRESH ;

: REF! ( n n n n -- n ) {: kind:n owner:n item:n addend:n :}
   REF-N @ {: idx:n :}
   idx MEM-MAX-N REF-BYTES / >= if BAD then
   idx 1+ REF-BYTES * REF-ROWS-RESERVE
   idx REF-BYTES * {: off:n :}
   kind off REF-ROWS LE:U64!
   owner off 8 + REF-ROWS LE:U64!
   item off 16 + REF-ROWS LE:U64!
   addend off 24 + REF-ROWS LE:U64!
   1 REF-N +!
   idx 1+ ;

: IMPORT-ROOM ( n -- ) {: bytes:n :}
   bytes 0 < bytes MEM-MAX-N IMPORT-U @ - > or if BAD then
   IMPORT-U @ bytes + IMPORT-BYTES-RESERVE ;
: OUTPUT-ROOM ( n -- ) {: bytes:n :}
   bytes 0 < bytes MEM-MAX-N OUTPUT-U @ - > or if BAD then
   OUTPUT-U @ bytes + OUTPUT-RESERVE ;
: O-N ( n -- ) {: value:n :}
   8 OUTPUT-ROOM
   value OUTPUT-U @ OUTPUT LE:U64!
   8 OUTPUT-U +! ;
: O-S ( ptr u8 n -- ) {: a:ptr u:n :}
   u OUTPUT-ROOM
   a OUTPUT-U @ OUTPUT u BYTE-COPY
   u OUTPUT-U +! ;
: PREP-ROOM ( n n -- ) {: bytes:n size:n :}
   bytes 0 < bytes size PREP-CURSOR @ - > or if BAD then ;

: IMPORT-KEY ( n ptr u8 n n n -- n )
   {: kind:n key:ptr keyu:n extent:n alignment:n :}
   keyu 0 <= extent 0 < or alignment 0 <= or if BAD then
   IMPORT-N @ {: row:n :}
   32 keyu + IMPORT-ROOM
   kind IMPORT-U @ IMPORT-BYTES LE:U64!
   keyu IMPORT-U @ 8 + IMPORT-BYTES LE:U64!
   extent IMPORT-U @ 16 + IMPORT-BYTES LE:U64!
   alignment IMPORT-U @ 24 + IMPORT-BYTES LE:U64!
   key IMPORT-U @ 32 + IMPORT-BYTES keyu BYTE-COPY
   32 keyu + IMPORT-U +!
   1 IMPORT-N +!
   row ;

: PREP-IMPORT-ROW$ ( n -- n ptr u8 n n n ) {: idx:n :}
   idx 0 < idx PREP-IMPORT-N @ >= or if BAD then
   PREP-SRC @ idx PREP-IMPORT-OFF @ + {: row:ptr :}
   row LE:U64@ row 32 + row 8 + LE:U64@
   row 16 + LE:U64@ row 24 + LE:U64@ ;

: IMPORT-DECL ( n n -- n ) {: kind:n ord:n :}
   kind ord RECORD-KEY$ 0 1 IMPORT-KEY ;

public

: BIND-FAMILY ( n n -- )
   FAMILY-FIND ! FAMILY-IDENTITY ! ;

: RESET-CONSTRUCTORS ( -- ) 0 CON-N ! ;

EXPORT IMPORT-KEY
EXPORT PREP-IMPORT-ROW$
EXPORT REGISTER-IMPL

1 constant CODE
2 constant RECORD
3 constant WID
4 constant DATA
5 constant LITERAL
0 constant LOCAL
1 constant EXTERNAL
\ A wordlist no record owns: item 0 the global wordlist (wid 0), item 1 the
\ retired one.
2 constant BUILTIN

\ The loader supplies the FILE-instance key: its parent instance, the load
\ kind, whether the file lies under the engine's source root or another one,
\ its path relative to that root, and the same-child load occurrence. Each
\ instance owns its own family ordinals.
: FILE-INSTANCE! ( ptr u8 n -- ) {: key:ptr keyu:n :}
   keyu 0 <= if BAD then
   INSTANCE-N @ {: row:n :}
   row 1+ INSTANCE-CELLS * INSTANCE-ROWS-RESERVE
   key keyu SAVE-KEY row 0 INSTANCE!
   keyu row 1 INSTANCE!
   0 row 2 INSTANCE!
   0 row 3 INSTANCE!
   row 1+ INSTANCE-N ! row CURRENT-INSTANCE ! ;

: FILE-INSTANCE-CLEAR ( -- ) -1 CURRENT-INSTANCE ! ;

: INSTANCE-ID ( -- n )
   CURRENT-INSTANCE @ dup 0 < if BAD then ;

private

: INST-KEY$ ( n -- ptr u8 n ) {: row:n :}
   row 0 INSTANCE@ KEY-ARENA row 1 INSTANCE@ ;

public

: INSTANCE-KEY$ ( -- ptr u8 n ) INSTANCE-ID INST-KEY$ ;

private

: OVERLAY-OPEN? ( -- bool ) data-base REPLAY-SCOPE:LATCH + @ 0<> ;

\ Close the innermost file's open run: bind the wordlists it made to that
\ file's instance, after those it made before; the caller starts the next
\ run. The runs between PUSH-FILE and POP-FILE are NUNIT-OBJECT's segments:
\ both close at a child load and reopen after it, and a run's count is WIDN
\ less its first wordlist as WID-ITEM's is, so each item is the index the
\ writer gives it and a hit's install mints the keys its cold load did.
\ Only a checker overlay's rollback lowers WIDN (replay-widn!), to a mark
\ taken while the overlay was open and never below the WIDN it saved when it
\ opened; one overlay opens at a time. An overlay that opens inside a run
\ rewinds only that run's numbers, which its count and WID-ITEM's both net
\ out. One open at a file boundary may rewind numbers already bound, by any
\ instance, or across the run starting there: it withdraws every file
\ identity of the session, so no binding names a wordlist another maker now
\ holds. A run can close below its first wordlist only after such a
\ boundary, so a negative count with identities kept is a broken invariant.
: WIDS-SYNC ( -- )
   OVERLAY-OPEN? if 1 WIDS-WITHDRAWN ! then
   FILE-DEPTH @ 0= if exit then
   INSTANCE-ID {: inst:n :}
   inst 3 INSTANCE@ {: base:n :}
   SEG-FIRST @ {: first:n :}
   AOT-ARM:WIDN first - {: count:n :}
   WIDS-WITHDRAWN @ 0<> if exit then
   count 0 < if BAD then
   count 0 ?do
      first i + WID-ROW {: row:n :}
      inst row 2 + WID-OWNERS !
      base i + row 3 + WID-OWNERS !
   loop
   base count + inst 3 INSTANCE! ;

\ The instance row that made wordlist wid and wid's item among that
\ instance's wordlists; -1 0 when no file of the session made it or the
\ session's file identities are withdrawn. The innermost file's open run
\ answers live, unbound: a lookup never closes a run, so a rewind inside it
\ leaves no binding behind.
: WID-MADE ( n -- n n ) {: wid:n :}
   WIDS-WITHDRAWN @ 0<> if -1 0 exit then
   FILE-DEPTH @ 0 > wid SEG-FIRST @ >= and wid AOT-ARM:WIDN < and if
      INSTANCE-ID {: inst:n :}
      inst inst 3 INSTANCE@ wid + SEG-FIRST @ - exit
   then
   wid 0 <= wid WID-CAP @ >= or if -1 0 exit then
   wid WID-CELLS * {: row:n :}
   row 2 + WID-OWNERS @ {: inst:n :}
   inst 0 < if -1 0 exit then
   inst row 3 + WID-OWNERS @ ;

\ The key of the file that made wordlist wid and true; NULL-PTR 0 false when
\ no file of the session made it.
: WID-FILE-KEY? ( n -- ptr u8 n bool ) {: wid:n :}
   wid WID-MADE {: inst:n item:n :}
   inst 0 < if NULL-PTR 0 false exit then
   0 KEY-U ! 8 K-N WID-FILE K-N inst INST-KEY$ K-S item K-N
   KEY$ true ;

\ True when key has a file wordlist's form: 8, WID-FILE, the instance key
\ and the item, as WID-FILE-KEY? builds it.
: WID-FILE-FORM? ( ptr u8 n -- bool ) {: key:ptr keyu:n :}
   keyu 32 < if false exit then
   key LE:U64@ 8 <> key 8 + LE:U64@ WID-FILE <> or if false exit then
   key 16 + LE:U64@ {: instu:n :}
   instu 0 > instu keyu 32 - = and ;

\ The instance row and item a file wordlist's key names and true; 0 0 false
\ for a key of another form or an instance this session did not make.
: WID-KEY-PARTS? ( ptr u8 n -- n n bool ) {: key:ptr keyu:n :}
   key keyu WID-FILE-FORM? 0= if 0 0 false exit then
   key 16 + LE:U64@ {: instu:n :}
   INSTANCE-N @ 0 ?do
      i 1 INSTANCE@ instu = if
         i INST-KEY$ key 24 + instu STR= if
            i key 24 + instu + LE:U64@ true unloop exit
         then
      then
   loop
   0 0 false ;

public

: PACKAGE-OF ( ptr n -- ptr u8 n n ) {: rec:ptr :}
   rec XREF-WORDLIST {: wid:n :}
   wid XREF-NAMESPACE-WL = if NULL-PTR 0 0 exit then
   wid XREF-RETIRED-WL = if NULL-PTR 0 3 exit then
   wid OWNER-API-PUB-WID = if s" hb-engine-api" 1 exit then
   wid OWNER-API-PRI-WID = if s" hb-engine-api" 2 exit then
   wid 0= if NULL-PTR 0 0 exit then
   wid WID-OWNER if {: ord:n vis:n :}
      ord XREF-REC XREF-NAME$ vis exit
   then 2drop BAD ;

: PUSH-FILE ( ptr u8 n n n -- )
   {: name:ptr nameu:n role:n kind:n :}
   nameu 0 <= role 0 < or kind 0 < or if BAD then
   \ The parent's run closes under the parent's instance.
   WIDS-SYNC
   0 KEY-U ! 4 K-N
   CURRENT-INSTANCE @ {: parent:n :}
   parent 0 >= if
      parent INST-KEY$ K-S
   else
      NULL-PTR 0 K-S
   then
   kind K-N role K-N name nameu K-S
   KEY-U @ {: baseu:n :}
   0 FILE-OCC !
   INSTANCE-N @ 0 ?do
      i 1 INSTANCE@ baseu 8 + = if
         i 0 INSTANCE@ KEY-ARENA baseu 0 KEY-BUF baseu STR= if
            FILE-OCC @ 1+ FILE-OCC !
         then
      then
   loop
   FILE-OCC @ K-N
   KEY$ FILE-INSTANCE!
   FILE-DEPTH @ 1+ FILE-STACK-RESERVE
   CURRENT-INSTANCE @ FILE-DEPTH @ FILE-STACK !
   FILE-DEPTH @ 1+ FILE-DEPTH !
   AOT-ARM:WIDN SEG-FIRST ! ;

: POP-FILE ( -- )
   FILE-DEPTH @ 0 <= if BAD then
   \ The child's last run closes under the child's instance.
   WIDS-SYNC
   -1 FILE-DEPTH +!
   AOT-ARM:WIDN SEG-FIRST !
   FILE-DEPTH @ 0= if -1 CURRENT-INSTANCE ! exit then
   FILE-DEPTH @ 1- FILE-STACK @ CURRENT-INSTANCE ! ;

: FAMILY-KEY$ ( n -- ptr u8 n bool ) {: id:n :}
   id FAMILY-IDENTITY @ FAMILY-IDENTITY-XT execute
      0= if 2drop 2drop NULL-PTR 0 false exit then
   {: pkg:ptr pkgu:n name:ptr nameu:n :}
   0 KEY-U ! 7 K-N
   pkg pkgu K-FOLD
   name nameu K-S
   KEY$ true ;

: FAMILY-PARTS? ( ptr u8 n -- ptr u8 n ptr u8 n bool )
   {: key:ptr keyu:n :}
   keyu 24 < if NULL-PTR 0 NULL-PTR 0 false exit then
   key LE:U64@ 7 <> if NULL-PTR 0 NULL-PTR 0 false exit then
   key 8 + LE:U64@ {: pkgu:n :}
   pkgu 0 < pkgu keyu 24 - > or if
      NULL-PTR 0 NULL-PTR 0 false exit then
   key 16 pkgu + + LE:U64@ {: nameu:n :}
   nameu 0 < nameu keyu 24 pkgu + - <> or if
      NULL-PTR 0 NULL-PTR 0 false exit then
   key 16 + pkgu key 24 pkgu + + nameu true ;

: FAMILY-KEY-MATCH? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n pkg:ptr pkgu:n name:ptr nameu:n :}
   key keyu FAMILY-PARTS? 0= if 2drop 2drop false exit then
   {: actual-pkg:ptr actual-pkgu:n actual-name:ptr actual-nameu:n :}
   actual-pkg actual-pkgu pkg pkgu STR=CI
   actual-name actual-nameu name nameu STR= and ;

: FAMILY-BY-KEY ( ptr u8 n -- n bool )
   FAMILY-PARTS? 0= if 2drop 2drop -1 false exit then
   FAMILY-FIND @ FAMILY-FIND-XT execute ;

: CON-KEY-NEXT$ ( -- ptr u8 n )
   CURRENT-INSTANCE @ {: row:n :}
   row 0 < row INSTANCE-N @ >= or if BAD then
   0 KEY-U ! 6 K-N
   row INST-KEY$ K-S
   row 2 INSTANCE@ dup K-N 1+ row 2 INSTANCE!
   KEY$ ;

: CON-KEY$ ( n -- ptr u8 n bool ) {: id:n :}
   CON-N @ 0 ?do
      i 0 CON@ id = if
         i 1 CON@ KEY-ARENA i 2 CON@ true unloop exit
      then
   loop NULL-PTR 0 false ;

: CON-BY-KEY ( ptr u8 n -- n bool ) {: key:ptr keyu:n :}
   CON-N @ 0 ?do
      i 1 CON@ KEY-ARENA i 2 CON@ key keyu STR= if
         i 0 CON@ true unloop exit
      then
   loop -1 false ;

: BIND-CON ( n ptr u8 n -- ) {: id:n key:ptr keyu:n :}
   id 0 < keyu 0 <= or if BAD then
   key keyu CON-BY-KEY if id <> if BAD then exit then drop
   CON-N @ 0 ?do
      i 0 CON@ id = if
         key keyu i 1 CON@ KEY-ARENA i 2 CON@ STR= 0= if BAD then
         unloop exit
      then
   loop
   CON-N @ {: row:n :}
   row 1+ 3 * CON-ROWS-RESERVE
   id row 0 CON!
   key keyu SAVE-KEY row 1 CON!
   keyu row 2 CON!
   1 CON-N +! ;

: CON-DECLARED ( n -- ) {: id:n :}
   CURRENT-INSTANCE @ 0 < if exit then
   CON-KEY-NEXT$ {: key:ptr keyu:n :}
   id key keyu BIND-CON ;

\ The publication owner calls this for the exact record it just emitted.
: BIND-DECL ( n ptr u8 n -- )
   {: ord:n raw:ptr keyu:n :}
   ord 0 < keyu 0 <= or if BAD then
   ord PENDING-ORD @ = if
      raw keyu PENDING-KEY-OFF @ KEY-ARENA PENDING-KEY-U @ STR=
         0= if BAD then
   then
   keyu INPUT-KEY-RESERVE
   raw 0 INPUT-KEY keyu BYTE-COPY
   0 INPUT-KEY {: key:ptr :}
   ord 1+ GROW-RECS
   ord REC-OFF ROW@ 0 >= if
      ord RECORD-KEY$ key keyu SAME-KEY? 0= if BAD then
      exit
   then
   key keyu GROUP-OBSERVE
   key keyu SAVE-KEY ord REC-OFF ROW!
   keyu ord REC-LEN ROW!
   key keyu ord 0 KEY-PUT ;

\ A published dictionary slot is identified by the engine's occurrence,
\ not by this session's key-table insertion order. A prepared projection has
\ no occurrence until its records are installed.
: BIND-LIVE ( n -- ) {: ord:n :}
   ord XREF-REC DEF-OCC:SELECT {: slot:n occurrence:n :}
   slot ord <> if BAD then
   occurrence ord REC-TOKEN ROW!
   0 ord REC-PROJECTED ROW!
   ord RECORD-KEY$ ord occurrence KEY-PUT ;

: CLEAR-RECORD ( n -- ) {: ord:n :}
   -1 ord REC-OFF ROW! 0 ord REC-LEN ROW!
   -1 ord REC-CON-OFF ROW! 0 ord REC-CON-U ROW!
   0 ord REC-TOKEN ROW! 0 ord REC-PROJECTED ROW! ;

\ A dictionary slot is reused only after a cut, and DICT-CUT has already
\ cleared the row of every record a cut removes. A bound row whose occurrence
\ went stale is therefore the same declaration re-stamped in place: a does>
\ reissues the occurrence of the record it patches and publishes it again. It
\ keeps its key and name occurrence and takes the live occurrence. A row past
\ the count is a record a cut removed, reported or not: it is cleared. A row
\ with no occurrence was declared but never published, and stays only for
\ the pending definition or a projected record.
: RETIRE-REUSED ( n -- ) {: ord:n :}
   ord REC-OFF ROW@ 0 < if exit then
   ord REC-TOKEN ROW@ {: token:n :}
   token 0= if
      ord PENDING-ORD @ = ord REC-PROJECTED ROW@ 0<> or if exit then
      ord CLEAR-RECORD exit
   then
   ord token LIVE-RECORD? if drop exit then drop
   ord ndict@ >= if ord CLEAR-RECORD exit then
   ord BIND-LIVE ;

: BASE-KEY$ ( n -- ptr u8 n ) {: ord:n :}
   CURRENT-INSTANCE @ {: instance:n :}
   instance 0 < instance INSTANCE-N @ >= or if BAD then
   ord XREF-REC {: rec:ptr :}
   rec PACKAGE-OF {: pkg:ptr pkgu:n vis:n :}
   vis 3 = if BAD then
   rec XREF-NAME$ {: name:ptr nameu:n :}
   0 KEY-U ! 2 K-N
   instance INST-KEY$ K-S
   pkg pkgu K-FOLD vis K-N name nameu K-FOLD
   KEY$ ;

: BIND-CONTRACT ( n ptr u8 n -- )
   {: ord:n bytes:ptr size:n :}
   ord 0 < ord REC-N @ >= or size 0 <= or if BAD then
   ord REC-CON-OFF ROW@ 0 >= if
      ord REC-CON-OFF ROW@ KEY-ARENA ord REC-CON-U ROW@
         bytes size STR= 0= if BAD then
      exit
   then
   bytes size SAVE-KEY ord REC-CON-OFF ROW!
   size ord REC-CON-U ROW! ;

: BIND-RECORD ( n -- ) {: ord:n :}
   ord 0 < ord ndict@ >= or if BAD then
   ord 1+ GROW-RECS
   ord RETIRE-REUSED
   ord REC-OFF ROW@ 0 >= if
      \ Prepared rows carry the original identity, even if later retired.
      ord REC-TOKEN ROW@ 0= ord REC-PROJECTED ROW@ 0= and if
         ord RECORD-KEY$ {: key:ptr keyu:n :}
         ord BASE-KEY$ {: base:ptr baseu:n :}
         keyu baseu 8 + <> if BAD then
         key baseu base baseu STR= 0= if BAD then
      then
      ord BIND-LIVE exit
   then
   ord PENDING-ORD @ = if
      PENDING-KEY-OFF @ KEY-ARENA PENDING-KEY-U @
   else
      ord BASE-KEY$ GROUP-GET {: group:n :}
      group 2 GROUP@ K-N
      KEY$
   then
   {: key:ptr keyu:n :}
   ord key keyu BIND-DECL
   ord BIND-LIVE ;

: PENDING-CLEAR ( -- )
   -1 PENDING-ORD ! -1 PENDING-GROUP !
   0 PENDING-OCC ! -1 PENDING-KEY-OFF ! 0 PENDING-KEY-U ! ;
PENDING-CLEAR
-1 DEFINITION-ORD !

: PENDING-RELEASE ( -- )
   PENDING-ORD @ 0 < if exit then
   PENDING-ORD @ {: ord:n :}
   ord ndict@ = ord REC-N @ < and if
      ord REC-OFF ROW@ 0 >= if
         ord CLEAR-RECORD
      then
   then
   PENDING-GROUP @ {: group:n :}
   group 2 GROUP@ PENDING-OCC @ 1+ = if
      PENDING-OCC @ group 2 GROUP!
   then
   PENDING-CLEAR ;

: PENDING-KEY$ ( -- ptr u8 n )
   PENDING-ORD @ 0 >= if BAD then
   CURRENT-INSTANCE @ 0 < if BAD then
   ndict@ {: ord:n :}
   ord REC-N @ < if ord RETIRE-REUSED then
   ord BASE-KEY$ {: base:ptr baseu:n :}
   base baseu GROUP-GET {: group:n :}
   group 2 GROUP@ {: occurrence:n :}
   occurrence K-N
   KEY$ {: key:ptr keyu:n :}
   key keyu SAVE-KEY PENDING-KEY-OFF !
   keyu PENDING-KEY-U !
   ord PENDING-ORD ! group PENDING-GROUP !
   occurrence PENDING-OCC !
   occurrence 1+ group 2 GROUP!
   ord PENDING-KEY-OFF @ KEY-ARENA keyu BIND-DECL
   PENDING-KEY-OFF @ KEY-ARENA PENDING-KEY-U @ ;

: ON-PUBLISHED ( n -- ) {: ord:n :}
   CURRENT-INSTANCE @ 0 < if exit then
   ord BIND-RECORD
   ord WID-BIND-REC
   ord XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> if
      rec XREF-START rec XREF-CODE-BYTES ord REGISTER-IMPL-RANGE
   then ;

\ The kernel's capture cut, noticed before the count drops to kept: the
\ records from kept to the count go, and a later record in one of their slots
\ is another declaration, which binds its own key. Rows past the count, the
\ pending definition's and projected ones, are no record the cut removes.
: DICT-CUT ( n -- ) {: kept:n :}
   ndict@ REC-N @ min kept ?do i CLEAR-RECORD loop ;

private

\ The pending definition ord is the one being published.
: PUBLISH-CHECK ( n -- ) {: ord:n :}
   ord 0 < ord ndict@ >= or if BAD then
   PENDING-ORD @ ord <> if BAD then ;

: PUBLISH-LIVE ( n -- ) {: ord:n :}
   ord BIND-LIVE
   ord LIVE-CONTRACT$ if
      ord -rot BIND-CONTRACT
   else 2drop then
   PENDING-CLEAR ;

public

: PUBLISH-DEFINITION ( n -- ) {: ord:n :}
   ord PUBLISH-CHECK
   ord PENDING-KEY-OFF @ KEY-ARENA PENDING-KEY-U @ BIND-DECL
   ord PUBLISH-LIVE ;

\ Publish the pending definition ord whose declaration a file object already
\ bound: PUBLISH-DEFINITION without binding its declaration.
: PUBLISH-PREPARED ( n -- )
   dup PUBLISH-CHECK PUBLISH-LIVE ;

: ALLOC-KEY$ ( n -- ptr u8 n ) {: row:n :}
   row 0 < row ALLOC-N @ >= or if BAD then
   row 0 ALLOC@ KEY-ARENA row 1 ALLOC@ ;

: FIND-ALLOC ( ptr u8 n -- n bool ) {: key:ptr keyu:n :}
   ALLOC-N @ 0 ?do
      i ALLOC-KEY$ key keyu STR= if i true unloop exit then
   loop -1 false ;

: BIND-ALLOC ( ptr u8 n n n n n -- n )
   {: key:ptr keyu:n base:n extent:n alignment:n storage:n :}
   keyu 0 <= extent 0 <= or alignment 0 <= or if BAD then
   key keyu FIND-ALLOC if {: row:n :}
      row 2 ALLOC@ base <> row 3 ALLOC@ extent <> or
      row 4 ALLOC@ alignment <> or
      row 5 ALLOC@ storage <> or if BAD then
      row exit
   then drop
   ALLOC-N @ {: row:n :}
   row 1+ 6 * ALLOC-ROWS-RESERVE
   key keyu SAVE-KEY row 0 ALLOC!
   keyu row 1 ALLOC!
   base row 2 ALLOC!
   extent row 3 ALLOC!
   alignment row 4 ALLOC!
   storage row 5 ALLOC!
   1 ALLOC-N +!
   row ;

\ Key every record below boot, the dictionary the process held when its
\ first session opened, as the seed's. A record an earlier session or the
\ code between sessions made is no file's the session can name: it stays
\ unkeyed, so a read of it is an unknown dependency and a write to it an
\ unowned update. Binding a package's wordlists is not keying: every
\ package record binds them, so PACKAGE-OF names the package of a record a
\ later file defines into it.
: BIND-SEED ( n -- ) {: boot:n :}
   s" hb-seed" FILE-INSTANCE!
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      i boot < if
         rec XREF-RETIRED? if
            \ A retired row no longer has a wordlist to name its declaration.
            0 KEY-U ! SEED-DECL K-N INSTANCE-KEY$ K-S i K-N
            i KEY$ BIND-DECL
            i BIND-LIVE
         else
            i BIND-RECORD
         then
         rec XREF-WORDLIST XREF-NAMESPACE-WL <> if
            rec XREF-START rec XREF-CODE-BYTES i REGISTER-IMPL-RANGE
         then
      then
      i WID-BIND-REC
   loop
   here BYTE-VIEW data-base BYTE-VIEW - {: bytes:n :}
   bytes 0 > if
      0 KEY-U ! 5 K-N
      s" hb-seed" K-S 0 K-N
      KEY$ DATA-VA VA>N bytes 1 0 BIND-ALLOC drop
   then
   FILE-INSTANCE-CLEAR ;

: DECL-KEY$ ( n -- ptr u8 n ) RECORD-KEY$ ;

: DECL-KEY? ( n -- ptr u8 n bool ) {: ord:n :}
   ord 0 < ord REC-N @ >= or if NULL-PTR 0 false exit then
   ord REC-OFF ROW@ 0 < if NULL-PTR 0 false exit then
   ord RECORD-KEY$ true ;

\ ord was alive when the session opened: BIND-SEED keyed its declaration in
\ the seed's file instance, the key's second field.
: SEED-RECORD? ( n -- bool ) {: ord:n :}
   ord DECL-KEY? {: key:ptr keyu:n known:bool :}
   known 0= if false exit then
   s" hb-seed" {: seed:ptr seedu:n :}
   keyu 16 seedu + < if false exit then
   key 8 + LE:U64@ seedu <> if false exit then
   key 16 + seedu seed seedu STR= ;

: INDEX-PREPARED-WIDS ( -- )
   PREP-REC-COUNT @ 0 ?do
      PREP-REC-BASE @ i + {: ord:n :}
      ord RECORD-AT @ execute {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC ord 1 WID-BIND
         rec XREF-PKG-PRIVATE ord 2 WID-BIND
      then
   loop
   1 PREP-WIDS-INDEXED ! ;

: MAYBE-INDEX-PREPARED-WIDS ( -- )
   PREP-WIDS-INDEXED @ 0= if INDEX-PREPARED-WIDS then ;

private

\ The live wordlist whose maker's key is key and true; 0 false when none is.
\ A file names only a wordlist no namespace owns, as WID-KEY? does: the
\ wordlist a file made at that item is a miss once a namespace owns it.
: WID-BY-KEY? ( ptr u8 n -- n bool ) {: key:ptr keyu:n :}
   key keyu WID-KEY-PARTS? 0= if 2drop 0 false exit then
   {: inst:n item:n :}
   AOT-ARM:WIDN 1 ?do
      i WID-MADE item = swap inst = and if
         MAYBE-INDEX-PREPARED-WIDS
         i WID-OWNER nip nip if 0 false else i true then unloop exit
      then
   loop
   0 false ;

public

\ The bytes that name wordlist wid to a later session: its owning namespace
\ record, -1 when none owns it, then the key and true. A builtin is named by
\ its tag, an owned wordlist by its namespace's declaration key and
\ visibility, and only an unowned one by the session file that made it. The
\ owner and NULL-PTR 0 false when nothing names it: the owner's declaration
\ has no key (a package an earlier session made), or, with -1, no file of
\ the session made the unowned wordlist.
: WID-KEY? ( n -- n ptr u8 n bool ) {: wid:n :}
   0 KEY-U ! 8 K-N
   wid 0= if -1 0 K-N KEY$ true exit then
   wid XREF-RETIRED-WL = if -1 1 K-N KEY$ true exit then
   wid OWNER-API-PUB-WID = if -1 4 K-N KEY$ true exit then
   wid OWNER-API-PRI-WID = if -1 5 K-N KEY$ true exit then
   MAYBE-INDEX-PREPARED-WIDS
   wid WID-OWNER if {: ord:n vis:n :}
      ord DECL-KEY? 0= if 2drop ord NULL-PTR 0 false exit then
      {: key:ptr keyu:n :}
      0 KEY-U ! 8 K-N vis 1 = if 2 else 3 then K-N key keyu K-S
      ord KEY$ true exit
   then 2drop
   -1 wid WID-FILE-KEY? ;

: CONTRACT$ ( n -- ptr u8 n bool ) {: ord:n :}
   ord 0 < ord REC-N @ >= or if NULL-PTR 0 false exit then
   ord ndict@ < if ord LIVE-CONTRACT$ exit then
   ord REC-CON-OFF ROW@ {: off:n :}
   off 0 < if NULL-PTR 0 false exit then
   off KEY-ARENA ord REC-CON-U ROW@ true ;

: USE-LIVE-CONTRACT ( [ n -- ptr u8 n bool ] -- ) is LIVE-CONTRACT$ ;
: USE-FIXED-READ ( [ n -- n n bool ] -- ) is FIXED-READ ;
: RECORD-RETIRED? ( n -- bool )
   LIVE-WID XREF-RETIRED-WL = ;

: REC-ORD ( ptr n -- n ) {: rec:ptr :}
   rec XREF-NULL = if -1 exit then
   rec XREF-A>U8 dbase@ N>U8 - {: off:n :}
   off 0 < off ndict@ DREC * >= or off DREC mod 0<> or if -1 exit then
   off DREC / ;

: FIND-NAMESPACE ( ptr u8 n -- n ) {: name:ptr nameu:n :}
   name nameu XREF-NAMESPACE-WL XREF:INDEXED-WL
   dup XREF-FOUND? 0= if drop -1 exit then
   REC-ORD ;

: FIND-WORD ( ptr u8 n n -- n )
   {: name:ptr nameu:n wid:n :}
   name nameu wid XREF:INDEXED-WL
   dup XREF-FOUND? 0= if drop -1 exit then
   REC-ORD ;

: QUALIFIED-TAIL ( ptr u8 n -- ptr u8 n )
   {: spelling:ptr spellingu:n :}
   spellingu 0 ?do
      spelling i + c@ [char] : = if
         i 0= i spellingu 1- = or if
            spelling spellingu unloop exit
         then
         spelling i 1+ + spellingu i 1+ - unloop exit
      then
   loop spelling spellingu ;

\ A resolution observation names one eligible wordlist, not a new search
\ order. Recheck exactly that candidate against the accepted prefix.
: RESOLVE-CANDIDATE ( n ptr u8 n ptr u8 n -- n n )
   {: role:n spelling:ptr spellingu:n namespace:ptr namespaceu:n :}
   role 0= if
      spelling spellingu QUALIFIED-TAIL 0 FIND-WORD 0 exit
   then
   namespace namespaceu FIND-NAMESPACE {: ns:n :}
   ns -2 = if -2 0 exit then
   ns -1 = if -1 0 exit then
   ns RECORD-AT @ execute {: rec:ptr :}
   1 rec XREF-PKG-PRIVATE 0<> if 2 or then {: shape:n :}
   role 5 >= if ns shape exit then
   role 2 = if rec XREF-PKG-PRIVATE else rec XREF-PKG-PUBLIC then
      {: wid:n :}
   role 4 = if
      spelling spellingu QUALIFIED-TAIL
   else spelling spellingu then wid FIND-WORD shape ;

: KEY-RECORD ( ptr u8 n -- ptr n n ) {: key:ptr keyu:n :}
   key keyu KEY-FIND dup 0 < if drop XREF-NULL -1 exit then
   {: row:n :}
   row 2 KEY@ {: ord:n :}
   ord 0 < ord REC-N @ >= or if XREF-NULL -1 exit then
   ord REC-TOKEN ROW@ row 3 KEY@ <> if XREF-NULL -1 exit then
   ord RECORD-KEY$ key keyu STR= 0= if XREF-NULL -1 exit then
   ord ndict@ < if
      row 3 KEY@ {: occurrence:n :}
      occurrence 0= if XREF-NULL -1 exit then
      ord occurrence LIVE-RECORD? if ord else drop XREF-NULL -1 then exit
   then
   ord PENDING-ORD @ = if XREF-NULL -1 exit then
   ord RECORD-AT @ execute ord ;

private

\ A retired record keeps the storage its definer stamped, and code compiled
\ before the `undefine` still reaches it; its name only is gone.
: DATA-ORIGIN-RECORD ( ptr n n -- n bool ) {: rec:ptr ord:n :}
   ord 0 < ord ndict@ >= or if 0 false exit then
   rec XREF-RETIRED? if rec NDICT:STAMP-ORIGIN exit then
   ord FIXED-READ {: kind:n value:n known:bool :}
   kind CHECKER-OWNER-ABI:RECORD-FIXED-ADDR = known and
      if value true exit then
   rec NDICT:REC-DEFER-CELL {: cell:n :}
   cell 0= if 0 false exit then
   cell true ;

public

: DATA-ORIGIN-BASE ( n -- n bool ) {: ord:n :}
   ord 0 < ord ndict@ >= or if 0 false exit then
   ord RECORD-AT @ execute ord DATA-ORIGIN-RECORD ;

: DATA-BY-KEY ( ptr u8 n n n n n -- n bool )
   {: key:ptr keyu:n addend:n requested:n extent:n alignment:n :}
   addend 0 < requested 0 < or extent 0 < or alignment 0 <= or
      if 0 false exit then
   key keyu KEY-RECORD {: rec:ptr ord:n :}
   rec XREF-NULL = if 0 false exit then
   rec ord DATA-ORIGIN-RECORD {: base:n known:bool :}
   known 0= if 0 false exit then
   requested 0<> extent 0<> or if 0 false exit then
   base addend + alignment mod 0<> if 0 false exit then
   base addend + true ;

: DECL-BY-KEY ( ptr u8 n -- n )
   KEY-RECORD {: rec:ptr ord:n :}
   rec NULL-PTR = if BAD then
   ord ;

: PREP-DECL-ORD ( n n -- n ) {: kind:n item:n :}
   item PREP-IMPORT-ROW$ {: rowkind:n key:ptr keyu:n extent:n alignment:n :}
   extent drop alignment drop
   rowkind kind <> if BAD then
   key keyu DECL-BY-KEY ;

: RECORD-AT! ( [ n -- ptr n ] -- ) RECORD-AT ! ;
: LOCAL-AT! ( [ n n n -- n ] -- ) LOCAL-AT ! ;

\ Reference index zero is the only null value. The fixed row format is
\ kind/owner/item/addend in little-endian u64, never a live process address.
: REF-LOCAL ( n n n -- n ) {: kind:n item:n addend:n :}
   kind LOCAL item addend REF! ;
: REF-IMPORT ( n n n -- n ) {: kind:n item:n addend:n :}
   kind EXTERNAL item addend REF! ;

\ One pending definition owns only its emitted code and one dictionary row.
\ Its source DATA prefix and FILE allocation range are outside this frame.
: BEGIN-DEFINITION-BOUNDS ( n n n -- )
   {: code:n size:n ord:n :}
   CAP-READY @ if BAD then
   code 0 <= size 0 <= or ord ndict@ <> or if BAD then
   code CAP-CODE ! size CAP-CODE-U !
   ord CAP-REC ! 1 CAP-REC-U !
   0 REF-N ! 0 IMPORT-N ! 0 IMPORT-U !
   1 CAP-READY ! ;

: END-CAPTURE ( -- ) 0 CAP-READY ! ;

: BEGIN-DEFINITION ( n -- ) {: ord:n :}
   ord ndict@ <> DEFINITION-ORD @ 0 >= or if BAD then
   ord DEFINITION-ORD ! ;
: END-DEFINITION ( -- ) -1 DEFINITION-ORD ! ;
: CURRENT-RECORD? ( n -- bool ) {: ord:n :}
   DEFINITION-ORD @ 0 >= ord DEFINITION-ORD @ = and ;

: REF-RECORD ( n -- n ) {: ord:n :}
   ord 0 < ord CAP-REC @ CAP-REC-U @ + >= or if BAD then
   ord CAP-REC @ >= if RECORD ord CAP-REC @ - 0 REF-LOCAL exit then
   RECORD ord IMPORT-DECL {: row:n :}
   RECORD row 0 REF-IMPORT ;

\ Record ord as a FILE object reference: row, its row among the object's own
\ records, else below zero for an earlier file's record, which it imports.
: FILE-REF-RECORD ( n n -- n ) {: row:n ord:n :}
   row 0 >= if RECORD row 0 REF-LOCAL exit then
   RECORD ord IMPORT-DECL {: decl:n :}
   RECORD decl 0 REF-IMPORT ;

: IMPL-SEARCH ( n n n n n -- n bool )
   {: entry:n node:n first:n last:n upper:n :}
   first upper >= if -1 false exit then
   node IMPL-TREE @ entry <= if -1 false exit then
   last first - 1 = if
      first IMPL-LIVE? if first 2 IMPL@ true exit then
      -1 false exit
   then
   first last + 2 / {: mid:n :}
   entry node 2 * 1+ mid last upper recurse
      if true exit then drop
   entry node 2 * first mid upper recurse ;

: IMPL-OWNER ( n -- n bool ) {: entry:n :}
   entry 0 < if -1 false exit then
   EXACT-READY @ if
      entry EXACT-SLOT EXACT-HEADS @
      begin dup 0 >= while
         dup 0 EXACT@ entry = if
            dup 1 EXACT@ over 2 EXACT@ LIVE-RECORD?
               if drop 1 EXACT@ true exit else drop then
         then
         3 EXACT@
      repeat drop
   then
   entry 1 0 IMPL-CAP @ entry IMPL-UPPER IMPL-SEARCH ;

: REF-CODE ( n -- n ) {: target:n :}
   target CAP-CODE @ >= target CAP-CODE @ CAP-CODE-U @ + < and if
      CODE target CAP-CODE @ - 0 REF-LOCAL exit
   then
   target IMPL-OWNER 0= if drop BAD then {: ord:n :}
   ord XREF-REC XREF-START {: first:n :}
   CODE ord IMPORT-DECL {: row:n :}
   CODE row target first - REF-IMPORT ;

\ The emitter names the declaration that supplied this literal. A bare
\ allocation address cannot establish that provenance.
: REF-DECL-DATA ( n n n n -- n )
   {: ord:n addend:n extent:n alignment:n :}
   addend 0 < extent 0 < or alignment 0 <= or if BAD then
   ord DECL-KEY? 0= if 2drop BAD then
   {: key:ptr keyu:n :}
   DATA key keyu extent alignment IMPORT-KEY {: row:n :}
   DATA row addend REF-IMPORT ;

: REF-LITERAL ( n n -- n ) {: ordinal:n addend:n :}
   ordinal 0 < addend 0 < or if BAD then
   LITERAL ordinal addend REF-LOCAL ;

\ The fixed kind and value the owner records for live record ord.
: FIXED@ ( n -- n n bool ) FIXED-READ ;

\ A FILE object's own references, taken with no definition capture open.
: BEGIN-FILE-REFS ( -- )
   CAP-READY @ if BAD then
   0 REF-N ! 0 IMPORT-N ! 0 IMPORT-U ! 0 CAP-CODE-U ! ;

\ The reference to wordlist wid made before the file: builtin, imported from
\ the package record that owns it, or imported by the key of the file that
\ made it. 0 when an unkeyed record owns it or nothing names it.
: REF-WID ( n -- n ) {: wid:n :}
   wid 0= if WID BUILTIN 0 0 REF! exit then
   wid XREF-RETIRED-WL = if WID BUILTIN 1 0 REF! exit then
   wid WID-OWNER 0= if 2drop
      wid WID-FILE-KEY? 0= if 2drop 0 exit then
      {: key:ptr keyu:n :}
      WID key keyu 0 1 IMPORT-KEY {: row:n :}
      WID row 0 REF-IMPORT exit
   then
   {: ord:n vis:n :}
   \ A package an earlier session made has no key; its 0 refuses the record
   \ as an unowned one does.
   ord DECL-KEY? nip nip 0= if 0 exit then
   WID ord IMPORT-DECL {: row:n :}
   WID row vis 1- REF-IMPORT ;

\ Declarations carry exact publication contracts beside their stable identity.
: ENCODE ( n n -- ptr u8 n ) {: first:n last:n :}
   first 0 < last first < or last REC-N @ > or if BAD then
   0 OUTPUT-U !
   REF-N @ O-N IMPORT-N @ O-N
   last first - O-N
   REF-N @ 0<> if REF-N @ REF-BYTES * 0 REF-ROWS swap O-S then
   IMPORT-U @ 0<> if 0 IMPORT-BYTES IMPORT-U @ O-S then
   last first ?do
      i RECORD-KEY$ {: key:ptr keyu:n :}
      i first - O-N
      keyu O-N
      i REC-CON-U ROW@ O-N
      key keyu O-S
      i REC-CON-U ROW@ 0<> if
         i REC-CON-OFF ROW@ KEY-ARENA i REC-CON-U ROW@ O-S
      then
   loop
   0 OUTPUT OUTPUT-U @ ;

private

\ Check the header, reference and import rows of a prepared table and make it
\ current; the cursor ends at the declarations, whose count is returned.
: PREPARE-ROWS ( ptr u8 n -- n ) {: src:ptr size:n :}
   size HEAD-BYTES < if BAD then
   src LE:U64@ {: refs:n :}
   src 8 + LE:U64@ {: imports:n :}
   refs 0 < refs size HEAD-BYTES - REF-BYTES / > or if BAD then
   imports 0 < imports size HEAD-BYTES - refs REF-BYTES * - 32 / > or
      if BAD then
   refs PREP-REF-N !
   src PREP-SRC !
   src HEAD-BYTES + PREP-REFS !
   HEAD-BYTES refs REF-BYTES * + PREP-CURSOR !
   imports PREP-IMPORT-N !
   imports PREP-IMPORT-OFF-RESERVE
   imports 0 ?do
      32 size PREP-ROOM
      PREP-CURSOR @ i PREP-IMPORT-OFF !
      src PREP-CURSOR @ 8 + + LE:U64@ {: keyu:n :}
      keyu 0 <= keyu size PREP-CURSOR @ 32 + - > or if BAD then
      src PREP-CURSOR @ 24 + + LE:U64@ 0 <= if BAD then
      32 keyu + PREP-CURSOR +!
   loop
   0 PREP-WIDS-INDEXED !
   src 16 + LE:U64@ ;

public

\ A prepared object keeps its copied section bytes alive through installation.
\ This parse registers projected declarations before any consumer resolves an
\ import, and checks every length before advancing into variable rows.
: PREPARE ( ptr u8 n n n n -- )
   {: src:ptr size:n recbase:n codebase:n codeu:n :}
   0 PREP-REC-COUNT ! 0 PREP-MAPPED !
   recbase PREP-REC-BASE ! codebase PREP-CODE-BASE !
   codeu PREP-CODE-U !
   src size PREPARE-ROWS {: decls:n :}
   \ Definition capture exports exactly its one pending dictionary record.
   decls 1 <> if BAD then
   decls 0 ?do
      DECL-HEAD size PREP-ROOM
      src PREP-CURSOR @ + {: row:ptr :}
      row LE:U64@ i <> if BAD then
      row 8 + LE:U64@ {: keyu:n :}
      row 16 + LE:U64@ {: contractu:n :}
      contractu 0 < if BAD then
      DECL-HEAD PREP-CURSOR +!
      keyu 0 <= keyu size PREP-CURSOR @ - > or if BAD then
      src PREP-CURSOR @ + {: key:ptr :}
      keyu PREP-CURSOR +!
      recbase i + key keyu BIND-DECL
      1 recbase i + REC-PROJECTED ROW!
      1 PREP-REC-COUNT +!
      contractu size PREP-CURSOR @ - > if BAD then
      contractu 0<> if
         recbase i + src PREP-CURSOR @ + contractu BIND-CONTRACT
      then
      contractu PREP-CURSOR +!
   loop
   PREP-CURSOR @ size <> if BAD then ;

\ A FILE object's own references carry no declarations.
: PREPARE-FILE ( ptr u8 n -- )
   {: src:ptr size:n :}
   src size PREPARE-ROWS 0<> if BAD then
   PREP-CURSOR @ size <> if BAD then
   0 PREP-REC-BASE ! 0 PREP-REC-COUNT !
   0 PREP-CODE-BASE ! 0 PREP-CODE-U !
   1 PREP-MAPPED !
   \ A file's records publish through ON-PUBLISHED (WID-BIND-REC); nothing is
   \ projected, so INDEX-PREPARED-WIDS has nothing to index.
   1 PREP-WIDS-INDEXED ! ;

\ A refused import may have published projected keys before its later sections
\ were checked. Installed records have live occurrences and no longer project.
: DISCARD-PREPARED ( -- )
   PREP-REC-COUNT @ 0 > if
      PREP-REC-BASE @ PREP-REC-COUNT @ +
      PREP-REC-BASE @ ?do
         i REC-PROJECTED ROW@ 0<> i REC-TOKEN ROW@ 0= and if
            i RECORD-KEY$ KEY-FIND dup 0 >= if
               {: row:n :}
               row 2 KEY@ i = row 3 KEY@ 0= and if
                  -1 row 2 KEY!
               then
            else drop then
            i CLEAR-RECORD
         then
      loop
   then
   0 PREP-REC-COUNT ! ;

: PREP-REF@ ( n -- n n n n ) {: idx:n :}
   idx 0 <= idx PREP-REF-N @ > or if BAD then
   PREP-REFS @ idx 1- REF-BYTES * + {: row:ptr :}
   row LE:U64@ row 8 + LE:U64@
   row 16 + LE:U64@ row 24 + LE:U64@ ;

: REF-KIND ( n -- n ) PREP-REF@ 2drop drop ;
\ The owner of reference idx: LOCAL, EXTERNAL or BUILTIN.
: REF-OWNER ( n -- n ) PREP-REF@ 2drop nip ;

: REF>RECORD ( n -- n ) {: idx:n :}
   idx 0= if -1 exit then
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind RECORD <> addend 0<> or if BAD then
   owner LOCAL = if
      PREP-MAPPED @ if RECORD item 0 LOCAL-AT @ execute exit then
      item 0 < item PREP-REC-COUNT @ >= or if BAD then
      PREP-REC-BASE @ item + exit
   then
   owner EXTERNAL = if RECORD item PREP-DECL-ORD exit then
   BAD ;

\ The record a RECORD reference names and true; -1 false when no live record
\ has an EXTERNAL reference's key: the record is gone.
: REF>RECORD? ( n -- n bool ) {: idx:n :}
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind RECORD <> addend 0<> or if BAD then
   owner EXTERNAL <> if idx REF>RECORD true exit then
   item PREP-IMPORT-ROW$ {: rowkind:n key:ptr keyu:n extent:n alignment:n :}
   extent drop alignment drop
   rowkind RECORD <> if BAD then
   key keyu KEY-RECORD {: rec:ptr ord:n :}
   rec XREF-NULL = if -1 false exit then
   ord true ;

: REF>CODE ( n -- n ) {: idx:n :}
   idx 0= if 0 exit then
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind CODE <> if BAD then
   owner LOCAL = if
      item 0 < item PREP-CODE-U @ >= or if BAD then
      addend 0<> if BAD then
      PREP-CODE-BASE @ item + exit
   then
   owner EXTERNAL = if
      CODE item PREP-DECL-ORD RECORD-AT @ execute {: rec:ptr :}
      addend 0 < addend rec XREF-CODE-BYTES >= or if BAD then
      rec XREF-START addend + exit
   then
   BAD ;

: REF>DATA ( n n -- ptr u8 ) {: idx:n extent:n :}
   idx 0= extent 0 < or if BAD then
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind DATA <> addend 0 < or if BAD then
   owner EXTERNAL = if
      item PREP-IMPORT-ROW$ {: rowkind:n key:ptr keyu:n
         requested:n alignment:n :}
      rowkind DATA <> requested extent < or if BAD then
      key keyu FIND-ALLOC if {: row:n :}
         alignment row 4 ALLOC@ > if BAD then
         addend row 3 ALLOC@ >
            requested row 3 ALLOC@ addend - > or
            extent row 3 ALLOC@ addend - > or if BAD then
         row 2 ALLOC@ addend + N>U8 exit
      then drop
      key keyu addend requested extent alignment DATA-BY-KEY
         0= if drop BAD then
      N>U8 exit
   then
   BAD ;

\ The DATA address a LOCAL DATA reference of a prepared FILE object names.
: REF>LOCAL-DATA ( n -- n ) {: idx:n :}
   idx 0= if BAD then
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind DATA <> owner LOCAL <> or PREP-MAPPED @ 0= or if BAD then
   DATA item addend LOCAL-AT @ execute ;

\ The code address a LOCAL CODE reference of a prepared FILE object names:
\ its record row, through LOCAL-AT, the live record, and the addend inside
\ that record's code.
: REF>LOCAL-CODE ( n -- n ) {: idx:n :}
   idx 0= if BAD then
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind CODE <> owner LOCAL <> or PREP-MAPPED @ 0= or if BAD then
   RECORD item 0 LOCAL-AT @ execute RECORD-AT @ execute {: rec:ptr :}
   addend 0 < addend rec XREF-CODE-BYTES >= or if BAD then
   rec XREF-START addend + ;

: REF>LITERAL ( n -- n n ) {: idx:n :}
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind LITERAL <> owner LOCAL <> or if BAD then
   item addend ;

\ The wordlist a WID reference names and true: builtin, one the file's
\ object made, one no namespace owns that an earlier file made (by its key,
\ addend 0), or a public (addend 0) or private (addend 1) wordlist of an
\ imported package. 0 false when no such wordlist or record has the import's
\ key: the maker is gone. A malformed reference is BAD.
: REF>WID? ( n -- n bool ) {: idx:n :}
   idx 0= if BAD then
   idx PREP-REF@ {: kind:n owner:n item:n addend:n :}
   kind WID <> if BAD then
   owner BUILTIN = if
      addend 0<> if BAD then
      item 0= if 0 true exit then
      item 1 = if XREF-RETIRED-WL true exit then
      BAD
   then
   owner LOCAL = if
      addend 0<> PREP-MAPPED @ 0= or if BAD then
      WID item 0 LOCAL-AT @ execute true exit
   then
   owner EXTERNAL <> addend 0 < or addend 1 > or if BAD then
   item PREP-IMPORT-ROW$ {: rowkind:n key:ptr keyu:n extent:n alignment:n :}
   extent drop alignment drop
   rowkind WID <> if BAD then
   key keyu WID-FILE-FORM? if
      addend 0<> if BAD then
      key keyu WID-BY-KEY? exit
   then
   key keyu KEY-RECORD drop {: rec:ptr :}
   rec XREF-NULL = if 0 false exit then
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> if BAD then
   addend 0= if rec XREF-PKG-PUBLIC else rec XREF-PKG-PRIVATE then true ;

: REF>WID ( n -- n ) REF>WID? 0= if BAD then ;

private

\ Result identity per definition, one row per ordinal for the session: a
\ digest of everything that fixes the word's runtime behavior and nothing a
\ comment or a relocation moves, and the edges its code and addresses name.
\ A row is the occurrence of the record it was bound for (0 for none), its
\ first edge row, its edge count and the worst host reason over its
\ functions. A cut that reuses the ordinal retires that occurrence, and with
\ it the row.
4 constant RESULT-CELLS
\ An edge row: reference kind (CODE, DATA or LITERAL), then the key row of the
\ declaration it imports. A LOCAL reference is the definition itself and needs
\ no edge.
2 constant EDGE-CELLS
32 constant RESULT-BYTES
\ Sections 2 and 3 hold one 40-byte row per site, its offset in section 0
\ first; a call row's reference is at 16, an address row's at 24.
40 constant SITE-BYTES
DYNAMIC-BUFFER RESULT-ROWS n
DYNAMIC-BUFFER RESULT-DIGESTS u8
DYNAMIC-BUFFER EDGE-ROWS n
DYNAMIC-BUFFER RESULT-IMPORT-OFF n
DYNAMIC-BUFFER RESULT-CODE u8
\ The hash context, then one cell of number bytes.
DYNAMIC-BUFFER RESULT-WORK u8
variable RESULT-N
\ The worst host reason over the functions R-HOST has read for one result.
variable HOST-WORST
variable EDGE-N
variable RESULT-IMPORT-N

: RESULT@ ( n n -- n ) {: ord:n field:n :}
   ord RESULT-CELLS * field + RESULT-ROWS @ ;
: RESULT! ( n n n -- ) {: value:n ord:n field:n :}
   value ord RESULT-CELLS * field + RESULT-ROWS ! ;
: EDGE@ ( n n -- n ) {: row:n field:n :}
   row EDGE-CELLS * field + EDGE-ROWS @ ;

: RESULT-GROW ( n -- ) {: ord:n :}
   ord RESULT-N @ < if exit then
   ord 1+ {: want:n :}
   want MEM-MAX-N RESULT-BYTES / > if BAD then
   want RESULT-CELLS * RESULT-ROWS-RESERVE
   want RESULT-BYTES * RESULT-DIGESTS-RESERVE
   want RESULT-N @ ?do 0 i 0 RESULT! 0 i 1 RESULT! 0 i 2 RESULT! 0 i 3 RESULT! loop
   want RESULT-N ! ;

\ Whether ord's row was bound for the record live at ord now.
: R-BOUND? ( n -- bool ) {: ord:n :}
   ord 0 < ord RESULT-N @ >= or if false exit then
   ord 0 RESULT@ {: occurrence:n :}
   occurrence 0= if false exit then
   ord occurrence LIVE-RECORD? nip ;

: R-CTX ( -- ptr u8 ) 0 RESULT-WORK ;
: R-N ( n -- )
   SHA256-CTX-BYTES RESULT-WORK {: cell:ptr :}
   cell LE:U64!
   R-CTX cell 8 SHA256-FEED ;
: R-S ( ptr u8 n -- ) {: a:ptr u:n :}
   u R-N
   u 0 > if R-CTX a u SHA256-FEED then ;

: EDGE+ ( n n -- ) {: kind:n key:n :}
   EDGE-N @ {: row:n :}
   row 1+ EDGE-CELLS * EDGE-ROWS-RESERVE
   kind row EDGE-CELLS * EDGE-ROWS !
   key row EDGE-CELLS * 1+ EDGE-ROWS !
   1 EDGE-N +! ;

\ Index the import rows of section 13 as ENCODE wrote it: the counts, the
\ reference rows, then each import's kind, key length, extent and alignment
\ before its key.
: R-IMPORTS ( ptr u8 n -- ) {: src:ptr size:n :}
   size HEAD-BYTES < if BAD then
   src LE:U64@ {: refs:n :}
   src 8 + LE:U64@ {: imports:n :}
   refs 0 < refs size HEAD-BYTES - REF-BYTES / > or if BAD then
   imports 0 < imports size HEAD-BYTES - refs REF-BYTES * - 32 / > or
      if BAD then
   imports RESULT-IMPORT-OFF-RESERVE
   imports RESULT-IMPORT-N !
   HEAD-BYTES refs REF-BYTES * +
   imports 0 ?do
      {: off:n :}
      off 32 + size > if BAD then
      src off + 8 + LE:U64@ {: keyu:n :}
      keyu 0 <= keyu size off 32 + - > or if BAD then
      off i RESULT-IMPORT-OFF !
      off 32 + keyu +
   loop drop ;

\ Reference idx of section 13: kind, owner, item and addend.
: R-REF@ ( ptr u8 n -- n n n n ) {: src:ptr idx:n :}
   idx 0 <= idx src LE:U64@ > or if BAD then
   src HEAD-BYTES + idx 1- REF-BYTES * + {: row:ptr :}
   row LE:U64@ row 8 + LE:U64@ row 16 + LE:U64@ row 24 + LE:U64@ ;

: R-KEY$ ( ptr u8 n -- ptr u8 n ) {: src:ptr item:n :}
   item 0 < item RESULT-IMPORT-N @ >= or if BAD then
   src item RESULT-IMPORT-OFF @ + {: row:ptr :}
   row 32 + row 8 + LE:U64@ ;

\ Hash reference idx's target and return its addend. A LOCAL target is the
\ definition itself, its own code or literal row; an EXTERNAL one is the
\ declaration key, whose row KEY-PUT made when the record was bound, and an edge.
: R-TARGET ( ptr u8 n -- n ) {: src:ptr idx:n :}
   src idx R-REF@ {: kind:n owner:n item:n addend:n :}
   kind CODE <> kind DATA <> and kind LITERAL <> and if BAD then
   kind R-N owner R-N
   owner LOCAL = if item R-N addend exit then
   owner EXTERNAL <> if BAD then
   src item R-KEY$ {: key:ptr keyu:n :}
   key keyu R-S
   key keyu KEY-FIND {: row:n :}
   row 0 < if BAD then
   kind row EDGE+ addend ;

\ Section 0 with the branch word at every call site zeroed: a moved target
\ rewrites it, the rows name it. Address carriers need no mask: capture already
\ zeroed them (tools/native-unit-object.f DEF-CAPTURE-ADDRS).
: R-MASK ( n n n -- ) {: site:n u:n size:n :}
   site 0 < site u + size > or if BAD then
   u 0 ?do 0 site i + RESULT-CODE c! loop ;

: R-CODE ( ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n calls:ptr callsu:n :}
   codeu 0 <= if BAD then
   codeu RESULT-CODE-RESERVE
   code 0 RESULT-CODE codeu BYTE-COPY
   callsu SITE-BYTES / 0 ?do
      calls i SITE-BYTES * + LE:U64@ 4 codeu R-MASK
   loop
   0 RESULT-CODE codeu R-S ;

\ Each site row's offset, kind, and the target and addend of its reference
\ at offset at (16 for a call row, 24 for an address row); a call's `loc` and
\ `known` stay out.
: R-SITES ( ptr u8 n ptr u8 n -- ) {: rows:ptr rowsu:n src:ptr at:n :}
   rowsu SITE-BYTES mod 0<> if BAD then
   rowsu SITE-BYTES / dup R-N 0 ?do
      rows i SITE-BYTES * + {: row:ptr :}
      row LE:U64@ R-N row 8 + LE:U64@ R-N
      src row at + LE:U64@ R-TARGET R-N
   loop ;

\ Section 1's recorded length, wide flag and minimum input depth; its code
\ length repeats section 0 and its history deltas stay out.
: R-META ( ptr u8 n -- ) {: meta:ptr metau:n :}
   metau 32 < if BAD then
   meta LE:U64@ R-N meta 16 + LE:U64@ R-N meta 24 + LE:U64@ R-N ;

public

\ NHOST:FIXED, the class of a definition naming a fixed record, and
\ NHOST:REBIND, of one compiling `is`. Named here because NHOST is baked and
\ a retained host's NHOST lacks them.
7 constant HOST-FIXED
8 constant HOST-REBIND

\ The reasons whose body the published facts account for (host.f's rule):
\ SAFE, MEMORY and FIXED; INDIRECT: checked code executes only a typed
\ xt, a quotation or tick the result's edges name or one a defer or declared
\ code cell holds, which the closure walk follows, and the runs a text it
\ evaluates makes are observed as the file's own; and REBIND: a defer's
\ dispatch cell is a declared address cell from its creation, so the xt an
\ `is` stores in an earlier file's defer is a slot row the object carries and
\ an edge the walk follows, and one stored in the file's own defer is its
\ DATA. Every other reason, a future one included, refuses.
: REASON-ADMITTED? ( n -- bool )
   {: reason:n :}
   reason NHOST:SAFE = reason NHOST:MEMORY = or reason HOST-FIXED = or
   reason NHOST:INDIRECT = or reason HOST-REBIND = or ;

private

\ The worse of two host reasons: one that is not admitted, the first such,
\ else the greater, SAFE < MEMORY < INDIRECT < HOST-FIXED < HOST-REBIND
\ among the admitted. The reasons are not ordered by risk: CAST lies below MEMORY.
: WORSE ( n n -- n ) {: a:n b:n :}
   a REASON-ADMITTED? 0= if a exit then
   b REASON-ADMITTED? 0= if b exit then
   a b max ;

\ Section 15: the function count, then each function's offset, length, depth
\ in and out, reason, loc and type count, then its types; a negative count
\ has none. Every cell but loc. Each reason folds into HOST-WORST.
: R-HOST ( ptr u8 n -- ) {: a:ptr u:n :}
   u 8 < if BAD then
   a LE:U64@ {: funs:n :}
   funs R-N
   8 funs 0 ?do
      {: off:n :}
      off 56 + u > if BAD then
      a off + {: row:ptr :}
      5 0 ?do row i 8 * + LE:U64@ R-N loop
      HOST-WORST @ row 32 + LE:U64@ WORSE HOST-WORST !
      row 48 + LE:U64@ dup R-N 0 max {: types:n :}
      types u off 56 + - 8 / > if BAD then
      types 0 ?do row 56 + i 8 * + LE:U64@ R-N loop
      off 56 + types 8 * +
   loop
   u <> if BAD then ;

public

\ Bind ord's result identity from the definition payload in the current
\ NUNIT-FILE section view, once per live record. The digest covers sections 0
\ (masked), 2 and 3 by target, 10, 15 without loc, and section 1's fields;
\ sections 5, 13's declaration rows and 14 stay out.
: BIND-RESULT ( n -- ) {: ord:n :}
   ord 0 < ord ndict@ >= or if BAD then
   ord R-BOUND? if BAD then
   ord XREF-REC DEF-OCC:SELECT {: slot:n occurrence:n :}
   slot ord <> occurrence 0= or if BAD then
   ord RESULT-GROW
   13 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   src size R-IMPORTS
   EDGE-N @ {: first:n :}
   SHA256-CTX-BYTES 8 + RESULT-WORK-RESERVE
   R-CTX SHA256-BEGIN
   s" habu-def-result-v1" R-S
   1 NUNIT-FILE:SECTION$ R-META
   0 NUNIT-FILE:SECTION$ 2 NUNIT-FILE:SECTION$ R-CODE
   2 NUNIT-FILE:SECTION$ src 16 R-SITES
   3 NUNIT-FILE:SECTION$ src 24 R-SITES
   10 NUNIT-FILE:SECTION$ R-S
   NHOST:SAFE HOST-WORST !
   15 NUNIT-FILE:SECTION$ R-HOST
   R-CTX ord RESULT-BYTES * RESULT-DIGESTS SHA256-END
   occurrence ord 0 RESULT!
   first ord 1 RESULT!
   EDGE-N @ first - ord 2 RESULT!
   HOST-WORST @ ord 3 RESULT! ;

\ The closure walk compares this: ord's 32-byte result digest, or false for a
\ record with no bound result. False is terminal only for a seed record; any
\ other record without a result (does-pending, refused by capture, compiled
\ with no file open) is unknown.
: RESULT-DIGEST ( n -- ptr u8 bool ) {: ord:n :}
   ord R-BOUND? 0= if NULL-PTR false exit then
   ord RESULT-BYTES * RESULT-DIGESTS true ;

: RESULT-EDGES ( n -- n ) {: ord:n :}
   ord R-BOUND? 0= if 0 exit then
   ord 2 RESULT@ ;

\ The closure walk follows edge idx of ord's result: its kind (CODE, DATA or
\ LITERAL) and the live record its target names now, -1 when none does.
: RESULT-EDGE ( n n -- n n ) {: ord:n idx:n :}
   idx 0 < idx ord RESULT-EDGES >= or if BAD then
   ord 1 RESULT@ idx + {: row:n :}
   row 0 EDGE@ {: kind:n :}
   row 1 EDGE@ {: key:n :}
   kind key 0 KEY@ KEY-ARENA key 1 KEY@ KEY-RECORD nip ;

\ The worst host reason over the functions of ord's result (NHOST:SAFE,
\ MEMORY, HOST-FIXED, CAST, ...), or false for a record with no bound result.
: RESULT-REASON ( n -- n bool ) {: ord:n :}
   ord R-BOUND? 0= if 0 false exit then
   ord 3 RESULT@ true ;

: CLOSE ( -- )
   0 FAMILY-IDENTITY ! 0 FAMILY-FIND !
   CONTRACT-DEFAULT
   FIXED-DEFAULT
   KEY-BUF-RELEASE INPUT-KEY-RELEASE KEY-ARENA-RELEASE REC-ROWS-RELEASE
   GROUP-ROWS-RELEASE GROUP-HEADS-RELEASE
   KEY-ROWS-RELEASE KEY-HEADS-RELEASE
   WID-OWNERS-RELEASE IMPL-ROWS-RELEASE IMPL-TREE-RELEASE
   EXACT-ROWS-RELEASE
   EXACT-HEADS-RELEASE
   INSTANCE-ROWS-RELEASE CON-ROWS-RELEASE
   FILE-STACK-RELEASE
   ALLOC-ROWS-RELEASE
   REF-ROWS-RELEASE IMPORT-BYTES-RELEASE
   PREP-IMPORT-OFF-RELEASE OUTPUT-RELEASE
   RESULT-ROWS-RELEASE RESULT-DIGESTS-RELEASE EDGE-ROWS-RELEASE
   RESULT-IMPORT-OFF-RELEASE RESULT-CODE-RELEASE RESULT-WORK-RELEASE
   0 RESULT-N ! 0 EDGE-N ! 0 RESULT-IMPORT-N !
   0 KEY-U ! 0 ARENA-U ! 0 REC-N ! 0 REF-N !
   0 GROUP-N ! 0 KEY-N ! 0 INDEX-READY ! 0 WID-CAP !
   0 IMPL-N ! 0 IMPL-CAP ! 0 EXACT-N ! 0 EXACT-READY !
   0 INSTANCE-N ! -1 CURRENT-INSTANCE ! 0 CON-N !
   0 FILE-DEPTH ! 0 FILE-OCC ! 0 SEG-FIRST ! 0 WIDS-WITHDRAWN !
   0 ALLOC-N !
   0 IMPORT-N ! 0 IMPORT-U ! 0 OUTPUT-U !
   NULL-PTR PREP-REFS ! 0 PREP-REF-N ! 0 PREP-IMPORT-N !
   NULL-PTR PREP-SRC ! 0 PREP-REC-BASE ! 0 PREP-REC-COUNT !
   0 PREP-CODE-BASE ! 0 PREP-CODE-U !
   0 PREP-WIDS-INDEXED ! 0 PREP-CURSOR !
   0 PREP-MAPPED !
   0 CAP-CODE ! 0 CAP-CODE-U ! 0 CAP-REC ! 0 CAP-REC-U !
   0 CAP-READY !
   -1 DEFINITION-ORD ! PENDING-CLEAR ;

;package
