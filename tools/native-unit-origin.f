\ Source-origin acceptances owned by one native FILE. The section stores
\ portable references and symbol keys; the receiver imports the acceptances
\ only after its checker and code have been installed.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require src/core/include.f
require src/core/checker-owner-abi.f
require src/habu/xref.f
require tools/native-file-reference.f

package NUNIT-ORIGIN
private

$4F524947494E3034 constant MAGIC
48 constant HEADER-BYTES
72 constant ROW-BYTES
0 constant EARLY
1 constant FULL
2 constant ABSENT
0 constant KEEP-HOOK
1 constant EARLY-HOOK
2 constant FULL-HOOK

DYNAMIC-BUFFER BYTES u8
variable USED
variable COUNT
variable MARK-SERIAL
variable MARK-HOOK
variable MARK-EARLY-XT
variable MARK-EARLY-ROWS
variable MARK-FULL-ROWS
variable FLOOR
variable CURSOR
TYPED-VARIABLE OWNER-QUERY [ -- ptr u8 ]
create PATH-BUF FS-PATH-CAP allot

CAST: MARK-XT ( n -- [ -- n n bool ] )
CAST: BOUNDARY-XT ( n -- [ -- n n ] )
CAST: HOOK-XT ( n -- [ -- bool ] )
CAST: FULL-STATE-XT ( n -- [ -- bool bool ] )
CAST: EARLY-RESTORE-XT ( n -- [ n bool -- ] )
CAST: FULL-RESTORE-XT ( n -- [ bool -- ] )
CAST: ACTION-XT ( n -- [ -- ] )
CAST: EARLY-ROWS-XT ( n -- [ n [ n n n n ptr u8 n ptr u8 n n n -- ] -- ] )
CAST: ROWS-XT ( n -- [ n [ n n n n ptr u8 n ptr u8 n n n -- ] -- ] )
CAST: EARLY-IMPORT-XT ( n -- [ n n n n ptr u8 n ptr u8 n -- ] )
CAST: FULL-IMPORT-XT ( n -- [ n n ptr u8 n ptr u8 n n n -- ] )
CAST: SYM-IDENTITY-XT ( n -- [ n -- ptr u8 n ptr u8 n n ] )
CAST: ORIGIN-SYM-XT ( n -- [ ptr u8 n n ptr u8 n -- n ] )
CAST: UNIT-SYM-XT ( n -- [ n ptr u8 n n ptr u8 n -- bool ] )

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: MALFORMED ( -- ) E-NUNIT-FILE throw ;
: NO-OWNER ( -- ptr u8 ) NULL-PTR ;
' NO-OWNER OWNER-QUERY !

: OWNER ( -- ptr u8 )
   OWNER-QUERY @ execute dup NULL-PTR = if BAD then ;

: OWNER-FIELD ( n -- n )
   OWNER swap + CELL-VIEW @ dup 0= if BAD then ;

: TARGET-XT? ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   dup XREF-RETIRED? if drop 0 exit then
   XREF-START dup cp@ >= if drop 0 then ;

: PENDING-TARGET-XT? ( ptr u8 n -- n )
   TARGET-XT? dup FLOOR @ < if drop 0 then ;

: TARGET-XT ( ptr u8 n -- n )
   TARGET-XT? dup 0= if BAD then ;

: ROOM ( n -- n )
   {: need:n :}
   need 0 < need MEM-MAX-N USED @ - > or if BAD then
   USED @ {: off:n :}
   off need + BYTES-RESERVE
   off need + USED !
   off ;

: APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   u ROOM {: off:n :}
   u 0 > if a off BYTES u BYTE-COPY then ;

: KEY$ ( n -- ptr u8 n n ptr u8 n )
   {: sym:n :}
   sym 0= if NULL-PTR 0 0 NULL-PTR 0 exit then
   sym CHECKER-OWNER-ABI:VERIFY-SYM-IDENTITY-OFF OWNER-FIELD
      SYM-IDENTITY-XT execute
      {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
   pkg pkgu vis tail tailu ;

: ROOT-REL ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   pathu rootu 1 + <= if NULL-PTR 0 exit then
   path rootu root rootu STR= 0= if NULL-PTR 0 exit then
   path rootu + c@ 47 <> if NULL-PTR 0 exit then
   path rootu 1 + + pathu rootu 1 + - ;

: PUT-ROW ( n n ptr u8 n n ptr u8 n ptr u8 n ptr u8 n n n -- )
   {: kind:n ref:n pkg:ptr pkgu:n vis:n tail:ptr tailu:n
      path:ptr pathu:n name:ptr nameu:n at:n end:n :}
   pkgu 0 < tailu 0 < or pathu 0 < or nameu 0 < or if BAD then
   COUNT @ MEM-MAX-N = if BAD then
   ROW-BYTES ROOM {: off:n :}
   kind off BYTES LE:U64!
   ref off 8 + BYTES LE:U64!
   pkgu off 16 + BYTES LE:U64!
   vis off 24 + BYTES LE:U64!
   tailu off 32 + BYTES LE:U64!
   pathu off 40 + BYTES LE:U64!
   nameu off 48 + BYTES LE:U64!
   at off 56 + BYTES LE:U64!
   end off 64 + BYTES LE:U64!
   pkg pkgu APPEND
   tail tailu APPEND
   path pathu APPEND
   name nameu APPEND
   1 COUNT +! ;

: EARLY-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n kind:n ord:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   serial drop
   kind EARLY <> if BAD then
   path pathu ROOT-REL {: rel:ptr relu:n :}
   ord NFILE-REF:REF-RECORD {: ref:n :}
   sym KEY$ {: pkg:ptr pkgu:n vis:n tail:ptr tailu:n :}
   relu 0= if
      EARLY ref pkg pkgu vis tail tailu NULL-PTR 0 name nameu
         0 0 PUT-ROW
   else
      EARLY ref pkg pkgu vis tail tailu rel relu name nameu
         at end PUT-ROW
   then ;

: FULL-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n kind:n xt:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   kind FULL <> if BAD then
   serial xt NFILE-REF:ORIGIN-RECORD? {: ord:n live:bool :}
   live 0= if exit then
   ord NFILE-REF:REF-RECORD {: ref:n :}
   sym KEY$ {: pkg:ptr pkgu:n vis:n tail:ptr tailu:n :}
   FULL ref pkg pkgu vis tail tailu path pathu name nameu
      at end PUT-ROW ;

: EXPORT-ROWS ( -- )
   \ Without the serial marker no FILE can distinguish its rows from the
   \ retained observer's earlier acceptances.
   s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT? 0= if exit then
   s" ENGINE-ORIGIN:FILE-ROWS" TARGET-XT? {: full:n :}
   full 0<> if
      MARK-SERIAL @ ['] FULL-ROW full ROWS-XT execute exit
   then
   s" ENGINE-ORIGIN-EARLY:FILE-ROWS" TARGET-XT? {: early:n :}
   early 0<> if MARK-SERIAL @ ['] EARLY-ROW early EARLY-ROWS-XT execute then ;

: ROW-ROOM ( n n -- n )
   {: size:n off:n :}
   size 0 < size MEM-MAX-N off - > or if MALFORMED then
   off size + ;

: ROW-END ( ptr u8 n n -- n )
   {: row:ptr size:n off:n :}
   size off - ROW-BYTES < if MALFORMED then
   off ROW-BYTES + {: next:n :}
   row 16 + LE:U64@ next ROW-ROOM
   row 32 + LE:U64@ swap ROW-ROOM
   row 40 + LE:U64@ swap ROW-ROOM
   row 48 + LE:U64@ swap ROW-ROOM
   dup size > if MALFORMED then ;

: CHECK-ROW ( ptr u8 -- )
   {: row:ptr :}
   row LE:U64@ {: kind:n :}
   row 16 + LE:U64@ {: pkgu:n :}
   row 24 + LE:U64@ {: vis:n :}
   row 32 + LE:U64@ {: tailu:n :}
   row 40 + LE:U64@ {: pathu:n :}
   row 48 + LE:U64@ {: nameu:n :}
   row 56 + LE:U64@ {: at:n :}
   row 64 + LE:U64@ {: end:n :}
   vis 0 < vis 2 > or if MALFORMED then
   tailu 0= if
      pkgu 0<> vis 0<> or if MALFORMED then
   else
      vis 0= if pkgu 0<> if MALFORMED then
      else pkgu 0= if MALFORMED then then
   then
   kind EARLY = if
      row 8 + LE:U64@ 0 <= if MALFORMED then
      at 0 < end at < or if MALFORMED then
   else
      kind FULL <> if MALFORMED then
      row 8 + LE:U64@ 0 <= if MALFORMED then
      at 0 < end at < or if MALFORMED then
   then
   nameu 0 <= if MALFORMED then
   pathu 0= if
      at 0<> end 0<> or if MALFORMED then
   else
      end at - nameu <> if MALFORMED then
   then
   pathu 0 > if
      row ROW-BYTES + pkgu + tailu + c@ 47 = if MALFORMED then
   then ;

: ROW-PATH$ ( ptr u8 -- ptr u8 n )
   {: row:ptr :}
   row ROW-BYTES + row 16 + LE:U64@ + row 32 + LE:U64@ +
      row 40 + LE:U64@ ;
: ROW-NAME$ ( ptr u8 -- ptr u8 n )
   {: row:ptr :}
   row ROW-PATH$ {: path:ptr pathu:n :}
   path pathu + row 48 + LE:U64@ ;
: ROW-PKG$ ( ptr u8 -- ptr u8 n )
   {: row:ptr :}
   row ROW-BYTES + row 16 + LE:U64@ ;
: ROW-TAIL$ ( ptr u8 -- ptr u8 n )
   {: row:ptr :}
   row ROW-PKG$ {: pkg:ptr pkgu:n :}
   pkg pkgu + row 32 + LE:U64@ ;

: CHECK-SYM ( n ptr u8 -- ) {: plan:n row:ptr :}
   row ROW-TAIL$ nip 0= if exit then
   plan row ROW-PKG$ row 24 + LE:U64@ row ROW-TAIL$
      CHECKER-OWNER-ABI:UNIT-SYM-OFF OWNER-FIELD
      UNIT-SYM-XT execute 0= if MALFORMED then ;

: NEED ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- bool ] -- )
   {: pkg:ptr pkgu:n name:ptr nameu:n provider :}
   pkg pkgu name nameu provider execute 0= if MALFORMED then ;

: ROW-SYM ( ptr u8 -- n )
   {: row:ptr :}
   row ROW-TAIL$ nip 0= if 0 exit then
   row ROW-PKG$ row 24 + LE:U64@ row ROW-TAIL$
      CHECKER-OWNER-ABI:ORIGIN-SYM-OFF OWNER-FIELD
      ORIGIN-SYM-XT execute dup 0= if BAD then ;

: FULL-INSTALL ( ptr u8 -- )
   {: row:ptr :}
   row ROW-SYM row 8 + LE:U64@
   row ROW-PATH$ row ROW-NAME$
   row 56 + LE:U64@ row 64 + LE:U64@
   s" ENGINE-ORIGIN:FILE-IMPORT" TARGET-XT FULL-IMPORT-XT execute ;

: EARLY-INSTALL ( ptr u8 -- )
   {: row:ptr :}
   row ROW-PATH$ {: rel:ptr relu:n :}
   relu 0= if NULL-PTR 0
   else SOURCE-ROOT:ENGINE$ rel relu PATH-BUF JOIN-PATH
      PATH-BUF swap then {: path:ptr pathu:n :}
   row 8 + LE:U64@ row ROW-SYM
   row 56 + LE:U64@ row 64 + LE:U64@
   path pathu row ROW-NAME$
   s" ENGINE-ORIGIN-EARLY:FILE-IMPORT" TARGET-XT
      EARLY-IMPORT-XT execute ;

: OWNER-HOOK ( -- n )
   s" ENGINE-ORIGIN:FILE-STATE" TARGET-XT? {: full:n :}
   full 0<> if
      full FULL-STATE-XT execute {: armed:bool hook:bool :}
      armed drop
      hook if FULL-HOOK exit then
   then
   s" ENGINE-ORIGIN-EARLY:FILE-HOOK?" TARGET-XT? {: early:n :}
   early 0<> if early HOOK-XT execute if EARLY-HOOK exit then then
   KEEP-HOOK ;

: STATE-HEADER ( -- )
   s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT? {: early:n :}
   early 0= if
      ABSENT 16 BYTES LE:U64!
      0 24 BYTES LE:U64!
      0 32 BYTES LE:U64!
      KEEP-HOOK 40 BYTES LE:U64!
      exit
   then
   early MARK-XT execute {: serial:n current:n armed:bool :}
   serial drop
   current 16 BYTES LE:U64!
   armed if 1 else 0 then 24 BYTES LE:U64!
   s" ENGINE-ORIGIN:FILE-STATE" TARGET-XT? {: full:n :}
   full 0<> if
      full FULL-STATE-XT execute {: armed:bool hook:bool :}
      hook drop
      armed if 1 else 0 then 32 BYTES LE:U64!
   else 0 32 BYTES LE:U64! then
   check@ MARK-HOOK @ = if KEEP-HOOK else OWNER-HOOK then
      40 BYTES LE:U64! ;

: CHECK-PATH ( ptr u8 -- )
   {: row:ptr :}
   row LE:U64@ EARLY <> if exit then
   row 40 + LE:U64@ {: relu:n :}
   relu 0= if exit then
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   rootu 0 < rootu FS-PATH-CAP > or if MALFORMED then
   rootu 0 > if
      root rootu 1- + c@ 47 = if 0 else 1 then
   else 1 then {: slash:n :}
   relu FS-PATH-CAP rootu - slash - > if MALFORMED then ;

public

: PENDING-AVAILABLE? ( -- bool )
   s" ENGINE-ORIGIN:ACCEPT-PENDING" PENDING-TARGET-XT? 0<>
   s" ENGINE-ORIGIN-EARLY:PREFIX-TAKE" PENDING-TARGET-XT? 0<> or ;

: ELIGIBLE? ( -- bool )
   \ The full journal and serial marker must belong to one compiled owner.
   s" ENGINE-ORIGIN:FILE-ROWS" TARGET-XT? {: full-rows:n :}
   full-rows 0<> if
      s" ENGINE-ORIGIN:FILE-BOUNDARY" TARGET-XT? {: boundary:n :}
      boundary 0= if false exit then
      boundary BOUNDARY-XT execute {: marker:n rows:n :}
      marker s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT? <>
      rows full-rows <> or if false exit then
   then
   \ A marker from one owner cannot bound another owner's row journal.
   MARK-EARLY-XT @ 0<> if
      s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT?
         MARK-EARLY-XT @ <> if false exit then
   else
      MARK-EARLY-ROWS @ 0<> MARK-FULL-ROWS @ 0<> or if
         false exit
      then
   then
   MARK-EARLY-ROWS @ 0<> if
      s" ENGINE-ORIGIN-EARLY:FILE-ROWS" TARGET-XT?
         MARK-EARLY-ROWS @ <> if false exit then
   then
   MARK-FULL-ROWS @ 0<> if
      s" ENGINE-ORIGIN:FILE-ROWS" TARGET-XT?
         MARK-FULL-ROWS @ <> if false exit then
   then
   \ A retained observer without FILE-MARK cannot bound its acceptance rows.
   s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT? 0= if
      s" ENGINE-ORIGIN:FILE-ROWS" TARGET-XT? 0<>
      s" ENGINE-ORIGIN-EARLY:FILE-ROWS" TARGET-XT? 0<> or if
         false exit
      then
   then
   \ A full provider exports its drained rows. A later early-hook acceptance
   \ remains in the early journal, so replay must take the source path.
   s" ENGINE-ORIGIN:FILE-STATE" TARGET-XT? 0<> if
      s" ENGINE-ORIGIN-EARLY:FILE-PENDING?" TARGET-XT?
         dup 0<> if HOOK-XT execute if false exit then
         else drop then
   then
   check@ MARK-HOOK @ = if true exit then
   OWNER-HOOK KEEP-HOOK <> ;

: BIND ( [ -- ptr u8 ] n -- )
   {: owner-query floor:n :}
   floor 0 < if BAD then
   owner-query OWNER-QUERY ! floor FLOOR ! ;

: MARK ( -- )
   0 MARK-SERIAL !
   check@ MARK-HOOK !
   s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT?
      dup MARK-EARLY-XT !
   s" ENGINE-ORIGIN-EARLY:FILE-ROWS" TARGET-XT?
      MARK-EARLY-ROWS !
   s" ENGINE-ORIGIN:FILE-ROWS" TARGET-XT?
      MARK-FULL-ROWS !
   dup 0= if drop exit then
   MARK-XT execute {: serial:n phase:n armed:bool :}
   phase 0 < phase 1 > or serial 0 < or if BAD then
   armed drop
   serial MARK-SERIAL ! ;

: EXPORT ( -- ptr u8 n )
   ELIGIBLE? 0= if BAD then
   0 USED ! 0 COUNT !
   HEADER-BYTES ROOM drop
   EXPORT-ROWS
   MAGIC 0 BYTES LE:U64!
   COUNT @ 8 BYTES LE:U64!
   STATE-HEADER
   0 BYTES USED @ ;

\ Resolve references in the prepared section while the reference table still
\ names this FILE. Another prepared child may replace that table before install.
: PREPARE ( ptr u8 n n [ ptr u8 n ptr u8 n -- bool ] -- )
   {: bytes:ptr size:n plan:n provider :}
   size HEADER-BYTES < if MALFORMED then
   bytes LE:U64@ MAGIC <> if MALFORMED then
   bytes 8 + LE:U64@ {: count:n :}
   count 0 < count size HEADER-BYTES - ROW-BYTES / > or if MALFORMED then
   bytes 16 + LE:U64@ {: phase:n :}
   bytes 24 + LE:U64@ {: early-armed:n :}
   bytes 32 + LE:U64@ {: full-armed:n :}
   bytes 40 + LE:U64@ {: hook:n :}
   phase 0 < phase ABSENT > or if MALFORMED then
   early-armed 0 < early-armed 1 > or if MALFORMED then
   full-armed 0 < full-armed 1 > or if MALFORMED then
   hook KEEP-HOOK < hook FULL-HOOK > or if MALFORMED then
   phase ABSENT = if
      early-armed 0<> full-armed 0<> or hook KEEP-HOOK <> or
         count 0<> or if MALFORMED then
   then
   phase EARLY = full-armed 0<> and if MALFORMED then
   phase FULL <> hook FULL-HOOK = and if MALFORMED then
   phase ABSENT <> if
      s" ENGINE-ORIGIN-EARLY" s" FILE-MARK" provider NEED
      s" ENGINE-ORIGIN-EARLY" s" FILE-RESTORE" provider NEED
   then
   phase FULL = if
      s" ENGINE-ORIGIN" s" FILE-DRAIN" provider NEED
      s" ENGINE-ORIGIN" s" FILE-RESTORE" provider NEED
   then
   hook EARLY-HOOK = if
      s" ENGINE-ORIGIN-EARLY" s" INSTALL" provider NEED
   then
   hook FULL-HOOK = if
      s" ENGINE-ORIGIN" s" INSTALL" provider NEED
   then
   HEADER-BYTES CURSOR !
   count 0 ?do
      bytes CURSOR @ + {: row:ptr :}
      row size CURSOR @ ROW-END CURSOR !
      row CHECK-ROW
      row CHECK-PATH
      plan row CHECK-SYM
      row LE:U64@ EARLY = if
         s" ENGINE-ORIGIN-EARLY" s" FILE-IMPORT" provider NEED
         row 8 + LE:U64@ NFILE-REF:REF>RECORD
      else
         s" ENGINE-ORIGIN" s" FILE-IMPORT" provider NEED
         row 8 + LE:U64@ NFILE-REF:REF>RECORD
      then row 8 + LE:U64!
   loop
   CURSOR @ size <> if MALFORMED then ;

: INSTALL ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size HEADER-BYTES < if MALFORMED then
   bytes 8 + LE:U64@ {: count:n :}
   bytes 16 + LE:U64@ {: phase:n :}
   phase ABSENT = if exit then
   s" ENGINE-ORIGIN-EARLY:FILE-MARK" TARGET-XT MARK-XT execute
      {: serial:n receiver:n armed:bool :}
   serial drop armed drop
   phase FULL = receiver EARLY = and if
      s" ENGINE-ORIGIN:FILE-DRAIN" TARGET-XT ACTION-XT execute
   then
   HEADER-BYTES CURSOR !
   count 0 ?do
      bytes CURSOR @ + {: row:ptr :}
      row size CURSOR @ ROW-END CURSOR !
      row LE:U64@ EARLY = if row EARLY-INSTALL
      else row FULL-INSTALL then
   loop
   phase FULL = if
      bytes 32 + LE:U64@ 0<>
      s" ENGINE-ORIGIN:FILE-RESTORE" TARGET-XT FULL-RESTORE-XT execute
   then
   phase bytes 24 + LE:U64@ 0<>
      s" ENGINE-ORIGIN-EARLY:FILE-RESTORE" TARGET-XT
      EARLY-RESTORE-XT execute
   bytes 40 + LE:U64@ dup EARLY-HOOK = if
      drop s" ENGINE-ORIGIN-EARLY:INSTALL" TARGET-XT ACTION-XT execute
   else FULL-HOOK = if
      s" ENGINE-ORIGIN:INSTALL" TARGET-XT ACTION-XT execute
   then then ;

: RESET ( -- )
   BYTES-RELEASE
   0 USED ! 0 COUNT ! 0 MARK-SERIAL ! 0 MARK-HOOK !
   0 MARK-EARLY-XT ! 0 MARK-EARLY-ROWS ! 0 MARK-FULL-ROWS !
   0 FLOOR ! 0 CURSOR !
   ['] NO-OWNER OWNER-QUERY ! ;

;package
