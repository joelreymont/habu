\ workspace.f - one transient address space for fixed compiler working
\ storage. Its dynamic control remains in DATA, so image capture releases the
\ mapping with every other dynamic allocation. Each accessor reads its DATA
\ descriptor, so FILE installation can place its allocation after the current
\ owner's declarations. A restored image opens and zeroes a fresh mapping on
\ first access.
require src/core/layout-buffer.f
require src/core/dynamic-storage.f
require src/lib/le.f
require src/lib/string.f

package CWORK
public
$100000 constant LIMIT
DYNAMIC-BUFFER MEM u8
private
variable USED
$100 constant DEFAULT-CAP
create DEFAULT-OFF DEFAULT-CAP cells allot
create DEFAULT-VALUE DEFAULT-CAP cells allot
variable DEFAULT-N

\ A declaration owns one aligned extent. FILE capture records only declarations
\ and DEFAULT operations in the active source range; child loads close a parent
\ range before opening their own. Dictionary ordinals are used only in this
\ process. The FILE bytes contain the reference module's publication keys.
5 constant ALLOC-CELLS
0 constant ALLOC-DECL
1 constant ALLOC-DESC
2 constant ALLOC-OFF
3 constant ALLOC-EXTENT
4 constant ALLOC-ALIGN
DYNAMIC-BUFFER ALLOC-ROW n
variable ALLOC-N

4 constant EVENT-CELLS
0 constant EVENT-KIND
1 constant EVENT-ALLOC
2 constant EVENT-OFF
3 constant EVENT-VALUE
0 constant ALLOC-EVENT
1 constant DEFAULT-EVENT
DYNAMIC-BUFFER EVENT-ROW n
variable EVENT-N
variable FILE-ACTIVE
variable FILE-FIRST
variable FILE-USED
variable FILE-DEFAULT-N

TYPED-VARIABLE KEY-XT [ n -- ptr u8 n ]
TYPED-VARIABLE RESOLVE-XT [ ptr u8 n -- n ]

\ Prepared sections are copied into one owner-scoped arena. A token is its
\ one-based byte offset; growth never invalidates a token.
DYNAMIC-BUFFER FILE-BUF u8
variable FILE-BYTES
DYNAMIC-BUFFER PREP-BUF u8
variable PREP-BYTES
3 constant PROJECT-CELLS
0 constant PROJECT-KEY
1 constant PROJECT-KEY-U
2 constant PROJECT-EXTENT
DYNAMIC-BUFFER PROJECT-ROW n
variable PROJECT-N
variable PROJECT-USED
variable PROJECT-DEFAULT-N
variable NEXT-INSTALL
variable CHECK-AT
variable CHECK-USED
variable INSTALL-AT

24 constant FILE-HEADER-U
CAST: DESCRIPTOR-XT ( n -- [ -- ptr n ] )

: ALLOC@ ( n n -- n ) {: row:n field:n :}
   row ALLOC-CELLS * field + ALLOC-ROW @ ;
: ALLOC! ( n n n -- ) {: value:n row:n field:n :}
   value row ALLOC-CELLS * field + ALLOC-ROW ! ;
: EVENT@ ( n n -- n ) {: row:n field:n :}
   row EVENT-CELLS * field + EVENT-ROW @ ;
: EVENT! ( n n n -- ) {: value:n row:n field:n :}
   value row EVENT-CELLS * field + EVENT-ROW ! ;

: APPEND-ALLOC ( n n n n -- n ) {: decl:n desc:n off:n extent:n :}
   ALLOC-N @ {: row:n :}
   row 1+ ALLOC-CELLS * ALLOC-ROW-RESERVE
   decl row ALLOC-DECL ALLOC!
   desc row ALLOC-DESC ALLOC!
   off row ALLOC-OFF ALLOC!
   extent row ALLOC-EXTENT ALLOC!
   CELL row ALLOC-ALIGN ALLOC!
   row 1+ ALLOC-N !
   row ;

: APPEND-EVENT ( n n n n -- ) {: kind:n alloc:n off:n value:n :}
   FILE-ACTIVE @ 0= if exit then
   EVENT-N @ {: row:n :}
   row 1+ EVENT-CELLS * EVENT-ROW-RESERVE
   kind row EVENT-KIND EVENT!
   alloc row EVENT-ALLOC EVENT!
   off row EVENT-OFF EVENT!
   value row EVENT-VALUE EVENT!
   row 1+ EVENT-N ! ;

: FIND-ALLOC ( n -- n ) {: off:n :}
   ALLOC-N @
   begin dup 0 > while
      1- dup ALLOC-OFF ALLOC@ {: base:n :}
      off base >= if
         off base - over ALLOC-EXTENT ALLOC@ CELL - <= if exit then
      then
   repeat
   drop E-LAYOUT-BUFFER throw ;

public
: DECLARED-BYTES ( -- n ) USED @ ;

\ Both parts only increase, and their sum is bounded by LIMIT + DEFAULT-CAP.
: LAYOUT-MARK ( -- n ) USED @ DEFAULT-N @ + ;

: TAKE ( n -- n ) {: bytes:n :}
   bytes 0 <= if E-LAYOUT-BUFFER throw then
   USED @ CELL 1- + CELL / CELL * {: off:n :}
   off LIMIT > if E-LAYOUT-BUFFER throw then
   bytes LIMIT off - > if E-LAYOUT-BUFFER throw then
   off bytes + USED !
   off ;

: DEFAULT ( n n -- ) {: off:n value:n :}
   DEFAULT-N @ DEFAULT-CAP >= if E-LAYOUT-BUFFER throw then
   FILE-ACTIVE @ if
      off FIND-ALLOC {: row:n :}
      DEFAULT-EVENT row off row ALLOC-OFF ALLOC@ - value APPEND-EVENT
   then
   off DEFAULT-OFF DEFAULT-N @ cells + !
   value DEFAULT-VALUE DEFAULT-N @ cells + !
   DEFAULT-N @ 1+ DEFAULT-N !
   MEM#base @ NULL-PTR <> if value MEM#base @ off + ! then ;

: OPEN ( -- )
   MEM#base @ NULL-PTR <> if exit then
   LIMIT MEM-RESERVE
   MEM#base @ {: base:ptr :}
   DEFAULT-N @ 0 ?do
      i cells DEFAULT-OFF + @ base +
      i cells DEFAULT-VALUE + @ swap !
   loop ;

\ The native FILE loader supplies publication identity without making this
\ baked compiler module depend on a build tool.
: FILE-IDENTITY-SET ( [ n -- ptr u8 n ] [ ptr u8 n -- n ] -- )
   RESOLVE-XT ! KEY-XT ! ;

: FILE-MARK ( -- )
   FILE-ACTIVE @ if E-LAYOUT-BUFFER throw then
   EVENT-N @ FILE-FIRST !
   USED @ FILE-USED !
   DEFAULT-N @ FILE-DEFAULT-N !
   1 FILE-ACTIVE ! ;

private

: FILE-BAD ( -- ) E-LAYOUT-BUFFER throw ;
: NO-KEY ( n -- ptr u8 n ) drop FILE-BAD ;
: NO-RESOLVE ( ptr u8 n -- n ) 2drop FILE-BAD ;
: KEY$ ( n -- ptr u8 n ) KEY-XT @ execute ;
: RESOLVE ( ptr u8 n -- n ) RESOLVE-XT @ execute ;

: FILE-ROOM ( n -- ptr u8 ) {: count:n :}
   count 0 < if FILE-BAD then
   FILE-BYTES @ {: at:n :}
   count $7FFFFFFFFFFFFFFF at - > if FILE-BAD then
   at count + FILE-BUF-RESERVE
   at count + FILE-BYTES !
   at FILE-BUF ;

: FILE-U32, ( n -- ) {: value:n :}
   value 0 < value $FFFFFFFF > or if FILE-BAD then
   value 4 FILE-ROOM LE:U32! ;
: FILE-U64, ( n -- ) 8 FILE-ROOM LE:U64! ;
: FILE-KEY, ( n -- )
   KEY$ {: key:ptr keyu:n :}
   keyu 0 <= if FILE-BAD then
   keyu FILE-U32,
   keyu FILE-ROOM key swap keyu BYTE-COPY ;

: EVENT-COUNT ( n -- n ) {: kind:n :}
   0 EVENT-N @ FILE-FIRST @ ?do
      i EVENT-KIND EVENT@ kind = if 1+ then
   loop ;

: EXPORT-ALLOC ( n -- ) {: event:n :}
   event EVENT-ALLOC EVENT@ {: row:n :}
   row ALLOC-DECL ALLOC@ FILE-KEY,
   row ALLOC-DESC ALLOC@ FILE-KEY,
   row ALLOC-EXTENT ALLOC@ FILE-U64,
   row ALLOC-ALIGN ALLOC@ FILE-U64, ;

: EXPORT-DEFAULT ( n -- ) {: event:n :}
   event EVENT-ALLOC EVENT@ ALLOC-DECL ALLOC@ FILE-KEY,
   event EVENT-OFF EVENT@ FILE-U64,
   event EVENT-VALUE EVENT@ FILE-U64, ;

: CHECK-CAPTURE ( -- )
   FILE-USED @ CHECK-USED !
   EVENT-N @ FILE-FIRST @ ?do
      i EVENT-KIND EVENT@ ALLOC-EVENT = if
         i EVENT-ALLOC EVENT@ {: row:n :}
         CHECK-USED @ CELL 1- + CELL / CELL *
            row ALLOC-OFF ALLOC@ <> if FILE-BAD then
         row ALLOC-EXTENT ALLOC@ {: extent:n :}
         extent 0 <= extent LIMIT row ALLOC-OFF ALLOC@ - > or if FILE-BAD then
         row ALLOC-OFF ALLOC@ extent + CHECK-USED !
      then
   loop
   CHECK-USED @ USED @ <> if FILE-BAD then
   DEFAULT-N @ FILE-DEFAULT-N @ -
      DEFAULT-EVENT EVENT-COUNT <> if FILE-BAD then ;

public

: FILE-EXPORT ( -- ptr u8 n )
   FILE-ACTIVE @ 0= if FILE-BAD then
   0 FILE-ACTIVE !
   CHECK-CAPTURE
   0 FILE-BYTES !
   8 FILE-ROOM s" HCWORK2!" drop swap 8 BYTE-COPY
   ALLOC-EVENT EVENT-COUNT FILE-U64,
   DEFAULT-EVENT EVENT-COUNT FILE-U64,
   EVENT-N @ FILE-FIRST @ ?do
      i EVENT-KIND EVENT@ ALLOC-EVENT = if i EXPORT-ALLOC then
   loop
   EVENT-N @ FILE-FIRST @ ?do
      i EVENT-KIND EVENT@ DEFAULT-EVENT = if i EXPORT-DEFAULT then
   loop
   0 FILE-BUF FILE-BYTES @ ;

private

: FILE-FIELD ( n n n -- ) {: size:n at:n width:n :}
   at 0 < at size > or if FILE-BAD then
   width size at - > if FILE-BAD then ;
: FILE-U32@ ( ptr u8 n n -- n )
   {: bytes:ptr size:n at:n :}
   size at 4 FILE-FIELD bytes at + LE:U32@ ;
: FILE-U64@ ( ptr u8 n n -- n )
   {: bytes:ptr size:n at:n :}
   size at 8 FILE-FIELD bytes at + LE:U64@ ;

: FILE-KEY@ ( ptr u8 n n -- ptr u8 n n )
   {: bytes:ptr size:n at:n :}
   bytes size at FILE-U32@ {: keyu:n :}
   keyu 0 <= if FILE-BAD then
   size at 4 + keyu FILE-FIELD
   bytes at 4 + + keyu at 4 + keyu + ;

: FILE-ALLOC@ ( ptr u8 n n -- ptr u8 n ptr u8 n n n n )
   {: bytes:ptr size:n at:n :}
   bytes size at FILE-KEY@ {: key:ptr keyu:n next:n :}
   bytes size next FILE-KEY@ {: desc:ptr descu:n fields:n :}
   size fields 16 FILE-FIELD
   key keyu desc descu
   bytes size fields FILE-U64@
   bytes size fields 8 + FILE-U64@
   fields 16 + ;

: FILE-DEFAULT@ ( ptr u8 n n -- ptr u8 n n n n )
   {: bytes:ptr size:n at:n :}
   bytes size at FILE-KEY@ {: key:ptr keyu:n next:n :}
   size next 16 FILE-FIELD
   key keyu
   bytes size next FILE-U64@
   bytes size next 8 + FILE-U64@
   next 16 + ;

: CHECK-FILE ( ptr u8 n -- ) {: bytes:ptr size:n :}
   size FILE-HEADER-U < if FILE-BAD then
   bytes 8 s" HCWORK2!" STR= 0= if FILE-BAD then
   bytes size 8 FILE-U64@ {: allocs:n :}
   bytes size 16 FILE-U64@ {: writes:n :}
   allocs 0 < allocs LIMIT CELL / > or
   writes 0 < writes DEFAULT-CAP > or or if FILE-BAD then
   FILE-HEADER-U CHECK-AT !
   allocs 0 ?do
      bytes size CHECK-AT @ FILE-ALLOC@
         {: key:ptr keyu:n desc:ptr descu:n extent:n align:n next:n :}
      key drop keyu drop desc drop descu drop
      align CELL <> extent 0 <= or if FILE-BAD then
      extent LIMIT > if FILE-BAD then
      next CHECK-AT !
   loop
   writes 0 ?do
      bytes size CHECK-AT @ FILE-DEFAULT@
         {: key:ptr keyu:n off:n value:n next:n :}
      key drop keyu drop value drop
      off 0 < off CELL mod 0<> or if FILE-BAD then
      next CHECK-AT !
   loop
   CHECK-AT @ size <> if FILE-BAD then ;

: PREP-SECTION ( n -- ptr u8 n ) {: token:n :}
   token 0 <= token PREP-BYTES @ > or if FILE-BAD then
   token 1- {: at:n :}
   8 PREP-BYTES @ at - > if FILE-BAD then
   at PREP-BUF LE:U64@ {: size:n :}
   size 0 < size PREP-BYTES @ at 8 + - > or if FILE-BAD then
   at 8 + PREP-BUF size ;

: ALLOC-BY-DECL ( n -- n ) {: decl:n :}
   ALLOC-N @
   begin dup 0 > while
      1- dup ALLOC-DECL ALLOC@ decl = if exit then
   repeat
   drop FILE-BAD ;

\ The factory records both the VARIABLE and accessor identities; the imported
\ records must preserve that immediate relation. The VARIABLE cell is ordinary
\ relocated DATA, and the accessor reads it on every call.
: DESCRIPTOR ( n n -- ptr n ) {: decl:n desc:n :}
   desc 0 < decl ndict@ >= or desc 1+ decl <> or if FILE-BAD then
   desc XREF-REC {: rec:ptr :}
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR <> if FILE-BAD then
   rec XREF-FLAGS DNAME-ALLOC and 0= if FILE-BAD then
   rec XREF-FLAGS DNAME-CELL and 0= if FILE-BAD then
   rec XREF-START DESCRIPTOR-XT execute ;

: PROJECT@ ( n n -- n ) {: row:n field:n :}
   row PROJECT-CELLS * field + PROJECT-ROW @ ;
: PROJECT! ( n n n -- ) {: value:n row:n field:n :}
   value row PROJECT-CELLS * field + PROJECT-ROW ! ;

: PROJECT-EXTENT? ( ptr u8 n -- n bool ) {: key:ptr keyu:n :}
   PROJECT-N @
   begin dup 0 > while
      1- dup {: row:n :}
      row PROJECT-KEY PROJECT@ PREP-BUF
         row PROJECT-KEY-U PROJECT@ key keyu STR= if
         drop row PROJECT-EXTENT PROJECT@ true exit
      then
   repeat drop
   ALLOC-N @
   begin dup 0 > while
      1- dup ALLOC-DECL ALLOC@ KEY$
         key keyu STR= if
         dup ALLOC-EXTENT ALLOC@ swap drop true exit
      then
   repeat drop
   0 false ;

: PROJECT-ALLOC ( ptr u8 n n -- )
   {: key:ptr keyu:n extent:n :}
   key keyu PROJECT-EXTENT? if drop FILE-BAD then drop
   PROJECT-USED @ CELL 1- + CELL / CELL * {: off:n :}
   extent LIMIT off - > if FILE-BAD then
   PROJECT-N @ {: row:n :}
   row 1+ PROJECT-CELLS * PROJECT-ROW-RESERVE
   key BYTE-VIEW 0 PREP-BUF BYTE-VIEW - row PROJECT-KEY PROJECT!
   keyu row PROJECT-KEY-U PROJECT!
   extent row PROJECT-EXTENT PROJECT!
   row 1+ PROJECT-N !
   off extent + PROJECT-USED ! ;

: PROJECT-SECTION ( ptr u8 n -- ) {: bytes:ptr size:n :}
   bytes size 8 FILE-U64@ {: allocs:n :}
   bytes size 16 FILE-U64@ {: writes:n :}
   writes DEFAULT-CAP PROJECT-DEFAULT-N @ - > if FILE-BAD then
   FILE-HEADER-U CHECK-AT !
   allocs 0 ?do
      bytes size CHECK-AT @ FILE-ALLOC@
         {: key:ptr keyu:n desc:ptr descu:n extent:n align:n next:n :}
      desc drop descu drop align drop
      key keyu extent PROJECT-ALLOC
      next CHECK-AT !
   loop
   writes 0 ?do
      bytes size CHECK-AT @ FILE-DEFAULT@
         {: key:ptr keyu:n off:n value:n next:n :}
      value drop
      key keyu PROJECT-EXTENT?
      0= if drop FILE-BAD then
      CELL - off < if FILE-BAD then
      next CHECK-AT !
   loop
   PROJECT-DEFAULT-N @ writes + PROJECT-DEFAULT-N ! ;

public

: FILE-PLAN-RESET ( -- )
   0 PREP-BYTES !  0 PROJECT-N !
   USED @ PROJECT-USED !
   DEFAULT-N @ PROJECT-DEFAULT-N !
   1 NEXT-INSTALL ! ;

: FILE-PREPARE ( ptr u8 n -- n ) {: bytes:ptr size:n :}
   bytes size CHECK-FILE
   PREP-BYTES @ {: at:n :}
   size $7FFFFFFFFFFFFFFF at 8 + - > if FILE-BAD then
   at size 8 + + PREP-BUF-RESERVE
   size at PREP-BUF LE:U64!
   bytes at 8 + PREP-BUF size BYTE-COPY
   at size 8 + + PREP-BYTES !
   at 8 + PREP-BUF size PROJECT-SECTION
   at 1+ ;

: FILE-INSTALL ( n -- )
   dup NEXT-INSTALL @ <> if FILE-BAD then
   PREP-SECTION {: bytes:ptr size:n :}
   bytes size 8 FILE-U64@ {: allocs:n :}
   bytes size 16 FILE-U64@ {: writes:n :}
   writes DEFAULT-CAP DEFAULT-N @ - > if FILE-BAD then
   FILE-HEADER-U INSTALL-AT !
   allocs 0 ?do
      bytes size INSTALL-AT @ FILE-ALLOC@
         {: key:ptr keyu:n desc:ptr descu:n extent:n align:n next:n :}
      align drop
      extent TAKE {: off:n :}
      key keyu RESOLVE {: decl:n :}
      desc descu RESOLVE {: owner:n :}
      decl owner DESCRIPTOR off swap !
      decl owner off extent APPEND-ALLOC drop
      next INSTALL-AT !
   loop
   writes 0 ?do
      bytes size INSTALL-AT @ FILE-DEFAULT@
         {: key:ptr keyu:n off:n value:n next:n :}
      key keyu RESOLVE ALLOC-BY-DECL {: row:n :}
      off row ALLOC-EXTENT ALLOC@ CELL - > if FILE-BAD then
      row ALLOC-OFF ALLOC@ off + value DEFAULT
      next INSTALL-AT !
   loop
   NEXT-INSTALL @ size 8 + + NEXT-INSTALL ! ;

: FILE-CLOSE ( -- )
   0 FILE-ACTIVE !  0 EVENT-N !  0 ALLOC-N !
   0 FILE-BYTES !  0 PREP-BYTES !  0 PROJECT-N !  0 NEXT-INSTALL !
   ALLOC-ROW-RELEASE EVENT-ROW-RELEASE
   FILE-BUF-RELEASE PREP-BUF-RELEASE PROJECT-ROW-RELEASE
   ['] NO-KEY KEY-XT !  ['] NO-RESOLVE RESOLVE-XT ! ;

private
0 constant BUFFER-KIND
1 constant SPAN-KIND
2 constant VARIABLE-KIND

\ This closed storage introducer owns the prehook parser and LBUF mint
\ boundary. Runtime allocation stays checked; LBUF-EVAL! checks the generated
\ accessor under the existing one-shot storage window.
TRUSTED: DECLARE ( n n -- ) {: operand:n kind:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if
      kind BUFFER-KIND = if s" CWORK-BUFFER" else
         kind SPAN-KIND = if s" CWORK-SPAN" else s" CWORK-VARIABLE" then
      then CHECKER-STORAGE-NAME-REFUSE exit
   then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAMES-OK? 0= if exit then
   kind VARIABLE-KIND = if
      1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
      LBUF-BYTES @
   else
      type typeu CHECKER-DYNAMIC-INFO 0= if
         drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
      then
      LBUF-W !
      operand LBUF-N !
      operand 0 <= LBUF-W @ 0 <= or if E-LAYOUT-BUFFER throw then
      operand LIMIT LBUF-W @ / > if E-LAYOUT-BUFFER throw then
      operand LBUF-W @ *
   then {: bytes:n :}
   bytes TAKE {: off:n :}
   LBUF-ALIGN
   here {: base:ptr :}
   ndict@ {: desc:n :}
   LBUF-CLEAR
   s" variable " LBUF-APP name nameu LBUF-BASE,
   s"  : " LBUF-APP name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   kind BUFFER-KIND = if
      s"  ( n -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
      LBUF-N @ LBUF-DEC,
      s"  DYNAMIC-STORAGE:BOUNDS " LBUF-APP
   else
      s"  ( -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
   then
   s" CWORK:OPEN CWORK:MEM#base @ " LBUF-APP
   name nameu LBUF-BASE, s"  @ + " LBUF-APP
   kind BUFFER-KIND = if
      s" swap " LBUF-APP LBUF-W @ LBUF-DEC, s"  * + " LBUF-APP
   then
   s" ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu LBUF-EVAL! drop
   here base CELL + <> if E-LAYOUT-BUFFER throw then
   off base !
   ndict@ 1- {: decl:n :}
   desc 1+ decl <> if E-LAYOUT-BUFFER throw then
   decl desc off bytes APPEND-ALLOC
      ALLOC-EVENT swap 0 0 APPEND-EVENT
   kind VARIABLE-KIND = operand 0<> and if off operand DEFAULT then ;

\ Global introduction forms keep their existing checker identities. Their
\ compiled calls retain these closed bridges after the names are retired.
public
: BUFFER ( n -- ) BUFFER-KIND DECLARE ;
: SPAN ( n -- ) SPAN-KIND DECLARE ;
: VARIABLE ( n -- ) VARIABLE-KIND DECLARE ;
;package

: CWORK-BUFFER ( n -- ) CWORK:BUFFER ;
: CWORK-SPAN ( n -- ) CWORK:SPAN ;
: CWORK-VARIABLE ( n -- ) CWORK:VARIABLE ;

undefine CWORK:BUFFER
undefine CWORK:SPAN
undefine CWORK:VARIABLE
