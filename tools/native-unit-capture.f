\ Capture one package's native code and publication sites while the retained
\ compiler produces it. The observer copies the owned emission rows before
\ retires them; final code and records are read only after publication ends.

require src/lib/errors.f
require src/compiler/native/publish.f
require src/compiler/session/emission.f
require src/compiler/native/string.f
require src/habu/xref.f
require src/habu/aot-arm.f

package NUNIT-CAPTURE

private

3 constant CALL-CELLS
2 constant ADDR-CELLS

DYNAMIC-BUFFER CALL-ROW n
DYNAMIC-BUFFER ADDR-ROW n
variable CALL-N
variable ADDR-N
variable CODE-START
variable CODE-END
variable REC-START
variable REC-END
variable WID-START
variable WID-END
variable DATA-START-N
variable DATA-END-N
variable PREFIX-DATA-N
variable PREFIX-READY
DYNAMIC-BUFFER PREFIX-BEFORE u8
variable PREFIX-BYTES
variable PREFIX-OFF
variable PREFIX-TOP
\ A later FILE may retire a row captured by an earlier FILE. Preserve the
\ prefix wordlist cells before loading this unit, alongside its byte prefix.
DYNAMIC-BUFFER PREFIX-WIDS n
variable PREFIX-WID-OFF
variable PREFIX-WID-TOP
\ Explicitly owned preloaded DATA is separate from the ordinary HERE prefix.
\ Rows hold DATA address, byte count and offset into the before-byte arena.
3 constant OWNED-CELLS
DYNAMIC-BUFFER OWNED-ROW n
DYNAMIC-BUFFER OWNED-BEFORE u8
variable OWNED-N
variable OWNED-FIRST
variable OWNED-TOP
variable SEMANTIC-ON
variable LITERAL-ROWS-N
variable LITERAL-BYTES-N
variable LITERAL-SEGMENTS-N
variable REQUIRE-FIRST
variable REQUIRE-END-N
variable ARMED
variable ACTIVE
variable FINISHED
variable CALL-FIRST
variable ADDR-FIRST
25 constant FRAME-CELLS
DYNAMIC-BUFFER FRAME n
variable FRAME-DEPTH
TYPED-VARIABLE UNIT-BODY [ -- ]

\ The capture records code and record addresses as integers.
CAST: CODE-BYTES ( n -- ptr u8 )
CAST: DATA-BYTES ( n -- ptr u8 )
CAST: NUMBER-XT ( n -- [ -- n ] )
CAST: WITH-UNIT-XT ( n -- [ [ n n n n -- ] [ -- ] -- ] )
CAST: ONE-XT ( n -- [ n -- n ] )
CAST: TWO-XT ( n -- [ n n -- n ] )
CAST: OWNED-XT ( n -- [ [ ptr u8 n -- ] [ ptr u8 n -- ] -- ] )
CAST: DECL-OWNED-XT ( n -- [ [ ptr u8 n -- ] -- ] )
CAST: ACTION-XT ( n -- [ -- ] )

: TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop E-NUNIT-PROFILE throw
   then
   XREF-START dup AOT-ARM:B0 @ < if E-NUNIT-PROFILE throw then ;

\ Optional runtime owners exist only after their support has been loaded.
: RUNTIME-OWNER-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   XREF-START dup AOT-ARM:B0 @ < if E-NUNIT-PROFILE throw then ;

: TARGET-ONE ( n ptr u8 n -- n ) {: x:n a:ptr u:n :}
   a u TARGET-XT ONE-XT x swap execute ;
: TARGET-TWO ( n n ptr u8 n -- n ) {: x:n i:n a:ptr u:n :}
   a u TARGET-XT TWO-XT x i rot execute ;

: TARGET-NUMBER ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop E-NUNIT-PROFILE throw
   then
   XREF-START dup AOT-ARM:B0 @ < if E-NUNIT-PROFILE throw then
   NUMBER-XT execute ;

: LITERAL-COUNT ( -- n ) s" NSTR:COUNT" TARGET-NUMBER ;
: LITERAL-BYTES ( -- n ) s" NSTR:BYTES" TARGET-NUMBER ;
: LITERAL-SEGMENTS ( -- n ) s" NSTR:SEGMENTS" TARGET-NUMBER ;
: REQUIRE-COUNT ( -- n ) s" REQUIRE-REG:COUNT" TARGET-NUMBER ;

: SNAP-PREFIX ( -- )
   DATA-START-N @ PREFIX-DATA-N @ - {: bytes:n :}
   bytes 0 < if E-NUNIT-PROFILE throw then
   bytes 0= if 0 PREFIX-BYTES ! exit then
   PREFIX-TOP @ bytes + PREFIX-BEFORE-RESERVE
   PREFIX-TOP @ PREFIX-OFF !
   PREFIX-DATA-N @ DATA-BYTES PREFIX-TOP @ PREFIX-BEFORE bytes BYTE-COPY
   bytes PREFIX-TOP +!
   bytes PREFIX-BYTES ! ;

: SNAP-PREFIX-WIDS ( -- )
   REC-START @ {: rows:n :}
   PREFIX-WID-TOP @ rows + PREFIX-WIDS-RESERVE
   PREFIX-WID-TOP @ PREFIX-WID-OFF !
   rows 0 ?do
      i XREF-REC XREF-WORDLIST
         PREFIX-WID-OFF @ i + PREFIX-WIDS !
   loop
   rows PREFIX-WID-TOP +! ;

: OWNED! ( n n n n -- ) {: address:n bytes:n off:n row:n :}
   address row OWNED-CELLS * OWNED-ROW !
   bytes row OWNED-CELLS * 1+ OWNED-ROW !
   off row OWNED-CELLS * 2 + OWNED-ROW ! ;

: KEEP-OWNER ( ptr u8 n bool -- )
   {: source:ptr bytes:n snapshot:bool :}
   bytes 0 <= if exit then
   source BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + {: address:n :}
   address DATA-VA VA>N < address AOT-ARM:HERE-N > or if E-NUNIT-PROFILE throw then
   bytes AOT-ARM:HERE-N address - > if E-NUNIT-PROFILE throw then
   OWNED-N @ 1+ OWNED-CELLS * OWNED-ROW-RESERVE
   snapshot if
      OWNED-TOP @ bytes + dup OWNED-TOP @ < if drop E-NUNIT-PROFILE throw then
         OWNED-BEFORE-RESERVE
      address bytes OWNED-TOP @ OWNED-N @ OWNED!
      source OWNED-TOP @ OWNED-BEFORE bytes BYTE-COPY
      bytes OWNED-TOP +!
   else
      address bytes 0 OWNED-N @ OWNED!
   then
   1 OWNED-N +! ;

: KEEP-DATA ( ptr u8 n -- ) true KEEP-OWNER ;
: KEEP-LIVE ( ptr u8 n -- ) false KEEP-OWNER ;

: IGNORE-DATA ( ptr u8 n -- ) 2drop ;

: CALLBACK-OWNERS ( [ ptr u8 n -- ] -- )
   {: keep :}
   s" FFI-CB:OWNED-CELLS" RUNTIME-OWNER-XT dup 0<> if
      DECL-OWNED-XT keep swap execute
   else drop then
   s" TASK:OWNED-CALLBACK-ROWS" RUNTIME-OWNER-XT dup 0<> if
      DECL-OWNED-XT keep swap execute
   else drop then ;

\ TASK declarations change baked DATA below the ordinary FILE prefix. Process
\ scratch is fresh state, so only the runtime's carried declarations are kept.
: TASK-OWNERS ( [ ptr u8 n -- ] -- )
   {: keep :}
   s" TASK:OWNED-CELLS" RUNTIME-OWNER-XT dup 0<> if
      OWNED-XT keep ['] IGNORE-DATA rot execute
   else drop then ;

: SNAP-OWNERS ( -- )
   s" FFI:OWNED-CELLS" TARGET-XT OWNED-XT
      ['] KEEP-DATA ['] IGNORE-DATA rot execute
   s" FFI-DECL:OWNED-CELLS" TARGET-XT DECL-OWNED-XT
      ['] KEEP-DATA swap execute
   ['] KEEP-DATA CALLBACK-OWNERS
   ['] KEEP-DATA TASK-OWNERS ;

: REFRESH-LIVE ( -- )
   0 OWNED-N ! 0 OWNED-FIRST ! 0 OWNED-TOP !
   s" FFI:OWNED-CELLS" TARGET-XT OWNED-XT
      ['] KEEP-LIVE ['] IGNORE-DATA rot execute
   s" FFI-DECL:OWNED-CELLS" TARGET-XT DECL-OWNED-XT
      ['] KEEP-LIVE swap execute
   ['] KEEP-LIVE CALLBACK-OWNERS
   ['] KEEP-LIVE TASK-OWNERS ;

: ARM-OWNERS ( -- )
   s" FFI-DECL:ARM-SCOPE" TARGET-XT ACTION-XT execute ;

: ARM-IMPORTED-CALLBACKS ( -- )
   s" TASK:ARM-IMPORTED-CALLBACKS" RUNTIME-OWNER-XT dup 0<> if
      ACTION-XT execute
   else drop then ;

: FORGET-FFI-CACHE ( -- )
   s" FFI:FORGET-CACHE" TARGET-XT ACTION-XT execute ;

: CALL! ( n n n n -- ) {: site:n kind:n target:n row:n :}
   site row CALL-CELLS * CALL-ROW !
   kind row CALL-CELLS * 1+ CALL-ROW !
   target row CALL-CELLS * 2 + CALL-ROW ! ;

: ADDR! ( n n n -- ) {: site:n kind:n row:n :}
   site row ADDR-CELLS * ADDR-ROW !
   kind row ADDR-CELLS * 1+ ADDR-ROW ! ;

: OBSERVE ( n n n n -- )
   {: e:n idx:n fn:n size:n :}
   ACTIVE @ 0= if exit then
   fn CODE-START @ < if E-NUNIT-PROFILE throw then
   e s" NART:CALL-SITES" TARGET-ONE 0 ?do
      CALL-N @ {: row:n :}
      row 1+ CALL-CELLS * CALL-ROW-RESERVE
      fn e i s" NART:CALL-SITE@" TARGET-TWO +
      e i s" NART:CALL-KIND@" TARGET-TWO
      e i s" NART:CALL-TARGET@" TARGET-TWO
      row CALL!
      row 1+ CALL-N !
   loop
   e s" NART:ADDR-SITES" TARGET-ONE 0 ?do
      ADDR-N @ {: row:n :}
      row 1+ ADDR-CELLS * ADDR-ROW-RESERVE
      fn e i s" NART:ADDR-SITE@" TARGET-TWO +
      e i s" NART:ADDR-SITE-KIND@" TARGET-TWO
      row ADDR!
      row 1+ ADDR-N !
   loop ;

: START ( -- )
   ARMED @ 0= ACTIVE @ 0<> or if E-NUNIT-PROFILE throw then
   SEMANTIC-ON @ if FORGET-FFI-CACHE then
   PREFIX-READY @ if
      AOT-ARM:HERE-N DATA-START-N !
      SNAP-PREFIX
   else
      AOT-ARM:HERE-N DATA-START-N !
      0 PREFIX-BYTES !
   then
   cp@ CODE-START !
   ndict@ REC-START !
   SNAP-PREFIX-WIDS
   AOT-ARM:WIDN WID-START !
   LITERAL-COUNT LITERAL-ROWS-N !
   LITERAL-BYTES LITERAL-BYTES-N !
   LITERAL-SEGMENTS LITERAL-SEGMENTS-N !
   REQUIRE-COUNT REQUIRE-FIRST !
   REQUIRE-FIRST @ REQUIRE-END-N !
   CALL-N @ CALL-FIRST ! ADDR-N @ ADDR-FIRST !
   OWNED-N @ OWNED-FIRST !
   SEMANTIC-ON @ if SNAP-OWNERS then
   0 FINISHED !
   1 ACTIVE ! ;

: FRAME! ( n n n -- ) {: x:n row:n field:n :}
   x row FRAME-CELLS * field + FRAME ! ;
: FRAME@ ( n n -- n ) {: row:n field:n :}
   row FRAME-CELLS * field + FRAME @ ;

: PUSH-FRAME ( -- )
   FRAME-DEPTH @ {: row:n :}
   row 1+ FRAME-CELLS * FRAME-RESERVE
   CODE-START @ row 0 FRAME!  CODE-END @ row 1 FRAME!
   REC-START @ row 2 FRAME!   REC-END @ row 3 FRAME!
   WID-START @ row 4 FRAME!   WID-END @ row 5 FRAME!
   DATA-START-N @ row 6 FRAME! DATA-END-N @ row 7 FRAME!
   PREFIX-BYTES @ row 8 FRAME! PREFIX-OFF @ row 9 FRAME!
   PREFIX-TOP @ row 10 FRAME!
   LITERAL-ROWS-N @ row 11 FRAME! LITERAL-BYTES-N @ row 12 FRAME!
   LITERAL-SEGMENTS-N @ row 13 FRAME! REQUIRE-FIRST @ row 14 FRAME!
   ACTIVE @ row 15 FRAME! FINISHED @ row 16 FRAME!
   CALL-FIRST @ row 17 FRAME! ADDR-FIRST @ row 18 FRAME!
   OWNED-FIRST @ row 19 FRAME! OWNED-N @ row 20 FRAME!
   OWNED-TOP @ row 21 FRAME!
   REQUIRE-END-N @ row 22 FRAME!
   PREFIX-WID-OFF @ row 23 FRAME!
   PREFIX-WID-TOP @ row 24 FRAME!
   1 FRAME-DEPTH +! ;

: POP-FRAME ( -- )
   FRAME-DEPTH @ 0 <= if E-NUNIT-PROFILE throw then
   -1 FRAME-DEPTH +! FRAME-DEPTH @ {: row:n :}
   row 0 FRAME@ CODE-START ! row 1 FRAME@ CODE-END !
   row 2 FRAME@ REC-START ! row 3 FRAME@ REC-END !
   row 4 FRAME@ WID-START ! row 5 FRAME@ WID-END !
   row 6 FRAME@ DATA-START-N ! row 7 FRAME@ DATA-END-N !
   row 8 FRAME@ PREFIX-BYTES ! row 9 FRAME@ PREFIX-OFF !
   row 10 FRAME@ PREFIX-TOP !
   row 11 FRAME@ LITERAL-ROWS-N ! row 12 FRAME@ LITERAL-BYTES-N !
   row 13 FRAME@ LITERAL-SEGMENTS-N ! row 14 FRAME@ REQUIRE-FIRST !
   row 15 FRAME@ ACTIVE ! row 16 FRAME@ FINISHED !
   row 17 FRAME@ CALL-FIRST ! row 18 FRAME@ ADDR-FIRST !
   row 19 FRAME@ OWNED-FIRST ! row 20 FRAME@ OWNED-N !
   row 21 FRAME@ OWNED-TOP !
   row 22 FRAME@ REQUIRE-END-N !
   row 23 FRAME@ PREFIX-WID-OFF !
   row 24 FRAME@ PREFIX-WID-TOP ! ;

: FINISH ( -- )
   ACTIVE @ 0= if E-NUNIT-PROFILE throw then
   cp@ CODE-END !
   ndict@ REC-END !
   AOT-ARM:WIDN WID-END !
   CODE-END @ CODE-START @ < if E-NUNIT-PROFILE throw then
   REC-END @ REC-START @ < if E-NUNIT-PROFILE throw then
   AOT-ARM:HERE-N DATA-END-N !
   DATA-END-N @ DATA-START-N @ < if E-NUNIT-PROFILE throw then
   REQUIRE-COUNT REQUIRE-END-N !
   0 ACTIVE !
   1 FINISHED ! ;

: READY ( -- )
   FINISHED @ 0= if E-NUNIT-PROFILE throw then ;

public

EXPORT ARM-IMPORTED-CALLBACKS

: START-PREFIX ( -- )
   0 OWNED-N ! 0 OWNED-FIRST ! 0 OWNED-TOP ! 0 SEMANTIC-ON !
   AOT-ARM:HERE-N PREFIX-DATA-N !
   1 PREFIX-READY ! ;

: START-FILE-PREFIX ( -- )
   ARM-OWNERS
   FORGET-FFI-CACHE
   START-PREFIX
   1 SEMANTIC-ON !
   REFRESH-LIVE ;

: REFRESH-OWNERS ( -- )
   SEMANTIC-ON @ 0= if exit then
   ACTIVE @ 0<> if E-NUNIT-PROFILE throw then
   REFRESH-LIVE ;

: RUN-BODY ( -- )
   \ The FILE body is compiled by the fresh target. Its borrowed emission
   \ must be read through that target's NART, never the retained arena.
   s" NPUB:WITH-UNIT" XREF-FIND dup XREF-FOUND? 0= if
      drop E-NUNIT-PROFILE throw
   then
   XREF-START dup AOT-ARM:B0 @ < if E-NUNIT-PROFILE throw then
   WITH-UNIT-XT
   ['] OBSERVE UNIT-BODY @ rot execute ;

: WITH ( [ -- ] -- )
   ARMED @ 0<> if E-NUNIT-PROFILE throw then
   UNIT-BODY !
   1 ARMED !
   0 ACTIVE ! 0 FINISHED !
   0 CALL-N ! 0 ADDR-N ! 0 FRAME-DEPTH ! 0 PREFIX-TOP !
   0 PREFIX-WID-TOP !
   ['] RUN-BODY catch {: rc:n :}
   0 ARMED !
   rc 0<> if 0 ACTIVE ! rc throw then
   FINISH ;

: START-UNIT ( -- ) START ;

: SESSION-WITH ( [ -- ] -- ) {: q :}
   ARMED @ 0<> if E-NUNIT-PROFILE throw then
   1 ARMED !
   0 ACTIVE ! 0 FINISHED ! 0 FRAME-DEPTH !
   0 CALL-N ! 0 ADDR-N ! 0 PREFIX-TOP ! 0 PREFIX-WID-TOP !
   q UNIT-BODY !
   ['] RUN-BODY catch {: rc:n :}
   0 ARMED !
   rc 0<> if 0 ACTIVE ! rc throw then
   ACTIVE @ 0<> FRAME-DEPTH @ 0<> or if E-NUNIT-PROFILE throw then ;

: ENTER ( -- )
   ARMED @ 0= if E-NUNIT-PROFILE throw then
   PUSH-FRAME START ;

: FINISH-UNIT ( -- ) FINISH ;
: REQUIRE-MARK ( -- n ) REQUIRE-COUNT ;
: LIMIT-REQUIRES ( n -- )
   {: count:n :}
   READY
   count REQUIRE-FIRST @ < count REQUIRE-END-N @ > or if E-NUNIT-PROFILE throw then
   count REQUIRE-END-N ! ;
: LEAVE-FRAME ( -- ) POP-FRAME ;

: IMPORT-CALL ( n n n -- ) {: site:n kind:n target:n :}
   ACTIVE @ 0= if exit then
   CALL-N @ {: row:n :}
   row 1+ CALL-CELLS * CALL-ROW-RESERVE
   site kind target row CALL!
   row 1+ CALL-N ! ;

: IMPORT-ADDR ( n n -- ) {: site:n kind:n :}
   ACTIVE @ 0= if exit then
   ADDR-N @ {: row:n :}
   row 1+ ADDR-CELLS * ADDR-ROW-RESERVE
   site kind row ADDR!
   row 1+ ADDR-N ! ;

: CALLS ( -- n ) READY CALL-N @ CALL-FIRST @ - ;

: CALL@ ( n -- n n n ) {: row:n :}
   READY
   row 0 < row CALL-N @ CALL-FIRST @ - >= or if E-NUNIT-PROFILE throw then
   row CALL-FIRST @ + CALL-CELLS * CALL-ROW @ CODE-START @ -
   row CALL-FIRST @ + CALL-CELLS * 1+ CALL-ROW @
   row CALL-FIRST @ + CALL-CELLS * 2 + CALL-ROW @ ;

: ADDRS ( -- n ) READY ADDR-N @ ADDR-FIRST @ - ;

: ADDR@ ( n -- n n ) {: row:n :}
   READY
   row 0 < row ADDR-N @ ADDR-FIRST @ - >= or if E-NUNIT-PROFILE throw then
   row ADDR-FIRST @ + ADDR-CELLS * ADDR-ROW @ CODE-START @ -
   row ADDR-FIRST @ + ADDR-CELLS * 1+ ADDR-ROW @ ;

: CODE$ ( -- ptr u8 n )
   READY CODE-START @ CODE-BYTES CODE-END @ CODE-START @ - ;

: RECORDS$ ( -- ptr u8 n )
   READY REC-START @ XREF-REC-ADDR CODE-BYTES
   REC-END @ REC-START @ - DREC * ;

: FIRST-REC ( -- n ) READY REC-START @ ;
: PREFIX-WID@ ( n -- n )
   {: row:n :}
   READY
   row 0 < row REC-START @ >= or if E-NUNIT-PROFILE throw then
   PREFIX-WID-OFF @ row + PREFIX-WIDS @ ;
: FIRST-CODE ( -- n ) READY CODE-START @ ;
: FIRST-WID ( -- n ) READY WID-START @ ;
: NEW-WIDS ( -- n ) READY WID-END @ WID-START @ - ;
: FIRST-DATA ( -- n ) READY DATA-START-N @ ;
: PREFIX-DATA ( -- n ) PREFIX-READY @ 0= if E-NUNIT-PROFILE throw then PREFIX-DATA-N @ ;
: PREFIX-BEFORE$ ( -- ptr u8 n )
   READY
   PREFIX-BYTES @ 0= if NULL-PTR 0 exit then
   PREFIX-OFF @ PREFIX-BEFORE PREFIX-BYTES @ ;
: DATA$ ( -- ptr u8 n )
   READY DATA-START-N @ DATA-BYTES DATA-END-N @ DATA-START-N @ - ;
: OWNED-SPANS ( -- n ) READY OWNED-N @ OWNED-FIRST @ - ;
: OWNED@ ( n -- n ptr u8 n )
   {: index:n :}
   READY
   index 0 < index OWNED-N @ OWNED-FIRST @ - >= or if E-NUNIT-PROFILE throw then
   index OWNED-FIRST @ + {: row:n :}
   row OWNED-CELLS * OWNED-ROW @
   row OWNED-CELLS * 2 + OWNED-ROW @ OWNED-BEFORE
   row OWNED-CELLS * 1+ OWNED-ROW @ ;

: IN-OWNED? ( n n n -- bool )
   {: address:n width:n prefix-end:n :}
   address PREFIX-DATA-N @ >=
   width prefix-end address - <= and if true exit then
   OWNED-N @ OWNED-FIRST @ ?do
      i OWNED-CELLS * OWNED-ROW @ {: first:n :}
      i OWNED-CELLS * 1+ OWNED-ROW @ {: bytes:n :}
      address first >= width first bytes + address - <= and if
         true unloop exit
      then
   loop false ;
: OWNED-BYTE? ( n n -- bool ) 1 swap IN-OWNED? ;
: OWNED-CELL? ( n n -- bool ) CELL swap IN-OWNED? ;
: FIRST-LITERAL ( -- n ) READY LITERAL-ROWS-N @ ;
: FIRST-SEGMENTS ( -- n ) READY LITERAL-SEGMENTS-N @ ;
: NEW-LITERALS ( -- n ) READY LITERAL-COUNT LITERAL-ROWS-N @ - ;
: FIRST-REQUIRE ( -- n ) READY REQUIRE-FIRST @ ;
: NEW-REQUIRES ( -- n ) READY REQUIRE-END-N @ REQUIRE-FIRST @ - ;

: CLOSE ( -- )
   0 UNIT-BODY BYTE-VIEW CELL-VIEW !
   0 ARMED ! 0 ACTIVE ! 0 FINISHED !
   0 CALL-N ! 0 ADDR-N !
   0 CODE-START ! 0 CODE-END ! 0 REC-START ! 0 REC-END !
   0 WID-START ! 0 WID-END ! 0 DATA-START-N ! 0 DATA-END-N !
   0 PREFIX-DATA-N ! 0 PREFIX-BYTES ! 0 PREFIX-OFF ! 0 PREFIX-TOP !
   0 PREFIX-WID-OFF ! 0 PREFIX-WID-TOP !
   0 PREFIX-READY ! 0 OWNED-N ! 0 OWNED-FIRST ! 0 OWNED-TOP !
   0 LITERAL-ROWS-N ! 0 LITERAL-BYTES-N ! 0 LITERAL-SEGMENTS-N !
   0 REQUIRE-FIRST ! 0 REQUIRE-END-N ! 0 CALL-FIRST ! 0 ADDR-FIRST !
   0 FRAME-DEPTH !
   0 SEMANTIC-ON !
   CALL-ROW-RELEASE ADDR-ROW-RELEASE PREFIX-BEFORE-RELEASE FRAME-RELEASE
   PREFIX-WIDS-RELEASE
   OWNED-ROW-RELEASE OWNED-BEFORE-RELEASE ;

;package
