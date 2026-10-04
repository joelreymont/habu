\ Capture one package's native code and publication sites while the retained
\ compiler produces it. The observer copies the owned emission rows before
\ retires them; final code and records are read only after publication ends.

require lib/errors.f
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
variable LITERAL-ROWS-N
variable LITERAL-BYTES-N
variable ARMED
variable ACTIVE
variable FINISHED
TYPED-VARIABLE UNIT-BODY [ -- ]

TRUSTED: CODE-BYTES ( n -- ptr u8 ) ;

: CALL! ( n n n n -- ) {: site:n kind:n target:n row:n :}
   site row CALL-CELLS * CALL-ROW !
   kind row CALL-CELLS * 1+ CALL-ROW !
   target row CALL-CELLS * 2 + CALL-ROW ! ;

: ADDR! ( n n n -- ) {: site:n kind:n row:n :}
   site row ADDR-CELLS * ADDR-ROW !
   kind row ADDR-CELLS * 1+ ADDR-ROW ! ;

: OBSERVE ( NART:emission n n n -- )
   {: e:NART:emission idx:n fn:n size:n :}
   ACTIVE @ 0= if exit then
   fn CODE-START @ < if E-NUNIT-PROFILE throw then
   e NART:CALL-SITES 0 ?do
      CALL-N @ {: row:n :}
      row 1+ CALL-CELLS * CALL-ROW-RESERVE
      fn e i NART:CALL-SITE@ + CODE-START @ -
      e i NART:CALL-KIND@
      e i NART:CALL-TARGET@
      row CALL!
      row 1+ CALL-N !
   loop
   e NART:ADDR-SITES 0 ?do
      ADDR-N @ {: row:n :}
      row 1+ ADDR-CELLS * ADDR-ROW-RESERVE
      fn e i NART:ADDR-SITE@ + CODE-START @ -
      e i NART:ADDR-SITE-KIND@
      row ADDR!
      row 1+ ADDR-N !
   loop ;

: START ( -- )
   ARMED @ 0= ACTIVE @ 0<> or if E-NUNIT-PROFILE throw then
   cp@ CODE-START !
   ndict@ REC-START !
   AOT-ARM:WIDN WID-START !
   AOT-ARM:HERE-N DATA-START-N !
   NSTR:COUNT LITERAL-ROWS-N !
   NSTR:BYTES LITERAL-BYTES-N !
   0 CALL-N ! 0 ADDR-N ! 0 FINISHED !
   1 ACTIVE ! ;

: FINISH ( -- )
   ACTIVE @ 0= if E-NUNIT-PROFILE throw then
   cp@ CODE-END !
   ndict@ REC-END !
   AOT-ARM:WIDN WID-END !
   AOT-ARM:HERE-N DATA-START-N @ <> if E-NUNIT-PROFILE throw then
   WID-END @ WID-START @ - 2 <> if E-NUNIT-PROFILE throw then
   CODE-END @ CODE-START @ < if E-NUNIT-PROFILE throw then
   REC-END @ REC-START @ <= if E-NUNIT-PROFILE throw then
   NSTR:COUNT LITERAL-ROWS-N @ <> if E-NUNIT-PROFILE throw then
   NSTR:BYTES LITERAL-BYTES-N @ <> if E-NUNIT-PROFILE throw then
   0 ACTIVE !
   1 FINISHED ! ;

: READY ( -- )
   FINISHED @ 0= if E-NUNIT-PROFILE throw then ;

public

: RUN-BODY ( -- )
   ['] OBSERVE UNIT-BODY @ NPUB:WITH-UNIT ;

: WITH ( [ -- ] -- ) {: q :}
   ARMED @ 0<> if E-NUNIT-PROFILE throw then
   1 ARMED !
   0 ACTIVE ! 0 FINISHED !
   q UNIT-BODY !
   ['] RUN-BODY catch {: rc:n :}
   0 ARMED !
   rc 0<> if 0 ACTIVE ! rc throw then
   FINISH ;

: START-UNIT ( -- ) START ;

: CALLS ( -- n ) READY CALL-N @ ;

: CALL@ ( n -- n n n ) {: row:n :}
   READY
   row 0 < row CALL-N @ >= or if E-NUNIT-PROFILE throw then
   row CALL-CELLS * CALL-ROW @
   row CALL-CELLS * 1+ CALL-ROW @
   row CALL-CELLS * 2 + CALL-ROW @ ;

: ADDRS ( -- n ) READY ADDR-N @ ;

: ADDR@ ( n -- n n ) {: row:n :}
   READY
   row 0 < row ADDR-N @ >= or if E-NUNIT-PROFILE throw then
   row ADDR-CELLS * ADDR-ROW @
   row ADDR-CELLS * 1+ ADDR-ROW @ ;

: CODE$ ( -- ptr u8 n )
   READY CODE-START @ CODE-BYTES CODE-END @ CODE-START @ - ;

: RECORDS$ ( -- ptr u8 n )
   READY REC-START @ XREF-REC-ADDR CODE-BYTES
   REC-END @ REC-START @ - DREC * ;

: FIRST-REC ( -- n ) READY REC-START @ ;
: FIRST-CODE ( -- n ) READY CODE-START @ ;
: FIRST-WID ( -- n ) READY WID-START @ ;

: CLOSE ( -- )
   0 ARMED ! 0 ACTIVE ! 0 FINISHED !
   CALL-ROW-RELEASE ADDR-ROW-RELEASE ;

;package
