\ Capture one package's native code and publication sites while the retained
\ compiler produces it. The observer copies the owned emission rows before
\ retires them; final code and records are read only after publication ends.

require src/lib/errors.f
require src/compiler/native/publish.f
require src/compiler/session/emission.f
require src/compiler/native/string.f
require src/habu/xref.f
require src/habu/aot-arm.f
require src/habu/address-cells.f
require src/lib/string.f

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
DYNAMIC-BUFFER SLOT-BEFORE n
variable SLOT-FIRST
variable SLOT-COUNT
variable SLOT-TOP
2 constant STATE-CELLS
DYNAMIC-BUFFER STATE-ROW n
variable STATE-FIRST
variable STATE-COUNT
variable STATE-TOP
TYPED-VARIABLE STATE-SPANS-XT [ [ ptr u8 n -- ] -- ]
variable SEMANTIC-ON
variable LITERAL-ROWS-N
variable LITERAL-BYTES-N
variable LITERAL-SEGMENTS-N
variable REQUIRE-FIRST
variable REQUIRE-END-N
variable ARMED
variable ACTIVE
variable FINISHED
variable PRIOR-CAPTURE-INVALIDATE
variable CALL-FIRST
variable ADDR-FIRST
31 constant FRAME-CELLS
DYNAMIC-BUFFER FRAME n
variable FRAME-DEPTH
DYNAMIC-BUFFER PRODUCER-NAME ptr u8
DYNAMIC-BUFFER PRODUCER-LEN n
DYNAMIC-BUFFER PRODUCER-XT n
variable PRODUCER-N
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
CAST: RECLAIM-CELL-XT ( n -- [ -- ptr n ] )
CAST: RECLAIM-SLOT ( ptr n -- ptr [ n n -- ] )
CAST: RECLAIM-XT ( n -- [ n n -- ] )

: BIND-ONE ( ptr u8 n bool -- ) {: a:ptr u:n optional:bool :}
   a u XREF-FIND dup XREF-FOUND? if XREF-START
   else drop optional 0= if E-NUNIT-PROFILE throw then 0 then
      {: xt:n :}
   PRODUCER-N @ {: row:n :}
   row 1+ PRODUCER-NAME-RESERVE
   row 1+ PRODUCER-LEN-RESERVE
   row 1+ PRODUCER-XT-RESERVE
   a row PRODUCER-NAME !
   u row PRODUCER-LEN !
   xt row PRODUCER-XT !
   1 PRODUCER-N +! ;

: BOUND-XT ( ptr u8 n -- n ) {: a:ptr u:n :}
   PRODUCER-N @ 0= if E-NUNIT-PROFILE throw then
   PRODUCER-N @ 0 ?do
      i PRODUCER-NAME @ i PRODUCER-LEN @ a u STR= if
         i PRODUCER-XT @ unloop exit
      then
   loop
   E-NUNIT-PROFILE throw ;

: TARGET-XT ( ptr u8 n -- n )
   BOUND-XT dup 0= if E-NUNIT-PROFILE throw then ;

\ These owners exist only when an application has loaded callback support.
: CALLBACK-OWNER-XT ( ptr u8 n -- n ) BOUND-XT ;

: TARGET-ONE ( n ptr u8 n -- n ) {: x:n a:ptr u:n :}
   a u TARGET-XT ONE-XT x swap execute ;
: TARGET-TWO ( n n ptr u8 n -- n ) {: x:n i:n a:ptr u:n :}
   a u TARGET-XT TWO-XT x i rot execute ;

: TARGET-NUMBER ( ptr u8 n -- n )
   TARGET-XT NUMBER-XT execute ;

: RECLAIM-CELL ( -- ptr n )
   s" native-unit-reclaim-cell" BOUND-XT dup 0= if E-NUNIT-PROFILE throw then
   RECLAIM-CELL-XT execute ;

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

: CALLBACK-OWNERS ( [ ptr u8 n -- ] -- ) {: keep :}
   s" FFI-CB:OWNED-CELLS" CALLBACK-OWNER-XT dup 0<> if
      DECL-OWNED-XT keep swap execute
   else drop then
   s" TASK:OWNED-CALLBACK-ROWS" CALLBACK-OWNER-XT dup 0<> if
      DECL-OWNED-XT keep swap execute
   else drop then ;

: TASK-OWNER-XT ( -- n )
   s" TASK:OWNED-CELLS" BOUND-XT dup 0<> if exit then
   drop
   \ The prefix may load TASK after BIND-PRODUCER ran.
   s" TASK:OWNED-CELLS" XREF-FIND dup XREF-FOUND? if
      XREF-START
   else drop 0 then ;

\ TASK declarations change baked DATA below the ordinary FILE prefix. Process
\ scratch is fresh state, so only the runtime's carried declarations are kept.
: TASK-OWNERS ( [ ptr u8 n -- ] -- )
   {: keep :}
   TASK-OWNER-XT dup 0<> if
      OWNED-XT keep ['] IGNORE-DATA rot execute
   else drop then ;

: SNAP-OWNERS ( -- )
   s" FFI:OWNED-CELLS" TARGET-XT OWNED-XT
      ['] KEEP-DATA ['] IGNORE-DATA rot execute
   s" FFI-DECL:OWNED-CELLS" TARGET-XT DECL-OWNED-XT
      ['] KEEP-DATA swap execute
   ['] KEEP-DATA CALLBACK-OWNERS
   ['] KEEP-DATA TASK-OWNERS ;

: SNAP-SLOTS ( -- )
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   SLOT-TOP @ SLOT-FIRST !
   count SLOT-COUNT !
   SLOT-TOP @ count + SLOT-BEFORE-RESERVE
   count 0 ?do
      rows i cells + @ SLOT-FIRST @ i + SLOT-BEFORE !
   loop
   count SLOT-TOP +! ;

: KEEP-STATE ( ptr u8 n -- ) {: source:ptr bytes:n :}
   bytes 0 <= if exit then
   source BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + {: address:n :}
   address DATA-VA VA>N max {: first:n :}
   address bytes + DATA-END-N @ min {: end:n :}
   end first <= if exit then
   STATE-TOP @ 1+ STATE-CELLS * STATE-ROW-RESERVE
   first STATE-TOP @ STATE-CELLS * STATE-ROW !
   end first - STATE-TOP @ STATE-CELLS * 1+ STATE-ROW !
   1 STATE-TOP +! ;

: IGNORE-STATE ( [ ptr u8 n -- ] -- ) drop ;

: SNAP-STATE ( -- )
   STATE-TOP @ STATE-FIRST !
   ['] KEEP-STATE STATE-SPANS-XT @ execute
   STATE-TOP @ STATE-FIRST @ - STATE-COUNT ! ;

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
   s" TASK:ARM-IMPORTED-CALLBACKS" CALLBACK-OWNER-XT dup 0<> if
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

: FRAME! ( n n n -- ) {: x:n row:n field:n :}
   x row FRAME-CELLS * field + FRAME ! ;
: FRAME@ ( n n -- n ) {: row:n field:n :}
   row FRAME-CELLS * field + FRAME @ ;

: SITE-KEPT? ( n n n -- bool )
   {: site:n first:n last:n :}
   site first < site last >= or ;

: KEPT-CALLS ( n n n -- n ) {: end:n first:n last:n :}
   0
   end 0 ?do
      i CALL-CELLS * CALL-ROW @ first last SITE-KEPT? if 1+ then
   loop ;

: KEPT-ADDRS ( n n n -- n ) {: end:n first:n last:n :}
   0
   end 0 ?do
      i ADDR-CELLS * ADDR-ROW @ first last SITE-KEPT? if 1+ then
   loop ;

: PRUNE-CALLS ( n n -- ) {: first:n last:n :}
   \ Parent and nested frames share these rows; shift their starts before compaction.
   CALL-FIRST @ first last KEPT-CALLS CALL-FIRST !
   FRAME-DEPTH @ 0 ?do
      i 17 FRAME@ first last KEPT-CALLS i 17 FRAME!
   loop
   CALL-N @ {: end:n :}
   0 CALL-N !
   end 0 ?do
      i CALL-CELLS * CALL-ROW @ {: site:n :}
      site first last SITE-KEPT? if
         site
         i CALL-CELLS * 1+ CALL-ROW @
         i CALL-CELLS * 2 + CALL-ROW @
         CALL-N @ CALL!
         1 CALL-N +!
      then
   loop ;

: PRUNE-ADDRS ( n n -- ) {: first:n last:n :}
   ADDR-FIRST @ first last KEPT-ADDRS ADDR-FIRST !
   FRAME-DEPTH @ 0 ?do
      i 18 FRAME@ first last KEPT-ADDRS i 18 FRAME!
   loop
   ADDR-N @ {: end:n :}
   0 ADDR-N !
   end 0 ?do
      i ADDR-CELLS * ADDR-ROW @ {: site:n :}
      site first last SITE-KEPT? if
         site i ADDR-CELLS * 1+ ADDR-ROW @ ADDR-N @ ADDR!
         1 ADDR-N +!
      then
   loop ;

: PRUNE-SITES ( n n -- )
   2dup PRUNE-CALLS PRUNE-ADDRS ;

: INVALIDATE ( n n -- )
   {: first:n last:n :}
   first last PRUNE-SITES ;

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
   SNAP-SLOTS
   SEMANTIC-ON @ if SNAP-OWNERS then
   STATE-TOP @ STATE-FIRST !  0 STATE-COUNT !
   0 FINISHED !
   1 ACTIVE ! ;

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
   SLOT-FIRST @ row 25 FRAME! SLOT-COUNT @ row 26 FRAME!
   SLOT-TOP @ row 27 FRAME!
   STATE-FIRST @ row 28 FRAME! STATE-COUNT @ row 29 FRAME!
   STATE-TOP @ row 30 FRAME!
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
   row 24 FRAME@ PREFIX-WID-TOP !
   row 25 FRAME@ SLOT-FIRST ! row 26 FRAME@ SLOT-COUNT !
   row 27 FRAME@ SLOT-TOP !
   row 28 FRAME@ STATE-FIRST ! row 29 FRAME@ STATE-COUNT !
   row 30 FRAME@ STATE-TOP ! ;

: FINISH ( -- )
   ACTIVE @ 0= if E-NUNIT-PROFILE throw then
   cp@ CODE-END !
   ndict@ REC-END !
   AOT-ARM:WIDN WID-END !
   CODE-END @ CODE-START @ < if E-NUNIT-PROFILE throw then
   REC-END @ REC-START @ < if E-NUNIT-PROFILE throw then
   AOT-ARM:HERE-N DATA-END-N !
   DATA-END-N @ DATA-START-N @ < if E-NUNIT-PROFILE throw then
   SNAP-STATE
   REQUIRE-COUNT REQUIRE-END-N !
   0 ACTIVE !
   1 FINISHED ! ;

: READY ( -- )
   FINISHED @ 0= if E-NUNIT-PROFILE throw then ;

public

EXPORT ARM-IMPORTED-CALLBACKS

: BIND-PRODUCER ( -- )
   0 PRODUCER-N !
   s" NPUB:WITH-UNIT" false BIND-ONE
   s" native-unit-reclaim-cell" true BIND-ONE
   s" NART:CALL-SITES" false BIND-ONE
   s" NART:CALL-SITE@" false BIND-ONE
   s" NART:CALL-KIND@" false BIND-ONE
   s" NART:CALL-TARGET@" false BIND-ONE
   s" NART:ADDR-SITES" false BIND-ONE
   s" NART:ADDR-SITE@" false BIND-ONE
   s" NART:ADDR-SITE-KIND@" false BIND-ONE
   s" NART:ADDR-SITE-ORIGIN@" true BIND-ONE
   s" NART:SIZE" false BIND-ONE
   s" NART:BYTES" false BIND-ONE
   s" NART:RET-BYTES" false BIND-ONE
   s" NART:FUNCTIONS" false BIND-ONE
   s" NART:FUNCTION-OFFSET@" false BIND-ONE
   s" NART:CALL-IMPL@" false BIND-ONE
   s" NART:CALL-LOC@" false BIND-ONE
   s" NHOST:TAKEN-FUNCTIONS" true BIND-ONE
   s" NHOST:TAKEN-FACTS" true BIND-ONE
   s" NHOST:TAKEN-TYPE@" true BIND-ONE
   s" NHOST:PREPARE-IMPORT" true BIND-ONE
   s" NHOST:ABANDON-TAKEN" true BIND-ONE
   s" NPUB:IMPORT-PENDING" true BIND-ONE
   s" NCOMP:DEFINITION-CACHEABLE?" true BIND-ONE
   s" NCOMP:DEFINITION-HISTORY" true BIND-ONE
   s" NCOMP:REPLAY-DEFINITION" true BIND-ONE
   s" NSTR:OWNER-ROW" true BIND-ONE
   s" NSTR:PREPARE-BEGIN" true BIND-ONE
   s" NSTR:PREPARE-INTERN" true BIND-ONE
   s" NSTR:PREPARE-DATA-END" true BIND-ONE
   s" NSTR:PREPARE-CLOSE" true BIND-ONE
   s" NSTR:INTERN" true BIND-ONE
   s" NSTR:SOURCE-BEGIN" true BIND-ONE
   s" NSTR:SOURCE-END" true BIND-ONE
   s" NSTR:SOURCE-COUNT" true BIND-ONE
   s" NSTR:SOURCE@" true BIND-ONE
   s" NSTR:SOURCE-CLOSE" true BIND-ONE
   s" NSTR:COUNT" false BIND-ONE
   s" NSTR:BYTES" false BIND-ONE
   s" NSTR:SEGMENTS" false BIND-ONE
   s" NSTR:CELL-SPANS" false BIND-ONE
   s" REQUIRE-REG:COUNT" false BIND-ONE
   s" FFI:OWNED-CELLS" false BIND-ONE
   s" FFI:SCAN-SPANS" false BIND-ONE
   s" FFI-DECL:OWNED-CELLS" false BIND-ONE
   s" FFI-DECL:SCAN-SPANS" false BIND-ONE
   s" FFI-DECL:ARM-SCOPE" false BIND-ONE
   s" FFI:FORGET-CACHE" false BIND-ONE
   s" FFI-CB:OWNED-CELLS" true BIND-ONE
   s" FFI-CB:SCAN-SPANS" true BIND-ONE
   s" TASK:OWNED-CALLBACK-ROWS" true BIND-ONE
   s" TASK:OWNED-CELLS" true BIND-ONE
   s" TASK:ARM-IMPORTED-CALLBACKS" true BIND-ONE ;

: SCAN-XTS ( -- n n n n n )
   s" NSTR:CELL-SPANS" BOUND-XT
   s" FFI:SCAN-SPANS" BOUND-XT
   s" FFI-DECL:SCAN-SPANS" BOUND-XT
   s" TASK:OWNED-CALLBACK-ROWS" BOUND-XT
   s" FFI-CB:SCAN-SPANS" BOUND-XT ;

: BOUND-PRODUCER-XT ( ptr u8 n -- n ) TARGET-XT ;
: OPTIONAL-PRODUCER-XT ( ptr u8 n -- n ) BOUND-XT ;

: WITH-OBSERVER ( [ n n n n -- ] [ -- ] -- )
   s" NPUB:WITH-UNIT" TARGET-XT WITH-UNIT-XT execute ;

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
   \ Keep the emitting compiler and its row readers paired across target reset.
   s" NPUB:WITH-UNIT" TARGET-XT WITH-UNIT-XT
   ['] OBSERVE UNIT-BODY @ rot execute ;

: RUN-OWNED ( -- )
   \ The kernel reports every reclaimed code span to this capture alone.
   RECLAIM-CELL {: slot:ptr :}
   NHOST:INSTALL
   slot @ PRIOR-CAPTURE-INVALIDATE !
   ['] INVALIDATE slot RECLAIM-SLOT xt!
   ['] RUN-BODY catch {: rc:n :}
   PRIOR-CAPTURE-INVALIDATE @ dup 0<> if
      RECLAIM-XT slot RECLAIM-SLOT xt!
   else drop 0 slot ! then
   0 PRIOR-CAPTURE-INVALIDATE !
   rc 0<> if rc throw then ;

: WITH ( [ -- ] -- )
   ARMED @ 0<> if E-NUNIT-PROFILE throw then
   UNIT-BODY !
   1 ARMED !
   0 ACTIVE ! 0 FINISHED !
   0 CALL-N ! 0 ADDR-N ! 0 CALL-FIRST ! 0 ADDR-FIRST !
   0 FRAME-DEPTH ! 0 PREFIX-TOP !
   0 PREFIX-WID-TOP ! 0 SLOT-TOP ! 0 STATE-TOP !
   ['] IGNORE-STATE STATE-SPANS-XT !
   ['] RUN-OWNED catch {: rc:n :}
   0 ARMED !
   rc 0<> if 0 ACTIVE ! rc throw then
   FINISH ;

: START-UNIT ( -- ) START ;

: SESSION-WITH ( [ -- ] -- ) {: q :}
   ARMED @ 0<> if E-NUNIT-PROFILE throw then
   1 ARMED !
   0 ACTIVE ! 0 FINISHED ! 0 FRAME-DEPTH !
   0 CALL-N ! 0 ADDR-N ! 0 CALL-FIRST ! 0 ADDR-FIRST !
   0 PREFIX-TOP ! 0 PREFIX-WID-TOP !
   0 SLOT-TOP ! 0 STATE-TOP !
   ['] IGNORE-STATE STATE-SPANS-XT !
   q UNIT-BODY !
   ['] RUN-OWNED catch {: rc:n :}
   0 ARMED !
   rc 0<> if 0 ACTIVE ! rc throw then
   ACTIVE @ 0<> FRAME-DEPTH @ 0<> or if E-NUNIT-PROFILE throw then ;

: ENTER ( [ [ ptr u8 n -- ] -- ] -- )
   ARMED @ 0= if E-NUNIT-PROFILE throw then
   STATE-SPANS-XT !
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
: STATE-SPAN? ( n n -- bool ) {: address:n width:n :}
   READY
   STATE-COUNT @ 0 ?do
      STATE-FIRST @ i + STATE-CELLS * STATE-ROW @ {: first:n :}
      STATE-FIRST @ i + STATE-CELLS * 1+ STATE-ROW @ {: bytes:n :}
      address first >= if
         address first - {: off:n :}
         off bytes <= width bytes off - <= and if true unloop exit then
      then
   loop false ;
: STATE-CELL? ( n -- bool ) CELL STATE-SPAN? ;
: STATE-BYTE? ( n -- bool ) 1 STATE-SPAN? ;
: NEUTRALIZE-FRESH ( ptr u8 n -- ) {: data:ptr size:n :}
   READY
   DATA-START-N @ {: start:n :}
   STATE-COUNT @ 0 ?do
      STATE-FIRST @ i + STATE-CELLS * STATE-ROW @ {: first:n :}
      STATE-FIRST @ i + STATE-CELLS * 1+ STATE-ROW @ first + {: end:n :}
      first start max {: lo:n :}
      end start size + min {: hi:n :}
      hi lo > if
         hi lo - 0 ?do 0 data lo start - i + + c! loop
      then
   loop ;
: DECLARED-BEFORE? ( n n -- bool ) {: row:n raw:n :}
   READY
   row SLOT-COUNT @ < if
      SLOT-FIRST @ row + SLOT-BEFORE @ raw = if true exit then
   then
   SLOT-COUNT @ 0 ?do
      SLOT-FIRST @ i + SLOT-BEFORE @ raw = if true unloop exit then
   loop false ;
: BEFORE-CELL ( n -- n bool ) {: site:n :}
   site PREFIX-DATA-N @ >= if
      site PREFIX-DATA-N @ - {: off:n :}
      off PREFIX-BYTES @ <= CELL PREFIX-BYTES @ off - <= and if
         PREFIX-OFF @ off + PREFIX-BEFORE CELL-VIEW @ true exit
      then
   then
   OWNED-N @ OWNED-FIRST @ ?do
      i OWNED-CELLS * OWNED-ROW @ {: first:n :}
      i OWNED-CELLS * 1+ OWNED-ROW @ {: bytes:n :}
      site first >= if
         site first - {: off:n :}
         off bytes <= CELL bytes off - <= and if
            i OWNED-CELLS * 2 + OWNED-ROW @ off +
               OWNED-BEFORE CELL-VIEW @ true unloop exit
         then
      then
   loop 0 false ;
: UNCHANGED-CELL? ( n -- bool ) {: site:n :}
   READY
   site BEFORE-CELL if site CODE-BYTES CELL-VIEW @ = exit then
   drop false ;
: FIRST-LITERAL ( -- n ) READY LITERAL-ROWS-N @ ;
: FIRST-SEGMENTS ( -- n ) READY LITERAL-SEGMENTS-N @ ;
: NEW-LITERALS ( -- n ) READY LITERAL-COUNT LITERAL-ROWS-N @ - ;
: FIRST-REQUIRE ( -- n ) READY REQUIRE-FIRST @ ;
: NEW-REQUIRES ( -- n ) READY REQUIRE-END-N @ REQUIRE-FIRST @ - ;

: CLOSE ( -- )
   PRODUCER-NAME-RELEASE PRODUCER-LEN-RELEASE PRODUCER-XT-RELEASE
   0 PRODUCER-N !
   0 UNIT-BODY BYTE-VIEW CELL-VIEW !
   0 ARMED ! 0 ACTIVE ! 0 FINISHED !
   0 PRIOR-CAPTURE-INVALIDATE !
   0 CALL-N ! 0 ADDR-N !
   0 CODE-START ! 0 CODE-END ! 0 REC-START ! 0 REC-END !
   0 WID-START ! 0 WID-END ! 0 DATA-START-N ! 0 DATA-END-N !
   0 PREFIX-DATA-N ! 0 PREFIX-BYTES ! 0 PREFIX-OFF ! 0 PREFIX-TOP !
   0 PREFIX-WID-OFF ! 0 PREFIX-WID-TOP !
   0 PREFIX-READY ! 0 OWNED-N ! 0 OWNED-FIRST ! 0 OWNED-TOP !
   SLOT-BEFORE-RELEASE 0 SLOT-FIRST ! 0 SLOT-COUNT ! 0 SLOT-TOP !
   STATE-ROW-RELEASE 0 STATE-FIRST ! 0 STATE-COUNT ! 0 STATE-TOP !
   ['] IGNORE-STATE STATE-SPANS-XT !
   0 LITERAL-ROWS-N ! 0 LITERAL-BYTES-N ! 0 LITERAL-SEGMENTS-N !
   0 REQUIRE-FIRST ! 0 REQUIRE-END-N ! 0 CALL-FIRST ! 0 ADDR-FIRST !
   0 FRAME-DEPTH !
   0 SEMANTIC-ON !
   CALL-ROW-RELEASE ADDR-ROW-RELEASE PREFIX-BEFORE-RELEASE FRAME-RELEASE
   PREFIX-WIDS-RELEASE
   OWNED-ROW-RELEASE OWNED-BEFORE-RELEASE ;

;package
