\ pg.f - PostgreSQL over libpq, bound through the FFI FUNCTION: declarer.
\
\ A connection and a result are nominal handles, never a raw cell: each names a
\ slot in this module's registry, which holds the libpq pointer, the task that
\ owns it and a generation. The generation is what makes the result owner
\ linear - CLEAR retires the slot, so the handle a caller still holds refuses
\ every later use with E-CLEARED instead of reaching a freed PGresult. The same
\ registry carries the concurrency rule from docs/database-models.md: a
\ connection and its results belong to the task that created them and a handle
\ presented by any other task is E-HANDLE.
\
\ Statement and parameter text have no fixed ceiling. Each connection builds one
\ call in an arena it allocates from lib/memory.f, sized to that call: the
\ statement bytes, every parameter with its NUL terminator, and the paramValues
\ pointer array. The arena is released as soon as libpq returns, and it belongs
\ to the connection, so no path leaks it - the next PARAMS, CLOSE and image
\ capture all release it too. CONFIGURE declares the connection, result and
\ parameter counts, with E-CAPACITY at those boundaries.
\
\ Parameters are text format throughout, so libpq's paramTypes, paramLengths
\ and paramFormats are all NULL: the server infers each type, lengths are
\ ignored for text, and a NULL formats array means text. Only paramValues is
\ built here.
\
\ See docs/db.md for the vocabulary, the buffer lifetimes and a worked example.

require lib/errors.f
require lib/ffi-abi.f
require lib/string.f
require lib/memory.f
require lib/type/deftype.f
require lib/task.f
require lib/aio.f
require lib/image-lifecycle.f

package PG

public

NEWTYPE connection 0
NEWTYPE result 0

DEFTYPE ROW
DEFTYPE COL

SUMTYPE connect-result 0
   VARIANT connected connection ;VARIANT
   VARIANT refused ptr u8 n ;VARIANT
;SUMTYPE

\ The failure spans are the server's SQLSTATE and its primary message, copied
\ into the owning connection's storage, so they outlive CLEAR and are replaced
\ by the next OUTCOME on that connection.
SUMTYPE outcome 0
   VARIANT ok ;VARIANT
   VARIANT rows ;VARIANT
   VARIANT failed ptr u8 n ptr u8 n ;VARIANT
;SUMTYPE

\ A dispatcher registers the waiting descriptor and event mask with its own
\ loop, then calls POLL again. No progress step waits for socket readiness.
SUMTYPE progress 0
   VARIANT waiting fd n ;VARIANT
   VARIANT connected connection ;VARIANT
   VARIANT completed result ;VARIANT
   VARIANT refused ptr u8 n ;VARIANT
;SUMTYPE

private

CAST: >CONNECTION ( n -- connection )
CAST: CONNECTION>N ( connection -- n )
CAST: >RESULT ( n -- result )
CAST: RESULT>N ( result -- n )

\ ---- libpq facts, read off /usr/include/libpq-fe.h and postgres_ext.h ------
0 constant PGRES-POLLING-FAILED           \ PostgresPollingStatusType
1 constant PGRES-POLLING-READING
2 constant PGRES-POLLING-WRITING
3 constant PGRES-POLLING-OK
1 constant CONNECTION-BAD
1 constant CONNECT-FIRST
2 constant CONNECT-POLLING
1 constant PGRES-COMMAND-OK               \ ExecStatusType
2 constant PGRES-TUPLES-OK
$43 constant PG-DIAG-SQLSTATE             \ 'C'
$4D constant PG-DIAG-MESSAGE-PRIMARY      \ 'M'
0 constant TEXT-RESULT                    \ PQexecParams resultFormat

\ ---- registry storage -----------------------------------------------------
\ Connection, result and parameter counts belong to the application. There is
\ no process-wide guessed connection ceiling: CONFIGURE allocates exactly the
\ registries the caller declares, and a live registry cannot be resized.
$0400 constant MESSAGE-CAP                \ copied primary server message
$05 constant SQLSTATE-LEN
$10 constant VERB-CAP                     \ BEGIN / COMMIT / ROLLBACK, this module's own
MESSAGE-CAP constant SCAN-CAP             \ bound on a NUL scan of a libpq C string
$1000 constant ARENA-MIN                  \ smallest call arena worth mapping
$100000000 constant HANDLE-STRIDE         \ low 32 bits name the registry slot
$7FFFFFFF constant GENERATION-MAX         \ positive signed handle, high 31 bits
$FFFFFFFFFFFFFFF8 constant CELL-MASK      \ round an arena offset up to a cell
-1 constant NO-OFFSET                     \ a NULL parameter has no arena bytes
-1 constant NO-CONN                       \ RES-CONN of a free or half-claimed slot
10 constant DEC-BASE
$30 constant DIGIT-ZERO

TYPED-VARIABLE CONN-CAPACITY n
TYPED-VARIABLE RESULT-CAPACITY n
TYPED-VARIABLE PARAM-CAPACITY n
TYPED-VARIABLE CONFIGURED bool

DYNAMIC-BUFFER CONN-PG ptr u8
DYNAMIC-BUFFER CONN-GEN n
DYNAMIC-BUFFER CONN-LIVE n
DYNAMIC-BUFFER CONN-OWNER n
DYNAMIC-BUFFER CONN-TX n
DYNAMIC-BUFFER CONN-PARAM-N n
DYNAMIC-BUFFER CONN-ARENA ptr u8
DYNAMIC-BUFFER CONN-ARENA-CAP n
DYNAMIC-BUFFER CONN-ARENA-U n
DYNAMIC-BUFFER CONN-LAST-RESULT ptr u8
DYNAMIC-BUFFER CONN-PENDING n
DYNAMIC-BUFFER CONN-CONNECTING n
DYNAMIC-BUFFER CONN-MESSAGE-U n
DYNAMIC-BUFFER CONN-SQLSTATE-U n
DYNAMIC-BUFFER CONN-PARAM-OFF n
DYNAMIC-BUFFER CONN-VERB n
DYNAMIC-BUFFER CONN-MESSAGE n
DYNAMIC-BUFFER CONN-SQLSTATE n

DYNAMIC-BUFFER RES-PG ptr u8
DYNAMIC-BUFFER RES-GEN n
DYNAMIC-BUFFER RES-LIVE n
DYNAMIC-BUFFER RES-OWNER n
DYNAMIC-BUFFER RES-CONN n

TYPED-VARIABLE REGISTERED bool

false CONFIGURED !
false REGISTERED !

here data-base - negate 7 and allot
variable GENERATION
variable CONFIG-MUTEX

: CONN-CAP ( -- n ) CONN-CAPACITY @ ;
: RESULT-CAP ( -- n ) RESULT-CAPACITY @ ;
: PARAM-CAP ( -- n ) PARAM-CAPACITY @ ;


\ ---- the declared libpq bindings ------------------------------------------
\ Every pointer is `ptr u8`: a PGconn, a PGresult and a C string are all
\ foreign-owned addresses this side only passes back or reads with the width
\ libpq states. None is written by the callee, so none carries an extent.
VERSIONED-LIBRARY pq 5

FUNCTION: LIB-CONNECT-START PQconnectStart ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-CONNECT-POLL PQconnectPoll ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-STATUS PQstatus ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-SET-NONBLOCKING PQsetnonblocking ( ptr u8 n -- n ) ;FUNCTION
FUNCTION: LIB-SOCKET PQsocket ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-ERROR-MESSAGE PQerrorMessage ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-FINISH PQfinish ( ptr u8 -- ) ;FUNCTION
FUNCTION: LIB-SEND-QUERY PQsendQuery ( ptr u8 ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-SEND-QUERY-PARAMS PQsendQueryParams
   ( ptr u8 ptr u8 n ptr u8 ptr u8 ptr u8 ptr u8 n -- n ) ;FUNCTION
FUNCTION: LIB-SEND-PREPARE PQsendPrepare
   ( ptr u8 ptr u8 ptr u8 n ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-SEND-QUERY-PREPARED PQsendQueryPrepared
   ( ptr u8 ptr u8 n ptr u8 ptr u8 ptr u8 n -- n ) ;FUNCTION
FUNCTION: LIB-FLUSH PQflush ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-CONSUME-INPUT PQconsumeInput ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-IS-BUSY PQisBusy ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-GET-RESULT PQgetResult ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-RESULT-STATUS PQresultStatus ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-RESULT-ERROR-FIELD PQresultErrorField ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-NTUPLES PQntuples ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-NFIELDS PQnfields ( ptr u8 -- n ) ;FUNCTION
FUNCTION: LIB-FNAME PQfname ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-GETVALUE PQgetvalue ( ptr u8 n n -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-GETISNULL PQgetisnull ( ptr u8 n n -- n ) ;FUNCTION
FUNCTION: LIB-GETLENGTH PQgetlength ( ptr u8 n n -- n ) ;FUNCTION
FUNCTION: LIB-CLEAR PQclear ( ptr u8 -- ) ;FUNCTION
FUNCTION: LIB-CMD-TUPLES PQcmdTuples ( ptr u8 -- ptr u8 ) ;FUNCTION


\ ---- foreign scalars and addresses ----------------------------------------
\ A C `int` comes back in the low half of x0 and the high half is unspecified.
: C-INT ( n -- n )
   $FFFFFFFF and dup $80000000 and 0 <> if $100000000 - then ;


: NULL-ARG ( -- ptr u8 )
   NULL-PTR BYTE-VIEW ;


: NULL-ADDR? ( ptr u8 -- bool ) {: p :}
   p FFI:>CELL 0= ;


\ Bound scans to the diagnostic buffer; libpq gives these strings no extent.
: CSTR-LEN ( ptr u8 -- n ) {: p :}
   SCAN-CAP 0 ?do
      p i + c@ 0= if i unloop exit then
   loop
   SCAN-CAP ;


: CSTR$ ( ptr u8 -- ptr u8 n ) {: p :}
   p p CSTR-LEN ;


\ ---- slot storage ---------------------------------------------------------
: CONN-CHECK ( n -- ) {: slot:n :}
   CONFIGURED @ 0= if E-HANDLE throw then
   slot 0 < if E-HANDLE throw then
   slot CONN-CAP >= if E-HANDLE throw then ;


: CONN-PG-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot CONN-CHECK
   slot CONN-PG ;


: CONN-PG@ ( n -- ptr u8 )   CONN-PG-CELL @ ;
: CONN-PG! ( ptr u8 n -- ) {: p slot:n :}   p slot CONN-PG-CELL ! ;

: ARENA-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot CONN-CHECK
   slot CONN-ARENA ;

: DRAIN-LAST-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot CONN-CHECK
   slot CONN-LAST-RESULT ;

: DRAIN-LAST@ ( n -- ptr u8 )   DRAIN-LAST-CELL @ ;
: DRAIN-LAST! ( ptr u8 n -- ) {: p slot:n :}   p slot DRAIN-LAST-CELL ! ;

: PENDING@ ( n -- n ) CONN-PENDING @ ;
: PENDING! ( n n -- ) CONN-PENDING ! ;
: CONNECTING@ ( n -- n ) CONN-CONNECTING @ ;
: CONNECTING! ( n n -- ) CONN-CONNECTING ! ;

: IDLE-CHECK ( n -- ) {: slot:n :}
   slot PENDING@ 0<> slot CONNECTING@ 0<> or if E-STATEMENT throw then ;


: ARENA-BASE ( n -- ptr u8 )   ARENA-CELL @ ;
: ARENA-BASE! ( ptr u8 n -- ) {: p slot:n :}   p slot ARENA-CELL ! ;

: CONN-GEN@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-GEN @ ;
: CONN-GEN! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-GEN ! ;
: CONN-LIVE-CELL ( n -- ptr n ) {: slot:n :}   slot CONN-CHECK slot CONN-LIVE ;
: CONN-LIVE@ ( n -- n )   CONN-LIVE-CELL atomic@ ;
: CONN-LIVE! ( n n -- ) {: v:n slot:n :}   v slot CONN-LIVE-CELL atomic! ;
: CONN-OWNER@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-OWNER @ ;
: CONN-OWNER! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-OWNER ! ;
: CONN-TX@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-TX @ ;
: CONN-TX! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-TX ! ;
: PARAM-N@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-PARAM-N @ ;
: PARAM-N! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-PARAM-N ! ;
: ARENA-CAP@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-ARENA-CAP @ ;
: ARENA-CAP! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-ARENA-CAP ! ;
: ARENA-U@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-ARENA-U @ ;
: ARENA-U! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-ARENA-U ! ;
: MESSAGE-U@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-MESSAGE-U @ ;
: MESSAGE-U! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-MESSAGE-U ! ;
: SQLSTATE-U@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-SQLSTATE-U @ ;
: SQLSTATE-U! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-SQLSTATE-U ! ;

: VERB-BUF ( n -- ptr u8 ) {: slot:n :}   slot CONN-CHECK 0 CONN-VERB BYTE-VIEW slot VERB-CAP * + ;
: MESSAGE-BUF ( n -- ptr u8 ) {: slot:n :}
   slot CONN-CHECK 0 CONN-MESSAGE BYTE-VIEW slot MESSAGE-CAP * + ;
: SQLSTATE-BUF ( n -- ptr u8 ) {: slot:n :}
   slot CONN-CHECK 0 CONN-SQLSTATE BYTE-VIEW slot SQLSTATE-LEN * + ;

: PARAM-CHECK ( n -- ) {: idx:n :}
   CONFIGURED @ 0= if E-CAPACITY throw then
   idx 0 < if E-CAPACITY throw then
   idx PARAM-CAP >= if E-CAPACITY throw then ;


: PARAM-OFF@ ( n n -- n ) {: slot:n idx:n :}
   slot CONN-CHECK idx PARAM-CHECK
   slot PARAM-CAP * idx + CONN-PARAM-OFF @ ;


: PARAM-OFF! ( n n n -- ) {: v:n slot:n idx:n :}
   slot CONN-CHECK idx PARAM-CHECK
   v slot PARAM-CAP * idx + CONN-PARAM-OFF ! ;


: RES-CHECK ( n -- ) {: slot:n :}
   CONFIGURED @ 0= if E-CLEARED throw then
   slot 0 < if E-HANDLE throw then
   slot RESULT-CAP >= if E-HANDLE throw then ;


: RES-PG-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot RES-CHECK
   slot RES-PG ;


: RES-PG@ ( n -- ptr u8 )   RES-PG-CELL @ ;
: RES-PG! ( ptr u8 n -- ) {: p slot:n :}   p slot RES-PG-CELL ! ;

: RES-GEN@ ( n -- n ) {: slot:n :}   slot RES-CHECK slot RES-GEN @ ;
: RES-GEN! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v slot RES-GEN ! ;
: RES-LIVE-CELL ( n -- ptr n ) {: slot:n :}   slot RES-CHECK slot RES-LIVE ;
: RES-LIVE@ ( n -- n )   RES-LIVE-CELL atomic@ ;
: RES-LIVE! ( n n -- ) {: v:n slot:n :}   v slot RES-LIVE-CELL atomic! ;
: RES-OWNER@ ( n -- n ) {: slot:n :}   slot RES-CHECK slot RES-OWNER @ ;
: RES-OWNER! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v slot RES-OWNER ! ;
: RES-CONN@ ( n -- n ) {: slot:n :}   slot RES-CHECK slot RES-CONN @ ;
: RES-CONN! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v slot RES-CONN ! ;


\ ---- the call arena -------------------------------------------------------
\ One OS mapping per connection, sized to the call being built and released the
\ moment libpq returns. Parameters are recorded as OFFSETS, never pointers, so
\ growing the arena may move it without invalidating anything already staged.
: ALLOC-ARENA ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;


: RELEASE-ARENA ( ptr u8 n -- )
   MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;


: LARGER ( n n -- n )
   2dup < if nip else drop then ;


\ Doubling, floored at ARENA-MIN and raised to whatever this call needs, so a
\ statement built byte by byte costs a bounded number of mappings. An
\ overflowing double falls through to `want`, which MEM:BYTES-ALLOC-LEN checks.
: ARENA-FIT ( n n -- n ) {: have:n want:n :}
   have 2 * ARENA-MIN LARGER want LARGER ;


: ARENA-RELEASE ( n -- ) {: slot:n :}
   slot ARENA-BASE {: base :}
   base NULL-ADDR? 0= if base slot ARENA-CAP@ RELEASE-ARENA then
   NULL-ARG slot ARENA-BASE!
   0 slot ARENA-CAP!
   0 slot ARENA-U! ;


: ARENA-MOVE ( n n -- ) {: slot:n cap:n :}
   cap ALLOC-ARENA {: fresh :}
   slot ARENA-BASE {: base :}
   base NULL-ADDR? 0= if
      base fresh slot ARENA-U@ BYTE-COPY
      base slot ARENA-CAP@ RELEASE-ARENA
   then
   fresh slot ARENA-BASE!
   cap slot ARENA-CAP! ;


: ARENA-ENSURE ( n n -- ) {: slot:n extra:n :}
   extra 0 < if E-CAPACITY throw then
   slot ARENA-U@ extra + {: want:n :}
   want slot ARENA-CAP@ <= if exit then
   slot slot ARENA-CAP@ want ARENA-FIT ARENA-MOVE ;


: ARENA-BYTE+ ( n n -- ) {: c:n slot:n :}
   slot 1 ARENA-ENSURE
   c slot ARENA-BASE slot ARENA-U@ + c!
   slot ARENA-U@ 1 + slot ARENA-U! ;


\ Copy the bytes plus a terminator and answer the C string's arena offset.
: ARENA-CSTR ( ptr u8 n n -- n ) {: a u:n slot:n :}
   u 0 < if E-CAPACITY throw then
   slot u 1 + ARENA-ENSURE
   slot ARENA-U@ {: off:n :}
   a u slot ARENA-BASE off + FFI:CSTR
   off u + 1 + slot ARENA-U!
   off ;


\ Decimal digits straight into the arena, most significant first.
: ARENA-DIGITS ( n n -- ) {: value:n slot:n :}
   value DEC-BASE < if
      value DIGIT-ZERO + slot ARENA-BYTE+ exit
   then
   value DEC-BASE / slot RECURSE
   value DEC-BASE mod DIGIT-ZERO + slot ARENA-BYTE+ ;


\ STR-MIN-I64 has no positive magnitude, so its digits come from the canonical
\ table rather than a negate that would overflow.
: ARENA-MIN-I64 ( n -- ) {: slot:n :}
   STR-MINUS slot ARENA-BYTE+
   slot STR-I64-DIGITS ARENA-ENSURE
   STR-MIN-I64$ slot ARENA-BASE slot ARENA-U@ + STR-I64-DIGITS BYTE-COPY
   slot ARENA-U@ STR-I64-DIGITS + slot ARENA-U! ;


: ARENA-INT ( n n -- n ) {: value:n slot:n :}
   slot ARENA-U@ {: off:n :}
   value STR-MIN-I64 = if
      slot ARENA-MIN-I64
   else
      value 0 < if STR-MINUS slot ARENA-BYTE+ then
      value 0 < if value negate else value then slot ARENA-DIGITS
   then
   0 slot ARENA-BYTE+
   off ;


: RESET-PARAMS ( n -- ) {: slot:n :}
   0 slot PARAM-N!
   slot ARENA-RELEASE ;


\ ---- the parameter list ---------------------------------------------------
: PARAM-ROOM ( n -- ) {: slot:n :}
   slot PARAM-N@ PARAM-CAP >= if E-CAPACITY throw then ;


: PARAM-OFF+ ( n n -- ) {: off:n slot:n :}
   slot PARAM-N@ {: idx:n :}
   off slot idx PARAM-OFF!
   idx 1 + slot PARAM-N! ;


: PARAM-PTR ( n n -- ptr u8 ) {: slot:n idx:n :}
   slot idx PARAM-OFF@ {: off:n :}
   off NO-OFFSET = if NULL-ARG exit then
   slot ARENA-BASE off + ;


: FILL-PARAM-ARRAY ( n n -- ) {: slot:n off:n :}
   slot ARENA-BASE off + {: arr :}
   slot PARAM-N@ 0 ?do
      slot i PARAM-PTR arr i ptr-field !
   loop ;


\ The `const char *const *` libpq reads, appended last so the pointers it holds
\ are computed against the arena's final base.
: PARAM-ARRAY ( n -- n ) {: slot:n :}
   slot ARENA-U@ CELL 1 - + CELL-MASK and {: off:n :}
   off slot PARAM-N@ cells + {: end:n :}
   slot end slot ARENA-U@ - ARENA-ENSURE
   end slot ARENA-U!
   slot off FILL-PARAM-ARRAY
   off ;


\ ---- handles --------------------------------------------------------------
: PACK-HANDLE ( n n -- n ) {: gen:n slot:n :}
   gen HANDLE-STRIDE * slot + ;


: HANDLE-SLOT ( n -- n ) {: handle:n :}
   handle HANDLE-STRIDE mod ;


: HANDLE-GEN ( n -- n ) {: handle:n :}
   handle HANDLE-STRIDE / ;


\ A process-wide generation survives registry release, so allocating the same
\ slot after image preparation cannot revive a saved handle.
: NEXT-GENERATION ( -- n )
   1 GENERATION atomic-add 1+ dup GENERATION-MAX > if E-CAPACITY throw then ;

: BUMP-CONN-GEN ( n -- ) {: slot:n :}
   NEXT-GENERATION slot CONN-GEN! ;


: BUMP-RES-GEN ( n -- ) {: slot:n :}
   NEXT-GENERATION slot RES-GEN! ;


: CONN-SLOT ( connection -- n ) {: handle:connection :}
   handle CONNECTION>N {: raw:n :}
   raw 0 < if E-HANDLE throw then
   raw HANDLE-SLOT {: slot:n :}
   slot CONN-LIVE@ 0= if E-HANDLE throw then
   slot CONN-GEN@ raw HANDLE-GEN <> if E-HANDLE throw then
   slot CONN-OWNER@ TASK:SELF-N <> if E-HANDLE throw then
   slot ;


: RESULT-SLOT ( result -- n ) {: handle:result :}
   handle RESULT>N {: raw:n :}
   raw 0 < if E-HANDLE throw then
   raw HANDLE-SLOT {: slot:n :}
   slot RES-LIVE@ 0= if E-CLEARED throw then
   slot RES-GEN@ raw HANDLE-GEN <> if E-CLEARED throw then
   slot RES-OWNER@ TASK:SELF-N <> if E-HANDLE throw then
   slot ;


\ The registry is shared by every task, so a slot is claimed by winning the
\ compare-and-swap on its live cell, never by a read-then-write a second task
\ could interleave with. The claim is what publishes the slot as taken; the
\ owner fills the rest afterwards, before any handle for it exists.
: CLAIM-CONN-SLOT ( -- n )
   CONN-CAP 0 ?do
      0 1 i CONN-LIVE-CELL atomic-cas 0= if i unloop exit then
   loop
   E-CAPACITY throw ;


: CLAIM-RES-SLOT ( -- n )
   RESULT-CAP 0 ?do
      0 1 i RES-LIVE-CELL atomic-cas 0= if i unloop exit then
   loop
   E-CAPACITY throw ;


\ ---- application-sized registry -----------------------------------------
\ Reserve once before publishing the registry. DYNAMIC-BUFFER owns allocation,
\ bounds and release, including cleanup after a partially completed reserve.
: RELEASE-REGISTRY ( -- )
   CONN-PG-RELEASE
   CONN-GEN-RELEASE
   CONN-LIVE-RELEASE
   CONN-OWNER-RELEASE
   CONN-TX-RELEASE
   CONN-PARAM-N-RELEASE
   CONN-ARENA-RELEASE
   CONN-ARENA-CAP-RELEASE
   CONN-ARENA-U-RELEASE
   CONN-LAST-RESULT-RELEASE
   CONN-PENDING-RELEASE
   CONN-CONNECTING-RELEASE
   CONN-MESSAGE-U-RELEASE
   CONN-SQLSTATE-U-RELEASE
   CONN-PARAM-OFF-RELEASE
   CONN-VERB-RELEASE
   CONN-MESSAGE-RELEASE
   CONN-SQLSTATE-RELEASE
   RES-PG-RELEASE
   RES-GEN-RELEASE
   RES-LIVE-RELEASE
   RES-OWNER-RELEASE
   RES-CONN-RELEASE
   0 CONN-CAPACITY !
   0 RESULT-CAPACITY !
   0 PARAM-CAPACITY !
   false CONFIGURED ! ;


: ALLOC-REGISTRY ( -- )
   CONN-CAP CONN-PG-RESERVE
   CONN-CAP CONN-GEN-RESERVE
   CONN-CAP CONN-LIVE-RESERVE
   CONN-CAP CONN-OWNER-RESERVE
   CONN-CAP CONN-TX-RESERVE
   CONN-CAP CONN-PARAM-N-RESERVE
   CONN-CAP CONN-ARENA-RESERVE
   CONN-CAP CONN-ARENA-CAP-RESERVE
   CONN-CAP CONN-ARENA-U-RESERVE
   CONN-CAP CONN-LAST-RESULT-RESERVE
   CONN-CAP CONN-PENDING-RESERVE
   CONN-CAP CONN-CONNECTING-RESERVE
   CONN-CAP CONN-MESSAGE-U-RESERVE
   CONN-CAP CONN-SQLSTATE-U-RESERVE
   CONN-CAP PARAM-CAP * CONN-PARAM-OFF-RESERVE
   CONN-CAP VERB-CAP * CELL / CONN-VERB-RESERVE
   CONN-CAP MESSAGE-CAP * CELL / CONN-MESSAGE-RESERVE
   CONN-CAP SQLSTATE-LEN * CELL 1- + CELL / CONN-SQLSTATE-RESERVE
   RESULT-CAP RES-PG-RESERVE
   RESULT-CAP RES-GEN-RESERVE
   RESULT-CAP RES-LIVE-RESERVE
   RESULT-CAP RES-OWNER-RESERVE
   RESULT-CAP RES-CONN-RESERVE
   RESULT-CAP 0 ?do NO-CONN i RES-CONN ! loop ;


\ ---- image capture --------------------------------------------------------
\ A restored image runs in another process: its PGconn and PGresult addresses
\ are gone and its mappings are not its own. Retiring every slot makes the
\ surviving handles refuse instead of reaching a freed pointer, and releases
\ the call arenas the image must not carry. The flag is set only after REGISTER
\ returns, so a throwing registration stays retryable (docs/forth.md rule).
: FORGET-HANDLES ( -- )
   CONFIGURED @ if
      RESULT-CAP 0 ?do
         i RES-LIVE@ 0 <> if i RES-PG@ LIB-CLEAR then
      loop
      CONN-CAP 0 ?do
         i CONN-LIVE@ 0 <> if
            i ARENA-RELEASE
            i DRAIN-LAST@ NULL-ADDR? 0= if i DRAIN-LAST@ LIB-CLEAR then
            i CONN-PG@ LIB-FINISH
         then
      loop
      RELEASE-REGISTRY
   then
   false REGISTERED ! ;


: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: FORGET-HANDLES ;] IMAGE-LIFECYCLE:REGISTER
      true REGISTERED !
   then ;


: CONFIGURE-REGISTRY ( n n n -- )
   {: conns:n results:n params:n :}
   conns 0 <= results 0 <= or params 0 <= or if E-CAPACITY throw then
   conns HANDLE-STRIDE >= results HANDLE-STRIDE >= or if E-CAPACITY throw then
   params MEM-MAX-CELLS conns / > if E-CAPACITY throw then
   CONFIGURED @ if
      conns CONN-CAP = results RESULT-CAP = and
      params PARAM-CAP = and if exit then
      E-CAPACITY throw
   then
   conns CONN-CAPACITY !
   results RESULT-CAPACITY !
   params PARAM-CAPACITY !
   [: ALLOC-REGISTRY REGISTER-CLEANUP ;] catch {: code:n :}
   code 0<> if RELEASE-REGISTRY code throw then
   true CONFIGURED ! ;

: CONFIG-LOCK ( -- ) begin 0 1 CONFIG-MUTEX atomic-cas 0= until ;
: CONFIG-UNLOCK ( -- ) 0 CONFIG-MUTEX atomic! ;


\ ---- slot lifecycle -------------------------------------------------------
: OPEN-CONN-SLOT ( n -- ) {: slot:n :}
   slot BUMP-CONN-GEN
   TASK:SELF-N slot CONN-OWNER!
   NULL-ARG slot DRAIN-LAST!
   0 slot PENDING!
   0 slot CONNECTING!
   0 slot CONN-TX!
   0 slot MESSAGE-U!
   0 slot SQLSTATE-U!
   slot RESET-PARAMS ;


: CONN-HANDLE ( n -- connection ) {: slot:n :}
   slot CONN-GEN@ slot PACK-HANDLE >CONNECTION ;


: RETIRE-CONN-SLOT ( n -- ) {: slot:n :}
   slot ARENA-RELEASE
   NULL-ARG slot CONN-PG!
   NULL-ARG slot DRAIN-LAST!
   0 slot PENDING!
   0 slot CONNECTING!
   0 slot CONN-GEN!
   0 slot CONN-TX!
   0 slot CONN-LIVE! ;


: FILL-RES-SLOT ( ptr u8 n n -- result ) {: pg conn:n slot:n :}
   pg slot RES-PG!
   slot BUMP-RES-GEN
   TASK:SELF-N slot RES-OWNER!
   conn slot RES-CONN!
   slot RES-GEN@ slot PACK-HANDLE >RESULT ;


\ The owning connection is unset BEFORE the slot is published as free, so a
\ concurrent CLOSE scanning for its own results can never match a slot another
\ task has just claimed but not yet filled. NO-CONN is what a free or
\ half-claimed slot reads, which is why the registry starts that way.
: RETIRE-RES-SLOT ( n -- ) {: slot:n :}
   NULL-ARG slot RES-PG!
   0 slot RES-GEN!
   NO-CONN slot RES-CONN!
   0 slot RES-LIVE! ;


: CLOSE-SLOT ( n -- ) {: slot:n :}
   RESULT-CAP 0 ?do
      i RES-LIVE@ 0<> i RES-CONN@ slot = and if
         i RES-PG@ LIB-CLEAR
         i RETIRE-RES-SLOT
      then
   loop
   slot DRAIN-LAST@ NULL-ADDR? 0= if slot DRAIN-LAST@ LIB-CLEAR then
   slot CONN-PG@ LIB-FINISH
   slot RETIRE-CONN-SLOT ;


\ ---- server diagnostics ---------------------------------------------------
: MESSAGE! ( n ptr u8 n -- ) {: slot:n a u:n :}
   u MESSAGE-CAP > if MESSAGE-CAP else u then {: kept:n :}
   a slot MESSAGE-BUF kept BYTE-COPY
   kept slot MESSAGE-U! ;


: MESSAGE$ ( n -- ptr u8 n ) {: slot:n :}
   slot MESSAGE-BUF slot MESSAGE-U@ ;


: SQLSTATE! ( n ptr u8 n -- ) {: slot:n a u:n :}
   u SQLSTATE-LEN > if SQLSTATE-LEN else u then {: kept:n :}
   a slot SQLSTATE-BUF kept BYTE-COPY
   kept slot SQLSTATE-U! ;


: SQLSTATE$ ( n -- ptr u8 n ) {: slot:n :}
   slot SQLSTATE-BUF slot SQLSTATE-U@ ;


: SQLSTATE-FROM ( n ptr u8 -- ) {: slot:n res :}
   res PG-DIAG-SQLSTATE LIB-RESULT-ERROR-FIELD {: p :}
   p NULL-ADDR? if 0 slot SQLSTATE-U! exit then
   slot p CSTR$ SQLSTATE! ;


\ A result that carries no primary message - a connection that died under the
\ query, or a status libpq produced without the server - still has libpq's own
\ connection message, so the failed arm is never silently empty.
: MESSAGE-FROM ( n ptr u8 -- ) {: slot:n res :}
   res PG-DIAG-MESSAGE-PRIMARY LIB-RESULT-ERROR-FIELD {: p :}
   p NULL-ADDR? 0= if slot p CSTR$ MESSAGE! exit then
   slot CONN-PG@ LIB-ERROR-MESSAGE {: q :}
   q NULL-ADDR? if 0 slot MESSAGE-U! exit then
   slot q CSTR$ MESSAGE! ;


: DIAGNOSE ( n ptr u8 -- ) {: slot:n res :}
   slot res SQLSTATE-FROM
   slot res MESSAGE-FROM ;


\ ---- nonblocking progress ------------------------------------------------
: COPY-CONN-MESSAGE ( n -- ) {: slot:n :}
   slot CONN-PG@ LIB-ERROR-MESSAGE {: p :}
   p NULL-ADDR? 0= if slot p CSTR$ MESSAGE! then ;


: FAIL-CONNECTION ( n -- ) {: slot:n :}
   slot COPY-CONN-MESSAGE
   slot CLOSE-SLOT ;


: WAITING ( n n -- progress ) {: slot:n events:n :}
   slot CONN-PG@ LIB-SOCKET C-INT {: socket:n :}
   socket 0 < if slot FAIL-CONNECTION E-EXEC throw then
   socket >FD events PG-PROGRESS:waiting ;


: CONNECT-REFUSED ( n -- progress ) {: slot:n :}
   slot COPY-CONN-MESSAGE
   slot MESSAGE$ {: ma:ptr mu:n :}
   slot CLOSE-SLOT
   ma mu PG-PROGRESS:refused ;


: POLL-CONNECT ( n -- progress ) {: slot:n :}
   \ libpq requires writable readiness before the first PQconnectPoll too.
   slot CONNECTING@ CONNECT-FIRST = if
      slot CONN-PG@ LIB-STATUS C-INT CONNECTION-BAD = if
         slot CONNECT-REFUSED exit
      then
      CONNECT-POLLING slot CONNECTING!
      slot AIO:WRITABLE WAITING exit
   then
   slot CONN-PG@ LIB-CONNECT-POLL C-INT {: status:n :}
   status PGRES-POLLING-READING = if slot AIO:READABLE WAITING exit then
   status PGRES-POLLING-WRITING = if slot AIO:WRITABLE WAITING exit then
   status PGRES-POLLING-OK = if
      0 slot CONNECTING!
      slot CONN-HANDLE PG-PROGRESS:connected exit
   then
   slot CONNECT-REFUSED ;


: ARM-QUERY ( n n -- ) {: slot:n res:n :}
   slot res RES-CONN!
   res 1+ slot PENDING!
   NULL-ARG slot DRAIN-LAST! ;


: QUERY-DONE ( n -- progress ) {: slot:n :}
   slot PENDING@ 1- {: res:n :}
   slot DRAIN-LAST@ {: pg :}
   pg NULL-ADDR? if slot FAIL-CONNECTION E-EXEC throw then
   NULL-ARG slot DRAIN-LAST!
   0 slot PENDING!
   pg slot res FILL-RES-SLOT PG-PROGRESS:completed ;


\ Consume whatever is available, flush without waiting, and drain only while
\ PQisBusy promises PQgetResult cannot block. The last result survives across
\ progress calls; all preceding results are released as a script advances.
: POLL-QUERY ( n -- progress ) {: slot:n :}
   slot CONN-PG@ LIB-CONSUME-INPUT C-INT 0= if
      slot FAIL-CONNECTION E-EXEC throw
   then
   slot CONN-PG@ LIB-FLUSH C-INT {: flushing:n :}
   flushing 0 < if slot FAIL-CONNECTION E-EXEC throw then
   flushing 0<> if slot AIO:READABLE AIO:WRITABLE or WAITING exit then
   begin
      slot CONN-PG@ LIB-IS-BUSY C-INT 0<> if
         slot AIO:READABLE WAITING exit
      then
      slot CONN-PG@ LIB-GET-RESULT {: pg :}
      pg NULL-ADDR? if slot QUERY-DONE exit then
      slot DRAIN-LAST@ {: previous :}
      previous NULL-ADDR? 0= if previous LIB-CLEAR then
      pg slot DRAIN-LAST!
   again ;


: POLL-SLOT ( n -- progress ) {: slot:n :}
   slot CONNECTING@ 0<> if slot POLL-CONNECT exit then
   slot PENDING@ 0= if E-STATEMENT throw then
   slot POLL-QUERY ;


\ Only the convenience words wait. A dispatcher consumes progress directly.
\ Habu's AWAIT parks the calling pthread, so this is not a coroutine suspension.
: WAIT-EVENT ( fd n -- fd n ) {: socket:fd events:n :}
   socket events -1 >MS AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF E-EXEC throw ENDOF
      cancelled OF E-EXEC throw ENDOF
      refused OF drop E-EXEC throw ENDOF
   ;MATCH
   socket events ;


: WAIT-PROGRESS ( n -- progress ) {: slot:n :}
   begin
      slot POLL-SLOT MATCH progress
         waiting OF
            [: WAIT-EVENT ;] catch {: code:n :} 2drop   \ the waiting payload is stale after a caught throw and unused
            code 0<> if slot CLOSE-SLOT code throw then
         ENDOF
         connected OF PG-PROGRESS:connected exit ENDOF
         completed OF PG-PROGRESS:completed exit ENDOF
         refused OF PG-PROGRESS:refused exit ENDOF
      ;MATCH
   again ;


: WAIT-QUERY ( n -- result )
   WAIT-PROGRESS MATCH progress
      completed OF ENDOF
      connected OF drop E-EXEC throw ENDOF
      refused OF 2drop E-EXEC throw ENDOF
      waiting OF 2drop E-EXEC throw ENDOF
   ;MATCH ;


\ ---- statement execution --------------------------------------------------
\ The result slot is claimed BEFORE libpq runs, so a full registry refuses
\ without ever leaving a PGresult nobody can clear, and the arena is released
\ the instant libpq returns - it has copied everything into its own message by
\ then. A throw before that leaves the arena for the next PARAMS or CLOSE.
: CLAIM-STATEMENT ( n -- n n ) {: slot:n :}
   slot PARAM-ARRAY CLAIM-RES-SLOT ;


: SEND-FAILED ( n n -- ) {: slot:n res:n :}
   res RETIRE-RES-SLOT
   slot COPY-CONN-MESSAGE
   E-EXEC throw ;


: EXEC-START ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot IDLE-CHECK
   u 0 <= if E-STATEMENT throw then
   a u slot ARENA-CSTR {: sql-off:n :}
   slot CLAIM-STATEMENT {: arr-off:n res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base sql-off + slot PARAM-N@ NULL-ARG base arr-off +
   NULL-ARG NULL-ARG TEXT-RESULT LIB-SEND-QUERY-PARAMS C-INT {: sent:n :}
   sent 0= if slot res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


: PREPARED-START ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot IDLE-CHECK
   u 0 <= if E-STATEMENT throw then
   a u slot ARENA-CSTR {: name-off:n :}
   slot CLAIM-STATEMENT {: arr-off:n res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base name-off + slot PARAM-N@ base arr-off +
   NULL-ARG NULL-ARG TEXT-RESULT LIB-SEND-QUERY-PREPARED C-INT {: sent:n :}
   sent 0= if slot res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


: PREPARE-START ( n ptr u8 n ptr u8 n -- ) {: slot:n na nu:n sa su:n :}
   slot IDLE-CHECK
   nu 0 <= if E-STATEMENT throw then
   su 0 <= if E-STATEMENT throw then
   na nu slot ARENA-CSTR {: name-off:n :}
   sa su slot ARENA-CSTR {: sql-off:n :}
   CLAIM-RES-SLOT {: res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base name-off + base sql-off + 0 NULL-ARG LIB-SEND-PREPARE C-INT {: sent:n :}
   sent 0= if slot res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


\ The simple-query protocol. It takes no parameters at all, so a pending
\ parameter list is a caller error with nowhere to go: refusing it by name
\ beats dropping the values the caller believes it bound.
: SCRIPT-START ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot IDLE-CHECK
   u 0 <= if E-STATEMENT throw then
   slot PARAM-N@ 0 <> if E-STATEMENT throw then
   a u slot ARENA-CSTR {: off:n :}
   CLAIM-RES-SLOT {: res:n :}
   slot CONN-PG@ slot ARENA-BASE off + LIB-SEND-QUERY C-INT {: sent:n :}
   sent 0= if slot res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


: RESULT-STATUS ( n -- n ) {: slot:n :}
   slot RES-PG@ LIB-RESULT-STATUS C-INT ;


: CLEAR-SLOT ( n -- ) {: slot:n :}
   slot RES-PG@ LIB-CLEAR
   slot RETIRE-RES-SLOT ;


\ ---- transactions ---------------------------------------------------------
\ BEGIN, COMMIT and ROLLBACK are this module's own fixed statements and take no
\ parameters, so they run out of a small per-connection buffer and never touch
\ the arena. That is what lets a caller build parameters BEFORE
\ WITH-TRANSACTION and spend them inside the body, which is the only way into a
\ quotation that may not read the caller's locals.
: VERB-TEXT! ( n ptr u8 n -- ptr u8 ) {: slot:n a u:n :}
   u 0 <= if E-STATEMENT throw then
   u VERB-CAP 1 - > if E-CAPACITY throw then
   a u slot VERB-BUF FFI:CSTR
   slot VERB-BUF ;


: RUN-VERB ( n ptr u8 n -- n ) {: slot:n a u:n :}
   slot IDLE-CHECK
   slot a u VERB-TEXT! {: sql :}
   CLAIM-RES-SLOT {: res:n :}
   slot CONN-PG@ sql 0 NULL-ARG NULL-ARG NULL-ARG NULL-ARG TEXT-RESULT
   LIB-SEND-QUERY-PARAMS C-INT {: sent:n :}
   sent 0= if slot res SEND-FAILED then
   slot res ARM-QUERY
   slot WAIT-QUERY RESULT-SLOT ;


\ A transaction verb must succeed or the caller's framing is a lie, so a server
\ refusal is E-EXEC rather than an outcome the caller could ignore.
: VERB ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot a u RUN-VERB {: res:n :}
   res RESULT-STATUS {: status:n :}
   status PGRES-COMMAND-OK <> if
      slot res RES-PG@ DIAGNOSE
      res CLEAR-SLOT
      E-EXEC throw
   then
   res CLEAR-SLOT ;


: BEGIN-TX ( n -- ) {: slot:n :}
   slot s" BEGIN" VERB
   1 slot CONN-TX! ;


: COMMIT-TX ( n -- ) {: slot:n :}
   0 slot CONN-TX!
   slot s" COMMIT" VERB ;


: ROLLBACK-TX ( n -- ) {: slot:n :}
   0 slot CONN-TX!
   slot s" ROLLBACK" VERB ;


\ ---- column access --------------------------------------------------------
: CELL-AT ( n row col -- n n n ) {: slot:n r:row c:col :}
   slot RES-PG@ {: res :}
   r ROW>N {: ri:n :}
   c COL>N {: ci:n :}
   ri 0 < if E-COLUMN throw then
   ci 0 < if E-COLUMN throw then
   ri res LIB-NTUPLES C-INT >= if E-COLUMN throw then
   ci res LIB-NFIELDS C-INT >= if E-COLUMN throw then
   slot ri ci ;


: VALUE$ ( n n n -- ptr u8 n ) {: slot:n ri:n ci:n :}
   slot RES-PG@ {: res :}
   res ri ci LIB-GETVALUE
   res ri ci LIB-GETLENGTH C-INT ;


: CELL-NULL? ( n n n -- bool ) {: slot:n ri:n ci:n :}
   slot RES-PG@ ri ci LIB-GETISNULL C-INT 0 <> ;


: CMD-COUNT ( n -- n ) {: slot:n :}
   slot RES-PG@ LIB-CMD-TUPLES CSTR$ {: a u:n :}
   u 0= if 0 exit then
   a u STR>NUMBER? MATCH option
      none OF E-TYPE throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ ---- the outcome ADT ------------------------------------------------------
: CLASSIFY ( n n -- outcome ) {: slot:n res:n :}
   res RESULT-STATUS {: status:n :}
   status PGRES-COMMAND-OK = if PG-OUTCOME:ok exit then
   status PGRES-TUPLES-OK = if PG-OUTCOME:rows exit then
   slot res RES-PG@ DIAGNOSE
   slot SQLSTATE$ slot MESSAGE$ PG-OUTCOME:failed ;


\ ---- platform -------------------------------------------------------------
: INIT ( -- )
   HB-TARGET-LINUX? 0= if E-PLATFORM throw then ;


public

\ CONFIGURE is the one registry sizing decision. Applications call it before
\ CONNECT; repeating the same declaration is harmless. A process cannot change
\ its declaration after use: a different one is refused with E-CAPACITY.
: CONFIGURE ( n n n -- )
   CONFIG-LOCK [: CONFIGURE-REGISTRY ;] [: CONFIG-UNLOCK ;] finally ;

\ CONNECT-START hands back an attempt owned by the calling task. POLL advances
\ it to connected or refused; CLOSE abandons it at any earlier point.
\ The slot is claimed first, so the conninfo it stages and the message a
\ refusal copies back are its own and no concurrent CONNECT shares them. A
\ refusal releases the slot, and those message bytes stay readable until the
\ next CONNECT takes it.
: CONNECT-START ( ptr u8 n -- connection ) {: a u:n :}
   INIT
   REGISTER-CLEANUP
   CLAIM-CONN-SLOT {: slot:n :}
   slot OPEN-CONN-SLOT
   a u slot ARENA-CSTR {: off:n :}
   slot ARENA-BASE off + LIB-CONNECT-START {: pg :}
   slot ARENA-RELEASE
   pg NULL-ADDR? if slot RETIRE-CONN-SLOT E-CONNECT throw then
   pg slot CONN-PG!
   pg 1 LIB-SET-NONBLOCKING C-INT 0<> if
      slot FAIL-CONNECTION
      E-CONNECT throw
   then
   CONNECT-FIRST slot CONNECTING!
   slot CONN-HANDLE ;


: POLL ( connection -- progress )
   CONN-SLOT POLL-SLOT ;


: AWAIT ( connection -- progress )
   CONN-SLOT WAIT-PROGRESS ;


: CONNECT ( ptr u8 n -- connect-result )
   CONNECT-START AWAIT MATCH progress
      connected OF PG-CONNECT--RESULT:connected ENDOF
      refused OF PG-CONNECT--RESULT:refused ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF drop E-CONNECT throw ENDOF
   ;MATCH ;


\ CLOSE clears whatever results the connection still owns, so libpq keeps no
\ orphan PGresult, then releases the call arena and retires the handle.
: CLOSE ( connection -- ) {: handle:connection :}
   handle CONN-SLOT CLOSE-SLOT ;


\ PARAMS empties the connection's parameter list and releases its call arena;
\ TEXT+, INT+ and NULL+ append in $1, $2, ... order. EXEC and EXEC-PREPARED
\ consume the list and leave it empty, so no call inherits another's parameters.
: PARAMS ( connection -- )
   CONN-SLOT dup IDLE-CHECK RESET-PARAMS ;


: TEXT+ ( connection ptr u8 n -- ) {: handle:connection a u:n :}
   handle CONN-SLOT {: slot:n :}
   slot IDLE-CHECK
   slot PARAM-ROOM
   a u slot ARENA-CSTR slot PARAM-OFF+ ;


: INT+ ( connection n -- ) {: handle:connection value:n :}
   handle CONN-SLOT {: slot:n :}
   slot IDLE-CHECK
   slot PARAM-ROOM
   value slot ARENA-INT slot PARAM-OFF+ ;


: NULL+ ( connection -- ) {: handle:connection :}
   handle CONN-SLOT {: slot:n :}
   slot IDLE-CHECK
   slot PARAM-ROOM
   NO-OFFSET slot PARAM-OFF+ ;


: SEND ( connection ptr u8 n -- ) {: handle:connection a u:n :}
   handle CONN-SLOT a u EXEC-START ;


: SEND-SCRIPT ( connection ptr u8 n -- ) {: handle:connection a u:n :}
   handle CONN-SLOT a u SCRIPT-START ;


: SEND-PREPARE ( connection ptr u8 n ptr u8 n -- )
   {: handle:connection na nu:n sa su:n :}
   handle CONN-SLOT na nu sa su PREPARE-START ;


: SEND-PREPARED ( connection ptr u8 n -- ) {: handle:connection a u:n :}
   handle CONN-SLOT a u PREPARED-START ;


: EXEC ( connection ptr u8 n -- result ) {: handle:connection a u:n :}
   handle a u SEND
   handle CONN-SLOT WAIT-QUERY ;


\ SCRIPT runs a whole script - a migration file, several statements in one
\ text - through the simple-query protocol, which is the only protocol that
\ takes more than one statement: the extended protocol EXEC rides answers a
\ second command with 42601. libpq returns the LAST statement's result and a
\ failing statement abandons the rest, so the outcome ADT covers it unchanged.
\ EXEC stays the default, because it is the one that takes parameters.
: SCRIPT ( connection ptr u8 n -- result ) {: handle:connection a u:n :}
   handle a u SEND-SCRIPT
   handle CONN-SLOT WAIT-QUERY ;


: PREPARE ( connection ptr u8 n ptr u8 n -- result )
   {: handle:connection na nu:n sa su:n :}
   handle na nu sa su SEND-PREPARE
   handle CONN-SLOT WAIT-QUERY ;


: EXEC-PREPARED ( connection ptr u8 n -- result ) {: handle:connection a u:n :}
   handle a u SEND-PREPARED
   handle CONN-SLOT WAIT-QUERY ;


\ The body receives the connection and returns it, which is what lets it cross
\ the catch boundary. A throw rolls back and reaches the caller unchanged.
: WITH-TRANSACTION ( connection [ connection -- connection ] -- )
   {: handle:connection body :}
   handle CONN-SLOT {: slot:n :}
   slot CONN-TX@ 0 <> if E-TRANSACTION throw then
   slot BEGIN-TX
   handle body catch {: leftover:connection code:n :}
   code 0= if slot COMMIT-TX exit then
   slot ROLLBACK-TX
   code throw ;


: OUTCOME ( result -- outcome ) {: handle:result :}
   handle RESULT-SLOT {: res:n :}
   res RES-CONN@ res CLASSIFY ;


: ROWS ( result -- count )
   RESULT-SLOT RES-PG@ LIB-NTUPLES C-INT >COUNT ;


: COLS ( result -- count )
   RESULT-SLOT RES-PG@ LIB-NFIELDS C-INT >COUNT ;


: AFFECTED ( result -- count )
   RESULT-SLOT CMD-COUNT >COUNT ;


: NAME$ ( result col -- ptr u8 n ) {: handle:result c:col :}
   handle RESULT-SLOT RES-PG@ {: res :}
   c COL>N {: ci:n :}
   ci 0 < if E-COLUMN throw then
   ci res LIB-NFIELDS C-INT >= if E-COLUMN throw then
   res ci LIB-FNAME {: p :}
   p NULL-ADDR? if E-COLUMN throw then
   p CSTR$ ;


\ The span is libpq's own bytes inside the result and stays valid until CLEAR.
: TEXT$ ( result row col -- ptr u8 n ) {: handle:result r:row c:col :}
   handle RESULT-SLOT r c CELL-AT VALUE$ ;


: NULL? ( result row col -- bool ) {: handle:result r:row c:col :}
   handle RESULT-SLOT r c CELL-AT CELL-NULL? ;


: INT ( result row col -- n ) {: handle:result r:row c:col :}
   handle RESULT-SLOT r c CELL-AT {: slot:n ri:n ci:n :}
   slot ri ci CELL-NULL? if E-TYPE throw then
   slot ri ci VALUE$ STR>NUMBER? MATCH option
      none OF E-TYPE throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ CLEAR is the result owner's single consumption point: the slot retires, so
\ this handle and every copy of it refuse afterwards with E-CLEARED.
: CLEAR ( result -- )
   RESULT-SLOT CLEAR-SLOT ;

;package
