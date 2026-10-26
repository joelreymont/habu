\ pg.f - PostgreSQL over libpq, bound through the FFI FUNCTION: declarer.
\
\ A connection is a nominal handle and a result a linear PG:result token, never
\ a raw cell: each names a slot in this module's registry, which holds the
\ libpq pointer, the task that owns it and a generation. A result lives in a
\ scope: WITH-RESULT, WITH-SCRIPT, WITH-PREPARE, WITH-PREPARED and
\ WITH-DESCRIPTION run one operation, hand the body its token and clear the
\ result when the body returns or throws, so no result outlives its scope. The
\ body threads the token through the accessors and hands it back. The one
\ unscoped token is the `completed` progress POLL or AWAIT answers, which CLEAR
\ consumes. The generation makes a stale handle refuse with E-CLEARED instead
\ of reaching a freed PGresult. The same registry carries the concurrency rule
\ (docs/design.md "Models taken from other Forths"): a connection and its
\ results belong to the task that created them and a handle presented by any
\ other task is E-HANDLE. A task that needs two
\ databases opens two connections.
\
\ STORAGE CLASS. PROCESS-WIDE registry sized by CONFIGURE. The same declaration
\ again is harmless from any task; a different one is refused until image
\ preparation releases the registry. The parameter list, call arena and
\ diagnostic buffers belong to each connection.
\
\ CLOSE also clears the connection's results. Closing a pending query first
\ sends a native cancel, which may block up to the connection's connect_timeout
\ (with none set, PG imposes no deadline) and cannot undo SQL already completed;
\ idle and connecting handles send none. ABANDON is the close that sends no
\ cancel and waits for nothing. LIVE? tells the owning task whether its handle
\ is still the slot's live generation, so cleanup closes only what lives.
\
\ CONNECT-START, SEND and POLL let one task drive many connections: the caller
\ owns readiness and deadlines (libpq ignores connect_timeout while polling),
\ registers the descriptor each POLL answers afresh, and uses a Unix socket or
\ a numeric hostaddr so DNS cannot block startup. CONNECT and the WITH- scopes
\ wait through AIO:AWAIT and need AIO:START. One operation may be pending per
\ connection; a send copies its statement and parameters. A
\ transport failure closes the connection and throws E-EXEC; a SQL rejection is
\ the result's `failed` outcome with SQLSTATE and message.
\
\ OUTCOME's `failed` spans, TEXT$ and NAME$ answer libpq's memory inside the
\ result: valid until its scope ends (CLEAR for a progress token) or its
\ connection closes, whichever comes first, so a body copies what it keeps. A
\ close under the body leaves the token answering E-CLEARED. CONNECT's
\ `refused` text lasts until the next CONNECT. A task ended by TASK:PAUSE under
\ HALT runs no `finally` cleanup, so its open result scopes are reclaimed only
\ when that ending unwinds.
\
\ WITH-DESCRIPTION prepares a text as the unnamed statement and describes it without
\ running it: the result answers COLS, NAME$ and OID (the column's type)
\ and has no rows. A SQL rejection is its `failed` outcome, as for WITH-RESULT.
\ It takes no parameters (a pending list is E-STATEMENT) and replaces the
\ connection's unnamed statement; only WITH-DESCRIPTION names that statement,
\ since WITH-PREPARE and WITH-PREPARED refuse an empty name.
\
\ DEADLINE! bounds every later WITH- scope's operation and transaction verb on
\ the connection: each operation's whole wait ends that many milliseconds after
\ it starts, and 0, the default, sets no bound; a bound
\ outside 0..$7FFFFFFF ms is E-BOUND. DEADLINE@ answers the bound the last
\ DEADLINE! set, 0 if none. CONNECT and AWAIT are not bounded. On
\ expiry the connection closes and the operation throws E-DEADLINE. While a
\ connection's bound is nonzero, every close but an explicit CLOSE and image
\ preparation (FORGET-HANDLES), which still send the native cancel - expiry, a
\ transport or readiness failure, an exception caught while waiting, a failed
\ ROLLBACK under a body's throw - finishes it WITHOUT the cancel, as ABANDON
\ does whatever the bound, since a cancel to an unreachable server would
\ stretch the bound, and the primary exception stands. The server may go on
\ running the abandoned statement: its own statement_timeout bounds that work.
\ A WITH-TRANSACTION whose body closed the connection rethrows without a
\ ROLLBACK, and one whose body returned on a closed connection is E-HANDLE. One
\ whose ROLLBACK fails after the body threw closes the connection, whose
\ transaction state is then unknown, and rethrows the body's code.
\
\ ID answers a connection's identity as a plain cell and OF-ID the connection
\ it names, so a cleanup that takes no arguments can keep the identity in
\ per-task memory. Validity is checked by use: a closed connection's identity
\ resolves to a handle LIVE? answers false for and every verb refuses.
\
\ WITH-RESULT runs exactly one statement on the extended protocol. WITH-SCRIPT
\ runs several on the simple-query protocol, which takes no parameters (a
\ pending list is E-STATEMENT) and runs them as one implicit transaction: a
\ failing statement rolls back the earlier ones. WITH-RESULT consumes the
\ parameter list; BEGIN, COMMIT
\ and ROLLBACK leave it untouched, which is how a WITH-TRANSACTION body carries
\ its caller's values in. A nested WITH-TRANSACTION is E-TRANSACTION, since
\ PostgreSQL ignores a nested BEGIN and the inner COMMIT would end the outer
\ transaction.
\
\ Handles do not survive image capture: preparation retires every slot and
\ frees its libpq resources and arenas. Stop the AIO loop before capture (a
\ live task at PREPARE ends the process) and CONFIGURE again in the new one.
\
\ Statement and parameter text have no fixed ceiling. Each connection builds one
\ call in an arena it allocates from src/lib/memory.f, sized to that call: the
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

require src/lib/errors.f
require src/lib/ffi-abi.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/type/deftype.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/image-lifecycle.f
require src/lib/fork-child.f

\ PostgreSQL over libpq: -9250..-9259, the decade after libcurl. Minted here,
\ beside the package its codes belong to (docs/forth-card.md § 6).
-9250 constant E-PG-FIRST
-9259 constant E-PG-LAST
\ That block is full, so PG's later codes take a second, separately named
\ region, -9560..-9569, the decade after OS-CPU (src/lib/errors.f).
-9560 constant E-PG-GROWTH-FIRST
-9569 constant E-PG-GROWTH-LAST

package PG

public

-9250 constant E-CONNECT      \ the server refused the conninfo, or libpq could not build a connection
-9251 constant E-EXEC         \ transport/readiness failure, no result, rejected transaction verb or failed cancel
-9252 constant E-COLUMN       \ a row or column index outside the result
-9253 constant E-TYPE         \ a column read as a type its bytes are not, including an integer read of NULL
-9254 constant E-CLEARED      \ a result used after CLEAR or its connection's close
-9255 constant E-TRANSACTION  \ a WITH-TRANSACTION inside another one on the same connection
-9256 constant E-HANDLE       \ a handle another task owns or an image restore invalidated, a scope's token cleared or not handed back
-9257 constant E-CAPACITY     \ more live connections, results or parameters than this module stores
-9258 constant E-PLATFORM     \ this module is qualified on Linux only, and refuses a foreign target before it binds
-9259 constant E-STATEMENT    \ an empty statement/name or embedded NUL in C-string text
-9560 constant E-DEADLINE     \ a wait past the connection's DEADLINE! bound, which closed it
-9561 constant E-BOUND        \ a DEADLINE! bound outside 0..WAIT-MAX ms

NEWTYPE connection
DEFLINEAR PG:result

DEFTYPE ROW
DEFTYPE COL

SUMTYPE connect-result
   VARIANT connected connection ;VARIANT
   VARIANT refused ptr u8 n ;VARIANT
;SUMTYPE

\ The failure spans are the server's SQLSTATE and its primary message, read in
\ place from the result's own bytes: they live exactly as long as TEXT$.
SUMTYPE outcome
   VARIANT ok ;VARIANT
   VARIANT rows ;VARIANT
   VARIANT failed ptr u8 n ptr u8 n ;VARIANT
;SUMTYPE

\ A dispatcher registers the waiting descriptor and event mask with its own
\ loop, then calls POLL again. No progress step waits for socket readiness.
SUMTYPE progress
   VARIANT waiting fd n ;VARIANT
   VARIANT connected connection ;VARIANT
   VARIANT completed PG:result ;VARIANT
   VARIANT refused ptr u8 n ;VARIANT
;SUMTYPE

private

CAST: >CONNECTION ( n -- connection )
CAST: CONNECTION>N ( connection -- n )
LINEAR: N>RESULT ( n -- PG:result )
LINEAR: RESULT>N ( PG:result -- n )

\ ---- libpq facts, read off /usr/include/libpq-fe.h and postgres_ext.h ------
0 constant PGRES-POLLING-FAILED           \ PostgresPollingStatusType
1 constant PGRES-POLLING-READING
2 constant PGRES-POLLING-WRITING
3 constant PGRES-POLLING-OK
1 constant CONNECTION-BAD
1 constant CONNECT-FIRST
2 constant CONNECT-POLLING
0 constant TRANS-IDLE                     \ PGTransactionStatusType
1 constant PGRES-COMMAND-OK               \ ExecStatusType
2 constant PGRES-TUPLES-OK
$43 constant PG-DIAG-SQLSTATE             \ 'C'
$4D constant PG-DIAG-MESSAGE-PRIMARY      \ 'M'
0 constant TEXT-RESULT                    \ PQexecParams resultFormat

\ ---- registry storage -----------------------------------------------------
\ Connection, result and parameter counts belong to the application. There is
\ no process-wide guessed connection ceiling: CONFIGURE allocates exactly the
\ registries the caller declares, and a live registry cannot be resized.
$10 constant VERB-CAP                     \ BEGIN / COMMIT / ROLLBACK, this module's own
$1000 constant ARENA-MIN                  \ smallest call arena worth mapping
$100000000 constant HANDLE-STRIDE         \ low 32 bits name the registry slot
$7FFFFFFF constant GENERATION-MAX         \ positive signed handle, high 31 bits
$FFFFFFFFFFFFFFF8 constant CELL-MASK      \ round an arena offset up to a cell
-1 constant NO-OFFSET                     \ a NULL parameter has no arena bytes
-1 constant NO-CONN                       \ RES-CONN of a free or half-claimed slot
10 constant DEC-BASE
$30 constant DIGIT-ZERO
0 constant NO-DEADLINE                    \ an operation on a connection with no bound
1000000 constant NS-PER-MS
$7FFFFFFF constant WAIT-MAX               \ the longest bound, in ms: the longest a readiness poll takes

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
DYNAMIC-BUFFER CONN-PARAM-OFF n
DYNAMIC-BUFFER CONN-VERB n
DYNAMIC-BUFFER CONN-MESSAGE ptr u8
DYNAMIC-BUFFER CONN-MESSAGE-CAP n
DYNAMIC-BUFFER CONN-BOUND n

DYNAMIC-BUFFER RES-PG ptr u8
DYNAMIC-BUFFER RES-GEN n
DYNAMIC-BUFFER RES-LIVE n
DYNAMIC-BUFFER RES-OWNER n
DYNAMIC-BUFFER RES-CONN n
DYNAMIC-BUFFER RES-OUTER n
DYNAMIC-BUFFER RES-SCOPED n

TYPED-VARIABLE REGISTERED bool

false CONFIGURED !
false REGISTERED !

here data-base - negate 7 and allot
variable GENERATION
variable CONFIG-MUTEX
\ The packed handle of the running task's innermost result scope ("result scopes").
TASK:#USER 7 + CELL-MASK and CELL TASK:+USER SCOPE-HEAD drop

: CONN-CAP ( -- n ) CONN-CAPACITY @ ;
: RESULT-CAP ( -- n ) RESULT-CAPACITY @ ;
: PARAM-CAP ( -- n ) PARAM-CAPACITY @ ;


\ ---- the declared libpq bindings ------------------------------------------
\ Every pointer is `ptr u8`: a PGconn, a PGresult and a C string are all
\ foreign-owned addresses this side only passes back or reads with the width
\ libpq states. None is written by the callee, so none carries an extent.
VERSIONED-LIBRARY pq 5

FUNCTION: LIB-CONNECT-START PQconnectStart ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-CONNECT-POLL PQconnectPoll ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-STATUS PQstatus ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-TRANSACTION-STATUS PQtransactionStatus ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-SET-NONBLOCKING PQsetnonblocking ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-SOCKET PQsocket ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-ERROR-MESSAGE PQerrorMessage ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-FINISH PQfinish ( ptr u8 -- ) ;FUNCTION
FUNCTION: LIB-CANCEL-CREATE PQcancelCreate ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-CANCEL-BLOCKING PQcancelBlocking ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-CANCEL-FINISH PQcancelFinish ( ptr u8 -- ) ;FUNCTION
FUNCTION: LIB-SEND-QUERY PQsendQuery ( ptr u8 ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-SEND-QUERY-PARAMS PQsendQueryParams
   ( ptr u8 ptr u8 n ptr u8 ptr u8 ptr u8 ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-SEND-PREPARE PQsendPrepare
   ( ptr u8 ptr u8 ptr u8 n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-SEND-QUERY-PREPARED PQsendQueryPrepared
   ( ptr u8 ptr u8 n ptr u8 ptr u8 ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-SEND-DESCRIBE-PREPARED PQsendDescribePrepared
   ( ptr u8 ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-FLUSH PQflush ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-CONSUME-INPUT PQconsumeInput ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-IS-BUSY PQisBusy ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-GET-RESULT PQgetResult ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-RESULT-STATUS PQresultStatus ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-RESULT-ERROR-FIELD PQresultErrorField ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-RESULT-ERROR-MESSAGE PQresultErrorMessage ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-NTUPLES PQntuples ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-NFIELDS PQnfields ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-FNAME PQfname ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-FTYPE PQftype ( ptr u8 n -- u32 ) ;FUNCTION
FUNCTION: LIB-FTABLE PQftable ( ptr u8 n -- u32 ) ;FUNCTION
FUNCTION: LIB-FTABLECOL PQftablecol ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-GETVALUE PQgetvalue ( ptr u8 n n -- ptr u8 ) ;FUNCTION
FUNCTION: LIB-GETISNULL PQgetisnull ( ptr u8 n n -- i32 ) ;FUNCTION
FUNCTION: LIB-GETLENGTH PQgetlength ( ptr u8 n n -- i32 ) ;FUNCTION
FUNCTION: LIB-CLEAR PQclear ( ptr u8 -- ) ;FUNCTION
FUNCTION: LIB-CMD-TUPLES PQcmdTuples ( ptr u8 -- ptr u8 ) ;FUNCTION


\ ---- foreign addresses ----------------------------------------------------
: NULL-ARG ( -- ptr u8 )
   NULL-PTR BYTE-VIEW ;


: NULL-ADDR? ( ptr u8 -- bool ) {: p :}
   p FFI:>CELL 0= ;


\ libpq gives its strings no extent: each ends at its NUL.
: CSTR$ ( ptr u8 -- ptr u8 n )
   dup ZLEN ;


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

: MESSAGE-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot CONN-CHECK
   slot CONN-MESSAGE ;

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
: MESSAGE-BASE ( n -- ptr u8 )   MESSAGE-CELL @ ;
: MESSAGE-BASE! ( ptr u8 n -- ) {: p slot:n :}   p slot MESSAGE-CELL ! ;

\ The generation publishes the slot's owner: written with release after the
\ owner and read with acquire before it, so a reader that sees a generation
\ sees the owner stored with it.
: CONN-GEN@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-GEN atomic@ ;
: CONN-GEN! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-GEN atomic! ;
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
: MESSAGE-CAP@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-MESSAGE-CAP @ ;
: MESSAGE-CAP! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-MESSAGE-CAP ! ;
: BOUND@ ( n -- n ) {: slot:n :}   slot CONN-CHECK slot CONN-BOUND @ ;
: BOUND! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v slot CONN-BOUND ! ;

: VERB-BUF ( n -- ptr u8 ) {: slot:n :}   slot CONN-CHECK 0 CONN-VERB BYTE-VIEW slot VERB-CAP * + ;

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
: RES-OUTER@ ( n -- n ) {: slot:n :}   slot RES-CHECK slot RES-OUTER @ ;
: RES-OUTER! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v slot RES-OUTER ! ;
: RES-SCOPED@ ( n -- n ) {: slot:n :}   slot RES-CHECK slot RES-SCOPED @ ;
: RES-SCOPED! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v slot RES-SCOPED ! ;


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


\ The bytes wanted are compared with the room left, and with what a cell can
\ still count, before they join the fill: added first, a length near the maximum
\ cell wraps the sum under the capacity and the arena is never grown.
: ARENA-ENSURE ( n n -- ) {: slot:n extra:n :}
   extra 0 < if E-CAPACITY throw then
   extra slot ARENA-CAP@ slot ARENA-U@ - <= if exit then
   extra MEM-MAX-N slot ARENA-U@ - > if E-CAPACITY throw then
   slot slot ARENA-CAP@ slot ARENA-U@ extra + ARENA-FIT ARENA-MOVE ;


: ARENA-BYTE+ ( n n -- ) {: c:n slot:n :}
   slot 1 ARENA-ENSURE
   c slot ARENA-BASE slot ARENA-U@ + c!
   slot ARENA-U@ 1 + slot ARENA-U! ;


\ Copy text plus a terminator and answer the C string's arena offset. libpq
\ ignores text span lengths, so an embedded NUL would silently truncate it.
\ Reserve first: an impossible length must fail without reading source bytes.
: ARENA-CSTR ( ptr u8 n n -- n ) {: a u:n slot:n :}
   u 0 < if E-CAPACITY throw then
   slot u 1 + ARENA-ENSURE
   u 0 ?do a i + c@ 0= if E-STATEMENT throw then loop
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


\ Generation 0 marks a slot no handle names: free, or claimed and not yet
\ opened. OF-ID can build any raw cell, so a handle below one stride is refused.
: CONN-SLOT ( connection -- n ) {: handle:connection :}
   handle CONNECTION>N {: raw:n :}
   raw HANDLE-STRIDE < if E-HANDLE throw then
   raw HANDLE-SLOT {: slot:n :}
   slot CONN-LIVE@ 0= if E-HANDLE throw then
   slot CONN-GEN@ raw HANDLE-GEN <> if E-HANDLE throw then
   slot CONN-OWNER@ TASK:SELF-N <> if E-HANDLE throw then
   slot ;


\ A scoped result whose connection closed keeps its slot, with no PGresult,
\ until its scope ends.
: RESULT-SLOT ( n -- n ) {: raw:n :}
   raw 0 < if E-HANDLE throw then
   raw HANDLE-SLOT {: slot:n :}
   slot RES-LIVE@ 0= if E-CLEARED throw then
   slot RES-GEN@ raw HANDLE-GEN <> if E-CLEARED throw then
   slot RES-OWNER@ TASK:SELF-N <> if E-HANDLE throw then
   slot RES-PG@ NULL-ADDR? if E-CLEARED throw then
   slot ;


\ The registry is shared by every task, so a slot is claimed by winning the
\ compare-and-swap on its live cell, never by a read-then-write a second task
\ could interleave with. The claim is what publishes the slot as taken; the
\ owner fills the rest afterwards; until it sets the generation no handle
\ matches the slot.
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


\ ---- the message copy -----------------------------------------------------
\ Each connection keeps its own copy of the last server message, as long as the
\ server sent it, in one OS mapping that grows to the longest message so far. It
\ outlives CLOSE, since CONNECT's `refused` text is read after the close, and
\ goes with the registry, image preparation included.
: MESSAGE-RELEASE ( n -- ) {: slot:n :}
   slot MESSAGE-BASE {: base :}
   base NULL-ADDR? 0= if base slot MESSAGE-CAP@ RELEASE-ARENA then
   NULL-ARG slot MESSAGE-BASE!
   0 slot MESSAGE-CAP!
   0 slot MESSAGE-U! ;


\ The old copy is never read again, so growth maps the new size and drops the
\ old mapping without moving its bytes.
: MESSAGE-ROOM ( n n -- ) {: slot:n want:n :}
   want slot MESSAGE-CAP@ <= if exit then
   want ALLOC-ARENA {: fresh :}
   slot MESSAGE-RELEASE
   fresh slot MESSAGE-BASE!
   want slot MESSAGE-CAP! ;


\ Only a registry CONFIGURE published holds copies: a partial reserve made none.
: RELEASE-MESSAGES ( -- )
   CONFIGURED @ 0= if exit then
   CONN-CAP 0 ?do i MESSAGE-RELEASE loop ;


\ ---- application-sized registry -----------------------------------------
\ Reserve once before publishing the registry. DYNAMIC-BUFFER owns allocation,
\ bounds and release, including cleanup after a partially completed reserve.
: RELEASE-REGISTRY ( -- )
   RELEASE-MESSAGES
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
   CONN-PARAM-OFF-RELEASE
   CONN-VERB-RELEASE
   CONN-MESSAGE-RELEASE
   CONN-MESSAGE-CAP-RELEASE
   CONN-BOUND-RELEASE
   RES-PG-RELEASE
   RES-GEN-RELEASE
   RES-LIVE-RELEASE
   RES-OWNER-RELEASE
   RES-CONN-RELEASE
   RES-OUTER-RELEASE
   RES-SCOPED-RELEASE
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
   CONN-CAP PARAM-CAP * CONN-PARAM-OFF-RESERVE
   CONN-CAP VERB-CAP * CELL / CONN-VERB-RESERVE
   CONN-CAP CONN-MESSAGE-RESERVE
   CONN-CAP CONN-MESSAGE-CAP-RESERVE
   CONN-CAP CONN-BOUND-RESERVE
   RESULT-CAP RES-PG-RESERVE
   RESULT-CAP RES-GEN-RESERVE
   RESULT-CAP RES-LIVE-RESERVE
   RESULT-CAP RES-OWNER-RESERVE
   RESULT-CAP RES-CONN-RESERVE
   RESULT-CAP RES-OUTER-RESERVE
   RESULT-CAP RES-SCOPED-RESERVE
   RESULT-CAP 0 ?do NO-CONN i RES-CONN ! loop ;


\ PQfinish alone lets a blocked statement execute after the client exits.
\ Cancel only an outstanding operation on this connection, before closing it.
\ libpq 17+ preserves the connection's encryption and timeout options. Dispatch
\ cannot undo a statement the server already completed.
: DISPATCH-CANCEL ( ptr u8 -- ptr u8 )
   dup LIB-CANCEL-BLOCKING 0= if E-EXEC throw then ;


: CANCEL-PENDING ( n -- n )
   {: slot:n :}
   slot PENDING@ 0= if slot exit then
   slot CONN-PG@ LIB-CANCEL-CREATE {: cancel :}
   cancel NULL-ADDR? if E-EXEC throw then
   cancel ['] DISPATCH-CANCEL catch {: held code:n :}
   cancel LIB-CANCEL-FINISH
   code 0<> if code throw then
   slot ;


\ ---- image capture --------------------------------------------------------
\ A restored image runs in another process: its PGconn and PGresult addresses
\ are gone and its mappings are not its own. Retiring every slot makes the
\ surviving handles refuse instead of reaching a freed pointer, and releases
\ the call arenas the image must not carry. The flag is set only after REGISTER
\ returns, so a throwing registration stays retryable
\ (`src/lib/image-lifecycle.f PREPARE`).
: FORGET-HANDLES ( -- )
   0
   CONFIGURED @ if
      \ Cancel every pending statement before closing any connection: closing
      \ an earlier lock holder first could let a later pending write commit.
      CONN-CAP 0 ?do
         i CONN-LIVE@ 0 <> if
            i ['] CANCEL-PENDING catch nip
            over 0= if nip else drop then
         then
      loop
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
   0 SCOPE-HEAD !
   false REGISTERED !
   dup 0<> if throw then drop ;


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

\ A process just forked frees the lock before anything else (src/lib/fork-child.f):
\ fork copies only the thread that called it, so a CONFIGURE another task was in
\ leaves the child's copy held with nobody to free it, and the child's own
\ CONFIGURE would wait for ever (src/lib/pg-fork-test.f).
: REGISTER-CHILD-RESET ( -- )
   [: CONFIG-UNLOCK ;] FORK-CHILD:REGISTER ;
REGISTER-CHILD-RESET


\ ---- slot lifecycle -------------------------------------------------------
\ The owner is stored before the generation a handle must match (CONN-GEN!), so
\ no handle matches the slot while the previous owner still stands.
: OPEN-CONN-SLOT ( n -- ) {: slot:n :}
   TASK:SELF-N slot CONN-OWNER!
   slot BUMP-CONN-GEN
   NULL-ARG slot DRAIN-LAST!
   0 slot PENDING!
   0 slot CONNECTING!
   0 slot CONN-TX!
   0 slot MESSAGE-U!
   0 slot BOUND!
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


: FILL-RES-SLOT ( ptr u8 n n -- n ) {: pg conn:n slot:n :}
   pg slot RES-PG!
   slot BUMP-RES-GEN
   TASK:SELF-N slot RES-OWNER!
   conn slot RES-CONN!
   slot RES-GEN@ slot PACK-HANDLE ;


\ The owning connection is unset BEFORE the slot is published as free, so a
\ concurrent CLOSE scanning for its own results can never match a slot another
\ task has just claimed but not yet filled. NO-CONN is what a free or
\ half-claimed slot reads, which is why the registry starts that way.
: RETIRE-RES-SLOT ( n -- ) {: slot:n :}
   NULL-ARG slot RES-PG!
   0 slot RES-GEN!
   NO-CONN slot RES-CONN!
   0 slot RES-OUTER!
   0 slot RES-SCOPED!
   0 slot RES-LIVE! ;


\ A scoped slot is retired only by its scope, which keeps the task's chain of
\ open scopes sound: a close under the body frees the PGresult and leaves the
\ slot to the scope, where every later read is E-CLEARED.
: DETACH-RES-SLOT ( n -- ) {: slot:n :}
   slot RES-PG@ LIB-CLEAR
   NULL-ARG slot RES-PG!
   NO-CONN slot RES-CONN! ;


\ Clears the connection's results and finishes it. A pending statement gets no
\ cancel here: CLOSE-SLOT sends one first; ABANDON never does, nor does
\ ABANDON-SLOT on a bounded connection.
: FINISH-SLOT ( n -- )
   {: slot:n :}
   RESULT-CAP 0 ?do
      i RES-LIVE@ 0<> i RES-CONN@ slot = and if
         i RES-SCOPED@ 0<> if
            i DETACH-RES-SLOT
         else
            i RES-PG@ LIB-CLEAR
            i RETIRE-RES-SLOT
         then
      then
   loop
   slot DRAIN-LAST@ NULL-ADDR? 0= if slot DRAIN-LAST@ LIB-CLEAR then
   slot CONN-PG@ LIB-FINISH
   slot RETIRE-CONN-SLOT ;


: CLOSE-SLOT ( n -- n )
   {: slot:n :}
   slot ['] CANCEL-PENDING catch nip {: code:n :}
   slot FINISH-SLOT
   code ;


\ A close the caller did not ask for, made under another failure that stays
\ primary. A bounded connection gets no native cancel, which could block on an
\ unreachable server past the bound.
: ABANDON-SLOT ( n -- )
   {: slot:n :}
   slot BOUND@ 0<> if slot FINISH-SLOT exit then
   slot CLOSE-SLOT drop ;


\ ---- server diagnostics ---------------------------------------------------
: MESSAGE! ( n ptr u8 n -- ) {: slot:n a u:n :}
   0 slot MESSAGE-U!
   slot u MESSAGE-ROOM
   a slot MESSAGE-BASE u BYTE-COPY
   u slot MESSAGE-U! ;


: MESSAGE$ ( n -- ptr u8 n ) {: slot:n :}
   slot MESSAGE-BASE slot MESSAGE-U@ ;


\ A failed result names its SQLSTATE and primary message in its own bytes. One
\ libpq made without the server - a connection that died under the query -
\ carries no primary message but holds the connection's message, copied in when
\ libpq built it, so the failed arm is never silently empty.
: FIELD$ ( ptr u8 -- ptr u8 n ) {: p :}
   p NULL-ADDR? if NULL-ARG 0 exit then
   p CSTR$ ;


: FAILED-SPANS ( ptr u8 -- ptr u8 n ptr u8 n ) {: res :}
   res PG-DIAG-SQLSTATE LIB-RESULT-ERROR-FIELD FIELD$
   res PG-DIAG-MESSAGE-PRIMARY LIB-RESULT-ERROR-FIELD {: p :}
   p NULL-ADDR? 0= if p CSTR$ exit then
   res LIB-RESULT-ERROR-MESSAGE FIELD$ ;


\ ---- nonblocking progress ------------------------------------------------
\ Only a refused CONNECT writes the connection's message copy.
: COPY-CONN-MESSAGE ( n -- ) {: slot:n :}
   slot CONN-PG@ LIB-ERROR-MESSAGE {: p :}
   p NULL-ADDR? 0= if slot p CSTR$ MESSAGE! then ;


: WAITING ( n n -- progress ) {: slot:n events:n :}
   slot CONN-PG@ LIB-SOCKET {: socket:n :}
   socket 0 < if slot ABANDON-SLOT E-EXEC throw then
   socket >FD events PG-PROGRESS:waiting ;


: CONNECT-REFUSED ( n -- progress ) {: slot:n :}
   slot COPY-CONN-MESSAGE
   slot MESSAGE$ {: ma:ptr mu:n :}
   slot CLOSE-SLOT drop
   ma mu PG-PROGRESS:refused ;


: POLL-CONNECT ( n -- progress ) {: slot:n :}
   \ libpq requires writable readiness before the first PQconnectPoll too.
   slot CONNECTING@ CONNECT-FIRST = if
      slot CONN-PG@ LIB-STATUS CONNECTION-BAD = if
         slot CONNECT-REFUSED exit
      then
      CONNECT-POLLING slot CONNECTING!
      slot AIO:WRITABLE WAITING exit
   then
   slot CONN-PG@ LIB-CONNECT-POLL {: status:n :}
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
   pg NULL-ADDR? if slot ABANDON-SLOT E-EXEC throw then
   NULL-ARG slot DRAIN-LAST!
   0 slot PENDING!
   pg slot res FILL-RES-SLOT N>RESULT PG-PROGRESS:completed ;


\ Consume whatever is available, flush without waiting, and drain only while
\ PQisBusy promises PQgetResult cannot block. The last result survives across
\ progress calls; all preceding results are released as a script advances.
: POLL-QUERY ( n -- progress ) {: slot:n :}
   slot CONN-PG@ LIB-CONSUME-INPUT 0= if
      slot ABANDON-SLOT E-EXEC throw
   then
   slot CONN-PG@ LIB-FLUSH {: flushing:n :}
   flushing 0 < if slot ABANDON-SLOT E-EXEC throw then
   flushing 0<> if slot AIO:READABLE AIO:WRITABLE or WAITING exit then
   begin
      slot CONN-PG@ LIB-IS-BUSY 0<> if
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


\ ---- wait deadlines -------------------------------------------------------
\ An operation's deadline on the monotonic clock, fixed when it starts so one
\ bound covers every wait the operation makes.
: OP-DEADLINE ( n -- n )
   {: slot:n :}
   slot BOUND@ {: bound:n :}
   bound 0= if NO-DEADLINE exit then
   mono-ns bound NS-PER-MS * + ;


\ The poll timeout left, rounded up to a whole millisecond so the poll cannot
\ end before the deadline. A deadline already passed is E-DEADLINE.
: REMAINING ( n -- ms )
   {: deadline:n :}
   deadline NO-DEADLINE = if -1 >MS exit then
   deadline mono-ns - {: left:n :}
   left 0 <= if E-DEADLINE throw then
   left NS-PER-MS 1- + NS-PER-MS / >MS ;


\ Only the convenience words wait. A dispatcher consumes progress directly.
\ Habu's AWAIT parks the calling pthread, so this is not a coroutine suspension.
: WAIT-EVENT ( fd n n -- fd n n )
   {: socket:fd events:n deadline:n :}
   socket events deadline REMAINING AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF deadline NO-DEADLINE = if E-EXEC else E-DEADLINE then throw ENDOF
      cancelled OF E-EXEC throw ENDOF
      refused OF drop E-EXEC throw ENDOF
   ;MATCH
   socket events deadline ;


: WAIT-PROGRESS ( n n -- progress )
   {: slot:n deadline:n :}
   begin
      slot POLL-SLOT MATCH progress
         waiting OF
            deadline [: WAIT-EVENT ;] catch {: code:n :} 2drop drop   \ the waiting payload is stale after a caught throw and unused
            code 0<> if slot ABANDON-SLOT code throw then
         ENDOF
         connected OF PG-PROGRESS:connected exit ENDOF
         completed OF PG-PROGRESS:completed exit ENDOF
         refused OF PG-PROGRESS:refused exit ENDOF
      ;MATCH
   again ;


: WAIT-QUERY ( n n -- n )
   WAIT-PROGRESS MATCH progress
      completed OF RESULT>N ENDOF
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


: SEND-FAILED ( n -- ) {: res:n :}
   res RETIRE-RES-SLOT
   E-EXEC throw ;


: EXEC-START ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot IDLE-CHECK
   u 0 <= if E-STATEMENT throw then
   a u slot ARENA-CSTR {: sql-off:n :}
   slot CLAIM-STATEMENT {: arr-off:n res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base sql-off + slot PARAM-N@ NULL-ARG base arr-off +
   NULL-ARG NULL-ARG TEXT-RESULT LIB-SEND-QUERY-PARAMS {: sent:n :}
   sent 0= if res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


: PREPARED-START ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot IDLE-CHECK
   u 0 <= if E-STATEMENT throw then
   a u slot ARENA-CSTR {: name-off:n :}
   slot CLAIM-STATEMENT {: arr-off:n res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base name-off + slot PARAM-N@ base arr-off +
   NULL-ARG NULL-ARG TEXT-RESULT LIB-SEND-QUERY-PREPARED {: sent:n :}
   sent 0= if res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


\ The empty name is the unnamed statement, which only DESCRIBE sends.
: PREPARE-SEND ( n ptr u8 n ptr u8 n -- )
   {: slot:n na nu:n sa su:n :}
   slot IDLE-CHECK
   su 0 <= if E-STATEMENT throw then
   na nu slot ARENA-CSTR {: name-off:n :}
   sa su slot ARENA-CSTR {: sql-off:n :}
   CLAIM-RES-SLOT {: res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base name-off + base sql-off + 0 NULL-ARG LIB-SEND-PREPARE {: sent:n :}
   sent 0= if res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


: PREPARE-START ( n ptr u8 n ptr u8 n -- )
   {: slot:n na nu:n sa su:n :}
   nu 0 <= if E-STATEMENT throw then
   slot na nu sa su PREPARE-SEND ;


: DESCRIBE-START ( n -- )
   {: slot:n :}
   slot IDLE-CHECK
   s" " slot ARENA-CSTR {: name-off:n :}
   CLAIM-RES-SLOT {: res:n :}
   slot CONN-PG@ slot ARENA-BASE name-off + LIB-SEND-DESCRIBE-PREPARED {: sent:n :}
   sent 0= if res SEND-FAILED then
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
   slot CONN-PG@ slot ARENA-BASE off + LIB-SEND-QUERY {: sent:n :}
   sent 0= if res SEND-FAILED then
   slot res ARM-QUERY
   slot RESET-PARAMS ;


: RESULT-STATUS ( n -- n ) {: slot:n :}
   slot RES-PG@ LIB-RESULT-STATUS ;


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


: RUN-VERB ( n ptr u8 n -- n )
   {: slot:n a u:n :}
   slot OP-DEADLINE {: deadline:n :}
   slot IDLE-CHECK
   slot a u VERB-TEXT! {: sql :}
   CLAIM-RES-SLOT {: res:n :}
   slot CONN-PG@ sql 0 NULL-ARG NULL-ARG NULL-ARG NULL-ARG TEXT-RESULT
   LIB-SEND-QUERY-PARAMS {: sent:n :}
   sent 0= if res SEND-FAILED then
   slot res ARM-QUERY
   slot deadline WAIT-QUERY RESULT-SLOT ;


\ A transaction verb must succeed or the caller's framing is a lie, so a server
\ refusal is E-EXEC rather than an outcome the caller could ignore.
: VERB ( n ptr u8 n -- ) {: slot:n a u:n :}
   slot a u RUN-VERB {: res:n :}
   res RESULT-STATUS {: status:n :}
   status PGRES-COMMAND-OK <> if
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
   ri res LIB-NTUPLES >= if E-COLUMN throw then
   ci res LIB-NFIELDS >= if E-COLUMN throw then
   slot ri ci ;


: VALUE$ ( n n n -- ptr u8 n ) {: slot:n ri:n ci:n :}
   slot RES-PG@ {: res :}
   res ri ci LIB-GETVALUE
   res ri ci LIB-GETLENGTH ;


: CELL-NULL? ( n n n -- bool ) {: slot:n ri:n ci:n :}
   slot RES-PG@ ri ci LIB-GETISNULL 0 <> ;


\ A column of the result's descriptor: the PGresult and the column index.
: COLUMN-AT ( n col -- ptr u8 n )
   {: slot:n c:col :}
   slot RES-PG@ {: res :}
   c COL>N {: ci:n :}
   ci 0 < if E-COLUMN throw then
   ci res LIB-NFIELDS >= if E-COLUMN throw then
   res ci ;


: CMD-COUNT ( n -- n ) {: slot:n :}
   slot RES-PG@ LIB-CMD-TUPLES CSTR$ {: a u:n :}
   u 0= if 0 exit then
   a u STR>NUMBER? MATCH option
      none OF E-TYPE throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ ---- the outcome ADT ------------------------------------------------------
: CLASSIFY ( n -- outcome ) {: res:n :}
   res RESULT-STATUS {: status:n :}
   status PGRES-COMMAND-OK = if PG-OUTCOME:ok exit then
   status PGRES-TUPLES-OK = if PG-OUTCOME:rows exit then
   res RES-PG@ FAILED-SPANS PG-OUTCOME:failed ;


\ ---- result scopes --------------------------------------------------------
\ The token does not survive a throw out of the body, so the scope keeps its
\ packed handle in the running task's SCOPE-HEAD cell, where the cleanup reads
\ it. A nested scope saves the cell in RES-OUTER under its own slot and its
\ cleanup puts it back, so each cleanup retires its own result. Zero is no
\ scope: a packed handle is at least one HANDLE-STRIDE, and image preparation,
\ which releases every slot, zeroes the cell of the task that runs it.

: SCOPE-ENTER ( n -- n ) {: raw:n :}
   raw HANDLE-SLOT {: slot:n :}
   SCOPE-HEAD @ slot RES-OUTER!
   1 slot RES-SCOPED!
   raw SCOPE-HEAD !
   raw ;


\ PQclear of the NULL a close under the body left is a no-op.
: SCOPE-LEAVE ( -- )
   SCOPE-HEAD @ {: raw:n :}
   raw 0= if exit then
   raw HANDLE-SLOT {: slot:n :}
   slot RES-OUTER@ SCOPE-HEAD !
   slot RES-PG@ LIB-CLEAR
   slot RETIRE-RES-SLOT ;


\ The body hands back the token it was given; any other is refused.
: RUN-SCOPED ( R PG:result [ R PG:result -- S PG:result ] -- S )
   execute RESULT>N SCOPE-HEAD @ <> if E-HANDLE throw then ;


: RESULT-SCOPE ( R n [ R PG:result -- S PG:result ] -- S ) {: raw:n body :}
   raw SCOPE-ENTER N>RESULT body [: RUN-SCOPED ;] [: SCOPE-LEAVE ;] finally ;


\ The accessors resolve the token's slot and hand the token back.
: PEEK ( PG:result -- PG:result n )
   RESULT>N dup RESULT-SLOT swap N>RESULT swap ;


\ A caught staging refusal still owns the claimed slot. The new arena starts
\ at offset zero, so the staging helper can preserve its catch input row.
: STAGE-CONNINFO ( ptr u8 n n -- ptr u8 n n ) {: a u:n slot:n :}
   a u slot ARENA-CSTR drop
   a u slot ;


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
   REGISTER-CLEANUP
   CLAIM-CONN-SLOT {: slot:n :}
   slot OPEN-CONN-SLOT
   a u slot ['] STAGE-CONNINFO catch {: code:n :}
   2drop drop
   code 0<> if slot RETIRE-CONN-SLOT code throw then
   slot ARENA-BASE LIB-CONNECT-START {: pg :}
   slot ARENA-RELEASE
   pg NULL-ADDR? if slot RETIRE-CONN-SLOT E-CONNECT throw then
   pg slot CONN-PG!
   pg 1 LIB-SET-NONBLOCKING 0<> if
      slot ABANDON-SLOT
      E-CONNECT throw
   then
   CONNECT-FIRST slot CONNECTING!
   slot CONN-HANDLE ;


: POLL ( connection -- progress )
   CONN-SLOT POLL-SLOT ;


: AWAIT ( connection -- progress )
   CONN-SLOT NO-DEADLINE WAIT-PROGRESS ;


: CONNECT ( ptr u8 n -- connect-result )
   CONNECT-START AWAIT MATCH progress
      connected OF PG-CONNECT--RESULT:connected ENDOF
      refused OF PG-CONNECT--RESULT:refused ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF RESULT>N RESULT-SLOT CLEAR-SLOT E-CONNECT throw ENDOF
   ;MATCH ;


\ A task retaining a CONNECT-START handle may need to clean it up after AWAIT
\ or POLL retired it internally. Check the registry before reading a slot: image
\ preparation releases those buffers. A reused slot has a different generation.
: LIVE? ( connection -- bool ) {: handle:connection :}
   CONFIGURED @ 0= if false exit then
   handle CONNECTION>N {: raw:n :}
   raw HANDLE-STRIDE < if false exit then
   raw HANDLE-SLOT {: slot:n :}
   slot CONN-CAP >= if false exit then
   slot CONN-LIVE@ 0= if false exit then
   slot CONN-GEN@ raw HANDLE-GEN <> if false exit then
   slot CONN-OWNER@ TASK:SELF-N <> if E-HANDLE throw then
   true ;


: ID ( connection -- n )   CONNECTION>N ;
: OF-ID ( n -- connection )   >CONNECTION ;


\ IDLE? is libpq's own view of the session, so it covers a transaction opened
\ by plain BEGIN text as well as WITH-TRANSACTION. It is false while a command
\ is in flight, inside a transaction block and after a failed statement there.
: IDLE? ( connection -- bool )
   CONN-SLOT CONN-PG@ LIB-TRANSACTION-STATUS TRANS-IDLE = ;


\ CLOSE cancels pending SQL, clears owned results and retires the handle even
\ when dispatch failed. An explicit close reports that failure after cleanup;
\ an internal close during AWAIT keeps the original wait/transport exception.
: CLOSE ( connection -- ) {: handle:connection :}
   handle CONN-SLOT CLOSE-SLOT
   dup 0<> if throw then drop ;


\ ABANDON closes without the cancel and without waiting on the network: the
\ pending statement, if any, runs on the server to its end or its
\ statement_timeout. For a cleanup under HALT or a failure that must not
\ wait on the server, whatever the connection's bound.
: ABANDON ( connection -- )
   CONN-SLOT FINISH-SLOT ;


\ PARAMS empties the connection's parameter list and releases its call arena;
\ TEXT+, INT+ and NULL+ append in $1, $2, ... order. WITH-RESULT and
\ WITH-PREPARED consume the list and leave it empty, so no call inherits
\ another's parameters.
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


\ Each WITH- word runs one operation and the body over its result, then clears
\ the result whether the body returns or throws. The body hands the token back.
: WITH-RESULT ( R connection ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: handle:connection a u:n body :}
   handle CONN-SLOT {: slot:n :}
   slot OP-DEADLINE {: deadline:n :}
   slot a u EXEC-START
   slot deadline WAIT-QUERY body RESULT-SCOPE ;


\ WITH-SCRIPT runs a whole script - a migration file, several statements in
\ one text - through the simple-query protocol, which is the only protocol that
\ takes more than one statement: the extended protocol WITH-RESULT rides
\ answers a second command with 42601. libpq returns the LAST statement's
\ result and a failing statement abandons the rest, so the outcome ADT covers
\ it unchanged. WITH-RESULT stays the default, because it takes parameters.
: WITH-SCRIPT ( R connection ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: handle:connection a u:n body :}
   handle CONN-SLOT {: slot:n :}
   slot OP-DEADLINE {: deadline:n :}
   slot a u SCRIPT-START
   slot deadline WAIT-QUERY body RESULT-SCOPE ;


: WITH-PREPARE ( R connection ptr u8 n ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: handle:connection na nu:n sa su:n body :}
   handle CONN-SLOT {: slot:n :}
   slot OP-DEADLINE {: deadline:n :}
   slot na nu sa su PREPARE-START
   slot deadline WAIT-QUERY body RESULT-SCOPE ;


: WITH-PREPARED ( R connection ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: handle:connection a u:n body :}
   handle CONN-SLOT {: slot:n :}
   slot OP-DEADLINE {: deadline:n :}
   slot a u PREPARED-START
   slot deadline WAIT-QUERY body RESULT-SCOPE ;


\ Two round trips under one deadline: the prepare, then the describe. A
\ rejected prepare is the body's result, with its `failed` outcome.
: WITH-DESCRIPTION ( R connection ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: handle:connection a u:n body :}
   handle CONN-SLOT {: slot:n :}
   slot OP-DEADLINE {: deadline:n :}
   slot PARAM-N@ 0 <> if E-STATEMENT throw then
   slot s" " a u PREPARE-SEND
   slot deadline WAIT-QUERY {: prepared:n :}
   prepared RESULT-SLOT {: res:n :}
   res RESULT-STATUS PGRES-COMMAND-OK <> if
      prepared body RESULT-SCOPE
   else
      res CLEAR-SLOT
      slot DESCRIBE-START
      slot deadline WAIT-QUERY body RESULT-SCOPE
   then ;


\ The bound covers each later operation from its start; one in flight keeps
\ the deadline it started with.
: DEADLINE! ( connection ms -- )
   {: handle:connection bound:ms :}
   handle CONN-SLOT {: slot:n :}
   bound MS>N {: v:n :}
   v 0 < v WAIT-MAX > or if E-BOUND throw then
   v slot BOUND! ;


\ The bound the last DEADLINE! set, so a caller can restore it.
: DEADLINE@ ( connection -- ms )
   {: handle:connection :}
   handle CONN-SLOT BOUND@ >MS ;


private

\ The ROLLBACK under a body's throw, which stays primary: a ROLLBACK that fails
\ leaves the transaction's state unknown, so the connection is abandoned. The
\ handle, not the slot, is checked after it, since a ROLLBACK that expired has
\ already closed the connection and freed the slot for another task.
: ABORT-TX ( connection -- )
   {: handle:connection :}
   handle LIVE? 0= if exit then
   handle CONN-SLOT [: dup ROLLBACK-TX ;] catch {: stale:n code:n :}
   code 0= if exit then
   handle LIVE? if handle CONN-SLOT ABANDON-SLOT then ;

public


\ The body receives the connection and returns it, which is what lets it cross
\ the catch boundary. A throw rolls back and reaches the caller unchanged. The
\ handle is resolved again after the body, which may have closed the connection
\ and let another CONNECT reuse its slot: a closed one gets no ROLLBACK, and a
\ COMMIT on one is E-HANDLE.
: WITH-TRANSACTION ( connection [ connection -- connection ] -- )
   {: handle:connection body :}
   handle CONN-SLOT {: slot:n :}
   slot CONN-TX@ 0 <> if E-TRANSACTION throw then
   slot BEGIN-TX
   handle body catch {: leftover:connection code:n :}
   code 0= if handle CONN-SLOT COMMIT-TX exit then
   handle ABORT-TX
   code throw ;


: OUTCOME ( PG:result -- PG:result outcome )
   PEEK CLASSIFY ;


: ROWS ( PG:result -- PG:result count )
   PEEK RES-PG@ LIB-NTUPLES >COUNT ;


: COLS ( PG:result -- PG:result count )
   PEEK RES-PG@ LIB-NFIELDS >COUNT ;


: AFFECTED ( PG:result -- PG:result count )
   PEEK CMD-COUNT >COUNT ;


: NAME$ ( PG:result col -- PG:result ptr u8 n ) {: c:col :}
   PEEK c COLUMN-AT LIB-FNAME {: p :}
   p NULL-ADDR? if E-COLUMN throw then
   p CSTR$ ;


\ The column's type OID, as pg_type numbers it.
: OID ( PG:result col -- PG:result u32 ) {: c:col :}
   PEEK c COLUMN-AT LIB-FTYPE ;


\ The OID of the column's source table; 0 unless it is a plain table column.
: TABLE ( PG:result col -- PG:result u32 ) {: c:col :}
   PEEK c COLUMN-AT LIB-FTABLE ;


\ The column's attnum in its TABLE; 0 unless it is a plain table column.
: TABLE-COL ( PG:result col -- PG:result n ) {: c:col :}
   PEEK c COLUMN-AT LIB-FTABLECOL ;


\ The span is libpq's own bytes inside the result, valid until its scope ends,
\ its CLEAR or its connection's close.
: TEXT$ ( PG:result row col -- PG:result ptr u8 n ) {: r:row c:col :}
   PEEK r c CELL-AT VALUE$ ;


: NULL? ( PG:result row col -- PG:result bool ) {: r:row c:col :}
   PEEK r c CELL-AT CELL-NULL? ;


: INT ( PG:result row col -- PG:result n ) {: r:row c:col :}
   PEEK r c CELL-AT {: slot:n ri:n ci:n :}
   slot ri ci CELL-NULL? if E-TYPE throw then
   slot ri ci VALUE$ STR>NUMBER? MATCH option
      none OF E-TYPE throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ CLEAR consumes the `completed` token POLL or AWAIT answered. A scope's token
\ goes back to its scope, which clears it; CLEAR of one is E-HANDLE.
: CLEAR ( PG:result -- )
   RESULT>N RESULT-SLOT {: slot:n :}
   slot RES-SCOPED@ 0<> if E-HANDLE throw then
   slot CLEAR-SLOT ;

;package
