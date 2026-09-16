\ pq.f - PostgreSQL over libpq, bound through the FFI FUNCTION: declarer.
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
\ capture all release it too. Only COUNTS are bounded, by named constants with
\ E-CAPACITY at the boundary.
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
require lib/image-lifecycle.f

package DB

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

private

CAST: >CONNECTION ( n -- connection )
CAST: CONNECTION>N ( connection -- n )
CAST: >RESULT ( n -- result )
CAST: RESULT>N ( result -- n )

\ ---- libpq facts, read off /usr/include/libpq-fe.h and postgres_ext.h ------
0 constant CONNECTION-OK                  \ ConnStatusType
1 constant PGRES-COMMAND-OK               \ ExecStatusType
2 constant PGRES-TUPLES-OK
$43 constant PG-DIAG-SQLSTATE             \ 'C'
$4D constant PG-DIAG-MESSAGE-PRIMARY      \ 'M'
0 constant TEXT-RESULT                    \ PQexecParams resultFormat

\ ---- registry storage -----------------------------------------------------
\ Every ceiling here is a COUNT. Statement and parameter text are bounded only
\ by the memory the call arena can map.
$08 constant CONN-CAP
$20 constant RESULT-CAP
$20 constant PARAM-CAP
$0400 constant MESSAGE-CAP                \ copied primary server message
$05 constant SQLSTATE-LEN
$10 constant VERB-CAP                     \ BEGIN / COMMIT / ROLLBACK, this module's own
MESSAGE-CAP constant SCAN-CAP             \ bound on a NUL scan of a libpq C string
$1000 constant ARENA-MIN                  \ smallest call arena worth mapping
$0100 constant HANDLE-STRIDE              \ handle = generation * HANDLE-STRIDE + slot
$FFFFFFFFFFFFFFF8 constant CELL-MASK      \ round an arena offset up to a cell
-1 constant NO-OFFSET                     \ a NULL parameter has no arena bytes
-1 constant NO-CONN                       \ RES-CONN of a free or half-claimed slot
10 constant DEC-BASE
$30 constant DIGIT-ZERO

create CONN-PG CONN-CAP cells allot
create CONN-GEN CONN-CAP cells allot
create CONN-LIVE CONN-CAP cells allot
create CONN-OWNER CONN-CAP cells allot
create CONN-TX CONN-CAP cells allot
create CONN-PARAM-N CONN-CAP cells allot
create CONN-ARENA CONN-CAP cells allot
create CONN-ARENA-CAP CONN-CAP cells allot
create CONN-ARENA-U CONN-CAP cells allot
create CONN-MESSAGE-U CONN-CAP cells allot
create CONN-SQLSTATE-U CONN-CAP cells allot
create CONN-PARAM-OFF CONN-CAP PARAM-CAP * cells allot
create CONN-VERB CONN-CAP VERB-CAP * allot
create CONN-MESSAGE CONN-CAP MESSAGE-CAP * allot
create CONN-SQLSTATE CONN-CAP SQLSTATE-LEN * allot

create RES-PG RESULT-CAP cells allot
create RES-GEN RESULT-CAP cells allot
create RES-LIVE RESULT-CAP cells allot
create RES-OWNER RESULT-CAP cells allot
create RES-CONN RESULT-CAP cells allot

variable REGISTERED


\ ---- the declared libpq bindings ------------------------------------------
\ Every pointer is `ptr u8`: a PGconn, a PGresult and a C string are all
\ foreign-owned addresses this side only passes back or reads with the width
\ libpq states. None is written by the callee, so none carries an extent.
LIBRARY libpq.so.5

FUNCTION: PQ-CONNECTDB PQconnectdb ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-STATUS PQstatus ( ptr u8 -- n ) ;FUNCTION
FUNCTION: PQ-ERROR-MESSAGE PQerrorMessage ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-FINISH PQfinish ( ptr u8 -- ) ;FUNCTION
FUNCTION: PQ-EXEC-PARAMS PQexecParams
   ( ptr u8 ptr u8 n ptr u8 ptr u8 ptr u8 ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-PREPARE PQprepare ( ptr u8 ptr u8 ptr u8 n ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-EXEC-PREPARED PQexecPrepared
   ( ptr u8 ptr u8 n ptr u8 ptr u8 ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-RESULT-STATUS PQresultStatus ( ptr u8 -- n ) ;FUNCTION
FUNCTION: PQ-RESULT-ERROR-FIELD PQresultErrorField ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-NTUPLES PQntuples ( ptr u8 -- n ) ;FUNCTION
FUNCTION: PQ-NFIELDS PQnfields ( ptr u8 -- n ) ;FUNCTION
FUNCTION: PQ-FNAME PQfname ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-GETVALUE PQgetvalue ( ptr u8 n n -- ptr u8 ) ;FUNCTION
FUNCTION: PQ-GETISNULL PQgetisnull ( ptr u8 n n -- n ) ;FUNCTION
FUNCTION: PQ-GETLENGTH PQgetlength ( ptr u8 n n -- n ) ;FUNCTION
FUNCTION: PQ-CLEAR PQclear ( ptr u8 -- ) ;FUNCTION
FUNCTION: PQ-CMD-TUPLES PQcmdTuples ( ptr u8 -- ptr u8 ) ;FUNCTION


\ ---- foreign scalars and addresses ----------------------------------------
\ A C `int` comes back in the low half of x0 and the high half is unspecified.
: C-INT ( n -- n )
   $FFFFFFFF and dup $80000000 and 0 <> if $100000000 - then ;


: NULL-ARG ( -- ptr u8 )
   NULL-PTR BYTE-VIEW ;


: NULL-ADDR? ( ptr u8 -- bool ) {: p :}
   p FFI:>CELL 0= ;


\ A libpq C string states no length, so the scan stops at the NUL or at
\ SCAN-CAP, whichever comes first, and never reads on. Every caller here reads
\ a diagnostic or an identifier, so a longer string is truncated rather than
\ refused: losing the tail of a server message beats losing the message.
: CSTR-LEN ( ptr u8 -- n ) {: p :}
   SCAN-CAP 0 ?do
      p i + c@ 0= if i unloop exit then
   loop
   SCAN-CAP ;


: CSTR$ ( ptr u8 -- ptr u8 n ) {: p :}
   p p CSTR-LEN ;


\ ---- slot storage ---------------------------------------------------------
: CONN-CHECK ( n -- ) {: slot:n :}
   slot 0 < if E-HANDLE throw then
   slot CONN-CAP >= if E-HANDLE throw then ;


: CONN-PG-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot CONN-CHECK
   CONN-PG BYTE-VIEW slot ptr-field ;


: CONN-PG@ ( n -- ptr u8 )   CONN-PG-CELL @ ;
: CONN-PG! ( ptr u8 n -- ) {: p slot:n :}   p slot CONN-PG-CELL ! ;

: ARENA-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot CONN-CHECK
   CONN-ARENA BYTE-VIEW slot ptr-field ;


: ARENA-BASE ( n -- ptr u8 )   ARENA-CELL @ ;
: ARENA-BASE! ( ptr u8 n -- ) {: p slot:n :}   p slot ARENA-CELL ! ;

: CONN-GEN@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-GEN slot cells + @ ;
: CONN-GEN! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-GEN slot cells + ! ;
: CONN-LIVE-CELL ( n -- ptr n ) {: slot:n :}   slot CONN-CHECK CONN-LIVE slot cells + ;
: CONN-LIVE@ ( n -- n )   CONN-LIVE-CELL atomic@ ;
: CONN-LIVE! ( n n -- ) {: v:n slot:n :}   v slot CONN-LIVE-CELL atomic! ;
: CONN-OWNER@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-OWNER slot cells + @ ;
: CONN-OWNER! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-OWNER slot cells + ! ;
: CONN-TX@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-TX slot cells + @ ;
: CONN-TX! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-TX slot cells + ! ;
: PARAM-N@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-PARAM-N slot cells + @ ;
: PARAM-N! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-PARAM-N slot cells + ! ;
: ARENA-CAP@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-ARENA-CAP slot cells + @ ;
: ARENA-CAP! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-ARENA-CAP slot cells + ! ;
: ARENA-U@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-ARENA-U slot cells + @ ;
: ARENA-U! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-ARENA-U slot cells + ! ;
: MESSAGE-U@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-MESSAGE-U slot cells + @ ;
: MESSAGE-U! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-MESSAGE-U slot cells + ! ;
: SQLSTATE-U@ ( n -- n ) {: slot:n :}   slot CONN-CHECK CONN-SQLSTATE-U slot cells + @ ;
: SQLSTATE-U! ( n n -- ) {: v:n slot:n :}   slot CONN-CHECK v CONN-SQLSTATE-U slot cells + ! ;

: VERB-BUF ( n -- ptr u8 ) {: slot:n :}   slot CONN-CHECK CONN-VERB slot VERB-CAP * + ;
: MESSAGE-BUF ( n -- ptr u8 ) {: slot:n :}
   slot CONN-CHECK CONN-MESSAGE slot MESSAGE-CAP * + ;
: SQLSTATE-BUF ( n -- ptr u8 ) {: slot:n :}
   slot CONN-CHECK CONN-SQLSTATE slot SQLSTATE-LEN * + ;

: PARAM-CHECK ( n -- ) {: idx:n :}
   idx 0 < if E-CAPACITY throw then
   idx PARAM-CAP >= if E-CAPACITY throw then ;


: PARAM-OFF@ ( n n -- n ) {: slot:n idx:n :}
   slot CONN-CHECK idx PARAM-CHECK
   CONN-PARAM-OFF slot PARAM-CAP * idx + cells + @ ;


: PARAM-OFF! ( n n n -- ) {: v:n slot:n idx:n :}
   slot CONN-CHECK idx PARAM-CHECK
   v CONN-PARAM-OFF slot PARAM-CAP * idx + cells + ! ;


: RES-CHECK ( n -- ) {: slot:n :}
   slot 0 < if E-HANDLE throw then
   slot RESULT-CAP >= if E-HANDLE throw then ;


: RES-PG-CELL ( n -- ptr ptr u8 ) {: slot:n :}
   slot RES-CHECK
   RES-PG BYTE-VIEW slot ptr-field ;


: RES-PG@ ( n -- ptr u8 )   RES-PG-CELL @ ;
: RES-PG! ( ptr u8 n -- ) {: p slot:n :}   p slot RES-PG-CELL ! ;

: RES-GEN@ ( n -- n ) {: slot:n :}   slot RES-CHECK RES-GEN slot cells + @ ;
: RES-GEN! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v RES-GEN slot cells + ! ;
: RES-LIVE-CELL ( n -- ptr n ) {: slot:n :}   slot RES-CHECK RES-LIVE slot cells + ;
: RES-LIVE@ ( n -- n )   RES-LIVE-CELL atomic@ ;
: RES-LIVE! ( n n -- ) {: v:n slot:n :}   v slot RES-LIVE-CELL atomic! ;
: RES-OWNER@ ( n -- n ) {: slot:n :}   slot RES-CHECK RES-OWNER slot cells + @ ;
: RES-OWNER! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v RES-OWNER slot cells + ! ;
: RES-CONN@ ( n -- n ) {: slot:n :}   slot RES-CHECK RES-CONN slot cells + @ ;
: RES-CONN! ( n n -- ) {: v:n slot:n :}   slot RES-CHECK v RES-CONN slot cells + ! ;


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


\ Retiring a slot bumps its generation, so every handle minted from it stops
\ resolving. That is the linear owner: one CLEAR, one CLOSE, and no second use.
: BUMP-CONN-GEN ( n -- ) {: slot:n :}
   slot CONN-GEN@ 1 + slot CONN-GEN! ;


: BUMP-RES-GEN ( n -- ) {: slot:n :}
   slot RES-GEN@ 1 + slot RES-GEN! ;


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


\ ---- image capture --------------------------------------------------------
\ A restored image runs in another process: its PGconn and PGresult addresses
\ are gone and its mappings are not its own. Retiring every slot makes the
\ surviving handles refuse instead of reaching a freed pointer, and releases
\ the call arenas the image must not carry. The flag is set only after REGISTER
\ returns, so a throwing registration stays retryable (docs/forth.md rule).
: FORGET-HANDLES ( -- )
   CONN-CAP 0 ?do
      i CONN-LIVE@ 0 <> if
         i ARENA-RELEASE
         i BUMP-CONN-GEN
         0 i CONN-LIVE!
      then
   loop
   RESULT-CAP 0 ?do
      i RES-LIVE@ 0 <> if i BUMP-RES-GEN NO-CONN i RES-CONN! 0 i RES-LIVE! then
   loop ;


: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: FORGET-HANDLES ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;


\ ---- slot lifecycle -------------------------------------------------------
: OPEN-CONN-SLOT ( n -- ) {: slot:n :}
   slot BUMP-CONN-GEN
   TASK:SELF-N slot CONN-OWNER!
   0 slot CONN-TX!
   0 slot MESSAGE-U!
   0 slot SQLSTATE-U!
   slot RESET-PARAMS ;


: CONN-HANDLE ( n -- connection ) {: slot:n :}
   slot CONN-GEN@ slot PACK-HANDLE >CONNECTION ;


: RETIRE-CONN-SLOT ( n -- ) {: slot:n :}
   slot ARENA-RELEASE
   slot BUMP-CONN-GEN
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
   slot BUMP-RES-GEN
   NO-CONN slot RES-CONN!
   0 slot RES-LIVE! ;


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
   res PG-DIAG-SQLSTATE PQ-RESULT-ERROR-FIELD {: p :}
   p NULL-ADDR? if 0 slot SQLSTATE-U! exit then
   slot p CSTR$ SQLSTATE! ;


\ A result that carries no primary message - a connection that died under the
\ query, or a status libpq produced without the server - still has libpq's own
\ connection message, so the failed arm is never silently empty.
: MESSAGE-FROM ( n ptr u8 -- ) {: slot:n res :}
   res PG-DIAG-MESSAGE-PRIMARY PQ-RESULT-ERROR-FIELD {: p :}
   p NULL-ADDR? 0= if slot p CSTR$ MESSAGE! exit then
   slot CONN-PG@ PQ-ERROR-MESSAGE {: q :}
   q NULL-ADDR? if 0 slot MESSAGE-U! exit then
   slot q CSTR$ MESSAGE! ;


: DIAGNOSE ( n ptr u8 -- ) {: slot:n res :}
   slot res SQLSTATE-FROM
   slot res MESSAGE-FROM ;


\ ---- statement execution --------------------------------------------------
\ The result slot is claimed BEFORE libpq runs, so a full registry refuses
\ without ever leaving a PGresult nobody can clear, and the arena is released
\ the instant libpq returns - it has copied everything into its own message by
\ then. A throw before that leaves the arena for the next PARAMS or CLOSE.
: CLAIM-STATEMENT ( n -- n n ) {: slot:n :}
   slot PARAM-ARRAY CLAIM-RES-SLOT ;


: TOOK-RESULT ( ptr u8 n n -- result ) {: pg slot:n res:n :}
   slot RESET-PARAMS
   pg NULL-ADDR? if 0 res RES-LIVE! E-EXEC throw then
   pg slot res FILL-RES-SLOT ;


: EXEC-RAW ( n ptr u8 n -- result ) {: slot:n a u:n :}
   u 0 <= if E-STATEMENT throw then
   a u slot ARENA-CSTR {: sql-off:n :}
   slot CLAIM-STATEMENT {: arr-off:n res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base sql-off + slot PARAM-N@ NULL-ARG base arr-off +
   NULL-ARG NULL-ARG TEXT-RESULT PQ-EXEC-PARAMS
   slot res TOOK-RESULT ;


: PREPARED-RAW ( n ptr u8 n -- result ) {: slot:n a u:n :}
   u 0 <= if E-STATEMENT throw then
   a u slot ARENA-CSTR {: name-off:n :}
   slot CLAIM-STATEMENT {: arr-off:n res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base name-off + slot PARAM-N@ base arr-off +
   NULL-ARG NULL-ARG TEXT-RESULT PQ-EXEC-PREPARED
   slot res TOOK-RESULT ;


: PREPARE-RAW ( n ptr u8 n ptr u8 n -- result ) {: slot:n na nu:n sa su:n :}
   nu 0 <= if E-STATEMENT throw then
   su 0 <= if E-STATEMENT throw then
   na nu slot ARENA-CSTR {: name-off:n :}
   sa su slot ARENA-CSTR {: sql-off:n :}
   CLAIM-RES-SLOT {: res:n :}
   slot ARENA-BASE {: base :}
   slot CONN-PG@ base name-off + base sql-off + 0 NULL-ARG PQ-PREPARE
   slot res TOOK-RESULT ;


: RESULT-STATUS ( n -- n ) {: slot:n :}
   slot RES-PG@ PQ-RESULT-STATUS C-INT ;


: CLEAR-SLOT ( n -- ) {: slot:n :}
   slot RES-PG@ PQ-CLEAR
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
   slot a u VERB-TEXT! {: sql :}
   CLAIM-RES-SLOT {: res:n :}
   slot CONN-PG@ sql 0 NULL-ARG NULL-ARG NULL-ARG NULL-ARG TEXT-RESULT
   PQ-EXEC-PARAMS {: pg :}
   pg NULL-ADDR? if 0 res RES-LIVE! E-EXEC throw then
   pg slot res FILL-RES-SLOT RESULT-SLOT ;


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
   ri res PQ-NTUPLES C-INT >= if E-COLUMN throw then
   ci res PQ-NFIELDS C-INT >= if E-COLUMN throw then
   slot ri ci ;


: VALUE$ ( n n n -- ptr u8 n ) {: slot:n ri:n ci:n :}
   slot RES-PG@ {: res :}
   res ri ci PQ-GETVALUE
   res ri ci PQ-GETLENGTH C-INT ;


: CELL-NULL? ( n n n -- bool ) {: slot:n ri:n ci:n :}
   slot RES-PG@ ri ci PQ-GETISNULL C-INT 0 <> ;


: CMD-COUNT ( n -- n ) {: slot:n :}
   slot RES-PG@ PQ-CMD-TUPLES CSTR$ {: a u:n :}
   u 0= if 0 exit then
   a u STR>NUMBER? MATCH option
      none OF E-TYPE throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ ---- the outcome ADT ------------------------------------------------------
: CLASSIFY ( n n -- outcome ) {: slot:n res:n :}
   res RESULT-STATUS {: status:n :}
   status PGRES-COMMAND-OK = if DB-OUTCOME:ok exit then
   status PGRES-TUPLES-OK = if DB-OUTCOME:rows exit then
   slot res RES-PG@ DIAGNOSE
   slot SQLSTATE$ slot MESSAGE$ DB-OUTCOME:failed ;


\ ---- platform -------------------------------------------------------------
: INIT ( -- )
   HB-TARGET-LINUX? 0= if E-PLATFORM throw then ;


\ Every result slot starts owned by no connection, so CLOSE's scan matches only
\ a slot some task actually filled.
: INIT-REGISTRY ( -- )
   RESULT-CAP 0 ?do NO-CONN i RES-CONN! loop ;

INIT-REGISTRY

public

\ CONNECT hands back an owned connection the calling task closes exactly once.
\ The slot is claimed first, so the conninfo it stages and the message a
\ refusal copies back are its own and no concurrent CONNECT shares them. A
\ refusal releases the slot, and those message bytes stay readable until the
\ next CONNECT takes it.
: CONNECT ( ptr u8 n -- connect-result ) {: a u:n :}
   INIT
   REGISTER-CLEANUP
   CLAIM-CONN-SLOT {: slot:n :}
   slot OPEN-CONN-SLOT
   a u slot ARENA-CSTR {: off:n :}
   slot ARENA-BASE off + PQ-CONNECTDB {: pg :}
   slot ARENA-RELEASE
   pg NULL-ADDR? if slot RETIRE-CONN-SLOT E-CONNECT throw then
   pg PQ-STATUS C-INT CONNECTION-OK = if
      pg slot CONN-PG!
      slot CONN-HANDLE DB-CONNECT--RESULT:connected exit
   then
   slot pg PQ-ERROR-MESSAGE CSTR$ MESSAGE!
   pg PQ-FINISH
   slot MESSAGE$ {: ma mu:n :}
   slot RETIRE-CONN-SLOT
   ma mu DB-CONNECT--RESULT:refused ;


\ CLOSE clears whatever results the connection still owns, so libpq keeps no
\ orphan PGresult, then releases the call arena and retires the handle.
: CLOSE ( connection -- ) {: handle:connection :}
   handle CONN-SLOT {: slot:n :}
   RESULT-CAP 0 ?do
      i RES-LIVE@ 0 <> i RES-CONN@ slot = and if i CLEAR-SLOT then
   loop
   slot CONN-PG@ PQ-FINISH
   slot RETIRE-CONN-SLOT ;


\ PARAMS empties the connection's parameter list and releases its call arena;
\ TEXT+, INT+ and NULL+ append in $1, $2, ... order. EXEC and EXEC-PREPARED
\ consume the list and leave it empty, so no call inherits another's parameters.
: PARAMS ( connection -- )
   CONN-SLOT RESET-PARAMS ;


: TEXT+ ( connection ptr u8 n -- ) {: handle:connection a u:n :}
   handle CONN-SLOT {: slot:n :}
   slot PARAM-ROOM
   a u slot ARENA-CSTR slot PARAM-OFF+ ;


: INT+ ( connection n -- ) {: handle:connection value:n :}
   handle CONN-SLOT {: slot:n :}
   slot PARAM-ROOM
   value slot ARENA-INT slot PARAM-OFF+ ;


: NULL+ ( connection -- ) {: handle:connection :}
   handle CONN-SLOT {: slot:n :}
   slot PARAM-ROOM
   NO-OFFSET slot PARAM-OFF+ ;


: EXEC ( connection ptr u8 n -- result ) {: handle:connection a u:n :}
   handle CONN-SLOT a u EXEC-RAW ;


: PREPARE ( connection ptr u8 n ptr u8 n -- result )
   {: handle:connection na nu:n sa su:n :}
   handle CONN-SLOT na nu sa su PREPARE-RAW ;


: EXEC-PREPARED ( connection ptr u8 n -- result ) {: handle:connection a u:n :}
   handle CONN-SLOT a u PREPARED-RAW ;


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
   RESULT-SLOT RES-PG@ PQ-NTUPLES C-INT >COUNT ;


: COLS ( result -- count )
   RESULT-SLOT RES-PG@ PQ-NFIELDS C-INT >COUNT ;


: AFFECTED ( result -- count )
   RESULT-SLOT CMD-COUNT >COUNT ;


: NAME$ ( result col -- ptr u8 n ) {: handle:result c:col :}
   handle RESULT-SLOT RES-PG@ {: res :}
   c COL>N {: ci:n :}
   ci 0 < if E-COLUMN throw then
   ci res PQ-NFIELDS C-INT >= if E-COLUMN throw then
   res ci PQ-FNAME {: p :}
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
