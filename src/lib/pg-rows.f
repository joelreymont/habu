\ pg-rows.f - package PG's connections per module, statements and row readers.
\ Before the first OPEN, read or query: `n PG:CONNECTIONS+` in each module for
\ the connections it opens, and `n PG:CONFIGURE-READERS` once for the tasks
\ that read rows or run queries. Span lifetimes: above NO-TASK.
\ STORAGE CLASS. TASK-LOCAL arenas and refusal texts, one row per reading task;
\ the declared connection and reader counts are PROCESS-WIDE.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/float.f
require src/lib/task.f
require src/lib/image-lifecycle.f
require src/lib/pg.f

\ PG's row readers: -9320..-9329, the decade after IMAGE-LIFECYCLE's. Minted
\ here, beside the words that throw them (docs/forth-card.md § 6). A refused
\ OPEN is E-CONNECT and a declaration out of range or changed after use is
\ E-CAPACITY, PG's own codes.
-9320 constant E-DB-FIRST
-9329 constant E-DB-LAST

package PG

public

-9321 constant E-QUERY        \ the server refused a query, a statement answered no rows where rows were read, or rows where EXEC expects none
-9322 constant E-ROW          \ the query answered no row, or a column its reader cannot read
-9323 constant E-READERS      \ more tasks read rows or run queries than the declared readers

private

64 constant RESULTS               \ live results across every connection
32 constant PARAMETERS            \ parameters one statement takes
$4000 constant READ-CAP           \ an arena's first chunk, and the least a later one maps
$74 constant TRUE-BYTE            \ PostgreSQL renders a boolean as 't' or 'f'

\ The process entry owns one connection. Each module that opens connections
\ declares its maximum beside the storage that owns them, before the first OPEN.
variable CONNECTION-N
1 CONNECTION-N !
variable CONNECTIONS-FROZEN

\ A record outlives the result it was read from, so every span it carries is
\ copied into the reading task's own arena first, and the result's scope clears
\ the result before the record is returned. A server reads rows from many tasks
\ at once: one shared arena would let another task's copy overwrite a span
\ between its lookup and its use. A record's spans are valid until the task
\ that read them reads again (READ-RESET, which BY-ID runs), and no longer.
\ COL- readers copy; AT readers answer libpq's bytes in place.
\
\ A row is taken on a task's first read, query or REFUSED$ and stays that
\ task's: a TCB is declared once (TASK:TASK) and reused for every body that runs
\ on it, so the rows are bounded by the tasks the image declares and never by
\ the jobs that pass through them. An untaken row costs a few cells; its bytes
\ are mapped only when its task first reads.
\
\ An arena is a chain of chunks, newest first. A record holds pointers into
\ them, so a chunk never moves: a span the newest cannot hold maps another of
\ READ-CAP bytes or of the span's own length behind it, whichever is larger,
\ whose header names the chunk before. The first chunk's header names none.
0 constant NO-TASK                \ a row no task has taken; also the main task's TCB
0 constant MAIN-ARENA             \ the arena of the main task, which has no TCB
1 constant FIRST-CLAIMED          \ the rows tasks take, MAIN-ARENA being taken already

variable READER-N                 \ the declared rows, zero until CONFIGURE-READERS
TYPED-VARIABLE READERS-REGISTERED bool
false READERS-REGISTERED !

STRUCTURE chunk DERIVE addr
   FIELD prev ptr u8
   FIELD prev-len n
;STRUCTURE

CAST: >CHUNK ( ptr u8 -- ptr chunk )

CHUNK-CELLS cells constant HEADER-BYTES

DYNAMIC-BUFFER READ-CHUNK SPAN:span<u8>   \ the newest chunk, empty until the first read
DYNAMIC-BUFFER READ-TASK n
DYNAMIC-BUFFER READ-USED n                \ the newest chunk's bytes taken, its header among them
DYNAMIC-BUFFER REFUSAL SPAN:span<u8>      \ the task's refusal text mapping, empty until its first refusal
DYNAMIC-BUFFER REFUSAL-U n                \ its bytes in use, zero when the last outcome was no refusal


\ The scan may pass rows another task takes while it runs: only this task ever
\ takes this task's row.
: MY-ARENA ( -- n )
   READER-N @ {: readers:n :}
   readers 0= if E-READERS throw then
   TASK:SELF-N {: me:n :}
   me NO-TASK = if MAIN-ARENA exit then
   readers FIRST-CLAIMED ?do
      i READ-TASK @ me = if i unloop exit then
      NO-TASK me i READ-TASK atomic-cas NO-TASK = if i unloop exit then
   loop
   E-READERS throw ;


: CHUNK+ ( n n -- ) {: at:n len:n :}
   len MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
   at READ-CHUNK @ SPAN:$ {: before:ptr before-len:n :}
   fresh SPAN:$ drop >CHUNK {: c :}
   before c CHUNK-PREV !
   before-len c CHUNK-PREV-LEN !
   fresh at READ-CHUNK !
   HEADER-BYTES at READ-USED ! ;


\ False when row at's newest chunk is its first.
: POP-CHUNK? ( n -- bool ) {: at:n :}
   at READ-CHUNK @ {: newest :}
   newest SPAN:$ drop >CHUNK {: c :}
   c CHUNK-PREV-LEN @ 0= if false exit then
   c CHUNK-PREV @ c CHUNK-PREV-LEN @ SPAN:MAKE at READ-CHUNK !
   newest MEM:FREE-SPAN
   true ;


: TRIM ( n -- ) {: at:n :}
   begin at POP-CHUNK? 0= until ;


\ The newest chunk of row at, with room for u more bytes. A row's first chunk
\ is mapped here on its first read, by the only task that ever asks for it -
\ so the map needs no lock, and main's row is mapped the same way as a
\ worker's. Its bytes live until image preparation.
: ROOM ( n n -- SPAN:span<u8> ) {: at:n u:n :}
   at READ-CHUNK @ SPAN:LEN 0= if at READ-CAP CHUNK+ then
   at READ-USED @ u + at READ-CHUNK @ SPAN:LEN > if
      at READ-CAP u HEADER-BYTES + max CHUNK+
   then
   at READ-CHUNK @ ;


: ARENA+ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   MY-ARENA {: at:n :}
   at u ROOM {: chunk :}
   at READ-USED @ {: used:n :}
   a u chunk used SPAN:SKIP SPAN:COPY
   used u + at READ-USED !
   chunk used u SPAN:SUB SPAN:$ ;


\ A refusal overwrites the one before it, so a task's text needs one mapping,
\ grown to the longest so far. The old bytes are never read again: growth maps
\ the new size and drops the old mapping without moving them.
: REFUSAL-ROOM ( n n -- ) {: at:n want:n :}
   want at REFUSAL @ SPAN:LEN <= if exit then
   want MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
   at REFUSAL @ SPAN:LEN 0<> if at REFUSAL @ MEM:FREE-SPAN then
   fresh at REFUSAL ! ;


: REFUSAL+ ( ptr u8 n n -- ) {: a:ptr u:n at:n :}
   a u at REFUSAL @ at REFUSAL-U @ SPAN:SKIP SPAN:COPY
   u at REFUSAL-U +! ;


\ A `failed` outcome's SQLSTATE and message become the task's refusal text,
\ `SQLSTATE: message`, or the message alone when libpq made the failure
\ without a SQLSTATE; then the query is E-QUERY.
: REFUSED ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ma:ptr mu:n :}
   MY-ARENA {: at:n :}
   0 at REFUSAL-U !
   at su mu + 2 + REFUSAL-ROOM
   su 0<> if
      sa su at REFUSAL+
      s" : " at REFUSAL+
   then
   ma mu at REFUSAL+
   E-QUERY throw ;


: NOT-REFUSED ( -- )
   0 MY-ARENA REFUSAL-U ! ;


\ A restored image runs in another process: the arenas are mappings it does not
\ own and the TCB numbers name tasks it never started. The rows are released
\ and the declaration with them; configure again in the new process.
: FORGET-READERS ( -- )
   READER-N @ 0<> if
      READER-N @ 0 ?do
         i READ-CHUNK @ SPAN:LEN 0<> if i TRIM i READ-CHUNK @ MEM:FREE-SPAN then
         i REFUSAL @ SPAN:LEN 0<> if i REFUSAL @ MEM:FREE-SPAN then
      loop
      READ-CHUNK-RELEASE
      READ-TASK-RELEASE
      READ-USED-RELEASE
      REFUSAL-RELEASE
      REFUSAL-U-RELEASE
      0 READER-N !
   then
   false READERS-REGISTERED ! ;


: CELL$ ( PG:result n -- PG:result ptr u8 n ) {: c:n :}
   0 >ROW c >COL NULL? if E-ROW throw then
   0 >ROW c >COL TEXT$ ;


: BOOL$ ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 <> if E-ROW throw then
   a c@ TRUE-BYTE = ;

public

\ Declares how many tasks read rows or run queries, the main task among them,
\ and maps the table of their rows. Like CONFIGURE it is one decision for the
\ process: repeating the same count is harmless, a different one is E-CAPACITY
\ until image preparation releases the rows. A read, query or REFUSED$ before
\ it is E-READERS, and so is one from one task more than the count.
: CONFIGURE-READERS ( n -- ) {: readers:n :}
   readers 1 < if E-CAPACITY throw then
   READER-N @ {: had:n :}
   had readers = if exit then
   had 0<> if E-CAPACITY throw then
   readers READ-CHUNK-RESERVE
   readers READ-TASK-RESERVE
   readers READ-USED-RESERVE
   readers REFUSAL-RESERVE
   readers REFUSAL-U-RESERVE
   readers READER-N !
   READERS-REGISTERED @ 0= if
      [: FORGET-READERS ;] IMAGE-LIFECYCLE:REGISTER
      true READERS-REGISTERED !
   then ;


\ The declared connection count, also used by suites that deliberately leave
\ a stated number of free connections.
: CONNECTIONS ( -- n ) CONNECTION-N @ ;


\ A load-time declaration, not a change to a running registry. PG allocates
\ the combined capacity on the first OPEN and keeps that bound for the process.
: CONNECTIONS+ ( n -- ) {: count:n :}
   count 0 <= CONNECTIONS-FROZEN @ 0<> or if E-CAPACITY throw then
   count CONNECTION-N +! ;


\ The attempt for a caller that drives POLL itself.
: OPEN-START ( ptr u8 n -- PG:connection )
   CONNECTIONS RESULTS PARAMETERS CONFIGURE
   1 CONNECTIONS-FROZEN !
   CONNECT-START ;


\ A connection for the calling task. The refusal text is libpq's and can name
\ the server and the user, so it stays inside the module and the caller gets
\ the code. AWAIT waits through AIO, so AIO:START comes first.
: OPEN ( ptr u8 n -- PG:connection )
   OPEN-START AWAIT MATCH progress
      connected OF ENDOF
      refused OF 2drop E-CONNECT throw ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF CLEAR E-CONNECT throw ENDOF
   ;MATCH ;


\ An empty conninfo makes libpq read PGHOST, PGPORT, PGDATABASE, PGUSER and
\ PGPASSWORD from the environment.
: OPEN-ENV ( -- PG:connection )
   s" " OPEN ;


\ Starts a read on this task: the spans of its previous read are given up, and
\ every chunk they took but the first, and the last refusal's text is emptied,
\ so a query that ends any other way than refused leaves REFUSED$ empty.
: READ-RESET ( -- )
   MY-ARENA {: at:n :}
   at READ-CHUNK @ SPAN:LEN 0<> if at TRIM then
   HEADER-BYTES at READ-USED !
   0 at REFUSAL-U ! ;


\ A result that answered rows, or E-QUERY. It runs inside the caller's result
\ scope, after the statement ran, so the caller takes the task's reader row
\ first (READ-RESET): an undeclared task would be E-READERS only once the
\ server had acted.
: ROWS-OR-THROW ( PG:result -- PG:result )
   OUTCOME MATCH outcome
      ok OF NOT-REFUSED E-QUERY throw ENDOF
      rows OF NOT-REFUSED ENDOF
      failed OF REFUSED ENDOF
   ;MATCH ;


\ A reader answers the row it was asked for or a named failure; it never
\ invents fields for an id that names nothing. Like ROWS-OR-THROW, it runs once
\ the statement ran, so the task's reader row is taken first.
: FIRST-ROW ( PG:result -- PG:result )
   ROWS-OR-THROW
   ROWS COUNT>N 0= if E-ROW throw then ;


\ The server's text for the task's last refused query, empty after an outcome
\ that was no refusal (see REFUSED for its shape). ROWS-OR-THROW, EXEC, QUERY,
\ BY-ID and ONE-INT record it before they throw E-QUERY. Valid until the task's
\ next query.
: REFUSED$ ( -- ptr u8 n )
   MY-ARENA {: at:n :}
   at REFUSAL @ SPAN:$ drop at REFUSAL-U @ ;

private

: RUN-FIRST ( R [ R PG:result -- S PG:result ] PG:result -- S PG:result )
   FIRST-ROW swap execute ;

: RUN-ROWS ( R [ R PG:result -- S PG:result ] PG:result -- S PG:result )
   ROWS-OR-THROW swap execute ;

: NO-ROWS ( PG:result -- PG:result )
   OUTCOME MATCH outcome
      ok OF NOT-REFUSED ENDOF
      rows OF NOT-REFUSED E-QUERY throw ENDOF
      failed OF REFUSED ENDOF
   ;MATCH ;

public


\ A statement that answers no rows, run on the parameters the caller staged:
\ DDL, or a command without RETURNING. Rows answered, even none, are E-QUERY.
\ The task's reader row is taken before the statement is sent, so an undeclared
\ task sends nothing.
: EXEC ( PG:connection ptr u8 n -- )
   NOT-REFUSED
   [: NO-ROWS ;] WITH-RESULT ;


\ A statement that answers rows, run on the parameters the caller staged. The
\ body runs on the result, whatever its row count, inside the result's scope,
\ which clears it when the body returns or throws.
: QUERY ( R PG:connection ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: c a:ptr u:n body :}
   READ-RESET
   body c a u [: RUN-ROWS ;] WITH-RESULT ;


\ One parameter, one row: the read of a row by its identity. The body runs on
\ the result once its first row is there; the scope clears the result when
\ the body returns or throws.
: BY-ID ( R PG:connection n ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: c id:n a:ptr u:n body :}
   READ-RESET
   c PARAMS
   c id INT+
   body c a u [: RUN-FIRST ;] WITH-RESULT ;


\ The first row's columns, copied into the task's arena.

\ SQL NULL is E-ROW; COL-TEXT? is the nullable reader.
: COL-TEXT ( PG:result n -- PG:result ptr u8 n )
   CELL$ ARENA+ ;


\ A nullable text: false means SQL NULL, and the span is then empty.
: COL-TEXT? ( PG:result n -- PG:result ptr u8 n bool ) {: c:n :}
   0 >ROW c >COL NULL? if s" " ARENA+ false exit then
   c COL-TEXT true ;


\ SQL NULL is E-TYPE; COL-ID is the nullable reader.
: COL-INT ( PG:result n -- PG:result n ) {: c:n :}
   0 >ROW c >COL INT ;


\ A number with a fraction. The server renders it as text, so this is where it
\ becomes one again; SQL NULL is zero.
: COL-REAL ( PG:result n -- PG:result r ) {: c:n :}
   0 >ROW c >COL NULL? if 0.0 exit then
   c CELL$ STR>FLOAT MATCH option
      none OF E-ROW throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ A nullable reference: SQL NULL is zero, which an identity column never holds.
: COL-ID ( PG:result n -- PG:result n ) {: c:n :}
   0 >ROW c >COL NULL? if 0 exit then
   c COL-INT ;


: COL-BOOL ( PG:result n -- PG:result bool )
   CELL$ BOOL$ ;


\ These answer libpq's own bytes, valid until the result's scope ends: a page
\ of rows is read inside its scope, and nothing is copied.

: AT$ ( PG:result n n -- PG:result ptr u8 n ) {: at:n col:n :}
   at >ROW col >COL TEXT$ ;


: AT?$ ( PG:result n n -- PG:result ptr u8 n bool ) {: at:n col:n :}
   at >ROW col >COL NULL? if s" " false exit then
   at col AT$ true ;


: AT-INT ( PG:result n n -- PG:result n ) {: at:n col:n :}
   at >ROW col >COL INT ;


: AT-BOOL ( PG:result n n -- PG:result bool )
   AT$ BOOL$ ;

private

: FIRST-INT ( PG:result -- n PG:result )
   FIRST-ROW
   0 0 AT$ STR>NUMBER? MATCH option
      none OF E-ROW throw ENDOF
      some OF ENDOF
   ;MATCH swap ;

public


\ The one integer a count or an aggregate answers, run in its own scope on
\ the parameters the caller staged. A NULL, a fraction or no row is E-ROW.
\ Like EXEC, it takes the task's reader row before the statement is sent.
: ONE-INT ( PG:connection ptr u8 n -- n )
   NOT-REFUSED
   [: FIRST-INT ;] WITH-RESULT ;

;package
