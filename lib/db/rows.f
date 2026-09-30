\ rows.f - connections and row readers over package PG.
\
\ OPEN hands out a connection for the calling task; the readers below turn a
\ PG:result into values. A span a reader answers is copied into the reading
\ task's own arena first, so it outlives PG:CLEAR: a record read through WITH-ROW
\ keeps its text after its result is gone. See docs/db.md "Row readers".
\
\ Two declarations come first, both before the first OPEN or read:
\
\    n DB-ROWS:CONNECTIONS+          \ each module, for the connections it opens
\    n DB-ROWS:CONFIGURE-READERS     \ once, for the tasks that read rows

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/span.f
require lib/float.f
require lib/task.f
require lib/image-lifecycle.f
require lib/pg.f

package DB-ROWS
private

64 constant RESULTS               \ live results across every connection
32 constant PARAMETERS            \ parameters one statement takes
$4000 constant READ-CAP           \ the bytes one read may copy out of its results
$74 constant TRUE-BYTE            \ PostgreSQL renders a boolean as 't' or 'f'

\ The process entry owns one connection. Each module that opens connections
\ declares its maximum beside the storage that owns them, before the first OPEN.
variable CONNECTION-N
1 CONNECTION-N !
variable CONNECTIONS-FROZEN

\ ---- the reader arenas ----------------------------------------------------------
\ A record outlives the result it was read from, so every span it carries is
\ copied into an arena first and the result is cleared before the record is
\ returned. The arena is the reading task's own, one row per task, because
\ static storage is one copy for the whole image (docs/threads.md) while a
\ server reads rows from many tasks at once, each on a connection of its own.
\ One arena between them hands a reader back the bytes another task copied after
\ it: Tender witnessed a platform whose adapter name had become another row's
\ text between the lookup and the run. A record's spans are therefore valid
\ until the task that read them reads again, and no longer.
\
\ A row is taken on a task's first read and stays that task's: a TCB is
\ declared once (TASK:TASK) and reused for every body that runs on it, so the
\ rows are bounded by the tasks the image declares and never by the jobs that
\ pass through them. An untaken row costs a few cells; its bytes are mapped only
\ when its task first reads.
0 constant NO-TASK                \ a row no task has taken; also the main task's TCB
0 constant MAIN-ARENA             \ the arena of the main task, which has no TCB
1 constant FIRST-CLAIMED          \ the rows tasks take, MAIN-ARENA being taken already

variable READER-N                 \ the declared rows, zero until CONFIGURE-READERS
TYPED-VARIABLE REGISTERED bool
false REGISTERED !

DYNAMIC-BUFFER READ-SPAN SPAN:span<u8>
DYNAMIC-BUFFER READ-TASK n
DYNAMIC-BUFFER READ-USED n
\ The result a row is decoding, so WITH-ROW's cleanup can reach it; it belongs
\ to this task until its record is decoded, on failure as well as success.
DYNAMIC-BUFFER READ-RESULT PG:result


\ The arena of the task that is running, taken here if this is its first read.
\ A row is found by the task that owns it, so the scan may pass rows another
\ task takes while it runs: only this task ever takes this task's row.
: MY-ARENA ( -- n )
   READER-N @ {: readers:n :}
   readers 0= if E-READERS throw then
   TASK:SELF-N {: me:n :}
   me NO-TASK = if MAIN-ARENA exit then
   readers FIRST-CLAIMED ?do
      i READ-TASK @ me = if i unloop exit then
      NO-TASK me i READ-TASK atomic-cas NO-TASK = if 0 i READ-USED ! i unloop exit then
   loop
   E-READERS throw ;


\ A row's span is empty until its first read and is mapped here, by the only
\ task that ever asks for it - so the map needs no lock, and main's row is
\ mapped the same way as a worker's. The bytes live until image preparation.
: ARENA-BUF ( n -- SPAN:span<u8> ) {: at:n :}
   at READ-SPAN @ SPAN:LEN 0= if
      READ-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN at READ-SPAN !
   then
   at READ-SPAN @ ;


: ARENA+ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   MY-ARENA {: at:n :}
   at READ-USED @ {: used:n :}
   used u + READ-CAP > if E-CAPACITY throw then
   a u at ARENA-BUF used SPAN:SKIP SPAN:COPY
   used u + at READ-USED !
   at ARENA-BUF used u SPAN:SUB SPAN:$ ;


\ A restored image runs in another process: the arenas are mappings it does not
\ own and the TCB numbers name tasks it never started. The rows are released
\ and the declaration with them; configure again in the new process.
: FORGET-READERS ( -- )
   READER-N @ 0<> if
      READER-N @ 0 ?do
         i READ-SPAN @ SPAN:LEN 0<> if i READ-SPAN @ MEM:FREE-SPAN then
      loop
      READ-SPAN-RELEASE
      READ-TASK-RELEASE
      READ-USED-RELEASE
      READ-RESULT-RELEASE
      0 READER-N !
   then
   false REGISTERED ! ;


: CLEAR-ROW ( -- )
   MY-ARENA READ-RESULT @ PG:CLEAR ;


: CELL$ ( PG:result n -- ptr u8 n ) {: r c:n :}
   r 0 PG:>ROW c PG:>COL PG:NULL? if E-ROW throw then
   r 0 PG:>ROW c PG:>COL PG:TEXT$ ;


: BOOL$ ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 <> if E-ROW throw then
   a c@ TRUE-BYTE = ;

public

\ ---- declarations ------------------------------------------------------------

\ Declares how many tasks read rows, the main task among them, and maps the
\ table of their rows. Like PG:CONFIGURE it is one decision for the process:
\ repeating the same count is harmless, a different one is E-CAPACITY until
\ image preparation releases the rows. A read before it is E-READERS, and so
\ is a read from one task more than the count.
: CONFIGURE-READERS ( n -- ) {: readers:n :}
   readers 1 < if E-CAPACITY throw then
   READER-N @ {: had:n :}
   had readers = if exit then
   had 0<> if E-CAPACITY throw then
   readers READ-SPAN-RESERVE
   readers READ-TASK-RESERVE
   readers READ-USED-RESERVE
   readers READ-RESULT-RESERVE
   readers READER-N !
   REGISTERED @ 0= if
      [: FORGET-READERS ;] IMAGE-LIFECYCLE:REGISTER
      true REGISTERED !
   then ;


\ The declared connection count, also used by suites that deliberately leave
\ a stated number of free connections.
: CONNECTIONS ( -- n ) CONNECTION-N @ ;


\ A load-time declaration, not a change to a running registry. PG allocates
\ the combined capacity on the first OPEN and keeps that bound for the process.
: CONNECTIONS+ ( n -- ) {: count:n :}
   count 0 <= CONNECTIONS-FROZEN @ 0<> or if E-CAPACITY throw then
   count CONNECTION-N +! ;


\ ---- connections -------------------------------------------------------------

\ The attempt for a caller that drives PG:POLL itself.
: OPEN-START ( ptr u8 n -- PG:connection )
   CONNECTIONS RESULTS PARAMETERS PG:CONFIGURE
   1 CONNECTIONS-FROZEN !
   PG:CONNECT-START ;


\ A connection for the calling task. The refusal text is libpq's and can name
\ the server and the user, so it stays inside the module and the caller gets
\ the code. PG:AWAIT waits through AIO, so AIO:START comes first.
: OPEN ( ptr u8 n -- PG:connection )
   OPEN-START PG:AWAIT MATCH PG:progress
      connected OF ENDOF
      refused OF 2drop E-CONNECT throw ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF PG:CLEAR E-CONNECT throw ENDOF
   ;MATCH ;


\ An empty conninfo makes libpq read PGHOST, PGPORT, PGDATABASE, PGUSER and
\ PGPASSWORD from the environment.
: OPEN-ENV ( -- PG:connection )
   s" " OPEN ;


\ ---- one read ----------------------------------------------------------------

\ Starts a read on this task: the spans of its previous read are given up.
: READ-RESET ( -- )
   0 MY-ARENA READ-USED ! ;


\ Runs the decoder over the result and clears the result on every path out.
: WITH-ROW ( PG:result [ PG:result -- R ] -- R ) {: r body :}
   r MY-ARENA READ-RESULT !
   r body [: CLEAR-ROW ;] finally ;


: ROWS-OR-THROW ( PG:result -- PG:result )
   dup PG:OUTCOME MATCH PG:outcome
      ok OF PG:CLEAR E-QUERY throw ENDOF
      rows OF ENDOF
      failed OF 2drop 2drop PG:CLEAR E-QUERY throw ENDOF
   ;MATCH ;


\ A reader answers the row it was asked for or a named failure; it never
\ invents fields for an id that names nothing.
: FIRST-ROW ( PG:result -- PG:result )
   ROWS-OR-THROW
   dup PG:ROWS COUNT>N 0= if PG:CLEAR E-ROW throw then ;


\ One parameter, one row: the read of a row by its identity.
: BY-ID ( PG:connection n ptr u8 n -- PG:result ) {: c id:n a:ptr u:n :}
   READ-RESET
   c PG:PARAMS
   c id PG:INT+
   c a u PG:EXEC FIRST-ROW ;


\ ---- the first row's columns, copied into the arena ---------------------------

\ SQL NULL is E-ROW; COL-TEXT? is the nullable reader.
: COL-TEXT ( PG:result n -- ptr u8 n )
   CELL$ ARENA+ ;


\ A nullable text: false means SQL NULL, and the span is then empty.
: COL-TEXT? ( PG:result n -- ptr u8 n bool ) {: r c:n :}
   r 0 PG:>ROW c PG:>COL PG:NULL? if s" " ARENA+ false exit then
   r c COL-TEXT true ;


\ SQL NULL is PG:E-TYPE; COL-ID is the nullable reader.
: COL-INT ( PG:result n -- n ) {: r c:n :}
   r 0 PG:>ROW c PG:>COL PG:INT ;


\ A number with a fraction. The server renders it as text, so this is where it
\ becomes one again; SQL NULL is zero.
: COL-REAL ( PG:result n -- r ) {: r c:n :}
   r 0 PG:>ROW c PG:>COL PG:NULL? if 0.0 exit then
   r c CELL$ STR>FLOAT MATCH option
      none OF E-ROW throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ A nullable reference: SQL NULL is zero, which an identity column never holds.
: COL-ID ( PG:result n -- n ) {: r c:n :}
   r 0 PG:>ROW c PG:>COL PG:NULL? if 0 exit then
   r c COL-INT ;


: COL-BOOL ( PG:result n -- bool )
   CELL$ BOOL$ ;


\ ---- any row, read in place ----------------------------------------------------
\ These answer libpq's own bytes, valid until PG:CLEAR: a page of rows is read
\ while its result is held, and nothing is copied.

: AT$ ( PG:result n n -- ptr u8 n ) {: r at:n col:n :}
   r at PG:>ROW col PG:>COL PG:TEXT$ ;


: AT?$ ( PG:result n n -- ptr u8 n bool ) {: r at:n col:n :}
   r at PG:>ROW col PG:>COL PG:NULL? if s" " false exit then
   r at col AT$ true ;


: AT-INT ( PG:result n n -- n ) {: r at:n col:n :}
   r at PG:>ROW col PG:>COL PG:INT ;


: AT-BOOL ( PG:result n n -- bool )
   AT$ BOOL$ ;


\ The one integer a count or an aggregate answers. The text is parsed before
\ the result is cleared, so a NULL or a fraction is E-ROW with nothing left
\ behind.
: ONE-INT ( PG:result -- n )
   FIRST-ROW {: r :}
   r 0 0 AT$ STR>NUMBER? {: v :}
   r PG:CLEAR
   v MATCH option
      none OF E-ROW throw ENDOF
      some OF ENDOF
   ;MATCH ;

;package
