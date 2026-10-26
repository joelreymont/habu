\ rows.f - connections and row readers over package PG.
\
\ OPEN hands out a connection for the calling task; the readers below turn a
\ PG:result into values inside the scope that owns it, threading the linear
\ token: BY-ID runs a query in a PG:WITH-RESULT scope, checks its first row and
\ runs a body on the result, and the scope clears it when the body returns or
\ throws. A span a COL- reader answers is copied into the reading task's own
\ arena first, so it outlives the scope and lasts until that task's next read
\ (READ-RESET, which BY-ID runs): a record BY-ID's body reads keeps its text
\ after its result is gone. The AT readers answer libpq's bytes, valid until
\ the result's scope ends.
\
\ STORAGE CLASS. TASK-LOCAL arenas: a reading task takes one on its first read
\ and keeps it, READ-CAP bytes that a longer read extends by chained chunks.
\ The declared connection and reader counts are PROCESS-WIDE.
\
\ Two declarations come first, both before the first OPEN or read:
\
\    n DB-ROWS:CONNECTIONS+          \ each module, for the connections it opens
\    n DB-ROWS:CONFIGURE-READERS     \ once, for the tasks that read rows

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/float.f
require src/lib/task.f
require src/lib/image-lifecycle.f
require src/lib/pg.f

\ The database layer over package PG: -9320..-9329, the decade after
\ IMAGE-LIFECYCLE's. Minted here, beside the package its codes belong to
\ (docs/forth-card.md § 6).
-9320 constant E-DB-FIRST
-9329 constant E-DB-LAST

package DB-ROWS

public

-9320 constant E-CONNECT      \ the database refused the connection
-9321 constant E-QUERY        \ the server refused a query, or a command answered no rows where rows were read
-9322 constant E-ROW          \ the query answered no row, or a column its reader cannot read
-9323 constant E-READERS      \ more tasks read rows than the declared readers
-9324 constant E-CAPACITY     \ a declaration out of range or after first use

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

\ ---- the reader arenas ----------------------------------------------------------
\ A record outlives the result it was read from, so every span it carries is
\ copied into an arena first and the result's scope clears it before the
\ record is returned. The arena is the reading task's own, one row per task, because
\ static storage is one copy for the whole image while a
\ server reads rows from many tasks at once, each on a connection of its own.
\ One arena between them would hand a reader back the bytes another task copied
\ after it, so a span could become another row's text between its lookup and
\ its use. A record's spans are therefore valid
\ until the task that read them reads again, and no longer.
\
\ A row is taken on a task's first read and stays that task's: a TCB is
\ declared once (TASK:TASK) and reused for every body that runs on it, so the
\ rows are bounded by the tasks the image declares and never by the jobs that
\ pass through them. An untaken row costs a few cells; its bytes are mapped only
\ when its task first reads.
\
\ An arena is a chain of chunks, newest first. A record holds pointers into
\ them, so a chunk never moves: a span the newest cannot hold maps another of
\ READ-CAP bytes or of the span's own length behind it, whichever is larger,
\ whose header names the chunk before. The first chunk's header names none.
0 constant NO-TASK                \ a row no task has taken; also the main task's TCB
0 constant MAIN-ARENA             \ the arena of the main task, which has no TCB
1 constant FIRST-CLAIMED          \ the rows tasks take, MAIN-ARENA being taken already

variable READER-N                 \ the declared rows, zero until CONFIGURE-READERS
TYPED-VARIABLE REGISTERED bool
false REGISTERED !

STRUCTURE chunk DERIVE addr
   FIELD prev ptr u8
   FIELD prev-len n
;STRUCTURE

CAST: >CHUNK ( ptr u8 -- ptr chunk )

CHUNK-CELLS cells constant HEADER-BYTES

DYNAMIC-BUFFER READ-CHUNK SPAN:span<u8>   \ the newest chunk, empty until the first read
DYNAMIC-BUFFER READ-TASK n
DYNAMIC-BUFFER READ-USED n                \ the newest chunk's bytes taken, its header among them


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
      NO-TASK me i READ-TASK atomic-cas NO-TASK = if i unloop exit then
   loop
   E-READERS throw ;


\ Maps a chunk of len bytes after the newest of row at, which it becomes.
: CHUNK+ ( n n -- ) {: at:n len:n :}
   len MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
   at READ-CHUNK @ SPAN:$ {: before:ptr before-len:n :}
   fresh SPAN:$ drop >CHUNK {: c :}
   before c CHUNK-PREV !
   before-len c CHUNK-PREV-LEN !
   fresh at READ-CHUNK !
   HEADER-BYTES at READ-USED ! ;


\ Gives up row at's newest chunk for the one before it; false at the first.
: POP-CHUNK? ( n -- bool ) {: at:n :}
   at READ-CHUNK @ {: newest :}
   newest SPAN:$ drop >CHUNK {: c :}
   c CHUNK-PREV-LEN @ 0= if false exit then
   c CHUNK-PREV @ c CHUNK-PREV-LEN @ SPAN:MAKE at READ-CHUNK !
   newest MEM:FREE-SPAN
   true ;


\ Gives up every chunk of a mapped row but the first.
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


\ A restored image runs in another process: the arenas are mappings it does not
\ own and the TCB numbers name tasks it never started. The rows are released
\ and the declaration with them; configure again in the new process.
: FORGET-READERS ( -- )
   READER-N @ 0<> if
      READER-N @ 0 ?do
         i READ-CHUNK @ SPAN:LEN 0<> if i TRIM i READ-CHUNK @ MEM:FREE-SPAN then
      loop
      READ-CHUNK-RELEASE
      READ-TASK-RELEASE
      READ-USED-RELEASE
      0 READER-N !
   then
   false REGISTERED ! ;


: CELL$ ( PG:result n -- PG:result ptr u8 n ) {: c:n :}
   0 PG:>ROW c PG:>COL PG:NULL? if E-ROW throw then
   0 PG:>ROW c PG:>COL PG:TEXT$ ;


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
   readers READ-CHUNK-RESERVE
   readers READ-TASK-RESERVE
   readers READ-USED-RESERVE
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

\ Starts a read on this task: the spans of its previous read are given up, and
\ every chunk they took but the first.
: READ-RESET ( -- )
   MY-ARENA {: at:n :}
   at READ-CHUNK @ SPAN:LEN 0<> if at TRIM then
   HEADER-BYTES at READ-USED ! ;


: ROWS-OR-THROW ( PG:result -- PG:result )
   PG:OUTCOME MATCH PG:outcome
      ok OF E-QUERY throw ENDOF
      rows OF ENDOF
      failed OF 2drop 2drop E-QUERY throw ENDOF
   ;MATCH ;


\ A reader answers the row it was asked for or a named failure; it never
\ invents fields for an id that names nothing.
: FIRST-ROW ( PG:result -- PG:result )
   ROWS-OR-THROW
   PG:ROWS COUNT>N 0= if E-ROW throw then ;

private

: RUN-FIRST ( R [ R PG:result -- S PG:result ] PG:result -- S PG:result )
   FIRST-ROW swap execute ;

public


\ One parameter, one row: the read of a row by its identity. The body runs on
\ the result once its first row is there; the scope clears the result when
\ the body returns or throws.
: BY-ID ( R PG:connection n ptr u8 n [ R PG:result -- S PG:result ] -- S )
   {: c id:n a:ptr u:n body :}
   READ-RESET
   c PG:PARAMS
   c id PG:INT+
   body c a u [: RUN-FIRST ;] PG:WITH-RESULT ;


\ ---- the first row's columns, copied into the arena ---------------------------

\ SQL NULL is E-ROW; COL-TEXT? is the nullable reader.
: COL-TEXT ( PG:result n -- PG:result ptr u8 n )
   CELL$ ARENA+ ;


\ A nullable text: false means SQL NULL, and the span is then empty.
: COL-TEXT? ( PG:result n -- PG:result ptr u8 n bool ) {: c:n :}
   0 PG:>ROW c PG:>COL PG:NULL? if s" " ARENA+ false exit then
   c COL-TEXT true ;


\ SQL NULL is PG:E-TYPE; COL-ID is the nullable reader.
: COL-INT ( PG:result n -- PG:result n ) {: c:n :}
   0 PG:>ROW c PG:>COL PG:INT ;


\ A number with a fraction. The server renders it as text, so this is where it
\ becomes one again; SQL NULL is zero.
: COL-REAL ( PG:result n -- PG:result r ) {: c:n :}
   0 PG:>ROW c PG:>COL PG:NULL? if 0.0 exit then
   c CELL$ STR>FLOAT MATCH option
      none OF E-ROW throw ENDOF
      some OF ENDOF
   ;MATCH ;


\ A nullable reference: SQL NULL is zero, which an identity column never holds.
: COL-ID ( PG:result n -- PG:result n ) {: c:n :}
   0 PG:>ROW c PG:>COL PG:NULL? if 0 exit then
   c COL-INT ;


: COL-BOOL ( PG:result n -- PG:result bool )
   CELL$ BOOL$ ;


\ ---- any row, read in place ----------------------------------------------------
\ These answer libpq's own bytes, valid until the result's scope ends: a page
\ of rows is read inside its scope, and nothing is copied.

: AT$ ( PG:result n n -- PG:result ptr u8 n ) {: at:n col:n :}
   at PG:>ROW col PG:>COL PG:TEXT$ ;


: AT?$ ( PG:result n n -- PG:result ptr u8 n bool ) {: at:n col:n :}
   at PG:>ROW col PG:>COL PG:NULL? if s" " false exit then
   at col AT$ true ;


: AT-INT ( PG:result n n -- PG:result n ) {: at:n col:n :}
   at PG:>ROW col PG:>COL PG:INT ;


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
: ONE-INT ( PG:connection ptr u8 n -- n )
   [: FIRST-INT ;] PG:WITH-RESULT ;

;package
