split-str
\ sqlite.f - SQLite through libsqlite3, bound through the FFI FUNCTION:
\ declarer, package SQLITE. A connection is the sqlite3 pointer; only the task
\ that opened it uses it. A statement is a linear token that lives inside
\ WITH-STATEMENT, which finalizes it when the body returns or throws.
\ Every failure is logged once, tagged sqlite, with SQLite's message and
\ extended code, then thrown: E-CORRUPT for a primary code CORRUPT or NOTADB,
\ E-FULL for FULL, E-SQL for any other. A misuse SQLite cannot see (a column
\ outside the row, a count past a C int, a NUL in a path or a text, a text with
\ no statement or more than one) is logged with its own text and is E-SQL.
\ A WITH-STATEMENT body that hands back another statement is E-HANDLE.
\ STORAGE CLASS. TASK-LOCAL: the cells sqlite3_open_v2 and sqlite3_prepare_v2
\ write their outputs to. CALLER-OWNED: connections and statements.

require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/memory.f
require src/lib/type/deftype.f
require src/lib/task.f
require src/lib/log.f

package SQLITE

private
-9620 constant E-FIRST
-9629 constant E-LAST

public
-9620 s" the database file is malformed or is not a database"
   ERROR:CODE E-CORRUPT
-9621 s" the database or its disk is full" ERROR:CODE E-FULL
-9622 s" SQLite refused the call" ERROR:CODE E-SQL
-9623 s" a statement scope's body handed back another statement"
   ERROR:CODE E-HANDLE

NEWTYPE connection ^u8
DEFLINEAR SQLITE:statement

private

CAST: >CONNECTION ( ^u8 -- connection )
CAST: DB ( connection -- ^u8 )
LINEAR: >STATEMENT ( ^u8 -- SQLITE:statement )
LINEAR: STATEMENT>PTR ( SQLITE:statement -- ^u8 )

\ sqlite3.h facts.
0 constant OK
100 constant ROW
101 constant DONE
7 constant NOMEM
11 constant CORRUPT
13 constant FULL
26 constant NOTADB
$FF constant PRIMARY-MASK          \ an extended code's low byte is its primary
5 constant NULL-TYPE               \ SQLITE_NULL, a column's fundamental type
1 constant READONLY                \ SQLITE_OPEN_* flags
2 constant READWRITE
4 constant OPEN-CREATE
$01000000 constant NOFOLLOW
-1 constant TRANSIENT              \ SQLITE_TRANSIENT: SQLite copies the bytes
$7FFFFFFF constant INT-MAX

TASK:+USER OUT-CELL ^u8
TASK:+USER TAIL-CELL ^u8

\ Every pointer is `^u8`, passed back or read at the width sqlite3.h states.
VERSIONED-LIBRARY sqlite3 0

FUNCTION: LIB-OPEN sqlite3_open_v2 ( ^u8 ^u8 n ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-CLOSE sqlite3_close ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-BUSY sqlite3_busy_timeout ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-ERRMSG sqlite3_errmsg ( ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: LIB-EXTENDED sqlite3_extended_errcode ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-PREPARE sqlite3_prepare_v2
   ( ^u8 str ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-STEP sqlite3_step ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-RESET sqlite3_reset ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-FINALIZE sqlite3_finalize ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-STMT-DB sqlite3_db_handle ( ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: LIB-BIND-INT sqlite3_bind_int64 ( ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: LIB-BIND-TEXT sqlite3_bind_text ( ^u8 n str n -- i32 ) ;FUNCTION
FUNCTION: LIB-BIND-BLOB sqlite3_bind_blob ( ^u8 n str n -- i32 ) ;FUNCTION
FUNCTION: LIB-BIND-NULL sqlite3_bind_null ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-DATA-COUNT sqlite3_data_count ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: LIB-COL-INT sqlite3_column_int64 ( ^u8 n -- n ) ;FUNCTION
FUNCTION: LIB-COL-TEXT sqlite3_column_text ( ^u8 n -- ^u8 ) ;FUNCTION
FUNCTION: LIB-COL-BLOB sqlite3_column_blob ( ^u8 n -- ^u8 ) ;FUNCTION
FUNCTION: LIB-COL-BYTES sqlite3_column_bytes ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-COL-TYPE sqlite3_column_type ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: LIB-CHANGES sqlite3_changes64 ( ^u8 -- n ) ;FUNCTION
FUNCTION: LIB-LAST-ROWID sqlite3_last_insert_rowid ( ^u8 -- n ) ;FUNCTION
FUNCTION: LIB-AUTOCOMMIT sqlite3_get_autocommit ( ^u8 -- i32 ) ;FUNCTION


: NULL-ARG ( -- ^u8 ) NULL-PTR BYTE-VIEW ;

: NULL-ADDR? ( ^u8 -- bool ) FFI:>CELL 0= ;

: LOG-OPEN ( -- ) LOG:ERROR s" sqlite" LOG:LINE ;

\ db's last failure, read before any other call on db replaces it.
: REPORT ( ^u8 -- )
   {: db:^ :}
   LOG-OPEN
   db LIB-ERRMSG dup ZLEN LOG:TEXT+
   s"  (extended code " LOG:TEXT+
   db LIB-EXTENDED LOG:INT+
   s" )" LOG:TEXT+ LOG:;LINE ;

: CLASS ( n -- n ) PRIMARY-MASK and {: p:n :}
   p CORRUPT = p NOTADB = or if E-CORRUPT exit then
   p FULL = if E-FULL exit then
   E-SQL ;

: FAIL ( n ^u8 -- )
   {: rc:n db:^ :}
   db REPORT rc CLASS throw ;

: CHECK ( n ^u8 -- )
   {: rc:n db:^ :}
   rc OK <> if rc db FAIL then ;

: MISUSE ( str -- )
   {: a:^ u:n :}
   LOG-OPEN a u LOG:TEXT+ LOG:;LINE E-SQL throw ;

: OUTSIDE ( n str -- )
   {: v:n a:^ u:n :}
   LOG-OPEN v LOG:INT+ a u LOG:TEXT+ LOG:;LINE E-SQL throw ;

\ A count or index SQLite takes as a C int, which would otherwise be cut.
: C-INT ( n -- n )
   {: v:n :}
   v 0 < v INT-MAX > or if v s"  is outside a C int" OUTSIDE then
   v ;

: STMT-CHECK ( n ^u8 -- ) LIB-STMT-DB CHECK ;

: PEEK ( SQLITE:statement -- SQLITE:statement ^u8 )
   STATEMENT>PTR dup >STATEMENT swap ;

\ A statement whose last step answered a row can still fail as it
\ finishes: a write with RETURNING commits here.
: FINALIZE ( ^u8 -- )
   {: stmt:^ :}
   stmt LIB-STMT-DB {: db:^ :}
   stmt LIB-FINALIZE db CHECK ;

\ ---- opening -------------------------------------------------------------

: NUL? ( str -- bool )
   {: a:^ u:n :}
   u 0 ?do a i + c@ 0= if true unloop exit then loop
   false ;

\ sqlite3_open_v2 answers a handle even when it fails, NULL only when out of
\ memory; only its close frees it.
: OPEN-AT ( str n ^u8 NUM:alloc-byte-len -- connection )
   drop {: a:^ u:n flags:n z:^ :}
   a z u BYTE-COPY  0 z u + c!
   z OUT-CELL BYTE-VIEW flags NULL-ARG LIB-OPEN
   OUT-CELL @ {: rc:n db:^ :}
   rc OK <> if db REPORT db LIB-CLOSE drop rc CLASS throw then
   db >CONNECTION ;

: OPEN-WITH ( str n -- connection )
   {: a:^ u:n flags:n :}
   a u NUL? if s" a path holds a NUL byte" MISUSE then
   a u flags u 1+ MEM:BYTES-ALLOC-LEN [: OPEN-AT ;] MEM:WITH-BYTES ;

\ ---- statements ----------------------------------------------------------

\ The text's first statement, NULL when only blanks and comments remain, and
\ the address after it.
: PREPARE ( connection str -- ^u8 ^u8 )
   {: c:connection a:^ u:n :}
   c DB {: db:^ :}
   db a u C-INT OUT-CELL BYTE-VIEW TAIL-CELL BYTE-VIEW LIB-PREPARE
   db CHECK
   OUT-CELL @ TAIL-CELL @ ;

\ A failed step is reset at once, so the statement's next RESET and its
\ finalize answer only their own failures; that reset repeats the step's.
: STEP? ( ^u8 -- bool )
   {: stmt:^ :}
   stmt LIB-STEP {: rc:n :}
   rc ROW = if true exit then
   rc DONE = if false exit then
   stmt LIB-STMT-DB REPORT
   stmt LIB-RESET drop
   rc CLASS throw ;

: DRAIN ( ^u8 -- )
   {: stmt:^ :}
   begin stmt STEP? while repeat ;

: RUN-ONE ( ^u8 -- )
   {: stmt:^ :}
   [: stmt DRAIN ;] [: stmt FINALIZE ;] finally ;

: EXEC-NEXT ( str connection -- str )
   {: a:^ u:n c:connection :}
   c a u PREPARE {: stmt:^ tail:^ :}
   stmt NULL-ADDR? if a u + 0 exit then
   stmt RUN-ONE
   tail  a u + tail - ;

\ SQLite stops reading a text at its first NUL.
: WHOLE ( str -- ) NUL? if s" a text holds a NUL byte" MISUSE then ;

\ The text's statement and the address after it.
: FIRST ( connection str -- ^u8 ^u8 )
   {: c:connection a:^ u:n :}
   a u WHOLE
   c a u PREPARE {: stmt:^ tail:^ :}
   stmt NULL-ADDR? if s" the text holds no statement" MISUSE then
   stmt tail ;

\ Runs in the first statement's scope, whose cleanup finalizes that one.
: ALONE ( connection str -- )
   PREPARE drop {: next:^ :}
   next NULL-ADDR? if exit then
   next FINALIZE
   s" a text holds more than one statement" MISUSE ;

: RUN-BODY
   ( R SQLITE:statement [ R SQLITE:statement -- S SQLITE:statement ]
     connection str -- S ^u8 )
   ALONE execute STATEMENT>PTR ;

\ A zero-length value needs a base: SQLite binds NULL for a NULL one.
: BASE ( str -- ^u8 )
   {: a:^ u:n :}
   u 0= if s" " drop exit then
   a ;

: COL-CHECK ( ^u8 n -- )
   {: stmt:^ col:n :}
   col 0 < col stmt LIB-DATA-COUNT >= or if
      col s"  is no column of the current row" OUTSIDE
   then ;

\ A NULL column and a zero-length blob answer a NULL base; a NULL with NOMEM
\ set is a failure.
: BYTES-CHECK ( ^u8 ^u8 -- )
   {: p:^ stmt:^ :}
   p NULL-ADDR? 0= if exit then
   stmt LIB-STMT-DB {: db:^ :}
   db LIB-EXTENDED PRIMARY-MASK and NOMEM = if NOMEM db FAIL then ;

: COL-BYTES ( ^u8 n ^u8 -- str )
   {: stmt:^ col:n p:^ :}
   p stmt BYTES-CHECK
   p stmt col LIB-COL-BYTES ;

public

\ ---- connections ---------------------------------------------------------

: OPEN ( str -- connection )
   READWRITE OPEN-CREATE or NOFOLLOW or OPEN-WITH ;

: OPEN-READ ( str -- connection )
   READONLY NOFOLLOW or OPEN-WITH ;

\ SQLite refuses to close a connection with a live statement.
: CLOSE ( connection -- )
   DB {: db:^ :}
   db LIB-CLOSE db CHECK ;

: BUSY-MS! ( n connection -- )
   {: ms:n c:connection :}
   c DB {: db:^ :}
   db ms C-INT LIB-BUSY db CHECK ;

\ Every statement in the text, in order; their rows are discarded.
: EXEC ( connection str -- )
   {: c:connection a:^ u:n :}
   a u WHOLE
   a u begin dup 0 > while c EXEC-NEXT repeat 2drop ;

\ Rolls back the connection's transaction if one is open: SQLite may have
\ rolled it back itself already, as it does on FULL.
: ROLLBACK-OPEN ( connection -- )
   {: c:connection :}
   c DB LIB-AUTOCOMMIT 0= if c s" ROLLBACK" EXEC then ;

private

: COMMIT-AFTER ( R connection [ R -- S ] -- S )
   {: c:connection body :}
   body execute
   c s" COMMIT" EXEC ;

\ Begins with the text; a body or COMMIT that fails is rolled back, and its
\ error goes on.
: TRANSACTION ( R connection str [ R -- S ] -- S )
   {: c:connection a:^ u:n body :}
   c a u EXEC
   c body [: COMMIT-AFTER ;] [: c ROLLBACK-OPEN ;] finally ;

public

\ The text's one statement, prepared for the body.
: WITH-STATEMENT
   ( R connection str [ R SQLITE:statement -- S SQLITE:statement ] -- S )
   {: c:connection a:^ u:n body :}
   c a u FIRST {: stmt:^ tail:^ :}
   stmt >STATEMENT body c tail a u + tail -
   [: RUN-BODY stmt <> if E-HANDLE throw then ;]
   [: stmt FINALIZE ;] finally ;

\ BEGIN IMMEDIATE: takes the write lock at once and holds off every other
\ writer until it ends. For a body that writes.
: WITH-TRANSACTION ( R connection [ R -- S ] -- S )
   {: c:connection body :}
   c s" BEGIN IMMEDIATE" body TRANSACTION ;

\ BEGIN DEFERRED: no lock until the first read, no write lock unless the
\ body writes. For a body that only reads, as one snapshot.
: WITH-READ ( R connection [ R -- S ] -- S )
   {: c:connection body :}
   c s" BEGIN DEFERRED" body TRANSACTION ;

: CHANGES ( connection -- n ) DB LIB-CHANGES ;

: LAST-ROWID ( connection -- n ) DB LIB-LAST-ROWID ;

\ ---- a statement ---------------------------------------------------------

: STEP ( SQLITE:statement -- SQLITE:statement bool )
   PEEK STEP? ;

: RESET ( SQLITE:statement -- SQLITE:statement )
   PEEK {: stmt:^ :}
   stmt LIB-RESET stmt STMT-CHECK ;

\ Value, then SQLite's 1-based index.
: BIND-INT ( SQLITE:statement n n -- SQLITE:statement )
   {: v:n idx:n :}
   PEEK {: stmt:^ :}
   stmt idx C-INT v LIB-BIND-INT stmt STMT-CHECK ;

: BIND-TEXT ( SQLITE:statement str n -- SQLITE:statement )
   {: a:^ u:n idx:n :}
   PEEK {: stmt:^ :}
   stmt idx C-INT a u BASE u C-INT TRANSIENT LIB-BIND-TEXT
   stmt STMT-CHECK ;

: BIND-BLOB ( SQLITE:statement str n -- SQLITE:statement )
   {: a:^ u:n idx:n :}
   PEEK {: stmt:^ :}
   stmt idx C-INT a u BASE u C-INT TRANSIENT LIB-BIND-BLOB
   stmt STMT-CHECK ;

: BIND-NULL ( SQLITE:statement n -- SQLITE:statement )
   {: idx:n :}
   PEEK {: stmt:^ :}
   stmt idx C-INT LIB-BIND-NULL stmt STMT-CHECK ;

\ Columns count from 0. A text or blob is borrowed until the next STEP or
\ RESET, or a COL-TEXT of the column a blob was read from.
: COL-INT ( SQLITE:statement n -- SQLITE:statement n )
   {: col:n :}
   PEEK {: stmt:^ :}
   stmt col COL-CHECK
   stmt col LIB-COL-INT ;

: COL-TEXT ( SQLITE:statement n -- SQLITE:statement str )
   {: col:n :}
   PEEK {: stmt:^ :}
   stmt col COL-CHECK
   stmt col stmt col LIB-COL-TEXT COL-BYTES ;

: COL-BLOB ( SQLITE:statement n -- SQLITE:statement str )
   {: col:n :}
   PEEK {: stmt:^ :}
   stmt col COL-CHECK
   stmt col stmt col LIB-COL-BLOB COL-BYTES ;

: COL-NULL? ( SQLITE:statement n -- SQLITE:statement bool )
   {: col:n :}
   PEEK {: stmt:^ :}
   stmt col COL-CHECK
   stmt col LIB-COL-TYPE NULL-TYPE = ;

;package
