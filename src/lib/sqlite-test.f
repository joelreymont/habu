split-str
\ sqlite-test.f - package SQLITE on database files under the test's own
\ temporary directory.
\ Run: bin/hb --load src/lib/sqlite-test.f
\ Failure modes: a bound value reads back as another kind or loses bytes;
\ CHANGES or LAST-ROWID miss an insert; EXEC runs only a text's first
\ statement; a transaction keeps a failed body's rows, swallows its throw or
\ stays open; WITH-READ holds off a writer; a throw in a statement's scope
\ leaves the statement live, so CLOSE refuses; a malformed file, a full
\ database or a held lock is thrown as another class; OPEN-READ follows a
\ symlink, or writes.
\ A reused statement keeps a failed step's error, or logs it twice; a write
\ that fails as it finishes, a RETURNING insert's or a COMMIT's, is dropped;
\ a body hands back another scope's statement unnoticed; a text with a NUL or
\ two statements runs in part; OPEN follows a symlink; an empty bind reads
\ back NULL, or a bound buffer is read when the statement steps.

require src/lib/string.f
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/test/stderr.f
require src/lib/sqlite.f

package SQLITE-TEST

create ROOT PATH-CAP allot
variable ROOT-U
create MAIN-PATH PATH-CAP allot
create LINK-PATH PATH-CAP allot
create SIDE-PATH PATH-CAP allot
create LOG-PATH PATH-CAP allot
4096 constant LOG-CAP
create LOG-BUF LOG-CAP allot
create HELD 3 allot

: ROOT$ ( -- str ) ROOT ROOT-U @ ;

: SETUP ( -- )
   s" hb-sqlite" FS:MKTEMP-DIR {: path:^ u:n :}
   path ROOT u BYTE-COPY
   u ROOT-U ! ;

: AT ( str ^u8 -- str )
   {: name:^ nu:n buf:^ :}
   ROOT$ name nu buf FS:JOIN-PATH {: u:n :}
   buf u ;

: MAIN$ ( -- str ) s" main.db" MAIN-PATH AT ;
: LINK$ ( -- str ) s" link.db" LINK-PATH AT ;
: LOG$ ( -- str ) s" log.txt" LOG-PATH AT ;
: HELD$ ( -- str ) HELD 3 ;

\ A blob with a 0 byte inside: its length crosses the FFI, never a NUL.
: BLOB$ ( -- str ) S\" a\x00b\xff" ;

: GARBAGE$ ( -- str )
   s" plain text where SQLite expects its header" ;

\ The answer of a one-row, one-column query.
: COUNT ( SQLITE:connection str -- n )
   [: SQLITE:STEP TTRUE 0 SQLITE:COL-INT swap ;] SQLITE:WITH-STATEMENT ;

\ ---- 1: binds, columns, CHANGES, LAST-ROWID --------------------------------

: BIND-ROW ( SQLITE:statement -- SQLITE:statement )
   42 1 SQLITE:BIND-INT
   s" text" 2 SQLITE:BIND-TEXT
   BLOB$ 3 SQLITE:BIND-BLOB
   4 SQLITE:BIND-NULL
   SQLITE:STEP TFALSE ;

: READ-ROW ( SQLITE:statement -- SQLITE:statement )
   SQLITE:STEP TTRUE
   0 SQLITE:COL-INT 42 T=
   1 SQLITE:COL-TEXT s" text" T$=
   2 SQLITE:COL-BLOB BLOB$ T$=
   3 SQLITE:COL-NULL? TTRUE
   0 SQLITE:COL-NULL? TFALSE
   SQLITE:STEP TFALSE ;

\ Row 40 comes first, so the bound insert's rowid is 41, which no count equals.
: ROUND-TRIP ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" CREATE TABLE t (i INTEGER, s TEXT, b BLOB, z)" SQLITE:EXEC
   c s" INSERT INTO t (rowid, i) VALUES (40, 0)" SQLITE:EXEC
   c s" INSERT INTO t VALUES (?, ?, ?, ?)" [: BIND-ROW ;]
   SQLITE:WITH-STATEMENT
   c SQLITE:CHANGES 1 T=
   c SQLITE:LAST-ROWID 41 T=
   c s" SELECT i, s, b, z FROM t WHERE rowid = 41" [: READ-ROW ;]
   SQLITE:WITH-STATEMENT ;

\ ---- 6f: an empty bind and a copied bind -----------------------------------

: BIND-EMPTY ( SQLITE:statement -- SQLITE:statement )
   NULL-PTR BYTE-VIEW 0 1 SQLITE:BIND-TEXT
   s" abc" drop HELD 3 BYTE-COPY
   HELD$ 2 SQLITE:BIND-BLOB
   s" xyz" drop HELD 3 BYTE-COPY
   SQLITE:STEP TFALSE ;

: READ-EMPTY ( SQLITE:statement -- SQLITE:statement )
   SQLITE:STEP TTRUE
   0 SQLITE:COL-NULL? TFALSE
   0 SQLITE:COL-TEXT nip 0 T=
   1 SQLITE:COL-BLOB s" abc" T$=
   SQLITE:STEP TFALSE ;

: EMPTY-AND-COPY ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" CREATE TABLE e (s TEXT, b BLOB)" SQLITE:EXEC
   c s" INSERT INTO e VALUES (?, ?)" [: BIND-EMPTY ;]
   SQLITE:WITH-STATEMENT
   c s" SELECT s, b FROM e" [: READ-EMPTY ;] SQLITE:WITH-STATEMENT ;

\ ---- 2: EXEC runs every statement -----------------------------------------

: TWO-STATEMENTS ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" CREATE TABLE u (v INTEGER); INSERT INTO u VALUES (1);" SQLITE:EXEC
   c s" SELECT count(*) FROM u" COUNT 1 T= ;

\ ---- 6d: a text SQLite cannot see whole runs none of it --------------------

: TWO-IN-SCOPE ( SQLITE:connection -- )
   s" INSERT INTO u VALUES (32); INSERT INTO u VALUES (33)"
   [: SQLITE:STEP TFALSE ;] SQLITE:WITH-STATEMENT ;

: NUL-TEXT ( SQLITE:connection -- )
   S\" INSERT INTO u VALUES (30);\x00INSERT INTO u VALUES (31)"
   SQLITE:EXEC ;

: WHOLE-TEXT ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   [: c NUL-TEXT ;] SQLITE:E-SQL TTHROWSQ
   [: c TWO-IN-SCOPE ;] SQLITE:E-SQL TTHROWSQ
   c s" SELECT count(*) FROM u WHERE v >= 30" COUNT 0 T= ;

\ ---- 6a: a statement reused, a failed step included ------------------------

: INSERT-AGAIN ( SQLITE:statement n -- SQLITE:statement )
   1 SQLITE:BIND-INT SQLITE:STEP TFALSE SQLITE:RESET ;

\ The UNIQUE violation is the one line the run logs.
: DUPLICATE ( SQLITE:statement -- SQLITE:statement )
   1 1 SQLITE:BIND-INT
   [: SQLITE:STEP drop ;] catch SQLITE:E-SQL T=
   SQLITE:RESET ;

: REUSE-BODY ( SQLITE:statement -- SQLITE:statement )
   1 INSERT-AGAIN 2 INSERT-AGAIN DUPLICATE 3 INSERT-AGAIN ;

: REUSE-RUN ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" CREATE TABLE r (v INTEGER UNIQUE)" SQLITE:EXEC
   c s" INSERT INTO r VALUES (?)" [: REUSE-BODY ;]
   SQLITE:WITH-STATEMENT ;

: LINES ( str -- n )
   {: a:^ u:n :}
   0 u 0 ?do a i + c@ $0A = if 1+ then loop ;

: REUSE ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   LOG$ STDERR:CAPTURE
   [: c REUSE-RUN ;] [: STDERR:;CAPTURE ;] finally
   LOG$ LOG-BUF LOG-CAP FS:READ-ALL {: n:n :}
   LOG-BUF n LINES 1 T=
   c s" SELECT count(*) FROM r" COUNT 3 T= ;

\ ---- 3: WITH-TRANSACTION --------------------------------------------------

: FAILING-BODY ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" INSERT INTO u VALUES (11)" SQLITE:EXEC
   c s" INSERT INTO nowhere VALUES (1)" SQLITE:EXEC ;

: FAILING-TX ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c [: c FAILING-BODY ;] SQLITE:WITH-TRANSACTION ;

: TX-ROLLBACK ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   [: c FAILING-TX ;] SQLITE:E-SQL TTHROWSQ
   c s" SELECT count(*) FROM u WHERE v = 11" COUNT 0 T= ;

\ Runs after TX-ROLLBACK: its BEGIN fails while that transaction is still open.
\ The count is a second connection's (6g), which sees only a committed row.
: TX-COMMIT ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c [: c s" INSERT INTO u VALUES (10)" SQLITE:EXEC ;]
   SQLITE:WITH-TRANSACTION
   MAIN$ SQLITE:OPEN {: other:SQLITE:connection :}
   other s" SELECT count(*) FROM u WHERE v = 10" COUNT 1 T=
   other SQLITE:CLOSE ;

\ ---- 3a: WITH-READ takes no write lock ------------------------------------

: FAILING-READ ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c [: c FAILING-BODY ;] SQLITE:WITH-READ ;

: READ-ROLLBACK ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   [: c FAILING-READ ;] SQLITE:E-SQL TTHROWSQ
   c s" SELECT count(*) FROM u WHERE v = 11" COUNT 0 T= ;

\ a reads a row, then b, which waits for no lock, asks for the write lock.
: READ-THEN-WRITE ( SQLITE:connection SQLITE:connection -- )
   {: a:SQLITE:connection b:SQLITE:connection :}
   a s" SELECT count(*) FROM u" COUNT drop
   b s" BEGIN IMMEDIATE" SQLITE:EXEC
   b s" ROLLBACK" SQLITE:EXEC ;

: WRITE-HELD ( SQLITE:connection SQLITE:connection -- )
   {: a:SQLITE:connection b:SQLITE:connection :}
   a [: a b READ-THEN-WRITE ;] SQLITE:WITH-TRANSACTION ;

: WRITE-REFUSED ( SQLITE:connection SQLITE:connection -- )
   {: a:SQLITE:connection b:SQLITE:connection :}
   [: a b WRITE-HELD ;] SQLITE:E-SQL TTHROWSQ ;

\ SQLITE_BUSY is primary code 5, which the one logged line names.
: BUSY ( SQLITE:connection SQLITE:connection -- )
   {: a:SQLITE:connection b:SQLITE:connection :}
   LOG$ STDERR:CAPTURE
   [: a b WRITE-REFUSED ;]
   [: STDERR:;CAPTURE ;] finally
   LOG$ LOG-BUF LOG-CAP FS:READ-ALL {: n:n :}
   LOG-BUF n LINES 1 T=
   LOG-BUF n s" (extended code 5)" CONTAINS? TTRUE ;

: READ-TX ( -- )
   MAIN$ SQLITE:OPEN {: a:SQLITE:connection :}
   MAIN$ SQLITE:OPEN {: b:SQLITE:connection :}
   0 b SQLITE:BUSY-MS!
   a READ-ROLLBACK
   a [: a b READ-THEN-WRITE ;] SQLITE:WITH-READ
   a b BUSY
   a SQLITE:CLOSE b SQLITE:CLOSE ;

\ ---- 6c: a body hands back another scope's statement ----------------------

: SWAP-INNER ( SQLITE:statement SQLITE:connection -- SQLITE:statement )
   s" SELECT 2" [: swap ;] SQLITE:WITH-STATEMENT ;

: SWAPPED ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" SELECT 1" [: c SWAP-INNER ;] SQLITE:WITH-STATEMENT ;

: ON-MAIN ( -- )
   MAIN$ SQLITE:OPEN {: c:SQLITE:connection :}
   c ROUND-TRIP c EMPTY-AND-COPY c TWO-STATEMENTS c WHOLE-TEXT
   c REUSE c TX-ROLLBACK c TX-COMMIT
   [: c SWAPPED ;] SQLITE:E-HANDLE TTHROWSQ
   c SQLITE:CLOSE ;

\ ---- 4: a throw in WITH-STATEMENT finalizes --------------------------------

\ Inside the scope the statement is live, so SQLite refuses the close.
: CLOSE-REFUSED ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   [: c SQLITE:CLOSE ;] SQLITE:E-SQL TTHROWSQ ;

: THROW-INSIDE ( SQLITE:connection -- )
   {: c:SQLITE:connection :}
   c s" SELECT v FROM u WHERE v = ?"
   [: c CLOSE-REFUSED 1 99 SQLITE:BIND-INT ;] SQLITE:WITH-STATEMENT ;

: FINALIZED ( -- )
   MAIN$ SQLITE:OPEN {: c:SQLITE:connection :}
   [: c THROW-INSIDE ;] SQLITE:E-SQL TTHROWSQ
   c SQLITE:CLOSE ;

\ ---- 6b: a write that fails as it finishes --------------------------------

\ A read in an open transaction keeps a SHARED lock on the rollback journal,
\ so a writer's commit is refused while it lasts.
: HOLD-READ ( SQLITE:connection -- )
   {: b:SQLITE:connection :}
   b s" BEGIN" SQLITE:EXEC
   b s" SELECT count(*) FROM u" SQLITE:EXEC ;

: RETURNING ( SQLITE:connection -- )
   s" INSERT INTO u VALUES (20) RETURNING v"
   [: SQLITE:STEP TTRUE ;] SQLITE:WITH-STATEMENT ;

: COMMIT-REFUSED ( SQLITE:connection -- )
   {: a:SQLITE:connection :}
   a [: a s" INSERT INTO u VALUES (21)" SQLITE:EXEC ;]
   SQLITE:WITH-TRANSACTION ;

: COMMIT-AGAIN ( SQLITE:connection -- )
   {: a:SQLITE:connection :}
   a [: a s" INSERT INTO u VALUES (22)" SQLITE:EXEC ;]
   SQLITE:WITH-TRANSACTION ;

: BUSY-FINISH ( -- )
   MAIN$ SQLITE:OPEN {: a:SQLITE:connection :}
   MAIN$ SQLITE:OPEN {: b:SQLITE:connection :}
   0 a SQLITE:BUSY-MS!
   b HOLD-READ
   [: a RETURNING ;] SQLITE:E-SQL TTHROWSQ
   [: a COMMIT-REFUSED ;] SQLITE:E-SQL TTHROWSQ
   b s" ROLLBACK" SQLITE:EXEC
   a s" SELECT count(*) FROM u WHERE v IN (20, 21)" COUNT 0 T=
   a COMMIT-AGAIN
   b s" SELECT count(*) FROM u WHERE v = 22" COUNT 1 T=
   a SQLITE:CLOSE b SQLITE:CLOSE ;

\ ---- 5: the three classes -------------------------------------------------

: QUERY-GARBAGE ( -- )
   s" garbage.db" SIDE-PATH AT SQLITE:OPEN {: c:SQLITE:connection :}
   [: c s" SELECT * FROM sqlite_master" SQLITE:EXEC ;]
   [: c SQLITE:CLOSE ;] finally ;

: GARBAGE ( -- )
   s" garbage.db" SIDE-PATH AT GARBAGE$ FS:WRITE-ALL
   [: QUERY-GARBAGE ;] SQLITE:E-CORRUPT TTHROWSQ ;

: FULL ( -- )
   s" full.db" SIDE-PATH AT SQLITE:OPEN {: c:SQLITE:connection :}
   c s" CREATE TABLE f (b BLOB); PRAGMA max_page_count = 2" SQLITE:EXEC
   [: c s" INSERT INTO f VALUES (zeroblob(100000))" SQLITE:EXEC ;]
   SQLITE:E-FULL TTHROWSQ
   c SQLITE:CLOSE ;

\ The insert that the lock refused succeeds once the lock is gone.
: LOCKED ( -- )
   MAIN$ SQLITE:OPEN {: a:SQLITE:connection :}
   MAIN$ SQLITE:OPEN {: b:SQLITE:connection :}
   0 b SQLITE:BUSY-MS!
   a s" BEGIN IMMEDIATE" SQLITE:EXEC
   [: b s" INSERT INTO u VALUES (12)" SQLITE:EXEC ;] SQLITE:E-SQL TTHROWSQ
   a s" ROLLBACK" SQLITE:EXEC
   b s" INSERT INTO u VALUES (12)" SQLITE:EXEC
   a SQLITE:CLOSE b SQLITE:CLOSE ;

\ ---- 6, 6e: a symlink, and OPEN-READ ---------------------------------------

: OPEN-LINK ( -- ) LINK$ SQLITE:OPEN-READ SQLITE:CLOSE ;
: OPEN-LINK-RW ( -- ) LINK$ SQLITE:OPEN SQLITE:CLOSE ;

: READ-ONLY ( -- )
   MAIN$ LINK$ FS:MAKE-SYMLINK
   [: OPEN-LINK ;] SQLITE:E-SQL TTHROWSQ
   [: OPEN-LINK-RW ;] SQLITE:E-SQL TTHROWSQ
   MAIN$ SQLITE:OPEN-READ {: c:SQLITE:connection :}
   [: c s" UPDATE u SET v = 0" SQLITE:EXEC ;] SQLITE:E-SQL TTHROWSQ
   c s" SELECT count(*) FROM u WHERE v = 0" COUNT 0 T=
   c SQLITE:CLOSE ;

: CASES ( -- )
   ON-MAIN READ-TX FINALIZED BUSY-FINISH GARBAGE FULL LOCKED READ-ONLY ;

: RUN ( -- )
   T-RESET SETUP
   [: CASES ;] catch {: code:n :}
   ROOT$ FS:REMOVE-TREE
   code 0<> if code throw then
   T-REPORT ;

RUN
;package
