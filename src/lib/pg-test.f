\ pg-test.f - package PG against a live PostgreSQL server.
\
\ test/db/pg-cluster.f starts a private cluster and runs this file with the
\ cluster's conninfo as its one script argument; the gate row is that harness:
\
\     bin/hb --load test/db/pg-cluster.f
\
\ Loaded without the argument, the file dies naming the harness. There is no
\ skip.

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/memory.f
require src/lib/fmt.f
require src/lib/process.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/image-lifecycle.f
require src/lib/pg.f

package PG-TEST

TYPED-VARIABLE POLL-FD n

\ Resolve the OS primitive before importing PG's POLL.
: READY? ( fd n -- bool ) {: socket:fd events:n :}
   events 32 lshift socket FD>N $FFFFFFFF and or POLL-FD !
   POLL-FD 1 0 poll {: count:n :}
   count 0 < if PG:E-EXEC throw then
   count 0 > ;

using PG

-77 constant BOOM                         \ the throw the rolled-back body raises
20 constant CONNECTIONS
64 constant RESULTS
32 constant PARAMETERS

CONNECTIONS TYPED-BUFFER HELD-CONNS PG:connection
RESULTS TYPED-BUFFER HELD-RESULTS PG:result
10000000000 constant PROGRESS-NS

0 constant TAG-OK
1 constant TAG-ROWS
2 constant TAG-FAILED


: CONNINFO$ ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;


: OPEN ( -- PG:connection )
   CONNECTIONS RESULTS PARAMETERS CONFIGURE
   CONNINFO$ CONNECT MATCH PG:connect-result
      connected OF ENDOF
      refused OF type cr E-CONNECT throw ENDOF
   ;MATCH ;


\ ---- outcome readers ------------------------------------------------------
: OUTCOME-TAG ( PG:result -- n )
   OUTCOME MATCH PG:outcome
      ok OF TAG-OK ENDOF
      rows OF TAG-ROWS ENDOF
      failed OF 2drop 2drop TAG-FAILED ENDOF
   ;MATCH ;


: FAILED-SQLSTATE$ ( PG:result -- ptr u8 n )
   OUTCOME MATCH PG:outcome
      ok OF s" <ok>" ENDOF
      rows OF s" <rows>" ENDOF
      failed OF 2drop ENDOF
   ;MATCH ;


: FAILED-MESSAGE$ ( PG:result -- ptr u8 n )
   OUTCOME MATCH PG:outcome
      ok OF s" " ENDOF
      rows OF s" " ENDOF
      failed OF 2swap 2drop ENDOF
   ;MATCH ;


: EXEC-TAG ( PG:connection ptr u8 n -- n )
   EXEC dup OUTCOME-TAG swap CLEAR ;


: EXEC-OK ( PG:connection ptr u8 n -- )
   EXEC-TAG TAG-OK T= ;


\ ---- connect --------------------------------------------------------------
\ Port 1 is refused before any protocol runs, so the arm carries libpq's own
\ message and no connection is left behind.
: REFUSED-LENGTH ( -- n )
   s" host=127.0.0.1 port=1 dbname=none user=none connect_timeout=2" CONNECT
   MATCH PG:connect-result
      connected OF PG:CLOSE 0 ENDOF
      refused OF nip ENDOF
   ;MATCH ;

: CLOSE-KEEP ( PG:connection -- PG:connection )
   dup PG:CLOSE ;


: CONNECT-NUL ( -- )
   S\" host=127.0.0.1 port=1\x00 dbname=none" CONNECT-START PG:CLOSE ;


: CONNECT-TEXT-CASES ( -- )
   s" rejected connection text releases its claimed slot" T-LABEL
   CONNECTIONS 1+ 0 ?do [: CONNECT-NUL ;] E-STATEMENT TTHROWSQ loop
   OPEN PG:CLOSE ;


: CONNECT-CASES ( -- )
   REFUSED-LENGTH 0 > TTRUE
   s" host=127.0.0.1 port=1 dbname=none user=none connect_timeout=2"
   CONNECT-START {: refused :}
   refused PG:AWAIT MATCH PG:progress
      refused OF 2drop ENDOF
      connected OF PG:CLOSE E-CONNECT throw ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF CLEAR E-CONNECT throw ENDOF
   ;MATCH
   refused PG:LIVE? TFALSE
   refused [: CLOSE-KEEP ;] catch {: stale code:n :}
   code E-HANDLE T= ;


\ The application can hold all twenty connections at once, and reaching its
\ declared limit leaves the existing connections usable and a closed slot reusable.
: CAPACITY-CASES ( -- )
   s" application-declared connections remain usable at capacity" T-LABEL
   CONNECTIONS 0 ?do OPEN i HELD-CONNS ! loop
   [: OPEN PG:CLOSE ;] E-CAPACITY TTHROWSQ
   [: CONNECTIONS 1+ RESULTS PARAMETERS CONFIGURE ;] E-CAPACITY TTHROWSQ
   0 HELD-CONNS @ {: stale :}
   stale PG:CLOSE
   stale PG:LIVE? TFALSE
   OPEN 0 HELD-CONNS !
   stale PG:LIVE? TFALSE
   0 HELD-CONNS @ PG:LIVE? TTRUE
   CONNECTIONS 0 ?do
      i HELD-CONNS @ {: c :}
      c s" select 42" EXEC {: r :}
      r 0 >ROW 0 >COL INT 42 T=
      r CLEAR c PG:CLOSE
   loop ;


\ The progress helpers. AWAIT-READY, OPEN-POLL and RESULT-POLL run before the
\ AIO loop starts: a single task advances both connections, and a progress call
\ that waits cannot let that task release the lock. CHECK-PROGRESS also ends
\ each turn of EXIT-CASES's wait, while the loop and the exit worker are active.
\ A wait that passes its PROGRESS-NS deadline is no assertion result
\ (docs/gate.md): CHECK-PROGRESS names it and throws E-PROC-TIMEOUT, which the
\ case engine exits with as PROC-TIMEOUT-RC (RUN).
: CHECK-PROGRESS ( n -- )
   {: deadline:n :}
   mono-ns deadline > if s" pg-test CHECK-PROGRESS" TEST-WAIT:EXPIRED then
   TASK:PAUSE ;

: AWAIT-READY ( fd n n -- ) {: socket:fd events:n deadline:n :}
   begin
      socket events READY? if exit then
      deadline CHECK-PROGRESS
   again ;

: OPEN-POLL ( -- PG:connection )
   CONNINFO$ CONNECT-START {: c :}
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin
      c PG:POLL MATCH PG:progress
         waiting OF deadline AWAIT-READY ENDOF
         connected OF exit ENDOF
         refused OF 2drop E-CONNECT throw ENDOF
         completed OF CLEAR E-CONNECT throw ENDOF
      ;MATCH
      deadline CHECK-PROGRESS
   again ;

: RESULT-POLL ( PG:connection -- PG:result ) {: c :}
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin
      c PG:POLL MATCH PG:progress
         waiting OF deadline AWAIT-READY ENDOF
         completed OF exit ENDOF
         connected OF drop E-EXEC throw ENDOF
         refused OF 2drop E-EXEC throw ENDOF
      ;MATCH
      deadline CHECK-PROGRESS
   again ;

: POLL-EXEC ( PG:connection ptr u8 n -- PG:result ) {: c a:ptr u:n :}
   c a u SEND c RESULT-POLL ;

: BUSY-SEND ( PG:connection -- PG:connection )
   dup s" select 2" SEND ;

: PROGRESS-CASES ( -- )
   s" one task releases a lock another query is waiting for" T-LABEL
   OPEN-POLL {: holder :}
   OPEN-POLL {: waiter :}
   holder s" select pg_advisory_lock(987654)" POLL-EXEC CLEAR
   waiter s" set statement_timeout = '10s'" POLL-EXEC CLEAR
   waiter s" select pg_advisory_lock(987654)" SEND
   waiter PG:POLL MATCH PG:progress
      waiting OF 2drop true ENDOF
      completed OF CLEAR false ENDOF
      connected OF drop false ENDOF
      refused OF 2drop false ENDOF
   ;MATCH TTRUE
   waiter [: BUSY-SEND ;] catch {: held code:n :}
   code E-STATEMENT T=
   holder s" select pg_advisory_unlock(987654)::int" POLL-EXEC {: unlocked :}
   unlocked 0 >ROW 0 >COL INT 1 T=
   unlocked CLEAR
   waiter RESULT-POLL {: acquired :}
   acquired OUTCOME-TAG TAG-ROWS T=
   acquired CLEAR
   \ Closing with a query in flight releases its reserved result as well.
   holder s" select pg_advisory_lock(987654)" SEND
   holder PG:CLOSE
   waiter s" select pg_advisory_unlock(987654)::int" POLL-EXEC CLEAR
   waiter PG:CLOSE ;

: RESULT-CAPACITY-CASES ( -- )
   s" closed pending queries leave the declared result capacity available" T-LABEL
   OPEN-POLL {: c :}
   RESULTS 0 ?do
      c s" select 7" POLL-EXEC i HELD-RESULTS !
   loop
   c [: BUSY-SEND ;] catch {: held code:n :}
   code E-CAPACITY T=
   RESULTS 0 ?do i HELD-RESULTS @ CLEAR loop
   c PG:CLOSE ;


\ An AIO wait failure retires the connection while AWAIT throws. The caller
\ still has the CONNECT-START handle for its owner cleanup.
: AWAIT-DROP ( PG:connection -- PG:connection ) {: c :}
   c PG:AWAIT MATCH PG:progress
      waiting OF 2drop ENDOF
      connected OF drop ENDOF
      completed OF CLEAR ENDOF
      refused OF 2drop ENDOF
   ;MATCH
   c ;

: STOPPED-AWAIT ( PG:connection -- PG:connection )
   AIO:STOP
   [: AWAIT-DROP ;] [: AIO:START ;] finally ;

: AIO-FAIL-CASES ( -- )
   OPEN {: c :}
   c s" select pg_sleep(10)" SEND
   c [: STOPPED-AWAIT ;] catch {: stale code:n :}
   code E-AIO-STATE T=
   c PG:LIVE? TFALSE ;


\ ---- schema ---------------------------------------------------------------
: DDL-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" set client_min_messages = warning" EXEC-OK
   c s" drop table if exists pqt" EXEC-OK
   c s" create table pqt (id int primary key, label text, note text)" EXEC-OK
   c ;


\ ---- parameters -----------------------------------------------------------
: INSERT-ROW ( PG:connection n ptr u8 n -- ) {: c id:n la:ptr lu:n :}
   c PARAMS
   c id INT+
   c la lu TEXT+
   c NULL+
   c s" insert into pqt (id, label, note) values ($1, $2, $3)" EXEC
   dup OUTCOME-TAG TAG-OK T=
   dup AFFECTED COUNT>N 1 T=
   CLEAR ;


: SELECT-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c 1 INT+
   c s" select id, label, note from pqt where id = $1" EXEC {: r :}
   r OUTCOME-TAG TAG-ROWS T=
   r ROWS COUNT>N 1 T=
   r COLS COUNT>N 3 T=
   r 0 >COL NAME$ s" id" T$=
   r 1 >COL NAME$ s" label" T$=
   r 0 >ROW 0 >COL INT 1 T=
   r 0 >ROW 1 >COL TEXT$ s" one" T$=
   r 0 >ROW 1 >COL NULL? TFALSE
   r 0 >ROW 2 >COL NULL? TTRUE
   r CLEAR
   c ;


: PARAM-CASES ( PG:connection -- PG:connection ) {: c :}
   c 1 s" one" INSERT-ROW
   c 2 s" two" INSERT-ROW
   c SELECT-CASES ;


: NUL-LEADING ( PG:connection -- PG:connection )
   dup S\" \x00A" TEXT+ ;

: NUL-EMBEDDED ( PG:connection -- PG:connection )
   dup S\" A\x00B" TEXT+ ;

: NUL-TRAILING ( PG:connection -- PG:connection )
   dup S\" A\x00" TEXT+ ;

: TEXT-BOUNDARY-CASES ( PG:connection -- PG:connection ) {: c :}
   s" text parameters reject NUL without losing staged parameters" T-LABEL
   c PARAMS
   c s" kept" TEXT+
   c [: NUL-LEADING ;] catch {: stale-lead lead-code:n :}
   lead-code E-STATEMENT T=
   c [: NUL-EMBEDDED ;] catch {: stale-mid mid-code:n :}
   mid-code E-STATEMENT T=
   c [: NUL-TRAILING ;] catch {: stale-tail tail-code:n :}
   tail-code E-STATEMENT T=
   c s" select $1::text" EXEC {: kept :}
   kept 0 >ROW 0 >COL TEXT$ s" kept" T$=
   kept CLEAR
   s" empty and UTF-8 text parameters round trip exactly" T-LABEL
   c PARAMS
   c s" " TEXT+
   c S\" \xc3\xa9A" TEXT+
   c s" select $1::text, $2::text" EXEC {: exact :}
   exact 0 >ROW 0 >COL TEXT$ s" " T$=
   exact 0 >ROW 1 >COL TEXT$ S\" \xc3\xa9A" T$=
   exact CLEAR
   c ;


\ The server quotes the bad input, so its message outgrows the 1024-byte copy
\ kept for it; the leading 'x' puts that cap inside an e-acute, which the copy
\ must not split: the copy keeps 1023 bytes and goes back as text.
: LONG-MESSAGE-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a capped server message ends on a character and goes back as text" T-LABEL
   c PARAMS
   c s" select ('x' || repeat(chr(233), 700))::int" EXEC {: r :}
   r FAILED-SQLSTATE$ s" 22P02" T$=
   r FAILED-MESSAGE$ {: ma mu:n :}
   mu 1023 T=
   c PARAMS
   c ma mu TEXT+
   c s" select $1::text" EXEC {: back :}
   back OUTCOME-TAG TAG-ROWS T=
   back 0 >ROW 0 >COL TEXT$ ma mu T$=
   back CLEAR
   r CLEAR
   c ;


\ ---- prepared statements --------------------------------------------------
: PREPARED-INSERT ( PG:connection n ptr u8 n -- ) {: c id:n la:ptr lu:n :}
   c PARAMS
   c id INT+
   c la lu TEXT+
   c s" pqt-note" TEXT+
   c s" pqt_ins" EXEC-PREPARED
   dup OUTCOME-TAG TAG-OK T=
   CLEAR ;


: PREPARED-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" pqt_ins" s" insert into pqt (id, label, note) values ($1, $2, $3)" PREPARE
   dup OUTCOME-TAG TAG-OK T=
   CLEAR
   c 10 s" ten" PREPARED-INSERT
   c 11 s" eleven" PREPARED-INSERT
   c PARAMS
   c s" select count(*) from pqt where note = 'pqt-note'" EXEC {: r :}
   r 0 >ROW 0 >COL INT 2 T=
   r CLEAR
   c ;


\ ---- transactions ---------------------------------------------------------
: TX-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqt (id, label, note) values (900, 'rolled', null)" EXEC
   dup OUTCOME-TAG TAG-OK T=
   CLEAR
   BOOM throw ;


: TX-RUN ( PG:connection -- PG:connection )
   dup [: TX-BODY ;] WITH-TRANSACTION ;


: COUNT-ID ( PG:connection n -- n ) {: c id:n :}
   c PARAMS
   c id INT+
   c s" select count(*) from pqt where id = $1" EXEC {: r :}
   r 0 >ROW 0 >COL INT {: found:n :}
   r CLEAR
   found ;


: ROLLBACK-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: TX-RUN ;] catch {: stale code:n :}
   code BOOM T=
   c 900 COUNT-ID 0 T=
   c ;


\ The parameters are built BEFORE the transaction opens, which is the only way
\ into a body that may not read the caller's locals: BEGIN must leave the
\ pending list alone.
: COMMIT-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqt (id, label, note) values ($1, $2, null)" EXEC
   dup OUTCOME-TAG TAG-OK T=
   CLEAR ;


: COMMIT-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c 901 INT+
   c s" kept" TEXT+
   c [: COMMIT-BODY ;] WITH-TRANSACTION
   c 901 COUNT-ID 1 T=
   c PARAMS
   c 901 INT+
   c s" select label from pqt where id = $1" EXEC {: r :}
   r 0 >ROW 0 >COL TEXT$ s" kept" T$=
   r CLEAR
   c ;


\ ---- failure arms ---------------------------------------------------------
: UNIQUE-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c s" insert into pqt (id, label, note) values (1, 'duplicate', null)" EXEC {: r :}
   r OUTCOME-TAG TAG-FAILED T=
   r FAILED-SQLSTATE$ s" 23505" T$=
   r FAILED-MESSAGE$ nip 0 > TTRUE
   r CLEAR
   c ;


: READ-BAD-COLUMN ( PG:result -- PG:result )
   dup 0 >ROW 99 >COL TEXT$ 2drop ;


: READ-BAD-ROW ( PG:result -- PG:result )
   dup 99 >ROW 0 >COL TEXT$ 2drop ;


: BAD-COLUMN-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c s" select id from pqt where id = 1" EXEC {: r :}
   r OUTCOME-TAG TAG-ROWS T=
   r [: READ-BAD-COLUMN ;] catch {: stale-col code:n :}
   code E-COLUMN T=
   r [: READ-BAD-ROW ;] catch {: stale-row row-code:n :}
   row-code E-COLUMN T=
   r CLEAR
   c ;


: EXEC-EMPTY ( PG:connection -- PG:connection )
   dup s" " EXEC CLEAR ;


: PREPARE-UNNAMED ( PG:connection -- PG:connection )
   dup s" " EXEC-PREPARED CLEAR ;


: EXEC-NUL ( PG:connection -- PG:connection )
   dup S\" select $1::text\x00; select 2" EXEC CLEAR ;

: PREPARE-NUL-NAME ( PG:connection -- PG:connection )
   dup S\" p\x00q" s" select 1" PREPARE CLEAR ;

: EXEC-NUL-NAME ( PG:connection -- PG:connection )
   dup S\" p\x00q" EXEC-PREPARED CLEAR ;


: STATEMENT-BOUNDARY-CASES ( PG:connection -- PG:connection ) {: c :}
   s" statements and prepared names reject NUL before libpq" T-LABEL
   c PARAMS
   c s" kept" TEXT+
   c [: EXEC-NUL ;] catch {: stale-sql sql-code:n :}
   sql-code E-STATEMENT T=
   c [: PREPARE-NUL-NAME ;] catch {: stale-prepare prepare-code:n :}
   prepare-code E-STATEMENT T=
   c [: EXEC-NUL-NAME ;] catch {: stale-name name-code:n :}
   name-code E-STATEMENT T=
   c s" select $1::text" EXEC {: r :}
   r 0 >ROW 0 >COL TEXT$ s" kept" T$=
   r CLEAR
   c ;


\ An empty statement would reach the server as PGRES_EMPTY_QUERY, whose result
\ carries no SQLSTATE and no message at all - measured - so the module refuses
\ it by name instead.
: EMPTY-STATEMENT-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: EXEC-EMPTY ;] catch {: stale-exec code:n :}
   code E-STATEMENT T=
   c [: PREPARE-UNNAMED ;] catch {: stale-name name-code:n :}
   name-code E-STATEMENT T=
   c ;


: READ-ROWS ( PG:result -- PG:result )
   dup ROWS drop ;


: CLEAR-AGAIN ( PG:result -- PG:result )
   dup CLEAR ;


: CLEARED-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c s" select 1" EXEC {: r :}
   r CLEAR
   r [: READ-ROWS ;] catch {: stale-read code:n :}
   code E-CLEARED T=
   r [: CLEAR-AGAIN ;] catch {: stale-clear again:n :}
   again E-CLEARED T=
   c ;


\ ---- no ceiling on text ---------------------------------------------------
\ The call arena is sized to the call, so a parameter far larger than any fixed
\ buffer round-trips unchanged and a statement longer than any fixed buffer
\ runs. Only the COUNTS are bounded.
$32000 constant BIG-LEN                   \ 204800 bytes of parameter text
create BIG-BUF BIG-LEN allot

$4000 constant STMT-CAP
create STMT-BUF STMT-CAP allot
variable STMT-U

1500 constant BIG-ROWS
8192 constant OLD-STATEMENT-CAP           \ the ceiling this test proves is gone


: BIG$ ( -- ptr u8 n )
   BIG-BUF BYTE-VIEW BIG-LEN ;


: FILL-BIG ( -- )
   BIG-LEN 0 ?do $41 i 26 mod + BIG-BUF BYTE-VIEW i + c! loop ;


: BIG-TEXT-CASES ( PG:connection -- PG:connection ) {: c :}
   FILL-BIG
   c s" create table pqtbig (id int primary key, body text)" EXEC-OK
   c PARAMS
   c 1 INT+
   c BIG$ TEXT+
   c s" insert into pqtbig (id, body) values ($1, $2)" EXEC-OK
   c PARAMS
   c 1 INT+
   c s" select body, length(body) from pqtbig where id = $1" EXEC {: r :}
   r OUTCOME-TAG TAG-ROWS T=
   r 0 >ROW 1 >COL INT BIG-LEN T=
   r 0 >ROW 0 >COL TEXT$ BIG$ T$=
   r CLEAR
   c ;


: STMT-RESET ( -- )
   0 STMT-U ! ;


: STMT+ ( ptr u8 n -- ) {: a u:n :}
   STMT-U @ u + STMT-CAP > if E-CAPACITY throw then
   a STMT-BUF BYTE-VIEW STMT-U @ + u BYTE-COPY
   STMT-U @ u + STMT-U ! ;


: STMT-C+ ( n -- ) {: ch:n :}
   STMT-U @ 1 + STMT-CAP > if E-CAPACITY throw then
   ch STMT-BUF BYTE-VIEW STMT-U @ + c!
   STMT-U @ 1 + STMT-U ! ;


: STMT-N+ ( n -- ) {: v:n :}
   v 10 < if v $30 + STMT-C+ exit then
   v 10 / RECURSE
   v 10 mod $30 + STMT-C+ ;


: STMT$ ( -- ptr u8 n )
   STMT-BUF BYTE-VIEW STMT-U @ ;


: BIG-VALUES ( -- )
   STMT-RESET
   s" insert into pqtrow (id) values " STMT+
   BIG-ROWS 0 ?do
      i 0 > if s" ," STMT+ then
      s" (" STMT+ i STMT-N+ s" )" STMT+
   loop ;


: BIG-STATEMENT-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" create table pqtrow (id int primary key)" EXEC-OK
   BIG-VALUES
   STMT$ nip OLD-STATEMENT-CAP > TTRUE
   c PARAMS
   c STMT$ EXEC {: r :}
   r OUTCOME-TAG TAG-OK T=
   r AFFECTED COUNT>N BIG-ROWS T=
   r CLEAR
   c ;


\ ---- the counts that remain bounded ---------------------------------------
: ADD-PAST-CAP ( PG:connection -- PG:connection )
   dup PARAMS
   33 0 ?do dup 1 INT+ loop ;


: PARAM-COUNT-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: ADD-PAST-CAP ;] catch {: stale-param code:n :}
   code E-CAPACITY T=
   c PARAMS
   c ;


\ A text length is compared with the arena's room, never added to its fill. One
\ staged parameter is enough for a length near the maximum cell to wrap that sum
\ back under the capacity, and the copy then runs past the arena.
: ADD-HUGE-TEXT ( PG:connection -- PG:connection )
   dup PARAMS
   dup s" kept" TEXT+
   dup BIG$ drop MEM-MAX-N 1- TEXT+ ;


\ With nothing staged the fill is zero and the same length wraps no sum: the
\ arena asks for a MEM-MAX-N-byte mapping, and the mapping is what refuses it.
: ADD-LONE-HUGE-TEXT ( PG:connection -- PG:connection )
   dup PARAMS
   dup BIG$ drop MEM-MAX-N 1- TEXT+ ;


: PARAM-LENGTH-CASES ( PG:connection -- PG:connection ) {: c :}
   s" TEXT+ refuses a length that would wrap the arena fill" T-LABEL
   c [: ADD-HUGE-TEXT ;] catch {: stale-param code:n :}
   code E-CAPACITY T=
   s" TEXT+ of that length on an empty list is refused by the allocator" T-LABEL
   c [: ADD-LONE-HUGE-TEXT ;] catch {: stale-lone lone-code:n :}
   lone-code E-MEM-MAP T=
   c PARAMS
   c ;


\ ---- multi-statement scripts ----------------------------------------------
\ SCRIPT is the only path that takes more than one statement; EXEC and
\ EXEC-PREPARED ride the extended protocol, where a second command is 42601.
\ MEASURED: PostgreSQL runs a multi-statement simple query as ONE implicit
\ transaction, so a failing statement rolls the earlier ones back with it - the
\ script is atomic even outside WITH-TRANSACTION.
: MULTI-VIA-EXEC-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c s" select 1; select 2" EXEC
   dup OUTCOME-TAG TAG-FAILED T=
   dup FAILED-SQLSTATE$ s" 42601" T$=
   CLEAR
   c ;


: SCRIPT-OK-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" drop table if exists pqtscript; create table pqtscript (id int); insert into pqtscript values (7)"
   SCRIPT
   dup OUTCOME-TAG TAG-OK T=
   dup AFFECTED COUNT>N 1 T=
   CLEAR
   c PARAMS
   c s" select count(*) from pqtscript" EXEC {: r :}
   r 0 >ROW 0 >COL INT 1 T=
   r CLEAR
   c ;


: SCRIPT-FAIL-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" insert into pqtscript values (8); insert into pqtnosuch values (1)" SCRIPT
   dup OUTCOME-TAG TAG-FAILED T=
   dup FAILED-SQLSTATE$ s" 42P01" T$=
   dup FAILED-MESSAGE$ nip 0 > TTRUE
   CLEAR
   c PARAMS
   c s" select count(*) from pqtscript where id = 8" EXEC {: r :}
   r 0 >ROW 0 >COL INT 0 T=
   r CLEAR
   c ;


: SCRIPT-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqtscript values (9); insert into pqtscript values (10)" SCRIPT
   dup OUTCOME-TAG TAG-OK T=
   CLEAR
   BOOM throw ;


: SCRIPT-TX-RUN ( PG:connection -- PG:connection )
   dup [: SCRIPT-BODY ;] WITH-TRANSACTION ;


: SCRIPT-TX-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: SCRIPT-TX-RUN ;] catch {: stale-tx code:n :}
   code BOOM T=
   c PARAMS
   c s" select count(*) from pqtscript where id in (9, 10)" EXEC {: r :}
   r 0 >ROW 0 >COL INT 0 T=
   r CLEAR
   c ;


: SCRIPT-EMPTY ( PG:connection -- PG:connection )
   dup s" " SCRIPT CLEAR ;


: SCRIPT-WITH-PARAMS ( PG:connection -- PG:connection )
   dup PARAMS
   dup 1 INT+
   dup s" select 1" SCRIPT CLEAR ;


: SCRIPT-REFUSAL-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: SCRIPT-EMPTY ;] catch {: stale-empty code:n :}
   code E-STATEMENT T=
   c [: SCRIPT-WITH-PARAMS ;] catch {: stale-params param-code:n :}
   param-code E-STATEMENT T=
   c PARAMS
   c ;


\ ---- slot recycling -------------------------------------------------------
\ More statements than the registry has result slots: every CLEAR must hand its
\ slot back or the run ends in PG:E-CAPACITY rather than an assertion.
: RECYCLE-CASES ( PG:connection -- PG:connection ) {: c :}
   64 0 ?do
      c PARAMS
      c s" select 1" EXEC CLEAR
   loop
   c PARAMS
   c s" select count(*) from pqt" EXEC {: r :}
   r OUTCOME-TAG TAG-ROWS T=
   r CLEAR
   c ;


\ CLOSE owns whatever results the connection still holds, so libpq keeps no
\ orphan PGresult and the caller's handle refuses afterwards.
: CLOSE-CLEARS-CASES ( -- )
   OPEN {: c :}
   c PARAMS
   c s" select 1" EXEC {: r :}
   c PG:CLOSE
   r [: READ-ROWS ;] catch {: stale code:n :}
   code E-CLEARED T= ;


\ A backend terminating its own session may fail the terminating query or the
\ next query. Either path leaves no connection for owner cleanup to close.
: END-SESSION ( PG:connection -- PG:connection )
   dup s" select pg_terminate_backend(pg_backend_pid())" EXEC CLEAR ;

: PROBE-SESSION ( PG:connection -- PG:connection )
   dup s" select 1" EXEC CLEAR ;

: LOST-CONNECTION-CASES ( -- )
   OPEN {: c :}
   c [: END-SESSION ;] catch {: held code:n :}
   code 0= if
      c [: PROBE-SESSION ;] catch {: again probe-code:n :}
      probe-code E-EXEC T=
   else
      code E-EXEC T=
   then
   c PG:LIVE? TFALSE ;


\ ---- task ownership -------------------------------------------------------
\ A handle laundered into shared typed storage still belongs to the task that
\ made it. The worker presents the main task's connection and is refused
\ before any libpq call.
1 TYPED-BUFFER SHARED-CONN PG:connection
variable FOREIGN-CODE
variable FOREIGN-DONE
TASK:MIN-STACK TASK:TASK FOREIGN-WORKER


: FOREIGN-USE ( -- )
   0 SHARED-CONN @ PG:LIVE? drop ;


: FOREIGN-WORK ( -- )
   [: FOREIGN-USE ;] catch FOREIGN-CODE !
   1 FOREIGN-DONE atomic-add drop ;


: OWNER-CASES ( PG:connection -- PG:connection ) {: c :}
   c 0 SHARED-CONN !
   0 FOREIGN-CODE !
   0 FOREIGN-DONE !
   ['] FOREIGN-WORK FOREIGN-WORKER TASK:ACTIVATE
   begin FOREIGN-DONE atomic@ 1 < while TASK:PAUSE repeat
   FOREIGN-WORKER TASK:KILL
   FOREIGN-CODE @ E-HANDLE T=
   c ;


\ The task owns the returned CONNECT-START handle. Its AT-EXIT callback must
\ close a live attempt and leave an auto-retired refusal alone.
1 TYPED-BUFFER EXIT-CONN PG:connection
TYPED-VARIABLE EXIT-LIVE bool
variable EXIT-STARTED
variable EXIT-BACKEND
variable EXIT-RELEASE
TASK:MIN-STACK TASK:TASK EXIT-WORKER

: EXIT-CLEAN ( -- )
   0 EXIT-CONN @ PG:LIVE? dup EXIT-LIVE !
   if 0 EXIT-CONN @ PG:CLOSE then ;

: EXIT-OPEN ( -- )
   CONNINFO$ CONNECT-START 0 EXIT-CONN !
   0 TASK:RETURN ;

: EXIT-REFUSED ( -- )
   s" host=127.0.0.1 port=1 dbname=none user=none connect_timeout=2"
   CONNECT-START 0 EXIT-CONN !
   0 EXIT-CONN @ PG:AWAIT MATCH PG:progress
      refused OF 2drop ENDOF
      connected OF PG:CLOSE E-CONNECT throw ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF CLEAR E-CONNECT throw ENDOF
   ;MATCH
   0 TASK:RETURN ;

: EXIT-SEND ( -- )
   CONNINFO$ CONNECT-START 0 EXIT-CONN !
   0 EXIT-CONN @ PG:AWAIT MATCH PG:progress
      connected OF drop ENDOF
      refused OF 2drop E-CONNECT throw ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF CLEAR E-CONNECT throw ENDOF
   ;MATCH
   0 EXIT-CONN @ s" select pg_backend_pid()" EXEC {: backend :}
   backend 0 >ROW 0 >COL INT EXIT-BACKEND atomic!
   backend CLEAR
   0 EXIT-CONN @ s" update pqtcancel set value=1 where id=1" SEND
   1 EXIT-STARTED atomic! ;

: EXIT-HALT ( -- )
   EXIT-SEND
   0 EXIT-CONN @ PG:AWAIT MATCH PG:progress
      completed OF CLEAR ENDOF
      connected OF drop E-EXEC throw ENDOF
      waiting OF 2drop E-EXEC throw ENDOF
      refused OF 2drop E-EXEC throw ENDOF
   ;MATCH
   0 TASK:RETURN ;

: EXIT-THROW ( -- )
   EXIT-SEND
   begin EXIT-RELEASE atomic@ 0= while TASK:PAUSE repeat
   BOOM throw ;

: EXIT-JOIN ( -- )
   EXIT-WORKER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

\ Returns once EXIT-HALT has sent its query, or has ended without sending it:
\ a worker that threw is no deadline, and its code reaches EXIT-CASES's join.
: AWAIT-STARTED ( -- )
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin
      EXIT-STARTED atomic@ 0= EXIT-WORKER TASK:DONE? 0= and while
      deadline CHECK-PROGRESS
   repeat ;

\ A worker that has not sent its query when AWAIT-STARTED passes its deadline
\ may be waiting in PG:AWAIT with an AIO record in flight, and MAIN's AIO:STOP
\ would meet that record and throw E-AIO-BUSY. So the worker is halted and
\ joined before the deadline leaves, as the normal path halts it: its scrub
\ cancels the record (src/lib/aio.f SCRUB) and EXIT-CLEAN closes the
\ connection. The halt's own answer, E-TASK-NO-RESULT, is no assertion, since
\ the deadline came first. Any other code the join carries, from the body or
\ EXIT-CLEAN, is named on one line, and the deadline goes on.
: STARTED-OR-HALT ( -- )
   [: AWAIT-STARTED ;] catch {: code:n :}
   code 0= if exit then
   EXIT-WORKER TASK:HALT
   EXIT-WORKER TASK:JOIN MATCH result
      ok OF drop ENDOF
      err OF
         dup E-TASK-NO-RESULT = if drop else
            s" pg-test EXIT-HALT: the halted worker threw " type FMT:.INT cr
         then
      ENDOF
   ;MATCH
   code throw ;

: EXIT-CASES ( -- )
   ['] EXIT-CLEAN EXIT-WORKER TASK:AT-EXIT
   ['] EXIT-OPEN EXIT-WORKER TASK:ACTIVATE
   EXIT-JOIN
   EXIT-LIVE @ TTRUE
   0 EXIT-CONN @ PG:LIVE? TFALSE
   ['] EXIT-REFUSED EXIT-WORKER TASK:ACTIVATE
   EXIT-JOIN
   EXIT-LIVE @ TFALSE
   0 EXIT-CONN @ PG:LIVE? TFALSE ;

\ The server-side boundary: closing a pending connection must cancel its
\ blocked write, not merely discard its client handle. A second connection
\ stays blocked on a different row until the holder releases both locks.
: BACKEND-COUNT ( PG:connection n ptr u8 n -- n )
   {: c pid:n sql:ptr size:n :}
   c PARAMS c pid INT+
   c sql size EXEC {: r :}
   r 0 >ROW 0 >COL INT
   r CLEAR ;

: LOCKED? ( PG:connection n -- bool )
   s" select count(*) from pg_stat_activity where pid=$1 and wait_event_type='Lock'"
   BACKEND-COUNT 1 = ;

: AWAIT-LOCK ( PG:connection n -- )
   {: c pid:n :}
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin c pid LOCKED? 0= while deadline CHECK-PROGRESS repeat ;

: AWAIT-GONE ( PG:connection n -- )
   {: c pid:n :}
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin
      c pid s" select count(*) from pg_stat_activity where pid=$1" BACKEND-COUNT 0 >
   while deadline CHECK-PROGRESS repeat ;

: BACKEND ( PG:connection -- n )
   s" select pg_backend_pid()" EXEC {: r :}
   r 0 >ROW 0 >COL INT
   r CLEAR ;

: CANCEL-CASES ( PG:connection -- PG:connection )
   {: c :}
   s" halt cancels a blocked write without cancelling another connection" T-LABEL
   c s" drop table if exists pqtcancel" EXEC-OK
   c s" create table pqtcancel (id int primary key, value int)" EXEC-OK
   c s" insert into pqtcancel values (1,0),(2,0)" EXEC-OK
   OPEN {: holder :}
   OPEN {: peer :}
   peer BACKEND {: peer-pid:n :}
   holder s" begin" EXEC-OK
   holder s" select * from pqtcancel for update" EXEC CLEAR
   peer s" update pqtcancel set value=1 where id=2" SEND
   c peer-pid AWAIT-LOCK
   0 EXIT-STARTED !
   ['] EXIT-HALT EXIT-WORKER TASK:ACTIVATE
   STARTED-OR-HALT
   c EXIT-BACKEND atomic@ AWAIT-LOCK
   EXIT-WORKER TASK:HALT
   EXIT-WORKER TASK:JOIN MATCH result
      ok OF drop E-EXEC throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   EXIT-LIVE @ TTRUE
   0 EXIT-CONN @ PG:LIVE? TFALSE
   c EXIT-BACKEND atomic@ AWAIT-GONE
   c peer-pid LOCKED? TTRUE
   holder s" rollback" EXEC-OK
   peer PG:AWAIT MATCH PG:progress
      completed OF dup OUTCOME-TAG TAG-OK T= CLEAR ENDOF
      connected OF PG:CLOSE E-EXEC throw ENDOF
      waiting OF 2drop E-EXEC throw ENDOF
      refused OF 2drop E-EXEC throw ENDOF
   ;MATCH
   c s" select value from pqtcancel order by id" EXEC {: values :}
   values 0 >ROW 0 >COL INT 0 T=
   values 1 >ROW 0 >COL INT 1 T=
   values CLEAR
   holder s" begin" EXEC-OK
   holder s" select * from pqtcancel for update" EXEC CLEAR
   s" a statement timeout returns its SQL error and leaves the connection usable" T-LABEL
   peer s" set statement_timeout='20ms'" EXEC-OK
   peer s" update pqtcancel set value=2 where id=1" EXEC {: timed :}
   timed OUTCOME-TAG TAG-FAILED T=
   timed FAILED-SQLSTATE$ s" 57014" T$=
   timed CLEAR
   peer PG:LIVE? TTRUE
   peer s" set statement_timeout=0" EXEC-OK
   s" direct close cancels a pending blocked write" T-LABEL
   peer s" update pqtcancel set value=2 where id=1" SEND
   c peer-pid AWAIT-LOCK
   peer PG:CLOSE
   c peer-pid AWAIT-GONE
   holder s" rollback" EXEC-OK
   s" pending close cleanup preserves the task body's exception" T-LABEL
   holder s" begin" EXEC-OK
   holder s" select * from pqtcancel for update" EXEC CLEAR
   0 EXIT-STARTED !
   0 EXIT-RELEASE !
   ['] EXIT-THROW EXIT-WORKER TASK:ACTIVATE
   STARTED-OR-HALT
   c EXIT-BACKEND atomic@ AWAIT-LOCK
   1 EXIT-RELEASE atomic!
   EXIT-WORKER TASK:JOIN MATCH result
      ok OF drop E-EXEC throw ENDOF
      err OF BOOM T= ENDOF
   ;MATCH
   c EXIT-BACKEND atomic@ AWAIT-GONE
   holder s" rollback" EXEC-OK
   holder PG:CLOSE
   c s" select value from pqtcancel where id=1" EXEC {: kept :}
   kept 0 >ROW 0 >COL INT 0 T=
   kept CLEAR
   c ;


\ ---- image capture --------------------------------------------------------
\ A restored image runs in another process where the libpq pointers are gone,
\ so PREPARE closes native resources and retires every handle. A new registry
\ cannot revive a stale handle even when it reuses the same slot number.
: USE-CONN ( PG:connection -- PG:connection )
   dup PARAMS ;


\ A capture runs after the program's tasks stop: PREPARE ends the process on a
\ task still activated (docs/threads.md), and the AIO loop is one. CAPTURE stops
\ it, prepares, and starts it again however PREPARE ends, so the loop is live
\ for the connections opened afterwards and for MAIN's cleanup.
: CAPTURE ( -- )
   AIO:STOP
   [: IMAGE-LIFECYCLE:PREPARE ;] [: AIO:START ;] finally ;


: IMAGE-CASES ( -- )
   OPEN {: c :}
   c s" drop table if exists pqtimage" EXEC-OK
   c s" create table pqtimage (value int)" EXEC-OK
   c s" insert into pqtimage values (0)" EXEC-OK
   c s" begin" EXEC-OK
   c s" select * from pqtimage for update" EXEC CLEAR
   OPEN {: waiter :}
   waiter BACKEND {: waiter-pid:n :}
   waiter s" update pqtimage set value=1" SEND
   OPEN {: observer :}
   observer waiter-pid AWAIT-LOCK
   c PARAMS
   c s" select 1" EXEC {: r :}
   CAPTURE
   c PG:LIVE? TFALSE
   c [: USE-CONN ;] catch {: stale-conn code:n :}
   code E-HANDLE T=
   r [: READ-ROWS ;] catch {: stale-res res-code:n :}
   res-code E-CLEARED T=
   OPEN {: fresh :}
   fresh PG:LIVE? TTRUE
   fresh s" select value from pqtimage" EXEC {: kept :}
   kept 0 >ROW 0 >COL INT 0 T=
   kept CLEAR
   c PG:LIVE? TFALSE
   fresh s" select 2" EXEC {: current :}
   c [: USE-CONN ;] catch {: again-conn again-code:n :}
   again-code E-HANDLE T=
   r [: READ-ROWS ;] catch {: again-res again-res-code:n :}
   again-res-code E-CLEARED T=
   current CLEAR fresh PG:CLOSE ;


\ ---- the live suite -------------------------------------------------------
: SERVER-CASES ( -- )
   OPEN
   DDL-CASES
   PARAM-CASES
   TEXT-BOUNDARY-CASES
   LONG-MESSAGE-CASES
   PREPARED-CASES
   ROLLBACK-CASES
   COMMIT-CASES
   UNIQUE-CASES
   BAD-COLUMN-CASES
   EMPTY-STATEMENT-CASES
   STATEMENT-BOUNDARY-CASES
   BIG-TEXT-CASES
   BIG-STATEMENT-CASES
   PARAM-COUNT-CASES
   PARAM-LENGTH-CASES
   MULTI-VIA-EXEC-CASES
   SCRIPT-OK-CASES
   SCRIPT-FAIL-CASES
   SCRIPT-TX-CASES
   SCRIPT-REFUSAL-CASES
   CLEARED-CASES
   OWNER-CASES
   EXIT-CASES
   CANCEL-CASES
   LOST-CONNECTION-CASES
   RECYCLE-CASES
   PG:CLOSE
   CLOSE-CLEARS-CASES
   IMAGE-CASES
   \ A later connection must arm cleanup for the next capture too.
   IMAGE-CASES ;


\ The loop's stop with a deadline pending. The exit worker EXIT-CASES halted
\ has had its records cancelled, but on Linux a cancel completes through the
\ loop (src/lib/aio.f CANCEL-STAGE), so the stop retries E-AIO-BUSY, its "not
\ yet". A stop still busy after PROGRESS-NS is this wait's own deadline, named
\ and thrown as a timeout. Any other code is named on one line and LATE-STOP
\ returns: the pending deadline stays the case's result, where under finally
\ the stop's code would replace it.
: LATE-STOP ( -- )
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin
      [: AIO:STOP ;] catch {: code:n :}
      code 0= if exit then
      code E-AIO-BUSY <> if
         s" pg-test LATE-STOP: AIO:STOP threw " type code FMT:.INT cr exit
      then
      mono-ns deadline > if s" pg-test LATE-STOP" TEST-WAIT:EXPIRED then
      TASK:PAUSE
   again ;

: LOOP-CASES ( -- )
   CONNECT-CASES CONNECT-TEXT-CASES CAPACITY-CASES AIO-FAIL-CASES SERVER-CASES ;

\ The AIO loop stops however LOOP-CASES ends. As under finally, a code from
\ AIO:STOP replaces the cases' code, except a deadline's: with a deadline
\ pending, LATE-STOP stops the loop or names why it could not, and the deadline
\ goes on.
: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" pg-test: no conninfo argument; test/db/pg-cluster.f runs this file" 2 die
   then
   CONNECTIONS RESULTS PARAMETERS CONFIGURE
   PROGRESS-CASES
   RESULT-CAPACITY-CASES
   AIO:START
   [: LOOP-CASES ;] catch {: code:n :}
   code E-PROC-TIMEOUT = if LATE-STOP E-PROC-TIMEOUT throw then
   AIO:STOP
   code 0<> if code throw then ;

\ The case engine's entry. A throw code does not cross the process boundary:
\ an uncaught one exits UNCAUGHT-RC (67), as a failure does. So a progress wait
\ past its deadline, which CHECK-PROGRESS has named on stdout, ends the engine
\ with PROC-TIMEOUT-RC (124), the case engine's timeout outcome, which
\ test/db/pg-cluster.f reads as a late step. Every other code is rethrown.
: RUN ( -- )
   ['] MAIN catch {: code:n :}
   code E-PROC-TIMEOUT = if s" " PROC-TIMEOUT-RC die then
   code 0<> if code throw then ;

T-RESET
RUN
T-REPORT

;using
;package
