\ pg-test.f - package PG against a live PostgreSQL server.
\
\ Run it through the fixture, which starts a throwaway cluster and exports the
\ conninfo:
\
\     test/db/pg-fixture.sh build/hb-pg --load lib/pg-test.f
\
\ Without HABU_PG_CONNINFO in the environment the file prints a named skip and
\ asserts nothing. It is registered in test/gate-stdlib-cases.f on those terms:
\ every gate run certifies package PG and prints the skip, and the fixture is
\ how the module is exercised against a real server.

require lib/test.f
require lib/task.f
require lib/aio.f
require lib/image-lifecycle.f
require lib/pg.f

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
   s" HABU_PG_CONNINFO" GETENV ;


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


: CONNECT-CASES ( -- )
   REFUSED-LENGTH 0 > TTRUE ;


\ The application can hold all twenty connections at once, and reaching its
\ declared limit leaves the existing connections usable and a closed slot reusable.
: CAPACITY-CASES ( -- )
   s" application-declared connections remain usable at capacity" T-LABEL
   CONNECTIONS 0 ?do OPEN i HELD-CONNS ! loop
   [: OPEN PG:CLOSE ;] E-CAPACITY TTHROWSQ
   [: CONNECTIONS 1+ RESULTS PARAMETERS CONFIGURE ;] E-CAPACITY TTHROWSQ
   0 HELD-CONNS @ PG:CLOSE
   OPEN 0 HELD-CONNS !
   CONNECTIONS 0 ?do
      i HELD-CONNS @ {: c :}
      c s" select 42" EXEC {: r :}
      r 0 >ROW 0 >COL INT 42 T=
      r CLEAR c PG:CLOSE
   loop ;


\ These helpers run before the AIO loop starts. A single task advances both
\ connections; a progress call that waits cannot let that task release the lock.
: CHECK-PROGRESS ( n -- ) {: deadline:n :}
   mono-ns deadline > if E-EXEC throw then
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


\ ---- task ownership -------------------------------------------------------
\ A handle laundered into shared typed storage still belongs to the task that
\ made it. The worker presents the main task's connection and is refused
\ before any libpq call.
1 TYPED-BUFFER SHARED-CONN PG:connection
variable FOREIGN-CODE
variable FOREIGN-DONE
TASK:MIN-STACK TASK:TASK FOREIGN-WORKER


: FOREIGN-USE ( -- )
   0 SHARED-CONN @ PARAMS ;


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


\ ---- image capture --------------------------------------------------------
\ A restored image runs in another process where the libpq pointers are gone,
\ so PREPARE closes native resources and retires every handle. A new registry
\ cannot revive a stale handle even when it reuses the same slot number.
: USE-CONN ( PG:connection -- PG:connection )
   dup PARAMS ;


: IMAGE-CASES ( -- )
   OPEN {: c :}
   c PARAMS
   c s" select 1" EXEC {: r :}
   IMAGE-LIFECYCLE:PREPARE
   c [: USE-CONN ;] catch {: stale-conn code:n :}
   code E-HANDLE T=
   r [: READ-ROWS ;] catch {: stale-res res-code:n :}
   res-code E-CLEARED T=
   OPEN {: fresh :}
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
   PREPARED-CASES
   ROLLBACK-CASES
   COMMIT-CASES
   UNIQUE-CASES
   BAD-COLUMN-CASES
   EMPTY-STATEMENT-CASES
   BIG-TEXT-CASES
   BIG-STATEMENT-CASES
   PARAM-COUNT-CASES
   MULTI-VIA-EXEC-CASES
   SCRIPT-OK-CASES
   SCRIPT-FAIL-CASES
   SCRIPT-TX-CASES
   SCRIPT-REFUSAL-CASES
   CLEARED-CASES
   OWNER-CASES
   RECYCLE-CASES
   PG:CLOSE
   CLOSE-CLEARS-CASES
   IMAGE-CASES
   \ A later connection must arm cleanup for the next capture too.
   IMAGE-CASES ;


: MAIN ( -- )
   CONNINFO$ nip 0= if
      s" pg-test: skipped, HABU_PG_CONNINFO names no server" type cr exit
   then
   CONNECTIONS RESULTS PARAMETERS CONFIGURE
   PROGRESS-CASES
   RESULT-CAPACITY-CASES
   AIO:START
   [: CONNECT-CASES CAPACITY-CASES SERVER-CASES ;]
   [: AIO:STOP ;]
   finally ;

T-RESET
MAIN
T-REPORT

;using
;package
