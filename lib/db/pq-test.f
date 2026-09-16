\ pq-test.f - package DB against a live PostgreSQL server.
\
\ Run it through the fixture, which starts a throwaway cluster and exports the
\ conninfo:
\
\     test/db/pg-fixture.sh build/hb-pq --load lib/db/pq-test.f
\
\ Without HABU_PG_CONNINFO in the environment the file prints a named skip and
\ asserts nothing. It is registered in test/gate-stdlib-cases.f on those terms:
\ every gate run certifies package DB and prints the skip, and the fixture is
\ how the module is exercised against a real server.

require lib/test.f
require lib/task.f
require lib/image-lifecycle.f
require lib/db/pq.f

package DB-TEST
using DB

-77 constant BOOM                         \ the throw the rolled-back body raises

0 constant TAG-OK
1 constant TAG-ROWS
2 constant TAG-FAILED


: CONNINFO$ ( -- ptr u8 n )
   s" HABU_PG_CONNINFO" GETENV ;


: OPEN ( -- DB:connection )
   CONNINFO$ CONNECT MATCH DB:connect-result
      connected OF ENDOF
      refused OF type cr E-CONNECT throw ENDOF
   ;MATCH ;


\ ---- outcome readers ------------------------------------------------------
: OUTCOME-TAG ( DB:result -- n )
   OUTCOME MATCH DB:outcome
      ok OF TAG-OK ENDOF
      rows OF TAG-ROWS ENDOF
      failed OF 2drop 2drop TAG-FAILED ENDOF
   ;MATCH ;


: FAILED-SQLSTATE$ ( DB:result -- ptr u8 n )
   OUTCOME MATCH DB:outcome
      ok OF s" <ok>" ENDOF
      rows OF s" <rows>" ENDOF
      failed OF 2drop ENDOF
   ;MATCH ;


: FAILED-MESSAGE$ ( DB:result -- ptr u8 n )
   OUTCOME MATCH DB:outcome
      ok OF s" " ENDOF
      rows OF s" " ENDOF
      failed OF 2swap 2drop ENDOF
   ;MATCH ;


: EXEC-TAG ( DB:connection ptr u8 n -- n )
   EXEC dup OUTCOME-TAG swap CLEAR ;


: EXEC-OK ( DB:connection ptr u8 n -- )
   EXEC-TAG TAG-OK T= ;


\ ---- connect --------------------------------------------------------------
\ Port 1 is refused before any protocol runs, so the arm carries libpq's own
\ message and no connection is left behind.
: REFUSED-LENGTH ( -- n )
   s" host=127.0.0.1 port=1 dbname=none user=none connect_timeout=2" CONNECT
   MATCH DB:connect-result
      connected OF DB:CLOSE 0 ENDOF
      refused OF nip ENDOF
   ;MATCH ;


: CONNECT-CASES ( -- )
   REFUSED-LENGTH 0 > TTRUE ;


\ ---- schema ---------------------------------------------------------------
: DDL-CASES ( DB:connection -- DB:connection ) {: c :}
   c s" set client_min_messages = warning" EXEC-OK
   c s" drop table if exists pqt" EXEC-OK
   c s" create table pqt (id int primary key, label text, note text)" EXEC-OK
   c ;


\ ---- parameters -----------------------------------------------------------
: INSERT-ROW ( DB:connection n ptr u8 n -- ) {: c id:n la:ptr lu:n :}
   c PARAMS
   c id INT+
   c la lu TEXT+
   c NULL+
   c s" insert into pqt (id, label, note) values ($1, $2, $3)" EXEC
   dup OUTCOME-TAG TAG-OK T=
   dup AFFECTED COUNT>N 1 T=
   CLEAR ;


: SELECT-CASES ( DB:connection -- DB:connection ) {: c :}
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


: PARAM-CASES ( DB:connection -- DB:connection ) {: c :}
   c 1 s" one" INSERT-ROW
   c 2 s" two" INSERT-ROW
   c SELECT-CASES ;


\ ---- prepared statements --------------------------------------------------
: PREPARED-INSERT ( DB:connection n ptr u8 n -- ) {: c id:n la:ptr lu:n :}
   c PARAMS
   c id INT+
   c la lu TEXT+
   c s" pqt-note" TEXT+
   c s" pqt_ins" EXEC-PREPARED
   dup OUTCOME-TAG TAG-OK T=
   CLEAR ;


: PREPARED-CASES ( DB:connection -- DB:connection ) {: c :}
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
: TX-BODY ( DB:connection -- DB:connection )
   dup s" insert into pqt (id, label, note) values (900, 'rolled', null)" EXEC
   dup OUTCOME-TAG TAG-OK T=
   CLEAR
   BOOM throw ;


: TX-RUN ( DB:connection -- DB:connection )
   dup [: TX-BODY ;] WITH-TRANSACTION ;


: COUNT-ID ( DB:connection n -- n ) {: c id:n :}
   c PARAMS
   c id INT+
   c s" select count(*) from pqt where id = $1" EXEC {: r :}
   r 0 >ROW 0 >COL INT {: found:n :}
   r CLEAR
   found ;


: ROLLBACK-CASES ( DB:connection -- DB:connection ) {: c :}
   c [: TX-RUN ;] catch {: stale code:n :}
   code BOOM T=
   c 900 COUNT-ID 0 T=
   c ;


\ The parameters are built BEFORE the transaction opens, which is the only way
\ into a body that may not read the caller's locals: BEGIN must leave the
\ pending list alone.
: COMMIT-BODY ( DB:connection -- DB:connection )
   dup s" insert into pqt (id, label, note) values ($1, $2, null)" EXEC
   dup OUTCOME-TAG TAG-OK T=
   CLEAR ;


: COMMIT-CASES ( DB:connection -- DB:connection ) {: c :}
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
: UNIQUE-CASES ( DB:connection -- DB:connection ) {: c :}
   c PARAMS
   c s" insert into pqt (id, label, note) values (1, 'duplicate', null)" EXEC {: r :}
   r OUTCOME-TAG TAG-FAILED T=
   r FAILED-SQLSTATE$ s" 23505" T$=
   r FAILED-MESSAGE$ nip 0 > TTRUE
   r CLEAR
   c ;


: READ-BAD-COLUMN ( DB:result -- DB:result )
   dup 0 >ROW 99 >COL TEXT$ 2drop ;


: READ-BAD-ROW ( DB:result -- DB:result )
   dup 99 >ROW 0 >COL TEXT$ 2drop ;


: BAD-COLUMN-CASES ( DB:connection -- DB:connection ) {: c :}
   c PARAMS
   c s" select id from pqt where id = 1" EXEC {: r :}
   r OUTCOME-TAG TAG-ROWS T=
   r [: READ-BAD-COLUMN ;] catch {: stale-col code:n :}
   code E-COLUMN T=
   r [: READ-BAD-ROW ;] catch {: stale-row row-code:n :}
   row-code E-COLUMN T=
   r CLEAR
   c ;


: EXEC-EMPTY ( DB:connection -- DB:connection )
   dup s" " EXEC CLEAR ;


: PREPARE-UNNAMED ( DB:connection -- DB:connection )
   dup s" " EXEC-PREPARED CLEAR ;


\ An empty statement would reach the server as PGRES_EMPTY_QUERY, whose result
\ carries no SQLSTATE and no message at all - measured - so the module refuses
\ it by name instead.
: EMPTY-STATEMENT-CASES ( DB:connection -- DB:connection ) {: c :}
   c [: EXEC-EMPTY ;] catch {: stale-exec code:n :}
   code E-STATEMENT T=
   c [: PREPARE-UNNAMED ;] catch {: stale-name name-code:n :}
   name-code E-STATEMENT T=
   c ;


: READ-ROWS ( DB:result -- DB:result )
   dup ROWS drop ;


: CLEAR-AGAIN ( DB:result -- DB:result )
   dup CLEAR ;


: CLEARED-CASES ( DB:connection -- DB:connection ) {: c :}
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


: BIG-TEXT-CASES ( DB:connection -- DB:connection ) {: c :}
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


: BIG-STATEMENT-CASES ( DB:connection -- DB:connection ) {: c :}
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
: ADD-PAST-CAP ( DB:connection -- DB:connection )
   dup PARAMS
   33 0 ?do dup 1 INT+ loop ;


: PARAM-COUNT-CASES ( DB:connection -- DB:connection ) {: c :}
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
: MULTI-VIA-EXEC-CASES ( DB:connection -- DB:connection ) {: c :}
   c PARAMS
   c s" select 1; select 2" EXEC
   dup OUTCOME-TAG TAG-FAILED T=
   dup FAILED-SQLSTATE$ s" 42601" T$=
   CLEAR
   c ;


: SCRIPT-OK-CASES ( DB:connection -- DB:connection ) {: c :}
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


: SCRIPT-FAIL-CASES ( DB:connection -- DB:connection ) {: c :}
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


: SCRIPT-BODY ( DB:connection -- DB:connection )
   dup s" insert into pqtscript values (9); insert into pqtscript values (10)" SCRIPT
   dup OUTCOME-TAG TAG-OK T=
   CLEAR
   BOOM throw ;


: SCRIPT-TX-RUN ( DB:connection -- DB:connection )
   dup [: SCRIPT-BODY ;] WITH-TRANSACTION ;


: SCRIPT-TX-CASES ( DB:connection -- DB:connection ) {: c :}
   c [: SCRIPT-TX-RUN ;] catch {: stale-tx code:n :}
   code BOOM T=
   c PARAMS
   c s" select count(*) from pqtscript where id in (9, 10)" EXEC {: r :}
   r 0 >ROW 0 >COL INT 0 T=
   r CLEAR
   c ;


: SCRIPT-EMPTY ( DB:connection -- DB:connection )
   dup s" " SCRIPT CLEAR ;


: SCRIPT-WITH-PARAMS ( DB:connection -- DB:connection )
   dup PARAMS
   dup 1 INT+
   dup s" select 1" SCRIPT CLEAR ;


: SCRIPT-REFUSAL-CASES ( DB:connection -- DB:connection ) {: c :}
   c [: SCRIPT-EMPTY ;] catch {: stale-empty code:n :}
   code E-STATEMENT T=
   c [: SCRIPT-WITH-PARAMS ;] catch {: stale-params param-code:n :}
   param-code E-STATEMENT T=
   c PARAMS
   c ;


\ ---- slot recycling -------------------------------------------------------
\ More statements than the registry has result slots: every CLEAR must hand its
\ slot back or the run ends in DB:E-CAPACITY rather than an assertion.
: RECYCLE-CASES ( DB:connection -- DB:connection ) {: c :}
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
   c DB:CLOSE
   r [: READ-ROWS ;] catch {: stale code:n :}
   code E-CLEARED T= ;


\ ---- task ownership -------------------------------------------------------
\ A handle laundered into shared typed storage still belongs to the task that
\ made it. The worker presents the main task's connection and is refused
\ before any libpq call.
1 TYPED-BUFFER SHARED-CONN DB:connection
variable FOREIGN-CODE
variable FOREIGN-DONE
TASK:MIN-STACK TASK:TASK FOREIGN-WORKER


: FOREIGN-USE ( -- )
   0 SHARED-CONN @ PARAMS ;


: FOREIGN-WORK ( -- )
   [: FOREIGN-USE ;] catch FOREIGN-CODE !
   1 FOREIGN-DONE atomic-add drop ;


: OWNER-CASES ( DB:connection -- DB:connection ) {: c :}
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
\ so PREPARE retires every handle. This runs LAST: PREPARE also clears package
\ FFI's symbol cache, and the connection it retires can no longer be closed,
\ so it is deliberately left to the exiting process.
: USE-CONN ( DB:connection -- DB:connection )
   dup PARAMS ;


: IMAGE-CASES ( -- )
   OPEN {: c :}
   c PARAMS
   c s" select 1" EXEC {: r :}
   IMAGE-LIFECYCLE:PREPARE
   c [: USE-CONN ;] catch {: stale-conn code:n :}
   code E-HANDLE T=
   r [: READ-ROWS ;] catch {: stale-res res-code:n :}
   res-code E-CLEARED T= ;


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
   DB:CLOSE
   CLOSE-CLEARS-CASES
   IMAGE-CASES ;


: MAIN ( -- )
   CONNINFO$ nip 0= if
      s" pq-test: skipped, HABU_PG_CONNINFO names no server" type cr exit
   then
   CONNECT-CASES
   SERVER-CASES ;

T-RESET
MAIN
T-REPORT

;using
;package
