\ pg-rows-test.f - package PG's row readers and statements against a live
\ PostgreSQL server.
\
\ test/db/pg-cluster.f starts a private cluster and runs this file, after
\ src/lib/pg-test.f, with the cluster's conninfo as its one script argument:
\
\     bin/hb --load test/db/pg-cluster.f
\
\ Loaded without the argument, the file dies naming the harness. There is no
\ skip. The main task reads records whose spans must outlive their result's
\ scope, every NULL arm and every rejection, and runs EXEC and QUERY with the
\ rejection text each leaves; two worker tasks then read at the same time and
\ each must keep its own bytes and rejection text, and a third worker is one
\ reader past the declaration, whose EXEC must send nothing.
\
\ Both sides are connected at once. The second reads only while the first holds
\ its record, and each looks at its bytes again only after both have read: with
\ one arena between them the second read would have copied its name over the
\ first's.

require src/core/exit-status.f
require src/lib/test.f
require src/lib/string.f
require src/lib/float.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/pg.f
require src/lib/pg-rows.f

package PG-TEST

3 constant CONNECTIONS            \ the most this file holds at once
64 constant RESULTS               \ live results across every connection
3 constant READERS                \ the main task and two workers
\ More reads than RESULTS, so a rejection that left its result behind would
\ end in PG:E-CAPACITY before the loop does.
70 constant OVER-RESULTS
$5000 constant LONG-TEXT          \ a text longer than an arena's first chunk

TYPED-VARIABLE CONN PG:connection


: CONNINFO$ ( -- str )
   0 SCRIPT-ARGV$ ;


: OPEN ( str -- PG:connection )
   CONNECTIONS RESULTS PG:CONFIGURE
   s" " PG:CONNECT MATCH PG:connect-result
      connected OF ENDOF
      rejected OF drop PG:E-CONNECT throw ENDOF
   ;MATCH ;

: PERSON-SQL$ ( -- str )
   s" select id, name, nick, score, boss, active from person where id = $1" ;


: RUN ( str -- ) {: s:str :}
   CONN @ PG:PARAMS
   CONN @ s [: ;] PG:WITH-RESULT ;


: SEED ( -- )
   s" create table person (id bigint primary key, name text not null, nick text, score float8, boss bigint, active boolean not null)" RUN
   s" insert into person values (1, 'Ada', 'ada', 0.5, null, true), (2, 'Grace', null, null, 1, false)" RUN ;


: NAMES-ROW ( PG:result -- str str bool PG:result )
   1 PG:COL-TEXT swap
   2 PG:COL-TEXT? {: k:str given:bool :}
   k swap given swap ;


: NAMES-OF ( PG:connection n -- str str bool )
   PERSON-SQL$ [: NAMES-ROW ;] PG:BY-ID ;


: CHURNED ( str str bool PG:result -- PG:result )
   >r {: n:str k:str given:bool :}
   s" name after clear" T-LABEL
   n s" Ada" T$=
   s" nick after clear" T-LABEL
   k s" ada" T$=
   given TTRUE
   r> ;


\ BY-ID's scope has cleared the result before the spans are read. A second
\ result of the same shape is held while they are compared, so a span still
\ pointing into the freed one would read that result's bytes.
: OUTLIVES-CASES ( -- )
   CONN @ 1 NAMES-OF
   CONN @ PG:PARAMS
   CONN @ s" select 1::bigint, 'Zed', 'zed', 0.25::float8, null::bigint, false"
   [: CHURNED ;] PG:WITH-RESULT ;


: ON-PERSON ( n [ PG:result -- PG:result ] -- ) {: id:n body :}
   CONN @ id PERSON-SQL$ body PG:BY-ID ;


: ADA-ROW ( PG:result -- PG:result )
   0 PG:COL-INT 1 T=
   1 PG:COL-TEXT s" Ada" T$=
   2 PG:COL-TEXT? TTRUE s" ada" T$=
   3 PG:COL-REAL 0.5 f= TTRUE
   s" a NULL reference reads as zero" T-LABEL
   4 PG:COL-ID 0 T=
   5 PG:COL-BOOL TTRUE ;


: ADA-CASES ( -- )
   1 [: ADA-ROW ;] ON-PERSON ;


: GRACE-ROW ( PG:result -- PG:result )
   s" a NULL nullable text is empty and false" T-LABEL
   2 PG:COL-TEXT? TFALSE STR:LEN 0 T=
   s" a NULL real is zero" T-LABEL
   3 PG:COL-REAL 0.0 f= TTRUE
   4 PG:COL-ID 1 T=
   5 PG:COL-BOOL TFALSE ;


\ Each rejection throws out of its own scope, which clears the result.
: NULL-TEXT ( -- )
   2 [: 2 PG:COL-TEXT drop ;] ON-PERSON ;

: NULL-BOOL ( -- )
   2 [: 2 PG:COL-BOOL drop ;] ON-PERSON ;

: NULL-INT ( -- )
   2 [: 3 PG:COL-INT drop ;] ON-PERSON ;


: GRACE-CASES ( -- )
   2 [: GRACE-ROW ;] ON-PERSON
   s" COL-TEXT rejects NULL" T-LABEL
   [: NULL-TEXT ;] PG:E-ROW TTHROWSQ
   s" COL-BOOL rejects NULL" T-LABEL
   [: NULL-BOOL ;] PG:E-ROW TTHROWSQ
   s" COL-INT of NULL is PG's rejection" T-LABEL
   [: NULL-INT ;] PG:E-TYPE TTHROWSQ ;


: PAGE-ROWS ( PG:result -- PG:result )
   PG:ROWS-OR-THROW
   2 0 PG:AT-INT 3 T=
   0 1 PG:AT-BOOL TFALSE
   1 1 PG:AT-BOOL TTRUE
   0 2 PG:AT$ s" v1" T$=
   2 2 PG:AT?$ TTRUE s" v3" T$=
   s" a NULL cell in place is empty and false" T-LABEL
   1 2 PG:AT?$ TFALSE STR:LEN 0 T= ;


: PAGE-CASES ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select i, i % 2 = 0, case when i = 2 then null else 'v' || i end from generate_series(1, 3) i"
   [: PAGE-ROWS ;] PG:WITH-RESULT
   CONN @ PG:PARAMS
   CONN @ s" select count(*) from person" PG:ONE-INT 2 T= ;


: LONG-SQL$ ( -- str )
   s" select $1::int, 'head', repeat('a', $1::int), repeat('b', 3 * $1::int)" ;


: LONG-ROW ( PG:result -- str str str PG:result )
   1 PG:COL-TEXT swap
   2 PG:COL-TEXT swap
   3 PG:COL-TEXT swap ;


\ The short text stays in the arena's first chunk while each long one chains
\ another.
: LONG-CHURNED ( str str str PG:result -- PG:result )
   >r {: h:str a:str b:str :}
   s" a short text beside long ones" T-LABEL
   h s" head" T$=
   s" a text longer than the first chunk reads whole" T-LABEL
   r> 0 2 PG:AT$ a T$=
   a STR:LEN LONG-TEXT T=
   s" a text of several chunks reads whole" T-LABEL
   0 3 PG:AT$ b T$=
   b STR:LEN LONG-TEXT 3 * T= ;


: LONG-READ ( -- )
   CONN @ LONG-TEXT LONG-SQL$ [: LONG-ROW ;] PG:BY-ID
   CONN @ PG:PARAMS
   CONN @ LONG-TEXT PG:INT+
   CONN @ LONG-SQL$ [: LONG-CHURNED ;] PG:WITH-RESULT ;


\ The second read gives up the first one's chunks and chains its own.
: LONG-CASES ( -- )
   LONG-READ
   LONG-READ ;


: EACH-REJECTS ( [ -- ] n -- ) {: q want:n :}
   OVER-RESULTS 0 ?do q want TTHROWSQ loop ;


: NO-SUCH ( -- )
   CONN @ 99 NAMES-OF drop drop drop ;


: REJECTED-QUERY ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select nothing from nowhere" [: PG:ROWS-OR-THROW ;] PG:WITH-RESULT ;


: NO-ROWS-COMMAND ( -- )
   CONN @ PG:PARAMS
   CONN @ s" update person set nick = nick where id = 99" [: PG:ROWS-OR-THROW ;] PG:WITH-RESULT ;


: NULL-COUNT ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select null::int" PG:ONE-INT drop ;


: EMPTY-COUNT ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select 1 where false" PG:ONE-INT drop ;


\ A binary bool is the one byte 01, which a text read would take for false.
: BINARY-BOOL ( -- )
   CONN @ PG:PARAMS
   CONN @ PG:BINARY-RESULT
   CONN @ s" select true" [: 0 PG:COL-BOOL drop ;] PG:WITH-RESULT ;


\ The decoder throws inside BY-ID's scope, which still clears the result.
: BAD-DECODE ( -- )
   CONN @ 1 PERSON-SQL$
   [: 1 PG:COL-BOOL swap ;] PG:BY-ID drop ;


: REJECTION-CASES ( -- )
   s" E-ROW on no row" T-LABEL
   [: NO-SUCH ;] PG:E-ROW EACH-REJECTS
   s" E-QUERY on a rejected query" T-LABEL
   [: REJECTED-QUERY ;] PG:E-QUERY EACH-REJECTS
   s" E-QUERY on a command with no rows" T-LABEL
   [: NO-ROWS-COMMAND ;] PG:E-QUERY EACH-REJECTS
   s" ONE-INT rejects a NULL" T-LABEL
   [: NULL-COUNT ;] PG:E-ROW EACH-REJECTS
   s" ONE-INT rejects an empty result" T-LABEL
   [: EMPTY-COUNT ;] PG:E-ROW EACH-REJECTS
   s" a decoder's rejection inside BY-ID" T-LABEL
   [: BAD-DECODE ;] PG:E-ROW EACH-REJECTS
   s" COL-BOOL of a binary column is PG's E-TYPE" T-LABEL
   [: BINARY-BOOL ;] PG:E-TYPE EACH-REJECTS ;


: REJECTION-NAMES ( str -- ) {: s:str :}
   PG:REJECTED$ s" 42P01: " STR:STARTS-WITH? TTRUE
   PG:REJECTED$ s STR:CONTAINS? TTRUE ;


: NO-REJECTION ( -- )
   PG:REJECTED$ STR:LEN 0 T= ;


: EXEC-ON ( str -- ) {: s:str :}
   CONN @ PG:PARAMS
   CONN @ s PG:EXEC ;


\ Ada's id is taken: the server's DETAIL names the key's value.
: DUPLICATE-ADA ( -- )
   s" insert into person (id, name, active) values (1, 'Ada', true)" EXEC-ON ;


: QUERY-ON ( str -- ) {: s:str :}
   CONN @ PG:PARAMS
   CONN @ s [: ;] PG:QUERY ;


\ Answers the row count and the last row's value, asserted once QUERY returns,
\ so a QUERY that never ran its body fails the case.
: GENERATED-ROWS ( PG:result -- n n PG:result )
   PG:ROWS COUNT>N {: rows:n :}
   2 0 PG:AT-INT rows swap rot ;


: BY-ID-MISSING ( -- )
   CONN @ 1 s" select name from by_id_missing_q where id = $1" [: ;] PG:BY-ID ;


: ONE-INT-MISSING ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select count(*) from one_int_missing_q" PG:ONE-INT drop ;


: STATEMENT-CASES ( -- )
   s" EXEC of a rejected statement" T-LABEL
   [: s" insert into exec_missing_q values (1)" EXEC-ON ;] PG:E-QUERY TTHROWSQ
   s" EXEC's rejection text" T-LABEL
   s" exec_missing_q" REJECTION-NAMES
   s" a rejection's text is the primary message, without the DETAIL" T-LABEL
   [: DUPLICATE-ADA ;] PG:E-QUERY TTHROWSQ
   PG:REJECTED$
   S\" 23505: duplicate key value violates unique constraint \qperson_pkey\q"
   T$=
   s" EXEC of DDL" T-LABEL
   [: s" create table gadget (id bigint)" EXEC-ON ;] 0 TTHROWSQ
   s" a successful EXEC empties the rejection text" T-LABEL
   NO-REJECTION
   s" EXEC of a statement answering rows" T-LABEL
   [: s" select 1" EXEC-ON ;] PG:E-QUERY TTHROWSQ
   NO-REJECTION
   s" QUERY of a rejected statement" T-LABEL
   [: s" select * from query_missing_q" QUERY-ON ;] PG:E-QUERY TTHROWSQ
   s" QUERY's rejection text" T-LABEL
   s" query_missing_q" REJECTION-NAMES
   s" QUERY's body sees the rows" T-LABEL
   CONN @ PG:PARAMS
   CONN @ s" select i * 10 from generate_series(1, 3) i" [: GENERATED-ROWS ;] PG:QUERY
   30 T= 3 T=
   s" a successful QUERY empties the rejection text" T-LABEL
   NO-REJECTION
   s" QUERY of a command answering no rows" T-LABEL
   [: s" update person set nick = nick where id = 99" QUERY-ON ;] PG:E-QUERY TTHROWSQ
   s" BY-ID's rejection text" T-LABEL
   [: BY-ID-MISSING ;] PG:E-QUERY TTHROWSQ
   s" by_id_missing_q" REJECTION-NAMES
   s" ONE-INT's rejection text" T-LABEL
   [: ONE-INT-MISSING ;] PG:E-QUERY TTHROWSQ
   s" one_int_missing_q" REJECTION-NAMES
   s" ROWS-OR-THROW's rejection text" T-LABEL
   [: REJECTED-QUERY ;] PG:E-QUERY TTHROWSQ
   s" nowhere" REJECTION-NAMES ;


TYPED-VARIABLE LOST-A PG:connection
TYPED-VARIABLE LOST-B PG:connection
10000 constant END-MS


: OPEN-AB ( -- )
   CONNINFO$ OPEN LOST-A !
   CONNINFO$ OPEN LOST-B ! ;

: CLOSE-AB ( -- )
   LOST-A @ PG:LIVE? if LOST-A @ PG:CLOSE then
   LOST-B @ PG:LIVE? if LOST-B @ PG:CLOSE then ;


: A-PID ( -- n )
   LOST-A @ PG:PARAMS
   LOST-A @ s" select pg_backend_pid()" PG:ONE-INT ;


: ENDED-ONE ( PG:result -- PG:result )
   0 0 PG:AT-INT 1 T= ;

\ The two-argument form answers once the backend has exited.
: END-A ( n -- ) {: pid:n :}
   LOST-B @ PG:PARAMS
   LOST-B @ pid PG:INT+
   LOST-B @ END-MS PG:INT+
   LOST-B @ s" select pg_terminate_backend($1, $2)::int" [: ENDED-ONE ;]
   PG:WITH-RESULT ;


: ON-A ( -- )
   LOST-A @ PG:PARAMS
   LOST-A @ s" select 1" PG:ONE-INT drop ;

: TRY-A ( -- n )
   [: ON-A ;] catch ;


: SELECT-ON ( PG:connection -- )
   dup PG:PARAMS
   s" select 1" [: ;] PG:WITH-RESULT ;

: LOSE-A ( -- n )
   [: LOST-A @ SELECT-ON ;] catch ;

: SELECT-B ( -- )
   LOST-B @ SELECT-ON ;


: REJECT-ON ( PG:connection -- )
   dup PG:PARAMS
   s" insert into lost_missing_q values (1)" PG:EXEC ;

: REJECT-A ( -- n )
   [: LOST-A @ REJECT-ON ;] catch ;

: REJECT-B ( -- n )
   [: LOST-B @ REJECT-ON ;] catch ;


: A-ENDED ( -- )
   PG:REJECTED$ s" terminating connection due to administrator command"
   STR:CONTAINS? TTRUE ;


: LOST-CASES ( -- )
   s" a backend ended from another connection" T-LABEL
   OPEN-AB
   A-PID END-A
   s" a statement on an ended backend" T-LABEL
   TRY-A PG:E-EXEC T=
   s" a lost connection keeps libpq's text" T-LABEL
   A-ENDED
   LOST-A @ PG:LIVE? TFALSE
   CLOSE-AB ;


: LOSS-AFTER-REJECTION ( -- )
   s" a loss after a rejection answers the loss" T-LABEL
   OPEN-AB
   A-PID END-A
   REJECT-B PG:E-QUERY T=
   LOSE-A PG:E-EXEC T=
   A-ENDED
   s" another operation's start empties a loss" T-LABEL
   [: SELECT-B ;] 0 TTHROWSQ
   NO-REJECTION
   CLOSE-AB ;


\ A's rejected statement frees its transaction's lock to B's waiting one, which
\ ends A's backend: B's AWAIT starts no operation, so the text stays.
: A-LOCKS ( -- )
   LOST-A @ PG:PARAMS
   LOST-A @ s" select pg_advisory_xact_lock(7)" [: ;] PG:WITH-RESULT ;

: SEND-END ( n -- ) {: pid:n :}
   LOST-B @ PG:PARAMS
   LOST-B @ pid PG:INT+
   LOST-B @ END-MS PG:INT+
   LOST-B @
   s" select pg_terminate_backend($1, $2)::int from pg_advisory_lock(7)"
   PG:SEND ;

: AWAITED ( PG:connection -- PG:result )
   PG:AWAIT MATCH PG:progress
      completed OF ENDOF
      connected OF drop PG:E-EXEC throw ENDOF
      waiting OF 2drop PG:E-EXEC throw ENDOF
      rejected OF drop PG:E-EXEC throw ENDOF
   ;MATCH ;

: AWAIT-END ( -- )
   LOST-B @ AWAITED ENDED-ONE PG:CLEAR ;

: AWAIT-A ( -- )
   LOST-A @ AWAITED PG:CLEAR ;

: SEND-A ( str -- ) {: s:str :}
   LOST-A @ PG:PARAMS
   LOST-A @ s PG:SEND ;

: REJECT-END ( PG:connection -- PG:connection )
   A-PID A-LOCKS SEND-END
   REJECT-A {: code:n :}
   AWAIT-END
   code 0<> if code throw then ;

: REJECT-IN-TX ( -- )
   LOST-A @ dup [: REJECT-END ;] PG:WITH-TRANSACTION drop ;

: ROLLBACK-LOST ( -- )
   s" a lost ROLLBACK keeps the body's rejection" T-LABEL
   OPEN-AB
   [: REJECT-IN-TX ;] PG:E-QUERY TTHROWSQ
   s" lost_missing_q" REJECTION-NAMES
   LOST-A @ PG:LIVE? TFALSE
   CLOSE-AB ;


\ The ROLLBACK on B under the body's E-EXEC is itself an operation's start.
: LOSE-END ( PG:connection -- PG:connection )
   A-PID END-A
   LOSE-A {: code:n :}
   code 0<> if code throw then ;

: LOSE-IN-TX ( -- )
   LOST-B @ dup [: LOSE-END ;] PG:WITH-TRANSACTION drop ;

: ROLLBACK-KEEPS ( -- )
   s" a ROLLBACK keeps the body's loss" T-LABEL
   OPEN-AB
   [: LOSE-IN-TX ;] PG:E-EXEC TTHROWSQ
   A-ENDED
   LOST-B @ PG:LIVE? TTRUE
   CLOSE-AB ;


: ON-A-EXEC ( str -- ) {: s:str :}
   LOST-A @ PG:PARAMS
   LOST-A @ s PG:EXEC ;

: DUPLICATE-ON-A ( -- )
   s" insert into detail_t values ('detail-key'), ('detail-key')" ON-A-EXEC ;

\ A's last statement is rejected with a DETAIL naming the key, then A's backend
\ ends while A is idle.
: DETAIL-THEN-END ( -- )
   OPEN-AB
   A-PID {: pid:n :}
   s" create temp table detail_t (k text primary key)" ON-A-EXEC
   [: DUPLICATE-ON-A ;] PG:E-QUERY TTHROWSQ
   pid END-A ;

: IDLE-LOSS-TEXT ( -- )
   LOST-A @ PG:LIVE? TFALSE
   PG:REJECTED$
   s" FATAL:  terminating connection due to administrator command"
   T$= ;

: ENDED-LOSS ( -- )
   s" ENDED? answers its own loss, with no earlier rejection" T-LABEL
   DETAIL-THEN-END
   LOST-A @ PG:ENDED? TTRUE
   IDLE-LOSS-TEXT
   CLOSE-AB ;

: A-NOTIFIED ( -- )
   LOST-A @ END-MS >MS PG:NOTIFIED? drop ;

: NOTIFIED-LOSS ( -- )
   s" NOTIFIED? answers its own loss, with no earlier rejection" T-LABEL
   DETAIL-THEN-END
   [: A-NOTIFIED ;] PG:E-EXEC TTHROWSQ
   IDLE-LOSS-TEXT
   CLOSE-AB ;

: A-NOTIFIED-NOW ( -- )
   LOST-A @ 0 >MS PG:NOTIFIED? drop ;

: NOTIFIED-NOW-LOSS ( -- )
   s" NOTIFIED? of no wait answers a loss that has arrived" T-LABEL
   DETAIL-THEN-END
   [: A-NOTIFIED-NOW ;] PG:E-EXEC TTHROWSQ
   IDLE-LOSS-TEXT
   CLOSE-AB ;


\ Returns once A's whole answer is sent, before anything else A's backend sends.
: A-ANSWERED ( n -- ) {: pid:n :}
   begin
      LOST-B @ PG:PARAMS
      LOST-B @ pid PG:INT+
      LOST-B @
      s" select count(*) from pg_stat_activity where pid = $1 and state = 'idle' and query = 'select 41'"
      PG:ONE-INT 0=
   while repeat ;

: ANSWERED-THEN-END ( -- )
   OPEN-AB
   A-PID {: pid:n :}
   s" select 41" SEND-A
   pid A-ANSWERED
   pid END-A ;

: ANSWERED-LOSS ( -- )
   s" a completed AWAIT keeps no FATAL that followed its answer" T-LABEL
   ANSWERED-THEN-END
   [: AWAIT-A ;] 0 TTHROWSQ
   NO-REJECTION
   CLOSE-AB
   s" a completed AWAIT adds no FATAL to an earlier rejection" T-LABEL
   ANSWERED-THEN-END
   REJECT-B PG:E-QUERY T=
   [: AWAIT-A ;] 0 TTHROWSQ
   PG:REJECTED$ S\" 42P01: relation \qlost_missing_q\q does not exist" T$=
   CLOSE-AB ;


: AWAIT-STOPPED ( -- )
   [: AWAIT-A ;] E-AIO-STATE TTHROWSQ ;

: STOPPED-WAIT ( -- )
   s" a wait the AIO loop rejects empties the rejection text" T-LABEL
   OPEN-AB
   s" select pg_sleep(10)" SEND-A
   REJECT-B PG:E-QUERY T=
   AIO:STOP
   [: AWAIT-STOPPED ;] [: AIO:START ;] finally
   NO-REJECTION
   LOST-A @ PG:LIVE? TFALSE
   CLOSE-AB ;


: A-UNSTAGED ( -- )
   LOST-A @ s" select 1" [: ;] PG:WITH-RESULT ;

: STALE-HANDLE ( -- )
   OPEN-AB
   LOST-A @ PG:CLOSE
   s" a statement on a closed connection empties the rejection text" T-LABEL
   REJECT-B PG:E-QUERY T=
   [: A-UNSTAGED ;] PG:E-HANDLE TTHROWSQ
   NO-REJECTION
   s" BY-ID on a closed connection empties the rejection text" T-LABEL
   REJECT-B PG:E-QUERY T=
   [: LOST-A @ 1 PERSON-SQL$ [: ;] PG:BY-ID ;] PG:E-HANDLE TTHROWSQ
   NO-REJECTION
   CLOSE-AB ;


: ROWS-AFTER ( PG:result -- PG:result )
   REJECT-B PG:E-QUERY T=
   PG:ROWS-OR-THROW ;

: FIRST-AFTER ( PG:result -- PG:result )
   REJECT-B PG:E-QUERY T=
   PG:FIRST-ROW ;

: ROWS-OF-OK ( -- )
   LOST-A @ PG:PARAMS
   LOST-A @ s" create temp table rows_ok_t (x int)" [: ROWS-AFTER ;]
   PG:WITH-RESULT ;

: FIRST-OF-NONE ( -- )
   LOST-A @ PG:PARAMS
   LOST-A @ s" select 1 where false" [: FIRST-AFTER ;] PG:WITH-RESULT ;

: TEXTLESS-FAILURES ( -- )
   OPEN-AB
   s" ROWS-OR-THROW of no rows empties a nested rejection's text" T-LABEL
   [: ROWS-OF-OK ;] PG:E-QUERY TTHROWSQ
   NO-REJECTION
   s" FIRST-ROW of no row empties a nested rejection's text" T-LABEL
   [: FIRST-OF-NONE ;] PG:E-ROW TTHROWSQ
   NO-REJECTION
   CLOSE-AB ;


\ Two equal keys, checked when COMMIT runs.
: DUPLICATE-KEYS ( -- )
   s" insert into dup_t values (1), (1)" RUN ;

: FAIL-QUIETLY ( -- )
   [: s" insert into tx_missing_q values (1)" EXEC-ON ;] PG:E-QUERY TTHROWSQ ;

: COMMIT-REJECTED ( -- )
   s" a rejected COMMIT answers its own text" T-LABEL
   s" create temp table dup_t (x int unique deferrable initially deferred)" RUN
   [: CONN @ REJECT-ON ;] PG:E-QUERY TTHROWSQ
   [: CONN @ [: DUPLICATE-KEYS ;] PG:WITH-TRANSACTION ;] PG:E-EXEC TTHROWSQ
   PG:REJECTED$
   S\" 23505: duplicate key value violates unique constraint \qdup_t_x_key\q"
   T$=
   s" a COMMIT of a failed transaction keeps the rejected statement's text" T-LABEL
   [: CONN @ [: FAIL-QUIETLY ;] PG:WITH-TRANSACTION ;]
   PG:E-EXEC TTHROWSQ
   s" tx_missing_q" REJECTION-NAMES ;


: REJECTED-THEN ( [ -- ] -- )
   REJECT-B PG:E-QUERY T=
   execute
   NO-REJECTION ;

: SUCCESS-EMPTIES ( -- )
   OPEN-AB
   s" a successful WITH-RESULT empties the rejection text" T-LABEL
   [: SELECT-B ;] REJECTED-THEN
   s" a false ENDED? empties the rejection text" T-LABEL
   [: LOST-B @ PG:ENDED? TFALSE ;] REJECTED-THEN
   s" a committed transaction empties the rejection text" T-LABEL
   [: LOST-B @ [: ;] PG:WITH-TRANSACTION ;] REJECTED-THEN
   CLOSE-AB ;


: DECLARATION-CASES ( -- )
   s" the same reader count again is harmless" T-LABEL
   [: READERS PG:CONFIGURE-READERS ;] 0 TTHROWSQ
   s" a different reader count" T-LABEL
   [: READERS 1 + PG:CONFIGURE-READERS ;] PG:E-CAPACITY TTHROWSQ ;


2 TYPED-BUFFER SIDE-CONN PG:connection
2 TYPED-BUFFER SIDE-ARRIVED n
2 TYPED-BUFFER SIDE-KEPT n
2 TYPED-BUFFER SIDE-CODE n
TYPED-VARIABLE THIRD-CONN PG:connection
variable SIDE-REJECTED
variable THIRD-CODE
variable THIRD-EXEC-CODE
variable THIRD-FAIL
variable ARRIVED
variable FINISHED
TASK:MIN-STACK TASK:TASK SIDE-A
TASK:MIN-STACK TASK:TASK SIDE-B
TASK:MIN-STACK TASK:TASK THIRD-TASK


: NAME-ROW ( PG:result -- str PG:result )
   1 PG:COL-TEXT swap ;


: NAME-OF ( PG:connection n -- str )
   PERSON-SQL$ [: NAME-ROW ;] PG:BY-ID ;


: SIDE-NAME$ ( n -- str )
   0= if s" Ada" exit then
   s" Grace" ;


: ARRIVE ( n -- ) {: k:n :}
   k SIDE-ARRIVED @ 0= if
      1 k SIDE-ARRIVED !
      1 ARRIVED atomic-add drop
   then ;


: SIDE-MISSING ( n -- ) {: k:n :}
   k SIDE-CONN @ PG:PARAMS
   k SIDE-CONN @ s" drop table side_missing_q" PG:EXEC ;


: SIDE ( n -- ) {: k:n :}
   CONNINFO$ OPEN k SIDE-CONN !
   k 0<> if begin ARRIVED atomic@ 1 < while TASK:PAUSE repeat then
   k SIDE-CONN @ k 1 + NAME-OF {: name:str :}
   k 0= if
      [: 0 SIDE-MISSING ;] catch PG:E-QUERY =
      PG:REJECTED$ s" side_missing_q" STR:CONTAINS? and
      if 1 else 0 then SIDE-REJECTED !
   then
   k ARRIVE
   begin ARRIVED atomic@ 2 < while TASK:PAUSE repeat
   name k SIDE-NAME$ STR:= if 1 else 0 then k SIDE-KEPT !
   k SIDE-CONN @ PG:CLOSE ;


\ A side that throws still arrives, so the other side never waits forever.
: SIDE-A-WORK ( -- )
   [: 0 SIDE ;] catch 0 SIDE-CODE !
   0 ARRIVE
   1 FINISHED atomic-add drop ;


: SIDE-B-WORK ( -- )
   [: 1 SIDE ;] catch 1 SIDE-CODE !
   1 ARRIVE
   1 FINISHED atomic-add drop ;


: THIRD-READ ( -- )
   THIRD-CONN @ 1 NAME-OF drop ;


: THIRD-DDL ( -- )
   THIRD-CONN @ PG:PARAMS
   THIRD-CONN @ s" create table third_ddl_q (id bigint)" PG:EXEC ;


: THIRD-SIDE ( -- )
   CONNINFO$ OPEN THIRD-CONN !
   [: THIRD-READ ;] catch THIRD-CODE !
   [: THIRD-DDL ;] catch THIRD-EXEC-CODE !
   THIRD-CONN @ PG:CLOSE ;


: THIRD-WORK ( -- )
   [: THIRD-SIDE ;] catch THIRD-FAIL !
   1 FINISHED atomic-add drop ;


: AWAIT-FINISHED ( n -- ) {: want:n :}
   begin FINISHED atomic@ want < while TASK:PAUSE repeat ;


: TASK-CASES ( -- )
   [: s" select * from main_missing_q" QUERY-ON ;] PG:E-QUERY TTHROWSQ
   0 SIDE-REJECTED !
   0 ARRIVED ! 0 FINISHED !
   2 0 ?do 0 i SIDE-ARRIVED ! 0 i SIDE-KEPT ! 0 i SIDE-CODE ! loop
   ['] SIDE-A-WORK SIDE-A TASK:ACTIVATE
   ['] SIDE-B-WORK SIDE-B TASK:ACTIVATE
   2 AWAIT-FINISHED
   SIDE-A TASK:KILL
   SIDE-B TASK:KILL
   0 SIDE-CODE @ 0 T=
   1 SIDE-CODE @ 0 T=
   s" the first reader kept its bytes" T-LABEL
   0 SIDE-KEPT @ 1 T=
   s" the second reader kept its bytes" T-LABEL
   1 SIDE-KEPT @ 1 T=
   s" a worker's rejection text is its own" T-LABEL
   SIDE-REJECTED @ 1 T=
   s" a worker's rejection leaves the main task's text" T-LABEL
   s" main_missing_q" REJECTION-NAMES
   \ Both workers keep their rows; a third task is one reader too many.
   ['] THIRD-WORK THIRD-TASK TASK:ACTIVATE
   3 AWAIT-FINISHED
   THIRD-TASK TASK:KILL
   THIRD-FAIL @ 0 T=
   s" E-READERS past the declared readers" T-LABEL
   THIRD-CODE @ PG:E-READERS T=
   s" EXEC past the declared readers sends nothing" T-LABEL
   THIRD-EXEC-CODE @ PG:E-READERS T=
   CONN @ PG:PARAMS
   CONN @ s" select count(*) from pg_tables where tablename = 'third_ddl_q'" PG:ONE-INT 0 T= ;


: SESSION ( -- )
   CONNINFO$ OPEN CONN !
   SEED
   OUTLIVES-CASES
   ADA-CASES
   GRACE-CASES
   PAGE-CASES
   LONG-CASES
   REJECTION-CASES
   STATEMENT-CASES
   LOST-CASES
   LOSS-AFTER-REJECTION
   ROLLBACK-LOST
   ROLLBACK-KEEPS
   ENDED-LOSS
   NOTIFIED-LOSS
   NOTIFIED-NOW-LOSS
   ANSWERED-LOSS
   STOPPED-WAIT
   STALE-HANDLE
   TEXTLESS-FAILURES
   COMMIT-REJECTED
   SUCCESS-EMPTIES
   DECLARATION-CASES
   TASK-CASES
   CONN @ PG:CLOSE ;


: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" pg-rows-test: no conninfo argument; test/db/pg-cluster.f runs this file"
      EXIT-STATUS:USAGE die
   then
   s" a read before the declaration" T-LABEL
   [: PG:READ-RESET ;] PG:E-READERS TTHROWSQ
   s" a declaration of no readers" T-LABEL
   [: 0 PG:CONFIGURE-READERS ;] PG:E-CAPACITY TTHROWSQ
   READERS PG:CONFIGURE-READERS
   AIO:START
   [: SESSION ;] [: AIO:STOP ;] finally ;

T-RESET
MAIN
T-REPORT

;package
