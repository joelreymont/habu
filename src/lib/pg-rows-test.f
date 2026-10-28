\ pg-rows-test.f - package PG's row readers and statements against a live
\ PostgreSQL server.
\
\ test/db/pg-cluster.f starts a private cluster and runs this file, after
\ src/lib/pg-test.f, with the cluster's conninfo as its one script argument; the
\ gate row is that harness:
\
\     bin/hb --load test/db/pg-cluster.f
\
\ Loaded without the argument, the file dies naming the harness. There is no
\ skip. The main task reads records whose spans must outlive their result's
\ scope, every NULL arm and every refusal, and runs EXEC and QUERY with the
\ refusal text each leaves; two worker tasks then read at the same time and
\ each must keep its own bytes and refusal text, and a third worker is one
\ reader past the declaration, whose EXEC must send nothing.

require src/lib/test.f
require src/lib/string.f
require src/lib/float.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/image-lifecycle.f
require src/lib/pg.f
require src/lib/pg-rows.f

package PG-TEST

\ Three workers open a connection each, beside the main task's one.
3 PG:CONNECTIONS+

3 constant READERS                \ the main task and two workers
\ More reads than PG's 64 live results, so a refusal that left its result
\ behind would end in PG:E-CAPACITY before the loop does.
70 constant OVER-RESULTS
$5000 constant LONG-TEXT          \ a text longer than an arena's first chunk

TYPED-VARIABLE CONN PG:connection


: CONNINFO$ ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: PERSON-SQL$ ( -- ptr u8 n )
   s" select id, name, nick, score, boss, active from person where id = $1" ;


: RUN ( ptr u8 n -- ) {: a:ptr u:n :}
   CONN @ PG:PARAMS
   CONN @ a u [: ;] PG:WITH-RESULT ;


: SEED ( -- )
   s" create table person (id bigint primary key, name text not null, nick text, score float8, boss bigint, active boolean not null)" RUN
   s" insert into person values (1, 'Ada', 'ada', 0.5, null, true), (2, 'Grace', null, null, 1, false)" RUN ;


\ ---- a record outlives its result ----------------------------------------------

: NAMES-ROW ( PG:result -- ptr u8 n ptr u8 n bool PG:result )
   1 PG:COL-TEXT rot
   2 PG:COL-TEXT? {: ka:ptr ku:n given:bool :}
   ka ku rot given swap ;


: NAMES-OF ( PG:connection n -- ptr u8 n ptr u8 n bool )
   PERSON-SQL$ [: NAMES-ROW ;] PG:BY-ID ;


: CHURNED ( ptr u8 n ptr u8 n bool PG:result -- PG:result )
   >r {: na:ptr nu:n ka:ptr ku:n given:bool :}
   s" name after clear" T-LABEL
   na nu s" Ada" T$=
   s" nick after clear" T-LABEL
   ka ku s" ada" T$=
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


\ ---- the first row's readers and their NULL arms ------------------------------

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
   2 PG:COL-TEXT? TFALSE nip 0 T=
   s" a NULL real is zero" T-LABEL
   3 PG:COL-REAL 0.0 f= TTRUE
   4 PG:COL-ID 1 T=
   5 PG:COL-BOOL TFALSE ;


\ Each refusal throws out of its own scope, which clears the result.
: NULL-TEXT ( -- )
   2 [: 2 PG:COL-TEXT 2drop ;] ON-PERSON ;

: NULL-BOOL ( -- )
   2 [: 2 PG:COL-BOOL drop ;] ON-PERSON ;

: NULL-INT ( -- )
   2 [: 3 PG:COL-INT drop ;] ON-PERSON ;


: GRACE-CASES ( -- )
   2 [: GRACE-ROW ;] ON-PERSON
   s" COL-TEXT refuses NULL" T-LABEL
   [: NULL-TEXT ;] PG:E-ROW TTHROWSQ
   s" COL-BOOL refuses NULL" T-LABEL
   [: NULL-BOOL ;] PG:E-ROW TTHROWSQ
   s" COL-INT of NULL is PG's refusal" T-LABEL
   [: NULL-INT ;] PG:E-TYPE TTHROWSQ ;


\ ---- rows read in place --------------------------------------------------------

: PAGE-ROWS ( PG:result -- PG:result )
   PG:ROWS-OR-THROW
   2 0 PG:AT-INT 3 T=
   0 1 PG:AT-BOOL TFALSE
   1 1 PG:AT-BOOL TTRUE
   0 2 PG:AT$ s" v1" T$=
   2 2 PG:AT?$ TTRUE s" v3" T$=
   s" a NULL cell in place is empty and false" T-LABEL
   1 2 PG:AT?$ TFALSE nip 0 T= ;


: PAGE-CASES ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select i, i % 2 = 0, case when i = 2 then null else 'v' || i end from generate_series(1, 3) i"
   [: PAGE-ROWS ;] PG:WITH-RESULT
   CONN @ PG:PARAMS
   CONN @ s" select count(*) from person" PG:ONE-INT 2 T= ;


\ ---- reads longer than an arena's first chunk -----------------------------------

: LONG-SQL$ ( -- ptr u8 n )
   s" select $1::int, 'head', repeat('a', $1::int), repeat('b', 3 * $1::int)" ;


: LONG-ROW ( PG:result -- ptr u8 n ptr u8 n ptr u8 n PG:result )
   1 PG:COL-TEXT rot
   2 PG:COL-TEXT rot
   3 PG:COL-TEXT rot ;


\ The short text stays in the arena's first chunk while each long one chains
\ another. The same texts are held in a second result while the spans are
\ compared, as in OUTLIVES-CASES.
: LONG-CHURNED ( ptr u8 n ptr u8 n ptr u8 n PG:result -- PG:result )
   >r {: ha:ptr hu:n aa:ptr au:n ba:ptr bu:n :}
   s" a short text beside long ones" T-LABEL
   ha hu s" head" T$=
   s" a text longer than the first chunk reads whole" T-LABEL
   r> 0 2 PG:AT$ aa au T$=
   au LONG-TEXT T=
   s" a text of several chunks reads whole" T-LABEL
   0 3 PG:AT$ ba bu T$=
   bu LONG-TEXT 3 * T= ;


: LONG-READ ( -- )
   CONN @ LONG-TEXT LONG-SQL$ [: LONG-ROW ;] PG:BY-ID
   CONN @ PG:PARAMS
   CONN @ LONG-TEXT PG:INT+
   CONN @ LONG-SQL$ [: LONG-CHURNED ;] PG:WITH-RESULT ;


\ The second read gives up the first one's chunks and chains its own.
: LONG-CASES ( -- )
   LONG-READ
   LONG-READ ;


\ ---- refusals, each leaving no result behind ------------------------------------

: EACH-REFUSES ( [ -- ] n -- ) {: q want:n :}
   OVER-RESULTS 0 ?do q want TTHROWSQ loop ;


: NO-SUCH ( -- )
   CONN @ 99 NAMES-OF drop 2drop 2drop ;


: REFUSED-QUERY ( -- )
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


\ The decoder throws inside BY-ID's scope, which still clears the result.
: BAD-DECODE ( -- )
   CONN @ 1 PERSON-SQL$
   [: 1 PG:COL-BOOL swap ;] PG:BY-ID drop ;


: REFUSAL-CASES ( -- )
   s" E-ROW on no row" T-LABEL
   [: NO-SUCH ;] PG:E-ROW EACH-REFUSES
   s" E-QUERY on a refused query" T-LABEL
   [: REFUSED-QUERY ;] PG:E-QUERY EACH-REFUSES
   s" E-QUERY on a command with no rows" T-LABEL
   [: NO-ROWS-COMMAND ;] PG:E-QUERY EACH-REFUSES
   s" ONE-INT refuses a NULL" T-LABEL
   [: NULL-COUNT ;] PG:E-ROW EACH-REFUSES
   s" ONE-INT refuses no row" T-LABEL
   [: EMPTY-COUNT ;] PG:E-ROW EACH-REFUSES
   s" a decoder's refusal inside BY-ID" T-LABEL
   [: BAD-DECODE ;] PG:E-ROW EACH-REFUSES ;


\ ---- statements and the refusal text ---------------------------------------------

: REFUSAL-NAMES ( ptr u8 n -- ) {: a:ptr u:n :}
   PG:REFUSED$ s" 42P01: " STARTS-WITH? TTRUE
   PG:REFUSED$ a u CONTAINS? TTRUE ;


: NO-REFUSAL ( -- )
   PG:REFUSED$ nip 0 T= ;


: EXEC-ON ( ptr u8 n -- ) {: a:ptr u:n :}
   CONN @ PG:PARAMS
   CONN @ a u PG:EXEC ;


: QUERY-ON ( ptr u8 n -- ) {: a:ptr u:n :}
   CONN @ PG:PARAMS
   CONN @ a u [: ;] PG:QUERY ;


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
   s" EXEC of a refused statement" T-LABEL
   [: s" insert into exec_missing_q values (1)" EXEC-ON ;] PG:E-QUERY TTHROWSQ
   s" EXEC's refusal text" T-LABEL
   s" exec_missing_q" REFUSAL-NAMES
   s" EXEC of DDL" T-LABEL
   [: s" create table gadget (id bigint)" EXEC-ON ;] 0 TTHROWSQ
   s" a successful EXEC empties the refusal text" T-LABEL
   NO-REFUSAL
   s" EXEC of a statement answering rows" T-LABEL
   [: s" select 1" EXEC-ON ;] PG:E-QUERY TTHROWSQ
   NO-REFUSAL
   s" QUERY of a refused statement" T-LABEL
   [: s" select * from query_missing_q" QUERY-ON ;] PG:E-QUERY TTHROWSQ
   s" QUERY's refusal text" T-LABEL
   s" query_missing_q" REFUSAL-NAMES
   s" QUERY's body sees the rows" T-LABEL
   CONN @ PG:PARAMS
   CONN @ s" select i * 10 from generate_series(1, 3) i" [: GENERATED-ROWS ;] PG:QUERY
   30 T= 3 T=
   s" a successful QUERY empties the refusal text" T-LABEL
   NO-REFUSAL
   s" QUERY of a command answering no rows" T-LABEL
   [: s" update person set nick = nick where id = 99" QUERY-ON ;] PG:E-QUERY TTHROWSQ
   s" BY-ID's refusal text" T-LABEL
   [: BY-ID-MISSING ;] PG:E-QUERY TTHROWSQ
   s" by_id_missing_q" REFUSAL-NAMES
   s" ONE-INT's refusal text" T-LABEL
   [: ONE-INT-MISSING ;] PG:E-QUERY TTHROWSQ
   s" one_int_missing_q" REFUSAL-NAMES
   s" ROWS-OR-THROW's refusal text" T-LABEL
   [: REFUSED-QUERY ;] PG:E-QUERY TTHROWSQ
   s" nowhere" REFUSAL-NAMES
   s" READ-RESET empties the refusal text" T-LABEL
   PG:READ-RESET NO-REFUSAL ;


: REFUSED-OPEN ( -- )
   s" host=127.0.0.1 port=1 connect_timeout=2" PG:OPEN PG:CLOSE ;


: DECLARATION-CASES ( -- )
   PG:CONNECTIONS 4 T=
   s" CONNECTIONS+ after the first OPEN" T-LABEL
   [: 1 PG:CONNECTIONS+ ;] PG:E-CAPACITY TTHROWSQ
   s" the same reader count again is harmless" T-LABEL
   [: READERS PG:CONFIGURE-READERS ;] 0 TTHROWSQ
   s" a different reader count" T-LABEL
   [: READERS 1 + PG:CONFIGURE-READERS ;] PG:E-CAPACITY TTHROWSQ
   s" a refused connection" T-LABEL
   [: REFUSED-OPEN ;] PG:E-CONNECT TTHROWSQ ;


\ ---- two readers at once, and one too many -------------------------------------
\ Both sides are connected at once. The second reads only while the first holds
\ its record, and each looks at its bytes again only after both have read: with
\ one arena between them the second read would have copied its name over the
\ first's, which is the order Tender witnessed.

2 TYPED-BUFFER SIDE-CONN PG:connection
2 TYPED-BUFFER SIDE-ARRIVED n
2 TYPED-BUFFER SIDE-KEPT n
2 TYPED-BUFFER SIDE-CODE n
TYPED-VARIABLE THIRD-CONN PG:connection
variable SIDE-REFUSED
variable THIRD-CODE
variable THIRD-EXEC-CODE
variable THIRD-FAIL
variable ARRIVED
variable FINISHED
TASK:MIN-STACK TASK:TASK SIDE-A
TASK:MIN-STACK TASK:TASK SIDE-B
TASK:MIN-STACK TASK:TASK THIRD-TASK


: NAME-ROW ( PG:result -- ptr u8 n PG:result )
   1 PG:COL-TEXT rot ;


: NAME-OF ( PG:connection n -- ptr u8 n )
   PERSON-SQL$ [: NAME-ROW ;] PG:BY-ID ;


: SIDE-NAME$ ( n -- ptr u8 n )
   0= if s" Ada" exit then
   s" Grace" ;


: ARRIVE ( n -- ) {: k:n :}
   k SIDE-ARRIVED @ 0= if
      1 k SIDE-ARRIVED !
      1 ARRIVED atomic-add drop
   then ;


\ Side 0 meets a refusal of its own, which must leave the main task's text.
: SIDE-MISSING ( n -- ) {: k:n :}
   k SIDE-CONN @ PG:PARAMS
   k SIDE-CONN @ s" drop table side_missing_q" PG:EXEC ;


: SIDE ( n -- ) {: k:n :}
   CONNINFO$ PG:OPEN k SIDE-CONN !
   k 0<> if begin ARRIVED atomic@ 1 < while TASK:PAUSE repeat then
   k SIDE-CONN @ k 1 + NAME-OF {: a:ptr u:n :}
   k 0= if
      [: 0 SIDE-MISSING ;] catch PG:E-QUERY =
      PG:REFUSED$ s" side_missing_q" CONTAINS? and
      if 1 else 0 then SIDE-REFUSED !
   then
   k ARRIVE
   begin ARRIVED atomic@ 2 < while TASK:PAUSE repeat
   a u k SIDE-NAME$ STR= if 1 else 0 then k SIDE-KEPT !
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
   THIRD-CONN @ 1 NAME-OF 2drop ;


: THIRD-DDL ( -- )
   THIRD-CONN @ PG:PARAMS
   THIRD-CONN @ s" create table third_ddl_q (id bigint)" PG:EXEC ;


: THIRD-SIDE ( -- )
   CONNINFO$ PG:OPEN THIRD-CONN !
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
   0 SIDE-REFUSED !
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
   s" a worker's refusal text is its own" T-LABEL
   SIDE-REFUSED @ 1 T=
   s" a worker's refusal leaves the main task's text" T-LABEL
   s" main_missing_q" REFUSAL-NAMES
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


\ ---- image preparation ------------------------------------------------------------
\ The rows go with an image: a read after it is E-READERS until the new process
\ declares its readers, and it may declare a different count.
: IMAGE-CASES ( -- )
   IMAGE-LIFECYCLE:PREPARE
   s" a read after image preparation" T-LABEL
   [: PG:READ-RESET ;] PG:E-READERS TTHROWSQ
   s" a new declaration after image preparation" T-LABEL
   [: READERS 1 + PG:CONFIGURE-READERS ;] 0 TTHROWSQ
   [: PG:READ-RESET ;] 0 TTHROWSQ ;


: SESSION ( -- )
   CONNINFO$ PG:OPEN CONN !
   SEED
   OUTLIVES-CASES
   ADA-CASES
   GRACE-CASES
   PAGE-CASES
   LONG-CASES
   REFUSAL-CASES
   STATEMENT-CASES
   DECLARATION-CASES
   TASK-CASES
   CONN @ PG:CLOSE ;


: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" pg-rows-test: no conninfo argument; test/db/pg-cluster.f runs this file" 2 die
   then
   s" a read before the declaration" T-LABEL
   [: PG:READ-RESET ;] PG:E-READERS TTHROWSQ
   s" a declaration of no readers" T-LABEL
   [: 0 PG:CONFIGURE-READERS ;] PG:E-CAPACITY TTHROWSQ
   READERS PG:CONFIGURE-READERS
   AIO:START
   [: SESSION ;] [: AIO:STOP ;] finally
   IMAGE-CASES ;

T-RESET
MAIN
T-REPORT

;package
