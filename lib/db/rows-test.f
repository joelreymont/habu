\ rows-test.f - package DB-ROWS against a live PostgreSQL server.
\
\ test/db/pg-cluster.f starts a private cluster and runs this file, after
\ lib/pg-test.f, with the cluster's conninfo as its one script argument; the
\ gate row is that harness:
\
\     bin/hb --load test/db/pg-cluster.f
\
\ Loaded without the argument, the file dies naming the harness. There is no
\ skip. The main task reads records whose spans must outlive PG:CLEAR, every
\ NULL arm and every refusal; two worker tasks then read at the same time and
\ each must keep its own bytes, and a third worker is one reader past the
\ declaration.

require lib/test.f
require lib/string.f
require lib/float.f
require lib/task.f
require lib/aio.f
require lib/image-lifecycle.f
require lib/pg.f
require lib/db/rows.f

package DB-ROWS-TEST

\ Three workers open a connection each, beside the main task's one.
3 DB-ROWS:CONNECTIONS+

3 constant READERS                \ the main task and two workers
\ More reads than PG's 64 live results, so a refusal that left its result
\ behind would end in PG:E-CAPACITY before the loop does.
70 constant OVER-RESULTS
$5000 constant PAST-ARENA         \ a text longer than one read's arena

TYPED-VARIABLE CONN PG:connection
TYPED-VARIABLE HELD PG:result


: CONNINFO$ ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: PERSON-SQL$ ( -- ptr u8 n )
   s" select id, name, nick, score, boss, active from person where id = $1" ;


: RUN ( ptr u8 n -- ) {: a:ptr u:n :}
   CONN @ PG:PARAMS
   CONN @ a u PG:EXEC PG:CLEAR ;


: SEED ( -- )
   s" create table person (id bigint primary key, name text not null, nick text, score float8, boss bigint, active boolean not null)" RUN
   s" insert into person values (1, 'Ada', 'ada', 0.5, null, true), (2, 'Grace', null, null, 1, false)" RUN ;


\ ---- a record outlives its result ----------------------------------------------

: NAMES-ROW ( PG:result -- ptr u8 n ptr u8 n bool ) {: r :}
   r 1 DB-ROWS:COL-TEXT
   r 2 DB-ROWS:COL-TEXT? ;


: NAMES-OF ( PG:connection n -- ptr u8 n ptr u8 n bool )
   PERSON-SQL$ DB-ROWS:BY-ID [: NAMES-ROW ;] DB-ROWS:WITH-ROW ;


\ WITH-ROW has cleared the result before the spans are read. A second result
\ of the same shape is held while they are compared, so a span still pointing
\ into the freed one would read that result's bytes.
: OUTLIVES-CASES ( -- )
   CONN @ 1 NAMES-OF {: na:ptr nu:n ka:ptr ku:n given:bool :}
   CONN @ PG:PARAMS
   CONN @ s" select 1::bigint, 'Zed', 'zed', 0.25::float8, null::bigint, false" PG:EXEC {: churn :}
   s" name after clear" T-LABEL
   na nu s" Ada" T$=
   s" nick after clear" T-LABEL
   ka ku s" ada" T$=
   given TTRUE
   churn PG:CLEAR ;


\ ---- the first row's readers and their NULL arms ------------------------------

: HOLD ( n -- ) {: id:n :}
   CONN @ id PERSON-SQL$ DB-ROWS:BY-ID HELD ! ;


: ADA-CASES ( -- )
   1 HOLD
   HELD @ 0 DB-ROWS:COL-INT 1 T=
   HELD @ 1 DB-ROWS:COL-TEXT s" Ada" T$=
   HELD @ 2 DB-ROWS:COL-TEXT? TTRUE s" ada" T$=
   HELD @ 3 DB-ROWS:COL-REAL 0.5 f= TTRUE
   s" a NULL reference reads as zero" T-LABEL
   HELD @ 4 DB-ROWS:COL-ID 0 T=
   HELD @ 5 DB-ROWS:COL-BOOL TTRUE
   HELD @ PG:CLEAR ;


: GRACE-CASES ( -- )
   2 HOLD
   s" a NULL nullable text is empty and false" T-LABEL
   HELD @ 2 DB-ROWS:COL-TEXT? TFALSE nip 0 T=
   s" a NULL real is zero" T-LABEL
   HELD @ 3 DB-ROWS:COL-REAL 0.0 f= TTRUE
   HELD @ 4 DB-ROWS:COL-ID 1 T=
   HELD @ 5 DB-ROWS:COL-BOOL TFALSE
   s" COL-TEXT refuses NULL" T-LABEL
   [: HELD @ 2 DB-ROWS:COL-TEXT 2drop ;] DB-ROWS:E-ROW TTHROWSQ
   s" COL-BOOL refuses NULL" T-LABEL
   [: HELD @ 2 DB-ROWS:COL-BOOL drop ;] DB-ROWS:E-ROW TTHROWSQ
   s" COL-INT of NULL is PG's refusal" T-LABEL
   [: HELD @ 3 DB-ROWS:COL-INT drop ;] PG:E-TYPE TTHROWSQ
   HELD @ PG:CLEAR ;


\ ---- rows read in place --------------------------------------------------------

: PAGE-CASES ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select i, i % 2 = 0, case when i = 2 then null else 'v' || i end from generate_series(1, 3) i" PG:EXEC
   DB-ROWS:ROWS-OR-THROW {: r :}
   r 2 0 DB-ROWS:AT-INT 3 T=
   r 0 1 DB-ROWS:AT-BOOL TFALSE
   r 1 1 DB-ROWS:AT-BOOL TTRUE
   r 0 2 DB-ROWS:AT$ s" v1" T$=
   r 2 2 DB-ROWS:AT?$ TTRUE s" v3" T$=
   s" a NULL cell in place is empty and false" T-LABEL
   r 1 2 DB-ROWS:AT?$ TFALSE nip 0 T=
   r PG:CLEAR
   CONN @ PG:PARAMS
   CONN @ s" select count(*) from person" PG:EXEC DB-ROWS:ONE-INT 2 T= ;


\ ---- refusals, each leaving no result behind ------------------------------------

: EACH-REFUSES ( [ -- ] n -- ) {: q want:n :}
   OVER-RESULTS 0 ?do q want TTHROWSQ loop ;


: NO-SUCH ( -- )
   CONN @ 99 NAMES-OF drop 2drop 2drop ;


: REFUSED-QUERY ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select nothing from nowhere" PG:EXEC DB-ROWS:ROWS-OR-THROW PG:CLEAR ;


: NO-ROWS-COMMAND ( -- )
   CONN @ PG:PARAMS
   CONN @ s" update person set nick = nick where id = 99" PG:EXEC DB-ROWS:ROWS-OR-THROW PG:CLEAR ;


: NULL-COUNT ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select null::int" PG:EXEC DB-ROWS:ONE-INT drop ;


: EMPTY-COUNT ( -- )
   CONN @ PG:PARAMS
   CONN @ s" select 1 where false" PG:EXEC DB-ROWS:ONE-INT drop ;


: LONG-ROW ( PG:result -- ptr u8 n )
   1 DB-ROWS:COL-TEXT ;


\ The decoder throws inside WITH-ROW, which still clears the result.
: PAST-ARENA-READ ( -- )
   CONN @ PAST-ARENA s" select $1::int, repeat('x', $1::int)" DB-ROWS:BY-ID
   [: LONG-ROW ;] DB-ROWS:WITH-ROW 2drop ;


: REFUSAL-CASES ( -- )
   s" E-ROW on no row" T-LABEL
   [: NO-SUCH ;] DB-ROWS:E-ROW EACH-REFUSES
   s" E-QUERY on a refused query" T-LABEL
   [: REFUSED-QUERY ;] DB-ROWS:E-QUERY EACH-REFUSES
   s" E-QUERY on a command with no rows" T-LABEL
   [: NO-ROWS-COMMAND ;] DB-ROWS:E-QUERY EACH-REFUSES
   s" ONE-INT refuses a NULL" T-LABEL
   [: NULL-COUNT ;] DB-ROWS:E-ROW EACH-REFUSES
   s" ONE-INT refuses no row" T-LABEL
   [: EMPTY-COUNT ;] DB-ROWS:E-ROW EACH-REFUSES
   s" a read past its arena" T-LABEL
   [: PAST-ARENA-READ ;] DB-ROWS:E-CAPACITY EACH-REFUSES ;


: REFUSED-OPEN ( -- )
   s" host=127.0.0.1 port=1 connect_timeout=2" DB-ROWS:OPEN PG:CLOSE ;


: DECLARATION-CASES ( -- )
   DB-ROWS:CONNECTIONS 4 T=
   s" CONNECTIONS+ after the first OPEN" T-LABEL
   [: 1 DB-ROWS:CONNECTIONS+ ;] DB-ROWS:E-CAPACITY TTHROWSQ
   s" the same reader count again is harmless" T-LABEL
   [: READERS DB-ROWS:CONFIGURE-READERS ;] 0 TTHROWSQ
   s" a different reader count" T-LABEL
   [: READERS 1 + DB-ROWS:CONFIGURE-READERS ;] DB-ROWS:E-CAPACITY TTHROWSQ
   s" a refused connection" T-LABEL
   [: REFUSED-OPEN ;] DB-ROWS:E-CONNECT TTHROWSQ ;


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
variable THIRD-CODE
variable THIRD-FAIL
variable ARRIVED
variable FINISHED
TASK:MIN-STACK TASK:TASK SIDE-A
TASK:MIN-STACK TASK:TASK SIDE-B
TASK:MIN-STACK TASK:TASK THIRD-TASK


: NAME-ROW ( PG:result -- ptr u8 n )
   1 DB-ROWS:COL-TEXT ;


: NAME-OF ( PG:connection n -- ptr u8 n )
   PERSON-SQL$ DB-ROWS:BY-ID [: NAME-ROW ;] DB-ROWS:WITH-ROW ;


: SIDE-NAME$ ( n -- ptr u8 n )
   0= if s" Ada" exit then
   s" Grace" ;


: ARRIVE ( n -- ) {: k:n :}
   k SIDE-ARRIVED @ 0= if
      1 k SIDE-ARRIVED !
      1 ARRIVED atomic-add drop
   then ;


: SIDE ( n -- ) {: k:n :}
   CONNINFO$ DB-ROWS:OPEN k SIDE-CONN !
   k 0<> if begin ARRIVED atomic@ 1 < while TASK:PAUSE repeat then
   k SIDE-CONN @ k 1 + NAME-OF {: a:ptr u:n :}
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


: THIRD-SIDE ( -- )
   CONNINFO$ DB-ROWS:OPEN THIRD-CONN !
   [: THIRD-READ ;] catch THIRD-CODE !
   THIRD-CONN @ PG:CLOSE ;


: THIRD-WORK ( -- )
   [: THIRD-SIDE ;] catch THIRD-FAIL !
   1 FINISHED atomic-add drop ;


: AWAIT-FINISHED ( n -- ) {: want:n :}
   begin FINISHED atomic@ want < while TASK:PAUSE repeat ;


: TASK-CASES ( -- )
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
   \ Both workers keep their rows; a third task is one reader too many.
   ['] THIRD-WORK THIRD-TASK TASK:ACTIVATE
   3 AWAIT-FINISHED
   THIRD-TASK TASK:KILL
   THIRD-FAIL @ 0 T=
   s" E-READERS past the declared readers" T-LABEL
   THIRD-CODE @ DB-ROWS:E-READERS T= ;


\ ---- image preparation ------------------------------------------------------------
\ The rows go with an image: a read after it is E-READERS until the new process
\ declares its readers, and it may declare a different count.
: IMAGE-CASES ( -- )
   IMAGE-LIFECYCLE:PREPARE
   s" a read after image preparation" T-LABEL
   [: DB-ROWS:READ-RESET ;] DB-ROWS:E-READERS TTHROWSQ
   s" a new declaration after image preparation" T-LABEL
   [: READERS 1 + DB-ROWS:CONFIGURE-READERS ;] 0 TTHROWSQ
   [: DB-ROWS:READ-RESET ;] 0 TTHROWSQ ;


: SESSION ( -- )
   CONNINFO$ DB-ROWS:OPEN CONN !
   SEED
   OUTLIVES-CASES
   ADA-CASES
   GRACE-CASES
   PAGE-CASES
   REFUSAL-CASES
   DECLARATION-CASES
   TASK-CASES
   CONN @ PG:CLOSE ;


: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" db-rows-test: no conninfo argument; test/db/pg-cluster.f runs this file" 2 die
   then
   s" a read before the declaration" T-LABEL
   [: DB-ROWS:READ-RESET ;] DB-ROWS:E-READERS TTHROWSQ
   s" a declaration of no readers" T-LABEL
   [: 0 DB-ROWS:CONFIGURE-READERS ;] DB-ROWS:E-CAPACITY TTHROWSQ
   READERS DB-ROWS:CONFIGURE-READERS
   AIO:START
   [: SESSION ;] [: AIO:STOP ;] finally
   IMAGE-CASES ;

T-RESET
MAIN
T-REPORT

;package
