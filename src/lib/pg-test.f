\ pg-test.f - package PG against a live PostgreSQL server.
\ test/db/pg-cluster.f starts a private cluster and runs this file with the
\ cluster's conninfo as its one script argument; the gate row is that harness
\ (`bin/hb --load test/db/pg-cluster.f`). Loaded without the argument, the file
\ dies naming the harness; there is no skip.

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

CONNECTIONS TYPED-BUFFER HELD-CONNS PG:connection
10000000000 constant PROGRESS-NS

0 constant TAG-OK
1 constant TAG-ROWS
2 constant TAG-FAILED


: CONNINFO$ ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;


: OPEN ( -- PG:connection )
   CONNECTIONS RESULTS CONFIGURE
   CONNINFO$ CONNECT MATCH PG:connect-result
      connected OF ENDOF
      refused OF type cr E-CONNECT throw ENDOF
   ;MATCH ;


: OUTCOME-TAG ( PG:result -- PG:result n )
   OUTCOME MATCH PG:outcome
      ok OF TAG-OK ENDOF
      rows OF TAG-ROWS ENDOF
      failed OF 2drop 2drop TAG-FAILED ENDOF
   ;MATCH ;


: FAILED-SQLSTATE$ ( PG:result -- PG:result ptr u8 n )
   OUTCOME MATCH PG:outcome
      ok OF s" <ok>" ENDOF
      rows OF s" <rows>" ENDOF
      failed OF 2drop ENDOF
   ;MATCH ;


: FAILED-MESSAGE$ ( PG:result -- PG:result ptr u8 n )
   OUTCOME MATCH PG:outcome
      ok OF s" " ENDOF
      rows OF s" " ENDOF
      failed OF 2swap 2drop ENDOF
   ;MATCH ;


: EXEC-TAG ( PG:connection ptr u8 n -- n )
   [: OUTCOME-TAG swap ;] WITH-RESULT ;


: EXEC-OK ( PG:connection ptr u8 n -- )
   EXEC-TAG TAG-OK T= ;


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
   CONNECTIONS 1+ 0 ?do [: CONNECT-NUL ;] E-FFI-NUL TTHROWSQ loop
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
   [: CONNECTIONS 1+ RESULTS CONFIGURE ;] E-CAPACITY TTHROWSQ
   0 HELD-CONNS @ {: stale :}
   stale PG:CLOSE
   stale PG:LIVE? TFALSE
   OPEN 0 HELD-CONNS !
   stale PG:LIVE? TFALSE
   0 HELD-CONNS @ PG:LIVE? TTRUE
   CONNECTIONS 0 ?do
      i HELD-CONNS @ {: c :}
      c s" select 42" [: 0 >ROW 0 >COL INT 42 T= ;] WITH-RESULT
      c PG:CLOSE
   loop ;


\ A cleanup that takes no arguments reaches its connection through an identity
\ held in a cell. Nothing else is open here, so the next OPEN reuses the
\ closed connection's slot under a new generation.
TYPED-VARIABLE HELD-ID n

: HELD-EXEC ( -- )
   HELD-ID @ PG:OF-ID s" select 1" [: ;] WITH-RESULT ;


: IDENTITY-CASES ( -- )
   s" a held identity names its connection until the slot is reused" T-LABEL
   OPEN {: c :}
   c PG:ID HELD-ID !
   HELD-ID @ PG:OF-ID {: held :}
   held s" select 1" EXEC-TAG TAG-ROWS T=
   held IDLE? TTRUE
   c PG:CLOSE
   HELD-ID @ PG:OF-ID PG:LIVE? TFALSE
   ['] HELD-EXEC E-HANDLE TTHROWSQ
   OPEN {: fresh :}
   HELD-ID @ PG:OF-ID PG:LIVE? TFALSE
   fresh PG:LIVE? TTRUE
   fresh s" select 1" EXEC-TAG TAG-ROWS T=
   fresh PG:CLOSE ;


\ AWAIT-READY, OPEN-POLL and RESULT-POLL run before the AIO loop starts: a single
\ task advances both connections, and a progress call that waits cannot let that
\ task release the lock. CHECK-PROGRESS also ends each turn of EXIT-CASES's wait,
\ while the loop and the exit worker are active. A wait that passes its
\ PROGRESS-NS deadline is no assertion result (docs/gate.md "How a suite runs,
\ and what that demands of its files"): CHECK-PROGRESS names it and throws
\ E-PROC-TIMEOUT, which the case engine exits with as PROC-TIMEOUT-RC (RUN).
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
   holder s" select pg_advisory_unlock(987654)::int" POLL-EXEC
   0 >ROW 0 >COL INT 1 T= CLEAR
   waiter RESULT-POLL OUTCOME-TAG TAG-ROWS T= CLEAR
   \ Closing with a query in flight releases its reserved result as well.
   holder s" select pg_advisory_lock(987654)" SEND
   holder PG:CLOSE
   waiter s" select pg_advisory_unlock(987654)::int" POLL-EXEC CLEAR
   waiter PG:CLOSE ;

: SEND-REFUSED ( PG:connection -- PG:connection ) {: c :}
   c [: BUSY-SEND ;] catch {: held code:n :}
   code E-CAPACITY T=
   c ;

\ Holds k `completed` tokens; with every slot held a send is refused, and each
\ CLEAR on the way back hands its slot back.
: HOLD-RESULTS ( PG:connection n -- PG:connection ) {: c k:n :}
   k 0= if c SEND-REFUSED exit then
   c s" select 7" POLL-EXEC c k 1- RECURSE swap CLEAR ;

: RESULT-CAPACITY-CASES ( -- )
   s" closed pending queries leave the declared result capacity available" T-LABEL
   OPEN-POLL RESULTS HOLD-RESULTS
   s" cleared completed tokens leave every slot for the next round" T-LABEL
   RESULTS HOLD-RESULTS
   PG:CLOSE ;


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


: DDL-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" set client_min_messages = warning" EXEC-OK
   c s" drop table if exists pqt" EXEC-OK
   c s" create table pqt (id int primary key, label text, note text)" EXEC-OK
   c ;


: INSERT-ROW ( PG:connection n ptr u8 n -- ) {: c id:n la:ptr lu:n :}
   c PARAMS
   c id INT+
   c la lu TEXT+
   c NULL+
   c s" insert into pqt (id, label, note) values ($1, $2, $3)"
   [: OUTCOME-TAG TAG-OK T= AFFECTED COUNT>N 1 T= ;] WITH-RESULT ;


: SELECTED ( PG:result -- PG:result )
   OUTCOME-TAG TAG-ROWS T=
   ROWS COUNT>N 1 T=
   COLS COUNT>N 3 T=
   0 >COL NAME$ s" id" T$=
   1 >COL NAME$ s" label" T$=
   0 >ROW 0 >COL INT 1 T=
   0 >ROW 1 >COL TEXT$ s" one" T$=
   0 >ROW 1 >COL NULL? TFALSE
   0 >ROW 2 >COL NULL? TTRUE ;


: SELECT-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c 1 INT+
   c s" select id, label, note from pqt where id = $1" [: SELECTED ;] WITH-RESULT
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

: TEXT-BOUNDARY-CASES ( PG:connection -- PG:connection )
   {: c :}
   s" text parameters reject NUL without losing staged parameters" T-LABEL
   c PARAMS
   c s" kept" TEXT+
   c [: NUL-LEADING ;] catch {: stale-lead lead-code:n :}
   lead-code E-FFI-NUL T=
   c [: NUL-EMBEDDED ;] catch {: stale-mid mid-code:n :}
   mid-code E-FFI-NUL T=
   c [: NUL-TRAILING ;] catch {: stale-tail tail-code:n :}
   tail-code E-FFI-NUL T=
   c s" select $1::text" [: 0 >ROW 0 >COL TEXT$ s" kept" T$= ;] WITH-RESULT
   s" empty and UTF-8 text parameters round trip exactly" T-LABEL
   c PARAMS
   c s" " TEXT+
   c S\" \xc3\xa9A" TEXT+
   c s" select $1::text, $2::text"
   [: 0 >ROW 0 >COL TEXT$ s" " T$= 0 >ROW 1 >COL TEXT$ S\" \xc3\xa9A" T$= ;] WITH-RESULT
   c ;


\ RAISE makes its argument the server's primary message, so the copy is checked
\ against the text the server itself builds: six KiB of two-byte characters,
\ then a longer message the connection's copy must grow past the first to hold.
: RAISED-SAME ( PG:connection ptr u8 n ptr u8 n -- PG:connection )
   {: c same sameu:n ma mu:n :}
   c PARAMS
   c ma mu TEXT+
   c same sameu
   [: OUTCOME-TAG TAG-ROWS T= 0 >ROW 0 >COL TEXT$ s" t" T$= ;] WITH-RESULT
   c ;


: RAISED-BODY ( PG:connection ptr u8 n n PG:result -- PG:connection PG:result )
   FAILED-SQLSTATE$ s" P0001" T$=
   FAILED-MESSAGE$ rot >r
   rot over swap T=
   RAISED-SAME r> ;


: RAISED-WHOLE ( PG:connection ptr u8 n ptr u8 n n -- PG:connection )
   {: c raise raiseu:n same sameu:n want:n :}
   c PARAMS
   c same sameu want c raise raiseu [: RAISED-BODY ;] WITH-RESULT ;


: LONG-MESSAGE-CASES ( PG:connection -- PG:connection )
   s" a server message past one KiB arrives whole and goes back as text" T-LABEL
   s" do $$ begin raise exception '%', 'x' || repeat(chr(233), 3000); end $$"
   s" select $1::text = ('x' || repeat(chr(233), 3000))"
   6001 RAISED-WHOLE
   s" a longer message after it grows the copy and arrives whole" T-LABEL
   s" do $$ begin raise exception '%', repeat('yz', 50000); end $$"
   s" select $1::text = repeat('yz', 50000)"
   100000 RAISED-WHOLE ;


\ A failed span is the result's own bytes, so a failing statement inside the
\ body on the same connection leaves it as it was. The two messages have one
\ length, so a copy the connection shared would show the inner bytes.
: INNER-FAILS ( PG:connection -- PG:connection )
   dup s" do $$ begin raise exception 'inner message'; end $$"
   [: FAILED-MESSAGE$ s" inner message" T$= ;] WITH-RESULT ;


: NESTED-SPAN-BODY ( PG:connection PG:result -- PG:connection PG:result )
   FAILED-MESSAGE$ rot >r
   rot INNER-FAILS -rot
   s" outer message" T$=
   r> ;


: NESTED-SPAN-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a failed span survives a nested failing statement on its connection" T-LABEL
   c c s" do $$ begin raise exception 'outer message'; end $$"
   [: NESTED-SPAN-BODY ;] WITH-RESULT ;


\ A deferred constraint fails the COMMIT, not the insert, with a message longer
\ than any the copy has held (this runs before LONG-MESSAGE-CASES). The
\ transaction verb's refusal is E-EXEC, not an OUTCOME, so a `failed` span held
\ from before stays mapped and unchanged.
: DEFERRED-INSERT ( PG:connection -- PG:connection )
   dup s" insert into held_t values (1)" EXEC-OK ;


: DEFERRED-RUN ( PG:connection -- PG:connection )
   dup [: DEFERRED-INSERT ;] WITH-TRANSACTION ;


: DEFERRED-FAILS ( PG:connection -- PG:connection ) {: c :}
   c [: DEFERRED-RUN ;] catch {: stale code:n :}
   code E-EXEC T=
   c ;


: HELD-SPAN-BODY ( PG:connection PG:result -- PG:connection PG:result )
   FAILED-MESSAGE$ rot >r
   rot DEFERRED-FAILS -rot
   s" short" T$=
   r> ;


: HELD-SPAN-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a failed COMMIT leaves a held failed span as it was" T-LABEL
   c s" create table held_t (x int)" EXEC-OK
   c s" create function held_f() returns trigger language plpgsql as $$ begin raise exception '%', repeat('z', 100000); end $$" EXEC-OK
   c s" create constraint trigger held_tg after insert on held_t deferrable initially deferred for each row execute function held_f()" EXEC-OK
   c c s" do $$ begin raise exception 'short'; end $$" [: HELD-SPAN-BODY ;] WITH-RESULT ;


: PREPARED-INSERT ( PG:connection n ptr u8 n -- ) {: c id:n la:ptr lu:n :}
   c PARAMS
   c id INT+
   c la lu TEXT+
   c s" pqt-note" TEXT+
   c s" pqt_ins" [: OUTCOME-TAG TAG-OK T= ;] WITH-PREPARED ;


: PREPARED-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" pqt_ins" s" insert into pqt (id, label, note) values ($1, $2, $3)"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-PREPARE
   c 10 s" ten" PREPARED-INSERT
   c 11 s" eleven" PREPARED-INSERT
   c PARAMS
   c s" select count(*) from pqt where note = 'pqt-note'"
   [: 0 >ROW 0 >COL INT 2 T= ;] WITH-RESULT
   c ;


\ Type OIDs from pg_type.dat; they are fixed for the built-in types.
2278 constant VOID-OID
1016 constant INT8-ARRAY-OID
25 constant TEXT-OID
\ Far inside the five-second sleep the described statement would run.
1000 constant PROMPT-MS


: ELAPSED-MS ( n -- n )
   mono-ns swap - PROC-NS-PER-MS / ;


: DESCRIBED$ ( -- ptr u8 n )
   s" select pg_sleep(5) as nap, array[1::int8] as ids, 'x'::text as label" ;


: DESCRIBED ( PG:result -- PG:result )
   OUTCOME-TAG TAG-OK T=
   ROWS COUNT>N 0 T=
   COLS COUNT>N 3 T=
   0 >COL NAME$ s" nap" T$=
   1 >COL NAME$ s" ids" T$=
   2 >COL NAME$ s" label" T$=
   0 >COL PG:OID VOID-OID T=
   1 >COL PG:OID INT8-ARRAY-OID T=
   2 >COL PG:OID TEXT-OID T= ;


: OID-PAST ( PG:connection -- PG:connection )
   dup DESCRIBED$ [: 3 >COL PG:OID drop ;] WITH-DESCRIPTION ;

: OID-BEFORE ( PG:connection -- PG:connection )
   dup DESCRIBED$ [: -1 >COL PG:OID drop ;] WITH-DESCRIPTION ;

: DESCRIBE-PENDING ( PG:connection -- PG:connection )
   dup s" select $1::int8" [: ;] WITH-DESCRIPTION ;


: DESCRIBE-CASES ( PG:connection -- PG:connection )
   {: c :}
   s" describe answers ordered names and type OIDs without executing" T-LABEL
   c PARAMS
   mono-ns {: started:n :}
   c DESCRIBED$ [: DESCRIBED ;] WITH-DESCRIPTION
   started ELAPSED-MS PROMPT-MS < TTRUE
   c [: OID-PAST ;] catch {: stale-past past-code:n :}
   past-code E-COLUMN T=
   c [: OID-BEFORE ;] catch {: stale-before before-code:n :}
   before-code E-COLUMN T=
   s" describe of invalid SQL is a failed outcome on a usable connection" T-LABEL
   c s" selec 1"
   [: OUTCOME-TAG TAG-FAILED T= FAILED-SQLSTATE$ s" 42601" T$= ;] WITH-DESCRIPTION
   c s" select 1" EXEC-TAG TAG-ROWS T=
   s" describe refuses a pending parameter list" T-LABEL
   c 1 INT+
   c [: DESCRIBE-PENDING ;] catch {: stale-pending pending-code:n :}
   pending-code E-STATEMENT T=
   c PARAMS
   c ;


: ORIGIN-SQL$ ( -- ptr u8 n )
   s" select id as key, label, length(label) as len from pqt" ;


: SCALAR ( PG:connection ptr u8 n -- n )
   [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;


: ATTNUM ( PG:connection ptr u8 n -- n ) {: c name:ptr nu:n :}
   c PARAMS
   c name nu TEXT+
   c s" select attnum from pg_attribute where attrelid = 'pqt'::regclass and attname = $1"
   SCALAR ;

: ORIGIN= ( PG:result n n n -- PG:result ) {: c:n tab:n num:n :}
   c >COL TABLE tab T=
   c >COL TABLE-COL num T= ;


: ORIGIN-CHECK ( n n n PG:result -- PG:result )
   -rot {: id-num:n label-num:n :}
   swap {: tab:n :}
   0 tab id-num ORIGIN=
   1 tab label-num ORIGIN=
   2 0 0 ORIGIN= ;


: ORIGIN-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" select 'pqt'::regclass::oid::int8" SCALAR {: tab:n :}
   c s" id" ATTNUM {: id-num:n :}
   c s" label" ATTNUM {: label-num:n :}
   s" a result column names its source table and attnum, 0 for an expression" T-LABEL
   tab id-num label-num c ORIGIN-SQL$ [: ORIGIN-CHECK ;] WITH-RESULT
   s" a described statement names the same column origins" T-LABEL
   tab id-num label-num c ORIGIN-SQL$ [: ORIGIN-CHECK ;] WITH-DESCRIPTION
   c ;


200 constant BOUND-MS
2000 constant ROOMY-MS

: SLEEP-PAST ( PG:connection -- PG:connection )
   dup s" select pg_sleep(5)" [: ;] WITH-RESULT ;

: NEGATIVE-BOUND ( PG:connection -- PG:connection )
   dup -1 >MS DEADLINE! ;

: HUGE-BOUND ( PG:connection -- PG:connection )
   dup $80000000 >MS DEADLINE! ;

: TX-SLEEP ( PG:connection -- PG:connection )
   dup [: SLEEP-PAST ;] WITH-TRANSACTION ;

\ Two rows of 200 kB, each 150 ms apart: every wait for the next bytes is
\ shorter than BOUND-MS, the whole statement longer.
: SPLIT-WAITS ( PG:connection -- PG:connection )
   dup s" select repeat($$x$$, 200000), pg_sleep(0.15) from generate_series(1,2)"
   [: ;] WITH-RESULT ;

: SLEEPING ( PG:connection n -- n )
   {: w pid:n :}
   w PARAMS
   w pid INT+
   w s" select count(*) from pg_stat_activity where pid = $1 and state = 'active' and query like '%pg_sleep(5)%'"
   [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;

: OPEN-BOUNDED ( -- PG:connection )
   OPEN {: c :}
   \ The client abandons the sleep without a cancel; the server's
   \ statement_timeout is what ends it.
   c s" set statement_timeout='1s'" EXEC-OK
   c BOUND-MS >MS DEADLINE!
   c ;

: EXPIRY ( PG:connection -- PG:connection ) {: c :}
   mono-ns {: started:n :}
   c [: SLEEP-PAST ;] catch {: stale code:n :}
   started ELAPSED-MS {: took:n :}
   code E-DEADLINE T=
   took BOUND-MS >= TTRUE
   took PROMPT-MS < TTRUE
   c PG:LIVE? TFALSE
   c ;

: STILL-SLEEPING ( n -- ) {: backend:n :}
   s" an expiry sends no cancel, so the server still runs the statement" T-LABEL
   OPEN {: w :}
   w backend SLEEPING 1 T=
   w PG:CLOSE ;

\ The expiry inside the body closes the connection and clears the body's
\ result with it, so the token's next read throws out of the body.
: KEPT-BODY ( PG:connection PG:result -- PG:connection PG:result )
   0 >ROW 0 >COL INT >r
   swap EXPIRY r> STILL-SLEEPING swap
   s" a result taken before the expiry is cleared with its connection" T-LABEL
   ROWS drop ;

: KEPT-RUN ( PG:connection -- PG:connection )
   dup s" select pg_backend_pid()" [: KEPT-BODY ;] WITH-RESULT ;


: DEADLINE-CASES ( -- )
   s" a wait past its deadline closes the connection and throws" T-LABEL
   OPEN-BOUNDED {: c :}
   c [: KEPT-RUN ;] catch {: stale-kept kept-code:n :}
   kept-code E-CLEARED T=
   s" one bound covers every wait of an operation" T-LABEL
   OPEN-BOUNDED {: whole :}
   whole [: SPLIT-WAITS ;] catch {: stale-whole whole-code:n :}
   whole-code E-DEADLINE T=
   s" an expiry inside a transaction body skips the rollback" T-LABEL
   OPEN-BOUNDED {: t :}
   t [: TX-SLEEP ;] catch {: stale-tx tx-code:n :}
   tx-code E-DEADLINE T=
   t PG:LIVE? TFALSE
   OPEN {: u :}
   u s" select 1" EXEC-TAG TAG-ROWS T=
   u IDLE? TTRUE
   u PG:CLOSE
   s" describe under a deadline does not run the statement" T-LABEL
   OPEN-BOUNDED {: v :}
   mono-ns {: described:n :}
   v s" select pg_sleep(5)" [: OUTCOME-TAG TAG-OK T= ;] WITH-DESCRIPTION
   described ELAPSED-MS BOUND-MS < TTRUE
   v PG:LIVE? TTRUE
   v PG:CLOSE
   s" a bound reads back as last set, 0 when none" T-LABEL
   OPEN {: d :}
   d DEADLINE@ MS>N 0 T=
   d ROOMY-MS >MS DEADLINE!
   d DEADLINE@ MS>N ROOMY-MS T=
   d 0 >MS DEADLINE!
   d DEADLINE@ MS>N 0 T=
   s" work inside its deadline, or with none, completes" T-LABEL
   d ROOMY-MS >MS DEADLINE!
   d s" select pg_sleep(0.3)" EXEC-TAG TAG-ROWS T=
   d BOUND-MS >MS DEADLINE!
   d 0 >MS DEADLINE!
   d s" select pg_sleep(0.3)" EXEC-TAG TAG-ROWS T=
   d PG:LIVE? TTRUE
   s" a negative deadline bound is a caller error that keeps the bound" T-LABEL
   d ROOMY-MS >MS DEADLINE!
   d [: NEGATIVE-BOUND ;] catch {: stale-bound bound-code:n :}
   bound-code E-BOUND T=
   d DEADLINE@ MS>N ROOMY-MS T=
   s" the widest bound is accepted and one past it is a caller error" T-LABEL
   d $7FFFFFFF >MS DEADLINE!
   d s" select 1" EXEC-TAG TAG-ROWS T=
   d [: HUGE-BOUND ;] catch {: stale-huge huge-code:n :}
   huge-code E-BOUND T=
   d PG:CLOSE ;


: TX-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqt (id, label, note) values (900, 'rolled', null)"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-RESULT
   BOOM throw ;


: TX-RUN ( PG:connection -- PG:connection )
   dup [: TX-BODY ;] WITH-TRANSACTION ;


: COUNT-ID ( PG:connection n -- n ) {: c id:n :}
   c PARAMS
   c id INT+
   c s" select count(*) from pqt where id = $1" [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;


: ROLLBACK-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: TX-RUN ;] catch {: stale code:n :}
   code BOOM T=
   c 900 COUNT-ID 0 T=
   c ;


\ The parameters are built BEFORE the transaction opens, which is the only way
\ into a body that may not read the caller's locals: BEGIN must leave the
\ pending list alone.
: COMMIT-BODY ( PG:connection -- PG:connection )
   dup IDLE? TFALSE
   dup s" insert into pqt (id, label, note) values ($1, $2, null)"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-RESULT ;


: COMMIT-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c 901 INT+
   c s" kept" TEXT+
   c [: COMMIT-BODY ;] WITH-TRANSACTION
   c IDLE? TTRUE
   c s" begin" EXEC-OK
   c IDLE? TFALSE
   c s" rollback" EXEC-OK
   c 901 COUNT-ID 1 T=
   c PARAMS
   c 901 INT+
   c s" select label from pqt where id = $1"
   [: 0 >ROW 0 >COL TEXT$ s" kept" T$= ;] WITH-RESULT
   c ;


\ A body that closes its connection and opens another, which takes the freed
\ slot, in a transaction of its own.
1 TYPED-BUFFER REOPENED PG:connection

: REOPEN-BODY ( PG:connection -- PG:connection )
   dup PG:CLOSE
   OPEN dup 0 REOPENED !
   s" begin" EXEC-OK ;


: REOPEN-RUN ( PG:connection -- PG:connection )
   dup [: REOPEN-BODY ;] WITH-TRANSACTION ;


\ The same body throwing: its connection's own transaction gets no ROLLBACK.
: REOPEN-THROW-BODY ( PG:connection -- PG:connection ) REOPEN-BODY BOOM throw ;


: REOPEN-THROW-RUN ( PG:connection -- PG:connection )
   dup [: REOPEN-THROW-BODY ;] WITH-TRANSACTION ;


\ A body that throws with a statement in flight, which the ROLLBACK refuses.
: PENDING-BODY ( PG:connection -- PG:connection )
   dup s" select pg_sleep(5)" SEND
   BOOM throw ;


: PENDING-RUN ( PG:connection -- PG:connection )
   dup [: PENDING-BODY ;] WITH-TRANSACTION ;


: LEFT-BODY-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a body that closed its connection commits nothing on the slot's next one" T-LABEL
   OPEN {: t :}
   t [: REOPEN-RUN ;] catch {: stale-reopen reopen-code:n :}
   reopen-code E-HANDLE T=
   0 REOPENED @ {: u :}
   u IDLE? TFALSE
   u s" rollback" EXEC-OK
   u PG:CLOSE
   s" a body that closed its connection and threw rolls back nothing on the slot's next one" T-LABEL
   OPEN {: w :}
   w [: REOPEN-THROW-RUN ;] catch {: stale-throw throw-code:n :}
   throw-code BOOM T=
   0 REOPENED @ {: v :}
   v IDLE? TFALSE
   v s" rollback" EXEC-OK
   v PG:CLOSE
   s" a failed rollback closes the connection and the body's throw stands" T-LABEL
   OPEN {: p :}
   p [: PENDING-RUN ;] catch {: stale-pending pending-code:n :}
   pending-code BOOM T=
   p PG:LIVE? TFALSE
   c ;


: UNIQUE-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c s" insert into pqt (id, label, note) values (1, 'duplicate', null)"
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" 23505" T$=
      FAILED-MESSAGE$ nip 0 > TTRUE ;] WITH-RESULT
   c ;


: READ-BAD-COLUMN ( PG:connection -- PG:connection )
   dup s" select id from pqt where id = 1"
   [: OUTCOME-TAG TAG-ROWS T= 0 >ROW 99 >COL TEXT$ 2drop ;] WITH-RESULT ;


: READ-BAD-ROW ( PG:connection -- PG:connection )
   dup s" select id from pqt where id = 1" [: 99 >ROW 0 >COL TEXT$ 2drop ;] WITH-RESULT ;


: BAD-COLUMN-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c [: READ-BAD-COLUMN ;] catch {: stale-col code:n :}
   code E-COLUMN T=
   c [: READ-BAD-ROW ;] catch {: stale-row row-code:n :}
   row-code E-COLUMN T=
   c ;


: EXEC-EMPTY ( PG:connection -- PG:connection )
   dup s" " [: ;] WITH-RESULT ;


: PREPARE-UNNAMED ( PG:connection -- PG:connection )
   dup s" " s" select 1" [: ;] WITH-PREPARE ;


: EXEC-PREPARED-UNNAMED ( PG:connection -- PG:connection )
   dup s" " [: ;] WITH-PREPARED ;


: EXEC-NUL ( PG:connection -- PG:connection )
   dup S\" select $1::text\x00; select 2" [: ;] WITH-RESULT ;

: PREPARE-NUL-NAME ( PG:connection -- PG:connection )
   dup S\" p\x00q" s" select 1" [: ;] WITH-PREPARE ;

: EXEC-NUL-NAME ( PG:connection -- PG:connection )
   dup S\" p\x00q" [: ;] WITH-PREPARED ;


: STATEMENT-BOUNDARY-CASES ( PG:connection -- PG:connection )
   {: c :}
   s" statements and prepared names reject NUL before libpq" T-LABEL
   c PARAMS
   c s" kept" TEXT+
   c [: EXEC-NUL ;] catch {: stale-sql sql-code:n :}
   sql-code E-FFI-NUL T=
   c [: PREPARE-NUL-NAME ;] catch {: stale-prepare prepare-code:n :}
   prepare-code E-FFI-NUL T=
   c [: EXEC-NUL-NAME ;] catch {: stale-name name-code:n :}
   name-code E-FFI-NUL T=
   c s" select $1::text" [: 0 >ROW 0 >COL TEXT$ s" kept" T$= ;] WITH-RESULT
   c ;


\ An empty statement would reach the server as PGRES_EMPTY_QUERY, whose result
\ carries no SQLSTATE and no message at all - measured - so the module refuses
\ it by name instead.
: EMPTY-STATEMENT-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: EXEC-EMPTY ;] catch {: stale-exec code:n :}
   code E-STATEMENT T=
   c [: PREPARE-UNNAMED ;] catch {: stale-prepare prepare-code:n :}
   prepare-code E-STATEMENT T=
   c [: EXEC-PREPARED-UNNAMED ;] catch {: stale-name name-code:n :}
   name-code E-STATEMENT T=
   c ;


\ Each repeated case runs one more scope than the registry has result slots, so
\ a slot a scope failed to retire ends the run in E-CAPACITY.
: THROW-BODY ( PG:connection -- PG:connection )
   dup s" select 1" [: BOOM throw ;] WITH-RESULT ;


: INNER-THROW ( PG:connection -- PG:connection )
   dup s" select 2" [: BOOM throw ;] WITH-RESULT ;


: INNER-CAUGHT ( PG:connection -- PG:connection ) {: c :}
   c [: INNER-THROW ;] catch {: stale code:n :}
   code BOOM T=
   c ;


\ The outer token still reads after one inner scope returned and another threw.
: NESTED-BODY ( PG:connection PG:result -- PG:connection PG:result )
   swap
   dup s" select 2" [: 0 >ROW 0 >COL INT 2 T= ;] WITH-RESULT
   INNER-CAUGHT
   swap
   0 >ROW 0 >COL INT 1 T= ;


: NESTED-RUN ( PG:connection -- PG:connection )
   dup s" select 1" [: NESTED-BODY ;] WITH-RESULT ;


\ The inner body hands back the outer scope's token, which the inner scope refuses.
: HAND-OUTER ( PG:connection PG:result -- PG:connection PG:result )
   swap dup s" select 2" [: rot ;] WITH-RESULT ;


: OUTER-HANDED ( PG:connection -- PG:connection )
   dup s" select 1" [: HAND-OUTER ;] WITH-RESULT ;


\ CLEAR refuses the body's own token, before the body could hand back another.
variable CLEAR-RETURNED

: SWAP-TOKEN ( PG:connection PG:result -- PG:connection PG:result )
   swap dup s" select 3" POLL-EXEC
   rot CLEAR
   1 CLEAR-RETURNED ! ;


: CLEAR-SCOPED ( PG:connection -- PG:connection )
   dup s" select 1" [: SWAP-TOKEN ;] WITH-RESULT ;


: SCOPE-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a body that throws leaves its result slot free" T-LABEL
   RESULTS 1+ 0 ?do
      c [: THROW-BODY ;] catch {: stale code:n :}
      code BOOM T=
   loop
   s" nested scopes each retire their own result, an inner throw included" T-LABEL
   RESULTS 1+ 0 ?do c NESTED-RUN drop loop
   s" an inner body handing back the outer token is E-HANDLE and both retire" T-LABEL
   RESULTS 1+ 0 ?do
      c [: OUTER-HANDED ;] catch {: stale code:n :}
      code E-HANDLE T=
   loop
   c s" select 5" [: 0 >ROW 0 >COL INT 5 T= ;] WITH-RESULT
   s" CLEAR of a scope's token is E-HANDLE" T-LABEL
   0 CLEAR-RETURNED !
   OPEN {: own :}
   own [: CLEAR-SCOPED ;] catch {: stale-own own-code:n :}
   own-code E-HANDLE T=
   CLEAR-RETURNED @ 0 T=
   own PG:CLOSE
   c ;


\ The call arena is sized to the call: a parameter or statement past any fixed
\ buffer round-trips. Only connections, results and the parameter count are bounded.
$32000 constant BIG-LEN                   \ 204800 bytes of parameter text
create BIG-BUF BIG-LEN allot

$80000 constant STMT-CAP                  \ holds the BIND-MAX-parameter statement
create STMT-BUF STMT-CAP allot
variable STMT-U

1500 constant BIG-ROWS
8192 constant OLD-STATEMENT-CAP           \ a statement past 8 KiB still runs
65535 constant BIND-MAX                   \ a Bind message counts its parameters in 16 bits
2147385345 constant BIND-SUM              \ 0 + 1 + ... + BIND-MAX - 1


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
   c s" select body, length(body) from pqtbig where id = $1"
   [: OUTCOME-TAG TAG-ROWS T=
      0 >ROW 1 >COL INT BIG-LEN T=
      0 >ROW 0 >COL TEXT$ BIG$ T$= ;] WITH-RESULT
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
   c STMT$ [: OUTCOME-TAG TAG-OK T= AFFECTED COUNT>N BIG-ROWS T= ;] WITH-RESULT
   c ;


: WIDE-STATEMENT ( -- )
   STMT-RESET
   s" select (select sum(v::int) from unnest(a) v) from (select array[" STMT+
   BIND-MAX 0 ?do
      i 0 > if s" ," STMT+ then
      s" $" STMT+ i 1 + STMT-N+
   loop
   s" ] as a) w" STMT+ ;


: BIND-WIDE ( PG:connection -- PG:connection )
   dup PARAMS
   BIND-MAX 0 ?do dup i INT+ loop ;


: BIND-PAST-MAX ( PG:connection -- PG:connection )
   dup 0 INT+ ;


\ The sum reads every value back, so an offset lost as the list grows shows.
: PARAM-COUNT-CASES ( PG:connection -- PG:connection ) {: c :}
   WIDE-STATEMENT
   c BIND-WIDE drop
   s" a 65536th parameter is refused and leaves the 65535 to run" T-LABEL
   c [: BIND-PAST-MAX ;] catch {: stale-param code:n :}
   code E-CAPACITY T=
   c STMT$
   [: 0 >ROW 0 >COL INT BIND-SUM T= ;] WITH-RESULT
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


\ WITH-SCRIPT is the only path that takes more than one statement; WITH-RESULT
\ and WITH-PREPARED ride the extended protocol, where a second command is 42601.
\ MEASURED: PostgreSQL runs a multi-statement simple query as ONE implicit
\ transaction, so a failing statement rolls the earlier ones back with it - the
\ script is atomic even outside WITH-TRANSACTION.
: MULTI-VIA-EXEC-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c s" select 1; select 2"
   [: OUTCOME-TAG TAG-FAILED T= FAILED-SQLSTATE$ s" 42601" T$= ;] WITH-RESULT
   c ;


: SCRIPT-OK-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" drop table if exists pqtscript; create table pqtscript (id int); insert into pqtscript values (7)"
   [: OUTCOME-TAG TAG-OK T= AFFECTED COUNT>N 1 T= ;] WITH-SCRIPT
   c PARAMS
   c s" select count(*) from pqtscript" [: 0 >ROW 0 >COL INT 1 T= ;] WITH-RESULT
   c ;


: SCRIPT-FAIL-CASES ( PG:connection -- PG:connection ) {: c :}
   c s" insert into pqtscript values (8); insert into pqtnosuch values (1)"
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" 42P01" T$=
      FAILED-MESSAGE$ nip 0 > TTRUE ;] WITH-SCRIPT
   c PARAMS
   c s" select count(*) from pqtscript where id = 8" [: 0 >ROW 0 >COL INT 0 T= ;] WITH-RESULT
   c ;


: SCRIPT-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqtscript values (9); insert into pqtscript values (10)"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-SCRIPT
   BOOM throw ;


: SCRIPT-TX-RUN ( PG:connection -- PG:connection )
   dup [: SCRIPT-BODY ;] WITH-TRANSACTION ;


: SCRIPT-TX-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: SCRIPT-TX-RUN ;] catch {: stale-tx code:n :}
   code BOOM T=
   c PARAMS
   c s" select count(*) from pqtscript where id in (9, 10)"
   [: 0 >ROW 0 >COL INT 0 T= ;] WITH-RESULT
   c ;


: SCRIPT-EMPTY ( PG:connection -- PG:connection )
   dup s" " [: ;] WITH-SCRIPT ;


: SCRIPT-WITH-PARAMS ( PG:connection -- PG:connection )
   dup PARAMS
   dup 1 INT+
   dup s" select 1" [: ;] WITH-SCRIPT ;


: SCRIPT-REFUSAL-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: SCRIPT-EMPTY ;] catch {: stale-empty code:n :}
   code E-STATEMENT T=
   c [: SCRIPT-WITH-PARAMS ;] catch {: stale-params param-code:n :}
   param-code E-STATEMENT T=
   c PARAMS
   c ;


\ More statements than the registry has result slots: every scope must hand its
\ slot back or the run ends in PG:E-CAPACITY rather than an assertion.
: RECYCLE-CASES ( PG:connection -- PG:connection ) {: c :}
   64 0 ?do
      c PARAMS
      c s" select 1" [: ;] WITH-RESULT
   loop
   c PARAMS
   c s" select count(*) from pqt" [: OUTCOME-TAG TAG-ROWS T= ;] WITH-RESULT
   c ;


\ CLOSE owns whatever results the connection still holds, so libpq keeps no
\ orphan PGresult and the body's token refuses afterwards. The scope still
\ retires the slot, so the case runs one more time than there are slots.
: CLOSE-UNDER ( PG:connection -- PG:connection )
   dup dup s" select 1" [: swap PG:CLOSE ROWS drop ;] WITH-RESULT ;


: CLOSE-CLEARS-CASES ( -- )
   s" a close in the body clears its token and leaves the slot to the scope" T-LABEL
   RESULTS 1+ 0 ?do
      OPEN {: c :}
      c PARAMS
      c [: CLOSE-UNDER ;] catch {: stale code:n :}
      code E-CLEARED T=
      c PG:LIVE? TFALSE
   loop ;


\ A backend terminating its own session may fail the terminating query or the
\ next query. Either path leaves no connection for owner cleanup to close.
: END-SESSION ( PG:connection -- PG:connection )
   dup s" select pg_terminate_backend(pg_backend_pid())" [: ;] WITH-RESULT ;

: PROBE-SESSION ( PG:connection -- PG:connection )
   dup s" select 1" [: ;] WITH-RESULT ;

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
   0 EXIT-CONN @ s" select pg_backend_pid()"
   [: 0 >ROW 0 >COL INT EXIT-BACKEND atomic! ;] WITH-RESULT
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
   c sql size [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;

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
   s" select pg_backend_pid()" [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;

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
   holder s" select * from pqtcancel for update" [: ;] WITH-RESULT
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
      completed OF OUTCOME-TAG TAG-OK T= CLEAR ENDOF
      connected OF PG:CLOSE E-EXEC throw ENDOF
      waiting OF 2drop E-EXEC throw ENDOF
      refused OF 2drop E-EXEC throw ENDOF
   ;MATCH
   c s" select value from pqtcancel order by id"
   [: 0 >ROW 0 >COL INT 0 T= 1 >ROW 0 >COL INT 1 T= ;] WITH-RESULT
   holder s" begin" EXEC-OK
   holder s" select * from pqtcancel for update" [: ;] WITH-RESULT
   s" a statement timeout returns its SQL error and leaves the connection usable" T-LABEL
   peer s" set statement_timeout='20ms'" EXEC-OK
   peer s" update pqtcancel set value=2 where id=1"
   [: OUTCOME-TAG TAG-FAILED T= FAILED-SQLSTATE$ s" 57014" T$= ;] WITH-RESULT
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
   holder s" select * from pqtcancel for update" [: ;] WITH-RESULT
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
   c s" select value from pqtcancel where id=1" [: 0 >ROW 0 >COL INT 0 T= ;] WITH-RESULT
   c ;


\ A restored image runs in another process where the libpq pointers are gone,
\ so PREPARE closes native resources and retires every handle. A new registry
\ cannot revive a stale handle even when it reuses the same slot number.
: USE-CONN ( PG:connection -- PG:connection )
   dup PARAMS ;


: STALE-CONN ( PG:connection -- PG:connection ) {: c :}
   c [: USE-CONN ;] catch {: stale code:n :}
   code E-HANDLE T=
   c ;


\ A capture runs after the program's tasks stop: PREPARE ends the process on a
\ task still activated (src/lib/task.f), and the AIO loop is one. CAPTURE stops
\ it, prepares, and starts it again however PREPARE ends, so the loop is live
\ for the connections opened afterwards and for MAIN's cleanup.
: CAPTURE ( -- )
   AIO:STOP
   [: IMAGE-LIFECYCLE:PREPARE ;] [: AIO:START ;] finally ;


\ Preparation retires the body's result under it, so the token's next read
\ throws out of the body.
: CAPTURE-UNDER ( PG:connection -- PG:connection )
   dup s" select 1" [: CAPTURE ROWS drop ;] WITH-RESULT ;


\ CLOSE frees an unscoped token's slot at once, so the next query takes it and
\ only the generation tells the stale token from the live result there. A CLEAR
\ that returned would have cleared that live result.
: CLEAR-REUSED ( PG:connection PG:connection -- PG:connection PG:connection )
   {: gone other :}
   gone s" select 1" POLL-EXEC
   gone PG:CLOSE
   other s" select 2" POLL-EXEC
   swap CLEAR
   1 CLEAR-RETURNED !
   CLEAR
   gone other ;


: IMAGE-CASES ( -- )
   OPEN {: c :}
   c s" drop table if exists pqtimage" EXEC-OK
   c s" create table pqtimage (value int)" EXEC-OK
   c s" insert into pqtimage values (0)" EXEC-OK
   c s" begin" EXEC-OK
   c s" select * from pqtimage for update" [: ;] WITH-RESULT
   OPEN {: waiter :}
   waiter BACKEND {: waiter-pid:n :}
   waiter s" update pqtimage set value=1" SEND
   OPEN {: observer :}
   observer waiter-pid AWAIT-LOCK
   c PARAMS
   c [: CAPTURE-UNDER ;] catch {: stale-res res-code:n :}
   res-code E-CLEARED T=
   c PG:LIVE? TFALSE
   c STALE-CONN drop
   OPEN {: fresh :}
   fresh PG:LIVE? TTRUE
   fresh s" select value from pqtimage" [: 0 >ROW 0 >COL INT 0 T= ;] WITH-RESULT
   c PG:LIVE? TFALSE
   c fresh s" select 2" [: swap STALE-CONN swap ;] WITH-RESULT drop
   s" a stale token is E-CLEARED after a later query reuses its slot" T-LABEL
   0 CLEAR-RETURNED !
   OPEN fresh [: CLEAR-REUSED ;] catch {: gone kept code:n :}
   code E-CLEARED T=
   CLEAR-RETURNED @ 0 T=
   fresh PG:CLOSE ;


\ One connection LISTENs, another NOTIFYs. NOTIFY-MS is the wait a notification
\ must cut short; PROMPT-MS bounds how long that takes.
10000 constant NOTIFY-MS

: NOTIFY-NOW ( PG:connection -- PG:connection )
   dup 0 >MS NOTIFIED? drop ;

: NOTIFY-LONG ( PG:connection -- PG:connection )
   dup NOTIFY-MS >MS NOTIFIED? drop ;

: NOTIFY-NEGATIVE ( PG:connection -- PG:connection )
   dup -1 >MS NOTIFIED? drop ;

: NOTIFY-IN-TX ( PG:connection -- PG:connection )
   dup [: NOTIFY-NOW ;] WITH-TRANSACTION ;

: REFUSES ( PG:connection [ PG:connection -- PG:connection ] n -- )
   {: c body want:n :}
   c body catch {: stale code:n :}
   code want T= ;

: TERMINATE ( PG:connection n -- )
   {: c pid:n :}
   c PARAMS c pid INT+
   c s" select pg_terminate_backend($1)" [: OUTCOME-TAG TAG-ROWS T= ;] WITH-RESULT ;


\ The worker waits on its own connection, which its cleanup closes. It answers
\ only after the wait, so a halt inside the wait leaves it no result.
1 TYPED-BUFFER NOTE-CONN PG:connection
variable NOTE-STARTED
TASK:MIN-STACK TASK:TASK NOTE-WORKER

: NOTE-CLEAN ( -- )
   0 NOTE-CONN @ PG:LIVE? if 0 NOTE-CONN @ PG:CLOSE then ;

: NOTE-WAIT ( -- )
   OPEN dup 0 NOTE-CONN !
   dup s" listen pqtnote" EXEC-OK
   1 NOTE-STARTED atomic!
   NOTIFY-MS >MS NOTIFIED? drop
   0 TASK:RETURN ;


: NOTE-HALT-CASES ( -- )
   s" a halt ends a task waiting for a notification" T-LABEL
   0 NOTE-STARTED !
   ['] NOTE-CLEAN NOTE-WORKER TASK:AT-EXIT
   ['] NOTE-WAIT NOTE-WORKER TASK:ACTIVATE
   mono-ns PROGRESS-NS + {: deadline:n :}
   begin
      NOTE-STARTED atomic@ 0= NOTE-WORKER TASK:DONE? 0= and while
      deadline CHECK-PROGRESS
   repeat
   mono-ns {: halted:n :}
   NOTE-WORKER TASK:HALT
   NOTE-WORKER TASK:JOIN MATCH result
      ok OF drop E-EXEC throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   halted ELAPSED-MS PROMPT-MS < TTRUE
   0 NOTE-CONN @ PG:LIVE? TFALSE ;


\ A listening session receives its own NOTIFY before the statement's
\ ReadyForQuery, so libpq holds it when the statement returns. Both of b's
\ notifications commit in one implicit transaction, so the listener's backend
\ sends them together and one wait sees both.
: NOTIFY-CASES ( -- )
   OPEN {: a :}
   OPEN {: b :}
   a s" listen pqtnote" EXEC-OK
   s" a wait of 0 ms takes a notification libpq already holds" T-LABEL
   a s" notify pqtnote, 'self'" EXEC-OK
   a 0 >MS NOTIFIED? TTRUE
   s" a wait of 0 ms with nothing sent answers false" T-LABEL
   a 0 >MS NOTIFIED? TFALSE
   s" a notification ends the wait, which takes every one queued" T-LABEL
   b s" notify pqtnote, 'one'; notify pqtnote, 'two'"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-SCRIPT
   mono-ns {: sent:n :}
   a NOTIFY-MS >MS NOTIFIED? TTRUE
   sent ELAPSED-MS PROMPT-MS < TTRUE
   a 0 >MS NOTIFIED? TFALSE
   s" a wait with nothing sent answers false once its time is up" T-LABEL
   mono-ns {: idle:n :}
   a BOUND-MS >MS NOTIFIED? TFALSE
   idle ELAPSED-MS {: took:n :}
   took BOUND-MS >= TTRUE
   took BOUND-MS PROMPT-MS + < TTRUE
   s" a wait in a transaction, failed or not, with a statement pending or out of range is refused" T-LABEL
   a [: NOTIFY-IN-TX ;] E-TRANSACTION REFUSES
   a IDLE? TTRUE
   a s" begin" EXEC-OK
   a [: NOTIFY-NOW ;] E-TRANSACTION REFUSES
   a s" select 1/0" EXEC-TAG TAG-FAILED T=
   a [: NOTIFY-NOW ;] E-TRANSACTION REFUSES
   a s" rollback" EXEC-OK
   a s" select 1" SEND
   a [: NOTIFY-NOW ;] E-STATEMENT REFUSES
   a RESULT-POLL CLEAR
   a [: NOTIFY-NEGATIVE ;] E-BOUND REFUSES
   a PG:LIVE? TTRUE
   s" a wait on a lost connection closes it and is refused at once" T-LABEL
   b a BACKEND TERMINATE
   mono-ns {: cut:n :}
   a [: NOTIFY-LONG ;] E-EXEC REFUSES
   cut ELAPSED-MS PROMPT-MS < TTRUE
   a PG:LIVE? TFALSE
   s" a wait on a closed connection is refused" T-LABEL
   b PG:CLOSE
   b [: NOTIFY-NOW ;] E-HANDLE REFUSES
   NOTE-HALT-CASES ;


: ABANDON-KEEP ( PG:connection -- PG:connection )
   dup PG:ABANDON ;


\ ABANDON opens no cancel connection, so cancel-target reachability cannot
\ affect it, and the SLEEPING check proves no cancel left the client: a cancel
\ would have ended the sleep.
: ABANDON-CASES ( -- )
   s" abandon closes a pending statement at once and sends no cancel" T-LABEL
   OPEN {: c :}
   c s" set statement_timeout='1s'" EXEC-OK
   c BACKEND {: backend:n :}
   c s" select pg_sleep(5)" SEND
   mono-ns {: started:n :}
   c PG:ABANDON
   started ELAPSED-MS PROMPT-MS < TTRUE
   c PG:LIVE? TFALSE
   OPEN {: w :}
   w backend SLEEPING 1 T=
   \ The statement_timeout and the dead client socket end the backend.
   w backend AWAIT-GONE
   s" abandon of a closed handle is E-HANDLE" T-LABEL
   c [: ABANDON-KEEP ;] catch {: stale code:n :}
   code E-HANDLE T=
   w PG:CLOSE ;


: SERVER-CASES ( -- )
   OPEN
   DDL-CASES
   PARAM-CASES
   TEXT-BOUNDARY-CASES
   HELD-SPAN-CASES
   LONG-MESSAGE-CASES
   NESTED-SPAN-CASES
   PREPARED-CASES
   DESCRIBE-CASES
   ORIGIN-CASES
   ROLLBACK-CASES
   COMMIT-CASES
   LEFT-BODY-CASES
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
   SCOPE-CASES
   OWNER-CASES
   EXIT-CASES
   CANCEL-CASES
   LOST-CONNECTION-CASES
   DEADLINE-CASES
   NOTIFY-CASES
   ABANDON-CASES
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
   CONNECT-CASES CONNECT-TEXT-CASES CAPACITY-CASES IDENTITY-CASES
   AIO-FAIL-CASES SERVER-CASES ;

\ The AIO loop stops however LOOP-CASES ends. As under finally, a code from
\ AIO:STOP replaces the cases' code, except a deadline's: with a deadline
\ pending, LATE-STOP stops the loop or names why it could not, and the deadline
\ goes on.
: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" pg-test: no conninfo argument; test/db/pg-cluster.f runs this file" 2 die
   then
   CONNECTIONS RESULTS CONFIGURE
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
