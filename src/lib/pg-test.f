\ pg-test.f - package PG against a live PostgreSQL server.
\ test/db/pg-cluster.f starts a private cluster and runs this file with the
\ cluster's conninfo as its one script argument; the gate row is that harness
\ (`bin/hb --load test/db/pg-cluster.f`). Loaded without the argument, the file
\ dies naming the harness; there is no skip.

require src/core/exit-status.f
require src/lib/test/assert.f
require src/lib/test/wait.f
require src/lib/memory.f
require src/lib/fmt.f
require src/lib/process.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/net/tcp4.f
require src/lib/be.f
require src/lib/ieee754.f
require src/lib/string.f
require src/lib/span.f
require src/lib/date.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/test/stderr.f
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

-77 constant BOOM
20 constant CONNECTIONS
64 constant RESULTS

CONNECTIONS TYPED-BUFFER HELD-CONNS PG:connection

0 constant TAG-OK
1 constant TAG-ROWS
2 constant TAG-FAILED


: CONNINFO$ ( -- str )
   0 SCRIPT-ARGV$ ;


: OPEN ( -- PG:connection )
   CONNECTIONS RESULTS CONFIGURE
   CONNINFO$ s" " CONNECT MATCH PG:connect-result
      connected OF ENDOF
      rejected OF type cr E-CONNECT throw ENDOF
   ;MATCH ;


: OUTCOME-TAG ( PG:result -- PG:result n )
   OUTCOME MATCH PG:outcome
      ok OF TAG-OK ENDOF
      rows OF TAG-ROWS ENDOF
      failed OF 2drop TAG-FAILED ENDOF
   ;MATCH ;


: FAILED-SQLSTATE$ ( PG:result -- PG:result str )
   OUTCOME MATCH PG:outcome
      ok OF s" <ok>" ENDOF
      rows OF s" <rows>" ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: FAILED-MESSAGE$ ( PG:result -- PG:result str )
   OUTCOME MATCH PG:outcome
      ok OF s" " ENDOF
      rows OF s" " ENDOF
      failed OF nip ENDOF
   ;MATCH ;


: EXEC-TAG ( PG:connection str -- n )
   [: OUTCOME-TAG swap ;] WITH-RESULT ;


: EXEC-OK ( PG:connection str -- )
   EXEC-TAG TAG-OK T= ;


\ A connection to port 1 fails before any protocol runs, so the arm carries
\ libpq's own message and no connection is left behind.
: PORT-REJECTED$ ( -- str )
   s" host=127.0.0.1 port=1 dbname=none user=none" s" " CONNECT
   MATCH PG:connect-result
      connected OF PG:CLOSE s" " ENDOF
      rejected OF ENDOF
   ;MATCH ;


: ENDS-LF? ( str -- bool )
   {: s:str :}
   s STR:LEN 0= if false exit then
   s STR:DATA s STR:LEN 1- + c@ STR:LF = ;


: CLOSE-KEEP ( PG:connection -- PG:connection )
   dup PG:CLOSE ;


: AWAIT-REJECTED ( str -- )
   s" " CONNECT-START {: rejected :}
   rejected PG:AWAIT MATCH PG:progress
      rejected OF drop true ENDOF
      connected OF PG:CLOSE false ENDOF
      waiting OF 2drop false ENDOF
      completed OF CLEAR false ENDOF
   ;MATCH TTRUE
   rejected PG:LIVE? TFALSE
   rejected [: CLOSE-KEEP ;] catch {: stale code:n :}
   code E-HANDLE T= ;


: CONNECT-NUL ( -- )
   S\" host=127.0.0.1 port=1\x00 dbname=none" s" "
   CONNECT-START PG:CLOSE ;


: PASSWORD-NUL ( -- )
   s" host=127.0.0.1 port=1 dbname=none" S\" pw\x00x"
   CONNECT-START PG:CLOSE ;


: CONNECT-TEXT-CASES ( -- )
   s" rejected connection text releases its claimed slot" T-LABEL
   CONNECTIONS 1+ 0 ?do [: CONNECT-NUL ;] E-FFI-NUL TTHROWSQ loop
   s" a rejected password releases its claimed slot" T-LABEL
   CONNECTIONS 1+ 0 ?do [: PASSWORD-NUL ;] E-FFI-NUL TTHROWSQ loop
   OPEN PG:CLOSE ;


: CONNECT-CASES ( -- )
   PORT-REJECTED$ {: p:str :}
   p STR:LEN 0 > TTRUE
   p ENDS-LF? TFALSE
   s" an attempt on a closed port is rejected by AWAIT" T-LABEL
   s" host=127.0.0.1 port=1 dbname=none user=none" AWAIT-REJECTED ;


\ The cluster rejects a missing database after the protocol starts, so the
\ text is the server's and grows with the name.
: MISSING-DB ( str -- PG:connect-result )
   {: name:str :}
   SB-RESET CONNINFO$ SB-APPEND s"  dbname=" SB-APPEND name SB-APPEND
   SB$ s" " CONNECT ;

: REJECTED$ ( PG:connect-result -- str )
   MATCH PG:connect-result
      connected OF PG:CLOSE E-CONNECT throw ENDOF
      rejected OF ENDOF
   ;MATCH ;


variable LONG-REJECTED-U
TASK:MIN-STACK TASK:TASK LONG-REJECTED-WORKER

: LONG-REJECTED ( -- )
   s" pgtest_rejected_with_a_much_longer_missing_database_name_xyz" MISSING-DB
   REJECTED$ STR:LEN LONG-REJECTED-U !
   0 TASK:RETURN ;


\ The second rejection claims the slot the first freed, the lowest free one, as
\ no other slot frees between them.
: REJECTION-TEXT-CASES ( -- )
   s" a rejection's text survives a longer rejection in another task" T-LABEL
   s" pgtest_rejected" MISSING-DB REJECTED$ {: r:str :}
   r STR:LEN 0 > TTRUE
   r ENDS-LF? TFALSE
   r STR:LEN MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: copy:^ cu :}
   r STR:DATA copy r STR:LEN BYTE-COPY
   0 LONG-REJECTED-U !
   ['] LONG-REJECTED LONG-REJECTED-WORKER TASK:ACTIVATE
   LONG-REJECTED-WORKER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH
   LONG-REJECTED-U @ r STR:LEN > TTRUE
   r copy r STR:LEN >str T$=
   copy cu MEM:RELEASE-BYTES ;


\ A socket directory that does not exist makes libpq reject the attempt inside
\ PQconnectStartParams, before any POLL.
: SOCKET-CONNINFO$ ( -- str )
   s" host=/nonexistent-habu-pgtest port=5432 dbname=none user=none" ;


: SOCKET-REJECTION-CASES ( -- )
   s" a connection libpq rejects at once answers its text" T-LABEL
   SOCKET-CONNINFO$ s" " CONNECT REJECTED$ STR:LEN 0 > TTRUE
   s" an attempt libpq rejects at once is rejected by AWAIT" T-LABEL
   SOCKET-CONNINFO$ AWAIT-REJECTED ;


\ The attempt keeps no slot, as CAPACITY-CASES then needs.
: MALFORMED-CASES ( -- )
   s" a conninfo libpq cannot parse throws E-CONNECT" T-LABEL
   [: s" postgresql://u:SECRETPW%zz@127.0.0.1:1/db" s" " CONNECT-START
      PG:CLOSE ;]
   E-CONNECT TTHROWSQ ;


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


\ PROGRESS-CASES's one task advances both connections, so no progress call
\ there may wait: a waiting call would keep that task from releasing the lock.
: AWAIT-READY ( fd n -- ) {: socket:fd events:n :}
   begin
      socket events READY? if exit then
      TASK:PAUSE
   again ;

: POLL-CONNECTED ( PG:connection -- PG:connection ) {: c :}
   begin
      c PG:POLL MATCH PG:progress
         waiting OF AWAIT-READY ENDOF
         connected OF exit ENDOF
         rejected OF drop E-CONNECT throw ENDOF
         completed OF CLEAR E-CONNECT throw ENDOF
      ;MATCH
      TASK:PAUSE
   again ;

: OPEN-POLL ( -- PG:connection )
   CONNINFO$ s" " CONNECT-START POLL-CONNECTED ;

: RESULT-POLL ( PG:connection -- PG:result ) {: c :}
   begin
      c PG:POLL MATCH PG:progress
         waiting OF AWAIT-READY ENDOF
         completed OF exit ENDOF
         connected OF drop E-EXEC throw ENDOF
         rejected OF drop E-EXEC throw ENDOF
      ;MATCH
      TASK:PAUSE
   again ;

: POLL-EXEC ( PG:connection str -- PG:result ) {: c s:str :}
   c s SEND c RESULT-POLL ;

: SEND-SELECT ( PG:connection -- PG:connection )
   dup s" select 1" SEND ;

: PROGRESS-CASES ( -- )
   s" one task releases a lock another query is waiting for" T-LABEL
   OPEN-POLL {: holder :}
   OPEN-POLL {: waiter :}
   holder s" select pg_advisory_lock(987654)" POLL-EXEC CLEAR
   waiter s" select pg_advisory_lock(987654)" SEND
   waiter PG:POLL MATCH PG:progress
      waiting OF 2drop true ENDOF
      completed OF CLEAR false ENDOF
      connected OF drop false ENDOF
      rejected OF drop false ENDOF
   ;MATCH TTRUE
   waiter [: SEND-SELECT ;] catch {: held code:n :}
   code E-STATEMENT T=
   holder s" select pg_advisory_unlock(987654)::int" POLL-EXEC
   0 >ROW 0 >COL INT 1 T= CLEAR
   waiter RESULT-POLL OUTCOME-TAG TAG-ROWS T= CLEAR
   \ Closing with a query in flight releases its reserved result as well.
   holder s" select pg_advisory_lock(987654)" SEND
   holder PG:CLOSE
   waiter s" select pg_advisory_unlock(987654)::int" POLL-EXEC CLEAR
   waiter PG:CLOSE ;

: SEND-REJECTED ( PG:connection -- PG:connection ) {: c :}
   c [: SEND-SELECT ;] catch {: held code:n :}
   code E-CAPACITY T=
   c ;

: HOLD-RESULTS ( PG:connection n -- PG:connection ) {: c k:n :}
   k 0= if c SEND-REJECTED exit then
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
      rejected OF drop ENDOF
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


: INSERT-ROW ( PG:connection n str -- ) {: c id:n ls:str :}
   c PARAMS
   c id INT+
   c ls TEXT+
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


3 BUFFER: HEX-BYTES
5 BUFFER: FIVE-BYTES
36 BUFFER: REAL-PAIR
700 constant FLOAT4-OID                   \ real, from pg_type.dat

: HEX-BYTES! ( -- )
   0 HEX-BYTES c!  $FF HEX-BYTES 1 + c!  $10 HEX-BYTES 2 + c! ;


\ PostgreSQL's array binary form of {1.5,-2}: ndim, has-null, element type,
\ length, lower bound, then each element's length and IEEE-754 binary32 bits.
: REAL-PAIR! ( -- )
   1 REAL-PAIR BE:U32!
   0 REAL-PAIR 4 + BE:U32!
   FLOAT4-OID REAL-PAIR 8 + BE:U32!
   2 REAL-PAIR 12 + BE:U32!
   1 REAL-PAIR 16 + BE:U32!
   4 REAL-PAIR 20 + BE:U32!
   $3FC00000 REAL-PAIR 24 + BE:U32!
   4 REAL-PAIR 28 + BE:U32!
   $C0000000 REAL-PAIR 32 + BE:U32! ;


: HEX-ANSWER ( PG:result -- PG:result )
   0 >ROW 0 >COL TEXT$ s" 00ff10" T$= ;


\ One past a C int's largest length; reading it would run past HEX-BYTES.
: HUGE-BINARY ( PG:connection -- PG:connection )
   dup HEX-BYTES $80000000 >str BINARY+ ;


: BINARY-CASES ( PG:connection -- PG:connection )
   {: c :}
   HEX-BYTES! REAL-PAIR!
   s" a binary parameter's bytes arrive exactly" T-LABEL
   c PARAMS
   c HEX-BYTES 3 >str BINARY+
   c s" select encode($1::bytea, 'hex')" [: HEX-ANSWER ;] WITH-RESULT
   s" a binary real[] arrives in PostgreSQL's array binary form" T-LABEL
   c PARAMS
   c REAL-PAIR 36 >str BINARY+
   c s" select array_to_string($1::real[], ',')"
   [: 0 >ROW 0 >COL TEXT$ s" 1.5,-2" T$= ;] WITH-RESULT
   s" text and binary parameters share one statement" T-LABEL
   c PARAMS
   c s" ab" TEXT+
   c FIVE-BYTES 5 >str BINARY+
   c s" select $1::text || ':' || length($2::bytea)"
   [: 0 >ROW 0 >COL TEXT$ s" ab:5" T$= ;] WITH-RESULT
   s" an empty binary parameter is zero bytes, not NULL" T-LABEL
   c PARAMS
   c HEX-BYTES 0 >str BINARY+
   c s" select length($1::bytea)"
   [: 0 >ROW 0 >COL TEXT$ s" 0" T$= ;] WITH-RESULT
   s" a prepared statement takes a binary parameter" T-LABEL
   c s" pqt_hex" s" select encode($1::bytea, 'hex')"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-PREPARE
   c PARAMS
   c HEX-BYTES 3 >str BINARY+
   c s" pqt_hex" [: HEX-ANSWER ;] WITH-PREPARED
   s" BINARY+ rejects a length past a C int and keeps what is staged" T-LABEL
   c PARAMS
   c s" kept" TEXT+
   c [: HUGE-BINARY ;] catch {: stale-huge huge-code:n :}
   huge-code E-CAPACITY T=
   c s" select $1::text" [: 0 >ROW 0 >COL TEXT$ s" kept" T$= ;] WITH-RESULT
   c ;


: BITS-ANSWER ( str PG:result -- PG:result )
   swap {: want:str :}
   0 PG:>ROW 0 PG:>COL PG:TEXT$ want T$= ;


\ want is the float's bits in hex. float8send takes only a float8, so its
\ untyped $1 is read in a float8 context.
: F64-CASE ( PG:connection r str -- PG:connection )
   {: c value:r want:str :}
   c PG:PARAMS
   c value PG:F64+
   want c s" select encode(float8send($1::float8), 'hex')" [: BITS-ANSWER ;] PG:WITH-RESULT
   c PG:PARAMS
   c value PG:F64+
   want c s" select encode(float8send($1), 'hex')" [: BITS-ANSWER ;] PG:WITH-RESULT
   c ;


: F64-CASES ( PG:connection -- PG:connection )
   s" F64+ binds 1e-400's nearest double, +0.0, by its bits" T-LABEL
   0 IEEE754:BITS>F64 s" 0000000000000000" F64-CASE
   s" F64+ binds the smallest subnormal by its bits" T-LABEL
   1 IEEE754:BITS>F64 s" 0000000000000001" F64-CASE
   s" F64+ binds 1.5 by its bits" T-LABEL
   1.5 s" 3ff8000000000000" F64-CASE
   s" F64+ binds -0.0 by its bits" T-LABEL
   -0.0 s" 8000000000000000" F64-CASE
   s" F64+ binds a quiet NaN by its bits" T-LABEL
   $7FF8000000000000 IEEE754:BITS>F64 s" 7ff8000000000000" F64-CASE
   s" F64+ binds +inf by its bits" T-LABEL
   $7FF0000000000000 IEEE754:BITS>F64 s" 7ff0000000000000" F64-CASE
   s" F64+ binds -inf by its bits" T-LABEL
   $FFF0000000000000 IEEE754:BITS>F64 s" fff0000000000000" F64-CASE ;


256 BUFFER: EVERY-BYTE

: EVERY-BYTE! ( -- )
   256 0 ?do i EVERY-BYTE i + c! loop ;


\ The bytea 00 01 .. ff, built by the server; a text; and the int4 whose
\ binary form is the bytes "1234", which a text parse would read as digits.
: EVERY-BYTE-SQL$ ( -- str )
   s" select decode(string_agg(lpad(to_hex(g), 2, '0'), '' order by g), 'hex'), 'kept'::text, 825373492 from generate_series(0, 255) g" ;


: BINARY-ANSWER ( PG:result -- PG:result )
   0 >ROW 0 >COL TEXT$ EVERY-BYTE 256 >str T$=
   0 >ROW 1 >COL TEXT$ s" kept" T$=
   0 >ROW 2 >COL INT drop ;


: READ-BINARY ( PG:connection -- PG:connection )
   dup BINARY-RESULT
   dup EVERY-BYTE-SQL$ [: BINARY-ANSWER ;] WITH-RESULT ;


: BINARY-RESULT-CASES ( PG:connection -- PG:connection )
   {: c :}
   EVERY-BYTE!
   s" a binary result gives a bytea's and a text's bytes; INT of it is E-TYPE" T-LABEL
   c PARAMS
   c [: READ-BINARY ;] catch {: stale-bin bin-code:n :}
   bin-code E-TYPE T=
   s" the query after a binary result is text again" T-LABEL
   c s" select 1234" [: 0 >ROW 0 >COL INT 1234 T= ;] WITH-RESULT
   s" PARAMS after a call rejected before its send leaves no binary form staged" T-LABEL
   c BINARY-RESULT
   c [: dup s" " [: ;] WITH-RESULT ;] catch {: stale-unsent unsent-code:n :}
   unsent-code E-STATEMENT T=
   c PARAMS
   c s" select 1234" [: 0 >ROW 0 >COL INT 1234 T= ;] WITH-RESULT
   c ;


\ Checked against the message the server builds: six KiB of two-byte characters,
\ then a longer message the connection's copy must grow past the first to hold.
: RAISED-SAME ( PG:connection str str -- PG:connection )
   {: c same:str ms:str :}
   c PARAMS
   c ms TEXT+
   c same
   [: OUTCOME-TAG TAG-ROWS T= 0 >ROW 0 >COL TEXT$ s" t" T$= ;] WITH-RESULT
   c ;


: RAISED-BODY ( PG:connection str PG:result -- PG:connection PG:result )
   FAILED-SQLSTATE$ s" P0001" T$=
   FAILED-MESSAGE$ swap >r
   RAISED-SAME r> ;


: RAISED-WHOLE ( PG:connection str str -- PG:connection )
   {: c raise:str same:str :}
   c PARAMS
   c same c raise [: RAISED-BODY ;] WITH-RESULT ;


: LONG-MESSAGE-CASES ( PG:connection -- PG:connection )
   s" a server message past one KiB arrives whole and goes back as text" T-LABEL
   s" do $$ begin raise exception '%', 'x' || repeat(chr(233), 3000); end $$"
   s" select $1::text = ('x' || repeat(chr(233), 3000))"
   RAISED-WHOLE
   s" a longer message after it grows the copy and arrives whole" T-LABEL
   s" do $$ begin raise exception '%', repeat('yz', 50000); end $$"
   s" select $1::text = repeat('yz', 50000)"
   RAISED-WHOLE ;


\ A failed span is the result's own bytes, so a failing statement inside the
\ body on the same connection leaves it as it was. The two messages have one
\ length, so a copy the connection shared would show the inner bytes.
: INNER-FAILS ( PG:connection -- PG:connection )
   dup s" do $$ begin raise exception 'inner message'; end $$"
   [: FAILED-MESSAGE$ s" inner message" T$= ;] WITH-RESULT ;


: NESTED-SPAN-BODY ( PG:connection PG:result -- PG:connection PG:result )
   FAILED-MESSAGE$ swap >r
   swap INNER-FAILS swap
   s" outer message" T$=
   r> ;


: NESTED-SPAN-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a failed span survives a nested failing statement on its connection" T-LABEL
   c c s" do $$ begin raise exception 'outer message'; end $$"
   [: NESTED-SPAN-BODY ;] WITH-RESULT ;


\ A deferred constraint fails the COMMIT, not the insert, with a message longer
\ than any the copy has held (this runs before LONG-MESSAGE-CASES). The
\ transaction verb's rejection is E-EXEC, not an OUTCOME, so a `failed` span held
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
   FAILED-MESSAGE$ swap >r
   swap DEFERRED-FAILS swap
   s" short" T$=
   r> ;


: REPORT-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a failed span is the server's primary message" T-LABEL
   c s" create function report_f() returns int language plpgsql as $$ begin raise exception 'm' using detail = 'd', hint = 'h'; end $$" EXEC-OK
   c s" select report_f()"
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" P0001" T$=
      FAILED-MESSAGE$ s" m" T$= ;] WITH-RESULT
   s" a comment-only statement fails with an empty report" T-LABEL
   c s" -- only a comment"
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" " T$=
      FAILED-MESSAGE$ s" " T$= ;] WITH-RESULT
   c ;


: HELD-SPAN-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a failed COMMIT leaves a held failed span as it was" T-LABEL
   c s" create table held_t (x int)" EXEC-OK
   c s" create function held_f() returns trigger language plpgsql as $$ begin raise exception '%', repeat('z', 100000); end $$" EXEC-OK
   c s" create constraint trigger held_tg after insert on held_t deferrable initially deferred for each row execute function held_f()" EXEC-OK
   c c s" do $$ begin raise exception 'short'; end $$" [: HELD-SPAN-BODY ;] WITH-RESULT ;


: PREPARED-INSERT ( PG:connection n str -- ) {: c id:n ls:str :}
   c PARAMS
   c id INT+
   c ls TEXT+
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
   mono-ns swap - PROCESS:NS-PER-MS / ;


: DESCRIBED$ ( -- str )
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
   s" describe rejects a pending parameter list" T-LABEL
   c 1 INT+
   c [: DESCRIBE-PENDING ;] catch {: stale-pending pending-code:n :}
   pending-code E-STATEMENT T=
   c PARAMS
   c ;


: ORIGIN-SQL$ ( -- str )
   s" select id as key, label, length(label) as len from pqt" ;


: SCALAR ( PG:connection str -- n )
   [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;


: ATTNUM ( PG:connection str -- n ) {: c name:str :}
   c PARAMS
   c name TEXT+
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


\ The parameters are built before the transaction opens, so BEGIN must leave
\ the pending list alone.
: COMMIT-BODY ( PG:connection -- PG:connection )
   dup IDLE? TFALSE
   dup s" insert into pqt (id, label, note) values ($1, $2, null)"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-RESULT ;


: COMMIT-CASES ( PG:connection -- PG:connection ) {: c :}
   c PARAMS
   c 901 INT+
   c s" kept" TEXT+
   c [: c COMMIT-BODY drop ;] WITH-TRANSACTION
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


: COUNTED-BODY ( PG:connection -- n )
   dup s" insert into pqt (id, label, note) values (902, 'counted', null)"
   EXEC-OK
   902 COUNT-ID ;


: ANSWER-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a transaction's body answers a value read inside it" T-LABEL
   c [: c COUNTED-BODY ;] WITH-TRANSACTION 1 T=
   c IDLE? TTRUE
   c 902 COUNT-ID 1 T=
   c ;


: SOFT-FAIL-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqt (id, label, note) values (970, 'soft', null)"
   EXEC-OK
   dup s" select 1/0" EXEC-TAG TAG-FAILED T= ;


: SOFT-FAIL-RUN ( PG:connection -- PG:connection )
   dup [: SOFT-FAIL-BODY ;] WITH-TRANSACTION ;


: SOFT-FAIL-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a body that returns after a failed statement rolls back with E-EXEC" T-LABEL
   c [: SOFT-FAIL-RUN ;] catch {: stale code:n :}
   code E-EXEC T=
   c IDLE? TTRUE
   c 970 COUNT-ID 0 T=
   c ;


: OWN-END-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqt (id, label, note) values (973, 'ended', null)"
   EXEC-OK
   dup s" rollback" EXEC-OK ;


: OWN-END-RUN ( PG:connection -- PG:connection )
   dup [: OWN-END-BODY ;] WITH-TRANSACTION ;


: OWN-END-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a body that ended its own transaction throws E-EXEC" T-LABEL
   c [: OWN-END-RUN ;] catch {: stale code:n :}
   code E-EXEC T=
   c IDLE? TTRUE
   c 973 COUNT-ID 0 T=
   c ;


1 TYPED-BUFFER REOPENED PG:connection

: REOPEN-BODY ( PG:connection -- PG:connection )
   dup PG:CLOSE
   OPEN dup 0 REOPENED !
   s" begin" EXEC-OK ;


: REOPEN-RUN ( PG:connection -- PG:connection )
   dup [: REOPEN-BODY ;] WITH-TRANSACTION ;


: REOPEN-THROW-BODY ( PG:connection -- PG:connection ) REOPEN-BODY BOOM throw ;


: REOPEN-THROW-RUN ( PG:connection -- PG:connection )
   dup [: REOPEN-THROW-BODY ;] WITH-TRANSACTION ;


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
      FAILED-MESSAGE$ STR:LEN 0 > TTRUE ;] WITH-RESULT
   c ;


: READ-BAD-COLUMN ( PG:connection -- PG:connection )
   dup s" select id from pqt where id = 1"
   [: OUTCOME-TAG TAG-ROWS T= 0 >ROW 99 >COL TEXT$ drop ;] WITH-RESULT ;


: READ-BAD-ROW ( PG:connection -- PG:connection )
   dup s" select id from pqt where id = 1" [: 99 >ROW 0 >COL TEXT$ drop ;] WITH-RESULT ;


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
\ carries no SQLSTATE and no message at all - so the module rejects
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


: NESTED-BODY ( PG:connection PG:result -- PG:connection PG:result )
   swap
   dup s" select 2" [: 0 >ROW 0 >COL INT 2 T= ;] WITH-RESULT
   INNER-CAUGHT
   swap
   0 >ROW 0 >COL INT 1 T= ;


: NESTED-RUN ( PG:connection -- PG:connection )
   dup s" select 1" [: NESTED-BODY ;] WITH-RESULT ;


: HAND-OUTER ( PG:connection PG:result -- PG:connection PG:result )
   swap dup s" select 2" [: rot ;] WITH-RESULT ;


: OUTER-HANDED ( PG:connection -- PG:connection )
   dup s" select 1" [: HAND-OUTER ;] WITH-RESULT ;


\ CLEAR rejects the body's own token, before the body could hand back another.
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


: BIG$ ( -- str )
   BIG-BUF BYTE-VIEW BIG-LEN >str ;


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


: STMT+ ( str -- ) {: s:str :}
   STMT-U @ s STR:LEN + STMT-CAP > if E-CAPACITY throw then
   s STR:DATA STMT-BUF BYTE-VIEW STMT-U @ + s STR:LEN BYTE-COPY
   STMT-U @ s STR:LEN + STMT-U ! ;


: STMT-C+ ( n -- ) {: ch:n :}
   STMT-U @ 1 + STMT-CAP > if E-CAPACITY throw then
   ch STMT-BUF BYTE-VIEW STMT-U @ + c!
   STMT-U @ 1 + STMT-U ! ;


: STMT-N+ ( n -- ) {: v:n :}
   v 10 < if v $30 + STMT-C+ exit then
   v 10 / RECURSE
   v 10 mod $30 + STMT-C+ ;


: STMT$ ( -- str )
   STMT-BUF BYTE-VIEW STMT-U @ >str ;


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
   STMT$ STR:LEN OLD-STATEMENT-CAP > TTRUE
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
   s" a 65536th parameter is rejected and leaves the 65535 to run" T-LABEL
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
   dup BIG$ STR:DATA MEM-MAX-N 1- >str TEXT+ ;


\ With nothing staged the fill is zero and the same length wraps no sum: the
\ arena asks for a MEM-MAX-N-byte mapping, and the mapping is what fails.
: ADD-LONE-HUGE-TEXT ( PG:connection -- PG:connection )
   dup PARAMS
   dup BIG$ STR:DATA MEM-MAX-N 1- >str TEXT+ ;


: PARAM-LENGTH-CASES ( PG:connection -- PG:connection ) {: c :}
   s" TEXT+ rejects a length that would wrap the arena fill" T-LABEL
   c [: ADD-HUGE-TEXT ;] catch {: stale-param code:n :}
   code E-CAPACITY T=
   s" TEXT+ of that length on an empty list fails in the allocator" T-LABEL
   c [: ADD-LONE-HUGE-TEXT ;] catch {: stale-lone lone-code:n :}
   lone-code E-MEM-MAP T=
   c PARAMS
   c ;


\ WITH-SCRIPT is the only path that takes more than one statement; WITH-RESULT
\ and WITH-PREPARED ride the extended protocol, where a second command is 42601.
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
      FAILED-MESSAGE$ STR:LEN 0 > TTRUE ;] WITH-SCRIPT
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


: SCRIPT-REJECTION-CASES ( PG:connection -- PG:connection ) {: c :}
   c [: SCRIPT-EMPTY ;] catch {: stale-empty code:n :}
   code E-STATEMENT T=
   c [: SCRIPT-WITH-PARAMS ;] catch {: stale-params param-code:n :}
   param-code E-STATEMENT T=
   c PARAMS
   c ;


: SELECT-ONE ( PG:connection -- )
   s" select 1" [: 0 >ROW 0 >COL INT 1 T= ;] WITH-RESULT ;


\ COPY TO STDOUT fails with PG's own spans although the server completed it.
: COPY-OUT-FAILS ( PG:connection str -- )
   {: c s:str :}
   c s
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" 0A000" T$=
      FAILED-MESSAGE$ s" COPY is not supported" T$= ;] WITH-RESULT
   c SELECT-ONE ;


\ 200000 rows take more than one read of the socket.
: COPY-CASES ( PG:connection -- PG:connection ) {: c :}
   s" COPY TO STDOUT of one row fails" T-LABEL
   c s" copy (select 1) to stdout" COPY-OUT-FAILS
   s" COPY TO STDOUT of 200000 rows fails" T-LABEL
   c s" copy (select generate_series(1,200000)) to stdout" COPY-OUT-FAILS
   s" COPY FROM STDIN fails with the server's report of PG's rejection" T-LABEL
   c s" create temp table pqtcopy (id int)" EXEC-OK
   c s" copy pqtcopy from stdin"
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" 57014" T$=
      FAILED-MESSAGE$ s" COPY from stdin failed: COPY is not supported" STR:CONTAINS? TTRUE ;]
   WITH-RESULT
   c SELECT-ONE
   s" A COPY TO STDOUT the server fails midway is the server's failure" T-LABEL
   c s" copy (select 1/(x-1) from generate_series(0,2) x) to stdout"
   [: OUTCOME-TAG TAG-FAILED T= FAILED-SQLSTATE$ s" 22012" T$= ;] WITH-RESULT
   c SELECT-ONE
   s" A script's error after its COPY TO STDOUT is the outcome" T-LABEL
   c s" copy (select 1) to stdout; select 1/0"
   [: OUTCOME-TAG TAG-FAILED T= FAILED-SQLSTATE$ s" 22012" T$= ;] WITH-SCRIPT
   c SELECT-ONE
   s" A script's statements after its COPY TO STDOUT run and stand" T-LABEL
   c s" copy (select 1) to stdout; insert into pqtcopy values (1); select 1"
   [: OUTCOME-TAG TAG-FAILED T=
      FAILED-SQLSTATE$ s" 0A000" T$=
      FAILED-MESSAGE$ s" COPY is not supported" T$= ;] WITH-SCRIPT
   c s" select count(*) from pqtcopy" [: 0 >ROW 0 >COL INT 1 T= ;] WITH-RESULT
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
\ orphan PGresult and the body's token is rejected afterwards. The scope still
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
\ made it. The worker presents the main task's connection and is rejected
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
\ close a live attempt and leave a rejected one, already retired, alone.
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
   CONNINFO$ s" " CONNECT-START 0 EXIT-CONN !
   0 TASK:RETURN ;

: EXIT-REJECTED ( -- )
   s" host=127.0.0.1 port=1 dbname=none user=none" s" "
   CONNECT-START 0 EXIT-CONN !
   0 EXIT-CONN @ PG:AWAIT MATCH PG:progress
      rejected OF drop ENDOF
      connected OF PG:CLOSE E-CONNECT throw ENDOF
      waiting OF 2drop E-CONNECT throw ENDOF
      completed OF CLEAR E-CONNECT throw ENDOF
   ;MATCH
   0 TASK:RETURN ;

: EXIT-SEND ( -- )
   CONNINFO$ s" " CONNECT-START 0 EXIT-CONN !
   0 EXIT-CONN @ PG:AWAIT MATCH PG:progress
      connected OF drop ENDOF
      rejected OF drop E-CONNECT throw ENDOF
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
      rejected OF drop E-EXEC throw ENDOF
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

: AWAIT-STARTED ( -- )
   EXIT-STARTED 1 EXIT-WORKER TEST-WAIT:FOR-CELL ;

: EXIT-CASES ( -- )
   ['] EXIT-CLEAN EXIT-WORKER TASK:AT-EXIT
   ['] EXIT-OPEN EXIT-WORKER TASK:ACTIVATE
   EXIT-JOIN
   EXIT-LIVE @ TTRUE
   0 EXIT-CONN @ PG:LIVE? TFALSE
   ['] EXIT-REJECTED EXIT-WORKER TASK:ACTIVATE
   EXIT-JOIN
   EXIT-LIVE @ TFALSE
   0 EXIT-CONN @ PG:LIVE? TFALSE ;

: BACKEND-COUNT ( PG:connection n str -- n )
   {: c pid:n sql:str :}
   c PARAMS c pid INT+
   c sql [: 0 >ROW 0 >COL INT swap ;] WITH-RESULT ;

: LOCKED? ( PG:connection n -- bool )
   s" select count(*) from pg_stat_activity where pid=$1 and wait_event_type='Lock'"
   BACKEND-COUNT 1 = ;

: AWAIT-LOCK ( PG:connection n -- )
   {: c pid:n :}
   begin c pid LOCKED? 0= while TASK:PAUSE repeat ;

: AWAIT-GONE ( PG:connection n -- )
   {: c pid:n :}
   begin
      c pid s" select count(*) from pg_stat_activity where pid=$1" BACKEND-COUNT 0 >
   while TASK:PAUSE repeat ;

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
   AWAIT-STARTED
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
      rejected OF drop E-EXEC throw ENDOF
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
   AWAIT-STARTED
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


: UNREAD-BODY ( PG:connection -- PG:connection )
   dup s" insert into pqt (id, label, note) values (971, 'unread', null)"
   EXEC-OK
   dup s" select 1" SEND ;


: UNREAD-RUN ( PG:connection -- PG:connection )
   dup [: UNREAD-BODY ;] WITH-TRANSACTION ;


\ The backend of a connection wrongly left open never goes, so it is closed.
: UNREAD-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a body that returns with a statement pending commits nothing" T-LABEL
   OPEN {: p :}
   p BACKEND {: pid:n :}
   p [: UNREAD-RUN ;] catch {: stale code:n :}
   code E-STATEMENT T=
   p PG:LIVE? dup TFALSE if p PG:CLOSE then
   c pid AWAIT-GONE
   c 971 COUNT-ID 0 T=
   c ;


: TX-PAUSE ( -- )
   0 EXIT-CONN @
   s" insert into pqt (id, label, note) values (972, 'halted', null)"
   EXEC-OK
   1 EXIT-STARTED atomic!
   begin TASK:PAUSE again ;


: TX-PAUSED ( -- )
   OPEN dup 0 EXIT-CONN !
   dup BACKEND EXIT-BACKEND atomic!
   [: TX-PAUSE ;] WITH-TRANSACTION ;


: TX-HALT-CASES ( PG:connection -- PG:connection ) {: c :}
   s" a halt inside a transaction closes its connection at once" T-LABEL
   0 EXIT-STARTED !
   ['] TX-PAUSED EXIT-WORKER TASK:ACTIVATE
   AWAIT-STARTED
   mono-ns {: halted:n :}
   EXIT-WORKER TASK:HALT
   EXIT-WORKER TASK:JOIN MATCH result
      ok OF drop E-EXEC throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   halted ELAPSED-MS PROMPT-MS < TTRUE
   EXIT-LIVE @ TFALSE
   c EXIT-BACKEND atomic@ AWAIT-GONE
   c 972 COUNT-ID 0 T=
   c ;


$7F000001 constant LOOPBACK
variable LISTENER-PORT
TASK:MIN-STACK TASK:TASK SILENT-WORKER

\ The server at LISTENER-PORT is the test itself, which answers no SSL or GSS
\ request, so libpq sends neither.
: LISTENER-CONNINFO$ ( -- str )
   SB-RESET s" hostaddr=127.0.0.1 dbname=none user=none sslmode=disable gssencmode=disable port=" SB-APPEND
   LISTENER-PORT @ FMT:SB-U
   SB$ ;

\ A server that never answers: the kernel completes the handshake from the
\ listen backlog, so libpq sends its first packet and waits for a reply.
: SILENT-CONNECT ( -- )
   LISTENER-CONNINFO$ s" " CONNECT MATCH PG:connect-result
      connected OF PG:CLOSE ENDOF
      rejected OF drop ENDOF
   ;MATCH
   0 TASK:RETURN ;

: STATUS-ERRNO ( TCP4:status -- n )
   MATCH TCP4:status
      ok OF 0 ENDOF
      failed OF TCP4:ERRNO>N ENDOF
   ;MATCH ;

: LISTENER ( -- TCP4:listener )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND MATCH TCP4:bind-result
      bound OF ENDOF
      failed OF TCP4:ERRNO>N drop E-EXEC throw ENDOF
   ;MATCH
   dup 1 TCP4:LISTEN STATUS-ERRNO 0 T=
   dup TCP4:LOCAL MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N LISTENER-PORT ! drop ENDOF
      failed OF TCP4:ERRNO>N drop E-EXEC throw ENDOF
   ;MATCH ;

\ A queued connection is libpq's socket, so the attempt holds its slot and its
\ next PAUSE is its AIO wait. A worker that ended first leaves its code to the join.
: AWAIT-QUEUED ( TCP4:listener -- ) {: lis :}
   begin
      SILENT-WORKER TASK:DONE? if exit then
      lis TCP4:PENDING? MATCH TCP4:ready-result
         ready OF exit ENDOF
         idle OF ENDOF
         failed OF TCP4:ERRNO>N drop E-EXEC throw ENDOF
      ;MATCH
      TASK:PAUSE
   again ;

: AWAIT-SILENT-DONE ( -- )
   begin SILENT-WORKER TASK:DONE? 0= while TASK:PAUSE repeat ;

\ A halt observed at PAUSE stores no result, so the join answers
\ E-TASK-NO-RESULT. The last OPEN needs the slot the halted attempt held.
: HALT-CONNECT-CASES ( -- )
   s" halt ends a connect the server never answers and frees its slot" T-LABEL
   LISTENER {: lis :}
   ['] SILENT-CONNECT SILENT-WORKER TASK:ACTIVATE
   lis AWAIT-QUEUED
   SILENT-WORKER TASK:HALT
   AWAIT-SILENT-DONE
   SILENT-WORKER TASK:JOIN MATCH result
      ok OF drop E-EXEC throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   CONNECTIONS 1- 0 ?do OPEN i HELD-CONNS ! loop
   [: OPEN PG:CLOSE ;] catch 0 T=
   CONNECTIONS 1- 0 ?do i HELD-CONNS @ PG:CLOSE loop
   lis TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


\ A peer's replies: AuthenticationOk and ReadyForQuery, then a message whose
\ length is under the four bytes of the length itself.
20 BUFFER: PEER-BYTES
15 constant STARTUP-LEN

: FILL-PEER ( -- )
   $52 PEER-BYTES c!  8 PEER-BYTES 4 + c!
   $5A PEER-BYTES 9 + c!  5 PEER-BYTES 13 + c!  $49 PEER-BYTES 14 + c!
   $43 PEER-BYTES STARTUP-LEN + c! ;

: ACCEPT-PEER ( TCP4:listener -- TCP4:connection ) {: lis :}
   begin
      lis TCP4:PENDING? MATCH TCP4:ready-result
         ready OF true ENDOF
         idle OF false ENDOF
         failed OF TCP4:ERRNO>N drop E-EXEC throw ENDOF
      ;MATCH
   0= while
      TASK:PAUSE
   repeat
   lis TCP4:ACCEPT MATCH TCP4:accept-result
      accepted OF TCP4:PORT>N drop TCP4:ADDRESS>N drop ENDOF
      failed OF TCP4:ERRNO>N drop E-EXEC throw ENDOF
   ;MATCH ;

: PEER-WRITE ( TCP4:connection str -- ) {: peer s:str :}
   peer s TCP4:WRITE STATUS-ERRNO 0 T= ;

\ libpq drops a connection whose reply breaks the protocol, so pg.f's next send
\ on it fails: E-EXEC, and the connection is closed with it.
: FAILED-SEND-CASES ( -- )
   s" a failed send throws E-EXEC and closes the connection" T-LABEL
   FILL-PEER
   LISTENER {: lis :}
   LISTENER-CONNINFO$ s" " CONNECT-START {: c :}
   lis ACCEPT-PEER {: peer :}
   peer PEER-BYTES STARTUP-LEN >str PEER-WRITE
   c POLL-CONNECTED drop
   c s" select 1" SEND
   peer PEER-BYTES STARTUP-LEN + 5 >str PEER-WRITE
   c RESULT-POLL CLEAR
   c [: SEND-SELECT ;] catch {: held code:n :}
   code E-EXEC T=
   c PG:LIVE? TFALSE
   peer TCP4:CLOSE STATUS-ERRNO 0 T=
   lis TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


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


: REUSE-CASES ( -- )
   OPEN {: fresh :}
   s" a stale token is E-CLEARED after a later query reuses its slot" T-LABEL
   0 CLEAR-RETURNED !
   OPEN fresh [: CLEAR-REUSED ;] catch {: gone kept code:n :}
   code E-CLEARED T=
   CLEAR-RETURNED @ 0 T=
   fresh PG:CLOSE ;


\ NOTIFIED?'s largest wait, past WAIT-MAX: a lost connection, a halt or a
\ notification cuts it short, and PROMPT-MS bounds how long each takes.
MEM-MAX-N constant FOREVER-MS

: NOTIFY-NOW ( PG:connection -- PG:connection )
   dup 0 >MS NOTIFIED? drop ;

: NOTIFY-LONG ( PG:connection -- PG:connection )
   dup FOREVER-MS >MS NOTIFIED? drop ;

: NOTIFY-NEGATIVE ( PG:connection -- PG:connection )
   dup -1 >MS NOTIFIED? drop ;

: NOTIFY-IN-TX ( PG:connection -- PG:connection )
   dup [: NOTIFY-NOW ;] WITH-TRANSACTION ;

: REJECTS ( PG:connection [ PG:connection -- PG:connection ] n -- )
   {: c body want:n :}
   c body catch {: stale code:n :}
   code want T= ;

\ pg_terminate_backend's own wait has no unbounded timeout, so the backend's
\ exit is awaited in pg_stat_activity.
: TERMINATE ( PG:connection n -- )
   {: c pid:n :}
   c PARAMS c pid INT+
   c s" select pg_terminate_backend($1)"
   [: OUTCOME-TAG TAG-ROWS T= 0 >ROW 0 >COL TEXT$ s" t" T$= ;] WITH-RESULT
   c pid AWAIT-GONE ;


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
   FOREVER-MS >MS NOTIFIED? drop
   0 TASK:RETURN ;


: NOTE-HALT-CASES ( -- )
   s" a halt ends a task waiting for a notification" T-LABEL
   0 NOTE-STARTED !
   ['] NOTE-CLEAN NOTE-WORKER TASK:AT-EXIT
   ['] NOTE-WAIT NOTE-WORKER TASK:ACTIVATE
   NOTE-STARTED 1 NOTE-WORKER TEST-WAIT:FOR-CELL
   mono-ns {: halted:n :}
   NOTE-WORKER TASK:HALT
   NOTE-WORKER TASK:JOIN MATCH result
      ok OF drop E-EXEC throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   halted ELAPSED-MS PROMPT-MS < TTRUE
   0 NOTE-CONN @ PG:LIVE? TFALSE ;

create PATH-BUF PATH-CAP allot          \ the stderr capture file's path
variable PATH-U

: PATH$ ( -- str )  PATH-BUF PATH-U @ >str ;

: PATH-MAKE ( -- )
   s" pg-test" FS:MKTEMP-DIR {: dir:str :}
   dir FS:CLEANUP-TREE+
   dir s" stderr.txt" PATH-BUF FS:JOIN-PATH PATH-U ! ;



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
   s" a notification ends a wait bound past one poll's, which takes every one queued" T-LABEL
   b s" notify pqtnote, 'one'; notify pqtnote, 'two'"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-SCRIPT
   mono-ns {: sent:n :}
   a FOREVER-MS >MS NOTIFIED? TTRUE
   sent ELAPSED-MS PROMPT-MS < TTRUE
   a 0 >MS NOTIFIED? TFALSE
   s" a wait with nothing sent answers false once its time is up" T-LABEL
   mono-ns {: idle:n :}
   a BOUND-MS >MS NOTIFIED? TFALSE
   idle ELAPSED-MS {: took:n :}
   took BOUND-MS >= TTRUE
   took BOUND-MS PROMPT-MS + < TTRUE
   s" a wait in a transaction, failed or not, with a statement pending or out of range is rejected" T-LABEL
   a [: NOTIFY-IN-TX ;] E-TRANSACTION REJECTS
   a IDLE? TTRUE
   a s" begin" EXEC-OK
   a [: NOTIFY-NOW ;] E-TRANSACTION REJECTS
   a s" select 1/0" EXEC-TAG TAG-FAILED T=
   a [: NOTIFY-NOW ;] E-TRANSACTION REJECTS
   a s" rollback" EXEC-OK
   a s" select 1" SEND
   a [: NOTIFY-NOW ;] E-STATEMENT REJECTS
   a RESULT-POLL CLEAR
   a [: NOTIFY-NEGATIVE ;] E-BOUND REJECTS
   a PG:LIVE? TTRUE
   s" a wait on a lost connection closes it and fails at once" T-LABEL
   b a BACKEND TERMINATE
   mono-ns {: cut:n :}
   a [: NOTIFY-LONG ;] E-EXEC REJECTS
   cut ELAPSED-MS PROMPT-MS < TTRUE
   a PG:LIVE? TFALSE
   s" a wait on a closed connection is rejected" T-LABEL
   b PG:CLOSE
   b [: NOTIFY-NOW ;] E-HANDLE REJECTS
   NOTE-HALT-CASES ;


$100 constant HEARD-CAP
HEARD-CAP BUFFER: HEARD
variable HEARD-U

: HEARD+ ( str -- ) {: s:str :}
   HEARD-U @ s STR:LEN + HEARD-CAP > if E-CAPACITY throw then
   s STR:DATA HEARD HEARD-U @ + s STR:LEN BYTE-COPY
   HEARD-U @ s STR:LEN + HEARD-U ! ;

\ Each pair lands as `channel=payload;`.
: HEAR ( str str -- ) {: cs:str ps:str :}
   cs HEARD+ s" =" HEARD+ ps HEARD+ s" ;" HEARD+ ;

\ b's three notifications commit in one implicit transaction, so a's backend
\ sends them together and one wait takes all three.
: NOTIFIED-CASES ( -- )
   OPEN {: a :}
   OPEN {: b :}
   a s" listen pqtjob" EXEC-OK
   a s" listen pqtview" EXEC-OK
   s" NOTIFIED hands each channel and payload over in order" T-LABEL
   0 HEARD-U !
   b s" notify pqtjob, '17'; notify pqtview, 'p2'; notify pqtjob, ''"
   [: OUTCOME-TAG TAG-OK T= ;] WITH-SCRIPT
   a FOREVER-MS >MS [: HEAR ;] NOTIFIED TTRUE
   HEARD HEARD-U @ >str s" pqtjob=17;pqtview=p2;pqtjob=;" T$=
   a PG:CLOSE
   b PG:CLOSE ;


: ENDED-NOW ( PG:connection -- PG:connection )
   dup ENDED? drop ;

\ A pending statement's socket, which stays the connection's for its life.
: PENDING-SOCKET ( PG:connection -- fd )
   PG:POLL MATCH PG:progress
      waiting OF drop ENDOF
      completed OF CLEAR E-EXEC throw ENDOF
      connected OF drop E-EXEC throw ENDOF
      rejected OF drop E-EXEC throw ENDOF
   ;MATCH ;


\ b's lock keeps a's statement pending, which gives a's socket; b's notify then
\ arrives there with a idle, so ENDED? is the read that takes it.
: ENDED-CASES ( -- )
   OPEN {: a :}
   OPEN {: b :}
   s" an idle open connection has not ended and runs a statement" T-LABEL
   a ENDED? TFALSE
   a s" select 1" EXEC-TAG TAG-ROWS T=
   s" a pending connection is rejected" T-LABEL
   b s" select pg_advisory_lock(987655)" EXEC-TAG TAG-ROWS T=
   a s" select pg_advisory_xact_lock(987655)" SEND
   a PENDING-SOCKET {: socket:fd :}
   a [: ENDED-NOW ;] E-STATEMENT REJECTS
   b s" select pg_advisory_unlock(987655)" EXEC-TAG TAG-ROWS T=
   a RESULT-POLL OUTCOME-TAG TAG-ROWS T= CLEAR
   s" a notification ENDED? reads stays for NOTIFIED?" T-LABEL
   a s" listen pqtended" EXEC-OK
   b s" notify pqtended" EXEC-OK
   socket AIO:READABLE AWAIT-READY
   a ENDED? TFALSE
   a 0 >MS NOTIFIED? TTRUE
   s" a connection the server ended has ended and is closed" T-LABEL
   b a BACKEND TERMINATE
   \ The backend leaves pg_stat_activity before its socket closes.
   begin a ENDED? 0= while TASK:PAUSE repeat
   a PG:LIVE? TFALSE
   b PG:CLOSE ;


TYPED-VARIABLE FILE-B ^VEC:vector<u8>   \ the capture file's bytes, read back
variable ALL-LINES
variable BARE-LINES
variable WARN-LINES


\ The whole file, valid until the next read.
: FILE$ ( -- str )
   PATH$ FS:FILE-SIZE {: size:n :}
   FILE-B @ 0 VEC:RESIZE
   FILE-B @ size VEC:ENSURE
   FILE-B @ VEC:BYTES {: buf:str :}
   PATH$ buf STR:DATA buf STR:LEN FS:READ-ALL size T=
   FILE-B @ VEC:BYTES ;


\ libpq's default notice processor writes the message as is.
: BARE? ( str -- bool ) {: s:str :}
   s s" FATAL:" STR:STARTS-WITH?
   s s" NOTICE:" STR:STARTS-WITH? or
   s s" WARNING:" STR:STARTS-WITH? or ;


: PG-WARN? ( str -- bool ) {: s:str :}
   s STR:LEN DATE:TIME-LEN < if false exit then
   s STR:DATA DATE:TIME-LEN + s STR:LEN DATE:TIME-LEN - >str s"  warn pg: WARNING:  w" STR:= ;


: LINE-COUNT ( str -- ) {: s:str :}
   s STR:LEN 0= if exit then
   1 ALL-LINES +!
   s BARE? if 1 BARE-LINES +! then
   s PG-WARN? if 1 WARN-LINES +! then ;


: LINES-COUNT ( str -- ) {: at:str :}
   0 ALL-LINES !  0 BARE-LINES !  0 WARN-LINES !
   0 begin
      at swap STR:LF swap STR:SPLIT-NEXT
   while
      >r LINE-COUNT r>
   repeat
   drop drop ;


: RAISES ( PG:connection -- )
   s" DO $$ BEGIN RAISE NOTICE 'n'; RAISE INFO 'i'; RAISE WARNING 'w'; END $$"
   EXEC-OK ;


\ The terminated backend's FATAL reaches a while it is idle, so the wait's
\ PQnotifies hands it to libpq's notice processor.
: NOTICE-CASES ( -- )
   OPEN {: a :}
   OPEN {: b :}
   s" a FATAL read while idle is no line" T-LABEL
   b a BACKEND TERMINATE
   PATH-MAKE
   PATH$ STDERR:CAPTURE
   [: a [: NOTIFY-LONG ;] E-EXEC REJECTS  b RAISES ;]
   [: STDERR:;CAPTURE ;] finally
   a PG:LIVE? TFALSE
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW FILE-B !
   [: FILE$ LINES-COUNT ;] [: FILE-B @ VEC:DELETE  NULL-PTR FILE-B ! ;] finally
   BARE-LINES @ 0 T=
   s" a WARNING is one warn line; NOTICE and INFO add none" T-LABEL
   WARN-LINES @ 1 T=
   ALL-LINES @ 1 T=
   b PG:CLOSE ;


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


\ Longer than a fresh call arena, so staging it grows the arena, and a
\ space every 13 bytes, which a conninfo value would have to quote.
5000 constant PW-LEN
PW-LEN BUFFER: PW-RIGHT

: PW-BYTE ( n -- n )
   13 mod dup 12 = if drop $20 else $61 + then ;

: PW-RIGHT! ( -- )
   PW-LEN 0 ?do i PW-BYTE PW-RIGHT i + c! loop ;

PW-RIGHT!

: PW-RIGHT$ ( -- str )   PW-RIGHT PW-LEN >str ;
: PW-WRONG$ ( -- str )   s" pgtest-pw-wrong" ;


: SIGNED-IN? ( PG:connect-result -- bool )
   MATCH PG:connect-result
      connected OF PG:CLOSE true ENDOF
      rejected OF drop false ENDOF
   ;MATCH ;


: PW-CONNINFO+ ( -- )
   SB-RESET CONNINFO$ SB-APPEND s"  user=habu_pw" SB-APPEND ;


: PW-CONNINFO$ ( -- str )
   PW-CONNINFO+ SB$ ;


: PW-HELD$ ( -- str )
   PW-CONNINFO+ s"  password=" SB-APPEND PW-WRONG$ SB-APPEND SB$ ;


\ The statement holds the whole password, past the string builder's room.
: ROLE-SQL ( ^VEC:vector<u8> -- ) {: v :}
   s" create role habu_pw login password '" v VEC:APPEND
   PW-RIGHT$ v VEC:APPEND
   s" '" v VEC:APPEND ;


: CREATE-ROLE ( PG:connection -- ) {: c :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v :}
   v ROLE-SQL
   [: c v VEC:BYTES EXEC-OK ;] [: v VEC:DELETE ;] finally ;


: PASSWORD-CASES ( PG:connection -- PG:connection ) {: c :}
   c CREATE-ROLE
   s" a password argument signs in to a role that needs one" T-LABEL
   PW-CONNINFO$ PW-RIGHT$ CONNECT SIGNED-IN? TTRUE
   s" a password argument overrides the one the conninfo holds" T-LABEL
   PW-HELD$ PW-RIGHT$ CONNECT SIGNED-IN? TTRUE
   s" a wrong password argument is rejected and its text omits it" T-LABEL
   PW-CONNINFO$ PW-WRONG$ CONNECT REJECTED$
   PW-WRONG$ STR:CONTAINS? TFALSE
   c ;


: SERVER-CASES ( -- )
   OPEN
   DDL-CASES
   PASSWORD-CASES
   PARAM-CASES
   TEXT-BOUNDARY-CASES
   BINARY-CASES
   F64-CASES
   BINARY-RESULT-CASES
   HELD-SPAN-CASES
   LONG-MESSAGE-CASES
   NESTED-SPAN-CASES
   REPORT-CASES
   PREPARED-CASES
   DESCRIBE-CASES
   ORIGIN-CASES
   ROLLBACK-CASES
   COMMIT-CASES
   ANSWER-CASES
   SOFT-FAIL-CASES
   OWN-END-CASES
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
   SCRIPT-REJECTION-CASES
   COPY-CASES
   SCOPE-CASES
   OWNER-CASES
   EXIT-CASES
   CANCEL-CASES
   UNREAD-CASES
   TX-HALT-CASES
   LOST-CONNECTION-CASES
   DEADLINE-CASES
   NOTIFY-CASES
   NOTIFIED-CASES
   ENDED-CASES
   NOTICE-CASES
   ABANDON-CASES
   RECYCLE-CASES
   PG:CLOSE
   CLOSE-CLEARS-CASES
   REUSE-CASES ;


: LOOP-CASES ( -- )
   CONNECT-CASES REJECTION-TEXT-CASES SOCKET-REJECTION-CASES MALFORMED-CASES
   CONNECT-TEXT-CASES CAPACITY-CASES IDENTITY-CASES
   HALT-CONNECT-CASES FAILED-SEND-CASES AIO-FAIL-CASES SERVER-CASES ;

\ AIO:STOP runs whatever the cases threw; a code it throws replaces theirs.
: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" pg-test: no conninfo argument; test/db/pg-cluster.f runs this file" EXIT-STATUS:USAGE die
   then
   CONNECTIONS RESULTS CONFIGURE
   PROGRESS-CASES
   RESULT-CAPACITY-CASES
   AIO:START
   [: LOOP-CASES ;] catch {: code:n :}
   AIO:STOP
   code 0<> if code throw then ;

T-RESET
MAIN
T-REPORT

;using
;package
