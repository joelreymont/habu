\ tcp4-test.f - a loopback TCP connection carries bytes both ways.
\
\ One process holds both peers: a listener task blocks in ACCEPT while the main
\ task connects, writes and reads back. Every timed wait runs on the AIO loop,
\ which this suite starts before its first case and stops after its last.
\ Run: bin/hb --load lib/net/tcp4-test.f
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require lib/ffi-abi.f
require lib/task.f
require lib/fs-list.f              \ the /proc/self/task entries the parked case counts
require lib/aio.f                  \ the loop every readiness wait runs on
require lib/net/tcp4.f

package TCP4-TEST

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
4 constant BACKLOG
$09 constant EBADF                 \ a transfer through a closed descriptor
$6F constant ECONNREFUSED          \ nobody listens on that port
$20 constant BUF-CAP
$40 constant POLL-TRIES

1000000 constant NS-PER-MS
\ The deadlines the timed waits are measured against. The lower bounds allow a
\ scheduler's slack while still failing a wait that did not happen at all.
500 constant WAIT-MS               \ far beyond the peer's delay: ready, not idle
100 constant IDLE-MS               \ what a silent peer and a quiet listener reach
50 constant PEER-MS                \ the peer's delay before it writes
40 constant LEAST-WAIT-MS          \ a ready answer still covered the peer's delay
90 constant LEAST-IDLE-MS          \ an idle answer waited out its deadline
20 constant SETTLE-MS              \ the armed task reaches its submission within this

create CLIENT-BUF BUF-CAP allot
create SERVER-BUF BUF-CAP allot

: TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

TEST-ALIGN8
variable ECHO-DONE
variable ECHO-BAD
variable ECHO-PEER
variable ECHO-GOT
variable ECHO-ERRNO
variable ECHO-LISTENER
variable PROBE-LISTENER
variable PROBE-CONNECTION
variable PROBE-PORT
variable CLIENT-FD
variable PEER-FD
variable THREAD-N
variable BASE-THREADS
variable PARKED-THREADS
variable PARK-ARMED
variable PARK-DONE
variable PARK-READY
variable PARK-FD

TASK:MIN-STACK TASK:TASK ECHO-TASK
TASK:MIN-STACK TASK:TASK PEER-TASK
TASK:MIN-STACK TASK:TASK PARK-TASK

: PING$ ( -- ptr u8 n )
   s" ping" ;

: CHAT$ ( -- ptr u8 n )
   s" hey" ;

\ Spans are measured from the messages themselves, so a reworded request cannot
\ leave a stale length behind for READ-EXACT to wait on.
: PING-BYTES ( -- n )
   PING$ swap drop ;

: CHAT-BYTES ( -- n )
   CHAT$ swap drop ;


\ ---- result inspectors -------------------------------------------------------

: STATUS-ERRNO ( TCP4:status -- n )
   MATCH TCP4:status
      ok OF 0 ENDOF
      failed OF TCP4:ERRNO>N ENDOF
   ;MATCH ;


: READY? ( TCP4:ready-result -- bool )
   MATCH TCP4:ready-result
      ready OF true ENDOF
      idle OF false ENDOF
      failed OF TCP4:ERRNO>N drop s" poll failed" T-FAIL-AS false ENDOF
   ;MATCH ;


: READY-DROP ( TCP4:ready-result -- )
   MATCH TCP4:ready-result
      ready OF ENDOF
      idle OF ENDOF
      failed OF TCP4:ERRNO>N drop ENDOF
   ;MATCH ;


: READ-DROP ( TCP4:read-result -- )
   MATCH TCP4:read-result
      data OF drop ENDOF
      closed OF drop ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: WANT-DATA ( TCP4:read-result n -- ) {: want:n :}
   MATCH TCP4:read-result
      data OF BLEN>N want T= ENDOF
      closed OF drop s" read: end of stream" T-FAIL-AS ENDOF
      failed OF TCP4:ERRNO>N drop s" read: failed" T-FAIL-AS ENDOF
   ;MATCH ;


: WANT-CLOSED ( TCP4:read-result -- )
   MATCH TCP4:read-result
      data OF drop s" read: unexpected data" T-FAIL-AS ENDOF
      closed OF BLEN>N 0 T= ENDOF
      failed OF TCP4:ERRNO>N drop s" read: failed" T-FAIL-AS ENDOF
   ;MATCH ;


: WANT-FAILED ( TCP4:read-result n -- ) {: want:n :}
   MATCH TCP4:read-result
      data OF drop s" read: unexpected data" T-FAIL-AS ENDOF
      closed OF drop s" read: unexpected end of stream" T-FAIL-AS ENDOF
      failed OF TCP4:ERRNO>N want T= ENDOF
   ;MATCH ;


: ACCEPT-FD ( TCP4:accept-result -- n )
   MATCH TCP4:accept-result
      accepted OF TCP4:PORT>N drop TCP4:ADDRESS>N drop TCP4:CONNECTION>N ENDOF
      failed OF TCP4:ERRNO>N drop s" accept failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


\ ---- the listener task -------------------------------------------------------

: ECHO-FAILED ( TCP4:errno -- )
   TCP4:ERRNO>N ECHO-ERRNO !
   1 ECHO-BAD +! ;


: ECHO-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF ECHO-FAILED ENDOF
   ;MATCH ;


\ The request is read whole, written back, and the stream half-closed so the
\ client's next read is the end of stream rather than a wait.
: ECHO-SERVE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn SERVER-BUF PING-BYTES TCP4:TRANSFER-BYTES TCP4:READ-EXACT
   MATCH TCP4:read-result
      data OF BLEN>N ECHO-GOT ! ENDOF
      closed OF drop 1 ECHO-BAD +! ENDOF
      failed OF ECHO-FAILED ENDOF
   ;MATCH
   conn SERVER-BUF ECHO-GOT @ TCP4:TRANSFER-BYTES TCP4:WRITE ECHO-STATUS
   conn TCP4:SENDING TCP4:SHUTDOWN ECHO-STATUS
   conn TCP4:CLOSE ECHO-STATUS ;


: ECHO-ACCEPTED ( TCP4:connection TCP4:address TCP4:port -- )
   {: conn:TCP4:connection peer:TCP4:address peer-port:TCP4:port :}
   peer TCP4:ADDRESS>N ECHO-PEER !
   peer-port TCP4:PORT>N 0= if 1 ECHO-BAD +! then
   conn ECHO-SERVE ;


: ECHO-WORK ( -- )
   ECHO-LISTENER @ TCP4:>LISTENER TCP4:ACCEPT
   MATCH TCP4:accept-result
      accepted OF ECHO-ACCEPTED ENDOF
      failed OF ECHO-FAILED ENDOF
   ;MATCH
   1 ECHO-DONE atomic-add drop ;


: ECHO-RESET ( -- )
   0 ECHO-DONE ! 0 ECHO-BAD ! 0 ECHO-PEER ! 0 ECHO-GOT ! 0 ECHO-ERRNO ! ;


: WAIT-DONE ( -- )
   begin ECHO-DONE atomic@ 0 > if exit then TASK:PAUSE again ;


\ ---- the delayed peer --------------------------------------------------------

\ Nothing reaches the reader before PEER-MS, so a ready answer sooner than that
\ never waited in poll(2). The peer sleeps out its delay rather than yielding
\ against a deadline: a spinning peer would take a core away from the reader it
\ is timing.
: PEER-WORK ( -- )
   PEER-MS >MS TASK:SLEEP
   PEER-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:WRITE ECHO-STATUS
   1 ECHO-DONE atomic-add drop ;


: ELAPSED-MS ( n -- n ) {: start:n :}
   mono-ns start - NS-PER-MS / ;


\ ---- the parked waiter -------------------------------------------------------

: THREAD-TALLY ( ptr u8 n -- )
   2drop 1 THREAD-N atomic-add drop ;


\ The live threads of this process, counted from its own task directory, the way
\ lib/aio-test.f counts them for the fan-out.
: THREADS ( -- n )
   0 THREAD-N !
   s" /proc/self/task" [: THREAD-TALLY ;] FS-LIST:EACH
   THREAD-N @ ;


: REACHED? ( ptr n n -- bool ) {: cell:ptr want:n :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      cell atomic@ want >= if true exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


\ One number per answer, because the assertions belong to the main thread.
: PARK-ANSWER ( TCP4:ready-result -- n )
   MATCH TCP4:ready-result
      ready OF 1 ENDOF
      idle OF 0 ENDOF
      failed OF TCP4:ERRNO>N negate ENDOF
   ;MATCH ;


\ Arms, then parks in the wait until the main thread's write reaches it.
: PARK-WORK ( -- )
   1 PARK-ARMED atomic-add drop
   PARK-FD @ TCP4:>CONNECTION WAIT-MS >MS TCP4:READABLE-WITHIN? PARK-ANSWER PARK-READY !
   1 PARK-DONE atomic-add drop ;


\ ---- endpoints the cases share -----------------------------------------------

: BIND-LISTENER ( -- n )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N ENDOF
      failed OF TCP4:ERRNO>N drop s" bind failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


: NEW-LISTENER ( -- n )
   BIND-LISTENER dup 0 >= TTRUE
   dup 0 >= if
      dup TCP4:>LISTENER BACKLOG TCP4:LISTEN STATUS-ERRNO 0 T=
   then ;


: LISTENER-PORT ( n -- n )
   TCP4:>LISTENER TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N swap TCP4:ADDRESS>N drop ENDOF
      failed OF TCP4:ERRNO>N drop s" local failed" T-FAIL-AS 0 ENDOF
   ;MATCH ;


: CONNECT-TO ( n -- n )
   LOOPBACK TCP4:ADDRESS swap TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CONNECTION>N ENDOF
      failed OF TCP4:ERRNO>N drop s" connect failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


: WAIT-PENDING ( n -- bool ) {: fd:n :}
   POLL-TRIES 0 do
      fd TCP4:>LISTENER TCP4:PENDING? READY? if unloop true exit then
      TASK:PAUSE
   loop false ;


: WAIT-READABLE ( n -- bool ) {: fd:n :}
   POLL-TRIES 0 do
      fd TCP4:>CONNECTION TCP4:READABLE? READY? if unloop true exit then
      TASK:PAUSE
   loop false ;


\ A loopback pair the timed cases share: the listener plus both ends of one
\ connection. The accept queue is reached through the timed listener wait, so
\ nothing here spins on the scheduler.
: OPEN-PAIR ( -- )
   NEW-LISTENER PROBE-LISTENER !
   PROBE-LISTENER @ LISTENER-PORT CONNECT-TO dup 0 >= TTRUE CLIENT-FD !
   PROBE-LISTENER @ TCP4:>LISTENER WAIT-MS >MS TCP4:PENDING-WITHIN? READY? TTRUE
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:ACCEPT ACCEPT-FD dup 0 >= TTRUE
   PROBE-CONNECTION ! ;


: CLOSE-PAIR ( -- )
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


\ ---- cases -------------------------------------------------------------------

: BAD-PORT ( -- )
   $10000 TCP4:PORT drop ;


: BAD-ADDRESS ( -- )
   -1 TCP4:ADDRESS drop ;


: BAD-TRANSFER ( -- )
   -1 TCP4:TRANSFER-BYTES drop ;


: BAD-BACKLOG ( -- )
   PROBE-LISTENER @ TCP4:>LISTENER 0 TCP4:LISTEN STATUS-ERRNO drop ;


: BAD-CAPACITY ( -- )
   CLIENT-FD @ TCP4:>CONNECTION CLIENT-BUF 0 TCP4:TRANSFER-BYTES TCP4:READ READ-DROP ;


: BAD-TIMEOUT ( -- )
   CLIENT-FD @ TCP4:>CONNECTION -1 >MS TCP4:READABLE-WITHIN? READY-DROP ;


: BAD-LISTEN-TIMEOUT ( -- )
   PROBE-LISTENER @ TCP4:>LISTENER -1 >MS TCP4:PENDING-WITHIN? READY-DROP ;


\ A deadline wider than the range this module declares is refused, not clamped.
: BAD-LONG-TIMEOUT ( -- )
   CLIENT-FD @ TCP4:>CONNECTION $80000000 >MS TCP4:READABLE-WITHIN? READY-DROP ;


\ Operands are checked before the platform and before any descriptor is touched,
\ so these need no socket.
: T-OPERANDS ( -- )
   s" an out-of-range operand throws before any socket call" T-LABEL
   [: BAD-PORT ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-ADDRESS ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-TRANSFER ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-BACKLOG ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-CAPACITY ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-TIMEOUT ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-LISTEN-TIMEOUT ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-LONG-TIMEOUT ;] TCP4:E-OPERAND TTHROWSQ ;


\ The waits do not park threads: one task waiting on a silent connection costs
\ its own thread and the loop's, and nothing per wait. This case runs before any
\ other task of the suite exists, so the count it pins is the base plus those two.
: T-PARKED-THREADS ( -- )
   s" a task parked in a timed wait costs no thread beyond the loop's" T-LABEL
   0 PARK-ARMED ! 0 PARK-DONE ! 0 PARK-READY !
   OPEN-PAIR
   PROBE-CONNECTION @ PARK-FD !
   ['] PARK-WORK PARK-TASK TASK:ACTIVATE
   PARK-ARMED 1 REACHED? TTRUE
   SETTLE-MS >MS TASK:SLEEP
   THREADS PARKED-THREADS !
   PARKED-THREADS @ BASE-THREADS @ 1 + 1 + T=
   s" and the peer's write answers that parked wait" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:WRITE STATUS-ERRNO 0 T=
   PARK-DONE 1 REACHED? TTRUE
   PARK-READY @ 1 T=
   PARK-TASK TASK:KILL
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      CHAT-BYTES WANT-DATA
   SERVER-BUF CHAT-BYTES CHAT$ T$=
   CLOSE-PAIR ;


: STOPPED-WAIT ( -- )
   PROBE-CONNECTION @ TCP4:>CONNECTION IDLE-MS >MS TCP4:READABLE-WITHIN? READY-DROP ;


\ The loop is the program's to start, and a wait without one says so by name
\ instead of falling back to a thread parked in poll(2).
: T-NO-LOOP ( -- )
   s" a wait with the loop stopped is refused by name" T-LABEL
   OPEN-PAIR
   AIO:LOOP-STOP
   [: STOPPED-WAIT ;] E-AIO-STATE TTHROWSQ
   AIO:LOOP-START
   CLOSE-PAIR ;


: ECHO-EXCHANGE ( -- )
   CLIENT-FD @ TCP4:>CONNECTION PING$ TCP4:TRANSFER-BYTES TCP4:WRITE STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION CLIENT-BUF PING-BYTES TCP4:TRANSFER-BYTES TCP4:READ-EXACT
      PING-BYTES WANT-DATA
   CLIENT-BUF PING-BYTES PING$ T$=
   s" the server's half-close is the client's end of stream" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION CLIENT-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ WANT-CLOSED
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T= ;


: ECHO-CHECK ( -- )
   s" the server task saw one loopback peer and the whole request" T-LABEL
   ECHO-BAD @ 0 T=
   ECHO-ERRNO @ 0 T=
   ECHO-GOT @ PING-BYTES T=
   ECHO-PEER @ LOOPBACK T= ;


: T-ECHO ( -- )
   s" a listener task accepts a loopback connection and echoes it" T-LABEL
   ECHO-RESET
   NEW-LISTENER ECHO-LISTENER !
   ECHO-LISTENER @ LISTENER-PORT dup 0 > TTRUE
   ['] ECHO-WORK ECHO-TASK TASK:ACTIVATE
   CONNECT-TO dup 0 >= TTRUE CLIENT-FD !
   ECHO-EXCHANGE
   WAIT-DONE
   ECHO-TASK TASK:KILL
   ECHO-CHECK
   ECHO-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


: T-READINESS ( -- )
   s" an idle listener has nothing pending and an idle stream nothing to read" T-LABEL
   NEW-LISTENER PROBE-LISTENER !
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:PENDING? READY? TFALSE
   PROBE-LISTENER @ LISTENER-PORT CONNECT-TO dup 0 >= TTRUE CLIENT-FD !
   s" a waiting connection is pending, and a sent request readable" T-LABEL
   PROBE-LISTENER @ WAIT-PENDING TTRUE
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:ACCEPT ACCEPT-FD dup 0 >= TTRUE PROBE-CONNECTION !
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:READABLE? READY? TFALSE
   CLIENT-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:WRITE STATUS-ERRNO 0 T=
   PROBE-CONNECTION @ WAIT-READABLE TTRUE
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      CHAT-BYTES WANT-DATA
   SERVER-BUF CHAT-BYTES CHAT$ T$=
   s" a live stream half-closes in either direction and in both" T-LABEL
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:BOTH TCP4:SHUTDOWN STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION TCP4:RECEIVING TCP4:SHUTDOWN STATUS-ERRNO 0 T=
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


: T-WAIT-DATA ( -- )
   s" a peer writing after 50 ms is ready within 500 ms" T-LABEL
   ECHO-RESET
   OPEN-PAIR
   CLIENT-FD @ PEER-FD !
   ['] PEER-WORK PEER-TASK TASK:ACTIVATE
   mono-ns {: start:n :}
   PROBE-CONNECTION @ TCP4:>CONNECTION WAIT-MS >MS TCP4:READABLE-WITHIN? READY? TTRUE
   s" and that wait covered the peer's delay" T-LABEL
   start ELAPSED-MS LEAST-WAIT-MS >= TTRUE
   WAIT-DONE
   PEER-TASK TASK:KILL
   ECHO-BAD @ 0 T=
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      CHAT-BYTES WANT-DATA
   SERVER-BUF CHAT-BYTES CHAT$ T$=
   CLOSE-PAIR ;


: T-WAIT-IDLE ( -- )
   s" a silent peer answers idle after 100 ms" T-LABEL
   OPEN-PAIR
   mono-ns {: start:n :}
   PROBE-CONNECTION @ TCP4:>CONNECTION IDLE-MS >MS TCP4:READABLE-WITHIN? READY? TFALSE
   s" and that wait reached its deadline" T-LABEL
   start ELAPSED-MS LEAST-IDLE-MS >= TTRUE
   CLOSE-PAIR ;


\ The peer's close is readable at once; the READ that follows says which
\ readiness it was.
: T-WAIT-CLOSED ( -- )
   s" a closed peer is ready, and reads as the end of stream" T-LABEL
   OPEN-PAIR
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   PROBE-CONNECTION @ TCP4:>CONNECTION WAIT-MS >MS TCP4:READABLE-WITHIN? READY? TTRUE
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      WANT-CLOSED
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


: T-WAIT-LISTENER ( -- )
   s" a quiet listener answers idle after 100 ms" T-LABEL
   NEW-LISTENER PROBE-LISTENER !
   mono-ns {: start:n :}
   PROBE-LISTENER @ TCP4:>LISTENER IDLE-MS >MS TCP4:PENDING-WITHIN? READY? TFALSE
   s" and that wait reached its deadline" T-LABEL
   start ELAPSED-MS LEAST-IDLE-MS >= TTRUE
   s" a pending connect makes the same listener ready" T-LABEL
   PROBE-LISTENER @ LISTENER-PORT CONNECT-TO dup 0 >= TTRUE CLIENT-FD !
   PROBE-LISTENER @ TCP4:>LISTENER WAIT-MS >MS TCP4:PENDING-WITHIN? READY? TTRUE
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:ACCEPT ACCEPT-FD dup 0 >= TTRUE PROBE-CONNECTION !
   CLOSE-PAIR ;


: T-REFUSED ( -- )
   s" connecting where nothing listens fails with ECONNREFUSED" T-LABEL
   NEW-LISTENER PROBE-LISTENER !
   PROBE-LISTENER @ LISTENER-PORT PROBE-PORT !
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T=
   LOOPBACK TCP4:ADDRESS PROBE-PORT @ TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CLOSE STATUS-ERRNO drop s" connect: unexpectedly opened" T-FAIL-AS ENDOF
      failed OF TCP4:ERRNO>N ECONNREFUSED T= ENDOF
   ;MATCH ;


\ No socket is opened between the close and the read, so the descriptor number
\ cannot have been handed to another socket in between.
: T-READ-AFTER-CLOSE ( -- )
   s" a read through a closed connection fails with EBADF" T-LABEL
   NEW-LISTENER PROBE-LISTENER !
   PROBE-LISTENER @ LISTENER-PORT CONNECT-TO dup 0 >= TTRUE CLIENT-FD !
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION CLIENT-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      EBADF WANT-FAILED
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


: RUN ( -- )
   T-RESET
   THREADS BASE-THREADS !
   AIO:LOOP-START
   T-OPERANDS
   T-PARKED-THREADS
   T-ECHO
   T-READINESS
   T-WAIT-DATA
   T-WAIT-IDLE
   T-WAIT-CLOSED
   T-WAIT-LISTENER
   T-REFUSED
   T-READ-AFTER-CLOSE
   T-NO-LOOP
   AIO:LOOP-STOP
   T-REPORT
   s" tcp4-test: ok" type cr ;

RUN

;package
