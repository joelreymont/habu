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
require lib/memory.f               \ the write cases' 16 MiB source
require lib/fmt.f                  \ the sipped slices' durations, on one line
require lib/task.f
require test/host-threads.f
require lib/aio.f                  \ the loop every readiness wait runs on
require lib/net/tcp4.f

package TCP4-TEST

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
4 constant BACKLOG
$09 constant EBADF                 \ a transfer through a closed descriptor
$20 constant EPIPE                 \ a send after the peer's reset, on both
: ECONNRESET ( -- n ) HB-TARGET-MACOS? if 54 else $68 then ;
: ECONNREFUSED ( -- n ) HB-TARGET-MACOS? if 61 else $6F then ;
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

\ The write cases. The flood outgrows what loopback's send and receive buffers
\ hold together, so a peer that never reads must leave part of it unsent.
$1000000 constant FLOOD-BYTES      \ 16 MiB
5000 constant DRAIN-MS             \ slack for the reading task, never reached when green
300 constant FILL-MS               \ the writer fills both buffers and blocks within this
1000 constant HALT-SLACK-MS        \ a halted writer or any slice ends within one slice and this
90 constant LEAST-SLICE-MS         \ an empty slice waited out its wait for room
251 constant PATTERN-MOD           \ byte i of the flood is i mod 251
$4000 constant DRAIN-CAP
10 constant SIP-MS                 \ the sipping reader takes DRAIN-CAP bytes this often
8 constant SIP-SLICES              \ the slices timed against the sipping reader
$100000 constant LEAST-SPAN        \ 1 MiB: the shortest span a timed slice is handed

create CLIENT-BUF BUF-CAP allot
create SERVER-BUF BUF-CAP allot
create DRAIN-BUF DRAIN-CAP allot

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
variable BASE-THREADS
variable PARKED-THREADS
variable PARK-ARMED
variable PARK-DONE
variable PARK-READY
variable PARK-FD
variable DRAIN-DONE
variable DRAIN-GOT
variable DRAIN-WRONG
variable DRAIN-ENDED
variable WRITER-ARMED
variable WRITER-ENDED
variable SIPPED
variable SIP-STOP
PTR-VARIABLE FLOOD-PTR

TASK:MIN-STACK TASK:TASK ECHO-TASK
TASK:MIN-STACK TASK:TASK PEER-TASK
TASK:MIN-STACK TASK:TASK PARK-TASK
TASK:MIN-STACK TASK:TASK DRAIN-TASK
TASK:MIN-STACK TASK:TASK WRITER-TASK

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


\ The bytes a send slice moved; a slice expected to move some that did not
\ fails the case.
: MOVED-BYTES ( TCP4:slice-result -- n )
   MATCH TCP4:slice-result
      moved OF BLEN>N ENDOF
      empty OF s" slice: unexpectedly empty" T-FAIL-AS 0 ENDOF
      failed OF TCP4:ERRNO>N drop s" slice: failed" T-FAIL-AS 0 ENDOF
   ;MATCH ;


\ The bytes a queue count found; a count the OS refused fails the case and
\ answers -1.
: QUEUED-BYTES ( TCP4:queue-result -- n )
   MATCH TCP4:queue-result
      queued OF BLEN>N ENDOF
      failed OF TCP4:ERRNO>N drop s" queue count failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


: QUEUE-ERRNO ( TCP4:queue-result -- n )
   MATCH TCP4:queue-result
      queued OF BLEN>N drop 0 ENDOF
      failed OF TCP4:ERRNO>N ENDOF
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


\ Writes the chat after PEER-MS and half-closes PEER-MS after that, so each READ
\ of the other end has something to wait for.
: LATE-WORK ( -- )
   PEER-WORK
   PEER-MS >MS TASK:SLEEP
   PEER-FD @ TCP4:>CONNECTION TCP4:SENDING TCP4:SHUTDOWN ECHO-STATUS ;


: ELAPSED-MS ( n -- n ) {: start:n :}
   mono-ns start - NS-PER-MS / ;


\ ---- the parked waiter -------------------------------------------------------

: THREADS ( -- n ) TEST-HOST:THREADS ;

: REACHED? ( ptr n n n -- bool ) {: cell:ptr want:n limit:n :}
   mono-ns limit NS-PER-MS * + {: deadline:n :}
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


\ ---- the draining reader -----------------------------------------------------

\ Counts each byte that is not the flood's byte at its place in the stream.
: DRAIN-CHECK ( n -- ) {: got:n :}
   got 0 ?do
      DRAIN-BUF i + c@ DRAIN-GOT @ i + PATTERN-MOD mod <> if 1 DRAIN-WRONG +! then
   loop
   got DRAIN-GOT +! ;


: DRAIN-STEP ( -- )
   PROBE-CONNECTION @ TCP4:>CONNECTION DRAIN-BUF DRAIN-CAP TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N DRAIN-CHECK ENDOF
      closed OF drop 1 DRAIN-ENDED ! ENDOF
      failed OF TCP4:ERRNO>N drop 1 DRAIN-ENDED ! ENDOF
   ;MATCH ;


\ Reads until the stream ends, checking every byte.
: DRAIN-WORK ( -- )
   begin DRAIN-ENDED @ 0= while DRAIN-STEP repeat
   1 DRAIN-DONE atomic-add drop ;


\ ---- the sipping reader -------------------------------------------------------

\ The chat back for every read, sent at once (NODELAY): the acknowledgement of
\ what the read took rides on it instead of waiting out a delayed ACK, so the
\ room each read makes reaches the writer within SIP-MS. True once it is sent.
: SIP-ANSWER ( -- bool )
   PROBE-CONNECTION @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:WRITE
   STATUS-ERRNO 0= ;


\ One read of at most DRAIN-CAP bytes, counted in SIPPED and answered: false
\ once the stream has ended or failed.
: SIP-STEP ( -- bool )
   PROBE-CONNECTION @ TCP4:>CONNECTION DRAIN-BUF DRAIN-CAP TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N SIPPED atomic-add drop SIP-ANSWER ENDOF
      closed OF drop false ENDOF
      failed OF TCP4:ERRNO>N drop false ENDOF
   ;MATCH ;


: SIPPING? ( -- bool )
   SIP-STOP atomic@ 0<> if false exit then
   SIP-STEP ;


\ Takes DRAIN-CAP bytes every SIP-MS, far closer together than a send slice
\ lasts, until SIP-STOP is set or the stream ends.
: SIP-WORK ( -- )
   begin SIPPING? while SIP-MS >MS TASK:SLEEP repeat
   1 DRAIN-DONE atomic-add drop ;


: FILL-PATTERN ( ptr u8 n -- ) {: bytes size:n :}
   size 0 ?do i PATTERN-MOD mod bytes i + c! loop ;


\ The 16 MiB source the write and slice cases send from, byte i holding i mod 251.
: FLOOD ( -- ptr u8 )
   FLOOD-PTR @ ;


: FLOOD-OPEN ( -- )
   FLOOD-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: bytes :}
   bytes FLOOD-BYTES FILL-PATTERN
   bytes FLOOD-PTR ! ;


: FLOOD-CLOSE ( -- )
   FLOOD FLOOD-BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;


\ ---- the blocked writer ------------------------------------------------------

\ Blocks in WRITE, because the peer never reads; WRITER-ENDED says it returned.
: WRITER-WORK ( -- )
   1 WRITER-ARMED atomic-add drop
   CLIENT-FD @ TCP4:>CONNECTION FLOOD FLOOD-BYTES TCP4:TRANSFER-BYTES TCP4:WRITE
   STATUS-ERRNO drop
   1 WRITER-ENDED atomic-add drop ;


: DONE-WITHIN? ( n -- bool ) {: limit:n :}
   mono-ns limit NS-PER-MS * + {: deadline:n :}
   begin
      WRITER-TASK TASK:DONE? if true exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


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


: PROBE-UNREAD ( -- n )
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:UNREAD QUEUED-BYTES ;


: CLIENT-UNSENT ( -- n )
   CLIENT-FD @ TCP4:>CONNECTION TCP4:UNSENT QUEUED-BYTES ;


\ The bytes the shared pair's two queues hold between them, from client to
\ peer: unsent or unacknowledged, and unread; -1 when either count failed.
: PIPE-HELD ( -- n )
   CLIENT-UNSENT PROBE-UNREAD {: unsent:n unread:n :}
   unsent 0 < unread 0 < or if -1 exit then
   unsent unread + ;


\ True once the count answers the bytes wanted; false once it fails, or once
\ WAIT-MS has passed: loopback delivers and acknowledges within it.
: COUNTS? ( [ -- n ] n -- bool ) {: count want:n :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      count execute {: got:n :}
      got want = if true exit then
      got 0 < if false exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


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
   PARK-ARMED 1 WAIT-MS REACHED? TTRUE
   SETTLE-MS >MS TASK:SLEEP
   THREADS PARKED-THREADS !
   PARKED-THREADS @ BASE-THREADS @ 1 + 1 + T=
   s" and the peer's write answers that parked wait" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:WRITE STATUS-ERRNO 0 T=
   PARK-DONE 1 WAIT-MS REACHED? TTRUE
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
   AIO:STOP
   [: STOPPED-WAIT ;] E-AIO-STATE TTHROWSQ
   AIO:START
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
   \ Shut the live receive half before the peer's FIN can close it. Darwin
   \ correctly returns ENOTCONN for SHUT_RD after that FIN has arrived.
   CLIENT-FD @ TCP4:>CONNECTION TCP4:RECEIVING TCP4:SHUTDOWN STATUS-ERRNO 0 T=
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:BOTH TCP4:SHUTDOWN STATUS-ERRNO 0 T=
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


\ Every connection this module makes is non-blocking, and READ still blocks:
\ the accepted end waits out the late peer's delay for the chat, then for the
\ end of stream its half-close sends.
: T-READ-WAITS ( -- )
   s" a read waits for a peer that writes after 50 ms" T-LABEL
   ECHO-RESET
   OPEN-PAIR
   CLIENT-FD @ PEER-FD !
   ['] LATE-WORK PEER-TASK TASK:ACTIVATE
   mono-ns {: start:n :}
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      CHAT-BYTES WANT-DATA
   start ELAPSED-MS LEAST-WAIT-MS >= TTRUE
   SERVER-BUF CHAT-BYTES CHAT$ T$=
   s" and then for the end of stream the peer's half-close sends" T-LABEL
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF BUF-CAP TCP4:TRANSFER-BYTES TCP4:READ
      WANT-CLOSED
   WAIT-DONE
   PEER-TASK TASK:KILL
   ECHO-BAD @ 0 T=
   CLOSE-PAIR ;


\ WRITE still blocks until every byte is accepted, but in slices of
\ SEND-SLICE-MS with a TASK:PAUSE between them, so a halt ends a writer whose
\ peer stopped reading. Closing the reading end first releases a writer that
\ never saw the halt, so a regression fails here instead of hanging.
: T-WRITE-HALTED ( -- )
   s" a task blocked in WRITE to a peer that never reads is still writing" T-LABEL
   0 WRITER-ARMED ! 0 WRITER-ENDED !
   OPEN-PAIR
   ['] WRITER-WORK WRITER-TASK TASK:ACTIVATE
   WRITER-ARMED 1 WAIT-MS REACHED? TTRUE
   FILL-MS >MS TASK:SLEEP
   WRITER-TASK TASK:DONE? TFALSE
   s" and ends within one send slice and a second of its halt" T-LABEL
   WRITER-TASK TASK:HALT
   TCP4:SEND-SLICE-MS HALT-SLACK-MS + DONE-WITHIN? TTRUE
   WRITER-ENDED @ 0 T=
   CLOSE-PAIR
   WRITER-TASK TASK:KILL ;


\ A slice of the flood to a peer that reads it as it comes moves part of the
\ span or all of it, and exactly that many bytes arrive, in order.
: T-SLICE-MOVED ( -- )
   s" a send slice to a reading peer moves at least one byte and at most the span" T-LABEL
   OPEN-PAIR
   0 DRAIN-DONE ! 0 DRAIN-GOT ! 0 DRAIN-WRONG ! 0 DRAIN-ENDED !
   ['] DRAIN-WORK DRAIN-TASK TASK:ACTIVATE
   CLIENT-FD @ TCP4:>CONNECTION FLOOD FLOOD-BYTES TCP4:TRANSFER-BYTES TCP4:SEND-SLICE
      MOVED-BYTES {: moved:n :}
   moved 0 > TTRUE
   moved FLOOD-BYTES <= TTRUE
   s" and exactly the bytes it moved arrive, in order" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION TCP4:SENDING TCP4:SHUTDOWN STATUS-ERRNO 0 T=
   DRAIN-DONE 1 DRAIN-MS REACHED? TTRUE
   DRAIN-TASK TASK:KILL
   DRAIN-GOT @ moved T=
   DRAIN-WRONG @ 0 T=
   CLOSE-PAIR ;


\ Slices of the flood to a peer that never reads until one answers empty: the
\ bytes they moved, and how long that one took, in ms, or -1 when none did
\ within POLL-TRIES slices.
: EMPTY-SLICE-MS ( -- n n )
   0 0                                   \ the bytes sent, and the slices tried
   begin {: sent:n tries:n :}
      tries POLL-TRIES >= if sent -1 exit then
      mono-ns {: start:n :}
      CLIENT-FD @ TCP4:>CONNECTION FLOOD sent + FLOOD-BYTES sent - TCP4:TRANSFER-BYTES
         TCP4:SEND-SLICE
      MATCH TCP4:slice-result
         moved OF BLEN>N sent + ENDOF
         empty OF sent start ELAPSED-MS exit ENDOF
         failed OF TCP4:ERRNO>N drop s" slice: failed" T-FAIL-AS sent -1 exit ENDOF
      ;MATCH
      tries 1+
   again ;


\ The first slices fill both loopback buffers; then a slice waits SEND-SLICE-MS
\ for room, finds none and answers empty, rather than blocking for as long as
\ the peer stays silent. The full pipe holds every byte the slices moved,
\ part still the writer's to send.
: T-SLICE-EMPTY ( -- )
   s" a send slice to a full peer that never reads answers empty" T-LABEL
   OPEN-PAIR
   EMPTY-SLICE-MS {: sent:n took:n :}
   took 0 >= TTRUE
   s" once it has waited out its wait for room, and within it and the slack" T-LABEL
   took LEAST-SLICE-MS >= TTRUE
   took TCP4:SEND-SLICE-MS HALT-SLACK-MS + < TTRUE
   s" the writer's UNSENT and the peer's UNREAD hold every byte moved between them" T-LABEL
   [: PIPE-HELD ;] sent COUNTS? TTRUE
   CLOSE-PAIR ;


\ The bound every slice is held to: one send slice and the scheduler's slack. The
\ two sends a slice makes never wait and copy a buffer's worth at most, so the
\ slack is how late a loaded host wakes the slice's wait for room.
: SLICE-BOUND-MS ( -- n )
   TCP4:SEND-SLICE-MS HALT-SLACK-MS + ;


\ Slices of the flood to the sipping reader, each handed the rest of the flood
\ and so at least LEAST-SPAN: the longest one's ms, once every one's is printed.
\ It stops after SIP-SLICES, or at the first slice past the bound, so a slice
\ that runs on for as long as the reader makes room is timed once.
: SIPPED-SLICE-MS ( -- n )
   s" sipped slices, ms:" type
   0 0 0                                 \ the bytes sent, the slices timed, the longest
   begin {: sent:n tries:n longest:n :}
      tries SIP-SLICES >= longest SLICE-BOUND-MS >= or
      FLOOD-BYTES sent - LEAST-SPAN < or if cr longest exit then
      mono-ns {: start:n :}
      CLIENT-FD @ TCP4:>CONNECTION FLOOD sent + FLOOD-BYTES sent - TCP4:TRANSFER-BYTES
         TCP4:SEND-SLICE
      start ELAPSED-MS {: took:n :}
      STR-SPACE emit took FMT:.INT
      MATCH TCP4:slice-result
         moved OF BLEN>N sent + ENDOF
         empty OF sent ENDOF
         failed OF TCP4:ERRNO>N drop s" slice: failed" T-FAIL-AS cr longest exit ENDOF
      ;MATCH
      tries 1+ longest took max
   again ;


\ A reader that keeps taking bytes keeps making room, so no wait for room runs
\ out and only the slice's own deadline can end a slice handed a long span: each
\ returns within the bound, where a send left to wait in the kernel, its every
\ wait for room under a timeout of its own, ran on until the flood was in.
: T-SLICE-SIPPED ( -- )
   s" a reader taking bytes every 10 ms takes them while send slices run" T-LABEL
   OPEN-PAIR
   PROBE-CONNECTION @ TCP4:>CONNECTION true TCP4:NODELAY! STATUS-ERRNO 0 T=
   0 SIPPED ! 0 SIP-STOP ! 0 DRAIN-DONE !
   ['] SIP-WORK DRAIN-TASK TASK:ACTIVATE
   SIPPED-SLICE-MS {: longest:n :}
   SIPPED atomic@ 0 > TTRUE
   s" and every slice, each handed 1 MiB or more, ends within one slice and a second" T-LABEL
   longest SLICE-BOUND-MS < TTRUE
   1 SIP-STOP !
   CLIENT-FD @ TCP4:>CONNECTION TCP4:SENDING TCP4:SHUTDOWN STATUS-ERRNO 0 T=
   DRAIN-DONE 1 DRAIN-MS REACHED? TTRUE
   DRAIN-TASK TASK:KILL
   CLOSE-PAIR ;


\ A writer whose reader takes bytes every SIP-MS has slices that move some
\ rather than ones that end empty, and a halt still ends it within one slice:
\ WRITE pauses after every slice that leaves bytes to send. Stopping the reader
\ and shutting the writer's side releases a writer that never saw the halt, so
\ a regression fails here instead of hanging.
: T-WRITE-SIPPED ( -- )
   s" a task in WRITE to a reader taking bytes every 10 ms is still writing" T-LABEL
   0 WRITER-ARMED ! 0 WRITER-ENDED !
   OPEN-PAIR
   PROBE-CONNECTION @ TCP4:>CONNECTION true TCP4:NODELAY! STATUS-ERRNO 0 T=
   0 SIPPED ! 0 SIP-STOP ! 0 DRAIN-DONE !
   ['] SIP-WORK DRAIN-TASK TASK:ACTIVATE
   ['] WRITER-WORK WRITER-TASK TASK:ACTIVATE
   WRITER-ARMED 1 WAIT-MS REACHED? TTRUE
   FILL-MS >MS TASK:SLEEP
   WRITER-TASK TASK:DONE? TFALSE
   SIPPED atomic@ 0 > TTRUE
   s" and ends within one send slice and a second of its halt" T-LABEL
   WRITER-TASK TASK:HALT
   TCP4:SEND-SLICE-MS HALT-SLACK-MS + DONE-WITHIN? TTRUE
   WRITER-ENDED @ 0 T=
   1 SIP-STOP !
   CLIENT-FD @ TCP4:>CONNECTION TCP4:SENDING TCP4:SHUTDOWN STATUS-ERRNO 0 T=
   DRAIN-DONE 1 DRAIN-MS REACHED? TTRUE
   DRAIN-TASK TASK:KILL
   CLOSE-PAIR
   WRITER-TASK TASK:KILL ;


\ Slices of the chat until one fails: its errno, or 0 when none did within
\ POLL-TRIES slices.
: FAILED-SLICE-ERRNO ( -- n )
   POLL-TRIES 0 do
      CLIENT-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:SEND-SLICE
      MATCH TCP4:slice-result
         moved OF drop ENDOF
         empty OF ENDOF
         failed OF TCP4:ERRNO>N unloop exit ENDOF
      ;MATCH
      SETTLE-MS >MS TASK:SLEEP
   loop 0 ;


\ The peer closes with the chat unread, and the connection is reset: the next
\ slices fail with the errno the reset left, EPIPE or ECONNRESET.
: T-SLICE-FAILED ( -- )
   s" a send slice to a peer that closed and reset the connection fails" T-LABEL
   OPEN-PAIR
   CLIENT-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:SEND-SLICE MOVED-BYTES
      CHAT-BYTES T=
   PROBE-CONNECTION @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   FAILED-SLICE-ERRNO {: errno:n :}
   errno EPIPE = errno ECONNRESET = or TTRUE
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


\ The peer's UNREAD is the bytes waiting for it, and a read takes its own off;
\ the writer's UNSENT empties once the peer acknowledges them, which can lag
\ their arrival. A closed connection is counted by neither.
: T-QUEUES ( -- )
   s" UNREAD counts the bytes a connection holds unread" T-LABEL
   OPEN-PAIR
   CLIENT-FD @ TCP4:>CONNECTION CHAT$ TCP4:TRANSFER-BYTES TCP4:WRITE STATUS-ERRNO 0 T=
   [: PROBE-UNREAD ;] CHAT-BYTES COUNTS? TTRUE
   PROBE-CONNECTION @ TCP4:>CONNECTION SERVER-BUF 1 TCP4:TRANSFER-BYTES TCP4:READ 1 WANT-DATA
   s" and a read takes what it read off the count" T-LABEL
   PROBE-UNREAD CHAT-BYTES 1- T=
   s" UNSENT empties once the peer acknowledges the bytes" T-LABEL
   [: CLIENT-UNSENT ;] 0 COUNTS? TTRUE
   CLOSE-PAIR
   s" UNREAD fails with EBADF on a closed connection" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION TCP4:UNREAD QUEUE-ERRNO EBADF T=
   s" and so does UNSENT" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION TCP4:UNSENT QUEUE-ERRNO EBADF T= ;


: T-NODELAY ( -- )
   s" NODELAY! answers ok on a connected socket, on and off" T-LABEL
   NEW-LISTENER PROBE-LISTENER !
   PROBE-LISTENER @ LISTENER-PORT CONNECT-TO dup 0 >= TTRUE CLIENT-FD !
   CLIENT-FD @ TCP4:>CONNECTION true TCP4:NODELAY! STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION false TCP4:NODELAY! STATUS-ERRNO 0 T=
   s" and fails with EBADF on a closed one" T-LABEL
   CLIENT-FD @ TCP4:>CONNECTION TCP4:CLOSE STATUS-ERRNO 0 T=
   CLIENT-FD @ TCP4:>CONNECTION true TCP4:NODELAY! STATUS-ERRNO EBADF T=
   PROBE-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-ERRNO 0 T= ;


: RUN ( -- )
   T-RESET
   THREADS BASE-THREADS !
   AIO:START
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
   T-READ-WAITS
   FLOOD-OPEN
   T-WRITE-HALTED
   T-SLICE-MOVED
   T-SLICE-EMPTY
   T-SLICE-SIPPED
   T-WRITE-SIPPED
   FLOOD-CLOSE
   T-SLICE-FAILED
   T-QUEUES
   T-NODELAY
   T-NO-LOOP
   AIO:STOP
   T-REPORT
   s" tcp4-test: ok" type cr ;

RUN

;package
