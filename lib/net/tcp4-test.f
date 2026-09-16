\ tcp4-test.f - a loopback TCP connection carries bytes both ways.
\
\ One process holds both peers: a listener task blocks in ACCEPT while the main
\ task connects, writes and reads back. Run: bin/hb --load lib/net/tcp4-test.f
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/ffi-abi.f
require lib/task.f
require lib/net/tcp4.f

package TCP4-TEST

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
4 constant BACKLOG
$09 constant EBADF                 \ a transfer through a closed descriptor
$6F constant ECONNREFUSED          \ nobody listens on that port
$20 constant BUF-CAP
$40 constant POLL-TRIES

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

TASK:MIN-STACK TASK:TASK ECHO-TASK

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


\ Operands are checked before the platform and before any descriptor is touched,
\ so these need no socket.
: T-OPERANDS ( -- )
   s" an out-of-range operand throws before any socket call" T-LABEL
   [: BAD-PORT ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-ADDRESS ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-TRANSFER ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-BACKLOG ;] TCP4:E-OPERAND TTHROWSQ
   [: BAD-CAPACITY ;] TCP4:E-OPERAND TTHROWSQ ;


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
   T-OPERANDS
   T-ECHO
   T-READINESS
   T-REFUSED
   T-READ-AFTER-CLOSE
   T-REPORT
   s" tcp4-test: ok" type cr ;

RUN

;package
