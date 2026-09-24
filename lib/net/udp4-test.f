\ udp4-test.f - loopback datagrams, every wait bounded and on the AIO loop.
\
\ Two sockets in one process talk over 127.0.0.1, so the suite needs no peer and
\ no network. Every RECEIVE that waits does so on the AIO loop, which this suite
\ starts before its first case and stops after its last.
\ Run: bin/hb --load lib/net/udp4-test.f
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require lib/ffi-abi.f
require lib/task.f
require lib/aio.f                  \ the loop a waiting RECEIVE runs on
require lib/net/udp4.f

package UDP4-TEST

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
$40 constant PAYLOAD-N             \ the datagram every case sends
$10 constant SMALL-CAP             \ the capacity the truncating case offers
$80 constant BUF-CAP
1000000 constant NS-PER-MS

\ The deadlines the timed cases are measured against. They allow a scheduler's
\ slack while still failing a wait that did not happen at all.
1000 constant WAIT-MS              \ far beyond a loopback datagram's flight
30 constant IDLE-MS                \ what an idle socket waits out
20 constant NO-WAIT-MS             \ a zero timeout answers well inside this
50 constant SETTLE-MS              \ the armed task reaches its wait within this

create SEND-BUF BUF-CAP allot
create RECV-BUF BUF-CAP allot
create READY-POLL 0 ,

: TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

TEST-ALIGN8
variable SOCK-A
variable SOCK-B
variable PORT-A
variable PORT-B
variable PARK-ARMED
variable PARK-DONE
variable PARK-GOT

TASK:MIN-STACK TASK:TASK PARK-TASK


\ ---- result inspectors -------------------------------------------------------

: STATUS-ERRNO ( UDP4:status -- n )
   MATCH UDP4:status
      ok OF 0 ENDOF
      failed OF UDP4:ERRNO>N ENDOF
   ;MATCH ;


: WANT-PACKET ( UDP4:receive-result n n n -- ) {: want:n address:n port:n :}
   MATCH UDP4:receive-result
      packet OF UDP4:PORT>N port T= UDP4:ADDRESS>N address T= BLEN>N want T= ENDOF
      truncated OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop
         s" receive: unexpectedly truncated" T-FAIL-AS ENDOF
      timeout OF s" receive: unexpected timeout" T-FAIL-AS ENDOF
      failed OF UDP4:ERRNO>N drop s" receive: failed" T-FAIL-AS ENDOF
   ;MATCH ;


: WANT-TRUNCATED ( UDP4:receive-result n n n -- ) {: want:n address:n port:n :}
   MATCH UDP4:receive-result
      packet OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop
         s" receive: unexpectedly whole" T-FAIL-AS ENDOF
      truncated OF UDP4:PORT>N port T= UDP4:ADDRESS>N address T= BLEN>N want T= ENDOF
      timeout OF s" receive: unexpected timeout" T-FAIL-AS ENDOF
      failed OF UDP4:ERRNO>N drop s" receive: failed" T-FAIL-AS ENDOF
   ;MATCH ;


: WANT-TIMEOUT ( UDP4:receive-result -- )
   MATCH UDP4:receive-result
      packet OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop
         s" receive: unexpected packet" T-FAIL-AS ENDOF
      truncated OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop
         s" receive: unexpected packet" T-FAIL-AS ENDOF
      timeout OF ENDOF
      failed OF UDP4:ERRNO>N drop s" receive: failed" T-FAIL-AS ENDOF
   ;MATCH ;


: RECEIVE-DROP ( UDP4:receive-result -- )
   MATCH UDP4:receive-result
      packet OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop ENDOF
      truncated OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop ENDOF
      timeout OF ENDOF
      failed OF UDP4:ERRNO>N drop ENDOF
   ;MATCH ;


\ ---- the pair the cases share ------------------------------------------------

: OPEN-SOCKET ( -- n )
   LOOPBACK UDP4:ADDRESS 0 UDP4:PORT UDP4:BIND
   MATCH UDP4:open-result
      opened OF UDP4:SOCKET>N ENDOF
      failed OF UDP4:ERRNO>N drop s" bind failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


: PORT-OF ( n -- n )
   UDP4:>SOCKET UDP4:LOCAL
   MATCH UDP4:endpoint-result
      endpoint OF UDP4:PORT>N swap UDP4:ADDRESS>N drop ENDOF
      failed OF UDP4:ERRNO>N drop s" local failed" T-FAIL-AS 0 ENDOF
   ;MATCH ;


: OPEN-PAIR ( -- )
   OPEN-SOCKET dup 0 >= TTRUE SOCK-A !
   OPEN-SOCKET dup 0 >= TTRUE SOCK-B !
   SOCK-A @ PORT-OF dup 0 > TTRUE PORT-A !
   SOCK-B @ PORT-OF dup 0 > TTRUE PORT-B ! ;


: CLOSE-PAIR ( -- )
   SOCK-A @ UDP4:>SOCKET UDP4:CLOSE STATUS-ERRNO 0 T=
   SOCK-B @ UDP4:>SOCKET UDP4:CLOSE STATUS-ERRNO 0 T= ;


\ A datagram whose every byte says where in the datagram it was, so a truncated
\ answer can be checked to be the datagram's first bytes and not any other's.
: PAT-FILL ( -- )
   PAYLOAD-N 0 do i 7 * 3 + $FF and SEND-BUF i + c! loop ;


: PAT-CHECK ( n -- bool ) {: count:n :}
   count 0 do
      RECV-BUF i + c@ i 7 * 3 + $FF and <> if unloop false exit then
   loop true ;


: SEND-PAYLOAD ( -- )
   SOCK-A @ UDP4:>SOCKET LOOPBACK UDP4:ADDRESS PORT-B @ UDP4:PORT
   SEND-BUF PAYLOAD-N UDP4:PAYLOAD-BYTES UDP4:SEND STATUS-ERRNO 0 T= ;


: RECEIVE-B ( n n -- UDP4:receive-result ) {: capacity:n timeout:n :}
   SOCK-B @ UDP4:>SOCKET RECV-BUF capacity UDP4:PAYLOAD-BYTES timeout >MS
   UDP4:RECEIVE ;


: ELAPSED-MS ( n -- n ) {: start:n :}
   mono-ns start - NS-PER-MS / ;


: REACHED? ( ptr n n -- bool ) {: cell:ptr want:n :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      cell atomic@ want >= if true exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


\ ---- cases -------------------------------------------------------------------

: T-DATAGRAM ( -- )
   s" a datagram arrives with its length and its sender's address and port" T-LABEL
   PAT-FILL
   OPEN-PAIR
   SEND-PAYLOAD
   BUF-CAP WAIT-MS RECEIVE-B PAYLOAD-N LOOPBACK PORT-A @ WANT-PACKET
   s" and the bytes delivered are the bytes that were sent" T-LABEL
   PAYLOAD-N PAT-CHECK TTRUE
   CLOSE-PAIR ;


: T-TIMEOUT ( -- )
   s" an idle socket answers timeout, and waits out its deadline" T-LABEL
   OPEN-PAIR
   mono-ns {: start:n :}
   BUF-CAP IDLE-MS RECEIVE-B WANT-TIMEOUT
   start ELAPSED-MS IDLE-MS >= TTRUE
   CLOSE-PAIR ;


\ A zero timeout is one immediate try: it never reaches the loop, so it answers
\ before any deadline could have been waited out. The queued datagram is the
\ same call's other answer. sendto only promises acceptance, so poll witnesses
\ delivery before the test asks for an immediate receive.
: T-ZERO-TIMEOUT ( -- )
   s" a zero timeout on an idle socket answers timeout without waiting" T-LABEL
   PAT-FILL
   OPEN-PAIR
   mono-ns {: start:n :}
   BUF-CAP 0 RECEIVE-B WANT-TIMEOUT
   start ELAPSED-MS NO-WAIT-MS < TTRUE
   s" and a zero timeout with a datagram queued answers it" T-LABEL
   SEND-PAYLOAD
   SOCK-B @ 1 32 lshift or READY-POLL !
   READY-POLL 1 WAIT-MS poll 1 T=
   BUF-CAP 0 RECEIVE-B PAYLOAD-N LOOPBACK PORT-A @ WANT-PACKET
   PAYLOAD-N PAT-CHECK TTRUE
   CLOSE-PAIR ;


: T-TRUNCATED ( -- )
   s" a datagram larger than the capacity is truncated, carrying its own length" T-LABEL
   PAT-FILL
   OPEN-PAIR
   SEND-PAYLOAD
   SMALL-CAP WAIT-MS RECEIVE-B PAYLOAD-N LOOPBACK PORT-A @ WANT-TRUNCATED
   s" and the capacity it filled holds the datagram's first bytes" T-LABEL
   SMALL-CAP PAT-CHECK TTRUE
   CLOSE-PAIR ;


\ The socket is nonblocking, so a RECEIVE with nothing queued is parked on the
\ loop and not inside recvfrom: the main thread's later send is what answers it.
: PARK-WORK ( -- )
   1 PARK-ARMED atomic-add drop
   BUF-CAP WAIT-MS RECEIVE-B
   MATCH UDP4:receive-result
      packet OF UDP4:PORT>N drop UDP4:ADDRESS>N drop BLEN>N PARK-GOT ! ENDOF
      truncated OF UDP4:PORT>N drop UDP4:ADDRESS>N drop drop -1 PARK-GOT ! ENDOF
      timeout OF -2 PARK-GOT ! ENDOF
      failed OF UDP4:ERRNO>N negate PARK-GOT ! ENDOF
   ;MATCH
   1 PARK-DONE atomic-add drop ;


: T-PARKED-RECEIVE ( -- )
   s" a RECEIVE parked in the loop is answered by a later send" T-LABEL
   0 PARK-ARMED ! 0 PARK-DONE ! 0 PARK-GOT !
   PAT-FILL
   OPEN-PAIR
   ['] PARK-WORK PARK-TASK TASK:ACTIVATE
   PARK-ARMED 1 REACHED? TTRUE
   SETTLE-MS >MS TASK:SLEEP
   SEND-PAYLOAD
   PARK-DONE 1 REACHED? TTRUE
   PARK-GOT @ PAYLOAD-N T=
   PARK-TASK TASK:KILL
   CLOSE-PAIR ;


: STOPPED-RECEIVE ( -- )
   BUF-CAP IDLE-MS RECEIVE-B RECEIVE-DROP ;


\ The loop is the program's to start, and a RECEIVE that must wait says so by
\ name instead of falling back to a thread parked in poll(2).
: T-NO-LOOP ( -- )
   s" a RECEIVE that must wait with the loop stopped is refused by name" T-LABEL
   OPEN-PAIR
   AIO:STOP
   [: STOPPED-RECEIVE ;] E-AIO-STATE TTHROWSQ
   AIO:START
   CLOSE-PAIR ;


: RUN ( -- )
   T-RESET
   AIO:START
   T-DATAGRAM
   T-TIMEOUT
   T-ZERO-TIMEOUT
   T-TRUNCATED
   T-PARKED-RECEIVE
   T-NO-LOOP
   AIO:STOP
   T-REPORT
   s" udp4-test: ok" type cr ;

RUN

;package
