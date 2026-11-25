\ serial-test.f - package SERIAL over a pseudoterminal pair, in one process: the
\ master is the peer, the slave opened by SERIAL:OPEN8N1 at 115200 as
\ test/serial.py's host does. Pinned here, not by the host: a parked READ is not
\ a thread parked in poll(2), a zero window answers a terminal holding bytes,
\ and a wait with no loop is rejected by name.
\ Run: bin/hb --load src/lib/serial-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/pty.f
require src/lib/serial.f

package SERIAL-TEST
private

CAST: BLEN>N ( NUM:byte-len -- n )

115200 constant SPEED
1000000 constant NS-PER-MS
30 constant IDLE-MS                \ the window an idle port waits out
50 constant PARK-MS                \ how long the parked task waits before bytes arrive

1 constant K-MOVED                 \ the four io-result variants, as numbers
2 constant K-TIMEOUT
3 constant K-CLOSED
4 constant K-FAILED

5 constant EIO#                    \ the errno a hung-up terminal answers a write with
9 constant EBADF#                  \ ... and the one a closed handle answers with

create SLAVE PTY:SLAVE-PATH-CAP allot
create SPARE PTY:SLAVE-PATH-CAP allot
TYPED-VARIABLE BUF ^VEC:vector<u8>   \ RUN's, which every read appends to

variable MASTER
variable HANDLE                    \ the open slave, as a number
variable PARK-KIND                 \ what the parked task's READ answered
variable PARK-COUNT

TASK:MIN-STACK TASK:TASK READER

: PORT ( -- SERIAL:handle )
   HANDLE @ SERIAL:>HANDLE ;

: PEER ( -- PTY:master )
   MASTER @ PTY:>MASTER ;

\ BUF emptied for a read that starts over.
: EMPTY ( -- ^VEC:vector<u8> )
   BUF @ dup 0 VEC:RESIZE ;

\ The io-result as a kind and its payload: the bytes moved, the errno, or zero.
: CLASSIFY ( SERIAL:io-result -- n n )
   MATCH SERIAL:io-result
      transferred OF BLEN>N K-MOVED swap ENDOF
      timeout OF K-TIMEOUT 0 ENDOF
      closed OF K-CLOSED 0 ENDOF
      failed OF SERIAL:ERRNO>N K-FAILED swap ENDOF
   ;MATCH ;

: WANT ( SERIAL:io-result n n -- ) {: kind:n payload:n :}
   CLASSIFY {: got-kind:n got-payload:n :}
   got-kind kind T=
   got-payload payload T= ;

: IO-DROP ( SERIAL:io-result -- )
   CLASSIFY 2drop ;

: OPENED ( SERIAL:open-result -- n )
   MATCH SERIAL:open-result
      opened OF SERIAL:HANDLE>N ENDOF
      failed OF SERIAL:ERRNO>N negate ENDOF
      unsupported OF -1 ENDOF
   ;MATCH ;

: CLOSED-OK ( SERIAL:status -- )
   MATCH SERIAL:status
      ok OF 0 0 T= ENDOF
      failed OF SERIAL:ERRNO>N 0 T= ENDOF
   ;MATCH ;

: ELAPSED-NS ( n -- n ) {: started:n :}
   mono-ns started - ;

\ One chunk at a time until the master has handed over n bytes in all, so a
\ split answer from the terminal is not read as a short transfer.
: PEER-EXACT ( ^VEC:vector<u8> n -- ) {: v:^ size:n :}
   begin v VEC:COUNT size < while
      PEER v size v VEC:COUNT - -1 >MS PTY:READ
      0 <= if E-PROC-OUTPUT throw then
   repeat ;

: JOIN-READER ( -- )
   [: READER TASK:DONE? ;] TEST-WAIT:POLL ;

: NUL-OPEN ( -- )
   S\" /dev/tty\x00x" SPEED SERIAL:BAUD SERIAL:OPEN8N1 OPENED drop ;

: OPEN-CASE ( -- )
   SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN {: m:PTY:master len:n :}
   m PTY:MASTER>N MASTER !

   s" the slave path of a pty pair opens at 115200 8N1" T-LABEL
   SLAVE len >str SPEED SERIAL:BAUD SERIAL:OPEN8N1 OPENED {: fd:n :}
   fd 0 >= TTRUE
   fd HANDLE !

   s" a device path holding a 0 byte is refused before the OS sees it" T-LABEL
   [: NUL-OPEN ;] E-FFI-NUL TTHROWSQ ;

: READ-CASE ( -- )
   PEER s" habu!" PTY:WRITE

   s" five bytes written at the master are read at the port" T-LABEL
   PORT EMPTY 5 SERIAL:BYTES -1 >MS SERIAL:READ K-MOVED 5 WANT

   s" ... and they are the bytes the master wrote" T-LABEL
   BUF @ VEC:BYTES s" habu!" STR:= TTRUE ;

\ A read appends: what the vector held stays, through a timeout and a read.
: APPEND-CASE ( -- )
   s" ab" EMPTY VEC:APPEND

   s" a read that times out leaves the bytes a vector holds" T-LABEL
   PORT BUF @ 1 SERIAL:BYTES 0 >MS SERIAL:READ K-TIMEOUT 0 WANT
   BUF @ VEC:BYTES s" ab" STR:= TTRUE

   PEER s" cd" PTY:WRITE
   s" ... and a read appends after them" T-LABEL
   PORT BUF @ 2 SERIAL:BYTES -1 >MS SERIAL:READ K-MOVED 2 WANT
   BUF @ VEC:BYTES s" abcd" STR:= TTRUE ;

: IDLE-CASE ( -- )
   mono-ns {: started:n :}
   PORT EMPTY 1 SERIAL:BYTES IDLE-MS >MS SERIAL:READ {: answer :}
   started ELAPSED-NS {: elapsed:n :}

   s" a read of an idle port answers a timeout" T-LABEL
   answer K-TIMEOUT 0 WANT

   s" ... after waiting out the window it was given" T-LABEL
   elapsed IDLE-MS NS-PER-MS * >= TTRUE ;

\ Blocks until the port is readable. Any other outcome leaves its bytes
\ missing, which the zero-window read after it reports.
: AWAIT-INPUT ( -- )
   HANDLE @ >FD AIO:READABLE -1 >MS AIO:WAIT-READY
   MATCH AIO:readiness
      ready OF drop ENDOF
      timed-out OF ENDOF
      rejected OF drop ENDOF
   ;MATCH ;

\ A zero timeout links a zero-length timeout behind the poll. The kernel serves
\ the poll inline before that timer is armed, so a port that already has bytes
\ answers with them and only an idle one answers the timeout.

: ZERO-CASE ( -- )
   s" a zero-millisecond read of an idle port answers a timeout" T-LABEL
   PORT EMPTY 1 SERIAL:BYTES 0 >MS SERIAL:READ K-TIMEOUT 0 WANT

   PEER s" abc" PTY:WRITE
   AWAIT-INPUT

   s" ... and one whose bytes are already there answers with them" T-LABEL
   PORT EMPTY 3 SERIAL:BYTES 0 >MS SERIAL:READ K-MOVED 3 WANT ;

: WRITE-CASE ( -- )
   s" six bytes written at the port are accepted whole" T-LABEL
   PORT s" forth!" -1 >MS SERIAL:WRITE K-MOVED 6 WANT

   s" ... and the master reads exactly those six" T-LABEL
   EMPTY 6 PEER-EXACT
   BUF @ VEC:BYTES s" forth!" STR:= TTRUE ;

\ A task asserts nothing: the reader records what its READ answered.

: PARK-WORK ( -- )
   PORT EMPTY 4 SERIAL:BYTES -1 >MS SERIAL:READ
   CLASSIFY PARK-COUNT ! PARK-KIND ! ;

: PARKED-CASE ( -- )
   0 PARK-KIND ! 0 PARK-COUNT !
   ['] PARK-WORK READER TASK:ACTIVATE
   PARK-MS >MS TASK:SLEEP
   PEER s" tick" PTY:WRITE
   JOIN-READER

   s" a read parked in a task answers the bytes the main task wrote at the master" T-LABEL
   PARK-KIND @ K-MOVED T=
   PARK-COUNT @ 4 T= ;

\ The loop is the program's to start, and a library never starts one: a wait
\ without one says so instead of falling back to a thread parked in poll(2).

: STOPPED-READ ( -- )
   PORT EMPTY 1 SERIAL:BYTES IDLE-MS >MS SERIAL:READ IO-DROP ;

: NO-LOOP-CASE ( -- )
   s" a read with the loop stopped is rejected by name" T-LABEL
   AIO:STOP
   [: STOPPED-READ ;] E-AIO-STATE TTHROWSQ
   AIO:START ;

\ The kernel fails the poll of a descriptor that names nothing with EBADF, so
\ the rejected arm answers what POLLNVAL answered on the poll(2) path: errno 9,
\ the number test/serial.py pins for a read after close.

: AFTER-CLOSE-CASE ( -- )
   s" CLOSE of the open port succeeds" T-LABEL
   PORT SERIAL:CLOSE CLOSED-OK

   s" ... and a read on the closed handle fails with EBADF" T-LABEL
   PORT EMPTY 1 SERIAL:BYTES 0 >MS SERIAL:READ K-FAILED EBADF# WANT

   PEER PTY:CLOSE ;

\ test/serial.py's `disconnect` case pins `closed` then `error 5` on the poll(2)
\ path; the loop answers the same, through the same two steps. The hang-up
\ reaches the poll as POLLHUP, the read that follows it is refused with EAGAIN,
\ and the answered bits are what makes that a hang-up rather than a retry.

: HANGUP-CASE ( -- )
   SPARE PTY:SLAVE-PATH-CAP PTY:OPEN {: m:PTY:master len:n :}
   SPARE len >str SPEED SERIAL:BAUD SERIAL:OPEN8N1 OPENED {: fd:n :}
   fd 0 >= TTRUE
   fd HANDLE !
   m PTY:CLOSE

   s" a read of a port whose master closed answers closed" T-LABEL
   PORT EMPTY 1 SERIAL:BYTES -1 >MS SERIAL:READ K-CLOSED 0 WANT

   s" ... and the write after it fails with EIO" T-LABEL
   PORT s" x" 0 >MS SERIAL:WRITE K-FAILED EIO# WANT

   PORT SERIAL:CLOSE CLOSED-OK ;

public

: RUN ( -- )
   T-RESET
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW BUF !
   AIO:START
   OPEN-CASE
   READ-CASE
   APPEND-CASE
   IDLE-CASE
   ZERO-CASE
   WRITE-CASE
   PARKED-CASE
   NO-LOOP-CASE
   AFTER-CLOSE-CASE
   HANGUP-CASE
   AIO:STOP
   BUF @ VEC:DELETE
   T-REPORT
   s" serial-test: ok" type cr ;

;package

SERIAL-TEST:RUN
