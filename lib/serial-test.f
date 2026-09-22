\ serial-test.f - package SERIAL over a pseudoterminal pair, in one process.
\
\ The pair is the device: PTY:OPEN gives the master and the slave's path, and
\ SERIAL:OPEN8N1 opens that path at 115200 the way test/serial.py's peer does,
\ so every case here drives the same code the device-peer host drives, with the
\ master side as the peer. What this file pins that the host cannot: the wait is
\ the AIO loop's, so a READ parked in a task is not a thread parked in poll(2),
\ a zero-millisecond window still answers for a terminal that already has bytes,
\ and a wait with no loop running is refused by name.
\
\ Run: bin/hb --load lib/serial-test.f

require lib/test.f
require lib/string.f
require lib/task.f
require lib/process.f
require lib/aio.f
require lib/pty.f
require lib/serial.f

package SERIAL-TEST
private

CAST: BLEN>N ( NUM:byte-len -- n )

115200 constant SPEED
$20 constant BUF-CAP
1000000 constant NS-PER-MS
1000 constant WAIT-MS              \ a window generous enough for a local pty
30 constant IDLE-MS                \ the window an idle port waits out
50 constant PARK-MS                \ how long the parked task waits before bytes arrive
20 constant SETTLE-MS              \ bytes written at the master are at the slave by then
2000 constant JOIN-MS

1 constant K-MOVED                 \ the four io-result variants, as numbers
2 constant K-TIMEOUT
3 constant K-CLOSED
4 constant K-FAILED

5 constant EIO#                    \ the errno a hung-up terminal answers a write with
9 constant EBADF#                  \ ... and the one a closed handle answers with

create SLAVE PTY:SLAVE-PATH-CAP allot
create SPARE PTY:SLAVE-PATH-CAP allot
create BUF BUF-CAP allot

variable MASTER                    \ the master descriptor of the pair under test
variable HANDLE                    \ the open slave, as a number
variable PARK-KIND                 \ what the parked task's READ answered
variable PARK-COUNT

TASK:MIN-STACK TASK:TASK READER

: PORT ( -- SERIAL:handle )
   HANDLE @ SERIAL:>HANDLE ;

: PEER ( -- PTY:master )
   MASTER @ PTY:>MASTER ;

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

\ One chunk at a time until the master has handed over every byte, so a split
\ answer from the terminal is not read as a short transfer.
: PEER-EXACT ( ptr u8 n -- ) {: bytes size:n :}
   0 begin dup size < while
      dup {: offset:n :}
      PEER bytes offset + size offset - WAIT-MS >MS PTY:READ {: got:n :}
      got 0 <= if E-PROC-OUTPUT throw then
      got +
   repeat drop ;

: JOIN-READER ( -- )
   JOIN-MS >MS PROC-DEADLINE-AT {: deadline :}
   begin
      READER TASK:DONE? if exit then
      deadline PROC-LEFT-MS MS>N 0= if exit then
      TASK:PAUSE
   again ;

\ ---- case one: the slave of a fresh pair opens as a serial port --------------

: OPEN-CASE ( -- )
   SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN {: m:PTY:master len:n :}
   m PTY:MASTER>N MASTER !

   s" the slave path of a pty pair opens at 115200 8N1" T-LABEL
   SLAVE len SPEED SERIAL:BAUD SERIAL:OPEN8N1 OPENED {: fd:n :}
   fd 0 >= TTRUE
   fd HANDLE ! ;

\ ---- case two: bytes written at the master arrive at the port ----------------

: READ-CASE ( -- )
   PEER s" habu!" PTY:WRITE

   s" five bytes written at the master are read at the port" T-LABEL
   PORT BUF 5 SERIAL:BYTES WAIT-MS >MS SERIAL:READ K-MOVED 5 WANT

   s" ... and they are the bytes the master wrote" T-LABEL
   BUF 5 s" habu!" STR= TTRUE ;

\ ---- case three: an idle port waits out its window ---------------------------

: IDLE-CASE ( -- )
   mono-ns {: started:n :}
   PORT BUF 1 SERIAL:BYTES IDLE-MS >MS SERIAL:READ {: answer :}
   started ELAPSED-NS {: elapsed:n :}

   s" a read of an idle port answers a timeout" T-LABEL
   answer K-TIMEOUT 0 WANT

   s" ... after waiting out the window it was given" T-LABEL
   elapsed IDLE-MS NS-PER-MS * >= TTRUE ;

\ ---- case four: the zero-length window, measured on a terminal ---------------
\ A zero timeout links a zero-length timeout behind the poll. The kernel serves
\ the poll inline before that timer is armed, so a port that already has bytes
\ answers with them and only an idle one answers the timeout.

: ZERO-CASE ( -- )
   s" a zero-millisecond read of an idle port answers a timeout" T-LABEL
   PORT BUF 1 SERIAL:BYTES 0 >MS SERIAL:READ K-TIMEOUT 0 WANT

   PEER s" abc" PTY:WRITE
   SETTLE-MS >MS TASK:SLEEP

   s" ... and one whose bytes are already there answers with them" T-LABEL
   PORT BUF 3 SERIAL:BYTES 0 >MS SERIAL:READ K-MOVED 3 WANT ;

\ ---- case five: the port writes and the master reads -------------------------

: WRITE-CASE ( -- )
   s" six bytes written at the port are accepted whole" T-LABEL
   PORT s" forth!" SERIAL:BYTES WAIT-MS >MS SERIAL:WRITE K-MOVED 6 WANT

   s" ... and the master reads exactly those six" T-LABEL
   BUF 6 PEER-EXACT
   BUF 6 s" forth!" STR= TTRUE ;

\ ---- case six: a read parked in a task is not a thread parked in poll --------

: PARK-WORK ( -- )
   PORT BUF 4 SERIAL:BYTES WAIT-MS >MS SERIAL:READ
   CLASSIFY PARK-COUNT ! PARK-KIND ! ;

: PARKED-CASE ( -- )
   0 PARK-KIND ! 0 PARK-COUNT !
   ['] PARK-WORK READER TASK:ACTIVATE
   PARK-MS >MS TASK:SLEEP
   PEER s" tick" PTY:WRITE
   JOIN-READER

   s" a read parked in a task finished the window it was given" T-LABEL
   READER TASK:DONE? TTRUE

   s" ... answering the bytes the main task wrote at the master" T-LABEL
   PARK-KIND @ K-MOVED T=
   PARK-COUNT @ 4 T= ;

\ ---- case seven: a wait with the loop stopped is refused by name -------------
\ The loop is the program's to start, and a library never starts one: a wait
\ without one says so instead of falling back to a thread parked in poll(2).

: STOPPED-READ ( -- )
   PORT BUF 1 SERIAL:BYTES IDLE-MS >MS SERIAL:READ IO-DROP ;

: NO-LOOP-CASE ( -- )
   s" a read with the loop stopped is refused by name" T-LABEL
   AIO:STOP
   [: STOPPED-READ ;] E-AIO-STATE TTHROWSQ
   AIO:START ;

\ ---- case eight: the handle after CLOSE --------------------------------------
\ The kernel refuses the poll of a descriptor that names nothing with EBADF, so
\ the refused arm answers what POLLNVAL answered on the poll(2) path: errno 9,
\ the number test/serial.py pins for a read after close.

: AFTER-CLOSE-CASE ( -- )
   s" CLOSE of the open port succeeds" T-LABEL
   PORT SERIAL:CLOSE CLOSED-OK

   s" ... and a read on the closed handle fails with EBADF" T-LABEL
   PORT BUF 1 SERIAL:BYTES 0 >MS SERIAL:READ K-FAILED EBADF# WANT

   PEER PTY:CLOSE ;

\ ---- case nine: the master closed under an open slave ------------------------
\ test/serial.py's `disconnect` case pins `closed` then `error 5` on the poll(2)
\ path; the loop answers the same, through the same two steps. The hang-up
\ reaches the poll as POLLHUP, the read that follows it is refused with EAGAIN,
\ and the answered bits are what makes that a hang-up rather than a retry.

: HANGUP-CASE ( -- )
   SPARE PTY:SLAVE-PATH-CAP PTY:OPEN {: m:PTY:master len:n :}
   SPARE len SPEED SERIAL:BAUD SERIAL:OPEN8N1 OPENED {: fd:n :}
   fd 0 >= TTRUE
   fd HANDLE !
   m PTY:CLOSE

   s" a read of a port whose master closed answers closed" T-LABEL
   PORT BUF 1 SERIAL:BYTES WAIT-MS >MS SERIAL:READ K-CLOSED 0 WANT

   s" ... and the write after it fails with EIO" T-LABEL
   PORT s" x" SERIAL:BYTES 0 >MS SERIAL:WRITE K-FAILED EIO# WANT

   PORT SERIAL:CLOSE CLOSED-OK ;

public

: RUN ( -- )
   T-RESET
   AIO:START
   OPEN-CASE
   READ-CASE
   IDLE-CASE
   ZERO-CASE
   WRITE-CASE
   PARKED-CASE
   NO-LOOP-CASE
   AFTER-CLOSE-CASE
   HANGUP-CASE
   AIO:STOP
   T-REPORT
   s" serial-test: ok" type cr ;

;package

SERIAL-TEST:RUN
