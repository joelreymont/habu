\ ws-test.f - RFC 6455 connections over a real loopback port: a Habu client on
\ TCP4 shakes hands with lib/net/ws.f inside an HTTP server and speaks frames
\ to it, built and read with the codec's own words (lib/net/ws-frame.f).
\
\ Every frame the client sends or reads one at a time goes into a transcript as
\ one line - direction, opcode (`+` when more fragments follow), length, and the
\ payload in hex or, past 24 bytes, as the sum of its bytes. The push is one line
\ of counts, and the burst logs none. The Linux stalled-pong cases record their
\ transport, mutex and close observations. The transcript is written to
\ build/ws-transcript.txt, read back and compared whole with the one this file
\ expects.
\
\ Every handler is defined before the server starts, because Habu forbids
\ dictionary mutation while a task is live.
\ Run: bin/hb --load lib/net/ws-test.f

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f
require lib/aio.f
require lib/span.f
require lib/memory.f
require lib/num-types.f
require lib/adt/result.f
require lib/net/tcp4.f
require lib/net/http.f
require lib/net/ws.f

\ White-box readers: they reopen WS so that the state a case waits for, which
\ no public word answers, is visible here. The definitions land in WS-TEST, so
\ WS's public surface gains nothing for being tested.
package WS
\ Whether the last slice of the write in hand on the socket's slot found no
\ room (PUT).
: WS-TEST:STALLED? ( socket -- bool )
   SOCKET>N >SLOT STALLED atomic@ ;

\ When every write on the socket's slot gives up, while the slot's worker
\ claims the lock; 0 otherwise (CLAIM).
: WS-TEST:CLAIM-DUE ( socket -- n )
   SOCKET>N >SLOT DUE atomic@ ;

\ Whether bytes the peer sent wait in the socket's connection, unread.
: WS-TEST:QUEUED? ( socket -- bool )
   SOCKET>N >SLOT mono-ns READABLE? ;

\ Whether the header of the frame in hand on the socket's slot is decoded, as
\ it stays until the frame has been acted on and reset (FRAME-IN, TURN).
: WS-TEST:DECODED? ( socket -- bool )
   SOCKET>N >SLOT HEAD-DONE @ 0 <> ;

\ How many of the bytes the peer sent behind its handshake the socket's slot
\ holds, untaken by any RECEIVE (TAKE-EARLY).
: WS-TEST:EARLY-HELD ( socket -- n )
   SOCKET>N >SLOT EARLY-U @ ;

\ The connection the socket's slot writes to.
: WS-TEST:LINK ( socket -- TCP4:connection )
   SOCKET>N >SLOT LINK @ ;

\ Linux TASK:FACILITY stores its pthread_mutex_t after its eight-byte owner.
\ The mutex's first int is glibc's private futex word.
: WS-TEST:LOCK-FUTEX ( socket -- ptr u8 )
   SOCKET>N >SLOT LOCK BYTE-VIEW 8 + ;

\ Set by the write that first sees HTTP's stop flag. Read after STOP returns.
: WS-TEST:CUT-AT ( socket -- n )
   SOCKET>N >SLOT CUT-AT @ ;
;package

package TCP4
: WS-TEST:FD ( connection -- n ) CONNECTION-FD ;
;package

package WS-TEST

$7F000001 constant LOOPBACK
2 constant WORKERS                \ so a socket is served in either slot
1 constant ONE-WORKER             \ so each socket is served in the slot the one before it held
$1F4 constant IDLE-MS
$32 constant RECEIVE-MS           \ short, so the timeout arm is taken while a client thinks
10 constant POLL-MS               \ how often a quiet client looks at the handler's count
$7D0 constant ANSWER-MS           \ a loopback answer that slow is a hang
$2710 constant PARK-MS            \ far past every stop bound
$3E8 constant GRACE-MS            \ what a bounded wait may run past its bound on a loaded box
\ The longest a case waits for a state that comes within a send slice or two:
\ a push stalled in the loopback buffers it fills at once, a push under way to
\ a client taking its bytes, the worker come for its slot's lock, bytes queued
\ in a socket, or the next pong of a fill. Each came within 130 ms at load
\ 57-180, the pongs within 70 ms at load 40-120.
$2710 constant REACH-MS
125 constant PING-LEN             \ the longest payload a control frame carries
4000 constant BURST-PINGS         \ empty pings sent in one write, which take seconds to answer
BURST-PINGS WS:HEADER-MAX * constant VOLLEY-CAP
16 constant FLOOD-PINGS           \ pings of PING-LEN bytes the flooding client sends in one write
$1000 constant BURST-CAP          \ FLOOD-PINGS pings, as the client sends them
$12000 constant BUF-CAP           \ the longest frame a case sends or is sent, with its header
$11170 constant BIG               \ 70000 bytes: past the 16-bit length form
$100000 constant TASK-STACK
$400 constant HEAD-CAP
$10000 constant STREAM-CAP        \ what one read takes of the server's stream
$3000 constant SCRIPT-CAP
$40 constant RAW-CAP
$18 constant HEX-MAX              \ the longest payload a transcript line spells out
5000 constant BLAST-BYTES         \ past one write: a header, then its payload
400 constant BLAST-N              \ frames from each of the two tasks that send at once
$50 constant PUSH-BYTE            \ 'P', what the pushing task's frames are full of
$48 constant HANDLER-BYTE         \ 'H', what the handler's are
$52 constant REST-BYTE            \ 'R', what the frame whose rest waits in the socket is full of
$40 constant LEAD-BYTES           \ of its payload, those that ride behind the handshake
$1000 constant REST-BYTES         \ the rest, in one write far under a loopback segment
LEAD-BYTES REST-BYTES + constant REST-FRAME-BYTES
$37FA213D constant MASK-KEY       \ the masking key of RFC 6455 section 5.7's examples
1000000 constant NS-PER-MS
1 constant SOL-SOCKET
7 constant SO-SNDBUF
8 constant SO-RCVBUF
6 constant IPPROTO-TCP
10 constant TCP-WINDOW-CLAMP
11 constant TCP-INFO
25 constant TCP-NOTSENT-LOWAT
98 constant ARM-FUTEX             \ Linux AArch64 __NR_futex
202 constant X64-FUTEX            \ Linux x86-64 __NR_futex
129 constant FUTEX-WAKE-PRIVATE  \ FUTEX_WAKE | FUTEX_PRIVATE_FLAG
2048 constant PEER-RCVBUF
2048 constant SERVER-SNDBUF
4096 constant PEER-WINDOW
1 constant SEND-LOWAT
$90 constant TCPI-NOTSENT-OFF
$E4 constant TCPI-SND-WND-OFF
$E8 constant TCP-INFO-MIN
$100 constant TCP-INFO-CAP
13 constant CR
10 constant LF
32 constant SP
-9980 constant E-SILENT           \ this file's own fixture refusals, outside every lib block
-9981 constant E-HANDLER          \ the handler that is meant to throw
-9982 constant E-DIAL
-9983 constant E-REFUSED          \ a handshake refused on a route only good ones are sent to
-9984 constant E-EXIT             \ the application exit hook that is meant to fail

: LINUX? ( -- bool )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or ;

CAST: BLEN>N ( NUM:byte-len -- n )

\ These Linux observations and socket controls belong only to this fixture.
\ The socket options fix the non-reading client's receive capacity and keep
\ the sender's unsent queue above its buffer cap. FUTEX_WAKE acknowledges a
\ real waiter on this glibc mutex; a spurious wake leaves pthread_mutex_lock
\ waiting while the pong owns it.
PROCESS-SYMBOLS
FUNCTION: SET-OPT-CALL setsockopt ( n n n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: GET-OPT-CALL getsockopt ( n n n ptr u8 ptr u8 -- i32 )
   3 TCP-INFO-CAP WRITES-BYTES
   4 4 WRITES-BYTES
;FUNCTION
FUNCTION: FUTEX-WAKE-CALL syscall ( n ptr u8 n n -- n )
   1 VARIADIC
;FUNCTION

TCP-INFO-CAP BUFFER: TCP-INFO-BUF
4 BUFFER: OPT-LEN
4 BUFFER: OPT-VAL

: OPT-U32! ( TCP4:connection n n n -- )
   {: conn:TCP4:connection level:n opt:n val:n :}
   val OPT-VAL LE:U32!
   conn WS-TEST:FD level opt OPT-VAL 4 SET-OPT-CALL 0 <> if E-SILENT throw then ;

: OPT-U32@ ( TCP4:connection n n -- n )
   {: conn:TCP4:connection level:n opt:n :}
   4 OPT-LEN LE:U32!
   conn WS-TEST:FD level opt TCP-INFO-BUF OPT-LEN GET-OPT-CALL
   0 <> if E-SILENT throw then
   OPT-LEN LE:U32@ 4 <> if E-SILENT throw then
   TCP-INFO-BUF LE:U32@ ;

: TCP-STATE ( TCP4:connection -- n n )
   {: conn:TCP4:connection :}
   TCP-INFO-CAP OPT-LEN LE:U32!
   conn WS-TEST:FD IPPROTO-TCP TCP-INFO TCP-INFO-BUF OPT-LEN GET-OPT-CALL
   0 <> if E-SILENT throw then
   OPT-LEN LE:U32@ TCP-INFO-MIN < if E-SILENT throw then
   TCP-INFO-BUF TCPI-NOTSENT-OFF + LE:U32@
   TCP-INFO-BUF TCPI-SND-WND-OFF + LE:U32@ ;

BUF-CAP SPAN-BUFFER: PATTERN
BUF-CAP SPAN-BUFFER: CLIENT-TX
BUF-CAP SPAN-BUFFER: CLIENT-RX
BLAST-BYTES SPAN-BUFFER: PUSH-BUF
BLAST-BYTES SPAN-BUFFER: HANDLER-BUF
WS:HEADER-MAX SPAN-BUFFER: RX-HEAD
HEAD-CAP SPAN-BUFFER: HEAD-BUF
STREAM-CAP SPAN-BUFFER: STREAM-BUF  \ what the client has read and not yet taken
HEAD-CAP SPAN-BUFFER: REQ-BUF
RAW-CAP SPAN-BUFFER: RAW-BUF
2 SPAN-BUFFER: STATUS-BUF
2 SPAN-BUFFER: NOT-UTF8             \ c3 28: a lead byte with no continuation behind it
2 SPAN-BUFFER: HEX-PAIR
REST-FRAME-BYTES SPAN-BUFFER: REST-BUF  \ that frame's payload
SCRIPT-CAP SPAN-BUFFER: SCRIPT-BUF  \ the transcript this run captured
SCRIPT-CAP SPAN-BUFFER: WANT-BUF    \ the transcript this file expects
SCRIPT-CAP SPAN-BUFFER: BACK-BUF    \ the transcript read back from its file
BURST-CAP SPAN-BUFFER: BURST-BUF   \ what the flooding client sends over and over
VOLLEY-CAP SPAN-BUFFER: VOLLEY-BUF \ the pings the client sends in one write

variable SCRIPT-U
variable WANT-U
variable BURST-U
variable HEAD-U
variable STREAM-AT
variable STREAM-U
variable REQ-U
variable RX-FIN
variable RX-LEN
variable RX-EOF
TYPED-VARIABLE RX-OP WS:opcode

\ What the handlers saw, read by the main thread once the server has ended the
\ stream their case ran on.
variable TIMEOUTS                 \ timeouts the serving handler has been answered
variable LAST-CLOSE               \ the status the last closed arm carried
variable BAD-CODE                 \ what closing with status 1005 threw
variable LATE-SEND                \ what sending behind the handler's own close threw
variable SECOND-ACCEPT            \ what accepting one request twice threw
variable REFUSED-WITH             \ the status /echo's handler was told its handshake was refused with
variable BAD-TEXT                 \ what sending text that is not UTF-8 threw
variable PUSHED-N                 \ whole frames the client read from the pushing task
variable HANDLER-N                \ and from the handler
variable TORN                     \ frames that were neither
variable KEPT                     \ set once the case's handler has kept its socket
variable STALE-SEND               \ what sending through a killed worker's socket threw
variable EXIT-FAULT               \ set while every worker's exit is to throw
variable CLOSED-AT                \ when the stalled socket's handler heard it close
variable GO-HOME                  \ set to make the handler that keeps its socket return
variable FLOOD-CODE               \ what refused the push of the handler that floods its own client
variable FLOOD-END                \ and when
variable EARLY-SEEN               \ the bytes the handler found held behind the handshake
variable REST-WRITTEN             \ set once the client's write of the frame's rest has returned
variable REST-SEEN                \ set once the handler found that rest queued in its socket before it received
variable ZERO-TIMEOUT             \ set when its RECEIVE that waits not at all answered timeout
variable LONGEST                  \ the longest the timed handler's RECEIVE took, in ns
variable TOOK-U                   \ the length of the binary message the timed handler took last
variable TOOK-BYTE                \ the one byte all of it was, or -1 when it was mixed
variable VOLLEY-U
variable PONGS                    \ pongs read whole of the ones the burst is owed
variable LATE-AT                  \ pongs read when the timed handler was first seen to time out during the burst
variable PEER-CAP                 \ Linux's actual fixed receive buffer size

1 TYPED-BUFFER HELD WS:socket     \ the socket a case keeps for another task
1 TYPED-BUFFER KEPT-REQUEST HTTP:request
1 TYPED-BUFFER KEPT-RESPONSE HTTP:response
1 TYPED-BUFFER PEER TCP4:connection  \ the connection a client task floods

: WAKE-LOCK-WAITER ( -- n )
   HB-TARGET-LINUX-X86-64? if X64-FUTEX else ARM-FUTEX then
   0 HELD @ WS-TEST:LOCK-FUTEX FUTEX-WAKE-PRIVATE 1
   FUTEX-WAKE-CALL dup 0 < if E-SILENT throw then ;

TASK:SEMAPHORE PUSH-GO            \ the handler has a socket for the pushing task
TASK:SEMAPHORE BLAST-GO           \ the client has asked both tasks to send at once
TASK-STACK TASK:TASK PUSH-TASK
TASK-STACK TASK:TASK PING-TASK    \ the client that pings and never reads
TASK-STACK TASK:TASK SEND-TASK    \ another task's send through the kept socket
TASK-STACK TASK:TASK STALE-TASK   \ another task's send through a killed worker's socket
TASK-STACK TASK:TASK VOLLEY-TASK  \ the client that sends the burst


\ ---- the handlers, all defined before any task is live -----------------------

\ What the probe answers once it answers other than 0, asked every POLL-MS; 0
\ once that many ms have passed without that.
: AWAITED ( [ -- n ] n -- n )
   {: probe limit:n :}
   mono-ns limit NS-PER-MS * + {: deadline:n :}
   begin
      probe execute {: got:n :}
      got 0 <> if got exit then
      mono-ns deadline > if 0 exit then
      POLL-MS >MS TASK:SLEEP
   again ;

: UNIFORM? ( ptr u8 n -- bool ) {: a u:n :}
   u 0 ?do a i + c@ a c@ <> if false unloop exit then loop
   true ;

: TEXT-BACK ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   sock a u WS:SEND-TEXT ;

: BINARY-BACK ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   sock a u WS:SEND-BINARY ;

\ Every message goes back as it came, until the socket closes.
: SERVE ( WS:socket -- ) {: sock:WS:socket :}
   begin
      sock RECEIVE-MS >MS WS:RECEIVE
      MATCH WS:message
         text OF sock TEXT-BACK ENDOF
         binary OF sock BINARY-BACK ENDOF
         closed OF LAST-CLOSE ! exit ENDOF
         timeout OF 1 TIMEOUTS +! ENDOF
      ;MATCH
   again ;

\ Messages are dropped until the socket closes, each waited for that long.
: DROPPED ( WS:socket ms -- ) {: sock:WS:socket limit:ms :}
   begin
      sock limit WS:RECEIVE
      MATCH WS:message
         text OF 2drop ENDOF
         binary OF 2drop ENDOF
         closed OF LAST-CLOSE ! exit ENDOF
         timeout OF ENDOF
      ;MATCH
   again ;

: UNTIL-CLOSED ( WS:socket -- )
   RECEIVE-MS >MS DROPPED ;

\ One message, whatever it says; nothing is left to wait for once the socket
\ has closed instead.
: ONE-MESSAGE ( WS:socket -- ) {: sock:WS:socket :}
   begin
      sock RECEIVE-MS >MS WS:RECEIVE
      MATCH WS:message
         text OF 2drop exit ENDOF
         binary OF 2drop exit ENDOF
         closed OF LAST-CLOSE ! exit ENDOF
         timeout OF ENDOF
      ;MATCH
   again ;

\ The socket of a handshake that passed. Every route but /echo is sent only
\ good handshakes, so a refusal there fails its case by name.
: OPENED ( WS:accepted -- WS:socket )
   MATCH WS:accepted
      open OF ENDOF
      refused OF drop E-REFUSED throw ENDOF
   ;MATCH ;

\ A handshake that passed is served; a refused one leaves its status.
: ECHO ( HTTP:request HTTP:response -- )
   WS:ACCEPT
   MATCH WS:accepted
      open OF SERVE ENDOF
      refused OF REFUSED-WITH ! ENDOF
   ;MATCH ;

\ The handler returns with its socket still open.
: LEAVER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED ONE-MESSAGE ;

: THROWER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED ONE-MESSAGE
   E-HANDLER throw ;

: CLOSE-UNSENDABLE ( -- )
   0 HELD @ 1005 WS:CLOSE ;

: SEND-HELD ( -- )
   0 HELD @ s" late" WS:SEND-TEXT ;

\ The server closes first: a status no frame may carry is refused, the second
\ close is nothing new, and nothing is sent behind the close.
: CLOSER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   sock ONE-MESSAGE
   [: CLOSE-UNSENDABLE ;] catch BAD-CODE !
   sock 1000 WS:CLOSE
   sock 1000 WS:CLOSE
   [: SEND-HELD ;] catch LATE-SEND !
   sock UNTIL-CLOSED ;

: ACCEPT-AGAIN ( -- )
   0 KEPT-REQUEST @ 0 KEPT-RESPONSE @ WS:ACCEPT OPENED drop ;

\ One request is one socket: the second accept is refused and the first serves on.
: TWICE ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   asked 0 KEPT-REQUEST !
   answer 0 KEPT-RESPONSE !
   asked answer WS:ACCEPT OPENED {: sock:WS:socket :}
   [: ACCEPT-AGAIN ;] catch SECOND-ACCEPT !
   sock SERVE ;

\ The socket is handed to the task that pushes. Once the client asks, the
\ handler sends its own frames while that task sends its, and then serves.
: PUSHED ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   PUSH-GO TASK:SIGNAL
   sock ONE-MESSAGE
   BLAST-GO TASK:SIGNAL
   BLAST-N 0 ?do sock HANDLER-BUF SPAN:$ WS:SEND-BINARY loop
   sock SERVE ;

: PUSHER ( -- )
   PUSH-GO TASK:WAIT
   0 HELD @ s" pushed by another task" WS:SEND-TEXT
   BLAST-GO TASK:WAIT
   BLAST-N 0 ?do 0 HELD @ PUSH-BUF SPAN:$ WS:SEND-BINARY loop
   BLAST-N TASK:RETURN ;

: PLAIN ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS!
   answer s" plain" HTTP:BODY! ;

\ The handler keeps its socket for another task and returns, which closes it.
: HOLDER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED 0 HELD ! ;

\ The handler keeps its socket and parks in an AIO wait past every stop bound,
\ so the stop kills its worker there with the socket still open.
: PARKER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED 0 HELD !
   1 KEPT !
   PARK-MS >MS AIO:TIMEOUT AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;

\ The slot's next handler sends through the socket the killed worker kept,
\ before it accepts its own: its own connection is open by then, on
\ descriptors the kernel handed out after the kill closed the old one.
: STALE-ECHO ( HTTP:request HTTP:response -- )
   [: SEND-HELD ;] catch STALE-SEND !
   WS:ACCEPT OPENED SERVE ;

\ An application's exit hook, registered after WS's own and so run before it,
\ that throws while EXIT-FAULT is set.
: FAULTY-EXIT ( -- )
   EXIT-FAULT @ 0 <> if E-EXIT throw then ;

\ Another task's send through the kept socket answers the code it threw: 0
\ when its frame was written.
: HELD-SEND ( -- )
   [: SEND-HELD ;] catch TASK:RETURN ;

\ The handler keeps its socket for another task and receives until it closes,
\ noting when the closed arm answered.
: STALLER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   1 KEPT !
   sock UNTIL-CLOSED
   mono-ns CLOSED-AT ! ;

: PUSH-PATTERN ( -- )
   0 HELD @ PATTERN SPAN:$ WS:SEND-BINARY ;

\ Frames go through the kept socket to a client that reads none until one is
\ refused: the code that refused it.
: REFUSED-PUSH ( -- n )
   begin
      [: PUSH-PATTERN ;] catch {: code:n :}
      code 0 <> if code exit then
   again ;

\ The handler keeps its socket and pushes frames at a client that reads none
\ until a push is refused, and keeps the code that refused it and when.
: FLOODER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED 0 HELD !
   1 KEPT !
   REFUSED-PUSH FLOOD-CODE !
   mono-ns FLOOD-END ! ;

\ The handler keeps its socket for another task and receives until it is told
\ to return, which it does with the socket still open.
: LEAVE-ON-CUE ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   1 KEPT !
   begin
      GO-HOME @ 0 <> if exit then
      sock RECEIVE-MS >MS WS:RECEIVE
      MATCH WS:message
         text OF 2drop ENDOF
         binary OF 2drop ENDOF
         closed OF LAST-CLOSE ! exit ENDOF
         timeout OF ENDOF
      ;MATCH
   again ;

\ The handler waits for each message far past every stop bound, until its
\ socket closes.
: PATIENT ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED
   1 KEPT !
   PARK-MS >MS DROPPED ;

\ The handler sends once the server is stopping, which starts the stop's cut,
\ and lets HTTP:RELEASE-MS pass before it receives until its socket closes.
: LATE-SENDER ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   1 KEPT !
   begin HTTP:STOPPING? 0= while POLL-MS >MS TASK:SLEEP repeat
   sock s" cut" WS:SEND-TEXT
   HTTP:RELEASE-MS >MS TASK:SLEEP
   sock UNTIL-CLOSED ;

: SEND-NOT-UTF8 ( -- )
   0 HELD @ NOT-UTF8 SPAN:$ WS:SEND-TEXT ;

\ The handler's text that is not UTF-8 is refused before a byte of it is
\ written, and then the socket is served.
: MISSPOKEN ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   [: SEND-NOT-UTF8 ;] catch BAD-TEXT !
   sock SERVE ;

\ What a binary message held: its length, and the byte all of it was.
: TOOK ( ptr u8 n -- )
   {: a u:n :}
   a u UNIFORM? if a c@ else -1 then TOOK-BYTE !
   u TOOK-U ! ;

\ The socket is received until it closes, keeping the longest any RECEIVE took
\ and what a binary message held.
: TIMING ( WS:socket -- ) {: sock:WS:socket :}
   begin
      mono-ns {: began:n :}
      sock RECEIVE-MS >MS WS:RECEIVE
      mono-ns began - LONGEST @ max LONGEST !
      MATCH WS:message
         text OF 2drop ENDOF
         binary OF TOOK ENDOF
         closed OF LAST-CLOSE ! exit ENDOF
         timeout OF 1 TIMEOUTS +! ENDOF
      ;MATCH
   again ;

\ The handler keeps its socket for another task and receives it as TIMING does.
: TIMED ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   1 KEPT !
   sock TIMING ;

\ 1 once the client's write of the frame's rest has returned and then bytes
\ wait in the kept socket, unread by any RECEIVE; 0 until then.
: REST-QUEUED ( -- n )
   REST-WRITTEN @ 0= if 0 exit then
   0 HELD @ WS-TEST:QUEUED? if 1 else 0 then ;

\ The handler keeps its socket and notes how many bytes it holds from behind
\ the handshake. Once the rest of the frame is queued it receives with no wait
\ at all, noting whether that answered timeout; then it receives as TIMING
\ does.
: ZERO-FIRST ( HTTP:request HTTP:response -- )
   WS:ACCEPT OPENED {: sock:WS:socket :}
   sock 0 HELD !
   sock WS-TEST:EARLY-HELD EARLY-SEEN !
   1 KEPT !
   [: REST-QUEUED ;] REACH-MS AWAITED 0 <> if 1 REST-SEEN ! then
   sock 0 >MS WS:RECEIVE
   MATCH WS:message
      text OF 2drop ENDOF
      binary OF TOOK ENDOF
      closed OF LAST-CLOSE ! exit ENDOF
      timeout OF 1 ZERO-TIMEOUT ! ENDOF
   ;MATCH
   sock TIMING ;

: INSTALL-ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /patient" [: PATIENT ;] HTTP:ROUTE
   s" GET" s" /timed" [: TIMED ;] HTTP:ROUTE
   s" GET" s" /zero" [: ZERO-FIRST ;] HTTP:ROUTE
   s" GET" s" /echo" [: ECHO ;] HTTP:ROUTE
   s" GET" s" /badtext" [: MISSPOKEN ;] HTTP:ROUTE
   s" POST" s" /echo" [: ECHO ;] HTTP:ROUTE
   s" GET" s" /leave" [: LEAVER ;] HTTP:ROUTE
   s" GET" s" /throw" [: THROWER ;] HTTP:ROUTE
   s" GET" s" /close" [: CLOSER ;] HTTP:ROUTE
   s" GET" s" /twice" [: TWICE ;] HTTP:ROUTE
   s" GET" s" /push" [: PUSHED ;] HTTP:ROUTE
   s" GET" s" /plain" [: PLAIN ;] HTTP:ROUTE
   s" GET" s" /hold" [: HOLDER ;] HTTP:ROUTE
   s" GET" s" /park" [: PARKER ;] HTTP:ROUTE
   s" GET" s" /stale" [: STALE-ECHO ;] HTTP:ROUTE
   s" GET" s" /stall" [: STALLER ;] HTTP:ROUTE
   s" GET" s" /flood" [: FLOODER ;] HTTP:ROUTE
   s" GET" s" /goodbye" [: LEAVE-ON-CUE ;] HTTP:ROUTE
   s" GET" s" /late" [: LATE-SENDER ;] HTTP:ROUTE ;


\ ---- the transcript ----------------------------------------------------------

: SCRIPT$ ( -- ptr u8 n )
   SCRIPT-BUF SCRIPT-U @ SPAN:TAKE SPAN:$ ;

: LOG ( ptr u8 n -- ) {: a u:n :}
   a u SCRIPT-BUF SCRIPT-U @ SPAN:SKIP SPAN:COPY
   u SCRIPT-U +! ;

: LOG-C ( n -- ) {: c:n :}
   c SCRIPT-BUF SCRIPT-U @ SPAN:U8!
   1 SCRIPT-U +! ;

: LOG-LINE ( ptr u8 n -- )
   LOG LF LOG-C ;

: LOG-N ( n -- ) {: v:n :}
   v 10 >= if v 10 / RECURSE then
   v 10 mod [char] 0 + LOG-C ;

: LOG-HEX ( ptr u8 n -- ) {: a u:n :}
   u 0 ?do
      a i + c@ HEX-PAIR SPAN:$ drop BYTE>HEX
      HEX-PAIR SPAN:$ LOG
   loop ;

: BYTE-SUM ( ptr u8 n -- n ) {: a u:n :}
   0 u 0 ?do a i + c@ + loop ;

: LOG-PAYLOAD ( ptr u8 n -- ) {: a u:n :}
   u 0= if exit then
   SP LOG-C
   u HEX-MAX <= if a u LOG-HEX exit then
   s" sum " LOG a u BYTE-SUM LOG-N ;

: OP$ ( WS:opcode -- ptr u8 n )
   MATCH WS:opcode
      continuation OF s" continuation" ENDOF
      text OF s" text" ENDOF
      binary OF s" binary" ENDOF
      close OF s" close" ENDOF
      ping OF s" ping" ENDOF
      pong OF s" pong" ENDOF
   ;MATCH ;

: LOG-FRAME ( ptr u8 n bool WS:opcode ptr u8 n -- ) {: dir du:n fin:bool op a u:n :}
   dir du LOG op OP$ LOG
   fin 0= if [char] + LOG-C then
   SP LOG-C u LOG-N a u LOG-PAYLOAD LF LOG-C ;


\ ---- the client --------------------------------------------------------------

: DROP-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;

: DIAL ( -- TCP4:connection )
   0 STREAM-AT ! 0 STREAM-U !
   LOOPBACK TCP4:ADDRESS HTTP:PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF ENDOF
      failed OF drop E-DIAL throw ENDOF
   ;MATCH ;

: SEND-RAW ( TCP4:connection ptr u8 n -- ) {: conn:TCP4:connection a u:n :}
   conn a u TCP4:TRANSFER-BYTES TCP4:WRITE DROP-STATUS ;

\ A server that says nothing for ANSWER-MS fails its case by name instead of
\ hanging the suite.
: AWAIT ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn ANSWER-MS >MS TCP4:READABLE-WITHIN?
   MATCH TCP4:ready-result
      ready OF ENDOF
      idle OF E-SILENT throw ENDOF
      failed OF drop ENDOF
   ;MATCH ;

\ One read of whatever the server has sent; nothing at the end of its stream.
: REFILL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn AWAIT
   conn STREAM-BUF SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF drop 0 ENDOF
      failed OF drop 0 ENDOF
   ;MATCH
   STREAM-U !
   0 STREAM-AT ! ;

\ Bytes into the span from what the client holds, read afresh when it holds
\ none: their count, 0 at the end of the stream.
: READ-SOME ( TCP4:connection SPAN:span<u8> -- n ) {: conn:TCP4:connection dst :}
   STREAM-AT @ STREAM-U @ >= if conn REFILL then
   STREAM-U @ STREAM-AT @ - dst SPAN:LEN min {: take:n :}
   STREAM-BUF STREAM-AT @ take SPAN:SUB SPAN:$ dst SPAN:COPY
   take STREAM-AT +!
   take ;

\ The whole span, or false when the stream ended first.
: READ-N ( TCP4:connection SPAN:span<u8> -- bool ) {: conn:TCP4:connection dst :}
   dst SPAN:LEN 0= if true exit then
   conn dst READ-SOME {: got:n :}
   got 0= if false exit then
   conn dst got SPAN:SKIP RECURSE ;

: HEAD$ ( -- ptr u8 n )
   HEAD-BUF HEAD-U @ SPAN:TAKE SPAN:$ ;

\ The response head and not a byte past it: frames may follow at once.
: READ-HEAD ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 HEAD-U !
   begin
      HEAD$ S\" \r\n\r\n" ENDS-WITH? if exit then
      conn HEAD-BUF HEAD-U @ 1 SPAN:SUB READ-N 0= if exit then
      1 HEAD-U +!
   again ;

: REQ$ ( -- ptr u8 n )
   REQ-BUF REQ-U @ SPAN:TAKE SPAN:$ ;

: REQ+ ( ptr u8 n -- ) {: a u:n :}
   a u REQ-BUF REQ-U @ SPAN:SKIP SPAN:COPY
   u REQ-U +! ;

: LINE+ ( ptr u8 n ptr u8 n -- ) {: name nu:n value vu:n :}
   name nu REQ+ s" : " REQ+ value vu REQ+ S\" \r\n" REQ+ ;

\ A request line naming that version, with no header behind it.
: REQ-BARE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: method mu:n path pu:n version vu:n :}
   0 REQ-U !
   method mu REQ+ s"  " REQ+ path pu REQ+ s"  " REQ+ version vu REQ+ S\" \r\n" REQ+ ;

: HOST+ ( -- )
   s" Host" s" ws-test" LINE+ ;

\ An HTTP/1.1 request line and its Host.
: REQ-START ( ptr u8 n ptr u8 n -- ) {: method mu:n path pu:n :}
   method mu path pu s" HTTP/1.1" REQ-BARE
   HOST+ ;

: KEY$ ( -- ptr u8 n )
   s" dGhlIHNhbXBsZSBub25jZQ==" ;

\ The four lines of a handshake with nothing wrong with it.
: GOOD-LINES ( -- )
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+ ;

\ The request as built goes out with whatever a case put behind its head, and
\ the answer's head is read.
: REQ-SEND ( -- TCP4:connection )
   DIAL {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   conn READ-HEAD
   conn ;

: UPGRADE ( ptr u8 n -- TCP4:connection ) {: path pu:n :}
   s" GET" path pu REQ-START GOOD-LINES S\" \r\n" REQ+
   REQ-SEND
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE ;

\ One masked frame as a client sends it, in the client's own buffer.
: FRAME$ ( bool WS:opcode ptr u8 n -- ptr u8 n ) {: fin:bool op a u:n :}
   fin op u MASK-KEY WS-HEADER:MAKE WS-SENDER:client CLIENT-TX WS:ENCODE-HEADER {: size:n :}
   a u CLIENT-TX size SPAN:SKIP SPAN:COPY
   MASK-KEY CLIENT-TX size u SPAN:SUB WS:MASK
   CLIENT-TX size u + SPAN:TAKE SPAN:$ ;

: SAY ( TCP4:connection bool WS:opcode ptr u8 n -- )
   {: conn:TCP4:connection fin:bool op a u:n :}
   s" > " fin op a u LOG-FRAME
   conn fin op a u FRAME$ SEND-RAW ;

: NIBBLE ( n -- n ) {: c:n :}
   c [char] a >= if c [char] a - 10 + exit then
   c [char] 0 - ;

\ Bytes written in hex, two lowercase digits each.
: BYTES ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   u 2 / {: k:n :}
   k 0 ?do
      a i 2 * + c@ NIBBLE 4 lshift a i 2 * + 1+ c@ NIBBLE or $FF and
      RAW-BUF i SPAN:U8!
   loop
   RAW-BUF k SPAN:TAKE SPAN:$ ;

\ Bytes no well-behaved client frames, as they are written.
: SAY-RAW ( TCP4:connection ptr u8 n -- ) {: conn:TCP4:connection a u:n :}
   s" > raw " LOG a u LOG-LINE
   conn a u BYTES SEND-RAW ;

: STATUS$ ( n -- ptr u8 n ) {: code:n :}
   code 8 rshift $FF and STATUS-BUF 0 SPAN:U8!
   code $FF and STATUS-BUF 1 SPAN:U8!
   STATUS-BUF SPAN:$ ;

: KEEP-HEADER ( WS:header n -- bool ) {: h size:n :}
   h WS-HEADER:UNMAKE {: fin:bool op len:n key:n :}
   fin if 1 else 0 then RX-FIN !
   op RX-OP !
   len RX-LEN !
   true ;

\ True once the bytes held are a whole header; until then the count it needs.
: HEADER? ( n -- n bool ) {: held:n :}
   RX-HEAD SPAN:$ drop held WS-SENDER:server BUF-CAP WS:DECODE-HEADER
   MATCH WS:decoded
      need OF false ENDOF
      frame OF KEEP-HEADER held swap ENDOF
   ;MATCH ;

: READ-HEADER ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn RX-HEAD 2 SPAN:TAKE READ-N 0= if false exit then
   2 HEADER? if drop true exit then {: size:n :}
   conn RX-HEAD 2 size 2 - SPAN:SUB READ-N 0= if false exit then
   size HEADER? nip ;

\ The next frame the server sent, or the end of its stream.
: NEXT-FRAME ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 RX-EOF ! 0 RX-LEN !
   conn READ-HEADER 0= if 1 RX-EOF ! exit then
   conn CLIENT-RX RX-LEN @ SPAN:TAKE READ-N 0= if 1 RX-EOF ! then ;

: RX$ ( -- ptr u8 n )
   CLIENT-RX RX-LEN @ SPAN:TAKE SPAN:$ ;

: HEAR ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn NEXT-FRAME
   RX-EOF @ 0 <> if s" < end of stream" LOG-LINE exit then
   s" < " RX-FIN @ 0 <> RX-OP @ RX$ LOG-FRAME ;

\ The frame in hand is final, of this opcode, with this payload.
: GOT ( WS:opcode ptr u8 n -- ) {: op want wu:n :}
   RX-EOF @ 0 T=
   RX-FIN @ 1 T=
   RX-OP @ OP$ op OP$ T$=
   RX$ want wu T$= ;

: GOT-CLOSE ( n -- ) {: code:n :}
   WS-OPCODE:close code STATUS$ GOT ;

: GOT-END ( -- )
   RX-EOF @ 1 T= ;

\ The server ends the stream, which it does once the handler has returned.
: ENDS ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn HEAR GOT-END
   conn TCP4:CLOSE DROP-STATUS ;

: EXCHANGE ( TCP4:connection WS:opcode ptr u8 n -- ) {: conn:TCP4:connection op a u:n :}
   conn true op a u SAY
   conn HEAR op a u GOT ;

\ The close handshake a client starts, then the end of the stream.
: FAREWELL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn true WS-OPCODE:close 1000 STATUS$ SAY
   conn HEAR 1000 GOT-CLOSE
   conn ENDS ;

\ The server fails the connection with this status and ends the stream.
: FAILED ( TCP4:connection n -- ) {: conn:TCP4:connection code:n :}
   conn HEAR code GOT-CLOSE
   conn ENDS
   LAST-CLOSE @ code T= ;

: SECTION ( ptr u8 n -- ) {: a u:n :}
   a u T-LABEL
   s" == " LOG a u LOG-LINE ;


\ ---- the cases ---------------------------------------------------------------

\ The head as it arrived, a line to a line.
: LOG-HEAD ( -- )
   HEAD$ {: a u:n :}
   u 0 ?do a i + c@ dup CR = if drop else LOG-C then loop ;

: FIRST-LINE ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   u 0 ?do a i + c@ CR = if a i unloop exit then loop
   a u ;

\ RFC 6455 section 1.3's key and the accept value it names. A client that ends
\ its stream with no close frame is reported as status 1006.
: HANDSHAKE-CASE ( -- )
   s" handshake" SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   HEAD$ S\" \r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" CONTAINS? TTRUE
   HEAD$ S\" \r\nUpgrade: websocket\r\n" CONTAINS? TTRUE
   HEAD$ S\" \r\nConnection: Upgrade\r\n" CONTAINS? TTRUE
   HEAD$ s" Content-Length" CONTAINS? TFALSE
   LOG-HEAD
   0 LAST-CLOSE !
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   conn ENDS
   LAST-CLOSE @ 1006 T= ;

\ The status line a refused handshake is answered with, and the status the
\ handler was told it was refused with.
: REFUSED ( ptr u8 n ptr u8 n n -- ) {: label lu:n want wu:n status:n :}
   label lu T-LABEL
   0 REFUSED-WITH !
   S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   HEAD$ want wu STARTS-WITH? TTRUE
   label lu LOG s" : " LOG HEAD$ FIRST-LINE LOG-LINE
   conn TCP4:CLOSE DROP-STATUS
   REFUSED-WITH @ status T= ;

: BAD$ ( -- ptr u8 n )
   s" HTTP/1.1 400 Bad Request" ;

\ Section 4.2.1's first two items: the handshake is an HTTP/1.1 GET carrying
\ one valid Host. The server refuses a request without one before any handler
\ runs, so no handler sees that status.
: REQUEST-REFUSALS ( -- )
   s" POST" s" /echo" REQ-START GOOD-LINES
   s" POST" BAD$ 400 REFUSED
   s" GET" s" /echo" s" HTTP/1.0" REQ-BARE HOST+ GOOD-LINES
   s" HTTP/1.0" BAD$ 400 REFUSED
   s" GET" s" /echo" s" HTTP/1.1" REQ-BARE GOOD-LINES
   s" no Host" BAD$ 0 REFUSED
   s" GET" s" /echo" REQ-START HOST+ GOOD-LINES
   s" two Hosts" BAD$ 0 REFUSED
   s" GET" s" /echo" s" HTTP/1.1" REQ-BARE s" Host" s" " LINE+ GOOD-LINES
   s" an empty Host" BAD$ 0 REFUSED
   s" GET" s" /echo" s" HTTP/1.1" REQ-BARE s" Host" s" a b" LINE+ GOOD-LINES
   s" a Host with a space" BAD$ 0 REFUSED ;

: REFUSAL-CASES ( -- )
   s" refused handshakes" SECTION
   s" GET" s" /echo" REQ-START
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" no Upgrade" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" h2c" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" Upgrade to h2c" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" close" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" Connection without Upgrade" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" no key" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" s" dGhlIHNhbXBsZSBub25jZQ=" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" a key of 23 characters" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" s" dGhlIHNhbXBsZSBub25jZQAA" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" a key of 18 bytes" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" s" dGhlIHNhbXBsZSBub25jZ!==" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" a key outside base64" BAD$ 400 REFUSED
   s" GET" s" /echo" REQ-START GOOD-LINES
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" two keys" BAD$ 400 REFUSED
   REQUEST-REFUSALS
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" no version" s" HTTP/1.1 426 Upgrade Required" 426 REFUSED
   s" GET" s" /echo" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 12" LINE+
   s" version 12" s" HTTP/1.1 426 Upgrade Required" 426 REFUSED
   s" a 426 names the version and the protocol" T-LABEL
   HEAD$ S\" \r\nSec-WebSocket-Version: 13\r\n" CONTAINS? TTRUE
   HEAD$ S\" \r\nUpgrade: websocket\r\n" CONTAINS? TTRUE ;

\ Header names and the two tokens are matched whatever their case, and the
\ Connection header is a list.
: TOLERANT-CASE ( -- )
   s" a handshake in another spelling" SECTION
   s" GET" s" /echo" REQ-START
   s" upgrade" s" WebSocket" LINE+
   s" connection" s" keep-alive, Upgrade" LINE+
   s" sec-websocket-key" KEY$ LINE+
   s" sec-websocket-version" s" 13" LINE+
   S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL ;

: ECHO-CASES ( -- )
   s" echo text" SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn WS-OPCODE:text s" Hello" EXCHANGE
   conn WS-OPCODE:text s" Hello" drop 0 EXCHANGE
   conn WS-OPCODE:text s" 68c3a96c6c6f20e282ac" BYTES EXCHANGE
   0 LAST-CLOSE !
   conn FAREWELL
   LAST-CLOSE @ 1000 T= ;

\ The handler's text that is not UTF-8 is refused before a byte of it goes
\ out: the first frame the client hears is the echo of its own.
: BAD-TEXT-CASE ( -- )
   s" text the server may not send" SECTION
   0 BAD-TEXT !
   s" /badtext" UPGRADE {: conn:TCP4:connection :}
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL
   s" text that is not UTF-8 is refused" T-LABEL
   BAD-TEXT @ E-WS-TEXT T= ;

: PATTERN$ ( n -- ptr u8 n )
   PATTERN swap SPAN:TAKE SPAN:$ ;

\ Each length form at its edges: 7 bits to 125, 16 bits from 126 to 65535, and
\ 64 bits past that.
: LENGTH-CASES ( -- )
   s" echo binary in each length form" SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn WS-OPCODE:binary 125 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary 126 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary 65535 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary 65536 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary BIG PATTERN$ EXCHANGE
   conn FAREWELL ;

\ A message in fragments comes back as one frame: control frames pass between
\ the fragments, a fragment may be empty, and a scalar may straddle two.
: FRAGMENT-CASES ( -- )
   s" fragments" SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn false WS-OPCODE:text s" Hel" SAY
   conn false WS-OPCODE:continuation s" lo," SAY
   conn true WS-OPCODE:continuation s"  world" SAY
   conn HEAR WS-OPCODE:text s" Hello, world" GOT
   conn false WS-OPCODE:binary s" 0102" BYTES SAY
   conn true WS-OPCODE:ping s" !" SAY
   conn HEAR WS-OPCODE:pong s" !" GOT
   conn true WS-OPCODE:continuation s" 0304" BYTES SAY
   conn HEAR WS-OPCODE:binary s" 01020304" BYTES GOT
   conn false WS-OPCODE:text s" c3" BYTES SAY
   conn true WS-OPCODE:continuation s" a9" BYTES SAY
   conn HEAR WS-OPCODE:text s" c3a9" BYTES GOT
   conn false WS-OPCODE:text s" hi" SAY
   conn true WS-OPCODE:continuation s" hi" drop 0 SAY
   conn HEAR WS-OPCODE:text s" hi" GOT
   conn FAREWELL ;

\ A ping is answered with its own payload, to the 125 bytes a control frame may
\ carry, and a pong nobody asked for is let pass.
: PING-CASES ( -- )
   s" ping and pong" SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn true WS-OPCODE:ping s" hb" SAY
   conn HEAR WS-OPCODE:pong s" hb" GOT
   conn true WS-OPCODE:ping s" hb" drop 0 SAY
   conn HEAR WS-OPCODE:pong s" hb" drop 0 GOT
   conn true WS-OPCODE:ping 125 PATTERN$ SAY
   conn HEAR WS-OPCODE:pong 125 PATTERN$ GOT
   conn true WS-OPCODE:pong s" x" SAY
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL ;

\ The client says nothing until the handler's wait has run out twice more, so
\ one whole wait began after the client's last byte. False when the handler
\ never times out.
: QUIET? ( -- bool )
   TIMEOUTS @ 2 + {: goal:n :}
   mono-ns ANSWER-MS NS-PER-MS * + {: deadline:n :}
   begin
      TIMEOUTS @ goal >= if true exit then
      mono-ns deadline > if false exit then
      POLL-MS >MS TASK:SLEEP
   again ;

\ One frame in two writes with the handler's wait running out between them:
\ RECEIVE answers timeout and takes the frame up where it stopped.
: CUT ( TCP4:connection ptr u8 n n -- ) {: conn:TCP4:connection a u:n at:n :}
   s" > text " LOG u LOG-N s"  cut after byte " LOG at LOG-N LF LOG-C
   true WS-OPCODE:text a u FRAME$ {: f fu:n :}
   conn f at SEND-RAW
   QUIET? TTRUE
   conn f at + fu at - SEND-RAW
   conn HEAR WS-OPCODE:text a u GOT ;

: TIMEOUT-CASE ( -- )
   s" timeouts" SECTION
   0 TIMEOUTS !
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   s" a quiet client is a timeout" T-LABEL
   QUIET? TTRUE
   s" a header cut in two" T-LABEL
   conn s" resumed" 1 CUT
   conn s" resumed" 3 CUT
   s" a payload cut in two" T-LABEL
   conn s" resumed" 9 CUT
   conn FAREWELL ;

\ A client that does not wait for the 101: its first frame rides behind the
\ request, where the HTTP server has already read it.
: EARLY-CASE ( -- )
   s" a frame behind the handshake" SECTION
   s" GET" s" /echo" REQ-START GOOD-LINES S\" \r\n" REQ+
   s" > " true WS-OPCODE:text s" early" LOG-FRAME
   true WS-OPCODE:text s" early" FRAME$ REQ+
   REQ-SEND {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE
   conn HEAR WS-OPCODE:text s" early" GOT
   conn FAREWELL ;

\ Raw bytes on a fresh connection, and the status the server fails it with.
: BREAKS ( ptr u8 n ptr u8 n n -- ) {: label lu:n a u:n code:n :}
   label lu SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn a u SAY-RAW
   conn code FAILED ;

\ What the codec refuses in a header, each from the fewest bytes that show it.
: HEADER-FAULTS ( -- )
   s" an unmasked frame" s" 810548656c6c6f" 1002 BREAKS
   s" a control frame past 125 bytes" s" 89fe" 1002 BREAKS
   s" a fragmented control frame" s" 0980" 1002 BREAKS
   s" a reserved bit" s" c180" 1002 BREAKS
   s" a reserved opcode" s" 8380" 1002 BREAKS
   s" a length in a longer form than it needs" s" 82fe007d" 1002 BREAKS
   s" a message one byte past the bound" s" 82ff0000000000100001" 1009 BREAKS ;

\ What only the connection can see: the order of the frames and what they say.
: MESSAGE-FAULTS ( -- )
   s" a continuation of nothing" s" 808037fa213d" 1002 BREAKS
   s" a new message inside another" SECTION
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn false WS-OPCODE:text s" a" SAY
   conn true WS-OPCODE:text s" b" SAY
   conn 1002 FAILED
   s" text that is not UTF-8" SECTION
   s" /echo" UPGRADE {: bad:TCP4:connection :}
   bad true WS-OPCODE:text s" c328" BYTES SAY
   bad 1007 FAILED
   s" fragments that are not UTF-8 together" SECTION
   s" /echo" UPGRADE {: split:TCP4:connection :}
   split false WS-OPCODE:text s" c3" BYTES SAY
   split true WS-OPCODE:continuation s" 28" BYTES SAY
   split 1007 FAILED
   s" fragments one byte past the bound" SECTION
   s" /echo" UPGRADE {: long:TCP4:connection :}
   long false WS-OPCODE:binary BIG PATTERN$ SAY
   long s" 80ff00000000000eee9137fa213d" SAY-RAW
   long 1009 FAILED ;

\ A frame whose header is accepted and whose payload never comes: the client
\ ends its stream, so the handler sees status 1006 where a refused length would
\ have answered 1009.
: BOUND-CASES ( -- )
   s" a message of exactly the bound" SECTION
   0 LAST-CLOSE !
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn s" 82ff000000000010000037fa213d" SAY-RAW
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   conn ENDS
   LAST-CLOSE @ 1006 T=
   s" fragments of exactly the bound" SECTION
   0 LAST-CLOSE !
   s" /echo" UPGRADE {: long:TCP4:connection :}
   long false WS-OPCODE:binary BIG PATTERN$ SAY
   long s" 80ff00000000000eee9037fa213d" SAY-RAW
   long TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   long ENDS
   LAST-CLOSE @ 1006 T= ;

\ A close frame's status comes back as it was sent when a frame may carry it,
\ and is a protocol error when none may.
: CLOSES-WITH ( n n -- ) {: code:n answer:n :}
   0 LAST-CLOSE !
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn true WS-OPCODE:close code STATUS$ SAY
   conn answer FAILED ;

: CLOSE-STATUS-CASES ( -- )
   s" close statuses a frame may carry" SECTION
   1000 1000 CLOSES-WITH
   1003 1003 CLOSES-WITH
   1007 1007 CLOSES-WITH
   1014 1014 CLOSES-WITH
   3000 3000 CLOSES-WITH
   4999 4999 CLOSES-WITH
   s" close statuses no frame may carry" SECTION
   999 1002 CLOSES-WITH
   1004 1002 CLOSES-WITH
   1005 1002 CLOSES-WITH
   1006 1002 CLOSES-WITH
   1015 1002 CLOSES-WITH
   2999 1002 CLOSES-WITH
   5000 1002 CLOSES-WITH ;

: CLOSE-CASES ( -- )
   s" a close with no status" SECTION
   0 LAST-CLOSE !
   s" /echo" UPGRADE {: bare:TCP4:connection :}
   bare true WS-OPCODE:close s" x" drop 0 SAY
   bare HEAR WS-OPCODE:close s" x" drop 0 GOT
   bare ENDS
   LAST-CLOSE @ 1005 T=
   s" a close with a reason" SECTION
   0 LAST-CLOSE !
   s" /echo" UPGRADE {: reason:TCP4:connection :}
   reason true WS-OPCODE:close s" 03e9627965" BYTES SAY
   reason 1001 FAILED
   s" a close with half a status" SECTION
   s" /echo" UPGRADE {: half:TCP4:connection :}
   half true WS-OPCODE:close s" 03" BYTES SAY
   half 1002 FAILED
   s" a close whose reason is not UTF-8" SECTION
   s" /echo" UPGRADE {: garbled:TCP4:connection :}
   garbled true WS-OPCODE:close s" 03e8c328" BYTES SAY
   garbled 1007 FAILED ;

\ The handler closes: a ping the client sends before it answers the close is
\ still answered (section 5.5.2), then the client answers the close and the
\ stream ends.
: SERVER-CLOSE-CASE ( -- )
   s" the server closes first" SECTION
   0 LAST-CLOSE ! 0 BAD-CODE ! 0 LATE-SEND !
   s" /close" UPGRADE {: conn:TCP4:connection :}
   conn true WS-OPCODE:text s" go" SAY
   conn HEAR 1000 GOT-CLOSE
   conn true WS-OPCODE:ping s" x" SAY
   conn HEAR WS-OPCODE:pong s" x" GOT
   conn true WS-OPCODE:close 1000 STATUS$ SAY
   conn ENDS
   s" the handler saw the client's close" T-LABEL
   LAST-CLOSE @ 1000 T=
   s" status 1005 is not one to send" T-LABEL
   BAD-CODE @ E-WS-CODE T=
   s" nothing is sent behind a close" T-LABEL
   LATE-SEND @ E-WS-CLOSED T= ;

\ A handler that returns with its socket open closes it normally; one that
\ throws closes it as an internal error.
: RETURN-CASES ( -- )
   s" the handler returns" SECTION
   s" /leave" UPGRADE {: conn:TCP4:connection :}
   conn true WS-OPCODE:text s" go" SAY
   conn HEAR 1000 GOT-CLOSE
   conn ENDS
   s" the handler throws" SECTION
   s" /throw" UPGRADE {: thrown:TCP4:connection :}
   thrown true WS-OPCODE:text s" go" SAY
   thrown HEAR 1011 GOT-CLOSE
   thrown ENDS ;

: TWICE-CASE ( -- )
   s" one request accepted twice" SECTION
   0 SECOND-ACCEPT !
   s" /twice" UPGRADE {: conn:TCP4:connection :}
   conn WS-OPCODE:text s" still here" EXCHANGE
   SECOND-ACCEPT @ HTTP:E-STATE T=
   conn FAREWELL ;

: DROP-MESSAGE ( WS:message -- )
   MATCH WS:message
      text OF 2drop ENDOF
      binary OF 2drop ENDOF
      closed OF drop ENDOF
      timeout OF ENDOF
   ;MATCH ;

: RECEIVE-ELSEWHERE ( -- )
   0 HELD @ 0 >MS WS:RECEIVE DROP-MESSAGE ;

: RECEIVE-BACKWARDS ( -- )
   0 HELD @ -1 >MS WS:RECEIVE DROP-MESSAGE ;

\ The frame in hand is one the pushing task sent or one the handler sent, whole.
: TALLY ( -- )
   RX$ {: a u:n :}
   RX-EOF @ 0 <> u BLAST-BYTES <> or if 1 TORN +! exit then
   a u UNIFORM? 0= if 1 TORN +! exit then
   a c@ PUSH-BYTE = if 1 PUSHED-N +! exit then
   a c@ HANDLER-BYTE = if 1 HANDLER-N +! exit then
   1 TORN +! ;

: JOINED ( ptr n -- n )
   TASK:JOIN
   MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

\ The burst: BURST-PINGS empty pings, as the client sends them. Every byte of
\ it fits the loopback buffers, so all of it is queued at the server at once.
: VOLLEY-FILL ( -- )
   true WS-OPCODE:ping PATTERN SPAN:$ drop 0 FRAME$ {: fa fu:n :}
   BURST-PINGS 0 ?do fa fu VOLLEY-BUF i fu * SPAN:SKIP SPAN:COPY loop
   BURST-PINGS fu * VOLLEY-U ! ;

\ The client task writes the whole burst in one write and answers how many
\ pings it wrote.
: VOLLEY ( -- )
   0 PEER @ VOLLEY-BUF VOLLEY-U @ SPAN:TAKE SPAN:$ TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF BURST-PINGS ENDOF
      failed OF drop 0 ENDOF
   ;MATCH
   TASK:RETURN ;

\ The frame in hand is a whole pong to one of the burst's pings.
: PONG-IN ( -- )
   RX-EOF @ 0 <> if exit then
   RX-OP @ OP$ s" pong" STR= RX-LEN @ 0= and if 1 PONGS +! then ;

\ The burst's pongs, read as they come. From the first on, the count read
\ before the handler is first seen to have answered a timeout is kept.
: DRAIN ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 PONGS ! 0 LATE-AT !
   conn NEXT-FRAME PONG-IN
   TIMEOUTS @ {: seen:n :}
   BURST-PINGS 1 ?do
      LATE-AT @ 0= TIMEOUTS @ seen > and if PONGS @ LATE-AT ! then
      conn NEXT-FRAME PONG-IN
   loop ;

\ A client task sends BURST-PINGS pings in one write, keeping bytes queued far
\ longer than a RECEIVE waits, while the main thread reads the pongs. The wait
\ is absolute: the handler is answered timeout while the burst is still being
\ answered, and no RECEIVE takes half as long as the burst, as one that read on
\ while bytes were queued would. A RECEIVE's length is kept when it returns,
\ so the handler is let time out again before the longest is read.
: BURST-CASE ( -- )
   s" a burst the peer keeps queued" SECTION
   0 TIMEOUTS ! 0 LONGEST !
   s" /timed" UPGRADE {: conn:TCP4:connection :}
   QUIET? TTRUE
   conn 0 PEER !
   VOLLEY-FILL
   mono-ns {: began:n :}
   [: VOLLEY ;] VOLLEY-TASK TASK:ACTIVATE
   conn DRAIN
   mono-ns began - {: took:n :}
   s" the handler waits again once the burst is answered" T-LABEL
   QUIET? TTRUE
   s" every ping of the burst was answered" T-LABEL
   VOLLEY-TASK JOINED BURST-PINGS T=
   PONGS @ BURST-PINGS T=
   s" the handler timed out while the burst was answered" T-LABEL
   LATE-AT @ 0 > TTRUE
   s" and no wait took half as long as the burst" T-LABEL
   LONGEST @ 2 * took < TTRUE
   conn FAREWELL ;

\ Whether the handler held the frame's header and first bytes, that many in
\ all, behind the handshake and then found the rest queued in its socket: each
\ a case of its own.
: REST-HELD? ( n -- bool )
   {: lead:n :}
   s" its header and first bytes were held behind the handshake" T-LABEL
   EARLY-SEEN @ lead T=
   s" and all of the rest waited in the socket" T-LABEL
   REST-SEEN @ 0 <> TTRUE
   EARLY-SEEN @ lead = REST-SEEN @ 0 <> and ;

\ A binary frame whose header and first LEAD-BYTES ride behind the handshake,
\ where the HTTP server has already read them, and whose other REST-BYTES the
\ client writes once the 101 is in. The case checks that, before its RECEIVE
\ that waits not at all, the handler held all of those first bytes in its
\ slot, so none of the frame waited in the socket, and then found bytes queued
\ there once the client's write of the rest had returned. On loopback that
\ write, far under a segment, is sent as one segment, which the receiving
\ stack queues whole, so those bytes are all of the rest. The RECEIVE then
\ takes the bytes in hand, asks the socket nothing more and answers timeout,
\ keeping them, though the rest of the frame is ready; the next takes the
\ rest, and the message arrives whole. The client closes once the handler has
\ found the rest, or REACH-MS after its write: a handler that missed it looked
\ from its 101 on, for REACH-MS, so it answers the close inside ANSWER-MS.
: REST-CASE ( -- )
   s" a frame whose rest waits in the socket" SECTION
   0 KEPT ! 0 TOOK-U ! 0 TOOK-BYTE ! 0 ZERO-TIMEOUT !
   0 EARLY-SEEN ! 0 REST-WRITTEN ! 0 REST-SEEN !
   s" GET" s" /zero" REQ-START GOOD-LINES S\" \r\n" REQ+
   s" > " true WS-OPCODE:binary REST-BUF SPAN:$ LOG-FRAME
   true WS-OPCODE:binary REST-BUF SPAN:$ FRAME$ {: f fu:n :}
   fu REST-BYTES - {: lead:n :}
   f lead REQ+
   REQ-SEND {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE
   conn f lead + REST-BYTES SEND-RAW
   1 REST-WRITTEN !
   [: REST-SEEN @ ;] REACH-MS AWAITED drop
   conn FAREWELL
   lead REST-HELD? if
      s" and a RECEIVE that waits not at all answered timeout" T-LABEL
      ZERO-TIMEOUT @ 0 <> TTRUE
   then
   s" the message arrived whole" T-LABEL
   TOOK-U @ REST-FRAME-BYTES T=
   TOOK-BYTE @ REST-BYTE T= ;

\ A second task pushes through the handler's socket, and then both send at
\ once. Each of their frames is two writes, a header and its payload, which
\ another task's frame would land between if sends were not serialised; every
\ frame the client reads is whole.
: PUSH-CASE ( -- )
   s" a push from a second task" SECTION
   0 PUSHED-N ! 0 HANDLER-N ! 0 TORN !
   [: PUSHER ;] PUSH-TASK TASK:ACTIVATE
   s" /push" UPGRADE {: conn:TCP4:connection :}
   conn HEAR WS-OPCODE:text s" pushed by another task" GOT
   s" only its worker receives on a socket" T-LABEL
   [: RECEIVE-ELSEWHERE ;] E-WS-SOCKET TTHROWSQ
   s" and nobody waits less than no time" T-LABEL
   [: RECEIVE-BACKWARDS ;] E-WS-WAIT TTHROWSQ
   conn true WS-OPCODE:text s" go" SAY
   BLAST-N 2 * 0 ?do conn NEXT-FRAME TALLY loop
   s" every frame of both tasks arrived whole" T-LABEL
   PUSHED-N @ BLAST-N T=
   HANDLER-N @ BLAST-N T=
   TORN @ 0 T=
   s" < " LOG PUSHED-N @ LOG-N s"  pushed frames and " LOG HANDLER-N @ LOG-N
   s"  of the handler's, " LOG TORN @ LOG-N s"  torn" LOG-LINE
   s" the pushing task sent every frame" T-LABEL
   PUSH-TASK JOINED BLAST-N T=
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL
   s" a push past the handler is refused" T-LABEL
   [: SEND-HELD ;] E-WS-CLOSED TTHROWSQ
   [: RECEIVE-ELSEWHERE ;] E-WS-SOCKET TTHROWSQ
   s" and a close past it is nothing new" T-LABEL
   0 HELD @ 1000 WS:CLOSE ;

: PLAIN-ONCE ( -- )
   s" GET" s" /plain" REQ-START
   s" Connection" s" close" LINE+
   S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 200 OK" STARTS-WITH? TTRUE
   conn TCP4:CLOSE DROP-STATUS ;

\ The worker that served a socket serves plain requests after it.
: PLAIN-CASE ( -- )
   s" plain HTTP after the sockets" SECTION
   WORKERS 2 * 0 ?do PLAIN-ONCE loop ;

\ True once the case's handler has kept its socket, false past ANSWER-MS.
: KEPT? ( -- bool )
   mono-ns ANSWER-MS NS-PER-MS * + {: deadline:n :}
   begin
      KEPT @ 0 <> if true exit then
      mono-ns deadline > if false exit then
      POLL-MS >MS TASK:SLEEP
   again ;

\ A handle kept past its handler names nothing once the slot serves the next
\ socket: a send through it is refused, and the socket in the slot hears only
\ its own frames.
: HELD-CASE ( -- )
   s" a handle kept past its socket" SECTION
   s" /hold" UPGRADE {: held:TCP4:connection :}
   held HEAR 1000 GOT-CLOSE
   held ENDS
   s" /echo" UPGRADE {: conn:TCP4:connection :}
   conn WS-OPCODE:text s" mine" EXCHANGE
   s" a send through it is refused while the slot serves another" T-LABEL
   [: SEND-HELD ;] E-WS-CLOSED TTHROWSQ
   conn WS-OPCODE:text s" only mine" EXCHANGE
   conn FAREWELL ;

\ A stop kills the worker parked with its socket open, and an application's
\ exit hook that runs before WS's own throws in that worker's exit. The peer
\ reads the end of the stream, and on the restarted server the kept handle
\ names nothing, not even the connection now on the killed one's descriptor:
\ the send through it is refused and the slot's next socket hears only its own
\ frames.
: KILLED-CASE ( -- )
   s" a handle kept past a killed worker" SECTION
   0 KEPT !
   0 STALE-SEND !
   s" /park" UPGRADE {: parked:TCP4:connection :}
   KEPT? TTRUE
   1 EXIT-FAULT !
   HTTP:STOP
   0 EXIT-FAULT !
   s" the stop killed the parked worker" T-LABEL
   HTTP:KILLED-TASKS 1 T=
   parked ENDS
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s" the slot's next socket is answered its own 101 first" T-LABEL
   s" /stale" UPGRADE {: conn:TCP4:connection :}
   conn WS-OPCODE:text s" fresh" EXCHANGE
   s" a send through it is refused on the restarted server" T-LABEL
   STALE-SEND @ E-WS-CLOSED T=
   conn FAREWELL ;

\ True once the task has ended, false once the deadline, a mono-ns time, has
\ passed.
: ENDED-BY? ( ptr n n -- bool ) {: tcb:ptr deadline:n :}
   begin
      tcb TASK:DONE? if true exit then
      mono-ns deadline > if false exit then
      POLL-MS >MS TASK:SLEEP
   again ;

\ One frame as the client sends it, behind what the burst holds.
: BURST+ ( WS:opcode ptr u8 n -- ) {: op a u:n :}
   true op a u FRAME$ {: fa fu:n :}
   fa fu BURST-BUF BURST-U @ SPAN:SKIP SPAN:COPY
   fu BURST-U +! ;

\ FLOOD-PINGS pings of PING-LEN bytes.
: BURST-FILL ( -- )
   0 BURST-U !
   FLOOD-PINGS 0 ?do WS-OPCODE:ping PATTERN PING-LEN SPAN:TAKE SPAN:$ BURST+ loop ;

\ The client task sends the burst over and over and reads nothing, until the
\ stream fails under it: past the handler's close it still sends, which the
\ worker's lingering close cuts at its deadline.
: PINGER ( -- )
   0 PEER @ {: conn:TCP4:connection :}
   BURST-BUF BURST-U @ SPAN:TAKE SPAN:$ {: a u:n :}
   begin
      conn a u TCP4:TRANSFER-BYTES TCP4:WRITE
      MATCH TCP4:status
         ok OF ENDOF
         failed OF drop exit ENDOF
      ;MATCH
   again ;

\ When the last poll for the pong's claim that found the header of the frame in
\ hand undecoded began. A ping's header is decoded before its claim samples the
\ clock and stays so until the frame is reset, after the claim is given up
\ (lib/net/ws.f FRAME-IN, TURN), so that poll, which samples the clock before it
\ reads the header, began before the stalled pong's claim: a time measured
\ apart from its deadline. A poll that found no claim
\ proves less, since CLAIM samples the clock before it publishes the deadline.
variable HEADLESS-AT

\ When the worker came for the lock to write the pong it owes, once that pong
\ has stopped moving bytes: the claim reads the same before and after the stall
\ is seen, so the stalled write is the pong under it. 0 until then.
: PONG-CLAIM ( -- n )
   0 HELD @ {: sock:WS:socket :}
   mono-ns {: began:n :}
   sock WS-TEST:DECODED? 0= if began HEADLESS-AT ! then
   sock WS-TEST:CLAIM-DUE {: due:n :}
   due 0= if 0 exit then
   sock WS-TEST:STALLED? 0= if 0 exit then
   sock WS-TEST:CLAIM-DUE due <> if 0 exit then
   due WS:STALL-MS NS-PER-MS * - ;

\ How long after the worker's claim on the lock its socket may fail, in ns:
\ the stall bound, the send slice in which the write notices it, and GRACE-MS.
: STALL-BOUND ( -- n )
   WS:STALL-MS TCP4:SEND-SLICE-MS + GRACE-MS + NS-PER-MS * ;

\ True once the handler has heard its socket close; false once the claim is
\ STALL-BOUND behind.
: HEARD-CLOSE? ( n -- bool )
   {: claim:n :}
   begin
      CLOSED-AT @ 0 <> if true exit then
      mono-ns claim - STALL-BOUND > if false exit then
      POLL-MS >MS TASK:SLEEP
   again ;

\ How long after the claim the handler heard its socket close.
: CLOSE-LAG ( n -- n )
   {: claim:n :}
   CLOSED-AT @ claim - ;

\ The bytes a queue holds, and whether the OS counted them: it does not once
\ the connection is gone.
: QUEUED-N ( TCP4:queue-result -- n bool )
   MATCH TCP4:queue-result
      queued OF BLEN>N true ENDOF
      failed OF drop 0 false ENDOF
   ;MATCH ;

: PRELOAD-ACKED? ( -- bool )
   0 HELD @ WS-TEST:LINK TCP4:UNSENT QUEUED-N {: pending:n counted:bool :}
   counted pending 0= and ;

: PRELOAD-ACKED-BY? ( -- bool )
   mono-ns REACH-MS NS-PER-MS * + {: deadline:n :}
   begin
      PRELOAD-ACKED? if true exit then
      mono-ns deadline >= if false exit then
      TASK:PAUSE
   again ;

\ Linux's fixed receive buffer is already overfull with unread bytes, and
\ the peer advertises no room. The unsent queue exceeds a pong's payload.
: LINUX-PONG-BACKED? ( -- bool )
   0 HELD @ WS-TEST:LINK TCP-STATE {: unsent:n window:n :}
   0 PEER @ TCP4:UNREAD QUEUED-N {: unread:n counted:bool :}
   counted unread PEER-CAP @ > and
   window 0= and unsent PING-LEN > and ;

\ Lower the sender's actual buffer cap below its unsent queue, then check the
\ same claim and empty send slice. An ACK for already transmitted data cannot
\ reduce that unsent queue; a zero peer window and the low-water mark prevent
\ any new send buffer until the peer reads. The reduced cap cannot make the
\ last send buffer accept bytes that its last slice refused.
: LINUX-PONG-CLAIM? ( n -- bool )
   {: claim:n :}
   LINUX-PONG-BACKED? 0= if false exit then
   0 HELD @ WS-TEST:LINK {: link:TCP4:connection :}
   link SOL-SOCKET SO-SNDBUF SERVER-SNDBUF OPT-U32!
   link SOL-SOCKET SO-SNDBUF OPT-U32@ {: cap:n :}
   link TCP-STATE {: unsent:n window:n :}
   0 HELD @ {: sock:WS:socket :}
   unsent cap > window 0= and
   sock WS-TEST:STALLED? and
   sock WS-TEST:CLAIM-DUE claim WS:STALL-MS NS-PER-MS * + = and ;

\ The pong bytes between the worker and the flooding client, which reads none,
\ and whether both queues were counted: those the server's connection holds
\ unsent or unacknowledged, and those unread in the client's. A pong's bytes
\ enter the first when the worker writes it and leave it only once they are in
\ the second, so the sum grows with each pong and holds still once one finds no
\ room.
: PONGS-HELD ( -- n bool )
   0 HELD @ WS-TEST:LINK TCP4:UNSENT QUEUED-N {: unsent:n sent-ok:bool :}
   0 PEER @ TCP4:UNREAD QUEUED-N {: unread:n read-ok:bool :}
   unsent unread + sent-ok read-ok and ;

\ The worker's claim for the stalled pong (PONG-CLAIM), asked every POLL-MS;
\ 0 once a queue cannot be counted, or once REACH-MS has passed with no more
\ pong bytes held than before. The wait lasts as long as load makes the fill
\ and misses only a fill that stopped short of a stall: a client that reads
\ its pongs, a worker that answers none.
: PONG-FILLED ( -- n )
   0 mono-ns                             \ the most pong bytes held yet, and when it grew
   begin {: most:n grew:n :}
      PONG-CLAIM {: claim:n :}
      claim 0 <> if
         LINUX? 0= if claim exit then
         claim LINUX-PONG-CLAIM? if
            s" < Linux pong: zero window, send queue over cap, unread peer" LOG-LINE
            claim exit
         then
      then
      PONGS-HELD {: held:n counted:bool :}
      counted 0= if 0 exit then
      held most > if
         held mono-ns
      else
         mono-ns grew - REACH-MS NS-PER-MS * > if 0 exit then
         most grew
      then
      POLL-MS >MS TASK:SLEEP
   again ;

\ The Linux kernel returns one only after the sender was actually waiting on
\ this pong's pthread mutex. Waking it is harmless while the pong owns the
\ mutex: pthread_mutex_lock checks the word again and waits.
: PONG-WAITER? ( n -- bool )
   {: claim:n :}
   begin
      mono-ns claim - WS:STALL-MS NS-PER-MS * >= if false exit then
      0 HELD @ WS-TEST:CLAIM-DUE claim WS:STALL-MS NS-PER-MS * + <>
      if false exit then
      WAKE-LOCK-WAITER {: woken:n :}
      woken 1 = if
         mono-ns claim - WS:STALL-MS NS-PER-MS * <
         0 HELD @ WS-TEST:CLAIM-DUE claim WS:STALL-MS NS-PER-MS * + = and
         exit
      then
      woken 0 <> if E-SILENT throw then
      TASK:PAUSE
   again ;

: LINUX-PONG-CONTROL ( TCP4:connection -- )
   {: conn:TCP4:connection :}
   LINUX? 0= if exit then
   \ Complete and acknowledge an ordinary frame before shrinking the peer.
   \ No old ACK can later free sender memory; the unread payload alone then
   \ exceeds the fixed receive capacity until the peer reads or closes.
   0 HELD @ PATTERN BIG SPAN:TAKE SPAN:$ WS:SEND-BINARY
   s" the Linux preload was acknowledged before the peer shrank" T-LABEL
   PRELOAD-ACKED-BY? dup TTRUE
   if s" < Linux preload acknowledged before peer shrank" LOG-LINE then
   conn SOL-SOCKET SO-RCVBUF PEER-RCVBUF OPT-U32!
   conn IPPROTO-TCP TCP-WINDOW-CLAMP PEER-WINDOW OPT-U32!
   conn SOL-SOCKET SO-RCVBUF OPT-U32@ PEER-CAP !
   0 HELD @ WS-TEST:LINK IPPROTO-TCP TCP-NOTSENT-LOWAT SEND-LOWAT OPT-U32! ;

\ A one-worker server whose handler keeps its socket and receives, and a client
\ task that floods it with pings and reads nothing, until the pong the worker
\ owes stops moving bytes: the connection, and the worker's claim on the lock
\ for that pong, 0 when the pongs stopped filling the buffers without a stall.
\ The polls count from just before the first ping, when no pong can have been
\ claimed.
: PONG-STALLED ( -- TCP4:connection n )
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 CLOSED-AT ! 0 LAST-CLOSE !
   s" /stall" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   conn 0 PEER !
   conn LINUX-PONG-CONTROL
   BURST-FILL
   mono-ns HEADLESS-AT !
   [: PINGER ;] PING-TASK TASK:ACTIVATE
   s" the pong the worker owes stalls, its client reading nothing" T-LABEL
   PONG-FILLED {: claim:n :}
   claim 0 <> TTRUE
   conn claim ;

\ The pong stalled under the claim. The socket fails under 1006 within
\ STALL-BOUND of the claim and no sooner than WS:STALL-MS after it, timed by
\ the deadline the claim published and from HEADLESS-AT, which a claim
\ published with a shorter deadline does not move. Another task's send, made
\ while the pong is stalled, waits behind it and is refused; one still waiting
\ at the deadline is left where it waits rather than joined.
: STALLED-PONG ( n -- )
   {: claim:n :}
   [: HELD-SEND ;] SEND-TASK TASK:ACTIVATE
   LINUX? if
      s" the sender queued on the stalled pong's mutex" T-LABEL
      claim PONG-WAITER? dup TTRUE
      if s" < Linux sender waited on this pong's mutex" LOG-LINE then
   then
   s" the socket fails under 1006 within the stall bound" T-LABEL
   claim HEARD-CLOSE? TTRUE
   LAST-CLOSE @ 1006 T=
   CLOSED-AT @ 0 <> claim CLOSE-LAG STALL-BOUND <= and TTRUE
   s" and no sooner than its deadline" T-LABEL
   claim CLOSE-LAG WS:STALL-MS NS-PER-MS * >= TTRUE
   s" nor sooner than WS:STALL-MS after a poll that began before the claim" T-LABEL
   HEADLESS-AT @ CLOSE-LAG WS:STALL-MS NS-PER-MS * >= TTRUE
   s" the send behind the stalled pong is refused" T-LABEL
   SEND-TASK mono-ns GRACE-MS NS-PER-MS * + ENDED-BY? {: ended:bool :}
   ended TTRUE
   ended if
      SEND-TASK JOINED E-WS-CLOSED T=
      LINUX? if
         s" < Linux pong closed 1006; sender returned E-WS-CLOSED" LOG-LINE
      then
   then ;

\ A client task floods the socket with pings and never reads, so the pong the
\ worker owes stalls (STALLED-PONG). The client floods on after the close, and
\ the worker, whose lingering close ends at its deadline however long a peer
\ sends, ends itself at the stop.
: STALL-CASE ( -- )
   s" a peer that stops reading" SECTION
   PONG-STALLED {: conn:TCP4:connection claim:n :}
   claim 0 <> if claim STALLED-PONG then
   HTTP:STOP
   s" the stalled socket's worker ended itself" T-LABEL
   HTTP:KILLED-TASKS 0 T=
   PING-TASK TASK:KILL
   conn TCP4:CLOSE DROP-STATUS ;

$BB8 constant STOP-CEILING-MS     \ over the server's own stop bound, far under a hang (lib/net/http-test.f)

\ The pushing task writes to a client that reads none until a push is
\ refused, and answers the code that refused it.
: PUSH-UNREAD ( -- )
   REFUSED-PUSH TASK:RETURN ;

\ 1 once the write in hand on the kept socket has stopped moving bytes, a push
\ held with the slot's lock, its client reading nothing; 0 while no write is
\ stalled. The probe answers within a slice of the stall, far inside
\ WS:STALL-MS.
: PUSH-STALL ( -- n )
   0 HELD @ WS-TEST:STALLED? if 1 else 0 then ;

\ When the kept socket's writes give up for the worker's claim on the lock, 0
\ while it makes none: only the worker claims, to answer, let go or retire.
: ANSWER-CLAIM ( -- n )
   0 HELD @ WS-TEST:CLAIM-DUE ;

\ A one-worker server whose handler keeps its socket and receives, and another
\ task's push to a client that reads none, held in its write with the slot's
\ lock: the connection, and whether the push was seen held.
: PUSH-STALLED ( -- TCP4:connection bool )
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 GO-HOME ! 0 LAST-CLOSE !
   s" /goodbye" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   [: PUSH-UNREAD ;] PUSH-TASK TASK:ACTIVATE
   s" the push is held, its client reading nothing" T-LABEL
   [: PUSH-STALL ;] REACH-MS AWAITED 0 <> {: held:bool :}
   held TTRUE
   conn held ;

\ The server stops and every task ends itself: none is killed, and the stop
\ returns far inside a hang.
: STOPS-IN-TIME ( ptr u8 n -- ) {: label lu:n :}
   mono-ns {: began:n :}
   HTTP:STOP
   mono-ns began - {: took:n :}
   label lu T-LABEL
   HTTP:KILLED-TASKS 0 T=
   s" and the stop returned in time" T-LABEL
   took STOP-CEILING-MS NS-PER-MS * < TTRUE ;

\ The push ends within GRACE-MS and is reaped; one still running then is left
\ where it waits. Its code is the refusal only when the push was seen held or
\ under way, the state the refusal depends on.
: PUSH-REFUSED ( bool -- )
   {: seen:bool :}
   s" the push ended" T-LABEL
   PUSH-TASK mono-ns GRACE-MS NS-PER-MS * + ENDED-BY? {: ended:bool :}
   ended TTRUE
   ended 0= if exit then
   PUSH-TASK JOINED {: code:n :}
   seen 0= if exit then
   s" the push was refused" T-LABEL
   code E-WS-CLOSED T= ;

\ How long after a write is cut short it may still run, in ns: the release
\ bound, the send slice in which the write notices it, and GRACE-MS.
: CUT-BOUND ( -- n )
   HTTP:RELEASE-MS TCP4:SEND-SLICE-MS + GRACE-MS + NS-PER-MS * ;

\ The server stops while the handler's push is under way, and every task ends
\ itself in time; the stop cut the push, which is E-WS-CLOSED within CUT-BOUND
\ of the stop.
: STOP-CUTS-FLOOD ( ptr u8 n -- ) {: label lu:n :}
   mono-ns {: began:n :}
   label lu STOPS-IN-TIME
   s" the stop cut the handler's push" T-LABEL
   FLOOD-CODE @ E-WS-CLOSED T=
   s" which was under way when the stop began, and ended within its bound" T-LABEL
   FLOOD-END @ began - {: took:n :}
   took 0 > took CUT-BOUND <= and TTRUE ;

\ A stop finds the handler inside its own push to a client that reads none,
\ held there far inside WS:STALL-MS. The stop cuts the push once its client has
\ taken no byte for HTTP:RELEASE-MS: the push is E-WS-CLOSED within CUT-BOUND
\ of the stop, the handler returns, and the stop kills no task. On the
\ restarted server another task's send through the handle the handler kept is
\ refused; one held past the deadline is left where it waits rather than
\ joined.
: FLOODED-CASE ( -- )
   s" a stop while the handler's own push is stalled" SECTION
   0 KEPT ! 0 FLOOD-CODE ! 0 FLOOD-END !
   s" /flood" UPGRADE {: flooded:TCP4:connection :}
   KEPT? TTRUE
   s" the handler's push is held, its client reading nothing" T-LABEL
   [: PUSH-STALL ;] REACH-MS AWAITED 0 <> {: held:bool :}
   held TTRUE
   held if
      s" the worker inside its push ended itself" STOP-CUTS-FLOOD
   else
      s" the worker inside its push ended itself" STOPS-IN-TIME
   then
   flooded TCP4:CLOSE DROP-STATUS
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   mono-ns GRACE-MS TCP4:SEND-SLICE-MS + NS-PER-MS * + {: deadline:n :}
   [: HELD-SEND ;] STALE-TASK TASK:ACTIVATE
   s" a send through its handle is refused on the restarted server" T-LABEL
   STALE-TASK deadline ENDED-BY? {: ended:bool :}
   ended TTRUE
   ended 0= if exit then
   STALE-TASK JOINED E-WS-CLOSED T= ;

\ One worker, so each socket is served in the slot the one before it held.
: SLOT-CASES ( -- )
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   HELD-CASE
   KILLED-CASE
   FLOODED-CASE
   HTTP:STOP
   s" the restarted server's tasks ended themselves" T-LABEL
   HTTP:KILLED-TASKS 0 T= ;

\ The handler returns on its cue behind another task's push, and the stop that
\ follows kills no task. A push seen held or under way is cut by the goodbye
\ within CUT-BOUND of the handler's turn, before any stop, and the handler
\ returned with its socket open; whatever was seen, the push is reaped.
: GOODBYE-CUT ( bool -- )
   {: seen:bool :}
   1 GO-HOME !
   seen if
      s" the goodbye cut the push, before any stop" T-LABEL
      PUSH-TASK mono-ns RECEIVE-MS NS-PER-MS * + CUT-BOUND + ENDED-BY? dup TTRUE
   else
      false
   then {: cut:bool :}
   s" the worker saying goodbye behind the push ended itself" STOPS-IN-TIME
   cut if
      s" the handler returned on its cue, with its socket open" T-LABEL
      LAST-CLOSE @ 0 T=
   then
   seen PUSH-REFUSED ;

\ The handler returns while another task's push to a client that reads none is
\ held in its write with the slot's lock. The worker comes for the lock for its
\ goodbye, due HTTP:RELEASE-MS later: the push gives up then and the socket is
\ lost, within CUT-BOUND of the handler's turn and with no stop to cut it,
\ rather than waiting out WS:STALL-MS. The goodbye writes nothing, and the stop
\ that follows kills no task.
: GOODBYE-CASE ( -- )
   s" a goodbye behind a stalled push" SECTION
   PUSH-STALLED {: conn:TCP4:connection held:bool :}
   held GOODBYE-CUT
   conn TCP4:CLOSE DROP-STATUS ;

\ A stop finds the handler in a RECEIVE that would wait far past the stop's
\ bound, its client saying nothing. RECEIVE reads the stop within a slice of
\ its wait and closes the socket under 1001, going away: the client reads that
\ close and then the end of the stream, the handler hears 1001, and the stop
\ kills no task.
: PATIENT-CASE ( -- )
   s" a stop while the handler waits" SECTION
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 LAST-CLOSE !
   s" /patient" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   s" the waiting worker ended itself" STOPS-IN-TIME
   s" the client is told the server is going away" T-LABEL
   conn HEAR 1001 GOT-CLOSE
   conn ENDS
   s" and so is the handler" T-LABEL
   LAST-CLOSE @ 1001 T= ;

\ A stop comes while the worker waits for its slot's lock to answer the
\ client's frame - the pong a ping is owed, or the close that answers a close -
\ behind another task's push that the client, reading nothing, has stalled.
\ The stop comes once the worker's claim on the lock stands behind the push,
\ and cuts the push once its client has taken no byte for HTTP:RELEASE-MS: the
\ socket is lost, the answer is never written, and the worker ends itself in
\ time.
: BEHIND-PUSH ( ptr u8 n WS:opcode ptr u8 n -- ) {: label lu:n op a u:n :}
   label lu SECTION
   PUSH-STALLED {: conn:TCP4:connection held:bool :}
   conn true op a u SAY
   held if
      s" the worker comes for the lock to answer, behind the push" T-LABEL
      [: ANSWER-CLAIM ;] REACH-MS AWAITED 0 <> dup TTRUE
   else
      false
   then {: behind:bool :}
   s" the worker answering behind the push ended itself" STOPS-IN-TIME
   behind if
      s" the socket was lost under the push" T-LABEL
      LAST-CLOSE @ 1006 T=
   then
   held PUSH-REFUSED
   conn TCP4:CLOSE DROP-STATUS ;

: PING-BEHIND-CASE ( -- )
   s" a stop while a pong waits behind a stalled push" WS-OPCODE:ping s" hb" BEHIND-PUSH ;

: CLOSE-BEHIND-CASE ( -- )
   s" a stop while a close waits behind a stalled push"
   WS-OPCODE:close 1000 STATUS$ BEHIND-PUSH ;

\ A stop comes while the worker is held in the pong it owes a client that
\ floods it with pings and reads nothing, the client's receive buffer full.
\ The stop cuts the pong once the client has taken no byte for
\ HTTP:RELEASE-MS: the socket is lost, and the worker ends itself in time.
: PONG-STOP-CASE ( -- )
   s" a stop while a pong is stalled" SECTION
   PONG-STALLED {: conn:TCP4:connection claim:n :}
   s" the worker held in the pong ended itself" STOPS-IN-TIME
   claim 0 <> if
      LINUX? if
         s" the pong saw stop before its own stall deadline" T-LABEL
         0 HELD @ WS-TEST:CUT-AT {: cut:n :}
         cut 0 > cut claim WS:STALL-MS NS-PER-MS * + < and TTRUE
         s" < Linux pong saw STOP before its stall deadline" LOG-LINE
         s" and closed under the stop cut, before that deadline" T-LABEL
         CLOSED-AT @ cut - HTTP:RELEASE-MS NS-PER-MS * >=
         CLOSED-AT @ claim WS:STALL-MS NS-PER-MS * + < and TTRUE
         s" < Linux stop closed this pong before its stall deadline" LOG-LINE
      then
      s" the socket was lost under the pong" T-LABEL
      LAST-CLOSE @ 1006 T=
   then
   PING-TASK TASK:KILL
   conn TCP4:CLOSE DROP-STATUS ;

\ A stop finds the handler about to send. Its send starts the stop's cut and
\ goes out whole; the close frame going away calls for comes HTTP:RELEASE-MS
\ into the cut, so it is refused before its first byte and the socket is
\ lost: the client reads the text and then the end of the stream, and the
\ handler hears 1006, a connection lost, not the 1001 that was never sent.
: UNSENT-CLOSE-CASE ( -- )
   s" a stop whose close cannot be sent" SECTION
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 LAST-CLOSE !
   s" /late" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   s" the worker whose close was refused ended itself" STOPS-IN-TIME
   s" the client reads the text and then the end of the stream" T-LABEL
   conn HEAR WS-OPCODE:text s" cut" GOT
   conn ENDS
   s" and the handler hears the connection lost" T-LABEL
   LAST-CLOSE @ 1006 T= ;

\ Every stop here comes while a handler is inside RECEIVE or about to be, and
\ none kills.
: STOP-CASES ( -- )
   PATIENT-CASE
   PING-BEHIND-CASE
   CLOSE-BEHIND-CASE
   PONG-STOP-CASE
   UNSENT-CLOSE-CASE ;


\ ---- a client that keeps taking bytes ----------------------------------------

\ The client takes SIP-BYTES every SIP-MS, 3.2 MB/s, so no write to it ever
\ waits out a bound on silence: only the cut of a write that keeps moving bytes
\ ends one.
$10000 constant SIP-BYTES
$14 constant SIP-MS
\ 48 MiB in one frame, more than 15 s at the client's pace: long enough to span
\ the WS:STALL-MS claim of a pong due behind it. Each case waits for its push
\ to be under way, its client taking bytes (PUSH-TAKEN), before relying on it.
$3000000 constant BIG-BYTES
$FA0 constant SIP-FOR-MS          \ how long past the wait for the handler's own pushes their client takes bytes: past the stop's ceiling
1 TYPED-BUFFER BIG-SPAN SPAN:span<u8>
SIP-BYTES SPAN-BUFFER: SIP-BUF
TASK-STACK TASK:TASK SIP-TASK
variable SIP-UNTIL                \ the mono-ns past which the client takes no more
variable SIPPED                   \ what it has taken

: SEND-BIG ( -- )
   0 HELD @ 0 BIG-SPAN @ SPAN:$ WS:SEND-BINARY ;

\ Another task writes one BIG-BYTES frame through the kept socket and answers
\ the code that refused it: 0 when the frame went out whole.
: PUSH-BIG ( -- )
   [: SEND-BIG ;] catch TASK:RETURN ;

\ The client takes bytes until its stream ends or SIP-UNTIL passes. Each turn
\ ends in a TASK:PAUSE, where a KILL that came during its sleep ends it.
: SIP ( -- )
   0 PEER @ {: conn:TCP4:connection :}
   begin
      mono-ns SIP-UNTIL @ > if exit then
      conn SIP-BUF SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
      MATCH TCP4:read-result
         data OF BLEN>N SIPPED +! ENDOF
         closed OF drop exit ENDOF
         failed OF drop exit ENDOF
      ;MATCH
      SIP-MS >MS TASK:SLEEP
      TASK:PAUSE
   again ;

\ The client takes bytes from the connection until the mono-ns given, or until
\ told to stop.
: SIP-FROM ( TCP4:connection n -- ) {: conn:TCP4:connection until:n :}
   0 SIPPED !
   until SIP-UNTIL !
   conn 0 PEER !
   [: SIP ;] SIP-TASK TASK:ACTIVATE ;

\ The client stops taking bytes and its task is reaped: the server has closed
\ the connection by now, so no read holds it.
: SIP-OVER ( -- )
   mono-ns SIP-UNTIL !
   s" the client that kept taking bytes ended" T-LABEL
   SIP-TASK mono-ns ANSWER-MS NS-PER-MS * + ENDED-BY? {: ended:bool :}
   ended TTRUE
   ended if SIP-TASK TASK:KILL then ;

\ What the client has taken, 0 until its first byte: the push to it is the
\ only writer on its connection, so a byte taken is one that push wrote,
\ holding the slot's lock.
: PUSH-TAKEN ( -- n )
   SIPPED @ ;

\ The client takes bytes from the connection, PARK-MS past the longest wait
\ for the push, and another task writes one BIG-BYTES frame to it through the
\ kept socket: whether that push was seen under way, its client taking bytes.
\ No reader here shows which task holds the slot's lock, so a byte taken while
\ the task is not done stands for it: the frame takes over 15 s at the client's
\ pace, past REACH-MS, and until the case cues the worker nothing claims the
\ lock or stops the server.
: BIG-UNDER-WAY ( TCP4:connection -- bool )
   {: conn:TCP4:connection :}
   conn mono-ns REACH-MS PARK-MS + NS-PER-MS * + SIP-FROM
   [: PUSH-BIG ;] PUSH-TASK TASK:ACTIVATE
   s" the push is under way, its client taking bytes" T-LABEL
   [: PUSH-TAKEN ;] REACH-MS AWAITED 0 <> PUSH-TASK TASK:DONE? 0= and dup TTRUE ;

\ The handler returns on its cue while another task's BIG-BYTES push is under
\ way to a client that keeps taking bytes. The worker comes for the lock for
\ its goodbye, due HTTP:RELEASE-MS later: the push gives up then, however many
\ bytes its client goes on taking, and the socket is lost, within CUT-BOUND of
\ the handler's turn and with no stop to cut it. The stop that follows kills
\ no task.
: TRICKLE-GOODBYE-CASE ( -- )
   s" a goodbye behind a push its client keeps taking" SECTION
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 GO-HOME ! 0 LAST-CLOSE !
   s" /goodbye" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   conn BIG-UNDER-WAY GOODBYE-CUT
   SIP-OVER
   conn TCP4:CLOSE DROP-STATUS ;

\ True once the handler has heard its socket close, false once the deadline, a
\ mono-ns time, has passed.
: CLOSED-BY? ( n -- bool )
   {: deadline:n :}
   begin
      LAST-CLOSE @ 0 <> if true exit then
      mono-ns deadline > if false exit then
      POLL-MS >MS TASK:SLEEP
   again ;

\ The client pings while the handler receives behind another task's BIG-BYTES
\ push to it, which it keeps taking. The pong is due WS:STALL-MS after the
\ worker comes for the lock: the push gives up then, however many bytes its
\ client goes on taking, and the socket is lost, so the RECEIVE that read the
\ ping answers closed 1006 no later than its wait, WS:STALL-MS and one send
\ slice. The stop that follows kills no task.
: PONG-DUE-CASE ( -- )
   s" a pong due behind a push its client keeps taking" SECTION
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 LONGEST ! 0 LAST-CLOSE !
   s" /timed" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   conn BIG-UNDER-WAY {: going:bool :}
   conn true WS-OPCODE:ping s" hb" SAY
   going if
      s" the socket was lost under the push in time" T-LABEL
      mono-ns STALL-BOUND + CLOSED-BY? TTRUE
      going PUSH-REFUSED
   then
   s" the worker that owed the pong ended itself" STOPS-IN-TIME
   going if
      s" the handler heard the connection lost" T-LABEL
      LAST-CLOSE @ 1006 T=
      s" and no RECEIVE ran past its wait, WS:STALL-MS and one send slice" T-LABEL
      LONGEST @ RECEIVE-MS NS-PER-MS * STALL-BOUND + <= TTRUE
   else
      going PUSH-REFUSED
   then
   SIP-OVER
   conn TCP4:CLOSE DROP-STATUS ;

\ A stop while the handler's own pushes go, frame after frame, to a client that
\ keeps taking bytes SIP-FOR-MS past the longest wait for them, past the stop's
\ ceiling. The stop cuts the slot, not one frame: HTTP:RELEASE-MS after the
\ first push that finds it cut, the push in flight gives up, or the next is
\ refused before its first byte, however many bytes the client goes on taking.
\ The push is E-WS-CLOSED within CUT-BOUND of the stop, and the stop kills no
\ task. The pushes count as under way once their client has taken a byte and
\ none was refused: before the stop nothing refuses one, since only a cut ends
\ a write that keeps moving bytes and nothing claims the lock.
: FLOOD-SIP-CASE ( -- )
   s" a stop while the handler pushes to a client that keeps taking bytes" SECTION
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   0 KEPT ! 0 FLOOD-CODE ! 0 FLOOD-END !
   s" /flood" UPGRADE {: conn:TCP4:connection :}
   KEPT? TTRUE
   conn mono-ns REACH-MS SIP-FOR-MS + NS-PER-MS * + SIP-FROM
   s" the handler's pushes are under way, their client taking bytes" T-LABEL
   [: PUSH-TAKEN ;] REACH-MS AWAITED 0 <> FLOOD-CODE @ 0= and {: going:bool :}
   going TTRUE
   going if
      s" the worker pushing to its client ended itself" STOP-CUTS-FLOOD
   else
      s" the worker pushing to its client ended itself" STOPS-IN-TIME
   then
   SIP-OVER
   conn TCP4:CLOSE DROP-STATUS ;

\ Every write here is to a client that keeps taking bytes, so only the cut of
\ a write that keeps moving them ends it. The big frame's bytes are held only
\ while these cases run.
: SIP-CASES ( -- )
   BIG-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN 0 BIG-SPAN !
   TRICKLE-GOODBYE-CASE
   PONG-DUE-CASE
   FLOOD-SIP-CASE
   0 BIG-SPAN @ MEM:FREE-SPAN ;


\ ---- the transcript this file expects ----------------------------------------

: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ SPAN:TAKE SPAN:$ ;

: W ( ptr u8 n -- ) {: a u:n :}
   a u WANT-BUF WANT-U @ SPAN:SKIP SPAN:COPY
   u WANT-U +!
   LF WANT-BUF WANT-U @ SPAN:U8!
   1 WANT-U +! ;

\ A close frame and the close that answers it, then the end of the stream.
: W-FAREWELL ( -- )
   s" > close 2 03e8" W
   s" < close 2 03e8" W
   s" < end of stream" W ;

: W-FAILED ( ptr u8 n -- )
   W
   s" < end of stream" W ;

: WANT-OPENING ( -- )
   s" == handshake" W
   s" HTTP/1.1 101 Switching Protocols" W
   s" Upgrade: websocket" W
   s" Connection: Upgrade" W
   s" Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" W
   s" " W
   s" < end of stream" W
   s" == refused handshakes" W
   s" no Upgrade: HTTP/1.1 400 Bad Request" W
   s" Upgrade to h2c: HTTP/1.1 400 Bad Request" W
   s" Connection without Upgrade: HTTP/1.1 400 Bad Request" W
   s" no key: HTTP/1.1 400 Bad Request" W
   s" a key of 23 characters: HTTP/1.1 400 Bad Request" W
   s" a key of 18 bytes: HTTP/1.1 400 Bad Request" W
   s" a key outside base64: HTTP/1.1 400 Bad Request" W
   s" two keys: HTTP/1.1 400 Bad Request" W
   s" POST: HTTP/1.1 400 Bad Request" W
   s" HTTP/1.0: HTTP/1.1 400 Bad Request" W
   s" no Host: HTTP/1.1 400 Bad Request" W
   s" two Hosts: HTTP/1.1 400 Bad Request" W
   s" an empty Host: HTTP/1.1 400 Bad Request" W
   s" a Host with a space: HTTP/1.1 400 Bad Request" W
   s" no version: HTTP/1.1 426 Upgrade Required" W
   s" version 12: HTTP/1.1 426 Upgrade Required" W
   s" == a handshake in another spelling" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL ;

: WANT-ECHOES ( -- )
   s" == echo text" W
   s" > text 5 48656c6c6f" W
   s" < text 5 48656c6c6f" W
   s" > text 0" W
   s" < text 0" W
   s" > text 10 68c3a96c6c6f20e282ac" W
   s" < text 10 68c3a96c6c6f20e282ac" W
   W-FAREWELL
   s" == text the server may not send" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL
   s" == echo binary in each length form" W
   s" > binary 125 sum 16101" W
   s" < binary 125 sum 16101" W
   s" > binary 126 sum 16143" W
   s" < binary 126 sum 16143" W
   s" > binary 65535 sum 8355608" W
   s" < binary 65535 sum 8355608" W
   s" > binary 65536 sum 8355840" W
   s" < binary 65536 sum 8355840" W
   s" > binary 70000 sum 8925000" W
   s" < binary 70000 sum 8925000" W
   W-FAREWELL
   s" == fragments" W
   s" > text+ 3 48656c" W
   s" > continuation+ 3 6c6f2c" W
   s" > continuation 6 20776f726c64" W
   s" < text 12 48656c6c6f2c20776f726c64" W
   s" > binary+ 2 0102" W
   s" > ping 1 21" W
   s" < pong 1 21" W
   s" > continuation 2 0304" W
   s" < binary 4 01020304" W
   s" > text+ 1 c3" W
   s" > continuation 1 a9" W
   s" < text 2 c3a9" W
   s" > text+ 2 6869" W
   s" > continuation 0" W
   s" < text 2 6869" W
   W-FAREWELL
   s" == ping and pong" W
   s" > ping 2 6862" W
   s" < pong 2 6862" W
   s" > ping 0" W
   s" < pong 0" W
   s" > ping 125 sum 16101" W
   s" < pong 125 sum 16101" W
   s" > pong 1 78" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL ;

: WANT-WAITS ( -- )
   s" == timeouts" W
   s" > text 7 cut after byte 1" W
   s" < text 7 726573756d6564" W
   s" > text 7 cut after byte 3" W
   s" < text 7 726573756d6564" W
   s" > text 7 cut after byte 9" W
   s" < text 7 726573756d6564" W
   W-FAREWELL
   s" == a frame behind the handshake" W
   s" > text 5 6561726c79" W
   s" < text 5 6561726c79" W
   W-FAREWELL ;

: WANT-FAULTS ( -- )
   s" == an unmasked frame" W
   s" > raw 810548656c6c6f" W
   s" < close 2 03ea" W-FAILED
   s" == a control frame past 125 bytes" W
   s" > raw 89fe" W
   s" < close 2 03ea" W-FAILED
   s" == a fragmented control frame" W
   s" > raw 0980" W
   s" < close 2 03ea" W-FAILED
   s" == a reserved bit" W
   s" > raw c180" W
   s" < close 2 03ea" W-FAILED
   s" == a reserved opcode" W
   s" > raw 8380" W
   s" < close 2 03ea" W-FAILED
   s" == a length in a longer form than it needs" W
   s" > raw 82fe007d" W
   s" < close 2 03ea" W-FAILED
   s" == a message one byte past the bound" W
   s" > raw 82ff0000000000100001" W
   s" < close 2 03f1" W-FAILED
   s" == a continuation of nothing" W
   s" > raw 808037fa213d" W
   s" < close 2 03ea" W-FAILED
   s" == a new message inside another" W
   s" > text+ 1 61" W
   s" > text 1 62" W
   s" < close 2 03ea" W-FAILED
   s" == text that is not UTF-8" W
   s" > text 2 c328" W
   s" < close 2 03ef" W-FAILED
   s" == fragments that are not UTF-8 together" W
   s" > text+ 1 c3" W
   s" > continuation 1 28" W
   s" < close 2 03ef" W-FAILED
   s" == fragments one byte past the bound" W
   s" > binary+ 70000 sum 8925000" W
   s" > raw 80ff00000000000eee9137fa213d" W
   s" < close 2 03f1" W-FAILED
   s" == a message of exactly the bound" W
   s" > raw 82ff000000000010000037fa213d" W
   s" < end of stream" W
   s" == fragments of exactly the bound" W
   s" > binary+ 70000 sum 8925000" W
   s" > raw 80ff00000000000eee9037fa213d" W
   s" < end of stream" W ;

: WANT-STATUSES ( -- )
   s" == close statuses a frame may carry" W
   s" > close 2 03e8" W
   s" < close 2 03e8" W-FAILED
   s" > close 2 03eb" W
   s" < close 2 03eb" W-FAILED
   s" > close 2 03ef" W
   s" < close 2 03ef" W-FAILED
   s" > close 2 03f6" W
   s" < close 2 03f6" W-FAILED
   s" > close 2 0bb8" W
   s" < close 2 0bb8" W-FAILED
   s" > close 2 1387" W
   s" < close 2 1387" W-FAILED
   s" == close statuses no frame may carry" W
   s" > close 2 03e7" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03ec" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03ed" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03ee" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03f7" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 0bb7" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 1388" W
   s" < close 2 03ea" W-FAILED ;

: WANT-CLOSES ( -- )
   s" == a close with no status" W
   s" > close 0" W
   s" < close 0" W-FAILED
   s" == a close with a reason" W
   s" > close 5 03e9627965" W
   s" < close 2 03e9" W-FAILED
   s" == a close with half a status" W
   s" > close 1 03" W
   s" < close 2 03ea" W-FAILED
   s" == a close whose reason is not UTF-8" W
   s" > close 4 03e8c328" W
   s" < close 2 03ef" W-FAILED
   s" == the server closes first" W
   s" > text 2 676f" W
   s" < close 2 03e8" W
   s" > ping 1 78" W
   s" < pong 1 78" W
   s" > close 2 03e8" W
   s" < end of stream" W
   s" == the handler returns" W
   s" > text 2 676f" W
   s" < close 2 03e8" W-FAILED
   s" == the handler throws" W
   s" > text 2 676f" W
   s" < close 2 03f3" W-FAILED
   s" == one request accepted twice" W
   s" > text 10 7374696c6c2068657265" W
   s" < text 10 7374696c6c2068657265" W
   W-FAREWELL
   s" == a push from a second task" W
   s" < text 22 70757368656420627920616e6f74686572207461736b" W
   s" > text 2 676f" W
   s" < 400 pushed frames and 400 of the handler's, 0 torn" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL
   s" == a burst the peer keeps queued" W
   W-FAREWELL
   s" == a frame whose rest waits in the socket" W
   s" > binary 4160 sum 341120" W
   W-FAREWELL
   s" == plain HTTP after the sockets" W ;

: WANT-SLOTS ( -- )
   s" == a handle kept past its socket" W
   s" < close 2 03e8" W-FAILED
   s" > text 4 6d696e65" W
   s" < text 4 6d696e65" W
   s" > text 9 6f6e6c79206d696e65" W
   s" < text 9 6f6e6c79206d696e65" W
   W-FAREWELL
   s" == a handle kept past a killed worker" W
   s" < end of stream" W
   s" > text 5 6672657368" W
   s" < text 5 6672657368" W
   W-FAREWELL
   s" == a stop while the handler's own push is stalled" W ;

: WANT-STALL ( -- )
   s" == a peer that stops reading" W
   LINUX? if
      s" < Linux preload acknowledged before peer shrank" W
      s" < Linux pong: zero window, send queue over cap, unread peer" W
      s" < Linux sender waited on this pong's mutex" W
      s" < Linux pong closed 1006; sender returned E-WS-CLOSED" W
   then
   s" == a goodbye behind a stalled push" W ;

: WANT-STOPS ( -- )
   s" == a stop while the handler waits" W
   s" < close 2 03e9" W-FAILED
   s" == a stop while a pong waits behind a stalled push" W
   s" > ping 2 6862" W
   s" == a stop while a close waits behind a stalled push" W
   s" > close 2 03e8" W
   s" == a stop while a pong is stalled" W
   LINUX? if
      s" < Linux preload acknowledged before peer shrank" W
      s" < Linux pong: zero window, send queue over cap, unread peer" W
      s" < Linux pong saw STOP before its stall deadline" W
      s" < Linux stop closed this pong before its stall deadline" W
   then
   s" == a stop whose close cannot be sent" W
   s" < text 3 637574" W
   s" < end of stream" W ;

: WANT-SIPS ( -- )
   s" == a goodbye behind a push its client keeps taking" W
   s" == a pong due behind a push its client keeps taking" W
   s" > ping 2 6862" W
   s" == a stop while the handler pushes to a client that keeps taking bytes" W ;

: WANT-TRANSCRIPT ( -- )
   0 WANT-U !
   WANT-OPENING WANT-ECHOES WANT-WAITS WANT-FAULTS WANT-STATUSES WANT-CLOSES
   WANT-SLOTS WANT-STALL WANT-STOPS WANT-SIPS ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole with the transcript this file expects.
: TRANSCRIPT-CASE ( -- )
   s" the transcript" T-LABEL
   s" build" MAKE-DIRS
   s" build/ws-transcript.txt" SCRIPT$ WRITE-ALL
   s" build/ws-transcript.txt" BACK-BUF SPAN:$ READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   BACK-BUF got SPAN:TAKE SPAN:$ WANT$ T$= ;


\ ---- the lifecycle -----------------------------------------------------------

$3E8 constant DRAIN-MS            \ how long a stop waits for the ring to drain
10 constant RETRY-MS

\ AIO:STOP is E-AIO-BUSY until the loop has drained every record, so the stop
\ is retried to a bound (lib/net/http-test.f AIO-STOP).
: AIO-STOP ( -- )
   mono-ns DRAIN-MS NS-PER-MS * + {: deadline:n :}
   begin
      [: AIO:STOP ;] catch {: code:n :}
      code 0= if exit then
      mono-ns deadline > if code throw then
      RETRY-MS >MS TASK:SLEEP
   again ;

: FILL-BUFFERS ( -- )
   BUF-CAP 0 ?do i 31 * 7 + $FF and PATTERN i SPAN:U8! loop
   PUSH-BYTE PUSH-BUF SPAN:FILL
   HANDLER-BYTE HANDLER-BUF SPAN:FILL
   REST-BYTE REST-BUF SPAN:FILL
   $C3 NOT-UTF8 0 SPAN:U8!
   $28 NOT-UTF8 1 SPAN:U8! ;

: CASES ( -- )
   HANDSHAKE-CASE REFUSAL-CASES TOLERANT-CASE
   ECHO-CASES BAD-TEXT-CASE LENGTH-CASES FRAGMENT-CASES PING-CASES
   TIMEOUT-CASE EARLY-CASE
   HEADER-FAULTS MESSAGE-FAULTS BOUND-CASES
   CLOSE-STATUS-CASES CLOSE-CASES SERVER-CLOSE-CASE RETURN-CASES TWICE-CASE
   PUSH-CASE BURST-CASE REST-CASE PLAIN-CASE ;

public

\ The loop first: every wait the server and this client make rides it. Every
\ handler has returned by the time the first server stops, so that stop kills
\ no task; the one-worker server's next stop kills a parked worker on purpose
\ (KILLED-CASE), and no later stop kills any.
: RUN ( -- )
   T-RESET
   0 SCRIPT-U !
   FILL-BUFFERS
   0 PUSH-GO TASK:SEMAPHORE-INIT
   0 BLAST-GO TASK:SEMAPHORE-INIT
   AIO:START
   INSTALL-ROUTES
   [: FAULTY-EXIT ;] HTTP:ON-WORKER-EXIT
   LOOPBACK 0 WORKERS IDLE-MS HTTP:START
   CASES
   HTTP:STOP
   s" every handler returned" T-LABEL
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   SLOT-CASES
   STALL-CASE
   GOODBYE-CASE
   STOP-CASES
   SIP-CASES
   AIO-STOP
   PUSH-GO TASK:SEMAPHORE-DESTROY
   BLAST-GO TASK:SEMAPHORE-DESTROY
   TRANSCRIPT-CASE ;

;package

WS-TEST:RUN
T-REPORT
