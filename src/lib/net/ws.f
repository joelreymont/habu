\ ws.f - RFC 6455 WebSocket connections on the HTTP/1.1 server (src/lib/net/http.f).
\
\ ACCEPT, inside a route handler, checks the request as an opening handshake
\ (section 4.2.1), takes the request's connection from the server for good
\ (HTTP:TAKE-OVER) and hands it to WS's own task, which answers 101 with
\ Sec-WebSocket-Accept. A handshake that is refused is answered through the
\ HTTP response - 400, 426 for a version other than 13, or 503 when WS holds
\ the most sockets START allowed or is stopping - and no socket exists for it.
\
\ One task, started by START, does every read and write on every socket and
\ is the only writer. It waits on one AIO group: a readiness poll per socket
\ and one on a wake pipe. Each socket's handler runs on that task, handed a
\ whole text or binary message, or `closed` with the status once, as the
\ socket ends. A handler must not block: every socket waits while it runs. A
\ handler's throw is reported on stderr as one line, `ws: socket <handle>
\ failed, throw <code>`, and a throw from a handler handed a message also
\ closes its socket under status 1011. The task reassembles fragmented
\ messages, answers a ping with a pong, even once this end has sent its close,
\ and a close frame with a close, and fails the connection when the peer
\ breaks the protocol: status 1002 for a frame the codec refuses
\ (src/lib/net/ws-frame.f) or one out of order, 1007 for text that is not
\ UTF-8, 1009 for a message past MESSAGE-MAX. A closed socket reports the
\ status its close frame carried, 1005 when the frame carried none and 1006
\ when the stream ended without one (section 7.1.5). A message's bytes are
\ BORROWED until the handler returns.
\
\ SEND-TEXT, SEND-BINARY and CLOSE may be called from any task, the handler
\ included: each appends whole frames to the socket's outgoing queue under one
\ TASK:FACILITY and pokes the wake pipe, and the task writes the queue when
\ the socket is writable, so a sender never waits for the peer. The queue
\ grows as frames arrive faster than the peer takes them, up to QUEUE-MAX
\ unwritten bytes: a frame queued behind that many loses the socket instead
\ (1006, its queue dropped), and the send is E-WS-CLOSED. A frame is never
\ refused for its own size. A send on a socket that is closing, closed or gone
\ is E-WS-CLOSED, and text that is not UTF-8 is E-WS-TEXT before anything is
\ queued.
\
\ The task never waits inside a transfer: it reads with TCP4:TRY-READ and
\ writes with TCP4:TRY-SEND once the poll reports the socket ready, and a
\ transfer that finds nothing to move after all (`empty`) leaves the socket to
\ the next poll.
\
\ A socket also closes under 1006, its queue dropped, when it gives up (GIVE-UP?):
\ its peer has taken no byte of a waiting queue for STALL-MS, has not answered
\ this end's CLOSE within STALL-MS, or WS has been stopping for GOODBYE-MS.
\ STOP sends 1001 to every open socket, closes each once its queue is written
\ or it gives up, and joins the task. A socket already closing gets no 1001:
\ it stays closing, and its handler is told its peer's answer, or 1006 once it
\ gives up. A task still in its body past STOP-BOUND-MS is killed. The kill
\ lands at the task's next TASK:PAUSE, which AIO:AWAIT-ANY reaches only once
\ nothing is ready (src/lib/task.f): a handler parked in a blocking call
\ (TCP4:READ, a database call) runs to its end once the call returns, the task
\ may go on serving until then, and STOP waits that long. Each socket left is
\ then closed at once: its peer may get no 1001 and its handler may not be
\ told.
\
\ No subprotocol and no extension is negotiated, and the 101 carries no header
\ line the handler added to the response.
\
\ STORAGE CLASS. PROCESS-WIDE tables indexed by socket slot, CAP of them,
\ mapped by START for its limit under LOCK while WS's task is not running.
\ Nothing moves a slot while any task may read one, and STOP never releases
\ them: the next START keeps them, or grows them under LOCK. A socket's message
\ buffer and scratch are one mapping, its input and outgoing queue two more;
\ ACCEPT makes them and the task gives them back as the socket ends. The
\ facility is made ready while this file loads, before any task is live, and
\ never destroyed.

require src/lib/errors.f
require src/lib/fd.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/utf8-scalar.f
require src/lib/base64.f
require src/lib/crypto/sha1.f
require src/lib/net/tcp4.f
require src/lib/net/http.f
require src/lib/net/ws-frame.f

package WS
public

\ One accepted connection: its slot and the count of retirements that slot had
\ seen when it opened, so a handle kept past its socket names no later one.
NEWTYPE socket

\ What a handler is handed: a whole text or binary message, its bytes and
\ their count; or that the socket has closed, with the status it closed under.
ENUM message
   VARIANT text FIELD chars ptr u8 FIELD count n ;VARIANT
   VARIANT binary FIELD bytes ptr u8 FIELD len n ;VARIANT
   VARIANT closed FIELD status n ;VARIANT
;ENUM

\ What ACCEPT answers: the socket of a handshake that passed, or the HTTP
\ status, 400, 426 or 503, a refused one is answered with once the handler
\ returns.
ENUM accepted
   VARIANT open FIELD sock socket ;VARIANT
   VARIANT refused FIELD status n ;VARIANT
;ENUM

$100000 constant MESSAGE-MAX      \ the longest message the task reassembles

\ How long a peer may take no byte of a waiting queue, or leave this end's
\ close unanswered, before its socket closes under 1006.
5000 constant STALL-MS

private

CAST: >SOCKET ( n -- socket )
CAST: SOCKET>N ( socket -- n )
CAST: BLEN>N ( NUM:byte-len -- n )
CAST: TICKET>N ( AIO:ticket -- n )

variable CAP                                \ START's limit: the slots the tables hold

\ A socket's life. Every change of phase is made under LOCK.
0 constant PHASE-NONE             \ a free slot
1 constant PHASE-OPEN             \ frames go both ways
2 constant PHASE-CLOSING          \ this end has sent its close frame and awaits the peer's
3 constant PHASE-CLOSED           \ the status is final; the socket ends once its queue is written
4 constant PHASE-RESERVED         \ ACCEPT is filling the slot

\ Close statuses (section 7.4.1).
1000 constant STATUS-NORMAL
1001 constant STATUS-AWAY         \ WS is stopping
1002 constant STATUS-PROTOCOL
1005 constant STATUS-NONE         \ reported for a close frame that carried no status; never sent
1006 constant STATUS-LOST         \ reported for a stream that ended with no close frame; never sent
1007 constant STATUS-DATA
1009 constant STATUS-TOO-BIG
1011 constant STATUS-INTERNAL

\ The slot is the low 32 bits of a handle and the slot's retirement count the
\ rest, so a limit past SLOT-MASK is E-WS-LIMIT and the count wraps at GEN-MASK.
32 constant GEN-SHIFT
$FFFFFFFF constant SLOT-MASK
$FFFFFFFF constant GEN-MASK

1000000 constant NS-PER-MS
10 constant LF
$C8 constant GOODBYE-MS           \ how long a stop waits for the 1001 frames to be written
$C8 constant STOP-MARGIN-MS       \ scheduling slack over GOODBYE-MS
$100000 constant TASK-STACK
$18 constant NONCE-CHARS          \ sixteen bytes in base64
$10 constant NONCE-BYTES
$18 constant NONCE-CAP            \ what 24 characters can decode to, to the cell
$1C constant ACCEPT-CHARS         \ a twenty-byte digest in base64
$4000 constant RX-CAP             \ the most one read takes
$1000 constant TX-CAP             \ a queue's first mapping
$40 constant WAKE-CHUNK           \ bytes taken off the wake pipe at once
$400000 constant QUEUE-MAX        \ unwritten bytes a socket's queue holds before a frame loses it

\ One socket's mapping: the message being reassembled, a control frame's
\ payload, the header being read, and the digest behind the accept value.
$80 constant CONTROL-CAP          \ 125 bytes, to the cell
$10 constant HEAD-CAP             \ HEADER-MAX bytes, to the cell
MESSAGE-MAX constant CONTROL-OFF
CONTROL-OFF CONTROL-CAP + constant HEAD-OFF
HEAD-OFF HEAD-CAP + constant CTX-OFF
CTX-OFF SHA1:CTX-BYTES + constant DIGEST-OFF
DIGEST-OFF SHA1:DIGEST-BYTES + constant REGION-BYTES

DYNAMIC-BUFFER LINK TCP4:connection
DYNAMIC-BUFFER HANDLER [ socket message -- ]
DYNAMIC-BUFFER REGION SPAN:span<u8>
DYNAMIC-BUFFER IN SPAN:span<u8>       \ bytes read and not yet parsed
DYNAMIC-BUFFER IN-AT n
DYNAMIC-BUFFER IN-U n
DYNAMIC-BUFFER OUT SPAN:span<u8>      \ the outgoing queue
DYNAMIC-BUFFER OUT-AT n               \ its first byte not yet written
DYNAMIC-BUFFER OUT-U n                \ its bytes queued
DYNAMIC-BUFFER SINCE n                \ when the queue last moved, or was first filled
DYNAMIC-BUFFER CLOSE-BY n             \ when a CLOSE gives up on the peer's answer
DYNAMIC-BUFFER GEN n                  \ retirements
DYNAMIC-BUFFER PHASE n
DYNAMIC-BUFFER STATUS n               \ the status a closed socket reports
DYNAMIC-BUFFER TICKET AIO:ticket
DYNAMIC-BUFFER ARMED n                \ the mask TICKET polls for; 0 when no poll is in the group
DYNAMIC-BUFFER ARMED-DUE n            \ the deadline TICKET carries; 0 for none
DYNAMIC-BUFFER HEAD-U n               \ header bytes held of the frame being read
DYNAMIC-BUFFER HEAD-WANT n            \ header bytes to hold before decoding again
DYNAMIC-BUFFER HEAD-DONE n            \ set once the header is decoded
DYNAMIC-BUFFER F-FIN n
DYNAMIC-BUFFER F-WIRE n               \ the frame's opcode, as on the wire
DYNAMIC-BUFFER F-LEN n
DYNAMIC-BUFFER F-KEY n
DYNAMIC-BUFFER PAY-U n                \ payload bytes of it read
DYNAMIC-BUFFER FAILING n              \ the status a faulty frame closes under
DYNAMIC-BUFFER MSG-U n                \ bytes of the message being reassembled
DYNAMIC-BUFFER MSG-WIRE n             \ its opcode; 0 while there is none

\ Every column holds n slots. A column that already holds them is left as it
\ is; fresh pages are zero, a free slot (PHASE-NONE). A refused mapping throws
\ with CAP unchanged.
: TABLES-RESERVE ( n -- ) {: n:n :}
   n LINK-RESERVE
   n HANDLER-RESERVE
   n REGION-RESERVE
   n IN-RESERVE
   n IN-AT-RESERVE
   n IN-U-RESERVE
   n OUT-RESERVE
   n OUT-AT-RESERVE
   n OUT-U-RESERVE
   n SINCE-RESERVE
   n CLOSE-BY-RESERVE
   n GEN-RESERVE
   n PHASE-RESERVE
   n STATUS-RESERVE
   n TICKET-RESERVE
   n ARMED-RESERVE
   n ARMED-DUE-RESERVE
   n HEAD-U-RESERVE
   n HEAD-WANT-RESERVE
   n HEAD-DONE-RESERVE
   n F-FIN-RESERVE
   n F-WIRE-RESERVE
   n F-LEN-RESERVE
   n F-KEY-RESERVE
   n PAY-U-RESERVE
   n FAILING-RESERVE
   n MSG-U-RESERVE
   n MSG-WIRE-RESERVE ;

\ The handshake's scratch, a row to each HTTP worker that may run ACCEPT.
HTTP:MAX-WORKERS TYPED-BUFFER KEY-AT ptr u8 \ Sec-WebSocket-Key as it was sent
HTTP:MAX-WORKERS TYPED-BUFFER KEY-U n
HTTP:MAX-WORKERS NONCE-CAP * SPAN-BUFFER: NONCES

TASK:FACILITY LOCK
TASK-STACK TASK:TASK WS-TASK
AIO:GROUP WAITERS
TYPED-VARIABLE WAKE-TICKET AIO:ticket
1 TYPED-BUFFER STOPPER ptr n                \ the task STOP waits in
variable WAKE-R
variable WAKE-W
variable POKED                              \ set once a byte is owed to the wake pipe; under LOCK
variable PIPED                              \ set while the wake pipe is open; under LOCK
variable STOPPING                           \ set by STOP; under LOCK
variable STOP-AT                            \ when the task gives up on the 1001 frames; 0 until it saw the stop
variable LEFT                               \ set under LOCK once the task has given everything back
variable DUE-IDX                            \ the slot whose handler runs; the task's alone
TYPED-VARIABLE DUE-MSG message
WAKE-CHUNK BUFFER: WAKE-BUF
create WAKE-BYTE $01 c,

\ WS's run, under LOCK: idle; starting, claimed by a START that has not yet
\ activated the task; or running, from then until the end of STOP.
ENUM run idle starting running ;ENUM
TYPED-VARIABLE RUN run

: RUN-IDLE ( -- )
   construct run idle RUN ! ;

\ How a read toward a frame went: the bytes are all held, the input ran out
\ first, or the frame breaks the protocol.
ENUM progress through late fault ;ENUM

\ No task is live while this file loads, so the lock is made ready here.
LOCK TASK:FACILITY-INIT
RUN-IDLE


\ ---- handles -----------------------------------------------------------------

: >SLOT ( n -- n )
   SLOT-MASK and ;


: HANDLE ( n -- n ) {: idx:n :}
   idx GEN @ GEN-SHIFT lshift idx or ;


\ A handle names its slot's socket while the slot is within START's limit and
\ has not been retired since the socket opened. A slot past the limit is one
\ an earlier run held, whose socket is gone.
: CURRENT? ( n -- bool ) {: h:n :}
   h >SLOT {: idx:n :}
   idx CAP @ >= if false exit then
   idx PHASE @ {: phase:n :}
   h GEN-SHIFT rshift idx GEN @ = phase PHASE-NONE <> and phase PHASE-RESERVED <> and ;


\ ---- the socket's buffers ----------------------------------------------------

\ Each narrows a mapping. A slot with no mapping holds a span with no reach, so
\ every narrowing of it throws instead of addressing a null base.

: MSG-BUF ( n -- SPAN:span<u8> )
   REGION @ MESSAGE-MAX SPAN:TAKE ;


: CONTROL-BUF ( n -- SPAN:span<u8> )
   REGION @ CONTROL-OFF CONTROL-CAP SPAN:SUB ;


: HEAD-BUF ( n -- SPAN:span<u8> )
   REGION @ HEAD-OFF HEAD-CAP SPAN:SUB ;


: CTX-BUF ( n -- SPAN:span<u8> )
   REGION @ CTX-OFF SHA1:CTX-BYTES SPAN:SUB ;


: DIGEST-BUF ( n -- SPAN:span<u8> )
   REGION @ DIGEST-OFF SHA1:DIGEST-BYTES SPAN:SUB ;


: NONCE-BUF ( n -- SPAN:span<u8> )
   NONCE-CAP * NONCES swap NONCE-CAP SPAN:SUB ;


: NEW-SPAN ( n -- SPAN:span<u8> )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;


\ The mapping is given back, and a span with no reach is left in its place.
: DROP-SPAN ( SPAN:span<u8> -- SPAN:span<u8> ) {: s :}
   s SPAN:LEN 0= if s exit then
   s MEM:FREE-SPAN
   s 0 SPAN:TAKE ;


\ ---- the wake pipe -----------------------------------------------------------

\ At most one byte is owed at a time, so the pipe never fills and a poke from
\ the task itself never blocks it. The task drains the pipe before it clears
\ POKED and scans after, so a change made before a poke is always seen. Once
\ STOP has closed the pipe a poke writes nothing, since its descriptor may
\ name another file by then.
: POKE ( -- )
   LOCK TASK:GET
   PIPED @ 0 <> POKED @ 0= and if
      1 POKED !
      WAKE-W @ WAKE-BYTE 1 write drop
   then
   LOCK TASK:RELEASE ;


: WAKE-ARM ( -- )
   WAKE-R @ >FD AIO:READABLE -1 >MS AIO:POLL {: t:AIO:ticket :}
   t WAITERS AIO:GROUP+
   t WAKE-TICKET ! ;


: WAKE-DRAIN ( -- )
   WAKE-R @ WAKE-BUF WAKE-CHUNK read drop
   LOCK TASK:GET
   0 POKED !
   LOCK TASK:RELEASE ;


: SAME-TICKET? ( AIO:ticket AIO:ticket -- bool )
   TICKET>N swap TICKET>N = ;


: OUTCOME-DROP ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;


\ A ticket the task no longer wants leaves the group and is awaited here, so
\ the group never holds two tickets for one socket. A poll is level-triggered:
\ readiness it reported is reported again to the next.
: UNWAIT ( AIO:ticket -- ) {: t:AIO:ticket :}
   t WAITERS AIO:GROUP-
   t AIO:CANCEL
   t AIO:AWAIT OUTCOME-DROP ;


\ ---- the outgoing queue -------------------------------------------------------

: QUEUED ( n -- n ) {: idx:n :}
   idx OUT-U @ idx OUT-AT @ - ;


\ Room for that many more bytes behind the queue. A full mapping is replaced
\ by one that holds the bytes still queued and the new ones, at least twice its
\ size, with the queued bytes moved to its start.
: ROOM ( n n -- ) {: idx:n more:n :}
   idx OUT-U @ more + idx OUT @ SPAN:LEN <= if exit then
   idx QUEUED {: live:n :}
   idx OUT @ SPAN:LEN 2 * live more + max NEW-SPAN {: fresh :}
   idx OUT @ idx OUT-AT @ live SPAN:SUB SPAN:$ fresh SPAN:COPY
   idx OUT @ DROP-SPAN drop
   fresh idx OUT !
   0 idx OUT-AT !
   live idx OUT-U ! ;


\ Bytes behind the queue. A queue that was empty starts its stall clock.
: QUEUE ( n ptr u8 n -- ) {: idx:n a u:n :}
   idx QUEUED 0= if mono-ns idx SINCE ! then
   idx u ROOM
   a u idx OUT @ idx OUT-U @ SPAN:SKIP SPAN:COPY
   u idx OUT-U +! ;


\ An unfragmented frame's header as a server sends it, with room behind it for
\ its payload.
: HEADER-OUT ( n opcode n -- ) {: idx:n op u:n :}
   idx QUEUED 0= if mono-ns idx SINCE ! then
   idx HEAD-CAP u + ROOM
   true op u 0 WS-HEADER:MAKE WS-SENDER:server idx OUT @ idx OUT-U @ SPAN:SKIP ENCODE-HEADER
   idx OUT-U +! ;


: FRAME-OUT ( n opcode ptr u8 n -- ) {: idx:n op a u:n :}
   idx op u HEADER-OUT
   idx a u QUEUE ;


\ A close frame carrying the status; an empty one for the status that stands
\ for none, and no frame for the status that stands for a lost connection.
: CLOSE-OUT ( n n -- ) {: idx:n code:n :}
   code STATUS-LOST = if exit then
   code STATUS-NONE = if 0 else CODE-BYTES then {: u:n :}
   idx WS-OPCODE:close u HEADER-OUT
   u 0= if exit then
   code >CLOSE-CODE idx OUT @ idx OUT-U @ SPAN:SKIP CLOSE-CODE!
   u idx OUT-U +! ;


\ ---- under the lock -----------------------------------------------------------

\ The socket ends under 1006 with its queue dropped; the task finishes it.
: LOST ( n -- ) {: idx:n :}
   STATUS-LOST idx STATUS !
   PHASE-CLOSED idx PHASE !
   0 idx OUT-AT !
   0 idx OUT-U ! ;


\ Whether a frame would be queued behind QUEUE-MAX unwritten bytes. Such a
\ socket is lost.
: OVERFLOW? ( n -- bool ) {: idx:n :}
   idx QUEUED QUEUE-MAX < if false exit then
   idx LOST
   true ;

\ The sections below run with LOCK held, given back by UNLOCK however they end.

: UNLOCK ( -- )
   LOCK TASK:RELEASE ;

\ Whether the slot's socket takes a frame of this opcode: an open one takes any,
\ and one that has sent its close still owes control frames, the pong a ping
\ asks for (section 5.5.2), though no more data (section 5.5.1).
: TAKES? ( n opcode -- bool ) {: idx:n op :}
   idx PHASE @ {: phase:n :}
   phase PHASE-OPEN = if true exit then
   phase PHASE-CLOSING = op CONTROL? and ;


\ A frame through a handle; 1 when it was queued, 0 when the socket does not
\ take it.
: SHIP ( n n ptr u8 n -- n ) {: h:n wire:n a u:n :}
   h CURRENT? 0= if 0 exit then
   h >SLOT {: idx:n :}
   idx wire WIRE>OPCODE TAKES? 0= if 0 exit then
   idx OVERFLOW? if 0 exit then
   idx wire WIRE>OPCODE a u FRAME-OUT
   1 ;


\ A frame through a handle, from any task.
: SEND ( n n ptr u8 n -- ) {: h:n wire:n a u:n :}
   u 0 < if E-WS-LENGTH throw then
   LOCK TASK:GET
   h wire a u [: SHIP ;] [: UNLOCK ;] finally {: got:n :}
   POKE
   got 0= if E-WS-CLOSED throw then ;


\ The socket moves toward its end under that status. An open one sends the
\ close frame the status calls for. By the phase asked for: closing, an open
\ socket awaits the peer's close; closed, a socket not closed already reports
\ the status. True when the close frame overflowed the queue and the socket
\ was lost instead.
: SHUT ( n n n -- bool ) {: idx:n code:n after:n :}
   idx PHASE @ {: phase:n :}
   phase PHASE-OPEN = if
      idx OVERFLOW? if true exit then
      idx code CLOSE-OUT
   then
   phase PHASE-CLOSED = if false exit then
   after PHASE-CLOSING = if
      phase PHASE-OPEN <> if false exit then
      mono-ns STALL-MS NS-PER-MS * + idx CLOSE-BY !
      PHASE-CLOSING idx PHASE ! false exit
   then
   code idx STATUS !
   PHASE-CLOSED idx PHASE ! false ;


: LOCKED-SHUT ( n n n -- ) {: idx:n code:n after:n :}
   LOCK TASK:GET
   idx code after [: SHUT ;] [: UNLOCK ;] finally drop ;


\ ---- writing ------------------------------------------------------------------

\ One send of the queue that never waits, made once the poll found room. False
\ once the connection failed.
: FLUSH ( n -- bool ) {: idx:n :}
   idx QUEUED 0= if true exit then
   idx LINK @ idx OUT @ idx OUT-AT @ idx QUEUED SPAN:SUB SPAN:$ TCP4:TRANSFER-BYTES TCP4:TRY-SEND
   MATCH TCP4:slice-result
      moved OF BLEN>N idx OUT-AT +! mono-ns idx SINCE ! ENDOF
      empty OF ENDOF
      failed OF drop false exit ENDOF
   ;MATCH
   idx QUEUED 0= if 0 idx OUT-AT ! 0 idx OUT-U ! then
   true ;


\ Appenders may move the queue's mapping, so it is written under the lock.
: LOCKED-FLUSH ( n -- bool ) {: idx:n :}
   LOCK TASK:GET
   idx [: FLUSH ;] [: UNLOCK ;] finally ;


\ ---- a socket's end ------------------------------------------------------------

\ The one rule a socket gives up by: its peer has taken no byte of a waiting
\ queue for STALL-MS, has left this end's close unanswered past CLOSE-BY, or
\ WS has been stopping for GOODBYE-MS (STOP-AT). Under LOCK.
: GIVE-UP? ( n -- bool ) {: idx:n :}
   idx QUEUED 0 > mono-ns idx SINCE @ - STALL-MS NS-PER-MS * >= and if true exit then
   idx PHASE @ PHASE-CLOSING = mono-ns idx CLOSE-BY @ >= and if true exit then
   STOP-AT @ 0 <> mono-ns STOP-AT @ >= and ;


\ The earliest moment the socket may give up, or 0 when nothing bounds it.
\ Under LOCK.
: DUE ( n -- n ) {: idx:n :}
   0
   idx QUEUED 0 > if drop idx SINCE @ STALL-MS NS-PER-MS * + then
   idx PHASE @ PHASE-CLOSING = if
      idx CLOSE-BY @ over 0= if nip else min then
   then
   STOP-AT @ 0 <> if STOP-AT @ over 0= if nip else min then then ;


: DISARM ( n -- ) {: idx:n :}
   idx ARMED @ 0= if exit then
   0 idx ARMED !
   idx TICKET @ UNWAIT ;


: RUN-HANDLER ( -- )
   DUE-IDX @ {: idx:n :}
   idx HANDLE >SOCKET DUE-MSG @ idx HANDLER @ execute ;


\ Ends the line in the builder with the throw code and writes it to stderr.
: WRITE-THROW ( n -- )
   FMT:SB-INT
   LF SB-APPEND-C
   FD:STDERR FD>N SB$ write drop ;

\ A handler's throw reaches the operator: one line on stderr with the socket's
\ handle and the code. The builder is task-local, and this is WS's task.
: REPORT-FAULT ( n n -- ) {: idx:n code:n :}
   SB-RESET
   s" ws: socket " SB-APPEND
   idx HANDLE FMT:SB-U
   s"  failed, throw " SB-APPEND
   code WRITE-THROW ;


\ The handler, handed the message on the task. True when it threw, its throw
\ reported.
: HANDED? ( n message -- bool ) {: idx:n msg :}
   idx DUE-IDX !
   msg DUE-MSG !
   [: RUN-HANDLER ;] catch {: code:n :}
   code 0= if false exit then
   idx code REPORT-FAULT
   true ;


\ Under LOCK: the slot free, its handles stale and its mappings given back.
: VACATE ( n -- ) {: idx:n :}
   idx GEN @ 1+ GEN-MASK and idx GEN !
   PHASE-NONE idx PHASE !
   0 idx ARMED !
   idx OUT @ DROP-SPAN idx OUT !
   0 idx OUT-AT !
   0 idx OUT-U !
   idx REGION @ DROP-SPAN idx REGION !
   idx IN @ DROP-SPAN idx IN ! ;


: RETIRE ( n -- )
   LOCK TASK:GET
   VACATE
   LOCK TASK:RELEASE ;


\ CLOSE's section: true when the close frame overflowed the queue.
: CLOSE-SHUT ( n n -- bool ) {: h:n code:n :}
   h CURRENT? 0= if false exit then
   h >SLOT code PHASE-CLOSING SHUT ;


\ The connection is closed and the handler told, once; a throw from it is
\ reported and has no socket left to close. The slot is retired as the handler ends, however it
\ ends, a halt included: the handler can still name the socket, and CLEANUP
\ never closes the connection a second time.
: FINISH ( n -- ) {: idx:n :}
   idx DISARM
   idx LINK @ TCP4:CLOSE drop
   idx idx STATUS @ WS-MESSAGE:closed [: HANDED? drop ;] [: DUE-IDX @ RETIRE ;] finally ;


\ The connection failed, its peer stalled, or the socket gave up: it ends
\ under 1006 with its queue dropped.
: LOSE ( n -- ) {: idx:n :}
   LOCK TASK:GET
   idx LOST
   LOCK TASK:RELEASE
   idx FINISH ;


\ ---- reading frames ----------------------------------------------------------

\ Bytes into the span from those read and not yet parsed: their count, 0 when
\ there are none.
: PULL ( n SPAN:span<u8> -- n ) {: idx:n dst :}
   dst SPAN:LEN idx IN-U @ min {: take:n :}
   idx IN @ idx IN-AT @ take SPAN:SUB SPAN:$ dst SPAN:COPY
   take idx IN-AT +!
   take negate idx IN-U +!
   take ;


: THROUGH? ( progress -- bool )
   MATCH progress
      through OF true ENDOF
      late OF false ENDOF
      fault OF false ENDOF
   ;MATCH ;


: FRAME-RESET ( n -- ) {: idx:n :}
   0 idx HEAD-U !
   2 idx HEAD-WANT !
   0 idx HEAD-DONE !
   0 idx PAY-U ! ;


: KEEP-FRAME ( header n n -- ) {: h size:n idx:n :}
   h WS-HEADER:UNMAKE {: fin:bool op len:n key:n :}
   fin if 1 else 0 then idx F-FIN !
   op OPCODE>WIRE idx F-WIRE !
   len idx F-LEN !
   key idx F-KEY !
   1 idx HEAD-DONE ! ;


\ What the bytes held say: the count the header needs, or the header itself.
\ The codec throws what breaks the protocol, so this runs under a catch.
: DECODE-HELD ( n -- n ) {: idx:n :}
   idx HEAD-BUF SPAN:$ drop idx HEAD-U @ WS-SENDER:client MESSAGE-MAX DECODE-HEADER
   MATCH decoded
      need OF idx HEAD-WANT ! ENDOF
      frame OF idx KEEP-FRAME ENDOF
   ;MATCH
   idx ;


: FRAME-FAULT? ( n -- bool ) {: code:n :}
   code E-WS-RSV <= code E-WS-MASK >= and ;


: CONTROL-FRAME? ( n -- bool )
   F-WIRE @ WIRE>OPCODE CONTROL? ;


\ 0 for a frame that may come now, else the status it fails with. A message's
\ first frame names its kind and the rest continue it (section 5.4), and a
\ message has MESSAGE-MAX bytes of room.
: ORDER-STATUS ( n -- n ) {: idx:n :}
   idx CONTROL-FRAME? if 0 exit then
   idx F-WIRE @ 0= idx MSG-WIRE @ 0 <> xor if STATUS-PROTOCOL exit then
   idx MSG-U @ idx F-LEN @ + MESSAGE-MAX > if STATUS-TOO-BIG exit then
   0 ;


\ 0 while the header bytes held break no rule, else the status they fail with.
\ A throw that is not a frame's fault is not this word's to answer.
: HEADER-STATUS ( n -- n ) {: idx:n :}
   idx [: DECODE-HELD ;] catch {: code:n :} drop
   code 0 <> if
      code FRAME-FAULT? 0= if code throw then
      code E-WS-TOO-BIG = if STATUS-TOO-BIG exit then
      STATUS-PROTOCOL exit
   then
   idx HEAD-DONE @ 0= if 0 exit then
   idx ORDER-STATUS ;


\ The header is decoded after every pull, so a fault shows in the fewest bytes
\ and a header the input cut short is taken up where it stopped.
: READ-HEADER ( n -- progress ) {: idx:n :}
   begin
      idx HEAD-DONE @ 0 <> if construct progress through exit then
      idx idx HEAD-BUF idx HEAD-U @ idx HEAD-WANT @ idx HEAD-U @ - SPAN:SUB PULL
      {: got:n :}
      got 0= if construct progress late exit then
      got idx HEAD-U +!
      idx HEADER-STATUS {: status:n :}
      status 0 <> if status idx FAILING ! construct progress fault exit then
   again ;


\ Where the frame in hand keeps its payload: a control frame's in the control
\ buffer, a data frame's behind what its message already holds.
: PAYLOAD ( n -- SPAN:span<u8> ) {: idx:n :}
   idx CONTROL-FRAME? if idx CONTROL-BUF idx F-LEN @ SPAN:TAKE exit then
   idx MSG-BUF idx MSG-U @ idx F-LEN @ SPAN:SUB ;


: READ-PAYLOAD ( n -- progress ) {: idx:n :}
   idx PAY-U @ idx F-LEN @ < if
      idx idx PAYLOAD idx PAY-U @ SPAN:SKIP PULL idx PAY-U +!
   then
   idx PAY-U @ idx F-LEN @ >= if construct progress through exit then
   construct progress late ;


\ One whole frame, its payload unmasked where it lies. The payload is unmasked
\ in one piece, so the key starts at its first byte.
: FRAME-IN ( n -- progress ) {: idx:n :}
   idx READ-HEADER {: head:progress :}
   head THROUGH? 0= if head exit then
   idx READ-PAYLOAD {: body:progress :}
   body THROUGH? 0= if body exit then
   idx F-KEY @ idx PAYLOAD MASK
   construct progress through ;


\ ---- what a frame means ------------------------------------------------------

: MESSAGE$ ( n -- ptr u8 n ) {: idx:n :}
   idx MSG-BUF idx MSG-U @ SPAN:TAKE SPAN:$ ;


: TEXT? ( n -- bool )
   WS-OPCODE:text OPCODE>WIRE = ;


\ The whole message goes to the handler, and its bytes are reused after it.
\ A handler that throws is reported and closes its socket under 1011.
: DELIVER ( n -- ) {: idx:n :}
   idx MESSAGE$ {: a u:n :}
   idx MSG-WIRE @ TEXT? if a u WS-MESSAGE:text else a u WS-MESSAGE:binary then {: msg :}
   0 idx MSG-U !
   0 idx MSG-WIRE !
   idx msg HANDED? if idx STATUS-INTERNAL PHASE-CLOSED LOCKED-SHUT then ;


\ A data frame joins its message and the last one completes it. Text is checked
\ whole, because a scalar may straddle two fragments.
: DATA-IN ( n -- ) {: idx:n :}
   idx F-LEN @ idx MSG-U +!
   idx F-WIRE @ 0 <> if idx F-WIRE @ idx MSG-WIRE ! then
   idx F-FIN @ 0= if exit then
   idx MSG-WIRE @ TEXT? if
      idx MESSAGE$ UTF8:VALID? 0= if idx STATUS-DATA PHASE-CLOSED LOCKED-SHUT exit then
   then
   idx DELIVER ;


\ The pong carries the ping's payload.
: PING-IN ( n -- ) {: idx:n :}
   idx HANDLE WS-OPCODE:pong OPCODE>WIRE idx CONTROL-BUF idx F-LEN @ SPAN:TAKE SPAN:$
   LOCK TASK:GET
   [: SHIP ;] [: UNLOCK ;] finally drop ;


\ The statuses a close frame may carry (section 7.4): those RFC 6455 and the
\ registry define, 1000 to 1014 without the three that are never sent, and the
\ ranges left to libraries and applications, 3000 to 4999.
: WIRE-STATUS? ( n -- bool ) {: code:n :}
   code 3000 >= code 4999 <= and if true exit then
   code 1000 < code 1014 > or if false exit then
   code 1004 < code 1006 > or ;


\ The status the close frame in hand closes the socket under: the one it
\ carries, 1005 when it carries none, and the fault's own status when the
\ payload is half a status, a status no frame may carry, or a reason that is
\ not UTF-8.
: CLOSE-STATUS ( n -- n ) {: idx:n :}
   idx F-LEN @ {: u:n :}
   u 0= if STATUS-NONE exit then
   u CODE-BYTES < if STATUS-PROTOCOL exit then
   idx CONTROL-BUF SPAN:$ drop CODE-BYTES BE@ {: code:n :}
   code WIRE-STATUS? 0= if STATUS-PROTOCOL exit then
   idx CONTROL-BUF CODE-BYTES u CODE-BYTES - SPAN:SUB SPAN:$ UTF8:VALID? 0= if STATUS-DATA exit then
   code ;


: TURN ( n -- ) {: idx:n :}
   idx F-WIRE @ WIRE>OPCODE
   MATCH opcode
      continuation OF idx DATA-IN ENDOF
      text OF idx DATA-IN ENDOF
      binary OF idx DATA-IN ENDOF
      close OF idx idx CLOSE-STATUS PHASE-CLOSED LOCKED-SHUT ENDOF
      ping OF idx PING-IN ENDOF
      pong OF ENDOF
   ;MATCH
   idx FRAME-RESET ;


: LIVE? ( n -- bool ) {: idx:n :}
   idx PHASE @ {: phase:n :}
   phase PHASE-OPEN = phase PHASE-CLOSING = or ;


\ Every frame the bytes read hold, while the socket still reads. A closed
\ socket's input is dropped.
: PARSE ( n -- ) {: idx:n :}
   begin
      idx LIVE? 0= if 0 idx IN-U ! exit then
      idx FRAME-IN
      MATCH progress
         through OF idx TURN ENDOF
         late OF exit ENDOF
         fault OF idx idx FAILING @ PHASE-CLOSED LOCKED-SHUT ENDOF
      ;MATCH
   again ;


\ One read that never waits. A stream that ends or fails ends the socket: under
\ the status it already closed with, or lost.
: INPUT ( n -- ) {: idx:n :}
   idx LINK @ idx IN @ RX-CAP SPAN:TAKE SPAN:$ TCP4:TRANSFER-BYTES TCP4:TRY-READ
   MATCH TCP4:try-read-result
      data OF BLEN>N idx IN-U ! 0 idx IN-AT ! idx PARSE ENDOF
      empty OF ENDOF
      closed OF drop idx LIVE? if idx LOSE else 0 idx OUT-AT ! 0 idx OUT-U ! idx FINISH then ENDOF
      failed OF drop idx LOSE ENDOF
   ;MATCH ;


\ ---- the task -----------------------------------------------------------------

: STOP-FLAG? ( -- bool )
   STOPPING @ 0 <> ;


\ Whether START has activated the task and no STOP has ended since.
: RUNNING? ( -- bool )
   RUN @
   MATCH run
      idle OF false ENDOF
      starting OF false ENDOF
      running OF true ENDOF
   ;MATCH ;


: PHASE-SET? ( n -- bool ) {: idx:n :}
   idx PHASE @ {: phase:n :}
   phase PHASE-NONE <> phase PHASE-RESERVED <> and ;


\ The mask the socket's poll wants: input always, room while bytes wait.
: WANTED ( n -- n ) {: idx:n :}
   idx QUEUED 0 > if AIO:READABLE AIO:WRITABLE or exit then
   AIO:READABLE ;


\ What is left until the deadline, as a poll takes it: a part of a
\ millisecond counts as a whole one, so a poll that times out has outlasted it.
: REMAINING ( n -- ms ) {: deadline:n :}
   deadline 0= if -1 >MS exit then
   deadline mono-ns - {: left:n :}
   left 0 <= if 0 >MS exit then
   left NS-PER-MS /mod {: part:n whole:n :}
   part 0 > if whole 1+ >MS exit then
   whole >MS ;


: ARM ( n n n -- ) {: idx:n mask:n due:n :}
   idx LINK @ TCP4:CONNECTION-FD mask due REMAINING AIO:POLL {: t:AIO:ticket :}
   t WAITERS AIO:GROUP+
   t idx TICKET !
   mask idx ARMED !
   due idx ARMED-DUE ! ;


\ What SYNC does next with a slot: nothing, since it holds no socket the task
\ serves; parse the bytes read; end a socket that gave up, or one closed with
\ its queue written; or poll with that mask and deadline.
ENUM course
   VARIANT vacant ;VARIANT
   VARIANT unparsed ;VARIANT
   VARIANT given-up ;VARIANT
   VARIANT spent ;VARIANT
   VARIANT waiting FIELD mask n FIELD due n ;VARIANT
;ENUM


\ The slot's course, under LOCK: its phase first, since LINK, IN and HANDLER
\ are the task's to read only once the slot is published. Bytes held
\ unparsed, the ones that rode behind the handshake, come first, since no
\ poll reports them; the stop turns an open socket to 1001, and a socket
\ already closing gets no 1001: it stays closing, and its handler is told its
\ peer's answer, or 1006 once it gives up.
: COURSE ( n -- course ) {: idx:n :}
   idx PHASE-SET? 0= if construct course vacant exit then
   idx IN-U @ 0 > if construct course unparsed exit then
   STOP-AT @ 0 <> idx PHASE @ PHASE-OPEN = and if idx STATUS-AWAY PHASE-CLOSED SHUT drop then
   idx GIVE-UP? if construct course given-up exit then
   idx PHASE @ PHASE-CLOSED = idx QUEUED 0= and if construct course spent exit then
   idx WANTED idx DUE construct course waiting ;


: LOCKED-COURSE ( n -- course )
   LOCK TASK:GET
   [: COURSE ;] [: UNLOCK ;] finally ;


\ The poll made again when what it waits for or its deadline changed.
: REARM ( n n n -- ) {: idx:n mask:n due:n :}
   idx ARMED @ mask = idx ARMED-DUE @ due = and if exit then
   idx DISARM
   idx mask due ARM ;


\ The socket after anything that may have changed it: one that gives up or
\ is spent ends, and the rest poll for what they now want.
: SYNC ( n -- ) {: idx:n :}
   begin
      idx LOCKED-COURSE
      MATCH course
         vacant OF exit ENDOF
         unparsed OF idx PARSE ENDOF
         given-up OF idx LOSE exit ENDOF
         spent OF idx FINISH exit ENDOF
         waiting OF idx -rot REARM exit ENDOF
      ;MATCH
   again ;


: SYNC-ALL ( -- )
   CAP @ 0 ?do i SYNC loop ;


\ The poll's readiness: room is written to, and anything else is read, which
\ is how an end of stream or an error is learnt.
: READY ( n n -- ) {: idx:n rev:n :}
   rev AIO:WRITABLE and 0 <> if
      idx LOCKED-FLUSH 0= if idx LOSE exit then
   then
   rev AIO:WRITABLE invert and 0 <> if idx INPUT then ;


: SLOT-OF ( AIO:ticket -- n ) {: t:AIO:ticket :}
   CAP @ 0 ?do
      i ARMED @ 0 <> if
         i TICKET @ t SAME-TICKET? if i unloop exit then
      then
   loop
   -1 ;


\ A poll that timed out is answered by SYNC, where its socket gives up.
: POLLED ( n AIO:outcome -- ) {: idx:n out :}
   0 idx ARMED !
   out
   MATCH AIO:outcome
      ready OF idx swap READY ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop idx LOSE ENDOF
   ;MATCH
   idx SYNC ;


\ The wake: the pipe is drained, the stop is noticed once, and every slot is
\ looked at again. The wake stays armed until SERVE leaves, so an ACCEPT that
\ ends while WS is stopping is still heard.
: WOKEN ( -- )
   WAKE-DRAIN
   STOP-FLAG? STOP-AT @ 0= and if mono-ns GOODBYE-MS NS-PER-MS * + STOP-AT ! then
   WAKE-ARM
   SYNC-ALL ;


: ANSWER ( AIO:ticket AIO:outcome -- ) {: t:AIO:ticket out :}
   t WAKE-TICKET @ SAME-TICKET? if out OUTCOME-DROP WOKEN exit then
   t SLOT-OF {: idx:n :}
   idx 0 < if out OUTCOME-DROP exit then
   idx out POLLED ;


: ANY-SET? ( -- bool )
   LOCK TASK:GET
   false
   CAP @ 0 ?do i PHASE @ PHASE-NONE <> if drop true leave then loop
   LOCK TASK:RELEASE ;


\ Everything the task holds is given back however it ends - its body done, or
\ killed past STOP-BOUND-MS: every connection closed and every socket
\ retired. LEFT is set first, so an ACCEPT has either published its socket for
\ this scan to close or finds LEFT and takes nothing. The stopper is told last.
\ SERVE registers this before the task's first AIO submission registers AIO's
\ own cleanup, which therefore runs first: it has given back every record the
\ task submitted and emptied WAITERS, so no ticket is left to take out of it.
: CLEANUP ( -- )
   LOCK TASK:GET
   1 LEFT !
   LOCK TASK:RELEASE
   CAP @ 0 ?do
      i PHASE-SET? if
         i LINK @ TCP4:CLOSE drop
         i RETIRE
      then
   loop
   0 STOPPER @ TASK:WAKE ;


: SERVING ( -- )
   WAKE-ARM
   SYNC-ALL
   begin
      STOP-AT @ 0 <> ANY-SET? 0= and if exit then
      WAITERS AIO:AWAIT-ANY ANSWER
   again ;


\ The task's throw reaches the operator as a handler's does, as one line on
\ stderr, built on the task.
: REPORT-END ( n -- ) {: code:n :}
   SB-RESET
   s" ws: task failed, throw " SB-APPEND
   code WRITE-THROW ;


\ WS's task. A throw from its loop - a poll AIO refuses at its ceiling, a
\ mapping the task cannot get - is reported and rethrown, so TASK:THROW@ keeps
\ it and CLEANUP closes every socket: each handshake is then 503 until STOP
\ and START. A halt passes the catch unreported (src/lib/task.f).
: SERVE ( -- )
   [: CLEANUP ;] TASK:SELF TASK:AT-EXIT
   [: SERVING ;] catch {: code:n :}
   code 0= if exit then
   code REPORT-END
   code throw ;


\ ---- the handshake -----------------------------------------------------------

\ The value of a header the request carries exactly once.
: SOLE ( HTTP:request ptr u8 n -- ptr u8 n bool ) {: asked:HTTP:request name nu:n :}
   asked name nu HTTP:HEADER-COUNT-OF 1 <> if name 0 false exit then
   asked name nu HTTP:HEADER-OF$ ;


: LISTS? ( HTTP:request ptr u8 n ptr u8 n -- bool )
   {: asked:HTTP:request name nu:n token tu:n :}
   asked name nu SOLE {: va vu:n found:bool :}
   found 0= if false exit then
   va vu token tu HTTP:LIST-HAS? ;


\ The count of bytes the key decodes to.
: DECODE-NONCE ( n -- n )
   {: w:n :}
   w KEY-AT @ w KEY-U @ w NONCE-BUF BASE64:DECODE ;


\ The key is the base64 of sixteen bytes (section 4.2.1), and is kept as it
\ was sent for the accept value. A throw that is not base64's own refusal is
\ not this word's to answer.
: KEY-OK? ( HTTP:request n -- bool )
   {: asked:HTTP:request w:n :}
   asked s" sec-websocket-key" SOLE {: va vu:n found:bool :}
   found 0= if false exit then
   vu NONCE-CHARS <> if false exit then
   va w KEY-AT !
   vu w KEY-U !
   w [: DECODE-NONCE ;] catch {: got code:n :}
   code 0= if got NONCE-BYTES = exit then
   code E-BASE64-FIRST > code E-BASE64-LAST < or if code throw then
   false ;


\ An HTTP/1.1 GET (section 4.2.1, item 1). Item 2, exactly one valid Host, the
\ server checks before any handler runs (src/lib/net/http-request.f HOST-OK?).
: GET-OK? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked HTTP:METHOD$ s" GET" STR= 0= if false exit then
   asked HTTP:VERSION$ s" HTTP/1.1" STR= ;


\ 0 for an opening handshake this server accepts, else the HTTP status that
\ refuses it: 426 when all that is wrong is the version (section 4.4). The
\ scratch is the HTTP worker's row.
: VERDICT ( HTTP:request n -- n ) {: asked:HTTP:request w:n :}
   asked GET-OK? 0= if 400 exit then
   asked s" upgrade" s" websocket" LISTS? 0= if 400 exit then
   asked s" connection" s" upgrade" LISTS? 0= if 400 exit then
   asked w KEY-OK? 0= if 400 exit then
   asked s" sec-websocket-version" SOLE {: va vu:n found:bool :}
   found 0= if 426 exit then
   va vu s" 13" STR= 0= if 426 exit then
   0 ;


\ The refusal, answered through the response once the route handler returns.
: REFUSE ( HTTP:response n -- accepted ) {: answer:HTTP:response status:n :}
   status 503 = if
      answer 503 s" websocket_unavailable" s" this server has no room for another WebSocket" HTTP:ERROR!
      503 WS-ACCEPTED:refused exit
   then
   status 426 = if
      answer s" Upgrade" s" websocket" HTTP:HEADER!
      answer s" Sec-WebSocket-Version" s" 13" HTTP:HEADER!
      answer 426 s" upgrade_required" s" this server speaks WebSocket version 13" HTTP:ERROR!
      426 WS-ACCEPTED:refused exit
   then
   answer 400 s" bad_handshake" s" the request is not a WebSocket opening handshake" HTTP:ERROR!
   400 WS-ACCEPTED:refused ;


: GUID$ ( -- ptr u8 n )
   s" 258EAFA5-E914-47DA-95CA-C5AB0DC85B11" ;


\ The accept value (section 4.2.2), the base64 of the SHA-1 of the key as it
\ was sent and the protocol's GUID, behind the queue.
: ACCEPT-VALUE ( n n -- ) {: idx:n w:n :}
   idx CTX-BUF SHA1:START
   idx CTX-BUF w KEY-AT @ w KEY-U @ SHA1:FEED
   idx CTX-BUF GUID$ SHA1:FEED
   idx CTX-BUF SHA1:FINISH idx DIGEST-BUF SHA1:DIGEST!
   idx ACCEPT-CHARS ROOM
   idx DIGEST-BUF SPAN:$ idx OUT @ idx OUT-U @ SPAN:SKIP BASE64:ENCODE idx OUT-U +! ;


\ The 101 answer, first in the queue.
: SWITCHING ( n n -- ) {: idx:n w:n :}
   idx S\" HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n" QUEUE
   idx S\" Connection: Upgrade\r\nSec-WebSocket-Accept: " QUEUE
   idx w ACCEPT-VALUE
   idx S\" \r\n\r\n" QUEUE ;


\ ---- a socket's birth -----------------------------------------------------------

\ A free slot, now ACCEPT's, or -1 when there is none or WS is not running or
\ is stopping.
: RESERVE ( -- n )
   LOCK TASK:GET
   -1
   RUNNING? STOP-FLAG? 0= and LEFT @ 0= and if
      CAP @ 0 ?do
         i PHASE @ PHASE-NONE = if drop i PHASE-RESERVED i PHASE ! leave then
      loop
   then
   LOCK TASK:RELEASE ;


\ Nagle's algorithm off, so a frame leaves at once instead of waiting for the
\ peer to acknowledge the one before it.
: NAGLE-OFF? ( TCP4:connection -- bool )
   true TCP4:NODELAY!
   MATCH TCP4:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ The slot's mappings and state, all but its input. A mapping that fails
\ throws with the ones before it in the slot, for RETIRE to give back.
: FILL ( n -- ) {: idx:n :}
   REGION-BYTES NEW-SPAN idx REGION !
   TX-CAP NEW-SPAN idx OUT !
   0 idx OUT-AT !
   0 idx OUT-U !
   idx FRAME-RESET
   0 idx MSG-U !
   0 idx MSG-WIRE ! ;


\ The slot's input, the bytes the peer sent behind its handshake first.
: FILL-INPUT ( n ptr u8 n -- n ptr u8 n ) {: idx:n early eu:n :}
   RX-CAP eu max NEW-SPAN idx IN !
   early eu idx IN @ SPAN:COPY
   0 idx IN-AT !
   eu idx IN-U !
   idx early eu ;


\ Under LOCK: the request's connection taken into the slot and the socket
\ published to the task, open with the 101 queued, or closed from birth under
\ 1006 when its connection refuses TCP_NODELAY. An input mapping that fails
\ closes the connection before the throw goes on, so nothing holds it.
: TAKE ( HTTP:request n -- HTTP:request n ) {: asked:HTTP:request idx:n :}
   asked HTTP:TAKE-OVER {: conn:TCP4:connection early eu:n :}
   idx early eu [: FILL-INPUT ;] catch {: code:n :} 2drop drop
   code 0 <> if conn TCP4:CLOSE drop code throw then
   conn idx LINK !
   conn NAGLE-OFF? if PHASE-OPEN else STATUS-LOST idx STATUS ! 0 idx OUT-U ! PHASE-CLOSED then idx PHASE !
   asked idx ;


\ Under LOCK: the connection is taken unless WS is stopping or its task has
\ given everything back, when nobody would serve the socket. The code TAKE
\ threw, or 0, and true when nothing was taken.
: ADOPT ( HTTP:request n -- n bool ) {: asked:HTTP:request idx:n :}
   STOP-FLAG? LEFT @ 0 <> or if 0 true exit then
   asked idx [: TAKE ;] catch {: code:n :} 2drop
   code false ;


\ The slot filled and the 101 queued, the connection not taken yet.
: PREPARE ( n n -- n n ) {: idx:n w:n :}
   idx FILL
   idx w SWITCHING
   idx w ;


\ A slot RESERVE gave out, retired; a STOP waiting for it is told, and the
\ task is poked, since a stopping task ends only once it sees no slot set.
: GIVE-BACK ( n -- )
   LOCK TASK:GET
   VACATE
   STOPPING @ 0 <> if 0 STOPPER @ TASK:WAKE then
   LOCK TASK:RELEASE
   POKE ;


\ The reserved slot becomes the request's socket, or, when WS began stopping
\ since RESERVE, is given back and the request refused with 503. A throw, from
\ a request already taken or from a mapping, gives the slot back before it
\ goes on, so no slot stays reserved and no connection open.
: OPEN-SOCKET ( HTTP:request HTTP:response [ socket message -- ] n n -- accepted )
   {: asked:HTTP:request answer:HTTP:response q idx:n w:n :}
   q idx HANDLER !
   idx HANDLE {: h:n :}
   idx w [: PREPARE ;] catch {: failed:n :} 2drop
   failed 0 <> if idx GIVE-BACK failed throw then
   LOCK TASK:GET
   asked idx ADOPT {: code:n gone:bool :}
   LOCK TASK:RELEASE
   code 0 <> if idx GIVE-BACK code throw then
   gone if idx GIVE-BACK answer 503 REFUSE exit then
   POKE
   h >SOCKET WS-ACCEPTED:open ;


: PIPE-OPEN ( -- )
   pipe {: r:n w:n rc:n :}
   rc 0 <> if E-WS-WAKE throw then
   r WAKE-R !
   w WAKE-W !
   LOCK TASK:GET
   0 POKED !
   1 PIPED !
   LOCK TASK:RELEASE ;


\ No poke writes to the pipe once it is closed.
: PIPE-CLOSE ( -- )
   LOCK TASK:GET
   0 PIPED !
   LOCK TASK:RELEASE
   WAKE-R @ close-rc drop
   WAKE-W @ close-rc drop ;


\ How long STOP waits for the task to end its own body.
: STOP-BOUND-MS ( -- n )
   GOODBYE-MS STOP-MARGIN-MS + ;


: AWAIT-LEFT ( -- )
   STOP-BOUND-MS NS-PER-MS * mono-ns + {: deadline:n :}
   begin
      LEFT @ 0 <> if exit then
      mono-ns deadline >= if exit then
      deadline REMAINING TASK:STOP-FOR drop
   again ;


\ Under LOCK: a handshake inside ACCEPT holds a slot.
: RESERVED? ( -- bool )
   false
   CAP @ 0 ?do i PHASE @ PHASE-RESERVED = if drop true leave then loop ;


\ Each handshake inside ACCEPT finds WS stopping and gives its slot back, so
\ none fills a slot once STOP has returned and a START may move the tables.
: AWAIT-GIVEN ( -- )
   begin
      LOCK TASK:GET
      RESERVED?
      LOCK TASK:RELEASE
      0= if exit then
      TASK:STOP
   again ;


\ Under LOCK: this task takes the stop, or false on WS's own task, which STOP
\ joins, outside a run or while START still starts it, or when another STOP
\ holds it.
: CLAIM-STOP ( -- bool )
   TASK:SELF WS-TASK = if false exit then
   RUNNING? 0= STOP-FLAG? or if false exit then
   TASK:SELF 0 STOPPER !
   1 STOPPING !
   true ;


\ START's part under LOCK: the tables mapped for n sockets, then the limit and
\ the stop's state set for the new run.
: SET-UP ( n -- ) {: n:n :}
   n TABLES-RESERVE
   n CAP !
   0 STOPPING !
   0 STOP-AT !
   0 LEFT ! ;


\ Under LOCK: this START claims the run, or false when WS is not idle.
: CLAIM-START ( -- bool )
   RUN @
   MATCH run
      idle OF construct run starting RUN ! true ENDOF
      starting OF false ENDOF
      running OF false ENDOF
   ;MATCH ;


\ Under LOCK: the run START claimed is running once its task is activated, or
\ idle again when START threw first.
: SETTLE ( n -- ) {: code:n :}
   code 0= if construct run running RUN ! else RUN-IDLE then ;


: ACTIVATE-TASK ( -- )
   [: SERVE ;] WS-TASK TASK:ACTIVATE ;


\ START's part past its claim. The wake pipe is closed again when the task
\ cannot be activated.
: LAUNCH ( n -- n ) {: n:n :}
   AIO:RUNNING? 0= if E-AIO-STATE throw then
   n 1 < n SLOT-MASK > or if E-WS-LIMIT throw then
   LOCK TASK:GET
   n [: SET-UP ;] [: UNLOCK ;] finally
   TASK:SELF 0 STOPPER !
   PIPE-OPEN
   [: ACTIVATE-TASK ;] catch {: code:n :}
   code 0 <> if PIPE-CLOSE code throw then
   n ;


public

\ Starts WS's task, holding at most n sockets at once: a handshake past them
\ is answered 503 websocket_unavailable. Each socket maps REGION-BYTES (about
\ 1 MiB) and may queue QUEUE-MAX (4 MiB) more, so n is sized from memory and
\ the descriptor limit, as nginx's worker_connections is. n must also stay
\ under the platform's AIO ceiling, less the wake pipe: 10240 polled
\ descriptors on Darwin, 512 completion records on Linux (a poll with a
\ deadline holds two) until dot habu-grow-the-linux-fd2c8749; the socket that
\ crosses it ends WS's task, which closes every socket and answers 503 until
\ STOP and START. n below 1 or past SLOT-MASK is E-WS-LIMIT, and a refused
\ mapping throws its code with nothing started. The AIO loop must be running
\ (E-AIO-STATE), and a START while another starts, or before STOP has ended,
\ is E-WS-STATE. A live task forbids compilation (src/lib/task.f), so a
\ program starts WS once it has defined every word.
: START ( n -- ) {: n:n :}
   LOCK TASK:GET
   CLAIM-START
   LOCK TASK:RELEASE
   0= if E-WS-STATE throw then
   n [: LAUNCH ;] catch {: code:n :} drop
   LOCK TASK:GET
   code SETTLE
   LOCK TASK:RELEASE
   code 0 <> if code throw then ;


\ Sends status 1001 to every open socket, closes each once its frames are
\ written or within GOODBYE-MS, telling each handler `closed`, and joins the
\ task. A socket already closing gets no 1001: it stays closing, and its
\ handler is told its peer's answer, or 1006 once it gives up. A task still in
\ its body past STOP-BOUND-MS is killed. The kill lands at the task's next
\ TASK:PAUSE, which AIO:AWAIT-ANY reaches only once nothing is ready
\ (src/lib/task.f): a handler parked in a blocking call (TCP4:READ, a database
\ call) runs to its end once the call returns, the task may go on serving until
\ then, and STOP waits that long. Each socket left is then closed at once: its
\ peer may get no 1001 and its handler may not be told. A handshake inside
\ ACCEPT is answered 503 and STOP waits for it, however long its worker takes
\ to get there. Outside a run or while START still starts it, while another
\ STOP runs, or on WS's own task, from a handler, it is E-WS-STATE: STOP joins
\ that task.
: STOP ( -- )
   LOCK TASK:GET
   CLAIM-STOP
   LOCK TASK:RELEASE
   0= if E-WS-STATE throw then
   POKE
   AWAIT-LEFT
   WS-TASK TASK:KILL
   AWAIT-GIVEN
   PIPE-CLOSE
   LOCK TASK:GET
   RUN-IDLE
   LOCK TASK:RELEASE ;


\ Inside a route handler: the request is checked as an opening handshake, that
\ is an HTTP/1.1 GET whose Upgrade names websocket, whose Connection names
\ Upgrade, whose Sec-WebSocket-Key is the base64 of sixteen bytes and whose
\ Sec-WebSocket-Version is 13, each of those four arriving once; the server
\ refuses an HTTP/1.1 request without exactly one valid Host before any
\ handler runs. A handshake that passes takes the connection from the server
\ and `open` carries the socket; the task answers 101 and hands the handler
\ every message and, once, `closed`. A socket whose connection refuses
\ TCP_NODELAY is closed from birth under 1006. One that does not pass is
\ answered 400, or 426 naming version 13, and one past START's limit, or while
\ WS is stopping, 503, through the response once the route handler returns,
\ and `refused` carries that status: no socket exists for it. Origin is the
\ route handler's to check before it accepts. Outside a run or while START
\ still starts it, it is E-WS-STATE.
: ACCEPT ( HTTP:request HTTP:response [ socket message -- ] -- accepted )
   {: asked:HTTP:request answer:HTTP:response q :}
   RUNNING? 0= if E-WS-STATE throw then
   HTTP:WORKER-SLOT {: w:n :}
   asked w VERDICT {: status:n :}
   status 0 <> if answer status REFUSE exit then
   RESERVE {: idx:n :}
   idx 0 < if answer 503 REFUSE exit then
   asked answer q idx w OPEN-SOCKET ;


\ One text message in one frame, from any task. Bytes that are not UTF-8 are
\ E-WS-TEXT, since a text frame carries only UTF-8 (section 5.6). E-WS-CLOSED
\ when the socket is not open to it.
: SEND-TEXT ( socket ptr u8 n -- ) {: sock a u:n :}
   a u UTF8:VALID? 0= if E-WS-TEXT throw then
   sock SOCKET>N WS-OPCODE:text OPCODE>WIRE a u SEND ;


: SEND-BINARY ( socket ptr u8 n -- ) {: sock a u:n :}
   sock SOCKET>N WS-OPCODE:binary OPCODE>WIRE a u SEND ;


\ Starts the closing handshake under that status, from any task: the close
\ frame is queued, nothing may be sent behind it, and the handler goes on
\ receiving until the peer's close arrives, or for STALL-MS. A socket already
\ closing or closed, or gone, is left as it is. A status no frame may carry is
\ E-WS-CODE, and a close frame behind QUEUE-MAX unwritten bytes loses the
\ socket and is E-WS-CLOSED.
: CLOSE ( socket n -- ) {: sock code:n :}
   code WIRE-STATUS? 0= if E-WS-CODE throw then
   sock SOCKET>N {: h:n :}
   LOCK TASK:GET
   h code [: CLOSE-SHUT ;] [: UNLOCK ;] finally {: lost:bool :}
   POKE
   lost if E-WS-CLOSED throw then ;

;package
