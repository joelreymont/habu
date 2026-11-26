\ ws.f - RFC 6455 WebSocket connections on the HTTP/1.1 server.
\ ACCEPT takes a route handler's connection (HTTP:TAKE-OVER) for WS's one task,
\ the only reader and writer of every socket, which waits on one AIO group: a
\ readiness poll per socket and one on a wake pipe, none with a deadline, and
\ one timer, due when the first socket is to give up, be pinged or be lost.
\ Every handler call runs on a request worker (HTTP:SUBMIT) and carries the
\ value ACCEPT was given; `opened` comes first and alone, so whatever the
\ handler does there precedes every message, and `closed` last. Between them a
\ socket has at most the config's calls message calls out. A peer gone without
\ a word is found by ping (config). STOP waits for handler calls at most
\ GOODBYE-MS and STOP-MARGIN-MS: the last call of a socket it leaves running
\ retires the slot as it ends, and START is E-WS-STATE until then.
\ STORAGE CLASS. PROCESS-WIDE tables indexed by socket slot.

require src/lib/error.f
require src/lib/string.f
require src/lib/log.f
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
require src/lib/net/http-arena.f
require src/lib/net/http-request.f
require src/lib/net/http-response.f

package WS
public

\ One accepted connection: its slot and the count of retirements that slot had
\ seen when it opened, so a handle kept past its socket names no later one.
NEWTYPE socket

\ What a handler is handed, with the socket and the value ACCEPT was given:
\ first, once, that the socket has opened; then each whole text or binary
\ message as one str, its bytes BORROWED until the handler returns; and
\ last, once, that the socket has closed, unless a STOP passes its bound
\ first (STOP), with its status: the close frame's, 1005 when it carried
\ none, 1006 when the stream ended without one (section 7.1.5). A handler
\ runs on a request worker (HTTP:SUBMIT) and may block. `opened` runs first
\ and alone, and `closed` last, once every message call has ended; with the
\ config's calls above 1 a socket's message calls may run on several workers
\ at once and in any order, and with calls 1 each waits for the one before.
\ Its throw is one LOG line, tag ws, `socket <handle> failed, throw <code>
\ <name>: <text>`, name and text when the code is declared, and a throw on
\ `opened` or a message also closes the socket under 1011. A peer that
\ breaks the protocol fails the connection: 1002 for a frame the codec
\ rejects or one out of order, 1007 for text that is not UTF-8, 1009 for a
\ message past MESSAGE-MAX. A ping is answered with a pong even once this end
\ has sent its close.
ENUM message
   VARIANT opened ;VARIANT
   VARIANT text FIELD chars str ;VARIANT
   VARIANT binary FIELD bytes str ;VARIANT
   VARIANT closed FIELD status n ;VARIANT
;ENUM

\ What ACCEPT answers: the socket of a handshake that passed, or the HTTP
\ status, 400, 426 or 503, a rejected one is answered with once the handler
\ returns.
ENUM accepted
   VARIANT open FIELD sock socket ;VARIANT
   VARIANT rejected FIELD status n ;VARIANT
;ENUM

$100000 constant MESSAGE-MAX

\ How a WS run is sized and bounded; WS:START reads it once.
\ sockets bounds open sockets (past it, 503); each maps REGION-BYTES, queues
\ up to QUEUE-MAX and costs one AIO record, its poll, beside the task's two,
\ the wake's and the timer's. Size it from memory and descriptors: a socket is
\ one, beside the run's four, its waker's pipe and its task's queue's two.
\ An open socket from which no byte has arrived for ping ms is pinged, and
\ lost under 1006 once pong ms more pass with none, counted from when the
\ ping left the queue for the kernel. The count runs only while its read is
\ armed: bytes sent while the socket has calls calls out are not read until
\ one ends, so that time says nothing of the peer. Bytes written say nothing of
\ it either, since a vanished peer's kernel buffer still takes them.
\ stall is how many ms a peer may take no byte of a waiting queue, or leave
\ this end's close unanswered, before its socket closes under 1006.
\ calls bounds the message calls one socket has queued or running at once:
\ WS reads nothing more of it until one ends, so TCP holds the peer back.
\ Each costs a MESSAGE-MAX buffer of the socket's REGION-BYTES.
STRUCTURE config DERIVE addr
   FIELD sockets n
   FIELD calls n
   FIELD ping ms
   FIELD pong ms
   FIELD stall ms
;STRUCTURE

private

CAST: >SOCKET ( n -- socket )
CAST: SOCKET>N ( socket -- n )
CAST: BLEN>N ( NUM:byte-len -- n )

SCRATCH-VARIABLE CAP                        \ START's limit: the slots the tables hold
SCRATCH-VARIABLE CALLS                      \ the run's calls: each slot's call rows

\ The numbers the wake's wait and the timer's carry in WAITERS; a slot's wait
\ carries its index.
-1 constant WAKE-WAIT
-2 constant TIMER-WAIT

\ Every change of phase is made under LOCK.
0 constant PHASE-NONE             \ a free slot
1 constant PHASE-OPEN             \ frames go both ways
2 constant PHASE-CLOSING          \ this end has sent its close frame and awaits the peer's
3 constant PHASE-CLOSED           \ the status is final; the socket ends once its queue is written
4 constant PHASE-RESERVED         \ ACCEPT is filling the slot

\ A slot's delivery, read and written only under LOCK, by ACCEPT, by WS's
\ task, by STOP and by the request workers running the slot's calls. The
\ socket is free (no socket); opening, its `opened` queued or running, with
\ nothing parsed or read; taking(n), n message calls out (CALLS-OUT), its
\ frames read and parsed while n < CALLS and the phase allows (READING?); or
\ closing, its `closed` queued or running. Each slot has CALLS call rows, each
\ free, queued or running, owning one MESSAGE-MAX buffer of the slot's region
\ and holding the message its call is handed; `opened` and `closed` take row
\ 0. Inputs: handed (task), started (worker), ended (worker; ok or failed),
\ finish (task: status final and queue written, or given up), dropped (STOP
\ gives up on a queued call), left (the task has left).
\   free       ACCEPT     opening: `opened` queued before the task sees the
\                         slot (OPEN-CALL)
\   opening    started    row 0 queued -> running
\   opening    ended      taking(0); failed also shuts under 1011; with the
\                         task left: closed, free (LEFT-OFF)
\   opening    dropped    free
\   taking(n)  handed     n < CALLS: taking(n+1), the message reassembled in
\                         a free row's buffer queued in that row; the next
\                         message fills another free row (DELIVER)
\   taking(n)  started    the row queued -> running; a row not queued
\                         (dropped) runs nothing (STARTS?)
\   taking(n)  ended      taking(n-1), the row free; failed shuts under 1011
\                         while the other calls run on; with the task left
\                         and n-1 = 0: closed, free (MESSAGE-ENDED)
\   taking(n)  dropped    taking(n-1); at 0 with the task left: closed, free
\   taking(0)  finish     closing (FINISH); with n > 0 finish waits, each end
\                         poking the task
\   taking(0)  left       closed, free (CLEANUP); any other state waits for
\                         its calls' ends or STOP's drop
\   closing    started    row 0 queued -> running
\   closing    ended      free
\   closing    dropped    free
\ No other pairing arrives. A call's job names its row and the slot's
\ retirement count (ROW-HANDLE): a dropped call is stale by its row's state,
\ a retired socket's by the count.
0 constant DELIVERY-FREE
1 constant DELIVERY-OPENING
2 constant DELIVERY-TAKING
3 constant DELIVERY-CLOSING
0 constant ROW-FREE
1 constant ROW-QUEUED
2 constant ROW-RUNNING

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
\ rest, and a call's job names its row the same way, so sockets past SLOT-MASK,
\ or rows (sockets times calls), is E-WS-LIMIT; the count wraps at GEN-MASK.
32 constant GEN-SHIFT
$FFFFFFFF constant SLOT-MASK
$FFFFFFFF constant GEN-MASK

1000000 constant NS-PER-MS
$C8 constant GOODBYE-MS           \ how long a stop waits for the 1001 frames to be written
$C8 constant STOP-MARGIN-MS       \ scheduling slack over GOODBYE-MS

\ The run's config's ping, pong and stall in ns, set by SET-UP before its
\ task starts.
SCRATCH-VARIABLE PING-NS
SCRATCH-VARIABLE PONG-NS
SCRATCH-VARIABLE STALL-NS
$100000 constant TASK-STACK
$18 constant NONCE-CHARS          \ sixteen bytes in base64
$10 constant NONCE-BYTES
$18 constant NONCE-CAP            \ what 24 characters can decode to, to the cell
$1C constant ACCEPT-CHARS         \ a twenty-byte digest in base64
$4000 constant RX-CAP             \ the most one read takes
$1000 constant TX-CAP             \ a queue's first mapping
$400000 constant QUEUE-MAX        \ unwritten bytes a socket's queue holds before a frame loses it

\ One socket's mapping: a message buffer to each call row, then a tail of a
\ control frame's payload, the header being read, and the digest behind the
\ accept value; the offsets are the tail's.
$80 constant CONTROL-CAP          \ 125 bytes, to the cell
$10 constant HEAD-CAP             \ HEADER-MAX bytes, to the cell
0 constant CONTROL-OFF
CONTROL-OFF CONTROL-CAP + constant HEAD-OFF
HEAD-OFF HEAD-CAP + constant CTX-OFF
CTX-OFF SHA1:CTX-BYTES + constant DIGEST-OFF
DIGEST-OFF SHA1:DIGEST-BYTES + constant TAIL-BYTES

\ Where a spell's ping is: none yet; queued, with that many bytes of the
\ queue, its own included, still to be written before it has left; or sent,
\ at that time. Zero-filled storage reads as none.
ENUM pinging
   VARIANT none ;VARIANT
   VARIANT queued FIELD ahead n ;VARIANT
   VARIANT sent FIELD at n ;VARIANT
;ENUM

DYNAMIC-BUFFER LINK TCP4:connection
\ The tables: CAP slots, mapped by START under LOCK while WS's task is not
\ running. Nothing moves a slot while any task may read one, since START is
\ E-WS-STATE while a handler call holds one (CLAIM-START), and STOP never
\ releases them: the next START keeps them or grows them under LOCK. A socket's
\ message buffers and scratch are one mapping (REGION), its input and outgoing
\ queue two more; ACCEPT makes them and the task gives them back as the socket
\ ends.
DYNAMIC-BUFFER HANDLER [ socket n message -- ]
DYNAMIC-BUFFER HANDLER-ARG n          \ the value ACCEPT was given, passed to every call
DYNAMIC-BUFFER REGION SPAN:span<u8>
DYNAMIC-BUFFER IN SPAN:span<u8>       \ bytes read and not yet parsed
DYNAMIC-BUFFER IN-AT n
DYNAMIC-BUFFER IN-U n
DYNAMIC-BUFFER OUT SPAN:span<u8>      \ the outgoing queue
DYNAMIC-BUFFER OUT-AT n               \ its first byte not yet written
DYNAMIC-BUFFER OUT-U n                \ its bytes queued
DYNAMIC-BUFFER SINCE n                \ when the queue last moved, or was first filled
DYNAMIC-BUFFER CLOSE-BY n             \ when a CLOSE gives up on the peer's answer
DYNAMIC-BUFFER HEARD n                \ when the quiet spell began: bytes read or read armed
DYNAMIC-BUFFER PINGED pinging         \ where that spell's ping is
DYNAMIC-BUFFER GEN n                  \ retirements
DYNAMIC-BUFFER PHASE n
DYNAMIC-BUFFER STATUS n               \ the status a closed socket reports
DYNAMIC-BUFFER TICKET AIO:ticket
DYNAMIC-BUFFER ARMED n                \ the mask TICKET polls for; 0 when no poll is in the group
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
DYNAMIC-BUFFER MSG-ROW n              \ the row whose buffer that message fills
DYNAMIC-BUFFER DELIVERY n             \ DELIVERY-FREE, -OPENING, -TAKING or -CLOSING
DYNAMIC-BUFFER CALLS-OUT n            \ message calls queued or running, while taking
\ The row columns hold CALLS rows to each slot, the slot's first at its index
\ times CALLS (ROW).
DYNAMIC-BUFFER ROW-STATE n            \ ROW-FREE, ROW-QUEUED or ROW-RUNNING
DYNAMIC-BUFFER ROW-MSG message        \ what the row's call is handed

\ Every slot column holds n slots and every row column that many rows. A
\ column that already holds them is left as it is; fresh pages are zero, a
\ free slot (PHASE-NONE) or row (ROW-FREE). A failed mapping throws with CAP
\ unchanged.
: TABLES-RESERVE ( n n -- ) {: n:n rows:n :}
   n LINK-RESERVE
   n HANDLER-RESERVE
   n HANDLER-ARG-RESERVE
   n REGION-RESERVE
   n IN-RESERVE
   n IN-AT-RESERVE
   n IN-U-RESERVE
   n OUT-RESERVE
   n OUT-AT-RESERVE
   n OUT-U-RESERVE
   n SINCE-RESERVE
   n CLOSE-BY-RESERVE
   n HEARD-RESERVE
   n PINGED-RESERVE
   n GEN-RESERVE
   n PHASE-RESERVE
   n STATUS-RESERVE
   n TICKET-RESERVE
   n ARMED-RESERVE
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
   n MSG-WIRE-RESERVE
   n MSG-ROW-RESERVE
   n DELIVERY-RESERVE
   n CALLS-OUT-RESERVE
   rows ROW-STATE-RESERVE
   rows ROW-MSG-RESERVE ;

\ The handshake's scratch, a row to each HTTP worker that may run ACCEPT.
HTTP:MAX-WORKERS TYPED-BUFFER KEY-AT ^u8 SCRATCH-ONLY \ Sec-WebSocket-Key as it was sent
HTTP:MAX-WORKERS TYPED-BUFFER KEY-U n SCRATCH-ONLY
HTTP:MAX-WORKERS NONCE-CAP * constant NONCES-BYTES
create NONCE-ROWS NONCES-BYTES SCRATCH-ALLOT
: NONCES ( -- SPAN:span<u8> ) NONCE-ROWS NONCES-BYTES SPAN:MAKE ;

\ The handler call each request worker runs, for HAND-ENDED, which takes
\ nothing: its row, and whether it failed, as it has until the handler
\ returns with no throw, so a call a halt cuts short is a failed one.
HTTP:MAX-WORKERS TYPED-BUFFER CALL-ROW n SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER CALL-FAILED bool SCRATCH-ONLY

TASK:FACILITY LOCK
TASK-STACK TASK:TASK WS-TASK
AIO:GROUP WAITERS
\ The task's own: its one timer, in WAITERS under TIMER-WAIT, and when that
\ fires, 0 while none is outstanding.
TYPED-VARIABLE TIMER AIO:ticket SCRATCH-ONLY
SCRATCH-VARIABLE TIMER-DUE
1 TYPED-BUFFER STOPPER ^n SCRATCH-ONLY   \ the task STOP waits in
TYPED-VARIABLE WAKER AIO:waker SCRATCH-ONLY \ the wake pipe the task is poked through
SCRATCH-VARIABLE PIPED                      \ set while the wake pipe is open; under LOCK
SCRATCH-VARIABLE STOPPING                   \ set by STOP; under LOCK
SCRATCH-VARIABLE STOP-AT                    \ when the task gives up on the 1001 frames; 0 until it saw the stop
SCRATCH-VARIABLE LEFT                       \ set under LOCK once the task has given everything back

\ WS's run, under LOCK: idle, its zero; starting, claimed by a START that has not yet
\ activated the task; or running, from then until the end of STOP.
ENUM run idle starting running ;ENUM
TYPED-VARIABLE RUN run SCRATCH-ONLY

\ How a read toward a frame went: the bytes are all held, the input ran out
\ first, or the frame breaks the protocol.
ENUM progress through late fault ;ENUM

\ No task is live while this file loads, so the lock is made ready here; it is
\ never destroyed.
LOCK TASK:FACILITY-INIT


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


\ The slot's call row r.
: ROW ( n n -- n ) {: idx:n r:n :}
   idx CALLS @ * r + ;


: ROW-SLOT ( n -- n )
   CALLS @ / ;


\ A call's job: the row in a handle's slot field, under its slot's count.
: ROW-HANDLE ( n -- n ) {: row:n :}
   row ROW-SLOT GEN @ GEN-SHIFT lshift row or ;


: REGION-BYTES ( -- n )
   CALLS @ MESSAGE-MAX * TAIL-BYTES + ;


\ Each narrows a mapping. A slot with no mapping holds a span with no reach, so
\ every narrowing of it throws instead of addressing a null base.

\ The buffer of the row the message being reassembled fills.
: MSG-BUF ( n -- SPAN:span<u8> ) {: idx:n :}
   idx REGION @ idx MSG-ROW @ MESSAGE-MAX * MESSAGE-MAX SPAN:SUB ;


: TAIL ( n -- SPAN:span<u8> )
   REGION @ CALLS @ MESSAGE-MAX * SPAN:SKIP ;


: CONTROL-BUF ( n -- SPAN:span<u8> )
   TAIL CONTROL-OFF CONTROL-CAP SPAN:SUB ;


: HEAD-BUF ( n -- SPAN:span<u8> )
   TAIL HEAD-OFF HEAD-CAP SPAN:SUB ;


: CTX-BUF ( n -- SPAN:span<u8> )
   TAIL CTX-OFF SHA1:CTX-BYTES SPAN:SUB ;


: DIGEST-BUF ( n -- SPAN:span<u8> )
   TAIL DIGEST-OFF SHA1:DIGEST-BYTES SPAN:SUB ;


: NONCE-BUF ( n -- SPAN:span<u8> )
   NONCE-CAP * NONCES swap NONCE-CAP SPAN:SUB ;


: NEW-SPAN ( n -- SPAN:span<u8> )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;


: DROP-SPAN ( SPAN:span<u8> -- SPAN:span<u8> ) {: s :}
   s MEM:FREE-SPAN
   s 0 SPAN:TAKE ;


\ Once STOP has closed the pipe a poke writes nothing, since its descriptor
\ may name another file by then.
: POKE ( -- )
   LOCK TASK:GET
   PIPED @ 0 <> if WAKER AIO:WAKE then
   LOCK TASK:RELEASE ;


: WAKE-ARM ( -- )
   WAKER AIO:WAKER-POLL {: t:AIO:ticket :}
   t WAKE-WAIT WAITERS AIO:GROUP+ ;


: OUTCOME-DROP ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      rejected OF drop ENDOF
   ;MATCH ;


\ A ticket the task no longer wants leaves the group and is awaited here, its
\ outcome read past, so the group never holds two tickets for one socket or
\ two timers. A poll is level-triggered: readiness it reported is reported
\ again to the next.
: UNWAIT ( AIO:ticket -- ) {: t:AIO:ticket :}
   t WAITERS AIO:GROUP-
   t AIO:CANCEL
   t AIO:AWAIT OUTCOME-DROP ;


: QUEUED ( n -- n ) {: idx:n :}
   idx OUT-U @ idx OUT-AT @ - ;


: ROOM ( n n -- ) {: idx:n more:n :}
   idx OUT-U @ more + idx OUT @ SPAN:LEN <= if exit then
   idx QUEUED {: live:n :}
   idx OUT @ SPAN:LEN 2 * live more + max NEW-SPAN {: fresh :}
   idx OUT @ idx OUT-AT @ live SPAN:SUB SPAN:$ fresh SPAN:COPY
   idx OUT @ DROP-SPAN drop
   fresh idx OUT !
   0 idx OUT-AT !
   live idx OUT-U ! ;


: QUEUE ( n str -- ) {: idx:n s:str :}
   idx QUEUED 0= if mono-ns idx SINCE ! then
   idx s STR:LEN ROOM
   s idx OUT @ idx OUT-U @ SPAN:SKIP SPAN:COPY
   s STR:LEN idx OUT-U +! ;


: HEADER-OUT ( n opcode n -- ) {: idx:n op u:n :}
   idx QUEUED 0= if mono-ns idx SINCE ! then
   idx HEAD-CAP u + ROOM
   true op u 0 WS-HEADER:MAKE WS-SENDER:SERVER idx OUT @ idx OUT-U @ SPAN:SKIP ENCODE-HEADER
   idx OUT-U +! ;


: FRAME-OUT ( n opcode str -- ) {: idx:n op s:str :}
   idx op s STR:LEN HEADER-OUT
   idx s QUEUE ;


: CLOSE-OUT ( n n -- ) {: idx:n code:n :}
   code STATUS-LOST = if exit then
   code STATUS-NONE = if 0 else CODE-BYTES then {: u:n :}
   idx WS-OPCODE:CLOSE u HEADER-OUT
   u 0= if exit then
   code >CLOSE-CODE idx OUT @ idx OUT-U @ SPAN:SKIP CLOSE-CODE!
   u idx OUT-U +! ;


\ The socket ends under 1006 with its queue dropped; the task finishes it.
: LOST ( n -- ) {: idx:n :}
   STATUS-LOST idx STATUS !
   PHASE-CLOSED idx PHASE !
   0 idx OUT-AT !
   0 idx OUT-U ! ;


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


: SHIP ( n n str -- n ) {: h:n wire:n s:str :}
   h CURRENT? 0= if 0 exit then
   h >SLOT {: idx:n :}
   idx wire WIRE>OPCODE TAKES? 0= if 0 exit then
   idx OVERFLOW? if 0 exit then
   idx wire WIRE>OPCODE s FRAME-OUT
   1 ;


: SEND ( n n str -- ) {: h:n wire:n s:str :}
   s STR:LEN 0 < if E-WS-LENGTH throw then
   LOCK TASK:GET
   h wire s [: SHIP ;] [: UNLOCK ;] finally {: got:n :}
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
      mono-ns STALL-NS @ + idx CLOSE-BY !
      PHASE-CLOSING idx PHASE ! false exit
   then
   code idx STATUS !
   PHASE-CLOSED idx PHASE ! false ;


: LOCKED-SHUT ( n n n -- ) {: idx:n code:n after:n :}
   LOCK TASK:GET
   idx code after [: SHUT ;] [: UNLOCK ;] finally drop ;


\ A ping with that many bytes still to be written before it has left; one
\ with none has left now.
: PING-AHEAD ( n -- pinging ) {: ahead:n :}
   ahead 0 > if ahead construct pinging queued exit then
   mono-ns construct pinging sent ;


\ Under LOCK: the queue moved by that many bytes, which may carry the spell's
\ ping to the kernel.
: MOVED ( n n -- ) {: idx:n u:n :}
   u idx OUT-AT +!
   mono-ns idx SINCE !
   idx PINGED @
   MATCH pinging
      none OF ENDOF
      queued OF u - PING-AHEAD idx PINGED ! ENDOF
      sent OF drop ENDOF
   ;MATCH ;


: FLUSH ( n -- bool ) {: idx:n :}
   idx QUEUED 0= if true exit then
   idx LINK @ idx OUT @ idx OUT-AT @ idx QUEUED SPAN:SUB SPAN:$ TCP4:TRY-SEND
   MATCH TCP4:slice-result
      moved OF BLEN>N idx swap MOVED ENDOF
      empty OF ENDOF
      failed OF drop false exit ENDOF
   ;MATCH
   idx QUEUED 0= if 0 idx OUT-AT ! 0 idx OUT-U ! then
   true ;


\ Appenders may move the queue's mapping, so it is written under the lock.
: LOCKED-FLUSH ( n -- bool ) {: idx:n :}
   LOCK TASK:GET
   idx [: FLUSH ;] [: UNLOCK ;] finally ;


\ Under LOCK.
: GIVE-UP? ( n -- bool ) {: idx:n :}
   idx QUEUED 0 > mono-ns idx SINCE @ - STALL-NS @ >= and if true exit then
   idx PHASE @ PHASE-CLOSING = mono-ns idx CLOSE-BY @ >= and if true exit then
   STOP-AT @ 0 <> mono-ns STOP-AT @ >= and ;


\ The earliest moment the socket may give up, or 0 when nothing bounds it.
\ Under LOCK.
: DUE ( n -- n ) {: idx:n :}
   0
   idx QUEUED 0 > if drop idx SINCE @ STALL-NS @ + then
   idx PHASE @ PHASE-CLOSING = if
      idx CLOSE-BY @ over 0= if nip else min then
   then
   STOP-AT @ 0 <> if STOP-AT @ over 0= if nip else min then then ;


\ Under LOCK, by the task on bytes read and by the worker whose call's end arms
\ the read: the quiet clock starts again, and the spell has no ping.
: HEARD-NOW ( n -- ) {: idx:n :}
   mono-ns idx HEARD !
   construct pinging none idx PINGED ! ;


\ When an open socket's ping is due, or once the ping has left the queue,
\ when the socket is lost; 0 for one not open or whose ping is still queued,
\ since the stall rule (GIVE-UP?) bounds a queue that stops moving. Under
\ LOCK, while its read is armed.
: QUIET-DUE ( n -- n ) {: idx:n :}
   idx PHASE @ PHASE-OPEN <> if 0 exit then
   idx PINGED @
   MATCH pinging
      none OF PING-NS @ idx HEARD @ + ENDOF
      queued OF drop 0 ENDOF
      sent OF PONG-NS @ + ENDOF
   ;MATCH ;


\ A due, 0 being none, has come.
: PASSED? ( n -- bool ) {: due:n :}
   due 0 <> mono-ns due >= and ;


\ Under LOCK: the spell has no ping yet.
: UNPINGED? ( n -- bool )
   PINGED @
   MATCH pinging
      none OF true ENDOF
      queued OF drop false ENDOF
      sent OF drop false ENDOF
   ;MATCH ;


\ Under LOCK: an open socket quiet for the run's ping is pinged, once a
\ spell. A ping behind queued bytes leaves once FLUSH has written them
\ (MOVED); one on an empty queue goes on the task's next pass, so it counts
\ as left at once.
: PROBE ( n -- ) {: idx:n :}
   idx UNPINGED? 0= if exit then
   idx QUIET-DUE PASSED? 0= if exit then
   idx OVERFLOW? if exit then
   idx QUEUED {: before:n :}
   idx WS-OPCODE:PING 0 HEADER-OUT
   before 0 > if idx QUEUED else 0 then PING-AHEAD idx PINGED ! ;


\ Under LOCK: the ping went unanswered for the run's pong after it left the
\ queue.
: SILENT? ( n -- bool ) {: idx:n :}
   idx PINGED @
   MATCH pinging
      none OF false ENDOF
      queued OF drop false ENDOF
      sent OF drop idx QUIET-DUE PASSED? ENDOF
   ;MATCH ;


\ The earlier of two dues, 0 being none.
: SOONER ( n n -- n ) {: a:n b:n :}
   a 0= if b exit then
   b 0= if a exit then
   a b min ;


\ DUE, or QUIET-DUE when that comes first, for a slot whose read is armed.
: READY-DUE ( n -- n ) {: idx:n :}
   idx DUE idx QUIET-DUE SOONER ;


: DISARM ( n -- ) {: idx:n :}
   idx ARMED @ 0= if exit then
   0 idx ARMED !
   idx TICKET @ UNWAIT ;


: REPORT-FAULT ( n n -- )
   {: idx:n code:n :}
   LOG:ERROR s" ws" LOG:LINE
   s" socket " LOG:TEXT+  idx HANDLE LOG:INT+
   s"  failed, throw " LOG:TEXT+  code LOG:CODE+
   LOG:;LINE ;


\ Under LOCK: the slot free, its handles stale and its mappings given back.
: VACATE ( n -- ) {: idx:n :}
   idx GEN @ 1+ GEN-MASK and idx GEN !
   PHASE-NONE idx PHASE !
   DELIVERY-FREE idx DELIVERY !
   0 idx CALLS-OUT !
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


: CLOSE-SHUT ( n n -- bool ) {: h:n code:n :}
   h CURRENT? 0= if false exit then
   h >SLOT code PHASE-CLOSING SHUT ;


\ Under LOCK: the slot's read is armed, its frames read and parsed: it is
\ taking, below CALLS calls out, and with none once it is closed, since it is
\ then finished.
: READING? ( n -- bool ) {: idx:n :}
   idx DELIVERY @ DELIVERY-TAKING <> if false exit then
   idx CALLS-OUT @ {: out:n :}
   idx PHASE @ PHASE-CLOSED = if out 0= exit then
   out CALLS @ < ;


: LOCKED-READING? ( n -- bool ) {: idx:n :}
   LOCK TASK:GET
   idx READING?
   UNLOCK ;


\ The row is queued or running, so nothing else writes its ROW-MSG, or its
\ slot's HANDLER, HANDLER-ARG or GEN, until the call ends.
: CALL-HANDLER ( n -- n ) {: row:n :}
   row ROW-SLOT {: idx:n :}
   idx HANDLE >SOCKET idx HANDLER-ARG @ row ROW-MSG @ idx HANDLER @ execute
   row ;


\ Under LOCK: a closing socket's peer has the run's stall, from when its read
\ is armed again, to answer, since nothing was read while it was not.
: EXTEND-CLOSE ( n -- ) {: idx:n :}
   idx PHASE @ PHASE-CLOSING <> if exit then
   mono-ns STALL-NS @ + idx CLOSE-BY @ max idx CLOSE-BY ! ;


\ Under LOCK: the slot has one message call fewer out, or, at the end of
\ `opened`, is taking.
: CALL-OFF ( n -- ) {: idx:n :}
   idx DELIVERY @ DELIVERY-OPENING = if
      DELIVERY-TAKING idx DELIVERY ! exit
   then
   -1 idx CALLS-OUT +! ;


\ Under LOCK, once the task has left (CLEANUP), which would serve the socket
\ no more: its last call's end closes it and retires the slot.
: LEFT-OFF ( n -- ) {: idx:n :}
   idx CALLS-OUT @ 0 <> if exit then
   idx LINK @ TCP4:CLOSE drop
   idx VACATE ;


\ Under LOCK: `opened` or a message call ended, or was dropped, in that row.
\ A failed call closes the socket under 1011 while its other calls run on.
\ An end that arms the read again starts the quiet clock and a closing
\ peer's time to answer over.
: MESSAGE-ENDED ( n n bool -- ) {: idx:n row:n failed:bool :}
   idx READING? {: before:bool :}
   ROW-FREE row ROW-STATE !
   idx CALL-OFF
   LEFT @ 0 <> if idx LEFT-OFF exit then
   failed if idx STATUS-INTERNAL PHASE-CLOSED SHUT drop then
   before idx READING? 0= or if exit then
   idx EXTEND-CLOSE
   idx HEARD-NOW ;


\ Under LOCK: the end of `closed` retires the slot, and any other is
\ MESSAGE-ENDED's.
: END-CALL ( n n bool -- ) {: idx:n row:n failed:bool :}
   idx DELIVERY @ DELIVERY-CLOSING = if
      ROW-FREE row ROW-STATE !
      idx VACATE exit
   then
   idx row failed MESSAGE-ENDED ;


\ Under LOCK, on the worker: the row's call ended, and a STOP waiting for
\ calls is told.
: CALL-ENDED ( n bool -- ) {: row:n failed:bool :}
   row ROW-SLOT row failed END-CALL
   STOPPING @ 0 <> if 0 STOPPER @ TASK:WAKE then ;


: RUN-CALL ( n -- n ) {: row:n :}
   row [: CALL-HANDLER ;] catch {: code:n :} drop
   code 0 <> if row ROW-SLOT code REPORT-FAULT row exit then
   false HTTP:WORKER-SLOT CALL-FAILED !
   row ;


\ HAND's cleanup, on the worker, however the call ended: the slot moves on
\ under LOCK and the task is poked to serve it.
: HAND-ENDED ( -- )
   HTTP:WORKER-SLOT {: w:n :}
   LOCK TASK:GET
   w CALL-ROW @ w CALL-FAILED @ [: CALL-ENDED ;] [: UNLOCK ;] finally
   POKE ;


\ Under LOCK: the call submitted for that job starts, its row running, or
\ false when STOP dropped it while it was queued (DROP-QUEUED): its row is
\ not queued, or its slot was retired since.
: STARTS? ( n -- bool ) {: h:n :}
   h >SLOT {: row:n :}
   h SLOT-MASK invert and row ROW-SLOT or CURRENT? 0= if false exit then
   row ROW-STATE @ ROW-QUEUED <> if false exit then
   ROW-RUNNING row ROW-STATE !
   true ;


\ A handler call, on a request worker, for the job of its row. A halt that
\ cuts the call short, as HTTP:STOP's kill does, still ends it, since a halt
\ unwinds through `finally`; a call that does not start runs nothing.
: HAND ( n -- ) {: h:n :}
   LOCK TASK:GET
   h [: STARTS? ;] [: UNLOCK ;] finally
   0= if exit then
   h >SLOT {: row:n :}
   HTTP:WORKER-SLOT {: w:n :}
   row w CALL-ROW !
   true w CALL-FAILED !
   row [: RUN-CALL ;] [: HAND-ENDED ;] finally drop ;


: QUEUE-HAND ( n -- n ) {: row:n :}
   row ROW-HANDLE [: HAND ;] HTTP:SUBMIT
   row ;


\ Under LOCK: the row's call is queued with that message.
: QUEUE-ROW ( n message -- ) {: row:n msg :}
   msg row ROW-MSG !
   ROW-QUEUED row ROW-STATE ! ;


\ Under LOCK: a call HTTP:SUBMIT rejected never runs, so its row is free
\ again, or the slot retired for a `closed` nobody is told.
: UNHAND ( n n -- ) {: idx:n row:n :}
   ROW-FREE row ROW-STATE !
   idx DELIVERY @ DELIVERY-CLOSING = if idx VACATE exit then
   -1 idx CALLS-OUT +! ;


\ The row's queued call is handed to the slot's handler on a request worker.
\ A throw from HTTP:SUBMIT goes on once the row is given back (UNHAND).
: HAND-OFF ( n n -- ) {: idx:n row:n :}
   row [: QUEUE-HAND ;] catch {: code:n :} drop
   code 0= if exit then
   LOCK TASK:GET
   idx row [: UNHAND ;] [: UNLOCK ;] finally
   code throw ;


\ Under LOCK: the message is queued in the row whose buffer holds it, which
\ the task chose below CALLS calls out (ROW-FOR).
: TAKE-MESSAGE ( n message -- n ) {: idx:n msg :}
   idx idx MSG-ROW @ ROW {: row:n :}
   row msg QUEUE-ROW
   1 idx CALLS-OUT +!
   row ;


\ Under LOCK: `closed`, with the socket's status, is queued in row 0.
: TAKE-CLOSED ( n -- n ) {: idx:n :}
   idx 0 ROW {: row:n :}
   row idx STATUS @ WS-MESSAGE:CLOSED QUEUE-ROW
   DELIVERY-CLOSING idx DELIVERY !
   row ;


\ The phase is closed. Only at taking(0), which only the task leaves, is the
\ connection closed and `closed` queued in row 0, whose end retires the slot.
: FINISH ( n -- ) {: idx:n :}
   idx LOCKED-READING? 0= if exit then
   idx DISARM
   idx LINK @ TCP4:CLOSE drop
   LOCK TASK:GET
   idx TAKE-CLOSED {: row:n :}
   UNLOCK
   idx row HAND-OFF ;


: LOSE ( n -- ) {: idx:n :}
   LOCK TASK:GET
   idx LOST
   UNLOCK
   idx FINISH ;


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


\ The codec throws what breaks the protocol, so this runs under a catch.
: DECODE-HELD ( n -- n ) {: idx:n :}
   idx HEAD-BUF SPAN:$ STR:DATA idx HEAD-U @ >str
   WS-SENDER:CLIENT MESSAGE-MAX DECODE-HEADER
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


\ Under LOCK: the slot's first row with no call. The task asks only below
\ CALLS calls out, so one is free.
: FREE-ROW ( n -- n ) {: idx:n :}
   0
   CALLS @ 0 ?do
      idx i ROW ROW-STATE @ ROW-FREE = if drop i leave then
   loop ;


\ A message's first frame takes a free row's buffer for the message.
: ROW-FOR ( n -- ) {: idx:n :}
   idx CONTROL-FRAME? if exit then
   idx F-WIRE @ 0= if exit then
   LOCK TASK:GET
   idx FREE-ROW idx MSG-ROW !
   UNLOCK ;


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
      idx HEAD-DONE @ 0 <> if idx ROW-FOR then
   again ;


: PAYLOAD ( n -- SPAN:span<u8> ) {: idx:n :}
   idx CONTROL-FRAME? if idx CONTROL-BUF idx F-LEN @ SPAN:TAKE exit then
   idx MSG-BUF idx MSG-U @ idx F-LEN @ SPAN:SUB ;


: READ-PAYLOAD ( n -- progress ) {: idx:n :}
   idx PAY-U @ idx F-LEN @ < if
      idx idx PAYLOAD idx PAY-U @ SPAN:SKIP PULL idx PAY-U +!
   then
   idx PAY-U @ idx F-LEN @ >= if construct progress through exit then
   construct progress late ;


\ The payload is unmasked in one piece, so the key starts at its first byte.
: FRAME-IN ( n -- progress ) {: idx:n :}
   idx READ-HEADER {: head:progress :}
   head THROUGH? 0= if head exit then
   idx READ-PAYLOAD {: body:progress :}
   body THROUGH? 0= if body exit then
   idx F-KEY @ idx PAYLOAD MASK
   construct progress through ;


: MESSAGE$ ( n -- str ) {: idx:n :}
   idx MSG-BUF idx MSG-U @ SPAN:TAKE SPAN:$ ;


: TEXT? ( n -- bool )
   WS-OPCODE:TEXT OPCODE>WIRE = ;


: DELIVER ( n -- ) {: idx:n :}
   idx MESSAGE$
   idx MSG-WIRE @ TEXT? if WS-MESSAGE:TEXT else WS-MESSAGE:BINARY then {: msg :}
   0 idx MSG-U !
   0 idx MSG-WIRE !
   LOCK TASK:GET
   idx msg TAKE-MESSAGE {: row:n :}
   UNLOCK
   idx row HAND-OFF ;


\ Text is checked whole, because a scalar may straddle two fragments.
: DATA-IN ( n -- ) {: idx:n :}
   idx F-LEN @ idx MSG-U +!
   idx F-WIRE @ 0 <> if idx F-WIRE @ idx MSG-WIRE ! then
   idx F-FIN @ 0= if exit then
   idx MSG-WIRE @ TEXT? if
      idx MESSAGE$ UTF8:VALID? 0= if idx STATUS-DATA PHASE-CLOSED LOCKED-SHUT exit then
   then
   idx DELIVER ;


: PING-IN ( n -- ) {: idx:n :}
   idx HANDLE WS-OPCODE:PONG OPCODE>WIRE idx CONTROL-BUF idx F-LEN @ SPAN:TAKE SPAN:$
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
   idx CONTROL-BUF SPAN:$ STR:DATA CODE-BYTES >str BE@ {: code:n :}
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


\ The parse ends once the slot's read is not armed, as at CALLS calls out,
\ and takes up again from IN-AT once a call's end has armed it.
: PARSE ( n -- ) {: idx:n :}
   begin
      idx LOCKED-READING? 0= if exit then
      idx LIVE? 0= if 0 idx IN-U ! exit then
      idx FRAME-IN
      MATCH progress
         through OF idx TURN ENDOF
         late OF exit ENDOF
         fault OF idx idx FAILING @ PHASE-CLOSED LOCKED-SHUT ENDOF
      ;MATCH
   again ;


\ The peer hung up: a closed socket keeps the status it closed under, its
\ unwritten queue dropped; any other is lost.
: HUNG-UP ( n -- ) {: idx:n :}
   LOCK TASK:GET
   idx LIVE? if idx LOST else 0 idx OUT-AT ! 0 idx OUT-U ! then
   UNLOCK
   idx FINISH ;


: INPUT ( n -- ) {: idx:n :}
   idx LINK @ idx IN @ RX-CAP SPAN:TAKE SPAN:$ str> TCP4:TRANSFER-BYTES TCP4:TRY-READ
   MATCH TCP4:try-read-result
      data OF
         BLEN>N idx IN-U ! 0 idx IN-AT !
         LOCK TASK:GET idx HEARD-NOW UNLOCK
         idx PARSE
      ENDOF
      empty OF ENDOF
      closed OF drop idx HUNG-UP ENDOF
      failed OF drop idx LOSE ENDOF
   ;MATCH ;


: STOP-FLAG? ( -- bool )
   STOPPING @ 0 <> ;


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


: WANTED ( n -- n ) {: idx:n :}
   idx QUEUED 0 > if AIO:READABLE AIO:WRITABLE or exit then
   AIO:READABLE ;


\ What is left until the deadline, as the timer takes it: a part of a
\ millisecond counts as a whole one, so a timer that fires has outlasted it.
: REMAINING ( n -- ms ) {: deadline:n :}
   deadline mono-ns - {: left:n :}
   left 0 <= if 0 >MS exit then
   left NS-PER-MS /mod {: part:n whole:n :}
   part 0 > if whole 1+ >MS exit then
   whole >MS ;


\ A socket's poll carries no deadline, so it costs one AIO record: the task's
\ one timer bounds it (BOUND).
: ARM ( n n -- ) {: idx:n mask:n :}
   idx LINK @ TCP4:CONNECTION-FD mask -1 >MS AIO:POLL {: t:AIO:ticket :}
   t idx WAITERS AIO:GROUP+
   t idx TICKET !
   mask idx ARMED ! ;


: TIMER-ARM ( n -- ) {: due:n :}
   due REMAINING AIO:TIMEOUT {: t:AIO:ticket :}
   t TIMER-WAIT WAITERS AIO:GROUP+
   t TIMER !
   due TIMER-DUE ! ;


\ The timer fires by that due, 0 for none: one outstanding for a later due is
\ cancelled first, so at most one is.
: BOUND ( n -- ) {: due:n :}
   due 0= if exit then
   TIMER-DUE @ {: at:n :}
   at 0 <> if
      at due <= if exit then
      TIMER @ UNWAIT
      0 TIMER-DUE !
   then
   due TIMER-ARM ;


\ What SYNC does next with a slot: nothing, since it holds no socket the task
\ serves; parse the bytes read; end a socket that gave up, or one closed with
\ its queue written; arm nothing while its handler call runs; or poll for that
\ mask, with no deadline, the task's timer bounding the wait by that due.
ENUM course
   VARIANT vacant ;VARIANT
   VARIANT unparsed ;VARIANT
   VARIANT given-up ;VARIANT
   VARIANT spent ;VARIANT
   VARIANT held ;VARIANT
   VARIANT waiting FIELD mask n FIELD due n ;VARIANT
;ENUM


\ Under LOCK: an open socket a stop has reached is closed under 1001.
: AWAY ( n -- ) {: idx:n :}
   STOP-AT @ 0 <> idx PHASE @ PHASE-OPEN = and if idx STATUS-AWAY PHASE-CLOSED SHUT drop then ;


\ The course of a slot whose read is not armed, under LOCK: its queue is
\ written, and lost when it gives up, as a reading slot's is, but nothing is
\ read, parsed or finished until a call's end arms the read.
: HELD ( n -- course ) {: idx:n :}
   idx AWAY
   idx QUEUED 0 > idx GIVE-UP? and if idx LOST then
   idx QUEUED 0= if construct course held exit then
   AIO:WRITABLE idx DUE construct course waiting ;


\ The slot's course, under LOCK: its phase first, since LINK, IN and HANDLER
\ are the task's to read only once the slot is published, then whether its
\ read is armed.
\ Bytes held unparsed, the ones that rode behind the handshake or behind a
\ message handed on, come next, since no poll reports them. A socket closed
\ with its queue written is spent before it may give up, so a stop's deadline
\ that passed during a call leaves its 1001 the status.
: COURSE ( n -- course ) {: idx:n :}
   idx PHASE-SET? 0= if construct course vacant exit then
   idx READING? 0= if idx HELD exit then
   idx IN-U @ 0 > if construct course unparsed exit then
   idx AWAY
   idx PROBE
   idx PHASE @ PHASE-CLOSED = idx QUEUED 0= and if construct course spent exit then
   idx GIVE-UP? idx SILENT? or if construct course given-up exit then
   idx WANTED idx READY-DUE construct course waiting ;


: LOCKED-COURSE ( n -- course )
   LOCK TASK:GET
   [: COURSE ;] [: UNLOCK ;] finally ;


\ A poll is replaced only for another mask: it carries no deadline, since the
\ task's one timer bounds every poll.
: REARM ( n n -- ) {: idx:n mask:n :}
   idx ARMED @ mask = if exit then
   idx DISARM
   idx mask ARM ;


\ The slot taken through its course; the due its wait is bounded by, 0 for
\ none.
: SYNC ( n -- n ) {: idx:n :}
   begin
      idx LOCKED-COURSE
      MATCH course
         vacant OF 0 exit ENDOF
         unparsed OF idx PARSE ENDOF
         given-up OF idx LOSE 0 exit ENDOF
         spent OF idx FINISH 0 exit ENDOF
         held OF idx DISARM 0 exit ENDOF
         waiting OF swap idx swap REARM exit ENDOF
      ;MATCH
   again ;


\ Every slot taken through its course, and the timer bounding the earliest
\ due among them.
: SYNC-ALL ( -- )
   0 CAP @ 0 ?do i SYNC SOONER loop BOUND ;


\ The timer fired, so none is outstanding: the slots now due give up, are
\ pinged or are lost, and the timer is armed for the next due.
: FIRED ( -- )
   0 TIMER-DUE !
   SYNC-ALL ;


\ The poll's readiness: room is written to, and anything else is read, which
\ is how an end of stream or an error is learnt, while the slot's read is
\ armed. Linux reports a TCP hang-up or error with POLLOUT, so a held slot's
\ write-only poll fails its write and is lost instead of polled again.
: READY ( n n -- ) {: idx:n rev:n :}
   rev AIO:WRITABLE and 0 <> if
      idx LOCKED-FLUSH 0= if idx LOSE exit then
   then
   rev AIO:WRITABLE invert and 0= if exit then
   idx LOCKED-READING? 0= if exit then
   idx INPUT ;


\ A poll carries no deadline, so it never times out; the timer bounds the due
\ SYNC answers.
: POLLED ( n AIO:outcome -- ) {: idx:n out :}
   0 idx ARMED !
   out
   MATCH AIO:outcome
      ready OF idx swap READY ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      rejected OF drop idx LOSE ENDOF
   ;MATCH
   idx SYNC BOUND ;


\ The wake stays armed until SERVE leaves, so an ACCEPT that ends while WS
\ is stopping is still heard.
: WOKEN ( -- )
   WAKER AIO:WOKE
   STOP-FLAG? STOP-AT @ 0= and if mono-ns GOODBYE-MS NS-PER-MS * + STOP-AT ! then
   WAKE-ARM
   SYNC-ALL ;


: ANSWER ( n AIO:outcome -- ) {: wait:n out :}
   wait WAKE-WAIT = if out OUTCOME-DROP WOKEN exit then
   wait TIMER-WAIT = if out OUTCOME-DROP FIRED exit then
   wait out POLLED ;


: ANY-SET? ( -- bool )
   LOCK TASK:GET
   false
   CAP @ 0 ?do i PHASE @ PHASE-NONE <> if drop true leave then loop
   LOCK TASK:RELEASE ;


\ The slot holds a socket and no handler call.
: UNCALLED? ( n -- bool ) {: idx:n :}
   LOCK TASK:GET
   idx PHASE-SET? idx DELIVERY @ DELIVERY-TAKING = and
   idx CALLS-OUT @ 0= and
   UNLOCK ;


\ LEFT is set first, so an ACCEPT has either published its socket for this
\ scan to close or finds LEFT and takes nothing, and the last handler call of
\ a socket that ends after it closes and retires its slot (LEFT-OFF).
\ SERVE registers this before the task's first AIO submission registers AIO's
\ own cleanup, which therefore runs first: it has given back every record the
\ task submitted, the timer's among them, and emptied WAITERS, so no ticket is
\ left to take out of it and no timer is outstanding.
: CLEANUP ( -- )
   0 TIMER-DUE !
   LOCK TASK:GET
   1 LEFT !
   LOCK TASK:RELEASE
   CAP @ 0 ?do
      i UNCALLED? if
         i LINK @ TCP4:CLOSE drop
         i RETIRE
      then
   loop
   0 STOPPER @ TASK:WAKE ;


\ Each wait ends at a socket's poll, the wake or the timer: the polls carry no
\ deadline, and the task's one timer bounds them. A stop that has emptied
\ every slot ends it.
: SERVING ( -- )
   WAKE-ARM
   SYNC-ALL
   begin
      STOP-AT @ 0 <> ANY-SET? 0= and if exit then
      WAITERS AIO:AWAIT-ANY ANSWER
   again ;


: REPORT-END ( n -- )
   {: code:n :}
   LOG:ERROR s" ws" LOG:LINE
   s" task failed, throw " LOG:TEXT+  code LOG:CODE+
   LOG:;LINE ;


\ WS's task. A throw from its loop, such as a mapping the task cannot get, is
\ reported and rethrown, so TASK:THROW@ keeps it and CLEANUP closes every
\ socket: each handshake is then 503 until STOP and START. A halt passes the
\ catch unreported.
: SERVE ( -- )
   [: CLEANUP ;] TASK:SELF TASK:AT-EXIT
   [: SERVING ;] catch {: code:n :}
   code 0= if exit then
   code REPORT-END
   code throw ;


: SOLE ( HTTP:request str -- str bool ) {: asked:HTTP:request name:str :}
   asked name HTTP:HEADER-COUNT-OF 1 <> if name STR:DATA 0 >str false exit then
   asked name HTTP:HEADER-OF$ ;


: LISTS? ( HTTP:request str str -- bool )
   {: asked:HTTP:request name:str token:str :}
   asked name SOLE {: v:str found:bool :}
   found 0= if false exit then
   v token HTTP:LIST-HAS? ;


: DECODE-NONCE ( n -- n )
   {: w:n :}
   w KEY-AT @ w KEY-U @ >str w NONCE-BUF BASE64:DECODE ;


\ The key is the base64 of sixteen bytes (section 4.2.1), and is kept as it
\ was sent for the accept value. A throw that is not base64's own rejection is
\ not this word's to answer.
: KEY-OK? ( HTTP:request n -- bool )
   {: asked:HTTP:request w:n :}
   asked s" sec-websocket-key" SOLE {: v:str found:bool :}
   found 0= if false exit then
   v STR:LEN NONCE-CHARS <> if false exit then
   v STR:DATA w KEY-AT !
   v STR:LEN w KEY-U !
   w [: DECODE-NONCE ;] catch {: got code:n :}
   code 0= if got NONCE-BYTES = exit then
   code E-BASE64-FIRST > code E-BASE64-LAST < or if code throw then
   false ;


\ An HTTP/1.1 GET (section 4.2.1, item 1). Item 2, exactly one valid Host, the
\ server checks before any handler runs.
: GET-OK? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked HTTP:METHOD$ s" GET" STR:= 0= if false exit then
   asked HTTP:VERSION$ s" HTTP/1.1" STR:= ;


\ 0 for an opening handshake this server accepts, else the HTTP status that
\ rejects it: 426 when all that is wrong is the version (section 4.4). The
\ scratch is the HTTP worker's row.
: VERDICT ( HTTP:request n -- n ) {: asked:HTTP:request w:n :}
   asked GET-OK? 0= if 400 exit then
   asked s" upgrade" s" websocket" LISTS? 0= if 400 exit then
   asked s" connection" s" upgrade" LISTS? 0= if 400 exit then
   asked w KEY-OK? 0= if 400 exit then
   asked s" sec-websocket-version" SOLE {: v:str found:bool :}
   found 0= if 426 exit then
   v s" 13" STR:= 0= if 426 exit then
   0 ;


: REJECT ( HTTP:response n -- accepted ) {: answer:HTTP:response status:n :}
   status 503 = if
      answer 503 s" websocket_unavailable" s" this server has no room for another WebSocket" HTTP:ERROR!
      503 WS-ACCEPTED:REJECTED exit
   then
   status 426 = if
      answer s" Upgrade" s" websocket" HTTP:HEADER!
      answer s" Sec-WebSocket-Version" s" 13" HTTP:HEADER!
      answer 426 s" upgrade_required" s" this server speaks WebSocket version 13" HTTP:ERROR!
      426 WS-ACCEPTED:REJECTED exit
   then
   answer 400 s" bad_handshake" s" the request is not a WebSocket opening handshake" HTTP:ERROR!
   400 WS-ACCEPTED:REJECTED ;


: GUID$ ( -- str )
   s" 258EAFA5-E914-47DA-95CA-C5AB0DC85B11" ;


\ The accept value (section 4.2.2), behind the queue.
: ACCEPT-VALUE ( n n -- ) {: idx:n w:n :}
   idx CTX-BUF SHA1:START
   idx CTX-BUF w KEY-AT @ w KEY-U @ >str SHA1:FEED
   idx CTX-BUF GUID$ SHA1:FEED
   idx CTX-BUF SHA1:FINISH idx DIGEST-BUF SHA1:DIGEST!
   idx ACCEPT-CHARS ROOM
   idx DIGEST-BUF SPAN:$ idx OUT @ idx OUT-U @ SPAN:SKIP BASE64:ENCODE idx OUT-U +! ;


\ The 101 answer, first in the queue. No subprotocol or extension is negotiated,
\ and it carries no header line the handler added to the response.
: SWITCHING ( n n -- ) {: idx:n w:n :}
   idx S\" HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n" QUEUE
   idx S\" Connection: Upgrade\r\nSec-WebSocket-Accept: " QUEUE
   idx w ACCEPT-VALUE
   idx S\" \r\n\r\n" QUEUE ;


: RESERVE ( -- n )
   LOCK TASK:GET
   -1
   RUNNING? STOP-FLAG? 0= and LEFT @ 0= and if
      CAP @ 0 ?do
         i PHASE @ PHASE-NONE = if drop i PHASE-RESERVED i PHASE ! leave then
      loop
   then
   LOCK TASK:RELEASE ;


\ A mapping that fails throws with the ones before it in the slot, for
\ RETIRE to give back.
: FILL ( n -- ) {: idx:n :}
   REGION-BYTES NEW-SPAN idx REGION !
   TX-CAP NEW-SPAN idx OUT !
   0 idx OUT-AT !
   0 idx OUT-U !
   idx FRAME-RESET
   0 idx MSG-U !
   0 idx MSG-WIRE ! ;


: FILL-INPUT ( n str -- n str ) {: idx:n early:str :}
   RX-CAP early STR:LEN max NEW-SPAN idx IN !
   early idx IN @ SPAN:COPY
   0 idx IN-AT !
   early STR:LEN idx IN-U !
   idx early ;


\ Under LOCK: the request's connection taken into the slot and the socket
\ published to the task, open with the 101 queued. An input mapping that fails
\ closes the connection before the throw goes on, so nothing holds it.
: TAKE ( HTTP:request n -- HTTP:request n ) {: asked:HTTP:request idx:n :}
   asked HTTP:TAKE-OVER {: conn:TCP4:connection early:str :}
   idx early [: FILL-INPUT ;] catch {: code:n :} drop drop
   code 0 <> if conn TCP4:CLOSE drop code throw then
   conn idx LINK !
   PHASE-OPEN idx PHASE !
   asked idx ;


\ Under LOCK: the connection is taken unless WS is stopping or its task has
\ given everything back, when nobody would serve the socket. The code TAKE
\ threw, or 0, and true when nothing was taken.
: ADOPT ( HTTP:request n -- n bool ) {: asked:HTTP:request idx:n :}
   STOP-FLAG? LEFT @ 0 <> or if 0 true exit then
   asked idx [: TAKE ;] catch {: code:n :} 2drop
   code false ;


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


\ Under LOCK, the connection taken: the handler, its value and `opened`, in
\ row 0, are the slot's, and that call is queued before the task can see the
\ slot, so it comes first; its worker waits for LOCK. The code HTTP:SUBMIT
\ threw, or 0; after a throw the row is free, the slot reserved again and its
\ connection closed.
: OPEN-CALL ( [ socket n message -- ] n n -- n ) {: q arg:n idx:n :}
   q idx HANDLER !
   arg idx HANDLER-ARG !
   idx 0 ROW {: row:n :}
   row WS-MESSAGE:OPENED QUEUE-ROW
   DELIVERY-OPENING idx DELIVERY !
   row [: QUEUE-HAND ;] catch {: code:n :} drop
   code 0= if 0 exit then
   ROW-FREE row ROW-STATE !
   idx LINK @ TCP4:CLOSE drop
   PHASE-RESERVED idx PHASE !
   code ;


\ The reserved slot becomes the request's socket, or, when WS began stopping
\ since RESERVE, is given back and the request rejected with 503. A throw, from
\ a request already taken, from a mapping or from HTTP:SUBMIT, gives the slot
\ back before it goes on, so no slot stays reserved and no connection open.
: OPEN-SOCKET ( HTTP:request HTTP:response [ socket n message -- ] n n n -- accepted )
   {: asked:HTTP:request answer:HTTP:response q arg:n idx:n w:n :}
   idx HANDLE {: h:n :}
   idx w [: PREPARE ;] catch {: failed:n :} 2drop
   failed 0 <> if idx GIVE-BACK failed throw then
   LOCK TASK:GET
   asked idx ADOPT {: code:n gone:bool :}
   code 0= gone 0= and if q arg idx OPEN-CALL else code then {: err:n :}
   LOCK TASK:RELEASE
   err 0 <> if idx GIVE-BACK err throw then
   gone if idx GIVE-BACK answer 503 REJECT exit then
   POKE
   h >SOCKET WS-ACCEPTED:OPEN ;


: PIPE-OPEN ( -- )
   WAKER AIO:WAKER-OPEN
   LOCK TASK:GET
   1 PIPED !
   LOCK TASK:RELEASE ;


\ No poke writes to the pipe once it is closed.
: PIPE-CLOSE ( -- )
   LOCK TASK:GET
   0 PIPED !
   LOCK TASK:RELEASE
   WAKER AIO:WAKER-CLOSE ;


\ When STOP gives up waiting for WS's task and the handler calls.
: STOP-DEADLINE ( -- n )
   GOODBYE-MS STOP-MARGIN-MS + NS-PER-MS * mono-ns + ;


\ CLEANUP's hint wakes this, or the deadline ends it; the other hints a stop
\ gets are read past.
: AWAIT-LEFT ( n -- ) {: deadline:n :}
   begin LEFT @ 0 <> if exit then deadline TASK:WAIT-UNTIL 0= until ;


\ Under LOCK: a handshake inside ACCEPT holds a slot.
: RESERVED? ( -- bool )
   false
   CAP @ 0 ?do i PHASE @ PHASE-RESERVED = if drop true leave then loop ;


\ Under LOCK: a call row is not free.
: CALLING? ( -- bool )
   false
   CAP @ CALLS @ * 0 ?do
      i ROW-STATE @ ROW-FREE <> if drop true leave then
   loop ;


\ Under LOCK, at one reading of the clock: what STOP still waits for, a
\ handshake inside ACCEPT whatever the time, and a handler call out before
\ the deadline.
: UNGIVEN ( n -- bool bool ) {: deadline:n :}
   RESERVED?
   mono-ns deadline < CALLING? and ;


\ Each handshake inside ACCEPT finds WS stopping and gives its slot back, so
\ none fills a slot once STOP has returned and a START may move the tables.
\ Each handler call ends, the last of a socket's retiring its slot, or is
\ still out at the deadline: a running one holds START off until it ends
\ (CLAIM-START), and STOP drops a queued one (DROP-QUEUED). Only a handshake
\ makes the wait unbounded, ended by GIVE-BACK's hint; a call's wait is
\ bounded by the deadline, so one that passes after UNGIVEN read the clock is
\ a wait of zero.
: AWAIT-GIVEN ( n -- ) {: deadline:n :}
   begin
      LOCK TASK:GET
      deadline UNGIVEN
      LOCK TASK:RELEASE
      swap if drop TASK:WAIT else
         0= if exit then
         deadline TASK:WAIT-UNTIL drop
      then
   again ;


\ Under LOCK, once the task has left: each call still queued is dropped, and
\ a slot it leaves no call is retired, its connection closed unless FINISH
\ has closed it, so neither a call a worker takes later nor one HTTP:STOP
\ drops unrun holds its row. A worker that takes such a call finds its row
\ free or its slot's count moved on, and runs nothing (HAND).
: DROP-QUEUED ( -- )
   CAP @ CALLS @ * 0 ?do
      i ROW-STATE @ ROW-QUEUED = if i ROW-SLOT i false END-CALL then
   loop ;


\ Under LOCK: the end of STOP, with no stopper left to tell.
: STOPPED ( -- )
   DROP-QUEUED
   0 STOPPING !
   RUN-IDLE RUN ! ;


\ Under LOCK: this task takes the stop, or false on WS's own task, which STOP
\ joins, outside a run or while START still starts it, or when another STOP
\ holds it.
: CLAIM-STOP ( -- bool )
   TASK:SELF WS-TASK = if false exit then
   RUNNING? 0= STOP-FLAG? or if false exit then
   TASK:SELF 0 STOPPER !
   1 STOPPING !
   true ;


\ START's part under LOCK: the tables mapped for n sockets of c calls, then
\ the limits, the config's bounds and the stop's state set for the new run.
: SET-UP ( ^config n n -- ) {: cfg:^ n:n c:n :}
   n n c * TABLES-RESERVE
   n CAP !
   c CALLS !
   cfg WS-CONFIG:PING @ MS>N NS-PER-MS * PING-NS !
   cfg WS-CONFIG:PONG @ MS>N NS-PER-MS * PONG-NS !
   cfg WS-CONFIG:STALL @ MS>N NS-PER-MS * STALL-NS !
   0 STOPPING !
   0 STOP-AT !
   0 LEFT ! ;


\ Under LOCK: this START claims the run, or false when WS is not idle, or
\ when a handler call from an earlier run is still out, reading the tables
\ SET-UP may move; every row is then free, whatever CALLS the run sets.
: CLAIM-START ( -- bool )
   CALLING? if false exit then
   RUN @
   MATCH run
      idle OF construct run starting RUN ! true ENDOF
      starting OF false ENDOF
      running OF false ENDOF
   ;MATCH ;


\ Under LOCK: the run START claimed is running once its task is activated, or
\ idle again when START threw first.
: SETTLE ( n -- ) {: code:n :}
   code 0= if construct run running RUN ! else RUN-IDLE RUN ! then ;


: ACTIVATE-TASK ( -- )
   [: SERVE ;] WS-TASK TASK:ACTIVATE ;


\ At least one socket of at least one call, and no row past SLOT-MASK.
: LIMITS? ( n n -- bool ) {: n:n c:n :}
   n 1 < c 1 < or if false exit then
   c SLOT-MASK n / <= ;


: LAUNCH ( ^config -- ^config ) {: cfg:^ :}
   cfg WS-CONFIG:SOCKETS @ {: n:n :}
   cfg WS-CONFIG:CALLS @ {: c:n :}
   n c LIMITS? 0= if E-WS-LIMIT throw then
   LOCK TASK:GET
   cfg n c [: SET-UP ;] [: UNLOCK ;] finally
   TASK:SELF 0 STOPPER !
   PIPE-OPEN
   [: ACTIVATE-TASK ;] catch {: code:n :}
   code 0 <> if PIPE-CLOSE code throw then
   cfg ;


public

\ One call at a time to a socket, in order. Leaves sockets as it is.
: DEFAULTS ( ^config -- ) {: cfg:^ :}
   1 cfg WS-CONFIG:CALLS !
   30000 >MS cfg WS-CONFIG:PING !
   10000 >MS cfg WS-CONFIG:PONG !
   5000 >MS cfg WS-CONFIG:STALL ! ;


\ While WS runs, or while a handler call an earlier STOP left running is
\ still out, it is E-WS-STATE.
: START ( ^config -- ) {: cfg:^ :}
   LOCK TASK:GET
   CLAIM-START
   LOCK TASK:RELEASE
   0= if E-WS-STATE throw then
   cfg [: LAUNCH ;] catch {: code:n :} drop
   LOCK TASK:GET
   code SETTLE
   LOCK TASK:RELEASE
   code 0 <> if code throw then ;


\ Sends status 1001 to every open socket, closes each once its frames are
\ written or within GOODBYE-MS, tells each handler `closed` once the calls
\ it has out, if any, have returned, and joins the task. A socket already
\ closing gets no 1001: it stays closing, and its handler is told its peer's
\ answer, or 1006 once it gives up. STOP waits for WS's task and the handler
\ calls at most GOODBYE-MS and STOP-MARGIN-MS, kills the task if it has not
\ left by then, and returns. A call still running then ends later: the last
\ of a socket's to end closes the socket and retires its slot, telling the
\ handler nothing more, and START is E-WS-STATE until it has. A call still
\ queued then is dropped and never runs (DROP-QUEUED). Called inside a
\ handler call, STOP waits out its bound for that call, then returns. The
\ calls run on HTTP's workers, so WS stops before HTTP does. A handshake
\ inside ACCEPT is answered 503 and STOP waits for it, however long its
\ worker takes to get there. Outside a run or while START still starts it,
\ while another STOP runs, or on WS's own task, it is E-WS-STATE.
: STOP ( -- )
   LOCK TASK:GET
   CLAIM-STOP
   LOCK TASK:RELEASE
   0= if E-WS-STATE throw then
   STOP-DEADLINE {: deadline:n :}
   POKE
   deadline AWAIT-LEFT
   WS-TASK TASK:KILL
   deadline AWAIT-GIVEN
   PIPE-CLOSE
   LOCK TASK:GET
   [: STOPPED ;] [: UNLOCK ;] finally ;


\ Inside a route handler: the request is checked as an opening handshake, that
\ is an HTTP/1.1 GET whose Upgrade names websocket, whose Connection names
\ Upgrade, whose Sec-WebSocket-Key is the base64 of sixteen bytes and whose
\ Sec-WebSocket-Version is 13, each of those four arriving once; the server
\ rejects an HTTP/1.1 request without exactly one valid Host before any handler
\ runs. A handshake that passes takes the connection from the server and `open`
\ carries the socket; the task answers 101, and the handler is called with the
\ socket, n and `opened`, then every message and, once, `closed`, short of a
\ STOP that passes its bound (STOP), each call with n, so whatever the handler
\ does for `opened`, such as recording the socket under n, precedes every
\ message. A peer gone without a close is found by ping (config). A handshake
\ that does not pass is answered 400, or 426 naming version 13, and one past
\ the config's sockets, or while WS is stopping, 503, through the response
\ once the route handler returns, and `rejected` carries that status: no
\ socket exists for it, and n is not kept.
\ Origin is the route handler's to check before it accepts. Outside a run or
\ while START still starts it, it is E-WS-STATE.
: ACCEPT ( HTTP:request HTTP:response [ socket n message -- ] n -- accepted )
   {: asked:HTTP:request answer:HTTP:response q arg:n :}
   RUNNING? 0= if E-WS-STATE throw then
   HTTP:WORKER-SLOT {: w:n :}
   asked w VERDICT {: status:n :}
   status 0 <> if answer status REJECT exit then
   RESERVE {: idx:n :}
   idx 0 < if answer 503 REJECT exit then
   asked answer q arg idx w OPEN-SOCKET ;


\ SEND-TEXT, SEND-BINARY and CLOSE may be called from any task, the handler
\ included: each appends whole frames to the socket's queue under LOCK and pokes
\ the wake pipe, so a sender never waits for the peer. A frame queued behind
\ QUEUE-MAX unwritten bytes loses the socket (1006, its queue dropped) and is
\ E-WS-CLOSED; a frame is never rejected for its own size. A send on a socket
\ closing, closed or gone is E-WS-CLOSED, and text that is not UTF-8 is
\ E-WS-TEXT before anything is queued (section 5.6).
: SEND-TEXT ( socket str -- ) {: sock s:str :}
   s UTF8:VALID? 0= if E-WS-TEXT throw then
   sock SOCKET>N WS-OPCODE:TEXT OPCODE>WIRE s SEND ;


: SEND-BINARY ( socket str -- ) {: sock s:str :}
   sock SOCKET>N WS-OPCODE:BINARY OPCODE>WIRE s SEND ;


\ Starts the closing handshake under that status, from any task: the close
\ frame is queued, nothing may be sent behind it, and the handler goes on
\ receiving until the peer's close arrives, or for the config's stall. A
\ socket already closing or closed, or gone, is left as it is. A status no
\ frame may carry is E-WS-CODE, and a close frame behind QUEUE-MAX unwritten
\ bytes loses the socket and is E-WS-CLOSED.
: CLOSE ( socket n -- ) {: sock code:n :}
   code WIRE-STATUS? 0= if E-WS-CODE throw then
   sock SOCKET>N {: h:n :}
   LOCK TASK:GET
   h code [: CLOSE-SHUT ;] [: UNLOCK ;] finally {: lost:bool :}
   POKE
   lost if E-WS-CLOSED throw then ;

;package
